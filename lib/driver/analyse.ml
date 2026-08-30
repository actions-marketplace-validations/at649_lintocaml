open Lintml_engine

type outcome = {
  diagnostics : Diagnostic.t list;
  suppressed : suppressed list;
  files_analysed : int;
  cmt_files_found : int;
  load_errors : (string * Tast_iface.load_error) list;
}

and suppressed = { rule_id : string; loc : Loc.t; reason : string option }

let compare_suppressed left right =
  match Loc.compare left.loc right.loc with
  | 0 -> (
      match String.compare left.rule_id right.rule_id with
      | 0 -> Option.compare String.compare left.reason right.reason
      | result -> result)
  | result -> result

(* .cmt files record the source path as the compiler saw it, and dune sandboxes
   cmt_builddir to a placeholder. Recover the real path by walking up from the
   .cmt's own directory looking for the recorded filename. *)
let resolve_source ~cmt_path filename =
  if filename = "" then None
  else if Sys.file_exists filename then Some filename
  else
    let rec collect dir depth candidates =
      if depth = 0 then candidates
      else
        let candidate = Filename.concat dir filename in
        let candidates =
          if Sys.file_exists candidate then candidate :: candidates else candidates
        in
        let parent = Filename.dirname dir in
        if String.equal parent dir then candidates
        else collect parent (depth - 1) candidates
    in
    let candidates = collect (Filename.dirname cmt_path) 8 [] |> List.rev in
    let rec nearest_build_directory directory =
      if String.equal (Filename.basename directory) "_build" then Some directory
      else
        let parent = Filename.dirname directory in
        if String.equal parent directory then None else nearest_build_directory parent
    in
    let outside_build =
      match nearest_build_directory (Filename.dirname cmt_path) with
      | None -> Fun.const true
      | Some build_directory ->
          let prefix = build_directory ^ Filename.dir_sep in
          fun path ->
            not (String.equal path build_directory || String.starts_with ~prefix path)
    in
    match List.find_opt outside_build candidates with
    | Some _ as source -> source
    | None -> ( match candidates with source :: _ -> Some source | [] -> None)

let analyse_cmt ~cfg ~rules path =
  match Tast_iface.load_cmt path with
  | Error error -> ([], [], false, Some (path, error))
  | Ok None -> ([], [], false, None)
  | Ok (Some (annotation, sourcefile, source_digest)) -> (
      try
        let acc = ref [] in
        let suppressed = ref [] in
        let source_cache = Hashtbl.create 1 in
        let resolve_location location =
          let recorded =
            if location.Loc.file = "" then Option.value sourcefile ~default:path
            else location.file
          in
          match resolve_source ~cmt_path:path recorded with
          | Some real -> { location with file = real }
          | None -> { location with file = recorded }
        in
        let source_fragment location =
          let location = resolve_location location in
          let source =
            match Hashtbl.find_opt source_cache location.file with
            | Some source -> source
            | None ->
                let source = Result.to_option (Source_text.read location.file) in
                Hashtbl.add source_cache location.file source;
                source
          in
          Option.bind source (fun source -> Source_text.slice source location)
        in
        let source_matches_artifact location =
          let location = resolve_location location in
          (* OCaml 5.5 changed the source digest stored in CMT files from MD5 to
           BLAKE128. lintml accepts artifacts from both sides of that change. *)
          let digest_matches source digest =
            String.equal (Digest.string source.Source_text.contents) digest
            || String.equal (Digest.BLAKE128.string source.contents) digest
          in
          match (source_digest, Hashtbl.find_opt source_cache location.file) with
          | None, _ -> false
          | Some digest, Some (Some source) -> digest_matches source digest
          | Some _, Some None -> false
          | Some digest, None -> (
              match Source_text.read location.file with
              | Error _ ->
                  Hashtbl.add source_cache location.file None;
                  false
              | Ok source ->
                  Hashtbl.add source_cache location.file (Some source);
                  digest_matches source digest)
        in
        Tast_iface.iter_views annotation ~f:(fun view ->
            List.iter
              (fun (r : Rule.t) ->
                List.iter
                  (fun (f : Rule.finding) ->
                    let loc = resolve_location f.f_loc in
                    match Config.severity_for_path cfg r loc.file with
                    | None -> ()
                    | Some severity -> (
                        match Expr_view.suppression_for r.id view with
                        | Some suppression ->
                            suppressed :=
                              { rule_id = r.id; loc; reason = suppression.reason }
                              :: !suppressed
                        | None ->
                            let fix =
                              match f.f_fix with
                              | None -> None
                              | Some replacement when source_matches_artifact f.f_loc ->
                                  Option.bind (source_fragment f.f_loc)
                                    (fun expected_source ->
                                      let replacement =
                                        match replacement with
                                        | Rule.Text text -> Some text
                                        | Rule.Source source -> source_fragment source
                                        | Rule.Parenthesized_source source ->
                                            Option.map
                                              (fun text -> "(" ^ text ^ ")")
                                              (source_fragment source)
                                      in
                                      Option.map
                                        (fun replacement ->
                                          (replacement, expected_source))
                                        replacement)
                              | Some _ -> None
                            in
                            let replacement = Option.map fst fix in
                            let expected_source = Option.map snd fix in
                            acc :=
                              Diagnostic.make ~rule_id:r.id ~severity ~loc
                                ~message:f.f_message ?suggestion:f.f_suggestion
                                ?replacement ?expected_source ()
                              :: !acc))
                  (r.check view))
              rules);
        (!acc, !suppressed, true, None)
      with
      | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
      | exn ->
          ( [],
            [],
            false,
            Some
              ( path,
                Tast_iface.Invalid_cmt
                  (Fmt.str "analysis failed: %s" (Printexc.to_string exn)) ) ))

let rec find_cmt_files dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.to_list entries
      |> List.concat_map (fun entry ->
          let path = Filename.concat dir entry in
          match (Unix.lstat path).st_kind with
          | Unix.S_DIR -> find_cmt_files path
          | Unix.S_REG
            when Filename.check_suffix entry ".cmt" || Filename.check_suffix entry ".cmti"
            ->
              [ path ]
          | _ -> []
          | exception Unix.Unix_error _ -> [])

let run ~cfg ~rules ~roots =
  let cmts = List.concat_map find_cmt_files roots |> List.sort_uniq String.compare in
  let analysed = List.map (analyse_cmt ~cfg ~rules) cmts in
  let diagnostics =
    List.concat_map (fun (diagnostics, _, _, _) -> diagnostics) analysed
  in
  let diagnostics = List.sort_uniq Diagnostic.compare diagnostics in
  let suppressed =
    List.concat_map (fun (_, suppressed, _, _) -> suppressed) analysed
    |> List.sort_uniq compare_suppressed
  in
  {
    diagnostics;
    suppressed;
    files_analysed =
      List.fold_left
        (fun count (_, _, was_analysed, _) -> if was_analysed then count + 1 else count)
        0 analysed;
    cmt_files_found = List.length cmts;
    load_errors = List.filter_map (fun (_, _, _, error) -> error) analysed;
  }
