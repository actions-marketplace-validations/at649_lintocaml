open Lintocaml_engine

type skip_reason = Overlapping | Stale_source | Invalid

type result = {
  applied : int;
  skipped : int;
  errors : string list;
  skipped_reasons : (skip_reason * int) list;
}

let skip_reason_to_string = function
  | Overlapping -> "overlapping"
  | Stale_source -> "stale source"
  | Invalid -> "invalid"

type edit = { start : int; finish : int; replacement : string }

(* Distinguishes the two ways a fix can be refused. A stale source is the
   common one and has an action attached ("rebuild"), so collapsing it into
   "invalid" throws away the only thing the user can do about it. *)
let edit_of_diagnostic source (diagnostic : Diagnostic.t) =
  match (diagnostic.replacement, diagnostic.expected_source) with
  | Some replacement, Some expected -> (
      match Source_text.range source diagnostic.loc with
      | None -> Error Stale_source
      | Some (start, finish) ->
          let current = String.sub source.contents start (finish - start) in
          if String.equal current expected then Ok { start; finish; replacement }
          else Error Stale_source)
  | _ -> Error Invalid

let select_non_overlapping edits =
  let edits =
    List.sort
      (fun left right ->
        match Int.compare left.start right.start with
        | 0 -> Int.compare right.finish left.finish
        | result -> result)
      edits
  in
  let rec loop last_finish selected skipped = function
    | [] -> (List.rev selected, skipped)
    | edit :: rest when edit.start < last_finish ->
        loop last_finish selected (skipped + 1) rest
    | edit :: rest -> loop edit.finish (edit :: selected) skipped rest
  in
  loop (-1) [] 0 edits

let apply_edits source edits =
  List.sort (fun left right -> Int.compare right.start left.start) edits
  |> List.fold_left
       (fun current edit ->
         String.sub current 0 edit.start
         ^ edit.replacement
         ^ String.sub current edit.finish (String.length current - edit.finish))
       source

let write_atomic path contents =
  let directory = Filename.dirname path in
  let temporary = ref None in
  let cleanup () =
    Option.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) !temporary
  in
  try
    let source_stat = Unix.lstat path in
    if source_stat.st_kind = Unix.S_LNK then
      raise (Sys_error "refusing to replace a symbolic link");
    let temporary_path = Filename.temp_file ~temp_dir:directory ".lintocaml-" ".tmp" in
    temporary := Some temporary_path;
    Out_channel.with_open_bin temporary_path (fun channel ->
        Out_channel.output_string channel contents);
    let permissions = source_stat.st_perm in
    Unix.chmod temporary_path permissions;
    Unix.rename temporary_path path;
    temporary := None;
    Ok ()
  with
  | (Sys_error _ | Unix.Unix_error _) as exn ->
      cleanup ();
      Error (Printexc.to_string exn)
  | exn ->
      cleanup ();
      raise exn

let apply_file path diagnostics =
  match Source_text.read path with
  | Error message -> (0, 0, [], [ Fmt.str "%s: %s" path message ])
  | Ok source -> (
      let edits, invalid, stale =
        List.fold_left
          (fun (edits, invalid, stale) diagnostic ->
            match edit_of_diagnostic source diagnostic with
            | Ok edit -> (edit :: edits, invalid, stale)
            | Error Stale_source -> (edits, invalid, stale + 1)
            | Error _ -> (edits, invalid + 1, stale))
          ([], 0, 0) diagnostics
      in
      let edits, overlapping = select_non_overlapping edits in
      let reasons =
        [ (Invalid, invalid); (Stale_source, stale); (Overlapping, overlapping) ]
      in
      let skipped = invalid + stale + overlapping in
      if edits = [] then (0, skipped, reasons, [])
      else
        match write_atomic path (apply_edits source.contents edits) with
        | Ok () -> (List.length edits, skipped, reasons, [])
        | Error message -> (0, skipped, reasons, [ Fmt.str "%s: %s" path message ]))

let run diagnostics =
  let fixable =
    List.filter
      (fun (diagnostic : Diagnostic.t) -> Option.is_some diagnostic.replacement)
      diagnostics
  in
  let module Files = Map.Make (String) in
  let by_file =
    List.fold_left
      (fun files (diagnostic : Diagnostic.t) ->
        Files.update diagnostic.loc.file
          (fun diagnostics -> Some (diagnostic :: Option.value ~default:[] diagnostics))
          files)
      Files.empty fixable
  in
  (* Counts are summed per reason rather than concatenated, or a run over three
     files reports "0 invalid, 1 overlapping, 2 invalid" and so on. *)
  let merge_reasons existing added =
    List.fold_left
      (fun acc (reason, count) ->
        let previous = Option.value (List.assoc_opt reason acc) ~default:0 in
        (reason, previous + count) :: List.remove_assoc reason acc)
      existing added
  in
  let result =
    Files.fold
      (fun path diagnostics result ->
        let applied, skipped, reasons, errors = apply_file path diagnostics in
        {
          applied = result.applied + applied;
          skipped = result.skipped + skipped;
          errors = result.errors @ errors;
          skipped_reasons = merge_reasons result.skipped_reasons reasons;
        })
      by_file
      { applied = 0; skipped = 0; errors = []; skipped_reasons = [] }
  in
  (* Reasons that did not occur are noise in the summary line. *)
  {
    result with
    skipped_reasons = List.filter (fun (_, count) -> count > 0) result.skipped_reasons;
  }
