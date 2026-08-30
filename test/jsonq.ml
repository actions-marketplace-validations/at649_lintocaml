(* Assertions over lintml's own JSON and SARIF output, for run_fixtures.sh.

   These checks used to be inline python3. Nothing in the package needs python,
   and the opam builders do not ship it, so every fixture assertion silently
   turned into a failure there. Yojson is already a dependency, so asking OCaml
   the same questions costs nothing and runs wherever lintml itself builds. *)

let die message =
  prerr_endline ("jsonq: " ^ message);
  exit 1

let input = lazy (Yojson.Safe.from_channel stdin)
let root () = Lazy.force input

let field name json =
  match json with
  | `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null
  | _ -> `Null

let list_of = function `List items -> items | _ -> []
let string_of = function `String s -> s | _ -> ""
let int_of = function `Int n -> n | _ -> min_int
let diagnostics () = list_of (field "diagnostics" (root ()))
let suppressed () = list_of (field "suppressed" (root ()))
let rule_of item = string_of (field "rule" item)
let line_of item = int_of (field "line" item)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  n = 0
  ||
  let rec scan i = i + n <= h && (String.sub haystack i n = needle || scan (i + 1)) in
  scan 0

let check condition message = if condition then exit 0 else die message

let () =
  match Array.to_list Sys.argv with
  | _ :: "has" :: rule :: line :: _ ->
      let line = int_of_string line in
      check
        (List.exists (fun d -> rule_of d = rule && line_of d = line) (diagnostics ()))
        (Printf.sprintf "no %s at line %d" rule line)
  | _ :: "rule-line-clean" :: rule :: line :: _ ->
      let line = int_of_string line in
      check
        (not
           (List.exists (fun d -> rule_of d = rule && line_of d = line) (diagnostics ())))
        (Printf.sprintf "%s fired at line %d" rule line)
  | _ :: "line-clean" :: line :: _ ->
      let line = int_of_string line in
      let fired =
        List.filter_map
          (fun d -> if line_of d = line then Some (rule_of d) else None)
          (diagnostics ())
      in
      if fired <> [] then prerr_endline ("   -> fired: " ^ String.concat ", " fired);
      check (fired = []) (Printf.sprintf "line %d not clean" line)
  | _ :: "suggests" :: line :: needle :: _ ->
      let line = int_of_string line in
      check
        (List.exists
           (fun d ->
             line_of d = line && contains ~needle (string_of (field "suggestion" d)))
           (diagnostics ()))
        (Printf.sprintf "line %d does not suggest %s" line needle)
  | _ :: "has-in-file" :: rule :: suffix :: line :: _ ->
      let line = int_of_string line in
      check
        (List.exists
           (fun d ->
             rule_of d = rule
             && String.ends_with ~suffix (string_of (field "file" d))
             && line_of d = line)
           (diagnostics ()))
        (Printf.sprintf "no %s in *%s at line %d" rule suffix line)
  | _ :: "no-diagnostics" :: _ -> check (diagnostics () = []) "expected no diagnostics"
  | _ :: "has-diagnostics" :: _ ->
      check (diagnostics () <> []) "expected at least one diagnostic"
  | _ :: "replacement-metadata" :: _ ->
      let d = diagnostics () in
      check
        (List.exists
           (fun x ->
             rule_of x = "constant-condition"
             && string_of (field "replacement" x) = "value")
           d
        && List.exists (fun x -> field "replacement" x = `Null) d)
        "JSON replacement metadata is incomplete"
  | _ :: "suppressed-reason" :: line :: reason :: _ ->
      let line = int_of_string line in
      check
        (List.exists
           (fun s -> line_of s = line && string_of (field "reason" s) = reason)
           (suppressed ()))
        (Printf.sprintf "no suppression at line %d with that reason" line)
  | _ :: "no-overlap" :: _ ->
      let key d =
        Printf.sprintf "%s:%d:%d"
          (string_of (field "file" d))
          (line_of d)
          (int_of (field "column" d))
      in
      let table = Hashtbl.create 64 in
      List.iter
        (fun d ->
          let k = key d in
          Hashtbl.replace table k
            (rule_of d :: Option.value ~default:[] (Hashtbl.find_opt table k)))
        (diagnostics ());
      let overlaps =
        Hashtbl.fold
          (fun k rules acc ->
            if List.compare_length_with rules 1 > 0 then (k, rules) :: acc else acc)
          table []
      in
      List.iter
        (fun (k, rules) -> prerr_endline ("   -> " ^ k ^ ": " ^ String.concat ", " rules))
        (List.sort compare overlaps);
      check (overlaps = []) "a location was reported by more than one rule"
  | _ :: "load-error" :: suffix :: _ -> (
      let errors = list_of (field "load_errors" (root ())) in
      match errors with
      | [ e ] ->
          let file = string_of (field "file" e)
          and message = string_of (field "error" e) in
          check
            (String.ends_with ~suffix file && not (contains ~needle:"Cmi_format" message))
            ("unhelpful load error: " ^ message)
      | _ ->
          die
            (Printf.sprintf "expected exactly 1 load error, got %d" (List.length errors)))
  | _ :: "sarif-structure" :: expected_rules :: _ ->
      let expected_rules = int_of_string expected_rules in
      let d = root () in
      let run = match list_of (field "runs" d) with r :: _ -> r | [] -> `Null in
      let declared =
        List.map
          (fun r -> string_of (field "id" r))
          (list_of (field "rules" (field "driver" (field "tool" run))))
      in
      let results = list_of (field "results" run) in
      if string_of (field "version" d) <> "2.1.0" then die "SARIF version is not 2.1.0";
      if List.length declared <> expected_rules then
        die
          (Printf.sprintf "declared %d rules, registry has %d" (List.length declared)
             expected_rules);
      if
        not
          (List.for_all
             (fun r -> List.mem (string_of (field "ruleId" r)) declared)
             results)
      then die "a result references a rule with no metadata";
      if results = [] then die "SARIF carried no results";
      check
        (List.exists (fun r -> list_of (field "fixes" r) <> []) results)
        "no SARIF result carried a fix"
  | _ :: "sarif-suppressions" :: _ ->
      let run = match list_of (field "runs" (root ())) with r :: _ -> r | [] -> `Null in
      let suppressed =
        List.filter
          (fun r -> list_of (field "suppressions" r) <> [])
          (list_of (field "results" run))
      in
      let first r =
        match list_of (field "suppressions" r) with s :: _ -> s | [] -> `Null
      in
      if suppressed = [] then die "no suppressed SARIF results";
      if
        not
          (List.for_all
             (fun r -> string_of (field "kind" (first r)) = "inSource")
             suppressed)
      then die "a suppression was not inSource";
      check
        (List.for_all
           (fun r -> string_of (field "justification" (first r)) <> "")
           suppressed)
        "a suppression carried no justification"
  | _ :: command :: _ -> die ("unknown command " ^ command)
  | _ -> die "usage: jsonq COMMAND [ARGS] < json"
