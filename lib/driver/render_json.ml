open Lintml_engine

let of_diagnostic (d : Diagnostic.t) =
  `Assoc
    [
      ("rule", `String d.rule_id);
      ("severity", `String (Severity.to_string d.severity));
      ("file", `String d.loc.file);
      ("line", `Int d.loc.line);
      ("column", `Int (d.loc.col + 1));
      ("end_line", `Int d.loc.end_line);
      ("end_column", `Int (d.loc.end_col + 1));
      ("message", `String d.message);
      ("suggestion", match d.suggestion with Some s -> `String s | None -> `Null);
      ("replacement", match d.replacement with Some s -> `String s | None -> `Null);
    ]

let of_suppressed (suppressed : Analyse.suppressed) =
  `Assoc
    [
      ("rule", `String suppressed.rule_id);
      ("file", `String suppressed.loc.file);
      ("line", `Int suppressed.loc.line);
      ("column", `Int (suppressed.loc.col + 1));
      ( "reason",
        match suppressed.reason with Some reason -> `String reason | None -> `Null );
    ]

(* Per-rule counts, so a noisy rule can be spotted without reading every
   finding. *)
let rule_stats (o : Analyse.outcome) =
  let module Counts = Map.Make (String) in
  let tally ids =
    List.fold_left
      (fun counts id ->
        Counts.update id (function None -> Some 1 | Some n -> Some (n + 1)) counts)
      Counts.empty ids
  in
  let findings = tally (List.map (fun (d : Diagnostic.t) -> d.rule_id) o.diagnostics) in
  let suppressed =
    tally (List.map (fun (s : Analyse.suppressed) -> s.rule_id) o.suppressed)
  in
  let rules =
    Counts.merge
      (fun _rule findings suppressed ->
        match (findings, suppressed) with
        | None, None -> None
        | findings, suppressed ->
            Some
              (`Assoc
                 [
                   ("findings", `Int (Option.value ~default:0 findings));
                   ("suppressed", `Int (Option.value ~default:0 suppressed));
                 ]))
      findings suppressed
  in
  `Assoc (Counts.bindings rules)

(* Artifacts that could not be read. Text output warns about these on stderr;
   without them here a tool consuming JSON cannot tell a clean run from one
   that skipped half the project. *)
let of_load_error (path, error) =
  `Assoc
    [
      ("file", `String path);
      ( "error",
        `String
          (match error with
          | Tast_iface.Unsupported_compiler version ->
              Fmt.str "produced by unsupported compiler %s" version
          | Tast_iface.Invalid_cmt message -> message) );
    ]

let render ~report_suppressed (o : Analyse.outcome) =
  let fields =
    [
      ("schema_version", `String "1");
      ("files_analysed", `Int o.files_analysed);
      ("diagnostics", `List (List.map of_diagnostic o.diagnostics));
      ("rule_stats", rule_stats o);
      ("load_errors", `List (List.map of_load_error o.load_errors));
    ]
  in
  let fields =
    if report_suppressed then
      fields @ [ ("suppressed", `List (List.map of_suppressed o.suppressed)) ]
    else fields
  in
  `Assoc fields |> Yojson.Safe.pretty_to_string
