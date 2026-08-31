open Lintocaml_engine

let colour = ref true
let sgr code s = if !colour then Fmt.str "\027[%sm%s\027[0m" code s else s
let bold = sgr "1"
let dim = sgr "2"

let severity_tag = function
  | Severity.Error -> sgr "1;31" "error"
  | Severity.Warning -> sgr "1;33" "warning"
  | Severity.Hint -> sgr "1;36" "hint"

(* Findings cluster in a handful of files, so read each one once per render.
   Keeping this cache local also prevents a second render in the same process
   from showing source text that has since changed on disk. *)
let source_lines cache file =
  match Hashtbl.find_opt cache file with
  | Some cached -> cached
  | None ->
      let lines =
        (* Binary, not text: the compiler reports columns as byte offsets, and
           text mode would translate CRLF and shift every caret on Windows.
           Source_text reads the same way for the same reason. *)
        match In_channel.with_open_bin file In_channel.input_all with
        | contents -> Some (Array.of_list (String.split_on_char '\n' contents))
        | exception Sys_error _ -> None
      in
      Hashtbl.replace cache file lines;
      lines

let source_line cache file lineno =
  match source_lines cache file with
  | Some lines when lineno >= 1 && lineno <= Array.length lines -> Some lines.(lineno - 1)
  | _ -> None

(* Indent the caret with the source line's own leading characters so that a tab
   lines up as a tab. Padding with spaces misplaces the caret on any line that
   is not entirely space-indented. *)
let caret_indent line column =
  String.init
    (min column (String.length line))
    (fun i -> match line.[i] with '\t' -> '\t' | _ -> ' ')

let pp_diagnostic cache ppf (d : Diagnostic.t) =
  Fmt.pf ppf "%a: %s: %s@." Loc.pp d.loc (severity_tag d.severity) d.message;
  (match source_line cache d.loc.file d.loc.line with
  | Some line ->
      let column = max 0 d.loc.col in
      let caret_len =
        if d.loc.end_line <> d.loc.line then 1
        else
          let start = min column (String.length line) in
          let available = max 1 (String.length line - start) in
          if d.loc.end_col <= column then 1 else min available (d.loc.end_col - column)
      in
      Fmt.pf ppf "  %s %s@." (dim "|") line;
      Fmt.pf ppf "  %s %s%s@." (dim "|") (caret_indent line column)
        (sgr "1;31" (String.make caret_len '^'))
  | None -> ());
  (match d.suggestion with
  | Some s -> Fmt.pf ppf "  %s %s@." (bold "hint:") s
  | None -> ());
  (match d.replacement with
  | Some _ -> Fmt.pf ppf "  %s available with --fix@." (bold "fix:")
  | None -> ());
  Fmt.pf ppf "  %s@.@." (dim (Fmt.str "rule: %s" d.rule_id))

let pp_summary ppf (o : Analyse.outcome) =
  let e, w, h =
    List.fold_left
      (fun (e, w, h) (d : Diagnostic.t) ->
        match d.severity with
        | Severity.Error -> (e + 1, w, h)
        | Severity.Warning -> (e, w + 1, h)
        | Severity.Hint -> (e, w, h + 1))
      (0, 0, 0) o.diagnostics
  in
  let plural count singular = if count = 1 then singular else singular ^ "s" in
  if o.diagnostics = [] then
    Fmt.pf ppf "%s no findings in %d %s@." (sgr "1;32" "clean:") o.files_analysed
      (plural o.files_analysed "file")
  else
    Fmt.pf ppf "%s %d %s, %d %s, %d %s across %d %s@." (bold "summary:") e
      (plural e "error") w (plural w "warning") h (plural h "hint") o.files_analysed
      (plural o.files_analysed "file")

let pp_suppressed ppf (suppressed : Analyse.suppressed) =
  Fmt.pf ppf "%a: suppressed: %s" Loc.pp suppressed.loc suppressed.rule_id;
  (match suppressed.reason with Some reason -> Fmt.pf ppf " (%s)" reason | None -> ());
  Fmt.pf ppf "@."

let pp ~report_suppressed ppf (o : Analyse.outcome) =
  let source_cache = Hashtbl.create 8 in
  List.iter (pp_diagnostic source_cache ppf) o.diagnostics;
  if report_suppressed then List.iter (pp_suppressed ppf) o.suppressed;
  pp_summary ppf o
