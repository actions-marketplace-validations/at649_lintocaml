open Lintml_engine
open Lintml_driver

let exit_ok = 0
let exit_findings = 1
let exit_usage = 2
let exit_no_cmt = 3
let exit_unsupported_compiler = 4
let exit_internal = 10

(* Stamped by dune from the (version) field in dune-project, so cutting a
   release does not mean remembering to edit this file too. *)
let version =
  match Build_info.V1.version () with
  | Some v -> Build_info.V1.Version.to_string v
  | None -> "dev"

type output_format = Text | Json | Sarif

let output_format_of_string = function
  | "text" -> Some Text
  | "json" -> Some Json
  | "sarif" -> Some Sarif
  | _ -> None

let threshold_of_string = function
  | "never" -> Some None
  | value -> Option.map Option.some (Severity.of_string value)

let read_config_file path =
  try Some (In_channel.with_open_text path In_channel.input_all)
  with Sys_error _ -> None

let absolute_path path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  match Unix.realpath path with path -> path | exception Unix.Unix_error _ -> path

let find_config_file roots =
  let rec search directory =
    let toml = Filename.concat directory "lintml.toml" in
    if Sys.file_exists toml then Some toml
    else
      let parent = Filename.dirname directory in
      if String.equal parent directory then None else search parent
  in
  let candidates =
    (Sys.getcwd () :: List.map absolute_path roots)
    @ List.map absolute_path (Option.to_list (Sys.getenv_opt "DUNE_SOURCEROOT"))
  in
  List.find_map search candidates

let parse_config_file ~known_rule_ids path =
  match read_config_file path with
  | None -> Error [ Fmt.str "cannot read config file %S" path ]
  | Some source ->
      Result.map
        (Config.with_root_dir (Filename.dirname (absolute_path path)))
        (Config.parse_string ~known_rule_ids source)

let load_config ~roots ~config_path ~profile_override =
  let known_rule_ids = Lintml_rules.Registry.ids in
  let base =
    match config_path with
    | None -> (
        match find_config_file roots with
        | None -> Ok Config.default
        | Some path -> parse_config_file ~known_rule_ids path)
    | Some path -> parse_config_file ~known_rule_ids path
  in
  match (base, profile_override) with
  | Error e, _ -> Error e
  | Ok cfg, None -> Ok cfg
  | Ok cfg, Some p -> Ok (Config.with_profile p cfg)

let run roots format profile_str config_path fail_on report_suppressed fix no_colour =
  Render_text.colour := not no_colour;
  let roots = if roots = [] then [ "_build" ] else roots in
  let profile_override = Option.bind profile_str Config.profile_of_string in
  let output_format = output_format_of_string format in
  let threshold = threshold_of_string fail_on in
  match (profile_str, profile_override, threshold, output_format) with
  | Some bad, None, _, _ ->
      Fmt.epr "lintml: unknown profile %S@." bad;
      exit_usage
  | _, _, None, _ ->
      Fmt.epr "lintml: unknown --fail-on value %S@." fail_on;
      exit_usage
  | _, _, _, None ->
      Fmt.epr "lintml: unknown --format value %S@." format;
      exit_usage
  | _, _, Some threshold, Some output_format -> (
      match load_config ~roots ~config_path ~profile_override with
      | Error errs ->
          List.iter (fun e -> Fmt.epr "lintml: config: %s@." e) errs;
          exit_usage
      | Ok cfg -> (
          let rules = Lintml_rules.Registry.all in
          let outcome = Analyse.run ~cfg ~rules ~roots in
          if outcome.cmt_files_found = 0 then (
            Fmt.epr
              "lintml: no .cmt or .cmti files found under %s@.\n\
               lintml reads the typed AST the compiler already produces.@.\n\
               Build the project first:@.@.    dune build @@check@.@."
              (String.concat ", " roots);
            exit_no_cmt)
          else
            (* An unreadable artifact is a warning, not the end of the run. One
               stale .cmt in a vendored directory must not suppress the
               findings from every other file. The dedicated exit codes are
               reserved for the case where nothing could be analysed at all. *)
            let all_unsupported =
              outcome.load_errors <> []
              && List.for_all
                   (function
                     | _, Tast_iface.Unsupported_compiler _ -> true
                     | _, Tast_iface.Invalid_cmt _ -> false)
                   outcome.load_errors
            in
            List.iter
              (fun (path, error) ->
                match error with
                | Tast_iface.Unsupported_compiler version ->
                    Fmt.epr
                      "lintml: warning: %s was produced by unsupported compiler %s@." path
                      version
                | Tast_iface.Invalid_cmt message ->
                    Fmt.epr "lintml: warning: cannot read %s: %s@." path message)
              outcome.load_errors;
            let nothing_analysed = outcome.files_analysed = 0 in
            if nothing_analysed then
              if all_unsupported then exit_unsupported_compiler else exit_internal
            else
              let fix_result =
                if fix then Some (Apply_fixes.run outcome.diagnostics) else None
              in
              let fix_failed =
                match fix_result with
                | None -> false
                | Some result ->
                    List.iter
                      (fun error -> Fmt.epr "lintml: fix: %s@." error)
                      result.errors;
                    (if result.errors = [] then
                       let reasons =
                         List.map
                           (fun (reason, count) ->
                             Fmt.str "%d %s" count
                               (Apply_fixes.skip_reason_to_string reason))
                           result.skipped_reasons
                       in
                       let skipped =
                         match reasons with
                         | [] -> ""
                         | _ -> Fmt.str " (%s)" (String.concat ", " reasons)
                       in
                       Fmt.epr
                         "lintml: applied %d fix(es), skipped %d%s.@.Rebuild before \
                          linting again.@."
                         result.applied result.skipped skipped);
                    result.errors <> []
              in
              let () =
                match output_format with
                | Json -> print_string (Render_json.render ~report_suppressed outcome)
                | Sarif ->
                    print_string
                      (Render_sarif.render ~version ~rules ~report_suppressed outcome)
                | Text -> Render_text.pp ~report_suppressed Fmt.stdout outcome
              in
              if fix_failed then exit_internal
              else
                match threshold with
                | None -> exit_ok
                | Some threshold ->
                    if
                      List.exists
                        (fun (d : Diagnostic.t) ->
                          Severity.at_least ~threshold d.severity)
                        outcome.diagnostics
                    then exit_findings
                    else exit_ok))

let cmd_explain rule_id =
  match Lintml_rules.Registry.find rule_id with
  | None ->
      Fmt.epr "lintml: no such rule %S@.@.Known rules:@." rule_id;
      List.iter (fun id -> Fmt.epr "  %s@." id) Lintml_rules.Registry.ids;
      exit_usage
  | Some r ->
      Fmt.pr "@[<v>%s@,%s@,@,category: %s@,default severity: %s@,profile: %s@,@,%s@]@."
        r.id
        (String.make (String.length r.id) '=')
        (Rule.category_to_string r.category)
        (Severity.to_string r.default_severity)
        (match r.profile with
        | Rule.Default -> "default"
        | Rule.Idiomatic -> "idiomatic"
        | Rule.Pedantic -> "pedantic")
        r.docs;
      exit_ok

let cmd_list_rules profile_str =
  let profile =
    match profile_str with
    | None -> Some Config.Profile_default
    | Some value -> Config.profile_of_string value
  in
  match profile with
  | None ->
      let value = Option.value ~default:"" profile_str in
      Fmt.epr "lintml: unknown profile %S@." value;
      exit_usage
  | Some profile ->
      let cfg = Config.with_profile profile Config.default in
      List.iter
        (fun (r : Rule.t) ->
          if Config.severity_for cfg r <> None then
            Fmt.pr "%-24s %-12s %-10s %s@." r.id
              (Rule.category_to_string r.category)
              (match r.profile with
              | Rule.Default -> "default"
              | Rule.Idiomatic -> "idiomatic"
              | Rule.Pedantic -> "pedantic")
              r.title)
        Lintml_rules.Registry.all;
      exit_ok

open Cmdliner

let exits =
  let info code doc = Cmd.Exit.info code ~doc in
  [
    info exit_ok "No finding reached the configured threshold.";
    info exit_findings "At least one finding reached the configured threshold.";
    info exit_usage "The command line or configuration is invalid.";
    info exit_no_cmt "No compiler artifacts were found.";
    info exit_unsupported_compiler
      "Every compiler artifact was produced by an unsupported OCaml version.";
    info exit_internal "No artifact could be read, or an internal operation failed.";
  ]

let roots_arg =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"PATH"
        ~doc:"Directories to scan for .cmt and .cmti files (default: _build).")

let format_arg =
  Arg.(
    value
    & opt string "text"
    & info [ "format" ] ~docv:"FMT" ~doc:"Output format: text, json, or sarif.")

let profile_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "profile" ] ~docv:"P" ~doc:"Rule profile: default, idiomatic, or pedantic.")

let config_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "config" ] ~docv:"FILE" ~doc:"Path to a lintml config file.")

let fail_on_arg =
  Arg.(
    value
    & opt string "error"
    & info [ "fail-on" ] ~docv:"SEV"
        ~doc:"Exit non-zero at this severity or worse: error, warning, hint, never.")

let no_colour_arg =
  Arg.(value & flag & info [ "no-color"; "no-colour" ] ~doc:"Disable coloured output.")

let report_suppressed_arg =
  Arg.(
    value
    & flag
    & info [ "report-suppressed" ]
        ~doc:"Include findings suppressed by lintml.allow attributes.")

let fix_arg =
  Arg.(
    value
    & flag
    & info [ "fix" ]
        ~doc:"Apply the mechanically safe subset of available fixes atomically.")

let lint_term =
  Term.(
    const run
    $ roots_arg
    $ format_arg
    $ profile_arg
    $ config_arg
    $ fail_on_arg
    $ report_suppressed_arg
    $ fix_arg
    $ no_colour_arg)

let lint_cmd =
  let doc = "Lint an OCaml project" in
  Cmd.v (Cmd.info "lint" ~doc ~exits) lint_term

let explain_cmd =
  let doc = "Explain a rule and why it exists" in
  let rule_arg = Arg.(required & pos 0 (some string) None & info [] ~docv:"RULE-ID") in
  Cmd.v (Cmd.info "explain" ~doc ~exits) Term.(const cmd_explain $ rule_arg)

let list_cmd =
  let doc = "List rules enabled by a profile" in
  Cmd.v (Cmd.info "list-rules" ~doc ~exits) Term.(const cmd_list_rules $ profile_arg)

let main =
  let doc = "An OCaml linter that works from the typed AST" in
  let info = Cmd.info "lintml" ~version ~doc ~exits in
  Cmd.group info ~default:lint_term [ lint_cmd; explain_cmd; list_cmd ]

let () =
  let code =
    match Cmd.eval_value main with
    | Ok (`Ok code) -> code
    | Ok (`Help | `Version) -> exit_ok
    | Error (`Parse | `Term) -> exit_usage
    | Error `Exn -> exit_internal
  in
  exit code
