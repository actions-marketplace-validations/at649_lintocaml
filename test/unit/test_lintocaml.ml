open Lintocaml_engine

let known = Lintocaml_rules.Registry.ids

let test_config_unknown_rule () =
  match Config.parse_string ~known_rule_ids:known "no-such-rule = error" with
  | Ok _ -> Alcotest.fail "an unknown rule id must be a hard error, not a silent ignore"
  | Error errs ->
      Alcotest.(check (list string))
        "reports the exact bad id"
        [ "line 1: unknown rule id \"no-such-rule\"" ]
        errs

let test_config_unknown_severity () =
  match Config.parse_string ~known_rule_ids:known "partial-function = loud" with
  | Ok _ -> Alcotest.fail "unknown severity must be rejected"
  | Error _ -> ()

let test_config_off () =
  match Config.parse_string ~known_rule_ids:known "partial-function = off" with
  | Error e -> Alcotest.fail (String.concat "; " e)
  | Ok cfg ->
      let r = Option.get (Lintocaml_rules.Registry.find "partial-function") in
      Alcotest.(check bool) "explicit off disables" true (Config.severity_for cfg r = None)

let test_config_comments_and_blanks () =
  match
    Config.parse_string ~known_rule_ids:known "# a comment\n\n  profile = idiomatic  \n"
  with
  | Error e -> Alcotest.fail (String.concat "; " e)
  | Ok cfg ->
      Alcotest.(check bool)
        "profile parsed" true
        (Config.profile cfg = Config.Profile_idiomatic)

let test_toml_syntax () =
  let source = "profile = \"idiomatic\"\n[rules]\npartial-function = \"off\"\n" in
  match Config.parse_string ~known_rule_ids:known source with
  | Error errors -> Alcotest.fail (String.concat "; " errors)
  | Ok cfg ->
      let rule = Option.get (Lintocaml_rules.Registry.find "partial-function") in
      Alcotest.(check bool)
        "quoted profile" true
        (Config.profile cfg = Config.Profile_idiomatic);
      Alcotest.(check bool) "rules section" true (Config.severity_for cfg rule = None)

let test_last_override_wins () =
  let source = "partial-function = off\npartial-function = error" in
  match Config.parse_string ~known_rule_ids:known source with
  | Error errors -> Alcotest.fail (String.concat "; " errors)
  | Ok cfg ->
      let rule = Option.get (Lintocaml_rules.Registry.find "partial-function") in
      Alcotest.(check bool)
        "last setting wins" true
        (Config.severity_for cfg rule = Some Severity.Error)

let test_default_profile_hides_idiom_rules () =
  let cfg = Config.default in
  let idiom = Option.get (Lintocaml_rules.Registry.find "redundant-if-bool") in
  let bug = Option.get (Lintocaml_rules.Registry.find "physical-eq-on-boxed") in
  Alcotest.(check bool)
    "idiom rule off by default" true
    (Config.severity_for cfg idiom = None);
  Alcotest.(check bool) "bug rule on by default" true (Config.severity_for cfg bug <> None)

let test_profiles_are_distinct () =
  let idiom = Option.get (Lintocaml_rules.Registry.find "redundant-if-bool") in
  let pedantic = Option.get (Lintocaml_rules.Registry.find "generic-failure") in
  let idiomatic_cfg = Config.with_profile Config.Profile_idiomatic Config.default in
  let pedantic_cfg = Config.with_profile Config.Profile_pedantic Config.default in
  Alcotest.(check bool)
    "idiomatic includes idiom rules" true
    (Config.severity_for idiomatic_cfg idiom <> None);
  Alcotest.(check bool)
    "idiomatic excludes pedantic rules" true
    (Config.severity_for idiomatic_cfg pedantic = None);
  Alcotest.(check bool)
    "pedantic includes pedantic rules" true
    (Config.severity_for pedantic_cfg pedantic <> None)

let parse_config source =
  match Config.parse_string ~known_rule_ids:known source with
  | Ok cfg -> cfg
  | Error errors -> Alcotest.fail (String.concat "; " errors)

let test_path_override_disables_matching_files () =
  let cfg =
    parse_config
      "profile = \"default\"\n\
       [[overrides]]\n\
       paths = [\"test/**\", \"vendor/**\"]\n\
       profile = \"off\"\n"
  in
  let rule = Option.get (Lintocaml_rules.Registry.find "partial-function") in
  Alcotest.(check bool)
    "test file disabled" true
    (Config.severity_for_path cfg rule "test/unit/example.ml" = None);
  Alcotest.(check bool)
    "vendor file disabled" true
    (Config.severity_for_path cfg rule "vendor/library.ml" = None);
  Alcotest.(check bool)
    "source file enabled" true
    (Config.severity_for_path cfg rule "src/example.ml" = Some Severity.Warning)

let test_path_override_rule_beats_profile () =
  let cfg =
    parse_config
      "[[overrides]]\n\
       paths = [\"test/**\"]\n\
       profile = \"off\"\n\
       [overrides.rules]\n\
       partial-function = \"hint\"\n"
  in
  let rule = Option.get (Lintocaml_rules.Registry.find "partial-function") in
  Alcotest.(check bool)
    "specific rule is re-enabled" true
    (Config.severity_for_path cfg rule "test/example.ml" = Some Severity.Hint)

let test_path_overrides_apply_in_order () =
  let cfg =
    parse_config
      "[[overrides]]\n\
       paths = [\"test/**\"]\n\
       profile = \"off\"\n\n\
       [[overrides]]\n\
       paths = [\"test/important/*.ml\"]\n\
       profile = \"default\"\n"
  in
  let rule = Option.get (Lintocaml_rules.Registry.find "partial-function") in
  Alcotest.(check bool)
    "later matching override wins" true
    (Config.severity_for_path cfg rule "test/important/example.ml" = Some Severity.Warning)

let test_glob_matching () =
  Alcotest.(check bool)
    "double star crosses directories" true
    (Config.glob_matches "test/**/generated-?.ml" "test/a/b/generated-x.ml");
  Alcotest.(check bool)
    "single star stays within a directory" false
    (Config.glob_matches "test/*.ml" "test/nested/example.ml");
  Alcotest.(check bool)
    "double star can match nothing" true
    (Config.glob_matches "src/**" "src")

let test_override_requires_paths () =
  match
    Config.parse_string ~known_rule_ids:known "[[overrides]]\nprofile = \"off\"\n"
  with
  | Ok _ -> Alcotest.fail "an override without paths must fail"
  | Error _ -> ()

let test_comments_inside_strings () =
  let cfg =
    parse_config
      "[[overrides]]\npaths = [\"generated/#cache/**\"] # comment\nprofile = \"off\"\n"
  in
  let rule = Option.get (Lintocaml_rules.Registry.find "partial-function") in
  Alcotest.(check bool)
    "hash in quoted path is preserved" true
    (Config.severity_for_path cfg rule "generated/#cache/file.ml" = None)

let test_invalid_path_arrays () =
  List.iter
    (fun value ->
      let source = Fmt.str "[[overrides]]\npaths = %s\nprofile = \"off\"\n" value in
      match Config.parse_string ~known_rule_ids:known source with
      | Ok _ -> Alcotest.failf "invalid paths array was accepted: %s" value
      | Error _ -> ())
    [ "[\"test/**\" \"vendor/**\"]"; "[test/**]"; "[\"unterminated]" ]

(* Alphabetic operators reach us escaped: [mod] resolves to "Stdlib.\\#mod".
   A rule naming "Stdlib.mod" silently never fired until this was handled. *)
let test_path_operator_escape () =
  Alcotest.(check bool)
    "escaped mod matches plain name" true
    (Expr_view.path_is "Stdlib.\\#mod" [ "Stdlib.mod" ]);
  Alcotest.(check bool)
    "escaped lor matches plain name" true
    (Expr_view.path_is "Stdlib.\\#lor" [ "Stdlib.lor" ]);
  Alcotest.(check bool)
    "escape does not create false matches" false
    (Expr_view.path_is "Stdlib.\\#mod" [ "Stdlib.land" ]);
  Alcotest.(check bool)
    "symbolic operators are unaffected" true
    (Expr_view.path_is "Stdlib./" [ "Stdlib./" ])

let test_path_stdlib_prefix () =
  Alcotest.(check bool)
    "Stdlib prefix is transparent" true
    (Expr_view.path_is "Stdlib.List.length" [ "List.length" ]);
  Alcotest.(check bool)
    "and in reverse" true
    (Expr_view.path_is "List.length" [ "Stdlib.List.length" ]);
  Alcotest.(check bool)
    "no false match" false
    (Expr_view.path_is "MyList.length" [ "List.length" ])

let test_severity_threshold () =
  Alcotest.(check bool)
    "error meets warning threshold" true
    (Severity.at_least ~threshold:Severity.Warning Severity.Error);
  Alcotest.(check bool)
    "hint does not meet warning threshold" false
    (Severity.at_least ~threshold:Severity.Warning Severity.Hint)

let test_diagnostic_ordering () =
  let mk file line col rule =
    Diagnostic.make ~rule_id:rule ~severity:Severity.Warning
      ~loc:{ Loc.file; line; col; end_line = line; end_col = col }
      ~message:"m" ()
  in
  let unsorted =
    [ mk "b.ml" 1 0 "z"; mk "a.ml" 9 0 "a"; mk "a.ml" 1 5 "a"; mk "a.ml" 1 0 "b" ]
  in
  let sorted = List.sort Diagnostic.compare unsorted in
  let keys =
    List.map
      (fun (d : Diagnostic.t) -> Fmt.str "%s:%d:%d" d.loc.file d.loc.line d.loc.col)
      sorted
  in
  Alcotest.(check (list string))
    "sorted by file, line, col"
    [ "a.ml:1:0"; "a.ml:1:5"; "a.ml:9:0"; "b.ml:1:0" ]
    keys

let test_diagnostic_comparison_is_total () =
  let loc = { Loc.file = "a.ml"; line = 1; col = 0; end_line = 1; end_col = 1 } in
  let first =
    Diagnostic.make ~rule_id:"rule" ~severity:Severity.Warning ~loc ~message:"first" ()
  in
  let second =
    Diagnostic.make ~rule_id:"rule" ~severity:Severity.Warning ~loc ~message:"second" ()
  in
  Alcotest.(check bool)
    "different findings are not deduplicated" true
    (Diagnostic.compare first second <> 0)

let test_all_rules_documented () =
  List.iter
    (fun (r : Rule.t) ->
      if String.length r.docs < 120 then
        Alcotest.failf "rule %s has no real rationale (%d chars)" r.id
          (String.length r.docs);
      if r.title = "" then Alcotest.failf "rule %s has no title" r.id)
    Lintocaml_rules.Registry.all

let test_rule_ids_unique () =
  let ids = List.sort String.compare Lintocaml_rules.Registry.ids in
  let rec dup = function
    | a :: b :: _ when a = b -> Some a
    | _ :: t -> dup t
    | [] -> None
  in
  match dup ids with Some d -> Alcotest.failf "duplicate rule id %s" d | None -> ()

let test_glob_literal () =
  Alcotest.(check bool) "exact segment" true (Config.glob_matches "src/a.ml" "src/a.ml");
  Alcotest.(check bool) "different file" false (Config.glob_matches "src/a.ml" "src/b.ml");
  Alcotest.(check bool)
    "a single * does not cross a separator" false
    (Config.glob_matches "src/*.ml" "src/deep/a.ml")

let test_glob_star () =
  Alcotest.(check bool) "star in segment" true (Config.glob_matches "src/*.ml" "src/a.ml");
  Alcotest.(check bool)
    "star matches empty" true
    (Config.glob_matches "src/*a.ml" "src/a.ml");
  Alcotest.(check bool) "question mark" true (Config.glob_matches "src/?.ml" "src/a.ml");
  Alcotest.(check bool)
    "question mark needs exactly one" false
    (Config.glob_matches "src/?.ml" "src/ab.ml")

let test_glob_globstar () =
  Alcotest.(check bool)
    "** spans segments" true
    (Config.glob_matches "test/**" "test/a/b/c.ml");
  Alcotest.(check bool)
    "** spans zero segments" true
    (Config.glob_matches "test/**" "test");
  Alcotest.(check bool)
    "** in the middle" true
    (Config.glob_matches "src/**/a.ml" "src/deep/nested/a.ml");
  Alcotest.(check bool)
    "** in the middle spanning nothing" true
    (Config.glob_matches "src/**/a.ml" "src/a.ml");
  Alcotest.(check bool)
    "no spurious match" false
    (Config.glob_matches "src/**/a.ml" "src/b.ml")

let test_glob_pathological () =
  (* Without memoisation this is exponential; it must return promptly. *)
  Alcotest.(check bool)
    "backtracking stays linear" false
    (Config.glob_matches "a*a*a*a*a*a*a*b" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaac")

let test_glob_normalises () =
  Alcotest.(check bool)
    "leading ./ ignored" true
    (Config.glob_matches "src/a.ml" "./src/a.ml");
  Alcotest.(check bool)
    "repeated separators" true
    (Config.glob_matches "src/a.ml" "src//a.ml")

(* Fix.atomic decides whether a spliced expression needs parentheses. Getting
   it wrong produces a fix that changes precedence, so both directions matter. *)
let view desc =
  {
    Expr_view.loc = { Loc.file = "t.ml"; line = 1; col = 0; end_line = 1; end_col = 1 };
    ty = Expr_view.Unknown_class;
    type_name = None;
    desc;
    allowed = [];
  }

let test_fix_atomic () =
  let atomic desc = Fix.atomic (view desc) in
  Alcotest.(check bool) "identifier" true (atomic (Expr_view.Ident "x"));
  Alcotest.(check bool)
    "string literal" true
    (atomic (Expr_view.Const (Expr_view.String "s")));
  Alcotest.(check bool)
    "non-negative int" true
    (atomic (Expr_view.Const (Expr_view.Int 3)));
  Alcotest.(check bool)
    "nullary constructor" true
    (atomic (Expr_view.Construct { name = "None"; args = [] }))

let test_fix_needs_parentheses () =
  let atomic desc = Fix.atomic (view desc) in
  (* [-1] spliced after an operator would read as a subtraction. *)
  Alcotest.(check bool)
    "negative int" false
    (atomic (Expr_view.Const (Expr_view.Int (-1))));
  Alcotest.(check bool)
    "negative float" false
    (atomic (Expr_view.Const (Expr_view.Float "-1.5")));
  Alcotest.(check bool)
    "application" false
    (atomic (Expr_view.Apply { callee = Some "f"; args = [] }));
  Alcotest.(check bool)
    "constructor with arguments" false
    (atomic
       (Expr_view.Construct { name = "Some"; args = [ view (Expr_view.Ident "x") ] }))

let test_fix_source_wraps () =
  let unwrapped = Fix.source (view (Expr_view.Ident "x")) in
  let wrapped = Fix.source (view (Expr_view.Apply { callee = Some "f"; args = [] })) in
  Alcotest.(check bool)
    "atomic is spliced bare" true
    (match unwrapped with Rule.Source _ -> true | _ -> false);
  Alcotest.(check bool)
    "compound is parenthesised" true
    (match wrapped with Rule.Parenthesized_source _ -> true | _ -> false)

(* The overlap guard itself runs in test/run_fixtures.sh, where the fixture
   build is guaranteed present;
   it is not a unit test because the fixture _build is cleaned concurrently by
   the fixture rule. *)

(* The JSON schema is a compatibility promise, so its shape is pinned here.
   Built from a synthetic outcome to avoid depending on the fixture build,
   which the fixture rule cleans concurrently. *)
let test_json_shape () =
  let loc = { Loc.file = "a.ml"; line = 1; col = 0; end_line = 1; end_col = 5 } in
  let diagnostic =
    Diagnostic.make ~rule_id:"example-rule" ~severity:Severity.Warning ~loc
      ~message:"a message" ()
  in
  let outcome =
    {
      Lintocaml_driver.Analyse.diagnostics = [ diagnostic ];
      suppressed = [];
      files_analysed = 1;
      cmt_files_found = 1;
      load_errors = [];
    }
  in
  let json = Lintocaml_driver.Render_json.render ~report_suppressed:true outcome in
  let parsed = Yojson.Safe.from_string json in
  let member name = function `Assoc fields -> List.assoc_opt name fields | _ -> None in
  let () =
    match member "rule_stats" parsed with
    | Some (`Assoc stats) -> (
        match List.assoc_opt "example-rule" stats with
        | Some (`Assoc _) -> ()
        | _ -> Alcotest.fail "rule_stats is missing the example rule")
    | _ -> Alcotest.fail "JSON output is missing the rule_stats object"
  in
  let () =
    match member "diagnostics" parsed with
    | Some (`List (first :: _)) -> (
        match member "end_line" first with
        | Some (`Int _) -> ()
        | _ -> Alcotest.fail "diagnostic JSON is missing end_line")
    | _ -> Alcotest.fail "JSON output is missing diagnostics"
  in
  ()

let () =
  Alcotest.run "lintocaml"
    [
      ( "fix",
        [
          Alcotest.test_case "atomic expressions" `Quick test_fix_atomic;
          Alcotest.test_case "expressions needing parentheses" `Quick
            test_fix_needs_parentheses;
          Alcotest.test_case "source wraps compound expressions" `Quick
            test_fix_source_wraps;
        ] );
      ( "glob",
        [
          Alcotest.test_case "literal" `Quick test_glob_literal;
          Alcotest.test_case "star and question" `Quick test_glob_star;
          Alcotest.test_case "globstar" `Quick test_glob_globstar;
          Alcotest.test_case "pathological backtracking" `Quick test_glob_pathological;
          Alcotest.test_case "path normalisation" `Quick test_glob_normalises;
          Alcotest.test_case "path matching" `Quick test_glob_matching;
        ] );
      ( "config",
        [
          Alcotest.test_case "unknown rule id is fatal" `Quick test_config_unknown_rule;
          Alcotest.test_case "unknown severity is fatal" `Quick
            test_config_unknown_severity;
          Alcotest.test_case "explicit off" `Quick test_config_off;
          Alcotest.test_case "comments and blanks" `Quick test_config_comments_and_blanks;
          Alcotest.test_case "TOML syntax" `Quick test_toml_syntax;
          Alcotest.test_case "last override wins" `Quick test_last_override_wins;
          Alcotest.test_case "default profile hides idiom rules" `Quick
            test_default_profile_hides_idiom_rules;
          Alcotest.test_case "profiles are distinct" `Quick test_profiles_are_distinct;
          Alcotest.test_case "path override disables files" `Quick
            test_path_override_disables_matching_files;
          Alcotest.test_case "rule beats override profile" `Quick
            test_path_override_rule_beats_profile;
          Alcotest.test_case "path override order" `Quick
            test_path_overrides_apply_in_order;
          Alcotest.test_case "override requires paths" `Quick test_override_requires_paths;
          Alcotest.test_case "comments inside strings" `Quick test_comments_inside_strings;
          Alcotest.test_case "invalid path arrays" `Quick test_invalid_path_arrays;
        ] );
      ( "paths",
        [
          Alcotest.test_case "stdlib prefix" `Quick test_path_stdlib_prefix;
          Alcotest.test_case "operator escape" `Quick test_path_operator_escape;
        ] );
      ("severity", [ Alcotest.test_case "threshold" `Quick test_severity_threshold ]);
      ( "determinism",
        [
          Alcotest.test_case "diagnostic ordering" `Quick test_diagnostic_ordering;
          Alcotest.test_case "comparison is total" `Quick
            test_diagnostic_comparison_is_total;
        ] );
      ( "rules",
        [
          Alcotest.test_case "all documented" `Quick test_all_rules_documented;
          Alcotest.test_case "ids unique" `Quick test_rule_ids_unique;
        ] );
      ("json", [ Alcotest.test_case "schema shape" `Quick test_json_shape ]);
    ]
