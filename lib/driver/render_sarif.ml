open Lintocaml_engine

(* SARIF 2.1.0 is what GitHub code scanning consumes, which puts findings
   inline on pull requests with no custom CI glue. *)

let level_of = function
  | Severity.Error -> "error"
  | Severity.Warning -> "warning"
  | Severity.Hint -> "note"

let rule_descriptor (r : Rule.t) =
  `Assoc
    [
      ("id", `String r.id);
      ("name", `String r.title);
      ("shortDescription", `Assoc [ ("text", `String r.title) ]);
      ("fullDescription", `Assoc [ ("text", `String r.docs) ]);
      ("defaultConfiguration", `Assoc [ ("level", `String (level_of r.default_severity)) ]);
      ("properties", `Assoc [ ("category", `String (Rule.category_to_string r.category)) ]);
    ]

let result_of (d : Diagnostic.t) =
  let fields =
    [
      ("ruleId", `String d.rule_id);
      ("level", `String (level_of d.severity));
      ( "message",
        `Assoc
          [
            ( "text",
              `String
                (match d.suggestion with
                | Some s -> Fmt.str "%s. Hint: %s" d.message s
                | None -> d.message) );
          ] );
      ( "locations",
        `List
          [
            `Assoc
              [
                ( "physicalLocation",
                  `Assoc
                    [
                      ("artifactLocation", `Assoc [ ("uri", `String d.loc.file) ]);
                      ( "region",
                        `Assoc
                          [
                            ("startLine", `Int d.loc.line);
                            ("startColumn", `Int (d.loc.col + 1));
                          ] );
                    ] );
              ];
          ] );
    ]
  in
  let fields =
    match d.replacement with
    | None -> fields
    | Some replacement ->
        fields
        @ [
            ( "fixes",
              `List
                [
                  `Assoc
                    [
                      ( "description",
                        `Assoc [ ("text", `String "Apply lintocaml's safe fix") ] );
                      ( "artifactChanges",
                        `List
                          [
                            `Assoc
                              [
                                ( "artifactLocation",
                                  `Assoc [ ("uri", `String d.loc.file) ] );
                                ( "replacements",
                                  `List
                                    [
                                      `Assoc
                                        [
                                          ( "deletedRegion",
                                            `Assoc
                                              [
                                                ("startLine", `Int d.loc.line);
                                                ("startColumn", `Int (d.loc.col + 1));
                                                ("endLine", `Int d.loc.end_line);
                                                ("endColumn", `Int (d.loc.end_col + 1));
                                              ] );
                                          ( "insertedContent",
                                            `Assoc [ ("text", `String replacement) ] );
                                        ];
                                    ] );
                              ];
                          ] );
                    ];
                ] );
          ]
  in
  `Assoc fields

let suppressed_result (suppressed : Analyse.suppressed) =
  let suppression =
    [ ("kind", `String "inSource") ]
    @
    match suppressed.reason with
    | None -> []
    | Some reason -> [ ("justification", `String reason) ]
  in
  `Assoc
    [
      ("ruleId", `String suppressed.rule_id);
      ("level", `String "none");
      ("message", `Assoc [ ("text", `String "finding suppressed in source") ]);
      ("suppressions", `List [ `Assoc suppression ]);
      ( "locations",
        `List
          [
            `Assoc
              [
                ( "physicalLocation",
                  `Assoc
                    [
                      ("artifactLocation", `Assoc [ ("uri", `String suppressed.loc.file) ]);
                      ( "region",
                        `Assoc
                          [
                            ("startLine", `Int suppressed.loc.line);
                            ("startColumn", `Int (suppressed.loc.col + 1));
                          ] );
                    ] );
              ];
          ] );
    ]

let render ~version ~rules ~report_suppressed (o : Analyse.outcome) =
  let results = List.map result_of o.diagnostics in
  let results =
    if report_suppressed then results @ List.map suppressed_result o.suppressed
    else results
  in
  `Assoc
    [
      ("$schema", `String "https://json.schemastore.org/sarif-2.1.0.json");
      ("version", `String "2.1.0");
      ( "runs",
        `List
          [
            `Assoc
              [
                ( "tool",
                  `Assoc
                    [
                      ( "driver",
                        `Assoc
                          [
                            ("name", `String "lintocaml");
                            ("version", `String version);
                            ("semanticVersion", `String version);
                            ( "informationUri",
                              `String "https://github.com/at649/lintocaml" );
                            ("rules", `List (List.map rule_descriptor rules));
                          ] );
                    ] );
                ("results", `List results);
              ];
          ] );
    ]
  |> Yojson.Safe.pretty_to_string
