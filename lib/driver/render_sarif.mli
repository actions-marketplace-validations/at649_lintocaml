(** SARIF 2.1.0, which GitHub code scanning consumes to place findings inline on pull
    requests. *)

val render :
  version:string ->
  rules:Lintocaml_engine.Rule.t list ->
  report_suppressed:bool ->
  Analyse.outcome ->
  string
(** Declares every rule in [rules], not only those that produced findings, so that rule
    metadata stays stable between runs. [version] identifies the analyser to code
    scanning, which otherwise cannot tell one lintocaml build's findings from another's.
*)
