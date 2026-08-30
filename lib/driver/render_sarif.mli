(** SARIF 2.1.0, which GitHub code scanning consumes to place findings inline on pull
    requests. *)

val render :
  rules:Lintml_engine.Rule.t list -> report_suppressed:bool -> Analyse.outcome -> string
(** Declares every rule in [rules], not only those that produced findings, so that rule
    metadata stays stable between runs. *)
