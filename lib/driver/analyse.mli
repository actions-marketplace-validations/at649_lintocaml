(** Running the rule set over compiler artifacts. *)

type outcome = {
  diagnostics : Lintocaml_engine.Diagnostic.t list;
  suppressed : suppressed list;
  files_analysed : int;
  cmt_files_found : int;
  load_errors : (string * Lintocaml_engine.Tast_iface.load_error) list;
      (** Artifacts that could not be read, paired with the reason. These are reported as
          warnings: one unreadable artifact must not fail a run. *)
}

and suppressed = {
  rule_id : string;
  loc : Lintocaml_engine.Loc.t;
  reason : string option;
}

val run :
  cfg:Lintocaml_engine.Config.t ->
  rules:Lintocaml_engine.Rule.t list ->
  roots:string list ->
  outcome
(** Discovers [.cmt] and [.cmti] files under [roots] and applies every rule the
    configuration enables. Diagnostics come back sorted and deduplicated, so identical
    input yields byte-identical output. *)
