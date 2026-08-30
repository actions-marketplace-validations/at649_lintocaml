(** A single reported finding, after the engine has resolved severity. *)

type t = private {
  rule_id : string;
  severity : Severity.t;
  loc : Loc.t;
  message : string;
  suggestion : string option;
  replacement : string option;
      (** Replacement source text, when the rule offers a mechanically safe fix. *)
  expected_source : string option;
      (** The source the rule saw. [--fix] compares this against the file on disk and
          declines to edit when they differ, so a stale compiler artifact cannot corrupt a
          source file. *)
}

val make :
  rule_id:string ->
  severity:Severity.t ->
  loc:Loc.t ->
  message:string ->
  ?suggestion:string ->
  ?replacement:string ->
  ?expected_source:string ->
  unit ->
  t

val compare : t -> t -> int
(** A total order. Identical input must produce byte-identical output, so findings
    differing only in their suggested fix still need a stable order. *)
