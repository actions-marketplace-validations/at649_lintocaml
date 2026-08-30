(** Machine-readable output. The [schema_version] field is a compatibility promise: any
    incompatible change to the shape increments it. *)

val render : report_suppressed:bool -> Analyse.outcome -> string
