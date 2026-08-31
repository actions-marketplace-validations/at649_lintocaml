(** Machine-readable output. [artifacts_found] records discovered [.cmt] and [.cmti]
    files, including any that could not be read. The [schema_version] field is a
    compatibility promise: any incompatible change to the shape increments it. *)

val render : report_suppressed:bool -> Analyse.outcome -> string
