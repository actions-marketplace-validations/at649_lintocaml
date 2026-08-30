(** Human-readable output: one finding per block, with a source excerpt. *)

val colour : bool ref
(** Set to [false] by [--no-color]. Checked when rendering rather than at startup so tests
    can toggle it. *)

val pp : report_suppressed:bool -> Format.formatter -> Analyse.outcome -> unit
