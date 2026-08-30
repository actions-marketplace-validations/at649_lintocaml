(** A source file indexed by line, for turning {!Lintocaml_engine.Loc.t} into byte
    offsets. *)

type t = private { contents : string; line_offsets : int array }

val of_string : string -> t
val read : string -> (t, string) result

val range : t -> Lintocaml_engine.Loc.t -> (int * int) option
(** Byte offsets of a location, or [None] when it falls outside the file. That happens
    whenever the compiler artifact is newer than the source on disk, which is exactly when
    an edit must not be attempted. *)

val slice : t -> Lintocaml_engine.Loc.t -> string option
(** The source text a location covers. *)
