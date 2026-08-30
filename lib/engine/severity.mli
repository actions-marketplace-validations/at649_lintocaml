type t = Error | Warning | Hint

val to_string : t -> string
val of_string : string -> t option

val rank : t -> int
(** Higher is louder. Exposed so diagnostics can order by severity. *)

val at_least : threshold:t -> t -> bool
(** [at_least ~threshold s] is whether [s] is as loud as [threshold], which is what
    [--fail-on] tests. *)
