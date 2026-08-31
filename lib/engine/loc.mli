type t = {
  file : string;
  line : int;
  col : int;  (** 0-based, as the compiler reports it. *)
  end_line : int;
  end_col : int;
}

val compare : t -> t -> int

val one_based_column : int -> int
(** Converts a compiler byte offset to a display column without overflowing on a malformed
    location. *)

val pp : Format.formatter -> t -> unit
