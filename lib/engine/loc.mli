type t = {
  file : string;
  line : int;
  col : int;  (** 0-based, as the compiler reports it. *)
  end_line : int;
  end_col : int;
}

val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
