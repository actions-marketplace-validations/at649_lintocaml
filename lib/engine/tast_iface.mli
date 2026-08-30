(** The only module permitted to depend on [compiler-libs].

    Everything else in lintml sees {!Expr_view.t} and nothing more. This signature is what
    makes that a fact rather than a convention: [Typedtree], [Types], [Path] and
    [Cmt_format] do not appear in it, so no rule can reach them even by accident.

    When a compiler release changes [compiler-libs], the compatibility work stays in this
    module. *)

type annotation
(** A typed implementation or interface loaded from a compiler artifact. *)

type load_error =
  | Unsupported_compiler of string
      (** The artifact was written by a different compiler version. *)
  | Invalid_cmt of string  (** The artifact could not be read at all. *)

val load_cmt :
  string -> ((annotation * string option * string option) option, load_error) result
(** [load_cmt path] reads a [.cmt] or [.cmti] file, returning its annotation, the source
    filename the compiler recorded, and the source digest.

    [Ok None] means the artifact was well-formed but carries no annotation to lint. Errors
    are returned rather than raised because an unreadable artifact must degrade to a
    warning, never take a build down. *)

val iter_views : annotation -> f:(Expr_view.t -> unit) -> unit
(** Applies [f] to every expression in the annotation, outermost first. *)
