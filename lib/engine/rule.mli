(** The rule-authoring API.

    A rule is a pure function from one expression to the findings at that expression. The
    engine walks the tree, applies configuration, and attaches the rule id and severity,
    so a rule needs to know none of those. *)

type category = Correctness | Performance | Idiom

val category_to_string : category -> string

type profile =
  | Default  (** Bugs and near-certain mistakes. Runs unless disabled. *)
  | Idiomatic  (** Taste. Opt-in. *)
  | Pedantic  (** Opinionated enough to need asking for by name. *)

type replacement =
  | Source of Loc.t  (** Replace with the source text at this location. *)
  | Parenthesized_source of Loc.t
      (** As [Source], wrapped in parentheses because the expression is not syntactically
          atomic at the replacement site. *)
  | Text of string  (** Replace with literal text. *)

type finding = {
  f_loc : Loc.t;
  f_message : string;
  f_suggestion : string option;
  f_fix : replacement option;
}

val finding : ?suggestion:string -> ?fix:replacement -> loc:Loc.t -> string -> finding

type t = {
  id : string;  (** Stable across releases; it appears in configs and SARIF. *)
  title : string;
  category : category;
  profile : profile;
  default_severity : Severity.t;
  docs : string;
      (** Printed by [lintml explain]. A rule that cannot justify itself in three
          sentences is an opinion, not a lint. *)
  check : Expr_view.t -> finding list;  (** Must be pure and independent of visit order. *)
}
