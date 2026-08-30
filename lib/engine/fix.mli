(** Constructing replacements for rules that can offer a mechanical fix. *)

val source : Expr_view.t -> Rule.replacement
(** Substitutes an expression's own source text, parenthesised unless the expression is
    syntactically atomic.

    Prefer this over building {!Lintocaml_engine.Rule.Source} directly: omitting
    parentheses around a compound expression produces a fix that changes precedence, which
    is worse than offering no fix at all. *)

val atomic : Expr_view.t -> bool
(** Whether an expression can be spliced without parentheses. Exposed for testing. *)
