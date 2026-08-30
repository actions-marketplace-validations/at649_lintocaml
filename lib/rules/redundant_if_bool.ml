open Lintml_engine
open Expr_view

let docs =
  {|`if c then true else false` is `c`. The branches add no information and make
the reader check that they are not inverted.

The same applies to `if c then false else true`, which is `not c`.

This is an idiom rule, not a bug: the original compiles to the same thing. It
ships in the `idiomatic` profile rather than the default one.|}

let check (e : Expr_view.t) =
  match e.desc with
  | If { cond = _; then_; else_ = Some else_ } ->
      if is_construct "true" then_ && is_construct "false" else_ then
        [
          Rule.finding ~loc:e.loc ~suggestion:"replace with the condition itself"
            "`if c then true else false` is just `c`";
        ]
      else if is_construct "false" then_ && is_construct "true" else_ then
        [
          Rule.finding ~loc:e.loc ~suggestion:"replace with `not c`"
            "`if c then false else true` is just `not c`";
        ]
      else []
  | _ -> []

let rule : Rule.t =
  {
    id = "redundant-if-bool";
    title = "Redundant boolean if-expression";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
