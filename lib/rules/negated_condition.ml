open Lintocaml_engine
open Expr_view

let docs =
  {|`if not c then a else b` makes the reader negate the condition mentally and
then map the branches back across it. Swapping the branches removes both steps.

Only two-branch conditionals are reported. `if not c then a` without an else has
nothing to swap, and rewriting it would change which branch is the implicit
unit.|}

let check (expression : Expr_view.t) =
  match expression.desc with
  | If { cond; else_ = Some _; _ } when callee_is cond [ "Stdlib.not" ] ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"drop the `not` and exchange the two branches"
          "a negated condition with both branches present reads more directly inverted";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "negated-condition";
    title = "Negated condition with both branches";
    category = Rule.Idiom;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
