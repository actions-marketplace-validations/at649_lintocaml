open Lintocaml_engine
open Expr_view

let docs =
  {|OCaml's structural comparison operators raise `Invalid_argument` when they
reach a functional value. A function is a valid operand at compile time, so the
failure otherwise appears only when this expression runs.

Functions have no general structural equality or ordering. Compare stable data
that identifies the intended behaviour, or redesign the surrounding type so it
does not require polymorphic comparison.|}

let comparison_operators =
  [
    "Stdlib.=";
    "Stdlib.<>";
    "Stdlib.<";
    "Stdlib.>";
    "Stdlib.<=";
    "Stdlib.>=";
    "Stdlib.compare";
  ]

let operand_class = function
  | first :: second :: _ -> if first.ty = Unknown_class then second.ty else first.ty
  | first :: _ -> first.ty
  | [] -> Unknown_class

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args }
    when path_is operator comparison_operators && operand_class args = Functional ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"do not use polymorphic comparison on functions"
          "polymorphic comparison raises Invalid_argument on functional values";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "poly-compare-on-function";
    title = "Polymorphic comparison on a function";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
