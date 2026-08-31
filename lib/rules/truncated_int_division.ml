open Lintocaml_engine
open Expr_view

let docs =
  {|`float_of_int (a / b)` performs the division on integers first, truncating
toward zero, and only then converts. `float_of_int (7 / 2)` is `3.`, not `3.5`.

The rounding is silent and easy to miss when test inputs divide evenly. In some
programs the truncation is intentional, so this check is advice rather than
proof of a defect.

Convert first and divide in floating point: `float_of_int a /. float_of_int b`.|}

let converters = [ "Stdlib.float_of_int"; "Stdlib.Float.of_int" ]
let integer_division = [ "Stdlib./"; "Stdlib.Int.div" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ argument ] }
    when path_is callee converters && callee_is argument integer_division ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"convert each operand first: float_of_int a /. float_of_int b"
          "integer division truncates before the conversion, discarding the remainder";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "truncated-int-division";
    title = "Integer division converted to float";
    category = Rule.Correctness;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
