open Lintocaml_engine
open Expr_view

let docs =
  {|`compare` is specified to return a negative integer, zero, or a positive
integer. It is not specified to return exactly -1 or 1.

Testing the result against a specific non-zero value therefore silently fails
for most inputs. `compare a b = 1` is false whenever the difference is not
exactly one.

Test the sign instead: `compare a b > 0`, or `= 0` for equality, which is the
one exact value the specification does guarantee.|}

let comparators =
  [
    "Stdlib.compare";
    "Stdlib.String.compare";
    "Stdlib.Int.compare";
    "Stdlib.Float.compare";
    "Stdlib.Bool.compare";
    "Stdlib.Char.compare";
    "Stdlib.List.compare";
    "Stdlib.Option.compare";
  ]

let is_comparison expression = callee_is expression comparators

(* Only -1 and 1 are wrong. Comparing against 0 is exactly the guarantee the
   specification makes, so it must not be reported. *)
let is_signed_unit expression =
  match expression.desc with Const (Int (1 | -1)) -> true | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.="; "Stdlib.<>" ]
         && ((is_comparison left && is_signed_unit right)
            || (is_signed_unit left && is_comparison right)) ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"test the sign instead, for example `compare a b > 0`"
          "compare returns any integer of the correct sign, not -1 or 1";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "compare-result-equality";
    title = "Comparison result tested against -1 or 1";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
