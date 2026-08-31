open Lintocaml_engine
open Expr_view

let docs =
  {|`compare` is specified to return a negative integer, zero, or a positive
integer. It is not specified to return exactly -1 or 1.

Testing the result against a specific non-zero value relies on an implementation
detail. Even if `compare a b = 1` happens to work for a particular comparison
function today, only the sign is part of its contract.

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
    "Stdlib.Bytes.compare";
    "Stdlib.Digest.compare";
    "Stdlib.Int32.compare";
    "Stdlib.Int64.compare";
    "Stdlib.Nativeint.compare";
    "Stdlib.Uchar.compare";
    "Stdlib.List.compare";
    "Stdlib.Option.compare";
    "Stdlib.Result.compare";
    "Stdlib.Either.compare";
    "Stdlib.Array.compare";
    "Stdlib.Iarray.compare";
    "Stdlib.Seq.compare";
  ]

let is_comparison expression = callee_is expression comparators

(* Zero is the one exact result the specification guarantees. Any other
   literal confuses a sign contract with a particular implementation. *)
let is_nonzero_int expression =
  match expression.desc with Const (Int value) -> value <> 0 | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.="; "Stdlib.<>" ]
         && ((is_comparison left && is_nonzero_int right)
            || (is_nonzero_int left && is_comparison right)) ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"test the sign instead, for example `compare a b > 0`"
          "compare guarantees a sign, not a particular non-zero result";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "compare-result-equality";
    title = "Comparison result tested against an exact non-zero value";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
