open Lintml_engine
open Expr_view

let docs =
  {|Standard collection constructors reject negative sizes with
`Invalid_argument`. When the size is a negative integer literal, the call is
guaranteed to fail before it can construct a value.

Pass a non-negative size. If the size comes from arithmetic, validate or clamp
it where that arithmetic is performed so the invariant is explicit.|}

let single_size_functions =
  [
    "Stdlib.Array.make";
    "Stdlib.Array.create";
    "Stdlib.Array.init";
    "Stdlib.List.init";
    "Stdlib.String.make";
    "Stdlib.Bytes.make";
    "Stdlib.Bytes.create";
  ]

let matrix_functions =
  [ "Stdlib.Array.make_matrix"; "Stdlib.Array.create_matrix"; "Stdlib.Array.init_matrix" ]

let is_negative = function { desc = Const (Int value); _ } -> value < 0 | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = size :: _ }
    when path_is callee single_size_functions && is_negative size ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"pass a non-negative collection size"
          "this collection constructor always raises on a negative size";
      ]
  | Apply { callee = Some callee; args = first :: second :: _ }
    when path_is callee matrix_functions && (is_negative first || is_negative second) ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"pass non-negative matrix dimensions"
          "this matrix constructor always raises on a negative dimension";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "negative-size";
    title = "Negative collection size";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
