open Lintocaml_engine
open Expr_view

let docs =
  {|Standard collection constructors reject negative sizes with
`Invalid_argument`. When the size is a negative integer literal, the call is
guaranteed to fail before it can construct a value.

Pass a non-negative size. If the size comes from arithmetic, validate or clamp
it where that arithmetic is performed so the invariant is explicit.|}

(* Paired with the number of positional arguments the stdlib function takes.
   Labelled arguments never reach a rule, so a call carrying any is short by at
   least one here. Declining then is what keeps [Array.create ~len:n (-1)],
   where the negative literal is the fill value rather than the size, from
   being read as a negative size. *)
let single_size_functions =
  [
    ("Stdlib.Array.make", 2);
    ("Stdlib.Array.create", 2);
    ("Stdlib.Array.init", 2);
    ("Stdlib.List.init", 2);
    ("Stdlib.String.make", 2);
    ("Stdlib.Bytes.make", 2);
    ("Stdlib.Bytes.create", 1);
  ]

let matrix_functions =
  [
    ("Stdlib.Array.make_matrix", 3);
    ("Stdlib.Array.create_matrix", 3);
    ("Stdlib.Array.init_matrix", 3);
  ]

(* [arity] is the count the fully applied call has; anything else means either a
   partial application or a labelled argument we cannot see. *)
let takes callee arity table =
  List.exists
    (fun (name, expected) -> path_is callee [ name ] && Int.equal expected arity)
    table

let is_negative = function { desc = Const (Int value); _ } -> value < 0 | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = size :: _ as args }
    when takes callee (List.length args) single_size_functions && is_negative size ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"pass a non-negative collection size"
          "this collection constructor always raises on a negative size";
      ]
  | Apply { callee = Some callee; args = first :: second :: _ as args }
    when takes callee (List.length args) matrix_functions
         && (is_negative first || is_negative second) ->
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
