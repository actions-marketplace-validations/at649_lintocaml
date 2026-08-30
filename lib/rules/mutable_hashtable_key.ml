open Lintml_engine
open Expr_view

let docs =
  {|A hash table records a key's hash when the key is inserted. If an array,
byte sequence, or reference is later mutated, its hash and equality behavior
can change while it remains in the old bucket, making the entry impossible to
find reliably.

Use an immutable key or take an immutable snapshot before insertion. This rule
checks insertion and replacement, where the table invariant is established,
and does not complain merely because mutable values exist elsewhere.|}

let is_known_mutable_key expression =
  List.exists
    (fun type_name -> type_name_ends_with type_name expression)
    [ "array"; "bytes"; "ref" ]

let mutating_operations = [ "Stdlib.Hashtbl.add"; "Stdlib.Hashtbl.replace" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = _table :: key :: _ }
    when path_is callee mutating_operations && is_known_mutable_key key ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"use an immutable key or insert an immutable snapshot"
          "mutating this hashtable key after insertion can make the entry unreachable";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "mutable-hashtable-key";
    title = "Mutable value used as a hashtable key";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
