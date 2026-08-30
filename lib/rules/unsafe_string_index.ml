open Lintml_engine
open Expr_view

let docs =
  {|Valid string indices stop at `String.length s - 1`. Indexing `s` at
`String.length s`, or at that length plus a non-negative offset, is therefore
always out of bounds and raises `Invalid_argument`.

This rule deliberately reports only expressions it can prove unsafe. It does
not guess about arbitrary index arithmetic or attempt to reconstruct guards
from surrounding control flow.|}

let same_ident left right =
  match (left.desc, right.desc) with
  | Ident left, Ident right -> String.equal left right
  | _ -> false

let is_length_of string expression =
  match expression.desc with
  | Apply { callee = Some callee; args = [ candidate ] } ->
      path_is callee [ "Stdlib.String.length" ] && same_ident string candidate
  | _ -> false

let is_nonnegative = function { desc = Const (Int value); _ } -> value >= 0 | _ -> false

let is_past_end string index =
  is_length_of string index
  ||
  match index.desc with
  | Apply { callee = Some operator; args = [ left; right ] }
    when path_is operator [ "Stdlib.+" ] ->
      (is_length_of string left && is_nonnegative right)
      || (is_nonnegative left && is_length_of string right)
  | _ -> false

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some callee; args = [ string; index ] }
    when path_is callee [ "Stdlib.String.get" ] && is_past_end string index ->
      [
        Rule.finding ~loc:e.loc ~suggestion:"use an index smaller than String.length s"
          "this string index is always outside the valid range";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "unsafe-string-index";
    title = "Provably out-of-bounds string index";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
