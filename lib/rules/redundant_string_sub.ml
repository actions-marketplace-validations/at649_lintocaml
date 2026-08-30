open Lintml_engine
open Expr_view

let docs =
  {|`String.sub s 0 (String.length s)` allocates a copy of the whole string and
returns it. OCaml strings are immutable, so the copy has the same contents as
the original and normally serves no purpose. Code that deliberately observes
allocation identity with `==` should suppress this rule.

This normally survives a refactor that removed the real offset and length.|}

let is_length_of subject expression =
  match expression.desc with
  | Apply { callee = Some callee; args = [ argument ] }
    when path_is callee [ "Stdlib.String.length" ] -> (
      match (subject.desc, argument.desc) with
      | Ident left, Ident right -> String.equal left right
      | _ -> false)
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ subject; start; length ] }
    when path_is callee [ "Stdlib.String.sub" ]
         && is_int_const 0 start
         && is_length_of subject length ->
      [
        Rule.finding ~loc:expression.loc ~suggestion:"use the string itself"
          "this allocates a whole-string copy with identical contents";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "redundant-string-sub";
    title = "Whole-string String.sub";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
