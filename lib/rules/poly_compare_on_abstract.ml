open Lintml_engine
open Expr_view

let docs =
  {|Polymorphic equality and comparison inspect OCaml's runtime representation.
For an abstract type, that representation is deliberately hidden and may change
without notice. Code outside the defining module should not make its behaviour
depend on representation details.

Use the equality, comparison, or hash function exposed by the type's module.
This is opt-in advice: some abstract types intentionally use structural
comparison, and lintml cannot infer that contract.|}

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

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some operator; args } when path_is operator comparison_operators -> (
      match operand_class args with
      | Abstract ->
          [
            Rule.finding ~loc:e.loc
              ~suggestion:
                "use the equality or comparison function exposed by this type's module"
              "polymorphic comparison bypasses the semantics of this abstract type";
          ]
      | Immediate | Boxed | Functional | Unknown_class -> [])
  | Apply { callee = Some callee; args = value :: _ }
    when path_is callee [ "Stdlib.Hashtbl.hash"; "Stdlib.Hashtbl.seeded_hash" ]
         && value.ty = Abstract ->
      [
        Rule.finding ~loc:e.loc
          ~suggestion:"use a hash function exposed by this type's module"
          "polymorphic hashing depends on the representation of this abstract type";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "poly-compare-on-abstract";
    title = "Polymorphic operations on an abstract representation";
    category = Rule.Correctness;
    profile = Rule.Pedantic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
