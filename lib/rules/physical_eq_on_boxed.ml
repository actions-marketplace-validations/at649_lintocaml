open Lintml_engine
open Expr_view

let docs =
  {|`==` is physical equality: it asks whether two values occupy the same memory,
not whether they are equal. For boxed values - strings, lists, tuples, arrays -
this is almost never what the author meant, and it is a bug that hides well:
`"ab" == "ab"` may be true or false depending on whether the compiler shared the
literals.

Use `=` for structural equality. Reserve `==` for deliberate identity checks
where you genuinely mean "the same allocation", and say so in a comment.

This rule fires only when the operand type is known to be boxed. Type variables
are left alone, and so are functions: `=` on a function raises
`Invalid_argument`, so `==` is the only comparison available and suggesting `=`
would be actively wrong.|}

let phys_ops = [ "Stdlib.=="; "Stdlib.!=" ]

(* [==] has type ['a -> 'a -> bool], so both operands share a type; consult
   whichever side the type checker resolved more precisely. *)
let operand_class = function
  | a :: b :: _ -> if a.ty = Unknown_class then b.ty else a.ty
  | [ a ] -> a.ty
  | [] -> Unknown_class

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some op; args } when path_is op phys_ops ->
      if operand_class args = Boxed then
        let structural = if path_is op [ "Stdlib.==" ] then "=" else "<>" in
        [
          Rule.finding ~loc:e.loc
            ~suggestion:(Fmt.str "use `%s` for structural equality" structural)
            (Fmt.str
               "physical equality `%s` on a boxed value compares allocations, not \
                contents"
               (strip_stdlib op));
        ]
      else []
  | _ -> []

let rule : Rule.t =
  {
    id = "physical-eq-on-boxed";
    title = "Physical equality used on a boxed value";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
