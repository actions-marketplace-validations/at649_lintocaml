open Lintocaml_engine
open Expr_view

let docs =
  {|`Obj.magic` tells the compiler to stop checking the relationship between two
types. A wrong cast is not required to fail at the call site; it can violate
runtime representation assumptions and surface much later in unrelated code.

There are legitimate uses - GADT-free existentials, interfacing with untyped
runtime data - but they are rare and each one deserves a comment explaining why
the cast is sound. If this rule fires somewhere you cannot write that comment,
the cast is a bug waiting for a refactor.

`Obj.obj` is reported for the same reason. `Obj.repr` is not: converting a
value to `Obj.t` is safe by itself; the unchecked step is converting it back to
an arbitrary type.

Reported as a warning rather than an error, because a cast is not by itself a
bug the way a division by zero is. It is a place where the compiler stopped
being able to help, which a reviewer should know about and an author should be
able to justify. Suppress the ones you have justified.|}

let unsafe = [ "Stdlib.Obj.magic"; "Stdlib.Obj.obj" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Ident name when path_is name unsafe ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:
            "if the cast is genuinely sound, suppress this with a comment saying why"
          (Fmt.str
             "`%s` bypasses the type system and can violate runtime representation \
              assumptions"
             (strip_stdlib name));
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "unsafe-cast";
    title = "Type system bypassed with Obj";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
