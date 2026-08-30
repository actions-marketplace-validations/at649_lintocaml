open Lintml_engine
open Expr_view

let docs =
  {|`Obj.magic` tells the compiler to stop checking. Every guarantee OCaml makes
about the program is void at that point: a wrong cast does not raise, it
corrupts memory, and the crash surfaces somewhere unrelated with a backtrace
that points at innocent code.

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
             "`%s` bypasses the type system; a wrong cast corrupts memory rather than \
              raising"
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
