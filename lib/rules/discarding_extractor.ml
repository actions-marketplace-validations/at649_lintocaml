open Lintocaml_engine
open Expr_view

let docs =
  {|`Result.get_ok` and `Result.get_error` raise when the value is not the
constructor they expect, and the exception says nothing about which call site
failed.

`Result.get_ok (Error e)` raises `Invalid_argument` and discards `e` entirely,
so the one piece of information that would explain the failure is thrown away at
the moment it is needed.

Match on the result, or use `Result.value ~default`.

`Option.get` has the same shape and is reported by `partial-function`, so it is
not listed here: one expression should draw one finding.|}

let extractors = [ "Stdlib.Result.get_ok"; "Stdlib.Result.get_error" ]

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some name; _ } when path_is name extractors ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"match on the value, or use the ~default variant"
          (Fmt.str "`%s` raises and discards the value that would explain the failure"
             (strip_stdlib name));
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "discarding-extractor";
    title = "Extractor that raises and drops the error";
    category = Rule.Correctness;
    profile = Rule.Idiomatic;
    default_severity = Severity.Hint;
    docs;
    check;
  }
