open Lintocaml_engine
open Expr_view

let docs =
  {|A floating warning attribute with -a or -A disables every compiler warning
for the rest of its scope. That hides exhaustiveness, unused-value,
fragile-pattern, and other diagnostics that often identify real defects before
they reach this linter.

Disable a specific warning number only where it is understood, and leave a
local explanation. Generated code can suppress this rule explicitly.|}

let disables_all value =
  let value = String.trim value in
  String.equal value "-a" || String.equal value "-A"

let check (expression : Expr_view.t) =
  match expression.desc with
  | Attribute { name; payload = Some value }
    when (String.equal name "warning" || String.equal name "ocaml.warning")
         && disables_all value ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:
            "remove this attribute or disable only the specific warning that is \
             understood"
          "this attribute disables all compiler warnings";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "disabled-all-warnings";
    title = "All compiler warnings disabled";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
