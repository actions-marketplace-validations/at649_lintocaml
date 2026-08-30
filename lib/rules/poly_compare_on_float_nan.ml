open Lintocaml_engine
open Expr_view

let docs =
  {|IEEE comparisons against NaN never establish equality or ordering:
value = nan, value < nan, and their reversed forms are always false, while
value <> nan is always true. Such a comparison therefore cannot test whether
a value is NaN.

Use Float.is_nan for that test. Ordinary comparisons between float values are
not reported because IEEE behavior is often exactly what numerical code wants.|}

let operators =
  [ "Stdlib.="; "Stdlib.<>"; "Stdlib.<"; "Stdlib.>"; "Stdlib.<="; "Stdlib.>=" ]

let is_nan_constant expression =
  List.exists
    (fun path -> ident_is path expression)
    [ "Stdlib.nan"; "Stdlib.Float.nan"; "Float.nan" ]

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some operator; args }
    when path_is operator operators && List.exists is_nan_constant args ->
      let suggestion =
        if path_is operator [ "Stdlib.="; "Stdlib.<>" ] then
          "use Float.is_nan and negate it explicitly when needed"
        else "use Float.is_nan before applying an ordering predicate"
      in
      [
        Rule.finding ~loc:e.loc ~suggestion
          "a direct comparison with NaN has a constant result";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "poly-compare-on-float-nan";
    title = "Direct comparison with NaN";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
