open Lintocaml_engine
open Expr_view

let docs =
  {|Strings are immutable, so each concatenation copies the complete accumulator.
Appending or prepending to a growing string on every fold iteration therefore
copies earlier content repeatedly and makes total work quadratic in the output
size.

Use a `Buffer`, add each fragment once, and call `Buffer.contents` after the
fold. The rule reports inline `List.fold_left` and `List.fold_right` callbacks
whose accumulator is visibly used by `(^)` or `String.concat`.|}

let accumulator = function
  | ( callee,
      { desc = Function { params = accumulator :: _; simple_params = true; body; _ }; _ }
    )
    when path_is callee [ "Stdlib.List.fold_left" ] ->
      Some (accumulator, body)
  | ( callee,
      {
        desc = Function { params = _ :: accumulator :: _; simple_params = true; body; _ };
        _;
      } )
    when path_is callee [ "Stdlib.List.fold_right" ] ->
      Some (accumulator, body)
  | _ -> None

let concatenates accumulator expression =
  match expression.desc with
  | Apply { callee = Some callee; args } when path_is callee [ "Stdlib.^" ] ->
      List.exists (ident_is accumulator) args
  | Apply { callee = Some callee; args = [ _separator; fragments ] }
    when path_is callee [ "Stdlib.String.concat" ] ->
      exists (ident_is accumulator) fragments
  | _ -> false

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some callee; args = callback :: _ } -> (
      match accumulator (callee, callback) with
      | Some (name, body) when exists (concatenates name) body ->
          [
            Rule.finding ~loc:e.loc
              ~suggestion:"accumulate fragments in a Buffer and call Buffer.contents once"
              "string concatenation with a growing fold accumulator is quadratic";
          ]
      | _ -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "quadratic-concat";
    title = "String concatenation in a fold";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
