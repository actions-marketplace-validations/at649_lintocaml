open Lintocaml_engine
open Expr_view

let docs =
  {|Back-to-back list transformations allocate an intermediate list that is
immediately consumed. Fusing the operations removes that allocation and the
extra traversal.

`List.concat (List.map f values)` can use `List.concat_map`, and
`List.rev (List.map f values)` can use `List.rev_map`. The rule matches
resolved standard-library functions, so similarly named project functions are
not reported. Nested `List.map` calls are not reported because fusing them can
change the ordering of callback effects.|}

let nested_call name = function
  | { desc = Apply { callee = Some callee; args }; _ } when path_is callee [ name ] ->
      Some args
  | _ -> None

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some outer; args = [ inner ] }
    when path_is outer [ "Stdlib.List.concat"; "Stdlib.List.flatten" ] -> (
      match nested_call "Stdlib.List.map" inner with
      | Some (_ :: _ :: _) ->
          [
            Rule.finding ~loc:expression.loc
              ~suggestion:
                "replace List.concat (List.map f values) with List.concat_map f values"
              "List.concat after List.map allocates an avoidable intermediate list";
          ]
      | _ -> [])
  | Apply { callee = Some outer; args = [ inner ] }
    when path_is outer [ "Stdlib.List.rev" ] -> (
      match nested_call "Stdlib.List.map" inner with
      | Some (_ :: _ :: _) ->
          [
            Rule.finding ~loc:expression.loc
              ~suggestion:
                "replace List.rev (List.map f values) with List.rev_map f values"
              "List.rev after List.map performs two traversals and allocates an \
               intermediate list";
          ]
      | _ -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "list-fusion";
    title = "Adjacent list transformations";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
