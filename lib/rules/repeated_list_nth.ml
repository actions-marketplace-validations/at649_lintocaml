open Lintml_engine
open Expr_view

let docs =
  {|`List.nth list index` walks from the head for every call. Calling it inside
an iteration over the same list can traverse the same prefixes repeatedly and
turn a linear pass into quadratic work.

Carry the needed value in the iterator callback, use `List.mapi` when an index
is required, or choose an array for genuinely random access. The rule reports
only inline standard-library iterators where the iterated list and `List.nth`
target are the same resolved identifier. A fixed literal index is excluded
because bounded lookup on every iteration is still linear overall.|}

let loop_parts callee args =
  if
    path_is callee
      [
        "Stdlib.List.iter";
        "Stdlib.List.iteri";
        "Stdlib.List.map";
        "Stdlib.List.mapi";
        "Stdlib.List.filter";
        "Stdlib.List.filter_map";
      ]
  then
    match args with
    | callback :: collection :: _ -> Some (callback, collection)
    | _ -> None
  else if path_is callee [ "Stdlib.List.fold_left" ] then
    match args with
    | callback :: _initial :: collection :: _ -> Some (callback, collection)
    | _ -> None
  else if path_is callee [ "Stdlib.List.fold_right" ] then
    match args with
    | callback :: collection :: _initial :: _ -> Some (callback, collection)
    | _ -> None
  else None

let same_ident left right =
  match (left.desc, right.desc) with
  | Ident left, Ident right -> String.equal left right
  | _ -> false

let nth_of collection expression =
  match expression.desc with
  | Apply { callee = Some callee; args = candidate :: index :: _ } -> (
      path_is callee [ "Stdlib.List.nth" ]
      && same_ident collection candidate
      && match index.desc with Const (Int _) -> false | _ -> true)
  | _ -> false

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some callee; args } -> (
      match loop_parts callee args with
      | Some ({ desc = Function { body; _ }; _ }, collection)
        when exists (nth_of collection) body ->
          [
            Rule.finding ~loc:e.loc
              ~suggestion:
                "use the callback value, List.mapi, or an array for random access"
              "List.nth is repeatedly called while iterating over the same list";
          ]
      | _ -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "repeated-list-nth";
    title = "List.nth inside an iteration over the same list";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
