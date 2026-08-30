open Lintocaml_engine
open Expr_view

let docs =
  {|`List.length l = 0` walks the entire list to answer a question that does not
need the length at all. On a long list this turns an O(1) emptiness test into an
O(n) traversal; on a very large one it is a hang rather than a slowdown.

Compare against the empty list instead: `l = []`, `l <> []`, or match on the
list directly.

Only `List.length` is reported. `String.length` and `Array.length` are O(1), so
comparing them to zero costs nothing.|}

let length_fns = [ "Stdlib.List.length" ]

(* Traversals that a more specific rule already reports. Without this,
   [List.length (List.filter p l) = 0] draws a finding from this rule and
   another from `exists-via-filter`, and the reader has to work out that both
   describe the same expression. The more specific advice wins. *)
let covered_elsewhere =
  [
    "Stdlib.List.filter";
    "Stdlib.List.find_all";
    "Stdlib.List.map";
    "Stdlib.List.mapi";
    "Stdlib.List.rev";
  ]

let is_length e =
  callee_is e length_fns
  &&
  match e.desc with
  | Apply { args = [ inner ]; _ } -> not (callee_is inner covered_elsewhere)
  | _ -> true

(* The suggestion depends on the operator and on which side the length sits:
   [length l > 0] means non-empty, but [0 > length l] is never true. Emitting
   `l = []` for all of them would be confidently wrong advice. *)
let replacement op ~length_on_left =
  match (strip_stdlib op, length_on_left) with
  | "=", _ -> Some "l = []"
  | "<>", _ -> Some "l <> []"
  | ">", true -> Some "l <> []"
  | "<", false -> Some "l <> []"
  | _ -> None

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some op; args = [ a; b ] } -> (
      let sides =
        if is_length a && is_int_const 0 b then Some true
        else if is_length b && is_int_const 0 a then Some false
        else None
      in
      match sides with
      | None -> []
      | Some length_on_left -> (
          match replacement op ~length_on_left with
          | None -> []
          | Some suggestion ->
              [
                Rule.finding ~loc:e.loc
                  ~suggestion:(Fmt.str "compare against the empty list: `%s`" suggestion)
                  "List.length is O(n); comparing it to 0 walks the whole list to answer \
                   an O(1) question";
              ]))
  | _ -> []

let rule : Rule.t =
  {
    id = "length-compare-zero";
    title = "List.length compared against zero";
    category = Rule.Performance;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
