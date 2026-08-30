open Lintocaml_engine
open Expr_view

let docs =
  {|These functions raise on inputs the type system considers perfectly valid:
`List.hd []` raises `Failure`, `Option.get None` raises `Invalid_argument`,
`Hashtbl.find` raises `Not_found`. The failure surfaces at runtime, often in
production, and the exception carries no context about which call site failed.

Each has a total counterpart that returns an option and forces the empty case to
be handled where it occurs. The option-returning version is not more verbose once
you count the exception handler you would otherwise need.

`Array.get` and `String.get` are deliberately absent: `a.(i)` and `s.[i]`
desugar to them, so including them would report every indexing expression in the
codebase.|}

let partials =
  [
    ("Stdlib.List.hd", "List.nth_opt l 0, or match on the list");
    ("Stdlib.List.tl", "match on the list");
    ("Stdlib.List.nth", "List.nth_opt");
    ("Stdlib.List.find", "List.find_opt");
    ("Stdlib.List.assoc", "List.assoc_opt");
    ("Stdlib.Option.get", "Option.value ~default, or match on the option");
    ("Stdlib.Hashtbl.find", "Hashtbl.find_opt");
  ]

let rec list_literal_length (e : Expr_view.t) =
  match e.desc with
  | Construct { name = "[]"; _ } -> Some 0
  | Construct { name = "::"; args = [ _; tail ] } ->
      Option.map (( + ) 1) (list_literal_length tail)
  | _ -> None

let obviously_safe path args =
  match (strip_stdlib path, args) with
  | ("List.hd" | "List.tl"), { desc = Construct { name = "::"; _ }; _ } :: _ -> true
  | "Option.get", { desc = Construct { name = "Some"; _ }; _ } :: _ -> true
  | "List.nth", list :: { desc = Const (Int index); _ } :: _ -> (
      match list_literal_length list with
      | Some length -> index >= 0 && index < length
      | None -> false)
  | _ -> false

(* A negative literal index is owned by the more specific
   [negative-list-index] rule, which reports it as an error with a dedicated
   rationale. Firing here too would double-report the same defect. *)
let negative_literal_index path args =
  match (strip_stdlib path, args) with
  | "List.nth", _ :: { desc = Const (Int index); _ } :: _ -> index < 0
  | _ -> false

(* [List.hd] applied to a traversal belongs to whichever rule can explain the
   traversal: [head-of-traversal] for a sort or reverse, [head-of-filter] for a
   filter. Each says what the wasted work is, which the generic "List.hd is
   partial" message does not. *)
let head_of_traversal path args =
  match (strip_stdlib path, args) with
  | "List.hd", { desc = Apply { callee = Some inner; _ }; _ } :: _ -> (
      match strip_stdlib inner with
      | "List.sort" | "List.stable_sort" | "List.fast_sort" | "List.rev" | "List.filter"
      | "List.find_all" ->
          true
      | _ -> false)
  | _ -> false

let check (e : Expr_view.t) =
  match e.desc with
  | Apply { callee = Some path; args } -> (
      let name = strip_stdlib path in
      match List.find_opt (fun (p, _) -> String.equal (strip_stdlib p) name) partials with
      | Some (_, suggestion)
        when (not (obviously_safe path args))
             && (not (negative_literal_index path args))
             && not (head_of_traversal path args) ->
          [
            Rule.finding ~loc:e.loc ~suggestion
              (Fmt.str "`%s` raises on inputs the type system allows" name);
          ]
      | Some _ | None -> [])
  | _ -> []

let rule : Rule.t =
  {
    id = "partial-function";
    title = "Partial stdlib function with a total alternative";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Warning;
    docs;
    check;
  }
