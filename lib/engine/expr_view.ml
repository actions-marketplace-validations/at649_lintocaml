(* A deliberately small, version-stable view of the typed AST.

   Rules match on this type, never on Typedtree. When a compiler release moves
   something, only Tast_iface changes; this file and every rule stay put. *)

type constant =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | String of string
  | Float of string
  | Char of char

(* [Unknown_class] covers type variables and anything not classified here.
   Rules must read it as "do not report". *)
type type_class =
  | Immediate (* int, bool, char, unit *)
  | Boxed (* string, list, tuple, ... *)
  | Functional (* structural equality on these raises at runtime *)
  | Abstract
  | Unknown_class

type suppression = { rule_id : string; reason : string option }

type t = {
  loc : Loc.t;
  ty : type_class;
  type_name : string option;
  desc : desc;
  allowed : suppression list;
}

and desc =
  | Ident of string (* resolved path, e.g. "Stdlib.List.length" *)
  | Const of constant
  | Construct of { name : string; args : t list } (* true, false, Some x, [] *)
  | Apply of { callee : string option; args : t list }
  | If of { cond : t; then_ : t; else_ : t option }
  | Try of { body : t; handlers : handler list }
  | Match of { scrutinee : t; arms : t list; empty_arm : bool }
    (* [empty_arm] is true when some arm pattern matches the empty list, so a
         partial-list call on the scrutinee inside a later arm is guarded. *)
  | Bool_match of { scrutinee : t; when_true : t; when_false : t }
  | Option_match of { scrutinee : t; binding : string; when_some : t; when_none : t }
  | Sequence of t * t
  | Let of { bindings : t list; body : t }
  | Function of {
      params : string list;
      recursive : bool;
          (* True when this function is the right-hand side of a [let rec].
             Eta-reducing such a binding yields [let rec f = g], which the
             compiler rejects: [let rec] requires a syntactic function. *)
      simple_params : bool;
          (* False when any parameter was labelled, optional, or a destructuring
             pattern. Such parameters are absent from [params], so a rule that
             compares parameters against arguments must decline rather than
             match on the remainder. *)
      body : t;
    }
  | Discard of t
  | Attribute of { name : string; payload : string option }
  | Other

and handler = {
  catches_all : bool;
  guarded : bool;
  reraises : bool;
  protects_fatal : bool;
  h_loc : Loc.t;
}

(* [List.length] surfaces as "Stdlib.List.length" or "List.length" depending on
   how it was brought into scope, so path comparison has to ignore the prefix. *)
let stdlib_prefix = "Stdlib."

(* Alphabetic operators are escaped in resolved paths: [mod] arrives as
   "Stdlib.\\#mod", not "Stdlib.mod". Rules name operators the way they are
   written in source, so the escape is skipped here rather than in every
   candidate list. Also affects land, lor, lxor, lsl, lsr and asr. *)
let operator_escape = "\\#"

let body_offset path =
  let offset =
    if String.starts_with ~prefix:stdlib_prefix path then String.length stdlib_prefix
    else 0
  in
  let escape = String.length operator_escape in
  if
    String.length path - offset >= escape
    && String.equal (String.sub path offset escape) operator_escape
  then offset + escape
  else offset

let strip_stdlib path =
  match body_offset path with
  | 0 -> path
  | offset -> String.sub path offset (String.length path - offset)

(* Every rule calls this for every candidate on every node, so it compares in
   place rather than allocating a stripped copy of each side. *)
let path_equal a b =
  let ia = body_offset a and ib = body_offset b in
  let length = String.length a - ia in
  length = String.length b - ib
  &&
  let rec same i = i = length || (a.[ia + i] = b.[ib + i] && same (i + 1)) in
  same 0

let path_is path candidates = List.exists (path_equal path) candidates

let callee_is expr candidates =
  match expr.desc with Apply { callee = Some c; _ } -> path_is c candidates | _ -> false

let is_int_const n expr = match expr.desc with Const (Int m) -> m = n | _ -> false

let is_construct name expr =
  match expr.desc with Construct c -> String.equal c.name name | _ -> false

let ident_is name expr =
  match expr.desc with Ident candidate -> String.equal candidate name | _ -> false

let type_name_ends_with suffix expr =
  match expr.type_name with
  | None -> false
  | Some name -> String.equal name suffix || String.ends_with ~suffix:("." ^ suffix) name

let type_name_is candidates expr =
  match expr.type_name with Some name -> path_is name candidates | None -> false

let suppression_for rule_id expr =
  List.find_opt (fun suppression -> String.equal suppression.rule_id rule_id) expr.allowed

let children expr =
  match expr.desc with
  | Construct { args; _ } | Apply { args; _ } -> args
  | If { cond; then_; else_ } -> cond :: then_ :: Option.to_list else_
  | Try { body; _ } -> [ body ]
  | Match { scrutinee; arms; _ } -> scrutinee :: arms
  | Bool_match { scrutinee; when_true; when_false } ->
      [ scrutinee; when_true; when_false ]
  | Option_match { scrutinee; when_some; when_none; _ } ->
      [ scrutinee; when_some; when_none ]
  | Sequence (left, right) -> [ left; right ]
  | Let { bindings; body } -> body :: bindings
  | Function { body; _ } | Discard body -> [ body ]
  | Ident _ | Const _ | Attribute _ | Other -> []

let rec exists predicate expr =
  predicate expr || List.exists (exists predicate) (children expr)
