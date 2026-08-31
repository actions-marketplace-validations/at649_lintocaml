(* Fixtures for lintocaml. Line numbers are asserted by test/run_fixtures.sh,
   so do not reflow this file without updating it. *)

(* ---- must be flagged ---- *)

let empty_check (l : int list) = List.length l = 0

let same_string (a : string) (b : string) = a == b

let risky l = List.hd l

let swallow () = try Some (List.hd []) with _ -> None

let silly c = if c then true else false

let nonempty (l : int list) = List.length l > 0

let nonempty' (l : int list) = 0 < List.length l

(* ---- must NOT be flagged ---- *)

let proper_empty (l : int list) = l = []

let int_identity (a : int) (b : int) = a == b

let fn_identity (f : int -> int) (g : int -> int) = f == g

let safe l = List.nth_opt l 0

let specific () = try Some (Hashtbl.hash 1) with Not_found -> None

let polymorphic_eq a b = a == b

let reraising () = try Some (List.length []) with e -> raise e

let indexing (a : int array) (s : string) i = (a.(i), s.[i])

let string_len (s : string) = String.length s = 0

let array_len (a : int array) = Array.length a = 0

let backwards (l : int list) = 0 > List.length l

let guarded () = try Some (List.length []) with e when e = Not_found -> None

let reraises_other () = try Some (List.length []) with _ -> raise Exit

let named_reraises_other () =
  try Some (List.length []) with _caught -> raise Exit

let reraises_after_logging () =
  try Some (List.length []) with caught ->
    ignore caught;
    raise caught

let safe_head = List.hd [ 1 ]
let safe_option = Option.get (Some 1)
let safe_nth = List.nth [ 1; 2 ] 1

type record = { value : int }
type payload = Payload of int | More of string
type enum = First | Second
type mixed = Empty | Full of int

let same_record (a : record) b = a == b
let same_payload (a : payload) b = a == b
let same_int64 (a : int64) b = a == b
let same_enum (a : enum) b = a == b
let same_mixed (a : mixed) b = a == b

let compare_functions (f : int -> int) g = f = g
let compare_floats (a : float) = a = Float.nan
let bounded_length (l : int list) = List.length l > 10
let invalid_index s = s.[String.length s]

let returns_result () = Error "no"
let drops_result () =
  let _ = returns_result () in
  ()

module Lwt = struct
  type 'a t = Done of 'a
  let return value = Done value
end

let drops_promise () = ignore (Lwt.return 1)

let compare_abstract (a : Format.formatter) b = a = b

let safe_float_compare (a : float) b = Float.equal a b
let safe_index s = if s = "" then None else Some s.[String.length s - 1]
let safe_result () = match returns_result () with Ok value -> value | Error _ -> 0

let quadratic_concat parts = List.fold_left (fun acc part -> acc ^ part) "" parts
let quadratic_append items = List.fold_left (fun acc item -> acc @ [ item ]) [] items
let repeated_nth items = List.mapi (fun index _item -> List.nth items index) items

let linear_concat parts = String.concat "" parts
let linear_list items = List.rev (List.fold_left (fun acc item -> item :: acc) [] items)
let nth_other items other = List.map (fun _item -> List.nth other 0) items
let allowed_identity (a : string) b = (a == b) [@lintocaml.allow ("physical-eq-on-boxed", "allocation identity is intended")]
let safe_float_literals = 1.0 = 2.0
let safe_ok_result () = ignore (Ok 1)

let reraises_in_both_branches condition =
  try raise Exit with caught ->
    if condition then raise caught else raise caught

let before_structure_allow (a : string) b = a == b

[@@@lintocaml.allow
  ("physical-eq-on-boxed", "generated module intentionally checks identity")]

let after_structure_allow (a : string) b = a == b

let fixed_nth items = List.map (fun _item -> List.nth items 0) items
let hash_abstract (formatter : Format.formatter) = Hashtbl.hash formatter
let hash_int value = Hashtbl.hash (value : int)

let literal_division_by_zero value = value / 0
let dynamic_division value divisor = value / divisor
let negative_array_size = Array.make (-1) 0
let dynamic_array_size size = Array.make size 0
let compare_boolean value = value = true
let use_boolean value = value
let negate_twice value = not (not value)
let negate_once value = not value
let concat_empty value = "" ^ value
let concat_prefix value = "prefix" ^ value
let nested_map f g values = List.map f (List.map g values)
let single_map f values = List.map f values
let mapped_concat f values = List.concat (List.map f values)
let direct_concat_map f values = List.concat_map f values
let mapped_reverse f values = List.rev (List.map f values)
let direct_reverse_map f values = List.rev_map f values
let reverse_twice values = List.rev (List.rev values)
let reverse_once values = List.rev values
let generic_failure () = failwith "missing value"
let specific_failure () = raise Not_found
let eta_target value = value + 1
let eta_wrapper value = eta_target value
let transformed_argument value = eta_target (value + 1)
let labelled_target ~value = value + 1
let labelled_wrapper value = labelled_target ~value
let boolean_match value = match value with true -> 1 | false -> 2
let option_match value = match value with Some _ -> 1 | None -> 2
let literal_remainder_by_zero value = value mod 0
let literal_int64_division_by_zero value = Int64.div value 0L
let negative_matrix_dimension = Array.make_matrix 2 (-1) 0
let concat_empty_right value = value ^ ""
let compare_boolean_not_false value = value <> false
let flattened_map f values = List.flatten (List.map f values)
[@@@warning "-a"]
let warning_suppression_scope value = value
[@@@warning "-32"]
let specific_warning_suppression value = value
let mutable_key_table : (int array, string) Hashtbl.t = Hashtbl.create 4
let insert_mutable_key key = Hashtbl.add mutable_key_table key "value"
let immutable_key_table : (int, string) Hashtbl.t = Hashtbl.create 4
let insert_immutable_key key = Hashtbl.add immutable_key_table key "value"
let keeps_promise () = Lwt.return 1
let meaningful_if condition = if condition then true else not condition
let option_target value = value + 1
let manual_option_map value =
  match value with Some item -> Some (option_target item) | None -> None
let nontrivial_option_match value =
  match value with Some item -> Some (option_target (item + 1)) | None -> None
let option_bind_target value = if value > 0 then Some value else None
let manual_option_bind value =
  match value with Some item -> option_bind_target item | None -> None
let constant_if value = if true then value else 0
let dynamic_if condition value = if condition then value else 0
let empty_list_append values = [] @ values
let nonempty_list_append values = [ 1 ] @ values
let left_nested_append first second third = (first @ second) @ third
let right_nested_append first second third = first @ (second @ third)
let invalid_literal_index = "abc".[3]
let valid_literal_index = "abc".[2]
let negative_substring_length value = String.sub value 0 (-1)
let dynamic_substring value offset length = String.sub value offset length
let ordinary_float_equality (left : float) right = left = right

(* compare-result-equality *)
let cmp_eq_one a b = compare a b = 1
let cmp_sign a b = compare a b > 0
let cmp_eq_zero a b = compare a b = 0

(* negative-list-index *)
let nth_negative l = List.nth l (-1)
let nth_ok l = List.nth l 2

(* length-of-mapped-list *)
let count_mapped l = List.length (List.map succ l)
let count_plain l = List.length l
let count_filtered l = List.length (List.filter (fun x -> x > 0) l)

(* head-of-traversal *)
let smallest l = List.hd (List.sort compare l)
let final l = List.hd (List.rev l)
let first l = List.hd l

(* negated-condition *)
let negated c a b = if not c then a else b
let negated_no_else c a = if not c then a
let plain c a b = if c then a else b

(* identity-map *)
let copy l = List.map (fun x -> x) l
let mapped l = List.map succ l

(* redundant-string-sub *)
let whole_copy s = String.sub s 0 (String.length s)
let real_slice s = String.sub s 1 (String.length s - 1)

(* exists-via-filter *)
let any_positive l = List.length (List.filter (fun x -> x > 0) l) > 0
let count_positive l = List.length (List.filter (fun x -> x > 0) l)
let uses_exists l = List.exists (fun x -> x > 0) l

(* hashtbl-mem-then-find *)
let table : (int, string) Hashtbl.t = Hashtbl.create 4
let double_lookup key =
  if Hashtbl.mem table key then Hashtbl.find table key else "absent"
let single_lookup key =
  match Hashtbl.find_opt table key with Some value -> value | None -> "absent"
let other_table : (int, string) Hashtbl.t = Hashtbl.create 4
let different_table key =
  if Hashtbl.mem table key then Hashtbl.find other_table key else "absent"

(* empty-filter-equality *)
let none_positive l = List.filter (fun x -> x > 0) l = []
let some_positive l = List.filter (fun x -> x > 0) l <> []
let kept_positive l = List.filter (fun x -> x > 0) l
let ordinary_compare l = List.filter (fun x -> x > 0) l = l

(* exists-equals-mem *)
let contains_target target l = List.exists (fun item -> item = target) l
let contains_transformed target l = List.exists (fun item -> item + 1 = target) l
let uses_mem target l = List.mem target l

(* string-length-compare-empty *)
let is_blank s = String.length s = 0
let is_blank_ne s = String.length s <> 0
let is_blank_lt s = String.length s < 1
let real_length_bound s = String.length s > 10
let direct_blank s = s = ""

(* partial-function: an empty-list arm guards List.hd in the other arm *)
let guarded_head (l : int list) =
  match l with [] -> 0 | _ -> List.hd l
let unguarded_head (l : int list) = List.hd l

(* physical-assoc-lookup *)
let assq_string (table : (string * int) list) key = List.assq key table
let assoc_string (table : (string * int) list) key = List.assoc key table
let assq_int (table : (int * int) list) key = List.assq key table

(* truncated-int-division *)
let ratio_wrong a b = float_of_int (a / b)
let ratio_right a b = float_of_int a /. float_of_int b

(* length-of-append *)
let total_wrong a b = List.length (a @ b)
let total_right a b = List.length a + List.length b

(* discarding-extractor *)
let unwrap (r : (int, string) result) = Result.get_ok r
let unwrap_safe (r : (int, string) result) = Result.value r ~default:0

(* unsafe-cast *)
let coerce (x : int) : string = Obj.magic x
let no_cast (x : int) = string_of_int x

(* head-of-filter *)
let first_match l = List.hd (List.filter (fun x -> x > 0) l)
let all_matches l = List.filter (fun x -> x > 0) l

(* partial-function: the exception is handled, so this is not a defect *)
let handled_find tbl key = try Hashtbl.find tbl key with Not_found -> 0
let handled_hd l = try List.hd l with Failure _ -> 0
let unhandled_find tbl key = Hashtbl.find tbl key
let wrong_handler tbl key = try Hashtbl.find tbl key with Failure _ -> 0

(* partial-function: a length guard proves the index is in range *)
let guarded_head l = if List.length l > 0 then List.hd l else 0
let guarded_nth l = if List.length l > 2 then List.nth l 1 else 0
let guarded_flipped l = if 0 < List.length l then List.hd l else 0
let guarded_ne l = if List.length l <> 0 then List.hd l else 0
let insufficient_guard l = if List.length l > 1 then List.nth l 5 else 0
let unguarded_head l = List.hd l
let wrong_direction l = if List.length l < 3 then List.hd l else 0
let dynamic_guard l n = if List.length l > n then List.hd l else 0

(* match-based partial-function guards *)
let singleton_does_not_cover_empty l =
  match l with [ item ] -> item | _ -> List.hd l
let match_fact_does_not_escape l =
  let _ = match l with [] -> 0 | _ -> 1 in
  List.hd l
let empty_arm_is_not_guarded l =
  match l with [] -> List.hd l | _ -> 0
let guarded_empty_arm_does_not_cover_empty condition l =
  match l with [] when condition -> 0 | _ -> List.hd l
let cons_pattern_proves_nonempty l =
  match l with _ :: _ -> List.hd l | [] -> 0

(* string-length comparison direction *)
let string_nonempty_left s = String.length s > 0
let string_nonempty_right s = 0 < String.length s
let string_length_never_negative s = String.length s < 0
let zero_never_above_string_length s = 0 > String.length s

(* Obj.repr is the safe half of an unchecked round trip. *)
let erased_representation value = Obj.repr value

(* Guard arithmetic must not overflow and manufacture a proof. *)
let huge_index_is_not_guarded l =
  if List.length l > 0 then List.nth l 4611686018427387903 else 0

let unsafe_option_get value = Option.get value

(* Predicates with per-item work are not equivalent to List.mem. *)
let recomputed_membership target values =
  List.exists (fun item -> item = target ()) values
let self_equality values = List.exists (fun item -> item = item) values

(* Filter/count existence tests in both useful directions. *)
let no_positive_via_count values =
  List.length (List.filter (fun item -> item > 0) values) = 0
let any_positive_reversed values =
  0 < List.length (List.filter (fun item -> item > 0) values)
let impossible_filter_count values =
  0 > List.length (List.filter (fun item -> item > 0) values)

(* A deferred lookup can observe a later table state. *)
let deferred_lookup key =
  if Hashtbl.mem table key then (fun () -> Hashtbl.find table key) else (fun () -> "absent")

module Custom = struct
  type result = Result of int
  type 'a option = None | Some of 'a
end

let discard_custom_result value = ignore (value : Custom.result)
let custom_option_map value =
  match (value : int Custom.option) with
  | Custom.Some item -> Custom.Some (option_target item)
  | Custom.None -> Custom.None

(* trivial-sprintf *)
let identity_format (s : string) = Printf.sprintf "%s" s
let int_format (n : int) = Printf.sprintf "%d" n
let bool_format (b : bool) = Printf.sprintf "%b" b
let float_format (f : float) = Printf.sprintf "%f" f
let real_format (n : int) = Printf.sprintf "count: %d" n
let two_args a b = Printf.sprintf "%s%s" a b

module Scoped_allow = struct
  [@@@lintocaml.allow
    ("partial-function", "this module handles partial calls at its boundary")]

  let hidden_partial value = List.hd value
end

let nested_structure_allow_does_not_escape value = List.hd value

(* swallowed-exception: a catch-all after a fatal re-raise is the correct shape *)
let guarded_catchall f =
  try f () with
  | (Out_of_memory | Stack_overflow | Sys.Break) as e -> raise e
  | _ -> 0

let bare_catchall f = try f () with _ -> 0

let nonfatal_reraise_does_not_protect f =
  try f () with
  | Not_found as e -> raise e
  | _ -> 0

(* negative-size: a labelled length hides the size from the rule, so the
   negative literal here is the fill value and must not be reported. The
   module has to be named Array for the path to match, which is how Base's
   Array.create reaches the rule. *)
module Array = struct
  include Stdlib.Array

  let create ~len value = Stdlib.Array.make len value
end

let labelled_fill_value = Array.create ~len:8 (-1)

(* poly-compare-on-abstract: seeded_hash takes the seed first, so the hashed
   value is the second argument *)
let seeded_hash_abstract (formatter : Format.formatter) =
  Hashtbl.seeded_hash 0 formatter

(* physical-assoc-lookup: a labelled key leaves the list in first position, and
   a list is boxed, so the rule must not read it as the key *)
module Keyed_list = struct
  let assq ~key l = Stdlib.List.assq key l
end

module List = struct
  include Stdlib.List

  let assq ~key l = Keyed_list.assq ~key l
end

let labelled_assq_key = List.assq ~key:"k" [ ("k", 1) ]

(* compare-result-equality: every exact non-zero value violates the contract *)
let cmp_eq_two a b = compare a b = 2
let cmp_eq_negative_two a b = -2 = compare a b

(* A guarded handler may decline the exception, and deferred code runs after
   its surrounding try expression has returned. *)
let conditionally_handled_find condition table key =
  try Hashtbl.find table key with Not_found when condition -> 0

let deferred_find table key =
  try (fun () -> Hashtbl.find table key) with Not_found -> fun () -> 0

let lazy_find table key =
  try lazy (Hashtbl.find table key) with Not_found -> lazy 0

let handled_or_pattern table key =
  try Hashtbl.find table key with (Not_found | Exit) -> 0

(* Subtree rules must see exception-handler bodies. *)
let concat_in_handler parts =
  List.fold_left
    (fun acc part -> try acc with Not_found -> acc ^ part)
    "" parts
