open Lintocaml_engine

let all : Rule.t list =
  [
    Physical_eq_on_boxed.rule;
    Poly_compare_on_function.rule;
    Poly_compare_on_abstract.rule;
    Poly_compare_on_float_nan.rule;
    Ignored_result.rule;
    Ignored_promise.rule;
    Division_by_zero.rule;
    Negative_size.rule;
    Invalid_string_bounds.rule;
    Disabled_all_warnings.rule;
    Mutable_hashtable_key.rule;
    Partial_function.rule;
    Swallowed_exception.rule;
    Unsafe_string_index.rule;
    Length_compare_zero.rule;
    Length_compare_n.rule;
    Quadratic_concat.rule;
    List_append_in_loop.rule;
    Left_nested_list_append.rule;
    Repeated_list_nth.rule;
    Redundant_string_concat.rule;
    List_fusion.rule;
    Redundant_reverse.rule;
    Redundant_if_bool.rule;
    Boolean_comparison.rule;
    Double_negation.rule;
    Constant_condition.rule;
    Option_match_to_combinator.rule;
    Redundant_list_append.rule;
    Redundant_fun_wrapper.rule;
    Match_bool.rule;
    Generic_failure.rule;
    Compare_result_equality.rule;
    Negative_list_index.rule;
    Length_of_mapped_list.rule;
    Head_of_traversal.rule;
    Negated_condition.rule;
    Identity_map.rule;
    Redundant_string_sub.rule;
    Exists_via_filter.rule;
    Hashtbl_mem_then_find.rule;
    Empty_filter_equality.rule;
    Exists_equals_mem.rule;
    String_length_compare_empty.rule;
    Physical_assoc_lookup.rule;
    Truncated_int_division.rule;
    Length_of_append.rule;
    Discarding_extractor.rule;
    Unsafe_cast.rule;
    Head_of_filter.rule;
    Trivial_sprintf.rule;
  ]
  |> List.sort (fun (left : Rule.t) right -> String.compare left.id right.id)

let ids = List.map (fun (r : Rule.t) -> r.Rule.id) all
let find id = List.find_opt (fun (r : Rule.t) -> String.equal r.Rule.id id) all
