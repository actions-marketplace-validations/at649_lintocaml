open Lintml_engine
open Expr_view

let docs =
  {|String and byte indexing and slicing raise Invalid_argument when a known
index, offset, or length is outside its valid range. Literal out-of-range
arguments are guaranteed failures rather than merely partial operations.

Use an in-range value or validate dynamic bounds before access. The rule proves
only negative bounds and bounds against literal strings; it does not infer the
length of arbitrary expressions.|}

let int_constant = function { desc = Const (Int value); _ } -> Some value | _ -> None

let literal_string = function
  | { desc = Const (String value); _ } -> Some value
  | _ -> None

let invalid_index source index =
  match int_constant index with
  | Some index when index < 0 -> true
  | Some index ->
      Option.fold ~none:false
        ~some:(fun value -> index >= String.length value)
        (literal_string source)
  | None -> false

let invalid_slice source offset length =
  match (int_constant offset, int_constant length) with
  | Some offset, _ when offset < 0 -> true
  | _, Some length when length < 0 -> true
  | Some offset, Some length -> (
      match literal_string source with
      | Some value -> offset > String.length value - length
      | None -> false)
  | _ -> false

let check (expression : Expr_view.t) =
  match expression.desc with
  | Apply { callee = Some callee; args = [ source; index ] }
    when path_is callee [ "Stdlib.String.get"; "Stdlib.Bytes.get" ]
         && invalid_index source index ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"use an index within the value's bounds"
          "this index is statically outside the value's bounds";
      ]
  | Apply { callee = Some callee; args = [ source; offset; length ] }
    when path_is callee [ "Stdlib.String.sub"; "Stdlib.Bytes.sub" ]
         && invalid_slice source offset length ->
      [
        Rule.finding ~loc:expression.loc
          ~suggestion:"use a non-negative slice contained within the value"
          "this slice is statically outside the value's bounds";
      ]
  | _ -> []

let rule : Rule.t =
  {
    id = "invalid-string-bounds";
    title = "Statically invalid string or byte bounds";
    category = Rule.Correctness;
    profile = Rule.Default;
    default_severity = Severity.Error;
    docs;
    check;
  }
