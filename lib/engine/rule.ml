type category = Correctness | Performance | Idiom

let category_to_string = function
  | Correctness -> "correctness"
  | Performance -> "performance"
  | Idiom -> "idiom"

(* Default runs unless disabled; the other two have to be asked for. *)
type profile = Default | Idiomatic | Pedantic

(* How a fix substitutes text. Built through [Fix.source] rather than directly,
   which decides whether parentheses are needed. *)
type replacement = Source of Loc.t | Parenthesized_source of Loc.t | Text of string

(* The engine attaches the rule id and the configured severity, so a rule needs
   to know neither. *)
type finding = {
  f_loc : Loc.t;
  f_message : string;
  f_suggestion : string option;
  f_fix : replacement option;
}

let finding ?suggestion ?fix ~loc message =
  { f_loc = loc; f_message = message; f_suggestion = suggestion; f_fix = fix }

type t = {
  id : string;
  title : string;
  category : category;
  profile : profile;
  default_severity : Severity.t;
  docs : string; (* Rendered by [lintml explain]. *)
  check : Expr_view.t -> finding list;
}
