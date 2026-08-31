type t = { file : string; line : int; col : int; end_line : int; end_col : int }

let compare a b =
  match String.compare a.file b.file with
  | 0 ->
      Stdlib.compare
        (a.line, a.col, a.end_line, a.end_col)
        (b.line, b.col, b.end_line, b.end_col)
  | ordering -> ordering

let one_based_column column =
  if column < 0 then 1 else if column = max_int then max_int else column + 1

let pp ppf t = Fmt.pf ppf "%s:%d:%d" t.file t.line (one_based_column t.col)
