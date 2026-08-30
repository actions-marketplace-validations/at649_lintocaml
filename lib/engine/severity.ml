type t = Error | Warning | Hint

let to_string = function Error -> "error" | Warning -> "warning" | Hint -> "hint"

let of_string = function
  | "error" -> Some Error
  | "warning" -> Some Warning
  | "hint" -> Some Hint
  | _ -> None

let rank = function Error -> 3 | Warning -> 2 | Hint -> 1
let at_least ~threshold t = rank t >= rank threshold
