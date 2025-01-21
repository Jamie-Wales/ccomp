
type operator =
  | Plus
  | Minus
  | Star
  | Slash
  | Equal
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | EqualEqual
  | BangEqual
  | Bang

let string_of_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Equal -> "="
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | EqualEqual -> "=="
  | BangEqual -> "!="
  | Bang -> "!"

