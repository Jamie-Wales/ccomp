type number = Float of float | Integer of int

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

type keyword = Return | Int | Main
type symbol = LeftParen | RightParen | LeftCurly | RightCurly | Semicolon
type literal = Number of number | String of string

type expression =
  | Literal of literal
  | Binary of expression * operator * expression
  | Unary of operator * expression

val string_of_operator: operator -> string

val literal_to_string: literal -> string

val expression_to_string: expression -> string
val print_expression: expression -> unit
