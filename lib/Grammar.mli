open Operator
type number = Float of float | Integer of int
type keyword = Return | Int | Main
type symbol = LeftParen | RightParen | LeftCurly | RightCurly | Semicolon
type literal = Number of number | String of string

type expression =
  | Literal of literal
  | Binary of expression * operator * expression
  | Unary of operator * expression

type statement =  ExpressionStatement of expression | FunctionStatement of statement list | ReturnStatement of expression
type program = Program of statement list

val literal_to_string: literal -> string

val expression_to_string: expression -> string
val print_expression: expression -> unit
val statement_to_string: statement -> string
val print_program: program -> unit
