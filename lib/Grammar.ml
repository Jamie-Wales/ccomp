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

let literal_to_string(num: literal) : string =
  match num with
  | Number n -> (
      match n with Integer i -> string_of_int i | Float f -> string_of_float f)
  | String s -> s

let rec expression_to_string (expr : expression) : string =
  match expr with
  | Literal l -> literal_to_string l
  | Binary (left, op, right) ->
        Printf.sprintf "(%s %s %s)" (expression_to_string left) (string_of_operator op) (expression_to_string right)
 | Unary (op, expr) -> 
        Printf.sprintf "(%s%s)" (string_of_operator op) (expression_to_string expr)

let print_expression (expr : expression) : unit =
  print_string (expression_to_string expr);
  print_newline ()
