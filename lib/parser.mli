open Grammar
open Scanner

type parser_state = { token : token array; current_token : int }
type parse_error = string
type 'a parse_result = ('a, parse_error) result

type precedence =
  | None
  | Assignment
  | Or
  | And
  | Equality
  | Comparison
  | Term
  | Factor
  | Call

val peek : parser_state -> token option
val advance : parser_state -> int -> parser_state
val current_token : parser_state -> token
val parse_literal : parser_state -> expression parse_result * parser_state

val parse_binary :
  parser_state -> expression -> expression parse_result * parser_state

val parse_precedence :
  precedence -> parser_state -> expression parse_result * parser_state

val parse_expression : parser_state -> expression parse_result * parser_state
val init_parser : token list -> expression parse_result
