open Grammar
open Scanner

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

type prefix_fn = token list -> (expression * token list) parse_result
type infix_fn = token list -> expression -> (expression * token list) parse_result

type parse_rule = {
  prefix : prefix_fn option;
  infix : infix_fn option;
  precedence : precedence;
}

val parse_literal : token list -> (expression * token list) parse_result
val parse_binary : token list -> expression -> (expression * token list) parse_result
val parse_precedence : precedence -> token list -> (expression * token list) parse_result
val parse_expression : token list -> (expression * token list) parse_result
val init_parser : token list -> expression parse_result
