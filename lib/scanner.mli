open Grammar

(* Token types *)
type token =
  | Number of number
  | String of string
  | Identifier of string
  | Keyword of keyword
  | Operator of operator
  | Symbol of symbol
  | EOF

type token' = { lexeme : token; line : int }

(* Scanner state *)
type scanner = { input : string; position : int; line : int }

val token_to_string: token -> string(* Regex pattern type *)
val init_scanner: string -> token' list
val print_token: token' -> unit
type regex_pattern = {
  regex : Str.regexp;
  constructor : string -> int -> token' option;
}




