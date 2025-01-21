open Grammar
open Operator

type token =
  | Number of number
  | String of string 
  | Identifier of string
  | Keyword of keyword
  | Operator of operator
  | Symbol of symbol
  | EOF

val token_to_string : token -> string
val init_scanner : string -> token list

(* Test helper *)
val test : unit -> unit




