open Token
open Grammar

type parser_state = { token : token list; current_token : int }
type parse_error = string
type success = expression option
type parse_result = (success, parse_error) result

(* Helper function to advance the parser state *)
let advance parser amount =
  { parser with current_token = parser.current_token + amount }

(* Function to parse a literal expression *)
let parse_literal parser : parse_result =
  match List.hd parser.token with
  | Number n -> Ok (Some (Literal n))
  | _ -> Error "Expected a number literal"

let rec parse (parser : parser_state) (acc : expression list) : parse_result =
  match parse_literal parser with
  | Ok (Some expr) -> parse (advance parser 1) (expr :: acc)
  | Ok None -> Ok None
  | Error msg -> Error msg

(* Initialize the parser and start parsing *)
let init_parser (tokens : token list) : parse_result =
  let initial_parser = { token = tokens; current_token = 0 } in
  parse initial_parser []
