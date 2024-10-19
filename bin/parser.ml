open Scanner
open Grammar

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

type prefix_fn = parser_state -> expression parse_result * parser_state

type infix_fn =
  parser_state -> expression -> expression parse_result * parser_state

type parse_rule = {
  prefix : prefix_fn option;
  infix : infix_fn option;
  precedence : precedence;
}

let peek parser =
  if parser.current_token < Array.length parser.token - 1 then
    Some parser.token.(parser.current_token + 1)
  else None

let advance parser amount =
  { parser with current_token = parser.current_token + amount }

let current_token (parser : parser_state) : token =
  parser.token.(parser.current_token)

let parse_literal (parser : parser_state) :
    expression parse_result * parser_state =
  match current_token parser with
  | Number n -> (
      match n with
      | Integer i -> (Ok (Literal (Number (Integer i))), advance parser 1)
      | Float f -> (Ok (Literal (Number (Float f))), advance parser 1))
  | _ ->
      ( Error
          (Printf.sprintf "Expected a number, got %s"
             (token_to_string (current_token parser))),
        parser )

let rec get_rule (token : token) : parse_rule =
  match token with
  | Number _ -> { prefix = Some parse_literal; infix = None; precedence = None }
  | Operator o -> (
      match o with
      | Plus | Minus ->
          { prefix = None; infix = Some parse_binary; precedence = Term }
      | Star | Slash ->
          { prefix = None; infix = Some parse_binary; precedence = Factor }
      | _ -> { prefix = None; infix = None; precedence = None })
  | _ -> { prefix = None; infix = None; precedence = None }

and parse_binary (parser : parser_state) (left : expression) :
    expression parse_result * parser_state =
  match current_token parser with
  | Operator op -> (
      let parser = advance parser 1 in
      let right_result, new_parser =
        parse_precedence (get_rule (Operator op)).precedence parser
      in
      match right_result with
      | Ok right -> (Ok (Binary (left, op, right)), new_parser)
      | Error e -> (Error e, new_parser))
  | _ ->
      ( Error
          (Printf.sprintf "Expected operator, got %s"
             (token_to_string (current_token parser))),
        parser )

and parse_precedence precedence parser =
  let current_tok = current_token parser in
  let prefix_rule = get_rule current_tok in
  let expr_result, parser =
    match prefix_rule.prefix with
    | Some prefix_fn -> prefix_fn parser
    | None ->
        ( Error
            (Printf.sprintf "Expected expression, got %s"
               (token_to_string current_tok)),
          parser )
  in
  let rec parse_precedence_loop expr_result parser =
    match expr_result with
    | Error _ -> (expr_result, parser)
    | Ok expr ->
        if parser.current_token >= Array.length parser.token then
          (Ok expr, parser)
        else
          let current_tok = current_token parser in
          let rule = get_rule current_tok in
          if precedence < rule.precedence then
            match rule.infix with
            | Some infix_fn ->
                let new_expr_result, new_parser = infix_fn parser expr in
                parse_precedence_loop new_expr_result new_parser
            | None -> (Ok expr, parser)
          else (Ok expr, parser)
  in
  parse_precedence_loop expr_result parser

let parse_expression parser = parse_precedence None parser

let init_parser (tokens : token list) : expression parse_result =
  let initial_parser = { token = Array.of_list tokens; current_token = 0 } in
  let result, _ = parse_expression initial_parser in
  result
