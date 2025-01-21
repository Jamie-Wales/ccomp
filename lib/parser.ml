open Grammar
open Scanner
open Operator

type parse_error = string
type 'a parse_result = ('a, parse_error) result

type precedence =
  | None
  | Assignment  (* lowest *)
  | Or
  | And 
  | Equality
  | Comparison
  | Term        (* + - *)
  | Factor      (* * / *)
  | Call        (* highest *)

type prefix_fn = token list -> (expression * token list) parse_result 
type infix_fn = token list -> expression -> (expression * token list) parse_result

type parse_rule = {
  prefix : prefix_fn option;
  infix : infix_fn option;
  precedence : precedence;
}

let parse_literal tokens =
  match tokens with
  | (Number n) :: rest -> 
      Ok (Literal (Number n), rest)
  | tok :: _ -> 
      Error (Printf.sprintf "Expected a number, got %s" (token_to_string tok))
  | [] -> 
      Error "Unexpected end of input"

let rec get_rule = function
  | Number _ -> 
      { prefix = Some parse_literal; infix = None; precedence = None }
  | Operator op -> (
      match op with
      | Minus ->
          { prefix = Some parse_unary; infix = Some parse_binary; precedence = Term }
      | Plus ->
          { prefix = None; infix = Some parse_binary; precedence = Term }
      | Star | Slash ->
          { prefix = None; infix = Some parse_binary; precedence = Factor }
      | _ -> 
          { prefix = None; infix = None; precedence = None })
  | _ -> 
      { prefix = None; infix = None; precedence = None }

and parse_unary tokens =
  match tokens with
  | (Operator op) :: rest -> (
      match parse_precedence Factor rest with
      | Ok (right, new_rest) -> 
          Ok (Unary (op, right), new_rest)
      | Error e -> 
          Error e)
  | tok :: _ ->
      Error (Printf.sprintf "Expected operator, got %s" (token_to_string tok))
  | [] -> 
      Error "Unexpected end of input"

and parse_binary tokens left =
  match tokens with
  | (Operator op) :: rest -> (
      let rule = get_rule (Operator op) in
      match parse_precedence rule.precedence rest with
      | Ok (right, new_rest) -> 
          Ok (Binary (left, op, right), new_rest)
      | Error e -> 
          Error e)
  | tok :: _ ->
      Error (Printf.sprintf "Expected operator, got %s" (token_to_string tok))
  | [] -> 
      Error "Unexpected end of input"

and parse_precedence precedence tokens =
  match tokens with
  | [] -> 
      Error "Unexpected end of input"
  | current_tok :: _ ->
      let prefix_rule = get_rule current_tok in
      match prefix_rule.prefix with
      | None ->
          Error (Printf.sprintf "Expected expression, got %s" 
                  (token_to_string current_tok))
      | Some prefix_fn ->
          match prefix_fn tokens with
          | Error e -> Error e
          | Ok (expr, rest) -> parse_precedence_loop precedence expr rest

and parse_precedence_loop precedence left_expr tokens =
  match tokens with
  | [] -> 
      Ok (left_expr, [])
  | next_tok :: _ ->
      let rule = get_rule next_tok in
      if rule.precedence > precedence then
        match rule.infix with
        | Some infix_fn ->
            (match infix_fn tokens left_expr with
            | Ok (new_expr, new_tokens) -> 
                parse_precedence_loop precedence new_expr new_tokens
            | Error e -> 
                Error e)
        | None -> 
            Ok (left_expr, tokens)
      else 
        Ok (left_expr, tokens)

let parse_expression = parse_precedence None

let parse_function tokens = 
    []

    

let parse_statement tokens =
    match tokens with
    | Keyword f :: Identifier s :: Symbol p :: tail -> 
        parse_function tail 
    | tail -> []
    


let init_parser tokens =
  match parse_expression tokens with
  | Ok (expr, [EOF]) -> Ok expr  (* Allow EOF at end *)
  | Ok (expr, []) -> Ok expr     (* Or empty list *)
  | Ok (_, rest) -> Error (Printf.sprintf "Unexpected tokens after expression: %s" 
                            (String.concat " " (List.map token_to_string rest)))
  | Error e -> Error e
