type number = Float of float | Integer of int
type operator = Plus | Minus | Star | Slash

type token =
  | Number of number
  | String of string
  | Keyword of string
  | Operator of operator

type token' = { lexeme : token; line : int }

let matchToken (str : string) (pos : int) (line : int) =
  if Str.string_match (Str.regexp "\\([0-9]+\\.[0-9]+\\)") str pos then
    Some
      ( {
          lexeme = Number (Float (float_of_string (Str.matched_group 1 str)));
          line;
        },
        Str.match_end () )
  else if Str.string_match (Str.regexp "\\([0-9]+\\)") str pos then
    Some
      ( {
          lexeme = Number (Integer (int_of_string (Str.matched_group 1 str)));
          line;
        },
        Str.match_end () )
  else if Str.string_match (Str.regexp "\\([+\\-\\*/]\\)") str pos then
    let op =
      match Str.matched_group 1 str with
      | "+" -> Plus
      | "-" -> Minus
      | "*" -> Star
      | "/" -> Slash
      | _ -> failwith "Unexpected operator"
    in
    Some ({ lexeme = Operator op; line }, Str.match_end ())
  else None

let print_token (tok : token') =
  Printf.printf "Token at %d: " tok.line;
  match tok.lexeme with
  | Number n -> (
      match n with
      | Integer i -> Printf.printf "Integer %d\n" i
      | Float f -> Printf.printf "Float %f\n" f)
  | String s -> Printf.printf "String \"%s\"\n" s
  | Keyword k -> Printf.printf "Keyword \"%s\"\n" k
  | Operator op ->
      Printf.printf "Operator %s\n"
        (match op with
        | Plus -> "+"
        | Minus -> "-"
        | Star -> "*"
        | Slash -> "/")
