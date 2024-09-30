(* Token types *)
type number = Float of float | Integer of int
type operator = Plus | Minus | Star | Slash
type keyword = Return | Int | Main
type symbol = LeftParen | RightParen | LeftCurly | RightCurly | Semicolon

type token =
  | Number of number
  | String of string
  | Identifier of string
  | Keyword of keyword
  | Operator of operator
  | Symbol of symbol

type token' = { lexeme : token; line : int }

(* Regex pattern type *)
type regex_pattern = {
  regex : Str.regexp;
  constructor : string -> int -> token' option;
}

(* List of keywords *)
let keywords = [ ("return", Return); ("int", Int); ("main", Main) ]

(* Helper function to check if a string is a keyword *)
let is_keyword s = List.find_opt (fun (kw, _) -> kw = s) keywords

(* List of regex patterns and their token constructors *)
let patterns =
  [
    {
      regex = Str.regexp "\\([0-9]+\\.[0-9]+\\)";
      constructor =
        (fun s line ->
          Some { lexeme = Number (Float (float_of_string s)); line });
    };
    {
      regex = Str.regexp "\\([0-9]+\\)";
      constructor =
        (fun s line ->
          Some { lexeme = Number (Integer (int_of_string s)); line });
    };
    {
      regex = Str.regexp "\\([a-zA-Z_][a-zA-Z0-9_]*\\)";
      constructor =
        (fun s line ->
          match is_keyword s with
          | Some (_, kw) -> Some { lexeme = Keyword kw; line }
          | None -> Some { lexeme = Identifier s; line });
    };
    {
      regex = Str.regexp "\\([+\\-\\*/]\\)";
      constructor =
        (fun s line ->
          let op =
            match s with
            | "+" -> Plus
            | "-" -> Minus
            | "*" -> Star
            | "/" -> Slash
            | _ -> failwith "Unexpected operator"
          in
          Some { lexeme = Operator op; line });
    };
    {
      regex = Str.regexp "\\([(]\\)";
      constructor = (fun _ line -> Some { lexeme = Symbol LeftParen; line });
    };
    {
      regex = Str.regexp "\\([)]\\)";
      constructor = (fun _ line -> Some { lexeme = Symbol RightParen; line });
    };
    {
      regex = Str.regexp "\\([{]\\)";
      constructor = (fun _ line -> Some { lexeme = Symbol LeftCurly; line });
    };
    {
      regex = Str.regexp "\\([}]\\)";
      constructor = (fun _ line -> Some { lexeme = Symbol RightCurly; line });
    };
    {
      regex = Str.regexp "\\([;]\\)";
      constructor = (fun _ line -> Some { lexeme = Symbol Semicolon; line });
    };
  ]

(* Token matching function *)
let match_token input position line =
  let rec try_patterns = function
    | [] -> None
    | pattern :: rest ->
        if Str.string_match pattern.regex input position then
          let matched = Str.matched_group 1 input in
          match pattern.constructor matched line with
          | Some token -> Some (token, Str.match_end ())
          | None -> try_patterns rest
        else try_patterns rest
  in
  try_patterns patterns

(* Print token function *)
let print_token (tok : token') =
  Printf.printf "Token at line %d: " tok.line;
  match tok.lexeme with
  | Number n -> (
      match n with
      | Integer i -> Printf.printf "Integer %d\n" i
      | Float f -> Printf.printf "Float %f\n" f)
  | String s -> Printf.printf "String \"%s\"\n" s
  | Identifier s -> Printf.printf "Identifier \"%s\"\n" s
  | Keyword k ->
      Printf.printf "Keyword \"%s\"\n"
        (match k with Return -> "return" | Int -> "int" | Main -> "main")
  | Operator op ->
      Printf.printf "Operator %s\n"
        (match op with
        | Plus -> "+"
        | Minus -> "-"
        | Star -> "*"
        | Slash -> "/")
  | Symbol sym ->
      Printf.printf "Symbol %s\n"
        (match sym with
        | LeftParen -> "("
        | RightParen -> ")"
        | LeftCurly -> "{"
        | RightCurly -> "}"
        | Semicolon -> ";")
