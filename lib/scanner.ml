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

let token_to_string = function
  | Number (Integer i) -> Printf.sprintf "Integer(%d)" i
  | Number (Float f) -> Printf.sprintf "Float(%f)" f
  | String s -> Printf.sprintf "String(\"%s\")" s
  | Identifier id -> Printf.sprintf "Identifier(%s)" id
  | Keyword k ->
      Printf.sprintf "Keyword(%s)"
        (match k with Return -> "return" | Int -> "int" | Main -> "main")
  | Operator op -> Printf.sprintf "Operator(%s)" (string_of_operator op)
  | Symbol s ->
      Printf.sprintf "Symbol(%s)"
        (match s with
        | LeftParen -> "("
        | RightParen -> ")"
        | LeftCurly -> "{"
        | RightCurly -> "}"
        | Semicolon -> ";")
  | EOF -> "EOF"

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
      regex = Str.regexp "\"\\([^\"]*\\)\"";
      constructor = (fun s line -> Some { lexeme = String s; line });
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
regex = Str.regexp "\\([+*/=<>!\\-]\\|<=\\|>=\\|==\\|!=\\)";
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

(* Helper functions *)
let update_scanner_state scanner new_position new_line =
  { scanner with position = new_position; line = new_line }

let is_whitespace ch = ch = ' ' || ch = '\t' || ch = '\r'

(* Main scanner function *)
let rec scan_tokens scanner acc =
  let rec skip_whitespace scanner =
    if
      scanner.position < String.length scanner.input
      && is_whitespace scanner.input.[scanner.position]
    then
      skip_whitespace
        (update_scanner_state scanner (scanner.position + 1) scanner.line)
    else scanner
  in
  let scanner = skip_whitespace scanner in
  if scanner.position >= String.length scanner.input then
    List.rev ({ lexeme = EOF; line = scanner.line } :: acc)
  else
    match match_token scanner.input scanner.position scanner.line with
    | Some (token, new_pos) ->
        let new_scanner = update_scanner_state scanner new_pos scanner.line in
        scan_tokens new_scanner (token :: acc)
    | None ->
        if scanner.input.[scanner.position] = '\n' then
          let new_scanner =
            update_scanner_state scanner (scanner.position + 1)
              (scanner.line + 1)
          in
          scan_tokens new_scanner acc
        else
          failwith
            (Printf.sprintf "Unexpected character at line %d, position %d"
               scanner.line (scanner.position + 1))

(* Initialize scanner and start tokenization *)
let init_scanner input =
  let initial_scanner = { input; position = 0; line = 1 } in
  scan_tokens initial_scanner []

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
  | Operator op -> Printf.printf "Operator %s\n" (string_of_operator op)
  | Symbol sym ->
      Printf.printf "Symbol %s\n"
        (match sym with
        | LeftParen -> "("
        | RightParen -> ")"
        | LeftCurly -> "{"
        | RightCurly -> "}"
        | Semicolon -> ";")
  | EOF -> Printf.printf "EOF\n"

(* Test function *)
let test () =
  let input = "int main() { return 42 + 3.14; }" in
  let tokens = init_scanner input in
  Printf.printf "Scanned %d tokens:\n" (List.length tokens);
  List.iter print_token tokens

(* Run the test *)
let () = test ()
