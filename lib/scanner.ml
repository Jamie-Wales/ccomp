open Grammar
open Operator

(* Token types *)
type token =
  | Number of number
  | String of string
  | Identifier of string
  | Keyword of keyword
  | Operator of operator
  | Symbol of symbol
  | EOF

(* Scanner state *)
type scanner = { input : string; position : int }

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
        | LeftParen -> "(" | RightParen -> ")"
        | LeftCurly -> "{" | RightCurly -> "}"
        | Semicolon -> ";")
  | EOF -> "EOF"

(* Regex pattern type *)
type regex_pattern = {
  regex : Str.regexp;
  constructor : string -> token option;
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
      constructor = (fun s -> Some (Number (Float (float_of_string s))));
    };
    {
      regex = Str.regexp "\\([0-9]+\\)";
      constructor = (fun s -> Some (Number (Integer (int_of_string s))));
    };
    {
      regex = Str.regexp "\"\\([^\"]*\\)\"";
      constructor = (fun s -> Some (String s));
    };
    {
      regex = Str.regexp "\\([a-zA-Z_][a-zA-Z0-9_]*\\)";
      constructor = (fun s ->
        match is_keyword s with
        | Some (_, kw) -> Some (Keyword kw)
        | None -> Some (Identifier s));
    };
    {
      regex = Str.regexp "\\([+*/=<>!\\-]\\|<=\\|>=\\|==\\|!=\\)";
      constructor = (fun s ->
        let op = match s with
          | "+" -> Plus
          | "-" -> Minus
          | "*" -> Star
          | "/" -> Slash
          | _ -> failwith "Unexpected operator"
        in
        Some (Operator op));
    };
    {
      regex = Str.regexp "\\([(]\\)";
      constructor = (fun _ -> Some (Symbol LeftParen));
    };
    {
      regex = Str.regexp "\\([)]\\)";
      constructor = (fun _ -> Some (Symbol RightParen));
    };
    {
      regex = Str.regexp "\\([{]\\)";
      constructor = (fun _ -> Some (Symbol LeftCurly));
    };
    {
      regex = Str.regexp "\\([}]\\)";
      constructor = (fun _ -> Some (Symbol RightCurly));
    };
    {
      regex = Str.regexp "\\([;]\\)";
      constructor = (fun _ -> Some (Symbol Semicolon));
    };
  ]

(* Token matching function *)
let match_token input position =
  let rec try_patterns = function
    | [] -> None
    | pattern :: rest ->
        if Str.string_match pattern.regex input position then
          let matched = Str.matched_group 1 input in
          match pattern.constructor matched with
          | Some token -> Some (token, Str.match_end ())
          | None -> try_patterns rest
        else try_patterns rest
  in
  try_patterns patterns

(* Helper functions *)
let advance scanner new_position =
  { scanner with position = new_position }

let is_whitespace ch = ch = ' ' || ch = '\t' || ch = '\r' || ch = '\n'

(* Main scanner function *)
let rec scan_tokens scanner acc =
  let rec skip_whitespace scanner =
    if scanner.position < String.length scanner.input 
       && is_whitespace scanner.input.[scanner.position] then
      skip_whitespace (advance scanner (scanner.position + 1))
    else scanner
  in
  let scanner = skip_whitespace scanner in
  if scanner.position >= String.length scanner.input then
    List.rev (EOF :: acc)
  else
    match match_token scanner.input scanner.position with
    | Some (token, new_pos) ->
        let new_scanner = advance scanner new_pos in
        scan_tokens new_scanner (token :: acc)
    | None ->
        if scanner.input.[scanner.position] = '\n' then
          scan_tokens (advance scanner (scanner.position + 1)) acc
        else
          failwith
            (Printf.sprintf "Unexpected character at position %d"
               (scanner.position + 1))

(* Initialize scanner and start tokenization *)
let init_scanner input =
  let initial_scanner = { input; position = 0 } in
  scan_tokens initial_scanner []

(* Test function *)
let test () =
  let input = "int main() { return 42 + 3.14; }" in
  let tokens = init_scanner input in
  Printf.printf "Scanned %d tokens:\n" (List.length tokens);
  List.iter (fun tok -> Printf.printf "%s\n" (token_to_string tok)) tokens

(* Run the test *)
let () = test ()
