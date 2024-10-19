open Scanner
open Parser
open Grammar

let test_scanner () =
  let input = "10 + 2 * 3" in
  let tokens = init_scanner input in
  Printf.printf "Scanned %d tokens:\n" (List.length tokens);
  List.iter print_token tokens;
  tokens (* Return the tokens for use in parser test *)

let test_parser tokens =
  (* Convert token' list to token list *)
  let simple_tokens = List.map (fun t -> t.lexeme) tokens in
  match init_parser simple_tokens with
  | Ok expr ->
      Printf.printf "\nParsed expression:\n%s\n" (expression_to_string expr)
  | Error msg -> Printf.printf "\nParse error: %s\n" msg

let main () =
  Printf.printf "Testing Scanner:\n";
  let tokens = test_scanner () in
  Printf.printf "\nTesting Parser:\n";
  test_parser tokens

let () = main ()
