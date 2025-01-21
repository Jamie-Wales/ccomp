open Compiler_lib
open Grammar
open Scanner
open Parser

let test_scanner () =
    let input = "10 + 2 * -3" in
    let tokens = init_scanner input in
    Printf.printf "Scanned %d tokens:\n" (List.length tokens);
    List.iter (fun token -> 
        Printf.printf "%s\n" (token_to_string token)
    ) tokens;
    tokens

let test_parser tokens =
    match init_parser tokens with
    | Ok expr ->
        Printf.printf "\nParsed expression:\n%s\n" (expression_to_string expr)
    | Error msg -> 
        Printf.printf "\nParse error: %s\n" msg

let main () =
    Printf.printf "Testing Scanner:\n";
    let tokens = test_scanner () in
    Printf.printf "\nTesting Parser:\n";
    test_parser tokens

let () = main ()
