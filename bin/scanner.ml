open Token

type scanner = { input : string; position : int; line : int }

let update_scanner_state scanner new_position new_line =
  { scanner with position = new_position; line = new_line }

let is_whitespace ch = ch = ' ' || ch = '\t' || ch = '\r'

let rec init_scanner input =
  let scanner = { input; position = 0; line = 1 } in
  scan_tokens scanner []

and scan_tokens scanner acc =
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
  if scanner.position >= String.length scanner.input then List.rev acc
  else
    match matchToken scanner.input scanner.position scanner.line with
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

(* Test the scanner *)
let test () =
  let input = "20 + 3.14 * 2" in
  let tokens = init_scanner input in
  Printf.printf "Scanned %d tokens:\n" (List.length tokens);
  List.iter print_token tokens
