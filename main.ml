open Lexer

let rec get_token lexbuf channel = 
  let curr_token = tokenize lexbuf in
  if curr_token = EOF then close_in channel
  else
    curr_token |> token_to_string |> print_endline;
    get_token lexbuf channel

let compile file = 
  let channel = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel channel in
  get_token lexbuf channel

let () = 
  let args = Sys.argv |> Array.to_list in
  match args with
  | [] 
  | _::[] -> print_endline "Please provide a file to compile"
  | _::file_name::[] -> compile file_name 
  | _ -> print_endline "Please provide only a single file"