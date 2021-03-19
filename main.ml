open Lexer
open Parser
open Codegen

let rec last = function
| [] -> None
| h::[] -> Some h
| _::t -> last t

let get_program_name path =
  if Filename.extension path = ".c" then Filename.remove_extension path
  else (print_endline "File must have .c extension"; exit 1)

let lex path =
  let channel = open_in path in
  let lexbuf = Sedlexing.Utf8.from_channel channel in
  let rec aux lexbuf =
    let tok = tokenize lexbuf in
    if tok = EOF then (close_in channel; exit 0)
    else
    print_endline (token_to_string tok);
    aux lexbuf
  in
  aux lexbuf

let parse_program path =
  let channel = open_in path in
  let lexbuf = Sedlexing.Utf8.from_channel channel in
  let _ = try
    let program = parse lexbuf channel in
    pp_program program
  with Parsing_exception e ->
    match e with
    | None -> print_endline "unknown parsing exception occurred"
    | Some t -> print_endline ("parsing exception occurred due to invalid token: "  ^ (token_to_string t))
  in
  close_in channel

let compile path = 
  let program_name = get_program_name path in
  let channel = open_in path in
  let lexbuf = Sedlexing.Utf8.from_channel channel in
  let program = parse lexbuf channel in
  begin
    generate program;
    ignore(Sys.command ("gcc asm.s -o " ^ program_name));
    ignore(Sys.command "rm asm.s")
  end

let () = 
  let args = Sys.argv |> Array.to_list in
  match args with
  | [] 
  | _::[] -> print_endline "Please provide a file to compile"
  | _::path::[] -> parse_program path
  | _ -> print_endline "Please provide only a single file"
