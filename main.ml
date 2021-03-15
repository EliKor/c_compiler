open Parser
open Codegen

let compile file = 
  let channel = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel channel in
  let program = parse lexbuf channel in
  generate program;
  ignore(Sys.command "gcc asm.s -o out");
  ignore(Sys.command "rm asm.s")

let () = 
  let args = Sys.argv |> Array.to_list in
  match args with
  | [] 
  | _::[] -> print_endline "Please provide a file to compile"
  | _::file_name::[] -> compile file_name 
  | _ -> print_endline "Please provide only a single file"