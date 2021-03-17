open Parser
open Codegen

let rec last = function
| [] -> None
| h::[] -> Some h
| _::t -> last t

let get_program_name path =
  if Filename.extension path = ".c" then Filename.remove_extension path
  else (print_endline "File must have .c extension"; exit 1)

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
  | _::file_name::[] -> compile file_name 
  | _ -> print_endline "Please provide only a single file"
