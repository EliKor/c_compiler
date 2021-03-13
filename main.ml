open Parser

let compile file = 
  let channel = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel channel in
  let program = parse lexbuf channel in
  pp_program program

let () = 
  let args = Sys.argv |> Array.to_list in
  match args with
  | [] 
  | _::[] -> print_endline "Please provide a file to compile"
  | _::file_name::[] -> compile file_name 
  | _ -> print_endline "Please provide only a single file"