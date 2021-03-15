open Parser

let gen_fun_header fun_header asm_channel = 
  let Fun_Header (fun_name, _) = fun_header in
  Printf.fprintf asm_channel ".global _%s\n" fun_name;
  Printf.fprintf asm_channel "_%s:\n" fun_name

let gen_fun_body body asm_channel =
  let Return (Const i) = body in
  Printf.fprintf asm_channel "  movl $%d, %%eax\n" i;
  Printf.fprintf asm_channel "  ret"

let generate (Prog fun_decl) = 
  let asm_channel = open_out "asm.s" in
  let Fun (header, body) = fun_decl in
  gen_fun_header header asm_channel;
  gen_fun_body body asm_channel;
  close_out asm_channel
