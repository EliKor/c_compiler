open Parser

let gen_fun_header fun_header asm_channel = 
  let Fun_Header (fun_name, _) = fun_header in
  Printf.fprintf asm_channel ".global _%s\n" fun_name;
  Printf.fprintf asm_channel "_%s:\n" fun_name

let gen_const i asm_channel =
  Printf.fprintf asm_channel "  movl $%d, %%eax\n" i

let gen_unop unop asm_channel =
  match unop with
  | Neg -> Printf.fprintf asm_channel "  neg %%eax\n"
  | Bitwise_comp -> Printf.fprintf asm_channel "  not %%eax\n"
  | Logical_neg -> 
    begin
      Printf.fprintf asm_channel "  cmpl $0, %%eax\n";
      Printf.fprintf asm_channel "  movl $0, %%eax\n";
      Printf.fprintf asm_channel "  sete %%al\n"
    end

let rec gen_expr e asm_channel =
  match e with
  | Const i -> gen_const i asm_channel
| Unop (unop, e') -> 
    gen_expr e' asm_channel; 
    gen_unop unop asm_channel
  | _ -> failwith "Unimplemented" 

let gen_fun_body body asm_channel =
  let Return e = body in
  begin
  gen_expr e asm_channel;
  Printf.fprintf asm_channel "  ret"
  end

let generate (Prog fun_decl) = 
  let asm_channel = open_out "asm.s" in
  let Fun (header, body) = fun_decl in
  gen_fun_header header asm_channel;
  gen_fun_body body asm_channel;
  close_out asm_channel
