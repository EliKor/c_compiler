open Lexer

type op = Neg | Bitwise_comp | Logical_neg 
type expr = Unop of op * expr | Const of int
type statement = Return of expr
type parameter = Param of string
type fun_header = Fun_Header of string * parameter list
type fun_decl = Fun of fun_header * statement
type prog = Prog of fun_decl

exception Parsing_exception
exception End_of_file

let get_token = tokenize

let build_level_str level = 
  let rec create_spaces acc level =
    if level = 0 then acc 
    else create_spaces (" "::acc) (level-1)
  in
  let spaces_lst = create_spaces [] level in
  String.concat " " spaces_lst

let pp_op op =
  match op with
  | Neg -> print_char '-'
  | Bitwise_comp -> print_char '~'
  | Logical_neg -> print_char '!'

let rec pp_expr e =
  match e with
  | Const i -> print_string (string_of_int i)
  | Unop (op, e') -> pp_op op; pp_expr e'

let rec pp_params params =
  match params with
  | [] -> ()
  | (Param s)::[] -> print_string s
  | (Param s)::t -> print_string (s ^ " "); pp_params t

let pp_fun_header (Fun_Header (fun_name, params)) level = 
  let level_str = build_level_str level in
  print_string level_str;
  print_string (fun_name ^ " (");
  pp_params params;
  print_string "):\n"

let pp_statement statement level = 
  let level_str = build_level_str level in
  print_string level_str;
  match statement with
  | Return e -> print_string "RETURN "; pp_expr e

let pp_fun_decl (Fun (header, body)) level = 
  pp_fun_header header level;
  pp_statement body (level+1);
  print_string "\n\n"

let pp_program (Prog function_declaration) = 
  print_endline "PROG:";
  pp_fun_decl function_declaration 1

let parse_param lexbuf =
  let param_type = get_token lexbuf in
  if param_type = CLOSED_PAREN then None 
  else
    match param_type, get_token lexbuf with
    | INT_T, ID i -> Some (Param i)
    | _ -> raise Parsing_exception

let parse_params lexbuf =
  let open_paren = get_token lexbuf in
  let rec params_helper lexbuf acc = 
    match parse_param lexbuf with
    | Some (Param i) -> 
        params_helper lexbuf ((Param i)::acc)
    | None -> acc 
  in
  let params = params_helper lexbuf [] in
  if open_paren = OPEN_PAREN then params 
  else raise Parsing_exception

let parse_fun_header lexbuf =
  let ret_type = get_token lexbuf in
  let name = get_token lexbuf in
  match ret_type, name with
  | INT_T, ID n -> 
    let args = parse_params lexbuf in
    Fun_Header (n, args)
  | _ -> raise Parsing_exception

let op_of_token tok =
  match tok with
  | NEG -> Neg
  | BIT_COMP -> Bitwise_comp
  | LOG_NEG -> Logical_neg
  | _ -> raise Parsing_exception

let rec parse_unop t lexbuf =
  let op = op_of_token t in
  Unop (op, parse_expression lexbuf)
and parse_expression lexbuf =
  let t = get_token lexbuf in
  match t with
  | INT i -> Const i
  | NEG 
  | BIT_COMP 
  | LOG_NEG -> parse_unop t lexbuf
  | _ -> raise Parsing_exception

let parse_statement lexbuf = 
  let ret = get_token lexbuf in
  let expression = parse_expression lexbuf in
  let semicolon = get_token lexbuf in
  match ret, semicolon with
  | RETURN, SEMICOLON -> Return expression 
  | _ -> raise Parsing_exception

let parse_body lexbuf = 
  let open_brace = get_token lexbuf in
  let st = parse_statement lexbuf in
  let closed_brace = get_token lexbuf in
  match open_brace, closed_brace with
  | OPEN_BRACE, CLOSED_BRACE -> st
  | _ -> raise Parsing_exception

let parse_function lexbuf = 
  let header = parse_fun_header lexbuf in
  let body = parse_body lexbuf in
  Fun (header, body)

let parse lexbuf channel =
  let program = Prog (parse_function lexbuf) in
  close_in channel;
  program
