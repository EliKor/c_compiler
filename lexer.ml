type token = 
  | INT_T 
  | ID of string 
  | OPEN_PAREN
  | CLOSED_PAREN
  | OPEN_BRACE
  | CLOSED_BRACE
  | RETURN
  | SEMICOLON
  | PLUS
  | TIMES
  | DIVIDE
  | NEG
  | BIT_COMP
  | LOG_NEG
  | INT of int 
  | EOF

exception Invalid_token of string

let token_to_string = function
  | INT_T -> "INT_T"
  | ID s -> "ID " ^ s
  | OPEN_PAREN -> ")"
  | CLOSED_PAREN -> "("
  | OPEN_BRACE -> "{"
  | CLOSED_BRACE -> "}"
  | RETURN -> "RETURN"
  | INT i -> "INT<" ^ (string_of_int i) ^ ">"
  | SEMICOLON -> ";"
  | PLUS -> "+"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | NEG -> "-"
  | BIT_COMP -> "~"
  | LOG_NEG -> "!"
  | EOF -> "EOF"

let digit = [%sedlex.regexp? ('0'..'9')]
let whitespace = [%sedlex.regexp? (' '|'\t'|'\r'|'\n')]
let number = [%sedlex.regexp? '0'..'9', Star (digit)]
let letter = [%sedlex.regexp? ('a'..'z'|'A'..'Z')]
let identifier = [%sedlex.regexp? letter, Star ('_'|digit|letter)]

let rec tokenize lexbuf =
  match%sedlex lexbuf with
  | whitespace -> tokenize lexbuf
  | number -> let i = lexbuf |> Sedlexing.Utf8.lexeme |> int_of_string in INT i 
  | "int" -> INT_T
  | "return" -> RETURN
  | '(' -> OPEN_PAREN
  | ')' -> CLOSED_PAREN
  | '{' -> OPEN_BRACE
  | '}' -> CLOSED_BRACE
  | ';' -> SEMICOLON
  | '+' -> PLUS
  | '*' -> TIMES
  | '/' -> DIVIDE
  | '-' -> NEG
  | '~' -> BIT_COMP
  | '!' -> LOG_NEG
  | identifier -> let id = lexbuf |> Sedlexing.Utf8.lexeme in ID id 
  | eof -> EOF
  | _ -> let invalid = lexbuf |> Sedlexing.Utf8.lexeme in raise (Invalid_token invalid)
