{
  open Parser
}

rule token = parse
  (* Whitespace but preserve newlines *)
  | [' ' '\t']+ { token lexbuf }
  | '\n' { Parser.line := !Parser.line + 1; NEWLINE }
  | '\r' '\n' { Parser.line := !Parser.line + 1; NEWLINE }
  | '\r' { Parser.line := !Parser.line + 1; NEWLINE }
  
  (* Comments *)
  | "//" [^ '\n']* { token lexbuf }
  
  (* Integer literals *)
  | ['0'-'9']+ as num
      { INT (int_of_string num) }
  | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ as num
      { INT (int_of_string num) }
  | "$" ['0'-'9' 'a'-'f' 'A'-'F']+ as num
      (* OCaml's int_of_string handles 0x but not $, so we convert $FF -> 0xFF *)
      { INT (int_of_string ("0x" ^ (String.sub num 1 (String.length num - 1)))) }
  
  (* Identifiers *)
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id
      { IDENT id }
  
  (* Single-character tokens *)
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '?'  { QUESTION }
  | ':'  { COLON }
  | '|'  { PIPE }
  | '^'  { CARET }
  | '&'  { AMPERSAND }
  | '~'  { TILDE }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { STAR }
  | '/'  { SLASH }
  | '%'  { PERCENT }
  | ','  { COMMA }
  
  (* Multi-character tokens *)
  | "=="  { EQUAL_EQUAL }
  | "="   { EQUAL }
  | "!="  { NOT_EQUAL }
  | "<="  { LESS_EQUAL }
  | ">="  { GREATER_EQUAL }
  | "<"   { LESS }
  | ">"   { GREATER }
  | "<<"  { LSHIFT }
  | ">>"  { RSHIFT }
  
  (* End of file *)
  | eof  { EOF }
  
  (* Error handling *)
  | _ as c
      { Printf.printf "Unexpected character: '%c'\n" c;
        token lexbuf }
