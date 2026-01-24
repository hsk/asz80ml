{
  open Parser
}
let digit = ['0'-'9']
let digit8 = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
rule token = parse
  | [' ' '\t']+ { token lexbuf }
  | '\n'        { loc := {!loc with line= !loc.line + 1}; EOL(!loc.line) }
  | '\r' '\n'   { loc := {!loc with line= !loc.line + 1}; EOL(!loc.line) }
  | '\r'        { loc := {!loc with line= !loc.line + 1}; EOL(!loc.line) }
  | ';' [^ '\n']*      { token lexbuf }
  | digit+ as n         { INT (int_of_string n) }
  | "0x" hex+ as n      { INT (int_of_string n) }
  | "$" (hex+ as n)     { INT (int_of_string ("0x" ^ n)) }
  | alpha alnum* "'"? as x   {
      match String.lowercase_ascii x with
      | "macro" -> MACRO
      | "endm" -> ENDM
      | "if" -> IF
      | "elif" -> ELIF
      | "else" -> ELSE
      | "endif" -> ENDIF
      | _ -> IDENT x
    }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '?'  { QUESTION }
  | ':'  { COLON }
  | '|'  { OR }
  | '^'  { CARET }
  | '&'  { AND }
  | '~'  { TILDE }
  | '+'  { ADD }
  | '-'  { SUB }
  | '*'  { MUL }
  | '/'  { DIV }
  | '%'  { MOD }
  | ','  { COMMA }
  | "=="  { EQ }
  | "="   { EQ }
  | "!="  { NE }
  | "<="  { LE }
  | ">="  { GE }
  | "<"   { LT }
  | ">"   { GT }
  | "<<"  { LSHIFT }
  | ">>"  { RSHIFT }
  | '"'   { read_string '"' (Buffer.create 17) lexbuf }
  | '\''  { read_string '\'' (Buffer.create 17) lexbuf }
  | eof   { EOF }
  | _ as c { Printf.printf "Unexpected character: '%c'\n" c; token lexbuf }

and read_string term buf = parse
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string term buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string term buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string term buf lexbuf }
  | '\\' 'a'  { Buffer.add_char buf (Char.chr 7); read_string term buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string term buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_string term buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string term buf lexbuf }
  | '\\' (digit8 digit8? digit8? as n)
      { Buffer.add_char buf (Char.chr (int_of_string ("0o" ^ n) land 0xFF)); read_string term buf lexbuf }
  | [^ '\\' '\'' '"']+ as s
      { Buffer.add_string buf s; read_string term buf lexbuf }
  | eof { failwith "Unterminated string" }
  | '\\' (_ as c) { (Buffer.add_char buf c; read_string term buf lexbuf) }
  | _ as c
      { if c = term then STRING (Buffer.contents buf) else (Buffer.add_char buf c; read_string term buf lexbuf) }
