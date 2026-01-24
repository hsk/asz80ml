{
  open Parser
}
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
rule token = parse
  | [' ' '\t']+ { token lexbuf }
  | '\n'        { loc := {!loc with line= !loc.line + 1}; EOL(!loc.line) }
  | '\r' '\n'   { loc := {!loc with line= !loc.line + 1}; EOL(!loc.line) }
  | '\r'        { loc := {!loc with line= !loc.line + 1}; EOL(!loc.line) }
  | "//" [^ '\n']*      { token lexbuf }
  | digit+ as n         { INT (int_of_string n) }
  | "0x" hex+ as n      { INT (int_of_string n) }
  | "$" (hex+ as n)     { INT (int_of_string ("0x" ^ n)) }
  | alpha alnum* as x   {
      match String.lowercase_ascii x with
      | "macro" -> MACRO
      | "endm" -> ENDM
      | "if" -> IF
      | "elif" -> ELIF
      | "else" -> ELSE
      | "endif" -> ENDIF
      | "include" -> INCLUDE
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
  | '"' [^ '"']* '"' as s { STRING (String.sub s 1 (String.length s - 2)) }
  | eof   { EOF }
  | _ as c { Printf.printf "Unexpected character: '%c'\n" c; token lexbuf }
