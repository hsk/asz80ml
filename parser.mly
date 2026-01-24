%{
  open Ast
  let loc = ref {file = ""; line = 0}
  let l operand = { location = !loc; operand }
  let l1 operand = { location = {!loc with line = !loc.line+1}; operand }
  let l2 (operand,line) = { location = {!loc with line}; operand }
%}
%token <int> INT EOL
%token <string> IDENT STRING
%token LPAREN "(" RPAREN ")" QUESTION "?" COLON ":" OR "|" CARET "^" AND "&"
%token MACRO "macro" ENDM "endm" IF "if" ELIF "elif" ELSE "else" ENDIF "endif" INCLUDE "include"
%token EQEQ "==" EQ "=" NE "!=" LE "<=" GE ">=" LT "<" GT ">"
%token LSHIFT "<<" RSHIFT ">>" ADD "+" SUB "-" MUL "*" DIV "/" MOD "%"
%token TILDE "~" COMMA "," EOF
%left "?" ":"
%left "|"
%left "^"
%left "&"
%left "==" "=" "!="
%left "<=" ">=" "<" ">"
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%right UNARY
%start <Ast.program> main
%%
main:
  | program_item* EOF       { List.flatten $1 }
program_item:
  | EOL                     { [] }
  | IDENT ":"               { [l1(Label $1)] }
  | "macro" separated_list(",", IDENT) EOL program_item* "endm" EOL
                            { [l2(MacroDef($2, List.flatten $4),$3)] }
  | "if" expr EOL if_parts  { let (t, e) = $4 in [l2(If($2, t, e),$3)] }
  | "include" STRING EOL    { [l(Include $2)] }
  | IDENT separated_nonempty_list(",", expr) EOL
                            { [l(Expr($1, $2))] }
  | IDENT EOL               { [l(Expr($1, []))] }
if_parts:
  | "endif" EOL             { ([], []) }
  | "else" EOL else_part    { ([], $3) }
  | "elif" expr EOL if_parts{ let (t, e) = $4 in ([], [l2(If($2, t, e),$3)]) }
  | program_item if_parts   { let (t, e) = $2 in ($1 @ t, e) }
else_part:
  | "endif" EOL { [] }
  | program_item else_part { $1 @ $2 }
expr:
  | expr "?" expr ":" expr  { Ternary($1, $3, $5) }
  | expr "|" expr           { Or($1, $3) }
  | expr "^" expr           { Xor($1, $3) }
  | expr "&" expr           { And($1, $3) }
  | expr "==" expr          { Eq($1, $3) }
  | expr "=" expr           { Eq($1, $3) }
  | expr "!=" expr          { Ne($1, $3) }
  | expr "<=" expr          { Le($1, $3) }
  | expr ">=" expr          { Ge($1, $3) }
  | expr "<" expr           { Lt($1, $3) }
  | expr ">" expr           { Gt($1, $3) }
  | expr "<<" expr          { LShift($1, $3) }
  | expr ">>" expr          { RShift($1, $3) }
  | expr "+" expr           { Add($1, $3) }
  | expr "-" expr           { Sub($1, $3) }
  | expr "*" expr           { Mul($1, $3) }
  | expr "/" expr           { Div($1, $3) }
  | expr "%" expr           { Mod($1, $3) }
  | "~" expr %prec UNARY    { Not($2) }
  | "+" expr %prec UNARY    { UAdd($2) }
  | "-" expr %prec UNARY    { USub($2) }
  | INT                     { Int($1) }
  | IDENT                   { Var($1) }
  | "(" expr ")"            { Paren($2) }
