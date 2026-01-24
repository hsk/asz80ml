%{
  open Ast
  let line = ref 0
  let file = ref ""
  let l operand = { location = {file = !file; line = !line}; operand }
  let l2 (operand,line) = { location = {file = !file; line}; operand }
%}
%token <int> INT EOL
%token <string> IDENT
%token LPAREN "(" RPAREN ")" QUESTION "?" COLON ":" OR "|" CARET "^" AND "&"
%token MACRO "macro" ENDM "endm" IF "if" ELIF "elif" ELSE "else" ENDIF "endif"
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
  | IDENT ":" EOL           { [l(Label $1)] }
  | IDENT ":"               { [l(Label $1)] }
  | "macro" separated_list(",", IDENT) EOL program_item* "endm" EOL
                            { [l2(MacroDef($2, List.flatten $4),$3)] }
  | "if" expr EOL if_parts  { let (t, e) = $4 in [l2(If($2, t, e),$3)] }
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
