%{
  open Ast
  let line = ref 0
  let file = ref ""
  let l operand = { location = {file = !file; line = !line}; operand }
%}
%token <int> INT
%token <string> IDENT
%token LPAREN "(" RPAREN ")" QUESTION "?" COLON ":" OR "|" CARET "^" AND "&" MACRO ENDM
%token EQEQ "==" EQ "=" NE "!=" LE "<=" GE ">=" LT "<" GT ">"
%token LSHIFT "<<" RSHIFT ">>" ADD "+" SUB "-" MUL "*" DIV "/" MOD "%"
%token TILDE "~" COMMA "," EOL EOF
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
  | program_item* EOF       { $1 }
program_item:
  | IDENT ":" EOL           { l(Label $1) }
  | IDENT ":"               { l(Label $1) }
  | MACRO macro_params EOL macro_body
                            { l(MacroDef($2, $4)) }
  | instruction             { $1 }
instruction:
  | IDENT exprs EOL         { l(Expr($1, $2)) }
  | IDENT EOL               { l(Expr($1, [])) }
exprs:
  | expr                    { [$1] }
  | expr "," exprs          { $1 :: $3 }
macro_params:
  | (* empty *)             { [] }
  | separated_nonempty_list(COMMA, IDENT) { $1 }
macro_body:
  | ENDM EOL                { [] }
  | program_item macro_body { $1 :: $2 }
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
