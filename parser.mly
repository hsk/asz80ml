%{
  open Ast
  let line = ref 0
  let file = ref ""
%}

/* Token declarations */
%token <int> INT
%token <string> IDENT
%token LPAREN RPAREN
%token QUESTION COLON
%token PIPE CARET AMPERSAND
%token EQUAL_EQUAL EQUAL NOT_EQUAL
%token LESS_EQUAL GREATER_EQUAL LESS GREATER
%token LSHIFT RSHIFT
%token PLUS MINUS
%token STAR SLASH PERCENT
%token TILDE
%token COMMA
%token EOF
%token NEWLINE

/* Operator precedence and associativity - lowest to highest */
%left QUESTION COLON
%left PIPE
%left CARET
%left AMPERSAND
%left EQUAL_EQUAL EQUAL NOT_EQUAL
%left LESS_EQUAL GREATER_EQUAL LESS GREATER
%left LSHIFT RSHIFT
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNARY_MINUS UNARY_PLUS TILDE

%start <Ast.program> main

%%

main:
  | program EOF
      { $1 }
  | EOF
      { [] }

program:
  | program_item
      { $1 }
  | program program_item
      { $1 @ $2 }

program_item:
  | IDENT COLON NEWLINE
      { [{ location = {file = !file; line = !line}; operand = Ast.Label $1 }] }
  | instruction
      { [$1] }
  | IDENT COLON instruction
      { [{ location = {file = !file; line = !line}; operand = Ast.Label $1 }; $3] }

instruction:
  | IDENT expr_list NEWLINE
      { { location = {file = !file; line = !line}; operand = Ast.Expr ($1, $2) } }
  | IDENT NEWLINE
      { { location = {file = !file; line = !line}; operand = Ast.Expr ($1, []) } }

expr_list:
  | expr
      { [$1] }
  | expr_list COMMA expr
      { $1 @ [$3] }

expr:
  | primary
      { $1 }
  | expr QUESTION expr COLON expr
      { Ternary($1, $3, $5) }
  | expr PIPE expr
      { BitwiseOr($1, $3) }
  | expr CARET expr
      { BitwiseXor($1, $3) }
  | expr AMPERSAND expr
      { BitwiseAnd($1, $3) }
  | expr EQUAL_EQUAL expr
      { Equal($1, $3) }
  | expr EQUAL expr
      { Equal($1, $3) }
  | expr NOT_EQUAL expr
      { NotEqual($1, $3) }
  | expr LESS_EQUAL expr
      { LessEqual($1, $3) }
  | expr GREATER_EQUAL expr
      { GreaterEqual($1, $3) }
  | expr LESS expr
      { Less($1, $3) }
  | expr GREATER expr
      { Greater($1, $3) }
  | expr LSHIFT expr
      { LeftShift($1, $3) }
  | expr RSHIFT expr
      { RightShift($1, $3) }
  | expr PLUS expr
      { Add($1, $3) }
  | expr MINUS expr
      { Sub($1, $3) }
  | expr STAR expr
      { Mul($1, $3) }
  | expr SLASH expr
      { Div($1, $3) }
  | expr PERCENT expr
      { Mod($1, $3) }
  | TILDE expr %prec UNARY_MINUS
      { BitwiseNot($2) }
  | PLUS expr %prec UNARY_PLUS
      { UnaryPlus($2) }
  | MINUS expr %prec UNARY_MINUS
      { UnaryMinus($2) }

primary:
  | INT
      { Const($1) }
  | IDENT
      { Var($1) }
  | LPAREN expr RPAREN
      { Paren($2) }
