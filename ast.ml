(* Expressions AST *)

type expr =
  (* Base cases *)
  | Const of int
  | Var of string
  
  (* Operators in order of precedence (lowest to highest) *)
  
  (* Lowest: Ternary conditional *)
  | Ternary of expr * expr * expr  (* a ? b : c *)
  
  (* Bitwise or *)
  | BitwiseOr of expr * expr  (* a | b *)
  
  (* Bitwise xor *)
  | BitwiseXor of expr * expr  (* a ^ b *)
  
  (* Bitwise and *)
  | BitwiseAnd of expr * expr  (* a & b *)
  
  (* Equality *)
  | Equal of expr * expr  (* a == b or a = b *)
  | NotEqual of expr * expr  (* a != b *)
  
  (* Inequality *)
  | LessEqual of expr * expr  (* a <= b *)
  | GreaterEqual of expr * expr  (* a >= b *)
  | Less of expr * expr  (* a < b *)
  | Greater of expr * expr  (* a > b *)
  
  (* Bit shift *)
  | LeftShift of expr * expr  (* a << b *)
  | RightShift of expr * expr  (* a >> b *)
  
  (* Addition and subtraction *)
  | Add of expr * expr  (* a + b *)
  | Sub of expr * expr  (* a - b *)
  
  (* Multiplication, division and modulo *)
  | Mul of expr * expr  (* a * b *)
  | Div of expr * expr  (* a / b *)
  | Mod of expr * expr  (* a % b *)
  
  (* Highest: Unary operators *)
  | BitwiseNot of expr  (* ~a *)
  | UnaryPlus of expr  (* +a *)
  | UnaryMinus of expr  (* -a *)
  
  (* Parentheses *)
  | Paren of expr  (* (a) *)

let rec show_expr = function
  | Const n -> string_of_int n
  | Var s -> s
  | Ternary (a, b, c) -> 
      Printf.sprintf "%s ? %s : %s" (show_expr a) (show_expr b) (show_expr c)
  | BitwiseOr (a, b) -> Printf.sprintf "%s | %s" (show_expr a) (show_expr b)
  | BitwiseXor (a, b) -> Printf.sprintf "%s ^ %s" (show_expr a) (show_expr b)
  | BitwiseAnd (a, b) -> Printf.sprintf "%s & %s" (show_expr a) (show_expr b)
  | Equal (a, b) -> Printf.sprintf "%s == %s" (show_expr a) (show_expr b)
  | NotEqual (a, b) -> Printf.sprintf "%s != %s" (show_expr a) (show_expr b)
  | LessEqual (a, b) -> Printf.sprintf "%s <= %s" (show_expr a) (show_expr b)
  | GreaterEqual (a, b) -> Printf.sprintf "%s >= %s" (show_expr a) (show_expr b)
  | Less (a, b) -> Printf.sprintf "%s < %s" (show_expr a) (show_expr b)
  | Greater (a, b) -> Printf.sprintf "%s > %s" (show_expr a) (show_expr b)
  | LeftShift (a, b) -> Printf.sprintf "%s << %s" (show_expr a) (show_expr b)
  | RightShift (a, b) -> Printf.sprintf "%s >> %s" (show_expr a) (show_expr b)
  | Add (a, b) -> Printf.sprintf "%s + %s" (show_expr a) (show_expr b)
  | Sub (a, b) -> Printf.sprintf "%s - %s" (show_expr a) (show_expr b)
  | Mul (a, b) -> Printf.sprintf "%s * %s" (show_expr a) (show_expr b)
  | Div (a, b) -> Printf.sprintf "%s / %s" (show_expr a) (show_expr b)
  | Mod (a, b) -> Printf.sprintf "%s %% %s" (show_expr a) (show_expr b)
  | BitwiseNot a -> Printf.sprintf "~%s" (show_expr a)
  | UnaryPlus a -> Printf.sprintf "+%s" (show_expr a)
  | UnaryMinus a -> Printf.sprintf "-%s" (show_expr a)
  | Paren a -> Printf.sprintf "(%s)" (show_expr a)

(* Environment type: maps variable names to integer values *)
type env = (string * int) list

(* Evaluator with environment *)
let rec eval_expr env = function
  | Const n -> Const n
  | Var s -> 
      (try
        Const (List.assoc s env)
      with Not_found -> 
        Var s)  (* Variable not found, keep it as is *)
  
  | Ternary (a, b, c) -> 
      let ea = eval_expr env a in
      let eb = eval_expr env b in
      let ec = eval_expr env c in
      (match ea with
      | Const 0 -> ec
      | Const _ -> eb
      | _ -> Ternary (ea, eb, ec))
  
  | BitwiseOr (a, b) -> eval_binop env (fun x y -> BitwiseOr (x, y)) a b (fun x y -> x lor y)
  | BitwiseXor (a, b) -> eval_binop env (fun x y -> BitwiseXor (x, y)) a b (fun x y -> x lxor y)
  | BitwiseAnd (a, b) -> eval_binop env (fun x y -> BitwiseAnd (x, y)) a b (fun x y -> x land y)
  
  | Equal (a, b) -> eval_cmp env (fun x y -> Equal (x, y)) a b (fun x y -> if x = y then 1 else 0)
  | NotEqual (a, b) -> eval_cmp env (fun x y -> NotEqual (x, y)) a b (fun x y -> if x <> y then 1 else 0)
  | LessEqual (a, b) -> eval_cmp env (fun x y -> LessEqual (x, y)) a b (fun x y -> if x <= y then 1 else 0)
  | GreaterEqual (a, b) -> eval_cmp env (fun x y -> GreaterEqual (x, y)) a b (fun x y -> if x >= y then 1 else 0)
  | Less (a, b) -> eval_cmp env (fun x y -> Less (x, y)) a b (fun x y -> if x < y then 1 else 0)
  | Greater (a, b) -> eval_cmp env (fun x y -> Greater (x, y)) a b (fun x y -> if x > y then 1 else 0)
  
  | LeftShift (a, b) -> eval_binop env (fun x y -> LeftShift (x, y)) a b (fun x y -> x lsl y)
  | RightShift (a, b) -> eval_binop env (fun x y -> RightShift (x, y)) a b (fun x y -> x asr y)
  
  | Add (a, b) -> eval_binop env (fun x y -> Add (x, y)) a b (fun x y -> x + y)
  | Sub (a, b) -> eval_binop env (fun x y -> Sub (x, y)) a b (fun x y -> x - y)
  
  | Mul (a, b) -> eval_binop env (fun x y -> Mul (x, y)) a b (fun x y -> x * y)
  | Div (a, b) -> eval_binop env (fun x y -> Div (x, y)) a b (fun x y -> if y = 0 then 0 else x / y)
  | Mod (a, b) -> eval_binop env (fun x y -> Mod (x, y)) a b (fun x y -> if y = 0 then 0 else x mod y)
  
  | BitwiseNot a -> 
      let ea = eval_expr env a in
      (match ea with
      | Const n -> Const (lnot n)
      | _ -> BitwiseNot ea)
  
  | UnaryPlus a -> 
      let ea = eval_expr env a in
      (match ea with
      | Const n -> Const n
      | _ -> UnaryPlus ea)
  
  | UnaryMinus a -> 
      let ea = eval_expr env a in
      (match ea with
      | Const n -> Const (-n)
      | _ -> UnaryMinus ea)
  
  | Paren a ->
      let ea = eval_expr env a in
      (match ea with
      | Const _ -> ea  (* If result is just a constant, remove parentheses *)
      | _ -> Paren ea)  (* Otherwise keep parentheses *)

(* Helper function for binary operations *)
and eval_binop env construct a b op =
  let ea = eval_expr env a in
  let eb = eval_expr env b in
  match ea, eb with
  | Const x, Const y -> Const (op x y)
  | _ -> construct ea eb

(* Helper function for comparison operations *)
and eval_cmp env construct a b op =
  let ea = eval_expr env a in
  let eb = eval_expr env b in
  match ea, eb with
  | Const x, Const y -> Const (op x y)
  | _ -> construct ea eb
type location = {
  file: string;
  line: int;
}
type operand =
  | Expr of string * expr list
  | Label of string
type instruction = {
  location: location;
  operand: operand;
}

(* Program type: list of instructions *)
type program = instruction list

(* String representation for operands *)
let show_operand = function
  | Expr (mnem, args) ->
      let arg_strs = List.map show_expr args in
      let args_str = String.concat "," arg_strs in
      if args_str = "" then
        mnem
      else
        Printf.sprintf "%s %s" mnem args_str
  | Label l -> l ^ ":"

(* String representation for instructions *)
let show_instruction instr =
  let op_str = show_operand instr.operand in
  Printf.sprintf "[%s:%d] %s" instr.location.file instr.location.line op_str

(* String representation for programs *)
let show_program prog =
  String.concat "\n" (List.map show_instruction prog)

(* Evaluate operand with environment *)
let eval_operand env = function
  | Expr (mnem, args) -> Expr (mnem, List.map (eval_expr env) args)
  | Label l -> Label l

(* Evaluate instruction with environment *)
let eval_instruction env instr =
  { instr with operand = eval_operand env instr.operand }

(* Evaluate program with environment *)
let eval_program env prog =
  List.map (eval_instruction env) prog
