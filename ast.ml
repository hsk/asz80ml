type expr =
  | Int of int
  | Var of string
  | Ternary of expr * expr * expr  (* a ? b : c *)
  | Or of expr * expr  (* a | b *)
  | Xor of expr * expr  (* a ^ b *)
  | And of expr * expr  (* a & b *)
  | Eq of expr * expr  (* a == b or a = b *)
  | Ne of expr * expr  (* a != b *)
  | Le of expr * expr  (* a <= b *)
  | Ge of expr * expr  (* a >= b *)
  | Lt of expr * expr  (* a < b *)
  | Gt of expr * expr  (* a > b *)
  | LShift of expr * expr  (* a << b *)
  | RShift of expr * expr  (* a >> b *)
  | Add of expr * expr  (* a + b *)
  | Sub of expr * expr  (* a - b *)
  | Mul of expr * expr  (* a * b *)
  | Div of expr * expr  (* a / b *)
  | Mod of expr * expr  (* a % b *)
  | Not of expr  (* ~a *)
  | UAdd of expr  (* +a *)
  | USub of expr  (* -a *)
  | Paren of expr  (* (a) *)

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

type program = instruction list

let rec show_expr = function
  | Int n -> string_of_int n
  | Var s -> s
  | Ternary (a, b, c) -> 
      Printf.sprintf "%s ? %s : %s" (show_expr a) (show_expr b) (show_expr c)
  | Or (a, b) -> Printf.sprintf "%s | %s" (show_expr a) (show_expr b)
  | Xor (a, b) -> Printf.sprintf "%s ^ %s" (show_expr a) (show_expr b)
  | And (a, b) -> Printf.sprintf "%s & %s" (show_expr a) (show_expr b)
  | Eq (a, b) -> Printf.sprintf "%s == %s" (show_expr a) (show_expr b)
  | Ne (a, b) -> Printf.sprintf "%s != %s" (show_expr a) (show_expr b)
  | Le (a, b) -> Printf.sprintf "%s <= %s" (show_expr a) (show_expr b)
  | Ge (a, b) -> Printf.sprintf "%s >= %s" (show_expr a) (show_expr b)
  | Lt (a, b) -> Printf.sprintf "%s < %s" (show_expr a) (show_expr b)
  | Gt (a, b) -> Printf.sprintf "%s > %s" (show_expr a) (show_expr b)
  | LShift (a, b) -> Printf.sprintf "%s << %s" (show_expr a) (show_expr b)
  | RShift (a, b) -> Printf.sprintf "%s >> %s" (show_expr a) (show_expr b)
  | Add (a, b) -> Printf.sprintf "%s + %s" (show_expr a) (show_expr b)
  | Sub (a, b) -> Printf.sprintf "%s - %s" (show_expr a) (show_expr b)
  | Mul (a, b) -> Printf.sprintf "%s * %s" (show_expr a) (show_expr b)
  | Div (a, b) -> Printf.sprintf "%s / %s" (show_expr a) (show_expr b)
  | Mod (a, b) -> Printf.sprintf "%s %% %s" (show_expr a) (show_expr b)
  | Not a -> Printf.sprintf "~%s" (show_expr a)
  | UAdd a -> Printf.sprintf "+%s" (show_expr a)
  | USub a -> Printf.sprintf "-%s" (show_expr a)
  | Paren a -> Printf.sprintf "(%s)" (show_expr a)

let show_operand = function
  | Expr (mnem, args) ->
      let arg_strs = List.map show_expr args in
      let args_str = String.concat "," arg_strs in
      if args_str = "" then mnem else
      Printf.sprintf "%s %s" mnem args_str
  | Label l -> l ^ ":"

let show_instruction instr =
  let op_str = show_operand instr.operand in
  Printf.sprintf "[%s:%d] %s" instr.location.file instr.location.line op_str

let show_program prog =
  String.concat "\n" (List.map show_instruction prog)

type env = (string * expr) list

let rec eval_expr env = function
  | Int n -> Int n
  | Var s -> 
      begin match List.assoc_opt s env with
      | None -> Var s
      | Some e -> e
      end
  | Or (a, b) -> eval_bin env (fun x y -> Or (x, y)) a b (fun x y -> x lor y)
  | Xor (a, b) -> eval_bin env (fun x y -> Xor (x, y)) a b (fun x y -> x lxor y)
  | And (a, b) -> eval_bin env (fun x y -> And (x, y)) a b (fun x y -> x land y)
  | Eq (a, b) -> eval_bin env (fun x y -> Eq (x, y)) a b (fun x y -> if x = y then 1 else 0)
  | Ne (a, b) -> eval_bin env (fun x y -> Ne (x, y)) a b (fun x y -> if x <> y then 1 else 0)
  | Le (a, b) -> eval_bin env (fun x y -> Le (x, y)) a b (fun x y -> if x <= y then 1 else 0)
  | Ge (a, b) -> eval_bin env (fun x y -> Ge (x, y)) a b (fun x y -> if x >= y then 1 else 0)
  | Lt (a, b) -> eval_bin env (fun x y -> Lt (x, y)) a b (fun x y -> if x < y then 1 else 0)
  | Gt (a, b) -> eval_bin env (fun x y -> Gt (x, y)) a b (fun x y -> if x > y then 1 else 0)
  | LShift (a, b) -> eval_bin env (fun x y -> LShift (x, y)) a b (fun x y -> x lsl y)
  | RShift (a, b) -> eval_bin env (fun x y -> RShift (x, y)) a b (fun x y -> x asr y)
  | Add (a, b) -> eval_bin env (fun x y -> Add (x, y)) a b (fun x y -> x + y)
  | Sub (a, b) -> eval_bin env (fun x y -> Sub (x, y)) a b (fun x y -> x - y)
  | Mul (a, b) -> eval_bin env (fun x y -> Mul (x, y)) a b (fun x y -> x * y)
  | Div (a, b) -> eval_bin env (fun x y -> Div (x, y)) a b (fun x y -> if y = 0 then 0 else x / y)
  | Mod (a, b) -> eval_bin env (fun x y -> Mod (x, y)) a b (fun x y -> if y = 0 then 0 else x mod y)
  | Not a -> eval_un env (fun x -> Not x) a lnot
  | UAdd a -> eval_un env (fun x -> UAdd x) a (fun x -> x)
  | USub a -> eval_un env (fun x -> USub x) a (fun x -> -x)
  | Paren a -> eval_un env (fun x -> Paren x) a (fun x -> x)
  | Ternary (a, b, c) -> 
      let ea = eval_expr env a in
      let eb = eval_expr env b in
      let ec = eval_expr env c in
      match ea with
      | Int 0 -> ec
      | Int _ -> eb
      | _ -> Ternary (ea, eb, ec)
and eval_bin env f a b op =
  let ea = eval_expr env a in
  let eb = eval_expr env b in
  match ea, eb with
  | Int x, Int y -> Int (op x y)
  | _ -> f ea eb
and eval_un env f a op =
  let ea = eval_expr env a in
  (match ea with
  | Int n -> Int (op n)
  | _ -> f ea)

let eval_expr env = function
  | Paren e -> Paren (eval_expr env e)
  | e -> eval_expr env e

let eval_operand env = function
  | Expr (mnem, args) -> Expr (mnem, List.map (eval_expr env) args)
  | Label l -> Label l

let eval_instruction env instr =
  { instr with operand = eval_operand env instr.operand }

let rec eval_program env = function
  | [] -> []
  | {operand=Label(n)}::{operand=Expr("equ", [e])}::prog ->
    eval_program ((n,eval_expr env e)::env) prog
  | instr::prog -> eval_instruction env instr::eval_program env prog

