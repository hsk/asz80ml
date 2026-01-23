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
  | MacroDef of string list * instruction list
and instruction = {
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
  | MacroDef (params, _body) ->
      Printf.sprintf "macro %s" (String.concat ", " params)

let show_instruction instr =
  let op_str = show_operand instr.operand in
  Printf.sprintf "[%s:%d] %s" instr.location.file instr.location.line op_str

let show_program prog =
  String.concat "\n" (List.map show_instruction prog)

type macro = {
  params: string list;
  body: instruction list;
}

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
  | MacroDef (p, b) -> MacroDef (p, b)

let eval_instruction env instr =
  { instr with operand = eval_operand env instr.operand }

(* --- Macro Expansion --- *)

let rec subst_expr (subst_env: (string * expr) list) expr =
  match expr with
  | Var s -> List.assoc_opt s subst_env |> Option.value ~default:expr
  | Int n -> Int n
  | Ternary (a,b,c) -> Ternary (subst_expr subst_env a, subst_expr subst_env b, subst_expr subst_env c)
  | Or (a, b) -> Or (subst_expr subst_env a, subst_expr subst_env b)
  | Xor (a, b) -> Xor (subst_expr subst_env a, subst_expr subst_env b)
  | And (a, b) -> And (subst_expr subst_env a, subst_expr subst_env b)
  | Eq (a, b) -> Eq (subst_expr subst_env a, subst_expr subst_env b)
  | Ne (a, b) -> Ne (subst_expr subst_env a, subst_expr subst_env b)
  | Le (a, b) -> Le (subst_expr subst_env a, subst_expr subst_env b)
  | Ge (a, b) -> Ge (subst_expr subst_env a, subst_expr subst_env b)
  | Lt (a, b) -> Lt (subst_expr subst_env a, subst_expr subst_env b)
  | Gt (a, b) -> Gt (subst_expr subst_env a, subst_expr subst_env b)
  | LShift (a, b) -> LShift (subst_expr subst_env a, subst_expr subst_env b)
  | RShift (a, b) -> RShift (subst_expr subst_env a, subst_expr subst_env b)
  | Add (a, b) -> Add (subst_expr subst_env a, subst_expr subst_env b)
  | Sub (a, b) -> Sub (subst_expr subst_env a, subst_expr subst_env b)
  | Mul (a, b) -> Mul (subst_expr subst_env a, subst_expr subst_env b)
  | Div (a, b) -> Div (subst_expr subst_env a, subst_expr subst_env b)
  | Mod (a, b) -> Mod (subst_expr subst_env a, subst_expr subst_env b)
  | Not a -> Not (subst_expr subst_env a)
  | UAdd a -> UAdd (subst_expr subst_env a)
  | USub a -> USub (subst_expr subst_env a)
  | Paren a -> Paren (subst_expr subst_env a)

let rec subst_operand subst_env = function
  | Expr (mnem, args) -> Expr (mnem, List.map (subst_expr subst_env) args)
  | Label l -> Label l
  | MacroDef (params, body) ->
      let new_subst_env = List.filter (fun (p, _) -> not (List.mem p params)) subst_env in
      MacroDef (params, List.map (subst_instruction new_subst_env) body)
and subst_instruction subst_env instr =
  { instr with operand = subst_operand subst_env instr.operand }

let rec eval_program_rec (env: env) (macros: (string * macro) list) (prog: program) : program =
  match prog with
  | [] -> []
  | {operand=Label name}::{operand = MacroDef (params, body)} :: rest ->
      let new_macros = (name, {params; body}) :: macros in
      eval_program_rec env new_macros rest
  | {operand=Label n}::{operand=Expr("equ", [e])}::rest ->
      let new_env = (n, eval_expr env e) :: env in
      eval_program_rec new_env macros rest
  | ({location; operand = Expr(name, args)} as instr) :: rest ->
      (match List.assoc_opt name macros with
      | Some macro ->
          if List.length macro.params <> List.length args then
            failwith (Printf.sprintf "Macro '%s' at %s:%d expects %d arguments, but got %d"
              name location.file location.line (List.length macro.params) (List.length args));
          let subst_env = List.combine macro.params args in
          let expanded_body = List.map (subst_instruction subst_env) macro.body in
          let processed_body = eval_program_rec env macros expanded_body in
          processed_body @ (eval_program_rec env macros rest)
      | None ->
          eval_instruction env instr :: eval_program_rec env macros rest)
  | instr :: rest ->
      eval_instruction env instr :: eval_program_rec env macros rest

let eval_program env prog =
  eval_program_rec env [] prog
