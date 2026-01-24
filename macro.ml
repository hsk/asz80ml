open Ast
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
  | String s when String.length s = 1 -> Int (Char.code s.[0])
  | String s -> String s
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
  | Sub (a, b) -> eval_bin env (fun x y -> Add (x, eval_expr env (USub y))) a b (fun x y -> x - y)
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
  | If (c, t, e) -> If (eval_expr env c, t, e)

let eval_instruction env instr =
  { instr with operand = eval_operand env instr.operand }

(* --- Macro Expansion --- *)

let rec subst_expr (subst_env: (string * expr) list) expr =
  match expr with
  | Var s -> List.assoc_opt s subst_env |> Option.value ~default:expr
  | Int n -> Int n
  | String s -> String s
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
  | If (cond, then_block, else_block) ->
      If (subst_expr subst_env cond,
          List.map (subst_instruction subst_env) then_block,
          List.map (subst_instruction subst_env) else_block)
and subst_instruction subst_env instr =
  { instr with operand = subst_operand subst_env instr.operand }

let rec eval_program_rec (loader: string -> program) (env: env) (macros: (string * macro) list) (prog: program) : program =
  match prog with
  | [] -> []
  | {operand=Expr("include", [src])} :: rest ->
      let filename = match eval_expr env src with
                     | String x -> x
                     | _ -> failwith "error"
      in
      let included_prog = loader filename in
      let evaluated_included = eval_program_rec loader env macros included_prog in
      evaluated_included @ (eval_program_rec loader env macros rest)
  | {operand=Label name}::{operand = MacroDef (params, body)} :: rest ->
      let new_macros = (name, {params; body}) :: macros in
      eval_program_rec loader env new_macros rest
  | {operand=Label n}::{operand=Expr("equ", [e])}::rest ->
      let new_env = (n, eval_expr env e) :: env in
      eval_program_rec loader new_env macros rest
  | {operand=If (cond, then_block, else_block)} :: rest ->
      let branch = match eval_expr env cond with
        | Int 0 -> else_block
        | _ -> then_block
      in
      eval_program_rec loader env macros (branch @ rest)
  | ({location; operand = Expr(name, args)} as instr) :: rest ->
      (match List.assoc_opt name macros with
      | Some macro ->
          if List.length macro.params <> List.length args then
            failwith (Printf.sprintf "Macro '%s' at %s:%d expects %d arguments, but got %d"
              name location.file location.line (List.length macro.params) (List.length args));
          let subst_env = List.combine macro.params args in
          let expanded_body = List.map (subst_instruction subst_env) macro.body in
          let processed_body = eval_program_rec loader env macros expanded_body in
          processed_body @ (eval_program_rec loader env macros rest)
      | None ->
          eval_instruction env instr :: eval_program_rec loader env macros rest)
  | instr :: rest ->
      eval_instruction env instr :: eval_program_rec loader env macros rest

let eval_program loader env prog =
  eval_program_rec loader env [] prog
