(* Parsing *)
let rec index_of_end_paren exp index depth = if depth = 0 then index
    else if index < String.length exp then match exp.[index] with
        | '(' -> index_of_end_paren exp (index + 1) (depth + 1)
        | ')' -> index_of_end_paren exp (index + 1) (depth - 1)
        | _ -> index_of_end_paren exp (index + 1) depth
    else -1

let is_exp exp = (exp.[0] = '(')
let grab_inner_exp exp = let e = String.trim exp in
    String.sub e 1 (String.length e - 2)

let split_exp e i =
    let cdr = try String.sub e i (String.length e - i) with _ -> "" in
    let car = try String.sub e 0 i with _ -> "" in (String.trim car, String.trim cdr)
let separate_lit exp =
    try let i = (String.index exp ' ') + 1 in split_exp exp i with _ -> (exp, "")
let separate_exp exp = let i = index_of_end_paren exp 1 1 in split_exp exp i

(* Environment *)
module Env = Map.Make(String);;
let env = ref Env.empty

let to_exp args = ("(" ^ (String.concat " " args) ^ ")")
let rec to_olisp_val v = try int_of_string v with _ -> to_olisp_val (Env.find v !env)

let int_op_n op args = string_of_int (List.fold_left op (List.hd args) (List.tl args))
let bool_op op args = if op (List.nth args 0) (List.nth args 1) then "1" else "0"
let if_op args = string_of_int (if (List.nth args 0) = 1 then (List.nth args 1) else (List.nth args 2))
let let_op args = env := Env.add (List.hd args) (List.nth args 1) !env; List.nth args 1

let rec apply car (cdr : string list) = if car = "let"
    then let_op cdr else let cdr = (List.map to_olisp_val cdr) in
    match car with
    | "+" -> int_op_n ( + ) cdr | "-" -> int_op_n ( - ) cdr
    | "*" -> int_op_n ( * ) cdr | "/" -> int_op_n ( / ) cdr
    | "<=" -> bool_op ( <= ) cdr | ">=" -> bool_op ( >= ) cdr
    | ">" -> bool_op ( > ) cdr | "<" -> bool_op ( < ) cdr
    | "=" -> bool_op ( = ) cdr | "<>" -> bool_op ( <> ) cdr
    | "if" -> if_op cdr | _ -> print_endline "error, proc not supported"; exit 0

and grab_exp exp = let inner_exp = grab_inner_exp exp in
    let rec helper exp args = match exp with
        | "" -> List.rev args
        | _ -> let (car, cdr) = if is_exp exp
            then let (car, cdr) = separate_exp exp in (eval car, cdr)
            else separate_lit exp in
        helper cdr (car :: args) in helper inner_exp []

and eval exp = if is_exp exp then match grab_exp exp with
        | [] -> "" | car :: cdr -> apply car cdr else exp
