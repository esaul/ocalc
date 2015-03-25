let rec index_of_end_paren exp index depth =
    if depth = 0 then index
    else if index < String.length exp then
        match exp.[index] with
        | '(' -> index_of_end_paren exp (index + 1) (depth + 1)
        | ')' -> index_of_end_paren exp (index + 1) (depth - 1)
        | _ -> index_of_end_paren exp (index + 1) depth
    else -1

let is_exp exp = (exp.[0] = '(')

let grab_inner_exp exp =
    let e = String.trim exp in
    String.sub e 1 (String.length e - 2)

let split_exp e i =
    let cdr =
        try String.sub e i (String.length e - i)
        with _ -> "" in
    let car =
        try String.sub e 0 i
        with _ -> "" in
    (String.trim car, String.trim cdr)

let separate_lit exp =
    try let i = (String.index exp ' ') + 1 in
        split_exp exp i
    with _ -> (exp, "")

let separate_exp exp =
    let i = index_of_end_paren exp 1 1 in
    split_exp exp i

let int_op_n op args =
    let i_args = (List.map int_of_string args) in
    match i_args with
    | (car :: cdr) -> string_of_int (List.fold_left op car cdr)
    | _ -> print_endline "Runtime error."; exit 0

let apply car (cdr : string list) =
    match car with
    | "+" -> int_op_n ( + ) cdr
    | "-" -> int_op_n ( - ) cdr
    | "*" -> int_op_n ( * ) cdr
    | "/" -> int_op_n ( / ) cdr
    | _ -> car

let rec grab_exp exp =
    let inner_exp = grab_inner_exp exp in
    let rec helper exp args =
        match exp with
        | "" -> List.rev args
        | _ -> let (car, cdr) =
            if is_exp exp
            then let (car, cdr) = separate_exp exp in (eval car, cdr)
            else separate_lit exp in
        helper cdr (car :: args) in
    helper inner_exp []

and eval exp =
    if is_exp exp then
        match grab_exp exp with
        | [] -> ""
        | car :: cdr -> apply car cdr
    else exp

let tests = [
    ("(+ 1 (+ 1 3))", "5");
    ("(- 3 2)", "1");
    ("(/ (- 8 2) 3)", "2");
    ("(* 9 (+ 3 5))", "72")
]

let rec run_tests tests = 
    match tests with
    | [] -> ()
    | ((test, answer) :: rem_tests) ->
        print_endline test;
        print_endline ((eval test) ^ " === " ^ answer ^ "\n");
        run_tests rem_tests;;
run_tests tests
