let rec index_of_end_paren exp index depth =
    if depth = 0 then index
    else if index < String.length exp then begin
        match exp.[index] with
        | '(' -> index_of_end_paren exp (index + 1) (depth + 1)
        | ')' -> index_of_end_paren exp (index + 1) (depth - 1)
        | _ -> index_of_end_paren exp (index + 1) depth
    end else -1

let map fn ls =
    let rec map_helper ls nls = match ls with
    | [] -> List.rev nls
    | (hd :: rs) -> map_helper rs (fn hd :: nls) in
    map_helper ls []

let grab_an_exp exp =
    let end_paren = index_of_end_paren exp 1 1 in
    print_int end_paren;
    print_endline exp;
    let an_exp = String.trim (String.sub exp 0 end_paren) in
    let rest_end = String.length exp - end_paren - 1 in
    (an_exp, end_paren + 1, rest_end)

let grab_a_lit exp =
    let end_lit =
        try String.index exp ' '
        with Not_found -> String.length exp in
    print_int end_lit;
    print_endline exp;
    let a_lit = String.trim (String.sub exp 0 end_lit) in
    let rest_end = String.length exp - end_lit - 1 in
    (a_lit, end_lit + 1, rest_end)

let grab_next exp =
    let exp = String.trim exp in
    match exp.[0] with
    | '(' -> grab_an_exp exp
    | _ -> grab_a_lit exp

let apply car cdr =
    match car with
    | "quote" -> String.concat " " cdr
    | _ -> car

let rec eval_args args e_args =
    match args with
    | "" -> let re_args = List.rev e_args in begin
        match re_args with
        | [] -> ""
        | (car :: cdr) -> eval_exp car cdr
    end
    | exp -> let (an_exp, rest_index, rest_len) =
        grab_next exp in
    let e_exp = eval_exp an_exp [] in
    let rest =
        try String.sub exp rest_index rest_len
        with _ -> "" in
    eval_args rest (e_exp :: e_args)

and eval_exp car cdr : string =
    let car =
        if car.[0] = '('
        then enter_exp car
        else car in
    print_endline car;
    apply car cdr

and enter_exp exp =
    print_endline exp;
    let the_exp = String.sub exp 1 (String.length exp - 2) in
    let (the_exp, rest_index, rest_len) = grab_next the_exp in
    the_exp

let eval exp =
    eval_args exp []

let test = "(quote (bar (tool bar)) ((catz)) kidd)";;

print_endline test;;
eval test
