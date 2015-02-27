let rec index_of_end_paren exp index depth =
    if depth = 0 then index
    else if index < String.length exp then begin
        match exp.[index] with
        | '(' -> index_of_end_paren exp (index + 1) (depth + 1)
        | ')' -> index_of_end_paren exp (index + 1) (depth - 1)
        | _ -> index_of_end_paren exp (index + 1) depth
    end else -1

let grab_an_exp exp i =
    index_of_end_paren exp i 1

let grab_a_lit exp i =
    try String.index_from exp i ' '
    with Not_found -> String.length exp

let grab_which exp =
    match exp.[0] with
    | '(' -> grab_an_exp
    | _ -> grab_a_lit

let grab_next exp =
    let exp = String.trim exp in
    let rec grab_next_helper f e i a =
        if i < String.length e
        then List.rev a
        else let cdr_index = f e i in
        let car = String.trim (String.sub e i cdr_index) in
        grab_next_helper f e (cdr_index + 1) (car :: a) in
    let f = grab_which exp in
    let res = grab_next_helper f exp 0 [] in
    print_endline (String.concat " " res);
    res

let is_exp exp = (exp.[0] = '(')

let apply car (cdr : string list) =
    match car with
    | "+" -> string_of_int (List.fold_left (+) 0 (List.map int_of_string cdr))
    | _ -> car

let rec enter_exp car =
    let car = String.trim car in
    let exp = (String.sub car 1 (String.length car - 2)) in
    eval exp

and eval exp =
    match grab_next exp with
    | [] -> ""
    | car :: cdr -> let car =
        if is_exp car
        then enter_exp car
        else car in
    apply car cdr

let test = "+ 1 1"

let () = 
    print_endline test;
    print_endline (eval test)
