open Olisp

let tests = [
    ("(+ 1 (+ 1 3))", "5");
    ("(- 3 2)", "1");
    ("(/ (- 8 2) 3)", "2");
    ("(* 9 (+ 3 5))", "72");
    ("(+ (if (< 2 3) 1 2) (if (> 4 5) 2 9) 72)", "82");
    ("(+ 1 (let (a 1)) (+ a 2))", "5")
]

let rec run_tests tests =
    match tests with
    | [] -> ()
    | ((test, answer) :: rem_tests) ->
        print_endline test;
        print_endline ((eval test) ^ " === " ^ answer ^ "\n");
        run_tests rem_tests;;

run_tests tests
