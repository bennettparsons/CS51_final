
(* Abstraction for unit tests *)

type test =
  {label: string; content: bool Lazy.t; time: int; fail_msg: string
  } ;;

type status =   
  | Passed   
  | Failed of string
  | Raised_exn of string
  | Timed_out of int ;;

exception Timeout ;;


let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout (time : int) (delayed : 'a Lazy.t) : 'a =
  let old_behavior =
    Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () =
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_behavior in
  ignore (Unix.alarm time) ;
  let res = Lazy.force delayed in
  reset_sigalrm () ; res ;;


let run_test ({label; content; time; fail_msg} : test) 
  (continue : string -> status -> unit): unit = 
    try
      if timeout time content
      then continue label Passed       
      else continue label (Failed fail_msg)
    with 
    | Timeout -> continue label (Timed_out time)
    | exn     -> continue label
                   (Raised_exn (Printexc.to_string exn)) ;;

open Printf;;

let present label status =
  match status with
  | Passed -> printf "%s: passed\n" label
  | Failed msg -> printf "%s: failed %s\n" label msg
  | Timed_out secs -> printf "%s: timed out in %d\n" label secs
  | Raised_exn msg -> printf "%s: raised %s\n" label msg ;;

let report (tests: test list) : unit = 
  List.iter (fun test -> run_test test present) tests ;;

let make_test contents = 
  List.map (fun (label,test,message) -> 
   {label = label; content = test; time = 3; fail_msg = message}) contents
;;

(* TESTING ZONE *)

open Miniml ;;
open Expr ;;
open Evaluation ;;
open Env ;;

let tests =
  let empty = create () in
  let x = Var "x" in
  let y = Var "y" in
  let f = Var "f" in
  let n1 = Num 1 in
  let n2 = Num 2 in
  let b1 = Binop("+", x, n1) in
  let b2 = Binop("+", x, y) in
  let v1 = Val (Num 3) in
  let v2 = Val (Num 4) in
  let v3 = Val (Binop("+", x, y)) in
  let vr1 = ref v1 in
  let vr2 = ref v2 in
  let vr3 = ref v3 in
  let f1 = Fun ("x", b2) in
  
(* Helper function tests *)
  let test1 = 
    let str = env_to_string empty in
    "Empty", lazy (str = ""), str in
   
  let env = extend empty "x" vr1 in
  let test2 = 
    let str = env_to_string env in
    "Extend1", lazy (str =  "x==>Val: 3  "), str in

  let env = extend env "x" vr2 in
  let test3 =
    let str = env_to_string env in
    "Extend2", lazy (str =  "x==>Val: 4  "), str in

  let env = extend env "y" vr1 in
  let test4 = 
    let str = env_to_string env in
    "Extend3", lazy (str =  "x==>Val: 4  y==>Val: 3  "), str in
  
  let env = extend env "y" vr2 in
  let test5 =
    let str = env_to_string env in
    "Extend4", lazy (str =  "x==>Val: 4  y==>Val: 4  "), str in

  let env_close1 = extend env "x" (ref (close f1 env)) in
  let test6 = 
    let str = env_to_string env_close1 in
  	"Close1", lazy (str =  
  	"x==>Closure: ((fun x -> (x) + (y)) in env: (x==>Val: 4  y==>Val: 4  ))  y==>Val: 4  "),
  	str in

  let env_close2 = extend env "x" (ref (close f1 empty)) in
  let test7 = 
    let str = env_to_string env_close2 in
  	"Close1", lazy (str =  
  	"x==>Closure: ((fun x -> (x) + (y)) in env: ())  y==>Val: 4  "),
  	str in
 
  let test8 =
    let str = value_to_string (lookup env "y") in
    "Lookup1", lazy (str =  "Val: 4"), str in

  let test9 =
    let str = value_to_string (lookup env_close1 "x") in
    "Lookup2", lazy (str = 
    "Closure: ((fun x -> (x) + (y)) in env: (x==>Val: 4  y==>Val: 4  ))"), 
    str in

  let test10 =
    let tester =
      try (lookup env "f") = v1 with
      | EvalError _ -> true
      | _ -> false in
    "Lookup3", lazy (tester), "No exception thrown" in

  let subst_tests =

    (* helpers for automatically naming tests *)
    let ctr = ref "7" in
    let dec () = ctr := string_of_int (int_of_string (!ctr) - 1) in
    
    (* creates tests for subst by using it as an evaluator *)
    let make_sub_test sub_evaler str_in str_out =
      dec ();
      let str () =
        try
          exp_to_string (sub_evaler (str_to_exp str_in))
        with
        | EvalError s -> s in

      ["subst" ^ !ctr, lazy ((str ()) = str_out), 
      	"|" ^ (str ()) ^ "|" ^ " = " ^ "|" ^ str_out ^ "|"] in

  (* Note: the following two tests fail because of new_varname generator; 
   * however, in viewing the printed results, one can see the expected result
   * is semantically equivalent to the actual result *)
  (make_sub_test (subst "x" (Var "y")) "fun y -> x;;" "(fun var1 -> y)") @
  (make_sub_test (subst "y" (Var "x")) "let x = y * y in x + y;;" 
  	"(let var0 = (x) * (x) in ((var0) + (x)))") @
  (make_sub_test (subst "y" (Num 3)) "let x = y * y in x + y;;" 
  	"(let x = (3) * (3) in ((x) + (3)))") @
  (make_sub_test (subst "x" (Bool false)) "(fun x -> x 3) x;;" 
  	"((fun x -> (x 3)) false)") @
  (make_sub_test (subst "y" (Fun("z", Var "z"))) "let x = y in fun y -> x;;" 
  	"(let x = (fun z -> z) in ((fun y -> x)))") @
  (make_sub_test (subst "x" (Num 3)) "(fun y -> fun x -> y (y x)) (fun x -> x * 2) x;;" 
  	"(((fun y -> (fun x -> (y (y x)))) (fun x -> (x) * (2))) 3)") in


(* Tests that hold for all evaluators *)
  let eval_tests = 

    (* new helpers for new naming set *)
    let ctr = ref "16" in
    let dec () = ctr := string_of_int (int_of_string (!ctr) - 1) in

    (* function that tests evaluation for eval_s, eval_d and eval_l *)
    let test_all str_in str_out =
      dec ();
      let str evaler =
        try
          exp_to_string (exp_of_val (evaler (str_to_exp str_in) empty))
        with
        | EvalError s -> s in

      ["Eval_s" ^ !ctr, lazy (str eval_s = str_out), str eval_s ;
      "Eval_d" ^ !ctr, lazy (str eval_d = str_out), str eval_d ;
      "Eval_d_ext" ^ !ctr, lazy (str eval_d_ext = str_out), str eval_d_ext ;
      "Eval_l" ^ !ctr, lazy (str eval_l = str_out), str eval_l] in

    (test_all "4 + 4;;" "8") @ 
    (test_all "4 <> 4;;" "false") @ 
    (test_all "~2;;" "-2") @ 
    (test_all "not true;;" "false") @ 
    (test_all "not false;;" "true") @ 
    (test_all "5 - 2;;" "3") @
    (test_all "10 / 3;;" "3") @
    (test_all "let x = 2 in x;;" "2") @
    (test_all "(fun x -> x * 2) 4;;" "8") @
    (test_all "let square = fun x -> x * x in let y = 3 in square y;;" "9") @
    (test_all 
      "let id = fun x -> x in let square = fun x -> x * x in let y = 3 in id square y;;" 
      "9") @
    (test_all 
      "let rec f = fun x -> if x = 0 then 1 else x * f (x -1 ) in f 5;;" 
      "120") @
    (test_all "let rec f = fun x -> if x = 0 then x else f (x - 1) in f 2 ;;" 
      "0") @
    (test_all "let x = y in x;;" "Unbound value y") @
    (test_all "true <> 3;;" 
      "This operator can only be applied to two Nums or two Bools") in


  (* tests which differ among the evaluators *)
  let eval_tests2 = 

  	(* new helpers for new naming set *)
    let ctr = ref "21" in
    let dec () = ctr := string_of_int (int_of_string (!ctr) - 1) in

    let test_one name evaler str_in str_out =
      let str () =
        try
          exp_to_string (exp_of_val (evaler (str_to_exp str_in) empty))
        with
        | EvalError s -> s in

      [name ^ !ctr, lazy ((str ()) = str_out), str ()] in

    (test_one "Eval_s" eval_s 
    	"let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4;;"
      "16") @
    (test_one "Eval_d" eval_d 
    	"let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4;;"
      "Unbound value y") @
    (test_one "Eval_d_ext" eval_d_ext 
    	"let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4;;"
      "16") @
    (dec (); test_one "Eval_s" eval_s 
    	"let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4;;"
      "16") @

    (test_one "Eval_s" eval_s "let f = fun x -> fun y -> x in f 5 10;;" "5") @
    (test_one "Eval_d" eval_d "let f = fun x -> fun y -> x in f 5 10;;" 
      "Unbound value x") @
    (test_one "Eval_d_ext" eval_d_ext 
      "let f = fun x -> fun y -> x in f 5 10;;" "5") @
    (dec (); test_one "Eval_s" eval_s 
      "let f = fun x -> fun y -> x in f 5 10;;" "5") @

    (test_one "Eval_s" eval_s "let f = fun x -> y + y in let y = 4 in f y ;;" 
      "Unbound value y") @
    (test_one "Eval_d" eval_d "let f = fun x -> y + y in let y = 4 in f y ;;" 
      "8") @
    (test_one "Eval_d_ext" eval_d_ext 
      "let f = fun x -> y + y in let y = 4 in f y ;;" "8") @
    (dec (); 
      test_one "Eval_l" eval_l "let f = fun x -> y + y in let y = 4 in f y ;;" 
      "Unbound value y") @

    (test_one "Eval_s" eval_s 
      "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;" "4") @
    (test_one "Eval_d" eval_d 
      "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;" "5") @
    (test_one "Eval_d_ext" eval_d_ext 
      "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;" "5") @
    (dec (); test_one "Eval_l" eval_l 
      "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;" "4") @

    (test_one "Eval_s" eval_s 
      "let f = fun x -> twice x in let twice = fun x -> x * 2 in f 5;;" 
      "Unbound value twice") @
    (test_one "Eval_d" eval_d 
      "let f = fun x -> twice x in let twice = fun x -> x * 2 in f 5;;" "10") @
    (test_one "Eval_d_ext" eval_d_ext
      "let f = fun x -> twice x in let twice = fun x -> x * 2 in f 5;;" "10") @
    (dec (); test_one "Eval_l" eval_l 
      "let f = fun x -> twice x in let twice = fun x -> x * 2 in f 5;;" 
      "Unbound value twice") in

    (* list of all gathered tests *)
    [test1; test2 ; test3; test4; test5; test6; test7; test8; test9; test10] 
      @ subst_tests @ eval_tests @ eval_tests2
   ;;

report (make_test tests) ;;












