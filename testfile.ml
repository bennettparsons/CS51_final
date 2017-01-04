(* open Expr;;
open Evaluation;; *)

(* Env.run_tests () ;; *)




(* 
let test () =

assert (mem "x" (free_vars (Let ("x", Fun ("y", Var "x"), Var "x"))))

;;

test();;
print_endline "All tests passed.";;


(* CORRECT TESTS FOR subst *)


(* fresh variable *)
# subst "x" (Var "y") (Fun ("y", Var "x"));;
- : expr = Fun ("var0", Var "y")

(* fresh variable *)
# subst "y" (Var "x") ((Let ("x", Binop ("*", Var "y", Var "y"), Binop ("+", Var "x", Var "y"))));;
- : expr =
Let ("var2", Binop ("*", Var "x", Var "x"), Binop ("+", Var "var2", Var "x"))

(* others *)
# subst "y" (Num 3) (Let ("x", Binop ("*", Var "y", Var "y"), Binop ("+", Var "x", Var "y")));;
- : expr = Let ("x", Binop ("*", Num 3, Num 3), Binop ("+", Var "x", Num 3))

# subst "x" (Bool false) (Let("x", Binop ("+", Var "x", Num 3), Var "x"));;
- : expr = Let ("x", Binop ("+", Bool false, Num 3), Var "x")

# subst "x" (Bool false) (Letrec ("x", Binop ("+", Var "x", Num 3), Var "x"));;
- : expr = Letrec ("x", Binop ("+", Var "x", Num 3), Var "x")

# subst "x" (Bool true) (Fun("x", App(Num 3, Num 3)));;
- : expr = Fun ("x", App (Num 3, Num 3))


# subst "y" (Var("zZz")) ( Let("x", Var("y"), Fun("y", Var("x"))) );;
- : expr = Let ("x", Var "zZz", Fun ("y", Var "x"))


# subst "x" (Num 3) (App(App(Fun("x", Binop("*", Var("x"), Var("x"))), Fun("x", Binop("*", Var("x"), Var("x")))), Var("x")));;
- : expr =
App
 (App (Fun ("x", Binop ("*", Var "x", Var "x")),
   Fun ("x", Binop ("*", Var "x", Var "x"))),
 Num 3)


 # subst "x" (Num 3) (Fun("x", App(App(Fun("x", Binop("*", Var("x"), Var("x"))), Fun("x", Binop("*", Var("x"), Var("x")))), Var("x"))));;
- : expr =
Fun ("x",
 App
  (App (Fun ("x", Binop ("*", Var "x", Var "x")),
    Fun ("x", Binop ("*", Var "x", Var "x"))),
  Var "x"))

(* CORRECT TESTS FOR eval_s!!! *)

<== let f = fun x -> y + y in let y = 4 in f y ;;
xx> evaluation error Unbound value y
<== (fun f -> fun x -> f (f x)) (fun x -> x * x) 3 ;;
==> 81
<== let twice = fun f -> fun x -> f (f x) in let square = fun x -> x * x in twice square 3 ;;
==> 81
<== let x = y in (fun y -> x);;
xx> evaluation error Unbound value y
<== let y = 3 in let x = y in (fun y -> x);;
==> (fun y -> 3)
<== let square = fun x -> x * x in let y = 3 in square y;;
==> 9
<== let id = fun x -> x in let square = fun x -> x * x in let y = 3 in id square y;;
==> 9
<== let rec f = fun x -> if x = 0 then 1 else x * f (x -1 ) in f 5;;
==> 120
<== let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;
==> 4
<== let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4 ;;
==> 16

(* Fun("x", App(App(Fun("x", Binop("*", Var("x"), Var("x"))), Fun("x", Binop("*", Var("x"), Var("x")))), Var("x"))) *)


(* ENV FUNCTIONS *)


(* HELPERS *)



(* eval_d *)

<== (fun x -> x + x) 5;;
==> 10
<== let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;
==> 5
<== let rec f = fun x -> if x = 0 then x else f (x - 1) in f 2 ;;
==> 0
<== let rec f = fun x -> if x = 0 then 1 else x * f (x -1 ) in f 5;;
==> 120
<== let rec x = x in x;;
xx> evaluation error This kind of expression is not allowed as right-hand side of `let rec'
<== let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4 ;;
==> 16
<== let f = fun x -> twice x in let twice = fun x -> x * 2 in f 5;;
==> 10
^^^really good test; doesn't work anywhere else

CONSIDER MAKING DOUBLE FUNCTION APPLICATIONS ABLE TO TURN OFF SO AS TO CONFORM TO TRUE DYNAMIC SCOPING






(* eval_l *)

<== let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;
==> 4
<== let twice = fun y -> fun x -> y (y x) in let times2 = fun x -> x * 2 in twice times2 4 ;;
==> 16
<== let rec f = fun x -> if x = 0 then 1 else x * f (x -1 ) in f 5;;
==> 120




 *)









