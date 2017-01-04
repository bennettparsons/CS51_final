
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;
  
type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;

open SS ;;

(* debugger *)
let print set = iter print_string set ;;
  
(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> add v empty
  | Num _i -> empty
  | Bool _b -> empty
  | Unop (_o, exp) -> free_vars exp
  | Binop (_o, exp1, exp2) -> union (free_vars exp1) (free_vars exp2)
  | Conditional (if_exp, then_exp, else_exp) -> 
    union (free_vars if_exp) (union (free_vars then_exp) 
                                          (free_vars else_exp))
  | Fun (v, exp) -> remove v (free_vars exp)
  | Let (v, def_exp, in_exp) -> 
    union (free_vars def_exp) (remove v (free_vars in_exp))
  | Letrec (v, def_exp, in_exp) -> 
    union (remove v (free_vars def_exp)) (remove v (free_vars in_exp))
  | Raise -> empty
  | Unassigned -> empty
  | App (app_exp, arg_exp) -> union (free_vars app_exp) (free_vars arg_exp) ;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname =
  let inc x = x := !x + 1 in
  let y = ref 0 in
  fun () ->
    let gensym s = 
      let temp = !y in
      inc y; s ^ (string_of_int temp) in 
    gensym "var" ;;
  

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
(* exp[var_name -> repl] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  match exp with
  | Var v -> if v = var_name then repl else exp
  | Num i -> exp
  | Bool b -> exp
  | Unop (o, exp) -> 
    Unop (o, subst var_name repl exp)
  | Binop (o, exp1, exp2) -> 
    Binop (o, subst var_name repl exp1, subst var_name repl exp2)
  | Conditional (if_exp, then_exp, else_exp) -> 
    Conditional (subst var_name repl if_exp, subst var_name repl then_exp, 
      subst var_name repl else_exp)
  | Fun (v, exp1) -> 
    if v = var_name then exp
    else if mem v (free_vars repl)
    then let z = new_varname () in
      Fun (z, subst var_name repl (subst v (Var z) exp1))
    else Fun (v, subst var_name repl exp1)
  | Let (v, def_exp, in_exp) ->
    if v = var_name then Let (v, subst var_name repl def_exp, in_exp)
    else if mem v (free_vars repl)
    then let z = new_varname () in
      Let (z, subst var_name repl def_exp, 
              subst var_name repl (subst v (Var z) in_exp))
    else Let (v, subst var_name repl def_exp, subst var_name repl in_exp)
  | Letrec (v, def_exp, in_exp) ->
    if v = var_name then exp
    else if mem v (free_vars repl)
    then let z = new_varname () in
      Letrec (z, subst var_name repl (subst v (Var z) def_exp), 
              subst var_name repl in_exp)
    else Letrec (v, subst var_name repl def_exp, subst var_name repl in_exp)
  | Raise -> exp
  | Unassigned -> exp
  | App (app_exp, arg_exp) -> 
    App (subst var_name repl app_exp, subst var_name repl arg_exp) ;;


(* flag that adjusts for printing of abstract syntax; must be set to false for
 * unit testing *)
let abstract = false

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
  if abstract then
    match exp with
    | Var v -> "Var(" ^ v ^ ")"
    | Num i -> "Num(" ^ string_of_int i ^ ")"
    | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
    | Unop (o, exp) -> "Unop(" ^ o ^ ", " ^ exp_to_string exp ^ ")"
    | Binop (o, exp1, exp2) -> 
      "Binop(" ^ o ^ ", " ^ exp_to_string exp1 ^ 
        ", " ^ exp_to_string exp2 ^ ")"
    | Conditional (if_exp, then_exp, else_exp) -> 
      "Conditional(" ^ exp_to_string if_exp ^ ", " ^ exp_to_string then_exp ^ 
      ", " ^ exp_to_string else_exp ^ ")"
    | Fun (v, exp) -> "Fun(" ^ v ^ ", " ^ exp_to_string exp ^ ")"
    | Let (v, def_exp, in_exp) -> 
      "Let(" ^ v ^ ", " ^ exp_to_string def_exp 
        ^ ", " ^ exp_to_string in_exp ^ ")"
    | Letrec (v, def_exp, in_exp) -> 
      "Letrec(" ^ v ^ ", " ^ exp_to_string def_exp 
        ^ ", " ^ exp_to_string in_exp ^ ")"
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (app_exp, arg_exp) -> 
      "App(" ^ exp_to_string app_exp ^ ", " ^ exp_to_string arg_exp ^ ")" 
  else 
    match exp with
    | Var v -> v
    | Num i -> string_of_int i
    | Bool b -> string_of_bool b
    | Unop (o, exp) -> o ^ "(" ^ exp_to_string exp ^ ")"
    | Binop (o, exp1, exp2) -> 
      "(" ^ exp_to_string exp1 ^ ") " ^ o ^ " (" ^ exp_to_string exp2 ^ ")"
    | Conditional (if_exp, then_exp, else_exp) -> 
      "if " ^ exp_to_string if_exp ^ " then " ^ exp_to_string then_exp ^ 
      " else " ^ exp_to_string else_exp
    | Fun (v, exp) -> "(fun " ^ v ^ " -> " ^ exp_to_string exp ^ ")"
    | Let (v, def_exp, in_exp) -> 
      "(let " ^ v ^ " = " ^ exp_to_string def_exp ^ " in (" ^ 
      exp_to_string in_exp ^ "))"
    | Letrec (v, def_exp, in_exp) -> 
      "let rec " ^ v ^ " = " ^ exp_to_string def_exp ^ " in " ^
       exp_to_string in_exp
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (app_exp, arg_exp) -> 
      "(" ^ exp_to_string app_exp ^ " " ^ exp_to_string arg_exp ^ ")" ;;


(* tests for free_vars since it requires access to SS *)
let test () =
assert (mem "v" (free_vars (Var("v"))));
assert (mem "x" (free_vars (Let ("x", Fun ("y", Var "x"), Var "x"))));
assert (mem "x" (free_vars (Let("x", Fun("y", Var("x")), Var("x")))));
assert 
  (mem "y" (free_vars (Let("x", Var("y"), Fun("y", Var("x"))))));
assert 
  (mem "f" (free_vars (Fun ("y", App(Var "f", Binop("+", Var "x", Var "y")))))
  &&
  mem "x" (free_vars (Fun ("y", App(Var "f", Binop("+", Var "x", Var "y"))))))
;;

test();;

print_endline "free_vars tests passed.";;

