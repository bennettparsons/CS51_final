# CS51_final

This repository constitutes my final project for CS51, Harvard's second introductory computer science course, focusing on abstraction and functional programming through the lens of the language OCaml. The project implements an Ocaml like language, MiniMl, and can implement lexical and dynamic scoping (for more details on scoping, see unit tests in stests.ml or refer to writeup.pdf).

What I implemented:
* evaluation.ml - functions within Env module, all stests.ml function variants
* expr.ml - all functions: ```free_vars```, ```new_varname```, ```subst```, ```exp_to_string```, as well as tests
* miniml_lex.mll - added ```not``` keyword, ```~``` negation, ```<>``` exor, ```<``` and ```>``` comparators, and ```/``` division
* miniml_parse.mly - same functionality added as miniml_lex.mll
* stests.ml - all

Installation:
latest version of OCaml: http://ocaml.org/docs/install.html

Run Instructions:
compile with ```make```
launch MiniMl with ```./miniml.byte```
run unit tests with ```./stests.byte```
