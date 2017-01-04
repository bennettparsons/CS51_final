all: miniml stests expr evaluation

miniml: miniml.ml
	ocamlbuild miniml.byte

stests: stests.ml
	ocamlbuild stests.byte

expr: expr.ml
	ocamlbuild expr.byte

evaluation: evaluation.ml
	ocamlbuild evaluation.byte

clean:
	rm -rf _build *.byte