.PHONY : run_test install remove clean doc

COMPILER := ocamlbuild -use-ocamlfind -syntax camlp4o -pkg core -pkg sexplib.syntax,comparelib.syntax -tag thread
FLAGS := -pkg dolog -pkg zarith -j 4
BUILD := _build/src/
INTERFACES := expression_f.cmi vector_f.cmi matrix_f.cmi dual_simplex_solver_f.cmi float_number.cmi opt_solver_f.cmi str_var.cmi rational_number.cmi number_intf.cmi var_intf.cmi

all:
	echo "Compiling native"
	$(COMPILER) $(FLAGS) onumerical.cmxa
	echo "Compiling bytecode"
	$(COMPILER) $(FLAGS) onumerical.cma

chem_balancer:
	$(COMPILER) -pkg re2 $(FLAGS) chem_balancer.native

test:
	$(COMPILER) -pkg oUnit -pkg re2 $(FLAGS) test_runner.byte

test.native:
	$(COMPILER) -pkg oUnit -pkg re2 $(FLAGS) test_runner.native

run_test: test
	./test_runner.byte

doc:
	$(COMPILER) doc.docdir/index.html

install:
	ocamlfind install onumerical META $(addprefix $(BUILD), onumerical.cmxa onumerical.cma) $(addprefix $(BUILD), $(INTERFACES))

remove:
	ocamlfind remove onumerical

clean:
	rm -rf _build
