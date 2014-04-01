.PHONY : run_test

all:
	corebuild -pkg re2 -pkg dolog -pkg zarith -j 4 opt_solver_f.cma

chem_balancer:
	corebuild -pkg re2 -pkg dolog -pkg zarith -j 4 chem_balancer.native

test:
	corebuild -pkg re2 -pkg oUnit -pkg dolog -pkg zarith -j 4 test_runner.byte

run_test: test
	./test_runner.byte
