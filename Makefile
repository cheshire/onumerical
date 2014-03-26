.PHONY : run_test

all:
	corebuild -pkg dolog -pkg zarith -j 4 simplex_f.cma

# TODO: run multiple test files at once?
test:
	corebuild -pkg oUnit -pkg dolog -pkg zarith -j 4 test_simplex.byte

run_test: test
	./test_simplex.byte
