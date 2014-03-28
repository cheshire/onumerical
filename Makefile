.PHONY : run_test

all:
	corebuild -pkg re2 -pkg dolog -pkg zarith -j 4 cli_main.native

test:
	corebuild -pkg re2 -pkg oUnit -pkg dolog -pkg zarith -j 4 test_runner.byte

run_test: test
	./test_runner.byte
