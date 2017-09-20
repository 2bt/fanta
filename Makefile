all: vm bytecode

vm: vm.cpp Makefile
	g++ -Wall -Og -g vm.cpp -o vm

bytecode:
	./compiler
