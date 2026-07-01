all: libruntime.so tests sample_program

libruntime.so: runtime.c runtime.h
	clang -g -shared runtime.c -o libruntime.so

tests: runtime.c runtime.h
	clang -O3 -g -DENABLE_TESTS runtime.c -o tests

sample_program: sample_program.c libruntime.so
	clang -O3 -g sample_program.c -I. -L. -lruntime -o sample_program
