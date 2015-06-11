all: main

main: expAlpha.o avgFun.o dsimp.o
	gcc -g -o main expAlpha.o avgFun.o dsimp.o -lgfortran -lm

expAlpha.o: avgFun.h
	gcc -g -c expAlpha.c

avgFun.o: avgFun.h
	gcc -g -c avgFun.c

dsimp.o: dsimp.for
	gfortran -g -c dsimp.for

clean:
	rm *.cmp *.f
	rm expAlpha.o avgFun.o main 
	rm dsimp.o
