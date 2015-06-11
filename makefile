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
	rm *.cmp *.f expAlpha.o parsedata.o main avgFun.o averages.o dsimp.o averages
