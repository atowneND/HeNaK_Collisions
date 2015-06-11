all: main averages

main: expAlpha.o parsedata.o
	gcc -g -o main expAlpha.o parsedata.o

expAlpha.o: parsedata.h
	gcc -g -c expAlpha.c

parsedata.o: parsedata.h
	gcc -g -c parsedata.c

averages: avgFun.o averages.o dsimp.o
	gcc -g -o averages avgFun.o averages.o dsimp.o -lgfortran -lm

avgFun.o: avgFun.h
	gcc -g -c avgFun.c

averages.o: averages.c
	gcc -g -c averages.c

dsimp.o: dsimp.for
	gfortran -g -c dsimp.for

clean:
	rm *.cmp *.f expAlpha.o parsedata.o main avgFun.o averages.o dsimp.o averages
