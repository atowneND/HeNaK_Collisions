all: main

main: expAlpha.o parsedata.o
	gcc -g -o main expAlpha.o parsedata.o

expAlpha.o: parsedata.h
	gcc -g -c expAlpha.c

parsedata.o: parsedata.h
	gcc -g -c parsedata.c

clean:
	rm *.f expAlpha.o parsedata.o main
