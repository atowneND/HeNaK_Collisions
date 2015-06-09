all: main

main: expAlpha.o parsedata.o
	gcc -o main expAlpha.o parsedata.o

expAlpha.o: parsedata.h
	gcc -c expAlpha.c

parsedata.o: parsedata.h
	gcc -c parsedata.c

clean:
	rm *.f expAlpha.o parsedata.o main
