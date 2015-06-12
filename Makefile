# Makefile for Ashley <3

# .o files for linking
LIBS = datetime.o utility.o stdio.o dpackb.o

# Executables
EXECS = dq2theta dqcalc pcc12

all: $(EXECS)

# Fpp rule
fpp: fpp.for
	gfortran -o fpp fpp.for

%.o: %.f
	gfortran -o $@ -c $<

%.f: %.for fpp
	./fpp < $< > $@

%: %.o $(LIBS)
	gfortran -o $@ $< $(LIBS)

clean:
	rm $(EXECS) fpp *.f *.o 
