# Makefile for Ashley <3
# .o are fortran
# .a are c

############# GIVEN_SCRIPTS ##########
# .o files for linking
LIBS = datetime.o utility.o stdio.o dpackb.o

# Executables
EXECS = dq2theta dqcalc pcc12

#################################

############# BAETOWNE_SCRIPTS  ################

DEPS = main.a dsimp.o avgFun.a

EXEC_A = main.out

LDFLAGS = -lgfortran -lm

COMP_FLAGS = -g


################################################

all: $(EXECS) $(EXEC_A)

# Fpp rule
fpp: fpp.for
	gfortran -o fpp fpp.for

$(EXEC_A): $(DEPS)
	gcc -o $@ $(DEPS) $(LDFLAGS)

%.a : %.c 
	gcc $(COMP_FLAGS) -o $@ -c $<

%.o: %.f
	gfortran -o $@ -c $<

%.f: %.for fpp
	./fpp < $< > $@

%: %.o $(LIBS)
	gfortran -o $@ $< $(LIBS)

clean:
	rm $(EXECS) 
	rm fpp 
	rm *.f 
	rm *.o 
	rm *.a
	rm $(EXEC_A)
