# makefile 
#
#
FC=gfortran
EXEC=survival
PY=python3
OBJ=survival.o

all:    
	
	$(PY) random_condition.py 	
	$(FC) -c  survival.f90

	$(FC) -c  main.f90
	$(FC) $(OBJ) main.o -o $(EXEC)

clean:
	rm -rf *.o *.mod *.dat $(EXEC)

