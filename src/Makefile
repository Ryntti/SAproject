FC=gfortran
SOURCES=mtfort90.f90 globalconstants.f90 creation.f90 costfunc.f90 moves.f90 SA.f90
OBJECTS = ${SOURCES: .f90 = .o}
DIR = ../run/

%.o: %.f90
	$(FC) -o $@ -c $<

SA: ${OBJECTS}
	$(FC) -o ${DIR}$@ ${OBJECTS}

clean:
	rm -f *.o *.mod $(DIR)SA