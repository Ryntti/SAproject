#SA

| File                    | Content                                                                 |
| ----------------------- | ------------------------------------------------------------------------|
| mtfort90.f90            | Mersenne twister RNG module provided by A. Kuronen                      |
| globalconstants.f90     | A module containing the global constants as parameters                  |
| creation.f90            | A module containing the functions needed to create the system           |
| costfunc.f90            | A module containing the cost and position functions of the footprint    |
| moves.f90               | A module containing all the functions needed to move the rectangles     |
| SA.f90                  | The main program that performs simulated annealing                      |

Compilation option 1: (Using makefile)
```
make
```

Compilation option 2: (Manual compilation if you prefer NOT to use makefile)
```
gfortran -c mtfort90.f90 -Ofast
gfortran -c globalconstants.f90 -Ofast
gfortran -c creation.f90 -Ofast
gfortran -c costfunc.f90 -Ofast
gfortran -c moves.f90 -Ofast
gfortran SA.f90 mtfort90.f90 globalconstants.f90 creation.f90 costfunc.f90 moves.f90 -o ../run/SA -Ofast
```

#TO ERASE ALL .mod AND .o -FILES, TYPE make clean INTO THE COMMAND LINE