module globalconstants
    implicit none
    ! For all constants in the project, I use the DEFAULT real and integer kinds because 
    ! it's sufficient.

    ! This global parameter n is the number of rectangles in the system
    ! and it is hardcoded here in the program, but can always be conveniently
    ! changed in this module. 
    integer, parameter :: n = 50    
    ! imax is the number of iterations in each metropolis monte carlo iteration i.e. the
    ! amount of accepted solutions in a sample of constant temperature. 
    integer, parameter :: imax = 30
    ! Cooling coefficient
    real, parameter :: a = 0.995

    ! initial temperature 
    real, parameter :: c0 = 40
    ! minimum temperature 
    real, parameter :: cmin = 0.001


end module globalconstants                