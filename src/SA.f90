program SA
    ! Main Program! 
    ! the simulated annealing is done here
    use globalconstants
    use costfunc
    use mtmod
    use moves
    implicit none
    type(rectangle), dimension(n) :: sys, sysin, syscand
    real :: c, costnow, costcand, delcost, xhi, cput1, cput2
    integer :: i, seed, ios

    ! create the system. The system will be random, but the amount of rectangles is hardcoded in a module.
    sys = create_system(n)
    ! Save the initial system config into a variable for plots
    sysin = sys

    print'(A, F8.2)', 'Initial cost: ', cost(sys)
    call cpu_time(cput1)    
    ! Now let's write the initial system state array into another file so that it can be plotted later. 
    open(unit = 4, file = '../run/sysconfig_initial.txt')
    do i = 1, n
        write(4,*,iostat = ios) sys(i)
        if (ios /= 0) then 
            exit
        end if
    end do
    close(unit = 4)

    ! Now, open the path to the ctgraphdata-file so we can write the cost and temp at the end of each
    ! Metropolis monte carlo step into another file, and plot the Cost as a function of Temperature later!
    open(unit = 12, file = '../run/ctgraph_data.txt')
    c = c0
    do while(c >= cmin)  
        i = 1
        ! Start the first Markov process. Let the sampling begin: 
        do i = 1, imax 
            
            ! check the cost of the current system
            costnow = cost(sys)
            ! change system configuration. The result is a candidate for the new system state.
            syscand = perform_random_move(sys)
            ! calculate the cost of the candidate system
            costcand = cost(syscand)
            delcost = costcand - costnow
        
            ! generate random number xhi in [0,1)
            seed = getseed()
            call sgrnd(seed)
            xhi = grnd()
    
            if (xhi < exp(-delcost/c)) then
                ! accept the candidate
                sys = syscand
            end if
            ! continue the process until an amount equal to imax systems are accepted, and only then
            ! move on to decreasing the temperature.
        end do
        ! Now decrease the temperature.
        c = c*a
        print'(A, f8.2, A, f6.3)', 'Cost: ', cost(sys), ' Temperature: ', c
        write(12,*) cost(sys), c
    end do

    ! At this point, it's helpful to know the cpu-time to know how much time the actual algorithm
    ! took.
    call cpu_time(cput2)
    close(unit = 12)
    print '("Cpu-time = ",f8.3," seconds.")', cput2 - cput1    ! just printing cpu time elapsed

    ! Now let's write the final system configuration into a file so that we can plot it later.
    open(unit = 10, file = '../run/sysconfend.txt')
    do i = 1, n
        write(10,*,iostat = ios) sys(i)
        if (ios /= 0) then 
            exit
        end if
    end do
    close(unit = 10)

    ! Write the initial and final system footprint properties, such as their 
    ! positions, widths and heights into a file for later plots
    open(unit = 8, file = '../run/sys_fp_properties.txt')
        write(8,*,iostat = ios) footprint_properties(sysin)
        write(8,*,iostat = ios) footprint_properties(sys)   
    close(unit = 8)

end program SA