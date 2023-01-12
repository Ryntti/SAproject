module moves
    use mtmod
    use globalconstants
    use costfunc
    implicit none
    ! Rotation, movement, swap and rectangle nullification aren't used anywhere outside of this module, so 
    ! they can be private. Only the perform_random_move is used in the main program.
    private :: rotate, move, swap, nullify_ith_rectangle
contains
        ! Next, I'll have the subroutines that perform the "moves" on the system of rectangles, such as rotation, 
        ! movement and swtiching. 

        !The following function rotates a rectangle by 90 degrees (90 deg rotation clockwise and
        ! counterclockwise are both the same operation)
    function rotate(v) result(s)
        implicit none
        type(rectangle), intent(in) :: v
        type(rectangle) :: vout, s
        vout = v

        vout%w = v%h
        vout%h = v%w
        s = vout
        return
    end function rotate

    ! The following function moves some chosen rectangle translationally. In more detail,
    ! the function takes in a rectangle and two floating point numbers as arguments and assigns the
    ! floating point numbers to the position coordinates of the given rectangle. 
    function move(v,newx,newy) result(s)
        implicit none
        type(rectangle), intent(in) :: v
        real, intent(in) :: newx, newy
        type(rectangle) :: vout, s
        vout = v
        vout%x = newx
        vout%y = newy
        s = vout
        return
    end function move


    ! The swap subroutine takes in two rectangles v and k, their respective indices in the system i and j
    ! and the array representing the whole system R, which is an array of rectangles. The subroutine
    ! swaps the position coordinates of the rectangles v and k with oneanother and thereby changes the 
    ! system configuration.
    subroutine swap(v, i, k, j, R)
        implicit none
        type(rectangle), intent(in) :: v, k
        type(rectangle), dimension(n), intent(out) :: R
        type(rectangle) :: p, b
        real :: t, l, m
        integer, intent(in) :: i, j
    
        p = v
        b = k
        b%x = v%x
        b%y = v%y

        p%x = k%x
        p%y = k%y
    
        R(i) = p
        R(j) = b
    end subroutine swap

    ! The following function randomly chooses one rectangle from the system and returns that 
    ! rectangle.
    function select_random_rectangle(R, m) result(s)
        implicit none
        type(rectangle), dimension(m), intent(in) :: R
        type(recwithind) :: s
        integer :: seed, t, m
        seed = getseed()
        call sgrnd(seed)
        t = igrnd(1, m)

        s%z = R(t)
        s%ind = t
        return
    end function select_random_rectangle

    ! The following function takes in an integer i and a system of rectangles as its arguments,
    ! and assigns the components of the i:th rectangle of the system R to such values that
    ! it can't possibly overlap with any of the other rectangles of the system, by changing its position to (-100, -100)
    ! on the xy-plane and making its area zero.
    function nullify_ith_rectangle(i,R) result(w)
        implicit none
        integer,intent(in) :: i
        type(rectangle), dimension(n), intent(in) :: R
        type(rectangle), dimension(n) :: tr
        type(rectangle), dimension(n) :: w
        tr = R
        tr(i)%x = -100.0
        tr(i)%y = -100.0
        tr(i)%w = 0.0
        tr(i)%h = 0.0
        w = tr
        return
    end function nullify_ith_rectangle

    ! The following function randomly chooses an integer out of 1, 2 and 3, and based on the result, performs one of 
    ! the moves rotation, movement or swap. If the chosen move causes overlap, the function starts over and randomly 
    ! chooses another move and continues with that until a move emerges that causes no overlap and is thereby accepted.
    function perform_random_move(R) result(Rout)
        implicit none
        type(rectangle), dimension(n), intent(in) :: R
        type(rectangle), dimension(n) :: Rout, Rdummy
        type(rectangle) :: tr, tr1, trswap
        type(recwithind) :: randselect, randswappable
        real :: ny, nx
        integer :: seed, i, t3, irswap, ir
        Rdummy = R
        do
            seed = getseed()
            call sgrnd(seed)
            t3 = igrnd(1,3)

            if(t3 == 1) then
                randselect = select_random_rectangle(Rdummy, n)
                tr = randselect%z
                ir = randselect%ind
                ! Note that Rdummy(ir) = randselect%z = tr

                tr1 = rotate(Rdummy(ir)) ! This is the same as writing tr = rotate(tr)
                ! Now set Rdummy(ir) to zero to check that rotate(Rdummy(ir)) doesn't overlap with any other rectangles of Rdummy
                Rdummy = nullify_ith_rectangle(ir, R)
                if (does_overlap_with_any(n, Rdummy, tr1) .eqv. .FALSE.) then
                    Rdummy(ir) = tr1
                    ! rotation successful, move accepted.                      
                    exit 
                else
                    Rdummy(ir) = tr
                    ! rotation unsuccessdul, move rejected!
                end if

            else if (t3 == 2) then
                randselect = select_random_rectangle(Rdummy, n)
                tr = randselect%z
                ir = randselect%ind
                ! Note again, tr = R(ir) at this point
                ! let's get random real numbers from the interval 0,100:
                nx = igrnd(0,100)*grnd()                    
                ny = igrnd(0,100)*grnd()
                tr1 = move(Rdummy(ir), nx, ny)
                ! Now again, let's set the existing R(ir) to zero since we are replacing it with move(R(ir)) and check overlaps
                Rdummy = nullify_ith_rectangle(ir, R)
                if (does_overlap_with_any(n, Rdummy, tr1) .eqv. .FALSE.) then
                    Rdummy(ir) = tr1
                    ! movement successful, move accepted.
                    exit
                else
                    Rdummy(ir) = tr
                    !  movement unsuccessful, move rejected.
                end if

            else if(t3 == 3) then
                ! Randomly select one of the rectangles of the system
                randselect = select_random_rectangle(Rdummy, n)
                tr = randselect%z
                ir = randselect%ind
                ! Note Rdummy(ir) = tr again
                ! Now randomly select another rectangle from the system to swap with, but make 
                ! sure that the second pick isn't the same as the one we picked already
                           
                randswappable = select_random_rectangle(Rdummy, n)
                trswap = randswappable%z
                irswap = randswappable%ind                  

                ! Note Rdummy(irswap) = trswap again
                call swap(tr, ir, trswap, irswap, Rdummy)
            
                if ((does_overlap_with_any(n, nullify_ith_rectangle(ir, Rdummy), Rdummy(ir)) .eqv. .FALSE.) .and. &
                (does_overlap_with_any(n, nullify_ith_rectangle(irswap, Rdummy), Rdummy(irswap)) .eqv. .FALSE.)) then
                    ! swap successful, move accepted.
                    exit
                else
                    Rdummy(irswap) = randswappable%z
                    Rdummy(ir) = randselect%z
                    ! swap unsuccessful, move rejected.
                end if
            
            end if

        end do

        Rout = Rdummy
        return
    end function perform_random_move

end module moves