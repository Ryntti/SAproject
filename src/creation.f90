module creation
    use globalconstants
    use mtmod
    implicit none
    ! Define the rectangle type, a derived type that has four real number components, two position vector components
    ! and two real numbers "w" standing for width and "h" standing for height as the third and fourth arguments
    ! Such a type contains all possible relevant information of a rectangle. 
    type :: rectangle
        real :: x, y, w, h
    end type rectangle
    
    ! The recwithind type exists just so that I can have functions that act on an array of rectangles, returning
    ! an array element rectangle AND its index in the array.
    type :: recwithind
        type(rectangle) :: z
        integer :: ind
    end type recwithind
        ! Declare the system, an array of rectangles.
    type(rectangle), dimension(n) :: system
    contains

        ! The does_overlap function checks whether the rectangles given as arguments
        ! overlap.
        logical function does_overlap(m,k)
        implicit none
        type(rectangle), intent(in) :: m, k
        real :: kymax, kymin, kxmax, kxmin, mxmax, mxmin, mymax, mymin     ! Let's define the max and min values for the outermost x- and y- components
        logical :: e                                                       ! outermost x- and y- components of both m and k rectangles
        kymax = k%y + 0.5*k%h                                              
        kymin = k%y - 0.5*k%h
        kxmax = k%x + 0.5*k%w
        kxmin = k%x - 0.5*k%w

        mymax = m%y + 0.5*m%h
        mymin = m%y - 0.5*m%h
        mxmax = m%x + 0.5*m%w
        mxmin = m%x - 0.5*m%w

        if(kxmax <= mxmin .OR. kxmin >= mxmax .OR. kymax <= mymin .OR. kymin >= mymax) then
            ! k doesn't overlap with m, return false
            e = .FALSE.
        else
            ! k overlaps with m, return true.
            e = .TRUE.
        end if
        does_overlap = e
        return
        end function does_overlap

        ! the following function takes in the dimension (num) of the system, the system array itself (R) 
        ! and and a rectangle (k). It then loops over an iterator i up to num and checks whether the rectangle k 
        ! overlaps with any of the elements of the array R.
        logical function does_overlap_with_any(num, R, k)
            implicit none
            integer, intent(in) :: num
            type(rectangle), dimension(num), intent(in) :: R
            type(rectangle), intent(in) :: k
            integer :: i
            logical :: e 
            do i = 1,num
                if(does_overlap(R(i), k) .eqv. .TRUE.) then
                ! return TRUE
                    e = .TRUE.
                    exit
                else 
                    e = .FALSE.
                end if
            end do
            does_overlap_with_any = e
            return
        end function does_overlap_with_any

        function create_rectangle() result(s)
            implicit none
            type(rectangle) :: da, s
            integer :: seed, i
            seed = getseed()
            call sgrnd(seed)
            da%x = igrnd(50,100)*grnd()
            da%y = igrnd(50,100)*grnd()
            da%w = igrnd(34,35)*grnd()/2.0
            da%h = igrnd(34,35)*grnd()/2.0
            s = da
        end function create_rectangle

        ! The create_system function takes in an integer which is the desired amount of rectangles to 
        ! be created, and returns an array of length num full of non-overlapping rectangles of random 
        ! positions, widths and heights. 
        function create_system(num) result(s)
            implicit none
            integer :: i, dim
            integer, intent(in) :: num
            type(rectangle) :: q
            type(rectangle), dimension(num) :: s, R
            R(1) = create_rectangle()
            dim = num 
            do i = 2, dim
                do
                    q = create_rectangle()
                    if(does_overlap_with_any(i, R, q) .eqv. .FALSE.) then
                        R(i) = q
                        exit
                    end if
                end do
            end do
            s = R
        end function create_system

end module creation