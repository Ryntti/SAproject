module costfunc
    use globalconstants
    use mtmod
    use creation
    implicit none
    ! the cost function calculates the total rectangular footprint of all the rectangles
    ! essentially it calculates the distance between the outermost edges of the
    ! horizontally outermost rectangles, and the distance between the outermost edges of the
    ! vertically outermost rectangles and multiplies them. It is therefore the maximum rectangular
    ! surface area occupied by the system.
    contains                                                
        real function cost(sys)
            implicit none
            type(rectangle), dimension(n), intent(in) :: sys
            integer :: i
            real :: x1, x2, x3, x4
            
            x1 = maxval(sys%x + 0.5*sys%w)
            x2 = maxval(sys%y + 0.5*sys%h)
            x3 = minval(sys%x - 0.5*sys%w)
            x4 = minval(sys%y - 0.5*sys%h)

            cost = (x1-x3)*(x2-x4)
            return
        end function cost

        ! The following footprint properties function is just for plotting:
        ! it returns the position coordinates of the lower left corner of 
        ! the footprint rectangle and the width and height thereof.
        function footprint_properties(sys) result(s)
            implicit none
            type(rectangle), dimension(n), intent(in) :: sys
            real, dimension(4) :: s, w
            integer :: i
            real :: x1, x2, x3, x4, r, t
        
            x1 = maxval(sys%x + 0.5*sys%w)
            x2 = maxval(sys%y + 0.5*sys%h)
            x3 = minval(sys%x - 0.5*sys%w)
            x4 = minval(sys%y - 0.5*sys%h)

            w = [x3, x4, x1-x3, x2-x4]
            s = w
            return
        end function footprint_properties

end module costfunc