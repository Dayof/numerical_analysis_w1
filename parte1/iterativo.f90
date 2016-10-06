program iterativo

    implicit none

    integer :: j
    real :: lambda, x, xn
    real, dimension (6) :: lambda_values

    lambda_values = (/ 0.4, 0.9, 1.4, 1.9, 2.4, 2.9 /)

    ! Write data in output file
    open(unit=1, file="iterativo.dat", status="unknown", action="write")

        do j=1, size(lambda_values)
            x = 100.0
            xn = 0.2

            do while(abs(xn - x) .gt. 0.0000001)
                x = xn
                xn = f(x, lambda_values(j))
            end do

            write(1, *) x, lambda_values(j)

        end do

    close(unit=1)

contains

    function f(x, l)

        implicit none

        real :: f, x, l

        f = l * (x - x*x)

        return
    end function f

end program
