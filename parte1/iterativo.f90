program iterativo

    implicit none

    real :: lambda, x=100, xn

    read(*,*) lambda, xn

    do while(abs(xn - x) .gt. 0.00001)
        x = xn
        xn = f(x)
        write(*,*) xn
    end do

    ! Write data in output file
    open(unit=1, file="iterativo.dat", status="unknown", action="write")



    close(unit=1)

contains

    function f(x)

        implicit none

        real :: f, x

        f = lambda * (x - x*x)

        return
    end function f

end program
