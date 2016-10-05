program iterativo

    implicit none

    double precision :: lambda, x=100, xn=0.2

    read(*,*) lambda

    ! Write data in output file
    open(unit=1, file="iterativo.dat", status="unknown", action="write")

        do lambda=0.4, 3, 0.5
            x = 100
            xn = 0.2

            do while(abs(xn - x) .gt. 0.0000001)
                x = xn
                xn = f(x, lambda)
            end do

            write(1, *) x, lambda

        end do

    close(unit=1)

contains

    function f(x, l)

        implicit none

        double precision :: f, x, l

        f = l * (x - x*x)

        return
    end function f

end program
