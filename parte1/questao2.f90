program questao2

    implicit none

    integer :: j, n
    real :: lambda, x, xn
    real, dimension (6) :: lambda_values

    lambda_values = (/ 0.4, 0.9, 1.4, 1.9, 2.4, 2.9 /)

    ! Write data in output files
    open(unit=1, file="data/questao2.1.dat", status="unknown", action="write")
    open(unit=2, file="data/questao2.2.dat", status="unknown", action="write")
    open(unit=3, file="data/questao2.3.dat", status="unknown", action="write")
    open(unit=4, file="data/questao2.4.dat", status="unknown", action="write")
    open(unit=5, file="data/questao2.5.dat", status="unknown", action="write")
    open(unit=6, file="data/questao2.6.dat", status="unknown", action="write")

        ! for each lambda
        do j=1, size(lambda_values)
            x = 100.0 ! just so abs(xn-x) is big
            xn = 0.4 ! lookup point (7) on report
            n = 0

            ! repeat until converge
            do while(abs(xn - x) .gt. 0.0001)
                n = n+1
                x = xn
                xn = f(x, lambda_values(j))
                write(j, *) n, xn
            end do

        end do

    close(unit=1)
    close(unit=2)
    close(unit=3)
    close(unit=4)
    close(unit=5)
    close(unit=6)

contains

    function f(x, l)

        implicit none

        real :: f, x, l

        f = l * (x - x*x)

        return
    end function f

end program
