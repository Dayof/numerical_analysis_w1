program questao6

    implicit none

    real, parameter :: Ti=10, Tf=50, q=1, a=1
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: ts, t0=1320 ! Tstar and Tzero
    real :: t, tn
    real :: x = 1

    integer :: j, sum, n = 1
    integer, dimension (6) :: coef ! t0's variation coeficient

    coef = (/ 1, 2, 3, 4, 5, 10 /)

    open(unit=1, file="data/questao6.dat", status="unknown", action="write")
    open(unit=2, file="data/questao6.1.dat", status="unknown", action="write")

    do j=1, size(coef)
        n = 1
        tn = coef(j) * t0
        do while(.TRUE.) ! infinite loop :D
            t = tn
            tn = newton(tn)

            if(isnan(tn)) then
                write(*,*) "Tn é NaN para t0=", t0*coef(j)
                exit
            end if

            if(equals(t, tn)) then
                write(2,'(i8, f14.6)') n, t0*coef(j)
                exit ! if precision is reached, break
            end if

            if(j.eq.1) write(1,'(i10,f12.6)') n, tn

            n = n+1
        end do
    end do

    close(unit=1)
    close(unit=2)

contains

    function newton(t)

        implicit none

        real :: newton, t

        newton = t - f(t)/Df(t)

        if(isnan(newton)) newton=0 ! fix for negative values of newton

        return
    end function


    function f(t)

        implicit none

        real :: f, t

        f = Ti - Tf + q*(2*sqrt(a*t/PI)*(e**(-x*x/(4*a*t))) - x*erfc(x/(2*sqrt(a*t))))

        return
    end function

    function Df(t)
        implicit none

        real Df, t

        Df = q*a*(e**(-x*x/(4*a*t)))/(sqrt(PI*a*t))

        return
    end function

    function erfc(t)
        implicit none

        real erfc, t
        integer :: i
        real, dimension(6) :: A

        erfc = 0

        A = (/  0.0705230784, &
                0.0422820123, &
                0.0092705272, &
                0.0001520143, &
                0.0002765672, &
                0.0000430638 /)

        do i=1, 6
            erfc = erfc + A(i)*(t**i)
        end do

        erfc = 1 / (1+ erfc)**16

    end function

    function equals(a, b)
        implicit none

        real :: a,b
        real, parameter :: precision = 0.0000001
        logical :: equals

        equals = abs(a-b).lt.precision

        return
    end function

end program questao6
