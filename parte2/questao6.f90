program questao6

    implicit none

    real, parameter :: Ti=10, Tf=50, q=1, a=1
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: ts, t0 ! Tstar and Tzero
    real :: t=100, tn=6000 ! T0 = 100
    real :: x = 1

    integer :: n = 1

    open(unit=1, file="data/questao6.dat", status="unknown", action="write")

    do while(.TRUE.) ! infinite loop :D
        t = tn
        tn = newton(tn)
        write(*,*) tn

        if(abs(t-tn) < 0.0000001 ) exit ! if precision is reached, break

        write(1,'(i10,f12.6)') n, tn
        n = n+1
    end do

    close(unit=1)

contains

    function newton(t)

        implicit none

        real :: newton, t

        newton = t - f(t)/Df(t)

        return
    end function


    function f(t)

        implicit none

        real :: f, t

        f = Ti - Tf + q*(2*sqrt(a*t/PI)*(e**(-x*x/(4*a*t))) - x*erfc(x/(2*sqrt(a*t))))

        if(isnan(f)) write(*,*) "f is nan"

        return
    end function

    function Df(t)
        implicit none

        real Df, t

        Df = q*a*(e**(-x*x/(4*a*t)))/(sqrt(PI*a*t))

        if(isnan(Df)) write(*,*) "Df is nan"

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


end program questao6
