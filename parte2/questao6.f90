program questao6

    implicit none

    real, parameter :: Ti=10, Tf=50, q=1, a=1
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: ts, t0 ! Tstar and Tzero
    real :: t=100, tn=100 ! T0 = 100
    real :: x = 1

    integer :: j, sum, n = 1
    integer, dimension (6) :: tn_var
    real, dimension (6) :: tn_var_fix

    tn_var = (/ tn, tn*2, tn*3, tn*4, tn*5, tn*10 /)
    tn_var_fix = tn_var

    open(unit=1, file="data/questao6.dat", status="unknown", action="write")
    open(unit=2, file="data/questao6.1.dat", status="unknown", action="write")

    do j=1, size(tn_var_fix)
        n = 1
        do while(.TRUE.) ! infinite loop :D
            t = tn_var_fix(j)
            tn_var_fix(j) = newton(tn_var_fix(j))

            if(abs(t-tn_var_fix(j)) < 0.0000001 ) then
                write(2,'(i10, i10)') n, tn_var(j)
                exit ! if precision is reached, break
            end if

            if(j == 1) write(1,'(i10,f12.6)') n, tn_var_fix(j)

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


end program questao6
