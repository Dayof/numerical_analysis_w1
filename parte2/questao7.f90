program questao7

    implicit none

    real, parameter :: Ti=10, Tf=50, q=1, a=1
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: x=1, answer

    call bissect(1000.0, 2000.0, answer)

    write(*,*) answer

contains

    function f(t)

        implicit none

        real :: f, t

        f = Ti - Tf + q*(2*sqrt(a*t/PI)*(e**(-x*x/(4*a*t))) - x*erfc(x/(2*sqrt(a*t))))

        return
    end function

    function equals(a, b)
        implicit none

        real :: a,b
        real, parameter :: precision = 0.0000001
        logical :: equals

        equals = abs(a-b) < precision

        return
    end function

    recursive subroutine bissect(s,e, answer)

        implicit none

        real, intent(in) :: s,e
        real, intent(out) :: answer
        real :: middle

        middle = (s+e)/2

        if(equals(f(middle), .0)) then
            answer = middle
            return
        end if

        if(f(s)*f(middle).lt.0) then
            call bissect(s, middle, answer)
        else
            call bissect(middle, e, answer)
        end if

        return
    end subroutine bissect

end program questao7
