program simplefun

    real, parameter :: Tf=50
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: x=1, q=1, a=1, Ti=10

    read(*,*) t
    write(*,*) f(t)

contains

    function f(t)

        implicit none

        real :: f, t

        f = Ti - Tf + q*(2*sqrt(a*t/PI)*(e**(-x*x/(4*a*t))) - x*erfc(x/(2*sqrt(a*t))))

        return
    end function

end program simplefun
