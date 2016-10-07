program questao789

    implicit none

    real, parameter :: Ti=10, Tf=50
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: x=1, q=1, a=1
    real :: answer, next, total=0

    ! answer = last calculated fix point
    ! next = next calculated fix point
    ! total = total of differences between fix points

    integer :: i

    character(1) :: mode

    write(*,*) "Qual constante deve ser variada? [x, a, q, none]"
    read(*,*) mode

    do i=1, 10

        if(mode.eq.'x') then !relevant to question 8
            x = 1 + i*0.4
        elseif ( mode.eq.'a' ) then ! relevant to question 9
            a = i*1.0
        elseif ( mode.eq.'q' ) then ! relevant to question 9
            q = i*1.0
        else
            mode = 'n' ! relevant to question 7
        end if

        answer = next

        call bissect(0.0, 2000.0, next)
        write(*,*) next

        if(i.ne.1) then
            total = total + (next - answer)
        elseif(mode.eq.'n') then
            exit ! if no variance is needed, break
        end if

    end do

    if(mode.ne.'n') then
        write(*,*) "Velocidade média de propagação: ", total/9
        ! irrelevant to q6
    end if

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

end program questao789
