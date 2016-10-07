program questao78910

    implicit none

    real, parameter :: Tf=50
    real, parameter :: PI=3.14159265359, e=2.71828182846
    real :: x=1, q=1, a=1, Ti=10
    real :: answer, next, total=0

    ! answer = last calculated fix point
    ! next = next calculated fix point
    ! total = total of differences between fix points

    integer :: i
    integer :: fileunit

    character(1) :: mode

    open(unit=7, file="data/questao7.dat", status="unknown", action="write")
    open(unit=8, file="data/questao8.dat", status="unknown", action="write")
    open(unit=91, file="data/questao9.1.dat", status="unknown", action="write")
    open(unit=92, file="data/questao9.2.dat", status="unknown", action="write")
    open(unit=925, file="data/questao10.dat", status="unknown", action="write")

    write(*,*) "Qual constante deve ser variada? [t, x, a, q, none(n)]"
    read(*,*) mode

    do i=1, 10

        if(mode.eq.'x') then !relevant to question 8
            x = 1 + i*0.4
            fileunit = 8
        elseif ( mode.eq.'a' ) then ! relevant to question 9
            a = i*1.0
            fileunit = 92
        elseif ( mode.eq.'q' ) then ! relevant to question 9
            q = i*1.0
            fileunit = 91
        elseif ( mode.eq.'t' ) then ! relevant to question 9
            Ti = i*25
            fileunit = 925
        else
            mode = 'n' ! relevant to question 7
            fileunit = 7
        end if

        answer = next

        call bissect(0.0, 1600.0, next)
        write(fileunit,*) i, next, x

        if(i.ne.1) then
            total = total + (next - answer)
        elseif(mode.eq.'n') then
            exit ! if no variance is needed, break
        end if

    end do

    if(mode.ne.'n') then
        write(*,*) "Variação de t: ", total
        write(*,*) "Velocidade média de propagação: ", 9/total
        ! irrelevant to q6
    end if

    close(unit=7)
    close(unit=8)
    close(unit=91)
    close(unit=92)
    close(unit=925)

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

        equals = abs(a-b).lt.precision

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

end program questao78910
