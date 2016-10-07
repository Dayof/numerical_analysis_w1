program questao3

    implicit none

    integer :: j, n
    real :: lambda=3.0, x, xn
    real, dimension(1000) :: xslist
    integer :: xslist_iter = 0

    real, parameter :: precision = 0.01 ! sorry about that

    ! Write data in output files
    open(unit=1, file="data/questao3.dat", status="unknown", action="write")
    open(unit=2, file="data/questao4.dat", status="unknown", action="write")


        !
        do j=3000, 4000
            lambda = lambda + 0.001
            xn = 0.4
            call resetList(xslist, xslist_iter)

            do n=1, 1000 ! pseudo-infinite loop
                x = xn
                xn = f(x, j/1000.0)

                if(isInside(xslist, xslist_iter, xn) .ne. 0) then
                    write(2, *) j/1000.0, xslist_iter-1
                    exit ! breaks loop if we find a cicle
                else
                    ! add xn to end of list
                    call addToList(xslist, xslist_iter, xn)
                end if
            end do

            write(1, *) lambda, xslist

        end do

    close(unit=1)
    close(unit=2)

contains

    function f(x, l)

        implicit none

        real :: f, x, l

        f = l * (x - x*x)

        return
    end function f

    function isInside(list, iter, element)

        implicit none

        real, dimension(1000) :: list
        real :: element
        integer :: isInside, iter

        integer :: i=1

        isInside = 0

        do i=1, iter
            if(abs(list(i) - element) .lt. precision ) then
            ! if list(i) ~ element
                isInside = i
                exit
            end if
        end do

        return

    end function isInside

    subroutine resetList(list, iterator)

        implicit none

        integer, intent(in out) :: iterator
        real, dimension(1000), intent(in out) :: list

        iterator = 1
        list = 0

        return
    end subroutine resetList

    subroutine addToList(list, iterator, element)

        implicit none

        real, dimension(1000), intent(in out) :: list
        real, intent(in) :: element
        integer, intent(in out) :: iterator

        list(iterator) = element
        iterator = iterator + 1

        return
    end subroutine addToList

end program
