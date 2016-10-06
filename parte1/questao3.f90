program questao3

    implicit none

    integer :: j, n
    real :: lambda=3, x, xn
    real, dimension(1000) :: xslist
    integer :: xslist_iter = 0

    real, parameter :: precision = 0.0001

    ! Write data in output files
    open(unit=1, file="data/questao3.dat", status="unknown", action="write")

        do j=3000, 4000
            lambda = lambda + 0.001
            xn = 0.2
            call resetIterator(xslist_iter)

            do n=1, 1000 ! infinite loop
                x = xn
                xn = f(x, lambda)

                if(isInside(xslist, xslist_iter, xn).eq.0) then
                    exit ! breaks loop if we find a cicle
                else
                    ! add xn to end of list
                    call addToList(xslist, xslist_iter, xn)
                end if
            end do

            write(1, *) lambda, xslist

        end do

    close(unit=1)

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

        integer :: i
        integer :: found=0

        do i=1, iter
            if(list(i) - element .lt. precision ) then ! if list(i) == element
                found = i
                exit
            end if
        end do

        isInside = found

        return

    end function isInside

    subroutine resetIterator(iterator)

        implicit none

        integer, intent(in out) :: iterator

        iterator = 1

        return
    end subroutine resetIterator

    subroutine addToList(list, iterator, element)

        implicit none

        real, dimension(1000), intent(in out) :: list
        real, intent(in) :: element
        integer, intent(in out) :: iterator

        list(iterator) = element
        iterator = iterator + 1

        write(*,*) iterator

        return
    end subroutine addToList

end program
