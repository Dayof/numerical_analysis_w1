program questao3

    implicit none

    integer :: j, n
    real :: lambda=3, x, xn
    real, dimension(1000) :: xslist
    integer :: xslist_iter = 0

    ! Write data in output files
    open(unit=1, file="data/questao3.dat", status="unknown", action="write")

        do j=3000, 4000
            lambda = lambda + 0.001
            xn = 0.2
            call resetIterator(xslist_iter)

            do n=1, 1000 ! infinite loop
                x = xn
                xn = f(x, lambda)

                if(any(xslist==xn)) then
                    exit ! breaks loop if we find a cicle
                else
                    write(*,*) xn
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
