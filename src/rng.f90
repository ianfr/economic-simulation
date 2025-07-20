!===============================================================
! rng.f90
! Random number generation utilities
!===============================================================
module rng_m
    use kinds_m
    implicit none
contains
    subroutine init_rng(seed)
        integer(int64), intent(in) :: seed
        integer, allocatable :: put(:)
        integer :: n, i
        call random_seed()                 ! portable default seed
        if (seed /= 0) then
            call random_seed(size=n)
            allocate (put(n))
            do i = 1, n
                put(i) = int(mod(seed + i * 37_int64, huge(1_int64)), kind(put))
            end do
            call random_seed(put=put)
        end if
    end subroutine init_rng

    subroutine shuffle_int(a)
        integer, intent(inout) :: a(:)   ! Fisher-Yates shuffle
        integer :: i, j, tmp, n
        real(rk) :: u
        n = size(a)
        do i = n, 2, -1
            call random_number(u)
            j = 1 + int(u * i)
            tmp = a(j); a(j) = a(i); a(i) = tmp
        end do
    end subroutine shuffle_int
end module rng_m
