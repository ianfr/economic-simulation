!  /$$$$$$$            /$$   /$$                                                                                  /$$
! | $$__  $$          | $$  | $$                                                                                 |__/
! | $$  \ $$  /$$$$$$ | $$ /$$$$$$  /$$$$$$$$ /$$$$$$/$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$   /$$$$$$  /$$$$$$/$$$$  /$$  /$$$$$$$  /$$$$$$$
! | $$$$$$$  /$$__  $$| $$|_  $$_/ |____ /$$/| $$_  $$_  $$ |____  $$| $$__  $$| $$__  $$ /$$__  $$| $$_  $$_  $$| $$ /$$_____/ /$$_____/
! | $$__  $$| $$  \ $$| $$  | $$      /$$$$/ | $$ \ $$ \ $$  /$$$$$$$| $$  \ $$| $$  \ $$| $$  \ $$| $$ \ $$ \ $$| $$| $$      |  $$$$$$
! | $$  \ $$| $$  | $$| $$  | $$ /$$ /$$__/  | $$ | $$ | $$ /$$__  $$| $$  | $$| $$  | $$| $$  | $$| $$ | $$ | $$| $$| $$       \____  $$
! | $$$$$$$/|  $$$$$$/| $$  |  $$$$//$$$$$$$$| $$ | $$ | $$|  $$$$$$$| $$  | $$| $$  | $$|  $$$$$$/| $$ | $$ | $$| $$|  $$$$$$$ /$$$$$$$/
! |_______/  \______/ |__/   \___/ |________/|__/ |__/ |__/ \_______/|__/  |__/|__/  |__/ \______/ |__/ |__/ |__/|__/ \_______/|_______/

!===============================================================
! rng.f90
! Random number generation utilities
!===============================================================
module rng_m
    use kinds_m
    use m_random
    use omp_lib
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

    ! See https://github.com/jannisteunissen/rng_fortran/blob/master/parallel_test.f90
    subroutine parallel_random_number(N_ENTRIES, data, rng, prng)
        ! Inputs
        integer, intent(in) :: N_ENTRIES
        real(rk), dimension(N_ENTRIES), intent(inout) :: data
        type(rng_t) :: rng
        type(prng_t) :: prng

        ! Data
        integer               :: n, tid

        ! Routine

#ifdef _OPENMP
        !$omp parallel private(n, tid, rng)
        tid = omp_get_thread_num() + 1
        !$omp do
        do n = 1, N_ENTRIES
            data(n) = prng%rngs(tid)%unif_01()
        end do
        !$omp end do
        !$omp end parallel
#else
        do n = 1, N_ENTRIES
            data(n) = prng%rngs(1)%unif_01()
        end do
#endif

        ! Update the rng seed afterwards, using the 'evolved' prng state
        call prng%update_seed(rng)

    end subroutine parallel_random_number
end module rng_m
