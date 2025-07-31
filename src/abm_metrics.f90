! Module with ABM metrics definitions completely independent of any specific model
! As soon as a metric is needed by more than one model, it should be moved here with a "metric_" prefix

module abm_metrics_m
    use, intrinsic :: iso_c_binding
    use kinds_m
    implicit none
#ifdef __GFORTRAN__
    ! Define an explicit interface for libc's qsort to maintain compatibility with gfortran
    ! https://gcc.gnu.org/onlinedocs/gcc-9.4.0/gfortran/Preprocessing-and-conditional-compilation.html
    ! Apparently ifdef etc. can't be indented https://fortran-lang.discourse.group/t/problem-using-c-preprocessor-with-gfortran/64
    interface
        subroutine qsort(base, nel, width, compar) bind(c, name='qsort')
            import c_size_t, c_int
            implicit none
            type(*), intent(inout) :: base(*)
            integer(c_size_t), value :: nel
            integer(c_size_t), value :: width
            abstract interface
                function compar_iface(a, b) bind(c)
                    import c_int, c_ptr
                    implicit none
                    integer(c_int) compar_iface
                    type(c_ptr), value :: a, b
                end function
            end interface
            procedure(compar_iface) compar
        end subroutine
    end interface
#endif

contains

! More code for an explicit interface for libc's qsort to maintain compatibility with gfortran
! Note: if rk ever becomes 32 bit, this will need to be updated
#ifdef __GFORTRAN__
    ! Comparison function for qsort to sort real values
    function less_real8(a, b) result(result)
        integer(c_int) result
        type(c_ptr), value :: a, b
        real(c_double), pointer :: ap, bp
        call c_f_pointer(a, ap)
        call c_f_pointer(b, bp)
        ! print*, "ap: ", ap
        ! print*, "bp: ", bp
        result = int(ap - bp, c_int)
    end function
    ! The actual qsort call to libc's qsort
    subroutine my_qsort_real8(a, nel)
        real(c_double), intent(inout) :: a(*)
        integer(4), value :: nel
        call qsort(a, int(nel, c_size_t), c_sizeof(a(1)), less_real8)
    end subroutine
#endif

    !------------------------------------------------------------
    ! Sorting utility function with associated comparison operator
    integer function sort_compar_op(a, b)
        real(rk) :: a, b
        if (a < b) then
            sort_compar_op = -1
        else if (a > b) then
            sort_compar_op = 1
        else
            sort_compar_op = 0
        end if
        return
    end function sort_compar_op
    subroutine sort_real_array(arr)
        real(rk), intent(inout) :: arr(:)
#ifdef __GFORTRAN__
        ! Use custom qsort for gfortran to avoid issues with type-bound procedures
        call my_qsort_real8(arr, size(arr)) ! custom sort for gfortran
#else
        call qsort(arr, size(arr), sizeof(rk), sort_compar_op) ! builtin sort
#endif
    end subroutine sort_real_array

    !------------------------------------------------------------
    ! Compute Gini coefficient for cash distribution
    function metric_gini_coefficient(cash_values) result(gini)
        real(rk), intent(in) :: cash_values(:)
        real(rk) :: gini
        real(rk), allocatable :: cash_sorted(:)
        integer :: n, i
        real(rk) :: sum_weighted, mean_cash

        n = size(cash_values)
        allocate (cash_sorted(n))

        ! Copy cash Rvalues
        cash_sorted = cash_values

        ! Sort cash values
        call sort_real_array(cash_sorted)

        ! Calculate mean cash
        mean_cash = sum(cash_sorted) / real(n, rk)

        ! Handle edge case where mean is zero or negative
        if (mean_cash <= 0.0_rk) then
            gini = 0.0_rk
            return
        end if

        ! Calculate Gini coefficient using the formula:
        ! G = (2 * sum(i * y_i)) / (n * sum(y_i)) - (n + 1) / n
        ! where y_i are sorted values and i is the rank (1-indexed)
        sum_weighted = 0.0_rk
        do i = 1, n
            sum_weighted = sum_weighted + real(i, rk) * cash_sorted(i)
        end do

        gini = (2.0_rk * sum_weighted) / (real(n, rk) * sum(cash_sorted)) - (real(n + 1, rk) / real(n, rk))

        ! Ensure Gini is in [0, 1] range
        gini = max(0.0_rk, min(1.0_rk, gini))
    end function metric_gini_coefficient

end module abm_metrics_m
