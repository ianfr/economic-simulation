!===============================================================
! agent_sim_base.f90
! Simulator base class for agent-based models
! Note: Each concrete simulator instance uses its own private Agent struct
!===============================================================

module sim_base_m
    use kinds_m
    use abstract_config_m
    implicit none
    type, abstract :: AbstractSimulator
    contains
        procedure(init_sim_iface), deferred :: init
        procedure(run_sim_iface), deferred :: run
        procedure(write_pop_csv_iface), deferred :: write_population_csv
        procedure(compute_metrics_iface), deferred :: compute_metrics
        procedure(get_metric_names_iface), deferred :: get_metric_names
        procedure :: write_metrics_csv
        procedure :: write_metrics_header
    end type AbstractSimulator

    abstract interface
        subroutine init_sim_iface(this, cfg)
            import :: AbstractSimulator, AbstractConfig
            class(AbstractSimulator), intent(out) :: this
            class(AbstractConfig), intent(in)          :: cfg
        end subroutine init_sim_iface
        subroutine run_sim_iface(this)
            import :: AbstractSimulator
            class(AbstractSimulator), intent(inout) :: this
        end subroutine run_sim_iface
        subroutine write_pop_csv_iface(this, filename)
            import :: AbstractSimulator
            class(AbstractSimulator), intent(in) :: this
            character(*), intent(in) :: filename
        end subroutine write_pop_csv_iface
        function compute_metrics_iface(this) result(metrics)
            import :: AbstractSimulator, rk
            class(AbstractSimulator), intent(in) :: this
            real(rk), allocatable :: metrics(:)
        end function compute_metrics_iface
        function get_metric_names_iface(this) result(names)
            import :: AbstractSimulator
            class(AbstractSimulator), intent(in) :: this
            character(len=32), allocatable :: names(:)
        end function get_metric_names_iface
    end interface

contains

    !------------------------------------------------------------
    ! Initialize the output CSV file for metrics with a header
    ! Relies on the overloaded get_metric_names method
    subroutine write_metrics_header(this, filename)
        class(AbstractSimulator), intent(in) :: this
        character(*), intent(in) :: filename
        character(len=32), allocatable :: names(:)
        integer :: unit, ios, i

        names = this % get_metric_names()

        open (newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write (*, '(a,a)') 'Error opening metrics file: ', filename
            return
        end if

        ! Write CSV header
        write (unit, '(a)', advance='no') 'timestep'
        do i = 1, size(names)
            write (unit, '(a,a)', advance='no') ',', trim(names(i))
        end do
        write (unit, '()')  ! newline

        close (unit)
    end subroutine write_metrics_header

    !------------------------------------------------------------
    ! Write a line of metrics to the CSV file
    ! This relies on the overloaded compute_metrics method
    subroutine write_metrics_csv(this, filename, timestep)
        class(AbstractSimulator), intent(in) :: this
        character(*), intent(in) :: filename
        integer(int64), intent(in) :: timestep
        real(rk), allocatable :: metrics(:)
        integer :: unit, ios, i
        logical :: file_exists

        print *, 'Writing metrics for timestep:', timestep, "..."

        metrics = this % compute_metrics()

        ! Check if file exists to determine if we need to append
        inquire (file=filename, exist=file_exists)

        if (file_exists) then
            open (newunit=unit, file=filename, status='old', position='append', action='write', iostat=ios)
        else
            open (newunit=unit, file=filename, status='new', action='write', iostat=ios)
        end if

        if (ios /= 0) then
            write (*, '(a,a)') 'Error opening metrics file: ', filename
            return
        end if

        ! Write timestep and metrics
        write (unit, '(i0)', advance='no') timestep
        do i = 1, size(metrics)
            write (unit, '(a,f0.6)', advance='no') ',', metrics(i)
        end do
        write (unit, '()')  ! newline

        close (unit)

        print *, "...done writing metrics."
    end subroutine write_metrics_csv
end module sim_base_m
