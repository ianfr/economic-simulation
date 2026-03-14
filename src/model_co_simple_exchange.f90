!  /$$$$$$$            /$$   /$$                                                                                  /$$
! | $$__  $$          | $$  | $$                                                                                 |__/
! | $$  \ $$  /$$$$$$ | $$ /$$$$$$  /$$$$$$$$ /$$$$$$/$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$   /$$$$$$  /$$$$$$/$$$$  /$$  /$$$$$$$  /$$$$$$$
! | $$$$$$$  /$$__  $$| $$|_  $$_/ |____ /$$/| $$_  $$_  $$ |____  $$| $$__  $$| $$__  $$ /$$__  $$| $$_  $$_  $$| $$ /$$_____/ /$$_____/
! | $$__  $$| $$  \ $$| $$  | $$      /$$$$/ | $$ \ $$ \ $$  /$$$$$$$| $$  \ $$| $$  \ $$| $$  \ $$| $$ \ $$ \ $$| $$| $$      |  $$$$$$
! | $$  \ $$| $$  | $$| $$  | $$ /$$ /$$__/  | $$ | $$ | $$ /$$__  $$| $$  | $$| $$  | $$| $$  | $$| $$ | $$ | $$| $$| $$       \____  $$
! | $$$$$$$/|  $$$$$$/| $$  |  $$$$//$$$$$$$$| $$ | $$ | $$|  $$$$$$$| $$  | $$| $$  | $$|  $$$$$$/| $$ | $$ | $$| $$|  $$$$$$$ /$$$$$$$/
! |_______/  \______/ |__/   \___/ |________/|__/ |__/ |__/ \_______/|__/  |__/|__/  |__/ \______/ |__/ |__/ |__/|__/ \_______/|_______/

!===============================================================
! model_co_simple_exchange.f90
! Simple exchange simulation implementation
! Leverages coarray Fortran provided by http://www.opencoarrays.org/
!
! Design notes:
!   Fortran does not permit allocatable coarray components inside a derived
!   type, so the population array lives at module scope as a workaround.
!   See: https://j3-fortran.org/doc/year/10/10-007.pdf C526
!   After some googling, it seems like this might be all resolved in f2023 but doing it
!   this way to be safe for now.
!
!   Parallelism strategy:
!     - Each coimage owns a contiguous slice of agents (co_pop(:)[*]).
!     - Every image generates the same random permutation (same RNG seed).
!     - Work is assigned by payer ownership rather than by pair index.
!     - Each image decides exchanges for the agents it owns when they appear
!       in the payer position of the permutation.
!     - If the receiver is remote, the payer image writes a single credit into
!       the receiver image's inbox instead of performing remote cash reads and
!       writes on the population itself.
!     - Because every agent appears exactly once in the permutation, each
!       receiver inbox slot can be written at most once per step, so the
!       remote credit notifications are race-free.
!     - sync all is used between inbox clear, remote credit writes, and local
!       inbox application so every image sees a consistent population state.
!===============================================================
module model_co_simple_exchange_m
    use kinds_m
    use abstract_config_m
    use config_m
    use rng_m
    use sim_base_m
    use abm_metrics_m
    implicit none
    public ! public by default, Agent below is private

    ! Define the agent privately
    private :: Agent
    type :: Agent
        real(rk) :: cash = 0.0_rk
    end type Agent

    ! Module-level coarray -- workaround for the restriction that allocatable
    ! coarrays cannot be components of a derived type (yet?).
    ! This limits the module to a single active CoSimpleExchange instance,
    ! which is fine for this simulation framework.
    type(Agent), allocatable :: co_pop(:)[:]
    real(rk), allocatable :: incoming_credit(:)[:]

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: CoSimpleExchange
        integer(int64) :: total_agents = 0
        integer(int64) :: local_n = 0      ! agents owned by this image
        character(len=32), allocatable :: names(:)
        real(rk) :: debt_limit   = 0.0_rk
        real(rk) :: exchange_delta = 1.0_rk
    contains
        ! Override deferred abstract methods
        procedure :: init
        procedure :: step
        procedure :: write_population_csv
        procedure :: compute_metrics
        procedure :: get_metric_names

        ! Override concrete base-class CSV helpers so only image 1 writes
        procedure :: write_metrics_header => co_write_metrics_header
        procedure :: write_metrics_csv    => co_write_metrics_csv

        ! Private helpers
        procedure, private :: gini_coefficient
    end type CoSimpleExchange

contains

    !------------------------------------------------------------
    ! Map a global agent index g (1-based) to (image, local index).
    ! Agents are distributed so the first rem images hold (base+1) agents
    ! and the remaining (nimg-rem) images hold base agents.
    pure subroutine global_to_local(g, n_total, nimg, img, local_idx)
        integer(int64), intent(in)  :: g, n_total
        integer,        intent(in)  :: nimg
        integer,        intent(out) :: img
        integer(int64), intent(out) :: local_idx
        integer(int64) :: base, rem, nimg64

        nimg64 = int(nimg, int64)
        base   = n_total / nimg64
        rem    = mod(n_total, nimg64)

        if (g <= rem * (base + 1_int64)) then
            img       = int((g - 1_int64) / (base + 1_int64)) + 1
            local_idx = mod(g - 1_int64, base + 1_int64) + 1_int64
        else
            ! adjusted index among the "base-size" images
            block
                integer(int64) :: g_adj
                g_adj     = g - rem * (base + 1_int64)
                img       = int(rem + (g_adj - 1_int64) / base) + 1
                local_idx = mod(g_adj - 1_int64, base) + 1_int64
            end block
        end if
    end subroutine global_to_local

    !------------------------------------------------------------
    ! Return the inclusive global index range owned by image me.
    pure subroutine local_global_bounds(me, n_total, nimg, g_lo, g_hi)
        integer,        intent(in)  :: me, nimg
        integer(int64), intent(in)  :: n_total
        integer(int64), intent(out) :: g_lo, g_hi
        integer(int64) :: base, rem, me64, nimg64

        nimg64 = int(nimg, int64)
        me64   = int(me, int64)
        base   = n_total / nimg64
        rem    = mod(n_total, nimg64)

        if (me64 <= rem) then
            g_lo = (me64 - 1_int64) * (base + 1_int64) + 1_int64
            g_hi = g_lo + base
        else
            g_lo = rem * (base + 1_int64) + (me64 - rem - 1_int64) * base + 1_int64
            g_hi = g_lo + base - 1_int64
        end if
    end subroutine local_global_bounds

    !------------------------------------------------------------
    subroutine init(this, cfg)
        class(CoSimpleExchange), intent(out) :: this
        class(AbstractConfig),   intent(in)  :: cfg
        integer(int64) :: i, base, rem
        integer        :: me, nimg

        me   = this_image()
        nimg = num_images()

        select type (cfg)
        type is (Config_SimpleExchange)
            call init_rng(cfg % seed)

            this % total_agents  = cfg % n_agents
            this % n_steps       = cfg % n_steps
            this % write_every   = cfg % write_every
            this % debt_limit    = cfg % debt_limit
            this % exchange_delta = cfg % exchange_delta

            ! Compute this image's slice size using the same formula as
            ! global_to_local so that ownership is consistent
            base = cfg % n_agents / int(nimg, int64)
            rem  = mod(cfg % n_agents, int(nimg, int64))
            if (int(me, int64) <= rem) then
                this % local_n = base + 1_int64
            else
                this % local_n = base
            end if

            allocate(co_pop(this % local_n)[*])
            allocate(incoming_credit(this % local_n)[*])
            do i = 1, this % local_n
                co_pop(i) % cash = cfg % init_cash
            end do
            incoming_credit = 0.0_rk

            allocate(this % names(1))
            this % names(1) = 'gini_coefficient'

        class default
            error stop 'Unsupported config type in CoSimpleExchange init'
        end select

        ! Ensure the distributed population is fully initialized before run()
        ! performs any cross-image reads for output or metrics at timestep 0.
        sync all
    end subroutine init

    !------------------------------------------------------------
    subroutine step(this)
        class(CoSimpleExchange), intent(inout) :: this
        integer :: n_pairs, k, me, nimg, owner_j
        integer, allocatable :: perm(:), pos(:)
        integer(int64) :: global_lo, global_hi, g, local_i, local_j

        me    = this_image()
        nimg  = num_images()
        n_pairs = int(this % total_agents) / 2

        ! All images generate the same permutation (same seed -> same RNG state).
        allocate(perm(int(this % total_agents)))
        allocate(pos(int(this % total_agents)))
        perm = [(k, k = 1, int(this % total_agents))]
        call shuffle_int(perm)
        do k = 1, size(perm)
            pos(perm(k)) = k
        end do

        call local_global_bounds(me, this % total_agents, nimg, global_lo, global_hi)

        ! Clear this image's inbox before other images begin depositing credits.
        incoming_credit = 0.0_rk
        sync all

        ! Only agents owned by this image are inspected as potential payers.
        do local_i = 1, this % local_n
            g = global_lo + local_i - 1_int64
            k = pos(int(g))

            ! Ignore unmatched agents when the population size is odd.
            if (k > 2 * n_pairs) cycle

            ! The agent participates as a payer only when it occupies the odd slot.
            if (mod(k, 2) == 0) cycle

            call global_to_local(int(perm(k + 1), int64), this % total_agents, nimg, owner_j, local_j)

            if (co_pop(local_i) % cash >= -this % debt_limit + this % exchange_delta) then
                co_pop(local_i) % cash = co_pop(local_i) % cash - this % exchange_delta
                if (owner_j == me) then
                    co_pop(local_j) % cash = co_pop(local_j) % cash + this % exchange_delta
                else
                    incoming_credit(local_j)[owner_j] = this % exchange_delta
                end if
            end if
        end do

        ! Ensure all remote credit notifications are visible before consuming them.
        sync all

        do local_i = 1, this % local_n
            co_pop(local_i) % cash = co_pop(local_i) % cash + incoming_credit(local_i)
        end do

        ! Ensure every image has applied its inbox before the next step or output.
        sync all
    end subroutine step

    !------------------------------------------------------------
    ! Only image 1 opens and writes the file; all others return early.
    subroutine write_population_csv(this, filename)
        class(CoSimpleExchange), intent(in) :: this
        character(*), intent(in) :: filename
        integer :: unit, ios, img, nimg
        integer(int64) :: local_i, global_i, owner_local_n, base, rem

        if (this_image() /= 1) return

        nimg = num_images()

        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(*, '(a,a)') 'Error opening file: ', filename
            return
        end if

        write(unit, '(a)') 'agent_id,cash'

        global_i = 0_int64
        base = this % total_agents / int(nimg, int64)
        rem  = mod(this % total_agents, int(nimg, int64))

        do img = 1, nimg
            ! Compute number of agents on image img
            if (int(img, int64) <= rem) then
                owner_local_n = base + 1_int64
            else
                owner_local_n = base
            end if

            do local_i = 1, owner_local_n
                global_i = global_i + 1_int64
                write(unit, '(i0,",",f0.4)') global_i, co_pop(local_i)[img] % cash
            end do
        end do

        close(unit)
        write(*, '(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(CoSimpleExchange), intent(in) :: this
        real(rk), allocatable :: metrics(:)
        allocate(metrics(1))
        metrics(1) = this % gini_coefficient()
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(CoSimpleExchange), intent(in) :: this
        character(len=32), allocatable :: result_names(:)
        integer :: i
        allocate(result_names(size(this % names)))
        do i = 1, size(this % names)
            result_names(i) = this % names(i)
        end do
    end function get_metric_names

    !------------------------------------------------------------
    ! Override base class: only image 1 writes the header
    subroutine co_write_metrics_header(this, filename)
        class(CoSimpleExchange), intent(in) :: this
        character(*), intent(in) :: filename
        character(len=32), allocatable :: names(:)
        integer :: unit, ios, i

        if (this_image() /= 1) return

        names = this % get_metric_names()

        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(*, '(a,a)') 'Error opening metrics file: ', filename
            return
        end if

        write(unit, '(a)', advance='no') 'timestep'
        do i = 1, size(names)
            write(unit, '(a,a)', advance='no') ',', trim(names(i))
        end do
        write(unit, '()')  ! newline

        close(unit)
    end subroutine co_write_metrics_header

    !------------------------------------------------------------
    ! Override base class: only image 1 writes metrics rows.
    ! gini_coefficient gathers data from all images via coarray reads,
    ! which is safe because step's trailing sync all ensures no concurrent writes.
    subroutine co_write_metrics_csv(this, filename, timestep)
        class(CoSimpleExchange), intent(in) :: this
        character(*), intent(in) :: filename
        integer(int64), intent(in) :: timestep
        real(rk), allocatable :: metrics(:)
        integer :: unit, ios, i
        logical :: file_exists

        if (this_image() /= 1) return

        print *, 'Writing metrics for timestep:', timestep, '...'

        metrics = this % compute_metrics()

        inquire(file=filename, exist=file_exists)
        if (file_exists) then
            open(newunit=unit, file=filename, status='old', position='append', action='write', iostat=ios)
        else
            open(newunit=unit, file=filename, status='new', action='write', iostat=ios)
        end if

        if (ios /= 0) then
            write(*, '(a,a)') 'Error opening metrics file: ', filename
            return
        end if

        write(unit, '(i0)', advance='no') timestep
        do i = 1, size(metrics)
            write(unit, '(a,f0.6)', advance='no') ',', metrics(i)
        end do
        write(unit, '()')  ! newline

        close(unit)
        print *, '...done writing metrics.'
    end subroutine co_write_metrics_csv

    !------------------------------------------------------------
    ! Gather all cash values from every image and compute the Gini coefficient.
    ! Called only from image 1 (via write_metrics_csv guard).
    function gini_coefficient(this) result(gini)
        class(CoSimpleExchange), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: cash_list(:)
        integer :: img, nimg
        integer(int64) :: global_i, local_i, owner_local_n, base, rem

        nimg = num_images()
        allocate(cash_list(this % total_agents))

        global_i = 0_int64
        base = this % total_agents / int(nimg, int64)
        rem  = mod(this % total_agents, int(nimg, int64))

        do img = 1, nimg
            if (int(img, int64) <= rem) then
                owner_local_n = base + 1_int64
            else
                owner_local_n = base
            end if

            do local_i = 1, owner_local_n
                global_i = global_i + 1_int64
                cash_list(global_i) = co_pop(local_i)[img] % cash
            end do
        end do

        gini = metric_gini_coefficient(cash_list)
    end function gini_coefficient

end module model_co_simple_exchange_m
