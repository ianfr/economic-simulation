!  /$$$$$$$            /$$   /$$                                                                                  /$$
! | $$__  $$          | $$  | $$                                                                                 |__/
! | $$  \ $$  /$$$$$$ | $$ /$$$$$$  /$$$$$$$$ /$$$$$$/$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$   /$$$$$$  /$$$$$$/$$$$  /$$  /$$$$$$$  /$$$$$$$
! | $$$$$$$  /$$__  $$| $$|_  $$_/ |____ /$$/| $$_  $$_  $$ |____  $$| $$__  $$| $$__  $$ /$$__  $$| $$_  $$_  $$| $$ /$$_____/ /$$_____/
! | $$__  $$| $$  \ $$| $$  | $$      /$$$$/ | $$ \ $$ \ $$  /$$$$$$$| $$  \ $$| $$  \ $$| $$  \ $$| $$ \ $$ \ $$| $$| $$      |  $$$$$$
! | $$  \ $$| $$  | $$| $$  | $$ /$$ /$$__/  | $$ | $$ | $$ /$$__  $$| $$  | $$| $$  | $$| $$  | $$| $$ | $$ | $$| $$| $$       \____  $$
! | $$$$$$$/|  $$$$$$/| $$  |  $$$$//$$$$$$$$| $$ | $$ | $$|  $$$$$$$| $$  | $$| $$  | $$|  $$$$$$/| $$ | $$ | $$| $$|  $$$$$$$ /$$$$$$$/
! |_______/  \______/ |__/   \___/ |________/|__/ |__/ |__/ \_______/|__/  |__/|__/  |__/ \______/ |__/ |__/ |__/|__/ \_______/|_______/

!===============================================================
! model_co_discrete_bouchaud_mezard.f90
! Discrete-time Bouchaud-Mezard model on a network, accelerated
! by coarray Fortran.
!
! Based on:
!   Di Matteo, Aste, and Hyde (2003)
!   "Exchanges in complex networks: income and wealth distributions"
!
! The wealth update equation (Eq. 3 from the paper) is:
!
!   w_l(t+1) = A_l(t) + (1 - q0) * w_l(t) + sum_{j in I_l} (q0 / z_j) * w_j(t)
!
! Where:
!   A_l(t) = additive Gaussian noise with mean 0, std dev sigma0
!   q0     = fraction of wealth each agent distributes to neighbors
!   z_j    = degree (number of neighbors) of agent j
!   I_l    = set of neighbors of agent l
!
! This is compute-bound because each agent must iterate over all its
! neighbors to sum their wealth contributions, weighted by q0/z_j.
! With degree 2k neighbors per agent, that is O(2k) FLOPs per agent
! per step, plus Gaussian noise sampling -- much heavier per-agent
! computation than CoSimpleExchange.
!
! Design notes:
!   Like CoSimpleExchange, Fortran does not permit allocatable coarray
!   components inside a derived type, so the population array lives at
!   module scope as a workaround.
!
!   Parallelism strategy:
!     - Each coimage owns a contiguous slice of agents (co_wealth(:)[*]).
!     - The network adjacency is stored per-agent as a list of global
!       neighbor indices and their degrees.
!     - Each step, every image computes new wealth for its local agents
!       by reading neighbor wealth values from remote images via coarray
!       indexing (one-sided reads).
!     - A sync barrier separates the read phase from the write-back phase
!       to ensure all images see consistent wealth values.
!     - This is SPMD: every image runs the same code on its own data
!       slice, and the heavy per-agent neighborhood summation keeps each
!       image compute-busy between sync points.
!===============================================================
module model_co_discrete_bouchaud_mezard_m
    use kinds_m
    use abstract_config_m
    use config_m
    use rng_m
    use m_random
    use sim_base_m
    use abm_metrics_m
    implicit none
    public ! public by default, Agent below is private

    ! Define the agent privately -- holds only the network topology.
    ! Wealth is stored separately in the coarray for distributed access.
    private :: Agent
    type :: Agent
        integer, allocatable :: neighbors(:)   ! Global indices of neighbors
        integer, allocatable :: neighbor_deg(:) ! Degree of each neighbor (z_j)
        integer :: degree = 0                  ! This agent's own degree (z_l)
    end type Agent

    ! Module-level coarrays -- workaround for the restriction that allocatable
    ! coarrays cannot be components of a derived type.
    ! co_wealth holds current wealth; co_wealth_new holds the updated values
    ! computed during a step before they are swapped in.
    real(rk), allocatable :: co_wealth(:)[:]
    real(rk), allocatable :: co_wealth_new(:)[:]

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: CoDiscreteBouchaudMezard
        integer(int64) :: total_agents = 0
        integer(int64) :: local_n = 0       ! agents owned by this image
        type(Agent), allocatable :: agents(:) ! local agent topology (not coarray)
        character(len=32), allocatable :: names(:)
        real(rk) :: q0 = 0.1_rk             ! fraction distributed to neighbors
        real(rk) :: sigma0 = 5.0_rk         ! std dev of additive Gaussian noise
        integer  :: k = 3                    ! half-degree of lattice (degree = 2k)
        real(rk) :: rewiring_probability = 0.0_rk
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
        procedure, private :: init_network
    end type CoDiscreteBouchaudMezard

contains

    !------------------------------------------------------------
    ! Map a global agent index g (1-based) to (image, local index).
    ! Agents are distributed so the first rem images hold (base+1)
    ! agents and the remaining (nimg-rem) images hold base agents.
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
    ! Build the network topology for agents owned by this image.
    ! Each agent starts on a 1D ring lattice with 2k neighbors,
    ! then connections are rewired with the given probability.
    ! All images must build the SAME global network so that neighbor
    ! references are consistent -- so all use the same RNG seed.
    subroutine init_network(this)
        class(CoDiscreteBouchaudMezard), intent(inout) :: this
        integer(int64) :: g_lo, g_hi, i_local, g, n_total
        integer :: me, nimg, j, deg, n_int, new_neighbor
        integer, allocatable :: global_neighbors(:,:)
        integer, allocatable :: global_degree(:)
        real(rk) :: u

        me   = this_image()
        nimg = num_images()
        n_total = this % total_agents
        n_int   = int(n_total)

        call local_global_bounds(me, n_total, nimg, g_lo, g_hi)

        deg = 2 * this % k

        ! Build the full global neighbor list so all images agree on topology.
        ! For large agent counts this could be distributed, but for correctness
        ! of the rewiring procedure we need a consistent global view.
        allocate(global_neighbors(deg, n_int))
        allocate(global_degree(n_int))

        ! Step 1: Build regular ring lattice neighbors for every global agent
        do g = 1, n_int
            do j = 1, this % k
                ! Left neighbors
                global_neighbors(j, int(g)) = mod(int(g) - j - 1 + n_int, n_int) + 1
                ! Right neighbors
                global_neighbors(this % k + j, int(g)) = mod(int(g) + j - 1, n_int) + 1
            end do
        end do

        ! Step 2: Rewire each edge with probability rewiring_probability
        ! Uses the Fortran intrinsic RNG (already seeded identically on all images)
        if (this % rewiring_probability > 0.0_rk) then
            do g = 1, n_int
                do j = 1, deg
                    call random_number(u)
                    if (u < this % rewiring_probability) then
                        ! Find a valid new neighbor: not self, not already connected
                        rewire_loop: do
                            call random_number(u)
                            new_neighbor = 1 + int(u * (n_int - 1))
                            if (new_neighbor >= int(g)) new_neighbor = new_neighbor + 1
                            ! Check it is not already a neighbor
                            if (.not. any(global_neighbors(:, int(g)) == new_neighbor)) then
                                global_neighbors(j, int(g)) = new_neighbor
                                exit rewire_loop
                            end if
                        end do rewire_loop
                    end if
                end do
            end do
        end if

        ! Step 3: Compute degree of every global agent
        ! In a regular lattice all agents have degree 2k, but rewiring can
        ! create asymmetric edges. We count in-degree (how many agents list
        ! this agent as a neighbor) since that determines how much wealth
        ! flows out of each agent in the Di Matteo model.
        ! For simplicity and consistency with the paper (which assumes
        ! undirected networks), we use the fixed degree 2k for all agents.
        global_degree(:) = deg

        ! Step 4: Extract only the local agents' neighbor data
        allocate(this % agents(this % local_n))
        do i_local = 1, this % local_n
            g = g_lo + i_local - 1_int64
            allocate(this % agents(i_local) % neighbors(deg))
            allocate(this % agents(i_local) % neighbor_deg(deg))
            this % agents(i_local) % degree = deg
            this % agents(i_local) % neighbors(:) = global_neighbors(:, int(g))
            ! Store each neighbor's degree for the q0/z_j weighting
            do j = 1, deg
                this % agents(i_local) % neighbor_deg(j) = &
                    global_degree(global_neighbors(j, int(g)))
            end do
        end do

        deallocate(global_neighbors, global_degree)
    end subroutine init_network

    !------------------------------------------------------------
    subroutine init(this, cfg)
        class(CoDiscreteBouchaudMezard), intent(out) :: this
        class(AbstractConfig),          intent(in)  :: cfg
        integer(int64) :: i, base, rem
        integer        :: me, nimg

        me   = this_image()
        nimg = num_images()

        select type (cfg)
        type is (Config_CoDiscreteBouchaudMezard)
            ! Seed the intrinsic RNG identically on all images so that
            ! every image generates the same global network topology.
            call init_rng(cfg % seed)

            ! Also initialize the xoroshiro128+ RNG for Gaussian noise.
            ! Each image gets a different stream by jumping.
            call this % rng % set_seed([cfg % seed, cfg % seed + 1_int64])
            do i = 1, int(me, int64) - 1_int64
                call this % rng % jump()
            end do

            this % total_agents       = cfg % n_agents
            this % n_steps            = cfg % n_steps
            this % write_every        = cfg % write_every
            this % q0                 = cfg % q0
            this % sigma0             = cfg % sigma0
            this % k                  = cfg % k
            this % rewiring_probability = cfg % rewiring_probability

            ! Compute this image's slice size
            base = cfg % n_agents / int(nimg, int64)
            rem  = mod(cfg % n_agents, int(nimg, int64))
            if (int(me, int64) <= rem) then
                this % local_n = base + 1_int64
            else
                this % local_n = base
            end if

            ! Allocate coarray wealth arrays
            allocate(co_wealth(this % local_n)[*])
            allocate(co_wealth_new(this % local_n)[*])

            ! Initialize all agents with equal wealth
            do i = 1, this % local_n
                co_wealth(i) = cfg % init_wealth
            end do
            co_wealth_new = 0.0_rk

            ! Build the network topology
            call this % init_network()

            ! Set up metric names
            allocate(this % names(1))
            this % names(1) = 'gini_coefficient'

        class default
            error stop 'Unsupported config type in CoDiscreteBouchaudMezard init'
        end select

        ! Ensure the distributed population is fully initialized before
        ! run() performs any cross-image reads for output or metrics.
        sync all
    end subroutine init

    !------------------------------------------------------------
    ! One timestep of the discrete BM model.
    !
    ! For each local agent l with global neighbors {j}:
    !   w_l(t+1) = A_l(t) + (1 - q0) * w_l(t)
    !              + sum_{j in I_l} (q0 / z_j) * w_j(t)
    !
    ! The computation is done in two phases:
    !   1. Compute new wealth into co_wealth_new using reads from
    !      co_wealth on local and remote images (one-sided coarray reads).
    !   2. After a sync barrier, copy co_wealth_new into co_wealth.
    subroutine step(this)
        class(CoDiscreteBouchaudMezard), intent(inout) :: this
        integer(int64) :: i_local, g_lo, g_hi
        integer :: me, nimg, j, owner_img, deg
        integer(int64) :: owner_local_idx
        real(rk) :: noise, neighbor_sum, w_neighbor, q0

        me   = this_image()
        nimg = num_images()
        q0   = this % q0

        call local_global_bounds(me, this % total_agents, nimg, g_lo, g_hi)

        ! Phase 1: Compute new wealth for each local agent
        ! This is the compute-heavy phase -- each agent iterates over
        ! all its neighbors, reading their wealth via coarray indexing.
        do i_local = 1, this % local_n
            deg = this % agents(i_local) % degree

            ! Accumulate weighted neighbor wealth contributions
            ! sum_{j in I_l} (q0 / z_j) * w_j(t)
            neighbor_sum = 0.0_rk
            do j = 1, deg
                ! Look up where this neighbor lives in the coarray
                call global_to_local( &
                    int(this % agents(i_local) % neighbors(j), int64), &
                    this % total_agents, nimg, owner_img, owner_local_idx)

                ! One-sided coarray read: fetch neighbor's current wealth
                w_neighbor = co_wealth(owner_local_idx)[owner_img]

                ! Weight by q0 / z_j (neighbor's degree)
                neighbor_sum = neighbor_sum + &
                    (q0 / real(this % agents(i_local) % neighbor_deg(j), rk)) * w_neighbor
            end do

            ! Sample additive Gaussian noise A_l(t) ~ N(0, sigma0^2)
            noise = this % rng % normal() * this % sigma0

            ! Apply the Di Matteo et al. wealth update equation (Eq. 3)
            co_wealth_new(i_local) = noise &
                + (1.0_rk - q0) * co_wealth(i_local) &
                + neighbor_sum
        end do

        ! Phase 2: All images must finish computing before anyone overwrites
        ! co_wealth, since remote reads in Phase 1 depend on the old values.
        sync all

        ! Swap new wealth values into the live array
        do i_local = 1, this % local_n
            co_wealth(i_local) = co_wealth_new(i_local)
        end do

        ! Ensure all images have updated before the next step or output
        sync all
    end subroutine step

    !------------------------------------------------------------
    ! Only image 1 opens and writes the file; all others return early.
    subroutine write_population_csv(this, filename)
        class(CoDiscreteBouchaudMezard), intent(in) :: this
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

        write(unit, '(a)') 'agent_id,wealth'

        global_i = 0_int64
        base = this % total_agents / int(nimg, int64)
        rem  = mod(this % total_agents, int(nimg, int64))

        do img = 1, nimg
            ! Compute number of agents on this image
            if (int(img, int64) <= rem) then
                owner_local_n = base + 1_int64
            else
                owner_local_n = base
            end if

            do local_i = 1, owner_local_n
                global_i = global_i + 1_int64
                write(unit, '(i0,",",f0.4)') global_i, co_wealth(local_i)[img]
            end do
        end do

        close(unit)
        write(*, '(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(CoDiscreteBouchaudMezard), intent(in) :: this
        real(rk), allocatable :: metrics(:)
        allocate(metrics(1))
        metrics(1) = this % gini_coefficient()
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(CoDiscreteBouchaudMezard), intent(in) :: this
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
        class(CoDiscreteBouchaudMezard), intent(in) :: this
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
    subroutine co_write_metrics_csv(this, filename, timestep)
        class(CoDiscreteBouchaudMezard), intent(in) :: this
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
            open(newunit=unit, file=filename, status='old', position='append', &
                 action='write', iostat=ios)
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
    ! Gather all wealth values from every image and compute the
    ! Gini coefficient. Called only from image 1.
    function gini_coefficient(this) result(gini)
        class(CoDiscreteBouchaudMezard), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: wealth_list(:)
        integer :: img, nimg
        integer(int64) :: global_i, local_i, owner_local_n, base, rem

        nimg = num_images()
        allocate(wealth_list(this % total_agents))

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
                wealth_list(global_i) = co_wealth(local_i)[img]
            end do
        end do

        gini = metric_gini_coefficient(wealth_list)
    end function gini_coefficient

end module model_co_discrete_bouchaud_mezard_m
