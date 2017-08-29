   
   module module_trajectory

   use module_driver_constants,  only : max_domains
#if( EM_CORE == 1 )
   use module_state_description, only : num_chem
#endif
#if( NMM_CORE == 1 )
   CONTAINS

   subroutine trajectory_init( grid, config_flags, &
                               ims,ime, jms,jme, kms,kme )

   use module_domain
   use module_configure,         only : grid_config_rec_type

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   integer, intent(in)      :: ims,ime, jms,jme, kms,kme
   type(domain), intent(inout)            :: grid
   type(grid_config_rec_type), intent(in) :: config_flags

   end subroutine trajectory_init

   subroutine trajectory_driver( grid )

   use module_domain

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   type(domain), intent(in) :: grid

   end subroutine trajectory_driver

#elif( EM_CORE == 1 )

   implicit none

   private
   public :: trajectory_init
#ifdef NETCDF
   public :: trajectory_driver
   public :: trajectory_dchm_tstep_init
   public :: trajectory_dchm_tstep_set
#endif
   public :: traject
   public :: traj_cnt

   integer, parameter :: vals_max = 1000
   integer, parameter :: traj_max = 1000
   integer, parameter :: var_max  = 100
   integer, parameter :: pkg_max  = 6
   integer, parameter :: dyn_max  = 8
   integer, parameter :: chm_pkg  = 1
   integer, parameter :: hyd_pkg  = 2
   integer, parameter :: trc_pkg  = 3
   integer, parameter :: dyn_pkg  = 4
   integer, parameter :: msc_pkg  = 5
   integer, parameter :: dchm_pkg = 6
   real, parameter    :: missing_val = -9999.
   real, parameter    :: zero_val    = 0.
   
   integer :: n_dom        ! domain count
   integer, pointer :: n_vals                         ! wrking count of time points in buffer
   integer, target  :: n_vals_dm(max_domains)         ! count of time points in buffer per domain
   integer, target  :: n_dchm_dm(max_domains)         ! count of dchm variables in  buffer per domain
   integer, allocatable, target :: num_msc_dm(:)      ! number misc species
   integer, allocatable, target :: dchm_buf_ndx_dm(:,:)  ! dchm buffer indices
   integer, pointer :: dchm_buf_ndx(:)
   integer :: offset       ! into variable arrays

   type default
     character(len=19) :: start_time
     character(len=19) :: stop_time
     character(len=32) :: chm_name(var_max)
     character(len=32) :: hyd_name(var_max)
     character(len=32) :: trc_name(var_max)
     character(len=32) :: dyn_name(var_max)
     character(len=32) :: msc_name(var_max)
     character(len=32) :: dchm_name(var_max)
   end type default

   type base
     real    :: lon
     real    :: lat
     real    :: lev
     real    :: x, y
     real    :: traj_var(var_max)
     integer :: n_chm_var
     integer :: n_ct_var
     integer :: n_hyd_var
     integer :: n_trc_var
     integer :: n_dyn_var
     integer :: n_msc_var
     integer :: n_dchm_var
     integer :: chm_ndx(var_max)
     integer :: hyd_ndx(var_max)
     integer :: trc_ndx(var_max)
     integer :: dyn_ndx(var_max)
     integer :: msc_ndx(var_max)
     integer :: dchm_ndx(var_max)
     character(len=19) :: start_time
     character(len=19) :: stop_time
     character(len=32) :: chm_spc(var_max)
     character(len=32) :: hyd_spc(var_max)
     character(len=32) :: trc_spc(var_max)
     character(len=32) :: dyn_var(var_max)
     character(len=32) :: msc_var(var_max)
     character(len=32) :: dchm_spc(var_max)
     logical :: in_dom
     logical :: in_patch
     logical :: is_stationary
     logical :: z_is_agl
     logical :: chm_is_gas(var_max)
   end type base

   type buffer
     real    :: trj_i(vals_max)
     real    :: trj_j(vals_max)
     real    :: trj_k(vals_max)
     real    :: trj_lons(vals_max)
     real    :: trj_lats(vals_max)
     real, allocatable :: chm_vals(:,:)
     real, allocatable :: trc_vals(:,:)
     real, allocatable :: hyd_vals(:,:)
     real, allocatable :: dyn_vals(:,:)
     real, allocatable :: msc_vals(:,:)
     real, allocatable :: dchm_vals(:,:)
     character(len=19) :: times(vals_max)
   end type buffer

   type statevar
     character(len=80) :: Varname
     character(len=80) :: Description
     character(len=80) :: Units
     character(len=10) :: MemOrd
     character(len=10) :: Stagger
     integer           :: Ndim
     real, pointer     :: rfield_2d(:,:)
     real, pointer     :: rfield_3d(:,:,:)
   end type statevar

   integer,    allocatable           :: traj_cnt(:)
   type(base), allocatable, target   :: traject(:,:)
   type(base), pointer               :: trjects(:)
   type(buffer), allocatable, target :: trj_buff(:,:)
   type(buffer), pointer             :: trj_pbf(:)
   type(statevar), allocatable, target :: St_Vars_dm(:,:)
   type(statevar), pointer             :: St_Vars(:)

   real, allocatable  :: dchm_buff(:,:,:,:)

   character(len=256) :: dyn_var_lst(dyn_max)
   character(len=32)  :: dyn_var_desc_att(dyn_max)
   character(len=32)  :: dyn_var_unit_att(dyn_max)
   character(len=4)   :: pkg_tag(pkg_max) = (/ 'chm ', 'hyd ', 'trc ', 'dyn ', 'msc ', 'dchm' /)

   logical                      :: do_chemstep
   logical, allocatable, target :: pkg_has_vars_dm(:,:,:)
   logical, pointer             :: pkg_has_vars(:,:)
   logical, allocatable, target :: trj_msk_dm(:,:,:,:)
   logical, pointer             :: trj_msk(:,:)
   logical, allocatable, target :: St_Vars_msk_dm(:,:)
   logical, pointer             :: St_Vars_msk(:)
   logical, allocatable, target :: dchm_msk_dm(:,:)
   logical, pointer             :: dchm_msk(:)
   logical, allocatable         :: is_initialized(:)
   logical, allocatable         :: dm_has_traj(:)

   CONTAINS

   subroutine trajectory_init( grid, config_flags, &
                               ims,ime, jms,jme, kms,kme )

   use module_domain
   use module_llxy,              only : proj_info, latlon_to_ij
   use module_configure,         only : grid_config_rec_type
   use module_state_description, only : no_trajectory, param_first_scalar, num_chem, num_moist, num_tracer
   use module_scalar_tables,     only : chem_dname_table, moist_dname_table, tracer_dname_table
   use module_model_constants,   only : g
   use module_domain_type,       only : fieldlist
#ifdef DM_PARALLEL
   use module_dm,                only : wrf_dm_sum_integer, wrf_dm_max_real
#endif

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   integer, intent(in)      :: ims,ime, jms,jme, kms,kme
   type(domain), intent(inout)            :: grid
   type(grid_config_rec_type), intent(in) :: config_flags

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: astat
   integer :: dm
   integer, pointer :: num_msc      ! number misc species
   integer, pointer :: n_dchm       ! number dchm buffer species
   integer :: ierr, ios
   integer :: i, j, k, k1, m, m1, m2
   integer :: i_end, j_end, u_lim
   integer :: n, pkg, trj
   integer :: n_traj, n_traj_1
   integer :: p_size
   integer :: unitno
   integer :: ids,ide, jds,jde, kds,kde
   integer :: ips,ipe, jps,jpe, kps,kpe
   integer :: dbg_lvl
   integer :: target
   integer :: n_def_var(pkg_max)
   real    :: x, y
   real    :: z_dm_bot, z_dm_top
   real    :: z(kms:kme-1)
   real    :: z_at_w(ims:ime,kms:kme,jms:jme)
   character(len=256), allocatable :: msc_tbl(:)
   character(len=32)  :: var_name
   character(len=128) :: filename
   character(len=256) :: err_mes
   character(len=32)  :: wrk_var_name
   character(len=32)  :: wrk_chr(var_max)
   character(len=32)  :: wrk_def_name(var_max)
   logical :: exists
   logical :: is_root_proc
   logical :: rstrt
   logical :: mask(var_max)
   logical :: trj_mask(traj_max)

   type(grid_config_rec_type) :: config_temp
   type(proj_info)            :: proj

   type(default) :: traj_def
   type(base)    :: traj_type(traj_max)

   logical, external :: wrf_dm_on_monitor
   integer, external :: get_unused_unit

   namelist / traj_spec /    traj_type
   namelist / traj_default / traj_def

#ifndef NETCDF
   call wrf_error_fatal( 'trajectory_init: requires netcdf' )
#endif

   offset = param_first_scalar - 1

   if( .not. allocated( dm_has_traj ) ) then
     call nl_get_max_dom( 1,n_dom )
     allocate( dm_has_traj(n_dom),traj_cnt(n_dom),stat=astat )
     if( astat /= 0 ) then
       write(err_mes,'(''trajectory_init('',i2.2,''): failed to allocate dm_has_traj,traj_cnt; error = '',i6)') dm,astat
       call wrf_error_fatal( trim( err_mes  ) )
     endif
     traj_cnt(:) = 0
   endif

!-----------------------------------------------------------------------------
!  check for trajectory option
!-----------------------------------------------------------------------------
   if( grid%traj_opt == no_trajectory ) then
     write(err_mes,'(''trajectory_init('',i2.2,''): traj_opt = no_trajectory'')') grid%id
     call wrf_message( trim(err_mes) )
     dm_has_traj(:) = .false.
     return
   endif

!-----------------------------------------------------------------------------
!  domain requests trajectories
!-----------------------------------------------------------------------------
   dm = grid%id
   if( .not. config_flags%dm_has_traj ) then
     write(err_mes,'(''trajectory_init('',i2.2,''): no trajectory calculation for domain '',i2.2)') dm,dm
     call wrf_message( trim( err_mes ) )
     dm_has_traj(dm) = .false.
     return
   else
     dm_has_traj(dm) = .true.
   endif

!-----------------------------------------------------------------------------
!  set domain count, restarting?
!-----------------------------------------------------------------------------
   call nl_get_restart( dm,rstrt )
   if( .not. allocated( traject ) ) then
!-----------------------------------------------------------------------------
!  allocate module variables
!-----------------------------------------------------------------------------
     allocate( traject(traj_max,n_dom), &
               pkg_has_vars_dm(traj_max,pkg_max,n_dom), &
               num_msc_dm(n_dom), is_initialized(n_dom),stat=astat )
     if( astat /= 0 ) then
       write(err_mes,'(''trajectory_init('',i2.2,''): failed to allocate traject...num_msc_dm; error = '',i6)') dm,astat
       call wrf_error_fatal( trim( err_mes  ) )
     endif
     is_initialized(:) = .false.
   endif

   if( is_initialized(dm) ) then
     return
   endif

   trjects => traject(:,dm)

   call get_ijk_from_grid( grid ,                   &
                           ids, ide, jds, jde, kds, kde, &
                           n,n,  n,n, n,n,          &
                           ips, ipe, jps, jpe, kps, kpe    )

   n_vals      => n_vals_dm(dm)
   n_vals      = 0
   is_root_proc = wrf_dm_on_monitor()
   pkg_has_vars => pkg_has_vars_dm(:,:,dm)

   dyn_var_lst(1:dyn_max)      = (/ 'p       ', 'T       ', 'z       ', 'u       ', &
                                    'v       ', 'w       ', 'rainprod', 'evapprod' /)
   dyn_var_unit_att(1:dyn_max) = (/ 'hPa', 'K  ', 'm  ', 'm/s', 'm/s', 'm/s', 's-1', 's-1' /)
   dyn_var_desc_att(1:dyn_max) = (/ 'pressure            ', 'temperature         ', 'height              ', &
                                    'x wind component    ', 'y wind component    ', 'z wind component    ', &
                                    'rain production rate', 'rain evap rate      ' /)
!-----------------------------------------------------------------------------
!  master proc
!-----------------------------------------------------------------------------
!master_proc: &
!   if( is_root_proc ) then
      write(filename,'(''wrfinput_traj_d'',i2.2)',iostat=ios) dm
      if( ios /= 0 ) then
        write(err_mes,'(''trajectory_init('',i2.2,''): failed to set filename: error = '',i6)') dm,ios
        call wrf_error_fatal( trim( err_mes  ) )
      endif
      inquire( file=trim(filename),exist=exists )
input_file: &
      if( exists ) then
        unitno = get_unused_unit()
        if( unitno <= 0 ) then
          call wrf_error_fatal( 'trajectory_init: failed to get unit number' )
        endif
!-----------------------------------------------------------------------------
!  open file
!-----------------------------------------------------------------------------
        open( unit = unitno,file=trim(filename),iostat=ios )
        if( ios /= 0 ) then
          write(err_mes,'(''trajectory_init('',i2.2,''): failed to open '',a,''; error = '',i6)') dm,trim(filename),ios
          call wrf_error_fatal( trim( err_mes  ) )
        endif
!-----------------------------------------------------------------------------
!  initialize trajectories
!-----------------------------------------------------------------------------
        traj_def%start_time = ' '
        traj_def%stop_time  = ' '
        traj_def%chm_name(:) = ' '
        traj_def%hyd_name(:) = ' '
        traj_def%trc_name(:) = ' '
        traj_def%dyn_name(:) = ' '
        traj_def%msc_name(:) = ' '
        traj_def%dchm_name(:) = ' '

        do trj = 1,traj_max
          traj_type(trj)%start_time = ' '
          traj_type(trj)%stop_time  = ' '
          traj_type(trj)%chm_spc(:) = ' '
          traj_type(trj)%dchm_spc(:) = ' '
          traj_type(trj)%hyd_spc(:) = ' '
          traj_type(trj)%trc_spc(:) = ' '
          traj_type(trj)%dyn_var(:) = ' '
          traj_type(trj)%msc_var(:) = ' '
          traj_type(trj)%chm_ndx(:) = 0
          traj_type(trj)%hyd_ndx(:) = 0
          traj_type(trj)%trc_ndx(:) = 0
          traj_type(trj)%dyn_ndx(:) = 0
          traj_type(trj)%msc_ndx(:) = 0
          traj_type(trj)%dchm_ndx(:) = 0
          traj_type(trj)%n_chm_var = 0 ; traj_type(trj)%n_ct_var = 0
          traj_type(trj)%n_hyd_var = 0 ; traj_type(trj)%n_trc_var = 0 
          traj_type(trj)%n_dyn_var = 0 ; traj_type(trj)%n_msc_var = 0
          traj_type(trj)%n_dchm_var = 0
          traj_type(trj)%is_stationary = .false.
          traj_type(trj)%chm_is_gas(:) = .true.
          traj_type(trj)%z_is_agl      = .true.
          traj_type(trj)%lon           = missing_val
          traj_type(trj)%lat           = missing_val
          traj_type(trj)%lev           = missing_val
        end do
!-----------------------------------------------------------------------------
!  read file
!-----------------------------------------------------------------------------
        read(unit=unitno,nml=traj_default,iostat=ios)
        if( ios /= 0 ) then
          close( unit=unitno )
          write(err_mes,'(''trajectory_init('',i2.2,''): failed to read '',a,''; error = '',i6)') dm,trim(filename),ios
          call wrf_error_fatal( trim( err_mes  ) )
        endif
        read(unit=unitno,nml=traj_spec,iostat=ios)
        if( ios /= 0 ) then
          close( unit=unitno )
          write(err_mes,'(''trajectory_init('',i2.2,''): failed to read '',a,''; error = '',i6)') dm,trim(filename),ios
          call wrf_error_fatal( trim( err_mes  ) )
        endif
        close( unit=unitno )
      else input_file
        write(err_mes,'(''trajectory_init('',i2.2,''): no '',a,'' file'')') dm,trim(filename)
        call wrf_message( trim( err_mes ) )
        traj_cnt(dm) = 0
        return
      endif input_file

!-----------------------------------------------------------------------------
!  process the namelist input
!-----------------------------------------------------------------------------
      do pkg = 1,pkg_max
        select case( trim(pkg_tag(pkg)) )
          case( 'chm' )
            wrk_def_name(:) = traj_def%chm_name(:)
          case( 'dchm' )
            wrk_def_name(:) = traj_def%dchm_name(:)
          case( 'hyd' )
            wrk_def_name(:) = traj_def%hyd_name(:)
          case( 'trc' )
            wrk_def_name(:) = traj_def%trc_name(:)
          case( 'dyn' )
            wrk_def_name(:) = traj_def%dyn_name(:)
          case( 'msc' )
            wrk_def_name(:) = traj_def%msc_name(:)
        end select
        do m = 1,var_max
          if( wrk_def_name(m) == ' ' ) then
            exit
          endif
        end do
        n_def_var(pkg) = m - 1
      end do

      if( traj_def%start_time /= ' ' ) then
        write(err_mes,'(''trajectory_init('',i2.2,''): default start time = '',a)') dm,traj_def%start_time
        call wrf_message( trim(err_mes) )
      endif
      if( traj_def%stop_time /= ' ' ) then
        write(err_mes,'(''trajectory_init('',i2.2,''): default stop  time = '',a)') dm,traj_def%stop_time
        call wrf_message( trim(err_mes) )
      endif

      do pkg = 1,pkg_max
        if( n_def_var(pkg) > 0 ) then
          write(*,*) ' '
          write(*,'(''trajectory_init('',i2.2,''): default '',a,'' variables'')') dm,pkg_tag(pkg)
          select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
            case( 'chm' )
              wrk_def_name(:) = traj_def%chm_name(:)
            case( 'dchm' )
              wrk_def_name(:) = traj_def%dchm_name(:)
#endif
            case( 'hyd' )
              wrk_def_name(:) = traj_def%hyd_name(:)
            case( 'trc' )
              wrk_def_name(:) = traj_def%trc_name(:)
            case( 'dyn' )
              wrk_def_name(:) = traj_def%dyn_name(:)
            case( 'msc' )
              wrk_def_name(:) = traj_def%msc_name(:)
          end select
          write(*,*) wrk_def_name(:n_def_var(pkg))
        endif
      end do

      do n_traj = 1,traj_max
        if( traj_type(n_traj)%lon == missing_val .or. &
            traj_type(n_traj)%lat == missing_val .or. &
            traj_type(n_traj)%lev == missing_val ) then
          exit
        endif
      end do
      n_traj = n_traj - 1

has_trajectories: &
      if( n_traj > 0 ) then
!-----------------------------------------------------------------------------
!  set individual trajectories to default if specified
!-----------------------------------------------------------------------------
        if( traj_def%start_time /= ' ' ) then
          do trj = 1,n_traj
            if( traj_type(trj)%start_time == ' ' ) then
              traj_type(trj)%start_time = traj_def%start_time
            endif
          end do
        endif
        if( traj_def%stop_time /= ' ' ) then
          do trj = 1,n_traj
            if( traj_type(trj)%stop_time == ' ' ) then
              traj_type(trj)%stop_time = traj_def%stop_time
            endif
          end do
        endif

        do pkg = 1,pkg_max
          select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
            case( 'chm' )
              wrk_def_name(:) = traj_def%chm_name(:)
            case( 'dchm' )
              wrk_def_name(:) = traj_def%dchm_name(:)
#endif
            case( 'hyd' )
              wrk_def_name(:) = traj_def%hyd_name(:)
            case( 'trc' )
              wrk_def_name(:) = traj_def%trc_name(:)
            case( 'dyn' )
              wrk_def_name(:) = traj_def%dyn_name(:)
            case( 'msc' )
              wrk_def_name(:) = traj_def%msc_name(:)
          end select
          do trj = 1,n_traj
            select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
              case( 'chm' )
                wrk_var_name = traj_type(trj)%chm_spc(1)
              case( 'dchm' )
                wrk_var_name = traj_type(trj)%dchm_spc(1)
#endif
              case( 'hyd' )
                wrk_var_name = traj_type(trj)%hyd_spc(1)
              case( 'trc' )
                wrk_var_name = traj_type(trj)%trc_spc(1)
              case( 'dyn' )
                wrk_var_name = traj_type(trj)%dyn_var(1)
              case( 'msc' )
                wrk_var_name = traj_type(trj)%msc_var(1)
            end select
            if( wrk_var_name == ' ' ) then
              m1 = n_def_var(pkg)
              select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
                case( 'chm' )
                  traj_type(trj)%chm_spc(:m1) = traj_def%chm_name(:m1)
                case( 'dchm' )
                  traj_type(trj)%dchm_spc(:m1) = traj_def%dchm_name(:m1)
#endif
                case( 'hyd' )
                  traj_type(trj)%hyd_spc(:m1) = traj_def%hyd_name(:m1)
                case( 'trc' )
                  traj_type(trj)%trc_spc(:m1) = traj_def%trc_name(:m1)
                case( 'dyn' )
                  traj_type(trj)%dyn_var(:m1) = traj_def%dyn_name(:m1)
                case( 'msc' )
                  traj_type(trj)%msc_var(:m1) = traj_def%msc_name(:m1)
              end select
            endif
          end do
        end do
!-----------------------------------------------------------------------------
!  scan registry real, 2d, 3d variables
!-----------------------------------------------------------------------------
        call reg_scan( grid )
        num_msc => num_msc_dm(dm)

        call get_wrf_debug_level( dbg_lvl )
        if( dbg_lvl > 200 ) then
          write(*,*) ' '
          write(*,'(''trajectory_init('',i2.2,''): Registry 2d,3d variables'')') dm
          do n = 1,num_msc
            write(*,*) trim(St_Vars(n)%varname)
          end do
          n = count( St_Vars(:num_msc)%Stagger == 'X' )
          if( n > 0 ) then
            write(*,*) ' '
            write(*,*) 'Registry 2d,3d variables with staggered X'
            do n = 1,num_msc
              if( St_Vars(n)%Stagger == 'X' ) then
                write(*,*) trim(St_Vars(n)%varname)
              endif
            end do
          endif
          n = count( St_Vars(:num_msc)%Stagger == 'Y' )
          if( n > 0 ) then
            write(*,*) ' '
            write(*,*) 'trajectory_init: Registry 2d,3d variables with staggered Y'
            do n = 1,num_msc
              if( St_Vars(n)%Stagger == 'Y' ) then
                write(*,*) trim(St_Vars(n)%varname)
              endif
            end do
          endif
          n = count( St_Vars(:num_msc)%Stagger == 'Z' )
          if( n > 0 ) then
            write(*,*) ' '
            write(*,*) 'trajectory_init: Registry 2d,3d variables with staggered Z'
            do n = 1,num_msc
              if( St_Vars(n)%Stagger == 'Z' ) then
                write(*,*) trim(St_Vars(n)%varname)
              endif
            end do
          endif
        endif
!-----------------------------------------------------------------------------
!  get variable counts
!-----------------------------------------------------------------------------
        do trj = 1,n_traj
          if( num_chem > 1 ) then
#if( WRF_CHEM == 1 )
            call get_var_cnt( traj_type(trj)%n_chm_var, traj_type(trj)%chm_spc )
            call get_var_cnt( traj_type(trj)%n_dchm_var, traj_type(trj)%dchm_spc )
#endif
          else
            traj_type(trj)%n_chm_var  = 0
            traj_type(trj)%n_dchm_var = 0
          endif
          if( num_moist > 1 ) then
            call get_var_cnt( traj_type(trj)%n_hyd_var, traj_type(trj)%hyd_spc )
          else
            traj_type(trj)%n_hyd_var = 0
          endif
          if( num_tracer > 1 ) then
            call get_var_cnt( traj_type(trj)%n_trc_var, traj_type(trj)%trc_spc )
          else
            traj_type(trj)%n_trc_var = 0
          endif
          if( num_msc > 1 ) then
            call get_var_cnt( traj_type(trj)%n_msc_var, traj_type(trj)%msc_var )
          else
            traj_type(trj)%n_msc_var = 0
          endif
          call get_var_cnt( traj_type(trj)%n_dyn_var, traj_type(trj)%dyn_var )
        end do

        if( any( traj_type(:n_traj)%n_msc_var > 0 ) ) then
          allocate( msc_tbl(num_msc),stat=astat )
          if( astat /= 0 ) then
            call wrf_error_fatal( 'trajectory_init: failed to find allocate msc_tbl' )
          endif
          do m = 1,num_msc
            msc_tbl(m) = trim( St_Vars(m)%Varname )
          end do
        endif
!-----------------------------------------------------------------------------
!  check for trajectory variables in simulation
!-----------------------------------------------------------------------------
        do trj = 1,n_traj
#if( WRF_CHEM == 1 )
          if( num_chem > 1 ) then
            if( traj_type(trj)%n_chm_var > 0 ) then
              mask(:) = .false.
              call scan_vars( traj_type(trj)%n_chm_var, traj_type(trj)%chm_spc, traj_type(trj)%chm_ndx, &
                              num_chem, chem_dname_table(dm,:), &
                              traj_type(trj)%chm_is_gas, 'chm' )
            endif
            if( traj_type(trj)%n_dchm_var > 0 ) then
              mask(:) = .false.
              call scan_vars( traj_type(trj)%n_dchm_var, traj_type(trj)%dchm_spc, traj_type(trj)%dchm_ndx, &
                              num_chem, chem_dname_table(dm,:), &
                              traj_type(trj)%chm_is_gas, 'chm' )
            endif
          endif
#endif
          if( traj_type(trj)%n_hyd_var > 0 .and. num_moist > 1 ) then
            mask(:) = .false.
            call scan_vars( traj_type(trj)%n_hyd_var, traj_type(trj)%hyd_spc, traj_type(trj)%hyd_ndx, &
                            num_moist, moist_dname_table(dm,:), &
                            traj_type(trj)%chm_is_gas, 'hyd' )
          endif
          if( traj_type(trj)%n_trc_var > 0 .and. num_tracer > 1 ) then
            mask(:) = .false.
            call scan_vars( traj_type(trj)%n_trc_var, traj_type(trj)%trc_spc, traj_type(trj)%trc_ndx, &
                            num_tracer, tracer_dname_table(dm,:), &
                            traj_type(trj)%chm_is_gas, 'trc' )
          endif
          if( traj_type(trj)%n_dyn_var > 0 ) then
            mask(:) = .false.
            call scan_vars( traj_type(trj)%n_dyn_var, traj_type(trj)%dyn_var, traj_type(trj)%dyn_ndx, &
                            dyn_max, dyn_var_lst(:), &
                            traj_type(trj)%chm_is_gas, 'dyn' )
          endif
          if( traj_type(trj)%n_msc_var > 0 ) then
            mask(:) = .false.
            call scan_vars( traj_type(trj)%n_msc_var, traj_type(trj)%msc_var, traj_type(trj)%msc_ndx, &
                            num_msc, msc_tbl(:), &
                            traj_type(trj)%chm_is_gas, 'msc' )
          endif
        end do

        do trj = 1,n_traj
          if( traj_type(trj)%n_msc_var > 0 ) then
            do m = 1,traj_type(trj)%n_msc_var
              St_Vars_msk(traj_type(trj)%msc_ndx(m)-offset) = .true.
            end do
          endif
        end do
        m = count( St_Vars_msk(:num_msc) )

        if( allocated( msc_tbl ) ) then
          deallocate( msc_tbl )
        endif

!-----------------------------------------------------------------------------
!  remove trajectories with no variables
!-----------------------------------------------------------------------------
        n_traj_1 = count( (traj_type(:n_traj)%n_chm_var + traj_type(:n_traj)%n_hyd_var &
                           + traj_type(:n_traj)%n_trc_var + traj_type(:n_traj)%n_dyn_var &
                           + traj_type(:n_traj)%n_msc_var + traj_type(:n_traj)%n_dchm_var) > 0 )
        if( n_traj_1 > 0 ) then
          if( n_traj_1 /= n_traj ) then
            trj_mask(1:n_traj) = traj_type(1:n_traj)%in_dom
            m = 1
            do trj = 1,n_traj
              if( trj_mask(trj) ) then
                if( trj /= m ) then
                  traj_type(m) = traj_type(trj)
                  m = m + 1
                endif
              endif
            end do
            n_traj = n_traj_1
          endif
        else
          dm_has_traj(dm) = .false.
          return
        endif
!-----------------------------------------------------------------------------
!  allocate buffer type
!-----------------------------------------------------------------------------
        if( is_root_proc ) then
          if( dm == 1 ) then
            allocate( trj_buff(traj_max,n_dom),stat=astat )
            if( astat /= 0 ) then
              write(err_mes,'(''trajectory_init: failed to allocate traj_buff; error = '',i6)') astat
              call wrf_error_fatal( trim( err_mes  ) )
            endif
          endif
          trj_pbf => trj_buff(:,dm)
        endif
      endif has_trajectories
!   endif master_proc
#if( WRF_CHEM == 1 )
!-----------------------------------------------------------------------------
!   for dchm package make sure dchm_ndx is ordered list
!-----------------------------------------------------------------------------
    do trj = 1,n_traj
      n = traj_type(trj)%n_dchm_var
      if( n > 0 .and.  any( traj_type(trj)%dchm_ndx(1:n-1) > traj_type(trj)%dchm_ndx(2:n) ) ) then
        trj_mask(:num_chem) = .false. 
        do m = 1,n
          trj_mask(traj_type(trj)%dchm_ndx(m)) = .true. 
        end do
        traj_type(trj)%dchm_ndx(:n) = pack( (/ (m,m=1,num_chem) /),trj_mask(:num_chem) )
        do m = 1,n
          m1 = traj_type(trj)%dchm_ndx(m)
          traj_type(trj)%dchm_spc(m) = trim(chem_dname_table(dm,m1))
        end do
      endif
    end do
#endif
!-----------------------------------------------------------------------------
!  if initial run, overwrite existing grid trajectory variables
!-----------------------------------------------------------------------------
#ifdef DM_PARALLEL
!  call wrf_dm_bcast_integer( n_traj,1 )
!  if( n_traj > 0 ) then
!    p_size = (5+var_max)*RWORDSIZE + (2+var_max)*LWORDSIZE &
!           + (5+4*var_max)*IWORDSIZE + 4*32*var_max + 38
!    do trj = 1,n_traj
!      call wrf_dm_bcast_bytes( traj_type(trj),p_size )
!    end do
!  endif
#endif
!  call wrf_abort( 'Debugging' )
is_cold_start: &
   if( .not. rstrt ) then
!-----------------------------------------------------------------------------
!  calc and check trajectory start point
!-----------------------------------------------------------------------------
has_trajectories_a: &
     if( n_traj > 0 ) then
       config_temp = config_flags
       call trajmapproj( grid, config_temp, proj )
       i_end = min(ipe,ide-1)
       j_end = min(jpe,jde-1)
       do j = jps,j_end
         do k = kps,kpe
           z_at_w(ips:i_end,k,j) = (grid%ph_2(ips:i_end,k,j) + grid%phb(ips:i_end,k,j))/g
         end do
       end do
!-----------------------------------------------------------------------------
!  first check x,y
!-----------------------------------------------------------------------------
traj_loop: &
       do trj = 1,n_traj
         if( traj_type(trj)%lat /= missing_val .and. &
             traj_type(trj)%lon /= missing_val ) then
           call latlon_to_ij( proj, traj_type(trj)%lat, traj_type(trj)%lon, x, y )
           traj_type(trj)%in_dom = &
                  (x >= real(ids) .and. x <= real(ide-1) .and. &
                   y >= real(jds) .and. y <= real(jde-1))
#ifdef SW_DEBUG
          write(err_mes,'(''trajectory_init('',i2.2,''): x,y = '',1p,2g15.7)') dm,x,y
          call wrf_debug( 0,trim(err_mes) )
#endif
         else
           traj_type(trj)%in_dom = .false.
         endif
#ifdef SW_DEBUG
         write(err_mes,'(''trajectory_init('',i2.2,''): traj '',i4,'' in dom  = '',l)') dm,trj,traj_type(trj)%in_dom
         call wrf_debug( 0,trim(err_mes) )
#endif
!-----------------------------------------------------------------------------
!  then check z
!-----------------------------------------------------------------------------
is_in_domain: &
         if( traj_type(trj)%in_dom ) then
           i = nint( x )
           j = nint( y )
           traj_type(trj)%in_patch = &
                  (i >= ips .and. i <= min(ipe,ide-1) .and. &
                   j >= jps .and. j <= min(jpe,jde-1))
is_in_patch: &
           if( traj_type(trj)%in_patch ) then
             k1 = kde - 1
             if( traj_type(trj)%z_is_agl ) then
               traj_type(trj)%lev = traj_type(trj)%lev + z_at_w(i,kds,j)
             endif
             z_dm_bot = z_at_w(i,kds,j)
             z_dm_top = z_at_w(i,k1,j)
             write(err_mes,'(''trajectory_init('',i2.2,''): traj '',i3.3,'' i,j,z_bot,z_top,lev = '',2i4,1p3g15.7)') &
                dm,trj,i,j,z_dm_bot,z_dm_top,traj_type(trj)%lev
             call wrf_debug( 0,trim(err_mes) )
             if( traj_type(trj)%lev >= z_dm_bot .and. &
                 traj_type(trj)%lev <= z_dm_top ) then
               traj_type(trj)%in_dom = .true.
               traj_type(trj)%x      = x
               traj_type(trj)%y      = y
             else
               traj_type(trj)%in_dom = .false.
             endif
           else is_in_patch
             traj_type(trj)%x = missing_val
             traj_type(trj)%y = missing_val
           endif is_in_patch
         endif is_in_domain
       end do traj_loop
       n_traj_1 = count( traj_type(:n_traj)%in_dom )
     else has_trajectories_a
       n_traj_1 = 0
     endif has_trajectories_a

!-----------------------------------------------------------------------------
!  remove out of domain trajectories
!-----------------------------------------------------------------------------
     write(err_mes,'(''trajectory_init('',i2.2,''): traj cnt = '',2i6)') dm,n_traj_1,n_traj
     call wrf_debug( 0,trim(err_mes) )
     if( n_traj_1 /= n_traj ) then
       trj_mask(1:n_traj) = traj_type(1:n_traj)%in_dom
       m = 1
       do trj = 1,n_traj
         if( trj_mask(trj) ) then
           if( trj /= m ) then
             traj_type(m) = traj_type(trj)
             m = m + 1
           endif
         endif
       end do
       n_traj = n_traj_1
     endif

has_trajectories_b: &
     if( n_traj_1 > 0 ) then
       grid%traj_i(:) = missing_val
       grid%traj_j(:) = missing_val
       grid%traj_k(:) = missing_val
       grid%traj_long(:) = missing_val
       grid%traj_lat(:)  = missing_val
!-----------------------------------------------------------------------------
!  set initial trajectory spatial coordinates
!-----------------------------------------------------------------------------
       k1 = kde - 1
       do trj = 1,n_traj
         if( traj_type(trj)%in_patch ) then
           grid%traj_i(trj) = traj_type(trj)%x
           grid%traj_j(trj) = traj_type(trj)%y
           grid%traj_long(trj) = traj_type(trj)%lon
           grid%traj_lat(trj)  = traj_type(trj)%lat
           i = nint( traj_type(trj)%x )
           j = nint( traj_type(trj)%y )
!          z(kds:k1) = .5*(z_at_w(i,kds:k1,j) + z_at_w(i,kds+1:k1+1,j))
           z(kds:k1) = z_at_w(i,kds:k1,j)
           do k = kds+1,k1
             if( traj_type(trj)%lev <= z(k) ) then
               grid%traj_k(trj) = real(k - 1) &
                              + (traj_type(trj)%lev - z(k-1))/(z(k) - z(k-1))
               exit
             endif
           end do
           write(err_mes,'(''trajectory_init('',i2.2,''): trj,k,z(k-1:k),traj_k = '',2i3,1p3g15.7)') &
              dm,trj,k,z(k-1:k),grid%traj_k(trj)
           call wrf_debug( 0,trim(err_mes) )
         endif
       end do
     else has_trajectories_b
       dm_has_traj(dm) = .false.
       return
     endif has_trajectories_b
   else is_cold_start
     if( n_traj > 0 ) then
       call set_in_dom
     else
       traj_cnt(dm) = n_traj
       return
     endif
   endif is_cold_start

!-----------------------------------------------------------------------------
!  transfer from local variable to module variable
!-----------------------------------------------------------------------------
   traj_cnt(dm) = n_traj
   do trj = 1,n_traj
     trjects(trj) = traj_type(trj)
   end do
!-----------------------------------------------------------------------------
!  create netcdf trajectory file
!-----------------------------------------------------------------------------
   if( .not. rstrt ) then
     do trj = 1,n_traj
#ifdef DM_PARALLEL
       grid%traj_i(trj) = wrf_dm_max_real( grid%traj_i(trj) )
       grid%traj_j(trj) = wrf_dm_max_real( grid%traj_j(trj) )
       grid%traj_k(trj) = wrf_dm_max_real( grid%traj_k(trj) )
       grid%traj_long(trj) = wrf_dm_max_real( grid%traj_long(trj) )
       grid%traj_lat(trj)  = wrf_dm_max_real( grid%traj_lat(trj) )
#else
       grid%traj_i(trj) = grid%traj_i(trj)
       grid%traj_j(trj) = grid%traj_j(trj)
       grid%traj_k(trj) = grid%traj_k(trj)
       grid%traj_long(trj) = grid%traj_long(trj)
       grid%traj_lat(trj)  = grid%traj_lat(trj)
#endif
     end do
   endif

   do pkg = 1,pkg_max
     select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
       case( 'chm' )
         pkg_has_vars(:n_traj,pkg) = trjects(:n_traj)%n_chm_var > 0
       case( 'dchm' )
         pkg_has_vars(:n_traj,pkg) = trjects(:n_traj)%n_dchm_var > 0
#endif
       case( 'hyd' )
         pkg_has_vars(:n_traj,pkg) = trjects(:n_traj)%n_hyd_var > 0
       case( 'trc' )
         pkg_has_vars(:n_traj,pkg) = trjects(:n_traj)%n_trc_var > 0
       case( 'dyn' )
         pkg_has_vars(:n_traj,pkg) = trjects(:n_traj)%n_dyn_var > 0
       case( 'msc' )
         pkg_has_vars(:n_traj,pkg) = trjects(:n_traj)%n_msc_var > 0
     end select
   end do

#if( WRF_CHEM == 1 )
!-----------------------------------------------------------------------------
!  setup for  dchm buffer
!-----------------------------------------------------------------------------
   n_dchm_dm(dm) = 0
   if( any( pkg_has_vars(:n_traj,dchm_pkg) ) ) then
     if( .not. allocated( dchm_msk_dm ) ) then
       allocate( dchm_msk_dm(num_chem,n_dom),stat=astat )
       if( astat /= 0 ) then
         write(err_mes,'(''trajectory_init('',i2.2,''): failed to allocate dchm_msk_dm; error = '',i6)') dm,astat
         call wrf_error_fatal( trim( err_mes  ) )
       endif
     endif
     if( .not. allocated( dchm_buf_ndx_dm ) ) then
       allocate( dchm_buf_ndx_dm(num_chem,n_dom),stat=astat )
       if( astat /= 0 ) then
         write(err_mes,'(''trajectory_init('',i2.2,''): failed to allocate dchm_buf_ndx_dm; error = '',i6)') dm,astat
         call wrf_error_fatal( trim( err_mes  ) )
       endif
     endif
     n_dchm   => n_dchm_dm(dm)
     dchm_msk => dchm_msk_dm(:,dm)
     dchm_msk(:) = .false.
     dchm_buf_ndx => dchm_buf_ndx_dm(:,dm)
     dchm_buf_ndx(:) = 0
     do trj = 1,n_traj
       do m = 1,trjects(trj)%n_dchm_var
         dchm_msk(trjects(trj)%dchm_ndx(m)) = .true.
       end do
     end do
     n_dchm = count( dchm_msk )
     dchm_buf_ndx(:n_dchm) = pack( (/ (m,m=1,num_chem) /),dchm_msk(:num_chem) )
     do trj = 1,n_traj
       if( trjects(trj)%n_dchm_var > 0 ) then
         do m1 = 1,trjects(trj)%n_dchm_var
           target = trjects(trj)%dchm_ndx(m1)
           do m2 = 1,n_dchm
             if( target == dchm_buf_ndx(m2) ) then
               trjects(trj)%dchm_ndx(m1) = m2 + offset
               exit
             endif
           end do
         end do
       endif
     end do
   endif
#endif

master_proc_a: &
   if( is_root_proc ) then
     if( dm == 1 ) then
       n = max(num_chem,num_moist,num_tracer,num_msc+offset,dyn_max+offset)
       if( n  > offset .and. .not. allocated(trj_msk_dm) ) then
         allocate( trj_msk_dm(traj_max,n,pkg_max,n_dom),stat=astat )
         if( astat /= 0 ) then
           write(err_mes,'(''trajectory_init: failed to allocate trj_msk_dm; error = '',i6)') astat
           call wrf_error_fatal( trim( err_mes  ) )
         endif
         trj_msk_dm(:,:,:,:) = .false.
       endif
     endif
is_initial: &
     if( .not. is_initialized(dm) ) then
!-----------------------------------------------------------------------------
!  allocate buffer arrays
!-----------------------------------------------------------------------------
trj_loop: &
       do trj = 1,n_traj
pkg_loop:  do pkg = 1,pkg_max
             astat = 0
             select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
               case( 'chm' )
                 trj_msk => trj_msk_dm(:,:,chm_pkg,dm)
                 m1 = trjects(trj)%n_chm_var
                 if( m1 > 0 ) then
                   allocate( trj_pbf(trj)%chm_vals(vals_max,m1),stat=astat)
                   do m = 1,m1
                     trj_msk(trj,trjects(trj)%chm_ndx(m)) = trjects(trj)%in_dom
                   end do
                 endif
               case( 'dchm' )
                 trj_msk => trj_msk_dm(:,:,dchm_pkg,dm)
                 m1 = trjects(trj)%n_dchm_var
                 if( m1 > 0 ) then
                   allocate( trj_pbf(trj)%dchm_vals(vals_max,m1),stat=astat)
                   do m = 1,m1
                     trj_msk(trj,trjects(trj)%dchm_ndx(m)) = trjects(trj)%in_dom
                   end do
                 endif
#endif
               case( 'hyd' )
                 trj_msk => trj_msk_dm(:,:,hyd_pkg,dm)
                 m1 = trjects(trj)%n_hyd_var
                 if( m1 > 0 ) then
                   allocate( trj_pbf(trj)%hyd_vals(vals_max,m1),stat=astat)
                   do m = 1,m1
                     trj_msk(trj,trjects(trj)%hyd_ndx(m)) = trjects(trj)%in_dom
                   end do
                 endif
               case( 'trc' )
                 trj_msk => trj_msk_dm(:,:,trc_pkg,dm)
                 m1 = trjects(trj)%n_trc_var
                 if( m1 > 0 ) then
                   allocate( trj_pbf(trj)%trc_vals(vals_max,m1),stat=astat)
                   do m = 1,m1
                     trj_msk(trj,trjects(trj)%trc_ndx(m)) = trjects(trj)%in_dom
                   end do
                 endif
               case( 'dyn' )
                 trj_msk => trj_msk_dm(:,:,dyn_pkg,dm)
                 m1 = trjects(trj)%n_dyn_var
                 if( m1 > 0 ) then
                   allocate( trj_pbf(trj)%dyn_vals(vals_max,m1),stat=astat)
                   do m = 1,m1
                     trj_msk(trj,trjects(trj)%dyn_ndx(m)) = trjects(trj)%in_dom
                   end do
                 endif
               case( 'msc' )
                 trj_msk => trj_msk_dm(:,:,msc_pkg,dm)
                 m1 = trjects(trj)%n_msc_var
                 if( m1 > 0 ) then
                   allocate( trj_pbf(trj)%msc_vals(vals_max,m1),stat=astat)
                   do m = 1,m1
                     trj_msk(trj,trjects(trj)%msc_ndx(m)) = trjects(trj)%in_dom
                   end do
                 endif
             end select
             if( astat /= 0 ) then
               write(err_mes,'(''trajectory_init: failed to allocate buffer%'',a,''; error = '',i6)') &
                   pkg_tag(pkg),astat
               call wrf_error_fatal( trim( err_mes  ) )
             endif
           end do pkg_loop
       end do trj_loop

       do pkg = 1,pkg_max
         select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
           case( 'chm' )
             trj_msk => trj_msk_dm(:,:,chm_pkg,dm)
             u_lim = num_chem
           case( 'dchm' )
             trj_msk => trj_msk_dm(:,:,dchm_pkg,dm)
             u_lim = num_chem
#endif
           case( 'hyd' )
             trj_msk => trj_msk_dm(:,:,hyd_pkg,dm)
             u_lim = num_moist
           case( 'trc' )
             trj_msk => trj_msk_dm(:,:,trc_pkg,dm)
             u_lim = num_tracer
           case( 'dyn' )
             trj_msk => trj_msk_dm(:,:,dyn_pkg,dm)
             u_lim = dyn_max + offset
           case( 'msc' )
             trj_msk => trj_msk_dm(:,:,msc_pkg,dm)
             u_lim = num_msc + offset
         end select
         do trj = 1,n_traj
           trj_msk(trj,1) = any( trj_msk(trj,param_first_scalar:u_lim) )
         end do
       end do
       is_initialized(dm) = .true.
       if( .not. rstrt ) then
         call trajectory_create_file( grid, n_traj )
       endif
     endif is_initial
   endif master_proc_a
#ifdef DM_PARALLEL
   call wrf_dm_bcast_logical( is_initialized,n_dom )
#endif

   CONTAINS

   subroutine reg_scan( grid )

   use module_domain_type, only : domain, fieldlist

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   type(domain), intent(in) :: grid

   integer, parameter :: nVerbotten = 8
!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer         :: astat
   integer         :: cnt
   integer         :: dm, dm_ndx
   integer         :: m, n
   type(fieldlist), pointer :: p
   logical         :: valid
   type(statevar), allocatable :: St_Vars_wrk(:,:)
   logical, allocatable :: St_Vars_msk_wrk(:,:)
   character(len=80) :: tstring
   character(len=9) :: Verbotten(nVerbotten) = (/ 'zx       ', 'zy       ', &
                                                  'RUNDGTEN ', 'RVNDGTEN ', &
                                                  'U_NDG_OLD', 'V_NDG_OLD', &
                                                  'U_NDG_NEW', 'V_NDG_NEW'/)

!-----------------------------------------------------------------------------
!  just get the count
!-----------------------------------------------------------------------------
   dm = grid%id
   p => grid%head_statevars%next ; cnt = 0
   do while( associated(p) )
     valid = (p%Type == 'R' .or. p%Type == 'r') .and. &
             (p%Ndim == 3 .or. (p%Ndim == 4 .and. p%scalar_array))
     if( valid ) then
       valid = p%MemoryOrder(:3) == 'XZY' .and. (p%em2 == kde-1 .or. p%em2 == kde)
       if( valid ) then
         if( p%Ndim == 3 ) then
           do m = 1,nVerbotten
             if( trim(Verbotten(m)) == trim(p%Varname) ) then
               valid = .false.
               exit
             endif
           enddo
           if( valid ) then
             cnt = cnt + 1
           endif
         elseif( p%Ndim == 4 ) then
           do n = param_first_scalar,p%num_table(dm)
             valid = .true.
             do m = 1,nVerbotten
               tstring = trim(Verbotten(m))
               call upcase( tstring )
               if( trim(tstring) == trim(p%dname_table(dm,n)) ) then
                 valid = .false.
                 exit
               endif
             enddo
             if( valid ) then
               cnt = cnt + 1
             endif
           enddo
         endif
       endif
     endif
     p => p%next
   end do

   write(*,'(''reg_scan('',i2.2,''): found '',i4,'' state variables '')') dm,cnt

!-----------------------------------------------------------------------------
!  now allocate and set St_Vars
!-----------------------------------------------------------------------------
   num_msc_dm(dm) = cnt
   if( cnt > 0 ) then
     if( .not. allocated( St_Vars_dm ) ) then
       allocate( St_Vars_dm(cnt,n_dom),St_Vars_msk_dm(cnt,n_dom),stat=astat )
       if( astat /= 0 ) then
         call wrf_error_fatal( 'reg_scan: failed to allocate St_Vars,St_Vars_msk' )
       endif
     elseif( cnt > maxval(num_msc_dm(1:dm-1)) ) then
       n = size( St_vars_dm,dim=1 )
       allocate( St_Vars_wrk(n,dm-1),St_Vars_msk_wrk(n,dm-1),stat=astat )
       if( astat /= 0 ) then
         call wrf_error_fatal( 'reg_scan: failed to allocate St_Vars,St_Vars_msk wrk arrays' )
       endif
       do dm_ndx = 1,dm-1
         do m = 1,n
           St_Vars_wrk(m,dm_ndx) = St_Vars_dm(m,dm_ndx)
           St_Vars_msk_wrk(m,dm_ndx) = St_Vars_msk_dm(m,dm_ndx)
         end do
       end do
       deallocate( St_vars_dm,St_Vars_msk_dm )
       allocate( St_Vars_dm(cnt,n_dom),St_Vars_msk_dm(cnt,n_dom),stat=astat )
       if( astat /= 0 ) then
         call wrf_error_fatal( 'reg_scan: failed to allocate St_Vars,St_Vars_msk' )
       endif
       do dm_ndx = 1,dm-1
         do m = 1,n
           St_Vars_dm(m,dm_ndx) = St_Vars_wrk(m,dm_ndx)
           St_Vars_msk_dm(m,dm_ndx) = St_Vars_msk_wrk(m,dm_ndx)
         end do
       end do
       deallocate( St_vars_wrk,St_Vars_msk_wrk )
     endif

     St_Vars     => St_Vars_dm(:,dm)
     St_Vars_msk => St_Vars_msk_dm(:,dm)

     St_Vars_msk(:cnt) = .false.
     p => grid%head_statevars%next ; cnt = 0

     do while( associated(p) )
       valid = (p%Type == 'R' .or. p%Type == 'r') .and. &
               (p%Ndim == 3 .or. (p%Ndim == 4 .and. p%scalar_array))
       if( valid ) then
         valid = p%MemoryOrder(:3) == 'XZY' .and. (p%em2 == kde-1 .or. p%em2 == kde)
         if( valid ) then
           if( p%Ndim == 3 ) then
             do m = 1,nVerbotten
               if( trim(Verbotten(m)) == trim(p%Varname) ) then
                 valid = .false.
                 exit
               endif
             enddo
             if( valid ) then
               cnt = cnt + 1
               St_Vars(cnt)%Varname = p%Varname 
               St_Vars(cnt)%Description = p%Description 
               St_Vars(cnt)%Units   = p%Units 
               St_Vars(cnt)%MemOrd  = p%MemoryOrder 
               St_Vars(cnt)%Stagger = p%Stagger
               St_Vars(cnt)%Ndim = p%Ndim 
               St_Vars(cnt)%rfield_3d => p%rfield_3d
             endif
           elseif( p%Ndim == 4 ) then
             do n = param_first_scalar,p%num_table(dm)
               valid = .true.
               do m = 1,nVerbotten
                 tstring = trim(Verbotten(m))
                 call upcase( tstring )
                 if( trim(tstring) == trim(p%dname_table(dm,n)) ) then
                   valid = .false.
                   exit
                 endif
               enddo
               if( valid ) then
                 cnt = cnt + 1
                 St_Vars(cnt)%Varname = trim(p%dname_table(dm,n))
                 St_Vars(cnt)%Description = p%Description 
                 St_Vars(cnt)%Units   = p%Units 
                 St_Vars(cnt)%MemOrd  = p%MemoryOrder 
                 St_Vars(cnt)%Stagger = p%Stagger
                 St_Vars(cnt)%Ndim = 3
                 St_Vars(cnt)%rfield_3d => p%rfield_4d(:,:,:,p%index_table(n,dm))
               endif
             enddo
           endif
         endif
       endif
       p => p%next
     end do
   endif

   end subroutine reg_scan

   subroutine get_var_cnt( n_vars, vars )

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   integer, intent(inout) :: n_vars
   character(len=32), intent(inout)  :: vars(:)

   mask(:) = vars(:) /= ' '
   wrk_chr(:) = ' '
   m1 = 0
   do n = 1,var_max
     if( mask(n) ) then
       m1 = m1 + 1
       wrk_chr(m1) = vars(n)
     endif
   end do

   n_vars  = count( wrk_chr(:) /= ' ' )
   vars(:) = wrk_chr(:)

   end subroutine get_var_cnt

   subroutine scan_vars( n_vars, vars, var_ndx, n_tbl, tbl, &
                         is_gas, var_type )

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   integer, intent(inout) :: n_vars
   integer, intent(in)    :: n_tbl
   integer, intent(inout) :: var_ndx(:)
   logical, intent(inout) :: is_gas(:)
   character(len=*), intent(in)      :: var_type
   character(len=32), intent(inout)  :: vars(:)
   character(len=256), intent(in)    :: tbl(:)

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: ml
   integer :: ndx(var_max)

var_loop: &
   do n = 1,n_vars
     var_name = vars(n)
     if( trim(var_type) == 'chm' ) then
       i = index( '(a)', var_name )
       if( i == 0 ) then
         i = index( '(A)', var_name )
       endif
       if( i > 0 ) then
         is_gas(n) = .false.
         var_name(i:) = ' '
         vars(n)(i:)  = ' '
       endif
     endif
     if( trim(var_type) == 'dyn' .or. trim(var_type) == 'msc' ) then
       ml = 1
     else
       ml = param_first_scalar
     endif
!    if( trim(var_type) /= 'dyn' ) then
!      ml = param_first_scalar
!    else
!      ml = 1
!    endif
     do m1 = ml,n_tbl
       if( trim(var_name) == trim(tbl(m1)) ) then
         mask(n) = .true.
         ndx(n)  = m1
         exit
       endif 
     end do
     if( .not. mask(n) ) then
       write(err_mes,'(''scan_vars('',i2.2,''): '',a,'' not in '',a,'' opt'')') dm,trim(var_name),var_type
       call wrf_message( trim(err_mes) )
     endif
   end do var_loop

   if( trim(var_type) == 'dyn' .or. trim(var_type) == 'msc' ) then
     ndx(1:n_vars) = ndx(1:n_vars) + offset
   endif

   wrk_chr(:) = ' '
   m1 = 0
   do n = 1,var_max
     if( mask(n) ) then
       m1 = m1 + 1
       wrk_chr(m1) = vars(n)
     endif
   end do

   var_ndx(:count(mask)) = pack( ndx(:),mask=mask)
   vars(:) = wrk_chr(:)
   n_vars  = count( mask )

   end subroutine scan_vars

   subroutine set_in_dom

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: ncid
   integer :: ios
   integer :: time_id
   integer :: varid
   integer :: time_ndx

   real    :: traj_i(n_traj)
   real    :: traj_j(n_traj)
   real    :: traj_k(n_traj)

   character(len=256) :: err_mes
   character(len=256) :: filename

include 'netcdf.inc'

!---------------------------------------------------------------------
!  open netcdf file
!---------------------------------------------------------------------
   write(filename,'(''wrfout_traj_d'',i2.2)',iostat=ios) dm
   if( ios /= 0 ) then
     write(err_mes,'(''set_in_dom: failed to set filename: error = '',i6)') ios
     call wrf_error_fatal( trim( err_mes  ) )
   endif
   ios = nf_open( trim(filename), nf_nowrite, ncid )
   if( ios /= 0 ) then
     write(err_mes,'(''set_in_dom: failed to open '',a,'': error = '',i6)') trim(filename),ios
     call wrf_error_fatal( trim( err_mes  ) )
   endif

!---------------------------------------------------------------------
!  get current time index
!---------------------------------------------------------------------
   err_mes = 'set_in_dom: failed to get time id'
   call handle_ncerr( nf_inq_dimid( ncid, 'time', time_id ),trim(err_mes) )
   err_mes = 'set_in_dom: failed to get time dimension'
   call handle_ncerr( nf_inq_dimlen( ncid, time_id, time_ndx ),trim(err_mes) )

!---------------------------------------------------------------------
!  read in last traj_{i,j,k} variables
!---------------------------------------------------------------------
   err_mes = 'set_in_dom: failed to get traj_i id'
   call handle_ncerr( nf_inq_varid( ncid, 'traj_i', varid ),trim(err_mes) )
   err_mes = 'set_in_dom: failed to get traj_i'
   call handle_ncerr( nf_get_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,1 /), &
                                        traj_i ),trim(err_mes) )
   err_mes = 'set_in_dom: failed to get traj_j id'
   call handle_ncerr( nf_inq_varid( ncid, 'traj_j', varid ),trim(err_mes) )
   err_mes = 'set_in_dom: failed to get traj_j'
   call handle_ncerr( nf_get_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,1 /), &
                                        traj_j ),trim(err_mes) )
   err_mes = 'set_in_dom: failed to get traj_k id'
   call handle_ncerr( nf_inq_varid( ncid, 'traj_k', varid ),trim(err_mes) )
   err_mes = 'set_in_dom: failed to get traj_k'
   call handle_ncerr( nf_get_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,1 /), &
                                        traj_k ),trim(err_mes) )
   traj_type(1:n_traj)%in_dom = traj_i(1:n_traj) /= missing_val .and.  traj_j(1:n_traj) /= missing_val &
                                                                .and.  traj_k(1:n_traj) /= missing_val

   ios = nf_close( ncid )

   end subroutine set_in_dom

   end subroutine trajectory_init

#ifdef NETCDF
   subroutine trajectory_driver( grid )

#ifdef DM_PARALLEL
   use module_dm, only : &
                  local_communicator, mytask, ntasks, ntasks_x, ntasks_y                   &
                 ,local_communicator_periodic, wrf_dm_max_real, wrf_dm_max_int
   use module_comm_dm, only : halo_em_chem_e_3_sub, halo_em_moist_e_3_sub
   use module_comm_dm, only : halo_em_tracer_e_3_sub
#endif
   use module_domain
   use module_date_time
   use module_state_description, only : num_chem
   use module_state_description, only : num_moist, num_tracer, param_first_scalar

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   type(domain), intent(in) :: grid

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: ims,ime, jms,jme, kms,kme
   integer :: ids,ide, jds,jde, kds,kde
   integer :: ips,ipe, jps,jpe, kps,kpe
   integer :: dm
   integer :: j, k
   integer :: il, iu, ios, jl, ju, kl, ku
   integer :: m, mu, n, ndx, n_vars, n_traj
   integer :: pkg_var_cnt(traj_max)
   integer :: ncid
   integer :: pkg, trj
   integer :: n_msc_buf
   integer :: num_chem_sav
   integer, pointer :: num_msc      ! number misc species
   integer, pointer :: n_dchm       ! number dchm buffer species
   integer, pointer :: dchm_buf_ndx(:)
   integer :: St_Vars_ndx
   integer, allocatable :: St_Vars_buf_ndx(:)
#ifndef DM_PARALLEL
   integer :: mytask
#endif
   integer :: traj_proc(traj_max), glb_traj_proc(traj_max)
   real :: dchm_fill_val(traj_max)
   real :: x, y, zi
   real :: delsx, delsy, o_delsx, o_delsy
   real :: delsz, o_delsz
   real :: max_conc
   real :: horz_conc(2)
   real, pointer :: traj_conc(:)
   real, target  :: traj_val(var_max,traj_max)
   real, pointer :: wrk4d(:,:,:,:)
   real, allocatable, target :: chem(:,:,:,:)
   real, allocatable, target :: moist(:,:,:,:)
   real, allocatable, target :: tracer(:,:,:,:)
   character(len=19)  :: current_timestr, next_timestr
   character(len=32)  :: var_name(var_max)
   character(len=256) :: err_mes
   logical :: has_dchm
   logical :: is_root_proc
   logical :: is_in_patch_gap
   logical :: flsh_buff
   logical :: found
   logical :: traj_is_active(traj_max)
   logical :: pkg_is_active(traj_max,pkg_max)
   logical, pointer :: pkg_has_vars(:,:)

   logical, external :: wrf_dm_on_monitor

   type(WRFU_Time) :: current_time, next_time
   type(WRFU_Time) :: start_time, stop_time

include 'netcdf.inc'

#ifndef DM_PARALLEL
   mytask = 0
#endif
   dm = grid%id
   n_traj = traj_cnt(dm)
has_trajectories: &
   if( dm_has_traj(dm) .and. n_traj > 0 ) then
     St_Vars => St_Vars_dm(:,dm)
     St_Vars_msk => St_Vars_msk_dm(:,dm)
     num_msc => num_msc_dm(dm)
     trjects => traject(:,dm)
     n_vals  => n_vals_dm(dm)
     is_root_proc = wrf_dm_on_monitor()
     if( is_root_proc ) then
       trj_pbf => trj_buff(:,dm)
     endif
     has_dchm = any( trjects(:n_traj)%n_dchm_var > 0 )
     if( has_dchm ) then
       n_dchm => n_dchm_dm(dm)
       dchm_buf_ndx => dchm_buf_ndx_dm(:,dm)
     endif

     call get_ijk_from_grid( grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

!-----------------------------------------------------------------------------
!  is trajectory in time interval?
!-----------------------------------------------------------------------------
     call domain_clock_get( grid, current_time=current_time, current_timestr=current_timestr)
     call geth_newdate( next_timestr, current_timestr, int(grid%dt) )
     call wrf_atotime( next_timestr(1:19), next_time )
     do trj = 1,n_traj
       call wrf_atotime( traject(trj,dm)%start_time(1:19), start_time )
       call wrf_atotime( traject(trj,dm)%stop_time(1:19), stop_time )
       traj_is_active(trj) = next_time .ge. start_time .and. next_time .le. stop_time
     end do
!-----------------------------------------------------------------------------
!  is trajectory in domain?
!-----------------------------------------------------------------------------
     pkg_has_vars => pkg_has_vars_dm(:,:,dm)
     do trj = 1,n_traj
       if( traj_is_active(trj) ) then
         trjects(trj)%in_dom = grid%traj_i(trj) >= real(ids) .and. grid%traj_i(trj) <= real(ide-1)
         if( trjects(trj)%in_dom ) then
           trjects(trj)%in_dom = grid%traj_j(trj) >= real(jds) .and. grid%traj_j(trj) <= real(jde-1)
         endif
         if( trjects(trj)%in_dom ) then
           trjects(trj)%in_dom = grid%traj_k(trj) >= real(kps) .and. grid%traj_k(trj) <= real( min( kpe,kde-1 ) )
         endif
         traj_is_active(trj) = trjects(trj)%in_dom
       endif
     end do
     do pkg = 1,pkg_max
       pkg_is_active(:n_traj,pkg) = traj_is_active(:n_traj) .and. pkg_has_vars(:n_traj,pkg)
     end do
!-----------------------------------------------------------------------------
!  check whether this is a chemistry time step
!-----------------------------------------------------------------------------
     dchm_fill_val(:n_traj) = missing_val
     if( .not. do_chemstep ) then
       do trj = 1,n_traj
         if( pkg_is_active(trj,dchm_pkg) ) then
           dchm_fill_val(trj) = zero_val
         endif
       end do
       pkg_is_active(:n_traj,dchm_pkg) = .false.
     endif
!-----------------------------------------------------------------------------
!  is trajectory in mpi process?
!-----------------------------------------------------------------------------
     traj_proc(:n_traj) = -1
     do trj = 1,n_traj
       if( traj_is_active(trj) ) then
         trjects(trj)%in_patch = grid%traj_i(trj) >= real(ips) .and. grid%traj_i(trj) <= real( min( ipe,ide-1 ) )
         if( trjects(trj)%in_patch ) then
           trjects(trj)%in_patch = grid%traj_j(trj) >= real(jps) .and. grid%traj_j(trj) <= real( min( jpe,jde-1 ) )
         endif
         if( trjects(trj)%in_patch ) then
           trjects(trj)%in_patch = grid%traj_k(trj) >= real(kps) .and. grid%traj_k(trj) <= real( min( kpe,kde-1 ) )
         endif
         if( trjects(trj)%in_patch ) then
           traj_proc(trj) = mytask + 1
         else
           traj_proc(trj) = 0
         endif
       endif
     end do
#ifdef DM_PARALLEL
     do trj = 1,n_traj
       glb_traj_proc(trj) = wrf_dm_max_int( traj_proc(trj) )
     end do
#else
     glb_traj_proc(1:n_traj) = traj_proc(1:n_traj)
#endif
!-----------------------------------------------------------------------------
!  any trajectories in "gap" between patches?
!-----------------------------------------------------------------------------
     do trj = 1,n_traj
       if( traj_is_active(trj) .and. glb_traj_proc(trj) == 0 ) then
         trjects(trj)%in_patch = grid%traj_i(trj) >= real(ips) .and. grid%traj_i(trj) <= real( min( ipe+1,ide-1 ) )
         if( trjects(trj)%in_patch ) then
           trjects(trj)%in_patch = grid%traj_j(trj) >= real(jps) .and. grid%traj_j(trj) <= real( min( jpe+1,jde-1 ) )
         endif
         if( trjects(trj)%in_patch ) then
           trjects(trj)%in_patch = grid%traj_k(trj) >= real(kps) .and. grid%traj_k(trj) <= real( min( kme,kde-1 ) )
         endif
         if( trjects(trj)%in_patch ) then
           traj_proc(trj) = mytask + 1
         else
           traj_proc(trj) = 0
         endif
         if( traj_proc(trj) /= 0 ) then
           call wrf_debug( 0,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^')
           write(err_mes,'(''Gapper '',i5,''; x,y,zi = '',1p,3g15.7)') trj,grid%traj_i(trj),grid%traj_j(trj),grid%traj_k(trj)
           call wrf_debug( 0,trim(err_mes) )
           write(err_mes,'(''Gapper ips,ipe,jps,jpe = '',4i5)') ips,ipe,jps,jpe
           call wrf_debug( 0,trim(err_mes) )
           call wrf_debug( 0,'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^')
         endif
       endif
     end do

!-----------------------------------------------------------------------------
!  buffer traj time and position
!-----------------------------------------------------------------------------
     if( is_root_proc ) then
       n_vals = n_vals + 1
       if( grid%itimestep > 0 ) then
         trj_pbf(:n_traj)%times(n_vals) = next_timestr
       else
         trj_pbf(:n_traj)%times(n_vals) = current_timestr
       endif
       do trj = 1,n_traj
         trj_pbf(trj)%trj_i(n_vals) = grid%traj_i(trj)
         trj_pbf(trj)%trj_j(n_vals) = grid%traj_j(trj)
         trj_pbf(trj)%trj_k(n_vals) = grid%traj_k(trj)
         trj_pbf(trj)%trj_lons(n_vals) = grid%traj_long(trj)
         trj_pbf(trj)%trj_lats(n_vals) = grid%traj_lat(trj)
       end do
     endif

     do trj = 1,n_traj
       traj_val(:,trj) = missing_val
     end do

pkg_loop: &
     do pkg = 1,pkg_max 
pkg_has_active_traj: &
       if( any( pkg_is_active(:n_traj,pkg) ) ) then
!-----------------------------------------------------------------------------
!  allocate working data array
!-----------------------------------------------------------------------------
         select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
           case( 'chm' )
             allocate( chem(ims:ime,kms:kme,jms:jme,num_chem),stat=ios )
           case( 'dchm' )
             if( n_dchm > 0 ) then
               allocate( chem(ims:ime,kms:kme,jms:jme,n_dchm+offset),stat=ios )
             endif
#endif
           case( 'hyd' )
             allocate( moist(ims:ime,kms:kme,jms:jme,num_moist),stat=ios )
           case( 'trc' )
             allocate( tracer(ims:ime,kms:kme,jms:jme,num_tracer),stat=ios )
           case( 'dyn' )
             allocate( chem(ims:ime,kms:kme,jms:jme,dyn_max+offset+2),stat=ios )
           case( 'msc' )
             m = count( St_Vars_msk(:num_msc) )
             if( m > 0 ) then
               allocate( chem(ims:ime,kms:kme,jms:jme,m+offset), &
                         St_Vars_buf_ndx(m+offset),stat=ios )
             endif
           case default
             ios = 0
         end select
         if( ios /= 0 ) then
           write(err_mes,'(''trajectory_driver: failed to allocate wrk4d: error = '',i6)') ios
           call wrf_error_fatal( trim( err_mes  ) )
         endif
         select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
           case( 'chm' )
             do m = 1,num_chem
               do j = jps,jpe
                 do k = kps,kpe
                   chem(ips:ipe,k,j,m) = grid%chem(ips:ipe,k,j,m)
                 end do
               end do
             end do
           case( 'dchm' )
             do m = param_first_scalar,n_dchm+offset
               do j = jps,jpe
                 do k = kps,kpe
                   chem(ips:ipe,k,j,m) = dchm_buff(ips:ipe,k,j,m)
                 end do
               end do
             end do
#endif
           case( 'hyd' )
             do m = 1,num_moist
               do j = jps,jpe
                 do k = kps,kpe
                   moist(ips:ipe,k,j,m) = grid%moist(ips:ipe,k,j,m)
                 end do
               end do
             end do
           case( 'trc' )
             do m = 1,num_tracer
               do j = jps,jpe
                 do k = kps,kpe
                   tracer(ips:ipe,k,j,m) = grid%tracer(ips:ipe,k,j,m)
                 end do
               end do
             end do
           case( 'dyn' )
             call pack_dyn_vals
           case( 'msc' )
             n_msc_buf = 1
             do m = 1,num_msc
               if( St_Vars_msk(m) ) then
                 n_msc_buf = n_msc_buf + 1
                 St_Vars_buf_ndx(n_msc_buf) = m
                 do j = jps,jpe
                   do k = kps,kpe
                     chem(ips:ipe,k,j,n_msc_buf) = St_Vars(m)%rfield_3d(ips:ipe,k,j)
                   end do
                 end do
               endif
             end do
         end select
#ifdef DM_PARALLEL
!-----------------------------------------------------------------------------
!  any trajectories in "gap" between patches?
!-----------------------------------------------------------------------------
             is_in_patch_gap = any( glb_traj_proc(:n_traj) == 0 .and. pkg_is_active(:n_traj,pkg) )
             if( is_in_patch_gap ) then
!              write(err_mes,'(''glb_traj_proc mask = '',10i5)') glb_traj_proc(:n_traj)
!              call wrf_debug( 0,trim(err_mes) )
               select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
                 case( 'chm' )
#      include "HALO_EM_CHEM_E_3.inc"
                 case( 'dchm' )
                   num_chem_sav = num_chem
                   num_chem     = n_dchm
#      include "HALO_EM_CHEM_E_3.inc"
                   num_chem = num_chem_sav
#endif
                 case( 'hyd' )
#      include "HALO_EM_MOIST_E_3.inc"
                 case( 'trc' )
#      include "HALO_EM_TRACER_E_3.inc"
                 case( 'dyn' )
                   num_chem_sav = num_chem
                   num_chem     = dyn_max + offset + 2
#      include "HALO_EM_CHEM_E_3.inc"
                   num_chem = num_chem_sav
                 case( 'msc' )
                   num_chem_sav = num_chem
                   num_chem     = n_msc_buf
#      include "HALO_EM_CHEM_E_3.inc"
                   num_chem = num_chem_sav
               end select
             endif
#endif

traj_loop: &
         do trj = 1,n_traj
           select case( trim(pkg_tag(pkg)) )
             case( 'chm' )
               n_vars = traject(trj,dm)%n_chm_var
             case( 'hyd' )
               n_vars = traject(trj,dm)%n_hyd_var
             case( 'trc' )
               n_vars = traject(trj,dm)%n_trc_var
             case( 'dyn' )
               n_vars = traject(trj,dm)%n_dyn_var
             case( 'msc' )
               n_vars = traject(trj,dm)%n_msc_var
             case( 'dchm' )
               n_vars = traject(trj,dm)%n_dchm_var
           end select
pkg_is_active_in_traj: &
           if( pkg_is_active(trj,pkg) ) then
             select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
               case( 'chm', 'dchm' )
                 wrk4d => chem
#endif
               case( 'dyn', 'msc' )
                 wrk4d => chem
               case( 'hyd' )
                 wrk4d => moist
               case( 'trc' )
                 wrk4d => tracer
             end select
in_patch:    if( traj_proc(trj) == mytask+1 ) then
               x = grid%traj_i(trj)
               y = grid%traj_j(trj)
               zi = grid%traj_k(trj)
               il = int( x ) ; iu = il + 1
               jl = int( y ) ; ju = jl + 1
               kl = int( zi ) ; ku = kl + 1
               delsx = x - floor(x) ; o_delsx = 1. - delsx
               delsy = y - floor(y) ; o_delsy = 1. - delsy
               delsz = zi - floor(zi) ; o_delsz = 1. - delsz
var_loop:      do n = 1,n_vars
                 found = .true.
                 select case( trim(pkg_tag(pkg)) )
                   case( 'chm' )
                     ndx = traject(trj,dm)%chm_ndx(n)
                   case( 'hyd' )
                     ndx = traject(trj,dm)%hyd_ndx(n)
                   case( 'trc' )
                     ndx = traject(trj,dm)%trc_ndx(n)
                   case( 'dyn' )
                     ndx = 1
                     call set_dyn_vals
                   case( 'msc' )
                     found = .false.
                     St_Vars_ndx = trjects(trj)%msc_ndx(n) - offset
                     do ndx = param_first_scalar,n_msc_buf
                       if( St_Vars_ndx == St_Vars_buf_ndx(ndx) ) then
                         found = .true.
                         exit
                       endif
                     end do
                     if( found ) then
                       call set_msc_vals
                     endif
                     ndx = 1
                   case( 'dchm' )
                     ndx = trjects(trj)%dchm_ndx(n)
                 end select
                 if( found ) then
                   horz_conc(1) = o_delsx*o_delsy*wrk4d(il,kl,jl,ndx) + o_delsy*delsx*wrk4d(iu,kl,jl,ndx) &
                                + delsy*o_delsx*wrk4d(il,kl,ju,ndx) + delsx*delsy*wrk4d(iu,kl,ju,ndx)
                   horz_conc(2) = o_delsx*o_delsy*wrk4d(il,ku,jl,ndx) + o_delsy*delsx*wrk4d(iu,ku,jl,ndx) &
                                + delsy*o_delsx*wrk4d(il,ku,ju,ndx) + delsx*delsy*wrk4d(iu,ku,ju,ndx)
                   traject(trj,dm)%traj_var(n) = delsz*horz_conc(2) + o_delsz*horz_conc(1)
                 else
                   traject(trj,dm)%traj_var(n) = missing_val
                 endif
               end do var_loop
             else in_patch
               traject(trj,dm)%traj_var(:n_vars) = missing_val
             endif in_patch
             traj_conc => traj_val(:,trj)
             do n = 1,n_vars
#ifdef DM_PARALLEL
               max_conc = wrf_dm_max_real( traject(trj,dm)%traj_var(n) )
#else
               max_conc = traject(trj,dm)%traj_var(n)
#endif
               if( is_root_proc ) then
                 traj_conc(n) = max_conc
               endif
             end do
           else pkg_is_active_in_traj
             if( is_root_proc .and. n_vars > 0 ) then
               traj_conc => traj_val(:,trj)
               traj_conc(:n_vars) = missing_val
             endif
           endif pkg_is_active_in_traj
!-----------------------------------------------------------------------------
!  buffer traj chm,trc,hyb,msc,dchm variables
!-----------------------------------------------------------------------------
           if( is_root_proc .and. n_vars > 0 ) then
             select case( pkg_tag(pkg) )
#if( WRF_CHEM == 1 )
               case( 'chm' )
                 trj_pbf(trj)%chm_vals(n_vals,:n_vars) = traj_conc(:n_vars)
               case( 'dchm' )
                 trj_pbf(trj)%dchm_vals(n_vals,:n_vars) = traj_conc(:n_vars)
#endif
               case( 'hyd' )
                 trj_pbf(trj)%hyd_vals(n_vals,:n_vars) = traj_conc(:n_vars)
               case( 'trc' )
                 trj_pbf(trj)%trc_vals(n_vals,:n_vars) = traj_conc(:n_vars)
               case( 'dyn' )
                 trj_pbf(trj)%dyn_vals(n_vals,:n_vars) = traj_conc(:n_vars)
               case( 'msc' )
                 trj_pbf(trj)%msc_vals(n_vals,:n_vars) = traj_conc(:n_vars)
             end select
           endif
         end do traj_loop
         if( allocated( chem ) ) then
           deallocate( chem )
         endif
         if( allocated( moist ) ) then
           deallocate( moist )
         endif
         if( allocated( tracer ) ) then
           deallocate( tracer )
         endif
         if( allocated( St_Vars_buf_ndx ) ) then
           deallocate( St_Vars_buf_ndx )
         endif
         if( pkg == dchm_pkg .and. allocated( dchm_buff ) ) then
           deallocate( dchm_buff )
         endif
       else pkg_has_active_traj
         if( is_root_proc ) then
           do trj = 1,n_traj
             select case( trim(pkg_tag(pkg)) )
#if( WRF_CHEM == 1 )
               case( 'chm' )
                 n_vars = traject(trj,dm)%n_chm_var
               case( 'dchm' )
                 n_vars = traject(trj,dm)%n_dchm_var
#endif
               case( 'hyd' )
                 n_vars = traject(trj,dm)%n_hyd_var
               case( 'trc' )
                 n_vars = traject(trj,dm)%n_trc_var
               case( 'dyn' )
                 n_vars = traject(trj,dm)%n_dyn_var
               case( 'msc' )
                 n_vars = traject(trj,dm)%n_msc_var
             end select
             if( n_vars > 0 ) then
               select case( pkg_tag(pkg) )
#if( WRF_CHEM == 1 )
                 case( 'chm' )
                   trj_pbf(trj)%chm_vals(n_vals,:n_vars) = missing_val
                 case( 'dchm' )
                   trj_pbf(trj)%dchm_vals(n_vals,:n_vars) = dchm_fill_val(trj)
#endif
                 case( 'hyd' )
                   trj_pbf(trj)%hyd_vals(n_vals,:n_vars) = missing_val
                 case( 'trc' )
                   trj_pbf(trj)%trc_vals(n_vals,:n_vars) = missing_val
                 case( 'dyn' )
                   trj_pbf(trj)%dyn_vals(n_vals,:n_vars) = missing_val
                 case( 'msc' )
                   trj_pbf(trj)%msc_vals(n_vals,:n_vars) = missing_val
               end select
             endif
           end do
         endif
       endif pkg_has_active_traj
     end do pkg_loop
!-----------------------------------------------------------------------------
!  output trajectory buffer
!-----------------------------------------------------------------------------
     if( is_root_proc ) then
       flsh_buff = n_vals == vals_max .or. domain_last_time_step( grid )
       if( flsh_buff ) then
         call trajectory_write_file( n_traj, n_vals, dm )
       endif
     endif
   endif has_trajectories

   CONTAINS

   subroutine pack_dyn_vals

   integer :: mp1

   do m = 1,dyn_max+2
     mp1 = m + 1
     select case( m )
       case( 1 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%p(ips:ipe,k,j)
           end do
         end do
       case( 2 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%t_2(ips:ipe,k,j)
           end do
         end do
       case( 3 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%pb(ips:ipe,k,j)
           end do
         end do
       case( 4 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%u_2(ips:ipe,k,j)
           end do
         end do
       case( 5 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%v_2(ips:ipe,k,j)
           end do
         end do
       case( 6 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%w_2(ips:ipe,k,j)
           end do
         end do
       case( 7 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%ph_2(ips:ipe,k,j)
           end do
         end do
       case( 8 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%phb(ips:ipe,k,j)
           end do
         end do
#if( WRF_CHEM == 1 )
       case( 9 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%rainprod(ips:ipe,k,j)
           end do
         end do
       case( 10 )
         do j = jps,jpe
           do k = kps,kpe
             chem(ips:ipe,k,j,mp1) = grid%evapprod(ips:ipe,k,j)
           end do
         end do
#endif
     end select
   end do

   end subroutine pack_dyn_vals

   subroutine set_dyn_vals

   use module_model_constants, only : g, t0, p1000mb, rcp

   integer :: ilp1, iup1, jlp1, jup1, klp1, kup1
   real    :: ginv, pinv

   select case( traject(trj,dm)%dyn_var(n) )
     case( 'p' )
       wrk4d(il,kl,jl,1) = chem(il,kl,jl,2) + chem(il,kl,jl,4)
       wrk4d(iu,kl,jl,1) = chem(iu,kl,jl,2) + chem(iu,kl,jl,4)
       wrk4d(il,ku,jl,1) = chem(il,ku,jl,2) + chem(il,ku,jl,4)
       wrk4d(iu,ku,jl,1) = chem(iu,ku,jl,2) + chem(iu,ku,jl,4)
       wrk4d(il,kl,ju,1) = chem(il,kl,ju,2) + chem(il,kl,ju,4)
       wrk4d(iu,kl,ju,1) = chem(iu,kl,ju,2) + chem(iu,kl,ju,4)
       wrk4d(il,ku,ju,1) = chem(il,ku,ju,2) + chem(il,ku,ju,4)
       wrk4d(iu,ku,ju,1) = chem(iu,ku,ju,2) + chem(iu,ku,ju,4)
     case( 'T' )
       wrk4d(il,kl,jl,1) = chem(il,kl,jl,2) + chem(il,kl,jl,4)
       wrk4d(iu,kl,jl,1) = chem(iu,kl,jl,2) + chem(iu,kl,jl,4)
       wrk4d(il,ku,jl,1) = chem(il,ku,jl,2) + chem(il,ku,jl,4)
       wrk4d(iu,ku,jl,1) = chem(iu,ku,jl,2) + chem(iu,ku,jl,4)
       wrk4d(il,kl,ju,1) = chem(il,kl,ju,2) + chem(il,kl,ju,4)
       wrk4d(iu,kl,ju,1) = chem(iu,kl,ju,2) + chem(iu,kl,ju,4)
       wrk4d(il,ku,ju,1) = chem(il,ku,ju,2) + chem(il,ku,ju,4)
       wrk4d(iu,ku,ju,1) = chem(iu,ku,ju,2) + chem(iu,ku,ju,4)

       pinv = 1./p1000mb
       wrk4d(il,kl,jl,1) = (chem(il,kl,jl,3) + t0)*(wrk4d(il,kl,jl,1)*pinv)**rcp
       wrk4d(iu,kl,jl,1) = (chem(iu,kl,jl,3) + t0)*(wrk4d(iu,kl,jl,1)*pinv)**rcp
       wrk4d(il,ku,jl,1) = (chem(il,ku,jl,3) + t0)*(wrk4d(il,ku,jl,1)*pinv)**rcp
       wrk4d(iu,ku,jl,1) = (chem(iu,ku,jl,3) + t0)*(wrk4d(iu,ku,jl,1)*pinv)**rcp
       wrk4d(il,kl,ju,1) = (chem(il,kl,ju,3) + t0)*(wrk4d(il,kl,ju,1)*pinv)**rcp
       wrk4d(iu,kl,ju,1) = (chem(iu,kl,ju,3) + t0)*(wrk4d(iu,kl,ju,1)*pinv)**rcp
       wrk4d(il,ku,ju,1) = (chem(il,ku,ju,3) + t0)*(wrk4d(il,ku,ju,1)*pinv)**rcp
       wrk4d(iu,ku,ju,1) = (chem(iu,ku,ju,3) + t0)*(wrk4d(iu,ku,ju,1)*pinv)**rcp
     case( 'z' )
       klp1 = kl + 1 ; kup1 = ku + 1
       ginv = 1./g
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,8) + chem(il,klp1,jl,8) &
                               + chem(il,kl,jl,9) + chem(il,klp1,jl,9))*ginv
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,8) + chem(iu,klp1,jl,8) &
                               + chem(iu,kl,jl,9) + chem(iu,klp1,jl,9))*ginv
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,8) + chem(il,kup1,jl,8) &
                               + chem(il,ku,jl,9) + chem(il,kup1,jl,9))*ginv
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,8) + chem(iu,kup1,jl,8) &
                               + chem(iu,ku,jl,9) + chem(iu,kup1,jl,9))*ginv
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,8) + chem(il,klp1,ju,8) &
                               + chem(il,kl,ju,9) + chem(il,klp1,ju,9))*ginv
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,8) + chem(iu,klp1,ju,8) &
                               + chem(iu,kl,ju,9) + chem(iu,klp1,ju,9))*ginv
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,8) + chem(il,kup1,ju,8) &
                               + chem(il,ku,ju,9) + chem(il,kup1,ju,9))*ginv
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,8) + chem(iu,kup1,ju,8) &
                               + chem(iu,ku,ju,9) + chem(iu,kup1,ju,9))*ginv
     case( 'u' )
       ilp1 = il + 1 ; iup1 = iu + 1
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,5) + chem(ilp1,kl,jl,5))
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,5) + chem(iup1,kl,jl,5))
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,5) + chem(ilp1,ku,jl,5))
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,5) + chem(iup1,ku,jl,5))
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,5) + chem(ilp1,kl,ju,5))
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,5) + chem(iup1,kl,ju,5))
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,5) + chem(ilp1,ku,ju,5))
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,5) + chem(iup1,ku,ju,5))
     case( 'v' )
       jlp1 = jl + 1 ; jup1 = ju + 1
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,6) + chem(il,kl,jlp1,6))
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,6) + chem(iu,kl,jlp1,6))
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,6) + chem(il,ku,jlp1,6))
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,6) + chem(iu,ku,jlp1,6))
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,6) + chem(il,kl,jup1,6))
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,6) + chem(iu,kl,jup1,6))
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,6) + chem(il,ku,jup1,6))
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,6) + chem(iu,ku,jup1,6))
     case( 'w' )
       klp1 = kl + 1 ; kup1 = ku + 1
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,7) + chem(il,klp1,jl,7))
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,7) + chem(iu,klp1,jl,7))
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,7) + chem(il,kup1,jl,7))
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,7) + chem(iu,kup1,jl,7))
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,7) + chem(il,klp1,ju,7))
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,7) + chem(iu,klp1,ju,7))
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,7) + chem(il,kup1,ju,7))
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,7) + chem(iu,kup1,ju,7))
#if( WRF_CHEM == 1 )
     case( 'rainprod' )
       wrk4d(il,kl,jl,1) = chem(il,kl,jl,10)
       wrk4d(iu,kl,jl,1) = chem(iu,kl,jl,10)
       wrk4d(il,ku,jl,1) = chem(il,ku,jl,10)
       wrk4d(iu,ku,jl,1) = chem(iu,ku,jl,10)
       wrk4d(il,kl,ju,1) = chem(il,kl,ju,10)
       wrk4d(iu,kl,ju,1) = chem(iu,kl,ju,10)
       wrk4d(il,ku,ju,1) = chem(il,ku,ju,10)
       wrk4d(iu,ku,ju,1) = chem(iu,ku,ju,10)
     case( 'evapprod' )
       wrk4d(il,kl,jl,1) = chem(il,kl,jl,11)
       wrk4d(iu,kl,jl,1) = chem(iu,kl,jl,11)
       wrk4d(il,ku,jl,1) = chem(il,ku,jl,11)
       wrk4d(iu,ku,jl,1) = chem(iu,ku,jl,11)
       wrk4d(il,kl,ju,1) = chem(il,kl,ju,11)
       wrk4d(iu,kl,ju,1) = chem(iu,kl,ju,11)
       wrk4d(il,ku,ju,1) = chem(il,ku,ju,11)
       wrk4d(iu,ku,ju,1) = chem(iu,ku,ju,11)
#endif
   end select

   end subroutine set_dyn_vals

   subroutine set_msc_vals

   integer :: ilp1, iup1, jlp1, jup1, klp1, kup1

   select case( trim(St_Vars(St_Vars_ndx)%Stagger) )
     case( 'X' )
       ilp1 = il + 1 ; iup1 = iu + 1
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,ndx) + chem(ilp1,kl,jl,ndx))
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,ndx) + chem(iup1,kl,jl,ndx))
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,ndx) + chem(ilp1,ku,jl,ndx))
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,ndx) + chem(iup1,ku,jl,ndx))
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,ndx) + chem(ilp1,kl,ju,ndx))
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,ndx) + chem(iup1,kl,ju,ndx))
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,ndx) + chem(ilp1,ku,ju,ndx))
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,ndx) + chem(iup1,ku,ju,ndx))
     case( 'Y' )
       jlp1 = jl + 1 ; jup1 = ju + 1
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,ndx) + chem(il,kl,jlp1,ndx))
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,ndx) + chem(iu,kl,jlp1,ndx))
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,ndx) + chem(il,ku,jlp1,ndx))
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,ndx) + chem(iu,ku,jlp1,ndx))
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,ndx) + chem(il,kl,jup1,ndx))
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,ndx) + chem(iu,kl,jup1,ndx))
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,ndx) + chem(il,ku,jup1,ndx))
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,ndx) + chem(iu,ku,jup1,ndx))
     case( 'Z' )
       klp1 = kl + 1 ; kup1 = ku + 1
       wrk4d(il,kl,jl,1) = .5*(chem(il,kl,jl,ndx) + chem(il,klp1,jl,ndx))
       wrk4d(iu,kl,jl,1) = .5*(chem(iu,kl,jl,ndx) + chem(iu,klp1,jl,ndx))
       wrk4d(il,ku,jl,1) = .5*(chem(il,ku,jl,ndx) + chem(il,kup1,jl,ndx))
       wrk4d(iu,ku,jl,1) = .5*(chem(iu,ku,jl,ndx) + chem(iu,kup1,jl,ndx))
       wrk4d(il,kl,ju,1) = .5*(chem(il,kl,ju,ndx) + chem(il,klp1,ju,ndx))
       wrk4d(iu,kl,ju,1) = .5*(chem(iu,kl,ju,ndx) + chem(iu,klp1,ju,ndx))
       wrk4d(il,ku,ju,1) = .5*(chem(il,ku,ju,ndx) + chem(il,kup1,ju,ndx))
       wrk4d(iu,ku,ju,1) = .5*(chem(iu,ku,ju,ndx) + chem(iu,kup1,ju,ndx))
     case default
       wrk4d(il,kl,jl,1) = chem(il,kl,jl,ndx)
       wrk4d(iu,kl,jl,1) = chem(iu,kl,jl,ndx)
       wrk4d(il,ku,jl,1) = chem(il,ku,jl,ndx)
       wrk4d(iu,ku,jl,1) = chem(iu,ku,jl,ndx)
       wrk4d(il,kl,ju,1) = chem(il,kl,ju,ndx)
       wrk4d(iu,kl,ju,1) = chem(iu,kl,ju,ndx)
       wrk4d(il,ku,ju,1) = chem(il,ku,ju,ndx)
       wrk4d(iu,ku,ju,1) = chem(iu,ku,ju,ndx)
   end select

   end subroutine set_msc_vals

   end subroutine trajectory_driver

   subroutine trajectory_dchm_tstep_init( grid, is_chemstep )
!-----------------------------------------------------------------------------
!  initialize dchm buffer
!-----------------------------------------------------------------------------
   use module_domain, only : domain, get_ijk_from_grid
   use module_state_description, only : num_chem

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   logical, intent(in)      :: is_chemstep
   type(domain), intent(in) :: grid

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: astat
   integer :: j, k, m
   integer :: chm_ndx
   integer :: dm
   integer :: ims,ime, jms,jme, kms,kme
   integer :: ids,ide, jds,jde, kds,kde
   integer :: ips,ipe, jps,jpe, kps,kpe
   integer, pointer :: n_dchm       ! number dchm buffer species
   integer, pointer :: dchm_buf_ndx(:)
   character(len=256) :: err_mes

   do_chemstep = is_chemstep

   if( is_chemstep ) then
     dm = grid%id
     n_dchm => n_dchm_dm(dm)
     if( n_dchm > 0 ) then
       call get_ijk_from_grid( grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )
       if( allocated( dchm_buff ) ) then
         deallocate( dchm_buff )
       endif
       allocate( dchm_buff(ims:ime,kms:kme,jms:jme,n_dchm+offset),stat=astat )
       if( astat /= 0 ) then
         write(err_mes,'(''trajectory_dchm_tstep_init('',i2.2,''): failed to allocate wrk4d: error = '',i6)') dm,astat
         call wrf_error_fatal( trim( err_mes  ) )
       endif
       dchm_buf_ndx => dchm_buf_ndx_dm(:,dm)
       do m = 1,n_dchm
         chm_ndx = dchm_buf_ndx(m)
         do j = jps,jpe
           do k = kps,kpe
             dchm_buff(ips:ipe,k,j,m+offset) = grid%chem(ips:ipe,k,j,chm_ndx)
           end do
         end do
       end do
     endif
   endif

   end subroutine trajectory_dchm_tstep_init

   subroutine trajectory_dchm_tstep_set( grid )
!-----------------------------------------------------------------------------
!  set dchm buffer
!-----------------------------------------------------------------------------
   use module_domain, only : domain, get_ijk_from_grid
   use module_state_description, only : num_chem

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   type(domain), intent(in) :: grid

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: j, k, m, mp1
   integer :: chm_ndx
   integer :: dm
   integer :: ims,ime, jms,jme, kms,kme
   integer :: ids,ide, jds,jde, kds,kde
   integer :: ips,ipe, jps,jpe, kps,kpe
   integer, pointer :: n_dchm       ! number dchm buffer species
   integer, pointer :: dchm_buf_ndx(:)

   dm = grid%id
   n_dchm => n_dchm_dm(dm)
   if( n_dchm > 0 ) then
     call get_ijk_from_grid( grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     dchm_buf_ndx => dchm_buf_ndx_dm(:,dm)
     do m = 1,n_dchm
       mp1 = m + offset
       chm_ndx = dchm_buf_ndx(m)
       do j = jps,jpe
         do k = kps,kpe
           dchm_buff(ips:ipe,k,j,mp1) = grid%chem(ips:ipe,k,j,chm_ndx) - dchm_buff(ips:ipe,k,j,mp1)
         end do
       end do
     end do
   endif

   end subroutine trajectory_dchm_tstep_set

   subroutine trajectory_create_file( grid, n_traj )
!-----------------------------------------------------------------------------
!  create trajectory netcdf file
!-----------------------------------------------------------------------------
   use module_domain
   use module_state_description, only : param_first_scalar, num_chem, num_moist, num_tracer
   use module_scalar_tables,     only : chem_dname_table, moist_dname_table, tracer_dname_table

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   integer, intent(in)       :: n_traj
   type(domain), intent(in)  :: grid

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: dm
   integer :: ncid, ios
   integer :: traj_dim, time_dim, Times_dim
   integer :: varid
   integer, pointer :: num_msc
   integer :: var_dims(2)
   integer :: m, n, trj, pkg
   character(len=10)  :: coord_name(5) = (/ 'traj_i    ', 'traj_j    ', 'traj_k    ', &
                                            'traj_long ', 'traj_lat  ' /)
   character(len=256) :: filename
   character(len=256) :: var_name
   character(len=256) :: err_mes
   character(len=256) :: description
   character(len=256) :: units

   logical, external :: wrf_dm_on_monitor

include 'netcdf.inc'

master_proc: &
   if( wrf_dm_on_monitor() ) then
     dm = grid%id
     write(filename,'(''wrfout_traj_d'',i2.2)',iostat=ios) dm
     if( ios /= 0 ) then
       write(err_mes,'(''trajectory_create_file: failed to set filename: error = '',i6)') ios
       call wrf_error_fatal( trim( err_mes  ) )
     endif
!    ios = nf_create( trim(filename), or(nf_clobber,nf_netcdf4), ncid )
     ios = nf_create( trim(filename), nf_clobber, ncid )
     if( ios /= nf_noerr ) then
       write(err_mes,'(''trajectory_create_file: failed to create '',a,'': error = '',i6)') trim(filename),ios
       call wrf_error_fatal( trim( err_mes  ) )
     endif
!-----------------------------------------------------------------------------
!  define dimensions
!-----------------------------------------------------------------------------
     err_mes = 'trajectory_create_file: failed to create traj dimension'
     call handle_ncerr( nf_def_dim( ncid, 'traj', n_traj, traj_dim ), trim(err_mes) )
     err_mes = 'trajectory_create_file: failed to create time dimension'
     call handle_ncerr( nf_def_dim( ncid, 'time', nf_unlimited, time_dim ), trim(err_mes) )
     err_mes = 'trajectory_create_file: failed to create Times dimension'
     call handle_ncerr( nf_def_dim( ncid, 'DateStrLen', 19, Times_dim ), trim(err_mes) )
!-----------------------------------------------------------------------------
!  define variables
!-----------------------------------------------------------------------------
     var_dims(:) = (/ Times_dim,time_dim /)
     err_mes = 'trajectory_create_file: failed to create Times variable'
     call handle_ncerr( nf_def_var( ncid, 'Times', nf_char, 2, var_dims, varid ), trim(err_mes) )

!-----------------------------------------------------------------------------
!  first the coordinate variables
!-----------------------------------------------------------------------------
     var_dims(:) = (/ traj_dim,time_dim /)
     do m = 1,5
       var_name = coord_name(m)
       err_mes = 'trajectory_create_file: failed to create ' // trim(var_name) // ' variable'
       call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
     end do
!-----------------------------------------------------------------------------
!  then the species variables
!-----------------------------------------------------------------------------
     num_msc => num_msc_dm(dm)
pgk_loop: &
     do pkg = 1,pkg_max
       select case( pkg_tag(pkg) )
         case( 'chm' )
           trj_msk => trj_msk_dm(:,:,chm_pkg,dm)
           if( any( trj_msk(:n_traj,1) ) ) then
             call def_vars( 'chm' )
           endif
         case( 'hyd' )
           trj_msk => trj_msk_dm(:,:,hyd_pkg,dm)
           if( any( trj_msk(:n_traj,1) ) ) then
             call def_vars( 'hyd' )
           endif
         case( 'trc' )
           trj_msk => trj_msk_dm(:,:,trc_pkg,dm)
           if( any( trj_msk(:n_traj,1) ) ) then
             call def_vars( 'trc' )
           endif
         case( 'dyn' )
           trj_msk => trj_msk_dm(:,:,dyn_pkg,dm)
           if( any( trj_msk(:n_traj,1) ) ) then
             call def_vars( 'dyn' )
           endif
         case( 'msc' )
           trj_msk => trj_msk_dm(:,:,msc_pkg,dm)
           if( any( trj_msk(:n_traj,1) ) ) then
             call def_vars( 'msc' )
           endif
         case( 'dchm' )
           trj_msk => trj_msk_dm(:,:,dchm_pkg,dm)
           if( any( trj_msk(:n_traj,1) ) ) then
             call def_vars( 'dchm' )
           endif
       end select
     end do pgk_loop

     err_mes = 'trajectory_create_file: failed to end definition for file ' // trim(filename)
     call handle_ncerr( nf_enddef( ncid ), trim(err_mes) )
     err_mes = 'trajectory_create_file: failed to close file ' // trim(filename)
     call handle_ncerr( nf_close( ncid ), trim(err_mes) )
   endif master_proc

   CONTAINS

   subroutine def_vars( var_type )

   character(len=*), intent(in)  :: var_type

   integer :: m, ndx, trj
   integer, pointer  :: n_dchm
   character(len=32) :: spc_name

   select case( var_type )
     case( 'chm' )
       trj_msk => trj_msk_dm(:,:,chm_pkg,dm)
       do n = param_first_scalar,num_chem
         if( any( trj_msk(:n_traj,n) ) ) then
           spc_name = chem_dname_table(dm,n)
           write(var_name,'(a,''_traj'')') trim(spc_name)
           err_mes = 'def_vars: failed to create ' // trim(var_name) // ' variable'
           call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
           description = trim(var_name) // ' mixing ratio'
           call handle_ncerr( nf_put_att_text( ncid, varid, 'description', len_trim(description), description ), trim(err_mes) )
           units = 'ppmv'
trj_loop:  do trj = 1,n_traj
             if( trj_msk(trj,n) ) then
               do m = 1,trjects(trj)%n_chm_var
                 if( trim(trjects(trj)%chm_spc(m)) == trim(spc_name) ) then
                   if( .not. trjects(trj)%chm_is_gas(m) ) then
                     units = 'ug/kg-dryair'
                   endif
                   exit trj_loop
                 endif
               end do
               exit trj_loop
             endif
           end do trj_loop
           call handle_ncerr( nf_put_att_text( ncid, varid, 'units', len_trim(units), units ), trim(err_mes) )
         endif
       end do
     case( 'hyd' )
       trj_msk => trj_msk_dm(:,:,hyd_pkg,dm)
       do n = param_first_scalar,num_moist
         if( any( trj_msk(:n_traj,n) ) ) then
           write(var_name,'(a,''_traj'')') trim(moist_dname_table(dm,n))
           err_mes = 'def_vars: failed to create ' // trim(var_name) // ' variable'
           call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
           description = trim(var_name) // ' mixing ratio'
           call handle_ncerr( nf_put_att_text( ncid, varid, 'description', len_trim(description), description ), trim(err_mes) )
           units = 'ug/kg-dryair'
           call handle_ncerr( nf_put_att_text( ncid, varid, 'units', len_trim(units), units ), trim(err_mes) )
         endif
       end do
     case( 'trc' )
       trj_msk => trj_msk_dm(:,:,trc_pkg,dm)
       do n = param_first_scalar,num_tracer
         if( any( trj_msk(:n_traj,n) ) ) then
           write(var_name,'(a,''_traj'')') trim(tracer_dname_table(dm,n))
           err_mes = 'def_vars: failed to create ' // trim(var_name) // ' variable'
           call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
           description = trim(var_name) // ' mixing ratio'
           call handle_ncerr( nf_put_att_text( ncid, varid, 'description', len_trim(description), description ), trim(err_mes) )
           units = ' '
           call handle_ncerr( nf_put_att_text( ncid, varid, 'units', len_trim(units), units ), trim(err_mes) )
         endif
       end do
     case( 'dyn' )
       trj_msk => trj_msk_dm(:,:,dyn_pkg,dm)
       do n = param_first_scalar,dyn_max + offset
         if( any( trj_msk(:n_traj,n) ) ) then
           write(var_name,'(a,''_traj'')') trim(dyn_var_lst(n-1))
           err_mes = 'def_vars: failed to create ' // trim(var_name) // ' variable'
           call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
           description = trim(dyn_var_desc_att(n-1))
           call handle_ncerr( nf_put_att_text( ncid, varid, 'description', len_trim(description), description ), trim(err_mes) )
           units = trim(dyn_var_unit_att(n-1))
           call handle_ncerr( nf_put_att_text( ncid, varid, 'units', len_trim(units), units ), trim(err_mes) )
         endif
       end do
     case( 'msc' )
       trj_msk => trj_msk_dm(:,:,msc_pkg,dm)
       do n = param_first_scalar,num_msc + offset
         if( any( trj_msk(:n_traj,n) ) ) then
           write(var_name,'(a,''_traj'')') trim(St_Vars(n-1)%Varname)
           err_mes = 'def_vars: failed to create ' // trim(var_name) // ' variable'
           call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
           description = trim(St_Vars(n-1)%Description)
           call handle_ncerr( nf_put_att_text( ncid, varid, 'description', len_trim(description), description ), trim(err_mes) )
           units = trim(St_Vars(n-1)%Units)
           call handle_ncerr( nf_put_att_text( ncid, varid, 'units', len_trim(units), units ), trim(err_mes) )
         endif
       end do
     case( 'dchm' )
       trj_msk => trj_msk_dm(:,:,dchm_pkg,dm)
       n_dchm  => n_dchm_dm(dm)
       dchm_buf_ndx => dchm_buf_ndx_dm(:,dm)
       do n = 1,n_dchm
         if( any( trj_msk(:n_traj,n+offset) ) ) then
           ndx = dchm_buf_ndx(n)
           spc_name = 'dchm_' // trim(chem_dname_table(dm,ndx))
           write(var_name,'(a,''_traj'')') trim(spc_name)
           err_mes = 'def_vars: failed to create ' // trim(var_name) // ' variable'
           call handle_ncerr( nf_def_var( ncid, trim(var_name), nf_real, 2, var_dims, varid ), trim(err_mes) )
           description = trim(var_name) // ' mixing ratio'
           call handle_ncerr( nf_put_att_text( ncid, varid, 'description', len_trim(description), description ), trim(err_mes) )
           units = 'ppmv'
           call handle_ncerr( nf_put_att_text( ncid, varid, 'units', len_trim(units), units ), trim(err_mes) )
         endif
       end do
   end select

   end subroutine def_vars

   end subroutine trajectory_create_file

   subroutine trajectory_write_file( n_traj, n_vals, dm )
!-----------------------------------------------------------------------------
!  create trajectory netcdf file
!-----------------------------------------------------------------------------
   use module_domain
   use module_state_description, only : param_first_scalar, num_chem, num_moist, num_tracer
   use module_scalar_tables,     only : chem_dname_table, moist_dname_table, tracer_dname_table

!-----------------------------------------------------------------------------
!  dummy arguments
!-----------------------------------------------------------------------------
   integer, intent(in)        :: n_traj
   integer, intent(inout)     :: n_vals
   integer, intent(in)        :: dm

!-----------------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------------
   integer :: ncid
   integer :: astat, ios
   integer :: time_id
   integer :: varid
   integer :: l, m, n, trj, pkg, spc, spcp1
   integer :: time_ndx
   integer :: buf_ndx
   integer :: ndx
   integer, pointer :: num_msc      ! number misc species
   integer, pointer :: n_dchm       ! total number of dchm species in domain
   real, allocatable :: holder(:,:)
   character(len=10)  :: coord_name(5) = (/ 'traj_i    ', 'traj_j    ', 'traj_k    ', &
                                            'traj_long ', 'traj_lat  ' /)
   character(len=256) :: var_name
   character(len=256) :: err_mes
   character(len=256) :: filename
   character(len=256) :: spcname

   logical :: found

include 'netcdf.inc'

!---------------------------------------------------------------------
!  open netcdf file
!---------------------------------------------------------------------
   write(filename,'(''wrfout_traj_d'',i2.2)',iostat=ios) dm
   if( ios /= 0 ) then
     write(err_mes,'(''trajectory_write_file: failed to set filename: error = '',i6)') ios
     call wrf_error_fatal( trim( err_mes  ) )
   endif
   ios = nf_open( trim(filename), nf_write, ncid )
   if( ios /= 0 ) then
     write(err_mes,'(''trajectory_write_file: failed to open '',a,'': error = '',i6)') trim(filename),ios
     call wrf_error_fatal( trim( err_mes  ) )
   endif

!---------------------------------------------------------------------
!  allocate wrking array
!---------------------------------------------------------------------
   allocate( holder(n_traj,n_vals),stat=astat )
   if( astat /= 0 ) then
     write(err_mes,'(''trajectory_write_file: failed to allocate holder; error = '',i6)') astat
     call wrf_error_fatal( trim( err_mes  ) )
   endif

!---------------------------------------------------------------------
!  get current time index
!---------------------------------------------------------------------
   err_mes = 'trajectory_write_file: failed to get time id'
   call handle_ncerr( nf_inq_dimid( ncid, 'time', time_id ),trim(err_mes) )
   err_mes = 'trajectory_write_file: failed to get time dimension'
   call handle_ncerr( nf_inq_dimlen( ncid, time_id, time_ndx ),trim(err_mes) )
   time_ndx = time_ndx + 1

!---------------------------------------------------------------------
!  write out trajectory times
!---------------------------------------------------------------------
   err_mes = 'trajectory_write_file: failed to get Times id'
   call handle_ncerr( nf_inq_varid( ncid, 'Times', varid ),trim(err_mes) )
   err_mes = 'trajectory_write_file: failed to write Times'
   call handle_ncerr( nf_put_vara_text( ncid, varid, (/ 1,time_ndx /), (/ 19,n_vals /), trj_pbf(1)%times(:n_vals) ), trim(err_mes) )

!---------------------------------------------------------------------
!  write out trajectory coordinates
!---------------------------------------------------------------------
coord_loop: &
   do l = 1,5
     var_name = coord_name(l)
     err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
     call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
     select case( l )
       case( 1 )
         do n = 1,n_traj
           holder(n,:n_vals) = trj_pbf(n)%trj_i(:n_vals)
         end do
       case( 2 )
         do n = 1,n_traj
           holder(n,:n_vals) = trj_pbf(n)%trj_j(:n_vals)
         end do
       case( 3 )
         do n = 1,n_traj
           holder(n,:n_vals) = trj_pbf(n)%trj_k(:n_vals)
         end do
       case( 4 )
         do n = 1,n_traj
           holder(n,:n_vals) = trj_pbf(n)%trj_lons(:n_vals)
         end do
       case( 5 )
         do n = 1,n_traj
           holder(n,:n_vals) = trj_pbf(n)%trj_lats(:n_vals)
         end do
       end select
       err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
       call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                            holder ), trim(err_mes) )
   end do coord_loop

   St_Vars => St_Vars_dm(:,dm)
   St_Vars_msk => St_Vars_msk_dm(:,dm)
   num_msc => num_msc_dm(dm)
!---------------------------------------------------------------------
!  write out trajectory variables
!---------------------------------------------------------------------
pkg_loop: &
   do pkg = 1,pkg_max
     select case( pkg_tag(pkg) )
       case( 'chm' )
         trj_msk => trj_msk_dm(:,:,chm_pkg,dm)
         do spc = param_first_scalar,num_chem
           if( any( trj_msk(:n_traj,spc) ) ) then
             holder(:,:) = missing_val
             do trj = 1,n_traj
               if( trj_msk(trj,spc) ) then
                 buf_ndx = get_spc_buf_ndx( trjects(trj)%n_chm_var, trjects(trj)%chm_spc, chem_dname_table(dm,spc) )
                 if( buf_ndx > 0 ) then
                   holder(trj,:n_vals) = trj_pbf(trj)%chm_vals(:n_vals,buf_ndx)
                 endif
               endif
             end do
             write(var_name,'(a,''_traj'')') trim(chem_dname_table(dm,spc))
             err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
             call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
             err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
             call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                                  holder ),trim(err_mes) )
           endif
         end do
       case( 'hyd' )
         trj_msk => trj_msk_dm(:,:,hyd_pkg,dm)
         do spc = param_first_scalar,num_moist
           if( any( trj_msk(:n_traj,spc) ) ) then
             holder(:,:) = missing_val
             do trj = 1,n_traj
               if( trj_msk(trj,spc) ) then
                 buf_ndx = get_spc_buf_ndx( trjects(trj)%n_hyd_var, trjects(trj)%hyd_spc, moist_dname_table(dm,spc) )
                 if( buf_ndx > 0 ) then
                   holder(trj,:n_vals) = trj_pbf(trj)%hyd_vals(:n_vals,buf_ndx)
                 endif
               endif
             end do
             write(var_name,'(a,''_traj'')') trim(moist_dname_table(dm,spc))
             err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
             call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
             err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
             call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                                  holder ),trim(err_mes) )
           endif
         end do
       case( 'trc' )
         trj_msk => trj_msk_dm(:,:,trc_pkg,dm)
         do spc = param_first_scalar,num_tracer
           if( any( trj_msk(:n_traj,spc) ) ) then
             holder(:,:) = missing_val
             do trj = 1,n_traj
               if( trj_msk(trj,spc) ) then
                 buf_ndx = get_spc_buf_ndx( trjects(trj)%n_trc_var, trjects(trj)%trc_spc, tracer_dname_table(dm,spc) )
                 if( buf_ndx > 0 ) then
                   holder(trj,:n_vals) = trj_pbf(trj)%trc_vals(:n_vals,buf_ndx)
                 endif
               endif
             end do
             write(var_name,'(a,''_traj'')') trim(tracer_dname_table(dm,spc))
             err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
             call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
             err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
             call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                                  holder ),trim(err_mes) )
           endif
         end do
       case( 'dyn' )
         trj_msk => trj_msk_dm(:,:,dyn_pkg,dm)
         do spc = param_first_scalar,dyn_max+offset
           if( any( trj_msk(:n_traj,spc) ) ) then
             holder(:,:) = missing_val
             do trj = 1,n_traj
               if( trj_msk(trj,spc) ) then
                 buf_ndx = get_spc_buf_ndx( trjects(trj)%n_dyn_var, trjects(trj)%dyn_var, dyn_var_lst(spc-offset) )
                 if( buf_ndx > 0 ) then
                   holder(trj,:n_vals) = trj_pbf(trj)%dyn_vals(:n_vals,buf_ndx)
                 endif
               endif
             end do
             write(var_name,'(a,''_traj'')') trim(dyn_var_lst(spc-offset))
             err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
             call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
             err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
             call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                                  holder ),trim(err_mes) )
           endif
         end do
       case( 'msc' )
         trj_msk => trj_msk_dm(:,:,msc_pkg,dm)
         do spc = param_first_scalar,num_msc+offset
           if( any( trj_msk(:n_traj,spc) ) ) then
             holder(:,:) = missing_val
             do trj = 1,n_traj
               if( trj_msk(trj,spc) ) then
                 found = .false.
                 do buf_ndx = 1,traject(trj,dm)%n_msc_var
                   if( traject(trj,dm)%msc_ndx(buf_ndx) == spc ) then
                     found = .true.
                     exit
                   endif
                 end do
                 if( found ) then
                   holder(trj,:n_vals) = trj_pbf(trj)%msc_vals(:n_vals,buf_ndx)
                 endif
               endif
             end do
             write(var_name,'(a,''_traj'')') trim(St_Vars(spc-offset)%Varname)
             err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
             call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
             err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
             call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                                  holder ),trim(err_mes) )
           endif
         end do
       case( 'dchm' )
         dchm_buf_ndx => dchm_buf_ndx_dm(:,dm)
         trj_msk => trj_msk_dm(:,:,dchm_pkg,dm)
         n_dchm  => n_dchm_dm(dm)
         do spc = 1,n_dchm
           spcp1 = spc + 1
           if( any( trj_msk(:n_traj,spcp1) ) ) then
             holder(:,:) = missing_val
             do trj = 1,n_traj
               if( trj_msk(trj,spcp1) ) then
                 buf_ndx = get_dchm_buf_ndx( trjects(trj)%n_dchm_var, trjects(trj)%dchm_ndx, spcp1 )
                 if( buf_ndx > 0 ) then
                   holder(trj,:n_vals) = trj_pbf(trj)%dchm_vals(:n_vals,buf_ndx)
                 endif
               endif
             end do
             ndx = dchm_buf_ndx(spc)
             var_name = 'dchm_' // trim(chem_dname_table(dm,ndx)) // '_traj'
!            write(var_name,'(a,''_traj'')') trim(chem_dname_table(dm,ndx))
             err_mes = 'trajectory_write_file: failed to get '// trim(var_name) // ' id'
             call handle_ncerr( nf_inq_varid( ncid, trim(var_name), varid ),trim(err_mes) )
             err_mes = 'trajectory_write_file: failed to write ' // trim(var_name)
             call handle_ncerr( nf_put_vara_real( ncid, varid, (/ 1,time_ndx /), (/ n_traj,n_vals /), &
                                                  holder ),trim(err_mes) )
           endif
         end do
     end select
   end do pkg_loop

   n_vals = 0

   ios = nf_close( ncid )

   if( allocated( holder ) ) then
     deallocate( holder )
   endif

   end subroutine trajectory_write_file

   integer function get_spc_buf_ndx( ncnt, list, match_name )

   integer, intent(in)          :: ncnt
   character(len=*), intent(in) :: match_name
   character(len=*), intent(in) :: list(:)

   integer :: spc

   get_spc_buf_ndx = -1
   do spc = 1,ncnt
     if( trim(match_name) == trim(list(spc)) ) then
       get_spc_buf_ndx = spc
       exit
     endif
   end do

   end function get_spc_buf_ndx

   integer function get_dchm_buf_ndx( ncnt, list, match_ndx )

   integer, intent(in)          :: ncnt
   integer, intent(in)          :: match_ndx
   integer, intent(in)          :: list(:)

   integer :: spc

   get_dchm_buf_ndx = -1
   do spc = 1,ncnt
     if( match_ndx == list(spc) ) then
       get_dchm_buf_ndx = spc
       exit
     endif
   end do

   end function get_dchm_buf_ndx

   subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
   integer, intent(in) :: ret
   character(len=*), intent(in) :: mes

include 'netcdf.inc'

   if( ret /= nf_noerr ) then
      call wrf_message( trim(mes) )
      call wrf_message( trim(nf_strerror(ret)) )
      call wrf_abort
   endif

   end subroutine handle_ncerr
#endif

   subroutine trajmapproj (grid,config_flags,ts_proj)

   use module_domain
   use module_llxy
   use module_configure, only : grid_config_rec_type, model_config_rec
   use module_dm, only : wrf_dm_min_real

   IMPLICIT NONE


!------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------
   TYPE(domain), INTENT(IN) :: grid
   TYPE(grid_config_rec_type) , INTENT(IN)  :: config_flags
   TYPE(PROJ_INFO), INTENT(out) :: ts_proj

!------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------
   REAL :: ts_rx, ts_ry, ts_xlat, ts_xlong, ts_hgt
   REAL :: known_lat, known_lon

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe

   TYPE (grid_config_rec_type)               :: config_flags_temp

   config_flags_temp = config_flags

   call get_ijk_from_grid ( grid ,                               &
                            ids, ide, jds, jde, kds, kde,        &
                            ims, ime, jms, jme, kms, kme,        &
                            ips, ipe, jps, jpe, kps, kpe )

   call model_to_grid_config_rec ( grid%id , model_config_rec , config_flags_temp )


!------------------------------------------------------------------------
! Set up map transformation structure
!------------------------------------------------------------------------
   call map_init( ts_proj )

   IF (ips <= 1 .AND. 1 <= ipe .AND. jps <= 1 .AND. 1 <= jpe) THEN
      known_lat = grid%xlat(1,1)
      known_lon = grid%xlong(1,1)
   ELSE
      known_lat = 9999.
      known_lon = 9999.
   END IF
   known_lat = wrf_dm_min_real(known_lat)
   known_lon = wrf_dm_min_real(known_lon)


   select case( config_flags%map_proj )
!------------------------------------------------------------------------
! Mercator
!------------------------------------------------------------------------
     case( PROJ_MERC )
       call map_set(PROJ_MERC, ts_proj,               &
                    truelat1 = config_flags%truelat1, &
                    lat1     = known_lat,             &
                    lon1     = known_lon,             &
                    knowni   = 1.,                    &
                    knownj   = 1.,                    &
                    dx       = config_flags%dx)
!------------------------------------------------------------------------
! Lambert conformal
!------------------------------------------------------------------------
     case( PROJ_LC )
       call map_set(PROJ_LC, ts_proj,                  &
                    truelat1 = config_flags%truelat1,  &
                    truelat2 = config_flags%truelat2,  &
                    stdlon   = config_flags%stand_lon, &
                    lat1     = known_lat,              &
                    lon1     = known_lon,              &
                    knowni   = 1.,                     &
                    knownj   = 1.,                     &
                    dx       = config_flags%dx)
!------------------------------------------------------------------------
! Polar stereographic
!------------------------------------------------------------------------
     case( PROJ_PS )
       call map_set(PROJ_PS, ts_proj,                  &
                    truelat1 = config_flags%truelat1,  &
                    stdlon   = config_flags%stand_lon, &
                    lat1     = known_lat,              &
                    lon1     = known_lon,              &
                    knowni   = 1.,                     &
                    knownj   = 1.,                     &
                    dx       = config_flags%dx)
!------------------------------------------------------------------------
! Cassini (global ARW)
!------------------------------------------------------------------------
     case( PROJ_CASSINI )
       call map_set(PROJ_CASSINI, ts_proj,                            &
                    latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                    loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), &
                    lat1     = known_lat,                             &
                    lon1     = known_lon,                             &
! We still need to get POLE_LAT and POLE_LON metadata variables before
!   this will work for rotated poles.
                    lat0     = 90.0,                                  &
                    lon0     = 0.0,                                   &
                    knowni   = 1.,                                    &
                    knownj   = 1.,                                    &
                    stdlon   = config_flags%stand_lon)
!------------------------------------------------------------------------
! Rotated latitude-longitude
!------------------------------------------------------------------------
     case( PROJ_ROTLL )
       call map_set(PROJ_ROTLL, ts_proj,                      &
                    ixdim    = grid%e_we-1,                   &
                    jydim    = grid%e_sn-1,                   &
                    phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                    lambda   = real(grid%e_we-2)*grid%dx,     &
                    lat1     = config_flags%cen_lat,          &
                    lon1     = config_flags%cen_lon,          &
                    latinc   = grid%dy,                       &
                    loninc   = grid%dx,                       &
                    stagger  = HH)
   end select

   end subroutine trajmapproj

   subroutine UPCASE( lstring )
!----------------------------------------------------------------------
!       ... Convert character string lstring to upper case
!----------------------------------------------------------------------
   implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
   character(len=*), intent(inout) ::  lstring

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
   integer :: i

   do i = 1,LEN_TRIM( lstring )
     if( ICHAR(lstring(i:i)) >= 97 .and.  ICHAR(lstring(i:i)) <= 122 ) then
       lstring(i:i) = CHAR(ICHAR(lstring(i:i)) - 32)
     end if
   end do

   end subroutine UPCASE

#endif

   end module module_trajectory
