#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
  module molec_diff

  !------------------------------------------------------------------------------------------------- !
  ! Module to compute molecular diffusivity for various constituents                                 !
  !                                                                                                  !    
  ! Public interfaces :                                                                              !
  !                                                                                                  !
  !    init_molec_diff           Initializes time independent coefficients                           !
  !    init_timestep_molec_diff  Time-step initialization for molecular diffusivity                  ! 
  !    compute_molec_diff        Computes constituent-independent terms for moleculuar diffusivity   !
  !    vd_lu_qdecomp             Computes constituent-dependent terms for moleculuar diffusivity and !
  !                              updates terms in the triadiagonal matrix used for the implicit      !
  !                              solution of the diffusion equation                                  !
  !                                                                                                  !
  !---------------------------Code history---------------------------------------------------------- !
  ! Modularized     :  J. McCaa, September 2004                                                      !
  ! Lastly Arranged :  S. Park,  January.  2010                                                      !
  !------------------------------------------------------------------------------------------------- !
#ifndef WRF_PORT 
  use perf_mod
  use cam_logfile,  only : iulog
#else
  use module_cam_support,   only: iulog, t_stopf, t_startf
#endif
  implicit none
  private       
  save

  public init_molec_diff 
#ifndef WRF_PORT    
  public init_timestep_molec_diff
#endif
  public compute_molec_diff 
  public vd_lu_qdecomp

  ! ---------- !
  ! Parameters ! 
  ! ---------- !

  integer,  parameter   :: r8 = selected_real_kind(12) ! 8 byte real
  
  real(r8), parameter   :: km_fac = 3.55E-7_r8         ! Molecular viscosity constant [ unit ? ]
  real(r8), parameter   :: pr_num = 1._r8              ! Prandtl number [ no unit ]
  real(r8), parameter   :: pwr    = 2._r8/3._r8        ! Exponentiation factor [ unit ? ]
  real(r8), parameter   :: d0     = 1.52E20_r8         ! Diffusion factor [ m-1 s-1 ] molec sqrt(kg/kmol/K) [ unit ? ]
                                                       ! Aerononmy, Part B, Banks and Kockarts (1973), p39
                                                       ! Note text cites 1.52E18 cm-1 ...

  real(r8)              :: rair                        ! Gas constant for dry air
  real(r8)              :: mw_dry                      ! Molecular weight of dry air
  real(r8)              :: n_avog                      ! Avogadro's number [ molec/kmol ]
  real(r8)              :: gravit     
  real(r8)              :: cpair
  real(r8)              :: kbtz                        ! Boltzman constant

  integer               :: ntop_molec                  ! Top    interface level to which molecular vertical diffusion is applied ( = 1 )
  integer               :: nbot_molec                  ! Bottom interface level to which molecular vertical diffusion is applied ( = pver )
  real(r8), allocatable :: mw_fac(:)                   ! sqrt(1/M_q + 1/M_d) in constituent diffusivity [  unit ? ]
  
  contains

  !============================================================================ !
  !                                                                             !
  !============================================================================ !

  subroutine init_molec_diff( kind, ncnst, rair_in, ntop_molec_in, nbot_molec_in, &
                              mw_dry_in, n_avog_in, gravit_in, cpair_in, kbtz_in )
    
    use constituents,     only : cnst_mw
    use upper_bc,         only : ubc_init
    
    integer,  intent(in)  :: kind           ! Kind of reals being passed in
    integer,  intent(in)  :: ncnst          ! Number of constituents
    integer,  intent(in)  :: ntop_molec_in  ! Top interface level to which molecular vertical diffusion is applied ( = 1 )
    integer,  intent(in)  :: nbot_molec_in  ! Bottom interface level to which molecular vertical diffusion is applied.
    real(r8), intent(in)  :: rair_in
    real(r8), intent(in)  :: mw_dry_in      ! Molecular weight of dry air
    real(r8), intent(in)  :: n_avog_in      ! Avogadro's number [ molec/kmol ]
    real(r8), intent(in)  :: gravit_in
    real(r8), intent(in)  :: cpair_in
    real(r8), intent(in)  :: kbtz_in        ! Boltzman constant
    integer               :: m              ! Constituent index
    
    if( kind .ne. r8 ) then
        write(iulog,*) 'KIND of reals passed to init_molec_diff -- exiting.'
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
        stop 'init_molec_diff'
    endif
    
    rair       = rair_in
    mw_dry     = mw_dry_in
    n_avog     = n_avog_in
    gravit     = gravit_in
    cpair      = cpair_in
    kbtz       = kbtz_in
    ntop_molec = ntop_molec_in
    nbot_molec = nbot_molec_in
    
  ! Initialize upper boundary condition variables

    call ubc_init()

  ! Molecular weight factor in constitutent diffusivity
  ! ***** FAKE THIS FOR NOW USING MOLECULAR WEIGHT OF DRY AIR FOR ALL TRACERS ****
 
    allocate(mw_fac(ncnst))
    do m = 1, ncnst
       mw_fac(m) = d0 * mw_dry * sqrt(1._r8/mw_dry + 1._r8/cnst_mw(m)) / n_avog
    end do

  end subroutine init_molec_diff

  !============================================================================ !
  !                                                                             !
  !============================================================================ !
#ifndef WRF_PORT 
  subroutine init_timestep_molec_diff(state)
    !--------------------------- !
    ! Timestep dependent setting ! 
    !--------------------------- !
    use upper_bc,     only : ubc_timestep_init
    use physics_types,only: physics_state
    use ppgrid,       only: begchunk, endchunk

    type(physics_state), intent(in) :: state(begchunk:endchunk)                 

    call ubc_timestep_init( state)
    
  end subroutine init_timestep_molec_diff
#endif
  !============================================================================ !
  !                                                                             !
  !============================================================================ !

  integer function compute_molec_diff( lchnk       ,                                                                      &
                                       pcols       , pver       , ncnst      , ncol     , t      , pmid   , pint        , &
                                       zi          , ztodt      , kvh        , kvm      , tint   , rhoi   , tmpi2       , &
                                       kq_scal     , ubc_t      , ubc_mmr    , dse_top  , cc_top , cd_top , cnst_mw_out , &
                                       cnst_fixed_ubc_out , mw_fac_out , ntop_molec_out , nbot_molec_out )
    
    use upper_bc,        only : ubc_get_vals
    use constituents,    only : cnst_mw, cnst_fixed_ubc

    ! --------------------- !
    ! Input-Output Argument !
    ! --------------------- !
    
    integer,  intent(in)    :: pcols
    integer,  intent(in)    :: pver
    integer,  intent(in)    :: ncnst
    integer,  intent(in)    :: ncol                      ! Number of atmospheric columns
    integer,  intent(in)    :: lchnk                     ! Chunk identifier
    real(r8), intent(in)    :: t(pcols,pver)             ! Temperature input
    real(r8), intent(in)    :: pmid(pcols,pver)          ! Midpoint pressures
    real(r8), intent(in)    :: pint(pcols,pver+1)        ! Interface pressures
    real(r8), intent(in)    :: zi(pcols,pver+1)          ! Interface heights
    real(r8), intent(in)    :: ztodt                     ! 2 delta-t
    
    real(r8), intent(inout) :: kvh(pcols,pver+1)         ! Diffusivity for heat
    real(r8), intent(inout) :: kvm(pcols,pver+1)         ! Viscosity ( diffusivity for momentum )
    real(r8), intent(inout) :: tint(pcols,pver+1)        ! Interface temperature
    real(r8), intent(inout) :: rhoi(pcols,pver+1)        ! Density ( rho ) at interfaces
    real(r8), intent(inout) :: tmpi2(pcols,pver+1)       ! dt*(g*rho)**2/dp at interfaces

    real(r8), intent(out)   :: kq_scal(pcols,pver+1)     ! kq_fac*sqrt(T)*m_d/rho for molecular diffusivity
    real(r8), intent(out)   :: ubc_mmr(pcols,ncnst)      ! Upper boundary mixing ratios [ kg/kg ]
    real(r8), intent(out)   :: cnst_mw_out(ncnst)
    logical,  intent(out)   :: cnst_fixed_ubc_out(ncnst)
    real(r8), intent(out)   :: mw_fac_out(ncnst)
    real(r8), intent(out)   :: dse_top(pcols)            ! dse on top boundary
    real(r8), intent(out)   :: cc_top(pcols)             ! Lower diagonal at top interface
    real(r8), intent(out)   :: cd_top(pcols)             ! cc_top * dse ubc value
    integer,  intent(out)   :: ntop_molec_out   
    integer,  intent(out)   :: nbot_molec_out   

    ! --------------- ! 
    ! Local variables !
    ! --------------- !

    integer                 :: m                          ! Constituent index
    integer                 :: i                          ! Column index
    integer                 :: k                          ! Level index
    real(r8)                :: mkvisc                     ! Molecular kinematic viscosity c*tint**(2/3)/rho
    real(r8)                :: ubc_t(pcols)               ! Upper boundary temperature (K)

    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- !

  ! Get upper boundary values

    call ubc_get_vals( lchnk, ncol, ntop_molec, pint, zi, ubc_t, ubc_mmr )

  ! Below are already computed, just need to be copied for output

    cnst_mw_out(:ncnst)        = cnst_mw(:ncnst)
    cnst_fixed_ubc_out(:ncnst) = cnst_fixed_ubc(:ncnst)
    mw_fac_out(:ncnst)         = mw_fac(:ncnst)
    ntop_molec_out             = ntop_molec
    nbot_molec_out             = nbot_molec
    
  ! Density and related factors for moecular diffusion and ubc.
  ! Always have a fixed upper boundary T if molecular diffusion is active. Why ?

    tint (:ncol,ntop_molec) = ubc_t(:ncol)
    rhoi (:ncol,ntop_molec) = pint(:ncol,ntop_molec) / ( rair * tint(:ncol,ntop_molec) )
    tmpi2(:ncol,ntop_molec) = ztodt * ( gravit * rhoi(:ncol,ntop_molec))**2 &
                                    / ( pmid(:ncol,ntop_molec) - pint(:ncol,ntop_molec) )
    
  ! Compute molecular kinematic viscosity, heat diffusivity and factor for constituent diffusivity
  ! This is a key part of the code.

    kq_scal(:ncol,1:ntop_molec-1) = 0._r8
    do k = ntop_molec, nbot_molec
       do i = 1, ncol
          mkvisc       = km_fac * tint(i,k)**pwr / rhoi(i,k)
          kvm(i,k)     = kvm(i,k) + mkvisc
          kvh(i,k)     = kvh(i,k) + mkvisc * pr_num
          kq_scal(i,k) = sqrt(tint(i,k)) / rhoi(i,k)
       end do
    end do
    kq_scal(:ncol,nbot_molec+1:pver+1) = 0._r8
    
  ! Top boundary condition for dry static energy

    dse_top(:ncol) = cpair * tint(:ncol,ntop_molec) + gravit * zi(:ncol,ntop_molec)

  ! Top value of cc for dry static energy

    do i = 1, ncol
       cc_top(i) = ztodt * gravit**2 * rhoi(i,ntop_molec) * km_fac * ubc_t(i)**pwr / &
                   ( ( pint(i,2) - pint(i,1) ) * ( pmid(i,1) - pint(i,1) ) )
    enddo
    cd_top(:ncol) = cc_top(:ncol) * dse_top(:ncol)
    
    compute_molec_diff = 1
    return
  end function compute_molec_diff

  !============================================================================ !
  !                                                                             !
  !============================================================================ !

  integer function vd_lu_qdecomp( pcols , pver   , ncol       , fixed_ubc  , mw     , ubc_mmr , &
                                  kv    , kq_scal, mw_facm    , tmpi       , rpdel  ,           &
                                  ca    , cc     , dnom       , ze         , rhoi   ,           &
                                  tint  , ztodt  , ntop_molec , nbot_molec , cd_top )

    !------------------------------------------------------------------------------ !
    ! Add the molecular diffusivity to the turbulent diffusivity for a consitutent. !
    ! Update the superdiagonal (ca(k)), diagonal (cb(k)) and subdiagonal (cc(k))    !
    ! coefficients of the tridiagonal diffusion matrix, also ze and denominator.    !
    !------------------------------------------------------------------------------ !

    ! ---------------------- !
    ! Input-Output Arguments !
    ! ---------------------- !

    integer,  intent(in)    :: pcols
    integer,  intent(in)    :: pver
    integer,  intent(in)    :: ncol                  ! Number of atmospheric columns

    integer,  intent(in)    :: ntop_molec
    integer,  intent(in)    :: nbot_molec

    logical,  intent(in)    :: fixed_ubc             ! Fixed upper boundary condition flag
    real(r8), intent(in)    :: kv(pcols,pver+1)      ! Eddy diffusivity
    real(r8), intent(in)    :: kq_scal(pcols,pver+1) ! Molecular diffusivity ( kq_fac*sqrt(T)*m_d/rho )
    real(r8), intent(in)    :: mw                    ! Molecular weight for this constituent
    real(r8), intent(in)    :: ubc_mmr(pcols)        ! Upper boundary mixing ratios [ kg/kg ]
    real(r8), intent(in)    :: mw_facm               ! sqrt(1/M_q + 1/M_d) for this constituent
    real(r8), intent(in)    :: tmpi(pcols,pver+1)    ! dt*(g/R)**2/dp*pi(k+1)/(.5*(tm(k+1)+tm(k))**2
    real(r8), intent(in)    :: rpdel(pcols,pver)     ! 1./pdel ( thickness bet interfaces )
    real(r8), intent(in)    :: rhoi(pcols,pver+1)    ! Density at interfaces [ kg/m3 ]
    real(r8), intent(in)    :: tint(pcols,pver+1)    ! Interface temperature [ K ]
    real(r8), intent(in)    :: ztodt                 ! 2 delta-t [ s ]

    real(r8), intent(inout) :: ca(pcols,pver)        ! -Upper diagonal
    real(r8), intent(inout) :: cc(pcols,pver)        ! -Lower diagonal
    real(r8), intent(inout) :: dnom(pcols,pver)      ! 1./(1. + ca(k) + cc(k) - cc(k)*ze(k-1)) , 1./(b(k) - c(k)*e(k-1))
    real(r8), intent(inout) :: ze(pcols,pver)        ! Term in tri-diag. matrix system

    real(r8), intent(out)   :: cd_top(pcols)         ! Term for updating top level with ubc

    ! --------------- !
    ! Local Variables !
    ! --------------- !

    integer                 :: i                     ! Longitude index
    integer                 :: k, kp1                ! Vertical indicies

    real(r8)                :: rghd(pcols,pver+1)    ! (1/H_i - 1/H) * (rho*g)^(-1)
    real(r8)                :: kmq(ncol)             ! Molecular diffusivity for constituent
    real(r8)                :: wrk0(ncol)            ! Work variable
    real(r8)                :: wrk1(ncol)            ! Work variable

    real(r8)                :: cb(pcols,pver)        ! - Diagonal
    real(r8)                :: kvq(pcols,pver+1)     ! Output vertical diffusion coefficient

    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- !   

    ! --------------------------------------------------------------------- !
    ! Determine superdiagonal (ca(k)) and subdiagonal (cc(k)) coeffs of the !
    ! tridiagonal diffusion matrix. The diagonal elements  (cb=1+ca+cc) are !
    ! a combination of ca and cc; they are not required by the solver.      !
    !---------------------------------------------------------------------- !

    call t_startf('vd_lu_qdecomp')

    kvq(:,:)  = 0._r8
    cd_top(:) = 0._r8

  ! Compute difference between scale heights of constituent and dry air

    do k = ntop_molec, nbot_molec
       do i = 1, ncol
          rghd(i,k) = gravit / ( kbtz * n_avog * tint(i,k) ) * ( mw - mw_dry )
          rghd(i,k) = ztodt * gravit * rhoi(i,k) * rghd(i,k) 
       enddo
    enddo

    !-------------------- !
    ! Molecular diffusion !
    !-------------------- !

    do k = nbot_molec - 1, ntop_molec, -1
       kp1 = k + 1
       kmq(:ncol)  = kq_scal(:ncol,kp1) * mw_facm
       wrk0(:ncol) = ( kv(:ncol,kp1) + kmq(:ncol) ) * tmpi(:ncol,kp1)
       wrk1(:ncol) = kmq(:ncol) * 0.5_r8 * rghd(:ncol,kp1)
     ! Add species separation term
       ca(:ncol,k  )  = ( wrk0(:ncol) - wrk1(:ncol) ) * rpdel(:ncol,k)
       cc(:ncol,kp1)  = ( wrk0(:ncol) + wrk1(:ncol) ) * rpdel(:ncol,kp1)
       kvq(:ncol,kp1) = kmq(:ncol)
    end do

    if( fixed_ubc ) then
        cc(:ncol,ntop_molec) = kq_scal(:ncol,ntop_molec) * mw_facm                 &
                             * ( tmpi(:ncol,ntop_molec) + rghd(:ncol,ntop_molec) ) &
                             * rpdel(:ncol,ntop_molec)
    end if

  ! Calculate diagonal elements

    do k = nbot_molec - 1, ntop_molec + 1, -1
       kp1 = k + 1
       cb(:ncol,k) = 1._r8 + ca(:ncol,k) + cc(:ncol,k)                   &
                   + rpdel(:ncol,k) * ( kvq(:ncol,kp1) * rghd(:ncol,kp1) &
                   - kvq(:ncol,k) * rghd(:ncol,k) )
       kvq(:ncol,kp1) = kv(:ncol,kp1) + kvq(:ncol,kp1)
    end do

    k   = ntop_molec
    kp1 = k + 1
    if( fixed_ubc ) then
        cb(:ncol,k) = 1._r8 + ca(:ncol,k)                                 &
                    + rpdel(:ncol,k) * kvq(:ncol,kp1) * rghd(:ncol,kp1)   &
                    + kq_scal(:ncol,ntop_molec) * mw_facm                 &
                    * ( tmpi(:ncol,ntop_molec) - rghd(:ncol,ntop_molec) ) &
                    * rpdel(:ncol,ntop_molec)
    else
        cb(:ncol,k) = 1._r8 + ca(:ncol,k) &
                    + rpdel(:ncol,k) * kvq(:ncol,kp1) * rghd(:ncol,kp1)
    end if

    k   = nbot_molec
    cb(:ncol,k) = 1._r8 + cc(:ncol,k) + ca(:ncol,k) &
                - rpdel(:ncol,k) * kvq(:ncol,k)*rghd(:ncol,k)
    do k = 1, nbot_molec + 1, -1
       cb(:ncol,k) = 1._r8 + ca(:ncol,k) + cc(:ncol,k)
    end do

  ! Compute term for updating top level mixing ratio for ubc

    if( fixed_ubc ) then
        cd_top(:ncol) = cc(:ncol,ntop_molec) * ubc_mmr(:ncol)
    end if

    !-------------------------------------------------------- !
    ! Calculate e(k).                                         !
    ! This term is required in solution of tridiagonal matrix ! 
    ! defined by implicit diffusion equation.                 !
    !-------------------------------------------------------- !

    do k = nbot_molec, ntop_molec + 1, -1
       dnom(:ncol,k) = 1._r8 / ( cb(:ncol,k) - ca(:ncol,k) * ze(:ncol,k+1) )
       ze(:ncol,k)   = cc(:ncol,k) * dnom(:ncol,k)
    end do
    k = ntop_molec
    dnom(:ncol,k) = 1._r8 / ( cb(:ncol,k) - ca(:ncol,k) * ze(:ncol,k+1) )

    vd_lu_qdecomp = 1
    call t_stopf('vd_lu_qdecomp')
    return

  end function vd_lu_qdecomp

  end module molec_diff
