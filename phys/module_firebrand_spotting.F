!-------------------------------------------------------------------------------
! This module was developed in RAL-NCAR/UCAR, 2019-2020 by:
! *** M. Frediani and T. Juliano ***
! This research was supervised by J. Knievel and B. Kosovic.
!-------------------------------------------------------------------------------

!=============================================================
!=============================================================

MODULE module_firebrand_spotting

    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE module_domain,       ONLY : get_ijk_from_grid, domain               ! grid
    USE module_configure,    ONLY : grid_config_rec_type                    ! config_flags
    USE module_symbols_util, ONLY : WRFU_TimeInterval, WRFU_TimeIntervalGet, WRFU_TimeIntervalSet
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
    USE MPI 
#endif

    IMPLICIT NONE

    PRIVATE
    PUBLIC firebrand_spotting_em_init, firebrand_spotting_em_driver, get_local_ijk

    ! THESE VARIABLES ARE IN MODULE SCOPE ! Careful with reassignments - don't reassign
    ! SAVE attribute is default

    !-------------------------------------------------------------------------------
    ! variables in module scope: private, only available module-wide (host association)
    !-------------------------------------------------------------------------------
    ! They should not be declared again in suboutines (may not compile)
    !  and must not be among routine dummy arguments. Consequently, cannot be IO variables
    !
    ! Runtime variables are not available at module level (e.g., namelist, tile dependent variables).
    ! Include here only what can be set during compilation:
    ! fixed parameters, allocatables, declarions (without initialization)

    !-------------------------------------------------------------------------------
    ! Fixed parameters ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    INTEGER, PARAMETER :: dp = KIND(0.d0)      ! double precision

    INTEGER, PARAMETER :: wrfdbg = 10    
    ! Fuel parameters must match values from module_fr_phys.F: SUBROUTINE set_fuel_params(nfuel_cat0, nfuel_cat)
    INTEGER, PARAMETER :: no_fuel_cat = 14
    INTEGER, PARAMETER :: nf_sb = 204          ! maximum category on
    INTEGER, PARAMETER :: nfuelcats = 53       ! number of fuel categories that are specified

    REAL,    PARAMETER :: grav = 9.80616_dp    ! gravity (m/s2)
    REAL,    PARAMETER :: rdry = 287.04_dp     ! dry air (J/Kg-K)
    REAL,    PARAMETER :: p2jm = 100.0_dp      ! mb to j/m3
    REAL,    PARAMETER :: rcd  = 0.45_dp       ! drag constant
    REAL,    PARAMETER :: rd   = 287.15_dp    ! gas constant dry air (J / kg K)
    REAL,    PARAMETER :: rv   = 461.6_dp
    REAL,    PARAMETER :: t0   = 300.0_dp
    REAL,    PARAMETER :: cp   = 7.0_dp*rd/2.0_dp
    REAL,    PARAMETER :: rcp  = rd/cp
    REAL,    PARAMETER :: p1000mb= 100000.0_dp
    REAL,    PARAMETER :: r1o3   = 1.0_dp/3.0_dp  ! ratio 1 over 3
    REAL,    PARAMETER :: sboltz = 5.67E-5_dp     ! stefan-boltzmann (g / s3-K4)
    REAL,    PARAMETER :: emiss  = 0.9_dp         ! emissivity
    REAL,    PARAMETER :: cpw    = 1466.0_dp      ! specific heat wood (J / kg-K)
    REAL,    PARAMETER :: cpc    = 712.0_dp       ! specific heat char (J / kg-K)
    REAL,    PARAMETER :: beta0  = 4.8E-7_dp      ! burning rate constant (m2 / s)
    REAL,    PARAMETER :: s_coeff= 110.4_dp       ! Sutherland's law coefficient (K)
    REAL,    PARAMETER :: b_coeff= 1.458E-3_dp    ! Sutherland's law coefficient [g / m-s-K-(1/2)]
    REAL,    PARAMETER :: shmt   = 0.7_dp         ! schmidt number
    REAL,    PARAMETER :: thcon  = 27.0_dp        ! thermal conductivity air

    !USE module_state_description, ONLY: p_qv
    !INTEGER, PARAMETER :: p_qv = 1
    REAL,    PARAMETER :: pr     = 0.7_dp         ! Prandtl

    REAL,    PARAMETER :: NEGLIGIBLE = 10*EPSILON(1.0)
    REAL,    PARAMETER :: ZERO_dp = 0.0_dp ! this is a real type variable, not a double precision type
    REAL,    PARAMETER :: dp05 = 0.5_dp
    REAL,    PARAMETER :: dp1 = 1.0_dp


    !-------------------------------------------------------------------------------
    ! ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    ! Mass threshold to consider burnout (g)
    REAL, PARAMETER :: br_min_mass = EPSILON(dp1) ! max precision for real(4)

    ! Diameter threshold to consider burnout (mm)
    REAL, PARAMETER :: br_min_diam = 0.0000001_dp

    !-------------------------------------------------------------------------------
    ! Generic variables for multiple use within module ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    CHARACTER (LEN=200), SAVE     :: msg
    CHARACTER (LEN=256), SAVE     :: fmt
    CHARACTER (LEN=200), DIMENSION(10) :: amsg
    INTEGER, SAVE :: imsg ! loop counters

    !-------------------------------------------------------------------------------
    ! variables from namelist ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    ! These will be available to all subroutines and will be set in init routine
    INTEGER, SAVE :: fs_array_maxsize   ! maximum number of particles carried in simulation
    INTEGER, SAVE :: fs_gen_levels      ! one per grid pt (may change if releasing at multiple levels)
    INTEGER, SAVE :: fs_gen_lim 
    INTEGER, SAVE :: fs_gen_dt

    REAL,    SAVE :: firebrand_dens      ! 513000.0_dp     ! density of firebrand (g / m3)
    REAL,    SAVE :: firebrand_dens_char ! 299000.0_dp     ! density of char (g m-3) [gupta et al 2002; fuel]


    !-------------------------------------------------------------------------------
    ! Fixed indices, ranks ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    LOGICAL              :: this_is_ideal = .FALSE. 

    TYPE p_properties  ! fb_prop
        REAL :: p_mass ! fbmass
        REAL :: p_diam ! fbdiam
        REAL :: p_effd ! fbediam
        REAL :: p_temp ! fbtemp
        REAL :: p_tvel ! fbvelo 
    END TYPE p_properties

    !-------------------------------------------------------------------------------
    ! grid and cf are not here because dimensions are given at runtime (derived types)
    ! Also, grid values change with timestep, so they need to be passed as dummy arguments
    !-------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------
    ! Variable bounds - Initialized in init, used in dummy arguments in driver 
    ! ***MODULE SCOPE***
    !-------------------------------------------------------------------------------
    INTEGER, SAVE :: ids, jds, ide, jde, kde      ! domain bounds
    INTEGER, SAVE :: ims, jms, ime, jme, kms, kme ! memory bounds
    INTEGER, SAVE :: is, ie, js, je, ks, ke       ! patch start/end 
    INTEGER, SAVE :: ifps, jfps, ifpe, jfpe       ! refined fire grid bounds

CONTAINS

!******************************************************************
!******************************************************************
!*                                                                *
!*                       Module Routines                          *
!*                                                                *
!******************************************************************
!******************************************************************



!=============================================================
    SUBROUTINE firebrand_spotting_em_init ( & 
!=============================================================
        grid,      &
        cf,        &
        fs_p_id,     &
        fs_p_src,    &
        fs_p_dt,     &
        fs_p_x,      &
        fs_p_y,      &
        fs_p_z,      &
        fs_last_gen_dt,&
        fs_gen_idmax,  &
        fs_count_reset,&
        fs_p_mass, &  
        fs_p_diam, & 
        fs_p_effd,& 
        fs_p_temp, & 
        fs_p_tvel, &
        fs_count_landed_all,&
        fs_count_landed_hist,&
        fs_landing_mask,&
        fs_spotting_lkhd,&
        fs_frac_landed)

!=============================================================
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
        USE module_firebrand_spotting_mpi,            ONLY: fs_mpi_init
#endif
        !-------------------------------------------------------------------------------
        ! Initialize all I/O and variables declared in the registry
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        !-------------------------------------------------------------------------------
        ! Arguments
        !-------------------------------------------------------------------------------

        TYPE(domain),               INTENT(IN) :: grid ! 
        TYPE(grid_config_rec_type), INTENT(IN) :: cf   !  run-time configuration (namelist) for domain

        INTEGER,  INTENT(INOUT) :: fs_last_gen_dt, fs_gen_idmax
        LOGICAL,  INTENT(INOUT) :: fs_count_reset

        ! Firebrand Spotting Particle Properties (fs_p_*)
        INTEGER,  INTENT(INOUT), DIMENSION(:)    :: fs_p_id, fs_p_dt, fs_p_src
        REAL,     INTENT(INOUT), DIMENSION(:)    :: fs_p_x, fs_p_y, fs_p_z ! positions are relative to grid edges: particle at grid center point (x,y,z) = (1.5, 1.5, 1.5)
        REAL,     INTENT(INOUT), DIMENSION(:)    :: fs_p_mass, fs_p_diam, fs_p_effd
        REAL,     INTENT(INOUT), DIMENSION(:)    :: fs_p_temp, fs_p_tvel
        REAL,     INTENT(INOUT), DIMENSION(ims:,jms:)  :: fs_count_landed_all, fs_count_landed_hist, fs_spotting_lkhd, fs_frac_landed
        INTEGER,  INTENT(INOUT), DIMENSION(ims:,jms:) :: fs_landing_mask

        !-------------------------------------------------------------------------------

        !-------------------------------------------------------------------------------
        ! Module variables from namelist/default

        fs_array_maxsize = cf%fs_array_maxsize
        fs_gen_levels    = cf%fs_firebrand_gen_levels
        fs_gen_lim       = cf%fs_firebrand_gen_lim
        fs_gen_dt        = cf%fs_firebrand_gen_dt

        fmt = '(A,1x,I6)' 
        WRITE (amsg(1),*)   'SPFire_init: fspotting_em_init'
        WRITE (amsg(2),fmt) 'SPFire_init:  firebrand limit =', fs_gen_lim
        WRITE (amsg(3),fmt) 'SPFire_init:               dt =', fs_gen_dt
        WRITE (amsg(4),fmt) 'SPFire_init: fs_array_maxsize =', fs_array_maxsize
        WRITE (amsg(5),fmt) 'SPFire_init:    fs_gen_levels =', fs_gen_levels
        DO imsg=1,5
            CALL wrf_debug (wrfdbg, TRIM(amsg(imsg)) )
        ENDDO

        !-------------------------------------------------------------------------------
        ! Set bounds to be used as dummy arguments in driver 
        ! ***variables declared in MODULE SCOPE***
        !-------------------------------------------------------------------------------

        CALL get_local_ijk(grid, & 
                           ifps=ifps, jfps=jfps, ifpe=ifpe, jfpe=jfpe, &
                           ids=ids, jds=jds, ide=ide, jde=jde, kde=kde, &
                           ims=ims, jms=jms, ime=ime, jme=jme, kms=kms, kme=kme, &
                           ips=is,  jps=js,  ipe=ie,  jpe=je,  kps=ks,  kpe=ke)        

        WRITE (msg,'(6(i6,1x))') is, ie, js, je, ks, ke
        CALL wrf_debug (wrfdbg, 'SPFire_init tile bounds: '//msg)

        WRITE (msg,'(6(i6,1x))') ims, ime, jms, jme, kms, kme
        CALL wrf_debug (wrfdbg, 'SPFire_init memory bounds: '//msg)

        WRITE (msg,'(4(i6,1x))') ifps, ifpe, jfps, jfpe
        CALL wrf_debug (wrfdbg, 'SPFire_init fire refined bounds: '//msg)


        !-------------------------------------------------------------------------------
        ! Initialize registry arrays
        !-------------------------------------------------------------------------------
        fs_count_reset=.FALSE. 

        fs_last_gen_dt = 0
        fs_gen_idmax   = 0
        fs_p_id    = 0
        fs_p_dt    = 0
        fs_p_src   = 0
        fs_p_x     = 0.0_dp
        fs_p_y     = 0.0_dp
        fs_p_z     = 0.0_dp
        fs_p_mass  = 0.0_dp
        fs_p_diam  = 0.0_dp
        fs_p_effd  = 0.0_dp
        fs_p_temp  = 0.0_dp
        fs_p_tvel  = 0.0_dp

        fs_landing_mask(:,:) = 0
        fs_count_landed_all (:,:) = 0.0_dp
        fs_count_landed_hist(:,:) = 0.0_dp
        fs_spotting_lkhd(:,:) = 0.0_dp
        fs_frac_landed(:,:)   = 0.0_dp

        !fs_fire_ROSdt(:,:) = 0.0_dp
        !fs_gen_inst(:,:) = 0

        !-------------------------------------------------------------------------------

        IF ( grid%this_is_an_ideal_run ) THEN

            this_is_ideal = .TRUE.
            CALL wrf_debug (wrfdbg, 'SPFire_init: Ideal Run detected' )

        ELSE

            this_is_ideal = .FALSE.
            CALL wrf_debug (wrfdbg, 'SPFire_init: Not an Ideal Run' )

        ENDIF


#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
        CALL fs_mpi_init(grid)
#endif

    END SUBROUTINE firebrand_spotting_em_init
!=============================================================
!=============================================================


!=============================================================
    PURE &
    FUNCTION order_val(arr, ord) 
!=============================================================

        IMPLICIT NONE

        REAL,    INTENT(IN), DIMENSION(:) :: arr
        INTEGER, INTENT(IN) :: ord
        REAL    :: order_val
        INTEGER :: diff, besti, iord, i, error

        !-----------------------------------------------------------
        ! Return the value in the array at the given order (allowing an error of +-2 positions)
        ! E.g.: 
        ! For an arr with dimension = 100
        ! ord = 50 will return one of the values near the 50% percentile
        ! i.e., the value in one of the positions between 48:52 if the array was ranked
        !
        ! The algorithm scans the array and 
        ! counts the number of elements with value below the scanned position 
        ! It returns when it finds a value where 
        ! (order - error) <= count <= (order + error)
        !-----------------------------------------------------------

        error = 2

        diff = SIZE(arr) -1
        besti = 1

        DO i=1,SIZE(arr)

            iord = COUNT(arr > arr(i))
            IF (ABS(ord - iord) <= diff) besti = i
            diff = MIN(diff, ABS(ord - iord))
            IF (diff <= error) EXIT
        ENDDO

        ! WRITE(msg, '(2(i6,1x))') iord, diff
        ! CALL wrf_debug (wrfdbg, 'SPFire_rel_order_val: '//msg)

        order_val = arr(besti)

END FUNCTION order_val
!=============================================================
!=============================================================



!=============================================================
    PURE &
    FUNCTION fire2tile(fr_arr, dsx,dsy) RESULT(new_arr)
!=============================================================
    
        IMPLICIT NONE

        REAL,    INTENT(IN), DIMENSION(ifps:ifpe,jfps:jfpe) :: fr_arr
        INTEGER, INTENT(IN)                 :: dsx, dsy
        REAL,    ALLOCATABLE,DIMENSION(:,:) :: new_arr
        INTEGER :: i, j
        INTEGER, DIMENSION(2)   :: fshp

        !-----------------------------------------------------------
        ! Converts a 2-D array from the fire refined grid to tile 
        ! Tile values are the sum of corresponding refined gridpoints
        !-----------------------------------------------------------

        fshp = SHAPE(fr_arr)

        ALLOCATE(new_arr(1:fshp(1)/dsx, 1:fshp(2)/dsy))
        new_arr(:,:) = ZERO_dp

        new_arr(1:fshp(1)/dsx, 1:fshp(2)/dsy) = &
                          RESHAPE([((&
                          SUM(fr_arr(i:i+dsx-1,j:j+dsy-1)), &
                                     i=ifps,ifpe-1,dsx),    &
                                     j=jfps,jfpe-1,dsy)],   &
                          [fshp(1)/dsx,fshp(2)/dsy])
        
    END FUNCTION fire2tile
!=============================================================
!=============================================================



!=============================================================
    ELEMENTAL FUNCTION fuel_spotting_risk(fuel_fgi, factor, fuel_mcg)!, fuel_mcg)
!=============================================================

        ! Returns the refined grid array with the fuel_spotting_risk for spotfires
        !-------------------------------------------------------------------------------

        ! fuel_name(k)       : fuel model name
        ! windrf(k)          : wind reduction factor from 20ft to midflame height
        ! fgi(k)             : initial total mass of surface fuel (kg/m**2)
        ! fgi_lh(k)          : initial total mass of surface live fuel [sb: 1-h] (kg/m**2)
        ! fgi_all(k)         : initial total mass of surface live fuel [sb: 1-h,10-h,100-h] (kg/m**2)
        ! fueldepthm(k)      : fuel depth (m)
        ! savr(k)            : fuel particle surface-area-to-volume ratio, 1/ft
        ! fuelmce(k)         : moisture content of extinction
        ! fueldens(k)        : ovendry particle density, lb/ft^3
        ! st(k)              : fuel particle total mineral content
        ! se(k)              : fuel particle effective mineral content
        ! weight(k)          : weighting parameter that determines the slope of the mass loss curve
        ! fci_d(k)           : initial dry mass of canopy fuel
        ! fct(k)             : burn out time for canopy fuel, after dry (s)
        ! ichap(k)           : 1 if chaparral, 0 if not
        ! fci(k)             : initial total mass of canopy fuel
        ! fcbr(k)            : fuel canopy burn rate (kg/m**2/s)
        ! hfgl               : surface fire heat flux threshold to ignite canopy (w/m^2)
        ! cmbcnst            : joules per kg of dry fuel
        ! fuelheat           : fuel particle low heat content, btu/lb
        ! fuelmc_g           : fuel particle (surface) moisture content
        ! fuelmc_c           : fuel particle (canopy) moisture content

        IMPLICIT NONE

        REAL     :: fuel_spotting_risk
        REAL, INTENT(IN) :: factor
        REAL, INTENT(IN) :: fuel_fgi, fuel_mcg ! fmcg2df ground dead fuel moisture content (fire grid)
 
        fuel_spotting_risk = factor * (dp1 - MIN(dp1, fuel_mcg/fuel_fgi)) ! *(fuel_fgi-fuel_mcg)

        END FUNCTION fuel_spotting_risk
!=============================================================
!=============================================================



!=============================================================
    ELEMENTAL FUNCTION spotting_threat_factor(fcat)
!=============================================================
        IMPLICIT NONE

        INTEGER, INTENT(IN)    :: fcat
        REAL,    DIMENSION(nfuelcats+1) :: factor
        REAL                            :: spotting_threat_factor
        
        ! ****************               read this from external input              ****************
        ! **************** call it in firebrand_spotting_driver once, when grid%itimestep =1  ****************
        ! ****************              and load a registry array                   ****************

        ! User defined coefficient to increase spotfire likelihood; 
        ! threat = 0 yields zero likelihood

        factor(1)     =  1.0_dp ! 1:  'Short grass (1 ft)',
        factor(2:13)  =  1.0_dp  
        factor(14)    =  ZERO_dp ! 14: 'no fuel',
        factor(15:nfuelcats+1) =  1.0_dp  

        spotting_threat_factor = factor(fcat)
        
        !CALL read_table_static

    END FUNCTION spotting_threat_factor

!=============================================================
!=============================================================


! !=============================================================
!     SUBROUTINE read_table_static ! Not debuggeed - from module_mp_thompson
! !=============================================================

!         USE module_domain
!         USE module_dm
!         IMPLICIT NONE

!         LOGICAL, EXTERNAL:: wrf_dm_on_monitor
!         INTEGER:: iunit_fs1, i
!         LOGICAL:: opened
!         CHARACTER (LEN=64) :: filename

!         iunit_fs1 = -1
!         IF ( wrf_dm_on_monitor() ) THEN
!             DO i = 20,99                
!                 INQUIRE ( i , OPENED = opened )
!                 IF ( .NOT. opened ) THEN
!                     iunit_fs1 = i
!                     EXIT
!                 ENDIF
!             ENDDO
!         ENDIF

! #if defined(DM_PARALLEL) && !defined(STUBMPI)
!         CALL wrf_dm_bcast_bytes ( iunit_fs1 , IWORDSIZE )
! #endif
!       IF ( iunit_fs1 < 0 ) &         
!           CALL wrf_error_fatal('SPFire_read_table_static: Cannot find a fortran unit to read file')

!       IF ( wrf_dm_on_monitor() ) THEN

!           WRITE(msg, '(i2,1x,a20)') iunit_fs1, filename
!           CALL wrf_debug(wrfdbg,'SPFire_read_table_static: opening table on unit ' // msg)

!           OPEN(iunit_fs1,FILE=tablename, FORM='UNFORMATTED',STATUS='OLD',ERR=9009)

!       ENDIF

! #define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes(A, size(A)*R4SIZE)

!       IF ( wrf_dm_on_monitor() ) READ(iunit_fs1,ERR=9010) tableval
! #if defined(DM_PARALLEL) && !defined(STUBMPI)
!       DM_BCAST_MACRO(tableval)
! #endif

!       RETURN

!  9009 CONTINUE
!       CALL wrf_error_fatal('SPFire_read_table_static: error openinig static table')
!       RETURN

!  9010 CONTINUE
!       CALL wrf_error_fatal('SPFire_read_table_static: error reading static table')


!       END SUBROUTINE read_table_static

! !=============================================================
! !=============================================================


!=============================================================
    ELEMENTAL FUNCTION get_fuel_cat(crosswalk, cat)
!=============================================================

        ! Use with Scott & Burgen fuel model (WRF CO-FSP)

        IMPLICIT NONE

        INTEGER :: get_fuel_cat
        LOGICAL, INTENT(IN):: crosswalk
        REAL,    INTENT(IN):: cat

        INTEGER, DIMENSION(1:nf_sb) :: ksb ! Anderson82 + S&B2005 fuel categories array
        INTEGER :: i, icat
        !-------------------------------------------------------------------------------

        icat = INT(cat)

        ksb = no_fuel_cat ! 14 ! Initialize all values to no-fuel

        ! Anderson 1982
        ksb(1:13) = [(i, i=1, 13)]

        IF ( crosswalk ) THEN ! Scott & Burgan crosswalks to Anderson

            ksb([101, 104, 107])=1        ! Short grass -- 1
            ksb(102)=2                    ! Timber grass and understory -- 2
            ksb(121:124) = 2              ! Timber grass and understory -- 2
            ksb([103,105,106,108,109])=3  ! Tall grass -- 3
            ksb([145, 147])=4             ! Chaparral -- 4
            ksb(142)=5                    ! Brush -- 5
            ksb([141, 146])=6             ! Dormant Brushi -- 6
            ksb([143, 144, 148, 149])=7   ! Southern Rough -- 7
            ksb([181, 183, 184, 187])=8   ! Compact Timber Litter -- 8
            ksb([182, 186, 188, 189])=9   ! Hardwood Litter -- 9
            ksb(161: 165)=10              ! Timber (understory) -- 10
            ksb([185, 201])=11            ! Light Logging Slash -- 11
            ksb(202)=12                   ! Medium Logging Slash -- 12
            ksb([203, 204])=13            ! Heavy Logging Slash -- 13

        ELSE ! full Scott and Burgan (2005)

            ksb(101:109) = [(i, i=15,23)] ! Grass (GR)
            ksb(121:124) = [(i, i=24,27)] ! Grass-Shrub (GS)
            ksb(141:149) = [(i, i=28,36)] ! Shrub (SH)
            ksb(161:165) = [(i, i=37,41)] ! Timber-Understory (TU)
            ksb(181:189) = [(i, i=42,50)] ! Timber litter (TL)
            ksb(201:204) = [(i, i=51,54)] ! Slash-Blowdown (SB)

        ENDIF

        get_fuel_cat = ksb(icat)

    END FUNCTION get_fuel_cat
!=============================================================
!=============================================================


!=============================================================
    ELEMENTAL FUNCTION firebrand_gen_factor(fcat)
!=============================================================
        IMPLICIT NONE

        REAL                :: firebrand_gen_factor
        INTEGER, INTENT(IN) :: fcat
        REAL,    DIMENSION(nfuelcats+1) :: factor

        
        ! ****************               read this from external input              ****************
        ! **************** call it in firebrand_spotting_driver once, when grid%itimestep =1  ****************
        ! ****************              and load a registry array                   ****************

        ! User defined coefficient to set release priority
        ! factor = 0 yields zero releases from gridpoint 

        factor(1:nfuelcats+1)=   1.0_dp 
        firebrand_gen_factor = factor(fcat)
        
    END FUNCTION firebrand_gen_factor

!=============================================================
!=============================================================



! !=============================================================
!     ELEMENTAL FUNCTION firebrand_gen_maxhgt(fcat)
! !=============================================================
!         IMPLICIT NONE

!         REAL                :: firebrand_gen_maxhgt
!         INTEGER, INTENT(IN) :: fcat
!         REAL,    DIMENSION(nfuelcats+1) :: maxhgt

        
!         ! ****************               read this from external input              ****************
!         ! **************** call it in firebrand_spotting_driver once, when grid%itimestep =1  ****************
!         ! ****************              and load a registry array                   ****************

!         ! User defined max height to release firebrand 

!         maxhgt(1:nfuelcats+1)=  50.0_dp 
!         firebrand_gen_maxhgt = maxhgt(fcat)
        
!     END FUNCTION firebrand_gen_maxhgt

!=============================================================
!=============================================================



!=============================================================
    ELEMENTAL FUNCTION firebrand_gen_potential(fuel_fgi, factor, fire_rate, fuel_mcg)!, fuel_mcg)
!=============================================================

        ! Returns the refined grid array with the potential for firebrand generation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        REAL     :: firebrand_gen_potential
        REAL, INTENT(IN) :: factor, fire_rate
        REAL, INTENT(IN) :: fuel_fgi, fuel_mcg ! fmcg2df ground dead fuel moisture content (fire grid)
 
        firebrand_gen_potential = factor * fire_rate * fuel_fgi !*(dp1 - fuel_mcg/(dp1+fuel_mcg)) ! fuelloadm; dry load from fr_fire_phys

        END FUNCTION firebrand_gen_potential
!=============================================================
!=============================================================



!=============================================================
!    PURE & ! can't call wrf_fatal in pure routines
    SUBROUTINE generate_firebrands( fs_p_id, fs_p_src, fs_p_dt, fs_p_z, fs_p_x, fs_p_y,   & 
                               release_i, release_j, release_k, &
                               release_dt, release_src, &
                               active_br, fs_gen_idmax, myprocid, &
                               release_prop, fs_p_prop)
!=============================================================

        IMPLICIT NONE

        TYPE(p_properties), INTENT(IN), DIMENSION(:):: release_prop
        TYPE(p_properties), INTENT(OUT),DIMENSION(:):: fs_p_prop

        INTEGER, INTENT(INOUT), DIMENSION(:)   :: fs_p_id, fs_p_src, fs_p_dt
        REAL,    INTENT(INOUT), DIMENSION(:)   :: fs_p_z, fs_p_x, fs_p_y
        REAL,    INTENT(INOUT), DIMENSION(:)   :: release_i, release_j, release_k
        INTEGER, OPTIONAL, INTENT(IN), DIMENSION(:) :: release_dt, release_src
        INTEGER, INTENT(INOUT)  :: fs_gen_idmax
        INTEGER, INTENT(IN)     :: myprocid
        INTEGER, INTENT(OUT)    :: active_br

        LOGICAL :: release_true
        INTEGER :: br, ii
        REAL    :: rx, ry, rz
        INTEGER :: new_release
        LOGICAL, DIMENSION(fs_array_maxsize) :: flag_true 

        !------------------------------------------------------------------------------- 
        release_true = .FALSE.  ! scalar: true when brands are ready to be released
        flag_true = .FALSE.     ! logical array: flag to find active elements in array 

        active_br = 0           ! number of active brands
        new_release = 0         ! number of brands to release 

        ! when all brand IDs of array elements are zero: nothing active
        active_br   = COUNT( fs_p_id > 0 ) 
        new_release = COUNT( (INT(release_i) > 0) .AND. (INT(release_j) > 0) ) 

        IF (new_release > 0 ) release_true = .TRUE. ! to-do: include dependency on release_dt  

        IF ( .NOT. release_true) THEN             
            RETURN !CALL wrf_debug (wrfdbg,  'SPFire_generate_firebrands: Found nothing to release')
        ENDIF

        !-------------------------------------------------------------------------------
        ! New brands fit in array? Release particles: set lat/lon/lev of new brands 
        !-------------------------------------------------------------------------------
        
        IF (active_br + new_release <= fs_array_maxsize) THEN

            !-------------------------------------------------------------------------------
            ! Add new grid positions to array: create a subroutine for this: brand_release
            !-------------------------------------------------------------------------------

            ! Flag is True where positions are available to be filled
            WHERE(fs_p_id == 0) flag_true = .TRUE. ! id_indx holds brand ID (unique integer)

            ii = active_br + 1 
            DO br=1, new_release

                ! Find an ii position available to store a new release
                IF ( .NOT. flag_true(ii) ) THEN ! WRITE (msg,'(3(i6,1x))') ii, active_br, fs_p_id(ii) ! CALL wrf_debug (wrfdbg, 'SPFire_release flag false >>> '// msg)
                    CALL wrf_error_fatal('SPFire_generate_firebrands: Did not find free index to release brands! Did you pack fs_p_x, fs_p_y, fs_p_id, etc?') 
                ENDIF

                ! Sanity check
                IF (INT(release_i(br)) == 0 .OR. &
                    INT(release_j(br)) == 0 ) &
                    CALL wrf_error_fatal('SPFire_generate_firebrands: release_ijk is zero! Positions cannot be zero.')

                IF (fs_gen_idmax + 10 >= HUGE(1)) fs_gen_idmax = 0

                ! Assign values and convert height to z index
                fs_p_x(ii) = release_i(br)
                fs_p_y(ii) = release_j(br)
                fs_p_z(ii) = release_k(br)
                fs_p_id(ii) = fs_gen_idmax + 1              ! Assign new IDmax to particle

                IF (.NOT. PRESENT(release_dt)) fs_p_dt(ii) = 0 ! firebrand has not been advected yet so dt is zero
                IF (.NOT. PRESENT(release_src)) fs_p_src(ii) = myprocid + fs_p_id(ii)

                IF (PRESENT(release_dt)) fs_p_dt(ii) = release_dt(br)
                IF (PRESENT(release_src)) fs_p_src(ii) = release_src(br)

                fs_p_prop(ii)%p_mass  = release_prop(br)%p_mass
                fs_p_prop(ii)%p_diam  = release_prop(br)%p_diam
                fs_p_prop(ii)%p_effd = release_prop(br)%p_effd
                fs_p_prop(ii)%p_temp  = release_prop(br)%p_temp 
                fs_p_prop(ii)%p_tvel  = release_prop(br)%p_tvel 

                fs_gen_idmax = fs_p_id(ii)

                flag_true(ii) = .FALSE.
                ii = ii + 1

            ENDDO

            active_br = active_br + new_release
            release_i = ZERO_dp
            release_j = ZERO_dp
            release_k = ZERO_dp

!             WRITE (msg,'(2(i6,1x))') new_release, fs_gen_idmax
!             CALL wrf_debug (wrfdbg, 'SPFire_release:  Released new brands fs_gen_idmax: '//msg)

!         ELSE

!             WRITE (msg,*) active_br + new_release, fs_array_maxsize
!             CALL wrf_debug (wrfdbg, 'SPFire_release: brand array is full, cannot release new brands'// msg)

        ENDIF


    END SUBROUTINE generate_firebrands

!=============================================================
!=============================================================




!=============================================================
    PURE &
    FUNCTION hgt2k(xp, yp, hgt, z_at_w, znw)
!=============================================================
    
    !-------------------------------------------------------------------------------
    ! Converts height in meters to vertical level
    !-------------------------------------------------------------------------------

        IMPLICIT NONE
        !INTEGER, INTENT(IN) :: ims, jms
        REAL,    INTENT(IN) :: hgt, xp, yp
        REAL,    INTENT(IN), DIMENSION(ims:,:,jms:) :: z_at_w
        REAL,    INTENT(IN), DIMENSION(:)         :: znw

        REAL    :: zdz, z_lower_at_p, z_upper_at_p, x0,y0
        REAL    :: hgt2k 
        INTEGER :: k, i, j

        ! An (xp,yp)=(1.5, 1.5) places the particle at the gridpoint center 
        ! but z_at_w is horizontally unstaggered, 
        ! so z_at_w(i,j) is at the horizontal grid center when (i,j)=(1,1) 
        !
        ! Hence, we adjust the particle position to fetch the horizontally adjacent heights for the linear interp. 
        ! We subtract the deltax between 
        ! position at grid center and position at grid edge from the particle position xp, yp, such that 
        ! z_at_w(i,1,j) for (i,j)=(1,1) corresponds to the height at (xp,yp)=(1.5,1.5):
        ! (i = xp - 0.5, j = yp - 0.5) = (i=1, j=1), for (xp=1.5, yp=1.5)

        x0 = xp - dp05
        y0 = yp - dp05

        i = FLOOR(x0) ! shift indices by 0.5 because z_at_w is given at horizontal grid center
        j = FLOOR(y0)

        ! Minloc where hgt - z_k is positive: level right below brand release 
        k = MINLOC(hgt - z_at_w(i,:,j), dim=1, &
                   mask=( hgt - z_at_w(i,:,j) >= 0.0_dp )) 

        z_lower_at_p = u_2d_interp(xp=x0, yp=y0, u_i0j0=z_at_w(i,  k,j), u_i0j1=z_at_w(i,  k,j+1),& 
                                                 u_i1j0=z_at_w(i+1,k,j), u_i1j1=z_at_w(i+1,k,j+1))

       z_upper_at_p = u_2d_interp(xp=x0, yp=y0, u_i0j0=z_at_w(i,  k+1,j), u_i0j1=z_at_w(i,  k+1,j+1),& 
                                                 u_i1j0=z_at_w(i+1,k+1,j), u_i1j1=z_at_w(i+1,k+1,j+1))

        zdz = (hgt - z_lower_at_p) / (z_upper_at_p-z_lower_at_p)
        hgt2k    = k + zdz
 
    END FUNCTION hgt2k
!=============================================================
!=============================================================




!============================================================= 
    PURE &
    SUBROUTINE releaseijk2atm (nij_2d, sr_x, sr_y, fcat, maxhgt_usr, pi, pj, pk, nij)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN),  DIMENSION(:,:) :: nij_2d ! do i need ifps here? No
        INTEGER, INTENT(IN),  DIMENSION(:,:) :: fcat   ! do i need ifps here? No
        INTEGER, INTENT(IN)   :: sr_x, sr_y
        REAL,    INTENT(IN)   :: maxhgt_usr

        REAL,    INTENT(INOUT), DIMENSION(:)   :: pj, pi, pk
        INTEGER, INTENT(INOUT), DIMENSION(:)   :: nij

        INTEGER, ALLOCATABLE, DIMENSION(:)   :: fpi,fpj
        INTEGER :: i, j, cnt

        !-------------------------------------------------------------------------------
        ! Returns a 1-D array with the release positions (i, j, k[m]) 
        !   and number of particles to release (nij)
        !   converted from fire refine grid to wrf grid         
        !   (i,j k are type float and represent the original refined grid position)
        !-------------------------------------------------------------------------------

        cnt = COUNT(nij_2d > 0)

        pi(:) = ZERO_dp
        pj(:) = ZERO_dp
        pk(:) = ZERO_dp
        nij(:) = 0

        ALLOCATE(fpi(cnt), fpj(cnt))
        fpi(:) = 0
        fpj(:) = 0


        ! fpi = RESHAPE([( [(i, i=ifps, ifpe)], j=jfps, jfpe)], SHAPE(fr_arr))
        ! fpj = RESHAPE([( [(j, i=ifps, ifpe)], j=jfps, jfpe)], SHAPE(fr_arr))

        fpi = PACK(RESHAPE([( [(i, i=ifps, ifpe)], j=jfps, jfpe)], SHAPE(nij_2d)), mask=(nij_2d > 0))
        fpj = PACK(RESHAPE([( [(j, i=ifps, ifpe)], j=jfps, jfpe)], SHAPE(nij_2d)), mask=(nij_2d > 0))
        nij = PACK(nij_2d, mask=(nij_2d > 0))

        !-------------------------------------------------------------------------------
        ! convert from refined fire grid to atm grid
        !-------------------------------------------------------------------------------            
        pi = fire2atm_ij(fpi, sr_x )
        pj = fire2atm_ij(fpj, sr_y )
        
        ! Release height and firebrand properties can depend on fuel category (in future developments)
        ! Because fcat is on the refined grid, hgt and fprop must be specified here
        
        ! pk = firebrand_gen_maxhgt(PACK(fcat, mask=(nij_2d > 0))) ! Use this to specify hgts for fuel types 
        pk = maxhgt_usr
        

    END SUBROUTINE  releaseijk2atm
!=============================================================
!=============================================================



!=============================================================
    ELEMENTAL FUNCTION fire2atm_ij (fp_ij, sr)
!=============================================================
! Convert refined subgrid to default atm grid
! Use function separately for i and j:
!     pi = fire2atm_ij(fpi, grid%sr_x )
!     pj = fire2atm_ij(fpj, grid%sr_y )

        IMPLICIT NONE

        REAL  :: fire2atm_ij
        INTEGER, INTENT(IN) :: fp_ij
        INTEGER, INTENT(IN) :: sr

        fire2atm_ij = ( dp1+ REAL(fp_ij - dp1)/REAL(sr) )

! fpij = 298, sr = 3: 297/3+1 = 100
! fpij = 299, sr = 3: 298/3+1 = 100.333
! fpij = 300, sr = 3: 299/3+1 = 100.666
! fpij = 301, sr = 3: 300/3+1 = 101

! fpij = 297, sr = 4: 296/4+1 = 75.00
! fpij = 298, sr = 4: 297/4+1 = 75.25
! fpij = 299, sr = 4: 298/4+1 = 75.50
! fpij = 300, sr = 4: 299/4+1 = 75.75
! fpij = 301, sr = 4: 299/4+1 = 76.00

    END FUNCTION fire2atm_ij
!=============================================================
!=============================================================



!=============================================================    
    ELEMENTAL FUNCTION atm2fire_ij (pij, sr)
!=============================================================
! Convert default atm grid to lower left index of refined grid
! (atm_i, atm_j) = (f_i : f_i + srx , f_j : f_j + sry)

        IMPLICIT NONE

        INTEGER  :: atm2fire_ij
        INTEGER, INTENT(IN) :: pij
        INTEGER, INTENT(IN) :: sr

        atm2fire_ij = (pij - 1) * sr + 1
        
! pij = 100, sr = 3: 99*3+1 = 298
! pij = 101, sr = 3: 100*3+1 = 301

    END FUNCTION atm2fire_ij
!=============================================================
!=============================================================



! !=============================================================
!     SUBROUTINE subgrid_fire_property(fire_ij, fuel_frac, grid)
! !=============================================================

! ! Not used - may be delted from final code version

!         IMPLICIT NONE

!         TYPE(domain), INTENT(IN) :: grid ! input data **Note: Intent IN**
!         REAL,    INTENT(OUT), DIMENSION(:) :: fuel_frac
!         INTEGER, INTENT(IN),  DIMENSION(:,:) :: fire_ij 

!         INTEGER, ALLOCATABLE, DIMENSION(:) :: fpi, fpj
!         INTEGER :: k, cnt, sri, srj, srx, sry

!         srx = grid%sr_x
!         sry = grid%sr_y

!         cnt = SIZE(fire_ij(:,1))
!         fpi = atm2fire_ij( fire_ij(:,1), srx)
!         fpj = atm2fire_ij( fire_ij(:,2), sry)
!         !ALLOCATE( flame(cnt), fuel(cnt))

!         ! Get flame and fuel_frac maxvals from subgrids
!         DO k=1,SIZE(fire_ij(:,1))

!             sri = fpi(k)
!             srj = fpj(k)

!             fuel_frac(k)  = MAXVAL(grid%fuel_frac( sri: sri+srx-1, srj : srj+sry-1))

!             !WRITE (msg,*) MAXVAL(grid%zsf(sri:sri+dsx,srj:srj+dsy)) always zero??

!         ENDDO

!     END SUBROUTINE subgrid_fire_property
! !=============================================================
! !=============================================================



! !=============================================================
!     PURE FUNCTION uniq_ij (fpi, fpj)
! !=============================================================
! ! remove duplicated grid points (after converting from refined fire grid to grid)

! ! Not used but quite handy - do not delete it. 

!         IMPLICIT NONE

!         INTEGER, INTENT(IN), DIMENSION(:) :: fpi, fpj
!         INTEGER, ALLOCATABLE, DIMENSION(:,:) :: uniq_ij
!         LOGICAL, ALLOCATABLE, DIMENSION(:) :: mask
!         INTEGER :: cnt, i

!         cnt = SIZE(fpi)

!         ALLOCATE(mask(cnt))
!         mask = .TRUE.

!         DO i = cnt, 2, -1
!             mask(i) = .NOT.( ANY(fpi(:i-1) == fpi(i) .AND. &
!                                  fpj(:i-1) == fpj(i)))
!         END DO

!         ALLOCATE( uniq_ij(COUNT(mask),2) )
!         uniq_ij(:,1) = REAL(PACK(fpi, mask))
!         uniq_ij(:,2) = REAL(PACK(fpj, mask))

!     END FUNCTION uniq_ij

! !=============================================================
! !=============================================================



!=============================================================
    SUBROUTINE prep_release_hgt(release_i, release_j, release_k, release_n, release_prop, levrand_seed, landhgt)
!=============================================================

        !----------------------------------------------------------
        ! Set release locations for new particles
        ! and distribute particles over fs_gen_levels heights
        !----------------------------------------------------------

        IMPLICIT NONE

        TYPE(p_properties), INTENT(INOUT),   DIMENSION(:) :: release_prop
        REAL,          INTENT(INOUT), DIMENSION(:) :: release_i, release_j
        REAL,          INTENT(INOUT), DIMENSION(:) :: release_k
        INTEGER,       INTENT(IN),    DIMENSION(:) :: release_n
        INTEGER,       INTENT(IN) :: levrand_seed
        REAL,          INTENT(IN) :: landhgt

        INTEGER :: ii, kk, cnt                   ! counters        
        INTEGER :: nseeds
        REAL    :: minhgt
        REAL,    ALLOCATABLE, DIMENSION(:) :: rand_arr, frachgt
        INTEGER, ALLOCATABLE, DIMENSION(:) :: seeds

        minhgt = dp1 + landhgt

        !-------------------------------------------------------------------------------
        ! Calculate number of releases and heights from this fire
        !-------------------------------------------------------------------------------

        ALLOCATE(frachgt(fs_gen_levels))
        ALLOCATE(rand_arr(SIZE(release_n)*fs_gen_levels))

        IF (fs_gen_levels == 1) THEN
            frachgt(:) = dp1
        ELSE
            frachgt(:) = [( REAL(kk-1)*(dp1/REAL(fs_gen_levels-1)), kk=fs_gen_levels, 1, -1)]
        ENDIF

        IF (levrand_seed > 0) THEN

            nseeds = SIZE(rand_arr)
            CALL random_seed(size=nseeds)
            ALLOCATE(seeds(nseeds))
            seeds = [(( release_i(ii) * release_j(ii) * levrand_seed * kk, &
                        kk=1, fs_gen_levels), &
                        ii=1, SIZE(release_n))] ! force seed to vary across space
            CALL random_seed(put = seeds)
            DEALLOCATE(seeds)
            CALL random_number(rand_arr)

        ENDIF


        ii = SIZE(release_n)+1           ! array index for release at various height (append to end of the array)

        DO kk=2, fs_gen_levels ! release at the remaining levels below maxhgt (k=1 is already filled with maxhgt) 
        DO cnt=1,SIZE(release_n)         ! loop over release gridpoints and set a new release at the same ij for 1 < hgt < release_k

                !-------------------------------------------------------------------------------
                ! Fill release array
                !-------------------------------------------------------------------------------

                release_i(ii) = release_i(cnt)    ! ii increments inside this loop
                release_j(ii) = release_j(cnt)
                IF (release_k(cnt) >= minhgt) THEN
                   release_k(ii) = ((release_k(cnt)-minhgt) * frachgt(kk)) + minhgt ! keep a minimum height of 1-m above landhgt 
                ! if we want to perturb the max_release_hgt in releases_k(cnt), we can change its value now
                IF (levrand_seed > 0) & 
                        release_k(ii) = ((release_k(cnt)-minhgt) * rand_arr(ii)) + minhgt
                ELSE
                   release_k(ii) = ((release_k(cnt)-dp1) * frachgt(kk)) + dp1 ! keep a minimum height of 1-m above landhgt 
                   IF (levrand_seed > 0) & 
                        release_k(ii) = ((release_k(cnt)-dp1) * rand_arr(ii)) + dp1
                ENDIF


                release_prop(ii)  = release_prop(cnt)

                ! IF (cnt < 3) THEN
                ! WRITE (msg,'(2(i4,1x),i8,1x,6(f8.3,1x))') cnt, kk, ii, &
                !      release_i(ii), release_j(ii), release_k(cnt), release_k(ii), frachgt(kk), rand_arr(ii)
                ! CALL wrf_debug (0, 'SPFire_prep_release c,k,i, ijk: '//msg)
                ! ENDIF

                ii = ii + 1 ! increment release array index

            ENDDO
        ENDDO

        ! Add another loop here to perturb release_k from ii=,SIZE(release_n) -> currently set to maxhgt

    END SUBROUTINE prep_release_hgt
!=============================================================
!=============================================================



!=============================================================
    FUNCTION firebrand_property(arr) RESULT(prop)
!=============================================================

        IMPLICIT NONE

        TYPE (p_properties) :: prop
        REAL, INTENT(IN),  DIMENSION(:) :: arr
        REAL, PARAMETER :: pi    = 4.0_dp * ATAN (1.0_dp)
        
        prop%p_diam  = arr(1)   ! 10.0_dp    ! mm
        prop%p_effd  = arr(2)   ! 10.0_dp    ! mm
        prop%p_temp  = arr(3)   ! 900.0_dp   ! K
        ! termvel should start with 0m/s because firebrand is released at top of trajectory
        prop%p_tvel  = arr(4)   ! 0.0_dp     ! m/s 
        prop%p_mass  =  (firebrand_dens * pi * (prop%p_effd/1000.0_dp)**2)/6.0_dp  ! g
 
    END FUNCTION firebrand_property

!=============================================================
!=============================================================



!=============================================================
    FUNCTION idx_packed_1d(mask) RESULT(mask_idx)
!=============================================================

        ! Return an array of indices where mask is true
        IMPLICIT NONE
        LOGICAL,  INTENT(IN),  DIMENSION(:) :: mask
        !INTEGER,  INTENT(IN) :: active_br
        INTEGER, ALLOCATABLE, DIMENSION(:) :: mask_idx
        INTEGER :: nresets, ii

        nresets = COUNT(mask)
        ALLOCATE(mask_idx(nresets))
        mask_idx = PACK([(ii, ii=1,SIZE(mask))], mask)   ! Get flag indices 

    END FUNCTION idx_packed_1d
!=============================================================
!=============================================================




!=============================================================
    PURE &
    SUBROUTINE firebrand_physics(dt, hgt, loc_p, loc_t, loc_d, loc_w, fbprop)
!=============================================================

        IMPLICIT NONE

        TYPE(p_properties), INTENT(INOUT) :: fbprop
        REAL, INTENT(IN)    :: loc_p, loc_t, loc_d, loc_w  ! local pressure, temp, density, w
        REAL, INTENT(IN)    :: dt
        REAL, INTENT(INOUT) :: hgt
        !REAL, INTENT(INOUT) :: p_mass, p_diam, p_effd, p_temp, p_tvel

        !-------------------------------------------------------------------------------
        ! firebrand burnout
        !-------------------------------------------------------------------------------

        ! WRITE (msg,'(6(f12.8,1x))') fbprop%p_mass, fbprop%p_diam, &
        !                             fbprop%p_effd, fbprop%p_temp, fbprop%p_tvel, hgt
        ! CALL wrf_debug (wrfdbg, 'SPFire br physics1 m,d,e,t,v,h : '//msg)
        
        CALL burnout(p_mass  = fbprop%p_mass,   & ! firebrand mass (g)                   
                     p_diam  = fbprop%p_diam,   & ! firebrand diameter (mm) 
                     p_effd  = fbprop%p_effd,   & ! firebrand effective diameter (mm) 
                     p_temp  = fbprop%p_temp,   & ! firebrand temperature (K) 
                     p_tvel  = fbprop%p_tvel,   & ! firebrand terminal velocity (m/s)
                     aird    = loc_d,    & ! g/m3   
                     pres    = loc_p,    & ! Pa
                     temp    = loc_t,    & ! K
                     loc_w   = loc_w,    & ! m/s
                     dt      = dt)

!         WRITE (msg,'(4(f12.8,1x))') fbprop%p_mass, fbprop%p_diam, &
!                                     fbprop%p_effd, fbprop%p_temp
!         CALL wrf_debug (wrfdbg, 'SPFire br physics2 m, d, e, t: '//msg)

        !-------------------------------------------------------------------------------
        ! firebrand terminal velocity
        !-------------------------------------------------------------------------------

        CALL termvel(p_diam  = fbprop%p_diam, &
                     p_effd  = fbprop%p_effd,&
                     p_temp  = fbprop%p_temp, &
                     p_tvel  = fbprop%p_tvel, &
                     aird    = loc_d,  & ! g/m3 
                     pres    = loc_p,  & ! Pa
                     temp    = loc_t,  & ! K
                     dt      = dt,     & ! sec
                     hgt     = hgt)      ! m

        ! WRITE (msg,'(6(f12.8,1x))') fbprop%p_mass, fbprop%p_diam, &
        !                             fbprop%p_effd, fbprop%p_temp, fbprop%p_tvel, hgt
        ! CALL wrf_debug (wrfdbg, 'SPFire br physics2 m,d,e,t,v,h : '//msg)

        ! **********
        ! NOTE:
        ! Should we calculate in double precision when diam is in meters?
        ! double: real(8), 15 precision digits
        ! single: real(4), 7  precision digits

    END SUBROUTINE firebrand_physics

!=============================================================
!=============================================================



!=============================================================
     ELEMENTAL SUBROUTINE burnout(pres, aird, temp, loc_w, dt, p_mass, p_diam, p_effd, p_temp, p_tvel)
!=============================================================

!-------------------------------------------------------------------------------
! This subroutine was developed in RAL-NCAR/UCAR, 2019-2020 by:
! *** T. W. Juliano ***
! based on 
! Tse & Fernandez-Pello 1998 (DOI: 10.1016/ S0379-7112(97)00050-7)
! Bhutia, Jenkins & Sun 2010 (DOI:10.3894/JAMES.2010.2.4)
!-------------------------------------------------------------------------------

        IMPLICIT NONE

        REAL,         INTENT(IN)    :: pres, aird, temp, loc_w, dt
        REAL,         INTENT(INOUT) :: p_mass    ! firebrand mass (g)
        REAL,         INTENT(INOUT) :: p_diam    ! firebrand diameter (mm)
        REAL,         INTENT(INOUT) :: p_effd   ! firebrand effective diameter (mm)
        REAL,         INTENT(INOUT) :: p_temp    ! firebrand temperature (K)
        REAL,         INTENT(IN)    :: p_tvel    ! new firebrand terminal velocity (m/s)

        !-------------------------------------------------------------------------------
        ! Constants are at module top
        !-------------------------------------------------------------------------------

        REAL, PARAMETER :: pi    = 4.0_dp * ATAN (1.0_dp)

        REAL :: aird2, wind, gama
        REAL :: pdia, pedia
        REAL :: p_effd2, pdia_new4, reyn, beta, deff, dmvc, dmvc2
        REAL :: parta, partb, dtemp, nuss, hbar, fbvol, qcon, qrad, partc
        REAL :: pratio, cpmix

        p_mass = MAX(p_mass, EPSILON(dp1))
        !-------------------------------------------------------------------------------
        
        ! air density surrounding firebrand
        aird2 = 1000.0_dp * pres / ( rd * p_temp )

        ! Assume vel diff between firebrand and wind is due only to
        ! vertical; that is, assume particle moves with horiz wind
        wind = ABS(loc_w - p_tvel)

        ! particle diameter (mm) to (m)
        pdia = p_diam / 1000.0_dp

        ! particle effective diameter (mm) to (m)
        pedia = p_effd / 1000.0_dp

        ! local atmospheric dynamic viscosity according to Sutherland's law
        dmvc = (b_coeff * temp**1.5_dp) / (temp + s_coeff)

        ! dynamic viscosity surrounding firebrand according to Sutherland's law
        dmvc2 = (b_coeff * p_temp**1.5_dp) / (p_temp + s_coeff)

        ! kinematic viscosity average for atmosphere and firebrand
        gama = ((dmvc / aird) + (dmvc2 / aird2))/2.0_dp

        ! reynolds number
        reyn = wind * pdia / gama

        ! nusselt number
        nuss = 2.0_dp + 0.6_dp * reyn**(dp05) * pr**(r1o3)

        ! convection heat transfer coefficient
        hbar = nuss * thcon / pdia

        ! burning rate
        beta = beta0 + beta0 * (0.276_dp * reyn**(dp05) * shmt**(r1o3))

        ! WRITE (msg,'(9(f12.8,1x),f12.3,1x,f12.8)') aird2, wind, pdia, &
        !                              pedia, dmvc, dmvc2, &
        !                               gama, reyn, nuss, hbar, beta
        ! CALL wrf_debug (wrfdbg, 'SPFire burnout1: '//msg)

        ! change in firebrand mass diameter (m)
        parta = pdia**4 
        partb = SQRT(3.0_dp) * (beta**2) * (dt**2)
        pdia_new4 = parta - partb
        p_diam = pdia_new4**(0.25_dp)

        ! change in firebrand effective mass diameter (m)
        p_effd2 = (pedia**2) - beta * dt
        p_effd = SQRT(p_effd2)

        ! new firebrand mass (g)
        p_mass = (firebrand_dens * pi * p_effd2 * p_effd)/6.0_dp

        ! new firebrand volume (m3)
        fbvol = p_mass / firebrand_dens

        ! new firebrand temp (K)
        qcon = hbar * (p_temp - temp)
        qrad = sboltz * emiss * (p_temp**4 - temp**4)
        pratio = p_effd / p_diam
        cpmix = (pratio * cpw) + ((1.0_dp - pratio) * cpc)
        partc = 6.0_dp / (p_mass * cpmix * p_diam)
        dtemp = dt * fbvol * partc * (qcon + qrad)
        p_temp = p_temp - dtemp

        ! WRITE (msg,'(8(f14.12,1x))') parta, partb, pdia_new4, p_diam, p_effd2, p_effd, p_mass, fbvol
        ! CALL wrf_debug (wrfdbg, 'SPFire burnout2: '//msg)
        ! WRITE (msg,'(2(f12.3,1x),f12.8,1x,2(f12.3,1x),2(f12.8,1x))') qcon, qrad, pratio, cpmix, partc, dtemp, p_temp
        ! CALL wrf_debug (wrfdbg, 'SPFire burnout3: '//msg)

        ! firebrand diameter from (m) to (mm)
        p_diam = 1000.0_dp * p_diam
        p_effd = 1000.0_dp * p_effd

    END SUBROUTINE burnout

!=============================================================
!=============================================================



!=============================================================
    ELEMENTAL  &
    SUBROUTINE termvel(p_diam, p_effd, p_tvel, p_temp, hgt, dt, pres, aird, temp)
!=============================================================

!-------------------------------------------------------------------------------
! This subroutine was developed in RAL-NCAR/UCAR, 2019-2020 by:
! *** T. W. Juliano ***
! based on Bhutia, Jenkins & Sun 2010 (DOI:10.3894/JAMES.2010.2.4)
!-------------------------------------------------------------------------------

        ! Constants are at module top
 
        IMPLICIT NONE

        REAL, INTENT(IN) :: pres, aird, temp, dt
        REAL, INTENT(IN) :: p_diam    ! firebrand diameter (mm)
        REAL, INTENT(IN) :: p_effd    ! firebrand effective diameter (mm)
        REAL, INTENT(IN) :: p_temp    ! firebrand temperature (K)
        REAL, INTENT(INOUT):: hgt     ! particle position at height agl (m)
        REAL, INTENT(INOUT):: p_tvel  ! new firebrand terminal velocity (m/s)

        REAL    :: pbot, ptop, aird2, pdia, parta, partb
        REAL    :: drop, vt, pratio ! fbtype,

        !-------------------------------------------------------------------------------

        ! air density around particle
        aird2 = 1000.0_dp * pres / (rd * p_temp)

        ! particle diameter (mm) to (m)
        pdia = p_diam /1000.0_dp
 
        ! terminal velocity
        pratio = p_effd / p_diam
        parta = ( (pratio*firebrand_dens) + ( (dp1-pratio) * firebrand_dens_char) ) *pdia*grav
        partb = 3.0_dp * ((aird + aird2) / 2.0_dp)*rcd

        pratio = MAX( MIN(HUGE(1.0),(parta / partb)), TINY(1.0)) ! Too large (small) values for the float type size leads to seg fault in debug mode

        ! WRITE (msg,'(4(f12.6,1x))') parta, partb, pratio, SQRT(pratio) 
        ! CALL wrf_debug (wrfdbg, 'SPFire termvel : '//msg)

        vt = SQRT(pratio) 
        
        ! change in vertical position due to settling
        drop = vt * ABS(dt)
        pbot = MAX(0.0_dp, hgt-drop)
        hgt  = pbot

        p_tvel=vt

    END SUBROUTINE termvel

!=============================================================
!=============================================================


!=============================================================
    PURE  &
    SUBROUTINE get_local_met(xp, yp, zp, loc_p, loc_d, loc_t, p, t, d, ihs, jhs, ihe, jhe)
!=============================================================

        !-------------------------------------------------------------------------------
        !
        ! Calculate met from variables updated in HALO after the call to microphysics:
        ! 
        ! HALO_EM_E_5:48: u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,;4:mu_1,mu_2

        

        IMPLICIT NONE

        !TYPE(domain), INTENT(IN) :: grid 
        INTEGER, INTENT(IN)  :: ihs, jhs, ihe, jhe  ! tile +- halo array bounds
        REAL,    INTENT(IN)  :: xp, yp, zp          ! particle position
        REAL,    INTENT(OUT) :: loc_p, loc_t, loc_d ! local met properties
        REAL,    INTENT(IN), DIMENSION(ims:,kms:,jms:):: p,t,d

        INTEGER :: i, j, k , kk
        REAL :: tmp1, tmp2, zph ! corresponding zp at half levels

        k = FLOOR(zp)
        j = MIN(MAX(FLOOR(yp-dp05), jhs), jhe)
        i = MIN(MAX(FLOOR(xp-dp05), ihs), ihe)

        ! physics variables with (i,j) = (1,1) corresponds to (x,y) = (1.5,1.5)
        ! T(i=1): x=1.5
        ! T(i=2): x=2.5
        ! To interpolate T(i=1:2), x=[1.5:2.5[
        ! See comment on function hgt2k

        ! boundaries ims, ..., ke are declared in module scope 

        ! p_hydrostatic is on full levels: no vertical (de)staggering needed
        loc_p = u_3d_interp( x=xp-dp05, y=yp-dp05, z=zp, &
            u_i0j0_bot = p(i,  k, j),   u_i0j1_bot = p(i,  k, j+1),&
            u_i1j0_bot = p(i+1,k, j),   u_i1j1_bot = p(i+1,k, j+1),&
            u_i0j0_top = p(i,  k+1, j), u_i0j1_top = p(i,  k+1,j+1),&
            u_i1j0_top = p(i+1,k+1, j), u_i1j1_top = p(i+1,k+1,j+1))

        ! theta moist is on full levels: no vertical (de)staggering needed: th8w
        loc_t = u_3d_interp( x=xp-dp05, y=yp-dp05, z=zp, &
            u_i0j0_bot = t(i,  k, j),   u_i0j1_bot = t(i,  k, j+1),&
            u_i1j0_bot = t(i+1,k, j),   u_i1j1_bot = t(i+1,k, j+1),&
            u_i0j0_top = t(i,  k+1, j), u_i0j1_top = t(i,  k+1,j+1),&
            u_i1j0_top = t(i+1,k+1, j), u_i1j1_top = t(i+1,k+1,j+1))

        ! Variables on half levels: provide the k index corresponding to particle bottom & top levels 
        ! Is a linear interpolation ok? 

        ! when zp = 1.0, particle is at the surface on the stag grid, 
        !                and at k=0.5 on half levels (below the first level)
        ! when zp = 1.5, particle is in the middle of the stag grid, and at k=1.0 on half level
        !                This is the location where the unstaggered variable value is valid,
        !                so the distance between the particle and this point must be 0.

        ! Physics variables are all unstaggered in x and y.

        zph = zp - dp05
        k = MAX(1, FLOOR(zph))

        loc_d = u_3d_interp( x=xp-dp05, y=yp-dp05, z=zph, &
            u_i0j0_bot = d(i,  k, j),   u_i0j1_bot = d(i,  k, j+1),&
            u_i1j0_bot = d(i+1,k, j),   u_i1j1_bot = d(i+1,k, j+1),&
            u_i0j0_top = d(i,  k+1, j), u_i0j1_top = d(i,  k+1,j+1),&
            u_i1j0_top = d(i+1,k+1, j), u_i1j1_top = d(i+1,k+1,j+1))

        loc_d = loc_d * 1000.0_dp  ! Convert from kg/m3 to g/m3 

        
    END SUBROUTINE get_local_met

!=============================================================

!=============================================================



!=============================================================
    SUBROUTINE firebrand_spotting_em_driver( &     
!=============================================================
        cf,                    &
        grid,                  &
        fs_p_id,               &
        fs_p_src,              &
        fs_p_dt,               &
        fs_p_x,                &
        fs_p_y,                &
        fs_p_z,                &
        fs_gen_inst,           &
        fs_p_mass,             &  
        fs_p_diam,             & 
        fs_p_effd,             & 
        fs_p_temp,             & 
        fs_p_tvel,             & 
        fs_last_gen_dt,        &
        fs_gen_idmax,          &
        fs_fire_ROSdt,         &
        fs_fire_area,          &
        fs_count_landed_all,   &
        fs_count_landed_hist,  &
        fs_landing_mask,       &
        fs_spotting_lkhd,      &
        fs_frac_landed,        &
        fs_fuel_spotting_risk, &
        fs_count_reset)


        !-------------------------------------------------------------------------------
        ! Modules
        !-------------------------------------------------------------------------------

        USE module_domain,            ONLY: domain_get_time_since_sim_start, &
                                            domain_clock_get, is_alarm_tstep
        USE module_domain_type,       ONLY: HISTORY_ALARM, restart_alarm, AUXHIST23_ALARM
        USE module_state_description, ONLY: p_qv, num_moist, param_first_scalar
        USE module_utility,           ONLY: WRFU_Alarm !WRFU_Clock, 
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
        USE module_firebrand_spotting_mpi,  ONLY: fs_mpi_init, &
                                            fs_mpi_sendbuff1_int,      &
                                            fs_mpi_sendbuff1_real,     &
                                            fs_mpi_recvbuff1_int,      &
                                            fs_mpi_recvbuff1_real,     &
                                            fs_mpi_nothing2send,       &
                                            fs_mpi_send2neighbors,     &
                                            fs_mpi_sendbuffsize,       &
                                            fs_mpi_sendbuff_int,       &
                                            fs_mpi_sendbuff_real,      &
                                            fs_mpi_recvbuffsize,       &
                                            fs_mpi_recvfrompatch_int,  &
                                            fs_mpi_recvfrompatch_real, &
                                            fs_mpi_sendbuff_real,      &
                                            fs_mpi_checkreceive,       &
                                            fs_mpi_recv
        USE module_firebrand_spotting_mpi,  ONLY: neighbors, my_id, task_id, mpiprocs,  &
                                            left_id, right_id, up_id, down_id, &
                                            upleft_id, upright_id, downleft_id, downright_id
#endif


        IMPLICIT NONE
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
        LOGICAL, EXTERNAL :: wrf_dm_on_monitor
#endif
        !-------------------------------------------------------------------------------
        ! WRF variables
        !-------------------------------------------------------------------------------

        TYPE(domain),               INTENT(IN)    :: grid ! input data **Note: Intent IN**
        TYPE (GRID_config_rec_type),INTENT(IN)    :: cf   ! type available by module host association 

        !-------------------------------------------------------------------------------
        ! Dummy Arguments
        !-------------------------------------------------------------------------------

        LOGICAL, INTENT(INOUT)  :: fs_count_reset
        INTEGER, INTENT(INOUT)  :: fs_last_gen_dt, fs_gen_idmax

        ! Firebrand Spotting Particle Properties (fs_p_*)
        INTEGER, INTENT(INOUT), DIMENSION(:)    :: fs_p_id, fs_p_src, fs_p_dt
        REAL,    INTENT(INOUT), DIMENSION(:)    :: fs_p_x, fs_p_y, fs_p_z ! positions are relative to grid edges: particle at grid center point (x,y,z) = (1.5, 1.5, 1.5)
        REAL,    INTENT(INOUT), DIMENSION(:)    :: fs_p_mass, fs_p_diam, fs_p_effd
        REAL,    INTENT(INOUT), DIMENSION(:)    :: fs_p_temp, fs_p_tvel
        REAL,    INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: fs_count_landed_all, fs_fire_area, fs_fuel_spotting_risk    ! Use memory bounds so positons match indices
        REAL,    INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: fs_count_landed_hist, fs_spotting_lkhd, fs_frac_landed
        INTEGER, INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: fs_landing_mask, fs_gen_inst
        REAL,    INTENT(INOUT), DIMENSION(ifps:ifpe,jfps:jfpe)  :: fs_fire_ROSdt

        !-------------------------------------------------------------------------------
        ! Parameters
        !-------------------------------------------------------------------------------

        !-------------------------------------------------------------------------------
        ! Local Variables - Arrays
        !-------------------------------------------------------------------------------

        ! Subroutine Control
        LOGICAL, DIMENSION(fs_array_maxsize) :: sparse_mask

        TYPE(p_properties), DIMENSION(fs_array_maxsize)   :: fs_p_prop

        ! Physics from WRF 
        REAL, DIMENSION(ims:ime, ks:ke, jms:jme):: w, z, p_hyd, dz8w, z_at_w ! z_at_w: height at interface of mdl level
        REAL, DIMENSION(ims:ime, ks:ke, jms:jme):: u, v, rho, th_phy         ! *_phy: half levels
        REAL, DIMENSION(ims:ime, ks:ke, jms:jme):: qtot, th8w, p_phy, p8w    ! *8w: full levels 
        REAL, DIMENSION(ims:ime, jms:jme)       :: msft, w1
        REAL, DIMENSION(ks:ke)                  :: znw
        REAL, ALLOCATABLE, DIMENSION(:)         :: xout, yout, zout ! firebrand position after advection

        !-------------------------------------------------------------------------------
        ! Likelihood
        !-------------------------------------------------------------------------------

        REAL,    ALLOCATABLE, DIMENSION(:,:) :: fuel_spotting_risk_fr, spotthreat_fr ! only need it for history intervals

        ! only need it in hist interval - do not hold memory
        LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: landing_mask
        INTEGER, ALLOCATABLE, DIMENSION(:)   :: landing_cnt
        INTEGER, ALLOCATABLE, DIMENSION(:)   :: np_landed_cnt, np_nlanded  ! (MPI: only Master allocates these)
        REAL,    ALLOCATABLE, DIMENSION(:)   :: np_landed_risk, np_frac_landed, np_lkhd ! (MPI: only Master allocates these)
        REAL,    ALLOCATABLE, DIMENSION(:)   :: landed_risk, recv_spot_lkhd, recv_frac_landed
        REAL,    ALLOCATABLE, DIMENSION(:)   :: tmp1
        INTEGER :: ndep, ndep_total
        !-------------------------------------------------------------------------------

        REAL, DIMENSION(ims:ime, jms:jme) :: dt_sum_landed ! sum timestep's deposited fb in 2-D memory array
        LOGICAL, SAVE :: hist_flag = .FALSE.  ! time of history output

        !-------------------------------------------------------------------------------
        ! Release
        !-------------------------------------------------------------------------------

        INTEGER, ALLOCATABLE, DIMENSION(:)    :: np_Ngrdpts, loc_ij 
        REAL,    ALLOCATABLE, DIMENSION(:)    :: np_MeanPot 
        REAL,    ALLOCATABLE, DIMENSION(:,:)  :: RelPot, fcat_factor, fr_rate_dummy
        INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: fcat_fr, n_rel_fr2d
        LOGICAL, ALLOCATABLE, DIMENSION(:,:)  :: rel_mask

        INTEGER :: Ngrdpts, Npts_total, relpts, rankpts 
        REAL    :: TotPot, MeanPot, PotThr, rel_ratio, cdf_coeff, LimPot

        ! Firebrand Release and Transport
        TYPE(p_properties), ALLOCATABLE, DIMENSION(:)   :: release_prop
        REAL,          ALLOCATABLE, DIMENSION(:)   :: release_i, release_j, release_k
        INTEGER,       ALLOCATABLE, DIMENSION(:)   :: release_n

        !-------------------------------------------------------------------------------
        ! Local Variables - Scalars
        !-------------------------------------------------------------------------------

        ! Note: 
        ! Variables for tile, memory, and domain bounds are in module scope & are assigned in firebrand_spotting_init

        INTEGER :: kk, pp, ii ! counters        
        INTEGER :: kk1, kk2, k_end
        INTEGER :: active_br, prior_br, nresets, idmax, firepts, nbr_sum
        REAL    :: rdx, rdy, dt, sr_xy ! grid length (1/dx) and wrf's time-step

        INTEGER :: firebrand_max_life_dt
        REAL    :: firebrand_land_hgt, firebrand_gen_maxhgt
        LOGICAL :: firebrand_gen_levrand
        REAL    :: firebrand_gen_prop_diam, firebrand_gen_prop_effd
        REAL    :: firebrand_gen_prop_temp, firebrand_gen_prop_tvel 
        INTEGER :: levrand_seed

        !-------------------------------------------------------------------------------
        ! MPI local Variables (some variables are in module scope)
        !-------------------------------------------------------------------------------

        INTEGER, SAVE :: MasterId = 0
        INTEGER, SAVE :: ntiles = 0
        INTEGER, SAVE :: myprocid = 0
        LOGICAL, SAVE :: IamMaster = .FALSE. 

        INTEGER, ALLOCATABLE, DIMENSION(:) :: nbr_id, r_id, r_dt, r_src
        REAL,    ALLOCATABLE, DIMENSION(:) :: r_x, r_y, r_z

        ! MPI receive p_properties
        REAL, ALLOCATABLE, DIMENSION(:) :: r_p_m,r_p_d,r_p_e,r_p_t,r_p_v
        TYPE(p_properties), ALLOCATABLE, DIMENSION(:) :: fb_mdetv


        !-------------------------------------------------------------------------------
        ! It was a dummy variable back when fuel moisture was not implemented in wrf-v4.0.1
        !-------------------------------------------------------------------------------

        REAL,  DIMENSION(ifps:ifpe,jfps:jfpe)  :: fmcg_2df        

        !===============================================================================
        ! Begin
        !===============================================================================


#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
        IF (mpiprocs == 0) & 
            CALL fs_mpi_init(grid) ! should be called in fspotting_init (?)

        ntiles = mpiprocs
        myprocid = my_id * 1000000
        IF ( wrf_dm_on_monitor() ) THEN 
            IamMaster = .TRUE. 
            MasterId = my_id
        ENDIF
#else
        IamMaster = .TRUE.  ! initialized with FALSE. It should be TRUE in serial compilation
        ntiles = 1 ! used to allocate the arrays for incomming mpi comms. ntiles=1 in serial builds
        myprocid = 1* 1000000
#endif

        hist_flag = .FALSE. 
        IF ( Is_alarm_tstep(grid%domain_clock, grid%alarms(HISTORY_ALARM)) ) hist_flag = .TRUE. 
        !IF ( Is_alarm_tstep(grid%domain_clock, grid%alarms(AUXHIST23_ALARM)) ) hist_flag = .TRUE. 
        IF (hist_flag) &
            CALL wrf_debug (wrfdbg, 'SPFire_driver: History output on next tstep')

        fs_last_gen_dt = fs_last_gen_dt + 1
        active_br = COUNT( fs_p_id > 0 )

        IF (grid%itimestep == 1) &
            fs_fire_ROSdt(ifps:ifpe,jfps:jfpe) =0.0_dp

        !===============================================================================
        ! Reset array after writting history file
        !===============================================================================

        IF (fs_count_reset) THEN ! if true
            !CALL wrf_debug (wrfdbg, 'SPFire_lkhd: resetting fs_count_reset!')
            fs_count_landed_hist(:,:)  = ZERO_dp
            fs_fuel_spotting_risk(:,:) = ZERO_dp
            fs_count_reset      = .FALSE.

            fs_gen_inst(is:ie,js:je) = 0
        ENDIF

        fs_landing_mask(ims:ime,jms:jme) = 0

        !===============================================================================
        ! Assign properties to derived type (used locally to shorten argument list)
        !===============================================================================

        fs_p_prop%p_mass  = ZERO_dp
        fs_p_prop%p_diam  = ZERO_dp
        fs_p_prop%p_effd  = ZERO_dp
        fs_p_prop%p_temp  = ZERO_dp
        fs_p_prop%p_tvel  = ZERO_dp

        fs_p_prop(:active_br)%p_mass  = fs_p_mass(:active_br)
        fs_p_prop(:active_br)%p_diam  = fs_p_diam(:active_br)
        fs_p_prop(:active_br)%p_effd  = fs_p_effd(:active_br)
        fs_p_prop(:active_br)%p_temp  = fs_p_temp(:active_br) 
        fs_p_prop(:active_br)%p_tvel  = fs_p_tvel(:active_br) 

        !===============================================================================
        ! Assign user defined values from namelist
        !===============================================================================

        firebrand_max_life_dt = cf%fs_firebrand_max_life_dt
        firebrand_land_hgt = cf%fs_firebrand_land_hgt
        firebrand_gen_maxhgt  = cf%fs_firebrand_gen_maxhgt
        firebrand_gen_levrand = cf%fs_firebrand_gen_levrand

        firebrand_gen_prop_diam = cf%fs_firebrand_gen_prop_diam
        firebrand_gen_prop_effd = cf%fs_firebrand_gen_prop_effd
        firebrand_gen_prop_temp = cf%fs_firebrand_gen_prop_temp
        firebrand_gen_prop_tvel = cf%fs_firebrand_gen_prop_tvel
        
        firebrand_dens      = cf%fs_firebrand_dens      ! 513000.0_dp     ! density of firebrand (g / m3)
        firebrand_dens_char = cf%fs_firebrand_dens_char ! 299000.0_dp     ! density of char (g m-3) [gupta et al 2002; fuel]

        !===============================================================================
        ! Firebrand Generation
        !===============================================================================

        !-------------------------------------------------------------------------------
        ! Accumulate burn rate between generation intervals
        !-------------------------------------------------------------------------------

        firepts = 0
        ! fs_fire_ROSdt(ifps:ifpe, jfps:jfpe) = fs_fire_ROSdt(ifps:ifpe, jfps:jfpe) + grid%burnt_area_dt(ifps:ifpe,jfps:jfpe)
        fs_fire_ROSdt(ifps:ifpe, jfps:jfpe) = grid%burnt_area_dt(ifps:ifpe,jfps:jfpe)
        ! Accumulation prioritises firebrands from gridpts mostly burnt vs recently ignited

        !-------------------------------------------------------------------------------
        ! Updated fuel moisture from WRF-fire
        !-------------------------------------------------------------------------------

        fmcg_2df(ifps:ifpe,jfps:jfpe) = grid%fmc_g(ifps:ifpe,jfps:jfpe)

        !===============================================================================
        ! Firebrand Generation - Release Firebrands - Part 1
        !===============================================================================

        !-------------------------------------------------------------------------------
        ! When a new release is due, calculate fraction of release by fuel category
        !-------------------------------------------------------------------------------

        IF (fs_last_gen_dt >= fs_gen_dt) THEN  

            firepts = COUNT(fs_fire_ROSdt(ifps:ifpe,jfps:jfpe) > ZERO_dp)
            Ngrdpts = 0 ! Number of active fire points of valid fuel categories

            LimPot = 0.000001_dp ! 1e-6
            MeanPot = ZERO_dp 

            IF (firepts > 0) THEN 

                !WRITE (msg,'(1(i6,1x))') firepts
                !CALL wrf_debug (wrfdbg, 'SPFire_rel firepts: '//msg)

                !-------------------------------------------------------------------------------
                ! Get fuel category from WRF-FIRE and calculate release potential
                !-------------------------------------------------------------------------------

                ALLOCATE(fcat_fr(ifps:ifpe,jfps:jfpe), fcat_factor(ifps:ifpe,jfps:jfpe), RelPot(ifps:ifpe,jfps:jfpe))!, rel_fcat(ifps:ifpe,jfps:jfpe))
                fcat_fr(:,:)  = 0
                RelPot(:,:) = ZERO_dp
                fcat_factor(:,:) = ZERO_dp

                fcat_fr(ifps:ifpe,jfps:jfpe) = RESHAPE(get_fuel_cat(crosswalk=grid%fuel_crosswalk, &
                                                                    cat=grid%nfuel_cat(ifps:ifpe,jfps:jfpe)), &
                                                       SHAPE(fcat_fr))

                fcat_factor(ifps:ifpe,jfps:jfpe)= firebrand_gen_factor(fcat_fr(ifps:ifpe,jfps:jfpe)) !RESHAPE, SHAPE(fcat_fr)) 

                RelPot(ifps:ifpe,jfps:jfpe) = RESHAPE(firebrand_gen_potential(&
                                                             fuel_fgi=grid%fgip(ifps:ifpe,jfps:jfpe), &
                                                             fuel_mcg=fmcg_2df(ifps:ifpe,jfps:jfpe), & 
                                                             !fuel_mcg=grid%fmcg2df(ifps:ifpe,jfps:jfpe), & 
                                                             factor=fcat_factor(ifps:ifpe,jfps:jfpe),&
                                                             fire_rate=fs_fire_ROSdt(ifps:ifpe,jfps:jfpe)), &
                                                         SHAPE(fcat_fr)) 

                ! Calculate Release Potential for each burning fuel cat
                !-------------------------------------------------------------------------------

                Ngrdpts= COUNT(RelPot(ifps:ifpe,jfps:jfpe) > LimPot)! RelPot do not count cat with factor of 0

                ! In Large Fires
                ! Try to increase minimum LimPot to remove very low values of Release Potential
                !-------------------------------------------------------------------------------
                IF (Ngrdpts > fs_gen_lim) THEN
                    LimPot = 10.0_dp * LimPot ! 1e-5
                    Ngrdpts= COUNT(RelPot(ifps:ifpe,jfps:jfpe) > LimPot)

                    IF (Ngrdpts > fs_gen_lim) THEN
                        LimPot = 10.0_dp * LimPot ! 1e-4
                        Ngrdpts= COUNT(RelPot(ifps:ifpe,jfps:jfpe) > LimPot)

                    ENDIF
                ENDIF
                ! WRITE (msg,'(1(i8,1x), 1(f14.8,1x))') Ngrdpts, LimPot
                ! CALL wrf_debug (wrfdbg, 'SPFire_rel_lim Ngrdpts, LimP: '//msg)


                WRITE (msg,'(1(i6,1x))') Ngrdpts
                CALL wrf_debug (wrfdbg, 'SPFire_rel Ngrdpts: '//msg)

                ! Set a threshold for Potentials - use only highest values
                !-------------------------------------------------------------------------------
                IF (Ngrdpts > fs_gen_lim) THEN

                    ! Find threshold value to select fs_gen_lim points
                    !-------------------------------------------------------------------------------

                    ALLOCATE(tmp1(Ngrdpts))
                    tmp1(1:Ngrdpts) = PACK(RelPot(ifps:ifpe,jfps:jfpe), &
                                mask=(RelPot(ifps:ifpe,jfps:jfpe) > LimPot))

                    ! Find a limit value that yields an array size close to fs_gen_lim
                    LimPot = order_val(tmp1, ORD=fs_gen_lim)
                    !maxpot = MAXVAL(tmp1)
                    DEALLOCATE(tmp1)

                    ! WRITE (msg,'(3(i8,1x), 2(f14.8,1x))') Ngrdpts, COUNT(RelPot > LimPot), SIZE(tmp1), LimPot, maxpot
                    ! CALL wrf_debug (wrfdbg, 'SPFire_rel_lim Ngrdpts, Cnt>Lim, LimP, max: '//msg)

                    ! Find the median rank among remaining points
                    !-------------------------------------------------------------------------------

                    ALLOCATE(tmp1(COUNT(RelPot(ifps:ifpe,jfps:jfpe) > LimPot)))
                    tmp1 = PACK(RelPot(ifps:ifpe,jfps:jfpe), &
                                mask=(RelPot(ifps:ifpe,jfps:jfpe) > LimPot))

                    MeanPot = order_val(tmp1, ORD=INT(REAL(SIZE(tmp1))/2.0))
                    !maxpot = MAXVAL(tmp1)
                    DEALLOCATE(tmp1)

                    ! WRITE (msg,'(4(i8,1x), 3(f14.8,1x))') Ngrdpts, &
                    !     COUNT(RelPot > MeanPot), SIZE(tmp1), INT(REAL(SIZE(tmp1))/2.0), &
                    !     MeanPot, MINVAL(tmp1), maxpot
                    ! CALL wrf_debug (wrfdbg, 'SPFire_rel_lim Ngrdpts, Cnt>Mean, MPot, min, max: '//msg)

                    Ngrdpts= COUNT(RelPot(ifps:ifpe,jfps:jfpe) > LimPot)

                !-------------------------------------------------------------------------------
                    
                ELSEIF (Ngrdpts > 0) THEN

                    ! Find the median rank among points
                    !-------------------------------------------------------------------------------
                    MeanPot = order_val(PACK(RelPot(ifps:ifpe,jfps:jfpe), &
                                      mask=(RelPot(ifps:ifpe,jfps:jfpe) > LimPot)), &
                                      ORD=INT(dp05*Ngrdpts))

                    ! WRITE (msg,'(2(i8,1x), 2(f14.8,1x))') Ngrdpts, COUNT(RelPot > MeanPot), MeanPot, LimPot
                    ! CALL wrf_debug (wrfdbg, 'SPFire_rel_lim Ngrdpts, Cnt>Mean, MPot, LimP: '//msg)

                ENDIF

                ! Prepare for MPI
                !-------------------------------------------------------------------------------

                IF (Ngrdpts == 0) firepts = 0

                !-------------------------------------------------------------------------------

            ENDIF ! (firepts > 0)
            !-------------------------------------------------------------------------------

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
            IF (.NOT. (IamMaster)) THEN

                ! nbr > 0 indicates there's fire on tile and RelPot > 0, not the array size to receive in next mpi comm
                ! if nbr > 0, next array will be one (array: MeanPot) 
                CALL fs_mpi_sendbuff1_int(sendto=MasterId, nbr=Ngrdpts) 

                IF (Ngrdpts > 0) THEN
                    CALL fs_mpi_sendbuff1_real(sendto=MasterId, nbr=MeanPot) 
                    ! WRITE (msg,'(1(i6,1x), (f14.6,1x))') Ngrdpts, MeanPot
                    ! CALL wrf_debug (wrfdbg, 'SPFire_rel mpi_send Npts,Meanpot: '//msg)
                ENDIF
            ENDIF
#endif

            !===============================================================================
            ! Firebrand Generation - Release Firebrands - Part 2
            !===============================================================================

            !-------------------------------------------------------------------------------
            ! MPI Master: Calculate Release Field (fire gridpt cnt by fuel category)
            !-------------------------------------------------------------------------------

            IF (IamMaster) THEN                      

                CALL wrf_debug (wrfdbg, 'SPFire_rel_master --------------------------------------------------- ')                    
                !-------------------------------------------------------------------------------
                ! Declare arrays to receive data

                Npts_total = 0
                PotThr = ZERO_dp 
                TotPot = ZERO_dp
                rel_ratio = ZERO_dp
                cdf_coeff = ZERO_dp

                
                ALLOCATE(np_Ngrdpts(ntiles)) ! Number of fuel cat actively burning in each mpiproc
                ALLOCATE(np_MeanPot(ntiles)) 
                np_MeanPot(:) = ZERO_dp
                np_Ngrdpts(:) = 0

                np_Ngrdpts(1) = Ngrdpts ! Array index for Master is 1
                np_MeanPot(1) = MeanPot

                !-------------------------------------------------------------------------------
                ! Receive release potential data from procs

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )

                DO kk=2, mpiprocs ! kk=1 is Master, MPI-IDs begin at 0
                    np_Ngrdpts(kk) = fs_mpi_recvbuff1_int(fromid=kk-1)  ! Comm receive: Number of pts with RelPot>0

                    IF (np_Ngrdpts(kk) > 0) THEN 
                        np_MeanPot(kk) = fs_mpi_recvbuff1_real(fromid=kk-1)
                        ! WRITE(msg, '(3(i8,1x), 2(f14.6,1x))') my_id, kk-1, np_Ngrdpts(kk), np_Meanpot(kk), TotPot
                        ! CALL wrf_debug (wrfdbg, 'SPFire_rel master proc Ngrdpts, MeanPot, TotPot: '//msg)
                    ENDIF
                ENDDO
#endif

                DO kk=1, ntiles ! at least 1 for serial compile 
                   TotPot = np_MeanPot(kk) * REAL(np_Ngrdpts(kk)) + TotPot 
                ENDDO

                !-------------------------------------------------------------------------------
                ! Communicate release sources between Master and procs

                Npts_total = SUM( np_Ngrdpts )
                IF ( Npts_total > 0) THEN 

                    TotPot = TotPot/(REAL(Npts_total))

                    !-------------------------------------------------------------------------------
                    ! Calculate potential threshold for grid pts

                    ! All elements fit in the array
                    IF (Npts_total <= fs_gen_lim) THEN
                        PotThr = ZERO_dp 

                    ! Estimate threshold with a linear regression 
                    ELSE ! IF (Npts_total > fs_gen_lim) 
                        rel_ratio = REAL(fs_gen_lim)/(REAL(Npts_total))
                        cdf_coeff = (dp1 - rel_ratio)/rel_ratio
                        PotThr = cdf_coeff * TotPot
                    ENDIF

                    ! WRITE(msg, '(2(i8,1x), 4(f12.6,1x))') fs_gen_lim, Npts_total, PotThr, rel_ratio, cdf_coeff, TotPot
                    ! CALL wrf_debug (wrfdbg, 'SPFire_rel_master max, Npts, PotThr: '//msg)

                    !-------------------------------------------------------------------------------
                    ! Send dat to procs

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )

                    DO kk=2, mpiprocs ! kk=1 is Master
                        IF (np_Ngrdpts(kk) > 0) THEN
                            CALL fs_mpi_sendbuff1_real(sendto=kk-1, nbr=PotThr)
                            ! WRITE(msg, '(1(i8,1x), 1(f12.6,1x))') kk-1, PotThr
                            ! CALL wrf_debug (wrfdbg, 'SPFire_rel_master mpi_send PotThr: '//msg)
                        ENDIF
                    ENDDO
#endif
                ENDIF ! npts_total > 0
                !-------------------------------------------------------------------------------

            ENDIF ! IamMaster 

            !-------------------------------------------------------------------------------

            !===============================================================================
            ! Firebrand Generation - Release Firebrands - Part 3
            !===============================================================================

            IF (Ngrdpts > 0) THEN

                !-------------------------------------------------------------------------------
                ! Receive Potential Threshold from Master 

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
                IF (.NOT.(IamMaster)) THEN
                    PotThr = ZERO_dp 
                    PotThr = fs_mpi_recvbuff1_real(fromid=MasterId) 
                    ! WRITE(msg, '((i4,1x), (f14.6,1x))')  my_id, PotThr
                    ! CALL wrf_debug (wrfdbg, 'SPFire_rel receive potential threshold: '//msg)
                ENDIF
#endif

                !------------------------------------------------------------------------------- 
                ! Assign release from the various fuel cat to corresponding gridpts

                !CALL wrf_debug (wrfdbg, 'SPFire_rel2fr relpts>0 --------------------------------------------- ') 

                ALLOCATE(n_rel_fr2d(ifps:ifpe, jfps:jfpe))
                n_rel_fr2d(ifps:ifpe, jfps:jfpe) = 0

                IF (PotThr > ZERO_dp) &
                    n_rel_fr2d(ifps:ifpe,jfps:jfpe) = n_rel_fr2d(ifps:ifpe,jfps:jfpe)  + &
                                                      UNPACK(SPREAD(1, DIM=1, NCOPIES=Ngrdpts), &
                                                                    mask=(RelPot(ifps:ifpe,jfps:jfpe) >= PotThr), FIELD=0)

                IF (PotThr == ZERO_dp) &
                    n_rel_fr2d(ifps:ifpe,jfps:jfpe) = n_rel_fr2d(ifps:ifpe,jfps:jfpe)  + &
                                                      UNPACK(SPREAD(1, DIM=1, NCOPIES=Ngrdpts), &
                                                                    mask=(fs_fire_ROSdt(ifps:ifpe,jfps:jfpe) > ZERO_dp), FIELD=0)

                !------------------------------------------------------------------------------- 
                ! update relpts to represent value assigned to gridpts 

                relpts = COUNT(n_rel_fr2d(ifps:ifpe,jfps:jfpe)>0) ! Limits to 1 release per refined gridpt, at various heights 

                WRITE(msg, '(1(i8,1x))') relpts
                CALL wrf_debug (wrfdbg, 'SPFire_rel2fr relpts: '//msg)

                !------------------------------------------------------------------------------- 
                ! Convert release locations to met grid

                ALLOCATE(release_i(relpts*fs_gen_levels), &
                         release_j(relpts*fs_gen_levels), &
                         release_k(relpts*fs_gen_levels), &
                         release_n(relpts))

                release_i(:)  = 0.0_dp
                release_j(:)  = 0.0_dp
                release_k(:)  = 0.0_dp
                release_n(:)  = 0

                ! Does not allow more than one release from the same refined gridpt
                CALL releaseijk2atm(nij_2d=n_rel_fr2d(ifps:ifpe,jfps:jfpe), &
                                    fcat=fcat_fr(ifps:ifpe,jfps:jfpe), &
                                    maxhgt_usr = firebrand_gen_maxhgt, &
                                    sr_x=grid%sr_x, sr_y=grid%sr_y, &
                                    pi=release_i(1:relpts), &
                                    pj=release_j(1:relpts), &
                                    pk=release_k(1:relpts), & ! max height
                                    nij=release_n)


                ! Adjust position to corresponding refined grid center
                release_i(1:relpts) = release_i(1:relpts) + dp05/grid%sr_x
                release_j(1:relpts) = release_j(1:relpts) + dp05/grid%sr_y

                DO kk=1,relpts ! track release points between history outputs

                    fs_gen_inst(INT(release_i(kk)),INT(release_j(kk))) = &
                        fs_gen_inst(INT(release_i(kk)),INT(release_j(kk))) + 1

                ENDDO
            
                CALL wrf_debug (wrfdbg, 'SPFire_rel2fr prep_release_hgt ---------------------------------------------')
                ALLOCATE(release_prop(relpts*fs_gen_levels))

                ! Create derived type
                release_prop(1:relpts) = firebrand_property([firebrand_gen_prop_diam, &
                                                             firebrand_gen_prop_effd, &
                                                             firebrand_gen_prop_temp, &
                                                             firebrand_gen_prop_tvel])
                
                levrand_seed = 0 ! means no random levels
                IF (firebrand_gen_levrand) levrand_seed = cf%fs_firebrand_gen_levrand_seed + 1 ! in case user sets it to 0
                ! include more releases to span over various heights
                CALL prep_release_hgt( &
                                  release_i = release_i, &
                                  release_j = release_j, &
                                  release_k = release_k, & ! max height 
                                  release_n = release_n, &
                                  landhgt = firebrand_land_hgt, &
                                  release_prop=release_prop, &
                                  levrand_seed = levrand_seed * grid%itimestep) ! force seed to vary in time

                prior_br = active_br
                idmax = fs_gen_idmax
            
                CALL wrf_debug (wrfdbg, 'SPFire_driver_call_generate_firebrands ----------------------------------------- ') 

                IF (active_br + COUNT( INT(release_i) > 0 )  > fs_array_maxsize) & 
                    CALL wrf_debug (wrfdbg, 'SPFire_driver_release: brand array is full, cannot release new brands') 
                
                CALL generate_firebrands(&
                    fs_p_id = fs_p_id, &
                    fs_p_src = fs_p_src, &
                    fs_p_dt = fs_p_dt, &
                    fs_p_z  = fs_p_z,  &
                    fs_p_x  = fs_p_x,  &
                    fs_p_y  = fs_p_y,  &
                    fs_p_prop   = fs_p_prop,    &
                    fs_gen_idmax= fs_gen_idmax, &
                    myprocid = myprocid,        &
                    release_prop= release_prop, &
                    release_i = release_i, &
                    release_j = release_j, &
                    release_k = release_k, &
                    active_br = active_br)

                !-------------------------------------------------------------------------------
                ! Track released firebrands
                !-------------------------------------------------------------------------------

                WRITE (msg,'(i8)') active_br
                CALL wrf_debug (wrfdbg, 'SPFire_driver: Active brands: '// msg )

                IF (cf%trackember) THEN
                
                   DO kk=prior_br+1,active_br
                      WRITE (msg,'(3(i8,1x),i10,1x,8(f12.6,1x))') &
                           kk, fs_p_id(kk), fs_p_dt(kk),fs_p_src(kk), &
                           fs_p_x(kk), fs_p_y(kk), fs_p_z(kk), &
                           fs_p_mass(kk), fs_p_diam(kk), fs_p_effd(kk), &
                           fs_p_temp(kk), fs_p_tvel(kk)
                      CALL wrf_debug (0, 'SPFire p,id,dt,src,x,y,z,m,d,e,t,v: '//msg)
                   ENDDO
                ENDIF
                !-------------------------------------------------------------------------------

                IF (active_br /= COUNT( fs_p_id > 0 ) ) CALL wrf_error_fatal('SPFire_driver: Active brands do not match!')

            ENDIF ! Ngrdpts > 0                                    

            fs_fire_ROSdt(:, :) = ZERO_dp
            relpts = 0
            fs_last_gen_dt = 0 ! Reset release timestep counter
            IF( ALLOCATED(fcat_fr))     DEALLOCATE(fcat_fr)
            IF( ALLOCATED(fcat_factor)) DEALLOCATE(fcat_factor)
            IF( ALLOCATED(RelPot))      DEALLOCATE(RelPot)
            IF( ALLOCATED(np_Ngrdpts))  DEALLOCATE(np_Ngrdpts)
            IF( ALLOCATED(np_MeanPot))  DEALLOCATE(np_MeanPot) 
            IF( ALLOCATED(n_rel_fr2d))  DEALLOCATE(n_rel_fr2d)
            IF( ALLOCATED(release_prop)) DEALLOCATE(release_prop)
        
        ENDIF ! IF (fs_last_gen_dt >= fs_gen_dt) THEN  

        !===============================================================================
        ! End of Releases
        !===============================================================================

        !-------------------------------------------------------------------------------
        ! Nothing to transport: MPI Wait
        !-------------------------------------------------------------------------------        

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
        IF ((active_br == 0))  THEN

            CALL fs_mpi_nothing2send(sendto=left_id)
            CALL fs_mpi_nothing2send(sendto=right_id)
            CALL fs_mpi_nothing2send(sendto=up_id)
            CALL fs_mpi_nothing2send(sendto=down_id)

            CALL fs_mpi_nothing2send(sendto=upleft_id)
            CALL fs_mpi_nothing2send(sendto=upright_id)
            CALL fs_mpi_nothing2send(sendto=downleft_id)
            CALL fs_mpi_nothing2send(sendto=downright_id)
            !CALL wrf_debug (wrfdbg, 'SPFire_driver: no fires or active brands in patch. MPI signal sent.')

        ENDIF
#endif
        !===============================================================================
        !===============================================================================



        !===============================================================================
        ! Transport active firebrands
        !===============================================================================

        IF (active_br > 0) THEN 

            !-------------------------------------------------------------------------------
            ! Fetch wrf variables for advection
            !-------------------------------------------------------------------------------
            
            ! Use arrays computed for the physics scheme on half (_phy) and full (8w) levels
            ! will skip calc at model top - particle will not live during journey (module_big_step_utilities_em.F)

            !-------------------------------------------------------------------------------
            ! Local met for fb physics: based on variables calculated in module_big_step_utilities_em.F
            !-------------------------------------------------------------------------------

            !HALO_EM_E_5:48: u_1,u_2,v_1,v_2,w_1,w_2,t_1,t_2,ph_1,ph_2,tke_1,tke_2,;4:mu_1,mu_2 moist
            !-------------------------------------------------------------------------------
            ! P, Th, Rho, hgt at full levels

            ! Direct assignments from existing Halos:

            ! Half Levels
            k_end = MIN( ke, kde-1 )
            ks = 1

            p_phy(ims:, ks:, jms:) = ZERO_dp
            th_phy(ims:, ks:, jms:) = ZERO_dp


            p_phy(ims:, ks:k_end, jms:) = grid%p(ims:ime,ks:k_end,jms:jme) + grid%pb(ims:ime,ks:k_end,jms:jme)
            th_phy(ims:, ks:, jms:) = grid%t_2(ims:ime,ks:k_end,jms:jme) + t0 

            !hypsometric_opt 2 (default) computes pressure in the model by using an alternative method 
            ! Temperature is moist theta by default (v4+, https://github.com/wrf-model/WRF/releases)
            IF ( ( grid%use_theta_m == 1 ) .AND. (P_Qv >= PARAM_FIRST_SCALAR) ) &
                th_phy = th_phy/(dp1 + Rv/Rd * grid%moist(ims:ime,ks:k_end,jms:jme,P_QV))
            
            ! Full Levels
            z_at_w(ims:,ks:,jms:)= (grid%phb(ims:ime,ks:ke,jms:jme) + grid%ph_2(ims:ime,ks:ke,jms:jme))/grav ! ASL

            u(ims:, ks:, jms:) = grid%u_2(ims:ime,ks:ke,jms:jme) 
            v(ims:, ks:, jms:) = grid%v_2(ims:ime,ks:ke,jms:jme)             
            w(ims:, ks:, jms:) = grid%w_2(ims:ime,ks:ke,jms:jme) 

            msft(ims:,   jms:) = grid%msftx(ims:ime,    jms:jme) 

            znw = grid%znw
            DT  = grid%dt
            rdx = grid%rdx ! inverse grid length [m]
            rdy = grid%rdy
            
            !-------------------------------------------------------------------------------
            ! Calculated variables 

            p_hyd(ims:, ks:, jms:) = ZERO_dp
            dz8w(ims:, ks:, jms:) = ZERO_dp
            th8w(ims:, ks:, jms:) = ZERO_dp
            p8w(ims:, ks:, jms:) = ZERO_dp
            qtot(ims:, ks:, jms:) = ZERO_dp
            rho(ims:, ks:, jms:) = ZERO_dp
            z(ims:, ks:, jms:) = ZERO_dp

            dz8w(ims:,ks:ke-1,jms:) = z_at_w(:,2:,:)-z_at_w(:,:ke-1,:) ! z_at_w: Height above the ground at interfaces
            z(ims:,ks:ke-1,jms:) = dp05 * (z_at_w(:,2:,:) + z_at_w(:,:ke-1,:) )

            qtot(ims:, ks:k_end, jms:) = SUM(grid%moist(ims:ime,ks:k_end,jms:jme,PARAM_FIRST_SCALAR:num_moist),DIM=4)
            p_hyd(ims:, ke, jms:) = grid%p_top
            DO kk = ke-1, ks, -1
                p_hyd(:,kk,:) = p_hyd(:,kk+1,:) - &
                    (dp1 + qtot(:,kk,:)) * (grid%c1h(kk) * grid%muts(:,:) + grid%c2h(kk)) * grid%dnw(kk)
            ENDDO

            rho(ims:,ks:k_end,jms:) = (dp1/&
                                        (grid%al(ims:ime,ks:k_end,jms:jme)+grid%alb(ims:ime,ks:k_end,jms:jme)))&
                                        *(dp1+grid%moist(ims:ime,ks:k_end,jms:jme,P_QV))
            DO kk = 2, k_end
                th8w(ims:,kk,jms:) = grid%fnm(kk) * th_phy(:,kk,:) +  th_phy(:,kk-1,:) * grid%fnp(kk)
                p8w(ims:,kk,jms:)  = grid%fnm(kk) *  p_phy(:,kk,:) +   p_phy(:,kk-1,:) * grid%fnp(kk)
            ENDDO

            w1 = (z(:,1,:) - z(:,2,:)) 
            WHERE(NINT(w1*100.0) == 0 ) w1 = dp1
            w1 = (z_at_w(:,1,:) - z(:,2,:)) / w1

            th8w(:,1,:) = w1 * th_phy(:,1,:) + (dp1 - w1) * th_phy(:,2,:) ! interp at bottom only - ignore top
            p8w(:,1,:)  = w1 *  p_phy(:,1,:) + (dp1 - w1) *  p_phy(:,2,:) ! interp at bottom only - ignore top

            DO kk = 1, ke
                z_at_w(:,kk,:)= z_at_w(:,kk,:) - z_at_w(:,1,:)
            ENDDO

            !-------------------------------------------------------------------------------
            !-------------------------------------------------------------------------------

            !-------------------------------------------------------------------------------
            ! Advect particle
            !-------------------------------------------------------------------------------

            CALL wrf_debug (wrfdbg, 'SPFire_driver begin transport ---------------------------------------------------- ')

            ALLOCATE(xout(active_br), &
                     yout(active_br), &
                     zout(active_br)) 

            CALL advect_xyz_m(grid=grid,           &
                            xp=  fs_p_x(:active_br), &
                            yp=  fs_p_y(:active_br), &
                            hgt= fs_p_z(:active_br), & ! height[m] is the fixed position reference between wrf time steps 
                            dtp= fs_p_dt(:active_br),&
                            idp= fs_p_id(:active_br),&
                            znw= znw,      &
                            mf = rdx*msft, &
                            z_at_w=z_at_w, &
                            u= u,     &
                            v= v,     &
                            w= w,     &
                            dt=dt,    &
                            ims= ims,  &
                            jms= jms,  &
                            kms= ks,  &
                            phyd= p_hyd, & ! air pressure (Pa)
                            thet= th8w, &  ! temperature (K)
                            rho = rho,  &  ! density (kg/m3)
                            xout= xout, &
                            yout= yout, &
                            zout= zout, &
                            fs_p_prop=fs_p_prop(:active_br), &
                            land_hgt = firebrand_land_hgt,   &
                            start_mom3d_dt = cf%fs_firebrand_gen_mom3d_dt, &
                            msg = msg)


            IF (LEN(TRIM(msg)) > 1) &
                CALL wrf_debug (wrfdbg, 'SPFire_transp: '//msg)

            !-------------------------------------------------------------------------------
            ! Update array for output before resetting deposited indices to zero
            !-------------------------------------------------------------------------------

            fs_p_x(:active_br) = xout
            fs_p_y(:active_br) = yout
            fs_p_z(:active_br) = zout
            fs_p_dt(:active_br)= fs_p_dt(:active_br) +1

            fs_p_mass(:active_br) = fs_p_prop(:active_br)%p_mass
            fs_p_diam(:active_br) = fs_p_prop(:active_br)%p_diam
            fs_p_effd(:active_br) = fs_p_prop(:active_br)%p_effd
            fs_p_temp(:active_br) = fs_p_prop(:active_br)%p_temp
            fs_p_tvel(:active_br) = fs_p_prop(:active_br)%p_tvel

            CALL wrf_debug (wrfdbg, 'SPFire_driver end transport   ---------------------------------------------------- ')

            !=============================================================
            ! Transport done :)
            !=============================================================

           !-------------------------------------------------------------------------------
           ! Track firebrands
           !-------------------------------------------------------------------------------

            WRITE (msg,'(2(i8,x))') active_br, COUNT( fs_p_id > 0 )
            CALL wrf_debug (wrfdbg, 'SPFire_driver: Active brands, id>0: '// msg )

            IF (cf%trackember) THEN
               DO kk=1,active_br
                  WRITE (msg,'(3(i8,1x),i10,1x,8(f12.6,1x))') &
                       kk, fs_p_id(kk), fs_p_dt(kk),fs_p_src(kk), &
                       fs_p_x(kk), fs_p_y(kk), fs_p_z(kk), &
                       fs_p_mass(kk), fs_p_diam(kk), fs_p_effd(kk), &
                       fs_p_temp(kk), fs_p_tvel(kk)
                  CALL wrf_debug (0, 'SPFire p,id,dt,src,x,y,z,m,d,e,t,v: '//msg)
               ENDDO
            ENDIF
            !-------------------------------------------------------------------------------

            !===============================================================================
            ! Remove particles
            !===============================================================================

            !-------------------------------------------------------------------------------
            ! Set NaN properties for particle removal
            !-------------------------------------------------------------------------------
            sparse_mask = .FALSE. 

            WHERE (IEEE_IS_NAN(fs_p_mass) .OR. IEEE_IS_NAN(fs_p_diam) .OR. &
                   IEEE_IS_NAN(fs_p_effd) .OR. IEEE_IS_NAN(fs_p_temp) .OR. &
                   IEEE_IS_NAN(fs_p_tvel)) fs_p_effd = ZERO_dp ! ediam=zero will be removed in remove_br


            !===============================================================================
            ! Remove particles - from burnout or out of domain
            !===============================================================================
            nresets = 0
            CALL remove_br(& !active_br= active_br, &
                           fs_p_id    = fs_p_id,     &
                           fs_p_x     = fs_p_x,      &
                           fs_p_y     = fs_p_y,      &
                           fs_p_z     = fs_p_z,      &
                           fs_p_dt    = fs_p_dt,     &
                           fs_p_effd  = fs_p_effd,   &
                           cnt        = nresets,     &
                           max_life_dt= firebrand_max_life_dt, &
                           land_hgt   = firebrand_land_hgt)
            WRITE (msg,'(i12)') nresets
            CALL wrf_debug (wrfdbg, 'SPFire_driver remove br: '//msg)


            !===============================================================================            
            ! Spotfires: Deposited Brands
            !===============================================================================

            dt_sum_landed(ims:, jms:) = ZERO_dp
            CALL deposit_br(& !active_br= active_br, &
                           fs_p_id    = fs_p_id,      &
                           fs_p_x     = fs_p_x,       &
                           fs_p_y     = fs_p_y,       &
                           fs_p_z     = fs_p_z,       &
                           sum_fbrand = dt_sum_landed,&
                           land_hgt   = firebrand_land_hgt)

            ! The mask is based on floor(fs_p_x,y) so deposits should not exist at ie or je
            fs_count_landed_all(is:ie, js:je)  = fs_count_landed_all(is:ie, js:je)  + dt_sum_landed(is:ie, js:je)
            fs_count_landed_hist(is:ie, js:je) = fs_count_landed_hist(is:ie, js:je) + dt_sum_landed(is:ie, js:je)
            WRITE (msg,'(2(i12,x))') COUNT(dt_sum_landed(is:ie, js:je) > 0), INT(SUM(dt_sum_landed(is:ie, js:je)))
            CALL wrf_debug (wrfdbg, 'SPFire_driver deposit br: '//msg)


            !=============================================================================== 
            !=============================================================================== 

            !******************************************************************
            !******************************************************************
            !*                                                                *
            !*                  MPI Send firebrands                   *
            !*                                                                *
            !******************************************************************
            !******************************************************************
            
            
            !-------------------------------------------------------------------------------
            !-------------------------------------------------------------------------------

            ! Re-PACK before incoming MPI

            !===============================================================================            
            ! MPI: Send Brands (out of this patch)
            !===============================================================================

            ! CALL wrf_debug (wrfdbg, 'SPFire_driver mpi transfer particles')

            ! Send / Receive arrays: 
            !     3 REAL arrays, 2 INTEGER arrays: fs_p_x, fs_p_y, fs_p_z, fs_p_id, fs_p_dt: 

            !   |____|____|____|
            !   |    | U  |    |
            !   |____|____|____|je
            !   |  L | Me | R  |
            !   |____|____|____|js
            !   |    | D  |    |
            !   |____|____|____|
            !        is   ie

            ! Diagonal???

            !-------------------------------------------------------------------------------
            ! Send any particle?
            !-------------------------------------------------------------------------------

            sparse_mask = .FALSE. 
            sparse_mask = (fs_p_id > 0 .AND. &
                          (FLOOR(fs_p_x) < is .OR. & ! LEFT
                           FLOOR(fs_p_x) > ie .OR. & ! RIGHT
                           FLOOR(fs_p_y) > je .OR. & ! UP
                           FLOOR(fs_p_y) < js ))     ! DONW
                   

            WRITE (msg,'(i6)') COUNT(sparse_mask) 
            CALL wrf_debug (wrfdbg, 'SPFire_driver mpi send away: '//msg)

            !-------------------------------------------------------------------------------
            ! interrupt before MPI
            ! IF (COUNT(sparse_mask) > 0) CALL wrf_error_fatal( "MPI called") 

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )            
            CALL fs_mpi_send2neighbors(task_id=task_id,&
                    mask=sparse_mask,&
                    p_x  = fs_p_x,   &
                    p_y  = fs_p_y,   &
                    p_z  = fs_p_z,   &
                    p_id = fs_p_id,  &
                    p_src = fs_p_src,  &
                    p_dt = fs_p_dt,  &
                    fs_p_m = fs_p_mass, &
                    fs_p_d = fs_p_diam, &
                    fs_p_e = fs_p_effd, &
                    fs_p_t = fs_p_temp, &
                    fs_p_v = fs_p_tvel)
#endif
            !-------------------------------------------------------------------------------
            ! Reset
            !-------------------------------------------------------------------------------                
            ! CALL wrf_debug (wrfdbg, 'SPFire_driver reset particles sent away')

            ! In serial compilation, firebrands outside the domain limits will be removed
            WHERE(sparse_mask) fs_p_id= 0

        !===============================================================================            
        ! Last step for IF active_br > 0
        !===============================================================================            

            ! WRITE (msg,'(2(i8,1x))') active_br, COUNT( fs_p_id > 0 )
            ! CALL wrf_debug (wrfdbg, 'SPFire_driver pack BEFORE: active_br >>> '// msg)

            !-------------------------------------------------------------------------------
            ! Repack all zeros: PACK where mask is TRUE
            !-------------------------------------------------------------------------------
            sparse_mask = .FALSE. 
            sparse_mask = (fs_p_id>0)
            fs_p_x  = PACK(fs_p_x,  sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_y  = PACK(fs_p_y,  sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_z  = PACK(fs_p_z,  sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_id = PACK(fs_p_id, sparse_mask, SPREAD(0,1,fs_array_maxsize) )
            fs_p_src = PACK(fs_p_src, sparse_mask, SPREAD(0,1,fs_array_maxsize) )
            fs_p_dt = PACK(fs_p_dt, sparse_mask, SPREAD(0,1,fs_array_maxsize) )
            fs_p_mass  = PACK(fs_p_mass , sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_diam  = PACK(fs_p_diam , sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_effd = PACK(fs_p_effd, sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_temp  = PACK(fs_p_temp , sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) )
            fs_p_tvel  = PACK(fs_p_tvel , sparse_mask, SPREAD(ZERO_dp,1,fs_array_maxsize) ) 
            
            active_br = COUNT( fs_p_id > 0 )

            WRITE (msg,'(1(i8,1x))') active_br
            CALL wrf_debug (wrfdbg, 'SPFire_driver pack AFTER mpi_send2neighbors active_br >>> '// msg)


        ENDIF

        !===============================================================================            
        ! Active_br work done ;)
        !===============================================================================            
        IF (active_br /= COUNT( fs_p_id > 0 ) ) CALL wrf_error_fatal('SPFire_driver: Active brands do not match!')

        !===============================================================================            
        !===============================================================================            



        !===============================================================================
        ! Calculate Spotfire Likelihood at history output time 
        !===============================================================================
        
        ! Registry
        ! fs_count_landed_all
        ! fs_count_landed_hist
        ! fs_frac_landed
        ! fs_spotting_lkhd            
        ! fs_count_reset 
        ! fs_fire_area
        
        ! store array of data for each proc: Master only 
        ! np_nlanded      [np]              ! ALLOCATABLE
        ! np_landed_cnt   [SUM(np_nlanded)] ! ALLOCATABLE
        ! np_landed_risk  [SUM(np_nlanded)] ! ALLOCATABLE
        ! np_frac_landed  [SUM(np_nlanded)] ! ALLOCATABLE
        ! np_lkhd         [SUM(np_nlanded)] ! ALLOCATABLE
        ! 
        
        ! store array of local data
        ! landing_mask    ! ALLOCATABLE
        ! landing_cnt	  ! ALLOCATABLE (mpi send)
        ! landed_risk	  ! ALLOCATABLE (mpi send)
        ! recv_spot_lkhd  ! ALLOCATABLE (mpi recv)
        ! recv_frac_landed ! ALLOCATABLE (mpi recv)
        
        !===============================================================================
        ! Check history interval (use diag_flag defined in solve_em for microphysics)
        !===============================================================================
        
        IF ((hist_flag) .AND. (grid%itimestep > 1)) THEN

            fs_count_reset = .TRUE. ! true
            fs_frac_landed(ims:ime, jms:jme) = ZERO_dp
            fs_spotting_lkhd(ims:ime, jms:jme) = ZERO_dp

            ! Was there any firebrand deposit? *Note that fire may exist on a different tile
            !-------------------------------------------------------------------------------

            ALLOCATE(landing_mask(ims:ime, jms:jme)) ! Allocate memory but only mask this tile
            landing_mask(ims:ime, jms:jme) = .FALSE. 
            landing_mask(is:ie, js:je) = (fs_count_landed_hist(is:ie, js:je) > 0)
            ndep = COUNT(landing_mask)

            ! convert fire_area from fire grid to atm grid
            !-------------------------------------------------------------------------------
            fs_fire_area(is:ie,js:je)  = ZERO_dp
                fs_fire_area(is:ie, js:je) = fire2tile(fr_arr=grid%fire_area(ifps:ifpe,jfps:jfpe), &
                                                   dsx=grid%sr_x,dsy=grid%sr_y)

            ! WRITE(msg, '(i8,1x,i12)') ndep, COUNT(fs_fire_area(is:ie,js:je)> ZERO_dp)
            ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd: deposit points, fire points: '//msg)

            IF (ndep > 0) THEN ! deposited anywhere

                landing_mask(ims:ime, jms:jme) = .FALSE. 
                landing_mask(is:ie, js:je) = ((fs_count_landed_hist(is:ie, js:je) > 0) .AND. &
                                          (fs_fire_area(is:ie, js:je) == ZERO_dp) )                       
                ndep = COUNT(landing_mask) 

                ! deposited on nonburning gridpt
                fs_landing_mask(is:ie, js:je) = UNPACK(SPREAD(1,DIM=1,NCOPIES=ndep), MASK=landing_mask(is:ie, js:je), FIELD=0)

                ! WRITE(msg, '(4(i8,1x))') COUNT(fs_count_landed_hist > ZERO_dp), ndep, COUNT(fs_landing_mask == 1), COUNT((fs_count_landed_hist > ZERO_dp).AND.(fs_fire_area > ZERO_dp))
                ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd deposits, non fire-point deposits, fire-point deposits: '//msg)

                ALLOCATE(tmp1(ndep))
                tmp1 = PACK(fs_count_landed_hist(is:ie, js:je), landing_mask(is:ie, js:je)) ! WHERE doest work on memory bounds
                fs_count_landed_hist(is:ie, js:je) = ZERO_dp
                fs_count_landed_hist(is:ie, js:je) = UNPACK(tmp1, MASK=landing_mask(is:ie, js:je), FIELD=ZERO_dp)

                ! WRITE(msg, '(2(i8,1x), 1(f14.6,1x))') COUNT(fs_count_landed_hist > ZERO_dp), ndep, MAXVAL(fs_count_landed_hist)
                ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd deposits, non fire-point deposits, fire-point deposits: '//msg)
                DEALLOCATE(tmp1)

                ! Were these firebrand deposited outside the fire area?
                !-------------------------------------------------------------------------------

                IF (ndep > 0) THEN 

                    ! Calculate fuel risk 
                    !-------------------------------------------------------------------------------

                    ALLOCATE(fuel_spotting_risk_fr(ifps:ifpe,jfps:jfpe), &!fs_fuel_spotting_risk(ims:ime,jms:jme), &
                             spotthreat_fr(ifps:ifpe,jfps:jfpe))
                    fuel_spotting_risk_fr(:,:) = ZERO_dp
                    fs_fuel_spotting_risk(:,:) = ZERO_dp
                    spotthreat_fr(:,:) = ZERO_dp

                    IF (.NOT. ALLOCATED(fcat_fr)) THEN 
                        ALLOCATE(fcat_fr(ifps:ifpe,jfps:jfpe))
                        fcat_fr(:,:) = ZERO_dp                    
                        fcat_fr(ifps:ifpe,jfps:jfpe) = RESHAPE(&
                                                       get_fuel_cat(crosswalk=grid%fuel_crosswalk, &
                                                                    cat=grid%nfuel_cat(ifps:ifpe,jfps:jfpe)), &
                                                       SHAPE(fuel_spotting_risk_fr))
                    ENDIF
                    !WRITE(msg, *) MAXVAL(fcat_fr), MINVAL(fcat_fr)
                    ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd fcat max,min       '//msg)

                    spotthreat_fr(ifps:ifpe,jfps:jfpe)= spotting_threat_factor(fcat_fr(ifps:ifpe,jfps:jfpe)) !RESHAPE(SHAPE(fuel_spotting_risk_fr))                  
                                                             ! get_fuel_cat(crosswalk=grid%fuel_crosswalk, &
                                                             !     cat=grid%nfuel_cat(ifps:ifpe,jfps:jfpe))), & 
                    ! WRITE(msg, *) MAXVAL(spotthreat_fr), MINVAL(spotthreat_fr)
                    ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd spotting_threat_factor max,min'//msg)

                    fuel_spotting_risk_fr(ifps:ifpe,jfps:jfpe) = RESHAPE(&
                                                        fuel_spotting_risk(&
                                                             fuel_fgi=grid%fgip(ifps:ifpe,jfps:jfpe), &
                                                             fuel_mcg=fmcg_2df(ifps:ifpe,jfps:jfpe), & 
                                                             !fuel_mcg=grid%fmcg2df(ifps:ifpe,jfps:jfpe), & 
                                                             factor=spotthreat_fr(ifps:ifpe,jfps:jfpe)), &
                                                             SHAPE(fuel_spotting_risk_fr)) !, fuel_mcg=grid%fmcg2df)

                    fs_fuel_spotting_risk(is:ie,js:je) = fire2tile(fr_arr=fuel_spotting_risk_fr(ifps:ifpe,jfps:jfpe), &
                                                         dsx=grid%sr_x,dsy=grid%sr_y)
                    DEALLOCATE(fcat_fr)

                    !-------------------------------------------------------------------------------


                    ! Prepare for MPI
                    !-------------------------------------------------------------------------------

                    ALLOCATE(landing_cnt(ndep), landed_risk(ndep), recv_spot_lkhd(ndep), recv_frac_landed(ndep))
                    landing_cnt = PACK(fs_count_landed_hist(is:ie,js:je), landing_mask(is:ie,js:je))
                    landed_risk = PACK(fs_fuel_spotting_risk(is:ie,js:je), landing_mask(is:ie,js:je))

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
                    IF (.NOT. (IamMaster)) THEN

                        CALL fs_mpi_sendbuffsize(sendto=MasterId, nbr=ndep) ! Comm send >0: BuffSz
                        CALL fs_mpi_sendbuff_int(sendto=MasterId, bsz=ndep, buff=landing_cnt(1:ndep))
                        CALL fs_mpi_sendbuff_real(sendto=MasterId, bsz=ndep, buff=landed_risk(1:ndep))
                            
                    ENDIF
#endif
                ENDIF ! (fs_count_landed_hist > 0 .AND. fire_area == 0)
            ENDIF ! SUM(fs_count_landed_hist) > 0

            !-------------------------------------------------------------------------------

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
            IF ((ndep == 0) .AND. .NOT.(IamMaster)) &
                CALL fs_mpi_nothing2send(sendto=MasterId) ! Comm send 0: BuffSz
#endif
            !-------------------------------------------------------------------------------
            ! Master: Calculate Likelihood Field
            !-------------------------------------------------------------------------------

            IF (IamMaster) THEN                      

                !-------------------------------------------------------------------------------
                ! Declare arrays to receive data

                ALLOCATE(np_nlanded(ntiles)) 
                np_nlanded(:) = 0
                np_nlanded(1) = ndep ! Array index for Master is 1

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
                DO kk=2, mpiprocs ! kk=1 is Master, MPI-IDs begin at 0
                    np_nlanded(kk) = fs_mpi_recvbuffsize(fromid=kk-1)  ! Comm receive: BuffSz
                    ! WRITE(msg, '(2(i8,1x))') kk,np_nlanded(kk)
                    ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd proc deposit: '//msg)
                ENDDO
#endif
                !-------------------------------------------------------------------------------
                ! Communicate deposits between Master and procs

                ndep_total = SUM(np_nlanded)

                IF ( ndep_total > 0) THEN 
                    ALLOCATE(np_landed_cnt(ndep_total), np_landed_risk(ndep_total))
                    IF (ndep > 0) THEN
                        np_landed_cnt(1:ndep) = landing_cnt
                        np_landed_risk(1:ndep)= landed_risk
                    ELSE 
                        np_landed_cnt(1) = ZERO_dp ! I don't think I need this 
                        np_landed_risk(1)= ZERO_dp
                    ENDIF

                    !-------------------------------------------------------------------------------
                    ! Receive deposit data from procs

                    kk1 = ndep + 1 ! index start after master's 

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
                    DO kk=2, mpiprocs ! kk=1 is Master

                        IF (np_nlanded(kk) > 0) THEN 

                            kk2 = kk1 + np_nlanded(kk) -1                

                            ! WRITE(msg, '(4(i8,1x))') kk, np_nlanded(kk), kk1, kk2
                            ! CALL wrf_debug (wrfdbg, 'SPFire_lkhd master kk, np_dep, k1:k2: '//msg)
                            np_landed_cnt(kk1:kk2) = fs_mpi_recvfrompatch_int(bsz=np_nlanded(kk), fromid=kk-1) 
                            np_landed_risk(kk1:kk2)= fs_mpi_recvfrompatch_real(bsz=np_nlanded(kk), fromid=kk-1)
                            kk1 = kk1 + np_nlanded(kk)
                        ENDIF
                    ENDDO
#endif

                    !-------------------------------------------------------------------------------
                    ! Calculate ratio and lkhd 

                    ALLOCATE(np_frac_landed(ndep_total), np_lkhd(ndep_total))
                    np_frac_landed(:) = ZERO_dp
                    np_lkhd(:) = ZERO_dp

                    np_frac_landed(1:ndep_total) = REAL(np_landed_cnt(1:ndep_total))/REAL(SUM(np_landed_cnt))
                    np_lkhd(1:ndep_total) = np_frac_landed(1:ndep_total) * np_landed_risk(1:ndep_total)
                    np_lkhd(1:ndep_total) = np_lkhd(1:ndep_total)/MAXVAL(np_lkhd)

                    ! DO pp=1,SIZE(np_landed_cnt)
                    !     WRITE(msg, '(3(i8,1x),4(f12.6,1x))') pp, np_landed_cnt(pp), SUM(np_landed_cnt), np_landed_risk(pp), np_frac_landed(pp), MAXVAL(np_lkhd), np_lkhd(pp)
                    !     CALL wrf_debug (wrfdbg, 'SPFire_lkhd master np_frac_landed, np_lkhd, : '//msg)
                    ! ENDDO

                    !-------------------------------------------------------------------------------
                    ! Send dat to procs

                    kk1 = ndep + 1 ! index start after master's 

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
                    DO kk=2, mpiprocs ! kk=1 is Master

                        IF (np_nlanded(kk) > 0) THEN 

                            kk2 = kk1 + np_nlanded(kk) -1                

                            CALL fs_mpi_sendbuff_real(sendto=kk-1, bsz=np_nlanded(kk), buff=np_frac_landed(kk1:kk2))
                            CALL fs_mpi_sendbuff_real(sendto=kk-1, bsz=np_nlanded(kk), buff=np_lkhd(kk1:kk2))

                            kk1 = kk1 + np_nlanded(kk)
                        ENDIF
                    ENDDO
#endif
                ENDIF ! SUM(np_nlanded) > 0
                !-------------------------------------------------------------------------------

            ENDIF ! IamMaster 

            !-------------------------------------------------------------------------------

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
            IF (.NOT.(IamMaster) .AND. (ndep > 0)) THEN

                recv_frac_landed = fs_mpi_recvfrompatch_real(bsz=ndep, fromid=MasterId) 
                fs_frac_landed(is:ie,js:je) = UNPACK(recv_frac_landed, MASK=landing_mask(is:ie,js:je), FIELD=ZERO_dp)

                recv_spot_lkhd = fs_mpi_recvfrompatch_real(bsz=ndep, fromid=MasterId) 
                fs_spotting_lkhd(is:ie,js:je) = UNPACK(recv_spot_lkhd, MASK=landing_mask(is:ie,js:je), FIELD=ZERO_dp)

            ENDIF
#endif

            IF ((IamMaster) .AND. (ndep > 0)) THEN

                fs_frac_landed(is:ie,js:je) = UNPACK(np_frac_landed(1:ndep), MASK=landing_mask(is:ie,js:je), FIELD=ZERO_dp)
                fs_spotting_lkhd(is:ie,js:je) = UNPACK(np_lkhd(1:ndep), MASK=landing_mask(is:ie,js:je), FIELD=ZERO_dp)

            ENDIF

        ENDIF ! hist_flag

        !===============================================================================            
        !===============================================================================            


        !******************************************************************
        !******************************************************************
        !*                                                                *
        !*                  MPI Receive firebrands                   *
        !*                                                                *
        !******************************************************************
        !******************************************************************

        
        !===============================================================================            
        ! MPI: Receive Brands (into this patch)
        !===============================================================================



        !-------------------------------------------------------------------------------
        ! Anything to receive? This part is only needed when compiled in parallel
        !-------------------------------------------------------------------------------

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )

        ALLOCATE(nbr_id(neighbors))
        nbr_id(:)=0
        nbr_id(:)=fs_mpi_checkreceive(task_list=task_id, np=neighbors)

        nbr_sum = SUM(nbr_id)
        IF (nbr_sum > 0) THEN 

            WRITE (msg,'(16(i4,1x))') ([task_id(ii), nbr_id(ii)], ii=1,neighbors)
            CALL wrf_debug (wrfdbg,  'SPFire_driver mpi_check_receive (taskID, N): '//msg)

            ! Receiving positions, id, dt
            ALLOCATE(r_x(nbr_sum), r_y(nbr_sum), r_z(nbr_sum), r_id(nbr_sum), r_src(nbr_sum), r_dt(nbr_sum))
            r_id(:) = 0
            r_src(:) = 0
            r_dt(:) = 0
            r_x(:) = ZERO_dp
            r_y(:) = ZERO_dp
            r_z(:) = ZERO_dp

            ! Receiving p_propertieserties
            ALLOCATE(r_p_m(nbr_sum), r_p_d(nbr_sum), r_p_e(nbr_sum), r_p_t(nbr_sum), r_p_v(nbr_sum))
            ALLOCATE(fb_mdetv(nbr_sum))

            CALL fs_mpi_recv(np_id=nbr_id, task_id=task_id, &
                                   r_x=r_x, r_y=r_y, r_z=r_z, &
                                   r_id=r_id, r_src=r_src, r_dt=r_dt, &
                                   r_p_m=r_p_m, &
                                   r_p_d=r_p_d, & 
                                   r_p_e=r_p_e, &
                                   r_p_t=r_p_t, & 
                                   r_p_v=r_p_v)

            ! Assign values to array and release incoming particles
            !release_mpi_ijk = RESHAPE([r_x,r_y,r_z], [nbr_sum,3])
            fb_mdetv  = [(p_properties(r_p_m(kk), r_p_d(kk), r_p_e(kk), r_p_t(kk), r_p_v(kk)), kk=1,nbr_sum)]

            ! DO kk=1, nbr_sum
            !     WRITE(msg,'(3(i6,1x),3(f12.6,1x))') r_id(kk), r_src(kk), r_dt(kk), r_x(kk), r_y(kk), r_z(kk)
            !     CALL wrf_debug (wrfdbg, 'SPFire_mpi recv AFTER  >>> '// msg)
            ! ENDDO

            prior_br = active_br + 1 ! Array is packed so new particles will be assgned at this position
            CALL generate_firebrands(&
                        fs_p_id = fs_p_id, &
                        fs_p_src = fs_p_src, &
                        fs_p_dt = fs_p_dt, &
                        fs_p_z  = fs_p_z,  &
                        fs_p_x  = fs_p_x,  &
                        fs_p_y  = fs_p_y,  &
                        fs_gen_idmax    = fs_gen_idmax,    &
                        myprocid = myprocid,        &
                        active_br   = active_br,   &
                        release_i = r_x, &
                        release_j = r_y, &
                        release_k = r_z, &
                        release_dt = r_dt, &
                        release_src = r_src, &
                        release_prop= fb_mdetv, & 
                        fs_p_prop   = fs_p_prop)

            WRITE (msg,'(2(i8,1x))') active_br, fs_gen_idmax
            CALL wrf_debug (wrfdbg, 'SPFire_driver mpi recv AFTER : ii, fs_gen_idmax >>> '// msg)

            fs_p_mass (prior_br:active_br) = r_p_m
            fs_p_diam (prior_br:active_br) = r_p_d
            fs_p_effd(prior_br:active_br) = r_p_e
            fs_p_temp (prior_br:active_br) = r_p_t
            fs_p_tvel (prior_br:active_br) = r_p_v

            ! DO kk=prior_br,prior_br+MIN(nbr_sum,5)
            !     WRITE(msg,'(2(i6,1x),6(f12.6,1x))') fs_p_id(kk), fs_p_dt(kk), fs_p_x(kk), fs_p_y(kk), fs_p_z(kk), fs_p_diam(kk), fs_p_temp(kk), fs_p_tvel(kk)
            !     CALL wrf_debug (wrfdbg, 'SPFire_mpi recv AFTER2  >>> '// msg)
            ! ENDDO

            DEALLOCATE(r_x, r_y, r_z, r_id, r_src, r_dt)
            !DEALLOCATE(release_mpi_ijk)
            DEALLOCATE(fb_mdetv)
            DEALLOCATE(r_p_m, r_p_d, r_p_e, r_p_t, r_p_v)

        ! ELSE         
        !     CALL wrf_debug (wrfdbg,  'SPFire_driver mpi nothing to receive')
        ENDIF
        DEALLOCATE(nbr_id)
        !-------------------------------------------------------------------------------
        ! DONE :)
        !-------------------------------------------------------------------------------
#endif
        !===============================================================================
        ! Check history interval 
        !===============================================================================

        hist_flag = .FALSE. 


    END SUBROUTINE firebrand_spotting_em_driver
!=============================================================

!=============================================================

    

!=============================================================
!=============================================================

!******************************************************************
!******************************************************************
!*                                                                *
!*                Particle Transport & Physics                    *
!*                                                                *
!******************************************************************
!******************************************************************
    
!=============================================================
    PURE &
    SUBROUTINE advect_xyz_m(grid, xp, yp, hgt, dtp, idp, &
                            u, v, w, dt, mf, z_at_w, znw, ims, jms, kms, &
                            phyd, thet, rho, &
                            xout, yout, zout, fs_p_prop, land_hgt, &
                            start_mom3d_dt, msg)
!=============================================================

    ! Transport a particle at a given xp, yp, zp position in meters
    ! given the corresponding velocities [m/s] at the enclosing grid points

    ! arguments 
    ! xp, yp, hgt: starting particle position
    ! u, v, w: wind components 
    ! dt: time interval in sec 
    ! ims, jms : start index of wind components
    ! mf: constant to convert from grid position to meters: rdx*msft(x,y)
    ! z_at_w: wrf height at vertical interface
    ! znw: eta levels

    !-------------------------------------------------------------

        IMPLICIT NONE

        TYPE(domain), INTENT(IN) :: grid 
        INTEGER, INTENT(IN)   :: ims, jms, kms, start_mom3d_dt
        REAL,    INTENT(IN)   :: dt
        REAL, INTENT(IN)      :: land_hgt
        REAL,    INTENT(IN),  DIMENSION(:) :: xp, yp, hgt ! particle position
        INTEGER, INTENT(IN),  DIMENSION(:) :: dtp, idp    ! particle life-time and ID
        REAL,    INTENT(IN),  DIMENSION(ims:, kms:, jms:) :: u, v, w, z_at_w
        REAL,    INTENT(IN),  DIMENSION(ims:, kms:, jms:) :: phyd, thet, rho
        REAL,    INTENT(IN),  DIMENSION(:) :: znw
        REAL,    INTENT(IN),  DIMENSION(ims:, jms:) :: mf ! map factor, msft * rdx
        REAL,    INTENT(OUT), DIMENSION(:) :: xout, yout, zout!, aglh
        TYPE(p_properties),INTENT(INOUT),DIMENSION(:):: fs_p_prop

        REAL,    DIMENSION(3) :: uvw2p 
        INTEGER :: pp, aux
        INTEGER :: hsz, ihs, jhs, ihe, jhe  ! tile +- halo array bounds
        REAL    :: xp_m0, yp_m0, zp_m0, zp0
        REAL    :: xp_m1, yp_m1, zp_m1, xp1, yp1, zp1
        REAL    :: xp_m2, yp_m2, zp_m2, xp2, yp2, zp2
        REAL    :: zp, wp, brz
        REAL    :: loc_p, loc_t, loc_d ! local met properties
        CHARACTER (LEN=200), INTENT(OUT) :: msg

        WRITE(msg, '(a100)') ' ' 
    !-------------------------------------------------------------------------------

        hsz = 4 ! halo size - get this value from grid%. Halo:48 = 4 points. How about staggering?
        ihs = is - hsz    ! MAX(, ids)
        jhs = js - hsz    ! MAX(, jds)
        ihe = ie -1 + hsz ! MIN(, ide-1) ! check for ide -1 bc we use i, i+1 to interpolate
        jhe = je -1 + hsz ! MIN(, jde-1)

        ! WRITE (msg,'(4(i4,1x))') ihs,ihe, jhs, jhe
        ! CALL wrf_debug (wrfdbg, 'SPFire_advect halo bounds ihs, ihe, jhs, jhe: >>> '//msg)

        !start_mom3d_dt = 4 ! min lifetime before calculating fb physics (number of timesteps for this nested grid)

        DO pp=1, SIZE(xp)

            ! CALL wrf_debug (wrfdbg, 'SPFire advect_xyz_m ---------------------------------------------------- ')

            ! WRITE (msg,'(3(i8,1x),3(f12.6,1x))') pp, idp(pp), dtp(pp), xp(pp), yp(pp), hgt(pp)
            ! CALL wrf_debug (wrfdbg, 'SPFire BEFORE advect:    pp,id,dt,xp,yp,zp:    >>> '//msg)

            !-------------------------------------------------------------------------------
            ! Convert horizontal positions to meters
            !-------------------------------------------------------------------------------
        
            xp_m0 = xp(pp) / mf(FLOOR(xp(pp)),FLOOR(yp(pp)))
            yp_m0 = yp(pp) / mf(FLOOR(xp(pp)),FLOOR(yp(pp)))
            zp_m0 = hgt(pp)

            ! zp changes slightly between wrf time steps, so it need to be calculated again
            zp0 = hgt2k(xp=xp(pp), yp=yp(pp), hgt=zp_m0, z_at_w=z_at_w, znw=znw)
            ! (Need fractional position for interpolation)

            ! WRITE (msg,'(3(i8,1x),4(f12.6,1x))') pp, idp(pp), dtp(pp), xp(pp), yp(pp), hgt(pp), zp0
            ! CALL wrf_debug (wrfdbg, 'SPFire_advect BEFORE:    pp,id,dt,xp,yp,zp,zk:      >>> '//msg)

            ! If this is a particle deposited by an adjacent tile, do not transport it 
            IF (zp_m0 <= land_hgt) THEN 
                ! CALL wrf_debug (wrfdbg, 'SPFire_advect particle at deposit threshold - will not advect') 

                xout(pp) = xp(pp)
                yout(pp) = yp(pp)
                zout(pp) = zp_m0
                CYCLE

            ENDIF

            !-------------------------------------------------------------------------------
            !-------------------------------------------------------------------------------

        
        ! 1ST PASS
        !=============================================================
        

            !-------------------------------------------------------------------------------
            ! #1 interpolate velocities to particle position & advect particle
            !-------------------------------------------------------------------------------
        
            uvw2p = uvw_3d_interp(x=xp(pp), y=yp(pp), z=zp0, u=u, v=v, w=w, ihs=ihs, jhs=jhs, ihe=ihe, jhe=jhe) ! args: grid fractional positions

            xp_m1 = uvw2p(1)*DT + xp_m0
            yp_m1 = uvw2p(2)*DT + yp_m0
            zp_m1 = uvw2p(3)*DT + zp_m0

            !-------------------------------------------------------------------------------
            ! Convert horizontal positions from meters
            !-------------------------------------------------------------------------------

            xp1 = xp_m1 * mf(FLOOR(xp(pp)),FLOOR(yp(pp))) ! 2 pass will account for grid pt changes in mf & u 
            yp1 = yp_m1 * mf(FLOOR(xp(pp)),FLOOR(yp(pp)))

            IF((ABS(xp1-xp(pp)) > 2.0) .OR. (ABS(yp1-yp(pp)) > 2.0)) THEN
                WRITE (msg,'(2(i4,1x),7(f10.6,1x))') pp, dtp(pp), xp(pp), yp(pp), zp0, &
                                                     uvw2p(1), uvw2p(2), uvw2p(3), dt ! 
                zout(pp) = ZERO_dp   
                xout(pp) = ZERO_dp 
                yout(pp) = ZERO_dp 
                CYCLE
            ENDIF

            !-------------------------------------------------------------------------------
            ! Convert z position from meters to k-fractional
            !-------------------------------------------------------------------------------

            ! CALL wrf_debug (wrfdbg, 'SPFire advect_xyz_m 1st pass --------------------------------------------- ')

            ! WRITE (msg,'(3(i8,1x),3(f12.6,1x))') pp, idp(pp), dtp(pp), xp1, yp1, zp_m1
            ! CALL wrf_debug (wrfdbg, 'SPFire after 1-pass adv: pp,id,dt,xp,yp,zp:     >>> '//msg)
            ! WRITE (msg,*) pp, z_at_w(FLOOR(xp1),1,FLOOR(yp1))
            ! CALL wrf_debug (wrfdbg, 'SPFire after 1-pass adv: zn:                    >>> '//msg)
            ! WRITE (msg,*) pp, z_at_w(FLOOR(xp1-dp05),1,FLOOR(yp1-dp05))
            ! CALL wrf_debug (wrfdbg, 'SPFire after 1-pass adv: zn:                    >>> '//msg)

            zp1 = hgt2k(xp=xp1, yp=yp1, hgt=zp_m1, z_at_w=z_at_w, znw=znw)
            wp = uvw2p(3)

            !-------------------------------------------------------------------------------
            ! Is the new position not on the same memory? 
            !-------------------------------------------------------------------------------
            ! check for floor(x)+1 bc we use i and i+1 to interpolate            
            IF (FLOOR(xp1-dp05) < ihs .OR. FLOOR(xp1-dp05) +1 > ihe .OR. &
                FLOOR(yp1-dp05) < jhs .OR. FLOOR(yp1-dp05) +1 > jhe) THEN
                ! Checking staggered positions relative to unstaggered variables (+- dp05)

                !-------------------------------------------------------------------------------
                ! Use only one pass
                !-------------------------------------------------------------------------------

                xout(pp) = xp1
                yout(pp) = yp1
                zout(pp) = zp_m1
                zp = zp1

            ELSE

            ! 2ND PASS
            !=============================================================

                !-------------------------------------------------------------------------------
                ! #2 interpolate velocities to particle position & advect particle
                !-------------------------------------------------------------------------------
        
                uvw2p = uvw_3d_interp(x=xp1, y=yp1, z=zp1, u=u, v=v, w=w, ihs=ihs, jhs=jhs, ihe=ihe, jhe=jhe) ! args: grid fractional positions

                ! WRITE (msg,'(3(f14.9,1x))') uvw2p(1), uvw2p(2), uvw2p(3)
                ! CALL wrf_debug (wrfdbg, 'SPFire_advect effective vel m/s: u,v,w 2nd pass>>> '//TRIM(msg))

                xp_m2 = uvw2p(1)*DT + xp_m0
                yp_m2 = uvw2p(2)*DT + yp_m0
                zp_m2 = uvw2p(3)*DT + zp_m0

                xp2 = xp_m2 * mf(FLOOR(xp1),FLOOR(yp1))
                yp2 = yp_m2 * mf(FLOOR(xp1),FLOOR(yp1))
                
                IF((ABS(xp2-xp1) > 2.0) .OR. (ABS(yp2-yp1) > 2.0)) THEN
                    WRITE (msg,'(2(i4,1x),7(f10.6,1x))') pp, dtp(pp), xp1, yp1, zp1, &
                                                         uvw2p(1), uvw2p(2), uvw2p(3), dt ! 
                    zout(pp) = ZERO_dp   
                    xout(pp) = ZERO_dp 
                    yout(pp) = ZERO_dp 
                CYCLE
            ENDIF


                !-------------------------------------------------------------------------------
                ! Is the new position not on the same memory?  
                !-------------------------------------------------------------------------------
            
                ! check against tile +- halo points instead

                IF (FLOOR(xp2-dp05) < ihs .OR. FLOOR(xp2-dp05) +1 > ihe .OR. &
                    FLOOR(yp2-dp05) < jhs .OR. FLOOR(yp2-dp05) +1 > jhe) THEN
                    ! Checking staggered positions relative to unstaggered variables (+- dp05)

                    !-------------------------------------------------------------------------------
                    ! Use xp1, yp1 to find zp2 (z_at_w will be out of bounds)
                    !-------------------------------------------------------------------------------

                    zp2 = hgt2k(xp=xp1, yp=yp1, hgt=zp_m2, z_at_w=z_at_w, znw=znw)

                ELSE

                    zp2 = hgt2k(xp=xp2, yp=yp2, hgt=zp_m2, z_at_w=z_at_w, znw=znw)
        
                ENDIF

            !=============================================================

                !-------------------------------------------------------------------------------
                ! Average both passes
                !-------------------------------------------------------------------------------

                xp2 = (xp1 + xp2) * dp05 
                yp2 = (yp1 + yp2) * dp05 
                zp_m2 = (zp_m1 + zp_m2) * dp05
                zp2 = (zp1 + zp2) * dp05

                xout(pp) = xp2
                yout(pp) = yp2
                zout(pp) = zp_m2
                zp = zp2
                wp = (uvw2p(3) + wp) * dp05
                
            ENDIF

            !=============================================================
            ! Transport Done :)
            !=============================================================
            ! WRITE (msg, '(a100)') ' ' 
            ! WRITE (msg,'(2(i6,1x),4(f12.6,1x))') pp, dtp(pp), xout(pp), yout(pp), zout(pp), zp
            ! CALL wrf_debug (wrfdbg, 'SPFire_advect AFTER :    pp,id,dt,xp,yp,zp,kp:      >>> '//msg)

            !=============================================================
            ! Firebrand Physics 
            !=============================================================

            loc_p = ZERO_dp
            loc_d = ZERO_dp
            loc_t = ZERO_dp

            IF (zout(pp) <= ZERO_dp) THEN
                fs_p_prop(pp) = p_properties(ZERO_DP, ZERO_DP, ZERO_DP, ZERO_DP, ZERO_DP)
                zout(pp) = ZERO_dp   
                CYCLE ! Skip physics              
            ENDIF

            IF (IEEE_IS_NAN(zout(pp)) .OR. ABS(zout(pp)) > HUGE(1.0)) THEN
                fs_p_prop(pp) = p_properties(ZERO_DP, ZERO_DP, ZERO_DP, ZERO_DP, ZERO_DP)
                zout(pp) = ZERO_dp   
                xout(pp) = ZERO_dp 
                yout(pp) = ZERO_dp 

                CYCLE ! Skip physics              
            ENDIF

            IF (dtp(pp) < start_mom3d_dt) THEN ! Temporarily testing transport: extending particle lifetime 
                ! WRITE (msg,*) dt
                ! CALL wrf_debug (wrfdbg, 'SPFire_physics cycle dt: '//msg)
                CYCLE
            ENDIF

                        
            CALL get_local_met( xp  = xout(pp),   &
                                yp  = yout(pp),   &
                                zp  = zp,         &
                                ihs = ihs,  &
                                jhs = jhs,  &
                                ihe = ihe,  &
                                jhe = jhe,  &
                                p = phyd, & ! air pressure (Pa)
                                t = thet, & ! temperature (K)
                                d = rho,  & ! density (kg/m3)
                                loc_p = loc_p,    & ! local air pressure 3d (Pa)
                                loc_d = loc_d,    & ! local density (g/m3)
                                loc_t = loc_t)      ! local temperature (K)
            
            ! WRITE (msg,'(3(f16.8,1x))') grid%p_hyd_w(FLOOR(xout(pp)),FLOOR(zp),FLOOR(yout(pp))), &
            !                             grid%rho(FLOOR(xout(pp)),FLOOR(zp),FLOOR(yout(pp))), &
            !                             grid%t_phy(FLOOR(xout(pp)),FLOOR(zp),FLOOR(yout(pp)))
            ! CALL wrf_debug (wrfdbg, 'SPFire grid%p, %rho, %t_phy      : '//msg)

            ! WRITE (msg,'(4(f16.8,1x))') loc_p, loc_d, loc_t, wp
            ! CALL wrf_debug (wrfdbg, 'SPFire br met p, d, t, w : '//msg)

            !-------------------------------------------------------------------------------
            ! Firebrand Burnout and Terminal Velocity
            !-------------------------------------------------------------------------------
                
            brz = zout(pp)
            CALL firebrand_physics(dt = dt,         &
                                     hgt = brz,  &
                                     loc_w = wp,      &
                                     loc_p = loc_p,   & 
                                     loc_t = loc_t,   & 
                                     loc_d = loc_d,   &     
                                     fbprop  = fs_p_prop(pp))
            
            IF (IEEE_IS_NAN(fs_p_prop(pp)%p_tvel)) THEN
                fs_p_prop(pp) = p_properties(ZERO_DP, ZERO_DP, ZERO_DP, ZERO_DP, ZERO_DP)
                zout(pp) = ZERO_dp

                ! WRITE (msg,'(3(i8,1x),4(f12.6,1x))') pp, idp(pp), dtp(pp), xout(pp), yout(pp), brz, fs_p_prop(pp)%p_tvel
                ! CALL wrf_debug (wrfdbg, 'SPFire_physics AFTER:   pp,id,dt,xp,yp,zp,fbvt:    >>> '//msg)
                    
            ELSE

                zout(pp) = brz

            ENDIF
            ! WRITE (msg,'(3(i8,1x),4(f12.6,1x))') pp, idp(pp), dtp(pp), xout(pp), yout(pp), brz, fs_p_prop(pp)%p_tvel*dt
            ! CALL wrf_debug (wrfdbg, 'SPFire_physics AFTER:   pp,id,dt,xp,yp,zp,fbvt:    >>> '//msg)

        ENDDO

    END SUBROUTINE advect_xyz_m
!=============================================================


! Cannot be elemental: array
!=============================================================
PURE FUNCTION uvw_3d_interp(x, y, z, u, v, w, ihs, jhs, ihe, jhe)
!=============================================================

    ! Wrapper to interpolate U,V,W to a point within a 3D grid box

    ! Interpolate velocities at grid point interfaces to particle position
    ! input range needed: i:i+1 | i < x(i) <= i+1
    !                     j:j+1 | j < y(j) <= j+1
    !                     k:k+1 | k < z(k) <= k+1

    ! arguments 
    ! x, y, z: fractional position to interpolate velocities (e.g.: z is a fraction of the k index)
    ! u, v, w: wind components 
    ! is, js : start index of wind components

    !-------------------------------------------------------------
    ! Interpolate between planes: U: xy(k), xy(k+1) stag x, dim=1
    !                             V: xy(k), xy(k+1) stag y, dim=2
    !                             W: xz(j), xz(j+1) stag z, dim=2
    !
    ! particle position is not staggered: 
    !          grid center is given by (FLOOR(x), FLOOR(y)) 
    !-------------------------------------------------------------

    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: ihs, jhs, ihe, jhe  ! tile +- halo array bounds
    REAL,    INTENT(IN)   :: x, y, z ! fractional positions relative to integral array indices
    REAL,    INTENT(IN), DIMENSION(ims:, 1:, jms:) :: u, v ,w
    REAL,    DIMENSION(3) :: uvw_3d_interp 
    REAL     :: tru, trv, trw, x0, y0, z0
    INTEGER  :: i, j, k, i0, j0, k0

    ! Unstaggerecd indices
    k = MAX(FLOOR(z), 1)
    j = FLOOR(y) 
    i = FLOOR(x) 

    ! Variable Staggering:
    ! particle position is equivalent to a staggered dimension
    ! the staggered domain begins 1/2 pnt before and ends 1/2 pnt after the regular domain 
    ! e.g: interpolate u (stag x): x_stag = xp, y_unst = yp -0.5, z_unst = zp -0.5
    ! By shifting the particle position by 1/2 point, 
    ! the unstaggered velocities will represent the grid edge instead of grid center

    x0 = x - dp05
    y0 = y - dp05
    z0 = z - dp05 ! z can be < 1.0 when below the first half level, u_3d_interp will extrapolate values

    i0 = FLOOR(x0) !MIN(MAX(ihs, ), ihe) ! shift indices by 0.5 because unstaggered variables given at horizontal grid center
    j0 = FLOOR(y0) !MIN(MAX(jhs, ), ihe)
    k0 = MAX(1, FLOOR(z0))

    !-----------------------------------------------------------------------------
    ! u,v interpolates between xy planes: xy(k) xy(k+1)
    !-----------------------------------------------------------------------------

    ! staggered x
    tru= u_3d_interp( x=x, y=y0, z=z0, &
        u_i0j0_bot=u(i, k0,   j0  ), u_i0j1_bot=u(i, k0,   j0+1), u_i1j0_bot=u(i+1, k0,   j0  ), u_i1j1_bot=u(i+1, k0,   j0+1), &
        u_i0j0_top=u(i, k0+1, j0  ), u_i0j1_top=u(i, k0+1, j0+1), u_i1j0_top=u(i+1, k0+1, j0  ), u_i1j1_top=u(i+1, k0+1, j0+1))

    ! staggered y
    trv= u_3d_interp( x=x0, y=y, z=z0, &
        u_i0j0_bot=v(i0, k0,   j  ), u_i0j1_bot=v(i0, k0,   j+1), u_i1j0_bot=v(i0+1, k0,   j  ), u_i1j1_bot=v(i0+1, k0,   j+1), &
        u_i0j0_top=v(i0, k0+1, j  ), u_i0j1_top=v(i0, k0+1, j+1), u_i1j0_top=v(i0+1, k0+1, j  ), u_i1j1_top=v(i0+1, k0+1, j+1))

    !-----------------------------------------------------------------------------
    ! w interpolates between xz planes, not xy (so y corresponds to z): xz(j) xz(j+1) 
    !-----------------------------------------------------------------------------

    ! z and w are both staggered (z is derived from grid%z_at_w) 
    trw= u_3d_interp( x=x0, y=z, z=y, &
        u_i0j0_bot=w(i0, k,   j  ), u_i0j1_bot=w(i0, k+1, j  ), u_i1j0_bot=w(i0+1, k,   j  ), u_i1j1_bot=w(i0+1, k+1, j  ), &
        u_i0j0_top=w(i0, k,   j+1), u_i0j1_top=w(i0, k+1, j+1), u_i1j0_top=w(i0+1, k,   j+1), u_i1j1_top=w(i0+1, k+1, j+1)) 

    !-----------------------------------------------------------------------------
    ! Return U, V, W at x, y, z positions
    !-----------------------------------------------------------------------------

    uvw_3d_interp = [tru, trv, trw]

END FUNCTION uvw_3d_interp
!=============================================================



!=============================================================
ELEMENTAL FUNCTION u_3d_interp(x, y, z, &
    u_i0j0_bot, u_i0j1_bot, u_i1j0_bot, u_i1j1_bot, &
    u_i0j0_top, u_i0j1_top, u_i1j0_top, u_i1j1_top )
!=============================================================

    ! Interpolate velocities at grid point interfaces to particle position
    ! input range needed: u( i: i+1, j: j+1, k: k+1) |
    !                       i < x <= i+1
    !                       j < y <= j+1
    !                       k < z <= k+1

    ! arguments 
    ! x, y, z: particle position to interpolate velocities
    ! u, v, w: wind components 

    !-----------------------------------------------------------------------------
    ! Bilinear interpolation :
    ! The product of the value at the desired point (x,y,z) and the area enclosed by the corners
    ! is equal to the sum of the 
    ! products of the value at each corner and the partial area diagonally opposite the corner
    !-----------------------------------------------------------------------------

    !-----------------------------------------------------------------------------
    ! The 3-D interpolation is achieved with a linear interpolation between 
    ! the xy-interpolated points from two adjacent planes
    !-----------------------------------------------------------------------------

    !-----------------------------------------------------------------------------
    ! Z must be offset by 0.5 to correctly interpolate between vertically unstaggered planes
    !-----------------------------------------------------------------------------

    REAL, INTENT(IN)   :: x, y, z ! fractional position
    REAL, INTENT(IN) :: u_i0j0_bot, u_i0j1_bot, u_i1j0_bot, u_i1j1_bot ! u_lower
    REAL, INTENT(IN) :: u_i0j0_top, u_i0j1_top, u_i1j0_top, u_i1j1_top ! u_upper
    REAL :: u_3d_interp

    REAL :: dbot, dtop, u_lower, u_upper
    INTEGER :: k, j, i

    !-----------------------------------------------------------------------------
    ! Get weighted U, V, W at particle position based on surrounding xy and xz planes
    !-----------------------------------------------------------------------------

    ! index
    k = MAX(1, FLOOR(z)) ! To interpolate w, z corresponds y:  ( x=x, y=z, z=y, ...

    ! z can be < 1.0 when interpolating below the first level on half levels
    ! e.g.: z=0.75, dbot = 0.75 -1 =-0.25, dtop = 2 - 0.75 =1.25
    !       u_3d_interp = u_upper*-0.25 + u_lower*1.25

    dbot = z - REAL(k) 
    dtop = REAL(k+1) - z

    !-----------------------------------------------------------------------------
    ! j = FLOOR(y) 
    ! i = FLOOR(x) 

    u_lower = u_2d_interp( u_i0j0=u_i0j0_bot, u_i0j1=u_i0j1_bot, &
                           u_i1j0=u_i1j0_bot, u_i1j1=u_i1j1_bot, xp=x, yp=y)
    u_upper = u_2d_interp( u_i0j0=u_i0j0_top, u_i0j1=u_i0j1_top, &
                           u_i1j0=u_i1j0_top, u_i1j1=u_i1j1_top, xp=x, yp=y)

    !-----------------------------------------------------------------------------
    ! Apply weights and calculate U at particle position
    !-----------------------------------------------------------------------------

    u_3d_interp = u_upper * dbot + & ! 
                  u_lower * dtop

    !-----------------------------------------------------------------------------

END FUNCTION u_3d_interp
!=============================================================

!=============================================================



!=============================================================
 ELEMENTAL FUNCTION u_2d_interp(u_i0j0, u_i0j1, u_i1j0, u_i1j1, xp, yp) 
!=============================================================

    ! Interpolate any variable at grid point interfaces to a position in between
    ! Minimum range: u(i:i+1, j:j+1) | i < xp <= i+1  &  j < yp <= j+1

    ! arguments 
    ! u      : 2D variable at grid 4 corners: (i0,j0); (i0,j1); (i1,j0); (i1,j1)
    ! xp, yp : position within grid box to interpolate velocity
    ! is, js : start index of variable array boundary

    !-------------------------------------------------------------
    ! 2D Interpolation sketch
    ! a, b, c, d: grid point corners
    ! da, db, dc, dd: distance between the nearest corner and point projection
    !
    ! a + + + + + + d
    !   + dc | db +
    !   +____|____+
    !   + dd | da +
    !   +    |    +
    ! b + + + + + + c
    !-------------------------------------------------------------

    IMPLICIT NONE
    
    !INTEGER, INTENT(IN) :: is, js
    REAL,    INTENT(IN) :: xp, yp
    REAL,    INTENT(IN) :: u_i0j1, u_i0j0, u_i1j0, u_i1j1 ! DIMENSION(is:,js:) :: uu
    REAL    :: u_2d_interp
    REAL    :: d_a, d_b, d_c, d_d      ! grid vertices 
    REAL    :: uu_a, uu_b, uu_c, uu_d  ! advection at vertices 
    INTEGER :: i, j

    i = FLOOR(xp)
    j = FLOOR(yp)

    uu_a = u_i0j1 ! uu(i  ,j+1)
    uu_b = u_i0j0 ! uu(i  ,j  )
    uu_c = u_i1j0 ! uu(i+1,j  )
    uu_d = u_i1j1 ! uu(i+1,j+1)

    ! WRITE (msg,'(4(f14.9,1x))') uu_a, uu_b, uu_c, uu_d
    ! CALL wrf_debug (wrfdbg, 'SPFire_uu_2d: uu _a,_b,_c,_d                   >>> '//TRIM(msg))

    d_a = ABS( ( REAL(i+1) - xp ) * ( REAL(j  ) - yp ) )
    d_b = ABS( ( REAL(i+1) - xp ) * ( REAL(j+1) - yp ) )
    d_c = ABS( ( REAL(i  ) - xp ) * ( REAL(j+1) - yp ) )
    d_d = ABS( ( REAL(i  ) - xp ) * ( REAL(j  ) - yp ) )

    ! WRITE (msg,'(4(f14.9,1x))') d_a, d_b, d_c, d_d
    ! CALL wrf_debug (wrfdbg, 'SPFire_uu_2d: d _a,_b,_c,_d                    >>> '//TRIM(msg))


   u_2d_interp = ( uu_a * d_a + &
               uu_b * d_b + &
               uu_c * d_c + &
               uu_d * d_d ) / &
               (d_a + d_b + d_c + d_d)

    ! WRITE (msg,*) uu_2d_interp
    ! CALL wrf_debug (wrfdbg, 'SPFire_uu_2d:                              >>> '//TRIM(msg))

END FUNCTION u_2d_interp
!=============================================================

!=============================================================




!=============================================================
PURE SUBROUTINE remove_br( fs_p_id, fs_p_x, fs_p_y, fs_p_z, fs_p_dt, fs_p_effd, cnt, max_life_dt, land_hgt)
!=============================================================
    IMPLICIT NONE
    INTEGER, INTENT(INOUT), DIMENSION(:) :: fs_p_id
    INTEGER, INTENT(IN),    DIMENSION(:) :: fs_p_dt
    !INTEGER, INTENT(IN) :: active_br
    REAL,    INTENT(IN), DIMENSION(:) :: fs_p_x, fs_p_y, fs_p_z, fs_p_effd
    LOGICAL, DIMENSION(fs_array_maxsize) :: sparse_mask
    INTEGER, INTENT(OUT) :: cnt

    INTEGER, INTENT(IN) :: max_life_dt
    REAL, INTENT(IN)    :: land_hgt
    
    !-------------------------------------------------------------------------------
    
    sparse_mask = .FALSE. 
    !------------------------------------------------------------------------------- 
    ! Flag to remove brands beyond domain bounds
    !-------------------------------------------------------------------------------

    sparse_mask = ( fs_p_id > 0 .AND. &
                    (FLOOR(fs_p_x-dp05)   < ids .OR. FLOOR(fs_p_y-dp05)   < jds .OR. &
                     FLOOR(fs_p_x-dp05)+1 > ide .OR. FLOOR(fs_p_y-dp05)+1 > jde)) 
    ! interpolation needs i, j, i+1, j+1
    ! First we unstagger fs_p_x, fs_p_y to align with met variables (x=fs_p_x-0.5, y=fs_p_y-0.5)
    ! Then we use FLOOR(x), FLOOR(y), FLOOR(x)+1, FLOOR(y)+1 to interpolate

    !-------------------------------------------------------------------------------
    ! Flag to remove brands living longer than firebrand_max_life_dt
    !-------------------------------------------------------------------------------

    sparse_mask = ( sparse_mask .OR. & 
                   (fs_p_id > 0 .AND. fs_p_dt > max_life_dt))

    !-------------------------------------------------------------------------------
    ! Flag to remove brands with near zero mass
    !-------------------------------------------------------------------------------

    ! sparse_mask = (sparse_mask .OR. & 
    !                (fs_p_id > 0 .AND. fs_p_mass <= br_min_mass))

    !-------------------------------------------------------------------------------
    ! Flag to remove brands with near zero diameter that are not at deposit height
    !-------------------------------------------------------------------------------

    sparse_mask = (sparse_mask .OR. & 
                   (fs_p_id > 0 .AND. ((fs_p_effd <= TINY(1.0) .AND. (fs_p_z > land_hgt)))))

    !-------------------------------------------------------------------------------
    ! Reset array
    !-------------------------------------------------------------------------------
    cnt = 0
    cnt = COUNT(sparse_mask)
    IF (cnt > 0) THEN

        WHERE(sparse_mask) fs_p_id = 0

        ! WRITE (msg,'(3(i8,1x))') COUNT(sparse_mask), COUNT( fs_p_id > 0 )
        ! CALL wrf_debug (wrfdbg, 'SPFire_remove removed, remaining: '//msg)
        !CALL wrf_error_fatal( "end of test") 
    ENDIF
    
END SUBROUTINE remove_br

!=============================================================
!=============================================================


!=============================================================
PURE SUBROUTINE deposit_br(fs_p_id, fs_p_x, fs_p_y, fs_p_z,  sum_fbrand, land_hgt)
!=============================================================

    IMPLICIT NONE
    INTEGER, INTENT(INOUT), DIMENSION(:) :: fs_p_id
    REAL,    INTENT(INOUT), DIMENSION(ims:ime,jms:jme) :: sum_fbrand
    REAL,    INTENT(IN),    DIMENSION(:) :: fs_p_x, fs_p_y, fs_p_z
    REAL,    INTENT(IN)    :: land_hgt

    !INTEGER, INTENT(IN) :: active_br

    !INTEGER, ALLOCATABLE, DIMENSION(:) :: mask_idx
    INTEGER, ALLOCATABLE, DIMENSION(:) :: rx, ry, rid
    LOGICAL, DIMENSION(fs_array_maxsize)    :: sparse_mask
    LOGICAL, DIMENSION(fs_array_maxsize)    :: bounds_mask
    !LOGICAL, ALLOCATABLE, DIMENSION(:) :: bounds_mask
    INTEGER :: nresets
    INTEGER :: x, y, i0,k ! counters
    !CHARACTER(LEN=10) fmt

    !CALL wrf_debug (wrfdbg, 'SPFire_deposit: check')
    sum_fbrand(:,:) = ZERO_dp

    !-------------------------------------------------------------------------------
    ! Flag brands near ground (deps_lev is a declared parameter)
    !-------------------------------------------------------------------------------

    sparse_mask = .FALSE. 
    bounds_mask = .FALSE. 

    ! Only deposit particles on this tile
    bounds_mask = ((FLOOR(fs_p_x) >= is) .OR. &
                   (FLOOR(fs_p_x) <= ie) .OR. &
                   (FLOOR(fs_p_y) >= js) .OR. &
                   (FLOOR(fs_p_y) <= je))

    sparse_mask = ( bounds_mask .AND. (fs_p_id > 0 .AND. fs_p_z <= land_hgt))

    !-------------------------------------------------------------------------------
    ! Deposit flagged brands
    !-------------------------------------------------------------------------------
    nresets = 0
    nresets = COUNT(sparse_mask)

    ! WRITE (msg,'(i6)') nresets
    ! CALL wrf_debug (wrfdbg, 'SPFire_deposit total: '//msg)

    IF (nresets > 0) THEN

        !-------------------------------------------------------------------------------
        ! convert position to integer indices
        !-------------------------------------------------------------------------------

        ALLOCATE(rx(nresets),ry(nresets))!, mask_idx(nresets), rid(nresets), bounds_mask(nresets))

        ! Get indices of particles when sparse_mask = True
        rx(:) = 0
        ry(:) = 0

        rx(1:nresets) = FLOOR(PACK(fs_p_x, sparse_mask))
        ry(1:nresets) = FLOOR(PACK(fs_p_y, sparse_mask))

        !-------------------------------------------------------------------------------
        ! Increment sum_fbrand
        !-------------------------------------------------------------------------------

        DO k=1,nresets
            sum_fbrand(rx(k),ry(k)) = sum_fbrand(rx(k),ry(k)) + 1.0_dp
        ENDDO

        !-------------------------------------------------------------------------------
        ! Reset array
        !-------------------------------------------------------------------------------

        WHERE(sparse_mask) fs_p_id= 0
        
        ! WRITE (msg,'(5(i8,1x))') COUNT(sparse_mask), COUNT( fs_p_id > 0 ), COUNT(rx >= ie), COUNT(ry >= je)
        ! CALL wrf_debug (wrfdbg, 'SPFire_deposit deposited, remaining, outofbounds: '//msg)

    ENDIF

END SUBROUTINE deposit_br


!=============================================================
!=============================================================



 !------------------------------------------------------------------------------
 ! UTILITIES
 !------------------------------------------------------------------------------


 !=============================================================
 
    SUBROUTINE get_local_ijk(grid, &
        ims, ime, jms, jme, kms, kme, &
        ips, ipe, jps, jpe, kps, kpe, &
        ifps, ifpe, jfps, jfpe,       &
        ifms, ifme, jfms, jfme,       &
        ids, ide, jds, jde, kds, kde, &
        m_idim,   m_jdim,   m_kdim,   &
        p_idim,   p_jdim,   p_kdim,   &
        d_idim,   d_jdim,   d_kdim)
        !p2m_is, p2m_ie, p2m_js, p2m_je)

        USE module_domain, ONLY: get_ijk_from_grid, get_ijk_from_subgrid

        IMPLICIT NONE

        TYPE(DOMAIN), INTENT(IN) :: grid 
        INTEGER,      INTENT(OUT), OPTIONAL :: ims, ime, jms, jme, kms, kme
        INTEGER,      INTENT(OUT), OPTIONAL :: ips, ipe, jps, jpe, kps, kpe
        INTEGER,      INTENT(OUT), OPTIONAL :: ifps, ifpe, jfps, jfpe
        INTEGER,      INTENT(OUT), OPTIONAL :: ifms, ifme, jfms, jfme
        INTEGER,      INTENT(OUT), OPTIONAL :: ids, ide, jds, jde, kds, kde
        INTEGER,      INTENT(OUT), OPTIONAL :: m_idim,   m_jdim,   m_kdim
        INTEGER,      INTENT(OUT), OPTIONAL :: p_idim,   p_jdim,   p_kdim
        INTEGER,      INTENT(OUT), OPTIONAL :: d_idim,   d_jdim,   d_kdim
        !INTEGER,      INTENT(OUT), OPTIONAL :: p2m_is, p2m_ie, p2m_js, p2m_je


        INTEGER :: iims, iime, jjms, jjme, kkms, kkme
        INTEGER :: iips, iipe, jjps, jjpe, kkps, kkpe
        INTEGER :: iids, iide, jjds, jjde, kkds, kkde

        INTEGER :: iifps, iifpe, jjfps, jjfpe, kkfps, kkfpe
        INTEGER :: iifms, iifme, jjfms, jjfme, kkfms, kkfme
        INTEGER :: iifds, iifde, jjfds, jjfde, kkfds, kkfde

        IF ((.NOT. PRESENT(ims)) .AND. &
            (.NOT. PRESENT(jms)) .AND. &
            (.NOT. PRESENT(kms)) .AND. &
            (.NOT. PRESENT(ime)) .AND. &
            (.NOT. PRESENT(jme)) .AND. &
            (.NOT. PRESENT(kme)) .AND. &
            !
            (.NOT. PRESENT(ips)) .AND. &
            (.NOT. PRESENT(jps)) .AND. &
            (.NOT. PRESENT(kps)) .AND. &
            (.NOT. PRESENT(ipe)) .AND. &
            (.NOT. PRESENT(jpe)) .AND. &
            (.NOT. PRESENT(kpe)) .AND. &
            !
            (.NOT. PRESENT(ifps)) .AND. &
            (.NOT. PRESENT(jfps)) .AND. &
            (.NOT. PRESENT(ifpe)) .AND. &
            (.NOT. PRESENT(jfpe)) .AND. &
            !
            (.NOT. PRESENT(ifms)) .AND. &
            (.NOT. PRESENT(jfms)) .AND. &
            (.NOT. PRESENT(ifme)) .AND. &
            (.NOT. PRESENT(jfme)) .AND. &
            !
            (.NOT. PRESENT(ids)) .AND. &
            (.NOT. PRESENT(jds)) .AND. &
            (.NOT. PRESENT(kds)) .AND. &
            (.NOT. PRESENT(ide)) .AND. &
            (.NOT. PRESENT(jde)) .AND. &
            (.NOT. PRESENT(kde)) .AND. &
            !
            ! (.NOT. PRESENT(p2m_is)) .AND. &
            ! (.NOT. PRESENT(p2m_ie)) .AND. &
            ! (.NOT. PRESENT(p2m_js)) .AND. &
            ! (.NOT. PRESENT(p2m_je)) .AND. &
            !
            (.NOT. PRESENT(m_idim)) .AND. &
            (.NOT. PRESENT(m_jdim)) .AND. &
            (.NOT. PRESENT(m_kdim)) .AND. &
            (.NOT. PRESENT(p_idim)) .AND. &
            (.NOT. PRESENT(p_jdim)) .AND. &
            (.NOT. PRESENT(p_kdim)) .AND. &
            (.NOT. PRESENT(d_idim)) .AND. &
            (.NOT. PRESENT(d_jdim)) .AND. &
            (.NOT. PRESENT(d_kdim))) &
            !
            CALL wrf_debug ( 1, 'get_local_ijk function is NOT requesting a result' )

        CALL get_ijk_from_grid (  grid ,        &
            iids, iide, jjds, jjde, kkds, kkde, &
            iims, iime, jjms, jjme, kkms, kkme, &
            iips, iipe, jjps, jjpe, kkps, kkpe)

        IF (PRESENT(ifps) .OR. &
            PRESENT(jfps) .OR. &
            PRESENT(ifpe) .OR. &
            PRESENT(jfpe) .OR. & 
            PRESENT(ifms) .OR. &
            PRESENT(jfms) .OR. &
            PRESENT(ifme) .OR. &
            PRESENT(jfme)) CALL get_ijk_from_subgrid(grid , &
                                    iifds, iifde, jjfds, jjfde, kkfds, kkfde, &
                                    iifms, iifme, jjfms, jjfme, kkfms, kkfme, &
                                    iifps, iifpe, jjfps, jjfpe, kkfps, kkfpe)
        

        IF (PRESENT(ims)) ims = iims
        IF (PRESENT(jms)) jms = jjms
        IF (PRESENT(kms)) kms = kkms
        IF (PRESENT(ime)) ime = iime
        IF (PRESENT(jme)) jme = jjme
        IF (PRESENT(kme)) kme = kkme

        IF (PRESENT(ips)) ips = iips
        IF (PRESENT(jps)) jps = jjps
        IF (PRESENT(kps)) kps = kkps
        IF (PRESENT(ipe)) ipe = iipe
        IF (PRESENT(jpe)) jpe = jjpe
        IF (PRESENT(kpe)) kpe = kkpe

        IF (PRESENT(ifps)) ifps = iifps
        IF (PRESENT(jfps)) jfps = jjfps
        IF (PRESENT(ifpe)) ifpe = iifpe
        IF (PRESENT(jfpe)) jfpe = jjfpe

        IF (PRESENT(ifms)) ifms = iifms
        IF (PRESENT(jfms)) jfms = jjfms
        IF (PRESENT(ifme)) ifme = iifme
        IF (PRESENT(jfme)) jfme = jjfme

        IF (PRESENT(ids)) ids = iids
        IF (PRESENT(jds)) jds = jjds
        IF (PRESENT(kds)) kds = kkds
        IF (PRESENT(ide)) ide = iide
        IF (PRESENT(jde)) jde = jjde
        IF (PRESENT(kde)) kde = kkde

        IF (PRESENT(m_idim))  m_idim = iime - iims  + 1
        IF (PRESENT(m_jdim))  m_jdim = jjme - jjms  + 1 
        IF (PRESENT(m_kdim))  m_kdim = kkme - kkms  + 1 
        IF (PRESENT(p_idim))  p_idim = iipe - iips  + 1 
        IF (PRESENT(p_jdim))  p_jdim = jjpe - jjps  + 1 
        IF (PRESENT(p_kdim))  p_kdim = kkpe - kkps  + 1 
        IF (PRESENT(d_idim))  d_idim = iide - iids  + 1 
        IF (PRESENT(d_jdim))  d_jdim = jjde - jjds  + 1 
        IF (PRESENT(d_kdim))  d_kdim = kkde - kkds  + 1 

        ! IF (PRESENT(p2m_is)) p2m_is = iips - iims             
        ! IF (PRESENT(p2m_ie)) p2m_ie = iipe - iims
        ! IF (PRESENT(p2m_js)) p2m_js = jjps - jjms             
        ! IF (PRESENT(p2m_je)) p2m_je = jjpe - jjms

        ! p2m_is = ips - ims
        ! p2m_ie = ips - ims + p_idim - 1
        ! p2m_js = jps - jms
        ! p2m_je = jps - jms + p_jdim - 1

    END SUBROUTINE get_local_ijk

!=============================================================
!=============================================================


    

END MODULE module_firebrand_spotting

