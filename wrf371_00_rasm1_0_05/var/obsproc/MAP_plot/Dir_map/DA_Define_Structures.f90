MODULE DA_Define_Structures

!------------------------------------------------------------------------------
!  PURPOSE: Collection of routines to define and allocate structures.
!
!  METHOD:  Varied.
!
!  HISTORY: 01/06/2000 - Creation of F90 version.           Dale Barker
!
!------------------------------------------------------------------------------

   USE DA_Constants

   IMPLICIT NONE
   
!-----------------------------------------------------------------------------
!  [1.0] MM5 model structure definition:
!-----------------------------------------------------------------------------

   type big_record_header
      integer(kind=4)                   :: header_flag
      integer(kind=4), dimension(50,20) :: bhi
      real(kind=4),    dimension(20,20) :: bhr
      character(80),   dimension(50,20) :: bhic
      character(80),   dimension(20,20) :: bhrc
   end type big_record_header

   type sub_record_header
      integer(kind=4)   :: ndim

      integer(kind=4), dimension(4) :: start_index, end_index

      real(kind=4)      :: current_time

      character(len= 4) :: staggering, ordering
      character(len= 8) :: char_date
      character(len= 9) :: name
      character(len=24) :: current_date
      character(len=25) :: units
      character(len=46) :: description
   end type sub_record_header

   TYPE mm5_model_type
      integer :: num_of_var

      TYPE (big_record_header)                             :: big_header

      TYPE (sub_record_header), dimension(max_num_of_var)  :: sub_header

!     Variables held on grid:
      REAL, POINTER    :: u(:,:,:)
      REAL, POINTER    :: v(:,:,:)
      REAL, POINTER    :: t(:,:,:)
      REAL, POINTER    :: q(:,:,:)
      REAL, POINTER    :: pp_c(:,:,:)
      REAL, POINTER    :: w(:,:,:)

      REAL, POINTER    :: psi(:,:,:)
      REAL, POINTER    :: chi(:,:,:)
      REAL, POINTER    :: blnc_p(:,:,:)

      REAL, POINTER    :: psac(:,:)
      REAL, POINTER    :: cori(:,:)
      REAL, POINTER    :: latc(:,:)
      REAL, POINTER    :: lonc(:,:)
      REAL, POINTER    :: latd(:,:)
      REAL, POINTER    :: lond(:,:)
      REAL, POINTER    :: msfc(:,:)
      REAL, POINTER    :: msfd(:,:)
      REAL, POINTER    :: tgrn(:,:)
      REAL, POINTER    :: terr(:,:)
      REAL, POINTER    :: lanu(:,:)
      REAL, POINTER    :: snow(:,:)
      REAL, POINTER    :: reference_pressure(:)
      REAL, POINTER    :: sigmah(:)
      REAL, POINTER    :: psad(:,:)

      REAL, POINTER    ::      t_eval(:)
      REAL, POINTER    ::      q_eval(:)
      REAL, POINTER    ::    psi_eval(:)
      REAL, POINTER    ::    chi_eval(:)
      REAL, POINTER    :: blnc_p_eval(:)

      REAL, POINTER    ::      t_evec(:,:)
      REAL, POINTER    ::      q_evec(:,:)
      REAL, POINTER    ::    psi_evec(:,:)
      REAL, POINTER    ::    chi_evec(:,:)
      REAL, POINTER    :: blnc_p_evec(:,:)
   END TYPE mm5_model_type

   TYPE xb_type
      INTEGER          :: map
      REAL             :: ptop
      REAL             :: ps0
      REAL             :: ts0
      REAL             :: tlp
      REAL             :: t_forecast
      REAL             :: ds
      INTEGER          :: mix
      INTEGER          :: mjx
      INTEGER          :: mkx
      INTEGER          :: ntotlev

!     Header
      integer :: num_of_var

      TYPE (big_record_header)                             :: big_header

      TYPE (sub_record_header), dimension(max_num_of_var)  :: sub_header

!     Grid variables
      REAL, POINTER    :: inv_grid_box_area_c(:,:)
      REAL, POINTER    :: inv_grid_box_area_d(:,:)
      REAL, POINTER    :: rf_scale_factor_c(:,:)
      REAL, POINTER    :: rf_scale_factor_d(:,:)
      REAL, POINTER    :: map_factor_c(:,:)
      REAL, POINTER    :: map_factor_d(:,:)

!     FFT variables:
      INTEGER, POINTER :: fft_factors_y(:)   ! FFT factors in y direction.
      INTEGER, POINTER :: fft_factors_x(:)   ! FFT factors in x direction.
      REAL, POINTER    :: trig_functs_y(:)   ! Trig functions in y direction.
      REAL, POINTER    :: trig_functs_x(:)   ! Trig functions in x direction.

!     Variables held on grid:
      REAL, POINTER    :: u(:,:,:)
      REAL, POINTER    :: v(:,:,:)
      REAL, POINTER    :: t(:,:,:)
      REAL, POINTER    :: q(:,:,:)
      REAL, POINTER    :: p_c(:,:,:)
      REAL, POINTER    :: p_d(:,:,:)
      REAL, POINTER    :: h_c(:,:,:)
      REAL, POINTER    :: h_d(:,:,:)
      REAL, POINTER    :: rho(:,:,:)

      REAL, POINTER    :: cori(:,:)
      REAL, POINTER    :: tgrn(:,:)
      REAL, POINTER    :: latc(:,:)
      REAL, POINTER    :: lonc(:,:)
      REAL, POINTER    :: latd(:,:)
      REAL, POINTER    :: lond(:,:)
      REAL, POINTER    :: terr(:,:)
      REAL, POINTER    :: psac(:,:)
      REAL, POINTER    :: lanu(:,:)
!     REAL, POINTER    :: landmask(:,:)
      REAL, POINTER    :: sigmah(:)
      REAL, POINTER    :: reference_pressure(:)
   END TYPE xb_type

!-----------------------------------------------------------------------------
!  [2.0] Observation structure definition:
!-----------------------------------------------------------------------------

!---------------------------------------------------------------------------

   TYPE crs_or_dot_type

      REAL                   :: crs             ! Field defined at cross point
      REAL                   :: dot             ! Field defined at dot point

   END TYPE crs_or_dot_type

!---------------------------------------------------------------------------

   TYPE field_type

      REAL                   :: data             !  Innovation vector
      INTEGER                :: qc              !  Observation QC
      REAL                   :: error           !  Observational error

   END TYPE field_type

!---------------------------------------------------------------------------

   TYPE model_loc_type

      TYPE (crs_or_dot_type)  :: yi
      TYPE (crs_or_dot_type)  :: xj
      TYPE (field_type)       :: slp            ! Pressure in Pa
      TYPE (field_type)       :: pw             ! Precipitable water vapor in cm
      TYPE (field_type)       :: speed          ! Wind speed in m/s
      TYPE (field_type)       :: tpw            ! Precipitable water vapor in cm
      TYPE (field_type)       :: tb19v          ! Brightness Temperature in K
      TYPE (field_type)       :: tb19h          ! Brightness Temperature in K
      TYPE (field_type)       :: tb22v          ! Brightness Temperature in K
      TYPE (field_type)       :: tb37v          ! Brightness Temperature in K
      TYPE (field_type)       :: tb37h          ! Brightness Temperature in K
      TYPE (field_type)       :: tb85v          ! Brightness Temperature in K
      TYPE (field_type)       :: tb85h          ! Brightness Temperature in K

   END TYPE model_loc_type

!-------------------------------------------------------------------------

!  Old format as of May 15, 2000

   TYPE each_level_type_old

      REAL                    :: height         ! Height in m
      REAL                    :: height_qc      ! Height QC
      TYPE (field_type)       :: h              ! Wind x-component in m/s
      TYPE (field_type)       :: u              ! Wind x-component in m/s
      TYPE (field_type)       :: v              ! Wind y-component in m/s
      TYPE (field_type)       :: p              ! Pressure in Pa
      TYPE (field_type)       :: t              ! Temperature in K
      TYPE (field_type )      :: q              ! Mixing ration kg/kg
      TYPE (crs_or_dot_type)  :: zk             ! MM5 k-coordinates

   END TYPE each_level_type_old

!-------------------------------------------------------------------------

!  format as of February 20, 2000

   TYPE each_level_type

      TYPE (field_type)       :: pressure       ! Pressure in Pa
      TYPE (field_type)       :: speed          ! Wind spped in m/s
      TYPE (field_type)       :: direction      ! Wind dirction in degree
      TYPE (field_type)       :: height         ! Height in m
      TYPE (field_type)       :: temperature    ! Temperature in K
      TYPE (field_type)       :: dew_point      ! Dew_point in K
      TYPE (field_type)       :: rh             ! Relative Humidity

   END TYPE each_level_type


!-------------------------------------------------------------------------

   TYPE info_type

        CHARACTER (LEN = 30)   :: name          ! Station name
        CHARACTER (LEN = 12)   :: platform      ! Instrument platform
        CHARACTER (LEN =  5)   :: id            ! 5 digit station identifer
        CHARACTER (LEN = 19)   :: date_char     ! CCYY-MM-DD_HH:MM:SS date
        INTEGER                :: levels        ! number of levels
        REAL                   :: lat           ! Latitude in degree
        REAL                   :: lon           ! Longitude in degree
        REAL                   :: elv           ! Elevation in m
        TYPE (crs_or_dot_type) :: pstar         ! Surface pressure

   END TYPE info_type

!-------------------------------------------------------------------------

   TYPE platform_type

        TYPE (info_type)                        :: info
        TYPE (model_loc_type)                   :: loc
        TYPE (each_level_type), &
             DIMENSION (max_levels)             :: each

   END TYPE platform_type

!-------------------------------------------------------------------------

   TYPE multi_level_type
        TYPE (info_type)                        :: info
        TYPE (model_loc_type)                   :: loc
        TYPE (each_level_type), &
              DIMENSION (max_levels)            :: each
   END TYPE multi_level_type

!-------------------------------------------------------------------------

   TYPE single_level_type
        TYPE (info_type)                        :: info
        TYPE (model_loc_type)                   :: loc
        TYPE (each_level_type)                  :: each
   END TYPE single_level_type

!-------------------------------------------------------------------------

   TYPE ob_type

        INTEGER :: total_obs, num_synop, &
                   num_sound, num_satob, &
                   num_pilot, num_satem, &
                   num_airep, num_metar, &
                   num_ships, num_gpspw, &
                   num_ssmt1, num_ssmt2, &
                   num_amdar,            &
                   num_ssmi,  num_tovs , &
                   num_qscat, num_profl, &
                   num_buoys, num_gpsref,&
                   num_bogus, num_airsret

        TYPE (multi_level_type),  DIMENSION (max_sound)   :: sound
        TYPE (multi_level_type),  DIMENSION (max_bogus)   :: bogus
        TYPE (multi_level_type),  DIMENSION (max_airep)   :: airep
        TYPE (multi_level_type),  DIMENSION (max_gpsref)  :: gpsref
        TYPE (multi_level_type),  DIMENSION (max_tovs)    :: tovs
        TYPE (multi_level_type),  DIMENSION (max_airsret) :: airsret

        TYPE (single_level_type), DIMENSION (max_satem)   :: satem
        TYPE (single_level_type), DIMENSION (max_pilot)   :: pilot
        TYPE (single_level_type), DIMENSION (max_amdar)   :: amdar
        TYPE (single_level_type), DIMENSION (max_synop)   :: synop
        TYPE (single_level_type), DIMENSION (max_satob)   :: satob
        TYPE (single_level_type), DIMENSION (max_metar)   :: metar
        TYPE (single_level_type), DIMENSION (max_gpspw)   :: gpspw
        TYPE (single_level_type), DIMENSION (max_ships)   :: ships
        TYPE (single_level_type), DIMENSION (max_ssmt1)   :: ssmt1
        TYPE (single_level_type), DIMENSION (max_ssmt2)   :: ssmt2
        TYPE (single_level_type), DIMENSION (max_ssmi)    :: ssmi
        TYPE (single_level_type), DIMENSION (max_qscat)   :: qscat
        TYPE (single_level_type), DIMENSION (max_profl)   :: profl
        TYPE (single_level_type), DIMENSION (max_buoys)   :: buoys

        REAL :: missing
        REAL :: ptop

   END TYPE ob_type

! Residual type:

   TYPE residual_type
        REAL                         :: u        ! Wind x-component in m/s
        REAL                         :: v        ! Wind y-component in m/s
        REAL                         :: p        ! Pressure in Pa
        REAL                         :: t        ! Temperature in K
        REAL                         :: q        ! Mixing ration kg/kg
        REAL                         :: tpw      ! Toatl precipitable water cm
   END TYPE residual_type

   TYPE multi_re_type
        TYPE (residual_type), &
              DIMENSION (max_levels) :: each
        INTEGER                      :: levels   ! number of levels
   END TYPE multi_re_type

   TYPE y_type
        INTEGER :: total_obs, num_synop, &
                   num_sound, num_satob, &
                   num_pilot, num_satem, &
                   num_airep, num_metar, &
                   num_ships, num_gpspw, &
                   num_ssmt1, num_ssmt2, &
                   num_ssmi,  num_tovs

        TYPE (multi_re_type), DIMENSION (max_sound)   :: sound
        TYPE (multi_re_type), DIMENSION (max_airep)   :: airep
        TYPE (residual_type), DIMENSION (max_satem)   :: satem
        TYPE (residual_type), DIMENSION (max_pilot)   :: pilot
        TYPE (residual_type), DIMENSION (max_synop)   :: synop
        TYPE (residual_type), DIMENSION (max_satob)   :: satob
        TYPE (residual_type), DIMENSION (max_metar)   :: metar
        TYPE (residual_type), DIMENSION (max_ships)   :: ships
        TYPE (residual_type), DIMENSION (max_gpspw)   :: gpspw
        TYPE (residual_type), DIMENSION (max_ssmt1)   :: ssmt1
        TYPE (residual_type), DIMENSION (max_ssmt2)   :: ssmt2
        TYPE (residual_type), DIMENSION (max_ssmi)    :: ssmi
        TYPE (residual_type), DIMENSION (max_tovs)    :: tovs
   END TYPE y_type

   TYPE stats_type
        TYPE (residual_type) :: maximum, minimum, average, rms_err
   END TYPE stats_type

!-----------------------------------------------------------------------------
!  [3.0] Control variable structure:
!-----------------------------------------------------------------------------

   TYPE cv_type
      INTEGER          :: option             ! 1 = use u, v,t,q,p, 2 = use psi, chi etc.
      INTEGER          :: size               ! Total size of control variable.
      INTEGER          :: size1              ! Size of CV array of 1st variable error.
      INTEGER          :: size2              ! Size of CV array of 2nd variable error.
      INTEGER          :: size3              ! Size of CV array of 3rd variable error.
      INTEGER          :: size4              ! Size of CV array of 4th variable error.
      INTEGER          :: size5              ! Size of CV array of 5th variable error.
      REAL, POINTER    :: val(:)
   END TYPE cv_type

   TYPE be_subtype
      INTEGER           :: mz                ! Vertical truncation of errors.
      INTEGER           :: stagger             ! 1=Cross point, 2 = Dot point.
      CHARACTER*5       :: name                ! Variable name.
      REAL, POINTER     :: val(:,:,:)          ! Gridpoint standard deviation.
      REAL, POINTER     :: eigenvec(:,:)       ! Vertical eigenvectors.
      REAL, POINTER     :: eigenval(:)         ! Vertical eigenvalues.
   END TYPE be_subtype

   TYPE be_type
!     Grid point errors:
      INTEGER           :: miy
      INTEGER           :: mjx
      TYPE (be_subtype) :: v1
      TYPE (be_subtype) :: v2
      TYPE (be_subtype) :: v3
      TYPE (be_subtype) :: v4
      TYPE (be_subtype) :: v5

!     Control variable space errors:
      TYPE (cv_type)    :: cv
   END TYPE be_type

   TYPE vp_type
      REAL, POINTER    :: v1(:,:,:)
      REAL, POINTER    :: v2(:,:,:)
      REAL, POINTER    :: v3(:,:,:)
      REAL, POINTER    :: v4(:,:,:)
      REAL, POINTER    :: v5(:,:,:)
   END TYPE vp_type

!-----------------------------------------------------------------------------
!  [4.0] Analysis increment structure:
!-----------------------------------------------------------------------------

   TYPE x_type
      REAL, POINTER    :: u(:,:,:)
      REAL, POINTER    :: v(:,:,:)
      REAL, POINTER    :: t(:,:,:)
      REAL, POINTER    :: q(:,:,:)
      REAL, POINTER    :: p(:,:,:)
      REAL, POINTER    :: rho(:,:,:)
      REAL, POINTER    :: tgrn(:,:)
   END TYPE x_type

! To declare the ob here to avoid ob as an argument to be passed.
! In IBM sp2 (blackforest), it has difficulty to use ob as argument
! in DA_Setup_Obs_Structures, etc., in running time.

   TYPE(ob_type) :: ob

END MODULE DA_Define_Structures
