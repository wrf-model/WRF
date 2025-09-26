!
! CRTM_SfcOptics_Define
!
! Module defining the CRTM SfcOptics structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       02-Apr-2004
!

MODULE CRTM_SfcOptics_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message,warning
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE CRTM_Parameters      , ONLY: ZERO, ONE, SET, NOT_SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_SfcOptics_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  ! Procedures
  PUBLIC :: CRTM_SfcOptics_Associated
  PUBLIC :: CRTM_SfcOptics_Destroy
  PUBLIC :: CRTM_SfcOptics_Create
  PUBLIC :: CRTM_SfcOptics_Zero
  PUBLIC :: CRTM_SfcOptics_Inspect
  PUBLIC :: CRTM_SfcOptics_DefineVersion
  PUBLIC :: CRTM_SfcOptics_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_SfcOptics_Equal
  END INTERFACE OPERATOR(==)
  
  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_SfcOptics_Add
  END INTERFACE OPERATOR(+)  
  
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_SfcOptics_Subtract
  END INTERFACE OPERATOR(-)  


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_SfcOptics_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'


  ! -----------------------------------
  ! Surface optics data type definition
  ! -----------------------------------
  TYPE :: CRTM_SfcOptics_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Angles = 0 ! I
    INTEGER :: n_Stokes = 0 ! Ls
    ! Flag for SfcOptics computation
    LOGICAL :: Compute = .TRUE.

    ! MW Water SfcOptics options
    LOGICAL  :: Use_New_MWSSEM = .TRUE.    ! Flag for MW Water SfcOptics algorithm switch
    REAL(fp) :: Azimuth_Angle  = 999.9_fp  ! Relative azimuth angle
    REAL(fp) :: Transmittance  = ZERO      ! Total atmospheric transmittance

    ! Index of the satellite view angle in the angle arrays
    INTEGER  :: Index_Sat_Ang = 1
    ! The counter for the m'th component of the Fourier exapnsion of
    ! the radiance for azimuth angle
    INTEGER  :: mth_Azi = 0
    ! The weighted mean surface temperature
    REAL(fp) :: Surface_Temperature = ZERO

    ! The stream angles and weights
    REAL(fp), ALLOCATABLE :: Angle(:)                 ! I
    REAL(fp), ALLOCATABLE :: Weight(:)                ! I
    ! The emissivities and reflectivities
    REAL(fp), ALLOCATABLE :: Emissivity(:,:)          ! I x Ls
    REAL(fp), ALLOCATABLE :: Reflectivity(:,:,:,:)    ! I x Ls x I x Ls
    REAL(fp), ALLOCATABLE :: Direct_Reflectivity(:,:) ! I x Ls
  END TYPE CRTM_SfcOptics_type

  ! Some notes regarding the above definition:
  !
  ! 1) The physical meaning of Reflectivity(:,:,:,:) is the following:
  !
  !    Given a pair of polarization indices, ip and rp, for the incident and
  !    reflected radiances respectively, assuming there are no cross contributions
  !    from incident radiation with different polarization, Reflectivity(:, rp, :, ip)
  !    is defined as a reflectivity matrix with
  !
  !      Reflectivity(:, rp, :, ip) = 0 ;  if rp /= ip
  !
  !    and
  !
  !      I(angle_r, p) = SUM( Reflectivity(angle_r, p, :, p) * I(:, p)), if rp=ip=p
  !
  !    where I(angle_r, p) is the reflected radiance at zenith angle with index angle_r,
  !    and I(:, p) are the incident radiances and the summation is over the number of
  !    incident angles.  Thus, if BRDF(angle_r, p, angle_in, p) is the bidirectional
  !    reflectivity distribution function, then
  !
  !       Reflectivity(angle_r, p, angle_in, p) = &
  !             BRDF(angle_r, p, angle_in, p)*cos(angle_in)*w(angle_in)
  !
  !    where w(angle_in) is the quadrature weight.
  !
  !    A SPECIAL CASE
  !    --------------
  !    For a Lambertian surface, if only one angle is given, then
  !
  !        I_r = Reflectivity(1, rp, 1, ip) * I_diff
  !
  !    where I_r is the reflected radiance, constant at all angles, I_diff
  !    is the incident radiance at the diffusivity angle.
  !
  !
  ! 2) Regarding the Direct_Reflectivity(:,:) component,
  !
  !    If I(angle_r, p) is the reflected radiance at the zenith angle with index
  !    angle_r and F_direct(angle_in) is the direct incident irradiance at the surface,
  !    then Direct_Reflectivity(angle_r, p) is defined as
  !
  !      I(angle_r, p) = Direct_Reflectivity(angle_r, p) * cos(angle_in) * F_direct(angle_in)
  !


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM SfcOptics object.
!
! CALLING SEQUENCE:
!       Status = CRTM_SfcOptics_Associated( SfcOptics )
!
! OBJECTS:
!       SfcOptics:   SfcOptics structure which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SfcOptics_type
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating the
!                    status of the SfcOptics members.
!                      .TRUE.  - if the array components are allocated.
!                      .FALSE. - if the array components are not allocated.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input SfcOptics argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Associated( self ) RESULT( Status )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION CRTM_SfcOptics_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM SfcOptics objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Destroy( SfcOptics )
!
! OBJECTS:
!       SfcOptics:    Re-initialized SfcOptics structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SfcOptics_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SfcOptics_Destroy( self )
    TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_SfcOptics_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM SfcOptics object.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Create( SfcOptics, n_Angles, n_Stokes )
!
! OBJECTS:
!       SfcOptics:    SfcOptics structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SfcOptics_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Angles:     Number of angles for which there is SfcOptics data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Conformable with SfcOptics object
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Stokes:     Number of Stokes components for which there is SfcOptics
!                     data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Conformable with SfcOptics object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SfcOptics_Create( &
    self, &
    n_Angles , &
    n_Stokes   )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: self
    INTEGER,                   INTENT(IN)  :: n_Angles
    INTEGER,                   INTENT(IN)  :: n_Stokes
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Angles < 1 .OR. n_Stokes < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( self%Angle( n_Angles ), &
              self%Weight( n_Angles ), &
              self%Emissivity( n_Angles, n_Stokes ), &
              self%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
              self%Direct_Reflectivity( n_Angles, n_Stokes ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    self%n_Angles = n_Angles
    self%n_Stokes = n_Stokes
    ! ...Arrays
    self%Angle               = ZERO
    self%Weight              = ZERO
    self%Emissivity          = ZERO
    self%Reflectivity        = ZERO
    self%Direct_Reflectivity = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_SfcOptics_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Zero
!
! PURPOSE:
!       Elemental subroutine to initialise the components of an SfcOptics
!       object to a value of zero.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Zero( SfcOptics )
!
! OBJECTS:
!       SfcOptics:   SfcOptics object which is to have its components
!                    set to a zero value.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SfcOptics_type
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_SfcOptics_Zero( self )
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: self
    self%Azimuth_Angle       = 999.9_fp
    self%Transmittance       = ZERO
    self%Surface_Temperature = ZERO
    IF ( .NOT. CRTM_SfcOptics_Associated( self ) ) RETURN
    self%Emissivity          = ZERO
    self%Reflectivity        = ZERO
    self%Direct_Reflectivity = ZERO
  END SUBROUTINE CRTM_SfcOptics_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM SfcOptics object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_Inspect( SfcOptics )
!
! INPUTS:
!       SfcOptics:     CRTM SfcOptics object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SfcOptics_Inspect( self )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: self

    WRITE(*, '(1x,"SfcOptics OBJECT")')
    ! Dimensions
    WRITE(*, '(3x,"n_Angles   :",1x,i0)') self%n_Angles
    WRITE(*, '(3x,"n_Stokes   :",1x,i0)') self%n_Stokes
    ! Display components
    WRITE(*, '(3x,"Compute flag              :",1x,l1)') self%Compute
    WRITE(*, '(3x,"Use_New_MWSSEM flag       :",1x,l1)') self%Use_New_MWSSEM
    WRITE(*, '(3x,"  MWSSEM- azimuth angle   :",1x,es13.6)') self%Azimuth_Angle
    WRITE(*, '(3x,"  MWSSEM- transmittance   :",1x,es13.6)') self%Transmittance
    WRITE(*, '(3x,"Satellite view angle index:",1x,i0)') self%Index_Sat_Ang
    WRITE(*, '(3x,"Azimuth Fourier component :",1x,i0)') self%mth_Azi
    WRITE(*, '(3x,"Weighted mean Tsfc        :",1x,es13.6)') self%Surface_Temperature
    IF ( .NOT. CRTM_SfcOptics_Associated(self) ) RETURN
    WRITE(*, '(3x,"Angle :")')
    WRITE(*, '(5(1x,es13.6,:))') self%Angle
    WRITE(*, '(3x,"Weight :")')
    WRITE(*, '(5(1x,es13.6,:))') self%Weight
    WRITE(*, '(3x,"Emissivity :")')
    WRITE(*, '(5(1x,es13.6,:))') self%Emissivity
    WRITE(*, '(3x,"Reflectivity :")')
    WRITE(*, '(5(1x,es13.6,:))') self%Reflectivity
    WRITE(*, '(3x,"Direct_Reflectivity :")')
    WRITE(*, '(5(1x,es13.6,:))') self%Direct_Reflectivity
  END SUBROUTINE CRTM_SfcOptics_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SfcOptics_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_SfcOptics_DefineVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SfcOptics_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_SfcOptics_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_SfcOptics_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_SfcOptics objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_SfcOptics_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM SfcOptics objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figure to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: x, y
    INTEGER,         OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( (.NOT. CRTM_SfcOptics_Associated(x)) .OR. &
         (.NOT. CRTM_SfcOptics_Associated(y)) ) RETURN

    ! Check dimensions
    IF ( (x%n_Angles /= y%n_Angles) .OR. &
         (x%n_Stokes /= y%n_Stokes) ) RETURN

    ! Check scalars
    ! ...Logicals
    IF ( (x%Compute        .NEQV. y%Compute       ) .OR. &
         (x%Use_New_MWSSEM .NEQV. y%Use_New_MWSSEM) ) RETURN
    ! ...Other types
    IF ( (.NOT. Compares_Within_Tolerance(x%Azimuth_Angle,y%Azimuth_Angle,n)) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Transmittance,y%Transmittance,n)) .OR. &
         (x%Index_Sat_Ang /= y%Index_Sat_Ang) .OR. &
         (x%mth_Azi       /= y%mth_Azi      ) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Surface_Temperature,y%Surface_Temperature,n)) ) RETURN
    
    ! Check arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Angle              ,y%Angle              ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Weight             ,y%Weight             ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Emissivity         ,y%Emissivity         ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Reflectivity       ,y%Reflectivity       ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Direct_Reflectivity,y%Direct_Reflectivity,n))) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_SfcOptics_Compare


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SfcOptics_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_SfcOptics objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_SfcOptics_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM SfcOptics objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( CRTM_SfcOptics_Associated(x) .NEQV. CRTM_SfcOptics_Associated(y) ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_Angles /= y%n_Angles) .OR. &
         (x%n_Stokes /= y%n_Stokes) ) RETURN
    ! ...Scalars
    IF ( .NOT. ((x%Compute               .EQV.   y%Compute            ) .AND. &
                (x%Use_New_MWSSEM        .EQV.   y%Use_New_MWSSEM     ) .AND. &
                (x%Azimuth_Angle       .EqualTo. y%Azimuth_Angle      ) .AND. &
                (x%Transmittance       .EqualTo. y%Transmittance      ) .AND. &
                (x%Index_Sat_Ang          ==     y%Index_Sat_Ang      ) .AND. &
                (x%mth_Azi                ==     y%mth_Azi            ) .AND. &
                (x%Surface_Temperature .EqualTo. y%Surface_Temperature)) ) RETURN
    ! ...Arrays
    IF ( CRTM_SfcOptics_Associated(x) .AND. CRTM_SfcOptics_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Angle               .EqualTo. y%Angle              ) .AND. &
                  ALL(x%Weight              .EqualTo. y%Weight             ) .AND. &
                  ALL(x%Emissivity          .EqualTo. y%Emissivity         ) .AND. &
                  ALL(x%Reflectivity        .EqualTo. y%Reflectivity       ) .AND. &
                  ALL(x%Direct_Reflectivity .EqualTo. y%Direct_Reflectivity)) ) RETURN
    END IF


    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION CRTM_SfcOptics_Equal

  
!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SfcOptics_Add
!
! PURPOSE:
!       Pure function to add two CRTM_SfcOptics objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       sosum = CRTM_SfcOptics_Add( so1, so2 )
!
!         or
!
!       sosum = so1 + so2
!
! INPUTS:
!       so1, so2:      Two CRTM SfcOptics objects to be added.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       sosum:         SfcOptics object containing the added components.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Add( so1, so2 ) RESULT( sosum )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: so1, so2
    TYPE(CRTM_SfcOptics_type) :: sosum

    ! Check the structure association status
    IF ( (.NOT. CRTM_SfcOptics_Associated(so1)) .OR. &
         (.NOT. CRTM_SfcOptics_Associated(so2))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (so1%n_Angles /= so2%n_Angles) .OR. &
         (so1%n_Stokes /= so2%n_Stokes) ) RETURN

    ! Copy the first structure
    sosum = so1
    
    ! And add its components to the second one
    ! ...The scalar values
    sosum%Transmittance       = sosum%Transmittance       + so2%Transmittance
    sosum%Surface_Temperature = sosum%Surface_Temperature + so2%Surface_Temperature
    ! ...The arrays
    sosum%Reflectivity        = sosum%Reflectivity        + so2%Reflectivity
    sosum%Direct_Reflectivity = sosum%Direct_Reflectivity + so2%Direct_Reflectivity
    sosum%Emissivity          = sosum%Emissivity          + so2%Emissivity    

  END FUNCTION CRTM_SfcOptics_Add


  
!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_SfcOptics_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM_SfcOptics objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       sodiff = CRTM_SfcOptics_Subtract( so1, so2 )
!
!         or
!
!       sodiff = so1 - so2
!
! INPUTS:
!       so1, so2:      Two CRTM SfcOptics objects to be subtracted.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       sodiff:        SfcOptics object containing the differenced components.
!                      UNITS:      N/A
!                      TYPE:       CRTM_SfcOptics_type
!                      DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_SfcOptics_Subtract( so1, so2 ) RESULT( sodiff )
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: so1, so2
    TYPE(CRTM_SfcOptics_type) :: sodiff

    ! Check the structure association status
    IF ( (.NOT. CRTM_SfcOptics_Associated(so1)) .OR. &
         (.NOT. CRTM_SfcOptics_Associated(so2))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (so1%n_Angles /= so2%n_Angles) .OR. &
         (so1%n_Stokes /= so2%n_Stokes) ) RETURN

    ! Copy the first structure
    sodiff = so1
    
    ! And subtract the second one from it
    ! ...The scalar values
    sodiff%Transmittance       = sodiff%Transmittance       - so2%Transmittance
    sodiff%Surface_Temperature = sodiff%Surface_Temperature - so2%Surface_Temperature
    ! ...The arrays
    sodiff%Reflectivity        = sodiff%Reflectivity        - so2%Reflectivity
    sodiff%Direct_Reflectivity = sodiff%Direct_Reflectivity - so2%Direct_Reflectivity
    sodiff%Emissivity          = sodiff%Emissivity          - so2%Emissivity    

  END FUNCTION CRTM_SfcOptics_Subtract

END MODULE CRTM_SfcOptics_Define
