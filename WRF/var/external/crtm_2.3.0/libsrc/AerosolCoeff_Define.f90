!
! AerosolCoeff_Define
!
! Module defining the AerosolCoeff data structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-Feb-2005
!                       paul.vandelst@noaa.gov
!       Modified by:    Quanhua Liu, QSS Group, Inc;  quanhua.liu@noaa.gov
!                       David Groff, SAIC;            david.groff@noaa.gov
!

MODULE AerosolCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds               , ONLY: fp, Long, Double
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers    , ONLY: OPERATOR(.EqualTo.)
  USE Spectral_Units_Conversion, ONLY: micron_to_inverse_cm
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: AerosolCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: AerosolCoeff_Associated
  PUBLIC :: AerosolCoeff_Destroy
  PUBLIC :: AerosolCoeff_Create
  PUBLIC :: AerosolCoeff_Inspect
  PUBLIC :: AerosolCoeff_ValidRelease
  PUBLIC :: AerosolCoeff_Info
  PUBLIC :: AerosolCoeff_Frequency
  PUBLIC :: AerosolCoeff_DefineVersion
  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE AerosolCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: AerosolCoeff_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: AEROSOLCOEFF_RELEASE = 3  ! This determines structure and file formats.
  INTEGER, PARAMETER :: AEROSOLCOEFF_VERSION = 1  ! This is just the data version for the release.
  ! String lengths
  INTEGER, PARAMETER :: SL = 80
  INTEGER, PARAMETER :: ML = 256
  ! Literals
  REAL(Double), PARAMETER :: ZERO = 0.0_Double


  ! ---------------------------------
  ! AerosolCoeff data type definition
  ! --------------------------------
  !:tdoc+:
  TYPE :: AerosolCoeff_type
    ! Release and version information
    INTEGER(Long) :: Release = AEROSOLCOEFF_RELEASE
    INTEGER(Long) :: Version = AEROSOLCOEFF_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Data source
    CHARACTER(SL) :: Data_Source = ''
    ! Array dimensions
    INTEGER(Long) :: n_Wavelengths      = 0   ! I1 dimension
    INTEGER(Long) :: n_Radii            = 0   ! I2 dimension
    INTEGER(Long) :: n_Types            = 0   ! I3 dimension
    INTEGER(Long) :: n_RH               = 0   ! I4 dimension
    INTEGER(Long) :: Max_Legendre_Terms = 0   ! I5 dimension
    INTEGER(Long) :: n_Legendre_Terms   = 0
    INTEGER(Long) :: Max_Phase_Elements = 0   ! I6 dimension
    INTEGER(Long) :: n_Phase_Elements   = 0   
    ! LUT dimension vectors
    INTEGER(Long), ALLOCATABLE :: Type(:)            ! I3
    CHARACTER(SL), ALLOCATABLE :: Type_Name(:)       ! I3
    REAL(Double),  ALLOCATABLE :: Wavelength(:)      ! I1
    REAL(Double),  ALLOCATABLE :: Frequency(:)       ! I1
    REAL(Double),  ALLOCATABLE :: Reff(:,:)          ! I2 x I3
    REAL(Double),  ALLOCATABLE :: RH(:)              ! I4
    ! LUT data
    REAL(Double),  ALLOCATABLE :: ke(:,:,:)          ! I1 x I2 x I3
    REAL(Double),  ALLOCATABLE :: w(:,:,:)           ! I1 x I2 x I3 
    REAL(Double),  ALLOCATABLE :: g(:,:,:)           ! I1 x I2 x I3
    REAL(Double),  ALLOCATABLE :: pcoeff(:,:,:,:,:)  ! I1 x I2 x I3 x I5 x I6
  END TYPE AerosolCoeff_type
  !:tdoc-:


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
!       AerosolCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a AerosolCoeff object.
!
! CALLING SEQUENCE:
!       Status = AerosolCoeff_Associated( AerosolCoeff )
!
! OBJECTS:
!       AerosolCoeff:  AerosolCoeff object which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AerosolCoeff_type)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the AerosolCoeff members.
!                      .TRUE.  - if ANY of the AerosolCoeff allocatable or
!                                pointer members are in use.
!                      .FALSE. - if ALL of the AerosolCoeff allocatable or
!                                pointer members are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input AerosolCoeff argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION AerosolCoeff_Associated( AerosolCoeff ) RESULT( Status )
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
    LOGICAL :: Status
    Status = AerosolCoeff%Is_Allocated
  END FUNCTION AerosolCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize AerosolCoeff objects.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_Destroy( AerosolCoeff )
!
! OBJECTS:
!       AerosolCoeff:   Re-initialized AerosolCoeff object.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
!                       DIMENSION:  Scalar OR any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AerosolCoeff_Destroy( AerosolCoeff )
    TYPE(AerosolCoeff_type), INTENT(OUT) :: AerosolCoeff
    AerosolCoeff%Is_Allocated = .FALSE.
    AerosolCoeff%n_Wavelengths      = 0
    AerosolCoeff%n_Radii            = 0
    AerosolCoeff%n_Types            = 0
    AerosolCoeff%n_RH               = 0
    AerosolCoeff%Max_Legendre_Terms = 0
    AerosolCoeff%n_Legendre_Terms   = 0
    AerosolCoeff%Max_Phase_Elements = 0
    AerosolCoeff%n_Phase_Elements   = 0
  END SUBROUTINE AerosolCoeff_Destroy
  

!--------------------------------------------------------------------------------
!
! NAME:
!       AerosolCoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of a AerosolCoeff object.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_Create( AerosolCoeff    , &
!                                 n_Wavelengths   , &
!                                 n_Radii         , &
!                                 n_Types         , &
!                                 n_RH            , &
!                                 n_Legendre_Terms, &
!                                 n_Phase_Elements  )
!
! OBJECTS:
!       AerosolCoeff:      AerosolCoeff object.
!                          UNITS:      N/A
!                          TYPE:       TYPE(AerosolCoeff_type)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Wavelengths:     The number of wavelengths in the look-up
!                          table (LUT) 
!                          The "I1" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Radii:           The number of discrete effective radii for
!                          scatterers in the LUT.
!                          The "I2" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Types:           The number of different aerosol types in
!                          the LUT.
!                          The "I3" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_RH:              The number of relative humidity entries in
!                          the LUT.
!                          The "I4" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT.
!                          The "I5" dimension. Can be = 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          The "I6" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AerosolCoeff_Create( &
    AerosolCoeff    , &
    n_Wavelengths   , &
    n_Radii         , &
    n_Types         , &
    n_RH            , &
    n_Legendre_Terms, &
    n_Phase_Elements  )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(OUT) :: AerosolCoeff
    INTEGER,                 INTENT(IN)  :: n_Wavelengths   
    INTEGER,                 INTENT(IN)  :: n_Radii         
    INTEGER,                 INTENT(IN)  :: n_Types         
    INTEGER,                 INTENT(IN)  :: n_RH            
    INTEGER,                 INTENT(IN)  :: n_Legendre_Terms
    INTEGER,                 INTENT(IN)  :: n_Phase_Elements
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_Create'
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Wavelengths    < 1 .OR. &
         n_Radii          < 1 .OR. &
         n_Types          < 1 .OR. &
         n_RH             < 1 .OR. &
         n_Legendre_Terms < 0 .OR. &
         n_Phase_Elements < 1      ) RETURN


    ! Perform the allocations.
    ALLOCATE( AerosolCoeff%Type( n_Types ), &
              AerosolCoeff%Type_Name( n_Types ), &
              AerosolCoeff%Wavelength( n_Wavelengths ), &
              AerosolCoeff%Frequency(  n_Wavelengths ), &
              AerosolCoeff%Reff( n_Radii, n_Types ), &
              AerosolCoeff%RH( n_RH ), &
              AerosolCoeff%ke( n_Wavelengths, n_Radii, n_Types ), &
              AerosolCoeff%w(  n_Wavelengths, n_Radii, n_Types ), &
              AerosolCoeff%g(  n_Wavelengths, n_Radii, n_Types ), &
              AerosolCoeff%pcoeff( n_Wavelengths     , &
                                   n_Radii           , &
                                   n_Types           , &
                                   0:n_Legendre_Terms, &
                                   n_Phase_Elements    ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    AerosolCoeff%n_Types            = n_Types 
    AerosolCoeff%n_Wavelengths      = n_Wavelengths 
    AerosolCoeff%n_Radii            = n_Radii 
    AerosolCoeff%n_RH               = n_RH 
    AerosolCoeff%Max_Legendre_Terms = n_Legendre_Terms
    AerosolCoeff%n_Legendre_Terms   = n_Legendre_Terms
    AerosolCoeff%Max_Phase_Elements = n_Phase_Elements
    AerosolCoeff%n_Phase_Elements   = n_Phase_Elements 
    ! ...Arrays
    AerosolCoeff%Type       = 0
    AerosolCoeff%Type_Name  = ''
    AerosolCoeff%Wavelength = ZERO
    AerosolCoeff%Frequency  = ZERO
    AerosolCoeff%Reff       = ZERO
    AerosolCoeff%RH         = ZERO
    AerosolCoeff%ke         = ZERO
    AerosolCoeff%w          = ZERO
    AerosolCoeff%g          = ZERO
    AerosolCoeff%pcoeff     = ZERO


    ! Set allocationindicator
    AerosolCoeff%Is_Allocated = .TRUE.

  END SUBROUTINE AerosolCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a AerosolCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_Inspect( AerosolCoeff )
!
! INPUTS:
!       AerosolCoeff:  AerosolCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AerosolCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AerosolCoeff_Inspect( AerosolCoeff )
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
    INTEGER :: i
    WRITE(*,'(1x,"AerosolCoeff OBJECT")')
    WRITE(*,'(3x,"Data source      :",1x,a )') TRIM(AerosolCoeff%Data_Source)
    WRITE(*,'(3x,"n_Wavelengths    :",1x,i0)') AerosolCoeff%n_Wavelengths   
    WRITE(*,'(3x,"n_Radii          :",1x,i0)') AerosolCoeff%n_Radii         
    WRITE(*,'(3x,"n_Types          :",1x,i0)') AerosolCoeff%n_Types         
    WRITE(*,'(3x,"n_RH             :",1x,i0)') AerosolCoeff%n_RH    
    WRITE(*,'(3x,"n_Legendre_Terms :",1x,i0)') AerosolCoeff%n_Legendre_Terms
    WRITE(*,'(3x,"n_Phase_Elements :",1x,i0)') AerosolCoeff%n_Phase_Elements
    IF ( .NOT. AerosolCoeff_Associated(AerosolCoeff) ) RETURN
    WRITE(*,'(3x,"AerosolCoeff Type_Name:")') 
    DO i = 1, AerosolCoeff%n_Types
      IF ( AerosolCoeff%Type(i) < 1 .OR. &
           AerosolCoeff%Type(i) > AerosolCoeff%n_Types ) THEN
        WRITE(*,'(5x,i2,") Invalid type")') i
      ELSE
        WRITE(*,'(5x,i2,") ",a)') i, TRIM(AerosolCoeff%Type_Name(i))
      END IF
    END DO
    WRITE(*,'(3x,"AerosolCoeff Wavelength:")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%Wavelength
    WRITE(*,'(3x,"AerosolCoeff Frequency :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%Frequency     
    WRITE(*,'(3x,"AerosolCoeff Reff      :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%Reff     
    WRITE(*,'(3x,"AerosolCoeff RH        :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%RH 
    WRITE(*,'(3x,"AerosolCoeff ke        :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%ke     
    WRITE(*,'(3x,"AerosolCoeff w         :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%w      
    WRITE(*,'(3x,"AerosolCoeff g         :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%g      
    WRITE(*,'(3x,"AerosolCoeff pcoeff    :")') 
    WRITE(*,'(5(1x,es13.6,:))') AerosolCoeff%pcoeff
  END SUBROUTINE AerosolCoeff_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the AerosolCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = AerosolCoeff_ValidRelease( AerosolCoeff )
!
! INPUTS:
!       AerosolCoeff:  AerosolCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AerosolCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION AerosolCoeff_ValidRelease( AerosolCoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( AerosolCoeff%Release < AEROSOLCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A AerosolCoeff data update is needed. ", &
                  &"AerosolCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  AerosolCoeff%Release, AEROSOLCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( AerosolCoeff%Release > AEROSOLCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A AerosolCoeff software update is needed. ", &
                  &"AerosolCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  AerosolCoeff%Release, AEROSOLCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION AerosolCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a AerosolCoeff object.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_Info( AerosolCoeff, Info )
!
! INPUTS:
!       AerosolCoeff:  AerosolCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AerosolCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed AerosolCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AerosolCoeff_Info( AerosolCoeff, Info )
    ! Arguments
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff
    CHARACTER(*),       INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"AerosolCoeff RELEASE.VERSION: ",i2,".",i2.2,2x, &
           &"N_WAVELENGTHS=",i4,2x,&
           &"N_RADII=",i3,2x,&
           &"N_TYPES=",i2,2x,&
           &"N_RH=",i3,2x,&
           &"N_LEGENDRE_TERMS=",i2,2x,&
           &"N_PHASE_ELEMENTS=",i2 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           AerosolCoeff%Release, AerosolCoeff%Version, &
           AerosolCoeff%n_Wavelengths   , &
           AerosolCoeff%n_Radii         , &
           AerosolCoeff%n_Types         , &
           AerosolCoeff%n_RH            , &
           AerosolCoeff%n_Legendre_Terms, &
           AerosolCoeff%n_Phase_Elements
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE AerosolCoeff_Info
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Frequency
!
! PURPOSE:
!       Elemental subroutine to fill the frequency component of
!       an AerosolCoeff object.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_Frequency( AerosolCoeff )
!
! INPUTS:
!       AerosolCoeff:  AerosolCoeff object for which the frequencies
!                      are to be computed.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AerosolCoeff_type)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AerosolCoeff_Frequency( AerosolCoeff )
    TYPE(AerosolCoeff_type), INTENT(IN OUT) :: AerosolCoeff
    IF ( .NOT. AerosolCoeff_Associated( AerosolCoeff ) ) RETURN
    AerosolCoeff%Frequency = micron_to_inverse_cm( REAL(AerosolCoeff%Wavelength,fp) )
  END SUBROUTINE AerosolCoeff_Frequency
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AerosolCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AerosolCoeff_DefineVersion




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
!       AerosolCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two AerosolCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = AerosolCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two AerosolCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AerosolCoeff_type)
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

  ELEMENTAL FUNCTION AerosolCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(AerosolCoeff_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the object association status
    IF ( (.NOT. AerosolCoeff_Associated(x)) .OR. &
         (.NOT. AerosolCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_Wavelengths      /= y%n_Wavelengths     ) .OR. &
         (x%n_Radii            /= y%n_Radii           ) .OR. &
         (x%n_Types            /= y%n_Types           ) .OR. &
         (x%n_RH               /= y%n_RH              ) .OR. &
         (x%Max_Legendre_Terms /= y%Max_Legendre_Terms) .OR. &
         (x%n_Legendre_Terms   /= y%n_Legendre_Terms  ) .OR. &
         (x%Max_Phase_Elements /= y%Max_Phase_Elements) .OR. &
         (x%n_Phase_Elements   /= y%n_Phase_Elements  ) ) RETURN
    ! ...Data
    IF ( ALL(x%Type          ==     y%Type       ) .AND. &
         ALL(x%Type_Name     ==     y%Type_Name  ) .AND. &
         ALL(x%Wavelength .EqualTo. y%Wavelength ) .AND. &
         ALL(x%Frequency  .EqualTo. y%Frequency  ) .AND. &
         ALL(x%Reff       .EqualTo. y%Reff       ) .AND. &
         ALL(x%RH         .EqualTo. y%RH         ) .AND. &
         ALL(x%ke         .EqualTo. y%ke         ) .AND. &
         ALL(x%w          .EqualTo. y%w          ) .AND. &
         ALL(x%g          .EqualTo. y%g          ) .AND. &
         ALL(x%pcoeff     .EqualTo. y%pcoeff     )       ) &
      is_equal = .TRUE.

  END FUNCTION AerosolCoeff_Equal
  
END MODULE AerosolCoeff_Define
