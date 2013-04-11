!
! Zeeman_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to Zeeman
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE Zeeman_Input_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Datatypes
  PUBLIC :: Zeeman_Input_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: Zeeman_Input_GetValue
  PUBLIC :: Zeeman_Input_SetValue
  PUBLIC :: Zeeman_Input_IsValid
  PUBLIC :: Zeeman_Input_Inspect
  PUBLIC :: Zeeman_Input_DefineVersion


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE Zeeman_Input_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Zeeman_Input_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256

  ! Literal constants  
  REAL(fp), PARAMETER :: ZERO = 0.0_fp

  ! Zeeman specific data
  REAL(fp), PARAMETER :: DEFAULT_MAGENTIC_FIELD = 0.3_fp
  

  !--------------------
  ! Structure defintion
  !--------------------
  !:tdoc+:
  TYPE :: Zeeman_Input_type
    PRIVATE
    ! Earth magnetic field strength in Gauss
    REAL(fp) :: Be = DEFAULT_MAGENTIC_FIELD
    ! Cosine of the angle between the Earth
    ! magnetic field and wave propagation direction                     
    REAL(fp) :: Cos_ThetaB = ZERO
    ! Cosine of the azimuth angle of the Be vector.
    REAL(fp) :: Cos_PhiB = ZERO
    ! Doppler frequency shift caused by Earth-rotation.
    REAL(fp) :: Doppler_Shift = ZERO 
  END TYPE Zeeman_Input_type
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
!       Zeeman_Input_SetValue
! 
! PURPOSE:
!       Elemental subroutine to set the values of Zeeman_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_SetValue( Zeeman_Input                   , &
!                                   Field_Strength = Field_Strength, & 
!                                   Cos_ThetaB     = Cos_ThetaB    , & 
!                                   Cos_PhiB       = Cos_PhiB      , &
!                                   Doppler_Shift  = Doppler_Shift   )
!
! OBJECTS:
!       Zeeman_Input:         Zeeman_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       Zeeman_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Field_Strength:       Earth's magnetic filed strength
!                             UNITS:      Gauss
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Cos_ThetaB:           Cosine of the angle between the Earth magnetic
!                             field and wave propagation vectors.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Cos_PhiB:             Cosine of the azimuth angle of the Earth magnetic
!                             field vector.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Doppler_Shift:        Doppler frequency shift caused by Earth-rotation.
!                             Positive towards sensor.
!                             UNITS:      KHz
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Zeeman_Input_SetValue( &
    Zeeman_Input  , &
    Field_Strength, &
    Cos_ThetaB    , &
    Cos_PhiB      , & 
    Doppler_Shift   )
    ! Arguments
    TYPE(Zeeman_Input_type), INTENT(IN OUT) :: Zeeman_Input
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Field_Strength
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Cos_ThetaB    
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Cos_PhiB    
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Doppler_Shift 
    ! Set components
    IF ( PRESENT(Field_Strength) ) Zeeman_Input%Be            = Field_Strength
    IF ( PRESENT(Cos_ThetaB    ) ) Zeeman_Input%Cos_ThetaB    = Cos_ThetaB    
    IF ( PRESENT(Cos_PhiB      ) ) Zeeman_Input%Cos_PhiB      = Cos_PhiB      
    IF ( PRESENT(Doppler_Shift ) ) Zeeman_Input%Doppler_Shift = Doppler_Shift 
  END SUBROUTINE Zeeman_Input_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_GetValue
! 
! PURPOSE:
!       Elemental subroutine to get the values of Zeeman_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_GetValue( Zeeman_Input                   , &
!                                   Field_Strength = Field_Strength, & 
!                                   Cos_ThetaB     = Cos_ThetaB    , & 
!                                   Cos_PhiB       = Cos_PhiB      , &
!                                   Doppler_Shift  = Doppler_Shift   )
!
! OBJECTS:
!       Zeeman_Input:         Zeeman_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       Zeeman_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       Field_Strength:       Earth's magnetic filed strength
!                             UNITS:      Gauss
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Cos_ThetaB:           Cosine of the angle between the Earth magnetic
!                             field and wave propagation vectors.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Cos_PhiB:             Cosine of the azimuth angle of the Earth magnetic
!                             field vector.
!                             UNITS:      N/A
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Doppler_Shift:        Doppler frequency shift caused by Earth-rotation.
!                             Positive towards sensor.
!                             UNITS:      KHz
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as Zeeman_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Zeeman_Input_GetValue( &
    Zeeman_Input  , &
    Field_Strength, &
    Cos_ThetaB    , &
    Cos_PhiB      , & 
    Doppler_Shift   )
    ! Arguments
    TYPE(Zeeman_Input_type),INTENT(IN)  :: Zeeman_Input
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Field_Strength
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_ThetaB    
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_PhiB    
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Doppler_Shift 
    ! Get components
    IF ( PRESENT(Field_Strength) ) Field_Strength = Zeeman_Input%Be
    IF ( PRESENT(Cos_ThetaB    ) ) Cos_ThetaB     = Zeeman_Input%Cos_ThetaB    
    IF ( PRESENT(Cos_PhiB      ) ) Cos_PhiB       = Zeeman_Input%Cos_PhiB    
    IF ( PRESENT(Doppler_Shift ) ) Doppler_Shift  = Zeeman_Input%Doppler_Shift 
  END SUBROUTINE Zeeman_Input_GetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       Zeeman_Input object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = Zeeman_Input_IsValid( z )
!
!         or
!
!       IF ( Zeeman_Input_IsValid( z ) ) THEN....
!
! OBJECTS:
!       z:         Zeeman_Input object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       Zeeman_Input_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  object can be used.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Zeeman_Input_IsValid( z ) RESULT( IsValid )
    TYPE(Zeeman_Input_type), INTENT(IN) :: z
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Zeeman_Input_IsValid'
!!!
real(fp), parameter :: big_number = 1.0e+09_fp
!!!
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check components
    IF ( z%Be < ZERO ) THEN
      msg = 'Invalid field strength'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( z%Cos_ThetaB > big_number ) THEN
      msg = 'Invalid COS(ThetaB)'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( z%Cos_PhiB > big_number ) THEN
      msg = 'Invalid COS(PhiB)'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( ABS(z%Doppler_Shift) > big_number ) THEN
      msg = 'Invalid Doppler shift'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
  END FUNCTION Zeeman_Input_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an Zeeman_Input object to stdout.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_Inspect( z )
!
! INPUTS:
!       z:             Zeeman_Input object to display.
!                      UNITS:      N/A
!                      TYPE:       Zeeman_Input_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Zeeman_Input_Inspect(z)
    TYPE(Zeeman_Input_type), INTENT(IN) :: z
    WRITE(*,'(3x,"Zeeman_Input OBJECT")')
    WRITE(*,'(5x,"Field strength (gauss):",1x,es22.15)') z%Be           
    WRITE(*,'(5x,"COS(ThetaB)           :",1x,es22.15)') z%Cos_ThetaB   
    WRITE(*,'(5x,"COS(PhiB)             :",1x,es22.15)') z%Cos_PhiB   
    WRITE(*,'(5x,"Doppler shift (KHz)   :",1x,es22.15)') z%Doppler_Shift
  END SUBROUTINE Zeeman_Input_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Zeeman_Input_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Zeeman_Input_DefineVersion( Id )
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

  SUBROUTINE Zeeman_Input_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Zeeman_Input_DefineVersion


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION Zeeman_Input_Equal(x, y) RESULT(is_equal)
    TYPE(Zeeman_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = (x%Be            .EqualTo. y%Be           ) .AND. &
               (x%Cos_ThetaB    .EqualTo. y%Cos_ThetaB   ) .AND. &
               (x%Cos_PhiB      .EqualTo. y%Cos_PhiB     ) .AND. &
               (x%Doppler_Shift .EqualTo. y%Doppler_Shift)
  END FUNCTION Zeeman_Input_Equal
  
END MODULE Zeeman_Input_Define
