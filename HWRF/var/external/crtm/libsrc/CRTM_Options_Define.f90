!
! CRTM_Options_Define
!
! Module defining the CRTM Options optional argument data structure
! and containing routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Sep-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Options_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE CRTM_Parameters      , ONLY: ZERO, ONE, STRLEN
  USE SSU_Input_Define     , ONLY: SSU_Input_type, &
                                   OPERATOR(==), &
                                   SSU_Input_IsValid, &
                                   SSU_Input_Inspect
  USE Zeeman_Input_Define  , ONLY: Zeeman_Input_type, &
                                   OPERATOR(==), &
                                   Zeeman_Input_IsValid, &
                                   Zeeman_Input_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_Options_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Public procedures
  PUBLIC :: CRTM_Options_Associated
  PUBLIC :: CRTM_Options_Destroy
  PUBLIC :: CRTM_Options_Create
  PUBLIC :: CRTM_Options_IsValid
  PUBLIC :: CRTM_Options_Inspect
  PUBLIC :: CRTM_Options_DefineVersion


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Options_Equal
  END INTERFACE OPERATOR(==)

  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Options_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ----------------------------
  ! Options data type definition
  ! ----------------------------
  !:tdoc+:
  TYPE :: CRTM_Options_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Input checking on by default
    LOGICAL :: Check_Input = .TRUE.
    ! User defined emissivity/reflectivity
    ! ...Dimensions
    INTEGER :: n_Channels = 0  ! L dimension
    ! ...Index into channel-specific components
    INTEGER :: Channel = 0
    ! ...Emissivity optional arguments
    LOGICAL :: Use_Emissivity = .FALSE.
    REAL(fp), ALLOCATABLE :: Emissivity(:)  ! L
    ! ...Direct reflectivity optional arguments
    LOGICAL :: Use_Direct_Reflectivity = .FALSE.
    REAL(fp), ALLOCATABLE :: Direct_Reflectivity(:) ! L
    ! Antenna correction application
    LOGICAL :: Use_Antenna_Correction = .FALSE.
    ! SSU instrument input
    TYPE(SSU_Input_type) :: SSU
    ! Zeeman-splitting input
    TYPE(Zeeman_Input_type) :: Zeeman
  END TYPE CRTM_Options_type
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
!       CRTM_Options_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM Options object.
!
! CALLING SEQUENCE:
!       Status = CRTM_Options_Associated( Options )
!
! OBJECTS:
!       Options:      Options structure which is to have its member's
!                     status tested.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:       The return value is a logical value indicating the
!                     status of the Options members.
!                       .TRUE.  - if the array components are allocated.
!                       .FALSE. - if the array components are not allocated.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Same as input Options argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Options_Associated( Options ) RESULT( Status )
    TYPE(CRTM_Options_type), INTENT(IN) :: Options
    LOGICAL :: Status
    Status = Options%Is_Allocated
  END FUNCTION CRTM_Options_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Options_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM Options objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Options_Destroy( Options )
!
! OBJECTS:
!       Options:      Re-initialized Options structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Options_Destroy( Options )
    TYPE(CRTM_Options_type), INTENT(OUT) :: Options
    Options%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_Options_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Options_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM Options object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Options_Create( Options, n_Channels )
!
! OBJECTS:
!       Options:      Options structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Options_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:   Number of channels for which there is Options data.
!                     Must be > 0.
!                     This dimension only applies to the emissivity-related
!                     components.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as Options object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Options_Create( Options, n_Channels )
    ! Arguments
    TYPE(CRTM_Options_type), INTENT(OUT) :: Options
    INTEGER,                 INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( Options%Emissivity(n_Channels), &
              Options%Direct_Reflectivity(n_Channels), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    Options%n_Channels = n_Channels
    ! ...Arrays
    Options%Emissivity = ZERO
    Options%Direct_Reflectivity = ZERO
    
    ! Set allocation indicator
    Options%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_Options_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Options_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM Options object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_Options_IsValid( opt )
!
!         or
!
!       IF ( CRTM_Options_IsValid( opt ) ) THEN....
!
! OBJECTS:
!       opt:       CRTM Options object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Options_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., Options object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  Options object can be used in CRTM.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Options_IsValid( opt ) RESULT( IsValid )
    TYPE(CRTM_Options_type), INTENT(IN) :: opt
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Options_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check emissivity options
    IF ( opt%Use_Emissivity .OR. opt%Use_Direct_Reflectivity ) THEN
      IsValid = CRTM_Options_Associated(opt)
      IF ( .NOT. IsValid ) THEN
        msg = 'Options structure not allocated'
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
        RETURN
      ENDIF
      IF ( opt%Use_Emissivity ) THEN
        IF ( ANY(opt%Emissivity < ZERO) .OR. ANY(opt%Emissivity > ONE) ) THEN
          msg = 'Invalid emissivity'
          CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
          IsValid = .FALSE.
        END IF
      END IF
      IF ( opt%Use_Direct_Reflectivity ) THEN
        IF ( ANY(opt%Direct_Reflectivity < ZERO) .OR. ANY(opt%Direct_Reflectivity > ONE) ) THEN
          msg = 'Invalid direct reflectivity'
          CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
          IsValid = .FALSE.
        END IF
      END IF
    END IF

    ! Check SSU input options
    IsValid = SSU_Input_IsValid( opt%SSU ) .AND. IsValid
    
    ! Check Zeeman input options    
    IsValid = Zeeman_Input_IsValid( opt%Zeeman ) .AND. IsValid

  END FUNCTION CRTM_Options_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Options_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM Options object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Options_Inspect( Options )
!
! INPUTS:
!       Options:       CRTM Options object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Options_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Options_Inspect( Options )
    TYPE(CRTM_Options_type), INTENT(IN) :: Options
    WRITE(*,'(1x,"Options OBJECT")')
    ! Display components
    ! ...Emissivity component
    WRITE(*,'(3x,"Emissivity component")')
    WRITE(*,'(5x,"n_Channels    :",1x,i0)') Options%n_Channels
    WRITE(*,'(5x,"Channel index :",1x,i0)') Options%Channel
    IF ( Options%Use_Emissivity ) THEN
      WRITE(*,'(5x,"Use emissivity : YES")')
      IF ( CRTM_Options_Associated(Options) ) THEN
        WRITE(*,'(5x,"Emissivity :")') 
        WRITE(*,'(5(1x,es13.6,:))') Options%Emissivity
      END IF
    ELSE
      WRITE(*,'(5x,"Use emissivity : NO")')
    END IF
    IF ( Options%Use_Direct_Reflectivity ) THEN
      WRITE(*,'(5x,"Use direct reflectivity : YES")')
      IF ( CRTM_Options_Associated(Options) ) THEN
        WRITE(*,'(5x,"Direct reflectivity :")') 
        WRITE(*,'(5(1x,es13.6,:))') Options%Direct_Reflectivity
      END IF
    ELSE
      WRITE(*,'(5x,"Use direct reflectivity : NO")')
    END IF
    ! ...Antenna correction component
    WRITE(*,'(3x,"Antenna correction component")')
    IF ( Options%Use_Antenna_Correction ) THEN
      WRITE(*,'(5x,"Use antenna correction : YES")')
    ELSE
      WRITE(*,'(5x,"Use antenna correction : NO")')
    END IF
    ! ...SSU input
    CALL SSU_Input_Inspect( Options%SSU )
    ! ...Zeeman input
    CALL Zeeman_Input_Inspect( Options%Zeeman )
        
  END SUBROUTINE CRTM_Options_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Options_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Options_DefineVersion( Id )
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

  SUBROUTINE CRTM_Options_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Options_DefineVersion


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
!       CRTM_Options_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_Options objects.
!       Used in OPERATOR(==) interface block.
!
!       Note: Only the dimensionality and radiance/brightness temperatures
!             are checked for equality.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_Options_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM Options objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Options_type
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

  ELEMENTAL FUNCTION CRTM_Options_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Options_type) , INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Emissivity component
    is_equal = ( (x%n_Channels == y%n_Channels) .AND. &
                 (x%Channel    == y%Channel   ) .AND. &
                 (x%Use_Emissivity          .EQV. y%Use_Emissivity) .AND. &
                 (x%Use_Direct_Reflectivity .EQV. y%Use_Direct_Reflectivity) )
    IF ( CRTM_Options_Associated(x) .AND. CRTM_Options_Associated(y) ) &
      is_equal = is_equal .AND. &
                 ALL(x%Emissivity .EqualTo. y%Emissivity) .AND. &
                 ALL(x%Direct_Reflectivity .EqualTo. y%Direct_Reflectivity)
    
    ! Antenna correction component
    is_equal = is_equal .AND. &
               (x%Use_Antenna_Correction .EQV. y%Use_Antenna_Correction)

    ! SSU input
    is_equal = is_equal .AND. &
               (x%SSU == y%SSU)
                 
    ! Zeeman input
    is_equal = is_equal .AND. &
               (x%Zeeman == y%Zeeman)
                 
  END FUNCTION CRTM_Options_Equal

END MODULE CRTM_Options_Define
