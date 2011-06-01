!
! SSU_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to SSU
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Oct 6, 2009
!                       yong.han@noaa.gov
!
!                       Paul van Delst, 20-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE SSU_Input_Define

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
  PUBLIC :: SSU_Input_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: SSU_Input_GetValue
  PUBLIC :: SSU_Input_SetValue
  PUBLIC :: SSU_Input_CellPressureIsSet
  PUBLIC :: SSU_Input_IsValid
  PUBLIC :: SSU_Input_Inspect
  PUBLIC :: SSU_Input_DefineVersion


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE SSU_Input_Equal
  END INTERFACE OPERATOR(==)

  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: SSU_Input_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256

  ! Literal constants  
  REAL(fp), PARAMETER :: ZERO = 0.0_fp

  ! SSU instrument specific data
  INTEGER,  PARAMETER :: MAX_N_CHANNELS = 3
  
  
  !--------------------
  ! Structure defintion
  !--------------------
  !:tdoc+:
  TYPE :: SSU_Input_type
    PRIVATE
    ! Time in decimal year (e.g. 2009.08892694 corresponds to 11:00 Feb. 2, 2009)
    REAL(fp) :: Time = ZERO
    ! SSU CO2 cell pressures (hPa)
    REAL(fp) :: Cell_Pressure(MAX_N_CHANNELS) = ZERO
  END TYPE SSU_Input_type
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
!       SSU_Input_SetValue
! 
! PURPOSE:
!       Elemental subroutine to set the values of SSU_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_SetValue( SSU_Input                    , &
!                                Time          = Time         , & 
!                                Cell_Pressure = Cell_Pressure, & 
!                                Channel       = Channel        ) 
!
! OBJECTS:
!       SSU_Input:            SSU_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       SSU_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Time:                 SSU instrument mission time.
!                             UNITS:      decimal year
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Cell_Pressure:        SSU channel CO2 cell pressure. Must be
!                             specified with the Channel optional dummy
!                             argument.
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Channel:              SSU channel for which the CO2 cell pressure
!                             is to be set. Must be specified with the 
!                             Cell_Pressure optional dummy argument.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SSU_Input_SetValue ( &
    SSU_Input    , &
    Time         , &
    Cell_Pressure, &
    Channel        )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN OUT) :: SSU_Input
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Time
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Cell_Pressure
    INTEGER,    OPTIONAL, INTENT(IN)     :: Channel
    ! Variables
    INTEGER :: n
    
    ! Set values
    IF ( PRESENT(Time) ) SSU_Input%Time = Time 
    IF ( PRESENT(Channel) .AND. PRESENT(Cell_Pressure) ) THEN
      n = MAX(MIN(Channel,MAX_N_CHANNELS),1)
      SSU_Input%Cell_Pressure(n) = Cell_Pressure
    END IF

  END SUBROUTINE SSU_Input_SetValue   


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_GetValue
! 
! PURPOSE:
!       Elemental subroutine to Get the values of SSU_Input
!       object components.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_GetValue( SSU_Input                    , &
!                                Channel       = Channel      , & 
!                                Time          = Time         , & 
!                                Cell_Pressure = Cell_Pressure, & 
!                                n_Channels    = n_Channels     ) 
!
! OBJECTS:
!       SSU_Input:            SSU_Input object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       SSU_Input_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Channel:              SSU channel for which the CO2 cell pressure
!                             is required.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Time:                 SSU instrument mission time.
!                             UNITS:      decimal year
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Cell_Pressure:        SSU channel CO2 cell pressure. Must be
!                             specified with the Channel optional input
!                             dummy argument.
!                             UNITS:      hPa
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:           Number of SSU channels..
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as SSU_Input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SSU_Input_GetValue( &
    SSU_Input    , &
    Channel      , &
    Time         , &
    Cell_Pressure, &
    n_Channels     )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN)  :: SSU_Input
    INTEGER,    OPTIONAL, INTENT(IN)  :: Channel
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Time
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Cell_Pressure
    INTEGER,    OPTIONAL, INTENT(OUT) :: n_Channels
    ! Variables
    INTEGER :: n

    ! Get values
    IF ( PRESENT(Time) ) Time = SSU_Input%Time
    IF ( PRESENT(Channel) .AND. PRESENT(Cell_Pressure) ) THEN
      n = MAX(MIN(Channel,MAX_N_CHANNELS),1)
      Cell_Pressure = SSU_Input%Cell_Pressure(n)
    END IF
    IF ( PRESENT(n_Channels) ) n_Channels = MAX_N_CHANNELS
    
  END SUBROUTINE SSU_Input_GetValue  


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_CellPressureIsSet
! 
! PURPOSE:
!       Elemental function to determine if SSU_Input object cell pressures
!       are set (i.e. > zero).
!
! CALLING SEQUENCE:
!       result = SSU_Input_CellPressureIsSet( ssu )
!
!         or
!
!       IF ( SSU_Input_CellPressureIsSet( ssu ) ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       ssu:       SSU_Input object for which the cell pressures 
!                  are to be tested.
!                  UNITS:      N/A
!                  TYPE:       SSU_Input_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not all the
!                  SSU cell pressures are set.
!                  If == .FALSE., cell pressure values are <= 0.0hPa and
!                                 thus are considered to be NOT set or valid.
!                     == .TRUE.,  cell pressure values are > 0.0hPa and
!                                 thus are considered to be set and valid.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION SSU_Input_CellPressureIsSet( ssu ) RESULT( Is_Set )
    TYPE(SSU_Input_type), INTENT(IN) :: ssu
    LOGICAL :: Is_Set
    Is_Set = ALL(ssu%Cell_Pressure > ZERO)
  END FUNCTION SSU_Input_CellPressureIsSet



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       SSU_Input object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = SSU_Input_IsValid( ssu )
!
!         or
!
!       IF ( SSU_Input_IsValid( ssu ) ) THEN....
!
! OBJECTS:
!       ssu:       SSU_Input object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       SSU_Input_type
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

  FUNCTION SSU_Input_IsValid( ssu ) RESULT( IsValid )
    TYPE(SSU_Input_type), INTENT(IN) :: ssu
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SSU_Input_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Check time
    IF ( ssu%Time < ZERO ) THEN
      msg = 'Invalid mission time'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
    ! Check cell pressures
    IF ( ANY(ssu%Cell_Pressure < ZERO) ) THEN
      msg = 'Invalid cell pressures'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
  END FUNCTION SSU_Input_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an SSU_Input object to stdout.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_Inspect( ssu )
!
! INPUTS:
!       ssu:           SSU_Input object to display.
!                      UNITS:      N/A
!                      TYPE:       SSU_Input_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE SSU_Input_Inspect(ssu)
    TYPE(SSU_Input_type), INTENT(IN) :: ssu
    WRITE(*,'(3x,"SSU_Input OBJECT")')
    WRITE(*,'(5x,"Mission time:",1x,es22.15)') ssu%Time
    WRITE(*,'(5x,"Channel cell pressures:",10(1x,es22.15,:))') ssu%Cell_Pressure
  END SUBROUTINE SSU_Input_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SSU_Input_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL SSU_Input_DefineVersion( Id )
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

  SUBROUTINE SSU_Input_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE SSU_Input_DefineVersion


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION SSU_Input_Equal(x, y) RESULT(is_equal)
    TYPE(SSU_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = (x%Time .EqualTo. y%Time) .AND. &
               ALL(x%Cell_Pressure .EqualTo. y%Cell_Pressure)
  END FUNCTION SSU_Input_Equal
  
END MODULE SSU_Input_Define
