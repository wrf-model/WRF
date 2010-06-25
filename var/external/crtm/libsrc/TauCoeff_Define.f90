!
! TauCoeff_Define
!
! Module defining the TauCoeff data structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 18-Mar-2002
!                       paul.vandelst@noaa.gov
!
!       Updated by:     David Groff, EMC/SAIC Oct-2009
!                       david.groff@noaa.gov
!

MODULE TauCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE ODAS_Define,           ONLY: ODAS_type
  USE ODPS_Define,           ONLY: ODPS_type
  USE ODSSU_Define,          ONLY: ODSSU_type
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: TauCoeff_type
  ! Operators
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(.EQ.)
  ! Procedures
  PUBLIC :: TauCoeff_Associated
  PUBLIC :: TauCoeff_Destroy
  PUBLIC :: TauCoeff_Create
  PUBLIC :: TauCoeff_Info

  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE TauCoeff_Assign
  END INTERFACE
  
  INTERFACE OPERATOR(.EQ.)
    MODULE PROCEDURE TauCoeff_Equal
  END INTERFACE
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: TauCoeff_Define.f90 5780 2009-11-19 17:22:43Z david.groff@noaa.gov $'
  ! Message string length
  INTEGER , PARAMETER :: ML = 256
  ! Sensor ID string length
  INTEGER , PARAMETER :: SL = 20

  
  ! -----------------------
  ! Derived type definition  
  ! -----------------------
  TYPE :: TauCoeff_type
    ! Array dimensions
    INTEGER :: n_Sensors          = 0       ! n
    INTEGER :: n_ODAS             = 0       ! I1
    INTEGER :: n_ODPS             = 0       ! I2
    INTEGER :: n_ODSSU            = 0       ! I3
    INTEGER :: n_ODZeeman         = 0       ! I4
    ! Arrays
    INTEGER, ALLOCATABLE :: Algorithm_ID(:)    ! n
    INTEGER, ALLOCATABLE :: Sensor_Index(:)    ! n
    INTEGER, ALLOCATABLE :: Sensor_LoIndex(:)  ! n ; Local sensor index for a collection
                                               !     of sensor using the same algorithm
    INTEGER, ALLOCATABLE :: ZSensor_LoIndex(:) ! n; Local sensor index for a collection of sensors 
                                               !    with channles requiring the Zeeman algorithm
    ! Sensor Info: Sensor types and IDs
    CHARACTER(SL), ALLOCATABLE :: Sensor_ID(:)  ! n
    INTEGER, ALLOCATABLE :: WMO_Satellite_ID(:) ! n
    INTEGER, ALLOCATABLE :: WMO_Sensor_ID(:)    ! n
    INTEGER, ALLOCATABLE :: Sensor_Type(:)      ! n
    
    ! Pointers
    TYPE(ODAS_type),  POINTER :: ODAS(:)     => NULL() ! I1
    TYPE(ODPS_type),  POINTER :: ODPS(:)     => NULL() ! I2
    TYPE(ODSSU_type), POINTER :: ODSSU(:)    => NULL() ! I3
    TYPE(ODPS_type),  POINTER :: ODZeeman(:) => NULL() ! I4

  END TYPE TauCoeff_type


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       TauCoeff_Associated
!
! PURPOSE:
!       Elemental Function to test the status of the allocatable and pointer 
!       components of a TauCoeff structure.
!
! CALLING SEQUENCE:
!       Is_Associated = TauCoeff_Associated( self )
!
! OBJECTS:
!       self:        TauCoeff structure which is to have its allocatable
!                    and pointer components status tested.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Is_Associated:  The return value is a logical value indicating the
!                       status of the allocatable and pointer components.     
!                       .TRUE.  - if ANY of the TauCoeff allocatable or       
!                                 pointer members are in use.                 
!                       .FALSE. - if ALL of the TauCoeff allocatable or       
!                                 pointer members are not in use.             
!                       UNITS:      N/A                                       
!                       TYPE:       LOGICAL                                   
!                       DIMENSION:  Scalar                                    
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION TauCoeff_Associated( self ) RESULT( Is_Associated )
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: Is_Associated

    ! Test the structure associations
    Is_Associated = &
      ALLOCATED(self%Algorithm_ID  ) .OR. &
      ALLOCATED(self%Sensor_Index  ) .OR. &
      ALLOCATED(self%Sensor_LoIndex) .OR. &
      ASSOCIATED(self%ODAS         ) .OR. &  ! Should this be tested?
      ASSOCIATED(self%ODPS         ) .OR. &  ! Should this be tested?
      ASSOCIATED(self%ODSSU        ) .OR. &  ! Should this be tested?
      ASSOCIATED(self%ODZeeman     )         ! Should this be tested?

  END FUNCTION TauCoeff_Associated
  

!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       TauCoeff_Destroy
! 
! PURPOSE:
!       Subroutine to re-initialize the TauCoeff data structures.
!
! CALLING SEQUENCE:
!       CALL TauCoeff_Destroy( self, err_stat )
!
! OBJECTS:
!       self:         Re-initialized TauCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       err_stat:     The error status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the object destruction was successful
!                        == FAILURE an error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the TauCoeff object argument is IN OUT rather than
!       just OUT. This is necessary because the argument has pointer components
!       and may be defined upon input. To prevent memory leaks, the IN OUT
!       INTENT is a must.
!:sdoc-:
!------------------------------------------------------------------------------
  SUBROUTINE TauCoeff_Destroy(self, err_stat)
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN OUT) :: self
    INTEGER,             INTENT(OUT)    :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'TauCoeff_Destroy'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat

    ! Set up
    err_stat = SUCCESS
    ! ...If structure is unused, do nothing
    IF ( .NOT. TauCoeff_Associated(self) ) RETURN
    
    ! Re-initialize the dimensions
    self%n_Sensors = 0

    ! Deallocate the non-scalar components
    DEALLOCATE( self%Algorithm_ID    , &
                self%Sensor_Index    , &
                self%Sensor_LoIndex  , &
                self%ZSensor_LoIndex , &
                self%Sensor_ID       , &
                self%WMO_Satellite_ID, &
                self%WMO_Sensor_ID   , &
                self%Sensor_Type     , &               
                STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg, '( "Error deallocating. STAT = ", i0 )' ) alloc_stat
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END IF
    
    ! Disassociate pointers
    NULLIFY( self%ODAS, self%ODPS, self%ODSSU, self%ODZeeman )

  END SUBROUTINE TauCoeff_Destroy


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       TauCoeff_Create
! 
! PURPOSE:
!       Subroutine to create an instance of the TauCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL TauCoeff_Create( self, n_Sensors, err_stat )
!
! OBJECTS:
!       self:         Instance of the TauCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       n_Sensors:    Number of sensors
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       err_stat:     The error status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the object creation was successful
!                        == FAILURE an error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the TauCoeff object argument is IN OUT rather than
!       just OUT. This is necessary because the argument has pointer components
!       and may be defined upon input. To prevent memory leaks, the IN OUT
!       INTENT is a must.
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE TauCoeff_Create(self, n_Sensors, err_stat)
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN OUT) :: self
    INTEGER,             INTENT(IN)     :: n_Sensors
    INTEGER,             INTENT(OUT)    :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'TauCoeff_Create'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat

    ! Set up
    err_stat = SUCCESS
    ! ...Check input
    IF ( TauCoeff_Associated( self ) ) THEN
      CALL TauCoeff_Destroy( self, err_stat )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error destroying TauCoeff prior to allocation.'
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
        RETURN
      END IF
    END IF
    IF ( n_Sensors < 1 ) THEN
      err_stat = FAILURE
      msg = 'n_Sensors must be > 0.'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF


    ! Perform the array allocation
    ALLOCATE( self%Algorithm_ID( n_Sensors )    , &
              self%Sensor_Index( n_Sensors )    , &
              self%Sensor_LoIndex( n_Sensors )  , &
              self%ZSensor_LoIndex( n_Sensors ) , &
              self%Sensor_ID( n_Sensors )       , &
              self%WMO_Satellite_ID( n_Sensors ), &
              self%WMO_Sensor_ID( n_Sensors )   , &
              self%Sensor_Type( n_Sensors )     , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg, '("Error allocating TauCoeff. STAT = ",i0)' ) alloc_stat
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
      RETURN
    END IF


    ! Initialise
    ! ...Dimensions
    self%n_Sensors = n_Sensors
    ! ...Arrays
    self%Algorithm_ID     = 0
    self%Sensor_Index     = 0
    self%Sensor_LoIndex   = 0
    self%ZSensor_LoIndex  = 0
    self%Sensor_ID        = ''
    self%WMO_Satellite_ID = 0
    self%WMO_Sensor_ID    = 0
    self%Sensor_Type      = 0
    ! ...Pointers (not required, but what the hell...)
    NULLIFY( self%ODAS, self%ODPS, self%ODSSU, self%ODZeeman )

  END SUBROUTINE TauCoeff_Create
  
!------------------------------------------------------------------------------
! NAME:
!       TauCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing information about
!       a TauCoeff data structure.
!
! CALLING SEQUENCE:
!       CALL TauCoeff_Info( self, info )
!
! OBJECTS:
!       self:          TauCoeff structure about which information is required.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       info:          String containing information about the passed
!                      TauCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!------------------------------------------------------------------------------

  SUBROUTINE TauCoeff_Info( self, info )
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN)  :: self
    CHARACTER(*),        INTENT(OUT) :: info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: long_string

    ! Write the required info to the local string
    WRITE( long_string, '( a, 2x, &
                           &"N_SENSORS=",i2,2x,&
                           &"N_ODAS=",i2,2x,&
                           &"N_ODPS=",i2,2x,&
                           &"N_ODSSU=",i2,2x,&
                           &"N_ODZeeman=",i2 )' ) &
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         self%n_Sensors, &
                         self%n_ODAS, &
                         self%n_ODPS, &
                         self%n_ODSSU, &
                         self%n_ODZeeman
    
    ! Trim the output based on the
    ! dummy argument string length
    info = long_string(1:MIN( LEN(info), LEN_TRIM( long_string ) ))

  END SUBROUTINE TauCoeff_Info

!------------------------------------------------------------------------------
!
! NAME:
!       TauCoeff_Assign
!
! PURPOSE:
!       Subroutine to copy valid TauCoeff structures. Used in ASSIGNMENT(=)
!       interface block.
!
! CALLING SEQUENCE:
!       CALL TauCoeff_Assign( copy, original )
!
!         or
!
!       copy = original
!
! OBJECTS:
!       copy:          Destination structure for copy.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!       original:      Structure to be copied.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       Note the INTENT on the TauCoeff copy argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE TauCoeff_Assign( copy, original )
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN OUT) :: copy
    TYPE(TauCoeff_type), INTENT(IN)     :: original
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'TauCoeff_Assign'
    ! Variables
    INTEGER :: err_stat

    ! Destroy the output structure
    CALL TauCoeff_Destroy( copy, err_stat )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Output TauCoeff re-init failed', err_stat )
      RETURN
    END IF

    ! If input structure not used, do nothing
    IF ( .NOT. TauCoeff_Associated( original ) ) RETURN

    ! Create the output structure
    CALL TauCoeff_Create( copy, original%n_Sensors, err_stat )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Output TauCoeff allocation failed', err_stat )
      RETURN
    END IF

    ! Copy array data
    copy%Algorithm_ID     = original%Algorithm_ID
    copy%Sensor_Index     = original%Sensor_Index
    copy%Sensor_LoIndex   = original%Sensor_LoIndex
    copy%ZSensor_LoIndex  = original%ZSensor_LoIndex
    copy%Sensor_ID        = original%Sensor_ID
    copy%WMO_Satellite_ID = original%WMO_Satellite_ID
    copy%WMO_Sensor_ID    = original%WMO_Sensor_ID
    copy%Sensor_Type      = original%Sensor_Type

    ! Set pointers
    IF ( ASSOCIATED(original%ODAS) ) copy%ODAS => original%ODAS
    IF ( ASSOCIATED(original%ODPS) ) copy%ODPS => original%ODPS
    IF ( ASSOCIATED(original%ODSSU) ) copy%ODSSU => original%ODSSU
    IF ( ASSOCIATED(original%ODZeeman) ) copy%ODZeeman => original%ODZeeman
    
  END SUBROUTINE TauCoeff_Assign

!------------------------------------------------------------------------------
!
! NAME:
!       TauCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two TauCoeff structures.
!       Used in OPERATOR(.EQ.) interface block.
!
! CALLING SEQUENCE:
!       is_equal = TauCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two TauCoeff structures to be compared.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION TauCoeff_Equal( x, y ) RESULT( is_equal )
    ! Arguments
    TYPE(TauCoeff_type), INTENT(IN) :: x, y
    ! Function result
    LOGICAL :: is_equal
    
    ! Setup
    is_equal = .FALSE.
    
    ! Check dimensions
    IF ( (x%n_Sensors /= y%n_Sensors) .OR. &
         (x%n_ODAS    /= y%n_ODAS   ) .OR. &
         (x%n_ODPS    /= y%n_ODPS   ) .OR. &
         (x%n_ODSSU   /= y%n_ODSSU  ) .OR. &
         (x%n_ODZeeman /= y%n_ODZeeman  ) ) RETURN

    ! Check arrays
    IF ( ANY(x%Algorithm_ID     /= y%Algorithm_ID  )    .OR. & 
         ANY(x%Sensor_Index     /= y%Sensor_Index  )    .OR. & 
         ANY(x%Sensor_LoIndex   /= y%Sensor_LoIndex)    .OR. & 
         ANY(x%ZSensor_LoIndex  /= y%ZSensor_LoIndex)   .OR. &
         ANY(x%Sensor_ID        /= y%Sensor_ID)         .OR. &
         ANY(x%WMO_Satellite_ID /= y%WMO_Satellite_ID)  .OR. &
         ANY(x%WMO_Sensor_ID    /= y%WMO_Sensor_ID)     .OR. &
         ANY(x%Sensor_Type      /= y%Sensor_Type)      ) RETURN

    ! Check pointers
    ! .... ? 
    ! Call individual ODAS_Equal, ODPS_Equal and ODSSU_Equal functions?
    ! If so, they must be elemental also!

    ! If we get here, everything is equal!
    is_equal = .TRUE.
    
  END FUNCTION TauCoeff_Equal

END MODULE TauCoeff_Define
