!
! CRTM_ChannelInfo_Define
!
! Module defining the CRTM ChannelInfo data structure and containing
! routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-May-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_ChannelInfo_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Intrinsic modules
  USE ISO_Fortran_Env      , ONLY: OUTPUT_UNIT
  ! Module use
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE File_Utility         , ONLY: File_Open
  USE CRTM_Parameters      , ONLY: STRLEN
  USE SensorInfo_Parameters, ONLY: INVALID_SENSOR, &
                                   INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID 
  USE Sort_Utility         , ONLY: InsertionSort  
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure definition
  PUBLIC :: CRTM_ChannelInfo_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Structure procedures
  PUBLIC :: CRTM_ChannelInfo_Associated
  PUBLIC :: CRTM_ChannelInfo_Destroy
  PUBLIC :: CRTM_ChannelInfo_Create
  PUBLIC :: CRTM_ChannelInfo_Inspect
  PUBLIC :: CRTM_ChannelInfo_DefineVersion
  PUBLIC :: CRTM_ChannelInfo_n_Channels
  PUBLIC :: CRTM_ChannelInfo_Channels
  PUBLIC :: CRTM_ChannelInfo_Subset


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_ChannelInfo_Equal
  END INTERFACE OPERATOR(==)
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_ChannelInfo_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  

  ! --------------------------------
  ! ChannelInfo data type definition
  ! --------------------------------
  !:tdoc+:
  TYPE :: CRTM_ChannelInfo_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Channels = 0  ! L dimension
    ! Scalar data
    CHARACTER(STRLEN) :: Sensor_ID        = ''
    INTEGER           :: Sensor_Type      = INVALID_SENSOR
    INTEGER           :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER           :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER           :: Sensor_Index     = 0
    ! Array data
    LOGICAL, ALLOCATABLE :: Process_Channel(:)  ! L
    INTEGER, ALLOCATABLE :: Sensor_Channel(:)   ! L
    INTEGER, ALLOCATABLE :: Channel_Index(:)    ! L
  END TYPE CRTM_ChannelInfo_type
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
!       CRTM_ChannelInfo_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM ChannelInfo object.
!
! CALLING SEQUENCE:
!       Status = CRTM_ChannelInfo_Associated( ChannelInfo )
!
! OBJECTS:
!       ChannelInfo:   ChannelInfo object which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CRTM_ChannelInfo_type)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the ChannelInfo members.
!                        .TRUE.  - if the array components are allocated.
!                        .FALSE. - if the array components are not allocated.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input ChannelInfo argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_ChannelInfo_Associated( ChannelInfo ) RESULT( Status )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    LOGICAL :: Status
    Status = ChannelInfo%Is_Allocated
  END FUNCTION CRTM_ChannelInfo_Associated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM ChannelInfo objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_ChannelInfo_Destroy( ChannelInfo )
!
! OBJECTS:
!       ChannelInfo:    Re-initialized ChannelInfo object.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_ChannelInfo_type)
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_ChannelInfo_Destroy( ChannelInfo )
    TYPE(CRTM_ChannelInfo_type), INTENT(OUT) :: ChannelInfo
    ChannelInfo%Is_Allocated = .FALSE.
    ChannelInfo%n_Channels = 0
  END SUBROUTINE CRTM_ChannelInfo_Destroy

  
!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_ChannelInfo_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of a CRTM ChannelInfo
!       object.
!
! CALLING SEQUENCE:
!       CALL CRTM_ChannelInfo_Create( ChannelInfo, n_Channels )
!
! OBJECTS:
!       ChannelInfo:       ChannelInfo object.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_ChannelInfo_type)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:        The number of channels in the ChannelInfo structure.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Conformable with ChannelInfo argument
!                          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_ChannelInfo_Create( &
    ChannelInfo, &
    n_Channels   )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(OUT) :: ChannelInfo
    INTEGER,                     INTENT(IN)  :: n_Channels
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Channels < 1 ) RETURN


    ! Perform the allocations.
    ALLOCATE( ChannelInfo%Process_Channel( n_Channels ), &
              ChannelInfo%Sensor_Channel( n_Channels ), &
              ChannelInfo%Channel_Index( n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    ChannelInfo%n_Channels = n_Channels
    ! ...Arrays
    ChannelInfo%Process_Channel = .TRUE.
    ChannelInfo%Sensor_Channel  = 0
    ChannelInfo%Channel_Index   = 0


    ! Set allocation indicator
    ChannelInfo%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_ChannelInfo_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM ChannelInfo object
!       to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_ChannelInfo_Inspect( chInfo, Unit=unit )
!
! OBJECTS:
!       chInfo:  ChannelInfo object to display.
!                UNITS:      N/A
!                TYPE:       TYPE(CRTM_ChannelInfo_type)
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Unit:    Unit number for an already open file to which the output
!                will be written.
!                If the argument is specified and the file unit is not
!                connected, the output goes to stdout.
!                UNITS:      N/A
!                TYPE:       INTEGER
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_ChannelInfo_Inspect( chInfo, Unit )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: chInfo
    INTEGER,           OPTIONAL, INTENT(IN) :: Unit
    ! Local variables
    INTEGER :: fid
    INTEGER :: i
    CHARACTER(3) :: process
    
    ! Setup
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF


    WRITE(fid,'(1x,"ChannelInfo OBJECT")')
    WRITE(fid,'(3x,"n_Channels       :",1x,i0)') chInfo%n_Channels
    WRITE(fid,'(3x,"Sensor Id        :",1x,a )') TRIM(chInfo%Sensor_ID)
    WRITE(fid,'(3x,"Sensor_Type      :",1x,i0)') chInfo%Sensor_Type
    WRITE(fid,'(3x,"WMO_Satellite_ID :",1x,i0)') chInfo%WMO_Satellite_ID
    WRITE(fid,'(3x,"WMO_Sensor_ID    :",1x,i0)') chInfo%WMO_Sensor_ID   
    WRITE(fid,'(3x,"Sensor_Index     :",1x,i0)') chInfo%Sensor_Index    
    IF ( .NOT. CRTM_ChannelInfo_Associated(chInfo) ) RETURN
    WRITE(fid,'(3x,"Channel#     Index     Process?")')
    DO i = 1, chInfo%n_Channels
      IF ( chInfo%Process_Channel(i) ) THEN
        process = 'yes'
      ELSE
        process = 'no'
      END IF
      WRITE(fid,'(4x,i5,7x,i5,8x,a)') &
        chInfo%Sensor_Channel(i), chInfo%Channel_Index(i), process
    END DO
  END SUBROUTINE CRTM_ChannelInfo_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_n_Channels
!
! PURPOSE:
!       Elemental function to return the number of channels flagged for 
!       processing in a ChannelInfo object.
!
! CALLING SEQUENCE:
!       n_Channels = CRTM_ChannelInfo_n_Channels( ChannelInfo )
!
! OBJECTS:
!       ChannelInfo: ChannelInfo object which is to have its processed 
!                    channels counted.
!                    UNITS:      N/A
!                    TYPE:       TYPE(CRTM_ChannelInfo_type)
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Channels:  The number of channels to be processed in the ChannelInfo
!                    object.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Same as input ChannelInfo argument.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_ChannelInfo_n_Channels( ChannelInfo ) RESULT( n_Channels )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER :: n_Channels
    n_Channels = COUNT(ChannelInfo%Process_Channel)
  END FUNCTION CRTM_ChannelInfo_n_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_Channels
!
! PURPOSE:
!       Pure function to return the list of channels to be processed in a
!       ChannelInfo object.
!
! CALLING SEQUENCE:
!       Channels = CRTM_ChannelInfo_Channels( ChannelInfo )
!
! OBJECTS:
!       ChannelInfo: ChannelInfo object which is to have its channel list queried.
!                    UNITS:      N/A
!                    TYPE:       TYPE(CRTM_ChannelInfo_type)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Channels:    The list of channels to be processed in the ChannelInfo
!                    object.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION CRTM_ChannelInfo_Channels( ChannelInfo ) RESULT( Channels )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER :: Channels(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    Channels = PACK(ChannelInfo%Sensor_Channel, MASK=ChannelInfo%Process_Channel)
  END FUNCTION CRTM_ChannelInfo_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_Subset
!
! PURPOSE:
!       Function to specify a channel subset for processing in the CRTM.
!       By default, ALL channels are processed. This function allows the
!       list of channels that are to be processed to be altered.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_ChannelInfo_Subset( ChannelInfo   , &
!                                               Channel_Subset, &
!                                               Reset           )
!
! OBJECTS:
!       ChannelInfo:    Valid ChannelInfo object for which a channel subset is
!                       to be specified.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_ChannelInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Channel_Subset: An integer array containing the subset list of channels.
!                       Future calls to the CRTM main functions using the passed
!                       ChannelInfo object will process ONLY the channels
!                       specified in this list.
!                       *** NOTE: This argument is ignored if the Reset optional
!                       *** argument is specified with a .TRUE. value.            
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Reset:          Logical flag to reset the ChannelInfo object channel
!                       processing subset to ALL channels.
!                       If == .TRUE.  Future calls to the CRTM main functions using
!                                     the passed ChannelInfo object will process
!                                     ALL the channels
!                          == .FALSE. Procedure execution is equivalent to the Reset
!                                     argument not being specified at all.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the channel subset setting was sucessful
!                          == FAILURE an error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       - The ChannelInfo object can be modified by this procedure.
!       - An error in this procedure will DISABLE processing for ALL channels.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_ChannelInfo_Subset( &
    ChannelInfo   , &  ! In/output
    Channel_Subset, &  ! Optional input
    Reset         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_ChannelInfo_type), INTENT(IN OUT) :: ChannelInfo
    INTEGER,           OPTIONAL, INTENT(IN)     :: Channel_Subset(:)
    LOGICAL,           OPTIONAL, INTENT(IN)     :: Reset
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_ChannelInfo_Subset'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat
    INTEGER :: i, j, n
    INTEGER :: channel_idx(ChannelInfo%n_Channels)
    INTEGER, ALLOCATABLE :: subset_idx(:)
    
    ! Setup
    err_stat = SUCCESS
    
    ! Process Reset option first
    IF ( PRESENT(Reset) ) THEN
      IF ( Reset ) THEN
        ChannelInfo%Process_Channel = .TRUE.
        RETURN
      END IF
    END IF
    
    ! Process channel list
    IF ( PRESENT(Channel_Subset) ) THEN
      n = SIZE(Channel_Subset)
      ! Default: turn off all processing
      ChannelInfo%Process_Channel = .FALSE.
      ! No channels specified
      IF ( n == 0 ) RETURN
      ! Too many specified
      IF ( n > ChannelInfo%n_Channels ) THEN
        err_stat = FAILURE
        msg = 'Specified Channel_Subset contains too many channels!'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
      ! Invalid channels specified
      IF ( ANY(Channel_Subset < MINVAL(ChannelInfo%Sensor_Channel)) .OR. &
           ANY(Channel_Subset > MAXVAL(ChannelInfo%Sensor_Channel)) ) THEN
        err_stat = FAILURE
        msg = 'Specified Channel_Subset contains invalid channels!'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
      ! Allocate subset index array
      ALLOCATE( subset_idx(n),STAT=alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        err_stat = FAILURE
        msg = 'Error allocating subset_idx array'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
      ! Turn on processing for selected channels
      CALL InsertionSort( ChannelInfo%Sensor_Channel, channel_idx )
      CALL InsertionSort( Channel_Subset, subset_idx )
      j = 1
      Channel_Loop: DO i = 1, ChannelInfo%n_Channels
        IF ( Channel_Subset(subset_idx(j)) == ChannelInfo%Sensor_Channel(channel_idx(i)) ) THEN
          ChannelInfo%Process_Channel(channel_idx(i)) = .TRUE.
          j = j + 1
          IF ( j > n ) EXIT Channel_Loop
        END IF
      END DO Channel_Loop
      ! Clean up
      DEALLOCATE( subset_idx )
    END IF

  END FUNCTION CRTM_ChannelInfo_Subset
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_ChannelInfo_DefineVersion( Id )
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

  SUBROUTINE CRTM_ChannelInfo_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_ChannelInfo_DefineVersion


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
!       CRTM_ChannelInfo_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM ChannelInfo objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_ChannelInfo_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM ChannelInfo objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CRTM_ChannelInfo_type)
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

  ELEMENTAL FUNCTION CRTM_ChannelInfo_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_ChannelInfo_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. CRTM_ChannelInfo_Associated(x)) .OR. &
         (.NOT. CRTM_ChannelInfo_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( x%n_Channels /= y%n_Channels ) RETURN
    ! ...Data
    IF ( (x%Sensor_ID        == y%Sensor_ID       ) .AND. &
         (x%Sensor_Type      == y%Sensor_Type     ) .AND. &
         (x%WMO_Satellite_ID == y%WMO_Satellite_ID) .AND. &
         (x%WMO_Sensor_ID    == y%WMO_Sensor_ID   ) .AND. &
         (x%Sensor_Index     == y%Sensor_Index    ) .AND. &
         ALL(x%Process_Channel .EQV. y%Process_Channel) .AND. &
         ALL(x%Sensor_Channel   ==   y%Sensor_Channel ) .AND. &
         ALL(x%Channel_Index    ==   y%Channel_Index  )       ) &
      is_equal = .TRUE.
                       
  END FUNCTION CRTM_ChannelInfo_Equal
 
END MODULE CRTM_ChannelInfo_Define
