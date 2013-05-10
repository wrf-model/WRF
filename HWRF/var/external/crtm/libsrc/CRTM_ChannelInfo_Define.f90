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
  ! Module use
  USE Type_Kinds
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE CRTM_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                             INVALID_WMO_SENSOR_ID   , &
                             SET, STRLEN
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

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_ChannelInfo_Equal
  END INTERFACE OPERATOR(==)
  
  INTERFACE CRTM_ChannelInfo_n_Channels
    MODULE PROCEDURE n_Channels_Scalar
    MODULE PROCEDURE n_Channels_Rank1
  END INTERFACE CRTM_ChannelInfo_n_Channels
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_ChannelInfo_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'


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
    INTEGER           :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER           :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER           :: Sensor_Index     = 0
    ! Array data
    INTEGER, ALLOCATABLE :: Sensor_Channel(:)  ! L
    INTEGER, ALLOCATABLE :: Channel_Index(:)   ! L
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
!                       DIMENSION:  Scalar OR any rank
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
!                          DIMENSION:  Scalar
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
    ALLOCATE( ChannelInfo%Sensor_Channel( n_Channels ), &
              ChannelInfo%Channel_Index( n_Channels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    ChannelInfo%n_Channels = n_Channels
    ! ...Arrays
    ChannelInfo%Sensor_Channel = 0
    ChannelInfo%Channel_Index  = 0


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
!       CALL CRTM_ChannelInfo_Inspect( ChannelInfo )
!
! INPUTS:
!       ChannelInfo:   ChannelInfo object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CRTM_ChannelInfo_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_ChannelInfo_Inspect( ChannelInfo )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    WRITE(*,'(1x,"ChannelInfo OBJECT")')
    WRITE(*,'(3x,"n_Channels       :",1x,i0)') ChannelInfo%n_Channels
    WRITE(*,'(3x,"Sensor Id        :",1x,a )') TRIM(ChannelInfo%Sensor_ID)
    WRITE(*,'(3x,"WMO_Satellite_ID :",1x,i0)') ChannelInfo%WMO_Satellite_ID
    WRITE(*,'(3x,"WMO_Sensor_ID    :",1x,i0)') ChannelInfo%WMO_Sensor_ID   
    WRITE(*,'(3x,"Sensor_Index     :",1x,i0)') ChannelInfo%Sensor_Index    
    IF ( .NOT. CRTM_ChannelInfo_Associated(ChannelInfo) ) RETURN
    WRITE(*,'(3x,"ChannelInfo Sensor Channel:")')
    WRITE(*,'(5(1x,i5,:))') ChannelInfo%Sensor_Channel
    WRITE(*,'(3x,"ChannelInfo Channel Index:")')
    WRITE(*,'(5(1x,i5,:))') ChannelInfo%Channel_Index
  END SUBROUTINE CRTM_ChannelInfo_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_ChannelInfo_n_Channels
!
! PURPOSE:
!       Function to return the number of channels defined in a ChannelInfo
!       structure or structure array
!
! CALLING SEQUENCE:
!       n_Channels = CRTM_ChannelInfo_n_Channels( ChannelInfo )
!
! INPUTS:
!       ChannelInfo: ChannelInfo structure or structure which is to have its
!                    channels counted.
!                    UNITS:      N/A
!                    TYPE:       TYPE(CRTM_ChannelInfo_type)
!                    DIMENSION:  Scalar
!                                  or
!                                Rank-1
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Channels:   The number of defined channels in the input argument.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION n_Channels_Scalar( ChannelInfo ) RESULT( n_Channels )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo
    INTEGER :: n_Channels
    n_Channels = ChannelInfo%n_Channels
  END FUNCTION n_Channels_Scalar
  
  FUNCTION n_Channels_Rank1( ChannelInfo ) RESULT( n_Channels )
    TYPE(CRTM_ChannelInfo_type), INTENT(IN) :: ChannelInfo(:) ! N
    INTEGER :: n_Channels
    n_Channels = SUM(ChannelInfo%n_Channels)
  END FUNCTION n_Channels_Rank1


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
         (x%WMO_Satellite_ID == y%WMO_Satellite_ID) .AND. &
         (x%WMO_Sensor_ID    == y%WMO_Sensor_ID   ) .AND. &
         (x%Sensor_Index     == y%Sensor_Index    ) .AND. &
         ALL(x%Sensor_Channel == y%Sensor_Channel) .AND. &
         ALL(x%Channel_Index  == y%Channel_Index )       ) &
      is_equal = .TRUE.
                       
  END FUNCTION CRTM_ChannelInfo_Equal
 
END MODULE CRTM_ChannelInfo_Define
