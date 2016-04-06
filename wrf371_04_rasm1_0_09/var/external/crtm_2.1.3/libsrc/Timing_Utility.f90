! Utility to define a timing structure and
! timing utility routines.
!
MODULE Timing_Utility

  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: INFORMATION, FAILURE, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE


  ! Visibilities
  PRIVATE
  ! ...Datatypes
  PUBLIC :: Timing_type
  ! ...Procedures
  PUBLIC :: Timing_Begin
  PUBLIC :: Timing_End
  PUBLIC :: Timing_Display
  PUBLIC :: Timing_Inspect
  PUBLIC :: Timing_Set
  PUBLIC :: Timing_Get
  ! ...Old named procedures
  PUBLIC :: Begin_Timing
  PUBLIC :: End_Timing
  PUBLIC :: Display_Timing


  ! Parameters
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Timing_Utility.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


  ! Overloads
  INTERFACE Begin_Timing
    MODULE PROCEDURE Timing_Begin
  END INTERFACE Begin_Timing
  
  INTERFACE End_Timing
    MODULE PROCEDURE Timing_End
  END INTERFACE End_Timing
  
  INTERFACE Display_Timing
    MODULE PROCEDURE Timing_Display
  END INTERFACE Display_Timing
  
  
  ! Derived type definitions
  !:tdoc+:
  TYPE :: Timing_type
    PRIVATE
    LOGICAL :: Is_Valid = .FALSE.
    INTEGER :: Hertz       = 0
    INTEGER :: Begin_Clock = 0
    INTEGER :: End_Clock   = 0
  END TYPE Timing_type
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
!   Timing_Begin
!
! PURPOSE:
!   Subroutine to set the begin time count in a timing object
!
! CALLING SEQUENCE:
!   CALL Timing_Begin( timing )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Begin( self )  ! In/Output
    TYPE(Timing_type), INTENT(OUT) :: self
    CALL SYSTEM_CLOCK( COUNT_RATE=self%Hertz, &
                       COUNT     =self%Begin_Clock )
    IF ( self%Hertz == 0 ) RETURN
    self%Is_Valid = .TRUE.
  END SUBROUTINE Timing_Begin


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_End
!
! PURPOSE:
!   Subroutine to set the end time count in a timing object
!
! CALLING SEQUENCE:
!   CALL Timing_End( timing )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_End( self )  ! In/Output
    TYPE(Timing_type), INTENT(IN OUT) :: self
    CALL SYSTEM_CLOCK( COUNT=self%End_Clock )
  END SUBROUTINE Timing_End


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Display
!
! PURPOSE:
!   Subroutine to display the elapsed time defined by the begin and end time
!   counts in the timing object.
!
! CALLING SEQUENCE:
!   CALL Timing_Display( timing, Caller = caller )
!
! INPUTS:
!   timing:  Timing object.
!            *** OBJECT is destroyed upon exit ***
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!   caller:  String containing the name of the calling routine.
!            If not specified, the name of this procedure is used.
!            UNITS:      N/A
!            TYPE:       CHARACTER(*)
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Display( self  , &  ! Input
                             Caller  )  ! Optional input
    ! Arguments
    TYPE(Timing_type),      INTENT(IN OUT) :: self
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Caller
    ! Local parameters
    REAL(fp), PARAMETER :: N_SECONDS_IN_HOUR        = 3600.0_fp
    REAL(fp), PARAMETER :: N_SECONDS_IN_MINUTE      =   60.0_fp
    REAL(fp), PARAMETER :: N_MILLISECONDS_IN_SECOND = 1000.0_fp
    ! Local variables
    CHARACTER(256) :: Routine_Name
    CHARACTER(256) :: Elapsed_Time
    REAL(fp)       :: Total_Time
    INTEGER        :: n_Hours
    INTEGER        :: n_Minutes
    INTEGER        :: n_Seconds
    INTEGER        :: n_milliSeconds

    ! Set up
    Routine_Name = 'Timing_Display'
    IF ( PRESENT(Caller) ) Routine_Name = TRIM(ADJUSTL(Caller))
    ! ...Check if timing structure valid for display
    IF ( .NOT. self%Is_Valid ) THEN
      CALL Display_Message( TRIM(Routine_Name), &
                            '***** Invalid timing structure! *****', &
                            FAILURE )
      RETURN
    END IF

    ! Compute the total time in seconds
    Total_Time = REAL(self%End_Clock - self%Begin_Clock, fp) / REAL(self%Hertz, fp)

    ! Split the total time into hours, minutes, seconds, and millseconds
    n_Hours        = INT(Total_Time / N_SECONDS_IN_HOUR)
    n_Minutes      = INT(MOD(Total_Time,N_SECONDS_IN_HOUR) / N_SECONDS_IN_MINUTE)
    n_Seconds      = INT(MOD(MOD(Total_Time,N_SECONDS_IN_HOUR), N_SECONDS_IN_MINUTE))
    n_milliSeconds = INT((Total_Time - AINT(Total_Time,fp)) * N_MILLISECONDS_IN_SECOND)

    ! Construct the character string
    WRITE( Elapsed_Time, '("Elapsed time-- ",i2.2,":",i2.2,":",i2.2,".",i3.3 )' ) &
                         n_Hours, n_Minutes, n_Seconds, n_milliSeconds
    CALL Display_Message( TRIM(Routine_Name), &
                          TRIM(Elapsed_Time), &
                          INFORMATION )
    
    ! Destroy the timing information
    CALL Timing_Destroy(self)
    
  END SUBROUTINE Timing_Display


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Inspect
!
! PURPOSE:
!   Subroutine to print the contents of a Timing object to stdout.
!
! CALLING SEQUENCE:
!   CALL Timing_Inspect( timing )
!
! OBJECTS:
!   Timing:  Timing object to display.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Inspect( self )
    TYPE(Timing_type), INTENT(IN) :: self
    WRITE(*,'(1x,"Timing OBJECT")')
    WRITE(*,'(3x,"Hertz       : ",i0)') self%Hertz      
    WRITE(*,'(3x,"Begin_Clock : ",i0)') self%Begin_Clock
    WRITE(*,'(3x,"End_Clock   : ",i0)') self%End_Clock  
    WRITE(*,'(3x,"Is_Valid    : ",l1)') self%Is_Valid   
  END SUBROUTINE Timing_Inspect
  
  
  ! Subroutine to set the components of a timing object
  ! Public, but only for testing so it's undocumented.
  SUBROUTINE Timing_Set( &
    self       , &
    Hertz      , &
    Begin_Clock, &
    End_Clock  , &
    Is_Valid     )
    ! Arguments
    TYPE(Timing_type), INTENT(IN OUT) :: self
    INTEGER, OPTIONAL, INTENT(IN)     :: Hertz
    INTEGER, OPTIONAL, INTENT(IN)     :: Begin_Clock
    INTEGER, OPTIONAL, INTENT(IN)     :: End_Clock
    LOGICAL, OPTIONAL, INTENT(IN)     :: Is_Valid
    ! Set object components
    IF ( PRESENT(Hertz      ) ) self%Hertz       = Hertz
    IF ( PRESENT(Begin_Clock) ) self%Begin_Clock = Begin_Clock
    IF ( PRESENT(End_Clock  ) ) self%End_Clock   = End_Clock
    IF ( PRESENT(Is_Valid   ) ) self%Is_Valid    = Is_Valid
  END SUBROUTINE Timing_Set

    
  ! Subroutine to get the components of a timing object
  ! Public, but only for testing so it's undocumented.
  SUBROUTINE Timing_Get( &
    self       , &
    Hertz      , &
    Begin_Clock, &
    End_Clock  , &
    Is_Valid     )
    ! Arguments
    TYPE(Timing_type), INTENT(IN)  :: self
    INTEGER, OPTIONAL, INTENT(OUT) :: Hertz
    INTEGER, OPTIONAL, INTENT(OUT) :: Begin_Clock
    INTEGER, OPTIONAL, INTENT(OUT) :: End_Clock
    LOGICAL, OPTIONAL, INTENT(OUT) :: Is_Valid
    ! Set object components
    IF ( PRESENT(Hertz      ) ) Hertz       = self%Hertz
    IF ( PRESENT(Begin_Clock) ) Begin_Clock = self%Begin_Clock
    IF ( PRESENT(End_Clock  ) ) End_Clock   = self%End_Clock
    IF ( PRESENT(Is_Valid   ) ) Is_Valid    = self%Is_Valid
  END SUBROUTINE Timing_Get


    
!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! Subroutine to reinitialise a timing object
  SUBROUTINE Timing_Destroy(self)
    TYPE(Timing_type), INTENT(OUT) :: self
    self%Is_Valid = .FALSE.
  END SUBROUTINE Timing_Destroy

END MODULE Timing_Utility
