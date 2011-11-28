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
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Type definition
  PUBLIC :: CRTM_SfcOptics_type
  ! Definition functions
  PUBLIC :: CRTM_Associated_SfcOptics
  PUBLIC :: CRTM_Destroy_SfcOptics
  PUBLIC :: CRTM_Allocate_SfcOptics
  PUBLIC :: CRTM_Assign_SfcOptics


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_SfcOptics_Define.f90 5959 2009-12-07 14:07:01Z paul.vandelst@noaa.gov $'

  
  ! -----------------------------------
  ! Surface optics data type definition
  ! -----------------------------------
  TYPE :: CRTM_SfcOptics_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Angles = 0 ! I
    INTEGER :: n_Stokes = 0 ! Ls
    ! Flag to determine if the sfc optics require calculation.
    ! Default is to compute the sfc optics.
    INTEGER :: Compute_Switch = SET
    ! Mandatory members
    INTEGER  :: Index_Sat_Ang       = 1
    INTEGER  :: mth_Azi       = 0
    REAL(fp) :: Surface_Temperature = ZERO    
    REAL(fp), DIMENSION(:),       POINTER :: Angle               => NULL() ! I
    REAL(fp), DIMENSION(:),       POINTER :: Weight              => NULL() ! I
    REAL(fp), DIMENSION(:,:),     POINTER :: Emissivity          => NULL() ! I x Ls
    REAL(fp), DIMENSION(:,:,:,:), POINTER :: Reflectivity        => NULL() ! I x Ls x I x Ls
    REAL(fp), DIMENSION(:,:),     POINTER :: Direct_Reflectivity => NULL() ! I x Ls
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


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Clear_SfcOptics
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_SfcOptics structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_SfcOptics( SfcOptics ) ! Output
!
! OUTPUT ARGUMENTS:
!       SfcOptics:   CRTM_SfcOptics structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SfcOptics_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_SfcOptics( SfcOptics )
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
    SfcOptics%n_Angles = 0
    SfcOptics%n_Stokes = 0
    SfcOptics%Compute_Switch = SET
    SfcOptics%Index_Sat_Ang = 1
    SfcOptics%Surface_Temperature = ZERO
  END SUBROUTINE CRTM_Clear_SfcOptics





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Associated_SfcOptics
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       SfcOptics structure.
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_SfcOptics( SfcOptics,          &  ! Input
!                                                       ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       SfcOptics:           CRTM_SfcOptics structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_SfcOptics_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_SfcOptics structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the CRTM_SfcOptics pointer members.
!                            .TRUE.  - if ALL the CRTM_SfcOptics pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CRTM_SfcOptics pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CRTM_SfcOptics pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_SfcOptics( SfcOptics, & ! Input
                                      ANY_Test ) & ! Optional input
                                    RESULT( Association_Status )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN) :: SfcOptics
    INTEGER,         OPTIONAL, INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test


    ! ------
    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! ---------------------------
    ! Test the association status
    ! ---------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( SfcOptics%Angle               ) .AND. &
           ASSOCIATED( SfcOptics%Weight              ) .AND. &
           ASSOCIATED( SfcOptics%Emissivity          ) .AND. &
           ASSOCIATED( SfcOptics%Reflectivity        ) .AND. &
           ASSOCIATED( SfcOptics%Direct_Reflectivity )       ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( SfcOptics%Angle               ) .OR. &
           ASSOCIATED( SfcOptics%Weight              ) .OR. &
           ASSOCIATED( SfcOptics%Emissivity          ) .OR. &
           ASSOCIATED( SfcOptics%Reflectivity        ) .OR. &
           ASSOCIATED( SfcOptics%Direct_Reflectivity )      ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
  END FUNCTION CRTM_Associated_SfcOptics


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_SfcOptics
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_SfcOptics data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_SfcOptics( SfcOptics,                &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics:    Re-initialized CRTM_SfcOptics structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SfcOptics_type
!                     DIMENSION:  Scalar OR Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_SfcOptics( SfcOptics,    &  ! Output
                                   No_Clear,     &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
    INTEGER,         OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_SfcOptics'
    ! Local variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------
    IF ( Clear ) CALL CRTM_Clear_SfcOptics( SfcOptics )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------
    IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! Deallocate the CRTM_SfcOptics Angle
    IF ( ASSOCIATED( SfcOptics%Angle ) ) THEN
      DEALLOCATE( SfcOptics%Angle, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Angle ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the CRTM_SfcOptics Weight 
    IF ( ASSOCIATED( SfcOptics%Weight ) ) THEN
      DEALLOCATE( SfcOptics%Weight, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Weight ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the CRTM_SfcOptics Emissivity
    IF ( ASSOCIATED( SfcOptics%Emissivity ) ) THEN
      DEALLOCATE( SfcOptics%Emissivity, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Emissivity ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the CRTM_SfcOptics Reflectivity
    IF ( ASSOCIATED( SfcOptics%Reflectivity ) ) THEN
      DEALLOCATE( SfcOptics%Reflectivity, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Reflectivity ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the CRTM_SfcOptics Direct_Reflectivity
    IF ( ASSOCIATED( SfcOptics%Direct_Reflectivity ) ) THEN
      DEALLOCATE( SfcOptics%Direct_Reflectivity, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Direct_Reflectivity ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -------------------------------------
    ! Decrement and test allocation counter
    ! -------------------------------------

    SfcOptics%n_Allocates = SfcOptics%n_Allocates - 1
    IF ( SfcOptics%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SfcOptics%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_SfcOptics


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_SfcOptics
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_SfcOptics
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_SfcOptics( n_Angles,                 &  ! Input
!                                               n_Stokes,                 &  ! Input
!                                               SfcOptics,                &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Angles:            Number of angles for which surface optical
!                            data are represented.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       n_Stokes:            Number of Stokes parameters used to represent the
!                            propagating radiation.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics:           CRTM_SfcOptics structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_SfcOptics_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_SfcOptics( n_Angles,     &  ! Input
                                    n_Stokes,     &  ! Input
                                    SfcOptics,    &  ! Output
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT( Error_Status )
    ! Arguments
    INTEGER,                   INTENT(IN)     :: n_Angles
    INTEGER,                   INTENT(IN)     :: n_Stokes
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_SfcOptics'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Dimensions
    IF ( n_Angles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Angles must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Stokes < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Stokes must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( CRTM_Associated_SfcOptics( SfcOptics, ANY_Test = SET ) ) THEN
      Error_Status = CRTM_Destroy_SfcOptics( SfcOptics, &
                                             No_Clear = SET, &
                                             Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_SfcOptics pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    ALLOCATE( SfcOptics%Angle( n_Angles ), SfcOptics%Weight( n_Angles ), &
              SfcOptics%Emissivity( n_Angles, n_Stokes ), &
              SfcOptics%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
              SfcOptics%Direct_Reflectivity( n_Angles, n_Stokes ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SfcOptics data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------------
    ! Assign dimensions and initialise variables
    ! ------------------------------------------
    SfcOptics%n_Angles = n_Angles
    SfcOptics%n_Stokes = n_Stokes
    SfcOptics%Angle               = ZERO
    SfcOptics%Weight              = ZERO
    SfcOptics%Emissivity          = ZERO
    SfcOptics%Reflectivity        = ZERO
    SfcOptics%Direct_Reflectivity = ZERO


    ! -----------------------------------------
    ! Increment and test the allocation counter
    ! -----------------------------------------
    SfcOptics%n_Allocates = SfcOptics%n_Allocates + 1
    IF ( SfcOptics%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SfcOptics%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF
  END FUNCTION CRTM_Allocate_SfcOptics


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Assign_SfcOptics
!
! PURPOSE:
!       Function to copy valid CRTM_SfcOptics structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_SfcOptics( SfcOptics_in,             &  ! Input
!                                             SfcOptics_out,            &  ! Output
!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SfcOptics_in:    CRTM_SfcOptics structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics_out:   Copy of the input structure, CRTM_SfcOptics_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_SfcOptics( SfcOptics_in,  &  ! Input
                                  SfcOptics_out, &  ! Output
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN)     :: SfcOptics_in
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_out
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_SfcOptics'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------
    IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics_In ) ) THEN
      Error_Status = CRTM_Destroy_SfcOptics( SfcOptics_Out, &
                                             Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_SfcOptics pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    Error_Status = CRTM_Allocate_SfcOptics( SfcOptics_in%n_Angles, &
                                            SfcOptics_in%n_Stokes, &
                                            SfcOptics_out, &
                                            Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output SfcOptics arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Assign scalar data
    ! ------------------
    SfcOptics_out%Compute_Switch      = SfcOptics_in%Compute_Switch
    SfcOptics_out%Index_Sat_Ang       = SfcOptics_in%Index_Sat_Ang
    SfcOptics_out%Surface_Temperature = SfcOptics_in%Surface_Temperature


    ! -----------------
    ! Assign array data
    ! -----------------
    SfcOptics_out%Angle               = SfcOptics_in%Angle
    SfcOptics_out%Weight              = SfcOptics_in%Weight
    SfcOptics_out%Emissivity          = SfcOptics_in%Emissivity
    SfcOptics_out%Reflectivity        = SfcOptics_in%Reflectivity
    SfcOptics_out%Direct_Reflectivity = SfcOptics_in%Direct_Reflectivity 
  END FUNCTION CRTM_Assign_SfcOptics

END MODULE CRTM_SfcOptics_Define
