!
! ODPS_Predictor_Define
!
! Module defining the Predictor data structure for the ODPS algorithm and 
! containing routines to manipulate it.
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA, NOAA/NESDIS 20-Jun-2008
!                       based on the content of CRTM_Predictor_Define.f90
!

MODULE ODPS_Predictor_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: fp, Single
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Predictor data structure definition
  PUBLIC :: Predictor_type
  ! Structure type for variable to hold predictor and absorption 
  ! forward variables across FWD, TL and AD calls.
  PUBLIC :: PAFV_type
  ! Structure procedures
  PUBLIC :: Associated_Predictor
  PUBLIC :: Destroy_Predictor
  PUBLIC :: Allocate_Predictor
  PUBLIC :: Zero_Predictor
  PUBLIC :: Destroy_PAFV
  PUBLIC :: Allocate_PAFV
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODPS_Predictor_Define.f90 1818 2008-07-1 yong.han@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256

  INTEGER, PARAMETER ::  ZERO = 0.0_fp
  INTEGER, PARAMETER ::  SET = 1

  ! C-OPTRAN max. order and number of predictors
  INTEGER, PUBLIC, PARAMETER :: MAX_OPTRAN_ORDER = 10
  INTEGER, PUBLIC, PARAMETER :: MAX_OPTRAN_PREDICTORS = 14
  INTEGER, PUBLIC, PARAMETER :: MAX_OPTRAN_USED_PREDICTORS = 6

  !-------------------------------------------------------
  ! Structure to hold predictor and absorption forward
  ! variables across FWD, TL and AD calls 
  !-------------------------------------------------------
  TYPE :: PAFV_type

    ! This flag indicates if this structure is activated.
    LOGICAL :: Active = .FALSE.

    ! Dimension variables
    INTEGER :: n_ODPS_Layers = 0   ! K
    INTEGER :: n_Absorbers   = 0   ! J
    INTEGER :: n_User_Layers = 0   ! uK
    
    INTEGER :: n_OUsed_Pred  = MAX_OPTRAN_USED_PREDICTORS   ! oI  --- # of OPTRAN used predictors 

    !----------------------
    ! ODPS FW variables
    !----------------------    
    ! Index array for ODPS to user profile interpolations
!    INTEGER,  DIMENSION(:,:), POINTER :: ODPS2User_Idx => NULL() ! 2 x uK
    INTEGER,  DIMENSION(:,:), POINTER :: ODPS2User_Idx => NULL() ! 2 x 0:uK
    ! accumulated weighting factors array for ODPS to user profile interpolations
!    REAL(fp), DIMENSION(:,:), POINTER :: Acc_Weighting_OD => NULL() ! K x uK
    
    ! Index array for user to ODPS profile interpolations
    INTEGER,  DIMENSION(:,:), POINTER :: interp_index  => NULL() ! 2 x K
    ! accumulated weighting factors array for user to ODPS profile interpolations
    REAL(fp), DIMENSION(:,:), POINTER :: Acc_Weighting => NULL() ! uK x K
     
    REAL(fp), DIMENSION(:),   POINTER :: Temperature   => NULL()  ! K
    REAL(fp), DIMENSION(:,:), POINTER :: Absorber      => NULL()  ! K
    INTEGER,  DIMENSION(:),   POINTER :: idx_map       => NULL()  ! K
    INTEGER  :: H2O_idx = 0
 
    ! pressure profiles for interpolations   
    REAL(fp), DIMENSION(:),   POINTER :: Ref_LnPressure  => NULL()  ! K
    REAL(fp), DIMENSION(:),   POINTER :: User_LnPressure => NULL()  ! uK
        
    ! Predictor FW variables
    REAL(fp), DIMENSION(:),   POINTER :: PDP => NULL(),    &  ! K
                                         Tz_ref => NULL(), &
                                         Tz => NULL(),     &
                                         Tzp_ref => NULL(),&
                                         Tzp => NULL()  
    REAL(fp), DIMENSION(:,:), POINTER :: GAz_ref => NULL(),  &  ! K x J
                                         GAz_sum => NULL(),  &
                                         GAz => NULL(),      &
                                         GAzp_ref => NULL(), &
                                         GAzp_sum => NULL(), &
                                         GAzp => NULL(),     &
                                         GATzp_ref => NULL(),&
                                         GATzp_sum => NULL(),&
                                         GATzp => NULL()  

    REAL(fp), DIMENSION(:),   POINTER :: DT          => NULL(),    &  ! K
                                         T           => NULL(),    &
                                         T2          => NULL(),    & 
                                         DT2         => NULL(),    & 
                                         H2O         => NULL(),    & 
                                         H2O_A       => NULL(),    & 
                                         H2O_R       => NULL(),    & 
                                         H2O_S       => NULL(),    & 
                                         H2O_R4      => NULL(),    & 
                                         H2OdH2OTzp  => NULL(),    & 
                                         CO2         => NULL(),    & 
                                         O3          => NULL(),    & 
                                         O3_A        => NULL(),    & 
                                         O3_R        => NULL(),    & 
                                         CO          => NULL(),    & 
                                         CO_A        => NULL(),    & 
                                         CO_R        => NULL(),    & 
                                         CO_S        => NULL(),    & 
                                         CO_ACOdCOzp => NULL(),    & 
                                         N2O         => NULL(),    &
                                         N2O_A       => NULL(),    & 
                                         N2O_R       => NULL(),    & 
                                         N2O_S       => NULL(),    & 
                                         CH4         => NULL(),    & 
                                         CH4_A       => NULL(),    & 
                                         CH4_R       => NULL(),    & 
                                         CH4_ACH4zp

    ! OD FW variables    
    REAL(fp), DIMENSION(:),   POINTER :: OD          => NULL(),    &  ! K 
                                         OD_Path     => NULL()
                                         
    ! Zeeman specific FW variables
    REAL(fp) :: w1, w2  ! weights for two-points linear interpolation
    INTEGER  :: inode   ! node position 
    
    !----------------------
    ! OPTRAN FW variables
    !----------------------
    LOGICAL :: OPTRAN = .FALSE.
    !--- variables for predictor calculations
    REAL(fp), DIMENSION(:),   POINTER :: dPonG => NULL(),      &   ! K
                                         d_Absorber => NULL(), &
                                         Int_vapor => NULL(),  &
                                         AveA => NULL(),       &
                                         Inverse => NULL(),    &
                                         s_t => NULL(),        &
                                         s_p => NULL(),        &
                                         Ap1 => NULL()
    !--- variables for OD calculations   
    REAL(fp), DIMENSION(:,:), POINTER :: b      => NULL()  ! K x 0:oI 
    REAL(fp), DIMENSION(:),   POINTER :: LN_Chi => NULL()  ! K
    REAL(fp), DIMENSION(:),   POINTER :: Chi    => NULL()  ! K
    
  END TYPE PAFV_type


  TYPE :: Predictor_type

    INTEGER :: n_Allocates = 0

    ! Dimension
    INTEGER :: Max_n_Layers = 0 ! K   - maximum number of layers
    INTEGER :: n_Components = 0 ! J   - number of tau components
    INTEGER :: n_Predictors = 0 ! I   - predictor dimension (Maximum)

    INTEGER :: n_Layers     = 0 ! K  - actual number of layers

    ! Secant Zenith angle
    REAL(fp), DIMENSION(:), POINTER ::  Secant_Zenith => NULL()  ! K
    ! Tau component ID
    INTEGER, DIMENSION(:),  POINTER ::  Component_ID  => NULL()  ! J
    ! Actual number of component predictors for each component 
    INTEGER,      DIMENSION(:),     POINTER ::  n_CP  => NULL()  ! J
    ! Predictor array
    REAL(fp), DIMENSION(:,:,:),     POINTER ::  X     => NULL()  ! K x I x J 

    ! For use of optical path profile interpolation
    INTEGER :: n_User_Layers = 0 ! Ku - number of layers of user profile
    REAL(fp), DIMENSION(:), POINTER :: Ref_Level_LnPressure  => NULL() ! 0:K 
    REAL(fp), DIMENSION(:), POINTER :: User_Level_LnPressure => NULL() ! 0:Ku 
    ! Secant zenith angle at the surface
    REAL(fp) :: Secant_Zenith_Surface
    ! variables used depending on the algorithm
    REAL(fp) :: u, v, w  

    !----------------------------
    ! Optional OPTRAN predictors
    !----------------------------
    ! flag indicates if OPTRAN algorithm is active 
    LOGICAL :: OPTRAN = .FALSE.
    ! Dimension
    INTEGER :: n_OPredictors  = 0 ! OI
    ! polynomial of the water vapor absorber level
    REAL(fp), DIMENSION(:,:), POINTER ::  Ap  => NULL()  ! MAX_OPTRAN_ORDER x K
    ! splant path layer integrated amount
    REAL(fp), DIMENSION(:),   POINTER ::  dA  => NULL()  ! K
    
    ! Predictor array    
    REAL(fp), DIMENSION(:,:), POINTER ::  OX  => NULL()  ! K x OI 

    !------------------------------------------------------
    ! Structure variable to hold predictor and absorption 
    ! forward variables across FWD, TL and AD calls. It 
    ! should be allocated only for the FWD Predictor variable. 
    !------------------------------------------------------
    TYPE(PAFV_type) :: PAFV
    
  END TYPE Predictor_type


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
!       Clear_Predictor
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Predictor structure.
!
! CALLING SEQUENCE:
!       CALL Clear_Predictor( Predictor ) ! Output
!
! OUTPUT ARGUMENTS:
!       Predictor:  Predictor structure for which the scalar
!                   members have been cleared.
!                   UNITS:      N/A
!                   TYPE:       Predictor_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Predictor(Predictor)
    TYPE(Predictor_type), INTENT(IN OUT) :: Predictor
  END SUBROUTINE Clear_Predictor



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_Predictor
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Predictor structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Predictor( Predictor,        &  ! Input
!                                                  ANY_Test=Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Predictor:           Predictor structure which is to have its
!                            pointer member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Predictor structure pointer members are
!                            associated.
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
!       Association_Status:  The return value is a logical value indicating
!                            the association status of the Predictor
!                            pointer members.
!                            .TRUE.  - if ALL the Predictor pointer
!                                      members are associated, or if the
!                                      ANY_Test argument is set and ANY of the
!                                      Predictor pointer members are
!                                      associated.
!                            .FALSE. - some or all of the Predictor
!                                      pointer members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_Predictor( Predictor, &  ! Input
                                 ANY_Test ) &  ! Optional input
                                RESULT(Association_Status)
    ! Arguments
    TYPE(Predictor_type), INTENT(IN)      :: Predictor
    INTEGER, OPTIONAL        , INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test
    INTEGER :: j

    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF

    ! Test the structure associations    
    Association_Status = .FALSE.
    IF (ALL_Test) THEN
      IF ( ASSOCIATED( Predictor%Secant_Zenith ) .AND. &
           ASSOCIATED( Predictor%Component_ID  ) .AND. &
           ASSOCIATED( Predictor%n_CP )          .AND. & 
           ASSOCIATED( Predictor%X   )           .AND. &
           ASSOCIATED( Predictor%Ref_Level_LnPressure )  ) THEN
        Association_Status = .TRUE.
      END IF
      IF( Predictor%n_User_Layers > 0 )THEN
        Association_Status = Association_Status .AND. &
                             ASSOCIATED( Predictor%User_Level_LnPressure )
      END IF
      IF( Predictor%n_OPredictors > 0 )THEN
        Association_Status = Association_Status .AND. &
                             ASSOCIATED( Predictor%OX ) .AND. &
                             ASSOCIATED( Predictor%Ap ) .AND. &
                             ASSOCIATED( Predictor%dA )
      END IF
    ELSE
      IF ( ASSOCIATED( Predictor%Secant_Zenith ) .OR. &
           ASSOCIATED( Predictor%Component_ID  ) .OR. &
           ASSOCIATED( Predictor%n_CP )          .OR. &
           ASSOCIATED( Predictor%X   )           .OR. &
           ASSOCIATED( Predictor%Ref_Level_LnPressure )  ) THEN
        Association_Status = .TRUE.
      END IF
      IF( Predictor%n_User_Layers > 0 )THEN
        Association_Status = Association_Status .OR. &
                             ASSOCIATED( Predictor%User_Level_LnPressure )
      END IF
      IF( Predictor%n_OPredictors > 0 )THEN
        Association_Status = Association_Status .OR. &
                             ASSOCIATED( Predictor%OX ) .OR. &
                             ASSOCIATED( Predictor%Ap ) .OR. &
                             ASSOCIATED( Predictor%dA )
      END IF
    END IF
    
  END FUNCTION Associated_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_Predictor
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a Predictor data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Predictor( Predictor              , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control     
!                                         Message_Log=Message_Log  )  ! Error messaging      
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:      Re-initialized Predictor structure.
!                       UNITS:      N/A
!                       TYPE:       Predictor_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Predictor( Predictor  , &  ! Output
                              No_Clear   , &  ! Optional input
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error messaging
                            RESULT(Error_Status)
    ! Arguments
    TYPE(Predictor_type)     , INTENT(IN OUT) :: Predictor 
    INTEGER,      OPTIONAL   , INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL   , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL   , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    
    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_Predictor(Predictor)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_Predictor(Predictor) ) RETURN

    ! Deallocate the pointer members                                         
    DEALLOCATE( Predictor%Secant_Zenith, &                                   
                Predictor%Component_ID, &                                    
                Predictor%n_CP, &                                            
                Predictor%X, & 
                Predictor%Ref_Level_LnPressure, &
                STAT = Allocate_Status )                                     
    IF ( Allocate_Status /= 0 ) THEN                                         
      WRITE( Message, '( "Error deallocating Predictor. STAT = ", i5 )' ) &  
                      Allocate_Status                                        
      Error_Status = FAILURE                                                 
      CALL Display_Message( &                                                
             ROUTINE_NAME, &                                                 
             TRIM(Message), &                                                
             Error_Status, &                                                 
             Message_Log=Message_Log )                                       
      RETURN                                                                 
    END IF                                                                   

    ! Reset the dimension indicators
    Predictor%Max_n_Layers =0
    Predictor%n_Components =0
    Predictor%n_Predictors =0

    Predictor%n_Layers     =0

    ! Deallocate the User_Level_Pressure array
    IF( Predictor%n_User_Layers > 0 )THEN
      DEALLOCATE( Predictor%User_Level_LnPressure, &                                             
                  STAT = Allocate_Status )                                    
      IF ( Allocate_Status /= 0 ) THEN                                        
        WRITE( Message, '( "Error deallocating the Predictor "//&
            &"User_Level_Pressure pointer member. STAT = ", i5 )' ) & 
                        Allocate_Status                                       
        Error_Status = FAILURE                                                
        CALL Display_Message( &                                               
               ROUTINE_NAME, &                                                
               TRIM(Message), &                                               
               Error_Status, &                                                
               Message_Log=Message_Log )                                      
        RETURN                                                                
      END IF 

      Predictor%n_User_Layers = 0
    END IF                                                                 
      
    ! Deallocate the OPTRAN predictors
    IF( Predictor%OPTRAN )THEN
      DEALLOCATE( Predictor%OX, &
                  Predictor%Ap, &                                  
                  Predictor%dA, &                                  
                  STAT = Allocate_Status )                                    
      IF ( Allocate_Status /= 0 ) THEN                                        
        WRITE( Message, '( "Error deallocating the Predictor OPTRAN component."//&
            &" STAT = ", i5 )' ) & 
                        Allocate_Status                                       
        Error_Status = FAILURE                                                
        CALL Display_Message( &                                               
               ROUTINE_NAME, &                                                
               TRIM(Message), &                                               
               Error_Status, &                                                
               Message_Log=Message_Log )                                      
        RETURN                                                                
      END IF 
      Predictor%OPTRAN = .FALSE.
      Predictor%n_OPredictors = 0
    END IF                                                                 
      
    ! Decrement and test allocation counter
    Predictor%n_Allocates = Predictor%n_Allocates - 1
    IF ( Predictor%n_Allocates /= 0 ) THEN
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Destroy_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_Predictor
! 
! PURPOSE:
!       Function to allocate the pointer members of the Predictor
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Predictor( n_Layers               , &  ! Input
!                                          n_Components           , &  ! Input                
!                                          n_Predictors           , &  ! Input                
!                                          Predictor              , &  ! Output 
!                                          OPTRAN                 , &  ! Optional input              
!                                          ! Array dimension of user pressure coordinates                
!                                          n_User_Layers=n_User_Layers, &  
!                                          RCS_Id     =RCS_Id     , &  ! Revision control     
!                                          Message_Log=Message_Log  )  ! Error messaging      
!
! INPUT ARGUMENTS:
!         n_Layers:          Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_Components:      Number of atmospheric transmittance components
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_Predictors:      Number of maximum absorption predictors.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!         OPTRAN:            OPTRAN flag. If present and its value is .true., 
!                            optran predictors will be allocated.
!                            UNITS:      N/A
!                            TYPE:       Logical
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!      n_User_Layers:        Number of user atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), Optional
!
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
!       Predictor:           Predictor structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       Predictor_type
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
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Predictor( n_Layers     , &  ! Input            
                               n_Components , &  ! Input                 
                               n_Predictors , &  ! Input                 
                               Predictor    , &  ! Output
                               OPTRAN,        &  ! Optional Input
                               n_User_Layers, &  ! Optional Input                
                               RCS_Id       , &  ! Revision control      
                               Message_Log  ) &  ! Error messaging       
                             RESULT( Error_Status )                      
    ! Arguments
    INTEGER                  , INTENT(IN)     :: n_Layers
    INTEGER                  , INTENT(IN)     :: n_Components
    INTEGER                  , INTENT(IN)     :: n_Predictors
    TYPE(Predictor_type)     , INTENT(IN OUT) :: Predictor
    LOGICAL     , OPTIONAL   , INTENT(IN)     :: OPTRAN
    INTEGER     , OPTIONAL   , INTENT(IN)     :: n_User_Layers
    CHARACTER(*), OPTIONAL   , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL   , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( n_Layers     < 1 .OR. &
         n_Components < 1 .OR. &
         n_Predictors < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             'Input Predictor dimensions must all be > 0.', &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_Predictor( Predictor, ANY_Test=1 ) ) THEN
      Error_Status = Destroy_Predictor( &
                       Predictor, &
                       No_Clear=1, &
                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               'Error deallocating Predictor prior to allocation.', &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    
    ! Perform the pointer allocation
    ALLOCATE( Predictor%Secant_Zenith(n_Layers), &
              Predictor%Component_ID(n_Components), &  
              Predictor%n_CP(n_Components), &          
              Predictor%X(n_Layers, n_Predictors, n_Components), & 
              Predictor%Ref_Level_LnPressure(0:n_Layers), &            
              STAT = Allocate_Status )   
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating Predictor data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Assign the dimensions
    Predictor%Max_n_Layers = n_Layers
    Predictor%n_Components = n_Components
    Predictor%n_Predictors = n_Predictors

    ! set the actual number to the maxium number
    Predictor%n_Layers     = n_Layers

    ! Initialise the arrays
    Predictor%Secant_Zenith = ZERO    
    Predictor%Component_ID  = 0
    Predictor%n_CP          = 0
    Predictor%X             = ZERO

    IF( PRESENT(n_User_Layers) )THEN
      IF ( n_User_Layers < 1 )THEN
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               'Input n_User_Layers must be > 0.', &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF
      ALLOCATE( Predictor%User_Level_LnPressure(0:n_User_Layers), &
                STAT = Allocate_Status )   
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating User_Level_Pressure array. STAT = ", i5 )' ) &
                        Allocate_Status
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               TRIM(Message), &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF

      Predictor%n_User_Layers =n_User_Layers

    END IF

    ! OPTRAN component
    IF(PRESENT(OPTRAN))THEN
      IF( OPTRAN )THEN
        ALLOCATE( Predictor%OX(n_Layers, MAX_OPTRAN_PREDICTORS), &
                  Predictor%Ap(n_Layers, MAX_OPTRAN_ORDER), &
                  Predictor%dA(n_Layers), &
                  STAT = Allocate_Status )   
        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message, '( "Error allocating OPTRAN predictor arrays. STAT = ", i5 )' ) &
                          Allocate_Status
          Error_Status = FAILURE
          CALL Display_Message( &
                 ROUTINE_NAME, &
                 TRIM(Message), &
                 Error_Status, &
                 Message_Log=Message_Log )
          RETURN
        END IF
        Predictor%n_OPredictors = MAX_OPTRAN_PREDICTORS
        Predictor%OX = ZERO
        Predictor%Ap = ZERO
        Predictor%dA = ZERO
        Predictor%OPTRAN = .TRUE.
      END IF
    END IF

    ! Increment and test the allocation counter
    Predictor%n_Allocates = Predictor%n_Allocates + 1
    IF ( Predictor%n_Allocates /= 1 ) THEN
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Allocate_Predictor

!--------------------------------------------------------------------------------
!
! NAME:
!       Zero_Predictor
! 
! PURPOSE:
!       Subroutine to zero-out all members of a Predictor structure - both
!       scalar and pointer.
!
! CALLING SEQUENCE:
!       CALL Zero_Predictor( Predictor )
!
! OUTPUT ARGUMENTS:
!       Predictor:    Zeroed out Predictor structure.
!                     UNITS:      N/A
!                     TYPE:       Predictor_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Predictor
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - Note the INTENT on the output Predictor argument is IN OUT rather than
!         just OUT. This is necessary because the argument must be defined upon
!         input.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Predictor( Predictor )  ! Output
    TYPE(Predictor_type),  INTENT(IN OUT) :: Predictor
    ! Reset the array components
    Predictor%Secant_Zenith = ZERO    
    Predictor%Component_ID  = 0
    Predictor%n_CP          = 0
    Predictor%X             = ZERO
    Predictor%Ref_Level_LnPressure = ZERO
    IF(Predictor%n_User_Layers > 0)THEN
      Predictor%User_Level_LnPressure = ZERO
    END IF
    IF(Predictor%OPTRAN)THEN
      Predictor%OX = ZERO
      Predictor%Ap = ZERO
      Predictor%dA = ZERO
    END IF
  END SUBROUTINE Zero_Predictor

  !----------------------------------------------------------
  ! Routing to deallocate the PAFV structure pointer members
  !----------------------------------------------------------
  FUNCTION Destroy_PAFV( PAFV        , &  ! Output
                         RCS_Id     , &  ! Revision control     
                         Message_Log) &  ! Error messaging      
                       RESULT(Error_Status)                     
    ! Arguments
    TYPE(PAFV_type)          , INTENT(IN OUT) :: PAFV 
    CHARACTER(*), OPTIONAL   , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL   , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_PAFV'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status(3)
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check if the PAFV is set
    IF ( .NOT. PAFV%Active ) RETURN

    ! Deallocate the pointer members                                         
    DEALLOCATE( PAFV%ODPS2User_Idx, &
!                PAFV%Acc_Weighting_OD, &
                PAFV%interp_index,  &
                PAFV%Acc_Weighting, &
                PAFV%Temperature, &
                PAFV%Absorber, &
                PAFV%idx_map, &
                PAFV%PDP, &
                PAFV%Tz_ref, &
                PAFV%Tz, &
                PAFV%Tzp_ref, &
                PAFV%Tzp, &
                PAFV%GAz_ref, &
                PAFV%GAz_sum, &
                PAFV%GAz, &
                PAFV%GAzp_ref, &
                PAFV%GAzp_sum, &
                PAFV%GAzp, &
                PAFV%GATzp_ref, &
                PAFV%GATzp_sum, &
                PAFV%GATzp, &
                PAFV%Ref_LnPressure, &
                PAFV%User_LnPressure, &
                STAT = Allocate_Status(1) )                                     
                                                
    DEALLOCATE( PAFV%DT,           &     
                PAFV%T,            &       
                PAFV%T2,           &        
                PAFV%DT2,          &        
                PAFV%H2O,          &      
                PAFV%H2O_A,        &        
                PAFV%H2O_R,        &       
                PAFV%H2O_S,        &       
                PAFV%H2O_R4,       &        
                PAFV%H2OdH2OTzp,   &       
                PAFV%CO2,          & 
                PAFV%O3,           & 
                PAFV%O3_A,         &     
                PAFV%O3_R,         & 
                PAFV%CO,           & 
                PAFV%CO_A,         & 
                PAFV%CO_R,         & 
                PAFV%CO_S,         & 
                PAFV%CO_ACOdCOzp,  &    
                PAFV%N2O,          &
                PAFV%N2O_A,        &      
                PAFV%N2O_R,        &    
                PAFV%N2O_S,        &   
                PAFV%CH4,          & 
                PAFV%CH4_A,        & 
                PAFV%CH4_R,        & 
                PAFV%CH4_ACH4zp,   &                                              
                STAT = Allocate_Status(2) )                                     

    DEALLOCATE( PAFV%OD,       &
                PAFV%OD_path,  &
                STAT = Allocate_Status(3) )                                     

    IF ( ANY(Allocate_Status /= 0) ) THEN                                         
      WRITE( Message, '( "Error deallocating predictor FW variables. STAT = ", i5 )' ) &  
                      Allocate_Status                                        
      Error_Status = FAILURE                                                 
      CALL Display_Message( ROUTINE_NAME, &                                                 
                            TRIM(Message), &                                 
                            Error_Status, &                                  
                            Message_Log=Message_Log )                        
      RETURN                                                                 
    END IF                                                                   

    IF(PAFV%OPTRAN)THEN
      DEALLOCATE( PAFV%dPonG, &
                  PAFV%d_Absorber, &
                  PAFV%Int_vapor, &
                  PAFV%AveA, &
                  PAFV%Inverse, &
                  PAFV%s_t, &
                  PAFV%s_p, &
                  PAFV%Ap1, &
                  PAFV%b,   &
                  PAFV%LN_Chi,  &
                   STAT = Allocate_Status(1) )                                    
      IF ( Allocate_Status(1) /= 0 ) THEN                                         
        WRITE( Message, '( "Error deallocating OPTRAN predictor FW variables. STAT = ", i5 )' ) &  
                        Allocate_Status                                        
        Error_Status = FAILURE                                                 
        CALL Display_Message( ROUTINE_NAME, &                                               
                              TRIM(Message), &                                 
                              Error_Status, &                                  
                              Message_Log=Message_Log )                        
        RETURN                                                                 
      END IF                                                                   
    END IF
    
    PAFV%H2O_idx = 0
    PAFV%n_OUsed_Pred  = MAX_OPTRAN_USED_PREDICTORS
    PAFV%Active  = .FALSE.
    PAFV%OPTRAN  = .FALSE.
            
  END FUNCTION Destroy_PAFV

  !---------------------------------------------------
  ! Routine to allocate memory for the PAFV structure
  ! pointer members to store FW variables for the TL 
  ! and AD routines. 
  !---------------------------------------------------
  FUNCTION Allocate_PAFV( n_ODPS_Layers,    & 
                          n_Absorbers,      &
                          n_User_Layers,    &
                          OPTRAN,           &
                          PAFV,             &            
                          RCS_Id       , &  ! Revision control           
                          Message_Log  ) &  ! Error messaging            
                        RESULT( Error_Status )                           
    ! Arguments
    INTEGER                  , INTENT(IN)     :: n_ODPS_Layers
    INTEGER                  , INTENT(IN)     :: n_Absorbers
    INTEGER                  , INTENT(IN)     :: n_User_Layers
    LOGICAL                  , INTENT(IN)     :: OPTRAN
    TYPE(PAFV_type)          , INTENT(IN OUT) :: PAFV 
    CHARACTER(*), OPTIONAL   , INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL   , INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_PAFV'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status(3)
    INTEGER :: n_OUsed_Pred
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( n_ODPS_Layers  < 1 .OR. &
         n_Absorbers    < 1 .OR. &
         n_User_Layers  < 1 )THEN
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             'Input Predictor dimensions must all be > 0.', &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
        
    ! Perform the pointer allocation
    ALLOCATE( PAFV%ODPS2User_Idx(2, 0:n_User_Layers), &
!             PAFV%ODPS2User_Idx(2, n_User_Layers), &
!             PAFV%Acc_Weighting_OD(n_ODPS_Layers,n_User_Layers), &
              PAFV%interp_index(2, n_ODPS_Layers),  &
              PAFV%Acc_Weighting(n_User_Layers,n_ODPS_Layers), &
              PAFV%Temperature(n_ODPS_Layers), &   
              PAFV%Absorber(n_ODPS_Layers, n_Absorbers), &
              PAFV%idx_map(n_Absorbers), &     
              PAFV%PDP(n_ODPS_Layers), &          
              PAFV%Tz_ref(n_ODPS_Layers), &       
              PAFV%Tz(n_ODPS_Layers), &           
              PAFV%Tzp_ref(n_ODPS_Layers), &  
              PAFV%Tzp(n_ODPS_Layers), &      
              PAFV%GAz_ref(n_ODPS_Layers, n_Absorbers), &      
              PAFV%GAz_sum(n_ODPS_Layers, n_Absorbers), &      
              PAFV%GAz(n_ODPS_Layers, n_Absorbers), &          
              PAFV%GAzp_ref(n_ODPS_Layers, n_Absorbers), &     
              PAFV%GAzp_sum(n_ODPS_Layers, n_Absorbers), &     
              PAFV%GAzp(n_ODPS_Layers, n_Absorbers), &         
              PAFV%GATzp_ref(n_ODPS_Layers, n_Absorbers), &    
              PAFV%GATzp_sum(n_ODPS_Layers, n_Absorbers), &    
              PAFV%GATzp(n_ODPS_Layers, n_Absorbers), & 
              PAFV%Ref_LnPressure(n_ODPS_Layers), &
              PAFV%User_LnPressure(n_User_Layers), &
              STAT = Allocate_Status(1) )                                     
                                               
    ALLOCATE( PAFV%DT(n_ODPS_Layers),           &       
              PAFV%T(n_ODPS_Layers),            &         
              PAFV%T2(n_ODPS_Layers),           &          
              PAFV%DT2(n_ODPS_Layers),          &          
              PAFV%H2O(n_ODPS_Layers),          &        
              PAFV%H2O_A(n_ODPS_Layers),        &          
              PAFV%H2O_R(n_ODPS_Layers),        &         
              PAFV%H2O_S(n_ODPS_Layers),        &         
              PAFV%H2O_R4(n_ODPS_Layers),       &          
              PAFV%H2OdH2OTzp(n_ODPS_Layers),   &         
              PAFV%CO2(n_ODPS_Layers),          & 
              PAFV%O3(n_ODPS_Layers),           &   
              PAFV%O3_A(n_ODPS_Layers),         &       
              PAFV%O3_R(n_ODPS_Layers),         &  
              PAFV%CO(n_ODPS_Layers),           &   
              PAFV%CO_A(n_ODPS_Layers),         &  
              PAFV%CO_R(n_ODPS_Layers),         &  
              PAFV%CO_S(n_ODPS_Layers),         & 
              PAFV%CO_ACOdCOzp(n_ODPS_Layers),  &      
              PAFV%N2O(n_ODPS_Layers),          &
              PAFV%N2O_A(n_ODPS_Layers),        &        
              PAFV%N2O_R(n_ODPS_Layers),        &      
              PAFV%N2O_S(n_ODPS_Layers),        &     
              PAFV%CH4(n_ODPS_Layers),          &  
              PAFV%CH4_A(n_ODPS_Layers),        & 
              PAFV%CH4_R(n_ODPS_Layers),        & 
              PAFV%CH4_ACH4zp(n_ODPS_Layers),   &
              STAT = Allocate_Status(2) )                                     

    ALLOCATE( PAFV%OD(n_ODPS_Layers),           &       
              PAFV%OD_path(0:n_ODPS_Layers),    &         
              STAT = Allocate_Status(3) )                                     

    IF ( ANY(Allocate_Status /= 0) ) THEN
      WRITE( Message, '( "Error allocating Predictor PAFV data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF

    IF(OPTRAN)THEN
      ALLOCATE( PAFV%dPonG(n_ODPS_Layers), &
                PAFV%d_Absorber(n_ODPS_Layers), &            
                PAFV%Int_vapor(n_ODPS_Layers), &             
                PAFV%AveA(n_ODPS_Layers), &                  
                PAFV%Inverse(n_ODPS_Layers), &               
                PAFV%s_t(n_ODPS_Layers), &                   
                PAFV%s_p(n_ODPS_Layers), & 
                PAFV%Ap1(n_ODPS_Layers), &
                PAFV%b(n_ODPS_Layers, 0:MAX_OPTRAN_USED_PREDICTORS), &
                PAFV%LN_Chi(n_ODPS_Layers), &                 
                PAFV%Chi(n_ODPS_Layers), &                 
                STAT = Allocate_Status(1) )                                      
      IF ( Allocate_Status(1) /= 0 ) THEN
        WRITE( Message, '( "Error allocating OPTRAN Predictor PAFV data arrays. STAT = ", i5 )' ) &
                        Allocate_Status
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               TRIM(Message), &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF
     
      PAFV%OPTRAN = OPTRAN
       
    END IF
     
    PAFV%n_ODPS_Layers = n_ODPS_Layers
    PAFV%n_Absorbers   = n_Absorbers
    PAFV%n_User_Layers = n_User_Layers 
    PAFV%n_OUsed_Pred  = MAX_OPTRAN_USED_PREDICTORS 
    
    PAFV%Active = .TRUE. 
    
  END FUNCTION Allocate_PAFV
  
END MODULE ODPS_Predictor_Define
