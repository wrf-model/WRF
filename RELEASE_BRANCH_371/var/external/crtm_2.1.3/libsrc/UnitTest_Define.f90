!
! UnitTest_Define
!
! Module defining the UnitTest object
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Feb-2007
!                       paul.vandelst@noaa.gov
!

MODULE UnitTest_Define

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: Byte, Short, Long, Single, Double
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: UnitTest_type
  ! Procedures
  PUBLIC :: UnitTest_Init
  PUBLIC :: UnitTest_Setup
  PUBLIC :: UnitTest_Report
  PUBLIC :: UnitTest_Summary
  PUBLIC :: UnitTest_n_Passed
  PUBLIC :: UnitTest_n_Failed
  PUBLIC :: UnitTest_Passed
  PUBLIC :: UnitTest_Assert
  PUBLIC :: UnitTest_IsEqual
  PUBLIC :: UnitTest_IsEqualWithin
  PUBLIC :: UnitTest_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  ! PUBLIC procedures
  INTERFACE UnitTest_IsEqual
    ! INTEGER(Byte) procedures
    MODULE PROCEDURE intbyte_isequal_scalar
    MODULE PROCEDURE intbyte_isequal_rank1
    MODULE PROCEDURE intbyte_isequal_rank2
    ! INTEGER(Short) procedures
    MODULE PROCEDURE intshort_isequal_scalar
    MODULE PROCEDURE intshort_isequal_rank1
    MODULE PROCEDURE intshort_isequal_rank2
    ! INTEGER(Long) procedures
    MODULE PROCEDURE intlong_isequal_scalar
    MODULE PROCEDURE intlong_isequal_rank1
    MODULE PROCEDURE intlong_isequal_rank2
    ! REAL(Single) procedures
    MODULE PROCEDURE realsp_isequal_scalar
    MODULE PROCEDURE realsp_isequal_rank1
    MODULE PROCEDURE realsp_isequal_rank2
    ! REAL(Double) procedures
    MODULE PROCEDURE realdp_isequal_scalar
    MODULE PROCEDURE realdp_isequal_rank1
    MODULE PROCEDURE realdp_isequal_rank2
    ! COMPLEX(Single) procedures
    MODULE PROCEDURE complexsp_isequal_scalar
    MODULE PROCEDURE complexsp_isequal_rank1
    MODULE PROCEDURE complexsp_isequal_rank2
    ! COMPLEX(Double) procedures
    MODULE PROCEDURE complexdp_isequal_scalar
    MODULE PROCEDURE complexdp_isequal_rank1
    MODULE PROCEDURE complexdp_isequal_rank2
    ! CHARACTER(*) procedures
    MODULE PROCEDURE char_isequal_scalar
    MODULE PROCEDURE char_isequal_rank1
    MODULE PROCEDURE char_isequal_rank2
  END INTERFACE UnitTest_IsEqual
  
  INTERFACE UnitTest_IsEqualWithin
    ! REAL(Single) procedures
    MODULE PROCEDURE realsp_isequalwithin_scalar
    MODULE PROCEDURE realsp_isequalwithin_rank1
    MODULE PROCEDURE realsp_isequalwithin_rank2
    ! REAL(Double) procedures
    MODULE PROCEDURE realdp_isequalwithin_scalar
    MODULE PROCEDURE realdp_isequalwithin_rank1
    MODULE PROCEDURE realdp_isequalwithin_rank2
    ! COMPLEX(Single) procedures
    MODULE PROCEDURE complexsp_isequalwithin_scalar
    MODULE PROCEDURE complexsp_isequalwithin_rank1
    MODULE PROCEDURE complexsp_isequalwithin_rank2
    ! COMPLEX(Double) procedures
    MODULE PROCEDURE complexdp_isequalwithin_scalar
    MODULE PROCEDURE complexdp_isequalwithin_rank1
    MODULE PROCEDURE complexdp_isequalwithin_rank2
  END INTERFACE UnitTest_IsEqualWithin
  
  
  ! PRIVATE procedures
  INTERFACE Get_Multiplier
    MODULE PROCEDURE realsp_get_multiplier
    MODULE PROCEDURE realdp_get_multiplier
  END INTERFACE Get_Multiplier
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: UnitTest_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  INTEGER,      PARAMETER :: SL = 512
  INTEGER,      PARAMETER :: CR = 13
  INTEGER,      PARAMETER :: LF = 10
  CHARACTER(2), PARAMETER :: CRLF = ACHAR(CR)//ACHAR(LF)
  CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
  CHARACTER(*), PARAMETER :: ZFMT = '"(",'//RFMT//',",",'//RFMT//',")"'
  LOGICAL,      PARAMETER :: DEFAULT_VERBOSE = .FALSE.

  ! Message levels
  INTEGER, PARAMETER :: N_MESSAGE_LEVELS = 5
  INTEGER, PARAMETER :: INIT_LEVEL    = 1
  INTEGER, PARAMETER :: SETUP_LEVEL   = 2
  INTEGER, PARAMETER :: TEST_LEVEL    = 3
  INTEGER, PARAMETER :: REPORT_LEVEL  = 4
  INTEGER, PARAMETER :: SUMMARY_LEVEL = 5
  CHARACTER(*), PARAMETER :: MESSAGE_LEVEL(N_MESSAGE_LEVELS) = &
    (/ 'INIT   ', 'SETUP  ', 'TEST   ', 'REPORT ', 'SUMMARY' /)

  ! ------------------------
  ! Derived type definitions
  ! ------------------------
  !:tdoc+:
  TYPE :: UnitTest_type
    PRIVATE
    ! User accessible test settings
    LOGICAL       :: Verbose = DEFAULT_VERBOSE
    CHARACTER(SL) :: Title   = ''
    CHARACTER(SL) :: Caller  = ''
    ! Internal test settings
    ! ...Test result messaging
    INTEGER       :: Level     = INIT_LEVEL
    CHARACTER(SL) :: Procedure = ''
    CHARACTER(SL) :: Message   = ''
    ! ...Test result (used for array argument procedures)
    LOGICAL :: Test_Result = .TRUE.
    ! ...Individual test counters
    INTEGER :: n_Tests        = 0
    INTEGER :: n_Passed_Tests = 0
    INTEGER :: n_Failed_Tests = 0
    ! ...All test counters
    INTEGER :: n_AllTests        = 0
    INTEGER :: n_Passed_AllTests = 0
    INTEGER :: n_Failed_AllTests = 0
  END TYPE UnitTest_type
  !:tdoc-:
  
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_Init
!
! PURPOSE:
!       UnitTest initialisation subroutine.
!
!       This subroutine should be called ONCE, BEFORE ANY tests are performed.
!
! CALLING SEQUENCE:
!       CALL UnitTest_Init( UnitTest, Verbose=Verbose )
!
! OBJECTS:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Verbose:       Logical argument to control length of reporting output.
!                      If == .FALSE., Only failed tests are reported [DEFAULT].
!                         == .TRUE.,  Both failed and passed tests are reported.
!                      If not specified, default is .TRUE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE UnitTest_Init( UnitTest, Verbose )
    ! Arguments
    TYPE(UnitTest_type), INTENT(OUT) :: UnitTest
    LOGICAL,   OPTIONAL, INTENT(IN)  :: Verbose
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Init'
    ! Variables
    LOGICAL :: local_Verbose
    
    ! Check optional arguments
    local_Verbose = DEFAULT_VERBOSE
    IF ( PRESENT(Verbose) ) local_Verbose = Verbose

    ! Perform initialisation
    CALL Set_Property( &
      UnitTest, &
      Verbose = local_Verbose, &
      Level     = INIT_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      n_Tests        = 0, &
      n_Passed_Tests = 0, &
      n_Failed_Tests = 0, &
      n_AllTests        = 0, &
      n_Passed_AllTests = 0, &
      n_Failed_AllTests = 0  )
    CALL Display_Message( UnitTest )
  END SUBROUTINE UnitTest_Init


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_Setup
!
! PURPOSE:
!       UnitTest individual test setup subroutine.
!
!       This subroutine should be called BEFORE each set of tests performed.
!
! CALLING SEQUENCE:
!       CALL UnitTest_Setup( UnitTest         , &
!                            Title            , &
!                            Caller  = Caller , &
!                            Verbose = Verbose  )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Title:         Character string containing the title of the test
!                      to be performed.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Caller:        Character string containing the name of the calling
!                      subprogram. If not specified, default is an empty string.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Verbose:       Logical argument to control length of reporting output.
!                      If == .FALSE., Only failed tests are reported [DEFAULT].
!                         == .TRUE.,  Both failed and passed tests are reported.
!                      If not specified, default is .TRUE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE UnitTest_Setup( UnitTest, Title, Caller, Verbose )
    ! Arguments
    TYPE(UnitTest_type)   , INTENT(IN OUT) :: UnitTest
    CHARACTER(*)          , INTENT(IN)     :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Caller
    LOGICAL,      OPTIONAL, INTENT(IN)     :: Verbose
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Setup'
    ! Variables
    CHARACTER(SL) :: local_Caller
    LOGICAL       :: local_Verbose
    CHARACTER(SL) :: Message

    ! Check arguments
    local_Caller = ''
    IF ( PRESENT(Caller) ) local_Caller = '; CALLER: '//TRIM(ADJUSTL(Caller))
    local_Verbose = DEFAULT_VERBOSE
    IF ( PRESENT(Verbose) ) local_Verbose = Verbose

    ! Create init message
    Message = TRIM(Title)//TRIM(local_Caller)

    ! Perform initialistion
    CALL Set_Property( &
      UnitTest, &
      Title   = ADJUSTL(Title), &
      Caller  = local_Caller  , &
      Verbose = local_Verbose , &
      Level     = SETUP_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message, &
      n_Tests        = 0, &
      n_Passed_Tests = 0, &
      n_Failed_Tests = 0  )
    CALL Display_Message( UnitTest )
                            
  END SUBROUTINE UnitTest_Setup


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_Report
!
! PURPOSE:
!       UnitTest individual test report subroutine
!
!       This subroutine should be called AFTER each set of tests performed.
!
! CALLING SEQUENCE:
!       CALL UnitTest_Report( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE UnitTest_Report( UnitTest )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Report'
    ! Variables
    INTEGER :: n_Tests       
    INTEGER :: n_Passed_Tests
    INTEGER :: n_Failed_Tests
    CHARACTER(SL) :: Message
    CHARACTER(SL) :: Attention

    ! Retrieve required properties
    CALL Get_Property( &
      UnitTest, &
      n_Tests        = n_Tests       , &
      n_Passed_Tests = n_Passed_Tests, &
      n_Failed_Tests = n_Failed_Tests  )
      
    ! Test fail attention-grabber
    Attention = ''
    IF ( n_Failed_Tests /= 0 ) Attention = '  <----<<<  **WARNING**'

    ! Output results
    WRITE( Message, &
      '(a,3x,"Passed ",i0," of ",i0," tests", &
       &a,3x,"Failed ",i0," of ",i0," tests",a)') &
      CRLF, &
      n_Passed_Tests, n_Tests, &
      CRLF, &
      n_Failed_Tests, n_Tests, &
      TRIM(Attention)
    CALL Set_Property( &
      UnitTest, &
      Level = REPORT_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    CALL Display_Message( UnitTest )
  END SUBROUTINE UnitTest_Report


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_Summary
!
! PURPOSE:
!       UnitTest test suite report summary subroutine
!
!       This subroutine should be called ONCE, AFTER ALL tests are performed.
!
! CALLING SEQUENCE:
!       CALL UnitTest_Summary( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE UnitTest_Summary( UnitTest )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Summary'
    ! Variables
    INTEGER :: n_AllTests       
    INTEGER :: n_Passed_AllTests
    INTEGER :: n_Failed_AllTests
    CHARACTER(SL) :: Message
    CHARACTER(SL) :: Attention
    
    ! Retrieve required properties
    CALL Get_Property( &
      UnitTest, &
      n_AllTests        = n_AllTests       , &
      n_Passed_AllTests = n_Passed_AllTests, &
      n_Failed_AllTests = n_Failed_AllTests  )
      
    ! Test fail attention-grabber
    Attention = ''
    IF ( n_Failed_AllTests /= 0 ) Attention = '  <----<<<  **WARNING**'

    ! Output results
    WRITE( Message, &
      '(a,1x,"Passed ",i0," of ",i0," total tests",&
       &a,1x,"Failed ",i0," of ",i0," total tests",a)') &
      CRLF, &
      n_Passed_AllTests, n_AllTests, &
      CRLF, &
      n_Failed_AllTests, n_AllTests, &
      TRIM(Attention)
    CALL Set_Property( &
      UnitTest, &
      Level = SUMMARY_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    CALL Display_Message( UnitTest )
  END SUBROUTINE UnitTest_Summary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_n_Passed
!
! PURPOSE:
!       Utility function to return the number of tests passed.
!
! CALLING SEQUENCE:
!       n = UnitTest_n_Passed( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:             The number of unit tests that have currently passed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  PURE FUNCTION UnitTest_n_Passed( UnitTest ) RESULT( n )
    TYPE(UnitTest_type), INTENT(IN) :: UnitTest
    INTEGER :: n
    CALL Get_Property( UnitTest, n_Passed_Tests = n )
  END FUNCTION UnitTest_n_Passed


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_n_Failed
!
! PURPOSE:
!       Utility function to return the number of tests failed.
!
! CALLING SEQUENCE:
!       n = UnitTest_n_Failed( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:             The number of unit tests that have currently failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  PURE FUNCTION UnitTest_n_Failed( UnitTest ) RESULT( n )
    TYPE(UnitTest_type), INTENT(IN) :: UnitTest
    INTEGER :: n
    CALL Get_Property( UnitTest, n_Failed_Tests = n )
  END FUNCTION UnitTest_n_Failed


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_Passed
!
! PURPOSE:
!       Function to inform if the last test performed passed.
!
! CALLING SEQUENCE:
!       result = UnitTest_Passed( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical to indicate if the last test performed passed.
!                      If == .TRUE.,  the last test passed,
!                         == .FALSE., the last test failed.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  PURE FUNCTION UnitTest_Passed( UnitTest ) RESULT( Passed )
    TYPE(UnitTest_type), INTENT(IN) :: UnitTest
    LOGICAL :: Passed
    CALL Get_Property( UnitTest, Test_Result = Passed )
  END FUNCTION UnitTest_Passed
  
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_Assert
!
! PURPOSE:
!       Subroutine to assert its test argument
!
! CALLING SEQUENCE:
!       CALL UnitTest_Assert(UnitTest, Test)
!
! OBJECTS:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Test:          The logical expression to assert.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE UnitTest_Assert(UnitTest, Test)
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    LOGICAL,             INTENT(IN)     :: Test
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Assert'
    ! Variables
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    Message = ''
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    
    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    CALL Test_Info_String( UnitTest, Message )
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
    
  END SUBROUTINE UnitTest_Assert
  

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_IsEqual
!
! PURPOSE:
!       Subroutine to assert that two arguments are equal.
!
! CALLING SEQUENCE:
!       CALL UnitTest_IsEqual( UnitTest, Expected, Actual )
!
! OBJECTS:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Expected:      The expected value of the variable being tested.
!                      UNITS:      N/A
!                      TYPE:       INTEGER(Byte)  , or
!                                  INTEGER(Short) , or
!                                  INTEGER(Long)  , or
!                                  REAL(Single)   , or
!                                  REAL(Double)   , or
!                                  COMPLEX(Single), or
!                                  COMPLEX(Double), or
!                                  CHARACTER(*)
!                      DIMENSION:  Scalar, or
!                                  Rank-1, or
!                                  Rank-2
!                      ATTRIBUTES: INTENT(IN)
!
!       Actual:        The actual value of the variable being tested.
!                      UNITS:      N/A
!                      TYPE:       Same as Expected input
!                      DIMENSION:  Same as Expected input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE intbyte_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Byte),       INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Byte)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message,'("Expected ",i0," and got ",i0)') Expected, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE intbyte_isequal_scalar

  
  SUBROUTINE intbyte_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Byte),       INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Byte)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL intbyte_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intbyte_isequal_rank1

  
  SUBROUTINE intbyte_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Byte),       INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Byte)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL intbyte_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intbyte_isequal_rank2


  SUBROUTINE intshort_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Short),       INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Short)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message,'("Expected ",i0," and got ",i0)') Expected, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE intshort_isequal_scalar

  
  SUBROUTINE intshort_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Short),      INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Short)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL intshort_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intshort_isequal_rank1

  
  SUBROUTINE intshort_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Short),      INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Short)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL intshort_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intshort_isequal_rank2


  SUBROUTINE intlong_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Long),       INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Long)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message,'("Expected ",i0," and got ",i0)') Expected, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE intlong_isequal_scalar

  
  SUBROUTINE intlong_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Long),       INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Long)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL intlong_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intlong_isequal_rank1

  
  SUBROUTINE intlong_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER(Long),       INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Long)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL intlong_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intlong_isequal_rank2


  SUBROUTINE realsp_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Single),        INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Single)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected: ",'//RFMT//',a,&
         &7x,"And got:  ",'//RFMT//')') &
      CRLF, Expected, CRLF, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE realsp_isequal_scalar

  
  SUBROUTINE realsp_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Single),        INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL realsp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE realsp_isequal_rank1

  
  SUBROUTINE realsp_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Single),        INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL realsp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE realsp_isequal_rank2


  SUBROUTINE realdp_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Double),        INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Double)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected: ",'//RFMT//',a,&
         &7x,"And got:  ",'//RFMT//')') &
      CRLF, Expected, CRLF, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE realdp_isequal_scalar

  
  SUBROUTINE realdp_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Double),        INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL realdp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE realdp_isequal_rank1

  
  SUBROUTINE realdp_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Double),        INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL realdp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE realdp_isequal_rank2


  SUBROUTINE complexsp_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Single),     INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Single)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected: ",'//ZFMT//',a,&
         &7x,"And got:  ",'//ZFMT//')') &
      CRLF, Expected, CRLF, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE complexsp_isequal_scalar

  
  SUBROUTINE complexsp_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Single),     INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL complexsp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE complexsp_isequal_rank1

  
  SUBROUTINE complexsp_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Single),     INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL complexsp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE complexsp_isequal_rank2


  SUBROUTINE complexdp_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Double),     INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Double)]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected: ",'//ZFMT//',a,&
         &7x,"And got:  ",'//ZFMT//')') &
      CRLF, Expected, CRLF, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE complexdp_isequal_scalar

  
  SUBROUTINE complexdp_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Double),     INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL complexdp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE complexdp_isequal_rank1

  
  SUBROUTINE complexdp_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Double),     INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL complexdp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE complexdp_isequal_rank2
  

  SUBROUTINE char_isequal_scalar( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    CHARACTER(*),        INTENT(IN)     :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[CHARACTER]'
    ! Variables
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Assign the test
    Test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message,'("Expected >",a,"< and got >",a,"<")') Expected, Actual
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE char_isequal_scalar

  
  SUBROUTINE char_isequal_rank1( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    CHARACTER(*),        INTENT(IN)     :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[CHARACTER]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
        isize, SIZE(Actual)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL char_isequal_scalar( UnitTest, Expected(i), Actual(i) )
    END DO
  END SUBROUTINE char_isequal_rank1

  
  SUBROUTINE char_isequal_rank2( UnitTest, Expected, Actual )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    CHARACTER(*),        INTENT(IN)     :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[CHARACTER]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL char_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE char_isequal_rank2


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_IsEqualWithin
!
! PURPOSE:
!       Subroutine to assert that two floating point arguments are equal to
!       within the specified tolerance.
!
! CALLING SEQUENCE:
!       CALL UnitTest_IsEqualWithin( UnitTest , &
!                                    Expected , &
!                                    Actual   , &
!                                    Tolerance, &
!                                    Epsilon_Scale = Epsilon_Scale )
!
! OBJECTS:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Expected:      The expected value of the variable being tested.
!                      UNITS:      N/A
!                      TYPE:       REAL(Single)   , or
!                                  REAL(Double)   , or
!                                  COMPLEX(Single), or
!                                  COMPLEX(Double)
!                      DIMENSION:  Scalar, or
!                                  Rank-1, or
!                                  Rank-2
!                      ATTRIBUTES: INTENT(IN)
!
!       Actual:        The actual value of the variable being tested.
!                      UNITS:      N/A
!                      TYPE:       Same as Expected input
!                      DIMENSION:  Same as Expected input
!                      ATTRIBUTES: INTENT(IN)
!
!       Tolerance:     The tolerance to within which the Expected and Actual
!                      values must agree. If negative, the value of
!                        EPSILON(Expected)
!                      is used.
!                      This argument is ignored if the EPSILON_SCALE optional
!                      argument is specified
!                      UNITS:      N/A
!                      TYPE:       Same as Expected input
!                      DIMENSION:  Same as Expected input
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Epsilon_Scale: Set this logical flag to compute and use the tolerance
!                      value:
!                        EPSILON(Expected) * Scale_Factor
!                      where the scaling factor is the exponent value of the
!                      input argument Expected.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL.
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE realsp_isequalwithin_scalar( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Single),        INTENT(IN)     :: Expected, Actual, Tolerance
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Single)]'
    ! Variables
    REAL(Single) :: tol
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Default tolerance
    tol = Tolerance
    ! ...Check optional arguments
    IF ( PRESENT(Epsilon_Scale) ) THEN
      IF ( Epsilon_Scale ) tol = EPSILON(Expected) * Get_Multiplier( Expected )
    END IF
    ! ...Assign the test
    Test = (ABS(Expected-Actual) < tol)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected:     ",'//RFMT//',a,&
         &7x,"To within:    ",'//RFMT//',a,&
         &7x,"And got:      ",'//RFMT//',a,&
         &7x,"|Difference|: ",'//RFMT//')') &
      CRLF, Expected, CRLF, tol, CRLF, Actual, CRLF, ABS(Expected-Actual)
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE realsp_isequalwithin_scalar

  
  SUBROUTINE realsp_isequalwithin_rank1(  &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Single),        INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual)    /= isize .OR. &
         SIZE(Tolerance) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
        isize, SIZE(Actual), SIZE(Tolerance)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL realsp_isequalwithin_scalar( &
        UnitTest    , &
        Expected(i) , &
        Actual(i)   , &
        Tolerance(i), &
        Epsilon_Scale = Epsilon_Scale )
    END DO
  END SUBROUTINE realsp_isequalwithin_rank1

  
  SUBROUTINE realsp_isequalwithin_rank2( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Single),        INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize .OR. &
         SIZE(Tolerance,DIM=1) /= isize .OR. &
         SIZE(Tolerance,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- ",&
        &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
        SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL realsp_isequalwithin_scalar( &
          UnitTest      , &
          Expected(i,j) , &
          Actual(i,j)   , &
          Tolerance(i,j), &
          Epsilon_Scale = Epsilon_Scale )
      END DO
    END DO
  END SUBROUTINE realsp_isequalwithin_rank2


  SUBROUTINE realdp_isequalwithin_scalar( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Double),        INTENT(IN)     :: Expected, Actual, Tolerance
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Double)]'
    ! Variables
    REAL(Double) :: tol
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Default tolerance
    tol = Tolerance
    ! ...Check optional arguments
    IF ( PRESENT(Epsilon_Scale) ) THEN
      IF ( Epsilon_Scale ) tol = EPSILON(Expected) * Get_Multiplier( Expected )
    END IF
    ! ...Assign the test
    Test = (ABS(Expected-Actual) < tol)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected:     ",'//RFMT//',a,&
         &7x,"To within:    ",'//RFMT//',a,&
         &7x,"And got:      ",'//RFMT//',a,&
         &7x,"|Difference|: ",'//RFMT//')') &
      CRLF, Expected, CRLF, tol, CRLF, Actual, CRLF, ABS(Expected-Actual)
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE realdp_isequalwithin_scalar

  
  SUBROUTINE realdp_isequalwithin_rank1(  &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Double),        INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual)    /= isize .OR. &
         SIZE(Tolerance) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
        isize, SIZE(Actual), SIZE(Tolerance)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL realdp_isequalwithin_scalar( &
        UnitTest    , &
        Expected(i) , &
        Actual(i)   , &
        Tolerance(i), &
        Epsilon_Scale = Epsilon_Scale )
    END DO
  END SUBROUTINE realdp_isequalwithin_rank1

  
  SUBROUTINE realdp_isequalwithin_rank2( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    REAL(Double),        INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize .OR. &
         SIZE(Tolerance,DIM=1) /= isize .OR. &
         SIZE(Tolerance,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- ",&
        &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
        SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL realdp_isequalwithin_scalar( &
          UnitTest      , &
          Expected(i,j) , &
          Actual(i,j)   , &
          Tolerance(i,j), &
          Epsilon_Scale = Epsilon_Scale )
      END DO
    END DO
  END SUBROUTINE realdp_isequalwithin_rank2


  SUBROUTINE complexsp_isequalwithin_scalar( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Single),     INTENT(IN)     :: Expected, Actual, Tolerance
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Single)]'
    ! Variables
    REAL(Single) :: tolr, toli
    REAL(Single) :: zr, zi
    REAL(Single) :: dzr, dzi
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Split expected into real and imag
    zr = REAL(Expected,Single)
    zi = AIMAG(Expected)
    ! ...Default tolerance
    tolr = REAL(Tolerance,Single)
    toli = AIMAG(Tolerance) 
    ! ...Check optional arguments
    IF ( PRESENT(Epsilon_Scale) ) THEN
      IF ( Epsilon_Scale ) THEN
        tolr = EPSILON(zr) * Get_Multiplier(zr)
        toli = EPSILON(zi) * Get_Multiplier(zi)
      END IF
    END IF
    ! ...Assign the test
    dzr = ABS(zr - REAL(Actual,Single))
    dzi = ABS(zi - AIMAG(Actual))
    Test = (dzr < tolr) .AND. (dzi < toli)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected:     ",'//ZFMT//',a,&
         &7x,"To within:    ",'//ZFMT//',a,&
         &7x,"And got:      ",'//ZFMT//',a,&
         &7x,"|Difference|: ",'//ZFMT//')') &
      CRLF, Expected, CRLF, CMPLX(tolr,toli,Single), CRLF, Actual, CRLF, dzr, dzi
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE complexsp_isequalwithin_scalar

  
  SUBROUTINE complexsp_isequalwithin_rank1( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Single),     INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual)    /= isize .OR. &
         SIZE(Tolerance) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
        isize, SIZE(Actual), SIZE(Tolerance)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL complexsp_isequalwithin_scalar( &
        UnitTest    , &
        Expected(i) , &
        Actual(i)   , &
        Tolerance(i), &
        Epsilon_Scale = Epsilon_Scale )
    END DO
  END SUBROUTINE complexsp_isequalwithin_rank1

  
  SUBROUTINE complexsp_isequalwithin_rank2( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Single),     INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize .OR. &
         SIZE(Tolerance,DIM=1) /= isize .OR. &
         SIZE(Tolerance,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- ",&
        &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
        SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL complexsp_isequalwithin_scalar( &
          UnitTest      , &
          Expected(i,j) , &
          Actual(i,j)   , &
          Tolerance(i,j), &
          Epsilon_Scale = Epsilon_Scale )
      END DO
    END DO
  END SUBROUTINE complexsp_isequalwithin_rank2


  SUBROUTINE complexdp_isequalwithin_scalar( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Double),     INTENT(IN)     :: Expected, Actual, Tolerance
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Double)]'
    ! Variables
    REAL(Double) :: tolr, toli
    REAL(Double) :: zr, zi
    REAL(Double) :: dzr, dzi
    LOGICAL :: Test
    LOGICAL :: Verbose
    CHARACTER(SL) :: Message
    
    ! Setup
    ! ...Split expected into real and imag
    zr = REAL(Expected,Double)
    zi = AIMAG(Expected)
    ! ...Default tolerance
    tolr = REAL(Tolerance,Double)
    toli = AIMAG(Tolerance) 
    ! ...Check optional arguments
    IF ( PRESENT(Epsilon_Scale) ) THEN
      IF ( Epsilon_Scale ) THEN
        tolr = EPSILON(zr) * Get_Multiplier(zr)
        toli = EPSILON(zi) * Get_Multiplier(zi)
      END IF
    END IF
    ! ...Assign the test
    dzr = ABS(zr - REAL(Actual,Double))
    dzi = ABS(zi - AIMAG(Actual))
    Test = (dzr < tolr) .AND. (dzi < toli)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      UnitTest, &
      Verbose = Verbose )
    Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
    

    ! Assert the test
    IF ( Test ) THEN
      CALL Test_Passed( UnitTest )
    ELSE
      CALL Test_Failed( UnitTest )
    END IF
    
    ! Output message
    WRITE( Message, &
      '(a,7x,"Expected:     ",'//ZFMT//',a,&
         &7x,"To within:    ",'//ZFMT//',a,&
         &7x,"And got:      ",'//ZFMT//',a,&
         &7x,"|Difference|: ",'//ZFMT//')') &
      CRLF, Expected, CRLF, CMPLX(tolr,toli,Double), CRLF, Actual, CRLF, dzr, dzi
    CALL Set_Property( &
      UnitTest, &
      Level = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message = Message )
    IF ( Verbose ) CALL Display_Message( UnitTest )
  END SUBROUTINE complexdp_isequalwithin_scalar

  
  SUBROUTINE complexdp_isequalwithin_rank1( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Double),     INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message

    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual)    /= isize .OR. &
         SIZE(Tolerance) /= isize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
        isize, SIZE(Actual), SIZE(Tolerance)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO i = 1, isize
      CALL complexdp_isequalwithin_scalar( &
        UnitTest    , &
        Expected(i) , &
        Actual(i)   , &
        Tolerance(i), &
        Epsilon_Scale = Epsilon_Scale )
    END DO
  END SUBROUTINE complexdp_isequalwithin_rank1

  
  SUBROUTINE complexdp_isequalwithin_rank2( &
    UnitTest     , &
    Expected     , &
    Actual       , &
    Tolerance    , &
    Epsilon_Scale  )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    COMPLEX(Double),     INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize .OR. &
         SIZE(Tolerance,DIM=1) /= isize .OR. &
         SIZE(Tolerance,DIM=2) /= jsize ) THEN
      CALL Test_Failed( UnitTest )
      WRITE( Message, &
        '("Array sizes are diffferent -- ",&
        &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
        SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
      CALL Set_Property( &
        UnitTest, &
        Level = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message = Message )
      CALL Display_Message( UnitTest )
      RETURN
    ENDIF
    
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL complexdp_isequalwithin_scalar( &
          UnitTest      , &
          Expected(i,j) , &
          Actual(i,j)   , &
          Tolerance(i,j), &
          Epsilon_Scale = Epsilon_Scale )
      END DO
    END DO
  END SUBROUTINE complexdp_isequalwithin_rank2
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       UnitTest_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL UnitTest_DefineVersion( Id )
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

  SUBROUTINE UnitTest_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE UnitTest_DefineVersion
  

!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!===================
! METHOD PROCEDURES
!===================

!------------------------------------------------------------------------------
!
! NAME:
!       Set_Property
!
! PURPOSE:
!       Private subroutine to set the properties of a UnitTest object.
!
!       All WRITE access to the UnitTest object properties should be
!       done using this subroutine.
!
! CALLING SEQUENCE:
!       CALL Set_Property( &
!         UnitTest, &
!         Verbose           = Verbose          , &
!         Title             = Title            , &
!         Caller            = Caller           , &
!         Level             = Level            , &
!         Procedure         = Procedure        , &
!         Message           = Message          , &
!         Test_Result       = Test_Result      , &
!         n_Tests           = n_Tests          , &
!         n_Passed_Tests    = n_Passed_Tests   , &
!         n_Failed_Tests    = n_Failed_Tests   , &
!         n_AllTests        = n_AllTests       , &
!         n_Passed_AllTests = n_Passed_AllTests, &
!         n_Failed_AllTests = n_Failed_AllTests  )
!
! OBJECT:
!       UnitTest:           UnitTest object.
!                           UNITS:      N/A
!                           TYPE:       TYPE(UnitTest_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Verbose:            Logical to control length of reporting output.
!                           If == .FALSE., Only failed tests are reported.
!                              == .TRUE.,  Both failed and passed tests are reported.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string containing the title of the
!                           test to be performed.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Caller:             Character string containing the name of the
!                           calling subprogram.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Level:              Integer flag specifying the output message level.
!                           UNITS:      N/A
!                           TYPE:       INTEGER     
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Procedure:          The name of the UnitTest procedure.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message:            Character string containing an informational
!                           message about the unit test performed.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Test_Result:        Logical to contain the result of unit tests
!                           performed
!                           If == .TRUE.,  Test passed.
!                              == .FALSE., Test failed.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Tests:            The number of tests performed for the current
!                           unit test type, i.e. since the last call to
!                           UnitTest_Setup().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Passed_Tests:     The number of tests passed for the current
!                           unit test type, i.e. since the last call to
!                           UnitTest_Setup().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Failed_Tests:     The number of tests failed for the current
!                           unit test type, i.e. since the last call to
!                           UnitTest_Setup().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_AllTests:         The total number of tests performed, i.e. since
!                           the last call to UnitTest_Init().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Passed_AllTests:  The total number of tests passed, i.e. since
!                           the last call to UnitTest_Init().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Failed_AllTests:  The total number of tests failed, i.e. since
!                           the last call to UnitTest_Init().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!------------------------------------------------------------------------------

  PURE SUBROUTINE Set_Property( &
    UnitTest         , & ! Object
    Verbose          , & ! Optional input
    Title            , & ! Optional input
    Caller           , & ! Optional input
    Level            , & ! Optional input
    Procedure        , & ! Optional input
    Message          , & ! Optional input
    Test_Result      , & ! Optional input
    n_Tests          , & ! Optional input
    n_Passed_Tests   , & ! Optional input
    n_Failed_Tests   , & ! Optional input
    n_AllTests       , & ! Optional input
    n_Passed_AllTests, & ! Optional input
    n_Failed_AllTests  ) ! Optional input
    ! Arguments
    TYPE(UnitTest_type)   , INTENT(IN OUT) :: UnitTest
    LOGICAL     , OPTIONAL, INTENT(IN)     :: Verbose
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Title  
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Caller 
    INTEGER     , OPTIONAL, INTENT(IN)     :: Level
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Procedure
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message   
    LOGICAL     , OPTIONAL, INTENT(IN)     :: Test_Result
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_Tests       
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_Passed_Tests
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_Failed_Tests
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_AllTests       
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_Passed_AllTests
    INTEGER     , OPTIONAL, INTENT(IN)     :: n_Failed_AllTests
    ! Set the object properties
    IF ( PRESENT(Verbose          ) ) UnitTest%Verbose           = Verbose
    IF ( PRESENT(Title            ) ) UnitTest%Title             = Title  
    IF ( PRESENT(Caller           ) ) UnitTest%Caller            = Caller 
    IF ( PRESENT(Level            ) ) UnitTest%Level             = Level 
    IF ( PRESENT(Procedure        ) ) UnitTest%Procedure         = Procedure
    IF ( PRESENT(Message          ) ) UnitTest%Message           = Message   
    IF ( PRESENT(Test_Result      ) ) UnitTest%Test_Result       = Test_Result
    IF ( PRESENT(n_Tests          ) ) UnitTest%n_Tests           = n_Tests       
    IF ( PRESENT(n_Passed_Tests   ) ) UnitTest%n_Passed_Tests    = n_Passed_Tests
    IF ( PRESENT(n_Failed_Tests   ) ) UnitTest%n_Failed_Tests    = n_Failed_Tests
    IF ( PRESENT(n_AllTests       ) ) UnitTest%n_AllTests        = n_AllTests       
    IF ( PRESENT(n_Passed_AllTests) ) UnitTest%n_Passed_AllTests = n_Passed_AllTests
    IF ( PRESENT(n_Failed_AllTests) ) UnitTest%n_Failed_AllTests = n_Failed_AllTests
  END SUBROUTINE Set_Property


!------------------------------------------------------------------------------
!
! NAME:
!       Get_Property
!
! PURPOSE:
!       Private subroutine to get the properties of a UnitTest object.
!
!       All READ access to the UnitTest object properties should be
!       done using this subroutine.
!
! CALLING SEQUENCE:
!       CALL Get_Property( &
!         UnitTest, &
!         Verbose           = Verbose          , &
!         Title             = Title            , &
!         Caller            = Caller           , &
!         Level             = Level            , &
!         Procedure         = Procedure        , &
!         Message           = Message          , &
!         Test_Result       = Test_Result      , &
!         n_Tests           = n_Tests          , &
!         n_Passed_Tests    = n_Passed_Tests   , &
!         n_Failed_Tests    = n_Failed_Tests   , &
!         n_AllTests        = n_AllTests       , &
!         n_Passed_AllTests = n_Passed_AllTests, &
!         n_Failed_AllTests = n_Failed_AllTests  )
!
! OBJECT:
!       UnitTest:           UnitTest object.
!                           UNITS:      N/A
!                           TYPE:       TYPE(UnitTest_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Verbose:            Logical to control length of reporting output.
!                           If == .FALSE., Only failed tests are reported.
!                              == .TRUE.,  Both failed and passed tests are reported.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string containing the title of the
!                           test to be performed.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Caller:             Character string containing the name of the
!                           calling subprogram.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Level:              Integer flag specifying the output message level.
!                           UNITS:      N/A
!                           TYPE:       INTEGER     
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Procedure:          The name of the last UnitTest Procedure called.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message:            Character string containing an informational
!                           message about the last unit test performed.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Test_Result:        Logical containing the result of the last
!                           unit test performed
!                           If == .TRUE.,  Test passed.
!                              == .FALSE., Test failed.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Tests:            The number of tests performed for the current
!                           unit test type, i.e. since the last call to
!                           UnitTest_Setup().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Passed_Tests:     The number of tests passed for the current
!                           unit test type, i.e. since the last call to
!                           UnitTest_Setup().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Failed_Tests:     The number of tests failed for the current
!                           unit test type, i.e. since the last call to
!                           UnitTest_Setup().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_AllTests:         The total number of tests performed, i.e. since
!                           the last call to UnitTest_Init().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Passed_AllTests:  The total number of tests passed, i.e. since
!                           the last call to UnitTest_Init().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Failed_AllTests:  The total number of tests failed, i.e. since
!                           the last call to UnitTest_Init().
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  PURE SUBROUTINE Get_Property( &
    UnitTest         , & ! Object
    Verbose          , & ! Optional output
    Title            , & ! Optional output
    Caller           , & ! Optional output
    Level            , & ! Optional output
    Procedure        , & ! Optional output
    Message          , & ! Optional output
    Test_Result      , & ! Optional output
    n_Tests          , & ! Optional output
    n_Passed_Tests   , & ! Optional output
    n_Failed_Tests   , & ! Optional output
    n_AllTests       , & ! Optional output
    n_Passed_AllTests, & ! Optional output
    n_Failed_AllTests  ) ! Optional output
    ! Arguments
    TYPE(UnitTest_type)   , INTENT(IN)  :: UnitTest
    LOGICAL     , OPTIONAL, INTENT(OUT) :: Verbose
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Caller 
    INTEGER     , OPTIONAL, INTENT(OUT) :: Level  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Procedure   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Message   
    LOGICAL     , OPTIONAL, INTENT(OUT) :: Test_Result
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Tests       
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Passed_Tests
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Failed_Tests
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_AllTests       
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Passed_AllTests
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Failed_AllTests
    ! Get the object properties
    IF ( PRESENT(Verbose          ) ) Verbose           = UnitTest%Verbose          
    IF ( PRESENT(Title            ) ) Title             = UnitTest%Title            
    IF ( PRESENT(Caller           ) ) Caller            = UnitTest%Caller           
    IF ( PRESENT(Level            ) ) Level             = UnitTest%Level            
    IF ( PRESENT(Procedure        ) ) Procedure         = UnitTest%Procedure        
    IF ( PRESENT(Message          ) ) Message           = UnitTest%Message          
    IF ( PRESENT(Test_Result      ) ) Test_Result       = UnitTest%Test_Result      
    IF ( PRESENT(n_Tests          ) ) n_Tests           = UnitTest%n_Tests          
    IF ( PRESENT(n_Passed_Tests   ) ) n_Passed_Tests    = UnitTest%n_Passed_Tests   
    IF ( PRESENT(n_Failed_Tests   ) ) n_Failed_Tests    = UnitTest%n_Failed_Tests   
    IF ( PRESENT(n_AllTests       ) ) n_AllTests        = UnitTest%n_AllTests       
    IF ( PRESENT(n_Passed_AllTests) ) n_Passed_AllTests = UnitTest%n_Passed_AllTests
    IF ( PRESENT(n_Failed_AllTests) ) n_Failed_AllTests = UnitTest%n_Failed_AllTests
  END SUBROUTINE Get_Property


!------------------------------------------------------------------------------
!
! NAME:
!       Test_Passed
!
! PURPOSE:
!       Subroutine to increment passed test counters.
!
! CALLING SEQUENCE:
!       CALL Test_Passed( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Test_Passed( UnitTest )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    ! Variables
    INTEGER :: n_Passed_Tests, n_Passed_AllTests

    ! Increment total test counters
    CALL Test_Increment( UnitTest )

    ! Increment the passed test counters
    ! ...Get 'em
    CALL Get_Property( &
      UnitTest, &
      n_Passed_Tests = n_Passed_Tests, &
      n_Passed_AllTests = n_Passed_AllTests )
    ! ...Increment
    n_Passed_Tests    = n_Passed_Tests    + 1
    n_Passed_AllTests = n_Passed_AllTests + 1
    ! ...Save 'em and set successful test result 
    CALL Set_Property( &
      UnitTest, &
      Test_Result = .TRUE., &
      n_Passed_Tests = n_Passed_Tests, &
      n_Passed_AllTests = n_Passed_AllTests )
  END SUBROUTINE Test_Passed


!------------------------------------------------------------------------------
!
! NAME:
!       Test_Failed
!
! PURPOSE:
!       Subroutine to increment failed test counters.
!
! CALLING SEQUENCE:
!       CALL Test_Failed( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Test_Failed( UnitTest )
    ! Arguments
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    ! Variables
    INTEGER :: n_Failed_Tests, n_Failed_AllTests

    ! Increment total test counters
    CALL Test_Increment( UnitTest )

    ! Increment the failed test counters
    ! ...Get 'em
    CALL Get_Property( &
      UnitTest, &
      n_Failed_Tests = n_Failed_Tests, &
      n_Failed_AllTests = n_Failed_AllTests )
    ! ...Increment
    n_Failed_Tests    = n_Failed_Tests    + 1
    n_Failed_AllTests = n_Failed_AllTests + 1
    ! ...Save 'em and set unsuccessful test result 
    CALL Set_Property( &
      UnitTest, &
      Test_Result = .FALSE., &
      n_Failed_Tests = n_Failed_Tests, &
      n_Failed_AllTests = n_Failed_AllTests )
  END SUBROUTINE Test_Failed


!------------------------------------------------------------------------------
!
! NAME:
!       Test_Increment
!
! PURPOSE:
!       Subroutine to increment the test total counters.
!
! CALLING SEQUENCE:
!       CALL Test_Increment( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Test_Increment( UnitTest )
    TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
    INTEGER :: n_Tests, n_AllTests
    
    CALL Get_Property( &
      UnitTest, &
      n_Tests    = n_Tests, &
      n_AllTests = n_AllTests )
      
    n_Tests    = n_Tests    + 1
    n_AllTests = n_AllTests + 1
    
    CALL Set_Property( &
      UnitTest, &
      n_Tests    = n_Tests, &
      n_AllTests = n_AllTests )
  END SUBROUTINE Test_Increment


!------------------------------------------------------------------------------
!
! NAME:
!       Display_Message
!
! PURPOSE:
!       Subroutine to display the unit test messages to stdout.
!
! CALLING SEQUENCE:
!       CALL Display_Message( UnitTest )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Display_Message( UnitTest )
    TYPE(UnitTest_type), INTENT(IN) :: UnitTest
    ! Variables
    INTEGER :: Level
    CHARACTER(SL) :: Procedure
    CHARACTER(SL) :: Message
    CHARACTER(SL) :: Fmt
    CHARACTER(SL) :: Prefix
    CHARACTER(SL) :: Test_Info
    INTEGER :: n_Spaces
    
    CALL Get_Property( &
      UnitTest, &
      Level = Level, &
      Procedure = Procedure, &
      Message = Message )
    
    ! Set output bits manually
    Test_Info = ''
    SELECT CASE(Level)
      CASE(INIT_LEVEL)
        Prefix = '/'
        n_Spaces = 1
      CASE(SETUP_LEVEL)
        Prefix = '/,3x,14("-"),/'
        n_Spaces = 3
      CASE(TEST_LEVEL)
        Prefix = ''
        n_Spaces = 5
        CALL Test_Info_String( UnitTest, Test_Info )
      CASE(REPORT_LEVEL)
        Prefix = ''
        n_Spaces = 3
      CASE(SUMMARY_LEVEL)
        Prefix = '/,1x,16("="),/'
        n_Spaces = 1
      CASE DEFAULT
        Prefix = '/,"INVALID MESSAGE LEVEL!!",/'
        n_Spaces = 15
    END SELECT
    
    ! Write the message to stdout
    WRITE(Fmt, '("(",a,i0,"x,a,"": "",a,1x,a)")') TRIM(Prefix), n_Spaces
    WRITE( *,FMT=Fmt ) TRIM(Procedure), TRIM(Test_Info), TRIM(Message)
    
  END SUBROUTINE Display_Message


!------------------------------------------------------------------------------
!
! NAME:
!       Test_Info_String
!
! PURPOSE:
!       Subroutine to construct an info string for message output.
!
! CALLING SEQUENCE:
!       CALL Test_Info_String( UnitTest, info )
!
! OBJECT:
!       UnitTest:      UnitTest object.
!                      UNITS:      N/A
!                      TYPE:       TYPE(UnitTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       info:          Character string containing the test number and
!                      whether the test passed or failed.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Test_Info_String( UnitTest, info )
    TYPE(UnitTest_Type), INTENT(IN) :: UnitTest
    CHARACTER(*), INTENT(OUT) :: info
    INTEGER :: n_Tests
    CHARACTER(6) :: PassFail
    CALL Get_Property( UnitTest, n_Tests = n_Tests )
    IF ( UnitTest_Passed( UnitTest ) ) THEN
      PassFail = 'PASSED'
    ELSE
      PassFail = 'FAILED'
    END IF
    WRITE( info,'("Test#",i0,1x,a,".")') n_Tests, PassFail
  END SUBROUTINE Test_Info_String


!====================
! UTILITY PROCEDURES
!====================

!------------------------------------------------------------------------------
!
! NAME:
!       Get_Multiplier
!
! PURPOSE:
!       Elemental function to compute the exponent multiplier of an input
!       for use in scaling tolerance values for floating point comparisons.
!
! CALLING SEQUENCE:
!       e = Get_Multiplier(x)
!
! INPUTS:
!       x:             Number for which the exponent multiplier is required.
!                      UNITS:      N/A
!                      TYPE:       REAL(Single)   , or
!                                  REAL(Double)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       e:             Exponent multiplier to use in scaling tolerance values.
!                      UNITS:      N/A
!                      TYPE:       Same as input x.
!                      DIMENSION:  Same as input x.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION realsp_get_multiplier(x) RESULT(e)
    REAL(Single), INTENT(IN) :: x
    REAL(Single) :: e
    IF (x > 0.0_Single) THEN
      e = 10.0_Single**FLOOR(LOG10(x))
    ELSE
      e = 1.0_Single
    END IF
  END FUNCTION realsp_get_multiplier
  
  ELEMENTAL FUNCTION realdp_get_multiplier(x) RESULT(e)
    REAL(Double), INTENT(IN) :: x
    REAL(Double) :: e
    IF (x > 0.0_Double) THEN
      e = 10.0_Double**FLOOR(LOG10(x))
    ELSE
      e = 1.0_Double
    END IF
  END FUNCTION realdp_get_multiplier
  
END MODULE UnitTest_Define
