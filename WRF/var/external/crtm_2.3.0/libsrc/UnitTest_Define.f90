!
! UnitTest_Define
!
! Module defining the UnitTest object
!
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, 05-Feb-2007
!                   paul.vandelst@noaa.gov
!

MODULE UnitTest_Define

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: Byte, Short, Long, Single, Double
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
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
  ! **** These procedure interfaces are kept for legacy
  ! **** purposes, but deprecated for new code
  PUBLIC :: UnitTest_Init
  PUBLIC :: UnitTest_Setup
  PUBLIC :: UnitTest_Report
  PUBLIC :: UnitTest_Summary
  PUBLIC :: UnitTest_n_Passed
  PUBLIC :: UnitTest_n_Failed
  PUBLIC :: UnitTest_Passed
  PUBLIC :: UnitTest_Failed
  PUBLIC :: UnitTest_Assert
  PUBLIC :: UnitTest_IsEqual
  PUBLIC :: UnitTest_IsEqualWithin
  PUBLIC :: UnitTest_IsWithinSigFig


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! **** Pre-type-bound procedure interface definitions
  ! **** Kept for legacy purposes, but deprecated for new code
  INTERFACE UnitTest_Init
    MODULE PROCEDURE Init
  END INTERFACE UnitTest_Init

  INTERFACE UnitTest_Setup
    MODULE PROCEDURE Setup
  END INTERFACE UnitTest_Setup

  INTERFACE UnitTest_Report
    MODULE PROCEDURE Report
  END INTERFACE UnitTest_Report

  INTERFACE UnitTest_Summary
    MODULE PROCEDURE Summary
  END INTERFACE UnitTest_Summary

  INTERFACE UnitTest_n_Passed
    MODULE PROCEDURE n_Passed
  END INTERFACE UnitTest_n_Passed

  INTERFACE UnitTest_n_Failed
    MODULE PROCEDURE n_Failed
  END INTERFACE UnitTest_n_Failed

  INTERFACE UnitTest_Passed
    MODULE PROCEDURE Passed
  END INTERFACE UnitTest_Passed

  INTERFACE UnitTest_Failed
    MODULE PROCEDURE Failed
  END INTERFACE UnitTest_Failed

  INTERFACE UnitTest_Assert
    MODULE PROCEDURE Assert
  END INTERFACE UnitTest_Assert

  INTERFACE UnitTest_IsEqual
    ! INTEGER(Byte) procedures
    MODULE PROCEDURE intbyte_assert_equal_s
    MODULE PROCEDURE intbyte_assert_equal_r1
    MODULE PROCEDURE intbyte_assert_equal_r2
    ! INTEGER(Short) procedures
    MODULE PROCEDURE intshort_assert_equal_s
    MODULE PROCEDURE intshort_assert_equal_r1
    MODULE PROCEDURE intshort_assert_equal_r2
    ! INTEGER(Long) procedures
    MODULE PROCEDURE intlong_assert_equal_s
    MODULE PROCEDURE intlong_assert_equal_r1
    MODULE PROCEDURE intlong_assert_equal_r2
    ! REAL(Single) procedures
    MODULE PROCEDURE realsp_assert_equal_s
    MODULE PROCEDURE realsp_assert_equal_r1
    MODULE PROCEDURE realsp_assert_equal_r2
    ! REAL(Double) procedures
    MODULE PROCEDURE realdp_assert_equal_s
    MODULE PROCEDURE realdp_assert_equal_r1
    MODULE PROCEDURE realdp_assert_equal_r2
    ! COMPLEX(Single) procedures
    MODULE PROCEDURE complexsp_assert_equal_s
    MODULE PROCEDURE complexsp_assert_equal_r1
    MODULE PROCEDURE complexsp_assert_equal_r2
    ! COMPLEX(Double) procedures
    MODULE PROCEDURE complexdp_assert_equal_s
    MODULE PROCEDURE complexdp_assert_equal_r1
    MODULE PROCEDURE complexdp_assert_equal_r2
    ! CHARACTER(*) procedures
    MODULE PROCEDURE char_assert_equal_s
    MODULE PROCEDURE char_assert_equal_r1
    MODULE PROCEDURE char_assert_equal_r2
  END INTERFACE UnitTest_IsEqual

  INTERFACE UnitTest_IsEqualWithin
    ! REAL(Single) procedures
    MODULE PROCEDURE realsp_assert_equalwithin_s
    MODULE PROCEDURE realsp_assert_equalwithin_r1
    MODULE PROCEDURE realsp_assert_equalwithin_r2
    ! REAL(Double) procedures
    MODULE PROCEDURE realdp_assert_equalwithin_s
    MODULE PROCEDURE realdp_assert_equalwithin_r1
    MODULE PROCEDURE realdp_assert_equalwithin_r2
    ! COMPLEX(Single) procedures
    MODULE PROCEDURE complexsp_assert_equalwithin_s
    MODULE PROCEDURE complexsp_assert_equalwithin_r1
    MODULE PROCEDURE complexsp_assert_equalwithin_r2
    ! COMPLEX(Double) procedures
    MODULE PROCEDURE complexdp_assert_equalwithin_s
    MODULE PROCEDURE complexdp_assert_equalwithin_r1
    MODULE PROCEDURE complexdp_assert_equalwithin_r2
  END INTERFACE UnitTest_IsEqualWithin

  INTERFACE UnitTest_IsWithinSigFig
    ! REAL(Single) procedures
    MODULE PROCEDURE realsp_assert_withinsigfig_s
    MODULE PROCEDURE realsp_assert_withinsigfig_r1
    MODULE PROCEDURE realsp_assert_withinsigfig_r2
    ! REAL(Double) procedures
    MODULE PROCEDURE realdp_assert_withinsigfig_s
    MODULE PROCEDURE realdp_assert_withinsigfig_r1
    MODULE PROCEDURE realdp_assert_withinsigfig_r2
    ! COMPLEX(Single) procedures
    MODULE PROCEDURE complexsp_assert_withinsigfig_s
    MODULE PROCEDURE complexsp_assert_withinsigfig_r1
    MODULE PROCEDURE complexsp_assert_withinsigfig_r2
    ! COMPLEX(Double) procedures
    MODULE PROCEDURE complexdp_assert_withinsigfig_s
    MODULE PROCEDURE complexdp_assert_withinsigfig_r1
    MODULE PROCEDURE complexdp_assert_withinsigfig_r2
  END INTERFACE UnitTest_IsWithinSigFig


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: UnitTest_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  INTEGER,      PARAMETER :: SL = 512
  INTEGER,      PARAMETER :: CR = 13
  INTEGER,      PARAMETER :: LF = 10
  CHARACTER(2), PARAMETER :: CRLF = ACHAR(CR)//ACHAR(LF)
  LOGICAL,      PARAMETER :: DEFAULT_VERBOSE = .FALSE.
  ! Message colours
  CHARACTER(*), PARAMETER :: GREEN_COLOUR = ACHAR(27)//'[1;32m'
  CHARACTER(*), PARAMETER :: RED_COLOUR   = ACHAR(27)//'[1;31m'
  CHARACTER(*), PARAMETER :: NO_COLOUR    = ACHAR(27)//'[0m'
  ! Message levels
  INTEGER, PARAMETER :: N_MESSAGE_LEVELS = 6
  INTEGER, PARAMETER :: INIT_LEVEL          = 1
  INTEGER, PARAMETER :: SETUP_LEVEL         = 2
  INTEGER, PARAMETER :: TEST_LEVEL          = 3
  INTEGER, PARAMETER :: REPORT_LEVEL        = 4
  INTEGER, PARAMETER :: SUMMARY_LEVEL       = 5
  INTEGER, PARAMETER :: INTERNAL_FAIL_LEVEL = 6
  CHARACTER(*), PARAMETER :: MESSAGE_LEVEL(N_MESSAGE_LEVELS) = &
    [ 'INIT            ', &
      'SETUP           ', &
      'TEST            ', &
      'REPORT          ', &
      'SUMMARY         ', &
      'INTERNAL FAILURE' ]


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
  CONTAINS
    PRIVATE
    ! Public methods
    PROCEDURE, PUBLIC, PASS(self) :: Init
    PROCEDURE, PUBLIC, PASS(self) :: Setup
    PROCEDURE, PUBLIC, PASS(self) :: Report
    PROCEDURE, PUBLIC, PASS(self) :: Summary
    PROCEDURE, PUBLIC, PASS(self) :: n_Passed
    PROCEDURE, PUBLIC, PASS(self) :: n_Failed
    PROCEDURE, PUBLIC, PASS(self) :: Passed
    PROCEDURE, PUBLIC, PASS(self) :: Failed
    PROCEDURE, PUBLIC, PASS(self) :: Assert
    PROCEDURE, PUBLIC, PASS(self) :: Refute
    GENERIC, PUBLIC :: Assert_Equal => &
      intbyte_assert_equal_s, intbyte_assert_equal_r1, intbyte_assert_equal_r2, &
      intshort_assert_equal_s, intshort_assert_equal_r1, intshort_assert_equal_r2, &
      intlong_assert_equal_s, intlong_assert_equal_r1, intlong_assert_equal_r2, &
      realsp_assert_equal_s, realsp_assert_equal_r1, realsp_assert_equal_r2, &
      realdp_assert_equal_s, realdp_assert_equal_r1, realdp_assert_equal_r2, &
      complexsp_assert_equal_s, complexsp_assert_equal_r1, complexsp_assert_equal_r2, &
      complexdp_assert_equal_s, complexdp_assert_equal_r1, complexdp_assert_equal_r2, &
      char_assert_equal_s, char_assert_equal_r1, char_assert_equal_r2
    PROCEDURE, PASS(self) :: intbyte_assert_equal_s
    PROCEDURE, PASS(self) :: intbyte_assert_equal_r1
    PROCEDURE, PASS(self) :: intbyte_assert_equal_r2
    PROCEDURE, PASS(self) :: intshort_assert_equal_s
    PROCEDURE, PASS(self) :: intshort_assert_equal_r1
    PROCEDURE, PASS(self) :: intshort_assert_equal_r2
    PROCEDURE, PASS(self) :: intlong_assert_equal_s
    PROCEDURE, PASS(self) :: intlong_assert_equal_r1
    PROCEDURE, PASS(self) :: intlong_assert_equal_r2
    PROCEDURE, PASS(self) :: realsp_assert_equal_s
    PROCEDURE, PASS(self) :: realsp_assert_equal_r1
    PROCEDURE, PASS(self) :: realsp_assert_equal_r2
    PROCEDURE, PASS(self) :: realdp_assert_equal_s
    PROCEDURE, PASS(self) :: realdp_assert_equal_r1
    PROCEDURE, PASS(self) :: realdp_assert_equal_r2
    PROCEDURE, PASS(self) :: complexsp_assert_equal_s
    PROCEDURE, PASS(self) :: complexsp_assert_equal_r1
    PROCEDURE, PASS(self) :: complexsp_assert_equal_r2
    PROCEDURE, PASS(self) :: complexdp_assert_equal_s
    PROCEDURE, PASS(self) :: complexdp_assert_equal_r1
    PROCEDURE, PASS(self) :: complexdp_assert_equal_r2
    PROCEDURE, PASS(self) :: char_assert_equal_s
    PROCEDURE, PASS(self) :: char_assert_equal_r1
    PROCEDURE, PASS(self) :: char_assert_equal_r2
    GENERIC, PUBLIC :: Refute_Equal => &
      intbyte_refute_equal_s, intbyte_refute_equal_r1, intbyte_refute_equal_r2, &
      intshort_refute_equal_s, intshort_refute_equal_r1, intshort_refute_equal_r2, &
      intlong_refute_equal_s, intlong_refute_equal_r1, intlong_refute_equal_r2, &
      realsp_refute_equal_s, realsp_refute_equal_r1, realsp_refute_equal_r2, &
      realdp_refute_equal_s, realdp_refute_equal_r1, realdp_refute_equal_r2, &
      complexsp_refute_equal_s, complexsp_refute_equal_r1, complexsp_refute_equal_r2, &
      complexdp_refute_equal_s, complexdp_refute_equal_r1, complexdp_refute_equal_r2, &
      char_refute_equal_s, char_refute_equal_r1, char_refute_equal_r2
    PROCEDURE, PASS(self) :: intbyte_refute_equal_s
    PROCEDURE, PASS(self) :: intbyte_refute_equal_r1
    PROCEDURE, PASS(self) :: intbyte_refute_equal_r2
    PROCEDURE, PASS(self) :: intshort_refute_equal_s
    PROCEDURE, PASS(self) :: intshort_refute_equal_r1
    PROCEDURE, PASS(self) :: intshort_refute_equal_r2
    PROCEDURE, PASS(self) :: intlong_refute_equal_s
    PROCEDURE, PASS(self) :: intlong_refute_equal_r1
    PROCEDURE, PASS(self) :: intlong_refute_equal_r2
    PROCEDURE, PASS(self) :: realsp_refute_equal_s
    PROCEDURE, PASS(self) :: realsp_refute_equal_r1
    PROCEDURE, PASS(self) :: realsp_refute_equal_r2
    PROCEDURE, PASS(self) :: realdp_refute_equal_s
    PROCEDURE, PASS(self) :: realdp_refute_equal_r1
    PROCEDURE, PASS(self) :: realdp_refute_equal_r2
    PROCEDURE, PASS(self) :: complexsp_refute_equal_s
    PROCEDURE, PASS(self) :: complexsp_refute_equal_r1
    PROCEDURE, PASS(self) :: complexsp_refute_equal_r2
    PROCEDURE, PASS(self) :: complexdp_refute_equal_s
    PROCEDURE, PASS(self) :: complexdp_refute_equal_r1
    PROCEDURE, PASS(self) :: complexdp_refute_equal_r2
    PROCEDURE, PASS(self) :: char_refute_equal_s
    PROCEDURE, PASS(self) :: char_refute_equal_r1
    PROCEDURE, PASS(self) :: char_refute_equal_r2
    GENERIC, PUBLIC :: Assert_EqualWithin => &
      realsp_assert_equalwithin_s, realsp_assert_equalwithin_r1, realsp_assert_equalwithin_r2, &
      realdp_assert_equalwithin_s, realdp_assert_equalwithin_r1, realdp_assert_equalwithin_r2, &
      complexsp_assert_equalwithin_s, complexsp_assert_equalwithin_r1, complexsp_assert_equalwithin_r2, &
      complexdp_assert_equalwithin_s, complexdp_assert_equalwithin_r1, complexdp_assert_equalwithin_r2
    PROCEDURE, PASS(self) :: realsp_assert_equalwithin_s
    PROCEDURE, PASS(self) :: realsp_assert_equalwithin_r1
    PROCEDURE, PASS(self) :: realsp_assert_equalwithin_r2
    PROCEDURE, PASS(self) :: realdp_assert_equalwithin_s
    PROCEDURE, PASS(self) :: realdp_assert_equalwithin_r1
    PROCEDURE, PASS(self) :: realdp_assert_equalwithin_r2
    PROCEDURE, PASS(self) :: complexsp_assert_equalwithin_s
    PROCEDURE, PASS(self) :: complexsp_assert_equalwithin_r1
    PROCEDURE, PASS(self) :: complexsp_assert_equalwithin_r2
    PROCEDURE, PASS(self) :: complexdp_assert_equalwithin_s
    PROCEDURE, PASS(self) :: complexdp_assert_equalwithin_r1
    PROCEDURE, PASS(self) :: complexdp_assert_equalwithin_r2
    GENERIC, PUBLIC :: Refute_EqualWithin => &
      realsp_refute_equalwithin_s, realsp_refute_equalwithin_r1, realsp_refute_equalwithin_r2, &
      realdp_refute_equalwithin_s, realdp_refute_equalwithin_r1, realdp_refute_equalwithin_r2, &
      complexsp_refute_equalwithin_s, complexsp_refute_equalwithin_r1, complexsp_refute_equalwithin_r2, &
      complexdp_refute_equalwithin_s, complexdp_refute_equalwithin_r1, complexdp_refute_equalwithin_r2
    PROCEDURE, PASS(self) :: realsp_refute_equalwithin_s
    PROCEDURE, PASS(self) :: realsp_refute_equalwithin_r1
    PROCEDURE, PASS(self) :: realsp_refute_equalwithin_r2
    PROCEDURE, PASS(self) :: realdp_refute_equalwithin_s
    PROCEDURE, PASS(self) :: realdp_refute_equalwithin_r1
    PROCEDURE, PASS(self) :: realdp_refute_equalwithin_r2
    PROCEDURE, PASS(self) :: complexsp_refute_equalwithin_s
    PROCEDURE, PASS(self) :: complexsp_refute_equalwithin_r1
    PROCEDURE, PASS(self) :: complexsp_refute_equalwithin_r2
    PROCEDURE, PASS(self) :: complexdp_refute_equalwithin_s
    PROCEDURE, PASS(self) :: complexdp_refute_equalwithin_r1
    PROCEDURE, PASS(self) :: complexdp_refute_equalwithin_r2
    GENERIC, PUBLIC :: Assert_WithinSigfig => &
      realsp_assert_withinsigfig_s, realsp_assert_withinsigfig_r1, realsp_assert_withinsigfig_r2, &
      realdp_assert_withinsigfig_s, realdp_assert_withinsigfig_r1, realdp_assert_withinsigfig_r2, &
      complexsp_assert_withinsigfig_s, complexsp_assert_withinsigfig_r1, complexsp_assert_withinsigfig_r2, &
      complexdp_assert_withinsigfig_s, complexdp_assert_withinsigfig_r1, complexdp_assert_withinsigfig_r2
    PROCEDURE, PASS(self) :: realsp_assert_withinsigfig_s
    PROCEDURE, PASS(self) :: realsp_assert_withinsigfig_r1
    PROCEDURE, PASS(self) :: realsp_assert_withinsigfig_r2
    PROCEDURE, PASS(self) :: realdp_assert_withinsigfig_s
    PROCEDURE, PASS(self) :: realdp_assert_withinsigfig_r1
    PROCEDURE, PASS(self) :: realdp_assert_withinsigfig_r2
    PROCEDURE, PASS(self) :: complexsp_assert_withinsigfig_s
    PROCEDURE, PASS(self) :: complexsp_assert_withinsigfig_r1
    PROCEDURE, PASS(self) :: complexsp_assert_withinsigfig_r2
    PROCEDURE, PASS(self) :: complexdp_assert_withinsigfig_s
    PROCEDURE, PASS(self) :: complexdp_assert_withinsigfig_r1
    PROCEDURE, PASS(self) :: complexdp_assert_withinsigfig_r2
    GENERIC, PUBLIC :: Refute_WithinSigfig => &
      realsp_refute_withinsigfig_s, realsp_refute_withinsigfig_r1, realsp_refute_withinsigfig_r2, &
      realdp_refute_withinsigfig_s, realdp_refute_withinsigfig_r1, realdp_refute_withinsigfig_r2, &
      complexsp_refute_withinsigfig_s, complexsp_refute_withinsigfig_r1, complexsp_refute_withinsigfig_r2, &
      complexdp_refute_withinsigfig_s, complexdp_refute_withinsigfig_r1, complexdp_refute_withinsigfig_r2
    PROCEDURE, PASS(self) :: realsp_refute_withinsigfig_s
    PROCEDURE, PASS(self) :: realsp_refute_withinsigfig_r1
    PROCEDURE, PASS(self) :: realsp_refute_withinsigfig_r2
    PROCEDURE, PASS(self) :: realdp_refute_withinsigfig_s
    PROCEDURE, PASS(self) :: realdp_refute_withinsigfig_r1
    PROCEDURE, PASS(self) :: realdp_refute_withinsigfig_r2
    PROCEDURE, PASS(self) :: complexsp_refute_withinsigfig_s
    PROCEDURE, PASS(self) :: complexsp_refute_withinsigfig_r1
    PROCEDURE, PASS(self) :: complexsp_refute_withinsigfig_r2
    PROCEDURE, PASS(self) :: complexdp_refute_withinsigfig_s
    PROCEDURE, PASS(self) :: complexdp_refute_withinsigfig_r1
    PROCEDURE, PASS(self) :: complexdp_refute_withinsigfig_r2
    ! Private methods
    PROCEDURE, PASS(self) :: Set_Property
    PROCEDURE, PASS(self) :: Get_Property
    PROCEDURE, PASS(self) :: Test_Passed
    PROCEDURE, PASS(self) :: Test_Failed
    PROCEDURE, PASS(self) :: Test_Increment
    PROCEDURE, PASS(self) :: Display_Message
    PROCEDURE, PASS(self) :: Test_Info_String
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

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Init
!
! PURPOSE:
!   UnitTest initialisation method.
!
!   This method should be called ONCE, BEFORE ANY tests are performed.
!
! CALLING SEQUENCE:
!   CALL utest%Init( Verbose=Verbose )
!
! OBJECTS:
!   utest:    UnitTest object.
!             UNITS:      N/A
!             CLASS:      UnitTest_type
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!   Verbose:  Logical argument to control length of reporting output.
!             If == .FALSE., Only failed tests are reported [DEFAULT].
!                == .TRUE.,  Both failed and passed tests are reported.
!             If not specified, default is .TRUE.
!             UNITS:      N/A
!             TYPE:       LOGICAL
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Init( self, Verbose )
    ! Arguments
    CLASS(UnitTest_type), INTENT(OUT) :: self
    LOGICAL,    OPTIONAL, INTENT(IN)  :: Verbose
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Init'

    ! Perform initialisation
    CALL Set_Property( &
      self, &
      Verbose           = Verbose , &
      Level             = INIT_LEVEL    , &
      Procedure         = PROCEDURE_NAME, &
      n_Tests           = 0, &
      n_Passed_Tests    = 0, &
      n_Failed_Tests    = 0, &
      n_AllTests        = 0, &
      n_Passed_AllTests = 0, &
      n_Failed_AllTests = 0  )

    CALL Display_Message( self )

  END SUBROUTINE Init


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Setup
!
! PURPOSE:
!   Individual test setup method.
!
!   This method should be called BEFORE each set of tests performed.
!
! CALLING SEQUENCE:
!   CALL utest_obj&Setup( Title            , &
!                         Caller  = Caller , &
!                         Verbose = Verbose  )
!
! OBJECT:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Title:      Character string containing the title of the test
!               to be performed.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Caller:     Character string containing the name of the calling
!               subprogram. If not specified, default is an empty string.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Verbose:    Logical argument to control length of reporting output.
!               If == .FALSE., Only failed tests are reported [DEFAULT].
!                  == .TRUE.,  Both failed and passed tests are reported.
!               If not specified, default is .TRUE.
!               UNITS:      N/A
!               TYPE:       LOGICAL
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Setup( self, Title, Caller, Verbose )
    ! Arguments
    CLASS(UnitTest_type)  , INTENT(IN OUT) :: self
    CHARACTER(*)          , INTENT(IN)     :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Caller
    LOGICAL,      OPTIONAL, INTENT(IN)     :: Verbose
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Setup'
    ! Variables
    CHARACTER(SL) :: the_caller
    CHARACTER(SL) :: message

    ! Check optional arguments
    the_caller = ''
    IF ( PRESENT(Caller) ) the_caller = '; CALLER: '//TRIM(ADJUSTL(Caller))

    ! Create setup message
    message = TRIM(ADJUSTL(Title))//TRIM(the_caller)

    ! Perform initialistion
    CALL Set_Property( &
      self, &
      Title          = Title         , &
      Caller         = Caller        , &
      Verbose        = Verbose       , &
      Level          = SETUP_LEVEL   , &
      Procedure      = PROCEDURE_NAME, &
      Message        = message       , &
      n_Tests        = 0             , &
      n_Passed_Tests = 0             , &
      n_Failed_Tests = 0               )

    CALL Display_Message( self )

  END SUBROUTINE Setup


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Report
!
! PURPOSE:
!   Individual test report method.
!
!   This method should be called AFTER each set of tests performed.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Report()
!
! OBJECT:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Report( self )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Report'
    ! Variables
    INTEGER :: n_tests
    INTEGER :: n_passed_tests
    INTEGER :: n_failed_tests
    CHARACTER(SL) :: message
    CHARACTER(SL) :: attention
    CHARACTER(SL) :: colour

    ! Retrieve required properties
    CALL Get_Property( &
      self, &
      n_Tests        = n_tests       , &
      n_Passed_Tests = n_passed_tests, &
      n_Failed_Tests = n_failed_tests  )

    ! Test fail attention-grabber
    colour    = GREEN_COLOUR
    attention = ''
    IF ( n_failed_tests /= 0 ) THEN
      colour    = RED_COLOUR
      attention = '  <----<<<  **WARNING**'
    END IF

    ! Generate report message
    WRITE( message, &
      '(a,a,3x,"Passed ",i0," of ",i0," tests", &
         &a,3x,"Failed ",i0," of ",i0," tests",a,a)') &
      TRIM(colour), CRLF, &
      n_passed_tests, n_tests, &
      CRLF, &
      n_failed_tests, n_tests, &
      TRIM(attention), NO_COLOUR

    ! Load object with report message
    CALL Set_Property( &
      self, &
      Level     = REPORT_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = Message )

    ! Report!
    CALL Display_Message( self )

  END SUBROUTINE Report


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Summary
!
! PURPOSE:
!   Test suite report summary method.
!
!   This method should be called ONCE, AFTER ALL tests are performed.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Summary()
!
! OBJECT:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Summary( self )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Summary'
    ! Variables
    INTEGER :: n_alltests
    INTEGER :: n_passed_alltests
    INTEGER :: n_failed_alltests
    CHARACTER(SL) :: message
    CHARACTER(SL) :: attention
    CHARACTER(SL) :: colour

    ! Retrieve required properties
    CALL Get_Property( &
      self, &
      n_AllTests        = n_alltests       , &
      n_Passed_AllTests = n_passed_alltests, &
      n_Failed_AllTests = n_failed_alltests  )

    ! Test fail attention-grabber
    colour    = GREEN_COLOUR
    attention = ''
    IF ( n_failed_alltests /= 0 ) THEN
      colour    = RED_COLOUR
      attention = '  <----<<<  **WARNING**'
    END IF

    ! Generate summary
    WRITE( message, &
      '(a,a,1x,"Passed ",i0," of ",i0," total tests",&
         &a,1x,"Failed ",i0," of ",i0," total tests",a,a)') &
      TRIM(colour), CRLF, &
      n_passed_alltests, n_alltests, &
      CRLF, &
      n_failed_alltests, n_alltests, &
      TRIM(attention), NO_COLOUR

    ! Load object with summary message
    CALL Set_Property( &
      self, &
      Level     = SUMMARY_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )

    ! Summarise!
    CALL Display_Message( self )

  END SUBROUTINE Summary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::n_Passed
!
! PURPOSE:
!   Method to return the number of tests passed.
!
! CALLING SEQUENCE:
!   n = utest_obj%n_Passed()
!
! OBJECT:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   n:          The number of exercised unit tests that have passed.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE INTEGER FUNCTION n_Passed( self )
    CLASS(UnitTest_type), INTENT(IN) :: self
    CALL Get_Property( self, n_Passed_Tests = n_Passed )
  END FUNCTION n_Passed


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::n_Failed
!
! PURPOSE:
!   Method to return the number of tests failed.
!
! CALLING SEQUENCE:
!   n = utest_obj%n_Failed()
!
! OBJECT:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   n:          The number of exercised unit tests that have failed.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE INTEGER FUNCTION n_Failed( self )
    CLASS(UnitTest_type), INTENT(IN) :: self
    CALL Get_Property( self, n_Failed_Tests = n_Failed )
  END FUNCTION n_Failed


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Passed
!
! PURPOSE:
!   Method to inform if the last test performed passed.
!
! CALLING SEQUENCE:
!   result = utest_obj%Passed()
!
! OBJECT:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   result:     Logical to indicate if the last test performed passed.
!               If == .TRUE.,  the last test passed,
!                  == .FALSE., the last test failed.
!               UNITS:      N/A
!               TYPE:       LOGICAL
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE LOGICAL FUNCTION Passed( self )
    CLASS(UnitTest_type), INTENT(IN) :: self
    CALL Get_Property( self, Test_Result = Passed )
  END FUNCTION Passed


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Failed
!
! PURPOSE:
!   Method to inform if the last test performed failed.
!
!   Syntactic sugar procedure.
!
! CALLING SEQUENCE:
!   result = utest_obj%Failed()
!
! OBJECT:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   result:     Logical to indicate if the last test performed failed.
!               If == .TRUE.,  the last test failed,
!                  == .FALSE., the last test passed.
!               UNITS:      N/A
!               TYPE:       LOGICAL
!               DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE LOGICAL FUNCTION Failed( self )
    CLASS(UnitTest_type), INTENT(IN) :: self
    Failed = .NOT. self%Passed()
  END FUNCTION Failed


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Assert
!
! PURPOSE:
!   Method to assert its logical argument as true.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Assert( boolean )
!
! OBJECTS:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   boolean:    The logical expression to assert. The test passes if the
!               expression is .TRUE.
!               UNITS:      N/A
!               TYPE:       LOGICAL
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Assert(self, boolean)
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    LOGICAL,              INTENT(IN)     :: boolean
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert'
    ! Variables
    LOGICAL :: verbose
    CHARACTER(SL) :: message

    ! Setup
    message = ''
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. boolean)  ! Always output test failure

    ! Assert the test
    IF ( boolean ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF

    ! Generate the assertion message
    CALL Test_Info_String( self, message )
    
    ! Load the object with message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL    , &
      Procedure = PROCEDURE_NAME, &
      Message   = message         )
      
    ! Output the assertion result
    IF ( verbose ) CALL Display_Message( self )

  END SUBROUTINE Assert


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Refute
!
! PURPOSE:
!   Method to refute its logical argument as false
!
! CALLING SEQUENCE:
!   CALL utest_obj%Assert( boolean )
!
! OBJECTS:
!   utest_obj:  UnitTest object.
!               UNITS:      N/A
!               CLASS:      UnitTest_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   boolean:    The logical expression to refute. The test passes if the
!               expression is .FALSE.
!               UNITS:      N/A
!               TYPE:       LOGICAL
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Refute(self, boolean)
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    LOGICAL,              INTENT(IN)     :: boolean
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute'
    ! Variables
    LOGICAL :: verbose
    CHARACTER(SL) :: message

    ! Setup
    message = ''
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. boolean  ! Always output test failure

    ! Refute the test
    IF ( .NOT. boolean ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF

    ! Generate the refutation message
    CALL Test_Info_String( self, message )
    
    ! Load the object with message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL    , &
      Procedure = PROCEDURE_NAME, &
      Message   = message         )
      
    ! Output the refuation result
    IF ( verbose ) CALL Display_Message( self )

  END SUBROUTINE Refute


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Assert_Equal
!
! PURPOSE:
!   Method to assert that two arguments are equal.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Assert_Equal( Expected, Actual )
!
! OBJECTS:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Expected:      The expected value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       INTEGER(Byte)  , or
!                              INTEGER(Short) , or
!                              INTEGER(Long)  , or
!                              REAL(Single)   , or
!                              REAL(Double)   , or
!                              COMPLEX(Single), or
!                              COMPLEX(Double), or
!                              CHARACTER(*)
!                  DIMENSION:  Scalar, or
!                              Rank-1, or
!                              Rank-2
!                  ATTRIBUTES: INTENT(IN)
!
!   Actual:        The actual value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE intbyte_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Byte), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Byte)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",i0,a,&
                       &7x,"And got:  ",i0)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE intbyte_assert_equal_s
  
  
  SUBROUTINE intbyte_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Byte), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Byte)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intbyte_assert_equal_r1
  
  
  SUBROUTINE intbyte_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Byte), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Byte)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intbyte_assert_equal_r2
  
  
  SUBROUTINE intshort_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Short), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Short)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",i0,a,&
                       &7x,"And got:  ",i0)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE intshort_assert_equal_s
  
  
  SUBROUTINE intshort_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Short), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Short)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intshort_assert_equal_r1
  
  
  SUBROUTINE intshort_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Short), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Short)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intshort_assert_equal_r2
  
  
  SUBROUTINE intlong_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Long), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Long)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",i0,a,&
                       &7x,"And got:  ",i0)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE intlong_assert_equal_s
  
  
  SUBROUTINE intlong_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Long), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Long)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intlong_assert_equal_r1
  
  
  SUBROUTINE intlong_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Long), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Long)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intlong_assert_equal_r2
  
  
  SUBROUTINE realsp_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Single)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
                       &7x,"And got:  ",es25.18)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realsp_assert_equal_s
  
  
  SUBROUTINE realsp_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE realsp_assert_equal_r1
  
  
  SUBROUTINE realsp_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE realsp_assert_equal_r2
  
  
  SUBROUTINE realdp_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Double)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
                       &7x,"And got:  ",es25.18)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realdp_assert_equal_s
  
  
  SUBROUTINE realdp_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE realdp_assert_equal_r1
  
  
  SUBROUTINE realdp_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE realdp_assert_equal_r2
  
  
  SUBROUTINE complexsp_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Single)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
                       &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexsp_assert_equal_s
  
  
  SUBROUTINE complexsp_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE complexsp_assert_equal_r1
  
  
  SUBROUTINE complexsp_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE complexsp_assert_equal_r2
  
  
  SUBROUTINE complexdp_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Double)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
                       &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexdp_assert_equal_s
  
  
  SUBROUTINE complexdp_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE complexdp_assert_equal_r1
  
  
  SUBROUTINE complexdp_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE complexdp_assert_equal_r2
  
  
  SUBROUTINE char_assert_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    CHARACTER(*), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[CHARACTER(*)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = (Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",">",a,"<",a,&
                       &7x,"And got:  ",">",a,"<")') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE char_assert_equal_s
  
  
  SUBROUTINE char_assert_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    CHARACTER(*), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[CHARACTER(*)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE char_assert_equal_r1
  
  
  SUBROUTINE char_assert_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    CHARACTER(*), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[CHARACTER(*)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE char_assert_equal_r2
  
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Refute_Equal
!
! PURPOSE:
!   Method to refute that two arguments are equal.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Refute_Equal( Expected, Actual )
!
! OBJECTS:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Expected:      The expected value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       INTEGER(Byte)  , or
!                              INTEGER(Short) , or
!                              INTEGER(Long)  , or
!                              REAL(Single)   , or
!                              REAL(Double)   , or
!                              COMPLEX(Single), or
!                              COMPLEX(Double), or
!                              CHARACTER(*)
!                  DIMENSION:  Scalar, or
!                              Rank-1, or
!                              Rank-2
!                  ATTRIBUTES: INTENT(IN)
!
!   Actual:        The actual value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE intbyte_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Byte), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Byte)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",i0,a,&
                       &7x,"And got:  ",i0)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE intbyte_refute_equal_s
  
  
  SUBROUTINE intbyte_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Byte), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Byte)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intbyte_refute_equal_r1
  
  
  SUBROUTINE intbyte_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Byte), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Byte)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intbyte_refute_equal_r2
  
  
  SUBROUTINE intshort_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Short), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Short)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",i0,a,&
                       &7x,"And got:  ",i0)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE intshort_refute_equal_s
  
  
  SUBROUTINE intshort_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Short), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Short)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intshort_refute_equal_r1
  
  
  SUBROUTINE intshort_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Short), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Short)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intshort_refute_equal_r2
  
  
  SUBROUTINE intlong_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Long), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Long)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",i0,a,&
                       &7x,"And got:  ",i0)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE intlong_refute_equal_s
  
  
  SUBROUTINE intlong_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Long), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Long)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE intlong_refute_equal_r1
  
  
  SUBROUTINE intlong_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER(Long), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Long)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE intlong_refute_equal_r2
  
  
  SUBROUTINE realsp_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Single)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
                       &7x,"And got:  ",es25.18)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realsp_refute_equal_s
  
  
  SUBROUTINE realsp_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE realsp_refute_equal_r1
  
  
  SUBROUTINE realsp_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE realsp_refute_equal_r2
  
  
  SUBROUTINE realdp_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Double)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
                       &7x,"And got:  ",es25.18)') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realdp_refute_equal_s
  
  
  SUBROUTINE realdp_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE realdp_refute_equal_r1
  
  
  SUBROUTINE realdp_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE realdp_refute_equal_r2
  
  
  SUBROUTINE complexsp_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Single)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
                       &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexsp_refute_equal_s
  
  
  SUBROUTINE complexsp_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE complexsp_refute_equal_r1
  
  
  SUBROUTINE complexsp_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE complexsp_refute_equal_r2
  
  
  SUBROUTINE complexdp_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Double)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected .EqualTo. Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
                       &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexdp_refute_equal_s
  
  
  SUBROUTINE complexdp_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE complexdp_refute_equal_r1
  
  
  SUBROUTINE complexdp_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE complexdp_refute_equal_r2
  
  
  SUBROUTINE char_refute_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    CHARACTER(*), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[CHARACTER(*)]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = .NOT.(Expected == Actual)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( message, '(a,7x,"Expected: ",">",a,"<",a,&
                       &7x,"And got:  ",">",a,"<")') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE char_refute_equal_s
  
  
  SUBROUTINE char_refute_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    CHARACTER(*), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[CHARACTER(*)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE char_refute_equal_r1
  
  
  SUBROUTINE char_refute_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    CHARACTER(*), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[CHARACTER(*)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE char_refute_equal_r2
  


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Assert_EqualWithin
!
! PURPOSE:
!   Method to assert that two floating point arguments are equal to
!   within the specified tolerance.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Assert_EqualWithin( Expected, Actual, Tolerance )
!
! OBJECTS:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Expected:      The expected value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       REAL(Single)   , or
!                              REAL(Double)   , or
!                              COMPLEX(Single), or
!                              COMPLEX(Double)
!                  DIMENSION:  Scalar, or
!                              Rank-1, or
!                              Rank-2
!                  ATTRIBUTES: INTENT(IN)
!
!   Actual:        The actual value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!   Tolerance:     The tolerance to within which the Expected and Actual
!                  values must agree. If negative, the value of
!                    EPSILON(Expected)
!                  is used.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE realsp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Single)]'
    ! Variables
    REAL(Single) :: delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Local delta for test
    delta = Tolerance
    IF ( delta < 0.0_Single ) delta = EPSILON(Expected)
    ! ...Assign the test
    test = (ABS(Expected-Actual) < delta)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"To within    : ",es25.18,a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realsp_assert_equalwithin_s
  
  
  SUBROUTINE realsp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE realsp_assert_equalwithin_r1
  
  
  SUBROUTINE realsp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE realsp_assert_equalwithin_r2
  
  
  SUBROUTINE realdp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Double)]'
    ! Variables
    REAL(Double) :: delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Local delta for test
    delta = Tolerance
    IF ( delta < 0.0_Double ) delta = EPSILON(Expected)
    ! ...Assign the test
    test = (ABS(Expected-Actual) < delta)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"To within    : ",es25.18,a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realdp_assert_equalwithin_s
  
  
  SUBROUTINE realdp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE realdp_assert_equalwithin_r1
  
  
  SUBROUTINE realdp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE realdp_assert_equalwithin_r2
  
  
  SUBROUTINE complexsp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Single)]'
    ! Variables
    REAL(Single) :: deltar, deltai
    REAL(Single) :: zr, zi
    REAL(Single) :: dzr, dzi
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    zr = REAL(Expected,Single)
    zi = AIMAG(Expected)
    ! ...Local delta for test
    deltar = REAL(Tolerance,Single)
    IF ( deltar < 0.0_Single ) deltar = EPSILON(zr)
    deltai = AIMAG(Tolerance)
    IF ( deltai < 0.0_Single ) deltai = EPSILON(zi)
    ! ...Assign the test
    dzr = ABS(zr - REAL(Actual,Single))
    dzi = ABS(zi - AIMAG(Actual))
    test = ((dzr < deltar) .AND. (dzi < deltai))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"To within    : ","(",es25.18,",",es25.18,")",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, CMPLX(deltar,deltai,Single), CRLF, Actual, CRLF, dzr, dzi
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexsp_assert_equalwithin_s
  
  
  SUBROUTINE complexsp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE complexsp_assert_equalwithin_r1
  
  
  SUBROUTINE complexsp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE complexsp_assert_equalwithin_r2
  
  
  SUBROUTINE complexdp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Double)]'
    ! Variables
    REAL(Double) :: deltar, deltai
    REAL(Double) :: zr, zi
    REAL(Double) :: dzr, dzi
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    zr = REAL(Expected,Double)
    zi = AIMAG(Expected)
    ! ...Local delta for test
    deltar = REAL(Tolerance,Double)
    IF ( deltar < 0.0_Double ) deltar = EPSILON(zr)
    deltai = AIMAG(Tolerance)
    IF ( deltai < 0.0_Double ) deltai = EPSILON(zi)
    ! ...Assign the test
    dzr = ABS(zr - REAL(Actual,Double))
    dzi = ABS(zi - AIMAG(Actual))
    test = ((dzr < deltar) .AND. (dzi < deltai))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"To within    : ","(",es25.18,",",es25.18,")",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, CMPLX(deltar,deltai,Double), CRLF, Actual, CRLF, dzr, dzi
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexdp_assert_equalwithin_s
  
  
  SUBROUTINE complexdp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE complexdp_assert_equalwithin_r1
  
  
  SUBROUTINE complexdp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE complexdp_assert_equalwithin_r2
  
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Refute_EqualWithin
!
! PURPOSE:
!   Method to refute that two floating point arguments are equal to
!   within the specified tolerance.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Refute_EqualWithin( Expected, Actual, Tolerance )
!
! OBJECTS:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Expected:      The expected value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       REAL(Single)   , or
!                              REAL(Double)   , or
!                              COMPLEX(Single), or
!                              COMPLEX(Double)
!                  DIMENSION:  Scalar, or
!                              Rank-1, or
!                              Rank-2
!                  ATTRIBUTES: INTENT(IN)
!
!   Actual:        The actual value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!   Tolerance:     The tolerance to within which the Expected and Actual
!                  values must agree. If negative, the value of
!                    EPSILON(Expected)
!                  is used.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE realsp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Single)]'
    ! Variables
    REAL(Single) :: delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Local delta for test
    delta = Tolerance
    IF ( delta < 0.0_Single ) delta = EPSILON(Expected)
    ! ...Assign the test
    test = .NOT.(ABS(Expected-Actual) < delta)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"Outside of   : ",es25.18,a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realsp_refute_equalwithin_s
  
  
  SUBROUTINE realsp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE realsp_refute_equalwithin_r1
  
  
  SUBROUTINE realsp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE realsp_refute_equalwithin_r2
  
  
  SUBROUTINE realdp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Double)]'
    ! Variables
    REAL(Double) :: delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Local delta for test
    delta = Tolerance
    IF ( delta < 0.0_Double ) delta = EPSILON(Expected)
    ! ...Assign the test
    test = .NOT.(ABS(Expected-Actual) < delta)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"Outside of   : ",es25.18,a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realdp_refute_equalwithin_s
  
  
  SUBROUTINE realdp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE realdp_refute_equalwithin_r1
  
  
  SUBROUTINE realdp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE realdp_refute_equalwithin_r2
  
  
  SUBROUTINE complexsp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Single)]'
    ! Variables
    REAL(Single) :: deltar, deltai
    REAL(Single) :: zr, zi
    REAL(Single) :: dzr, dzi
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    zr = REAL(Expected,Single)
    zi = AIMAG(Expected)
    ! ...Local delta for test
    deltar = REAL(Tolerance,Single)
    IF ( deltar < 0.0_Single ) deltar = EPSILON(zr)
    deltai = AIMAG(Tolerance)
    IF ( deltai < 0.0_Single ) deltai = EPSILON(zi)
    ! ...Assign the test
    dzr = ABS(zr - REAL(Actual,Single))
    dzi = ABS(zi - AIMAG(Actual))
    test = .NOT.((dzr < deltar) .AND. (dzi < deltai))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"Outside of   : ","(",es25.18,",",es25.18,")",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, CMPLX(deltar,deltai,Single), CRLF, Actual, CRLF, dzr, dzi
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexsp_refute_equalwithin_s
  
  
  SUBROUTINE complexsp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE complexsp_refute_equalwithin_r1
  
  
  SUBROUTINE complexsp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE complexsp_refute_equalwithin_r2
  
  
  SUBROUTINE complexdp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected, Actual, Tolerance
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Double)]'
    ! Variables
    REAL(Double) :: deltar, deltai
    REAL(Double) :: zr, zi
    REAL(Double) :: dzr, dzi
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    zr = REAL(Expected,Double)
    zi = AIMAG(Expected)
    ! ...Local delta for test
    deltar = REAL(Tolerance,Double)
    IF ( deltar < 0.0_Double ) deltar = EPSILON(zr)
    deltai = AIMAG(Tolerance)
    IF ( deltai < 0.0_Double ) deltai = EPSILON(zi)
    ! ...Assign the test
    dzr = ABS(zr - REAL(Actual,Double))
    dzi = ABS(zi - AIMAG(Actual))
    test = .NOT.((dzr < deltar) .AND. (dzi < deltai))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"Outside of   : ","(",es25.18,",",es25.18,")",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, CMPLX(deltar,deltai,Double), CRLF, Actual, CRLF, dzr, dzi
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexdp_refute_equalwithin_s
  
  
  SUBROUTINE complexdp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
    END DO
  END SUBROUTINE complexdp_refute_equalwithin_r1
  
  
  SUBROUTINE complexdp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
      END DO
    END DO
  END SUBROUTINE complexdp_refute_equalwithin_r2
  


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Assert_WithinSigFig
!
! PURPOSE:
!   Method to assert that two floating point arguments are equal to
!   within the specified number of significant figures.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Assert_WithinSigFig( Expected, Actual, n_SigFig )
!
! OBJECTS:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Expected:      The expected value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       REAL(Single)   , or
!                              REAL(Double)   , or
!                              COMPLEX(Single), or
!                              COMPLEX(Double)
!                  DIMENSION:  Scalar, or
!                              Rank-1, or
!                              Rank-2
!                  ATTRIBUTES: INTENT(IN)
!
!   Actual:        The actual value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!   n_SigFig:      The number of sgnificant figures within which the
!                  expected and actual numbers are to be compared.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE realsp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Single)]'
    ! Variables
    REAL(Single) :: epsilon_delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Compute the test cutoff
    epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Single)**(EXPONENT(Expected)-1)
    ! ...Assign the test
    test = Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"To within    : ",i0," significant figures",a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realsp_assert_withinsigfig_s
  
  
  SUBROUTINE realsp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE realsp_assert_withinsigfig_r1
  
  
  SUBROUTINE realsp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE realsp_assert_withinsigfig_r2
  
  
  SUBROUTINE realdp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Double)]'
    ! Variables
    REAL(Double) :: epsilon_delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Compute the test cutoff
    epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Double)**(EXPONENT(Expected)-1)
    ! ...Assign the test
    test = Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"To within    : ",i0," significant figures",a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realdp_assert_withinsigfig_s
  
  
  SUBROUTINE realdp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE realdp_assert_withinsigfig_r1
  
  
  SUBROUTINE realdp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE realdp_assert_withinsigfig_r2
  
  
  SUBROUTINE complexsp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Single)]'
    ! Variables
    REAL(Single) :: ezr, ezi
    REAL(Single) :: azr, azi
    REAL(Single) :: epsilon_delta_r, epsilon_delta_i
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    ezr = REAL(Expected,Single)
    ezi = AIMAG(Expected)
    azr = REAL(Actual,Single)
    azi = AIMAG(Actual)
    ! ...Compute the test cutoffs
    epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Single)**(EXPONENT(ezr)-1)
    epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Single)**(EXPONENT(ezi)-1)
    ! ...Assign the test
    test = Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
           Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"To within    : ",i0," significant figures",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexsp_assert_withinsigfig_s
  
  
  SUBROUTINE complexsp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE complexsp_assert_withinsigfig_r1
  
  
  SUBROUTINE complexsp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE complexsp_assert_withinsigfig_r2
  
  
  SUBROUTINE complexdp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Double)]'
    ! Variables
    REAL(Double) :: ezr, ezi
    REAL(Double) :: azr, azi
    REAL(Double) :: epsilon_delta_r, epsilon_delta_i
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    ezr = REAL(Expected,Double)
    ezi = AIMAG(Expected)
    azr = REAL(Actual,Double)
    azi = AIMAG(Actual)
    ! ...Compute the test cutoffs
    epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Double)**(EXPONENT(ezr)-1)
    epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Double)**(EXPONENT(ezi)-1)
    ! ...Assign the test
    test = Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
           Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i)
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"To within    : ",i0," significant figures",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexdp_assert_withinsigfig_s
  
  
  SUBROUTINE complexdp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE complexdp_assert_withinsigfig_r1
  
  
  SUBROUTINE complexdp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE complexdp_assert_withinsigfig_r2
  
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   UnitTest::Refute_WithinSigFig
!
! PURPOSE:
!   Method to refute that two floating point arguments are equal to
!   within the specified number of significant figures.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Refute_WithinSigFig( Expected, Actual, n_SigFig )
!
! OBJECTS:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   Expected:      The expected value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       REAL(Single)   , or
!                              REAL(Double)   , or
!                              COMPLEX(Single), or
!                              COMPLEX(Double)
!                  DIMENSION:  Scalar, or
!                              Rank-1, or
!                              Rank-2
!                  ATTRIBUTES: INTENT(IN)
!
!   Actual:        The actual value of the variable being tested.
!                  UNITS:      N/A
!                  TYPE:       Same as Expected input
!                  DIMENSION:  Same as Expected input
!                  ATTRIBUTES: INTENT(IN)
!
!   n_SigFig:      The number of sgnificant figures within which the
!                  expected and actual numbers are to be compared.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE realsp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Single)]'
    ! Variables
    REAL(Single) :: epsilon_delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Compute the test cutoff
    epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Single)**(EXPONENT(Expected)-1)
    ! ...Assign the test
    test = .NOT.(Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"Outside of   : ",i0," significant figures",a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realsp_refute_withinsigfig_s
  
  
  SUBROUTINE realsp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE realsp_refute_withinsigfig_r1
  
  
  SUBROUTINE realsp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE realsp_refute_withinsigfig_r2
  
  
  SUBROUTINE realdp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Double)]'
    ! Variables
    REAL(Double) :: epsilon_delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Compute the test cutoff
    epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Double)**(EXPONENT(Expected)-1)
    ! ...Assign the test
    test = .NOT.(Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",es25.18,a,&
         &7x,"Outside of   : ",i0," significant figures",a,&
         &7x,"And got      : ",es25.18,a,&
         &7x,"|Difference| : ",es25.18)') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE realdp_refute_withinsigfig_s
  
  
  SUBROUTINE realdp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE realdp_refute_withinsigfig_r1
  
  
  SUBROUTINE realdp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE realdp_refute_withinsigfig_r2
  
  
  SUBROUTINE complexsp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Single)]'
    ! Variables
    REAL(Single) :: ezr, ezi
    REAL(Single) :: azr, azi
    REAL(Single) :: epsilon_delta_r, epsilon_delta_i
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    ezr = REAL(Expected,Single)
    ezi = AIMAG(Expected)
    azr = REAL(Actual,Single)
    azi = AIMAG(Actual)
    ! ...Compute the test cutoffs
    epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Single)**(EXPONENT(ezr)-1)
    epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Single)**(EXPONENT(ezi)-1)
    ! ...Assign the test
    test = .NOT.(Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
           Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"Outside of   : ",i0," significant figures",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexsp_refute_withinsigfig_s
  
  
  SUBROUTINE complexsp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE complexsp_refute_withinsigfig_r1
  
  
  SUBROUTINE complexsp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Single)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE complexsp_refute_withinsigfig_r2
  
  
  SUBROUTINE complexdp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Double)]'
    ! Variables
    REAL(Double) :: ezr, ezi
    REAL(Double) :: azr, azi
    REAL(Double) :: epsilon_delta_r, epsilon_delta_i
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    ezr = REAL(Expected,Double)
    ezi = AIMAG(Expected)
    azr = REAL(Actual,Double)
    azi = AIMAG(Actual)
    ! ...Compute the test cutoffs
    epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Double)**(EXPONENT(ezr)-1)
    epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Double)**(EXPONENT(ezi)-1)
    ! ...Assign the test
    test = .NOT.(Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
           Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
         &7x,"Outside of   : ",i0," significant figures",a,&
         &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
         &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE complexdp_refute_withinsigfig_s
  
  
  SUBROUTINE complexdp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE complexdp_refute_withinsigfig_r1
  
  
  SUBROUTINE complexdp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Double)]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE complexdp_refute_withinsigfig_r2
  



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Set_Property
!
! PURPOSE:
!   Private method to set the properties of a UnitTest object.
!
!   All WRITE access to the UnitTest object properties should be
!   done using this method.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Set_Property( Verbose           = Verbose          , &
!                                Title             = Title            , &
!                                Caller            = Caller           , &
!                                Level             = Level            , &
!                                Procedure         = Procedure        , &
!                                Message           = Message          , &
!                                Test_Result       = Test_Result      , &
!                                n_Tests           = n_Tests          , &
!                                n_Passed_Tests    = n_Passed_Tests   , &
!                                n_Failed_Tests    = n_Failed_Tests   , &
!                                n_AllTests        = n_AllTests       , &
!                                n_Passed_AllTests = n_Passed_AllTests, &
!                                n_Failed_AllTests = n_Failed_AllTests  )
!
! OBJECT:
!   utest_obj:          UnitTest object.
!                       UNITS:      N/A
!                       CLASS:      UnitTest_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!   Verbose:            Logical to control length of reporting output.
!                       If == .FALSE., Only failed tests are reported.
!                          == .TRUE.,  Both failed and passed tests are reported.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Title:              Character string containing the title of the
!                       test to be performed.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Caller:             Character string containing the name of the
!                       calling subprogram.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Level:              Integer flag specifying the output message level.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Procedure:          The name of the UnitTest procedure.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Message:            Character string containing an informational
!                       message about the unit test performed.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Test_Result:        Logical to contain the result of unit tests
!                       performed
!                       If == .TRUE.,  Test passed.
!                          == .FALSE., Test failed.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   n_Tests:            The number of tests performed for the current
!                       unit test type, i.e. since the last call to
!                       UnitTest_Setup().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   n_Passed_Tests:     The number of tests passed for the current
!                       unit test type, i.e. since the last call to
!                       UnitTest_Setup().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   n_Failed_Tests:     The number of tests failed for the current
!                       unit test type, i.e. since the last call to
!                       UnitTest_Setup().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   n_AllTests:         The total number of tests performed, i.e. since
!                       the last call to UnitTest_Init().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   n_Passed_AllTests:  The total number of tests passed, i.e. since
!                       the last call to UnitTest_Init().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   n_Failed_AllTests:  The total number of tests failed, i.e. since
!                       the last call to UnitTest_Init().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!--------------------------------------------------------------------------------

  PURE SUBROUTINE Set_Property( &
    self             , & ! Object
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
    CLASS(UnitTest_type)  , INTENT(IN OUT) :: self
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
    IF ( PRESENT(Verbose          ) ) self%Verbose           = Verbose
    IF ( PRESENT(Title            ) ) self%Title             = Title
    IF ( PRESENT(Caller           ) ) self%Caller            = Caller
    IF ( PRESENT(Level            ) ) self%Level             = Level
    IF ( PRESENT(Procedure        ) ) self%Procedure         = Procedure
    IF ( PRESENT(Message          ) ) self%Message           = Message
    IF ( PRESENT(Test_Result      ) ) self%Test_Result       = Test_Result
    IF ( PRESENT(n_Tests          ) ) self%n_Tests           = n_Tests
    IF ( PRESENT(n_Passed_Tests   ) ) self%n_Passed_Tests    = n_Passed_Tests
    IF ( PRESENT(n_Failed_Tests   ) ) self%n_Failed_Tests    = n_Failed_Tests
    IF ( PRESENT(n_AllTests       ) ) self%n_AllTests        = n_AllTests
    IF ( PRESENT(n_Passed_AllTests) ) self%n_Passed_AllTests = n_Passed_AllTests
    IF ( PRESENT(n_Failed_AllTests) ) self%n_Failed_AllTests = n_Failed_AllTests
  END SUBROUTINE Set_Property


!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Get_Property
!
! PURPOSE:
!   Private method to get the properties of a UnitTest object.
!
!   All READ access to the UnitTest object properties should be
!   done using this method.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Get_Property( Verbose           = Verbose          , &
!                                Title             = Title            , &
!                                Caller            = Caller           , &
!                                Level             = Level            , &
!                                Procedure         = Procedure        , &
!                                Message           = Message          , &
!                                Test_Result       = Test_Result      , &
!                                n_Tests           = n_Tests          , &
!                                n_Passed_Tests    = n_Passed_Tests   , &
!                                n_Failed_Tests    = n_Failed_Tests   , &
!                                n_AllTests        = n_AllTests       , &
!                                n_Passed_AllTests = n_Passed_AllTests, &
!                                n_Failed_AllTests = n_Failed_AllTests  )
!
! OBJECT:
!   utest_obj:          UnitTest object.
!                       UNITS:      N/A
!                       CLASS:      UnitTest_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!   Verbose:            Logical to control length of reporting output.
!                       If == .FALSE., Only failed tests are reported.
!                          == .TRUE.,  Both failed and passed tests are reported.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   Title:              Character string containing the title of the
!                       test to be performed.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   Caller:             Character string containing the name of the
!                       calling subprogram.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   Level:              Integer flag specifying the output message level.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   Procedure:          The name of the last UnitTest Procedure called.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Message:            Character string containing an informational
!                       message about the last unit test performed.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   Test_Result:        Logical containing the result of the last
!                       unit test performed
!                       If == .TRUE.,  Test passed.
!                          == .FALSE., Test failed.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   n_Tests:            The number of tests performed for the current
!                       unit test type, i.e. since the last call to
!                       UnitTest_Setup().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   n_Passed_Tests:     The number of tests passed for the current
!                       unit test type, i.e. since the last call to
!                       UnitTest_Setup().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   n_Failed_Tests:     The number of tests failed for the current
!                       unit test type, i.e. since the last call to
!                       UnitTest_Setup().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   n_AllTests:         The total number of tests performed, i.e. since
!                       the last call to UnitTest_Init().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   n_Passed_AllTests:  The total number of tests passed, i.e. since
!                       the last call to UnitTest_Init().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   n_Failed_AllTests:  The total number of tests failed, i.e. since
!                       the last call to UnitTest_Init().
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  PURE SUBROUTINE Get_Property( &
    self             , & ! Object
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
    CLASS(UnitTest_type)  , INTENT(IN)  :: self
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
    IF ( PRESENT(Verbose          ) ) Verbose           = self%Verbose
    IF ( PRESENT(Title            ) ) Title             = self%Title
    IF ( PRESENT(Caller           ) ) Caller            = self%Caller
    IF ( PRESENT(Level            ) ) Level             = self%Level
    IF ( PRESENT(Procedure        ) ) Procedure         = self%Procedure
    IF ( PRESENT(Message          ) ) Message           = self%Message
    IF ( PRESENT(Test_Result      ) ) Test_Result       = self%Test_Result
    IF ( PRESENT(n_Tests          ) ) n_Tests           = self%n_Tests
    IF ( PRESENT(n_Passed_Tests   ) ) n_Passed_Tests    = self%n_Passed_Tests
    IF ( PRESENT(n_Failed_Tests   ) ) n_Failed_Tests    = self%n_Failed_Tests
    IF ( PRESENT(n_AllTests       ) ) n_AllTests        = self%n_AllTests
    IF ( PRESENT(n_Passed_AllTests) ) n_Passed_AllTests = self%n_Passed_AllTests
    IF ( PRESENT(n_Failed_AllTests) ) n_Failed_AllTests = self%n_Failed_AllTests
  END SUBROUTINE Get_Property


!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Test_Passed
!
! PURPOSE:
!   Private method to increment passed test counters.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Test_Passed()
!
! OBJECT:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Test_Passed( self )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    ! Variables
    INTEGER :: n_Passed_Tests, n_Passed_AllTests

    ! Increment total test counters
    CALL self%Test_Increment()

    ! Increment the passed test counters
    ! ...Get 'em
    CALL self%Get_Property( &
      n_Passed_Tests    = n_Passed_Tests, &
      n_Passed_AllTests = n_Passed_AllTests )
    ! ...Increment
    n_Passed_Tests    = n_Passed_Tests    + 1
    n_Passed_AllTests = n_Passed_AllTests + 1
    ! ...Save 'em and set successful test result
    CALL self%Set_Property( &
      Test_Result       = .TRUE., &
      n_Passed_Tests    = n_Passed_Tests, &
      n_Passed_AllTests = n_Passed_AllTests )
  END SUBROUTINE Test_Passed


!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Test_Failed
!
! PURPOSE:
!   Private method to increment failed test counters.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Test_Failed()
!
! OBJECT:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Test_Failed( self )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    ! Variables
    INTEGER :: n_Failed_Tests, n_Failed_AllTests

    ! Increment total test counters
    CALL self%Test_Increment()

    ! Increment the failed test counters
    ! ...Get 'em
    CALL self%Get_Property( &
      n_Failed_Tests    = n_Failed_Tests, &
      n_Failed_AllTests = n_Failed_AllTests )
    ! ...Increment
    n_Failed_Tests    = n_Failed_Tests    + 1
    n_Failed_AllTests = n_Failed_AllTests + 1
    ! ...Save 'em and set unsuccessful test result
    CALL self%Set_Property( &
      Test_Result       = .FALSE., &
      n_Failed_Tests    = n_Failed_Tests, &
      n_Failed_AllTests = n_Failed_AllTests )
  END SUBROUTINE Test_Failed


!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Test_Increment
!
! PURPOSE:
!   Private method to increment the test total counters.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Test_Increment()
!
! OBJECT:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Test_Increment( self )
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    INTEGER :: n_Tests, n_AllTests

    CALL self%Get_Property( &
      n_Tests    = n_Tests, &
      n_AllTests = n_AllTests )

    n_Tests    = n_Tests    + 1
    n_AllTests = n_AllTests + 1

    CALL self%Set_Property( &
      n_Tests    = n_Tests, &
      n_AllTests = n_AllTests )
  END SUBROUTINE Test_Increment


!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Display_Message
!
! PURPOSE:
!   Private method to display the unit test messages to stdout.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Display_Message()
!
! OBJECT:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Display_Message( self )
    CLASS(UnitTest_type), INTENT(IN) :: self
    ! Variables
    INTEGER :: level
    CHARACTER(SL) :: procedure
    CHARACTER(SL) :: message
    CHARACTER(SL) :: fmt
    CHARACTER(SL) :: prefix
    CHARACTER(SL) :: test_info
    INTEGER :: n_spaces

    CALL self%Get_Property( &
      Level     = level, &
      Procedure = procedure, &
      Message   = message )

    ! Set output bits manually
    test_info = ''
    SELECT CASE(level)
      CASE(INIT_LEVEL)
        prefix = '/'
        n_spaces = 1
      CASE(SETUP_LEVEL)
        prefix = '/,3x,14("-"),/'
        n_spaces = 3
      CASE(TEST_LEVEL)
        prefix = ''
        n_spaces = 5
        CALL self%Test_Info_String( test_info )
      CASE(REPORT_LEVEL)
        prefix = ''
        n_spaces = 3
      CASE(SUMMARY_LEVEL)
        prefix = '/,1x,16("="),/'
        n_spaces = 1
      CASE DEFAULT
        level = INTERNAL_FAIL_LEVEL
        prefix = '/,"INVALID MESSAGE LEVEL!!",/'
        n_spaces = 15
    END SELECT

    ! Write the message to stdout
    WRITE(fmt, '("(",a,i0,"x,""("",a,"") "",a,"": "",a,1x,a)")') TRIM(prefix), n_spaces
    WRITE( *,FMT=fmt ) TRIM(MESSAGE_LEVEL(level)), TRIM(procedure), TRIM(test_info), TRIM(message)

  END SUBROUTINE Display_Message


!--------------------------------------------------------------------------------
!
! NAME:
!   UnitTest::Test_Info_String
!
! PURPOSE:
!   Private method to construct an info string for message output.
!
! CALLING SEQUENCE:
!   CALL utest_obj%Test_Info_String( info )
!
! OBJECT:
!   utest_obj:     UnitTest object.
!                  UNITS:      N/A
!                  CLASS:      UnitTest_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!   info:          Character string containing the test number and
!                  whether the test passed or failed.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Test_Info_String( self, info )
    CLASS(UnitTest_Type), INTENT(IN)  :: self
    CHARACTER(*),         INTENT(OUT) :: info
    INTEGER :: n_tests
    CHARACTER(6) :: passfail
    CALL self%Get_Property( n_Tests = n_Tests )
    IF ( self%Passed() ) THEN
      passfail = 'PASSED'
    ELSE
      passfail = 'FAILED'
    END IF
    WRITE( info,'("Test#",i0,1x,a,".")') n_tests, passfail
  END SUBROUTINE Test_Info_String

END MODULE UnitTest_Define
