!
! Sub-system tests for esmf_time_f90
!
! Someday, switch over to funit!  
!

MODULE my_tests
  USE ESMF_Mod
  IMPLICIT NONE

  ! Set this to .TRUE. to make wrf_error_fatal3() print a message on failure 
  ! instead of stopping the program.  Use for testing only (since we cannot 
  ! catch exceptions in Fortran90!!)  
  LOGICAL :: WRF_ERROR_FATAL_PRINT = .FALSE.

CONTAINS

  ! Test printing of an ESMF_Time or ESMF_TimeInterval object.  
  !
  ! Correct results are also passed in through this interface and compared 
  ! with computed results.  PASS/FAIL messages are printed.  
  !
  SUBROUTINE test_print(  t_yy,  t_mm,  t_dd,  t_h,  t_m,  t_s, t_sn, t_sd, &
                         ti_yy, ti_mm, ti_dd, ti_h, ti_m, ti_s, ti_sn, ti_sd, &
                         res_str, testname, expect_error )
    INTEGER, INTENT(IN), OPTIONAL :: t_YY
    INTEGER, INTENT(IN), OPTIONAL :: t_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: t_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: t_H
    INTEGER, INTENT(IN), OPTIONAL :: t_M
    INTEGER, INTENT(IN), OPTIONAL :: t_S
    INTEGER, INTENT(IN), OPTIONAL :: t_Sn
    INTEGER, INTENT(IN), OPTIONAL :: t_Sd
    INTEGER, INTENT(IN), OPTIONAL :: ti_YY
    INTEGER, INTENT(IN), OPTIONAL :: ti_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: ti_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: ti_H
    INTEGER, INTENT(IN), OPTIONAL :: ti_M
    INTEGER, INTENT(IN), OPTIONAL :: ti_S
    INTEGER, INTENT(IN), OPTIONAL :: ti_Sn
    INTEGER, INTENT(IN), OPTIONAL :: ti_Sd
    CHARACTER (LEN=*), INTENT(IN) :: res_str
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: testname
    LOGICAL, OPTIONAL, INTENT(IN) :: expect_error
    ! locals
    INTEGER :: it_YY
    INTEGER :: it_MM  ! month
    INTEGER :: it_DD  ! day of month
    INTEGER :: it_H
    INTEGER :: it_M
    INTEGER :: it_S
    INTEGER :: it_Sn
    INTEGER :: it_Sd
    INTEGER :: iti_YY
    INTEGER :: iti_MM  ! month
    INTEGER :: iti_DD  ! day of month
    INTEGER :: iti_H
    INTEGER :: iti_M
    INTEGER :: iti_S
    INTEGER :: iti_Sn
    INTEGER :: iti_Sd
    LOGICAL :: is_t 
    LOGICAL :: is_ti
    CHARACTER (LEN=512) :: itestname
    LOGICAL :: iexpect_error
    INTEGER rc
    TYPE(ESMF_Time)           :: t
    TYPE(ESMF_TimeInterval)   :: ti
    CHARACTER(LEN=ESMF_MAXSTR) :: str, computed_str, frac_str
    CHARACTER(LEN=17) :: type_str
    INTEGER :: res_len, computed_len, Sn, Sd
    LOGICAL :: test_passed

!  PRINT *,'DEBUG:  BEGIN test_print()'
    it_YY = 0
    it_MM = 1
    it_DD = 1
    it_H = 0
    it_M = 0
    it_S = 0
    it_Sn = 0
    it_Sd = 0
    iti_YY = 0
    iti_MM = 0
    iti_DD = 0
    iti_H = 0
    iti_M = 0
    iti_S = 0
    iti_Sn = 0
    iti_Sd = 0
    itestname = ''
    iexpect_error = .FALSE.

    IF ( PRESENT( t_YY ) ) it_YY = t_YY
    IF ( PRESENT( t_MM ) ) it_MM = t_MM
    IF ( PRESENT( t_DD ) ) it_DD = t_DD
    IF ( PRESENT( t_H ) ) it_H = t_H
    IF ( PRESENT( t_M ) ) it_M = t_M
    IF ( PRESENT( t_S ) ) it_S = t_S
    IF ( PRESENT( t_Sn ) ) it_Sn = t_Sn
    IF ( PRESENT( t_Sd ) ) it_Sd = t_Sd
    IF ( PRESENT( ti_YY ) ) iti_YY = ti_YY
    IF ( PRESENT( ti_MM ) ) iti_MM = ti_MM
    IF ( PRESENT( ti_DD ) ) iti_DD = ti_DD
    IF ( PRESENT( ti_H ) ) iti_H = ti_H
    IF ( PRESENT( ti_M ) ) iti_M = ti_M
    IF ( PRESENT( ti_S ) ) iti_S = ti_S
    IF ( PRESENT( ti_Sn ) ) iti_Sn = ti_Sn
    IF ( PRESENT( ti_Sd ) ) iti_Sd = ti_Sd
    IF ( PRESENT( testname ) ) itestname = TRIM(testname)
    IF ( PRESENT( expect_error ) ) iexpect_error = expect_error

    ! Ensure that optional arguments are consistent...
    is_t = ( PRESENT( t_YY ) .OR. PRESENT( t_MM ) .OR. &
             PRESENT( t_DD ) .OR. PRESENT( t_H ) .OR.  &
             PRESENT( t_M )  .OR. PRESENT( t_S ) .OR.  &
             PRESENT( t_Sn )  .OR. PRESENT( t_Sd ) )
    is_ti = ( PRESENT( ti_YY ) .OR. PRESENT( ti_MM ) .OR. &
              PRESENT( ti_DD ) .OR. PRESENT( ti_H ) .OR.  &
              PRESENT( ti_M )  .OR. PRESENT( ti_S ) .OR.  &
              PRESENT( ti_Sn )  .OR. PRESENT( ti_Sd ) )
    IF ( is_t .EQV. is_ti ) THEN
      CALL wrf_error_fatal3( __FILE__ , __LINE__ , &
        'ERROR test_print:  inconsistent args' )
    ENDIF

!PRINT *,'DEBUG:  test_print():  init objects'
    ! Initialize object to be tested
    ! modify behavior of wrf_error_fatal3 for tests expected to fail
    IF ( iexpect_error ) WRF_ERROR_FATAL_PRINT = .TRUE.
    Sn = 0
    Sd = 0
    IF ( is_t ) THEN
      type_str = 'ESMF_Time'
!PRINT *,'DEBUG:  test_print():  calling ESMF_TimeSet()'
!PRINT *,'DEBUG:  test_print():  YY,MM,DD,H,M,S,Sn,Sd = ', it_YY,it_MM,it_DD,it_H,it_M,it_S,it_Sn,it_Sd
      CALL ESMF_TimeSet( t, YY=it_YY, MM=it_MM, DD=it_DD , &
                             H=it_H, M=it_M, S=it_S, Sn=it_Sn, Sd=it_Sd, rc=rc )
!PRINT *,'DEBUG:  test_print():  back from ESMF_TimeSet()'
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeSet() ', &
                             __FILE__ , &
                             __LINE__  )
!PRINT *,'DEBUG:  test_print():  calling ESMF_TimeGet()'
      CALL ESMF_TimeGet( t, timeString=computed_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
!PRINT *,'DEBUG:  test_print():  back from ESMF_TimeGet(), computed_str = ',TRIM(computed_str)
    ELSE
      type_str = 'ESMF_TimeInterval'
!PRINT *,'DEBUG:  test_print():  calling ESMF_TimeIntervalSet()'
      CALL ESMF_TimeIntervalSet( ti, YY=iti_YY, MM=iti_MM, &
                                      D=iti_DD ,           &
                                      H=iti_H, M=iti_M,    &
                                      S=iti_S, Sn=iti_Sn, Sd=iti_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeIntervalSet() ', &
                             __FILE__ , &
                             __LINE__  )
!PRINT *,'DEBUG:  test_print():  calling ESMF_TimeIntervalGet()'
      CALL ESMF_TimeIntervalGet( ti, timeString=computed_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
    ENDIF
    ! handle fractions
    IF ( Sd > 0 ) THEN
      IF ( Sn > 0 ) THEN
        WRITE(frac_str,FMT="('+',I2.2,'/',I2.2)") abs(Sn), Sd
      ELSE IF ( Sn < 0 ) THEN
        WRITE(frac_str,FMT="('-',I2.2,'/',I2.2)") abs(Sn), Sd
      ELSE
        frac_str = ''
      ENDIF
      computed_str = TRIM(computed_str)//TRIM(frac_str)
    ENDIF
    ! restore default behavior of wrf_error_fatal3
    IF ( iexpect_error ) WRF_ERROR_FATAL_PRINT = .FALSE.
!PRINT *,'DEBUG:  test_print():  done init objects'

!PRINT *,'DEBUG:  test_print():  check result'
    ! check result
    test_passed = .FALSE.
    res_len = LEN_TRIM(res_str)
    computed_len = LEN_TRIM(computed_str)
    IF ( res_len == computed_len ) THEN
      IF ( computed_str(1:computed_len) == res_str(1:res_len) ) THEN
        test_passed = .TRUE.
      ENDIF
    ENDIF
    IF ( test_passed ) THEN
      WRITE(*,FMT='(A)') 'PASS:  '//TRIM(itestname)
    ELSE
      WRITE(*,'(9A)') 'FAIL:  ',TRIM(itestname),':  printing ',TRIM(type_str), &
        '  expected <', TRIM(res_str),'>  but computed <',TRIM(computed_str),'>'
    ENDIF
!PRINT *,'DEBUG:  END test_print()'

  END SUBROUTINE test_print



  ! Test the following arithmetic operations on ESMF_Time and 
  ! ESMF_TimeInterval objects:
  !  ESMF_Time         = ESMF_Time         + ESMF_TimeInterval
  !  ESMF_Time         = ESMF_TimeInterval + ESMF_Time
  !  ESMF_Time         = ESMF_Time         - ESMF_TimeInterval
  !  ESMF_TimeInterval = ESMF_Time         - ESMF_Time        
  !  ESMF_TimeInterval = ESMF_TimeInterval + ESMF_TimeInterval
  !  ESMF_TimeInterval = ESMF_TimeInterval - ESMF_TimeInterval
  !  ESMF_TimeInterval = ESMF_TimeInterval * INTEGER
  !  ESMF_TimeInterval = ESMF_TimeInterval / INTEGER
  !
  ! Correct results are also passed in through this interface and compared 
  ! with computed results.  PASS/FAIL messages are printed.  
  !
  ! Operations are expressed as res = op1 +|- op2
  !
  SUBROUTINE test_arithmetic( add_op, multiply_op,                                       &
     op1_t_yy,  op1_t_mm,  op1_t_dd,  op1_t_h,  op1_t_m,  op1_t_s,  op1_t_sn,  op1_t_sd, &
    op1_ti_yy, op1_ti_mm, op1_ti_dd, op1_ti_h, op1_ti_m, op1_ti_s, op1_ti_sn, op1_ti_sd, &
     op2_t_yy,  op2_t_mm,  op2_t_dd,  op2_t_h,  op2_t_m,  op2_t_s,  op2_t_sn,  op2_t_sd, &
    op2_ti_yy, op2_ti_mm, op2_ti_dd, op2_ti_h, op2_ti_m, op2_ti_s, op2_ti_sn, op2_ti_sd, &
    op2_int,                                                                             &
     res_t_yy,  res_t_mm,  res_t_dd,  res_t_h,  res_t_m,  res_t_s,  res_t_sn,  res_t_sd, &
    res_ti_yy, res_ti_mm, res_ti_dd, res_ti_h, res_ti_m, res_ti_s, res_ti_sn, res_ti_sd, &
    res_int, testname, expect_error )
    LOGICAL, INTENT(IN), OPTIONAL :: add_op      ! .TRUE.=add, .FALSE.=subtract
    LOGICAL, INTENT(IN), OPTIONAL :: multiply_op ! .TRUE.=multiply, .FALSE.=divide
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_YY
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_H
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_M
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_S
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_Sn
    INTEGER, INTENT(IN), OPTIONAL :: op1_t_Sd
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_YY
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_H
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_M
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_S
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_Sn
    INTEGER, INTENT(IN), OPTIONAL :: op1_ti_Sd
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_YY
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_H
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_M
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_S
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_Sn
    INTEGER, INTENT(IN), OPTIONAL :: op2_t_Sd
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_YY
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_H
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_M
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_S
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_Sn
    INTEGER, INTENT(IN), OPTIONAL :: op2_ti_Sd
    INTEGER, INTENT(IN), OPTIONAL :: op2_int
    INTEGER, INTENT(IN), OPTIONAL :: res_t_YY
    INTEGER, INTENT(IN), OPTIONAL :: res_t_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: res_t_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: res_t_H
    INTEGER, INTENT(IN), OPTIONAL :: res_t_M
    INTEGER, INTENT(IN), OPTIONAL :: res_t_S
    INTEGER, INTENT(IN), OPTIONAL :: res_t_Sn
    INTEGER, INTENT(IN), OPTIONAL :: res_t_Sd
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_YY
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_H
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_M
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_S
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_Sn
    INTEGER, INTENT(IN), OPTIONAL :: res_ti_Sd
    INTEGER, INTENT(IN), OPTIONAL :: res_int
    CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: testname
    LOGICAL, OPTIONAL, INTENT(IN) :: expect_error
    ! locals
    LOGICAL :: iadd_op
    LOGICAL :: isubtract_op
    LOGICAL :: imultiply_op
    LOGICAL :: idivide_op
    INTEGER :: iop1_t_YY
    INTEGER :: iop1_t_MM  ! month
    INTEGER :: iop1_t_DD  ! day of month
    INTEGER :: iop1_t_H
    INTEGER :: iop1_t_M
    INTEGER :: iop1_t_S
    INTEGER :: iop1_t_Sn
    INTEGER :: iop1_t_Sd
    INTEGER :: iop1_ti_YY
    INTEGER :: iop1_ti_MM  ! month
    INTEGER :: iop1_ti_DD  ! day of month
    INTEGER :: iop1_ti_H
    INTEGER :: iop1_ti_M
    INTEGER :: iop1_ti_S
    INTEGER :: iop1_ti_Sn
    INTEGER :: iop1_ti_Sd
    INTEGER :: iop2_t_YY
    INTEGER :: iop2_t_MM  ! month
    INTEGER :: iop2_t_DD  ! day of month
    INTEGER :: iop2_t_H
    INTEGER :: iop2_t_M
    INTEGER :: iop2_t_S
    INTEGER :: iop2_t_Sn
    INTEGER :: iop2_t_Sd
    INTEGER :: iop2_ti_YY
    INTEGER :: iop2_ti_MM  ! month
    INTEGER :: iop2_ti_DD  ! day of month
    INTEGER :: iop2_ti_H
    INTEGER :: iop2_ti_M
    INTEGER :: iop2_ti_S
    INTEGER :: iop2_ti_Sn
    INTEGER :: iop2_ti_Sd
    INTEGER :: ires_t_YY
    INTEGER :: ires_t_MM  ! month
    INTEGER :: ires_t_DD  ! day of month
    INTEGER :: ires_t_H
    INTEGER :: ires_t_M
    INTEGER :: ires_t_S
    INTEGER :: ires_t_Sn
    INTEGER :: ires_t_Sd
    INTEGER :: ires_ti_YY
    INTEGER :: ires_ti_MM  ! month
    INTEGER :: ires_ti_DD  ! day of month
    INTEGER :: ires_ti_H
    INTEGER :: ires_ti_M
    INTEGER :: ires_ti_S
    INTEGER :: ires_ti_Sn
    INTEGER :: ires_ti_Sd
    LOGICAL :: op1_is_t , op2_is_t , res_is_t
    LOGICAL :: op1_is_ti, op2_is_ti, res_is_ti, op2_is_int
    LOGICAL :: res_is_int
    INTEGER :: num_ops, num_op1, num_op2, num_res
    LOGICAL :: unsupported_op, test_passed
    CHARACTER (LEN=512) :: itestname
    LOGICAL :: iexpect_error
    INTEGER :: rc
    INTEGER :: computed_int, Sn, Sd
    TYPE(ESMF_Time)           :: op1_t , op2_t , res_t, computed_t
    TYPE(ESMF_TimeInterval)   :: op1_ti, op2_ti, res_ti, computed_ti
    CHARACTER(LEN=ESMF_MAXSTR) :: str, op1_str, op2_str, res_str, computed_str, frac_str
    CHARACTER(LEN=1) :: op_str
    CHARACTER(LEN=17) :: op1_type_str, op2_type_str, res_type_str

    iadd_op = .FALSE.
    isubtract_op = .FALSE.
    imultiply_op = .FALSE.
    idivide_op = .FALSE.
    iop1_t_YY = 0
    iop1_t_MM = 1
    iop1_t_DD = 1
    iop1_t_H = 0
    iop1_t_M = 0
    iop1_t_S = 0
    iop1_t_Sn = 0
    iop1_t_Sd = 0
    iop1_ti_YY = 0
    iop1_ti_MM = 0
    iop1_ti_DD = 0
    iop1_ti_H = 0
    iop1_ti_M = 0
    iop1_ti_S = 0
    iop1_ti_Sn = 0
    iop1_ti_Sd = 0
    iop2_t_YY = 0
    iop2_t_MM = 1
    iop2_t_DD = 1
    iop2_t_H = 0
    iop2_t_M = 0
    iop2_t_S = 0
    iop2_t_Sn = 0
    iop2_t_Sd = 0
    iop2_ti_YY = 0
    iop2_ti_MM = 0
    iop2_ti_DD = 0
    iop2_ti_H = 0
    iop2_ti_M = 0
    iop2_ti_S = 0
    iop2_ti_Sn = 0
    iop2_ti_Sd = 0
    ires_t_YY = 0
    ires_t_MM = 1
    ires_t_DD = 1
    ires_t_H = 0
    ires_t_M = 0
    ires_t_S = 0
    ires_t_Sn = 0
    ires_t_Sd = 0
    ires_ti_YY = 0
    ires_ti_MM = 0
    ires_ti_DD = 0
    ires_ti_H = 0
    ires_ti_M = 0
    ires_ti_S = 0
    ires_ti_Sn = 0
    ires_ti_Sd = 0
    itestname = ''
    iexpect_error = .FALSE.

    IF ( PRESENT( add_op ) ) THEN
      iadd_op = add_op
      isubtract_op = ( .NOT. add_op )
    ENDIF
    IF ( PRESENT( multiply_op ) ) THEN
      imultiply_op = multiply_op
      idivide_op = ( .NOT. multiply_op )
    ENDIF
    num_ops = 0
    IF ( iadd_op )      num_ops = num_ops + 1
    IF ( isubtract_op ) num_ops = num_ops + 1
    IF ( imultiply_op ) num_ops = num_ops + 1
    IF ( idivide_op )   num_ops = num_ops + 1
    IF ( num_ops /= 1 ) THEN
      CALL wrf_error_fatal3( __FILE__ , __LINE__ , &
        'ERROR test_arithmetic:  inconsistent operation' )
    ENDIF
    IF ( PRESENT( op1_t_YY ) ) iop1_t_YY = op1_t_YY
    IF ( PRESENT( op1_t_MM ) ) iop1_t_MM = op1_t_MM
    IF ( PRESENT( op1_t_DD ) ) iop1_t_DD = op1_t_DD
    IF ( PRESENT( op1_t_H ) ) iop1_t_H = op1_t_H
    IF ( PRESENT( op1_t_M ) ) iop1_t_M = op1_t_M
    IF ( PRESENT( op1_t_S ) ) iop1_t_S = op1_t_S
    IF ( PRESENT( op1_t_Sn ) ) iop1_t_Sn = op1_t_Sn
    IF ( PRESENT( op1_t_Sd ) ) iop1_t_Sd = op1_t_Sd
    IF ( PRESENT( op1_ti_YY ) ) iop1_ti_YY = op1_ti_YY
    IF ( PRESENT( op1_ti_MM ) ) iop1_ti_MM = op1_ti_MM
    IF ( PRESENT( op1_ti_DD ) ) iop1_ti_DD = op1_ti_DD
    IF ( PRESENT( op1_ti_H ) ) iop1_ti_H = op1_ti_H
    IF ( PRESENT( op1_ti_M ) ) iop1_ti_M = op1_ti_M
    IF ( PRESENT( op1_ti_S ) ) iop1_ti_S = op1_ti_S
    IF ( PRESENT( op1_ti_Sn ) ) iop1_ti_Sn = op1_ti_Sn
    IF ( PRESENT( op1_ti_Sd ) ) iop1_ti_Sd = op1_ti_Sd
    IF ( PRESENT( op2_t_YY ) ) iop2_t_YY = op2_t_YY
    IF ( PRESENT( op2_t_MM ) ) iop2_t_MM = op2_t_MM
    IF ( PRESENT( op2_t_DD ) ) iop2_t_DD = op2_t_DD
    IF ( PRESENT( op2_t_H ) ) iop2_t_H = op2_t_H
    IF ( PRESENT( op2_t_M ) ) iop2_t_M = op2_t_M
    IF ( PRESENT( op2_t_S ) ) iop2_t_S = op2_t_S
    IF ( PRESENT( op2_t_Sn ) ) iop2_t_Sn = op2_t_Sn
    IF ( PRESENT( op2_t_Sd ) ) iop2_t_Sd = op2_t_Sd
    IF ( PRESENT( op2_ti_YY ) ) iop2_ti_YY = op2_ti_YY
    IF ( PRESENT( op2_ti_MM ) ) iop2_ti_MM = op2_ti_MM
    IF ( PRESENT( op2_ti_DD ) ) iop2_ti_DD = op2_ti_DD
    IF ( PRESENT( op2_ti_H ) ) iop2_ti_H = op2_ti_H
    IF ( PRESENT( op2_ti_M ) ) iop2_ti_M = op2_ti_M
    IF ( PRESENT( op2_ti_S ) ) iop2_ti_S = op2_ti_S
    IF ( PRESENT( op2_ti_Sn ) ) iop2_ti_Sn = op2_ti_Sn
    IF ( PRESENT( op2_ti_Sd ) ) iop2_ti_Sd = op2_ti_Sd
    IF ( PRESENT( res_t_YY ) ) ires_t_YY = res_t_YY
    IF ( PRESENT( res_t_MM ) ) ires_t_MM = res_t_MM
    IF ( PRESENT( res_t_DD ) ) ires_t_DD = res_t_DD
    IF ( PRESENT( res_t_H ) ) ires_t_H = res_t_H
    IF ( PRESENT( res_t_M ) ) ires_t_M = res_t_M
    IF ( PRESENT( res_t_S ) ) ires_t_S = res_t_S
    IF ( PRESENT( res_t_Sn ) ) ires_t_Sn = res_t_Sn
    IF ( PRESENT( res_t_Sd ) ) ires_t_Sd = res_t_Sd
    IF ( PRESENT( res_ti_YY ) ) ires_ti_YY = res_ti_YY
    IF ( PRESENT( res_ti_MM ) ) ires_ti_MM = res_ti_MM
    IF ( PRESENT( res_ti_DD ) ) ires_ti_DD = res_ti_DD
    IF ( PRESENT( res_ti_H ) ) ires_ti_H = res_ti_H
    IF ( PRESENT( res_ti_M ) ) ires_ti_M = res_ti_M
    IF ( PRESENT( res_ti_S ) ) ires_ti_S = res_ti_S
    IF ( PRESENT( res_ti_Sn ) ) ires_ti_Sn = res_ti_Sn
    IF ( PRESENT( res_ti_Sd ) ) ires_ti_Sd = res_ti_Sd
    IF ( PRESENT( testname ) ) itestname = TRIM(testname)
    IF ( PRESENT( expect_error ) ) iexpect_error = expect_error

    ! Ensure that optional arguments are consistent...
    op1_is_t = ( PRESENT( op1_t_YY ) .OR. PRESENT( op1_t_MM ) .OR. &
                 PRESENT( op1_t_DD ) .OR. PRESENT( op1_t_H ) .OR.  &
                 PRESENT( op1_t_M )  .OR. PRESENT( op1_t_S ) .OR.  &
                 PRESENT( op1_t_Sn )  .OR. PRESENT( op1_t_Sd ) )
    op1_is_ti = ( PRESENT( op1_ti_YY ) .OR. PRESENT( op1_ti_MM ) .OR. &
                  PRESENT( op1_ti_DD ) .OR. PRESENT( op1_ti_H ) .OR.  &
                  PRESENT( op1_ti_M )  .OR. PRESENT( op1_ti_S ) .OR.  &
                  PRESENT( op1_ti_Sn )  .OR. PRESENT( op1_ti_Sd ) )
    op2_is_t = ( PRESENT( op2_t_YY ) .OR. PRESENT( op2_t_MM ) .OR. &
                 PRESENT( op2_t_DD ) .OR. PRESENT( op2_t_H ) .OR.  &
                 PRESENT( op2_t_M )  .OR. PRESENT( op2_t_S ) .OR.  &
                 PRESENT( op2_t_Sn )  .OR. PRESENT( op2_t_Sd ) )
    op2_is_ti = ( PRESENT( op2_ti_YY ) .OR. PRESENT( op2_ti_MM ) .OR. &
                  PRESENT( op2_ti_DD ) .OR. PRESENT( op2_ti_H ) .OR.  &
                  PRESENT( op2_ti_M )  .OR. PRESENT( op2_ti_S ) .OR.  &
                  PRESENT( op2_ti_Sn )  .OR. PRESENT( op2_ti_Sd ) )
    op2_is_int = ( PRESENT( op2_int ) )
    res_is_t = ( PRESENT( res_t_YY ) .OR. PRESENT( res_t_MM ) .OR. &
                 PRESENT( res_t_DD ) .OR. PRESENT( res_t_H ) .OR.  &
                 PRESENT( res_t_M )  .OR. PRESENT( res_t_S ) .OR.  &
                 PRESENT( res_t_Sn )  .OR. PRESENT( res_t_Sd ) )
    res_is_ti = ( PRESENT( res_ti_YY ) .OR. PRESENT( res_ti_MM ) .OR. &
                  PRESENT( res_ti_DD ) .OR. PRESENT( res_ti_H ) .OR.  &
                  PRESENT( res_ti_M )  .OR. PRESENT( res_ti_S ) .OR.  &
                  PRESENT( res_ti_Sn )  .OR. PRESENT( res_ti_Sd ) )
    res_is_int = ( PRESENT( res_int ) )
    num_op1 = 0
    IF ( op1_is_t   ) num_op1 = num_op1 + 1
    IF ( op1_is_ti  ) num_op1 = num_op1 + 1
    IF ( num_op1 /= 1 ) THEN
      CALL wrf_error_fatal3( __FILE__ , __LINE__ , &
        'ERROR test_arithmetic:  inconsistent args for op1' )
    ENDIF
    num_op2 = 0
    IF ( op2_is_t   ) num_op2 = num_op2 + 1
    IF ( op2_is_ti  ) num_op2 = num_op2 + 1
    IF ( op2_is_int ) num_op2 = num_op2 + 1
    IF ( num_op2 /= 1 ) THEN
      CALL wrf_error_fatal3( __FILE__ , __LINE__ , &
        'ERROR test_arithmetic:  inconsistent args for op2' )
    ENDIF
    num_res = 0
    IF ( res_is_t   ) num_res = num_res + 1
    IF ( res_is_ti  ) num_res = num_res + 1
    IF ( res_is_int ) num_res = num_res + 1
    IF ( num_res /= 1 ) THEN
      CALL wrf_error_fatal3( __FILE__ , __LINE__ , &
        'ERROR test_arithmetic:  inconsistent args for result' )
    ENDIF

    ! Initialize op1
    IF ( op1_is_t ) THEN
      op1_type_str = 'ESMF_Time'
      CALL ESMF_TimeSet( op1_t, YY=iop1_t_YY, MM=iop1_t_MM, DD=iop1_t_DD , &
                                 H=iop1_t_H, M=iop1_t_M, S=iop1_t_S, Sn=iop1_t_Sn, Sd=iop1_t_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeSet() ', &
                             __FILE__ , &
                             __LINE__  )
      CALL ESMF_TimeGet( op1_t, timeString=op1_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      ! handle fractions
      CALL fraction_to_string( Sn, Sd, frac_str )
      op1_str = TRIM(op1_str)//TRIM(frac_str)
    ELSE
      op1_type_str = 'ESMF_TimeInterval'
      CALL ESMF_TimeIntervalSet( op1_ti, YY=iop1_ti_YY, MM=iop1_ti_MM, &
                                          D=iop1_ti_DD ,               &
                                          H=iop1_ti_H, M=iop1_ti_M,    &
                                          S=iop1_ti_S, Sn=iop1_ti_Sn, Sd=iop1_ti_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeIntervalSet() ', &
                             __FILE__ , &
                             __LINE__  )
      CALL ESMF_TimeIntervalGet( op1_ti, timeString=op1_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      ! handle fractions
      CALL fraction_to_string( Sn, Sd, frac_str )
      op1_str = TRIM(op1_str)//TRIM(frac_str)
    ENDIF
    ! Initialize op2
    IF ( op2_is_t ) THEN
      op2_type_str = 'ESMF_Time'
      CALL ESMF_TimeSet( op2_t, YY=iop2_t_YY, MM=iop2_t_MM, DD=iop2_t_DD , &
                                 H=iop2_t_H, M=iop2_t_M, S=iop2_t_S, Sn=iop2_t_Sn, Sd=iop2_t_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeSet() ', &
                             __FILE__ , &
                             __LINE__  )
      CALL ESMF_TimeGet( op2_t, timeString=op2_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      ! handle fractions
      CALL fraction_to_string( Sn, Sd, frac_str )
      op2_str = TRIM(op2_str)//TRIM(frac_str)
    ELSE IF ( op2_is_ti ) THEN
      op2_type_str = 'ESMF_TimeInterval'
      CALL ESMF_TimeIntervalSet( op2_ti, YY=iop2_ti_YY, MM=iop2_ti_MM, &
                                          D=iop2_ti_DD ,               &
                                          H=iop2_ti_H, M=iop2_ti_M,    &
                                          S=iop2_ti_S, Sn=iop2_ti_Sn, Sd=iop2_ti_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeIntervalSet() ', &
                             __FILE__ , &
                             __LINE__  )
      CALL ESMF_TimeIntervalGet( op2_ti, timeString=op2_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      ! handle fractions
      CALL fraction_to_string( Sn, Sd, frac_str )
      op2_str = TRIM(op2_str)//TRIM(frac_str)
    ELSE
      op2_type_str = 'INTEGER'
      IF ( op2_int > 0 ) THEN
        WRITE(op2_str,FMT="('+',I8.8)") ABS(op2_int)
      ELSE
        WRITE(op2_str,FMT="('-',I8.8)") ABS(op2_int)
      ENDIF
    ENDIF
    ! Initialize res
    IF ( res_is_t ) THEN  ! result is ESMF_Time
      res_type_str = 'ESMF_Time'
      CALL ESMF_TimeSet( res_t, YY=ires_t_YY, MM=ires_t_MM, DD=ires_t_DD , &
                                 H=ires_t_H, M=ires_t_M, S=ires_t_S, Sn=ires_t_Sn, Sd=ires_t_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeSet() ', &
                             __FILE__ , &
                             __LINE__  )
      CALL ESMF_TimeGet( res_t, timeString=res_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      ! handle fractions
      CALL fraction_to_string( Sn, Sd, frac_str )
      res_str = TRIM(res_str)//TRIM(frac_str)
    ELSE IF ( res_is_ti ) THEN  ! result is ESMF_TimeInterval
      res_type_str = 'ESMF_TimeInterval'
      CALL ESMF_TimeIntervalSet( res_ti, YY=ires_ti_YY, MM=ires_ti_MM, &
                                          D=ires_ti_DD ,               &
                                          H=ires_ti_H, M=ires_ti_M,    &
                                          S=ires_ti_S, Sn=ires_ti_Sn, Sd=ires_ti_Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                             TRIM(itestname)//'ESMF_TimeIntervalSet() ', &
                             __FILE__ , &
                             __LINE__  )
      CALL ESMF_TimeIntervalGet( res_ti, timeString=res_str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      ! handle fractions
      CALL fraction_to_string( Sn, Sd, frac_str )
      res_str = TRIM(res_str)//TRIM(frac_str)
    ELSE  ! result is INTEGER
      res_type_str = 'INTEGER'
      IF ( res_int > 0 ) THEN
        WRITE(res_str,FMT="('+',I8.8)") ABS(res_int)
      ELSE
        WRITE(res_str,FMT="('-',I8.8)") ABS(res_int)
      ENDIF
    ENDIF

    ! perform requested operation
    unsupported_op = .FALSE.
    ! modify behavior of wrf_error_fatal3 for operator being tested
    IF ( iexpect_error ) WRF_ERROR_FATAL_PRINT = .TRUE.
    ! add
    IF ( iadd_op ) THEN
      op_str = '+'
      IF ( res_is_t ) THEN  ! result is ESMF_Time
        IF ( op1_is_t .AND. op2_is_ti ) THEN
          !  ESMF_Time         = ESMF_Time         + ESMF_TimeInterval
          computed_t = op1_t + op2_ti
        ELSE IF ( op1_is_ti .AND. op2_is_t ) THEN
          !  ESMF_Time         = ESMF_TimeInterval + ESMF_Time
          computed_t = op1_ti + op2_t
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ELSE  ! result is ESMF_TimeInterval
        IF ( op1_is_ti .AND. op2_is_ti ) THEN
          !  ESMF_TimeInterval = ESMF_TimeInterval + ESMF_TimeInterval
          computed_ti = op1_ti + op2_ti
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ENDIF
    ! subtract
    ELSE  IF ( isubtract_op ) THEN
      op_str = '-'
      IF ( res_is_t ) THEN  ! result is ESMF_Time
        IF ( op1_is_t .AND. op2_is_ti ) THEN
          !  ESMF_Time         = ESMF_Time         - ESMF_TimeInterval
          computed_t = op1_t - op2_ti
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ELSE  ! result is ESMF_TimeInterval
        IF ( op1_is_t .AND. op2_is_t ) THEN
          !  ESMF_TimeInterval = ESMF_Time         - ESMF_Time        
          computed_ti = op1_t - op2_t
        ELSE IF ( op1_is_ti .AND. op2_is_ti ) THEN
          !  ESMF_TimeInterval = ESMF_TimeInterval - ESMF_TimeInterval
          computed_ti = op1_ti - op2_ti
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ENDIF
    ELSE  IF ( imultiply_op ) THEN
      op_str = '*'
      IF ( res_is_ti ) THEN  ! result is ESMF_TimeInterval
        IF ( op1_is_ti .AND. op2_is_int ) THEN
          !  ESMF_TimeInterval = ESMF_TimeInterval * INTEGER
          computed_ti = op1_ti * op2_int
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ENDIF
    ELSE  IF ( idivide_op ) THEN
      op_str = '/'
      IF ( res_is_ti ) THEN  ! result is ESMF_TimeInterval
        IF ( op1_is_ti .AND. op2_is_int ) THEN
          !  ESMF_TimeInterval = ESMF_TimeInterval / INTEGER
          computed_ti = op1_ti / op2_int
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ELSE IF ( res_is_int ) THEN  ! result is INTEGER
        IF ( op1_is_ti .AND. op2_is_ti ) THEN
          !  INTEGER = ESMF_TimeInterval / ESMF_TimeInterval
          ! number of whole time intervals
          computed_int = ESMF_TimeIntervalDIVQuot( op1_ti , op2_ti )
        ELSE
          unsupported_op = .TRUE.
        ENDIF
      ENDIF
    ENDIF
    ! restore default behavior of wrf_error_fatal3
    IF ( iexpect_error ) WRF_ERROR_FATAL_PRINT = .FALSE.
    IF ( unsupported_op ) THEN
      WRITE(str,*) 'ERROR test_arithmetic ',TRIM(itestname), &
        ':  unsupported operation (',                           &
        TRIM(res_type_str),' = ',TRIM(op1_type_str),' ',TRIM(op_str),' ', &
        TRIM(op2_type_str),')'
      CALL wrf_error_fatal3( __FILE__ , __LINE__ , str )
    ENDIF

    ! check result
    test_passed = .FALSE.
    IF ( res_is_t ) THEN  ! result is ESMF_Time
      IF ( computed_t == res_t ) THEN
        test_passed = .TRUE.
      ELSE
        CALL ESMF_TimeGet( computed_t, timeString=computed_str, Sn=Sn, Sd=Sd, rc=rc )
        CALL test_check_error( ESMF_SUCCESS, rc, &
                              TRIM(itestname)//'ESMF_TimeGet() ', &
                              __FILE__ , &
                              __LINE__  )
        ! handle fractions
        CALL fraction_to_string( Sn, Sd, frac_str )
        computed_str = TRIM(computed_str)//TRIM(frac_str)
      ENDIF
    ELSE IF ( res_is_ti ) THEN  ! result is ESMF_TimeInterval
      IF ( computed_ti == res_ti ) THEN
        test_passed = .TRUE.
      ELSE
        CALL ESMF_TimeIntervalGet( computed_ti, timeString=computed_str, Sn=Sn, Sd=Sd, rc=rc )
        CALL test_check_error( ESMF_SUCCESS, rc, &
                              TRIM(itestname)//'ESMF_TimeGet() ', &
                              __FILE__ , &
                              __LINE__  )
        ! handle fractions
        CALL fraction_to_string( Sn, Sd, frac_str )
        computed_str = TRIM(computed_str)//TRIM(frac_str)
      ENDIF
    ELSE  ! result is INTEGER
      IF ( computed_int == res_int ) THEN
        test_passed = .TRUE.
      ELSE
        IF ( computed_int > 0 ) THEN
          WRITE(computed_str,FMT="('+',I8.8)") ABS(computed_int)
        ELSE
          WRITE(computed_str,FMT="('-',I8.8)") ABS(computed_int)
        ENDIF
      ENDIF
    ENDIF
    IF ( test_passed ) THEN
      WRITE(*,FMT='(A)') 'PASS:  '//TRIM(itestname)
    ELSE
      WRITE(*,*) 'FAIL:  ',TRIM(itestname),':  (',                        &
        TRIM(res_type_str),' = ',TRIM(op1_type_str),' ',TRIM(op_str),' ', &
        TRIM(op2_type_str),')  expected ',                                &
        TRIM(res_str),' = ',TRIM(op1_str),' ',TRIM(op_str),' ',           &
        TRIM(op2_str),'  but computed ',TRIM(computed_str)
    ENDIF

  END SUBROUTINE test_arithmetic



  ! simple clock creation and advance with add-subtract tests thrown in
  ! no self checks (yet)
  SUBROUTINE test_clock_advance(                                              &
    start_yy, start_mm, start_dd, start_h, start_m, start_s,                  &
     stop_yy,  stop_mm,  stop_dd,  stop_h,  stop_m,  stop_s,                  &
    timestep_d, timestep_h, timestep_m, timestep_s, timestep_sn, timestep_sd, &
    testname, increment_S, increment_Sn, increment_Sd )
    INTEGER, INTENT(IN), OPTIONAL :: start_YY
    INTEGER, INTENT(IN), OPTIONAL :: start_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: start_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: start_H
    INTEGER, INTENT(IN), OPTIONAL :: start_M
    INTEGER, INTENT(IN), OPTIONAL :: start_S
    INTEGER, INTENT(IN), OPTIONAL :: stop_YY
    INTEGER, INTENT(IN), OPTIONAL :: stop_MM  ! month
    INTEGER, INTENT(IN), OPTIONAL :: stop_DD  ! day of month
    INTEGER, INTENT(IN), OPTIONAL :: stop_H
    INTEGER, INTENT(IN), OPTIONAL :: stop_M
    INTEGER, INTENT(IN), OPTIONAL :: stop_S
    INTEGER, INTENT(IN), OPTIONAL :: timestep_D  ! day
    INTEGER, INTENT(IN), OPTIONAL :: timestep_H
    INTEGER, INTENT(IN), OPTIONAL :: timestep_M
    INTEGER, INTENT(IN), OPTIONAL :: timestep_S
    INTEGER, INTENT(IN), OPTIONAL :: timestep_Sn
    INTEGER, INTENT(IN), OPTIONAL :: timestep_Sd
    CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: testname
    INTEGER, INTENT(IN), OPTIONAL :: increment_S  ! add and subtract this 
    INTEGER, INTENT(IN), OPTIONAL :: increment_Sn ! value each time step
    INTEGER, INTENT(IN), OPTIONAL :: increment_Sd

    ! locals
    INTEGER :: istart_YY
    INTEGER :: istart_MM  ! month
    INTEGER :: istart_DD  ! day of month
    INTEGER :: istart_H
    INTEGER :: istart_M
    INTEGER :: istart_S
    INTEGER :: istop_YY
    INTEGER :: istop_MM  ! month
    INTEGER :: istop_DD  ! day of month
    INTEGER :: istop_H
    INTEGER :: istop_M
    INTEGER :: istop_S
    INTEGER :: itimestep_D  ! day
    INTEGER :: itimestep_H
    INTEGER :: itimestep_M
    INTEGER :: itimestep_S
    INTEGER :: itimestep_Sn
    INTEGER :: itimestep_Sd
    CHARACTER (LEN=512) :: itestname, itestfullname
    INTEGER :: iincrement_S
    INTEGER :: iincrement_Sn
    INTEGER :: iincrement_Sd
    INTEGER :: Sn, Sd
    INTEGER rc
    TYPE(ESMF_Time)           :: start_time, stop_time, current_time
    TYPE(ESMF_Clock), POINTER :: domain_clock
    TYPE(ESMF_TimeInterval)   :: timestep, increment
    TYPE(ESMF_Time)           :: add_time, subtract_time
    INTEGER :: itimestep
    REAL(ESMF_KIND_R8) :: dayr8
    CHARACTER(LEN=ESMF_MAXSTR) :: str, frac_str

    istart_YY = 0
    istart_MM = 1
    istart_DD = 1
    istart_H = 0
    istart_M = 0
    istart_S = 0
    istop_YY = 0
    istop_MM = 1
    istop_DD = 1
    istop_H = 0
    istop_M = 0
    istop_S = 0
    itimestep_D = 0
    itimestep_H = 0
    itimestep_M = 0
    itimestep_S = 0
    itimestep_Sn = 0
    itimestep_Sd = 0
    itestname = ''
    iincrement_S = 0
    iincrement_Sn = 0
    iincrement_Sd = 0

    IF ( PRESENT( start_YY ) ) istart_YY = start_YY
    IF ( PRESENT( start_MM ) ) istart_MM = start_MM
    IF ( PRESENT( start_DD ) ) istart_DD = start_DD
    IF ( PRESENT( start_H ) ) istart_H = start_H
    IF ( PRESENT( start_M ) ) istart_M = start_M
    IF ( PRESENT( start_S ) ) istart_S = start_S
    IF ( PRESENT( stop_YY ) ) istop_YY = stop_YY
    IF ( PRESENT( stop_MM ) ) istop_MM = stop_MM
    IF ( PRESENT( stop_DD ) ) istop_DD = stop_DD
    IF ( PRESENT( stop_H ) ) istop_H = stop_H
    IF ( PRESENT( stop_M ) ) istop_M = stop_M
    IF ( PRESENT( stop_S ) ) istop_S = stop_S
    IF ( PRESENT( timestep_D ) ) itimestep_D = timestep_D
    IF ( PRESENT( timestep_H ) ) itimestep_H = timestep_H
    IF ( PRESENT( timestep_M ) ) itimestep_M = timestep_M
    IF ( PRESENT( timestep_S ) ) itimestep_S = timestep_S
    IF ( PRESENT( timestep_Sn ) ) itimestep_Sn = timestep_Sn
    IF ( PRESENT( timestep_Sd ) ) itimestep_Sd = timestep_Sd
    IF ( PRESENT( testname ) ) itestname = TRIM(testname)//'_'
    IF ( PRESENT( increment_S ) ) iincrement_S = increment_S
    IF ( PRESENT( increment_Sn ) ) iincrement_Sn = increment_Sn
    IF ( PRESENT( increment_Sd ) ) iincrement_Sd = increment_Sd

    ! Initialize start time, stop time, time step, clock for simple case. 
    itestfullname = TRIM(itestname)//'SETUP'
    CALL ESMF_TimeSet( start_time, YY=istart_YY, MM=istart_MM, DD=istart_DD , &
                                   H=istart_H, M=istart_M, S=istart_S, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeSet() ', &
                          __FILE__ , &
                          __LINE__  )

    CALL ESMF_TimeGet( start_time, timeString=str, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeGet() ', &
                          __FILE__ , &
                          __LINE__  )
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  start_time = <',TRIM(str),'>'

    CALL ESMF_TimeSet( stop_time, YY=istop_YY, MM=istop_MM, DD=istop_DD , &
                                   H=istop_H, M=istop_M, S=istop_S, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeSet() ', &
                          __FILE__ , &
                          __LINE__  )

    CALL ESMF_TimeGet( stop_time, timeString=str, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeGet() ', &
                          __FILE__ , &
                          __LINE__  )
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  stop_time = <',TRIM(str),'>'

    CALL ESMF_TimeIntervalSet( timestep, D=itimestep_D, H=itimestep_H, &
                                         M=itimestep_M, S=itimestep_S, &
                                         Sn=itimestep_Sn, Sd=itimestep_Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeIntervalSet() ', &
                          __FILE__ , &
                          __LINE__  )

    CALL ESMF_TimeIntervalGet( timestep, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeIntervalGet() ', &
                          __FILE__ , &
                          __LINE__  )
    ! handle fractions
    CALL fraction_to_string( Sn, Sd, frac_str )
    str = TRIM(str)//TRIM(frac_str)
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  timestep = <',TRIM(str),'>'

    CALL ESMF_TimeIntervalSet( increment, S=iincrement_S, &
                               Sn=iincrement_Sn, Sd=iincrement_Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeIntervalSet() ', &
                          __FILE__ , &
                          __LINE__  )

    CALL ESMF_TimeIntervalGet( increment, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeIntervalGet() ', &
                          __FILE__ , &
                          __LINE__  )
    ! handle fractions
    CALL fraction_to_string( Sn, Sd, frac_str )
    str = TRIM(str)//TRIM(frac_str)
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  increment = <',TRIM(str),'>'

    ALLOCATE( domain_clock )
    domain_clock = ESMF_ClockCreate( TimeStep= timestep,  &
                                     StartTime=start_time, &
                                     StopTime= stop_time,  &
                                     rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_ClockCreate() ', &
                          __FILE__ , &
                          __LINE__  )

    CALL ESMF_ClockGet( domain_clock, CurrTime=current_time, &
                        rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_ClockGet() ', &
                          __FILE__ , &
                          __LINE__  )

    CALL ESMF_TimeGet( current_time, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeGet() ', &
                          __FILE__ , &
                          __LINE__  )
    CALL fraction_to_string( Sn, Sd, frac_str )
    str = TRIM(str)//TRIM(frac_str)
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  clock current_time = <',TRIM(str),'>'

    CALL ESMF_TimeGet( current_time, dayOfYear_r8=dayr8, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeGet() ', &
                          __FILE__ , &
                          __LINE__  )
    WRITE(*,FMT='(A,A,F10.6,A)') TRIM(itestfullname),':  current_time dayOfYear_r8 = < ',dayr8,' >'

    subtract_time = current_time - increment
    CALL ESMF_TimeGet( subtract_time, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeGet() ', &
                          __FILE__ , &
                          __LINE__  )
    CALL fraction_to_string( Sn, Sd, frac_str )
    str = TRIM(str)//TRIM(frac_str)
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  current_time-increment = <',TRIM(str),'>'

    add_time = current_time + increment
    CALL ESMF_TimeGet( add_time, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
    CALL test_check_error( ESMF_SUCCESS, rc, &
                          TRIM(itestfullname)//'ESMF_TimeGet() ', &
                          __FILE__ , &
                          __LINE__  )
    CALL fraction_to_string( Sn, Sd, frac_str )
    str = TRIM(str)//TRIM(frac_str)
    WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  current_time+increment = <',TRIM(str),'>'

    ! Advance clock.  
    itestfullname = TRIM(itestname)//'ADVANCE'
    itimestep = 0
    DO WHILE ( .NOT. ESMF_ClockIsStopTime(domain_clock ,rc=rc) )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestfullname)//'ESMF_ClockIsStopTime() ', &
                            __FILE__ , &
                            __LINE__  )
      itimestep = itimestep + 1

      CALL ESMF_ClockAdvance( domain_clock, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestfullname)//'ESMF_ClockAdvance() ', &
                            __FILE__ , &
                            __LINE__  )

      CALL ESMF_ClockGet( domain_clock, CurrTime=current_time, &
                          rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestfullname)//'ESMF_ClockGet() ', &
                            __FILE__ , &
                            __LINE__  )

      CALL ESMF_TimeGet( current_time, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestfullname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      CALL fraction_to_string( Sn, Sd, frac_str )
      str = TRIM(str)//TRIM(frac_str)
      WRITE(*,FMT='(A,A,I6.6,A,A,A)') TRIM(itestfullname),':  count = ', &
        itimestep,'  current_time = <',TRIM(str),'>'

      subtract_time = current_time - increment
      CALL ESMF_TimeGet( subtract_time, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestfullname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      CALL fraction_to_string( Sn, Sd, frac_str )
      str = TRIM(str)//TRIM(frac_str)
      WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  current_time-increment = <',TRIM(str),'>'

      add_time = current_time + increment
      CALL ESMF_TimeGet( add_time, timeString=str, Sn=Sn, Sd=Sd, rc=rc )
      CALL test_check_error( ESMF_SUCCESS, rc, &
                            TRIM(itestfullname)//'ESMF_TimeGet() ', &
                            __FILE__ , &
                            __LINE__  )
      CALL fraction_to_string( Sn, Sd, frac_str )
      str = TRIM(str)//TRIM(frac_str)
      WRITE(*,FMT='(A,A,A,A)') TRIM(itestfullname),':  current_time+increment = <',TRIM(str),'>'

    ENDDO

    DEALLOCATE( domain_clock )
  
  END SUBROUTINE test_clock_advance

END MODULE my_tests


#if defined( TIME_F90_ONLY ) 

! TBH:  Improve the build of Test1.exe to use WRF versions of these 
! TBH:  routines and remove these hacked-in duplicates!!  

SUBROUTINE wrf_abort
  IMPLICIT NONE
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  INCLUDE 'mpif.h'
  INTEGER ierr
  CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
  STOP
#endif
END SUBROUTINE wrf_abort

SUBROUTINE wrf_message( str )
  IMPLICIT NONE
  CHARACTER*(*) str
#if defined( DM_PARALLEL ) && ! defined( STUBMPI)
  write(0,*) str
#endif
  print*, str
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2( str )
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) str
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3( file_str, line, str )
  USE my_tests
  IMPLICIT NONE
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line  ! only print file and line if line > 0
  CHARACTER*(*) str
  CHARACTER*256 :: line_str
  write(line_str,'(i6)') line
  ! special behavior for testing since Fortran cannot catch exceptions
 IF ( WRF_ERROR_FATAL_PRINT ) THEN
  ! just print message and continue
  CALL wrf_message( 'ERROR IN FILE:  '//TRIM(file_str)//'  LINE:  '//TRIM(line_str) )
 ELSE
  ! normal behavior
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
#else
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2( str )
  CALL wrf_message2( '-------------------------------------------' )
#endif
  CALL wrf_abort
 ENDIF
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_error_fatal( str )
  IMPLICIT NONE
  CHARACTER*(*) str
  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

#endif


! Check to see if expected value == actual value
! If not, print message and exit.
SUBROUTINE test_check_error( expected, actual, str, file_str, line )
  IMPLICIT NONE
  INTEGER , INTENT (IN) :: expected
  INTEGER , INTENT (IN) :: actual
  CHARACTER*(*) str
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc
  IF ( expected .ne. actual ) THEN
    WRITE (rc_str,*) '  Routine returned error code = ',actual
    str_with_rc = 'FAIL:  '//TRIM(str)//TRIM(rc_str)
    CALL wrf_error_fatal3( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE test_check_error



PROGRAM time_manager_test
  USE ESMF_Mod
  USE my_tests
  IMPLICIT NONE
  INTEGER :: rc

  PRINT *,'BEGIN TEST SUITE'

  CALL ESMF_Initialize( defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )
  CALL test_check_error( ESMF_SUCCESS, rc, &
                        'ESMF_Initialize() ', &
                        __FILE__ , &
                        __LINE__  )
!  PRINT *,'DEBUG:  back from ESMF_Initialize(), rc = ',rc

!  CALL test_print(  t_yy,  t_mm,  t_dd,  t_h,  t_m,  t_s, &
!                   ti_yy, ti_mm, ti_dd, ti_h, ti_m, ti_s, &
!                   res_str, testname )

  ! Print times
  ! "vanilla" tests
!  PRINT *,'DEBUG:  calling 1st test_print()'
  CALL test_print( t_yy=2001,  t_mm=12,  t_dd=3,  t_h=1,  t_m=20,  t_s=10, &
    res_str='2001-12-03_01:20:10', testname='printT_1' )
!  PRINT *,'DEBUG:  back from 1st test_print()'
  CALL test_print( t_yy=0,  t_mm=1,  t_dd=1,  t_h=0,  t_m=0,  t_s=0, &
    res_str='0000-01-01_00:00:00', testname='printT_2' )
  CALL test_print( t_yy=2003,  t_mm=12,  t_dd=30,  t_h=23,  t_m=59,  t_s=50, &
    res_str='2003-12-30_23:59:50', testname='printT_3' )
  CALL test_print( t_yy=2003,  t_mm=12,  t_dd=31,  t_h=23,  t_m=59,  t_s=50, &
    res_str='2003-12-31_23:59:50', testname='printT_4' )
  CALL test_print( t_yy=2004,  t_mm=12,  t_dd=30,  t_h=23,  t_m=59,  t_s=50, &
    res_str='2004-12-30_23:59:50', testname='printT_5' )
  CALL test_print( t_yy=2004,  t_mm=12,  t_dd=31,  t_h=23,  t_m=59,  t_s=50, &
    res_str='2004-12-31_23:59:50', testname='printT_6' )
!$$$  NOTE that this fails -- need to fix up output string for negative year
!  CALL test_print( t_yy=-2004,  t_mm=12,  t_dd=31,  t_h=23,  t_m=59,  t_s=50, &
!    res_str='-2004-12-31_23:59:50', testname='printT_6' )

  ! these test default behavior of test harness
  CALL test_print( t_s=0, &
    res_str='0000-01-01_00:00:00', testname='printT_D1' )
  CALL test_print( t_yy=0, &
    res_str='0000-01-01_00:00:00', testname='printT_D2' )

  ! fractions
  CALL test_print( t_yy=2001,  t_mm=12,  t_dd=3,  t_h=1,  t_m=20,  t_s=10, &
    t_sn=1, t_sd=3, &
    res_str='2001-12-03_01:20:10+01/03', testname='printT_F1' )
  CALL test_print( t_yy=2001,  t_mm=12,  t_dd=3,  t_h=1,  t_m=20,  t_s=10, &
    t_sn=4, t_sd=3, &
    res_str='2001-12-03_01:20:11+01/03', testname='printT_F2' )
  CALL test_print( t_yy=2001,  t_mm=12,  t_dd=3,  t_h=1,  t_m=20,  t_s=10, &
    t_sn=12, t_sd=3, &
    res_str='2001-12-03_01:20:14', testname='printT_F3' )
  CALL test_print( t_yy=2001,  t_mm=12,  t_dd=3,  t_h=1,  t_m=20,  t_s=10, &
    t_sn=-1, t_sd=3, &
    res_str='2001-12-03_01:20:09+02/03', testname='printT_F4' )

  ! ERROR, MM out of range
!$$$here...  fix so this just prints "ERROR:  <testname>" in failure case
!$$$here...  also need "expect_fail" to reverse sense of PASS/FAIL message for 
!$$$here...  tests that should fail
!  CALL test_print( t_yy=2001,  t_mm=13,  t_dd=3,  t_h=1,  t_m=20,  t_s=10, &
!    res_str='2002-01-03_01:20:10', testname='printT_E1', expect_error=.TRUE. )

  ! Print time intervals
  ! "vanilla" tests
  CALL test_print( ti_yy=0,  ti_mm=0,  ti_dd=0,  ti_h=0,  ti_m=0,  ti_s=0, &
    res_str='0000000000_000:000:000', testname='printTI_1' )
  CALL test_print( ti_yy=0,  ti_mm=0,  ti_dd=500,  ti_h=0,  ti_m=0,  ti_s=7270, &
    res_str='0000000500_002:001:010', testname='printTI_2' )

  ! these test default behavior of test harness
  CALL test_print( ti_s=0, &
    res_str='0000000000_000:000:000', testname='printTI_D1' )
  CALL test_print( ti_yy=0, &
    res_str='0000000000_000:000:000', testname='printTI_D2' )

  ! these test negative values
  CALL test_print( ti_yy=0000,  ti_mm=0,  ti_dd=-3,  ti_h=-1,  ti_m=-20,  ti_s=-10, &
    res_str='-0000000003_001:020:010', testname='printTI_N1' )

  ! these test mixed values
  CALL test_print( ti_yy=0000,  ti_mm=0,  ti_dd=-3,  ti_h=1,  ti_m=20,  ti_s=10, &
    res_str='-0000000002_022:039:050', testname='printTI_M1' )

  ! fractions
  CALL test_print( ti_yy=0000,  ti_mm=0,  ti_dd=3,  ti_h=1,  ti_m=20,  ti_s=10, &
    ti_sn=1, ti_sd=3, &
    res_str='0000000003_001:020:010+01/03', testname='printTI_F1' )
  CALL test_print( ti_yy=0000,  ti_mm=0,  ti_dd=3,  ti_h=1,  ti_m=20,  ti_s=10, &
    ti_sn=5, ti_sd=3, &
    res_str='0000000003_001:020:011+02/03', testname='printTI_F2' )
  CALL test_print( ti_yy=0000,  ti_mm=0,  ti_dd=-3,  ti_h=-1,  ti_m=-20,  ti_s=-10, &
    ti_sn=-1, ti_sd=3, &
    res_str='-0000000003_001:020:010-01/03', testname='printTI_F3' )
  CALL test_print( ti_yy=0000,  ti_mm=0,  ti_dd=-3,  ti_h=-1,  ti_m=-20,  ti_s=-10, &
    ti_sn=1, ti_sd=3, &
    res_str='-0000000003_001:020:009-02/03', testname='printTI_F4' )

  ! these test non-normalized values
!  CALL test_print( ti_yy=2001,  ti_mm=1,  ti_dd=3,  ti_h=1,  ti_m=20,  ti_s=10, &
!    res_str='02001-001-003_001:020:010', testname='printTI_NN1', expect_error=.TRUE. )
!  CALL test_print( ti_yy=2001,  ti_mm=12,  ti_dd=3,  ti_h=1,  ti_m=20,  ti_s=10, &
!    res_str='02002-000-003_001:020:010', testname='printTI_NN2', expect_error=.TRUE. )
!  CALL test_print( ti_yy=2002,  ti_mm=5,  ti_dd=500,  ti_h=0,  ti_m=0,  ti_s=7270, &
!    res_str='02002-005-500_002:001:010', testname='printTI_NN3', expect_error=.TRUE. )

  ! Addition tests
  ! ESMF_Time = ESMF_Time + ESMF_TimeInterval
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2001,  op1_t_mm=12,  op1_t_dd=3,  op1_t_h=1,  op1_t_m=20,  op1_t_s=10, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=3, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2001,  res_t_mm=12,  res_t_dd=3,  res_t_h=4,  res_t_m=30,  res_t_s=20, &
    testname='AddT_T_TI1' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2001,  op1_t_mm=12,  op1_t_dd=31,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2002,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI2' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2003,  op1_t_mm=12,  op1_t_dd=31,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI3' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=31,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2005,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI4' )
  ! this case hung after the CCSM contribution
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=31,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI5' )
! NOTE:  CCSM folks need to decide what it means to add "1 month" to Feb. 29.  And all the 
!        other very similar cases.  Then, write this unit test!  
!  CALL test_arithmetic( add_op=.TRUE.,                                             &
!     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=31,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
!    op2_ti_yy=   2, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
!     res_t_yy=2007,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
!    testname='AddT_T_TI6' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=365, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2005,  res_t_mm=12,  res_t_dd=30,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI7' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=367, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2006,  res_t_mm=01,  res_t_dd=01,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI8' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2003,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=365, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=29,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI9' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2003,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=366, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=30,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI10' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2003,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=367, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=31,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI11' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2003,  op1_t_mm=12,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=368, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2005,  res_t_mm=01,  res_t_dd=01,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI12' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=03,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=365, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2005,  res_t_mm=03,  res_t_dd=30,  res_t_h=8,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI13' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=03,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=365, op2_ti_h=22, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2005,  res_t_mm=03,  res_t_dd=31,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI14' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=03,  op1_t_dd=30,  op1_t_h=4,  op1_t_m=30,  op1_t_s=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=366, op2_ti_h=22, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2005,  res_t_mm=04,  res_t_dd=01,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_T_TI15' )
  ! ESMF_Time = ESMF_Time + ESMF_TimeInterval with fractions
  CALL test_arithmetic( add_op=.TRUE.,                                             &
     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=31,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
     op1_t_sn=01,  op1_t_sd=03, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
    op2_ti_sn=01, op2_ti_sd=03, &
     res_t_yy=2005,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
     res_t_sn=02,  res_t_sd=03, &
    testname='AddT_T_TI_F1' )
  ! this should fail (and does)
!  CALL test_arithmetic( add_op=.TRUE.,                                             &
!     op1_t_yy=2004,  op1_t_mm=12,  op1_t_dd=31,  op1_t_h=22,  op1_t_m=30,  op1_t_s=00, &
!     op1_t_sn=01,  op1_t_sd=03, &
!    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
!    op2_ti_sn=01, op2_ti_sd=03, &
!     res_t_yy=2005,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
!     res_t_sn=01,  res_t_sd=03, &
!    testname='AddT_T_TI_F2' )
  ! ESMF_Time = ESMF_TimeInterval + ESMF_Time
  CALL test_arithmetic( add_op=.TRUE.,                                             &
    op1_ti_yy=   0, op1_ti_mm= 0, op1_ti_dd=0, op1_ti_h=3, op1_ti_m=10, op1_ti_s=10, &
     op2_t_yy=2001,  op2_t_mm=12,  op2_t_dd=3,  op2_t_h=1,  op2_t_m=20,  op2_t_s=10, &
     res_t_yy=2001,  res_t_mm=12,  res_t_dd=3,  res_t_h=4,  res_t_m=30,  res_t_s=20, &
    testname='AddT_TI_T1' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
    op1_ti_yy=   0, op1_ti_mm= 0, op1_ti_dd=0, op1_ti_h=4, op1_ti_m=10, op1_ti_s=10, &
     op2_t_yy=2001,  op2_t_mm=12,  op2_t_dd=31,  op2_t_h=22,  op2_t_m=30,  op2_t_s=00, &
     res_t_yy=2002,  res_t_mm= 1,  res_t_dd=1,  res_t_h=2,  res_t_m=40,  res_t_s=10, &
    testname='AddT_TI_T2' )
  ! ESMF_TimeInterval = ESMF_TimeInterval + ESMF_TimeInterval
  CALL test_arithmetic( add_op=.TRUE.,                                             &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=3, op1_ti_h=1, op1_ti_m=20, op1_ti_s=10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=1, op2_ti_h=1, op2_ti_m=10, op2_ti_s=10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=4, res_ti_h=2, res_ti_m=30, res_ti_s=20, &
    testname='AddTI_TI_TI1' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=-3, op1_ti_h=-1, op1_ti_m=-20, op1_ti_s=-10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=1, op2_ti_h=1, op2_ti_m=10, op2_ti_s=10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=-2, res_ti_h=0, res_ti_m=-10, res_ti_s=00, &
    testname='AddTI_TI_TI2' )
  CALL test_arithmetic( add_op=.TRUE.,                                             &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=-3, op1_ti_h=-1, op1_ti_m=-20, op1_ti_s=-10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=-1, op2_ti_h=-1, op2_ti_m=-10, op2_ti_s=-10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=-4, res_ti_h=-2, res_ti_m=-30, res_ti_s=-20, &
    testname='AddTI_TI_TI3' )

  ! Subtraction tests
  ! ESMF_Time = ESMF_Time - ESMF_TimeInterval
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2001,  op1_t_mm=12,  op1_t_dd=3,  op1_t_h=1,  op1_t_m=20,  op1_t_s=10, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=3, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2001,  res_t_mm=12,  res_t_dd=2,  res_t_h=22, res_t_m=10,  res_t_s=0,  &
    testname='SubtractT_T_TI1' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2005,  op1_t_mm=1,   op1_t_dd=1,  op1_t_h=0,  op1_t_m=00,  op1_t_s=0,  &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=0, op2_ti_m=00, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=31, res_t_h=23, res_t_m=59,  res_t_s=50, &
    testname='SubtractT_T_TI2' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2004,  op1_t_mm=1,   op1_t_dd=1,  op1_t_h=0,  op1_t_m=00,  op1_t_s=0,  &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=0, op2_ti_m=00, op2_ti_s=10, &
     res_t_yy=2003,  res_t_mm=12,  res_t_dd=31, res_t_h=23, res_t_m=59,  res_t_s=50, &
    testname='SubtractT_T_TI3' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2003,  op1_t_mm=1,   op1_t_dd=1,  op1_t_h=0,  op1_t_m=00,  op1_t_s=0,  &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=0, op2_ti_m=00, op2_ti_s=10, &
     res_t_yy=2002,  res_t_mm=12,  res_t_dd=31, res_t_h=23, res_t_m=59,  res_t_s=50, &
    testname='SubtractT_T_TI4' )
  CALL test_arithmetic( add_op=.FALSE.,                                             &
     op1_t_yy=2005,  op1_t_mm=04,  op1_t_dd=01,  op1_t_h=2,  op1_t_m=40,  op1_t_s=10, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=366, op2_ti_h=22, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=03,  res_t_dd=30,  res_t_h=4,  res_t_m=30,  res_t_s=00, &
    testname='SubtractT_T_TI5' )
  CALL test_arithmetic( add_op=.FALSE.,                                             &
     op1_t_yy=2006,  op1_t_mm=01,  op1_t_dd=01,  op1_t_h=8,  op1_t_m=40,  op1_t_s=10, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=367, op2_ti_h=4, op2_ti_m=10, op2_ti_s=10, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=30,  res_t_h=4,  res_t_m=30,  res_t_s=00, &
    testname='SubtractT_T_TI6' )
  ! ESMF_Time = ESMF_Time - ESMF_TimeInterval with fractions
  CALL test_arithmetic( add_op=.FALSE.,                                             &
     op1_t_yy=2005,  op1_t_mm=01,  op1_t_dd=01,  op1_t_h=00,  op1_t_m=00,  op1_t_s=00, &
     op1_t_sn=00,  op1_t_sd=00, &
    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=0, op2_ti_m=00, op2_ti_s=01, &
    op2_ti_sn=01, op2_ti_sd=03, &
     res_t_yy=2004,  res_t_mm=12,  res_t_dd=31,  res_t_h=23,  res_t_m=59,  res_t_s=58, &
     res_t_sn=02,  res_t_sd=03, &
    testname='SubtractT_T_TI_F1' )
  ! ESMF_TimeInterval = ESMF_Time - ESMF_Time
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2001,  op1_t_mm=12,  op1_t_dd=3,  op1_t_h=1,  op1_t_m=20,  op1_t_s=10, &
     op2_t_yy=2001,  op2_t_mm=12,  op2_t_dd=1,  op2_t_h=1,  op2_t_m=10,  op2_t_s=10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=2, res_ti_h=0, res_ti_m=10, res_ti_s=0,  &
    testname='SubtractTI_T_T1' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2002,  op1_t_mm=1,   op1_t_dd=1,  op1_t_h=0,  op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2001,  op2_t_mm=12,  op2_t_dd=31, op2_t_h=23, op2_t_m=59,  op2_t_s=50, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=0, res_ti_h=0, res_ti_m=00, res_ti_s=10, &
    testname='SubtractTI_T_T2' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2005,  op1_t_mm=1,   op1_t_dd=1,  op1_t_h=0,  op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2004,  op2_t_mm=12,  op2_t_dd=31, op2_t_h=23, op2_t_m=59,  op2_t_s=50, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=0, res_ti_h=0, res_ti_m=00, res_ti_s=10, &
    testname='SubtractTI_T_T3' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2003,  op1_t_mm=03,  op1_t_dd=01, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2003,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=23, op2_t_m=59,  op2_t_s=50, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=0, res_ti_h=0, res_ti_m=00, res_ti_s=10, &
    testname='SubtractTI_T_T4' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2004,  op1_t_mm=03,  op1_t_dd=01, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2004,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=23, op2_t_m=59,  op2_t_s=50, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=1, res_ti_h=0, res_ti_m=00, res_ti_s=10, &
    testname='SubtractTI_T_T5' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2002,  op1_t_mm=02,  op1_t_dd=28, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2002,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=0, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T6' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2003,  op1_t_mm=02,  op1_t_dd=28, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2002,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=365, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T7' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2004,  op1_t_mm=02,  op1_t_dd=28, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2003,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=365, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T8' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2005,  op1_t_mm=02,  op1_t_dd=28, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2004,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=366, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T9' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2003,  op1_t_mm=03,  op1_t_dd=01, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2002,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=366, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T10' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2005,  op1_t_mm=03,  op1_t_dd=01, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2004,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=367, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T11' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2005,  op1_t_mm=03,  op1_t_dd=01, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=2004,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=23, op2_t_m=59,  op2_t_s=50, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=366, res_ti_h=0, res_ti_m=00, res_ti_s=10, &
    testname='SubtractTI_T_T12' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=2004,  op1_t_mm=02,  op1_t_dd=28, op1_t_h=23, op1_t_m=59,  op1_t_s=50, &
     op2_t_yy=2005,  op2_t_mm=03,  op2_t_dd=01, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=-366, res_ti_h=0, res_ti_m=00, res_ti_s=-10, &
    testname='SubtractTI_T_T13' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
     op1_t_yy=-2002,  op1_t_mm=02,  op1_t_dd=28, op1_t_h=00, op1_t_m=00,  op1_t_s=00, &
     op2_t_yy=-2002,  op2_t_mm=02,  op2_t_dd=28, op2_t_h=00, op2_t_m=00,  op2_t_s=00, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=0, res_ti_h=0, res_ti_m=00, res_ti_s=00, &
    testname='SubtractTI_T_T14' )
  ! ESMF_TimeInterval = ESMF_TimeInterval - ESMF_TimeInterval
  CALL test_arithmetic( add_op=.FALSE.,                                            &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=3, op1_ti_h=1, op1_ti_m=20, op1_ti_s=10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=1, op2_ti_h=1, op2_ti_m=10, op2_ti_s=10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=2, res_ti_h=0, res_ti_m=10, res_ti_s=0,  &
    testname='SubtractTI_TI_TI1' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=3, op1_ti_h=1, op1_ti_m=20, op1_ti_s=10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=-1, op2_ti_h=-1, op2_ti_m=-10, op2_ti_s=-10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=4, res_ti_h=2, res_ti_m=30, res_ti_s=20,  &
    testname='SubtractTI_TI_TI2' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=-1, op1_ti_h=-1, op1_ti_m=-10, op1_ti_s=-10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=-3, op2_ti_h=-1, op2_ti_m=-20, op2_ti_s=-10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=2, res_ti_h=0, res_ti_m=10, res_ti_s=00,  &
    testname='SubtractTI_TI_TI3' )
  ! Negative result ESMF_TimeInterval = ESMF_TimeInterval - ESMF_TimeInterval
  CALL test_arithmetic( add_op=.FALSE.,                                            &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=1, op1_ti_h=1, op1_ti_m=10, op1_ti_s=10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=3, op2_ti_h=1, op2_ti_m=20, op2_ti_s=10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=-2, res_ti_h=0, res_ti_m=-10, res_ti_s=0,  &
    testname='SubtractTI_TI_TIN1' )
  CALL test_arithmetic( add_op=.FALSE.,                                            &
    op1_ti_yy=0000, op1_ti_mm=00, op1_ti_dd=-1, op1_ti_h=-1, op1_ti_m=-10, op1_ti_s=-10, &
    op2_ti_yy=0000, op2_ti_mm=00, op2_ti_dd=3, op2_ti_h=1, op2_ti_m=20, op2_ti_s=10, &
    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=-4, res_ti_h=-2, res_ti_m=-30, res_ti_s=-20,  &
    testname='SubtractTI_TI_TIN2' )

  ! Un-normalized ESMF_TimeInterval = ESMF_TimeInterval - ESMF_TimeInterval
  ! this is an error
!  CALL test_arithmetic( add_op=.FALSE.,                                            &
!    op1_ti_yy=2001, op1_ti_mm=11, op1_ti_dd=3, op1_ti_h=1, op1_ti_m=20, op1_ti_s=10, &
!    op2_ti_yy=2001, op2_ti_mm=11, op2_ti_dd=1, op2_ti_h=1, op2_ti_m=10, op2_ti_s=10, &
!    res_ti_yy=0000, res_ti_mm=00, res_ti_dd=2, res_ti_h=0, res_ti_m=10, res_ti_s=0,  &
!    testname='SubtractTI_TI_TIU1', expect_error=.TRUE. )

  ! this one should FAIL, and does
!  CALL test_arithmetic( add_op=.TRUE.,                                             &
!     op1_t_yy=2001,  op1_t_mm=12,  op1_t_dd=3,  op1_t_h=1,  op1_t_m=20,  op1_t_s=10, &
!    op2_ti_yy=   0, op2_ti_mm= 0, op2_ti_dd=0, op2_ti_h=3, op2_ti_m=10, op2_ti_s=10, &
!     res_t_yy=2002,  res_t_mm=12,  res_t_dd=3,  res_t_h=4,  res_t_m=30,  res_t_s=20, &
!    testname='AddTT1' )

  ! Multiplication tests
  ! ESMF_TimeInterval = ESMF_TimeInterval * INTEGER
  CALL test_arithmetic( multiply_op=.TRUE.,                &
    op1_ti_dd=3,  op1_ti_h=12,  op1_ti_m=18,  op1_ti_s=33, &
    op2_int=2,                                             &
    res_ti_dd=6,  res_ti_h=24, res_ti_m=37,  res_ti_s=06,  &
    testname='MultiplyTI_TI_INT1' )
  CALL test_arithmetic( multiply_op=.TRUE.,                &
    op1_ti_dd=350,  op1_ti_h=23,  op1_ti_m=50,  op1_ti_s=50, &
    op2_int=2,                                             &
    res_ti_dd=701,  res_ti_h=23, res_ti_m=41,  res_ti_s=40,&
    testname='MultiplyTI_TI_INT2' )
  CALL test_arithmetic( multiply_op=.TRUE.,                &
    op1_ti_s=01, op1_ti_sn=03, op1_ti_sd=04,               &
    op2_int=8,                                             &
    res_ti_s=14,                                           &
    testname='MultiplyTI_TI_INT3' )

  ! Division tests
  ! ESMF_TimeInterval = ESMF_TimeInterval / INTEGER
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=3,  op1_ti_h=12,  op1_ti_m=18,  op1_ti_s=33, &
    op2_int=3,                                             &
    res_ti_dd=1,  res_ti_h=04, res_ti_m=06,  res_ti_s=11,  &
    testname='DivideTI_TI_INT1' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=3,  op1_ti_h=12,  op1_ti_m=18,  op1_ti_s=33, &
    op2_int=4,                                             &
    res_ti_dd=0,  res_ti_h=21, res_ti_m=04,  res_ti_s=38,  &
    res_ti_sn=1,  res_ti_sd=4,                             &
    testname='DivideTI_TI_INT2' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_s=01, op1_ti_sn=03, op1_ti_sd=04,               &
    op2_int=5,                                             &
    res_ti_s=0, res_ti_sn=7,  res_ti_sd=20,                &
    testname='DivideTI_TI_INT3' )
  ! INTEGER = ESMF_TimeInterval / ESMF_TimeInterval
  ! this operator truncates to whole integers
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=3,  op1_ti_h=12,  op1_ti_m=18,  op1_ti_s=33, &
    op2_ti_dd=3,  op2_ti_h=12,  op2_ti_m=18,  op2_ti_s=33, &
    res_int=1,                                             &
    testname='DivideINT_TI_TI1' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=6,  op1_ti_h=24,  op1_ti_m=36,  op1_ti_s=66, &
    op2_ti_dd=3,  op2_ti_h=12,  op2_ti_m=18,  op2_ti_s=33, &
    res_int=2,                                             &
    testname='DivideINT_TI_TI2' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=0,  op1_ti_h=00,  op1_ti_m=00,  op1_ti_s=00, &
    op2_ti_dd=3,  op2_ti_h=12,  op2_ti_m=18,  op2_ti_s=33, &
    res_int=0,                                             &
    testname='DivideINT_TI_TI3' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=1,  op1_ti_h=00,  op1_ti_m=00,  op1_ti_s=00, &
    op2_ti_dd=0,  op2_ti_h=01,  op2_ti_m=00,  op2_ti_s=00, &
    res_int=24,                                            &
    testname='DivideINT_TI_TI4' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=1,  op1_ti_h=00,  op1_ti_m=00,  op1_ti_s=00, &
    op2_ti_dd=0,  op2_ti_h=00,  op2_ti_m=01,  op2_ti_s=00, &
    res_int=1440,                                          &
    testname='DivideINT_TI_TI5' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=1,  op1_ti_h=00,  op1_ti_m=00,  op1_ti_s=00, &
    op2_ti_dd=0,  op2_ti_h=00,  op2_ti_m=00,  op2_ti_s=01, &
    res_int=86400,                                         &
    testname='DivideINT_TI_TI6' )
  ! rounding
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=0,  op1_ti_h=00,  op1_ti_m=00,  op1_ti_s=03, &
    op2_ti_dd=0,  op2_ti_h=00,  op2_ti_m=00,  op2_ti_s=02, &
    res_int=1,                                             &
    testname='DivideINT_TI_TIR1' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=1,  op1_ti_h=00,  op1_ti_m=00,  op1_ti_s=02, &
    op2_ti_dd=1,  op2_ti_h=00,  op2_ti_m=00,  op2_ti_s=03, &
    res_int=0,                                             &
    testname='DivideINT_TI_TIR2' )
  ! fractional operands
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_m=00,  op1_ti_s=00, op1_ti_sn=03, op1_ti_sd=04, &
    op2_ti_m=00,  op2_ti_s=00, op2_ti_sn=03, op2_ti_sd=04, &
    res_int=1,                                             &
    testname='DivideINT_TI_TIF1' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_m=00,  op1_ti_s=00, op1_ti_sn=06, op1_ti_sd=08, &
    op2_ti_m=00,  op2_ti_s=00, op2_ti_sn=03, op2_ti_sd=04, &
    res_int=1,                                             &
    testname='DivideINT_TI_TIF2' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_m=00,  op1_ti_s=00, op1_ti_sn=03, op1_ti_sd=04, &
    op2_ti_m=00,  op2_ti_s=00, op2_ti_sn=04, op2_ti_sd=03, &
    res_int=0,                                             &
    testname='DivideINT_TI_TIF3' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_m=00,  op1_ti_s=02, op1_ti_sn=03, op1_ti_sd=04, &
    op2_ti_m=00,  op2_ti_s=01, op2_ti_sn=01, op2_ti_sd=03, &
    res_int=2,                                             &
    testname='DivideINT_TI_TIF4' )
  ! negative operands
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=-6,  op1_ti_h=-24,  op1_ti_m=-36,  op1_ti_s=-66, &
    op2_ti_dd=3,  op2_ti_h=12,  op2_ti_m=18,  op2_ti_s=33, &
    res_int=-2,                                             &
    testname='DivideINT_TI_TIN1' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=6,  op1_ti_h=24,  op1_ti_m=36,  op1_ti_s=66, &
    op2_ti_dd=-3,  op2_ti_h=-12,  op2_ti_m=-18,  op2_ti_s=-33, &
    res_int=-2,                                             &
    testname='DivideINT_TI_TIN2' )
  CALL test_arithmetic( multiply_op=.FALSE.,               &
    op1_ti_dd=-6,  op1_ti_h=-24,  op1_ti_m=-36,  op1_ti_s=-66, &
    op2_ti_dd=-3,  op2_ti_h=-12,  op2_ti_m=-18,  op2_ti_s=-33, &
    res_int=2,                                             &
    testname='DivideINT_TI_TIN3' )

!$$$here...  modify these to add self-test PASS/FAIL output
  CALL test_clock_advance(                                                    &
    start_yy=2002, start_mm=12, start_dd=27, start_h=3, start_m=0, start_s=0, &
     stop_yy=2002,  stop_mm=12,  stop_dd=28,  stop_h=8,  stop_m=0,  stop_s=0, &
    timestep_d=0, timestep_h=0, timestep_m=0, timestep_s=600,                 &
    testname="SimpleClockAdvance" )

  CALL test_clock_advance(                                                    &
    start_yy=2003, start_mm=12, start_dd=29, start_h=9, start_m=0, start_s=0, &
     stop_yy=2004,  stop_mm=1,   stop_dd=2,   stop_h=9,  stop_m=0,  stop_s=0, &
    timestep_d=0, timestep_h=0, timestep_m=0, timestep_s=3600,                &
    testname="StdYearClockAdvance", increment_S=10 )

  CALL test_clock_advance(                                                    &
    start_yy=2004, start_mm=12, start_dd=29, start_h=9, start_m=0, start_s=0, &
     stop_yy=2005,  stop_mm=1,   stop_dd=2,   stop_h=9,  stop_m=0,  stop_s=0, &
    timestep_d=0, timestep_h=0, timestep_m=0, timestep_s=3600,                &
    testname="LeapYearClockAdvance", increment_S=10 )

  ! NRCM domain 3 case:  120 seconds / 9 
  ! 18 timesteps through end of leap year
  CALL test_clock_advance(                                                    &
    start_yy=2004, start_mm=12, start_dd=31, start_h=23, start_m=58, start_s=0,&
     stop_yy=2005,  stop_mm=1,   stop_dd=1,   stop_h=0,  stop_m=2,  stop_s=0, &
    timestep_d=0, timestep_h=0, timestep_m=0, timestep_s=13,                  &
    timestep_sn=1, timestep_sd=3,                                             &
    testname="LeapYearFractionClockAdvance",                                  &
    increment_S=1, increment_Sn=1, increment_Sd=3 )

  CALL ESMF_Finalize( rc=rc )
  CALL test_check_error( ESMF_SUCCESS, rc, &
                        'ESMF_Finalize() ', &
                        __FILE__ , &
                        __LINE__  )

  PRINT *,'END TEST SUITE'

END PROGRAM time_manager_test

