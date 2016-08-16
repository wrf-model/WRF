!
! Compare_Float_Numbers
!
! Module containing routines to perform equality and relational
! comparisons on floating point numbers.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-Apr-2003
!                       paul.vandelst@noaa.gov
!

MODULE Compare_Float_Numbers


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: Single, Double
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: DEFAULT_N_SIGFIG
  ! Operators
  PUBLIC :: OPERATOR (.EqualTo.)
  PUBLIC :: OPERATOR (.GreaterThan.)
  PUBLIC :: OPERATOR (.LessThan.)
  ! Procedures
  PUBLIC :: Compare_Float
  PUBLIC :: Tolerance
  PUBLIC :: Compares_Within_Tolerance


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Compare_Float
    MODULE PROCEDURE Compare_Real_Single
    MODULE PROCEDURE Compare_Real_Double
    MODULE PROCEDURE Compare_Complex_Single
    MODULE PROCEDURE Compare_Complex_Double
  END INTERFACE Compare_Float

  INTERFACE OPERATOR (.EqualTo.)
    MODULE PROCEDURE EqualTo_Real_Single
    MODULE PROCEDURE EqualTo_Real_Double
    MODULE PROCEDURE EqualTo_Complex_Single
    MODULE PROCEDURE EqualTo_Complex_Double
  END INTERFACE OPERATOR (.EqualTo.)

  INTERFACE OPERATOR (.GreaterThan.)
    MODULE PROCEDURE Is_Greater_Than_Single
    MODULE PROCEDURE Is_Greater_Than_Double
  END INTERFACE OPERATOR (.GreaterThan.)

  INTERFACE OPERATOR (.LessThan.)
    MODULE PROCEDURE Is_Less_Than_Single
    MODULE PROCEDURE Is_Less_Than_Double
  END INTERFACE OPERATOR (.LessThan.)

  INTERFACE Tolerance
    MODULE PROCEDURE Tolerance_Real_Single
    MODULE PROCEDURE Tolerance_Real_Double
    MODULE PROCEDURE Tolerance_Complex_Single
    MODULE PROCEDURE Tolerance_Complex_Double
  END INTERFACE Tolerance

  INTERFACE Compares_Within_Tolerance
    MODULE PROCEDURE cwt_Real_Single
    MODULE PROCEDURE cwt_Real_Double
    MODULE PROCEDURE cwt_Complex_Single
    MODULE PROCEDURE cwt_Complex_Double
  END INTERFACE Compares_Within_Tolerance

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module Version Id
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: Compare_Float_Numbers.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Numeric literals
  REAL(Single), PARAMETER :: SP_ZERO = 0.0_Single
  REAL(Double), PARAMETER :: DP_ZERO = 0.0_Double
  REAL(Single), PARAMETER :: SP_ONE = 1.0_Single
  REAL(Double), PARAMETER :: DP_ONE = 1.0_Double
  REAL(Single), PARAMETER :: SP_TEN = 10.0_Single
  REAL(Double), PARAMETER :: DP_TEN = 10.0_Double
  REAL(Single), PARAMETER :: SP_HUNDRED = 100.0_Single
  REAL(Double), PARAMETER :: DP_HUNDRED = 100.0_Double
  REAL(Single), PARAMETER :: SP_COMPARE_CUTOFF = 1.0e-15_Single
  REAL(Double), PARAMETER :: DP_COMPARE_CUTOFF = 1.0e-15_Double
  ! Default number of significant figures
  INTEGER, PARAMETER :: DEFAULT_N_SIGFIG = 6


CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       .EqualTo.
!
! PURPOSE:
!       Relational operator to test the equality of REAL operands.
!
! CALLING SEQUENCE:
!       IF ( x .EqualTo. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single),    REAL(Double)
!                                COMPLEX(Single), COMPLEX(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .EqualTo. y)    The result is a logical value indicating whether
!                          the operands are equal to within numerical precision
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., the numbers are considered equal. For complex
!       input the test is applied separately to the real and imaginary parts.
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION EqualTo_Real_Single( x, y ) RESULT( EqualTo )
    REAL(Single), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    EqualTo = ABS(x-y) < SPACING( MAX(ABS(x),ABS(y)) )
  END FUNCTION EqualTo_Real_Single

  ELEMENTAL FUNCTION EqualTo_Real_Double( x, y ) RESULT( EqualTo )
    REAL(Double), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    EqualTo = ABS(x-y) < SPACING( MAX(ABS(x),ABS(y)) )
  END FUNCTION EqualTo_Real_Double

  ELEMENTAL FUNCTION EqualTo_Complex_Single( x, y ) RESULT( EqualTo )
    COMPLEX(Single), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    REAL(Single) :: rx, ix
    REAL(Single) :: ry, iy
    rx = REAL(x,Single); ix = AIMAG(x)
    ry = REAL(y,Single); iy = AIMAG(y)
    EqualTo = EqualTo_Real_Single( rx, ry ) .AND. EqualTo_Real_Single( ix, iy )
  END FUNCTION EqualTo_Complex_Single

  ELEMENTAL FUNCTION EqualTo_Complex_Double( x, y ) RESULT( EqualTo )
    COMPLEX(Double), INTENT(IN)  :: x, y
    LOGICAL :: EqualTo
    REAL(Double) :: rx, ix
    REAL(Double) :: ry, iy
    rx = REAL(x,Double); ix = AIMAG(x)
    ry = REAL(y,Double); iy = AIMAG(y)
    EqualTo = EqualTo_Real_Double( rx, ry ) .AND. EqualTo_Real_Double( ix, iy )
  END FUNCTION EqualTo_Complex_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       .GreaterThan.
!
! PURPOSE:
!       Relational operator to test if one REAL operand is greater than another.
!
! CALLING SEQUENCE:
!       IF ( x .GreaterThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .GreaterThan. y)    The result is a logical value indicating whether
!                              the operand x is greater than y by more than
!                              the spacing between representable floating point
!                              numbers.
!                              UNITS:      N/A
!                              TYPE:       LOGICAL
!                              DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( x - y ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered greater than y.
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Greater_Than_Single( x, y ) RESULT ( Greater_Than )
    REAL(Single), INTENT(IN) :: x, y
    LOGICAL :: Greater_Than
    IF ( (x-y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Single


  ELEMENTAL FUNCTION Is_Greater_Than_Double( x, y ) RESULT ( Greater_Than )
    REAL(Double), INTENT(IN) :: x, y
    LOGICAL :: Greater_Than
    IF ( (x-y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       .LessThan.
!
! PURPOSE:
!       Relational operator to test if one REAL operand is less than another.
!
! CALLING SEQUENCE:
!       IF ( x .LessThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .LessThan. y)    The result is a logical value indicating whether
!                           the operand x is less than y by more than the
!                           spacing between representable floating point
!                           numbers.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( y - x ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered less than y.
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Less_Than_Single( x, y ) RESULT ( Less_Than )
    REAL(Single), INTENT(IN) :: x, y
    LOGICAL :: Less_Than
    IF ( (y-x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Single


  ELEMENTAL FUNCTION Is_Less_Than_Double( x, y ) RESULT ( Less_Than )
    REAL(Double), INTENT(IN) :: x, y
    LOGICAL :: Less_Than
    IF ( (y-x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Compare_Float
!
! PURPOSE:
!       Function to compare floating point scalars and arrays with adjustible
!       precision tolerance.
!
! CALLING SEQUENCE:
!       Result = Compare_Float( x, y,            &  ! Input
!                               ULP    =ULP    , &  ! Optional input
!                               Percent=Percent  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                                  OR
!                                COMPLEX(Single)
!                                  OR
!                                COMPLEX(Double)
!                    DIMENSION:  Scalar, or any allowed rank array.
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP:         Unit of data precision. The acronym stands for "unit in
!                    the last place," the smallest possible increment or decrement
!                    that can be made using a machine's floating point arithmetic.
!                    A 0.5 ulp maximum error is the best you could hope for, since
!                    this corresponds to always rounding to the nearest representable
!                    floating-point number. Value must be positive - if a negative
!                    value is supplied, the absolute value is used.
!                    If not specified, the default value is 1.
!                    This argument is ignored if the Percent optioanl argument is specifed.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Percent:     Specify a percentage difference value to use in comparing
!                    the numbers rather than testing within some numerical
!                    limit. The ULP argument is ignored if this argument is
!                    specified.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)  for REAL(Single) or COMPLEX(Single) x,y
!                                  OR
!                                REAL(Double)  for REAL(Double) or COMPLEX(Double) x,y
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Result:      The return value is a logical value indicating whether
!                    the inputs are equal (to within the required precision)
!                    .TRUE.  - if the floating point numbers are equal to
!                              within the specified tolerance. 
!                    .FALSE. - if the floating point numbers are different.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Scalar
!
! PROCEDURE:
!       ULP Test
!       --------
!       The test performed is
!
!         ABS( x - y ) < ( ULP * SPACING( MAX(ABS(x),ABS(y)) ) )
!
!       If the result is .TRUE., the numbers are considered equal.
!
!       The intrinsic function SPACING(x) returns the absolute spacing of numbers
!       near the value of x,
!
!                      {     EXPONENT(x)-DIGITS(x)
!                      {  2.0                        for x /= 0
!         SPACING(x) = {
!                      {  
!                      {  TINY(x)                    for x == 0
!
!       The ULP optional argument scales the comparison.
!
!       James Van Buskirk and James Giles suggested this method for floating
!       point comparisons in the comp.lang.fortran newsgroup.
!
!
!       Percent Test
!       ------------
!       The test performed is
!
!         100.0 * ABS((x-y)/x) < Percent
!
!       If the result is .TRUE., the numbers are considered equal.
!
!
!       For complex numbers, the same test is applied to both the real and
!       imaginary parts and each result is ANDed.
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Compare_Real_Single( x, y, ULP, Percent ) RESULT( Compare )
    ! Arguments
    REAL(Single),           INTENT(IN) :: x
    REAL(Single),           INTENT(IN) :: y
    INTEGER     , OPTIONAL, INTENT(IN) :: ULP
    REAL(Single), OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    LOGICAL      :: ULP_Test
    REAL(Single) :: Rel
    
    ! Set up
    ! ------
    ULP_Test = .TRUE.
    IF ( PRESENT(ULP) ) THEN
      Rel = REAL(ABS(ULP), Single)
    ELSE
      Rel = SP_ONE
    END IF
    IF ( PRESENT(Percent) ) THEN
      ULP_Test = .FALSE.
      ! Test for zero x (elementals can't be recursive)
      IF ( ABS(x) < ( SPACING( MAX(ABS(x),SP_ZERO) ) ) ) ULP_Test = .TRUE.
    END IF
    
    ! Compare the numbers
    ! -------------------
    IF ( ULP_Test ) THEN
      Compare = ABS(x-y) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )
    ELSE
      Compare = SP_HUNDRED*ABS((x-y)/x) < Percent
    END IF
  END FUNCTION Compare_Real_Single


  ELEMENTAL FUNCTION Compare_Real_Double( x, y, ULP, Percent ) RESULT( Compare )
    ! Arguments
    REAL(Double),           INTENT(IN) :: x
    REAL(Double),           INTENT(IN) :: y
    INTEGER     , OPTIONAL, INTENT(IN) :: ULP
    REAL(Double), OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    LOGICAL      :: ULP_Test
    REAL(Double) :: Rel
    
    ! Set up
    ! ------
    ULP_Test = .TRUE.
    IF ( PRESENT(ULP) ) THEN
      Rel = REAL(ABS(ULP), Double)
    ELSE
      Rel = DP_ONE
    END IF
    IF ( PRESENT(Percent) ) THEN
      ULP_Test = .FALSE.
      ! Test for zero x (elementals can't be recursive)
      IF ( ABS(x) < ( SPACING( MAX(ABS(x),DP_ZERO) ) ) ) ULP_Test = .TRUE.
    END IF
    
    ! Compare the numbers
    ! -------------------
    IF ( ULP_Test ) THEN
      Compare = ABS(x-y) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )
    ELSE
      Compare = DP_HUNDRED*ABS((x-y)/x) < Percent
    END IF
  END FUNCTION Compare_Real_Double


  ELEMENTAL FUNCTION Compare_Complex_Single( x, y, ULP, Percent ) RESULT( Compare )
    ! Arguments
    COMPLEX(Single),           INTENT(IN) :: x
    COMPLEX(Single),           INTENT(IN) :: y
    INTEGER        , OPTIONAL, INTENT(IN) :: ULP
    REAL(Single)   , OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    REAL(Single) :: xr, xi
    REAL(Single) :: yr, yi
    
    ! Separate real and complex parts
    ! -------------------------------
    xr=REAL(x,Single); xi=AIMAG(x)
    yr=REAL(y,Single); yi=AIMAG(y)
    
    ! Compare each part separately
    ! ----------------------------
    Compare = Compare_Real_Single(xr,yr,ULP=ULP,Percent=Percent) .AND. &
              Compare_Real_Single(xi,yi,ULP=ULP,Percent=Percent)
  END FUNCTION Compare_Complex_Single


  ELEMENTAL FUNCTION Compare_Complex_Double( x, y, ULP, Percent ) RESULT( Compare )
    ! Arguments
    COMPLEX(Double),           INTENT(IN) :: x
    COMPLEX(Double),           INTENT(IN) :: y
    INTEGER        , OPTIONAL, INTENT(IN) :: ULP
    REAL(Double)   , OPTIONAL, INTENT(IN) :: Percent
    ! Function result
    LOGICAL :: Compare
    ! Local variables
    REAL(Double) :: xr, xi
    REAL(Double) :: yr, yi
    
    ! Separate real and complex parts
    ! -------------------------------
    xr=REAL(x,Double); xi=AIMAG(x)
    yr=REAL(y,Double); yi=AIMAG(y)
    
    ! Compare each part separately
    ! ----------------------------
    Compare = Compare_Real_Double(xr,yr,ULP=ULP,Percent=Percent) .AND. &
              Compare_Real_Double(xi,yi,ULP=ULP,Percent=Percent)
  END FUNCTION Compare_Complex_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Tolerance
!
! PURPOSE:
!       Elemental function to compute a tolerance value for a given input for a
!       specified number of significant figures.
!
! CALLING SEQUENCE:
!       Result = Tolerance( x, n )
!
! INPUT ARGUMENTS:
!       x:           Floating point value for which a tolerance value is required.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                                  OR
!                                COMPLEX(Single)
!                                  OR
!                                COMPLEX(Double)
!                    DIMENSION:  Scalar or any rank array.
!                    ATTRIBUTES: INTENT(IN)
!
!       n:           The approximate number of significant figures for which the 
!                    tolerance is required.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar or same as input x.
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Result:      The return value is a tolerance value that can be used to
!                    compare two numbers.
!                    UNITS:      N/A
!                    TYPE:       Same as input x.
!                    DIMENSION:  Same as input x.
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Tolerance_Real_Single(x,n) RESULT( Tolerance )
    REAL(Single), INTENT(IN) :: x
    INTEGER     , INTENT(IN) :: n
    REAL(Single) :: Tolerance
    INTEGER :: e
    IF (ABS(x) > SP_ZERO) THEN
      e = FLOOR(LOG10(ABS(x))) - n
      Tolerance = SP_TEN**e
    ELSE
      Tolerance = SP_ONE
    END IF
  END FUNCTION Tolerance_Real_Single
  
  ELEMENTAL FUNCTION Tolerance_Real_Double(x,n) RESULT( Tolerance )
    REAL(Double), INTENT(IN) :: x
    INTEGER,      INTENT(IN) :: n
    REAL(Double) :: Tolerance
    INTEGER :: e
    IF (ABS(x) > DP_ZERO) THEN
      e = FLOOR(LOG10(ABS(x))) - n
      Tolerance = DP_TEN**e
    ELSE
      Tolerance = DP_ONE
    END IF
  END FUNCTION Tolerance_Real_Double
  
  ELEMENTAL FUNCTION Tolerance_Complex_Single(x,n) RESULT( Tolerance )
    COMPLEX(Single), INTENT(IN) :: x
    INTEGER,         INTENT(IN) :: n
    COMPLEX(Single) :: Tolerance
    REAL(Single) :: tr, ti
    tr = Tolerance_Real_Single(REAL(x),n)
    ti = Tolerance_Real_Single(AIMAG(x),n)
    Tolerance = CMPLX(tr,ti,Single)
  END FUNCTION Tolerance_Complex_Single
  
  ELEMENTAL FUNCTION Tolerance_Complex_Double(x,n) RESULT( Tolerance )
    COMPLEX(Double), INTENT(IN) :: x
    INTEGER,         INTENT(IN) :: n
    COMPLEX(Double) :: Tolerance
    REAL(Double) :: tr, ti
    tr = Tolerance_Real_Double(REAL(x),n)
    ti = Tolerance_Real_Double(AIMAG(x),n)
    Tolerance = CMPLX(tr,ti,Double)
  END FUNCTION Tolerance_Complex_Double


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Compares_Within_Tolerance
!
! PURPOSE:
!       Elemental function to determine if two values are comparable withing
!       a given tolerance determined by the number of significant figures
!       used in the comparison.
!
! CALLING SEQUENCE:
!       Result = Compare_Within_Tolerance( x, y, n, cutoff=cutoff )
!
! INPUTS:
!       x, y:        Floating point values to be compared.
!                    UNITS:      N/A
!                    TYPE:       REAL(Single)   [ == default real]
!                                  OR
!                                REAL(Double)
!                                  OR
!                                COMPLEX(Single)
!                                  OR
!                                COMPLEX(Double)
!                    DIMENSION:  Scalar or any rank array.
!                    ATTRIBUTES: INTENT(IN)
!
!       n:           The approximate number of significant figures for which the 
!                    tolerance is required.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar or same as input x, y.
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       cutoff:      Floating point value below which the comparison is not
!                    performed. In this case, the function result will be .TRUE.
!                    If not specified, the default value is 1.0e-15 for real
!                    input, or (1.0e-15,1.0e-15) for complex input.
!                    UNITS:      N/A
!                    TYPE:       Same as input x
!                    DIMENSION:  Scalar or same as input x, y.
!                    ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Result:      The return value is a logical value indicating if the 
!                    comparison was successful or not.
!                    If .TRUE. , the two numbers compare within the prescribed
!                                tolerance, or
!                       .FALSE., they do not.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input x, y.
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION cwt_Real_Single(x,y,n,cutoff) RESULT(is_comparable)
    REAL(Single),           INTENT(IN) :: x, y
    INTEGER,                INTENT(IN) :: n
    REAL(Single), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    REAL(Single) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = SP_COMPARE_CUTOFF
    END IF
    is_comparable = .TRUE.
    IF ( ABS(x) > c .OR. ABS(y) > c ) is_comparable = ABS(x-y) < Tolerance(x,n)
  END FUNCTION cwt_Real_Single


  ELEMENTAL FUNCTION cwt_Real_Double(x,y,n,cutoff) RESULT(is_comparable)
    REAL(Double),           INTENT(IN) :: x, y
    INTEGER,                INTENT(IN) :: n
    REAL(Double), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    REAL(Double) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = DP_COMPARE_CUTOFF
    END IF
    is_comparable = .TRUE.
    IF ( ABS(x) > c .OR. ABS(y) > c ) is_comparable = ABS(x-y) < Tolerance(x,n)
  END FUNCTION cwt_Real_Double


  ELEMENTAL FUNCTION cwt_Complex_Single(x,y,n,cutoff) RESULT(is_comparable)
    COMPLEX(Single),           INTENT(IN) :: x, y
    INTEGER,                   INTENT(IN) :: n
    COMPLEX(Single), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    COMPLEX(Single) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = CMPLX(SP_COMPARE_CUTOFF,SP_COMPARE_CUTOFF,Single)
    END IF
    is_comparable = cwt_Real_Single(REAL(x) ,REAL(y) ,n,cutoff=REAL(c) ) .AND. &
                    cwt_Real_Single(AIMAG(x),AIMAG(y),n,cutoff=AIMAG(c))
  END FUNCTION cwt_Complex_Single


  ELEMENTAL FUNCTION cwt_Complex_Double(x,y,n,cutoff) RESULT(is_comparable)
    COMPLEX(Double),           INTENT(IN) :: x, y
    INTEGER,                   INTENT(IN) :: n
    COMPLEX(Double), OPTIONAL, INTENT(IN) :: cutoff
    LOGICAL :: is_comparable
    COMPLEX(Double) :: c
    IF ( PRESENT(cutoff) ) THEN
      c = cutoff
    ELSE
      c = CMPLX(DP_COMPARE_CUTOFF,DP_COMPARE_CUTOFF,Double)
    END IF
    is_comparable = cwt_Real_Double(REAL(x) ,REAL(y) ,n,cutoff=REAL(c) ) .AND. &
                    cwt_Real_Double(AIMAG(x),AIMAG(y),n,cutoff=AIMAG(c))
  END FUNCTION cwt_Complex_Double

END MODULE Compare_Float_Numbers
