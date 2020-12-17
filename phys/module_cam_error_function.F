#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov

module error_function

! This module provides generic interfaces for functions that evaluate
! erf(x), erfc(x), and exp(x*x)*erfc(x) in either single or double precision.

implicit none
private
save

! Public functions
public :: erf, erfc, erfcx

interface erf
   module procedure erf_r4
   module procedure derf
end interface

interface erfc
   module procedure erfc_r4
   module procedure derfc
end interface

interface erfcx
   module procedure erfcx_r4
   module procedure derfcx
end interface

! Private variables
integer, parameter :: r4 = selected_real_kind(6)  ! 4 byte real
integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real

contains

!------------------------------------------------------------------
!
! 6 December 2006 -- B. Eaton
! The following comments are from the original version of CALERF.
! The only changes in implementing this module are that the function
! names previously used for the single precision versions have been
! adopted for the new generic interfaces.  To support these interfaces
! there is now both a single precision version (calerf_r4) and a
! double precision version (calerf_r8) of CALERF below.  These versions
! are hardcoded to use IEEE arithmetic.
!
!------------------------------------------------------------------
!
! This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x)
!   for a real argument  x.  It contains three FUNCTION type
!   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX),
!   and one SUBROUTINE type subprogram, CALERF.  The calling
!   statements for the primary entries are:
!
!                   Y=ERF(X)     (or   Y=DERF(X)),
!
!                   Y=ERFC(X)    (or   Y=DERFC(X)),
!   and
!                   Y=ERFCX(X)   (or   Y=DERFCX(X)).
!
!   The routine  CALERF  is intended for internal packet use only,
!   all computations within the packet being concentrated in this
!   routine.  The function subprograms invoke  CALERF  with the
!   statement
!
!          CALL CALERF(ARG,RESULT,JINT)
!
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALERF
!       call              ARG                  Result          JINT
!
!     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0
!     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1
!     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2
!
!   The main computation evaluates near-minimax approximations
!   from "Rational Chebyshev approximations for the error function"
!   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
!   transportable program uses rational functions that theoretically
!   approximate  erf(x)  and  erfc(x)  to at least 18 significant
!   decimal digits.  The accuracy achieved depends on the arithmetic
!   system, the compiler, the intrinsic functions, and proper
!   selection of the machine-dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   XMIN   = the smallest positive floating-point number.
!   XINF   = the largest positive finite floating-point number.
!   XNEG   = the largest negative argument acceptable to ERFCX;
!            the negative of the solution to the equation
!            2*exp(x*x) = XINF.
!   XSMALL = argument below which erf(x) may be represented by
!            2*x/sqrt(pi)  and above which  x*x  will not underflow.
!            A conservative value is the largest machine number X
!            such that   1.0 + X = 1.0   to machine precision.
!   XBIG   = largest argument acceptable to ERFC;  solution to
!            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where
!            W(x) = exp(-x*x)/[x*sqrt(pi)].
!   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to
!            machine precision.  A conservative value is
!            1/[2*sqrt(XSMALL)]
!   XMAX   = largest acceptable argument to ERFCX; the minimum
!            of XINF and 1/[sqrt(pi)*XMIN].
!
!   Approximate values for some important machines are:
!
!                          XMIN       XINF        XNEG     XSMALL
!
!  CDC 7600      (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-15
!  CRAY-1        (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-15
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16
!  IBM 195       (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-17
!  UNIVAC 1108   (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-18
!  VAX D-Format  (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-17
!  VAX G-Format  (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-16
!
!
!                          XBIG       XHUGE       XMAX
!
!  CDC 7600      (S.P.)  25.922      8.39E+6     1.80X+293
!  CRAY-1        (S.P.)  75.326      8.39E+6     5.45E+2465
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)   9.194      2.90E+3     4.79E+37
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307
!  IBM 195       (D.P.)  13.306      1.90D+8     7.23E+75
!  UNIVAC 1108   (D.P.)  26.582      5.37D+8     8.98D+307
!  VAX D-Format  (D.P.)   9.269      1.90D+8     1.70D+38
!  VAX G-Format  (D.P.)  26.569      6.71D+7     8.98D+307
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns  ERFC = 0      for  ARG .GE. XBIG;
!
!                       ERFCX = XINF  for  ARG .LT. XNEG;
!      and
!                       ERFCX = 0     for  ARG .GE. XMAX.
!
!
! Intrinsic functions required are:
!
!     ABS, AINT, EXP
!
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: March 19, 1990
!
!------------------------------------------------------------------

SUBROUTINE CALERF_r8(ARG, RESULT, JINT)

   !------------------------------------------------------------------
   !  This version uses 8-byte reals
   !------------------------------------------------------------------
   integer, parameter :: rk = r8

   ! arguments
   real(rk), intent(in)  :: arg
   integer,  intent(in)  :: jint
   real(rk), intent(out) :: result

   ! local variables
   INTEGER :: I

   real(rk) :: X, Y, YSQ, XNUM, XDEN, DEL

   !------------------------------------------------------------------
   !  Mathematical constants
   !------------------------------------------------------------------
   real(rk), parameter :: ZERO   = 0.0E0_rk
   real(rk), parameter :: FOUR   = 4.0E0_rk
   real(rk), parameter :: ONE    = 1.0E0_rk
   real(rk), parameter :: HALF   = 0.5E0_rk
   real(rk), parameter :: TWO    = 2.0E0_rk
   real(rk), parameter :: SQRPI  = 5.6418958354775628695E-1_rk
   real(rk), parameter :: THRESH = 0.46875E0_rk
   real(rk), parameter :: SIXTEN = 16.0E0_rk

!------------------------------------------------------------------
!  Machine-dependent constants: IEEE single precision values
!------------------------------------------------------------------
!S      real, parameter :: XINF   =  3.40E+38
!S      real, parameter :: XNEG   = -9.382E0
!S      real, parameter :: XSMALL =  5.96E-8 
!S      real, parameter :: XBIG   =  9.194E0
!S      real, parameter :: XHUGE  =  2.90E3
!S      real, parameter :: XMAX   =  4.79E37

   !------------------------------------------------------------------
   !  Machine-dependent constants: IEEE double precision values
   !------------------------------------------------------------------
   real(rk), parameter :: XINF   =   1.79E308_r8
   real(rk), parameter :: XNEG   = -26.628E0_r8
   real(rk), parameter :: XSMALL =   1.11E-16_r8
   real(rk), parameter :: XBIG   =  26.543E0_r8
   real(rk), parameter :: XHUGE  =   6.71E7_r8
   real(rk), parameter :: XMAX   =   2.53E307_r8

   !------------------------------------------------------------------
   !  Coefficients for approximation to  erf  in first interval
   !------------------------------------------------------------------
   real(rk), parameter :: A(5) = (/ 3.16112374387056560E00_rk, 1.13864154151050156E02_rk, &
                                    3.77485237685302021E02_rk, 3.20937758913846947E03_rk, &
                                    1.85777706184603153E-1_rk /)
   real(rk), parameter :: B(4) = (/ 2.36012909523441209E01_rk, 2.44024637934444173E02_rk, &
                                    1.28261652607737228E03_rk, 2.84423683343917062E03_rk /)

   !------------------------------------------------------------------
   !  Coefficients for approximation to  erfc  in second interval
   !------------------------------------------------------------------
   real(rk), parameter :: C(9) = (/ 5.64188496988670089E-1_rk, 8.88314979438837594E00_rk, &
                                    6.61191906371416295E01_rk, 2.98635138197400131E02_rk, &
                                    8.81952221241769090E02_rk, 1.71204761263407058E03_rk, &
                                    2.05107837782607147E03_rk, 1.23033935479799725E03_rk, &
                                    2.15311535474403846E-8_rk /)
   real(rk), parameter :: D(8) = (/ 1.57449261107098347E01_rk, 1.17693950891312499E02_rk, &
                                    5.37181101862009858E02_rk, 1.62138957456669019E03_rk, &
                                    3.29079923573345963E03_rk, 4.36261909014324716E03_rk, &
                                    3.43936767414372164E03_rk, 1.23033935480374942E03_rk /)

   !------------------------------------------------------------------
   !  Coefficients for approximation to  erfc  in third interval
   !------------------------------------------------------------------
   real(rk), parameter :: P(6) = (/ 3.05326634961232344E-1_rk, 3.60344899949804439E-1_rk, &
                                    1.25781726111229246E-1_rk, 1.60837851487422766E-2_rk, &
                                    6.58749161529837803E-4_rk, 1.63153871373020978E-2_rk /)
   real(rk), parameter :: Q(5) = (/ 2.56852019228982242E00_rk, 1.87295284992346047E00_rk, &
                                    5.27905102951428412E-1_rk, 6.05183413124413191E-2_rk, &
                                    2.33520497626869185E-3_rk /)

   !------------------------------------------------------------------
   X = ARG
   Y = ABS(X)
   IF (Y .LE. THRESH) THEN
      !------------------------------------------------------------------
      !  Evaluate  erf  for  |X| <= 0.46875
      !------------------------------------------------------------------
      YSQ = ZERO
      IF (Y .GT. XSMALL) YSQ = Y * Y
      XNUM = A(5)*YSQ
      XDEN = YSQ
      DO I = 1, 3
         XNUM = (XNUM + A(I)) * YSQ
         XDEN = (XDEN + B(I)) * YSQ
      end do
      RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
      IF (JINT .NE. 0) RESULT = ONE - RESULT
      IF (JINT .EQ. 2) RESULT = EXP(YSQ) * RESULT
      GO TO 80
   ELSE IF (Y .LE. FOUR) THEN
      !------------------------------------------------------------------
      !  Evaluate  erfc  for 0.46875 <= |X| <= 4.0
      !------------------------------------------------------------------
      XNUM = C(9)*Y
      XDEN = Y
      DO I = 1, 7
         XNUM = (XNUM + C(I)) * Y
         XDEN = (XDEN + D(I)) * Y
      end do
      RESULT = (XNUM + C(8)) / (XDEN + D(8))
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   ELSE
      !------------------------------------------------------------------
      !  Evaluate  erfc  for |X| > 4.0
      !------------------------------------------------------------------
      RESULT = ZERO
      IF (Y .GE. XBIG) THEN
         IF ((JINT .NE. 2) .OR. (Y .GE. XMAX)) GO TO 30
         IF (Y .GE. XHUGE) THEN
            RESULT = SQRPI / Y
            GO TO 30
         END IF
      END IF
      YSQ = ONE / (Y * Y)
      XNUM = P(6)*YSQ
      XDEN = YSQ
      DO I = 1, 4
         XNUM = (XNUM + P(I)) * YSQ
         XDEN = (XDEN + Q(I)) * YSQ
      end do
      RESULT = YSQ *(XNUM + P(5)) / (XDEN + Q(5))
      RESULT = (SQRPI -  RESULT) / Y
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   END IF
30 continue
   !------------------------------------------------------------------
   !  Fix up for negative argument, erf, etc.
   !------------------------------------------------------------------
   IF (JINT .EQ. 0) THEN
      RESULT = (HALF - RESULT) + HALF
      IF (X .LT. ZERO) RESULT = -RESULT
   ELSE IF (JINT .EQ. 1) THEN
      IF (X .LT. ZERO) RESULT = TWO - RESULT
   ELSE
      IF (X .LT. ZERO) THEN
         IF (X .LT. XNEG) THEN
            RESULT = XINF
         ELSE
            YSQ = AINT(X*SIXTEN)/SIXTEN
            DEL = (X-YSQ)*(X+YSQ)
            Y = EXP(YSQ*YSQ) * EXP(DEL)
            RESULT = (Y+Y) - RESULT
         END IF
      END IF
   END IF
80 continue
end SUBROUTINE CALERF_r8

!------------------------------------------------------------------------------------------

SUBROUTINE CALERF_r4(ARG, RESULT, JINT)

   !------------------------------------------------------------------
   !  This version uses 4-byte reals
   !------------------------------------------------------------------
   integer, parameter :: rk = r4

   ! arguments
   real(rk), intent(in)  :: arg
   integer,  intent(in)  :: jint
   real(rk), intent(out) :: result

   ! local variables
   INTEGER :: I

   real(rk) :: X, Y, YSQ, XNUM, XDEN, DEL

   !------------------------------------------------------------------
   !  Mathematical constants
   !------------------------------------------------------------------
   real(rk), parameter :: ZERO   = 0.0E0_rk
   real(rk), parameter :: FOUR   = 4.0E0_rk
   real(rk), parameter :: ONE    = 1.0E0_rk
   real(rk), parameter :: HALF   = 0.5E0_rk
   real(rk), parameter :: TWO    = 2.0E0_rk
   real(rk), parameter :: SQRPI  = 5.6418958354775628695E-1_rk
   real(rk), parameter :: THRESH = 0.46875E0_rk
   real(rk), parameter :: SIXTEN = 16.0E0_rk

   !------------------------------------------------------------------
   !  Machine-dependent constants: IEEE single precision values
   !------------------------------------------------------------------
   real(rk), parameter :: XINF   =  3.40E+38_r4
   real(rk), parameter :: XNEG   = -9.382E0_r4
   real(rk), parameter :: XSMALL =  5.96E-8_r4 
   real(rk), parameter :: XBIG   =  9.194E0_r4
   real(rk), parameter :: XHUGE  =  2.90E3_r4
   real(rk), parameter :: XMAX   =  4.79E37_r4

   !------------------------------------------------------------------
   !  Coefficients for approximation to  erf  in first interval
   !------------------------------------------------------------------
   real(rk), parameter :: A(5) = (/ 3.16112374387056560E00_rk, 1.13864154151050156E02_rk, &
                                    3.77485237685302021E02_rk, 3.20937758913846947E03_rk, &
                                    1.85777706184603153E-1_rk /)
   real(rk), parameter :: B(4) = (/ 2.36012909523441209E01_rk, 2.44024637934444173E02_rk, &
                                    1.28261652607737228E03_rk, 2.84423683343917062E03_rk /)

   !------------------------------------------------------------------
   !  Coefficients for approximation to  erfc  in second interval
   !------------------------------------------------------------------
   real(rk), parameter :: C(9) = (/ 5.64188496988670089E-1_rk, 8.88314979438837594E00_rk, &
                                    6.61191906371416295E01_rk, 2.98635138197400131E02_rk, &
                                    8.81952221241769090E02_rk, 1.71204761263407058E03_rk, &
                                    2.05107837782607147E03_rk, 1.23033935479799725E03_rk, &
                                    2.15311535474403846E-8_rk /)
   real(rk), parameter :: D(8) = (/ 1.57449261107098347E01_rk, 1.17693950891312499E02_rk, &
                                    5.37181101862009858E02_rk, 1.62138957456669019E03_rk, &
                                    3.29079923573345963E03_rk, 4.36261909014324716E03_rk, &
                                    3.43936767414372164E03_rk, 1.23033935480374942E03_rk /)

   !------------------------------------------------------------------
   !  Coefficients for approximation to  erfc  in third interval
   !------------------------------------------------------------------
   real(rk), parameter :: P(6) = (/ 3.05326634961232344E-1_rk, 3.60344899949804439E-1_rk, &
                                    1.25781726111229246E-1_rk, 1.60837851487422766E-2_rk, &
                                    6.58749161529837803E-4_rk, 1.63153871373020978E-2_rk /)
   real(rk), parameter :: Q(5) = (/ 2.56852019228982242E00_rk, 1.87295284992346047E00_rk, &
                                    5.27905102951428412E-1_rk, 6.05183413124413191E-2_rk, &
                                    2.33520497626869185E-3_rk /)

   !------------------------------------------------------------------
   X = ARG
   Y = ABS(X)
   IF (Y .LE. THRESH) THEN
      !------------------------------------------------------------------
      !  Evaluate  erf  for  |X| <= 0.46875
      !------------------------------------------------------------------
      YSQ = ZERO
      IF (Y .GT. XSMALL) YSQ = Y * Y
      XNUM = A(5)*YSQ
      XDEN = YSQ
      DO I = 1, 3
         XNUM = (XNUM + A(I)) * YSQ
         XDEN = (XDEN + B(I)) * YSQ
      end do
      RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
      IF (JINT .NE. 0) RESULT = ONE - RESULT
      IF (JINT .EQ. 2) RESULT = EXP(YSQ) * RESULT
      GO TO 80
   ELSE IF (Y .LE. FOUR) THEN
      !------------------------------------------------------------------
      !  Evaluate  erfc  for 0.46875 <= |X| <= 4.0
      !------------------------------------------------------------------
      XNUM = C(9)*Y
      XDEN = Y
      DO I = 1, 7
         XNUM = (XNUM + C(I)) * Y
         XDEN = (XDEN + D(I)) * Y
      end do
      RESULT = (XNUM + C(8)) / (XDEN + D(8))
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   ELSE
      !------------------------------------------------------------------
      !  Evaluate  erfc  for |X| > 4.0
      !------------------------------------------------------------------
      RESULT = ZERO
      IF (Y .GE. XBIG) THEN
         IF ((JINT .NE. 2) .OR. (Y .GE. XMAX)) GO TO 30
         IF (Y .GE. XHUGE) THEN
            RESULT = SQRPI / Y
            GO TO 30
         END IF
      END IF
      YSQ = ONE / (Y * Y)
      XNUM = P(6)*YSQ
      XDEN = YSQ
      DO I = 1, 4
         XNUM = (XNUM + P(I)) * YSQ
         XDEN = (XDEN + Q(I)) * YSQ
      end do
      RESULT = YSQ *(XNUM + P(5)) / (XDEN + Q(5))
      RESULT = (SQRPI -  RESULT) / Y
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   END IF
30 continue
   !------------------------------------------------------------------
   !  Fix up for negative argument, erf, etc.
   !------------------------------------------------------------------
   IF (JINT .EQ. 0) THEN
      RESULT = (HALF - RESULT) + HALF
      IF (X .LT. ZERO) RESULT = -RESULT
   ELSE IF (JINT .EQ. 1) THEN
      IF (X .LT. ZERO) RESULT = TWO - RESULT
   ELSE
      IF (X .LT. ZERO) THEN
         IF (X .LT. XNEG) THEN
            RESULT = XINF
         ELSE
            YSQ = AINT(X*SIXTEN)/SIXTEN
            DEL = (X-YSQ)*(X+YSQ)
            Y = EXP(YSQ*YSQ) * EXP(DEL)
            RESULT = (Y+Y) - RESULT
         END IF
      END IF
   END IF
80 continue
end SUBROUTINE CALERF_r4

!------------------------------------------------------------------------------------------

FUNCTION DERF(X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for erf(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, January 8, 1985
!
!--------------------------------------------------------------------
   integer, parameter :: rk = r8 ! 8 byte real

   ! argument
   real(rk), intent(in) :: X

   ! return value
   real(rk) :: DERF

   ! local variables
   INTEGER :: JINT = 0
   !------------------------------------------------------------------

   CALL CALERF_r8(X, DERF, JINT)
END FUNCTION DERF

!------------------------------------------------------------------------------------------

FUNCTION ERF_r4(X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for erf(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, January 8, 1985
!
!--------------------------------------------------------------------
   integer, parameter :: rk = r4 ! 4 byte real

   ! argument
   real(rk), intent(in) :: X

   ! return value
   real(rk) :: ERF_r4

   ! local variables
   INTEGER :: JINT = 0
   !------------------------------------------------------------------

   CALL CALERF_r4(X, ERF_r4, JINT)
END FUNCTION ERF_r4

!------------------------------------------------------------------------------------------

FUNCTION DERFC(X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for erfc(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, January 8, 1985
!
!--------------------------------------------------------------------
   integer, parameter :: rk = r8 ! 8 byte real

   ! argument
   real(rk), intent(in) :: X

   ! return value
   real(rk) :: DERFC

   ! local variables
   INTEGER :: JINT = 1
   !------------------------------------------------------------------

   CALL CALERF_r8(X, DERFC, JINT)
END FUNCTION DERFC

!------------------------------------------------------------------------------------------

FUNCTION ERFC_r4(X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for erfc(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, January 8, 1985
!
!--------------------------------------------------------------------
   integer, parameter :: rk = r4 ! 4 byte real

   ! argument
   real(rk), intent(in) :: X

   ! return value
   real(rk) :: ERFC_r4

   ! local variables
   INTEGER :: JINT = 1
   !------------------------------------------------------------------

   CALL CALERF_r4(X, ERFC_r4, JINT)
END FUNCTION ERFC_r4

!------------------------------------------------------------------------------------------

FUNCTION DERFCX(X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for exp(x*x) * erfc(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, March 30, 1987
!
!--------------------------------------------------------------------
   integer, parameter :: rk = r8 ! 8 byte real

   ! argument
   real(rk), intent(in) :: X

   ! return value
   real(rk) :: DERFCX

   ! local variables
   INTEGER :: JINT = 2
   !------------------------------------------------------------------

   CALL CALERF_r8(X, DERFCX, JINT)
END FUNCTION DERFCX

!------------------------------------------------------------------------------------------

FUNCTION ERFCX_R4(X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for exp(x*x) * erfc(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, March 30, 1987
!
!--------------------------------------------------------------------
   integer, parameter :: rk = r4 ! 8 byte real

   ! argument
   real(rk), intent(in) :: X

   ! return value
   real(rk) :: ERFCX_R4

   ! local variables
   INTEGER :: JINT = 2
   !------------------------------------------------------------------

   CALL CALERF_r4(X, ERFCX_R4, JINT)
END FUNCTION ERFCX_R4

!------------------------------------------------------------------------------------------

end module error_function
