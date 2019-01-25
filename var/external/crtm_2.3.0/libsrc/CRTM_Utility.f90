!
! CRTM_Utility 
!
! Module containing CRTM numerical utility routines.
!
! NOTE: This utility is specifically for use with RTSolution codes.
!       Adapted from package of MOM 1991, ASYMTX adapted from DISORT.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    Quanhua.Liu@noaa.gov
!                       Yong Han,       Yong.Han@noaa.gov
!                       Paul van Delst, paul.vandelst@noaa.g
!                       08-Jun-2004
!

MODULE CRTM_UTILITY 


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: Display_Message, SUCCESS, FAILURE, WARNING
  USE CRTM_Parameters, ONLY: ZERO, ONE, TWO, &
                             POINT_25, POINT_5, &
                             PI
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: matinv
  PUBLIC :: DOUBLE_GAUSS_QUADRATURE
  PUBLIC :: Legendre
  PUBLIC :: Legendre_M
  PUBLIC :: ASYMTX,ASYMTX_TL,ASYMTX_AD
  PUBLIC :: ASYMTX2_AD


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Legendre 
    MODULE PROCEDURE Legendre_scalar
    MODULE PROCEDURE Legendre_rank1
  END INTERFACE 


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module 
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = & 
  '$Id: CRTM_Utility.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Numerical small threshold value in Eigensystem
  REAL(fp), PARAMETER :: EIGEN_THRESHOLD = 1.0e-20_fp


CONTAINS


  SUBROUTINE DOUBLE_GAUSS_QUADRATURE(NUM, ABSCISSAS, WEIGHTS)
    ! Generates the abscissas and weights for an even 2*NUM point
    ! Gauss-Legendre quadrature.  Only the NUM positive points are returned.
    IMPLICIT NONE
    INTEGER, INTENT(IN) ::  NUM
    REAL( fp), DIMENSION(:) ::   ABSCISSAS, WEIGHTS
    INTEGER  N, K, I, J, L
    REAL(fp) ::   X, XP, PL, PL1, PL2, DPL
    ! REAL(fp), PARAMETER :: TINY1=3.0E-14_fp
    REAL(fp) :: TINY1
    PARAMETER(TINY1=3.0D-14)
    N = NUM
    K = (N+1)/2
    DO J = 1, K
      X = COS(PI*(J-POINT_25)/(N+POINT_5))
      I = 0
    100  CONTINUE
      PL1 = 1
      PL = X
      DO L = 2, N
        PL2 = PL1
        PL1 = PL
        PL = ( (2*L-1)*X*PL1 - (L-1)*PL2 )/L
      END DO
      DPL = N*(X*PL-PL1)/(X*X-1)
      XP = X
      X = XP - PL/DPL
      I = I+1
      IF (ABS(X-XP).GT.TINY1 .AND. I.LT.10) GO TO 100
        ABSCISSAS(J) = (ONE-X)/TWO
        ABSCISSAS(NUM+1-J) = (ONE+X)/TWO
        WEIGHTS(NUM+1-J) = ONE/((ONE-X*X)*DPL*DPL)
        WEIGHTS(J)       = ONE/((ONE-X*X)*DPL*DPL)
    END DO
    RETURN
  END SUBROUTINE DOUBLE_GAUSS_QUADRATURE
!
!
  FUNCTION matinv(A, Error_Status)
    ! --------------------------------------------------------------------
    ! Compute the inversion of the matrix A
    ! Invert matrix by Gauss method
    ! --------------------------------------------------------------------
    IMPLICIT NONE
    REAL(fp), intent(in),dimension(:,:) :: a
    INTEGER, INTENT( OUT ) :: Error_Status

    INTEGER:: n
    REAL(fp), dimension(size(a,1),size(a,2)) :: b
    REAL(fp ), dimension(size(a,1),size(a,2)) :: matinv 
    REAL(fp), dimension(size(a,1)) :: temp 
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'matinv'
    ! - - - Local Variables - - -
    REAL(fp) :: c, d
    INTEGER :: i, j, k, m, imax(1), ipvt(size(a,1))
    ! - - - - - - - - - - - - - -
    Error_Status = SUCCESS     
    b = a
    n=size(a,1)
    matinv=a
    ipvt = (/ (i, i = 1, n) /)
    ! Go into loop- b, the inverse, is initialized equal to a above
    DO k = 1,n
    ! Find the largest value and position of that value
      imax = MAXLOC(ABS(b(k:n,k)))
      m = k-1+imax(1)
    ! sigular matrix check
      IF ( ABS(b(m,k)).LE.(1.E-40_fp) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, 'Singular matrix', Error_Status )                                          
        RETURN
      END IF    
    ! get the row beneath the current row if the current row will
    ! not compute
      IF ( m .ne. k ) THEN
        ipvt( (/m,k/) ) = ipvt( (/k,m/) )
        b((/m,k/),:) = b((/k,m/),:)
      END IF
    ! d is a coefficient - brings the pivot value to one and then is applied
    ! to the rest of the row
      d = 1/b(k,k)
      temp = b(:,k)
      DO j = 1, n
         c = b(k,j)*d
         b(:,j) = b(:,j)-temp*c
         b(k,j) = c
      END DO 
      b(:,k) = temp*(-d)
      b(k,k) = d
    END DO
    matinv(:,ipvt) = b
  END FUNCTION matinv
!
!
  SUBROUTINE Legendre_Scalar(MOA,ANG,PL)
    !  calculating Legendre polynomial using recurrence relationship
    IMPLICIT NONE
    INTEGER, INTENT(in) :: MOA
    REAL(fp), intent(in) :: ANG
    REAL(fp), intent(out), dimension(0:MOA):: PL
    INTEGER :: j
    PL(0)=ONE
    PL(1)=ANG
    IF ( MOA.GE.2 ) THEN
      DO J = 1, MOA -1
        PL(J+1)=REAL(2*J+1,fp)/REAL(J+1,fp)*ANG*PL(J) &
          -REAL(J,fp)/REAL(J+1,fp)*PL(J-1)
      END DO
    END IF
    RETURN
  END SUBROUTINE Legendre_Scalar
!
!
   SUBROUTINE Legendre_Rank1(MOA,NU,ANG,PL)
     !  calculating Legendre polynomial using recurrence relationship
     IMPLICIT NONE
     INTEGER, intent(in):: MOA,NU
     REAL(fp), INTENT(in), dimension(NU):: ANG
     REAL(fp), INTENT(out),dimension(0:MOA,NU):: PL
     REAL(fp) :: TE
     INTEGER :: i,j
     DO i = 1,NU
       TE=ANG(i)
       PL(0,i)=ONE
       PL(1,i)=TE
       IF(MOA.GE.2) THEN
         DO J = 1, MOA-1
           PL(J+1,i)=REAL(2*J+1, fp)/REAL(J+1, fp)*TE*PL(J,i) &
           -REAL(J, fp)/REAL(J+1, fp)*PL(J-1,i)
         END DO
       END IF
     END DO
     RETURN
   END SUBROUTINE Legendre_Rank1
!       
!
   SUBROUTINE Legendre_M(MF,N,U,PL)
     IMPLICIT NONE
     REAL(fp), INTENT(IN) :: U
     INTEGER, INTENT(IN) :: N,MF
     REAL(fp), DIMENSION(0:N) :: PL 
     REAL(fp) :: f
     INTEGER:: J
     IF ( MF.GT.N ) THEN
       PL = 0.0_fp
     ELSE
       IF ( MF .EQ. 0 ) THEN
         f = 1.0_fp
         PL(0) = 1.0_fp
       ELSE
         f=sqrt(gamma2(2*MF))/gamma2(MF)/(2**MF)
         PL(MF)=f*sqrt((1.0_fp-U*U)**MF)
       END IF
       IF ( N.GT.MF )  PL(MF+1)=U*sqrt(2*MF+1.0_fp)*PL(MF)
       IF( N.gt.(MF+1) ) THEN
         DO J=MF+1,N-1
           PL(J+1)=(FLOAT(2*J+1)*U*PL(J) &
           -sqrt(FLOAT(J*J-MF*MF))*PL(J-1))/sqrt(float((J+1)**2-MF*MF))
         END DO
       END IF
     END IF     
   END SUBROUTINE Legendre_M
!
!
   FUNCTION gamma2(N)
     ! Compute gamma function value
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: N
     INTEGER :: i
     REAL(fp) :: gamma2
       
     IF ( N.lt.0 ) THEN
       CALL Display_Message('gamma2','Invalid input', WARNING)
       gamma2 = 1.0_fp
     ELSE
       gamma2=1.0_fp
       IF( N.gt.0 ) THEN
         DO i = 1 , N
         gamma2=gamma2*float(i)
         END DO
        END IF
     END IF
   END FUNCTION gamma2
!
!
   SUBROUTINE  ASYMTX( AAD, M, IA, IEVEC, &
                            EVECD, EVALD, IER)

!    =======  D O U B L E    P R E C I S I O N    V E R S I O N  ======

!       Solves eigenfunction problem for real asymmetric matrix
!       for which it is known a priori that the eigenvalues are real.

!       This is an adaptation of a subroutine EIGRF in the IMSL
!       library to use real instead of complex arithmetic, accounting
!       for the known fact that the eigenvalues and eigenvectors in
!       the discrete ordinate solution are real.  Other changes include
!       putting all the called subroutines in-line, deleting the
!       performance index calculation, updating many DO-loops
!       to Fortran77, and in calculating the machine precision
!       TOL instead of specifying it in a data statement.

!       EIGRF is based primarily on EISPACK routines.  The matrix is
!       first balanced using the parlett-reinsch algorithm.  Then
!       the Martin-Wilkinson algorithm is applied.

!       References:
!          Dongarra, J. and C. Moler, EISPACK -- A Package for Solving
!             Matrix Eigenvalue Problems, in Cowell, ed., 1984:
!             Sources and Development of Mathematical Software,
!             Prentice-Hall, Englewood Cliffs, NJ
!         Parlett and Reinsch, 1969: Balancing a Matrix for Calculation
!             of Eigenvalues and Eigenvectors, Num. Math. 13, 293-304
!         Wilkinson, J., 1965: The Algebraic Eigenvalue Problem,
!             Clarendon Press, Oxford

!   I N P U T    V A R I A B L E S:

!        AAD  :  input asymmetric matrix, destroyed after solved
!        M    :  order of  A
!       IA    :  first dimension of  A
!    IEVEC    :  first dimension of  EVECD

!   O U T P U T    V A R I A B L E S:

!       EVECD :  (unnormalized) eigenvectors of  A
!                   ( column J corresponds to EVALD(J) )

!       EVALD :  (unordered) eigenvalues of  A ( dimension at least M )

!       IER   :  if .NE. 0, signals that EVALD(IER) failed to converge;
!                   in that case eigenvalues IER+1,IER+2,...,M  are
!                   correct but eigenvalues 1,...,IER are set to zero.
      IMPLICIT NONE
      LOGICAL            BAD_STATUS
      CHARACTER(len=200) :: MESSAGE

!   S C R A T C H   V A R I A B L E S:

!       WKD    :  WORK AREA ( DIMENSION AT LEAST 2*M )
!+---------------------------------------------------------------------+

!  include file for dimensioning


!!      INCLUDE '../includes/VLIDORT.PARS'

!  input/output arguments

      INTEGER :: M, IA, IEVEC, IER
      INTEGER, PARAMETER :: MAXSTRMSTKS = 30
      REAL( fp), PARAMETER :: C1=0.4375_fp,C2=0.5_fp,C3=0.75_fp,C4=0.95_fp,C5=16.0_fp,C6=256.0_fp
      REAL( fp), DIMENSION(:,:) :: AAD, EVECD
      REAL( fp), DIMENSION(:) :: EVALD
      REAL( fp) :: WKD(4*MAXSTRMSTKS), nor_factor
      

!  local variables (explicit declaration

      LOGICAL           NOCONV, NOTLAS
      INTEGER :: I, J, L, K, KKK, LLL,N, N1, N2, IN, LB, KA, II
!      DOUBLE PRECISION  TOL, DISCRI, SGN, RNORM, W, F, G, H, P, Q, R
      REAL(fp) :: TOL, DISCRI, SGN, RNORM, W, F, G, H, P, Q, R
      REAL(fp) :: REPL, COL, ROW, SCALE, T, X, Z, S, Y, UU, VV
!
      IER = 0
      BAD_STATUS = .FALSE.
      MESSAGE = ' '

!       Here change to bypass D1MACH:
      TOL = 1.0D-12
!        TOL = D1MACH(4)
      IF ( M.LT.1 .OR. IA.LT.M .OR. IEVEC.LT.M ) THEN
        MESSAGE = 'ASYMTX--bad input variable(s)'
        BAD_STATUS = .TRUE.
        RETURN
      ENDIF
!                           ** HANDLE 1X1 AND 2X2 SPECIAL CASES
      IF ( M.EQ.1 )  THEN
         EVALD(1) = AAD(1,1)
         EVECD(1,1) = 1.0D0
         RETURN
      ELSE IF ( M.EQ.2 )  THEN
         DISCRI = ( AAD(1,1) - AAD(2,2) )**2 + 4.0D0*AAD(1,2)*AAD(2,1)
         IF ( DISCRI.LT.ZERO ) THEN
           MESSAGE = 'ASYMTX--COMPLEX EVALS IN 2X2 CASE'
           BAD_STATUS = .TRUE.
           RETURN
         ENDIF
         SGN = ONE
         IF ( AAD(1,1).LT.AAD(2,2) )  SGN = - ONE
         EVALD(1) = 0.5D0*( AAD(1,1) + AAD(2,2) + SGN*SQRT(DISCRI) )
         EVALD(2) = 0.5D0*( AAD(1,1) + AAD(2,2) - SGN*SQRT(DISCRI) )
         EVECD(1,1) = ONE
         EVECD(2,2) = ONE
         IF ( AAD(1,1).EQ.AAD(2,2) .AND. &
               (AAD(2,1).EQ.ZERO.OR.AAD(1,2).EQ.ZERO) ) THEN
            RNORM = ABS(AAD(1,1))+ABS(AAD(1,2))+ &
                      ABS(AAD(2,1))+ABS(AAD(2,2))
            W = TOL * RNORM
            EVECD(2,1) = AAD(2,1) / W
            EVECD(1,2) = - AAD(1,2) / W
         ELSE
            EVECD(2,1) = AAD(2,1) / ( EVALD(1) - AAD(2,2) )
            EVECD(1,2) = AAD(1,2) / ( EVALD(2) - AAD(1,1) )
         ENDIF
         RETURN
      END IF
!                                        ** INITIALIZE OUTPUT VARIABLES
      DO 20 I = 1, M
         EVALD(I) = ZERO
         DO 10 J = 1, M
            EVECD(I,J) = ZERO
10       CONTINUE
         EVECD(I,I) = ONE
20    CONTINUE
!                  ** BALANCE THE INPUT MATRIX AND REDUCE ITS NORM BY
!                  ** DIAGONAL SIMILARITY TRANSFORMATION STORED IN WK;
!                  ** THEN SEARCH FOR ROWS ISOLATING AN EIGENVALUE
!                  ** AND PUSH THEM DOWN
      RNORM = ZERO
      L  = 1
      K  = M

30    KKK = K
         DO 70  J = KKK, 1, -1
            ROW = ZERO
            DO 40 I = 1, K
               IF ( I.NE.J ) ROW = ROW + ABS( AAD(J,I) )
40          CONTINUE
            IF ( ROW.EQ.ZERO ) THEN
               WKD(K) = J
               IF ( J.NE.K ) THEN
                  DO 50 I = 1, K
                     REPL   = AAD(I,J)
                     AAD(I,J) = AAD(I,K)
                     AAD(I,K) = REPL
50                CONTINUE
                  DO 60 I = L, M
                     REPL   = AAD(J,I)
                     AAD(J,I) = AAD(K,I)
                     AAD(K,I) = REPL
60                CONTINUE
               END IF
               K = K - 1
               GO TO 30
            END IF
70       CONTINUE
!                                     ** SEARCH FOR COLUMNS ISOLATING AN
!                                       ** EIGENVALUE AND PUSH THEM LEFT
80    LLL = L
         DO 120 J = LLL, K
            COL = ZERO
            DO 90 I = L, K
               IF ( I.NE.J ) COL = COL + ABS( AAD(I,J) )
90          CONTINUE
            IF ( COL.EQ.ZERO ) THEN
               WKD(L) = J
               IF ( J.NE.L ) THEN
                  DO 100 I = 1, K
                     REPL   = AAD(I,J)
                     AAD(I,J) = AAD(I,L)
                     AAD(I,L) = REPL
100               CONTINUE
                  DO 110 I = L, M
                     REPL   = AAD(J,I)
                     AAD(J,I) = AAD(L,I)
                     AAD(L,I) = REPL
110               CONTINUE
               END IF
               L = L + 1
               GO TO 80
            END IF
120      CONTINUE
!                           ** BALANCE THE SUBMATRIX IN ROWS L THROUGH K
      DO 130 I = L, K
         WKD(I) = ONE
130   CONTINUE

140   NOCONV = .FALSE.
         DO 200 I = L, K
            COL = ZERO
            ROW = ZERO
            DO 150 J = L, K
               IF ( J.NE.I ) THEN
                  COL = COL + ABS( AAD(J,I) )
                  ROW = ROW + ABS( AAD(I,J) )
               END IF
150         CONTINUE
            F = ONE
            G = ROW / C5
            H = COL + ROW
160         IF ( COL.LT.G ) THEN
               F   = F * C5
               COL = COL * C6
               GO TO 160
            END IF
            G = ROW * C5
170         IF ( COL.GE.G ) THEN
               F   = F / C5
               COL = COL / C6
               GO TO 170
            END IF
!                                                         ** NOW BALANCE
            IF ( (COL+ROW)/F .LT. C4*H ) THEN
               WKD(I)  = WKD(I) * F
               NOCONV = .TRUE.
               DO 180 J = L, M
                  AAD(I,J) = AAD(I,J) / F
180            CONTINUE
               DO 190 J = 1, K
                  AAD(J,I) = AAD(J,I) * F
190            CONTINUE
            END IF
200      CONTINUE

      IF ( NOCONV ) GO TO 140
!                                  ** IS -A- ALREADY IN HESSENBERG FORM?
      IF ( K-1 .LT. L+1 ) GO TO 350
!                                   ** TRANSFER -A- TO A HESSENBERG FORM
      DO 290 N = L+1, K-1
         H        = ZERO
         WKD(N+M) = ZERO
         SCALE    = ZERO
!                                                        ** SCALE COLUMN
         DO 210 I = N, K
            SCALE = SCALE + ABS(AAD(I,N-1))
210      CONTINUE
         IF ( SCALE.NE.ZERO ) THEN
            DO 220 I = K, N, -1
               WKD(I+M) = AAD(I,N-1) / SCALE
               H = H + WKD(I+M)**2
220         CONTINUE
            G = - SIGN( SQRT(H), WKD(N+M) )
            H = H - WKD(N+M) * G
            WKD(N+M) = WKD(N+M) - G
!                                                 ** FORM (I-(U*UT)/H)*A
            DO 250 J = N, M
               F = ZERO
               DO 230  I = K, N, -1
                  F = F + WKD(I+M) * AAD(I,J)
230            CONTINUE
               DO 240 I = N, K
                  AAD(I,J) = AAD(I,J) - WKD(I+M) * F / H
240            CONTINUE
250         CONTINUE
!                                    ** FORM (I-(U*UT)/H)*A*(I-(U*UT)/H)
            DO 280 I = 1, K
               F = ZERO
               DO 260  J = K, N, -1
                  F = F + WKD(J+M) * AAD(I,J)
260            CONTINUE
               DO 270 J = N, K
                  AAD(I,J) = AAD(I,J) - WKD(J+M) * F / H
270            CONTINUE
280         CONTINUE
            WKD(N+M)  = SCALE * WKD(N+M)
            AAD(N,N-1) = SCALE * G
         END IF
290   CONTINUE

      DO 340  N = K-2, L, -1
         N1 = N + 1
         N2 = N + 2
         F  = AAD(N1,N)
         IF ( F.NE.ZERO ) THEN
            F  = F * WKD(N1+M)
            DO 300 I = N2, K
               WKD(I+M) = AAD(I,N)
300         CONTINUE
            IF ( N1.LE.K ) THEN
               DO 330 J = 1, M
                  G = ZERO
                  DO 310 I = N1, K
                     G = G + WKD(I+M) * EVECD(I,J)
310               CONTINUE
                  G = G / F
                  DO 320 I = N1, K
                     EVECD(I,J) = EVECD(I,J) + G * WKD(I+M)
320               CONTINUE
330            CONTINUE
            END IF
         END IF
340   CONTINUE

350   CONTINUE
      N = 1
      DO 370 I = 1, M
         DO 360 J = N, M
            RNORM = RNORM + ABS(AAD(I,J))
360      CONTINUE
         N = I
         IF ( I.LT.L .OR. I.GT.K ) EVALD(I) = AAD(I,I)
370   CONTINUE
      N = K
      T = ZERO
!                                         ** SEARCH FOR NEXT EIGENVALUES
380   IF ( N.LT.L ) GO TO 530
      IN = 0
      N1 = N - 1
      N2 = N - 2
!                          ** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
390   CONTINUE
      DO 400 I = L, N
         LB = N+L - I
         IF ( LB.EQ.L ) GO TO 410
         S = ABS( AAD(LB-1,LB-1) ) + ABS( AAD(LB,LB) )
         IF ( S.EQ.ZERO ) S = RNORM
         IF ( ABS(AAD(LB,LB-1)) .LE. TOL*S ) GO TO 410
400   CONTINUE

410   X = AAD(N,N)
      IF ( LB.EQ.N ) THEN
!                                        ** ONE EIGENVALUE FOUND
         AAD(N,N)  = X + T
         EVALD(N) = AAD(N,N)
         N = N1
         GO TO 380
      END IF

      Y = AAD(N1,N1)
      W = AAD(N,N1) * AAD(N1,N)
      IF ( LB.EQ.N1 ) THEN
!                                        ** TWO EIGENVALUES FOUND
         P = (Y-X) * C2
         Q = P**2 + W
         Z = SQRT( ABS(Q) )
         AAD(N,N) = X + T
         X = AAD(N,N)
         AAD(N1,N1) = Y + T
!                                        ** REAL PAIR
         Z = P + SIGN(Z,P)
         EVALD(N1) = X + Z
         EVALD(N)  = EVALD(N1)
         IF ( Z.NE.ZERO ) EVALD(N) = X - W / Z
         X = AAD(N,N1)
!                                  ** EMPLOY SCALE FACTOR IN CASE
!                                  ** X AND Z ARE VERY SMALL
         R = SQRT( X*X + Z*Z )
         P = X / R
         Q = Z / R
!                                             ** ROW MODIFICATION
         DO 420 J = N1, M
            Z = AAD(N1,J)
            AAD(N1,J) = Q * Z + P * AAD(N,J)
            AAD(N,J)  = Q * AAD(N,J) - P * Z
420      CONTINUE
!                                             ** COLUMN MODIFICATION
         DO 430 I = 1, N
            Z = AAD(I,N1)
            AAD(I,N1) = Q * Z + P * AAD(I,N)
            AAD(I,N)  = Q * AAD(I,N) - P * Z
430      CONTINUE
!                                          ** ACCUMULATE TRANSFORMATIONS
         DO 440 I = L, K
            Z = EVECD(I,N1)
            EVECD(I,N1) = Q * Z + P * EVECD(I,N)
            EVECD(I,N)  = Q * EVECD(I,N) - P * Z
440      CONTINUE

         N = N2
         GO TO 380
      END IF

      IF ( IN.EQ.30 ) THEN
!                    ** NO CONVERGENCE AFTER 30 ITERATIONS; SET ERROR
!                    ** INDICATOR TO THE INDEX OF THE CURRENT EIGENVALUE
         IER = N
         GO TO 670
      END IF
!                                                          ** FORM SHIFT
      IF ( IN.EQ.10 .OR. IN.EQ.20 ) THEN
         T = T + X
         DO 450 I = L, N
            AAD(I,I) = AAD(I,I) - X
450      CONTINUE
         S = ABS(AAD(N,N1)) + ABS(AAD(N1,N2))
         X = C3 * S
         Y = X
         W = - C1 * S**2
      END IF

      IN = IN + 1
!                ** LOOK FOR TWO CONSECUTIVE SMALL SUB-DIAGONAL ELEMENTS

      DO 460 J = LB, N2
         I = N2+LB - J
         Z = AAD(I,I)
         R = X - Z
         S = Y - Z
         P = ( R * S - W ) / AAD(I+1,I) + AAD(I,I+1)
         Q = AAD(I+1,I+1) - Z - R - S
         R = AAD(I+2,I+1)
         S = ABS(P) + ABS(Q) + ABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF ( I.EQ.LB ) GO TO 470
         UU = ABS( AAD(I,I-1) ) * ( ABS(Q) + ABS(R) )
         VV = ABS(P)*(ABS(AAD(I-1,I-1))+ABS(Z)+ABS(AAD(I+1,I+1)))
         IF ( UU .LE. TOL*VV ) GO TO 470
460   CONTINUE

470   CONTINUE
      AAD(I+2,I) = ZERO
      DO 480 J = I+3, N
         AAD(J,J-2) = ZERO
         AAD(J,J-3) = ZERO
480   CONTINUE

!             ** DOUBLE QR STEP INVOLVING ROWS K TO N AND COLUMNS M TO N

      DO 520 KA = I, N1
         NOTLAS = KA.NE.N1
         IF ( KA.EQ.I ) THEN
            S = SIGN( SQRT( P*P + Q*Q + R*R ), P )
            IF ( LB.NE.I ) AAD(KA,KA-1) = - AAD(KA,KA-1)
         ELSE
            P = AAD(KA,KA-1)
            Q = AAD(KA+1,KA-1)
            R = ZERO
            IF ( NOTLAS ) R = AAD(KA+2,KA-1)
            X = ABS(P) + ABS(Q) + ABS(R)
            IF ( X.EQ.ZERO ) GO TO 520
            P = P / X
            Q = Q / X
            R = R / X
            S = SIGN( SQRT( P*P + Q*Q + R*R ), P )
            AAD(KA,KA-1) = - S * X
         END IF
         P = P + S
         X = P / S
         Y = Q / S
         Z = R / S
         Q = Q / P
         R = R / P
!                                                    ** ROW MODIFICATION
         DO 490 J = KA, M
            P = AAD(KA,J) + Q * AAD(KA+1,J)
            IF ( NOTLAS ) THEN
               P = P + R * AAD(KA+2,J)
               AAD(KA+2,J) = AAD(KA+2,J) - P * Z
            END IF
            AAD(KA+1,J) = AAD(KA+1,J) - P * Y
            AAD(KA,J)   = AAD(KA,J)   - P * X
490      CONTINUE
!                                                 ** COLUMN MODIFICATION
         DO 500 II = 1, MIN0(N,KA+3)
            P = X * AAD(II,KA) + Y * AAD(II,KA+1)
            IF ( NOTLAS ) THEN
               P = P + Z * AAD(II,KA+2)
               AAD(II,KA+2) = AAD(II,KA+2) - P * R
            END IF
            AAD(II,KA+1) = AAD(II,KA+1) - P * Q
            AAD(II,KA)   = AAD(II,KA) - P
500      CONTINUE
!                                          ** ACCUMULATE TRANSFORMATIONS
         DO 510 II = L, K
            P = X * EVECD(II,KA) + Y * EVECD(II,KA+1)
            IF ( NOTLAS ) THEN
               P = P + Z * EVECD(II,KA+2)
               EVECD(II,KA+2) = EVECD(II,KA+2) - P * R
            END IF
            EVECD(II,KA+1) = EVECD(II,KA+1) - P * Q
            EVECD(II,KA)   = EVECD(II,KA) - P
510      CONTINUE

520   CONTINUE
      GO TO 390
!                     ** ALL EVALS FOUND, NOW BACKSUBSTITUTE REAL VECTOR
530   CONTINUE
      IF ( RNORM.NE.ZERO ) THEN
         DO 560  N = M, 1, -1
            N2 = N
            AAD(N,N) = ONE
            DO 550  I = N-1, 1, -1
               W = AAD(I,I) - EVALD(N)
               IF ( W.EQ.ZERO ) W = TOL * RNORM
               R = AAD(I,N)
               DO 540 J = N2, N-1
                  R = R + AAD(I,J) * AAD(J,N)
540            CONTINUE
               AAD(I,N) = - R / W
               N2 = I
550         CONTINUE
560      CONTINUE
!                      ** END BACKSUBSTITUTION VECTORS OF ISOLATED EVALS

         DO 580 I = 1, M
            IF ( I.LT.L .OR. I.GT.K ) THEN
               DO 570 J = I, M
                  EVECD(I,J) = AAD(I,J)
570            CONTINUE
            END IF
580      CONTINUE
!                                   ** MULTIPLY BY TRANSFORMATION MATRIX
         IF ( K.NE.0 ) THEN
            DO J = M, L, -1
               DO I = L, K
                  Z = ZERO
                  DO 590 N = L, MIN0(J,K)
                     Z = Z + EVECD(I,N) * AAD(N,J)
590               CONTINUE
                  EVECD(I,J) = Z
               END DO
            END DO
         END IF

      END IF

      DO I = L, K
         DO J = 1, M
            EVECD(I,J) = EVECD(I,J) * WKD(I)
         END DO
      END DO
!                           ** INTERCHANGE ROWS IF PERMUTATIONS OCCURRED
      DO 640  I = L-1, 1, -1
         J = INT(WKD(I))
         IF ( I.NE.J ) THEN
            DO 630 N = 1, M
               REPL       = EVECD(I,N)
               EVECD(I,N) = EVECD(J,N)
               EVECD(J,N) = REPL
630         CONTINUE
         END IF
640   CONTINUE

      DO 660 I = K+1, M
         J = INT(WKD(I))
         IF ( I.NE.J ) THEN
            DO 650 N = 1, M
               REPL       = EVECD(I,N)
               EVECD(I,N) = EVECD(J,N)
               EVECD(J,N) = REPL
650         CONTINUE
         END IF
660   CONTINUE
!
  670 CONTINUE
!
!  normalizing eigenvector
      DO j = 1, M
        nor_factor = ZERO
        DO i = 1, M
        nor_factor = nor_factor + EVECD(I,J)**2
        END DO
        nor_factor = sqrt(nor_factor)
        EVECD(:,j) = EVECD(:,j)/nor_factor
      END DO
      RETURN
   END SUBROUTINE  ASYMTX
!
!
     SUBROUTINE ASYMTX_TL( nZ, V, VAL, A_TL, V_TL, VAL_TL, Error_Status)
       IMPLICIT NONE
       INTEGER :: nZ,Error_Status
       REAL(fp), DIMENSION(:,:) :: V, A_TL, V_TL
       REAL(fp), DIMENSION(:) :: VAL, VAL_TL
       REAL(fp), DIMENSION(nZ,nZ) :: V_int, A1_TL
       REAL(fp) :: b_TL
       INTEGER :: i,j
       CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ASYMTX_TL'
       CHARACTER(256) :: Message
     !    
       V_int = matinv( V(1:nZ,1:nZ), Error_Status )
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv( V(1:nZ,1:nZ), Error_Status ) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF              
       ! part 1
       !!  A1_TL = ZERO
       A1_TL = matmul( V_int, matmul(A_TL, V) )
       
       ! part 2
       !!  A1_TL = ZERO
       !!  V_TL = ZERO
       DO i = 1, nZ
         VAL_TL(i) = A1_TL(i,i)
         DO j = 1, nZ
          IF( i /= j ) THEN
            if( abs(VAL(j)-VAL(i)) > EIGEN_THRESHOLD ) then
              V_TL(i,j) = A1_TL(i,j)/(VAL(j)-VAL(i))
            else
              V_TL(i,j) = ZERO
            end if
          ELSE
            V_TL(i,j) = ZERO
          END IF
         END DO
       END DO
       
       ! part 3
       !!  A1_TL = ZERO
       A1_TL = matmul( V, V_TL) 
       !!  V_TL = ZERO   
       ! part 4
       DO i = 1, nZ  
         b_TL = ZERO
         DO j = 1, nZ
           b_TL = b_TL - V(j,i)*A1_TL(j,i)
         END DO
!
         DO j = 1, nZ
           V_TL(j,i) = A1_TL(j,i) + V(j,i)*b_TL
         END DO
       END DO

       RETURN
       END SUBROUTINE ASYMTX_TL
!
!
     SUBROUTINE ASYMTX_AD( nZ, V, VAL, V_AD, VAL_AD, A_AD, Error_Status)
       IMPLICIT NONE
       INTEGER :: nZ,Error_Status
       REAL(fp), DIMENSION(:,:) :: V, V_AD, A_AD
       REAL(fp), DIMENSION(:) :: VAL, VAL_AD
       INTEGER :: i,j,k
       REAL(fp), DIMENSION(nZ,nZ) :: V_int, A1_AD
       CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ASYMTX_AD'
       CHARACTER(256) :: Message
     !
       A1_AD = ZERO
       DO i = 1, nZ
         DO j = 1, nZ
         DO k = 1, nZ
          IF( k /= j ) THEN
            if( abs(VAL(j)-VAL(k)) > EIGEN_THRESHOLD ) &
             A1_AD(k,j) = A1_AD(k,j) + V(i,k)*V_AD(i,j)/(VAL(j)-VAL(k))
          END IF
         END DO
         END DO
         A1_AD(i,i) = A1_AD(i,i) + VAL_AD(i)
       END DO       
       V_int = matinv( V(1:nZ,1:nZ), Error_Status )
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv( V(1:nZ,1:nZ), Error_Status ) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF           
       
       A_AD = matmul( transpose(V_int), matmul(A1_AD, transpose(V) ) )     
       RETURN
     END SUBROUTINE ASYMTX_AD
!
!
     SUBROUTINE ASYMTX2_AD( COS_Angle,n_streams, nZ, V, VAL, V_AD, VAL_AD, A_AD, Error_Status)
       IMPLICIT NONE
       INTEGER :: nZ,Error_Status
       REAL(fp), DIMENSION(:,:) :: V, V_AD, A_AD
       REAL(fp), DIMENSION(:) :: VAL, VAL_AD
       INTEGER :: i,j,n_streams,n
       REAL(fp) :: COS_Angle,d0,d1
       REAL(fp), DIMENSION(nZ,nZ) :: V_int, A1_AD
       REAL(fp) :: b_AD
     !
       n = 0
       IF( nZ /= n_streams ) THEN
       ! additional stream for satellite viewing angle is used.
          d0 = ONE/COS_Angle
          d0 = d0 * d0
          d1 = 100000.0_fp
          DO i = 1, nZ
            if( abs( d0 - VAL(i) ) < d1 .and. abs(V(2,i)) < EIGEN_THRESHOLD ) then
              n = i
              d1 = abs( d0 - VAL(i) )
            end if
          END DO
       END IF 
       IF( n /= 0 ) VAL_AD(n) = ZERO 

     ! using normoalization condition
       b_AD = ZERO
       !! A1_AD = ZERO
       ! part 4
       DO i = nZ, 1, -1 
         DO j = nZ, 1, -1
           b_AD = b_AD + V(j,i)*V_AD(j,i)
           A1_AD(j,i) = V_AD(j,i)
           !! A1_AD(j,i) = A1_AD(j,i) + V_AD(j,i)
         END DO

         DO j = nZ, 1, -1
           A1_AD(j,i) = A1_AD(j,i) - V(j,i)*b_AD
         END DO
         b_AD = ZERO
       END DO

       ! part 3
       !! V_AD = ZERO
       !! V_AD = V_AD + matmul( transpose(V), A1_AD)        
       V_AD = matmul( transpose(V), A1_AD) 

       ! part 2
       A1_AD = ZERO
       DO i = nZ, 1, -1
         DO j = nZ, 1, -1
          IF( i /= j .and. j /= n ) THEN
            if( abs(VAL(j)-VAL(i)) > EIGEN_THRESHOLD ) then
              A1_AD(i,j) = V_AD(i,j)/(VAL(j)-VAL(i))
            else
              V_AD(i,j) = ZERO
            end if
          ELSE
            V_AD(i,j) = ZERO
          END IF
         END DO
         A1_AD(i,i) = A1_AD(i,i) + VAL_Ad(i)
       END DO

       V_int = matinv( V(1:nZ,1:nZ), Error_Status )
       ! part 1
       !! A_AD = ZERO
       A_AD = matmul( transpose(V_int), matmul(A1_AD, transpose(V) ) )

       RETURN
     END SUBROUTINE ASYMTX2_AD
!
!
 END MODULE CRTM_UTILITY 
!
