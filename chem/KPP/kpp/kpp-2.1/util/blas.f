!--------------------------------------------------------------
!
! BLAS/LAPACK-like subroutines used by the integration algorithms
! It is recommended to replace them by calls to the optimized
!      BLAS/LAPACK library for your machine
!
!  (C) Adrian Sandu, Aug. 2004
!      Virginia Polytechnic Institute and State University
!--------------------------------------------------------------


!--------------------------------------------------------------
      SUBROUTINE  WCOPY(N,X,incX,Y,incY)
!--------------------------------------------------------------
!     copies a vector, x, to a vector, y:  y <- x
!     only for incX=incY=1
!     after BLAS
!     replace this by the function from the optimized BLAS implementation:
!         CALL  SCOPY(N,X,1,Y,1)   or   CALL  DCOPY(N,X,1,Y,1)
!--------------------------------------------------------------
      
      INTEGER i,incX,incY,M,MP1,N
      KPP_REAL X(N),Y(N)

      IF (N.LE.0) RETURN

      M = MOD(N,8)
      IF( M .NE. 0 ) THEN
        DO i = 1,M
          Y(i) = X(i)
        END DO
        IF( N .LT. 8 ) RETURN
      END IF	
      MP1 = M+1
      DO i = MP1,N,8
        Y(i) = X(i)
        Y(i + 1) = X(i + 1)
        Y(i + 2) = X(i + 2)
        Y(i + 3) = X(i + 3)
        Y(i + 4) = X(i + 4)
        Y(i + 5) = X(i + 5)
        Y(i + 6) = X(i + 6)
        Y(i + 7) = X(i + 7)
      END DO
      RETURN
      END


!--------------------------------------------------------------
      SUBROUTINE WAXPY(N,Alpha,X,incX,Y,incY)
!--------------------------------------------------------------
!     constant times a vector plus a vector: y <- y + Alpha*x
!     only for incX=incY=1
!     after BLAS
!     replace this by the function from the optimized BLAS implementation:
!         CALL SAXPY(N,Alpha,X,1,Y,1) or  CALL DAXPY(N,Alpha,X,1,Y,1)
!--------------------------------------------------------------

      INTEGER i,incX,incY,M,MP1,N
      KPP_REAL X(N),Y(N),Alpha
      KPP_REAL ZERO
      PARAMETER( ZERO = 0.0d0 )

      IF (Alpha .EQ. ZERO) RETURN
      IF (N .LE. 0) RETURN

      M = MOD(N,4)
      IF( M .NE. 0 ) THEN
        DO i = 1,M
          Y(i) = Y(i) + Alpha*X(i)
        END DO
        IF( N .LT. 4 ) RETURN
      END IF
      MP1 = M + 1
      DO i = MP1,N,4
        Y(i) = Y(i) + Alpha*X(i)
        Y(i + 1) = Y(i + 1) + Alpha*X(i + 1)
        Y(i + 2) = Y(i + 2) + Alpha*X(i + 2)
        Y(i + 3) = Y(i + 3) + Alpha*X(i + 3)
      END DO
      RETURN
      END



!--------------------------------------------------------------
      SUBROUTINE WSCAL(N,Alpha,X,incX)
!--------------------------------------------------------------
!     constant times a vector: x(1:N) <- Alpha*x(1:N) 
!     only for incX=incY=1
!     after BLAS
!     replace this by the function from the optimized BLAS implementation:
!         CALL SSCAL(N,Alpha,X,1) or  CALL DSCAL(N,Alpha,X,1)
!--------------------------------------------------------------

      INTEGER i,incX,M,MP1,N
      KPP_REAL X(N),Alpha
      KPP_REAL ZERO, ONE
      PARAMETER( ZERO = 0.0d0 )
      PARAMETER( ONE  = 1.0d0 )

      IF (Alpha .EQ. ONE) RETURN
      IF (N .LE. 0) RETURN

      M = MOD(N,5)
      IF( M .NE. 0 ) THEN
        IF (Alpha .EQ. (-ONE)) THEN
          DO i = 1,M
            X(i) = -X(i)
          END DO
        ELSEIF (Alpha .EQ. ZERO) THEN
          DO i = 1,M
            X(i) = ZERO
          END DO
        ELSE
          DO i = 1,M
            X(i) = Alpha*X(i)
          END DO
        END IF
        IF( N .LT. 5 ) RETURN
      END IF
      MP1 = M + 1
      IF (Alpha .EQ. (-ONE)) THEN
        DO i = MP1,N,5
          X(i)     = -X(i)
          X(i + 1) = -X(i + 1)
          X(i + 2) = -X(i + 2)
          X(i + 3) = -X(i + 3)
          X(i + 4) = -X(i + 4)
        END DO
      ELSEIF (Alpha .EQ. ZERO) THEN
        DO i = MP1,N,5
          X(i)     = ZERO
          X(i + 1) = ZERO
          X(i + 2) = ZERO
          X(i + 3) = ZERO
          X(i + 4) = ZERO
        END DO
      ELSE
        DO i = MP1,N,5
          X(i)     = Alpha*X(i)
          X(i + 1) = Alpha*X(i + 1)
          X(i + 2) = Alpha*X(i + 2)
          X(i + 3) = Alpha*X(i + 3)
          X(i + 4) = Alpha*X(i + 4)
        END DO
      END IF
      RETURN
      END

!--------------------------------------------------------------
      KPP_REAL FUNCTION WLAMCH( C )
!--------------------------------------------------------------
!     returns epsilon machine
!     after LAPACK
!     replace this by the function from the optimized LAPACK implementation:
!          CALL SLAMCH('E') or CALL DLAMCH('E')
!--------------------------------------------------------------

      CHARACTER C
      INTEGER   i
      KPP_REAL  ONE, HALF, Eps, Sum
      PARAMETER (ONE  = 1.0d0)
      PARAMETER (HALF = 0.5d0)
      LOGICAL   First
      SAVE     First, Eps
      DATA     First /.TRUE./
      
      IF (First) THEN
        First = .FALSE.
        Eps = HALF**(16)
        DO i = 17, 80
          Eps = Eps*HALF
	  CALL WLAMCH_ADD(ONE,Eps,Sum)
	  IF (Sum.LE.ONE) GOTO 10
        END DO
        PRINT*,'ERROR IN WLAMCH. EPS < ',Eps
        RETURN
10      Eps = Eps*2
        i = i-1      
      END IF

      WLAMCH = Eps

      RETURN
      END
     
      SUBROUTINE WLAMCH_ADD( A, B, Sum )
      KPP_REAL A, B, Sum
      Sum = A + B
      RETURN
      END
!--------------------------------------------------------------
