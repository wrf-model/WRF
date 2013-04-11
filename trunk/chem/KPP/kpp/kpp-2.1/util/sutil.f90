
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE KppDecomp( JVS, IER )
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        Sparse LU factorization
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  USE KPP_ROOT_Parameters
  USE KPP_ROOT_JacobianSP

      INTEGER  :: IER
      KPP_REAL :: JVS(KPP_LU_NONZERO), W(KPP_NVAR), a
      INTEGER  :: k, kk, j, jj

      a = 0. ! mz_rs_20050606
      IER = 0
      DO k=1,NVAR
        ! mz_rs_20050606: don't check if real value == 0
        ! IF ( JVS( LU_DIAG(k) ) .EQ. 0. ) THEN
        IF ( ABS(JVS(LU_DIAG(k))) < TINY(a) ) THEN
            IER = k
            RETURN
        END IF
        DO kk = LU_CROW(k), LU_CROW(k+1)-1
              W( LU_ICOL(kk) ) = JVS(kk)
        END DO
        DO kk = LU_CROW(k), LU_DIAG(k)-1
            j = LU_ICOL(kk)
            a = -W(j) / JVS( LU_DIAG(j) )
            W(j) = -a
            DO jj = LU_DIAG(j)+1, LU_CROW(j+1)-1
               W( LU_ICOL(jj) ) = W( LU_ICOL(jj) ) + a*JVS(jj)
            END DO
         END DO
         DO kk = LU_CROW(k), LU_CROW(k+1)-1
            JVS(kk) = W( LU_ICOL(kk) )
         END DO
      END DO
      
END SUBROUTINE KppDecomp


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE KppDecompCmplx( JVS, IER )
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        Sparse LU factorization, complex
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  USE KPP_ROOT_Parameters
  USE KPP_ROOT_JacobianSP

      INTEGER  :: IER
      DOUBLE COMPLEX :: JVS(KPP_LU_NONZERO), W(KPP_NVAR), a
      INTEGER  :: k, kk, j, jj

      IER = 0
      DO k=1,NVAR
        IF ( JVS( LU_DIAG(k) ) .EQ. 0. ) THEN
            IER = k
            RETURN
        END IF
        DO kk = LU_CROW(k), LU_CROW(k+1)-1
              W( LU_ICOL(kk) ) = JVS(kk)
        END DO
        DO kk = LU_CROW(k), LU_DIAG(k)-1
            j = LU_ICOL(kk)
            a = -W(j) / JVS( LU_DIAG(j) )
            W(j) = -a
            DO jj = LU_DIAG(j)+1, LU_CROW(j+1)-1
               W( LU_ICOL(jj) ) = W( LU_ICOL(jj) ) + a*JVS(jj)
            END DO
         END DO
         DO kk = LU_CROW(k), LU_CROW(k+1)-1
            JVS(kk) = W( LU_ICOL(kk) )
         END DO
      END DO
      
END SUBROUTINE KppDecompCmplx

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE KppSolveIndirect( JVS, X )
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        Sparse solve subroutine using indirect addressing
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  USE KPP_ROOT_Parameters
  USE KPP_ROOT_JacobianSP

      INTEGER i, j
      KPP_REAL JVS(KPP_LU_NONZERO), X(KPP_NVAR), sum

      DO i=1,NVAR
         DO j = LU_CROW(i), LU_DIAG(i)-1 
             X(i) = X(i) - JVS(j)*X(LU_ICOL(j));
         END DO  
      END DO

      DO i=NVAR,1,-1
        sum = X(i);
        DO j = LU_DIAG(i)+1, LU_CROW(i+1)-1
          sum = sum - JVS(j)*X(LU_ICOL(j));
        END DO
        X(i) = sum/JVS(LU_DIAG(i));
      END DO
      
END SUBROUTINE KppSolveIndirect

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE KppSolveCmplx( JVS, X )
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        Complex sparse solve subroutine using indirect addressing
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  USE KPP_ROOT_Parameters
  USE KPP_ROOT_JacobianSP

      INTEGER i, j
      DOUBLE COMPLEX JVS(KPP_LU_NONZERO), X(KPP_NVAR), sum

      DO i=1,NVAR
         DO j = LU_CROW(i), LU_DIAG(i)-1 
             X(i) = X(i) - JVS(j)*X(LU_ICOL(j));
         END DO  
      END DO

      DO i=NVAR,1,-1
        sum = X(i);
        DO j = LU_DIAG(i)+1, LU_CROW(i+1)-1
          sum = sum - JVS(j)*X(LU_ICOL(j));
        END DO
        X(i) = sum/JVS(LU_DIAG(i));
      END DO
      
END SUBROUTINE KppSolveCmplx
