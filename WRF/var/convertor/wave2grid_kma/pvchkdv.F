CSHCO PROGRAM CPVFULL
      SUBROUTINE CPVFULL !SHCN
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER( IMAX=1,JMAX=1,KMAX=3 )
      DIMENSION GPV(IMAX,JMAX,KMAX),GPS(IMAX,JMAX),GPS9(IMAX,JMAX),
     .          A(KMAX),B(KMAX),GPS8(IMAX,JMAX),GPS1(IMAX,JMAX),
     .          GPV9(IMAX,JMAX,KMAX),GPV8(IMAX,JMAX,KMAX)
      DIMENSION GPVC9(IMAX,JMAX,KMAX)
C : eta-level coefficients
      A(1) = 0.01
      A(2) = 0.021
      A(3) = 0.03
      B(1) = 1.0
      B(2) = 0.95
      B(3) = 0.8
C : base state
      GPS9(1,1)= 1013.
C : input variable
      GPS(1,1) = GPS9(1,1)*0.01
C : original log P on full model levels
      JSTA=1
      JFIN=1
      CALL PVFULL
     I  (GPS9,IMAX,JMAX,KMAX,A,B,
     I   JSTA,JFIN,
     O   GPV9)
C     WRITE(6,*) ' GPV9=',(DEXP(GPV9(1,1,K)),K=1,KMAX)
      CALL PVFULLC
     I  (GPS9,IMAX,JMAX,KMAX,A,B,
     I   JSTA,JFIN,
     O   GPVC9)
C : tangent code
      CALL TPVFULL
     I  (GPS,IMAX,JMAX,KMAX,
     I   GPVC9,
     I   JSTA,JFIN,
     O   GPV)
C : input variable
C     DO 8000 N=1,10
      DO 8000 N=-4,8
        ALFA = 10.D0**(-DFLOAT(N)/2.D0)
        GPS8(1,1) = GPS9(1,1)+GPS(1,1)*ALFA
C : original code
      CALL PVFULL
     I  (GPS8,IMAX,JMAX,KMAX,A,B,
     I   JSTA,JFIN,
     O   GPV8)
C : incremental check
        DOT1 = 0.D0
        DOT2 = 0.D0
        DOT  = 0.D0
          DOT1 = DOT1 + (GPS8(1,1)-GPS9(1,1))**2
          DOT2 = DOT2 +  GPS(1,1)**2
          DOT  = DOT  + (GPS8(1,1)-GPS9(1,1))*GPS(1,1)
C         write(6,*) ' GPS DOT1,DOT2,DOT=',DOT1,DOT2,DOT
        DO 8100 K=1,KMAX
          DOT1V=        (GPV8(1,1,K)-GPV9(1,1,K))**2.D0
     . *1.D5**2
          DOT2V=         GPV(1,1,K)**2
     . *1.D5**2
          DOTV =        (GPV8(1,1,K)-GPV9(1,1,K))*GPV(1,1,K)
     . *1.D5**2
          DOT1 = DOT1 + DOT1V
          DOT2 = DOT2 + DOT2V
          DOT  = DOT  + DOTV
C         write(6,*) ' GPV DOT1,DOT2,DOT=',DOT1V,DOT2V,DOTV
 8100   CONTINUE
          WRITE(6,*) ' GSI DEV=',N,DOT/DSQRT(DOT1*DOT2),ALFA*GPS(1,1)
 8000 CONTINUE
C : left-hand side calculation (inner product of tagent code output)
      RLEFT = 0.
      RLEFT = RLEFT + GPS(1,1)*GPS(1,1)
      DO 3100 K=1,KMAX
      RLEFT = RLEFT + GPV(1,1,K)*GPV(1,1,K)
 3100 CONTINUE
      WRITE(6,*) ' RLEFT=',RLEFT
C : adjoint code
      GPS1(1,1) = GPS(1,1)
      CALL APVFULL
     I  (GPS1,IMAX,JMAX,KMAX,
     I   GPVC9,
     I   JSTA,JFIN,
     O   GPV)
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*) ' AGPV=',GPV
      WRITE(6,*) ' GPS9=',GPS9
      WRITE(6,*) ' AGPS=',GPS1
C : right-hand side calculation
      RIGHT= 0.
      RIGHT = RIGHT + GPS(1,1)*GPS1(1,1)
      WRITE(6,*) ' LEFT,RIGHT,DEV=',RLEFT,RIGHT,RLEFT-RIGHT
      STOP
      END
      SUBROUTINE PVFULL
     I  (GPS,IMAX,JMAX,KMAX,A,B,
     I   JSTA,JFIN,
     O   GPV)
C**********************************************************************
C full-level (L) pressure (PV) (HPA) calcualtion (log P)
C                                            2000.01.19 Y.TAKEUCHI
C<INPUT>
C GPS(IMAX,JMAX): surface pressure (hPa)
C<OUTPUT>
C GPV(IMAX,JMAX,KMAX): full-level log P (NOUNIT)
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GPS(IMAX,JMAX),GPV(IMAX,JMAX,KMAX)
      REAL*8    A(50), B(50)
C******************** PROCEDURE ***************************************
C : half-level (L+1/2,L-1/2) pressure (HPA) calculation
C : parallelization, modified for vectorization
C     IJFACT = 8
C     IJFACT = 1
CSHCO*POPTION PARALLEL
CSHCO*POPTION TLOCAL(I,J,K,PU,PD)
CSHCO*POPTION INDEP(GPV)
      DO 160 J=JSTA,JFIN
C     DO 160 J1= 1,JMAX/IJFACT
CSHCO*VOPTION INDEP
*vdir novector !SHCN
      DO 161 I=1,IMAX
C     DO 161 I1= 1,IMAX*IJFACT
C       I = MOD(I1-1,IMAX)+1
C       J = (J1-1)*IJFACT+(I1-1)/IMAX+1
C       write(6,*) ' I,J,I1,J1=',I,J,I1,J1
*vdir novector  !SHCN
      DO 165 K = 1,KMAX-1
C : half-level (L+1/2,L-1/2) pressure (HPA) calculation
          PU  = A(K+1) + B(K+1)*GPS(I,J)
          PD  = A(K  ) + B(K  )*GPS(I,J)
c         if (pd.eq.0.0.or.pu.eq.0.0) write(999,*) i,j,pd,pu  !SHCN
C : full-level (L) pressure (HPA) calculation
          GPV(I,J,K) = ( PD*DLOG(PD)-PU*DLOG(PU) )/(PD-PU) -1.D0
C       if(I.EQ.1.AND.J.EQ.1) write(6,*) ' GPV',K,PU,PD,GPV(I,J,K)
C         GPV(I,J,K) = GPV(I,J,K)*1000.D0
  165 CONTINUE
          GPV(I,J,KMAX) = DLOG((A(KMAX)+B(KMAX)*GPS(I,J))/2.D0)
C         GPV(I,J,KMAX) = GPV(I,J,KMAX)*1000.D0
C       if(I.EQ.1.AND.J.EQ.1) write(6,*) ' GPV',KMAX,PU,PD,GPV(I,J,KMAX)
  161 CONTINUE
  160 CONTINUE
      RETURN
      END
      SUBROUTINE TPVFULL
     I  (GPS,IMAX,JMAX,KMAX,
     I   GPVC9,
     I   JSTA,JFIN,
     O   GPV)
C**********************************************************************
C : PV: full-level (L) pressure (HPA) calculation  GPV DELETE
C : tangent code
C : log P
C                                            2000.01.19 Y.TAKEUCHI
C<INPUT>
C GPS(IMAX,JMAX): surface pressure increment (hPa)
C GPVC9(IMAX,JMAX,KMAX): coefficient
C<OUTPUT>
C GPV(IMAX,JMAX,KMAX): full-level log P increment (NOUNIT)
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GPS(IMAX,JMAX),GPVC9(IMAX,JMAX,KMAX)
      DIMENSION GPV(IMAX,JMAX,KMAX)
C******************** PROCEDURE ***************************************
C : parallelization, modified for vectorization
C     IJFACT = 8
C     IJFACT = 1
*POPTION PARALLEL
*POPTION TLOCAL(I,J,K)
*POPTION INDEP(GPVC9)
      DO 960 J= JSTA,JFIN
C     DO 960 J1= 1,JMAX/IJFACT
*VOPTION INDEP
      DO 961 I = 1,IMAX
C     DO 961 I1= 1,IMAX*IJFACT
C       I = MOD(I1-1,IMAX)+1
C       J = (J1-1)*IJFACT+(I1-1)/IMAX+1

C     DO 960 J = 1,JMAX
C     DO 961 I = 1,IMAX

          GPV(I,J,KMAX) = GPS(I,J) * GPVC9(I,J,KMAX)
  961 CONTINUE
      DO 965 K = 1,KMAX-1
      DO 966 I = 1,IMAX
          GPV(I,J,K) = GPS(I,J)*GPVC9(I,J,K)
  966 CONTINUE
  965 CONTINUE
  960 CONTINUE
      RETURN
      END
      SUBROUTINE APVFULL
     I  (GPS,IMAX,JMAX,KMAX,
     I   GPVC9,
     I   JSTA,JFIN,
     O   GPV)
C**********************************************************************
C : PV: full-level (L) pressure (HPA) calculation  GPV DELETE
C : verification of the adjoint code TDVAR.FORT(PVFULL)
C : log P
C                                            2000.01.19 Y.TAKEUCHI
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GPS(IMAX,JMAX),GPV(IMAX,JMAX,KMAX),
     .          GPVC9(IMAX,JMAX,KMAX)
C******************** PROCEDURE ***************************************
C2000.09.11
*POPTION PARALLEL,PRIND((J,1))
*POPTION TLOCAL(I,J,K)
      DO 960 J = JSTA,JFIN
      DO 961 I = 1,IMAX
          GPS(I,J) = GPS(I,J)+GPV(I,J,KMAX)*GPVC9(I,J,KMAX)
  961 CONTINUE
      DO 965 K = 1,KMAX-1
      DO 966 I = 1,IMAX
          GPS(I,J) = GPS(I,J) +GPV(I,J,K)*GPVC9(I,J,K) 
  966 CONTINUE
  965 CONTINUE
  960 CONTINUE
      RETURN
      END
      SUBROUTINE PVFULLC
     I  (GPS9,IMAX,JMAX,KMAX,A,B,
     I   JSTA,JFIN,
     O   GPVC9)
C**********************************************************************
C : coefficients for full-level pressure calculation
C : log P
C                                            2000.01.19 Y.TAKEUCHI
C<INPUT>
C GPS9(IMAX,JMAX): base state of surface pressure (hPa)
C<OUTPUT>
C GPVC9(IMAX,JMAX,KMAX): 
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GPS9(IMAX,JMAX),GPVC9(IMAX,JMAX,KMAX)
      REAL*8    A(50), B(50)
C******************** PROCEDURE ***************************************
C : half-level (L+1/2,L-1/2) pressure (HPA) calculation

C : parallelization, modified for vectorization
C     IJFACT = 8
C     IJFACT = 1
*POPTION PARALLEL
*POPTION TLOCAL(I,J,K,PU9,PD9)
*POPTION INDEP(GPVC9)
      DO 960 J=JSTA,JFIN
C     DO 960 J1= 1,JMAX/IJFACT
*VOPTION INDEP
      DO 961 I=1,IMAX
C     DO 961 I1= 1,IMAX*IJFACT
C       I = MOD(I1-1,IMAX)+1
C       J = (J1-1)*IJFACT+(I1-1)/IMAX+1

C     DO 960 J = 1,JMAX
C     DO 961 I = 1,IMAX

          GPVC9(I,J,KMAX) = 
     .       B(KMAX)/(A(KMAX)+B(KMAX)*GPS9(I,J))
      DO 965 K = 1,KMAX-1
          PU9 = A(K+1) + B(K+1)*GPS9(I,J)
          PD9 = A(K  ) + B(K  )*GPS9(I,J)

          GPVC9(I,J,K) = B(K  )*
     .     1.D0/(PD9-PU9)**2*(-PU9*(DLOG(PD9)-DLOG(PU9))+PD9-PU9)
     .                 + B(K+1)*
     .     1.D0/(PD9-PU9)**2*(PD9*(DLOG(PD9)-DLOG(PU9))-PD9+PU9)
  965 CONTINUE
  961 CONTINUE
  960 CONTINUE
      RETURN
      END
