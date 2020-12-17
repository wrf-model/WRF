module da_rf_cv3

   !---------------------------------------------------------------------------
   ! Purpose: da_rf subroutines from NCEP, used for CV3
   !---------------------------------------------------------------------------

   use da_mat_cv3, only : zerm, mulmv, linmm

contains

      subroutine smoothx(nx,ny,p1,sli,&
        m,be,nta,swidth,table)
      DIMENSION &
       ii(0:NX,0:NY),sli(0:nx,0:ny)&
      ,P1(0:NX,0:NY)&
      ,BE(M),table(nta,m)&
      ,ALX(0:NX,0:NY,M)

      KMOD2=MOD(M,2)
      IF(KMOD2.EQ.1)THEN
       DO IY=0,NY
        ALX(0,IY,1)=1.
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       DO IY=0,NY
        ALX(0,IY,KR)=1.
        ALX(0,IY,KI)=0.
       ENDDO
      ENDDO
      ta=float(nta)/swidth
       wni2=ta
       DO IY=0,NY
      DO IX=0,NX
        ii(ix,iy)=nint(wni2*sli(IX,IY))
        ii(ix,iy)=max(1,min(ii(ix,iy),nta))
       ENDDO
      ENDDO
        do im=1,m
       DO IY=0,NY
      DO IX=0,NX
        alx(ix,iy,im)=table(ii(ix,iy),im)
        enddo
       ENDDO
      ENDDO
!      sum1=0.
!       DO IY=0,NY
!      DO IX=0,NX
!       sum1=sum1+p1(ix,iy)**2
!        enddo
!       ENDDO

      CALL RFX(P1,NX,NY,M,ALX,BE)
!      sumx=0. 
!       DO IY=0,NY
!      DO IX=0,NX
!       sumx=sumx+p1(ix,iy)**2
!        enddo
!       ENDDO
!      print *,'p1 =',sum1,sumx
       return
        end subroutine smoothx
      subroutine smoothy(nx,ny,p1,sli,&
        m,be,nta,swidth,table)
      DIMENSION &
       ii(0:NX,0:NY),sli(0:nx,0:ny)&
      ,P1(0:NX,0:NY)&
      ,BE(M),table(nta,m)&
      ,ALY(0:NX,0:NY,M)
      KMOD2=MOD(M,2)
      IF(KMOD2.EQ.1)THEN
       DO IX=0,NX
        ALY(IX,0,1)=1.
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       DO IX=0,NX
        ALY(IX,0,KR)=1.
        ALY(IX,0,KI)=0.
       ENDDO
      ENDDO
      ta=float(nta)/swidth
       wni2=ta
       DO IY=0,NY
      DO IX=0,NX
        ii(ix,iy)=nint(wni2*sli(IX,IY))
        ii(ix,iy)=max(1,min(ii(ix,iy),nta))
       ENDDO
      ENDDO
!    sum1=0.
        do im=1,m
       DO IY=0,NY
      DO IX=0,NX
        aly(ix,iy,im)=table(ii(ix,iy),im)
!       sum1=sum1+aly(ix,iy,im)**2
        enddo
       ENDDO
      ENDDO

!      sumx=0.
!       DO IY=0,NY
!      DO IX=0,NX
!       sumx=sumx+p1(ix,iy)**2
!        enddo
!       ENDDO
!      print *,'p1 =',sum1,sumx,nx,ny,m,be

      CALL RFY(P1,NX,NY,M,ALY,BE)
!      sumx=0.
!       DO IY=0,NY
!      DO IX=0,NX
!       sumx=sumx+p1(ix,iy)**2
!        enddo
!       ENDDO
!      print *,'p1 after smooth =',sumx

       return
        end subroutine smoothy
      SUBROUTINE RFX(P1,NX,NY,M,ALX,BE)
      DIMENSION P1(0:NX,0:NY),P2(0:NX,0:NY)&
      ,GAX1(M,0:NY),DEX1(M,0:NY),GAY1(0:NX,M),DEY1(0:NX,M)&
      ,GAX2(M,0:NY),DEX2(M,0:NY),GAY2(0:NX,M),DEY2(0:NX,M)&
      ,GAGAXY1(M,M),GADEXY1(M,M),DEGAXY1(M,M),DEDEXY1(M,M)&
      ,GAGAXY2(M,M),GADEXY2(M,M),DEGAXY2(M,M),DEDEXY2(M,M)&
      ,ALX(0:NX,0:NY,M),ALY(0:NX,0:NY,M),BE(M)&
      ,DSSX(0:NX,0:NY),DSSy(0:NX,0:NY),DUMMY(1)

      CALL RF0XY(GAX1,DEX1,GAY1,DEY1,GAGAXY1,DEGAXY1,GADEXY1,DEDEXY1&
      ,NX,NY,M)
      CALL RF0XY(GAX2,DEX2,GAY2,DEY2,GAGAXY2,DEGAXY2,GADEXY2,DEDEXY2&
      ,NX,NY,M)


      CALL RFHX(P1,  DUMMY,		   DUMMY&
      ,P2,GAX2,DEX2, DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY&
      , NX,NY,M,0,ALX,BE)
      do j=0,ny
       do i=0,nx
        p1(i,j)=p2(i,j)
       enddo
      enddo
      RETURN
      END SUBROUTINE RFX
      SUBROUTINE RFY(P1,NX,NY,M,ALY,BE)
      DIMENSION P1(0:NX,0:NY),P2(0:NX,0:NY)&
      ,GAX1(M,0:NY),DEX1(M,0:NY),GAY1(0:NX,M),DEY1(0:NX,M)&
      ,GAX2(M,0:NY),DEX2(M,0:NY),GAY2(0:NX,M),DEY2(0:NX,M)&
      ,GAGAXY1(M,M),GADEXY1(M,M),DEGAXY1(M,M),DEDEXY1(M,M)&
      ,GAGAXY2(M,M),GADEXY2(M,M),DEGAXY2(M,M),DEDEXY2(M,M)&
      ,ALX(0:NX,0:NY,M),ALY(0:NX,0:NY,M),BE(M)&
      ,DSSX(0:NX,0:NY),DSSy(0:NX,0:NY),DUMMY(1)

      CALL RF0XY(GAX1,DEX1,GAY1,DEY1,GAGAXY1,DEGAXY1,GADEXY1,DEDEXY1&
      ,NX,NY,M)
      CALL RF0XY(GAX2,DEX2,GAY2,DEY2,GAGAXY2,DEGAXY2,GADEXY2,DEDEXY2&
      ,NX,NY,M)


!   IX < 0	|	   |	 IX > NX
!  ---------------------------------------
!	GADEXY1 |   DEY1   |DEDEXY1	   <-- IY > NY
!	  GAX1	|    P1	   | DEX1
!	GAGAXY1 |   GAY1   |DEGAXY1	   <-- IY < 0
!  ---------------------------------------

!      sumx=0.
!       DO IY=0,NY
!      DO IX=0,NX
!       sumx=sumx+p1(ix,iy)**2
!        enddo
!       ENDDO
!      print *,'p1 after smooth =',sumx

      CALL RFHY(P1,  DEX1,                 GAX1&
      ,P2,GAY1,DEY1, DEX2,DEGAXY1,DEDEXY1, GAX2,GAGAXY1,GADEXY1&
      , NX,NY,0,M,ALY,BE)
      do j=0,ny
       do i=0,nx
        p1(i,j)=p2(i,j)
       enddo
      enddo

      RETURN
      END SUBROUTINE RFY

      SUBROUTINE RF0XY(GAX,DEX,GAY,DEY,GAGAXY,DEGAXY,GADEXY,DEDEXY&
      ,NX,NY,M)
      DIMENSION GAX(M,0:NY),DEX(M,0:NY),GAY(0:NX,M),DEY(0:NX,M)&
      ,GAGAXY(M,M),DEGAXY(M,M),GADEXY(M,M),DEDEXY(M,M)
      NXP=NX+1
      NYP=NY+1
      CALL ZERM(GAX,M,NYP,M)
      CALL ZERM(DEX,M,NYP,M)
      CALL ZERM(GAY,NXP,M,NXP)
      CALL ZERM(DEY,NXP,M,NXP)
      CALL ZERM(GAGAXY,M,M,M)
      CALL ZERM(DEGAXY,M,M,M)
      CALL ZERM(GADEXY,M,M,M)
      CALL ZERM(DEDEXY,M,M,M)
      RETURN
      END SUBROUTINE RF0XY


      SUBROUTINE RFIV(P2,P1,N,M,GAP,DEP,DSS,SAMP,TURN)
      PARAMETER(NN=12)
      DIMENSION P2(0:N),P1(0:N),GAP(M),DEP(M),DSS(0:N),TURN(M,M)&
      ,GAT(NN),DET(NN)
      DO I=1,M
       GAT(I)=GAP(I)*SAMP
       DET(I)=DEP(I)*SAMP
      ENDDO
      DO I=0,N
       P1(I)=P2(I)*DSS(I)*SAMP
      ENDDO
      CALL MULMV(TURN,GAT,DEP,M,M,M)
      CALL MULMV(TURN,DET,GAP,M,M,M)
      RETURN
      END SUBROUTINE RFIV

      SUBROUTINE ZROOTS(A,M,ROOTS,POLISH)
      PARAMETER (EPS=1.E-6,MAXM=101)
      COMPLEX A(*),ROOTS(M),AD(MAXM),X,B,C
      LOGICAL POLISH
      DO 11 J=1,M+1
        AD(J)=A(J)
11    CONTINUE
      DO 13 J=M,1,-1
        X=CMPLX(0.,0.)
        CALL LAGUER(AD,J,X,EPS,.FALSE.)
        IF(ABS(AIMAG(X)).LE.2.*EPS**2*ABS(REAL(X))) X=CMPLX(REAL(X),0.)
        ROOTS(J)=X
        B=AD(J+1)
        DO 12 JJ=J,1,-1
          C=AD(JJ)
          AD(JJ)=B
          B=X*B+C
12      CONTINUE
13    CONTINUE
      IF (POLISH) THEN
        DO 14 J=1,M
          CALL LAGUER(A,M,ROOTS(J),EPS,.TRUE.)
14      CONTINUE
      ENDIF
      DO 16 J=2,M
        X=ROOTS(J)
        DO 15 I=J-1,1,-1
          IF(REAL(ROOTS(I)).LE.REAL(X))GO TO 10
          ROOTS(I+1)=ROOTS(I)
15      CONTINUE
        I=0
10      ROOTS(I+1)=X
16    CONTINUE
      RETURN
      END SUBROUTINE ZROOTS

      SUBROUTINE LAGUER(A,M,X,EPS,POLISH)
      COMPLEX A(*),X,DX,X1,B,D,F,G,H,SQ,GP,GM,G2,ZERO
      LOGICAL POLISH
      PARAMETER (ZERO=(0.,0.),EPSS=6.E-8,MAXIT=100)
      DXOLD=CABS(X)
      DO 12 ITER=1,MAXIT
        B=A(M+1)
        ERR=CABS(B)
        D=ZERO
        F=ZERO
        ABX=CABS(X)
        DO 11 J=M,1,-1
          F=X*F+D
          D=X*D+B
          B=X*B+A(J)
          ERR=CABS(B)+ABX*ERR
11      CONTINUE
        ERR=EPSS*ERR
        IF(CABS(B).LE.ERR) THEN
          RETURN
        ELSE
          G=D/B
          G2=G*G
          H=G2-2.*F/B
          SQ=CSQRT((M-1)*(M*H-G2))
          GP=G+SQ
          GM=G-SQ
          IF(CABS(GP).LT.CABS(GM)) GP=GM
          DX=M/GP
        ENDIF
        X1=X-DX
        IF(X.EQ.X1)RETURN
        X=X1
        CDX=CABS(DX)
        DXOLD=CDX
        IF(.NOT.POLISH)THEN
          IF(CDX.LE.EPS*CABS(X))RETURN
        ENDIF
12    CONTINUE
      PAUSE 'too many iterations'
      RETURN
      END SUBROUTINE LAGUER

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE RFDPAR1
! Initialize the Beta and exponential Rate vectors characterizing the unit-
! width pseudo-Gaussian of degree M.
!
! <-- BE:   Beta vector of coupling coefficients between input and each
!	    exponential component. For complex exponentials (which occur in
!	    conjugate pairs) store REAL, then IMAGINARY, of one member of pair.
! <-- RATE: Vector of "decay rates" characteristic of the exponential
!	    components. For paired complex rates, same convention as for BE.
! --> M:    Degree of approximation to the Gaussian [FT of this pseudo-
!	    Gaussian is proportional to 1./(1+k**2+ ...+k**(2M)/M!)].
!------------------------------------------------------------------------------
      SUBROUTINE RFDPAR1(BE,RATE,M)
      IMPLICIT COMPLEX(C)
      PARAMETER(QCRIT=1.E-3)
      PARAMETER(NN=12)
      LOGICAL POLISH
      DIMENSION COF(0:NN),CROOT(NN),VAN(NN,NN),BE(m),RATE(m)
      DATA POLISH/.TRUE./
      polish=.true.
      KMOD2=MOD(M,2)
      COF(0)=1.
      DO I=1,M
       COF(I)=.5*COF(I-1)/I
      ENDDO
      CALL ZROOTS(COF,M,CROOT,POLISH)
      IF(KMOD2.EQ.1)THEN    ! Treat the single real root:
       R=-REAL(CROOT(1))
       Q=-AIMAG(CROOT(1))
       QA=ABS(Q)
       IF(QA.GT.QCRIT)STOP
       R=SQRT(R)
       RATE(1)=R
       VAN(1,1)=1.
       VAN(2,1)=r
       DO I=3,M
       VAN(I,1)=VAN(I-1,1)*R*R
       ENDDO
      ENDIF
      DO J2=2,M,2	  ! Loop over remaining independent complex roots
       JREAL=KMOD2+J2-1
       JIMAG=KMOD2+J2
       CA=-CROOT(J2)
       CB=CSQRT(CA)
       R=REAL(CB)
       Q=AIMAG(CB)
       IF(R.LT.0.)THEN
	CB=-CB
	R=-R
	Q=-Q
       ENDIF
       RATE(JREAL)=R
       RATE(JIMAG)=Q
       VAN(1,JREAL)=1.
       VAN(1,JIMAG)=0.
       DO I=2,M
	IPOW=I*2-3
	CC=CB**IPOW
	VAN(I,JREAL)=REAL(CC)
	VAN(I,JIMAG)=-AIMAG(CC)
       ENDDO
      ENDDO
      BE(1)=1.
      DO I=2,M
       BE(I)=0.
      ENDDO
      CALL LINMM(VAN,BE,M,1,NN,M)
      RETURN
      END SUBROUTINE RFDPAR1

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE RFDPAR2
! Initialize the "turning" matrix and the amplitude rescaling factor
! of the pseudo-Gaussian smoother of degree M
!
! --> BE:   Beta vector (computed in previous call to RFDPAR1).
! --> RATE: Rate vector of the (in general complex) exponential contributions.
! <-- TURN: Turning matrix for 2nd sweep of filter along a line.
! <-- SAMP: Factor by which the amplitude at second sweep is normalized.
! --> M:    Degree of pseudo-Gaussian
!------------------------------------------------------------------------------
      SUBROUTINE RFDPAR2(BE,RATE,TURN,SAMP,M)
      PARAMETER(NN=12)
      IMPLICIT COMPLEX(C)
      DIMENSION BE(M),RATE(M),TURN(M,M)
      KMOD2=MOD(M,2)
      S=0.
      IF(KMOD2.EQ.1)THEN     ! The first root is real:
       R1=RATE(1)
       BE1=BE(1)
       TURN(1,1)=BE1/(2*R1)
       S=S+TURN(1,1)*BE1
       DO LR=KMOD2+1,M,2
	LI=LR+1
	CBE=CMPLX(BE(LR),BE(LI))
	CRL=CMPLX(RATE(LR),RATE(LI))
	CL1=CBE/(R1+CRL)
	TURN(LR,1)=REAL(CL1)
	TURN(LI,1)=AIMAG(CL1)
	C1L=BE1/(R1+CRL)
	TURN(1,LR)=REAL(C1L)
	TURN(1,LI)=-AIMAG(C1L)
	S=S+TURN(LR,1)*BE1+TURN(1,LR)*BE(LR)+TURN(1,LI)*BE(LI)
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       CRK=CMPLX(RATE(KR),RATE(KI))
       CRJ=CONJG(CRK)
       BEKR=BE(KR)
       BEKI=BE(KI)
       DO LR=KMOD2+1,M,2
	LI=LR+1
	CBEH=.5*CMPLX(BE(LR),BE(LI))
	CRL=CMPLX(RATE(LR),RATE(LI))
	CLK=CBEH/(CRL+CRK)
	CLJ=CBEH/(CRL+CRJ)
	CLKR=CLK+CLJ
	CLKI=CLK-CLJ
	TURN(LR,KR)=  REAL(CLKR)
	TURN(LI,KR)= AIMAG(CLKR)
	TURN(LR,KI)=-AIMAG(CLKI)
	TURN(LI,KI)=  REAL(CLKI)
	S=S+TURN(LR,KR)*BEKR+TURN(LR,KI)*BEKI
       ENDDO
      ENDDO
      SAMP=.5/S
      RETURN
      END SUBROUTINE RFDPAR2

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE RFDPARV
! Initialize Alpha coefficients for pseudo-Gaussian smoother of degree M
! along a line with varying effective scale at each point
!
! --> DSH:  Array of "effective" spacing (in stretched space S) between gridpts
! --> RATE: Exponential rates in this degree of approximation to the Gaussian.
! <-- AL:   Array of "transfer function" coefficients to propagate vectors
!	    GAP and DEP of amplitudes of the exponential contributions to
!	    a filtered field along this grid line.
! --> N:    Number of grid spaces along this line (ie, number of grdpts = N+1)
! --> M:    Degree of approximation to pseudo-Gaussian.
!------------------------------------------------------------------------------
      SUBROUTINE RFDPARV(DSH,RATE,AL,N,M)
      IMPLICIT COMPLEX(C)
      DIMENSION DSH(N),RATE(M),AL(n,m)

      KMOD2=MOD(M,2)
      IF(KMOD2.EQ.1)THEN
       R1=RATE(1)
       DO I=1,N
        AL(I,1)=EXP(-R1*DSH(I))
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       CRK=CMPLX(RATE(KR),RATE(KI))
       DO I=1,N
	CRDS=CRK*DSH(I)
	CAL=CEXP(-CRDS)
	AL(i,KR)=REAL(CAL)
	AL(i,KI)=AIMAG(CAL)
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RFDPARV

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE RFDPARX
! Initialize Alpha coefficients for pseudo-Gaussian smoother of degree M
! along X lines with varying effective scales at each point
!
! --> DSHX: Array of "effective" spacing (in stretched space S) between gridpts
! --> RATE: Exponential rates in this degree of approximation to the Gaussian.
! <-- ALX:  Array of "transfer function" coefficients to propagate vectors
!	    GA and DE of amplitudes of the exponential contributions to
!	    a filtered field along these X lines.
! --> NX:   Number of grid spaces along X lines (ie, number of grdpts = NX+1)
! --> NY:   Number of grid spaces along Y lines
! --> M:    Degree of approximation to pseudo-Gaussian.
!------------------------------------------------------------------------------
      SUBROUTINE RfDPARX(DSHX,RATE,ALX,NX,NY,M)
      IMPLICIT COMPLEX(C)
      DIMENSION DSHX(NX,0:NY),RATE(M),ALX(0:NX,0:NY,M)
      KMOD2=MOD(M,2)
      IF(KMOD2.EQ.1)THEN
       R1=RATE(1)
       DO IY=0,NY
        ALX(0,IY,1)=1.
       ENDDO
        DO IY=0,NY
       DO IX=1,NX
         ALX(IX,IY,1)=EXP(-R1*DSHX(IX,IY))
        ENDDO
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       CRK=CMPLX(RATE(KR),RATE(KI))
       DO IY=0,NY
        ALX(0,IY,KR)=1.
        ALX(0,IY,KI)=0.
       ENDDO
        DO IY=0,NY
       DO IX=1,NX
         CRDS=CRK*DSHX(IX,IY)
         CAL=CEXP(-CRDS)
         ALX(IX,IY,KR)=REAL(CAL)
         ALX(IX,IY,KI)=AIMAG(CAL)
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RfDPARX


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE RFDPARY
! Initialize Alpha coefficients for pseudo-Gaussian smoother of degree M
! along Y lines with varying effective scale at each point
!
! --> DSHY: Array of "effective" spacing (in stretched space S) between gridpts
! --> RATE: Exponential rates in this degree of approximation to the Gaussian.
! <-- ALY:  Array of "transfer function" coefficients to propagate vectors
!	    GA and DE of amplitudes of the exponential contributions to
!	    a filtered field along these Y grid line.
! --> NX:   Number of grid spaces along X lines (ie, number of grdpts = NX+1)
! --> NY:   Number of grid spaces along Y lines.
! --> M:    Degree of approximation to pseudo-Gaussian.
!------------------------------------------------------------------------------
      SUBROUTINE RFDPARY(DSHY,RATE,ALY,NX,NY,M)
      IMPLICIT COMPLEX(C)
      DIMENSION DSHY(0:NX,NY),RATE(M),ALY(0:NX,0:NY,M)
      KMOD2=MOD(M,2)
      IF(KMOD2.EQ.1)THEN
       R1=RATE(1)
       DO IX=0,NX
        ALY(IX,0,1)=1.
       ENDDO
       DO IY=1,NY
        DO IX=0,NX
         ALY(IX,IY,1)=EXP(-R1*DSHY(IX,IY))
        ENDDO
       ENDDO
      ENDIF
      DO KR=KMOD2+1,M,2
       KI=KR+1
       CRK=CMPLX(RATE(KR),RATE(KI))
       DO IX=0,NX
        ALY(IX,0,KR)=1.
        ALY(IX,0,KI)=0.
       ENDDO
       DO IY=1,NY
        DO IX=0,NX
         CRDS=CRK*DSHY(IX,IY)
         CAL=CEXP(-CRDS)
         ALY(IX,IY,KR)=REAL(CAL)
         ALY(IX,IY,KI)=AIMAG(CAL)
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RFDPARY

      SUBROUTINE RFIXY(P1,GAX1,DEX1,GAY1,DEY1&
      ,GAGAXY1,DEGAXY1,GADEXY1,DEDEXY1&
      ,GAX2,DEX2,GAY2,DEY2&
      ,GAGAXY2,DEGAXY2,GADEXY2,DEDEXY2&
      ,DSSX,DSSY,SAMP,TURN, NX,NY,M)
      DIMENSION P1(0:NX,0:NY)&
      ,GAX1(M,0:NY),DEX1(M,0:NY),GAY1(0:NX,M),DEY1(0:NX,M)&
      ,GAGAXY1(M,M),DEGAXY1(M,M),GADEXY1(M,M),DEDEXY1(M,M)&
      ,GAX2(M,0:NY),DEX2(M,0:NY),GAY2(0:NX,M),DEY2(0:NX,M)&
      ,GAGAXY2(M,M),DEGAXY2(M,M),GADEXY2(M,M),DEDEXY2(M,M)&
      ,DSSX(0:NX,0:NY),DSSY(0:NX,0:NY),TURN(M,M)
      NXP=NX+1
      NYP=NY+1
      SAMPS=SAMP*SAMP

      DSS00=DSSX( 0, 0)*DSSY( 0, 0)*SAMPS
      DSS0N=DSSX( 0,NY)*DSSY( 0,NY)*SAMPS
      DSSN0=DSSX(NX, 0)*DSSY(NX, 0)*SAMPS
      DSSNN=DSSX(NX,NY)*DSSY(NX,NY)*SAMPS
       DO IY=0,NY
      DO LX=1,M
	GAX2(LX,IY)=GAX1(LX,IY)*DSSY(NX,IY)*SAMPS
	DEX2(LX,IY)=DEX1(LX,IY)*DSSY( 0,IY)*SAMPS
       ENDDO
       ENDDO
       DO LY=1,M
      DO LX=1,M
	GAGAXY1(LX,LY)=GAGAXY1(LX,LY)*DSSNN
	GADEXY1(LX,LY)=GADEXY1(LX,LY)*DSSN0
	DEGAXY1(LX,LY)=DEGAXY1(LX,LY)*DSS0N
	DEDEXY1(LX,LY)=DEDEXY1(LX,LY)*DSS00
       ENDDO
      ENDDO
      DO LY=1,M
       DO IX=0,NX
	GAY2(IX,LY)=GAY1(IX,LY)*DSSX(IX,NY)*SAMPS
	DEY2(IX,LY)=DEY1(IX,LY)*DSSX(IX, 0)*SAMPS
       ENDDO
      ENDDO

      CALL MULMVY(TURN,GAX2,   DEX1,   M,M,NYP, M,M,M)
      CALL MULMVY(TURN,GAGAXY1,DEGAXY2,M,M,M,	M,M,M)
      CALL MULMVY(TURN,GADEXY1,DEDEXY2,M,M,M,	M,M,M)
      CALL MULMVY(TURN,DEX2,   GAX1,   M,M,NYP, M,M,M)
      CALL MULMVY(TURN,DEGAXY1,GAGAXY2,M,M,M,	M,M,M)
      CALL MULMVY(TURN,DEDEXY1,GADEXY2,M,M,M,	M,M,M)

      CALL MULMVX(TURN,  GAY2 ,	 DEY1 ,	 M,M,NXP, M,NXP,NXP)
      CALL MULMVX(TURN,GAGAXY2,GADEXY1,	 M,M,M,   M,M,M)
      CALL MULMVX(TURN,DEGAXY2,DEDEXY1,	 M,M,M,   M,M,M)
      CALL MULMVX(TURN,  DEY2 ,	 GAY1 ,	 M,M,NXP, M,NXP,NXP)
      CALL MULMVX(TURN,GADEXY2,GAGAXY1,	 M,M,M,   M,M,M)
      CALL MULMVX(TURN,DEDEXY2,DEGAXY1,	 M,M,M,   M,M,M)

      DO IY=0,NY
       DO IX=0,NX
	P1(IX,IY)=P1(IX,IY)*DSSX(IX,IY)*DSSY(IX,IY)*SAMPS
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RFIXY




      SUBROUTINE RFHX(P1	 ,EN1		 ,E01&
      		     ,P2,GAP,DEP ,EN2,GAEN,DEEN  ,E02,GAE0,DEE0&
      , NX,NY,MX,MY,ALX,BE)
      LOGICAL LMOD2
      DIMENSION P1(0:NX,0:NY),E01(0:NX,MY),EN1(0:NX,MY)&
      	       ,P2(0:NX,0:NY),E02(0:NX,MY),EN2(0:NX,MY)&
      ,ALX(0:NX,0:NY,MX),BE(MX)&
      ,GAP(MX,0:NY),DEP(MX,0:NY)&
      ,GAE0(MX,MY),DEE0(MX,MY),GAEN(MX,MY),DEEN(MX,MY)
      KMOD2=MOD(MX,2)
      LMOD2=KMOD2.EQ.1
      IF(LMOD2)BE1=BE(1)

      DO IY=0,NY
      DO IX=0,NX
       P2(IX,IY)=0.
      ENDDO
      ENDDO
      DO LY=1,MY
      DO IX=0,NX
       E02(IX,LY)=0.
       EN2(IX,LY)=0.
      ENDDO
      ENDDO

! Advancing filter:
      DO IX=0,NX
       IF(LMOD2)THEN	! Treat the real root:
	DO IY=0,NY
	 GAP(1,IY)=ALX(IX,IY,1)*GAP(1,IY)+BE1*P1(IX,IY)
	 P2(IX,IY)=P2(IX,IY)+GAP(1,IY)
	ENDDO
	AL01=ALX(IX, 0,1)
	ALN1=ALX(IX,NY,1)
	DO LY=1,MY
	 GAE0(1,LY)=AL01*GAE0(1,LY)+BE1*E01(IX,LY)
	 E02(IX,LY)=E02(IX,LY)+GAE0(1,LY)
	 GAEN(1,LY)=ALN1*GAEN(1,LY)+BE1*EN1(IX,LY)
	 EN2(IX,LY)=EN2(IX,LY)+GAEN(1,LY)
	ENDDO
       ENDIF
			   ! Treat remaining complex roots:
       DO KR=KMOD2+1,MX,2  ! <-- Index of "real" components
	KI=KR+1 	   ! <-- Index of "imag" components
	BEKR=BE(KR)
	BEKI=BE(KI)
	DO IY=0,NY
	 GAKR=GAP(KR,IY)
	 GAKI=GAP(KI,IY)
	 GAP(KR,IY)=ALX(IX,IY,KR)*GAKR&
      		   -ALX(IX,IY,KI)*GAKI+BEKR*P1(IX,IY)
	 GAP(KI,IY)=ALX(IX,IY,KI)*GAKR&
      		   +ALX(IX,IY,KR)*GAKI+BEKI*P1(IX,IY)
	 P2(IX,IY)=P2(IX,IY)+GAP(KR,IY)
	ENDDO
	AL0KR=ALX(IX,0 ,KR)
	AL0KI=ALX(IX,0 ,KI)
	ALNKR=ALX(IX,NY,KR)
	ALNKI=ALX(IX,NY,KI)
	DO LY=1,MY
	 GAKR=GAE0(KR,LY)
	 GAKI=GAE0(KI,LY)
	 GAE0(KR,LY)=AL0KR*GAKR&
      		    -AL0KI*GAKI+BEKR*E01(IX,LY)
	 GAE0(KI,LY)=AL0KI*GAKR&
      		   +AL0KR*GAKI+BEKI*E01(IX,LY)
	 E02(IX,LY)=E02(IX,LY)+GAE0(KR,LY)
	 GAKR=GAEN(KR,LY)
	 GAKI=GAEN(KI,LY)
	 GAEN(KR,LY)=ALNKR*GAKR&
      		    -ALNKI*GAKI+BEKR*EN1(IX,LY)
	 GAEN(KI,LY)=ALNKI*GAKR&
      		    +ALNKR*GAKI+BEKI*EN1(IX,LY)
	 EN2(IX,LY)=EN2(IX,LY)+GAEN(KR,LY)
	ENDDO
       ENDDO
      ENDDO

! Backing filter:
      DO IX=NX,0,-1
       IF(LMOD2)THEN	! Treat the real root:
	DO IY=0,NY
	 P2(IX,IY)=P2(IX,IY)+DEP(1,IY)
	 DEP(1,IY)=ALX(IX,IY,1)*(DEP(1,IY)+BE1*P1(IX,IY))
	ENDDO
	AL01=ALX(IX, 0,1)
	ALN1=ALX(IX,NY,1)
	DO LY=1,MY
	 E02(IX,LY)=E02(IX,LY)+DEE0(1,LY)
	 DEE0(1,LY)=AL01*(DEE0(1,LY)+BE1*E01(IX,LY))
	 EN2(IX,LY)=EN2(IX,LY)+DEEN(1,LY)
	 DEEN(1,LY)=ALN1*(DEEN(1,LY)+BE1*EN1(IX,LY))
	ENDDO
       ENDIF
			   ! Treat remaining complex roots:
       DO KR=KMOD2+1,MX,2   ! <-- Index of "real" components
	KI=KR+1 	   ! <-- Index of "imag" components
	BEKR=BE(KR)
	BEKI=BE(KI)
	DO IY=0,NY
	 P2(IX,IY)=P2(IX,IY)+DEP(KR,IY)
	 DEKR=DEP(KR,IY)+BEKR*P1(IX,IY)
	 DEKI=DEP(KI,IY)+BEKI*P1(IX,IY)
	 DEP(KR,IY)=ALX(IX,IY,KR)*DEKR-ALX(IX,IY,KI)*DEKI
	 DEP(KI,IY)=ALX(IX,IY,KI)*DEKR+ALX(IX,IY,KR)*DEKI
	ENDDO
	AL0KR=ALX(IX, 0,KR)
	AL0KI=ALX(IX, 0,KI)
	ALNKR=ALX(IX,NY,KR)
	ALNKI=ALX(IX,NY,KI)
	DO LY=1,MY
	 E02(IX,LY)=E02(IX,LY)+DEE0(KR,LY)
	 DEKR=DEE0(KR,LY)+BEKR*E01(IX,LY)
	 DEKI=DEE0(KI,LY)+BEKI*E01(IX,LY)
	 DEE0(KR,LY)=AL0KR*DEKR-AL0KI*DEKI
	 DEE0(KI,LY)=AL0KI*DEKR+AL0KR*DEKI
	 EN2(IX,LY)=EN2(IX,LY)+DEEN(KR,LY)
	 DEKR=DEEN(KR,LY)+BEKR*EN1(IX,LY)
	 DEKI=DEEN(KI,LY)+BEKI*EN1(IX,LY)
	 DEEN(KR,LY)=ALNKR*DEKR-ALNKI*DEKI
	 DEEN(KI,LY)=ALNKI*DEKR+ALNKR*DEKI
	ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RFHX

      SUBROUTINE RFHY(P1	 ,EN1		,E01&
      		     ,P2,GAP,DEP ,EN2,GAEN,DEEN ,E02,GAE0,DEE0&
      , NX,NY,MX,MY,ALY,BE)
      LOGICAL LMOD2
      DIMENSION P1(0:NX,0:NY),E01(MX,0:NY),EN1(MX,0:NY)&
      	       ,P2(0:NX,0:NY),E02(MX,0:NY),EN2(MX,0:NY)&
      ,ALY(0:NX,0:NY,MY),BE(MY)&
      ,GAP(0:NX,MY),DEP(0:NX,MY)&
      ,GAE0(MX,MY),DEE0(MX,MY),GAEN(MX,MY),DEEN(MX,MY)
      KMOD2=MOD(MY,2)
      LMOD2=KMOD2.EQ.1
      IF(LMOD2)BE1=BE(1)

      DO IY=0,NY
      DO IX=0,NX
       P2(IX,IY)=0.
      ENDDO
      ENDDO
      DO IY=0,NY
      DO LX=1,MX
       E02(LX,IY)=0.
       EN2(LX,IY)=0.
      ENDDO
      ENDDO

! Advancing filter:
      DO IY=0,NY
       IF(LMOD2)THEN	! Treat the real root:
	DO IX=0,NX
	 GAP(IX,1)=ALY(IX,IY,1)*GAP(IX,1)+BE1*P1(IX,IY)
	 P2(IX,IY)=P2(IX,IY)+GAP(IX,1)
	ENDDO
	AL01=ALY( 0,IY,1)
	ALN1=ALY(NX,IY,1)
	DO LX=1,MX
	 GAE0(LX,1)=AL01*GAE0(LX,1)+BE1*E01(LX,IY)
	 E02(LX,IY)=E02(LX,IY)+GAE0(LX,1)
	 GAEN(LX,1)=ALN1*GAEN(LX,1)+BE1*EN1(LX,IY)
	 EN2(LX,IY)=EN2(LX,IY)+GAEN(LX,1)
	ENDDO
       ENDIF
			   ! Treat remaining complex roots:
       DO KR=KMOD2+1,MY,2  ! <-- Index of "real" components
	KI=KR+1 	   ! <-- Index of "imag" components
	BEKR=BE(KR)
	BEKI=BE(KI)
	DO IX=0,NX
	 GAKR=GAP(IX,KR)
	 GAKI=GAP(IX,KI)
	 GAP(IX,KR)=ALY(IX,IY,KR)*GAKR&
      		   -ALY(IX,IY,KI)*GAKI+BEKR*P1(IX,IY)
	 GAP(IX,KI)=ALY(IX,IY,KI)*GAKR&
      		   +ALY(IX,IY,KR)*GAKI+BEKI*P1(IX,IY)
	 P2(IX,IY)=P2(IX,IY)+GAP(IX,KR)
	ENDDO
	AL0KR=ALY( 0,IY,KR)
	AL0KI=ALY( 0,IY,KI)
	ALNKR=ALY(NX,IY,KR)
	ALNKI=ALY(NX,IY,KI)
	DO LX=1,MX
	 GAKR=GAE0(LX,KR)
	 GAKI=GAE0(LX,KI)
	 GAE0(LX,KR)=AL0KR*GAKR&
      		    -AL0KI*GAKI+BEKR*E01(LX,IY)
	 GAE0(LX,KI)=AL0KI*GAKR&
      		    +AL0KR*GAKI+BEKI*E01(LX,IY)
	 E02(LX,IY)=E02(LX,IY)+GAE0(LX,KR)
	 GAKR=GAEN(LX,KR)
	 GAKI=GAEN(LX,KI)
	 GAEN(LX,KR)=ALNKR*GAKR&
      		   -ALNKI*GAKI+BEKR*EN1(LX,IY)
	 GAEN(LX,KI)=ALNKI*GAKR&
      		   +ALNKR*GAKI+BEKI*EN1(LX,IY)
	 EN2(LX,IY)=EN2(LX,IY)+GAEN(LX,KR)
	ENDDO
       ENDDO
      ENDDO

! Backing filter:
      DO IY=NY,0,-1
       IF(LMOD2)THEN	! Treat the real root:
	DO IX=0,NX
	 P2(IX,IY)=P2(IX,IY)+DEP(IX,1)
	 DEP(IX,1)=ALY(IX,IY,1)*(DEP(IX,1)+BE1*P1(IX,IY))
	ENDDO
	AL01=ALY( 0,IY,1)
	ALN1=ALY(NX,IY,1)
	DO LX=1,MX
	 E02(LX,IY)=E02(LX,IY)+DEE0(LX,1)
	 DEE0(LX,1)=AL01*(DEE0(LX,1)+BE1*E01(LX,IY))
	 EN2(LX,IY)=EN2(LX,IY)+DEEN(LX,1)
	 DEEN(LX,1)=ALN1*(DEEN(LX,1)+BE1*EN1(LX,IY))
	ENDDO
       ENDIF
			   ! Treat remaining complex roots:
       DO KR=KMOD2+1,MY,2  ! <-- Index of "real" components
	KI=KR+1 	   ! <-- Index of "imag" components
	BEKR=BE(KR)
	BEKI=BE(KI)
	DO IX=0,NX
	 P2(IX,IY)=P2(IX,IY)+DEP(IX,KR)
	 DEKR=DEP(IX,KR)+BEKR*P1(IX,IY)
	 DEKI=DEP(IX,KI)+BEKI*P1(IX,IY)
	 DEP(IX,KR)=ALY(IX,IY,KR)*DEKR-ALY(IX,IY,KI)*DEKI
	 DEP(IX,KI)=ALY(IX,IY,KI)*DEKR+ALY(IX,IY,KR)*DEKI
	ENDDO
	AL0KR=ALY( 0,IY,KR)
	AL0KI=ALY( 0,IY,KI)
	ALNKR=ALY(NX,IY,KR)
	ALNKI=ALY(NX,IY,KI)
	DO LX=1,MX
	 E02(LX,IY)=E02(LX,IY)+DEE0(LX,KR)
	 DEKR=DEE0(LX,KR)+BEKR*E01(LX,IY)
	 DEKI=DEE0(LX,KI)+BEKI*E01(LX,IY)
	 DEE0(LX,KR)=AL0KR*DEKR-AL0KI*DEKI
	 DEE0(LX,KI)=AL0KI*DEKR+AL0KR*DEKI
	 EN2(LX,IY)=EN2(LX,IY)+DEEN(LX,KR)
	 DEKR=DEEN(LX,KR)+BEKR*EN1(LX,IY)
	 DEKI=DEEN(LX,KI)+BEKI*EN1(LX,IY)
	 DEEN(LX,KR)=ALNKR*DEKR-ALNKI*DEKI
	 DEEN(LX,KI)=ALNKI*DEKR+ALNKR*DEKI
	ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RFHY

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1998
!		    SUBROUTINE RFHV
!   Apply pseudo-Gaussian "right-plus-left" smoother to a single "vector"
!   of data.
!
! --> P1:   Input data.
! <-- P2:   Output data.
! --> N:    Number of grid intervals (one less than number of points).
! --> M:    Degree of approximation of smoother to a Gaussian.
! --> AL:   Matrix of "Alpha" coefficients (computed in RFDPARV).
! --> BE:   Vector of "Beta" coefficients (computed in RFDPAR1).
! <-> GAP:  Gamma vector of exponential amplitudes of P decaying to the right.
! <-> DEP:  Delta vector of exponential amplitudes of P decaying to the left.
!------------------------------------------------------------------------------

      SUBROUTINE MULMVY(A,B,C,MI,MJ,MK, NA,NB,NC)
!  Here we assume MK >> MI and MK >> MJ and arrange to get "long" vectors.
!  Functionally equivalent to MULMM(A,B,C,MI,MJ,MK,...).
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      DO I=1,MI
       DO K=1,MK
	C(I,K)=0.
       ENDDO
       DO J=1,MJ
	AIJ=A(I,J)
	DO K=1,MK
	 C(I,K)=C(I,K)+AIJ*B(J,K)
	ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MULMVY
      SUBROUTINE MULMVX(A,B,C,MI,MJ,MK, NA,NB,NC)
!  Here we assume MK >> MI and MK >> MJ and arrange to get "long" vectors.
!  Functionally equivalent to MULMT(B,A,C,MK,MJ,MI,...).
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      DO I=1,MI
       DO K=1,MK
	C(K,I)=0.
       ENDDO
       DO J=1,MJ
	AIJ=A(I,J)
	DO K=1,MK
	 C(K,I)=C(K,I)+AIJ*B(K,J)
	ENDDO
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MULMVX

end module da_rf_cv3
