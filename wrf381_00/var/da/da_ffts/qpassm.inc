!C     SUBROUTINE 'QPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART!C     OF MULTIPLE REAL FFT (FOURIER ANALYSIS) ROUTINE
!C
!C     A IS FIRST REAL INPUT VECTOR
!C         EQUIVALENCE B(1) WITH A(IFAC*LA*INC1+1)
!C     C IS FIRST REAL OUTPUT VECTOR
!C         EQUIVALENCE D(1) WITH C(LA*INC2+1)
!C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES
!C     INC1 IS THE ADDRESSING INCREMENT FOR A
!C     INC2 IS THE ADDRESSING INCREMENT FOR C
!C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A
!C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C
!C     LOT IS THE NUMBER OF VECTORS
!C     N IS THE LENGTH OF THE VECTORS
!C     IFAC IS THE CURRENT FACTOR OF N
!C     LA = N/(PRODUCT OF FACTORS USED SO FAR)
!C     IERR IS AN ERROR INDICATOR:
!C              0 - PASS COMPLETED WITHOUT ERROR
!C              1 - LOT GREATER THAN VectorLength
!C              2 - IFAC NOT CATERED FOR
!C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC
!C
!C-----------------------------------------------------------------------
!C
     SUBROUTINE QPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,LA,IERR)

      INTEGER :: inc1, inc2, inc3, inc4, lot, n, ifac, la, ierr

      REAL  :: a, b, c, d, trigs
      DIMENSION A(*),B(*),C(*),D(*),TRIGS(N)
! Local named constants
 CHARACTER (LEN=*), PARAMETER :: RoutineName = "QPASSM"
!
      REAL  :: SIN36, SIN72, QRT5, SIN60
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/, &
          QRT5/0.559016994374947/,SIN60/0.866025403784437/

      REAL    :: s1, s2, s3, s4, s5
      REAL    :: sin45, zsin36, zsin72, zqrt5, zsin60, zsin45, z
      REAL    :: a0, a1, a2, a3, a4, a5, a6, a10, a11, a20, a21
      REAL    :: b0, b1, b2, b3, b4, b5, b6, b10, b11, b20, b21
!      REAL    :: c0, c1, c2, c3, c4, c5
      REAL    :: c1, c2, c3, c4, c5
      INTEGER :: i, ijk, l, k, kb, m, iink, jink, ijump, kstop
      INTEGER :: ibad, igo, ia, ie, je, ibase, jbase, ja, jb, j, ic
      INTEGER :: if, jf, kf, ib, jc, kc, id, jd, kd, ke, ig, ih
      INTEGER :: vectorlength
!
!- End of header ---------------------------------------------------------------


      M=N/IFAC
      IINK=LA*INC1
      JINK=LA*INC2
      IJUMP=(IFAC-1)*IINK
      KSTOP=(N-IFAC)/(2*IFAC)
!
      IBAD=1
      VectorLength = lot
      IF (LOT.GT.VectorLength) GO TO 910
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.EQ.7) IGO=6
      IBAD=2
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910
      GO TO (200,300,400,500,600,800),IGO
!
!     CODING FOR FACTOR 2
!     -------------------
  200 CONTINUE
      IA=1
      IB=IA+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
!
      IF (LA.EQ.M) GO TO 290
!
      DO 220 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  210 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  220 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JB) GO TO 260
      DO 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      JBASE=0
      DO 240 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+(C1*A(IB+I)+S1*B(IB+I))
      C(JB+J)=A(IA+I)-(C1*A(IB+I)+S1*B(IB+I))
      D(JA+J)=(C1*B(IB+I)-S1*A(IB+I))+B(IA+I)
      D(JB+J)=(C1*B(IB+I)-S1*A(IB+I))-B(IA+I)
      I=I+INC3
      J=J+INC4
  230 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  240 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB-JINK
  250 CONTINUE
      IF (JA.GT.JB) GO TO 900
  260 CONTINUE
      JBASE=0
      DO 280 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      D(JA+J)=-A(IB+I)
      I=I+INC3
      J=J+INC4
  270 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  280 CONTINUE
      GO TO 900
!
  290 CONTINUE
      Z=1.0/FLOAT(N)
      DO 294 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 292 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+A(IB+I))
      C(JB+J)=Z*(A(IA+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  292 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  294 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 3
!     -------------------
  300 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB
!
      IF (LA.EQ.M) GO TO 390
!
      DO 320 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-0.5*(A(IB+I)+A(IC+I))
      D(JB+J)=SIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  310 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  320 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JC) GO TO 360
      DO 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      JBASE=0
      DO 340 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 330 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C2*A(IC+I)+S2*B(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C2*B(IC+I)-S2*A(IC+I))
      A2=A(IA+I)-0.5*A1
      B2=B(IA+I)-0.5*B1
      A3=SIN60*((C1*A(IB+I)+S1*B(IB+I))-(C2*A(IC+I)+S2*B(IC+I)))
      B3=SIN60*((C1*B(IB+I)-S1*A(IB+I))-(C2*B(IC+I)-S2*A(IC+I)))
      C(JA+J)=A(IA+I)+A1
      D(JA+J)=B(IA+I)+B1
      C(JB+J)=A2+B3
      D(JB+J)=B2-A3
      C(JC+J)=A2-B3
      D(JC+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  330 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  340 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
  350 CONTINUE
      IF (JA.GT.JC) GO TO 900
  360 CONTINUE
      JBASE=0
      DO 380 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+0.5*(A(IB+I)-A(IC+I))
      D(JA+J)=-SIN60*(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-(A(IB+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  370 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  380 CONTINUE
      GO TO 900
!
  390 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 394 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 392 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=Z*(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))
      D(JB+J)=ZSIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  392 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  394 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 4
!     -------------------
  400 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JB
!
      IF (LA.EQ.M) GO TO 490
!
      DO 420 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      C(JB+J)=A(IA+I)-A(IC+I)
      D(JB+J)=A(ID+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  410 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  420 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JC) GO TO 460
      DO 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      JBASE=0
      DO 440 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 430 IJK=1,LOT
      A0=A(IA+I)+(C2*A(IC+I)+S2*B(IC+I))
      A2=A(IA+I)-(C2*A(IC+I)+S2*B(IC+I))
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C3*A(ID+I)+S3*B(ID+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C3*A(ID+I)+S3*B(ID+I))
      B0=B(IA+I)+(C2*B(IC+I)-S2*A(IC+I))
      B2=B(IA+I)-(C2*B(IC+I)-S2*A(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C3*B(ID+I)-S3*A(ID+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C3*B(ID+I)-S3*A(ID+I))
      C(JA+J)=A0+A1
      C(JC+J)=A0-A1
      D(JA+J)=B0+B1
      D(JC+J)=B1-B0
      C(JB+J)=A2+B3
      C(JD+J)=A2-B3
      D(JB+J)=B2-A3
      D(JD+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  430 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  440 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
  450 CONTINUE
      IF (JB.GT.JC) GO TO 900
  460 CONTINUE
      SIN45=SQRT(0.5)
      JBASE=0
      DO 480 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+SIN45*(A(IB+I)-A(ID+I))
      C(JB+J)=A(IA+I)-SIN45*(A(IB+I)-A(ID+I))
      D(JA+J)=-A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      D(JB+J)=A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      I=I+INC3
      J=J+INC4
  470 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  480 CONTINUE
      GO TO 900
!
  490 CONTINUE
      Z=1.0/FLOAT(N)
      DO 494 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 492 IJK=1,LOT
      C(JA+J)=Z*((A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JC+J)=Z*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JB+J)=Z*(A(IA+I)-A(IC+I))
      D(JB+J)=Z*(A(ID+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  492 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  494 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 5
!     -------------------
  500 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC
      JE=JB
!
      IF (LA.EQ.M) GO TO 590
!
      DO 520 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 510 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-SIN72*A3-SIN36*A4
      D(JC+J)=-SIN36*A3+SIN72*A4
      I=I+INC3
      J=J+INC4
  510 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  520 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JD) GO TO 560
      DO 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      JBASE=0
      DO 540 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 530 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C4*A(IE+I)+S4*B(IE+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C4*A(IE+I)+S4*B(IE+I))
      A2=(C2*A(IC+I)+S2*B(IC+I))+(C3*A(ID+I)+S3*B(ID+I))
      A4=(C2*A(IC+I)+S2*B(IC+I))-(C3*A(ID+I)+S3*B(ID+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C4*B(IE+I)-S4*A(IE+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C4*B(IE+I)-S4*A(IE+I))
      B2=(C2*B(IC+I)-S2*A(IC+I))+(C3*B(ID+I)-S3*A(ID+I))
      B4=(C2*B(IC+I)-S2*A(IC+I))-(C3*B(ID+I)-S3*A(ID+I))
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      B5=B(IA+I)-0.25*(B1+B2)
      B6=QRT5*(B1-B2)
      A10=A5+A6
      A20=A5-A6
      B10=B5+B6
      B20=B5-B6
      A11=SIN72*B3+SIN36*B4
      A21=SIN36*B3-SIN72*B4
      B11=SIN72*A3+SIN36*A4
      B21=SIN36*A3-SIN72*A4
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A10+A11
      C(JE+J)=A10-A11
      C(JC+J)=A20+A21
      C(JD+J)=A20-A21
      D(JA+J)=B(IA+I)+(B1+B2)
      D(JB+J)=B10-B11
      D(JE+J)=-(B10+B11)
      D(JC+J)=B20-B21
      D(JD+J)=-(B20+B21)
      I=I+INC3
      J=J+INC4
  530 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  540 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
  550 CONTINUE
      IF (JB.GT.JD) GO TO 900
  560 CONTINUE
      JBASE=0
      DO 580 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 570 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)+0.25*(A3-A4)
      A6=QRT5*(A3+A4)
      C(JA+J)=A5+A6
      C(JB+J)=A5-A6
      C(JC+J)=A(IA+I)-(A3-A4)
      D(JA+J)=-SIN36*A1-SIN72*A2
      D(JB+J)=-SIN72*A1+SIN36*A2
      I=I+INC3
      J=J+INC4
  570 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  580 CONTINUE
      GO TO 900
!
  590 CONTINUE
      Z=1.0/FLOAT(N)
      ZQRT5=Z*QRT5
      ZSIN36=Z*SIN36
      ZSIN72=Z*SIN72
      DO 594 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 592 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=Z*(A(IA+I)-0.25*(A1+A2))
      A6=ZQRT5*(A1-A2)
      C(JA+J)=Z*(A(IA+I)+(A1+A2))
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-ZSIN72*A3-ZSIN36*A4
      D(JC+J)=-ZSIN36*A3+ZSIN72*A4
      I=I+INC3
      J=J+INC4
  592 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  594 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 6
!     -------------------
  600 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JC
      JF=JB
!
      IF (LA.EQ.M) GO TO 690
!
      DO 620 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 610 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=(A(IA+I)+A(ID+I))+A11
      C(JC+J)=(A(IA+I)+A(ID+I)-0.5*A11)
      D(JC+J)=SIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=(A(IA+I)-A(ID+I))-0.5*A11
      D(JB+J)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=(A(IA+I)-A(ID+I))+A11
      I=I+INC3
      J=J+INC4
  610 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  620 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JC.EQ.JD) GO TO 660
      DO 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      JBASE=0
      DO 640 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 630 IJK=1,LOT
      A1=C1*A(IB+I)+S1*B(IB+I)
      B1=C1*B(IB+I)-S1*A(IB+I)
      A2=C2*A(IC+I)+S2*B(IC+I)
      B2=C2*B(IC+I)-S2*A(IC+I)
      A3=C3*A(ID+I)+S3*B(ID+I)
      B3=C3*B(ID+I)-S3*A(ID+I)
      A4=C4*A(IE+I)+S4*B(IE+I)
      B4=C4*B(IE+I)-S4*A(IE+I)
      A5=C5*A(IF+I)+S5*B(IF+I)
      B5=C5*B(IF+I)-S5*A(IF+I)
      A11=(A2+A5)+(A1+A4)
      A20=(A(IA+I)+A3)-0.5*A11
      A21=SIN60*((A2+A5)-(A1+A4))
      B11=(B2+B5)+(B1+B4)
      B20=(B(IA+I)+B3)-0.5*B11
      B21=SIN60*((B2+B5)-(B1+B4))
      C(JA+J)=(A(IA+I)+A3)+A11
      D(JA+J)=(B(IA+I)+B3)+B11
      C(JC+J)=A20-B21
      D(JC+J)=A21+B20
      C(JE+J)=A20+B21
      D(JE+J)=A21-B20
      A11=(A2-A5)+(A4-A1)
      A20=(A(IA+I)-A3)-0.5*A11
      A21=SIN60*((A4-A1)-(A2-A5))
      B11=(B5-B2)-(B4-B1)
      B20=(B3-B(IA+I))-0.5*B11
      B21=SIN60*((B5-B2)+(B4-B1))
      C(JB+J)=A20-B21
      D(JB+J)=A21-B20
      C(JD+J)=A11+(A(IA+I)-A3)
      D(JD+J)=B11+(B3-B(IA+I))
      C(JF+J)=A20+B21
      D(JF+J)=A21+B20
      I=I+INC3
      J=J+INC4
  630 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  640 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
  650 CONTINUE
      IF (JC.GT.JD) GO TO 900
  660 CONTINUE
      JBASE=0
      DO 680 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 670 IJK=1,LOT
      C(JA+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))+ SIN60*(A(IB+I)-A(IF+I))
      D(JA+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))-SIN60*(A(IC+I)+A(IE+I))
      C(JB+J)=A(IA+I)-(A(IC+I)-A(IE+I))
      D(JB+J)=A(ID+I)-(A(IB+I)+A(IF+I))
      C(JC+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))-SIN60*(A(IB+I)-A(IF+I))
      D(JC+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))+SIN60*(A(IC+I)+A(IE+I))
      I=I+INC3
      J=J+INC4
  670 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  680 CONTINUE
      GO TO 900
!
  690 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 694 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 692 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=Z*((A(IA+I)+A(ID+I))+A11)
      C(JC+J)=Z*((A(IA+I)+A(ID+I))-0.5*A11)
      D(JC+J)=ZSIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=Z*((A(IA+I)-A(ID+I))-0.5*A11)
      D(JB+J)=ZSIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=Z*((A(IA+I)-A(ID+I))+A11)
      I=I+INC3
      J=J+INC4
  692 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  694 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 8
!     -------------------
  800 CONTINUE
      IBAD=3
      IF (LA.NE.M) GO TO 910
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      IG=IF+IINK
      IH=IG+IINK
      JA=1
      JB=JA+LA*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JD+2*M*INC2
      Z=1.0/FLOAT(N)
      ZSIN45=Z*SQRT(0.5)
!
      DO 820 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 810 IJK=1,LOT
      C(JA+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))+ &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JE+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))- &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JC+J)=Z*((A(IA+I)+A(IE+I))-(A(IC+I)+A(IG+I)))
      D(JC+J)=Z*((A(ID+I)+A(IH+I))-(A(IB+I)+A(IF+I)))
      C(JB+J)=Z*(A(IA+I)-A(IE+I))+ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      C(JD+J)=Z*(A(IA+I)-A(IE+I))-ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      D(JB+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))+Z*(A(IG+I)-A(IC+I))
      D(JD+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))-Z*(A(IG+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  810 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  820 CONTINUE
!

  900 CONTINUE
      IBAD=0
  910 CONTINUE
      IERR=IBAD

      RETURN
      END SUBROUTINE QPASSM
