      SUBROUTINE LGNUV
     I(MEND1,NEND1,JEND1,MNWAV,IMAX,JMAX,JMAXHF,KMAX,PNM,DPNM,
     I QPSIX,QCHIX,
     O GUX  ,GVX   ,
     W GWRK)
C

      DIMENSION Q(KMAX,5,MNWAV)  
      DIMENSION QR(KMAX,5,MNWAV)  
      DIMENSION QPSIX(2,KMAX,MNWAV) ,QCHIX(2,KMAX,MNWAV)
      DIMENSION DPNM(MNWAV,JMAXHF)  ,PNM (MNWAV,JMAXHF)


      DIMENSION GU(KMAX,4,MEND1,2)
      DIMENSION GUX (IMAX,JMAX,KMAX),GVX (IMAX,JMAX,KMAX)
      DIMENSION GWRK(IMAX,4)
C
C     Rearrange the input data array so that we can do the main loop
C     as a matrix * vector operation with vector length 4*KMAX.
C     This gets the time consumption in this subroutine from a total of
C     nearly 50% down to 7%.
C     Probably one can improve the stacking of arrays and even get
C     VLEN=8*KMAX. But not tried yet.
C
C
C     Note we set distinct signs, because we wont to collapse 4
C     different assignments into one
!CDIR NOVECTOR
      DO K=1,KMAX
!CDIR NODEP
      DO L=1,MNWAV
      Q(K,1,L)=QPSIX(1,K,L)
      Q(K,2,L)=QPSIX(2,K,L)
      Q(K,3,L)=-QCHIX(1,K,L)
      Q(K,4,L)=-QCHIX(2,K,L)
      QR(K,4,L)=-QPSIX(1,K,L)
      QR(K,3,L)=QPSIX(2,K,L)
      QR(K,2,L)=-QCHIX(1,K,L)
      QR(K,1,L)=QCHIX(2,K,L)
      ENDDO
      ENDDO 
C
c*nec MNWAV=MNWAV
C*NEC 1999/05/12 start
      CALL RESET(gux ,IMAX*JMAX*KMAX)
      CALL RESET(gvx ,IMAX*JMAX*KMAX)
C     DO I=1,IMAX*JMAX*KMAX
Cc*nec CALL RESET(gu(,,)  ,IMAX*JMAX*KMAX)
C     gu(1,I,1)=0.0
Cc*nec CALL RESET(gv(,,)  ,IMAX*JMAX*KMAX)
C     gv(1,I,1)=0.0
C     ENDDO
Cc*nec
C*NEC 1999/05/12 end
C


      DO 150 J=1,JMAXHF
      JM=       J
      JP=JMAX+1-J
      JMC=1
      JPC=2
      IF ( JM.EQ.JP) JPC=1
      CALL RESET(GU,KMAX*4*MEND1*2)
      L =0
      DO 120 M=1,MEND1
      NMAX=MIN(JEND1+1-M,NEND1)
      QM  =FLOAT(M-1)
      DO 140 N=1,NMAX,2
      DO K=1,KMAX*4
      GU(K,1,M,JMC)=GU(K,1,M,JMC)-QM*QR(K,1,L+N)* PNM(L+N,J)
C-COLL      GU(K,2,M,JMC)=GU(K,2,M,JMC)+QM*QR(K,2,L+N)* PNM(L+N,J)
C-COLL      GU(K,3,M,JMC)=GU(K,3,M,JMC)-QM*QR(K,3,L+N)* PNM(L+N,J)
C-COLL      GU(K,4,M,JMC)=GU(K,4,M,JMC)+QM*QR(K,4,L+N)* PNM(L+N,J)
      ENDDO
  140 CONTINUE
      DO 160 N=1,NMAX,2
      DO K=1,KMAX*4
      GU(K,1,M,JPC)=GU(K,1,M,JPC)+   Q(K,1,L+N)*DPNM(L+N,J)
C-COLL      GU(K,2,M,JPC)=GU(K,2,M,JPC)+   Q(K,2,L+N)*DPNM(L+N,J)
C-COLL      GU(K,3,M,JPC)=GU(K,3,M,JPC)-   Q(K,3,L+N)*DPNM(L+N,J)
C-COLL      GU(K,4,M,JPC)=GU(K,4,M,JPC)-   Q(K,4,L+N)*DPNM(L+N,J)
      ENDDO
  160 CONTINUE
      DO 180 N=2,NMAX,2
      DO K=1,KMAX*4
      GU(K,1,M,JPC)=GU(K,1,M,JPC)-QM*QR(K,1,L+N)* PNM(L+N,J)
C-COLL      GU(K,2,M,JPC)=GU(K,2,M,JPC)+QM*QR(K,2,L+N)* PNM(L+N,J)
C-COLL      GU(K,3,M,JPC)=GU(K,3,M,JPC)-QM*QR(K,3,L+N)* PNM(L+N,J)
C-COLL      GU(K,4,M,JPC)=GU(K,4,M,JPC)+QM*QR(K,4,L+N)* PNM(L+N,J)
      ENDDO
  180 CONTINUE
      DO 200 N=2,NMAX,2
      DO K=1,KMAX*4
      GU(K,1,M,JMC)=GU(K,1,M,JMC)+   Q(K,1,L+N)*DPNM(L+N,J)
C-COLL      GU(K,2,M,JMC)=GU(K,2,M,JMC)+   Q(K,2,L+N)*DPNM(L+N,J)
C-COLL      GU(K,3,M,JMC)=GU(K,3,M,JMC)-   Q(K,3,L+N)*DPNM(L+N,J)
C-COLL      GU(K,4,M,JMC)=GU(K,4,M,JMC)-   Q(K,4,L+N)*DPNM(L+N,J)
      ENDDO
  200 CONTINUE
      L=L+NMAX
  120 CONTINUE
  100 CONTINUE
C
!CDIR NOVECTOR
        DO K=1,KMAX
!CDIR NODEP
        DO M=1,MEND1
        GUX(2*M-1,JM,K)=GU(K,1,M,JMC)
        GUX(2*M  ,JM,K)=GU(K,2,M,JMC)
        GVX(2*M-1,JM,K)=GU(K,3,M,JMC)
        GVX(2*M  ,JM,K)=GU(K,4,M,JMC)
        GUX(2*M-1,JP,K)=GU(K,1,M,JPC)
        GUX(2*M  ,JP,K)=GU(K,2,M,JPC)
        GVX(2*M-1,JP,K)=GU(K,3,M,JPC)
        GVX(2*M  ,JP,K)=GU(K,4,M,JPC)
        ENDDO
        ENDDO
C
  150 CONTINUE
C
      DO 300 K=1,KMAX
      DO 300 J=1,JMAXHF
      JM=       J
      JP=JMAX+1-J

      IF(MOD(JMAX,2).EQ.1.AND.J.EQ.JMAXHF) GO TO 300
      DO 220 M=1,MEND1*2
      GWRKM1=GUX(M,JM,K)+GUX(M,JP,K)
      GWRKM2=GUX(M,JM,K)-GUX(M,JP,K)
      GWRKM3=GVX(M,JM,K)+GVX(M,JP,K)
      GWRKM4=GVX(M,JM,K)-GVX(M,JP,K)
      GUX  (M,JM,K)=GWRKM1
      GUX  (M,JP,K)=GWRKM2
      GVX  (M,JM,K)=GWRKM3
      GVX  (M,JP,K)=GWRKM4
  220 CONTINUE
  300 CONTINUE
C
      RETURN
      END SUBROUTINE LGNUV

