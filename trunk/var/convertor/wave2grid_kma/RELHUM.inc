      SUBROUTINE RELHUM             !shc start
     I  (GTMP,GWV,GPS,IMAX,JMAX,KMAX,A,B,
     O   GRH)
C***********************************************************************
C                  CALCULATION OF RELATIVE HUMIDITY
C   フルレベル気圧計算機能付（J分割版）
C   CREATED                                      MAR.05,1998  T.TSUYUKI
C   REVISED TO DECREASE LOG CALCULATION          AUG    1999  Y.TAHARA
C 2000.04.18 作業配列を用いた高速版 RELHUM9とどちらが速いか
C            未チェック
C***********************************************************************
C<INPUT>
C GTMP: 気温（フルレベル）(K)
C GWV: 比湿（フルレベル）(KG/KG)
C GPS: モデル地表気圧(レベル1/2）(HPA)
C<OUTPUT>
C GRH: 相対湿度（フルレベル）(NON-UNIT)
C***********************************************************************
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (KM=50,IMAXD=640)
C
      DIMENSION GTMP(IMAX,JMAX,KMAX), GWV (IMAX,JMAX,KMAX),
     &          GPS (IMAX,JMAX     )
      DIMENSION GRH (IMAX,JMAX,KMAX)
C
      DIMENSION PFULL(IMAXD,KM)
      REAL*8    A(KMAX+1), B(KMAX+1)
      PARAMETER( E0C=6.11D0,AL=17.3D0,BL=237.3D0,
     .                                  AI=21.9D0,BI=265.3D0)
C******************** PROCEDURE ****************************************
      IF (KMAX.GT.KM) THEN
        WRITE(6,*) ' ERROR: <KMAX> IS TOO LARGE. in RELHUM'
        STOP 100
      END IF
C : 格子点毎の処理
      DO 1000 J = 1, JMAX
C : ハーフレベル気圧(HPA)計算
        DO 100 K = 1, KMAX-1
      DO 1100 I = 1, IMAX
          PU  = A(K+1) + B(K+1)*GPS(I,J)
          PD  = A(K  ) + B(K  )*GPS(I,J)
C : フルレベル気圧(HPA)計算
! WRFVAR compiles at double precision by default, so DLOG is overkill
!          PFULL(I,K) = DEXP(( PD*DLOG(PD)-PU*DLOG(PU) )/(PD-PU) -1.D0)
          PFULL(I,K) = EXP(( PD*LOG(PD)-PU*LOG(PU) )/(PD-PU) -1.D0)
 1100 CONTINUE
  100   CONTINUE
      DO 1300 I = 1, IMAX
        PFULL(I,KMAX) = (A(KMAX)+B(KMAX)*GPS(I,J))/2.D0
 1300 CONTINUE

C : フルレベル相対湿度(NON-UNIT)計算
        DO 300 K = 1, KMAX
      DO 1400 I = 1, IMAX
          P = PFULL(I,K)
          T = GTMP(I,J,K)
          Q = GWV (I,J,K)

          E = Q*P/(0.378D0*Q+0.622D0)
C         CALL TETEN  (ES,T)
C=====================================================
      TC = T-t_kelvin
      IF (TC.GE.0.D0) THEN
        ES= E0C *  EXP(AL*TC/(BL+TC))
      ELSE IF (TC.LE.-15.D0) THEN
        ES= E0C *  EXP(AI*TC/(BI+TC))
      ELSE
        ES= E0C * (EXP(AL*TC/(BL+TC))*(15.D0+TC)/15.D0 +
     .             EXP(AI*TC/(BI+TC))*(-TC)/15.D0)
      END IF
C=====================================================
          GRH(I,J,K) = E/ES

 1400 CONTINUE
  300   CONTINUE

 1000 CONTINUE

      RETURN
      END SUBROUTINE RELHUM    !shc end
