!
!NCEP_MESO:MODEL_LAYER: BOUNDARY CONDITION UPDATES
!
!----------------------------------------------------------------------
!
      MODULE module_NEST_UTIL
!
!----------------------------------------------------------------------
      USE MODULE_MPP
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_DM
!
!#ifdef DM_PARALLEL
!      INCLUDE "mpif.h"
!#endif
!----------------------------------------------------------------------
      CONTAINS
!
!*********************************************************************************************
      SUBROUTINE NESTBC_PATCH(PD_BXS,PD_BXE,PD_BYS,PD_BYE                                 &
                             ,T_BXS,T_BXE,T_BYS,T_BYE,Q_BXS,Q_BXE,Q_BYS,Q_BYE             &
                             ,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS,V_BXE,V_BYS,V_BYE             &
                             ,Q2_BXS,Q2_BXE,Q2_BYS,Q2_BYE                                 &
                             ,CWM_BXS,CWM_BXE,CWM_BYS,CWM_BYE                             &
                             ,PD_BTXS,PD_BTXE,PD_BTYS,PD_BTYE                             &
                             ,T_BTXS,T_BTXE,T_BTYS,T_BTYE,Q_BTXS,Q_BTXE,Q_BTYS,Q_BTYE     &
                             ,U_BTXS,U_BTXE,U_BTYS,U_BTYE,V_BTXS,V_BTXE,V_BTYS,V_BTYE     &
                             ,Q2_BTXS,Q2_BTXE,Q2_BTYS,Q2_BTYE                             &
                             ,CWM_BTXS,CWM_BTXE,CWM_BTYS,CWM_BTYE                         &
!
                             ,PDTMP_B,TTMP_B, QTMP_B,UTMP_B,VTMP_B,Q2TMP_B,CWMTMP_B       &
                             ,PDTMP_BT,TTMP_BT,QTMP_BT,UTMP_BT,VTMP_BT,Q2TMP_BT,CWMTMP_BT &
!
                             ,SPEC_BDY_WIDTH                                              &  
                             ,IDS,IDE,JDS,JDE,KDS,KDE                                     &
                             ,IMS,IME,JMS,JME,KMS,KME                                     &
                             ,ITS,ITE,JTS,JTE,KTS,KTE                                     )
!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    PATCH       
!   PRGRMMR: gopal 
!     
! ABSTRACT:
!         THIS IS JUST A FIX FOR USING NESTED BOUNDARIES IN THE HALO REGION     
! PROGRAM HISTORY LOG:
!   09-23-2004  : gopal 
!     
! USAGE: CALL PATCH FROM SUBROUTINE SOLVE_RUNSTREAM FOR NESTED DOMAIN ONLY
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH
!
!
      REAL,DIMENSION(IMS:IME,1,SPEC_BDY_WIDTH)                     &
                                           ,INTENT(INOUT) :: PD_BYS,PD_BYE &
                                                          ,PD_BTYS,PD_BTYE

      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BYS,CWM_BYE &
                                                       ,Q_BYS,Q_BYE     &
                                                       ,Q2_BYS,Q2_BYE   &
                                                       ,T_BYS,T_BYE     &
                                                       ,U_BYS,U_BYE     &
                                                       ,V_BYS,V_BYE     

      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BTYS,CWM_BTYE &
                                                       ,Q_BTYS,Q_BTYE     &
                                                       ,Q2_BTYS,Q2_BTYE   &
                                                       ,T_BTYS,T_BTYE     &
                                                       ,U_BTYS,U_BTYE     &
                                                       ,V_BTYS,V_BTYE     

!

      REAL,DIMENSION(JMS:JME,1,SPEC_BDY_WIDTH)                     &
                                           ,INTENT(INOUT) :: PD_BXS,PD_BXE &
                                                          ,PD_BTXS,PD_BTXE

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BXS,CWM_BXE &
                                                       ,Q_BXS,Q_BXE     &
                                                       ,Q2_BXS,Q2_BXE   &
                                                       ,T_BXS,T_BXE     &
                                                       ,U_BXS,U_BXE     &
                                                       ,V_BXS,V_BXE     

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BTXS,CWM_BTXE &
                                                       ,Q_BTXS,Q_BTXE     &
                                                       ,Q2_BTXS,Q2_BTXE   &
                                                       ,T_BTXS,T_BTXE     &
                                                       ,U_BTXS,U_BTXE     &
                                                       ,V_BTXS,V_BTXE     

!

      REAL,DIMENSION(IMS:IME,JMS:JME)                     &
                                      ,INTENT(IN) :: PDTMP_B,PDTMP_BT

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME)                     &
                                      ,INTENT(IN) :: CWMTMP_B,CWMTMP_BT  &
                                                    ,QTMP_B,QTMP_BT     &
                                                    ,Q2TMP_B,Q2TMP_BT   &
                                                    ,TTMP_B,TTMP_BT     &
                                                    ,UTMP_B,UTMP_BT     &
                                                    ,VTMP_B,VTMP_BT    

!

!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY
      INTEGER :: I,J,K,IBDY,II,JJ,IB,JB,IIM,JJM,BF
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      W_BDY=(ITS==IDS)
      E_BDY=(ITE==IDE)
      S_BDY=(JTS==JDS)
      N_BDY=(JTE==JDE)

!----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR WEST; 2 FOR EAST.

!      WRITE(0,*)'WESTERN BC FOR PATCH',IDS,MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
!

      DO IBDY=1,2
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF(W_BDY.AND.IBDY.EQ.1)THEN
!            BF=P_XSB     ! Which boundary (XSB=the boundary where X is at its start)
            IB=1         ! Which cell in from boundary
            II=1         ! Which cell in the domain

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
             IF(MOD(J,2).EQ.1)THEN                 ! J=3,5,7,9
                PD_BXS(J,1,IB)  =PDTMP_B(II,J)
                PD_BTXS(J,1,IB) =PDTMP_BT(II,J)
             ENDIF
          ENDDO
!
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
              IF(MOD(J,2).EQ.1)THEN                  ! J=3,5,7,9
                T_BXS(J,K,IB)    = TTMP_B(II,J,K)
                T_BTXS(J,K,IB)   = TTMP_BT(II,J,K)
                Q_BXS(J,K,IB)    = QTMP_B(II,J,K)
                Q_BTXS(J,K,IB)   = QTMP_BT(II,J,K)
                Q2_BXS(J,K,IB)   = Q2TMP_B(II,J,K)
                Q2_BTXS(J,K,IB)  = Q2TMP_BT(II,J,K)
                CWM_BXS(J,K,IB)  = CWMTMP_B(II,J,K)
                CWM_BTXS(J,K,IB) = CWMTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2).EQ.0)THEN                  ! J=2,4,6,8  
                U_BXS(J,K,IB)    = UTMP_B(II,J,K)
                U_BTXS(J,K,IB)   = UTMP_BT(II,J,K)
                V_BXS(J,K,IB)    = VTMP_B(II,J,K)
                V_BTXS(J,K,IB)   = VTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

        ELSEIF (E_BDY.AND.IBDY.EQ.2) THEN

!            BF=P_XEB     ! Which boundary (XEB=the boundary where X is at its end)
            IB=1         ! Which cell in from boundary
            II=IDE       ! Which cell in the domain

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
             IF(MOD(J,2).EQ.1)THEN                 ! J=3,5,7,9
                PD_BXE(J,1,IB)  =PDTMP_B(II,J)
                PD_BTXE(J,1,IB) =PDTMP_BT(II,J)
             ENDIF
          ENDDO
!
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
              IF(MOD(J,2).EQ.1)THEN                  ! J=3,5,7,9
                T_BXE(J,K,IB)    = TTMP_B(II,J,K)
                T_BTXE(J,K,IB)   = TTMP_BT(II,J,K)
                Q_BXE(J,K,IB)    = QTMP_B(II,J,K)
                Q_BTXE(J,K,IB)   = QTMP_BT(II,J,K)
                Q2_BXE(J,K,IB)   = Q2TMP_B(II,J,K)
                Q2_BTXE(J,K,IB)  = Q2TMP_BT(II,J,K)
                CWM_BXE(J,K,IB)  = CWMTMP_B(II,J,K)
                CWM_BTXE(J,K,IB) = CWMTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2).EQ.0)THEN                  ! J=2,4,6,8  
                U_BXE(J,K,IB)    = UTMP_B(II,J,K)
                U_BTXE(J,K,IB)   = UTMP_BT(II,J,K)
                V_BXE(J,K,IB)    = VTMP_B(II,J,K)
                V_BTXE(J,K,IB)   = VTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
!***  SOUTH AND NORTH BOUNDARIES
!----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR SOUTH; 2 FOR NORTH
!
      DO IBDY=1,2
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF(S_BDY.AND.IBDY.EQ.1) THEN 
!
!            BF=P_YSB     ! Which boundary (YSB=the boundary where Y is at its start)
            JB=1         ! Which cell in from boundary
            JJ=1         ! Which cell in the domain
!
          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYS(I,1,JB) = PDTMP_B(I,JJ)
            PD_BTYS(I,1,JB)= PDTMP_BT(I,JJ)
          ENDDO

!
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYS(I,K,JB)   = TTMP_B(I,JJ,K)
              T_BTYS(I,K,JB)  = TTMP_BT(I,JJ,K)
              Q_BYS(I,K,JB)   = QTMP_B(I,JJ,K)
              Q_BTYS(I,K,JB)  = QTMP_BT(I,JJ,K)
              Q2_BYS(I,K,JB)  = Q2TMP_B(I,JJ,K)
              Q2_BTYS(I,K,JB) = Q2TMP_BT(I,JJ,K)
              CWM_BYS(I,K,JB) = CWMTMP_B(I,JJ,K)
              CWM_BTYS(I,K,JB)= CWMTMP_BT(I,JJ,K)
            ENDDO
          ENDDO

          DO K=KTS,KTE
           DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYS(I,K,JB)   = UTMP_B(I,JJ,K)
              U_BTYS(I,K,JB)  = UTMP_BT(I,JJ,K)
              V_BYS(I,K,JB)   = VTMP_B(I,JJ,K)
              V_BTYS(I,K,JB)  = VTMP_BT(I,JJ,K)
           ENDDO
          ENDDO

          ELSEIF (N_BDY.AND.IBDY.EQ.2) THEN
!            BF=P_YEB      ! Which boundary (YEB=the boundary where Y is at its end)
            JB=1          ! Which cell in from boundary
            JJ=JDE        ! Which cell in the domain

          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYE(I,1,JB) = PDTMP_B(I,JJ)
            PD_BTYE(I,1,JB)= PDTMP_BT(I,JJ)
          ENDDO

!
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYE(I,K,JB)   = TTMP_B(I,JJ,K)
              T_BTYE(I,K,JB)  = TTMP_BT(I,JJ,K)
              Q_BYE(I,K,JB)   = QTMP_B(I,JJ,K)
              Q_BTYE(I,K,JB)  = QTMP_BT(I,JJ,K)
              Q2_BYE(I,K,JB)  = Q2TMP_B(I,JJ,K)
              Q2_BTYE(I,K,JB) = Q2TMP_BT(I,JJ,K)
              CWM_BYE(I,K,JB) = CWMTMP_B(I,JJ,K)
              CWM_BTYE(I,K,JB)= CWMTMP_BT(I,JJ,K)
            ENDDO
          ENDDO

          DO K=KTS,KTE
           DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYE(I,K,JB)   = UTMP_B(I,JJ,K)
              U_BTYE(I,K,JB)  = UTMP_BT(I,JJ,K)
              V_BYE(I,K,JB)   = VTMP_B(I,JJ,K)
              V_BTYE(I,K,JB)  = VTMP_BT(I,JJ,K)
           ENDDO
          ENDDO



        ENDIF
      ENDDO
END  SUBROUTINE NESTBC_PATCH
!----------------------------------------------------------------------------------
SUBROUTINE MSLP_DIAG (MSLP,PINT,T,Q               &
                     ,FIS,PD,DETA1,DETA2,PDTOP    &
                     ,IDS,IDF,JDS,JDF,KDS,KDE     &
                     ,IMS,IME,JMS,JME,KMS,KME     &
                     ,ITS,ITE,JTS,JTE,KTS,KTE     )


!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:  MSLP_DIAG 
!   PRGRMMR: gopal
!
! ABSTRACT:
!         THIS ROUTINE COMPUTES MSLP OVER THE PARENT DOMAIN FOR DIAGONOSTIC PURPOSE 
!
! Note: domain I & J end bounds are NOT the usual bounds.  They are
! IDE-1 and JDE-1.
!
! PROGRAM HISTORY LOG:
!   07-21-2005  : gopal
!   01-23-2012  : sam: removed 3D Z calculation, updated comments
!
! USAGE: CALL MSLP_DIAG FROM THE SOLVER 
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP/Linux cluster
!$$$

      USE MODULE_MODEL_CONSTANTS
      USE MODULE_DM

      IMPLICIT NONE

!     global variables

      INTEGER,INTENT(IN)                                      :: IDS,IDF,JDS,JDF,KDS,KDE   &
                                                                ,IMS,IME,JMS,JME,KMS,KME   & 
                                                                ,ITS,ITE,JTS,JTE,KTS,KTE   

      REAL,                                     INTENT(IN)    :: PDTOP
      REAL, DIMENSION(KMS:KME),                 INTENT(IN)    :: DETA1,DETA2
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(INOUT) :: MSLP
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)    :: FIS,PD
      REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN)    :: PINT,T,Q

!     local variables

      REAL, PARAMETER                                       :: LAPSR=6.5E-3, GI=1./G,D608=0.608
      REAL, PARAMETER                                       :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
      REAL, PARAMETER                                       :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
      REAL                                                  :: RTOPP,APELP,DZ,SFCT,A,Z1,Z2
      INTEGER                                               :: I,J,K
!-----------------------------------------------------------------------------------------------------

     MSLP=-9999.99
     K=1
     DO J = JTS, MIN(JTE,JDF)
       DO I = ITS, MIN(ITE,IDF)
         Z1 = FIS(I,J)*GI
         APELP      = (PINT(I,J,K+1)+PINT(I,J,K))
         RTOPP      = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608)/APELP
         DZ         = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))
         Z2         = Z1 + DZ

         SFCT      = T(I,J,1)*(1.+D608*Q(I,J,1)) + LAPSR*(Z1+Z2)*0.5
         A         = LAPSR*Z1/SFCT
         MSLP(I,J) = PINT(I,J,1)*(1-A)**COEF2
       ENDDO
     ENDDO

END SUBROUTINE MSLP_DIAG
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
SUBROUTINE MOVE_LAND (SM,TSK                      &
                     ,SST,FIS                     &
                     ,PINT,T,Q                    &
                     ,NTSD                        &
                     ,IDS,IDE,JDS,JDE,KDS,KDE     &
                     ,IMS,IME,JMS,JME,KMS,KME     &
                     ,ITS,ITE,JTS,JTE,KTS,KTE,DIRN)

      USE MODULE_MODEL_CONSTANTS
      USE MODULE_DM

      IMPLICIT NONE

!     global variables

      INTEGER,INTENT(IN)                                      :: NTSD,DIRN

      INTEGER,INTENT(IN)                                      :: IDS,IDE,JDS,JDE,KDS,KDE   &
                                                                ,IMS,IME,JMS,JME,KMS,KME   &
                                                                ,ITS,ITE,JTS,JTE,KTS,KTE

      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(INOUT) :: SM,TSK
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)    :: SST,FIS
      REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN)    :: PINT,T,Q

!     local variables

      INTEGER                                               :: I,J,K,CNT
      REAL                                                  :: PSFC,EXNSFC,CAPA,SUMTHS,AVGTHS
!-----------------------------------------------------------------------------------------------------
!
!     Note: make appropriate changes for boundary condition updates in
!     d02 and d03 by adding "i01rhd=(DownNear)f=(BdyNear)" for SM in the
!     Registry.NMM_HWRF. Also module_BNDRY_COND.F needs to be updated
!     for SM. This is subashini's doing for advecting land surface in
!     idealized framework
!
     IF(DIRN == 1) THEN
      DO J = MAX(JTS,2), MIN(JTE,JDE-1)
       DO I = MIN(ITE,IDE),MAX(ITS,2),-1
           SM(I,J)=SM(I-1,J)          ! Motion of land (0) from West to East
       ENDDO
      ENDDO
     ELSE 
      DO J = MAX(JTS,2), MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
           SM(I,J)=SM(I+1,J)          ! Motion of land (0) from East to west
       ENDDO
      ENDDO
     ENDIF

END SUBROUTINE MOVE_LAND
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
!------------------------------------------------------------------------------------------------------
SUBROUTINE CALC_BEST_MSLP(BEST_MSLP,MSLP,MEMBRANE_MSLP,FIS &
                         ,IDS,IDE,JDS,JDE,KDS,KDE     &
                         ,IMS,IME,JMS,JME,KMS,KME     &
                         ,ITS,ITE,JTS,JTE,KTS,KTE     )
  ! Author: Sam Trahan, January 2014

  ! Calculates a "best estimate" BEST_MSLP from the low-quality MSLP
  ! (updated every timestep) and high-quality MEMBRANE_MSLP (updated
  ! infrequently).  The MSLP is generally bad over high or sharp
  ! terrain.
  ! Cases:
  !   invalid membrane_mslp: use mslp (leading edge of nest after move)
  !   height>200m: use membrane_mslp, which is better over terrain
  !   height<=0m:  use mslp, which should be identical to membrane_mslp here
  !   0<height<200m: linearly interpolate between mslp at 0m and
  !                  membrane_mslp at 200m to allow a smooth transition
  use module_model_constants, only: g
  integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE     &
                        ,IMS,IME,JMS,JME,KMS,KME     &
                        ,ITS,ITE,JTS,JTE,KTS,KTE
  real, dimension(ims:ime,jms:jme), intent(in) :: MSLP,MEMBRANE_MSLP,FIS
  real, dimension(ims:ime,jms:jme), intent(out) :: BEST_MSLP
  integer :: i,j
  real :: z,w
  real, parameter :: gi=1./g
  
  do j=max(jts,jds), min(jte,jde-1)
     do i=max(its,ids), min(ite,ide-1)
        if(membrane_mslp(i,j)<7e4) then
           best_mslp(i,j)=mslp(i,j)
        else
           z=fis(i,j)*gi
           if(z<200.) then
              w=max(0.,z)/200.
              best_mslp(i,j)=membrane_mslp(i,j)*w+mslp(i,j)*(1-w)
           else
              best_mslp(i,j)=membrane_mslp(i,j)
           endif
        endif
     enddo
  enddo
END SUBROUTINE CALC_BEST_MSLP
!------------------------------------------------------------------------------------------------------

END  MODULE module_NEST_UTIL
