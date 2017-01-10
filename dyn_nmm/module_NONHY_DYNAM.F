!----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: NONHYDROSTATIC DYNAMICS ROUTINES
!
!----------------------------------------------------------------------
!
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!
!----------------------------------------------------------------------
!
      MODULE MODULE_NONHY_DYNAM
!
!----------------------------------------------------------------------
      USE MODULE_MODEL_CONSTANTS
!     USE MODULE_INDX
!----------------------------------------------------------------------
!
      REAL :: CAPA=R_D/CP,RG=1./G,TRG=2.*R_D/G
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE EPS(NTSD,DT,HYDRO,DX,DY,FAD                            &
                    ,AETA1,DETA1,DETA2,PDTOP,PT                               &
                    ,HBM2,HBM3                                          &
                    ,PDSL,PDSLO,PINT,RTOP,PETDT,PDWDT                   &
                    ,DWDT,DWDTMN,DWDTMX                                 &
                    ,FNS,FEW,FNE,FSE                                    &
                    ,T,U,V,W,W_TOT,Q,CWM                                      &
                    ,DEF3D,HDAC,BARO                                    &
                    ,WP,dwdt_damping_lev                                                 &    
                    ,IHE,IHW,IVE,IVW                                    &
                    ,IDS,IDE,JDS,JDE,KDS,KDE                            &
                    ,IMS,IME,JMS,JME,KMS,KME                            &
                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    EPS
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 9?-??-??
!
! ABSTRACT:
!     EPS COMPUTES THE VERTICAL AND HORIZONTAL ADVECTION OF DZ/DT
!
! PROGRAM HISTORY LOG:
!   9?-??-??  JANJIC     - ORIGINATOR
!   00-01-05  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   02-02-07  BLACK      - CONVERTED TO WRF STRUCTURE
!   04-11-22  BLACK      - THREADED
!   05-12-12  BLACK      - CONVERTED FROM IKJ TO IJK
!
! USAGE: CALL EPS FROM SUBROUTINE SOLVE_RUNSTREAM
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!
!     UNIQUE: NONE
!
!     LIBRARY: NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE 
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,DY,PDTOP,PT,WP
! level where starts dwdt damping, read in from namelist
      real,INTENT(IN) :: dwdt_damping_lev
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2,AETA1
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DWDTMN,DWDTMX,DX    &
                                                   ,FAD,HBM2,HBM3       &
                                                   ,PDSL,PDSLO          &
                                                   ,HDAC,BARO
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PETDT,DEF3D
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: CWM         &
                                                           ,FEW,FNE     &
                                                           ,FNS,FSE     &
                                                           ,Q,RTOP      &
                                                           ,U,V
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DWDT     &
                                                              ,PDWDT    &
                                                              ,T
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PINT,W
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT)   :: W_TOT
!
      LOGICAL,INTENT(IN) :: HYDRO
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: NTSHY=2
!
      REAL,PARAMETER :: WGHT=0.35
      REAL,PARAMETER :: SLPD=1500.
      REAL,PARAMETER :: PI=3.14159
!
      INTEGER,DIMENSION(KTS:KTE) :: LA
!
      INTEGER :: I,J,K,LMP,LSLTP,L
!
      REAL,DIMENSION(KTS:KTE) :: B1,B2,B3,C0,CWM_K,DWDT_K,Q_K,RDPP      &
                                ,RTOP_K,T_K,WHY
!
      REAL,DIMENSION(ITS:ITE) :: DPTU_I,PNP1_I,PSTRUP_I
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: TTB,WEW,WNE,WNS,WSE
!
      REAL,DIMENSION(ITS:ITE,KTS:KTE) :: B1_IK,B2_IK,B3_IK,C0_IK        &
                                        ,CWM_IK,DWDT_IK,Q_IK            &
                                        ,RDPP_IK,RTOP_IK,T_IK
!
      REAL,DIMENSION(ITS:ITE,KTS:KTE+1) :: CHI_IK,COFF_IK               &
                                          ,PINT_IK,PSTR_IK,W_IK
!
      REAL :: ADDT,DELP,DETAL,DP,DPDE,DPPL,DPSTR,DPTL,DPTU              &
             ,DWDTT,EPSN,FCT,FFC,GDT,GDT2                               &
             ,PNP,PP1,PSTRDN,PSTRUP,RDP,RDPDN,RDPUP,RDT                 &
             ,TFC,TMP,TTAL,TTFC,HKNE_IJ,HKSE_IJ,ADVEC,ARG,SLTP,PAVG
!
      LOGICAL :: BOT,TOP,DIFFW
!
      CHARACTER(LEN=255) :: message

!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      IF(NTSD<=NTSHY.OR.HYDRO)THEN
!***
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P1,MYIE_P1
          PINT(I,J,KTE+1)=PT
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTS,KTE
          DO J=MYJS_P2,MYJE_P2
          DO I=MYIS_P1,MYIE_P1
            DWDT(I,J,K)=1.
            PDWDT(I,J,K)=1.
          ENDDO
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTE,KTS,-1
          DO J=MYJS_P2,MYJE_P2
          DO I=MYIS_P1,MYIE_P1
            PINT(I,J,K)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)+PINT(I,J,K+1)
          ENDDO
          ENDDO
        ENDDO
!***
        RETURN
!***
      ENDIF
!-----------------------------------------------------------------------
      ADDT=DT
      RDT=1./ADDT
!-----------------------------------------------------------------------
!
!***  TIME TENDENCY
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KTS,KTE
        DO J=MYJS_P1,MYJE_P1
        DO I=MYIS_P1,MYIE_P1
          DWDT(I,J,K)=(W(I,J,K)-DWDT(I,J,K))*HBM2(I,J)*RDT
        ENDDO
        ENDDO
      ENDDO


      IF (DT > 0 .and. WP .ge. 0.001) THEN
         DIFFW=.TRUE.
      ELSE
         DIFFW=.FALSE.
      ENDIF

      IF(DIFFW) THEN

        DO K=KTS,KTE
         DO J=MYJS_P1,MYJE1_P1
          DO I=MYIS_P1,MYIE1_P1
            HKNE_IJ=(DEF3D(I,J,K)+DEF3D(I+IHE(J),J+1,K))
            HKSE_IJ=(DEF3D(I+IHE(J),J-1,K)+DEF3D(I,J,K))
            WNE(I,J)=(W (I+IHE(J),J+1,K)-W (I,J,K))*HKNE_IJ
            WSE(I,J)=(W (I+IHE(J),J-1,K)-W (I,J,K))*HKSE_IJ
          ENDDO 
         ENDDO 

         DO J=MYJS2,MYJE2
          DO I=MYIS,MYIE
              DWDT(I,J,K)= DWDT(I,J,K) - ( WNE (I,J)-WNE (I+IHW(J),J-1) +  &
                                           WSE (I,J)-WSE (I+IHW(J),J+1) ) * &
                                           HDAC(I,J)*HBM2(I,J)*RDT 
          ENDDO 
         ENDDO 
        ENDDO

       ENDIF
!
!-----------------------------------------------------------------------
!***
!***  VERTICAL ADVECTION
!***
!-----------------------------------------------------------------------
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        TTB(I,J)=0.
      ENDDO
      ENDDO
!
      DO K=KTE,KTS+1,-1
!
!$omp parallel do                                                       &
!$omp& private(i,j,ttal)
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        TTAL=(W(I,J,K-1)-W(I,J,K))*PETDT(I,J,K-1)*0.5
        DWDT(I,J,K)=(TTAL+TTB(I,J))                                     &
                   /(DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J))                &
                    +DWDT(I,J,K)
        TTB(I,J)=TTAL
      ENDDO
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        TTB(I,J)=(W(I,J,KTS)-W(I,J,KTS+1))*PETDT(I,J,KTS)*0.5
        DWDT(I,J,KTS)=TTB(I,J)/(DETA1(KTS)*PDTOP+DETA2(KTS)*PDSLO(I,J)) &
                     +DWDT(I,J,KTS)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***
!***  END OF VERTICAL ADVECTION
!***
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***
!***  HORIZONTAL ADVECTION
!***
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpde,i,j,k)
!
      main_horizontal:  DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!***  THE WORKING ARRAYS FOR THE PRIMARY VARIABLES
!-----------------------------------------------------------------------
!
        DO J=MYJS1_P3,MYJE1_P3
        DO I=MYIS_P3,MYIE_P3
          WEW(I,J)=FEW(I,J,K)*(W(I+IVE(J),J,K)-W(I+IVW(J),J,K))
          WNS(I,J)=FNS(I,J,K)*(W(I,J+1,K)-W(I,J-1,K))
        ENDDO
        ENDDO
!
!***    DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!
        DO J=MYJS1_P2,MYJE2_P2
        DO I=MYIS_P2,MYIE1_P2
          WNE(I,J)=FNE(I,J,K)*(W(I+IHE(J),J+1,K)-W(I,J,K))
        ENDDO
        ENDDO
!
        DO J=MYJS2_P2,MYJE1_P2
        DO I=MYIS_P2,MYIE1_P2
          WSE(I,J)=FSE(I,J,K)*(W(I+IHE(J),J-1,K)-W(I,J,K))
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***    ADVECTION OF W
!-----------------------------------------------------------------------
!
        DO J=MYJS3,MYJE3
        DO I=MYIS2,MYIE2
          DPDE=DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J)
          ADVEC=-(WEW(I+IHW(J),J)      +WEW(I+IHE(J),J)           &
                       +WNS(I,J-1)           +WNS(I,J+1)                &
                       +WNE(I+IHW(J),J-1)    +WNE(I,J)                  &
                       +WSE(I,J)             +WSE(I+IHW(J),J+1))        &
                       *FAD(I,J)*2.0*HBM3(I,J)/(DPDE*DT)
          DWDT(I,J,K)= ADVEC + DWDT(I,J,K)
        ENDDO
        ENDDO

      ENDDO main_horizontal

!-----------------------------------------------------------------------
!***
!***  END OF HORIZONTAL ADVECTION
!***
!-----------------------------------------------------------------------

      IF (WP .ge. 0.001) then

        do l=1,KTE
          why(l)=-99.
        enddo
!
        lsltp=0
        SLTP=(PT+AETA1(KTE-1)*PDTOP+PT+AETA1(KTE)*PDTOP)*0.5
        DO L=KTE,2,-1
          PAVG=PT+PDTOP*(AETA1(L)+AETA1(L-1))*0.5
          ARG=( PAVG-SLTP )/SLPD
          if(arg.gt.1.) exit
          why(l)=1.-cos(arg*pi*0.5)**2
          lsltp=l
        enddo


!
      DO l=lsltp,KTE
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
            dwdt(i,j,l)=dwdt(i,j,l)*why(l)
        ENDDO
       ENDDO
      ENDDO

!-----------------------------------------------------------------------
!---taking external mode out--------------------------------------------
!-----------------------------------------------------------------------

       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
          TTB(I,J)=0.
        ENDDO
       ENDDO
!
      DO K=KTS,KTE
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
            DPDE=DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J)
            TTB(I,J)=DPDE*DWDT(I,J,K)+TTB(I,J)
        ENDDO
       ENDDO
      ENDDO
!
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
          TTB(I,J)=TTB(I,J)/(PDSLO(I,J)+PDTOP)
        ENDDO
       ENDDO
!
      DO K=KTS,KTE
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
            DWDT(I,J,K)=DWDT(I,J,K)-TTB(I,J)
        ENDDO
       ENDDO
      ENDDO
     endif

!dumping dwdt at stratsphere, Zhan

      if ( dwdt_damping_lev .ge. 0.001 ) then
        lsltp=0
        SLTP=(PT+AETA1(KTE-1)*PDTOP+PT+AETA1(KTE)*PDTOP)*0.5
        DO L=KTE,2,-1
          PAVG=PT+PDTOP*(AETA1(L)+AETA1(L-1))*0.5
          ARG=( PAVG-SLTP )/dwdt_damping_lev
          if(arg.gt.1.) exit
          why(l)=1.-cos(arg*pi*0.5)**2
          lsltp=l
        enddo
!
      DO l=lsltp,KTE
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
            dwdt(i,j,l)=dwdt(i,j,l)*why(l)
        ENDDO
       ENDDO
      ENDDO
      endif
!dumping dwdt at stratsphere, Zhan

!$omp parallel do                                                       &
!$omp& private(dwdtt,i,j,k)
      DO K=KTS,KTE
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DWDTT=DWDT(I,J,K)
        DWDTT=MAX(DWDTT,DWDTMN(I,J))
        DWDTT=MIN(DWDTT,DWDTMX(I,J))
!
        DWDT(I,J,K)=(DWDTT*RG+1.)*(1.-WP)+PDWDT(I,J,K)*WP
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!---setting dwdt (epsilon at this point) on h points to 1. along bdy ---
!-----------------------------------------------------------------------
      IF (WP .ge. 0.001) THEN

        do K=KTS,KTE
          if (JDS .eq. JTS) then
            do J=jts,jts+1
              do I=its,ite
                DWDT(I,J,K)=1.
              enddo
            enddo
          endif
!
          if (JTE .ge. JDE-2) then
            do j=jte-1,jte
              do i=its,ite
                DWDT(I,J,K)=1.
              enddo
            enddo
          endif
!
          if(ITS .eq. IDS)then
            do j=jts,jte
              do i=its,its+1
                DWDT(I,J,K)=1.
              enddo
            enddo
          endif
!
          if(ITE .ge. IDE-2)then
            do j=jts,jte
              do i=ite-1,ite
                DWDT(I,J,K)=1.
              enddo
            enddo
          endif
        enddo

        ENDIF
 
      GDT=G*DT
      GDT2=GDT*GDT
      FFC=-R_D/GDT2
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(b1_ik,b2_ik,b3_ik,c0_ik,chi_ik,coff_ik,cwm_ik,           &
!$omp&        delp,dppl,dpstr,dptl,dptu_i,dwdt_ik,fct,i,j,k,            &
!$omp&        pint_ik,pnp1_i,pp1,pstr_ik,pstrdn,pstrup_i,q_ik,          &
!$omp&        rdpdn,rdpup,rtop_ik,t_ik,tfc,tmp,ttfc,w_ik)
!


      final_update:  DO J=MYJS3,MYJE3
!
!-----------------------------------------------------------------------
!***  EXTRACT COLUMNS FROM 3-D ARRAYS
!-----------------------------------------------------------------------
!
        DO K=KTS,KTE
        DO I=MYIS2,MYIE2
          CWM_IK(I,K)=CWM(I,J,K)
          DWDT_IK(I,K)=DWDT(I,J,K)
          Q_IK(I,K)=Q(I,J,K)
          RTOP_IK(I,K)=RTOP(I,J,K)
          T_IK(I,K)=T(I,J,K)
        ENDDO
        ENDDO
!
        DO K=KTS,KTE+1
        DO I=MYIS2,MYIE2
          PINT_IK(I,K)=PINT(I,J,K)
          W_IK(I,K)=W(I,J,K)
        ENDDO
        ENDDO
!
        DO I=MYIS2,MYIE2
          PSTR_IK(I,KTE+1)=PT
        ENDDO
!
!-----------------------------------------------------------------------
!
        DO K=KTE,KTS,-1
!
          IF(K==KTE)THEN
            DO I=MYIS2,MYIE2
              DPPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
              RDPP_IK(I,K)=1./DPPL
              DPSTR=DWDT_IK(I,K)*DPPL
              PSTR_IK(I,K)=PT+DPSTR
              PP1=PT+DPSTR
              PNP1_I(I)=(PP1-PINT_IK(I,K))*WGHT+PINT_IK(I,K)
              TFC=Q_IK(I,K)*P608+(1.-CWM_IK(I,K))
              TTFC=-CAPA*TFC+1.
              COFF_IK(I,K)=T_IK(I,K)*TTFC*TFC*DPPL*FFC                  &
                          /((PT+PNP1_I(I))*(PT+PNP1_I(I)))
            ENDDO
          ELSE
            DO I=MYIS2,MYIE2
              DPPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
              RDPP_ik(I,K)=1./DPPL
              DPSTR=DWDT_IK(I,K)*DPPL
              PSTR_IK(I,K)=PSTR_IK(I,K+1)+DPSTR
              PP1=PNP1_I(I)+DPSTR
              PNP=(PP1-PINT_IK(I,K))*WGHT+PINT_IK(I,K)
              TFC=Q_IK(I,K)*P608+(1.-CWM_IK(I,K))
              TTFC=-CAPA*TFC+1.
              COFF_IK(I,K)=T_IK(I,K)*TTFC*TFC*DPPL*FFC                  &
                          /((PNP1_I(I)+PNP)*(PNP+PNP1_I(I)))
              PNP1_I(I)=PNP
            ENDDO 
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
        DO I=MYIS2,MYIE2
!!BUG!!   PSTRUP_I(I)=-(PSTR_IK(I,KTE)-PINT_IK(I,KTE))*COFF_IK(I,KTE)
          PSTRUP_I(I)=-(PSTR_IK(I,KTE+1)+PSTR_IK(I,KTE)                 &
                       -PINT_IK(I,KTE+1)-PINT_IK(I,KTE))*COFF_IK(I,KTE)
        ENDDO
!
!-----------------------------------------------------------------------
        DO K=KTE-1,KTS,-1
!
          IF(K==KTE-1)THEN
            DO I=MYIS2,MYIE2
              RDPDN=RDPP_IK(I,K)
              RDPUP=RDPP_IK(I,K+1)
!
              PSTRDN=-(PSTR_IK(I,K+1)+PSTR_IK(I,K)                      &
                      -PINT_IK(I,K+1)-PINT_IK(I,K))                     &
                      *COFF_IK(I,K)
!
              B1_IK(I,K)=COFF_IK(I,K+1)+RDPUP
              B2_IK(I,K)=(COFF_IK(I,K+1)+COFF_IK(I,K))-(RDPUP+RDPDN)
              B3_IK(I,K)=COFF_IK(I,K)+RDPDN
              C0_IK(I,K)=PSTRUP_I(I)+PSTRDN
              PSTRUP_I(I)=PSTRDN
            ENDDO
          ELSE
            DO I=MYIS2,MYIE2
              RDPDN=RDPP_IK(I,K)
              RDPUP=RDPP_IK(I,K+1)
!
              PSTRDN=-(PSTR_IK(I,K+1)+PSTR_IK(I,K)                      &
                      -PINT_IK(I,K+1)-PINT_IK(I,K))                     &
                      *COFF_IK(I,K)
!
              B1_IK(I,K)=COFF_IK(I,K+1)+RDPUP
              B2_IK(I,K)=(COFF_IK(I,K+1)+COFF_IK(I,K))-(RDPUP+RDPDN)
              B3_IK(I,K)=COFF_IK(I,K)+RDPDN
              C0_IK(I,K)=PSTRUP_I(I)+PSTRDN
              PSTRUP_I(I)=PSTRDN
            ENDDO
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!***  ELIMINATION
!-----------------------------------------------------------------------
!
        DO K=KTE-2,KTS,-1
!
          IF(K>KTS)THEN
            DO I=MYIS2,MYIE2
              TMP=-B1_IK(I,K)/B2_IK(I,K+1)
              B2_IK(I,K)=B3_IK(I,K+1)*TMP+B2_IK(I,K)
              C0_IK(I,K)=C0_IK(I,K+1)*TMP+C0_IK(I,K)
            ENDDO
          ELSE
            DO I=MYIS2,MYIE2
              TMP=-B1_IK(I,K)/B2_IK(I,K+1)
              B2_IK(I,K)=B3_IK(I,K+1)*TMP                               &
                       +(B2_IK(I,KTS)+B3_IK(I,KTS))
              C0_IK(I,K)=C0_IK(I,K+1)*TMP+C0_IK(I,K)
            ENDDO
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!***  BACK SUBSTITUTION
!-----------------------------------------------------------------------
!
        DO K=KTS+1,KTE
!
          IF(K==KTS+1)THEN
            DO I=MYIS2,MYIE2
              CHI_IK(I,K)=C0_IK(I,KTS)/B2_IK(I,KTS)
              CHI_IK(I,KTS)=CHI_IK(I,K)
            ENDDO
          ELSE
            DO I=MYIS2,MYIE2
              CHI_IK(I,K)=(-B3_IK(I,K-1)*CHI_IK(I,K-1)+C0_IK(I,K-1))    &
                          /B2_IK(I,K-1)
            ENDDO
          ENDIF
!
        ENDDO
!-----------------------------------------------------------------------
!
        FCT=0.5/CP
!
        DO K=KTE,KTS,-1
!
          IF(K==KTE)THEN
            DO I=MYIS2,MYIE2
              DPTL=(CHI_IK(I,K)+PSTR_IK(I,K)-PINT_IK(I,K))*HBM3(I,J)
              PINT_IK(I,K)=PINT_IK(I,K)+DPTL
              T_IK(I,K)=DPTL*RTOP_IK(I,K)*FCT+T_IK(I,K)
              DELP=(PINT_IK(I,K)-PINT_IK(I,K+1))*RDPP_IK(I,K)
              W_IK(I,K)=((DELP-DWDT_IK(I,K))*GDT+W_IK(I,K))*HBM3(I,J)
              DWDT_IK(I,K)=(DELP-1.)*HBM3(I,J)+1.
              DPTU_I(I)=DPTL
            ENDDO
          ELSE
            DO I=MYIS2,MYIE2
              DPTL=(CHI_IK(I,K)+PSTR_IK(I,K)-PINT_IK(I,K))*HBM3(I,J)
              PINT_IK(I,K)=PINT_IK(I,K)+DPTL
              T_IK(I,K)=(DPTU_I(I)+DPTL)*RTOP_IK(I,K)*FCT+T_IK(I,K)
              DELP=(PINT_IK(I,K)-PINT_IK(I,K+1))*RDPP_IK(I,K)
              W_IK(I,K)=((DELP-DWDT_IK(I,K))*GDT+W_IK(I,K))*HBM3(I,J)
              DWDT_IK(I,K)=(DELP-1.)*HBM3(I,J)+1.
              DPTU_I(I)=DPTL
            ENDDO
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
        DO K=KTS,KTE
        DO I=MYIS2,MYIE2
          PINT(I,J,K)=PINT_IK(I,K)
          T(I,J,K)=T_IK(I,K)
          W(I,J,K)=W_IK(I,K)
          DWDT(I,J,K)=DWDT_IK(I,K)
        ENDDO
        ENDDO
!-----------------------------------------------------------------------
!
      ENDDO final_update
!
!-----------------------------------------------------------------------
!***  DEFINE OUTPUT W FIELD
!-----------------------------------------------------------------------
!
        IF (WP .ge. 0.001) THEN
            DO K=KTS,KTE+1
            DO J=MYJS,MYJE
            DO I=MYIS,MYIE
               W_TOT(I,J,K)=W(I,J,K)+BARO(I,J)
            ENDDO
            ENDDO
            ENDDO
        ELSE
            DO K=KTS,KTE+1
            DO J=MYJS,MYJE
            DO I=MYIS,MYIE
               W_TOT(I,J,K)=W(I,J,K)
            ENDDO
            ENDDO
            ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE EPS
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE VADZ(NTSD,DT,FIS,SIGMA,DFL,HBM2                        &
                     ,DETA1,DETA2,PDTOP                                 &
                     ,PINT,PDSL,PDSLO,PETDT                             &
                     ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                       &
                     ,IHE,IHW,IVE,IVW                                   &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                           &
                     ,IMS,IME,JMS,JME,KMS,KME                           &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    VADZ        VERTICAL ADVECTION OF HEIGHT
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
!     
! ABSTRACT:
!     VADV CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION
!     OF HEIGHT IN ORDER TO COMPUTE W=DZ/DT DIAGNOSTICALLY
!     
! PROGRAM HISTORY LOG:
!   96-05-??  JANJIC     - ORIGINATOR
!   00-01-04  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   01-03-26  BLACK      - CONVERTED TO WRF STRUCTURE
!   02-02-19  BLACK      - CONVERSION UPDATED
!   04-11-22  BLACK      - THREADED
!   05-12-12  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL VADZ FROM MAIN PROGRAM
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!
!     UNIQUE: NONE
!
!     LIBRARY: NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
#ifdef  AS_RECEIVED
      LOGICAL,INTENT(IN) :: SIGMA
#else
      INTEGER,INTENT(IN) :: SIGMA
#endif
!
      INTEGER,INTENT(IN) :: NTSD
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!
      REAL,INTENT(IN) :: DT,PDTOP
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: DFL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PDSL,PDSLO
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: CWM,Q       &
                                                           ,RTOP,T
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: PDWDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DWDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: W,Z
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER :: I,J,K
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: FNE,FSE,TTB
!
      REAL :: DZ,RDT,TTAL,ZETA
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      RDT=1./DT
!
      DO K=KTS,KTE
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          PDWDT(I,J,K)=DWDT(I,J,K)
          DWDT(I,J,K)=W(I,J,K)
        ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        W(I,J,KTS)=0.
!
#ifdef AS_RECEIVED
        IF(SIGMA)THEN
#else
        IF(SIGMA==1)THEN
#endif
          Z(I,J,KTS)=FIS(I,J)*RG
        ELSE
          Z(I,J,KTS)=0.
        ENDIF
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dz,i,j,k,zeta)
!-----------------------------------------------------------------------
!
      kloop1 : DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
!
          ZETA=DFL(K+1)*RG
          DZ=(Q(I,J,K)*P608-CWM(I,J,K)+1.)*T(I,J,K)                     &
            /(PINT(I,J,K+1)+PINT(I,J,K))                                &
            *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*TRG
          Z(I,J,K+1)=Z(I,J,K)+DZ
          W(I,J,K+1)=(DZ-RTOP(I,J,K)                                    &
                    *(DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J))*RG)           &
                    *HBM2(I,J)                                          &
                    +W(I,J,K)
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO kloop1
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
!-----------------------------------------------------------------------
      DO K=KTS,KTE
!
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Z(I,J,K)=(Z(I,J,K+1)+Z(I,J,K))*0.5
          W(I,J,K)=(W(I,J,K+1)+W(I,J,K))*HBM2(I,J)*0.5*RDT
        ENDDO
        ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TTB(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k,ttal)
!-----------------------------------------------------------------------
      DO K=KTE,KTS+1,-1
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          TTAL=(Z(I,J,K-1)-Z(I,J,K))*PETDT(I,J,K-1)*0.5
          W(I,J,K)=(TTAL+TTB(I,J))/(DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J)) &
                  +W(I,J,K)
          TTB(I,J)=TTAL
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
!-----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        W(I,J,KTS)=TTB(I,J)/(DETA1(KTS)*PDTOP+DETA2(KTS)*PDSLO(I,J))    &
                  +W(I,J,KTS)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE VADZ
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE HADZ(NTSD,DT,HYDRO,HBM2,DETA1,DETA2,PDTOP              &
                     ,DX,DY,FAD                                         &
                     ,FEW,FNS,FNE,FSE                                   &
                     ,PDSL,U,V,W,Z,WP,BARO                              &
                     ,IHE,IHW,IVE,IVW                                   &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                           &
                     ,IMS,IME,JMS,JME,KMS,KME                           &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    HADZ        HORIZONTAL ADVECTION OF HEIGHT
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-05-??       
!     
! ABSTRACT:
!     HADZ CALCULATES DIAGNOSTICALLY THE CONTRIBUTION OF
!     THE HORIZONTAL ADVECTION OF HEIGHT
!     
! PROGRAM HISTORY LOG:
!   96-05-??  JANJIC     - ORIGINATOR
!   00-01-04  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   01-03-26  BLACK      - CONVERTED TO WRF STRUCTURE
!   04-11-22  BLACK      - THREADED
!   05-12-12  BLACK      - CONVERTED FROM IKJ TO IJK
!
! USAGE: CALL HADZ FROM MAIN PROGRAM
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT FILES:
!
!   SUBPROGRAMS CALLED:
!
!     UNIQUE: NONE
!
!     LIBRARY: NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: HYDRO
!
      INTEGER,INTENT(IN) :: NTSD
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!
      REAL,INTENT(IN) :: DT,DY,PDTOP,WP
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,FAD,HBM2,PDSL

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: BARO
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: U,V
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: FEW,FNE    &
                                                            ,FNS,FSE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: W
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER,PARAMETER :: NTSHY=2
!
      INTEGER :: I,J,K
!
      REAL :: FEWP,FNEP,FNSP,FSEP,UDY,VDX
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: DPDE,UNED,USED         &
     &                                          ,ZEW,ZNE,ZNS,ZSE,TTB
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      IF(NTSD+1<=NTSHY.OR.HYDRO)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTS,KTE
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            W(I,J,K)=0.
          ENDDO
          ENDDO
        ENDDO
!***
        RETURN
!***
      ENDIF
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  FIRST ZERO OUT SOME WORKING ARRAYS
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        ZEW(i,j)=0
        ZNE(i,j)=0
        ZNS(i,j)=0
        ZSE(i,j)=0
        TTB(i,j)=0
        DPDE(I,J)=0.
        UNED(I,J)=0.
        USED(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dpde,fewp,fnep,fnsp,fsep,i,j,udy,uned,used,vdx           &
!$omp&        ,zew,zne,zns,zse)
!-----------------------------------------------------------------------
!
      main_integration:  DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
!***  MASS FLUXES AND MASS POINT ADVECTION COMPONENTS
!
!-----------------------------------------------------------------------
!
        DO J=MYJS_P3,MYJE_P3
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
        ENDDO
        ENDDO
!
        DO J=MYJS1_P3,MYJE1_P3
        DO I=MYIS_P3,MYIE_P3
          UDY=U(I,J,K)*DY
          VDX=V(I,J,K)*DX(I,J)
!
          FEWP=UDY*(DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J))
          FNSP=VDX*(DPDE(I,J-1)+DPDE(I,J+1))
!
          FEW(I,J,K)=FEWP
          FNS(I,J,K)=FNSP
!
          ZEW(I,J)=FEWP*(Z(I+IVE(J),J,K)-Z(I+IVW(J),J,K))
          ZNS(I,J)=FNSP*(Z(I,J+1,K)-Z(I,J-1,K))
!
          UNED(I,J)=UDY+VDX
          USED(I,J)=UDY-VDX
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
!***  DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!
!-----------------------------------------------------------------------
        DO J=MYJS1_P2,MYJE2_P2
        DO I=MYIS_P2,MYIE1_P2
          FNEP=(UNED(I+IHE(J),J)+UNED(I,J+1))                           &
     &        *(DPDE(I,J)+DPDE(I+IHE(J),J+1))
          FNE(I,J,K)=FNEP
          ZNE(I,J)=FNEP*(Z(I+IHE(J),J+1,K)-Z(I,J,K))
        ENDDO
        ENDDO
!
        DO J=MYJS2_P2,MYJE1_P2
        DO I=MYIS_P2,MYIE1_P2
          FSEP=(USED(I+IHE(J),J)+USED(I,J-1))                           &
     &        *(DPDE(I,J)+DPDE(I+IHE(J),J-1))
          FSE(I,J,K)=FSEP
          ZSE(I,J)=FSEP*(Z(I+IHE(J),J-1,K)-Z(I,J,K))
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
!***  ADVECTION OF Z
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1

          W(I,J,K)=-(ZEW(I+IHW(J),J)  +ZEW(I+IHE(J),J)                  &
                    +ZNS(I,J-1)       +ZNS(I,J+1)                       &
                    +ZNE(I+IHW(J),J-1)+ZNE(I,J)                         &
                    +ZSE(I,J)         +ZSE(I+IHW(J),J+1))               &
                    *FAD(I,J)*2.0*HBM2(I,J)/(DPDE(I,J)*DT)              &
                    +W(I,J,K)

        ENDDO
        ENDDO

      ENDDO main_integration

      IF (WP .ge. 0.001) then
!-----------------------------------------------------------------------
!---taking external mode out--------------------------------------------
!-----------------------------------------------------------------------
	
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
          BARO(I,J)=0.
        ENDDO
       ENDDO
!
      DO K=KTS,KTE
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
            DPDE(I,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
            BARO(I,J)=(DPDE(I,J))*W(I,J,K)+BARO(I,J)
        ENDDO
       ENDDO
      ENDDO
!
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
          BARO(I,J)=BARO(I,J)/(PDSL(I,J)+PDTOP)
        ENDDO
       ENDDO
!

      DO K=KTS,KTE
       DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
            W(I,J,K)=W(I,J,K)-BARO(I,J)
        ENDDO
       ENDDO
      ENDDO
 
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE HADZ
!
!-----------------------------------------------------------------------
!
      END MODULE MODULE_NONHY_DYNAM
!
!-----------------------------------------------------------------------
