!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_read_nmm ( grid , config_flags , ntsd, dt_from_file, tstart_from_file, tend_from_file, &
!
#include <nmm_dummy_args.inc>
!
     )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
!----------------------------------------------------------------------

   IMPLICIT NONE

!----------------------------------------------------------------------

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

#include <nmm_dummy_decl.inc>

!----------------------------------------------------------------------
  ! Local

   REAL, DIMENSION(1:2*NMM_MAX_DIM,2)           :: PDB
   REAL, DIMENSION(1:2*NMM_MAX_DIM,grid%sd32:grid%ed32-1,2) :: TB,QB,UB,VB,Q2B,CWMB

   INTEGER :: NUNIT_PARMETA=10,NUNIT_FCSTDATA=11                  &
             ,NUNIT_NHB=12,NUNIT_CO2=14,NUNIT_Z0=22
   INTEGER :: NMAP,NRADSH,NRADLH,NTDDMP
   INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE
   INTEGER :: IPS,IPE,JPS,JPE,KPS,KPE
   INTEGER :: IMS,IME,JMS,JME,KMS,KME
   INTEGER :: IM,JM,LM,NROOT,INPES,JNPES,NFCST,NUNIT_NBC,LISTB
   INTEGER :: I,J,K,IHRST,JAM,NTSD,IHRSTB,IHH,IHL
   INTEGER :: KBI,KBI2,LRECBC
   INTEGER :: N,ISTART,LB,NREC
   INTEGER,DIMENSION(3) :: IDAT,IDATB
   LOGICAL :: RESTRT,SINGLRST,NEST,RUN,RUNB
   REAL :: TSTART,TEND,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC
   REAL :: BCHR,TSTEPS,TSPH,TBOCO
   REAL,DIMENSION(39) :: SPL
   REAL,DIMENSION(99) :: TSHDE
   REAL,ALLOCATABLE,DIMENSION(:) :: TEMP1
   REAL,ALLOCATABLE,DIMENSION(:,:) :: TEMP
   INTEGER,ALLOCATABLE,DIMENSION(:,:) :: ITEMP
   REAL,ALLOCATABLE,DIMENSION(:,:,:) :: HOLD
   REAL ::                      TDDAMP                     &
                                            ,ETA
   REAL :: PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
   REAL :: ROS,CS,DS,ROI,CI,DI                             &
          ,PL,THL,RDQ,RDTH,RDP,RDTHE                       &
                ,QS0,SQS,STHE,THE0
!!!tlb   REAL :: PTBL,TTBL                                       &
   REAL :: WBD,SBD,TLM0D,TPH0D,R,        CMLD,DP30               &
    ,X1P,Y1P,IXM,IYM
   INTEGER :: NN, mype
   REAL :: dt_from_file
   REAL :: tstart_from_file, tend_from_file
   real :: dtx
#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif




!**********************************************************************
!
!***  Temporary fix for reading in lookup tables
!
   INTEGER,PARAMETER :: ITB=76,JTB=134,ITBQ=152,JTBQ=440
   REAL,DIMENSION(ITB,JTB) :: PTBL
   REAL,DIMENSION(JTB,ITB) :: TTBL
   REAL,DIMENSION(JTBQ,ITBQ) :: TTBLQ
!**********************************************************************
   CHARACTER*256 mess
!----------------------------------------------------------------------
! small file with global dimensions
   NAMELIST /PARMNMM/ IM,JM,LM,NSOIL,NROOT,INPES,JNPES
!
! another small file with forecast parameters
   NAMELIST /FCSTDATA/                                            &
    TSTART,TEND,RESTRT,SINGLRST,NMAP,TSHDE,SPL                    &
   ,NPHS,NCNVC,NRADSH,NRADLH,NTDDMP                               &
   ,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC                           &
   ,NEST,HYDRO
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
#include "deref_kludge.h"
#define COPY_IN
#include <nmm_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    include <nmm_data_calls.inc>
#endif

!
   REWIND NUNIT_PARMETA
   READ(NUNIT_PARMETA,PARMNMM)
   NSOIL=4
      write(0,*)' assigned nsoil=',nsoil
   CALL wrf_debug ( 100 , 'nmm: read global dimensions file' )

! temporarily produce array limits here
!   IDS=1
!   IDE=IM
!   JDS=1
!   JDE=JM
!   KDS=1
!   KDE=LM

!----------------------------------------------------------------------
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

! GLOBAL GRID DIMS ARE WHAT WRF CONSIDERS UNSTAGGERED
   ide = ide - 1
   jde = jde - 1
   kde = kde - 1
   NSOIL=4

   CALL wrf_debug(100,'in mediation_read_nmm')
   WRITE(mess,*)'ids,ide,jds,jde,kds,kde ',ids,ide,jds,jde,kds,kde
   CALL wrf_debug(100,mess)

!----------------------------------------------------------------------
! read constants file
      write(0,*)' before allocates and nhb nsoil=',nsoil
   ALLOCATE(TEMP1(1:NSOIL),STAT=I)
   ALLOCATE(ITEMP(IDS:IDE,JDS:JDE),STAT=I)
   ALLOCATE(TEMP(IDS:IDE,JDS:JDE),STAT=I)
   ALLOCATE(HOLD(IDS:IDE,JDS:JDE,KDS:KDE),STAT=I)
!
!----------------------------------------------------------------------
! read z0 file
      READ(NUNIT_Z0)TEMP
      DO J=JDS,JDE
      DO I=IDS,IDE
        Z0(I,J)=TEMP(I,J)
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!
   READ(NUNIT_NHB) NFCST,NUNIT_NBC,LISTB,DT,IDTAD,SIGMA
      write(0,*)' read_nmm sigma=',sigma
   dt_from_file = dt
   WRITE( mess, * ) 'NFCST = ',NFCST,'  DT  = ',DT
   WRITE( 0, * ) 'NFCST = ',NFCST,'  DT  = ',DT,' NHB=',NUNIT_NHB 
   CALL wrf_debug(100, mess)
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       LMH(I,J)=ITEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       LMV(I,J)=ITEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       HBM2(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   DO J=JDS,JDE
     DO I=IDS,IDE
       HBM3(I,J)=0.
     ENDDO
   ENDDO
!
   DO J=JDS,JDE
     IHWG(J)=MOD(J+1,2)-1
     IF(J.GE.JDS+3.AND.J.LE.JDE-3)THEN
       IHL=2-IHWG(J)
!      IHWG=MOD(J+1,2)-1
!      IHL=2-IHWG
       IHL=2-IHWG(J)
       IHH=IDE-2
       DO I=IDS,IDE
         IF(I.GE.IHL.AND.I.LE.IHH)HBM3(I,J)=1.
       ENDDO
     ENDIF
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       VBM2(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       VBM3(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       SM(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       SICE(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NUNIT_NHB)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read HTM into HOLD' )
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         HTM(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read of record' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NUNIT_NHB)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read VTM into HOLD' )
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         VTM(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read VTM' )
!----------------------------------------------------------------------
   JAM=6+2*(JDE-JDS-9)
   READ(NUNIT_NHB)DY_NMM,CPGFV,EN,ENT,R,PT,TDDAMP                 &
                 ,F4D,F4Q,EF4T,PDTOP                              &
                 ,(DETA(KME-K),K=KMS,KME-1)                       &
                 ,(AETA(KME-K),K=KMS,KME-1)                       &
                 ,(F4Q2(KME-K),K=KMS,KME-1)                       &
                 ,(ETAX(KME+1-K),K=KMS,KME)                       &
                 ,(DFL(KME+1-K),K=KMS,KME)                        &
                 ,(DETA1(KME-K),K=KMS,KME-1)                      &
                 ,(AETA1(KME-K),K=KMS,KME-1)                      &
                 ,(ETA1(KME+1-K),K=KMS,KME)                       &
                 ,(DETA2(KME-K),K=KMS,KME-1)                      &
                 ,(AETA2(KME-K),K=KMS,KME-1)                      &
                 ,(ETA2(KME+1-K),K=KMS,KME)                       &
                 ,(EM(K),K=1,JAM)                                 &
                 ,(EMT(K),K=1,JAM)
   CALL wrf_debug ( 100 , 'nmm: read NMM_DX_NMM' )
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       DX_NMM(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_WPDAR' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       WPDAR(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_CPGFU' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       CPGFU(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_CURV' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       CURV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_FCP' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       FCP(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   CALL wrf_debug ( 100 , 'nmm: read NMM_FDIV' )
   DO J=JDS,JDE
     DO I=IDS,IDE
       FDIV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   CALL wrf_debug ( 100 , 'nmm: read NMM_FAD' )
   DO J=JDS,JDE
     DO I=IDS,IDE
       FAD(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_F' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       F(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_DDMPU' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       DDMPU(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_DDMPV' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       DDMPV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_GLAT' )
   READ(NUNIT_NHB) PT, TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       GLAT(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read NMM_GLON' )
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       GLON(I,J)=-TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q' )
   READ(NUNIT_NHB)PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
!                ,(STHEQ(K),K=1,ITBQ)                             &
!                ,(THE0Q(K),K=1,ITBQ)
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read ROS,CS,DS,ROI,CI,DI' )
   READ(NUNIT_NHB)ROS,CS,DS,ROI,CI,DI                             &
           ,PL,THL,RDQ,RDTH,RDP,RDTHE                             &
           ,(DETA(KME-K),K=KMS,KME-1)                             &
           ,(AETA(KME-K),K=KMS,KME-1)                             &
           ,(DFRLG(KME+1-K),K=KMS,KME)                            &
           ,(DETA1(KME-K),K=KMS,KME-1)                            &
           ,(AETA1(KME-K),K=KMS,KME-1)                            &
           ,(DETA2(KME-K),K=KMS,KME-1)                            &
           ,(AETA2(KME-K),K=KMS,KME-1)                            &
           ,QS0,SQS,STHE,THE0
!          ,(QS0(K),K=1,JTB)                                      &
!          ,(SQS(K),K=1,JTB)                                      &
!          ,(STHE(K),K=1,ITB)                                     &                
!          ,(THE0(K),K=1,ITB)
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       MXSNAL(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       EPSR(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       TG(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       GFFC(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       SST(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       ALBASE(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       HDAC(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       HDACV(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
!!!tlb   READ(NUNIT_NHB) TEMP
    READ(NUNIT_NHB) TTBLQ
!   DO J=JDS,JDE
!     DO I=IDS,IDE
!       TTBLQ(I,J)=TEMP(I,J)
!     ENDDO
!   ENDDO
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read PTBL,TTBL' )
        READ(NUNIT_NHB)PTBL,TTBL                                       &
                ,R,PT,TSPH                                             &
                ,WBD,SBD,TLM0D,TPH0D,DLMD,DPHD,CMLD,DP30               &
                ,X1P,Y1P,IXM,IYM                                       &
                ,(DETA(KME-K),K=KMS,KME-1)                             &
                ,(AETA(KME-K),K=KMS,KME-1)                             &
                ,(ETAX(KME+1-K),K=KMS,KME)                             &
                ,(DETA1(KME-K),K=KMS,KME-1)                            &
                ,(AETA1(KME-K),K=KMS,KME-1)                            &
                ,(ETA1(KME+1-K),K=KMS,KME)                             &
                ,(DETA2(KME-K),K=KMS,KME-1)                            &
                ,(AETA2(KME-K),K=KMS,KME-1)                            &
                ,(ETA2(KME+1-K),K=KMS,KME)                              
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
    DO J=JDS,JDE
      DO I=IDS,IDE
        IVGTYP(I,J)=ITEMP(I,J)
      ENDDO
    ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
    DO J=JDS,JDE
      DO I=IDS,IDE
        ISLTYP(I,J)=ITEMP(I,J)
      ENDDO
    ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) ITEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       ISLOPE(I,J)=ITEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       VEGFRC(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NUNIT_NHB) (SLDPTH(N),N=1,NSOIL)
!----------------------------------------------------------------------
   READ(NUNIT_NHB) (RTDPTH(N),N=1,NSOIL)
!----------------------------------------------------------------------
   CALL wrf_debug ( 100 , 'nmm: read constants file' )

   REWIND NUNIT_FCSTDATA
   READ(NUNIT_FCSTDATA,FCSTDATA)
   tstart_from_file = tstart
   tend_from_file   = tend
   CALL wrf_debug ( 100 , 'nmm: read forecast parameters file' )
!----------------------------------------------------------------------

   nrads = nint(nradsh*tsph)
   nradl = nint(nradlh*tsph)
!----------------------------------------------------------------------
!
! INITIAL CONDITIONS
!
!----------------------------------------------------------------------
   REWIND NFCST
   READ(NFCST)RUN,IDAT,IHRST,NTSD
   IF(NTSD.EQ.1)NTSD=0
!----------------------------------------------------------------------
   READ(NFCST) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       PD(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NFCST) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       RES(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
!----------------------------------------------------------------------
   READ(NFCST) TEMP
   DO J=JDS,JDE
     DO I=IDS,IDE
       FIS(I,J)=TEMP(I,J)
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read FIS' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read U into HOLD' )
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         U(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read U' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         V(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read V' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         T(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read T' )
!----------------------------------------------------------------------
   DO K=KDE,KDS,-1
     READ(NFCST)((HOLD(I,J,K),I=IDS,IDE),J=JDS,JDE)
   ENDDO
   DO K=KDS,KDE
     DO J=JDS,JDE
       DO I=IDS,IDE
         Q(I,K,J)=HOLD(I,J,K)
       ENDDO
     ENDDO
   ENDDO
   CALL wrf_debug ( 100 , 'nmm: read Q' )
!----------------------------------------------------------------------
   READ(NFCST)((SI(I,J),I=IDS,IDE),J=JDS,JDE)
   READ(NFCST)((SNO(I,J),I=IDS,IDE),J=JDS,JDE)
!  READ(NFCST)(((SMC(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   READ(NFCST)(((hold(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
      do k=1,nsoil
        do j=jds,jde
        do i=ids,ide
          smc(i,k,j)=hold(i,j,k)
        enddo
        enddo
      enddo
   READ(NFCST)((CMC(I,J),I=IDS,IDE),J=JDS,JDE)
!  READ(NFCST)(((STC(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   READ(NFCST)(((hold(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   do k=1,nsoil
     do j=jds,jde
     do i=ids,ide
       stc(i,k,j)=hold(i,j,k)
     enddo
     enddo
   enddo
!  READ(NFCST)(((SH2O(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   READ(NFCST)(((hold(I,J,N),I=IDS,IDE),J=JDS,JDE),N=1,NSOIL)
   do k=1,nsoil
     do j=jds,jde
     do i=ids,ide
       sh2o(i,k,j)=hold(i,j,k)
!      sh2o(i,k,j)=0.05
     enddo
     enddo
   enddo
   CALL wrf_debug ( 100 , 'nmm: read initial conditions file' )


!!!!!!!!!!!!!!!!!!!!!!!!!!
ENTRY med_read_nmm_bdy ( grid , config_flags , ntsd , dt_from_file, tstart_from_file, tend_from_file, &
!
#include <nmm_dummy_args.inc>
!
     )
!!!!!!!!!!!!!!!!!!!!!!!!!!



!----------------------------------------------------------------------
!***  READ BOUNDARY CONDITIONS.
!----------------------------------------------------------------------
!
      DT = dt_from_file
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

! GLOBAL GRID DIMS ARE WHAT WRF CONSIDERS UNSTAGGERED
   ide = ide - 1
   jde = jde - 1
   kde = kde - 1
   NSOIL=4

   CALL wrf_debug(100,'in mediation_read_nmm')
   WRITE(mess,*)'ids,ide,jds,jde,kds,kde ',ids,ide,jds,jde,kds,kde
   CALL wrf_debug(100,mess)

      mype = 0
      IF(MYPE.EQ.0)THEN
        IF(NEST)THEN
          KBI=2*IM+JM-3
          KBI2=KBI-4
#ifdef DEC_ALPHA
          LRECBC=(1+(1+6*LM)*KBI*2+(KBI+KBI2)*(LM+1))
#else
          LRECBC=4*(1+(1+6*LM)*KBI*2+(KBI+KBI2)*(LM+1))
#endif
          OPEN(UNIT=NUNIT_NBC,ACCESS='DIRECT',RECL=LRECBC)
          read(nunit_nbc,rec=2) bchr
        ENDIF
!
        IF(.NOT.NEST)REWIND NUNIT_NBC
!
#ifdef DP_REAL
        IF(NEST)THEN
          READ(NUNIT_NBC,REC=1)RUNBX,IDATBX,IHRSTBX,TBOCO
        ELSE
          READ(NUNIT_NBC)RUNBX,IDATBX,IHRSTBX,TBOCO
        ENDIF
!
        RUNB=RUNBX
        IDATB=IDATBX
        IHRSTB=IHRSTBX
#else
        IF(NEST)THEN
          READ(NUNIT_NBC,REC=1)RUNB,IDATB,IHRSTB,TBOCO
        ELSE
          READ(NUNIT_NBC)RUNB,IDATB,IHRSTB,TBOCO
        ENDIF
#endif
      ENDIF
!
!      CALL MPI_BCAST(RUNB,1,MPI_LOGICAL,0,MPI_COMM_COMP,IRTN)
!      CALL MPI_BCAST(IDATB,3,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
!      CALL MPI_BCAST(IHRSTB,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
!      CALL MPI_BCAST(TBOCO,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
!
!      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      ISTART=NINT(TSTART)
      LB=2*(IDE-IDS+1)+(JDE-JDS+1)-3
!

      IF(MYPE.EQ.0.AND..NOT.NEST)THEN
!
        READ(NUNIT_NBC)BCHR
  205   READ(NUNIT_NBC)((PDB(N,I),N=1,LB),I=1,2)
        READ(NUNIT_NBC)(((TB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((QB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((UB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((VB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((Q2B(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
        READ(NUNIT_NBC)(((CWMB(N,K,I),N=1,LB),K=KDE,KDS,-1),I=1,2)
!
        IF(ISTART.EQ.NINT(BCHR))THEN
          IF(ISTART.GT.0)READ(NUNIT_NBC)BCHR
          GO TO 215
        ELSE
          READ(NUNIT_NBC)BCHR
        ENDIF
!
      write(0,*)' read_nmm istart=',istart,' bchr=',bchr,' tsph=',tsph
        IF(ISTART.GE.NINT(BCHR))THEN
          GO TO 205
        ELSEIF(ISTART.LT.NINT(BCHR))THEN
          TSTEPS=ISTART*TSPH
!
          DO N=1,LB
      if(n==5.or.n==6)then
        write(0,*)' read_nmm i=',i,' pdb(1)=',pdb(n,1),' pdb(2)=',pdb(n,2),' dt=',dt,' tsteps=',tsteps
      endif
            PDB(N,1)=PDB(N,1)+PDB(N,2)*DT*TSTEPS
          ENDDO
!
          DO K=1,LM
          DO N=1,LB
            TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT*TSTEPS
            QB(N,K,1)=QB(N,K,1)+QB(N,K,2)*DT*TSTEPS
            UB(N,K,1)=UB(N,K,1)+UB(N,K,2)*DT*TSTEPS
            VB(N,K,1)=VB(N,K,1)+VB(N,K,2)*DT*TSTEPS
            Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT*TSTEPS
            CWMB(N,K,1)=CWMB(N,K,1)+CWMB(N,K,2)*DT*TSTEPS
          ENDDO
          ENDDO
          GO TO 215
        ENDIF
      ENDIF
!
      IF(MYPE.EQ.0.AND.NEST)THEN
        NREC=1
!
  210   NREC=NREC+1
        READ(NUNIT_NBC,REC=NREC)BCHR
!
        IF(ISTART.EQ.NINT(BCHR))THEN
!!!!!     IF(ISTART.GT.0)READ(NUNIT_NBC,REC=NREC+1)BCHR
          GO TO 215
        ELSE
          GO TO 210
        ENDIF
      ENDIF
!
  215 CONTINUE

      IF(NTSD.EQ.0)THEN
        IF(MYPE.EQ.0.AND..NOT.NEST.AND.ISTART.GE.NINT(BCHR))THEN
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
          BACKSPACE NUNIT_NBC
!          WRITE(LIST,*)'  BACKSPACE UNIT NBC=',NUNIT_NBC
        ENDIF
      ENDIF

      IF(MYPE.EQ.0.AND.NEST)THEN
          NREC=NINT(((NTSD-1)*DT)/3600.)+2
          READ(NUNIT_NBC,REC=NREC)BCHR                                  &
                         ,((PDB(N,NN),N=1,LB),NN=1,2)                  &
                         ,(((TB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                         ,(((QB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                         ,(((UB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                         ,(((VB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                        ,(((Q2B(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)   &
                       ,(((CWMB(N,K,NN),N=1,LB),K=KDE,KDS,-1),NN=1,2)
      ENDIF

! Copy the bounary into the WRF framework boundary data structs

      N=1
!
!***  SOUTH BOUNDARY
!
      DO I=1,IDE
        PD_B(I,1,1,P_YSB) = PDB(N,1)
        PD_BT(I,1,1,P_YSB) = PDB(N,2)
        N=N+1
      ENDDO
!
!***  NORTH BOUNDARY
!
      DO I=1,IDE
        PD_B(I,1,1,P_YEB) = PDB(N,1)
        PD_BT(I,1,1,P_YEB) = PDB(N,2)
        N=N+1
      ENDDO
!
!***  WEST BOUNDARY
!
      DO J=3,JDE-2,2
        PD_B(J,1,1,P_XSB) = PDB(N,1)
        PD_BT(J,1,1,P_XSB) = PDB(N,2)
        N=N+1
      ENDDO
!
!***  EAST BOUNDARY
!
      DO J=3,JDE-2,2
        PD_B(J,1,1,P_XEB) = PDB(N,1)
        PD_BT(J,1,1,P_XEB) = PDB(N,2)
        N=N+1
      ENDDO
!
      DO K=KDS,KDE
        N=1
!
!***  SOUTH BOUNDARY
!
        DO I=1,IDE
          T_B(I,k,1,P_YSB) = TB(N,k,1)
          T_BT(I,k,1,P_YSB) = TB(N,k,2)
          Q_B(I,k,1,P_YSB) = QB(N,k,1)
          Q_BT(I,k,1,P_YSB) = QB(N,k,2)
          Q2_B(I,k,1,P_YSB) = Q2B(N,k,1)
          Q2_BT(I,k,1,P_YSB) = Q2B(N,k,2)
          CWM_B(I,k,1,P_YSB) = CWMB(N,k,1)
          CWM_BT(I,k,1,P_YSB) = CWMB(N,k,2)
          N=N+1
        ENDDO
!
!***  NORTH BOUNDARY
!
        DO I=1,IDE
          T_B(I,k,1,P_YEB) = TB(N,k,1)
          T_BT(I,k,1,P_YEB) = TB(N,k,2)
          Q_B(I,k,1,P_YEB) = QB(N,k,1)
          Q_BT(I,k,1,P_YEB) = QB(N,k,2)
          Q2_B(I,k,1,P_YEB) = Q2B(N,k,1)
          Q2_BT(I,k,1,P_YEB) = Q2B(N,k,2)
          CWM_B(I,k,1,P_YEB) = CWMB(N,k,1)
          CWM_BT(I,k,1,P_YEB) = CWMB(N,k,2)
          N=N+1
        ENDDO
!
!***  WEST BOUNDARY
!
        DO J=3,JDE-2,2
          T_B(J,k,1,P_XSB) = TB(N,k,1)
          T_BT(J,k,1,P_XSB) = TB(N,k,2)
          Q_B(J,k,1,P_XSB) = QB(N,k,1)
          Q_BT(J,k,1,P_XSB) = QB(N,k,2)
          Q2_B(J,k,1,P_XSB) = Q2B(N,k,1)
          Q2_BT(J,k,1,P_XSB) = Q2B(N,k,2)
          CWM_B(J,k,1,P_XSB) = CWMB(N,k,1)
          CWM_BT(J,k,1,P_XSB) = CWMB(N,k,2)
          N=N+1
        ENDDO
!
!***  EAST BOUNDARY
!
        DO J=3,JDE-2,2
          T_B(J,k,1,P_XEB) = TB(N,k,1)
          T_BT(J,k,1,P_XEB) = TB(N,k,2)
      if(k.eq.1.and.j.eq.79)then
        write(0,62510)ntsd,nrec
        write(0,62511)p_xeb,t_b(j,k,1,p_xeb),t_bt(j,k,1,p_xeb)
62510   format(' ntsd=',i5,' nrec=',i5)
62511   format(' p_xeb=',i2,' t_b=',z8,' t_bt=',z8)
      endif
          Q_B(J,k,1,P_XEB) = QB(N,k,1)
          Q_BT(J,k,1,P_XEB) = QB(N,k,2)
          Q2_B(J,k,1,P_XEB) = Q2B(N,k,1)
          Q2_BT(J,k,1,P_XEB) = Q2B(N,k,2)
          CWM_B(J,k,1,P_XEB) = CWMB(N,k,1)
          CWM_BT(J,k,1,P_XEB) = CWMB(N,k,2)
          N=N+1
        ENDDO
      ENDDO

      DO K=KDS,KDE
        N=1
!
!***  SOUTH BOUNDARY
!
        DO I=1,IDE-1
          U_B(I,k,1,P_YSB) = UB(N,k,1)
          U_BT(I,k,1,P_YSB) = UB(N,k,2)
          V_B(I,k,1,P_YSB) = VB(N,k,1)
          V_BT(I,k,1,P_YSB) = VB(N,k,2)
          N=N+1
        ENDDO
!
!***  NORTH BOUNDARY
!
        DO I=1,IDE-1
          U_B(I,k,1,P_YEB) = UB(N,k,1)
          U_BT(I,k,1,P_YEB) = UB(N,k,2)
          V_B(I,k,1,P_YEB) = VB(N,k,1)
          V_BT(I,k,1,P_YEB) = VB(N,k,2)
          N=N+1
        ENDDO
!
!***  WEST BOUNDARY
!
        DO J=2,JDE-1,2
          U_B(J,k,1,P_XSB) = UB(N,k,1)
          U_BT(J,k,1,P_XSB) = UB(N,k,2)
          V_B(J,k,1,P_XSB) = VB(N,k,1)
          V_BT(J,k,1,P_XSB) = VB(N,k,2)
          N=N+1
        ENDDO
!
!***  EAST BOUNDARY
!
        DO J=2,JDE-1,2
          U_B(J,k,1,P_XEB) = UB(N,k,1)
          U_BT(J,k,1,P_XEB) = UB(N,k,2)
          V_B(J,k,1,P_XEB) = VB(N,k,1)
          V_BT(J,k,1,P_XEB) = VB(N,k,2)
          N=N+1
        ENDDO
      ENDDO

!
!      CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
!
!      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
!      IF(MYPE.EQ.0)WRITE(LIST,*)'  READ UNIT NBC=',NUNIT_NBC
!
!***
!***  COMPUTE THE 1ST TIME FOR BOUNDARY CONDITION READ
!***
!
!      NBOCO=NINT(BCHR*TSPH)
!

!

   DEALLOCATE(TEMP1,STAT=I)
   DEALLOCATE(ITEMP,STAT=I)
   DEALLOCATE(TEMP,STAT=I)
   DEALLOCATE(HOLD,STAT=I)

      CALL wrf_debug ( 100 , 'nmm: returnomatic' )

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

   RETURN
END SUBROUTINE med_read_nmm

