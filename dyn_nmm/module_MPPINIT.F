!
!NCEP_MESO:MEDIATION_LAYER: SET UP DOMAIN DECOMPOSITION VARIABLES
!
!
!----------------------------------------------------------------------
!
      MODULE MODULE_MPPINIT
!
!----------------------------------------------------------------------
      USE MODULE_MPP
!----------------------------------------------------------------------
!
      CONTAINS
!
!**********************************************************************
      SUBROUTINE MPPINIT(IDS,IDE,JDS,JDE,KDS,KDE                       &
                        ,IMS,IME,JMS,JME,KMS,KME                       &
                        ,IPS,IPE,JPS,JPE,KPS,KPE)
!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    MPPINIT     SET UP DECOMPOSITION VARIABLES
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 98-10-28
!
! ABSTRACT:
!     MPPINIT DETERMINES ALL RELEVANT VALUES FOR DIMENSIONS OF THE
!     DISTRIBUTED SUBDOMAINS AND THEIR HALOES.
!
! PROGRAM HISTORY LOG:
!   97-??-??  MEYS       - ORIGINATOR
!   97-??-??  BLACK      - CHANGES MADE FOR CLARITY
!   98-10-29  BLACK      - REWRITTEN FOR CLARITY
!
! USAGE: CALL MPPINIT FROM MAIN PROGRAM NCEP_MESO
!   INPUT ARGUMENT LIST:

!   OUTPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!
!     UNIQUE:
!        NONE
!
!     LIBRARY:
!        NONE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,IPS,IPE,JPS,JPE,KPS,KPE
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------

      CALL wrf_get_myproc ( MYPE )
      CALL wrf_get_nproc  ( NPES )
      CALL wrf_get_dm_communicator ( mpi_comm_comp )

!jm
      MYIS    = MAX( IPS-0, IDS+0 )
      MYIS_P1 = MAX( IPS-1, IDS+0 )
      MYIS_P2 = MAX( IPS-2, IDS+0 )
      MYIS_P3 = MAX( IPS-3, IDS+0 )
      MYIS_P4 = MAX( IPS-4, IDS+0 )
      MYIS_P5 = MAX( IPS-5, IDS+0 )
!
      MYIS1   = MAX( IPS-0, IDS+1 )
      MYIS1_P1= MAX( IPS-1, IDS+1 )
      MYIS1_P2= MAX( IPS-2, IDS+1 )
      MYIS1_P3= MAX( IPS-3, IDS+1 )
      MYIS1_P4= MAX( IPS-4, IDS+1 )
      MYIS1_P5= MAX( IPS-5, IDS+1 )
!
      MYIS2   = MAX( IPS-0, IDS+2 )
      MYIS2_P1= MAX( IPS-1, IDS+2 )
      MYIS2_P2= MAX( IPS-2, IDS+2 )
      MYIS2_P3= MAX( IPS-3, IDS+2 )
      MYIS2_P4= MAX( IPS-4, IDS+2 )
      MYIS2_P5= MAX( IPS-5, IDS+2 )
!
      MYIS3   = MAX( IPS-0, IDS+3 )
      MYIS3_P1= MAX( IPS-1, IDS+3 )
      MYIS3_P2= MAX( IPS-2, IDS+3 )
      MYIS3_P3= MAX( IPS-3, IDS+3 )
      MYIS3_P4= MAX( IPS-4, IDS+3 )
      MYIS3_P5= MAX( IPS-5, IDS+3 )
!
      MYIS4   = MAX( IPS-0, IDS+4 )
      MYIS4_P1= MAX( IPS-1, IDS+4 )
      MYIS4_P2= MAX( IPS-2, IDS+4 )
      MYIS4_P3= MAX( IPS-3, IDS+4 )
      MYIS4_P4= MAX( IPS-4, IDS+4 )
      MYIS4_P5= MAX( IPS-5, IDS+4 )
!
      MYIS5   = MAX( IPS-0, IDS+5 )
      MYIS5_P1= MAX( IPS-1, IDS+5 )
      MYIS5_P2= MAX( IPS-2, IDS+5 )
      MYIS5_P3= MAX( IPS-3, IDS+5 )
      MYIS5_P4= MAX( IPS-4, IDS+5 )
      MYIS5_P5= MAX( IPS-5, IDS+5 )

!jm
      MYIE    = MIN( IPE+0, IDE-0 )
      MYIE_P1 = MIN( IPE+1, IDE-0 )
      MYIE_P2 = MIN( IPE+2, IDE-0 )
      MYIE_P3 = MIN( IPE+3, IDE-0 )
      MYIE_P4 = MIN( IPE+4, IDE-0 )
      MYIE_P5 = MIN( IPE+5, IDE-0 )
!
      MYIE1   = MIN( IPE+0, IDE-1 )
      MYIE1_P1= MIN( IPE+1, IDE-1 )
      MYIE1_P2= MIN( IPE+2, IDE-1 )
      MYIE1_P3= MIN( IPE+3, IDE-1 )
      MYIE1_P4= MIN( IPE+4, IDE-1 )
      MYIE1_P5= MIN( IPE+5, IDE-1 )
!
      MYIE2   = MIN( IPE+0, IDE-2 )
      MYIE2_P1= MIN( IPE+1, IDE-2 )
      MYIE2_P2= MIN( IPE+2, IDE-2 )
      MYIE2_P3= MIN( IPE+3, IDE-2 )
      MYIE2_P4= MIN( IPE+4, IDE-2 )
      MYIE2_P5= MIN( IPE+5, IDE-2 )
!
      MYIE3   = MIN( IPE+0, IDE-3 )
      MYIE3_P1= MIN( IPE+1, IDE-3 )
      MYIE3_P2= MIN( IPE+2, IDE-3 )
      MYIE3_P3= MIN( IPE+3, IDE-3 )
      MYIE3_P4= MIN( IPE+4, IDE-3 )
      MYIE3_P5= MIN( IPE+5, IDE-3 )
!
      MYIE4   = MIN( IPE+0, IDE-4 )
      MYIE4_P1= MIN( IPE+1, IDE-4 )
      MYIE4_P2= MIN( IPE+2, IDE-4 )
      MYIE4_P3= MIN( IPE+3, IDE-4 )
      MYIE4_P4= MIN( IPE+4, IDE-4 )
      MYIE4_P5= MIN( IPE+5, IDE-4 )
!
      MYIE5   = MIN( IPE+0, IDE-5 )
      MYIE5_P1= MIN( IPE+1, IDE-5 )
      MYIE5_P2= MIN( IPE+2, IDE-5 )
      MYIE5_P3= MIN( IPE+3, IDE-5 )
      MYIE5_P4= MIN( IPE+4, IDE-5 )
      MYIE5_P5= MIN( IPE+5, IDE-5 )

!jm
      MYJS    = MAX( JPS-0, JDS+0 )
      MYJS_P1 = MAX( JPS-1, JDS+0 )
      MYJS_P2 = MAX( JPS-2, JDS+0 )
      MYJS_P3 = MAX( JPS-3, JDS+0 )
      MYJS_P4 = MAX( JPS-4, JDS+0 )
      MYJS_P5 = MAX( JPS-5, JDS+0 )
!
      MYJS1   = MAX( JPS-0, JDS+1 )
      MYJS1_P1= MAX( JPS-1, JDS+1 )
      MYJS1_P2= MAX( JPS-2, JDS+1 )
      MYJS1_P3= MAX( JPS-3, JDS+1 )
      MYJS1_P4= MAX( JPS-4, JDS+1 )
      MYJS1_P5= MAX( JPS-5, JDS+1 )
!
      MYJS2   = MAX( JPS-0, JDS+2 )
      MYJS2_P1= MAX( JPS-1, JDS+2 )
      MYJS2_P2= MAX( JPS-2, JDS+2 )
      MYJS2_P3= MAX( JPS-3, JDS+2 )
      MYJS2_P4= MAX( JPS-4, JDS+2 )
      MYJS2_P5= MAX( JPS-5, JDS+2 )
!
      MYJS3   = MAX( JPS-0, JDS+3 )
      MYJS3_P1= MAX( JPS-1, JDS+3 )
      MYJS3_P2= MAX( JPS-2, JDS+3 )
      MYJS3_P3= MAX( JPS-3, JDS+3 )
      MYJS3_P4= MAX( JPS-4, JDS+3 )
      MYJS3_P5= MAX( JPS-5, JDS+3 )
!
      MYJS4   = MAX( JPS-0, JDS+4 )
      MYJS4_P1= MAX( JPS-1, JDS+4 )
      MYJS4_P2= MAX( JPS-2, JDS+4 )
      MYJS4_P3= MAX( JPS-3, JDS+4 )
      MYJS4_P4= MAX( JPS-4, JDS+4 )
      MYJS4_P5= MAX( JPS-5, JDS+4 )
!
      MYJS5   = MAX( JPS-0, JDS+5 )
      MYJS5_P1= MAX( JPS-1, JDS+5 )
      MYJS5_P2= MAX( JPS-2, JDS+5 )
      MYJS5_P3= MAX( JPS-3, JDS+5 )
      MYJS5_P4= MAX( JPS-4, JDS+5 )
      MYJS5_P5= MAX( JPS-5, JDS+5 )
!
!jm
      MYJE    = MIN( JPE+0, JDE-0 )
      MYJE_P1 = MIN( JPE+1, JDE-0 )
      MYJE_P2 = MIN( JPE+2, JDE-0 )
      MYJE_P3 = MIN( JPE+3, JDE-0 )
      MYJE_P4 = MIN( JPE+4, JDE-0 )
      MYJE_P5 = MIN( JPE+5, JDE-0 )
!
      MYJE1   = MIN( JPE+0, JDE-1 )
      MYJE1_P1= MIN( JPE+1, JDE-1 )
      MYJE1_P2= MIN( JPE+2, JDE-1 )
      MYJE1_P3= MIN( JPE+3, JDE-1 )
      MYJE1_P4= MIN( JPE+4, JDE-1 )
      MYJE1_P5= MIN( JPE+5, JDE-1 )
!
      MYJE2   = MIN( JPE+0, JDE-2 )
      MYJE2_P1= MIN( JPE+1, JDE-2 )
      MYJE2_P2= MIN( JPE+2, JDE-2 )
      MYJE2_P3= MIN( JPE+3, JDE-2 )
      MYJE2_P4= MIN( JPE+4, JDE-2 )
      MYJE2_P5= MIN( JPE+5, JDE-2 )
!
      MYJE3   = MIN( JPE+0, JDE-3 )
      MYJE3_P1= MIN( JPE+1, JDE-3 )
      MYJE3_P2= MIN( JPE+2, JDE-3 )
      MYJE3_P3= MIN( JPE+3, JDE-3 )
      MYJE3_P4= MIN( JPE+4, JDE-3 )
      MYJE3_P5= MIN( JPE+5, JDE-3 )
!
      MYJE4   = MIN( JPE+0, JDE-4 )
      MYJE4_P1= MIN( JPE+1, JDE-4 )
      MYJE4_P2= MIN( JPE+2, JDE-4 )
      MYJE4_P3= MIN( JPE+3, JDE-4 )
      MYJE4_P4= MIN( JPE+4, JDE-4 )
      MYJE4_P5= MIN( JPE+5, JDE-4 )
!
      MYJE5   = MIN( JPE+0, JDE-5 )
      MYJE5_P1= MIN( JPE+1, JDE-5 )
      MYJE5_P2= MIN( JPE+2, JDE-5 )
      MYJE5_P3= MIN( JPE+3, JDE-5 )
      MYJE5_P4= MIN( JPE+4, JDE-5 )
      MYJE5_P5= MIN( JPE+5, JDE-5 )

!----------------------------------------------------------------------
      END SUBROUTINE MPPINIT
!**********************************************************************
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!jm!**********************************************************************
!jm!----------------------------------------------------------------------
!jm!
!jm!**********************************************************************
!jm      SUBROUTINE GOSSIP(IDS,IDE,JDS,JDE,KDS,KDE                        &
!jm                       ,IMS,IME,JMS,JME,KMS,KME                        &
!jm                       ,ITS,ITE,JTS,JTE,KTS,KTE)
!jm!**********************************************************************
!jm!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!jm!                .      .    .
!jm! SUBPROGRAM:    GOSSIP      EXCHANGE OF FIELDS BETWEEN PROCESSORS
!jm!   PRGRMMR: BLACK           ORG: W/NP2      DATE: 97-08-30
!jm!
!jm! ABSTRACT:
!jm!     GOSSIP EXCHANGES MANY FIELDS BETWEEN PROCESSORS
!jm!     IN ORDER TO FILL THE HALOES
!jm!
!jm! PROGRAM HISTORY LOG:
!jm!   97-05-??  MEYS       - ORIGINATOR
!jm!   98-10-23  BLACK      - MODIFIED FOR CURRENT VERSION OF MODEL 
!jm!   01-03-21  BLACK      - CONVERTED TO WRF FORMAT
!jm!
!jm! USAGE: CALL GOSSIP FROM MAIN PROGRAM EBU
!jm!   INPUT ARGUMENT LIST:
!jm!       IMS - TASKS'S STARTING I ADDRESS
!jm!       IME - TASKS'S ENDING I ADDRESS
!jm!       JMS - TASKS'S STARTING J ADDRESS
!jm!       JME - TASKS'S ENDING J ADDRESS
!jm!       KMS - TASKS'S STARTING K ADDRESS
!jm!       KME - TASKS'S ENDING K ADDRESS
!jm!
!jm!   OUTPUT ARGUMENT LIST:
!jm!     NONE
!jm!
!jm!   OUTPUT FILES:
!jm!     NONE
!jm!
!jm!   SUBPROGRAMS CALLED:
!jm!
!jm!     UNIQUE: EXCH
!jm!
!jm!     LIBRARY: NONE
!jm!
!jm! ATTRIBUTES:
!jm!   LANGUAGE: FORTRAN 90
!jm!   MACHINE : IBM SP
!jm!$$$
!jm!**********************************************************************
!jm!----------------------------------------------------------------------
!jm!
!jm      IMPLICIT NONE
!jm!
!jm!----------------------------------------------------------------------
!jm      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
!jm                           ,IMS,IME,JMS,JME,KMS,KME                    &
!jm                           ,ITS,ITE,JTS,JTE,KTS,KTE
!jm!
!jm      INTEGER :: J
!jm!----------------------------------------------------------------------
!jm!**********************************************************************
!jm!
!jm!***  THE NHB ARRAYS
!jm!
!jm      CALL EXCH(LMH,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(LMV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(HBM2,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(HBM3,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(VBM2,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(VBM3,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(SM,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(SICE,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(HTM,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(VTM,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(DX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(WPDAR,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(CPGFU,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(CURV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(FCP,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(FDIV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(FAD,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(F,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(DDMPU,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(DDMPV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(GLAT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(GLON,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(EPSR,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(TG,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(GFFC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(SST,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(ALBASE,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(HDAC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(HDACV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(IVGTYP,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(ISLTYP,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(ISLOPE,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH(VEGFRC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm!
!jm!***  THE RESTRT FILE ARRAYS
!jm!
!jm      CALL EXCH (OMGALF,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (PD,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (RES,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (FIS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (T,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (U,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (V,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (Q,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (Q2,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CWM,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (TRAIN,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (TCUCN,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (RSWIN,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (RSWOUT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (TG,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (Z0,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (AKMS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CZEN,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (AKHS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (THS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (QS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (TWBS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (QWBS,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (HBOT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CFRACL,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (THZ0,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (QZ0,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (UZ0,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (VZ0,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (USTAR,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (HTOP,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CFRACM,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SNO,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SI,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CLDEFI,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (RF,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CUPPT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CFRACH,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SOILTB,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SFCEXC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SMSTAV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SMSTOT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (GRNFLX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (PCTSNO,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (RLWIN,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (RADOT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CZMEAN,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SIGT4,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (U00,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (LC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SR,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (PREC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ACPREC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ACCLIQ,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CUPREC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ACFRCV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (NCFRCV,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ACFRST,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (NCFRST,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ACSNOW,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ACSNOM,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SSROFF,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (BGROFF,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SFCSHX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SFCLHX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SUBSHX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SNOPCX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SFCUVX,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SFCEVP,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (POTEVP,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ASWIN,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ASWOUT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ASWTOA,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ALWIN,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ALWOUT,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ALWTOA,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SMC,NSOIL,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (CMC,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (STC,NSOIL,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (SH2O,NSOIL,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (ALBEDO,1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm!
!jm      CALL EXCH (PINT,LM+1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (Z,LM+1,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (DWDT,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (TOLD,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (UOLD,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm      CALL EXCH (VOLD,LM,5,5,IMS,IME,JMS,JME,KMS,KME)
!jm!
!jm      DO J=MYJS_P4,MYJE_P4
!jm        IVW(J)=IVWG(J+MY_JS_GLB-1)
!jm        IVE(J)=IVEG(J+MY_JS_GLB-1)
!jm        IHE(J)=IHEG(J+MY_JS_GLB-1)
!jm        IHW(J)=IHWG(J+MY_JS_GLB-1)
!jm      ENDDO
!jm!
!jm!----------------------------------------------------------------------
!jm      END SUBROUTINE GOSSIP
!----------------------------------------------------------------------
      END MODULE MODULE_MPPINIT

