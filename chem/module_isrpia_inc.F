!C=======================================================================
!C *** ISORROPIA CODE II
!C *** INCLUDE FILE 'ISRPIA.INC'
!C *** THIS FILE CONTAINS THE DECLARATIONS OF THE GLOBAL CONSTANTS
!C     AND VARIABLES. 
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
	
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!liqy
!
!	INTEGER NCOMP, NIONS,NGASAQ,NSLDS,NPAIR,NZSR,NERRMX
!liqy-20140507
      PARAMETER (NCOMP=8,NIONS=10,NGASAQ=3,NSLDS=19,NPAIR=23,NZSR=100,&
                NERRMX=25)
!liqy
!change the NERRMX from 25 to 10000.
!liqy-20140507

!C
!C *** INPUT VARIABLES **************************************************
!C
      INTEGER METSTBL
!liqy
!	INTEGER IPROB,NADJ
!	REAL W, WAER,TEMP,RH
!	DIMENSION W(NCOMP),WAER(NCOMP)
!liqy-20140507
      COMMON /INPT/ W(NCOMP), WAER(NCOMP), TEMP, RH, IPROB, METSTBL,&
                   NADJ
!C
!C *** WATER ACTIVITIES OF PURE SALT SOLUTIONS **************************
!C
!liqy
!	REAL AWAS, AWSS, AWAC, AWSC,&
!		AWAN, AWSN, AWSB, AWAB,&
!                   AWSA, AWLC, AWCS, AWCN, &
!			AWCC, AWPS, AWPB, AWPN,&
!                	   AWPC, AWMS, AWMN, AWMC
!
!	DIMENSION AWAS(NZSR), AWSS(NZSR), AWAC(NZSR), AWSC(NZSR),&
!                   AWAN(NZSR), AWSN(NZSR), AWSB(NZSR), AWAB(NZSR),&
!                   AWSA(NZSR), AWLC(NZSR), AWCS(NZSR), AWCN(NZSR),&
!                   AWCC(NZSR), AWPS(NZSR), AWPB(NZSR), AWPN(NZSR),&
!                   AWPC(NZSR), AWMS(NZSR), AWMN(NZSR), AWMC(NZSR)
!liqy-20140507
      COMMON /ZSR / AWAS(NZSR), AWSS(NZSR), AWAC(NZSR), AWSC(NZSR),&
                   AWAN(NZSR), AWSN(NZSR), AWSB(NZSR), AWAB(NZSR),&
                   AWSA(NZSR), AWLC(NZSR), AWCS(NZSR), AWCN(NZSR),&
                   AWCC(NZSR), AWPS(NZSR), AWPB(NZSR), AWPN(NZSR),&
                   AWPC(NZSR), AWMS(NZSR), AWMN(NZSR), AWMC(NZSR)
!C
!C *** DELIQUESCENCE RELATIVE HUMIDITIES ********************************
!C
      INTEGER WFTYP
!liqy
!	REAL  DRH2SO4,  DRNH42S4, DRNAHSO4, DRNACL,   DRNANO3, &
!                   DRNA2SO4, DRNH4HS4, DRLC,     DRNH4NO3, DRNH4CL,&
!                   DRCASO4,  DRCANO32, DRCACL2,  DRK2SO4,  DRKHSO4,&
!                   DRKNO3,   DRKCL,    DRMGSO4,  DRMGNO32, DRMGCL2
!liqy-20140507
      COMMON /DRH / DRH2SO4,  DRNH42S4, DRNAHSO4, DRNACL,   DRNANO3, &
                   DRNA2SO4, DRNH4HS4, DRLC,     DRNH4NO3, DRNH4CL,&
                   DRCASO4,  DRCANO32, DRCACL2,  DRK2SO4,  DRKHSO4,&
                   DRKNO3,   DRKCL,    DRMGSO4,  DRMGNO32, DRMGCL2
!C
!liqy
!	REAL  DRMLCAB,  DRMLCAS,  DRMASAN,  DRMG1,    DRMG2,&
!                   DRMG3,    DRMH1,    DRMH2,    DRMI1,    DRMI2,&
!                   DRMI3,    DRMQ1,    DRMR1,    DRMR2,    DRMR3,&
!                   DRMR4,    DRMR5,    DRMR6,    DRMR7,    DRMR8,&
!                   DRMR9,    DRMR10,   DRMR11,   DRMR12,   DRMR13
!	INTEGER   WFTYP
!liqy-20140507
      COMMON /MDRH/ DRMLCAB,  DRMLCAS,  DRMASAN,  DRMG1,    DRMG2,&
                   DRMG3,    DRMH1,    DRMH2,    DRMI1,    DRMI2,&
                   DRMI3,    DRMQ1,    DRMR1,    DRMR2,    DRMR3,&
                   DRMR4,    DRMR5,    DRMR6,    DRMR7,    DRMR8,&
                   DRMR9,    DRMR10,   DRMR11,   DRMR12,   DRMR13,&
                   WFTYP
!C

!liqy
!	REAL  DRMO1,    DRMO2,    DRMO3,    DRML1,    DRML2,&
!                    DRML3,    DRMM1,    DRMM2,    DRMP1,    DRMP2,&
!                    DRMP3,    DRMP4,    DRMP5,    DRMV1
!li1y-20140507
      COMMON /MDRH2/ DRMO1,    DRMO2,    DRMO3,    DRML1,    DRML2,&
                    DRML3,    DRMM1,    DRMM2,    DRMP1,    DRMP2,&
                    DRMP3,    DRMP4,    DRMP5,    DRMV1

!C
!C *** VARIABLES FOR LIQUID AEROSOL PHASE *******************************
!C
      DOUBLE PRECISION MOLAL, MOLALR, M0
      REAL IONIC
      LOGICAL CALAOU, CALAIN, FRST, DRYF
!liqy
!	DIMENSION MOLAL(NIONS), MOLALR(NPAIR)

!	INTEGER ZZ,Z,IACALC
!	REAL GAMA, &
!                  GAMOU,  GAMIN,&
!                   GASAQ,&
!                   EPSACT,       COH,           CHNO3,       CHCL,     &   
!                   WATER              
                  
!	DIMENSION GAMA(NPAIR), ZZ(NPAIR),&
!                   Z(NIONS),     GAMOU(NPAIR),  GAMIN(NPAIR),&
!                   GASAQ(NGASAQ)
!liqy-20140507
      COMMON /IONS/ MOLAL(NIONS), MOLALR(NPAIR), GAMA(NPAIR), ZZ(NPAIR),&
                   Z(NIONS),     GAMOU(NPAIR),  GAMIN(NPAIR),M0(NPAIR),&
                   GASAQ(NGASAQ),&
                   EPSACT,       COH,           CHNO3,       CHCL,     &   
                   WATER,        IONIC,         IACALC,      &
                   FRST,         CALAIN,        CALAOU,      DRYF 
!C
!C *** VARIABLES FOR SOLID AEROSOL PHASE ********************************
!C
!liqy
!	REAL CH2SO4,  CNH42S4, CNH4HS4, CNACL,   CNA2SO4, &
!                   CNANO3,  CNH4NO3, CNH4CL,  CNAHSO4, CLC, CCASO4,&
!                   CCANO32, CCACL2,  CK2SO4,  CKHSO4,  CKNO3, CKCL,&
!                   CMGSO4,  CMGNO32, CMGCL2
!liqy-20140507
      COMMON /SALT/ CH2SO4,  CNH42S4, CNH4HS4, CNACL,   CNA2SO4, &
                   CNANO3,  CNH4NO3, CNH4CL,  CNAHSO4, CLC, CCASO4,&
                   CCANO32, CCACL2,  CK2SO4,  CKHSO4,  CKNO3, CKCL,&
                   CMGSO4,  CMGNO32, CMGCL2
!C
!C *** VARIABLES FOR GAS PHASE ******************************************
!C

!liqy
!	REAL GNH3, GHNO3, GHCL 
!liqy-20140507
      COMMON /GAS / GNH3, GHNO3, GHCL 
!C
!C *** EQUILIBRIUM CONSTANTS ********************************************
!C
!liqy
!	REAL XK1, XK2, XK3, XK4, XK5, XK6, XK7, XK8, XK9, XK10,&
!                   XK11,XK12,XK13,XK14,XKW, XK21,XK22,XK31,XK32,XK41,&
!                   XK42, XK15, XK16, XK17, XK18, XK19, XK20, XK23,&
!                   XK24, XK25
!liqy-20140507
      COMMON /EQUK/ XK1, XK2, XK3, XK4, XK5, XK6, XK7, XK8, XK9, XK10,&
                   XK11,XK12,XK13,XK14,XKW, XK21,XK22,XK31,XK32,XK41,&
                   XK42, XK15, XK16, XK17, XK18, XK19, XK20, XK23,&
                   XK24, XK25
!C     &              , XK26, XK27
!C
!C *** MOLECULAR WEIGHTS ************************************************
!C
      DOUBLE PRECISION IMW

!liqy
!	REAL R,WMW, SMW
!	DIMENSIOn WMW(NCOMP), SMW(NPAIR), IMW(NIONS)
!liqy-20140507
      COMMON /OTHR/ R, IMW(NIONS), WMW(NCOMP), SMW(NPAIR)
!C
!C *** SOLUTION/INFO VARIABLES ******************************************
!C
      CHARACTER SCASE*15
!liqy
!	REAL SULRATW, SULRAT, SODRAT, SO4RAT, CRNARAT, CRRAT
!liqy-20140507
      COMMON /CASE/ SULRATW, SULRAT, SODRAT, SO4RAT, CRNARAT, CRRAT, &
                   SCASE
!C
!liqy
!	REAL EPS 
!	INTEGER MAXIT, NSWEEP, NDIV, ICLACT
!liqy-20140507
      COMMON /SOLN/ EPS, MAXIT, NSWEEP, NDIV, ICLACT
!C
!C *** ERROR SYSTEM *****************************************************
!C
      CHARACTER ERRMSG*40
      INTEGER   ERRSTK, NOFER   
      LOGICAL   STKOFL   
      COMMON /EROR/ STKOFL, NOFER, ERRSTK(NERRMX), ERRMSG(NERRMX)
!C
!C *** GENERIC VARIABLES ************************************************
!C
      CHARACTER VERSION*15
!liqy
!	REAL GREAT, TINY, TINY2, ZERO, ONE
!liqy-20140507
      COMMON /CGEN/ GREAT, TINY, TINY2, ZERO, ONE, VERSION
!C
!C *** END OF INCLUDE FILE **********************************************
!C
