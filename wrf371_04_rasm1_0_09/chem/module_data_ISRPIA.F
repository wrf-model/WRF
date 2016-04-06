      MODULE ISRPIA
! %W% %P% %G% %U%
!***********************************************************************
!***********************************************************************
! Development of this code was sponsored by EPRI, 3412 Hillview Ave.,  *
! Palo Alto, CA 94304 under Contract WO8221-01                         *
!                                                                      *
! Developed by Yang Zhang, Betty Pun, and Christian Seigneur,          *
! Atmospheric and Environmental Research, Inc., 2682 Bishop Drive,     *
! Suite 120, San Ramon, CA 94583                                       *
!
! Development of previously available modules are listed at the        *
! begining of the code of the corresponding module.  Some of these     *
! modules may be copyrighted                                           *
!***********************************************************************
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /ncsu/volume2/Mac_Share3/yang/madrid_final/revision/code/july2002/models/CCTM/src/aero/aero_MADRID1_mebi/ISRPIA.EXT,v 1.1 2002/10/24 00:55:29 models3 Exp $

!=======================================================================
!
! *** ISORROPIA PLUS CODE
! *** INCLUDE FILE 'ISRPIA.EXT'
! *** THIS FILE CONTAINS THE DECLARATIONS OF THE GLOBAL CONSTANTS
!     AND VARIABLES.
!
! *** COPYRIGHT 1996-98, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
! *** WRITTEN BY ATHANASIOS NENES
!
! **************************************************************************************
!  FUNCTION: Include file for the ISORROPIA thermodynamic module                       *
!  PRECONDITION REQUIRED:  Aerosol Option: MADRID (Development and                     *
!                                     Application of the Model of Aerosol              *
!                               Dynamics, Reaction, Ionization and Dissolution)        *
!  RETURN VALUES:  gaseous and PM species concentrations                               *
!  KEY SUBROUTINES AND FUNCTIONS CALLED:                                               *
!  REVISION HISTORY:                                                                   *
!         Original coagsolv code provided by ATHANASIOS NENES, Caltech (now at         *
!             Georgia Tech), 2000                                                      *
!         Revised by Yang Zhang and Xiao-Ming Hu, NCSU, May-June 2005                  *
!             to incorporate MADRID into WRF/Chem                                      *
!         Revised by Ying Pan and Yang Zhang, NCSU, Oct. 2009                          *
!             to couple CB05 with MADE/SORGAM-AQ-ISORROPIA                             *
! **************************************************************************************

!=======================================================================
!
!      USE OTHR
!      USE CGEN
!      USE SOLN
!      USE IONSdata
!      USE ZSRdata
!      USE SALT
!      USE GAS_MADRID
!      USE EQUK
      USE module_data_isrpia_data

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
       PARAMETER (NCOMP=5, NIONS=7, NGASAQ=3, NSLDS=9, NPAIR=13,   &
                  NZSR=100, NERRMX=25)
!      PARAMETER (NIONS=7, NSLDS=9, NERRMX=25)

      DOUBLE PRECISION  W(NCOMP), WAER(NCOMP), TEMP, RH, IPROB
      INTEGER METSTBL, NADJ

      DOUBLE PRECISION  DRH2SO4,  DRNH42S4, DRNAHSO4, DRNACL,   DRNANO3,   &
                        DRNA2SO4, DRNH4HS4, DRLC,     DRNH4NO3, DRNH4CL

      DOUBLE PRECISION :: DRMLCAB,DRMLCAS,DRMASAN,DRMG1,  DRMG2,     &
                    DRMG3,    DRMH1,    DRMH2,    DRMI1,    DRMI2,   &
                    DRMI3,    DRMQ1,    DRMR1,    DRMR2,    DRMR3,   &
                    DRMR4,    DRMR5,    DRMR6,    DRMR7,    DRMR8,   &
                    DRMR9,    DRMR10,   DRMR11,   DRMR12,   DRMR13
      INTEGER WFTYP

!     DOUBLE PRECISION CH2SO4,  CNH42S4, CNH4HS4, CNACL,   CNA2SO4, &
!                      CNANO3,  CNH4NO3, CNH4CL,  CNAHSO4, CLC

!     DOUBLE PRECISION GNH3, GHNO3, GHCL

!     DOUBLE PRECISION XK1, XK2, XK3, XK4, XK5, XK6, XK7, XK8, XK9, XK10,   &
!                      XK11, XK12, XK13, XK14, XKW, XK21, XK22, XK31, XK32, &
!                      XK41, XK42

      CHARACTER SCASE*15
      DOUBLE PRECISION SULRATW, SULRAT, SODRAT

!      DOUBLE PRECISION EPS, MAXIT, NSWEEP, NDIV, ICLACT
!     DATA   EPS/1D-6/, MAXIT/100/,NSWEEP/4/,NDIV/5/

      CHARACTER*40 ERRMSG(NERRMX)
      INTEGER   ERRSTK(NERRMX), NOFER
      LOGICAL   STKOFL
      DATA ERRSTK/NERRMX*0/,   ERRMSG/NERRMX*' '/,  NOFER/0/,   &
           STKOFL/.FALSE./

!     CHARACTER VERSION*15
!     DOUBLE PRECISION GREAT, TINY, TINY2, ZERO, ONE
!     DATA   &
!          TINY/1D-20/, GREAT/1D10/, ZERO/0.0D0/, ONE/1.0D0/,   &
!          TINY2/1D-11/
!       DATA VERSION /'1.7 (03/26/07)'/

!
! *** INPUT VARIABLES **************************************************
!
!      INTEGER METSTBL
!      COMMON /INPT/ W(NCOMP), WAER(NCOMP), TEMP, RH, IPROB, METSTBL
!
! *** WATER ACTIVITIES OF PURE SALT SOLUTIONS **************************
!
!      COMMON /ZSR / AWAS(NZSR), AWSS(NZSR), AWAC(NZSR), AWSC(NZSR),
!     &              AWAN(NZSR), AWSN(NZSR), AWSB(NZSR), AWAB(NZSR),
!     &              AWSA(NZSR), AWLC(NZSR)
!
! *** DELIQUESCENCE RELATIVE HUMIDITIES ********************************
!
!      INTEGER WFTYP
!      COMMON /DRH / DRH2SO4,  DRNH42S4, DRNAHSO4, DRNACL,   DRNANO3,
!     &              DRNA2SO4, DRNH4HS4, DRLC,     DRNH4NO3, DRNH4CL
!      COMMON /MDRH/ DRMLCAB,  DRMLCAS,  DRMASAN,  DRMG1,    DRMG2,
!     &              DRMG3,    DRMH1,    DRMH2,    DRMI1,    DRMI2,
!     &              DRMI3,    DRMQ1,    DRMR1,    DRMR2,    DRMR3,
!     &              DRMR4,    DRMR5,    DRMR6,    DRMR7,    DRMR8,
!     &              DRMR9,    DRMR10,   DRMR11,   DRMR12,   DRMR13,
!     &              WFTYP
!
! *** VARIABLES FOR LIQUID AEROSOL PHASE *******************************
!
!      DOUBLE PRECISION MOLAL, MOLALR, M0
!      REAL IONIC
!      LOGICAL CALAOU, CALAIN, FRST, DRYF
!      COMMON /IONS/ MOLAL(NIONS), MOLALR(NPAIR), GAMA(NPAIR), ZZ(NPAIR),
!     &              Z(NIONS),     GAMOU(NPAIR),  GAMIN(NPAIR),M0(NPAIR),
!     &              GASAQ(NGASAQ),
!     &              EPSACT,       COH,           CHNO3,       CHCL,
!     &              WATER,        IONIC,         IACALC,
!     &              FRST,         CALAIN,        CALAOU,      DRYF
!
! *** VARIABLES FOR SOLID AEROSOL PHASE ********************************
!
!      COMMON /SALT/ CH2SO4,  CNH42S4, CNH4HS4, CNACL,   CNA2SO4,
!     &              CNANO3,  CNH4NO3, CNH4CL,  CNAHSO4, CLC
!
! *** VARIABLES FOR GAS PHASE ******************************************
!
!      COMMON /GAS / GNH3, GHNO3, GHCL
!
! *** EQUILIBRIUM CONSTANTS ********************************************
!
!      COMMON /EQUK/ XK1, XK2, XK3, XK4, XK5, XK6, XK7, XK8, XK9, XK10,
!     &              XK11, XK12, XK13, XK14, XKW, XK21, XK22, XK31, XK32,
!     &              XK41, XK42
!
! *** MOLECULAR WEIGHTS ************************************************
!
!      DOUBLE PRECISION IMW
!      COMMON /OTHR/ R, IMW(NIONS), WMW(NCOMP), SMW(NPAIR)
!
! *** SOLUTION/INFO VARIABLES ******************************************
!
!      CHARACTER SCASE*15
!      COMMON /CASE/ SULRATW, SULRAT, SODRAT, SCASE
!
!      COMMON /SOLN/ EPS, MAXIT, NSWEEP, NDIV, ICLACT
!
! *** ERROR SYSTEM *****************************************************
!
!      CHARACTER ERRMSG*40
!      INTEGER   ERRSTK, NOFER
!      LOGICAL   STKOFL
!      COMMON /EROR/ STKOFL, NOFER, ERRSTK(NERRMX), ERRMSG(NERRMX)
!
! *** GENERIC VARIABLES ************************************************
!
!      CHARACTER VERSION*14
!      COMMON /CGEN/ GREAT, TINY, TINY2, ZERO, ONE, VERSION

!
! *** END OF INCLUDE FILE **********************************************
      END MODULE ISRPIA
!
