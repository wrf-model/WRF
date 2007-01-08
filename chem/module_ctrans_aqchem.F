MODULE module_ctrans_aqchem
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/CCTM/src/cloud/cloud_radm/aqchem.F,v 1.19 2002/12/12 20:08:47 sjr Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
!

CONTAINS
      SUBROUTINE AQCHEM ( TEMP, PRES_PA, TAUCLD, PRCRATE,               &
                          WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS, &
                          AEROSOL, GASWDEP, AERWDEP, HPWDEP )

!-----------------------------------------------------------------------
!
!  DESCRIPTION:
!    Compute concentration changes in cloud due to aqueous chemistry,
!    scavenging and wet deposition amounts.
!
!  Revision History:
!      No   Date   Who	What
!      -- -------- ---  -----------------------------------------
!      0  / /86    CW   BEGIN PROGRAM - Walceks's Original Code
!      1  / /86    RB   INCORPORATE INTO RADM
!      2  03/23/87 DH   REFORMAT
!      3  04/11/88 SJR  STREAMLINED CODE - ADDED COMMENTS
!      4  08/27/88 SJR  COMMENTS, MODIFIED FOR RPM
!      4a 03/15/96 FSB  Scanned hard copy to develop Models3
!                       Version.
!      5  04/24/96 FSB  Made into Models3 Format
!      6  02/18/97 SJR  Revisions to link with Models3
!      7  08/12/97 SJR  Revised for new concentration units (moles/mole)
!                       and new treatment of nitrate and nitric acid
!      8  01/15/98 sjr  revised to add new aitken mode scavenging
!                       and aerosol number scavenging
!      9  12/15/98 David Wong at LM:
!             -- change division of XL, TEMP to multiplication of XL, TEMP
!                reciprocal, respectively
!             -- change / TOTOX / TSIV to / ( TOTOX * TSIV )
!     10  03/18/99 David Wong at LM:
!             -- removed "* 1.0" redundant calculation at TEMP1 calculation
!     11  04/27/00 sjr  Added aerosol surface area as modeled species
!
!  Reference:
!     Walcek & Taylor, 1986, A theoretical Method for computing
!      vertical distributions of acidity and sulfate within cumulus
!      clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
!
!  Called by:  AQMAP
!
!  Calls the following subroutines:  none
!
!  Calls the following functions:  HLCONST
!
!  ARGUMENTS     TYPE      I/O       DESCRIPTION
!  ---------     ----  ------------  --------------------------------
!  GAS(ngas)     real  input&output  Concentration for species i=1,11
!  GASWDEP(ngas) real     output     wet deposition for species
!                                    (1) = SO2   conc (mol/mol of S02)
!                                    (2) = HNO3  conc (mol/mol of HNO3)
!                                    (3) = N2O5  conc (mol/mol of N2O5)
!                                    (4) = CO2   conc (mol/mol of CO2)
!                                    (5) = NH3   conc (mol/mol of NH3)
!                                    (6) = H2O2  conc (mol/mol of H2O2)
!                                    (7) = O3    conc (mol/mol of O3)
!                                    (8) = FOA   conc (mol/mol of FOA)
!                                    (9) = MHP   conc (mol/mol of MHP)
!                                    (10)= PAA   conc (mol/mol of PAA)
!                                    (11)= H2SO4 conc (mol/mol of H2SO4)
!
!  AEROSOL(naer) real input&output   Concentration for species i=1,21
!  AERWDEP(naer) real     output     wet deposition for species
!                                    (1) = SO4AKN conc (mol/mol)
!                                    (2) = SO4ACC conc (mol/mol)
!                                    (3) = NH4AKN conc (mol/mol)
!                                    (4) = NH4ACC conc (mol/mol)
!                                    (5) = NO3AKN conc (mol/mol)
!                                    (6) = NO3ACC conc (mol/mol)
!                                    (7) = NO3COR conc (mol/mol)
!                                    (8) = ORGAKN conc (mol/mol)
!                                    (9) = ORGACC conc (mol/mol)
!                                    (10)= PRIAKN conc (mol/mol)
!                                    (11)= PRIACC conc (mol/mol)
!                                    (12)= PRICOR conc (mol/mol)
!                                    (13)= CACO3  conc (mol/mol)
!                                    (14)= MGCO3  conc (mol/mol)
!                                    (15)= NACL   conc (mol/mol)
!                                    (16)= A3FE   conc (mol/mol)
!                                    (17)= B2MN   conc (mol/mol)
!                                    (18)= KCL    conc (mol/mol)
!                                    (19)= NUMAKN conc (#/mol)
!                                    (20)= NUMACC conc (#/mol)
!                                    (21)= NUMCOR conc (#/mol)
!                                    (22)= SRFAKN conc (m2/mol)
!                                    (23)= SRFACC conc (m2/mol)
!
!-----------------------------------------------------------------------

      IMPLICIT NONE



!     INCLUDE 'CONST.EXT'          ! constants
!  INCLUDE FILE  CONST.EXT
!  Contains:  Fundamental constants for air quality modeling
!  Dependent Upon:  none
!  Revision History: 
!    Adapted 6/92 by CJC from ROM's PI.EXT.
!    3/1/93 John McHenry - include constants needed by LCM aqueous chemistry
!    9/93 by John McHenry - include additional constants needed for FMEM clouds
!    and aqueous chemistry
 
!    3/4/96 Dr. Francis S. Binkowski - reflect current Models3 view that MKS
!    units should be used wherever possible and that sources be documented.
!    Some variables have been added, names changed, and values revised.
 
!    3/7/96 - add universal gas constant and compute gas constant in chemical
!    form. TWOPI is now calculated rather than input. 
 
!    3/13/96 - group declarations and parameter statements
!    9/13/96 - include more physical constants
!    12/24/96 - eliminate silly EPSILON, AMISS
!    1/06/97 - eliminate most derived constants - YOJ
!    1/17/97 (comments only) to provide numerical values as reference - DWB 
 
! FSB References:

!      CRC76,        CRC Handbook of Chemistry and Physics (76th Ed),
!                     CRC Press, 1995 
!      Hobbs, P.V.   Basic Physical Chemistry for the Atmospheric Sciences,
!                     Cambridge Univ. Press, 206 pp, 1995.  
!      Snyder, J.P., Map Projections-A Working Manual, U.S. Geological Survey
!                     Paper 1395 U.S.GPO, Washington, DC, 1987.
!      Stull, R. B., An Introduction to Bounday Layer Meteorology, Kluwer, 
!                     Dordrecht, 1988
!.......................................................................


! Geometric Constants:

      REAL*8      PI ! pi (single precision 3.141593)
      PARAMETER ( PI = 3.14159265358979324 )

      REAL        PI180 ! pi/180 [ rad/deg ]
      PARAMETER ( PI180  = PI / 180.0 )

! Geodetic Constants:

      REAL        REARTH ! radius of the earth [ m ]
                         ! FSB: radius of sphere having same surface area as
                         ! Clarke ellipsoid of 1866 ( Source: Snyder, 1987)
      PARAMETER ( REARTH = 6370997.0 )

      REAL        SIDAY ! length of a sidereal day [ sec ]
                        ! FSB: Source: CRC76 pp. 14-6 
      PARAMETER ( SIDAY = 86164.09 )

      REAL        GRAV ! mean gravitational acceleration [ m/sec**2 ]
                       ! FSB: Value is mean of polar and equatorial values.
                       ! Source: CRC Handbook (76th Ed) pp. 14-6
      PARAMETER ( GRAV = 9.80622 )

      REAL        DG2M ! latitude degrees to meters
      PARAMETER ( DG2M = REARTH * PI180 )

! Solar Constant: 
      REAL        SOLCNST ! Solar constant [ W/m**2 ], p14-2 CRC76
      PARAMETER ( SOLCNST = 1373.0 )

! Fundamental Constants: ( Source: CRC76, pp. 1-1 to 1-6)

      REAL        AVO ! Avogadro's Constant [ number/mol ]
      PARAMETER ( AVO = 6.0221367E23 )

      REAL        RGASUNIV ! universal gas constant [ J/mol-K ]
      PARAMETER ( RGASUNIV = 8.314510 )

      REAL        STDATMPA ! standard atmosphere  [ Pa ]
      PARAMETER ( STDATMPA = 101325.0 )

      REAL        STDTEMP ! Standard Temperature [ K ]
      PARAMETER ( STDTEMP = 273.15 )

      REAL        STFBLZ ! Stefan-Boltzmann [ W/(m**2 K**4) ]
      PARAMETER ( STFBLZ = 5.67051E-8 ) 

! FSB Non-MKS

      REAL        MOLVOL ! Molar volume at STP [ L/mol ] Non MKS units 
      PARAMETER ( MOLVOL = 22.41410 ) 

! Atmospheric Constants: 

      REAL        MWAIR ! mean molecular weight for dry air [ g/mol ]
                        ! FSB: 78.06% N2, 21% O2, and 0.943% A on a mole 
                        ! fraction basis ( Source : Hobbs, 1995) pp. 69-70
      PARAMETER ( MWAIR = 28.9628 )

      REAL        RDGAS  ! dry-air gas constant [ J / kg-K ]
      PARAMETER ( RDGAS = 1.0E3 * RGASUNIV / MWAIR ) ! 287.07548994

      REAL        MWWAT ! mean molecular weight for water vapor [ g/mol ]
      PARAMETER ( MWWAT = 18.0153 )

      REAL        RWVAP ! gas constant for water vapor [ J/kg-K ]
      PARAMETER ( RWVAP = 1.0E3 * RGASUNIV / MWWAT ) ! 461.52492604

! FSB NOTE: CPD, CVD, CPWVAP and CVWVAP are calculated assuming dry air and
! water vapor are classical ideal gases, i.e. vibration does not contribute
! to internal energy.

      REAL        CPD ! specific heat of dry air at constant pressure [ J/kg-K ]
      PARAMETER ( CPD = 7.0 * RDGAS / 2.0 )          ! 1004.7642148 

      REAL        CVD ! specific heat of dry air at constant volume [ J/kg-K ]
      PARAMETER ( CVD = 5.0 * RDGAS / 2.0 )          ! 717.68872485

      REAL        CPWVAP ! specific heat for water vapor at constant pressure [ J/kg-K ]
      PARAMETER ( CPWVAP = 4.0 * RWVAP )             ! 1846.0997042

      REAL        CVWVAP ! specific heat for water vapor at constant volume [ J/kg-K ]
      PARAMETER ( CVWVAP = 3.0 * RWVAP )             ! 1384.5747781

      REAL        VP0 ! vapor press of water at 0 C [ Pa ] Source: CRC76 pp. 6-15
      PARAMETER ( VP0 = 611.29 )

! FSB The following values are taken from p. 641 of Stull (1988):

      REAL        LV0 ! latent heat of vaporization of water at 0 C [ J/kg ]
      PARAMETER ( LV0 = 2.501E6 )

      REAL        DLVDT ! Rate of change of latent heat of vaporization with
                        ! respect to temperature [ J/kg-K ]
      PARAMETER ( DLVDT = 2370.0 ) 

      REAL        LF0 ! latent heat of fusion of water at 0 C [ J/kg ]
      PARAMETER ( LF0 = 3.34E5 )

!.......................................................................
!     INCLUDE 'PARMS3.EXT'         ! I/O parameters definitions
!     INCLUDE 'AQ_PARAMS2.EXT'      ! aqueous chemistry shared parameters


! Aqeuous species pointers INCLUDE File

!...........PARAMETERS and their descriptions:

      INTEGER      NGAS            ! number of gas phase species for AQCHEM
      PARAMETER  ( NGAS  = 11 )

      INTEGER      NAER            ! number of aerosol species for AQCHEM
      PARAMETER  ( NAER  = 23 )

!...pointers for the AQCHEM array GAS

      INTEGER      LSO2            ! local pointer to SO2
      PARAMETER  ( LSO2   =  1 )

      INTEGER      LHNO3           ! local pointer to HNO3
      PARAMETER  ( LHNO3  =  2 )

      INTEGER      LN2O5           ! local pointer to N2O5
      PARAMETER  ( LN2O5  =  3 )

      INTEGER      LCO2            ! local pointer to CO2
      PARAMETER  ( LCO2   =  4 )

      INTEGER      LNH3            ! local pointer to NH3
      PARAMETER  ( LNH3   =  5 )

      INTEGER      LH2O2           ! local pointer to H2O2
      PARAMETER  ( LH2O2  =  6 )

      INTEGER      LO3             ! local pointer to O3
      PARAMETER  ( LO3    =  7 )

      INTEGER      LFOA            ! local pointer to FOA
      PARAMETER  ( LFOA   =  8 )

      INTEGER      LMHP            ! local pointer to MHP
      PARAMETER  ( LMHP   =  9 )

      INTEGER      LPAA            ! local pointer to PAA
      PARAMETER  ( LPAA   = 10 )

      INTEGER      LH2SO4          ! local pointer to H2SO4
      PARAMETER  ( LH2SO4 = 11 )

!...pointers for the AQCHEM array AEROSOL

      INTEGER      LSO4AKN         ! local pointer to SO4I aerosol
      PARAMETER  ( LSO4AKN =  1 )

      INTEGER      LSO4ACC         ! local pointer to SO4 aerosol
      PARAMETER  ( LSO4ACC =  2 )

      INTEGER      LNH4AKN         ! local pointer to NH4I aerosol
      PARAMETER  ( LNH4AKN =  3 )

      INTEGER      LNH4ACC         ! local pointer to NH4 aerosol
      PARAMETER  ( LNH4ACC =  4 )

      INTEGER      LNO3AKN         ! local pointer to NO3I aerosol
      PARAMETER  ( LNO3AKN =  5 )

      INTEGER      LNO3ACC         ! local pointer to NO3 aerosol
      PARAMETER  ( LNO3ACC =  6 )

      INTEGER      LNO3COR         ! local pointer to course aerosol nitrate
      PARAMETER  ( LNO3COR =  7 )

      INTEGER      LORGAKN         ! local pointer to organic I aerosol
      PARAMETER  ( LORGAKN =  8 )

      INTEGER      LORGACC         ! local pointer to organic aerosol
      PARAMETER  ( LORGACC =  9 )

      INTEGER      LPRIAKN         ! local pointer to primary I aerosol
      PARAMETER  ( LPRIAKN = 10 )

      INTEGER      LPRIACC         ! local pointer to primary aerosol
      PARAMETER  ( LPRIACC = 11 )

      INTEGER      LPRICOR         ! local pointer to primary I aerosol
      PARAMETER  ( LPRICOR = 12 )

      INTEGER      LCACO3          ! local pointer to CaCO3 aerosol
      PARAMETER  ( LCACO3  = 13 )

      INTEGER      LMGCO3          ! local pointer to MgCO3 aerosol
      PARAMETER  ( LMGCO3  = 14 )

      INTEGER      LNACL           ! local pointer to NaCl aerosol
      PARAMETER  ( LNACL   = 15 )

      INTEGER      LA3FE           ! local pointer to Fe+++ aerosol
      PARAMETER  ( LA3FE   = 16 )

      INTEGER      LB2MN           ! local pointer to Mn++ aerosol
      PARAMETER  ( LB2MN   = 17 )

      INTEGER      LKCL            ! local pointer to NaCl aerosol
      PARAMETER  ( LKCL    = 18 )

      INTEGER      LNUMAKN         ! local pointer to # Aitken aerosol
      PARAMETER  ( LNUMAKN = 19 )

      INTEGER      LNUMACC         ! local pointer to # accumulation aerosol
      PARAMETER  ( LNUMACC = 20 )

      INTEGER      LNUMCOR         ! local pointer to # coarse aerosol
      PARAMETER  ( LNUMCOR = 21 )

      INTEGER      LSRFAKN         ! local pointer to sfc area Aitken aerosol
      PARAMETER  ( LSRFAKN = 22 )

      INTEGER      LSRFACC         ! local pntr to sfc area accumulation aerosol
      PARAMETER  ( LSRFACC = 23 )

!...surrogate names, their background values, and units
!...  for AQCHEM's GAS species

      CHARACTER*16 SGRGAS( NGAS )  ! surrogate name for gases
      SAVE         SGRGAS

      REAL         BGNDGAS( NGAS ) ! background values for each gas
      SAVE         BGNDGAS

      CHARACTER*16 BUNTSGAS( NGAS ) ! units of bkgnd values
      SAVE         BUNTSGAS

      DATA SGRGAS(  1 ), BGNDGAS(  1 ) /'SO2             ',   0.0 /
      DATA SGRGAS(  2 ), BGNDGAS(  2 ) /'HNO3            ',   0.0 /
      DATA SGRGAS(  3 ), BGNDGAS(  3 ) /'N2O5            ',   0.0 /
      DATA SGRGAS(  4 ), BGNDGAS(  4 ) /'CO2             ', 340.0 /
      DATA SGRGAS(  5 ), BGNDGAS(  5 ) /'NH3             ',   0.0 /
      DATA SGRGAS(  6 ), BGNDGAS(  6 ) /'H2O2            ',   0.0 /
      DATA SGRGAS(  7 ), BGNDGAS(  7 ) /'O3              ',   0.0 /
      DATA SGRGAS(  8 ), BGNDGAS(  8 ) /'FOA             ',   0.0 /
      DATA SGRGAS(  9 ), BGNDGAS(  9 ) /'MHP             ',   0.0 /
      DATA SGRGAS( 10 ), BGNDGAS( 10 ) /'PAA             ',   0.0 /
      DATA SGRGAS( 11 ), BGNDGAS( 11 ) /'H2SO4           ',   0.0 /

      DATA BUNTSGAS(  1 ) / 'ppm' /
      DATA BUNTSGAS(  2 ) / 'ppm' /
      DATA BUNTSGAS(  3 ) / 'ppm' /
      DATA BUNTSGAS(  4 ) / 'ppm' /
      DATA BUNTSGAS(  5 ) / 'ppm' /
      DATA BUNTSGAS(  6 ) / 'ppm' /
      DATA BUNTSGAS(  7 ) / 'ppm' /
      DATA BUNTSGAS(  8 ) / 'ppm' /
      DATA BUNTSGAS(  9 ) / 'ppm' /
      DATA BUNTSGAS( 10 ) / 'ppm' /
      DATA BUNTSGAS( 11 ) / 'ppm' /

!...surrogate names, their background values, units, and molecular weights
!...  for AQCHEM's AEROSOL species

      CHARACTER*16 SGRAER( NAER )   ! surrogate name for aerosols
      SAVE         SGRAER

      REAL         SGRAERMW( NAER ) ! molecular weight for aerosol species
      SAVE         SGRAERMW

      CHARACTER*16 BUNTSAER( NAER ) ! units of bkgnd values
      SAVE         BUNTSAER

      REAL         BGNDAER( NAER ) ! bkground vals each aerosols
      SAVE         BGNDAER

      DATA SGRAER(  1 ), SGRAERMW(  1 ) / 'SO4_AITKEN      ' ,  96.0 /
      DATA SGRAER(  2 ), SGRAERMW(  2 ) / 'SO4_ACCUM       ' ,  96.0 /
      DATA SGRAER(  3 ), SGRAERMW(  3 ) / 'NH4_AITKEN      ' ,  18.0 /
      DATA SGRAER(  4 ), SGRAERMW(  4 ) / 'NH4_ACCUM       ' ,  18.0 /
      DATA SGRAER(  5 ), SGRAERMW(  5 ) / 'NO3_AITKEN      ' ,  62.0 /
      DATA SGRAER(  6 ), SGRAERMW(  6 ) / 'NO3_ACCUM       ' ,  62.0 /
      DATA SGRAER(  7 ), SGRAERMW(  7 ) / 'NO3_COARSE      ' ,  62.0 /
      DATA SGRAER(  8 ), SGRAERMW(  8 ) / 'ORG_AITKEN      ' , 120.0 /
      DATA SGRAER(  9 ), SGRAERMW(  9 ) / 'ORG_ACCUM       ' , 120.0 /
      DATA SGRAER( 10 ), SGRAERMW( 10 ) / 'PRI_AITKEN      ' , 200.0 /
      DATA SGRAER( 11 ), SGRAERMW( 11 ) / 'PRI_ACCUM       ' , 200.0 /
      DATA SGRAER( 12 ), SGRAERMW( 12 ) / 'PRI_COARSE      ' , 100.0 /
      DATA SGRAER( 13 ), SGRAERMW( 13 ) / 'CACO3           ' , 100.1 /
      DATA SGRAER( 14 ), SGRAERMW( 14 ) / 'MGCO3           ' ,  84.3 /
      DATA SGRAER( 15 ), SGRAERMW( 15 ) / 'NACL            ' ,  58.4 /
      DATA SGRAER( 16 ), SGRAERMW( 16 ) / 'A3FE            ' ,  55.8 /
      DATA SGRAER( 17 ), SGRAERMW( 17 ) / 'B2MN            ' ,  54.9 /
      DATA SGRAER( 18 ), SGRAERMW( 18 ) / 'KCL             ' ,  74.6 /
      DATA SGRAER( 19 ), SGRAERMW( 19 ) / 'NUM_AITKEN      ' ,   1.0 /
      DATA SGRAER( 20 ), SGRAERMW( 20 ) / 'NUM_ACCUM       ' ,   1.0 /
      DATA SGRAER( 21 ), SGRAERMW( 21 ) / 'NUM_COARSE      ' ,   1.0 /
      DATA SGRAER( 22 ), SGRAERMW( 22 ) / 'SRF_AITKEN      ' ,   1.0 /
      DATA SGRAER( 23 ), SGRAERMW( 23 ) / 'SRF_ACCUM       ' ,   1.0 /

      DATA BGNDAER(  1 ), BUNTSAER(  1 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  2 ), BUNTSAER(  2 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  3 ), BUNTSAER(  3 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  4 ), BUNTSAER(  4 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  5 ), BUNTSAER(  5 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  6 ), BUNTSAER(  6 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  7 ), BUNTSAER(  7 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  8 ), BUNTSAER(  8 ) /  0.0,   'ug/m3' /
      DATA BGNDAER(  9 ), BUNTSAER(  9 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 10 ), BUNTSAER( 10 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 11 ), BUNTSAER( 11 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 12 ), BUNTSAER( 12 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 13 ), BUNTSAER( 13 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 14 ), BUNTSAER( 14 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 15 ), BUNTSAER( 15 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 16 ), BUNTSAER( 16 ) /  0.010, 'ug/m3' /
      DATA BGNDAER( 17 ), BUNTSAER( 17 ) /  0.005, 'ug/m3' /
      DATA BGNDAER( 18 ), BUNTSAER( 18 ) /  0.0,   'ug/m3' /
      DATA BGNDAER( 19 ), BUNTSAER( 19 ) /  0.0,   '#/m3' /
      DATA BGNDAER( 20 ), BUNTSAER( 20 ) /  0.0,   '#/m3' /
      DATA BGNDAER( 21 ), BUNTSAER( 21 ) /  0.0,   '#/m3' /
      DATA BGNDAER( 22 ), BUNTSAER( 22 ) /  0.0,   'm2/m3' /
      DATA BGNDAER( 23 ), BUNTSAER( 23 ) /  0.0,   'm2/m3' /
      CHARACTER*120 XMSG           ! Exit status message
      DATA          XMSG / ' ' /

!...........PARAMETERS and their descriptions:

      INTEGER      NUMOX           ! number of oxidizing reactions
      PARAMETER  ( NUMOX =  5 )

      REAL         H2ODENS         ! density of water at 20 C and 1 ATM
      PARAMETER  ( H2ODENS = 1000.0 )  ! (kg/m3)

      INTEGER      NLIQS           ! number of liquid phase species
      PARAMETER  ( NLIQS = 33 )

      REAL         ONETHIRD       ! 1/3
      PARAMETER  ( ONETHIRD = 1.0 / 3.0 )

      REAL         TWOTHIRDS       ! 2/3
      PARAMETER  ( TWOTHIRDS = 2.0 / 3.0 )
      
      REAL         CONCMIN         ! minimum concentration
      PARAMETER  ( CONCMIN = 1.0E-30 )
      
      REAL         SEC2HR          ! convert seconds to hours
      PARAMETER  ( SEC2HR = 1.0 / 3600.0 )

!...........ARGUMENTS and their descriptions

      INTEGER      JDATE           ! current model date, coded YYYYDDD
      INTEGER      JTIME           ! current model time, coded HHMMSS

      REAL         AIRM            ! total air mass in cloudy layers (mol/m2)
      REAL         ALFA0           ! scav coef for aitken aerosol number
      REAL         ALFA2           ! scav coef for aitken aerosol sfc area
      REAL         ALFA3           ! scav coef for aitken aerosol mass
      REAL         HPWDEP          ! hydrogen wet deposition (mm mol/liter)
      REAL         PRCRATE         ! precip rate (mm/hr)
      REAL         PRES_PA         ! pressure (Pa)
      REAL         TAUCLD          ! timestep for cloud (s)
      REAL         TEMP            ! temperature (K)
      REAL         WCAVG           ! liquid water content (kg/m3)
      REAL         WTAVG           ! total water content (kg/m3)
      REAL         GAS    ( NGAS ) ! gas phase concentrations (mol/molV)
      REAL         AEROSOL( NAER ) ! aerosol concentrations (mol/molV)
      REAL         GASWDEP( NGAS ) ! gas phase wet deposition array (mm mol/liter)
      REAL         AERWDEP( NAER ) ! aerosol wet deposition array (mm mol/liter)

!...........LOCAL VARIABLES (scalars) and their descriptions:

      CHARACTER*16 PNAME          ! driver program name
      DATA         PNAME / 'AQCHEM' /
      SAVE         PNAME

      INTEGER      I20C            ! loop counter for do loop 20
      INTEGER      I30C            ! loop counter for do loop 30
      INTEGER      ITERAT          ! # iterations of aqueaous chemistry solver
      INTEGER      I7777C          ! aqueous chem iteration counter
      INTEGER      ICNTAQ          ! aqueous chem iteration counter
      INTEGER      LIQ             ! loop counter for liquid species
      INTEGER      IOX             ! index over oxidation reactions

      REAL         atrah
      REAL         DEPSUM
      REAL         BETASO4
      REAL         A               ! iron's anion concentration
      REAL         AC              ! H+ concentration in cloudwater (mol/liter)
      REAL         ACT1            ! activity corretion factor!single ions
      REAL         ACT2            ! activity factor correction!double ions
      REAL         ACTB            !
      REAL         AE              ! guess for H+ conc in cloudwater (mol/liter)
      REAL         B               ! manganese's anion concentration
      REAL         PRES_ATM        ! pressure (Atm)
      REAL         BB              ! lower limit guess of cloudwater pH
      REAL         CA              ! Calcium conc in cloudwater (mol/liter)
      REAL         CAA             ! inital Calcium in cloudwater (mol/liter)
      REAL         NO3CORA         ! initial NO3COR in cloudwater (mol/liter)
      REAL         CL              ! Cl-  conc in cloudwater (mol/liter)
      REAL         CLA             ! initial Cl in cloudwater (mol/liter)
      REAL         CO2H            ! Henry's Law constant for CO2
      REAL         CO21            ! First dissociation constant for CO2
      REAL         CO22            ! Second dissociation constant for CO2
      REAL         CO212           ! CO21*CO22
      REAL         CO212H          ! CO2H*CO21*CO22
      REAL         CO21H           ! CO2H*CO21
      REAL         CO2L            ! CO2 conc in cloudwater (mol/liter)
      REAL         CO3             ! CO3= conc in cloudwater (mol/liter)
      REAL         CO3A            ! initial CO3 in cloudwater (mol/liter)
      REAL         CTHK1           ! cloud thickness (m)
      REAL         DTRMV           !
      REAL         DTS6            !
      REAL         EBETASO4T       ! EXP( -BETASO4 * TAUCLD )
      REAL         EALFA0T         ! EXP( -ALFA0 * TAUCLD )
      REAL         EALFA2T         ! EXP( -ALFA2 * TAUCLD )
      REAL         EALFA3T         ! EXP( -ALFA3 * TAUCLD )
      REAL         FA              ! functional value ??
      REAL         FB              ! functional value ??
      REAL         FE              ! Fe+++ conc in cloudwater (mol/liter)
      REAL         FEA             ! initial Fe in cloudwater (mol/liter)
      REAL         FNH3            ! frac weight of NH3 to total ammonia
      REAL         FNH4ACC         ! frac weight of NH4 acc to total ammonia
      REAL         FHNO3           ! frac weight of HNO3 to total NO3
      REAL         FNO3ACC         ! frac weight of NO3 acc to total NO3
      REAL         FNO3COR         ! frac weight of NO3 cor to total NO3
      REAL         FRACACC         ! frac ACC that was from accum mode
      REAL         FRACCOR         ! frac NO3 that was from coarse mode
      REAL         FRACLIQ         ! fraction of water in liquid form
      REAL         FOA1            ! First dissociation constant for FOA
      REAL         FOAH            ! Henry's Law constant for FOA
      REAL         FOA1H           ! FOAH*FOA1
      REAL         FOAL            ! FOA conc in cloudwater (mol/liter)
      REAL         FTST            !
      REAL         GM              !
      REAL         GM1             !
      REAL         GM1LOG          !
      REAL         GM2             ! activity correction factor
      REAL         GM2LOG          !
      REAL         HA              !
      REAL         HB              !
      REAL         H2OW            !
      REAL         H2O2H           ! Henry's Law Constant for H2O2
      REAL         H2O2L           ! H2O2 conc in cloudwater (mol/liter)
      REAL         HCO2            ! HCO2 conc in cloudwater (mol/liter)
      REAL         HCO3            ! HCO3 conc in cloudwater (mol/liter)
      REAL         HNO3H           ! Henry's Law Constant for HNO3
      REAL         HNO31           ! First dissociation constant for HNO3
      REAL         HNO31H          !
      REAL         HNO3L           ! HNO3 conc in cloudwater (mol/liter)
      REAL         HSO3            ! HSO3 conc in cloudwater (mol/liter)
      REAL         HSO4            ! HSO4 concn in cloudwater (mol/liter)
      REAL         HTST            !
      REAL         K               ! K conc in cloudwater (mol/liter)
      REAL         KA              ! initial K in cloudwater (mol/liter)
      REAL         LGTEMP          ! log of TEMP
      REAL         M3NEW           ! accumulation mode mass at time t
      REAL         M3OLD           ! accumulation mode mass at time 0
      REAL         MG              !
      REAL         MGA             ! inital Mg in cloudwater (mol/liter)
      REAL         MHPH            ! Henry's Law Constant for MHP
      REAL         MHPL            ! MHP conc in cloudwater (mol/liter)
      REAL         MN              ! Mn++ conc in cloudwater (mol/liter)
      REAL         MNA             ! initial Mn in cloudwater (mol/liter)
      REAL         NA              ! Na conc in cloudwater (mol/liter)
      REAL         NAA             ! initial Na in cloudwater (mol/liter)
      REAL         NH31            ! First dissociation constant for NH3
      REAL         NH3H            ! Henry's Law Constant for NH3
      REAL         NH3DH20         !
      REAL         NH31HDH         !
      REAL         NH3L            ! NH3 conc in cloudwater (mol/liter)
      REAL         NH4             ! NH4+ conc in cloudwater (mol/liter)
      REAL         NH4AKNA         ! init NH4 akn conc in cloudwater (mol/liter)
      REAL         NH4ACCA         ! init NH4 acc conc in cloudwater (mol/liter)
      REAL         NITAER          ! total aerosol nitrate 
      REAL         NO3             ! NO3 conc in cloudwater (mol/liter)
      REAL         NO3ACCA         ! init NO3 acc conc in cloudwater (mol/liter)
      REAL         NO3AKNA         ! init NO3 akn conc in cloudwater (mol/liter)
      REAL         O3H             ! Henry's Law Constant for O3
      REAL         O3L             ! O3 conc in cloudwater (mol/liter)
      REAL         OH              ! OH conc in cloudwater (mol/liter)
      REAL         ORGN            ! ORGANIC aerosol in cloudwater (mol/liter)
      REAL         ORGACCA         ! init ORG ACC aerosol in cloudwater (mol/liter)
      REAL         ORGAKNA         ! init ORG AKN aerosol in cloudwater (mol/liter)
      REAL         PAAH            ! Henry's Law Constant for PAA
      REAL         PAAL            ! PAA conc in cloudwater (mol/liter)
      REAL         PCO20           ! total CO2 partial pressure (atm)
      REAL         PCO2F           ! gas only CO2 partial pressure (atm)
      REAL         PFOA0           ! total ORGANIC acid partial pressure (atm)
      REAL         PFOAF           ! gas only ORGANIC ACID partial press (atm)
      REAL         PH2O20          ! total H2O2 partial pressure (atm)
      REAL         PH2O2F          ! gas only H2O2 partial pressure (atm)
      REAL         PHNO30          ! total HNO3 partial pressure (atm)
      REAL         PHNO3F          ! gas only HNO3 partial pressure (atm)
      REAL         PMHP0           ! total MHP partial pressure (atm)
      REAL         PMHPF           ! gas only MHP partial pressure (atm)
      REAL         PNH30           ! total NH3 partial pressure (atm)
      REAL         PNH3F           ! gas only NH3 partial pressure (atm)
      REAL         PO30            ! total O3 partial pressure (atm)
      REAL         PO3F            ! gas only O3 partial pressure (atm)
      REAL         PPAA0           ! total PAA partial pressure (atm)
      REAL         PPAAF           ! gas only PAA partial pressure (atm)
      REAL         PRIM            ! PRIMARY acc+akn aerosol in cloudwater (mol/liter)
      REAL         PRIMCOR         ! PRIMARY coarse aerosol in cloudwater (mol/liter)
      REAL         PRIACCA         ! init PRI ACC aerosol in cloudwater (mol/liter)
      REAL         PRIAKNA         ! init PRI AKN aerosol in cloudwater (mol/liter)
      REAL         PRICORA         ! init PRI COR aerosol in cloudwater (mol/liter)
      REAL         PSO20           ! total SO2 partial pressure (atm)
      REAL         PSO2F           ! gas only SO2 partial pressure (atm)
      REAL         RATE            !
      REAL         RECIPA1         !
      REAL         RECIPA2         !
      REAL         RECIPAP1        ! one over pressure (/atm)
      REAL         RH2O2           !
      REAL         RMHP            !
      REAL         RPAA            !
      REAL         RT              ! gas const * temperature (liter atm/mol)
      REAL         SCVEFF          ! Scavenging efficiency (%)
      DATA         SCVEFF / 100.0 / ! currently set to 100%
      SAVE         SCVEFF
      REAL         SIV             ! dissolved so2 in cloudwater (mol/liter)
      REAL         SK6             !
      REAL         SK6TS6          !
      REAL         SO21            ! First dissociation constant for SO2
      REAL         SO22            ! Second dissociation constant for SO2
      REAL         SO2H            ! Henry's Law Constant for SO2
      REAL         SO212           ! SO21*SO22
      REAL         SO212H          ! SO21*SO22*SO2H
      REAL         SO21H           ! SO21*SO2H
      REAL         SO2L            ! SO2 conc in cloudwater (mol/liter)
      REAL         SO3             ! SO3= conc in cloudwater (mol/liter)
      REAL         SO4             ! SO4= conc in cloudwater (mol/liter)
      REAL         STION           ! ionic strength
      REAL         TAC             !
      REAL         TEMP1           !
      REAL         TIMEW           ! cloud chemistry clock (sec)
      REAL         TOTOX           !
      REAL         TOTAMM          ! total ammonium
      REAL         TOTNIT          ! total nitrate
      REAL         TS6             ! SO4 conc in cloudwater (mol/liter)
      REAL         TS6AKNA         ! init SO4 akn conc in cloudwater (mol/liter)
      REAL         TS6ACCA         ! init SO4 acc conc in cloudwater (mol/liter)
      REAL         TSIV            !
      REAL         TST             !
      REAL         WETFAC          ! converts mol/l to mm-mol/l based on precip
      REAL         XC1             ! (/mm)
      REAL         XC2             ! (liter-atm/mol/mm)
      REAL         XL              ! conversion factor (liter-atm/mol)
      REAL         ONE_OVER_XL     ! 1.0 / XL
      REAL         PRES_ATM_OVER_XL     ! PRES_ATM / XL
      REAL         XLCO2           !
      REAL         XLH2O2          !
      REAL         XLHNO3          !
      REAL         XLMHP           !
      REAL         XLNH3           !
      REAL         XLO3            !
      REAL         XLPAA           !
      REAL         XLSO2           !

!...........LOCAL VARIABLES (arrays) and their descriptions:

      REAL         LIQUID( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL         WETDEP( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL         DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL         DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      REAL*8         DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      REAL         ONE_OVER_TEMP     ! 1.0 / TEMP

!...........EXTERNAL FUNCTIONS and their descriptions:

!     REAL          HLCONST
!     EXTERNAL      HLCONST

!*********************************************************************
!     begin body of subroutine AQCHEM

      ONE_OVER_TEMP = 1.0 / TEMP

!...check for bad temperature, cloud air mass, or pressure

      IF ( TEMP .LE. 0.0 ) THEN
        IF ( AIRM .LE. 0.0 ) THEN
          IF ( PRES_PA .LE. 0.0 ) THEN
!           XMSG = 'MET DATA ERROR'
!cc            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            write(0,*)TEMP,AIRM,PRES_PA
            CALL wrf_error_fatal ( ' met data error in aqchem')
          END IF
        END IF
      END IF

!...compute several conversion factors

      ICNTAQ = 0
      ITERAT = 0
      RT = ( MOLVOL / STDTEMP ) * TEMP             ! R * T (liter atm / mol)
      PRES_ATM = PRES_PA /  STDATMPA               ! pressure (atm)
      CTHK1 = AIRM * RT / ( PRES_ATM * 1000.0 )    ! cloud thickness (m)
      XL   = WCAVG * RT / H2ODENS     ! conversion factor (l-atm/mol)
      ONE_OVER_XL = 1.0 / XL
      PRES_ATM_OVER_XL = PRES_ATM / XL
      TST  = 0.999
      GM   = SCVEFF / 100.0
      ACT1 = 1.0
      ACT2 = 1.0
      GM2  = 1.0
      TIMEW = 0.0
      RECIPAP1 = 1.0 / PRES_ATM
      XC1  = 1.0 / ( WCAVG * CTHK1 )
      XC2  = RT / ( 1000.0 * CTHK1 )
      FRACLIQ = WCAVG / WTAVG
	gaswdep=0.
	aerwdep=0.

!...set equilibrium constants as a function of temperature
!...   Henry's law constants

      SO2H  = HLCONST( 'SO2             ', TEMP, .FALSE., 0.0 )
      CO2H  = HLCONST( 'CO2             ', TEMP, .FALSE., 0.0 )
      NH3H  = HLCONST( 'NH3             ', TEMP, .FALSE., 0.0 )
      H2O2H = HLCONST( 'H2O2            ', TEMP, .FALSE., 0.0 )
      O3H   = HLCONST( 'O3              ', TEMP, .FALSE., 0.0 )
      HNO3H = HLCONST( 'HNO3            ', TEMP, .FALSE., 0.0 )
      MHPH  = HLCONST( 'METHYLHYDROPEROX', TEMP, .FALSE., 0.0 )
      PAAH  = HLCONST( 'PEROXYACETIC_ACI', TEMP, .FALSE., 0.0 )
      FOAH  = HLCONST( 'FORMIC_ACID     ', TEMP, .FALSE., 0.0 )
      atraH  = HLCONST( 'ATRA           ', TEMP, .true., 1.0e-4 )

      TEMP1 = ONE_OVER_TEMP - 1.0 / 298.0

!...dissociation constants

      FOA1  = 1.80E-04 * EXP( -2.00E+01 * TEMP1 )      ! Martell and Smith (1977)
      SK6   = 1.02E-02 * EXP(  2.72E+03 * TEMP1 )      ! Smith and Martell (1976)
      SO21  = 1.30E-02 * EXP(  1.96E+03 * TEMP1 )      ! Smith and Martell (1976)
      SO22  = 6.60E-08 * EXP(  1.50E+03 * TEMP1 )      ! Smith and Martell (1976)
      CO21  = 4.30E-07 * EXP( -1.00E+03 * TEMP1 )      ! Smith and Martell (1976)
      CO22  = 4.68E-11 * EXP( -1.76E+03 * TEMP1 )      ! Smith and Martell (1976)
      H2OW  = 1.00E-14 * EXP( -6.71E+03 * TEMP1 )      ! Smith and Martell (1976)
      NH31  = 1.70E-05 * EXP( -4.50E+02 * TEMP1 )      ! Smith and Martell (1976)
      HNO31 = 1.54E+01 * EXP(  8.70E+03 * TEMP1 )      ! Schwartz (1984)

!...Kinetic oxidation rates
!...   From Chamedies (1982)

      RH2O2 = 8.0E+04 * EXP( -3650.0 * TEMP1 )

!...From Kok

      RMHP = 1.75E+07 * EXP( -3801.0 * TEMP1 )
      RPAA = 3.64E+07 * EXP( -3994.0 * TEMP1 )

!...make initializations

      DO LIQ = 1, NLIQS
        WETDEP( LIQ ) = 0.0
      END DO

      DO IOX = 0, NUMOX
        DSIVDT( IOX ) = 0.0
        DTW   ( IOX ) = 0.0
        DS4   ( IOX ) = 0.0
      END DO

!...compute the initial accumulation aerosol 3rd moment

      M3OLD = ( AEROSOL( LSO4ACC ) * SGRAERMW( LSO4ACC ) / 1.8e6 &
            +   AEROSOL( LNH4ACC ) * SGRAERMW( LNH4ACC ) / 1.8e6 &
            +   AEROSOL( LNO3ACC ) * SGRAERMW( LNO3ACC ) / 1.8e6 &
            +   AEROSOL( LORGACC ) * SGRAERMW( LORGACC ) / 2.0e6 &
            +   AEROSOL( LPRIACC ) * SGRAERMW( LPRIACC ) / 2.2e6 )
!cc     &      * 6.0 / PI    ! cancels out in division at end of subroutine

!...compute fractional weights for several species

      NITAER = AEROSOL( LNO3ACC ) + AEROSOL( LNO3COR )
      IF ( NITAER .GT. 0.0 ) THEN
        FRACACC = AEROSOL( LNO3ACC ) / NITAER
        FRACCOR = AEROSOL( LNO3COR ) / NITAER
      ELSE
        FRACACC = 1.0
        FRACCOR = 0.0
      END IF
      
      TOTNIT = GAS( LHNO3 ) + AEROSOL( LNO3ACC ) + AEROSOL( LNO3COR )
      IF ( TOTNIT .GT. 0.0 ) THEN
        FHNO3   = GAS( LHNO3 ) / TOTNIT
        FNO3ACC = AEROSOL( LNO3ACC ) / TOTNIT
        FNO3COR = AEROSOL( LNO3COR ) / TOTNIT
      ELSE
        FHNO3   = 1.0
        FNO3ACC = 0.0
        FNO3COR = 0.0
      END IF

      TOTAMM = GAS( LNH3 ) + AEROSOL( LNH4ACC )
      IF ( TOTAMM .GT. 0.0 ) THEN
        FNH3    = GAS( LNH3 ) / TOTAMM
        FNH4ACC = AEROSOL( LNH4ACC ) / TOTAMM
      ELSE
        FNH3    = 1.0
        FNH4ACC = 0.0
      END IF

!...initial concentration from accumulation-mode aerosol loading (mol/liter)
!...  an assumption is made that all of the accumulation-mode
!...  aerosol mass in incorporated into the cloud droplets

      TS6ACCA = ( AEROSOL( LSO4ACC ) &
              +   GAS    ( LH2SO4  ) ) * PRES_ATM_OVER_XL
      NO3ACCA =   AEROSOL( LNO3ACC )   * PRES_ATM_OVER_XL
      NH4ACCA =   AEROSOL( LNH4ACC )   * PRES_ATM_OVER_XL
      ORGACCA =   AEROSOL( LORGACC )   * PRES_ATM_OVER_XL
      PRIACCA =   AEROSOL( LPRIACC )   * PRES_ATM_OVER_XL

!...initial concentration from coarse-mode aerosol loading (mol/liter)
!...  an assumption is made that all of the coarse-mode
!...  aerosol mass in incorporated into the cloud droplets

      CLA     = ( AEROSOL( LNACL   ) &
              +   AEROSOL( LKCL    ) ) * PRES_ATM_OVER_XL
      NO3CORA =   AEROSOL( LNO3COR )   * PRES_ATM_OVER_XL
      CAA     =   AEROSOL( LCACO3  )   * PRES_ATM_OVER_XL
      MGA     =   AEROSOL( LMGCO3  )   * PRES_ATM_OVER_XL
      NAA     =   AEROSOL( LNACL   )   * PRES_ATM_OVER_XL
      KA      =   AEROSOL( LKCL    )   * PRES_ATM_OVER_XL
      FEA     =   AEROSOL( LA3FE   )   * PRES_ATM_OVER_XL
      MNA     =   AEROSOL( LB2MN   )   * PRES_ATM_OVER_XL
      CO3A    = ( AEROSOL( LCACO3  ) &
              +   AEROSOL( LMGCO3  ) ) * PRES_ATM_OVER_XL
      PRICORA =   AEROSOL( LPRICOR )   * PRES_ATM_OVER_XL

!...set constant factors that will be used in later multiplications (moles/atm)

      XLH2O2  = H2O2H * XL
      XLO3    = O3H   * XL
      XLMHP   = MHPH  * XL
      XLPAA   = PAAH  * XL
      XLSO2   = SO2H  * XL
      XLNH3   = NH3H  * XL
      XLHNO3  = HNO3H * XL
      XLCO2   = CO2H  * XL

      SO212   = SO21  * SO22
      SO21H   = SO21  * SO2H
      SO212H  = SO212 * SO2H
      CO212   = CO21  * CO22
      CO21H   = CO21  * CO2H
      CO212H  = CO22  * CO21H
      NH3DH20 = NH31  / H2OW
      NH31HDH = NH3H  * NH3DH20
      FOA1H   = FOA1  * FOAH
      HNO31H  = HNO31 * HNO3H

!...If kinetic calculations are made, return to this point

      I20C = 0
20    CONTINUE

      I20C = I20C + 1
      IF ( I20C .GE. 1000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I20C'
!cc        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

!...set aitken-mode aerosol loading (mol/liter)

      NO3AKNA = AEROSOL( LNO3AKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      NH4AKNA = AEROSOL( LNH4AKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      TS6AKNA = AEROSOL( LSO4AKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      ORGAKNA = AEROSOL( LORGAKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      PRIAKNA = AEROSOL( LPRIAKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )

!...Initial gas phase partial pressures (atm)
!...   = initial partial pressure - amount deposited partial pressure

      PSO20  = GAS( LSO2  ) * PRES_ATM &
             + DS4( 0 ) * XL &
             - ( WETDEP(  8 ) + WETDEP(  9 ) + WETDEP( 10 ) ) * XC2
      PNH30  = GAS( LNH3  ) * PRES_ATM &
             + ( NH4ACCA + NH4AKNA ) * XL &
             - ( WETDEP(  2 ) + WETDEP( 15 ) ) * XC2
      PHNO30 = ( GAS( LHNO3 ) + 2.0 * GAS( LN2O5 ) ) * PRES_ATM &
             + ( NO3ACCA + NO3CORA + NO3AKNA ) * XL &
             - ( WETDEP( 14 ) + WETDEP( 32 ) ) * XC2
      PH2O20 = GAS( LH2O2 ) * PRES_ATM - WETDEP( 17 ) * XC2
      PO30   = GAS( LO3   ) * PRES_ATM - WETDEP( 18 ) * XC2
      PFOA0  = GAS( LFOA  ) * PRES_ATM &
             - ( WETDEP( 22 ) + WETDEP( 23 ) ) * XC2
      PMHP0  = GAS( LMHP  ) * PRES_ATM - WETDEP( 24 ) * XC2
      PPAA0  = GAS( LPAA  ) * PRES_ATM - WETDEP( 25 ) * XC2
      PCO20  = GAS( LCO2  ) * PRES_ATM &
             + CO3A * XL &
             - ( WETDEP( 11 ) + WETDEP( 12 ) + WETDEP( 13 ) ) * XC2

!...don't allow gas concentrations to go below zero

      PSO20  = MAX( PSO20,  0.0 )
      PNH30  = MAX( PNH30,  0.0 )
      PH2O20 = MAX( PH2O20, 0.0 )
      PO30   = MAX( PO30,   0.0 )
      PFOA0  = MAX( PFOA0,  0.0 )
      PMHP0  = MAX( PMHP0,  0.0 )
      PPAA0  = MAX( PPAA0,  0.0 )
      PCO20  = MAX( PCO20,  0.0 )
      PHNO30 = MAX( PHNO30, 0.0 )

!...Molar concentrations of soluble aerosols
!...   = Initial amount - amount deposited  (mol/liter)

      TS6     = TS6ACCA  + TS6AKNA &
              - ( WETDEP(  6 ) + WETDEP(  7 ) ) * XC1 &
              - DS4( 0 )
      CL      = CLA      -   WETDEP( 16 )  * XC1
      CA      = CAA      -   WETDEP(  3 )  * XC1
      MG      = MGA      -   WETDEP( 29 )  * XC1
      NA      = NAA      -   WETDEP(  4 )  * XC1
      K       = KA       -   WETDEP( 30 )  * XC1
      FE      = FEA      -   WETDEP( 19 )  * XC1
      MN      = MNA      -   WETDEP( 20 )  * XC1
      ORGN    = ORGACCA + ORGAKNA - WETDEP( 27 )  * XC1
      PRIM    = PRIACCA + PRIAKNA - WETDEP( 28 )  * XC1
      PRIMCOR = PRICORA  -   WETDEP( 33 )  * XC1
      A       = 3.0 * FE
      B       = 2.0 * MN

!...don't allow aerosol concentrations to go below zero

      TS6     = MAX( TS6,     0.0 )
      CL      = MAX( CL,      0.0 )
      CA      = MAX( CA,      0.0 )
      MG      = MAX( MG,      0.0 )
      NA      = MAX( NA,      0.0 )
      K       = MAX( K,       0.0 )
      FE      = MAX( FE,      0.0 )
      MN      = MAX( MN,      0.0 )
      ORGN    = MAX( ORGN,    0.0 )
      PRIM    = MAX( PRIM,    0.0 )
      PRIMCOR = MAX( PRIMCOR, 0.0 )
      A       = MAX( A,       0.0 )
      B       = MAX( B,       0.0 )

      SK6TS6 = SK6 * TS6

!...find solution of the equation using a method of reiterative
!...  bisections Make initial guesses for pH:   between .01  to  10.

      HA =  0.01
      HB = 10.0

      I7777C = 0
7777  CONTINUE

      I7777C = I7777C + 1
      IF ( I7777C .GE. 1000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I7777C'
!cc        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

      HA = MAX( HA - 0.8, 0.1 )
      HB = MIN( HB + 0.8, 9.9 )
      AE = 10.0**( -HA )

      RECIPA1 = 1.0 / ( AE * ACT1 )
      RECIPA2 = 1.0 / ( AE * AE * ACT2 )

!...calculate final gas phase partial pressure of SO2, NH3, HNO3
!...  HCOOH, and CO2 (atm)

      PSO2F = PSO20 / ( 1.0 + XLSO2 * ( 1.0 + SO21 * RECIPA1 &
            + SO212 * RECIPA2 ) )

      PNH3F = PNH30 / ( 1.0 + XLNH3 * ( 1.0 + NH3DH20 * AE ) )

      PFOAF = PFOA0 / ( 1.0 + XL * ( FOAH + FOA1H * RECIPA1 ) )

      PHNO3F = PHNO30 / ( 1.0 + XLHNO3 * ( 1.0 + HNO31 * RECIPA1 ) )

      PCO2F = PCO20 / ( 1.0 + XLCO2 * ( 1.0 + CO21 * RECIPA1 &
            + CO212 * RECIPA2 ) )

!...calculate liquid phase concentrations (moles/liter)

      SO4  = SK6TS6 / ( AE * GM2 + SK6 )
      HSO4 = TS6 - SO4
      SO3  = SO212H  * PSO2F  * RECIPA2
      HSO3 = SO21H   * PSO2F  * RECIPA1
      CO3  = CO212H  * PCO2F  * RECIPA2
      HCO3 = CO21H   * PCO2F  * RECIPA1
      OH   = H2OW    * RECIPA1
      NH4  = NH31HDH * PNH3F  * AE
      HCO2 = FOA1H   * PFOAF  * RECIPA1
      NO3  = HNO31H  * PHNO3F * RECIPA1

!...compute functional value

      FA = AE + NH4 + 2.0 *  (CA + MG - CO3 - SO3 - SO4 ) - OH - HCO3 &
         - HSO3 - NO3 - HSO4 - HCO2

!...Start iteration and bisection ****************<<<<<<<

      I30C = 0
30    CONTINUE

      I30C = I30C + 1
      IF ( I30C .GE. 1000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I30C'
!cc        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

      BB = ( HA + HB ) / 2.0
      AE = 10.0**( -BB )

      ICNTAQ = ICNTAQ + 1
      IF ( ICNTAQ .GE. 3000 ) THEN
        XMSG = 'Maximum AQCHEM total iterations exceeded'
!cc        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

      RECIPA1 = 1.0 / ( AE * ACT1 )
      RECIPA2 = 1.0 / ( AE * AE * ACT2 )

!...calculate final gas phase partial pressure of SO2, NH3, HNO3
!...  HCOOH, and CO2 (atm)

      PSO2F = PSO20 / ( 1.0 + XLSO2 &
      	    * ( 1.0 + SO21 * RECIPA1 + SO212 * RECIPA2 ) )

      PNH3F = PNH30 / ( 1.0 + XLNH3 * ( 1.0 + NH3DH20 * AE ) )

      PHNO3F = PHNO30 / ( 1.0 + XLHNO3 * ( 1.0 + HNO31 * RECIPA1 ) )

      PFOAF = PFOA0 / ( 1.0 + XL * ( FOAH + FOA1H * RECIPA1 ) )

      PCO2F = PCO20 / ( 1.0 + XLCO2 * ( 1.0 + CO21 * RECIPA1 &
            + CO212 * RECIPA2 ) )

!...calculate liquid phase concentrations (moles/liter)

      SO4  = SK6TS6 / ( AE * GM2 + SK6 )
      HSO4 = TS6 - SO4
      SO3  = SO212H  * PSO2F  * RECIPA2
      HSO3 = SO21H   * PSO2F  * RECIPA1
      CO3  = CO212H  * PCO2F  * RECIPA2
      HCO3 = CO21H   * PCO2F  * RECIPA1
      OH   = H2OW    * RECIPA1
      NH4  = NH31HDH * PNH3F  * AE
      HCO2 = FOA1H   * PFOAF  * RECIPA1
      NO3  = HNO31H  * PHNO3F * RECIPA1

!...compute functional value

      FB = AE + NH4 + 2.0 * ( CA + MG - CO3 - SO3 - SO4 ) - OH - HCO3 &
         - HSO3 - NO3 - HSO4 - HCO2

!...Calculate and check the sign of the product of the two functional values

      FTST = FA * FB
      IF ( FTST .LE. 0.0 ) THEN
        HB = BB
      ELSE
        HA = BB
        FA = FB
      END IF

!...Check convergence of solutions

      HTST = HA / HB
      IF ( HTST .LE. TST ) GO TO 30

!...end of zero-finding routine ****************<<<<<<<<<<<<

!...compute Ionic strength and activity coefficient by the Davies equation

      STION = 0.5 * (AE + NH4 + OH + HCO3 + HSO3 &
            + 4.0 * (SO4 + CO3 + SO3 + CA + MG + MN) &
            + NO3 + HSO4 + 9.0 * FE + NA + K + CL + A + B + HCO2)
      GM1LOG = -0.509 * ( SQRT( STION ) &
             / ( 1.0 + SQRT( STION ) ) - 0.2 * STION )
      GM2LOG = GM1LOG * 4.0
      GM1  = 10.0**GM1LOG
      GM2  = MAX( 10.0**GM2LOG, 1.0E-30 )
      ACTB = ACT1
      ACT1 = MAX( GM1 * GM1, 1.0E-30 )
      ACT2 = MAX( GM1 * GM1 * GM2, 1.0E-30 )

!...check for convergence and possibly go to 7777, to recompute
!...  Gas and liquid phase concentrations

      TAC = ABS( ACTB - ACT1 ) / ACTB
      IF ( TAC .GE. 1.0E-2 ) GO TO 7777

!...return an error if the pH is not in range

!cc      IF ( ( HA .LT. 0.02 ) .OR. ( HA .GT. 9.49 ) ) THEN
      IF ( ( HA .LT. 0.1 ) .OR. ( HA .GT. 9.9 ) ) THEN
        print *, ha
!       XMSG = 'PH VALUE OUT OF RANGE'
!CC        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
      END IF

!...Make those concentration calculations which can be made outside
!...  of the function.

      SO2L = SO2H * PSO2F
      AC = 10.0**( -BB )
      SIV = SO3 + HSO3 + SO2L

!...Calculate final gas phase concentrations of oxidants (atm)

      PH2O2F = ( PH2O20 + XL * DS4( 1 ) ) / ( 1.0 + XLH2O2 )
      PO3F   = ( PO30   + XL * DS4( 2 ) ) / ( 1.0 + XLO3   )
      PMHPF  = ( PMHP0  + XL * DS4( 4 ) ) / ( 1.0 + XLMHP  )
      PPAAF  = ( PPAA0  + XL * DS4( 5 ) ) / ( 1.0 + XLPAA  )

      PH2O2F = MAX( PH2O2F, 0.0 )
      PO3F   = MAX( PO3F,   0.0 )
      PMHPF  = MAX( PMHPF,  0.0 )
      PPAAF  = MAX( PPAAF,  0.0 )

!...Calculate liquid phase concentrations of oxidants (moles/liter)

      H2O2L = PH2O2F * H2O2H
      O3L   = PO3F   * O3H
      MHPL  = PMHPF  * MHPH
      PAAL  = PPAAF  * PAAH
      FOAL  = PFOAF  * FOAH
      NH3L  = PNH3F  * NH3H
      CO2L  = PCO2F  * CO2H
      HNO3L = PHNO3F * HNO3H

!...load the liquid concentration array with current values

      LIQUID(  1 ) = AC
      LIQUID(  2 ) = NH4
      LIQUID(  3 ) = CA
      LIQUID(  4 ) = NA
      LIQUID(  5 ) = OH
      LIQUID(  6 ) = SO4
      LIQUID(  7 ) = HSO4
      LIQUID(  8 ) = SO3
      LIQUID(  9 ) = HSO3
      LIQUID( 10 ) = SO2L
      LIQUID( 11 ) = CO3
      LIQUID( 12 ) = HCO3
      LIQUID( 13 ) = CO2L
      LIQUID( 14 ) = NO3
      LIQUID( 15 ) = NH3L
      LIQUID( 16 ) = CL
      LIQUID( 17 ) = H2O2L
      LIQUID( 18 ) = O3L
      LIQUID( 19 ) = FE
      LIQUID( 20 ) = MN
      LIQUID( 21 ) = A
      LIQUID( 22 ) = FOAL
      LIQUID( 23 ) = HCO2
      LIQUID( 24 ) = MHPL
      LIQUID( 25 ) = PAAL
      LIQUID( 26 ) = 0.0
      LIQUID( 27 ) = ORGN
      LIQUID( 28 ) = PRIM
      LIQUID( 29 ) = MG
      LIQUID( 30 ) = K
      LIQUID( 31 ) = B
      LIQUID( 32 ) = HNO3L
      LIQUID( 33 ) = PRIMCOR

!...if the maximum cloud lifetime has not been reached, the compute
!...  the next timestep.

      IF ( TIMEW .LT. TAUCLD ) THEN

!...make kinetics calculations
!...  note: DS4(i) and DSIV(I) are negative numbers!

        DTRMV = 300.0
        IF ( ( CTHK1 .GT. 1.0E-10 ) .AND. ( PRCRATE .GT. 1.0E-10 ) ) &
           DTRMV = 3.6 * WTAVG * 1000.0 * CTHK1 / PRCRATE  ! <<<uma found bug, was .36
        DTRMV = MIN( DTRMV, 300.0 )
        ITERAT = ITERAT + 1

!...Define the total S(iv) available for oxidation

        TSIV = PSO20 * ONE_OVER_XL

!...Calculate sulfur iv oxidation rate due to H2O2

        DSIVDT( 1 ) = -RH2O2 * H2O2L * SO2L / ( 0.1 + AC )
        TOTOX = PH2O20 * ONE_OVER_XL
        IF ( ( DSIVDT( 1 ) .EQ. 0.0 ) .OR. &
             ( TSIV  .LE. CONCMIN ) .OR. &
             ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 1 ) = DTRMV
        ELSE
          DTW( 1 ) = -0.05 * MIN( TOTOX, TSIV ) / DSIVDT( 1 )
        END IF

!...Calculate sulfur iv oxidation rate due to O3

        IF ( BB .GE. 2.7 ) THEN
          DSIVDT( 2 ) = -4.19E5 * ( 1.0 + 2.39E-4 / AC ) * O3L * SIV
        ELSE
          DSIVDT( 2 ) = -1.9E4 * SIV * O3L / SQRT( AC )
        END IF
        TOTOX = PO30 * ONE_OVER_XL
        IF ( ( DSIVDT( 2 ) .EQ. 0.0 ) .OR. &
             ( TSIV  .LE. CONCMIN ) .OR. &
             ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 2 ) = DTRMV
        ELSE
          DTW( 2 ) = -0.01 * MIN( TOTOX, TSIV ) / DSIVDT( 2 )
        END IF

!...Calculate sulfur iv oxidation rate due to 02 catalyzed by Mn++
!...  and Fe+++  See Table IV Walcek & Taylor ( 1986)

        IF ( BB .GE. 4.0 )  THEN  ! 4.0  < pH

          IF ( SIV .LE. 1.0E-5 ) THEN
            DSIVDT( 3 ) = -5000.0 * MN * HSO3
          ELSE IF ( SIV .GT. 1.0E-5 ) THEN
            DSIVDT( 3 ) = -( 4.7 * MN * MN / AC &
                        + 1.0E7 * FE * SIV * SIV )
          END IF  ! end of first pass through SIV conc.

        ELSE          ! pH , + 4.0

	  IF ( SIV .LE. 1.0E-5 ) THEN
            DSIVDT( 3 ) = -3.0 * ( 5000.0 * MN * HSO3 &
                        + 0.82 * FE * SIV / AC )
          ELSE
            DSIVDT( 3 ) = -( 4.7 * MN * MN / AC &
                        + ( 0.82 * FE * SIV / AC ) &
                        * ( 1.0 + 1.7E3 * MN**1.5 / ( 6.3E-6 + FE ) ) )
          END IF ! end of second pass through SIV conc.

        END IF  ! end of pass through pH

        IF ( ( DSIVDT( 3 ) .EQ. 0.0 ) .OR. ( TSIV .LE. CONCMIN ) ) THEN
          DTW( 3 ) = DTRMV
        ELSE
          DTW( 3 ) = -0.1 * TSIV / DSIVDT( 3 )
        END IF

!...Calculate sulfur oxidation rate due to MHP

        DSIVDT( 4 ) = -RMHP * AC * MHPL * HSO3
        TOTOX = PMHP0 * ONE_OVER_XL
        IF ( ( DSIVDT( 4 ) .EQ. 0.0 ) .OR.    &
             ( TSIV  .LE. CONCMIN ) .OR.    &
             ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 4 ) = DTRMV
        ELSE
          DTW( 4 ) = -0.1 * MIN( TOTOX, TSIV ) / DSIVDT( 4 )
        END IF

!...Calculate sulfur oxidation due to PAA

        DSIVDT( 5 ) = -RPAA * HSO3 * PAAL * ( AC + 1.65E-5 )
        TOTOX = PPAA0 * ONE_OVER_XL
        IF ( ( DSIVDT( 5 ) .EQ. 0.0 ) .OR.   &
             ( TSIV  .LE. CONCMIN ) .OR.     &
             ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 5 ) = DTRMV
        ELSE
          DTW( 5 ) = -0.1 * MIN( TOTOX, TSIV ) / DSIVDT( 5 )
        END IF

!...Calculate total sulfur iv oxidation rate

        DSIVDT( 0 ) = 0.0
        DO IOX = 1, NUMOX
          DSIVDT( 0 ) = DSIVDT( 0 ) + DSIVDT( IOX )
        END DO

!...Calculate a minimum time step required

        DTW( 0 ) = MIN( DTW( 1 ), DTW( 2 ), DTW( 3 ), &
                        DTW( 4 ), DTW( 5 ) )

!...check for large time step

        IF ( DTW( 0 ) .GT. 8.0E+37 ) THEN
          WRITE(6,1001) PRCRATE, DSIVDT(0), TS6, DTW(0), CTHK1, WTAVG
        ELSE

!...calculate the change in sulfur iv for this time step

60        DTS6 = ABS( DTW( 0 ) * ( -DSIVDT( 0 ) - TS6 * PRCRATE &
               / ( 3600.0 * CTHK1 * WTAVG ) ) )

!...If DSIV(0), sulfur iv oxidized during this time step would be
!... less than 5% of sulfur oxidized since time 0, then double DT

          IF ( DTW( 0 ) .LE. TAUCLD ) THEN
            IF ( DTS6 .LT. 0.05 * TS6 ) THEN
              DTW( 0 ) = DTW( 0 ) * 2.0
              GO TO 60
            END IF
          END IF
        END IF
        DTW( 0 ) = MIN( REAL(DTW( 0 )), DTRMV )

!...Limit the timestep to prevent negative SO2 concentrations and mass creation
!...  for sulfate (suggested by Bonyoung Koo, ENVIRON Corp)

        IF ( DSIVDT( 0 ) .LT. 0.0 ) THEN
          DTW( 0 ) = MIN( REAL(DTW( 0 )), -TSIV * 1.00001 / DSIVDT( 0 ) )
        ENDIF

!...alternative bug fix (BUT EXPENSIVE)

!        if ( ( -DSIVDT( 0 ) * DTW( 0 ) ) .gt. TSIV ) then
!          dtw( 0 ) = -TSIV * 1.00001 / DSIVDT( 0 )
!        end if

!...If the total time after this time increment will be greater than
!...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        IF ( TIMEW + DTW( 0 ) .GT. TAUCLD ) DTW( 0 ) = TAUCLD - TIMEW
!cc        IF ( TS6 .LT. 1.0E-11 ) DTW( 0 ) = TAUCLD - TIMEW
        IF ( ITERAT .GT. 100 ) DTW( 0 ) = TAUCLD - TIMEW

!       print *, iterat, dtw(0)

!...Set DSIV(I), I = 0,NUMOX, the amount of S(IV) oxidized by each
!... individual oxidizing agent, as well as the total.

        DO IOX = 0, NUMOX
          DS4( IOX ) = DS4( IOX ) + DTW( 0 ) * DSIVDT( IOX )
        END DO

!...Compute depositions and concentrations for each species

        WETFAC = PRCRATE * FRACLIQ * DTW( 0 ) * SEC2HR
        DO LIQ = 1, NLIQS
          WETDEP( LIQ ) = WETDEP( LIQ ) + LIQUID( LIQ ) * WETFAC
        END DO

        TIMEW = TIMEW + DTW( 0 )

!...Return to make additional calculations

        GO TO 20
      END IF

!...At this point, TIMEW=TAUCLD
!...  compute the scavenging coefficient for SO4 which will be used for
!...  scavenging aerosol number in the accumulation and coarse mode

      DEPSUM = ( WETDEP( 6 ) + WETDEP( 7 ) ) * XC1

      IF ( ( TS6ACCA + TS6AKNA - DS4( 0 ) ) .NE. 0.0 ) THEN
        BETASO4 = DEPSUM / ( ( TS6ACCA + TS6AKNA - DS4( 0 ) ) * TAUCLD )
      ELSE
        BETASO4 = 0.0
      END IF

      EBETASO4T = EXP( -BETASO4 * TAUCLD )
      EALFA0T   = EXP( -ALFA0 * TAUCLD )
      EALFA2T   = EXP( -ALFA2 * TAUCLD )
      EALFA3T   = EXP( -ALFA3 * TAUCLD )

!...Compute the output concentrations and wetdeposition amounts

      TOTAMM = ( PNH3F  + ( NH4 + NH3L  ) * XL ) * RECIPAP1
      TOTNIT = ( PHNO3F + ( NO3 + HNO3L ) * XL ) * RECIPAP1

!...gas-phase species wet deposition (mm mol/lit)
       if(prcrate.gt.1.e-10)then
      GASWDEP( LSO2   ) =  ( WETDEP(  8 ) + WETDEP(  9 ) + WETDEP( 10 ) ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LNH3   ) = WETDEP( 15 )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LH2O2  ) = WETDEP( 17 )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LO3    ) = WETDEP( 18 )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LCO2   ) = (WETDEP( 11 ) + WETDEP( 12 ) + WETDEP( 13 ) )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LFOA   ) = (WETDEP( 22 ) + WETDEP( 23 ) )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LMHP   ) = WETDEP( 24 )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LPAA   ) = WETDEP( 25 )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LHNO3  ) = WETDEP( 32 )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      GASWDEP( LN2O5  ) = 0.0
      GASWDEP( LH2SO4 ) = 0.0

!...gas concentrations (mol/molV)


!...aerosol species wet deposition (mm mol/lit)
!...  there is no wet deposition of aitken particles, they attached
!...  to the accumulation mode particles

      AERWDEP( LSO4AKN ) = 0.0
      AERWDEP( LSO4ACC ) = ( WETDEP(  6 ) + WETDEP(  7 ) ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LNH4AKN ) = 0.0
      AERWDEP( LNH4ACC ) = WETDEP(  2 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LNO3AKN ) = 0.0
      AERWDEP( LNO3ACC ) = WETDEP( 14 ) * FRACACC * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LNO3COR ) = WETDEP( 14 ) * FRACCOR * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LORGAKN ) = 0.0
      AERWDEP( LORGACC ) = WETDEP( 27 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LPRIAKN ) = 0.0
      AERWDEP( LPRIACC ) = WETDEP( 28 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LPRICOR ) = WETDEP( 33 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LNACL   ) = WETDEP(  4 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LA3FE   ) = WETDEP( 19 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LB2MN   ) = WETDEP( 20 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LCACO3  ) = WETDEP(  3 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LMGCO3  ) = WETDEP( 29 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LKCL    ) = WETDEP( 30 ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LNUMAKN ) = 0.0
      AERWDEP( LNUMACC ) = AEROSOL( LNUMACC ) * AIRM &
                         * ( 1.0 - EBETASO4T ) * XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LNUMCOR ) = AEROSOL( LNUMCOR ) * AIRM &
                         * ( 1.0 - EBETASO4T )* XL * RECIPAP1 / PRCRATE * FRACLIQ * taucld * SEC2HR
      AERWDEP( LSRFAKN ) = 0.0
      AERWDEP( LSRFACC ) = 0.0

	endif
      GAS( LSO2   ) = ( PSO2F   + XL *  SIV )   * RECIPAP1
      GAS( LNH3   ) = FNH3 * TOTAMM
      GAS( LH2O2  ) = ( PH2O2F  + XL *  H2O2L ) * RECIPAP1
      GAS( LO3    ) = ( PO3F    + XL *  O3L )   * RECIPAP1
      GAS( LCO2   ) = ( PCO2F   + XL *  CO2L )  * RECIPAP1
      GAS( LFOA   ) = ( PFOAF   + XL * ( FOAL              &
                    +  HCO2 ) ) * RECIPAP1
      GAS( LMHP   ) = ( PMHPF   + XL *  MHPL )  * RECIPAP1
      GAS( LPAA   ) = ( PPAAF   + XL *  PAAL )  * RECIPAP1
      GAS( LHNO3  ) = FHNO3 * TOTNIT
      GAS( LN2O5  ) = 0.0 ! assume all into aerosol
      GAS( LH2SO4 ) = 0.0 ! assume all into aerosol

!...aerosol concentrations (mol/molV)

      AEROSOL( LSO4AKN ) = AEROSOL( LSO4AKN ) * EALFA3T
      AEROSOL( LSO4ACC ) = TS6    * XL * RECIPAP1
      AEROSOL( LNH4AKN ) = AEROSOL( LNH4AKN ) * EALFA3T
      AEROSOL( LNH4ACC ) = FNH4ACC * TOTAMM
      AEROSOL( LNO3AKN ) = AEROSOL( LNO3AKN ) * EALFA3T
      AEROSOL( LNO3ACC ) = FNO3ACC * TOTNIT
      AEROSOL( LNO3COR ) = FNO3COR * TOTNIT
      AEROSOL( LORGAKN ) = AEROSOL( LORGAKN ) * EALFA3T
      AEROSOL( LORGACC ) = ORGN   * XL * RECIPAP1
      AEROSOL( LPRIAKN ) = AEROSOL( LPRIAKN ) * EALFA3T
      AEROSOL( LPRIACC ) = PRIM   * XL * RECIPAP1
      AEROSOL( LPRICOR ) = PRIMCOR* XL * RECIPAP1
      AEROSOL( LNACL   ) = NA     * XL * RECIPAP1
      AEROSOL( LA3FE   ) = FE     * XL * RECIPAP1
      AEROSOL( LB2MN   ) = MN     * XL * RECIPAP1
      AEROSOL( LCACO3  ) = CA     * XL * RECIPAP1
      AEROSOL( LMGCO3  ) = MG     * XL * RECIPAP1
      AEROSOL( LKCL    ) = K      * XL * RECIPAP1
      AEROSOL( LNUMAKN ) = AEROSOL( LNUMAKN ) * EALFA0T
      AEROSOL( LNUMACC ) = AEROSOL( LNUMACC ) * EBETASO4T
      AEROSOL( LNUMCOR ) = AEROSOL( LNUMCOR ) * EBETASO4T

!...compute the final accumulation aerosol 3rd moment

      M3NEW = ( AEROSOL( LSO4ACC ) * SGRAERMW( LSO4ACC ) / 1.8e6 &
            +   AEROSOL( LNH4ACC ) * SGRAERMW( LNH4ACC ) / 1.8e6 &
            +   AEROSOL( LNO3ACC ) * SGRAERMW( LNO3ACC ) / 1.8e6 &
            +   AEROSOL( LORGACC ) * SGRAERMW( LORGACC ) / 2.0e6 &
            +   AEROSOL( LPRIACC ) * SGRAERMW( LPRIACC ) / 2.2e6 )
!CC     &      * 6.0 / PI      ! cancels out in division below

      AEROSOL( LSRFAKN ) = AEROSOL( LSRFAKN ) * EALFA2T
      AEROSOL( LSRFACC ) = AEROSOL( LSRFACC    )                         &
                         * ( EXP( -BETASO4 * TAUCLD * ONETHIRD ) )       &
                         * ( M3NEW / MAX( M3OLD, CONCMIN) ) ** TWOTHIRDS

!...store the amount of hydrogen deposition

      HPWDEP = WETDEP( 1 )

      RETURN

!...formats

1001  FORMAT (1X,'STORM RATE=', F6.3, 'DSIVDT(0) =', F10.5,     &
             'TS6=', F10.5, 'DTW(0)=', F10.5, 'CTHK1=', F10.5,  &
             'WTAVG=', F10.5)

END SUBROUTINE AQCHEM
!.........................................................................

        INTEGER  FUNCTION  TRIMLEN ( STRING )

!***********************************************************************
!  function body starts at line 43
!
!  FUNCTION:  return the effective length of argument CHARACTER*(*) STRING,
!             after trailing blanks have been trimmed.
!
!  PRECONDITIONS REQUIRED:  none
!
!  SUBROUTINES AND FUNCTIONS CALLED:  none
!
!  REVISION  HISTORY:  
!             Prototype 8/91 by CJC
!             Version 2/93 for CRAY by CJC
!
!***********************************************************************

      IMPLICIT NONE


!...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)	STRING


!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER  	L, K


!***********************************************************************
!   begin body of function  TRIMLEN

        L = LEN( STRING )
        DO  11  K = L, 1, -1
            IF ( STRING( K:K ) .NE. ' ' ) THEN
                GO TO  12
            END IF
11      CONTINUE

        K = 1

12      CONTINUE

        TRIMLEN = K

!       RETURN

END FUNCTION TRIMLEN


!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/CCTM/src/cloud/cloud_radm/hlconst.F,v 1.8 2002/05/02 20:43:08 sjr Exp $ 

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

      REAL FUNCTION HLCONST ( NAME, TEMP, EFFECTIVE, HPLUS )

!-----------------------------------------------------------------------
!
!  FUNCTION: return the Henry's law constant for the specified substance
!            at the given temperature
!
!  revision history:
!    who        when           what
!  ---------  --------  -------------------------------------
!  S.Roselle  08/15/97  code written for Models-3
!  J.Gipson   06/18/01  added Henry's Law constants 50-55 for saprc99
!  W.Hutzell  07/03/01  added Henry's Law constants 56-57 for Atrazine
!                       and the daughter products from Atrazine and OH
!                       reactions.
!  J.Gipson.  09/06/02  added Henry's Law constants 59-73   for toxicz
!  S.Roselle  11/07/02  added capability for calculating the effective
!                       Henry's law constant and updated coefficients
!                       in Henry's law constant table
!-----------------------------------------------------------------------

      IMPLICIT NONE

!...........INCLUDES and their descriptions

!     INCLUDE 'IODECL3.EXT'            ! I/O definitions and declarations
!     INCLUDE 'PARMS3.EXT'             ! I/O parameters definitions

!...........PARAMETERS and their descriptions:

      INTEGER       MXSPCS              ! Number of substances
      PARAMETER   ( MXSPCS = 73 )

      INTEGER       MXDSPCS             ! Number of dissociating species
      PARAMETER   ( MXDSPCS = 12 )

!...........ARGUMENTS and their descriptions

      CHARACTER*(*) NAME                ! name of substance
      REAL          TEMP                ! temperature (K)
      LOGICAL       EFFECTIVE           ! true=compute the effective henry's law constant
      REAL          HPLUS               ! hydrogen ion concentration (mol/l)

!...........SCRATCH LOCAL VARIABLES and their descriptions:

      CHARACTER*16  PNAME               ! program name
      DATA          PNAME / 'HLCONST' /
      SAVE          PNAME
      CHARACTER*16  SUBNAME( MXSPCS )   ! list of substance names
      SAVE          SUBNAME
      CHARACTER*120 XMSG                ! Exit status message
      DATA          XMSG / ' ' /

      INTEGER       SPC                 ! species index
      INTEGER       LSO2                ! SO2 pointer
      INTEGER       LHSO3               ! HSO3 pointer
      INTEGER       LHNO2               ! HNO3 pointer
      INTEGER       LHNO3               ! HNO3 pointer
      INTEGER       LCO2                ! CO2 pointer
      INTEGER       LHCO3               ! HCO3 pointer
      INTEGER       LH2O2               ! H2O2 pointer
      INTEGER       LHCHO               ! HCHO pointer
      INTEGER       LHCOOH              ! HCOOH pointer
      INTEGER       LHO2                ! HO2 pointer
      INTEGER       LNH4OH              ! NH4OH pointer
      INTEGER       LH2O                ! H2O pointer

      REAL          HPLUSI              ! 1 / HPLUS
      REAL          HPLUS2I             ! 1 / HPLUS**2
      REAL          TFAC                ! (298-T)/(T*298)
      REAL          AKEQ1               ! temp var for dissociation constant
      REAL          AKEQ2               ! temp var for dissociation constant
      REAL          OHION               ! OH ion concentration
      REAL          KH                  ! temp var for henry's law constant
      REAL          A( MXSPCS )         ! Henry's law constants at 298.15K (M/atm) (taken from Rolf Sanders'
      SAVE          A                   !  Compilation of Henry's Law Constants for Inorganic and Organic Species
                                        !  of Potential Importance in Environment Chemistry 1999)
      REAL          E( MXSPCS )         ! enthalpy (like activation energy) (K) (taken from Rolf Sanders'
      SAVE          E                   !  Compilation of Henry's Law Constants for Inorganic and Organic Species
                                        !  of Potential Importance in Environment Chemistry 1999)
      REAL          B( MXDSPCS )        ! dissociation constant at 298.15K (M or M2) (taken from Table 6.A.1,
      SAVE          B                   !  Seinfeld and Pandis, Atmospheric Chemistry and Physics, 1997)
      REAL          D( MXDSPCS )        ! -dH/R (K) (taken from Table 6.A.1,
      SAVE          D                   !  Seinfeld and Pandis, Atmospheric Chemistry and Physics, 1997)

      DATA SUBNAME(  1), A(  1), E(  1) / 'O3              ', 1.2E-02, 2.7E+03 /  ! Chameides 1984
      DATA SUBNAME(  2), A(  2), E(  2) / 'HO2             ', 4.0E+03, 5.9E+03 /  ! Hanson et al. 1992
      DATA SUBNAME(  3), A(  3), E(  3) / 'H2O2            ', 8.3E+04, 7.4E+03 /  ! OSullivan et al. 1996
      DATA SUBNAME(  4), A(  4), E(  4) / 'NH3             ', 6.1E+01, 4.2E+03 /  ! Clegg and Brimblecombe 1989
      DATA SUBNAME(  5), A(  5), E(  5) / 'NO              ', 1.9E-03, 1.4E+03 /  ! Lide and Frederikse 1995
      DATA SUBNAME(  6), A(  6), E(  6) / 'NO2             ', 1.2E-02, 2.5E+03 /  ! Chameides 1984
      DATA SUBNAME(  7), A(  7), E(  7) / 'NO3             ', 2.0E+00, 2.0E+03 /  ! Thomas et al. 1993
      DATA SUBNAME(  8), A(  8), E(  8) / 'N2O5            ', 1.0E+30, 0.0E+00 /  ! "inf" Sander and Crutzen 1996
      DATA SUBNAME(  9), A(  9), E(  9) / 'HNO2            ', 5.0E+01, 4.9E+03 /  ! Becker et al. 1996
      DATA SUBNAME( 10), A( 10), E( 10) / 'HNO3            ', 2.1E+05, 8.7E+03 /  ! Leieveld and Crutzen 1991
      DATA SUBNAME( 11), A( 11), E( 11) / 'HNO4            ', 1.2E+04, 6.9E+03 /  ! Regimbal and Mozurkewich 1997
      DATA SUBNAME( 12), A( 12), E( 12) / 'SO2             ', 1.4E+00, 2.9E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 13), A( 13), E( 13) / 'H2SO4           ', 1.0E+30, 0.0E+00 /  ! infinity
      DATA SUBNAME( 14), A( 14), E( 14) / 'METHANE         ', 1.4E-03, 1.6E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 15), A( 15), E( 15) / 'ETHANE          ', 1.9E-03, 2.3E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 16), A( 16), E( 16) / 'PROPANE         ', 1.5E-03, 2.7E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 17), A( 17), E( 17) / 'BUTANE          ', 1.1E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 18), A( 18), E( 18) / 'PENTANE         ', 8.1E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 19), A( 19), E( 19) / 'HEXANE          ', 6.0E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 20), A( 20), E( 20) / 'OCTANE          ', 3.4E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 21), A( 21), E( 21) / 'NONANE          ', 2.0E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 22), A( 22), E( 22) / 'DECANE          ', 1.4E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 23), A( 23), E( 23) / 'ETHENE          ', 4.7E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 24), A( 24), E( 24) / 'PROPENE         ', 4.8E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 25), A( 25), E( 25) / 'ISOPRENE        ', 2.8E-02, 0.0E+00 /  ! Karl and Lindinger 1997
      DATA SUBNAME( 26), A( 26), E( 26) / 'ACETYLENE       ', 4.1E-02, 1.8E+03 /  ! Wilhelm et al. 1977
      DATA SUBNAME( 27), A( 27), E( 27) / 'BENZENE         ', 1.6E-01, 4.1E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 28), A( 28), E( 28) / 'TOLUENE         ', 1.5E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 29), A( 29), E( 29) / 'O-XYLENE        ', 1.9E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 30), A( 30), E( 30) / 'METHANOL        ', 2.2E+02, 0.0E+00 /  ! Snider and Dawson 1985
      DATA SUBNAME( 31), A( 31), E( 31) / 'ETHANOL         ', 1.9E+02, 6.6E+03 /  ! Snider and Dawson 1985
      DATA SUBNAME( 32), A( 32), E( 32) / '2-CRESOL        ', 8.2E+02, 0.0E+00 /  ! Betterton 1992
      DATA SUBNAME( 33), A( 33), E( 33) / '4-CRESOL        ', 1.3E+02, 0.0E+00 /  ! Betterton 1992
      DATA SUBNAME( 34), A( 34), E( 34) / 'METHYLHYDROPEROX', 3.1E+02, 5.2E+03 /  ! OSullivan et al. 1996
      DATA SUBNAME( 35), A( 35), E( 35) / 'FORMALDEHYDE    ', 3.2E+03, 6.8E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 36), A( 36), E( 36) / 'ACETALDEHYDE    ', 1.4E+01, 5.6E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 37), A( 37), E( 37) / 'GENERIC_ALDEHYDE', 4.2E+03, 0.0E+00 /  ! Graedel and Goldberg 1983
      DATA SUBNAME( 38), A( 38), E( 38) / 'GLYOXAL         ', 3.6E+05, 0.0E+00 /  ! Zhou and Mopper 1990
      DATA SUBNAME( 39), A( 39), E( 39) / 'ACETONE         ', 3.0E+01, 4.6E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 40), A( 40), E( 40) / 'FORMIC_ACID     ', 8.9E+03, 6.1E+03 /  ! Johnson et al. 1996
      DATA SUBNAME( 41), A( 41), E( 41) / 'ACETIC_ACID     ', 4.1E+03, 6.3E+03 /  ! Johnson et al. 1996
      DATA SUBNAME( 42), A( 42), E( 42) / 'METHYL_GLYOXAL  ', 3.2E+04, 0.0E+00 /  ! Zhou and Mopper 1990
      DATA SUBNAME( 43), A( 43), E( 43) / 'CO              ', 9.9E-04, 1.3E+03 /  ! Linde and Frederikse 1995
      DATA SUBNAME( 44), A( 44), E( 44) / 'CO2             ', 3.6E-02, 2.2E+03 /  ! Zheng et al. 1997
      DATA SUBNAME( 45), A( 45), E( 45) / 'PAN             ', 2.8E+00, 6.5E+03 /  ! Kames et al. 1991
      DATA SUBNAME( 46), A( 46), E( 46) / 'MPAN            ', 1.7E+00, 0.0E+00 /  ! Kames and Schurath 1995
      DATA SUBNAME( 47), A( 47), E( 47) / 'OH              ', 3.0E+01, 4.5E+03 /  ! Hanson et al. 1992
      DATA SUBNAME( 48), A( 48), E( 48) / 'METHYLPEROXY_RAD', 2.0E+03, 6.6E+03 /  ! Lelieveld and Crutzen 1991
      DATA SUBNAME( 49), A( 49), E( 49) / 'PEROXYACETIC_ACI', 8.4E+02, 5.3E+03 /  ! OSullivan et al. 1996
      DATA SUBNAME( 50), A( 50), E( 50) / 'PROPANOIC_ACID  ', 5.7E+03, 0.0E+00 /  ! Kahn et al. 1995
      DATA SUBNAME( 51), A( 51), E( 51) / '2-NITROPHENOL   ', 7.0E+01, 4.6E+03 /  ! USEPA 1982
      DATA SUBNAME( 52), A( 52), E( 52) / 'PHENOL          ', 1.9E+03, 7.3E+03 /  ! USEPA 1982
      DATA SUBNAME( 53), A( 53), E( 53) / 'BIACETYL        ', 7.4E+01, 5.7E+03 /  ! Betteron 1991
      DATA SUBNAME( 54), A( 54), E( 54) / 'BENZALDEHYDE    ', 3.9E+01, 4.8E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 55), A( 55), E( 55) / 'PINENE          ', 4.9E-02, 0.0E+00 /  ! Karl and Lindinger 1997
      DATA SUBNAME( 56), A( 56), E( 56) / 'ATRA            ', 4.1E+05, 6.0E+03 /  ! CIBA Corp (1989) and Scholtz (1999)
      DATA SUBNAME( 57), A( 57), E( 57) / 'DATRA           ', 4.1E+05, 6.0E+03 /  ! assumed same as Atrazine
      DATA SUBNAME( 58), A( 58), E( 58) / 'ADIPIC_ACID     ', 2.0E+08, 0.0E+00 /  ! Saxena and Hildemann (1996)
      DATA SUBNAME( 59), A( 59), E( 59) / 'ACROLEIN        ', 8.2E+00, 0.0E+00 /  ! Meylan and Howard (1991)
      DATA SUBNAME( 60), A( 60), E( 60) / '1,3-BUTADIENE   ', 1.4E-02, 0.0E+00 /  ! Mackay and Shiu (1981)
      DATA SUBNAME( 61), A( 61), E( 61) / 'ACRYLONITRILE   ', 7.3E+00, 0.0E+00 /  ! Meylan and Howard (1991)
      DATA SUBNAME( 62), A( 62), E( 62) / 'CARBONTETRACHLOR', 3.4E-02, 4.2E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 63), A( 63), E( 63) / 'PROPYLENE_DICHLO', 3.4E-01, 4.3E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 64), A( 64), E( 64) / '1,3DICHLORPROPEN', 6.5E-01, 4.2E+03 /  ! Wright et al (1992b)
      DATA SUBNAME( 65), A( 65), E( 65) / '1,1,2,2-CL4ETHAN', 2.4E+00, 3.2E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 66), A( 66), E( 66) / 'CHLOROFORM      ', 2.5E-01, 4.5E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 67), A( 67), E( 67) / '1,2DIBROMOETHANE', 1.5E+00, 3.9E+03 /  ! Ashworth et al (1988)
      DATA SUBNAME( 68), A( 68), E( 68) / '1,2DICHLOROETHAN', 7.3E-01, 4.2E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 69), A( 69), E( 69) / 'METHYLENE_CHLORI', 3.6E-01, 4.1E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 70), A( 70), E( 70) / 'PERCHLOROETHYLEN', 5.9E-02, 4.8E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 71), A( 71), E( 71) / 'TRICHLOROETHENE ', 1.0E-01, 4.6E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 72), A( 72), E( 72) / 'VINYL_CHLORIDE  ', 3.9E-02, 3.1E+03 /  ! Staudinger and Roberts (1996)
      DATA SUBNAME( 73), A( 73), E( 73) / 'ETHYLENE_OXIDE  ', 8.4E+00, 0.0E+00 /  ! CRC

      DATA LSO2,   B(  1), D(  1) /  1, 1.30E-02,  1.96E+03 /  ! SO2*H2O<=>HSO3+H     : Smith and Martell (1976)
      DATA LHSO3,  B(  2), D(  2) /  2, 6.60E-08,  1.50E+03 /  ! HSO3<=>SO3+H         : Smith and Martell (1976)
      DATA LHNO2,  B(  3), D(  3) /  3, 5.10E-04, -1.26E+03 /  ! HNO2(aq)<=>NO2+H     : Schwartz and White (1981)
      DATA LHNO3,  B(  4), D(  4) /  4, 1.54E+01,  8.70E+03 /  ! HNO3(aq)<=>NO3+H     : Schwartz (1984)
      DATA LCO2,   B(  5), D(  5) /  5, 4.30E-07, -1.00E+03 /  ! CO2*H2O<=>HCO3+H     : Smith and Martell (1976)
      DATA LHCO3,  B(  6), D(  6) /  6, 4.68E-11, -1.76E+03 /  ! HCO3<=>CO3+H         : Smith and Martell (1976)
      DATA LH2O2,  B(  7), D(  7) /  7, 2.20E-12, -3.73E+03 /  ! H2O2(aq)<=>HO2+H     : Smith and Martell (1976)
      DATA LHCHO,  B(  8), D(  8) /  8, 2.53E+03,  4.02E+03 /  ! HCHO(aq)<=>H2C(OH)2  : Le Hanaf (1968)
      DATA LHCOOH, B(  9), D(  9) /  9, 1.80E-04, -2.00E+01 /  ! HCOOH(aq)<=>HCOO+H   : Martell and Smith (1977)
      DATA LHO2,   B( 10), D( 10) / 10, 3.50E-05,  0.00E+00 /  ! HO2(aq)<=>H+O2       : Perrin (1982)
      DATA LNH4OH, B( 11), D( 11) / 11, 1.70E-05, -4.50E+02 /  ! NH4*OH<=>NH4+OH      : Smith and Martell (1976)
      DATA LH2O,   B( 12), D( 12) / 12, 1.00E-14, -6.71E+03 /  ! H2O<=>H+OH           : Smith and Martell (1976)

!...........EXTERNAL FUNCTIONS and their descriptions:

!     INTEGER       INDEX1
!     INTEGER       TRIMLEN             !  string length, excl. trailing blanks

!     EXTERNAL      TRIMLEN

!-----------------------------------------------------------------------
!  begin body of subroutine HLCONST

      SPC = INDEX1( NAME, MXSPCS, SUBNAME )

!...error if species not found in table

      IF ( SPC .LE. 0 ) THEN
!       XMSG = NAME( 1:TRIMLEN( NAME ) ) // ' not found in Henry''s '//
!    &         ' Law Constant table in routine HLCONST.'
!CC        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

!...compute the Henry's Law Constant

      TFAC = ( 298.0 - TEMP) / ( 298.0 * TEMP )
      KH = A( SPC ) * EXP( E( SPC ) * TFAC )
      HLCONST = KH

!...compute the effective Henry's law constants

      IF ( EFFECTIVE ) THEN

        IF ( HPLUS .LE. 0.0 ) THEN
!         XMSG = 'Negative or Zero [H+] concentration specified ' //
!    &           'in HLCONST '
!CC          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
        END IF

        HPLUSI = 1.0 / HPLUS
        HPLUS2I = HPLUSI * HPLUSI

        CHECK_NAME: SELECT CASE ( NAME( 1:TRIMLEN( NAME ) ) )

        CASE ('SO2')            !   SO2H2O <=> HSO3- + H+
                                ! & HSO3- <=> SO3= + H+

          AKEQ1 = B( LSO2 )  * EXP( D( LSO2 )  * TFAC )
          AKEQ2 = B( LHSO3 ) * EXP( D( LHSO3 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI + AKEQ1 * AKEQ2 * HPLUS2I )

        CASE ('HNO2')           ! HNO2(aq) <=> NO2- + H+

          AKEQ1 = B( LHNO2 ) * EXP( D( LHNO2 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('HNO3')           ! HNO3(aq) <=> NO3- + H+

          AKEQ1 = B( LHNO3 ) * EXP( D( LHNO3 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('CO2')            !   CO2H2O <=> HCO3- + H+
                                ! & HCO3- <=> CO3= + H+

          AKEQ1 = B( LCO2 )  * EXP( D( LCO2 )  * TFAC )
          AKEQ2 = B( LHCO3 ) * EXP( D( LHCO3 ) * TFAC )
          HLCONST = KH &
                  * ( 1.0 + AKEQ1 * HPLUSI + AKEQ1 * AKEQ2 * HPLUS2I )

        CASE ('H2O2')           ! H2O2(aq) <=> HO2- + H+

          AKEQ1 = B( LH2O2 ) * EXP( D( LH2O2 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('FORMALDEHYDE')   ! HCHO(aq) <=> H2C(OH)2(aq)

          AKEQ1 = B( LHCHO ) * EXP( D( LHCHO ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 )

        CASE ('FORMIC_ACID')    ! HCOOH(aq) <=> HCOO- + H+

          AKEQ1 = B( LHCOOH ) * EXP( D( LHCOOH ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('HO2')            ! HO2(aq) <=> H+ + O2-

          AKEQ1 = B( LHO2 ) * EXP( D( LHO2 ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ('NH3')            ! NH4OH <=> NH4+ + OH-

          AKEQ1 = B( LNH4OH ) * EXP( D( LNH4OH ) * TFAC )
          AKEQ2 = B( LH2O ) * EXP( D( LH2O ) * TFAC )
          OHION = AKEQ2 * HPLUSI
          HLCONST = KH * ( 1.0 + AKEQ1 / OHION )

        END SELECT CHECK_NAME

      END IF

!     RETURN
END FUNCTION HLCONST
!.........................................................................
! Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/index1.f,v 1.2 2000/11/28 21:22:49 smith_w Exp $"
! EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.........................................................................

      INTEGER FUNCTION INDEX1 (NAME, N, NLIST)

!***********************************************************************
!  subroutine body starts at line 46
!
!  FUNCTION:
!
!    Searches for NAME in list NLIST and returns the subscript
!    (1...N) at which it is found, or returns 0 when NAME not
!    found in NLIST
!
!  PRECONDITIONS REQUIRED:  none
!
!  SUBROUTINES AND FUNCTIONS CALLED:  none
!
!  REVISION HISTORY:
!
!    5/88   Modified for ROMNET
!    9/94   Modified for Models-3 by CJC
!
!***********************************************************************

      IMPLICIT NONE

!.......   Arguments and their descriptions:

      CHARACTER*(*) NAME        !  Character string being searched for
      INTEGER       N           !  Length of array to be searched
      CHARACTER*(*) NLIST(*)    !  array to be searched

!.......   Local variable:

      INTEGER       I   !  loop counter

!.....................................................................
!.......   begin body of INDEX1()

      DO 100 I = 1, N

          IF ( NAME .EQ. NLIST( I ) ) THEN    ! Found NAME in NLIST
              INDEX1 = I
              RETURN
          ENDIF

100   CONTINUE

      INDEX1 = 0        !  not found
      RETURN

END FUNCTION INDEX1

END MODULE module_ctrans_aqchem
