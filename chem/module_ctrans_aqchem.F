MODULE module_ctrans_aqchem

CONTAINS

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
! $Header: /project/work/rep/CCTM/src/cloud/cloud_acm/aqchem.F,v 1.32 2008/09/10 19:40:39 sjr Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:

      SUBROUTINE AQCHEM ( TEMP, PRES_PA, TAUCLD, PRCRATE, &
                          WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS, &
                          AEROSOL, LIQUID, GASWDEP, AERWDEP, HPWDEP )

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
!     12  12/02    sjr  changed calls to HLCONST and updated the dissociation
!                       constants
!     13  06/26/03 sjr  revised calculations of DTW based on CMAS website
!                       discussions
!     14  08/05/03 sjr  revision made to the coarse aerosol number washout
!     15  04/20/05  us  revisions to add sea salt species in the fine and
!                       coarse aerosol modes, and HCl dissolution/dissociation
!     16  10/13/05 sjr  fixed bug in the integration time step calculation
!                       (reported by Bonyoung Koo)
!     17  03/01/06 sjr  added elemental carbon aerosol; organic aerosols
!                       replaced with primary, secondary biogenic, and
!                       secondary anthropogenic; fixed 3rd moment calc to
!                       include EC and primary organics (not secondary);
!                       re-arranged logic for setting Cl & Na ending conc;
!                       added pointers/indirect addressing for arrays WETDEP
!                       and LIQUID
!     16  03/30/07 sjr  Limit integration timestep by cloud washout time
!     17  04/10/07 sjr  increased loop limits as follows: I20C <10000,
!                       I7777C <10000, I30C <10000, ICNTAQ <60000
!                       
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
!  GAS(ngas)     real  input&output  Concentration for species i=1,12
!  GASWDEP(ngas) real     output     wet deposition for species
!                                    (01)= SO2   conc (mol/mol)
!                                    (02)= HNO3  conc (mol/mol)
!                                    (03)= N2O5  conc (mol/mol)
!                                    (04)= CO2   conc (mol/mol)
!                                    (05)= NH3   conc (mol/mol)
!                                    (06)= H2O2  conc (mol/mol)
!                                    (07)= O3    conc (mol/mol)
!                                    (08)= FOA   conc (mol/mol)
!                                    (09)= MHP   conc (mol/mol)
!                                    (10)= PAA   conc (mol/mol)
!                                    (11)= H2SO4 conc (mol/mol)
!                                    (12)= HCL   conc (mol/mol)
!
!  AEROSOL(naer) real input&output   Concentration for species i=1,36
!  AERWDEP(naer) real     output     wet deposition for species
!                                    (01)= SO4AKN  conc (mol/mol)
!                                    (02)= SO4ACC  conc (mol/mol)
!                                    (03)= SO4COR  conc (mol/mol)
!                                    (04)= NH4AKN  conc (mol/mol)
!                                    (05)= NH4ACC  conc (mol/mol)
!                                    (06)= NO3AKN  conc (mol/mol)
!                                    (07)= NO3ACC  conc (mol/mol)
!                                    (08)= NO3COR  conc (mol/mol)
!                                    (09)= ORGAAKN conc (mol/mol)
!                                    (10)= ORGAACC conc (mol/mol)
!                                    (11)= ORGPAKN conc (mol/mol)
!                                    (12)= ORGPACC conc (mol/mol)
!                                    (13)= ORGBAKN conc (mol/mol)
!                                    (14)= ORGBACC conc (mol/mol)
!                                    (15)= ECAKN   conc (mol/mol)
!                                    (16)= ECACC   conc (mol/mol)
!                                    (17)= PRIAKN  conc (mol/mol)
!                                    (18)= PRIACC  conc (mol/mol)
!                                    (19)= PRICOR  conc (mol/mol)
!                                    (20)= NAAKN   conc (mol/mol)
!                                    (21)= NAACC   conc (mol/mol)
!                                    (22)= NACOR   conc (mol/mol)
!                                    (23)= CLAKN   conc (mol/mol)
!                                    (24)= CLACC   conc (mol/mol)
!                                    (25)= CLCOR   conc (mol/mol)
!                                    (26)= NUMAKN  conc ( #/mol )
!                                    (27)= NUMACC  conc ( #/mol )
!                                    (28)= NUMCOR  conc ( #/mol )
!                                    (29)= SRFAKN  conc (m2/mol )
!                                    (30)= SRFACC  conc (m2/mol )
!                                    (31)= NACL    conc (mol/mol)
!                                    (32)= CACO3   conc (mol/mol)
!                                    (33)= MGCO3   conc (mol/mol)
!                                    (34)= A3FE    conc (mol/mol)
!                                    (35)= B2MN    conc (mol/mol)
!                                    (36)= K       conc (mol/mol)

!-----------------------------------------------------------------------

      IMPLICIT NONE

!      INCLUDE SUBST_IOPARMS        ! I/O parameters definitions
!      INCLUDE SUBST_RXCMMN         ! Mechanism reaction common block

!.......................................................................
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
!    4/30/08 - Changed REARTH to match default value in MM5 and WRF - TLO
! FSB References:
!      CRC76,        "CRC Handbook of Chemistry and Physics (76th Ed)",
!                     CRC Press, 1995 
!      Hobbs, P.V.   "Basic Physical Chemistry for the Atmospheric Sciences",
!                     Cambridge Univ. Press, 206 pp, 1995.  
!      Snyder, J.P., "Map Projections-A Working Manual, U.S. Geological Survey
!                     Paper 1395 U.S.GPO, Washington, DC, 1987.
!      Stull, R. B., "An Introduction to Bounday Layer Meteorology", Kluwer, 
!                     Dordrecht, 1988
!.......................................................................


! Geometric Constants:

      REAL        PI ! pi (single precision 3.141593)
      PARAMETER ( PI = 3.14159265358979324 )
 
      REAL        PI180 ! pi/180 [ rad/deg ]
      PARAMETER ( PI180  = PI / 180.0 )

! Geodetic Constants:
 
      REAL        REARTH ! radius of the earth [ m ]
                         ! FSB: radius of sphere having same surface area as
                         ! Clarke ellipsoid of 1866 ( Source: Snyder, 1987)
!     PARAMETER ( REARTH = 6370997.0 )
      PARAMETER ( REARTH = 6370000.0 )  ! default Re in MM5 and WRF
 
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

!...Aqueous species pointers INCLUDE File

!...........PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: NGAS = 12  ! number of gas-phase species for AQCHEM
      INTEGER, PARAMETER :: NAER = 36  ! number of aerosol species for AQCHEM
      INTEGER, PARAMETER :: NLIQS = 41 ! number of liquid-phase species in AQCHEM

!...pointers for the AQCHEM array GAS

      INTEGER, PARAMETER :: LSO2    =  1  ! Sulfur Dioxide
      INTEGER, PARAMETER :: LHNO3   =  2  ! Nitric Acid
      INTEGER, PARAMETER :: LN2O5   =  3  ! Dinitrogen Pentoxide
      INTEGER, PARAMETER :: LCO2    =  4  ! Carbon Dioxide
      INTEGER, PARAMETER :: LNH3    =  5  ! Ammonia
      INTEGER, PARAMETER :: LH2O2   =  6  ! Hydrogen Perioxide
      INTEGER, PARAMETER :: LO3     =  7  ! Ozone
      INTEGER, PARAMETER :: LFOA    =  8  ! Formic Acid
      INTEGER, PARAMETER :: LMHP    =  9  ! Methyl Hydrogen Peroxide
      INTEGER, PARAMETER :: LPAA    = 10  ! Peroxyacidic Acid
      INTEGER, PARAMETER :: LH2SO4  = 11  ! Sulfuric Acid
      INTEGER, PARAMETER :: LHCL    = 12  ! Hydrogen Chloride

!...pointers for the AQCHEM array AEROSOL

      INTEGER, PARAMETER :: LSO4AKN  =  1  ! Aitken-mode Sulfate
      INTEGER, PARAMETER :: LSO4ACC  =  2  ! Accumulation-mode Sulfate
      INTEGER, PARAMETER :: LSO4COR  =  3  ! Coarse-mode Sulfate
      INTEGER, PARAMETER :: LNH4AKN  =  4  ! Aitken-mode Ammonium
      INTEGER, PARAMETER :: LNH4ACC  =  5  ! Accumulation-mode Ammonium
      INTEGER, PARAMETER :: LNO3AKN  =  6  ! Aitken-mode Nitrate
      INTEGER, PARAMETER :: LNO3ACC  =  7  ! Accumulation-mode Nitrate
      INTEGER, PARAMETER :: LNO3COR  =  8  ! Coarse-mode Nitrate
      INTEGER, PARAMETER :: LORGAAKN =  9  ! Aitken-mode anthropogenic SOA
      INTEGER, PARAMETER :: LORGAACC = 10  ! Accumulation-mode anthropogenic SOA
      INTEGER, PARAMETER :: LORGPAKN = 11  ! Aitken-mode primary organic aerosol
      INTEGER, PARAMETER :: LORGPACC = 12  ! Accumulation-mode primary organic aerosol
      INTEGER, PARAMETER :: LORGBAKN = 13  ! Aitken-mode biogenic SOA
      INTEGER, PARAMETER :: LORGBACC = 14  ! Accumulation-mode biogenic SOA
      INTEGER, PARAMETER :: LECAKN   = 15  ! Aitken-mode elemental carbon
      INTEGER, PARAMETER :: LECACC   = 16  ! Accumulation-mode elemental carbon
      INTEGER, PARAMETER :: LPRIAKN  = 17  ! Aitken-mode primary aerosol
      INTEGER, PARAMETER :: LPRIACC  = 18  ! Accumulation-mode primary aerosol
      INTEGER, PARAMETER :: LPRICOR  = 19  ! Coarse-mode primary aerosol
      INTEGER, PARAMETER :: LNAAKN   = 20  ! Aitken-mode Sodium
      INTEGER, PARAMETER :: LNAACC   = 21  ! Accumulation-mode Sodium
      INTEGER, PARAMETER :: LNACOR   = 22  ! Coarse-mode Sodium
      INTEGER, PARAMETER :: LCLAKN   = 23  ! Aitken-mode Chloride ion
      INTEGER, PARAMETER :: LCLACC   = 24  ! Accumulation-mode Chloride ion
      INTEGER, PARAMETER :: LCLCOR   = 25  ! Coarse-mode Chloride ion
      INTEGER, PARAMETER :: LNUMAKN  = 26  ! Aitken-mode number
      INTEGER, PARAMETER :: LNUMACC  = 27  ! Accumulation-mode number
      INTEGER, PARAMETER :: LNUMCOR  = 28  ! Coarse-mode number
      INTEGER, PARAMETER :: LSRFAKN  = 29  ! Aitken-mode surface area
      INTEGER, PARAMETER :: LSRFACC  = 30  ! Accumulation-mode surface area
      INTEGER, PARAMETER :: LNACL    = 31  ! Sodium Chloride aerosol for AE3 only {depreciated in AE4}
      INTEGER, PARAMETER :: LCACO3   = 32  ! Calcium Carbonate aerosol (place holder)
      INTEGER, PARAMETER :: LMGCO3   = 33  ! Magnesium Carbonate aerosol (place holder)
      INTEGER, PARAMETER :: LA3FE    = 34  ! Iron aerosol (place holder)
      INTEGER, PARAMETER :: LB2MN    = 35  ! Manganese aerosol (place holder)
      INTEGER, PARAMETER :: LK       = 36  ! Potassium aerosol (Cl- tracked separately) (place holder)

!...pointers for the AQCHEM arrays LIQUID and WETDEP

      INTEGER, PARAMETER :: LACL        =  1  ! Hydrogen ion
      INTEGER, PARAMETER :: LNH4L       =  2  ! Ammonium
      INTEGER, PARAMETER :: LCAL        =  3  ! Calcium
      INTEGER, PARAMETER :: LNAACCL     =  4  ! Sodium
      INTEGER, PARAMETER :: LOHL        =  5  ! Hydroxyl radical ion
      INTEGER, PARAMETER :: LSO4ACCL    =  6  ! Sulfate (attributed to accumulation mode)
      INTEGER, PARAMETER :: LHSO4ACCL   =  7  ! bisulfate (attributed to accumulation mode)
      INTEGER, PARAMETER :: LSO3L       =  8  ! sulfite
      INTEGER, PARAMETER :: LHSO3L      =  9  ! bisulfite
      INTEGER, PARAMETER :: LSO2L       = 10  ! sulfur dioxide
      INTEGER, PARAMETER :: LCO3L       = 11  ! carbonate
      INTEGER, PARAMETER :: LHCO3L      = 12  ! bicarbonate
      INTEGER, PARAMETER :: LCO2L       = 13  ! carbon dioxide
      INTEGER, PARAMETER :: LNO3ACCL    = 14  ! nitrate(attributed to accumulation mode)
      INTEGER, PARAMETER :: LNH3L       = 15  ! ammonia
      INTEGER, PARAMETER :: LCLACCL     = 16  ! chloride ion (attributed to accumulation mode)
      INTEGER, PARAMETER :: LH2O2L      = 17  ! hydrogen peroxide
      INTEGER, PARAMETER :: LO3L        = 18  ! ozone
      INTEGER, PARAMETER :: LFEL        = 19  ! iron
      INTEGER, PARAMETER :: LMNL        = 20  ! Manganese
      INTEGER, PARAMETER :: LAL         = 21  ! generalized anion associated with iron
      INTEGER, PARAMETER :: LFOAL       = 22  ! Formic acid
      INTEGER, PARAMETER :: LHCO2L      = 23  ! HCOO- ion
      INTEGER, PARAMETER :: LMHPL       = 24  ! Methyl hydrogen peroxide
      INTEGER, PARAMETER :: LPAAL       = 25  ! Peroxyacidic acid
      INTEGER, PARAMETER :: LHCLL       = 26  ! Hydrogen chloride
      INTEGER, PARAMETER :: LPRIML      = 27  ! primary aerosol
      INTEGER, PARAMETER :: LMGL        = 28  ! Magnesium
      INTEGER, PARAMETER :: LKL         = 29  ! potassium
      INTEGER, PARAMETER :: LBL         = 30  ! generalized anion associated with manganese
      INTEGER, PARAMETER :: LHNO3L      = 31  ! nitric acid
      INTEGER, PARAMETER :: LPRIMCORL   = 32  ! coarse-mode primary aerosol
      INTEGER, PARAMETER :: LNUMCORL    = 33  ! coarse-mode number
      INTEGER, PARAMETER :: LTS6CORL    = 34  ! sulfate (attributed to coarse mode)
      INTEGER, PARAMETER :: LNACORL     = 35  ! sodium (attributed to coarse mode)
      INTEGER, PARAMETER :: LCLCORL     = 36  ! chloride ion (attributed to coarse mode)
      INTEGER, PARAMETER :: LNO3CORL    = 37  ! nitrate (attributed to coarse mode)
      INTEGER, PARAMETER :: LORGAL      = 38  ! anthropogenic SOA
      INTEGER, PARAMETER :: LORGPL      = 39  ! primary organic aerosols
      INTEGER, PARAMETER :: LORGBL      = 40  ! biogenic SOA
      INTEGER, PARAMETER :: LECL        = 41  ! elemental carbon

!...surrogate names, their background values, and units
!...  for AQCHEM's GAS species

      CHARACTER*16, SAVE :: SGRGAS  ( NGAS ) ! surrogate name for gases
      CHARACTER*16, SAVE :: BUNTSGAS( NGAS ) ! units of bkgnd values

      REAL, SAVE :: BGNDGAS( NGAS ) ! background values for each gas

      DATA SGRGAS( LSO2   ), BGNDGAS( LSO2   ) /'SO2             ',   0.0 /
      DATA SGRGAS( LHNO3  ), BGNDGAS( LHNO3  ) /'HNO3            ',   0.0 /
      DATA SGRGAS( LN2O5  ), BGNDGAS( LN2O5  ) /'N2O5            ',   0.0 /
      DATA SGRGAS( LCO2   ), BGNDGAS( LCO2   ) /'CO2             ', 340.0 /
      DATA SGRGAS( LNH3   ), BGNDGAS( LNH3   ) /'NH3             ',   0.0 /
      DATA SGRGAS( LH2O2  ), BGNDGAS( LH2O2  ) /'H2O2            ',   0.0 /
      DATA SGRGAS( LO3    ), BGNDGAS( LO3    ) /'O3              ',   0.0 /
      DATA SGRGAS( LFOA   ), BGNDGAS( LFOA   ) /'FOA             ',   0.0 /
      DATA SGRGAS( LMHP   ), BGNDGAS( LMHP   ) /'MHP             ',   0.0 /
      DATA SGRGAS( LPAA   ), BGNDGAS( LPAA   ) /'PAA             ',   0.0 /
      DATA SGRGAS( LH2SO4 ), BGNDGAS( LH2SO4 ) /'H2SO4           ',   0.0 /
      DATA SGRGAS( LHCL   ), BGNDGAS( LHCL   ) /'HCL             ',   0.0 /

      DATA BUNTSGAS( LSO2   ) / 'ppm' /
      DATA BUNTSGAS( LHNO3  ) / 'ppm' /
      DATA BUNTSGAS( LN2O5  ) / 'ppm' /
      DATA BUNTSGAS( LCO2   ) / 'ppm' /
      DATA BUNTSGAS( LNH3   ) / 'ppm' /
      DATA BUNTSGAS( LH2O2  ) / 'ppm' /
      DATA BUNTSGAS( LO3    ) / 'ppm' /
      DATA BUNTSGAS( LFOA   ) / 'ppm' /
      DATA BUNTSGAS( LMHP   ) / 'ppm' /
      DATA BUNTSGAS( LPAA   ) / 'ppm' /
      DATA BUNTSGAS( LH2SO4 ) / 'ppm' /
      DATA BUNTSGAS( LHCL   ) / 'ppm' /

!...surrogate names, their background values, units, and molecular weights
!...  for AQCHEM's AEROSOL species

      CHARACTER*16, SAVE :: SGRAER  ( NAER ) ! surrogate name for aerosols
      CHARACTER*16, SAVE :: BUNTSAER( NAER ) ! units of bkgnd values

      REAL, SAVE :: SGRAERMW( NAER ) ! molecular weight for aerosol species
      REAL, SAVE :: BGNDAER ( NAER ) ! bkground vals each aerosols

      DATA SGRAER( LSO4AKN  ), SGRAERMW( LSO4AKN  ) / 'SO4_AITKEN      ' ,  96.0 /
      DATA SGRAER( LSO4ACC  ), SGRAERMW( LSO4ACC  ) / 'SO4_ACCUM       ' ,  96.0 /
      DATA SGRAER( LSO4COR  ), SGRAERMW( LSO4COR  ) / 'SO4_COARSE      ' ,  96.0 /
      DATA SGRAER( LNH4AKN  ), SGRAERMW( LNH4AKN  ) / 'NH4_AITKEN      ' ,  18.0 /
      DATA SGRAER( LNH4ACC  ), SGRAERMW( LNH4ACC  ) / 'NH4_ACCUM       ' ,  18.0 /
      DATA SGRAER( LNO3AKN  ), SGRAERMW( LNO3AKN  ) / 'NO3_AITKEN      ' ,  62.0 /
      DATA SGRAER( LNO3ACC  ), SGRAERMW( LNO3ACC  ) / 'NO3_ACCUM       ' ,  62.0 /
      DATA SGRAER( LNO3COR  ), SGRAERMW( LNO3COR  ) / 'NO3_COARSE      ' ,  62.0 /
      DATA SGRAER( LORGAAKN ), SGRAERMW( LORGAAKN ) / 'ORGA_AITKEN     ' , 150.0 /
      DATA SGRAER( LORGAACC ), SGRAERMW( LORGAACC ) / 'ORGA_ACCUM      ' , 150.0 /
      DATA SGRAER( LORGPAKN ), SGRAERMW( LORGPAKN ) / 'ORGP_AITKEN     ' , 220.0 /
      DATA SGRAER( LORGPACC ), SGRAERMW( LORGPACC ) / 'ORGP_ACCUM      ' , 220.0 /
      DATA SGRAER( LORGBAKN ), SGRAERMW( LORGBAKN ) / 'ORGB_AITKEN     ' , 177.0 /
      DATA SGRAER( LORGBACC ), SGRAERMW( LORGBACC ) / 'ORGB_ACCUM      ' , 177.0 /
      DATA SGRAER( LECAKN   ), SGRAERMW( LECAKN   ) / 'EC_AITKEN       ' ,  12.0 /
      DATA SGRAER( LECACC   ), SGRAERMW( LECACC   ) / 'EC_ACCUM        ' ,  12.0 /
      DATA SGRAER( LPRIAKN  ), SGRAERMW( LPRIAKN  ) / 'PRI_AITKEN      ' , 200.0 /
      DATA SGRAER( LPRIACC  ), SGRAERMW( LPRIACC  ) / 'PRI_ACCUM       ' , 200.0 /
      DATA SGRAER( LPRICOR  ), SGRAERMW( LPRICOR  ) / 'PRI_COARSE      ' , 100.0 /
      DATA SGRAER( LNAAKN   ), SGRAERMW( LNAAKN   ) / 'NA_AITKEN       ' ,  23.0 /
      DATA SGRAER( LNAACC   ), SGRAERMW( LNAACC   ) / 'NA_ACCUM        ' ,  23.0 /
      DATA SGRAER( LNACOR   ), SGRAERMW( LNACOR   ) / 'NA_COARSE       ' ,  23.0 /
      DATA SGRAER( LCLAKN   ), SGRAERMW( LCLAKN   ) / 'CL_AITKEN       ' ,  35.5 /
      DATA SGRAER( LCLACC   ), SGRAERMW( LCLACC   ) / 'CL_ACCUM        ' ,  35.5 /
      DATA SGRAER( LCLCOR   ), SGRAERMW( LCLCOR   ) / 'CL_COARSE       ' ,  35.5 /
      DATA SGRAER( LNUMAKN  ), SGRAERMW( LNUMAKN  ) / 'NUM_AITKEN      ' ,   1.0 /
      DATA SGRAER( LNUMACC  ), SGRAERMW( LNUMACC  ) / 'NUM_ACCUM       ' ,   1.0 /
      DATA SGRAER( LNUMCOR  ), SGRAERMW( LNUMCOR  ) / 'NUM_COARSE      ' ,   1.0 /
      DATA SGRAER( LSRFAKN  ), SGRAERMW( LSRFAKN  ) / 'SRF_AITKEN      ' ,   1.0 /
      DATA SGRAER( LSRFACC  ), SGRAERMW( LSRFACC  ) / 'SRF_ACCUM       ' ,   1.0 /
      DATA SGRAER( LNACL    ), SGRAERMW( LNACL    ) / 'NACL            ' ,  58.4 /  ! AE3 NaCl aerosol {depreciated in AE4}
      DATA SGRAER( LCACO3   ), SGRAERMW( LCACO3   ) / 'CACO3           ' , 100.1 /
      DATA SGRAER( LMGCO3   ), SGRAERMW( LMGCO3   ) / 'MGCO3           ' ,  84.3 /
      DATA SGRAER( LA3FE    ), SGRAERMW( LA3FE    ) / 'A3FE            ' ,  55.8 /
      DATA SGRAER( LB2MN    ), SGRAERMW( LB2MN    ) / 'B2MN            ' ,  54.9 /
      DATA SGRAER( LK       ), SGRAERMW( LK       ) / 'K               ' ,  39.1 /

      DATA BGNDAER( LSO4AKN  ), BUNTSAER( LSO4AKN  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LSO4ACC  ), BUNTSAER( LSO4ACC  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LSO4COR  ), BUNTSAER( LSO4COR  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNH4AKN  ), BUNTSAER( LNH4AKN  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNH4ACC  ), BUNTSAER( LNH4ACC  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNO3AKN  ), BUNTSAER( LNO3AKN  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNO3ACC  ), BUNTSAER( LNO3ACC  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNO3COR  ), BUNTSAER( LNO3COR  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LORGAAKN ), BUNTSAER( LORGAAKN ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LORGAACC ), BUNTSAER( LORGAACC ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LORGPAKN ), BUNTSAER( LORGPAKN ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LORGPACC ), BUNTSAER( LORGPACC ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LORGBAKN ), BUNTSAER( LORGBAKN ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LORGBACC ), BUNTSAER( LORGBACC ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LECAKN   ), BUNTSAER( LECAKN   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LECACC   ), BUNTSAER( LECACC   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LPRIAKN  ), BUNTSAER( LPRIAKN  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LPRIACC  ), BUNTSAER( LPRIACC  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LPRICOR  ), BUNTSAER( LPRICOR  ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNAAKN   ), BUNTSAER( LNAAKN   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNAACC   ), BUNTSAER( LNAACC   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNACOR   ), BUNTSAER( LNACOR   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LCLAKN   ), BUNTSAER( LCLAKN   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LCLACC   ), BUNTSAER( LCLACC   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LCLCOR   ), BUNTSAER( LCLCOR   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LNUMAKN  ), BUNTSAER( LNUMAKN  ) /  0.0,   ' #/m3' /
      DATA BGNDAER( LNUMACC  ), BUNTSAER( LNUMACC  ) /  0.0,   ' #/m3' /
      DATA BGNDAER( LNUMCOR  ), BUNTSAER( LNUMCOR  ) /  0.0,   ' #/m3' /
      DATA BGNDAER( LSRFAKN  ), BUNTSAER( LSRFAKN  ) /  0.0,   'm2/m3' /
      DATA BGNDAER( LSRFACC  ), BUNTSAER( LSRFACC  ) /  0.0,   'm2/m3' /
      DATA BGNDAER( LNACL    ), BUNTSAER( LNACL    ) /  0.0,   'ug/m3' /  ! AE3 NaCl aerosol {depreciated in AE4}
      DATA BGNDAER( LCACO3   ), BUNTSAER( LCACO3   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LMGCO3   ), BUNTSAER( LMGCO3   ) /  0.0,   'ug/m3' /
      DATA BGNDAER( LA3FE    ), BUNTSAER( LA3FE    ) /  0.010, 'ug/m3' /
      DATA BGNDAER( LB2MN    ), BUNTSAER( LB2MN    ) /  0.005, 'ug/m3' /
      DATA BGNDAER( LK       ), BUNTSAER( LK       ) /  0.0,   'ug/m3' /

      CHARACTER*120 XMSG           ! Exit status message
      DATA          XMSG / ' ' /

!...........PARAMETERS and their descriptions:

      INTEGER      NUMOX           ! number of oxidizing reactions
      PARAMETER  ( NUMOX =  5 )

      REAL         H2ODENS         ! density of water at 20 C and 1 ATM
      PARAMETER  ( H2ODENS = 1000.0 )  ! (kg/m3)

      REAL         ONETHIRD       ! 1/3
      PARAMETER  ( ONETHIRD = 1.0 / 3.0 )

      REAL         TWOTHIRDS       ! 2/3
      PARAMETER  ( TWOTHIRDS = 2.0 / 3.0 )

      REAL         CONCMIN         ! minimum concentration
      PARAMETER  ( CONCMIN = 1.0E-30 )

      REAL         SEC2HR          ! convert seconds to hours
      PARAMETER  ( SEC2HR = 1.0 / 3600.0 )

!...........ARGUMENTS and their descriptions

!      INTEGER      JDATE           ! current model date, coded YYYYDDD
!      INTEGER      JTIME           ! current model time, coded HHMMSS

      REAL         AIRM                       ! total air mass in cloudy layers (mol/m2)
      REAL         ALFA0                      ! scav coef for aitken aerosol number
      REAL         ALFA2                      ! scav coef for aitken aerosol sfc area
      REAL         ALFA3                      ! scav coef for aitken aerosol mass
      REAL         HPWDEP                     ! hydrogen wet deposition (mm mol/liter)
      REAL         PRCRATE                    ! precip rate (mm/hr)
      REAL         PRES_PA                    ! pressure (Pa)
      REAL         TAUCLD                     ! timestep for cloud (s)
      REAL         TEMP                       ! temperature (K)
      REAL         WCAVG                      ! liquid water content (kg/m3)
      REAL         WTAVG                      ! total water content (kg/m3)
      
      REAL,  INTENT(INOUT) :: GAS ( NGAS )    ! gas phase concentrations (mol/molV)
      REAL,  INTENT(INOUT) :: AEROSOL( NAER ) ! aerosol concentrations (mol/molV)
      REAL,  INTENT(OUT)   :: LIQUID( NLIQS ) ! liquid concentrations (moles/liter)
      REAL,  INTENT(OUT)   :: GASWDEP( NGAS ) ! gas phase wet deposition array (mm mol/liter)
      REAL,  INTENT(OUT)   :: AERWDEP( NAER ) ! aerosol wet deposition array (mm mol/liter)

!...........LOCAL VARIABLES (scalars) and their descriptions:

      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru

      CHARACTER*6  PNAME           ! driver program name
      DATA         PNAME / 'AQCHEM' /
      SAVE         PNAME
      CHARACTER( 16 ), SAVE :: AE_VRSN ! Aerosol version name

      INTEGER      I20C            ! loop counter for do loop 20
      INTEGER      I30C            ! loop counter for do loop 30
      INTEGER      ITERAT          ! # iterations of aqueous chemistry solver
      INTEGER      I7777C          ! aqueous chem iteration counter
      INTEGER      ICNTAQ          ! aqueous chem iteration counter
      INTEGER      LIQ             ! loop counter for liquid species
      INTEGER      IOX             ! index over oxidation reactions

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
      REAL         CL              ! total Cl-  conc in cloudwater (mol/liter)
      REAL         CLACC           ! fine Cl- in cloudwater (mol/liter)
      REAL         CLACCA          ! initial fine Cl in cloudwater (mol/liter)
      REAL         CLAKNA          ! initial interstitial aero Cl (mol/liter)
      REAL         CLCOR           ! coarse Cl-  conc in cloudwater (mol/liter)
      REAL         CLCORA          ! init coarse Cl-  in cloudwater (mol/liter)
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
      REAL         EC              ! elemental carbon acc+akn aerosol in cloudwater (mol/liter)
      REAL         ECACCA          ! init EC ACC aerosol in cloudwater (mol/liter)
      REAL         ECAKNA          ! init EC AKN aerosol in cloudwater (mol/liter)
      REAL         FA              ! functional value ??
      REAL         FB              ! functional value ??
      REAL         FE              ! Fe+++ conc in cloudwater (mol/liter)
      REAL         FEA             ! initial Fe in cloudwater (mol/liter)
      REAL         FNH3            ! frac weight of NH3 to total ammonia
      REAL         FNH4ACC         ! frac weight of NH4 acc to total ammonia
      REAL         FHNO3           ! frac weight of HNO3 to total NO3
      REAL         FNO3ACC         ! frac weight of NO3 acc to total NO3
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
      REAL         HCLH            ! Henry's Law Constant for HCL
      REAL         HCL1            ! First dissociation constant for HCL
      REAL         HCL1H           ! HCL1*HCLH
      REAL         HCLL            ! HCl  conc in  cloudwater (mol/liter)
      REAL         HCO2            ! HCO2 conc in cloudwater (mol/liter)
      REAL         HCO3            ! HCO3 conc in cloudwater (mol/liter)
      REAL         HNO3H           ! Henry's Law Constant for HNO3
      REAL         HNO31           ! First dissociation constant for HNO3
      REAL         HNO31H          !
      REAL         HNO3L           ! HNO3 conc in cloudwater (mol/liter)
      REAL         HSO3            ! HSO3 conc in cloudwater (mol/liter)
      REAL         HSO4            ! HSO4 concn in cloudwater (mol/liter)
      REAL         HSO4ACC         ! accumulation mode HSO4 concn in cloudwater (mol/liter)
      REAL         HSO4COR         ! coarse HSO4 concn in cloudwater (mol/liter)
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
      REAL         NAACC           ! Na in cloudwater (mol/liter)
      REAL         NAACCA          ! initial Na in cloudwater (mol/liter)
      REAL         NAAKNA          ! init Aitken mode aer conc (mol/liter)
      REAL         NACOR           ! coarse Na in cloudwater (mol/liter)
      REAL         NACORA          ! init Coarse Na in cloudwater (mol/liter)
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
      REAL         NO3ACC          ! NO3 acc conc in cloudwater (mol/liter)
      REAL         NO3ACCA         ! init NO3 acc conc in cloudwater (mol/liter)
      REAL         NO3AKNA         ! init NO3 akn conc in cloudwater (mol/liter)
      REAL         NO3CORA         ! init NO3 coa conc in cloudwater (mol/liter)
      REAL         NO3COR          ! NO3 coarse conc in cloudwater (mol/liter)
      REAL         NUMCOR          ! coarse aerosol number in cloudwater (mol/liter)
      REAL         NUMCORA         ! initial coarse aerosol number in cloudwater (mol/liter)
      REAL         O3H             ! Henry's Law Constant for O3
      REAL         O3L             ! O3 conc in cloudwater (mol/liter)
      REAL         OH              ! OH conc in cloudwater (mol/liter)
      REAL         ORGA            ! anthro SOA in cloudwater (mol/liter)
      REAL         ORGAACCA        ! init anthro ACC SOA in cloudwater (mol/liter)
      REAL         ORGAAKNA        ! init anthro AKN SOA in cloudwater (mol/liter)
      REAL         ORGP            ! primary ORGANIC aerosol in cloudwater (mol/liter)
      REAL         ORGPACCA        ! init primary ORG ACC aerosol in cloudwater (mol/liter)
      REAL         ORGPAKNA        ! init primary ORG AKN aerosol in cloudwater (mol/liter)
      REAL         ORGB            ! biogenic SOA in cloudwater (mol/liter)
      REAL         ORGBACCA        ! init biogenic ACC SOA in cloudwater (mol/liter)
      REAL         ORGBAKNA        ! init biogenic AKN SOA in cloudwater (mol/liter)
      REAL         PAAH            ! Henry's Law Constant for PAA
      REAL         PAAL            ! PAA conc in cloudwater (mol/liter)
      REAL         PCO20           ! total CO2 partial pressure (atm)
      REAL         PCO2F           ! gas only CO2 partial pressure (atm)
      REAL         PFOA0           ! total ORGANIC acid partial pressure (atm)
      REAL         PFOAF           ! gas only ORGANIC ACID partial press (atm)
      REAL         PH2O20          ! total H2O2 partial pressure (atm)
      REAL         PH2O2F          ! gas only H2O2 partial pressure (atm)
      REAL         PHCL0           ! total HCL partial pressure (atm)
      REAL         PHCLF           ! gas only HCL partial pressure (atm)
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
      REAL         SO4ACC          ! accumulation mode SO4= conc in cloudwater (mol/liter)
      REAL         SO4COR          ! coarse SO4= conc in cloudwater (mol/liter)
      REAL         STION           ! ionic strength
      REAL         TAC             !
      REAL         TEMP1           !
      REAL         TIMEW           ! cloud chemistry clock (sec)
      REAL         TOTOX           !
      REAL         TOTAMM          ! total ammonium
      REAL         TOTNIT          ! total nitrate (excluding coarse mode)
      REAL         TS6             ! SO4 conc in cloudwater (mol/liter)
      REAL         TS6AKNA         ! init SO4 akn conc in cloudwater (mol/liter)
      REAL         TS6ACC          ! SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6ACCA         ! init SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6COR          ! coarse SO4 conc in cloudwater   (mol/liter)
      REAL         TS6CORA         ! init SO4 coa conc in cloudwater (mol/liter)
      REAL         TSIV            !
      REAL         TST             !
      REAL         TWASH           ! washout time for clouds (sec)
      REAL         WETFAC          ! converts mol/l to mm-mol/l based on precip
      REAL         XC1             ! (/mm)
      REAL         XC2             ! (liter-atm/mol/mm)
      REAL         XL              ! conversion factor (liter-atm/mol)
      REAL         ONE_OVER_XL     ! 1.0 / XL
      REAL         PRES_ATM_OVER_XL ! PRES_ATM / XL
      REAL         XLCO2           !
      REAL         XLH2O2          !
      REAL         XLHCL           ! const in calc of HCL final partial pres
      REAL         XLHNO3          !
      REAL         XLMHP           !
      REAL         XLNH3           !
      REAL         XLO3            !
      REAL         XLPAA           !
      REAL         XLSO2           !

!...........LOCAL VARIABLES (arrays) and their descriptions:

      REAL         WETDEP( NLIQS ) ! wet deposition array (mm mol/liter)
      REAL         DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL         DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      REAL         DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      REAL         ONE_OVER_TEMP     ! 1.0 / TEMP

!...........EXTERNAL FUNCTIONS and their descriptions:

!      REAL          HLCONST
!      EXTERNAL      HLCONST

!*********************************************************************
!     begin body of subroutine AQCHEM

      ONE_OVER_TEMP = 1.0 / TEMP

!...check for bad temperature, cloud air mass, or pressure

      IF ( TEMP .LE. 0.0 .OR. AIRM .LE. 0.0 .OR. PRES_PA .LE. 0.0 ) THEN
         XMSG = 'MET DATA ERROR, EXITING ROUTINE.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        write(0,*) ''
        write(0,*) 'TEMP :'
        write(0,*) TEMP
        write(0,*) 'PRES_PA :'
        write(0,*) PRES_PA
        write(0,*) 'TAUCLD :'
        write(0,*) TAUCLD
        write(0,*) 'PRCRATE :'
        write(0,*) PRCRATE
        write(0,*) 'WCAVG :'
        write(0,*) WCAVG
        write(0,*) 'WTAVG :'
        write(0,*) WTAVG
        write(0,*) 'AIRM :'
        write(0,*) AIRM
        write(0,*) 'ALFA0 :'
        write(0,*) ALFA0
        write(0,*) 'ALFA2 :'
        write(0,*) ALFA2
        write(0,*) 'ALFA3 :'
        write(0,*) ALFA3
        write(0,*) 'GAS :'
        write(0,*) GAS
        write(0,*) 'AEROSOL :'
        write(0,*) AEROSOL
        write(0,*) 'GASWDEP :'
        write(0,*) GASWDEP
        write(0,*) 'AERWDEP :'
        write(0,*) AERWDEP
        write(0,*) 'HPWDEP :'
        write(0,*) HPWDEP
        write(0,*) ''
        return
      END IF

!...initialize counters and compute several conversion factors

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
      TWASH = WTAVG * 1000.0 * CTHK1 * 3600.0 &
            / ( H2ODENS * MAX( 1.0E-20, PRCRATE ) )

!...set equilibrium constants as a function of temperature
!...   Henry's law constants

      SO2H  = HLCONST( 'SO2             ', TEMP, .FALSE., 0.0 )
      CO2H  = HLCONST( 'CO2             ', TEMP, .FALSE., 0.0 )
      NH3H  = HLCONST( 'NH3             ', TEMP, .FALSE., 0.0 )
      H2O2H = HLCONST( 'H2O2            ', TEMP, .FALSE., 0.0 )
      O3H   = HLCONST( 'O3              ', TEMP, .FALSE., 0.0 )
      HCLH  = HLCONST( 'HCL             ', TEMP, .FALSE., 0.0 )
      HNO3H = HLCONST( 'HNO3            ', TEMP, .FALSE., 0.0 )
      MHPH  = HLCONST( 'METHYLHYDROPEROX', TEMP, .FALSE., 0.0 )
      PAAH  = HLCONST( 'PEROXYACETIC_ACI', TEMP, .FALSE., 0.0 )
      FOAH  = HLCONST( 'FORMIC_ACID     ', TEMP, .FALSE., 0.0 )

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
      HCL1  = 1.74E+06 * EXP(  6.90E+03 * TEMP1 )      ! Marsh and McElroy (1985)
      HNO31 = 1.54E+01 * EXP(  8.70E+03 * TEMP1 )      ! Schwartz (1984)

!...Kinetic oxidation rates
!...   From Chamedies (1982)

!      RH2O2 = 8.0E+04 * EXP( -3650.0 * TEMP1 )
!KW based on CMAQv5.0 From Jacobson  (1997)
       RH2O2 = 7.45E+07 * EXP( -15.96E0 * ( ( 298.0E0 / TEMP )  - 1.0E0 ) )

!...From Kok

!      RMHP = 1.75E+07 * EXP( -3801.0 * TEMP1 )
!      RPAA = 3.64E+07 * EXP( -3994.0 * TEMP1 )
!KW based on CMAQv5.0 From Jacobson  (1997)

      RMHP = 1.90E+07 * EXP( -12.75E0 * ( ( 298.0E0 / TEMP )  - 1.0E0 ) )
      RPAA = 3.67E+07 * EXP( -13.42E0 * ( ( 298.0E0 / TEMP )  - 1.0E0 ) )

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
!...  secondary organic aerosol and water are not included

      M3OLD = ( AEROSOL( LSO4ACC  ) * SGRAERMW( LSO4ACC  ) / 1.8e6 &
            +   AEROSOL( LNH4ACC  ) * SGRAERMW( LNH4ACC  ) / 1.8e6 &
            +   AEROSOL( LNO3ACC  ) * SGRAERMW( LNO3ACC  ) / 1.8e6 &
            +   AEROSOL( LORGPACC ) * SGRAERMW( LORGPACC ) / 2.0e6 &
            +   AEROSOL( LECACC   ) * SGRAERMW( LECACC   ) / 2.2e6 &
            +   AEROSOL( LPRIACC  ) * SGRAERMW( LPRIACC  ) / 2.2e6 &
            +   AEROSOL( LNAACC   ) * SGRAERMW( LNAACC   ) / 2.2e6 &
            +   AEROSOL( LCLACC   ) * SGRAERMW( LCLACC   ) / 2.2e6 )
!cc     &      * 6.0 / PI    ! cancels out in division at end of subroutine

!...compute fractional weights for several species

      TOTNIT = GAS( LHNO3 ) + AEROSOL( LNO3ACC )
      IF ( TOTNIT .GT. 0.0 ) THEN
        FHNO3   = GAS( LHNO3 ) / TOTNIT
        FNO3ACC = AEROSOL( LNO3ACC ) / TOTNIT
      ELSE
        FHNO3   = 1.0
        FNO3ACC = 0.0
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
      ORGAACCA =  AEROSOL( LORGAACC )  * PRES_ATM_OVER_XL
      ORGPACCA =  AEROSOL( LORGPACC )  * PRES_ATM_OVER_XL
      ORGBACCA =  AEROSOL( LORGBACC )  * PRES_ATM_OVER_XL
      ECACCA  =   AEROSOL( LECACC  )   * PRES_ATM_OVER_XL
      PRIACCA =   AEROSOL( LPRIACC )   * PRES_ATM_OVER_XL
      NAACCA  =   AEROSOL( LNAACC  )   * PRES_ATM_OVER_XL
      CLACCA  =   AEROSOL( LCLACC  )   * PRES_ATM_OVER_XL

!...initial concentration from coarse-mode aerosol loading (mol/liter)
!...  an assumption is made that all of the coarse-mode
!...  aerosol mass in incorporated into the cloud droplets

      TS6CORA =   AEROSOL( LSO4COR )   * PRES_ATM_OVER_XL
      NO3CORA =   AEROSOL( LNO3COR )   * PRES_ATM_OVER_XL

      IF ( AE_VRSN .EQ. 'AE3' ) THEN
        CLCORA  = AEROSOL( LNACL   )   * PRES_ATM_OVER_XL
        NACORA  = AEROSOL( LNACL   )   * PRES_ATM_OVER_XL
      ELSE
        CLCORA  = AEROSOL( LCLCOR  )   * PRES_ATM_OVER_XL
        NACORA  = AEROSOL( LNACOR  )   * PRES_ATM_OVER_XL
      END IF

      KA      =   AEROSOL( LK      )   * PRES_ATM_OVER_XL
      CAA     =   AEROSOL( LCACO3  )   * PRES_ATM_OVER_XL
      MGA     =   AEROSOL( LMGCO3  )   * PRES_ATM_OVER_XL
      FEA     =   AEROSOL( LA3FE   )   * PRES_ATM_OVER_XL
      MNA     =   AEROSOL( LB2MN   )   * PRES_ATM_OVER_XL
      CO3A    = ( AEROSOL( LCACO3  ) &
              +   AEROSOL( LMGCO3  ) ) * PRES_ATM_OVER_XL
      PRICORA =   AEROSOL( LPRICOR )   * PRES_ATM_OVER_XL
      NUMCORA =   AEROSOL( LNUMCOR )   * PRES_ATM_OVER_XL

!...set constant factors that will be used in later multiplications (moles/atm)

      XLH2O2  = H2O2H * XL
      XLO3    = O3H   * XL
      XLMHP   = MHPH  * XL
      XLPAA   = PAAH  * XL
      XLSO2   = SO2H  * XL
      XLNH3   = NH3H  * XL
      XLHCL   = HCLH  * XL
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
      HCL1H   = HCL1  * HCLH
      HNO31H  = HNO31 * HNO3H

!...If kinetic calculations are made, return to this point

      I20C = 0
20    CONTINUE

      I20C = I20C + 1
      IF ( I20C .GE. 10000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I20C, EXITING ROUTINE.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        write(0,*) ''
        write(0,*) 'TEMP :'
        write(0,*) TEMP
        write(0,*) 'PRES_PA :'
        write(0,*) PRES_PA
        write(0,*) 'TAUCLD :'
        write(0,*) TAUCLD
        write(0,*) 'PRCRATE :'
        write(0,*) PRCRATE
        write(0,*) 'WCAVG :'
        write(0,*) WCAVG
        write(0,*) 'WTAVG :'
        write(0,*) WTAVG
        write(0,*) 'AIRM :'
        write(0,*) AIRM
        write(0,*) 'ALFA0 :'
        write(0,*) ALFA0
        write(0,*) 'ALFA2 :'
        write(0,*) ALFA2
        write(0,*) 'ALFA3 :'
        write(0,*) ALFA3
        write(0,*) 'GAS :'
        write(0,*) GAS
        write(0,*) 'AEROSOL :'
        write(0,*) AEROSOL
        write(0,*) 'GASWDEP :'
        write(0,*) GASWDEP
        write(0,*) 'AERWDEP :'
        write(0,*) AERWDEP
        write(0,*) 'HPWDEP :'
        write(0,*) HPWDEP
        write(0,*) ''
        return
      END IF

!...set aitken-mode aerosol loading (mol/liter)

      NO3AKNA = AEROSOL( LNO3AKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      NH4AKNA = AEROSOL( LNH4AKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      TS6AKNA = AEROSOL( LSO4AKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      ORGAAKNA = AEROSOL( LORGAAKN ) * PRES_ATM_OVER_XL &
               * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      ORGPAKNA = AEROSOL( LORGPAKN ) * PRES_ATM_OVER_XL &
               * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      ORGBAKNA = AEROSOL( LORGBAKN ) * PRES_ATM_OVER_XL &
               * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      ECAKNA  = AEROSOL( LECAKN  ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      PRIAKNA = AEROSOL( LPRIAKN ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      NAAKNA  = AEROSOL( LNAAKN  ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )
      CLAKNA  = AEROSOL( LCLAKN  ) * PRES_ATM_OVER_XL &
              * ( 1.0 - EXP( -ALFA3 * TIMEW ) )

!...Initial gas phase partial pressures (atm)
!...   = initial partial pressure - amount deposited partial pressure

      PSO20  = GAS( LSO2  ) * PRES_ATM &
             + DS4( 0 ) * XL &
             - ( WETDEP( LSO3L ) + WETDEP( LHSO3L ) + WETDEP( LSO2L ) ) * XC2
      PNH30  = GAS( LNH3  ) * PRES_ATM &
             + ( NH4ACCA + NH4AKNA ) * XL &
             - ( WETDEP( LNH4L ) + WETDEP( LNH3L ) ) * XC2
      PHNO30 = ( GAS( LHNO3 ) + 2.0 * GAS( LN2O5 ) ) * PRES_ATM &
             + ( NO3ACCA + NO3CORA + NO3AKNA ) * XL &
             - ( WETDEP( LNO3ACCL ) + WETDEP( LHNO3L ) + WETDEP( LNO3CORL ) ) * XC2
      PHCL0  = GAS(  LHCL ) * PRES_ATM &
             + ( CLACCA + CLCORA + CLAKNA ) * XL & ! new for sea salt
             - ( WETDEP( LCLACCL ) + WETDEP( LHCLL ) + WETDEP( LCLCORL ) ) * XC2
      PH2O20 = GAS( LH2O2 ) * PRES_ATM - WETDEP( LH2O2L ) * XC2
      PO30   = GAS( LO3   ) * PRES_ATM - WETDEP( LO3L   ) * XC2
      PFOA0  = GAS( LFOA  ) * PRES_ATM &
             - ( WETDEP( LFOAL ) + WETDEP( LHCO2L ) ) * XC2
      PMHP0  = GAS( LMHP  ) * PRES_ATM - WETDEP( LMHPL  ) * XC2
      PPAA0  = GAS( LPAA  ) * PRES_ATM - WETDEP( LPAAL  ) * XC2
      PCO20  = GAS( LCO2  ) * PRES_ATM &
             + CO3A * XL &
             - ( WETDEP( LCO3L ) + WETDEP( LHCO3L ) + WETDEP( LCO2L ) ) * XC2

!...don't allow gas concentrations to go below zero

      PSO20  = MAX( PSO20,  0.0 )
      PNH30  = MAX( PNH30,  0.0 )
      PH2O20 = MAX( PH2O20, 0.0 )
      PO30   = MAX( PO30,   0.0 )
      PFOA0  = MAX( PFOA0,  0.0 )
      PMHP0  = MAX( PMHP0,  0.0 )
      PPAA0  = MAX( PPAA0,  0.0 )
      PCO20  = MAX( PCO20,  0.0 )
      PHCL0  = MAX( PHCL0,  0.0 )
      PHNO30 = MAX( PHNO30, 0.0 )

!...Molar concentrations of soluble aerosols
!...   = Initial amount - amount deposited  (mol/liter)

      TS6COR  = MAX( TS6CORA - WETDEP( LTS6CORL ) * XC1, 0.0 )
      NO3COR  = MAX( NO3CORA - WETDEP( LNO3CORL ) * XC1, 0.0 )
      NACOR   = MAX( NACORA  - WETDEP( LNACORL  ) * XC1, 0.0 )
      CLCOR   = MAX( CLCORA  - WETDEP( LCLCORL  ) * XC1, 0.0 )

      TS6     = TS6ACCA  + TS6AKNA + TS6COR &
              - ( WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL ) ) * XC1 &
              - DS4( 0 )
      NA      = NAACCA   + NAAKNA  + NACOR &
              - WETDEP( LNAACCL ) * XC1
      CA      = CAA      -   WETDEP( LCAL ) * XC1
      MG      = MGA      -   WETDEP( LMGL ) * XC1
      K       = KA       -   WETDEP( LKL  ) * XC1
      FE      = FEA      -   WETDEP( LFEL ) * XC1
      MN      = MNA      -   WETDEP( LMNL ) * XC1
      ORGA    = ORGAACCA + ORGAAKNA - WETDEP( LORGAL ) * XC1
      ORGP    = ORGPACCA + ORGPAKNA - WETDEP( LORGPL ) * XC1
      ORGB    = ORGBACCA + ORGBAKNA - WETDEP( LORGBL ) * XC1
      EC      = ECACCA   + ECAKNA   - WETDEP( LECL   ) * XC1
      PRIM    = PRIACCA  + PRIAKNA  - WETDEP( LPRIML ) * XC1
      PRIMCOR = PRICORA - WETDEP( LPRIMCORL ) * XC1
      NUMCOR  = NUMCORA - WETDEP( LNUMCORL  ) * XC1
      A       = 3.0 * FE
      B       = 2.0 * MN

!...don't allow aerosol concentrations to go below zero

      TS6     = MAX( TS6,     0.0 )
      NA      = MAX( NA,      0.0 )
      CA      = MAX( CA,      0.0 )
      MG      = MAX( MG,      0.0 )
      K       = MAX( K,       0.0 )
      FE      = MAX( FE,      0.0 )
      MN      = MAX( MN,      0.0 )
      ORGA    = MAX( ORGA,    0.0 )
      ORGP    = MAX( ORGP,    0.0 )
      ORGB    = MAX( ORGB,    0.0 )
      EC      = MAX( EC,      0.0 )
      PRIM    = MAX( PRIM,    0.0 )
      PRIMCOR = MAX( PRIMCOR, 0.0 )
      NUMCOR  = MAX( NUMCOR,  0.0 )
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
      IF ( I7777C .GE. 10000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I7777C, EXITING ROUTINE.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        write(0,*) ''
        write(0,*) 'TEMP :'
        write(0,*) TEMP
        write(0,*) 'PRES_PA :'
        write(0,*) PRES_PA
        write(0,*) 'TAUCLD :'
        write(0,*) TAUCLD
        write(0,*) 'PRCRATE :'
        write(0,*) PRCRATE
        write(0,*) 'WCAVG :'
        write(0,*) WCAVG
        write(0,*) 'WTAVG :'
        write(0,*) WTAVG
        write(0,*) 'AIRM :'
        write(0,*) AIRM
        write(0,*) 'ALFA0 :'
        write(0,*) ALFA0
        write(0,*) 'ALFA2 :'
        write(0,*) ALFA2
        write(0,*) 'ALFA3 :'
        write(0,*) ALFA3
        write(0,*) 'GAS :'
        write(0,*) GAS
        write(0,*) 'AEROSOL :'
        write(0,*) AEROSOL
        write(0,*) 'GASWDEP :'
        write(0,*) GASWDEP
        write(0,*) 'AERWDEP :'
        write(0,*) AERWDEP
        write(0,*) 'HPWDEP :'
        write(0,*) HPWDEP
        write(0,*) ''
        return
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

      PHCLF = PHCL0 / ( 1.0 + XLHCL *  ( 1.0 + HCL1 * RECIPA1 ) )

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
      CL   = HCL1H   * PHCLF  * RECIPA1 ! new for sea salt

!...compute functional value

      FA = AE + NH4 + NA + 2.0 * ( CA + MG - CO3 - SO3 - SO4 ) &
         - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL

!...Start iteration and bisection ****************<<<<<<<

      I30C = 0
30    CONTINUE

      I30C = I30C + 1
      IF ( I30C .GE. 10000 ) THEN
        XMSG = 'EXCESSIVE LOOPING AT I30C, EXITING ROUTINE.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        write(0,*) ''
        write(0,*) 'TEMP :'
        write(0,*) TEMP
        write(0,*) 'PRES_PA :'
        write(0,*) PRES_PA
        write(0,*) 'TAUCLD :'
        write(0,*) TAUCLD
        write(0,*) 'PRCRATE :'
        write(0,*) PRCRATE
        write(0,*) 'WCAVG :'
        write(0,*) WCAVG
        write(0,*) 'WTAVG :'
        write(0,*) WTAVG
        write(0,*) 'AIRM :'
        write(0,*) AIRM
        write(0,*) 'ALFA0 :'
        write(0,*) ALFA0
        write(0,*) 'ALFA2 :'
        write(0,*) ALFA2
        write(0,*) 'ALFA3 :'
        write(0,*) ALFA3
        write(0,*) 'GAS :'
        write(0,*) GAS
        write(0,*) 'AEROSOL :'
        write(0,*) AEROSOL
        write(0,*) 'GASWDEP :'
        write(0,*) GASWDEP
        write(0,*) 'AERWDEP :'
        write(0,*) AERWDEP
        write(0,*) 'HPWDEP :'
        write(0,*) HPWDEP
        write(0,*) ''
        return
      END IF

      BB = ( HA + HB ) / 2.0
      AE = 10.0**( -BB )

      ICNTAQ = ICNTAQ + 1
      IF ( ICNTAQ .GE. 60000 ) THEN
        XMSG = 'Maximum AQCHEM total iterations exceeded, EXITING ROUTINE.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        write(0,*) ''
        write(0,*) 'TEMP :'
        write(0,*) TEMP
        write(0,*) 'PRES_PA :'
        write(0,*) PRES_PA
        write(0,*) 'TAUCLD :'
        write(0,*) TAUCLD
        write(0,*) 'PRCRATE :'
        write(0,*) PRCRATE
        write(0,*) 'WCAVG :'
        write(0,*) WCAVG
        write(0,*) 'WTAVG :'
        write(0,*) WTAVG
        write(0,*) 'AIRM :'
        write(0,*) AIRM
        write(0,*) 'ALFA0 :'
        write(0,*) ALFA0
        write(0,*) 'ALFA2 :'
        write(0,*) ALFA2
        write(0,*) 'ALFA3 :'
        write(0,*) ALFA3
        write(0,*) 'GAS :'
        write(0,*) GAS
        write(0,*) 'AEROSOL :'
        write(0,*) AEROSOL
        write(0,*) 'GASWDEP :'
        write(0,*) GASWDEP
        write(0,*) 'AERWDEP :'
        write(0,*) AERWDEP
        write(0,*) 'HPWDEP :'
        write(0,*) HPWDEP
        write(0,*) ''
        return
      END IF

      RECIPA1 = 1.0 / ( AE * ACT1 )
      RECIPA2 = 1.0 / ( AE * AE * ACT2 )

!...calculate final gas phase partial pressure of SO2, NH3, HCL, HNO3
!...  HCOOH, and CO2 (atm)

      PSO2F = PSO20 / ( 1.0 + XLSO2 &
      	    * ( 1.0 + SO21 * RECIPA1 + SO212 * RECIPA2 ) )

      PNH3F = PNH30 / ( 1.0 + XLNH3 * ( 1.0 + NH3DH20 * AE ) )

      PHCLF = PHCL0  / ( 1.0 + XLHCL *  ( 1.0 + HCL1 * RECIPA1 ) )

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
      CL   = HCL1H   * PHCLF  * RECIPA1 ! new for sea salt

!...compute functional value

      FB = AE + NH4 + NA + 2.0 * ( CA + MG - CO3 - SO3 - SO4 ) &
           - OH - HCO3 - HSO3 - NO3 - HSO4 - HCO2 - CL

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
        XMSG = 'PH VALUE OUT OF RANGE, EXITING ROUTINE.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        write(0,*) ''
        write(0,*) 'TEMP :'
        write(0,*) TEMP
        write(0,*) 'PRES_PA :'
        write(0,*) PRES_PA
        write(0,*) 'TAUCLD :'
        write(0,*) TAUCLD
        write(0,*) 'PRCRATE :'
        write(0,*) PRCRATE
        write(0,*) 'WCAVG :'
        write(0,*) WCAVG
        write(0,*) 'WTAVG :'
        write(0,*) WTAVG
        write(0,*) 'AIRM :'
        write(0,*) AIRM
        write(0,*) 'ALFA0 :'
        write(0,*) ALFA0
        write(0,*) 'ALFA2 :'
        write(0,*) ALFA2
        write(0,*) 'ALFA3 :'
        write(0,*) ALFA3
        write(0,*) 'GAS :'
        write(0,*) GAS
        write(0,*) 'AEROSOL :'
        write(0,*) AEROSOL
        write(0,*) 'GASWDEP :'
        write(0,*) GASWDEP
        write(0,*) 'AERWDEP :'
        write(0,*) AERWDEP
        write(0,*) 'HPWDEP :'
        write(0,*) HPWDEP
        write(0,*) ''
        return
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
      HCLL  = PHCLF  * HCLH
      HNO3L = PHNO3F * HNO3H

!...compute modal concentrations

      SO4COR  = SK6 * TS6COR / ( AE * GM2 + SK6 )
      HSO4COR = MAX( TS6COR - SO4COR, 0.0 )

      TS6ACC  = MAX( TS6  - TS6COR,   0.0 )
      SO4ACC  = MAX( SO4  - SO4COR,   0.0 )
      HSO4ACC = MAX( HSO4 - HSO4COR,  0.0 )
      NO3ACC  = MAX( NO3  - NO3COR,   0.0 )
      NAACC   = MAX( NA   - NACOR,    0.0 )
      CLACC   = MAX( CL   - CLCOR,    0.0 )

!...load the liquid concentration array with current values

      LIQUID( LACL      ) = AC
      LIQUID( LNH4L     ) = NH4
      LIQUID( LCAL      ) = CA
      LIQUID( LNAACCL   ) = NAACC
      LIQUID( LOHL      ) = OH
      LIQUID( LSO4ACCL  ) = SO4ACC
      LIQUID( LHSO4ACCL ) = HSO4ACC
      LIQUID( LSO3L     ) = SO3
      LIQUID( LHSO3L    ) = HSO3
      LIQUID( LSO2L     ) = SO2L
      LIQUID( LCO3L     ) = CO3
      LIQUID( LHCO3L    ) = HCO3
      LIQUID( LCO2L     ) = CO2L
      LIQUID( LNO3ACCL  ) = NO3ACC
      LIQUID( LNH3L     ) = NH3L
      LIQUID( LCLACCL   ) = CLACC
      LIQUID( LH2O2L    ) = H2O2L
      LIQUID( LO3L      ) = O3L
      LIQUID( LFEL      ) = FE
      LIQUID( LMNL      ) = MN
      LIQUID( LAL       ) = A
      LIQUID( LFOAL     ) = FOAL
      LIQUID( LHCO2L    ) = HCO2
      LIQUID( LMHPL     ) = MHPL
      LIQUID( LPAAL     ) = PAAL
      LIQUID( LHCLL     ) = HCLL
      LIQUID( LORGAL    ) = ORGA
      LIQUID( LPRIML    ) = PRIM
      LIQUID( LMGL      ) = MG
      LIQUID( LKL       ) = K
      LIQUID( LBL       ) = B
      LIQUID( LHNO3L    ) = HNO3L
      LIQUID( LPRIMCORL ) = PRIMCOR
      LIQUID( LNUMCORL  ) = NUMCOR
      LIQUID( LTS6CORL  ) = TS6COR
      LIQUID( LNACORL   ) = NACOR
      LIQUID( LCLCORL   ) = CLCOR
      LIQUID( LNO3CORL  ) = NO3COR
      LIQUID( LORGPL    ) = ORGP
      LIQUID( LORGBL    ) = ORGB
      LIQUID( LECL      ) = EC

!...if the maximum cloud lifetime has not been reached, then compute
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

!KW        DSIVDT( 1 ) = -RH2O2 * H2O2L * SO2L / ( 0.1 + AC )
!KW based on CMAQv5.0
        DSIVDT( 1 ) = -RH2O2 * H2O2L * HSO3 * AC / ( 0.1 + 13.0 * AC )
       
        TOTOX = PH2O20 * ONE_OVER_XL
        IF ( ( DSIVDT( 1 ) .EQ. 0.0 ) .OR. &
             ( TSIV  .LE. CONCMIN ) .OR. &
             ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 1 ) = DTRMV
        ELSE
          DTW( 1 ) = -0.05 * MIN( TOTOX, TSIV ) / DSIVDT( 1 )
        END IF

!...Calculate sulfur iv oxidation rate due to O3

!KW        IF ( BB .GE. 2.7 ) THEN
!KW          DSIVDT( 2 ) = -4.19E5 * ( 1.0 + 2.39E-4 / AC ) * O3L * SIV
!KW        ELSE
!KW          DSIVDT( 2 ) = -1.9E4 * SIV * O3L / SQRT( AC )
!KW        END IF
!KW based on CMAQv5.0
        DSIVDT( 2 ) = -( 2.4E4 * SO2L                                          + &
                        3.7E5 * EXP( -18.56 * ( ( 298.0E0 / TEMP ) - 1.0E0 ) ) * HSO3 + &
                        1.5E9 * EXP( -17.72 * ( ( 298.0E0 / TEMP ) - 1.0E0 ) ) * SO3 ) * O3L
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

!KW        IF ( BB .GE. 4.0 )  THEN  ! 4.0  < pH

!          IF ( SIV .LE. 1.0E-5 ) THEN
!            DSIVDT( 3 ) = -5000.0 * MN * HSO3
!          ELSE IF ( SIV .GT. 1.0E-5 ) THEN
!            DSIVDT( 3 ) = -( 4.7 * MN * MN / AC &
!                        + 1.0E7 * FE * SIV * SIV )
!          END IF  ! end of first pass through SIV conc.

!        ELSE          ! pH , + 4.0

!	  IF ( SIV .LE. 1.0E-5 ) THEN
!            DSIVDT( 3 ) = -3.0 * ( 5000.0 * MN * HSO3 &
!                        + 0.82 * FE * SIV / AC )
!          ELSE
!            DSIVDT( 3 ) = -( 4.7 * MN * MN / AC &
!                        + ( 0.82 * FE * SIV / AC ) &
!                        * ( 1.0 + 1.7E3 * MN**1.5 / ( 6.3E-6 + FE ) ) )
!          END IF ! end of second pass through SIV conc.

!KW        END IF  ! end of pass through pH

!KW based on CMAQv5.0
!...Calculate sulfur iv oxidation rate due to 02 catalyzed by Mn++ and Fe+++
!...(Martin and Goodman, 1991) prescribled 0.01 ug/m3 for FeIII and 0.005 ug/m3 for MnII
        DSIVDT( 3 ) = - ( 750.0E0  * MN * SIV +            &     ! GS 4May2011
                         2600.0E0 * FE * SIV +             &    ! GS 4May2011
                         1.0E10   * MN * FE * SIV )         ! GS 4May2011

        IF ( ( DSIVDT( 3 ) .EQ. 0.0 ) .OR. ( TSIV .LE. CONCMIN ) ) THEN
          DTW( 3 ) = DTRMV
        ELSE
          DTW( 3 ) = -0.1 * TSIV / DSIVDT( 3 )
        END IF

!...Calculate sulfur oxidation rate due to MHP

        DSIVDT( 4 ) = -RMHP * AC * MHPL * HSO3
        TOTOX = PMHP0 * ONE_OVER_XL
        IF ( ( DSIVDT( 4 ) .EQ. 0.0 ) .OR. &
             ( TSIV  .LE. CONCMIN ) .OR. &
             ( TOTOX .LE. CONCMIN ) ) THEN
          DTW( 4 ) = DTRMV
        ELSE
          DTW( 4 ) = -0.1 * MIN( TOTOX, TSIV ) / DSIVDT( 4 )
        END IF

!...Calculate sulfur oxidation due to PAA

!KW        DSIVDT( 5 ) = -RPAA * HSO3 * PAAL * ( AC + 1.65E-5 )
!KW based on CMAQv5.0
        DSIVDT( 5 ) = -( RPAA * AC + 7.00E2 ) * HSO3 * PAAL
        TOTOX = PPAA0 * ONE_OVER_XL
        IF ( ( DSIVDT( 5 ) .EQ. 0.0 ) .OR. &
             ( TSIV  .LE. CONCMIN ) .OR. &
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
        DTW( 0 ) = MIN( DTW( 0 ), DTRMV )

!...Limit the timestep to prevent negative SO2 concentrations and mass creation
!...  for sulfate (suggested by Bonyoung Koo)

        IF ( DSIVDT( 0 ) .LT. 0.0 ) THEN
          DTW( 0 ) = MIN( DTW( 0 ), -TSIV * 1.00001 / DSIVDT( 0 ) )
        END IF

!...If the total time after this time increment will be greater than
!...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        IF ( TIMEW + DTW( 0 ) .GT. TAUCLD ) DTW( 0 ) = TAUCLD - TIMEW
!CC        IF ( TS6 .LT. 1.0E-11 ) DTW( 0 ) = TAUCLD - TIMEW
        IF ( ITERAT .GT. 100 ) DTW( 0 ) = TAUCLD - TIMEW

!...limit timestep to no more than the washout time

        DTW( 0 ) = MIN( DTW( 0 ), TWASH )

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
!...  scavenging aerosol number in the accumulation mode

      DEPSUM = ( WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL ) ) * XC1

      IF ( ( TS6ACCA + TS6AKNA - DS4( 0 ) ) .NE. 0.0 ) THEN
        BETASO4 = DEPSUM / ( ( TS6ACCA + TS6AKNA - DS4( 0 ) ) * TAUCLD )
      ELSE
        BETASO4 = 0.0
      END IF

      EBETASO4T = EXP( -BETASO4 * TAUCLD )
      EALFA0T   = EXP( -ALFA0 * TAUCLD )
      EALFA2T   = EXP( -ALFA2 * TAUCLD )
      EALFA3T   = EXP( -ALFA3 * TAUCLD )

!...Compute the output concentrations and wet deposition amounts

      TOTAMM = ( PNH3F  + ( NH4 + NH3L  ) * XL ) * RECIPAP1
      TOTNIT = ( PHNO3F + ( NO3ACC + HNO3L ) * XL ) * RECIPAP1

!...gas-phase species wet deposition (mm mol/lit)

      GASWDEP( LSO2   ) = WETDEP( LSO3L  ) + WETDEP( LHSO3L ) &
                        + WETDEP( LSO2L  )
      GASWDEP( LNH3   ) = WETDEP( LNH3L  )
      GASWDEP( LH2O2  ) = WETDEP( LH2O2L )
      GASWDEP( LO3    ) = WETDEP( LO3L   )
      GASWDEP( LCO2   ) = WETDEP( LCO3L  ) + WETDEP( LHCO3L ) &
                        + WETDEP( LCO2L  )
      GASWDEP( LFOA   ) = WETDEP( LFOAL  ) + WETDEP( LHCO2L )
      GASWDEP( LMHP   ) = WETDEP( LMHPL  )
      GASWDEP( LPAA   ) = WETDEP( LPAAL  )
      GASWDEP( LHCL   ) = WETDEP( LHCLL  )
      GASWDEP( LHNO3  ) = WETDEP( LHNO3L )
      GASWDEP( LN2O5  ) = 0.0
      GASWDEP( LH2SO4 ) = 0.0

!...gas concentrations (mol/molV)

      GAS( LSO2   ) = ( PSO2F   + XL *  SIV )   * RECIPAP1
      GAS( LH2O2  ) = ( PH2O2F  + XL *  H2O2L ) * RECIPAP1
      GAS( LO3    ) = ( PO3F    + XL *  O3L )   * RECIPAP1
      GAS( LCO2   ) = ( PCO2F   + XL *  CO2L )  * RECIPAP1
      GAS( LFOA   ) = ( PFOAF   + XL * ( FOAL + HCO2 ) ) * RECIPAP1
      GAS( LMHP   ) = ( PMHPF   + XL *  MHPL )  * RECIPAP1
      GAS( LPAA   ) = ( PPAAF   + XL *  PAAL )  * RECIPAP1
      GAS( LHCL   ) = ( PHCLF   + XL *  HCLL )  * RECIPAP1

      GAS( LNH3   ) = FNH3  * TOTAMM
      GAS( LHNO3  ) = FHNO3 * TOTNIT
      GAS( LN2O5  ) = 0.0 ! assume all into aerosol
      GAS( LH2SO4 ) = 0.0 ! assume all into aerosol

!...aerosol species wet deposition (mm mol/lit)
!...  there is no wet deposition of aitken particles, they attached
!...  to the accumulation mode particles

      AERWDEP( LSO4AKN ) = 0.0
      AERWDEP( LNH4AKN ) = 0.0
      AERWDEP( LNO3AKN ) = 0.0
      AERWDEP( LECAKN  ) = 0.0
      AERWDEP( LPRIAKN ) = 0.0

      AERWDEP( LORGAAKN ) = 0.0
      AERWDEP( LORGPAKN ) = 0.0
      AERWDEP( LORGBAKN ) = 0.0

      AERWDEP( LSO4ACC ) = WETDEP( LSO4ACCL ) + WETDEP( LHSO4ACCL )
      AERWDEP( LNH4ACC ) = WETDEP( LNH4L    )
      AERWDEP( LNO3ACC ) = WETDEP( LNO3ACCL )
      AERWDEP( LECACC  ) = WETDEP( LECL     )
      AERWDEP( LPRIACC ) = WETDEP( LPRIML   )

      AERWDEP( LORGAACC ) = WETDEP( LORGAL )
      AERWDEP( LORGPACC ) = WETDEP( LORGPL )
      AERWDEP( LORGBACC ) = WETDEP( LORGBL )

      AERWDEP( LSO4COR ) = WETDEP( LTS6CORL  )
      AERWDEP( LNO3COR ) = WETDEP( LNO3CORL  )
      AERWDEP( LPRICOR ) = WETDEP( LPRIMCORL )

      IF ( AE_VRSN .EQ. 'AE3' ) THEN
        AERWDEP( LNACL  ) = WETDEP( LNACORL )
      ELSE
        AERWDEP( LNAAKN ) = 0.0
        AERWDEP( LCLAKN ) = 0.0
        AERWDEP( LNAACC ) = WETDEP( LNAACCL )
        AERWDEP( LCLACC ) = WETDEP( LCLACCL )
        AERWDEP( LNACOR ) = WETDEP( LNACORL )
        AERWDEP( LCLCOR ) = WETDEP( LCLCORL )
      END IF

      AERWDEP( LK      ) = WETDEP( LKL  )
      AERWDEP( LA3FE   ) = WETDEP( LFEL )
      AERWDEP( LB2MN   ) = WETDEP( LMNL )
      AERWDEP( LCACO3  ) = WETDEP( LCAL )
      AERWDEP( LMGCO3  ) = WETDEP( LMGL )

      AERWDEP( LNUMAKN ) = 0.0
      AERWDEP( LNUMACC ) = 0.0
      AERWDEP( LNUMCOR ) = 0.0
      AERWDEP( LSRFAKN ) = 0.0
      AERWDEP( LSRFACC ) = 0.0

!...aerosol concentrations (mol/molV)

      AEROSOL( LSO4AKN ) = AEROSOL( LSO4AKN ) * EALFA3T
      AEROSOL( LNH4AKN ) = AEROSOL( LNH4AKN ) * EALFA3T
      AEROSOL( LNO3AKN ) = AEROSOL( LNO3AKN ) * EALFA3T
      AEROSOL( LECAKN  ) = AEROSOL( LECAKN  ) * EALFA3T
      AEROSOL( LPRIAKN ) = AEROSOL( LPRIAKN ) * EALFA3T

      AEROSOL( LORGAAKN ) = AEROSOL( LORGAAKN ) * EALFA3T
      AEROSOL( LORGPAKN ) = AEROSOL( LORGPAKN ) * EALFA3T
      AEROSOL( LORGBAKN ) = AEROSOL( LORGBAKN ) * EALFA3T

      AEROSOL( LSO4ACC ) = TS6ACC * XL * RECIPAP1
      AEROSOL( LECACC  ) = EC     * XL * RECIPAP1
      AEROSOL( LPRIACC ) = PRIM   * XL * RECIPAP1

      AEROSOL( LORGAACC ) = ORGA * XL * RECIPAP1
      AEROSOL( LORGPACC ) = ORGP * XL * RECIPAP1
      AEROSOL( LORGBACC ) = ORGB * XL * RECIPAP1

      AEROSOL( LNH4ACC ) = FNH4ACC * TOTAMM
      AEROSOL( LNO3ACC ) = FNO3ACC * TOTNIT

      AEROSOL( LSO4COR ) = TS6COR * XL * RECIPAP1
      AEROSOL( LNO3COR ) = NO3COR * XL * RECIPAP1
      AEROSOL( LPRICOR ) = PRIMCOR* XL * RECIPAP1
      AEROSOL( LK      ) = K      * XL * RECIPAP1
      AEROSOL( LA3FE   ) = FE     * XL * RECIPAP1
      AEROSOL( LB2MN   ) = MN     * XL * RECIPAP1
      AEROSOL( LCACO3  ) = CA     * XL * RECIPAP1
      AEROSOL( LMGCO3  ) = MG     * XL * RECIPAP1

      IF ( AE_VRSN .EQ. 'AE3' ) THEN
        AEROSOL( LNACL  ) = NACOR * XL * RECIPAP1
      ELSE
        AEROSOL( LNAAKN ) = AEROSOL( LNAAKN ) * EALFA3T
        AEROSOL( LCLAKN ) = AEROSOL( LCLAKN ) * EALFA3T
        AEROSOL( LNAACC ) = NAACC * XL * RECIPAP1
        AEROSOL( LCLACC ) = CLACC * XL * RECIPAP1
        AEROSOL( LNACOR ) = NACOR * XL * RECIPAP1
        AEROSOL( LCLCOR ) = CLCOR * XL * RECIPAP1
      END IF

      AEROSOL( LNUMAKN ) = AEROSOL( LNUMAKN ) * EALFA0T
      AEROSOL( LNUMACC ) = AEROSOL( LNUMACC ) * EBETASO4T
      AEROSOL( LNUMCOR ) = NUMCOR * XL * RECIPAP1

!...compute the final accumulation aerosol 3rd moment

      M3NEW = ( AEROSOL( LSO4ACC  ) * SGRAERMW( LSO4ACC  ) / 1.8e6 &
            +   AEROSOL( LNH4ACC  ) * SGRAERMW( LNH4ACC  ) / 1.8e6 &
            +   AEROSOL( LNO3ACC  ) * SGRAERMW( LNO3ACC  ) / 1.8e6 &
            +   AEROSOL( LORGPACC ) * SGRAERMW( LORGPACC ) / 2.0e6 &
            +   AEROSOL( LECACC   ) * SGRAERMW( LECACC   ) / 2.2e6 &
            +   AEROSOL( LPRIACC  ) * SGRAERMW( LPRIACC  ) / 2.2e6 &
            +   AEROSOL( LNAACC   ) * SGRAERMW( LNAACC   ) / 2.2e6 &
            +   AEROSOL( LCLACC   ) * SGRAERMW( LCLACC   ) / 2.2e6 )
!CC     &      * 6.0 / PI      ! cancels out in division below

      AEROSOL( LSRFAKN ) = AEROSOL( LSRFAKN ) * EALFA2T
      AEROSOL( LSRFACC ) = AEROSOL( LSRFACC ) &
                         * ( EXP( -BETASO4 * TAUCLD * ONETHIRD ) ) &
                         * ( M3NEW / MAX( M3OLD, CONCMIN) ) ** TWOTHIRDS

!...store the amount of hydrogen deposition

      HPWDEP = WETDEP( LACL )

      RETURN

!...formats

1001  FORMAT (1X,'STORM RATE=', F6.3, 'DSIVDT(0) =', F10.5, &
             'TS6=', F10.5, 'DTW(0)=', F10.5, 'CTHK1=', F10.5, &
             'WTAVG=', F10.5)

      END SUBROUTINE AQCHEM

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
! $Header: /project/work/rep/CCTM/src/cloud/cloud_acm/hlconst.F,v 1.15 2008/05/21 12:34:14 sjr Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

      REAL FUNCTION HLCONST ( CNAME, TEMP, EFFECTIVE, HPLUS )

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
!  J.Gipson.  09/06/02  added Henry's Law constants 59-73   for toxics
!  S.Roselle  11/07/02  added capability for calculating the effective
!                       Henry's law constant and updated coefficients
!                       in Henry's law constant table
!  J.Gipson   08/06/03  added Henry's Law constants 77-79
!  G.Sarwar   11/21/04  added constants for chlorine chemistry (Henry's
!                       law constants 80-85 and dissociation constants
!                       14-16
!  R.Bullock  07/05/05  added Henry's Law constants 86-87 for mercury 
!                       with enthalpy calculated from cited laboratory 
!                       data fit to an Arrhenius equation
!  W.Hutzell  02/14/06  added HLC 88 to 116, dissociation constant for
!                       17 (hydrazine)
!  A.Carlton  09/20/06  updated Henry's Law constants for 1,7,19,20,21,30 
!                       O3, NO3, hexane, octane, nonane, methanol and
!                       isoprene reference
!  S.Roselle  10/10/07  changed pointers to parameters; reformatted
!                       variable declarations
!-----------------------------------------------------------------------

      IMPLICIT NONE

!...........INCLUDES and their descriptions

!      INCLUDE SUBST_IODECL              ! I/O definitions and declarations
!      INCLUDE SUBST_IOPARMS             ! I/O parameters definitions

!...........PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: MXSPCS = 116     ! Number of substances
      INTEGER, PARAMETER :: MXDSPCS = 17     ! Number of dissociating species

!...pointers for the dissociation constants (array B and D)

      INTEGER, PARAMETER :: LSO2       =  1  ! SO2
      INTEGER, PARAMETER :: LHSO3      =  2  ! HSO3
      INTEGER, PARAMETER :: LHNO2      =  3  ! HNO3
      INTEGER, PARAMETER :: LHNO3      =  4  ! HNO3
      INTEGER, PARAMETER :: LCO2       =  5  ! CO2
      INTEGER, PARAMETER :: LHCO3      =  6  ! HCO3
      INTEGER, PARAMETER :: LH2O2      =  7  ! H2O2
      INTEGER, PARAMETER :: LHCHO      =  8  ! HCHO
      INTEGER, PARAMETER :: LHCOOH     =  9  ! HCOOH
      INTEGER, PARAMETER :: LHO2       = 10  ! HO2
      INTEGER, PARAMETER :: LNH4OH     = 11  ! NH4OH
      INTEGER, PARAMETER :: LH2O       = 12  ! H2O
      INTEGER, PARAMETER :: LATRA      = 13  ! Atrazine
      INTEGER, PARAMETER :: LCL2       = 14  ! CL2
      INTEGER, PARAMETER :: LHOCL      = 15  ! HOCL
      INTEGER, PARAMETER :: LHCL       = 16  ! HCL
      INTEGER, PARAMETER :: LHYDRAZINE = 17  ! Hydrazine

!...........ARGUMENTS and their descriptions

      CHARACTER*(*) CNAME               ! name of substance
      REAL          TEMP                ! temperature (K)
      LOGICAL       EFFECTIVE           ! true=compute the effective henry's law constant
      REAL          HPLUS               ! hydrogen ion concentration (mol/l)

!...........SCRATCH LOCAL VARIABLES and their descriptions:

      CHARACTER(  7 ), SAVE :: PNAME = 'HLCONST'  ! program name
      CHARACTER( 16 ), SAVE :: SUBNAME( MXSPCS )  ! list of substance names

      CHARACTER( 120 ) :: XMSG = ' '    ! exit status message string

      INTEGER       SPC                 ! species index

      REAL          HPLUSI              ! 1 / HPLUS
      REAL          HPLUS2I             ! 1 / HPLUS**2
      REAL          CLMINUS             ! chlorine ion conc [CL-]
      REAL          CLMINUSI            ! 1 / CLMINUS
      REAL          TFAC                ! (298-T)/(T*298)
      REAL          AKEQ1               ! temp var for dissociation constant
      REAL          AKEQ2               ! temp var for dissociation constant
      REAL          OHION               ! OH ion concentration
      REAL          KH                  ! temp var for henry's law constant

!...Henry's law constant data taken mostly from Rolf Sanders' Compilation of
!...  Henry's Law Constants for Inorganic and Organic Species of Potential
!...  Importance in Environment Chemistry 1999

      REAL, SAVE :: A( MXSPCS )         ! Henry's law constants at 298.15K (M/atm)
      REAL, SAVE :: E( MXSPCS )         ! enthalpy (like activation energy) (K)

!...dissociation constant data taken mostly from 6.A.1 of Seinfeld and Pandis
!...  Atmospheric Chemistry and Physics, 1997

      REAL, SAVE :: B( MXDSPCS )        ! dissociation constant at 298.15K (M or M2)
      REAL, SAVE :: D( MXDSPCS )        ! -dH/R (K)

      DATA SUBNAME(  1), A(  1), E(  1) / 'O3              ', 1.14E-02, 2.3E+03 / ! Kosak 1983
      DATA SUBNAME(  2), A(  2), E(  2) / 'HO2             ', 4.0E+03, 5.9E+03 /  ! Hanson et al. 1992
      DATA SUBNAME(  3), A(  3), E(  3) / 'H2O2            ', 8.3E+04, 7.4E+03 /  ! O'Sullivan et al. 1996
      DATA SUBNAME(  4), A(  4), E(  4) / 'NH3             ', 6.1E+01, 4.2E+03 /  ! Clegg and Brimblecombe 1989
      DATA SUBNAME(  5), A(  5), E(  5) / 'NO              ', 1.9E-03, 1.4E+03 /  ! Lide and Frederikse 1995
      DATA SUBNAME(  6), A(  6), E(  6) / 'NO2             ', 1.2E-02, 2.5E+03 /  ! Chameides 1984
      DATA SUBNAME(  7), A(  7), E(  7) / 'NO3             ', 0.6E+00, 0.0E+00 /  ! Rudich, Talukdar et al.1996
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
      DATA SUBNAME( 19), A( 19), E( 19) / 'HEXANE          ', 0.1E-03, 7.5E+03 /  ! Ashworth, Howe et al 1988
      DATA SUBNAME( 20), A( 20), E( 20) / 'OCTANE          ', 2.9E-03, 7.8E+03 /  ! Hansen et al. 1993
      DATA SUBNAME( 21), A( 21), E( 21) / 'NONANE          ', 2.4E-03, 2.1E+02 /  ! Ashworth, Howe et al 1988
      DATA SUBNAME( 22), A( 22), E( 22) / 'DECANE          ', 1.4E-04, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 23), A( 23), E( 23) / 'ETHENE          ', 4.7E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 24), A( 24), E( 24) / 'PROPENE         ', 4.8E-03, 0.0E+00 /  ! Mackay and Shiu 1981
      DATA SUBNAME( 25), A( 25), E( 25) / 'ISOPRENE        ', 2.8E-02, 0.0E+00 /  ! Karl, Lindinger et al 2003
      DATA SUBNAME( 26), A( 26), E( 26) / 'ACETYLENE       ', 4.1E-02, 1.8E+03 /  ! Wilhelm et al. 1977
      DATA SUBNAME( 27), A( 27), E( 27) / 'BENZENE         ', 1.6E-01, 4.1E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 28), A( 28), E( 28) / 'TOLUENE         ', 1.5E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 29), A( 29), E( 29) / 'O-XYLENE        ', 1.9E-01, 4.0E+03 /  ! Staudinger and Roberts 1996
      DATA SUBNAME( 30), A( 30), E( 30) / 'METHANOL        ', 2.2E+02, 5.2E+03 /  ! Snider and Dawson 1985
      DATA SUBNAME( 31), A( 31), E( 31) / 'ETHANOL         ', 1.9E+02, 6.6E+03 /  ! Snider and Dawson 1985
      DATA SUBNAME( 32), A( 32), E( 32) / '2-CRESOL        ', 8.2E+02, 0.0E+00 /  ! Betterton 1992
      DATA SUBNAME( 33), A( 33), E( 33) / '4-CRESOL        ', 1.3E+02, 0.0E+00 /  ! Betterton 1992
      DATA SUBNAME( 34), A( 34), E( 34) / 'METHYLHYDROPEROX', 3.1E+02, 5.2E+03 /  ! O'Sullivan et al. 1996
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
      DATA SUBNAME( 49), A( 49), E( 49) / 'PEROXYACETIC_ACI', 8.4E+02, 5.3E+03 /  ! O'Sullivan et al. 1996
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
      DATA SUBNAME( 74), A( 74), E( 74) / 'PPN             ', 2.9E+00, 0.0E+00 /  ! Kames and Schurath (1995)
      DATA SUBNAME( 75), A( 75), E( 75) / 'NAPHTHALENE     ', 2.0E+00, 3.6E+03 /  ! USEPA 1982
      DATA SUBNAME( 76), A( 76), E( 76) / 'QUINOLINE       ', 3.7E+03, 5.4E+03 /  ! USEPA 1982
      DATA SUBNAME( 77), A( 77), E( 77) / 'MEK             ', 2.0E+01, 5.0E+03 /  ! Zhou and Mopper 1990
      DATA SUBNAME( 78), A( 78), E( 78) / 'MVK             ', 4.1E+01, 0.0E+00 /  ! Iraci et al. 1998
      DATA SUBNAME( 79), A( 79), E( 79) / 'METHACROLEIN    ', 6.5E+00, 0.0E+00 /  ! Iraci et al. 1998
      DATA SUBNAME( 80), A( 80), E( 80) / 'CL2             ', 8.6E-02, 2.0E+03 /  ! ROLF SANDERS COMPILATION (1999)/KAVANAUGH AND TRUSSELL (1980)
      DATA SUBNAME( 81), A( 81), E( 81) / 'HOCL            ', 6.6E+02, 5.9E+03 /  ! ROLF SANDERS COMPILATION (1999)/HUTHWELKER ET AL (1995)
      DATA SUBNAME( 82), A( 82), E( 82) / 'HCL             ', 1.9E+01, 6.0E+02 /  ! ROLF SANDERS COMPILATION (1999)/DEAN (1992)
      DATA SUBNAME( 83), A( 83), E( 83) / 'FMCL            ', 1.1E+00, 0.0E+00 /  ! EPA SUITE PROGRAM/UNIT CONVERTED TO MATCH THE DEFINITION BY ROLF SANDERS.
      DATA SUBNAME( 84), A( 84), E( 84) / 'ICL1            ', 6.9E+01, 0.0E+00 /  ! EPA SUITE PROGRAM/UNIT CONVERTED TO MATCH THE DEFINITION BY ROLF SANDERS.
      DATA SUBNAME( 85), A( 85), E( 85) / 'ICL2            ', 6.9E+01, 0.0E+00 /  ! EPA SUITE PROGRAM/ASSUMED EQUAL TO THAT OF ICL1
      DATA SUBNAME( 86), A( 86), E( 86) / 'HG              ', 1.11E-01, 4.97E+03 /! Elemental Mercury from Clever et al. (1985)
      DATA SUBNAME( 87), A( 87), E( 87) / 'HGIIGAS         ', 1.41E+06, 5.26E+03 /! Hg(II) gas as mercuric chloride from Lindqvist and Rodhe (1985)
      DATA SUBNAME( 88), A( 88), E( 88) / 'TECDD_2378      ', 5.1E+00, 3.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 89), A( 89), E( 89) / 'PECDD_12378     ', 4.6E+00, 3.2E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 90), A( 90), E( 90) / 'HXCDD_123478    ', 8.1E+00, 2.9E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 91), A( 91), E( 91) / 'HXCDD_123678    ', 2.9E+00, 2.8E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 92), A( 92), E( 92) / 'HXCDD_123789    ', 6.5E+00, 2.7E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 93), A( 93), E( 93) / 'HPCDD_1234678   ', 1.2E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 94), A( 94), E( 94) / 'OTCDD           ', 9.8E+00, 2.3E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 95), A( 95), E( 95) / 'TECDF_2378      ', 8.5E+01, 3.7E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 96), A( 96), E( 96) / 'PECDF_12378     ', 5.2E+01, 2.9E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 97), A( 97), E( 97) / 'PECDF_23478     ', 1.8E+02, 3.0E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 98), A( 98), E( 98) / 'HXCDF_123478    ', 3.8E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME( 99), A( 99), E( 99) / 'HXCDF_123678    ', 9.0E+01, 2.9E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(100), A(100), E(100) / 'HXCDF_234678    ', 1.0E+02, 2.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(101), A(101), E(101) / 'HXCDF_123789    ', 5.6E+01, 2.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(102), A(102), E(102) / 'HPCDF_1234678   ', 2.8E+01, 1.6E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(103), A(103), E(103) / 'HPCDF_1234789   ', 8.0E+01, 2.1E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(104), A(104), E(104) / 'OTCDF           ', 7.6E+01, 2.4E+03 /  ! Paasivirta et al. (1999)
      DATA SUBNAME(105), A(105), E(105) / 'NAPHTHOL        ', 3.60E+03, 0.0E+00 / ! Eabraham et al. (1994)
      DATA SUBNAME(106), A(106), E(106) / '1NITRONAPHTHALEN', 5.68E+02, 0.0E+00 / ! Altschuh et al. (1999)
      DATA SUBNAME(107), A(107), E(107) / '2NITRONAPHTHALEN', 6.42E+02, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(108), A(108), E(108) / '14NAPHTHOQUINONE', 5.08E+05, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(109), A(109), E(109) / '2,4-TOLUENE_DIIS', 7.25E+00, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(110), A(110), E(110) / 'HEXAMETHYLE_DIIS', 2.08E+01, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(111), A(111), E(111) / 'HYDRAZINE       ', 1.14E+03, 0.0E+00 / ! Daubert and Danner (1989), and Amoore and Hautala (1983)
      DATA SUBNAME(112), A(112), E(112) / 'MALEIC_ANHYDRIDE', 2.54E+02, 0.0E+00 / ! HENRYWIN v3.10 (Meylan and Howard, 1991)
      DATA SUBNAME(113), A(113), E(113) / 'TRIETHYLAMINE   ', 6.71E+00, 0.0E+00 / ! Yalkowsky and Dannenfelser (1992), and Riddick et al. (1986)
      DATA SUBNAME(114), A(114), E(114) / 'P_DICHLOROBENZEN', 2.38E+00, 0.0E+00 / ! MacKay and Shiu (1981), measured
      DATA SUBNAME(115), A(115), E(115) / 'M-XYLENE        ', 1.43E-01, 3.9E+03 / ! Staudinger and Roberts (2001)
      DATA SUBNAME(116), A(116), E(116) / 'P-XYLENE        ', 1.35E-01, 3.7E+03 / ! Staudinger and Roberts (2001)

      DATA B( LSO2   ), D( LSO2   ) / 1.30E-02,  1.96E+03 /  ! SO2*H2O<=>HSO3+H     : Smith and Martell (1976)
      DATA B( LHSO3  ), D( LHSO3  ) / 6.60E-08,  1.50E+03 /  ! HSO3<=>SO3+H         : Smith and Martell (1976)
      DATA B( LHNO2  ), D( LHNO2  ) / 5.10E-04, -1.26E+03 /  ! HNO2(aq)<=>NO2+H     : Schwartz and White (1981)
      DATA B( LHNO3  ), D( LHNO3  ) / 1.54E+01,  8.70E+03 /  ! HNO3(aq)<=>NO3+H     : Schwartz (1984)
      DATA B( LCO2   ), D( LCO2   ) / 4.30E-07, -1.00E+03 /  ! CO2*H2O<=>HCO3+H     : Smith and Martell (1976)
      DATA B( LHCO3  ), D( LHCO3  ) / 4.68E-11, -1.76E+03 /  ! HCO3<=>CO3+H         : Smith and Martell (1976)
      DATA B( LH2O2  ), D( LH2O2  ) / 2.20E-12, -3.73E+03 /  ! H2O2(aq)<=>HO2+H     : Smith and Martell (1976)
      DATA B( LHCHO  ), D( LHCHO  ) / 2.53E+03,  4.02E+03 /  ! HCHO(aq)<=>H2C(OH)2  : Le Hanaf (1968)
      DATA B( LHCOOH ), D( LHCOOH ) / 1.80E-04, -2.00E+01 /  ! HCOOH(aq)<=>HCOO+H   : Martell and Smith (1977)
      DATA B( LHO2   ), D( LHO2   ) / 3.50E-05,  0.00E+00 /  ! HO2(aq)<=>H+O2       : Perrin (1982)
      DATA B( LNH4OH ), D( LNH4OH ) / 1.70E-05, -4.50E+02 /  ! NH4*OH<=>NH4+OH      : Smith and Martell (1976)
      DATA B( LH2O   ), D( LH2O   ) / 1.00E-14, -6.71E+03 /  ! H2O<=>H+OH           : Smith and Martell (1976)
      DATA B( LATRA  ), D( LATRA  ) / 2.09E-02,  0.00E+00 /  ! C8H14ClN5<=>C8H13ClN5+H  : Weber (1970)
      DATA B( LCL2   ), D( LCL2   ) / 5.01E-04,  0.00E+00 /  ! CL2*H2O <=> HOCL + H + CL : LIN AND PEHKONEN, JGR, 103, D21, 28093-28102, NOVEMBER 20, 1998. ALSO SEE NOTE BELOW
      DATA B( LHOCL  ), D( LHOCL  ) / 3.16E-08,  0.00E+00 /  ! HOCL <=>H + OCL      : LIN AND PEHKONEN, JGR, 103, D21, 28093-28102, NOVEMBER 20, 1998
      DATA B( LHCL   ), D( LHCL   ) / 1.74E+06,  6.90E+03 /  ! HCL <=> H + CL       : Marsh and McElroy (1985)
      DATA B( LHYDRAZINE), D( LHYDRAZINE) / 1.11E-08,  0.00E+00 /  ! HYDRAZINE <=> HYDRAZINE+ + OH-  : Moliner and Street (1989)
!-------------------------------------------------------------------------------
! Note for dissociation constant for equation 14: CL2*H2O <=> HOCL + H + CL
! Need aqueous [CL-] concentration to calculate effective henry's law coefficient
! Used a value of 2.0 mM following Lin and Pehkonen, JGR, 103, D21, 28093-28102, November 20, 1998
!-------------------------------------------------------------------------------

!...........EXTERNAL FUNCTIONS and their descriptions:

!      INTEGER, EXTERNAL :: INDEX1   ! array position for string matching
!      INTEGER, EXTERNAL :: TRIMLEN  ! string length, excl. trailing blanks

!-----------------------------------------------------------------------
!  begin body of subroutine HLCONST

      SPC = INDEX1( CNAME, MXSPCS, SUBNAME )

!...error if species not found in table

      IF ( SPC .LE. 0 ) THEN
        XMSG = CNAME( 1:TRIMLEN( CNAME ) ) // ' not found in Henry''s Law Constant table, aborting.'
!        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
        write(0,*) ''
        write(0,*) PNAME,' : ',XMSG
        stop
      END IF

!...compute the Henry's Law Constant

      TFAC = ( 298.0 - TEMP) / ( 298.0 * TEMP )
      KH = A( SPC ) * EXP( E( SPC ) * TFAC )
      HLCONST = KH

!...compute the effective Henry's law constants

      IF ( EFFECTIVE ) THEN

        IF ( HPLUS .LE. 0.0 ) THEN
          XMSG = 'Negative or Zero [H+] concentration specified, aborting.'
!        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
          write(0,*) ''
          write(0,*) PNAME,' : ',XMSG
          stop
        END IF

        HPLUSI = 1.0 / HPLUS
        HPLUS2I = HPLUSI * HPLUSI

!...assign a value for clminus.  use 2.0 mM based on Lin and Pehkonene, 1998, JGR

        CLMINUS   = 2.0E-03                ! chlorine ion conc [CL-]
        CLMINUSI  = 1.0 / CLMINUS          ! 1 / CLMINUS

        CHECK_NAME: SELECT CASE ( CNAME( 1:TRIMLEN( CNAME ) ) )

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

        CASE ('HYDRAZINE')      ! HYDRAZINE <=> HYDRAZINE+ + OH-

          AKEQ1 = B( LHYDRAZINE ) * EXP( D( LHYDRAZINE ) * TFAC )
          AKEQ2 = B( LH2O ) * EXP( D( LH2O ) * TFAC )
          OHION = AKEQ2 * HPLUSI
          HLCONST = KH * ( 1.0 + AKEQ1 / OHION )

        CASE ('ATRA', 'DATRA')  !     ATRA(aq)  <=>  ATRA- + H
                                !  or DATRA(aq) <=> DATRA- + H

          AKEQ1   = B( LATRA ) * EXP( D( LATRA ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'CL2' )          ! CL2*H2O <=> HOCL + H + CL
                                ! HOCL <=>H + OCL

          AKEQ1   = B( LCL2 )  * EXP( D( LCL2 ) * TFAC )
          AKEQ2   = B( LHOCL ) * EXP( D( LHOCL ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI * CLMINUSI &
                  + AKEQ1 * AKEQ2 * HPLUS2I * CLMINUSI )

        CASE ( 'HCL' )          ! HCL <=> H+ + CL-

          AKEQ1   = B( LHCL ) * EXP( D( LHCL ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        CASE ( 'HOCL' )         ! HOCL <=> H+ + OCL-

          AKEQ1   = B( LHOCL ) * EXP( D( LHOCL ) * TFAC )
          HLCONST = KH * ( 1.0 + AKEQ1 * HPLUSI )

        END SELECT CHECK_NAME

      END IF

      RETURN
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
