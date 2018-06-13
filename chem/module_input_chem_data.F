!dis
!dis    Open Source License/Disclaimer, Forecast Systems Laboratory
!dis    NOAA/OAR/FSL, 325 Broadway Boulder, CO 80305
!dis    
!dis    This software is distributed under the Open Source Definition,
!dis    which may be found at http://www.opensource.org/osd.html.
!dis    
!dis    In particular, redistribution and use in source and binary forms,
!dis    with or without modification, are permitted provided that the
!dis    following conditions are met:
!dis    
!dis    - Redistributions of source code must retain this notice, this
!dis    list of conditions and the following disclaimer.
!dis    
!dis    - Redistributions in binary form must provide access to this
!dis    notice, this list of conditions and the following disclaimer, and
!dis    the underlying source code.
!dis    
!dis    - All modifications to this software must be clearly documented,
!dis    and are solely the responsibility of the agent making the
!dis    modifications.
!dis    
!dis    - If significant modifications or enhancements are made to this
!dis    software, the FSL Software Policy Manager
!dis    (softwaremgr@fsl.noaa.gov) should be notified.
!dis    
!dis    THIS SOFTWARE AND ITS DOCUMENTATION ARE IN THE PUBLIC DOMAIN
!dis    AND ARE FURNISHED "AS IS."  THE AUTHORS, THE UNITED STATES
!dis    GOVERNMENT, ITS INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND
!dis    AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE USEFULNESS
!dis    OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE.  THEY ASSUME
!dis    NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND
!dis    DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.
!dis   
!dis 

!WRF:PACKAGE:IO


!CPP directives to control ic/bc conditions...
!(The directive in module_mosaic_initmixrats also needs to be set.)
!  CASENAME = 0   Uses Texas August 2004 case values and profiles
!             1   Uses same concentrations as TX, but uses different
!                 profiles depending on the species. (NEAQS2004 case)
#define CASENAME 4


MODULE module_input_chem_data

   USE module_io_domain
   USE module_domain
   USE module_data_sorgam, ONLY : conmin, rgasuniv, epsilc, grav
   USE module_get_file_names, ONLY : eligible_file_name, number_of_eligible_files, unix_ls
   USE module_model_constants

   IMPLICIT NONE

!  REAL :: grav = 9.8104
   REAL, PARAMETER :: mwso4 = 96.0576

! Variables for adaptive time steps...
      TYPE(WRFU_Time), DIMENSION(max_domains) :: last_chem_time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initial atmospheric chemistry profile data
    INTEGER :: k_loop          ! Used for loop index
    INTEGER :: lo              ! number of chemicals in inital profile
    INTEGER :: logg            ! number of final chemical species (nch-1)
    INTEGER :: kx              ! number of vertical levels in temp profile        
    INTEGER :: kxm1

    PARAMETER( kx=16, kxm1=kx-1, logg=350, lo=34) ! DL (6/2/2013) Changed value of logg from 200 to 350 for additional gas species
   
    INTEGER, DIMENSION(logg)                     :: iref

    REAL, DIMENSION(logg)                        :: fracref
    REAL, DIMENSION(kx)                          :: dens
    REAL, DIMENSION(kx+1)                        :: zfa
    REAL, DIMENSION(kx+1)                        :: zfa_bdy
    REAL, DIMENSION(lo  ,kx)                     :: xl
    REAL                                         :: so4vaptoaer
    DATA so4vaptoaer/.999/

    CHARACTER (LEN=20), DIMENSION(logg) :: ggnam !BSINGH(12/04/13): changed length(LEN) from 4 to 20

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  The idealized profile is converted from the NALROM chemistry model.
!
! The variable "iref" is the reference index 
!    "fracref" is the reference fraction correpsonding to iref
!
!  For example: WRF-Chem species number 1 for RADM is SO2.  iref(1)
!  is 12, and XL(12,K) is the profile for SO2.
!
!  Note: NALROM calculates lumped OX (XL(1) =O3+NO2+HNO3+...) and a
!  lumped NOX (XL(2)=NO+NO2+NO3+2N2O5+HO2NO2+HONO).  But XL(33) is
!  strictly O3, and XL(34)=NOx=(NO+NO2 only).
!
!  Short-lived species are initialized to steady-state equilibrium - 
!  since they are short-lived.  The short-lived species within a lumped category 
!  (Ox , NOx, or NO3+N2O5 in our case) would be renormalized to the lumped class 
!  after the steady-state equilibrium concentrations are determined.
!
!  The following is a list of long-lived species 
! 
!      NAMEL( 1)='OX        '
!      NAMEL( 2)='NOX       '
!      NAMEL( 3)='HNO3      '
!      NAMEL( 4)='H2O2      '
!      NAMEL( 5)='CH3OOH    '
!      NAMEL( 6)='CO        '
!      NAMEL( 7)='ISOPRENE  '
!      NAMEL( 8)='CH2O      '
!      NAMEL( 9)='CH3CHO    '
!      NAMEL(10)='PAN       '
!      NAMEL(11)='OTHER ALKA'
!      NAMEL(12)='SO2       '
!      NAMEL(13)='BUTANE    '
!      NAMEL(14)='ETHENE    '
!      NAMEL(15)='PROPENE+   '
!      NAMEL(16)='PPN       '
!      NAMEL(17)='MEK       '
!      NAMEL(18)='RCHO      '
!      NAMEL(19)='SO4=      '
!      NAMEL(20)='MVK       '
!      NAMEL(21)='MACR      '
!      NAMEL(22)='HAC       '
!      NAMEL(23)='MGLY      '
!      NAMEL(24)='HPAN      '
!      NAMEL(25)='MPAN      '
!      NAMEL(26)='PROPANE   '
!      NAMEL(27)='ACETYLENE '
!      NAMEL(28)='OH        '
!      NAMEL(29)='HO2       '
!      NAMEL(30)='NO3+N2O5  '
!      NAMEL(31)='HO2NO2    '
!      NAMEL(32)='SUM RO2   '
!      NAMEL(33)='OZONE     '
!      NAMEL(34)='NOX       ' 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

     DATA dens/ 2.738E+18, 5.220E+18, 7.427E+18, 9.202E+18, &
                1.109E+19, 1.313E+19, 1.525E+19, 1.736E+19, &
                1.926E+19, 2.074E+19, 2.188E+19, 2.279E+19, &
                2.342E+19, 2.384E+19, 2.414E+19, 2.434E+19  /

!     Profile heights in meters
!     DATA ZFA/    0.,   85.,  212.,  385.,  603.,  960., 1430., 2010., &
!               2850., 4010., 5340., 6900., 8510.,10200.,12100.,16000., &
!              21000./
      DATA ZFA_BDY/    0.,   85.,  212.,  385.,  603.,  960., 1430., 2010., &
                2850., 4010., 5340., 6900., 8510.,10200.,12100.,16000., &
               21000./

!     Profile heights in meters
      DATA ZFA/    0.,   85.,  212.,  385.,  603.,  960., 1430., 2010., &
                2850., 4010., 5340., 6900., 8510.,10200.,12100.,16000., &
               21000./

!wig: To match the xl profile to the correct species, match WRF's p_<species>
!     flag with iref(p_<species>-1) to get the value of the first index in xl,
!     e.g. p_o3=6, iref(6-1)=1, so xl(1,:) is the ozone profile.
!     See gasprofile_init_pnnl for an explination of what height
!     each index represents.
      DATA (xl(1,k_loop),k_loop=1,kx) &
      / 1.68E-07, 1.68E-07, 5.79E-08, 5.24E-08, 5.26E-08, &
       5.16E-08, 4.83E-08, 4.50E-08, 4.16E-08, 3.80E-08, 3.56E-08, &
       3.35E-08, 3.15E-08, 3.08E-08, 3.06E-08, 3.00E-08/

      DATA (xl(2,k_loop),k_loop=1,kx) &
      / 4.06E-10, 4.06E-10, 2.16E-10, 1.37E-10, 9.47E-11, &
       6.95E-11, 5.31E-11, 4.19E-11, 3.46E-11, 3.01E-11, 2.71E-11, &
       2.50E-11, 2.35E-11, 2.26E-11, 2.20E-11, 2.16E-11/  

      DATA (xl(3,k_loop),k_loop=1,kx) &
      / 9.84E-10, 9.84E-10, 5.66E-10, 4.24E-10, 3.26E-10, &
       2.06E-10, 1.12E-10, 7.33E-11, 7.03E-11, 7.52E-11, 7.96E-11, &
       7.56E-11, 7.27E-11, 7.07E-11, 7.00E-11, 7.00E-11/

      DATA (xl(4,k_loop),k_loop=1,kx) &
      / 8.15E-10, 8.15E-10, 8.15E-10, 8.15E-10, 8.15E-10, &
       8.65E-10, 1.07E-09, 1.35E-09, 1.47E-09, 1.47E-09, 1.47E-09, &
       1.47E-09, 1.45E-09, 1.43E-09, 1.40E-09, 1.38E-09/

      DATA (xl(5,k_loop),k_loop=1,kx) &
      / 4.16E-10, 4.16E-10, 4.16E-10, 4.16E-10, 4.16E-10, &
       4.46E-10, 5.57E-10, 1.11E-09, 1.63E-09, 1.63E-09, 1.63E-09, &
       1.63E-09, 1.61E-09, 1.59E-09, 1.57E-09, 1.54E-09/

!  CO is 70 ppbv at top, 80 throughout troposphere
      DATA (xl(6,k_loop),k_loop=1,kx)  / 7.00E-08, kxm1*8.00E-08/

      DATA (xl(7,k_loop),k_loop=1,kx) &
      / 8.33E-29, 8.33E-29, 8.33E-29, 8.33E-29, 8.33E-29, &
       1.33E-28, 3.54E-28, 1.85E-28, 1.29E-29, 1.03E-30, 1.72E-31, &
       7.56E-32, 1.22E-31, 2.14E-31, 2.76E-31, 2.88E-31/

      DATA (xl(8,k_loop),k_loop=1,kx) &
      / 9.17E-11, 9.17E-11, 9.17E-11, 9.17E-11, 9.17E-11, &
       1.03E-10, 1.55E-10, 2.68E-10, 4.47E-10, 4.59E-10, 4.72E-10, &
       4.91E-10, 5.05E-10, 5.13E-10, 5.14E-10, 5.11E-10/
      DATA (xl(9,k_loop),k_loop=1,kx) &
      / 7.10E-12, 7.10E-12, 7.10E-12, 7.10E-12, 7.10E-12, &
       7.36E-12, 1.02E-11, 2.03E-11, 2.98E-11, 3.01E-11, 3.05E-11, &
       3.08E-11, 3.08E-11, 3.06E-11, 3.03E-11, 2.99E-11/
      DATA (xl(10,k_loop),k_loop=1,kx) &
      / 4.00E-11, 4.00E-11, 4.00E-11, 3.27E-11, 2.51E-11, &
       2.61E-11, 2.20E-11, 1.69E-11, 1.60E-11, 1.47E-11, 1.37E-11, &
       1.30E-11, 1.24E-11, 1.20E-11, 1.18E-11, 1.17E-11/
      DATA (xl(11,k_loop),k_loop=1,kx) &
      / 1.15E-16, 1.15E-16, 2.46E-15, 2.30E-14, 1.38E-13, &
       6.25E-13, 2.31E-12, 7.32E-12, 1.87E-11, 3.68E-11, 6.10E-11, &
       9.05E-11, 1.22E-10, 1.50E-10, 1.70E-10, 1.85E-10/
      DATA (xl(12,k_loop),k_loop=1,kx) &
      / 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, &
       1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, &
       1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10/
      DATA (xl(13,k_loop),k_loop=1,kx) &
      / 1.26E-11, 1.26E-11, 2.02E-11, 2.50E-11, 3.02E-11, &
       4.28E-11, 6.62E-11, 1.08E-10, 1.54E-10, 2.15E-10, 2.67E-10, &
       3.24E-10, 3.67E-10, 3.97E-10, 4.16E-10, 4.31E-10/
      DATA (xl(14,k_loop),k_loop=1,kx) &
      / 1.15E-16, 1.15E-16, 2.46E-15, 2.30E-14, 1.38E-13, &
       6.25E-13, 2.31E-12, 7.32E-12, 1.87E-11, 3.68E-11, 6.10E-11, &
       9.05E-11, 1.22E-10, 1.50E-10, 1.70E-10, 1.85E-10/
      DATA (xl(15,k_loop),k_loop=1,kx) &
      / 1.00E-20, 1.00E-20, 6.18E-20, 4.18E-18, 1.23E-16, &
       2.13E-15, 2.50E-14, 2.21E-13, 1.30E-12, 4.66E-12, 1.21E-11, &
       2.54E-11, 4.47E-11, 6.63E-11, 8.37E-11, 9.76E-11/
      DATA (xl(16,k_loop),k_loop=1,kx) &
      / 1.23E-11, 1.23E-11, 1.23E-11, 1.23E-11, 1.23E-11, &
       1.20E-11, 9.43E-12, 3.97E-12, 1.19E-12, 1.11E-12, 9.93E-13, &
       8.66E-13, 7.78E-13, 7.26E-13, 7.04E-13, 6.88E-13/
      DATA (xl(17,k_loop),k_loop=1,kx) &
      / 1.43E-12, 1.43E-12, 1.43E-12, 1.43E-12, 1.43E-12, &
       1.50E-12, 2.64E-12, 8.90E-12, 1.29E-11, 1.30E-11, 1.32E-11, &
       1.32E-11, 1.31E-11, 1.30E-11, 1.29E-11, 1.27E-11/
      DATA (xl(18,k_loop),k_loop=1,kx) &
       / 3.61E-13, 3.61E-13, 3.61E-13, 3.61E-13, 3.61E-13, &
       3.58E-13, 5.22E-13, 1.75E-12, 2.59E-12, 2.62E-12, 2.64E-12, &
       2.66E-12, 2.65E-12, 2.62E-12, 2.60E-12, 2.57E-12/
      DATA (xl(19,k_loop),k_loop=1,kx) &
       / 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, &
       5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, &
       5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11/

      DATA (xl(20,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(21,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(22,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(23,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(24,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(25,k_loop),k_loop=1,kx)/kx*1.E-20/

! Propane - Gregory PEM-West A 25 ppt median marine boundary layer
      DATA (xl(26,k_loop),k_loop=1,kx) &
      /5.00E-13, 1.24E-12, 2.21E-12, 3.27E-12, 4.71E-12, &
       6.64E-12, 9.06E-12, 1.19E-11, 1.47E-11, 1.72E-11, &
       1.93E-11, 2.11E-11, 2.24E-11, 2.34E-11, 2.42E-11, 2.48E-11/
! Acetylene - Gregory PEM-West A 53 ppt median marine boundary layer
      DATA (xl(27,k_loop),k_loop=1,kx) &
      /1.00E-12, 2.48E-12, 4.42E-12, 6.53E-12, 9.42E-12, &
       1.33E-11, 1.81E-11, 2.37E-11, 2.95E-11, 3.44E-11, &
       3.85E-11, 4.22E-11, 4.49E-11, 4.69E-11, 4.84E-11, 4.95E-11/
! OH
      DATA (xl(28,k_loop),k_loop=1,kx) &
       / 9.80E+06, 9.80E+06, 4.89E+06, 2.42E+06, 1.37E+06, &
       9.18E+05, 7.29E+05, 6.26E+05, 5.01E+05, 4.33E+05, 4.05E+05, &
       3.27E+05, 2.54E+05, 2.03E+05, 1.74E+05, 1.52E+05/
! HO2
      DATA (xl(29,k_loop),k_loop=1,kx) &
       / 5.74E+07, 5.74E+07, 7.42E+07, 8.38E+07, 8.87E+07, &
       9.76E+07, 1.15E+08, 1.34E+08, 1.46E+08, 1.44E+08, 1.40E+08, &
       1.36E+08, 1.31E+08, 1.28E+08, 1.26E+08, 1.26E+08/
! NO3+N2O5
      DATA (xl(30,k_loop),k_loop=1,kx) &
       / 5.52E+05, 5.52E+05, 3.04E+05, 2.68E+05, 2.32E+05, &
       1.66E+05, 1.57E+05, 1.72E+05, 1.98E+05, 2.22E+05, 2.43E+05, &
       2.75E+05, 3.00E+05, 3.18E+05, 3.32E+05, 3.39E+05/
! HO2NO2
      DATA (xl(31,k_loop),k_loop=1,kx) &
       / 7.25E+07, 7.25E+07, 6.36E+07, 5.55E+07, 4.94E+07, &
       3.66E+07, 2.01E+07, 9.57E+06, 4.75E+06, 2.37E+06, 1.62E+06, &
       9.86E+05, 7.05E+05, 5.63E+05, 4.86E+05, 4.41E+05/
! Sum of RO2 &
      DATA (xl(32,k_loop),k_loop=1,kx) &
       / 9.14E+06, 9.14E+06, 1.46E+07, 2.14E+07, 2.76E+07, &
       3.62E+07, 5.47E+07, 1.19E+08, 2.05E+08, 2.25E+08, 2.39E+08, &
       2.58E+08, 2.82E+08, 2.99E+08, 3.08E+08, 3.15E+08/
! O3 <--This is not the O3 used for RADM2 or CBMZ (wig)
      DATA (xl(33,k_loop),k_loop=1,kx) &
       / 8.36E+11, 8.36E+11, 4.26E+11, 4.96E+11, 6.05E+11, &
       6.93E+11, 7.40E+11, 7.74E+11, 7.82E+11, 7.75E+11, 7.69E+11, &
       7.59E+11, 7.54E+11, 7.50E+11, 7.47E+11, 7.45E+11/
! NOx (NO+NO2)
      DATA (xl(34,k_loop),k_loop=1,kx) &
       / 1.94E+09, 1.94E+09, 1.53E+09, 1.24E+09, 1.04E+09, &
       8.96E+08, 7.94E+08, 7.11E+08, 6.44E+08, 6.00E+08, 5.70E+08, &
       5.49E+08, 5.35E+08, 5.28E+08, 5.24E+08, 5.23E+08/

      type lbc_concentration
        real, pointer :: ch4_lbc(:,:)
        real, pointer :: n2o_lbc(:,:)
        real, pointer :: h2_lbc(:,:)
        logical       :: is_allocated
      end type lbc_concentration

      type(lbc_concentration), allocatable :: fixed_lbc(:)

      !PV to O3 conversion parameterization lookup values
      INTEGER, PARAMETER :: levs_pv2o3=4
      REAL, DIMENSION(1:levs_pv2o3) :: pref_pv2o3 = (/5856.,7640.,9562.,50000./)
      REAL, DIMENSION(6) :: pv2o3_lv1 = (/203.53,-13.622,0.54157,-9.4264E-3,7.299E-5,-2.0214E-7/)
      REAL, DIMENSION(6) :: pv2o3_lv2 = (/151.22,-15.762,0.816918,-.0180488,1.8418E-4,-7.1408E-7/)
      REAL, DIMENSION(6) :: pv2o3_lv3 = (/62.217,-1.4435,0.030439,0.00058,-1.541E-5,8.2912E-8/)
      REAL, DIMENSION(6) :: pv2o3_lv4 = (/62.217,-1.4435,0.030439,0.00058,-1.541E-5,8.2912E-8/)
      ! Upper limit on interpolation

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sets up the cross reference mapping indices and fractional
! apportionment of the default species profiles for use with
! ICs and BCs.
! William.Gustafson@pnl.gov; 2-May-2007
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE setup_gasprofile_maps(chem_opt, numgas)
  integer, intent(in) :: chem_opt, numgas

!!! TUCCELLA
  select case(chem_opt)
  case (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, &
        RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP,       &
        RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACMSORG_KPP, RACM_SOA_VBS_KPP,&
        RACM_SOA_VBS_AQCHEM_KPP,GOCARTRACM_KPP, GOCARTRADM2, CHEM_TRACER, CHEM_TRACE2, &
        RACM_SOA_VBS_HET_KPP)
     call setup_gasprofile_map_radm_racm

  case (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP,      &
     SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP, SAPRC99_MOSAIC_8BIN_VBS2_KPP)!BSINGH(12/04/2013): Added SAPRC 8 bin and non-aq on 04/03/2014)
     call setup_gasprofile_map_saprcnov



  case (CBMZ, CBMZ_BB, CBMZ_BB_KPP, &
        CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_4BIN,  &
        CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ,CBMZSORG, &
        CBMZSORG_AQ, CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, &
        CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
		CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)
!    This call works for CBMZ_BB_KPP and CBMZ_SORGAM_KPP if all additional
!    gas species such as ch4, api, lim etc. are put after ho2 in the
!    package definitions in registry.chem. This is important to not mess
!    up the mapping of the first 33 species, and also make sure the last
!    gas is isopo2. This is important for aerosol driver routines. Right
!    now I put all additional species just before isopo2 in cbmz_mosaic_kpp
!    Manish Shrivastava 12/8/2009
     call setup_gasprofile_map_cbmz(numgas)

  case (CBM4_KPP)
     call setup_gasprofile_map_cbm4(numgas)
  case (CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
     call wrf_debug("setup_profile_maps: nothing done for cb05")
  case (GOCART_SIMPLE)
     call wrf_debug("setup_profile_maps: nothing done for gocart simple")
  case (CHEM_VASH)
     call wrf_debug("setup_profile_maps: nothing done for volcanic ash")
  case (CHEM_VOLC)
     call wrf_debug("setup_profile_maps: nothing done for volcanic ash")
  case (CHEM_VOLC_4BIN)
     call wrf_debug("setup_profile_maps: nothing done for volcanic ash")
  case (DUST)
     call wrf_debug("setup_profile_maps: nothing done for volcanic ash")
  case (MOZART_KPP)
     call wrf_debug("setup_profile_maps: nothing done for mozart_kpp")

  case (CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
      call wrf_debug("setup_profile_maps: nothing done for crimech")

  case (MOZCART_KPP)
     call wrf_debug("setup_profile_maps: nothing done for mozcart_kpp")

  case (T1_MOZCART_KPP)
     call wrf_debug("setup_profile_maps: nothing done for t1_mozcart_kpp")

  case (MOZART_MOSAIC_4BIN_KPP)
     call wrf_debug("setup_profile_maps: nothing done for mozart_mosaic_4bin_kpp")

  case (MOZART_MOSAIC_4BIN_AQ_KPP)
     call wrf_debug("setup_profile_maps: nothing done for mozart_mosaic_4bin_aq_kpp")

 case (CO2_TRACER,GHG_TRACER)
     call wrf_debug("setup_profile_maps: nothing done for the GHG options")

  case default
     call wrf_error_fatal("setup_profile_maps: could not decipher chem_opt value")

  end select

END SUBROUTINE setup_gasprofile_maps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Maps for indices and fractional apportionment of the default
! species profiles to the RADM2 and RACM species for ICs and BCs.
! William.Gustafson@pnl.gov; 2-May-2007
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE setup_gasprofile_map_radm_racm
  
  iref(:)    = 7 !default value
  iref(1:41) = (/12,19,2,2,1,3,4,9,8,5,5,32,6,6,6,30,30,10,26,13,11,6,6, &
                 14,15,15,23,23,32,16,23,31,17,23,23,23,23,23,7,28,29/)

  fracref(:)    = 1. !default value
  fracref(1:41) = (/1.,1.,.75,.25,1.,1.,1.,1.,1.,1., &
                    .5,.5,6.25E-4,7.5E-4,6.25E-5,.1, &
                    .9,1.,1.,1.,1.,8.E-3,1.,1.,1.,.5,&
                    1.,1.,.5,1.,1.,1.,1.,1.,1.,1.,1.,&
                    1.,1.,1.,1./)

  ggnam(:) = 'JUNK' !default value
  ggnam(1:41) = (/ 'SO2 ','SULF','NO2 ','NO  ','O3  ','HNO3',    &
                   'H2O2','ALD ','HCHO','OP1 ','OP2 ','PAA ',    &
                   'ORA1','ORA2','NH3 ','N2O5','NO3 ','PAN ',    &
                   'HC3 ','HC5 ','HC8 ','ETH ','CO  ','OL2 ',    &
                   'OLT ','OLI ','TOL ','XYL ','ACO3','TPAN',    &
                   'HONO','HNO4','KET ','GLY ','MGLY','DCB ',    &
                   'ONIT','CSL ','ISO ','HO  ','HO2 '           /)

END SUBROUTINE setup_gasprofile_map_radm_racm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Maps for indices and fractional apportionment of the default
! species profiles to the SAPRC99 species for ICs and BCs.
! Code build atomatically using write_BD_and_IC_code.m script and
! BD_and_IC_NALROM_to_WRF-SAPRC.txt file
! pablo-saide@uiowa.edu; 25-Aug-2009
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE setup_gasprofile_map_saprcnov
  iref(:)    = 7 !default value
  iref(1:49) = (/1,4,2,2,30,30,23,3,31,12, &
                  19,6,8,9,18,17,6,6,6,23, &
                  23,23,21,20,14,7,26,27,15,13, &
                  11,11,23,23,6,6,23,10,16,25, &
                  32,32,32,28,29,5,5,32,32/)

  fracref(:)    = 1. !default value
  fracref(1:49) = (/1.00E+00,1.00E+00,2.50E-01,7.50E-01,9.00E-01,1.00E-01, &
                  1.00E+00,1.00E+00,1.00E+00,1.00E+00,1.00E+00,1.00E+00, &
                  1.00E+00,1.00E+00,1.00E+00,1.00E+00,6.25E-04,5.00E-04, &
                  2.50E-04,1.00E+00,1.00E+00,1.00E+00,1.00E+00,1.00E+00, &
                  1.00E+00,1.00E+00,1.00E+00,1.00E+00,1.00E+00,1.00E+00, &
                  5.00E-01,5.00E-01,1.00E+00,1.00E+00,8.00E-03,8.00E-03, &
                  1.00E+00,1.00E+00,1.00E+00,1.00E+00,3.00E-01,2.00E-01, &
                  2.00E-01,1.00E+00,1.00E+00,1.00E+00,5.00E-01,1.00E+00, &
                  3.00E-01/)

  ggnam(:) = 'JUNK' !default value
  ggnam(1:49) = (/'O3  ','H2O2','NO  ','NO2 ','NO3 ','N2O5', &
                  'HONO','HNO3','HNO4','SO2 ','H2SO','CO  ', & !H2SO really is H2SO4
                  'HCHO','CCHO','RCHO','MEK ','HCOH','CCOH', & !HCOH=HCOOH, CCOH=CCO_OH
                  'RCOH','GLY ','MGLY','CRES','MEAC','MVK ', & !RCOH=RCO_OH, MEAC=METHACRO
                  'ETHE','ISPR','C3H8','C2H2','C3H6','ALK3', & !ETHE=ETHENE, ISPR=ISOPRENE
                  'ALK4','ALK5','ARO1','ARO2','OLE1','OLE2', &
                  'RNO3','PAN ','PAN2','MAPN','COOH','RCO2', & !MAPN=MA_PAN, COOH=CCO_OOH, RCO2=RCO_O2
                  'ROOH','OH  ','HO2 ','COOH','ROOH','RO2R', & !ROOH=RCO_OOH, RO2R=RO2_R
                  'RO2R'/)                                     !RO2R=RO2_R

END SUBROUTINE setup_gasprofile_map_saprcnov


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Maps for indices and fractional apportionment of the default
! species profiles to the CBMZ species for ICs and BCs. The profiles
! for HC3, HC5, and HC8 used for RADM are tacked onto the end of the
! CBMZ list in order to construct the PAR species.
! William.Gustafson@pnl.gov; 2-May-2007
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE setup_gasprofile_map_cbmz(numgas)
  integer, intent(in) :: numgas
  integer, parameter :: listlast = 33
  iref(:)    = 7 !default value
  iref(1:listlast) = (/12,19, 2, 2, 1, 3, &
                        4, 9, 8, 5, 5, 6, &
                        6, 6,30,30,10, 6, &
                        6,14,15,15,23,23, &
                       23,31,17,23,23,23, &
                        7,28,29          /)

  fracref(:)    = 1. !default value
  fracref(1:listlast) = (/1.,1.,.75,.25,1.,1., &
                          1.,1.,1.,1.,.5,6.25E-4, &
                          7.5E-4,6.25E-5,.1,.9,1.,8.E-3, &
                          1.,1.,1.,.5,1.,1.,   &
                          1.,1.,1.,1.,1.,1.,   &
                          1.,1.,1.            /)

  ggnam(:) = 'JUNK' !default value - not really all junk, the
                    !last species all just goto the default
  ggnam(1:listlast) = (/ 'SO2 ','SULF','NO2 ','NO  ','O3  ','HNO3', &
                         'H2O2','ALD ','HCHO','OP1 ','OP2 ','ORA1', &
                         'ORA2','NH3 ','N2O5','NO3 ','PAN ','ETH ', &
                         'CO  ','OL2 ','OLT ','OLI ','TOL ','XYL ', &
                         'HONO','HNO4','KET ','MGLY','ONIT','CSL ', &
                         'ISO ','HO  ','HO2 '                      /)
!
! After the CBMZ species, add the RADM HC3, HC5, and HC8 to be used
! for constructing PAR..
!
  if( numgas < listlast ) &
       call wrf_error_fatal("numgas < listlast in setup_gasprofile_map_cbmz")
  iref(numgas+1:numgas+3)    = (/   26,     13,    11/)
  fracref(numgas+1:numgas+3) = (/   1.,     1.,    1./)
  ggnam(numgas+1:numgas+3)   = (/'HC3 ','HC5 ','HC8 '/)
  
END SUBROUTINE setup_gasprofile_map_cbmz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Maps for indices and fractional apportionment of the default
! species profiles to the CBM4 species for ICs and BCs. The profiles
! for HC3, HC5, and HC8 used for RADM are tacked onto the end of the
! CBM4 list in order to construct the PAR species.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE setup_gasprofile_map_cbm4(numgas)
  integer, intent(in) :: numgas
  integer, parameter :: listlast = 24
  iref(:)    = 7 !default value
  iref(1:listlast) = (/12,19, 2, 2, 1, 3, &
                        4, 9, 8,  &
                        6,30,30,10, 6, &
                        6,15,23,23, &
                        23,23,23, &
                        7,28,29          /)

  fracref(:)    = 1. !default value
  fracref(1:listlast) = (/1.,1.,.75,.25,1.,1., &
                          1.,1.,1., &
                          6.25E-5,.1,.9,1.,1., &
                          1.,7.,1.,1.,   &
                          1.,1.,1.,   &
                          1.,1.,1.            /)

  ggnam(:) = 'JUNK' !default value - not really all junk, the
                    !last species all just goto the default
  ggnam(1:listlast) = (/ 'SO2 ','SULF','NO2 ','NO  ','O3  ','HNO3', &
                         'H2O2','ALD2','HCHO', &
                         'NH3 ','N2O5','NO3 ','PAN ','ETH ', &
                         'CO  ','OLE ','TOL ','XYL ', &
                         'HONO','ONIT','CRES', &
                         'ISO ','HO  ','HO2 '                      /)
!
! After the CBM4 species, add the RADM HC3, HC5, and HC8 to be used
! for constructing PAR..
!
  if( numgas < listlast ) &
       call wrf_error_fatal("numgas < listlast in setup_gasprofile_map_cbm4")
  iref(numgas+1:numgas+7)    = (/   26,     13,    11,  17,   15,     15,   6/)
  fracref(numgas+1:numgas+7) = (/   1.,     1.,    1.,   1.,   .5,   1.,   7.5E-4/)
  ggnam(numgas+1:numgas+7)   = (/'HC3 ','HC5 ','HC8 ','KET ','OLI ','OLT ','ORA2'/)
  
END SUBROUTINE setup_gasprofile_map_cbm4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE vinterp_chem(nx1, nx2, ny1, ny2, nz1, nz_in, nz_out, nch, z_in, z_out, &
                 data_in, data_out, extrapolate)

    ! Interpolates columns of chemistry data from one set of height surfaces to
    ! another
 
    INTEGER, INTENT(IN)                :: nx1, nx2
    INTEGER, INTENT(IN)                :: ny1, ny2
    INTEGER, INTENT(IN)                :: nz1
    INTEGER, INTENT(IN)                :: nz_in
    INTEGER, INTENT(IN)                :: nz_out
    INTEGER, INTENT(IN)                :: nch
    REAL, INTENT(IN)                   :: z_in (nx1:nx2,nz1:nz_in ,ny1:ny2)
    REAL, INTENT(IN)                   :: z_out(nx1:nx2,nz1:nz_out,ny1:ny2)
    REAL, INTENT(IN)                   :: data_in (nx1:nx2,nz1:nz_in ,ny1:ny2,nch)
    REAL, INTENT(OUT)                  :: data_out(nx1:nx2,nz1:nz_out,ny1:ny2,nch)
    LOGICAL, INTENT(IN)                :: extrapolate

    INTEGER                            :: i,j,l
    INTEGER                            :: k,kk
    REAL                               :: desired_z
    REAL                               :: dvaldz
    REAL                               :: wgt0
  
!   Loop over the number of chemical species
    chem_loop: DO l = 2, nch

      data_out(:,:,:,l) = -99999.9

      DO j = ny1, ny2
        DO i = nx1, nx2

          output_loop: DO k = nz1, nz_out

            desired_z = z_out(i,k,j)
            IF (desired_z .LT. z_in(i,1,j)) THEN

              IF ((desired_z - z_in(i,1,j)).LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,1,j,l)
              ELSE
                IF (extrapolate) THEN
                  ! Extrapolate downward because desired height level is below
                  ! the lowest level in our input data.  Extrapolate using simple
                  ! 1st derivative of value with respect to height for the bottom 2
                  ! input layers.
  
                  ! Add a check to make sure we are not using the gradient of 
                  ! a very thin layer
  
                  IF ( (z_in(i,1,j) - z_in(i,2,j)) .GT. 0.001) THEN
                    dvaldz = (data_in(i,1,j,l) - data_in(i,2,j,l)) / &
                              (z_in(i,1,j)  - z_in(i,2,j) )
                  ELSE
                    dvaldz = (data_in(i,1,j,l) - data_in(i,3,j,l)) / &
                              (z_in(i,1,j)  - z_in(i,3,j) )
                  ENDIF
                  data_out(i,k,j,l) = MAX( data_in(i,1,j,l) + &
                                dvaldz * (desired_z-z_in(i,1,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,1,j,l)
                ENDIF
              ENDIF
            ELSE IF (desired_z .GT. z_in(i,nz_in,j)) THEN
              IF ( (z_in(i,nz_in,j) - desired_z) .LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,nz_in,j,l)
              ELSE
                IF (extrapolate) THEN
                  ! Extrapolate upward
                  IF ( (z_in(i,nz_in-1,j)-z_in(i,nz_in,j)) .GT. 0.0005) THEN
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-1,j,l)) / &
                               (z_in(i,nz_in,j)  - z_in(i,nz_in-1,j))
                  ELSE
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-2,j,l)) / &
                               (z_in(i,nz_in,j)  - z_in(i,nz_in-2,j)) 
                  ENDIF
                  data_out(i,k,j,l) =  MAX( data_in(i,nz_in,j,l) + &
                           dvaldz * (desired_z-z_in(i,nz_in,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,nz_in,j,l)
                ENDIF
              ENDIF
            ELSE
              ! We can trap between two levels and linearly interpolate
  
              input_loop:  DO kk = 1, nz_in-1
                IF (desired_z .EQ. z_in(i,kk,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk,j,l)
                  EXIT input_loop
                ELSE IF (desired_z .EQ. z_in(i,kk+1,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk+1,j,l)
                  EXIT input_loop
                ELSE IF ( (desired_z .GT. z_in(i,kk,j)) .AND. &
                          (desired_z .LT. z_in(i,kk+1,j)) ) THEN
                  wgt0 = (desired_z - z_in(i,kk+1,j)) / &
                         (z_in(i,kk,j)-z_in(i,kk+1,j))
                  data_out(i,k,j,l) = MAX( wgt0*data_in(i,kk,j,l) + &
                                    (1.-wgt0)*data_in(i,kk+1,j,l), 0.)
                  EXIT input_loop
                ENDIF        
              ENDDO input_loop

            ENDIF
          ENDDO output_loop
        ENDDO 
      ENDDO 
    ENDDO chem_loop

    RETURN
  END SUBROUTINE vinterp_chem
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE input_chem_profile (si_grid)

   IMPLICIT NONE

   TYPE(domain)           ::  si_grid

   INTEGER :: i , j , k, &
              ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe    
   INTEGER :: fid, ierr, numgas
   INTEGER :: debug_level

   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: si_zsigf, si_zsig


   CHARACTER (LEN=80) :: inpname, message

   write(message,'(A)') 'Subroutine input_chem_profile: '
   CALL  wrf_message ( TRIM(message) )

   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1,debug_level )
   CALL set_wrf_debug_level ( debug_level )
   
   ! Get grid dimensions
   CALL get_ijk_from_grid (  si_grid ,                        &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   ! Get scalar grid point heights
   ALLOCATE( si_zsigf(ims:ime,kms:kme,jms:jme) )
   ALLOCATE(  si_zsig(ims:ime,kms:kme,jms:jme) )

   write(message,'(A)') 'WRF_EM_CORE  '
   si_zsigf = (si_grid%ph_1 + si_grid%phb) / grav

   do k=1,kde-1
     si_zsig(:,k,:) = 0.5 * ( si_zsigf(:,k,:) + si_zsigf(:,k+1,:) ) 
   enddo
   si_zsig(:,kde,:) = 0.5 * ( 3. * si_zsigf(:,kde,:) - si_zsigf(:,kde-1,:) ) 

   ! Determine the index of the last gas species
   numgas = get_last_gas(si_grid%chem_opt)

   ! Setup the cross reference mappings between the default profiles and
   ! the gas mechanism species (wig, 2-May-2007)
   call setup_gasprofile_maps(si_grid%chem_opt, numgas)

   ! An alternative ozone profile option for initialization
   if( si_grid%gas_ic_opt == GAS_IC_PNNL ) &
        call gasprofile_init_pnnl( si_grid%chem_opt )

   ! Interpolate the chemistry data to the SI grid. These values should typically
   ! be set to match the values in bdy_chem_value_tracer so that the boundaries
   ! and interior match each other.
   IF ( si_grid%chem_opt == CHEM_TRACER ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,1:numgas) = 0.0001
!     si_grid%chem(ims:ime,kms:kme,jms:jme,p_so2) = 0.0001
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_co ) = 0.08
   ELSE IF ( si_grid%chem_opt == CHEM_TRACE2 ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_TRACER_1 ) = 0.08
   ELSE IF ( si_grid%chem_opt == CHEM_VASH ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,1:numgas ) = 0.
   ELSE IF ( si_grid%chem_opt == CHEM_VOLC ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,1:numgas ) = 0.
   ELSE IF ( si_grid%chem_opt == CHEM_VOLC_4BIN ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,1:numgas ) = 0.
   ELSE IF ( si_grid%chem_opt == DUST ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,1:numgas ) = 0.
   ELSE IF ( si_grid%chem_opt == GOCART_SIMPLE ) THEN
      si_grid%chem(ims:ime,kms:kme,jms:jme,1:num_chem) = 1.e-12
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_so2) = 1.e-6
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_sulf) = 3.e-6
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_dms) = 1.e-6
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_msa) = 1.e-6
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_bc1 ) = 1.e-2
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_bc2 ) = 1.e-2
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_oc1 ) = 1.e-2
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_oc2 ) = 1.e-2
      si_grid%chem(ims:ime,kms:kme,jms:jme,p_p25 ) = 1.
   ELSE IF ( si_grid%chem_opt==CO2_TRACER .OR. si_grid%chem_opt==GHG_TRACER ) THEN
       ! Do nothing, since for GHGs initialization is done in chemics_init.F
   ELSE
      CALL make_chem_profile (ims, ime, jms, jme, kms, kme, num_chem, numgas, &
                              si_grid%chem_opt, si_zsig, si_grid%chem)
   END IF

   CALL wrf_debug       ( 100,' input_chem_profile: exit subroutine ')

   DEALLOCATE( si_zsigf ); DEALLOCATE( si_zsig )
   RETURN

  END SUBROUTINE input_chem_profile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE make_chem_profile ( nx1, nx2, ny1, ny2, nz1, nz2, nch, numgas, &
                                 chem_opt, zgrid, chem )
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nx1, ny1, nz1
    INTEGER, INTENT(IN) :: nx2, ny2, nz2
    INTEGER, INTENT(IN) :: nch, numgas, chem_opt

    REAL, INTENT(IN), DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid

    CHARACTER (LEN=80) :: message
    INTEGER :: i, j, k, l, is

    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2,lo+1):: chprof
    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2)     :: zprof

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,nch) :: chem
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,lo ) :: stor

    REAL :: hc358 !wig, 2-May-2007
    REAL :: olit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Check the number of species

     if( nch .NE. num_chem) then
       message = ' Input_chem_profile: wrong number of chemical species'
!       CALL WRF_ERROR_FATAL ( message )
     endif
       
      ! Vertically flip the chemistry data as it is given top down and
      ! heights are bottom up. Fill temp 3D chemical and profile array,
      ! keep chem slot 1 open as vinterp_chem assumes there is no data.
      DO j=ny1,ny2
      DO k=  1,kx 
      DO i=nx1,nx2 
         chprof(i,k,j,2:lo+1) = xl(1:lo,kx-k+1)
         zprof(i,k,j) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
      ENDDO
      ENDDO
!
! return xl to previous value for next time... 
!   34 chemicals (lo), 16 vertical levels (kx)
!     DO i=lo-6,lo               
!       xl(i,1:kx)=xl(i,1:kx)*dens(1:kx)
!     ENDDO

! Change number concentrations to mixing ratios for short-lived NALROM species
      do k=1,kx
         chprof(:,k,:,lo-5:lo+1) = chprof(:,k,:,lo-5:lo+1)/dens(k)
      end do

      ! Interpolate temp 3D chemical and profile array to WRF grid
      call vinterp_chem(nx1, nx2, ny1, ny2, nz1, kx, nz2, lo, zprof, zgrid, &
                          chprof, chem, .false.)

      ! place interpolated data into temp storage array
      stor(nx1:nx2,nz1:nz2,ny1:ny2,1:lo) = chem(nx1:nx2,nz1:nz2,ny1:ny2,2:lo+1)

      ! Here is where the chemistry profile is constructed
      !chem(:,:,:,1) = stor(:,:,:,1) * 0.
      chem(nx1:nx2,nz1:nz2,ny1:ny2,1) = -999.

      DO  l=2, numgas
         is=iref(l-1)
         DO j=ny1,ny2
         DO k=nz1,nz2
         DO i=nx1,nx2
            chem(i,k,j,l)=fracref(l-1)*stor(i,k,j,is) * 1.E6
         ENDDO
         ENDDO
         ENDDO
      ENDDO

!
! For CBMZ, we need to construct PAR based on a combination of other
! species. This cannot be done with the looping construct above so
! we have to treat it separately. (wig, 2-May-2007)
!
      SELECT CASE(chem_opt)
      CASE (CBMZ,CBMZ_BB,CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, &
            CBMZ_MOSAIC_4BIN,CBMZ_MOSAIC_8BIN, &
            CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ, &
            CBMZSORG,CBMZSORG_AQ, CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, & 
            CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
            CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) =                                    &
                 0.4*chem(i,k,j,p_ald) + hc358                     &
                 + 0.9*chem(i,k,j,p_ket) + 2.8*chem(i,k,j,p_oli)   &
                 + 1.8*chem(i,k,j,p_olt) + 1.0*chem(i,k,j,p_ora2)
         end do
         end do
         end do



      CASE (CBM4_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            olit = ( 0.9*fracref(numgas+4)*stor(i,k,j,iref(numgas+4)) &
                     +2.8*fracref(numgas+5)*stor(i,k,j,iref(numgas+5)) &
                     +1.8*fracref(numgas+6)*stor(i,k,j,iref(numgas+6)) &
                     +1.0*fracref(numgas+7)*stor(i,k,j,iref(numgas+7)) &
                    )*1.E6
            chem(i,k,j,p_par) =  0.4*chem(i,k,j,p_ald2) + hc358  + olit
         end do
         end do
         end do

      CASE (CB05_SORG_AQ_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) =                                    &
                 0.4*chem(i,k,j,p_ald2)  + hc358                     &
                 +0.4*chem(i,k,j,p_aldx)  + 2.8*chem(i,k,j,p_ole)    &
                 + 1.8*chem(i,k,j,p_iole) + 1.0*chem(i,k,j,p_aacd)
         end do
         end do
         end do

      CASE (CB05_SORG_VBS_AQ_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) =                                    &
                 0.4*chem(i,k,j,p_ald2)  + hc358                     &
                 +0.4*chem(i,k,j,p_aldx)  + 2.8*chem(i,k,j,p_ole)    &
                 + 1.8*chem(i,k,j,p_iole) + 1.0*chem(i,k,j,p_aacd)
         end do
         end do
         end do

      END SELECT

      RETURN
  END SUBROUTINE make_chem_profile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE initial_pvo3 ( nx1, nx2, ny1, ny2, nz1, nz2, nch, numgas, &
                                 chem_opt, zgrid, o3, &
                                 UB, VB, TB, P, PB,  &
                                 SIGMA, XMSF, UMSF, VMSF, CORL,PSB,DX,XLAT,JULDAY, &
                                 ids,ide,jds,jde,kds,kde, &
                                 ips,ipe,jps,jpe,kps,kpe)

    USE module_configure, only: model_config_rec

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nx1, ny1, nz1
    INTEGER, INTENT(IN) :: nx2, ny2, nz2
    INTEGER, INTENT(IN) :: ips,ipe,jps,jpe,kps,kpe
    INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde
    INTEGER, INTENT(IN) :: nch, numgas, chem_opt

    REAL, INTENT(IN), DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid

    REAL, INTENT(INOUT), DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: o3

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2), INTENT(IN) :: &
                                             TB, UB, VB, P, PB
    REAL, DIMENSION(nz1:nz2), INTENT(IN)                 :: SIGMA
    REAL, DIMENSION(nx1:nx2,ny1:ny2), INTENT(IN)         :: &
                             XMSF, UMSF, VMSF, CORL, PSB , XLAT
    REAL, INTENT(IN)                                     :: DX
    INTEGER, INTENT(IN)                                  :: julday


    CHARACTER (LEN=80) :: message
    INTEGER :: i, j, k, l, is

    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2,lo+1):: chprof
    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2)     :: zprof

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,lo ) :: stor

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,nch) :: chem_local

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: PV
    REAL                                     :: preshPa, pv2o3_con
    REAL      :: day_fac  ! SAM 10/3/17 temporal fac .from Xing etal.(2016) PV-O3 relat. - depnds only on julday
    LOGICAL   :: PVBOOL


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Check the number of species

     if( nch .NE. num_chem) then
       message = ' Input_chem_profile: wrong number of chemical species'
!       CALL WRF_ERROR_FATAL ( message )
     endif
       
      ! Vertically flip the chemistry data as it is given top down and
      ! heights are bottom up. Fill temp 3D chemical and profile array,
      ! keep chem slot 1 open as vinterp_chem assumes there is no data.
      DO j=ny1,ny2
      DO k=  1,kx 
      DO i=nx1,nx2 
         chprof(i,k,j,2:lo+1) = xl(1:lo,kx-k+1)
         zprof(i,k,j) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
      ENDDO
      ENDDO
!
! return xl to previous value for next time... 
!   34 chemicals (lo), 16 vertical levels (kx)
!     DO i=lo-6,lo               
!       xl(i,1:kx)=xl(i,1:kx)*dens(1:kx)
!     ENDDO

! Change number concentrations to mixing ratios for short-lived NALROM species
      do k=1,kx
         chprof(:,k,:,lo-5:lo+1) = chprof(:,k,:,lo-5:lo+1)/dens(k)
      end do

      ! Interpolate temp 3D chemical and profile array to WRF grid
      call vinterp_chem(nx1, nx2, ny1, ny2, nz1, kx, nz2, lo, zprof, zgrid, &
                          chprof, chem_local, .false.)

      ! place interpolated data into temp storage array
      stor(nx1:nx2,nz1:nz2,ny1:ny2,1:lo) = chem_local(nx1:nx2,nz1:nz2,ny1:ny2,2:lo+1)

      ! Here is where the o3 profile is constructed
      if (model_config_rec%do_pvozone .and. p_o3 .gt. 1) THEN
        day_fac=1.+0.22*sin(0.174533*12.*(30.*(julday/365.)+2.))
         call PVS(nx1,nx2,ny1,ny2,nz1,nz2, &
                  UB,VB,TB,SIGMA,XMSF, UMSF, &
                  VMSF,CORL,PSB,DX,PV, &
                  ids,ide,jds,jde,kds,kde, &
                  ips,ipe,jps,jpe,kps,kpe)

      else
         return
      end if 

      l = p_o3
      is=iref(l-1)
      DO j=ny1,ny2
      DO k=nz1,nz2
      DO i=nx1,nx2
         PVBOOL = .false.
         ! PV2O3 extrapolation from 100 to 500mb. SAM 8/5/16, Yuyan Cui
         preshPa=p(i,k,j)+pb(i,k,j)
         if(preshPa.le.50000.) then
            CALL PV2O3_CONST(XLAT(i,j),preshPa,pv2o3_con)
            pv2o3_con=pv2o3_con*day_fac  ! 10/3/17 Add in temporal factor for O3-PV ratio
! chem_local is in (ppmv), stor is in mixing ratio by volume, pv2o3_con is in (ppbv/PVunit)
            if (stor(i,k,j,is).lt.pv2o3_con*PV(i,k,j)*1.E-9) then
               PVBOOL = .true.
            end if
         end if
         if (PVBOOL) then
            o3(i,k,j)=fracref(l-1)*pv2o3_con*PV(i,k,j)*1.E-9 * 1.E6
         else
            o3(i,k,j)=fracref(l-1)*stor(i,k,j,is) * 1.E6
         end if
      ENDDO
      ENDDO
      ENDDO

      RETURN
  END SUBROUTINE initial_pvo3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!
! this is a kludge routine as of now....
!
  SUBROUTINE bdy_chem_value_sorgam (chem, z, nch, config_flags, &
                                      alt,convfac,g)
  USE module_data_sorgam

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: z          ! 3D height array
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
    REAL,  INTENT(IN   ) ::   alt, convfac
    real, INTENT (IN) :: g
    TYPE (grid_config_rec_type), intent(in) :: config_flags

    INTEGER :: i, k, l
    REAL, DIMENSION(lo+1,1:kx):: cprof  ! chemical profile, diff. index order

    REAL, DIMENSION(1:kx):: zprof
    REAL, DIMENSION(lo ) :: stor
    REAL                 :: wgt0

    real :: chemsulf_radm,chem_so4aj,chem_so4ai
     real tempfac
      REAL :: splitfac
                        !between gas and aerosol phase

!factor for splitting initial conc. of SO4
!3rd moment i-mode [3rd moment/m^3]
      REAL :: m3nuc
!3rd MOMENT j-mode [3rd moment/m^3]
      REAL :: m3acc
!       REAL ESN36
      REAL :: m3cor
      DATA splitfac/.98/

!
! method for bc calculation is determined by aer_bc_opt
!
       if (config_flags%aer_bc_opt == AER_BC_PNNL .and. &
           config_flags%chem_opt .ne. CB05_SORG_VBS_AQ_KPP) then
           call sorgam_set_aer_bc_pnnl( chem, z, nch, config_flags )
           return
       else if (config_flags%aer_bc_opt == AER_BC_PNNL .and. &
            config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) then
           call sorgam_vbs_set_aer_bc_pnnl( chem, z, nch, config_flags )
           return
       else if (config_flags%aer_bc_opt == AER_BC_DEFAULT) then
           continue
       else
           call wrf_error_fatal(   &
               "bdy_chem_value_sorgam -- unable to parse aer_bc_opt" )
       end if

! do default calculation of sorgam aerosol bc values
       chem=conmin
!      tempfac=(t+t0)*((p+pb)/p1000mb)**rcp
!      convfac=(p+pb)/rgasuniv/tempfac
!
!--- units for advection....
!
       if(nch.eq.p_nu0)chem=1.e8*alt
       if(nch.eq.p_ac0)chem=1.e8*alt
       if(nch.eq.p_nh4aj)chem=10.e-5*alt
       if(nch.eq.p_nh4ai)chem=10.e-5*alt
       if(nch.eq.p_no3aj)chem=10.e-5*alt
       if(nch.eq.p_no3ai)chem=10.e-5*alt
!
! recalculate sulf profile for aerosols
!
     if   ( nch .eq. p_so4aj.or.nch.eq.p_so4ai                        &
        .or.nch .eq. p_nu0  .or.nch.eq.p_ac0                          &
        .or.nch .eq. p_corn                    ) then

      ! Vertically flip the chemistry data as it is given top down 
      !     and heights in zfa are bottom up
      ! Fill chemical profile array cprof
      ! Keep chem slot 1 open as vinterp_chem assumes there is no data
      !     (this isn't really needed in this subr)
      ! Convert species 28-34 (lo-6:lo) from (molecules/cm3) to (mol/mol)
      DO k = 1,kx 
        zprof(k) = 0.5*(zfa_bdy(k)+zfa_bdy(k+1))
        DO l = 1,lo-7
           cprof(l+1,k) = xl(l,kx+1-k)
        END DO
! Fix number concentrations to mixing ratios for short-lived NALROM species
        DO l = lo-6,lo
            cprof(l+1,k) = xl(l,kx+1-k)/dens(kx+1-k)
        ENDDO
      ENDDO

      ! Interpolate temp 1D chemical profile array to WRF field
      IF (z .LT. zprof(1)) THEN 
        stor(1:lo) = cprof(2:lo+1,1) 
      ELSE IF (z .GE. zprof(kx)) THEN
        stor(1:lo) = cprof(2:lo+1,kx)
      ELSE
        ! We can trap between two levels and linearly interpolate
        input_loop:  DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN 
            stor(1:lo) = cprof(2:lo+1,k)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z   - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor(1:lo) = MAX( wgt0 *cprof(2:lo+1,k  ) + &
                          (1.-wgt0)*cprof(2:lo+1,k+1), 0.)
            EXIT input_loop
          ENDIF  
        ENDDO input_loop
      ENDIF 

      ! Here is where the chemistry value is constructed
      chemsulf_radm = fracref(p_sulf-1)*stor( iref(p_sulf-1) )*1.E6
!
! now have sulf
!
       chem_so4aj=chemsulf_radm*CONVFAC*MWSO4*splitfac*so4vaptoaer
       chem_so4ai=chemsulf_radm*CONVFAC*MWSO4*(1.-splitfac)*so4vaptoaer
       if(nch.eq.p_so4aj)chem=chem_so4aj*alt
       if(nch.eq.p_so4ai)chem=chem_so4ai*alt
       m3nuc=so4fac*chem_so4ai+conmin*(nh4fac+no3fac+orgfac*9+2*anthfac)
       m3acc=so4fac*chem_so4aj+conmin*(nh4fac+no3fac+orgfac*9+2*anthfac)
       m3cor=conmin*(soilfac+seasfac+anthfac)
!
! compute values for aerosol input data
!
       if(nch.eq.p_nu0.or.nch.eq.p_ac0.or.nch.eq.p_corn)then
         xxlsgn = log(sginin)
        xxlsga = log(sginia)
        xxlsgc = log(sginic)

        l2sginin = xxlsgn**2
        l2sginia = xxlsga**2
        l2sginic = xxlsgc**2

        en1 = exp(0.125*l2sginin)
        ea1 = exp(0.125*l2sginia)
        ec1 = exp(0.125*l2sginic)

        esn04 = en1**4
        esa04 = ea1**4
        esc04 = ec1**4

        esn05 = esn04*en1
        esa05 = esa04*ea1

        esn08 = esn04*esn04
        esa08 = esa04*esa04
        esc08 = esc04*esc04

        esn09 = esn04*esn05
        esa09 = esa04*esa05

        esn12 = esn04*esn04*esn04
        esa12 = esa04*esa04*esa04
        esc12 = esc04*esc04*esc04

        esn16 = esn08*esn08
        esa16 = esa08*esa08
        esc16 = esc08*esc08

        esn20 = esn16*esn04
        esa20 = esa16*esa04
        esc20 = esc16*esc04

        esn24 = esn12*esn12
        esa24 = esa12*esa12
        esc24 = esc12*esc12

        esn25 = esn16*esn09
        esa25 = esa16*esa09

        esn28 = esn20*esn08
        esa28 = esa20*esa08
        esc28 = esc20*esc08


        esn32 = esn16*esn16
        esa32 = esa16*esa16
        esc32 = esc16*esc16

        esn36 = esn16*esn20
        esa36 = esa16*esa20
        esc36 = esc16*esc20
       endif
!
! Units are something like number concentration
!
       if(nch.eq.p_nu0)chem=m3nuc/((dginin**3)*esn36)*alt
       if(nch.eq.p_ac0)chem=m3acc/((dginia**3)*esa36)*alt
       if(nch.eq.p_corn)chem=m3cor/((dginic**3)*esc36)*alt
     endif

   
  END SUBROUTINE bdy_chem_value_sorgam

  SUBROUTINE bdy_chem_value_gocart ( chem, nch )

! This subroutine is called to set the boundary values of chemistry

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if( nch == p_so2  ) then
       chem = 5.e-6
    else if( nch == p_sulf ) then
       chem = 3.e-6
    else if( nch == p_dms ) then
       chem = 1.e-6
    else if( nch == p_msa ) then
       chem = 1.e-6
    else if( nch == p_bc1 ) then
       chem = 1.e-2
    else if( nch == p_bc2 ) then
       chem = 1.e-2
    else if( nch == p_oc1 ) then
       chem = 1.e-2
    else if( nch == p_oc2 ) then
       chem = 1.e-2
    else if( nch == p_p25 ) then
       chem = 1.
    else
       chem = 1.e-12
    end if

  END SUBROUTINE bdy_chem_value_GOCART
  SUBROUTINE bdy_chem_value_tracer ( chem, nch )

! This subroutine is called to set the boundary values of chemistry
! species when chem_opt==CHEM_TRACER. Typically, the boundary values
! here should be set to match those in input_chem_profile so that the
! interior and boundary values are the same.
! William.Gustafson@pnl.gov; 16-Jun-2005

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if( nch .ne. p_co  ) then
       chem = 0.0001
    else if( nch == p_co ) then
       chem = 0.08
    else
       chem = conmin
    end if
    if( nch .eq. p_tracer_1  ) then
       chem = 0.08
    endif

  END SUBROUTINE bdy_chem_value_tracer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE bdy_chem_value_racm ( chem, z, nch, numgas,p_co2 )
                                  
    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: z          ! 3D height array
    INTEGER, intent(IN)   :: nch,p_co2  ! index number of chemical species
    INTEGER, intent(IN)   :: numgas     ! index number of last gas species

    INTEGER :: i, k, irefcur

    REAL, DIMENSION(kx):: cprof         ! chemical profile, diff. index order

    REAL, DIMENSION(1:kx):: zprof
    REAL                 :: stor
    REAL                 :: wgt0

    CHARACTER (LEN=80) :: message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Check the number of species
!     if((nch-1).gt.logg)return
     if (nch.eq.p_co2)then
       chem=370.
       return
     endif
     if (nch.eq.p_co2+1)then
       chem=1.7
       return
     endif
     if (nch.ge.p_co2+2)return
    

!    if( nch .GT. logg+1) then
     if( nch .GT. numgas) then
       message = ' Input_chem_profile: wrong number of chemical species'
       return
!       CALL WRF_ERROR_FATAL ( message )
     endif

      ! Vertically flip the chemistry data as it is given top down 
      !     and heights in zfa are bottom up
      ! Fill 1D chemical profile array cprof
      ! Convert species 28-34 (lo-6:lo) from (molecules/cm3) to (mol/mol)
      irefcur = iref(nch-1)
      DO k = 1,kx 
        zprof(k) = 0.5*(zfa_bdy(k)+zfa_bdy(k+1))
        if (irefcur .lt. lo-6) then
          cprof(k) = xl(irefcur,kx+1-k)
        else
          cprof(k) = xl(irefcur,kx+1-k)/dens(kx+1-k)
        end if
      ENDDO

      ! Interpolate temp 3D chemical profile array to WRF field
      IF (z .LT. zprof(1)) THEN 
        stor = cprof(1) 
      ELSE IF (z .GT. zprof(kx)) THEN
        stor = cprof(kx)
      ELSE
        ! We can trap between two levels and linearly interpolate
        input_loop:  DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN 
            stor = cprof(k)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z   - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor = MAX( wgt0 *cprof(k  ) + &
                     (1.-wgt0)*cprof(k+1), 0.)
            EXIT input_loop
          ENDIF  
        ENDDO input_loop
      ENDIF 

      ! Here is where the chemistry value is constructed
      chem = fracref(nch-1)*stor*1.E6

      ! special code for sulfate/h2so4
      if(nch.eq.p_sulf.and.p_nu0.gt.1)then
        chem=chem*(1.-so4vaptoaer)
      endif

      RETURN
  END SUBROUTINE bdy_chem_value_racm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE bdy_chem_value ( chem, z, nch, numgas )

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: z          ! 3D height array
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
    INTEGER, intent(IN)   :: numgas     ! index number of last gas species

    INTEGER :: i, k, irefcur

    REAL, DIMENSION(kx):: cprof         ! chemical profile, diff. index order

    REAL, DIMENSION(1:kx):: zprof
    REAL                 :: stor
    REAL                 :: wgt0

    CHARACTER (LEN=80) :: message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Check the number of species
!     if((nch-1).gt.logg)return
! for radmkpp there is co2 and ch4 in the variable list
!
     if(p_co2.gt.1)then
     if (nch.eq.p_co2)then
       chem=370.
       return
     endif
     if (nch.eq.p_ch4)then
       chem=1.7
       return
     endif
     endif

!     if( nch .GT. numgas) then
!       message = ' Input_chem_profile: wrong number of chemical species'
!       return
!       CALL WRF_ERROR_FATAL ( message )
!     endif

      ! Vertically flip the chemistry data as it is given top down 
      !     and heights in zfa are bottom up
      ! Fill 1D chemical profile array cprof
      ! Convert species 28-34 (lo-6:lo) from (molecules/cm3) to (mol/mol)
      irefcur = iref(nch-1)
      DO k = 1,kx 
        zprof(k) = 0.5*(zfa_bdy(k)+zfa_bdy(k+1))
        if (irefcur .lt. lo-6) then
          cprof(k) = xl(irefcur,kx+1-k)
        else
          cprof(k) = xl(irefcur,kx+1-k)/dens(kx+1-k)
        end if
      ENDDO

      ! Interpolate temp 3D chemical profile array to WRF field
      IF (z .LT. zprof(1)) THEN 
        stor = cprof(1) 
      ELSE IF (z .GT. zprof(kx)) THEN
        stor = cprof(kx)
      ELSE
        ! We can trap between two levels and linearly interpolate
        input_loop:  DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN 
            stor = cprof(k)
            EXIT input_loop
          ELSE IF (z .EQ. zprof(k+1) )THEN
            stor = cprof(k+1)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z   - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor = MAX( wgt0 *cprof(k  ) + &
                     (1.-wgt0)*cprof(k+1), 0.)
            EXIT input_loop
          ENDIF  
        ENDDO input_loop
      ENDIF 

      ! Here is where the chemistry value is constructed
      chem = fracref(nch-1)*stor*1.E6

      ! special code for sulfate/h2so4
      if(nch.eq.p_sulf.and.p_nu0.gt.1)then
        chem=chem*(1.-so4vaptoaer)
      endif

      RETURN
  END SUBROUTINE bdy_chem_value
!**********************************************************************
SUBROUTINE bdy_chem_value_ghg ( chem, nch )
! This subroutine is called to set the boundary constan values for
! greenhouse gases when gas_bc_opt=16

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if( nch==p_co2_bck .OR. nch==p_co2_bio .OR. nch==p_co2_oce .OR. nch==p_co2_ant &
    .OR. nch==p_co2_bbu .OR. nch==p_co2_tst ) then
    chem = 380.
else if( nch==p_co_bck .OR. nch==p_co_ant .OR. nch==p_co_bbu .OR. nch==p_co_tst) then
    chem = 0.1
else if( nch==p_ch4_bck .OR. nch==p_ch4_bio .OR. nch==p_ch4_ant &
         .OR. nch==p_ch4_bbu .OR. nch==p_ch4_tst ) then
   chem = 1.77
else
   chem = 1.e-12
end if

END SUBROUTINE bdy_chem_value_ghg
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE flow_dep_bdy_chem  (  chem,                                       &
                               chem_bxs,chem_btxs,                                  &
                               chem_bxe,chem_btxe,                                  &
                               chem_bys,chem_btys,                                  &
                               chem_bye,chem_btye,                                  &
                               dt,                                              &
                               spec_bdy_width,z,                                &
                               have_bcs_chem,                        & 
                               u, v, config_flags, alt, & 
                               t,pb,p,t0,p1000mb,rcp,ph,phb,g, &
                               spec_zone, ic,julday,    &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte,  &
                               u_pv,v_pv,t_pv,sigma,XMSF, UMSF, VMSF,CORL,PSB,DX,XLAT,pv )

!  This subroutine sets zero gradient conditions for outflow and a set profile value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      USE module_cam_mam_initmixrats, only:  bdy_chem_value_cam_mam

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone,spec_bdy_width,ic,julday
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: chem
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width), INTENT(IN   ) :: chem_bxs, chem_bxe, chem_btxs, chem_btxe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width), INTENT(IN   ) :: chem_bys, chem_bye, chem_btys, chem_btye
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: z
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alt
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: ph,phb,t,pb,p
      REAL, INTENT (IN) :: g,rcp,t0,p1000mb
      TYPE( grid_config_rec_type ), intent(IN) :: config_flags

      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: XMSF, CORL, PSB, XLAT
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(INOUT   ) :: UMSF, VMSF
      REAL,  DIMENSION( kms:kme ), INTENT(IN   ) :: sigma
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u_pv, v_pv, t_pv
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT   ) :: pv
      REAL, INTENT(IN)  :: DX


      INTEGER    :: i, j, k, numgas
      INTEGER    :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: i_inner, j_inner
      INTEGER    :: b_dist
      integer    :: itestbc, i_bdy_method
      real       :: tempfac,convfac,preshPa
      real       :: chem_bv_def
      logical    :: have_bcs_chem

      REAL       :: pv2o3_con   ! for determination of O3/PV proportionality constant
      REAL      :: day_fac  ! SAM 10/3/17 temporal fac .from Xing etal.(2016) PV-O3 relat. - depnds only on julday

      chem_bv_def = conmin
      numgas = get_last_gas(config_flags%chem_opt)
      itestbc=0
      if(p_nu0.gt.1)itestbc=1
      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      if (config_flags%do_pvozone) THEN
        day_fac=1.+0.22*sin(0.174533*12.*(30.*(julday/365.)+2.))
         call PVS(ims,ime,jms,jme,kms,kme, &
                  u_pv, v_pv,t_pv+t0, sigma, XMSF, UMSF, &
                  VMSF,CORL,PSB,DX,pv, &
                  ids,ide,jds,jde,kds,kde, &
                  its,ite,jts,jte,kts,kte)
      endif

! i_bdy_method determines which "bdy_chem_value" routine to use
!   1=radm2 or racm gas for  p_so2     <= ic <= p_ho2
!   2=sorgam aerosol    for  p_so4aj   <= ic <= p_corn
!   3=cbmz gas          for  p_hcl     <= ic <= p_isopo2
!                        OR  p_dms     <= ic <= p_mtf
!   4=mosaic aerosol    for  p_so4_a01 <= ic <= p_num_a01
!                        OR  p_so4_a02 <= ic <= p_num_a02
!                        OR  ...
!   5=tracer mode
!   0=none              for all other ic values
! (note:  some cbmz packages use dms,...,mtf while others do not)
! (note:  different mosaic packages use different number of sections)
      i_bdy_method = 0
      if ((ic .ge. p_so2) .and. (ic .le. p_ho2)) then
          i_bdy_method = 1

        if (config_flags%chem_opt == RACM_KPP .or.          &
            config_flags%chem_opt == GOCARTRACM_KPP .or.      &
            config_flags%chem_opt == RACMSORG_KPP .or.      &
            config_flags%chem_opt == RACM_ESRLSORG_KPP .or.      &
            config_flags%chem_opt == RACM_SOA_VBS_KPP .or.      & 
            config_flags%chem_opt == RACM_MIM_KPP .or.      & 
            config_flags%chem_opt == RACMSORG_AQCHEM_KPP .or.      & 
            config_flags%chem_opt == RACM_ESRLSORG_AQCHEM_KPP .or. &
            config_flags%chem_opt == RACM_SOA_VBS_AQCHEM_KPP  .or. &
            config_flags%chem_opt == RACM_SOA_VBS_HET_KPP ) then
          i_bdy_method = 9
        end if
        if (config_flags%chem_opt == RACMPM_KPP ) then
          i_bdy_method = 9
        end if


      else if ((ic .ge. p_so4aj) .and. (ic .le. p_corn)) then
          i_bdy_method = 2
      else if ((ic .ge. p_hcl) .and. (ic .le. p_isopo2)) then
          i_bdy_method = 3
      else if ((ic .ge. p_dms) .and. (ic .le. p_mtf)) then
          i_bdy_method = 3
      else if ((ic .ge. p_so4_a01) .and. (ic .le. p_num_a01)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a02) .and. (ic .le. p_num_a02)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a03) .and. (ic .le. p_num_a03)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a04) .and. (ic .le. p_num_a04)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a05) .and. (ic .le. p_num_a05)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a06) .and. (ic .le. p_num_a06)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a07) .and. (ic .le. p_num_a07)) then
          i_bdy_method = 4
      else if ((ic .ge. p_so4_a08) .and. (ic .le. p_num_a08)) then
          i_bdy_method = 4
      else if (config_flags%chem_opt == CHEM_TRACER) then
          i_bdy_method = 5
      else if (config_flags%chem_opt == CHEM_TRACE2) then
          i_bdy_method = 5
      else if (config_flags%chem_opt == GOCART_SIMPLE) then
          i_bdy_method = 7
      else if (config_flags%chem_opt == CB05_SORG_AQ_KPP) then
          if (ic .le. numgas) then
          i_bdy_method = 15
          end if
      else if (config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) then
          if (ic .le. numgas) then
          i_bdy_method = 17
          end if
      else if (config_flags%chem_opt == DUST) then
          i_bdy_method = 7
      else if (config_flags%chem_opt == CHEM_VASH) then
          i_bdy_method = 8
      else if (config_flags%chem_opt == CHEM_VOLC) then
          i_bdy_method = 8
      else if (config_flags%chem_opt == CHEM_VOLC_4BIN) then
          i_bdy_method = 8
      else if (config_flags%chem_opt==CO2_TRACER .OR. config_flags%chem_opt==GHG_TRACER) then
          i_bdy_method = 16
!         CALL wrf_message( "For GHGs strongly recommended to include LBCs from a global model!" )
      else if (config_flags%chem_opt == cbmz_cam_mam3_noaq .or. &
               config_flags%chem_opt == cbmz_cam_mam3_aq  ) then
          if ((ic .ge. p_so4_a1) .and. (ic .le. p_num_a3)) then
              i_bdy_method = 501
          end if
      else if (config_flags%chem_opt == cbmz_cam_mam7_noaq .or. &
               config_flags%chem_opt == cbmz_cam_mam7_aq  ) then
          if ((ic .ge. p_so4_a1) .and. (ic .le. p_num_a7)) then
              i_bdy_method = 501
          end if
      end if
      if (have_bcs_chem) i_bdy_method =6
      if (ic .lt. param_first_scalar) i_bdy_method = 0

!----------------------------------------------------------------------
!      if (i_bdy_method .eq. 1) then
!          print 90010, '_bdy_radm2  for ic=', ic, i_bdy_method
!      else if (i_bdy_method .eq. 2) then
!          print 90010, '_bdy_sorgam for ic=', ic, i_bdy_method
!      else if (i_bdy_method .eq. 3) then
!          print 90010, '_bdy_cbmz   for ic=', ic, i_bdy_method
!      else if (i_bdy_method .eq. 4) then
!          print 90010, '_bdy_mosaic for ic=', ic, i_bdy_method
!      else if (i_bdy_method .eq. 5) then
!          print 90010, '_bdy_tracer for ic=', ic, i_bdy_method
!          print *,numgas,num_chem
!      else 
!          print 90010, '_bdy_NONE** for ic=', ic, i_bdy_method
!      end if
!90010 format( a, 2(1x,i5) )
!90020 format( a, 1p, 2e12.2 )
!----------------------------------------------------------------------

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
           ! print*,'jts,jtf,jbs,its,b_dist,ibs,itf,ibe,spec_zone'
           ! print*, jts,jtf,jbs,its,b_dist,ibs,itf,ibe,spec_zone
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbs+spec_zone)
              ELSE
                if (i_bdy_method .eq. 1) then
                   CALL bdy_chem_value (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 9) then
                   CALL bdy_chem_value_racm(   &
                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
                else if (i_bdy_method .eq. 2) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_sorgam (   &
                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
                        alt(i,k,j),convfac,g)
                else if (i_bdy_method .eq. 3) then
                   CALL bdy_chem_value_cbmz (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 4) then
                   CALL bdy_chem_value_mosaic (   &
                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
                else if (i_bdy_method .eq. 5) then
                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 7) then
                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 8) then
                   chem(i,k,j) = 0.
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_gcm ( chem(i,k,j),chem_bys(i,k,1),chem_btys(i,k,1),dt,ic)
                else if (i_bdy_method .eq. 16) then
                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic ) 
                else if (i_bdy_method .eq. 15) then
                   CALL bdy_chem_value_cb05 (   &
                        1, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 17) then
                   CALL bdy_chem_value_cb05_vbs (   &
                        1, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 501) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_cam_mam(   &
                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                else
                   chem(i,k,j) = chem_bv_def
                endif

                preshPa=p(i,k,j)+pb(i,k,j)
                IF (config_flags%do_pvozone &
                    .and. p_o3.gt.1 &
                    .and. ic.eq.p_o3 &
                    .and. preshPa.le.50000.) then
                   CALL PV2O3_CONST(xlat(i,j),preshPa,pv2o3_con)
                  pv2o3_con=pv2o3_con*day_fac  ! 10/3/17 Add in temporal factor for O3-PV ratio
                   IF (chem(i,k,j)*1.E-6/fracref(ic-1).lt.pv2o3_con*pv(i,k,j)*1.E-9) then
                      chem(i,k,j) = fracref(ic-1)*pv(i,k,j)*pv2o3_con*1.E-9 * 1.E6
                   ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbe-spec_zone)
              ELSE
                if (i_bdy_method .eq. 1) then
                   CALL bdy_chem_value (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 9) then
                   CALL bdy_chem_value_racm (   &
                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
                else if (i_bdy_method .eq. 2) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_sorgam (   &
                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
                        alt(i,k,j),convfac,g)
                else if (i_bdy_method .eq. 3) then
                   CALL bdy_chem_value_cbmz (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 4) then
                   CALL bdy_chem_value_mosaic (   &
                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
                else if (i_bdy_method .eq. 5) then
                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_gcm ( chem(i,k,j),chem_bye(i,k,1),chem_btye(i,k,1),dt,ic)
                else if (i_bdy_method .eq. 7) then
                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 8) then
                   chem(i,k,j) = 0.
                else if (i_bdy_method .eq. 16) then
                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic )  ! For GHGs
                else if (i_bdy_method .eq. 15) then
                   CALL bdy_chem_value_cb05 (   &
                        2, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 17) then
                   CALL bdy_chem_value_cb05_vbs (   &
                        2, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 501) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_cam_mam(   &
                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                else
                   chem(i,k,j) = chem_bv_def
                endif
                preshPa=p(i,k,j)+pb(i,k,j)
                IF (config_flags%do_pvozone &
                    .and. p_o3.gt.1 &
                    .and. ic.eq.p_o3 &
                    .and. preshPa.le.50000.) then
                   CALL PV2O3_CONST(xlat(i,j),preshPa,pv2o3_con)
                  pv2o3_con=pv2o3_con*day_fac  ! 10/3/17 Add in temporal factor for O3-PV ratio
                   IF (chem(i,k,j)*1.E-6/fracref(ic-1).lt.pv2o3_con*pv(i,k,j)*1.E-9) then
                      chem(i,k,j) = fracref(ic-1)*pv(i,k,j)*pv2o3_con*1.E-9 * 1.E6
                   ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(ibs+spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 1) then
                   CALL bdy_chem_value (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 9) then
                   CALL bdy_chem_value_racm (   &
                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
                else if (i_bdy_method .eq. 2) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_sorgam (   &
                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
                        alt(i,k,j),convfac,g)
                else if (i_bdy_method .eq. 3) then
                   CALL bdy_chem_value_cbmz (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 4) then
                   CALL bdy_chem_value_mosaic (   &
                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
                else if (i_bdy_method .eq. 5) then
                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_gcm ( chem(i,k,j),chem_bxs(j,k,1),chem_btxs(j,k,1),dt,ic)
                else if (i_bdy_method .eq. 7) then
                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 8) then
                   chem(i,k,j) = 0.
                else if (i_bdy_method .eq. 16) then
                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic )  ! For GHGs
                else if (i_bdy_method .eq. 15) then
                   CALL bdy_chem_value_cb05 (   &
                        3, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 17) then
                   CALL bdy_chem_value_cb05_vbs (   &
                        3, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 501) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_cam_mam(   &
                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                else
                   chem(i,k,j) = chem_bv_def
                endif
                preshPa=p(i,k,j)+pb(i,k,j)
                IF (config_flags%do_pvozone &
                    .and. p_o3.gt.1 &
                    .and. ic.eq.p_o3 &
                    .and. preshPa.le.50000.) then
                   CALL PV2O3_CONST(xlat(i,j),preshPa,pv2o3_con)
                  pv2o3_con=pv2o3_con*day_fac  ! 10/3/17 Add in temporal factor for O3-PV ratio
                   IF (chem(i,k,j)*1.E-6/fracref(ic-1).lt.pv2o3_con*pv(i,k,j)*1.E-9) then
                      chem(i,k,j) = fracref(ic-1)*pv(i,k,j)*pv2o3_con*1.E-9 * 1.E6
                   ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                chem(i,k,j) = chem(ibe-spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 1) then
                   CALL bdy_chem_value (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 9) then
                   CALL bdy_chem_value_racm (   &
                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
                else if (i_bdy_method .eq. 2) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_sorgam (   &
                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
                        alt(i,k,j),convfac,g)
                else if (i_bdy_method .eq. 3) then
                   CALL bdy_chem_value_cbmz (   &
                        chem(i,k,j), z(i,k,j), ic, numgas )
                else if (i_bdy_method .eq. 4) then
                   CALL bdy_chem_value_mosaic (   &
                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
                else if (i_bdy_method .eq. 5) then
                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_chem_value_gcm ( chem(i,k,j),chem_bxe(j,k,1),chem_btxe(j,k,1),dt,ic)
                else if (i_bdy_method .eq. 7) then
                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
                else if (i_bdy_method .eq. 8) then
                   chem(i,k,j) = 0.
                else if (i_bdy_method .eq. 16) then
                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic ) ! For GHGs
                else if (i_bdy_method .eq. 15) then
                   CALL bdy_chem_value_cb05 (   &
                        4, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 17) then
                   CALL bdy_chem_value_cb05_vbs (   &
                        4, chem(i,k,j), k, ic, config_flags, numgas )
                else if (i_bdy_method .eq. 501) then
                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
                   CALL bdy_chem_value_cam_mam(   &
                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                else
                   chem(i,k,j) = chem_bv_def
                endif
                preshPa=p(i,k,j)+pb(i,k,j)
                IF (config_flags%do_pvozone &
                    .and. p_o3.gt.1 &
                    .and. ic.eq.p_o3 &
                    .and. preshPa.le.50000.) then
                   CALL PV2O3_CONST(xlat(i,j),preshPa,pv2o3_con)
                  pv2o3_con=pv2o3_con*day_fac  ! 10/3/17 Add in temporal factor for O3-PV ratio
                   IF (chem(i,k,j)*1.E-6/fracref(ic-1).lt.pv2o3_con*pv(i,k,j)*1.E-9) then
                      chem(i,k,j) = fracref(ic-1)*pv(i,k,j)*pv2o3_con*1.E-9 * 1.E6
                   ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

! SAM 10/4/17 Force top boundary according to PV if do_pvozone is on
     IF (config_flags%do_pvozone &
        .and. p_o3.gt.1 &
        .and. ic.eq.p_o3 ) THEN
          k = kte
        DO i = its, ite
            DO j = jts, jte
                preshPa=p(i,k,j)+pb(i,k,j)
                   CALL PV2O3_CONST(xlat(i,j),preshPa,pv2o3_con)
                  pv2o3_con=pv2o3_con*day_fac  ! 10/3/17 Add in temporal factor for O3-PV ratio
                   IF (chem(i,k,j)*1.E-6/fracref(ic-1).lt.pv2o3_con*pv(i,k,j)*1.E-9) then
                      chem(i,k,j) = fracref(ic-1)*pv(i,k,j)*pv2o3_con*1.E-9 * 1.E6
                   ENDIF
            ENDDO
        ENDDO
      ENDIF 


   END SUBROUTINE flow_dep_bdy_chem
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! this is a kludge routine as of now....
!


   SUBROUTINE flow_dep_bdy_s1 (  field,                     &
                               u, v, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte)

!  This subroutine sets zero gradient conditions for outflow and zero value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
      TYPE( grid_config_rec_type ) config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x
!----------------------------------------------
!jeff
!--   hardwire outmost lateral boundary value at constant value

      real value_bc

      value_bc = 1.0

!-----------------------------------------------

      periodic_x = config_flags%periodic_x

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbs+spec_zone)
              ELSE
                field(i,k,j) = value_bc
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (jbe - jtf .lt. spec_zone) THEN
! Y-end boundary
        DO j = max(jts,jbe-spec_zone+1), jtf
          b_dist = jbe - j
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j+1) .gt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbe-spec_zone)
              ELSE
                field(i,k,j) = value_bc
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(ibs+spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = value_bc
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                field(i,k,j) = field(ibe-spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = value_bc
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

   END SUBROUTINE flow_dep_bdy_s1

!------------------------------------------------------------------------------

   SUBROUTINE flow_dep_bdy_s2 (  field,                     &
                               u, v, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte, dtstep, ktau)

!  This subroutine sets zero gradient conditions for outflow and zero value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
      TYPE( grid_config_rec_type ) config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x
!----------------------------------------------
!jeff
!--   hardwire outmost lateral boundary value with 1-day efolding decay

      real,    INTENT(IN   ) :: dtstep
      integer, INTENT(IN   ) :: ktau

      real value_bc
      real factor_decay

!--  initial value

      value_bc = 1.0

!-- decay factor, with efolding time of one day

      factor_decay = 1./(86400./dtstep)

!-----------------------------------------------

      periodic_x = config_flags%periodic_x

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbs+spec_zone)
              ELSE
                if (ktau .eq. 1) then
                   field(i,k,j) = value_bc
                else
                   field(i,k,j) = field(i,k,j) * (1. - factor_decay)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (jbe - jtf .lt. spec_zone) THEN
! Y-end boundary
        DO j = max(jts,jbe-spec_zone+1), jtf
          b_dist = jbe - j
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j+1) .gt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbe-spec_zone)
              ELSE
                if (ktau .eq. 1) then
                   field(i,k,j) = value_bc
                else
                   field(i,k,j) = field(i,k,j) * (1. - factor_decay)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(ibs+spec_zone,k,j_inner)
              ELSE
                if (ktau .eq. 1) then
                   field(i,k,j) = value_bc
                else
                   field(i,k,j) = field(i,k,j) * (1. - factor_decay)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                field(i,k,j) = field(ibe-spec_zone,k,j_inner)
              ELSE
                if (ktau .eq. 1) then
                   field(i,k,j) = value_bc
                else
                   field(i,k,j) = field(i,k,j) * (1. - factor_decay)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

   END SUBROUTINE flow_dep_bdy_s2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE bdy_chem_value_gcm ( chem, chem_b, chem_bt, dt,ic)

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: chem_b
    REAL,    intent(IN)   :: chem_bt
    REAL,    intent(IN)   :: dt
    INTEGER, intent(IN)   :: ic


    CHARACTER (LEN=80) :: message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     if( nch .GT. numgas) then
!       message = ' Input_chem_profile: wrong number of chemical species'
!       return
!       CALL WRF_ERROR_FATAL ( message )
!     endif


      !print*,'before',chem,chem_bt ,dt, ic

      chem=max(epsilc,chem_b + chem_bt * dt)
      !print*,'after',chem
      RETURN
  END SUBROUTINE bdy_chem_value_gcm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE cv_mmdd_jday ( YY, MM, DD, JDAY)
!
!     Subroutine to compute the julian day given the month and day
!
!
      INTEGER,      INTENT(IN )    :: YY, MM, DD
      INTEGER,      INTENT(OUT)    :: JDAY

      INTEGER, DIMENSION(12) :: imon, imon_a
      INTEGER                :: i

      DATA imon_a /0,31,59,90,120,151,181,212,243,273,304,334/
!
!..... Check for leap year.
!
      do i=1,12
         imon(i) = imon_a(i)
      enddo 
      if(YY .eq. (YY/4)*4) then
         do i=3,12
            imon(i) = imon(i) + 1
         enddo 
      endif
!
!..... Convert month, day to julian day.
!
      jday = imon(mm) + dd


   END SUBROUTINE cv_mmdd_jday

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   integer FUNCTION get_last_gas(chem_opt)
     implicit none
     integer, intent(in) :: chem_opt

     ! Determine the index of the last gas species, which depends
     ! upon the gas mechanism.

     select case (chem_opt)
     case (0)
        get_last_gas = 0

!!! TUCCELLA
     case (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, &
           RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP,       &
           RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACMSORG_KPP, RACM_SOA_VBS_KPP,& 
           RACM_SOA_VBS_AQCHEM_KPP,GOCARTRACM_KPP,GOCARTRADM2,  &
           RACM_SOA_VBS_HET_KPP)
        get_last_gas = p_ho2

     case (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
          SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP )!BSINGH(12/13/2013): Added SAPRC 8 bin AQ case and non-aq on 04/03/2014
        get_last_gas = p_ch4


     case (CBMZ,CBMZ_MOSAIC_DMS_4BIN,CBMZ_MOSAIC_DMS_8BIN,CBMZ_MOSAIC_DMS_4BIN_AQ,CBMZ_MOSAIC_DMS_8BIN_AQ)
        get_last_gas = p_mtf

     case (CBMZ_BB,CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_4BIN, &
           CBMZ_MOSAIC_8BIN,CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ,CBMZSORG,CBMZSORG_AQ)
        get_last_gas = p_isopo2

     case (CHEM_TRACER)
        get_last_gas = p_co

     case (CHEM_TRACE2)
        get_last_gas = p_tracer_1

     case (GOCART_SIMPLE)
        get_last_gas = p_msa

     case (CBM4_KPP)
        get_last_gas = p_ho2

     case (CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
        get_last_gas = p_nh3

     case (CHEM_VASH)
        get_last_gas = 0
     case (CHEM_VOLC)
        get_last_gas = p_sulf
     case (CHEM_VOLC_4BIN)
        get_last_gas = 0
     case (DUST)
        get_last_gas = 0
     case (MOZART_KPP)
        get_last_gas = p_meko2

     case (CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
        GET_LAST_GAS = p_ic3h7no3

     case (MOZCART_KPP)
        get_last_gas = p_meko2

     case (T1_MOZCART_KPP)
        get_last_gas = p_xylolo2

     case (MOZART_MOSAIC_4BIN_KPP)
        get_last_gas = p_meko2

     case (MOZART_MOSAIC_4BIN_AQ_KPP)
        get_last_gas = p_meko2

     case (CO2_TRACER,GHG_TRACER) ! No gas chemistry or deposition for GHGs
        get_last_gas = 0
     case ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
        get_last_gas = p_soag
     case default
        call wrf_error_fatal("get_last_gas: could not decipher chem_opt value")

     end select

   END FUNCTION get_last_gas
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!****************************************************************
!                                                               *
!   SUBROUTINE TO SET AEROSOL BC VALUES USING THE               *
!   aer_bc_opt == aer_bc_pnnl OPTION.                           *
!                                                               *
!   wig 22-Apr-2004, original routine                           *
!       rce 25-apr-2004 - changed name to                       *
!                         "sorgam_set_aer_bc_pnnl"              *
!       wig  7-May-2004, added height dependance                *
!                                                               *
!   CALLS THE FOLLOWING SUBROUTINES:  NONE                      *
!                                                               *
!   CALLED BY:                        bdy_chem_value_sorgam     *
!                                                               *
!****************************************************************
    SUBROUTINE sorgam_set_aer_bc_pnnl( chem, z, nch, config_flags )
      USE module_data_sorgam, ONLY : dginia, dginin, dginic, esn36, esc36, esa36, seasfac, no3fac, nh4fac, so4fac, soilfac, anthfac, orgfac

      implicit none

      INTEGER,INTENT(IN   ) :: nch
      real,intent(in      ) :: z
      REAL,INTENT(INOUT   ) :: chem
      TYPE (grid_config_rec_type) , INTENT (in) :: config_flags

      REAL :: mult,                       &
              m3acc, m3cor, m3nuc,        &
              bv_so4ai, bv_so4aj,         &
              bv_nh4ai, bv_nh4aj,         &
              bv_no3ai, bv_no3aj,         &
              bv_eci,   bv_ecj,           &
              bv_p25i,  bv_p25j,          &
              bv_orgpai,bv_orgpaj,        &
              bv_antha, bv_seas, bv_soila

!
! Determine height multiplier...
! This should mimic the calculation in sorgam_init_aer_ic_pnnl,
! mosaic_init_wrf_mixrats_opt2, and bdy_chem_value_mosaic
!!$!    Height(m)     Multiplier
!!$!    ---------     ----------
!!$!    <=2000        1.0
!!$!    2000<z<3000   linear transition zone to 0.5
!!$!    3000<z<5000   linear transision zone to 0.25
!!$!    >=5000        0.25
!!$!
!!$! which translates to:
!!$!    2000<z<3000   mult = 1.0 + (z-2000.)*(0.5-1.0)/(3000.-2000.)
!!$!    3000<z<5000   mult = 0.5 + (z-3000.)*(0.25-0.5)/(5000.-3000.)
!!$! or in reduced form:
!!$      if( z <= 2000. ) then
!!$         mult = 1.0
!!$      elseif( z > 2000. &
!!$           .and. z <= 3000. ) then
!!$         mult = 1.0 - 0.0005*(z-2000.)
!!$      elseif( z > 3000. &
!!$           .and. z <= 5000. ) then
!!$         mult = 0.5 - 1.25e-4*(z-3000.)
!!$      else
!!$         mult = 0.25
!!$      end if
! Updated aerosol profile multiplier 1-Apr-2005:
!    Height(m)     Multiplier
!    ---------     ----------
!    <=2000        1.0
!    2000<z<3000   linear transition zone to 0.25
!    3000<z<5000   linear transision zone to 0.125
!    >=5000        0.125
!
! which translates to:
!    2000<z<3000   mult = 1.00 + (z-2000.)*(0.25-1.0)/(3000.-2000.)
!    3000<z<5000   mult = 0.25 + (z-3000.)*(0.125-0.25)/(5000.-3000.)
! or in reduced form:
!       if( z <= 2000. ) then
!          mult = 1.0
!       elseif( z > 2000. &
!            .and. z <= 3000. ) then
!          mult = 1.0 - 0.00075*(z-2000.)
!       elseif( z > 3000. &
!            .and. z <= 5000. ) then
!          mult = 0.25 - 4.166666667e-5*(z-3000.)
!       else
!          mult = 0.125
!       end if
        if( z <= 500. ) then
           mult = 1.0
        elseif( z > 500. &
             .and. z <= 1000. ) then
           mult = 1.0 - 0.001074*(z-500.)
        elseif( z > 1000. &
             .and. z <= 5000. ) then
           mult = 0.463 - 0.000111*(z-1000.)
        else
           mult = 0.019
        end if

! These should match what is in sorgam_init_aer_ic_pnnl.
! Boundary values as of 2-Dec-2004:
!     bv_so4aj  = mult*2.375
!     bv_so4ai  = mult*0.179
!     bv_nh4aj  = mult*0.9604
!     bv_nh4ai  = mult*0.0196
!     bv_no3aj  = mult*0.0650
!     bv_no3ai  = mult*0.0050
!     bv_ecj    = mult*0.1630
!     bv_eci    = mult*0.0120
!     bv_p25j   = mult*0.6350
!     bv_p25i   = mult*0.0490
!     bv_orgpaj = mult*0.9300
!     bv_orgpai = mult*0.0700
!     bv_antha  = mult*2.2970
!     bv_seas   = mult*0.2290
!     bv_soila  = conmin
      bv_so4aj = mult*0.300*0.97
      bv_so4ai = mult*0.300*0.03
      bv_nh4aj = mult*0.094*0.97
      bv_nh4ai = mult*0.094*0.03
      bv_no3aj = mult*0.001*0.97
      bv_no3ai = mult*0.001*0.03
      bv_ecj   = mult*0.013*0.97
      bv_eci   = mult*0.013*0.03
      bv_p25j  = mult*4.500*0.97
      bv_p25i  = mult*4.500*0.03
      bv_antha = mult*4.500/2.0
      bv_orgpaj = mult*0.088*0.97
      bv_orgpai = mult*0.088*0.03
      bv_seas   = mult*1.75
      bv_soila  = conmin

#if (CASENAME == 4)
        if( z <= 2000. ) then
           mult = 1.0
        elseif( z > 2000. &
             .and. z <= 3000. ) then
           mult = 1.0 - 0.00075*(z-2000.)
        elseif( z > 3000. &
             .and. z <= 5000. ) then
           mult = 0.25 - 4.166666667e-5*(z-3000.)
        else
           mult = 0.125
        end if
      bv_so4aj = mult*(0.0004810001+0.7271175)*0.97
      bv_so4ai = mult*(0.0004810001+0.7271175)*0.03
      bv_nh4aj = mult*0.2133708*0.97
      bv_nh4ai = mult*0.2133708*0.03
      bv_no3aj = mult*0.01399485*0.97
      bv_no3ai = mult*0.01399485*0.03
      bv_ecj = mult*0.04612048*0.97
      bv_eci = mult*0.04612048*0.03
      bv_p25j = mult*1.890001e-05*0.97
      bv_p25i = mult*1.890001e-05*0.03
      bv_antha = conmin
      bv_orgpaj = mult*0.5844942*0.97
      bv_orgpai = mult*0.5844942*0.03
      bv_seas = conmin
      bv_soila = conmin

#endif

! m3... calculations should match the very end of module_aerosols_sorgam.F
!... i-mode (note that the 8 SOA species have bv=conmin)
      m3nuc = so4fac*bv_so4ai + nh4fac*bv_nh4ai + &
        no3fac*bv_no3ai + &
        orgfac*8.0*conmin + orgfac*bv_orgpai + &
        anthfac*bv_p25i + anthfac*bv_eci

!... j-mode (note that the 8 SOA species have bv=conmin)
      m3acc = so4fac*bv_so4aj + nh4fac*bv_nh4aj + &
        no3fac*bv_no3aj + &
        orgfac*8.0*conmin + orgfac*bv_orgpaj + &
        anthfac*bv_p25j + anthfac*bv_ecj

!...c-mode
      m3cor = soilfac*bv_soila + seasfac*bv_seas + &
        anthfac*bv_antha

! Cannot set_sulf here because it is a "radm2" species whose bc value
! is set via bdy_chem_value. Instead, xl(iref(p_sulf-1),:) is set to
! the value conmin in subroutine gasprofile_init_pnnl
!      if( nch == p_sulf    ) chem = conmin !as per rce's 0 recommendation

      if( nch == p_so4aj   ) chem = bv_so4aj
      if( nch == p_so4ai   ) chem = bv_so4ai
      if( nch == p_nh4aj   ) chem = bv_nh4aj
      if( nch == p_nh4ai   ) chem = bv_nh4ai
      if( nch == p_no3aj   ) chem = bv_no3aj
      if( nch == p_no3ai   ) chem = bv_no3ai
      if( nch == p_ecj     ) chem = bv_ecj
      if( nch == p_eci     ) chem = bv_eci
      if( nch == p_p25j    ) chem = bv_p25j
      if( nch == p_p25i    ) chem = bv_p25i
      if( nch == p_orgpaj  ) chem = bv_orgpaj
      if( nch == p_orgpai  ) chem = bv_orgpai

      if( nch == p_orgaro1j) chem = conmin
      if( nch == p_orgaro1i) chem = conmin
      if( nch == p_orgaro2j) chem = conmin
      if( nch == p_orgaro2i) chem = conmin
      if( nch == p_orgalk1j) chem = conmin
      if( nch == p_orgalk1i) chem = conmin
      if( nch == p_orgole1j) chem = conmin
      if( nch == p_orgole1i) chem = conmin
      if( nch == p_orgba1j ) chem = conmin
      if( nch == p_orgba1i ) chem = conmin
      if( nch == p_orgba2j ) chem = conmin
      if( nch == p_orgba2i ) chem = conmin
      if( nch == p_orgba3j ) chem = conmin
      if( nch == p_orgba3i ) chem = conmin
      if( nch == p_orgba4j ) chem = conmin
      if( nch == p_orgba4i ) chem = conmin

      if( nch == p_antha   ) chem = bv_antha
      if( nch == p_soila   ) chem = bv_soila
      if( nch == p_seas    ) chem = bv_seas

      if( nch == p_nu0     ) chem = m3nuc/((dginin**3)*esn36)
      if( nch == p_ac0     ) chem = m3acc/((dginia**3)*esa36)
      if( nch == p_corn    ) chem = m3cor/((dginic**3)*esc36)

    END SUBROUTINE sorgam_set_aer_bc_pnnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE sorgam_vbs_set_aer_bc_pnnl( chem, z, nch, config_flags )
      USE module_data_sorgam_vbs, ONLY : dginia, dginin, dginic, esn36, esc36, esa36, seasfac, no3fac, nh4fac, so4fac, soilfac, anthfac, orgfac

      implicit none

      INTEGER,INTENT(IN   ) :: nch
      real,intent(in      ) :: z
      REAL,INTENT(INOUT   ) :: chem
      TYPE (grid_config_rec_type) , INTENT (in) :: config_flags

      REAL :: mult,                       &
              m3acc, m3cor, m3nuc,        &
              bv_so4ai, bv_so4aj,         &
              bv_nh4ai, bv_nh4aj,         &
              bv_no3ai, bv_no3aj,         &
              bv_eci,   bv_ecj,           &
              bv_p25i,  bv_p25j,          &
              bv_orgpai,bv_orgpaj,        &
              bv_antha, bv_seas, bv_soila

        if( z <= 500. ) then
           mult = 1.0
        elseif( z > 500. &
             .and. z <= 1000. ) then
           mult = 1.0 - 0.001074*(z-500.)
        elseif( z > 1000. &
             .and. z <= 5000. ) then
           mult = 0.463 - 0.000111*(z-1000.)
        else
           mult = 0.019
        end if

      bv_so4aj = mult*0.300*0.97
      bv_so4ai = mult*0.300*0.03
      bv_nh4aj = mult*0.094*0.97
      bv_nh4ai = mult*0.094*0.03
      bv_no3aj = mult*0.001*0.97
      bv_no3ai = mult*0.001*0.03
      bv_ecj   = mult*0.013*0.97
      bv_eci   = mult*0.013*0.03
      bv_p25j  = mult*4.500*0.97
      bv_p25i  = mult*4.500*0.03
      bv_antha = mult*4.500/2.0
      bv_orgpaj = mult*0.088*0.97
      bv_orgpai = mult*0.088*0.03
      bv_seas   = mult*1.75
      bv_soila  = conmin

#if (CASENAME == 4)
        if( z <= 2000. ) then
           mult = 1.0
        elseif( z > 2000. &
             .and. z <= 3000. ) then
           mult = 1.0 - 0.00075*(z-2000.)
        elseif( z > 3000. &
             .and. z <= 5000. ) then
           mult = 0.25 - 4.166666667e-5*(z-3000.)
        else
           mult = 0.125
        end if
      bv_so4aj = mult*(0.0004810001+0.7271175)*0.97
      bv_so4ai = mult*(0.0004810001+0.7271175)*0.03
      bv_nh4aj = mult*0.2133708*0.97
      bv_nh4ai = mult*0.2133708*0.03
      bv_no3aj = mult*0.01399485*0.97
      bv_no3ai = mult*0.01399485*0.03
      bv_ecj = mult*0.04612048*0.97
      bv_eci = mult*0.04612048*0.03
      bv_p25j = mult*1.890001e-05*0.97
      bv_p25i = mult*1.890001e-05*0.03
      bv_antha = conmin
      bv_orgpaj = mult*0.5844942*0.97
      bv_orgpai = mult*0.5844942*0.03
      bv_seas = conmin
      bv_soila = conmin

#endif

! m3... calculations should match the very end of module_aerosols_sorgam.F
!... i-mode (note that the 8 SOA species have bv=conmin)
      m3nuc = so4fac*bv_so4ai + nh4fac*bv_nh4ai + &
        no3fac*bv_no3ai + &
        orgfac*8.0*conmin + orgfac*bv_orgpai + &
        anthfac*bv_p25i + anthfac*bv_eci

!... j-mode (note that the 8 SOA species have bv=conmin)
      m3acc = so4fac*bv_so4aj + nh4fac*bv_nh4aj + &
        no3fac*bv_no3aj + &
        orgfac*8.0*conmin + orgfac*bv_orgpaj + &
        anthfac*bv_p25j + anthfac*bv_ecj

!...c-mode
      m3cor = soilfac*bv_soila + seasfac*bv_seas + &
        anthfac*bv_antha

! Cannot set_sulf here because it is a "radm2" species whose bc value
! is set via bdy_chem_value. Instead, xl(iref(p_sulf-1),:) is set to
! the value conmin in subroutine gasprofile_init_pnnl
!      if( nch == p_sulf    ) chem = conmin !as per rce's 0 recommendation

      if( nch == p_so4aj   ) chem = bv_so4aj
      if( nch == p_so4ai   ) chem = bv_so4ai
      if( nch == p_nh4aj   ) chem = bv_nh4aj
      if( nch == p_nh4ai   ) chem = bv_nh4ai
      if( nch == p_no3aj   ) chem = bv_no3aj
      if( nch == p_no3ai   ) chem = bv_no3ai
      if( nch == p_ecj     ) chem = bv_ecj
      if( nch == p_eci     ) chem = bv_eci
      if( nch == p_p25j    ) chem = bv_p25j
      if( nch == p_p25i    ) chem = bv_p25i
      if( nch == p_orgpaj  ) chem = bv_orgpaj
      if( nch == p_orgpai  ) chem = bv_orgpai

          if( nch == p_asoa1j) chem = conmin
          if( nch == p_asoa1i) chem = conmin
          if( nch == p_asoa2j) chem = conmin
          if( nch == p_asoa2i) chem = conmin
          if( nch == p_asoa3j) chem = conmin
          if( nch == p_asoa3i) chem = conmin
          if( nch == p_asoa4j) chem = conmin
          if( nch == p_asoa4i) chem = conmin
          if( nch == p_bsoa1j ) chem = conmin
          if( nch == p_bsoa1i ) chem = conmin
          if( nch == p_bsoa2j ) chem = conmin
          if( nch == p_bsoa2i ) chem = conmin
          if( nch == p_bsoa3j ) chem = conmin
          if( nch == p_bsoa3i ) chem = conmin
          if( nch == p_bsoa4j ) chem = conmin
          if( nch == p_bsoa4i ) chem = conmin

      if( nch == p_antha   ) chem = bv_antha
      if( nch == p_soila   ) chem = bv_soila
      if( nch == p_seas    ) chem = bv_seas

      if( nch == p_nu0     ) chem = m3nuc/((dginin**3)*esn36)
      if( nch == p_ac0     ) chem = m3acc/((dginia**3)*esa36)
      if( nch == p_corn    ) chem = m3cor/((dginic**3)*esc36)

    END SUBROUTINE sorgam_vbs_set_aer_bc_pnnl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!****************************************************************
!                                                               *
!   SUBROUTINE TO OVERWRITE THE PREDEFINED OZONE PROFILE        *
!   WHEN gas_ic_opt == gas_ic_pnnl                              *
!	  OR gas_bc_opt == gas_bc_pnnl                              *
!                                                               *
!   wig, 21-Apr-2004                                            *
!       rce 25-apr-2004 - changed name to                       *
!                         "gasprofile_init_pnnl"                *
!                                                               *
!   CALLS THE FOLLOWING SUBROUTINES:  NONE                      *
!                                                               *
!   CALLED BY:                        chem_init                 *
!                                     input_chem_profile        *
!                                                               *
!****************************************************************
    SUBROUTINE gasprofile_init_pnnl( chem_opt )
      use module_data_sorgam,only:  conmin
      implicit none
      INTEGER, INTENT (in) :: chem_opt

      integer :: k

      call wrf_debug ( 500 , 'wrfchem:gasprofile_init_pnnl' )
!     print*,'gasprofile_init_pnnl redefining o3 and sulf profiles.'

! Original O3 profile values:
!      / 1.68E-07, 1.68E-07, 5.79E-08, 5.24E-08, 5.26E-08, &
!       5.16E-08, 4.83E-08, 4.50E-08, 4.16E-08, 3.80E-08, 3.56E-08, &
!       3.35E-08, 3.15E-08, 3.08E-08, 3.06E-08, 3.00E-08/  

! Note that heights associated with 2nd index of xl correspond to upside-down
! zfa values that have been "de-staggered".
! Height = 0.5*(zfa(1:kx) + zfa(2:kx+1)) and then flipped:
!      / 18500., 14050., 11150., 9355., 7705., 6120., 4675., 3430.,
!         2430.,  1720.,  1195., 781.5,  494., 298.5, 148.5,  42.5
 
#if (CASENAME != 4)
 
#if (CASENAME == 0)
      if( p_o3 > 1 ) then
                                          !Rounded to closest level:
         xl(iref(p_o3-1),11:16) = 4.00e-8 !40 ppbv below 1 km
         xl(iref(p_o3-1),3:10)  = 6.50e-8 !65 ppbv > 2 km and < stratosphere
                                          !  Changed from 70 ppbv 1-Apr-2005
      end if
#endif

#if (CASENAME == 1)
      if( p_o3 > 1 ) then
         xl(iref(p_o3-1),11:16) = 3.50e-8 !35 ppbv below 1 km
         xl(iref(p_o3-1),3:10)  = 6.00e-8 !60 ppbv > 2 km and < stratosphere
      end if
#endif

#if (CASENAME == 101)

      if( p_o3 > 1 ) then
         xl(iref(p_o3-1),12:16) = 1.4e-7
         xl(iref(p_o3-1),11:11) = 1.40e-7
         xl(iref(p_o3-1),10:10) = 1.4e-7
         xl(iref(p_o3-1),9:9)   = 1.4e-7
         xl(iref(p_o3-1),8:8)   = 1.4e-7
         xl(iref(p_o3-1),7:7)   = 1.4e-7
         xl(iref(p_o3-1),6:6)   = 1.4e-7
         xl(iref(p_o3-1),5:5)   = 1.4e-7
         xl(iref(p_o3-1),4:4)   = 1.4e-7
         xl(iref(p_o3-1),3:3)   = 1.4e-7
         xl(iref(p_o3-1),2:2)   = 1.44e-7
         xl(iref(p_o3-1),1:1)   = 2.00e-7
      end if

      if( p_co > 1 ) then
         xl(iref(p_co-1),12:16) = 5.0e-7
         xl(iref(p_co-1),11:11) = 5.0e-7
         xl(iref(p_co-1),10:10) = 5.0e-7
         xl(iref(p_co-1),9:9)   = 5.0e-7
         xl(iref(p_co-1),8:8)   = 5.0e-7
         xl(iref(p_co-1),7:7)   = 5.0e-7
         xl(iref(p_co-1),6:6)   = 5.0e-7
         xl(iref(p_co-1),5:5)   = 5.0e-7
         xl(iref(p_co-1),4:4)   = 5.0e-7
         xl(iref(p_co-1),3:3)   = 5.0e-7
         xl(iref(p_co-1),2:2)   = 6.50e-8
         xl(iref(p_co-1),1:1)   = 6.10e-8
      end if

      if( p_so2 > 1 ) then
         xl(iref(p_so2-1),12:16) = 6.00e-9
         xl(iref(p_so2-1),11:11) = 1.00e-9
         xl(iref(p_so2-1),10:10) = 4.00e-9
         xl(iref(p_so2-1),9:9)   = 5.00e-9
         xl(iref(p_so2-1),8:8)   = 2.00e-9
         xl(iref(p_so2-1),7:7)   = 2.00e-9
         xl(iref(p_so2-1),6:6)   = 5.00e-9
         xl(iref(p_so2-1),5:5)   = 4.00e-9
         xl(iref(p_so2-1),4:4)   = 5.00e-9
         xl(iref(p_so2-1),3:3)   = 1.00e-9
         xl(iref(p_so2-1),2:2)   = 2.00e-9
         xl(iref(p_so2-1),1:1)   = 5.00e-9
      end if
      if( p_no2 > 1 ) then
         xl(iref(p_no2-1),12:16) = 5.00e-9
         xl(iref(p_no2-1),11:11) = 5.00e-9
         xl(iref(p_no2-1),10:10) = 5.0e-9
         xl(iref(p_no2-1),9:9)   = 5.0e-9
         xl(iref(p_no2-1),8:8)   = 5.0e-9
         xl(iref(p_no2-1),7:7)   = 5.0e-9
         xl(iref(p_no2-1),6:6)   = 5.0e-9
         xl(iref(p_no2-1),5:5)   = 5.0e-9
         xl(iref(p_no2-1),4:4)   = 5.0e-9
         xl(iref(p_no2-1),3:3)   = 5.0e-9
         xl(iref(p_no2-1),2:2)   = 2.50e-10
         xl(iref(p_no2-1),1:1)   = 2.80e-10
      end if
      if( p_h2o2 > 1 ) then
         xl(iref(p_h2o2-1),12:16) = 0.18e-9
         xl(iref(p_h2o2-1),11:11) = 0.13e-9
         xl(iref(p_h2o2-1),10:10) = 0.30e-9
         xl(iref(p_h2o2-1),9:9)   = 0.77e-9
         xl(iref(p_h2o2-1),8:8)   = 0.30e-9
         xl(iref(p_h2o2-1),7:7)   = 0.25e-9
         xl(iref(p_h2o2-1),6:6)   = 0.21e-9
         xl(iref(p_h2o2-1),5:5)   = 0.23e-9
         xl(iref(p_h2o2-1),4:4)   = 0.24e-9
         xl(iref(p_h2o2-1),3:3)   = 0.36e-9
         xl(iref(p_h2o2-1),2:2)   = 0.15e-10
         xl(iref(p_h2o2-1),1:1)   = 0.17e-10
      end if

      if( p_hno3 > 1 ) then
         xl(iref(p_hno3-1),12:16) = 2.0e-10
         xl(iref(p_hno3-1),11:11) = 2.0e-10
         xl(iref(p_hno3-1),10:10) = 2.0e-10
         xl(iref(p_hno3-1),9:9)   = 2.0e-10
         xl(iref(p_hno3-1),8:8)   = 2.0e-10
         xl(iref(p_hno3-1),7:7)   = 2.0e-10
         xl(iref(p_hno3-1),6:6)   = 2.0e-10
         xl(iref(p_hno3-1),5:5)   = 2.0e-10
         xl(iref(p_hno3-1),4:4)   = 2.0e-10
         xl(iref(p_hno3-1),3:3)   = 0.57e-10
         xl(iref(p_hno3-1),2:2)   = 0.17e-9
         xl(iref(p_hno3-1),1:1)   = 0.12e-9
      end if
      if( p_no3 > 1 ) then
         xl(iref(p_no3-1),12:16) = 0.98e-11
         xl(iref(p_no3-1),11:11) = 0.58e-11
         xl(iref(p_no3-1),10:10) = 0.53e-11
         xl(iref(p_no3-1),9:9)   = 0.53e-11
         xl(iref(p_no3-1),8:8)   = 0.68e-11
         xl(iref(p_no3-1),7:7)   = 0.25e-11
         xl(iref(p_no3-1),6:6)   = 0.82e-11
         xl(iref(p_no3-1),5:5)   = 0.21e-11
         xl(iref(p_no3-1),4:4)   = 0.39e-11
         xl(iref(p_no3-1),3:3)   = 0.57e-10
         xl(iref(p_no3-1),2:2)   = 0.17e-9
         xl(iref(p_no3-1),1:1)   = 0.12e-9
      end if

      if( p_n2o5 > 1 ) then
         xl(iref(p_n2o5-1),12:16) = 0.12e-14
         xl(iref(p_n2o5-1),11:11) = 0.11e-12
         xl(iref(p_n2o5-1),10:10) = 0.20e-12
         xl(iref(p_n2o5-1),9:9)   = 0.25e-12
         xl(iref(p_n2o5-1),8:8)   = 0.98e-12
         xl(iref(p_n2o5-1),7:7)   = 0.12e-11
         xl(iref(p_n2o5-1),6:6)   = 0.13e-11
         xl(iref(p_n2o5-1),5:5)   = 0.45e-12
         xl(iref(p_n2o5-1),4:4)   = 0.38e-12
         xl(iref(p_n2o5-1),3:3)   = 0.94e-12
         xl(iref(p_n2o5-1),2:2)   = 0.16e-11
         xl(iref(p_n2o5-1),1:1)   = 0.11e-11
      end if
      if( p_nh3 > 1 ) then
         xl(iref(p_nh3-1),12:16) = 0.14e-9
         xl(iref(p_nh3-1),11:11) = 0.60e-10
         xl(iref(p_nh3-1),10:10) = 0.30e-10
         xl(iref(p_nh3-1),9:9)   = 0.24e-10
         xl(iref(p_nh3-1),8:8)   = 0.49e-11
         xl(iref(p_nh3-1),7:7)   = 0.19e-11
         xl(iref(p_nh3-1),6:6)   = 0.41e-11
         xl(iref(p_nh3-1),5:5)   = 0.11e-11
         xl(iref(p_nh3-1),4:4)   = 0.52e-11
         xl(iref(p_nh3-1),3:3)   = 0.35e-10
         xl(iref(p_nh3-1),2:2)   = 0.86e-11
         xl(iref(p_nh3-1),1:1)   = 0.84e-11
      end if
      if( p_hcho > 1 ) then
         xl(iref(p_hcho-1),12:16) = 2.00e-9
         xl(iref(p_hcho-1),11:11) = 2.0e-9
         xl(iref(p_hcho-1),10:10) = 2.0e-9
         xl(iref(p_hcho-1),9:9)   = 2.00e-9
         xl(iref(p_hcho-1),8:8)   = 2.00e-9
         xl(iref(p_hcho-1),7:7)   = 2.00e-9
         xl(iref(p_hcho-1),6:6)   = 2.00e-9
         xl(iref(p_hcho-1),5:5)   = 2.00e-9
         xl(iref(p_hcho-1),4:4)   = 2.00e-9
         xl(iref(p_hcho-1),3:3)   = 2.00e-9
         xl(iref(p_hcho-1),2:2)   = 2.00e-9
         xl(iref(p_hcho-1),1:1)   = 1.40e-11
      end if
      if( p_ket > 1 ) then
         xl(iref(p_ket-1),12:16) = 0.19e-9
         xl(iref(p_ket-1),11:11) = 0.15e-9
         xl(iref(p_ket-1),10:10) = 0.13e-9
         xl(iref(p_ket-1),9:9)   = 0.98e-10
         xl(iref(p_ket-1),8:8)   = 0.54e-10
         xl(iref(p_ket-1),7:7)   = 0.46e-10
         xl(iref(p_ket-1),6:6)   = 0.66e-10
         xl(iref(p_ket-1),5:5)   = 0.64e-10
         xl(iref(p_ket-1),4:4)   = 0.68e-10
         xl(iref(p_ket-1),3:3)   = 0.63e-10
         xl(iref(p_ket-1),2:2)   = 0.91e-11
         xl(iref(p_ket-1),1:1)   = 0.55e-11
      end if
      if( p_par > 1 ) then
         xl(iref(p_par-1),12:16) = 0.75e-10+0.68e-10
         xl(iref(p_par-1),11:11) = 0.50e-10+0.21e-10
         xl(iref(p_par-1),10:10) = 0.41e-10+0.11e-10
         xl(iref(p_par-1),9:9)   = 0.37e-10+0.17e-10
         xl(iref(p_par-1),8:8)   = 0.24e-10+0.53e-11
         xl(iref(p_par-1),7:7)   = 0.22e-10+0.45e-11
         xl(iref(p_par-1),6:6)   = 0.31e-10+0.79e-11
         xl(iref(p_par-1),5:5)   = 0.28e-10+0.22e-11
         xl(iref(p_par-1),4:4)   = 0.29e-10+0.26e-11
         xl(iref(p_par-1),3:3)   = 0.29e-10+0.43e-11
         xl(iref(p_par-1),2:2)   = 0.16e-10+0.18e-12
         xl(iref(p_par-1),1:1)   = 0.17e-10+0.26e-12
      end if
      if( p_olt > 1 ) then
         xl(iref(p_olt-1),12:16) = 0.54e-13
         xl(iref(p_olt-1),11:11) = 0.52e-13
         xl(iref(p_olt-1),10:10) = 0.33e-13
         xl(iref(p_olt-1),9:9)   = 0.16e-13
         xl(iref(p_olt-1),8:8)   = 0.20e-14
         xl(iref(p_olt-1),7:7)   = 0.14e-14
         xl(iref(p_olt-1),6:6)   = 0.37e-14
         xl(iref(p_olt-1),5:5)   = 0.26e-15
         xl(iref(p_olt-1),4:4)   = 0.40e-15
         xl(iref(p_olt-1),3:3)   = 0.15e-14
         xl(iref(p_olt-1),2:2)   = 0.33e-17
         xl(iref(p_olt-1),1:1)   = 0.27e-15
      end if
      if( p_ol2 > 1 ) then
         xl(iref(p_ol2-1),12:16) = 0.87e-12
         xl(iref(p_ol2-1),11:11) = 0.10e-11
         xl(iref(p_ol2-1),10:10) = 0.68e-12
         xl(iref(p_ol2-1),9:9)   = 0.38e-12
         xl(iref(p_ol2-1),8:8)   = 0.38e-12
         xl(iref(p_ol2-1),7:7)   = 0.45e-13
         xl(iref(p_ol2-1),6:6)   = 0.58e-13
         xl(iref(p_ol2-1),5:5)   = 0.52e-14
         xl(iref(p_ol2-1),4:4)   = 0.12e-13
         xl(iref(p_ol2-1),3:3)   = 0.66e-13
         xl(iref(p_ol2-1),2:2)   = 0.14e-14
         xl(iref(p_ol2-1),1:1)   = 0.51e-13
      end if
      if( p_ald > 1 ) then
         xl(iref(p_ald-1),12:16) = 0.30e-10
         xl(iref(p_ald-1),11:11) = 0.28e-10
         xl(iref(p_ald-1),10:10) = 0.22e-10
         xl(iref(p_ald-1),9:9)   = 0.16e-10
         xl(iref(p_ald-1),8:8)   = 0.10e-10
         xl(iref(p_ald-1),7:7)   = 0.83e-11
         xl(iref(p_ald-1),6:6)   = 0.89e-11
         xl(iref(p_ald-1),5:5)   = 0.93e-11
         xl(iref(p_ald-1),4:4)   = 0.98e-11
         xl(iref(p_ald-1),3:3)   = 0.10e-10
         xl(iref(p_ald-1),2:2)   = 0.93e-12
         xl(iref(p_ald-1),1:1)   = 0.64e-12
      end if
      if( p_tol > 1 ) then
         xl(iref(p_tol-1),12:16) = 0.29e-11/2.0
         xl(iref(p_tol-1),11:11) = 0.16e-11/2.0
         xl(iref(p_tol-1),10:10) = 0.12e-11/2.0
         xl(iref(p_tol-1),9:9)   = 0.97e-12/2.0
         xl(iref(p_tol-1),8:8)   = 0.16e-12/2.0
         xl(iref(p_tol-1),7:7)   = 0.13e-12/2.0
         xl(iref(p_tol-1),6:6)   = 0.13e-12/2.0
         xl(iref(p_tol-1),5:5)   = 0.28e-13/2.0
         xl(iref(p_tol-1),4:4)   = 0.42e-13/2.0
         xl(iref(p_tol-1),3:3)   = 0.12e-12/2.0
         xl(iref(p_tol-1),2:2)   = 0.39e-15/2.0
         xl(iref(p_tol-1),1:1)   = 0.11e-14/2.0
      end if
      if( p_xyl > 1 ) then
         xl(iref(p_xyl-1),12:16) = 0.29e-11/2.0
         xl(iref(p_xyl-1),11:11) = 0.16e-11/2.0
         xl(iref(p_xyl-1),10:10) = 0.12e-11/2.0
         xl(iref(p_xyl-1),9:9)   = 0.97e-12/2.0
         xl(iref(p_xyl-1),8:8)   = 0.16e-12/2.0
         xl(iref(p_xyl-1),7:7)   = 0.13e-12/2.0
         xl(iref(p_xyl-1),6:6)   = 0.13e-12/2.0
         xl(iref(p_xyl-1),5:5)   = 0.28e-13/2.0
         xl(iref(p_xyl-1),4:4)   = 0.42e-13/2.0
         xl(iref(p_xyl-1),3:3)   = 0.12e-12/2.0
         xl(iref(p_xyl-1),2:2)   = 0.39e-15/2.0
         xl(iref(p_xyl-1),1:1)   = 0.11e-14/2.0
      end if
      if( p_eth > 1 ) then
         xl(iref(p_eth-1),12:16) = 0.60e-9
         xl(iref(p_eth-1),11:11) = 0.63e-9
         xl(iref(p_eth-1),10:10) = 0.49e-9
         xl(iref(p_eth-1),9:9)   = 0.51e-9
         xl(iref(p_eth-1),8:8)   = 0.44e-9
         xl(iref(p_eth-1),7:7)   = 0.43e-9
         xl(iref(p_eth-1),6:6)   = 0.45e-9
         xl(iref(p_eth-1),5:5)   = 0.47e-9
         xl(iref(p_eth-1),4:4)   = 0.48e-9
         xl(iref(p_eth-1),3:3)   = 0.47e-9
         xl(iref(p_eth-1),2:2)   = 0.38e-9
         xl(iref(p_eth-1),1:1)   = 0.36e-9
      end if

#endif

#if (CASENAME == 2)
      if( p_o3 > 1 ) then
         xl(iref(p_o3-1),12:16) = 2.50e-8
         xl(iref(p_o3-1),11:11) = 3.10e-8
         xl(iref(p_o3-1),10:10) = 3.20e-8
         xl(iref(p_o3-1),9:9)   = 3.60e-8
         xl(iref(p_o3-1),8:8)   = 4.20e-8
         xl(iref(p_o3-1),7:7)   = 4.20e-8
         xl(iref(p_o3-1),6:6)   = 4.20e-8
         xl(iref(p_o3-1),5:5)   = 3.80e-8
         xl(iref(p_o3-1),4:4)   = 3.80e-8
         xl(iref(p_o3-1),3:3)   = 5.80e-8
         xl(iref(p_o3-1),2:2)   = 1.04e-7
         xl(iref(p_o3-1),1:1)   = 2.00e-7
      end if
      if( p_co > 1 ) then
         xl(iref(p_co-1),12:16) = 1.00e-7
         xl(iref(p_co-1),11:11) = 1.00e-7
         xl(iref(p_co-1),10:10) = 9.80e-8
         xl(iref(p_co-1),9:9)   = 8.80e-8
         xl(iref(p_co-1),8:8)   = 7.80e-8
         xl(iref(p_co-1),7:7)   = 7.70e-8
         xl(iref(p_co-1),6:6)   = 8.00e-8
         xl(iref(p_co-1),5:5)   = 8.20e-8
         xl(iref(p_co-1),4:4)   = 8.20e-8
         xl(iref(p_co-1),3:3)   = 8.20e-8
         xl(iref(p_co-1),2:2)   = 6.50e-8
         xl(iref(p_co-1),1:1)   = 6.10e-8
      end if
      if( p_so2 > 1 ) then
         xl(iref(p_so2-1),12:16) = 6.00e-11
         xl(iref(p_so2-1),11:11) = 1.00e-11
         xl(iref(p_so2-1),10:10) = 4.00e-12
         xl(iref(p_so2-1),9:9)   = 5.00e-12
         xl(iref(p_so2-1),8:8)   = 2.00e-12
         xl(iref(p_so2-1),7:7)   = 2.00e-12
         xl(iref(p_so2-1),6:6)   = 5.00e-12
         xl(iref(p_so2-1),5:5)   = 4.00e-12
         xl(iref(p_so2-1),4:4)   = 5.00e-12
         xl(iref(p_so2-1),3:3)   = 1.00e-11
         xl(iref(p_so2-1),2:2)   = 2.00e-12
         xl(iref(p_so2-1),1:1)   = 5.00e-13
      end if
      if( p_no2 > 1 ) then
         xl(iref(p_no2-1),12:16) = 1.00e-11
         xl(iref(p_no2-1),11:11) = 1.50e-11
         xl(iref(p_no2-1),10:10) = 1.80e-11
         xl(iref(p_no2-1),9:9)   = 1.90e-11
         xl(iref(p_no2-1),8:8)   = 2.80e-11
         xl(iref(p_no2-1),7:7)   = 3.20e-11
         xl(iref(p_no2-1),6:6)   = 2.60e-11
         xl(iref(p_no2-1),5:5)   = 1.80e-11
         xl(iref(p_no2-1),4:4)   = 1.70e-11
         xl(iref(p_no2-1),3:3)   = 4.30e-11
         xl(iref(p_no2-1),2:2)   = 2.50e-10
         xl(iref(p_no2-1),1:1)   = 2.80e-10
      end if
      if( p_h2o2 > 1 ) then
         xl(iref(p_h2o2-1),12:16) = 0.18e-8
         xl(iref(p_h2o2-1),11:11) = 0.13e-8
         xl(iref(p_h2o2-1),10:10) = 0.30e-9
         xl(iref(p_h2o2-1),9:9)   = 0.77e-9
         xl(iref(p_h2o2-1),8:8)   = 0.30e-9
         xl(iref(p_h2o2-1),7:7)   = 0.25e-9
         xl(iref(p_h2o2-1),6:6)   = 0.21e-9
         xl(iref(p_h2o2-1),5:5)   = 0.23e-9
         xl(iref(p_h2o2-1),4:4)   = 0.24e-9
         xl(iref(p_h2o2-1),3:3)   = 0.36e-9
         xl(iref(p_h2o2-1),2:2)   = 0.15e-10
         xl(iref(p_h2o2-1),1:1)   = 0.17e-10
      end if
      if( p_hno3 > 1 ) then
         xl(iref(p_hno3-1),12:16) = 0.98e-11
         xl(iref(p_hno3-1),11:11) = 0.58e-11
         xl(iref(p_hno3-1),10:10) = 0.53e-11
         xl(iref(p_hno3-1),9:9)   = 0.53e-11
         xl(iref(p_hno3-1),8:8)   = 0.67e-11
         xl(iref(p_hno3-1),7:7)   = 0.82e-11
         xl(iref(p_hno3-1),6:6)   = 0.82e-11
         xl(iref(p_hno3-1),5:5)   = 0.21e-11
         xl(iref(p_hno3-1),4:4)   = 0.39e-11
         xl(iref(p_hno3-1),3:3)   = 0.57e-10
         xl(iref(p_hno3-1),2:2)   = 0.17e-9
         xl(iref(p_hno3-1),1:1)   = 0.12e-9
      end if
      if( p_no3 > 1 ) then
         xl(iref(p_no3-1),12:16) = 0.98e-11
         xl(iref(p_no3-1),11:11) = 0.58e-11
         xl(iref(p_no3-1),10:10) = 0.53e-11
         xl(iref(p_no3-1),9:9)   = 0.53e-11
         xl(iref(p_no3-1),8:8)   = 0.68e-11
         xl(iref(p_no3-1),7:7)   = 0.25e-11
         xl(iref(p_no3-1),6:6)   = 0.82e-11
         xl(iref(p_no3-1),5:5)   = 0.21e-11
         xl(iref(p_no3-1),4:4)   = 0.39e-11
         xl(iref(p_no3-1),3:3)   = 0.57e-10
         xl(iref(p_no3-1),2:2)   = 0.17e-9
         xl(iref(p_no3-1),1:1)   = 0.12e-9
      end if
      if( p_n2o5 > 1 ) then
         xl(iref(p_n2o5-1),12:16) = 0.12e-14
         xl(iref(p_n2o5-1),11:11) = 0.11e-12
         xl(iref(p_n2o5-1),10:10) = 0.20e-12
         xl(iref(p_n2o5-1),9:9)   = 0.25e-12
         xl(iref(p_n2o5-1),8:8)   = 0.98e-12
         xl(iref(p_n2o5-1),7:7)   = 0.12e-11
         xl(iref(p_n2o5-1),6:6)   = 0.13e-11
         xl(iref(p_n2o5-1),5:5)   = 0.45e-12
         xl(iref(p_n2o5-1),4:4)   = 0.38e-12
         xl(iref(p_n2o5-1),3:3)   = 0.94e-12
         xl(iref(p_n2o5-1),2:2)   = 0.16e-11
         xl(iref(p_n2o5-1),1:1)   = 0.11e-11
      end if
      if( p_nh3 > 1 ) then
         xl(iref(p_nh3-1),12:16) = 0.14e-9
         xl(iref(p_nh3-1),11:11) = 0.60e-10
         xl(iref(p_nh3-1),10:10) = 0.30e-10
         xl(iref(p_nh3-1),9:9)   = 0.24e-10
         xl(iref(p_nh3-1),8:8)   = 0.49e-11
         xl(iref(p_nh3-1),7:7)   = 0.19e-11
         xl(iref(p_nh3-1),6:6)   = 0.41e-11
         xl(iref(p_nh3-1),5:5)   = 0.11e-11
         xl(iref(p_nh3-1),4:4)   = 0.52e-11
         xl(iref(p_nh3-1),3:3)   = 0.35e-10
         xl(iref(p_nh3-1),2:2)   = 0.86e-11
         xl(iref(p_nh3-1),1:1)   = 0.84e-11
      end if
      if( p_hcho > 1 ) then
         xl(iref(p_hcho-1),12:16) = 4.00e-10
         xl(iref(p_hcho-1),11:11) = 4.20e-10
         xl(iref(p_hcho-1),10:10) = 3.60e-10
         xl(iref(p_hcho-1),9:9)   = 3.00e-10
         xl(iref(p_hcho-1),8:8)   = 1.80e-10
         xl(iref(p_hcho-1),7:7)   = 1.40e-10
         xl(iref(p_hcho-1),6:6)   = 7.60e-11
         xl(iref(p_hcho-1),5:5)   = 6.20e-11
         xl(iref(p_hcho-1),4:4)   = 5.20e-11
         xl(iref(p_hcho-1),3:3)   = 8.80e-11
         xl(iref(p_hcho-1),2:2)   = 1.30e-11
         xl(iref(p_hcho-1),1:1)   = 1.40e-11
      end if
      if( p_ket > 1 ) then
         xl(iref(p_ket-1),12:16) = 0.19e-9
         xl(iref(p_ket-1),11:11) = 0.15e-9
         xl(iref(p_ket-1),10:10) = 0.13e-9
         xl(iref(p_ket-1),9:9)   = 0.98e-10
         xl(iref(p_ket-1),8:8)   = 0.54e-10
         xl(iref(p_ket-1),7:7)   = 0.46e-10
         xl(iref(p_ket-1),6:6)   = 0.66e-10
         xl(iref(p_ket-1),5:5)   = 0.64e-10
         xl(iref(p_ket-1),4:4)   = 0.68e-10
         xl(iref(p_ket-1),3:3)   = 0.63e-10
         xl(iref(p_ket-1),2:2)   = 0.91e-11
         xl(iref(p_ket-1),1:1)   = 0.55e-11
      end if
      if( p_par > 1 ) then
         xl(iref(p_par-1),12:16) = 0.75e-10+0.68e-10
         xl(iref(p_par-1),11:11) = 0.50e-10+0.21e-10
         xl(iref(p_par-1),10:10) = 0.41e-10+0.11e-10
         xl(iref(p_par-1),9:9)   = 0.37e-10+0.17e-10
         xl(iref(p_par-1),8:8)   = 0.24e-10+0.53e-11
         xl(iref(p_par-1),7:7)   = 0.22e-10+0.45e-11
         xl(iref(p_par-1),6:6)   = 0.31e-10+0.79e-11
         xl(iref(p_par-1),5:5)   = 0.28e-10+0.22e-11
         xl(iref(p_par-1),4:4)   = 0.29e-10+0.26e-11
         xl(iref(p_par-1),3:3)   = 0.29e-10+0.43e-11
         xl(iref(p_par-1),2:2)   = 0.16e-10+0.18e-12
         xl(iref(p_par-1),1:1)   = 0.17e-10+0.26e-12
      end if
      if( p_olt > 1 ) then
         xl(iref(p_olt-1),12:16) = 0.54e-13
         xl(iref(p_olt-1),11:11) = 0.52e-13
         xl(iref(p_olt-1),10:10) = 0.33e-13
         xl(iref(p_olt-1),9:9)   = 0.16e-13
         xl(iref(p_olt-1),8:8)   = 0.20e-14
         xl(iref(p_olt-1),7:7)   = 0.14e-14
         xl(iref(p_olt-1),6:6)   = 0.37e-14
         xl(iref(p_olt-1),5:5)   = 0.26e-15
         xl(iref(p_olt-1),4:4)   = 0.40e-15
         xl(iref(p_olt-1),3:3)   = 0.15e-14
         xl(iref(p_olt-1),2:2)   = 0.33e-17
         xl(iref(p_olt-1),1:1)   = 0.27e-15
      end if
      if( p_ol2 > 1 ) then
         xl(iref(p_ol2-1),12:16) = 0.87e-12
         xl(iref(p_ol2-1),11:11) = 0.10e-11
         xl(iref(p_ol2-1),10:10) = 0.68e-12
         xl(iref(p_ol2-1),9:9)   = 0.38e-12
         xl(iref(p_ol2-1),8:8)   = 0.38e-12
         xl(iref(p_ol2-1),7:7)   = 0.45e-13
         xl(iref(p_ol2-1),6:6)   = 0.58e-13
         xl(iref(p_ol2-1),5:5)   = 0.52e-14
         xl(iref(p_ol2-1),4:4)   = 0.12e-13
         xl(iref(p_ol2-1),3:3)   = 0.66e-13
         xl(iref(p_ol2-1),2:2)   = 0.14e-14
         xl(iref(p_ol2-1),1:1)   = 0.51e-13
      end if
      if( p_ald > 1 ) then
         xl(iref(p_ald-1),12:16) = 0.30e-10
         xl(iref(p_ald-1),11:11) = 0.28e-10
         xl(iref(p_ald-1),10:10) = 0.22e-10
         xl(iref(p_ald-1),9:9)   = 0.16e-10
         xl(iref(p_ald-1),8:8)   = 0.10e-10
         xl(iref(p_ald-1),7:7)   = 0.83e-11
         xl(iref(p_ald-1),6:6)   = 0.89e-11
         xl(iref(p_ald-1),5:5)   = 0.93e-11
         xl(iref(p_ald-1),4:4)   = 0.98e-11
         xl(iref(p_ald-1),3:3)   = 0.10e-10
         xl(iref(p_ald-1),2:2)   = 0.93e-12
         xl(iref(p_ald-1),1:1)   = 0.64e-12
      end if
      if( p_tol > 1 ) then
         xl(iref(p_tol-1),12:16) = 0.29e-11/2.0
         xl(iref(p_tol-1),11:11) = 0.16e-11/2.0
         xl(iref(p_tol-1),10:10) = 0.12e-11/2.0
         xl(iref(p_tol-1),9:9)   = 0.97e-12/2.0
         xl(iref(p_tol-1),8:8)   = 0.16e-12/2.0
         xl(iref(p_tol-1),7:7)   = 0.13e-12/2.0
         xl(iref(p_tol-1),6:6)   = 0.13e-12/2.0
         xl(iref(p_tol-1),5:5)   = 0.28e-13/2.0
         xl(iref(p_tol-1),4:4)   = 0.42e-13/2.0
         xl(iref(p_tol-1),3:3)   = 0.12e-12/2.0
         xl(iref(p_tol-1),2:2)   = 0.39e-15/2.0
         xl(iref(p_tol-1),1:1)   = 0.11e-14/2.0
      end if
      if( p_xyl > 1 ) then
         xl(iref(p_xyl-1),12:16) = 0.29e-11/2.0
         xl(iref(p_xyl-1),11:11) = 0.16e-11/2.0
         xl(iref(p_xyl-1),10:10) = 0.12e-11/2.0
         xl(iref(p_xyl-1),9:9)   = 0.97e-12/2.0
         xl(iref(p_xyl-1),8:8)   = 0.16e-12/2.0
         xl(iref(p_xyl-1),7:7)   = 0.13e-12/2.0
         xl(iref(p_xyl-1),6:6)   = 0.13e-12/2.0
         xl(iref(p_xyl-1),5:5)   = 0.28e-13/2.0
         xl(iref(p_xyl-1),4:4)   = 0.42e-13/2.0
         xl(iref(p_xyl-1),3:3)   = 0.12e-12/2.0
         xl(iref(p_xyl-1),2:2)   = 0.39e-15/2.0
         xl(iref(p_xyl-1),1:1)   = 0.11e-14/2.0
      end if
      if( p_eth > 1 ) then
         xl(iref(p_eth-1),12:16) = 0.60e-9
         xl(iref(p_eth-1),11:11) = 0.63e-9
         xl(iref(p_eth-1),10:10) = 0.49e-9
         xl(iref(p_eth-1),9:9)   = 0.51e-9
         xl(iref(p_eth-1),8:8)   = 0.44e-9
         xl(iref(p_eth-1),7:7)   = 0.43e-9
         xl(iref(p_eth-1),6:6)   = 0.45e-9
         xl(iref(p_eth-1),5:5)   = 0.47e-9
         xl(iref(p_eth-1),4:4)   = 0.48e-9
         xl(iref(p_eth-1),3:3)   = 0.47e-9
         xl(iref(p_eth-1),2:2)   = 0.38e-9
         xl(iref(p_eth-1),1:1)   = 0.36e-9
      end if
#endif

#endif

#if (CASENAME == 1)
! so2 profile based on mirage 2 output, used for neaqs case, 7-20-05 egc
! decreased by one magnitude, 27-oct-2005 wig
      if( p_so2 > 1 ) then
         xl(iref(p_so2-1), 1:2) = 0.035e-10
         xl(iref(p_so2-1),   3) = 0.081e-10
         xl(iref(p_so2-1), 4:8) = 0.10e-10
         xl(iref(p_so2-1),   9) = 0.60e-10
         xl(iref(p_so2-1), 10) = 1.1e-10
         xl(iref(p_so2-1), 11) = 1.46e-10
         xl(iref(p_so2-1), 12) = 1.74e-10
         xl(iref(p_so2-1), 13) = 1.94e-10
         xl(iref(p_so2-1), 14) = 2.80e-10
         xl(iref(p_so2-1), 15:16) = 3.0e-10
      end if
#endif

      if( p_sulf > 1 ) then
         xl(iref(p_sulf-1),:)   = conmin
      end if

    end SUBROUTINE gasprofile_init_pnnl

#ifdef CHEM_DBG_I
!-----------------------------------------------------------------------
subroutine chem_dbg(i,j,k,dtstep,itimestep,                           &
     dz8w,t_phy,p_phy,rho_phy,chem,                                   &
     emis_ant,                                                        &
     ids,ide, jds,jde, kds,kde,                                       &
     ims,ime, jms,jme, kms,kme,                                       &
     its,ite, jts,jte, kts,kte,                                       &
     kemit,                                                           &
     ph_macr,ph_o31d,ph_o33p,ph_no2,                                  &
     ph_cl2,ph_hocl,ph_clno2,ph_fmcl,                                 &
     ph_no3o2,ph_no3o,ph_hno2,                                        &
     ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,             &
     ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,                  &
     ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,ph_n2o5, &
     ph_o2                                                            )

  IMPLICIT NONE
  INTEGER,      INTENT(IN   ) :: i,j,k,                        &
                                 ids,ide, jds,jde, kds,kde,    &
                                 ims,ime, jms,jme, kms,kme,    &
                                 its,ite, jts,jte, kts,kte,    &
                                 kemit
  real,         intent(in   ) :: dtstep
  integer,      intent(in   ) :: itimestep
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
       INTENT(INOUT ) :: chem
  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),    &
       INTENT(IN   ) ::  dz8w,t_phy,p_phy,rho_phy
  REAL, DIMENSION( ims:ime, kms:kemit, jms:jme,num_emis_ant ),          &
       INTENT(IN ) ::                                                   &
                emis_ant
  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                      &
       INTENT(IN   ), OPTIONAL ::                                       &
       ph_macr,ph_o31d,ph_o33p,ph_no2,                                  &
       ph_cl2,ph_hocl,ph_clno2,ph_fmcl,                                 &
       ph_no3o2,ph_no3o,ph_hno2,                                        &
       ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,             &
       ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,                  &
       ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,ph_n2o5, &
       ph_o2

  integer :: n
  real :: conva,convg

  print*,"itimestep =",itimestep
  print*,"MET DATA AT (i,k,j):",i,k,j
  print*,"t_phy,p_phy,rho_phy=",t_phy(i,k,j),p_phy(i,k,j),rho_phy(i,k,j)

  if(dz8w(i,k,j) /= 0.) then
     conva = dtstep/(dz8w(i,k,j)*60.)
     convg = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
     print*,"ADJUSTED EMISSIONS (PPM) AT (i,k,j):",i,k,j
     print*,"dtstep,dz8w(i,k,j):",dtstep,dz8w(i,k,j)
     print*,"e_pm25 i,j:",emis_ant(i,k,j,p_e_pm25i)*conva, &
          emis_ant(i,k,j,p_e_pm25j)*conva
     print*,"e_ec i,j:",emis_ant(i,k,j,p_e_eci)*conva, &
          emis_ant(i,k,j,p_e_ecj)*conva
     print*,"e_org i,j:",emis_ant(i,k,j,p_e_orgi)*conva, &
          emis_ant(i,k,j,p_e_orgj)*conva
     print*,"e_so2:",emis_ant(i,k,j,p_e_so2)*convg
     print*,"e_no:",emis_ant(i,k,j,p_e_no)*convg
     print*,"e_co:",emis_ant(i,k,j,p_e_co)*convg
     print*,"e_eth:",emis_ant(i,k,j,p_e_eth)*convg
     print*,"e_hc3:",emis_ant(i,k,j,p_e_hc3)*convg
     print*,"e_hc5:",emis_ant(i,k,j,p_e_hc5)*convg
     print*,"e_hc8:",emis_ant(i,k,j,p_e_hc8)*convg
     print*,"e_xyl:",emis_ant(i,k,j,p_e_xyl)*convg
     print*,"e_ol2:",emis_ant(i,k,j,p_e_ol2)*convg
     print*,"e_olt:",emis_ant(i,k,j,p_e_olt)*convg
     print*,"e_oli:",emis_ant(i,k,j,p_e_oli)*convg
     print*,"e_tol:",emis_ant(i,k,j,p_e_tol)*convg
     print*,"e_csl:",emis_ant(i,k,j,p_e_csl)*convg
     print*,"e_hcho:",emis_ant(i,k,j,p_e_hcho)*convg
     print*,"e_ald:",emis_ant(i,k,j,p_e_ald)*convg
     print*,"e_ket:",emis_ant(i,k,j,p_e_ket)*convg
     print*,"e_ora2:",emis_ant(i,k,j,p_e_ora2)*convg
     print*,"e_pm25:",emis_ant(i,k,j,p_e_pm_25)*conva
     print*,"e_pm10:",emis_ant(i,k,j,p_e_pm_10)*conva
     print*,"e_nh3:",emis_ant(i,k,j,p_e_nh3)*convg
     print*,"e_no2:",emis_ant(i,k,j,p_e_no2)*convg
     print*,"e_ch3oh:",emis_ant(i,k,j,p_e_ch3oh)*convg
     print*,"e_c2h5oh:",emis_ant(i,k,j,p_e_c2h5oh)*convg
     print*,"e_iso:",emis_ant(i,k,j,p_e_iso)*convg
     print*,"e_so4 f,c:",emis_ant(i,k,j,p_e_so4j)*conva
     print*,"e_no3 f,c:",emis_ant(i,k,j,p_e_no3j)*conva
     print*,"e_orgc:",emis_ant(i,k,j,p_e_orgc)*conva
     print*,"e_ecc:",emis_ant(i,k,j,p_e_ecc)*conva
     print*
  else
     print*,"dz8w=0 so cannot show adjusted emissions"
  end if
  print*,"CHEM_DBG PRINT (PPM or ug/m^3) AT (i,k,j):",i,k,j
  do n=1,num_chem
     print*,n,chem(i,k,j,n)
  end do
  if( present(ph_macr) ) then
     print*,"PHOTOLYSIS DATA:"
     print*,"ph_macr:",ph_macr(i,:,j)
     print*,"ph_o31d:",ph_o31d(i,:,j)
     print*,"ph_o33p:",ph_o33p(i,:,j)
     print*,"ph_no2:",ph_no2(i,:,j)
     print*,"ph_no3o2:",ph_no3o2(i,:,j)
     print*,"ph_no3o:",ph_no3o(i,:,j)
     print*,"ph_hno2:",ph_hno2(i,:,j)
     print*,"ph_hno3:",ph_hno3(i,:,j)
     print*,"ph_hno4:",ph_hno4(i,:,j)
     print*,"ph_h2o2:",ph_h2o2(i,:,j)
     print*,"ph_ch2or:",ph_ch2or(i,:,j)
     print*,"ph_ch2om:",ph_ch2om(i,:,j)
     print*,"ph_ch3cho:",ph_ch3cho(i,:,j)
     print*,"ph_ch3coch3:",ph_ch3coch3(i,:,j)
     print*,"ph_ch3coc2h5:",ph_ch3coc2h5(i,:,j)
     print*,"ph_hcocho:",ph_hcocho(i,:,j)
     print*,"ph_ch3cocho:",ph_ch3cocho(i,:,j)
     print*,"ph_hcochest:",ph_hcochest(i,:,j)
     print*,"ph_ch3o2h:",ph_ch3o2h(i,:,j)
     print*,"ph_ch3coo2h:",ph_ch3coo2h(i,:,j)
     print*,"ph_ch3ono2:",ph_ch3ono2(i,:,j)
     print*,"ph_hcochob:",ph_hcochob(i,:,j)
     print*,"ph_n2o5:",ph_n2o5(i,:,j)
     print*,"ph_o2:",ph_o2(i,:,j)
  end if
  print*
end subroutine chem_dbg
#endif

SUBROUTINE mozcart_lbc_init( chem, num_chem,  id, &
                             ims, ime, jms, jme, kms, kme,    &
                             its, ite, jts, jte, kts )

    USE module_state_description, only : p_ch4, p_n2o, p_h2

integer, intent(in) :: id
integer, intent(in) :: num_chem
integer, intent(in) :: ims, ime, jms, jme, kms, kme,    &
                       its, ite, jts, jte, kts
real, intent(in)    :: chem(ims:ime,kms:kme,jms:jme,num_chem)

   integer           :: astat
   integer           :: max_dom
   character(len=128) :: err_msg

   if( id == 1 .and. .not. allocated(fixed_lbc) ) then
     CALL nl_get_max_dom( 1,max_dom )
     allocate( fixed_lbc(max_dom),stat=astat )
     if( astat /= 0 ) then
       CALL wrf_message( 'lbc_init: failed to allocate lbc_concentration type fix_lbc' )
       CALL wrf_abort
     end if
     write(err_msg,*) 'lbc_init: initializing ',max_dom,' domains'
     call wrf_message( trim(err_msg) )
     fixed_lbc(:)%is_allocated = .false.
   endif

   if( .not. fixed_lbc(id)%is_allocated ) then
     if( p_ch4 > 1 ) then
       allocate( fixed_lbc(id)%ch4_lbc(its:ite,jts:jte),stat=astat )
       if( astat /= 0 ) then
         write(*,*) 'mozcart_lbc_init: its,ite,jts,jte = ',its,ite,jts,jte
         call wrf_error_fatal("mozcart_lbc_init: failed to allocate ch4 lbc")
       end if
       fixed_lbc(id)%ch4_lbc(its:ite,jts:jte) = chem(its:ite,kts,jts:jte,p_ch4)
     end if
     if( p_n2o > 1 ) then
       allocate( fixed_lbc(id)%n2o_lbc(its:ite,jts:jte),stat=astat )
       if( astat /= 0 ) then
         call wrf_error_fatal("mozcart_lbc_init: failed to allocate n2o lbc")
       end if
       fixed_lbc(id)%n2o_lbc(its:ite,jts:jte) = chem(its:ite,kts,jts:jte,p_n2o)
     end if
     if( p_h2 > 1 ) then
       allocate( fixed_lbc(id)%h2_lbc(its:ite,jts:jte),stat=astat )
       if( astat /= 0 ) then
         call wrf_error_fatal("mozcart_lbc_init: failed to allocate h2 lbc")
       end if
       fixed_lbc(id)%h2_lbc(its:ite,jts:jte) = chem(its:ite,kts,jts:jte,p_h2)
     end if
     fixed_lbc(id)%is_allocated = .true.
   end if

END SUBROUTINE mozcart_lbc_init

SUBROUTINE mozcart_lbc_set( chem, num_chem, id, &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts )

    USE module_state_description, only : p_ch4, p_n2o, p_h2

integer, intent(in) :: id
integer, intent(in) :: num_chem
integer, intent(in) :: ims, ime, jms, jme, kms, kme,    &
                       its, ite, jts, jte, kts
real, intent(inout) :: chem(ims:ime,kms:kme,jms:jme,num_chem)

   if( p_ch4 > 1 ) then
      chem(its:ite,kts,jts:jte,p_ch4) = fixed_lbc(id)%ch4_lbc(its:ite,jts:jte)
   end if
   if( p_n2o > 1 ) then
      chem(its:ite,kts,jts:jte,p_n2o) = fixed_lbc(id)%n2o_lbc(its:ite,jts:jte)
   end if
   if( p_h2 > 1 ) then
      chem(its:ite,kts,jts:jte,p_h2) = fixed_lbc(id)%h2_lbc(its:ite,jts:jte)
   end if

END SUBROUTINE mozcart_lbc_set
SUBROUTINE PVS(ims,ime,jms,jme,nz1,nz2, &
               UB, VB,TB, SIGMA, XMSF, UMSF, &
               VMSF,COR,PSB,DX,PV, &
               ids,ide,jds,jde,kds,kde, &
               ips,ipe,jps,jpe,kps,kpe)

   IMPLICIT NONE

   INTEGER, INTENT(IN)                :: ims, ime
   INTEGER, INTENT(IN)                :: jms, jme
   INTEGER, INTENT(IN)                :: nz1, nz2
   INTEGER, INTENT(IN)                :: ips,ipe,jps,jpe,kps,kpe
   INTEGER, INTENT(IN)                :: ids,ide,jds,jde,kds,kde
 
   REAL, INTENT(IN)                   :: TB(ims:ime,nz1:nz2,jms:jme)
   REAL, INTENT(IN)                   :: UB(ims:ime,nz1:nz2,jms:jme)
   REAL, INTENT(IN)                   :: VB(ims:ime,nz1:nz2,jms:jme)
   REAL, INTENT(OUT)                  :: PV(ims:ime,nz1:nz2,jms:jme)
   REAL, INTENT(IN)                   :: XMSF(ims:ime,jms:jme)
   REAL, INTENT(IN)                   :: UMSF(ims:ime,jms:jme)
   REAL, INTENT(IN)                   :: VMSF(ims:ime,jms:jme)
   REAL, INTENT(IN)                   :: COR(ims:ime,jms:jme)
   REAL, INTENT(IN)                   :: PSB(ims:ime,jms:jme)
   REAL, INTENT(IN)                   :: SIGMA(nz1:nz2)

   real :: DSX, DSY,F0,F1,F2,T00,T1,T2,T3
   integer :: k, K0, K1, K2, i, j, iend, jend, kend, ii, jj
   real :: DX, SCALE, GRAV, XE
   real,dimension(ims:ime,jms:jme) :: UD,VD,DUDS,DVDS,DTDX,DTDY,DTDS,THEH,VOR 
   SCALE=-1.E6
   GRAV= 9.8104
   XE=0.286

   kend=min(kpe,kde-1)
   jend=min(jpe,jde-1)
   iend=min(ipe,ide-1)

!    OPEN (6,FILE='pv_stu.out')

   PV = 0.D0
   DO k=kps,kend !level loop
      IF(k.EQ.kps) THEN
         K0=K
         K1=K+1
         K2=K+2
         F0=-1.*(1./(SIGMA(K1)-SIGMA(K0))+1./(SIGMA(K2)-SIGMA(K0)))
         F1=1./(SIGMA(K1)-SIGMA(K0))+1./(SIGMA(K2)-SIGMA(K1))
         F2=-1.*((SIGMA(K1)-SIGMA(K0))/((SIGMA(K2)-SIGMA(K0))*(SIGMA(K2)-SIGMA(K1))))
      ELSE IF(k.EQ.kend) THEN
         K0=K-2
         K1=K-1
         K2=K
         F0=(SIGMA(K2)-SIGMA(K1))/((SIGMA(K2)-SIGMA(K0))*(SIGMA(K1)-SIGMA(K0)))
         F1=-1./(SIGMA(K1)-SIGMA(K0))-1./(SIGMA(K2)-SIGMA(K1))
         F2=1./(SIGMA(K2)-SIGMA(K0))+1./(SIGMA(K2)-SIGMA(K1))
      ELSE
         K0=K-1
         K1=K
         K2=K+1
         F0=-1.*(SIGMA(K2)-SIGMA(K1))/((SIGMA(K1)-SIGMA(K0))*(SIGMA(K2)-SIGMA(K0)))
         F1=1./(SIGMA(K1)-SIGMA(K0))-1./(SIGMA(K2)-SIGMA(K1))
         F2=(SIGMA(K1)-SIGMA(K0))/((SIGMA(K2)-SIGMA(K1))*(SIGMA(K2)-SIGMA(K0)))
      ENDIF

      DO j=jps,jend
      DO i=ips,iend

!        ... COMPUTE VERTICAL DERIVATIVES: dU/ds, dV/ds, dTHETA/ds


         DUDS(i,j)=.5*(F0*(UB(i+1,K0,j)+UB(i,K0,j))+F1*(UB(i+1,K1,j) &
                    +UB(i,K1,j))+F2*(UB(i+1,K2,j)+UB(i,K2,j)))
         DVDS(i,j)=.5*(F0*(VB(i,K0,j+1)+VB(i,K0,j))+F1*(VB(i,K1,j+1) &
                    +VB(i,K1,j))+F2*(VB(i,K2,j+1)+VB(i,K2,j)))
         T00=TB(i,K0,j)
         T1=TB(i,K1,j)
         T2=TB(i,K2,j)

!         THEH(i,j)=TB(i,k,j)
         DTDS(i,j)=F0*T00+F1*T1+F2*T2
      END DO
      END DO

      THEH = 0.D0
      DO j=jms,jme
         DO i=ims,ime
            ii = i
            jj = j
            if (i.le.ids) &
               ii = ids
            if (i.ge.ide) &
               ii = iend
            if (j.le.jds) &
               jj = jds
            if (j.ge.jde) &
               jj = jend
            THEH(i,j)=TB(ii,k,jj)
         END DO
      END DO

      DSX=2.*DX
      DSY=2.*DX

      !!COMPUTE zonal HORIZONTAL DERIVATIVES: dTHETA/dx
      DTDX = 0.D0
      DO j=jps,jend
         DO i=ips,iend
            if (i.eq.ips .and. i.le.ids) then
               T1=THEH(i,j)/XMSF(i,j)
               T2=THEH(i+1,j)/XMSF(i+1,j)
               T3=THEH(i+2,j)/XMSF(i+2,j)
               DTDX(i,j)=(XMSF(i,j)**2)*(-1.5*T1+2.*T2-.5*T3)/DX
            elseif (i.eq.iend .and. i.ge.ide-1) then
               T00=THEH(i-2,j)/XMSF(i-2,j)
               T1=THEH(i-1,j)/XMSF(i-1,j)
               T2=THEH(i,j)/XMSF(i,j)
               DTDX(i,j)=(XMSF(i,j)**2)*(.5*T00-2.*T1+1.5*T2)/DX
            else
               T1=THEH(i-1,j)/XMSF(I-1,j)
               T2=THEH(i+1,j)/XMSF(I+1,j)
               DTDX(i,j)=(XMSF(i,j)**2)*(T2-T1)/DSX
            end if
!    write(6,'(3I5, 5E10.3)'),i,k,j, THEH(i-1,j),THEH(i+1,j),xmsf(i-1,j),xmsf(i+1,j),DTDX(i,j)

         END DO
      END DO

      VD = 0.D0
      DO j=jms,jme
         DO i=ims,ime
            if (i.le.ids) then
!            if (i.eq.ips .and. i.le.ids) then
               VD(i,j)=VB(ids,k,j)/VMSF(ids,j)
            elseif (i.ge.ide) then
!            elseif (i.eq.iend+1 .and. i.ge.ide) then
               VD(i,j)=VB(iend,k,j)/VMSF(iend,j)
            else
               VD(i,j)=.5*(VB(i,k,j)/VMSF(i,j)+VB(i-1,k,j)/VMSF(i-1,j))
            end if
         END DO
      END DO


      !!COMPUTE meridional HORIZONTAL DERIVATIVES: dTHETA/dy
      DTDY = 0.D0
      DO i=ips,iend
         DO j=jps,jend
            if (j.eq.jps .and. j.le.jds) then
               T1=THEH(i,j)/XMSF(i,j)
               T2=THEH(i,j+1)/XMSF(i,j+1)
               T3=THEH(i,j+2)/XMSF(i,j+2)
               DTDY(i,j)=(XMSF(i,j)**2)*(-1.5*T1+2.*T2-.5*T3)/DX
            elseif (j.eq.jend .and. j.ge.jde-1) then
               T00=THEH(i,j-2)/XMSF(i,j-2)
               T1=THEH(i,j-1)/XMSF(i,j-1)
               T2=THEH(i,j)/XMSF(i,j)
               DTDY(i,j)=(XMSF(i,j)**2)*(.5*T00-2.*T1+1.5*T2)/DX
            else
               T1=THEH(i,j-1)/XMSF(i,j-1)
               T2=THEH(i,j+1)/XMSF(i,j+1)
               DTDY(i,j)=(XMSF(i,j)**2)*(T2-T1)/DSY
            end if
! write(6,'(3I5, 5E10.3)'),i,k,j, THEH(i,j-1),THEH(i,j+1),xmsf(i,j-1),xmsf(i,j+1),DTDY(i,j)
         END DO

      END DO


      UD = 0.D0
      DO i=ims,ime
         DO j=jms,jme
            if (j.le.jds) then
!            if (j.eq.jps .and. j.le.jds) then
               UD(i,j)=UB(i,k,jds)/UMSF(i,jds)
            elseif (j.ge.jde) then
!            elseif (j.eq.jend+1 .and. j.ge.jde) then
               UD(i,j)=UB(i,k,jend)/UMSF(i,jend)
            else
               UD(i,j)=.5*(UB(i,k,j)/UMSF(i,j)+UB(i,k,j-1)/UMSF(i,j-1))
            end if
         END DO
      END DO


      !!COMPUTE SLAB ABSOLUTE VORTICITY
      !!Store Potential Vorticity in PV
      DO j=jps,jend
      DO i=ips,iend
         VOR(i,j) = (XMSF(i,j)**2)*                                  &
                     ((VD(i+1,j)+VD(i+1,j+1)-VD(i,j)-VD(i,j+1))/DSX  &
                     -(UD(i,j+1)+UD(i+1,j+1)-UD(i,j)-UD(i+1,j))/DSY) &
                     +COR(i,j)

         PV(i,k,j) = (GRAV*SCALE/PSB(i,j))*(VOR(i,j)*DTDS(i,j)       &
                      -DVDS(i,j)*DTDX(i,j)+DUDS(i,j)*DTDY(i,j))
 !     if(k.eq.37) then
 !     write(*,'(3I5, 1P, 24E10.3)'),i,k,j,tb(i,k,j),uc(i,k,j),vb(i,k,j), &
 !                                     xmsf(i,j),umsf(i,j),vmsf(i,j), &
 !                                     cor(i,j), psb(i,j), sigma(k),   &
 !                                     vd(i+1,j), vd(i+1,j+1), vd(i,j), vd(i,j+1), &                  !
 !                                     ud(i,j+1), ud(i+1, j+1), ud(i,j), ud(i+1,j), &                 !
 !                                     vor(i,j), dtds(i,j), dvds(i,j), &                              !
 !                                     dtdx(i,j), duds(i,j), dtdy(i,j), pv(i,k,j)
 !     endif

 !     write(6,'(3I5, 16E10.3)'),i,k,j,vd(i+1,j), vd(i+1,j+1), vd(i,j), vd(i,j+1), &
 !                                     ud(i,j+1), ud(i+1, j+1), ud(i,j), ud(i+1,j), &
 !                                     psb(i,j), vor(i,j), dtds(i,j), dvds(i,j), &
 !                                     dtdx(i,j), duds(i,j), dtdy(i,j), pv(i,k,j) 
      END DO
      END DO

   END DO



!      RETURN
END SUBROUTINE PVS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE PV2O3_CONST(xlat,preshPa,pv2o3_con)
!    USE module_data_pv2o3  
 
    REAL, INTENT(IN)                   :: xlat,preshPa
    REAL, INTENT(OUT)                  :: pv2o3_con ! O3/PV constant in ppbv/PVunit
    REAL                               :: con_reflv(4) ! O3/PV constants at refrnc levs in ppbv/PVunit
    REAL                               :: wgt,pres,xlt
    INTEGER                            :: k
! Xing et al. (2016) ACP, Representing the effects of STE on 3D O3 distributions
! in chemistry transport models using a PV based parameterization.
! O3/PV factor versus latitude given by 5th order polynomial, for 3 pressure levels,
! with seasonal corrections. Here, seasonal correction neglected (July=0.9).
! 8/5/16 SAM: Constants in 5th order polynomial given at 58.56, 76.4 and 95.62 mb,
! extrapolate those constants to 500mb for intialization and boundary conditions
! below 95.62mb.
    xlt=abs(xlat) ! xlat must be in degrees

    con_reflv(1)=pv2o3_lv1(1)+pv2o3_lv1(2)*xlt+pv2o3_lv1(3)*xlt**2+ &
                pv2o3_lv1(4)*xlt**3+pv2o3_lv1(5)*xlt**4+pv2o3_lv1(6)*xlt**5
    con_reflv(2)=pv2o3_lv2(1)+pv2o3_lv2(2)*xlt+pv2o3_lv2(3)*xlt**2+ &
                pv2o3_lv2(4)*xlt**3+pv2o3_lv2(5)*xlt**4+pv2o3_lv2(6)*xlt**5
    con_reflv(3)=pv2o3_lv3(1)+pv2o3_lv3(2)*xlt+pv2o3_lv3(3)*xlt**2+ &
                pv2o3_lv3(4)*xlt**3+pv2o3_lv3(5)*xlt**4+pv2o3_lv3(6)*xlt**5
    con_reflv(4)=pv2o3_lv4(1)+pv2o3_lv4(2)*xlt+pv2o3_lv4(3)*xlt**2+ &
                pv2o3_lv4(4)*xlt**3+pv2o3_lv4(5)*xlt**4+pv2o3_lv4(6)*xlt**5 

    pres=preshPa
    if(pres.lt.pref_pv2o3(1))then
       pres=pref_pv2o3(1) ! Limit lower pressure to lowest ref pressure of Xing et al. (58.56 mb)
       pv2o3_con=-99.0
    endif
! Interpolate according to log(pressure) 
! - Recommended that if interpolation performed at pressures outside those in pref_pv2o3, it be done according to potential temperature, not pressure (GG and SAM).
! - Code kept as placeholder for now
!    do k=1,levs_pv2o3-1
!    if(pres.le.pref_pv2o3(k+1))then
!    wgt=(alog(pref_pv2o3(k+1))-alog(pres))/(alog(pref_pv2o3(k+1))-alog(pref_pv2o3(k)))
!    pv2o3_con=wgt*con_reflv(k+1)+(1.-wgt)*con_reflv(k)
!    exit
!    endif
!    enddo

    input_loop: do k=1,levs_pv2o3-1 
       IF (pres .EQ. pref_pv2o3(k) )THEN
          pv2o3_con=con_reflv(k)
          EXIT input_loop
       ELSE IF ( (pres .GT. pref_pv2o3(k)) .AND. &
                  (pres .LT. pref_pv2o3(k+1)) ) THEN
          wgt=(alog(pref_pv2o3(k+1))-alog(pres))/ &
              (alog(pref_pv2o3(k+1))-alog(pref_pv2o3(k)))
          pv2o3_con = MAX( wgt *con_reflv(k+1) + &
                     (1.-wgt)*con_reflv(k), 0.)
          EXIT input_loop
       ENDIF
    ENDDO input_loop

!   RETURN
END SUBROUTINE PV2O3_CONST
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bdy_chem_value_top_pv ( chem,xlat,         &
                                   ims,ime, jms,jme, kms,kme,    &
                                   its,ite, jts,jte, kts,kte,    &
                                   pb,p, &
                                   pv)

    IMPLICIT NONE

    REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_chem ),  &
               intent(INOUT) :: chem
    REAL, DIMENSION( ims:ime , jms:jme ),   intent(IN)   :: xlat     ! for determination of O3/PV p\
roportionality constant
    REAL                  :: pv2o3_con   ! for determination of O3/PV proportionality constant
    REAL, DIMENSION(ims:ime , kms:kme , jms:jme), intent(IN) :: pv,pb,p
    REAL   :: preshPa

    INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
    INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
    INTEGER :: i,j

!    OPEN (6,FILE='pv_stu.out')
       DO i=its,ite
           DO j=jts,jte
!       write(6,'(3I5, 3E10.3)'),i,kte,j, pv(i,kte,j),preshPa,xlat(i,j)
             preshPa=pb(i,kte,j)+p(i,kte,j)
             CALL PV2O3_CONST(xlat(i,j),preshPa,pv2o3_con)
               chem(i,kte,j,p_co_ant)= max(0.168,pv(i,kte,j)*pv2o3_con*1.E-3)
               chem(i,kte,j,p_co2_ant)=chem(i,kte,j,p_co2_ant)

           ENDDO
         ENDDO


!    RETURN
END SUBROUTINE bdy_chem_value_top_pv



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE module_input_chem_data
