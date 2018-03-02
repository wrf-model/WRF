module module_data_mosaic_aero
!-----------------------------------------------------------------------
!
! 2015-06-09 r.c.easter - changes to the MOSAIC box model version for use in WRF-Chem  
!
! nbin_a_max is a parameter
!
!-----------------------------------------------------------------------

  
  use module_data_mosaic_kind, only:  r8
  
  implicit none
  
  ! mosaic.21.0.h
  !   09-jan-07 raz - major clean up of variables
  !   31-jul-06 raz - implemented Li and Lu (2001) surface tension model
  !   19-apr-06 raz - updated max nh4 concentration constraints
  !   11-apr-05 raz - added SOA based on SORGAM mechanism
  !   07-jan-05 raz - updated and cleaned up variable lists
  !   08-jul-03 raz - updated many variables
  !   07-aug-02 rce - this is rahul's latest version from freshair
  !   19-aug-02 raz - declared mass_soluble_a and kg as real
  !   07-oct-02 raz - declared zc and za as integer
  !   09-oct-02 raz - explicitly declared all variables
  !   29-oct-02 raz - defined naercomp as the total number of aerosol compounds
  !----------------------------------------------------------------------
  
  ! number of aerosol bins
! integer, save :: nbin_a_max = -999888777  ! maximum number of aerosol bins !BSINGH - 05/28/2013(RCE updates)
  integer, parameter :: nbin_a_max = 8  ! maximum number of aerosol bins !BSINGH - 05/28/2013(RCE updates)

  integer, save :: nbin_a     = -999888777  ! in-use  number of aerosol bins !BSINGH - namelist variable
  
  ! mosaic-specific parameters
  integer, parameter :: ngas_ioa = 4+1	! inorganic volatile aerosol species that have a gaseous counterpart

! SOA is treated using a simplified approach for anthropogenic and biomass burning species based on Hodzic and Jimenez, GMD, 2011
! Alma added 2 smp species - just before the traditional ant1_c 
! also 10 new VBS species
  integer, parameter :: ngas_soa = 68+2+16+10	! volatile soa species that have a gaseous counterpart

  integer, parameter :: ngas_het = 2+2
  integer, parameter :: ngas_volatile = ngas_ioa + ngas_soa + 2 ! OH and glyoxal at the end
  integer, parameter :: ngas_aerchtot = ngas_volatile + ngas_het ! total number of gases used in mosaic aerchemistry

  integer, parameter :: naer = 11+68+2+16+5+10	! num of chemical species per bin = 11*(inorg+oc+bc+oin) + (68+2+16)*soa + 5*glysoa + 10*VBS

  integer, parameter :: naercomp = 26+68+2+16+5+10	! 26*(electrolytes + oc,bc,oin) + (68+2+16)*soa + 5*glysoa + 10*VBS

  integer, parameter :: nelectrolyte = 22+1 ! num of electrolytes
  integer, parameter :: nsalt   = 12+3	! num of soluble salts
  integer, parameter :: nsoluble= 16+4  ! num of soluble electrolytes
  integer, parameter :: ncation = 4	! num of cations
  integer, parameter :: nanion  = 4+1	! num of anions
  
  integer, parameter :: nrxn_aer_gl = 4 ! num of gas-liquid equilibria
  integer, parameter :: nrxn_aer_ll = 3 ! num of liquid-liquid equilibria
  integer, parameter :: nrxn_aer_sg = 2 ! num of solid-gas equilibria
  integer, parameter :: nrxn_aer_sl = nsalt! num of solid-liquid equilibria
  
  integer, parameter :: mASTEM = 1	! Adaptive Step Time-Split Euler Method
  integer, parameter :: mLSODE = 2	! LSODES integrator
  integer, parameter :: mMODAL  = 1	! Modal size distribution framework
  integer, parameter :: mUNSTRUCTURED = 2 ! "unstructured" size distribution framework
  ! (no special organization of bins; no transfer of particles between bins)
  integer, parameter :: mSECTIONAL = 3	! Sectional size distribution framework
  integer, parameter :: mON     = 1	! flag: ON
  integer, parameter :: mOFF    = 0    ! flag:OFF
  integer, parameter :: mYES	= mON	! flag: yes or true
  integer, parameter :: mNO	= mOFF	! flag: no or false
  
  integer, parameter :: jsolid = 1
  integer, parameter :: jliquid= 2
  integer, parameter :: jtotal = 3
  
  integer, parameter :: jhyst_lo = 0	! lower hysteresis leg
  integer, parameter :: jhyst_up = 1 	! upper hysteresis leg
  integer, parameter :: jhyst_undefined = -1	! undefined
  
  ! values for mhyst_method
  integer, parameter :: mhyst_uporlo_jhyst = 1	
  ! select upper/lower using "box method" involving jhyst_leg
  !     *** this should only be used for box model applications
  !     *** do not use with for transport model applications (cam5, wrf-chem, etc)
  integer, parameter :: mhyst_uporlo_waterhyst = 2	
  ! select upper/lower using "3-d method" involving water_a_hyst
  integer, parameter :: mhyst_force_up = 3	! force upper leg
  integer, parameter :: mhyst_force_lo = 4	! force lower leg
  
  real(r8), parameter :: xhyst_up_crustal_thresh = 0.30_r8
  ! when rh_crystal < rh < rh_deliquesce and bin mass fraction of crustal species (oin, caco3, and caso4) 
  !    exceeds xhyst_up_crustal_thresh, the bin is not allowed to be on the upper hysteresis curve.
  ! mineral dust is mostly externally mixed, so a minor fraction of mineral dust species 
  !   will most likely reside in a minor fraction of the particles of the bin, 
  !   and should not control the bin aerosol water.  (value = 0.0 in previous versions of mosaic)

  integer, parameter :: no_aerosol = 0	! flag
  integer, parameter :: all_solid  = 1 ! flag
  integer, parameter :: all_liquid = 2 ! flag
  integer, parameter :: mixed      = 3	! flag
  
  integer, parameter :: soluble   = 1  ! flag
  integer, parameter :: insoluble = 2  ! flag

  integer, parameter :: MDRH_T_NUM     = 63     !BSINGH - Number of entities in MDRH_T array
  integer, parameter :: jsulf_poor_NUM = 211    !BSINGH - Number of entities in jsulf_poor array
  integer, parameter :: jsulf_rich_NUM = 71     !BSINGH - Number of entities in jsulf_rich array
  integer, parameter :: d_mdrh_DIM2    = 4      !BSINGH - Number of entities in d_MDRH 2nd dimension
  !     real(r8), parameter :: mass_cutoff = 1.e-3	! ng/m^3
  real(r8), parameter :: mass_cutoff = 1.e-6	! new value on 02-mar-2010
  
  real(r8), parameter :: density_min_allow = 1.0	! minimum allowed density (g/cc)
  real(r8), parameter :: density_max_allow = 3.0	! maximum allowed density (g/cc)
  real(r8), parameter :: ah2o_max = 0.99                ! maximum water activity allowed in aerosol water uptake calculations

  ! note - purpose of this data structure is to simplify passing new variables 
  !        into and out of the many mosaic routines
  type :: mosaic_vars_aa_type
     integer :: it_host
     integer :: it_mosaic
     integer, dimension(6) :: hostgridinfo(6)
     integer :: idiagbb_host
     integer :: f_mos_fail
     integer :: isteps_astem
     integer :: isteps_astem_max
     integer :: jastem_call
     integer :: jastem_fail
     integer :: jmesa_call
     integer :: jmesa_fail
     integer :: niter_mesa_max
     integer :: nmax_astem
     integer :: nmax_mesa
     integer :: fix_astem_negative
     logical :: flag_itr_kel
     logical :: zero_water_flag
     real(r8) :: cumul_steps_astem
     real(r8) :: niter_mesa
     real(r8) :: swdown
     real(r8), dimension(5,4) :: xnerr_astem_negative
     integer, dimension(:), allocatable :: iter_mesa
  end type mosaic_vars_aa_type

  
  
  !----------------------------------------------------------------------
  ! MOSAIC species indices
  !
  ! position of first soa species in aer and gas arrays
  integer, save :: isoa_first
  ! position of first soa species in comp_a and electrolyte arrays
  integer, save :: jsoa_first

  ! gas
  integer, save ::   &
       ih2so4_g,     ihno3_g,      ihcl_g,      inh3_g,   &
       imsa_g,       in2o5_g,      iclno2_g,   &
       iaro1_g,      iaro2_g,      ialk1_g,     iole1_g,   &   ! not sure about these
       iapi1_g,      iapi2_g,      ilim1_g,     ilim2_g        ! not sure about these
  
  ! aerosol generic
  integer, save ::   &
       iso4_a,     ino3_a,     icl_a,     inh4_a,     ico3_a,   &
       imsa_a,     ina_a,      ica_a,     ioc_a,      ibc_a,   &
       ioin_a,   &
       iaro1_a,    iaro2_a,   ialk1_a,    iole1_a,   &   ! not sure about these
       iapi1_a,    iapi2_a,    ilim1_a,   ilim2_a        ! not sure about these
  
  ! aerosol elecctrolytes/compounds
  integer, save ::   &
       jnh4so4,    jlvcite,    jnh4hso4,   jnh4no3,    jnh4cl,   &
       jna2so4,    jna3hso4,   jnahso4,    jnano3,     jnacl,   &
       jcaso4,     jcano3,     jcacl2,     jcaco3,     jh2so4,   &
       jhno3,      jhcl,       jhhso4,   &
       jnh4msa,    jnamsa,     jcamsa2,    jmsa,   &
       joc,        jbc,        join,       jh2o,   &
       jaro1,      jaro2,      jalk1,      jole1,   &   ! not sure about these
       japi1,      japi2,      jlim1,      jlim2        ! not sure about these
  
  ! aerosol ions
  integer, save ::   &
       jc_h,    jc_nh4, jc_na,  jc_ca,   &
       ja_hso4, ja_so4, ja_no3, ja_cl, ja_msa     ! , ja_co3
  
  ! gas - soa vbs
      integer, save ::                                        &
      ipcg1_b_c_g, ipcg2_b_c_g, ipcg3_b_c_g, ipcg4_b_c_g, &
      ipcg5_b_c_g, ipcg6_b_c_g, ipcg7_b_c_g, ipcg8_b_c_g, ipcg9_b_c_g, &
      ipcg1_b_o_g, ipcg2_b_o_g, ipcg3_b_o_g, ipcg4_b_o_g, &
      ipcg5_b_o_g, ipcg6_b_o_g, ipcg7_b_o_g, ipcg8_b_o_g, ipcg9_b_o_g, &
      iopcg1_b_c_g, iopcg2_b_c_g, iopcg3_b_c_g, iopcg4_b_c_g, &
      iopcg5_b_c_g, iopcg6_b_c_g, iopcg7_b_c_g, iopcg8_b_c_g, &
      iopcg1_b_o_g, iopcg2_b_o_g, iopcg3_b_o_g, iopcg4_b_o_g, &
      iopcg5_b_o_g, iopcg6_b_o_g, iopcg7_b_o_g, iopcg8_b_o_g, &
      ipcg1_f_c_g, ipcg2_f_c_g, ipcg3_f_c_g, ipcg4_f_c_g, &
      ipcg5_f_c_g, ipcg6_f_c_g, ipcg7_f_c_g, ipcg8_f_c_g, ipcg9_f_c_g, &
      ipcg1_f_o_g, ipcg2_f_o_g, ipcg3_f_o_g, ipcg4_f_o_g, &
      ipcg5_f_o_g, ipcg6_f_o_g, ipcg7_f_o_g, ipcg8_f_o_g, ipcg9_f_o_g, &
      iopcg1_f_c_g, iopcg2_f_c_g, iopcg3_f_c_g, iopcg4_f_c_g, &
      iopcg5_f_c_g, iopcg6_f_c_g, iopcg7_f_c_g, iopcg8_f_c_g, &
      iopcg1_f_o_g, iopcg2_f_o_g, iopcg3_f_o_g, iopcg4_f_o_g, &
      iopcg5_f_o_g, iopcg6_f_o_g, iopcg7_f_o_g, iopcg8_f_o_g, &
      iant1_c_g, iant2_c_g, iant3_c_g, iant4_c_g, &
      iant1_o_g, iant2_o_g, iant3_o_g, iant4_o_g, &
      ibiog1_c_g, ibiog2_c_g, ibiog3_c_g, ibiog4_c_g, &
      ibiog1_o_g, ibiog2_o_g, ibiog3_o_g, ibiog4_o_g, &
      ismpa_g, ismpbb_g, &
      iasoaX_g, iasoa1_g, iasoa2_g, iasoa3_g, iasoa4_g, &
      ibsoaX_g, ibsoa1_g, ibsoa2_g, ibsoa3_g, ibsoa4_g, &
      igly, iho

  ! aerosol generic - soa vbs
      integer, save ::   &
      ipcg1_b_c_a, ipcg2_b_c_a, ipcg3_b_c_a, ipcg4_b_c_a, &
      ipcg5_b_c_a, ipcg6_b_c_a, ipcg7_b_c_a, ipcg8_b_c_a, ipcg9_b_c_a, &
      ipcg1_b_o_a, ipcg2_b_o_a, ipcg3_b_o_a, ipcg4_b_o_a, &
      ipcg5_b_o_a, ipcg6_b_o_a, ipcg7_b_o_a, ipcg8_b_o_a, ipcg9_b_o_a, &
      iopcg1_b_c_a, iopcg2_b_c_a, iopcg3_b_c_a, iopcg4_b_c_a, &
      iopcg5_b_c_a, iopcg6_b_c_a, iopcg7_b_c_a, iopcg8_b_c_a, &
      iopcg1_b_o_a, iopcg2_b_o_a, iopcg3_b_o_a, iopcg4_b_o_a, &
      iopcg5_b_o_a, iopcg6_b_o_a, iopcg7_b_o_a, iopcg8_b_o_a,&
      ipcg1_f_c_a, ipcg2_f_c_a, ipcg3_f_c_a, ipcg4_f_c_a, &
      ipcg5_f_c_a, ipcg6_f_c_a, ipcg7_f_c_a, ipcg8_f_c_a, ipcg9_f_c_a, &
      ipcg1_f_o_a, ipcg2_f_o_a, ipcg3_f_o_a, ipcg4_f_o_a, &
      ipcg5_f_o_a, ipcg6_f_o_a, ipcg7_f_o_a, ipcg8_f_o_a, ipcg9_f_o_a, &
      iopcg1_f_c_a, iopcg2_f_c_a, iopcg3_f_c_a, iopcg4_f_c_a, &
      iopcg5_f_c_a, iopcg6_f_c_a, iopcg7_f_c_a, iopcg8_f_c_a, &
      iopcg1_f_o_a, iopcg2_f_o_a, iopcg3_f_o_a, iopcg4_f_o_a, &
      iopcg5_f_o_a, iopcg6_f_o_a, iopcg7_f_o_a, iopcg8_f_o_a, &
      ismpa_a, ismpbb_a, &
      iglysoa_r1_a, iglysoa_r2_a, iglysoa_oh_a, iglysoa_sfc_a, iglysoa_nh4_a, &
      iant1_c_a, iant2_c_a, iant3_c_a, iant4_c_a,&
      iant1_o_a, iant2_o_a, iant3_o_a, iant4_o_a,&
      ibiog1_c_a, ibiog2_c_a, ibiog3_c_a, ibiog4_c_a, &
      ibiog1_o_a, ibiog2_o_a, ibiog3_o_a, ibiog4_o_a, &
      iasoaX_a, iasoa1_a, iasoa2_a, iasoa3_a, iasoa4_a, &
      ibsoaX_a, ibsoa1_a, ibsoa2_a, ibsoa3_a, ibsoa4_a

  ! aerosol elecctrolytes/compounds - soa vbs
      integer, save ::   &
      jpcg1_b_c, jpcg2_b_c, jpcg3_b_c, jpcg4_b_c, &
      jpcg5_b_c, jpcg6_b_c, jpcg7_b_c, jpcg8_b_c, jpcg9_b_c, &
      jpcg1_b_o, jpcg2_b_o, jpcg3_b_o, jpcg4_b_o, &
      jpcg5_b_o, jpcg6_b_o, jpcg7_b_o, jpcg8_b_o, jpcg9_b_o, &
      jopcg1_b_c, jopcg2_b_c, jopcg3_b_c, jopcg4_b_c, &
      jopcg5_b_c, jopcg6_b_c, jopcg7_b_c, jopcg8_b_c, &
      jopcg1_b_o, jopcg2_b_o, jopcg3_b_o, jopcg4_b_o, &
      jopcg5_b_o, jopcg6_b_o, jopcg7_b_o, jopcg8_b_o, &
      jpcg1_f_c, jpcg2_f_c, jpcg3_f_c, jpcg4_f_c, &
      jpcg5_f_c, jpcg6_f_c, jpcg7_f_c, jpcg8_f_c, jpcg9_f_c, &
      jpcg1_f_o, jpcg2_f_o, jpcg3_f_o, jpcg4_f_o, &
      jpcg5_f_o, jpcg6_f_o, jpcg7_f_o, jpcg8_f_o, jpcg9_f_o, &
      jopcg1_f_c, jopcg2_f_c, jopcg3_f_c, jopcg4_f_c, &
      jopcg5_f_c, jopcg6_f_c, jopcg7_f_c, jopcg8_f_c, &
      jopcg1_f_o, jopcg2_f_o, jopcg3_f_o, jopcg4_f_o, &
      jopcg5_f_o, jopcg6_f_o, jopcg7_f_o, jopcg8_f_o, &
      jsmpa, jsmpbb, &
      jglysoa_r1, jglysoa_r2, jglysoa_oh, jglysoa_sfc, jglysoa_nh4, &
      jant1_c, jant2_c, jant3_c, jant4_c,  &
      jant1_o, jant2_o, jant3_o, jant4_o,  &
      jbiog1_c, jbiog2_c, jbiog3_c, jbiog4_c, &
      jbiog1_o, jbiog2_o, jbiog3_o, jbiog4_o, &
      jasoaX, jasoa1, jasoa2, jasoa3, jasoa4, &
      jbsoaX, jbsoa1, jbsoa2, jbsoa3, jbsoa4

  !----------------------------------------------------------------------
  ! MOSAIC variables

  ! NOTES on use_cam5mam_soa_params and use_cam5mam_accom_coefs
  !    pure MOSAIC box model runs - these should be 0
  !    pure CAM5 runs - these can be 0 or 1 (usually 1)
  !       their values are set in module_mosaic_cam_init.F90
  !    MOSAIC box model runs that emulate CAM5 behavior (e.g. for debugging etc.) - 
  !       their values should match those in the CAM5 run (usually 1)
  integer, save :: use_cam5mam_soa_params  = 0   ! if >0, use cam5-mam soa/soag parameter values
  integer, save :: use_cam5mam_accom_coefs = 0   ! if >0, use cam5-mam accomodation coefficient values

  integer, save :: 	&
       !it_mosaic,			   &  ! time-step index
       !irepeat_mosaic,			   &  ! "repeat" index
       !iclm_aer,			   &  ! i-location
       !jclm_aer,			   &  ! j-location
       !kclm_aer,			   &  ! k-location
       mclm_aer,			   &  ! m-subarea
       mGAS_AER_XFER,			   &  ! flag: mON, mOFF
       mDYNAMIC_SOLVER,		   &  ! flag: mASTEM, mLSODE
       mSIZE_FRAMEWORK,		   &  ! flag: mMODAL, mSECTIONAL
       mhyst_method,			   &  ! flag: 0, 1, 2
       maersize_init_flag1,		   &  ! flag: 0, 1, 2
       mcoag_flag1,			   &  ! flag: 0, 1, 2, ...
       mmovesect_flag1,		   &  ! flag: 0, 1, 2, ...
       mnewnuc_flag1,			   &  ! flag: 0, 1, 2, ...
       msectional_flag1,		   &  ! flag: 0, 1, 2, ...
       msectional_flag2,		   &  ! flag: 0, 1, 2, ...
       method_bcfrac,      		   &  ! flag: ...
       method_kappa,       		   &  ! flag: ...
       ifreq_coag,            		   &  ! frequency at which coagulation is done
       ipmcmos_aero,                       &
       maeroptic_aero,      		   &
       msoa_flag1,      		   &  ! flag: 0/neg = no soa, 1 = default, 1000 = vbs
       msoa_vbs_info(9)    		      ! flags controlling vbs soa mechanism in wrfchem

       !jaerosolstate(nbin_a_max),	   &  ! flag: no_aerosol, all_solid, all_liquid, mixed
       !jaerosolstate_bgn(nbin_a_max)!,	   &  ! flag: no_aerosol, all_solid, all_liquid, mixed
  !jphase(nbin_a_max),		   &  ! phase index: jtotal, jsolid, jliquid
  !jhyst_leg(nbin_a_max)!,		   &  ! hysteresis leg: jhyst_up, jhyst_lo
  !iprint_input			      ! flag: mON, mOFF
  
  !real(r8), save :: 	&
       !num_a(nbin_a_max), 		   &  ! #/cc(air)
       !Dpgn_a(nbin_a_max), 		   &  ! cm
       !Dp_dry_a(nbin_a_max),		   &  ! cm
       !Dp_wet_a(nbin_a_max),		   &  ! cm
       !Dp_core_a(nbin_a_max),		   &  ! diameter of "optical core" (cm)
       !area_dry_a(nbin_a_max),		   &  ! cm^2/cc(air)
       !area_wet_a(nbin_a_max),		   &  ! cm^2/cc(air)
       !mass_dry_salt(nbin_a_max),	   &  ! g/cc(air)
       !mass_dry_a_bgn(nbin_a_max),	   &  ! g/cc(air)
       !mass_dry_a(nbin_a_max),		   &  ! g/cc(air)
       !mass_wet_a(nbin_a_max),		   &  ! g/cc(air)
       !mass_soluble_a(nbin_a_max),	   &  ! ng/cc(air)
       !vol_dry_a(nbin_a_max),		   &  ! cc/cc(air)
       !vol_wet_a(nbin_a_max),		   &  ! cc/cc(air)
       !dens_dry_a_bgn(nbin_a_max),	   &  ! g/cc
       !dens_dry_a(nbin_a_max),		   &  ! g/cc
       !dens_wet_a(nbin_a_max),		   &  ! g/cc
       !sigmag_a(nbin_a_max),		   &  ! -
       !water_a(nbin_a_max), 		   &  ! kg(water)/m^3(air)
       !water_a_hyst(nbin_a_max),	   &  ! kg(water)/m^3(air) hysteresis (at 60% RH)
       !water_a_up(nbin_a_max),		   &  ! kg(water)/m^3(air) at 60% RH
       !pH(nbin_a_max),			   &  ! pH
       !aer(naer,3,nbin_a_max),		   &  ! nmol/m^3
       !aer_sum(3,nbin_a_max),		   &  ! nmol/m^3
       !aer_percent(naer,3,nbin_a_max),	   &  ! %
       !comp_a(naercomp),		   &  ! g/cc(air)
       !electrolyte(nelectrolyte,3,nbin_a_max), &  ! nmol/m^3
       !electrolyte_sum(3,nbin_a_max),	        &  ! nmol/m^3
       !epercent(nelectrolyte,3,nbin_a_max),    &  ! %
       !gas(ngas_volatile),		        &  ! nmol/m^3
       !aH2O,   &
       !aH2O_a(nbin_a_max),   &
       !DpmV(nbin_a_max),   &
       !volume_a(nbin_a_max),   &
       !volume_bin(nbin_a_max),		   &  ! dry volume of one particle
       !kelvin(nbin_a_max),		   &  ! kelvin factor for water content
       !kel(ngas_volatile,nbin_a_max),	   &  ! kelvin factor for condensing species
       !kelvin_nh4no3,   &
       !kelvin_nh4cl!,   &
       !total_species(ngas_volatile),   &
       !ext_cross(nbin_a_max),             &  ! extinction cross section of a particle (cm^-2)
       !scat_cross(nbin_a_max),            &  ! scattering cross section of a particle (cm^-2)
       !asym_particle(nbin_a_max)             ! asymmetry parameter of a particle (dimensionless)
  
  real(r8), save :: 	&
       dlo_aersize_init, 		   &  ! lowermost dry Dp for aersize init (micron)
       dhi_aersize_init, 		   &  ! uppermost dry Dp for aersize init (micron)
       xcutlo_atype_md1_init, 		   &  ! lowermost & uppermost bc mass fractions for
       xcuthi_atype_md1_init, 		   &  !    for initializing the "atype_md1" dimension
       xcutlo_atype_md2_init, 		   &  ! lowermost & uppermost hygroscopicity (kappa) for
       xcuthi_atype_md2_init  		      !    for initializing the "atype_md2" dimension
  
  integer, save :: 	&
       method_atype_md1_init, 		   &  ! method for initializing "atype_md1"
       method_atype_md2_init  		      ! method for initializing "atype_md2"
  
  
  !----------------------------------------------------------------------
  ! ASTEM variables
  integer, save ::	   &
  !idry_case3a(nbin_a_max),	   &  ! mYES, mNO
  !ieqblm_bin(nbin_a_max),		   &  ! mYES, mNO
  !ieqblm_ASTEM,			   &  ! mYES, mNO
  !ieqblm_soa,			   &  ! mYES, mNO
  !jASTEM_call,   &
  !jASTEM_fail,   &
  !isteps_ASTEM,   &
  !isteps_SOA,   &
  !isteps_ASTEM_max,   &
  nmax_ASTEM!,   &
  !integrate(ngas_volatile,3,nbin_a_max)	! mYES, mNO
  
  integer, save :: m_gas2bin_uptk_flag = 1
! m_gas2bin_uptk_flag = 1 if all gases can condense to all bins,
!    and = 0 otherwise, which currently only happens with cam5-mosaic
  integer, save :: i_gas2bin_uptk_flag(ngas_aerchtot,nbin_a_max)
! i_gas2bin_uptk_flag(iv,ibin) = 1 if gas iv can condense to bin ibin, 
!    and = 0 if it cannot (i.e., is not allowed to)
! mosaic box model, partmc-mosaic, and wrf-chem do not have to allocate it

  real(r8), save ::	&
  !Po_soa(ngas_volatile),		     &  ! Pascal
  !sat_soa(ngas_volatile),		     &  ! nmol/m^3(air)
  !x_soa(naer),			     &  ! soa mole fraction
  !sfc_a(ngas_volatile),		     &  ! nmol/m^3
  !Heff(ngas_volatile,nbin_a_max),	     &  !
  !kg(ngas_volatile,nbin_a_max), 	     &  ! 1/s
  !df_gas_s(ngas_volatile,nbin_a_max),  &  ! nmol/m^3 (G-G*) = driving force)
  !df_gas_l(ngas_volatile,nbin_a_max),  &  ! nmol/m^3 (G-G*) = driving force)
  !df_gas_o(ngas_volatile,nbin_a_max),  &  ! nmol/m^3 (G-G*) = driving force)
  !df_gas(ngas_volatile,nbin_a_max),    &  ! nmol/m^3 (G-G*) = driving force)
  !flux_s(ngas_volatile,nbin_a_max),    &  ! nmol/m^3/s
  !flux_l(ngas_volatile,nbin_a_max),    &  ! nmol/m^3/s
  !flux_o(ngas_volatile,nbin_a_max),    &  ! nmol/m^3/s
  !flux(ngas_volatile,nbin_a_max),      &  ! nmol/m^3/s
  !sumkg_h2so4,			     &  ! 1/s
  !sumkg_msa,			     &  ! 1/s
  !sumkg_nh3,			     &  ! 1/s
  !sumkg_hno3,			     &  ! 1/s
  !sumkg_hcl,			     &  ! 1/s
  !delta_nh3_max(nbin_a_max),	     &  ! nmol/m^3
  !delta_hno3_max(nbin_a_max),	     &  ! nmol/m^3
  !delta_hcl_max(nbin_a_max),	     &  ! nmol/m^3
  !Keq_nh4no3,			     &
  !Keq_nh4cl,			     &
  !volatile_s(ngas_volatile,nbin_a_max),     &  ! nmol/m^3
  !phi_volatile_s(ngas_volatile,nbin_a_max), &  ! relative dr. force = (G-G*)/G
  !phi_volatile_l(ngas_volatile,nbin_a_max), &  ! relative dr. force = (G-G*)/G
  !phi_volatile_o(ngas_volatile,nbin_a_max), &  ! relative dr. force = (G-G*)/G
  !phi_nh4no3_s,			     &  ! relative dr. force: 0 to 1
  !phi_nh4cl_s,			     &  ! relative dr. force: 0 to 1
  !sum_vdf_s(ngas_volatile),	     &  ! (nmol/m^3)^2
  !sum_vol_s(ngas_volatile),	     &  ! nmol/m^3
  !sum_bin_s(ngas_volatile),	     &  ! number of bins that have flux_s(iv) < 0
  !avg_df_gas_s(ngas_volatile),	     &  !
  !h_s_i_m(ngas_volatile,nbin_a_max),   &  ! s
  !alpha_gas(ngas_volatile),	     &  ! - adaptive
  alpha_ASTEM,			     &  ! 0.01 to 0.05
  rtol_eqb_ASTEM,			     &  ! 0.01 to 0.03
  ptol_mol_ASTEM!,			     &  ! 0.01 to 1.0
  !cumul_steps_ASTEM,		     &
  !avg_steps_ASTEM
  
  !----------------------------------------------------------------------
  ! MESA variables
  integer, save ::	&
       jsalt_index(nsalt),   &
       jsulf_poor(jsulf_poor_NUM),   &
       jsulf_rich(jsulf_rich_NUM),   &
       !jsalt_present(nsalt),   &
       Nmax_mesa!,   &
       !jMESA_call,   &
       !jMESA_fail,   &
       !iter_MESA(nbin_a_max),   &
       !niter_MESA_max

  real(r8), save ::	&
       !eleliquid(nelectrolyte),   &
       !flux_sl(nsalt),   &
       !phi_salt(nsalt),   &
       !phi_salt_old(nsalt),   &
       !phi_bar(nsalt),   &
       !alpha_salt(nsalt),   &
       !sat_ratio(nsalt),   &
       !hsalt(nsalt),   &
       !hsalt_max,   &
       !frac_salt_liq(nsalt),   &
       !frac_salt_solid(nsalt),   &
       !growth_factor(nbin_a_max),   &
       d_mdrh(MDRH_T_NUM,d_mdrh_DIM2),		   &  ! mdrh(T) poly coeffs
       !MDRH(nbin_a_max),   &
       !MDRH_T(MDRH_T_NUM),   & 
       !molality0(nelectrolyte),   &
       rtol_mesa!,   &
       !niter_MESA,   &
       !niter_MESA_avg,   &
       !G_MX(nelectrolyte),   &
       !K_MX(nelectrolyte)
  
  !----------------------------------------------------------------------
  ! MOSAIC physico-chemical constants
  character(len= 6), save :: phasestate(0:4)
  character(len=16), save :: ename(nelectrolyte)	! electrolyte names
  character(len=16), save :: aer_name(naer)		! generic aerosol species name
  character(len=16), save :: gas_name(ngas_aerchtot)	! gas species name
  
  real(r8), save ::      &
       !T_K,					   &  ! temperature (K)
       !P_atm,					   &  ! pressure (atm)
       !RH_pc,					   &  ! relative humidity (%)
       !cair_mol_cc,				   &  ! air conc in mol/cc
       !cair_mol_m3,				   &  ! air conc in mol/m^3
       !conv1a,   &
       !conv1b,   &
       !conv2a,   &
       !conv2b,   &
       mw_electrolyte(nelectrolyte),		   &  ! molecular wt of electrolytes
       mw_aer_mac(naer),			   &  ! molecular wt of generic species
       mw_comp_a(naercomp),			   &  ! molecular wt of compounds
       mw_c(ncation),				   &  ! molecular wt of cations
       mw_a(nanion),				   &  ! molecular wt of anions
       mw_gas(ngas_aerchtot),			   &  ! molecular wt of gases
       dens_electrolyte(nelectrolyte),		   &  ! g/cc
       dens_aer_mac(naer),			   &  ! g/cc
       dens_comp_a(naercomp),			   &  ! g/cc (density of compounds)
       kappa_aer_mac(naer),			   &  ! "kappa" value (= hygroscopicity)
       partial_molar_vol(ngas_aerchtot),	   &  ! cc/mol (used in aerosol_phase_state)
       v_molar_gas(ngas_aerchtot)		      ! cc/mol (used in aerosolmtc)
       !sigma_water,				   &  ! water surface tension (N/m)
       !sigma_soln(nbin_a_max),    		   &  ! solution surface tension (N/m)
       !Keq_gl(nrxn_aer_gl),			   &  ! gas-liq eqblm const
       !Keq_ll(nrxn_aer_ll),			   &  ! liq-liq eqblm const
       !Keq_sg(nrxn_aer_sg),			   &  ! solid-gas eqbln const
       !Keq_sl(nrxn_aer_sl), 			   &  ! solid-liq eqblm const
       !Kp_nh3, 				   &  !
       !Kp_nh4no3, 				   &  !
       !Kp_nh4cl				      !
  
  complex   &
       ref_index_a(naercomp)!,			   &  ! refractive index of compounds
  !     ri_avg_a(nbin_a_max),			   &  ! vol avg ref index of bin
  !     ri_shell_a(nbin_a_max),			   &  ! vol avg ref index of bin for shell
  !     ri_core_a(nbin_a_max) 			      ! vol avg ref index of bin for core
  
  !----------------------------------------------------------------------
  ! MOSAIC activity coefficient models variables
  
  real(r8), save ::	&
       !mc(Ncation,nbin_a_max),		     &  ! mol/kg(water)
       !ma(Nanion,nbin_a_max),		     &  ! mol/kg(water)
       !mSULF,   &
       zc(Ncation),			     &  ! real charge
       za(Nanion),			     &  ! real charge
       !gam(nelectrolyte,nbin_a_max),   &
       !gam_ratio(nbin_a_max),   &
       !log_gamZ(nelectrolyte,nelectrolyte), &
       !log_gam(nelectrolyte),   &
       !activity(nelectrolyte,nbin_a_max),  &
       !xeq_a(nanion),   &
       !xeq_c(ncation),   &
       !na_Ma(nanion),   &
       !nc_Mc(ncation),   &
       a_zsr(6,nelectrolyte),		     &  ! binary molality polynomial coeffs
       b_zsr(nelectrolyte),		     &  ! binary molality coeff
       aw_min(nelectrolyte),		     &  ! minimum frh at which molality polynomial can be used
       b_mtem(6,nelectrolyte,nelectrolyte)     ! MTEM poly coeffs
  
  !----------------------------------------------------------------------
  ! MOSAIC massbalance variables
! real(r8), save ::   &
!      tot_so4_in,   &
!      tot_no3_in,   &
!      tot_cl_in,   &
!      tot_nh4_in,   &
!      tot_na_in,   &
!      tot_ca_in,   &
!      tot_so4_out,   &
!      tot_no3_out,   &
!      tot_cl_out,   &
!      tot_nh4_out,   &
!      tot_na_out,   &
!      tot_ca_out,   &
!      diff_so4,   &
!      diff_no3,   &
!      diff_cl,   &
!      diff_nh4,   &
!      diff_na,   &
!      diff_ca,   &
!      reldiff_so4,   &
!      reldiff_no3,   &
!      reldiff_cl,   &
!      reldiff_nh4,   &
!      reldiff_na,   &
!      reldiff_ca
  
  !----------------------------------------------------------------------

end module module_data_mosaic_aero
