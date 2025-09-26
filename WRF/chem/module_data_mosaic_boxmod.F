      module module_data_mosaic_boxmod
!-----------------------------------------------------------------------
!
! 2015-06-09 r.c.easter - changes to the MOSAIC box model version for use in WRF-Chem  
!
! Only a few variables are used in the WRF-Chem version.
!
!-----------------------------------------------------------------------

      use module_data_mosaic_constants, only:  &
          avogad, deg2rad, pi, piover4, piover6, third

      implicit none


      integer, save :: idiag_sect_coag = 0

      integer, save :: idiag_sect_movesect = 0

      integer, save :: idiag_sect_newnuc = 0

      character(len=20), allocatable :: name_rbox(:)


!------------------------------------------------------------------------
!
! species indices in rbox array (and chem array too)
! for the gas species used in the mosaic aerosol chemistry code
!
! (pointers for aerosol species are in module_data_mosaic_asecthp)
!
      integer, save :: kh2so4     = -999888777
      integer, save :: khno3      = -999888777
      integer, save :: knh3       = -999888777
      integer, save :: khcl       = -999888777
      integer, save :: kmsa       = -999888777
      integer, save :: karo1      = -999888777
      integer, save :: karo2      = -999888777
      integer, save :: kalk1      = -999888777
      integer, save :: kole1      = -999888777
      integer, save :: kapi1      = -999888777
      integer, save :: kapi2      = -999888777
      integer, save :: klim1      = -999888777
      integer, save :: klim2      = -999888777
      integer, save :: kn2o5      = -999888777
      integer, save :: kclno2     = -999888777
      integer, save :: ko3        = -999888777
      integer, save :: kpcg1_b_c  = -999888777
      integer, save :: kpcg2_b_c  = -999888777
      integer, save :: kpcg3_b_c  = -999888777
      integer, save :: kpcg4_b_c  = -999888777
      integer, save :: kpcg5_b_c  = -999888777
      integer, save :: kpcg6_b_c  = -999888777
      integer, save :: kpcg7_b_c  = -999888777
      integer, save :: kpcg8_b_c  = -999888777
      integer, save :: kpcg9_b_c  = -999888777
      integer, save :: kpcg1_b_o  = -999888777
      integer, save :: kpcg2_b_o  = -999888777
      integer, save :: kpcg3_b_o  = -999888777
      integer, save :: kpcg4_b_o  = -999888777
      integer, save :: kpcg5_b_o  = -999888777
      integer, save :: kpcg6_b_o  = -999888777
      integer, save :: kpcg7_b_o  = -999888777
      integer, save :: kpcg8_b_o  = -999888777
      integer, save :: kpcg9_b_o  = -999888777
      integer, save :: kopcg1_b_c = -999888777
      integer, save :: kopcg2_b_c = -999888777
      integer, save :: kopcg3_b_c = -999888777
      integer, save :: kopcg4_b_c = -999888777
      integer, save :: kopcg5_b_c = -999888777
      integer, save :: kopcg6_b_c = -999888777
      integer, save :: kopcg7_b_c = -999888777
      integer, save :: kopcg8_b_c = -999888777
      integer, save :: kopcg1_b_o = -999888777
      integer, save :: kopcg2_b_o = -999888777
      integer, save :: kopcg3_b_o = -999888777
      integer, save :: kopcg4_b_o = -999888777
      integer, save :: kopcg5_b_o = -999888777
      integer, save :: kopcg6_b_o = -999888777
      integer, save :: kopcg7_b_o = -999888777
      integer, save :: kopcg8_b_o = -999888777
      integer, save :: kpcg1_f_c  = -999888777
      integer, save :: kpcg2_f_c  = -999888777
      integer, save :: kpcg3_f_c  = -999888777
      integer, save :: kpcg4_f_c  = -999888777
      integer, save :: kpcg5_f_c  = -999888777
      integer, save :: kpcg6_f_c  = -999888777
      integer, save :: kpcg7_f_c  = -999888777
      integer, save :: kpcg8_f_c  = -999888777
      integer, save :: kpcg9_f_c  = -999888777
      integer, save :: kpcg1_f_o  = -999888777
      integer, save :: kpcg2_f_o  = -999888777
      integer, save :: kpcg3_f_o  = -999888777
      integer, save :: kpcg4_f_o  = -999888777
      integer, save :: kpcg5_f_o  = -999888777
      integer, save :: kpcg6_f_o  = -999888777
      integer, save :: kpcg7_f_o  = -999888777
      integer, save :: kpcg8_f_o  = -999888777
      integer, save :: kpcg9_f_o  = -999888777
      integer, save :: kopcg1_f_c = -999888777
      integer, save :: kopcg2_f_c = -999888777
      integer, save :: kopcg3_f_c = -999888777
      integer, save :: kopcg4_f_c = -999888777
      integer, save :: kopcg5_f_c = -999888777
      integer, save :: kopcg6_f_c = -999888777
      integer, save :: kopcg7_f_c = -999888777
      integer, save :: kopcg8_f_c = -999888777
      integer, save :: kopcg1_f_o = -999888777
      integer, save :: kopcg2_f_o = -999888777
      integer, save :: kopcg3_f_o = -999888777
      integer, save :: kopcg4_f_o = -999888777
      integer, save :: kopcg5_f_o = -999888777
      integer, save :: kopcg6_f_o = -999888777
      integer, save :: kopcg7_f_o = -999888777
      integer, save :: kopcg8_f_o = -999888777
      integer, save :: ksmpa      = -999888777
      integer, save :: ksmpbb     = -999888777
!     integer, save :: kiepox     = -999888777
!     integer, save :: kgly       = -999888777
      integer, save :: kant1_c    = -999888777
      integer, save :: kant2_c    = -999888777
      integer, save :: kant3_c    = -999888777
      integer, save :: kant4_c    = -999888777
      integer, save :: kant1_o    = -999888777
      integer, save :: kant2_o    = -999888777
      integer, save :: kant3_o    = -999888777
      integer, save :: kant4_o    = -999888777
      integer, save :: kbiog1_c   = -999888777
      integer, save :: kbiog2_c   = -999888777
      integer, save :: kbiog3_c   = -999888777
      integer, save :: kbiog4_c   = -999888777
      integer, save :: kbiog1_o   = -999888777
      integer, save :: kbiog2_o   = -999888777
      integer, save :: kbiog3_o   = -999888777
      integer, save :: kbiog4_o   = -999888777
      integer, save :: kso2       = -999888777
      integer, save :: kh2o2      = -999888777
      integer, save :: khcho      = -999888777
      integer, save :: khcooh     = -999888777
      integer, save :: koh        = -999888777
      integer, save :: kho2       = -999888777
      integer, save :: kno3       = -999888777
      integer, save :: kno        = -999888777
      integer, save :: kno2       = -999888777
      integer, save :: khono      = -999888777
      integer, save :: kpan       = -999888777
      integer, save :: kch3o2     = -999888777
      integer, save :: kch3oh     = -999888777
      integer, save :: kch3ooh    = -999888777

!     integer, save ::   &
!      kh2so4,      khno3,       khcl,        knh3,        kno,   &
!      kno2,        kno3,        kn2o5,       khono,       khno4,   &
!      ko3,         ko1d,        ko3p,        koh,         kho2,   &
!      kh2o2,       kco,         kso2,        kch4,        kc2h6,   &
!      kch3o2,      kethp,       khcho,       kch3oh,      kanol,   &
!      kch3ooh,     kethooh,     kald2,       khcooh,      krcooh,   &
!      kc2o3,       kpan,   &
!      karo1,       karo2,       kalk1,       kole1,       kapi1,   &
!      kapi2,       klim1,       klim2,   &
!      kpar,        kaone,       kmgly,       keth,        kolet,   &
!      kolei,       ktol,        kxyl,        kcres,       kto2,   &
!      kcro,        kopen,       konit,       krooh,       kro2,   &
!      kano2,       knap,        kxo2,        kxpar,   &
!      kisop,       kisoprd,     kisopp,      kisopn,      kisopo2,   &
!      kapi,        klim,   &
!      kdms,        kmsa,        kdmso,       kdmso2,      kch3so2h,   &
!      kch3sch2oo,  kch3so2,     kch3so3,     kch3so2ch2oo,kch3so2oo,   &
!      ksulfhox

! none of these should be needed in wrf-chem
!     integer, save ::   &
!      knum_a,      kdpdry_a,    ksigmag_a,  kjhyst_a,   &
!      kwater_a,    kso4_a,      kno3_a,     kcl_a,       knh4_a,   &
!      koc_a,       kmsa_a,      kco3_a,     kna_a,       kca_a,   &
!      kbc_a,       koin_a,      karo1_a,    karo2_a,     kalk1_a,   &
!      kole1_a,     kapi1_a,     kapi2_a,    klim1_a,     klim2_a

! none of these should be needed in wrf-chem
!     integer, save ::   &
!      knum_c,      kwater_c,    kso4_c,     kno3_c,     kcl_c,   &
!      kmsa_c,      kco3_c,      knh4_c,     kna_c,      kca_c,   &
!      koc_c,       kbc_c,       koin_c,   &
!      karo1_c,     karo2_c,     kalk1_c,    kole1_c,    kapi1_c,   &
!      kapi2_c,     klim1_c,     klim2_c


!------------------------------------------------------------------------

      end module module_data_mosaic_boxmod
