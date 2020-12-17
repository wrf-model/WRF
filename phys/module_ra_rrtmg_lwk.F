#if( BUILD_RRTMK != 1)
      MODULE module_ra_rrtmg_lwk
      CONTAINS
      SUBROUTINE rrtmg_lw
         REAL :: dummy
         dummy = 1
      END SUBROUTINE rrtmg_lw
      END MODULE module_ra_rrtmg_lwk
#else
!
! module module_ra_rrtmg_lw
!
!-------------------------------------------------------------------------------
   module parkind_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg kinds
!  Define integer and real kinds for various types.
!
!  Initial version: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!-------------------------------------------------------------------------------
!
!  implicit none
!
   save
!
! integer kinds
! 
   integer, parameter :: kind_ib = selected_int_kind(13)  ! 8 byte integer
   integer, parameter :: kind_im = selected_int_kind(6)   ! 4 byte integer
   integer, parameter :: kind_in = kind(1)                ! native integer
!
! real kinds
!
!  integer, parameter :: kind_rb = selected_real_kind(12) ! 8 byte real
!  integer, parameter :: kind_rm = selected_real_kind(6)  ! 4 byte real
!  integer, parameter :: kind_rn = kind(1.0)              ! native real
!
   integer, parameter :: kind_rb = kind(1.0)              ! native real
!
!-------------------------------------------------------------------------------
   end module parkind_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module parrrtm_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw main parameters
!
!  Initial version:  JJMorcrette, ECMWF, Jul 1998
!  Revised: MJIacono, AER, Jun 2006
!  Revised: MJIacono, AER, Aug 2007
!  Revised: MJIacono, AER, Aug 2008
!
!  name      type     purpose
!  -----  :  ----   : ----------------------------------------------
!  mxlay  :  integer: maximum number of layers
!  mg     :  integer: number of original g-intervals per spectral band
!  nbndlw :  integer: number of spectral bands
!  maxxsec:  integer: maximum number of cross-section molecules
!                    (e.g. cfcs)
!  maxinpx:  integer: 
!  ngptlw :  integer: total number of reduced g-intervals for rrtmg_lw
!  ngNN   :  integer: number of reduced g-intervals per spectral band
!  ngsNN  :  integer: cumulative number of g-intervals per band
!
!-------------------------------------------------------------------------------
   use parkind_k,  only : im => kind_im
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: mxlay  = 203
   integer(kind=im), parameter :: mg     = 16
   integer(kind=im), parameter :: nbndlw = 16
   integer(kind=im), parameter :: maxxsec= 4
   integer(kind=im), parameter :: mxmol  = 38
   integer(kind=im), parameter :: maxinpx= 38
   integer(kind=im), parameter :: nmol   = 7
!
! Use for 140 g-point model 
!
   integer(kind=im), parameter :: ngptlw = 140
!
! Use for 256 g-point model 
!  integer(kind=im), parameter :: ngptlw = 256
!
! Use for 140 g-point model
!
   integer(kind=im), parameter :: ng1  = 10
   integer(kind=im), parameter :: ng2  = 12
   integer(kind=im), parameter :: ng3  = 16
   integer(kind=im), parameter :: ng4  = 14
   integer(kind=im), parameter :: ng5  = 16
   integer(kind=im), parameter :: ng6  = 8
   integer(kind=im), parameter :: ng7  = 12
   integer(kind=im), parameter :: ng8  = 8
   integer(kind=im), parameter :: ng9  = 12
   integer(kind=im), parameter :: ng10 = 6
   integer(kind=im), parameter :: ng11 = 8
   integer(kind=im), parameter :: ng12 = 8
   integer(kind=im), parameter :: ng13 = 4
   integer(kind=im), parameter :: ng14 = 2
   integer(kind=im), parameter :: ng15 = 2
   integer(kind=im), parameter :: ng16 = 2
!
   integer(kind=im), parameter :: ngs1  = 10
   integer(kind=im), parameter :: ngs2  = 22
   integer(kind=im), parameter :: ngs3  = 38
   integer(kind=im), parameter :: ngs4  = 52
   integer(kind=im), parameter :: ngs5  = 68
   integer(kind=im), parameter :: ngs6  = 76
   integer(kind=im), parameter :: ngs7  = 88
   integer(kind=im), parameter :: ngs8  = 96
   integer(kind=im), parameter :: ngs9  = 108
   integer(kind=im), parameter :: ngs10 = 114
   integer(kind=im), parameter :: ngs11 = 122
   integer(kind=im), parameter :: ngs12 = 130
   integer(kind=im), parameter :: ngs13 = 134
   integer(kind=im), parameter :: ngs14 = 136
   integer(kind=im), parameter :: ngs15 = 138
!
! Use for 256 g-point model
!  integer(kind=im), parameter :: ng1  = 16
!  integer(kind=im), parameter :: ng2  = 16
!  integer(kind=im), parameter :: ng3  = 16
!  integer(kind=im), parameter :: ng4  = 16
!  integer(kind=im), parameter :: ng5  = 16
!  integer(kind=im), parameter :: ng6  = 16
!  integer(kind=im), parameter :: ng7  = 16
!  integer(kind=im), parameter :: ng8  = 16
!  integer(kind=im), parameter :: ng9  = 16
!  integer(kind=im), parameter :: ng10 = 16
!  integer(kind=im), parameter :: ng11 = 16
!  integer(kind=im), parameter :: ng12 = 16
!  integer(kind=im), parameter :: ng13 = 16
!  integer(kind=im), parameter :: ng14 = 16
!  integer(kind=im), parameter :: ng15 = 16
!  integer(kind=im), parameter :: ng16 = 16
!  integer(kind=im), parameter :: ngs1  = 16
!  integer(kind=im), parameter :: ngs2  = 32
!  integer(kind=im), parameter :: ngs3  = 48
!  integer(kind=im), parameter :: ngs4  = 64
!  integer(kind=im), parameter :: ngs5  = 80
!  integer(kind=im), parameter :: ngs6  = 96
!  integer(kind=im), parameter :: ngs7  = 112
!  integer(kind=im), parameter :: ngs8  = 128
!  integer(kind=im), parameter :: ngs9  = 144
!  integer(kind=im), parameter :: ngs10 = 160
!  integer(kind=im), parameter :: ngs11 = 176
!  integer(kind=im), parameter :: ngs12 = 192
!  integer(kind=im), parameter :: ngs13 = 208
!  integer(kind=im), parameter :: ngs14 = 224
!  integer(kind=im), parameter :: ngs15 = 240
!  integer(kind=im), parameter :: ngs16 = 256
!
!-------------------------------------------------------------------------------
   end module parrrtm_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_cld_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw cloud property coefficients
!
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  name      type     purpose
!  -----  :  ----   : ----------------------------------------------
!  abscld1:  real   : 
!  absice0:  real   : 
!  absice1:  real   : 
!  absice2:  real   : 
!  absice3:  real   : 
!  absliq0:  real   : 
!  absliq1:  real   : 
!
!-------------------------------------------------------------------------------
   use parkind_k, only : rb => kind_rb
!
!  implicit none
!
   save
!
   real(kind=rb)                   :: abscld1
   real(kind=rb), dimension(2)     :: absice0
   real(kind=rb), dimension(2,5)   :: absice1
   real(kind=rb), dimension(43,16) :: absice2
   real(kind=rb), dimension(46,16) :: absice3
   real(kind=rb)                   :: absliq0
   real(kind=rb), dimension(58,16) :: absliq1
!
!-------------------------------------------------------------------------------
   end module rrlw_cld_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_con_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw constants
!
!  Initial version: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  name      type     purpose
!  -----   :  ----   : ----------------------------------------------
!  fluxfac :  real   : radiance to flux conversion factor 
!  heatfac :  real   : flux to heating rate conversion factor
!  oneminus:  real   : 1.-1.e-6
!  pi      :  real   : pi
!  grav    :  real   : acceleration of gravity
!  planck  :  real   : planck constant
!  boltz   :  real   : boltzmann constant
!  clight  :  real   : speed of light
!  avogad  :  real   : avogadro constant 
!  alosmt  :  real   : loschmidt constant
!  gascon  :  real   : molar gas constant
!  radcn1  :  real   : first radiation constant
!  radcn2  :  real   : second radiation constant
!  sbcnst  :  real   : stefan-boltzmann constant
!  secdy   :  real   : seconds per day  
!
!-------------------------------------------------------------------------------
   use parkind_k, only : rb => kind_rb
!
!  implicit none
!
   save
!
   real(kind=rb)        :: fluxfac, heatfac
   real(kind=rb)        :: oneminus, pi, grav
   real(kind=rb)        :: planck, boltz, clight
   real(kind=rb)        :: avogad, alosmt, gascon
   real(kind=rb)        :: radcn1, radcn2
   real(kind=rb)        :: sbcnst, secdy
!
!-------------------------------------------------------------------------------
   end module rrlw_con_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg01_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 1
!  band 1:  10-250 cm-1 (low - h2o; high - h2o)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real
!  kao      : real     
!  kbo      : real     
!  kao_mn2  : real     
!  kbo_mn2  : real     
!  selfrefo : real     
!  forrefo  : real
!
!  COMBINED
!  name      type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real
!  ka       : real     
!  kb       : real     
!  absa     : real
!  absb     : real
!  ka_mn2   : real     
!  kb_mn2   : real     
!  selfref  : real     
!  forref   : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no1  = 16
!
   real(kind=rb), dimension(no1)         :: fracrefao, fracrefbo
   real(kind=rb), dimension(5,13,no1)    :: kao
   real(kind=rb), dimension(5,13:59,no1) :: kbo
   real(kind=rb), dimension(19,no1)      :: kao_mn2, kbo_mn2
   real(kind=rb), dimension(10,no1)      :: selfrefo
   real(kind=rb), dimension(4,no1)       :: forrefo
!
   integer(kind=im), parameter :: ng1  = 10
!
   real(kind=rb), dimension(ng1)         :: fracrefa, fracrefb
   real(kind=rb), dimension(5,13,ng1)    :: ka
   real(kind=rb), dimension(65,ng1)      :: absa
   real(kind=rb), dimension(5,13:59,ng1) :: kb
   real(kind=rb), dimension(235,ng1)     :: absb
   real(kind=rb), dimension(19,ng1)      :: ka_mn2, kb_mn2
   real(kind=rb), dimension(10,ng1)      :: selfref(10,ng1)
   real(kind=rb), dimension(4,ng1)       :: forref(4,ng1)
!
   equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg01_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg02_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 2
!  band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real
!  kao      : real     
!  kbo      : real     
!  selfrefo : real     
!  forrefo  : real
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real
!  ka       : real     
!  kb       : real     
!  absa     : real
!  absb     : real
!  selfref  : real     
!  forref   : real
!
!  refparam : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no2  = 16
!
   real(kind=rb), dimension(no2) :: fracrefao, fracrefbo
   real(kind=rb), dimension(5,13,no2) :: kao
   real(kind=rb), dimension(5,13:59,no2) :: kbo
   real(kind=rb), dimension(10,no2) :: selfrefo(10,no2)
   real(kind=rb), dimension(4,no2) :: forrefo(4,no2)
!
   integer(kind=im), parameter :: ng2  = 12
!
   real(kind=rb), dimension(ng2)         :: fracrefa, fracrefb
   real(kind=rb), dimension(5,13,ng2)    :: ka(5,13,ng2)
   real(kind=rb), dimension(65,ng2)      :: absa
   real(kind=rb), dimension(5,13:59,ng2) :: kb
   real(kind=rb), dimension(235,ng2)     :: absb
   real(kind=rb), dimension(10,ng2)      :: selfref(10,ng2)
   real(kind=rb), dimension(4,ng2)       :: forref(4,ng2)
!
   real(kind=rb), dimension(13) :: refparam
!
   equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg02_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg03_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 3
!  band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real
!  kao      : real     
!  kbo      : real     
!  kao_mn2o : real     
!  kbo_mn2o : real     
!  selfrefo : real     
!  forrefo  : real
!
!  COMBINED
!  name      type     purpose
!  ----   : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real
!  ka      : real     
!  kb      : real     
!  ka_mn2o : real     
!  kb_mn2o : real     
!  selfref : real     
!  forref  : real
!
!  absa    : real
!  absb    : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no3  = 16
!
   real(kind=rb), dimension(no3,9)         :: fracrefao
   real(kind=rb), dimension(no3,5)         :: fracrefbo(no3,5)
   real(kind=rb), dimension(9,5,13,no3)    :: kao
   real(kind=rb), dimension(5,5,13:59,no3) :: kbo
   real(kind=rb), dimension(9,19,no3)      :: kao_mn2o
   real(kind=rb), dimension(5,19,no3)      :: kbo_mn2o
   real(kind=rb), dimension(10,no3)        :: selfrefo
   real(kind=rb), dimension(4,no3)         :: forrefo
!
   integer(kind=im), parameter :: ng3  = 16
!
   real(kind=rb), dimension(ng3,9)         :: fracrefa
   real(kind=rb), dimension(ng3,5)         :: fracrefb
   real(kind=rb), dimension(9,5,13,ng3)    :: ka
   real(kind=rb), dimension(585,ng3)       :: absa
   real(kind=rb), dimension(5,5,13:59,ng3) :: kb
   real(kind=rb), dimension(1175,ng3)      :: absb
   real(kind=rb), dimension(9,19,ng3)      :: ka_mn2o
   real(kind=rb), dimension(5,19,ng3)      :: kb_mn2o
   real(kind=rb), dimension(10,ng3)        :: selfref
   real(kind=rb), dimension(4,ng3)         :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg03_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg04_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 4
!  band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real
!  kao      : real     
!  kbo      : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  absa     : real
!  absb     : real
!  fracrefa : real    
!  fracrefb : real
!  ka       : real     
!  kb       : real     
!  selfref  : real     
!  forref   : real     
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no4  = 16
!
   real(kind=rb), dimension(no4,9) :: fracrefao
   real(kind=rb), dimension(no4,5) :: fracrefbo
   real(kind=rb), dimension(9,5,13,no4) :: kao
   real(kind=rb), dimension(5,5,13:59,no4) :: kbo
   real(kind=rb), dimension(10,no4) :: selfrefo
   real(kind=rb), dimension(4,no4) :: forrefo
!
   integer(kind=im), parameter :: ng4  = 14
!
   real(kind=rb), dimension(ng4,9) :: fracrefa
   real(kind=rb), dimension(ng4,5) :: fracrefb
   real(kind=rb), dimension(9,5,13,ng4) :: ka
   real(kind=rb), dimension(585,ng4) :: absa
   real(kind=rb), dimension(5,5,13:59,ng4) :: kb
   real(kind=rb), dimension(1175,ng4) :: absb
   real(kind=rb), dimension(10,ng4) :: selfref
   real(kind=rb), dimension(4,ng4) :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg04_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg05_k
!-------------------------------------------------------------------------------
!
!  abstract :  rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 5
!  band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real
!  kao      : real     
!  kbo      : real     
!  kao_mo3  : real     
!  selfrefo : real     
!  forrefo  : real     
!  ccl4o    : real
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real
!  ka       : real     
!  kb       : real     
!  ka_mo3   : real     
!  selfref  : real     
!  forref   : real     
!  ccl4     : real
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no5  = 16
!
   real(kind=rb), dimension(no5,9)         :: fracrefao
   real(kind=rb), dimension(no5,5)         :: fracrefbo
   real(kind=rb), dimension(9,5,13,no5)    :: kao
   real(kind=rb), dimension(5,5,13:59,no5) :: kbo
   real(kind=rb), dimension(9,19,no5)      :: kao_mo3
   real(kind=rb), dimension(10,no5)        :: selfrefo
   real(kind=rb), dimension(4,no5)         :: forrefo
   real(kind=rb), dimension(no5)           :: ccl4o
!
   integer(kind=im), parameter :: ng5  = 16
!
   real(kind=rb), dimension(ng5,9)         :: fracrefa
   real(kind=rb), dimension(ng5,5)         :: fracrefb
   real(kind=rb), dimension(9,5,13,ng5)    :: ka
   real(kind=rb), dimension(585,ng5)       :: absa
   real(kind=rb), dimension(5,5,13:59,ng5) :: kb
   real(kind=rb), dimension(1175,ng5)      :: absb
   real(kind=rb), dimension(9,19,ng5)      :: ka_mo3
   real(kind=rb), dimension(10,ng5)        :: selfref
   real(kind=rb), dimension(4,ng5)         :: forref
   real(kind=rb), dimension(ng5)           :: ccl4
!      
   equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg05_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg06_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 6
!  band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  kao      : real     
!  kao_mco2 : real     
!  selfrefo : real     
!  forrefo  : real     
!  cfc11adjo: real
!  cfc12o   : real
!
!  COMBINED 
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  ka       : real     
!  ka_mco2  : real     
!  selfref  : real     
!  forref   : real     
!  cfc11adj : real
!  cfc12    : real
!
!  absa     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no6  = 16
!
   real(kind=rb), dimension(no6)      :: fracrefao
   real(kind=rb), dimension(5,13,no6) :: kao
   real(kind=rb), dimension(19,no6)   :: kao_mco2
   real(kind=rb), dimension(10,no6)   :: selfrefo
   real(kind=rb), dimension(4,no6)    :: forrefo
!
   real(kind=rb) , dimension(no6) :: cfc11adjo, cfc12o
!
   integer(kind=im), parameter :: ng6  = 8
!
   real(kind=rb), dimension(ng6)      :: fracrefa
   real(kind=rb), dimension(5,13,ng6) :: ka
   real(kind=rb), dimension(65,ng6)   :: absa
   real(kind=rb), dimension(19,ng6)   :: ka_mco2
   real(kind=rb), dimension(10,ng6)   :: selfref
   real(kind=rb), dimension(4,ng6)    :: forref
!
   real(kind=rb) , dimension(ng6) :: cfc11adj, cfc12
!
   equivalence (ka(1,1,1),absa(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg06_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg07_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 7
!  band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  kao_mco2 : real     
!  kbo_mco2 : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real    
!  ka       : real     
!  kb       : real     
!  ka_mco2  : real     
!  kb_mco2  : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no7  = 16
!
   real(kind=rb), dimension(no7)         :: fracrefbo
   real(kind=rb), dimension(no7,9)       :: fracrefao
   real(kind=rb), dimension(9,5,13,no7)  :: kao
   real(kind=rb), dimension(5,13:59,no7) :: kbo
   real(kind=rb), dimension(9,19,no7)    :: kao_mco2
   real(kind=rb), dimension(19,no7)      :: kbo_mco2
   real(kind=rb), dimension(10,no7)      :: selfrefo
   real(kind=rb), dimension(4,no7)       :: forrefo
!
   integer(kind=im), parameter :: ng7  = 12
!
   real(kind=rb), dimension(ng7)         :: fracrefb
   real(kind=rb), dimension(ng7,9)       :: fracrefa
   real(kind=rb), dimension(9,5,13,ng7)  :: ka
   real(kind=rb), dimension(585,ng7)     :: absa
   real(kind=rb), dimension(5,13:59,ng7) :: kb
   real(kind=rb), dimension(235,ng7)     :: absb
   real(kind=rb), dimension(9,19,ng7)    :: ka_mco2
   real(kind=rb), dimension(19,ng7)      :: kb_mco2
   real(kind=rb), dimension(10,ng7)      :: selfref
   real(kind=rb), dimension(4,ng7)       :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg07_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg08_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 8
!  band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  kao_mco2 : real     
!  kbo_mco2 : real     
!  kao_mn2o : real     
!  kbo_mn2o : real     
!  kao_mo3  : real     
!  selfrefo : real     
!  forrefo  : real     
!  cfc12o   : real     
!  cfc22adjo: real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real    
!  ka       : real     
!  kb       : real     
!  ka_mco2  : real     
!  kb_mco2  : real     
!  ka_mn2o  : real     
!  kb_mn2o  : real     
!  ka_mo3   : real     
!  selfref  : real     
!  forref   : real     
!  cfc12    : real     
!  cfc22adj : real     
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no8  = 16
!
   real(kind=rb), dimension(no8) :: fracrefao, fracrefbo, cfc12o, cfc22adjo
!
   real(kind=rb), dimension(5,13,no8)    :: kao(5,13,no8)
   real(kind=rb), dimension(19,no8)      :: kao_mco2, kao_mn2o, kao_mo3
   real(kind=rb), dimension(5,13:59,no8) :: kbo
   real(kind=rb), dimension(19,no8)      :: kbo_mco2, kbo_mn2o
   real(kind=rb), dimension(10,no8)      :: selfrefo
   real(kind=rb), dimension(4,no8)       :: forrefo
!
   integer(kind=im), parameter :: ng8  = 8
!
   real(kind=rb) , dimension(ng8) :: fracrefa, fracrefb, cfc12, cfc22adj
!
   real(kind=rb), dimension(5,13,ng8)    :: ka
   real(kind=rb), dimension(65,ng8)      :: absa
   real(kind=rb), dimension(5,13:59,ng8) :: kb
   real(kind=rb), dimension(235,ng8)     :: absb
   real(kind=rb), dimension(19,ng8)      :: ka_mco2, ka_mn2o, ka_mo3,          &
                                            kb_mco2, kb_mn2o
   real(kind=rb), dimension(10,ng8)      :: selfref
   real(kind=rb), dimension(4,ng8)       :: forref
!
   equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg08_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg09_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 9
!  band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  kao_mn2o : real     
!  kbo_mn2o : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real    
!  ka       : real     
!  kb       : real     
!  ka_mn2o  : real     
!  kb_mn2o  : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no9  = 16
!
   real(kind=rb), dimension(no9) :: fracrefbo
!
   real(kind=rb), dimension(no9,9)       :: fracrefao
   real(kind=rb), dimension(9,5,13,no9)  :: kao
   real(kind=rb), dimension(5,13:59,no9) :: kbo
   real(kind=rb), dimension(9,19,no9)    :: kao_mn2o
   real(kind=rb), dimension(19,no9)      :: kbo_mn2o
   real(kind=rb), dimension(10,no9)      :: selfrefo
   real(kind=rb), dimension(4,no9)       :: forrefo
!
   integer(kind=im), parameter :: ng9  = 12
!
   real(kind=rb), dimension(ng9)         :: fracrefb
   real(kind=rb), dimension(ng9,9)       :: fracrefa
   real(kind=rb), dimension(9,5,13,ng9)  :: ka
   real(kind=rb), dimension(585,ng9)     :: absa
   real(kind=rb), dimension(5,13:59,ng9) :: kb
   real(kind=rb), dimension(235,ng9)     :: absb
   real(kind=rb), dimension(9,19,ng9)    :: ka_mn2o
   real(kind=rb), dimension(19,ng9)      :: kb_mn2o
   real(kind=rb), dimension(10,ng9)      :: selfref
   real(kind=rb), dimension(4,ng9)       :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg09_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg10_k
!-------------------------------------------------------------------------------
!
!  abstarct : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 10
!  band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED 
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no10 = 16
!
   real(kind=rb), dimension(no10) :: fracrefao, fracrefbo
!
   real(kind=rb), dimension(5,13,no10)    :: kao
   real(kind=rb), dimension(5,13:59,no10) :: kbo
   real(kind=rb), dimension(10,no10)      :: selfrefo
   real(kind=rb), dimension(4,no10)       :: forrefo
!
   integer(kind=im), parameter :: ng10 = 6
!
   real(kind=rb), dimension(ng10)         :: fracrefa, fracrefb
   real(kind=rb), dimension(5,13,ng10)    :: ka
   real(kind=rb), dimension(65,ng10)      :: absa
   real(kind=rb), dimension(5,13:59,ng10) :: kb
   real(kind=rb), dimension(235,ng10)     :: absb
   real(kind=rb), dimension(10,ng10)      :: selfref
   real(kind=rb), dimension(4,ng10)       :: forref
!
   equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg10_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg11_k
!-------------------------------------------------------------------------------
!
!  abtract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 11
!  band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  kao_mo2  : real     
!  kbo_mo2  : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real    
!  ka       : real     
!  kb       : real     
!  ka_mo2   : real     
!  kb_mo2   : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no11 = 16
!
   real(kind=rb), dimension(no11) :: fracrefao, fracrefbo
!
   real(kind=rb), dimension(5,13,no11)    :: kao
   real(kind=rb), dimension(5,13:59,no11) :: kbo
   real(kind=rb), dimension(19,no11)      :: kao_mo2, kbo_mo2
   real(kind=rb), dimension(10,no11)      :: selfrefo
   real(kind=rb), dimension(4,no11)       :: forrefo
!
   integer(kind=im), parameter :: ng11 = 8
!
   real(kind=rb) , dimension(ng11) :: fracrefa, fracrefb
!
   real(kind=rb), dimension(5,13,ng11)    :: ka
   real(kind=rb), dimension(65,ng11)      :: absa
   real(kind=rb), dimension(5,13:59,ng11) :: kb
   real(kind=rb), dimension(235,ng11)     :: absb
   real(kind=rb), dimension(19,ng11)      :: ka_mo2, kb_mo2
   real(kind=rb), dimension(10,ng11)      :: selfref
   real(kind=rb), dimension(4,ng11)       :: forref
!
   equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg11_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg12_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 12
!  band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  kao      : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  ka       : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no12 = 16
!
   real(kind=rb),dimension(no12,9)      :: fracrefao
   real(kind=rb),dimension(9,5,13,no12) :: kao
   real(kind=rb),dimension(10,no12)     :: selfrefo
   real(kind=rb),dimension(4,no12)      :: forrefo
!
   integer(kind=im), parameter :: ng12 = 8
!
   real(kind=rb),dimension(ng12,9)      :: fracrefa
   real(kind=rb),dimension(9,5,13,ng12) :: ka
   real(kind=rb),dimension(585,ng12)    :: absa
   real(kind=rb),dimension(10,ng12)     :: selfref
   real(kind=rb),dimension(4,ng12)      :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg12_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg13_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 13
!  band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  kao      : real     
!  kao_mco2 : real     
!  kao_mco  : real     
!  kbo_mo3  : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  ka       : real     
!  ka_mco2  : real     
!  ka_mco   : real     
!  kb_mo3   : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no13 = 16
!
   real(kind=rb), dimension(no13) :: fracrefbo
!
   real(kind=rb), dimension(no13,9)      :: fracrefao
   real(kind=rb), dimension(9,5,13,no13) :: kao
   real(kind=rb), dimension(9,19,no13)   :: kao_mco2, kao_mco
   real(kind=rb), dimension(19,no13)     :: kbo_mo3
   real(kind=rb), dimension(10,no13)     :: selfrefo
   real(kind=rb), dimension(4,no13)      :: forrefo
!
!
   integer(kind=im), parameter :: ng13 = 4
!
   real(kind=rb) , dimension(ng13) :: fracrefb
!
   real(kind=rb), dimension(ng13,9)      :: fracrefa
   real(kind=rb), dimension(9,5,13,ng13) :: ka
   real(kind=rb), dimension(585,ng13)    :: absa
   real(kind=rb), dimension(9,19,ng13)   :: ka_mco2, ka_mco
   real(kind=rb), dimension(19,ng13)     :: kb_mo3
   real(kind=rb), dimension(10,ng13)     :: selfref
   real(kind=rb), dimension(4,ng13)      :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg13_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg14_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 14
!  band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  fracrefbo: real    
!  kao      : real     
!  kbo      : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  fracrefb : real    
!  ka       : real     
!  kb       : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no14 = 16
!
   real(kind=rb), dimension(no14) :: fracrefao, fracrefbo
!
   real(kind=rb), dimension(5,13,no14)    :: kao
   real(kind=rb), dimension(5,13:59,no14) :: kbo
   real(kind=rb), dimension(10,no14)      :: selfrefo
   real(kind=rb), dimension(4,no14)       :: forrefo
!
   integer(kind=im), parameter :: ng14 = 2
!
   real(kind=rb) , dimension(ng14) :: fracrefa, fracrefb
!
   real(kind=rb), dimension(5,13,ng14)    :: ka
   real(kind=rb), dimension(65,ng14)      :: absa
   real(kind=rb), dimension(5,13:59,ng14) :: kb
   real(kind=rb), dimension(235,ng14)     :: absb
   real(kind=rb), dimension(10,ng14)      :: selfref
   real(kind=rb), dimension(4,ng14)       :: forref
!
   equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg14_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg15_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 15
!  band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  kao      : real     
!  kao_mn2  : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  ka       : real     
!  ka_mn2   : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no15 = 16
!
   real(kind=rb), dimension(no15,9)      :: fracrefao
   real(kind=rb), dimension(9,5,13,no15) :: kao
   real(kind=rb), dimension(9,19,no15)   :: kao_mn2
   real(kind=rb), dimension(10,no15)     :: selfrefo
   real(kind=rb), dimension(4,no15)      :: forrefo
!
   integer(kind=im), parameter :: ng15 = 2
!
   real(kind=rb), dimension(ng15,9)      :: fracrefa
   real(kind=rb), dimension(9,5,13,ng15) :: ka
   real(kind=rb), dimension(585,ng15)    :: absa
   real(kind=rb), dimension(9,19,ng15)   :: ka_mn2
   real(kind=rb), dimension(10,ng15)     :: selfref
   real(kind=rb), dimension(4,ng15)      :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg15_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_kg16_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw ORIGINAL/COMBINED abs. coefficients for interval 16
!  band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  ORIGINAL
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefao: real    
!  kao      : real     
!  kbo      : real     
!  selfrefo : real     
!  forrefo  : real     
!
!  COMBINED
!  name       type     purpose
!  ----     : ----   : ---------------------------------------------
!  fracrefa : real    
!  ka       : real     
!  kb       : real     
!  selfref  : real     
!  forref   : real     
!
!  absa     : real
!  absb     : real
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: no16 = 16
!
   real(kind=rb), dimension(no16) :: fracrefbo
!
   real(kind=rb), dimension(no16,9)       :: fracrefao
   real(kind=rb), dimension(9,5,13,no16)  :: kao
   real(kind=rb), dimension(5,13:59,no16) :: kbo
   real(kind=rb), dimension(10,no16)      :: selfrefo
   real(kind=rb), dimension(4,no16)       :: forrefo
!
!
   integer(kind=im), parameter :: ng16 = 2
!
   real(kind=rb) , dimension(ng16) :: fracrefb
!
   real(kind=rb), dimension(ng16,9)       :: fracrefa
   real(kind=rb), dimension(9,5,13,ng16)  :: ka
   real(kind=rb), dimension(585,ng16)     :: absa
   real(kind=rb), dimension(5,13:59,ng16) :: kb
   real(kind=rb), dimension(235,ng16)     :: absb
   real(kind=rb), dimension(10,ng16)      :: selfref
   real(kind=rb), dimension(4,ng16)       :: forref
!
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!
!-------------------------------------------------------------------------------
   end module rrlw_kg16_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_ref_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw reference atmosphere 
!  Based on standard mid-latitude summer profile
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  name      type     purpose
!  -----  :  ----   : ----------------------------------------------
!  pref   :  real   : Reference pressure levels
!  preflog:  real   : Reference pressure levels, ln(pref)
!  tref   :  real   : Reference temperature levels for MLS profile
!  chi_mls:  real   : 
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   real(kind=rb), dimension(59)   :: pref
   real(kind=rb), dimension(59)   :: preflog
   real(kind=rb), dimension(59)   :: tref
   real(kind=rb), dimension(7,59) :: chi_mls
!
!-------------------------------------------------------------------------------
   end module rrlw_ref_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_tbl_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw exponential lookup table arrays
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, Jun 2006
!  Revised: MJIacono, AER, Aug 2007
!  Revised: MJIacono, AER, Aug 2008
!
!  name      type     purpose
!  -----  :  ----   : ----------------------------------------------
!  ntbl   :  integer: Lookup table dimension
!  tblint :  real   : Lookup table conversion factor
!  tau_tbl:  real   : Clear-sky optical depth (used in cloudy radiative
!                     transfer)
!  exp_tbl:  real   : Transmittance lookup table
!  tfn_tbl:  real   : Tau transition function; i.e. the transition of
!                     the Planck function from that for the mean layer
!                     temperature to that for the layer boundary
!                     temperature as a function of optical depth.
!                     The "linear in tau" method is used to make 
!                     the table.
!  pade   :  real   : Pade constant   
!  bpade  :  real   : Inverse of Pade constant   
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
!
   integer(kind=im), parameter :: ntbl = 10000
!
   real(kind=rb), parameter :: tblint = 10000.0_rb
!
   real(kind=rb), dimension(0:ntbl) :: tau_tbl
   real(kind=rb), dimension(0:ntbl) :: exp_tbl
   real(kind=rb), dimension(0:ntbl) :: tfn_tbl
!
   real(kind=rb), parameter :: pade = 0.278_rb
   real(kind=rb) :: bpade
!
!-------------------------------------------------------------------------------
   end module rrlw_tbl_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_vsn_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw version information
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  name      type     purpose
!  -----  :  ----   : ----------------------------------------------
!  hnamrtm :character: 
!  hnamini :character: 
!  hnamcld :character: 
!  hnamclc :character: 
!  hnamrtr :character: 
!  hnamrtx :character: 
!  hnamrtc :character: 
!  hnamset :character: 
!  hnamtau :character: 
!  hnamatm :character: 
!  hnamutl :character: 
!  hnamext :character: 
!  hnamkg  :character: 
!
!  hvrrtm :character: 
!  hvrini :character: 
!  hvrcld :character: 
!  hvrclc :character: 
!  hvrrtr :character: 
!  hvrrtx :character: 
!  hvrrtc :character: 
!  hvrset :character: 
!  hvrtau :character: 
!  hvratm :character: 
!  hvrutl :character: 
!  hvrext :character: 
!  hvrkg  :character: 
!
!-------------------------------------------------------------------------------
!
!  implicit none
!
   save
!
   character*18 hvrrtm,hvrini,hvrcld,hvrclc,hvrrtr,hvrrtx,                     &
                hvrrtc,hvrset,hvrtau,hvratm,hvrutl,hvrext
   character*20 hnamrtm,hnamini,hnamcld,hnamclc,hnamrtr,hnamrtx,               &
                hnamrtc,hnamset,hnamtau,hnamatm,hnamutl,hnamext
!
   character*18 hvrkg
   character*20 hnamkg
!
!-------------------------------------------------------------------------------
   end module rrlw_vsn_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrlw_wvn_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw spectral information
!
!  Initial version:  JJMorcrette, ECMWF, jul1998
!  Revised: MJIacono, AER, jun2006
!  Revised: MJIacono, AER, aug2008
!
!  name       type     purpose
!  -----   :  ----   : ----------------------------------------------
!  ng      :  integer: number of original g-intervals in each spectral band
!  nspa    :  integer: For the lower atmosphere, the number of reference
!                      atmospheres that are stored for each spectral band
!                      per pressure level and temperature.  Each of these
!                      atmospheres has different relative amounts of the 
!                      key species for the band (i.e. different binary
!                      species parameters).
!  nspb    :  integer: Same as nspa for the upper atmosphere
!  wavenum1:  real   : Spectral band lower boundary in wavenumbers
!  wavenum2:  real   : Spectral band upper boundary in wavenumbers
!  delwave :  real   : Spectral band width in wavenumbers
!  totplnk :  real   : integrated Planck value for each band; (band 16
!                      includes total from 2600 cm-1 to infinity)
!                      Used for calculation across total spectrum
!  totplk16:  real   : integrated Planck value for band 16 (2600-3250 cm-1)
!                      Used for calculation in band 16 only if 
!                      individual band output requested
!
!  ngc     :  integer: The number of new g-intervals in each band
!  ngs     :  integer: The cumulative sum of new g-intervals for each band
!  ngm     :  integer: The index of each new g-interval relative to the
!                      original 16 g-intervals in each band
!  ngn     :  integer: The number of original g-intervals that are 
!                      combined to make each new g-intervals in each band
!  ngb     :  integer: The band index for each new g-interval
!  wt      :  real   : RRTM weights for the original 16 g-intervals
!  rwgt    :  real   : Weights for combining original 16 g-intervals 
!                      (256 total) into reduced set of g-intervals 
!                      (140 total)
!  nxmol   :  integer: number of cross-section molecules
!  ixindx  :  integer: Flag for active cross-sections in calculation
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrtm_k, only : nbndlw, mg, ngptlw, maxinpx
!
!  implicit none
!
   save
!
   integer(kind=im), dimension(nbndlw) :: ng
   integer(kind=im), dimension(nbndlw) :: nspa
   integer(kind=im), dimension(nbndlw) :: nspb
!
   real(kind=rb), dimension(nbndlw) :: wavenum1
   real(kind=rb), dimension(nbndlw) :: wavenum2
   real(kind=rb), dimension(nbndlw) :: delwave
!
   real(kind=rb), dimension(181,nbndlw) :: totplnk
   real(kind=rb), dimension(181)        :: totplk16
!
   integer(kind=im), dimension(nbndlw)    :: ngc
   integer(kind=im), dimension(nbndlw)    :: ngs
   integer(kind=im), dimension(ngptlw)    :: ngn
   integer(kind=im), dimension(ngptlw)    :: ngb
   integer(kind=im), dimension(nbndlw*mg) :: ngm
!
   real(kind=rb), dimension(mg)        :: wt
   real(kind=rb), dimension(nbndlw*mg) :: rwgt
!
   integer(kind=im)                     :: nxmol
   integer(kind=im), dimension(maxinpx) :: ixindx
!
!-------------------------------------------------------------------------------
   end module rrlw_wvn_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module mersennetwister_k
!-------------------------------------------------------------------------------
!
!  abstract :
!  path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
!  author:    $Author: trn $
!  revision:  $Revision: 1.3 $
!  created:   $Date: 2009/04/16 19:54:22 $
!
!  Fortran-95 implementation of the Mersenne Twister 19937, following 
!  the C implementation described below (code mt19937ar-cok.c, dated 2002/2/10),
!  adapted cosmetically by making the names more general.  
!  Users must declare one or more variables of type randomnumbersequence 
!  in the calling 
!  procedure which are then initialized using a required seed. If the 
!  variable is not initialized the random numbers will all be 0. 
!  For example: 
!  program testrandoms 
!  use randomnumbers
!  type(randomnumbersequence) :: randomnumbers
!  integer                    :: i
!   
!  randomnumbers = new_randomnumbersequence(seed = 100)
!  do i = 1, 10
!    print ('(f12.10, 2x)'), getrandomreal(randomnumbers)
!  end do
!  end program testrandoms
! 
!  Fortran-95 implementation by 
!  Robert Pincus
!  NOAA-CIRES Climate Diagnostics Center
!  Boulder, CO 80305 
!  email: Robert.Pincus@colorado.edu
!
!  This documentation in the original C program reads:
!  --------------------------------------------------------------
!  A C-program for MT19937, with initialization improved 2002/2/10.
!  Coded by Takuji Nishimura and Makoto Matsumoto.
!  This is a faster version by taking Shawn Cokus's optimization,
!  Matthe Bellew's simplification, Isaku Wada's real version.
! 
!  Before using, initialize the state by using init_genrand(seed) 
!  or init_by_array(init_key, key_length).
! 
!  Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
!  All rights reserved.                          
! 
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted provided that the following conditions
!  are met:
! 
!  1. Redistributions of source code must retain the above copyright
!     notice, this list of conditions and the following disclaimer.
! 
!  2. Redistributions in binary form must reproduce the above copyright
!     notice, this list of conditions and the following disclaimer in the
!     documentation and/or other materials provided with the distribution.
! 
!  3. The names of its contributors may not be used to endorse or promote 
!     products derived from this software without specific prior written 
!     permission.
! 
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
!  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
!  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF use, data, OR
!  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAuseD AND ON ANY THEORY OF
!  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
!  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY out OF THE use OF THIS
!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! 
!  Any feedback is very welcome.
!  http://www.math.keio.ac.jp/matumoto/emt.html
!  email: matumoto@math.keio.ac.jp
!
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb 
!
   implicit none
!
   private
!  
! Algorithm parameters
! -------
! Period parameters
!
   integer(kind=im), parameter :: blocksize = 624,                             &
                                  m         = 397,                             &
! constant vector a (0x9908b0dfUL)
                                  matrix_a  = -1727483681,                     & 
! most significant w-r bits (0x80000000UL)
                                  umask     = -2147483647-1,                   &
! least significant r bits (0x7fffffffUL) 
                                  lmask     =  2147483647    
!
! Tempering parameters
!
! (0x9d2c5680UL)
   integer(kind=im), parameter :: tmaskb = -1658038656,                        &
! (0xefc60000UL)
                                  tmaskc = -272236544 
! 
! The type containing the state variable  
!
   type randomnumbersequence
     integer(kind=im)                            :: currentelement ! = blocksize
     integer(kind=im), dimension(0:blocksize -1) :: state          ! = 0
   end type randomnumbersequence
!-------------------------------------------------------------------------------
!
   interface new_randomnumbersequence
     module procedure initialize_scalar, initialize_vector
   end interface new_randomnumbersequence 
!
   public :: randomnumbersequence
   public :: new_randomnumbersequence, finalize_randomnumbersequence,          &
            getrandomint, getrandompositiveint, getrandomreal
!
   contains
!-------------------------------------------------------------------------------
! 
!
!-------------------------------------------------------------------------------
   function mixbits(u, v)
!-------------------------------------------------------------------------------
   integer(kind=im), intent(in   ) :: u, v
   integer(kind=im)                :: mixbits
!-------------------------------------------------------------------------------
   mixbits = ior(iand(u, umask), iand(v, lmask))
!
   end function mixbits
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   function twist(u, v)
!-------------------------------------------------------------------------------
   integer(kind=im), intent(in   ) :: u, v
   integer(kind=im)             :: twist
!
! Local variable
!
   integer(kind=im), parameter, dimension(0:1) :: t_matrix = (/0_im, matrix_a/)
!-------------------------------------------------------------------------------
   twist = ieor(ishft(mixbits(u, v), -1_im), t_matrix(iand(v, 1_im)))
   twist = ieor(ishft(mixbits(u, v), -1_im), t_matrix(iand(v, 1_im)))
!
   end function twist
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine nextstate(twister)
!-------------------------------------------------------------------------------
   type(randomnumbersequence), intent(inout) :: twister
!    
! Local variables
!
   integer(kind=im) :: k
!-------------------------------------------------------------------------------
!    
   do k = 0,blocksize-m-1
     twister%state(k) = ieor(twister%state(k+m),                               &
                             twist(twister%state(k),twister%state(k+1_im)))
   enddo 
!
   do k = blocksize-m,blocksize-2
     twister%state(k) = ieor(twister%state(k+m-blocksize),                     &
                             twist(twister%state(k),twister%state(k+1_im)))
   enddo
! 
   twister%state(blocksize-1_im) = ieor(twister%state(m-1_im),                 &
                                        twist(twister%state(blocksize-1_im),   &
                                        twister%state(0_im)))
   twister%currentelement = 0_im
!
   end subroutine nextstate
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   elemental function temper(y)
!-------------------------------------------------------------------------------
   integer(kind=im), intent(in   ) :: y
   integer(kind=im)                :: temper
!    
   integer(kind=im) :: x
!-------------------------------------------------------------------------------
!    
! Tempering
!
   x      = ieor(y, ishft(y, -11))
   x      = ieor(x, iand(ishft(x,  7), tmaskb))
   x      = ieor(x, iand(ishft(x, 15), tmaskc))
   temper = ieor(x, ishft(x, -18))
!
   end function temper
!-------------------------------------------------------------------------------
!
! public (but hidden) functions
!
!-------------------------------------------------------------------------------
   function initialize_scalar(seed) result(twister)
!-------------------------------------------------------------------------------
   integer(kind=im), intent(in   ) :: seed
   type(randomnumbersequence)      :: twister 
!    
   integer(kind=im) :: i
!-------------------------------------------------------------------------------
!
! See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. In the previous versions, 
! MSBs of the seed affect only MSBs of the array state[].                       
! 2002/01/09 modified by Makoto Matsumoto            
!
   twister%state(0) = iand(seed,-1_im)
!
   do i = 1,blocksize-1 ! ubound(twister%state)
     twister%state(i) = 1812433253_im*ieor(twister%state(i-1),                 &
                                           ishft(twister%state(i-1),-30_im))+i
     twister%state(i) = iand(twister%state(i),-1_im) ! for >32 bit machines
   enddo
!
   twister%currentelement = blocksize
!
   end function initialize_scalar
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   function initialize_vector(seed) result(twister)
!-------------------------------------------------------------------------------
   integer(kind=im), dimension(0:), intent(in   ) :: seed
   type(randomnumbersequence)                     :: twister 
!
   integer(kind=im) :: i, j, k, nfirstloop, nwraps
!-------------------------------------------------------------------------------
   nwraps  = 0
   twister = initialize_scalar(19650218_im)
!    
   nfirstloop = max(blocksize,size(seed))
!
   do k = 1,nfirstloop
     i = mod(k+nwraps,blocksize)
     j = mod(k-1,     size(seed))
     if (i == 0) then
       twister%state(i) = twister%state(blocksize-1)
       twister%state(1) = ieor(twister%state(1),                               &
                               ieor(twister%state(1-1),                        &
                               ishft(twister%state(1-1),-30_im))               &
                               *1664525_im)+seed(j)+j   ! Non-linear
       twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
       nwraps = nwraps+1
     else
       twister%state(i) = ieor(twister%state(i),                               &
                          ieor(twister%state(i-1),                             &
                          ishft(twister%state(i-1),-30_im))                    &
                          *1664525_im)+seed(j)+j        ! Non-linear
       twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
     endif
   enddo
!
! Walk through the state array, beginning where we left off in the block above
! 
   do i = mod(nfirstloop,blocksize)+nwraps+1,blocksize-1
     twister%state(i) = ieor(twister%state(i),                                 &
                        ieor(twister%state(i-1),                               &
                        ishft(twister%state(i-1),-30_im))                      &
                        *1566083941_im)-i             ! Non-linear
     twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
   enddo
!    
   twister%state(0) = twister%state(blocksize-1) 
!    
   do i = 1,mod(nfirstloop,blocksize)+nwraps
     twister%state(i) = ieor(twister%state(i),                                 &
                        ieor(twister%state(i-1),                               &
                        ishft(twister%state(i-1),-30_im))                      &
                        *1566083941_im)-i             ! Non-linear
     twister%state(i) = iand(twister%state(i), -1_im) ! for >32 bit machines
   enddo
!    
   twister%state(0) = umask 
   twister%currentelement = blocksize
!    
   end function initialize_vector
!-------------------------------------------------------------------------------
!
! public functions
!
!-------------------------------------------------------------------------------
   function getrandomint(twister)
!-------------------------------------------------------------------------------
!
!  abstract : Generate a random integer on the interval [0,0xffffffff]
!  Equivalent to genrand_int32 in the C code. 
!  Fortran doesn't have a type that's unsigned like C does, 
!  so this is integers in the range -2**31 - 2**31
!  All functions for getting random numbers call this one, 
!  then manipulate the result
!    
!-------------------------------------------------------------------------------
   type(randomnumbersequence), intent(inout) :: twister
   integer(kind=im)                          :: getrandomint
!-------------------------------------------------------------------------------
!
   if (twister%currentelement >= blocksize) call nextstate(twister)
!      
   getrandomint = temper(twister%state(twister%currentelement))
   twister%currentelement = twister%currentelement + 1
   end function getrandomint
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   function getrandompositiveint(twister)
!-------------------------------------------------------------------------------
!
!  abstact :  Generate a random integer on the interval [0,0x7fffffff]
!  or [0,2**31]
!  Equivalent to genrand_int31 in the C code. 
!
!-------------------------------------------------------------------------------
   type(randomnumbersequence), intent(inout) :: twister
   integer(kind=im)                          :: getrandompositiveint
!    
! Local integers
!
   integer(kind=im) :: localint
!-------------------------------------------------------------------------------
   localint = getrandomint(twister)
   getrandompositiveint = ishft(localint, -1)
!  
   end function getrandompositiveint
!-------------------------------------------------------------------------------
!
! mji - modified Jan 2007, double converted to rrtmg real kind type
!
!-------------------------------------------------------------------------------
   function getrandomreal(twister)
!-------------------------------------------------------------------------------
!
!  abstract : Generate a random number on [0,1]
!  Equivalent to genrand_real1 in the C code
!  The result is stored as double precision but has 32 bit resolution
!
!-------------------------------------------------------------------------------
   type(randomnumbersequence), intent(inout) :: twister
!  double precision                          :: getrandomreal
   real(kind=rb)                             :: getrandomreal
!
   integer(kind=im) :: localint
!-------------------------------------------------------------------------------
   localint = getrandomint(twister)
   if (localint < 0) then
!    getrandomreal = real(localint + 2.0**32)/(2.0**32 - 1.0)
     getrandomreal = (localint+2.0**32_rb)/(2.0**32_rb-1.0_rb)
   else
!    getrandomreal = real(localint        )/(2.0**32 - 1.0)
     getrandomreal = (localint            )/(2.0**32_rb-1.0_rb)
   endif
!
   end function getrandomreal
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine finalize_randomnumbersequence(twister)
!-------------------------------------------------------------------------------
   type(randomnumbersequence), intent(inout) :: twister
!    
   twister%currentelement = blocksize
   twister%state(:) = 0_im
!
   end subroutine finalize_randomnumbersequence
!-------------------------------------------------------------------------------
!
!  
!-------------------------------------------------------------------------------
   end module mersennetwister_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module mcica_random_numbers_k
!-------------------------------------------------------------------------------
!
!  abstract : Generic module to wrap random number generators. 
!  The module defines a type that identifies the particular stream of random 
!  numbers, and has procedures for initializing it and getting real numbers 
!  in the range 0 to 1. 
!  This version uses the Mersenne Twister to generate random numbers on [0, 1]. 
!
!-------------------------------------------------------------------------------
! The random number engine.
   use mersennetwister_k,  only : randomnumbersequence,                        &
                                new_randomnumbersequence, getrandomreal
! mji
!  use time_manager_mod, only : time_type, get_date
!
   use parkind_k,          only : im => kind_im, rb => kind_rb 
!
   implicit none
!
   private
!  
   type randomnumberstream
     type(randomnumbersequence) :: thenumbers
   end type randomnumberstream
!-------------------------------------------------------------------------------
!  
   interface getrandomnumbers
     module procedure getrandomnumber_scalar, getrandomnumber_1d,              &
                      getrandomnumber_2d
   end interface getrandomnumbers
! 
   interface initializerandomnumberstream
     module procedure initializerandomnumberstream_s,                          &
                      initializerandomnumberstream_v
   end interface initializerandomnumberstream
!
   public :: randomnumberstream,                                               &
             initializerandomnumberstream, getrandomnumbers
!! mji
!!           initializerandomnumberstream, getrandomnumbers,                   &
!!           constructSeed
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   function initializerandomnumberstream_s(seed) result(new) 
!-------------------------------------------------------------------------------
   integer(kind=im), intent(in   ) :: seed
   type(randomnumberstream)        :: new
!-------------------------------------------------------------------------------
   new%thenumbers = new_randomnumbersequence(seed)
!    
   end function initializerandomnumberstream_s
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   function initializerandomnumberstream_v(seed) result(new) 
!-------------------------------------------------------------------------------
   integer(kind=im), dimension(:), intent(in   ) :: seed
   type(randomnumberstream)                      :: new
!-------------------------------------------------------------------------------
   new%thenumbers = new_randomnumbersequence(seed)
!    
   end function initializerandomnumberstream_v
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine getrandomnumber_scalar(stream, number)
!-------------------------------------------------------------------------------
!
!  abstract : Procedures for drawing random numbers
!
!-------------------------------------------------------------------------------
   type(randomnumberstream), intent(inout) :: stream
   real(kind=rb)           , intent(  out) :: number
!-------------------------------------------------------------------------------
   number = getrandomreal(stream%thenumbers)
!
   end subroutine getrandomnumber_scalar
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine getrandomnumber_1d(stream, numbers)
!-------------------------------------------------------------------------------
   type(randomnumberstream)   , intent(inout) :: stream
   real(kind=rb), dimension(:), intent(  out) :: numbers
!
! Local variables
!
   integer(kind=im) :: i
!-------------------------------------------------------------------------------
!    
   do i = 1,size(numbers)
     numbers(i) = getrandomreal(stream%thenumbers)
   enddo
!
   end subroutine getrandomnumber_1d
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine getrandomnumber_2d(stream, numbers)
!-------------------------------------------------------------------------------
   type(randomnumberstream)     , intent(inout) :: stream
   real(kind=rb), dimension(:,:), intent(  out) :: numbers
!
! Local variables
!
   integer(kind=im) :: i
!-------------------------------------------------------------------------------
!    
   do i = 1,size(numbers,2)
     call getrandomnumber_1d(stream, numbers(:, i))
   enddo
!
   end subroutine getrandomnumber_2d
!-------------------------------------------------------------------------------
! mji
!  ! ---------------------------------------------------------------------------
!  ! Constructing a unique seed from grid cell index and model date/time
!  !   Once we have the GFDL stuff we'll add the year, month, day, hour, minute
!  ! ---------------------------------------------------------------------------
!  function constructSeed(i, j, time) result(seed)
!    integer(kind=im),         intent(in   )  :: i, j
!    type(time_type), intent(in   ) :: time
!    integer(kind=im), dimension(8) :: seed
!    
!    ! Local variables
!    integer(kind=im) :: year, month, day, hour, minute, second
!    
!    
!    call get_date(time, year, month, day, hour, minute, second)
!    seed = (/ i, j, year, month, day, hour, minute, second /)
!  end function constructSeed
!
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module mcica_random_numbers_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module mcica_subcol_gen_k
!-------------------------------------------------------------------------------
!   --------------------------------------------------------------------------
!  |                                                                          |
!  |  Copyright 2006-2008, Atmospheric & Environmental Research, Inc. (AER).  |
!  |  This software may be used, copied, or redistributed as long as it is    |
!  |  not sold and this copyright notice is reproduced on each copy made.     |
!  |  This model is provided as is without any express or implied warranties. |
!  |                       (http://www.rtweb.aer.com/)                        |
!  |                                                                          |
!   --------------------------------------------------------------------------
!
!  Purpose: Create McICA stochastic arrays for cloud physical or optical 
!  properties.
!  Two options are possible:
!  1) Input cloud physical properties: cloud fraction, ice and liquid water
!     paths, ice fraction, and particle sizes.  Output will be stochastic
!     arrays of these variables.  (inflag = 1)
!  2) Input cloud optical properties directly: cloud optical depth, single
!     scattering albedo and asymmetry parameter.  Output will be stochastic
!     arrays of these variables.  (inflag = 0; longwave scattering is not
!     yet available, ssac and asmc are for future expansion)
!
!-------------------------------------------------------------------------------
   use parkind_k,  only : im => kind_im, rb => kind_rb
   use parrrtm_k,  only : nbndlw, ngptlw
   use rrlw_con_k, only : grav
   use rrlw_wvn_k, only : ngb
   use rrlw_vsn_k
!
   implicit none
!
! public interfaces/functions/subroutines
!
   public :: mcica_subcol, generate_stochastic_redu 
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
  subroutine mcica_subcol (iplon, ncol, nlay, icld, permuteseed, irng, play,   &
                           cldfrac, ciwp, clwp, ciwpmcl, clwpmcl,              &
                           cswp, cswpmcl,                                      &
                           cldfmcl)
!-------------------------------------------------------------------------------
!
!  abstract :  REDUCED SUBCOLUMN FOR MCICA
!  Sunghye Baek 2016.5.17
!
!  input :
!    iplon        - column/longitude index
!    ncol         - number of columns
!    nlay         - number of model layers
!    icld         - clear/cloud, cloud overlap flag
!    permuteseed  - if the cloud generator is called multiple times, permute 
!                   the seed between each call. 
!                   recommended
!                   permuteseed differes by 'ngpt'
!    irng         - flag for random number generator
!                   0 = kissvec
!                   1 = Mersenne Twister
!    play(:,:)    - layer pressures (mb) 
!                   Dimensions: (ncol,nlay)
!    cldfrac(:,:) - layer cloud fraction
!                   Dimensions: (ncol,nlay)
!    ciwp(:,:)    - in-cloud ice water path
!                   Dimensions: (ncol,nlay)
!    clwp(:,:)    - in-cloud liquid water path
!                   Dimensions: (ncol,nlay)
!    cswp(:,:)    - in-cloud snow path
!                   Dimensions: (ncol,nlay)
!
!  output :
!    cldfmcl(:,:,:) - cloud fraction [mcica]
!                     Dimensions: (ngptlw,ncol,nlay)
!    ciwpmcl(:,:,:) - in-cloud ice water path [mcica]
!                     Dimensions: (ngptlw,ncol,nlay)
!    clwpmcl(:,:,:) - in-cloud liquid water path [mcica]
!                     Dimensions: (ngptlw,ncol,nlay)
!    cswpmcl(:,:,:) - in-cloud snow path [mcica]
!                     Dimensions: (ngptlw,ncol,nlay)
!
!  local variables :
!  nsubclw          - number of sub-columns (g-point intervals)
!  ilev             - loop index
!  pmid(ncol, nlay) - layer pressures (Pa) 
!
!-------------------------------------------------------------------------------
! ----- Input -----
! Control
!
   integer(kind=im), intent(in   ) :: iplon  
   integer(kind=im), intent(in   ) :: ncol  
   integer(kind=im), intent(in   ) :: nlay 
   integer(kind=im), intent(in   ) :: icld  
   integer(kind=im), intent(in   ) :: permuteseed 
   integer(kind=im), intent(inout) :: irng    
!
! Atmosphere
!
   real(kind=rb), dimension(:,:), intent(in   ) :: play
!
! Atmosphere/clouds - cldprop
!
   real(kind=rb), dimension(:,:), intent(in   ) :: cldfrac
   real(kind=rb), dimension(:,:), intent(in   ) :: ciwp
   real(kind=rb), dimension(:,:), intent(in   ) :: clwp
   real(kind=rb), dimension(:,:), intent(in   ) :: cswp
!
! ----- Output -----
!
! Atmosphere/clouds - cldprmc [mcica]
!
   real(kind=rb), dimension(:,:,:), intent(  out) :: cldfmcl
   real(kind=rb), dimension(:,:,:), intent(  out) :: ciwpmcl
   real(kind=rb), dimension(:,:,:), intent(  out) :: clwpmcl
   real(kind=rb), dimension(:,:,:), intent(  out) :: cswpmcl
!
! ----- Local -----
!
! Stochastic cloud generator variables [mcica]
!
   integer(kind=im), parameter :: nsubclw = ngptlw 
   integer(kind=im) :: ilev                       
!
   real(kind=rb), dimension(ncol, nlay) :: pmid
!-------------------------------------------------------------------------------
!
! Return if clear sky; or stop if icld out of range
!
   if (icld.eq.0) return
   if (icld.lt.0.or.icld.gt.3) then
     stop 'MCICA_sUBCOL: INVALID ICLD'
   endif
!
   pmid(:ncol,:nlay) = play(:ncol,:nlay)*1.e2_rb
!
   call generate_stochastic_redu(ncol, nlay, nsubclw, icld, irng, pmid,        &
                                 cldfrac, clwp, ciwp,                          &
                                 cldfmcl, clwpmcl, ciwpmcl,                    &
                                 cswp, cswpmcl,                                &
                                 permuteseed )
!
   end subroutine mcica_subcol
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine generate_stochastic_redu (ncol, nlay, nsubcol, icld, irng, pmid, &
                                        cld, clwp, ciwp,                       &
                                        cld_stoch, clwp_stoch, ciwp_stoch,     &
                                        cswp, cswp_stoch,                      &
                                        changeSeed )
!-------------------------------------------------------------------------------
!
!  input :
!    ncol       - number of columns
!    nlay       - number of layers
!    icld       - clear/cloud, cloud overlap flag
!    irng       - flag for random number generator
!                 0 = kissvec
!                 1 = Mersenne Twister
!    nsubcol    - number of sub-columns (g-point intervals)
!    changeSeed - allows permuting seed
!    pmid(:,:)  - layer pressure (Pa)
!                 Dimensions: (ncol,nlay)
!    cld(:,:)   - cloud fraction 
!                 Dimensions: (ncol,nlay)
!    clwp(:,:)  - in-cloud liquid water path
!                 Dimensions: (ncol,nlay)
!    ciwp(:,:)  - in-cloud ice water path
!                 Dimensions: (ncol,nlay)
!    cswp(:,:)  - in-cloud snow path
!                 Dimensions: (ncol,nlay) 
!  output :
!    cld_stoch(:,:,:)  - subcolumn cloud fraction 
!                        Dimensions: (ngptlw,ncol,nlay)
!    clwp_stoch(:,:,:) - subcolumn in-cloud liquid water path
!                        Dimensions: (ngptlw,ncol,nlay)
!    ciwp_stoch(:,:,:) - subcolumn in-cloud ice water path
!                        Dimensions: (ngptlw,ncol,nlay)
!    cswp_stoch(:,:,:) - subcolumn in-cloud snow path
!                        Dimensions: (ngptlw,ncol,nlay)
!    cswp_stoch(:,:,:) - subcolumn in-cloud snow path
!                        Dimensions: (ngptlw,ncol,nlay)
!
!  local variables :
!    cldf(ncol,nlay)            ! cloud fraction 
!    overlap                    ! 1 = random overlap, 
!                                 2 = maximum/random,
!                                 3 = maximum overlap, 
!    cldmin                     ! min cloud fraction
!    cdf, cdf2                  ! random numbers
!    seed1, seed2, seed3, seed4 ! seed to create random number (kissvec)
!    rand_num                   ! random number (kissvec)
!    iseed                      ! seed to create random number(Mersenne Teister)
!    rand_num_mt                ! random number (Mersenne Twister)
!    iscloudy                   ! flag that says whether a gridbox is cloudy
!    ilev, isubcol, i, n        ! indices
!    nsub28                     ! REDUCED SUBCOL
!
!-------------------------------------------------------------------------------
   use mcica_random_numbers_k
!
! The Mersenne Twister random number engine
!
   use mersennetwister_k, only : randomnumbersequence,                         &
                               new_randomnumbersequence, getrandomreal
!
   type(randomnumbersequence) :: randomnumbers
!
! Arguments
!
   integer(kind=im), intent(in   ) :: ncol    
   integer(kind=im), intent(in   ) :: nlay   
   integer(kind=im), intent(in   ) :: icld   
   integer(kind=im), intent(inout) :: irng   
   integer(kind=im), intent(in   ) :: nsubcol    
   integer(kind=im), optional, intent(in   ) :: changeSeed  
!
! Column state (cloud fraction, cloud water, cloud ice) + 
! variables needed to read physics state 
!
   real(kind=rb), intent(in   ) :: pmid(:,:)   
   real(kind=rb), intent(in   ) :: cld(:,:)    
   real(kind=rb), intent(in   ) :: clwp(:,:)  
   real(kind=rb), intent(in   ) :: ciwp(:,:)   
   real(kind=rb), intent(in   ) :: cswp(:,:) 
   real(kind=rb), intent(  out) :: cld_stoch(:,:,:)
   real(kind=rb), intent(  out) :: clwp_stoch(:,:,:) 
   real(kind=rb), intent(  out) :: ciwp_stoch(:,:,:) 
   real(kind=rb), intent(  out) :: cswp_stoch(:,:,:) 
!
! Local variables
!
   real(kind=rb), dimension(ncol,nlay) :: cldf
!
! Set overlap
!
   integer(kind=im) :: overlap  
!
! Constants (min value for cloud fraction and cloud water and ice)
!
   real(kind=rb), parameter :: cldmin = 1.0e-20_rb 
!
! Variables related to random number and seed 
!
   real(kind=rb), dimension(nsubcol,ncol,nlay) :: cdf, cdf2
   integer(kind=im), dimension(ncol)           :: seed1, seed2, seed3, seed4 
   real(kind=rb), dimension(ncol)              :: rand_num  
   integer(kind=im)                            :: iseed                  
   real(kind=rb)                               :: rand_num_mt              
!
! Flag to identify cloud fraction in subcolumns
!
   logical, dimension(nsubcol,ncol,nlay) :: iscloudy 
!
! Indices
!
   integer(kind=im) :: ilev, isubcol, i, n 
!   
! REDUCED SUBCOL
   integer(kind=im) :: nsub28 = 28 
!-------------------------------------------------------------------------------
!
! Check that irng is in bounds; if not, set to default
!
   if (irng.ne.0) irng = 1
!
! Pass input cloud overlap setting to local variable
!
   overlap = icld
!
! Ensure that cloud fractions are in bounds 
!
   do ilev = 1,nlay
     do i = 1,ncol
       cldf(i,ilev) = cld(i,ilev)
       if (cldf(i,ilev) < cldmin) then
         cldf(i,ilev) = 0._rb
       endif
     enddo
   enddo
!
! ----- Create seed  --------
!   
! Advance randum number generator by changeseed values
!
   if (irng.eq.0) then
!
! For kissvec, create a seed that depends on the state of the columns. 
! Maybe not the best way, but it works.  
! Must use pmid from bottom four layers. 
!
     do i = 1,ncol
       if (pmid(i,1).lt.pmid(i,2)) then
         stop 'MCICA_sUBCOL: KISSVEC SEED GENERATOR REQUIRES PMID FROM '//     &
              'BOTTOM FOUR LAYERS.'
       endif
       seed1(i) = (pmid(i,1) - int(pmid(i,1)))  * 1000000000_im
       seed2(i) = (pmid(i,2) - int(pmid(i,2)))  * 1000000000_im
       seed3(i) = (pmid(i,3) - int(pmid(i,3)))  * 1000000000_im
       seed4(i) = (pmid(i,4) - int(pmid(i,4)))  * 1000000000_im
     enddo
!
     do i = 1,changeSeed
       call kissvec(seed1, seed2, seed3, seed4, rand_num)
     enddo
   else if (irng.eq.1) then
     randomnumbers = new_randomnumbersequence(seed = changeSeed)
   endif
!
! ------ Apply overlap assumption --------
!
! generate the random numbers  
!
   select case (overlap)
     case(2)
!
! Maximum-random overlap
! i) pick a random number for top layer.
! ii) walk down the column: 
! - if the layer above is cloudy, we use the same random number than 
! in the layer above
! - if the layer above is clear, we use a new random number 
!
       if (irng.eq.0) then
!        do isubcol = 1,nsubcol
         do isubcol = 1, nsub28  
           do ilev = 1,nlay
             call kissvec(seed1, seed2, seed3, seed4, rand_num)
             cdf(isubcol,:,ilev) = rand_num
           enddo
         enddo
       else if (irng.eq.1) then
!        do isubcol = 1,nsubcol
         do isubcol = 1, nsub28 
           do i = 1,ncol
             do ilev = 1,nlay
               rand_num_mt = getrandomreal(randomnumbers)
               cdf(isubcol,i,ilev) = rand_num_mt
             enddo
           enddo
         enddo
       endif
!
!      do ilev = 2,nlay
       do ilev = nlay-1,1,-1
         do i = 1,ncol
!          do isubcol = 1,nsubcol
           do isubcol = 1, nsub28
!            if (cdf(isubcol, i, ilev-1)>1._rb-cldf(i,ilev-1) ) then
!              cdf(isubcol,i,ilev) = cdf(isubcol,i,ilev-1)
             if (cdf(isubcol, i, ilev+1) > 1._rb - cldf(i,ilev+1) ) then
               cdf(isubcol,i,ilev) = cdf(isubcol,i,ilev+1)
             else
!              cdf(isubcol,i,ilev) = cdf(isubcol,i,ilev)*(1._rb-cldf(i,ilev-1))
               cdf(isubcol,i,ilev) = cdf(isubcol,i,ilev)*(1._rb-cldf(i,ilev+1)) 
             endif
           enddo
         enddo
       enddo
   end select
!
! !!!!! COPY !!!!!!!!!
!
   cdf(nsub28+1:nsub28*2,:,:)   = cdf(1:nsub28,:,:)
   cdf(nsub28*2+1:nsub28*3,:,:) = cdf(1:nsub28,:,:)
   cdf(nsub28*3+1:nsub28*4,:,:) = cdf(1:nsub28,:,:)
   cdf(nsub28*4+1:nsub28*5,:,:) = cdf(1:nsub28,:,:)
! 
! -- generate subcolumns for homogeneous clouds -----
!
   do ilev = 1,nlay
     iscloudy(:,:,ilev) = (cdf(:,:,ilev)>=                                     &
                           1._rb-spread(cldf(:,ilev), dim=1, nCopies=nsubcol))
   enddo
!
! where the subcolumn is cloudy, the subcolumn cloud fraction is 1;
! where the subcolumn is not cloudy, the subcolumn cloud fraction is 0;
! where there is a cloud, define the subcolumn cloud properties, 
! otherwise set these to zero
!
   do ilev = 1,nlay
     do i = 1,ncol
       do isubcol = 1,nsubcol
         if (iscloudy(isubcol,i,ilev) ) then
           cld_stoch(isubcol,i,ilev) = 1._rb
           clwp_stoch(isubcol,i,ilev) = clwp(i,ilev)
           ciwp_stoch(isubcol,i,ilev) = ciwp(i,ilev)
           cswp_stoch(isubcol,i,ilev) = cswp(i,ilev)
         else
           cld_stoch(isubcol,i,ilev) = 0._rb
           clwp_stoch(isubcol,i,ilev) = 0._rb
           ciwp_stoch(isubcol,i,ilev) = 0._rb
           cswp_stoch(isubcol,i,ilev) = 0._rb
         endif
       enddo
     enddo
   enddo
!
  end subroutine generate_stochastic_redu
!-------------------------------------------------------------------------------
!
! Private subroutines
!
!-------------------------------------------------------------------------------
   subroutine kissvec(seed1,seed2,seed3,seed4,ran_arr)
!-------------------------------------------------------------------------------
!
! public domain code
! made available from http://www.fortran.com/
! downloaded by pjr on 03/16/04 for NCAR CAM
! converted to vector form, functions inlined by pjr,mvr on 05/10/2004
!
! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
!  Overall period>2^123; 
!
!-------------------------------------------------------------------------------
   real(kind=rb), dimension(:), intent(inout)  :: ran_arr
   integer(kind=im), dimension(:), intent(inout) :: seed1,seed2,seed3,seed4
   integer(kind=im) :: i,sz,kiss
   integer(kind=im) :: m, k, n
!-------------------------------------------------------------------------------
!
! inline function 
!
   m(k, n) = ieor (k, ishft (k, n) )
!
   sz = size(ran_arr)
!
   do i = 1,sz
     seed1(i) = 69069_im*seed1(i)+1327217885_im
     seed2(i) = m (m (m (seed2(i), 13_im), - 17_im), 5_im)
     seed3(i) = 18000_im*iand(seed3(i),65535_im)+ishft(seed3(i),- 16_im)
     seed4(i) = 30903_im*iand(seed4(i),65535_im)+ishft(seed4(i),- 16_im)
     kiss = seed1(i)+seed2(i)+ishft(seed3(i),16_im)+seed4(i)
     ran_arr(i) = kiss*2.328306e-10_rb+0.5_rb
   enddo
!    
   end subroutine kissvec
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module mcica_subcol_gen_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_lw_cldprmc_k
!-------------------------------------------------------------------------------
!   --------------------------------------------------------------------------
!  |                                                                          |
!  |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
!  |  This software may be used, copied, or redistributed as long as it is    |
!  |  not sold and this copyright notice is reproduced on each copy made.     |
!  |  This model is provided as is without any express or implied warranties. |
!  |                       (http://www.rtweb.aer.com/)                        |
!  |                                                                          |
!   --------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   use parkind_k,  only : im => kind_im, rb => kind_rb
   use parrrtm_k,  only : ngptlw, nbndlw
   use rrlw_cld_k, only : abscld1, absliq0, absliq1,                           &
                        absice0, absice1, absice2, absice3
   use rrlw_wvn_k, only : ngb
   use rrlw_vsn_k, only : hvrclc, hnamclc
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cldprmc(nlayers, inflag, iceflag, liqflag, cldfmc,               &
                      ciwpmc, clwpmc, reicmc, relqmc,                          &
                      cswpmc, resnmc,                                          &
                      ncbands, taucmc)
!-------------------------------------------------------------------------------
!
!  Purpose:  Compute the cloud optical depth(s) for each cloudy layer.
!
!  input :
!    nlayers      - total number of layers
!    nflag        - see definitions
!    ceflag       - see definitions
!    iqflag       - see definitions
!    cldfmc(:,:)  - cloud fraction [mcica]
!                   Dimensions: (ngptlw,nlayers)
!    ciwpmc(:,:)  - cloud ice water path [mcica]
!                   Dimensions: (ngptlw,nlayers)
!    clwpmc(:,:)  - cloud liquid water path [mcica]
!                   Dimensions: (ngptlw,nlayers)
!    cswpmc(:,:)  - cloud snow path [mcica]
!                   Dimensions: (ngptlw,nlayers)
!    relqmc(:)    - liquid particle effective radius (microns)
!                   Dimensions: (nlayers)
!    reicmc(:)    - ice particle effective radius (microns)
!                   Dimensions: (nlayers)
!    resnmc(:)    - snow particle effective radius (microns)
!                   Dimensions: (nlayers)
!  output :
!    ncbands      - number of cloud spectral bands
!    taucmc(:,:)  - cloud optical depth [mcica]
!                   Dimensions: (ngptlw,nlayers)
!
!  local variables :
!    lay                 - Layer index
!    ib                  - spectral band index
!    ig                  - g-point interval index
!    index
!    icb(nbndlw)
!    abscoice(ngptlw)    - ice absorption coefficients
!    abscoliq(ngptlw)    - liquid absorption coefficients
!    abscosno(ngptlw)    - snow absorption coefficients
!    cwp                 - cloud water path
!    radice              - cloud ice effective size (microns)
!    factor              -
!    fint                -
!    radliq              - cloud liquid droplet radius (microns)
!    radsno              - cloud snow effective size (microns)
!    eps                 - epsilon
!    cldmin              - minimum value for cloud quantities 
!
!-------------------------------------------------------------------------------
!
! Input
!
   integer(kind=im), intent(in   ) :: nlayers      
   integer(kind=im), intent(in   ) :: inflag       
   integer(kind=im), intent(in   ) :: iceflag      
   integer(kind=im), intent(in   ) :: liqflag      
!
   real(kind=rb), dimension(:,:), intent(in   ) :: cldfmc
   real(kind=rb), dimension(:,:), intent(in   ) :: ciwpmc
   real(kind=rb), dimension(:,:), intent(in   ) :: clwpmc
   real(kind=rb), dimension(:,:), intent(in   ) :: cswpmc
   real(kind=rb), dimension(:)  , intent(in   ) :: relqmc
   real(kind=rb), dimension(:)  , intent(in   ) :: reicmc
   real(kind=rb), dimension(:)  , intent(in   ) :: resnmc
!
! specific definition of reicmc depends on setting of iceflag:
! iceflag = 0: ice effective radius, r_ec, (Ebert and Curry, 1992),
!              r_ec must be >= 10.0 microns
! iceflag = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
!              r_ec range is limited to 13.0 to 130.0 microns
! iceflag = 2: ice effective radius, r_k, (Key, streamer Ref. Manual, 1996)
!              r_k range is limited to 5.0 to 131.0 microns
! iceflag = 3: generalized effective size, dge, (Fu, 1996),
!              dge range is limited to 5.0 to 140.0 microns
!              [dge = 1.0315 * r_ec]
!
! Output
!
   integer(kind=im)             , intent(  out) :: ncbands    
   real(kind=rb), dimension(:,:), intent(inout) :: taucmc
!
! Local 
!
   integer(kind=im)                    :: lay                  
   integer(kind=im)                    :: ib                   
   integer(kind=im)                    :: ig                   
   integer(kind=im)                    :: index 
   integer(kind=im), dimension(nbndlw) :: icb
!
   real(kind=rb), dimension(ngptlw) :: abscoice
   real(kind=rb), dimension(ngptlw) :: abscoliq
   real(kind=rb), dimension(ngptlw) :: abscosno
   real(kind=rb) :: cwp                  
   real(kind=rb) :: radice                   
   real(kind=rb) :: factor                    
   real(kind=rb) :: fint                      
   real(kind=rb) :: radliq                    
   real(kind=rb) :: radsno                   
   real(kind=rb), parameter :: eps = 1.e-6_rb     
   real(kind=rb), parameter :: cldmin = 1.e-20_rb 
!
! Definitions
!
!     Explanation of the method for each value of INFLAG.  Values of
!     0 or 1 for INFLAG do not distingish being liquid and ice clouds.
!     INFLAG = 2 does distinguish between liquid and ice clouds, and
!     requires further user input to specify the method to be used to 
!     compute the aborption due to each.
!     INFLAG = 0:  For each cloudy layer, the cloud fraction and (gray)
!                  optical depth are input.  
!     INFLAG = 1:  For each cloudy layer, the cloud fraction and cloud
!                  water path (g/m2) are input.  The (gray) cloud optical 
!                  depth is computed as in CCM2.
!     INFLAG = 2:  For each cloudy layer, the cloud fraction, cloud 
!                  water path (g/m2), and cloud ice fraction are input.
!       ICEFLAG = 0:  The ice effective radius (microns) is input and the
!                     optical depths due to ice clouds are computed as in CCM3.
!       ICEFLAG = 1:  The ice effective radius (microns) is input and the
!                     optical depths due to ice clouds are computed as in 
!                     Ebert and Curry, JGR, 97, 3831-3836 (1992).  The 
!                     spectral regions in this work have been matched with
!                     the spectral bands in RRTM to as great an extent 
!                     as possible:  
!                     E&C 1      IB = 5      RRTM bands 9-16
!                     E&C 2      IB = 4      RRTM bands 6-8
!                     E&C 3      IB = 3      RRTM bands 3-5
!                     E&C 4      IB = 2      RRTM band 2
!                     E&C 5      IB = 1      RRTM band 1
!       ICEFLAG = 2:  The ice effective radius (microns) is input and the
!                     optical properties due to ice clouds are computed from
!                     the optical properties stored in the RT code,
!                     STREAMER v3.0 (Reference: Key. J., streamer 
!                     User's Guide, Cooperative Institute for
!                     Meteorological Satellite Studies, 2001, 96 pp.).
!                     Valid range of values for re are between 5.0 and
!                     131.0 micron.
!       ICEFLAG = 3: The ice generalized effective size (dge) is input
!                    and the optical properties, are calculated as in
!                    Q. Fu, J. Climate, (1998). Q. Fu provided high resolution
!                    tables which were appropriately averaged for the
!                    bands in RRTM_LW.  Linear interpolation is used to
!                    get the coefficients from the stored tables.
!                    Valid range of values for dge are between 5.0 and
!                    140.0 micron.
!       LIQFLAG = 0:  The optical depths due to water clouds are computed as
!                     in CCM3.
!       LIQFLAG = 1:  The water droplet effective radius (microns) is input 
!                     and the optical depths due to water clouds are computed 
!                     as in Hu and Stamnes, J., Clim., 6, 728-742, (1993).
!                     The values for absorption coefficients appropriate for
!                     the spectral bands in RRTM have been obtained for a 
!                     range of effective radii by an averaging procedure 
!                     based on the work of J. Pinto (private communication).
!                     Linear interpolation is used to get the absorption 
!                     coefficients for the input effective radius.
!-------------------------------------------------------------------------------
   data icb /1,2,3,3,3,4,4,4,5, 5, 5, 5, 5, 5, 5, 5/
!
   hvrclc = '$Revision: 1.8 $'
!
   ncbands = 1
!
! This initialization is done in rrtmg_lw_subcol.F90.
!      do lay = 1, nlayers
!         do ig = 1, ngptlw
!            taucmc(ig,lay) = 0.0_rb
!         enddo
!      enddo
!
! Main layer loop
!
   do lay = 1,nlayers
!
     do ig = 1,ngptlw
       cwp = ciwpmc(ig,lay)+clwpmc(ig,lay)+cswpmc(ig,lay)
       if (cldfmc(ig,lay).ge.cldmin .and.                                      &
          (cwp.ge.cldmin .or. taucmc(ig,lay).ge.cldmin)) then
!
! Ice clouds and water clouds combined.
!
         if (inflag.eq.0) then
!
! Cloud optical depth already defined in taucmc, return to main program
!
           return
!
         else if (inflag.eq.1) then 
           stop 'INFLAG = 1 OPTION NOT AVAILABLE WITH MCICA'
!          cwp = ciwpmc(ig,lay) + clwpmc(ig,lay)
!          taucmc(ig,lay) = abscld1 * cwp
!
! Separate treatement of ice clouds and water clouds.
!
         else if (inflag.ge.2) then
           radice = reicmc(lay)
!
! Calculation of absorption coefficients due to ice clouds.
!
           if ((ciwpmc(ig,lay)+cswpmc(ig,lay)).eq.0.0_rb) then
             abscoice(ig) = 0.0_rb
             abscosno(ig) = 0.0_rb
           else if (iceflag.eq.0) then
             if (radice.lt.10.0_rb) stop 'ICE RADIUS TOO SMALL'
             abscoice(ig) = absice0(1) + absice0(2)/radice
             abscosno(ig) = 0.0_rb
           else if (iceflag.eq.1) then
             if (radice.lt.13.0_rb .or. radice.gt.130._rb) stop                &
               'ICE RADIUS out OF BOUNDS'
             ncbands = 5
             ib = icb(ngb(ig))
             abscoice(ig) = absice1(1,ib)+absice1(2,ib)/radice
             abscosno(ig) = 0.0_rb
!
! For iceflag=2 option, ice particle effective radius is limited 
! to 5.0 to 131.0 microns
!
           else if (iceflag.eq.2) then
             if (radice.lt.5.0_rb .or. radice.gt.131.0_rb) stop                &
               'ICE RADIUS out OF BOUNDS'
             ncbands = 16
             factor = (radice-2._rb)/3._rb
             index = int(factor)
             if (index.eq.43) index = 42
             fint = factor-real(index)
             ib = ngb(ig)
             abscoice(ig) = absice2(index,ib)+fint*                            &
                           (absice2(index+1,ib)-(absice2(index,ib))) 
             abscosno(ig) = 0.0_rb
!               
! For iceflag=3 option, ice particle generalized effective size is limited 
! to 5.0 to 140.0 microns
!
           else if (iceflag .ge. 3) then
             if (radice.lt.5.0_rb .or. radice.gt.140.0_rb) stop                &
               'ICE GENERALIZED EFFECTIVE SIZE out OF BOUNDS'
             ncbands = 16
             factor = (radice-2._rb)/3._rb
             index = int(factor)
             if (index.eq.46) index = 45
             fint = factor-real(index)
             ib = ngb(ig)
             abscoice(ig) = absice3(index,ib)+fint*                            &
                           (absice3(index+1,ib)-(absice3(index,ib)))
             abscosno(ig) = 0.0_rb
           endif
!
! Incorporate additional effects due to snow.
!
           if (cswpmc(ig,lay).gt.0.0_rb .and. iceflag.eq.5) then
             radsno = resnmc(lay)
             if (radsno.lt.5.0_rb .or. radsno.gt.140.0_rb) stop                &
               'ERROR: SNOW GENERALIZED EFFECTIVE SIZE out OF BOUNDS'   
             ncbands = 16
             factor = (radsno-2._rb)/3._rb
             index = int(factor)
             if (index.eq.46) index = 45
             fint = factor-real(index)
             ib = ngb(ig)
             abscosno(ig) = absice3(index,ib)+fint*                            &
                           (absice3(index+1,ib) - (absice3(index,ib)))
           endif
!                  
! Calculation of absorption coefficients due to water clouds.
!
           if (clwpmc(ig,lay).eq.0.0_rb) then
             abscoliq(ig) = 0.0_rb
!
           else if (liqflag.eq.0) then
             abscoliq(ig) = absliq0
!
           else if (liqflag.eq.1) then
             radliq = relqmc(lay)
             if (radliq.lt.2.5_rb .or. radliq.gt.60._rb) stop                  &
                       'LIQUID EFFECTIVE RADIUS out OF BOUNDS'
             index = int(radliq-1.5_rb)
             if (index.eq.0) index = 1
             if (index.eq.58) index = 57
             fint = radliq-1.5_rb-real(index)
             ib = ngb(ig)
             abscoliq(ig) = absliq1(index,ib)+fint*                            &
                           (absliq1(index+1,ib)-(absliq1(index,ib)))
           endif
!
           taucmc(ig,lay) = ciwpmc(ig,lay)*abscoice(ig) +                      &
                            clwpmc(ig,lay)*abscoliq(ig) +                      &
                            cswpmc(ig,lay)*abscosno(ig)
         endif
       endif
     enddo
   enddo
!
   end subroutine cldprmc
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_lw_cldprmc_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_lw_rtrnmc_k
!-------------------------------------------------------------------------------
!   --------------------------------------------------------------------------
!  |                                                                          |
!  |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
!  |  This software may be used, copied, or redistributed as long as it is    |
!  |  not sold and this copyright notice is reproduced on each copy made.     |
!  |  This model is provided as is without any express or implied warranties. |
!  |                       (http://www.rtweb.aer.com/)                        |
!  |                                                                          |
!   --------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   use parkind_k,  only : im => kind_im, rb => kind_rb
   use parrrtm_k,  only : mg, nbndlw, ngptlw
   use rrlw_con_k, only : fluxfac, heatfac
   use rrlw_wvn_k, only : delwave, ngb, ngs
   use rrlw_tbl_k, only : tblint, bpade, tau_tbl, exp_tbl, tfn_tbl
   use rrlw_vsn_k, only : hvrrtc, hnamrtc
!
   implicit none
!
   real(kind=rb) :: wtdiff, rec_6
!
! diffusivity angle adjustment coefficients
!
   real(kind=rb), dimension(nbndlw) :: a0, a1, a2
!
! This secant and weight corresponds to the standard diffusivity 
! angle.  This initial value is redefined below for some bands.
!
   data wtdiff /0.5_rb/
   data rec_6 /0.166667_rb/
!
! Reset diffusivity angle for Bands 2-3 and 5-9 to vary (between 1.50
! and 1.80) as a function of total column water vapor.  The function
! has been defined to minimize flux and cooling rate errors in these bands
! over a wide range of precipitable water values.
!
   data a0 / 1.66_rb,  1.55_rb,   1.58_rb,  1.66_rb,                           &
             1.54_rb,  1.454_rb,  1.89_rb,  1.33_rb,                           &
             1.668_rb, 1.66_rb,   1.66_rb,  1.66_rb,                           &
             1.66_rb,  1.66_rb,   1.66_rb,  1.66_rb /
   data a1 / 0.00_rb,  0.25_rb,   0.22_rb,  0.00_rb,                           &
             0.13_rb,  0.446_rb, -0.10_rb,  0.40_rb,                           &
            -0.006_rb, 0.00_rb,   0.00_rb,  0.00_rb,                           &
             0.00_rb,  0.00_rb,   0.00_rb,  0.00_rb /
   data a2 / 0.00_rb, -12.0_rb,  -11.7_rb,  0.00_rb,                           &
            -0.72_rb, -0.243_rb,  0.19_rb, -0.062_rb,                          &
             0.414_rb, 0.00_rb,   0.00_rb,  0.00_rb,                           &
             0.00_rb,  0.00_rb,   0.00_rb,  0.00_rb /
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands,         &
                     cldfmc, taucmc, planklay, planklev, plankbnd,             &
                     pwvcm, fracs, taut,                                       &
                     totuflux, totdflux, fnet, htr,                            &
                     totuclfl, totdclfl, fnetc, htrc ) 
!-------------------------------------------------------------------------------
!
!  Original version:   E. J. Mlawer, et al. RRTM_v3.0
!  Revision for GCMs:  Michael J. Iacono; October, 2002
!  Revision for F90:  Michael J. Iacono; June, 2006
!
!  This program calculates the upward fluxes, downward fluxes, and
!  heating rates for an arbitrary clear or cloudy atmosphere.  The input
!  to this program is the atmospheric profile, all Planck function
!  information, and the cloud fraction by layer.  A variable diffusivity 
!  angle (SECDIFF) is used for the angle integration.  Bands 2-3 and 5-9 
!  use a value for SECDIFF that varies from 1.50 to 1.80 as a function of 
!  the column water vapor, and other bands use a value of 1.66.  The Gaussian 
!  weight appropriate to this angle (WTDIFF=0.5) is applied here.  Note that 
!  use of the emissivity angle for the flux integration can cause errors of 
!  1 to 4 W/m2 within cloudy layers.  
!  Clouds are treated with the McICA stochastic approach and maximum-random
!  cloud overlap. 
!
!  input :
!    nlayers         - total number of layers
!    istart          - beginning band of calculation
!    iend            - ending band of calculation
!    iout            - output option flag
!    pz(0:)          - level (interface) pressures (hPa, mb)
!                      Dimensions: (0:nlayers)
!    pwvcm           - precipitable water vapor (cm)
!    semiss(:)       - lw surface emissivity
!                      Dimensions: (nbndlw)
!    planklay(:,:) 
!                      Dimensions: (nlayers,nbndlw)
!    planklev(0:,:)  
!                      Dimensions: (0:nlayers,nbndlw)
!    plankbnd(:)     
!                      Dimensions: (nbndlw)
!    fracs(:,:)      
!                      Dimensions: (nlayers,ngptw)
!    taut(:,:)       - gaseous + aerosol optical depths
!                      Dimensions: (nlayers,ngptlw)
!    ncbands         - number of cloud spectral bands
!    cldfmc(:,:)     - layer cloud fraction [mcica]
!                      Dimensions: (ngptlw,nlayers)
!    taucmc(:,:)     - layer cloud optical depth [mcica]
!                      Dimensions: (ngptlw,nlayers) 
!  output :
!    totuflux(0:)    - upward longwave flux (w/m2)
!                      Dimensions: (0:nlayers)
!    totdflux(0:)    - downward longwave flux (w/m2)
!                      Dimensions: (0:nlayers)
!    fnet(0:)        - net longwave flux (w/m2)
!                      Dimensions: (0:nlayers)
!    htr(0:)         - longwave heating rate (k/day)
!                      Dimensions: (0:nlayers)
!    totuclfl(0:)    - clear sky upward longwave flux (w/m2)
!                      Dimensions: (0:nlayers)
!    totdclfl(0:)    - clear sky downward longwave flux (w/m2)
!                      Dimensions: (0:nlayers)
!    fnetc(0:)       - clear sky net longwave flux (w/m2)
!                      Dimensions: (0:nlayers)
!    htrc(0:)        - clear sky longwave heating rate (k/day)
!                      Dimensions: (0:nlayers)
! 
!  local variables :
!    secdiff(nbndlw)                   - secant of diffusivity angle
!    icldlyr(nlayers)                  - flag for cloud in layer
!    ibnd, ib, iband, lay, lev, l, ig  - loop indices
!    igc                               - g-point interval counter
!    iclddn                            - flag for cloud in down path
!    ittot, itgas, itr                 - lookup table indices
!
! ------- Definitions -------
!  input
!    nlayers                    ! number of model layers
!    ngptlw                     ! total number of g-point subintervals
!    nbndlw                     ! number of longwave spectral bands
!    ncbands                    ! number of spectral bands for clouds
!    secdiff                    ! diffusivity angle
!    wtdiff                     ! weight for radiance to flux conversion
!    pavel                      ! layer pressures (mb)
!    pz                         ! level (interface) pressures (mb)
!    tavel                      ! layer temperatures (k)
!    tz                         ! level (interface) temperatures(mb)
!    tbound                     ! surface temperature (k)
!    cldfrac                    ! layer cloud fraction
!    taucloud                   ! layer cloud optical depth
!    itr                        ! integer look-up table index
!    icldlyr                    ! flag for cloudy layers
!    iclddn                     ! flag for cloud in column at any layer
!    semiss                     ! surface emissivities for each band
!    reflect                    ! surface reflectance
!    bpade                      ! 1/(pade constant)
!    tau_tbl                    ! clear sky optical depth look-up table
!    exp_tbl                    ! exponential look-up table for transmittance
!    tfn_tbl                    ! tau transition function look-up table
!
!  local
!    atrans                     ! gaseous absorptivity
!    abscld                     ! cloud absorptivity
!    atot                       ! combined gaseous and cloud absorptivity
!    odclr                      ! clear sky (gaseous) optical depth
!    odcld                      ! cloud optical depth
!    odtot                      ! optical depth of gas and cloud
!    tfacgas                    ! gas-only pade factor, used for planck fn
!    tfactot                    ! gas and cloud pade factor, used for planck fn
!    bbdgas                     ! gas-only planck function for downward rt
!    bbugas                     ! gas-only planck function for upward rt
!    bbdtot                     ! gas and cloud planck function for downward rt
!    bbutot                     ! gas and cloud planck function for upward calc.
!    gassrc                     ! source radiance due to gas only
!    efclfrac                   ! effective cloud fraction
!    radlu                      ! spectrally summed upward radiance 
!    radclru                    ! spectrally summed clear sky upward radiance 
!    urad                       ! upward radiance by layer
!    clrurad                    ! clear sky upward radiance by layer
!    radld                      ! spectrally summed downward radiance 
!    radclrd                    ! spectrally summed clear sky downward radiance 
!    drad                       ! downward radiance by layer
!    clrdrad                    ! clear sky downward radiance by layer
!
!  output
!    totuflux                   ! upward longwave flux (w/m2)
!    totdflux                   ! downward longwave flux (w/m2)
!    fnet                       ! net longwave flux (w/m2)
!    htr                        ! longwave heating rate (k/day)
!    totuclfl                   ! clear sky upward longwave flux (w/m2)
!    totdclfl                   ! clear sky downward longwave flux (w/m2)
!    fnetc                      ! clear sky net longwave flux (w/m2)
!    htrc                       ! clear sky longwave heating rate (k/day)
!
!-------------------------------------------------------------------------------
!
! Declarations
!
! Input
!
   integer(kind=im), intent(in   ) :: nlayers   
   integer(kind=im), intent(in   ) :: istart   
   integer(kind=im), intent(in   ) :: iend     
   integer(kind=im), intent(in   ) :: iout     
!
! Atmosphere
!
   real(kind=rb), dimension(0:)  , intent(in   ) :: pz
   real(kind=rb)                 , intent(in   ) :: pwvcm      
   real(kind=rb), dimension(:)   , intent(in   ) :: semiss
   real(kind=rb), dimension(:,:) , intent(in   ) :: planklay
   real(kind=rb), dimension(0:,:), intent(in   ) :: planklev
   real(kind=rb), dimension(:)   , intent(in   ) :: plankbnd
   real(kind=rb), dimension(:,:) , intent(in   ) :: fracs
   real(kind=rb), dimension(:,:) , intent(in   ) :: taut
!
! Clouds
!
   integer(kind=im)             , intent(in   ) :: ncbands      
   real(kind=rb), dimension(:,:), intent(in   ) :: cldfmc
   real(kind=rb), dimension(:,:), intent(in   ) :: taucmc
!
! Output
!
   real(kind=rb), dimension(0:), intent(  out) :: totuflux
   real(kind=rb), dimension(0:), intent(  out) :: totdflux
   real(kind=rb), dimension(0:), intent(  out) :: fnet
   real(kind=rb), dimension(0:), intent(  out) :: htr
   real(kind=rb), dimension(0:), intent(  out) :: totuclfl
   real(kind=rb), dimension(0:), intent(  out) :: totdclfl
   real(kind=rb), dimension(0:), intent(  out) :: fnetc
   real(kind=rb), dimension(0:), intent(  out) :: htrc
!
! Local 
!
! Declarations for radiative transfer
!
   real(kind=rb), dimension(nlayers,ngptlw) :: abscld
   real(kind=rb), dimension(nlayers)        :: atot
   real(kind=rb), dimension(nlayers)        :: atrans
   real(kind=rb), dimension(nlayers)        :: bbugas
   real(kind=rb), dimension(nlayers)        :: bbutot
   real(kind=rb), dimension(0:nlayers)      :: clrurad
   real(kind=rb), dimension(0:nlayers)      :: clrdrad
   real(kind=rb), dimension(nlayers,ngptlw) :: efclfrac
   real(kind=rb), dimension(0:nlayers)      :: uflux
   real(kind=rb), dimension(0:nlayers)      :: dflux
   real(kind=rb), dimension(0:nlayers)      :: urad
   real(kind=rb), dimension(0:nlayers)      :: drad
   real(kind=rb), dimension(0:nlayers)      :: uclfl
   real(kind=rb), dimension(0:nlayers)      :: dclfl
   real(kind=rb), dimension(nlayers,ngptlw) :: odcld
!
   real(kind=rb), dimension(nbndlw)         :: secdiff
!
   real(kind=rb) :: transcld, radld, radclrd, plfrac, blay, dplankup, dplankdn
   real(kind=rb) :: odepth, odtot, odepth_rec, odtot_rec, gassrc
   real(kind=rb) :: tblind, tfactot, bbd, bbdtot, tfacgas, transc, tausfac
   real(kind=rb) :: rad0, reflect, radlu, radclru
!
   integer(kind=im), dimension(nlayers) :: icldlyr     
   integer(kind=im) :: ibnd, ib, iband, lay, lev, l, ig 
   integer(kind=im) :: igc                              
   integer(kind=im) :: iclddn                           
   integer(kind=im) :: ittot, itgas, itr                
!-------------------------------------------------------------------------------
!
   hvrrtc = '$Revision: 1.3 $'
!
   do ibnd = 1,nbndlw
     if (ibnd.eq.1 .or. ibnd.eq.4 .or. ibnd.ge.10) then
       secdiff(ibnd) = 1.66_rb
     else
       secdiff(ibnd) = a0(ibnd)+a1(ibnd)*exp(a2(ibnd)*pwvcm)
       if (secdiff(ibnd).gt.1.80_rb) secdiff(ibnd) = 1.80_rb
       if (secdiff(ibnd).lt.1.50_rb) secdiff(ibnd) = 1.50_rb
     endif
   enddo
!
   urad     = 0.0_rb
   drad     = 0.0_rb
   totuflux = 0.0_rb
   totdflux = 0.0_rb
   clrurad  = 0.0_rb
   clrdrad  = 0.0_rb
   totuclfl = 0.0_rb
   totdclfl = 0.0_rb
   icldlyr  = 0
!
   do lay = 1,nlayers
!
! Change to band loop?
!
     do ig = 1,ngptlw
       if (cldfmc(ig,lay).eq.1._rb) then
         ib = ngb(ig)
         odcld(lay,ig) = secdiff(ib)*taucmc(ig,lay)
         transcld = exp(-odcld(lay,ig))
         abscld(lay,ig) = 1._rb-transcld
         efclfrac(lay,ig) = abscld(lay,ig)*cldfmc(ig,lay)
         icldlyr(lay) = 1
       else
         odcld(lay,ig) = 0.0_rb
         abscld(lay,ig) = 0.0_rb
         efclfrac(lay,ig) = 0.0_rb
       endif
     enddo
   enddo
!
   igc = 1
!
! Loop over frequency bands.
!
   do iband = istart,iend
!
! Reinitialize g-point counter for each band if output for each band 
! is requested.
!
     if (iout.gt.0 .and. iband.ge.2) igc = ngs(iband-1)+1
!
! Loop over g-channels.
!
     1000 continue
!
! Radiative transfer starts here.
!
     radld = 0._rb
     radclrd = 0._rb
     iclddn = 0
!
! Downward radiative transfer loop.  
!
     do lev = nlayers,1,-1
       plfrac = fracs(lev,igc)
       blay = planklay(lev,iband)
       dplankup = planklev(lev,iband)-blay
       dplankdn = planklev(lev-1,iband)-blay
       odepth = secdiff(iband)*taut(lev,igc)
       if (odepth.lt.0.0_rb) odepth = 0.0_rb
!
!  Cloudy layer
!
       if (icldlyr(lev).eq.1) then
         iclddn = 1
         odtot = odepth+odcld(lev,igc)
         if (odtot.lt.0.06_rb) then
           atrans(lev) = odepth-0.5_rb*odepth*odepth
           odepth_rec = rec_6*odepth
           gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)
!
           atot(lev) =  odtot - 0.5_rb*odtot*odtot
           odtot_rec = rec_6*odtot
           bbdtot =  plfrac * (blay+dplankdn*odtot_rec)
           bbd = plfrac*(blay+dplankdn*odepth_rec)
           radld = radld-radld*(atrans(lev)+efclfrac(lev,igc)*                 &
                   (1.-atrans(lev)))+&
                   gassrc + cldfmc(igc,lev)*(bbdtot*atot(lev)-gassrc)
           drad(lev-1) = drad(lev-1)+radld
!                  
           bbugas(lev) =  plfrac*(blay+dplankup*odepth_rec)
           bbutot(lev) =  plfrac*(blay+dplankup*odtot_rec)
!
         else if (odepth.le.0.06_rb) then
           atrans(lev) = odepth-0.5_rb*odepth*odepth
           odepth_rec = rec_6*odepth
           gassrc = plfrac*(blay+dplankdn*odepth_rec)*atrans(lev)
!
           odtot = odepth+odcld(lev,igc)
           tblind = odtot/(bpade+odtot)
           ittot = tblint*tblind+0.5_rb
           tfactot = tfn_tbl(ittot)
           bbdtot = plfrac*(blay+tfactot*dplankdn)
           bbd = plfrac*(blay+dplankdn*odepth_rec)
           atot(lev) = 1.-exp_tbl(ittot)
!
           radld = radld-radld*(atrans(lev)+                                   &
                   efclfrac(lev,igc)*(1._rb-atrans(lev)))+                     &
                   gassrc+cldfmc(igc,lev)*(bbdtot*atot(lev)-gassrc)
           drad(lev-1) = drad(lev-1)+radld
!
           bbugas(lev) = plfrac*(blay+dplankup*odepth_rec)
           bbutot(lev) = plfrac*(blay+tfactot*dplankup)
!
         else
!
           tblind = odepth/(bpade+odepth)
           itgas = tblint*tblind+0.5_rb
           odepth = tau_tbl(itgas)
           atrans(lev) = 1._rb-exp_tbl(itgas)
           tfacgas = tfn_tbl(itgas)
           gassrc = atrans(lev)*plfrac*(blay+tfacgas*dplankdn)
!
           odtot = odepth+odcld(lev,igc)
           tblind = odtot/(bpade+odtot)
           ittot = tblint*tblind+0.5_rb
           tfactot = tfn_tbl(ittot)
           bbdtot = plfrac*(blay+tfactot*dplankdn)
           bbd = plfrac*(blay+tfacgas*dplankdn)
           atot(lev) = 1._rb-exp_tbl(ittot)
!
           radld = radld-radld*(atrans(lev)+                                   &
                   efclfrac(lev,igc)*(1._rb-atrans(lev)))+                     &
                   gassrc + cldfmc(igc,lev)*(bbdtot*atot(lev)-gassrc)
           drad(lev-1) = drad(lev-1) + radld
           bbugas(lev) = plfrac*(blay+tfacgas*dplankup)
           bbutot(lev) = plfrac*(blay+tfactot*dplankup)
         endif
!
! Clear layer
!
       else
         if (odepth.le.0.06_rb) then
           atrans(lev) = odepth-0.5_rb*odepth*odepth
           odepth = rec_6*odepth
           bbd = plfrac*(blay+dplankdn*odepth)
           bbugas(lev) = plfrac*(blay+dplankup*odepth)
         else
           tblind = odepth/(bpade+odepth)
           itr = tblint*tblind+0.5_rb
           transc = exp_tbl(itr)
           atrans(lev) = 1._rb-transc
           tausfac = tfn_tbl(itr)
           bbd = plfrac*(blay+tausfac*dplankdn)
           bbugas(lev) = plfrac*(blay+tausfac*dplankup)
         endif   
         radld = radld + (bbd-radld)*atrans(lev)
         drad(lev-1) = drad(lev-1)+radld
       endif
!
! Set clear sky stream to total sky stream as long as layers
! remain clear.  streams diverge when a cloud is reached (iclddn=1),
! and clear sky stream must be computed separately from that point.
!
       if (iclddn.eq.1) then
         radclrd = radclrd+(bbd-radclrd)*atrans(lev) 
         clrdrad(lev-1) = clrdrad(lev-1)+radclrd
       else
         radclrd = radld
         clrdrad(lev-1) = drad(lev-1)
       endif
     enddo
!
! Spectral emissivity & reflectance
! Include the contribution of spectrally varying longwave emissivity
! and reflection from the surface to the upward radiative transfer.
! Note: Spectral and Lambertian reflection are identical for the
! diffusivity angle flux integration used here.
!
     rad0 = fracs(1,igc)*plankbnd(iband)
!
! Add in specular reflection of surface downward radiance.
!
     reflect = 1._rb-semiss(iband)
     radlu = rad0+reflect*radld
     radclru = rad0+reflect*radclrd
!
! Upward radiative transfer loop.
!
     urad(0) = urad(0)+radlu
     clrurad(0) = clrurad(0)+radclru
!
     do lev = 1,nlayers
!
! Cloudy layer
!
       if (icldlyr(lev).eq.1) then
         gassrc = bbugas(lev)*atrans(lev)
         radlu = radlu-radlu*(atrans(lev)+                                     &
                 efclfrac(lev,igc)*(1._rb-atrans(lev)))+                       &
                 gassrc+cldfmc(igc,lev)*(bbutot(lev)*atot(lev)-gassrc)
         urad(lev) = urad(lev)+radlu
!
! Clear layer
!
       else
         radlu = radlu+(bbugas(lev)-radlu)*atrans(lev)
         urad(lev) = urad(lev)+radlu
       endif
!
! Set clear sky stream to total sky stream as long as all layers
! are clear (iclddn=0).  streams must be calculated separately at 
! all layers when a cloud is present (ICLDDN=1), because surface 
! reflectance is different for each stream.
!
       if (iclddn.eq.1) then
         radclru = radclru+(bbugas(lev)-radclru)*atrans(lev) 
         clrurad(lev) = clrurad(lev)+radclru
       else
         radclru = radlu
         clrurad(lev) = urad(lev)
       endif
     enddo
!
! Increment g-point counter
!
     igc = igc + 1
!
! Return to continue radiative transfer for all g-channels in present band
!
     if (igc.le.ngs(iband)) go to 1000
!
! Process longwave output from band for total and clear streams.
! Calculate upward, downward, and net flux.
!
     do lev = nlayers,0,-1
       uflux(lev) = urad(lev)*wtdiff
       dflux(lev) = drad(lev)*wtdiff
       urad(lev) = 0.0_rb
       drad(lev) = 0.0_rb
       totuflux(lev) = totuflux(lev)+uflux(lev)*delwave(iband)
       totdflux(lev) = totdflux(lev)+dflux(lev)*delwave(iband)
       uclfl(lev) = clrurad(lev)*wtdiff
       dclfl(lev) = clrdrad(lev)*wtdiff
       clrurad(lev) = 0.0_rb
       clrdrad(lev) = 0.0_rb
       totuclfl(lev) = totuclfl(lev)+uclfl(lev)*delwave(iband)
       totdclfl(lev) = totdclfl(lev)+dclfl(lev)*delwave(iband)
     enddo
!
! End spectral band loop
!
   enddo
!
! Calculate fluxes at surface
!
   totuflux(0) = totuflux(0)*fluxfac
   totdflux(0) = totdflux(0)*fluxfac
   fnet(0) = totuflux(0)-totdflux(0)
   totuclfl(0) = totuclfl(0)*fluxfac
   totdclfl(0) = totdclfl(0)*fluxfac
   fnetc(0) = totuclfl(0)-totdclfl(0)
!
! Calculate fluxes at model levels
!
   do lev = 1,nlayers
     totuflux(lev) = totuflux(lev)*fluxfac
     totdflux(lev) = totdflux(lev)*fluxfac
     fnet(lev) = totuflux(lev)-totdflux(lev)
     totuclfl(lev) = totuclfl(lev)*fluxfac
     totdclfl(lev) = totdclfl(lev)*fluxfac
     fnetc(lev) = totuclfl(lev)-totdclfl(lev)
     l = lev-1
!
! Calculate heating rates at model layers
!
     htr(l)=heatfac*(fnet(l)-fnet(lev))/(pz(l)-pz(lev)) 
     htrc(l)=heatfac*(fnetc(l)-fnetc(lev))/(pz(l)-pz(lev)) 
   enddo
!
! Set heating rate to zero in top layer
!
   htr(nlayers) = 0.0_rb
   htrc(nlayers) = 0.0_rb
!
   end subroutine rtrnmc
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_lw_rtrnmc_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_lw_setcoef_k
!-------------------------------------------------------------------------------
!   --------------------------------------------------------------------------
!  |                                                                          |
!  |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
!  |  This software may be used, copied, or redistributed as long as it is    |
!  |  not sold and this copyright notice is reproduced on each copy made.     |
!  |  This model is provided as is without any express or implied warranties. |
!  |                       (http://www.rtweb.aer.com/)                        |
!  |                                                                          |
!   --------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   use parkind_k,  only : im => kind_im, rb => kind_rb
   use parrrtm_k,  only : nbndlw, mg, maxxsec, mxmol
   use rrlw_wvn_k, only : totplnk, totplk16
   use rrlw_ref_k
   use rrlw_vsn_k, only : hvrset, hnamset
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss,       &
                      coldry, wkl, wbroad,                                     &
                      laytrop, jp, jt, jt1, planklay, planklev, plankbnd,      &
                      colh2o, colco2, colo3, coln2o, colco, colch4, colo2,     &
                      colbrd, fac00, fac01, fac10, fac11,                      &
                      rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,        &
                      rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,      &
                      rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,        &
                      selffac, selffrac, indself, forfac, forfrac, indfor,     &
                      minorfrac, scaleminor, scaleminorn2, indminor)
!-------------------------------------------------------------------------------
!
!  Purpose:  For a given atmosphere, calculate the indices and
!  fractions related to the pressure and temperature interpolations.
!  Also calculate the values of the integrated Planck functions 
!  for each band at the level and layer temperatures.
!
!  input :
!    nlayers         - total number of layers
!    istart          - beginning band of calculation
!    pavel(:)        - layer pressures (mb)
!                      Dimensions: (nlayers)
!    tavel(:)        - layer temperatures (K)
!                      Dimensions: (nlayers)
!    tz(0:)          - level (interface) temperatures (K)
!                      Dimensions: (0:nlayers)
!    tbound          - surface temperature (K)
!    coldry(:)       - dry air column density (mol/cm2)
!                      Dimensions: (nlayers)
!    wbroad(:)       - broadening gas column density (mol/cm2)
!                      Dimensions: (nlayers)
!    wkl(:,:)        - molecular amounts (mol/cm-2)
!                      Dimensions: (mxmol,nlayers)
!    semiss(:)       - lw surface emissivity
!                      Dimensions: (nbndlw)
!  output :
!    laytrop                    - tropopause layer index
!    jp(nlayers)                -
!    jt(nlayers)    
!    jt1(nlayers)   
!    planklay(nlayers,nbndlw)
!    planklev(0:nlayers,nbndlw)
!    plankbnd(nbndlw)
!    colh2o(nlayers)            - column amount (h2o)
!    colco2(nlayers)            - column amount (co2)
!    colo3(nlayers)             - column amount (o3)
!    coln2o(nlayers)            - column amount (n2o)
!    colco(nlayers)             - column amount (co)
!    colch4(nlayers)            - column amount (ch4)
!    colo2(nlayers)             - column amount (o2)
!    colbrd(nlayers)            - column amount (broadening gases)
!
!    indself(nlayers)
!    indfor(nlayers)
!    selffac(nlayers)
!    selffrac(nlayers)
!    forfac(nlayers)
!    forfrac(nlayers)
!    indminor(nlayers)
!    minorfrac(nlayers)
!    scaleminor(nlayers)
!    scaleminorn2(nlayers)
!    minorfrac(nlayers)
!    scaleminor(nlayers)
!    scaleminorn2(nlayers)
!    fac00(nlayers), fac01(nlayers), fac10(nlayers), fac11(nlayers)
!    rat_h2oco2(nlayers),rat_h2oco2_1(nlayers)
!    rat_h2oo3(nlayers),rat_h2oo3_1(nlayers)
!    rat_h2on2o(nlayers),rat_h2on2o_1(nlayers)
!    rat_h2och4(nlayers),rat_h2och4_1(nlayers)
!    rat_n2oco2(nlayers),rat_n2oco2_1(nlayers)
!    rat_o3co2(nlayers),rat_o3co2_1(nlayers)
!
!  local varialbles :
!-------------------------------------------------------------------------------
!
! Input
!
   integer(kind=im), intent(in   ) :: nlayers  
   integer(kind=im), intent(in   ) :: istart   
!
   real(kind=rb), dimension(:)   , intent(in   ) :: pavel
   real(kind=rb), dimension(:)   , intent(in   ) :: tavel
   real(kind=rb), dimension(0:)  , intent(in   ) :: tz
   real(kind=rb)                 , intent(in   ) :: tbound      
   real(kind=rb), dimension(:)   , intent(in   ) :: coldry
   real(kind=rb), dimension(:)   , intent(in   ) :: wbroad
   real(kind=rb), dimension(:,:) , intent(in   ) :: wkl
   real(kind=rb), dimension(:)   , intent(in   ) :: semiss
!
! Output
!
   integer(kind=im)              , intent(  out) :: laytrop    
   integer(kind=im), dimension(:), intent(  out) :: jp
   integer(kind=im), dimension(:), intent(  out) :: jt
   integer(kind=im), dimension(:), intent(  out) :: jt1
   real(kind=rb), dimension(:,:) , intent(  out) :: planklay
   real(kind=rb), dimension(0:,:), intent(  out) :: planklev
   real(kind=rb), dimension(:)   , intent(  out) :: plankbnd
!
   real(kind=rb), dimension(:), intent(  out) :: colh2o      
   real(kind=rb), dimension(:), intent(  out) :: colco2      
   real(kind=rb), dimension(:), intent(  out) :: colo3      
   real(kind=rb), dimension(:), intent(  out) :: coln2o     
   real(kind=rb), dimension(:), intent(  out) :: colco      
   real(kind=rb), dimension(:), intent(  out) :: colch4     
   real(kind=rb), dimension(:), intent(  out) :: colo2      
   real(kind=rb), dimension(:), intent(  out) :: colbrd     
!
   integer(kind=im), dimension(:), intent(  out) :: indself
   integer(kind=im), dimension(:), intent(  out) :: indfor
   real(kind=rb), dimension(:)   , intent(  out) :: selffac
   real(kind=rb), dimension(:)   , intent(  out) :: selffrac
   real(kind=rb), dimension(:)   , intent(  out) :: forfac
   real(kind=rb), dimension(:)   , intent(  out) :: forfrac
!
   integer(kind=im), dimension(:), intent(  out) :: indminor
   real(kind=rb), dimension(:)   , intent(  out) :: minorfrac
   real(kind=rb), dimension(:)   , intent(  out) :: scaleminor
   real(kind=rb), dimension(:)   , intent(  out) :: scaleminorn2
!
   real(kind=rb), dimension(:)   , intent(  out) :: fac00, fac01, fac10, fac11
   real(kind=rb), dimension(:)   , intent(  out) :: rat_h2oco2, rat_h2oco2_1,  &
                                                      rat_h2oo3, rat_h2oo3_1,  &
                                                     rat_h2on2o,rat_h2on2o_1,  &
                                                     rat_h2och4,rat_h2och4_1,  &
                                                     rat_n2oco2,rat_n2oco2_1,  &
                                                       rat_o3co2,rat_o3co2_1 
!
! Local
!
   integer(kind=im) :: indbound, indlev0
   integer(kind=im) :: lay, indlay, indlev, iband
   integer(kind=im) :: jp1
   real(kind=rb)    :: stpfac, tbndfrac, t0frac, tlayfrac, tlevfrac
   real(kind=rb)    :: dbdtlev, dbdtlay
   real(kind=rb)    :: plog, fp, ft, ft1, water, scalefac, factor, compfp
!-------------------------------------------------------------------------------
!
   hvrset = '$Revision: 1.3 $'
!
   stpfac = 296._rb/1013._rb
!
   indbound = tbound-159._rb
!
   if (indbound.lt.1) then
     indbound = 1
   else if (indbound.gt.180) then
     indbound = 180
   endif
!
   tbndfrac = tbound-159._rb-real(indbound)
   indlev0 = tz(0)-159._rb
!
   if (indlev0.lt.1) then
     indlev0 = 1
   else if (indlev0.gt.180) then
     indlev0 = 180
   endif
!
   t0frac = tz(0)-159._rb-real(indlev0)
   laytrop = 0
!
! Begin layer loop 
! Calculate the integrated Planck functions for each band at the
! surface, level, and layer temperatures.
!
   do lay = 1,nlayers
     indlay = tavel(lay)-159._rb
     if (indlay.lt.1) then
       indlay = 1
     else if (indlay.gt.180) then
       indlay = 180
     endif
!
     tlayfrac = tavel(lay)-159._rb-real(indlay)
     indlev = tz(lay)-159._rb
     if (indlev.lt.1) then
       indlev = 1
     else if (indlev.gt.180) then
       indlev = 180
     endif
     tlevfrac = tz(lay)-159._rb-real(indlev)
!
! Begin spectral band loop 
!
     do iband = 1,15
       if (lay.eq.1) then
         dbdtlev = totplnk(indbound+1,iband)-totplnk(indbound,iband)
         plankbnd(iband) = semiss(iband)*                                      &
                          (totplnk(indbound,iband)+tbndfrac*dbdtlev)
         dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
         planklev(0,iband) = totplnk(indlev0,iband)+t0frac*dbdtlev
       endif
       dbdtlev = totplnk(indlev+1,iband)-totplnk(indlev,iband)
       dbdtlay = totplnk(indlay+1,iband)-totplnk(indlay,iband)
       planklay(lay,iband) = totplnk(indlay,iband)+tlayfrac*dbdtlay
       planklev(lay,iband) = totplnk(indlev,iband)+tlevfrac*dbdtlev
     enddo
!
! For band 16, if radiative transfer will be performed on just
! this band, use integrated Planck values up to 3250 cm-1.  
! If radiative transfer will be performed across all 16 bands,
! then include in the integrated Planck values for this band
! contributions from 2600 cm-1 to infinity.
!
     iband = 16
     if (istart.eq.16) then
       if (lay.eq.1) then
         dbdtlev = totplk16(indbound+1)-totplk16(indbound)
         plankbnd(iband) = semiss(iband)*                                      &
                          (totplk16(indbound)+tbndfrac*dbdtlev)
         dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
         planklev(0,iband) = totplk16(indlev0)+t0frac*dbdtlev
       endif
       dbdtlev = totplk16(indlev+1)-totplk16(indlev)
       dbdtlay = totplk16(indlay+1)-totplk16(indlay)
       planklay(lay,iband) = totplk16(indlay)+tlayfrac*dbdtlay
       planklev(lay,iband) = totplk16(indlev)+tlevfrac*dbdtlev
     else
       if (lay.eq.1) then
         dbdtlev = totplnk(indbound+1,iband)-totplnk(indbound,iband)
         plankbnd(iband) = semiss(iband)*                                      &
                          (totplnk(indbound,iband)+tbndfrac*dbdtlev)
         dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
         planklev(0,iband) = totplnk(indlev0,iband)+t0frac*dbdtlev
       endif
       dbdtlev = totplnk(indlev+1,iband)-totplnk(indlev,iband)
       dbdtlay = totplnk(indlay+1,iband)-totplnk(indlay,iband)
       planklay(lay,iband) = totplnk(indlay,iband)+tlayfrac*dbdtlay
       planklev(lay,iband) = totplnk(indlev,iband)+tlevfrac*dbdtlev
     endif
!
! Find the two reference pressures on either side of the
! layer pressure.  Store them in JP and JP1.  Store in FP the
! fraction of the difference (in ln(pressure)) between these
! two values that the layer pressure lies.
!
     plog = log(pavel(lay))
!    plog = dlog(pavel(lay))
     jp(lay) = int(36._rb - 5*(plog+0.04_rb))
!
     if (jp(lay).lt.1) then
       jp(lay) = 1
     else if (jp(lay).gt.58) then
       jp(lay) = 58
     endif
!
     jp1 = jp(lay)+1
     fp = 5._rb*(preflog(jp(lay))-plog)
!
! Determine, for each reference pressure (JP and JP1), which
! reference temperature (these are different for each  
! reference pressure) is nearest the layer temperature but does
! not exceed it.  Store these indices in JT and JT1, resp.
! Store in FT (resp. FT1) the fraction of the way between JT
! (JT1) and the next highest reference temperature that the 
! layer temperature falls.
!
     jt(lay) = int(3._rb+(tavel(lay)-tref(jp(lay)))/15._rb)
!
     if (jt(lay).lt.1) then
       jt(lay) = 1
     else if (jt(lay).gt.4) then
       jt(lay) = 4
     endif
!
     ft = ((tavel(lay)-tref(jp(lay)))/15._rb)-real(jt(lay)-3)
     jt1(lay) = int(3._rb+(tavel(lay)-tref(jp1))/15._rb)
!
     if (jt1(lay).lt.1) then
       jt1(lay) = 1
     else if (jt1(lay).gt.4) then
       jt1(lay) = 4
     endif
!
     ft1 = ((tavel(lay)-tref(jp1))/15._rb)-real(jt1(lay)-3)
     water = wkl(1,lay)/coldry(lay)
     scalefac = pavel(lay)*stpfac /tavel(lay)
!
! If the pressure is less than ~100mb, perform a different
! set of species interpolations.
!
     if (plog.le.4.56_rb) go to 5300
     laytrop =  laytrop+1
!
     forfac(lay) = scalefac/(1.+water)
     factor = (332.0_rb-tavel(lay))/36.0_rb
     indfor(lay) = min(2, max(1,int(factor)))
     forfrac(lay) = factor-real(indfor(lay))
!
! Set up factors needed to separately include the water vapor
! self-continuum in the calculation of absorption coefficient.
!
     selffac(lay) = water*forfac(lay)
     factor = (tavel(lay)-188.0_rb)/7.2_rb
     indself(lay) = min(9, max(1,int(factor)-7))
     selffrac(lay) = factor-real(indself(lay)+ 7)
!
! Set up factors needed to separately include the minor gases
! in the calculation of absorption coefficient
!
     scaleminor(lay) = pavel(lay)/tavel(lay)
     scaleminorn2(lay) = (pavel(lay)/tavel(lay))                               &
                        *(wbroad(lay)/(coldry(lay)+wkl(1,lay)))
     factor = (tavel(lay)-180.8_rb)/7.2_rb
     indminor(lay) = min(18,max(1,int(factor)))
     minorfrac(lay) = factor-real(indminor(lay))
!
! Setup reference ratio to be used in calculation of binary
! species parameter in lower atmosphere.
!
     rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
     rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)
!
     rat_h2oo3(lay)=chi_mls(1,jp(lay))/chi_mls(3,jp(lay))
     rat_h2oo3_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(3,jp(lay)+1)
!
     rat_h2on2o(lay)=chi_mls(1,jp(lay))/chi_mls(4,jp(lay))
     rat_h2on2o_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(4,jp(lay)+1)
!
     rat_h2och4(lay)=chi_mls(1,jp(lay))/chi_mls(6,jp(lay))
     rat_h2och4_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(6,jp(lay)+1)
!
     rat_n2oco2(lay)=chi_mls(4,jp(lay))/chi_mls(2,jp(lay))
     rat_n2oco2_1(lay)=chi_mls(4,jp(lay)+1)/chi_mls(2,jp(lay)+1)
!
! Calculate needed column amounts.
!
     colh2o(lay) = 1.e-20_rb*wkl(1,lay)
     colco2(lay) = 1.e-20_rb*wkl(2,lay)
     colo3(lay) = 1.e-20_rb*wkl(3,lay)
     coln2o(lay) = 1.e-20_rb*wkl(4,lay)
     colco(lay) = 1.e-20_rb*wkl(5,lay)
     colch4(lay) = 1.e-20_rb*wkl(6,lay)
     colo2(lay) = 1.e-20_rb*wkl(7,lay)
     if (colco2(lay).eq.0._rb) colco2(lay) = 1.e-32_rb*coldry(lay)
     if (colo3(lay).eq.0._rb)  colo3(lay)  = 1.e-32_rb*coldry(lay)
     if (coln2o(lay).eq.0._rb) coln2o(lay) = 1.e-32_rb*coldry(lay)
     if (colco(lay).eq.0._rb)  colco(lay)  = 1.e-32_rb*coldry(lay)
     if (colch4(lay).eq.0._rb) colch4(lay) = 1.e-32_rb*coldry(lay)
     colbrd(lay) = 1.e-20_rb*wbroad(lay)
     go to 5400
!
! Above laytrop.
!
     5300 continue
!
     forfac(lay) = scalefac/(1.+water)
     factor = (tavel(lay)-188.0_rb)/36.0_rb
     indfor(lay) = 3
     forfrac(lay) = factor-1.0_rb
!
! Set up factors needed to separately include the water vapor
! self-continuum in the calculation of absorption coefficient.
!
     selffac(lay) = water*forfac(lay)
!
! Set up factors needed to separately include the minor gases
! in the calculation of absorption coefficient
!
     scaleminor(lay) = pavel(lay)/tavel(lay)         
     scaleminorn2(lay) = (pavel(lay)/tavel(lay))                               &
                        *(wbroad(lay)/(coldry(lay)+wkl(1,lay)))
     factor = (tavel(lay)-180.8_rb)/7.2_rb
     indminor(lay) = min(18,max(1,int(factor)))
     minorfrac(lay) = factor-real(indminor(lay))
!
! Setup reference ratio to be used in calculation of binary
! species parameter in upper atmosphere.
!
     rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
     rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)         
!
     rat_o3co2(lay)=chi_mls(3,jp(lay))/chi_mls(2,jp(lay))
     rat_o3co2_1(lay)=chi_mls(3,jp(lay)+1)/chi_mls(2,jp(lay)+1)         
!
! Calculate needed column amounts.
!
     colh2o(lay) = 1.e-20_rb*wkl(1,lay)
     colco2(lay) = 1.e-20_rb*wkl(2,lay)
     colo3(lay) = 1.e-20_rb*wkl(3,lay)
     coln2o(lay) = 1.e-20_rb*wkl(4,lay)
     colco(lay) = 1.e-20_rb*wkl(5,lay)
     colch4(lay) = 1.e-20_rb*wkl(6,lay)
     colo2(lay) = 1.e-20_rb*wkl(7,lay)
     if (colco2(lay).eq.0._rb) colco2(lay) = 1.e-32_rb*coldry(lay)
     if (colo3(lay).eq.0._rb)  colo3(lay)  = 1.e-32_rb*coldry(lay)
     if (coln2o(lay).eq.0._rb) coln2o(lay) = 1.e-32_rb*coldry(lay)
     if (colco(lay).eq.0._rb)  colco(lay)  = 1.e-32_rb*coldry(lay)
     if (colch4(lay).eq.0._rb) colch4(lay) = 1.e-32_rb*coldry(lay)
     colbrd(lay) = 1.e-20_rb*wbroad(lay)
     5400    continue
!
! We have now isolated the layer ln pressure and temperature,
! between two reference pressures and two reference temperatures 
! (for each reference pressure).  We multiply the pressure 
! fraction FP with the appropriate temperature fractions to get 
! the factors that will be needed for the interpolation that yields
! the optical depths (performed in routines TAUGBn for band n).`
!
     compfp = 1.-fp
     fac10(lay) = compfp*ft
     fac00(lay) = compfp*(1._rb-ft)
     fac11(lay) = fp*ft1
     fac01(lay) = fp*(1._rb-ft1)
!
! Rescale selffac and forfac for use in taumol
!
     selffac(lay) = colh2o(lay)*selffac(lay)
     forfac(lay) = colh2o(lay)*forfac(lay)
!
! End layer loop
!
   enddo
!
   end subroutine setcoef
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lwatmref
!-------------------------------------------------------------------------------
! 
!  These pressures are chosen such that the ln of the first pressure
!  has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
!  each subsequent ln(pressure) differs from the previous one by 0.2.
!
!-------------------------------------------------------------------------------
!
   save
!
   pref(:) = (/                                                                &
    1.05363e+03_rb,8.62642e+02_rb,7.06272e+02_rb,5.78246e+02_rb,4.73428e+02_rb,&
    3.87610e+02_rb,3.17348e+02_rb,2.59823e+02_rb,2.12725e+02_rb,1.74164e+02_rb,&
    1.42594e+02_rb,1.16746e+02_rb,9.55835e+01_rb,7.82571e+01_rb,6.40715e+01_rb,&
    5.24573e+01_rb,4.29484e+01_rb,3.51632e+01_rb,2.87892e+01_rb,2.35706e+01_rb,&
    1.92980e+01_rb,1.57998e+01_rb,1.29358e+01_rb,1.05910e+01_rb,8.67114e+00_rb,&
    7.09933e+00_rb,5.81244e+00_rb,4.75882e+00_rb,3.89619e+00_rb,3.18993e+00_rb,&
    2.61170e+00_rb,2.13828e+00_rb,1.75067e+00_rb,1.43333e+00_rb,1.17351e+00_rb,&
    9.60789e-01_rb,7.86628e-01_rb,6.44036e-01_rb,5.27292e-01_rb,4.31710e-01_rb,&
    3.53455e-01_rb,2.89384e-01_rb,2.36928e-01_rb,1.93980e-01_rb,1.58817e-01_rb,&
    1.30029e-01_rb,1.06458e-01_rb,8.71608e-02_rb,7.13612e-02_rb,5.84256e-02_rb,&
    4.78349e-02_rb,3.91639e-02_rb,3.20647e-02_rb,2.62523e-02_rb,2.14936e-02_rb,&
    1.75975e-02_rb,1.44076e-02_rb,1.17959e-02_rb,9.65769e-03_rb/)
   preflog(:) = (/                                                             &
    6.9600e+00_rb, 6.7600e+00_rb, 6.5600e+00_rb, 6.3600e+00_rb, 6.1600e+00_rb, &
    5.9600e+00_rb, 5.7600e+00_rb, 5.5600e+00_rb, 5.3600e+00_rb, 5.1600e+00_rb, &
    4.9600e+00_rb, 4.7600e+00_rb, 4.5600e+00_rb, 4.3600e+00_rb, 4.1600e+00_rb, &
    3.9600e+00_rb, 3.7600e+00_rb, 3.5600e+00_rb, 3.3600e+00_rb, 3.1600e+00_rb, &
    2.9600e+00_rb, 2.7600e+00_rb, 2.5600e+00_rb, 2.3600e+00_rb, 2.1600e+00_rb, &
    1.9600e+00_rb, 1.7600e+00_rb, 1.5600e+00_rb, 1.3600e+00_rb, 1.1600e+00_rb, &
    9.6000e-01_rb, 7.6000e-01_rb, 5.6000e-01_rb, 3.6000e-01_rb, 1.6000e-01_rb, &
   -4.0000e-02_rb,-2.4000e-01_rb,-4.4000e-01_rb,-6.4000e-01_rb,-8.4000e-01_rb, &
   -1.0400e+00_rb,-1.2400e+00_rb,-1.4400e+00_rb,-1.6400e+00_rb,-1.8400e+00_rb, &
   -2.0400e+00_rb,-2.2400e+00_rb,-2.4400e+00_rb,-2.6400e+00_rb,-2.8400e+00_rb, &
   -3.0400e+00_rb,-3.2400e+00_rb,-3.4400e+00_rb,-3.6400e+00_rb,-3.8400e+00_rb, &
   -4.0400e+00_rb,-4.2400e+00_rb,-4.4400e+00_rb,-4.6400e+00_rb/)
!
! These are the temperatures associated with the respective 
! pressures for the mls standard atmosphere. 
!
   tref(:) = (/                                                                &
    2.9420e+02_rb, 2.8799e+02_rb, 2.7894e+02_rb, 2.6925e+02_rb, 2.5983e+02_rb, &
    2.5017e+02_rb, 2.4077e+02_rb, 2.3179e+02_rb, 2.2306e+02_rb, 2.1578e+02_rb, &
    2.1570e+02_rb, 2.1570e+02_rb, 2.1570e+02_rb, 2.1706e+02_rb, 2.1858e+02_rb, &
    2.2018e+02_rb, 2.2174e+02_rb, 2.2328e+02_rb, 2.2479e+02_rb, 2.2655e+02_rb, &
    2.2834e+02_rb, 2.3113e+02_rb, 2.3401e+02_rb, 2.3703e+02_rb, 2.4022e+02_rb, &
    2.4371e+02_rb, 2.4726e+02_rb, 2.5085e+02_rb, 2.5457e+02_rb, 2.5832e+02_rb, &
    2.6216e+02_rb, 2.6606e+02_rb, 2.6999e+02_rb, 2.7340e+02_rb, 2.7536e+02_rb, &
    2.7568e+02_rb, 2.7372e+02_rb, 2.7163e+02_rb, 2.6955e+02_rb, 2.6593e+02_rb, &
    2.6211e+02_rb, 2.5828e+02_rb, 2.5360e+02_rb, 2.4854e+02_rb, 2.4348e+02_rb, &
    2.3809e+02_rb, 2.3206e+02_rb, 2.2603e+02_rb, 2.2000e+02_rb, 2.1435e+02_rb, &
    2.0887e+02_rb, 2.0340e+02_rb, 1.9792e+02_rb, 1.9290e+02_rb, 1.8809e+02_rb, &
    1.8329e+02_rb, 1.7849e+02_rb, 1.7394e+02_rb, 1.7212e+02_rb/)
!
   chi_mls(1,1:12) = (/                                                        &
    1.8760e-02_rb, 1.2223e-02_rb, 5.8909e-03_rb, 2.7675e-03_rb, 1.4065e-03_rb, &
    7.5970e-04_rb, 3.8876e-04_rb, 1.6542e-04_rb, 3.7190e-05_rb, 7.4765e-06_rb, &
    4.3082e-06_rb, 3.3319e-06_rb/)
   chi_mls(1,13:59) = (/                                                       &
    3.2039e-06_rb, 3.1619e-06_rb, 3.2524e-06_rb, 3.4226e-06_rb, 3.6288e-06_rb, &
    3.9148e-06_rb, 4.1488e-06_rb, 4.3081e-06_rb, 4.4420e-06_rb, 4.5778e-06_rb, &
    4.7087e-06_rb, 4.7943e-06_rb, 4.8697e-06_rb, 4.9260e-06_rb, 4.9669e-06_rb, &
    4.9963e-06_rb, 5.0527e-06_rb, 5.1266e-06_rb, 5.2503e-06_rb, 5.3571e-06_rb, &
    5.4509e-06_rb, 5.4830e-06_rb, 5.5000e-06_rb, 5.5000e-06_rb, 5.4536e-06_rb, &
    5.4047e-06_rb, 5.3558e-06_rb, 5.2533e-06_rb, 5.1436e-06_rb, 5.0340e-06_rb, &
    4.8766e-06_rb, 4.6979e-06_rb, 4.5191e-06_rb, 4.3360e-06_rb, 4.1442e-06_rb, &
    3.9523e-06_rb, 3.7605e-06_rb, 3.5722e-06_rb, 3.3855e-06_rb, 3.1988e-06_rb, &
    3.0121e-06_rb, 2.8262e-06_rb, 2.6407e-06_rb, 2.4552e-06_rb, 2.2696e-06_rb, &
    4.3360e-06_rb, 4.1442e-06_rb/)
   chi_mls(2,1:12) = (/                                                        &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb/)
   chi_mls(2,13:59) = (/                                                       &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, 3.5500e-04_rb, &
    3.5500e-04_rb, 3.5471e-04_rb, 3.5427e-04_rb, 3.5384e-04_rb, 3.5340e-04_rb, &
    3.5500e-04_rb, 3.5500e-04_rb/)
   chi_mls(3,1:12) = (/                                                        &
    3.0170e-08_rb, 3.4725e-08_rb, 4.2477e-08_rb, 5.2759e-08_rb, 6.6944e-08_rb, &
    8.7130e-08_rb, 1.1391e-07_rb, 1.5677e-07_rb, 2.1788e-07_rb, 3.2443e-07_rb, &
    4.6594e-07_rb, 5.6806e-07_rb/)
   chi_mls(3,13:59) = (/                                                       &
    6.9607e-07_rb, 1.1186e-06_rb, 1.7618e-06_rb, 2.3269e-06_rb, 2.9577e-06_rb, &
    3.6593e-06_rb, 4.5950e-06_rb, 5.3189e-06_rb, 5.9618e-06_rb, 6.5113e-06_rb, &
    7.0635e-06_rb, 7.6917e-06_rb, 8.2577e-06_rb, 8.7082e-06_rb, 8.8325e-06_rb, &
    8.7149e-06_rb, 8.0943e-06_rb, 7.3307e-06_rb, 6.3101e-06_rb, 5.3672e-06_rb, &
    4.4829e-06_rb, 3.8391e-06_rb, 3.2827e-06_rb, 2.8235e-06_rb, 2.4906e-06_rb, &
    2.1645e-06_rb, 1.8385e-06_rb, 1.6618e-06_rb, 1.5052e-06_rb, 1.3485e-06_rb, &
    1.1972e-06_rb, 1.0482e-06_rb, 8.9926e-07_rb, 7.6343e-07_rb, 6.5381e-07_rb, &
    5.4419e-07_rb, 4.3456e-07_rb, 3.6421e-07_rb, 3.1194e-07_rb, 2.5967e-07_rb, &
    2.0740e-07_rb, 1.9146e-07_rb, 1.9364e-07_rb, 1.9582e-07_rb, 1.9800e-07_rb, &
    7.6343e-07_rb, 6.5381e-07_rb/)
   chi_mls(4,1:12) = (/                                                        &
    3.2000e-07_rb, 3.2000e-07_rb, 3.2000e-07_rb, 3.2000e-07_rb, 3.2000e-07_rb, &
    3.1965e-07_rb, 3.1532e-07_rb, 3.0383e-07_rb, 2.9422e-07_rb, 2.8495e-07_rb, &
    2.7671e-07_rb, 2.6471e-07_rb/)
   chi_mls(4,13:59) = (/                                                       &
    2.4285e-07_rb, 2.0955e-07_rb, 1.7195e-07_rb, 1.3749e-07_rb, 1.1332e-07_rb, &
    1.0035e-07_rb, 9.1281e-08_rb, 8.5463e-08_rb, 8.0363e-08_rb, 7.3372e-08_rb, &
    6.5975e-08_rb, 5.6039e-08_rb, 4.7090e-08_rb, 3.9977e-08_rb, 3.2979e-08_rb, &
    2.6064e-08_rb, 2.1066e-08_rb, 1.6592e-08_rb, 1.3017e-08_rb, 1.0090e-08_rb, &
    7.6249e-09_rb, 6.1159e-09_rb, 4.6672e-09_rb, 3.2857e-09_rb, 2.8484e-09_rb, &
    2.4620e-09_rb, 2.0756e-09_rb, 1.8551e-09_rb, 1.6568e-09_rb, 1.4584e-09_rb, &
    1.3195e-09_rb, 1.2072e-09_rb, 1.0948e-09_rb, 9.9780e-10_rb, 9.3126e-10_rb, &
    8.6472e-10_rb, 7.9818e-10_rb, 7.5138e-10_rb, 7.1367e-10_rb, 6.7596e-10_rb, &
    6.3825e-10_rb, 6.0981e-10_rb, 5.8600e-10_rb, 5.6218e-10_rb, 5.3837e-10_rb, &
    9.9780e-10_rb, 9.3126e-10_rb/)
   chi_mls(5,1:12) = (/                                                        &
    1.5000e-07_rb, 1.4306e-07_rb, 1.3474e-07_rb, 1.3061e-07_rb, 1.2793e-07_rb, &
    1.2038e-07_rb, 1.0798e-07_rb, 9.4238e-08_rb, 7.9488e-08_rb, 6.1386e-08_rb, &
    4.5563e-08_rb, 3.3475e-08_rb/)
   chi_mls(5,13:59) = (/                                                       &
    2.5118e-08_rb, 1.8671e-08_rb, 1.4349e-08_rb, 1.2501e-08_rb, 1.2407e-08_rb, &
    1.3472e-08_rb, 1.4900e-08_rb, 1.6079e-08_rb, 1.7156e-08_rb, 1.8616e-08_rb, &
    2.0106e-08_rb, 2.1654e-08_rb, 2.3096e-08_rb, 2.4340e-08_rb, 2.5643e-08_rb, &
    2.6990e-08_rb, 2.8456e-08_rb, 2.9854e-08_rb, 3.0943e-08_rb, 3.2023e-08_rb, &
    3.3101e-08_rb, 3.4260e-08_rb, 3.5360e-08_rb, 3.6397e-08_rb, 3.7310e-08_rb, &
    3.8217e-08_rb, 3.9123e-08_rb, 4.1303e-08_rb, 4.3652e-08_rb, 4.6002e-08_rb, &
    5.0289e-08_rb, 5.5446e-08_rb, 6.0603e-08_rb, 6.8946e-08_rb, 8.3652e-08_rb, &
    9.8357e-08_rb, 1.1306e-07_rb, 1.4766e-07_rb, 1.9142e-07_rb, 2.3518e-07_rb, &
    2.7894e-07_rb, 3.5001e-07_rb, 4.3469e-07_rb, 5.1938e-07_rb, 6.0407e-07_rb, &
    6.8946e-08_rb, 8.3652e-08_rb/)
   chi_mls(6,1:12) = (/                                                        &
    1.7000e-06_rb, 1.7000e-06_rb, 1.6999e-06_rb, 1.6904e-06_rb, 1.6671e-06_rb, &
    1.6351e-06_rb, 1.6098e-06_rb, 1.5590e-06_rb, 1.5120e-06_rb, 1.4741e-06_rb, &
    1.4385e-06_rb, 1.4002e-06_rb/)
   chi_mls(6,13:59) = (/                                                       &
    1.3573e-06_rb, 1.3130e-06_rb, 1.2512e-06_rb, 1.1668e-06_rb, 1.0553e-06_rb, &
    9.3281e-07_rb, 8.1217e-07_rb, 7.5239e-07_rb, 7.0728e-07_rb, 6.6722e-07_rb, &
    6.2733e-07_rb, 5.8604e-07_rb, 5.4769e-07_rb, 5.1480e-07_rb, 4.8206e-07_rb, &
    4.4943e-07_rb, 4.1702e-07_rb, 3.8460e-07_rb, 3.5200e-07_rb, 3.1926e-07_rb, &
    2.8646e-07_rb, 2.5498e-07_rb, 2.2474e-07_rb, 1.9588e-07_rb, 1.8295e-07_rb, &
    1.7089e-07_rb, 1.5882e-07_rb, 1.5536e-07_rb, 1.5304e-07_rb, 1.5072e-07_rb, &
    1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, &
    1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, &
    1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, 1.5000e-07_rb, &
    1.5000e-07_rb, 1.5000e-07_rb/)
   chi_mls(7,1:12) = (/                                                        &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb/)
   chi_mls(7,13:59) = (/                                                       &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb, 0.2090_rb,                     &
    0.2090_rb, 0.2090_rb/)
!
   end subroutine lwatmref
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lwavplank
!-------------------------------------------------------------------------------
!
   save
! 
   totplnk(1:50,  1) = (/                                                      &
   0.14783e-05_rb,0.15006e-05_rb,0.15230e-05_rb,0.15455e-05_rb,0.15681e-05_rb, &
   0.15908e-05_rb,0.16136e-05_rb,0.16365e-05_rb,0.16595e-05_rb,0.16826e-05_rb, &
   0.17059e-05_rb,0.17292e-05_rb,0.17526e-05_rb,0.17762e-05_rb,0.17998e-05_rb, &
   0.18235e-05_rb,0.18473e-05_rb,0.18712e-05_rb,0.18953e-05_rb,0.19194e-05_rb, &
   0.19435e-05_rb,0.19678e-05_rb,0.19922e-05_rb,0.20166e-05_rb,0.20412e-05_rb, &
   0.20658e-05_rb,0.20905e-05_rb,0.21153e-05_rb,0.21402e-05_rb,0.21652e-05_rb, &
   0.21902e-05_rb,0.22154e-05_rb,0.22406e-05_rb,0.22659e-05_rb,0.22912e-05_rb, &
   0.23167e-05_rb,0.23422e-05_rb,0.23678e-05_rb,0.23934e-05_rb,0.24192e-05_rb, &
   0.24450e-05_rb,0.24709e-05_rb,0.24968e-05_rb,0.25229e-05_rb,0.25490e-05_rb, &
   0.25751e-05_rb,0.26014e-05_rb,0.26277e-05_rb,0.26540e-05_rb,0.26805e-05_rb/)
   totplnk(51:100,  1) = (/                                                    &
   0.27070e-05_rb,0.27335e-05_rb,0.27602e-05_rb,0.27869e-05_rb,0.28136e-05_rb, &
   0.28404e-05_rb,0.28673e-05_rb,0.28943e-05_rb,0.29213e-05_rb,0.29483e-05_rb, &
   0.29754e-05_rb,0.30026e-05_rb,0.30298e-05_rb,0.30571e-05_rb,0.30845e-05_rb, &
   0.31119e-05_rb,0.31393e-05_rb,0.31669e-05_rb,0.31944e-05_rb,0.32220e-05_rb, &
   0.32497e-05_rb,0.32774e-05_rb,0.33052e-05_rb,0.33330e-05_rb,0.33609e-05_rb, &
   0.33888e-05_rb,0.34168e-05_rb,0.34448e-05_rb,0.34729e-05_rb,0.35010e-05_rb, &
   0.35292e-05_rb,0.35574e-05_rb,0.35857e-05_rb,0.36140e-05_rb,0.36424e-05_rb, &
   0.36708e-05_rb,0.36992e-05_rb,0.37277e-05_rb,0.37563e-05_rb,0.37848e-05_rb, &
   0.38135e-05_rb,0.38421e-05_rb,0.38708e-05_rb,0.38996e-05_rb,0.39284e-05_rb, &
   0.39572e-05_rb,0.39861e-05_rb,0.40150e-05_rb,0.40440e-05_rb,0.40730e-05_rb/)
   totplnk(101:150,  1) = (/                                                   &
   0.41020e-05_rb,0.41311e-05_rb,0.41602e-05_rb,0.41893e-05_rb,0.42185e-05_rb, &
   0.42477e-05_rb,0.42770e-05_rb,0.43063e-05_rb,0.43356e-05_rb,0.43650e-05_rb, &
   0.43944e-05_rb,0.44238e-05_rb,0.44533e-05_rb,0.44828e-05_rb,0.45124e-05_rb, &
   0.45419e-05_rb,0.45715e-05_rb,0.46012e-05_rb,0.46309e-05_rb,0.46606e-05_rb, &
   0.46903e-05_rb,0.47201e-05_rb,0.47499e-05_rb,0.47797e-05_rb,0.48096e-05_rb, &
   0.48395e-05_rb,0.48695e-05_rb,0.48994e-05_rb,0.49294e-05_rb,0.49594e-05_rb, &
   0.49895e-05_rb,0.50196e-05_rb,0.50497e-05_rb,0.50798e-05_rb,0.51100e-05_rb, &
   0.51402e-05_rb,0.51704e-05_rb,0.52007e-05_rb,0.52309e-05_rb,0.52612e-05_rb, &
   0.52916e-05_rb,0.53219e-05_rb,0.53523e-05_rb,0.53827e-05_rb,0.54132e-05_rb, &
   0.54436e-05_rb,0.54741e-05_rb,0.55047e-05_rb,0.55352e-05_rb,0.55658e-05_rb/)
   totplnk(151:181,  1) = (/                                                   &
   0.55964e-05_rb,0.56270e-05_rb,0.56576e-05_rb,0.56883e-05_rb,0.57190e-05_rb, &
   0.57497e-05_rb,0.57804e-05_rb,0.58112e-05_rb,0.58420e-05_rb,0.58728e-05_rb, &
   0.59036e-05_rb,0.59345e-05_rb,0.59653e-05_rb,0.59962e-05_rb,0.60272e-05_rb, &
   0.60581e-05_rb,0.60891e-05_rb,0.61201e-05_rb,0.61511e-05_rb,0.61821e-05_rb, &
   0.62131e-05_rb,0.62442e-05_rb,0.62753e-05_rb,0.63064e-05_rb,0.63376e-05_rb, &
   0.63687e-05_rb,0.63998e-05_rb,0.64310e-05_rb,0.64622e-05_rb,0.64935e-05_rb, &
   0.65247e-05_rb/)
   totplnk(1:50,  2) = (/                                                      &
   0.20262e-05_rb,0.20757e-05_rb,0.21257e-05_rb,0.21763e-05_rb,0.22276e-05_rb, &
   0.22794e-05_rb,0.23319e-05_rb,0.23849e-05_rb,0.24386e-05_rb,0.24928e-05_rb, &
   0.25477e-05_rb,0.26031e-05_rb,0.26591e-05_rb,0.27157e-05_rb,0.27728e-05_rb, &
   0.28306e-05_rb,0.28889e-05_rb,0.29478e-05_rb,0.30073e-05_rb,0.30673e-05_rb, &
   0.31279e-05_rb,0.31890e-05_rb,0.32507e-05_rb,0.33129e-05_rb,0.33757e-05_rb, &
   0.34391e-05_rb,0.35029e-05_rb,0.35674e-05_rb,0.36323e-05_rb,0.36978e-05_rb, &
   0.37638e-05_rb,0.38304e-05_rb,0.38974e-05_rb,0.39650e-05_rb,0.40331e-05_rb, &
   0.41017e-05_rb,0.41708e-05_rb,0.42405e-05_rb,0.43106e-05_rb,0.43812e-05_rb, &
   0.44524e-05_rb,0.45240e-05_rb,0.45961e-05_rb,0.46687e-05_rb,0.47418e-05_rb, &
   0.48153e-05_rb,0.48894e-05_rb,0.49639e-05_rb,0.50389e-05_rb,0.51143e-05_rb/)
   totplnk(51:100,  2) = (/                                                    &
   0.51902e-05_rb,0.52666e-05_rb,0.53434e-05_rb,0.54207e-05_rb,0.54985e-05_rb, &
   0.55767e-05_rb,0.56553e-05_rb,0.57343e-05_rb,0.58139e-05_rb,0.58938e-05_rb, &
   0.59742e-05_rb,0.60550e-05_rb,0.61362e-05_rb,0.62179e-05_rb,0.63000e-05_rb, &
   0.63825e-05_rb,0.64654e-05_rb,0.65487e-05_rb,0.66324e-05_rb,0.67166e-05_rb, &
   0.68011e-05_rb,0.68860e-05_rb,0.69714e-05_rb,0.70571e-05_rb,0.71432e-05_rb, &
   0.72297e-05_rb,0.73166e-05_rb,0.74039e-05_rb,0.74915e-05_rb,0.75796e-05_rb, &
   0.76680e-05_rb,0.77567e-05_rb,0.78459e-05_rb,0.79354e-05_rb,0.80252e-05_rb, &
   0.81155e-05_rb,0.82061e-05_rb,0.82970e-05_rb,0.83883e-05_rb,0.84799e-05_rb, &
   0.85719e-05_rb,0.86643e-05_rb,0.87569e-05_rb,0.88499e-05_rb,0.89433e-05_rb, &
   0.90370e-05_rb,0.91310e-05_rb,0.92254e-05_rb,0.93200e-05_rb,0.94150e-05_rb/)
   totplnk(101:150,  2) = (/                                                   &
   0.95104e-05_rb,0.96060e-05_rb,0.97020e-05_rb,0.97982e-05_rb,0.98948e-05_rb, &
   0.99917e-05_rb,0.10089e-04_rb,0.10186e-04_rb,0.10284e-04_rb,0.10382e-04_rb, &
   0.10481e-04_rb,0.10580e-04_rb,0.10679e-04_rb,0.10778e-04_rb,0.10877e-04_rb, &
   0.10977e-04_rb,0.11077e-04_rb,0.11178e-04_rb,0.11279e-04_rb,0.11380e-04_rb, &
   0.11481e-04_rb,0.11583e-04_rb,0.11684e-04_rb,0.11786e-04_rb,0.11889e-04_rb, &
   0.11992e-04_rb,0.12094e-04_rb,0.12198e-04_rb,0.12301e-04_rb,0.12405e-04_rb, &
   0.12509e-04_rb,0.12613e-04_rb,0.12717e-04_rb,0.12822e-04_rb,0.12927e-04_rb, &
   0.13032e-04_rb,0.13138e-04_rb,0.13244e-04_rb,0.13349e-04_rb,0.13456e-04_rb, &
   0.13562e-04_rb,0.13669e-04_rb,0.13776e-04_rb,0.13883e-04_rb,0.13990e-04_rb, &
   0.14098e-04_rb,0.14206e-04_rb,0.14314e-04_rb,0.14422e-04_rb,0.14531e-04_rb/)
   totplnk(151:181,  2) = (/                                                   &
   0.14639e-04_rb,0.14748e-04_rb,0.14857e-04_rb,0.14967e-04_rb,0.15076e-04_rb, &
   0.15186e-04_rb,0.15296e-04_rb,0.15407e-04_rb,0.15517e-04_rb,0.15628e-04_rb, &
   0.15739e-04_rb,0.15850e-04_rb,0.15961e-04_rb,0.16072e-04_rb,0.16184e-04_rb, &
   0.16296e-04_rb,0.16408e-04_rb,0.16521e-04_rb,0.16633e-04_rb,0.16746e-04_rb, &
   0.16859e-04_rb,0.16972e-04_rb,0.17085e-04_rb,0.17198e-04_rb,0.17312e-04_rb, &
   0.17426e-04_rb,0.17540e-04_rb,0.17654e-04_rb,0.17769e-04_rb,0.17883e-04_rb, &
   0.17998e-04_rb/)
   totplnk(1:50, 3) = (/                                                       &
   1.34822e-06_rb,1.39134e-06_rb,1.43530e-06_rb,1.48010e-06_rb,1.52574e-06_rb, &
   1.57222e-06_rb,1.61956e-06_rb,1.66774e-06_rb,1.71678e-06_rb,1.76666e-06_rb, &
   1.81741e-06_rb,1.86901e-06_rb,1.92147e-06_rb,1.97479e-06_rb,2.02898e-06_rb, &
   2.08402e-06_rb,2.13993e-06_rb,2.19671e-06_rb,2.25435e-06_rb,2.31285e-06_rb, &
   2.37222e-06_rb,2.43246e-06_rb,2.49356e-06_rb,2.55553e-06_rb,2.61837e-06_rb, &
   2.68207e-06_rb,2.74664e-06_rb,2.81207e-06_rb,2.87837e-06_rb,2.94554e-06_rb, &
   3.01356e-06_rb,3.08245e-06_rb,3.15221e-06_rb,3.22282e-06_rb,3.29429e-06_rb, &
   3.36662e-06_rb,3.43982e-06_rb,3.51386e-06_rb,3.58876e-06_rb,3.66451e-06_rb, &
   3.74112e-06_rb,3.81857e-06_rb,3.89688e-06_rb,3.97602e-06_rb,4.05601e-06_rb, &
   4.13685e-06_rb,4.21852e-06_rb,4.30104e-06_rb,4.38438e-06_rb,4.46857e-06_rb/)
   totplnk(51:100, 3) = (/                                                     &
   4.55358e-06_rb,4.63943e-06_rb,4.72610e-06_rb,4.81359e-06_rb,4.90191e-06_rb, &
   4.99105e-06_rb,5.08100e-06_rb,5.17176e-06_rb,5.26335e-06_rb,5.35573e-06_rb, &
   5.44892e-06_rb,5.54292e-06_rb,5.63772e-06_rb,5.73331e-06_rb,5.82970e-06_rb, &
   5.92688e-06_rb,6.02485e-06_rb,6.12360e-06_rb,6.22314e-06_rb,6.32346e-06_rb, &
   6.42455e-06_rb,6.52641e-06_rb,6.62906e-06_rb,6.73247e-06_rb,6.83664e-06_rb, &
   6.94156e-06_rb,7.04725e-06_rb,7.15370e-06_rb,7.26089e-06_rb,7.36883e-06_rb, &
   7.47752e-06_rb,7.58695e-06_rb,7.69712e-06_rb,7.80801e-06_rb,7.91965e-06_rb, &
   8.03201e-06_rb,8.14510e-06_rb,8.25891e-06_rb,8.37343e-06_rb,8.48867e-06_rb, &
   8.60463e-06_rb,8.72128e-06_rb,8.83865e-06_rb,8.95672e-06_rb,9.07548e-06_rb, &
   9.19495e-06_rb,9.31510e-06_rb,9.43594e-06_rb,9.55745e-06_rb,9.67966e-06_rb/)
   totplnk(101:150, 3) = (/                                                    &
   9.80254e-06_rb,9.92609e-06_rb,1.00503e-05_rb,1.01752e-05_rb,1.03008e-05_rb, &
   1.04270e-05_rb,1.05539e-05_rb,1.06814e-05_rb,1.08096e-05_rb,1.09384e-05_rb, &
   1.10679e-05_rb,1.11980e-05_rb,1.13288e-05_rb,1.14601e-05_rb,1.15922e-05_rb, &
   1.17248e-05_rb,1.18581e-05_rb,1.19920e-05_rb,1.21265e-05_rb,1.22616e-05_rb, &
   1.23973e-05_rb,1.25337e-05_rb,1.26706e-05_rb,1.28081e-05_rb,1.29463e-05_rb, &
   1.30850e-05_rb,1.32243e-05_rb,1.33642e-05_rb,1.35047e-05_rb,1.36458e-05_rb, &
   1.37875e-05_rb,1.39297e-05_rb,1.40725e-05_rb,1.42159e-05_rb,1.43598e-05_rb, &
   1.45044e-05_rb,1.46494e-05_rb,1.47950e-05_rb,1.49412e-05_rb,1.50879e-05_rb, &
   1.52352e-05_rb,1.53830e-05_rb,1.55314e-05_rb,1.56803e-05_rb,1.58297e-05_rb, &
   1.59797e-05_rb,1.61302e-05_rb,1.62812e-05_rb,1.64327e-05_rb,1.65848e-05_rb/)
   totplnk(151:181, 3) = (/                                                    &
   1.67374e-05_rb,1.68904e-05_rb,1.70441e-05_rb,1.71982e-05_rb,1.73528e-05_rb, &
   1.75079e-05_rb,1.76635e-05_rb,1.78197e-05_rb,1.79763e-05_rb,1.81334e-05_rb, &
   1.82910e-05_rb,1.84491e-05_rb,1.86076e-05_rb,1.87667e-05_rb,1.89262e-05_rb, &
   1.90862e-05_rb,1.92467e-05_rb,1.94076e-05_rb,1.95690e-05_rb,1.97309e-05_rb, &
   1.98932e-05_rb,2.00560e-05_rb,2.02193e-05_rb,2.03830e-05_rb,2.05472e-05_rb, &
   2.07118e-05_rb,2.08768e-05_rb,2.10423e-05_rb,2.12083e-05_rb,2.13747e-05_rb, &
   2.15414e-05_rb/)
   totplnk(1:50, 4) = (/                                                       &
   8.90528e-07_rb,9.24222e-07_rb,9.58757e-07_rb,9.94141e-07_rb,1.03038e-06_rb, &
   1.06748e-06_rb,1.10545e-06_rb,1.14430e-06_rb,1.18403e-06_rb,1.22465e-06_rb, &
   1.26618e-06_rb,1.30860e-06_rb,1.35193e-06_rb,1.39619e-06_rb,1.44136e-06_rb, &
   1.48746e-06_rb,1.53449e-06_rb,1.58246e-06_rb,1.63138e-06_rb,1.68124e-06_rb, &
   1.73206e-06_rb,1.78383e-06_rb,1.83657e-06_rb,1.89028e-06_rb,1.94495e-06_rb, &
   2.00060e-06_rb,2.05724e-06_rb,2.11485e-06_rb,2.17344e-06_rb,2.23303e-06_rb, &
   2.29361e-06_rb,2.35519e-06_rb,2.41777e-06_rb,2.48134e-06_rb,2.54592e-06_rb, &
   2.61151e-06_rb,2.67810e-06_rb,2.74571e-06_rb,2.81433e-06_rb,2.88396e-06_rb, &
   2.95461e-06_rb,3.02628e-06_rb,3.09896e-06_rb,3.17267e-06_rb,3.24741e-06_rb, &
   3.32316e-06_rb,3.39994e-06_rb,3.47774e-06_rb,3.55657e-06_rb,3.63642e-06_rb/)
   totplnk(51:100, 4) = (/                                                     &
   3.71731e-06_rb,3.79922e-06_rb,3.88216e-06_rb,3.96612e-06_rb,4.05112e-06_rb, &
   4.13714e-06_rb,4.22419e-06_rb,4.31227e-06_rb,4.40137e-06_rb,4.49151e-06_rb, &
   4.58266e-06_rb,4.67485e-06_rb,4.76806e-06_rb,4.86229e-06_rb,4.95754e-06_rb, &
   5.05383e-06_rb,5.15113e-06_rb,5.24946e-06_rb,5.34879e-06_rb,5.44916e-06_rb, &
   5.55053e-06_rb,5.65292e-06_rb,5.75632e-06_rb,5.86073e-06_rb,5.96616e-06_rb, &
   6.07260e-06_rb,6.18003e-06_rb,6.28848e-06_rb,6.39794e-06_rb,6.50838e-06_rb, &
   6.61983e-06_rb,6.73229e-06_rb,6.84573e-06_rb,6.96016e-06_rb,7.07559e-06_rb, &
   7.19200e-06_rb,7.30940e-06_rb,7.42779e-06_rb,7.54715e-06_rb,7.66749e-06_rb, &
   7.78882e-06_rb,7.91110e-06_rb,8.03436e-06_rb,8.15859e-06_rb,8.28379e-06_rb, &
   8.40994e-06_rb,8.53706e-06_rb,8.66515e-06_rb,8.79418e-06_rb,8.92416e-06_rb/)
   totplnk(101:150, 4) = (/                                                    &
   9.05510e-06_rb,9.18697e-06_rb,9.31979e-06_rb,9.45356e-06_rb,9.58826e-06_rb, &
   9.72389e-06_rb,9.86046e-06_rb,9.99793e-06_rb,1.01364e-05_rb,1.02757e-05_rb, &
   1.04159e-05_rb,1.05571e-05_rb,1.06992e-05_rb,1.08422e-05_rb,1.09861e-05_rb, &
   1.11309e-05_rb,1.12766e-05_rb,1.14232e-05_rb,1.15707e-05_rb,1.17190e-05_rb, &
   1.18683e-05_rb,1.20184e-05_rb,1.21695e-05_rb,1.23214e-05_rb,1.24741e-05_rb, &
   1.26277e-05_rb,1.27822e-05_rb,1.29376e-05_rb,1.30939e-05_rb,1.32509e-05_rb, &
   1.34088e-05_rb,1.35676e-05_rb,1.37273e-05_rb,1.38877e-05_rb,1.40490e-05_rb, &
   1.42112e-05_rb,1.43742e-05_rb,1.45380e-05_rb,1.47026e-05_rb,1.48680e-05_rb, &
   1.50343e-05_rb,1.52014e-05_rb,1.53692e-05_rb,1.55379e-05_rb,1.57074e-05_rb, &
   1.58778e-05_rb,1.60488e-05_rb,1.62207e-05_rb,1.63934e-05_rb,1.65669e-05_rb/)
   totplnk(151:181, 4) = (/                                                    &
   1.67411e-05_rb,1.69162e-05_rb,1.70920e-05_rb,1.72685e-05_rb,1.74459e-05_rb, &
   1.76240e-05_rb,1.78029e-05_rb,1.79825e-05_rb,1.81629e-05_rb,1.83440e-05_rb, &
   1.85259e-05_rb,1.87086e-05_rb,1.88919e-05_rb,1.90760e-05_rb,1.92609e-05_rb, &
   1.94465e-05_rb,1.96327e-05_rb,1.98199e-05_rb,2.00076e-05_rb,2.01961e-05_rb, &
   2.03853e-05_rb,2.05752e-05_rb,2.07658e-05_rb,2.09571e-05_rb,2.11491e-05_rb, &
   2.13418e-05_rb,2.15352e-05_rb,2.17294e-05_rb,2.19241e-05_rb,2.21196e-05_rb, &
   2.23158e-05_rb/)
   totplnk(1:50, 5) = (/                                                       &
   5.70230e-07_rb,5.94788e-07_rb,6.20085e-07_rb,6.46130e-07_rb,6.72936e-07_rb, &
   7.00512e-07_rb,7.28869e-07_rb,7.58019e-07_rb,7.87971e-07_rb,8.18734e-07_rb, &
   8.50320e-07_rb,8.82738e-07_rb,9.15999e-07_rb,9.50110e-07_rb,9.85084e-07_rb, &
   1.02093e-06_rb,1.05765e-06_rb,1.09527e-06_rb,1.13378e-06_rb,1.17320e-06_rb, &
   1.21353e-06_rb,1.25479e-06_rb,1.29698e-06_rb,1.34011e-06_rb,1.38419e-06_rb, &
   1.42923e-06_rb,1.47523e-06_rb,1.52221e-06_rb,1.57016e-06_rb,1.61910e-06_rb, &
   1.66904e-06_rb,1.71997e-06_rb,1.77192e-06_rb,1.82488e-06_rb,1.87886e-06_rb, &
   1.93387e-06_rb,1.98991e-06_rb,2.04699e-06_rb,2.10512e-06_rb,2.16430e-06_rb, &
   2.22454e-06_rb,2.28584e-06_rb,2.34821e-06_rb,2.41166e-06_rb,2.47618e-06_rb, &
   2.54178e-06_rb,2.60847e-06_rb,2.67626e-06_rb,2.74514e-06_rb,2.81512e-06_rb/)
   totplnk(51:100, 5) = (/                                                     &
   2.88621e-06_rb,2.95841e-06_rb,3.03172e-06_rb,3.10615e-06_rb,3.18170e-06_rb, &
   3.25838e-06_rb,3.33618e-06_rb,3.41511e-06_rb,3.49518e-06_rb,3.57639e-06_rb, &
   3.65873e-06_rb,3.74221e-06_rb,3.82684e-06_rb,3.91262e-06_rb,3.99955e-06_rb, &
   4.08763e-06_rb,4.17686e-06_rb,4.26725e-06_rb,4.35880e-06_rb,4.45150e-06_rb, &
   4.54537e-06_rb,4.64039e-06_rb,4.73659e-06_rb,4.83394e-06_rb,4.93246e-06_rb, &
   5.03215e-06_rb,5.13301e-06_rb,5.23504e-06_rb,5.33823e-06_rb,5.44260e-06_rb, &
   5.54814e-06_rb,5.65484e-06_rb,5.76272e-06_rb,5.87177e-06_rb,5.98199e-06_rb, &
   6.09339e-06_rb,6.20596e-06_rb,6.31969e-06_rb,6.43460e-06_rb,6.55068e-06_rb, &
   6.66793e-06_rb,6.78636e-06_rb,6.90595e-06_rb,7.02670e-06_rb,7.14863e-06_rb, &
   7.27173e-06_rb,7.39599e-06_rb,7.52142e-06_rb,7.64802e-06_rb,7.77577e-06_rb/)
   totplnk(101:150, 5) = (/                                                    &
   7.90469e-06_rb,8.03477e-06_rb,8.16601e-06_rb,8.29841e-06_rb,8.43198e-06_rb, &
   8.56669e-06_rb,8.70256e-06_rb,8.83957e-06_rb,8.97775e-06_rb,9.11706e-06_rb, &
   9.25753e-06_rb,9.39915e-06_rb,9.54190e-06_rb,9.68580e-06_rb,9.83085e-06_rb, &
   9.97704e-06_rb,1.01243e-05_rb,1.02728e-05_rb,1.04224e-05_rb,1.05731e-05_rb, &
   1.07249e-05_rb,1.08779e-05_rb,1.10320e-05_rb,1.11872e-05_rb,1.13435e-05_rb, &
   1.15009e-05_rb,1.16595e-05_rb,1.18191e-05_rb,1.19799e-05_rb,1.21418e-05_rb, &
   1.23048e-05_rb,1.24688e-05_rb,1.26340e-05_rb,1.28003e-05_rb,1.29676e-05_rb, &
   1.31361e-05_rb,1.33056e-05_rb,1.34762e-05_rb,1.36479e-05_rb,1.38207e-05_rb, &
   1.39945e-05_rb,1.41694e-05_rb,1.43454e-05_rb,1.45225e-05_rb,1.47006e-05_rb, &
   1.48797e-05_rb,1.50600e-05_rb,1.52413e-05_rb,1.54236e-05_rb,1.56070e-05_rb/)
   totplnk(151:181, 5) = (/                                                    &
   1.57914e-05_rb,1.59768e-05_rb,1.61633e-05_rb,1.63509e-05_rb,1.65394e-05_rb, &
   1.67290e-05_rb,1.69197e-05_rb,1.71113e-05_rb,1.73040e-05_rb,1.74976e-05_rb, &
   1.76923e-05_rb,1.78880e-05_rb,1.80847e-05_rb,1.82824e-05_rb,1.84811e-05_rb, &
   1.86808e-05_rb,1.88814e-05_rb,1.90831e-05_rb,1.92857e-05_rb,1.94894e-05_rb, &
   1.96940e-05_rb,1.98996e-05_rb,2.01061e-05_rb,2.03136e-05_rb,2.05221e-05_rb, &
   2.07316e-05_rb,2.09420e-05_rb,2.11533e-05_rb,2.13657e-05_rb,2.15789e-05_rb, &
   2.17931e-05_rb/)
   totplnk(1:50, 6) = (/                                                       &
   2.73493e-07_rb,2.87408e-07_rb,3.01848e-07_rb,3.16825e-07_rb,3.32352e-07_rb, &
   3.48439e-07_rb,3.65100e-07_rb,3.82346e-07_rb,4.00189e-07_rb,4.18641e-07_rb, &
   4.37715e-07_rb,4.57422e-07_rb,4.77774e-07_rb,4.98784e-07_rb,5.20464e-07_rb, &
   5.42824e-07_rb,5.65879e-07_rb,5.89638e-07_rb,6.14115e-07_rb,6.39320e-07_rb, &
   6.65266e-07_rb,6.91965e-07_rb,7.19427e-07_rb,7.47666e-07_rb,7.76691e-07_rb, &
   8.06516e-07_rb,8.37151e-07_rb,8.68607e-07_rb,9.00896e-07_rb,9.34029e-07_rb, &
   9.68018e-07_rb,1.00287e-06_rb,1.03860e-06_rb,1.07522e-06_rb,1.11274e-06_rb, &
   1.15117e-06_rb,1.19052e-06_rb,1.23079e-06_rb,1.27201e-06_rb,1.31418e-06_rb, &
   1.35731e-06_rb,1.40141e-06_rb,1.44650e-06_rb,1.49257e-06_rb,1.53965e-06_rb, &
   1.58773e-06_rb,1.63684e-06_rb,1.68697e-06_rb,1.73815e-06_rb,1.79037e-06_rb/)
   totplnk(51:100, 6) = (/                                                     &
   1.84365e-06_rb,1.89799e-06_rb,1.95341e-06_rb,2.00991e-06_rb,2.06750e-06_rb, &
   2.12619e-06_rb,2.18599e-06_rb,2.24691e-06_rb,2.30895e-06_rb,2.37212e-06_rb, &
   2.43643e-06_rb,2.50189e-06_rb,2.56851e-06_rb,2.63628e-06_rb,2.70523e-06_rb, &
   2.77536e-06_rb,2.84666e-06_rb,2.91916e-06_rb,2.99286e-06_rb,3.06776e-06_rb, &
   3.14387e-06_rb,3.22120e-06_rb,3.29975e-06_rb,3.37953e-06_rb,3.46054e-06_rb, &
   3.54280e-06_rb,3.62630e-06_rb,3.71105e-06_rb,3.79707e-06_rb,3.88434e-06_rb, &
   3.97288e-06_rb,4.06270e-06_rb,4.15380e-06_rb,4.24617e-06_rb,4.33984e-06_rb, &
   4.43479e-06_rb,4.53104e-06_rb,4.62860e-06_rb,4.72746e-06_rb,4.82763e-06_rb, &
   4.92911e-06_rb,5.03191e-06_rb,5.13603e-06_rb,5.24147e-06_rb,5.34824e-06_rb, &
   5.45634e-06_rb,5.56578e-06_rb,5.67656e-06_rb,5.78867e-06_rb,5.90213e-06_rb/)
   totplnk(101:150, 6) = (/                                                    &
   6.01694e-06_rb,6.13309e-06_rb,6.25060e-06_rb,6.36947e-06_rb,6.48968e-06_rb, &
   6.61126e-06_rb,6.73420e-06_rb,6.85850e-06_rb,6.98417e-06_rb,7.11120e-06_rb, &
   7.23961e-06_rb,7.36938e-06_rb,7.50053e-06_rb,7.63305e-06_rb,7.76694e-06_rb, &
   7.90221e-06_rb,8.03887e-06_rb,8.17690e-06_rb,8.31632e-06_rb,8.45710e-06_rb, &
   8.59928e-06_rb,8.74282e-06_rb,8.88776e-06_rb,9.03409e-06_rb,9.18179e-06_rb, &
   9.33088e-06_rb,9.48136e-06_rb,9.63323e-06_rb,9.78648e-06_rb,9.94111e-06_rb, &
   1.00971e-05_rb,1.02545e-05_rb,1.04133e-05_rb,1.05735e-05_rb,1.07351e-05_rb, &
   1.08980e-05_rb,1.10624e-05_rb,1.12281e-05_rb,1.13952e-05_rb,1.15637e-05_rb, &
   1.17335e-05_rb,1.19048e-05_rb,1.20774e-05_rb,1.22514e-05_rb,1.24268e-05_rb, &
   1.26036e-05_rb,1.27817e-05_rb,1.29612e-05_rb,1.31421e-05_rb,1.33244e-05_rb/)
   totplnk(151:181, 6) = (/                                                    &
   1.35080e-05_rb,1.36930e-05_rb,1.38794e-05_rb,1.40672e-05_rb,1.42563e-05_rb, &
   1.44468e-05_rb,1.46386e-05_rb,1.48318e-05_rb,1.50264e-05_rb,1.52223e-05_rb, &
   1.54196e-05_rb,1.56182e-05_rb,1.58182e-05_rb,1.60196e-05_rb,1.62223e-05_rb, &
   1.64263e-05_rb,1.66317e-05_rb,1.68384e-05_rb,1.70465e-05_rb,1.72559e-05_rb, &
   1.74666e-05_rb,1.76787e-05_rb,1.78921e-05_rb,1.81069e-05_rb,1.83230e-05_rb, &
   1.85404e-05_rb,1.87591e-05_rb,1.89791e-05_rb,1.92005e-05_rb,1.94232e-05_rb, &
   1.96471e-05_rb/)
   totplnk(1:50, 7) = (/                                                       &
   1.25349e-07_rb,1.32735e-07_rb,1.40458e-07_rb,1.48527e-07_rb,1.56954e-07_rb, &
   1.65748e-07_rb,1.74920e-07_rb,1.84481e-07_rb,1.94443e-07_rb,2.04814e-07_rb, &
   2.15608e-07_rb,2.26835e-07_rb,2.38507e-07_rb,2.50634e-07_rb,2.63229e-07_rb, &
   2.76301e-07_rb,2.89864e-07_rb,3.03930e-07_rb,3.18508e-07_rb,3.33612e-07_rb, &
   3.49253e-07_rb,3.65443e-07_rb,3.82195e-07_rb,3.99519e-07_rb,4.17428e-07_rb, &
   4.35934e-07_rb,4.55050e-07_rb,4.74785e-07_rb,4.95155e-07_rb,5.16170e-07_rb, &
   5.37844e-07_rb,5.60186e-07_rb,5.83211e-07_rb,6.06929e-07_rb,6.31355e-07_rb, &
   6.56498e-07_rb,6.82373e-07_rb,7.08990e-07_rb,7.36362e-07_rb,7.64501e-07_rb, &
   7.93420e-07_rb,8.23130e-07_rb,8.53643e-07_rb,8.84971e-07_rb,9.17128e-07_rb, &
   9.50123e-07_rb,9.83969e-07_rb,1.01868e-06_rb,1.05426e-06_rb,1.09073e-06_rb/)
   totplnk(51:100, 7) = (/                                                     &
   1.12810e-06_rb,1.16638e-06_rb,1.20558e-06_rb,1.24572e-06_rb,1.28680e-06_rb, &
   1.32883e-06_rb,1.37183e-06_rb,1.41581e-06_rb,1.46078e-06_rb,1.50675e-06_rb, &
   1.55374e-06_rb,1.60174e-06_rb,1.65078e-06_rb,1.70087e-06_rb,1.75200e-06_rb, &
   1.80421e-06_rb,1.85749e-06_rb,1.91186e-06_rb,1.96732e-06_rb,2.02389e-06_rb, &
   2.08159e-06_rb,2.14040e-06_rb,2.20035e-06_rb,2.26146e-06_rb,2.32372e-06_rb, &
   2.38714e-06_rb,2.45174e-06_rb,2.51753e-06_rb,2.58451e-06_rb,2.65270e-06_rb, &
   2.72210e-06_rb,2.79272e-06_rb,2.86457e-06_rb,2.93767e-06_rb,3.01201e-06_rb, &
   3.08761e-06_rb,3.16448e-06_rb,3.24261e-06_rb,3.32204e-06_rb,3.40275e-06_rb, &
   3.48476e-06_rb,3.56808e-06_rb,3.65271e-06_rb,3.73866e-06_rb,3.82595e-06_rb, &
   3.91456e-06_rb,4.00453e-06_rb,4.09584e-06_rb,4.18851e-06_rb,4.28254e-06_rb/)
   totplnk(101:150, 7) = (/                                                    &
   4.37796e-06_rb,4.47475e-06_rb,4.57293e-06_rb,4.67249e-06_rb,4.77346e-06_rb, &
   4.87583e-06_rb,4.97961e-06_rb,5.08481e-06_rb,5.19143e-06_rb,5.29948e-06_rb, &
   5.40896e-06_rb,5.51989e-06_rb,5.63226e-06_rb,5.74608e-06_rb,5.86136e-06_rb, &
   5.97810e-06_rb,6.09631e-06_rb,6.21597e-06_rb,6.33713e-06_rb,6.45976e-06_rb, &
   6.58388e-06_rb,6.70950e-06_rb,6.83661e-06_rb,6.96521e-06_rb,7.09531e-06_rb, &
   7.22692e-06_rb,7.36005e-06_rb,7.49468e-06_rb,7.63084e-06_rb,7.76851e-06_rb, &
   7.90773e-06_rb,8.04846e-06_rb,8.19072e-06_rb,8.33452e-06_rb,8.47985e-06_rb, &
   8.62674e-06_rb,8.77517e-06_rb,8.92514e-06_rb,9.07666e-06_rb,9.22975e-06_rb, &
   9.38437e-06_rb,9.54057e-06_rb,9.69832e-06_rb,9.85762e-06_rb,1.00185e-05_rb, &
   1.01810e-05_rb,1.03450e-05_rb,1.05106e-05_rb,1.06777e-05_rb,1.08465e-05_rb/)
   totplnk(151:181, 7) = (/                                                    &
   1.10168e-05_rb,1.11887e-05_rb,1.13621e-05_rb,1.15372e-05_rb,1.17138e-05_rb, &
   1.18920e-05_rb,1.20718e-05_rb,1.22532e-05_rb,1.24362e-05_rb,1.26207e-05_rb, &
   1.28069e-05_rb,1.29946e-05_rb,1.31839e-05_rb,1.33749e-05_rb,1.35674e-05_rb, &
   1.37615e-05_rb,1.39572e-05_rb,1.41544e-05_rb,1.43533e-05_rb,1.45538e-05_rb, &
   1.47558e-05_rb,1.49595e-05_rb,1.51647e-05_rb,1.53716e-05_rb,1.55800e-05_rb, &
   1.57900e-05_rb,1.60017e-05_rb,1.62149e-05_rb,1.64296e-05_rb,1.66460e-05_rb, &
   1.68640e-05_rb/)
   totplnk(1:50, 8) = (/                                                       &
   6.74445e-08_rb,7.18176e-08_rb,7.64153e-08_rb,8.12456e-08_rb,8.63170e-08_rb, &
   9.16378e-08_rb,9.72168e-08_rb,1.03063e-07_rb,1.09184e-07_rb,1.15591e-07_rb, &
   1.22292e-07_rb,1.29296e-07_rb,1.36613e-07_rb,1.44253e-07_rb,1.52226e-07_rb, &
   1.60540e-07_rb,1.69207e-07_rb,1.78236e-07_rb,1.87637e-07_rb,1.97421e-07_rb, &
   2.07599e-07_rb,2.18181e-07_rb,2.29177e-07_rb,2.40598e-07_rb,2.52456e-07_rb, &
   2.64761e-07_rb,2.77523e-07_rb,2.90755e-07_rb,3.04468e-07_rb,3.18673e-07_rb, &
   3.33381e-07_rb,3.48603e-07_rb,3.64352e-07_rb,3.80638e-07_rb,3.97474e-07_rb, &
   4.14871e-07_rb,4.32841e-07_rb,4.51395e-07_rb,4.70547e-07_rb,4.90306e-07_rb, &
   5.10687e-07_rb,5.31699e-07_rb,5.53357e-07_rb,5.75670e-07_rb,5.98652e-07_rb, &
   6.22315e-07_rb,6.46672e-07_rb,6.71731e-07_rb,6.97511e-07_rb,7.24018e-07_rb/)
   totplnk(51:100, 8) = (/                                                     &
   7.51266e-07_rb,7.79269e-07_rb,8.08038e-07_rb,8.37584e-07_rb,8.67922e-07_rb, &
   8.99061e-07_rb,9.31016e-07_rb,9.63797e-07_rb,9.97417e-07_rb,1.03189e-06_rb, &
   1.06722e-06_rb,1.10343e-06_rb,1.14053e-06_rb,1.17853e-06_rb,1.21743e-06_rb, &
   1.25726e-06_rb,1.29803e-06_rb,1.33974e-06_rb,1.38241e-06_rb,1.42606e-06_rb, &
   1.47068e-06_rb,1.51630e-06_rb,1.56293e-06_rb,1.61056e-06_rb,1.65924e-06_rb, &
   1.70894e-06_rb,1.75971e-06_rb,1.81153e-06_rb,1.86443e-06_rb,1.91841e-06_rb, &
   1.97350e-06_rb,2.02968e-06_rb,2.08699e-06_rb,2.14543e-06_rb,2.20500e-06_rb, &
   2.26573e-06_rb,2.32762e-06_rb,2.39068e-06_rb,2.45492e-06_rb,2.52036e-06_rb, &
   2.58700e-06_rb,2.65485e-06_rb,2.72393e-06_rb,2.79424e-06_rb,2.86580e-06_rb, &
   2.93861e-06_rb,3.01269e-06_rb,3.08803e-06_rb,3.16467e-06_rb,3.24259e-06_rb/)
   totplnk(101:150, 8) = (/                                                    &
   3.32181e-06_rb,3.40235e-06_rb,3.48420e-06_rb,3.56739e-06_rb,3.65192e-06_rb, &
   3.73779e-06_rb,3.82502e-06_rb,3.91362e-06_rb,4.00359e-06_rb,4.09494e-06_rb, &
   4.18768e-06_rb,4.28182e-06_rb,4.37737e-06_rb,4.47434e-06_rb,4.57273e-06_rb, &
   4.67254e-06_rb,4.77380e-06_rb,4.87651e-06_rb,4.98067e-06_rb,5.08630e-06_rb, &
   5.19339e-06_rb,5.30196e-06_rb,5.41201e-06_rb,5.52356e-06_rb,5.63660e-06_rb, &
   5.75116e-06_rb,5.86722e-06_rb,5.98479e-06_rb,6.10390e-06_rb,6.22453e-06_rb, &
   6.34669e-06_rb,6.47042e-06_rb,6.59569e-06_rb,6.72252e-06_rb,6.85090e-06_rb, &
   6.98085e-06_rb,7.11238e-06_rb,7.24549e-06_rb,7.38019e-06_rb,7.51646e-06_rb, &
   7.65434e-06_rb,7.79382e-06_rb,7.93490e-06_rb,8.07760e-06_rb,8.22192e-06_rb, &
   8.36784e-06_rb,8.51540e-06_rb,8.66459e-06_rb,8.81542e-06_rb,8.96786e-06_rb/)
   totplnk(151:181, 8) = (/                                                    &
   9.12197e-06_rb,9.27772e-06_rb,9.43513e-06_rb,9.59419e-06_rb,9.75490e-06_rb, &
   9.91728e-06_rb,1.00813e-05_rb,1.02471e-05_rb,1.04144e-05_rb,1.05835e-05_rb, &
   1.07543e-05_rb,1.09267e-05_rb,1.11008e-05_rb,1.12766e-05_rb,1.14541e-05_rb, &
   1.16333e-05_rb,1.18142e-05_rb,1.19969e-05_rb,1.21812e-05_rb,1.23672e-05_rb, &
   1.25549e-05_rb,1.27443e-05_rb,1.29355e-05_rb,1.31284e-05_rb,1.33229e-05_rb, &
   1.35193e-05_rb,1.37173e-05_rb,1.39170e-05_rb,1.41185e-05_rb,1.43217e-05_rb, &
   1.45267e-05_rb/)
   totplnk(1:50, 9) = (/                                                       &
   2.61522e-08_rb,2.80613e-08_rb,3.00838e-08_rb,3.22250e-08_rb,3.44899e-08_rb, &
   3.68841e-08_rb,3.94129e-08_rb,4.20820e-08_rb,4.48973e-08_rb,4.78646e-08_rb, &
   5.09901e-08_rb,5.42799e-08_rb,5.77405e-08_rb,6.13784e-08_rb,6.52001e-08_rb, &
   6.92126e-08_rb,7.34227e-08_rb,7.78375e-08_rb,8.24643e-08_rb,8.73103e-08_rb, &
   9.23832e-08_rb,9.76905e-08_rb,1.03240e-07_rb,1.09039e-07_rb,1.15097e-07_rb, &
   1.21421e-07_rb,1.28020e-07_rb,1.34902e-07_rb,1.42075e-07_rb,1.49548e-07_rb, &
   1.57331e-07_rb,1.65432e-07_rb,1.73860e-07_rb,1.82624e-07_rb,1.91734e-07_rb, &
   2.01198e-07_rb,2.11028e-07_rb,2.21231e-07_rb,2.31818e-07_rb,2.42799e-07_rb, &
   2.54184e-07_rb,2.65983e-07_rb,2.78205e-07_rb,2.90862e-07_rb,3.03963e-07_rb, &
   3.17519e-07_rb,3.31541e-07_rb,3.46039e-07_rb,3.61024e-07_rb,3.76507e-07_rb/)
   totplnk(51:100, 9) = (/                                                     &
   3.92498e-07_rb,4.09008e-07_rb,4.26050e-07_rb,4.43633e-07_rb,4.61769e-07_rb, &
   4.80469e-07_rb,4.99744e-07_rb,5.19606e-07_rb,5.40067e-07_rb,5.61136e-07_rb, &
   5.82828e-07_rb,6.05152e-07_rb,6.28120e-07_rb,6.51745e-07_rb,6.76038e-07_rb, &
   7.01010e-07_rb,7.26674e-07_rb,7.53041e-07_rb,7.80124e-07_rb,8.07933e-07_rb, &
   8.36482e-07_rb,8.65781e-07_rb,8.95845e-07_rb,9.26683e-07_rb,9.58308e-07_rb, &
   9.90732e-07_rb,1.02397e-06_rb,1.05803e-06_rb,1.09292e-06_rb,1.12866e-06_rb, &
   1.16526e-06_rb,1.20274e-06_rb,1.24109e-06_rb,1.28034e-06_rb,1.32050e-06_rb, &
   1.36158e-06_rb,1.40359e-06_rb,1.44655e-06_rb,1.49046e-06_rb,1.53534e-06_rb, &
   1.58120e-06_rb,1.62805e-06_rb,1.67591e-06_rb,1.72478e-06_rb,1.77468e-06_rb, &
   1.82561e-06_rb,1.87760e-06_rb,1.93066e-06_rb,1.98479e-06_rb,2.04000e-06_rb/)
   totplnk(101:150, 9) = (/                                                    &
   2.09631e-06_rb,2.15373e-06_rb,2.21228e-06_rb,2.27196e-06_rb,2.33278e-06_rb, &
   2.39475e-06_rb,2.45790e-06_rb,2.52222e-06_rb,2.58773e-06_rb,2.65445e-06_rb, &
   2.72238e-06_rb,2.79152e-06_rb,2.86191e-06_rb,2.93354e-06_rb,3.00643e-06_rb, &
   3.08058e-06_rb,3.15601e-06_rb,3.23273e-06_rb,3.31075e-06_rb,3.39009e-06_rb, &
   3.47074e-06_rb,3.55272e-06_rb,3.63605e-06_rb,3.72072e-06_rb,3.80676e-06_rb, &
   3.89417e-06_rb,3.98297e-06_rb,4.07315e-06_rb,4.16474e-06_rb,4.25774e-06_rb, &
   4.35217e-06_rb,4.44802e-06_rb,4.54532e-06_rb,4.64406e-06_rb,4.74428e-06_rb, &
   4.84595e-06_rb,4.94911e-06_rb,5.05376e-06_rb,5.15990e-06_rb,5.26755e-06_rb, &
   5.37671e-06_rb,5.48741e-06_rb,5.59963e-06_rb,5.71340e-06_rb,5.82871e-06_rb, &
   5.94559e-06_rb,6.06403e-06_rb,6.18404e-06_rb,6.30565e-06_rb,6.42885e-06_rb/)
   totplnk(151:181, 9) = (/                                                    &
   6.55364e-06_rb,6.68004e-06_rb,6.80806e-06_rb,6.93771e-06_rb,7.06898e-06_rb, &
   7.20190e-06_rb,7.33646e-06_rb,7.47267e-06_rb,7.61056e-06_rb,7.75010e-06_rb, &
   7.89133e-06_rb,8.03423e-06_rb,8.17884e-06_rb,8.32514e-06_rb,8.47314e-06_rb, &
   8.62284e-06_rb,8.77427e-06_rb,8.92743e-06_rb,9.08231e-06_rb,9.23893e-06_rb, &
   9.39729e-06_rb,9.55741e-06_rb,9.71927e-06_rb,9.88291e-06_rb,1.00483e-05_rb, &
   1.02155e-05_rb,1.03844e-05_rb,1.05552e-05_rb,1.07277e-05_rb,1.09020e-05_rb, &
   1.10781e-05_rb/)
   totplnk(1:50,10) = (/                                                       &
   8.89300e-09_rb,9.63263e-09_rb,1.04235e-08_rb,1.12685e-08_rb,1.21703e-08_rb, &
   1.31321e-08_rb,1.41570e-08_rb,1.52482e-08_rb,1.64090e-08_rb,1.76428e-08_rb, &
   1.89533e-08_rb,2.03441e-08_rb,2.18190e-08_rb,2.33820e-08_rb,2.50370e-08_rb, &
   2.67884e-08_rb,2.86402e-08_rb,3.05969e-08_rb,3.26632e-08_rb,3.48436e-08_rb, &
   3.71429e-08_rb,3.95660e-08_rb,4.21179e-08_rb,4.48040e-08_rb,4.76294e-08_rb, &
   5.05996e-08_rb,5.37201e-08_rb,5.69966e-08_rb,6.04349e-08_rb,6.40411e-08_rb, &
   6.78211e-08_rb,7.17812e-08_rb,7.59276e-08_rb,8.02670e-08_rb,8.48059e-08_rb, &
   8.95508e-08_rb,9.45090e-08_rb,9.96873e-08_rb,1.05093e-07_rb,1.10733e-07_rb, &
   1.16614e-07_rb,1.22745e-07_rb,1.29133e-07_rb,1.35786e-07_rb,1.42711e-07_rb, &
   1.49916e-07_rb,1.57410e-07_rb,1.65202e-07_rb,1.73298e-07_rb,1.81709e-07_rb/)
   totplnk(51:100,10) = (/                                                     &
   1.90441e-07_rb,1.99505e-07_rb,2.08908e-07_rb,2.18660e-07_rb,2.28770e-07_rb, &
   2.39247e-07_rb,2.50101e-07_rb,2.61340e-07_rb,2.72974e-07_rb,2.85013e-07_rb, &
   2.97467e-07_rb,3.10345e-07_rb,3.23657e-07_rb,3.37413e-07_rb,3.51623e-07_rb, &
   3.66298e-07_rb,3.81448e-07_rb,3.97082e-07_rb,4.13212e-07_rb,4.29848e-07_rb, &
   4.47000e-07_rb,4.64680e-07_rb,4.82898e-07_rb,5.01664e-07_rb,5.20991e-07_rb, &
   5.40888e-07_rb,5.61369e-07_rb,5.82440e-07_rb,6.04118e-07_rb,6.26410e-07_rb, &
   6.49329e-07_rb,6.72887e-07_rb,6.97095e-07_rb,7.21964e-07_rb,7.47506e-07_rb, &
   7.73732e-07_rb,8.00655e-07_rb,8.28287e-07_rb,8.56635e-07_rb,8.85717e-07_rb, &
   9.15542e-07_rb,9.46122e-07_rb,9.77469e-07_rb,1.00960e-06_rb,1.04251e-06_rb, &
   1.07623e-06_rb,1.11077e-06_rb,1.14613e-06_rb,1.18233e-06_rb,1.21939e-06_rb/)
   totplnk(101:150,10) = (/                                                    &
   1.25730e-06_rb,1.29610e-06_rb,1.33578e-06_rb,1.37636e-06_rb,1.41785e-06_rb, &
   1.46027e-06_rb,1.50362e-06_rb,1.54792e-06_rb,1.59319e-06_rb,1.63942e-06_rb, &
   1.68665e-06_rb,1.73487e-06_rb,1.78410e-06_rb,1.83435e-06_rb,1.88564e-06_rb, &
   1.93797e-06_rb,1.99136e-06_rb,2.04582e-06_rb,2.10137e-06_rb,2.15801e-06_rb, &
   2.21576e-06_rb,2.27463e-06_rb,2.33462e-06_rb,2.39577e-06_rb,2.45806e-06_rb, &
   2.52153e-06_rb,2.58617e-06_rb,2.65201e-06_rb,2.71905e-06_rb,2.78730e-06_rb, &
   2.85678e-06_rb,2.92749e-06_rb,2.99946e-06_rb,3.07269e-06_rb,3.14720e-06_rb, &
   3.22299e-06_rb,3.30007e-06_rb,3.37847e-06_rb,3.45818e-06_rb,3.53923e-06_rb, &
   3.62161e-06_rb,3.70535e-06_rb,3.79046e-06_rb,3.87695e-06_rb,3.96481e-06_rb, &
   4.05409e-06_rb,4.14477e-06_rb,4.23687e-06_rb,4.33040e-06_rb,4.42538e-06_rb/)
   totplnk(151:181,10) = (/                                                    &
   4.52180e-06_rb,4.61969e-06_rb,4.71905e-06_rb,4.81991e-06_rb,4.92226e-06_rb, &
   5.02611e-06_rb,5.13148e-06_rb,5.23839e-06_rb,5.34681e-06_rb,5.45681e-06_rb, &
   5.56835e-06_rb,5.68146e-06_rb,5.79614e-06_rb,5.91242e-06_rb,6.03030e-06_rb, &
   6.14978e-06_rb,6.27088e-06_rb,6.39360e-06_rb,6.51798e-06_rb,6.64398e-06_rb, &
   6.77165e-06_rb,6.90099e-06_rb,7.03198e-06_rb,7.16468e-06_rb,7.29906e-06_rb, &
   7.43514e-06_rb,7.57294e-06_rb,7.71244e-06_rb,7.85369e-06_rb,7.99666e-06_rb, &
   8.14138e-06_rb/)
   totplnk(1:50,11) = (/                                                       &
   2.53767e-09_rb,2.77242e-09_rb,3.02564e-09_rb,3.29851e-09_rb,3.59228e-09_rb, &
   3.90825e-09_rb,4.24777e-09_rb,4.61227e-09_rb,5.00322e-09_rb,5.42219e-09_rb, &
   5.87080e-09_rb,6.35072e-09_rb,6.86370e-09_rb,7.41159e-09_rb,7.99628e-09_rb, &
   8.61974e-09_rb,9.28404e-09_rb,9.99130e-09_rb,1.07437e-08_rb,1.15436e-08_rb, &
   1.23933e-08_rb,1.32953e-08_rb,1.42522e-08_rb,1.52665e-08_rb,1.63410e-08_rb, &
   1.74786e-08_rb,1.86820e-08_rb,1.99542e-08_rb,2.12985e-08_rb,2.27179e-08_rb, &
   2.42158e-08_rb,2.57954e-08_rb,2.74604e-08_rb,2.92141e-08_rb,3.10604e-08_rb, &
   3.30029e-08_rb,3.50457e-08_rb,3.71925e-08_rb,3.94476e-08_rb,4.18149e-08_rb, &
   4.42991e-08_rb,4.69043e-08_rb,4.96352e-08_rb,5.24961e-08_rb,5.54921e-08_rb, &
   5.86277e-08_rb,6.19081e-08_rb,6.53381e-08_rb,6.89231e-08_rb,7.26681e-08_rb/)
   totplnk(51:100,11) = (/                                                     &
   7.65788e-08_rb,8.06604e-08_rb,8.49187e-08_rb,8.93591e-08_rb,9.39879e-08_rb, &
   9.88106e-08_rb,1.03834e-07_rb,1.09063e-07_rb,1.14504e-07_rb,1.20165e-07_rb, &
   1.26051e-07_rb,1.32169e-07_rb,1.38525e-07_rb,1.45128e-07_rb,1.51982e-07_rb, &
   1.59096e-07_rb,1.66477e-07_rb,1.74132e-07_rb,1.82068e-07_rb,1.90292e-07_rb, &
   1.98813e-07_rb,2.07638e-07_rb,2.16775e-07_rb,2.26231e-07_rb,2.36015e-07_rb, &
   2.46135e-07_rb,2.56599e-07_rb,2.67415e-07_rb,2.78592e-07_rb,2.90137e-07_rb, &
   3.02061e-07_rb,3.14371e-07_rb,3.27077e-07_rb,3.40186e-07_rb,3.53710e-07_rb, &
   3.67655e-07_rb,3.82031e-07_rb,3.96848e-07_rb,4.12116e-07_rb,4.27842e-07_rb, &
   4.44039e-07_rb,4.60713e-07_rb,4.77876e-07_rb,4.95537e-07_rb,5.13706e-07_rb, &
   5.32392e-07_rb,5.51608e-07_rb,5.71360e-07_rb,5.91662e-07_rb,6.12521e-07_rb/)
   totplnk(101:150,11) = (/                                                    &
   6.33950e-07_rb,6.55958e-07_rb,6.78556e-07_rb,7.01753e-07_rb,7.25562e-07_rb, &
   7.49992e-07_rb,7.75055e-07_rb,8.00760e-07_rb,8.27120e-07_rb,8.54145e-07_rb, &
   8.81845e-07_rb,9.10233e-07_rb,9.39318e-07_rb,9.69113e-07_rb,9.99627e-07_rb, &
   1.03087e-06_rb,1.06286e-06_rb,1.09561e-06_rb,1.12912e-06_rb,1.16340e-06_rb, &
   1.19848e-06_rb,1.23435e-06_rb,1.27104e-06_rb,1.30855e-06_rb,1.34690e-06_rb, &
   1.38609e-06_rb,1.42614e-06_rb,1.46706e-06_rb,1.50886e-06_rb,1.55155e-06_rb, &
   1.59515e-06_rb,1.63967e-06_rb,1.68512e-06_rb,1.73150e-06_rb,1.77884e-06_rb, &
   1.82715e-06_rb,1.87643e-06_rb,1.92670e-06_rb,1.97797e-06_rb,2.03026e-06_rb, &
   2.08356e-06_rb,2.13791e-06_rb,2.19330e-06_rb,2.24975e-06_rb,2.30728e-06_rb, &
   2.36589e-06_rb,2.42560e-06_rb,2.48641e-06_rb,2.54835e-06_rb,2.61142e-06_rb/)
   totplnk(151:181,11) = (/                                                    &
   2.67563e-06_rb,2.74100e-06_rb,2.80754e-06_rb,2.87526e-06_rb,2.94417e-06_rb, &
   3.01429e-06_rb,3.08562e-06_rb,3.15819e-06_rb,3.23199e-06_rb,3.30704e-06_rb, &
   3.38336e-06_rb,3.46096e-06_rb,3.53984e-06_rb,3.62002e-06_rb,3.70151e-06_rb, &
   3.78433e-06_rb,3.86848e-06_rb,3.95399e-06_rb,4.04084e-06_rb,4.12907e-06_rb, &
   4.21868e-06_rb,4.30968e-06_rb,4.40209e-06_rb,4.49592e-06_rb,4.59117e-06_rb, &
   4.68786e-06_rb,4.78600e-06_rb,4.88561e-06_rb,4.98669e-06_rb,5.08926e-06_rb, &
   5.19332e-06_rb/)
   totplnk(1:50,12) = (/                                                       &
   2.73921e-10_rb,3.04500e-10_rb,3.38056e-10_rb,3.74835e-10_rb,4.15099e-10_rb, &
   4.59126e-10_rb,5.07214e-10_rb,5.59679e-10_rb,6.16857e-10_rb,6.79103e-10_rb, &
   7.46796e-10_rb,8.20335e-10_rb,9.00144e-10_rb,9.86671e-10_rb,1.08039e-09_rb, &
   1.18180e-09_rb,1.29142e-09_rb,1.40982e-09_rb,1.53757e-09_rb,1.67529e-09_rb, &
   1.82363e-09_rb,1.98327e-09_rb,2.15492e-09_rb,2.33932e-09_rb,2.53726e-09_rb, &
   2.74957e-09_rb,2.97710e-09_rb,3.22075e-09_rb,3.48145e-09_rb,3.76020e-09_rb, &
   4.05801e-09_rb,4.37595e-09_rb,4.71513e-09_rb,5.07672e-09_rb,5.46193e-09_rb, &
   5.87201e-09_rb,6.30827e-09_rb,6.77205e-09_rb,7.26480e-09_rb,7.78794e-09_rb, &
   8.34304e-09_rb,8.93163e-09_rb,9.55537e-09_rb,1.02159e-08_rb,1.09151e-08_rb, &
   1.16547e-08_rb,1.24365e-08_rb,1.32625e-08_rb,1.41348e-08_rb,1.50554e-08_rb/)
   totplnk(51:100,12) = (/                                                     &
   1.60264e-08_rb,1.70500e-08_rb,1.81285e-08_rb,1.92642e-08_rb,2.04596e-08_rb, &
   2.17171e-08_rb,2.30394e-08_rb,2.44289e-08_rb,2.58885e-08_rb,2.74209e-08_rb, &
   2.90290e-08_rb,3.07157e-08_rb,3.24841e-08_rb,3.43371e-08_rb,3.62782e-08_rb, &
   3.83103e-08_rb,4.04371e-08_rb,4.26617e-08_rb,4.49878e-08_rb,4.74190e-08_rb, &
   4.99589e-08_rb,5.26113e-08_rb,5.53801e-08_rb,5.82692e-08_rb,6.12826e-08_rb, &
   6.44245e-08_rb,6.76991e-08_rb,7.11105e-08_rb,7.46634e-08_rb,7.83621e-08_rb, &
   8.22112e-08_rb,8.62154e-08_rb,9.03795e-08_rb,9.47081e-08_rb,9.92066e-08_rb, &
   1.03879e-07_rb,1.08732e-07_rb,1.13770e-07_rb,1.18998e-07_rb,1.24422e-07_rb, &
   1.30048e-07_rb,1.35880e-07_rb,1.41924e-07_rb,1.48187e-07_rb,1.54675e-07_rb, &
   1.61392e-07_rb,1.68346e-07_rb,1.75543e-07_rb,1.82988e-07_rb,1.90688e-07_rb/)
   totplnk(101:150,12) = (/                                                    &
   1.98650e-07_rb,2.06880e-07_rb,2.15385e-07_rb,2.24172e-07_rb,2.33247e-07_rb, &
   2.42617e-07_rb,2.52289e-07_rb,2.62272e-07_rb,2.72571e-07_rb,2.83193e-07_rb, &
   2.94147e-07_rb,3.05440e-07_rb,3.17080e-07_rb,3.29074e-07_rb,3.41430e-07_rb, &
   3.54155e-07_rb,3.67259e-07_rb,3.80747e-07_rb,3.94631e-07_rb,4.08916e-07_rb, &
   4.23611e-07_rb,4.38725e-07_rb,4.54267e-07_rb,4.70245e-07_rb,4.86666e-07_rb, &
   5.03541e-07_rb,5.20879e-07_rb,5.38687e-07_rb,5.56975e-07_rb,5.75751e-07_rb, &
   5.95026e-07_rb,6.14808e-07_rb,6.35107e-07_rb,6.55932e-07_rb,6.77293e-07_rb, &
   6.99197e-07_rb,7.21656e-07_rb,7.44681e-07_rb,7.68278e-07_rb,7.92460e-07_rb, &
   8.17235e-07_rb,8.42614e-07_rb,8.68606e-07_rb,8.95223e-07_rb,9.22473e-07_rb, &
   9.50366e-07_rb,9.78915e-07_rb,1.00813e-06_rb,1.03802e-06_rb,1.06859e-06_rb/)
   totplnk(151:181,12) = (/                                                    &
   1.09986e-06_rb,1.13184e-06_rb,1.16453e-06_rb,1.19796e-06_rb,1.23212e-06_rb, &
   1.26703e-06_rb,1.30270e-06_rb,1.33915e-06_rb,1.37637e-06_rb,1.41440e-06_rb, &
   1.45322e-06_rb,1.49286e-06_rb,1.53333e-06_rb,1.57464e-06_rb,1.61679e-06_rb, &
   1.65981e-06_rb,1.70370e-06_rb,1.74847e-06_rb,1.79414e-06_rb,1.84071e-06_rb, &
   1.88821e-06_rb,1.93663e-06_rb,1.98599e-06_rb,2.03631e-06_rb,2.08759e-06_rb, &
   2.13985e-06_rb,2.19310e-06_rb,2.24734e-06_rb,2.30260e-06_rb,2.35888e-06_rb, &
   2.41619e-06_rb/)
   totplnk(1:50,13) = (/                                                       &
   4.53634e-11_rb,5.11435e-11_rb,5.75754e-11_rb,6.47222e-11_rb,7.26531e-11_rb, &
   8.14420e-11_rb,9.11690e-11_rb,1.01921e-10_rb,1.13790e-10_rb,1.26877e-10_rb, &
   1.41288e-10_rb,1.57140e-10_rb,1.74555e-10_rb,1.93665e-10_rb,2.14613e-10_rb, &
   2.37548e-10_rb,2.62633e-10_rb,2.90039e-10_rb,3.19948e-10_rb,3.52558e-10_rb, &
   3.88073e-10_rb,4.26716e-10_rb,4.68719e-10_rb,5.14331e-10_rb,5.63815e-10_rb, &
   6.17448e-10_rb,6.75526e-10_rb,7.38358e-10_rb,8.06277e-10_rb,8.79625e-10_rb, &
   9.58770e-10_rb,1.04410e-09_rb,1.13602e-09_rb,1.23495e-09_rb,1.34135e-09_rb, &
   1.45568e-09_rb,1.57845e-09_rb,1.71017e-09_rb,1.85139e-09_rb,2.00268e-09_rb, &
   2.16464e-09_rb,2.33789e-09_rb,2.52309e-09_rb,2.72093e-09_rb,2.93212e-09_rb, &
   3.15740e-09_rb,3.39757e-09_rb,3.65341e-09_rb,3.92579e-09_rb,4.21559e-09_rb/)
   totplnk(51:100,13) = (/                                                     &
   4.52372e-09_rb,4.85115e-09_rb,5.19886e-09_rb,5.56788e-09_rb,5.95928e-09_rb, &
   6.37419e-09_rb,6.81375e-09_rb,7.27917e-09_rb,7.77168e-09_rb,8.29256e-09_rb, &
   8.84317e-09_rb,9.42487e-09_rb,1.00391e-08_rb,1.06873e-08_rb,1.13710e-08_rb, &
   1.20919e-08_rb,1.28515e-08_rb,1.36514e-08_rb,1.44935e-08_rb,1.53796e-08_rb, &
   1.63114e-08_rb,1.72909e-08_rb,1.83201e-08_rb,1.94008e-08_rb,2.05354e-08_rb, &
   2.17258e-08_rb,2.29742e-08_rb,2.42830e-08_rb,2.56545e-08_rb,2.70910e-08_rb, &
   2.85950e-08_rb,3.01689e-08_rb,3.18155e-08_rb,3.35373e-08_rb,3.53372e-08_rb, &
   3.72177e-08_rb,3.91818e-08_rb,4.12325e-08_rb,4.33727e-08_rb,4.56056e-08_rb, &
   4.79342e-08_rb,5.03617e-08_rb,5.28915e-08_rb,5.55270e-08_rb,5.82715e-08_rb, &
   6.11286e-08_rb,6.41019e-08_rb,6.71951e-08_rb,7.04119e-08_rb,7.37560e-08_rb/)
   totplnk(101:150,13) = (/                                                    &
   7.72315e-08_rb,8.08424e-08_rb,8.45927e-08_rb,8.84866e-08_rb,9.25281e-08_rb, &
   9.67218e-08_rb,1.01072e-07_rb,1.05583e-07_rb,1.10260e-07_rb,1.15107e-07_rb, &
   1.20128e-07_rb,1.25330e-07_rb,1.30716e-07_rb,1.36291e-07_rb,1.42061e-07_rb, &
   1.48031e-07_rb,1.54206e-07_rb,1.60592e-07_rb,1.67192e-07_rb,1.74015e-07_rb, &
   1.81064e-07_rb,1.88345e-07_rb,1.95865e-07_rb,2.03628e-07_rb,2.11643e-07_rb, &
   2.19912e-07_rb,2.28443e-07_rb,2.37244e-07_rb,2.46318e-07_rb,2.55673e-07_rb, &
   2.65316e-07_rb,2.75252e-07_rb,2.85489e-07_rb,2.96033e-07_rb,3.06891e-07_rb, &
   3.18070e-07_rb,3.29576e-07_rb,3.41417e-07_rb,3.53600e-07_rb,3.66133e-07_rb, &
   3.79021e-07_rb,3.92274e-07_rb,4.05897e-07_rb,4.19899e-07_rb,4.34288e-07_rb, &
   4.49071e-07_rb,4.64255e-07_rb,4.79850e-07_rb,4.95863e-07_rb,5.12300e-07_rb/)
   totplnk(151:181,13) = (/                                                    &
   5.29172e-07_rb,5.46486e-07_rb,5.64250e-07_rb,5.82473e-07_rb,6.01164e-07_rb, &
   6.20329e-07_rb,6.39979e-07_rb,6.60122e-07_rb,6.80767e-07_rb,7.01922e-07_rb, &
   7.23596e-07_rb,7.45800e-07_rb,7.68539e-07_rb,7.91826e-07_rb,8.15669e-07_rb, &
   8.40076e-07_rb,8.65058e-07_rb,8.90623e-07_rb,9.16783e-07_rb,9.43544e-07_rb, &
   9.70917e-07_rb,9.98912e-07_rb,1.02754e-06_rb,1.05681e-06_rb,1.08673e-06_rb, &
   1.11731e-06_rb,1.14856e-06_rb,1.18050e-06_rb,1.21312e-06_rb,1.24645e-06_rb, &
   1.28049e-06_rb/)
   totplnk(1:50,14) = (/                                                       &
   1.40113e-11_rb,1.59358e-11_rb,1.80960e-11_rb,2.05171e-11_rb,2.32266e-11_rb, &
   2.62546e-11_rb,2.96335e-11_rb,3.33990e-11_rb,3.75896e-11_rb,4.22469e-11_rb, &
   4.74164e-11_rb,5.31466e-11_rb,5.94905e-11_rb,6.65054e-11_rb,7.42522e-11_rb, &
   8.27975e-11_rb,9.22122e-11_rb,1.02573e-10_rb,1.13961e-10_rb,1.26466e-10_rb, &
   1.40181e-10_rb,1.55206e-10_rb,1.71651e-10_rb,1.89630e-10_rb,2.09265e-10_rb, &
   2.30689e-10_rb,2.54040e-10_rb,2.79467e-10_rb,3.07128e-10_rb,3.37190e-10_rb, &
   3.69833e-10_rb,4.05243e-10_rb,4.43623e-10_rb,4.85183e-10_rb,5.30149e-10_rb, &
   5.78755e-10_rb,6.31255e-10_rb,6.87910e-10_rb,7.49002e-10_rb,8.14824e-10_rb, &
   8.85687e-10_rb,9.61914e-10_rb,1.04385e-09_rb,1.13186e-09_rb,1.22631e-09_rb, &
   1.32761e-09_rb,1.43617e-09_rb,1.55243e-09_rb,1.67686e-09_rb,1.80992e-09_rb/)
   totplnk(51:100,14) = (/                                                     &
   1.95212e-09_rb,2.10399e-09_rb,2.26607e-09_rb,2.43895e-09_rb,2.62321e-09_rb, &
   2.81949e-09_rb,3.02844e-09_rb,3.25073e-09_rb,3.48707e-09_rb,3.73820e-09_rb, &
   4.00490e-09_rb,4.28794e-09_rb,4.58819e-09_rb,4.90647e-09_rb,5.24371e-09_rb, &
   5.60081e-09_rb,5.97875e-09_rb,6.37854e-09_rb,6.80120e-09_rb,7.24782e-09_rb, &
   7.71950e-09_rb,8.21740e-09_rb,8.74271e-09_rb,9.29666e-09_rb,9.88054e-09_rb, &
   1.04956e-08_rb,1.11434e-08_rb,1.18251e-08_rb,1.25422e-08_rb,1.32964e-08_rb, &
   1.40890e-08_rb,1.49217e-08_rb,1.57961e-08_rb,1.67140e-08_rb,1.76771e-08_rb, &
   1.86870e-08_rb,1.97458e-08_rb,2.08553e-08_rb,2.20175e-08_rb,2.32342e-08_rb, &
   2.45077e-08_rb,2.58401e-08_rb,2.72334e-08_rb,2.86900e-08_rb,3.02122e-08_rb, &
   3.18021e-08_rb,3.34624e-08_rb,3.51954e-08_rb,3.70037e-08_rb,3.88899e-08_rb/)
   totplnk(101:150,14) = (/                                                    &
   4.08568e-08_rb,4.29068e-08_rb,4.50429e-08_rb,4.72678e-08_rb,4.95847e-08_rb, &
   5.19963e-08_rb,5.45058e-08_rb,5.71161e-08_rb,5.98309e-08_rb,6.26529e-08_rb, &
   6.55857e-08_rb,6.86327e-08_rb,7.17971e-08_rb,7.50829e-08_rb,7.84933e-08_rb, &
   8.20323e-08_rb,8.57035e-08_rb,8.95105e-08_rb,9.34579e-08_rb,9.75488e-08_rb, &
   1.01788e-07_rb,1.06179e-07_rb,1.10727e-07_rb,1.15434e-07_rb,1.20307e-07_rb, &
   1.25350e-07_rb,1.30566e-07_rb,1.35961e-07_rb,1.41539e-07_rb,1.47304e-07_rb, &
   1.53263e-07_rb,1.59419e-07_rb,1.65778e-07_rb,1.72345e-07_rb,1.79124e-07_rb, &
   1.86122e-07_rb,1.93343e-07_rb,2.00792e-07_rb,2.08476e-07_rb,2.16400e-07_rb, &
   2.24568e-07_rb,2.32988e-07_rb,2.41666e-07_rb,2.50605e-07_rb,2.59813e-07_rb, &
   2.69297e-07_rb,2.79060e-07_rb,2.89111e-07_rb,2.99455e-07_rb,3.10099e-07_rb/)
   totplnk(151:181,14) = (/                                                    &
   3.21049e-07_rb,3.32311e-07_rb,3.43893e-07_rb,3.55801e-07_rb,3.68041e-07_rb, &
   3.80621e-07_rb,3.93547e-07_rb,4.06826e-07_rb,4.20465e-07_rb,4.34473e-07_rb, &
   4.48856e-07_rb,4.63620e-07_rb,4.78774e-07_rb,4.94325e-07_rb,5.10280e-07_rb, &
   5.26648e-07_rb,5.43436e-07_rb,5.60652e-07_rb,5.78302e-07_rb,5.96397e-07_rb, &
   6.14943e-07_rb,6.33949e-07_rb,6.53421e-07_rb,6.73370e-07_rb,6.93803e-07_rb, &
   7.14731e-07_rb,7.36157e-07_rb,7.58095e-07_rb,7.80549e-07_rb,8.03533e-07_rb, &
   8.27050e-07_rb/)
   totplnk(1:50,15) = (/                                                       &
   3.90483e-12_rb,4.47999e-12_rb,5.13122e-12_rb,5.86739e-12_rb,6.69829e-12_rb, &
   7.63467e-12_rb,8.68833e-12_rb,9.87221e-12_rb,1.12005e-11_rb,1.26885e-11_rb, &
   1.43534e-11_rb,1.62134e-11_rb,1.82888e-11_rb,2.06012e-11_rb,2.31745e-11_rb, &
   2.60343e-11_rb,2.92087e-11_rb,3.27277e-11_rb,3.66242e-11_rb,4.09334e-11_rb, &
   4.56935e-11_rb,5.09455e-11_rb,5.67338e-11_rb,6.31057e-11_rb,7.01127e-11_rb, &
   7.78096e-11_rb,8.62554e-11_rb,9.55130e-11_rb,1.05651e-10_rb,1.16740e-10_rb, &
   1.28858e-10_rb,1.42089e-10_rb,1.56519e-10_rb,1.72243e-10_rb,1.89361e-10_rb, &
   2.07978e-10_rb,2.28209e-10_rb,2.50173e-10_rb,2.73999e-10_rb,2.99820e-10_rb, &
   3.27782e-10_rb,3.58034e-10_rb,3.90739e-10_rb,4.26067e-10_rb,4.64196e-10_rb, &
   5.05317e-10_rb,5.49631e-10_rb,5.97347e-10_rb,6.48689e-10_rb,7.03891e-10_rb/)
   totplnk(51:100,15) = (/                                                     &
   7.63201e-10_rb,8.26876e-10_rb,8.95192e-10_rb,9.68430e-10_rb,1.04690e-09_rb, &
   1.13091e-09_rb,1.22079e-09_rb,1.31689e-09_rb,1.41957e-09_rb,1.52922e-09_rb, &
   1.64623e-09_rb,1.77101e-09_rb,1.90401e-09_rb,2.04567e-09_rb,2.19647e-09_rb, &
   2.35690e-09_rb,2.52749e-09_rb,2.70875e-09_rb,2.90127e-09_rb,3.10560e-09_rb, &
   3.32238e-09_rb,3.55222e-09_rb,3.79578e-09_rb,4.05375e-09_rb,4.32682e-09_rb, &
   4.61574e-09_rb,4.92128e-09_rb,5.24420e-09_rb,5.58536e-09_rb,5.94558e-09_rb, &
   6.32575e-09_rb,6.72678e-09_rb,7.14964e-09_rb,7.59526e-09_rb,8.06470e-09_rb, &
   8.55897e-09_rb,9.07916e-09_rb,9.62638e-09_rb,1.02018e-08_rb,1.08066e-08_rb, &
   1.14420e-08_rb,1.21092e-08_rb,1.28097e-08_rb,1.35446e-08_rb,1.43155e-08_rb, &
   1.51237e-08_rb,1.59708e-08_rb,1.68581e-08_rb,1.77873e-08_rb,1.87599e-08_rb/)
   totplnk(101:150,15) = (/                                                    &
   1.97777e-08_rb,2.08423e-08_rb,2.19555e-08_rb,2.31190e-08_rb,2.43348e-08_rb, &
   2.56045e-08_rb,2.69302e-08_rb,2.83140e-08_rb,2.97578e-08_rb,3.12636e-08_rb, &
   3.28337e-08_rb,3.44702e-08_rb,3.61755e-08_rb,3.79516e-08_rb,3.98012e-08_rb, &
   4.17265e-08_rb,4.37300e-08_rb,4.58143e-08_rb,4.79819e-08_rb,5.02355e-08_rb, &
   5.25777e-08_rb,5.50114e-08_rb,5.75393e-08_rb,6.01644e-08_rb,6.28896e-08_rb, &
   6.57177e-08_rb,6.86521e-08_rb,7.16959e-08_rb,7.48520e-08_rb,7.81239e-08_rb, &
   8.15148e-08_rb,8.50282e-08_rb,8.86675e-08_rb,9.24362e-08_rb,9.63380e-08_rb, &
   1.00376e-07_rb,1.04555e-07_rb,1.08878e-07_rb,1.13349e-07_rb,1.17972e-07_rb, &
   1.22751e-07_rb,1.27690e-07_rb,1.32793e-07_rb,1.38064e-07_rb,1.43508e-07_rb, &
   1.49129e-07_rb,1.54931e-07_rb,1.60920e-07_rb,1.67099e-07_rb,1.73473e-07_rb/)
   totplnk(151:181,15) = (/                                                    &
   1.80046e-07_rb,1.86825e-07_rb,1.93812e-07_rb,2.01014e-07_rb,2.08436e-07_rb, &
   2.16082e-07_rb,2.23957e-07_rb,2.32067e-07_rb,2.40418e-07_rb,2.49013e-07_rb, &
   2.57860e-07_rb,2.66963e-07_rb,2.76328e-07_rb,2.85961e-07_rb,2.95868e-07_rb, &
   3.06053e-07_rb,3.16524e-07_rb,3.27286e-07_rb,3.38345e-07_rb,3.49707e-07_rb, &
   3.61379e-07_rb,3.73367e-07_rb,3.85676e-07_rb,3.98315e-07_rb,4.11287e-07_rb, &
   4.24602e-07_rb,4.38265e-07_rb,4.52283e-07_rb,4.66662e-07_rb,4.81410e-07_rb, &
   4.96535e-07_rb/)
   totplnk(1:50,16) = (/                                                       &
   0.28639e-12_rb,0.33349e-12_rb,0.38764e-12_rb,0.44977e-12_rb,0.52093e-12_rb, &
   0.60231e-12_rb,0.69522e-12_rb,0.80111e-12_rb,0.92163e-12_rb,0.10586e-11_rb, &
   0.12139e-11_rb,0.13899e-11_rb,0.15890e-11_rb,0.18138e-11_rb,0.20674e-11_rb, &
   0.23531e-11_rb,0.26744e-11_rb,0.30352e-11_rb,0.34401e-11_rb,0.38936e-11_rb, &
   0.44011e-11_rb,0.49681e-11_rb,0.56010e-11_rb,0.63065e-11_rb,0.70919e-11_rb, &
   0.79654e-11_rb,0.89357e-11_rb,0.10012e-10_rb,0.11205e-10_rb,0.12526e-10_rb, &
   0.13986e-10_rb,0.15600e-10_rb,0.17380e-10_rb,0.19342e-10_rb,0.21503e-10_rb, &
   0.23881e-10_rb,0.26494e-10_rb,0.29362e-10_rb,0.32509e-10_rb,0.35958e-10_rb, &
   0.39733e-10_rb,0.43863e-10_rb,0.48376e-10_rb,0.53303e-10_rb,0.58679e-10_rb, &
   0.64539e-10_rb,0.70920e-10_rb,0.77864e-10_rb,0.85413e-10_rb,0.93615e-10_rb/)
   totplnk(51:100,16) = (/                                                     &
   0.10252e-09_rb,0.11217e-09_rb,0.12264e-09_rb,0.13397e-09_rb,0.14624e-09_rb, &
   0.15950e-09_rb,0.17383e-09_rb,0.18930e-09_rb,0.20599e-09_rb,0.22399e-09_rb, &
   0.24339e-09_rb,0.26427e-09_rb,0.28674e-09_rb,0.31090e-09_rb,0.33686e-09_rb, &
   0.36474e-09_rb,0.39466e-09_rb,0.42676e-09_rb,0.46115e-09_rb,0.49800e-09_rb, &
   0.53744e-09_rb,0.57964e-09_rb,0.62476e-09_rb,0.67298e-09_rb,0.72448e-09_rb, &
   0.77945e-09_rb,0.83809e-09_rb,0.90062e-09_rb,0.96725e-09_rb,0.10382e-08_rb, &
   0.11138e-08_rb,0.11941e-08_rb,0.12796e-08_rb,0.13704e-08_rb,0.14669e-08_rb, &
   0.15694e-08_rb,0.16781e-08_rb,0.17934e-08_rb,0.19157e-08_rb,0.20453e-08_rb, &
   0.21825e-08_rb,0.23278e-08_rb,0.24815e-08_rb,0.26442e-08_rb,0.28161e-08_rb, &
   0.29978e-08_rb,0.31898e-08_rb,0.33925e-08_rb,0.36064e-08_rb,0.38321e-08_rb/)
   totplnk(101:150,16) = (/                                                    &
   0.40700e-08_rb,0.43209e-08_rb,0.45852e-08_rb,0.48636e-08_rb,0.51567e-08_rb, &
   0.54652e-08_rb,0.57897e-08_rb,0.61310e-08_rb,0.64897e-08_rb,0.68667e-08_rb, &
   0.72626e-08_rb,0.76784e-08_rb,0.81148e-08_rb,0.85727e-08_rb,0.90530e-08_rb, &
   0.95566e-08_rb,0.10084e-07_rb,0.10638e-07_rb,0.11217e-07_rb,0.11824e-07_rb, &
   0.12458e-07_rb,0.13123e-07_rb,0.13818e-07_rb,0.14545e-07_rb,0.15305e-07_rb, &
   0.16099e-07_rb,0.16928e-07_rb,0.17795e-07_rb,0.18699e-07_rb,0.19643e-07_rb, &
   0.20629e-07_rb,0.21656e-07_rb,0.22728e-07_rb,0.23845e-07_rb,0.25010e-07_rb, &
   0.26223e-07_rb,0.27487e-07_rb,0.28804e-07_rb,0.30174e-07_rb,0.31600e-07_rb, &
   0.33084e-07_rb,0.34628e-07_rb,0.36233e-07_rb,0.37902e-07_rb,0.39637e-07_rb, &
   0.41440e-07_rb,0.43313e-07_rb,0.45259e-07_rb,0.47279e-07_rb,0.49376e-07_rb/)
   totplnk(151:181,16) = (/                                                    &
   0.51552e-07_rb,0.53810e-07_rb,0.56153e-07_rb,0.58583e-07_rb,0.61102e-07_rb, &
   0.63713e-07_rb,0.66420e-07_rb,0.69224e-07_rb,0.72129e-07_rb,0.75138e-07_rb, &
   0.78254e-07_rb,0.81479e-07_rb,0.84818e-07_rb,0.88272e-07_rb,0.91846e-07_rb, &
   0.95543e-07_rb,0.99366e-07_rb,0.10332e-06_rb,0.10740e-06_rb,0.11163e-06_rb, &
   0.11599e-06_rb,0.12050e-06_rb,0.12515e-06_rb,0.12996e-06_rb,0.13493e-06_rb, &
   0.14005e-06_rb,0.14534e-06_rb,0.15080e-06_rb,0.15643e-06_rb,0.16224e-06_rb, &
   0.16823e-06_rb/)
   totplk16(1:50) = (/                                                         &
   0.28481e-12_rb,0.33159e-12_rb,0.38535e-12_rb,0.44701e-12_rb,0.51763e-12_rb, &
   0.59836e-12_rb,0.69049e-12_rb,0.79549e-12_rb,0.91493e-12_rb,0.10506e-11_rb, &
   0.12045e-11_rb,0.13788e-11_rb,0.15758e-11_rb,0.17984e-11_rb,0.20493e-11_rb, &
   0.23317e-11_rb,0.26494e-11_rb,0.30060e-11_rb,0.34060e-11_rb,0.38539e-11_rb, &
   0.43548e-11_rb,0.49144e-11_rb,0.55387e-11_rb,0.62344e-11_rb,0.70086e-11_rb, &
   0.78692e-11_rb,0.88248e-11_rb,0.98846e-11_rb,0.11059e-10_rb,0.12358e-10_rb, &
   0.13794e-10_rb,0.15379e-10_rb,0.17128e-10_rb,0.19055e-10_rb,0.21176e-10_rb, &
   0.23508e-10_rb,0.26070e-10_rb,0.28881e-10_rb,0.31963e-10_rb,0.35339e-10_rb, &
   0.39034e-10_rb,0.43073e-10_rb,0.47484e-10_rb,0.52299e-10_rb,0.57548e-10_rb, &
   0.63267e-10_rb,0.69491e-10_rb,0.76261e-10_rb,0.83616e-10_rb,0.91603e-10_rb/)
   totplk16(51:100) = (/                                                       &
   0.10027e-09_rb,0.10966e-09_rb,0.11983e-09_rb,0.13084e-09_rb,0.14275e-09_rb, &
   0.15562e-09_rb,0.16951e-09_rb,0.18451e-09_rb,0.20068e-09_rb,0.21810e-09_rb, &
   0.23686e-09_rb,0.25704e-09_rb,0.27875e-09_rb,0.30207e-09_rb,0.32712e-09_rb, &
   0.35400e-09_rb,0.38282e-09_rb,0.41372e-09_rb,0.44681e-09_rb,0.48223e-09_rb, &
   0.52013e-09_rb,0.56064e-09_rb,0.60392e-09_rb,0.65015e-09_rb,0.69948e-09_rb, &
   0.75209e-09_rb,0.80818e-09_rb,0.86794e-09_rb,0.93157e-09_rb,0.99929e-09_rb, &
   0.10713e-08_rb,0.11479e-08_rb,0.12293e-08_rb,0.13157e-08_rb,0.14074e-08_rb, &
   0.15047e-08_rb,0.16079e-08_rb,0.17172e-08_rb,0.18330e-08_rb,0.19557e-08_rb, &
   0.20855e-08_rb,0.22228e-08_rb,0.23680e-08_rb,0.25214e-08_rb,0.26835e-08_rb, &
   0.28546e-08_rb,0.30352e-08_rb,0.32257e-08_rb,0.34266e-08_rb,0.36384e-08_rb/)
   totplk16(101:150) = (/                                                      &
   0.38615e-08_rb,0.40965e-08_rb,0.43438e-08_rb,0.46041e-08_rb,0.48779e-08_rb, &
   0.51658e-08_rb,0.54683e-08_rb,0.57862e-08_rb,0.61200e-08_rb,0.64705e-08_rb, &
   0.68382e-08_rb,0.72240e-08_rb,0.76285e-08_rb,0.80526e-08_rb,0.84969e-08_rb, &
   0.89624e-08_rb,0.94498e-08_rb,0.99599e-08_rb,0.10494e-07_rb,0.11052e-07_rb, &
   0.11636e-07_rb,0.12246e-07_rb,0.12884e-07_rb,0.13551e-07_rb,0.14246e-07_rb, &
   0.14973e-07_rb,0.15731e-07_rb,0.16522e-07_rb,0.17347e-07_rb,0.18207e-07_rb, &
   0.19103e-07_rb,0.20037e-07_rb,0.21011e-07_rb,0.22024e-07_rb,0.23079e-07_rb, &
   0.24177e-07_rb,0.25320e-07_rb,0.26508e-07_rb,0.27744e-07_rb,0.29029e-07_rb, &
   0.30365e-07_rb,0.31753e-07_rb,0.33194e-07_rb,0.34691e-07_rb,0.36246e-07_rb, &
   0.37859e-07_rb,0.39533e-07_rb,0.41270e-07_rb,0.43071e-07_rb,0.44939e-07_rb/)
   totplk16(151:181) = (/                                                      &
   0.46875e-07_rb,0.48882e-07_rb,0.50961e-07_rb,0.53115e-07_rb,0.55345e-07_rb, &
   0.57655e-07_rb,0.60046e-07_rb,0.62520e-07_rb,0.65080e-07_rb,0.67728e-07_rb, &
   0.70466e-07_rb,0.73298e-07_rb,0.76225e-07_rb,0.79251e-07_rb,0.82377e-07_rb, &
   0.85606e-07_rb,0.88942e-07_rb,0.92386e-07_rb,0.95942e-07_rb,0.99612e-07_rb, &
   0.10340e-06_rb,0.10731e-06_rb,0.11134e-06_rb,0.11550e-06_rb,0.11979e-06_rb, &
   0.12421e-06_rb,0.12876e-06_rb,0.13346e-06_rb,0.13830e-06_rb,0.14328e-06_rb, &
   0.14841e-06_rb/)
!
   end subroutine lwavplank
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_lw_setcoef_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_lw_taumol_k
!-------------------------------------------------------------------------------
!   --------------------------------------------------------------------------
!  |                                                                          |
!  |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
!  |  This software may be used, copied, or redistributed as long as it is    |
!  |  not sold and this copyright notice is reproduced on each copy made.     |
!  |  This model is provided as is without any express or implied warranties. |
!  |                       (http://www.rtweb.aer.com/)                        |
!  |                                                                          |
!   --------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   use parkind_k,  only : im => kind_im, rb => kind_rb 
   use parrrtm_k,  only : mg, nbndlw, maxxsec, ngptlw
   use rrlw_con_k, only : oneminus
   use rrlw_wvn_k, only : nspa, nspb
   use rrlw_vsn_k, only : hvrtau, hnamtau
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol(nlayers, pavel, wx, coldry,                               &
                     laytrop, jp, jt, jt1, planklay, planklev, plankbnd,       &
                     colh2o, colco2, colo3, coln2o, colco, colch4, colo2,      &
                     colbrd, fac00, fac01, fac10, fac11,                       &
                     rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,         &
                     rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,       &
                     rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,         &
                     selffac, selffrac, indself, forfac, forfrac, indfor,      &
                     minorfrac, scaleminor, scaleminorn2, indminor,            &
                     fracs, taug)
!-------------------------------------------------------------------------------
!                                                                              *
!                   Optical depths developed for the                           *
!                                                                              *
!                 RAPID RADIATIVE TRANSFER MODEL (RRTM)                        *
!                                                                              *
!                                                                              *
!             ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                     *
!                         131 HARTWELL AVENUE                                  *
!                         LEXINGTON, MA 02421                                  *
!                                                                              *
!                                                                              *
!                            ELI J. MLAWER                                     *
!                          JENNIFER DELAMERE                                   *
!                          STEVEN J. TAUBMAN                                   *
!                          SHEPARD A. CLOUGH                                   *
!                                                                              *
!                                                                              *
!                                                                              *
!                                                                              *
!                        email:  mlawer@aer.com                                *
!                        email:  jdelamer@aer.com                              *
!                                                                              *
!         The authors wish to acknowledge the contributions of the             *
!         following people:  Karen Cady-Pereira, Patrick D. Brown,             *
!         Michael J. Iacono, Ronald E. Farren, Luke Chen, Robert Bergstrom.    *
!                                                                              *
! ******************************************************************************
!                                                                              *
!   Revision for g-point reduction: Michael J. Iacono, AER, Inc.               *
!                                                                              *
! ******************************************************************************
!      TAUMOL                                                                  *
!                                                                              *
!      This file contains the subroutines TAUGBn (where n goes from            *
!      1 to 16).  TAUGBn calculates the optical depths and Planck fractions    *
!      per g-value and layer for band n.                                       *
!                                                                              *
!   Output:  optical depths (unitless)                                         *
!            fractions needed to compute Planck functions at every layer       *
!                and g-value                                                   *
!                                                                              *
!      COMMON /TAUGCOM/  TAUG(MXLAY,MG)                                        *
!      COMMON /PLANKG/   FRACS(MXLAY,MG)                                       *
!                                                                              *
!   Input                                                                      *
!                                                                              *
!      COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                  *
!      COMMON /PRECISE/  ONEMINUS                                              *
!      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                    *
!      &                 PZ(0:MXLAY),TZ(0:MXLAY)                               *
!      COMMON /PROFdata/ LAYTROP,                                              *
!     &                  COLH2O(MXLAY),COLCO2(MXLAY),COLO3(MXLAY),             *
!     &                  COLN2O(MXLAY),COLCO(MXLAY),COLCH4(MXLAY),             *
!     &                  COLO2(MXLAY)
!      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                            *
!     &                  FAC10(MXLAY),FAC11(MXLAY)                             *
!      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                        *
!      COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)       *
!                                                                              *
!      Description:                                                            *
!      NG(IBAND) - number of g-values in band IBAND                            *
!      NSPA(IBAND) - for the lower atmosphere, the number of reference         *
!                    atmospheres that are stored for band IBAND per            *
!                    pressure level and temperature.  Each of these            *
!                    atmospheres has different relative amounts of the         *
!                    key species for the band (i.e. different binary           *
!                    species parameters).                                      *
!      NSPB(IBAND) - same for upper atmosphere                                 *
!      ONEMINUS - since problems are caused in some cases by interpolation     *
!                 parameters equal to or greater than 1, for these cases       *
!                 these parameters are set to this value, slightly < 1.        *
!      PAVEL - layer pressures (mb)                                            *
!      TAVEL - layer temperatures (degrees K)                                  *
!      PZ - level pressures (mb)                                               *
!      TZ - level temperatures (degrees K)                                     *
!      LAYTROP - layer at which switch is made from one combination of         *
!                key species to another                                        *
!      COLH2O, COLCO2, COLO3, COLN2O, COLCH4 - column amounts of water         *
!                vapor,carbon dioxide, ozone, nitrous ozide, methane,          *
!                respectively (molecules/cm**2)                                *
!      FACij(LAY) - for layer LAY, these are factors that are needed to        *
!                   compute the interpolation factors that multiply the        *
!                   appropriate reference k-values.  A value of 0 (1) for      *
!                   i,j indicates that the corresponding factor multiplies     *
!                   reference k-value for the lower (higher) of the two        *
!                   appropriate temperatures, and altitudes, respectively.     *
!      JP - the index of the lower (in altitude) of the two appropriate        *
!           reference pressure levels needed for interpolation                 *
!      JT, JT1 - the indices of the lower of the two appropriate reference     *
!                temperatures needed for interpolation (for pressure           *
!                levels JP and JP+1, respectively)                             *
!      SELFFAC - scale factor needed for water vapor self-continuum, equals    *
!                (water vapor density)/(atmospheric density at 296K and        *
!                1013 mb)                                                      *
!      SELFFRAC - factor needed for temperature interpolation of reference     *
!                 water vapor self-continuum data                              *
!      INDSELF - index of the lower of the two appropriate reference           *
!                temperatures needed for the self-continuum interpolation      *
!      FORFAC  - scale factor needed for water vapor foreign-continuum.        *
!      FORFRAC - factor needed for temperature interpolation of reference      *
!                 water vapor foreign-continuum data                           *
!      INDFOR  - index of the lower of the two appropriate reference           *
!                temperatures needed for the foreign-continuum interpolation   *
!                                                                              *
!   Data input                                                                 *
!      COMMON /Kn/ KA(NSPA(n),5,13,MG), KB(NSPB(n),5,13:59,MG), SELFREF(10,MG),*
!                  FORREF(4,MG), KA_M'MGAS', KB_M'MGAS'                        *
!         (note:  n is the band number,'MGAS' is the species name of the minor *
!          gas)                                                                *
!                                                                              *
!      Description:                                                            *
!      KA - k-values for low reference atmospheres (key-species only)          *
!           (units: cm**2/molecule)                                            *
!      KB - k-values for high reference atmospheres (key-species only)         *
!           (units: cm**2/molecule)                                            *
!      KA_M'MGAS' - k-values for low reference atmosphere minor species        *
!           (units: cm**2/molecule)                                            *
!      KB_M'MGAS' - k-values for high reference atmosphere minor species       *
!           (units: cm**2/molecule)                                            *
!      SELFREF - k-values for water vapor self-continuum for reference         *
!                atmospheres (used below LAYTROP)                              *
!                (units: cm**2/molecule)                                       *
!      FORREF  - k-values for water vapor foreign-continuum for reference      *
!                atmospheres (used below/above LAYTROP)                        *
!                (units: cm**2/molecule)                                       *
!                                                                              *
!      dimension ABSA(65*NSPA(n),MG), ABSB(235*NSPB(n),MG)                     *
!      equivalence (KA,ABSA),(KB,ABSB)                                         *
!-------------------------------------------------------------------------------
!  input :
!    layers                   - total number of layers
!    pavel(nlayers)           - layer pressures (mb)
!    wx(maxxsec,nlayers)      - cross-section amounts (mol/cm2)
!    coldry(nlayers)          - column amount (dry air)
!    laytrop                  - tropopause layer index
!    jp(nlayers)    
!    jt(nlayers)    
!    jt1(nlayers)   
!    planklay(nlayers,nbndlw)
!    planklev(nlayers,nbndlw)
!    plankbnd(nbndlw) 
!    colh2o(nlayers)          - column amount (h2o)
!    colco2(nlayers)          - column amount (co2)
!    colo3(nlayers)           - column amount (o3)
!    coln2o(nlayers)          - column amount (n2o)
!    colco(nlayers)           - column amount (co)
!    colch4(nlayers)          - column amount (ch4)
!    colo2(nlayers)           - column amount (o2)
!    colbrd(nlayers)          - column amount (broadening gases)
!    indself(nlayers)
!    indfor(nlayers)
!    selffac(nlayers)
!    selffrac(nlayers)
!    forfac(nlayers)
!    forfrac(nlayers)
!    indminor(nlayers)
!    minorfrac(nlayers)
!    scaleminor(nlayers)
!    scaleminorn2(nlayers)
!    fac00(nlayers), fac01(nlayers), fac10(nlayers), fac11(nlayers)
!    rat_h2oco2(nlayers), rat_h2oco2_1(nlayers)
!    rat_h2oo3(nlayers),rat_h2oo3_1(nlayers)
!    rat_h2on2o(nlayers),rat_h2on2o_1(nlayers)
!    rat_h2och4(nlayers),rat_h2och4_1(nlayers)
!    rat_n2oco2(nlayers),rat_n2oco2_1(nlayers)
!    rat_o3co2(nlayers),rat_o3co2_1(nlayers)
!
!  output :
!    fracs(nlayers,ngptlw)    - planck fractions
!    taug(nlayers,ngptlw)     - gaseous optical depth
!
!-------------------------------------------------------------------------------
!
! Input
!
   integer(kind=im)              , intent(in   ) :: nlayers       
   real(kind=rb), dimension(:)   , intent(in   ) :: pavel         
   real(kind=rb), dimension(:,:) , intent(in   ) :: wx
   real(kind=rb), dimension(:)   , intent(in   ) :: coldry
!
   integer(kind=im)              , intent(in   ) :: laytrop        
   integer(kind=im), dimension(:), intent(in   ) :: jp         
   integer(kind=im), dimension(:), intent(in   ) :: jt          
   integer(kind=im), dimension(:), intent(in   ) :: jt1     
   real(kind=rb), dimension(:,:) , intent(in   ) :: planklay
   real(kind=rb), dimension(0:,:), intent(in   ) :: planklev
   real(kind=rb), dimension(:)   , intent(in   ) :: plankbnd
!
   real(kind=rb), dimension(:)   , intent(in   ) :: colh2o  
   real(kind=rb), dimension(:)   , intent(in   ) :: colco2  
   real(kind=rb), dimension(:)   , intent(in   ) :: colo3   
   real(kind=rb), dimension(:)   , intent(in   ) :: coln2o  
   real(kind=rb), dimension(:)   , intent(in   ) :: colco   
   real(kind=rb), dimension(:)   , intent(in   ) :: colch4    
   real(kind=rb), dimension(:)   , intent(in   ) :: colo2     
   real(kind=rb), dimension(:)   , intent(in   ) :: colbrd    
!
   integer(kind=im), dimension(:), intent(in   ) :: indself
   integer(kind=im), dimension(:), intent(in   ) :: indfor
   real(kind=rb), dimension(:)   , intent(in   ) :: selffac
   real(kind=rb), dimension(:)   , intent(in   ) :: selffrac
   real(kind=rb), dimension(:)   , intent(in   ) :: forfac
   real(kind=rb), dimension(:)   , intent(in   ) :: forfrac
!
   integer(kind=im), dimension(:), intent(in   ) :: indminor
   real(kind=rb), dimension(:)   , intent(in   ) :: minorfrac
   real(kind=rb), dimension(:)   , intent(in   ) :: scaleminor
   real(kind=rb), dimension(:)   , intent(in   ) :: scaleminorn2
   real(kind=rb), dimension(:)   , intent(in   ) :: fac00, fac01, fac10, fac11 
   real(kind=rb), dimension(:)   , intent(in   ) :: rat_h2oco2, rat_h2oco2_1,  &
                                                      rat_h2oo3, rat_h2oo3_1,  &
                                                    rat_h2on2o, rat_h2on2o_1,  &
                                                    rat_h2och4, rat_h2och4_1,  &
                                                    rat_n2oco2, rat_n2oco2_1,  &
                                                      rat_o3co2, rat_o3co2_1
!
! Output 
!
   real(kind=rb), dimension(:,:), intent(  out) :: fracs
   real(kind=rb), dimension(:,:), intent(  out) :: taug
!-------------------------------------------------------------------------------
!
   hvrtau = '$Revision: 1.7 $'
!
! Calculate gaseous optical depth and planck fractions for each spectral band.
!
   call taugb1
   call taugb2
   call taugb3
   call taugb4
   call taugb5
   call taugb6
   call taugb7
   call taugb8
   call taugb9
   call taugb10
   call taugb11
   call taugb12
   call taugb13
   call taugb14
   call taugb15
   call taugb16
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb1
!-------------------------------------------------------------------------------
!
!  ------- Modifications -------
!  Written by Eli J. Mlawer, Atmospheric & Environmental Research.
!  Revised by Michael J. Iacono, Atmospheric & Environmental Research.
!
!     band 1:  10-350 cm-1 (low key - h2o; low minor - n2)
!                          (high key - h2o; high minor - n2)
!
!     note: previous versions of rrtm band 1: 
!           10-250 cm-1 (low - h2o; high - h2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng1
   use rrlw_kg01_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         ka_mn2, kb_mn2, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   real(kind=rb)    :: pp, corradj, scalen2, tauself, taufor, taun2
!-------------------------------------------------------------------------------
!
! Minor gas mapping levels:
!     lower - n2, p = 142.5490 mbar, t = 215.70 k
!     upper - n2, p = 142.5490 mbar, t = 215.70 k
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(1) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(1) + 1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
     pp = pavel(lay)
     corradj =  1.
     if (pp.lt.250._rb) then
       corradj = 1._rb-0.15_rb*(250._rb-pp)/154.4_rb
     endif
!
     scalen2 = colbrd(lay) * scaleminorn2(lay)
     do ig = 1,ng1
       tauself = selffac(lay)*(selfref(inds,ig)+selffrac(lay)*                 &
                (selfref(inds+1,ig)-selfref(inds,ig)))
       taufor = forfac(lay)*(forref(indf,ig)+forfrac(lay)*                     &
               (forref(indf+1,ig)- forref(indf,ig))) 
       taun2 = scalen2*(ka_mn2(indm,ig)+                                       &
               minorfrac(lay)*(ka_mn2(indm+1,ig)-ka_mn2(indm,ig)))
       taug(lay,ig) = corradj*(colh2o(lay)*                                    &
                      (fac00(lay)*absa(ind0,ig)+                               &
                       fac10(lay)*absa(ind0+1,ig)+                             &
                       fac01(lay)*absa(ind1,ig)+                               &
                       fac11(lay)*absa(ind1+1,ig))                             &
                      +tauself+taufor+taun2)
       fracs(lay,ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(1) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(1) + 1
     indf = indfor(lay)
     indm = indminor(lay)
     pp = pavel(lay)
     corradj =  1._rb-0.15_rb*(pp/95.6_rb)
!
     scalen2 = colbrd(lay)*scaleminorn2(lay)
!
     do ig = 1,ng1
       taufor = forfac(lay)*(forref(indf,ig)+                                  &
                forfrac(lay)*(forref(indf+1,ig)-forref(indf,ig))) 
       taun2 = scalen2*(kb_mn2(indm,ig)+                                       &
               minorfrac(lay)*(kb_mn2(indm+1,ig)-kb_mn2(indm,ig)))
       taug(lay,ig) = corradj*(colh2o(lay)*                                    &
                      (fac00(lay)*absb(ind0,ig)+                               &
                       fac10(lay)*absb(ind0+1,ig)+                             &
                       fac01(lay)*absb(ind1,ig)+                               &
                       fac11(lay)*absb(ind1+1,ig))                             &
                      +taufor + taun2)
       fracs(lay,ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb1
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb2
!-------------------------------------------------------------------------------
!
!  abstract : band 2,  350-500 cm-1 (low key - h2o; high key - h2o)
!
!  note: previous version of rrtm band 2: 
!        250 - 500 cm-1 (low - h2o; high - h2o)
! 
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng2, ngs1
   use rrlw_kg02_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
   real(kind=rb)    :: pp, corradj, tauself, taufor
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(2) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(2) + 1
     inds = indself(lay)
     indf = indfor(lay)
     pp = pavel(lay)
     corradj = 1._rb-.05_rb*(pp-100._rb)/900._rb
     do ig = 1,ng2
       tauself = selffac(lay)*(selfref(inds,ig)+selffrac(lay)*                 &
                (selfref(inds+1,ig)-selfref(inds,ig)))
       taufor =  forfac(lay)*(forref(indf,ig)+forfrac(lay)*                    &
                (forref(indf+1,ig) - forref(indf,ig))) 
       taug(lay,ngs1+ig) = corradj*(colh2o(lay)*                               &
                           (fac00(lay)*absa(ind0,ig)+                          &
                            fac10(lay)*absa(ind0+1,ig)+                        &
                            fac01(lay)*absa(ind1,ig)+                          &
                            fac11(lay)*absa(ind1+1,ig))                        &
                           +tauself+taufor)
       fracs(lay,ngs1+ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(2) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(2) + 1
     indf = indfor(lay)
     do ig = 1,ng2
       taufor = forfac(lay)*(forref(indf,ig)+                                  &
                forfrac(lay)*(forref(indf+1,ig)-forref(indf,ig))) 
       taug(lay,ngs1+ig) = colh2o(lay)*                                        &
                           (fac00(lay)*absb(ind0,ig)+                          &
                            fac10(lay)*absb(ind0+1,ig)+                        &
                            fac01(lay)*absb(ind1,ig)+                          &
                            fac11(lay)*absb(ind1+1,ig))                        &
                           +taufor
       fracs(lay,ngs1+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb2
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb3
!-------------------------------------------------------------------------------
!
!  abstract : band 3,  500-630 cm-1 (low key - h2o,co2; low minor - n2o)
!                                   (high key - h2o,co2; high minor - n2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng3, ngs2
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg03_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         ka_mn2o, kb_mn2o, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   integer(kind=im) :: js, js1, jmn2o, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_mn2o, specparm_mn2o, specmult_mn2o,            &
                       fmn2o, fmn2omf, chi_n2o, ratn2o, adjfac, adjcoln2o
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor, n2om1, n2om2, absn2o
   real(kind=rb)    :: refrat_planck_a, refrat_planck_b, refrat_m_a, refrat_m_b
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Minor gas mapping levels:
!     lower - n2o, p = 706.272 mbar, t = 278.94 k
!     upper - n2o, p = 95.58 mbar, t = 215.7 k
!
!  P = 212.725 mb
!
   refrat_planck_a = chi_mls(1,9)/chi_mls(2,9)
!
!  P = 95.58 mb
!
   refrat_planck_b = chi_mls(1,13)/chi_mls(2,13)
!
!  P = 706.270mb
!
   refrat_m_a = chi_mls(1,3)/chi_mls(2,3)
!
!  P = 95.58 mb 
!
   refrat_m_b = chi_mls(1,13)/chi_mls(2,13)
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the water vapor 
! self-continuum and foreign continuum is interpolated (in temperature) 
! separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colh2o(lay)+rat_h2oco2(lay)*colco2(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1+int(specmult)
     fs = mod(specmult,1.0_rb)        
!
     speccomb1 = colh2o(lay)+rat_h2oco2_1(lay)*colco2(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1.ge.oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1+int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_mn2o = colh2o(lay)+refrat_m_a*colco2(lay)
     specparm_mn2o = colh2o(lay)/speccomb_mn2o
     if (specparm_mn2o.ge.oneminus) specparm_mn2o = oneminus
     specmult_mn2o = 8._rb*specparm_mn2o
     jmn2o = 1+int(specmult_mn2o)
     fmn2o = mod(specmult_mn2o,1.0_rb)
     fmn2omf = minorfrac(lay)*fmn2o
!
! In atmospheres where the amount of N2O is too great to be considered
! a minor species, adjust the column amount of N2O by an empirical factor 
! to obtain the proper contribution.
!
     chi_n2o = coln2o(lay)/coldry(lay)
     ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
     if (ratn2o.gt.1.5_rb) then
       adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
       adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcoln2o = coln2o(lay)
     endif
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck.ge.oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1+int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(3)+js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(3)+js1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     if (specparm.lt.0.125_rb) then
       p = fs-1
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm.gt.0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb-fs)*fac00(lay)
       fac010 = (1._rb-fs)*fac10(lay)
       fac100 = fs*fac00(lay)
       fac110 = fs*fac10(lay)
     endif
!
     if (specparm1.lt.0.125_rb) then
       p = fs1-1
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1.gt.0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb-fs1)*fac01(lay)
       fac011 = (1._rb-fs1)*fac11(lay)
       fac101 = fs1*fac01(lay)
       fac111 = fs1*fac11(lay)
     endif
!
     do ig = 1,ng3
       tauself = selffac(lay)*(selfref(inds,ig)+selffrac(lay)*                 &
                (selfref(inds+1,ig)-selfref(inds,ig)))
       taufor = forfac(lay)*(forref(indf,ig)+forfrac(lay)*                     &
               (forref(indf+1,ig)-forref(indf,ig))) 
       n2om1 = ka_mn2o(jmn2o,indm,ig)+fmn2o*                                   &
              (ka_mn2o(jmn2o+1,indm,ig)-ka_mn2o(jmn2o,indm,ig))
       n2om2 = ka_mn2o(jmn2o,indm+1,ig)+fmn2o*                                 &
              (ka_mn2o(jmn2o+1,indm+1,ig)-ka_mn2o(jmn2o,indm+1,ig))
       absn2o = n2om1 + minorfrac(lay)*(n2om2 - n2om1)
!
       if (specparm.lt.0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm.gt.0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1.lt.0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1.gt.0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs2+ig) = tau_major+tau_major1                                &
                         + tauself+taufor                                      &
                         + adjcoln2o*absn2o
       fracs(lay,ngs2+ig) = fracrefa(ig,jpl)+fpl*                              &
                           (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     speccomb = colh2o(lay)+rat_h2oco2(lay)*colco2(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 4._rb*(specparm)
     js = 1+int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay)+rat_h2oco2_1(lay)*colco2(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1.ge.oneminus) specparm1 = oneminus
     specmult1 = 4._rb*(specparm1)
     js1 = 1+int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     fac000 = (1._rb-fs)*fac00(lay)
     fac010 = (1._rb-fs)*fac10(lay)
     fac100 = fs*fac00(lay)
     fac110 = fs*fac10(lay)
     fac001 = (1._rb-fs1)*fac01(lay)
     fac011 = (1._rb-fs1)*fac11(lay)
     fac101 = fs1*fac01(lay)
     fac111 = fs1*fac11(lay)
!
     speccomb_mn2o = colh2o(lay)+refrat_m_b*colco2(lay)
     specparm_mn2o = colh2o(lay)/speccomb_mn2o
     if (specparm_mn2o.ge.oneminus) specparm_mn2o = oneminus
     specmult_mn2o = 4._rb*specparm_mn2o
     jmn2o = 1+int(specmult_mn2o)
     fmn2o = mod(specmult_mn2o,1.0_rb)
     fmn2omf = minorfrac(lay)*fmn2o
!
! In atmospheres where the amount of N2O is too great to be considered
! a minor species, adjust the column amount of N2O by an empirical factor 
! to obtain the proper contribution.
!
     chi_n2o = coln2o(lay)/coldry(lay)
     ratn2o = 1.e20*chi_n2o/chi_mls(4,jp(lay)+1)
     if (ratn2o .gt. 1.5_rb) then
       adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
       adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcoln2o = coln2o(lay)
     endif
!
     speccomb_planck = colh2o(lay)+refrat_planck_b*colco2(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 4._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(3) + js
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(3) + js1
     indf = indfor(lay)
     indm = indminor(lay)
!
     do ig = 1,ng3
       taufor = forfac(lay)*(forref(indf,ig)+                                  &
                forfrac(lay)*(forref(indf+1,ig) - forref(indf,ig))) 
       n2om1 = kb_mn2o(jmn2o,indm,ig)+fmn2o*                                   &
              (kb_mn2o(jmn2o+1,indm,ig)-kb_mn2o(jmn2o,indm,ig))
       n2om2 = kb_mn2o(jmn2o,indm+1,ig)+fmn2o*                                 &
              (kb_mn2o(jmn2o+1,indm+1,ig)-kb_mn2o(jmn2o,indm+1,ig))
       absn2o = n2om1 + minorfrac(lay)*(n2om2 - n2om1)
       taug(lay,ngs2+ig) = speccomb *                                          &
                           (fac000 * absb(ind0,ig) +                           &
                            fac100 * absb(ind0+1,ig) +                         &
                            fac010 * absb(ind0+5,ig) +                         &
                            fac110 * absb(ind0+6,ig))                          &
                           +speccomb1 *                                        &
                            (fac001 * absb(ind1,ig) +                          &
                            fac101 * absb(ind1+1,ig) +                         &
                            fac011 * absb(ind1+5,ig) +                         &
                            fac111 * absb(ind1+6,ig))                          &
                           +taufor                                             &
                           +adjcoln2o*absn2o
       fracs(lay,ngs2+ig) = fracrefb(ig,jpl)+fpl*                              &
                           (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
     enddo
   enddo
!
   end subroutine taugb3
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb4
!-------------------------------------------------------------------------------
!
!  abstract : band 4,  630-700 cm-1 (low key - h2o,co2; high key - o3,co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng4, ngs3
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg04_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
   integer(kind=im) :: js, js1, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor
   real(kind=rb)    :: refrat_planck_a, refrat_planck_b
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! P =   142.5940 mb
!
   refrat_planck_a = chi_mls(1,11)/chi_mls(2,11)
!
! P = 95.58350 mb
!
   refrat_planck_b = chi_mls(3,13)/chi_mls(2,13)
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the water 
! vapor self-continuum and foreign continuum is interpolated (in temperature) 
! separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colh2o(lay)+rat_h2oco2(lay)*colco2(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1+int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay)+rat_h2oco2_1(lay)*colco2(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1.ge.oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1+int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck.ge.oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1+int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(4) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(4) + js1
     inds = indself(lay)
     indf = indfor(lay)
!
     if (specparm.lt.0.125_rb) then
       p = fs-1
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm.gt.0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb-fs)*fac00(lay)
       fac010 = (1._rb-fs)*fac10(lay)
       fac100 = fs*fac00(lay)
       fac110 = fs*fac10(lay)
     endif
!
     if (specparm1.lt.0.125_rb) then
       p = fs1-1
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1.gt.0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1-p-2.0_rb*p4
       fk2 = p+p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb-fs1)*fac01(lay)
       fac011 = (1._rb-fs1)*fac11(lay)
       fac101 = fs1*fac01(lay)
       fac111 = fs1*fac11(lay)
     endif
!
     do ig = 1,ng4
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *               &
                (forref(indf+1,ig) - forref(indf,ig))) 
!
       if (specparm.lt.0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm.gt.0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1.lt.0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1.gt.0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs3+ig) = tau_major+tau_major1                                &
                         + tauself + taufor
       fracs(lay,ngs3+ig) = fracrefa(ig,jpl)+fpl*                              &
                           (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     speccomb = colo3(lay)+rat_o3co2(lay)*colco2(lay)
     specparm = colo3(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 4._rb*(specparm)
     js = 1+int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colo3(lay)+rat_o3co2_1(lay)*colco2(lay)
     specparm1 = colo3(lay)/speccomb1
     if (specparm1.ge.oneminus) specparm1 = oneminus
     specmult1 = 4._rb*(specparm1)
     js1 = 1+int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     fac000 = (1._rb-fs)*fac00(lay)
     fac010 = (1._rb-fs)*fac10(lay)
     fac100 = fs*fac00(lay)
     fac110 = fs*fac10(lay)
     fac001 = (1._rb-fs1)*fac01(lay)
     fac011 = (1._rb-fs1)*fac11(lay)
     fac101 = fs1*fac01(lay)
     fac111 = fs1*fac11(lay)
!
     speccomb_planck = colo3(lay)+refrat_planck_b*colco2(lay)
     specparm_planck = colo3(lay)/speccomb_planck
     if (specparm_planck.ge.oneminus) specparm_planck=oneminus
     specmult_planck = 4._rb*specparm_planck
     jpl = 1+int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(4) + js
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(4) + js1
!
     do ig = 1,ng4
       taug(lay,ngs3+ig) = speccomb *                                          &
                           (fac000 * absb(ind0,ig) +                           &
                            fac100 * absb(ind0+1,ig) +                         &
                            fac010 * absb(ind0+5,ig) +                         &
                            fac110 * absb(ind0+6,ig))                          &
                           +speccomb1 *                                        &
                           (fac001 * absb(ind1,ig ) +                          &
                            fac101 * absb(ind1+1,ig) +                         &
                            fac011 * absb(ind1+5,ig) +                         &
                            fac111 * absb(ind1+6,ig))
       fracs(lay,ngs3+ig) = fracrefb(ig,jpl) + fpl *                           &
                           (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
     enddo
!
! Empirical modification to code to improve stratospheric cooling rates
! for co2.  Revised to apply weighting for g-point reduction in this band.
!
     taug(lay,ngs3+8)  = taug(lay,ngs3+8)*0.92
     taug(lay,ngs3+9)  = taug(lay,ngs3+9)*0.88
     taug(lay,ngs3+10) = taug(lay,ngs3+10)*1.07
     taug(lay,ngs3+11) = taug(lay,ngs3+11)*1.1
     taug(lay,ngs3+12) = taug(lay,ngs3+12)*0.99
     taug(lay,ngs3+13) = taug(lay,ngs3+13)*0.88
     taug(lay,ngs3+14) = taug(lay,ngs3+14)*0.943
!
   enddo
!
   end subroutine taugb4
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb5
!-------------------------------------------------------------------------------
!
!  abstract : band 5,  700-820 cm-1 (low key - h2o,co2; low minor - o3, ccl4)
!                                   (high key - o3,co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng5, ngs4
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg05_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         ka_mo3, selfref, forref, ccl4
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   integer(kind=im) :: js, js1, jmo3, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_mo3, specparm_mo3, specmult_mo3, fmo3
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor, o3m1, o3m2, abso3
   real(kind=rb)    :: refrat_planck_a, refrat_planck_b, refrat_m_a
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Minor gas mapping level :
!     lower - o3, p = 317.34 mbar, t = 240.77 k
!     lower - ccl4
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.
!
! P = 473.420 mb
!
   refrat_planck_a = chi_mls(1,5)/chi_mls(2,5)
!
! P = 0.2369 mb
!
   refrat_planck_b = chi_mls(3,43)/chi_mls(2,43)
!
! P = 317.3480
!
   refrat_m_a = chi_mls(1,7)/chi_mls(2,7)
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the 
! water vapor self-continuum and foreign continuum is 
! interpolated (in temperature) separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colh2o(lay)+rat_h2oco2(lay)*colco2(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1+int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1 .ge. oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_mo3 = colh2o(lay) + refrat_m_a*colco2(lay)
     specparm_mo3 = colh2o(lay)/speccomb_mo3
     if (specparm_mo3.ge.oneminus) specparm_mo3 = oneminus
     specmult_mo3 = 8._rb*specparm_mo3
     jmo3 = 1 + int(specmult_mo3)
     fmo3 = mod(specmult_mo3,1.0_rb)
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck.ge.oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(5) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(5) + js1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     if (specparm.lt.0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm.gt.0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm1.lt.0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1.gt.0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1,ng5
       tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *            &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *               &
                (forref(indf+1,ig) - forref(indf,ig))) 
       o3m1 = ka_mo3(jmo3,indm,ig) + fmo3 *                                    &
             (ka_mo3(jmo3+1,indm,ig)-ka_mo3(jmo3,indm,ig))
       o3m2 = ka_mo3(jmo3,indm+1,ig) + fmo3 *                                  &
             (ka_mo3(jmo3+1,indm+1,ig)-ka_mo3(jmo3,indm+1,ig))
       abso3 = o3m1 + minorfrac(lay)*(o3m2-o3m1)
!
       if (specparm.lt.0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm.gt.0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1.lt.0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1.gt.0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs4+ig) = tau_major + tau_major1                              &
                         + tauself + taufor                                    &
                         + abso3*colo3(lay)                                    &
                         + wx(1,lay) * ccl4(ig)
       fracs(lay,ngs4+ig) = fracrefa(ig,jpl) + fpl *                           &
                           (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     speccomb = colo3(lay) + rat_o3co2(lay)*colco2(lay)
     specparm = colo3(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 4._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colo3(lay) + rat_o3co2_1(lay)*colco2(lay)
     specparm1 = colo3(lay)/speccomb1
     if (specparm1.ge.oneminus) specparm1 = oneminus
     specmult1 = 4._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs1) * fac01(lay)
     fac011 = (1._rb - fs1) * fac11(lay)
     fac101 = fs1 * fac01(lay)
     fac111 = fs1 * fac11(lay)
!
     speccomb_planck = colo3(lay)+refrat_planck_b*colco2(lay)
     specparm_planck = colo3(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 4._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(5) + js
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(5) + js1
!         
     do ig = 1,ng5
       taug(lay,ngs4+ig) = speccomb *                                          &
                           (fac000 * absb(ind0,ig) +                           &
                            fac100 * absb(ind0+1,ig) +                         &
                            fac010 * absb(ind0+5,ig) +                         &
                            fac110 * absb(ind0+6,ig))                          &
                           +speccomb1 *                                        &
                           (fac001 * absb(ind1,ig) +                           &
                            fac101 * absb(ind1+1,ig) +                         &
                            fac011 * absb(ind1+5,ig) +                         &
                            fac111 * absb(ind1+6,ig))                          &
                           +wx(1,lay) * ccl4(ig)
       fracs(lay,ngs4+ig) = fracrefb(ig,jpl) + fpl *                           &
                           (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
     enddo
   enddo
!
   end subroutine taugb5
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb6
!-------------------------------------------------------------------------------
!
!  abstract : band 6, 820-980 cm-1 (low key - h2o; low minor - co2)
!                                  (high key - nothing; high minor-cfc11, cfc12)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ngs5
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg06_k
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   real(kind=rb)    :: chi_co2, ratco2, adjfac, adjcolco2
   real(kind=rb)    :: tauself, taufor, absco2
!-------------------------------------------------------------------------------
!
! Minor gas mapping level:
!     lower - co2, p = 706.2720 mb, t = 294.2 k
!     upper - cfc11, cfc12
!
! Compute the optical depth by interpolating in ln(pressure) and
! temperature. The water vapor self-continuum and foreign continuum
! is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
! In atmospheres where the amount of CO2 is too great to be considered
! a minor species, adjust the column amount of CO2 by an empirical factor 
! to obtain the proper contribution.
!
     chi_co2 = colco2(lay)/(coldry(lay))
     ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
!
     if (ratco2.gt.3.0_rb) then
       adjfac = 2.0_rb+(ratco2-2.0_rb)**0.77_rb
       adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcolco2 = colco2(lay)
     endif
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(6) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(6) + 1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     do ig = 1,ng6
       tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *            &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *               &
                (forref(indf+1,ig) - forref(indf,ig)))
       absco2 =  (ka_mco2(indm,ig) + minorfrac(lay) *                          &
                 (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
       taug(lay,ngs5+ig) = colh2o(lay) *                                       &
                           (fac00(lay) * absa(ind0,ig) +                       &
                            fac10(lay) * absa(ind0+1,ig) +                     &
                            fac01(lay) * absa(ind1,ig) +                       &
                            fac11(lay) * absa(ind1+1,ig))                      &
                           +tauself + taufor                                   &
                           +adjcolco2 * absco2                                 &
                           +wx(2,lay) * cfc11adj(ig)                           &
                           +wx(3,lay) * cfc12(ig)
       fracs(lay,ngs5+ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
! Nothing important goes on above laytrop in this band.
!
   do lay = laytrop+1,nlayers
     do ig = 1,ng6
       taug(lay,ngs5+ig) = 0.0_rb                                              &
                         + wx(2,lay) * cfc11adj(ig)                            &
                         + wx(3,lay) * cfc12(ig)
       fracs(lay,ngs5+ig) = fracrefa(ig)
     enddo
    enddo
!
   end subroutine taugb6
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb7
!-------------------------------------------------------------------------------
!
!  abstract : band 7,  980-1080 cm-1 (low key - h2o,o3; low minor - co2)
!                                    (high key - o3; high minor - co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng7, ngs6
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg07_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         ka_mco2, kb_mco2, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   integer(kind=im) :: js, js1, jmco2, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor, co2m1, co2m2, absco2
   real(kind=rb)    :: chi_co2, ratco2, adjfac, adjcolco2
   real(kind=rb)    :: refrat_planck_a, refrat_m_a
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Minor gas mapping level :
!     lower - co2, p = 706.2620 mbar, t= 278.94 k
!     upper - co2, p = 12.9350 mbar, t = 234.01 k
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower atmosphere.
!
! P = 706.2620 mb
!
   refrat_planck_a = chi_mls(1,3)/chi_mls(3,3)
!
! P = 706.2720 mb
!
   refrat_m_a = chi_mls(1,3)/chi_mls(3,3)
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately. 
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colh2o(lay) + rat_h2oo3(lay)*colo3(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm.ge.oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay) + rat_h2oo3_1(lay)*colo3(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1.ge.oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_mco2 = colh2o(lay) + refrat_m_a*colo3(lay)
     specparm_mco2 = colh2o(lay)/speccomb_mco2
     if (specparm_mco2.ge.oneminus) specparm_mco2 = oneminus
     specmult_mco2 = 8._rb*specparm_mco2
!
     jmco2 = 1+int(specmult_mco2)
     fmco2 = mod(specmult_mco2,1.0_rb)
!
!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
!
     chi_co2 = colco2(lay)/(coldry(lay))
     ratco2 = 1.e20*chi_co2/chi_mls(2,jp(lay)+1)
     if (ratco2.gt.3.0_rb) then
       adjfac = 3.0_rb+(ratco2-3.0_rb)**0.79_rb
       adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcolco2 = colco2(lay)
     endif
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colo3(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck.ge.oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(7) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(7) + js1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     if (specparm .lt. 0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm.gt.0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm.lt.0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1.gt.0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1,ng7
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig))) 
       co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 *                                &
              (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
       co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 *                              &
              (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
       absco2 = co2m1 + minorfrac(lay) * (co2m2 - co2m1)
!
       if (specparm .lt. 0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm.gt.0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1.lt.0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1.gt.0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs6+ig) = tau_major + tau_major1                              &
                         + tauself + taufor                                    &
                         + adjcolco2*absco2
       fracs(lay,ngs6+ig) = fracrefa(ig,jpl) + fpl *                           &
                           (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
!
!  In atmospheres where the amount of CO2 is too great to be considered
!  a minor species, adjust the column amount of CO2 by an empirical factor 
!  to obtain the proper contribution.
!
     chi_co2 = colco2(lay)/(coldry(lay))
     ratco2 = 1.e20*chi_co2/chi_mls(2,jp(lay)+1)
     if (ratco2 .gt. 3.0_rb) then
       adjfac = 2.0_rb+(ratco2-2.0_rb)**0.79_rb
       adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcolco2 = colco2(lay)
     endif
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(7) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(7) + 1
     indm = indminor(lay)
!
     do ig = 1,ng7
       absco2 = kb_mco2(indm,ig) + minorfrac(lay) *                            &
               (kb_mco2(indm+1,ig) - kb_mco2(indm,ig))
       taug(lay,ngs6+ig) = colo3(lay) *                                        &
                           (fac00(lay) * absb(ind0,ig) +                       &
                            fac10(lay) * absb(ind0+1,ig) +                     &
                            fac01(lay) * absb(ind1,ig) +                       &
                            fac11(lay) * absb(ind1+1,ig))                      &
                           +adjcolco2 * absco2
       fracs(lay,ngs6+ig) = fracrefb(ig)
     enddo
!
! Empirical modification to code to improve stratospheric cooling rates
! for o3.  Revised to apply weighting for g-point reduction in this band.
!
     taug(lay,ngs6+6)  = taug(lay,ngs6+6)*0.92_rb
     taug(lay,ngs6+7)  = taug(lay,ngs6+7)*0.88_rb
     taug(lay,ngs6+8)  = taug(lay,ngs6+8)*1.07_rb
     taug(lay,ngs6+9)  = taug(lay,ngs6+9)*1.1_rb
     taug(lay,ngs6+10) = taug(lay,ngs6+10)*0.99_rb
     taug(lay,ngs6+11) = taug(lay,ngs6+11)*0.855_rb
!
   enddo
!
   end subroutine taugb7
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb8
!-------------------------------------------------------------------------------
!
!  abstract : band 8,  1080-1180 cm-1 (low key - h2o; low minor - co2,o3,n2o)
!                                     (high key - o3; high minor - co2, n2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng8, ngs7
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg08_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         ka_mco2, ka_mn2o, ka_mo3, kb_mco2, kb_mn2o,           &
                         selfref, forref, cfc12, cfc22adj
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   real(kind=rb)    :: tauself, taufor, absco2, abso3, absn2o
   real(kind=rb)    :: chi_co2, ratco2, adjfac, adjcolco2
!-------------------------------------------------------------------------------
!
! Minor gas mapping level:
!     lower - co2, p = 1053.63 mb, t = 294.2 k
!     lower - o3,  p = 317.348 mb, t = 240.77 k
!     lower - n2o, p = 706.2720 mb, t= 278.94 k
!     lower - cfc12,cfc11
!     upper - co2, p = 35.1632 mb, t = 223.28 k
!     upper - n2o, p = 8.716e-2 mb, t = 226.03 k
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature, and appropriate species.  Below laytrop, the water vapor 
! self-continuum and foreign continuum is interpolated (in temperature) 
! separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
! In atmospheres where the amount of CO2 is too great to be considered
! a minor species, adjust the column amount of CO2 by an empirical factor 
! to obtain the proper contribution.
!
     chi_co2 = colco2(lay)/(coldry(lay))
     ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
     if (ratco2 .gt. 3.0_rb) then
       adjfac = 2.0_rb+(ratco2-2.0_rb)**0.65_rb
       adjcolco2 = adjfac*chi_mls(2,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcolco2 = colco2(lay)
     endif
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(8) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(8) + 1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     do ig = 1, ng8
       tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *            &
                                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
                              (forref(indf+1,ig) - forref(indf,ig)))
       absco2 = (ka_mco2(indm,ig) + minorfrac(lay) *                           &
                (ka_mco2(indm+1,ig) - ka_mco2(indm,ig)))
       abso3 = (ka_mo3(indm,ig) + minorfrac(lay) *                             &
               (ka_mo3(indm+1,ig) - ka_mo3(indm,ig)))
       absn2o = (ka_mn2o(indm,ig) + minorfrac(lay) *                           &
                (ka_mn2o(indm+1,ig) - ka_mn2o(indm,ig)))
       taug(lay,ngs7+ig) = colh2o(lay) *                                       &
                           (fac00(lay) * absa(ind0,ig) +                       &
                            fac10(lay) * absa(ind0+1,ig) +                     &
                            fac01(lay) * absa(ind1,ig) +                       &
                            fac11(lay) * absa(ind1+1,ig))                      &
                            + tauself + taufor                                 &
                            + adjcolco2 * absco2                               &
                            + colo3(lay) * abso3                               &
                            + coln2o(lay) * absn2o                             &
                            + wx(3,lay) * cfc12(ig)                            &
                            + wx(4,lay) * cfc22adj(ig)
       fracs(lay,ngs7+ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1, nlayers
!
! In atmospheres where the amount of CO2 is too great to be considered
! a minor species, adjust the column amount of CO2 by an empirical factor 
! to obtain the proper contribution.
!
     chi_co2 = colco2(lay)/coldry(lay)
     ratco2 = 1.e20_rb*chi_co2/chi_mls(2,jp(lay)+1)
     if (ratco2 .gt. 3.0_rb) then
       adjfac = 2.0_rb+(ratco2-2.0_rb)**0.65_rb
       adjcolco2 = adjfac*chi_mls(2,jp(lay)+1) * coldry(lay)*1.e-20_rb
     else
       adjcolco2 = colco2(lay)
     endif
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(8) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(8) + 1
     indm = indminor(lay)
!
     do ig = 1, ng8
       absco2 = (kb_mco2(indm,ig) + minorfrac(lay) *                           &
                (kb_mco2(indm+1,ig) - kb_mco2(indm,ig)))
       absn2o = (kb_mn2o(indm,ig) + minorfrac(lay) *                           &
                (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig)))
       taug(lay,ngs7+ig) = colo3(lay)  *                                       &
                           (fac00(lay) * absb(ind0,ig) +                       &
                            fac10(lay) * absb(ind0+1,ig) +                     &
                            fac01(lay) * absb(ind1,ig) +                       &
                            fac11(lay) * absb(ind1+1,ig))                      &
                            + adjcolco2 * absco2                               &
                            + coln2o(lay)* absn2o                              &
                            + wx(3,lay) * cfc12(ig)                            &
                            + wx(4,lay) * cfc22adj(ig)
       fracs(lay,ngs7+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb8
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb9
!-------------------------------------------------------------------------------
!
!  abstract : band 9,  1180-1390 cm-1 (low key - h2o,ch4; low minor - n2o)
!                                     (high key - ch4; high minor - n2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng9, ngs8
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg09_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         ka_mn2o, kb_mn2o, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   integer(kind=im) :: js, js1, jmn2o, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, fmn2o
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor, n2om1, n2om2, absn2o
   real(kind=rb)    :: chi_n2o, ratn2o, adjfac, adjcoln2o
   real(kind=rb)    :: refrat_planck_a, refrat_m_a
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Minor gas mapping level :
!     lower - n2o, p = 706.272 mbar, t = 278.94 k
!     upper - n2o, p = 95.58 mbar, t = 215.7 k
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.
!
! P = 212 mb
!
   refrat_planck_a = chi_mls(1,9)/chi_mls(6,9)
!
! P = 706.272 mb 
!
   refrat_m_a = chi_mls(1,3)/chi_mls(6,3)
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
     speccomb = colh2o(lay) + rat_h2och4(lay)*colch4(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay) + rat_h2och4_1(lay)*colch4(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1 .ge. oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_mn2o = colh2o(lay) + refrat_m_a*colch4(lay)
     specparm_mn2o = colh2o(lay)/speccomb_mn2o
     if (specparm_mn2o .ge. oneminus) specparm_mn2o = oneminus
     specmult_mn2o = 8._rb*specparm_mn2o
     jmn2o = 1 + int(specmult_mn2o)
     fmn2o = mod(specmult_mn2o,1.0_rb)
!
!  In atmospheres where the amount of N2O is too great to be considered
!  a minor species, adjust the column amount of N2O by an empirical factor 
!  to obtain the proper contribution.
!
     chi_n2o = coln2o(lay)/(coldry(lay))
     ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
     if (ratn2o .gt. 1.5_rb) then
       adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
       adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcoln2o = coln2o(lay)
     endif
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colch4(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(9) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(9) + js1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     if (specparm .lt. 0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm .gt. 0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm1 .lt. 0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1 .gt. 0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1, ng9
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                               (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
                              (forref(indf+1,ig) - forref(indf,ig))) 
       n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o *                                &
              (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
       n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o *                              &
              (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
       absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)
!
       if (specparm .lt. 0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm .gt. 0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1 .lt. 0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1 .gt. 0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
       taug(lay,ngs8+ig) = tau_major + tau_major1                              &
                         + tauself + taufor                                    &
                         + adjcoln2o*absn2o
       fracs(lay,ngs8+ig) = fracrefa(ig,jpl) + fpl *                           &
                           (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
!
! In atmospheres where the amount of N2O is too great to be considered
! a minor species, adjust the column amount of N2O by an empirical factor 
! to obtain the proper contribution.
!
     chi_n2o = coln2o(lay)/(coldry(lay))
     ratn2o = 1.e20_rb*chi_n2o/chi_mls(4,jp(lay)+1)
     if (ratn2o .gt. 1.5_rb) then
       adjfac = 0.5_rb+(ratn2o-0.5_rb)**0.65_rb
       adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_rb
     else
       adjcoln2o = coln2o(lay)
     endif
!
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(9) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(9) + 1
     indm = indminor(lay)
!
     do ig = 1,ng9
       absn2o = kb_mn2o(indm,ig) + minorfrac(lay) *                            &
               (kb_mn2o(indm+1,ig) - kb_mn2o(indm,ig))
       taug(lay,ngs8+ig) = colch4(lay) *                                       &
                           (fac00(lay) * absb(ind0,ig) +                       &
                            fac10(lay) * absb(ind0+1,ig) +                     &
                            fac01(lay) * absb(ind1,ig) +                       &
                            fac11(lay) * absb(ind1+1,ig))                      &
                            + adjcoln2o*absn2o
       fracs(lay,ngs8+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb9
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb10
!-------------------------------------------------------------------------------
!
!  abstract : band 10,  1390-1480 cm-1 (low key - h2o; high key - h2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng10, ngs9
   use rrlw_kg10_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
   real(kind=rb)    :: tauself, taufor
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(10) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(10) + 1
     inds = indself(lay)
     indf = indfor(lay)
!
     do ig = 1,ng10
       tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *            &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig))) 
       taug(lay,ngs9+ig) = colh2o(lay) *                                       &
                           (fac00(lay) * absa(ind0,ig) +                       &
                            fac10(lay) * absa(ind0+1,ig) +                     &
                            fac01(lay) * absa(ind1,ig) +                       &
                            fac11(lay) * absa(ind1+1,ig))                      &
                            + tauself + taufor
       fracs(lay,ngs9+ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(10) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(10) + 1
     indf = indfor(lay)
!
     do ig = 1,ng10
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig))) 
       taug(lay,ngs9+ig) = colh2o(lay) *                                       &
                           (fac00(lay) * absb(ind0,ig) +                       &
                           fac10(lay) * absb(ind0+1,ig) +                      &
                           fac01(lay) * absb(ind1,ig) +                        &
                           fac11(lay) * absb(ind1+1,ig))                       &
                           + taufor
       fracs(lay,ngs9+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb10
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb11
!-------------------------------------------------------------------------------
!
!  abstract : band 11,  1480-1800 cm-1 (low - h2o; low minor - o2)
!                                      (high key - h2o; high minor - o2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng11, ngs10
   use rrlw_kg11_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                            ka_mo2, kb_mo2, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   real(kind=rb)    :: scaleo2, tauself, taufor, tauo2
!-------------------------------------------------------------------------------
!
! Minor gas mapping level :
!     lower - o2, p = 706.2720 mbar, t = 278.94 k
!     upper - o2, p = 4.758820 mbarm t = 250.85 k
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum and
! foreign continuum is interpolated (in temperature) separately.
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(11) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(11) + 1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
     scaleo2 = colo2(lay)*scaleminor(lay)
     do ig = 1,ng11
       tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *            &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig)))
       tauo2 =  scaleo2 * (ka_mo2(indm,ig) + minorfrac(lay) *                  &
               (ka_mo2(indm+1,ig) - ka_mo2(indm,ig)))
       taug(lay,ngs10+ig) = colh2o(lay) *                                      &
                            (fac00(lay) * absa(ind0,ig) +                      &
                             fac10(lay) * absa(ind0+1,ig) +                    &
                             fac01(lay) * absa(ind1,ig) +                      &
                             fac11(lay) * absa(ind1+1,ig))                     &
                             + tauself + taufor                                &
                             + tauo2
       fracs(lay,ngs10+ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(11) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(11) + 1
     indf = indfor(lay)
     indm = indminor(lay)
     scaleo2 = colo2(lay)*scaleminor(lay)
     do ig = 1,ng11
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig))) 
       tauo2 =  scaleo2 * (kb_mo2(indm,ig) + minorfrac(lay) *                  &
               (kb_mo2(indm+1,ig) - kb_mo2(indm,ig)))
       taug(lay,ngs10+ig) = colh2o(lay) *                                      &
                            (fac00(lay) * absb(ind0,ig) +                      &
                             fac10(lay) * absb(ind0+1,ig) +                    &
                             fac01(lay) * absb(ind1,ig) +                      &
                             fac11(lay) * absb(ind1+1,ig))                     &
                             + taufor                                          &
                             + tauo2
       fracs(lay,ngs10+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb11
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb12
!-------------------------------------------------------------------------------
!
!  abstract : band 12,  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng12, ngs11
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg12_k, only : fracrefa, absa, ka,                                 &
                         selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
   integer(kind=im) :: js, js1, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor
   real(kind=rb)    :: refrat_planck_a
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.
!
! P =   174.164 mb 
!
   refrat_planck_a = chi_mls(1,10)/chi_mls(2,10)
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum adn foreign continuum is interpolated 
! (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
     speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1 .ge. oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(12) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(12) + js1
     inds = indself(lay)
     indf = indfor(lay)
!
     if (specparm .lt. 0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm .gt. 0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm1 .lt. 0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1 .gt. 0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1,ng12
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig))) 
!
       if (specparm .lt. 0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm .gt. 0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1 .lt. 0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1 .gt. 0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs11+ig) = tau_major + tau_major1                             &
                          + tauself + taufor
       fracs(lay,ngs11+ig) = fracrefa(ig,jpl) + fpl *                          &
                            (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
      do lay = laytrop+1, nlayers
!
         do ig = 1, ng12
            taug(lay,ngs11+ig) = 0.0_rb
            fracs(lay,ngs11+ig) = 0.0_rb
         enddo
      enddo
!
   end subroutine taugb12
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb13
!-------------------------------------------------------------------------------
!
!  abstract : band 13, 2080-2250 cm-1 (low key - h2o,n2o; high minor - o3 minor)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng13, ngs12
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg13_k, only : fracrefa, fracrefb, absa, ka,                       &
                         ka_mco2, ka_mco, kb_mo3, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   integer(kind=im) :: js, js1, jmco2, jmco, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_mco2, specparm_mco2, specmult_mco2, fmco2
   real(kind=rb)    :: speccomb_mco, specparm_mco, specmult_mco, fmco
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor, co2m1, co2m2, absco2 
   real(kind=rb)    :: com1, com2, absco, abso3
   real(kind=rb)    :: chi_co2, ratco2, adjfac, adjcolco2
   real(kind=rb)    :: refrat_planck_a, refrat_m_a, refrat_m_a3
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Minor gas mapping levels :
!     lower - co2, p = 1053.63 mb, t = 294.2 k
!     lower - co, p = 706 mb, t = 278.94 k
!     upper - o3, p = 95.5835 mb, t = 215.7 k
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower/upper atmosphere.
!
! P = 473.420 mb (Level 5)
!
   refrat_planck_a = chi_mls(1,5)/chi_mls(4,5)
!
! P = 1053. (Level 1)
!
   refrat_m_a = chi_mls(1,1)/chi_mls(4,1)
!
! P = 706. (Level 3)
!
   refrat_m_a3 = chi_mls(1,3)/chi_mls(4,3)
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
     speccomb = colh2o(lay) + rat_h2on2o(lay)*coln2o(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay) + rat_h2on2o_1(lay)*coln2o(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1 .ge. oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_mco2 = colh2o(lay) + refrat_m_a*coln2o(lay)
     specparm_mco2 = colh2o(lay)/speccomb_mco2
     if (specparm_mco2 .ge. oneminus) specparm_mco2 = oneminus
     specmult_mco2 = 8._rb*specparm_mco2
     jmco2 = 1 + int(specmult_mco2)
     fmco2 = mod(specmult_mco2,1.0_rb)
!
! In atmospheres where the amount of CO2 is too great to be considered
! a minor species, adjust the column amount of CO2 by an empirical factor 
! to obtain the proper contribution.
!
     chi_co2 = colco2(lay)/(coldry(lay))
     ratco2 = 1.e20_rb*chi_co2/3.55e-4_rb
     if (ratco2 .gt. 3.0_rb) then
       adjfac = 2.0_rb+(ratco2-2.0_rb)**0.68_rb
       adjcolco2 = adjfac*3.55e-4*coldry(lay)*1.e-20_rb
     else
       adjcolco2 = colco2(lay)
     endif
!
     speccomb_mco = colh2o(lay) + refrat_m_a3*coln2o(lay)
     specparm_mco = colh2o(lay)/speccomb_mco
     if (specparm_mco .ge. oneminus) specparm_mco = oneminus
     specmult_mco = 8._rb*specparm_mco
     jmco = 1 + int(specmult_mco)
     fmco = mod(specmult_mco,1.0_rb)
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*coln2o(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(13) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(13) + js1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!
     if (specparm .lt. 0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm .gt. 0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm1 .lt. 0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1 .gt. 0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1, ng13
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) *                &
               (forref(indf+1,ig) - forref(indf,ig))) 
       co2m1 = ka_mco2(jmco2,indm,ig) + fmco2 *                                &
              (ka_mco2(jmco2+1,indm,ig) - ka_mco2(jmco2,indm,ig))
       co2m2 = ka_mco2(jmco2,indm+1,ig) + fmco2 *                              &
              (ka_mco2(jmco2+1,indm+1,ig) - ka_mco2(jmco2,indm+1,ig))
       absco2 = co2m1 + minorfrac(lay) * (co2m2 - co2m1)
       com1 = ka_mco(jmco,indm,ig) + fmco *                                    &
             (ka_mco(jmco+1,indm,ig) - ka_mco(jmco,indm,ig))
       com2 = ka_mco(jmco,indm+1,ig) + fmco *                                  &
             (ka_mco(jmco+1,indm+1,ig) - ka_mco(jmco,indm+1,ig))
       absco = com1 + minorfrac(lay) * (com2 - com1)
!
       if (specparm .lt. 0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm .gt. 0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1 .lt. 0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1 .gt. 0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs12+ig) = tau_major + tau_major1                             &
                          + tauself + taufor                                   &
                          + adjcolco2*absco2                                   &
                          + colco(lay)*absco
       fracs(lay,ngs12+ig) = fracrefa(ig,jpl) + fpl *                          &
                            (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     indm = indminor(lay)
     do ig = 1,ng13
       abso3 = kb_mo3(indm,ig) + minorfrac(lay) *                              &
              (kb_mo3(indm+1,ig) - kb_mo3(indm,ig))
       taug(lay,ngs12+ig) = colo3(lay)*abso3
       fracs(lay,ngs12+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb13
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb14
!-------------------------------------------------------------------------------
!
!  abstract : band 14,  2250-2380 cm-1 (low - co2; high - co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng14, ngs13
   use rrlw_kg14_k, only : fracrefa, fracrefb, absa, ka, absb, kb,             &
                         selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
   real(kind=rb)    :: tauself, taufor
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure) and 
! temperature.  Below laytrop, the water vapor self-continuum 
! and foreign continuum is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(14) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(14) + 1
     inds = indself(lay)
     indf = indfor(lay)
     do ig = 1,ng14
       tauself = selffac(lay) * (selfref(inds,ig) + selffrac(lay) *            &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor =  forfac(lay) * (forref(indf,ig) + forfrac(lay) *               &
                (forref(indf+1,ig) - forref(indf,ig))) 
       taug(lay,ngs13+ig) = colco2(lay) *                                      &
                            (fac00(lay) * absa(ind0,ig) +                      &
                             fac10(lay) * absa(ind0+1,ig) +                    &
                             fac01(lay) * absa(ind1,ig) +                      &
                             fac11(lay) * absa(ind1+1,ig))                     &
                             + tauself + taufor
       fracs(lay,ngs13+ig) = fracrefa(ig)
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(14) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(14) + 1
     do ig = 1,ng14
       taug(lay,ngs13+ig) = colco2(lay) *                                      &
                            (fac00(lay) * absb(ind0,ig) +                      &
                             fac10(lay) * absb(ind0+1,ig) +                    &
                             fac01(lay) * absb(ind1,ig) +                      &
                             fac11(lay) * absb(ind1+1,ig))
       fracs(lay,ngs13+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb14
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb15
!-------------------------------------------------------------------------------
!
!  abstract : band 15,  2380-2600 cm-1 (low - n2o,co2; low minor - n2)
!                                      (high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng15, ngs14
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg15_k, only : fracrefa, absa, ka, ka_mn2, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, indm, ig
   integer(kind=im) :: js, js1, jmn2, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_mn2, specparm_mn2, specmult_mn2, fmn2
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: scalen2, tauself, taufor, n2m1, n2m2, taun2 
   real(kind=rb)    :: refrat_planck_a, refrat_m_a
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Minor gas mapping level : 
!     Lower - Nitrogen Continuum, P = 1053., T = 294.
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower atmosphere.
!
! P = 1053. mb (Level 1)
!
   refrat_planck_a = chi_mls(4,1)/chi_mls(2,1)
!
! P = 1053.
!
   refrat_m_a = chi_mls(4,1)/chi_mls(2,1)
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
     speccomb = coln2o(lay) + rat_n2oco2(lay)*colco2(lay)
     specparm = coln2o(lay)/speccomb
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = coln2o(lay) + rat_n2oco2_1(lay)*colco2(lay)
     specparm1 = coln2o(lay)/speccomb1
     if (specparm1 .ge. oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_mn2 = coln2o(lay) + refrat_m_a*colco2(lay)
     specparm_mn2 = coln2o(lay)/speccomb_mn2
     if (specparm_mn2 .ge. oneminus) specparm_mn2 = oneminus
     specmult_mn2 = 8._rb*specparm_mn2
     jmn2 = 1 + int(specmult_mn2)
     fmn2 = mod(specmult_mn2,1.0_rb)
!
     speccomb_planck = coln2o(lay) + refrat_planck_a*colco2(lay)
     specparm_planck = coln2o(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(15) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(15) + js1
     inds = indself(lay)
     indf = indfor(lay)
     indm = indminor(lay)
!         
     scalen2 = colbrd(lay)*scaleminor(lay)
!
     if (specparm .lt. 0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm .gt. 0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm1 .lt. 0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1 .gt. 0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1,ng15
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                               (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor  = forfac(lay) * (forref(indf,ig) + forfrac(lay) *               &
                               (forref(indf+1,ig) - forref(indf,ig))) 
       n2m1 = ka_mn2(jmn2,indm,ig) + fmn2 *                                    &
             (ka_mn2(jmn2+1,indm,ig) - ka_mn2(jmn2,indm,ig))
       n2m2 = ka_mn2(jmn2,indm+1,ig) + fmn2 *                                  &
             (ka_mn2(jmn2+1,indm+1,ig) - ka_mn2(jmn2,indm+1,ig))
       taun2 = scalen2 * (n2m1 + minorfrac(lay) * (n2m2 - n2m1))
!
       if (specparm .lt. 0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm .gt. 0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif 
!
       if (specparm1 .lt. 0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1 .gt. 0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs14+ig) = tau_major + tau_major1                             &
                          + tauself + taufor                                   &
                          + taun2
       fracs(lay,ngs14+ig) = fracrefa(ig,jpl) + fpl *                          &
                            (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     do ig = 1,ng15
       taug(lay,ngs14+ig)  = 0.0_rb
       fracs(lay,ngs14+ig) = 0.0_rb
     enddo
   enddo
!
   end subroutine taugb15
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taugb16
!-------------------------------------------------------------------------------
!
!  abstract : band 16,  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : ng16, ngs15
   use rrlw_ref_k,  only : chi_mls
   use rrlw_kg16_k, only : fracrefa, fracrefb, absa, ka,                       &
                           absb, kb, selfref, forref
!
! Local 
!
   integer(kind=im) :: lay, ind0, ind1, inds, indf, ig
   integer(kind=im) :: js, js1, jpl
   real(kind=rb)    :: speccomb, specparm, specmult, fs
   real(kind=rb)    :: speccomb1, specparm1, specmult1, fs1
   real(kind=rb)    :: speccomb_planck, specparm_planck, specmult_planck, fpl
   real(kind=rb)    :: p, p4, fk0, fk1, fk2
   real(kind=rb)    :: fac000, fac100, fac200, fac010, fac110, fac210
   real(kind=rb)    :: fac001, fac101, fac201, fac011, fac111, fac211
   real(kind=rb)    :: tauself, taufor
   real(kind=rb)    :: refrat_planck_a
   real(kind=rb)    :: tau_major, tau_major1
!-------------------------------------------------------------------------------
!
! Calculate reference ratio to be used in calculation of Planck
! fraction in lower atmosphere.
! P = 387. mb (Level 6)
!
   refrat_planck_a = chi_mls(1,6)/chi_mls(6,6)
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature,and appropriate species.  Below laytrop, the water
! vapor self-continuum and foreign continuum is interpolated 
! (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
!
     speccomb = colh2o(lay) + rat_h2och4(lay)*colch4(lay)
     specparm = colh2o(lay)/speccomb
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult,1.0_rb)
!
     speccomb1 = colh2o(lay) + rat_h2och4_1(lay)*colch4(lay)
     specparm1 = colh2o(lay)/speccomb1
     if (specparm1 .ge. oneminus) specparm1 = oneminus
     specmult1 = 8._rb*(specparm1)
     js1 = 1 + int(specmult1)
     fs1 = mod(specmult1,1.0_rb)
!
     speccomb_planck = colh2o(lay)+refrat_planck_a*colch4(lay)
     specparm_planck = colh2o(lay)/speccomb_planck
     if (specparm_planck .ge. oneminus) specparm_planck=oneminus
     specmult_planck = 8._rb*specparm_planck
     jpl = 1 + int(specmult_planck)
     fpl = mod(specmult_planck,1.0_rb)
!
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(16) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(16) + js1
     inds = indself(lay)
     indf = indfor(lay)
!
     if (specparm .lt. 0.125_rb) then
       p = fs - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else if (specparm .gt. 0.875_rb) then
       p = -fs 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac000 = fk0*fac00(lay)
       fac100 = fk1*fac00(lay)
       fac200 = fk2*fac00(lay)
       fac010 = fk0*fac10(lay)
       fac110 = fk1*fac10(lay)
       fac210 = fk2*fac10(lay)
     else
       fac000 = (1._rb - fs) * fac00(lay)
       fac010 = (1._rb - fs) * fac10(lay)
       fac100 = fs * fac00(lay)
       fac110 = fs * fac10(lay)
     endif
!
     if (specparm1 .lt. 0.125_rb) then
       p = fs1 - 1
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else if (specparm1 .gt. 0.875_rb) then
       p = -fs1 
       p4 = p**4
       fk0 = p4
       fk1 = 1 - p - 2.0_rb*p4
       fk2 = p + p4
       fac001 = fk0*fac01(lay)
       fac101 = fk1*fac01(lay)
       fac201 = fk2*fac01(lay)
       fac011 = fk0*fac11(lay)
       fac111 = fk1*fac11(lay)
       fac211 = fk2*fac11(lay)
     else
       fac001 = (1._rb - fs1) * fac01(lay)
       fac011 = (1._rb - fs1) * fac11(lay)
       fac101 = fs1 * fac01(lay)
       fac111 = fs1 * fac11(lay)
     endif
!
     do ig = 1,ng16
       tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) *             &
                (selfref(inds+1,ig) - selfref(inds,ig)))
       taufor  = forfac(lay) * (forref(indf,ig) + forfrac(lay) *               &
                (forref(indf+1,ig) - forref(indf,ig))) 
!
       if (specparm .lt. 0.125_rb) then
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac200 * absa(ind0+2,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig) +                              &
                      fac210 * absa(ind0+11,ig))
       else if (specparm .gt. 0.875_rb) then
         tau_major = speccomb *                                                &
                     (fac200 * absa(ind0-1,ig) +                               &
                      fac100 * absa(ind0,ig) +                                 &
                      fac000 * absa(ind0+1,ig) +                               &
                      fac210 * absa(ind0+8,ig) +                               &
                      fac110 * absa(ind0+9,ig) +                               &
                      fac010 * absa(ind0+10,ig))
       else
         tau_major = speccomb *                                                &
                     (fac000 * absa(ind0,ig) +                                 &
                      fac100 * absa(ind0+1,ig) +                               &
                      fac010 * absa(ind0+9,ig) +                               &
                      fac110 * absa(ind0+10,ig))
       endif
!
       if (specparm1 .lt. 0.125_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac201 * absa(ind1+2,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig) +                             &
                       fac211 * absa(ind1+11,ig))
       else if (specparm1 .gt. 0.875_rb) then
         tau_major1 = speccomb1 *                                              &
                      (fac201 * absa(ind1-1,ig) +                              &
                       fac101 * absa(ind1,ig) +                                &
                       fac001 * absa(ind1+1,ig) +                              &
                       fac211 * absa(ind1+8,ig) +                              &
                       fac111 * absa(ind1+9,ig) +                              &
                       fac011 * absa(ind1+10,ig))
       else
         tau_major1 = speccomb1 *                                              &
                      (fac001 * absa(ind1,ig) +                                &
                       fac101 * absa(ind1+1,ig) +                              &
                       fac011 * absa(ind1+9,ig) +                              &
                       fac111 * absa(ind1+10,ig))
       endif
!
       taug(lay,ngs15+ig) = tau_major + tau_major1                             &
                          + tauself + taufor
       fracs(lay,ngs15+ig) = fracrefa(ig,jpl) + fpl *                          &
                            (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(16) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(16) + 1
     do ig = 1,ng16
       taug(lay,ngs15+ig) = colch4(lay) *                                      &
                            (fac00(lay) * absb(ind0,ig) +                      &
                             fac10(lay) * absb(ind0+1,ig) +                    &
                             fac01(lay) * absb(ind1,ig) +                      &
                             fac11(lay) * absb(ind1+1,ig))
       fracs(lay,ngs15+ig) = fracrefb(ig)
     enddo
   enddo
!
   end subroutine taugb16
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end subroutine taumol
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_lw_taumol_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!
! path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
! author:    $Author: trn $
! revision:  $Revision: 1.3 $
! created:   $Date: 2009/04/16 19:54:22 $
!
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_lw_init_k
!-------------------------------------------------------------------------------
!
!  abstract : rrtmg_lw_init (Steven Cavallo: added for buffer layer adjustment)
!
!  --------------------------------------------------------------------------
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
!  --------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   use parkind_k,          only : im => kind_im, rb => kind_rb
   use rrlw_wvn_k
   use rrtmg_lw_setcoef_k, only : lwatmref, lwavplank
!
   implicit none
!
   integer, save    :: nlayers 
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rrtmg_lw_ini (cpdair)
!-------------------------------------------------------------------------------
!
!  abstract : 
!  This subroutine performs calculations necessary for the initialization
!  of the longwave model.  Lookup tables are computed for use in the LW
!  radiative transfer, and input absorption coefficient data for each
!  spectral band are reduced from 256 g-point intervals to 140.
!
!  history log :
!    1998-07-01  Michael J. Iacono  original version
!    1998-09-01                     first revision for GCMs
!    2002-09-01                     second revision for RRTM_V3.0
!
!  input :
!    cpdair - Specific heat capacity of dry air at constant pressure at 273 K
!             (J kg-1 K-1)
!
!  local variable :
!    expeps - Smallest value for exponential table
!
!-------------------------------------------------------------------------------
   use parrrtm_k,  only : mg, nbndlw, ngptlw
   use rrlw_tbl_k, only : ntbl, tblint, pade, bpade, tau_tbl, exp_tbl, tfn_tbl
   use rrlw_vsn_k, only : hvrini, hnamini
!
   real(kind=rb), intent(in   ) :: cpdair
!
! Local
!
   integer(kind=im) :: itr, ibnd, igc, ig, ind, ipr 
   integer(kind=im) :: igcsm, iprsm
   real(kind=rb)    :: wtsum, wtsm(mg)
   real(kind=rb)    :: tfn
   real(kind=rb), parameter :: expeps = 1.e-20
!-------------------------------------------------------------------------------
!
! ------- Definitions -------
! Arrays for 10000-point look-up tables:
! tau_tbl Clear-sky optical depth (used in cloudy radiative transfer)
! exp_tbl Exponential lookup table for ransmittance
! tfn_tbl Tau transition function; i.e. the transition of the Planck
!         function from that for the mean layer temperature to that for
!         the layer boundary temperature as a function of optical depth.
!         The "linear in tau" method is used to make the table.
! pade    Pade approximation constant (= 0.278)
! bpade   Inverse of the Pade approximation constant
!
   hvrini = '$Revision: 1.3 $'
!
! Initialize model data
!
   call lwdatinit(cpdair)
   call lwcmbdat               ! g-point interval reduction data
   call lwcldpr                ! cloud optical properties
   call lwatmref               ! reference MLS profile
   call lwavplank              ! Planck function 
!
! Moved to module_ra_rrtmg_lw for WRF
!
!  call lw_kgb01               ! molecular absorption coefficients
!  call lw_kgb02
!  call lw_kgb03
!  call lw_kgb04
!  call lw_kgb05
!  call lw_kgb06
!  call lw_kgb07
!  call lw_kgb08
!  call lw_kgb09
!  call lw_kgb10
!  call lw_kgb11
!  call lw_kgb12
!  call lw_kgb13
!  call lw_kgb14
!  call lw_kgb15
!  call lw_kgb16
!
! Compute lookup tables for transmittance, tau transition function,
! and clear sky tau (for the cloudy sky radiative transfer).  Tau is 
! computed as a function of the tau transition function, transmittance 
! is calculated as a function of tau, and the tau transition function 
! is calculated using the linear in tau formulation at values of tau 
! above 0.01.  TF is approximated as tau/6 for tau < 0.01.  All tables 
! are computed at intervals of 0.001.  The inverse of the constant used
! in the Pade approximation to the tau transition function is set to b.
!
   tau_tbl(0) = 0.0_rb
   tau_tbl(ntbl) = 1.e10_rb
   exp_tbl(0) = 1.0_rb
   exp_tbl(ntbl) = expeps
   tfn_tbl(0) = 0.0_rb
   tfn_tbl(ntbl) = 1.0_rb
   bpade = 1.0_rb / pade
!
   do itr = 1,ntbl-1
     tfn = real(itr) / real(ntbl)
     tau_tbl(itr) = bpade * tfn / (1._rb - tfn)
     exp_tbl(itr) = exp(-tau_tbl(itr))
     if (exp_tbl(itr) .le. expeps) exp_tbl(itr) = expeps
     if (tau_tbl(itr) .lt. 0.06_rb) then
       tfn_tbl(itr) = tau_tbl(itr)/6._rb
     else
       tfn_tbl(itr) = 1._rb-2._rb*((1._rb/tau_tbl(itr))                        &
                                  -(exp_tbl(itr)/(1.-exp_tbl(itr))))
     endif
   enddo
!
! Perform g-point reduction from 16 per band (256 total points) to
! a band dependant number (140 total points) for all absorption
! coefficient input data and Planck fraction input data.
! Compute relative weighting for new g-point combinations.
!
   igcsm = 0
   do ibnd = 1,nbndlw
     iprsm = 0
     if (ngc(ibnd).lt.mg) then
       do igc = 1,ngc(ibnd) 
         igcsm = igcsm + 1
         wtsum = 0._rb
         do ipr = 1,ngn(igcsm)
           iprsm = iprsm + 1
           wtsum = wtsum + wt(iprsm)
         enddo
         wtsm(igc) = wtsum
       enddo
!
       do ig = 1,ng(ibnd)
         ind = (ibnd-1)*mg + ig
         rwgt(ind) = wt(ig)/wtsm(ngm(ind))
       enddo
     else
       do ig = 1,ng(ibnd)
         igcsm = igcsm + 1
         ind = (ibnd-1)*mg + ig
         rwgt(ind) = 1.0_rb
       enddo
     endif
   enddo
!
! Reduce g-points for absorption coefficient data in each LW spectral band.
!
   call cmbgb1
   call cmbgb2
   call cmbgb3
   call cmbgb4
   call cmbgb5
   call cmbgb6
   call cmbgb7
   call cmbgb8
   call cmbgb9
   call cmbgb10
   call cmbgb11
   call cmbgb12
   call cmbgb13
   call cmbgb14
   call cmbgb15
   call cmbgb16
!
   end subroutine rrtmg_lw_ini
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lwdatinit (cpdair)
!-------------------------------------------------------------------------------
!
!  abstract : lwdatinit
!
!  input :
!    cpdair - Specific heat capacity of dry air at constant pressure at 273 K
!             (J kg-1 K-1)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,  only : maxxsec, maxinpx
   use rrlw_con_k, only : heatfac, grav, planck, boltz,                        &
                        clight, avogad, alosmt, gascon, radcn1, radcn2,        &
                        sbcnst, secdy 
   use rrlw_vsn_k
!
   save 
! 
   real(kind=rb), intent(in   ) :: cpdair
!-------------------------------------------------------------------------------
!
! Longwave spectral band limits (wavenumbers)
!
   wavenum1(:) = (/ 10._rb, 350._rb, 500._rb, 630._rb, 700._rb, 820._rb,       &
                   980._rb,1080._rb,1180._rb,1390._rb,1480._rb,1800._rb,       &
                  2080._rb,2250._rb,2380._rb,2600._rb/)
   wavenum2(:) = (/350._rb, 500._rb, 630._rb, 700._rb, 820._rb, 980._rb,       &
                  1080._rb,1180._rb,1390._rb,1480._rb,1800._rb,2080._rb,       &
                  2250._rb,2380._rb,2600._rb,3250._rb/)
   delwave(:)  = (/340._rb, 150._rb, 130._rb,  70._rb, 120._rb, 160._rb,       &
                   100._rb, 100._rb, 210._rb,  90._rb, 320._rb, 280._rb,       &
                   170._rb, 130._rb, 220._rb, 650._rb/)
!
! Spectral band information
!
   ng(:) = (/16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16/)
   nspa(:) = (/1,1,9,9,9,1,9,1,9,1,1,9,9,1,9,9/)
   nspb(:) = (/1,1,5,5,5,0,1,1,1,1,1,0,0,1,0,0/)
!
! nxmol     - number of cross-sections input by user
! ixindx(i) - index of cross-section molecule corresponding to Ith
!             cross-section specified by user
!             = 0 -- not allowed in rrtm
!             = 1 -- ccl4
!             = 2 -- cfc11
!             = 3 -- cfc12
!             = 4 -- cfc22
!
   nxmol = 4
   ixindx(1) = 1
   ixindx(2) = 2
   ixindx(3) = 3
   ixindx(4) = 4
   ixindx(5:maxinpx) = 0
!
! Fundamental physical constants from NIST 2002
!
   grav = 9.8066_rb                        ! Acceleration of gravity
                                           ! (m s-2)
   planck = 6.62606876e-27_rb              ! Planck constant
                                           ! (ergs s; g cm2 s-1)
   boltz = 1.3806503e-16_rb                ! Boltzmann constant
                                           ! (ergs K-1; g cm2 s-2 K-1)
   clight = 2.99792458e+10_rb              ! Speed of light in a vacuum  
                                           ! (cm s-1)
   avogad = 6.02214199e+23_rb              ! Avogadro constant
                                           ! (mol-1)
   alosmt = 2.6867775e+19_rb               ! Loschmidt constant
                                           ! (cm-3)
   gascon = 8.31447200e+07_rb              ! Molar gas constant
                                           ! (ergs mol-1 K-1)
   radcn1 = 1.191042722e-12_rb             ! First radiation constant
                                           ! (W cm2 sr-1)
   radcn2 = 1.4387752_rb                   ! Second radiation constant
                                           ! (cm K)
   sbcnst = 5.670400e-04_rb                ! Stefan-Boltzmann constant
                                           ! (W cm-2 K-4)
   secdy = 8.6400e4_rb                     ! Number of seconds per day
                                           ! (s d-1)
!
! units are generally cgs
!
! The first and second radiation constants are taken from NIST.
! They were previously obtained from the relations:
!      radcn1 = 2.*planck*clight*clight*1.e-07
!      radcn2 = planck*clight/boltz
!
! Heatfac is the factor by which delta-flux / delta-pressure is
! multiplied, with flux in W/m-2 and pressure in mbar, to get 
! the heating rate in units of degrees/day.  It is equal to:
! Original value:
!       (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!       Here, cpdair (1.004) is in units of J g-1 K-1, and the 
!       constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!    =  (9.8066)(86400)(1e-5)/(1.004)
! heatfac = 8.4391_rb
!
! Modified value for consistency with CAM3:
!       (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!       Here, cpdair (1.00464) is in units of J g-1 K-1, and the
!       constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!    =  (9.80616)(86400)(1e-5)/(1.00464)
! heatfac = 8.43339130434_rb
!
! Calculated value:
!    (grav) x (#sec/day) / (specific heat of dry air at const. p x 1.e2)
!       Here, cpdair is in units of J kg-1 K-1, and the constant (1.e2) 
!       converts mb to Pa when heatfac is multiplied by W m-2 mb-1. 
!
   heatfac = grav * secdy / (cpdair * 1.e2_rb)
!
   end subroutine lwdatinit
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lwcmbdat
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays for the g-point reduction from 256 to 140 for the 16 LW bands:
!  This mapping from 256 to 140 points has been carefully selected to 
!  minimize the effect on the resulting fluxes and cooling rates, and
!  caution should be used if the mapping is modified.  The full 256
!  g-point set can be restored with ngptlw=256, ngc=16*16, ngn=256*1., etc.
!
!  data :
!    ngptlw  The total number of new g-points
!    ngc     The number of new g-points in each band
!    ngs     The cumulative sum of new g-points for each band
!    ngm     The index of each new g-point relative to the original
!            16 g-points for each band.  
!    ngn     The number of original g-points that are combined to make
!            each new g-point in each band.
!    ngb     The band index for each new g-point.
!    wt      RRTM weights for 16 g-points.
!
!-------------------------------------------------------------------------------
!
   save
! 
! ------- Data statements -------
!
   ngc(:) = (/10,12,16,14,16,8,12,8,12,6,8,8,4,2,2,2/)
   ngs(:) = (/10,22,38,52,68,76,88,96,108,114,122,130,134,136,138,140/)
   ngm(:) = (/1,2,3,3,4,4,5,5,6,6,7,7,8,8,9,10,                      & ! band 1
              1,2,3,4,5,6,7,8,9,9,10,10,11,11,12,12,                 & ! band 2
              1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,                & ! band 3
              1,2,3,4,5,6,7,8,9,10,11,12,13,14,14,14,                & ! band 4
              1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,                & ! band 5
              1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,                       & ! band 6
              1,1,2,2,3,4,5,6,7,8,9,10,11,11,12,12,                  & ! band 7
              1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,                       & ! band 8
              1,2,3,4,5,6,7,8,9,9,10,10,11,11,12,12,                 & ! band 9
              1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6,                       & ! band 10
              1,2,3,3,4,4,5,5,6,6,7,7,7,8,8,8,                       & ! band 11
              1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8,                       & ! band 12
              1,1,1,2,2,2,3,3,3,3,4,4,4,4,4,4,                       & ! band 13
              1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,                       & ! band 14
              1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,                       & ! band 15
              1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2/)                        ! band 16
   ngn(:) = (/1,1,2,2,2,2,2,2,1,1,                                   & ! band 1
              1,1,1,1,1,1,1,1,2,2,2,2,                               & ! band 2
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,                       & ! band 3
              1,1,1,1,1,1,1,1,1,1,1,1,1,3,                           & ! band 4
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,                       & ! band 5
              2,2,2,2,2,2,2,2,                                       & ! band 6
              2,2,1,1,1,1,1,1,1,1,2,2,                               & ! band 7
              2,2,2,2,2,2,2,2,                                       & ! band 8
              1,1,1,1,1,1,1,1,2,2,2,2,                               & ! band 9
              2,2,2,2,4,4,                                           & ! band 10
              1,1,2,2,2,2,3,3,                                       & ! band 11
              1,1,1,1,2,2,4,4,                                       & ! band 12
              3,3,4,6,                                               & ! band 13
              8,8,                                                   & ! band 14
              8,8,                                                   & ! band 15
              4,12/)                                                   ! band 16
   ngb(:) = (/1,1,1,1,1,1,1,1,1,1,                                   & ! band 1
              2,2,2,2,2,2,2,2,2,2,2,2,                               & ! band 2
              3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,                       & ! band 3
              4,4,4,4,4,4,4,4,4,4,4,4,4,4,                           & ! band 4
              5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,                       & ! band 5
              6,6,6,6,6,6,6,6,                                       & ! band 6
              7,7,7,7,7,7,7,7,7,7,7,7,                               & ! band 7
              8,8,8,8,8,8,8,8,                                       & ! band 8
              9,9,9,9,9,9,9,9,9,9,9,9,                               & ! band 9
              10,10,10,10,10,10,                                     & ! band 10
              11,11,11,11,11,11,11,11,                               & ! band 11
              12,12,12,12,12,12,12,12,                               & ! band 12
              13,13,13,13,                                           & ! band 13
              14,14,                                                 & ! band 14
              15,15,                                                 & ! band 15
              16,16/)                                                  ! band 16
   wt(:) = (/ 0.1527534276_rb, 0.1491729617_rb, 0.1420961469_rb,               &
              0.1316886544_rb, 0.1181945205_rb, 0.1019300893_rb,               &
              0.0832767040_rb, 0.0626720116_rb, 0.0424925000_rb,               &
              0.0046269894_rb, 0.0038279891_rb, 0.0030260086_rb,               &
              0.0022199750_rb, 0.0014140010_rb, 0.0005330000_rb,               &
              0.0000750000_rb/)
!
   end subroutine lwcmbdat
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb1
!-------------------------------------------------------------------------------
!
!  abstract :
!  The subroutines CMBGB1->CMBGB16 input the absorption coefficient
!  data for each band, which are defined for 16 g-points and 16 spectral
!  bands. The data are combined with appropriate weighting following the
!  g-point mapping arrays specified in RRTMINIT.  Plank fraction data
!  in arrays FRACREFA and FRACREFB are combined without weighting.  All
!  g-point reduced data are put into new arrays for use in RRTM.
!
!  band 1:  10-350 cm-1 (low key - h2o; low minor - n2)
!                       (high key - h2o; high minor - n2)
!  note: previous versions of rrtm band 1: 
!        10-250 cm-1 (low - h2o; high - h2o)
!
!  history log :
!    1998-07-01  MJIacono  original version
!    1998-09-01  MJIacono  revision for GCMs
!    2002-09-01  MJIacono  revision for RRTMG
!    2006-06-01  MJIacono  revision for F90 reformatting
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng1
   use rrlw_kg01_k, only : fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2,   &
                         selfrefo, forrefo,                                    &
                         fracrefa, fracrefb, absa, ka, absb, kb, ka_mn2,kb_mn2,&
                         selfref, forref
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumk1, sumk2, sumf1, sumf2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(1)
         sumk = 0.
         do ipr = 1,ngn(igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
!
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(1)
         sumk = 0.
         do ipr = 1,ngn(igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(1)
       sumk = 0.
       do ipr = 1,ngn(igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(1)
       sumk = 0.
       do ipr = 1,ngn(igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(1)
       sumk1 = 0.
       sumk2 = 0.
       do ipr = 1,ngn(igc)
         iprsm = iprsm + 1
         sumk1 = sumk1 + kao_mn2(jt,iprsm)*rwgt(iprsm)
         sumk2 = sumk2 + kbo_mn2(jt,iprsm)*rwgt(iprsm)
       enddo
       ka_mn2(jt,igc) = sumk1
       kb_mn2(jt,igc) = sumk2
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(1)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(igc)
       iprsm = iprsm + 1
       sumf1 = sumf1+ fracrefao(iprsm)
       sumf2 = sumf2+ fracrefbo(iprsm)
     enddo
     fracrefa(igc) = sumf1
     fracrefb(igc) = sumf2
   enddo
!
   end subroutine cmbgb1
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb2
!-------------------------------------------------------------------------------
!
!  abstract : 
!  band 2:  350-500 cm-1 (low key - h2o; high key - h2o)
!
!  note: previous version of rrtm band 2: 
!        250 - 500 cm-1 (low - h2o; high - h2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng2
   use rrlw_kg02_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo,  &
                         fracrefa, fracrefb, absa, ka, absb, kb, selfref, forref
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf1, sumf2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(2)
         sumk = 0.
         do ipr = 1,ngn(ngs(1)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+16)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
!
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(2)
         sumk = 0.
         do ipr = 1,ngn(ngs(1)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+16)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(2)
       sumk = 0.
       do ipr = 1,ngn(ngs(1)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+16)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(2)
       sumk = 0.
       do ipr = 1,ngn(ngs(1)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+16)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(2)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(1)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1+ fracrefao(iprsm)
       sumf2 = sumf2+ fracrefbo(iprsm)
     enddo
     fracrefa(igc) = sumf1
     fracrefb(igc) = sumf2
   enddo
!
   end subroutine cmbgb2
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb3
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 3:  500-630 cm-1 (low key - h2o,co2; low minor - n2o)
!                        (high key - h2o,co2; high minor - n2o)
!
!  old band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng3
   use rrlw_kg03_k, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, &
                         selfrefo, forrefo,                                    &
                         fracrefa, fracrefb, absa, ka, absb,kb,ka_mn2o,kb_mn2o,&
                         selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(3)
          sumk = 0.
           do ipr = 1,ngn(ngs(2)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+32)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,5
     do jt = 1,5
       do jp = 13,59
         iprsm = 0
         do igc = 1,ngc(3)
           sumk = 0.
           do ipr = 1,ngn(ngs(2)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+32)
           enddo
           kb(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,9
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(3)
         sumk = 0.
         do ipr = 1,ngn(ngs(2)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao_mn2o(jn,jt,iprsm)*rwgt(iprsm+32)
         enddo
         ka_mn2o(jn,jt,igc) = sumk
       enddo
     enddo
   enddo
!
   do jn = 1,5
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(3)
         sumk = 0.
         do ipr = 1,ngn(ngs(2)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo_mn2o(jn,jt,iprsm)*rwgt(iprsm+32)
         enddo
         kb_mn2o(jn,jt,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(3)
       sumk = 0.
       do ipr = 1,ngn(ngs(2)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+32)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(3)
       sumk = 0.
       do ipr = 1,ngn(ngs(2)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+32)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(3)
       sumf = 0.
       do ipr = 1,ngn(ngs(2)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   do jp = 1,5
     iprsm = 0
     do igc = 1,ngc(3)
       sumf = 0.
       do ipr = 1,ngn(ngs(2)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefbo(iprsm,jp)
       enddo
       fracrefb(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb3
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb4
!-------------------------------------------------------------------------------
!
!  abstract : 
!  band 4:  630-700 cm-1 (low key - h2o,co2; high key - o3,co2)
!
!  old band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng4
   use rrlw_kg04_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo,  &
                         fracrefa, fracrefb, absa, ka, absb, kb, selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(4)
          sumk = 0.
           do ipr = 1,ngn(ngs(3)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+48)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,5
     do jt = 1,5
       do jp = 13,59
         iprsm = 0
         do igc = 1,ngc(4)
           sumk = 0.
           do ipr = 1,ngn(ngs(3)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+48)
           enddo
           kb(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(4)
       sumk = 0.
       do ipr = 1,ngn(ngs(3)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+48)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(4)
       sumk = 0.
       do ipr = 1,ngn(ngs(3)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+48)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(4)
       sumf = 0.
       do ipr = 1,ngn(ngs(3)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   do jp = 1,5
     iprsm = 0
     do igc = 1,ngc(4)
       sumf = 0.
       do ipr = 1,ngn(ngs(3)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefbo(iprsm,jp)
       enddo
       fracrefb(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb4
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb5
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 5:  700-820 cm-1 (low key - h2o,co2; low minor - o3, ccl4)
!                        (high key - o3,co2)
!
!  old band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng5
   use rrlw_kg05_k, only : fracrefao, fracrefbo, kao, kbo, kao_mo3, ccl4o,     &
                         selfrefo, forrefo,                                    &
                         fracrefa, fracrefb, absa, ka, absb, kb, ka_mo3, ccl4, &
                         selfref, forref
!
! Local
!
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
      real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(5)
           sumk = 0.
           do ipr = 1,ngn(ngs(4)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+64)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,5
     do jt = 1,5
       do jp = 13,59
         iprsm = 0
         do igc = 1,ngc(5)
           sumk = 0.
           do ipr = 1,ngn(ngs(4)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+64)
           enddo
           kb(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,9
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(5)
        sumk = 0.
         do ipr = 1,ngn(ngs(4)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao_mo3(jn,jt,iprsm)*rwgt(iprsm+64)
         enddo
         ka_mo3(jn,jt,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(5)
       sumk = 0.
       do ipr = 1,ngn(ngs(4)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+64)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(5)
       sumk = 0.
       do ipr = 1,ngn(ngs(4)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+64)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(5)
       sumf = 0.
       do ipr = 1,ngn(ngs(4)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   do jp = 1,5
     iprsm = 0
     do igc = 1,ngc(5)
       sumf = 0.
       do ipr = 1,ngn(ngs(4)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefbo(iprsm,jp)
       enddo
       fracrefb(igc,jp) = sumf
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(5)
     sumk = 0.
     do ipr = 1,ngn(ngs(4)+igc)
       iprsm = iprsm + 1
       sumk = sumk + ccl4o(iprsm)*rwgt(iprsm+64)
     enddo
     ccl4(igc) = sumk
   enddo
!
   end subroutine cmbgb5
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb6
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 6:  820-980 cm-1 (low key - h2o; low minor - co2)
!                        (high key - nothing; high minor - cfc11, cfc12)
!
!  old band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw
   use rrlw_kg06
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf, sumk1, sumk2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(6)
         sumk = 0.
         do ipr = 1,ngn(ngs(5)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+80)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(6)
       sumk = 0.
       do ipr = 1,ngn(ngs(5)+igc)
         iprsm = iprsm + 1
         sumk = sumk + kao_mco2(jt,iprsm)*rwgt(iprsm+80)
       enddo
       ka_mco2(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(6)
       sumk = 0.
       do ipr = 1,ngn(ngs(5)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+80)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(6)
       sumk = 0.
       do ipr = 1,ngn(ngs(5)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+80)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(6)
     sumf = 0.
     sumk1 = 0.
     sumk2 = 0.
     do ipr = 1,ngn(ngs(5)+igc)
       iprsm = iprsm + 1
       sumf = sumf + fracrefao(iprsm)
       sumk1 = sumk1+ cfc11adjo(iprsm)*rwgt(iprsm+80)
       sumk2 = sumk2+ cfc12o(iprsm)*rwgt(iprsm+80)
     enddo
     fracrefa(igc) = sumf
     cfc11adj(igc) = sumk1
     cfc12(igc) = sumk2
   enddo
!
   end subroutine cmbgb6
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb7
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 7:  980-1080 cm-1 (low key - h2o,o3; low minor - co2)
!                         (high key - o3; high minor - co2)
!
!  old band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng7
   use rrlw_kg07_k, only : fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, &
                         selfrefo, forrefo,                                    &
                         fracrefa, fracrefb, absa, ka, absb,kb,ka_mco2,kb_mco2,&
                         selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(7)
           sumk = 0.
           do ipr = 1,ngn(ngs(6)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+96)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(7)
         sumk = 0.
         do ipr = 1,ngn(ngs(6)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+96)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jn = 1,9
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(7)
         sumk = 0.
         do ipr = 1,ngn(ngs(6)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao_mco2(jn,jt,iprsm)*rwgt(iprsm+96)
         enddo
         ka_mco2(jn,jt,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(7)
       sumk = 0.
       do ipr = 1,ngn(ngs(6)+igc)
         iprsm = iprsm + 1
         sumk = sumk + kbo_mco2(jt,iprsm)*rwgt(iprsm+96)
       enddo
       kb_mco2(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(7)
       sumk = 0.
       do ipr = 1,ngn(ngs(6)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+96)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(7)
       sumk = 0.
       do ipr = 1,ngn(ngs(6)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+96)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(7)
       sumf = 0.
       do ipr = 1,ngn(ngs(6)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(7)
     sumf = 0.
     do ipr = 1,ngn(ngs(6)+igc)
       iprsm = iprsm + 1
       sumf = sumf + fracrefbo(iprsm)
     enddo
     fracrefb(igc) = sumf
   enddo
!
   end subroutine cmbgb7
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb8
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 8:  1080-1180 cm-1 (low key - h2o; low minor - co2,o3,n2o)
!                          (high key - o3; high minor - co2, n2o)
!
!  old band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng8
   use rrlw_kg08_k, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mn2o,      &
                         kao_mo3, kbo, kbo_mco2, kbo_mn2o, selfrefo, forrefo,  &
                         cfc12o, cfc22adjo,                                    &
                         fracrefa, fracrefb, absa, ka, ka_mco2, ka_mn2o,       &
                         ka_mo3, absb, kb, kb_mco2, kb_mn2o, selfref, forref,  &
                         cfc12, cfc22adj
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumk1, sumk2, sumk3, sumk4, sumk5, sumf1, sumf2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(8)
         sumk = 0.
         do ipr = 1,ngn(ngs(7)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+112)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(8)
         sumk = 0.
         do ipr = 1,ngn(ngs(7)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+112)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(8)
       sumk = 0.
       do ipr = 1,ngn(ngs(7)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+112)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(8)
       sumk = 0.
       do ipr = 1,ngn(ngs(7)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+112)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(8)
       sumk1 = 0.
       sumk2 = 0.
       sumk3 = 0.
       sumk4 = 0.
       sumk5 = 0.
       do ipr = 1,ngn(ngs(7)+igc)
         iprsm = iprsm + 1
         sumk1 = sumk1 + kao_mco2(jt,iprsm)*rwgt(iprsm+112)
         sumk2 = sumk2 + kbo_mco2(jt,iprsm)*rwgt(iprsm+112)
         sumk3 = sumk3 + kao_mo3(jt,iprsm)*rwgt(iprsm+112)
         sumk4 = sumk4 + kao_mn2o(jt,iprsm)*rwgt(iprsm+112)
         sumk5 = sumk5 + kbo_mn2o(jt,iprsm)*rwgt(iprsm+112)
       enddo
       ka_mco2(jt,igc) = sumk1
       kb_mco2(jt,igc) = sumk2
       ka_mo3(jt,igc) = sumk3
       ka_mn2o(jt,igc) = sumk4
       kb_mn2o(jt,igc) = sumk5
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(8)
     sumf1 = 0.
     sumf2 = 0.
     sumk1 = 0.
     sumk2 = 0.
     do ipr = 1,ngn(ngs(7)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1+ fracrefao(iprsm)
       sumf2 = sumf2+ fracrefbo(iprsm)
       sumk1 = sumk1+ cfc12o(iprsm)*rwgt(iprsm+112)
       sumk2 = sumk2+ cfc22adjo(iprsm)*rwgt(iprsm+112)
     enddo
     fracrefa(igc) = sumf1
     fracrefb(igc) = sumf2
     cfc12(igc) = sumk1
     cfc22adj(igc) = sumk2
   enddo
!
   end subroutine cmbgb8
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb9
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 9:  1180-1390 cm-1 (low key - h2o,ch4; low minor - n2o)
!                          (high key - ch4; high minor - n2o)!
!
!  old band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng9
   use rrlw_kg09_k, only : fracrefao, fracrefbo, kao, kao_mn2o,                &
                         kbo, kbo_mn2o, selfrefo, forrefo,                     &
                         fracrefa, fracrefb, absa, ka, ka_mn2o,                &
                         absb, kb, kb_mn2o, selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(9)
           sumk = 0.
           do ipr = 1,ngn(ngs(8)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+128)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(9)
         sumk = 0.
         do ipr = 1,ngn(ngs(8)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+128)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jn = 1,9
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(9)
         sumk = 0.
         do ipr = 1,ngn(ngs(8)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao_mn2o(jn,jt,iprsm)*rwgt(iprsm+128)
         enddo
         ka_mn2o(jn,jt,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(9)
       sumk = 0.
       do ipr = 1,ngn(ngs(8)+igc)
         iprsm = iprsm + 1
         sumk = sumk + kbo_mn2o(jt,iprsm)*rwgt(iprsm+128)
       enddo
       kb_mn2o(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(9)
       sumk = 0.
       do ipr = 1,ngn(ngs(8)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+128)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(9)
       sumk = 0.
       do ipr = 1,ngn(ngs(8)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+128)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(9)
       sumf = 0.
       do ipr = 1,ngn(ngs(8)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(9)
     sumf = 0.
     do ipr = 1,ngn(ngs(8)+igc)
       iprsm = iprsm + 1
       sumf = sumf + fracrefbo(iprsm)
     enddo
     fracrefb(igc) = sumf
   enddo
!
   end subroutine cmbgb9
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb10
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 10:  1390-1480 cm-1 (low key - h2o; high key - h2o)
!
!  old band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng10
   use rrlw_kg10_k, only : fracrefao, fracrefbo, kao, kbo,                     &
                         selfrefo, forrefo,                                    &
                         fracrefa, fracrefb, absa, ka, absb, kb,               &
                         selfref, forref
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf1, sumf2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(10)
         sumk = 0.
         do ipr = 1,ngn(ngs(9)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+144)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(10)
         sumk = 0.
         do ipr = 1,ngn(ngs(9)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+144)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(10)
       sumk = 0.
       do ipr = 1,ngn(ngs(9)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+144)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(10)
       sumk = 0.
       do ipr = 1,ngn(ngs(9)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+144)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(10)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(9)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1+ fracrefao(iprsm)
       sumf2 = sumf2+ fracrefbo(iprsm)
     enddo
     fracrefa(igc) = sumf1
     fracrefb(igc) = sumf2
   enddo
!
   end subroutine cmbgb10
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb11
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 11:  1480-1800 cm-1 (low - h2o; low minor - o2)
!                           (high key - h2o; high minor - o2)
!
!  old band 11:  1480-1800 cm-1 (low - h2o; low minor - o2)
!                               (high key - h2o; high minor - o2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng11
   use rrlw_kg11_k, only : fracrefao, fracrefbo, kao, kao_mo2,                 &
                         kbo, kbo_mo2, selfrefo, forrefo,                      &
                         fracrefa, fracrefb, absa, ka, ka_mo2,                 &
                         absb, kb, kb_mo2, selfref, forref
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumk1, sumk2, sumf1, sumf2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(11)
         sumk = 0.
         do ipr = 1,ngn(ngs(10)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+160)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(11)
         sumk = 0.
         do ipr = 1,ngn(ngs(10)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+160)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(11)
       sumk1 = 0.
       sumk2 = 0.
       do ipr = 1,ngn(ngs(10)+igc)
         iprsm = iprsm + 1
         sumk1 = sumk1 + kao_mo2(jt,iprsm)*rwgt(iprsm+160)
         sumk2 = sumk2 + kbo_mo2(jt,iprsm)*rwgt(iprsm+160)
       enddo
       ka_mo2(jt,igc) = sumk1
       kb_mo2(jt,igc) = sumk2
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(11)
       sumk = 0.
       do ipr = 1,ngn(ngs(10)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+160)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(11)
       sumk = 0.
       do ipr = 1,ngn(ngs(10)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+160)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(11)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(10)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1+ fracrefao(iprsm)
       sumf2 = sumf2+ fracrefbo(iprsm)
     enddo
     fracrefa(igc) = sumf1
     fracrefb(igc) = sumf2
   enddo
!
   end subroutine cmbgb11
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb12
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
!  old band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng12
   use rrlw_kg12_k, only : fracrefao, kao, selfrefo, forrefo,                  &
                         fracrefa, absa, ka, selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(12)
           sumk = 0.
           do ipr = 1,ngn(ngs(11)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+176)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(12)
       sumk = 0.
       do ipr = 1,ngn(ngs(11)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+176)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(12)
       sumk = 0.
       do ipr = 1,ngn(ngs(11)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+176)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(12)
       sumf = 0.
       do ipr = 1,ngn(ngs(11)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb12
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb13
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 13:  2080-2250 cm-1 (low key - h2o,n2o; high minor - o3 minor)
!
!  old band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng13
   use rrlw_kg13_k, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mco,       &
                         kbo_mo3, selfrefo, forrefo,                           &
                         fracrefa, fracrefb, absa, ka, ka_mco2, ka_mco,        &
                         kb_mo3, selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumk1, sumk2, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(13)
           sumk = 0.
           do ipr = 1,ngn(ngs(12)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+192)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,9
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(13)
         sumk1 = 0.
         sumk2 = 0.
         do ipr = 1,ngn(ngs(12)+igc)
           iprsm = iprsm + 1
           sumk1 = sumk1 + kao_mco2(jn,jt,iprsm)*rwgt(iprsm+192)
           sumk2 = sumk2 + kao_mco(jn,jt,iprsm)*rwgt(iprsm+192)
         enddo
         ka_mco2(jn,jt,igc) = sumk1
         ka_mco(jn,jt,igc) = sumk2
       enddo
     enddo
   enddo
!
   do jt = 1,19
     iprsm = 0
     do igc = 1,ngc(13)
       sumk = 0.
       do ipr = 1,ngn(ngs(12)+igc)
         iprsm = iprsm + 1
         sumk = sumk + kbo_mo3(jt,iprsm)*rwgt(iprsm+192)
       enddo
       kb_mo3(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(13)
       sumk = 0.
       do ipr = 1,ngn(ngs(12)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+192)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(13)
       sumk = 0.
       do ipr = 1,ngn(ngs(12)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+192)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(13)
     sumf = 0.
     do ipr = 1,ngn(ngs(12)+igc)
       iprsm = iprsm + 1
       sumf = sumf + fracrefbo(iprsm)
     enddo
     fracrefb(igc) = sumf
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(13)
       sumf = 0.
       do ipr = 1,ngn(ngs(12)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb13
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb14
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
!  old band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng14
   use rrlw_kg14_k, only : fracrefao, fracrefbo, kao, kbo,                     &
                         selfrefo, forrefo,                                    &
                         fracrefa, fracrefb, absa, ka, absb, kb,               &
                         selfref, forref
!
! Local
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf1, sumf2
!-------------------------------------------------------------------------------
!
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(14)
         sumk = 0.
         do ipr = 1,ngn(ngs(13)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+208)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(14)
         sumk = 0.
         do ipr = 1,ngn(ngs(13)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+208)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(14)
       sumk = 0.
       do ipr = 1,ngn(ngs(13)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+208)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(14)
       sumk = 0.
       do ipr = 1,ngn(ngs(13)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+208)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(14)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(13)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1+ fracrefao(iprsm)
       sumf2 = sumf2+ fracrefbo(iprsm)
     enddo
     fracrefa(igc) = sumf1
     fracrefb(igc) = sumf2
   enddo
!
   end subroutine cmbgb14
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb15
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 15:  2380-2600 cm-1 (low - n2o,co2; low minor - n2)
!                           (high - nothing)
!
!  old band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng15
   use rrlw_kg15_k, only : fracrefao, kao, kao_mn2, selfrefo, forrefo,         &
                         fracrefa, absa, ka, ka_mn2, selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(15)
           sumk = 0.
           do ipr = 1,ngn(ngs(14)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+224)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jn = 1,9
     do jt = 1,19
       iprsm = 0
       do igc = 1,ngc(15)
         sumk = 0.
         do ipr = 1,ngn(ngs(14)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao_mn2(jn,jt,iprsm)*rwgt(iprsm+224)
         enddo
         ka_mn2(jn,jt,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(15)
       sumk = 0.
       do ipr = 1,ngn(ngs(14)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+224)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(15)
       sumk = 0.
       do ipr = 1,ngn(ngs(14)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+224)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(15)
       sumf = 0.
       do ipr = 1,ngn(ngs(14)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb15
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb16
!-------------------------------------------------------------------------------
!
!  abstract :
!  band 16:  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!
!  old band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,   only : mg, nbndlw, ngptlw, ng16
   use rrlw_kg16_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo,  &
                        fracrefa, fracrefb, absa, ka, absb, kb, selfref, forref
!
! Local
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm 
   real(kind=rb)    :: sumk, sumf
!-------------------------------------------------------------------------------
!
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(16)
           sumk = 0.
           do ipr = 1,ngn(ngs(15)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+240)
           enddo
           ka(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(16)
         sumk = 0.
         do ipr = 1,ngn(ngs(15)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+240)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   do jt = 1,10
     iprsm = 0
     do igc = 1,ngc(16)
       sumk = 0.
       do ipr = 1,ngn(ngs(15)+igc)
         iprsm = iprsm + 1
         sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+240)
       enddo
       selfref(jt,igc) = sumk
     enddo
   enddo
!
   do jt = 1,4
     iprsm = 0
     do igc = 1,ngc(16)
       sumk = 0.
       do ipr = 1,ngn(ngs(15)+igc)
         iprsm = iprsm + 1
         sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+240)
       enddo
       forref(jt,igc) = sumk
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(16)
     sumf = 0.
     do ipr = 1,ngn(ngs(15)+igc)
       iprsm = iprsm + 1
       sumf = sumf + fracrefbo(iprsm)
     enddo
     fracrefb(igc) = sumf
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(16)
       sumf = 0.
       do ipr = 1,ngn(ngs(15)+igc)
         iprsm = iprsm + 1
         sumf = sumf + fracrefao(iprsm,jp)
       enddo
       fracrefa(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb16
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lwcldpr
!-------------------------------------------------------------------------------
   use rrlw_cld_k, only : abscld1, absliq0, absliq1,                           &
                        absice0, absice1, absice2, absice3
!
   save
!
! abscldn is the liquid water absorption coefficient (m2/g). 
! For inflag = 1.
!
   abscld1 = 0.0602410_rb
!  
! Everything below is for inflag = 2.
!
! absicen(j,ib) are the parameters needed to compute the liquid water 
! absorption coefficient in spectral region ib for iceflag=n.  The units
! of absicen(1,ib) are m2/g and absicen(2,ib) has units (microns (m2/g)).
! For iceflag = 0.
!
   absice0(:)= (/0.005_rb,  1.0_rb/)
!
! For iceflag = 1.
!
   absice1(1,:) = (/0.0036_rb, 0.0068_rb, 0.0003_rb, 0.0016_rb, 0.0020_rb/)
   absice1(2,:) = (/1.136_rb , 0.600_rb , 1.338_rb , 1.166_rb , 1.118_rb /)
!
! For iceflag = 2.  In each band, the absorption
! coefficients are listed for a range of effective radii from 5.0
! to 131.0 microns in increments of 3.0 microns.
! Spherical Ice Particle Parameterization
! absorption units (abs coef/iwc): [(m^-1)/(g m^-3)]
!
! band 1
!
   absice2(:,1) = (/                                                           &
   7.798999e-02_rb,6.340479e-02_rb,5.417973e-02_rb,4.766245e-02_rb,            &
   4.272663e-02_rb,3.880939e-02_rb,3.559544e-02_rb,3.289241e-02_rb,            &
   3.057511e-02_rb,2.855800e-02_rb,2.678022e-02_rb,2.519712e-02_rb,            &
   2.377505e-02_rb,2.248806e-02_rb,2.131578e-02_rb,2.024194e-02_rb,            &
   1.925337e-02_rb,1.833926e-02_rb,1.749067e-02_rb,1.670007e-02_rb,            &
   1.596113e-02_rb,1.526845e-02_rb,1.461739e-02_rb,1.400394e-02_rb,            &
   1.342462e-02_rb,1.287639e-02_rb,1.235656e-02_rb,1.186279e-02_rb,            &
   1.139297e-02_rb,1.094524e-02_rb,1.051794e-02_rb,1.010956e-02_rb,            &
   9.718755e-03_rb,9.344316e-03_rb,8.985139e-03_rb,8.640223e-03_rb,            &
   8.308656e-03_rb,7.989606e-03_rb,7.682312e-03_rb,7.386076e-03_rb,            &
   7.100255e-03_rb,6.824258e-03_rb,6.557540e-03_rb/)
!
! band 2
!
   absice2(:,2) = (/                                                           &
   2.784879e-02_rb,2.709863e-02_rb,2.619165e-02_rb,2.529230e-02_rb,            &
   2.443225e-02_rb,2.361575e-02_rb,2.284021e-02_rb,2.210150e-02_rb,            &
   2.139548e-02_rb,2.071840e-02_rb,2.006702e-02_rb,1.943856e-02_rb,            &
   1.883064e-02_rb,1.824120e-02_rb,1.766849e-02_rb,1.711099e-02_rb,            &
   1.656737e-02_rb,1.603647e-02_rb,1.551727e-02_rb,1.500886e-02_rb,            &
   1.451045e-02_rb,1.402132e-02_rb,1.354084e-02_rb,1.306842e-02_rb,            &
   1.260355e-02_rb,1.214575e-02_rb,1.169460e-02_rb,1.124971e-02_rb,            &
   1.081072e-02_rb,1.037731e-02_rb,9.949167e-03_rb,9.526021e-03_rb,            &
   9.107615e-03_rb,8.693714e-03_rb,8.284096e-03_rb,7.878558e-03_rb,            &
   7.476910e-03_rb,7.078974e-03_rb,6.684586e-03_rb,6.293589e-03_rb,            &
   5.905839e-03_rb,5.521200e-03_rb,5.139543e-03_rb/)
!
! band 3
!
   absice2(:,3) = (/                                                           &
   1.065397e-01_rb,8.005726e-02_rb,6.546428e-02_rb,5.589131e-02_rb,            &
   4.898681e-02_rb,4.369932e-02_rb,3.947901e-02_rb,3.600676e-02_rb,            &
   3.308299e-02_rb,3.057561e-02_rb,2.839325e-02_rb,2.647040e-02_rb,            &
   2.475872e-02_rb,2.322164e-02_rb,2.183091e-02_rb,2.056430e-02_rb,            &
   1.940407e-02_rb,1.833586e-02_rb,1.734787e-02_rb,1.643034e-02_rb,            &
   1.557512e-02_rb,1.477530e-02_rb,1.402501e-02_rb,1.331924e-02_rb,            &
   1.265364e-02_rb,1.202445e-02_rb,1.142838e-02_rb,1.086257e-02_rb,            &
   1.032445e-02_rb,9.811791e-03_rb,9.322587e-03_rb,8.855053e-03_rb,            &
   8.407591e-03_rb,7.978763e-03_rb,7.567273e-03_rb,7.171949e-03_rb,            &
   6.791728e-03_rb,6.425642e-03_rb,6.072809e-03_rb,5.732424e-03_rb,            &
   5.403748e-03_rb,5.086103e-03_rb,4.778865e-03_rb/)
!
! band 4
!
   absice2(:,4) = (/                                                           &
   1.804566e-01_rb,1.168987e-01_rb,8.680442e-02_rb,6.910060e-02_rb,            &
   5.738174e-02_rb,4.902332e-02_rb,4.274585e-02_rb,3.784923e-02_rb,            &
   3.391734e-02_rb,3.068690e-02_rb,2.798301e-02_rb,2.568480e-02_rb,            &
   2.370600e-02_rb,2.198337e-02_rb,2.046940e-02_rb,1.912777e-02_rb,            &
   1.793016e-02_rb,1.685420e-02_rb,1.588193e-02_rb,1.499882e-02_rb,            &
   1.419293e-02_rb,1.345440e-02_rb,1.277496e-02_rb,1.214769e-02_rb,            &
   1.156669e-02_rb,1.102694e-02_rb,1.052412e-02_rb,1.005451e-02_rb,            &
   9.614854e-03_rb,9.202335e-03_rb,8.814470e-03_rb,8.449077e-03_rb,            &
   8.104223e-03_rb,7.778195e-03_rb,7.469466e-03_rb,7.176671e-03_rb,            &
   6.898588e-03_rb,6.634117e-03_rb,6.382264e-03_rb,6.142134e-03_rb,            &
   5.912913e-03_rb,5.693862e-03_rb,5.484308e-03_rb/)
!
! band 5
!
   absice2(:,5) = (/                                                           &
   2.131806e-01_rb,1.311372e-01_rb,9.407171e-02_rb,7.299442e-02_rb,            &
   5.941273e-02_rb,4.994043e-02_rb,4.296242e-02_rb,3.761113e-02_rb,            &
   3.337910e-02_rb,2.994978e-02_rb,2.711556e-02_rb,2.473461e-02_rb,            &
   2.270681e-02_rb,2.095943e-02_rb,1.943839e-02_rb,1.810267e-02_rb,            &
   1.692057e-02_rb,1.586719e-02_rb,1.492275e-02_rb,1.407132e-02_rb,            &
   1.329989e-02_rb,1.259780e-02_rb,1.195618e-02_rb,1.136761e-02_rb,            &
   1.082583e-02_rb,1.032552e-02_rb,9.862158e-03_rb,9.431827e-03_rb,            &
   9.031157e-03_rb,8.657217e-03_rb,8.307449e-03_rb,7.979609e-03_rb,            &
   7.671724e-03_rb,7.382048e-03_rb,7.109032e-03_rb,6.851298e-03_rb,            &
   6.607615e-03_rb,6.376881e-03_rb,6.158105e-03_rb,5.950394e-03_rb,            &
   5.752942e-03_rb,5.565019e-03_rb,5.385963e-03_rb/)
!
! band 6
!
   absice2(:,6) = (/                                                           &
   1.546177e-01_rb,1.039251e-01_rb,7.910347e-02_rb,6.412429e-02_rb,            &
   5.399997e-02_rb,4.664937e-02_rb,4.104237e-02_rb,3.660781e-02_rb,            &
   3.300218e-02_rb,3.000586e-02_rb,2.747148e-02_rb,2.529633e-02_rb,            &
   2.340647e-02_rb,2.174723e-02_rb,2.027731e-02_rb,1.896487e-02_rb,            &
   1.778492e-02_rb,1.671761e-02_rb,1.574692e-02_rb,1.485978e-02_rb,            &
   1.404543e-02_rb,1.329489e-02_rb,1.260066e-02_rb,1.195636e-02_rb,            &
   1.135657e-02_rb,1.079664e-02_rb,1.027257e-02_rb,9.780871e-03_rb,            &
   9.318505e-03_rb,8.882815e-03_rb,8.471458e-03_rb,8.082364e-03_rb,            &
   7.713696e-03_rb,7.363817e-03_rb,7.031264e-03_rb,6.714725e-03_rb,            &
   6.413021e-03_rb,6.125086e-03_rb,5.849958e-03_rb,5.586764e-03_rb,            &
   5.334707e-03_rb,5.093066e-03_rb,4.861179e-03_rb/)
!
! band 7
!
   absice2(:,7) = (/                                                           &
   7.583404e-02_rb,6.181558e-02_rb,5.312027e-02_rb,4.696039e-02_rb,            &
   4.225986e-02_rb,3.849735e-02_rb,3.538340e-02_rb,3.274182e-02_rb,            &
   3.045798e-02_rb,2.845343e-02_rb,2.667231e-02_rb,2.507353e-02_rb,            &
   2.362606e-02_rb,2.230595e-02_rb,2.109435e-02_rb,1.997617e-02_rb,            &
   1.893916e-02_rb,1.797328e-02_rb,1.707016e-02_rb,1.622279e-02_rb,            &
   1.542523e-02_rb,1.467241e-02_rb,1.395997e-02_rb,1.328414e-02_rb,            &
   1.264164e-02_rb,1.202958e-02_rb,1.144544e-02_rb,1.088697e-02_rb,            &
   1.035218e-02_rb,9.839297e-03_rb,9.346733e-03_rb,8.873057e-03_rb,            &
   8.416980e-03_rb,7.977335e-03_rb,7.553066e-03_rb,7.143210e-03_rb,            &
   6.746888e-03_rb,6.363297e-03_rb,5.991700e-03_rb,5.631422e-03_rb,            &
   5.281840e-03_rb,4.942378e-03_rb,4.612505e-03_rb/)
!
! band 8
!
   absice2(:,8) = (/                                                           &
   9.022185e-02_rb,6.922700e-02_rb,5.710674e-02_rb,4.898377e-02_rb,            &
   4.305946e-02_rb,3.849553e-02_rb,3.484183e-02_rb,3.183220e-02_rb,            &
   2.929794e-02_rb,2.712627e-02_rb,2.523856e-02_rb,2.357810e-02_rb,            &
   2.210286e-02_rb,2.078089e-02_rb,1.958747e-02_rb,1.850310e-02_rb,            &
   1.751218e-02_rb,1.660205e-02_rb,1.576232e-02_rb,1.498440e-02_rb,            &
   1.426107e-02_rb,1.358624e-02_rb,1.295474e-02_rb,1.236212e-02_rb,            &
   1.180456e-02_rb,1.127874e-02_rb,1.078175e-02_rb,1.031106e-02_rb,            &
   9.864433e-03_rb,9.439878e-03_rb,9.035637e-03_rb,8.650140e-03_rb,            &
   8.281981e-03_rb,7.929895e-03_rb,7.592746e-03_rb,7.269505e-03_rb,            &
   6.959238e-03_rb,6.661100e-03_rb,6.374317e-03_rb,6.098185e-03_rb,            &
   5.832059e-03_rb,5.575347e-03_rb,5.327504e-03_rb/)
!
! band 9
!
   absice2(:,9) = (/                                                           &
   1.294087e-01_rb,8.788217e-02_rb,6.728288e-02_rb,5.479720e-02_rb,            &
   4.635049e-02_rb,4.022253e-02_rb,3.555576e-02_rb,3.187259e-02_rb,            &
   2.888498e-02_rb,2.640843e-02_rb,2.431904e-02_rb,2.253038e-02_rb,            &
   2.098024e-02_rb,1.962267e-02_rb,1.842293e-02_rb,1.735426e-02_rb,            &
   1.639571e-02_rb,1.553060e-02_rb,1.474552e-02_rb,1.402953e-02_rb,            &
   1.337363e-02_rb,1.277033e-02_rb,1.221336e-02_rb,1.169741e-02_rb,            &
   1.121797e-02_rb,1.077117e-02_rb,1.035369e-02_rb,9.962643e-03_rb,            &
   9.595509e-03_rb,9.250088e-03_rb,8.924447e-03_rb,8.616876e-03_rb,            &
   8.325862e-03_rb,8.050057e-03_rb,7.788258e-03_rb,7.539388e-03_rb,            &
   7.302478e-03_rb,7.076656e-03_rb,6.861134e-03_rb,6.655197e-03_rb,            &
   6.458197e-03_rb,6.269543e-03_rb,6.088697e-03_rb/)
!
! band 10
!
   absice2(:,10) = (/                                                          &
   1.593628e-01_rb,1.014552e-01_rb,7.458955e-02_rb,5.903571e-02_rb,            &
   4.887582e-02_rb,4.171159e-02_rb,3.638480e-02_rb,3.226692e-02_rb,            &
   2.898717e-02_rb,2.631256e-02_rb,2.408925e-02_rb,2.221156e-02_rb,            &
   2.060448e-02_rb,1.921325e-02_rb,1.799699e-02_rb,1.692456e-02_rb,            &
   1.597177e-02_rb,1.511961e-02_rb,1.435289e-02_rb,1.365933e-02_rb,            &
   1.302890e-02_rb,1.245334e-02_rb,1.192576e-02_rb,1.144037e-02_rb,            &
   1.099230e-02_rb,1.057739e-02_rb,1.019208e-02_rb,9.833302e-03_rb,            &
   9.498395e-03_rb,9.185047e-03_rb,8.891237e-03_rb,8.615185e-03_rb,            &
   8.355325e-03_rb,8.110267e-03_rb,7.878778e-03_rb,7.659759e-03_rb,            &
   7.452224e-03_rb,7.255291e-03_rb,7.068166e-03_rb,6.890130e-03_rb,            &
   6.720536e-03_rb,6.558794e-03_rb,6.404371e-03_rb/)
!
! band 11
!
   absice2(:,11) = (/                                                          &
   1.656227e-01_rb,1.032129e-01_rb,7.487359e-02_rb,5.871431e-02_rb,            &
   4.828355e-02_rb,4.099989e-02_rb,3.562924e-02_rb,3.150755e-02_rb,            &
   2.824593e-02_rb,2.560156e-02_rb,2.341503e-02_rb,2.157740e-02_rb,            &
   2.001169e-02_rb,1.866199e-02_rb,1.748669e-02_rb,1.645421e-02_rb,            &
   1.554015e-02_rb,1.472535e-02_rb,1.399457e-02_rb,1.333553e-02_rb,            &
   1.273821e-02_rb,1.219440e-02_rb,1.169725e-02_rb,1.124104e-02_rb,            &
   1.082096e-02_rb,1.043290e-02_rb,1.007336e-02_rb,9.739338e-03_rb,            &
   9.428223e-03_rb,9.137756e-03_rb,8.865964e-03_rb,8.611115e-03_rb,            &
   8.371686e-03_rb,8.146330e-03_rb,7.933852e-03_rb,7.733187e-03_rb,            &
   7.543386e-03_rb,7.363597e-03_rb,7.193056e-03_rb,7.031072e-03_rb,            &
   6.877024e-03_rb,6.730348e-03_rb,6.590531e-03_rb/)
!
! band 12
!
   absice2(:,12) = (/                                                          &
   9.194591e-02_rb,6.446867e-02_rb,4.962034e-02_rb,4.042061e-02_rb,            &
   3.418456e-02_rb,2.968856e-02_rb,2.629900e-02_rb,2.365572e-02_rb,            &
   2.153915e-02_rb,1.980791e-02_rb,1.836689e-02_rb,1.714979e-02_rb,            &
   1.610900e-02_rb,1.520946e-02_rb,1.442476e-02_rb,1.373468e-02_rb,            &
   1.312345e-02_rb,1.257858e-02_rb,1.209010e-02_rb,1.164990e-02_rb,            &
   1.125136e-02_rb,1.088901e-02_rb,1.055827e-02_rb,1.025531e-02_rb,            &
   9.976896e-03_rb,9.720255e-03_rb,9.483022e-03_rb,9.263160e-03_rb,            &
   9.058902e-03_rb,8.868710e-03_rb,8.691240e-03_rb,8.525312e-03_rb,            &
   8.369886e-03_rb,8.224042e-03_rb,8.086961e-03_rb,7.957917e-03_rb,            &
   7.836258e-03_rb,7.721400e-03_rb,7.612821e-03_rb,7.510045e-03_rb,            &
   7.412648e-03_rb,7.320242e-03_rb,7.232476e-03_rb/)
!
! band 13
!
   absice2(:,13) = (/                                                          &
   1.437021e-01_rb,8.872535e-02_rb,6.392420e-02_rb,4.991833e-02_rb,            &
   4.096790e-02_rb,3.477881e-02_rb,3.025782e-02_rb,2.681909e-02_rb,            &
   2.412102e-02_rb,2.195132e-02_rb,2.017124e-02_rb,1.868641e-02_rb,            &
   1.743044e-02_rb,1.635529e-02_rb,1.542540e-02_rb,1.461388e-02_rb,            &
   1.390003e-02_rb,1.326766e-02_rb,1.270395e-02_rb,1.219860e-02_rb,            &
   1.174326e-02_rb,1.133107e-02_rb,1.095637e-02_rb,1.061442e-02_rb,            &
   1.030126e-02_rb,1.001352e-02_rb,9.748340e-03_rb,9.503256e-03_rb,            &
   9.276155e-03_rb,9.065205e-03_rb,8.868808e-03_rb,8.685571e-03_rb,            &
   8.514268e-03_rb,8.353820e-03_rb,8.203272e-03_rb,8.061776e-03_rb,            &
   7.928578e-03_rb,7.803001e-03_rb,7.684443e-03_rb,7.572358e-03_rb,            &
   7.466258e-03_rb,7.365701e-03_rb,7.270286e-03_rb/)
!
! band 14
!
   absice2(:,14) = (/                                                          &
   1.288870e-01_rb,8.160295e-02_rb,5.964745e-02_rb,4.703790e-02_rb,            &
   3.888637e-02_rb,3.320115e-02_rb,2.902017e-02_rb,2.582259e-02_rb,            &
   2.330224e-02_rb,2.126754e-02_rb,1.959258e-02_rb,1.819130e-02_rb,            &
   1.700289e-02_rb,1.598320e-02_rb,1.509942e-02_rb,1.432666e-02_rb,            &
   1.364572e-02_rb,1.304156e-02_rb,1.250220e-02_rb,1.201803e-02_rb,            &
   1.158123e-02_rb,1.118537e-02_rb,1.082513e-02_rb,1.049605e-02_rb,            &
   1.019440e-02_rb,9.916989e-03_rb,9.661116e-03_rb,9.424457e-03_rb,            &
   9.205005e-03_rb,9.001022e-03_rb,8.810992e-03_rb,8.633588e-03_rb,            &
   8.467646e-03_rb,8.312137e-03_rb,8.166151e-03_rb,8.028878e-03_rb,            &
   7.899597e-03_rb,7.777663e-03_rb,7.662498e-03_rb,7.553581e-03_rb,            &
   7.450444e-03_rb,7.352662e-03_rb,7.259851e-03_rb/)
!
! band 15
!
   absice2(:,15) = (/                                                          &
   8.254229e-02_rb,5.808787e-02_rb,4.492166e-02_rb,3.675028e-02_rb,            &
   3.119623e-02_rb,2.718045e-02_rb,2.414450e-02_rb,2.177073e-02_rb,            &
   1.986526e-02_rb,1.830306e-02_rb,1.699991e-02_rb,1.589698e-02_rb,            &
   1.495199e-02_rb,1.413374e-02_rb,1.341870e-02_rb,1.278883e-02_rb,            &
   1.223002e-02_rb,1.173114e-02_rb,1.128322e-02_rb,1.087900e-02_rb,            &
   1.051254e-02_rb,1.017890e-02_rb,9.873991e-03_rb,9.594347e-03_rb,            &
   9.337044e-03_rb,9.099589e-03_rb,8.879842e-03_rb,8.675960e-03_rb,            &
   8.486341e-03_rb,8.309594e-03_rb,8.144500e-03_rb,7.989986e-03_rb,            &
   7.845109e-03_rb,7.709031e-03_rb,7.581007e-03_rb,7.460376e-03_rb,            &
   7.346544e-03_rb,7.238978e-03_rb,7.137201e-03_rb,7.040780e-03_rb,            &
   6.949325e-03_rb,6.862483e-03_rb,6.779931e-03_rb/)
!
! band 16
!
   absice2(:,16) = (/                                                          &
   1.382062e-01_rb,8.643227e-02_rb,6.282935e-02_rb,4.934783e-02_rb,            &
   4.063891e-02_rb,3.455591e-02_rb,3.007059e-02_rb,2.662897e-02_rb,            &
   2.390631e-02_rb,2.169972e-02_rb,1.987596e-02_rb,1.834393e-02_rb,            &
   1.703924e-02_rb,1.591513e-02_rb,1.493679e-02_rb,1.407780e-02_rb,            &
   1.331775e-02_rb,1.264061e-02_rb,1.203364e-02_rb,1.148655e-02_rb,            &
   1.099099e-02_rb,1.054006e-02_rb,1.012807e-02_rb,9.750215e-03_rb,            &
   9.402477e-03_rb,9.081428e-03_rb,8.784143e-03_rb,8.508107e-03_rb,            &
   8.251146e-03_rb,8.011373e-03_rb,7.787140e-03_rb,7.577002e-03_rb,            &
   7.379687e-03_rb,7.194071e-03_rb,7.019158e-03_rb,6.854061e-03_rb,            &
   6.697986e-03_rb,6.550224e-03_rb,6.410138e-03_rb,6.277153e-03_rb,            &
   6.150751e-03_rb,6.030462e-03_rb,5.915860e-03_rb/)
!
! iceflag = 3; Fu parameterization. Particle size 5 - 140 micron in 
! increments of 3 microns.
! units = m2/g
! Hexagonal Ice Particle Parameterization
! absorption units (abs coef/iwc): [(m^-1)/(g m^-3)]
!
! band 1
!
   absice3(:,1) = (/                                                           &
   3.110649e-03_rb,4.666352e-02_rb,6.606447e-02_rb,6.531678e-02_rb,            &
   6.012598e-02_rb,5.437494e-02_rb,4.906411e-02_rb,4.441146e-02_rb,            &
   4.040585e-02_rb,3.697334e-02_rb,3.403027e-02_rb,3.149979e-02_rb,            &
   2.931596e-02_rb,2.742365e-02_rb,2.577721e-02_rb,2.433888e-02_rb,            &
   2.307732e-02_rb,2.196644e-02_rb,2.098437e-02_rb,2.011264e-02_rb,            &
   1.933561e-02_rb,1.863992e-02_rb,1.801407e-02_rb,1.744812e-02_rb,            &
   1.693346e-02_rb,1.646252e-02_rb,1.602866e-02_rb,1.562600e-02_rb,            &
   1.524933e-02_rb,1.489399e-02_rb,1.455580e-02_rb,1.423098e-02_rb,            &
   1.391612e-02_rb,1.360812e-02_rb,1.330413e-02_rb,1.300156e-02_rb,            &
   1.269801e-02_rb,1.239127e-02_rb,1.207928e-02_rb,1.176014e-02_rb,            &
   1.143204e-02_rb,1.109334e-02_rb,1.074243e-02_rb,1.037786e-02_rb,            &
   9.998198e-03_rb,9.602126e-03_rb/)
!
! band 2
!
   absice3(:,2) = (/                                                           &
   3.984966e-04_rb,1.681097e-02_rb,2.627680e-02_rb,2.767465e-02_rb,            &
   2.700722e-02_rb,2.579180e-02_rb,2.448677e-02_rb,2.323890e-02_rb,            &
   2.209096e-02_rb,2.104882e-02_rb,2.010547e-02_rb,1.925003e-02_rb,            &
   1.847128e-02_rb,1.775883e-02_rb,1.710358e-02_rb,1.649769e-02_rb,            &
   1.593449e-02_rb,1.540829e-02_rb,1.491429e-02_rb,1.444837e-02_rb,            &
   1.400704e-02_rb,1.358729e-02_rb,1.318654e-02_rb,1.280258e-02_rb,            &
   1.243346e-02_rb,1.207750e-02_rb,1.173325e-02_rb,1.139941e-02_rb,            &
   1.107487e-02_rb,1.075861e-02_rb,1.044975e-02_rb,1.014753e-02_rb,            &
   9.851229e-03_rb,9.560240e-03_rb,9.274003e-03_rb,8.992020e-03_rb,            &
   8.713845e-03_rb,8.439074e-03_rb,8.167346e-03_rb,7.898331e-03_rb,            &
   7.631734e-03_rb,7.367286e-03_rb,7.104742e-03_rb,6.843882e-03_rb,            &
   6.584504e-03_rb,6.326424e-03_rb/)
!
! band 3
!
   absice3(:,3) = (/                                                           &
   6.933163e-02_rb,8.540475e-02_rb,7.701816e-02_rb,6.771158e-02_rb,            &
   5.986953e-02_rb,5.348120e-02_rb,4.824962e-02_rb,4.390563e-02_rb,            &
   4.024411e-02_rb,3.711404e-02_rb,3.440426e-02_rb,3.203200e-02_rb,            &
   2.993478e-02_rb,2.806474e-02_rb,2.638464e-02_rb,2.486516e-02_rb,            &
   2.348288e-02_rb,2.221890e-02_rb,2.105780e-02_rb,1.998687e-02_rb,            &
   1.899552e-02_rb,1.807490e-02_rb,1.721750e-02_rb,1.641693e-02_rb,            &
   1.566773e-02_rb,1.496515e-02_rb,1.430509e-02_rb,1.368398e-02_rb,            &
   1.309865e-02_rb,1.254634e-02_rb,1.202456e-02_rb,1.153114e-02_rb,            &
   1.106409e-02_rb,1.062166e-02_rb,1.020224e-02_rb,9.804381e-03_rb,            &
   9.426771e-03_rb,9.068205e-03_rb,8.727578e-03_rb,8.403876e-03_rb,            &
   8.096160e-03_rb,7.803564e-03_rb,7.525281e-03_rb,7.260560e-03_rb,            &
   7.008697e-03_rb,6.769036e-03_rb/)
!
! band 4
!
   absice3(:,4) = (/                                                           &
   1.765735e-01_rb,1.382700e-01_rb,1.095129e-01_rb,8.987475e-02_rb,            &
   7.591185e-02_rb,6.554169e-02_rb,5.755500e-02_rb,5.122083e-02_rb,            &
   4.607610e-02_rb,4.181475e-02_rb,3.822697e-02_rb,3.516432e-02_rb,            &
   3.251897e-02_rb,3.021073e-02_rb,2.817876e-02_rb,2.637607e-02_rb,            &
   2.476582e-02_rb,2.331871e-02_rb,2.201113e-02_rb,2.082388e-02_rb,            &
   1.974115e-02_rb,1.874983e-02_rb,1.783894e-02_rb,1.699922e-02_rb,            &
   1.622280e-02_rb,1.550296e-02_rb,1.483390e-02_rb,1.421064e-02_rb,            &
   1.362880e-02_rb,1.308460e-02_rb,1.257468e-02_rb,1.209611e-02_rb,            &
   1.164628e-02_rb,1.122287e-02_rb,1.082381e-02_rb,1.044725e-02_rb,            &
   1.009154e-02_rb,9.755166e-03_rb,9.436783e-03_rb,9.135163e-03_rb,            &
   8.849193e-03_rb,8.577856e-03_rb,8.320225e-03_rb,8.075451e-03_rb,            &
   7.842755e-03_rb,7.621418e-03_rb/)
!
! band 5
!
   absice3(:,5) = (/                                                           &
   2.339673e-01_rb,1.692124e-01_rb,1.291656e-01_rb,1.033837e-01_rb,            &
   8.562949e-02_rb,7.273526e-02_rb,6.298262e-02_rb,5.537015e-02_rb,            &
   4.927787e-02_rb,4.430246e-02_rb,4.017061e-02_rb,3.669072e-02_rb,            &
   3.372455e-02_rb,3.116995e-02_rb,2.894977e-02_rb,2.700471e-02_rb,            &
   2.528842e-02_rb,2.376420e-02_rb,2.240256e-02_rb,2.117959e-02_rb,            &
   2.007567e-02_rb,1.907456e-02_rb,1.816271e-02_rb,1.732874e-02_rb,            &
   1.656300e-02_rb,1.585725e-02_rb,1.520445e-02_rb,1.459852e-02_rb,            &
   1.403419e-02_rb,1.350689e-02_rb,1.301260e-02_rb,1.254781e-02_rb,            &
   1.210941e-02_rb,1.169468e-02_rb,1.130118e-02_rb,1.092675e-02_rb,            &
   1.056945e-02_rb,1.022757e-02_rb,9.899560e-03_rb,9.584021e-03_rb,            &
   9.279705e-03_rb,8.985479e-03_rb,8.700322e-03_rb,8.423306e-03_rb,            &
   8.153590e-03_rb,7.890412e-03_rb/)
!
! band 6
!
   absice3(:,6) = (/                                                           &
   1.145369e-01_rb,1.174566e-01_rb,9.917866e-02_rb,8.332990e-02_rb,            &
   7.104263e-02_rb,6.153370e-02_rb,5.405472e-02_rb,4.806281e-02_rb,            &
   4.317918e-02_rb,3.913795e-02_rb,3.574916e-02_rb,3.287437e-02_rb,            &
   3.041067e-02_rb,2.828017e-02_rb,2.642292e-02_rb,2.479206e-02_rb,            &
   2.335051e-02_rb,2.206851e-02_rb,2.092195e-02_rb,1.989108e-02_rb,            &
   1.895958e-02_rb,1.811385e-02_rb,1.734245e-02_rb,1.663573e-02_rb,            &
   1.598545e-02_rb,1.538456e-02_rb,1.482700e-02_rb,1.430750e-02_rb,            &
   1.382150e-02_rb,1.336499e-02_rb,1.293447e-02_rb,1.252685e-02_rb,            &
   1.213939e-02_rb,1.176968e-02_rb,1.141555e-02_rb,1.107508e-02_rb,            &
   1.074655e-02_rb,1.042839e-02_rb,1.011923e-02_rb,9.817799e-03_rb,            &
   9.522962e-03_rb,9.233688e-03_rb,8.949041e-03_rb,8.668171e-03_rb,            &
   8.390301e-03_rb,8.114723e-03_rb/)
!
! band 7
!
   absice3(:,7) = (/                                                           &
   1.222345e-02_rb,5.344230e-02_rb,5.523465e-02_rb,5.128759e-02_rb,            &
   4.676925e-02_rb,4.266150e-02_rb,3.910561e-02_rb,3.605479e-02_rb,            &
   3.342843e-02_rb,3.115052e-02_rb,2.915776e-02_rb,2.739935e-02_rb,            &
   2.583499e-02_rb,2.443266e-02_rb,2.316681e-02_rb,2.201687e-02_rb,            &
   2.096619e-02_rb,2.000112e-02_rb,1.911044e-02_rb,1.828481e-02_rb,            &
   1.751641e-02_rb,1.679866e-02_rb,1.612598e-02_rb,1.549360e-02_rb,            &
   1.489742e-02_rb,1.433392e-02_rb,1.380002e-02_rb,1.329305e-02_rb,            &
   1.281068e-02_rb,1.235084e-02_rb,1.191172e-02_rb,1.149171e-02_rb,            &
   1.108936e-02_rb,1.070341e-02_rb,1.033271e-02_rb,9.976220e-03_rb,            &
   9.633021e-03_rb,9.302273e-03_rb,8.983216e-03_rb,8.675161e-03_rb,            &
   8.377478e-03_rb,8.089595e-03_rb,7.810986e-03_rb,7.541170e-03_rb,            &
   7.279706e-03_rb,7.026186e-03_rb/)
!
! band 8
!
   absice3(:,8) = (/                                                           &
   6.711058e-02_rb,6.918198e-02_rb,6.127484e-02_rb,5.411944e-02_rb,            &
   4.836902e-02_rb,4.375293e-02_rb,3.998077e-02_rb,3.683587e-02_rb,            &
   3.416508e-02_rb,3.186003e-02_rb,2.984290e-02_rb,2.805671e-02_rb,            &
   2.645895e-02_rb,2.501733e-02_rb,2.370689e-02_rb,2.250808e-02_rb,            &
   2.140532e-02_rb,2.038609e-02_rb,1.944018e-02_rb,1.855918e-02_rb,            &
   1.773609e-02_rb,1.696504e-02_rb,1.624106e-02_rb,1.555990e-02_rb,            &
   1.491793e-02_rb,1.431197e-02_rb,1.373928e-02_rb,1.319743e-02_rb,            &
   1.268430e-02_rb,1.219799e-02_rb,1.173682e-02_rb,1.129925e-02_rb,            &
   1.088393e-02_rb,1.048961e-02_rb,1.011516e-02_rb,9.759543e-03_rb,            &
   9.421813e-03_rb,9.101089e-03_rb,8.796559e-03_rb,8.507464e-03_rb,            &
   8.233098e-03_rb,7.972798e-03_rb,7.725942e-03_rb,7.491940e-03_rb,            &
   7.270238e-03_rb,7.060305e-03_rb/)
!
! band 9
!
   absice3(:,9) = (/                                                           &
   1.236780e-01_rb,9.222386e-02_rb,7.383997e-02_rb,6.204072e-02_rb,            &
   5.381029e-02_rb,4.770678e-02_rb,4.296928e-02_rb,3.916131e-02_rb,            &
   3.601540e-02_rb,3.335878e-02_rb,3.107493e-02_rb,2.908247e-02_rb,            &
   2.732282e-02_rb,2.575276e-02_rb,2.433968e-02_rb,2.305852e-02_rb,            &
   2.188966e-02_rb,2.081757e-02_rb,1.982974e-02_rb,1.891599e-02_rb,            &
   1.806794e-02_rb,1.727865e-02_rb,1.654227e-02_rb,1.585387e-02_rb,            &
   1.520924e-02_rb,1.460476e-02_rb,1.403730e-02_rb,1.350416e-02_rb,            &
   1.300293e-02_rb,1.253153e-02_rb,1.208808e-02_rb,1.167094e-02_rb,            &
   1.127862e-02_rb,1.090979e-02_rb,1.056323e-02_rb,1.023786e-02_rb,            &
   9.932665e-03_rb,9.646744e-03_rb,9.379250e-03_rb,9.129409e-03_rb,            &
   8.896500e-03_rb,8.679856e-03_rb,8.478852e-03_rb,8.292904e-03_rb,            &
   8.121463e-03_rb,7.964013e-03_rb/)
!
! band 10
!
   absice3(:,10) = (/                                                          &
   1.655966e-01_rb,1.134205e-01_rb,8.714344e-02_rb,7.129241e-02_rb,            &
   6.063739e-02_rb,5.294203e-02_rb,4.709309e-02_rb,4.247476e-02_rb,            &
   3.871892e-02_rb,3.559206e-02_rb,3.293893e-02_rb,3.065226e-02_rb,            &
   2.865558e-02_rb,2.689288e-02_rb,2.532221e-02_rb,2.391150e-02_rb,            &
   2.263582e-02_rb,2.147549e-02_rb,2.041476e-02_rb,1.944089e-02_rb,            &
   1.854342e-02_rb,1.771371e-02_rb,1.694456e-02_rb,1.622989e-02_rb,            &
   1.556456e-02_rb,1.494415e-02_rb,1.436491e-02_rb,1.382354e-02_rb,            &
   1.331719e-02_rb,1.284339e-02_rb,1.239992e-02_rb,1.198486e-02_rb,            &
   1.159647e-02_rb,1.123323e-02_rb,1.089375e-02_rb,1.057679e-02_rb,            &
   1.028124e-02_rb,1.000607e-02_rb,9.750376e-03_rb,9.513303e-03_rb,            &
   9.294082e-03_rb,9.092003e-03_rb,8.906412e-03_rb,8.736702e-03_rb,            &
   8.582314e-03_rb,8.442725e-03_rb/)
!
! band 11
!
   absice3(:,11) = (/                                                          &
   1.775615e-01_rb,1.180046e-01_rb,8.929607e-02_rb,7.233500e-02_rb,            &
   6.108333e-02_rb,5.303642e-02_rb,4.696927e-02_rb,4.221206e-02_rb,            &
   3.836768e-02_rb,3.518576e-02_rb,3.250063e-02_rb,3.019825e-02_rb,            &
   2.819758e-02_rb,2.643943e-02_rb,2.487953e-02_rb,2.348414e-02_rb,            &
   2.222705e-02_rb,2.108762e-02_rb,2.004936e-02_rb,1.909892e-02_rb,            &
   1.822539e-02_rb,1.741975e-02_rb,1.667449e-02_rb,1.598330e-02_rb,            &
   1.534084e-02_rb,1.474253e-02_rb,1.418446e-02_rb,1.366325e-02_rb,            &
   1.317597e-02_rb,1.272004e-02_rb,1.229321e-02_rb,1.189350e-02_rb,            &
   1.151915e-02_rb,1.116859e-02_rb,1.084042e-02_rb,1.053338e-02_rb,            &
   1.024636e-02_rb,9.978326e-03_rb,9.728357e-03_rb,9.495613e-03_rb,            &
   9.279327e-03_rb,9.078798e-03_rb,8.893383e-03_rb,8.722488e-03_rb,            &
   8.565568e-03_rb,8.422115e-03_rb/)
!
! band 12
!
   absice3(:,12) = (/                                                          &
   9.465447e-02_rb,6.432047e-02_rb,5.060973e-02_rb,4.267283e-02_rb,            &
   3.741843e-02_rb,3.363096e-02_rb,3.073531e-02_rb,2.842405e-02_rb,            &
   2.651789e-02_rb,2.490518e-02_rb,2.351273e-02_rb,2.229056e-02_rb,            &
   2.120335e-02_rb,2.022541e-02_rb,1.933763e-02_rb,1.852546e-02_rb,            &
   1.777763e-02_rb,1.708528e-02_rb,1.644134e-02_rb,1.584009e-02_rb,            &
   1.527684e-02_rb,1.474774e-02_rb,1.424955e-02_rb,1.377957e-02_rb,            &
   1.333549e-02_rb,1.291534e-02_rb,1.251743e-02_rb,1.214029e-02_rb,            &
   1.178265e-02_rb,1.144337e-02_rb,1.112148e-02_rb,1.081609e-02_rb,            &
   1.052642e-02_rb,1.025178e-02_rb,9.991540e-03_rb,9.745130e-03_rb,            &
   9.512038e-03_rb,9.291797e-03_rb,9.083980e-03_rb,8.888195e-03_rb,            &
   8.704081e-03_rb,8.531306e-03_rb,8.369560e-03_rb,8.218558e-03_rb,            &
   8.078032e-03_rb,7.947730e-03_rb/)
!
! band 13
!
   absice3(:,13) = (/                                                          &
   1.560311e-01_rb,9.961097e-02_rb,7.502949e-02_rb,6.115022e-02_rb,            &
   5.214952e-02_rb,4.578149e-02_rb,4.099731e-02_rb,3.724174e-02_rb,            &
   3.419343e-02_rb,3.165356e-02_rb,2.949251e-02_rb,2.762222e-02_rb,            &
   2.598073e-02_rb,2.452322e-02_rb,2.321642e-02_rb,2.203516e-02_rb,            &
   2.096002e-02_rb,1.997579e-02_rb,1.907036e-02_rb,1.823401e-02_rb,            &
   1.745879e-02_rb,1.673819e-02_rb,1.606678e-02_rb,1.544003e-02_rb,            &
   1.485411e-02_rb,1.430574e-02_rb,1.379215e-02_rb,1.331092e-02_rb,            &
   1.285996e-02_rb,1.243746e-02_rb,1.204183e-02_rb,1.167164e-02_rb,            &
   1.132567e-02_rb,1.100281e-02_rb,1.070207e-02_rb,1.042258e-02_rb,            &
   1.016352e-02_rb,9.924197e-03_rb,9.703953e-03_rb,9.502199e-03_rb,            &
   9.318400e-03_rb,9.152066e-03_rb,9.002749e-03_rb,8.870038e-03_rb,            &
   8.753555e-03_rb,8.652951e-03_rb/)
!
! band 14
!
   absice3(:,14) = (/                                                          &
   1.559547e-01_rb,9.896700e-02_rb,7.441231e-02_rb,6.061469e-02_rb,            &
   5.168730e-02_rb,4.537821e-02_rb,4.064106e-02_rb,3.692367e-02_rb,            &
   3.390714e-02_rb,3.139438e-02_rb,2.925702e-02_rb,2.740783e-02_rb,            &
   2.578547e-02_rb,2.434552e-02_rb,2.305506e-02_rb,2.188910e-02_rb,            &
   2.082842e-02_rb,1.985789e-02_rb,1.896553e-02_rb,1.814165e-02_rb,            &
   1.737839e-02_rb,1.666927e-02_rb,1.600891e-02_rb,1.539279e-02_rb,            &
   1.481712e-02_rb,1.427865e-02_rb,1.377463e-02_rb,1.330266e-02_rb,            &
   1.286068e-02_rb,1.244689e-02_rb,1.205973e-02_rb,1.169780e-02_rb,            &
   1.135989e-02_rb,1.104492e-02_rb,1.075192e-02_rb,1.048004e-02_rb,            &
   1.022850e-02_rb,9.996611e-03_rb,9.783753e-03_rb,9.589361e-03_rb,            &
   9.412924e-03_rb,9.253977e-03_rb,9.112098e-03_rb,8.986903e-03_rb,            &
   8.878039e-03_rb,8.785184e-03_rb/)
!
! band 15
!
   absice3(:,15) = (/                                                          &
   1.102926e-01_rb,7.176622e-02_rb,5.530316e-02_rb,4.606056e-02_rb,            &
   4.006116e-02_rb,3.579628e-02_rb,3.256909e-02_rb,3.001360e-02_rb,            &
   2.791920e-02_rb,2.615617e-02_rb,2.464023e-02_rb,2.331426e-02_rb,            &
   2.213817e-02_rb,2.108301e-02_rb,2.012733e-02_rb,1.925493e-02_rb,            &
   1.845331e-02_rb,1.771269e-02_rb,1.702531e-02_rb,1.638493e-02_rb,            &
   1.578648e-02_rb,1.522579e-02_rb,1.469940e-02_rb,1.420442e-02_rb,            &
   1.373841e-02_rb,1.329931e-02_rb,1.288535e-02_rb,1.249502e-02_rb,            &
   1.212700e-02_rb,1.178015e-02_rb,1.145348e-02_rb,1.114612e-02_rb,            &
   1.085730e-02_rb,1.058633e-02_rb,1.033263e-02_rb,1.009564e-02_rb,            &
   9.874895e-03_rb,9.669960e-03_rb,9.480449e-03_rb,9.306014e-03_rb,            &
   9.146339e-03_rb,9.001138e-03_rb,8.870154e-03_rb,8.753148e-03_rb,            &
   8.649907e-03_rb,8.560232e-03_rb/)
!
! band 16
!
   absice3(:,16) = (/                                                          &
   1.688344e-01_rb,1.077072e-01_rb,7.994467e-02_rb,6.403862e-02_rb,            &
   5.369850e-02_rb,4.641582e-02_rb,4.099331e-02_rb,3.678724e-02_rb,            &
   3.342069e-02_rb,3.065831e-02_rb,2.834557e-02_rb,2.637680e-02_rb,            &
   2.467733e-02_rb,2.319286e-02_rb,2.188299e-02_rb,2.071701e-02_rb,            &
   1.967121e-02_rb,1.872692e-02_rb,1.786931e-02_rb,1.708641e-02_rb,            &
   1.636846e-02_rb,1.570743e-02_rb,1.509665e-02_rb,1.453052e-02_rb,            &
   1.400433e-02_rb,1.351407e-02_rb,1.305631e-02_rb,1.262810e-02_rb,            &
   1.222688e-02_rb,1.185044e-02_rb,1.149683e-02_rb,1.116436e-02_rb,            &
   1.085153e-02_rb,1.055701e-02_rb,1.027961e-02_rb,1.001831e-02_rb,            &
   9.772141e-03_rb,9.540280e-03_rb,9.321966e-03_rb,9.116517e-03_rb,            &
   8.923315e-03_rb,8.741803e-03_rb,8.571472e-03_rb,8.411860e-03_rb,            &
   8.262543e-03_rb,8.123136e-03_rb/)
!
! For liqflag = 0.
!
   absliq0 = 0.0903614_rb
!
! For liqflag = 1.  In each band, the absorption
! coefficients are listed for a range of effective radii from 2.5
! to 59.5 microns in increments of 1.0 micron.
!
! band  1
!
   absliq1(:, 1) = (/                                                          &
   1.64047e-03_rb,6.90533e-02_rb,7.72017e-02_rb,7.78054e-02_rb,7.69523e-02_rb, &
   7.58058e-02_rb,7.46400e-02_rb,7.35123e-02_rb,7.24162e-02_rb,7.13225e-02_rb, &
   6.99145e-02_rb,6.66409e-02_rb,6.36582e-02_rb,6.09425e-02_rb,5.84593e-02_rb, &
   5.61743e-02_rb,5.40571e-02_rb,5.20812e-02_rb,5.02245e-02_rb,4.84680e-02_rb, &
   4.67959e-02_rb,4.51944e-02_rb,4.36516e-02_rb,4.21570e-02_rb,4.07015e-02_rb, &
   3.92766e-02_rb,3.78747e-02_rb,3.64886e-02_rb,3.53632e-02_rb,3.41992e-02_rb, &
   3.31016e-02_rb,3.20643e-02_rb,3.10817e-02_rb,3.01490e-02_rb,2.92620e-02_rb, &
   2.84171e-02_rb,2.76108e-02_rb,2.68404e-02_rb,2.61031e-02_rb,2.53966e-02_rb, &
   2.47189e-02_rb,2.40678e-02_rb,2.34418e-02_rb,2.28392e-02_rb,2.22586e-02_rb, &
   2.16986e-02_rb,2.11580e-02_rb,2.06356e-02_rb,2.01305e-02_rb,1.96417e-02_rb, &
   1.91682e-02_rb,1.87094e-02_rb,1.82643e-02_rb,1.78324e-02_rb,1.74129e-02_rb, &
   1.70052e-02_rb,1.66088e-02_rb,1.62231e-02_rb/)
!
! band  2
!
   absliq1(:, 2) = (/                                                          &
   2.19486e-01_rb,1.80687e-01_rb,1.59150e-01_rb,1.44731e-01_rb,1.33703e-01_rb, &
   1.24355e-01_rb,1.15756e-01_rb,1.07318e-01_rb,9.86119e-02_rb,8.92739e-02_rb, &
   8.34911e-02_rb,7.70773e-02_rb,7.15240e-02_rb,6.66615e-02_rb,6.23641e-02_rb, &
   5.85359e-02_rb,5.51020e-02_rb,5.20032e-02_rb,4.91916e-02_rb,4.66283e-02_rb, &
   4.42813e-02_rb,4.21236e-02_rb,4.01330e-02_rb,3.82905e-02_rb,3.65797e-02_rb, &
   3.49869e-02_rb,3.35002e-02_rb,3.21090e-02_rb,3.08957e-02_rb,2.97601e-02_rb, &
   2.86966e-02_rb,2.76984e-02_rb,2.67599e-02_rb,2.58758e-02_rb,2.50416e-02_rb, &
   2.42532e-02_rb,2.35070e-02_rb,2.27997e-02_rb,2.21284e-02_rb,2.14904e-02_rb, &
   2.08834e-02_rb,2.03051e-02_rb,1.97536e-02_rb,1.92271e-02_rb,1.87239e-02_rb, &
   1.82425e-02_rb,1.77816e-02_rb,1.73399e-02_rb,1.69162e-02_rb,1.65094e-02_rb, &
   1.61187e-02_rb,1.57430e-02_rb,1.53815e-02_rb,1.50334e-02_rb,1.46981e-02_rb, &
   1.43748e-02_rb,1.40628e-02_rb,1.37617e-02_rb/)
!
! band  3
!
   absliq1(:, 3) = (/                                                          &
   2.95174e-01_rb,2.34765e-01_rb,1.98038e-01_rb,1.72114e-01_rb,1.52083e-01_rb, &
   1.35654e-01_rb,1.21613e-01_rb,1.09252e-01_rb,9.81263e-02_rb,8.79448e-02_rb, &
   8.12566e-02_rb,7.44563e-02_rb,6.86374e-02_rb,6.36042e-02_rb,5.92094e-02_rb, &
   5.53402e-02_rb,5.19087e-02_rb,4.88455e-02_rb,4.60951e-02_rb,4.36124e-02_rb, &
   4.13607e-02_rb,3.93096e-02_rb,3.74338e-02_rb,3.57119e-02_rb,3.41261e-02_rb, &
   3.26610e-02_rb,3.13036e-02_rb,3.00425e-02_rb,2.88497e-02_rb,2.78077e-02_rb, &
   2.68317e-02_rb,2.59158e-02_rb,2.50545e-02_rb,2.42430e-02_rb,2.34772e-02_rb, &
   2.27533e-02_rb,2.20679e-02_rb,2.14181e-02_rb,2.08011e-02_rb,2.02145e-02_rb, &
   1.96561e-02_rb,1.91239e-02_rb,1.86161e-02_rb,1.81311e-02_rb,1.76673e-02_rb, &
   1.72234e-02_rb,1.67981e-02_rb,1.63903e-02_rb,1.59989e-02_rb,1.56230e-02_rb, &
   1.52615e-02_rb,1.49138e-02_rb,1.45791e-02_rb,1.42565e-02_rb,1.39455e-02_rb, &
   1.36455e-02_rb,1.33559e-02_rb,1.30761e-02_rb/)
!
! band  4
!
   absliq1(:, 4) = (/                                                          &
   3.00925e-01_rb,2.36949e-01_rb,1.96947e-01_rb,1.68692e-01_rb,1.47190e-01_rb, &
   1.29986e-01_rb,1.15719e-01_rb,1.03568e-01_rb,9.30028e-02_rb,8.36658e-02_rb, &
   7.71075e-02_rb,7.07002e-02_rb,6.52284e-02_rb,6.05024e-02_rb,5.63801e-02_rb, &
   5.27534e-02_rb,4.95384e-02_rb,4.66690e-02_rb,4.40925e-02_rb,4.17664e-02_rb, &
   3.96559e-02_rb,3.77326e-02_rb,3.59727e-02_rb,3.43561e-02_rb,3.28662e-02_rb, &
   3.14885e-02_rb,3.02110e-02_rb,2.90231e-02_rb,2.78948e-02_rb,2.69109e-02_rb, &
   2.59884e-02_rb,2.51217e-02_rb,2.43058e-02_rb,2.35364e-02_rb,2.28096e-02_rb, &
   2.21218e-02_rb,2.14700e-02_rb,2.08515e-02_rb,2.02636e-02_rb,1.97041e-02_rb, &
   1.91711e-02_rb,1.86625e-02_rb,1.81769e-02_rb,1.77126e-02_rb,1.72683e-02_rb, &
   1.68426e-02_rb,1.64344e-02_rb,1.60427e-02_rb,1.56664e-02_rb,1.53046e-02_rb, &
   1.49565e-02_rb,1.46214e-02_rb,1.42985e-02_rb,1.39871e-02_rb,1.36866e-02_rb, &
   1.33965e-02_rb,1.31162e-02_rb,1.28453e-02_rb/)
!
! band  5
!
   absliq1(:, 5) = (/                                                          &
   2.64691e-01_rb,2.12018e-01_rb,1.78009e-01_rb,1.53539e-01_rb,1.34721e-01_rb, &
   1.19580e-01_rb,1.06996e-01_rb,9.62772e-02_rb,8.69710e-02_rb,7.87670e-02_rb, &
   7.29272e-02_rb,6.70920e-02_rb,6.20977e-02_rb,5.77732e-02_rb,5.39910e-02_rb, &
   5.06538e-02_rb,4.76866e-02_rb,4.50301e-02_rb,4.26374e-02_rb,4.04704e-02_rb, &
   3.84981e-02_rb,3.66948e-02_rb,3.50394e-02_rb,3.35141e-02_rb,3.21038e-02_rb, &
   3.07957e-02_rb,2.95788e-02_rb,2.84438e-02_rb,2.73790e-02_rb,2.64390e-02_rb, &
   2.55565e-02_rb,2.47263e-02_rb,2.39437e-02_rb,2.32047e-02_rb,2.25056e-02_rb, &
   2.18433e-02_rb,2.12149e-02_rb,2.06177e-02_rb,2.00495e-02_rb,1.95081e-02_rb, &
   1.89917e-02_rb,1.84984e-02_rb,1.80269e-02_rb,1.75755e-02_rb,1.71431e-02_rb, &
   1.67283e-02_rb,1.63303e-02_rb,1.59478e-02_rb,1.55801e-02_rb,1.52262e-02_rb, &
   1.48853e-02_rb,1.45568e-02_rb,1.42400e-02_rb,1.39342e-02_rb,1.36388e-02_rb, &
   1.33533e-02_rb,1.30773e-02_rb,1.28102e-02_rb/)
!
! band  6
!
   absliq1(:, 6) = (/                                                          &
   8.81182e-02_rb,1.06745e-01_rb,9.79753e-02_rb,8.99625e-02_rb,8.35200e-02_rb, &
   7.81899e-02_rb,7.35939e-02_rb,6.94696e-02_rb,6.56266e-02_rb,6.19148e-02_rb, &
   5.83355e-02_rb,5.49306e-02_rb,5.19642e-02_rb,4.93325e-02_rb,4.69659e-02_rb, &
   4.48148e-02_rb,4.28431e-02_rb,4.10231e-02_rb,3.93332e-02_rb,3.77563e-02_rb, &
   3.62785e-02_rb,3.48882e-02_rb,3.35758e-02_rb,3.23333e-02_rb,3.11536e-02_rb, &
   3.00310e-02_rb,2.89601e-02_rb,2.79365e-02_rb,2.70502e-02_rb,2.62618e-02_rb, &
   2.55025e-02_rb,2.47728e-02_rb,2.40726e-02_rb,2.34013e-02_rb,2.27583e-02_rb, &
   2.21422e-02_rb,2.15522e-02_rb,2.09869e-02_rb,2.04453e-02_rb,1.99260e-02_rb, &
   1.94280e-02_rb,1.89501e-02_rb,1.84913e-02_rb,1.80506e-02_rb,1.76270e-02_rb, &
   1.72196e-02_rb,1.68276e-02_rb,1.64500e-02_rb,1.60863e-02_rb,1.57357e-02_rb, &
   1.53975e-02_rb,1.50710e-02_rb,1.47558e-02_rb,1.44511e-02_rb,1.41566e-02_rb, &
   1.38717e-02_rb,1.35960e-02_rb,1.33290e-02_rb/)
! band  7
!
   absliq1(:, 7) = (/                                                          &
   4.32174e-02_rb,7.36078e-02_rb,6.98340e-02_rb,6.65231e-02_rb,6.41948e-02_rb, &
   6.23551e-02_rb,6.06638e-02_rb,5.88680e-02_rb,5.67124e-02_rb,5.38629e-02_rb, &
   4.99579e-02_rb,4.86289e-02_rb,4.70120e-02_rb,4.52854e-02_rb,4.35466e-02_rb, &
   4.18480e-02_rb,4.02169e-02_rb,3.86658e-02_rb,3.71992e-02_rb,3.58168e-02_rb, &
   3.45155e-02_rb,3.32912e-02_rb,3.21390e-02_rb,3.10538e-02_rb,3.00307e-02_rb, &
   2.90651e-02_rb,2.81524e-02_rb,2.72885e-02_rb,2.62821e-02_rb,2.55744e-02_rb, &
   2.48799e-02_rb,2.42029e-02_rb,2.35460e-02_rb,2.29108e-02_rb,2.22981e-02_rb, &
   2.17079e-02_rb,2.11402e-02_rb,2.05945e-02_rb,2.00701e-02_rb,1.95663e-02_rb, &
   1.90824e-02_rb,1.86174e-02_rb,1.81706e-02_rb,1.77411e-02_rb,1.73281e-02_rb, &
   1.69307e-02_rb,1.65483e-02_rb,1.61801e-02_rb,1.58254e-02_rb,1.54835e-02_rb, &
   1.51538e-02_rb,1.48358e-02_rb,1.45288e-02_rb,1.42322e-02_rb,1.39457e-02_rb, &
   1.36687e-02_rb,1.34008e-02_rb,1.31416e-02_rb/)
!
! band  8
!
   absliq1(:, 8) = (/                                                          &
   1.41881e-01_rb,7.15419e-02_rb,6.30335e-02_rb,6.11132e-02_rb,6.01931e-02_rb, &
   5.92420e-02_rb,5.78968e-02_rb,5.58876e-02_rb,5.28923e-02_rb,4.84462e-02_rb, &
   4.60839e-02_rb,4.56013e-02_rb,4.45410e-02_rb,4.31866e-02_rb,4.17026e-02_rb, &
   4.01850e-02_rb,3.86892e-02_rb,3.72461e-02_rb,3.58722e-02_rb,3.45749e-02_rb, &
   3.33564e-02_rb,3.22155e-02_rb,3.11494e-02_rb,3.01541e-02_rb,2.92253e-02_rb, &
   2.83584e-02_rb,2.75488e-02_rb,2.67925e-02_rb,2.57692e-02_rb,2.50704e-02_rb, &
   2.43918e-02_rb,2.37350e-02_rb,2.31005e-02_rb,2.24888e-02_rb,2.18996e-02_rb, &
   2.13325e-02_rb,2.07870e-02_rb,2.02623e-02_rb,1.97577e-02_rb,1.92724e-02_rb, &
   1.88056e-02_rb,1.83564e-02_rb,1.79241e-02_rb,1.75079e-02_rb,1.71070e-02_rb, &
   1.67207e-02_rb,1.63482e-02_rb,1.59890e-02_rb,1.56424e-02_rb,1.53077e-02_rb, &
   1.49845e-02_rb,1.46722e-02_rb,1.43702e-02_rb,1.40782e-02_rb,1.37955e-02_rb, &
   1.35219e-02_rb,1.32569e-02_rb,1.30000e-02_rb/)
!
! band  9
!
   absliq1(:, 9) = (/                                                          &
   6.72726e-02_rb,6.61013e-02_rb,6.47866e-02_rb,6.33780e-02_rb,6.18985e-02_rb, &
   6.03335e-02_rb,5.86136e-02_rb,5.65876e-02_rb,5.39839e-02_rb,5.03536e-02_rb, &
   4.71608e-02_rb,4.63630e-02_rb,4.50313e-02_rb,4.34526e-02_rb,4.17876e-02_rb, &
   4.01261e-02_rb,3.85171e-02_rb,3.69860e-02_rb,3.55442e-02_rb,3.41954e-02_rb, &
   3.29384e-02_rb,3.17693e-02_rb,3.06832e-02_rb,2.96745e-02_rb,2.87374e-02_rb, &
   2.78662e-02_rb,2.70557e-02_rb,2.63008e-02_rb,2.52450e-02_rb,2.45424e-02_rb, &
   2.38656e-02_rb,2.32144e-02_rb,2.25885e-02_rb,2.19873e-02_rb,2.14099e-02_rb, &
   2.08554e-02_rb,2.03230e-02_rb,1.98116e-02_rb,1.93203e-02_rb,1.88482e-02_rb, &
   1.83944e-02_rb,1.79578e-02_rb,1.75378e-02_rb,1.71335e-02_rb,1.67440e-02_rb, &
   1.63687e-02_rb,1.60069e-02_rb,1.56579e-02_rb,1.53210e-02_rb,1.49958e-02_rb, &
   1.46815e-02_rb,1.43778e-02_rb,1.40841e-02_rb,1.37999e-02_rb,1.35249e-02_rb, &
   1.32585e-02_rb,1.30004e-02_rb,1.27502e-02_rb/)
!
! band 10
!
   absliq1(:,10) = (/                                                          &
   7.97040e-02_rb,7.63844e-02_rb,7.36499e-02_rb,7.13525e-02_rb,6.93043e-02_rb, &
   6.72807e-02_rb,6.50227e-02_rb,6.22395e-02_rb,5.86093e-02_rb,5.37815e-02_rb, &
   5.14682e-02_rb,4.97214e-02_rb,4.77392e-02_rb,4.56961e-02_rb,4.36858e-02_rb, &
   4.17569e-02_rb,3.99328e-02_rb,3.82224e-02_rb,3.66265e-02_rb,3.51416e-02_rb, &
   3.37617e-02_rb,3.24798e-02_rb,3.12887e-02_rb,3.01812e-02_rb,2.91505e-02_rb, &
   2.81900e-02_rb,2.72939e-02_rb,2.64568e-02_rb,2.54165e-02_rb,2.46832e-02_rb, &
   2.39783e-02_rb,2.33017e-02_rb,2.26531e-02_rb,2.20314e-02_rb,2.14359e-02_rb, &
   2.08653e-02_rb,2.03187e-02_rb,1.97947e-02_rb,1.92924e-02_rb,1.88106e-02_rb, &
   1.83483e-02_rb,1.79043e-02_rb,1.74778e-02_rb,1.70678e-02_rb,1.66735e-02_rb, &
   1.62941e-02_rb,1.59286e-02_rb,1.55766e-02_rb,1.52371e-02_rb,1.49097e-02_rb, &
   1.45937e-02_rb,1.42885e-02_rb,1.39936e-02_rb,1.37085e-02_rb,1.34327e-02_rb, &
   1.31659e-02_rb,1.29075e-02_rb,1.26571e-02_rb/)
!
! band 11
!
   absliq1(:,11) = (/                                                          &
   1.49438e-01_rb,1.33535e-01_rb,1.21542e-01_rb,1.11743e-01_rb,1.03263e-01_rb, &
   9.55774e-02_rb,8.83382e-02_rb,8.12943e-02_rb,7.42533e-02_rb,6.70609e-02_rb, &
   6.38761e-02_rb,5.97788e-02_rb,5.59841e-02_rb,5.25318e-02_rb,4.94132e-02_rb, &
   4.66014e-02_rb,4.40644e-02_rb,4.17706e-02_rb,3.96910e-02_rb,3.77998e-02_rb, &
   3.60742e-02_rb,3.44947e-02_rb,3.30442e-02_rb,3.17079e-02_rb,3.04730e-02_rb, &
   2.93283e-02_rb,2.82642e-02_rb,2.72720e-02_rb,2.61789e-02_rb,2.53277e-02_rb, &
   2.45237e-02_rb,2.37635e-02_rb,2.30438e-02_rb,2.23615e-02_rb,2.17140e-02_rb, &
   2.10987e-02_rb,2.05133e-02_rb,1.99557e-02_rb,1.94241e-02_rb,1.89166e-02_rb, &
   1.84317e-02_rb,1.79679e-02_rb,1.75238e-02_rb,1.70983e-02_rb,1.66901e-02_rb, &
   1.62983e-02_rb,1.59219e-02_rb,1.55599e-02_rb,1.52115e-02_rb,1.48761e-02_rb, &
   1.45528e-02_rb,1.42411e-02_rb,1.39402e-02_rb,1.36497e-02_rb,1.33690e-02_rb, &
   1.30976e-02_rb,1.28351e-02_rb,1.25810e-02_rb/)
!
! band 12
!
   absliq1(:,12) = (/                                                          &
   3.71985e-02_rb,3.88586e-02_rb,3.99070e-02_rb,4.04351e-02_rb,4.04610e-02_rb, &
   3.99834e-02_rb,3.89953e-02_rb,3.74886e-02_rb,3.54551e-02_rb,3.28870e-02_rb, &
   3.32576e-02_rb,3.22444e-02_rb,3.12384e-02_rb,3.02584e-02_rb,2.93146e-02_rb, &
   2.84120e-02_rb,2.75525e-02_rb,2.67361e-02_rb,2.59618e-02_rb,2.52280e-02_rb, &
   2.45327e-02_rb,2.38736e-02_rb,2.32487e-02_rb,2.26558e-02_rb,2.20929e-02_rb, &
   2.15579e-02_rb,2.10491e-02_rb,2.05648e-02_rb,1.99749e-02_rb,1.95704e-02_rb, &
   1.91731e-02_rb,1.87839e-02_rb,1.84032e-02_rb,1.80315e-02_rb,1.76689e-02_rb, &
   1.73155e-02_rb,1.69712e-02_rb,1.66362e-02_rb,1.63101e-02_rb,1.59928e-02_rb, &
   1.56842e-02_rb,1.53840e-02_rb,1.50920e-02_rb,1.48080e-02_rb,1.45318e-02_rb, &
   1.42631e-02_rb,1.40016e-02_rb,1.37472e-02_rb,1.34996e-02_rb,1.32586e-02_rb, &
   1.30239e-02_rb,1.27954e-02_rb,1.25728e-02_rb,1.23559e-02_rb,1.21445e-02_rb, &
   1.19385e-02_rb,1.17376e-02_rb,1.15417e-02_rb/)
!
! band 13
!
   absliq1(:,13) = (/                                                          &
   3.11868e-02_rb,4.48357e-02_rb,4.90224e-02_rb,4.96406e-02_rb,4.86806e-02_rb, &
   4.69610e-02_rb,4.48630e-02_rb,4.25795e-02_rb,4.02138e-02_rb,3.78236e-02_rb, &
   3.74266e-02_rb,3.60384e-02_rb,3.47074e-02_rb,3.34434e-02_rb,3.22499e-02_rb, &
   3.11264e-02_rb,3.00704e-02_rb,2.90784e-02_rb,2.81463e-02_rb,2.72702e-02_rb, &
   2.64460e-02_rb,2.56698e-02_rb,2.49381e-02_rb,2.42475e-02_rb,2.35948e-02_rb, &
   2.29774e-02_rb,2.23925e-02_rb,2.18379e-02_rb,2.11793e-02_rb,2.07076e-02_rb, &
   2.02470e-02_rb,1.97981e-02_rb,1.93613e-02_rb,1.89367e-02_rb,1.85243e-02_rb, &
   1.81240e-02_rb,1.77356e-02_rb,1.73588e-02_rb,1.69935e-02_rb,1.66392e-02_rb, &
   1.62956e-02_rb,1.59624e-02_rb,1.56393e-02_rb,1.53259e-02_rb,1.50219e-02_rb, &
   1.47268e-02_rb,1.44404e-02_rb,1.41624e-02_rb,1.38925e-02_rb,1.36302e-02_rb, &
   1.33755e-02_rb,1.31278e-02_rb,1.28871e-02_rb,1.26530e-02_rb,1.24253e-02_rb, &
   1.22038e-02_rb,1.19881e-02_rb,1.17782e-02_rb/)
!
! band 14
!
   absliq1(:,14) = (/                                                          &
   1.58988e-02_rb,3.50652e-02_rb,4.00851e-02_rb,4.07270e-02_rb,3.98101e-02_rb, &
   3.83306e-02_rb,3.66829e-02_rb,3.50327e-02_rb,3.34497e-02_rb,3.19609e-02_rb, &
   3.13712e-02_rb,3.03348e-02_rb,2.93415e-02_rb,2.83973e-02_rb,2.75037e-02_rb, &
   2.66604e-02_rb,2.58654e-02_rb,2.51161e-02_rb,2.44100e-02_rb,2.37440e-02_rb, &
   2.31154e-02_rb,2.25215e-02_rb,2.19599e-02_rb,2.14282e-02_rb,2.09242e-02_rb, &
   2.04459e-02_rb,1.99915e-02_rb,1.95594e-02_rb,1.90254e-02_rb,1.86598e-02_rb, &
   1.82996e-02_rb,1.79455e-02_rb,1.75983e-02_rb,1.72584e-02_rb,1.69260e-02_rb, &
   1.66013e-02_rb,1.62843e-02_rb,1.59752e-02_rb,1.56737e-02_rb,1.53799e-02_rb, &
   1.50936e-02_rb,1.48146e-02_rb,1.45429e-02_rb,1.42782e-02_rb,1.40203e-02_rb, &
   1.37691e-02_rb,1.35243e-02_rb,1.32858e-02_rb,1.30534e-02_rb,1.28270e-02_rb, &
   1.26062e-02_rb,1.23909e-02_rb,1.21810e-02_rb,1.19763e-02_rb,1.17766e-02_rb, &
   1.15817e-02_rb,1.13915e-02_rb,1.12058e-02_rb/)
!
! band 15
!
   absliq1(:,15) = (/                                                          &
   5.02079e-03_rb,2.17615e-02_rb,2.55449e-02_rb,2.59484e-02_rb,2.53650e-02_rb, &
   2.45281e-02_rb,2.36843e-02_rb,2.29159e-02_rb,2.22451e-02_rb,2.16716e-02_rb, &
   2.11451e-02_rb,2.05817e-02_rb,2.00454e-02_rb,1.95372e-02_rb,1.90567e-02_rb, &
   1.86028e-02_rb,1.81742e-02_rb,1.77693e-02_rb,1.73866e-02_rb,1.70244e-02_rb, &
   1.66815e-02_rb,1.63563e-02_rb,1.60477e-02_rb,1.57544e-02_rb,1.54755e-02_rb, &
   1.52097e-02_rb,1.49564e-02_rb,1.47146e-02_rb,1.43684e-02_rb,1.41728e-02_rb, &
   1.39762e-02_rb,1.37797e-02_rb,1.35838e-02_rb,1.33891e-02_rb,1.31961e-02_rb, &
   1.30051e-02_rb,1.28164e-02_rb,1.26302e-02_rb,1.24466e-02_rb,1.22659e-02_rb, &
   1.20881e-02_rb,1.19131e-02_rb,1.17412e-02_rb,1.15723e-02_rb,1.14063e-02_rb, &
   1.12434e-02_rb,1.10834e-02_rb,1.09264e-02_rb,1.07722e-02_rb,1.06210e-02_rb, &
   1.04725e-02_rb,1.03269e-02_rb,1.01839e-02_rb,1.00436e-02_rb,9.90593e-03_rb, &
   9.77080e-03_rb,9.63818e-03_rb,9.50800e-03_rb/)
!
! band 16
!
   absliq1(:,16) = (/                                                          &
   5.64971e-02_rb,9.04736e-02_rb,8.11726e-02_rb,7.05450e-02_rb,6.20052e-02_rb, &
   5.54286e-02_rb,5.03503e-02_rb,4.63791e-02_rb,4.32290e-02_rb,4.06959e-02_rb, &
   3.74690e-02_rb,3.52964e-02_rb,3.33799e-02_rb,3.16774e-02_rb,3.01550e-02_rb, &
   2.87856e-02_rb,2.75474e-02_rb,2.64223e-02_rb,2.53953e-02_rb,2.44542e-02_rb, &
   2.35885e-02_rb,2.27894e-02_rb,2.20494e-02_rb,2.13622e-02_rb,2.07222e-02_rb, &
   2.01246e-02_rb,1.95654e-02_rb,1.90408e-02_rb,1.84398e-02_rb,1.80021e-02_rb, &
   1.75816e-02_rb,1.71775e-02_rb,1.67889e-02_rb,1.64152e-02_rb,1.60554e-02_rb, &
   1.57089e-02_rb,1.53751e-02_rb,1.50531e-02_rb,1.47426e-02_rb,1.44428e-02_rb, &
   1.41532e-02_rb,1.38734e-02_rb,1.36028e-02_rb,1.33410e-02_rb,1.30875e-02_rb, &
   1.28420e-02_rb,1.26041e-02_rb,1.23735e-02_rb,1.21497e-02_rb,1.19325e-02_rb, &
   1.17216e-02_rb,1.15168e-02_rb,1.13177e-02_rb,1.11241e-02_rb,1.09358e-02_rb, &
   1.07525e-02_rb,1.05741e-02_rb,1.04003e-02_rb/)
!
   end subroutine lwcldpr
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_lw_init_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!
! path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_lw.F,v $
! author:    $Author: trn $
! revision:  $Revision: 1.3 $
! created:   $Date: 2009/04/16 19:54:22 $
!
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_lw_rad_k
!-------------------------------------------------------------------------------
!
!  abstract : 
!
!  --------------------------------------------------------------------------
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
!  --------------------------------------------------------------------------
!
! ****************************************************************************
! *                                                                          *
! *                              RRTMG_LW                                    *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                   a rapid radiative transfer model                       *
! *                       for the longwave region                            * 
! *             for application to general circulation models                *
! *                                                                          *
! *                                                                          *
! *            Atmospheric and Environmental Research, Inc.                  *
! *                        131 Hartwell Avenue                               *
! *                        Lexington, MA 02421                               *
! *                                                                          *
! *                                                                          *
! *                           Eli J. Mlawer                                  *
! *                        Jennifer S. Delamere                              *
! *                         Michael J. Iacono                                *
! *                         Shepard A. Clough                                *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                       email:  miacono@aer.com                            *
! *                       email:  emlawer@aer.com                            *
! *                       email:  jdelamer@aer.com                           *
! *                                                                          *
! *        The authors wish to acknowledge the contributions of the          *
! *        following people:  Steven J. Taubman, Karen Cady-Pereira,         *
! *        Patrick D. Brown, Ronald E. Farren, Luke Chen, Robert Bergstrom.  *
! *                                                                          *
! ****************************************************************************
!
!-------------------------------------------------------------------------------
   use parkind_k,             only : im => kind_im, rb => kind_rb
   use rrlw_vsn_k
   use mcica_subcol_gen_k,    only : mcica_subcol
   use rrtmg_lw_cldprmc_k,    only : cldprmc
!
! *** Move the required call to rrtmg_lw_ini below and the following 
! use association to the GCM initialization area ***
!
!  use rrtmg_lw_init,       only : rrtmg_lw_ini
   use rrtmg_lw_rtrnmc_k,     only : rtrnmc
   use rrtmg_lw_setcoef_k,    only : setcoef
   use rrtmg_lw_taumol_k,     only : taumol
!
   implicit none
!
! public interfaces/functions/subroutines
!
   public :: rrtmg_lw, inatm
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
! public subroutines
!-------------------------------------------------------------------------------
   subroutine rrtmg_lw                                                         &
            (ncol    ,nlay    ,icld    ,                                       &
             play    ,plev    ,tlay    ,tlev    ,tsfc    ,                     &
             h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr ,              &
             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    ,                     &
             inflglw ,iceflglw,liqflglw,cldfmcl ,                              &
             taucmcl ,ciwpmcl ,clwpmcl ,reicmcl ,relqmcl ,                     &
             cswpmcl, resnmcl,                                                 &
             tauaer  ,                                                         &
             uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc)
!-------------------------------------------------------------------------------
!
!  abstract :
!  This program is the driver subroutine for RRTMG_LW, the AER LW radiation 
!  model for application to GCMs, that has been adapted from RRTM_LW for
!  improved efficiency.
!
!  .not.: The call to RRTMG_LW_INI should be moved to the GCM initialization
!  area, since this has to be called only once. 
!
!  This routine:
!    a) calls INATM to read in the atmospheric profile from GCM;
!       all layering in RRTMG is ordered from surface to toa. 
!    b) calls CLDPRMC to set cloud optical depth for McICA based 
!       on input cloud properties 
!    c) calls SETCOEF to calculate various quantities needed for 
!       the radiative transfer algorithm
!    d) calls TAUMOL to calculate gaseous optical depths for each 
!       of the 16 spectral bands
!    e) calls RTRNMC (for both clear and cloudy profiles) to perform the
!       radiative transfer calculation using McICA, the Monte-Carlo 
!       Independent Column Approximation, to represent sub-grid scale 
!       cloud variability
!    f) passes the necessary fluxes and cooling rates back to GCM
!
!  Two modes of operation are possible:
!    The mode is chosen by using either rrtmg_lw.nomcica.f90 (to not use
!    McICA) or rrtmg_lw.f90 (to use McICA) to interface with a GCM. 
!
!    1) Standard, single forward model calculation (imca = 0)
!    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al., 
!       JC, 2003) method is applied to the forward model calculation (imca = 1)
!
!  This call to RRTMG_LW must be preceeded by a call to the module
!    mcica_subcol_gen_lw.f90 to run the McICA sub-column cloud generator,
!    which will provide the cloud physical or cloud optical properties
!    on the RRTMG quadrature point (ngpt) dimension.
!    Two random number generators are available for use when imca = 1.
!    This is chosen by setting flag irnd on input to mcica_subcol_gen_lw.
!    1) KISSVEC (irnd = 0)
!    2) Mersenne-Twister (irnd = 1)
!
!  Two methods of cloud property input are possible:
!    Cloud properties can be input in one of two ways (controlled by input 
!    flags inflglw, iceflglw, and liqflglw; see text file rrtmg_lw_instructions
!    and subroutine rrtmg_lw_cldprop.f90 for further details):
!
!    1) Input cloud fraction and cloud optical depth directly (inflglw = 0)
!    2) Input cloud fraction and cloud physical properties (inflglw = 1 or 2);  
!       cloud optical properties are calculated by cldprop or cldprmc based
!       on input settings of iceflglw and liqflglw.  Ice particle size provided
!       must be appropriately defined for the ice parameterization selected. 
!
!  One method of aerosol property input is possible:
!    Aerosol properties can be input in only one way (controlled by input 
!    flag iaer; see text file rrtmg_lw_instructions for further details):
!
!    1) Input aerosol optical depth directly by layer & spectral band (iaer=10);
!       band average optical depth at the mid-point of each spectral band.
!       RRTMG_LW currently treats only aerosol absorption;
!       scattering capability is not presently available.
!
!  ------- Modifications -------
!
!  This version of RRTMG_LW has been modified from RRTM_LW to use a reduced 
!  set of g-points for application to GCMs.  
!
!  history log :
!    1999        M. J. Iacono, AER, Inc.  Original version (derived from
!                                         RRTM_LW), reduction of g-points,
!                                         other revisions for use with GCMs
!    2004-05-01  M. J. Iacono, AER, Inc.  Adapted for use with NCAR/CAM
!    2005-11-01  M. J. Iacono, AER, Inc.  Revised to add McICA capability
!    2007-02-01  M. J. Iacono, AER, Inc.  Conversion to F90 formatting for      
!                                         consistency with rrtmg_sw
!    2007-08-01  M. J. Iacono, AER, Inc.  Modifications to formatting to use 
!                                         assumed-shape arrays
!    2008-04-01  M. J. Iacono, AER, Inc.  Modified to add lw aerosol absorption
!
!  input :
!    ncol     - Number of horizontal columns
!    nlay     - Number of model layers
!    icld     - Cloud overlap method (0: Clear only, 1: Random
!                                     2: Maxiumu/random, 4: Maximum)
!    play     - Layer     pressures (hPa, mb) (ncol,nlay)
!    plev     - Interface pressures (hPa, mb) (ncol,nlay+1)
!    tlay     - Layer     temperature (K) (ncol,nlay)
!    tlev     - Interface temperature (K) (ncol,nlay+1)
!    tsfc     - Surface   temperature (K) (ncol)
!    h2ovmr   - H2O     volume mixing ratio (ncol,nlay)
!    o3vmr    - O3      volume mixing ratio (ncol,nlay)
!    co2vmr   - CO2     volume mixing ratio (ncol,nlay)
!    ch4vmr   - Methane volume mixing ratio (ncol,nlay)
!    n2ovmr   - Nitrous oxide volume mixing ratio (ncol,nlay)
!    o2vmr    - Oxygen  volume mixing ratio (ncol,nlay)
!    cfc11vmr - CFC11   volume mixing ratio (ncol,nlay)
!    cfc12vmr - CFC12   volume mixing ratio (ncol,nlay)
!    cfc22vmr - CFC22   volume mixing ratio (ncol,nlay)
!    ccl4vmr  - CCL4    volume mixing ratio (ncol,nlay)
!    emis     - Surface emissivity (ncol,nbndlw)
!
!    inflglw  - Flag for cloud optical properties
!    iceflglw - Flag for ice particle specification
!    liqflglw - Flag for liquid droplet specification
!
!    cldfmcl  - Cloud fraction (ngptlw,ncol,nlay)
!    ciwpmcl  - In-cloud    ice water path (g/m2) (ngptlw,ncol,nlay)
!    clwpmcl  - In-cloud liquid water path (g/m2) (ngptlw,ncol,nlay)
!    cswpmcl  - In-cloud   snow water path (g/m2) (ngptlw,ncol,nlay)
!    reicmcl  - Cloud ice particle effective size (microns) (ncol,nlay)
!    relqmcl  - Cloud water drop effective radius (microns) (ncol,nlay)
!    resnmcl  - Snow effective radius (microns) (ncol,nlay)
!    taucmcl  - In-cloud optical depth (ngptlw,ncol,nlay) 
!    ssacmcl  - In-cloud single scattering albedo (ngptlw,ncol,nlay)
!               for future expansion (lw scattering not yet available)
!    asmcmcl  - In-cloud asymmetry parameter (ngptlw,ncol,nlay)
!               for future expansion (lw scattering not yet available)
!    tauaer   - aerosol optical depth at mid-point of LW spectral bands
!                (ncol,nlay,nbndlw)
!    ssaaer   - aerosol single scattering albedo (ncol,nlay,nbndlw)
!               for future expansion (lw aerosols/scattering not yet available)
!    asmaer   - aerosol asymmetry parameter (ncol,nlay,nbndlw)
!               for future expansion (lw aerosols/scattering not yet available)
!
!  output :
!    uflx     - Total sky longwave   upward flux (W/m2) (ncol,nlay+1)
!    dflx     - Total sky longwave downward flux (W/m2) (ncol,nlay+1)
!    hr       - Total sky longwave radiative heating rate (K/d) (ncol,nlay)
!    uflxc    - Clear sky longwave   upward flux (W/m2) (ncol,nlay+1)
!    dflxc    - Clear sky longwave downward flux (W/m2) (ncol,nlay+1)
!    hrc      - Clear sky longwave radiative heating rate (K/d) (ncol,nlay)
!
!  local variable :
!    nlayers  - total number of layers
!    istart   - beginning band of calculation
!    iend     - ending band of calculation
!    iout     - output option flag (inactive)
!    iaer     - aerosol option flag
!    iplon    - column loop index
!    imca     - flag for mcica [0=off, 1=on]
!    ims      - value for changing mcica permute seed
!    k        - layer loop index
!    ig       - g-point loop index
!
!    pavel    - layer pressures (mb)
!    tavel    - layer temperatures (K)
!    pz       - level (interface) pressures (hPa, mb)
!    tz       - level (interface) temperatures (K)
!    tbound   - surface temperature (K)
!    coldry   - dry air column density (mol/cm2)
!    wbrodl   - broadening gas column density (mol/cm2)
!    wkl      - molecular amounts (mol/cm-2)
!    wx       - cross-section amounts (mol/cm-2)
!    pwvcm    - precipitable water vapor (cm)
!    semiss   - lw surface emissivity
!    taug     - gaseous optical depths
!    taut     - gaseous + aerosol optical depths
!    taua     - aerosol optical depth
!    ssaa     - aerosol single scattering albedo
!               for future expansion (lw aerosols/scattering not yet available)
!    asma     - aerosol asymmetry parameter
!               for future expansion (lw aerosols/scattering not yet available)
!
!    laytrop  - tropopause layer index
!    jp       - lookup table index
!    jt       - lookup table index
!    jt1      - lookup table index 
!    colh2o   - column amount (h2o)
!    colco2   - column amount (co2)
!    colo3    - column amount (o3)
!    coln2o   - column amount (n2o)
!    colco    - column amount (co)
!    colch4   - column amount (ch4)
!    colo2    - column amount (o2)
!    colbrd   - column amount (broadening gases)
!
!    ncbands  - number of cloud spectral bands    
!    inflag   - flag for cloud property method
!    iceflag  - flag for ice cloud properties
!    liqflag  - flag for liquid cloud properties
!
!    cldfmc   - cloud fraction [mcica]
!    ciwpmc   - in-cloud ice water path [mcica]
!    clwpmc   - in-cloud liquid water path [mcica]
!    cswpmc   - in-cloud snow path [mcica]
!    relqmc   - liquid particle effective radius (microns)
!    reicmc   - ice particle effective size (microns)
!    resnmc   - snow particle effective size (microns)
!    taucmc   - in-cloud optical depth [mcica]
!    ssacmc   - in-cloud single scattering albedo [mcica]
!               for future expansion (lw scattering not yet available)
!    asmcmc   - in-cloud asymmetry parameter [mcica]
!               for future expansion (lw scattering not yet available)
! 
!    totuflux - upward longwave flux (w/m2)
!    totdflux - downward longwave flux (w/m2)
!    fnet     - net longwave flux (w/m2)
!    htr      - longwave heating rate (k/day)
!    totuclfl - clear sky upward longwave flux (w/m2)
!    totdclfl - clear sky downward longwave flux (w/m2)
!    fnetc    - clear sky net longwave flux (w/m2)
!    htrc     - lear sky longwave heating rate (k/day)
!
!-------------------------------------------------------------------------------
   use parrrtm_k,  only : nbndlw, ngptlw, maxxsec, mxmol
   use rrlw_con_k, only : fluxfac, heatfac, oneminus, pi
   use rrlw_wvn_k, only : ng, ngb, nspa, nspb, wavenum1, wavenum2, delwave
!
! ----- Input -----
!
   integer(kind=im),                intent(in   ) :: ncol
   integer(kind=im),                intent(in   ) :: nlay
   integer(kind=im),                intent(inout) :: icld
!
   real(kind=rb), dimension(:,:),   intent(in   ) :: play
   real(kind=rb), dimension(:,:),   intent(in   ) :: plev    ! nlay+1
   real(kind=rb), dimension(:,:),   intent(in   ) :: tlay
   real(kind=rb), dimension(:,:),   intent(in   ) :: tlev    ! nlay+1
   real(kind=rb), dimension(:),     intent(in   ) :: tsfc
   real(kind=rb), dimension(:,:),   intent(in   ) :: h2ovmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: o3vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: co2vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: ch4vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: n2ovmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: o2vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: cfc11vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: cfc12vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: cfc22vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: ccl4vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: emis    ! nbndlw
!
   integer(kind=im),                intent(in   ) :: inflglw
   integer(kind=im),                intent(in   ) :: iceflglw
   integer(kind=im),                intent(in   ) :: liqflglw
!
   real(kind=rb), dimension(:,:,:), intent(in   ) :: cldfmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: ciwpmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: clwpmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: cswpmcl
   real(kind=rb), dimension(:,:),   intent(in   ) :: reicmcl
   real(kind=rb), dimension(:,:),   intent(in   ) :: relqmcl
   real(kind=rb), dimension(:,:),   intent(in   ) :: resnmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: taucmcl
!  real(kind=rb), dimension(:,:,:), intent(in   ) :: ssacmcl
!  real(kind=rb), dimension(:,:,:), intent(in   ) :: asmcmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: tauaer
!  real(kind=rb), dimension(:,:,:), intent(in   ) :: ssaaer
!  real(kind=rb), dimension(:,:,:), intent(in   ) :: asmaer
!
! ----- Output -----
!
   real(kind=rb), dimension(:,:),   intent(  out) :: uflx    ! nlay+1
   real(kind=rb), dimension(:,:),   intent(  out) :: dflx    ! nlay+1
   real(kind=rb), dimension(:,:),   intent(  out) :: hr
   real(kind=rb), dimension(:,:),   intent(  out) :: uflxc   ! nlay+1
   real(kind=rb), dimension(:,:),   intent(  out) :: dflxc   ! nlay+1
   real(kind=rb), dimension(:,:),   intent(  out) :: hrc
!
! ----- Local -----
!
! Control
!
   integer(kind=im)                               :: nlayers
   integer(kind=im)                               :: istart
   integer(kind=im)                               :: iend
   integer(kind=im)                               :: iout
   integer(kind=im)                               :: iaer
   integer(kind=im)                               :: iplon
   integer(kind=im)                               :: imca
   integer(kind=im)                               :: ims
   integer(kind=im)                               :: k
   integer(kind=im)                               :: ig
!
! Atmosphere
!
   real(kind=rb),    dimension(nlay+1)            :: pavel
   real(kind=rb),    dimension(nlay+1)            :: tavel
   real(kind=rb),    dimension(0:nlay+1)          :: pz
   real(kind=rb),    dimension(0:nlay+1)          :: tz
   real(kind=rb)                                  :: tbound
   real(kind=rb),    dimension(nlay+1)            :: coldry
   real(kind=rb),    dimension(nlay+1)            :: wbrodl
   real(kind=rb),    dimension(mxmol,nlay+1)      :: wkl
   real(kind=rb),    dimension(maxxsec,nlay+1)    :: wx
   real(kind=rb)                                  :: pwvcm
   real(kind=rb),    dimension(nbndlw)            :: semiss
   real(kind=rb),    dimension(nlay+1,ngptlw)     :: fracs
   real(kind=rb),    dimension(nlay+1,ngptlw)     :: taug
   real(kind=rb),    dimension(nlay+1,ngptlw)     :: taut
   real(kind=rb),    dimension(nlay+1,nbndlw)     :: taua
!  real(kind=rb),    dimension(nlay+1,nbndlw)     :: ssaa
!  real(kind=rb),    dimension(nlay+1,nbndlw)     :: asma
!
! Atmosphere - setcoef
!
   integer(kind=im)                               :: laytrop
   integer(kind=im), dimension(nlay+1)            :: jp
   integer(kind=im), dimension(nlay+1)            :: jt
   integer(kind=im), dimension(nlay+1)            :: jt1
   real(kind=rb),    dimension(nlay+1,nbndlw)     :: planklay
   real(kind=rb),    dimension(0:nlay+1,nbndlw)   :: planklev
   real(kind=rb),    dimension(nbndlw)            :: plankbnd
!
   real(kind=rb),    dimension(nlay+1)            :: colh2o
   real(kind=rb),    dimension(nlay+1)            :: colco2
   real(kind=rb),    dimension(nlay+1)            :: colo3
   real(kind=rb),    dimension(nlay+1)            :: coln2o
   real(kind=rb),    dimension(nlay+1)            :: colco
   real(kind=rb),    dimension(nlay+1)            :: colch4
   real(kind=rb),    dimension(nlay+1)            :: colo2
   real(kind=rb),    dimension(nlay+1)            :: colbrd
!
   integer(kind=im), dimension(nlay+1)            :: indself
   integer(kind=im), dimension(nlay+1)            :: indfor
   real(kind=rb),    dimension(nlay+1)            :: selffac
   real(kind=rb),    dimension(nlay+1)            :: selffrac
   real(kind=rb),    dimension(nlay+1)            :: forfac
   real(kind=rb),    dimension(nlay+1)            :: forfrac
!
   integer(kind=im), dimension(nlay+1)            :: indminor
   real(kind=rb),    dimension(nlay+1)            :: minorfrac
   real(kind=rb),    dimension(nlay+1)            :: scaleminor
   real(kind=rb),    dimension(nlay+1)            :: scaleminorn2
!
   real(kind=rb),    dimension(nlay+1)            :: fac00, fac01, fac10, fac11
   real(kind=rb),    dimension(nlay+1)            :: rat_h2oco2, rat_h2oco2_1, &
                                                     rat_h2oo3,  rat_h2oo3_1,  &
                                                     rat_h2on2o, rat_h2on2o_1, &
                                                     rat_h2och4, rat_h2och4_1, &
                                                     rat_n2oco2, rat_n2oco2_1, &
                                                     rat_o3co2,  rat_o3co2_1
!
! Atmosphere/clouds - cldprop
!
   integer(kind=im)                               :: ncbands
   integer(kind=im)                               :: inflag
   integer(kind=im)                               :: iceflag
   integer(kind=im)                               :: liqflag
!
! Atmosphere/clouds - cldprmc [mcica]
!
   real(kind=rb),    dimension(ngptlw,nlay+1)     :: cldfmc
   real(kind=rb),    dimension(ngptlw,nlay+1)     :: ciwpmc
   real(kind=rb),    dimension(ngptlw,nlay+1)     :: clwpmc
   real(kind=rb),    dimension(ngptlw,nlay+1)     :: cswpmc
   real(kind=rb),    dimension(nlay+1)            :: relqmc
   real(kind=rb),    dimension(nlay+1)            :: reicmc
   real(kind=rb),    dimension(nlay+1)            :: resnmc
   real(kind=rb),    dimension(ngptlw,nlay+1)     :: taucmc
!  real(kind=rb),    dimension(ngptlw,nlay+1)     :: ssacmc
!  real(kind=rb),    dimension(ngptlw,nlay+1)     :: asmcmc
!
! Output
!
   real(kind=rb),    dimension(0:nlay+1)          :: totuflux
   real(kind=rb),    dimension(0:nlay+1)          :: totdflux
   real(kind=rb),    dimension(0:nlay+1)          :: fnet
   real(kind=rb),    dimension(0:nlay+1)          :: htr
   real(kind=rb),    dimension(0:nlay+1)          :: totuclfl
   real(kind=rb),    dimension(0:nlay+1)          :: totdclfl
   real(kind=rb),    dimension(0:nlay+1)          :: fnetc
   real(kind=rb),    dimension(0:nlay+1)          :: htrc
!-------------------------------------------------------------------------------
!
! Initializations
!
   oneminus = 1._rb - 1.e-6_rb
   pi = 2._rb * asin(1._rb)
   fluxfac = pi * 2.e4_rb                  ! orig:   fluxfac = pi * 2.d4  
   istart = 1
   iend = 16
   iout = 0
   ims = 1
!
! Set imca to select calculation type:
! imca = 0, use standard forward model calculation
! imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
!
! *** This version uses McICA (imca = 1) ***
!
! Set icld to select of clear or cloud calculation and cloud overlap method  
! icld = 0, clear only
! icld = 1, with clouds using random cloud overlap
! icld = 2, with clouds using maximum/random cloud overlap
! icld = 3, with clouds using maximum cloud overlap (McICA only)
!
   if (icld.lt.0.or.icld.gt.3) icld = 2
!
! Set iaer to select aerosol option
! iaer = 0, no aerosols
! icld = 10, input total aerosol optical depth (tauaer) directly
!
   iaer = 10
!
! Call model and data initialization, compute lookup tables, perform
! reduction of g-points from 256 to 140 for input absorption coefficient 
! data and other arrays.
!
! In a GCM this call should be placed in the model initialization
! area, since this has to be called only once.  
!
!  call rrtmg_lw_ini(cpdair)
!
! This is the main longitude/column loop within RRTMG.
!
   do iplon = 1,ncol
!
! Prepare atmospheric profile from GCM for use in RRTMG, and define
! other input parameters.  
!
     call inatm (iplon, nlay, icld, iaer,                                      &
          play, plev, tlay, tlev, tsfc, h2ovmr,                                &
          o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr,            &
          cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw,                &
          cldfmcl, taucmcl,                                                    &
          ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer,                          &
          cswpmcl, resnmcl,                                                    &
          nlayers, pavel, pz, tavel, tz, tbound, semiss, coldry,               &
          wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag,                    &
          cldfmc, taucmc, ciwpmc, clwpmc, reicmc, relqmc,                      &
          cswpmc, resnmc,                                                      &
          taua)
!
! For cloudy atmosphere, use cldprop to set cloud optical properties based on
! input cloud physical properties.  Select method based on choices described
! in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
! effective radius must be passed into cldprop.  Cloud fraction and cloud
! optical depth are transferred to rrtmg_lw arrays in cldprop.  
!
     call cldprmc(nlayers, inflag, iceflag, liqflag, cldfmc, ciwpmc,           &
                  clwpmc, reicmc, relqmc,                                      &
                  cswpmc, resnmc,                                              &
                  ncbands, taucmc)
!
! Calculate information needed by the radiative transfer routine
! that is specific to this atmosphere, especially some of the 
! coefficients and indices needed to compute the optical depths
! by interpolating data from stored reference atmospheres. 
!
     call setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss,           &
                  coldry, wkl, wbrodl,                                         &
                  laytrop, jp, jt, jt1, planklay, planklev, plankbnd,          &
                  colh2o, colco2, colo3, coln2o, colco, colch4, colo2,         &
                  colbrd, fac00, fac01, fac10, fac11,                          &
                  rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,            &
                  rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,          &
                  rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,            &
                  selffac, selffrac, indself, forfac, forfrac, indfor,         &
                  minorfrac, scaleminor, scaleminorn2, indminor)
!
! Calculate the gaseous optical depths and Planck fractions for 
! each longwave spectral band.
!
     call taumol(nlayers, pavel, wx, coldry,                                   &
                 laytrop, jp, jt, jt1, planklay, planklev, plankbnd,           &
                 colh2o, colco2, colo3, coln2o, colco, colch4, colo2,          &
                 colbrd, fac00, fac01, fac10, fac11,                           &
                 rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1,             &
                 rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1,           &
                 rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1,             &
                 selffac, selffrac, indself, forfac, forfrac, indfor,          &
                 minorfrac, scaleminor, scaleminorn2, indminor,                &
                 fracs, taug)
!
! Combine gaseous and aerosol optical depths, if aerosol active
!
     if (iaer .eq. 0) then
       do k = 1,nlayers
         do ig = 1,ngptlw
           taut(k,ig) = taug(k,ig)
         enddo
       enddo
     else if (iaer .eq. 10) then
       do k = 1,nlayers
         do ig = 1,ngptlw
           taut(k,ig) = taug(k,ig) + taua(k,ngb(ig))
         enddo
       enddo
     endif
!
! Call the radiative transfer routine.
! Either routine can be called to do clear sky calculation.  If clouds
! are present, then select routine based on cloud overlap assumption
! to be used.  Clear sky calculation is done simultaneously.
! For McICA, RTRNMC is called for clear and cloudy calculations.
!
     call rtrnmc(nlayers, istart, iend, iout, pz, semiss, ncbands,             &
                 cldfmc, taucmc, planklay, planklev, plankbnd,                 &
                 pwvcm, fracs, taut,                                           &
                 totuflux, totdflux, fnet, htr,                                &
                 totuclfl, totdclfl, fnetc, htrc )
!
! Transfer up and down fluxes and heating rate to output arrays.
! Vertical indexing goes from bottom to top; reverse here for GCM if necessary.
!
     do k = 0,nlayers
       uflx(iplon,k+1) = totuflux(k)
       dflx(iplon,k+1) = totdflux(k)
       uflxc(iplon,k+1) = totuclfl(k)
       dflxc(iplon,k+1) = totdclfl(k)
     enddo
!
     do k = 0,nlayers-1
       hr(iplon,k+1) = htr(k)
       hrc(iplon,k+1) = htrc(k)
     enddo
   enddo
!
   end subroutine rrtmg_lw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine inatm (iplon, nlay, icld, iaer,                                  &
              play, plev, tlay, tlev, tsfc, h2ovmr,                            &
              o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, cfc11vmr, cfc12vmr,        &
              cfc22vmr, ccl4vmr, emis, inflglw, iceflglw, liqflglw,            &
              cldfmcl, taucmcl, ciwpmcl, clwpmcl, reicmcl, relqmcl, tauaer,    &
              cswpmcl, resnmcl,                                                &
              nlayers, pavel, pz, tavel, tz, tbound, semiss, coldry,           &
              wkl, wbrodl, wx, pwvcm, inflag, iceflag, liqflag,                &
              cldfmc, taucmc, ciwpmc, clwpmc, reicmc, relqmc,                  &
              cswpmc, resnmc,                                                  &
              taua)
!-------------------------------------------------------------------------------
!
!  abstract : 
!  Input atmospheric profile from GCM, and prepare it for use in RRTMG_LW.
!  Set other RRTMG_LW input parameters.  
!
!  input :
!    iplon    - Column loop index
!    nlay     - Number of model layers
!    icld     - Cloud overlap method (0: Clear only, 1: Random
!                                     2: Maxiumu/random, 4: Maximum)
!    iaer     - Aerosol option flag
!
!    play     - Layer     pressures (hPa, mb) (ncol,nlay)
!    plev     - Interface pressures (hPa, mb) (ncol,nlay+1)
!    tlay     - Layer     temperature (K) (ncol,nlay)
!    tlev     - Interface temperature (K) (ncol,nlay+1)
!    tsfc     - Surface   temperature (K) (ncol)
!    h2ovmr   - H2O     volume mixing ratio (ncol,nlay)
!    o3vmr    - O3      volume mixing ratio (ncol,nlay)
!    co2vmr   - CO2     volume mixing ratio (ncol,nlay)
!    ch4vmr   - Methane volume mixing ratio (ncol,nlay)
!    n2ovmr   - Nitrous oxide volume mixing ratio (ncol,nlay)
!    o2vmr    - Oxygen  volume mixing ratio (ncol,nlay)
!    cfc11vmr - CFC11   volume mixing ratio (ncol,nlay)
!    cfc12vmr - CFC12   volume mixing ratio (ncol,nlay)
!    cfc22vmr - CFC22   volume mixing ratio (ncol,nlay)
!    ccl4vmr  - CCL4    volume mixing ratio (ncol,nlay)
!    emis     - Surface emissivity (ncol,nbndlw)
!
!    inflglw  - Flag for cloud optical properties
!    iceflglw - Flag for ice particle specification
!    liqflglw - Flag for liquid droplet specification
!
!    cldfmcl  - Cloud fraction (ngptlw,ncol,nlay)
!    ciwpmcl  - In-cloud    ice water path (g/m2) (ngptlw,ncol,nlay)
!    clwpmcl  - In-cloud liquid water path (g/m2) (ngptlw,ncol,nlay)
!    cswpmcl  - In-cloud   snow water path (g/m2) (ngptlw,ncol,nlay)
!    reicmcl  - Cloud ice particle effective size (microns) (ncol,nlay)
!    relqmcl  - Cloud water drop effective radius (microns) (ncol,nlay)
!    resnmcl  - Snow effective radius (microns) (ncol,nlay)
!    taucmcl  - In-cloud optical depth (ngptlw,ncol,nlay)
!    tauaer   - aerosol  optical depth (ncol,nlay,nbndlw)
!
!  output :
!    nlayers  - number of layers
!    pavel    - layer pressures (mb)
!    tavel    - layer temperatures (K)
!    pz       - level (interface) pressures (hPa, mb)
!    tz       - level (interface) temperatures (K)
!    tbound   - surface temperature (K)
!    coldry   - dry air column density (mol/cm2)
!    wbrodl   - broadening gas column density (mol/cm2)
!    wkl      - molecular amounts (mol/cm-2)
!    wx       - cross-section amounts (mol/cm-2)
!    pwvcm    - precipitable water vapor (cm)
!    semiss   - lw surface emissivity
!
!    inflag   - flag for cloud property method
!    iceflag  - flag for ice cloud properties
!    liqflag  - flag for liquid cloud properties
!    cldfmc   - cloud fraction [mcica]
!    ciwpmc   - in-cloud ice water path [mcica]
!    clwpmc   - in-cloud liquid water path [mcica]
!    cswpmc   - in-cloud snow path [mcica]
!    relqmc   - liquid particle effective radius (microns)
!    reicmc   - ice particle effective size (microns)
!    resnmc   - snow particle effective size (microns)
!    taucmc   - in-cloud optical depth [mcica]
!    taua     - aerosol optical depth
!
!  local variable :
!    amd      - Effective molecular weight of dry air (g/mol)
!    amw      - Molecular weight of water vapor (g/mol)
!    amc      - Molecular weight of carbon dioxide (g/mol)
!    amo      - Molecular weight of ozone (g/mol)
!    amo2     - Molecular weight of oxygen (g/mol)
!    amch4    - Molecular weight of methane (g/mol)
!    amn2o    - Molecular weight of nitrous oxide (g/mol)
!    amc11    - Molecular weight of CFC11 (g/mol) - CCL3F
!    amc12    - Molecular weight of CFC12 (g/mol) - CCL2F2
!    amc22    - Molecular weight of CFC22 (g/mol) - CHCLF2
!    amc14    - Molecular weight of CCL4  (g/mol) - CCL4
!
!    amdw     - Molecular weight of dry air / water vapor
!    amdc     - Molecular weight of dry air / carbon dioxide
!    amdo     - Molecular weight of dry air / ozone
!    amdm     - Molecular weight of dry air / methane
!    amdn     - Molecular weight of dry air / nitrous oxide
!    amdo2    - Molecular weight of dry air / oxygen
!    amdc1    - Molecular weight of dry air / CFC11
!    amdc2    - Molecular weight of dry air / CFC12
!
!-------------------------------------------------------------------------------
   use parrrtm_k,  only : nbndlw, ngptlw, nmol, maxxsec, mxmol
   use rrlw_con_k, only : fluxfac, heatfac, oneminus, pi, grav, avogad
   use rrlw_wvn_k, only : ng, nspa, nspb, wavenum1, wavenum2, delwave, ixindx
!
! ----- Input -----
!
   integer(kind=im),                intent(in   ) :: iplon
   integer(kind=im),                intent(in   ) :: nlay
   integer(kind=im),                intent(in   ) :: icld
   integer(kind=im),                intent(in   ) :: iaer
!
   real(kind=rb), dimension(:,:),   intent(in   ) :: play
   real(kind=rb), dimension(:,:),   intent(in   ) :: plev    ! nlay+1
   real(kind=rb), dimension(:,:),   intent(in   ) :: tlay
   real(kind=rb), dimension(:,:),   intent(in   ) :: tlev    ! nlay+1
   real(kind=rb), dimension(:),     intent(in   ) :: tsfc
   real(kind=rb), dimension(:,:),   intent(in   ) :: h2ovmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: o3vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: co2vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: ch4vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: n2ovmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: o2vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: cfc11vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: cfc12vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: cfc22vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: ccl4vmr
   real(kind=rb), dimension(:,:),   intent(in   ) :: emis    ! nbndlw
!
   integer(kind=im),                intent(in   ) :: inflglw
   integer(kind=im),                intent(in   ) :: iceflglw
   integer(kind=im),                intent(in   ) :: liqflglw
!
   real(kind=rb), dimension(:,:,:), intent(in   ) :: cldfmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: ciwpmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: clwpmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: cswpmcl
   real(kind=rb), dimension(:,:),   intent(in   ) :: reicmcl
   real(kind=rb), dimension(:,:),   intent(in   ) :: relqmcl
   real(kind=rb), dimension(:,:),   intent(in   ) :: resnmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: taucmcl
   real(kind=rb), dimension(:,:,:), intent(in   ) :: tauaer
!
! ----- Output -----
!
! Atmosphere
!
   integer(kind=im),                intent(  out) :: nlayers
   real(kind=rb), dimension(:),     intent(  out) :: pavel
   real(kind=rb), dimension(:),     intent(  out) :: tavel
   real(kind=rb), dimension(0:),    intent(  out) :: pz
   real(kind=rb), dimension(0:),    intent(  out) :: tz
   real(kind=rb),                   intent(  out) :: tbound
   real(kind=rb), dimension(:),     intent(  out) :: coldry
   real(kind=rb), dimension(:),     intent(  out) :: wbrodl
   real(kind=rb), dimension(:,:),   intent(  out) :: wkl
   real(kind=rb), dimension(:,:),   intent(  out) :: wx
   real(kind=rb),                   intent(  out) :: pwvcm
   real(kind=rb), dimension(:),     intent(  out) :: semiss
!
! Atmosphere/clouds - cldprop
!
   integer(kind=im),                intent(  out) :: inflag
   integer(kind=im),                intent(  out) :: iceflag
   integer(kind=im),                intent(  out) :: liqflag
   real(kind=rb), dimension(:,:),   intent(  out) :: cldfmc
   real(kind=rb), dimension(:,:),   intent(  out) :: ciwpmc
   real(kind=rb), dimension(:,:),   intent(  out) :: clwpmc
   real(kind=rb), dimension(:,:),   intent(  out) :: cswpmc
   real(kind=rb), dimension(:),     intent(  out) :: relqmc
   real(kind=rb), dimension(:),     intent(  out) :: reicmc 
   real(kind=rb), dimension(:),     intent(  out) :: resnmc
   real(kind=rb), dimension(:,:),   intent(  out) :: taucmc
   real(kind=rb), dimension(:,:),   intent(  out) :: taua
!
! ----- Local -----
!
   real(kind=rb), parameter :: amd = 28.9660_rb
   real(kind=rb), parameter :: amw = 18.0160_rb
!  real(kind=rb), parameter :: amc = 44.0098_rb
!  real(kind=rb), parameter :: amo = 47.9998_rb
!  real(kind=rb), parameter :: amo2 = 31.9999_rb
!  real(kind=rb), parameter :: amch4 = 16.0430_rb
!  real(kind=rb), parameter :: amn2o = 44.0128_rb
!  real(kind=rb), parameter :: amc11 = 137.3684_rb
!  real(kind=rb), parameter :: amc12 = 120.9138_rb
!  real(kind=rb), parameter :: amc22 = 86.4688_rb
!  real(kind=rb), parameter :: amcl4 = 153.823_rb
!
! Set molecular weight ratios (for converting mmr to vmr)
! e.g. h2ovmr = h2ommr * amdw)
!
   real(kind=rb), parameter :: amdw = 1.607793_rb
   real(kind=rb), parameter :: amdc = 0.658114_rb
   real(kind=rb), parameter :: amdo = 0.603428_rb
   real(kind=rb), parameter :: amdm = 1.805423_rb
   real(kind=rb), parameter :: amdn = 0.658090_rb
   real(kind=rb), parameter :: amdo2 = 0.905140_rb
   real(kind=rb), parameter :: amdc1 = 0.210852_rb
   real(kind=rb), parameter :: amdc2 = 0.239546_rb
!
   integer(kind=im)         :: isp, l, ix, n, imol, ib, ig   ! Loop indices
   real(kind=rb)            :: amm, amttl, wvttl, wvsh, summol  
!-------------------------------------------------------------------------------
!
! Add one to nlayers here to include extra model layer at top of atmosphere
!
   nlayers = nlay
!
! Initialize all molecular amounts and cloud properties to zero here,
! then pass input amounts into RRTM arrays below.
!
   wkl    = 0.0_rb ; wx     = 0.0_rb ; cldfmc = 0.0_rb
   taucmc = 0.0_rb ; ciwpmc = 0.0_rb ; clwpmc = 0.0_rb
   cswpmc = 0.0_rb
   reicmc = 0.0_rb ; relqmc = 0.0_rb
   resnmc = 0.0_rb
   taua   = 0.0_rb ; amttl  = 0.0_rb ; wvttl  = 0.0_rb
! 
! Set surface temperature.
!
   tbound = tsfc(iplon)
!
! Install input GCM arrays into RRTMG_LW arrays for pressure, temperature,
! and molecular amounts.  
! Pressures are input in mb, or are converted to mb here.
! Molecular amounts are input in volume mixing ratio, or are converted from 
! mass mixing ratio (or specific humidity for h2o) to volume mixing ratio
! here. These are then converted to molecular amount (molec/cm2) below.  
! The dry air column COLDRY (in molec/cm2) is calculated from the level 
! pressures, pz (in mb), based on the hydrostatic equation and includes a 
! correction to account for h2o in the layer.  The molecular weight of moist 
! air (amm) is calculated for each layer.  
! Note: In RRTMG, layer indexing goes from bottom to top, and coding below
! assumes GCM input fields are also bottom to top. Input layer indexing
! from GCM fields should be reversed here if necessary.
!
   pz(0) = plev(iplon,1)
   tz(0) = tlev(iplon,1)
!
   do l = 1,nlayers
     pavel(l) = play(iplon,l)
     tavel(l) = tlay(iplon,l)
     pz(l) = plev(iplon,l+1)
     tz(l) = tlev(iplon,l+1)
!
! For h2o input in vmr:
!
     wkl(1,l) = h2ovmr(iplon,l)
!
! For h2o input in mmr:
!
!    wkl(1,l) = h2o(iplon,l)*amdw
!
! For h2o input in specific humidity;
!
!    wkl(1,l) = (h2o(iplon,l)/(1._rb - h2o(iplon,l)))*amdw
!
     wkl(2,l) = co2vmr(iplon,l)
     wkl(3,l) = o3vmr(iplon,l)
     wkl(4,l) = n2ovmr(iplon,l)
     wkl(6,l) = ch4vmr(iplon,l)
     wkl(7,l) = o2vmr(iplon,l)
     amm = (1._rb - wkl(1,l)) * amd + wkl(1,l) * amw            
     coldry(l) = (pz(l-1)-pz(l)) * 1.e3_rb * avogad /                          &
                 (1.e2_rb * grav * amm * (1._rb + wkl(1,l)))
   enddo
!
! Set cross section molecule amounts from input; convert to vmr if necessary
!
   do l = 1,nlayers
     wx(1,l) = ccl4vmr(iplon,l)
     wx(2,l) = cfc11vmr(iplon,l)
     wx(3,l) = cfc12vmr(iplon,l)
     wx(4,l) = cfc22vmr(iplon,l)
   enddo      
!
! The following section can be used to set values for an additional layer (from
! the GCM top level to 1.e-4 mb) for improved calculation of TOA fluxes. 
! Temperature and molecular amounts in the extra model layer are set to 
! their values in the top GCM model layer, though these can be modified
! here if necessary. 
! If this feature is utilized, increase nlayers by one above, limit the two
! loops above to (nlayers-1), and set the top most (extra) layer values here. 
!
!  pavel(nlayers) = 0.5_rb * pz(nlayers-1)
!  tavel(nlayers) = tavel(nlayers-1)
!  pz(nlayers) = 1.e-4_rb
!  tz(nlayers-1) = 0.5_rb * (tavel(nlayers)+tavel(nlayers-1))
!  tz(nlayers) = tz(nlayers-1)
!  wkl(1,nlayers) = wkl(1,nlayers-1)
!  wkl(2,nlayers) = wkl(2,nlayers-1)
!  wkl(3,nlayers) = wkl(3,nlayers-1)
!  wkl(4,nlayers) = wkl(4,nlayers-1)
!  wkl(6,nlayers) = wkl(6,nlayers-1)
!  wkl(7,nlayers) = wkl(7,nlayers-1)
!  amm = (1._rb - wkl(1,nlayers-1)) * amd + wkl(1,nlayers-1) * amw
!  coldry(nlayers) = (pz(nlayers-1)) * 1.e3_rb * avogad /                      &
!                    (1.e2_rb * grav * amm * (1._rb + wkl(1,nlayers-1)))
!  wx(1,nlayers) = wx(1,nlayers-1)
!  wx(2,nlayers) = wx(2,nlayers-1)
!  wx(3,nlayers) = wx(3,nlayers-1)
!  wx(4,nlayers) = wx(4,nlayers-1)
!
! At this point all molecular amounts in wkl and wx are in volume mixing ratio; 
! convert to molec/cm2 based on coldry for use in rrtm.  also, compute 
! precipitable water vapor for diffusivity angle adjustments in rtrn and rtrnmr.
!
   do l = 1,nlayers
     summol = 0.0_rb
     do imol = 2,nmol
       summol = summol + wkl(imol,l)
     enddo
!
     wbrodl(l) = coldry(l) * (1._rb - summol)
     do imol = 1,nmol
       wkl(imol,l) = coldry(l) * wkl(imol,l)
     enddo
!
     amttl = amttl + coldry(l)+wkl(1,l)
     wvttl = wvttl + wkl(1,l)
     do ix = 1,maxxsec
       if (ixindx(ix) .ne. 0) then
         wx(ixindx(ix),l) = coldry(l) * wx(ix,l) * 1.e-20_rb
       endif
     enddo
   enddo
!
   wvsh = (amw * wvttl) / (amd * amttl)
   pwvcm = wvsh * (1.e3_rb * pz(0)) / (1.e2_rb * grav)
!
! Set spectral surface emissivity for each longwave band.  
!
   do n = 1,nbndlw
     semiss(n) = emis(iplon,n)
!    semiss(n) = 1.0_rb
   enddo
!
! Transfer aerosol optical properties to RRTM variable;
! modify to reverse layer indexing here if necessary.
!
   if (iaer .ge. 1) then
     do l = 1,nlayers
       do ib = 1,nbndlw
         taua(l,ib) = tauaer(iplon,l,ib)
       enddo
     enddo
   endif
!
! Transfer cloud fraction and cloud optical properties to RRTM variables,
! modify to reverse layer indexing here if necessary.
!
   if (icld .ge. 1) then 
     inflag = inflglw
     iceflag = iceflglw
     liqflag = liqflglw
!
! Move incoming GCM cloud arrays to RRTMG cloud arrays.
! For GCM input, incoming reicmcl is defined based on selected ice 
! parameterization (inflglw)
!
     do l = 1,nlayers
       do ig = 1,ngptlw
         cldfmc(ig,l) = cldfmcl(ig,iplon,l)
         taucmc(ig,l) = taucmcl(ig,iplon,l)
         ciwpmc(ig,l) = ciwpmcl(ig,iplon,l)
         clwpmc(ig,l) = clwpmcl(ig,iplon,l)
         cswpmc(ig,l) = cswpmcl(ig,iplon,l)
       enddo
       reicmc(l) = reicmcl(iplon,l)
       relqmc(l) = relqmcl(iplon,l)
       resnmc(l) = resnmcl(iplon,l)
     enddo
!
! If an extra layer is being used in RRTMG, 
! set all cloud properties to zero in the extra layer.
!
!    cldfmc(:,nlayers) = 0.0_rb
!    taucmc(:,nlayers) = 0.0_rb
!    ciwpmc(:,nlayers) = 0.0_rb
!    clwpmc(:,nlayers) = 0.0_rb
!    reicmc(nlayers) = 0.0_rb
!    relqmc(nlayers) = 0.0_rb
!    taua(nlayers,:) = 0.0_rb
!
   endif
!      
   end subroutine inatm
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_lw_rad_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module module_ra_rrtmg_lwk
!-------------------------------------------------------------------------------
!   use rad_effective_radius,   only : effectRad_wdm, cldf_to_qcqi
!  use comio
   use parrrtm_k,                only : nbndlw, ngptlw
   use rrtmg_lw_init_k,          only : rrtmg_lw_ini
   use rrtmg_lw_rad_k,           only : rrtmg_lw
   use mcica_subcol_gen_k,       only : mcica_subcol
!
   real retab(95)
   data retab /                                                                &
         5.92779, 6.26422, 6.61973, 6.99539, 7.39234,                          &
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,                 &
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,                 &
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,                 &
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,                 &
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,                 &
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,                 &
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,                 &
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,                 &
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,                 &
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,                 &
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,                 &
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,                 &
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956,                 &
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699,                 &
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639/
!
   save retab
!
! For buffer layer adjustment.  Steven Cavallo, Dec 2010.
!
   real, parameter    :: qmin=0., cp=1.0046e+3, t0c=2.7315e+2, rd=2.8705e+2
   integer, save   :: nlayers    
   real, parameter :: deltap = 4.  ! Pressure interval for buffer layer in mb
!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   subroutine inirad (o3prof, plev, kts, kte)
!-------------------------------------------------------------------------------
!
!  abstract : compute ozone mixing ratio distribution
!
!-------------------------------------------------------------------------------
!
   implicit none
!
   integer,                      intent(in   ) :: kts, kte
   real, dimension( kts:kte+1 ), intent(inout) :: o3prof
   real, dimension( kts:kte+2 ), intent(in   ) :: plev
!
! local var
! 
   integer :: k
!-------------------------------------------------------------------------------
!
   do k = kts,kte+1
     o3prof(k) = 0.                                                       
   enddo
!
   call o3data(o3prof, plev, kts, kte)
!
   end subroutine inirad
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine o3data (o3prof, plev, kts, kte)
!-------------------------------------------------------------------------------
!
   implicit none
!
   integer,                      intent(in   ) :: kts, kte
   real, dimension( kts:kte+1 ), intent(inout) :: o3prof
   real, dimension( kts:kte+2 ), intent(in   ) :: plev
!
! local var
!
   integer ::  k, jj
   real, dimension(kts:kte+2) :: prlevh
   real, dimension(32)        :: ppwrkh
   real, dimension(31)        :: o3wrk, ppwrk, o3sum, ppsum,                   &
                                 o3win, ppwin, o3ann, ppann
   real    ::  pb1, pb2, pt1, pt2
!
   data o3sum  /5.297e-8,5.852e-8,6.579e-8,7.505e-8,                           &
                8.577e-8,9.895e-8,1.175e-7,1.399e-7,1.677e-7,2.003e-7,         &
                2.571e-7,3.325e-7,4.438e-7,6.255e-7,8.168e-7,1.036e-6,         &
                1.366e-6,1.855e-6,2.514e-6,3.240e-6,4.033e-6,4.854e-6,         &
                5.517e-6,6.089e-6,6.689e-6,1.106e-5,1.462e-5,1.321e-5,         &
                9.856e-6,5.960e-6,5.960e-6/
!
   data ppsum  /955.890,850.532,754.599,667.742,589.841,                       &
                519.421,455.480,398.085,347.171,301.735,261.310,225.360,       &
                193.419,165.490,141.032,120.125,102.689, 87.829, 75.123,       &
                64.306, 55.086, 47.209, 40.535, 34.795, 29.865, 19.122,        &
                9.277,  4.660,  2.421,  1.294,  0.647/
!
   data o3win  /4.629e-8,4.686e-8,5.017e-8,5.613e-8,                           &
                6.871e-8,8.751e-8,1.138e-7,1.516e-7,2.161e-7,3.264e-7,         &
                4.968e-7,7.338e-7,1.017e-6,1.308e-6,1.625e-6,2.011e-6,         &
                2.516e-6,3.130e-6,3.840e-6,4.703e-6,5.486e-6,6.289e-6,         &
                6.993e-6,7.494e-6,8.197e-6,9.632e-6,1.113e-5,1.146e-5,         &
                9.389e-6,6.135e-6,6.135e-6/
!
   data ppwin  /955.747,841.783,740.199,649.538,568.404,                       &
                495.815,431.069,373.464,322.354,277.190,237.635,203.433,       &
                174.070,148.949,127.408,108.915, 93.114, 79.551, 67.940,       &
                58.072, 49.593, 42.318, 36.138, 30.907, 26.362, 16.423,        &
                7.583,  3.620,  1.807,  0.938,  0.469/
!-------------------------------------------------------------------------------
!
   do k = 1,31                                                              
     ppann(k) = ppsum(k)                                                        
   enddo
!
   o3ann(1) = 0.5*(o3sum(1)+o3win(1))                                           
!
   do k = 2,31                                                              
     o3ann(k) = o3win(k-1)+(o3win(k)-o3win(k-1))/(ppwin(k)-ppwin(k-1))*        &
               (ppsum(k)-ppwin(k-1))                                           
   enddo
!
   do k = 2,31                                                              
     o3ann(k) = 0.5*(o3ann(k)+o3sum(k))                                         
   enddo
!
   do k = 1,31                                                                
     o3wrk(k) = o3ann(k)                                                        
     ppwrk(k) = ppann(k)                                                        
   enddo
!
! calculate half pressure levels for model.and.data levels                     
!
! plev is total P at model levels, from bottom to top
! plev is in mb
!
   do k = kts,kte+2
     prlevh(k) = plev(k)
   enddo
!
   ppwrkh(1) = 1100.                                                        
   do k = 2,31                                                           
     ppwrkh(k) = (ppwrk(k)+ppwrk(k-1))/2.                                   
   enddo
!
   ppwrkh(32) = 0.                                                          
   do k = kts,kte+1
     do 25 jj = 1,31                                                        
       if ((-(prlevh(k)-ppwrkh(jj))).ge.0.) then                            
         pb1 = 0.                                                           
       else                                                               
         pb1 = prlevh(k)-ppwrkh(jj)                                         
       endif                                                              
!
       if ((-(prlevh(k)-ppwrkh(jj+1))).ge.0.) then                          
         pb2 = 0.                                                           
       else                                                               
         pb2 = prlevh(k)-ppwrkh(jj+1)                                       
       endif                                                              
!
       if ((-(prlevh(k+1)-ppwrkh(jj))).ge.0.) then                          
         pt1 = 0.                                                           
       else                                                               
         pt1 = prlevh(k+1)-ppwrkh(jj)                                       
       endif                                                              
!
       if ((-(prlevh(k+1)-ppwrkh(jj+1))).ge.0.) then                        
         pt2 = 0.                                                           
       else                                                               
         pt2 = prlevh(k+1)-ppwrkh(jj+1)                                     
       endif                                                              
!
       o3prof(k) = o3prof(k)+(pb2-pb1-pt2+pt1)*o3wrk(jj)                
  25 continue                                                             
     o3prof(k) = o3prof(k)/(prlevh(k)-prlevh(k+1))                      
   enddo
!
   end subroutine o3data
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rrtmg_lwinit_k(                                                  &
                       allowed_to_read ,                                       &
                       ids, ide, jds, jde, kds, kde,                           &
                       ims, ime, jms, jme, kms, kme,                           &
                       its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------------------
!
   implicit none
!
   logical , intent(in   )           :: allowed_to_read
   integer , intent(in   )           :: ids, ide, jds, jde, kds, kde,          &
                                        ims, ime, jms, jme, kms, kme,          &
                                        its, ite, jts, jte, kts, kte
!-------------------------------------------------------------------------------
!   
   nlayers = kte  ! changed, shbaek 
!
! Read in absorption coefficients and other data
!
   if (allowed_to_read) then
     call rrtmg_lwlookuptable
   endif
!
! Perform g-point reduction and other initializations
! Specific heat of dry air (cp) used in flux to heating rate conversion factor.
!
   call rrtmg_lw_ini(cp)
!
   end subroutine rrtmg_lwinit_k  
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rrtmg_lwlookuptable
!-------------------------------------------------------------------------------
!
   implicit none
!
! Local                                    
!
   integer                 :: i
   logical                 :: opened
   logical , external      :: wrf_dm_on_monitor
!
   character*80            :: errmess
   integer                 :: rrtmg_unit
!-------------------------------------------------------------------------------
!

   if (wrf_dm_on_monitor()) then
     do i = 10,99
       inquire ( i , opened = opened )
       if ( .not. opened ) then
         rrtmg_unit = i
         goto 2010
       endif
     enddo
     rrtmg_unit = -1
2010 continue
   endif
!


     CALL wrf_dm_bcast_bytes ( rrtmg_unit , 4 )
      IF ( rrtmg_unit < 0 ) THEN
        CALL wrf_error_fatal ( 'module_ra_rrtmg_lw: rrtm_lwlookuptable: Can not '// &
                               'find unused fortran unit to read in lookup table.' )
      ENDIF


   if ( wrf_dm_on_monitor() ) then
     open(rrtmg_unit,file='RRTMG_LW_DATA',                                     &
             form='unformatted',status='old',err=9009)
   endif
!
   call lw_kgb01(rrtmg_unit)
   call lw_kgb02(rrtmg_unit)
   call lw_kgb03(rrtmg_unit)
   call lw_kgb04(rrtmg_unit)
   call lw_kgb05(rrtmg_unit)
   call lw_kgb06(rrtmg_unit)
   call lw_kgb07(rrtmg_unit)
   call lw_kgb08(rrtmg_unit)
   call lw_kgb09(rrtmg_unit)
   call lw_kgb10(rrtmg_unit)
   call lw_kgb11(rrtmg_unit)
   call lw_kgb12(rrtmg_unit)
   call lw_kgb13(rrtmg_unit)
   call lw_kgb14(rrtmg_unit)
   call lw_kgb15(rrtmg_unit)
   call lw_kgb16(rrtmg_unit)
!
   if ( wrf_dm_on_monitor() ) close (rrtmg_unit)
!
   return
9009 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error opening RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine rrtmg_lwlookuptable
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!
!  RRTMG Longwave Radiative Transfer Model
!  Atmospheric and Environmental Research, Inc., Cambridge, MA
!
!  Original version:   E. J. Mlawer, et al.
!  Revision for GCMs:  Michael J. Iacono; October, 2002
!  Revision for F90 formatting:  Michael J. Iacono; June 2006
!
!  This file contains 16 READ statements that include the 
!  absorption coefficients and other data for each of the 16 longwave
!  spectral bands used in RRTMG_LW.  Here, the data are defined for 16
!  g-points, or sub-intervals, per band.  These data are combined and
!  weighted using a mapping procedure in module RRTMG_LW_INIT to reduce
!  the total number of g-points from 256 to 140 for use in the GCM.
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb01(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: P = 212.7250 mbar, T = 223.06 K
!
!  The array KAO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels > ~100mb and temperatures.  The first
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the corresponding TREF for this  pressure level, 
!  JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30, 
!  JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second 
!  index, JP, runs from 1 to 13 and refers to the corresponding 
!  pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).  
!  The third index, IG, goes from 1 to 16, and tells us which 
!  g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The arrays kao_mn2 and kbo_mn2 contain the coefficients of the 
!  nitrogen continuum for the upper and lower atmosphere.
!  Minor gas mapping levels: 
!  Lower - n2: P = 142.5490 mbar, T = 215.70 K
!  Upper - n2: P = 142.5490 mbar, T = 215.70 K
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg01_k, only : fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2,   &
                         absa, absb, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mn2, kbo_mn2, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mn2 , size ( kao_mn2 ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mn2 , size ( kbo_mn2 ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb01
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb02(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 1053.630 mbar, T = 294.2 K
!  Upper: P = 3.206e-2 mb, T = 197.92 K
!
!  The array KAO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels > ~100mb and temperatures.  The first
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the corresponding TREF for this  pressure level, 
!  JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30, 
!  JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second 
!  index, JP, runs from 1 to 13 and refers to the corresponding 
!  pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).  
!  The third index, IG, goes from 1 to 16, and tells us which 
!  g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg02_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb02
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb03(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 212.7250 mbar, T = 223.06 K
!  Upper: P = 95.8 mbar, T = 215.7 k
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array KBO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level above 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amounts ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 to 
!  that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg03_k, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o,           &
                         kbo_mn2o, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mn2o , size ( kao_mn2o ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mn2o , size ( kbo_mn2o ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb03 
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb04(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower : P = 142.5940 mbar, T = 215.70 K
!  Upper : P = 95.58350 mb, T = 215.70 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels  < ~100mb, temperatures, and ratios
!  of H2O to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index, JT, which
!  runs from 1 to 5, corresponds to different temperatures.  More 
!  specifically, JT = 3 means that the data are for the corresponding 
!  reference temperature TREF for this  pressure level, JT = 2 refers 
!  to the TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and
!  JT = 5 is for TREF+30.  The third index, JP, runs from 13 to 59 and
!  refers to the corresponding pressure level in PREF (e.g. JP = 13 is
!  for a pressure of 95.5835 mb).  The fourth index, IG, goes from 1 to
!  16, and tells us which g-interval the absorption coefficients are for.
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg04_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb04
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb05(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 473.42 mb, T = 259.83
!  Upper: P = 0.2369280 mbar, T = 253.60 K
!
!  The arrays kao_mo3 and ccl4o contain the coefficients for
!  ozone and ccl4 in the lower atmosphere.
!  Minor gas mapping level:
!  Lower - o3: P = 317.34 mbar, T = 240.77 k
!  Lower - ccl4:
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels  < ~100mb, temperatures, and ratios
!  of H2O to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index, JT, which
!  runs from 1 to 5, corresponds to different temperatures.  More 
!  specifically, JT = 3 means that the data are for the corresponding 
!  reference temperature TREF for this  pressure level, JT = 2 refers 
!  to the TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and
!  JT = 5 is for TREF+30.  The third index, JP, runs from 13 to 59 and
!  refers to the corresponding pressure level in PREF (e.g. JP = 13 is
!  for a pressure of 95.5835 mb).  The fourth index, IG, goes from 1 to
!  16, and tells us which g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg05_k, only : fracrefao, fracrefbo, kao, kbo, kao_mo3,            &
                         selfrefo, forrefo, ccl4o
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mo3, ccl4o, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mo3 , size ( kao_mo3 ) * 4 )
   call wrf_dm_bcast_bytes ( ccl4o , size ( ccl4o ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb05
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb06(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: : P = 473.4280 mb, T = 259.83 K
!
!  The arrays kao_mco2, cfc11adjo and cfc12o contain the coefficients for
!  carbon dioxide in the lower atmosphere and cfc11 and cfc12 in the upper
!  atmosphere.
!  Original cfc11 is multiplied by 1.385 to account for the 1060-1107 cm-1 band.
!  Minor gas mapping level:
!  Lower - co2: P = 706.2720 mb, T = 294.2 k
!  Upper - cfc11, cfc12
!
!  The array KAO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels > ~100mb and temperatures.  The first
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the corresponding TREF for this  pressure level, 
!  JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30, 
!  JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second 
!  index, JP, runs from 1 to 13 and refers to the corresponding 
!  pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).  
!  The third index, IG, goes from 1 to 16, and tells us which 
!  g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg06_k
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, kao, kao_mco2, cfc11adjo, cfc12o, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
   call wrf_dm_bcast_bytes ( cfc11adjo , size ( cfc11adjo ) * 4 )
   call wrf_dm_bcast_bytes ( cfc12o , size ( cfc12o ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb06
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb07(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower : P = 706.27 mb, T = 278.94 K
!  Upper : P = 95.58 mbar, T= 215.70 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array KBO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level above 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296_rb,260_rb,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg07_k, only : fracrefao, fracrefbo, kao, kbo, kao_mco2,           &
                         kbo_mco2, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mco2 , size ( kbo_mco2 ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb07
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb08(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P=473.4280 mb, T = 259.83 K
!  Upper: P=95.5835 mb, T= 215.7 K
!  The arrays kao_mco2, kbo_mco2, kao_mn2o, kbo_mn2o contain the coefficients
!  for carbon dioxide and n2o in the lower and upper atmosphere.
!  The array kao_mo3 contains the coefficients for ozone in the lower atmosphere
!  , and arrays cfc12o & cfc12adjo contain the coefficients for cfc12 & cfc22.
!  Original cfc22 is multiplied by 1.485 to account for the 780-850 cm-1 
!  and 1290-1335 cm-1 bands.
!  Minor gas mapping level:
!  Lower - co2: P = 1053.63 mb, T = 294.2 k
!  Lower - o3: P = 317.348 mb, T = 240.77 k
!  Lower - n2o: P = 706.2720 mb, T= 278.94 k
!  Lower - cfc12, cfc22
!  Upper - co2: P = 35.1632 mb, T = 223.28 k
!  Upper - n2o: P = 8.716e-2 mb, T = 226.03 k
!  The array KAO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels > ~100mb and temperatures.  The first
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the corresponding TREF for this  pressure level, 
!  JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30, 
!  JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second 
!  index, JP, runs from 1 to 13 and refers to the corresponding 
!  pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).  
!  The third index, IG, goes from 1 to 16, and tells us which 
!  g-interval the absorption coefficients are for.
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!  The array KBO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level above 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg08_k, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mn2o,      &
                         kao_mo3, kbo, kbo_mco2, kbo_mn2o, selfrefo, forrefo,  &
                         cfc12o, cfc22adjo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mco2, kbo_mco2, kao_mn2o,          &
        kbo_mn2o, kao_mo3, cfc12o, cfc22adjo, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mco2 , size ( kbo_mco2 ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mn2o , size ( kao_mn2o ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mn2o , size ( kbo_mn2o ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mo3 , size ( kao_mo3 ) * 4 )
   call wrf_dm_bcast_bytes ( cfc12o , size ( cfc12o ) * 4 )
   call wrf_dm_bcast_bytes ( cfc22adjo , size ( cfc22adjo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb08
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb09(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P=212.7250 mb, T = 223.06 K
!  Upper: P=3.20e-2 mb, T = 197.92 k
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array KBO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level above 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg09_k, only : fracrefao, fracrefbo, kao, kbo, kao_mn2o,           &
                         kbo_mn2o, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mn2o, kbo_mn2o, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mn2o , size ( kao_mn2o ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mn2o , size ( kbo_mn2o ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb09
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb10(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 212.7250 mb, T = 223.06 K
!  Upper: P = 95.58350 mb, T = 215.70 K
!
!  The array KAO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels > ~100mb and temperatures.  The first
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the corresponding TREF for this  pressure level, 
!  JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30, 
!  JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second 
!  index, JP, runs from 1 to 13 and refers to the corresponding 
!  pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).  
!  The third index, IG, goes from 1 to 16, and tells us which 
!  g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg10_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb10
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb11(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P=1053.63 mb, T= 294.2 K
!  Upper: P=0.353 mb, T = 262.11 K
!
!  The array KAO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels > ~100mb and temperatures.  The first
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the corresponding TREF for this  pressure level, 
!  JT = 2 refers to the temperatureTREF-15, JT = 1 is for TREF-30, 
!  JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  The second 
!  index, JP, runs from 1 to 13 and refers to the corresponding 
!  pressure level in PREF (e.g. JP = 1 is for a pressure of 1053.63 mb).  
!  The third index, IG, goes from 1 to 16, and tells us which 
!  g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!
!  The array KBO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level above 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg11_k, only : fracrefao, fracrefbo, kao, kbo, kao_mo2,            &
                         kbo_mo2, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, kao_mo2, kbo_mo2, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mo2 , size ( kao_mo2 ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mo2 , size ( kbo_mo2 ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb11
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb12(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 174.1640 mbar, T= 215.78 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg12_k, only : fracrefao, kao, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
         fracrefao, kao, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb12
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb13(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P=473.4280 mb, T = 259.83 K      
!  Upper: P=4.758820 mb, T = 250.85 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KAO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array KBO_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level above 100~ mb.   The first index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The second index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg13_k, only : fracrefao, fracrefbo, kao, kao_mco2, kao_mco,       &
                         kbo_mo3, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kao_mco2, kao_mco, kbo_mo3, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mco2 , size ( kao_mco2 ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mco , size ( kao_mco ) * 4 )
   call wrf_dm_bcast_bytes ( kbo_mo3 , size ( kbo_mo3 ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb13
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb14(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 142.5940 mb, T = 215.70 K
!  Upper: P = 4.758820 mb, T = 250.85 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg14_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb14
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb15(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 1053. mb, T = 294.2 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KA_Mxx contains the absorption coefficient for 
!  a minor species at the 16 chosen g-values for a reference pressure
!  level below 100~ mb.   The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.  The second index refers to temperature 
!  in 7.2 degree increments.  For instance, JT = 1 refers to a 
!  temperature of 188.0, JT = 2 refers to 195.2, etc. The third index 
!  runs over the g-channel (1 to 16).
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg15_k, only : fracrefao, kao, kao_mn2, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, kao, kao_mn2, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kao_mn2 , size ( kao_mn2 ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb15
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine lw_kgb16(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!  abstract :
!  Arrays fracrefao and fracrefbo are the Planck fractions for the lower
!  and upper atmosphere.
!  Planck fraction mapping levels: 
!  Lower: P = 387.6100 mbar, T = 250.17 K
!  Upper: P=95.58350 mb, T = 215.70 K
!
!  The array KAO contains absorption coefs for each of the 16 g-intervals
!  for a range of pressure levels > ~100mb, temperatures, and ratios
!  of water vapor to CO2.  The first index in the array, JS, runs
!  from 1 to 10, and corresponds to different gas column amount ratios,
!  as expressed through the binary species parameter eta, defined as
!  eta = gas1/(gas1 + (rat) * gas2), where rat is the 
!  ratio of the reference MLS column amount value of gas 1 
!  to that of gas2.
!  The 2nd index in the array, JT, which runs from 1 to 5, corresponds 
!  to different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this  pressure 
!  level, JT = 2 refers to the temperature
!  TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!  is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!  to the reference pressure level (e.g. JP = 1 is for a
!  pressure of 1053.63 mb).  The fourth index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array KBO contains absorption coefs at the 16 chosen g-values 
!  for a range of pressure levels < ~100mb and temperatures. The first 
!  index in the array, JT, which runs from 1 to 5, corresponds to 
!  different temperatures.  More specifically, JT = 3 means that the 
!  data are for the reference temperature TREF for this pressure 
!  level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!  TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!  The second index, JP, runs from 13 to 59 and refers to the JPth
!  reference pressure level (see taumol.f for the value of these
!  pressure levels in mb).  The third index, IG, goes from 1 to 16,
!  and tells us which g-interval the absorption coefficients are for.
!
!  The array FORREFO contains the coefficient of the water vapor
!  foreign-continuum (including the energy term).  The first 
!  index refers to reference temperature (296,260,224,260) and 
!  pressure (970,475,219,3 mbar) levels.  The second index 
!  runs over the g-channel (1 to 16).
!
!  The array SELFREFO contains the coefficient of the water vapor
!  self-continuum (including the energy term).  The first index
!  refers to temperature in 7.2 degree increments.  For instance,
!  JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!  etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrlw_kg16_k, only : fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in   ) :: rrtmg_unit
!
! Local                                    
!
   character*80       :: errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
        fracrefao, fracrefbo, kao, kbo, selfrefo, forrefo
   call wrf_dm_bcast_bytes ( fracrefao , size ( fracrefao ) * 4 )
   call wrf_dm_bcast_bytes ( fracrefbo , size ( fracrefbo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(a,i4)' ) 'module_ra_rrtmg_lw: error reading RRTMG_LW_'// &
                               'DATA on unit ',rrtmg_unit
!
   end subroutine lw_kgb16
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine relcalc(ncol, pcols, pver, t, landfrac, landm, icefrac, rel,snowh)
!-------------------------------------------------------------------------------
!
!  abstract :
!
!  Purpose: 
!  Compute cloud water size
! 
!  Method: 
!  analytic formula following the formulation originally developed by J.T. Kiehl
! 
!  Author: Phil Rasch
!
!  input :
!    landfrac - Land fraction
!    icefrac  - Ice fraction
!    snowh    - Snow depth over land, water equivalent (m)
!    landm    - Land fraction ramping to zero over ocean
!    t        - Temperature
! 
!  output :
!    rel      - Liquid effective drop size (microns)
! 
!-------------------------------------------------------------------------------
!
   implicit none
!
! Input arguments
!
   integer,                     intent(in   ) :: ncol
   integer,                     intent(in   ) :: pcols, pver
   real, dimension(pcols),      intent(in   ) :: landfrac
   real, dimension(pcols),      intent(in   ) :: icefrac
   real, dimension(pcols),      intent(in   ) :: snowh
   real, dimension(pcols),      intent(in   ) :: landm
   real, dimension(pcols,pver), intent(in   ) :: t
!
! Output arguments
!
   real, dimension(pcols,pver), intent(  out) :: rel
!
! Local
!
   integer :: i, k             ! lon, lev indices
   real    :: tmelt            ! freezing temperature of fresh water (K)
   real    :: rliqland         ! liquid drop size if over land
   real    :: rliqocean        ! liquid drop size if over ocean
   real    :: rliqice          ! liquid drop size if over sea ice
!-------------------------------------------------------------------------------
!
   tmelt = 273.16
   rliqocean = 14.0
   rliqice   = 14.0
   rliqland  = 8.0
!
   do k = 1,pver
     do i = 1,ncol
!
! jrm Reworked effective radius algorithm
! Start with temperature-dependent value appropriate for continental air
! Note: findmcnew has a pressure dependence here
!
       rel(i,k) = rliqland + (rliqocean-rliqland)                              &
                             *min(1.0,max(0.0,(tmelt-t(i,k))*0.05))
!
! Modify for snow depth over land
!
       rel(i,k) = rel(i,k) + (rliqocean-rel(i,k))*min(1.0,max(0.0,snowh(i)*10.))
!
! Ramp between polluted value over land to clean value over ocean.
!
       rel(i,k) = rel(i,k) + (rliqocean-rel(i,k))*min(1.0,max(0.0,1.0-landm(i)))
!
! Ramp between the resultant value and a sea ice value in the presence of ice.
!
       rel(i,k) = rel(i,k) + (rliqice-rel(i,k))*min(1.0,max(0.0,icefrac(i)))
!
! end jrm
!
     enddo
   enddo
!
   end subroutine relcalc
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine reicalc(ncol, pcols, pver, t, re)
!-------------------------------------------------------------------------------
!
   integer,                     intent(in   ) :: ncol, pcols, pver
   real, dimension(pcols,pver), intent(in   ) :: t
   real, dimension(pcols,pver), intent(  out) :: re
!
! local variables
!
   real    :: corr
   integer :: i, k, index
!-------------------------------------------------------------------------------
!
! Tabulated values of re(T) in the temperature interval
! 180 K -- 274 K; hexagonal columns assumed:
!
   do k = 1,pver
     do i = 1,ncol
       index = int(t(i,k)-179.)
       index = min(max(index,1),94)
       corr = t(i,k) - int(t(i,k))
       re(i,k) = retab(index)*(1.-corr) + retab(index+1)*corr
!      re(i,k) = amax1(amin1(re(i,k),30.),10.)
     enddo
   enddo
!
   return
   end subroutine reicalc
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module module_ra_rrtmg_lwk
!-------------------------------------------------------------------------------
#endif
