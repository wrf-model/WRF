#if( BUILD_RRTMK != 1)
      MODULE module_ra_rrtmg_swk
      CONTAINS
      SUBROUTINE rrtmg_sw
         REAL :: dummy
         dummy = 1
      END SUBROUTINE rrtmg_sw
      END MODULE module_ra_rrtmg_swk
#else
!
!-------------------------------------------------------------------------------
   module parrrsw_k
!-------------------------------------------------------------------------------
!  abstract : rrtmg_sw main parameters
!  
!  history log :
!    1998-07 JJMorcrette  Initial version
!    2006-06 MJIacono     Revised
!    2008-08 MJIacono     Revised
! 
!  variable : 
!-------------------------------------------------------------------------------
!
!  name     type     purpose
! -----  :  ----   : -----------------------------------------------------------
! mxlay  :  integer: maximum number of layers
! mg     :  integer: number of original g-intervals per spectral band
! nbndsw :  integer: number of spectral bands
! naerec :  integer: number of aerosols (iaer=6, ecmwf aerosol option)
! ngptsw :  integer: total number of reduced g-intervals for rrtmg_lw
! ngNN   :  integer: number of reduced g-intervals per spectral band
! ngsNN  :  integer: cumulative number of g-intervals per band
!-------------------------------------------------------------------------------
   use parkind_k , only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
   integer(kind=im), parameter :: mxlay  = 203    !jplay, klev
   integer(kind=im), parameter :: mg     = 16     !jpg
   integer(kind=im), parameter :: nbndsw = 14     !jpsw, ksw
   integer(kind=im), parameter :: naerec  = 6     !jpaer
   integer(kind=im), parameter :: mxmol  = 38
   integer(kind=im), parameter :: nstr   = 2
   integer(kind=im), parameter :: nmol   = 7
!
! Use for 112 g-point model   
!
   integer(kind=im), parameter :: ngptsw = 112    !jpgpt
!
! Use for 224 g-point model   
!      integer(kind=im), parameter :: ngptsw = 224   !jpgpt
!
! may need to rename these - from v2.6
!
   integer(kind=im), parameter :: jpband   = 29
   integer(kind=im), parameter :: jpb1     = 16   !istart
   integer(kind=im), parameter :: jpb2     = 29   !iend
   integer(kind=im), parameter :: jmcmu    = 32
   integer(kind=im), parameter :: jmumu    = 32
   integer(kind=im), parameter :: jmphi    = 3
   integer(kind=im), parameter :: jmxang   = 4
   integer(kind=im), parameter :: jmxstr   = 16
!
! Use for 112 g-point model   
!
   integer(kind=im), parameter :: ng16 = 6
   integer(kind=im), parameter :: ng17 = 12
   integer(kind=im), parameter :: ng18 = 8
   integer(kind=im), parameter :: ng19 = 8
   integer(kind=im), parameter :: ng20 = 10
   integer(kind=im), parameter :: ng21 = 10
   integer(kind=im), parameter :: ng22 = 2
   integer(kind=im), parameter :: ng23 = 10
   integer(kind=im), parameter :: ng24 = 8
   integer(kind=im), parameter :: ng25 = 6
   integer(kind=im), parameter :: ng26 = 6
   integer(kind=im), parameter :: ng27 = 8
   integer(kind=im), parameter :: ng28 = 6
   integer(kind=im), parameter :: ng29 = 12
   integer(kind=im), parameter :: ngs16 = 6
   integer(kind=im), parameter :: ngs17 = 18
   integer(kind=im), parameter :: ngs18 = 26
   integer(kind=im), parameter :: ngs19 = 34
   integer(kind=im), parameter :: ngs20 = 44
   integer(kind=im), parameter :: ngs21 = 54
   integer(kind=im), parameter :: ngs22 = 56
   integer(kind=im), parameter :: ngs23 = 66
   integer(kind=im), parameter :: ngs24 = 74
   integer(kind=im), parameter :: ngs25 = 80
   integer(kind=im), parameter :: ngs26 = 86
   integer(kind=im), parameter :: ngs27 = 94
   integer(kind=im), parameter :: ngs28 = 100
   integer(kind=im), parameter :: ngs29 = 112
!
! Use for 224 g-point model   
!  integer(kind=im), parameter :: ng16 = 16
!  integer(kind=im), parameter :: ng17 = 16
!  integer(kind=im), parameter :: ng18 = 16
!  integer(kind=im), parameter :: ng19 = 16
!  integer(kind=im), parameter :: ng20 = 16
!  integer(kind=im), parameter :: ng21 = 16
!  integer(kind=im), parameter :: ng22 = 16
!  integer(kind=im), parameter :: ng23 = 16
!  integer(kind=im), parameter :: ng24 = 16
!  integer(kind=im), parameter :: ng25 = 16
!  integer(kind=im), parameter :: ng26 = 16
!  integer(kind=im), parameter :: ng27 = 16
!  integer(kind=im), parameter :: ng28 = 16
!  integer(kind=im), parameter :: ng29 = 16
!
!  integer(kind=im), parameter :: ngs16 = 16
!  integer(kind=im), parameter :: ngs17 = 32
!  integer(kind=im), parameter :: ngs18 = 48
!  integer(kind=im), parameter :: ngs19 = 64
!  integer(kind=im), parameter :: ngs20 = 80
!  integer(kind=im), parameter :: ngs21 = 96
!  integer(kind=im), parameter :: ngs22 = 112
!  integer(kind=im), parameter :: ngs23 = 128
!  integer(kind=im), parameter :: ngs24 = 144
!  integer(kind=im), parameter :: ngs25 = 160
!  integer(kind=im), parameter :: ngs26 = 176
!  integer(kind=im), parameter :: ngs27 = 192
!  integer(kind=im), parameter :: ngs28 = 208
!  integer(kind=im), parameter :: ngs29 = 224
!
! Source function solar constant
!
   real(kind=rb), parameter :: rrsw_scon = 1.36822e+03     ! W/m2
!-------------------------------------------------------------------------------
   end module parrrsw_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_aer_k
!-------------------------------------------------------------------------------
!  abstract :
!    rrtmg_sw aerosol optical properties
!    Data derived from six ECMWF aerosol types and defined for
!    the rrtmg_sw spectral intervals
!
!  history log :
!    2003-03 J.-J. Morcrette, ECMWF Initial 
!    2006-07 MJIacono         Revised
!    2008-08 MJIacono         Revised
!-------------------------------------------------------------------------------
!
!-- The six ECMWF aerosol types ar e respectively:
!
!  1/ continental average                 2/ maritime
!  3/ desert                              4/ urban
!  5/ volcanic active                     6/ stratospheric background
!
! computed from Hess and Koepke (con, mar, des, urb)
!          from Bonnel et al.   (vol, str)
!
! rrtmg_sw 14 spectral intervals (microns):
!  3.846 -  3.077
!  3.077 -  2.500
!  2.500 -  2.150
!  2.150 -  1.942
!  1.942 -  1.626
!  1.626 -  1.299
!  1.299 -  1.242
!  1.242 -  0.7782
!  0.7782-  0.6250
!  0.6250-  0.4415
!  0.4415-  0.3448
!  0.3448-  0.2632
!  0.2632-  0.2000
! 12.195 -  3.846
!
!-------------------------------------------------------------------------------
!
!  name     type     purpose
! -----   : ----   : ----------------------------------------------
! rsrtaua : real   : ratio of average optical thickness in 
!                    spectral band to that at 0.55 micron
! rsrpiza : real   : average single scattering albedo (unitless)
! rsrasya : real   : average asymmetry parameter (unitless)
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : nbndsw, naerec
!
!  implicit none
!
   save
   real(kind=rb), dimension(nbndsw,naerec) :: rsrtaua
   real(kind=rb), dimension(nbndsw,naerec) :: rsrpiza
   real(kind=rb), dimension(nbndsw,naerec) :: rsrasya
!-------------------------------------------------------------------------------
   end module rrsw_aer_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_cld_k
!-------------------------------------------------------------------------------
! abstract :
!   rrtmg_sw cloud property coefficients
!
! history log : 
!   1999-08 J.-J. Morcrette, ECMWF      Initial
!   2005-08 J. Delamere/MJIacono, AER,  Revised
!   2005-11 MJIacono, AER               Revised
!   2008-08 MJIacono, AER               Revised
!-------------------------------------------------------------------------------
!
!  name     type     purpose
! -----  :  ----   : --------------------------------------------------------
! xxxliq1 : real   : optical properties (extinction coefficient, single 
!                    scattering albedo, assymetry factor) from
!                    Hu & Stamnes, j. clim., 6, 728-742, 1993.  
! xxxice2 : real   : optical properties (extinction coefficient, single 
!                    scattering albedo, assymetry factor) from streamer v3.0,
!                    Key, streamer user's guide, cooperative institude 
!                    for meteorological studies, 95 pp., 2001.
! xxxice3 : real   : optical properties (extinction coefficient, single 
!                    scattering albedo, assymetry factor) from
!                    Fu, j. clim., 9, 1996.
! xbari   : real   : optical property coefficients for five spectral 
!                    intervals (2857-4000, 4000-5263, 5263-7692, 7692-14285,
!                    and 14285-40000 wavenumbers) following 
!                    Ebert and Curry, jgr, 97, 3831-3836, 1992.
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
   real(kind=rb), dimension(58,16:29) :: extliq1, ssaliq1, asyliq1
   real(kind=rb), dimension(43,16:29) :: extice2, ssaice2, asyice2
   real(kind=rb), dimension(46,16:29) :: extice3, ssaice3, asyice3
   real(kind=rb), dimension(46,16:29) :: fdlice3
   real(kind=rb), dimension(5) :: abari, bbari, cbari, dbari, ebari, fbari
!-------------------------------------------------------------------------------
   end module rrsw_cld_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_con_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw constants
!
!  history log : 
!    2006-06 MJIacono AER    Initial
!    2008-08 MJIacono AER    Revised
!-------------------------------------------------------------------------------
!  name     type     purpose
! -----  :  ----   : -----------------------------------------------------------
! fluxfac:  real   : radiance to flux conversion factor 
! heatfac:  real   : flux to heating rate conversion factor
!oneminus:  real   : 1.-1.e-6
! pi     :  real   : pi
! grav   :  real   : acceleration of gravity
! planck :  real   : planck constant
! boltz  :  real   : boltzmann constant
! clight :  real   : speed of light
! avogad :  real   : avogadro constant 
! alosmt :  real   : loschmidt constant
! gascon :  real   : molar gas constant
! radcn1 :  real   : first radiation constant
! radcn2 :  real   : second radiation constant
! sbcnst :  real   : stefan-boltzmann constant
!  secdy :  real   : seconds per day
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
   real(kind=rb) :: fluxfac, heatfac
   real(kind=rb) :: oneminus, pi, grav
   real(kind=rb) :: planck, boltz, clight
   real(kind=rb) :: avogad, alosmt, gascon
   real(kind=rb) :: radcn1, radcn2
   real(kind=rb) :: sbcnst, secdy
!-------------------------------------------------------------------------------
   end module rrsw_con_k
!-------------------------------------------------------------------------------
!
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg16_k
!-------------------------------------------------------------------------------
! abstract : 
!   rrtmg_sw ORIGINAL abs. coefficients for interval 16
!   band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 16
! band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng16
!
!  implicit none
!
   save
   integer(kind=im), parameter           :: no16 = 16
   real(kind=rb), dimension(9,5,13,no16) :: kao
   real(kind=rb), dimension(5,13:59,no16):: kbo
   real(kind=rb), dimension(10,no16)     :: selfrefo
   real(kind=rb), dimension(3,no16)      :: forrefo
   real(kind=rb), dimension(no16)        :: sfluxrefo
   integer(kind=im)                      :: layreffr
   real(kind=rb)                         :: rayl, strrat1
!
   real(kind=rb), dimension(9,5,13,ng16) :: ka
   real(kind=rb), dimension(585,ng16)    :: absa
   real(kind=rb), dimension(5,13:59,ng16):: kb 
   real(kind=rb), dimension(235,ng16)    :: absb
   real(kind=rb), dimension(10,ng16)     :: selfref
   real(kind=rb), dimension(3,ng16)      :: forref
   real(kind=rb), dimension(ng16)        :: sfluxref
!
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg16_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg17_k
!-------------------------------------------------------------------------------
! abstract : 
!   rrtmg_sw ORIGINAL abs. coefficients for interval 17
!   band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 17
! band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng17
!-------------------------------------------------------------------------------
!
!  implicit none
!
   save
   integer(kind=im), parameter             :: no17 = 16
!
   real(kind=rb), dimension(9,5,13,no17)   :: kao
   real(kind=rb), dimension(5,5,13:59,no17):: kbo
   real(kind=rb), dimension(10,no17)       :: selfrefo
   real(kind=rb), dimension(4,no17)        :: forrefo
   real(kind=rb), dimension(no17,5)        :: sfluxrefo
!
   integer(kind=im)                        :: layreffr
   real(kind=rb)                           :: rayl, strrat
!
   real(kind=rb), dimension(9,5,13,ng17)   :: ka 
   real(kind=rb), dimension(585,ng17)      :: absa
   real(kind=rb), dimension(5,5,13:59,ng17):: kb
   real(kind=rb), dimension(1175,ng17)     :: absb
   real(kind=rb), dimension(10,ng17)       :: selfref
   real(kind=rb), dimension(4,ng17)        :: forref
   real(kind=rb), dimension(ng17,5)        :: sfluxref
!
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg17_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg18_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw ORIGINAL abs. coefficients for interval 18
!    band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!
!  history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 18
! band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng18
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no18 = 16
!
   real(kind=rb), dimension(9,5,13,no18)   :: kao
   real(kind=rb), dimension(5,13:59,no18)  :: kbo
   real(kind=rb), dimension(10,no18)       :: selfrefo
   real(kind=rb), dimension(3,no18)        :: forrefo
   real(kind=rb), dimension(no18,9)        :: sfluxrefo
!
   integer(kind=im)                        :: layreffr
   real(kind=rb)                           :: rayl, strrat
!
   real(kind=rb), dimension(9,5,13,ng18)   :: ka
   real(kind=rb), dimension(585,ng18)      :: absa
   real(kind=rb), dimension(5,13:59,ng18)  :: kb
   real(kind=rb), dimension(235,ng18)      :: absb
   real(kind=rb), dimension(10,ng18)       :: selfref
   real(kind=rb), dimension(3,ng18)        :: forref
   real(kind=rb), dimension(ng18,9)        :: sfluxref
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg18_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg19_k
!-------------------------------------------------------------------------------
! abstract :
!   rrtmg_sw ORIGINAL abs. coefficients for interval 19
!    band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 19
! band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng19
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no19 = 16
!
   real(kind=rb), dimension(9,5,13,no19)   :: kao
   real(kind=rb), dimension(5,13:59,no19)  :: kbo
   real(kind=rb), dimension(10,no19)       :: selfrefo
   real(kind=rb), dimension(3,no19)        :: forrefo
   real(kind=rb), dimension(no19,9)        :: sfluxrefo
!
   integer(kind=im)                        :: layreffr
   real(kind=rb)                           :: rayl, strrat
!
   real(kind=rb), dimension(9,5,13,ng19)   :: ka
   real(kind=rb), dimension(585,ng19)      :: absa
   real(kind=rb), dimension(5,13:59,ng19)  :: kb
   real(kind=rb), dimension(235,ng19)      :: absb
   real(kind=rb), dimension(10,ng19)       :: selfref
   real(kind=rb), dimension(3,ng19)        :: forref
   real(kind=rb), dimension(ng19,9)        :: sfluxref
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg19_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg20_k
!-------------------------------------------------------------------------------
! abstract : 
!   rrtmg_sw ORIGINAL abs. coefficients for interval 20
!   band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
! absch4o : real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 20
! band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
! absch4  : real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng20
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no20 = 16
!
   real(kind=rb), dimension(5,13,no20)     :: kao
   real(kind=rb), dimension(5,13:59,no20)  :: kbo
   real(kind=rb), dimension(10,no20)       :: selfrefo
   real(kind=rb), dimension(4,no20)        :: forrefo
   real(kind=rb), dimension(no20)          :: sfluxrefo
   real(kind=rb), dimension(no20)          :: absch4o
!
   integer(kind=im)                        :: layreffr
   real(kind=rb)                           :: rayl, strrat
!
   real(kind=rb), dimension(5,13,ng20)     :: ka
   real(kind=rb), dimension(65,ng20)       :: absa
   real(kind=rb), dimension(5,13:59,ng20)  :: kb
   real(kind=rb), dimension(235,ng20)      :: absb
   real(kind=rb), dimension(10,ng20)       :: selfref
   real(kind=rb), dimension(4,ng20)        :: forref
   real(kind=rb), dimension(ng20)          :: sfluxref
   real(kind=rb), dimension(no20)          :: absch4
   equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg20_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg21_k
!-------------------------------------------------------------------------------
!  abstract :
!    rrtmg_sw ORIGINAL abs. coefficients for interval 21
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 21
! band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng21
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no21 = 16
!
   real(kind=rb), dimension(9,5,13,no21)     :: kao
   real(kind=rb), dimension(5,5,13:59,no21)  :: kbo
   real(kind=rb), dimension(10,no21)         :: selfrefo
   real(kind=rb), dimension(4,no21)          :: forrefo
   real(kind=rb), dimension(no21,9)          :: sfluxrefo
!
   integer(kind=im)                          :: layreffr
   real(kind=rb)                             :: rayl, strrat
!
   real(kind=rb), dimension(9,5,13,ng21)     :: ka
   real(kind=rb), dimension(585,ng21)        :: absa
   real(kind=rb), dimension(5,5,13:59,ng21)  :: kb
   real(kind=rb), dimension(1175,ng21)       :: absb
   real(kind=rb), dimension(10,ng21)         :: selfref
   real(kind=rb), dimension(4,ng21)          :: forref
   real(kind=rb), dimension(ng21,9)          :: sfluxref
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg21_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg22_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw ORIGINAL abs. coefficients for interval 22
!    band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 22
! band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng22
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no22 = 16
!
   real(kind=rb), dimension(9,5,13,no22)     :: kao
   real(kind=rb), dimension(5,13:59,no22)    :: kbo
   real(kind=rb), dimension(10,no22)         :: selfrefo
   real(kind=rb), dimension(3,no22)          :: forrefo
   real(kind=rb), dimension(no22,9)          :: sfluxrefo
!
   integer(kind=im)                          :: layreffr
   real(kind=rb)                             :: rayl, strrat
!
   real(kind=rb), dimension(9,5,13,ng22)     :: ka
   real(kind=rb), dimension(585,ng22)        :: absa
   real(kind=rb), dimension(5,13:59,ng22)    :: kb
   real(kind=rb), dimension(235,ng22)        :: absb
   real(kind=rb), dimension(10,ng22)         :: selfref
   real(kind=rb), dimension(3,ng22)          :: forref
   real(kind=rb), dimension(ng22,9)          :: sfluxref
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg22_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg23_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw ORIGINAL abs. coefficients for interval 23
!    band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!
!  history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 23
! band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng23
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no23 = 16
!
   real(kind=rb), dimension(5,13,no23)       :: kao
   real(kind=rb), dimension(10,no23)         :: selfrefo
   real(kind=rb), dimension(3,no23)          :: forrefo
   real(kind=rb), dimension(no23)            :: sfluxrefo
   real(kind=rb), dimension(no23)            :: raylo
!
   integer(kind=im)                          :: layreffr
   real(kind=rb)                             :: givfac
!
   real(kind=rb), dimension(5,13,ng23)       :: ka
   real(kind=rb), dimension(65,ng23)         :: absa
   real(kind=rb), dimension(10,ng23)         :: selfref
   real(kind=rb), dimension(3,ng23)          :: forref
   real(kind=rb), dimension(ng23)            :: sfluxref
   real(kind=rb), dimension(no23)            :: rayl
   equivalence (ka(1,1,1),absa(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg23_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg24_k
!-------------------------------------------------------------------------------
! abstract :
!   rrtmg_sw ORIGINAL abs. coefficients for interval 24
!   band 24: 12850-16000 cm-1 (low - h2o,o2; high - o2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
! abso3ao : real     
! abso3bo : real     
! raylao  : real     
! raylbo  : real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 24
! band 24: 12850-16000 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
! sfluxref: real     
! abso3a  : real     
! abso3b  : real     
! rayla   : real     
! raylb   : real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng24
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no24 = 16
!
   real(kind=rb), dimension(9,5,13,no24)     :: kao
   real(kind=rb), dimension(5,13:59,no24)    :: kbo
   real(kind=rb), dimension(10,no24)         :: selfrefo
   real(kind=rb), dimension(3,no24)          :: forrefo
   real(kind=rb), dimension(no24,9)          :: sfluxrefo
   real(kind=rb), dimension(no24)            :: abso3ao
   real(kind=rb), dimension(no24)            :: abso3bo
   real(kind=rb), dimension(no24,9)          :: raylao
   real(kind=rb), dimension(no24)            :: raylbo
!
   integer(kind=im)                          :: layreffr
   real(kind=rb)                             :: strrat
!
   real(kind=rb), dimension(9,5,13,ng24)     :: ka
   real(kind=rb), dimension(585,ng24)        :: absa
   real(kind=rb), dimension(5,13:59,ng24)    :: kb
   real(kind=rb), dimension(235,ng24)        :: absb
   real(kind=rb), dimension(10,ng24)         :: selfref
   real(kind=rb), dimension(3,ng24)          :: forref
   real(kind=rb), dimension(ng24,9)          :: sfluxref
   real(kind=rb), dimension(ng24)            :: abso3a 
   real(kind=rb), dimension(ng24)            :: abso3b
   real(kind=rb), dimension(ng24,9)          :: rayla
   real(kind=rb), dimension(ng24)            :: raylb
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg24_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg25_k
!-------------------------------------------------------------------------------
!  abstract :
!    rrtmg_sw ORIGINAL abs. coefficients for interval 25
!    band 25: 16000-22650 cm-1 (low - h2o; high - nothing)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
!sfluxrefo: real     
! abso3ao : real     
! abso3bo : real     
! raylo   : real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 25
! band 25: 16000-22650 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! absa    : real
! sfluxref: real     
! abso3a  : real     
! abso3b  : real     
! rayl    : real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng25
!
!  implicit none
!
   save
   integer(kind=im), parameter          :: no25 = 16
!
   real(kind=rb), dimension(5,13,no25)  :: kao
   real(kind=rb), dimension(no25)       :: sfluxrefo
   real(kind=rb), dimension(no25)       :: abso3ao
   real(kind=rb), dimension(no25)       :: abso3bo
   real(kind=rb), dimension(no25)       :: raylo
!
   integer(kind=im)                     :: layreffr
!
   real(kind=rb), dimension(5,13,ng25)  :: ka
   real(kind=rb), dimension(65,ng25)    :: absa
   real(kind=rb), dimension(ng25)       :: sfluxref
   real(kind=rb), dimension(ng25)       :: abso3a
   real(kind=rb), dimension(ng25)       :: abso3b
   real(kind=rb), dimension(ng25)       :: rayl
   equivalence (ka(1,1,1),absa(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg25_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg26_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw ORIGINAL abs. coefficients for interval 26
!    band 26: 22650-29000 cm-1 (low - nothing; high - nothing)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!sfluxrefo: real     
! raylo   : real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 26
! band 26: 22650-29000 cm-1 (low - nothing; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! sfluxref: real     
! rayl    : real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng26
!
!  implicit none
!
   save
   integer(kind=im), parameter     :: no26 = 16
!
   real(kind=rb), dimension(no26) :: sfluxrefo
   real(kind=rb), dimension(no26) :: raylo
!
   real(kind=rb), dimension(ng26) :: sfluxref
   real(kind=rb), dimension(ng26) :: rayl
!-------------------------------------------------------------------------------
   end module rrsw_kg26_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg27_k
!-------------------------------------------------------------------------------
! abstract :
!   rrtmg_sw ORIGINAL abs. coefficients for interval 27
!   band 27: 29000-38000 cm-1 (low - o3; high - o3)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
!sfluxrefo: real     
! raylo   : real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 27
! band 27: 29000-38000 cm-1 (low - o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! sfluxref: real     
! rayl    : real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng27
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no27 = 16
!
   real(kind=rb), dimension(5,13,no27)    :: kao
   real(kind=rb), dimension(5,13:59,no27) :: kbo
   real(kind=rb), dimension(no27)         :: sfluxrefo
   real(kind=rb), dimension(no27)         :: raylo
!
   integer(kind=im)                       :: layreffr
   real(kind=rb)                          :: scalekur
!
   real(kind=rb), dimension(5,13,ng27)    :: ka
   real(kind=rb), dimension(65,ng27)      :: absa
   real(kind=rb), dimension(5,13:59,ng27) :: kb
   real(kind=rb), dimension(235,ng27)     :: absb
   real(kind=rb), dimension(ng27)         :: sfluxref
   real(kind=rb), dimension(ng27)         :: rayl
!
   equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg27_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg28_k
!-------------------------------------------------------------------------------
!  abstract :
!    rrtmg_sw ORIGINAL abs. coefficients for interval 28
!    band 28: 38000-50000 cm-1 (low - o3, o2; high - o3, o2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
!sfluxrefo: real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 28
! band 28: 38000-50000 cm-1 (low - o3, o2; high - o3, o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! sfluxref: real     
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng28
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no28 = 16
!
   real(kind=rb), dimension(9,5,13,no28)    :: kao
   real(kind=rb), dimension(5,5,13:59,no28) :: kbo
   real(kind=rb), dimension(no28,5)         :: sfluxrefo
!
   integer(kind=im)                         :: layreffr
   real(kind=rb)                            :: rayl, strrat
!
   real(kind=rb), dimension(9,5,13,ng28)    :: ka
   real(kind=rb), dimension(585,ng28)       :: absa
   real(kind=rb), dimension(5,5,13:59,ng28) :: kb
   real(kind=rb), dimension(1175,ng28)      :: absb
   real(kind=rb), dimension(ng28,5)         :: sfluxref
   equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg28_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_kg29_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw ORIGINAL abs. coefficients for interval 29
!    band 29:  820-2600 cm-1 (low - h2o; high - co2)
!
! history log : 
!   1999-10 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!sfluxrefo: real     
! absh2oo : real     
! absco2o : real     
!-------------------------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 29
! band 29:  820-2600 cm-1 (low - h2o; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-------------------------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
! sfluxref: real     
! absh2o  : real     
! absco2  : real     
!-------------------------------------------------------------------------------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ng29
!
!  implicit none
!
   save
   integer(kind=im), parameter :: no29 = 16
!
   real(kind=rb), dimension(5,13,no29)    :: kao
   real(kind=rb), dimension(5,13:59,no29) :: kbo
   real(kind=rb), dimension(10,no29)      :: selfrefo
   real(kind=rb), dimension(4,no29)       :: forrefo
   real(kind=rb), dimension(no29)         :: sfluxrefo
   real(kind=rb), dimension(no29)         :: absh2oo, absco2o
!
   integer(kind=im)                       :: layreffr
   real(kind=rb)                          :: rayl
!
   real(kind=rb), dimension(5,13,ng29)    :: ka
   real(kind=rb), dimension(65,ng29)      :: absa
   real(kind=rb), dimension(5,13:59,ng29) :: kb
   real(kind=rb), dimension(235,ng29)     :: absb
   real(kind=rb), dimension(10,ng29)      :: selfref
   real(kind=rb), dimension(4,ng29)       ::  forref
   real(kind=rb), dimension(ng29)         :: sfluxref
   real(kind=rb), dimension(ng29)         :: absh2o,absco2
   equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))
!-------------------------------------------------------------------------------
   end module rrsw_kg29_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_ref_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw reference atmosphere 
!    Based on standard mid-latitude summer profile
!
! history log : 
!   1998-07 JJMorcrette  Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! pref   :  real   : Reference pressure levels
! preflog:  real   : Reference pressure levels, ln(pref)
! tref   :  real   : Reference temperature levels for MLS profile
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
   real(kind=rb) , dimension(59) :: pref
   real(kind=rb) , dimension(59) :: preflog
   real(kind=rb) , dimension(59) :: tref
!-------------------------------------------------------------------------------
   end module rrsw_ref_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_tbl_k
!-------------------------------------------------------------------------------
!  abstract :
!    rrtmg_sw lookup table arrays
!
!  history log : 
!   2007-05 MJIAcono     Initial version
!   2006-07 MJIacono     Revised, AER
!   2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! ntbl   :  integer: Lookup table dimension
! tblint :  real   : Lookup table conversion factor
! tau_tbl:  real   : Clear-sky optical depth 
! exp_tbl:  real   : Exponential lookup table for transmittance
! od_lo  :  real   : Value of tau below which expansion is used
!                  : in place of lookup table
! pade   :  real   : Pade approximation constant
! bpade  :  real   : Inverse of Pade constant
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
!
!  implicit none
!
   save
   integer(kind=im), parameter      :: ntbl = 10000
   real(kind=rb), parameter         :: tblint = 10000.0_rb
   real(kind=rb), parameter         :: od_lo = 0.06_rb
   real(kind=rb)                    :: tau_tbl
   real(kind=rb), dimension(0:ntbl) :: exp_tbl
!
   real(kind=rb), parameter         :: pade = 0.278_rb
   real(kind=rb)                    :: bpade
!
   end module rrsw_tbl_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_vsn_k
!-------------------------------------------------------------------------------
!  abstract : 
!    rrtmg_sw version information
!
!  history log : 
!    1998-07 JJMorcrette  Initial version
!    2006-07 MJIacono     Revised, AER
!    2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
!hnamrtm :character: 
!hnamini :character: 
!hnamcld :character: 
!hnamclc :character: 
!hnamrft :character: 
!hnamspv :character: 
!hnamspc :character: 
!hnamset :character: 
!hnamtau :character: 
!hnamvqd :character: 
!hnamatm :character: 
!hnamutl :character: 
!hnamext :character: 
!hnamkg  :character: 
!
! hvrrtm :character: 
! hvrini :character: 
! hvrcld :character: 
! hvrclc :character: 
! hvrrft :character: 
! hvrspv :character: 
! hvrspc :character: 
! hvrset :character: 
! hvrtau :character: 
! hvrvqd :character: 
! hvratm :character: 
! hvrutl :character: 
! hvrext :character: 
! hvrkg  :character: 
!-------------------------------------------------------------------------------
!
!  implicit none
!
   save
   character*18 hvrrtm,hvrini,hvrcld,hvrclc,hvrrft,hvrspv,                     &
                hvrspc,hvrset,hvrtau,hvrvqd,hvratm,hvrutl,hvrext
   character*20 hnamrtm,hnamini,hnamcld,hnamclc,hnamrft,hnamspv,               &
                hnamspc,hnamset,hnamtau,hnamvqd,hnamatm,hnamutl,hnamext
!
   character*18 hvrkg
   character*20 hnamkg
!-------------------------------------------------------------------------------
   end module rrsw_vsn_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrsw_wvn_k
!-------------------------------------------------------------------------------
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : nbndsw, mg, ngptsw, jpb1, jpb2
!-------------------------------------------------------------------------------
!  abstract :
!    rrtmg_sw spectral information
!
!  history log : 
!    1998-07 JJMorcrette  Initial version
!    2006-07 MJIacono     Revised, AER
!    2008-08 MJIacono     Revised, AER
!-------------------------------------------------------------------------------
!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! ng     :  integer: Number of original g-intervals in each spectral band
! nspa   :  integer: 
! nspb   :  integer: 
!wavenum1:  real   : Spectral band lower boundary in wavenumbers
!wavenum2:  real   : Spectral band upper boundary in wavenumbers
! delwave:  real   : Spectral band width in wavenumbers
!
! ngc    :  integer: The number of new g-intervals in each band
! ngs    :  integer: The cumulative sum of new g-intervals for each band
! ngm    :  integer: The index of each new g-interval relative to the
!                    original 16 g-intervals in each band
! ngn    :  integer: The number of original g-intervals that are 
!                    combined to make each new g-intervals in each band
! ngb    :  integer: The band index for each new g-interval
! wt     :  real   : RRTM weights for the original 16 g-intervals
! rwgt   :  real   : Weights for combining original 16 g-intervals 
!                    (224 total) into reduced set of g-intervals 
!                    (112 total)
!-------------------------------------------------------------------------------
!
!  implicit none
!
   save
   integer(kind=im), dimension(jpb1:jpb2) :: ng, nspa, nspb
!
   real(kind=rb), dimension(jpb1:jpb2)    :: wavenum1, wavenum2, delwave
!
   integer(kind=im), dimension(nbndsw)    :: ngc, ngs
   integer(kind=im), dimension(ngptsw)    :: ngn, ngb
   integer(kind=im), dimension(nbndsw*mg) :: ngm
!
   real(kind=rb), dimension(mg)           :: wt
   real(kind=rb), dimension(nbndsw*mg)    :: rwgt
!-------------------------------------------------------------------------------
   end module rrsw_wvn_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_cldprmc_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : ngptsw, jpband, jpb1, jpb2
   use rrsw_cld_k, only : extliq1, ssaliq1, asyliq1,                             &
                        extice2, ssaice2, asyice2,                             &
                        extice3, ssaice3, asyice3, fdlice3,                    &
                        abari, bbari, cbari, dbari, ebari, fbari
   use rrsw_wvn_k, only : wavenum1, wavenum2, ngb
   use rrsw_vsn_k, only : hvrclc, hnamclc
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
   subroutine cldprmc_sw(nlayers, inflag, iceflag, liqflag, cldfmc,            &
                         ciwpmc, clwpmc, reicmc, relqmc,                       &
                            cswpmc, resnmc,                                    &
                         dtliq, dtice, dtsno, dwliq, dwice, dwsno,             & 
                         daliq, daice, dasno,                                  & 
                         taormc, taucmc, ssacmc, asmcmc, fsfcmc)
!-------------------------------------------------------------------------------
!
!  abstract: 
!    Compute the cloud optical properties for each cloudy layer
!    and g-point interval for use by the McICA method.  
!    Note: Only inflag = 0 and inflag=2/liqflag=1/iceflag=2,3 are available;
!
!  history log :
! 
!  reference : 
!    Hu & Stamnes, Key, and Fu
!
!  variables :
!  input : 
!    nlayer - total number of layers
!    inflag - see definitions
!    iceflag - see definitions
!    liqflag - see definitions
!    cldfmc(ngptsw,nlayers) - cloud fraction [mcica]
!    ciwpmc(ngptsw,nlayers) - cloud ice water path [mcica]
!    clwpmc(ngptsw,nlayers) - cloud liquid water path [mcica]
!    cswpmc(ngptsw,nlayers) - cloud snow water path [mcica]
!    relqmc(nlayers) -  cloud liquid particle effective radius (microns)
!    reicmc(nlayers) - cloud ice particle effective radius (microns)
!    resnmc(nlayers) -  cloud snow particle effective radius (microns)
!
!    specific definition of reicmc depends on setting of iceflag:
!    iceflag = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
!                r_ec range is limited to 13.0 to 130.0 microns
!    iceflag = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
!               r_k range is limited to 5.0 to 131.0 microns
!    iceflag = 3: generalized effective size, dge, (Fu, 1996),
!              dge range is limited to 5.0 to 140.0 microns
!              [dge = 1.0315 * r_ec]
!    fsfcmc(ngptsw,nlayers) - cloud forward scattering fraction
!
!  output : 
!    taucmc(ngptsw,nlayers) -  cloud optical depth (delta scaled)
!    ssacmc(ngptsw,nlayers) -  single scattering albedo (delta scaled)
!    asmcmc(ngptsw,nlayers) -  asymmetry parameter (delta scaled)
!    taormc(ngptsw,nlayers) -  cloud optical depth (non-delta scaled)     
!
!  local : 
!    eps - epsilon
!    cldmin - minimum value for cloud quantities
!    cwp - total cloud water path
!    radliq - cloud liquid droplet radius (microns)
!    radice - cloud ice effective size (microns)  
!    radsno - cloud snow effective size (microns)
!
! ------- Input -------
!
   integer(kind=im), intent(in   ) :: nlayers         
   integer(kind=im), intent(in   ) :: inflag         
   integer(kind=im), intent(in   ) :: iceflag        
   integer(kind=im), intent(in   ) :: liqflag       
   real(kind=rb), dimension(:,:), intent(in   ) :: cldfmc   
   real(kind=rb), dimension(:,:), intent(in   ) :: ciwpmc    
   real(kind=rb), dimension(:,:), intent(in   ) :: clwpmc   
   real(kind=rb), dimension(:,:), intent(in   ) :: cswpmc  
   real(kind=rb), dimension(:),        intent(in   ) :: relqmc  
   real(kind=rb), dimension(:),        intent(in   ) :: reicmc 
   real(kind=rb), dimension(:),        intent(in   ) :: resnmc
   real(kind=rb), dimension(:,:), intent(in   ) :: fsfcmc       
!
! ------- Output -------
!
   real(kind=rb), dimension(:,:), intent(inout) :: taucmc     
   real(kind=rb), dimension(:,:), intent(inout) :: ssacmc     
   real(kind=rb), dimension(:,:), intent(inout) :: asmcmc     
   real(kind=rb), dimension(:,:), intent(out) :: taormc       
   real(kind=rb), dimension(:,:), intent(inout) :: dtliq, dtice, dtsno
   real(kind=rb), dimension(:,:), intent(inout) :: dwliq, dwice, dwsno
   real(kind=rb), dimension(:,:), intent(inout) :: daliq, daice, dasno
!
! ------- Local -------
!
   integer(kind=im) :: ib, lay, istr, index, icx, ig
!
   real(kind=rb), parameter :: eps = 1.e-06_rb   
   real(kind=rb), parameter :: cldmin = 1.e-20_rb
   real(kind=rb) :: cwp                          
   real(kind=rb) :: radliq                      
   real(kind=rb) :: radice                     
   real(kind=rb) :: radsno                        
   real(kind=rb) :: factor
   real(kind=rb) :: fint
!
   real(kind=rb) :: taucldorig_a, taucloud_a, ssacloud_a, ffp, ffp1, ffpssa
   real(kind=rb) :: tauiceorig, scatice, ssaice, tauice
   real(kind=rb) :: tauliqorig, scatliq, ssaliq, tauliq
   real(kind=rb) :: tausnoorig, scatsno, ssasno, tausno
   real(kind=rb), dimension(ngptsw) :: fdelta
   real(kind=rb), dimension(ngptsw) :: extcoice, gice
   real(kind=rb), dimension(ngptsw) :: ssacoice, forwice
   real(kind=rb), dimension(ngptsw) :: extcoliq, gliq
   real(kind=rb), dimension(ngptsw) :: ssacoliq, forwliq
   real(kind=rb), dimension(ngptsw) :: extcosno, gsno
   real(kind=rb), dimension(ngptsw) :: ssacosno, forwsno
!-------------------------------------------------------------------------------
!
! Initialize
!
   hvrclc = '$Revision: 1.3 $'
!
! Some of these initializations are done elsewhere
!
   do lay = 1,nlayers
     do ig = 1,ngptsw
       taormc(ig,lay) = taucmc(ig,lay)
!      taucmc(ig,lay) = 0.0_rb
!      ssacmc(ig,lay) = 1.0_rb
!      asmcmc(ig,lay) = 0.0_rb
     enddo
   enddo
!
! Main layer loop
!
   do lay = 1,nlayers
!
! Main g-point interval loop
!
     do ig = 1,ngptsw 
       cwp = ciwpmc(ig,lay) + clwpmc(ig,lay) + cswpmc(ig,lay)
       if(cldfmc(ig,lay).ge.cldmin .and.                                       &
         (cwp.ge.cldmin .or. taucmc(ig,lay).ge.cldmin)) then
!
! (inflag=0): Cloud optical properties input directly
!
         if(inflag.eq.0) then
!
! Cloud optical properties already defined in taucmc, ssacmc, asmcmc are 
! unscaled;  Apply delta-M scaling here (using Henyey-Greenstein approximation)
!
           taucldorig_a = taucmc(ig,lay)
           ffp = fsfcmc(ig,lay)
           ffp1 = 1.0_rb-ffp
           ffpssa = 1.0_rb-ffp*ssacmc(ig,lay)
           ssacloud_a = ffp1*ssacmc(ig,lay)/ffpssa
           taucloud_a = ffpssa*taucldorig_a
!
           taormc(ig,lay) = taucldorig_a
           ssacmc(ig,lay) = ssacloud_a
           taucmc(ig,lay) = taucloud_a
           asmcmc(ig,lay) = (asmcmc(ig,lay) - ffp) / (ffp1)
!
         elseif(inflag.eq.1) then 
           stop 'INFLAG = 1 OPTION NOT AVAILABLE WITH MCICA'
!
! (inflag=2): Separate treatement of ice clouds and water clouds.
!
         elseif(inflag.ge.2) then       
           radice = reicmc(lay)
!
! Calculation of absorption coefficients due to ice clouds.
!
           if((ciwpmc(ig,lay)+cswpmc(ig,lay)).eq.0.0_rb) then
             extcoice(ig) = 0.0_rb
             ssacoice(ig) = 0.0_rb
             gice(ig)     = 0.0_rb
             forwice(ig)  = 0.0_rb
             extcosno(ig) = 0.0_rb
             ssacosno(ig) = 0.0_rb
             gsno(ig)     = 0.0_rb
             forwsno(ig)  = 0.0_rb
!
! (iceflag = 1): 
! Note: This option uses Ebert and Curry approach for all particle sizes similar
! to CAM3 implementation, though this is somewhat unjustified for large ice 
! particles
!
           elseif(iceflag.eq.1) then
             ib = ngb(ig)
             if(wavenum2(ib).gt.1.43e04_rb) then
               icx = 1
             elseif (wavenum2(ib).gt.7.7e03_rb) then
               icx = 2
             elseif (wavenum2(ib).gt.5.3e03_rb) then
               icx = 3
             elseif (wavenum2(ib).gt.4.0e03_rb) then
               icx = 4
             elseif (wavenum2(ib).ge.2.5e03_rb) then
               icx = 5
             endif
             extcoice(ig) = (abari(icx)+bbari(icx)/radice)
             ssacoice(ig) = 1._rb-cbari(icx)-dbari(icx)*radice
             gice(ig) = ebari(icx)+fbari(icx)*radice
!
! Check to ensure upper limit of gice is within physical limits for large 
! particles
!
             if (gice(ig).ge.1._rb) gice(ig) = 1._rb-eps
             forwice(ig) = gice(ig)*gice(ig)
!
! Check to ensure all calculated quantities are within physical limits.
!
             if(extcoice(ig).lt.0.0_rb) stop 'ICE EXTINCTION LESS THAN 0.0'
             if(ssacoice(ig).gt.1.0_rb) stop 'ICE SSA GRTR THAN 1.0'
             if(ssacoice(ig) .lt. 0.0_rb) stop 'ICE SSA LESS THAN 0.0'
             if(gice(ig).gt.1.0_rb) stop 'ICE ASYM GRTR THAN 1.0'
             if(gice(ig).lt.0.0_rb) stop 'ICE ASYM LESS THAN 0.0'
!
! For iceflag=2 option, ice particle effective radius is limited to 5.0 to 131.0
! microns
!
           elseif (iceflag .eq. 2) then
             if(radice.lt.5.0_rb .or. radice.gt.131.0_rb)                      &
                 stop 'ICE RADIUS OUT OF BOUNDS'
             factor = (radice - 2._rb)/3._rb
             index = int(factor)
             if(index.eq.43) index = 42
             fint = factor - real(index)
             ib = ngb(ig)
             extcoice(ig) = extice2(index,ib) + fint *                         &
                           (extice2(index+1,ib) -  extice2(index,ib))
             ssacoice(ig) = ssaice2(index,ib) + fint *                         &
                           (ssaice2(index+1,ib) -  ssaice2(index,ib))
             gice(ig) = asyice2(index,ib) + fint *                             &
                       (asyice2(index+1,ib) -  asyice2(index,ib))
             forwice(ig) = gice(ig)*gice(ig)
!
! Check to ensure all calculated quantities are within physical limits.
!
             if(extcoice(ig).lt.0.0_rb) stop 'ICE EXTINCTION LESS THAN 0.0'
             if(ssacoice(ig).gt.1.0_rb) stop 'ICE SSA GRTR THAN 1.0'
             if(ssacoice(ig).lt.0.0_rb) stop 'ICE SSA LESS THAN 0.0'
             if(gice(ig).gt.1.0_rb) stop 'ICE ASYM GRTR THAN 1.0'
             if(gice(ig).lt.0.0_rb) stop 'ICE ASYM LESS THAN 0.0'
!
! For iceflag=3 option, ice particle generalized effective size is limited to 
! 5.0 to 140.0 microns
!
           elseif(iceflag.ge.3) then
!
             if(radice.lt.5.0_rb .or. radice.gt.140.0_rb)                      &
                  stop 'ICE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'
             factor = (radice - 2._rb)/3._rb
             index = int(factor)
             if(index.eq.46) index = 45
             fint = factor - real(index)
             ib = ngb(ig)
             extcoice(ig) = extice3(index,ib)+fint*                            &
                           (extice3(index+1,ib)-extice3(index,ib))
             ssacoice(ig) = ssaice3(index,ib)+fint*                            &
                           (ssaice3(index+1,ib)-ssaice3(index,ib))
             gice(ig) = asyice3(index,ib)+fint*                                &
                       (asyice3(index+1,ib)-asyice3(index,ib))
             fdelta(ig) = fdlice3(index,ib)+fint*                              &
                         (fdlice3(index+1,ib)-fdlice3(index,ib))
             if(fdelta(ig).lt.0.0_rb) stop 'FDELTA LESS THAN 0.0'
             if(fdelta(ig).gt.1.0_rb) stop 'FDELTA GT THAN 1.0'
             forwice(ig) = fdelta(ig)+0.5_rb/ssacoice(ig)
!
! See Fu 1996 p. 2067 
!
             if (forwice(ig) .gt. gice(ig)) forwice(ig) = gice(ig)
!
! Check to ensure all calculated quantities are within physical limits.  
!
             if(extcoice(ig).lt.0.0_rb) stop 'ICE EXTINCTION LESS THAN 0.0'
             if(ssacoice(ig).gt.1.0_rb) stop 'ICE SSA GRTR THAN 1.0'
             if(ssacoice(ig).lt.0.0_rb) stop 'ICE SSA LESS THAN 0.0'
             if(gice(ig).gt.1.0_rb) stop 'ICE ASYM GRTR THAN 1.0'
             if(gice(ig).lt.0.0_rb) stop 'ICE ASYM LESS THAN 0.0'
           endif   
!
! INSERT THE EQUIVALENT SNOW VARIABLE CODE HERE
! Although far from perfect, the snow will utilize the
! same lookup table constants as cloud ice.  Changes
! to those constants for larger particle snow would be
! an improvement.
!
           if(cswpmc(ig,lay).gt.0.0_rb .and. iceflag.eq.5) then
             radsno = resnmc(lay)
             if(radsno.lt.5.0_rb .or. radsno.gt.140.0_rb) stop                 &
                      'ERROR: SNOW GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'   
             factor = (radsno - 2._rb)/3._rb
             index = int(factor)
             if(index.eq.167) index = 166 
             fint = factor-real(index)
             ib = ngb(ig)
             extcosno(ig) = extice3(index,ib)+fint*                            &
                           (extice3(index+1,ib)-extice3(index,ib))
             ssacosno(ig) = ssaice3(index,ib)+fint*                            &
                           (ssaice3(index+1,ib)-ssaice3(index,ib))
             gsno(ig) = asyice3(index,ib)+fint*                                &
                       (asyice3(index+1,ib)-asyice3(index,ib))
             fdelta(ig) = fdlice3(index,ib)+fint*                              &
                         (fdlice3(index+1,ib)-fdlice3(index,ib))
             if(fdelta(ig).lt.0.0_rb) stop 'FDELTA LESS THAN 0.0'
             if(fdelta(ig).gt.1.0_rb) stop 'FDELTA GT THAN 1.0'
             forwsno(ig) = fdelta(ig)+0.5_rb/ssacosno(ig)
             if(forwsno(ig).gt.gsno(ig)) forwsno(ig) = gsno(ig)
!
! Check to ensure all calculated quantities are within physical limits.  
!
             if(extcosno(ig).lt.0.0_rb) stop 'SNOW EXTINCTION LESS THAN 0.0'
             if(ssacosno(ig).gt.1.0_rb) stop 'SNOW SSA GRTR THAN 1.0'
             if(ssacosno(ig).lt.0.0_rb) stop 'SNOW SSA LESS THAN 0.0'
             if(gsno(ig).gt.1.0_rb) stop 'SNOW ASYM GRTR THAN 1.0'
             if(gsno(ig).lt.0.0_rb) stop 'SNOW ASYM LESS THAN 0.0'
           else
             extcosno(ig) = 0.0_rb
             ssacosno(ig) = 0.0_rb
             gsno(ig)     = 0.0_rb
             forwsno(ig)  = 0.0_rb
           endif
!
! Calculation of absorption coefficients due to water clouds.
!
           if (clwpmc(ig,lay) .eq. 0.0_rb) then
             extcoliq(ig) = 0.0_rb
             ssacoliq(ig) = 0.0_rb
             gliq(ig) = 0.0_rb
             forwliq(ig) = 0.0_rb
           elseif (liqflag .eq. 1) then
             radliq = relqmc(lay)
             if (radliq .lt. 1.5_rb .or. radliq .gt. 60._rb) stop              &
                        'liquid effective radius out of bounds'
             index = int(radliq - 1.5_rb)
             if (index .eq. 0) index = 1
             if (index .eq. 58) index = 57
             fint = radliq - 1.5_rb - real(index)
             ib = ngb(ig)
             extcoliq(ig) = extliq1(index,ib) + fint *                         &
                                   (extliq1(index+1,ib) - extliq1(index,ib))
             ssacoliq(ig) = ssaliq1(index,ib) + fint *                         &
                                   (ssaliq1(index+1,ib) - ssaliq1(index,ib))
             if (fint .lt. 0._rb .and. ssacoliq(ig) .gt. 1._rb)                &
                                    ssacoliq(ig) = ssaliq1(index,ib)
             gliq(ig) = asyliq1(index,ib) + fint *                             &
                               (asyliq1(index+1,ib) - asyliq1(index,ib))
             forwliq(ig) = gliq(ig)*gliq(ig)
!
! Check to ensure all calculated quantities are within physical limits.
!
             if (extcoliq(ig) .lt. 0.0_rb) stop 'LIQUID EXTINCTION LESS THAN 0.0'
             if (ssacoliq(ig) .gt. 1.0_rb) stop 'LIQUID SSA GRTR THAN 1.0'
             if (ssacoliq(ig) .lt. 0.0_rb) stop 'LIQUID SSA LESS THAN 0.0'
             if (gliq(ig) .gt. 1.0_rb) stop 'LIQUID ASYM GRTR THAN 1.0'
             if (gliq(ig) .lt. 0.0_rb) stop 'LIQUID ASYM LESS THAN 0.0'
           endif
!
           if(iceflag.lt.5) then
             tauliqorig = clwpmc(ig,lay)*extcoliq(ig)
             tauiceorig = ciwpmc(ig,lay)*extcoice(ig)
             taormc(ig,lay) = tauliqorig+tauiceorig
!
             ssaliq = ssacoliq(ig)*(1._rb-forwliq(ig))                         &
                                  /(1._rb-forwliq(ig)*ssacoliq(ig))
             tauliq = (1._rb-forwliq(ig)*ssacoliq(ig))*tauliqorig
             ssaice = ssacoice(ig)*(1._rb-forwice(ig))                         &
                                  /(1._rb-forwice(ig)*ssacoice(ig))
             tauice = (1._rb-forwice(ig)*ssacoice(ig))*tauiceorig
!
             scatliq = ssaliq*tauliq
             scatice = ssaice*tauice
             scatsno = 0.0_rb
             taucmc(ig,lay) = tauliq+tauice
           else
             tauliqorig = clwpmc(ig,lay)*extcoliq(ig)
             tauiceorig = ciwpmc(ig,lay)*extcoice(ig)
             tausnoorig = cswpmc(ig,lay)*extcosno(ig)
             taormc(ig,lay) = tauliqorig+tauiceorig+tausnoorig
!
             ssaliq = ssacoliq(ig)*(1._rb-forwliq(ig))                         &
                                  /(1._rb-forwliq(ig)*ssacoliq(ig))
             tauliq = (1._rb-forwliq(ig)*ssacoliq(ig))*tauliqorig
             ssaice = ssacoice(ig)*(1._rb-forwice(ig))                         &
                                  /(1._rb-forwice(ig)*ssacoice(ig))
             tauice = (1._rb-forwice(ig)*ssacoice(ig))*tauiceorig
             ssasno = ssacosno(ig)*(1._rb-forwsno(ig))                         &
                                  /(1._rb-forwsno(ig)*ssacosno(ig))
             tausno = (1._rb-forwsno(ig)*ssacosno(ig))*tausnoorig
             scatliq = ssaliq*tauliq
             scatice = ssaice*tauice
             scatsno = ssasno*tausno
             taucmc(ig,lay) = tauliq+tauice+tausno
           endif
           dtliq(ig,lay)=tauliq ; dwliq(ig,lay)=ssaliq
           dtice(ig,lay)=tauice ; dwice(ig,lay)=ssaice
           dtsno(ig,lay)=tausno ; dwsno(ig,lay)=ssasno
           daliq(ig,lay)=(gliq(ig)-forwliq(ig))/(1._rb-forwliq(ig))
           daice(ig,lay)=(gice(ig)-forwice(ig))/(1._rb-forwice(ig))
           dasno(ig,lay)=(gsno(ig)-forwsno(ig))/(1._rb-forwsno(ig))
!
! Ensure non-zero taucmc and scatice
!
           if(taucmc(ig,lay).eq.0.) taucmc(ig,lay) = cldmin
           if(scatice.eq.0.) scatice = cldmin
           if(scatsno.eq.0.) scatsno = cldmin
!
           if(iceflag.lt.5) then
             ssacmc(ig,lay) = (scatliq+scatice)/taucmc(ig,lay)
           else
             ssacmc(ig,lay) = (scatliq+scatice+scatsno)/taucmc(ig,lay)
           endif
!
           if(iceflag.eq.3 .or. iceflag.eq.4) then
!
! In accordance with the 1996 Fu paper, equation A.3, 
! the moments for ice were calculated depending on whether using spheres
! or hexagonal ice crystals.
! Set asymetry parameter to first moment (istr=1)
!
             istr = 1
             asmcmc(ig,lay) = (1.0_rb/(scatliq+scatice))*                      &
                   (scatliq*(gliq(ig)**istr - forwliq(ig)) /                   &
                   (1.0_rb - forwliq(ig)) + scatice * ((gice(ig)-forwice(ig))/ &
                   (1.0_rb - forwice(ig)))**istr)
           elseif(iceflag.eq.5) then
             istr = 1
             asmcmc(ig,lay) = (1.0_rb/(scatliq+scatice+scatsno))               &
                   *(scatliq*(gliq(ig)**istr-forwliq(ig))/(1.0_rb-forwliq(ig)) &
                   + scatice*((gice(ig)-forwice(ig))/(1.0_rb-forwice(ig)))     &
                   + scatsno*((gsno(ig)-forwsno(ig))/                          &
                     (1.0_rb-forwsno(ig)))**istr)
           else 
!
! This code is the standard method for delta-m scaling. 
! Set asymetry parameter to first moment (istr=1)
!
             istr = 1
             asmcmc(ig,lay) = (scatliq *                                       &
                        (gliq(ig)**istr - forwliq(ig)) /                       &
           (1.0_rb - forwliq(ig)) + scatice * (gice(ig)**istr - forwice(ig)) / &
                        (1.0_rb - forwice(ig)))/(scatliq + scatice)
           endif 
!
         endif  
!
       endif
!
! End g-point interval loop
!
     enddo
!
! End layer loop
!
   enddo
!
   end subroutine cldprmc_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_cldprmc_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_reftra_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use rrsw_tbl_k, only : tblint, bpade, od_lo, exp_tbl
   use rrsw_vsn_k, only : hvrrft, hnamrft
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine reftra_sw(nlayers, lrtchk, pgg, prmuz, ptau, pw,                 &
                           al1, al2, al3,                                      &    
                           pref, prefd, ptra, ptrad)
!-------------------------------------------------------------------------------
!
! abstract: 
!   computes the reflectivity and transmissivity of a clear or 
!   cloudy layer using a choice of various approximations.
!
! history log :
!  2016-10-27 sunghye baek revised TSA 
! Interface:  *rrtmg_sw_reftra* is called by *rrtmg_sw_spcvrt*
!
! Description:
! explicit arguments :
! --------------------
! inputs
! ------ 
!      lrtchk  = .t. for all layers in clear profile
!      lrtchk  = .t. for cloudy layers in cloud profile 
!              = .f. for clear layers in cloud profile
!      pgg     = assymetry factor
!      prmuz   = cosine solar zenith angle
!      ptau    = optical thickness
!      pw      = single scattering albedo
!      al,a2,a3= coefficients for Ritter and Geleyn 1992 convention
!
! outputs
! -------
!      pref    : collimated beam reflectivity
!      prefd   : diffuse beam reflectivity 
!      ptra    : collimated beam transmissivity
!      ptrad   : diffuse beam transmissivity
!
!
! Method:
! -------
!      standard delta-eddington, p.i.f.m., or d.o.m. layer calculations.
!      kmodts  = 1 eddington (joseph et al., 1976)
!              = 2 pifm (zdunkowski et al., 1980)
!              = 3 discrete ordinates (liou, 1973)
!              = 4 same as pifm, but Ritter and Geleyn 1992 convention
!              = 5 revised TSA (sunghye baek) 
!
! Modifications:
! --------------
! Original: J-JMorcrette, ECMWF, Feb 2003
! Revised for F90 reformatting: MJIacono, AER, Jul 2006
! Revised to add exponential lookup table: MJIacono, AER, Aug 2007
! Reformulated some code to avoid potential fpes: MJIacono, AER, Nov 2008
!
!
! lrtchk(nlayers) : Logical flag for reflectivity and transmissivity calculation
! pgg(nlayers) : asymmetry parameter
! ptau(nlayers) : optical depth
! pw(nlayers) : single scattering albedo
! prmuz : cosine of solar zenith angle 
!
! pref(nlayers+1) :  direct beam reflectivity
! prefd(nlayers+1) : diffuse beam reflectivity
! ptra(nlayers+1) : direct beam transmissivity
! ptrad(nlayers+1) : diffuse beam transmissivity
! ------------------------------------------------------------------------------
!
! ------- Declarations ------
!
! ------- Input -------
!
   integer(kind=im),                  intent(in   ) :: nlayers
   logical, dimension(:),         intent(in   ) :: lrtchk                
   real(kind=rb), dimension(:),   intent(in   ) :: pgg                  
   real(kind=rb), dimension(:),   intent(in   ) :: ptau
   real(kind=rb), dimension(:),   intent(in   ) :: pw                
   real(kind=rb),                 intent(in   ) :: prmuz 
   real(kind=rb), dimension(:),   intent(in   ) :: al1
   real(kind=rb), dimension(:),   intent(in   ) :: al2
   real(kind=rb), dimension(:),   intent(in   ) :: al3
!
! ------- Output -------
!
   real(kind=rb), dimension(:),   intent(inout) :: pref             
   real(kind=rb), dimension(:),   intent(inout) :: prefd            
   real(kind=rb), dimension(:),   intent(inout) :: ptra   
   real(kind=rb), dimension(:),   intent(inout) :: ptrad
!
! ------- Local -------
!
   integer(kind=im) :: jk, jl, kmodts
   integer(kind=im) :: itind
!
   real(kind=rb) :: tblind
   real(kind=rb) :: za, za1, za2
   real(kind=rb) :: zbeta, zdend, zdenr, zdent
   real(kind=rb) :: ze1, ze2, zem1, zem2, zemm, zep1, zep2
   real(kind=rb) :: zg, zg3, zgamma1, zgamma2, zgamma3, zgamma4, zgt
   real(kind=rb) :: zr1, zr2, zr3, zr4, zr5
   real(kind=rb) :: zrk, zrk2, zrkg, zrm1, zrp, zrp1, zrpp
   real(kind=rb) :: zsr3, zt1, zt2, zt3, zt4, zt5, zto1
   real(kind=rb) :: zw, zwcrit, zwo
   real(kind=rb) :: denom
!
   real(kind=rb), parameter :: eps = 1.e-08_rb
   real(kind=rb) :: zbetab, zbetad, ztos, ztoa, zto2
!-------------------------------------------------------------------------------
!
! Initialize
!
   hvrrft = '$Revision: 1.3 $'
   zsr3=sqrt(3._rb)
   zwcrit=0.9999995_rb
   kmodts=5
!
   do jk = 1,nlayers
     if (.not.lrtchk(jk)) then
       pref(jk) =0._rb
       ptra(jk) =1._rb
       prefd(jk)=0._rb
       ptrad(jk)=1._rb
     else
       zto1=ptau(jk)
       zw  =pw(jk)
       zg  =pgg(jk)  
!
! General two-stream expressions
!
       zg3= 3._rb * zg
       if (kmodts == 1) then
         zgamma1= (7._rb - zw * (4._rb + zg3)) * 0.25_rb
         zgamma2=-(1._rb - zw * (4._rb - zg3)) * 0.25_rb
         zgamma3= (2._rb - zg3 * prmuz ) * 0.25_rb
       else if (kmodts == 2) then  
         zgamma1= (8._rb - zw * (5._rb + zg3)) * 0.25_rb
         zgamma2=  3._rb *(zw * (1._rb - zg )) * 0.25_rb
         zgamma3= (2._rb - zg3 * prmuz ) * 0.25_rb
       else if (kmodts == 3) then  
         zgamma1= zsr3 * (2._rb - zw * (1._rb + zg)) * 0.5_rb
         zgamma2= zsr3 * zw * (1._rb - zg ) * 0.5_rb
         zgamma3= (1._rb - zsr3 * zg * prmuz ) * 0.5_rb
       else if (kmodts == 4) then ! Ritter and Geleyn 1992 convention 
         zbetad = 3._rb*0.125_rb*(1._rb-zg)
         zbetab = 0.25_rb*(2._rb-3._rb*zg*prmuz)
         ztos = zw*zto1
         ztoa = zto1 - ztos
         zto2 = ztos/zto1
         zgamma1= (2._rb * ztoa + 2._rb * zbetad * ztos) / zto1
         zgamma2= 2._rb * zbetad * zto2
         zgamma3= zbetab*zw
       else if (kmodts == 5) then
         zgamma1= al1(jk)
         zgamma2= al2(jk)
         zgamma3= al3(jk)
       end if
       if(kmodts .ge. 4) then
         zgamma4= zw - zgamma3
       else
         zgamma4= 1._rb - zgamma3
       endif
!
! Recompute original s.s.a. to test for conservative solution
!
       zwo = 0._rb
       denom = 1._rb
       if (zg .ne. 1._rb) denom = (1._rb - (1._rb - zw) * (zg / (1._rb - zg))**2)
       if (zw .gt. 0._rb .and. denom .ne. 0._rb) zwo = zw / denom
!
       if (zwo >= zwcrit) then
!
! Conservative scattering
!
         za  = zgamma1 * prmuz 
         za1 = za - zgamma3
         zgt = zgamma1 * zto1
!
! Homogeneous reflectance and transmittance,
! collimated beam
!
         ze1 = min ( zto1 / prmuz , 500._rb)
!               ze2 = exp( -ze1 )
!
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
!
         if (ze1 .le. od_lo) then 
           ze2 = 1._rb - ze1 + 0.5_rb * ze1 * ze1
         else
           tblind = ze1 / (bpade + ze1)
           itind = tblint * tblind + 0.5_rb
           ze2 = exp_tbl(itind)
         endif
!
         pref(jk) = (zgt - za1 * (1._rb - ze2)) / (1._rb + zgt)
         ptra(jk) = 1._rb - pref(jk)
!
! isotropic incidence
!
         prefd(jk) = zgt / (1._rb + zgt)
         ptrad(jk) = 1._rb - prefd(jk)        
!
! This is applied for consistency between total (delta-scaled) and direct 
! (unscaled) calculations at very low optical depths (tau < 1.e-4) when 
! the exponential lookup table returns a transmittance of 1.0.
!
         if (ze2 .eq. 1.0_rb) then 
           pref(jk) = 0.0_rb
           ptra(jk) = 1.0_rb
           prefd(jk) = 0.0_rb
           ptrad(jk) = 1.0_rb
         endif
!
       else
!
! Non-conservative scattering
!
         za1 = zgamma1 * zgamma4 + zgamma2 * zgamma3
         za2 = zgamma1 * zgamma3 + zgamma2 * zgamma4
         zrk = sqrt ( zgamma1**2 - zgamma2**2)
         zrp = zrk * prmuz               
         zrp1 = 1._rb + zrp
         zrm1 = 1._rb - zrp
         zrk2 = 2._rb * zrk
         zrpp = 1._rb - zrp*zrp
         zrkg = zrk + zgamma1
         zr1  = zrm1 * (za2 + zrk * zgamma3)
         zr2  = zrp1 * (za2 - zrk * zgamma3)
         zr3  = zrk2 * (zgamma3 - za2 * prmuz )
         zr4  = zrpp * zrkg
         zr5  = zrpp * (zrk - zgamma1)
         zt1  = zrp1 * (za1 + zrk * zgamma4)
         zt2  = zrm1 * (za1 - zrk * zgamma4)
         zt3  = zrk2 * (zgamma4 + za1 * prmuz )
         zt4  = zr4
         zt5  = zr5
!
! mji - reformulated code to avoid potential floating point exceptions
!        zbeta = - zr5 / zr4
         zbeta = (zgamma1 - zrk) / zrkg
!
! Homogeneous reflectance and transmittance
!
         ze1 = min ( zrk * zto1, 500._rb)
         ze2 = min ( zto1 / prmuz , 500._rb)
!
! Original
!        zep1 = exp( ze1 )
!        zem1 = exp(-ze1 )
!        zep2 = exp( ze2 )
!        zem2 = exp(-ze2 )
!
! Revised original, to reduce exponentials
!        zep1 = exp( ze1 )
!        zem1 = 1._rb / zep1
!        zep2 = exp( ze2 )
!        zem2 = 1._rb / zep2
!
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
!
         if (ze1 .le. od_lo) then 
           zem1 = 1._rb - ze1 + 0.5_rb * ze1 * ze1
           zep1 = 1._rb / zem1
         else
           tblind = ze1 / (bpade + ze1)
           itind = tblint * tblind + 0.5_rb
           zem1 = exp_tbl(itind)
           zep1 = 1._rb / zem1
         endif
!
         if (ze2 .le. od_lo) then 
           zem2 = 1._rb - ze2 + 0.5_rb * ze2 * ze2
           zep2 = 1._rb / zem2
         else
           tblind = ze2 / (bpade + ze2)
           itind = tblint * tblind + 0.5_rb
           zem2 = exp_tbl(itind)
           zep2 = 1._rb / zem2
         endif
!
! collimated beam
!
! mji - reformulated code to avoid potential floating point exceptions
!       zdenr = zr4*zep1 + zr5*zem1
!       pref(jk) = zw * (zr1*zep1 - zr2*zem1 - zr3*zem2) / zdenr
!       zdent = zt4*zep1 + zt5*zem1
!       ptra(jk) = zem2 - zem2 * zw * (zt1*zep1 - zt2*zem1 - zt3*zep2) / zdent
!
         zdenr = zr4*zep1 + zr5*zem1
         zdent = zt4*zep1 + zt5*zem1
         if (zdenr .ge. -eps .and. zdenr .le. eps) then
           pref(jk) = eps
           ptra(jk) = zem2
         else
           if(kmodts .ge. 4) then
             pref(jk) = (zr1*zep1 - zr2*zem1 - zr3*zem2) / zdenr
             ptra(jk) = zem2 - zem2 * (zt1*zep1 - zt2*zem1 - zt3*zep2) / zdent
           else
             pref(jk) = zw * (zr1*zep1 - zr2*zem1 - zr3*zem2) / zdenr
             ptra(jk) = zem2 - zem2 * zw * (zt1*zep1 - zt2*zem1 - zt3*zep2) / zdent
           endif
         endif
!
! diffuse beam
!
         zemm = zem1*zem1
         zdend = 1._rb / ( (1._rb - zbeta*zemm ) * zrkg)
         prefd(jk) =  zgamma2 * (1._rb - zemm) * zdend
         ptrad(jk) =  zrk2*zem1*zdend
       endif
!
     endif         
!
   enddo    
!
   end subroutine reftra_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_reftra_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_setcoef_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : mxmol
   use rrsw_ref_k, only : pref, preflog, tref
   use rrsw_vsn_k, only : hvrset, hnamset
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine setcoef_sw(nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl,   &
                            laytrop, layswtch, laylow, jp, jt, jt1,            &
                            co2mult, colch4, colco2, colh2o, colmol, coln2o,   &
                            colo2, colo3, fac00, fac01, fac10, fac11,          &
                            selffac, selffrac, indself, forfac, forfrac, indfor)
!-------------------------------------------------------------------------------
!
! abstract:  For a given atmosphere, calculate the indices and
! fractions related to the pressure and temperature interpolations.
!
! history log:
!   2002-04-01 J. Delamere, AER, Inc.   version 2.5
!   2003-02-24 JJMorcrette              Rewritten and adapted to ECMWF F90
!   2006-07    MJIacono                 For uniform rrtmg formatting
!
! input : 
!   nlayers : total number of layers
!   pavel(nlayers) : layer pressures (mb)
!   tavel(nlayers) : layer temperatures (K)
!   pz(0:nlayers) : level (interface) pressures (hPa, mb)
!   tz(0:nlayers) : level (interface) temperatures (K)
!   tbound : surface temperature (K)
!   coldry(nlayers) : dry air column density (mol/cm2)
!   wkl(mxmol,nlayers) : molecular amounts (mol/cm-2)
!
! output : 
!   laytrop : tropopause layer index
!   jp(nlayers) 
!   jt(nlayers)
!   jt1(nlayers)
!   colh2o(nlayers) column amount (h2o)
!   colco2(nlayers) column amount (co2)
!   colo3(nlayers) column amount (o3)
!   coln2o(nlayers) column amount (n20)
!   colch4(nlayers) column amount (ch4)
!   colo2(nlayers) column amount (o2)
!   colmol(nlayers)
!   co2mult(nlayers)
!
!   indself(nlayers)
!   indfor(nlayers)
!   selffac(nlayers)
!   forfac(nlayers)
!   forfrac(nlayers)  
!   fac00(nlayers), fac01, fac10, fac11
!-------------------------------------------------------------------------------
!
! ----- Input -----
!
   integer(kind=im),   intent(in   ) :: nlayers         
   real(kind=rb), dimension(:),  intent(in   ) :: pavel           
   real(kind=rb), dimension(:),  intent(in   ) :: tavel          
   real(kind=rb), dimension(0:), intent(in   ) :: pz          
   real(kind=rb), dimension(0:), intent(in   ) :: tz          
   real(kind=rb),                intent(in   ) :: tbound         
   real(kind=rb), dimension(:),  intent(in   ) :: coldry
   real(kind=rb), dimension(:,:),intent(in   ) :: wkl     
!
! ----- Output -----
!
   integer(kind=im),   intent(  out) :: laytrop        
   integer(kind=im),   intent(  out) :: layswtch       
   integer(kind=im),   intent(  out) :: laylow          
   integer(kind=im), dimension(:),  intent(  out) :: jp           
   integer(kind=im), dimension(:),  intent(  out) :: jt
   integer(kind=im), dimension(:),  intent(  out) :: jt1         
   real(kind=rb), dimension(:),     intent(  out) :: colh2o        
   real(kind=rb), dimension(:),     intent(  out) :: colco2         
   real(kind=rb), dimension(:),     intent(  out) :: colo3          
   real(kind=rb), dimension(:),     intent(  out) :: coln2o         
   real(kind=rb), dimension(:),     intent(  out) :: colch4         
   real(kind=rb), dimension(:),     intent(  out) :: colo2         
   real(kind=rb), dimension(:),     intent(  out) :: colmol          
   real(kind=rb), dimension(:),     intent(  out) :: co2mult        
!
   integer(kind=im), dimension(:),  intent(   out) :: indself
   integer(kind=im), dimension(:),  intent(   out) :: indfor
   real(kind=rb), dimension(:), intent(out) :: selffac
   real(kind=rb), dimension(:), intent(out) :: selffrac
   real(kind=rb), dimension(:), intent(out) :: forfac
   real(kind=rb), dimension(:), intent(out) :: forfrac
   real(kind=rb), dimension(:), intent(out) :: fac00, fac01
   real(kind=rb), dimension(:), intent(out) :: fac10, fac11 
!
! ----- Local -----
!
   integer(kind=im) :: indbound
   integer(kind=im) :: indlev0
   integer(kind=im) :: lay
   integer(kind=im) :: jp1
!
   real(kind=rb) :: stpfac
   real(kind=rb) :: tbndfrac
   real(kind=rb) :: t0frac
   real(kind=rb) :: plog
   real(kind=rb) :: fp
   real(kind=rb) :: ft
   real(kind=rb) :: ft1
   real(kind=rb) :: water
   real(kind=rb) :: scalefac
   real(kind=rb) :: factor
   real(kind=rb) :: co2reg
   real(kind=rb) :: compfp
!-------------------------------------------------------------------------------
!
! Initializations
!
   stpfac = 296._rb/1013._rb
!
   indbound = tbound - 159._rb
   tbndfrac = tbound - int(tbound)
   indlev0  = tz(0) - 159._rb
   t0frac   = tz(0) - int(tz(0))
!
   laytrop  = 0
   layswtch = 0
   laylow   = 0
!
! Begin layer loop
!
   do lay = 1,nlayers
!
! Find the two reference pressures on either side of the
! layer pressure.  Store them in JP and JP1.  Store in FP the
! fraction of the difference (in ln(pressure)) between these
! two values that the layer pressure lies.
!
     plog = log(pavel(lay))
     jp(lay) = int(36._rb - 5*(plog+0.04_rb))
     if (jp(lay) .lt. 1) then
       jp(lay) = 1
     elseif (jp(lay) .gt. 58) then
       jp(lay) = 58
     endif
     jp1 = jp(lay) + 1
     fp = 5._rb * (preflog(jp(lay)) - plog)
!
! Determine, for each reference pressure (JP and JP1), which
! reference temperature (these are different for each  
! reference pressure) is nearest the layer temperature but does
! not exceed it.  Store these indices in JT and JT1, resp.
! Store in FT (resp. FT1) the fraction of the way between JT
! (JT1) and the next highest reference temperature that the 
! layer temperature falls.
!
     jt(lay) = int(3._rb + (tavel(lay)-tref(jp(lay)))/15._rb)
     if (jt(lay) .lt. 1) then
       jt(lay) = 1
     elseif (jt(lay) .gt. 4) then
       jt(lay) = 4
     endif
     ft = ((tavel(lay)-tref(jp(lay)))/15._rb) - real(jt(lay)-3)
     jt1(lay) = int(3._rb + (tavel(lay)-tref(jp1))/15._rb)
     if (jt1(lay) .lt. 1) then
       jt1(lay) = 1
     elseif (jt1(lay) .gt. 4) then
       jt1(lay) = 4
     endif
     ft1 = ((tavel(lay)-tref(jp1))/15._rb) - real(jt1(lay)-3)
!
     water = wkl(1,lay)/coldry(lay)
     scalefac = pavel(lay) * stpfac / tavel(lay)
!
! If the pressure is less than ~100mb, perform a different
! set of species interpolations.
!
     if (plog .le. 4.56_rb) go to 5300
     laytrop =  laytrop + 1
     if (plog .ge. 6.62_rb) laylow = laylow + 1
!
! Set up factors needed to separately include the water vapor
! foreign-continuum in the calculation of absorption coefficient.
!
     forfac(lay) = scalefac / (1.+water)
     factor = (332.0_rb-tavel(lay))/36.0_rb
     indfor(lay) = min(2, max(1, int(factor)))
     forfrac(lay) = factor - real(indfor(lay))
!
! Set up factors needed to separately include the water vapor
! self-continuum in the calculation of absorption coefficient.
!
     selffac(lay) = water * forfac(lay)
     factor = (tavel(lay)-188.0_rb)/7.2_rb
     indself(lay) = min(9, max(1, int(factor)-7))
     selffrac(lay) = factor - real(indself(lay) + 7)
!
! Calculate needed column amounts.
!
     colh2o(lay) = 1.e-20_rb * wkl(1,lay)
     colco2(lay) = 1.e-20_rb * wkl(2,lay)
     colo3(lay) = 1.e-20_rb * wkl(3,lay)
!    colo3(lay) = 0._rb
!    colo3(lay) = colo3(lay)/1.16_rb
     coln2o(lay) = 1.e-20_rb * wkl(4,lay)
     colch4(lay) = 1.e-20_rb * wkl(6,lay)
     colo2(lay) = 1.e-20_rb * wkl(7,lay)
     colmol(lay) = 1.e-20_rb * coldry(lay) + colh2o(lay)
!    colco2(lay) = 0._rb
!    colo3(lay) = 0._rb
!    coln2o(lay) = 0._rb
!    colch4(lay) = 0._rb
!    colo2(lay) = 0._rb
!    colmol(lay) = 0._rb
     if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
     if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
     if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
     if (colo2(lay) .eq. 0._rb) colo2(lay) = 1.e-32_rb * coldry(lay)
!
! Using E = 1334.2 cm-1.
!
     co2reg = 3.55e-24_rb * coldry(lay)
     co2mult(lay)= (colco2(lay) - co2reg) *                                    &
               272.63_rb*exp(-1919.4_rb/tavel(lay))/(8.7604e-4_rb*tavel(lay))
     goto 5400
!
! Above laytrop.
!
5300    continue
!
! Set up factors needed to separately include the water vapor
! foreign-continuum in the calculation of absorption coefficient.
!
     forfac(lay) = scalefac / (1.+water)
     factor = (tavel(lay)-188.0_rb)/36.0_rb
     indfor(lay) = 3
     forfrac(lay) = factor - 1.0_rb
!
! Calculate needed column amounts.
!
     colh2o(lay) = 1.e-20_rb * wkl(1,lay)
     colco2(lay) = 1.e-20_rb * wkl(2,lay)
     colo3(lay)  = 1.e-20_rb * wkl(3,lay)
     coln2o(lay) = 1.e-20_rb * wkl(4,lay)
     colch4(lay) = 1.e-20_rb * wkl(6,lay)
     colo2(lay)  = 1.e-20_rb * wkl(7,lay)
     colmol(lay) = 1.e-20_rb * coldry(lay) + colh2o(lay)
     if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
     if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
     if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
     if (colo2(lay)  .eq. 0._rb) colo2(lay)  = 1.e-32_rb * coldry(lay)
     co2reg = 3.55e-24_rb * coldry(lay)
     co2mult(lay)= (colco2(lay) - co2reg) *                                    &
               272.63_rb*exp(-1919.4_rb/tavel(lay))/(8.7604e-4_rb*tavel(lay))
     selffac(lay) = 0._rb
     selffrac(lay)= 0._rb
     indself(lay) = 0
!
5400    continue
!
! We have now isolated the layer ln pressure and temperature,
! between two reference pressures and two reference temperatures 
! (for each reference pressure).  We multiply the pressure 
! fraction FP with the appropriate temperature fractions to get 
! the factors that will be needed for the interpolation that yields
! the optical depths (performed in routines TAUGBn for band n).
!
     compfp = 1._rb - fp
     fac10(lay) = compfp * ft
     fac00(lay) = compfp * (1._rb - ft)
     fac11(lay) = fp * ft1
     fac01(lay) = fp * (1._rb - ft1)
!
! End layer loop
!
   enddo
!
   end subroutine setcoef_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine swatmref
!-------------------------------------------------------------------------------
!
   save
! 
! These pressures are chosen such that the ln of the first pressure
! has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
! each subsequent ln(pressure) differs from the previous one by 0.2.
!
   pref(:) = (/                                                                &
   1.05363e+03_rb,8.62642e+02_rb,7.06272e+02_rb,5.78246e+02_rb,4.73428e+02_rb, &
   3.87610e+02_rb,3.17348e+02_rb,2.59823e+02_rb,2.12725e+02_rb,1.74164e+02_rb, &
   1.42594e+02_rb,1.16746e+02_rb,9.55835e+01_rb,7.82571e+01_rb,6.40715e+01_rb, &
   5.24573e+01_rb,4.29484e+01_rb,3.51632e+01_rb,2.87892e+01_rb,2.35706e+01_rb, &
   1.92980e+01_rb,1.57998e+01_rb,1.29358e+01_rb,1.05910e+01_rb,8.67114e+00_rb, &
   7.09933e+00_rb,5.81244e+00_rb,4.75882e+00_rb,3.89619e+00_rb,3.18993e+00_rb, &
   2.61170e+00_rb,2.13828e+00_rb,1.75067e+00_rb,1.43333e+00_rb,1.17351e+00_rb, &
   9.60789e-01_rb,7.86628e-01_rb,6.44036e-01_rb,5.27292e-01_rb,4.31710e-01_rb, &
   3.53455e-01_rb,2.89384e-01_rb,2.36928e-01_rb,1.93980e-01_rb,1.58817e-01_rb, &
   1.30029e-01_rb,1.06458e-01_rb,8.71608e-02_rb,7.13612e-02_rb,5.84256e-02_rb, &
   4.78349e-02_rb,3.91639e-02_rb,3.20647e-02_rb,2.62523e-02_rb,2.14936e-02_rb, &
   1.75975e-02_rb,1.44076e-02_rb,1.17959e-02_rb,9.65769e-03_rb /)
!
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
   -4.0400e+00_rb,-4.2400e+00_rb,-4.4400e+00_rb,-4.6400e+00_rb /)
!
! These are the temperatures associated with the respective 
! pressures for the MLS standard atmosphere. 
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
    1.8329e+02_rb, 1.7849e+02_rb, 1.7394e+02_rb, 1.7212e+02_rb /)
!
   end subroutine swatmref
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_setcoef_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_taumol_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
!  use parrrsw, only : mg, jpband, nbndsw, ngptsw
   use rrsw_con_k, only: oneminus
   use rrsw_wvn_k, only: nspa, nspb
   use rrsw_vsn_k, only: hvrtau, hnamtau
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol_sw(nlayers,                                               &
                           colh2o, colco2, colch4, colo2, colo3, colmol,       &
                           laytrop, jp, jt, jt1,                               &
                           fac00, fac01, fac10, fac11,                         &
                           selffac, selffrac, indself, forfac, forfrac, indfor,&
                           sfluxzen, taug, taur)
!-------------------------------------------------------------------------------
! ******************************************************************************
! *                                                                            *
! *                 Optical depths developed for the                           *
! *                                                                            *
! *               RAPID RADIATIVE TRANSFER MODEL (RRTM)                        *
! *                                                                            *
! *                                                                            *
! *           ATMOSPHERIC AND ENVIRONMENTAL RESEARCH, INC.                     *
! *                       131 HARTWELL AVENUE                                  *
! *                       LEXINGTON, MA 02421                                  *
! *                                                                            *
! *                                                                            *
! *                          ELI J. MLAWER                                     *
! *                        JENNIFER DELAMERE                                   *
! *                        STEVEN J. TAUBMAN                                   *
! *                        SHEPARD A. CLOUGH                                   *
! *                                                                            *
! *                                                                            *
! *                                                                            *
! *                                                                            *
! *                      email:  mlawer@aer.com                                *
! *                      email:  jdelamer@aer.com                              *
! *                                                                            *
! *       The authors wish to acknowledge the contributions of the             *
! *       following people:  Patrick D. Brown, Michael J. Iacono,              *
! *       Ronald E. Farren, Luke Chen, Robert Bergstrom.                       *
! *                                                                            *
! ******************************************************************************
! *    TAUMOL                                                                  *
! *                                                                            *
! *    This file contains the subroutines TAUGBn (where n goes from            *
! *    1 to 28).  TAUGBn calculates the optical depths and Planck fractions    *
! *    per g-value and layer for band n.                                       *
! *                                                                            *
! * Output:  optical depths (unitless)                                         *
! *          fractions needed to compute Planck functions at every layer       *
! *              and g-value                                                   *
! *                                                                            *
! *    COMMON /TAUGCOM/  TAUG(MXLAY,MG)                                        *
! *    COMMON /PLANKG/   FRACS(MXLAY,MG)                                       *
! *                                                                            *
! * Input                                                                      *
! *                                                                            *
! *    PARAMETER (MG=16, MXLAY=203, NBANDS=14)                                 *
! *                                                                            *
! *    COMMON /FEATURES/ NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)                  *
! *    COMMON /PRECISE/  ONEMINUS                                              *
! *    COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),                    *
! *   &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND                        *
! *    COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,                              *
! *   &                  COLH2O(MXLAY),COLCO2(MXLAY),                          *
! *   &                  COLO3(MXLAY),COLN2O(MXLAY),COLCH4(MXLAY),             *
! *   &                  COLO2(MXLAY),CO2MULT(MXLAY)                           *
! *    COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),                            *
! *   &                  FAC10(MXLAY),FAC11(MXLAY)                             *
! *    COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)                        *
! *    COMMON /SELF/     SELFFAC(MXLAY), SELFFRAC(MXLAY), INDSELF(MXLAY)       *
! *                                                                            *
! *    Description:                                                            *
! *    NG(IBAND) - number of g-values in band IBAND                            *
! *    NSPA(IBAND) - for the lower atmosphere, the number of reference         *
! *                  atmospheres that are stored for band IBAND per            *
! *                  pressure level and temperature.  Each of these            *
! *                  atmospheres has different relative amounts of the         *
! *                  key species for the band (i.e. different binary           *
! *                  species parameters).                                      *
! *    NSPB(IBAND) - same for upper atmosphere                                 *
! *    ONEMINUS - since problems are caused in some cases by interpolation     *
! *               parameters equal to or greater than 1, for these cases       *
! *               these parameters are set to this value, slightly < 1.        *
! *    PAVEL - layer pressures (mb)                                            *
! *    TAVEL - layer temperatures (degrees K)                                  *
! *    PZ - level pressures (mb)                                               *
! *    TZ - level temperatures (degrees K)                                     *
! *    LAYTROP - layer at which switch is made from one combination of         *
! *              key species to another                                        *
! *    COLH2O, COLCO2, COLO3, COLN2O, COLCH4 - column amounts of water         *
! *              vapor,carbon dioxide, ozone, nitrous ozide, methane,          *
! *              respectively (molecules/cm**2)                                *
! *    CO2MULT - for bands in which carbon dioxide is implemented as a         *
! *              trace species, this is the factor used to multiply the        *
! *              band's average CO2 absorption coefficient to get the added    *
! *              contribution to the optical depth relative to 355 ppm.        *
! *    FACij(LAY) - for layer LAY, these are factors that are needed to        *
! *                 compute the interpolation factors that multiply the        *
! *                 appropriate reference k-values.  A value of 0 (1) for      *
! *                 i,j indicates that the corresponding factor multiplies     *
! *                 reference k-value for the lower (higher) of the two        *
! *                 appropriate temperatures, and altitudes, respectively.     *
! *    JP - the index of the lower (in altitude) of the two appropriate        *
! *         reference pressure levels needed for interpolation                 *
! *    JT, JT1 - the indices of the lower of the two appropriate reference     *
! *              temperatures needed for interpolation (for pressure           *
! *              levels JP and JP+1, respectively)                             *
! *    SELFFAC - scale factor needed to water vapor self-continuum, equals     *
! *              (water vapor density)/(atmospheric density at 296K and        *
! *              1013 mb)                                                      *
! *    SELFFRAC - factor needed for temperature interpolation of reference     *
! *               water vapor self-continuum data                              *
! *    INDSELF - index of the lower of the two appropriate reference           *
! *              temperatures needed for the self-continuum interpolation      *
! *                                                                            *
! * Data input                                                                 *
! *    COMMON /Kn/ KA(NSPA(n),5,13,MG), KB(NSPB(n),5,13:59,MG), SELFREF(10,MG) *
! *       (note:  n is the band number)                                        *
! *                                                                            *
! *    Description:                                                            *
! *    KA - k-values for low reference atmospheres (no water vapor             *
! *         self-continuum) (units: cm**2/molecule)                            *
! *    KB - k-values for high reference atmospheres (all sources)              *
! *         (units: cm**2/molecule)                                            *
! *    SELFREF - k-values for water vapor self-continuum for reference         *
! *              atmospheres (used below LAYTROP)                              *
! *              (units: cm**2/molecule)                                       *
! *                                                                            *
! *    DIMENSION ABSA(65*NSPA(n),MG), ABSB(235*NSPB(n),MG)                     *
! *    EQUIVALENCE (KA,ABSA),(KB,ABSB)                                         *
! *                                                                            *
! *****************************************************************************
!
! history log : 
!
! Revised: Adapted to F90 coding, J.-J.Morcrette, ECMWF, Feb 2003
! Revised: Modified for g-point reduction, MJIacono, AER, Dec 2003
! Revised: Reformatted for consistency with rrtmg_lw, MJIacono, AER, Jul 2006
!
! input : 
!   nlayers : total number of layers
!   laytrop : tropopause layer index
!   jp(nlayers)
!   jt(nlayers)
!   jt1(nlayers)
!
!   colh2o(nlayers) column amount (h2o)
!   colco2(nlayers) column amount (co2)
!   colo3(nlayers) column amount (o3)
!   coln2o(nlayers) column amount (n20)
!   colch4(nlayers) column amount (ch4)
!   colo2(nlayers) column amount (o2)
!   colmol(nlayers)
!
!   indself(nlayers)
!   indfor(nlayers)
!   selffac(nlayers)
!   selffrac(nlayers)
!   forfac(nlayers)
!   forfrac(nlayers)  
!   fac00(nlayers), fac01, fac10, fac11
!
! output :
!  sfluxzen(ngptsw) : solar source function
!  taug(nlayers,ngptsw) : gaseous optical depth 
!  taur(nlayers,ngptsw) : Rayleigh scattering
!  ssa(nlayers,ngptsw) : single scattering albedo (inactive)
! ------- Declarations -------
!
! ----- Input -----
   integer(kind=im), intent(in   ) :: nlayers            
   integer(kind=im), intent(in   ) :: laytrop            
   integer(kind=im), dimension(:), intent(in   ) :: jp              
   integer(kind=im), dimension(:), intent(in   ) :: jt            
   integer(kind=im), dimension(:), intent(in   ) :: jt1          
   real(kind=rb),    dimension(:), intent(in   ) :: colh2o            
   real(kind=rb),    dimension(:), intent(in   ) :: colco2             
   real(kind=rb),    dimension(:), intent(in   ) :: colo3             
   real(kind=rb),    dimension(:), intent(in   ) :: colch4             
   real(kind=rb),    dimension(:), intent(in   ) :: colo2              
   real(kind=rb),    dimension(:), intent(in   ) :: colmol              
   integer(kind=im), dimension(:), intent(in   ) :: indself
   integer(kind=im), dimension(:), intent(in   ) :: indfor
   real(kind=rb),    dimension(:), intent(in   ) :: selffac
   real(kind=rb),    dimension(:), intent(in   ) :: selffrac
   real(kind=rb),    dimension(:), intent(in   ) :: forfac
   real(kind=rb),    dimension(:), intent(in   ) :: forfrac
   real(kind=rb),    dimension(:), intent(in   ) :: fac00, fac01                
   real(kind=rb),    dimension(:), intent(in   ) :: fac10, fac11
!
! ----- Output -----
!
   real(kind=rb), dimension(:),   intent(  out) :: sfluxzen        
   real(kind=rb), dimension(:,:), intent(  out) :: taug          
   real(kind=rb), dimension(:,:), intent(  out) :: taur          
!  real(kind=rb), intent(out) :: ssa(:,:)          
   hvrtau = '$Revision: 1.3 $'
!
! Calculate gaseous optical depth and planck fractions for each spectral band.
!
   call taumol16
   call taumol17
   call taumol18
   call taumol19
   call taumol20
   call taumol21
   call taumol22
   call taumol23
   call taumol24
   call taumol25
   call taumol26
   call taumol27
   call taumol28
   call taumol29
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol16
!-------------------------------------------------------------------------------
!     band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng16
   use rrsw_kg16_k, only : absa, ka, absb, kb, forref, selfref,                  &
                         sfluxref, rayl, layreffr, strrat1
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                    fac110, fac111, fs, speccomb, specmult, specparm,          &
                    tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colh2o(lay) + strrat1*colch4(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(16) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(16) + js
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng16
       taug(lay,ig) = speccomb *                                               &
                (fac000 * absa(ind0   ,ig) +                                   &
                 fac100 * absa(ind0 +1,ig) +                                   &
                 fac010 * absa(ind0 +9,ig) +                                   &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1   ,ig) +                                   &
                 fac101 * absa(ind1 +1,ig) +                                   &
                 fac011 * absa(ind1 +9,ig) +                                   &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!      ssa(lay,ig) = tauray/taug(lay,ig)
       taur(lay,ig) = tauray
     enddo
   enddo
!
   laysolfr = nlayers
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr)                  &
            laysolfr = lay
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(16) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(16) + 1
     tauray = colmol(lay) * rayl
     do ig = 1,ng16
       taug(lay,ig) = colch4(lay) *                                            &
                (fac00(lay) * absb(ind0  ,ig) +                                &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1  ,ig) +                                &
                 fac11(lay) * absb(ind1+1,ig)) 
!      ssa(lay,ig) = tauray/taug(lay,ig)
       if (lay .eq. laysolfr) sfluxzen(ig) = sfluxref(ig) 
       taur(lay,ig) = tauray  
     enddo
   enddo
!
   end subroutine taumol16
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol17
!-------------------------------------------------------------------------------
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng17, ngs16
   use rrsw_kg17_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, rayl, layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colh2o(lay) + strrat*colco2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(17) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(17) + js
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng17
       taug(lay,ngs16+ig) = speccomb *                                         &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!      ssa(lay,ngs16+ig) = tauray/taug(lay,ngs16+ig)
       taur(lay,ngs16+ig) = tauray
     enddo
   enddo
!
   laysolfr = nlayers
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr)                  &
            laysolfr = lay
     speccomb = colh2o(lay) + strrat*colco2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 4._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(17) + js
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(17) + js
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng17
       taug(lay,ngs16+ig) = speccomb *                                         &
                (fac000 * absb(ind0,ig) +                                      &
                 fac100 * absb(ind0+1,ig) +                                    &
                 fac010 * absb(ind0+5,ig) +                                    &
                 fac110 * absb(ind0+6,ig) +                                    &
                 fac001 * absb(ind1,ig) +                                      &
                 fac101 * absb(ind1+1,ig) +                                    &
                 fac011 * absb(ind1+5,ig) +                                    &
                 fac111 * absb(ind1+6,ig)) +                                   &
                 colh2o(lay) *                                                 &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig))) 
!      ssa(lay,ngs16+ig) = tauray/taug(lay,ngs16+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs16+ig) = sfluxref(ig,js)             &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs16+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol17
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol18
!-------------------------------------------------------------------------------
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng18, ngs17
   use rrsw_kg18_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, rayl, layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!      
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     speccomb = colh2o(lay) + strrat*colch4(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(18) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(18) + js
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng18
       taug(lay,ngs17+ig) = speccomb *                                         &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!      ssa(lay,ngs17+ig) = tauray/taug(lay,ngs17+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs17+ig) = sfluxref(ig,js)             &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs17+ig) = tauray
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(18) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(18) + 1
     tauray = colmol(lay) * rayl
!
     do ig = 1,ng18
       taug(lay,ngs17+ig) = colch4(lay) *                                      &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  &
                 fac11(lay) * absb(ind1+1,ig)) 
!      ssa(lay,ngs17+ig) = tauray/taug(lay,ngs17+ig)
       taur(lay,ngs17+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol18
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol19
!-------------------------------------------------------------------------------
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng19, ngs18
   use rrsw_kg19_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, rayl, layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop      
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     speccomb = colh2o(lay) + strrat*colco2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(19) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(19) + js
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng19
       taug(lay,ngs18+ig) = speccomb *                                         &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!      ssa(lay,ngs18+ig) = tauray/taug(lay,ngs18+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs18+ig) = sfluxref(ig,js)             &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs18+ig) = tauray   
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(19) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(19) + 1
     tauray = colmol(lay) * rayl
     do ig = 1,ng19
       taug(lay,ngs18+ig) = colco2(lay) *                                      &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  &
                 fac11(lay) * absb(ind1+1,ig)) 
!      ssa(lay,ngs18+ig) = tauray/taug(lay,ngs18+ig) 
       taur(lay,ngs18+ig) = tauray   
     enddo
   enddo
!
   end subroutine taumol19
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol20
!-------------------------------------------------------------------------------
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng20, ngs19
   use rrsw_kg20_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, absch4, rayl, layreffr
!
   implicit none
!
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(20) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(20) + 1
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng20
       taug(lay,ngs19+ig) = colh2o(lay) *                                      &
               ((fac00(lay) * absa(ind0,ig) +                                  &
                 fac10(lay) * absa(ind0+1,ig) +                                &
                 fac01(lay) * absa(ind1,ig) +                                  &
                 fac11(lay) * absa(ind1+1,ig)) +                               &
                 selffac(lay) * (selfref(inds,ig) +                            & 
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig))))                       &
                 + colch4(lay) * absch4(ig)
!      ssa(lay,ngs19+ig) = tauray/taug(lay,ngs19+ig)
       taur(lay,ngs19+ig) = tauray 
       if (lay .eq. laysolfr) sfluxzen(ngs19+ig) = sfluxref(ig) 
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(20) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(20) + 1
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng20
       taug(lay,ngs19+ig) = colh2o(lay) *                                      &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  &
                 fac11(lay) * absb(ind1+1,ig) +                                &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))) +                     &
                 colch4(lay) * absch4(ig)
!      ssa(lay,ngs19+ig) = tauray/taug(lay,ngs19+ig)
       taur(lay,ngs19+ig) = tauray 
     enddo
   enddo
!
   end subroutine taumol20
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol21
!-------------------------------------------------------------------------------
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng21, ngs20
   use rrsw_kg21_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, rayl, layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     speccomb = colh2o(lay) + strrat*colco2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(21) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(21) + js
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng21
       taug(lay,ngs20+ig) = speccomb *                                         &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig))))
!      ssa(lay,ngs20+ig) = tauray/taug(lay,ngs20+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs20+ig) = sfluxref(ig,js)             &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs20+ig) = tauray
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     speccomb = colh2o(lay) + strrat*colco2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 4._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(21) + js
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(21) + js
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng21
       taug(lay,ngs20+ig) = speccomb *                                         &
                (fac000 * absb(ind0,ig) +                                      &
                 fac100 * absb(ind0+1,ig) +                                    &
                 fac010 * absb(ind0+5,ig) +                                    &
                 fac110 * absb(ind0+6,ig) +                                    &
                 fac001 * absb(ind1,ig) +                                      &
                 fac101 * absb(ind1+1,ig) +                                    &
                 fac011 * absb(ind1+5,ig) +                                    &
                 fac111 * absb(ind1+6,ig)) +                                   &
                 colh2o(lay) *                                                 &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))
!      ssa(lay,ngs20+ig) = tauray/taug(lay,ngs20+ig)
       taur(lay,ngs20+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol21
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol22
!-------------------------------------------------------------------------------
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng22, ngs21
   use rrsw_kg22_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, rayl, layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray, o2adj, o2cont
!-------------------------------------------------------------------------------
!
! The following factor is the ratio of total O2 band intensity (lines 
! and Mate continuum) to O2 band intensity (line only).  It is needed
! to adjust the optical depths since the k's include only lines.
!
   o2adj = 1.6_rb
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     o2cont = 4.35e-4_rb*colo2(lay)/(350.0_rb*2.0_rb)
     speccomb = colh2o(lay) + o2adj*strrat*colo2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
!    odadj = specparm + o2adj * (1._rb - specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(22) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(22) + js
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng22
       taug(lay,ngs21+ig) = speccomb *                                         &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                  (selfref(inds+1,ig) - selfref(inds,ig))) +                   &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig))))                       &
                 + o2cont
!      ssa(lay,ngs21+ig) = tauray/taug(lay,ngs21+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs21+ig) = sfluxref(ig,js)             &
                + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs21+ig) = tauray
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     o2cont = 4.35e-4_rb*colo2(lay)/(350.0_rb*2.0_rb)
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(22) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(22) + 1
     tauray = colmol(lay) * rayl
     do ig = 1,ng22
       taug(lay,ngs21+ig) = colo2(lay) * o2adj *                               &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  &
                 fac11(lay) * absb(ind1+1,ig)) +                               &
                 o2cont
!      ssa(lay,ngs21+ig) = tauray/taug(lay,ngs21+ig)
       taur(lay,ngs21+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol22
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol23
!-------------------------------------------------------------------------------
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng23, ngs22
   use rrsw_kg23_k, only : absa, ka, forref, selfref,                            &
                            sfluxref, rayl, layreffr, givfac
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(23) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(23) + 1
     inds = indself(lay)
     indf = indfor(lay)
     do ig = 1,ng23
       tauray = colmol(lay) * rayl(ig)
       taug(lay,ngs22+ig) = colh2o(lay) *                                      &
                (givfac * (fac00(lay) * absa(ind0,ig) +                        &
                 fac10(lay) * absa(ind0+1,ig) +                                &
                 fac01(lay) * absa(ind1,ig) +                                  &
                 fac11(lay) * absa(ind1+1,ig)) +                               &
                 selffac(lay) * (selfref(inds,ig) +                            &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!      ssa(lay,ngs22+ig) = tauray/taug(lay,ngs22+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs22+ig) = sfluxref(ig) 
       taur(lay,ngs22+ig) = tauray
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     do ig = 1,ng23
!      taug(lay,ngs22+ig) = colmol(lay) * rayl(ig)
!      ssa(lay,ngs22+ig) = 1.0_rb
       taug(lay,ngs22+ig) = 0._rb
       taur(lay,ngs22+ig) = colmol(lay) * rayl(ig) 
     enddo
   enddo
!
   end subroutine taumol23
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol24
!-------------------------------------------------------------------------------
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng24, ngs23
   use rrsw_kg24_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, abso3a, abso3b, rayla, raylb,            &
                            layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     speccomb = colh2o(lay) + strrat*colo2(lay)
     specparm = colh2o(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(24) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(24) + js
     inds = indself(lay)
     indf = indfor(lay)
     do ig = 1,ng24
       tauray = colmol(lay) * (rayla(ig,js) +                                  &
               fs * (rayla(ig,js+1) - rayla(ig,js)))
            taug(lay,ngs23+ig) = speccomb *                                    &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) +                                  &
                 colo3(lay) * abso3a(ig) +                                     &
                 colh2o(lay) *                                                 &
                 (selffac(lay) * (selfref(inds,ig) +                           &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig))))
!      ssa(lay,ngs23+ig) = tauray/taug(lay,ngs23+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs23+ig) = sfluxref(ig,js)             &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs23+ig) = tauray
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(24) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(24) + 1
     do ig = 1,ng24
       tauray = colmol(lay) * raylb(ig)
       taug(lay,ngs23+ig) = colo2(lay) *                                       &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  &
                 fac11(lay) * absb(ind1+1,ig)) +                               &
                 colo3(lay) * abso3b(ig)
!      ssa(lay,ngs23+ig) = tauray/taug(lay,ngs23+ig)
       taur(lay,ngs23+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol24
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol25
!-------------------------------------------------------------------------------
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng25, ngs24
   use rrsw_kg25_k, only : absa, ka,                                             &
                            sfluxref, abso3a, abso3b, rayl, layreffr
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr)                  &
            laysolfr = min(lay+1,laytrop)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(25) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(25) + 1
     do ig = 1,ng25
       tauray = colmol(lay) * rayl(ig)
       taug(lay,ngs24+ig) = colh2o(lay) *                                      &
                (fac00(lay) * absa(ind0,ig) +                                  &
                 fac10(lay) * absa(ind0+1,ig) +                                &
                 fac01(lay) * absa(ind1,ig) +                                  &
                 fac11(lay) * absa(ind1+1,ig)) +                               &
                 colo3(lay) * abso3a(ig) 
!      ssa(lay,ngs24+ig) = tauray/taug(lay,ngs24+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs24+ig) = sfluxref(ig) 
       taur(lay,ngs24+ig) = tauray
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     do ig = 1,ng25
       tauray = colmol(lay) * rayl(ig)
       taug(lay,ngs24+ig) = colo3(lay) * abso3b(ig) 
!      ssa(lay,ngs24+ig) = tauray/taug(lay,ngs24+ig)
       taur(lay,ngs24+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol25
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol26
!-------------------------------------------------------------------------------
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng26, ngs25
   use rrsw_kg26_k, only : sfluxref, rayl
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
   laysolfr = laytrop
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     do ig = 1,ng26 
!      taug(lay,ngs25+ig) = colmol(lay) * rayl(ig)
!      ssa(lay,ngs25+ig) = 1.0_rb
       if (lay .eq. laysolfr) sfluxzen(ngs25+ig) = sfluxref(ig) 
       taug(lay,ngs25+ig) = 0._rb
       taur(lay,ngs25+ig) = colmol(lay) * rayl(ig) 
     enddo
   enddo
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     do ig = 1,ng26
!      taug(lay,ngs25+ig) = colmol(lay) * rayl(ig)
!      ssa(lay,ngs25+ig) = 1.0_rb
       taug(lay,ngs25+ig) = 0._rb
       taur(lay,ngs25+ig) = colmol(lay) * rayl(ig) 
     enddo
   enddo
!
   end subroutine taumol26
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol27
!-------------------------------------------------------------------------------
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng27, ngs26
   use rrsw_kg27_k, only : absa, ka, absb, kb,                                   &
                            sfluxref, rayl, layreffr, scalekur
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(27) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(27) + 1
     do ig = 1,ng27
       tauray = colmol(lay) * rayl(ig)
       taug(lay,ngs26+ig) = colo3(lay) *                                       &
                (fac00(lay) * absa(ind0,ig) +                                  &
                 fac10(lay) * absa(ind0+1,ig) +                                &
                 fac01(lay) * absa(ind1,ig) +                                  &
                 fac11(lay) * absa(ind1+1,ig))
!      ssa(lay,ngs26+ig) = tauray/taug(lay,ngs26+ig)
       taur(lay,ngs26+ig) = tauray
     enddo
   enddo
!
   laysolfr = nlayers
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr)                  &
            laysolfr = lay
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(27) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(27) + 1
     do ig = 1,ng27
       tauray = colmol(lay) * rayl(ig)
       taug(lay,ngs26+ig) = colo3(lay) *                                       &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  & 
                 fac11(lay) * absb(ind1+1,ig))
!      ssa(lay,ngs26+ig) = tauray/taug(lay,ngs26+ig)
       if (lay.eq.laysolfr) sfluxzen(ngs26+ig) = scalekur * sfluxref(ig) 
       taur(lay,ngs26+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol27
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol28
!-------------------------------------------------------------------------------
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)
!------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng28, ngs27
   use rrsw_kg28_k, only : absa, ka, absb, kb,                                   &
                            sfluxref, rayl, layreffr, strrat
!
! ------- Declarations -------
!
! Local
!
   integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
   real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,            &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     speccomb = colo3(lay) + strrat*colo2(lay)
     specparm = colo3(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 8._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(28) + js
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(28) + js
     tauray = colmol(lay) * rayl
     do ig = 1,ng28
       taug(lay,ngs27+ig) = speccomb *                                         &
                (fac000 * absa(ind0,ig) +                                      &
                 fac100 * absa(ind0+1,ig) +                                    &
                 fac010 * absa(ind0+9,ig) +                                    &
                 fac110 * absa(ind0+10,ig) +                                   &
                 fac001 * absa(ind1,ig) +                                      &
                 fac101 * absa(ind1+1,ig) +                                    &
                 fac011 * absa(ind1+9,ig) +                                    &
                 fac111 * absa(ind1+10,ig)) 
!      ssa(lay,ngs27+ig) = tauray/taug(lay,ngs27+ig)
       taur(lay,ngs27+ig) = tauray
     enddo
   enddo
!
   laysolfr = nlayers
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr)                  &
            laysolfr = lay
     speccomb = colo3(lay) + strrat*colo2(lay)
     specparm = colo3(lay)/speccomb 
     if (specparm .ge. oneminus) specparm = oneminus
     specmult = 4._rb*(specparm)
     js = 1 + int(specmult)
     fs = mod(specmult, 1._rb )
     fac000 = (1._rb - fs) * fac00(lay)
     fac010 = (1._rb - fs) * fac10(lay)
     fac100 = fs * fac00(lay)
     fac110 = fs * fac10(lay)
     fac001 = (1._rb - fs) * fac01(lay)
     fac011 = (1._rb - fs) * fac11(lay)
     fac101 = fs * fac01(lay)
     fac111 = fs * fac11(lay)
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(28) + js
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(28) + js
     tauray = colmol(lay) * rayl
     do ig = 1,ng28
       taug(lay,ngs27+ig) = speccomb *                                         &
                (fac000 * absb(ind0,ig) +                                      &
                 fac100 * absb(ind0+1,ig) +                                    &
                 fac010 * absb(ind0+5,ig) +                                    &
                 fac110 * absb(ind0+6,ig) +                                    &
                 fac001 * absb(ind1,ig) +                                      &
                 fac101 * absb(ind1+1,ig) +                                    &
                 fac011 * absb(ind1+5,ig) +                                    &
                 fac111 * absb(ind1+6,ig)) 
!      ssa(lay,ngs27+ig) = tauray/taug(lay,ngs27+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs27+ig) = sfluxref(ig,js)             &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
       taur(lay,ngs27+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol28
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine taumol29
!-------------------------------------------------------------------------------
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)
!-------------------------------------------------------------------------------
!
! ------- Modules -------
!
   use parrrsw_k, only : ng29, ngs28
   use rrsw_kg29_k, only : absa, ka, absb, kb, forref, selfref,                  &
                            sfluxref, absh2o, absco2, rayl, layreffr
!
! ------- Declarations -------
! Local
      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101,         &
                       fac110, fac111, fs, speccomb, specmult, specparm,       &
                       tauray
!-------------------------------------------------------------------------------
!
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  
!
! Lower atmosphere loop
!
   do lay = 1,laytrop
     ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(29) + 1
     ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(29) + 1
     inds = indself(lay)
     indf = indfor(lay)
     tauray = colmol(lay) * rayl
     do ig = 1,ng29
       taug(lay,ngs28+ig) = colh2o(lay) *                                      &
               ((fac00(lay) * absa(ind0,ig) +                                  &
                 fac10(lay) * absa(ind0+1,ig) +                                &
                 fac01(lay) * absa(ind1,ig) +                                  &
                 fac11(lay) * absa(ind1+1,ig)) +                               &
                 selffac(lay) * (selfref(inds,ig) +                            &
                 selffrac(lay) *                                               &
                 (selfref(inds+1,ig) - selfref(inds,ig))) +                    &
                 forfac(lay) * (forref(indf,ig) +                              &
                 forfrac(lay) *                                                &
                 (forref(indf+1,ig) - forref(indf,ig))))                       &
                 + colco2(lay) * absco2(ig) 
!      ssa(lay,ngs28+ig) = tauray/taug(lay,ngs28+ig)
       taur(lay,ngs28+ig) = tauray
     enddo
   enddo
!
   laysolfr = nlayers
!
! Upper atmosphere loop
!
   do lay = laytrop+1,nlayers
     if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr)                  &
            laysolfr = lay
     ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(29) + 1
     ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(29) + 1
     tauray = colmol(lay) * rayl
     do ig = 1,ng29
       taug(lay,ngs28+ig) = colco2(lay) *                                      &
                (fac00(lay) * absb(ind0,ig) +                                  &
                 fac10(lay) * absb(ind0+1,ig) +                                &
                 fac01(lay) * absb(ind1,ig) +                                  &
                 fac11(lay) * absb(ind1+1,ig))                                 &
                 + colh2o(lay) * absh2o(ig) 
!      ssa(lay,ngs28+ig) = tauray/taug(lay,ngs28+ig)
       if (lay .eq. laysolfr) sfluxzen(ngs28+ig) = sfluxref(ig) 
       taur(lay,ngs28+ig) = tauray
     enddo
   enddo
!
   end subroutine taumol29
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end subroutine taumol_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_taumol_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_init_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use rrsw_wvn_k
   use rrtmg_sw_setcoef_k, only: swatmref
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rrtmg_sw_ini(cpdair)
!-------------------------------------------------------------------------------
!  abstract : 
!    This subroutine performs calculations necessary for the initialization
!    of the shortwave model.  Lookup tables are computed for use in the SW
!    radiative transfer, and input absorption coefficient data for each
!    spectral band are reduced from 224 g-point intervals to 112.
!
!  history log : 
!    2004-02 Michael J. Iacono  Original version
!    2006-07 M. J. Iacono       Revision for F90 formatting
!
!-------------------------------------------------------------------------------
   use parrrsw_k, only : mg, nbndsw, ngptsw
   use rrsw_tbl_k, only: ntbl, tblint, pade, bpade, tau_tbl, exp_tbl
   use rrsw_vsn_k, only: hvrini, hnamini
!
   real(kind=rb), intent(in) :: cpdair     ! Specific heat capacity of dry air
                                           ! at constant pressure at 273 K
                                           ! (J kg-1 K-1)
!
! ------- Local -------
!
   integer(kind=im) :: ibnd, igc, ig, ind, ipr
   integer(kind=im) :: igcsm, iprsm
   integer(kind=im) :: itr
!
   real(kind=rb) :: wtsum, wtsm(mg)
   real(kind=rb) :: tfn
!
   real(kind=rb), parameter :: expeps = 1.e-20                                 
                               ! Smallest value for exponential table
!
! ------- Definitions -------
!     Arrays for 10000-point look-up tables:
!     TAU_TBL  Clear-sky optical depth 
!     EXP_TBL  Exponential lookup table for transmittance
!     PADE     Pade approximation constant (= 0.278)
!     BPADE    Inverse of the Pade approximation constant
!
   hvrini = '$Revision: 1.3 $'
!
! Initialize model data
!
   call swdatinit(cpdair)
   call swcmbdat              ! g-point interval reduction data
   call swaerpr               ! aerosol optical properties
   call swcldpr               ! cloud optical properties
   call swatmref              ! reference MLS profile
!
! Moved to module_ra_rrtmg_sw for WRF
!  call sw_kgb16              ! molecular absorption coefficients
!  call sw_kgb17
!  call sw_kgb18
!  call sw_kgb19
!  call sw_kgb20
!  call sw_kgb21
!  call sw_kgb22
!  call sw_kgb23
!  call sw_kgb24
!  call sw_kgb25
!  call sw_kgb26
!  call sw_kgb27
!  call sw_kgb28
!  call sw_kgb29
!
! Define exponential lookup tables for transmittance. Tau is
! computed as a function of the tau transition function, and transmittance 
! is calculated as a function of tau.  All tables are computed at intervals 
! of 0.0001.  The inverse of the constant used in the Pade approximation to 
! the tau transition function is set to bpade.
!
   exp_tbl(0) = 1.0_rb
   exp_tbl(ntbl) = expeps
   bpade = 1.0_rb / pade
   do itr = 1,ntbl-1
     tfn = real(itr) / real(ntbl)
     tau_tbl = bpade * tfn / (1._rb - tfn)
     exp_tbl(itr) = exp(-tau_tbl)
     if (exp_tbl(itr) .le. expeps) exp_tbl(itr) = expeps
   enddo
!
! Perform g-point reduction from 16 per band (224 total points) to
! a band dependent number (112 total points) for all absorption
! coefficient input data and Planck fraction input data.
! Compute relative weighting for new g-point combinations.
!
   igcsm = 0
   do ibnd = 1,nbndsw
     iprsm = 0
     if (ngc(ibnd).lt.mg) then
       do igc = 1,ngc(ibnd)
         igcsm = igcsm + 1
         wtsum = 0.
         do ipr = 1,ngn(igcsm)
           iprsm = iprsm + 1
           wtsum = wtsum + wt(iprsm)
         enddo
         wtsm(igc) = wtsum
       enddo
       do ig = 1,ng(ibnd+15)
         ind = (ibnd-1)*mg + ig
         rwgt(ind) = wt(ig)/wtsm(ngm(ind))
       enddo
     else
       do ig = 1,ng(ibnd+15)
         igcsm = igcsm + 1
         ind = (ibnd-1)*mg + ig
         rwgt(ind) = 1.0_rb
       enddo
     endif
   enddo
!
! Reduce g-points for absorption coefficient data in each LW spectral band.
!
   call cmbgb16s
   call cmbgb17
   call cmbgb18
   call cmbgb19
   call cmbgb20
   call cmbgb21
   call cmbgb22
   call cmbgb23
   call cmbgb24
   call cmbgb25
   call cmbgb26
   call cmbgb27
   call cmbgb28
   call cmbgb29
!
   end subroutine rrtmg_sw_ini
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine swdatinit(cpdair)
!
! --------- Modules ----------
!
   use rrsw_con_k, only: heatfac, grav, planck, boltz,                         &
                          clight, avogad, alosmt, gascon, radcn1, radcn2,      &
                          sbcnst, secdy 
   use rrsw_vsn_k
!
   save 
! 
   real(kind=rb), intent(in) :: cpdair     ! Specific heat capacity of dry air
                                           ! at constant pressure at 273 K
                                           ! (J kg-1 K-1)
!
! Shortwave spectral band limits (wavenumbers)
!
   wavenum1(:) = (/2600._rb, 3250._rb, 4000._rb, 4650._rb, 5150._rb, 6150._rb, &
                   7700._rb, 8050._rb,12850._rb,16000._rb,22650._rb,29000._rb, &
                   38000._rb,  820._rb/)
   wavenum2(:) = (/3250._rb, 4000._rb, 4650._rb, 5150._rb, 6150._rb, 7700._rb, &
                   8050._rb, 2850._rb,16000._rb,22650._rb,29000._rb,38000._rb, &
                   50000._rb, 2600._rb/)
   delwave(:) =  (/ 650._rb,  750._rb,  650._rb,  500._rb, 1000._rb, 1550._rb, &
                    350._rb, 4800._rb, 3150._rb, 6650._rb, 6350._rb, 9000._rb, &
                   12000._rb, 1780._rb/)
!
! Spectral band information
!
   ng(:) = (/16,16,16,16,16,16,16,16,16,16,16,16,16,16/)
   nspa(:) = (/9,9,9,9,1,9,9,1,9,1,0,1,9,1/)
   nspb(:) = (/1,5,1,1,1,5,1,0,1,0,0,1,5,1/)
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
   radcn1 = 1.191042772e-12_rb             ! First radiation constant
                                           ! (W cm2 sr-1)
   radcn2 = 1.4387752_rb                   ! Second radiation constant
                                           ! (cm K)
   sbcnst = 5.670400e-04_rb                ! Stefan-Boltzmann constant
                                           ! (W cm-2 K-4)
   secdy = 8.6400e4_rb                     ! Number of seconds per day
                                           ! (s d-1)
!
!     units are generally cgs
!
!     The first and second radiation constants are taken from NIST.
!     They were previously obtained from the relations:
!          radcn1 = 2.*planck*clight*clight*1.e-07
!          radcn2 = planck*clight/boltz
!
!     Heatfac is the factor by which delta-flux / delta-pressure is
!     multiplied, with flux in W/m-2 and pressure in mbar, to get 
!     the heating rate in units of degrees/day.  It is equal to:
!     Original value:
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!           Here, cpdair (1.004) is in units of J g-1 K-1, and the 
!           constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!        =  (9.8066)(86400)(1e-5)/(1.004)
!      heatfac = 8.4391_rb
!
!     Modified value for consistency with CAM3:
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!           Here, cpdair (1.00464) is in units of J g-1 K-1, and the
!           constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!        =  (9.80616)(86400)(1e-5)/(1.00464)
!      heatfac = 8.43339130434_rb
!
!     Calculated value (from constants above and input cpdair)
!        (grav) x (#sec/day) / (specific heat of dry air at const. p x 1.e2)
!           Here, cpdair is in units of J kg-1 K-1, and the constant (1.e2) 
!           converts mb to Pa when heatfac is multiplied by W m-2 mb-1. 
!
   heatfac = grav * secdy / (cpdair * 1.e2_rb)
!
   end subroutine swdatinit
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine swcmbdat
!-------------------------------------------------------------------------------
! 
! ------- Definitions -------
!     Arrays for the g-point reduction from 224 to 112 for the 16 LW bands:
!     This mapping from 224 to 112 points has been carefully selected to 
!     minimize the effect on the resulting fluxes and cooling rates, and
!     caution should be used if the mapping is modified.  The full 224
!     g-point set can be restored with ngpt=224, ngc=16*16, ngn=224*1., etc.
!     ngpt    The total number of new g-points
!     ngc     The number of new g-points in each band
!     ngs     The cumulative sum of new g-points for each band
!     ngm     The index of each new g-point relative to the original
!             16 g-points for each band.  
!     ngn     The number of original g-points that are combined to make
!             each new g-point in each band.
!     ngb     The band index for each new g-point.
!     wt      RRTM weights for 16 g-points.
!
! Use this set for 112 quadrature point (g-point) model
! ------- Data statements -------
!
   save
   ngc(:) = (/ 6,12, 8, 8,10,10, 2,10, 8, 6, 6, 8, 6,12 /)
   ngs(:) = (/ 6,18,26,34,44,54,56,66,74,80,86,94,100,112 /)
   ngm(:) = (/ 1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6,                                &
               ! band 16
               1,2,3,4,5,6,6,7,8,8,9,10,10,11,12,12,                           &
               ! band 17
               1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8,                                &
               ! band 18
               1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8,                                &
               ! band 19
               1,2,3,4,5,6,7,8,9,9,10,10,10,10,10,10,                          &
               ! band 20
               1,2,3,4,5,6,7,8,9,9,10,10,10,10,10,10,                          &
               ! band 21
               1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,                                &
               ! band 22
               1,1,2,2,3,4,5,6,7,8,9,9,10,10,10,10,                            &
               ! band 23
               1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,                                &
               ! band 24
               1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6,                                &
               ! band 25
               1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6,                                &
               ! band 26
               1,2,3,4,5,6,7,7,7,7,8,8,8,8,8,8,                                &
               ! band 27
               1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6,                                &
               ! band 28
               1,2,3,4,5,5,6,6,7,7,8,8,9,10,11,12 /)                           
               ! band 29
   ngn(:) = (/ 2,2,2,2,4,4,                                                    &
               ! band 16
               1,1,1,1,1,2,1,2,1,2,1,2,                                        &
               ! band 17
               1,1,1,1,2,2,4,4,                                                &
               ! band 18
               1,1,1,1,2,2,4,4,                                                &
               ! band 19
               1,1,1,1,1,1,1,1,2,6,                                            &
               ! band 20
               1,1,1,1,1,1,1,1,2,6,                                            &
               ! band 21
               8,8,                                                            &
               ! band 22
               2,2,1,1,1,1,1,1,2,4,                                            &
               ! band 23
               2,2,2,2,2,2,2,2,                                                &
               ! band 24
               1,1,2,2,4,6,                                                    &
               ! band 25
               1,1,2,2,4,6,                                                    &
               ! band 26
               1,1,1,1,1,1,4,6,                                                &
               ! band 27
               1,1,2,2,4,6,                                                    &
               ! band 28
               1,1,1,1,2,2,2,2,1,1,1,1 /)                                      
               ! band 29
   ngb(:) = (/ 16,16,16,16,16,16,                                              &
               ! band 16
               17,17,17,17,17,17,17,17,17,17,17,17,                            &
               ! band 17
               18,18,18,18,18,18,18,18,                                        &
               ! band 18
               19,19,19,19,19,19,19,19,                                        &
               ! band 19
               20,20,20,20,20,20,20,20,20,20,                                  &
               ! band 20
               21,21,21,21,21,21,21,21,21,21,                                  &
               ! band 21
               22,22,                                                          &
               ! band 22
               23,23,23,23,23,23,23,23,23,23,                                  &
               ! band 23
               24,24,24,24,24,24,24,24,                                        &
               ! band 24
               25,25,25,25,25,25,                                              &
               ! band 25
               26,26,26,26,26,26,                                              &
               ! band 26
               27,27,27,27,27,27,27,27,                                        &
               ! band 27
               28,28,28,28,28,28,                                              &
               ! band 28
               29,29,29,29,29,29,29,29,29,29,29,29 /)                          
               ! band 29
!
! Use this set for full 224 quadrature point (g-point) model
! ------- Data statements -------
!      ngc(:) = (/ 16,16,16,16,16,16,16,16,16,16,16,16,16,16 /)
!      ngs(:) = (/ 16,32,48,64,80,96,112,128,144,160,176,192,208,224 /)
!      ngm(:) = (/ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 16
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 17
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 18
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 19
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 20
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 21
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 22
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 23
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 24
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 25
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 26
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 27
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 28
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 /)    ! band 29
!      ngn(:) = (/ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 16
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 17
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 18
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 19
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 20
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 21
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 22
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 23
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 24
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 25
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 26
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 27
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 28
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 /)           ! band 29
!      ngb(:) = (/ 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16, &  ! band 16
!                  17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17, &  ! band 17
!                  18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18, &  ! band 18
!                  19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19, &  ! band 19
!                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20, &  ! band 20
!                  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21, &  ! band 21
!                  22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22, &  ! band 22
!                  23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23, &  ! band 23
!                  24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24, &  ! band 24
!                  25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25, &  ! band 25
!                  26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26, &  ! band 26
!                  27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27, &  ! band 27
!                  28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28, &  ! band 28
!                  29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29 /)  ! band 29
   wt(:) =  (/ 0.1527534276_rb, 0.1491729617_rb, 0.1420961469_rb,              &
               0.1316886544_rb, 0.1181945205_rb, 0.1019300893_rb,              &
               0.0832767040_rb, 0.0626720116_rb, 0.0424925000_rb,              &
               0.0046269894_rb, 0.0038279891_rb, 0.0030260086_rb,              &
               0.0022199750_rb, 0.0014140010_rb, 0.0005330000_rb,              &
               0.0000750000_rb /)
!
   end subroutine swcmbdat
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine swaerpr
!-------------------------------------------------------------------------------
!
! abstract: 
!   Define spectral aerosol properties for six ECMWF aerosol types
!   as used in the ECMWF IFS model (see module rrsw_aer.F90 for details)
!
!  history log :
!    2003-02 JJMorcrette, ECMWF  Defined for rrtmg_sw 14 spectral bands
!    2006-07 MJIacono, AER       Reformatted for consistency with rrtmg_lw
!
!-------------------------------------------------------------------------------
   use rrsw_aer_k, only : rsrtaua, rsrpiza, rsrasya
!
   save
!
   rsrtaua( 1, :) = (/                                                         &
     0.10849_rb, 0.66699_rb, 0.65255_rb, 0.11600_rb, 0.06529_rb, 0.04468_rb/)
   rsrtaua( 2, :) = (/                                                         &
     0.10849_rb, 0.66699_rb, 0.65255_rb, 0.11600_rb, 0.06529_rb, 0.04468_rb/)
   rsrtaua( 3, :) = (/                                                         &
     0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
   rsrtaua( 4, :) = (/                                                         &
     0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
   rsrtaua( 5, :) = (/                                                         &
     0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
   rsrtaua( 6, :) = (/                                                         &
     0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
   rsrtaua( 7, :) = (/                                                         &
     0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
   rsrtaua( 8, :) = (/                                                         &
     0.52838_rb, 0.93285_rb, 0.93449_rb, 0.53078_rb, 0.67148_rb, 0.46608_rb/)
   rsrtaua( 9, :) = (/                                                         &
     0.52838_rb, 0.93285_rb, 0.93449_rb, 0.53078_rb, 0.67148_rb, 0.46608_rb/)
   rsrtaua(10, :) = (/                                                         &
     1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
   rsrtaua(11, :) = (/                                                         &
     1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
   rsrtaua(12, :) = (/                                                         &
     1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
   rsrtaua(13, :) = (/                                                         &
     1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
   rsrtaua(14, :) = (/                                                         &
     0.10849_rb, 0.66699_rb, 0.65255_rb, 0.11600_rb, 0.06529_rb, 0.04468_rb/)
! 
   rsrpiza( 1, :) = (/.5230504_rb, .7868518_rb, .8531531_rb, .4048149_rb,      &
     .8748231_rb, .2355667_rb/)
   rsrpiza( 2, :) = (/.5230504_rb, .7868518_rb, .8531531_rb, .4048149_rb,      &
     .8748231_rb, .2355667_rb/)
   rsrpiza( 3, :) = (/.8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb,      &
     .9467578_rb, .9955938_rb/)
   rsrpiza( 4, :) = (/.8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb,      &
     .9467578_rb, .9955938_rb/)
   rsrpiza( 5, :) = (/.8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb,      &
     .9467578_rb, .9955938_rb/)
   rsrpiza( 6, :) = (/.8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb,      &
     .9467578_rb, .9955938_rb/)
   rsrpiza( 7, :) = (/.8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb,      &
     .9467578_rb, .9955938_rb/)
   rsrpiza( 8, :) = (/.8970131_rb, .9984940_rb, .9245594_rb, .7768385_rb,      &
     .9532763_rb, .9999999_rb/)
   rsrpiza( 9, :) = (/.8970131_rb, .9984940_rb, .9245594_rb, .7768385_rb,      &
     .9532763_rb, .9999999_rb/)
   rsrpiza(10, :) = (/.9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb,      &
     .9401905_rb, .9999999_rb/)
   rsrpiza(11, :) = (/.9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb,      &
     .9401905_rb, .9999999_rb/)
   rsrpiza(12, :) = (/.9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb,      &
     .9401905_rb, .9999999_rb/)
   rsrpiza(13, :) = (/.9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb,      &
     .9401905_rb, .9999999_rb/)
   rsrpiza(14, :) = (/.5230504_rb, .7868518_rb, .8531531_rb, .4048149_rb,      &
     .8748231_rb, .2355667_rb/)
!
   rsrasya( 1, :) = (/0.700610_rb, 0.818871_rb, 0.702399_rb, 0.689886_rb,      &
     .4629866_rb, .1907639_rb/)
   rsrasya( 2, :) = (/0.700610_rb, 0.818871_rb, 0.702399_rb, 0.689886_rb,      &
     .4629866_rb, .1907639_rb/)
   rsrasya( 3, :) = (/0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb,      &
     .6105750_rb, .4760794_rb/)
   rsrasya( 4, :) = (/0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb,      &
     .6105750_rb, .4760794_rb/)
   rsrasya( 5, :) = (/0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb,      &
     .6105750_rb, .4760794_rb/)
   rsrasya( 6, :) = (/0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb,      &
     .6105750_rb, .4760794_rb/)
   rsrasya( 7, :) = (/0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb,      &
     .6105750_rb, .4760794_rb/)
   rsrasya( 8, :) = (/0.668431_rb, 0.788530_rb, 0.698682_rb, 0.657422_rb,      &
     .6735182_rb, .6519706_rb/)
   rsrasya( 9, :) = (/0.668431_rb, 0.788530_rb, 0.698682_rb, 0.657422_rb,      &
     .6735182_rb, .6519706_rb/)
   rsrasya(10, :) = (/0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb,      &
     .7008249_rb, .7270548_rb/)
   rsrasya(11, :) = (/0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb,      &
     .7008249_rb, .7270548_rb/)
   rsrasya(12, :) = (/0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb,      &
     .7008249_rb, .7270548_rb/)
   rsrasya(13, :) = (/0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb,      &
     .7008249_rb, .7270548_rb/)
   rsrasya(14, :) = (/0.700610_rb, 0.818871_rb, 0.702399_rb, 0.689886_rb,      &
     .4629866_rb, .1907639_rb/)
!
   end subroutine swaerpr
!-------------------------------------------------------------------------------
! 
!
!-------------------------------------------------------------------------------
   subroutine cmbgb16s
!-------------------------------------------------------------------------------
!
!  abstract :
!    The subroutines CMBGB16->CMBGB29 input the absorption coefficient
!    data for each band, which are defined for 16 g-points and 14 spectral
!    bands. The data are combined with appropriate weighting following the
!    g-point mapping arrays specified in RRTMG_SW_INIT.  Solar source 
!    function data in array SFLUXREF are combined without weighting.  All
!    g-point reduced data are put into new arrays for use in RRTMG_SW.
!
!    band 16:  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!
!  history log :
!    1998-07  MJIacono   Original version
!    2002-11  MJIacono   Revision for RRTM_SW
!    2003-12  MJIacono   Revision for RRTMG_SW
!    2006-07  MJIacono   Revision for F90 reformatting
!-----------------------------------------------------------------------
!
   use rrsw_kg16_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absa, ka, absb, kb, selfref, forref, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(1)
           sumk = 0.
           do ipr = 1,ngn(igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm)
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
   do jt = 1,3
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
   iprsm = 0
   do igc = 1,ngc(1)
     sumf = 0.
     do ipr = 1,ngn(igc)
       iprsm = iprsm + 1
       sumf = sumf + sfluxrefo(iprsm)
     enddo
     sfluxref(igc) = sumf
   enddo
!
   end subroutine cmbgb16s
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb17
!-------------------------------------------------------------------------------
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg17_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absa, ka, absb, kb, selfref, forref, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(2)
           sumk = 0.
           do ipr = 1,ngn(ngs(1)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+16)
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
         do igc = 1,ngc(2)
           sumk = 0.
           do ipr = 1,ngn(ngs(1)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+16)
           enddo
           kb(jn,jt,jp,igc) = sumk
         enddo
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
   do jp = 1,5
     iprsm = 0
     do igc = 1,ngc(2)
       sumf = 0.
       do ipr = 1,ngn(ngs(1)+igc)
         iprsm = iprsm + 1
         sumf = sumf + sfluxrefo(iprsm,jp)
       enddo
       sfluxref(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb17
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb18
!-------------------------------------------------------------------------------
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!-------------------------------------------------------------------------------
!
   use rrsw_kg18_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absa, ka, absb, kb, selfref, forref, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
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
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(3)
         sumk = 0.
         do ipr = 1,ngn(ngs(2)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+32)
         enddo
         kb(jt,jp,igc) = sumk
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
   do jt = 1,3
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
         sumf = sumf + sfluxrefo(iprsm,jp)
       enddo
       sfluxref(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb18
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb19
!-------------------------------------------------------------------------------
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg19_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absa, ka, absb, kb, selfref, forref, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
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
   do jt = 1,5
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(4)
         sumk = 0.
         do ipr = 1,ngn(ngs(3)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+48)
         enddo
         kb(jt,jp,igc) = sumk
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
   do jt = 1,3
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
         sumf = sumf + sfluxrefo(iprsm,jp)
       enddo
       sfluxref(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb19
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb20
!-------------------------------------------------------------------------------
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!-------------------------------------------------------------------------------
!
   use rrsw_kg20_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo, absch4o,    &
                         absa, ka, absb, kb, selfref, forref, sfluxref, absch4
!
! ------- Local -------
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf1, sumf2
!-------------------------------------------------------------------------------
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(5)
         sumk = 0.
         do ipr = 1,ngn(ngs(4)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+64)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(5)
         sumk = 0.
         do ipr = 1,ngn(ngs(4)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+64)
         enddo
         kb(jt,jp,igc) = sumk
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
   iprsm = 0
   do igc = 1,ngc(5)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(4)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + sfluxrefo(iprsm)
       sumf2 = sumf2 + absch4o(iprsm)*rwgt(iprsm+64)
     enddo
     sfluxref(igc) = sumf1
     absch4(igc) = sumf2
   enddo
!
   end subroutine cmbgb20
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb21
!-------------------------------------------------------------------------------
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg21_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absa, ka, absb, kb, selfref, forref, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
   do jn = 1,9
     do jt = 1,5
       do jp = 1,13
         iprsm = 0
         do igc = 1,ngc(6)
           sumk = 0.
           do ipr = 1,ngn(ngs(5)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+80)
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
         do igc = 1,ngc(6)
           sumk = 0.
           do ipr = 1,ngn(ngs(5)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+80)
           enddo
           kb(jn,jt,jp,igc) = sumk
         enddo
       enddo
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
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(6)
       sumf = 0.
       do ipr = 1,ngn(ngs(5)+igc)
         iprsm = iprsm + 1
         sumf = sumf + sfluxrefo(iprsm,jp)
       enddo
       sfluxref(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb21
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb22
!-------------------------------------------------------------------------------
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg22_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absa, ka, absb, kb, selfref, forref, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
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
   do jt = 1,3
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
         sumf = sumf + sfluxrefo(iprsm,jp)
       enddo
       sfluxref(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb22
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb23
!-------------------------------------------------------------------------------
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!-------------------------------------------------------------------------------
!
   use rrsw_kg23_k, only : kao, selfrefo, forrefo, sfluxrefo, raylo,           &
                            absa, ka, selfref, forref, sfluxref, rayl
!
! ------- Local -------
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf1, sumf2
!-------------------------------------------------------------------------------
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
   do jt = 1,3
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
   iprsm = 0
   do igc = 1,ngc(8)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(7)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + sfluxrefo(iprsm)
       sumf2 = sumf2 + raylo(iprsm)*rwgt(iprsm+112)
     enddo
     sfluxref(igc) = sumf1
     rayl(igc) = sumf2
   enddo
!
   end subroutine cmbgb23
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb24
!-------------------------------------------------------------------------------
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg24_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            abso3ao, abso3bo, raylao, raylbo,                  &
                            absa, ka, absb, kb, selfref, forref, sfluxref,     &
                            abso3a, abso3b, rayla, raylb
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf1, sumf2, sumf3
!-------------------------------------------------------------------------------
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
   do jt = 1,3
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
   iprsm = 0
   do igc = 1,ngc(9)
     sumf1 = 0.
     sumf2 = 0.
     sumf3 = 0.
     do ipr = 1,ngn(ngs(8)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + raylbo(iprsm)*rwgt(iprsm+128)
       sumf2 = sumf2 + abso3ao(iprsm)*rwgt(iprsm+128)
       sumf3 = sumf3 + abso3bo(iprsm)*rwgt(iprsm+128)
     enddo
     raylb(igc) = sumf1
     abso3a(igc) = sumf2
     abso3b(igc) = sumf3
   enddo
!
   do jp = 1,9
     iprsm = 0
     do igc = 1,ngc(9)
       sumf1 = 0.
       sumf2 = 0.
       do ipr = 1,ngn(ngs(8)+igc)
         iprsm = iprsm + 1
         sumf1 = sumf1 + sfluxrefo(iprsm,jp)
         sumf2 = sumf2 + raylao(iprsm,jp)*rwgt(iprsm+128)
       enddo
       sfluxref(igc,jp) = sumf1
       rayla(igc,jp) = sumf2
     enddo
   enddo
!
   end subroutine cmbgb24
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb25
!-------------------------------------------------------------------------------
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)
!-------------------------------------------------------------------------------
!
   use rrsw_kg25_k, only : kao, sfluxrefo,                                     &
                            abso3ao, abso3bo, raylo,                           &
                            absa, ka, sfluxref,                                &
                            abso3a, abso3b, rayl
!
! ------- Local -------
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf1, sumf2, sumf3, sumf4
!-------------------------------------------------------------------------------
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
   iprsm = 0
   do igc = 1,ngc(10)
     sumf1 = 0.
     sumf2 = 0.
     sumf3 = 0.
     sumf4 = 0.
     do ipr = 1,ngn(ngs(9)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + sfluxrefo(iprsm)
       sumf2 = sumf2 + abso3ao(iprsm)*rwgt(iprsm+144)
       sumf3 = sumf3 + abso3bo(iprsm)*rwgt(iprsm+144)
       sumf4 = sumf4 + raylo(iprsm)*rwgt(iprsm+144)
     enddo
     sfluxref(igc) = sumf1
     abso3a(igc) = sumf2
     abso3b(igc) = sumf3
     rayl(igc) = sumf4
   enddo
!
   end subroutine cmbgb25
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb26
!-------------------------------------------------------------------------------
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!-------------------------------------------------------------------------------
!
   use rrsw_kg26_k, only : sfluxrefo, raylo, sfluxref, rayl
!
! ------- Local -------
!
   integer(kind=im) :: igc, ipr, iprsm
   real(kind=rb) :: sumf1, sumf2
!-------------------------------------------------------------------------------
   iprsm = 0
   do igc = 1,ngc(11)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(10)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + raylo(iprsm)*rwgt(iprsm+160)
       sumf2 = sumf2 + sfluxrefo(iprsm)
     enddo
     rayl(igc) = sumf1
     sfluxref(igc) = sumf2
   enddo
!
   end subroutine cmbgb26
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb27
!-------------------------------------------------------------------------------
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)
!-------------------------------------------------------------------------------
!
   use rrsw_kg27_k, only : kao, kbo, sfluxrefo, raylo,                         &
                            absa, ka, absb, kb, sfluxref, rayl
!
! ------- Local -------
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf1, sumf2
!-------------------------------------------------------------------------------
   do jt = 1,5
     do jp = 1,13
       iprsm = 0
       do igc = 1,ngc(12)
         sumk = 0.
         do ipr = 1,ngn(ngs(11)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+176)
         enddo
         ka(jt,jp,igc) = sumk
       enddo
     enddo
     do jp = 13,59
       iprsm = 0
       do igc = 1,ngc(12)
         sumk = 0.
         do ipr = 1,ngn(ngs(11)+igc)
           iprsm = iprsm + 1
           sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+176)
         enddo
         kb(jt,jp,igc) = sumk
       enddo
     enddo
   enddo
!
   iprsm = 0
   do igc = 1,ngc(12)
     sumf1 = 0.
     sumf2 = 0.
     do ipr = 1,ngn(ngs(11)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + sfluxrefo(iprsm)
       sumf2 = sumf2 + raylo(iprsm)*rwgt(iprsm+176)
     enddo
     sfluxref(igc) = sumf1
     rayl(igc) = sumf2
   enddo
!
   end subroutine cmbgb27
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb28
!-------------------------------------------------------------------------------
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg28_k, only : kao, kbo, sfluxrefo,                                &
                            absa, ka, absb, kb, sfluxref
!
! ------- Local -------
!
   integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf
!-------------------------------------------------------------------------------
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
   do jn = 1,5
     do jt = 1,5
       do jp = 13,59
         iprsm = 0
         do igc = 1,ngc(13)
           sumk = 0.
           do ipr = 1,ngn(ngs(12)+igc)
             iprsm = iprsm + 1
             sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+192)
           enddo
           kb(jn,jt,jp,igc) = sumk
         enddo
       enddo
     enddo
   enddo
!
   do jp = 1,5
     iprsm = 0
     do igc = 1,ngc(13)
       sumf = 0.
       do ipr = 1,ngn(ngs(12)+igc)
         iprsm = iprsm + 1
         sumf = sumf + sfluxrefo(iprsm,jp)
       enddo
       sfluxref(igc,jp) = sumf
     enddo
   enddo
!
   end subroutine cmbgb28
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine cmbgb29
!-------------------------------------------------------------------------------
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)
!-------------------------------------------------------------------------------
!
   use rrsw_kg29_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absh2oo, absco2o,                                  &
                            absa, ka, absb, kb, selfref, forref, sfluxref,     &
                            absh2o, absco2
!
! ------- Local -------
!
   integer(kind=im) :: jt, jp, igc, ipr, iprsm
   real(kind=rb) :: sumk, sumf1, sumf2, sumf3
!-------------------------------------------------------------------------------
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
     sumf3 = 0.
     do ipr = 1,ngn(ngs(13)+igc)
       iprsm = iprsm + 1
       sumf1 = sumf1 + sfluxrefo(iprsm)
       sumf2 = sumf2 + absco2o(iprsm)*rwgt(iprsm+208)
       sumf3 = sumf3 + absh2oo(iprsm)*rwgt(iprsm+208)
     enddo
     sfluxref(igc) = sumf1
     absco2(igc) = sumf2
     absh2o(igc) = sumf3
   enddo
!
   end subroutine cmbgb29
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine swcldpr
!-------------------------------------------------------------------------------
!
! abstract: 
!   Define cloud extinction coefficient, single scattering albedo
!   and asymmetry parameter data.
!
! history log :
!
!-------------------------------------------------------------------------------
!
! Explanation of the method for each value of INFLAG.  A value of
!  0 for INFLAG do not distingish being liquid and ice clouds.
!  INFLAG = 2 does distinguish between liquid and ice clouds, and
!    requires further user input to specify the method to be used to 
!    compute the aborption due to each.
!  INFLAG = 0:  For each cloudy layer, the cloud fraction, the cloud optical
!    depth, the cloud single-scattering albedo, and the
!    moments of the phase function (0:NSTREAM).  Note
!    that these values are delta-m scaled within this
!    subroutine.
!  INFLAG = 2:  For each cloudy layer, the cloud fraction, cloud 
!    water path (g/m2), and cloud ice fraction are input.
!  ICEFLAG = 2:  The ice effective radius (microns) is input and the
!    optical properties due to ice clouds are computed from
!    the optical properties stored in the RT code, STREAMER v3.0 
!    (Reference: Key. J., Streamer User's Guide, Cooperative 
!    Institute for Meteorological Satellite Studies, 2001, 96 pp.).
!    Valid range of values for re are between 5.0 and
!    131.0 micron.
!    This version uses Ebert and Curry, JGR, (1992) method for 
!    ice particles larger than 131.0 microns. 
!  ICEFLAG = 3:  The ice generalized effective size (dge) is input
!    and the optical depths, single-scattering albedo,
!    and phase function moments are calculated as in
!    Q. Fu, J. Climate, (1996). Q. Fu provided high resolution
!    tables which were appropriately averaged for the
!    bands in RRTM_SW.  Linear interpolation is used to
!    get the coefficients from the stored tables.
!    Valid range of values for dge are between 5.0 and
!    140.0 micron. 
!    This version uses Ebert and Curry, JGR, (1992) method for 
!    ice particles larger than 140.0 microns. 
!  LIQFLAG = 1:  The water droplet effective radius (microns) is input 
!    and the optical depths due to water clouds are computed 
!    as in Hu and Stamnes, J., Clim., 6, 728-742, (1993).
!    The values for absorption coefficients appropriate for
!    the spectral bands in RRTM have been obtained for a 
!    range of effective radii by an averaging procedure 
!    based on the work of J. Pinto (private communication).
!    Linear interpolation is used to get the absorption 
!    coefficients for the input effective radius.
!
!     ------------------------------------------------------------------
!
   use rrsw_cld_k, only : extliq1, ssaliq1, asyliq1,                           &
                           extice2, ssaice2, asyice2,                          &
                           extice3, ssaice3, asyice3, fdlice3,                 &
                           abari, bbari, cbari, dbari, ebari, fbari
!
   save
! Everything below is for INFLAG = 2.
!
! Coefficients for Ebert and Curry method
!
   abari(:) = (/                                                               &
         3.448e-03_rb,3.448e-03_rb,3.448e-03_rb,3.448e-03_rb,3.448e-03_rb /)
   bbari(:) = (/                                                               &
         2.431e+00_rb,2.431e+00_rb,2.431e+00_rb,2.431e+00_rb,2.431e+00_rb /)
   cbari(:) = (/                                                               &
         1.000e-05_rb,1.100e-04_rb,1.240e-02_rb,3.779e-02_rb,4.666e-01_rb /)
   dbari(:) = (/                                                               &
         0.000e+00_rb,1.405e-05_rb,6.867e-04_rb,1.284e-03_rb,2.050e-05_rb /)
   ebari(:) = (/                                                               &
         7.661e-01_rb,7.730e-01_rb,7.865e-01_rb,8.172e-01_rb,9.595e-01_rb /)
   fbari(:) = (/                                                               &
         5.851e-04_rb,5.665e-04_rb,7.204e-04_rb,7.463e-04_rb,1.076e-04_rb /)
!
! Extinction coefficient
!
   extliq1(:, 16) = (/                                                         &
         8.981463e-01_rb,6.317895e-01_rb,4.557508e-01_rb,3.481624e-01_rb,      &
         2.797950e-01_rb,2.342753e-01_rb,2.026934e-01_rb,1.800102e-01_rb,      &
         1.632408e-01_rb,1.505384e-01_rb,1.354524e-01_rb,1.246520e-01_rb,      &
         1.154342e-01_rb,1.074756e-01_rb,1.005353e-01_rb,9.442987e-02_rb,      &
         8.901760e-02_rb,8.418693e-02_rb,7.984904e-02_rb,7.593229e-02_rb,      &
         7.237827e-02_rb,6.913887e-02_rb,6.617415e-02_rb,6.345061e-02_rb,      &
         6.094001e-02_rb,5.861834e-02_rb,5.646506e-02_rb,5.446250e-02_rb,      &
         5.249596e-02_rb,5.081114e-02_rb,4.922243e-02_rb,4.772189e-02_rb,      &
         4.630243e-02_rb,4.495766e-02_rb,4.368189e-02_rb,4.246995e-02_rb,      &
         4.131720e-02_rb,4.021941e-02_rb,3.917276e-02_rb,3.817376e-02_rb,      &
         3.721926e-02_rb,3.630635e-02_rb,3.543237e-02_rb,3.459491e-02_rb,      &
         3.379171e-02_rb,3.302073e-02_rb,3.228007e-02_rb,3.156798e-02_rb,      &
         3.088284e-02_rb,3.022315e-02_rb,2.958753e-02_rb,2.897468e-02_rb,      &
         2.838340e-02_rb,2.781258e-02_rb,2.726117e-02_rb,2.672821e-02_rb,      &
         2.621278e-02_rb,2.5714e-02_rb /)
   extliq1(:, 17) = (/                                                         &
         8.293797e-01_rb,6.048371e-01_rb,4.465706e-01_rb,3.460387e-01_rb,      &
         2.800064e-01_rb,2.346584e-01_rb,2.022399e-01_rb,1.782626e-01_rb,      &
         1.600153e-01_rb,1.457903e-01_rb,1.334061e-01_rb,1.228548e-01_rb,      &
         1.138396e-01_rb,1.060486e-01_rb,9.924856e-02_rb,9.326208e-02_rb,      &
         8.795158e-02_rb,8.320883e-02_rb,7.894750e-02_rb,7.509792e-02_rb,      &
         7.160323e-02_rb,6.841653e-02_rb,6.549889e-02_rb,6.281763e-02_rb,      &
         6.034516e-02_rb,5.805802e-02_rb,5.593615e-02_rb,5.396226e-02_rb,      &
         5.202302e-02_rb,5.036246e-02_rb,4.879606e-02_rb,4.731610e-02_rb,      &
         4.591565e-02_rb,4.458852e-02_rb,4.332912e-02_rb,4.213243e-02_rb,      &
         4.099390e-02_rb,3.990941e-02_rb,3.887522e-02_rb,3.788792e-02_rb,      &
         3.694440e-02_rb,3.604183e-02_rb,3.517760e-02_rb,3.434934e-02_rb,      &
         3.355485e-02_rb,3.279211e-02_rb,3.205925e-02_rb,3.135458e-02_rb,      &
         3.067648e-02_rb,3.002349e-02_rb,2.939425e-02_rb,2.878748e-02_rb,      &
         2.820200e-02_rb,2.763673e-02_rb,2.709062e-02_rb,2.656272e-02_rb,      &
         2.605214e-02_rb,2.5558e-02_rb /)
   extliq1(:, 18) = (/                                                         &
         9.193685e-01_rb,6.128292e-01_rb,4.344150e-01_rb,3.303048e-01_rb,      &
         2.659500e-01_rb,2.239727e-01_rb,1.953457e-01_rb,1.751012e-01_rb,      &
         1.603515e-01_rb,1.493360e-01_rb,1.323791e-01_rb,1.219335e-01_rb,      &
         1.130076e-01_rb,1.052926e-01_rb,9.855839e-02_rb,9.262925e-02_rb,      &
         8.736918e-02_rb,8.267112e-02_rb,7.844965e-02_rb,7.463585e-02_rb,      &
         7.117343e-02_rb,6.801601e-02_rb,6.512503e-02_rb,6.246815e-02_rb,      &
         6.001806e-02_rb,5.775154e-02_rb,5.564872e-02_rb,5.369250e-02_rb,      &
         5.176284e-02_rb,5.011536e-02_rb,4.856099e-02_rb,4.709211e-02_rb,      &
         4.570193e-02_rb,4.438430e-02_rb,4.313375e-02_rb,4.194529e-02_rb,      &
         4.081443e-02_rb,3.973712e-02_rb,3.870966e-02_rb,3.772866e-02_rb,      &
         3.679108e-02_rb,3.589409e-02_rb,3.503514e-02_rb,3.421185e-02_rb,      &
         3.342206e-02_rb,3.266377e-02_rb,3.193513e-02_rb,3.123447e-02_rb,      &
         3.056018e-02_rb,2.991081e-02_rb,2.928502e-02_rb,2.868154e-02_rb,      &
         2.809920e-02_rb,2.753692e-02_rb,2.699367e-02_rb,2.646852e-02_rb,      &
         2.596057e-02_rb,2.5469e-02_rb /)
   extliq1(:, 19) = (/                                                         &
         9.136931e-01_rb,5.743244e-01_rb,4.080708e-01_rb,3.150572e-01_rb,      &
         2.577261e-01_rb,2.197900e-01_rb,1.933037e-01_rb,1.740212e-01_rb,      &
         1.595056e-01_rb,1.482756e-01_rb,1.312164e-01_rb,1.209246e-01_rb,      &
         1.121227e-01_rb,1.045095e-01_rb,9.785967e-02_rb,9.200149e-02_rb,      &
         8.680170e-02_rb,8.215531e-02_rb,7.797850e-02_rb,7.420361e-02_rb,      &
         7.077530e-02_rb,6.764798e-02_rb,6.478369e-02_rb,6.215063e-02_rb,      &
         5.972189e-02_rb,5.747458e-02_rb,5.538913e-02_rb,5.344866e-02_rb,      &
         5.153216e-02_rb,4.989745e-02_rb,4.835476e-02_rb,4.689661e-02_rb,      &
         4.551629e-02_rb,4.420777e-02_rb,4.296563e-02_rb,4.178497e-02_rb,      &
         4.066137e-02_rb,3.959081e-02_rb,3.856963e-02_rb,3.759452e-02_rb,      &
         3.666244e-02_rb,3.577061e-02_rb,3.491650e-02_rb,3.409777e-02_rb,      &
         3.331227e-02_rb,3.255803e-02_rb,3.183322e-02_rb,3.113617e-02_rb,      &
         3.046530e-02_rb,2.981918e-02_rb,2.919646e-02_rb,2.859591e-02_rb,      &
         2.801635e-02_rb,2.745671e-02_rb,2.691599e-02_rb,2.639324e-02_rb,      &
         2.588759e-02_rb,2.5398e-02_rb /)
   extliq1(:, 20) = (/                                                         &
         8.447548e-01_rb,5.326840e-01_rb,3.921523e-01_rb,3.119082e-01_rb,      &
         2.597055e-01_rb,2.228737e-01_rb,1.954157e-01_rb,1.741155e-01_rb,      &
         1.570881e-01_rb,1.431520e-01_rb,1.302034e-01_rb,1.200491e-01_rb,      &
         1.113571e-01_rb,1.038330e-01_rb,9.725657e-02_rb,9.145949e-02_rb,      &
         8.631112e-02_rb,8.170840e-02_rb,7.756901e-02_rb,7.382641e-02_rb,      &
         7.042616e-02_rb,6.732338e-02_rb,6.448069e-02_rb,6.186672e-02_rb,      &
         5.945494e-02_rb,5.722277e-02_rb,5.515089e-02_rb,5.322262e-02_rb,      &
         5.132153e-02_rb,4.969799e-02_rb,4.816556e-02_rb,4.671686e-02_rb,      &
         4.534525e-02_rb,4.404480e-02_rb,4.281014e-02_rb,4.163643e-02_rb,      &
         4.051930e-02_rb,3.945479e-02_rb,3.843927e-02_rb,3.746945e-02_rb,      &
         3.654234e-02_rb,3.565518e-02_rb,3.480547e-02_rb,3.399088e-02_rb,      &
         3.320930e-02_rb,3.245876e-02_rb,3.173745e-02_rb,3.104371e-02_rb,      &
         3.037600e-02_rb,2.973287e-02_rb,2.911300e-02_rb,2.851516e-02_rb,      &
         2.793818e-02_rb,2.738101e-02_rb,2.684264e-02_rb,2.632214e-02_rb,      &
         2.581863e-02_rb,2.5331e-02_rb /)
   extliq1(:, 21) = (/                                                         &
         7.727642e-01_rb,5.034865e-01_rb,3.808673e-01_rb,3.080333e-01_rb,      &
         2.586453e-01_rb,2.224989e-01_rb,1.947060e-01_rb,1.725821e-01_rb,      &
         1.545096e-01_rb,1.394456e-01_rb,1.288683e-01_rb,1.188852e-01_rb,      &
         1.103317e-01_rb,1.029214e-01_rb,9.643967e-02_rb,9.072239e-02_rb,      &
         8.564194e-02_rb,8.109758e-02_rb,7.700875e-02_rb,7.331026e-02_rb,      &
         6.994879e-02_rb,6.688028e-02_rb,6.406807e-02_rb,6.148133e-02_rb,      &
         5.909400e-02_rb,5.688388e-02_rb,5.483197e-02_rb,5.292185e-02_rb,      &
         5.103763e-02_rb,4.942905e-02_rb,4.791039e-02_rb,4.647438e-02_rb,      &
         4.511453e-02_rb,4.382497e-02_rb,4.260043e-02_rb,4.143616e-02_rb,      &
         4.032784e-02_rb,3.927155e-02_rb,3.826375e-02_rb,3.730117e-02_rb,      &
         3.638087e-02_rb,3.550013e-02_rb,3.465646e-02_rb,3.384759e-02_rb,      &
         3.307141e-02_rb,3.232598e-02_rb,3.160953e-02_rb,3.092040e-02_rb,      &
         3.025706e-02_rb,2.961810e-02_rb,2.900220e-02_rb,2.840814e-02_rb,      &
         2.783478e-02_rb,2.728106e-02_rb,2.674599e-02_rb,2.622864e-02_rb,      &
         2.572816e-02_rb,2.5244e-02_rb /)
   extliq1(:, 22) = (/                                                         &
         7.416833e-01_rb,4.959591e-01_rb,3.775057e-01_rb,3.056353e-01_rb,      &
         2.565943e-01_rb,2.206935e-01_rb,1.931479e-01_rb,1.712860e-01_rb,      &
         1.534837e-01_rb,1.386906e-01_rb,1.281198e-01_rb,1.182344e-01_rb,      &
         1.097595e-01_rb,1.024137e-01_rb,9.598552e-02_rb,9.031320e-02_rb,      &
         8.527093e-02_rb,8.075927e-02_rb,7.669869e-02_rb,7.302481e-02_rb,      &
         6.968491e-02_rb,6.663542e-02_rb,6.384008e-02_rb,6.126838e-02_rb,      &
         5.889452e-02_rb,5.669654e-02_rb,5.465558e-02_rb,5.275540e-02_rb,      &
         5.087937e-02_rb,4.927904e-02_rb,4.776796e-02_rb,4.633895e-02_rb,      &
         4.498557e-02_rb,4.370202e-02_rb,4.248306e-02_rb,4.132399e-02_rb,      &
         4.022052e-02_rb,3.916878e-02_rb,3.816523e-02_rb,3.720665e-02_rb,      &
         3.629011e-02_rb,3.541290e-02_rb,3.457257e-02_rb,3.376685e-02_rb,      &
         3.299365e-02_rb,3.225105e-02_rb,3.153728e-02_rb,3.085069e-02_rb,      &
         3.018977e-02_rb,2.955310e-02_rb,2.893940e-02_rb,2.834742e-02_rb,      &
         2.777606e-02_rb,2.722424e-02_rb,2.669099e-02_rb,2.617539e-02_rb,      &
         2.567658e-02_rb,2.5194e-02_rb /)
   extliq1(:, 23) = (/                                                         &
         7.058580e-01_rb,4.866573e-01_rb,3.712238e-01_rb,2.998638e-01_rb,      &
         2.513441e-01_rb,2.161972e-01_rb,1.895576e-01_rb,1.686669e-01_rb,      &
         1.518437e-01_rb,1.380046e-01_rb,1.267564e-01_rb,1.170399e-01_rb,      &
         1.087026e-01_rb,1.014704e-01_rb,9.513729e-02_rb,8.954555e-02_rb,      &
         8.457221e-02_rb,8.012009e-02_rb,7.611136e-02_rb,7.248294e-02_rb,      &
         6.918317e-02_rb,6.616934e-02_rb,6.340584e-02_rb,6.086273e-02_rb,      &
         5.851465e-02_rb,5.634001e-02_rb,5.432027e-02_rb,5.243946e-02_rb,      &
         5.058070e-02_rb,4.899628e-02_rb,4.749975e-02_rb,4.608411e-02_rb,      &
         4.474303e-02_rb,4.347082e-02_rb,4.226237e-02_rb,4.111303e-02_rb,      &
         4.001861e-02_rb,3.897528e-02_rb,3.797959e-02_rb,3.702835e-02_rb,      &
         3.611867e-02_rb,3.524791e-02_rb,3.441364e-02_rb,3.361360e-02_rb,      &
         3.284577e-02_rb,3.210823e-02_rb,3.139923e-02_rb,3.071716e-02_rb,      &
         3.006052e-02_rb,2.942791e-02_rb,2.881806e-02_rb,2.822974e-02_rb,      &
         2.766185e-02_rb,2.711335e-02_rb,2.658326e-02_rb,2.607066e-02_rb,      &
         2.557473e-02_rb,2.5095e-02_rb /)
   extliq1(:, 24) = (/                                                         &
         6.822779e-01_rb,4.750373e-01_rb,3.634834e-01_rb,2.940726e-01_rb,      &
         2.468060e-01_rb,2.125768e-01_rb,1.866586e-01_rb,1.663588e-01_rb,      &
         1.500326e-01_rb,1.366192e-01_rb,1.253472e-01_rb,1.158052e-01_rb,      &
         1.076101e-01_rb,1.004954e-01_rb,9.426089e-02_rb,8.875268e-02_rb,      &
         8.385090e-02_rb,7.946063e-02_rb,7.550578e-02_rb,7.192466e-02_rb,      &
         6.866669e-02_rb,6.569001e-02_rb,6.295971e-02_rb,6.044642e-02_rb,      &
         5.812526e-02_rb,5.597500e-02_rb,5.397746e-02_rb,5.211690e-02_rb,      &
         5.027505e-02_rb,4.870703e-02_rb,4.722555e-02_rb,4.582373e-02_rb,      &
         4.449540e-02_rb,4.323497e-02_rb,4.203742e-02_rb,4.089821e-02_rb,      &
         3.981321e-02_rb,3.877867e-02_rb,3.779118e-02_rb,3.684762e-02_rb,      &
         3.594514e-02_rb,3.508114e-02_rb,3.425322e-02_rb,3.345917e-02_rb,      &
         3.269698e-02_rb,3.196477e-02_rb,3.126082e-02_rb,3.058352e-02_rb,      &
         2.993141e-02_rb,2.930310e-02_rb,2.869732e-02_rb,2.811289e-02_rb,      &
         2.754869e-02_rb,2.700371e-02_rb,2.647698e-02_rb,2.596760e-02_rb,      &
         2.547473e-02_rb,2.4998e-02_rb /)
   extliq1(:, 25) = (/                                                         &
         6.666233e-01_rb,4.662044e-01_rb,3.579517e-01_rb,2.902984e-01_rb,      &
         2.440475e-01_rb,2.104431e-01_rb,1.849277e-01_rb,1.648970e-01_rb,      &
         1.487555e-01_rb,1.354714e-01_rb,1.244173e-01_rb,1.149913e-01_rb,      &
         1.068903e-01_rb,9.985323e-02_rb,9.368351e-02_rb,8.823009e-02_rb,      &
         8.337507e-02_rb,7.902511e-02_rb,7.510529e-02_rb,7.155482e-02_rb,      &
         6.832386e-02_rb,6.537113e-02_rb,6.266218e-02_rb,6.016802e-02_rb,      &
         5.786408e-02_rb,5.572939e-02_rb,5.374598e-02_rb,5.189830e-02_rb,      &
         5.006825e-02_rb,4.851081e-02_rb,4.703906e-02_rb,4.564623e-02_rb,      &
         4.432621e-02_rb,4.307349e-02_rb,4.188312e-02_rb,4.075060e-02_rb,      &
         3.967183e-02_rb,3.864313e-02_rb,3.766111e-02_rb,3.672269e-02_rb,      &
         3.582505e-02_rb,3.496559e-02_rb,3.414196e-02_rb,3.335198e-02_rb,      &
         3.259362e-02_rb,3.186505e-02_rb,3.116454e-02_rb,3.049052e-02_rb,      &
         2.984152e-02_rb,2.921617e-02_rb,2.861322e-02_rb,2.803148e-02_rb,      &
         2.746986e-02_rb,2.692733e-02_rb,2.640295e-02_rb,2.589582e-02_rb,      &
         2.540510e-02_rb,2.4930e-02_rb /)
   extliq1(:, 26) = (/                                                         &
         6.535669e-01_rb,4.585865e-01_rb,3.529226e-01_rb,2.867245e-01_rb,      &
         2.413848e-01_rb,2.083956e-01_rb,1.833191e-01_rb,1.636150e-01_rb,      &
         1.477247e-01_rb,1.346392e-01_rb,1.236449e-01_rb,1.143095e-01_rb,      &
         1.062828e-01_rb,9.930773e-02_rb,9.319029e-02_rb,8.778150e-02_rb,      &
         8.296497e-02_rb,7.864847e-02_rb,7.475799e-02_rb,7.123343e-02_rb,      &
         6.802549e-02_rb,6.509332e-02_rb,6.240285e-02_rb,5.992538e-02_rb,      &
         5.763657e-02_rb,5.551566e-02_rb,5.354483e-02_rb,5.170870e-02_rb,      &
         4.988866e-02_rb,4.834061e-02_rb,4.687751e-02_rb,4.549264e-02_rb,      &
         4.417999e-02_rb,4.293410e-02_rb,4.175006e-02_rb,4.062344e-02_rb,      &
         3.955019e-02_rb,3.852663e-02_rb,3.754943e-02_rb,3.661553e-02_rb,      &
         3.572214e-02_rb,3.486669e-02_rb,3.404683e-02_rb,3.326040e-02_rb,      &
         3.250542e-02_rb,3.178003e-02_rb,3.108254e-02_rb,3.041139e-02_rb,      &
         2.976511e-02_rb,2.914235e-02_rb,2.854187e-02_rb,2.796247e-02_rb,      &
         2.740309e-02_rb,2.686271e-02_rb,2.634038e-02_rb,2.583520e-02_rb,      &
         2.534636e-02_rb,2.4873e-02_rb /)
   extliq1(:, 27) = (/                                                         &
         6.448790e-01_rb,4.541425e-01_rb,3.503348e-01_rb,2.850494e-01_rb,      &
         2.401966e-01_rb,2.074811e-01_rb,1.825631e-01_rb,1.629515e-01_rb,      &
         1.471142e-01_rb,1.340574e-01_rb,1.231462e-01_rb,1.138628e-01_rb,      &
         1.058802e-01_rb,9.894286e-02_rb,9.285818e-02_rb,8.747802e-02_rb,      &
         8.268676e-02_rb,7.839271e-02_rb,7.452230e-02_rb,7.101580e-02_rb,      &
         6.782418e-02_rb,6.490685e-02_rb,6.222991e-02_rb,5.976484e-02_rb,      &
         5.748742e-02_rb,5.537703e-02_rb,5.341593e-02_rb,5.158883e-02_rb,      &
         4.977355e-02_rb,4.823172e-02_rb,4.677430e-02_rb,4.539465e-02_rb,      &
         4.408680e-02_rb,4.284533e-02_rb,4.166539e-02_rb,4.054257e-02_rb,      &
         3.947283e-02_rb,3.845256e-02_rb,3.747842e-02_rb,3.654737e-02_rb,      &
         3.565665e-02_rb,3.480370e-02_rb,3.398620e-02_rb,3.320198e-02_rb,      &
         3.244908e-02_rb,3.172566e-02_rb,3.103002e-02_rb,3.036062e-02_rb,      &
         2.971600e-02_rb,2.909482e-02_rb,2.849582e-02_rb,2.791785e-02_rb,      &
         2.735982e-02_rb,2.682072e-02_rb,2.629960e-02_rb,2.579559e-02_rb,      &
         2.530786e-02_rb,2.4836e-02_rb /)
   extliq1(:, 28) = (/                                                         &
         6.422688e-01_rb,4.528453e-01_rb,3.497232e-01_rb,2.847724e-01_rb,      &
         2.400815e-01_rb,2.074403e-01_rb,1.825502e-01_rb,1.629415e-01_rb,      &
         1.470934e-01_rb,1.340183e-01_rb,1.230935e-01_rb,1.138049e-01_rb,      &
         1.058201e-01_rb,9.888245e-02_rb,9.279878e-02_rb,8.742053e-02_rb,      &
         8.263175e-02_rb,7.834058e-02_rb,7.447327e-02_rb,7.097000e-02_rb,      &
         6.778167e-02_rb,6.486765e-02_rb,6.219400e-02_rb,5.973215e-02_rb,      &
         5.745790e-02_rb,5.535059e-02_rb,5.339250e-02_rb,5.156831e-02_rb,      &
         4.975308e-02_rb,4.821235e-02_rb,4.675596e-02_rb,4.537727e-02_rb,      &
         4.407030e-02_rb,4.282968e-02_rb,4.165053e-02_rb,4.052845e-02_rb,      &
         3.945941e-02_rb,3.843980e-02_rb,3.746628e-02_rb,3.653583e-02_rb,      &
         3.564567e-02_rb,3.479326e-02_rb,3.397626e-02_rb,3.319253e-02_rb,      &
         3.244008e-02_rb,3.171711e-02_rb,3.102189e-02_rb,3.035289e-02_rb,      &
         2.970866e-02_rb,2.908784e-02_rb,2.848920e-02_rb,2.791156e-02_rb,      &
         2.735385e-02_rb,2.681507e-02_rb,2.629425e-02_rb,2.579053e-02_rb,      &
         2.530308e-02_rb,2.4831e-02_rb /)
   extliq1(:, 29) = (/                                                         &
         4.614710e-01_rb,4.556116e-01_rb,4.056568e-01_rb,3.529833e-01_rb,      &
         3.060334e-01_rb,2.658127e-01_rb,2.316095e-01_rb,2.024325e-01_rb,      &
         1.773749e-01_rb,1.556867e-01_rb,1.455558e-01_rb,1.332882e-01_rb,      &
         1.229052e-01_rb,1.140067e-01_rb,1.062981e-01_rb,9.955703e-02_rb,      &
         9.361333e-02_rb,8.833420e-02_rb,8.361467e-02_rb,7.937071e-02_rb,      &
         7.553420e-02_rb,7.204942e-02_rb,6.887031e-02_rb,6.595851e-02_rb,      &
         6.328178e-02_rb,6.081286e-02_rb,5.852854e-02_rb,5.640892e-02_rb,      &
         5.431269e-02_rb,5.252561e-02_rb,5.084345e-02_rb,4.925727e-02_rb,      &
         4.775910e-02_rb,4.634182e-02_rb,4.499907e-02_rb,4.372512e-02_rb,      &
         4.251484e-02_rb,4.136357e-02_rb,4.026710e-02_rb,3.922162e-02_rb,      &
         3.822365e-02_rb,3.727004e-02_rb,3.635790e-02_rb,3.548457e-02_rb,      &
         3.464764e-02_rb,3.384488e-02_rb,3.307424e-02_rb,3.233384e-02_rb,      &
         3.162192e-02_rb,3.093688e-02_rb,3.027723e-02_rb,2.964158e-02_rb,      &
         2.902864e-02_rb,2.843722e-02_rb,2.786621e-02_rb,2.731457e-02_rb,      &
         2.678133e-02_rb,2.6266e-02_rb /)
!
! Single scattering albedo     
!
   ssaliq1(:, 16) = (/                                                         &
         8.143821e-01_rb,7.836739e-01_rb,7.550722e-01_rb,7.306269e-01_rb,      &
         7.105612e-01_rb,6.946649e-01_rb,6.825556e-01_rb,6.737762e-01_rb,      &
         6.678448e-01_rb,6.642830e-01_rb,6.679741e-01_rb,6.584607e-01_rb,      &
         6.505598e-01_rb,6.440951e-01_rb,6.388901e-01_rb,6.347689e-01_rb,      &
         6.315549e-01_rb,6.290718e-01_rb,6.271432e-01_rb,6.255928e-01_rb,      &
         6.242441e-01_rb,6.229207e-01_rb,6.214464e-01_rb,6.196445e-01_rb,      &
         6.173388e-01_rb,6.143527e-01_rb,6.105099e-01_rb,6.056339e-01_rb,      &
         6.108290e-01_rb,6.073939e-01_rb,6.043073e-01_rb,6.015473e-01_rb,      &
         5.990913e-01_rb,5.969173e-01_rb,5.950028e-01_rb,5.933257e-01_rb,      &
         5.918636e-01_rb,5.905944e-01_rb,5.894957e-01_rb,5.885453e-01_rb,      &
         5.877209e-01_rb,5.870003e-01_rb,5.863611e-01_rb,5.857811e-01_rb,      &
         5.852381e-01_rb,5.847098e-01_rb,5.841738e-01_rb,5.836081e-01_rb,      &
         5.829901e-01_rb,5.822979e-01_rb,5.815089e-01_rb,5.806011e-01_rb,      &
         5.795521e-01_rb,5.783396e-01_rb,5.769413e-01_rb,5.753351e-01_rb,      &
         5.734986e-01_rb,5.7141e-01_rb /)
   ssaliq1(:, 17) = (/                                                         &
         8.165821e-01_rb,8.002015e-01_rb,7.816921e-01_rb,7.634131e-01_rb,      &
         7.463721e-01_rb,7.312469e-01_rb,7.185883e-01_rb,7.088975e-01_rb,      &
         7.026671e-01_rb,7.004020e-01_rb,7.042138e-01_rb,6.960930e-01_rb,      &
         6.894243e-01_rb,6.840459e-01_rb,6.797957e-01_rb,6.765119e-01_rb,      &
         6.740325e-01_rb,6.721955e-01_rb,6.708391e-01_rb,6.698013e-01_rb,      &
         6.689201e-01_rb,6.680339e-01_rb,6.669805e-01_rb,6.655982e-01_rb,      &
         6.637250e-01_rb,6.611992e-01_rb,6.578588e-01_rb,6.535420e-01_rb,      &
         6.584449e-01_rb,6.553992e-01_rb,6.526547e-01_rb,6.501917e-01_rb,      &
         6.479905e-01_rb,6.460313e-01_rb,6.442945e-01_rb,6.427605e-01_rb,      &
         6.414094e-01_rb,6.402217e-01_rb,6.391775e-01_rb,6.382573e-01_rb,      &
         6.374413e-01_rb,6.367099e-01_rb,6.360433e-01_rb,6.354218e-01_rb,      &
         6.348257e-01_rb,6.342355e-01_rb,6.336313e-01_rb,6.329935e-01_rb,      &
         6.323023e-01_rb,6.315383e-01_rb,6.306814e-01_rb,6.297122e-01_rb,      &
         6.286110e-01_rb,6.273579e-01_rb,6.259333e-01_rb,6.243176e-01_rb,      &
         6.224910e-01_rb,6.2043e-01_rb /)
   ssaliq1(:, 18) = (/                                                         &
         9.900163e-01_rb,9.854307e-01_rb,9.797730e-01_rb,9.733113e-01_rb,      &
         9.664245e-01_rb,9.594976e-01_rb,9.529055e-01_rb,9.470112e-01_rb,      &
         9.421695e-01_rb,9.387304e-01_rb,9.344918e-01_rb,9.305302e-01_rb,      &
         9.267048e-01_rb,9.230072e-01_rb,9.194289e-01_rb,9.159616e-01_rb,      &
         9.125968e-01_rb,9.093260e-01_rb,9.061409e-01_rb,9.030330e-01_rb,      &
         8.999940e-01_rb,8.970154e-01_rb,8.940888e-01_rb,8.912058e-01_rb,      &
         8.883579e-01_rb,8.855368e-01_rb,8.827341e-01_rb,8.799413e-01_rb,      &
         8.777423e-01_rb,8.749566e-01_rb,8.722298e-01_rb,8.695605e-01_rb,      &
         8.669469e-01_rb,8.643875e-01_rb,8.618806e-01_rb,8.594246e-01_rb,      &
         8.570179e-01_rb,8.546589e-01_rb,8.523459e-01_rb,8.500773e-01_rb,      &
         8.478516e-01_rb,8.456670e-01_rb,8.435219e-01_rb,8.414148e-01_rb,      &
         8.393439e-01_rb,8.373078e-01_rb,8.353047e-01_rb,8.333330e-01_rb,      &
         8.313911e-01_rb,8.294774e-01_rb,8.275904e-01_rb,8.257282e-01_rb,      &
         8.238893e-01_rb,8.220721e-01_rb,8.202751e-01_rb,8.184965e-01_rb,      &
         8.167346e-01_rb,8.1499e-01_rb /)
   ssaliq1(:, 19) = (/                                                         &
         9.999916e-01_rb,9.987396e-01_rb,9.966900e-01_rb,9.950738e-01_rb,      &
         9.937531e-01_rb,9.925912e-01_rb,9.914525e-01_rb,9.902018e-01_rb,      &
         9.887046e-01_rb,9.868263e-01_rb,9.849039e-01_rb,9.832372e-01_rb,      &
         9.815265e-01_rb,9.797770e-01_rb,9.779940e-01_rb,9.761827e-01_rb,      &
         9.743481e-01_rb,9.724955e-01_rb,9.706303e-01_rb,9.687575e-01_rb,      &
         9.668823e-01_rb,9.650100e-01_rb,9.631457e-01_rb,9.612947e-01_rb,      &
         9.594622e-01_rb,9.576534e-01_rb,9.558734e-01_rb,9.541275e-01_rb,      &
         9.522059e-01_rb,9.504258e-01_rb,9.486459e-01_rb,9.468676e-01_rb,      &
         9.450921e-01_rb,9.433208e-01_rb,9.415548e-01_rb,9.397955e-01_rb,      &
         9.380441e-01_rb,9.363022e-01_rb,9.345706e-01_rb,9.328510e-01_rb,      &
         9.311445e-01_rb,9.294524e-01_rb,9.277761e-01_rb,9.261167e-01_rb,      &
         9.244755e-01_rb,9.228540e-01_rb,9.212534e-01_rb,9.196748e-01_rb,      &
         9.181197e-01_rb,9.165894e-01_rb,9.150851e-01_rb,9.136080e-01_rb,      &
         9.121596e-01_rb,9.107410e-01_rb,9.093536e-01_rb,9.079987e-01_rb,      &
         9.066775e-01_rb,9.0539e-01_rb /)
   ssaliq1(:, 20) = (/                                                         &
         9.979493e-01_rb,9.964113e-01_rb,9.950014e-01_rb,9.937045e-01_rb,      &
         9.924964e-01_rb,9.913546e-01_rb,9.902575e-01_rb,9.891843e-01_rb,      &
         9.881136e-01_rb,9.870238e-01_rb,9.859934e-01_rb,9.849372e-01_rb,      &
         9.838873e-01_rb,9.828434e-01_rb,9.818052e-01_rb,9.807725e-01_rb,      &
         9.797450e-01_rb,9.787225e-01_rb,9.777047e-01_rb,9.766914e-01_rb,      &
         9.756823e-01_rb,9.746771e-01_rb,9.736756e-01_rb,9.726775e-01_rb,      &
         9.716827e-01_rb,9.706907e-01_rb,9.697014e-01_rb,9.687145e-01_rb,      &
         9.678060e-01_rb,9.668108e-01_rb,9.658218e-01_rb,9.648391e-01_rb,      &
         9.638629e-01_rb,9.628936e-01_rb,9.619313e-01_rb,9.609763e-01_rb,      &
         9.600287e-01_rb,9.590888e-01_rb,9.581569e-01_rb,9.572330e-01_rb,      &
         9.563176e-01_rb,9.554108e-01_rb,9.545128e-01_rb,9.536239e-01_rb,      &
         9.527443e-01_rb,9.518741e-01_rb,9.510137e-01_rb,9.501633e-01_rb,      &
         9.493230e-01_rb,9.484931e-01_rb,9.476740e-01_rb,9.468656e-01_rb,      &
         9.460683e-01_rb,9.452824e-01_rb,9.445080e-01_rb,9.437454e-01_rb,      &
         9.429948e-01_rb,9.4226e-01_rb /)
   ssaliq1(:, 21) = (/                                                         &
         9.988742e-01_rb,9.982668e-01_rb,9.976935e-01_rb,9.971497e-01_rb,      &
         9.966314e-01_rb,9.961344e-01_rb,9.956545e-01_rb,9.951873e-01_rb,      &
         9.947286e-01_rb,9.942741e-01_rb,9.938457e-01_rb,9.933947e-01_rb,      &
         9.929473e-01_rb,9.925032e-01_rb,9.920621e-01_rb,9.916237e-01_rb,      &
         9.911875e-01_rb,9.907534e-01_rb,9.903209e-01_rb,9.898898e-01_rb,      &
         9.894597e-01_rb,9.890304e-01_rb,9.886015e-01_rb,9.881726e-01_rb,      &
         9.877435e-01_rb,9.873138e-01_rb,9.868833e-01_rb,9.864516e-01_rb,      &
         9.860698e-01_rb,9.856317e-01_rb,9.851957e-01_rb,9.847618e-01_rb,      &
         9.843302e-01_rb,9.839008e-01_rb,9.834739e-01_rb,9.830494e-01_rb,      &
         9.826275e-01_rb,9.822083e-01_rb,9.817918e-01_rb,9.813782e-01_rb,      &
         9.809675e-01_rb,9.805598e-01_rb,9.801552e-01_rb,9.797538e-01_rb,      &
         9.793556e-01_rb,9.789608e-01_rb,9.785695e-01_rb,9.781817e-01_rb,      &
         9.777975e-01_rb,9.774171e-01_rb,9.770404e-01_rb,9.766676e-01_rb,      &
         9.762988e-01_rb,9.759340e-01_rb,9.755733e-01_rb,9.752169e-01_rb,      &
         9.748649e-01_rb,9.7452e-01_rb /)
   ssaliq1(:, 22) = (/                                                         &
         9.994441e-01_rb,9.991608e-01_rb,9.988949e-01_rb,9.986439e-01_rb,      &
         9.984054e-01_rb,9.981768e-01_rb,9.979557e-01_rb,9.977396e-01_rb,      &
         9.975258e-01_rb,9.973120e-01_rb,9.971011e-01_rb,9.968852e-01_rb,      &
         9.966708e-01_rb,9.964578e-01_rb,9.962462e-01_rb,9.960357e-01_rb,      &
         9.958264e-01_rb,9.956181e-01_rb,9.954108e-01_rb,9.952043e-01_rb,      &
         9.949987e-01_rb,9.947937e-01_rb,9.945892e-01_rb,9.943853e-01_rb,      &
         9.941818e-01_rb,9.939786e-01_rb,9.937757e-01_rb,9.935728e-01_rb,      &
         9.933922e-01_rb,9.931825e-01_rb,9.929739e-01_rb,9.927661e-01_rb,      &
         9.925592e-01_rb,9.923534e-01_rb,9.921485e-01_rb,9.919447e-01_rb,      &
         9.917421e-01_rb,9.915406e-01_rb,9.913403e-01_rb,9.911412e-01_rb,      &
         9.909435e-01_rb,9.907470e-01_rb,9.905519e-01_rb,9.903581e-01_rb,      &
         9.901659e-01_rb,9.899751e-01_rb,9.897858e-01_rb,9.895981e-01_rb,      &
         9.894120e-01_rb,9.892276e-01_rb,9.890447e-01_rb,9.888637e-01_rb,      &
         9.886845e-01_rb,9.885070e-01_rb,9.883314e-01_rb,9.881576e-01_rb,      &
         9.879859e-01_rb,9.8782e-01_rb /)
   ssaliq1(:, 23) = (/                                                         &
         9.999138e-01_rb,9.998730e-01_rb,9.998338e-01_rb,9.997965e-01_rb,      &
         9.997609e-01_rb,9.997270e-01_rb,9.996944e-01_rb,9.996629e-01_rb,      &
         9.996321e-01_rb,9.996016e-01_rb,9.995690e-01_rb,9.995372e-01_rb,      &
         9.995057e-01_rb,9.994744e-01_rb,9.994433e-01_rb,9.994124e-01_rb,      &
         9.993817e-01_rb,9.993510e-01_rb,9.993206e-01_rb,9.992903e-01_rb,      &
         9.992600e-01_rb,9.992299e-01_rb,9.991998e-01_rb,9.991698e-01_rb,      &
         9.991398e-01_rb,9.991098e-01_rb,9.990799e-01_rb,9.990499e-01_rb,      &
         9.990231e-01_rb,9.989920e-01_rb,9.989611e-01_rb,9.989302e-01_rb,      &
         9.988996e-01_rb,9.988690e-01_rb,9.988386e-01_rb,9.988084e-01_rb,      &
         9.987783e-01_rb,9.987485e-01_rb,9.987187e-01_rb,9.986891e-01_rb,      &
         9.986598e-01_rb,9.986306e-01_rb,9.986017e-01_rb,9.985729e-01_rb,      &
         9.985443e-01_rb,9.985160e-01_rb,9.984879e-01_rb,9.984600e-01_rb,      &
         9.984324e-01_rb,9.984050e-01_rb,9.983778e-01_rb,9.983509e-01_rb,      &
         9.983243e-01_rb,9.982980e-01_rb,9.982719e-01_rb,9.982461e-01_rb,      &
         9.982206e-01_rb,9.9820e-01_rb /)
   ssaliq1(:, 24) = (/                                                         &
         9.999985e-01_rb,9.999979e-01_rb,9.999972e-01_rb,9.999966e-01_rb,      &
         9.999961e-01_rb,9.999955e-01_rb,9.999950e-01_rb,9.999944e-01_rb,      &
         9.999938e-01_rb,9.999933e-01_rb,9.999927e-01_rb,9.999921e-01_rb,      &
         9.999915e-01_rb,9.999910e-01_rb,9.999904e-01_rb,9.999899e-01_rb,      &
         9.999893e-01_rb,9.999888e-01_rb,9.999882e-01_rb,9.999877e-01_rb,      &
         9.999871e-01_rb,9.999866e-01_rb,9.999861e-01_rb,9.999855e-01_rb,      &
         9.999850e-01_rb,9.999844e-01_rb,9.999839e-01_rb,9.999833e-01_rb,      &
         9.999828e-01_rb,9.999823e-01_rb,9.999817e-01_rb,9.999812e-01_rb,      &
         9.999807e-01_rb,9.999801e-01_rb,9.999796e-01_rb,9.999791e-01_rb,      &
         9.999786e-01_rb,9.999781e-01_rb,9.999776e-01_rb,9.999770e-01_rb,      &
         9.999765e-01_rb,9.999761e-01_rb,9.999756e-01_rb,9.999751e-01_rb,      &
         9.999746e-01_rb,9.999741e-01_rb,9.999736e-01_rb,9.999732e-01_rb,      &
         9.999727e-01_rb,9.999722e-01_rb,9.999718e-01_rb,9.999713e-01_rb,      &
         9.999709e-01_rb,9.999705e-01_rb,9.999701e-01_rb,9.999697e-01_rb,      &
         9.999692e-01_rb,9.9997e-01_rb /)
   ssaliq1(:, 25) = (/                                                         &
         9.999999e-01_rb,9.999998e-01_rb,9.999997e-01_rb,9.999997e-01_rb,      &
         9.999997e-01_rb,9.999996e-01_rb,9.999996e-01_rb,9.999995e-01_rb,      &
         9.999995e-01_rb,9.999994e-01_rb,9.999994e-01_rb,9.999993e-01_rb,      &
         9.999993e-01_rb,9.999992e-01_rb,9.999992e-01_rb,9.999991e-01_rb,      &
         9.999991e-01_rb,9.999991e-01_rb,9.999990e-01_rb,9.999989e-01_rb,      &
         9.999989e-01_rb,9.999989e-01_rb,9.999988e-01_rb,9.999988e-01_rb,      &
         9.999987e-01_rb,9.999987e-01_rb,9.999986e-01_rb,9.999986e-01_rb,      &
         9.999985e-01_rb,9.999985e-01_rb,9.999984e-01_rb,9.999984e-01_rb,      &
         9.999984e-01_rb,9.999983e-01_rb,9.999983e-01_rb,9.999982e-01_rb,      &
         9.999982e-01_rb,9.999982e-01_rb,9.999981e-01_rb,9.999980e-01_rb,      &
         9.999980e-01_rb,9.999980e-01_rb,9.999979e-01_rb,9.999979e-01_rb,      &
         9.999978e-01_rb,9.999978e-01_rb,9.999977e-01_rb,9.999977e-01_rb,      &
         9.999977e-01_rb,9.999976e-01_rb,9.999976e-01_rb,9.999975e-01_rb,      &
         9.999975e-01_rb,9.999974e-01_rb,9.999974e-01_rb,9.999974e-01_rb,      &
         9.999973e-01_rb,1.0000e+00_rb /)
   ssaliq1(:, 26) = (/                                                         &
         9.999997e-01_rb,9.999995e-01_rb,9.999993e-01_rb,9.999992e-01_rb,      &
         9.999990e-01_rb,9.999989e-01_rb,9.999988e-01_rb,9.999987e-01_rb,      &
         9.999986e-01_rb,9.999985e-01_rb,9.999984e-01_rb,9.999983e-01_rb,      &
         9.999982e-01_rb,9.999981e-01_rb,9.999980e-01_rb,9.999978e-01_rb,      &
         9.999977e-01_rb,9.999976e-01_rb,9.999975e-01_rb,9.999974e-01_rb,      &
         9.999973e-01_rb,9.999972e-01_rb,9.999970e-01_rb,9.999969e-01_rb,      &
         9.999968e-01_rb,9.999967e-01_rb,9.999966e-01_rb,9.999965e-01_rb,      &
         9.999964e-01_rb,9.999963e-01_rb,9.999962e-01_rb,9.999961e-01_rb,      &
         9.999959e-01_rb,9.999958e-01_rb,9.999957e-01_rb,9.999956e-01_rb,      &
         9.999955e-01_rb,9.999954e-01_rb,9.999953e-01_rb,9.999952e-01_rb,      &
         9.999951e-01_rb,9.999949e-01_rb,9.999949e-01_rb,9.999947e-01_rb,      &
         9.999946e-01_rb,9.999945e-01_rb,9.999944e-01_rb,9.999943e-01_rb,      &
         9.999942e-01_rb,9.999941e-01_rb,9.999940e-01_rb,9.999939e-01_rb,      &
         9.999938e-01_rb,9.999937e-01_rb,9.999936e-01_rb,9.999935e-01_rb,      &
         9.999934e-01_rb,9.9999e-01_rb /)
   ssaliq1(:, 27) = (/                                                         &
         9.999984e-01_rb,9.999976e-01_rb,9.999969e-01_rb,9.999962e-01_rb,      &
         9.999956e-01_rb,9.999950e-01_rb,9.999945e-01_rb,9.999940e-01_rb,      &
         9.999935e-01_rb,9.999931e-01_rb,9.999926e-01_rb,9.999920e-01_rb,      &
         9.999914e-01_rb,9.999908e-01_rb,9.999903e-01_rb,9.999897e-01_rb,      &
         9.999891e-01_rb,9.999886e-01_rb,9.999880e-01_rb,9.999874e-01_rb,      &
         9.999868e-01_rb,9.999863e-01_rb,9.999857e-01_rb,9.999851e-01_rb,      &
         9.999846e-01_rb,9.999840e-01_rb,9.999835e-01_rb,9.999829e-01_rb,      &
         9.999824e-01_rb,9.999818e-01_rb,9.999812e-01_rb,9.999806e-01_rb,      &
         9.999800e-01_rb,9.999795e-01_rb,9.999789e-01_rb,9.999783e-01_rb,      &
         9.999778e-01_rb,9.999773e-01_rb,9.999767e-01_rb,9.999761e-01_rb,      &
         9.999756e-01_rb,9.999750e-01_rb,9.999745e-01_rb,9.999739e-01_rb,      &
         9.999734e-01_rb,9.999729e-01_rb,9.999723e-01_rb,9.999718e-01_rb,      &
         9.999713e-01_rb,9.999708e-01_rb,9.999703e-01_rb,9.999697e-01_rb,      &
         9.999692e-01_rb,9.999687e-01_rb,9.999683e-01_rb,9.999678e-01_rb,      &
         9.999673e-01_rb,9.9997e-01_rb /)
   ssaliq1(:, 28) = (/                                                         &
         9.999981e-01_rb,9.999973e-01_rb,9.999965e-01_rb,9.999958e-01_rb,      &
         9.999951e-01_rb,9.999943e-01_rb,9.999937e-01_rb,9.999930e-01_rb,      &
         9.999924e-01_rb,9.999918e-01_rb,9.999912e-01_rb,9.999905e-01_rb,      &
         9.999897e-01_rb,9.999890e-01_rb,9.999883e-01_rb,9.999876e-01_rb,      &
         9.999869e-01_rb,9.999862e-01_rb,9.999855e-01_rb,9.999847e-01_rb,      &
         9.999840e-01_rb,9.999834e-01_rb,9.999827e-01_rb,9.999819e-01_rb,      &
         9.999812e-01_rb,9.999805e-01_rb,9.999799e-01_rb,9.999791e-01_rb,      &
         9.999785e-01_rb,9.999778e-01_rb,9.999771e-01_rb,9.999764e-01_rb,      &
         9.999757e-01_rb,9.999750e-01_rb,9.999743e-01_rb,9.999736e-01_rb,      &
         9.999729e-01_rb,9.999722e-01_rb,9.999715e-01_rb,9.999709e-01_rb,      &
         9.999701e-01_rb,9.999695e-01_rb,9.999688e-01_rb,9.999682e-01_rb,      &
         9.999675e-01_rb,9.999669e-01_rb,9.999662e-01_rb,9.999655e-01_rb,      &
         9.999649e-01_rb,9.999642e-01_rb,9.999636e-01_rb,9.999630e-01_rb,      &
         9.999624e-01_rb,9.999618e-01_rb,9.999612e-01_rb,9.999606e-01_rb,      &
         9.999600e-01_rb,9.9996e-01_rb /)
   ssaliq1(:, 29) = (/                                                         &
         8.505737e-01_rb,8.465102e-01_rb,8.394829e-01_rb,8.279508e-01_rb,      &
         8.110806e-01_rb,7.900397e-01_rb,7.669615e-01_rb,7.444422e-01_rb,      &
         7.253055e-01_rb,7.124831e-01_rb,7.016434e-01_rb,6.885485e-01_rb,      &
         6.767340e-01_rb,6.661029e-01_rb,6.565577e-01_rb,6.480013e-01_rb,      &
         6.403373e-01_rb,6.334697e-01_rb,6.273034e-01_rb,6.217440e-01_rb,      &
         6.166983e-01_rb,6.120740e-01_rb,6.077796e-01_rb,6.037249e-01_rb,      &
         5.998207e-01_rb,5.959788e-01_rb,5.921123e-01_rb,5.881354e-01_rb,      &
         5.891285e-01_rb,5.851143e-01_rb,5.814653e-01_rb,5.781606e-01_rb,      &
         5.751792e-01_rb,5.724998e-01_rb,5.701016e-01_rb,5.679634e-01_rb,      &
         5.660642e-01_rb,5.643829e-01_rb,5.628984e-01_rb,5.615898e-01_rb,      &
         5.604359e-01_rb,5.594158e-01_rb,5.585083e-01_rb,5.576924e-01_rb,      &
         5.569470e-01_rb,5.562512e-01_rb,5.555838e-01_rb,5.549239e-01_rb,      &
         5.542503e-01_rb,5.535420e-01_rb,5.527781e-01_rb,5.519374e-01_rb,      &
         5.509989e-01_rb,5.499417e-01_rb,5.487445e-01_rb,5.473865e-01_rb,      &
         5.458466e-01_rb,5.4410e-01_rb /)
!
! asymmetry parameter
!
   asyliq1(:, 16) = (/                                                         &
         8.133297e-01_rb,8.133528e-01_rb,8.173865e-01_rb,8.243205e-01_rb,      &
         8.333063e-01_rb,8.436317e-01_rb,8.546611e-01_rb,8.657934e-01_rb,      &
         8.764345e-01_rb,8.859837e-01_rb,8.627394e-01_rb,8.824569e-01_rb,      &
         8.976887e-01_rb,9.089541e-01_rb,9.167699e-01_rb,9.216517e-01_rb,      &
         9.241147e-01_rb,9.246743e-01_rb,9.238469e-01_rb,9.221504e-01_rb,      &
         9.201045e-01_rb,9.182299e-01_rb,9.170491e-01_rb,9.170862e-01_rb,      &
         9.188653e-01_rb,9.229111e-01_rb,9.297468e-01_rb,9.398950e-01_rb,      &
         9.203269e-01_rb,9.260693e-01_rb,9.309373e-01_rb,9.349918e-01_rb,      &
         9.382935e-01_rb,9.409030e-01_rb,9.428809e-01_rb,9.442881e-01_rb,      &
         9.451851e-01_rb,9.456331e-01_rb,9.456926e-01_rb,9.454247e-01_rb,      &
         9.448902e-01_rb,9.441503e-01_rb,9.432661e-01_rb,9.422987e-01_rb,      &
         9.413094e-01_rb,9.403594e-01_rb,9.395102e-01_rb,9.388230e-01_rb,      &
         9.383594e-01_rb,9.381810e-01_rb,9.383489e-01_rb,9.389251e-01_rb,      &
         9.399707e-01_rb,9.415475e-01_rb,9.437167e-01_rb,9.465399e-01_rb,      &
         9.500786e-01_rb,9.5439e-01_rb /)
   asyliq1(:, 17) = (/                                                         &
         8.794448e-01_rb,8.819306e-01_rb,8.837667e-01_rb,8.853832e-01_rb,      &
         8.871010e-01_rb,8.892675e-01_rb,8.922584e-01_rb,8.964666e-01_rb,      &
         9.022940e-01_rb,9.101456e-01_rb,8.839999e-01_rb,9.035610e-01_rb,      &
         9.184568e-01_rb,9.292315e-01_rb,9.364282e-01_rb,9.405887e-01_rb,      &
         9.422554e-01_rb,9.419703e-01_rb,9.402759e-01_rb,9.377159e-01_rb,      &
         9.348345e-01_rb,9.321769e-01_rb,9.302888e-01_rb,9.297166e-01_rb,      &
         9.310075e-01_rb,9.347080e-01_rb,9.413643e-01_rb,9.515216e-01_rb,      &
         9.306286e-01_rb,9.361781e-01_rb,9.408374e-01_rb,9.446692e-01_rb,      &
         9.477363e-01_rb,9.501013e-01_rb,9.518268e-01_rb,9.529756e-01_rb,      &
         9.536105e-01_rb,9.537938e-01_rb,9.535886e-01_rb,9.530574e-01_rb,      &
         9.522633e-01_rb,9.512688e-01_rb,9.501370e-01_rb,9.489306e-01_rb,      &
         9.477126e-01_rb,9.465459e-01_rb,9.454934e-01_rb,9.446183e-01_rb,      &
         9.439833e-01_rb,9.436519e-01_rb,9.436866e-01_rb,9.441508e-01_rb,      &
         9.451073e-01_rb,9.466195e-01_rb,9.487501e-01_rb,9.515621e-01_rb,      &
         9.551185e-01_rb,9.5948e-01_rb /)
   asyliq1(:, 18) = (/                                                         &
         8.478817e-01_rb,8.269312e-01_rb,8.161352e-01_rb,8.135960e-01_rb,      &
         8.173586e-01_rb,8.254167e-01_rb,8.357072e-01_rb,8.461167e-01_rb,      &
         8.544952e-01_rb,8.586776e-01_rb,8.335562e-01_rb,8.524273e-01_rb,      &
         8.669052e-01_rb,8.775014e-01_rb,8.847277e-01_rb,8.890958e-01_rb,      &
         8.911173e-01_rb,8.913038e-01_rb,8.901669e-01_rb,8.882182e-01_rb,      &
         8.859692e-01_rb,8.839315e-01_rb,8.826164e-01_rb,8.825356e-01_rb,      &
         8.842004e-01_rb,8.881223e-01_rb,8.948131e-01_rb,9.047837e-01_rb,      &
         8.855951e-01_rb,8.911796e-01_rb,8.959229e-01_rb,8.998837e-01_rb,      &
         9.031209e-01_rb,9.056939e-01_rb,9.076609e-01_rb,9.090812e-01_rb,      &
         9.100134e-01_rb,9.105167e-01_rb,9.106496e-01_rb,9.104712e-01_rb,      &
         9.100404e-01_rb,9.094159e-01_rb,9.086568e-01_rb,9.078218e-01_rb,      &
         9.069697e-01_rb,9.061595e-01_rb,9.054499e-01_rb,9.048999e-01_rb,      &
         9.045683e-01_rb,9.045142e-01_rb,9.047962e-01_rb,9.054730e-01_rb,      &
         9.066037e-01_rb,9.082472e-01_rb,9.104623e-01_rb,9.133079e-01_rb,      &
         9.168427e-01_rb,9.2113e-01_rb /)
   asyliq1(:, 19) = (/                                                         &
         8.216697e-01_rb,7.982871e-01_rb,7.891147e-01_rb,7.909083e-01_rb,      &
         8.003833e-01_rb,8.142516e-01_rb,8.292290e-01_rb,8.420356e-01_rb,      &
         8.493945e-01_rb,8.480316e-01_rb,8.212381e-01_rb,8.394984e-01_rb,      &
         8.534095e-01_rb,8.634813e-01_rb,8.702242e-01_rb,8.741483e-01_rb,      &
         8.757638e-01_rb,8.755808e-01_rb,8.741095e-01_rb,8.718604e-01_rb,      &
         8.693433e-01_rb,8.670686e-01_rb,8.655464e-01_rb,8.652872e-01_rb,      &
         8.668006e-01_rb,8.705973e-01_rb,8.771874e-01_rb,8.870809e-01_rb,      &
         8.678284e-01_rb,8.732315e-01_rb,8.778084e-01_rb,8.816166e-01_rb,      &
         8.847146e-01_rb,8.871603e-01_rb,8.890116e-01_rb,8.903266e-01_rb,      &
         8.911632e-01_rb,8.915796e-01_rb,8.916337e-01_rb,8.913834e-01_rb,      &
         8.908869e-01_rb,8.902022e-01_rb,8.893873e-01_rb,8.885001e-01_rb,      &
         8.875986e-01_rb,8.867411e-01_rb,8.859852e-01_rb,8.853891e-01_rb,      &
         8.850111e-01_rb,8.849089e-01_rb,8.851405e-01_rb,8.857639e-01_rb,      &
         8.868372e-01_rb,8.884185e-01_rb,8.905656e-01_rb,8.933368e-01_rb,      &
         8.967899e-01_rb,9.0098e-01_rb /)
   asyliq1(:, 20) = (/                                                         &
         8.063610e-01_rb,7.938147e-01_rb,7.921304e-01_rb,7.985092e-01_rb,      &
         8.101339e-01_rb,8.242175e-01_rb,8.379913e-01_rb,8.486920e-01_rb,      &
         8.535547e-01_rb,8.498083e-01_rb,8.224849e-01_rb,8.405509e-01_rb,      &
         8.542436e-01_rb,8.640770e-01_rb,8.705653e-01_rb,8.742227e-01_rb,      &
         8.755630e-01_rb,8.751004e-01_rb,8.733491e-01_rb,8.708231e-01_rb,      &
         8.680365e-01_rb,8.655035e-01_rb,8.637381e-01_rb,8.632544e-01_rb,      &
         8.645665e-01_rb,8.681885e-01_rb,8.746346e-01_rb,8.844188e-01_rb,      &
         8.648180e-01_rb,8.700563e-01_rb,8.744672e-01_rb,8.781087e-01_rb,      &
         8.810393e-01_rb,8.833174e-01_rb,8.850011e-01_rb,8.861485e-01_rb,      &
         8.868183e-01_rb,8.870687e-01_rb,8.869579e-01_rb,8.865441e-01_rb,      &
         8.858857e-01_rb,8.850412e-01_rb,8.840686e-01_rb,8.830263e-01_rb,      &
         8.819726e-01_rb,8.809658e-01_rb,8.800642e-01_rb,8.793260e-01_rb,      &
         8.788099e-01_rb,8.785737e-01_rb,8.786758e-01_rb,8.791746e-01_rb,      &
         8.801283e-01_rb,8.815955e-01_rb,8.836340e-01_rb,8.863024e-01_rb,      &
         8.896592e-01_rb,8.9376e-01_rb /)
   asyliq1(:, 21) = (/                                                         &
         7.885899e-01_rb,7.937172e-01_rb,8.020658e-01_rb,8.123971e-01_rb,      &
         8.235502e-01_rb,8.343776e-01_rb,8.437336e-01_rb,8.504711e-01_rb,      &
         8.534421e-01_rb,8.514978e-01_rb,8.238888e-01_rb,8.417463e-01_rb,      &
         8.552057e-01_rb,8.647853e-01_rb,8.710038e-01_rb,8.743798e-01_rb,      &
         8.754319e-01_rb,8.746786e-01_rb,8.726386e-01_rb,8.698303e-01_rb,      &
         8.667724e-01_rb,8.639836e-01_rb,8.619823e-01_rb,8.612870e-01_rb,      &
         8.624165e-01_rb,8.658893e-01_rb,8.722241e-01_rb,8.819394e-01_rb,      &
         8.620216e-01_rb,8.671239e-01_rb,8.713983e-01_rb,8.749032e-01_rb,      &
         8.776970e-01_rb,8.798385e-01_rb,8.813860e-01_rb,8.823980e-01_rb,      &
         8.829332e-01_rb,8.830500e-01_rb,8.828068e-01_rb,8.822623e-01_rb,      &
         8.814750e-01_rb,8.805031e-01_rb,8.794056e-01_rb,8.782407e-01_rb,      &
         8.770672e-01_rb,8.759432e-01_rb,8.749275e-01_rb,8.740784e-01_rb,      &
         8.734547e-01_rb,8.731146e-01_rb,8.731170e-01_rb,8.735199e-01_rb,      &
         8.743823e-01_rb,8.757625e-01_rb,8.777191e-01_rb,8.803105e-01_rb,      &
         8.835953e-01_rb,8.8763e-01_rb /)
   asyliq1(:, 22) = (/                                                         &
         7.811516e-01_rb,7.962229e-01_rb,8.096199e-01_rb,8.212996e-01_rb,      &
         8.312212e-01_rb,8.393430e-01_rb,8.456236e-01_rb,8.500214e-01_rb,      &
         8.524950e-01_rb,8.530031e-01_rb,8.251485e-01_rb,8.429043e-01_rb,      &
         8.562461e-01_rb,8.656954e-01_rb,8.717737e-01_rb,8.750020e-01_rb,      &
         8.759022e-01_rb,8.749953e-01_rb,8.728027e-01_rb,8.698461e-01_rb,      &
         8.666466e-01_rb,8.637257e-01_rb,8.616047e-01_rb,8.608051e-01_rb,      &
         8.618483e-01_rb,8.652557e-01_rb,8.715487e-01_rb,8.812485e-01_rb,      &
         8.611645e-01_rb,8.662052e-01_rb,8.704173e-01_rb,8.738594e-01_rb,      &
         8.765901e-01_rb,8.786678e-01_rb,8.801517e-01_rb,8.810999e-01_rb,      &
         8.815713e-01_rb,8.816246e-01_rb,8.813185e-01_rb,8.807114e-01_rb,      &
         8.798621e-01_rb,8.788290e-01_rb,8.776713e-01_rb,8.764470e-01_rb,      &
         8.752152e-01_rb,8.740343e-01_rb,8.729631e-01_rb,8.720602e-01_rb,      &
         8.713842e-01_rb,8.709936e-01_rb,8.709475e-01_rb,8.713041e-01_rb,      &
         8.721221e-01_rb,8.734602e-01_rb,8.753774e-01_rb,8.779319e-01_rb,      &
         8.811825e-01_rb,8.8519e-01_rb /)
   asyliq1(:, 23) = (/                                                         &
         7.865744e-01_rb,8.093340e-01_rb,8.257596e-01_rb,8.369940e-01_rb,      &
         8.441574e-01_rb,8.483602e-01_rb,8.507096e-01_rb,8.523139e-01_rb,      &
         8.542834e-01_rb,8.577321e-01_rb,8.288960e-01_rb,8.465308e-01_rb,      &
         8.597175e-01_rb,8.689830e-01_rb,8.748542e-01_rb,8.778584e-01_rb,      &
         8.785222e-01_rb,8.773728e-01_rb,8.749370e-01_rb,8.717419e-01_rb,      &
         8.683145e-01_rb,8.651816e-01_rb,8.628704e-01_rb,8.619077e-01_rb,      &
         8.628205e-01_rb,8.661356e-01_rb,8.723803e-01_rb,8.820815e-01_rb,      &
         8.616715e-01_rb,8.666389e-01_rb,8.707753e-01_rb,8.741398e-01_rb,      &
         8.767912e-01_rb,8.787885e-01_rb,8.801908e-01_rb,8.810570e-01_rb,      &
         8.814460e-01_rb,8.814167e-01_rb,8.810283e-01_rb,8.803395e-01_rb,      &
         8.794095e-01_rb,8.782971e-01_rb,8.770613e-01_rb,8.757610e-01_rb,      &
         8.744553e-01_rb,8.732031e-01_rb,8.720634e-01_rb,8.710951e-01_rb,      &
         8.703572e-01_rb,8.699086e-01_rb,8.698084e-01_rb,8.701155e-01_rb,      &
         8.708887e-01_rb,8.721872e-01_rb,8.740698e-01_rb,8.765957e-01_rb,      &
         8.798235e-01_rb,8.8381e-01_rb /)
   asyliq1(:, 24) = (/                                                         &
         8.069513e-01_rb,8.262939e-01_rb,8.398241e-01_rb,8.486352e-01_rb,      &
         8.538213e-01_rb,8.564743e-01_rb,8.576854e-01_rb,8.585455e-01_rb,      &
         8.601452e-01_rb,8.635755e-01_rb,8.337383e-01_rb,8.512655e-01_rb,      &
         8.643049e-01_rb,8.733896e-01_rb,8.790535e-01_rb,8.818295e-01_rb,      &
         8.822518e-01_rb,8.808533e-01_rb,8.781676e-01_rb,8.747284e-01_rb,      &
         8.710690e-01_rb,8.677229e-01_rb,8.652236e-01_rb,8.641047e-01_rb,      &
         8.648993e-01_rb,8.681413e-01_rb,8.743640e-01_rb,8.841007e-01_rb,      &
         8.633558e-01_rb,8.682719e-01_rb,8.723543e-01_rb,8.756621e-01_rb,      &
         8.782547e-01_rb,8.801915e-01_rb,8.815318e-01_rb,8.823347e-01_rb,      &
         8.826598e-01_rb,8.825663e-01_rb,8.821135e-01_rb,8.813608e-01_rb,      &
         8.803674e-01_rb,8.791928e-01_rb,8.778960e-01_rb,8.765366e-01_rb,      &
         8.751738e-01_rb,8.738670e-01_rb,8.726755e-01_rb,8.716585e-01_rb,      &
         8.708755e-01_rb,8.703856e-01_rb,8.702483e-01_rb,8.705229e-01_rb,      &
         8.712687e-01_rb,8.725448e-01_rb,8.744109e-01_rb,8.769260e-01_rb,      &
         8.801496e-01_rb,8.8414e-01_rb /)
   asyliq1(:, 25) = (/                                                         &
         8.252182e-01_rb,8.379244e-01_rb,8.471709e-01_rb,8.535760e-01_rb,      &
         8.577540e-01_rb,8.603183e-01_rb,8.618820e-01_rb,8.630578e-01_rb,      &
         8.644587e-01_rb,8.666970e-01_rb,8.362159e-01_rb,8.536817e-01_rb,      &
         8.666387e-01_rb,8.756240e-01_rb,8.811746e-01_rb,8.838273e-01_rb,      &
         8.841191e-01_rb,8.825871e-01_rb,8.797681e-01_rb,8.761992e-01_rb,      &
         8.724174e-01_rb,8.689593e-01_rb,8.663623e-01_rb,8.651632e-01_rb,      &
         8.658988e-01_rb,8.691064e-01_rb,8.753226e-01_rb,8.850847e-01_rb,      &
         8.641620e-01_rb,8.690500e-01_rb,8.731026e-01_rb,8.763795e-01_rb,      &
         8.789400e-01_rb,8.808438e-01_rb,8.821503e-01_rb,8.829191e-01_rb,      &
         8.832095e-01_rb,8.830813e-01_rb,8.825938e-01_rb,8.818064e-01_rb,      &
         8.807787e-01_rb,8.795704e-01_rb,8.782408e-01_rb,8.768493e-01_rb,      &
         8.754557e-01_rb,8.741193e-01_rb,8.728995e-01_rb,8.718561e-01_rb,      &
         8.710484e-01_rb,8.705360e-01_rb,8.703782e-01_rb,8.706347e-01_rb,      &
         8.713650e-01_rb,8.726285e-01_rb,8.744849e-01_rb,8.769933e-01_rb,      &
         8.802136e-01_rb,8.8421e-01_rb /)
   asyliq1(:, 26) = (/                                                         &
         8.370583e-01_rb,8.467920e-01_rb,8.537769e-01_rb,8.585136e-01_rb,      &
         8.615034e-01_rb,8.632474e-01_rb,8.642468e-01_rb,8.650026e-01_rb,      &
         8.660161e-01_rb,8.677882e-01_rb,8.369760e-01_rb,8.543821e-01_rb,      &
         8.672699e-01_rb,8.761782e-01_rb,8.816454e-01_rb,8.842103e-01_rb,      &
         8.844114e-01_rb,8.827872e-01_rb,8.798766e-01_rb,8.762179e-01_rb,      &
         8.723500e-01_rb,8.688112e-01_rb,8.661403e-01_rb,8.648758e-01_rb,      &
         8.655563e-01_rb,8.687206e-01_rb,8.749072e-01_rb,8.846546e-01_rb,      &
         8.636289e-01_rb,8.684849e-01_rb,8.725054e-01_rb,8.757501e-01_rb,      &
         8.782785e-01_rb,8.801503e-01_rb,8.814249e-01_rb,8.821620e-01_rb,      &
         8.824211e-01_rb,8.822620e-01_rb,8.817440e-01_rb,8.809268e-01_rb,      &
         8.798699e-01_rb,8.786330e-01_rb,8.772756e-01_rb,8.758572e-01_rb,      &
         8.744374e-01_rb,8.730760e-01_rb,8.718323e-01_rb,8.707660e-01_rb,      &
         8.699366e-01_rb,8.694039e-01_rb,8.692271e-01_rb,8.694661e-01_rb,      &
         8.701803e-01_rb,8.714293e-01_rb,8.732727e-01_rb,8.757702e-01_rb,      &
         8.789811e-01_rb,8.8297e-01_rb /)
   asyliq1(:, 27) = (/                                                         &
         8.430819e-01_rb,8.510060e-01_rb,8.567270e-01_rb,8.606533e-01_rb,      &
         8.631934e-01_rb,8.647554e-01_rb,8.657471e-01_rb,8.665760e-01_rb,      &
         8.676496e-01_rb,8.693754e-01_rb,8.384298e-01_rb,8.557913e-01_rb,      &
         8.686214e-01_rb,8.774605e-01_rb,8.828495e-01_rb,8.853287e-01_rb,      &
         8.854393e-01_rb,8.837215e-01_rb,8.807161e-01_rb,8.769639e-01_rb,      &
         8.730053e-01_rb,8.693812e-01_rb,8.666321e-01_rb,8.652988e-01_rb,      &
         8.659219e-01_rb,8.690419e-01_rb,8.751999e-01_rb,8.849360e-01_rb,      &
         8.638013e-01_rb,8.686371e-01_rb,8.726369e-01_rb,8.758605e-01_rb,      &
         8.783674e-01_rb,8.802176e-01_rb,8.814705e-01_rb,8.821859e-01_rb,      &
         8.824234e-01_rb,8.822429e-01_rb,8.817038e-01_rb,8.808658e-01_rb,      &
         8.797887e-01_rb,8.785323e-01_rb,8.771560e-01_rb,8.757196e-01_rb,      &
         8.742828e-01_rb,8.729052e-01_rb,8.716467e-01_rb,8.705666e-01_rb,      &
         8.697250e-01_rb,8.691812e-01_rb,8.689950e-01_rb,8.692264e-01_rb,      &
         8.699346e-01_rb,8.711795e-01_rb,8.730209e-01_rb,8.755181e-01_rb,      &
         8.787312e-01_rb,8.8272e-01_rb /)
   asyliq1(:, 28) = (/                                                         &
         8.452284e-01_rb,8.522700e-01_rb,8.572973e-01_rb,8.607031e-01_rb,      &
         8.628802e-01_rb,8.642215e-01_rb,8.651198e-01_rb,8.659679e-01_rb,      &
         8.671588e-01_rb,8.690853e-01_rb,8.383803e-01_rb,8.557485e-01_rb,      &
         8.685851e-01_rb,8.774303e-01_rb,8.828245e-01_rb,8.853077e-01_rb,      &
         8.854207e-01_rb,8.837034e-01_rb,8.806962e-01_rb,8.769398e-01_rb,      &
         8.729740e-01_rb,8.693393e-01_rb,8.665761e-01_rb,8.652247e-01_rb,      &
         8.658253e-01_rb,8.689182e-01_rb,8.750438e-01_rb,8.847424e-01_rb,      &
         8.636140e-01_rb,8.684449e-01_rb,8.724400e-01_rb,8.756589e-01_rb,      &
         8.781613e-01_rb,8.800072e-01_rb,8.812559e-01_rb,8.819671e-01_rb,      &
         8.822007e-01_rb,8.820165e-01_rb,8.814737e-01_rb,8.806322e-01_rb,      &
         8.795518e-01_rb,8.782923e-01_rb,8.769129e-01_rb,8.754737e-01_rb,      &
         8.740342e-01_rb,8.726542e-01_rb,8.713934e-01_rb,8.703111e-01_rb,      &
         8.694677e-01_rb,8.689222e-01_rb,8.687344e-01_rb,8.689646e-01_rb,      &
         8.696715e-01_rb,8.709156e-01_rb,8.727563e-01_rb,8.752531e-01_rb,      &
         8.784659e-01_rb,8.8245e-01_rb /)
   asyliq1(:, 29) = (/                                                         &
         7.800869e-01_rb,8.091120e-01_rb,8.325369e-01_rb,8.466266e-01_rb,      &
         8.515495e-01_rb,8.499371e-01_rb,8.456203e-01_rb,8.430521e-01_rb,      &
         8.470286e-01_rb,8.625431e-01_rb,8.402261e-01_rb,8.610822e-01_rb,      &
         8.776608e-01_rb,8.904485e-01_rb,8.999294e-01_rb,9.065860e-01_rb,      &
         9.108995e-01_rb,9.133503e-01_rb,9.144187e-01_rb,9.145855e-01_rb,      &
         9.143320e-01_rb,9.141402e-01_rb,9.144933e-01_rb,9.158754e-01_rb,      &
         9.187716e-01_rb,9.236677e-01_rb,9.310503e-01_rb,9.414058e-01_rb,      &
         9.239108e-01_rb,9.300719e-01_rb,9.353612e-01_rb,9.398378e-01_rb,      &
         9.435609e-01_rb,9.465895e-01_rb,9.489829e-01_rb,9.508000e-01_rb,      &
         9.521002e-01_rb,9.529424e-01_rb,9.533860e-01_rb,9.534902e-01_rb,      &
         9.533143e-01_rb,9.529177e-01_rb,9.523596e-01_rb,9.516997e-01_rb,      &
         9.509973e-01_rb,9.503121e-01_rb,9.497037e-01_rb,9.492317e-01_rb,      &
         9.489558e-01_rb,9.489356e-01_rb,9.492311e-01_rb,9.499019e-01_rb,      &
         9.510077e-01_rb,9.526084e-01_rb,9.547636e-01_rb,9.575331e-01_rb,      &
         9.609766e-01_rb,9.6515e-01_rb /)
!
! Spherical Ice Particle Parameterization
! extinction units (ext coef/iwc): [(m^-1)/(g m^-3)]
!
   extice2(:, 16) = (/                                                         &
! band 16
         4.101824e-01_rb,2.435514e-01_rb,1.713697e-01_rb,1.314865e-01_rb,      &
         1.063406e-01_rb,8.910701e-02_rb,7.659480e-02_rb,6.711784e-02_rb,      &
         5.970353e-02_rb,5.375249e-02_rb,4.887577e-02_rb,4.481025e-02_rb,      &
         4.137171e-02_rb,3.842744e-02_rb,3.587948e-02_rb,3.365396e-02_rb,      &
         3.169419e-02_rb,2.995593e-02_rb,2.840419e-02_rb,2.701091e-02_rb,      &
         2.575336e-02_rb,2.461293e-02_rb,2.357423e-02_rb,2.262443e-02_rb,      &
         2.175276e-02_rb,2.095012e-02_rb,2.020875e-02_rb,1.952199e-02_rb,      &
         1.888412e-02_rb,1.829018e-02_rb,1.773586e-02_rb,1.721738e-02_rb,      &
         1.673144e-02_rb,1.627510e-02_rb,1.584579e-02_rb,1.544122e-02_rb,      &
         1.505934e-02_rb,1.469833e-02_rb,1.435654e-02_rb,1.403251e-02_rb,      &
         1.372492e-02_rb,1.343255e-02_rb,1.315433e-02_rb /)
   extice2(:, 17) = (/                                                         &
! band 17
         3.836650e-01_rb,2.304055e-01_rb,1.637265e-01_rb,1.266681e-01_rb,      &
         1.031602e-01_rb,8.695191e-02_rb,7.511544e-02_rb,6.610009e-02_rb,      &
         5.900909e-02_rb,5.328833e-02_rb,4.857728e-02_rb,4.463133e-02_rb,      &
         4.127880e-02_rb,3.839567e-02_rb,3.589013e-02_rb,3.369280e-02_rb,      &
         3.175027e-02_rb,3.002079e-02_rb,2.847121e-02_rb,2.707493e-02_rb,      &
         2.581031e-02_rb,2.465962e-02_rb,2.360815e-02_rb,2.264363e-02_rb,      &
         2.175571e-02_rb,2.093563e-02_rb,2.017592e-02_rb,1.947015e-02_rb,      &
         1.881278e-02_rb,1.819901e-02_rb,1.762463e-02_rb,1.708598e-02_rb,      &
         1.657982e-02_rb,1.610330e-02_rb,1.565390e-02_rb,1.522937e-02_rb,      &
         1.482768e-02_rb,1.444706e-02_rb,1.408588e-02_rb,1.374270e-02_rb,      &
         1.341619e-02_rb,1.310517e-02_rb,1.280857e-02_rb /)
   extice2(:, 18) = (/                                                         &
! band 18
         4.152673e-01_rb,2.436816e-01_rb,1.702243e-01_rb,1.299704e-01_rb,      &
         1.047528e-01_rb,8.756039e-02_rb,7.513327e-02_rb,6.575690e-02_rb,      &
         5.844616e-02_rb,5.259609e-02_rb,4.781531e-02_rb,4.383980e-02_rb,      &
         4.048517e-02_rb,3.761891e-02_rb,3.514342e-02_rb,3.298525e-02_rb,      &
         3.108814e-02_rb,2.940825e-02_rb,2.791096e-02_rb,2.656858e-02_rb,      &
         2.535869e-02_rb,2.426297e-02_rb,2.326627e-02_rb,2.235602e-02_rb,      &
         2.152164e-02_rb,2.075420e-02_rb,2.004613e-02_rb,1.939091e-02_rb,      &
         1.878296e-02_rb,1.821744e-02_rb,1.769015e-02_rb,1.719741e-02_rb,      &
         1.673600e-02_rb,1.630308e-02_rb,1.589615e-02_rb,1.551298e-02_rb,      &
         1.515159e-02_rb,1.481021e-02_rb,1.448726e-02_rb,1.418131e-02_rb,      &
         1.389109e-02_rb,1.361544e-02_rb,1.335330e-02_rb /)
   extice2(:, 19) = (/                                                         &
! band 19
         3.873250e-01_rb,2.331609e-01_rb,1.655002e-01_rb,1.277753e-01_rb,      &
         1.038247e-01_rb,8.731780e-02_rb,7.527638e-02_rb,6.611873e-02_rb,      &
         5.892850e-02_rb,5.313885e-02_rb,4.838068e-02_rb,4.440356e-02_rb,      &
         4.103167e-02_rb,3.813804e-02_rb,3.562870e-02_rb,3.343269e-02_rb,      &
         3.149539e-02_rb,2.977414e-02_rb,2.823510e-02_rb,2.685112e-02_rb,      &
         2.560015e-02_rb,2.446411e-02_rb,2.342805e-02_rb,2.247948e-02_rb,      &
         2.160789e-02_rb,2.080438e-02_rb,2.006139e-02_rb,1.937238e-02_rb,      &
         1.873177e-02_rb,1.813469e-02_rb,1.757689e-02_rb,1.705468e-02_rb,      &
         1.656479e-02_rb,1.610435e-02_rb,1.567081e-02_rb,1.526192e-02_rb,      &
         1.487565e-02_rb,1.451020e-02_rb,1.416396e-02_rb,1.383546e-02_rb,      &
         1.352339e-02_rb,1.322657e-02_rb,1.294392e-02_rb /)
   extice2(:, 20) = (/                                                         &
! band 20
         3.784280e-01_rb,2.291396e-01_rb,1.632551e-01_rb,1.263775e-01_rb,      &
         1.028944e-01_rb,8.666975e-02_rb,7.480952e-02_rb,6.577335e-02_rb,      &
         5.866714e-02_rb,5.293694e-02_rb,4.822153e-02_rb,4.427547e-02_rb,      &
         4.092626e-02_rb,3.804918e-02_rb,3.555184e-02_rb,3.336440e-02_rb,      &
         3.143307e-02_rb,2.971577e-02_rb,2.817912e-02_rb,2.679632e-02_rb,      &
         2.554558e-02_rb,2.440903e-02_rb,2.337187e-02_rb,2.242173e-02_rb,      &
         2.154821e-02_rb,2.074249e-02_rb,1.999706e-02_rb,1.930546e-02_rb,      &
         1.866212e-02_rb,1.806221e-02_rb,1.750152e-02_rb,1.697637e-02_rb,      &
         1.648352e-02_rb,1.602010e-02_rb,1.558358e-02_rb,1.517172e-02_rb,      &
         1.478250e-02_rb,1.441413e-02_rb,1.406498e-02_rb,1.373362e-02_rb,      &
         1.341872e-02_rb,1.311911e-02_rb,1.283371e-02_rb /)
   extice2(:, 21) = (/                                                         &
! band 21
         3.719909e-01_rb,2.259490e-01_rb,1.613144e-01_rb,1.250648e-01_rb,      &
         1.019462e-01_rb,8.595358e-02_rb,7.425064e-02_rb,6.532618e-02_rb,      &
         5.830218e-02_rb,5.263421e-02_rb,4.796697e-02_rb,4.405891e-02_rb,      &
         4.074013e-02_rb,3.788776e-02_rb,3.541071e-02_rb,3.324008e-02_rb,      &
         3.132280e-02_rb,2.961733e-02_rb,2.809071e-02_rb,2.671645e-02_rb,      &
         2.547302e-02_rb,2.434276e-02_rb,2.331102e-02_rb,2.236558e-02_rb,      &
         2.149614e-02_rb,2.069397e-02_rb,1.995163e-02_rb,1.926272e-02_rb,      &
         1.862174e-02_rb,1.802389e-02_rb,1.746500e-02_rb,1.694142e-02_rb,      &
         1.644994e-02_rb,1.598772e-02_rb,1.555225e-02_rb,1.514129e-02_rb,      &
         1.475286e-02_rb,1.438515e-02_rb,1.403659e-02_rb,1.370572e-02_rb,      &
         1.339124e-02_rb,1.309197e-02_rb,1.280685e-02_rb /)
   extice2(:, 22) = (/                                                         &
! band 22
         3.713158e-01_rb,2.253816e-01_rb,1.608461e-01_rb,1.246718e-01_rb,      &
         1.016109e-01_rb,8.566332e-02_rb,7.399666e-02_rb,6.510199e-02_rb,      &
         5.810290e-02_rb,5.245608e-02_rb,4.780702e-02_rb,4.391478e-02_rb,      &
         4.060989e-02_rb,3.776982e-02_rb,3.530374e-02_rb,3.314296e-02_rb,      &
         3.123458e-02_rb,2.953719e-02_rb,2.801794e-02_rb,2.665043e-02_rb,      &
         2.541321e-02_rb,2.428868e-02_rb,2.326224e-02_rb,2.232173e-02_rb,      &
         2.145688e-02_rb,2.065899e-02_rb,1.992067e-02_rb,1.923552e-02_rb,      &
         1.859808e-02_rb,1.800356e-02_rb,1.744782e-02_rb,1.692721e-02_rb,      &
         1.643855e-02_rb,1.597900e-02_rb,1.554606e-02_rb,1.513751e-02_rb,      &
         1.475137e-02_rb,1.438586e-02_rb,1.403938e-02_rb,1.371050e-02_rb,      &
         1.339793e-02_rb,1.310050e-02_rb,1.281713e-02_rb /)
   extice2(:, 23) = (/                                                         &
! band 23
         3.605883e-01_rb,2.204388e-01_rb,1.580431e-01_rb,1.229033e-01_rb,      &
         1.004203e-01_rb,8.482616e-02_rb,7.338941e-02_rb,6.465105e-02_rb,      &
         5.776176e-02_rb,5.219398e-02_rb,4.760288e-02_rb,4.375369e-02_rb,      &
         4.048111e-02_rb,3.766539e-02_rb,3.521771e-02_rb,3.307079e-02_rb,      &
         3.117277e-02_rb,2.948303e-02_rb,2.796929e-02_rb,2.660560e-02_rb,      &
         2.537086e-02_rb,2.424772e-02_rb,2.322182e-02_rb,2.228114e-02_rb,      &
         2.141556e-02_rb,2.061649e-02_rb,1.987661e-02_rb,1.918962e-02_rb,      &
         1.855009e-02_rb,1.795330e-02_rb,1.739514e-02_rb,1.687199e-02_rb,      &
         1.638069e-02_rb,1.591845e-02_rb,1.548276e-02_rb,1.507143e-02_rb,      &
         1.468249e-02_rb,1.431416e-02_rb,1.396486e-02_rb,1.363318e-02_rb,      &
         1.331781e-02_rb,1.301759e-02_rb,1.273147e-02_rb /)
   extice2(:, 24) = (/                                                         &
! band 24
         3.527890e-01_rb,2.168469e-01_rb,1.560090e-01_rb,1.216216e-01_rb,      &
         9.955787e-02_rb,8.421942e-02_rb,7.294827e-02_rb,6.432192e-02_rb,      &
         5.751081e-02_rb,5.199888e-02_rb,4.744835e-02_rb,4.362899e-02_rb,      &
         4.037847e-02_rb,3.757910e-02_rb,3.514351e-02_rb,3.300546e-02_rb,      &
         3.111382e-02_rb,2.942853e-02_rb,2.791775e-02_rb,2.655584e-02_rb,      &
         2.532195e-02_rb,2.419892e-02_rb,2.317255e-02_rb,2.223092e-02_rb,      &
         2.136402e-02_rb,2.056334e-02_rb,1.982160e-02_rb,1.913258e-02_rb,      &
         1.849087e-02_rb,1.789178e-02_rb,1.733124e-02_rb,1.680565e-02_rb,      &
         1.631187e-02_rb,1.584711e-02_rb,1.540889e-02_rb,1.499502e-02_rb,      &
         1.460354e-02_rb,1.423269e-02_rb,1.388088e-02_rb,1.354670e-02_rb,      &
         1.322887e-02_rb,1.292620e-02_rb,1.263767e-02_rb /)
   extice2(:, 25) = (/                                                         &
! band 25
         3.477874e-01_rb,2.143515e-01_rb,1.544887e-01_rb,1.205942e-01_rb,      &
         9.881779e-02_rb,8.366261e-02_rb,7.251586e-02_rb,6.397790e-02_rb,      &
         5.723183e-02_rb,5.176908e-02_rb,4.725658e-02_rb,4.346715e-02_rb,      &
         4.024055e-02_rb,3.746055e-02_rb,3.504080e-02_rb,3.291583e-02_rb,      &
         3.103507e-02_rb,2.935891e-02_rb,2.785582e-02_rb,2.650042e-02_rb,      &
         2.527206e-02_rb,2.415376e-02_rb,2.313142e-02_rb,2.219326e-02_rb,      &
         2.132934e-02_rb,2.053122e-02_rb,1.979169e-02_rb,1.910456e-02_rb,      &
         1.846448e-02_rb,1.786680e-02_rb,1.730745e-02_rb,1.678289e-02_rb,      &
         1.628998e-02_rb,1.582595e-02_rb,1.538835e-02_rb,1.497499e-02_rb,      &
         1.458393e-02_rb,1.421341e-02_rb,1.386187e-02_rb,1.352788e-02_rb,      &
         1.321019e-02_rb,1.290762e-02_rb,1.261913e-02_rb /)
   extice2(:, 26) = (/                                                         &
! band 26
         3.453721e-01_rb,2.130744e-01_rb,1.536698e-01_rb,1.200140e-01_rb,      &
         9.838078e-02_rb,8.331940e-02_rb,7.223803e-02_rb,6.374775e-02_rb,      &
         5.703770e-02_rb,5.160290e-02_rb,4.711259e-02_rb,4.334110e-02_rb,      &
         4.012923e-02_rb,3.736150e-02_rb,3.495208e-02_rb,3.283589e-02_rb,      &
         3.096267e-02_rb,2.929302e-02_rb,2.779560e-02_rb,2.644517e-02_rb,      &
         2.522119e-02_rb,2.410677e-02_rb,2.308788e-02_rb,2.215281e-02_rb,      &
         2.129165e-02_rb,2.049602e-02_rb,1.975874e-02_rb,1.907365e-02_rb,      &
         1.843542e-02_rb,1.783943e-02_rb,1.728162e-02_rb,1.675847e-02_rb,      &
         1.626685e-02_rb,1.580401e-02_rb,1.536750e-02_rb,1.495515e-02_rb,      &
         1.456502e-02_rb,1.419537e-02_rb,1.384463e-02_rb,1.351139e-02_rb,      &
         1.319438e-02_rb,1.289246e-02_rb,1.260456e-02_rb /)
   extice2(:, 27) = (/                                                         &
! band 27
         3.417883e-01_rb,2.113379e-01_rb,1.526395e-01_rb,1.193347e-01_rb,      &
         9.790253e-02_rb,8.296715e-02_rb,7.196979e-02_rb,6.353806e-02_rb,      &
         5.687024e-02_rb,5.146670e-02_rb,4.700001e-02_rb,4.324667e-02_rb,      &
         4.004894e-02_rb,3.729233e-02_rb,3.489172e-02_rb,3.278257e-02_rb,      &
         3.091499e-02_rb,2.924987e-02_rb,2.775609e-02_rb,2.640859e-02_rb,      &
         2.518695e-02_rb,2.407439e-02_rb,2.305697e-02_rb,2.212303e-02_rb,      &
         2.126273e-02_rb,2.046774e-02_rb,1.973090e-02_rb,1.904610e-02_rb,      &
         1.840801e-02_rb,1.781204e-02_rb,1.725417e-02_rb,1.673086e-02_rb,      &
         1.623902e-02_rb,1.577590e-02_rb,1.533906e-02_rb,1.492634e-02_rb,      &
         1.453580e-02_rb,1.416571e-02_rb,1.381450e-02_rb,1.348078e-02_rb,      &
         1.316327e-02_rb,1.286082e-02_rb,1.257240e-02_rb /)
   extice2(:, 28) = (/                                                         &
! band 28
         3.416111e-01_rb,2.114124e-01_rb,1.527734e-01_rb,1.194809e-01_rb,      &
         9.804612e-02_rb,8.310287e-02_rb,7.209595e-02_rb,6.365442e-02_rb,      &
         5.697710e-02_rb,5.156460e-02_rb,4.708957e-02_rb,4.332850e-02_rb,      &
         4.012361e-02_rb,3.736037e-02_rb,3.495364e-02_rb,3.283879e-02_rb,      &
         3.096593e-02_rb,2.929589e-02_rb,2.779751e-02_rb,2.644571e-02_rb,      &
         2.522004e-02_rb,2.410369e-02_rb,2.308271e-02_rb,2.214542e-02_rb,      &
         2.128195e-02_rb,2.048396e-02_rb,1.974429e-02_rb,1.905679e-02_rb,      &
         1.841614e-02_rb,1.781774e-02_rb,1.725754e-02_rb,1.673203e-02_rb,      &
         1.623807e-02_rb,1.577293e-02_rb,1.533416e-02_rb,1.491958e-02_rb,      &
         1.452727e-02_rb,1.415547e-02_rb,1.380262e-02_rb,1.346732e-02_rb,      &
         1.314830e-02_rb,1.284439e-02_rb,1.255456e-02_rb /)
   extice2(:, 29) = (/                                                         &
! band 29
         4.196611e-01_rb,2.493642e-01_rb,1.761261e-01_rb,1.357197e-01_rb,      &
         1.102161e-01_rb,9.269376e-02_rb,7.992985e-02_rb,7.022538e-02_rb,      &
         6.260168e-02_rb,5.645603e-02_rb,5.139732e-02_rb,4.716088e-02_rb,      &
         4.356133e-02_rb,4.046498e-02_rb,3.777303e-02_rb,3.541094e-02_rb,      &
         3.332137e-02_rb,3.145954e-02_rb,2.978998e-02_rb,2.828419e-02_rb,      &
         2.691905e-02_rb,2.567559e-02_rb,2.453811e-02_rb,2.349350e-02_rb,      &
         2.253072e-02_rb,2.164042e-02_rb,2.081464e-02_rb,2.004652e-02_rb,      &
         1.933015e-02_rb,1.866041e-02_rb,1.803283e-02_rb,1.744348e-02_rb,      &
         1.688894e-02_rb,1.636616e-02_rb,1.587244e-02_rb,1.540539e-02_rb,      &
         1.496287e-02_rb,1.454295e-02_rb,1.414392e-02_rb,1.376423e-02_rb,      &
         1.340247e-02_rb,1.305739e-02_rb,1.272784e-02_rb /)
!
! single-scattering albedo: unitless
!
   ssaice2(:, 16) = (/                                                         &
! band 16
         6.630615e-01_rb,6.451169e-01_rb,6.333696e-01_rb,6.246927e-01_rb,      &
         6.178420e-01_rb,6.121976e-01_rb,6.074069e-01_rb,6.032505e-01_rb,      &
         5.995830e-01_rb,5.963030e-01_rb,5.933372e-01_rb,5.906311e-01_rb,      &
         5.881427e-01_rb,5.858395e-01_rb,5.836955e-01_rb,5.816896e-01_rb,      &
         5.798046e-01_rb,5.780264e-01_rb,5.763429e-01_rb,5.747441e-01_rb,      &
         5.732213e-01_rb,5.717672e-01_rb,5.703754e-01_rb,5.690403e-01_rb,      &
         5.677571e-01_rb,5.665215e-01_rb,5.653297e-01_rb,5.641782e-01_rb,      &
         5.630643e-01_rb,5.619850e-01_rb,5.609381e-01_rb,5.599214e-01_rb,      &
         5.589328e-01_rb,5.579707e-01_rb,5.570333e-01_rb,5.561193e-01_rb,      &
         5.552272e-01_rb,5.543558e-01_rb,5.535041e-01_rb,5.526708e-01_rb,      &
         5.518551e-01_rb,5.510561e-01_rb,5.502729e-01_rb /)
   ssaice2(:, 17) = (/                                                         &
! band 17
         7.689749e-01_rb,7.398171e-01_rb,7.205819e-01_rb,7.065690e-01_rb,      &
         6.956928e-01_rb,6.868989e-01_rb,6.795813e-01_rb,6.733606e-01_rb,      &
         6.679838e-01_rb,6.632742e-01_rb,6.591036e-01_rb,6.553766e-01_rb,      &
         6.520197e-01_rb,6.489757e-01_rb,6.461991e-01_rb,6.436531e-01_rb,      &
         6.413075e-01_rb,6.391375e-01_rb,6.371221e-01_rb,6.352438e-01_rb,      &
         6.334876e-01_rb,6.318406e-01_rb,6.302918e-01_rb,6.288315e-01_rb,      &
         6.274512e-01_rb,6.261436e-01_rb,6.249022e-01_rb,6.237211e-01_rb,      &
         6.225953e-01_rb,6.215201e-01_rb,6.204914e-01_rb,6.195055e-01_rb,      &
         6.185592e-01_rb,6.176492e-01_rb,6.167730e-01_rb,6.159280e-01_rb,      &
         6.151120e-01_rb,6.143228e-01_rb,6.135587e-01_rb,6.128177e-01_rb,      &
         6.120984e-01_rb,6.113993e-01_rb,6.107189e-01_rb /)
   ssaice2(:, 18) = (/                                                         &
! band 18
         9.956167e-01_rb,9.814770e-01_rb,9.716104e-01_rb,9.639746e-01_rb,      &
         9.577179e-01_rb,9.524010e-01_rb,9.477672e-01_rb,9.436527e-01_rb,      &
         9.399467e-01_rb,9.365708e-01_rb,9.334672e-01_rb,9.305921e-01_rb,      &
         9.279118e-01_rb,9.253993e-01_rb,9.230330e-01_rb,9.207954e-01_rb,      &
         9.186719e-01_rb,9.166501e-01_rb,9.147199e-01_rb,9.128722e-01_rb,      &
         9.110997e-01_rb,9.093956e-01_rb,9.077544e-01_rb,9.061708e-01_rb,      &
         9.046406e-01_rb,9.031598e-01_rb,9.017248e-01_rb,9.003326e-01_rb,      &
         8.989804e-01_rb,8.976655e-01_rb,8.963857e-01_rb,8.951389e-01_rb,      &
         8.939233e-01_rb,8.927370e-01_rb,8.915785e-01_rb,8.904464e-01_rb,      &
         8.893392e-01_rb,8.882559e-01_rb,8.871951e-01_rb,8.861559e-01_rb,      &
         8.851373e-01_rb,8.841383e-01_rb,8.831581e-01_rb /)
   ssaice2(:, 19) = (/                                                         &
! band 19
         9.723177e-01_rb,9.452119e-01_rb,9.267592e-01_rb,9.127393e-01_rb,      &
         9.014238e-01_rb,8.919334e-01_rb,8.837584e-01_rb,8.765773e-01_rb,      &
         8.701736e-01_rb,8.643950e-01_rb,8.591299e-01_rb,8.542942e-01_rb,      &
         8.498230e-01_rb,8.456651e-01_rb,8.417794e-01_rb,8.381324e-01_rb,      &
         8.346964e-01_rb,8.314484e-01_rb,8.283687e-01_rb,8.254408e-01_rb,      &
         8.226505e-01_rb,8.199854e-01_rb,8.174348e-01_rb,8.149891e-01_rb,      &
         8.126403e-01_rb,8.103808e-01_rb,8.082041e-01_rb,8.061044e-01_rb,      &
         8.040765e-01_rb,8.021156e-01_rb,8.002174e-01_rb,7.983781e-01_rb,      &
         7.965941e-01_rb,7.948622e-01_rb,7.931795e-01_rb,7.915432e-01_rb,      &
         7.899508e-01_rb,7.884002e-01_rb,7.868891e-01_rb,7.854156e-01_rb,      &
         7.839779e-01_rb,7.825742e-01_rb,7.812031e-01_rb /)
   ssaice2(:, 20) = (/                                                         &
! band 20
         9.933294e-01_rb,9.860917e-01_rb,9.811564e-01_rb,9.774008e-01_rb,      &
         9.743652e-01_rb,9.718155e-01_rb,9.696159e-01_rb,9.676810e-01_rb,      &
         9.659531e-01_rb,9.643915e-01_rb,9.629667e-01_rb,9.616561e-01_rb,      &
         9.604426e-01_rb,9.593125e-01_rb,9.582548e-01_rb,9.572607e-01_rb,      &
         9.563227e-01_rb,9.554347e-01_rb,9.545915e-01_rb,9.537888e-01_rb,      &
         9.530226e-01_rb,9.522898e-01_rb,9.515874e-01_rb,9.509130e-01_rb,      &
         9.502643e-01_rb,9.496394e-01_rb,9.490366e-01_rb,9.484542e-01_rb,      &
         9.478910e-01_rb,9.473456e-01_rb,9.468169e-01_rb,9.463039e-01_rb,      &
         9.458056e-01_rb,9.453212e-01_rb,9.448499e-01_rb,9.443910e-01_rb,      &
         9.439438e-01_rb,9.435077e-01_rb,9.430821e-01_rb,9.426666e-01_rb,      &
         9.422607e-01_rb,9.418638e-01_rb,9.414756e-01_rb /)
   ssaice2(:, 21) = (/                                                         &
! band 21
         9.900787e-01_rb,9.828880e-01_rb,9.779258e-01_rb,9.741173e-01_rb,      &
         9.710184e-01_rb,9.684012e-01_rb,9.661332e-01_rb,9.641301e-01_rb,      &
         9.623352e-01_rb,9.607083e-01_rb,9.592198e-01_rb,9.578474e-01_rb,      &
         9.565739e-01_rb,9.553856e-01_rb,9.542715e-01_rb,9.532226e-01_rb,      &
         9.522314e-01_rb,9.512919e-01_rb,9.503986e-01_rb,9.495472e-01_rb,      &
         9.487337e-01_rb,9.479549e-01_rb,9.472077e-01_rb,9.464897e-01_rb,      &
         9.457985e-01_rb,9.451322e-01_rb,9.444890e-01_rb,9.438673e-01_rb,      &
         9.432656e-01_rb,9.426826e-01_rb,9.421173e-01_rb,9.415684e-01_rb,      &
         9.410351e-01_rb,9.405164e-01_rb,9.400115e-01_rb,9.395198e-01_rb,      &
         9.390404e-01_rb,9.385728e-01_rb,9.381164e-01_rb,9.376707e-01_rb,      &
         9.372350e-01_rb,9.368091e-01_rb,9.363923e-01_rb /)
   ssaice2(:, 22) = (/                                                         &
! band 22
         9.986793e-01_rb,9.985239e-01_rb,9.983911e-01_rb,9.982715e-01_rb,      &
         9.981606e-01_rb,9.980562e-01_rb,9.979567e-01_rb,9.978613e-01_rb,      &
         9.977691e-01_rb,9.976798e-01_rb,9.975929e-01_rb,9.975081e-01_rb,      &
         9.974251e-01_rb,9.973438e-01_rb,9.972640e-01_rb,9.971855e-01_rb,      &
         9.971083e-01_rb,9.970322e-01_rb,9.969571e-01_rb,9.968830e-01_rb,      &
         9.968099e-01_rb,9.967375e-01_rb,9.966660e-01_rb,9.965951e-01_rb,      &
         9.965250e-01_rb,9.964555e-01_rb,9.963867e-01_rb,9.963185e-01_rb,      &
         9.962508e-01_rb,9.961836e-01_rb,9.961170e-01_rb,9.960508e-01_rb,      &
         9.959851e-01_rb,9.959198e-01_rb,9.958550e-01_rb,9.957906e-01_rb,      &
         9.957266e-01_rb,9.956629e-01_rb,9.955997e-01_rb,9.955367e-01_rb,      &
         9.954742e-01_rb,9.954119e-01_rb,9.953500e-01_rb /)
   ssaice2(:, 23) = (/                                                         &
! band 23
         9.997944e-01_rb,9.997791e-01_rb,9.997664e-01_rb,9.997547e-01_rb,      &
         9.997436e-01_rb,9.997327e-01_rb,9.997219e-01_rb,9.997110e-01_rb,      &
         9.996999e-01_rb,9.996886e-01_rb,9.996771e-01_rb,9.996653e-01_rb,      &
         9.996533e-01_rb,9.996409e-01_rb,9.996282e-01_rb,9.996152e-01_rb,      &
         9.996019e-01_rb,9.995883e-01_rb,9.995743e-01_rb,9.995599e-01_rb,      &
         9.995453e-01_rb,9.995302e-01_rb,9.995149e-01_rb,9.994992e-01_rb,      &
         9.994831e-01_rb,9.994667e-01_rb,9.994500e-01_rb,9.994329e-01_rb,      &
         9.994154e-01_rb,9.993976e-01_rb,9.993795e-01_rb,9.993610e-01_rb,      &
         9.993422e-01_rb,9.993230e-01_rb,9.993035e-01_rb,9.992837e-01_rb,      &
         9.992635e-01_rb,9.992429e-01_rb,9.992221e-01_rb,9.992008e-01_rb,      &
         9.991793e-01_rb,9.991574e-01_rb,9.991352e-01_rb /)
   ssaice2(:, 24) = (/                                                         &
! band 24
         9.999949e-01_rb,9.999947e-01_rb,9.999943e-01_rb,9.999939e-01_rb,      &
         9.999934e-01_rb,9.999927e-01_rb,9.999920e-01_rb,9.999913e-01_rb,      &
         9.999904e-01_rb,9.999895e-01_rb,9.999885e-01_rb,9.999874e-01_rb,      &
         9.999863e-01_rb,9.999851e-01_rb,9.999838e-01_rb,9.999824e-01_rb,      &
         9.999810e-01_rb,9.999795e-01_rb,9.999780e-01_rb,9.999764e-01_rb,      &
         9.999747e-01_rb,9.999729e-01_rb,9.999711e-01_rb,9.999692e-01_rb,      &
         9.999673e-01_rb,9.999653e-01_rb,9.999632e-01_rb,9.999611e-01_rb,      &
         9.999589e-01_rb,9.999566e-01_rb,9.999543e-01_rb,9.999519e-01_rb,      &
         9.999495e-01_rb,9.999470e-01_rb,9.999444e-01_rb,9.999418e-01_rb,      &
         9.999392e-01_rb,9.999364e-01_rb,9.999336e-01_rb,9.999308e-01_rb,      &
         9.999279e-01_rb,9.999249e-01_rb,9.999219e-01_rb /)
   ssaice2(:, 25) = (/                                                         &
! band 25
         9.999997e-01_rb,9.999997e-01_rb,9.999997e-01_rb,9.999996e-01_rb,      &
         9.999996e-01_rb,9.999995e-01_rb,9.999994e-01_rb,9.999993e-01_rb,      &
         9.999993e-01_rb,9.999992e-01_rb,9.999991e-01_rb,9.999989e-01_rb,      &
         9.999988e-01_rb,9.999987e-01_rb,9.999986e-01_rb,9.999984e-01_rb,      &
         9.999983e-01_rb,9.999981e-01_rb,9.999980e-01_rb,9.999978e-01_rb,      &
         9.999976e-01_rb,9.999974e-01_rb,9.999972e-01_rb,9.999971e-01_rb,      &
         9.999969e-01_rb,9.999966e-01_rb,9.999964e-01_rb,9.999962e-01_rb,      &
         9.999960e-01_rb,9.999957e-01_rb,9.999955e-01_rb,9.999953e-01_rb,      &
         9.999950e-01_rb,9.999947e-01_rb,9.999945e-01_rb,9.999942e-01_rb,      &
         9.999939e-01_rb,9.999936e-01_rb,9.999934e-01_rb,9.999931e-01_rb,      &
         9.999928e-01_rb,9.999925e-01_rb,9.999921e-01_rb /)
   ssaice2(:, 26) = (/                                                         &
! band 26
         9.999997e-01_rb,9.999996e-01_rb,9.999996e-01_rb,9.999995e-01_rb,      &
         9.999994e-01_rb,9.999993e-01_rb,9.999992e-01_rb,9.999991e-01_rb,      &
         9.999990e-01_rb,9.999989e-01_rb,9.999987e-01_rb,9.999986e-01_rb,      &
         9.999984e-01_rb,9.999982e-01_rb,9.999980e-01_rb,9.999978e-01_rb,      &
         9.999976e-01_rb,9.999974e-01_rb,9.999972e-01_rb,9.999970e-01_rb,      &
         9.999967e-01_rb,9.999965e-01_rb,9.999962e-01_rb,9.999959e-01_rb,      &
         9.999956e-01_rb,9.999954e-01_rb,9.999951e-01_rb,9.999947e-01_rb,      &
         9.999944e-01_rb,9.999941e-01_rb,9.999938e-01_rb,9.999934e-01_rb,      &
         9.999931e-01_rb,9.999927e-01_rb,9.999923e-01_rb,9.999920e-01_rb,      &
         9.999916e-01_rb,9.999912e-01_rb,9.999908e-01_rb,9.999904e-01_rb,      &
         9.999899e-01_rb,9.999895e-01_rb,9.999891e-01_rb /)
   ssaice2(:, 27) = (/                                                         &
! band 27
         9.999987e-01_rb,9.999987e-01_rb,9.999985e-01_rb,9.999984e-01_rb,      &
         9.999982e-01_rb,9.999980e-01_rb,9.999978e-01_rb,9.999976e-01_rb,      &
         9.999973e-01_rb,9.999970e-01_rb,9.999967e-01_rb,9.999964e-01_rb,      &
         9.999960e-01_rb,9.999956e-01_rb,9.999952e-01_rb,9.999948e-01_rb,      &
         9.999944e-01_rb,9.999939e-01_rb,9.999934e-01_rb,9.999929e-01_rb,      &
         9.999924e-01_rb,9.999918e-01_rb,9.999913e-01_rb,9.999907e-01_rb,      &
         9.999901e-01_rb,9.999894e-01_rb,9.999888e-01_rb,9.999881e-01_rb,      &
         9.999874e-01_rb,9.999867e-01_rb,9.999860e-01_rb,9.999853e-01_rb,      &
         9.999845e-01_rb,9.999837e-01_rb,9.999829e-01_rb,9.999821e-01_rb,      &
         9.999813e-01_rb,9.999804e-01_rb,9.999796e-01_rb,9.999787e-01_rb,      &
         9.999778e-01_rb,9.999768e-01_rb,9.999759e-01_rb /)
   ssaice2(:, 28) = (/                                                         &
! band 28
         9.999989e-01_rb,9.999989e-01_rb,9.999987e-01_rb,9.999986e-01_rb,      &
         9.999984e-01_rb,9.999982e-01_rb,9.999980e-01_rb,9.999978e-01_rb,      &
         9.999975e-01_rb,9.999972e-01_rb,9.999969e-01_rb,9.999966e-01_rb,      &
         9.999962e-01_rb,9.999958e-01_rb,9.999954e-01_rb,9.999950e-01_rb,      &
         9.999945e-01_rb,9.999941e-01_rb,9.999936e-01_rb,9.999931e-01_rb,      &
         9.999925e-01_rb,9.999920e-01_rb,9.999914e-01_rb,9.999908e-01_rb,      &
         9.999902e-01_rb,9.999896e-01_rb,9.999889e-01_rb,9.999883e-01_rb,      &
         9.999876e-01_rb,9.999869e-01_rb,9.999861e-01_rb,9.999854e-01_rb,      &
         9.999846e-01_rb,9.999838e-01_rb,9.999830e-01_rb,9.999822e-01_rb,      &
         9.999814e-01_rb,9.999805e-01_rb,9.999796e-01_rb,9.999787e-01_rb,      &
         9.999778e-01_rb,9.999769e-01_rb,9.999759e-01_rb /)
   ssaice2(:, 29) = (/                                                         &
! band 29
         7.042143e-01_rb,6.691161e-01_rb,6.463240e-01_rb,6.296590e-01_rb,      &
         6.166381e-01_rb,6.060183e-01_rb,5.970908e-01_rb,5.894144e-01_rb,      &
         5.826968e-01_rb,5.767343e-01_rb,5.713804e-01_rb,5.665256e-01_rb,      &
         5.620867e-01_rb,5.579987e-01_rb,5.542101e-01_rb,5.506794e-01_rb,      &
         5.473727e-01_rb,5.442620e-01_rb,5.413239e-01_rb,5.385389e-01_rb,      &
         5.358901e-01_rb,5.333633e-01_rb,5.309460e-01_rb,5.286277e-01_rb,      &
         5.263988e-01_rb,5.242512e-01_rb,5.221777e-01_rb,5.201719e-01_rb,      &
         5.182280e-01_rb,5.163410e-01_rb,5.145062e-01_rb,5.127197e-01_rb,      &
         5.109776e-01_rb,5.092766e-01_rb,5.076137e-01_rb,5.059860e-01_rb,      &
         5.043911e-01_rb,5.028266e-01_rb,5.012904e-01_rb,4.997805e-01_rb,      &
         4.982951e-01_rb,4.968326e-01_rb,4.953913e-01_rb /)
!
! asymmetry factor: unitless
!
   asyice2(:, 16) = (/                                                         &
! band 16
         7.946655e-01_rb,8.547685e-01_rb,8.806016e-01_rb,8.949880e-01_rb,      &
         9.041676e-01_rb,9.105399e-01_rb,9.152249e-01_rb,9.188160e-01_rb,      &
         9.216573e-01_rb,9.239620e-01_rb,9.258695e-01_rb,9.274745e-01_rb,      &
         9.288441e-01_rb,9.300267e-01_rb,9.310584e-01_rb,9.319665e-01_rb,      &
         9.327721e-01_rb,9.334918e-01_rb,9.341387e-01_rb,9.347236e-01_rb,      &
         9.352551e-01_rb,9.357402e-01_rb,9.361850e-01_rb,9.365942e-01_rb,      &
         9.369722e-01_rb,9.373225e-01_rb,9.376481e-01_rb,9.379516e-01_rb,      &
         9.382352e-01_rb,9.385010e-01_rb,9.387505e-01_rb,9.389854e-01_rb,      &
         9.392070e-01_rb,9.394163e-01_rb,9.396145e-01_rb,9.398024e-01_rb,      &
         9.399809e-01_rb,9.401508e-01_rb,9.403126e-01_rb,9.404670e-01_rb,      &
         9.406144e-01_rb,9.407555e-01_rb,9.408906e-01_rb /)
   asyice2(:, 17) = (/                                                         &
! band 17
         9.078091e-01_rb,9.195850e-01_rb,9.267250e-01_rb,9.317083e-01_rb,      &
         9.354632e-01_rb,9.384323e-01_rb,9.408597e-01_rb,9.428935e-01_rb,      &
         9.446301e-01_rb,9.461351e-01_rb,9.474555e-01_rb,9.486259e-01_rb,      &
         9.496722e-01_rb,9.506146e-01_rb,9.514688e-01_rb,9.522476e-01_rb,      &
         9.529612e-01_rb,9.536181e-01_rb,9.542251e-01_rb,9.547883e-01_rb,      &
         9.553124e-01_rb,9.558019e-01_rb,9.562601e-01_rb,9.566904e-01_rb,      &
         9.570953e-01_rb,9.574773e-01_rb,9.578385e-01_rb,9.581806e-01_rb,      &
         9.585054e-01_rb,9.588142e-01_rb,9.591083e-01_rb,9.593888e-01_rb,      &
         9.596569e-01_rb,9.599135e-01_rb,9.601593e-01_rb,9.603952e-01_rb,      &
         9.606219e-01_rb,9.608399e-01_rb,9.610499e-01_rb,9.612523e-01_rb,      &
         9.614477e-01_rb,9.616365e-01_rb,9.618192e-01_rb /)
   asyice2(:, 18) = (/                                                         &
! band 18
         8.322045e-01_rb,8.528693e-01_rb,8.648167e-01_rb,8.729163e-01_rb,      &
         8.789054e-01_rb,8.835845e-01_rb,8.873819e-01_rb,8.905511e-01_rb,      &
         8.932532e-01_rb,8.955965e-01_rb,8.976567e-01_rb,8.994887e-01_rb,      &
         9.011334e-01_rb,9.026221e-01_rb,9.039791e-01_rb,9.052237e-01_rb,      &
         9.063715e-01_rb,9.074349e-01_rb,9.084245e-01_rb,9.093489e-01_rb,      &
         9.102154e-01_rb,9.110303e-01_rb,9.117987e-01_rb,9.125253e-01_rb,      &
         9.132140e-01_rb,9.138682e-01_rb,9.144910e-01_rb,9.150850e-01_rb,      &
         9.156524e-01_rb,9.161955e-01_rb,9.167160e-01_rb,9.172157e-01_rb,      &
         9.176959e-01_rb,9.181581e-01_rb,9.186034e-01_rb,9.190330e-01_rb,      &
         9.194478e-01_rb,9.198488e-01_rb,9.202368e-01_rb,9.206126e-01_rb,      &
         9.209768e-01_rb,9.213301e-01_rb,9.216731e-01_rb /)
   asyice2(:, 19) = (/                                                         &
! band 19
         8.116560e-01_rb,8.488278e-01_rb,8.674331e-01_rb,8.788148e-01_rb,      &
         8.865810e-01_rb,8.922595e-01_rb,8.966149e-01_rb,9.000747e-01_rb,      &
         9.028980e-01_rb,9.052513e-01_rb,9.072468e-01_rb,9.089632e-01_rb,      &
         9.104574e-01_rb,9.117713e-01_rb,9.129371e-01_rb,9.139793e-01_rb,      &
         9.149174e-01_rb,9.157668e-01_rb,9.165400e-01_rb,9.172473e-01_rb,      &
         9.178970e-01_rb,9.184962e-01_rb,9.190508e-01_rb,9.195658e-01_rb,      &
         9.200455e-01_rb,9.204935e-01_rb,9.209130e-01_rb,9.213067e-01_rb,      &
         9.216771e-01_rb,9.220262e-01_rb,9.223560e-01_rb,9.226680e-01_rb,      &
         9.229636e-01_rb,9.232443e-01_rb,9.235112e-01_rb,9.237652e-01_rb,      &
         9.240074e-01_rb,9.242385e-01_rb,9.244594e-01_rb,9.246708e-01_rb,      &
         9.248733e-01_rb,9.250674e-01_rb,9.252536e-01_rb /)
   asyice2(:, 20) = (/                                                         &
! band 20
         8.047113e-01_rb,8.402864e-01_rb,8.570332e-01_rb,8.668455e-01_rb,      &
         8.733206e-01_rb,8.779272e-01_rb,8.813796e-01_rb,8.840676e-01_rb,      &
         8.862225e-01_rb,8.879904e-01_rb,8.894682e-01_rb,8.907228e-01_rb,      &
         8.918019e-01_rb,8.927404e-01_rb,8.935645e-01_rb,8.942943e-01_rb,      &
         8.949452e-01_rb,8.955296e-01_rb,8.960574e-01_rb,8.965366e-01_rb,      &
         8.969736e-01_rb,8.973740e-01_rb,8.977422e-01_rb,8.980820e-01_rb,      &
         8.983966e-01_rb,8.986889e-01_rb,8.989611e-01_rb,8.992153e-01_rb,      &
         8.994533e-01_rb,8.996766e-01_rb,8.998865e-01_rb,9.000843e-01_rb,      &
         9.002709e-01_rb,9.004474e-01_rb,9.006146e-01_rb,9.007731e-01_rb,      &
         9.009237e-01_rb,9.010670e-01_rb,9.012034e-01_rb,9.013336e-01_rb,      &
         9.014579e-01_rb,9.015767e-01_rb,9.016904e-01_rb /)
   asyice2(:, 21) = (/                                                         &
! band 21
         8.179122e-01_rb,8.480726e-01_rb,8.621945e-01_rb,8.704354e-01_rb,      &
         8.758555e-01_rb,8.797007e-01_rb,8.825750e-01_rb,8.848078e-01_rb,      &
         8.865939e-01_rb,8.880564e-01_rb,8.892765e-01_rb,8.903105e-01_rb,      &
         8.911982e-01_rb,8.919689e-01_rb,8.926446e-01_rb,8.932419e-01_rb,      &
         8.937738e-01_rb,8.942506e-01_rb,8.946806e-01_rb,8.950702e-01_rb,      &
         8.954251e-01_rb,8.957497e-01_rb,8.960477e-01_rb,8.963223e-01_rb,      &
         8.965762e-01_rb,8.968116e-01_rb,8.970306e-01_rb,8.972347e-01_rb,      &
         8.974255e-01_rb,8.976042e-01_rb,8.977720e-01_rb,8.979298e-01_rb,      &
         8.980784e-01_rb,8.982188e-01_rb,8.983515e-01_rb,8.984771e-01_rb,      &
         8.985963e-01_rb,8.987095e-01_rb,8.988171e-01_rb,8.989195e-01_rb,      &
         8.990172e-01_rb,8.991104e-01_rb,8.991994e-01_rb /)
   asyice2(:, 22) = (/                                                         &
! band 22
         8.169789e-01_rb,8.455024e-01_rb,8.586925e-01_rb,8.663283e-01_rb,      &
         8.713217e-01_rb,8.748488e-01_rb,8.774765e-01_rb,8.795122e-01_rb,      &
         8.811370e-01_rb,8.824649e-01_rb,8.835711e-01_rb,8.845073e-01_rb,      &
         8.853103e-01_rb,8.860068e-01_rb,8.866170e-01_rb,8.871560e-01_rb,      &
         8.876358e-01_rb,8.880658e-01_rb,8.884533e-01_rb,8.888044e-01_rb,      &
         8.891242e-01_rb,8.894166e-01_rb,8.896851e-01_rb,8.899324e-01_rb,      &
         8.901612e-01_rb,8.903733e-01_rb,8.905706e-01_rb,8.907545e-01_rb,      &
         8.909265e-01_rb,8.910876e-01_rb,8.912388e-01_rb,8.913812e-01_rb,      &
         8.915153e-01_rb,8.916419e-01_rb,8.917617e-01_rb,8.918752e-01_rb,      &
         8.919829e-01_rb,8.920851e-01_rb,8.921824e-01_rb,8.922751e-01_rb,      &
         8.923635e-01_rb,8.924478e-01_rb,8.925284e-01_rb /)
   asyice2(:, 23) = (/                                                         &
! band 23
         8.387642e-01_rb,8.569979e-01_rb,8.658630e-01_rb,8.711825e-01_rb,      &
         8.747605e-01_rb,8.773472e-01_rb,8.793129e-01_rb,8.808621e-01_rb,      &
         8.821179e-01_rb,8.831583e-01_rb,8.840361e-01_rb,8.847875e-01_rb,      &
         8.854388e-01_rb,8.860094e-01_rb,8.865138e-01_rb,8.869634e-01_rb,      &
         8.873668e-01_rb,8.877310e-01_rb,8.880617e-01_rb,8.883635e-01_rb,      &
         8.886401e-01_rb,8.888947e-01_rb,8.891298e-01_rb,8.893477e-01_rb,      &
         8.895504e-01_rb,8.897393e-01_rb,8.899159e-01_rb,8.900815e-01_rb,      &
         8.902370e-01_rb,8.903833e-01_rb,8.905214e-01_rb,8.906518e-01_rb,      &
         8.907753e-01_rb,8.908924e-01_rb,8.910036e-01_rb,8.911094e-01_rb,      &
         8.912101e-01_rb,8.913062e-01_rb,8.913979e-01_rb,8.914856e-01_rb,      &
         8.915695e-01_rb,8.916498e-01_rb,8.917269e-01_rb /)
   asyice2(:, 24) = (/                                                         &
! band 24
         8.522208e-01_rb,8.648132e-01_rb,8.711224e-01_rb,8.749901e-01_rb,      &
         8.776354e-01_rb,8.795743e-01_rb,8.810649e-01_rb,8.822518e-01_rb,      &
         8.832225e-01_rb,8.840333e-01_rb,8.847224e-01_rb,8.853162e-01_rb,      &
         8.858342e-01_rb,8.862906e-01_rb,8.866962e-01_rb,8.870595e-01_rb,      &
         8.873871e-01_rb,8.876842e-01_rb,8.879551e-01_rb,8.882032e-01_rb,      &
         8.884316e-01_rb,8.886425e-01_rb,8.888380e-01_rb,8.890199e-01_rb,      &
         8.891895e-01_rb,8.893481e-01_rb,8.894968e-01_rb,8.896366e-01_rb,      &
         8.897683e-01_rb,8.898926e-01_rb,8.900102e-01_rb,8.901215e-01_rb,      &
         8.902272e-01_rb,8.903276e-01_rb,8.904232e-01_rb,8.905144e-01_rb,      &
         8.906014e-01_rb,8.906845e-01_rb,8.907640e-01_rb,8.908402e-01_rb,      &
         8.909132e-01_rb,8.909834e-01_rb,8.910507e-01_rb /)
   asyice2(:, 25) = (/                                                         &
! band 25
         8.578202e-01_rb,8.683033e-01_rb,8.735431e-01_rb,8.767488e-01_rb,      &
         8.789378e-01_rb,8.805399e-01_rb,8.817701e-01_rb,8.827485e-01_rb,      &
         8.835480e-01_rb,8.842152e-01_rb,8.847817e-01_rb,8.852696e-01_rb,      &
         8.856949e-01_rb,8.860694e-01_rb,8.864020e-01_rb,8.866997e-01_rb,      &
         8.869681e-01_rb,8.872113e-01_rb,8.874330e-01_rb,8.876360e-01_rb,      &
         8.878227e-01_rb,8.879951e-01_rb,8.881548e-01_rb,8.883033e-01_rb,      &
         8.884418e-01_rb,8.885712e-01_rb,8.886926e-01_rb,8.888066e-01_rb,      &
         8.889139e-01_rb,8.890152e-01_rb,8.891110e-01_rb,8.892017e-01_rb,      &
         8.892877e-01_rb,8.893695e-01_rb,8.894473e-01_rb,8.895214e-01_rb,      &
         8.895921e-01_rb,8.896597e-01_rb,8.897243e-01_rb,8.897862e-01_rb,      &
         8.898456e-01_rb,8.899025e-01_rb,8.899572e-01_rb /)
   asyice2(:, 26) = (/                                                         &
! band 26
         8.625615e-01_rb,8.713831e-01_rb,8.755799e-01_rb,8.780560e-01_rb,      &
         8.796983e-01_rb,8.808714e-01_rb,8.817534e-01_rb,8.824420e-01_rb,      &
         8.829953e-01_rb,8.834501e-01_rb,8.838310e-01_rb,8.841549e-01_rb,      &
         8.844338e-01_rb,8.846767e-01_rb,8.848902e-01_rb,8.850795e-01_rb,      &
         8.852484e-01_rb,8.854002e-01_rb,8.855374e-01_rb,8.856620e-01_rb,      &
         8.857758e-01_rb,8.858800e-01_rb,8.859759e-01_rb,8.860644e-01_rb,      &
         8.861464e-01_rb,8.862225e-01_rb,8.862935e-01_rb,8.863598e-01_rb,      &
         8.864218e-01_rb,8.864800e-01_rb,8.865347e-01_rb,8.865863e-01_rb,      &
         8.866349e-01_rb,8.866809e-01_rb,8.867245e-01_rb,8.867658e-01_rb,      &
         8.868050e-01_rb,8.868423e-01_rb,8.868778e-01_rb,8.869117e-01_rb,      &
         8.869440e-01_rb,8.869749e-01_rb,8.870044e-01_rb /)
   asyice2(:, 27) = (/                                                         &
! band 27
         8.587495e-01_rb,8.684764e-01_rb,8.728189e-01_rb,8.752872e-01_rb,      &
         8.768846e-01_rb,8.780060e-01_rb,8.788386e-01_rb,8.794824e-01_rb,      &
         8.799960e-01_rb,8.804159e-01_rb,8.807660e-01_rb,8.810626e-01_rb,      &
         8.813175e-01_rb,8.815390e-01_rb,8.817335e-01_rb,8.819057e-01_rb,      &
         8.820593e-01_rb,8.821973e-01_rb,8.823220e-01_rb,8.824353e-01_rb,      &
         8.825387e-01_rb,8.826336e-01_rb,8.827209e-01_rb,8.828016e-01_rb,      &
         8.828764e-01_rb,8.829459e-01_rb,8.830108e-01_rb,8.830715e-01_rb,      &
         8.831283e-01_rb,8.831817e-01_rb,8.832320e-01_rb,8.832795e-01_rb,      &
         8.833244e-01_rb,8.833668e-01_rb,8.834071e-01_rb,8.834454e-01_rb,      &
         8.834817e-01_rb,8.835164e-01_rb,8.835495e-01_rb,8.835811e-01_rb,      &
         8.836113e-01_rb,8.836402e-01_rb,8.836679e-01_rb /)
   asyice2(:, 28) = (/                                                         &
! band 28
         8.561110e-01_rb,8.678583e-01_rb,8.727554e-01_rb,8.753892e-01_rb,      &
         8.770154e-01_rb,8.781109e-01_rb,8.788949e-01_rb,8.794812e-01_rb,      &
         8.799348e-01_rb,8.802952e-01_rb,8.805880e-01_rb,8.808300e-01_rb,      &
         8.810331e-01_rb,8.812058e-01_rb,8.813543e-01_rb,8.814832e-01_rb,      &
         8.815960e-01_rb,8.816956e-01_rb,8.817839e-01_rb,8.818629e-01_rb,      &
         8.819339e-01_rb,8.819979e-01_rb,8.820560e-01_rb,8.821089e-01_rb,      &
         8.821573e-01_rb,8.822016e-01_rb,8.822425e-01_rb,8.822801e-01_rb,      &
         8.823150e-01_rb,8.823474e-01_rb,8.823775e-01_rb,8.824056e-01_rb,      &
         8.824318e-01_rb,8.824564e-01_rb,8.824795e-01_rb,8.825011e-01_rb,      &
         8.825215e-01_rb,8.825408e-01_rb,8.825589e-01_rb,8.825761e-01_rb,      &
         8.825924e-01_rb,8.826078e-01_rb,8.826224e-01_rb /)
   asyice2(:, 29) = (/                                                         &
! band 29
         8.311124e-01_rb,8.688197e-01_rb,8.900274e-01_rb,9.040696e-01_rb,      &
         9.142334e-01_rb,9.220181e-01_rb,9.282195e-01_rb,9.333048e-01_rb,      &
         9.375689e-01_rb,9.412085e-01_rb,9.443604e-01_rb,9.471230e-01_rb,      &
         9.495694e-01_rb,9.517549e-01_rb,9.537224e-01_rb,9.555057e-01_rb,      &
         9.571316e-01_rb,9.586222e-01_rb,9.599952e-01_rb,9.612656e-01_rb,      &
         9.624458e-01_rb,9.635461e-01_rb,9.645756e-01_rb,9.655418e-01_rb,      &
         9.664513e-01_rb,9.673098e-01_rb,9.681222e-01_rb,9.688928e-01_rb,      &
         9.696256e-01_rb,9.703237e-01_rb,9.709903e-01_rb,9.716280e-01_rb,      &
         9.722391e-01_rb,9.728258e-01_rb,9.733901e-01_rb,9.739336e-01_rb,      &
         9.744579e-01_rb,9.749645e-01_rb,9.754546e-01_rb,9.759294e-01_rb,      &
         9.763901e-01_rb,9.768376e-01_rb,9.772727e-01_rb /)
!
! Hexagonal Ice Particle Parameterization
! extinction units (ext coef/iwc): [(m^-1)/(g m^-3)]
!
   extice3(:, 16) = (/                                                         &
! band 16
         5.194013e-01_rb,3.215089e-01_rb,2.327917e-01_rb,1.824424e-01_rb,      &
         1.499977e-01_rb,1.273492e-01_rb,1.106421e-01_rb,9.780982e-02_rb,      &
         8.764435e-02_rb,7.939266e-02_rb,7.256081e-02_rb,6.681137e-02_rb,      &
         6.190600e-02_rb,5.767154e-02_rb,5.397915e-02_rb,5.073102e-02_rb,      &
         4.785151e-02_rb,4.528125e-02_rb,4.297296e-02_rb,4.088853e-02_rb,      &
         3.899690e-02_rb,3.727251e-02_rb,3.569411e-02_rb,3.424393e-02_rb,      &
         3.290694e-02_rb,3.167040e-02_rb,3.052340e-02_rb,2.945654e-02_rb,      &
         2.846172e-02_rb,2.753188e-02_rb,2.666085e-02_rb,2.584322e-02_rb,      &
         2.507423e-02_rb,2.434967e-02_rb,2.366579e-02_rb,2.301926e-02_rb,      &
         2.240711e-02_rb,2.182666e-02_rb,2.127551e-02_rb,2.075150e-02_rb,      &
         2.025267e-02_rb,1.977725e-02_rb,1.932364e-02_rb,1.889035e-02_rb,      &
         1.847607e-02_rb,1.807956e-02_rb /)
   extice3(:, 17) = (/                                                         &
! band 17
         4.901155e-01_rb,3.065286e-01_rb,2.230800e-01_rb,1.753951e-01_rb,      &
         1.445402e-01_rb,1.229417e-01_rb,1.069777e-01_rb,9.469760e-02_rb,      &
         8.495824e-02_rb,7.704501e-02_rb,7.048834e-02_rb,6.496693e-02_rb,      &
         6.025353e-02_rb,5.618286e-02_rb,5.263186e-02_rb,4.950698e-02_rb,      &
         4.673585e-02_rb,4.426164e-02_rb,4.203904e-02_rb,4.003153e-02_rb,      &
         3.820932e-02_rb,3.654790e-02_rb,3.502688e-02_rb,3.362919e-02_rb,      &
         3.234041e-02_rb,3.114829e-02_rb,3.004234e-02_rb,2.901356e-02_rb,      &
         2.805413e-02_rb,2.715727e-02_rb,2.631705e-02_rb,2.552828e-02_rb,      &
         2.478637e-02_rb,2.408725e-02_rb,2.342734e-02_rb,2.280343e-02_rb,      &
         2.221264e-02_rb,2.165242e-02_rb,2.112043e-02_rb,2.061461e-02_rb,      &
         2.013308e-02_rb,1.967411e-02_rb,1.923616e-02_rb,1.881783e-02_rb,      &
         1.841781e-02_rb,1.803494e-02_rb /)
   extice3(:, 18) = (/                                                         &
! band 18
         5.056264e-01_rb,3.160261e-01_rb,2.298442e-01_rb,1.805973e-01_rb,      &
         1.487318e-01_rb,1.264258e-01_rb,1.099389e-01_rb,9.725656e-02_rb,      &
         8.719819e-02_rb,7.902576e-02_rb,7.225433e-02_rb,6.655206e-02_rb,      &
         6.168427e-02_rb,5.748028e-02_rb,5.381296e-02_rb,5.058572e-02_rb,      &
         4.772383e-02_rb,4.516857e-02_rb,4.287317e-02_rb,4.079990e-02_rb,      &
         3.891801e-02_rb,3.720217e-02_rb,3.563133e-02_rb,3.418786e-02_rb,      &
         3.285686e-02_rb,3.162569e-02_rb,3.048352e-02_rb,2.942104e-02_rb,      &
         2.843018e-02_rb,2.750395e-02_rb,2.663621e-02_rb,2.582160e-02_rb,      &
         2.505539e-02_rb,2.433337e-02_rb,2.365185e-02_rb,2.300750e-02_rb,      &
         2.239736e-02_rb,2.181878e-02_rb,2.126937e-02_rb,2.074699e-02_rb,      &
         2.024968e-02_rb,1.977567e-02_rb,1.932338e-02_rb,1.889134e-02_rb,      &
         1.847823e-02_rb,1.808281e-02_rb /)
   extice3(:, 19) = (/                                                         &
! band 19
         4.881605e-01_rb,3.055237e-01_rb,2.225070e-01_rb,1.750688e-01_rb,      &
         1.443736e-01_rb,1.228869e-01_rb,1.070054e-01_rb,9.478893e-02_rb,      &
         8.509997e-02_rb,7.722769e-02_rb,7.070495e-02_rb,6.521211e-02_rb,      &
         6.052311e-02_rb,5.647351e-02_rb,5.294088e-02_rb,4.983217e-02_rb,      &
         4.707539e-02_rb,4.461398e-02_rb,4.240288e-02_rb,4.040575e-02_rb,      &
         3.859298e-02_rb,3.694016e-02_rb,3.542701e-02_rb,3.403655e-02_rb,      &
         3.275444e-02_rb,3.156849e-02_rb,3.046827e-02_rb,2.944481e-02_rb,      &
         2.849034e-02_rb,2.759812e-02_rb,2.676226e-02_rb,2.597757e-02_rb,      &
         2.523949e-02_rb,2.454400e-02_rb,2.388750e-02_rb,2.326682e-02_rb,      &
         2.267909e-02_rb,2.212176e-02_rb,2.159253e-02_rb,2.108933e-02_rb,      &
         2.061028e-02_rb,2.015369e-02_rb,1.971801e-02_rb,1.930184e-02_rb,      &
         1.890389e-02_rb,1.852300e-02_rb /)
   extice3(:, 20) = (/                                                         &
! band 20
         5.103703e-01_rb,3.188144e-01_rb,2.317435e-01_rb,1.819887e-01_rb,      &
         1.497944e-01_rb,1.272584e-01_rb,1.106013e-01_rb,9.778822e-02_rb,      &
         8.762610e-02_rb,7.936938e-02_rb,7.252809e-02_rb,6.676701e-02_rb,      &
         6.184901e-02_rb,5.760165e-02_rb,5.389651e-02_rb,5.063598e-02_rb,      &
         4.774457e-02_rb,4.516295e-02_rb,4.284387e-02_rb,4.074922e-02_rb,      &
         3.884792e-02_rb,3.711438e-02_rb,3.552734e-02_rb,3.406898e-02_rb,      &
         3.272425e-02_rb,3.148038e-02_rb,3.032643e-02_rb,2.925299e-02_rb,      &
         2.825191e-02_rb,2.731612e-02_rb,2.643943e-02_rb,2.561642e-02_rb,      &
         2.484230e-02_rb,2.411284e-02_rb,2.342429e-02_rb,2.277329e-02_rb,      &
         2.215686e-02_rb,2.157231e-02_rb,2.101724e-02_rb,2.048946e-02_rb,      &
         1.998702e-02_rb,1.950813e-02_rb,1.905118e-02_rb,1.861468e-02_rb,      &
         1.819730e-02_rb,1.779781e-02_rb /)
   extice3(:, 21) = (/                                                         &
! band 21
         5.031161e-01_rb,3.144511e-01_rb,2.286942e-01_rb,1.796903e-01_rb,      &
         1.479819e-01_rb,1.257860e-01_rb,1.093803e-01_rb,9.676059e-02_rb,      &
         8.675183e-02_rb,7.861971e-02_rb,7.188168e-02_rb,6.620754e-02_rb,      &
         6.136376e-02_rb,5.718050e-02_rb,5.353127e-02_rb,5.031995e-02_rb,      &
         4.747218e-02_rb,4.492952e-02_rb,4.264544e-02_rb,4.058240e-02_rb,      &
         3.870979e-02_rb,3.700242e-02_rb,3.543933e-02_rb,3.400297e-02_rb,      &
         3.267854e-02_rb,3.145345e-02_rb,3.031691e-02_rb,2.925967e-02_rb,      &
         2.827370e-02_rb,2.735203e-02_rb,2.648858e-02_rb,2.567798e-02_rb,      &
         2.491555e-02_rb,2.419710e-02_rb,2.351893e-02_rb,2.287776e-02_rb,      &
         2.227063e-02_rb,2.169491e-02_rb,2.114821e-02_rb,2.062840e-02_rb,      &
         2.013354e-02_rb,1.966188e-02_rb,1.921182e-02_rb,1.878191e-02_rb,      &
         1.837083e-02_rb,1.797737e-02_rb /)
   extice3(:, 22) = (/                                                         &
! band 22
         4.949453e-01_rb,3.095918e-01_rb,2.253402e-01_rb,1.771964e-01_rb,      &
         1.460446e-01_rb,1.242383e-01_rb,1.081206e-01_rb,9.572235e-02_rb,      &
         8.588928e-02_rb,7.789990e-02_rb,7.128013e-02_rb,6.570559e-02_rb,      &
         6.094684e-02_rb,5.683701e-02_rb,5.325183e-02_rb,5.009688e-02_rb,      &
         4.729909e-02_rb,4.480106e-02_rb,4.255708e-02_rb,4.053025e-02_rb,      &
         3.869051e-02_rb,3.701310e-02_rb,3.547745e-02_rb,3.406631e-02_rb,      &
         3.276512e-02_rb,3.156153e-02_rb,3.044494e-02_rb,2.940626e-02_rb,      &
         2.843759e-02_rb,2.753211e-02_rb,2.668381e-02_rb,2.588744e-02_rb,      &
         2.513839e-02_rb,2.443255e-02_rb,2.376629e-02_rb,2.313637e-02_rb,      &
         2.253990e-02_rb,2.197428e-02_rb,2.143718e-02_rb,2.092649e-02_rb,      &
         2.044032e-02_rb,1.997694e-02_rb,1.953478e-02_rb,1.911241e-02_rb,      &
         1.870855e-02_rb,1.832199e-02_rb /)
   extice3(:, 23) = (/                                                         &
! band 23
         5.052816e-01_rb,3.157665e-01_rb,2.296233e-01_rb,1.803986e-01_rb,      &
         1.485473e-01_rb,1.262514e-01_rb,1.097718e-01_rb,9.709524e-02_rb,      &
         8.704139e-02_rb,7.887264e-02_rb,7.210424e-02_rb,6.640454e-02_rb,      &
         6.153894e-02_rb,5.733683e-02_rb,5.367116e-02_rb,5.044537e-02_rb,      &
         4.758477e-02_rb,4.503066e-02_rb,4.273629e-02_rb,4.066395e-02_rb,      &
         3.878291e-02_rb,3.706784e-02_rb,3.549771e-02_rb,3.405488e-02_rb,      &
         3.272448e-02_rb,3.149387e-02_rb,3.035221e-02_rb,2.929020e-02_rb,      &
         2.829979e-02_rb,2.737397e-02_rb,2.650663e-02_rb,2.569238e-02_rb,      &
         2.492651e-02_rb,2.420482e-02_rb,2.352361e-02_rb,2.287954e-02_rb,      &
         2.226968e-02_rb,2.169136e-02_rb,2.114220e-02_rb,2.062005e-02_rb,      &
         2.012296e-02_rb,1.964917e-02_rb,1.919709e-02_rb,1.876524e-02_rb,      &
         1.835231e-02_rb,1.795707e-02_rb /)
   extice3(:, 24) = (/                                                         &
! band 24
         5.042067e-01_rb,3.151195e-01_rb,2.291708e-01_rb,1.800573e-01_rb,      &
         1.482779e-01_rb,1.260324e-01_rb,1.095900e-01_rb,9.694202e-02_rb,      &
         8.691087e-02_rb,7.876056e-02_rb,7.200745e-02_rb,6.632062e-02_rb,      &
         6.146600e-02_rb,5.727338e-02_rb,5.361599e-02_rb,5.039749e-02_rb,      &
         4.754334e-02_rb,4.499500e-02_rb,4.270580e-02_rb,4.063815e-02_rb,      &
         3.876135e-02_rb,3.705016e-02_rb,3.548357e-02_rb,3.404400e-02_rb,      &
         3.271661e-02_rb,3.148877e-02_rb,3.034969e-02_rb,2.929008e-02_rb,      &
         2.830191e-02_rb,2.737818e-02_rb,2.651279e-02_rb,2.570039e-02_rb,      &
         2.493624e-02_rb,2.421618e-02_rb,2.353650e-02_rb,2.289390e-02_rb,      &
         2.228541e-02_rb,2.170840e-02_rb,2.116048e-02_rb,2.063950e-02_rb,      &
         2.014354e-02_rb,1.967082e-02_rb,1.921975e-02_rb,1.878888e-02_rb,      &
         1.837688e-02_rb,1.798254e-02_rb /)
   extice3(:, 25) = (/                                                         &
! band 25
         5.022507e-01_rb,3.139246e-01_rb,2.283218e-01_rb,1.794059e-01_rb,      &
         1.477544e-01_rb,1.255984e-01_rb,1.092222e-01_rb,9.662516e-02_rb,      &
         8.663439e-02_rb,7.851688e-02_rb,7.179095e-02_rb,6.612700e-02_rb,      &
         6.129193e-02_rb,5.711618e-02_rb,5.347351e-02_rb,5.026796e-02_rb,      &
         4.742530e-02_rb,4.488721e-02_rb,4.260724e-02_rb,4.054790e-02_rb,      &
         3.867866e-02_rb,3.697435e-02_rb,3.541407e-02_rb,3.398029e-02_rb,      &
         3.265824e-02_rb,3.143535e-02_rb,3.030085e-02_rb,2.924551e-02_rb,      &
         2.826131e-02_rb,2.734130e-02_rb,2.647939e-02_rb,2.567026e-02_rb,      &
         2.490919e-02_rb,2.419203e-02_rb,2.351509e-02_rb,2.287507e-02_rb,      &
         2.226903e-02_rb,2.169434e-02_rb,2.114862e-02_rb,2.062975e-02_rb,      &
         2.013578e-02_rb,1.966496e-02_rb,1.921571e-02_rb,1.878658e-02_rb,      &
         1.837623e-02_rb,1.798348e-02_rb /)
   extice3(:, 26) = (/                                                         &
! band 26
         5.068316e-01_rb,3.166869e-01_rb,2.302576e-01_rb,1.808693e-01_rb,      &
         1.489122e-01_rb,1.265423e-01_rb,1.100080e-01_rb,9.728926e-02_rb,      &
         8.720201e-02_rb,7.900612e-02_rb,7.221524e-02_rb,6.649660e-02_rb,      &
         6.161484e-02_rb,5.739877e-02_rb,5.372093e-02_rb,5.048442e-02_rb,      &
         4.761431e-02_rb,4.505172e-02_rb,4.274972e-02_rb,4.067050e-02_rb,      &
         3.878321e-02_rb,3.706244e-02_rb,3.548710e-02_rb,3.403948e-02_rb,      &
         3.270466e-02_rb,3.146995e-02_rb,3.032450e-02_rb,2.925897e-02_rb,      &
         2.826527e-02_rb,2.733638e-02_rb,2.646615e-02_rb,2.564920e-02_rb,      &
         2.488078e-02_rb,2.415670e-02_rb,2.347322e-02_rb,2.282702e-02_rb,      &
         2.221513e-02_rb,2.163489e-02_rb,2.108390e-02_rb,2.056002e-02_rb,      &
         2.006128e-02_rb,1.958591e-02_rb,1.913232e-02_rb,1.869904e-02_rb,      &
         1.828474e-02_rb,1.788819e-02_rb /)
   extice3(:, 27) = (/                                                         &
! band 27
         5.077707e-01_rb,3.172636e-01_rb,2.306695e-01_rb,1.811871e-01_rb,      &
         1.491691e-01_rb,1.267565e-01_rb,1.101907e-01_rb,9.744773e-02_rb,      &
         8.734125e-02_rb,7.912973e-02_rb,7.232591e-02_rb,6.659637e-02_rb,      &
         6.170530e-02_rb,5.748120e-02_rb,5.379634e-02_rb,5.055367e-02_rb,      &
         4.767809e-02_rb,4.511061e-02_rb,4.280423e-02_rb,4.072104e-02_rb,      &
         3.883015e-02_rb,3.710611e-02_rb,3.552776e-02_rb,3.407738e-02_rb,      &
         3.274002e-02_rb,3.150296e-02_rb,3.035532e-02_rb,2.928776e-02_rb,      &
         2.829216e-02_rb,2.736150e-02_rb,2.648961e-02_rb,2.567111e-02_rb,      &
         2.490123e-02_rb,2.417576e-02_rb,2.349098e-02_rb,2.284354e-02_rb,      &
         2.223049e-02_rb,2.164914e-02_rb,2.109711e-02_rb,2.057222e-02_rb,      &
         2.007253e-02_rb,1.959626e-02_rb,1.914181e-02_rb,1.870770e-02_rb,      &
         1.829261e-02_rb,1.789531e-02_rb /)
   extice3(:, 28) = (/                                                         &
! band 28
         5.062281e-01_rb,3.163402e-01_rb,2.300275e-01_rb,1.807060e-01_rb,      &
         1.487921e-01_rb,1.264523e-01_rb,1.099403e-01_rb,9.723879e-02_rb,      &
         8.716516e-02_rb,7.898034e-02_rb,7.219863e-02_rb,6.648771e-02_rb,      &
         6.161254e-02_rb,5.740217e-02_rb,5.372929e-02_rb,5.049716e-02_rb,      &
         4.763092e-02_rb,4.507179e-02_rb,4.277290e-02_rb,4.069649e-02_rb,      &
         3.881175e-02_rb,3.709331e-02_rb,3.552008e-02_rb,3.407442e-02_rb,      &
         3.274141e-02_rb,3.150837e-02_rb,3.036447e-02_rb,2.930037e-02_rb,      &
         2.830801e-02_rb,2.738037e-02_rb,2.651132e-02_rb,2.569547e-02_rb,      &
         2.492810e-02_rb,2.420499e-02_rb,2.352243e-02_rb,2.287710e-02_rb,      &
         2.226604e-02_rb,2.168658e-02_rb,2.113634e-02_rb,2.061316e-02_rb,      &
         2.011510e-02_rb,1.964038e-02_rb,1.918740e-02_rb,1.875471e-02_rb,      &
         1.834096e-02_rb,1.794495e-02_rb /)
   extice3(:, 29) = (/                                                         &
! band 29
         1.338834e-01_rb,1.924912e-01_rb,1.755523e-01_rb,1.534793e-01_rb,      &
         1.343937e-01_rb,1.187883e-01_rb,1.060654e-01_rb,9.559106e-02_rb,      &
         8.685880e-02_rb,7.948698e-02_rb,7.319086e-02_rb,6.775669e-02_rb,      &
         6.302215e-02_rb,5.886236e-02_rb,5.517996e-02_rb,5.189810e-02_rb,      &
         4.895539e-02_rb,4.630225e-02_rb,4.389823e-02_rb,4.171002e-02_rb,      &
         3.970998e-02_rb,3.787493e-02_rb,3.618537e-02_rb,3.462471e-02_rb,      &
         3.317880e-02_rb,3.183547e-02_rb,3.058421e-02_rb,2.941590e-02_rb,      &
         2.832256e-02_rb,2.729724e-02_rb,2.633377e-02_rb,2.542675e-02_rb,      &
         2.457136e-02_rb,2.376332e-02_rb,2.299882e-02_rb,2.227443e-02_rb,      &
         2.158707e-02_rb,2.093400e-02_rb,2.031270e-02_rb,1.972091e-02_rb,      &
         1.915659e-02_rb,1.861787e-02_rb,1.810304e-02_rb,1.761055e-02_rb,      &
         1.713899e-02_rb,1.668704e-02_rb /)
!
! single-scattering albedo: unitless
!
   ssaice3(:, 16) = (/                                                         &
! band 16
         6.749442e-01_rb,6.649947e-01_rb,6.565828e-01_rb,6.489928e-01_rb,      &
         6.420046e-01_rb,6.355231e-01_rb,6.294964e-01_rb,6.238901e-01_rb,      &
         6.186783e-01_rb,6.138395e-01_rb,6.093543e-01_rb,6.052049e-01_rb,      &
         6.013742e-01_rb,5.978457e-01_rb,5.946030e-01_rb,5.916302e-01_rb,      &
         5.889115e-01_rb,5.864310e-01_rb,5.841731e-01_rb,5.821221e-01_rb,      &
         5.802624e-01_rb,5.785785e-01_rb,5.770549e-01_rb,5.756759e-01_rb,      &
         5.744262e-01_rb,5.732901e-01_rb,5.722524e-01_rb,5.712974e-01_rb,      &
         5.704097e-01_rb,5.695739e-01_rb,5.687747e-01_rb,5.679964e-01_rb,      &
         5.672238e-01_rb,5.664415e-01_rb,5.656340e-01_rb,5.647860e-01_rb,      &
         5.638821e-01_rb,5.629070e-01_rb,5.618452e-01_rb,5.606815e-01_rb,      &
         5.594006e-01_rb,5.579870e-01_rb,5.564255e-01_rb,5.547008e-01_rb,      &
         5.527976e-01_rb,5.507005e-01_rb /)
   ssaice3(:, 17) = (/                                                         &
! band 17
         7.628550e-01_rb,7.567297e-01_rb,7.508463e-01_rb,7.451972e-01_rb,      &
         7.397745e-01_rb,7.345705e-01_rb,7.295775e-01_rb,7.247881e-01_rb,      &
         7.201945e-01_rb,7.157894e-01_rb,7.115652e-01_rb,7.075145e-01_rb,      &
         7.036300e-01_rb,6.999044e-01_rb,6.963304e-01_rb,6.929007e-01_rb,      &
         6.896083e-01_rb,6.864460e-01_rb,6.834067e-01_rb,6.804833e-01_rb,      &
         6.776690e-01_rb,6.749567e-01_rb,6.723397e-01_rb,6.698109e-01_rb,      &
         6.673637e-01_rb,6.649913e-01_rb,6.626870e-01_rb,6.604441e-01_rb,      &
         6.582561e-01_rb,6.561163e-01_rb,6.540182e-01_rb,6.519554e-01_rb,      &
         6.499215e-01_rb,6.479099e-01_rb,6.459145e-01_rb,6.439289e-01_rb,      &
         6.419468e-01_rb,6.399621e-01_rb,6.379686e-01_rb,6.359601e-01_rb,      &
         6.339306e-01_rb,6.318740e-01_rb,6.297845e-01_rb,6.276559e-01_rb,      &
         6.254825e-01_rb,6.232583e-01_rb /)
   ssaice3(:, 18) = (/                                                         &
! band 18
         9.924147e-01_rb,9.882792e-01_rb,9.842257e-01_rb,9.802522e-01_rb,      &
         9.763566e-01_rb,9.725367e-01_rb,9.687905e-01_rb,9.651157e-01_rb,      &
         9.615104e-01_rb,9.579725e-01_rb,9.544997e-01_rb,9.510901e-01_rb,      &
         9.477416e-01_rb,9.444520e-01_rb,9.412194e-01_rb,9.380415e-01_rb,      &
         9.349165e-01_rb,9.318421e-01_rb,9.288164e-01_rb,9.258373e-01_rb,      &
         9.229027e-01_rb,9.200106e-01_rb,9.171589e-01_rb,9.143457e-01_rb,      &
         9.115688e-01_rb,9.088263e-01_rb,9.061161e-01_rb,9.034362e-01_rb,      &
         9.007846e-01_rb,8.981592e-01_rb,8.955581e-01_rb,8.929792e-01_rb,      &
         8.904206e-01_rb,8.878803e-01_rb,8.853562e-01_rb,8.828464e-01_rb,      &
         8.803488e-01_rb,8.778616e-01_rb,8.753827e-01_rb,8.729102e-01_rb,      &
         8.704421e-01_rb,8.679764e-01_rb,8.655112e-01_rb,8.630445e-01_rb,      &
         8.605744e-01_rb,8.580989e-01_rb /)
   ssaice3(:, 19) = (/                                                         &
! band 19
         9.629413e-01_rb,9.517182e-01_rb,9.409209e-01_rb,9.305366e-01_rb,      &
         9.205529e-01_rb,9.109569e-01_rb,9.017362e-01_rb,8.928780e-01_rb,      &
         8.843699e-01_rb,8.761992e-01_rb,8.683536e-01_rb,8.608204e-01_rb,      &
         8.535873e-01_rb,8.466417e-01_rb,8.399712e-01_rb,8.335635e-01_rb,      &
         8.274062e-01_rb,8.214868e-01_rb,8.157932e-01_rb,8.103129e-01_rb,      &
         8.050336e-01_rb,7.999432e-01_rb,7.950294e-01_rb,7.902798e-01_rb,      &
         7.856825e-01_rb,7.812250e-01_rb,7.768954e-01_rb,7.726815e-01_rb,      &
         7.685711e-01_rb,7.645522e-01_rb,7.606126e-01_rb,7.567404e-01_rb,      &
         7.529234e-01_rb,7.491498e-01_rb,7.454074e-01_rb,7.416844e-01_rb,      &
         7.379688e-01_rb,7.342485e-01_rb,7.305118e-01_rb,7.267468e-01_rb,      &
         7.229415e-01_rb,7.190841e-01_rb,7.151628e-01_rb,7.111657e-01_rb,      &
         7.070811e-01_rb,7.028972e-01_rb /)
   ssaice3(:, 20) = (/                                                         &
! band 20
         9.942270e-01_rb,9.909206e-01_rb,9.876775e-01_rb,9.844960e-01_rb,      &
         9.813746e-01_rb,9.783114e-01_rb,9.753049e-01_rb,9.723535e-01_rb,      &
         9.694553e-01_rb,9.666088e-01_rb,9.638123e-01_rb,9.610641e-01_rb,      &
         9.583626e-01_rb,9.557060e-01_rb,9.530928e-01_rb,9.505211e-01_rb,      &
         9.479895e-01_rb,9.454961e-01_rb,9.430393e-01_rb,9.406174e-01_rb,      &
         9.382288e-01_rb,9.358717e-01_rb,9.335446e-01_rb,9.312456e-01_rb,      &
         9.289731e-01_rb,9.267255e-01_rb,9.245010e-01_rb,9.222980e-01_rb,      &
         9.201147e-01_rb,9.179496e-01_rb,9.158008e-01_rb,9.136667e-01_rb,      &
         9.115457e-01_rb,9.094359e-01_rb,9.073358e-01_rb,9.052436e-01_rb,      &
         9.031577e-01_rb,9.010763e-01_rb,8.989977e-01_rb,8.969203e-01_rb,      &
         8.948423e-01_rb,8.927620e-01_rb,8.906778e-01_rb,8.885879e-01_rb,      &
         8.864907e-01_rb,8.843843e-01_rb /)
   ssaice3(:, 21) = (/                                                         &
! band 21
         9.934014e-01_rb,9.899331e-01_rb,9.865537e-01_rb,9.832610e-01_rb,      &
         9.800523e-01_rb,9.769254e-01_rb,9.738777e-01_rb,9.709069e-01_rb,      &
         9.680106e-01_rb,9.651862e-01_rb,9.624315e-01_rb,9.597439e-01_rb,      &
         9.571212e-01_rb,9.545608e-01_rb,9.520605e-01_rb,9.496177e-01_rb,      &
         9.472301e-01_rb,9.448954e-01_rb,9.426111e-01_rb,9.403749e-01_rb,      &
         9.381843e-01_rb,9.360370e-01_rb,9.339307e-01_rb,9.318629e-01_rb,      &
         9.298313e-01_rb,9.278336e-01_rb,9.258673e-01_rb,9.239302e-01_rb,      &
         9.220198e-01_rb,9.201338e-01_rb,9.182700e-01_rb,9.164258e-01_rb,      &
         9.145991e-01_rb,9.127874e-01_rb,9.109884e-01_rb,9.091999e-01_rb,      &
         9.074194e-01_rb,9.056447e-01_rb,9.038735e-01_rb,9.021033e-01_rb,      &
         9.003320e-01_rb,8.985572e-01_rb,8.967766e-01_rb,8.949879e-01_rb,      &
         8.931888e-01_rb,8.913770e-01_rb /)
   ssaice3(:, 22) = (/                                                         &
! band 22
         9.994833e-01_rb,9.992055e-01_rb,9.989278e-01_rb,9.986500e-01_rb,      &
         9.983724e-01_rb,9.980947e-01_rb,9.978172e-01_rb,9.975397e-01_rb,      &
         9.972623e-01_rb,9.969849e-01_rb,9.967077e-01_rb,9.964305e-01_rb,      &
         9.961535e-01_rb,9.958765e-01_rb,9.955997e-01_rb,9.953230e-01_rb,      &
         9.950464e-01_rb,9.947699e-01_rb,9.944936e-01_rb,9.942174e-01_rb,      &
         9.939414e-01_rb,9.936656e-01_rb,9.933899e-01_rb,9.931144e-01_rb,      &
         9.928390e-01_rb,9.925639e-01_rb,9.922889e-01_rb,9.920141e-01_rb,      &
         9.917396e-01_rb,9.914652e-01_rb,9.911911e-01_rb,9.909171e-01_rb,      &
         9.906434e-01_rb,9.903700e-01_rb,9.900967e-01_rb,9.898237e-01_rb,      &
         9.895510e-01_rb,9.892784e-01_rb,9.890062e-01_rb,9.887342e-01_rb,      &
         9.884625e-01_rb,9.881911e-01_rb,9.879199e-01_rb,9.876490e-01_rb,      &
         9.873784e-01_rb,9.871081e-01_rb /)
   ssaice3(:, 23) = (/                                                         &
! band 23
         9.999343e-01_rb,9.998917e-01_rb,9.998492e-01_rb,9.998067e-01_rb,      &
         9.997642e-01_rb,9.997218e-01_rb,9.996795e-01_rb,9.996372e-01_rb,      &
         9.995949e-01_rb,9.995528e-01_rb,9.995106e-01_rb,9.994686e-01_rb,      &
         9.994265e-01_rb,9.993845e-01_rb,9.993426e-01_rb,9.993007e-01_rb,      &
         9.992589e-01_rb,9.992171e-01_rb,9.991754e-01_rb,9.991337e-01_rb,      &
         9.990921e-01_rb,9.990505e-01_rb,9.990089e-01_rb,9.989674e-01_rb,      &
         9.989260e-01_rb,9.988846e-01_rb,9.988432e-01_rb,9.988019e-01_rb,      &
         9.987606e-01_rb,9.987194e-01_rb,9.986782e-01_rb,9.986370e-01_rb,      &
         9.985959e-01_rb,9.985549e-01_rb,9.985139e-01_rb,9.984729e-01_rb,      &
         9.984319e-01_rb,9.983910e-01_rb,9.983502e-01_rb,9.983094e-01_rb,      &
         9.982686e-01_rb,9.982279e-01_rb,9.981872e-01_rb,9.981465e-01_rb,      &
         9.981059e-01_rb,9.980653e-01_rb /)
   ssaice3(:, 24) = (/                                                         &
! band 24
         9.999978e-01_rb,9.999965e-01_rb,9.999952e-01_rb,9.999939e-01_rb,      &
         9.999926e-01_rb,9.999913e-01_rb,9.999900e-01_rb,9.999887e-01_rb,      &
         9.999873e-01_rb,9.999860e-01_rb,9.999847e-01_rb,9.999834e-01_rb,      &
         9.999821e-01_rb,9.999808e-01_rb,9.999795e-01_rb,9.999782e-01_rb,      &
         9.999769e-01_rb,9.999756e-01_rb,9.999743e-01_rb,9.999730e-01_rb,      &
         9.999717e-01_rb,9.999704e-01_rb,9.999691e-01_rb,9.999678e-01_rb,      &
         9.999665e-01_rb,9.999652e-01_rb,9.999639e-01_rb,9.999626e-01_rb,      &
         9.999613e-01_rb,9.999600e-01_rb,9.999587e-01_rb,9.999574e-01_rb,      &
         9.999561e-01_rb,9.999548e-01_rb,9.999535e-01_rb,9.999522e-01_rb,      &
         9.999509e-01_rb,9.999496e-01_rb,9.999483e-01_rb,9.999470e-01_rb,      &
         9.999457e-01_rb,9.999444e-01_rb,9.999431e-01_rb,9.999418e-01_rb,      &
         9.999405e-01_rb,9.999392e-01_rb /)
   ssaice3(:, 25) = (/                                                         &
! band 25
         9.999994e-01_rb,9.999993e-01_rb,9.999991e-01_rb,9.999990e-01_rb,      &
         9.999989e-01_rb,9.999987e-01_rb,9.999986e-01_rb,9.999984e-01_rb,      &
         9.999983e-01_rb,9.999982e-01_rb,9.999980e-01_rb,9.999979e-01_rb,      &
         9.999977e-01_rb,9.999976e-01_rb,9.999975e-01_rb,9.999973e-01_rb,      &
         9.999972e-01_rb,9.999970e-01_rb,9.999969e-01_rb,9.999967e-01_rb,      &
         9.999966e-01_rb,9.999965e-01_rb,9.999963e-01_rb,9.999962e-01_rb,      &
         9.999960e-01_rb,9.999959e-01_rb,9.999957e-01_rb,9.999956e-01_rb,      &
         9.999954e-01_rb,9.999953e-01_rb,9.999952e-01_rb,9.999950e-01_rb,      &
         9.999949e-01_rb,9.999947e-01_rb,9.999946e-01_rb,9.999944e-01_rb,      &
         9.999943e-01_rb,9.999941e-01_rb,9.999940e-01_rb,9.999939e-01_rb,      &
         9.999937e-01_rb,9.999936e-01_rb,9.999934e-01_rb,9.999933e-01_rb,      &
         9.999931e-01_rb,9.999930e-01_rb /)
   ssaice3(:, 26) = (/                                                         &
! band 26
         9.999997e-01_rb,9.999995e-01_rb,9.999992e-01_rb,9.999990e-01_rb,      &
         9.999987e-01_rb,9.999985e-01_rb,9.999983e-01_rb,9.999980e-01_rb,      &
         9.999978e-01_rb,9.999976e-01_rb,9.999973e-01_rb,9.999971e-01_rb,      &
         9.999969e-01_rb,9.999967e-01_rb,9.999965e-01_rb,9.999963e-01_rb,      &
         9.999960e-01_rb,9.999958e-01_rb,9.999956e-01_rb,9.999954e-01_rb,      &
         9.999952e-01_rb,9.999950e-01_rb,9.999948e-01_rb,9.999946e-01_rb,      &
         9.999944e-01_rb,9.999942e-01_rb,9.999939e-01_rb,9.999937e-01_rb,      &
         9.999935e-01_rb,9.999933e-01_rb,9.999931e-01_rb,9.999929e-01_rb,      &
         9.999927e-01_rb,9.999925e-01_rb,9.999923e-01_rb,9.999920e-01_rb,      &
         9.999918e-01_rb,9.999916e-01_rb,9.999914e-01_rb,9.999911e-01_rb,      &
         9.999909e-01_rb,9.999907e-01_rb,9.999905e-01_rb,9.999902e-01_rb,      &
         9.999900e-01_rb,9.999897e-01_rb /)
   ssaice3(:, 27) = (/                                                         &
! band 27
         9.999991e-01_rb,9.999985e-01_rb,9.999980e-01_rb,9.999974e-01_rb,      &
         9.999968e-01_rb,9.999963e-01_rb,9.999957e-01_rb,9.999951e-01_rb,      &
         9.999946e-01_rb,9.999940e-01_rb,9.999934e-01_rb,9.999929e-01_rb,      &
         9.999923e-01_rb,9.999918e-01_rb,9.999912e-01_rb,9.999907e-01_rb,      &
         9.999901e-01_rb,9.999896e-01_rb,9.999891e-01_rb,9.999885e-01_rb,      &
         9.999880e-01_rb,9.999874e-01_rb,9.999869e-01_rb,9.999863e-01_rb,      &
         9.999858e-01_rb,9.999853e-01_rb,9.999847e-01_rb,9.999842e-01_rb,      &
         9.999836e-01_rb,9.999831e-01_rb,9.999826e-01_rb,9.999820e-01_rb,      &
         9.999815e-01_rb,9.999809e-01_rb,9.999804e-01_rb,9.999798e-01_rb,      &
         9.999793e-01_rb,9.999787e-01_rb,9.999782e-01_rb,9.999776e-01_rb,      &
         9.999770e-01_rb,9.999765e-01_rb,9.999759e-01_rb,9.999754e-01_rb,      &
         9.999748e-01_rb,9.999742e-01_rb /)
   ssaice3(:, 28) = (/                                                         &
! band 28
         9.999975e-01_rb,9.999961e-01_rb,9.999946e-01_rb,9.999931e-01_rb,      &
         9.999917e-01_rb,9.999903e-01_rb,9.999888e-01_rb,9.999874e-01_rb,      &
         9.999859e-01_rb,9.999845e-01_rb,9.999831e-01_rb,9.999816e-01_rb,      &
         9.999802e-01_rb,9.999788e-01_rb,9.999774e-01_rb,9.999759e-01_rb,      &
         9.999745e-01_rb,9.999731e-01_rb,9.999717e-01_rb,9.999702e-01_rb,      &
         9.999688e-01_rb,9.999674e-01_rb,9.999660e-01_rb,9.999646e-01_rb,      &
         9.999631e-01_rb,9.999617e-01_rb,9.999603e-01_rb,9.999589e-01_rb,      &
         9.999574e-01_rb,9.999560e-01_rb,9.999546e-01_rb,9.999532e-01_rb,      &
         9.999517e-01_rb,9.999503e-01_rb,9.999489e-01_rb,9.999474e-01_rb,      &
         9.999460e-01_rb,9.999446e-01_rb,9.999431e-01_rb,9.999417e-01_rb,      &
         9.999403e-01_rb,9.999388e-01_rb,9.999374e-01_rb,9.999359e-01_rb,      &
         9.999345e-01_rb,9.999330e-01_rb /)
   ssaice3(:, 29) = (/                                                         &
! band 29
         4.526500e-01_rb,5.287890e-01_rb,5.410487e-01_rb,5.459865e-01_rb,      &
         5.485149e-01_rb,5.498914e-01_rb,5.505895e-01_rb,5.508310e-01_rb,      &
         5.507364e-01_rb,5.503793e-01_rb,5.498090e-01_rb,5.490612e-01_rb,      &
         5.481637e-01_rb,5.471395e-01_rb,5.460083e-01_rb,5.447878e-01_rb,      &
         5.434946e-01_rb,5.421442e-01_rb,5.407514e-01_rb,5.393309e-01_rb,      &
         5.378970e-01_rb,5.364641e-01_rb,5.350464e-01_rb,5.336582e-01_rb,      &
         5.323140e-01_rb,5.310283e-01_rb,5.298158e-01_rb,5.286914e-01_rb,      &
         5.276704e-01_rb,5.267680e-01_rb,5.260000e-01_rb,5.253823e-01_rb,      &
         5.249311e-01_rb,5.246629e-01_rb,5.245946e-01_rb,5.247434e-01_rb,      &
         5.251268e-01_rb,5.257626e-01_rb,5.266693e-01_rb,5.278653e-01_rb,      &
         5.293698e-01_rb,5.312022e-01_rb,5.333823e-01_rb,5.359305e-01_rb,      &
         5.388676e-01_rb,5.422146e-01_rb /)
!
! asymmetry factor: unitless
!
   asyice3(:, 16) = (/                                                         &
! band 16
         8.340752e-01_rb,8.435170e-01_rb,8.517487e-01_rb,8.592064e-01_rb,      &
         8.660387e-01_rb,8.723204e-01_rb,8.780997e-01_rb,8.834137e-01_rb,      &
         8.882934e-01_rb,8.927662e-01_rb,8.968577e-01_rb,9.005914e-01_rb,      &
         9.039899e-01_rb,9.070745e-01_rb,9.098659e-01_rb,9.123836e-01_rb,      &
         9.146466e-01_rb,9.166734e-01_rb,9.184817e-01_rb,9.200886e-01_rb,      &
         9.215109e-01_rb,9.227648e-01_rb,9.238661e-01_rb,9.248304e-01_rb,      &
         9.256727e-01_rb,9.264078e-01_rb,9.270505e-01_rb,9.276150e-01_rb,      &
         9.281156e-01_rb,9.285662e-01_rb,9.289806e-01_rb,9.293726e-01_rb,      &
         9.297557e-01_rb,9.301435e-01_rb,9.305491e-01_rb,9.309859e-01_rb,      &
         9.314671e-01_rb,9.320055e-01_rb,9.326140e-01_rb,9.333053e-01_rb,      &
         9.340919e-01_rb,9.349861e-01_rb,9.360000e-01_rb,9.371451e-01_rb,      &
         9.384329e-01_rb,9.398744e-01_rb /)
   asyice3(:, 17) = (/                                                         &
! band 17
         8.728160e-01_rb,8.777333e-01_rb,8.823754e-01_rb,8.867535e-01_rb,      &
         8.908785e-01_rb,8.947611e-01_rb,8.984118e-01_rb,9.018408e-01_rb,      &
         9.050582e-01_rb,9.080739e-01_rb,9.108976e-01_rb,9.135388e-01_rb,      &
         9.160068e-01_rb,9.183106e-01_rb,9.204595e-01_rb,9.224620e-01_rb,      &
         9.243271e-01_rb,9.260632e-01_rb,9.276788e-01_rb,9.291822e-01_rb,      &
         9.305817e-01_rb,9.318853e-01_rb,9.331012e-01_rb,9.342372e-01_rb,      &
         9.353013e-01_rb,9.363013e-01_rb,9.372450e-01_rb,9.381400e-01_rb,      &
         9.389939e-01_rb,9.398145e-01_rb,9.406092e-01_rb,9.413856e-01_rb,      &
         9.421511e-01_rb,9.429131e-01_rb,9.436790e-01_rb,9.444561e-01_rb,      &
         9.452517e-01_rb,9.460729e-01_rb,9.469270e-01_rb,9.478209e-01_rb,      &
         9.487617e-01_rb,9.497562e-01_rb,9.508112e-01_rb,9.519335e-01_rb,      &
         9.531294e-01_rb,9.544055e-01_rb /)
   asyice3(:, 18) = (/                                                         &
! band 18
         7.897566e-01_rb,7.948704e-01_rb,7.998041e-01_rb,8.045623e-01_rb,      &
         8.091495e-01_rb,8.135702e-01_rb,8.178290e-01_rb,8.219305e-01_rb,      &
         8.258790e-01_rb,8.296792e-01_rb,8.333355e-01_rb,8.368524e-01_rb,      &
         8.402343e-01_rb,8.434856e-01_rb,8.466108e-01_rb,8.496143e-01_rb,      &
         8.525004e-01_rb,8.552737e-01_rb,8.579384e-01_rb,8.604990e-01_rb,      &
         8.629597e-01_rb,8.653250e-01_rb,8.675992e-01_rb,8.697867e-01_rb,      &
         8.718916e-01_rb,8.739185e-01_rb,8.758715e-01_rb,8.777551e-01_rb,      &
         8.795734e-01_rb,8.813308e-01_rb,8.830315e-01_rb,8.846799e-01_rb,      &
         8.862802e-01_rb,8.878366e-01_rb,8.893534e-01_rb,8.908350e-01_rb,      &
         8.922854e-01_rb,8.937090e-01_rb,8.951099e-01_rb,8.964925e-01_rb,      &
         8.978609e-01_rb,8.992192e-01_rb,9.005718e-01_rb,9.019229e-01_rb,      &
         9.032765e-01_rb,9.046369e-01_rb /)
   asyice3(:, 19) = (/                                                         &
! band 19
         7.812615e-01_rb,7.887764e-01_rb,7.959664e-01_rb,8.028413e-01_rb,      &
         8.094109e-01_rb,8.156849e-01_rb,8.216730e-01_rb,8.273846e-01_rb,      &
         8.328294e-01_rb,8.380166e-01_rb,8.429556e-01_rb,8.476556e-01_rb,      &
         8.521258e-01_rb,8.563753e-01_rb,8.604131e-01_rb,8.642481e-01_rb,      &
         8.678893e-01_rb,8.713455e-01_rb,8.746254e-01_rb,8.777378e-01_rb,      &
         8.806914e-01_rb,8.834948e-01_rb,8.861566e-01_rb,8.886854e-01_rb,      &
         8.910897e-01_rb,8.933779e-01_rb,8.955586e-01_rb,8.976402e-01_rb,      &
         8.996311e-01_rb,9.015398e-01_rb,9.033745e-01_rb,9.051436e-01_rb,      &
         9.068555e-01_rb,9.085185e-01_rb,9.101410e-01_rb,9.117311e-01_rb,      &
         9.132972e-01_rb,9.148476e-01_rb,9.163905e-01_rb,9.179340e-01_rb,      &
         9.194864e-01_rb,9.210559e-01_rb,9.226505e-01_rb,9.242784e-01_rb,      &
         9.259476e-01_rb,9.276661e-01_rb /)
   asyice3(:, 20) = (/                                                         &
! band 20
         7.640720e-01_rb,7.691119e-01_rb,7.739941e-01_rb,7.787222e-01_rb,      &
         7.832998e-01_rb,7.877304e-01_rb,7.920177e-01_rb,7.961652e-01_rb,      &
         8.001765e-01_rb,8.040551e-01_rb,8.078044e-01_rb,8.114280e-01_rb,      &
         8.149294e-01_rb,8.183119e-01_rb,8.215791e-01_rb,8.247344e-01_rb,      &
         8.277812e-01_rb,8.307229e-01_rb,8.335629e-01_rb,8.363046e-01_rb,      &
         8.389514e-01_rb,8.415067e-01_rb,8.439738e-01_rb,8.463560e-01_rb,      &
         8.486568e-01_rb,8.508795e-01_rb,8.530274e-01_rb,8.551039e-01_rb,      &
         8.571122e-01_rb,8.590558e-01_rb,8.609378e-01_rb,8.627618e-01_rb,      &
         8.645309e-01_rb,8.662485e-01_rb,8.679178e-01_rb,8.695423e-01_rb,      &
         8.711251e-01_rb,8.726697e-01_rb,8.741792e-01_rb,8.756571e-01_rb,      &
         8.771065e-01_rb,8.785307e-01_rb,8.799331e-01_rb,8.813169e-01_rb,      &
         8.826854e-01_rb,8.840419e-01_rb /)
   asyice3(:, 21) = (/                                                         &
! band 21
         7.602598e-01_rb,7.651572e-01_rb,7.699014e-01_rb,7.744962e-01_rb,      &
         7.789452e-01_rb,7.832522e-01_rb,7.874205e-01_rb,7.914538e-01_rb,      &
         7.953555e-01_rb,7.991290e-01_rb,8.027777e-01_rb,8.063049e-01_rb,      &
         8.097140e-01_rb,8.130081e-01_rb,8.161906e-01_rb,8.192645e-01_rb,      &
         8.222331e-01_rb,8.250993e-01_rb,8.278664e-01_rb,8.305374e-01_rb,      &
         8.331153e-01_rb,8.356030e-01_rb,8.380037e-01_rb,8.403201e-01_rb,      &
         8.425553e-01_rb,8.447121e-01_rb,8.467935e-01_rb,8.488022e-01_rb,      &
         8.507412e-01_rb,8.526132e-01_rb,8.544210e-01_rb,8.561675e-01_rb,      &
         8.578554e-01_rb,8.594875e-01_rb,8.610665e-01_rb,8.625951e-01_rb,      &
         8.640760e-01_rb,8.655119e-01_rb,8.669055e-01_rb,8.682594e-01_rb,      &
         8.695763e-01_rb,8.708587e-01_rb,8.721094e-01_rb,8.733308e-01_rb,      &
         8.745255e-01_rb,8.756961e-01_rb /)      
   asyice3(:, 22) = (/                                                         &
! band 22
         7.568957e-01_rb,7.606995e-01_rb,7.644072e-01_rb,7.680204e-01_rb,      &
         7.715402e-01_rb,7.749682e-01_rb,7.783057e-01_rb,7.815541e-01_rb,      &
         7.847148e-01_rb,7.877892e-01_rb,7.907786e-01_rb,7.936846e-01_rb,      &
         7.965084e-01_rb,7.992515e-01_rb,8.019153e-01_rb,8.045011e-01_rb,      &
         8.070103e-01_rb,8.094444e-01_rb,8.118048e-01_rb,8.140927e-01_rb,      &
         8.163097e-01_rb,8.184571e-01_rb,8.205364e-01_rb,8.225488e-01_rb,      &
         8.244958e-01_rb,8.263789e-01_rb,8.281993e-01_rb,8.299586e-01_rb,      &
         8.316580e-01_rb,8.332991e-01_rb,8.348831e-01_rb,8.364115e-01_rb,      &
         8.378857e-01_rb,8.393071e-01_rb,8.406770e-01_rb,8.419969e-01_rb,      &
         8.432682e-01_rb,8.444923e-01_rb,8.456706e-01_rb,8.468044e-01_rb,      &
         8.478952e-01_rb,8.489444e-01_rb,8.499533e-01_rb,8.509234e-01_rb,      &
         8.518561e-01_rb,8.527528e-01_rb /)
   asyice3(:, 23) = (/                                                         &
! band 23
         7.575066e-01_rb,7.606912e-01_rb,7.638236e-01_rb,7.669035e-01_rb,      &
         7.699306e-01_rb,7.729046e-01_rb,7.758254e-01_rb,7.786926e-01_rb,      &
         7.815060e-01_rb,7.842654e-01_rb,7.869705e-01_rb,7.896211e-01_rb,      &
         7.922168e-01_rb,7.947574e-01_rb,7.972428e-01_rb,7.996726e-01_rb,      &
         8.020466e-01_rb,8.043646e-01_rb,8.066262e-01_rb,8.088313e-01_rb,      &
         8.109796e-01_rb,8.130709e-01_rb,8.151049e-01_rb,8.170814e-01_rb,      &
         8.190001e-01_rb,8.208608e-01_rb,8.226632e-01_rb,8.244071e-01_rb,      &
         8.260924e-01_rb,8.277186e-01_rb,8.292856e-01_rb,8.307932e-01_rb,      &
         8.322411e-01_rb,8.336291e-01_rb,8.349570e-01_rb,8.362244e-01_rb,      &
         8.374312e-01_rb,8.385772e-01_rb,8.396621e-01_rb,8.406856e-01_rb,      &
         8.416476e-01_rb,8.425479e-01_rb,8.433861e-01_rb,8.441620e-01_rb,      &
         8.448755e-01_rb,8.455263e-01_rb /)
   asyice3(:, 24) = (/                                                         &
! band 24
         7.568829e-01_rb,7.597947e-01_rb,7.626745e-01_rb,7.655212e-01_rb,      &
         7.683337e-01_rb,7.711111e-01_rb,7.738523e-01_rb,7.765565e-01_rb,      &
         7.792225e-01_rb,7.818494e-01_rb,7.844362e-01_rb,7.869819e-01_rb,      &
         7.894854e-01_rb,7.919459e-01_rb,7.943623e-01_rb,7.967337e-01_rb,      &
         7.990590e-01_rb,8.013373e-01_rb,8.035676e-01_rb,8.057488e-01_rb,      &
         8.078802e-01_rb,8.099605e-01_rb,8.119890e-01_rb,8.139645e-01_rb,      &
         8.158862e-01_rb,8.177530e-01_rb,8.195641e-01_rb,8.213183e-01_rb,      &
         8.230149e-01_rb,8.246527e-01_rb,8.262308e-01_rb,8.277483e-01_rb,      &
         8.292042e-01_rb,8.305976e-01_rb,8.319275e-01_rb,8.331929e-01_rb,      &
         8.343929e-01_rb,8.355265e-01_rb,8.365928e-01_rb,8.375909e-01_rb,      &
         8.385197e-01_rb,8.393784e-01_rb,8.401659e-01_rb,8.408815e-01_rb,      &
         8.415240e-01_rb,8.420926e-01_rb /)
   asyice3(:, 25) = (/                                                         &
! band 25
         7.548616e-01_rb,7.575454e-01_rb,7.602153e-01_rb,7.628696e-01_rb,      &
         7.655067e-01_rb,7.681249e-01_rb,7.707225e-01_rb,7.732978e-01_rb,      &
         7.758492e-01_rb,7.783750e-01_rb,7.808735e-01_rb,7.833430e-01_rb,      &
         7.857819e-01_rb,7.881886e-01_rb,7.905612e-01_rb,7.928983e-01_rb,      &
         7.951980e-01_rb,7.974588e-01_rb,7.996789e-01_rb,8.018567e-01_rb,      &
         8.039905e-01_rb,8.060787e-01_rb,8.081196e-01_rb,8.101115e-01_rb,      &
         8.120527e-01_rb,8.139416e-01_rb,8.157764e-01_rb,8.175557e-01_rb,      &
         8.192776e-01_rb,8.209405e-01_rb,8.225427e-01_rb,8.240826e-01_rb,      &
         8.255585e-01_rb,8.269688e-01_rb,8.283117e-01_rb,8.295856e-01_rb,      &
         8.307889e-01_rb,8.319198e-01_rb,8.329767e-01_rb,8.339579e-01_rb,      &
         8.348619e-01_rb,8.356868e-01_rb,8.364311e-01_rb,8.370930e-01_rb,      &
         8.376710e-01_rb,8.381633e-01_rb /)
   asyice3(:, 26) = (/                                                         &
! band 26
         7.491854e-01_rb,7.518523e-01_rb,7.545089e-01_rb,7.571534e-01_rb,      &
         7.597839e-01_rb,7.623987e-01_rb,7.649959e-01_rb,7.675737e-01_rb,      &
         7.701303e-01_rb,7.726639e-01_rb,7.751727e-01_rb,7.776548e-01_rb,      &
         7.801084e-01_rb,7.825318e-01_rb,7.849230e-01_rb,7.872804e-01_rb,      &
         7.896020e-01_rb,7.918862e-01_rb,7.941309e-01_rb,7.963345e-01_rb,      &
         7.984951e-01_rb,8.006109e-01_rb,8.026802e-01_rb,8.047009e-01_rb,      &
         8.066715e-01_rb,8.085900e-01_rb,8.104546e-01_rb,8.122636e-01_rb,      &
         8.140150e-01_rb,8.157072e-01_rb,8.173382e-01_rb,8.189063e-01_rb,      &
         8.204096e-01_rb,8.218464e-01_rb,8.232148e-01_rb,8.245130e-01_rb,      &
         8.257391e-01_rb,8.268915e-01_rb,8.279682e-01_rb,8.289675e-01_rb,      &
         8.298875e-01_rb,8.307264e-01_rb,8.314824e-01_rb,8.321537e-01_rb,      &
         8.327385e-01_rb,8.332350e-01_rb /)
   asyice3(:, 27) = (/                                                         &
! band 27
         7.397086e-01_rb,7.424069e-01_rb,7.450955e-01_rb,7.477725e-01_rb,      &
         7.504362e-01_rb,7.530846e-01_rb,7.557159e-01_rb,7.583283e-01_rb,      &
         7.609199e-01_rb,7.634888e-01_rb,7.660332e-01_rb,7.685512e-01_rb,      &
         7.710411e-01_rb,7.735009e-01_rb,7.759288e-01_rb,7.783229e-01_rb,      &
         7.806814e-01_rb,7.830024e-01_rb,7.852841e-01_rb,7.875246e-01_rb,      &
         7.897221e-01_rb,7.918748e-01_rb,7.939807e-01_rb,7.960380e-01_rb,      &
         7.980449e-01_rb,7.999995e-01_rb,8.019000e-01_rb,8.037445e-01_rb,      &
         8.055311e-01_rb,8.072581e-01_rb,8.089235e-01_rb,8.105255e-01_rb,      &
         8.120623e-01_rb,8.135319e-01_rb,8.149326e-01_rb,8.162626e-01_rb,      &
         8.175198e-01_rb,8.187025e-01_rb,8.198089e-01_rb,8.208371e-01_rb,      &
         8.217852e-01_rb,8.226514e-01_rb,8.234338e-01_rb,8.241306e-01_rb,      &
         8.247399e-01_rb,8.252599e-01_rb /)
   asyice3(:, 28) = (/                                                         &
! band 28
         7.224533e-01_rb,7.251681e-01_rb,7.278728e-01_rb,7.305654e-01_rb,      &
         7.332444e-01_rb,7.359078e-01_rb,7.385539e-01_rb,7.411808e-01_rb,      &
         7.437869e-01_rb,7.463702e-01_rb,7.489291e-01_rb,7.514616e-01_rb,      &
         7.539661e-01_rb,7.564408e-01_rb,7.588837e-01_rb,7.612933e-01_rb,      &
         7.636676e-01_rb,7.660049e-01_rb,7.683034e-01_rb,7.705612e-01_rb,      &
         7.727767e-01_rb,7.749480e-01_rb,7.770733e-01_rb,7.791509e-01_rb,      &
         7.811789e-01_rb,7.831556e-01_rb,7.850791e-01_rb,7.869478e-01_rb,      &
         7.887597e-01_rb,7.905131e-01_rb,7.922062e-01_rb,7.938372e-01_rb,      &
         7.954044e-01_rb,7.969059e-01_rb,7.983399e-01_rb,7.997047e-01_rb,      &
         8.009985e-01_rb,8.022195e-01_rb,8.033658e-01_rb,8.044357e-01_rb,      &
         8.054275e-01_rb,8.063392e-01_rb,8.071692e-01_rb,8.079157e-01_rb,      &
         8.085768e-01_rb,8.091507e-01_rb /)
   asyice3(:, 29) = (/                                                         &
! band 29
         8.850026e-01_rb,9.005489e-01_rb,9.069242e-01_rb,9.121799e-01_rb,      &
         9.168987e-01_rb,9.212259e-01_rb,9.252176e-01_rb,9.289028e-01_rb,      &
         9.323000e-01_rb,9.354235e-01_rb,9.382858e-01_rb,9.408985e-01_rb,      &
         9.432734e-01_rb,9.454218e-01_rb,9.473557e-01_rb,9.490871e-01_rb,      &
         9.506282e-01_rb,9.519917e-01_rb,9.531904e-01_rb,9.542374e-01_rb,      &
         9.551461e-01_rb,9.559298e-01_rb,9.566023e-01_rb,9.571775e-01_rb,      &
         9.576692e-01_rb,9.580916e-01_rb,9.584589e-01_rb,9.587853e-01_rb,      &
         9.590851e-01_rb,9.593729e-01_rb,9.596632e-01_rb,9.599705e-01_rb,      &
         9.603096e-01_rb,9.606954e-01_rb,9.611427e-01_rb,9.616667e-01_rb,      &
         9.622826e-01_rb,9.630060e-01_rb,9.638524e-01_rb,9.648379e-01_rb,      &
         9.659788e-01_rb,9.672916e-01_rb,9.687933e-01_rb,9.705014e-01_rb,      &
         9.724337e-01_rb,9.746084e-01_rb /)
!
! fdelta: unitless
!
   fdlice3(:, 16) = (/                                                         &
! band 16
         4.959277e-02_rb,4.685292e-02_rb,4.426104e-02_rb,4.181231e-02_rb,      &
         3.950191e-02_rb,3.732500e-02_rb,3.527675e-02_rb,3.335235e-02_rb,      &
         3.154697e-02_rb,2.985578e-02_rb,2.827395e-02_rb,2.679666e-02_rb,      &
         2.541909e-02_rb,2.413640e-02_rb,2.294378e-02_rb,2.183639e-02_rb,      &
         2.080940e-02_rb,1.985801e-02_rb,1.897736e-02_rb,1.816265e-02_rb,      &
         1.740905e-02_rb,1.671172e-02_rb,1.606585e-02_rb,1.546661e-02_rb,      &
         1.490917e-02_rb,1.438870e-02_rb,1.390038e-02_rb,1.343939e-02_rb,      &
         1.300089e-02_rb,1.258006e-02_rb,1.217208e-02_rb,1.177212e-02_rb,      &
         1.137536e-02_rb,1.097696e-02_rb,1.057210e-02_rb,1.015596e-02_rb,      &
         9.723704e-03_rb,9.270516e-03_rb,8.791565e-03_rb,8.282026e-03_rb,      &
         7.737072e-03_rb,7.151879e-03_rb,6.521619e-03_rb,5.841467e-03_rb,      &
         5.106597e-03_rb,4.312183e-03_rb /)
   fdlice3(:, 17) = (/                                                         &
! band 17
         5.071224e-02_rb,5.000217e-02_rb,4.933872e-02_rb,4.871992e-02_rb,      &
         4.814380e-02_rb,4.760839e-02_rb,4.711170e-02_rb,4.665177e-02_rb,      &
         4.622662e-02_rb,4.583426e-02_rb,4.547274e-02_rb,4.514007e-02_rb,      &
         4.483428e-02_rb,4.455340e-02_rb,4.429544e-02_rb,4.405844e-02_rb,      &
         4.384041e-02_rb,4.363939e-02_rb,4.345340e-02_rb,4.328047e-02_rb,      &
         4.311861e-02_rb,4.296586e-02_rb,4.282024e-02_rb,4.267977e-02_rb,      &
         4.254248e-02_rb,4.240640e-02_rb,4.226955e-02_rb,4.212995e-02_rb,      &
         4.198564e-02_rb,4.183462e-02_rb,4.167494e-02_rb,4.150462e-02_rb,      &
         4.132167e-02_rb,4.112413e-02_rb,4.091003e-02_rb,4.067737e-02_rb,      &
         4.042420e-02_rb,4.014854e-02_rb,3.984840e-02_rb,3.952183e-02_rb,      &
         3.916683e-02_rb,3.878144e-02_rb,3.836368e-02_rb,3.791158e-02_rb,      &
         3.742316e-02_rb,3.689645e-02_rb /)
   fdlice3(:, 18) = (/                                                         &
! band 18
         1.062938e-01_rb,1.065234e-01_rb,1.067822e-01_rb,1.070682e-01_rb,      &
         1.073793e-01_rb,1.077137e-01_rb,1.080693e-01_rb,1.084442e-01_rb,      &
         1.088364e-01_rb,1.092439e-01_rb,1.096647e-01_rb,1.100970e-01_rb,      &
         1.105387e-01_rb,1.109878e-01_rb,1.114423e-01_rb,1.119004e-01_rb,      &
         1.123599e-01_rb,1.128190e-01_rb,1.132757e-01_rb,1.137279e-01_rb,      &
         1.141738e-01_rb,1.146113e-01_rb,1.150385e-01_rb,1.154534e-01_rb,      &
         1.158540e-01_rb,1.162383e-01_rb,1.166045e-01_rb,1.169504e-01_rb,      &
         1.172741e-01_rb,1.175738e-01_rb,1.178472e-01_rb,1.180926e-01_rb,      &
         1.183080e-01_rb,1.184913e-01_rb,1.186405e-01_rb,1.187538e-01_rb,      &
         1.188291e-01_rb,1.188645e-01_rb,1.188580e-01_rb,1.188076e-01_rb,      &
         1.187113e-01_rb,1.185672e-01_rb,1.183733e-01_rb,1.181277e-01_rb,      &
         1.178282e-01_rb,1.174731e-01_rb /)
   fdlice3(:, 19) = (/                                                         &
! band 19
         1.076195e-01_rb,1.065195e-01_rb,1.054696e-01_rb,1.044673e-01_rb,      &
         1.035099e-01_rb,1.025951e-01_rb,1.017203e-01_rb,1.008831e-01_rb,      &
         1.000808e-01_rb,9.931116e-02_rb,9.857151e-02_rb,9.785939e-02_rb,      &
         9.717230e-02_rb,9.650774e-02_rb,9.586322e-02_rb,9.523623e-02_rb,      &
         9.462427e-02_rb,9.402484e-02_rb,9.343544e-02_rb,9.285358e-02_rb,      &
         9.227675e-02_rb,9.170245e-02_rb,9.112818e-02_rb,9.055144e-02_rb,      &
         8.996974e-02_rb,8.938056e-02_rb,8.878142e-02_rb,8.816981e-02_rb,      &
         8.754323e-02_rb,8.689919e-02_rb,8.623517e-02_rb,8.554869e-02_rb,      &
         8.483724e-02_rb,8.409832e-02_rb,8.332943e-02_rb,8.252807e-02_rb,      &
         8.169175e-02_rb,8.081795e-02_rb,7.990419e-02_rb,7.894796e-02_rb,      &
         7.794676e-02_rb,7.689809e-02_rb,7.579945e-02_rb,7.464834e-02_rb,      &
         7.344227e-02_rb,7.217872e-02_rb /)
   fdlice3(:, 20) = (/                                                         &
! band 20
         1.119014e-01_rb,1.122706e-01_rb,1.126690e-01_rb,1.130947e-01_rb,      &
         1.135456e-01_rb,1.140199e-01_rb,1.145154e-01_rb,1.150302e-01_rb,      &
         1.155623e-01_rb,1.161096e-01_rb,1.166703e-01_rb,1.172422e-01_rb,      &
         1.178233e-01_rb,1.184118e-01_rb,1.190055e-01_rb,1.196025e-01_rb,      &
         1.202008e-01_rb,1.207983e-01_rb,1.213931e-01_rb,1.219832e-01_rb,      &
         1.225665e-01_rb,1.231411e-01_rb,1.237050e-01_rb,1.242561e-01_rb,      &
         1.247926e-01_rb,1.253122e-01_rb,1.258132e-01_rb,1.262934e-01_rb,      &
         1.267509e-01_rb,1.271836e-01_rb,1.275896e-01_rb,1.279669e-01_rb,      &
         1.283134e-01_rb,1.286272e-01_rb,1.289063e-01_rb,1.291486e-01_rb,      &
         1.293522e-01_rb,1.295150e-01_rb,1.296351e-01_rb,1.297104e-01_rb,      &
         1.297390e-01_rb,1.297189e-01_rb,1.296480e-01_rb,1.295244e-01_rb,      &
         1.293460e-01_rb,1.291109e-01_rb /)
   fdlice3(:, 21) = (/                                                         &
! band 21
         1.133298e-01_rb,1.136777e-01_rb,1.140556e-01_rb,1.144615e-01_rb,      &
         1.148934e-01_rb,1.153492e-01_rb,1.158269e-01_rb,1.163243e-01_rb,      &
         1.168396e-01_rb,1.173706e-01_rb,1.179152e-01_rb,1.184715e-01_rb,      &
         1.190374e-01_rb,1.196108e-01_rb,1.201897e-01_rb,1.207720e-01_rb,      &
         1.213558e-01_rb,1.219389e-01_rb,1.225194e-01_rb,1.230951e-01_rb,      &
         1.236640e-01_rb,1.242241e-01_rb,1.247733e-01_rb,1.253096e-01_rb,      &
         1.258309e-01_rb,1.263352e-01_rb,1.268205e-01_rb,1.272847e-01_rb,      &
         1.277257e-01_rb,1.281415e-01_rb,1.285300e-01_rb,1.288893e-01_rb,      &
         1.292173e-01_rb,1.295118e-01_rb,1.297710e-01_rb,1.299927e-01_rb,      &
         1.301748e-01_rb,1.303154e-01_rb,1.304124e-01_rb,1.304637e-01_rb,      &
         1.304673e-01_rb,1.304212e-01_rb,1.303233e-01_rb,1.301715e-01_rb,      &
         1.299638e-01_rb,1.296983e-01_rb /)
   fdlice3(:, 22) = (/                                                         &
! band 22
         1.145360e-01_rb,1.153256e-01_rb,1.161453e-01_rb,1.169929e-01_rb,      &
         1.178666e-01_rb,1.187641e-01_rb,1.196835e-01_rb,1.206227e-01_rb,      &
         1.215796e-01_rb,1.225522e-01_rb,1.235383e-01_rb,1.245361e-01_rb,      &
         1.255433e-01_rb,1.265579e-01_rb,1.275779e-01_rb,1.286011e-01_rb,      &
         1.296257e-01_rb,1.306494e-01_rb,1.316703e-01_rb,1.326862e-01_rb,      &
         1.336951e-01_rb,1.346950e-01_rb,1.356838e-01_rb,1.366594e-01_rb,      &
         1.376198e-01_rb,1.385629e-01_rb,1.394866e-01_rb,1.403889e-01_rb,      &
         1.412678e-01_rb,1.421212e-01_rb,1.429469e-01_rb,1.437430e-01_rb,      &
         1.445074e-01_rb,1.452381e-01_rb,1.459329e-01_rb,1.465899e-01_rb,      &
         1.472069e-01_rb,1.477819e-01_rb,1.483128e-01_rb,1.487976e-01_rb,      &
         1.492343e-01_rb,1.496207e-01_rb,1.499548e-01_rb,1.502346e-01_rb,      &
         1.504579e-01_rb,1.506227e-01_rb /)
   fdlice3(:, 23) = (/                                                         &
! band 23
         1.153263e-01_rb,1.161445e-01_rb,1.169932e-01_rb,1.178703e-01_rb,      &
         1.187738e-01_rb,1.197016e-01_rb,1.206516e-01_rb,1.216217e-01_rb,      &
         1.226099e-01_rb,1.236141e-01_rb,1.246322e-01_rb,1.256621e-01_rb,      &
         1.267017e-01_rb,1.277491e-01_rb,1.288020e-01_rb,1.298584e-01_rb,      &
         1.309163e-01_rb,1.319736e-01_rb,1.330281e-01_rb,1.340778e-01_rb,      &
         1.351207e-01_rb,1.361546e-01_rb,1.371775e-01_rb,1.381873e-01_rb,      &
         1.391820e-01_rb,1.401593e-01_rb,1.411174e-01_rb,1.420540e-01_rb,      &
         1.429671e-01_rb,1.438547e-01_rb,1.447146e-01_rb,1.455449e-01_rb,      &
         1.463433e-01_rb,1.471078e-01_rb,1.478364e-01_rb,1.485270e-01_rb,      &
         1.491774e-01_rb,1.497857e-01_rb,1.503497e-01_rb,1.508674e-01_rb,      &
         1.513367e-01_rb,1.517554e-01_rb,1.521216e-01_rb,1.524332e-01_rb,      &
         1.526880e-01_rb,1.528840e-01_rb /)
   fdlice3(:, 24) = (/                                                         &
! band 24
         1.160842e-01_rb,1.169118e-01_rb,1.177697e-01_rb,1.186556e-01_rb,      &
         1.195676e-01_rb,1.205036e-01_rb,1.214616e-01_rb,1.224394e-01_rb,      &
         1.234349e-01_rb,1.244463e-01_rb,1.254712e-01_rb,1.265078e-01_rb,      &
         1.275539e-01_rb,1.286075e-01_rb,1.296664e-01_rb,1.307287e-01_rb,      &
         1.317923e-01_rb,1.328550e-01_rb,1.339149e-01_rb,1.349699e-01_rb,      &
         1.360179e-01_rb,1.370567e-01_rb,1.380845e-01_rb,1.390991e-01_rb,      &
         1.400984e-01_rb,1.410803e-01_rb,1.420429e-01_rb,1.429840e-01_rb,      &
         1.439016e-01_rb,1.447936e-01_rb,1.456579e-01_rb,1.464925e-01_rb,      &
         1.472953e-01_rb,1.480642e-01_rb,1.487972e-01_rb,1.494923e-01_rb,      &
         1.501472e-01_rb,1.507601e-01_rb,1.513287e-01_rb,1.518511e-01_rb,      &
         1.523252e-01_rb,1.527489e-01_rb,1.531201e-01_rb,1.534368e-01_rb,      &
         1.536969e-01_rb,1.538984e-01_rb /)
   fdlice3(:, 25) = (/                                                         &
! band 25
         1.168725e-01_rb,1.177088e-01_rb,1.185747e-01_rb,1.194680e-01_rb,      &
         1.203867e-01_rb,1.213288e-01_rb,1.222923e-01_rb,1.232750e-01_rb,      &
         1.242750e-01_rb,1.252903e-01_rb,1.263187e-01_rb,1.273583e-01_rb,      &
         1.284069e-01_rb,1.294626e-01_rb,1.305233e-01_rb,1.315870e-01_rb,      &
         1.326517e-01_rb,1.337152e-01_rb,1.347756e-01_rb,1.358308e-01_rb,      &
         1.368788e-01_rb,1.379175e-01_rb,1.389449e-01_rb,1.399590e-01_rb,      &
         1.409577e-01_rb,1.419389e-01_rb,1.429007e-01_rb,1.438410e-01_rb,      &
         1.447577e-01_rb,1.456488e-01_rb,1.465123e-01_rb,1.473461e-01_rb,      &
         1.481483e-01_rb,1.489166e-01_rb,1.496492e-01_rb,1.503439e-01_rb,      &
         1.509988e-01_rb,1.516118e-01_rb,1.521808e-01_rb,1.527038e-01_rb,      &
         1.531788e-01_rb,1.536037e-01_rb,1.539764e-01_rb,1.542951e-01_rb,      &
         1.545575e-01_rb,1.547617e-01_rb /)      
   fdlice3(:, 26) = (/                                                         &
! band 26
         1.180509e-01_rb,1.189025e-01_rb,1.197820e-01_rb,1.206875e-01_rb,      &
         1.216171e-01_rb,1.225687e-01_rb,1.235404e-01_rb,1.245303e-01_rb,      &
         1.255363e-01_rb,1.265564e-01_rb,1.275888e-01_rb,1.286313e-01_rb,      &
         1.296821e-01_rb,1.307392e-01_rb,1.318006e-01_rb,1.328643e-01_rb,      &
         1.339284e-01_rb,1.349908e-01_rb,1.360497e-01_rb,1.371029e-01_rb,      &
         1.381486e-01_rb,1.391848e-01_rb,1.402095e-01_rb,1.412208e-01_rb,      &
         1.422165e-01_rb,1.431949e-01_rb,1.441539e-01_rb,1.450915e-01_rb,      &
         1.460058e-01_rb,1.468947e-01_rb,1.477564e-01_rb,1.485888e-01_rb,      &
         1.493900e-01_rb,1.501580e-01_rb,1.508907e-01_rb,1.515864e-01_rb,      &
         1.522428e-01_rb,1.528582e-01_rb,1.534305e-01_rb,1.539578e-01_rb,      &
         1.544380e-01_rb,1.548692e-01_rb,1.552494e-01_rb,1.555767e-01_rb,      &
         1.558490e-01_rb,1.560645e-01_rb /)
   fdlice3(:, 27) = (/                                                         &
! band 27
         1.200480e-01_rb,1.209267e-01_rb,1.218304e-01_rb,1.227575e-01_rb,      &
         1.237059e-01_rb,1.246739e-01_rb,1.256595e-01_rb,1.266610e-01_rb,      &
         1.276765e-01_rb,1.287041e-01_rb,1.297420e-01_rb,1.307883e-01_rb,      &
         1.318412e-01_rb,1.328988e-01_rb,1.339593e-01_rb,1.350207e-01_rb,      &
         1.360813e-01_rb,1.371393e-01_rb,1.381926e-01_rb,1.392396e-01_rb,      &
         1.402783e-01_rb,1.413069e-01_rb,1.423235e-01_rb,1.433263e-01_rb,      &
         1.443134e-01_rb,1.452830e-01_rb,1.462332e-01_rb,1.471622e-01_rb,      &
         1.480681e-01_rb,1.489490e-01_rb,1.498032e-01_rb,1.506286e-01_rb,      &
         1.514236e-01_rb,1.521863e-01_rb,1.529147e-01_rb,1.536070e-01_rb,      &
         1.542614e-01_rb,1.548761e-01_rb,1.554491e-01_rb,1.559787e-01_rb,      &
         1.564629e-01_rb,1.568999e-01_rb,1.572879e-01_rb,1.576249e-01_rb,      &
         1.579093e-01_rb,1.581390e-01_rb /)
   fdlice3(:, 28) = (/                                                         &
! band 28
         1.247813e-01_rb,1.256496e-01_rb,1.265417e-01_rb,1.274560e-01_rb,      &
         1.283905e-01_rb,1.293436e-01_rb,1.303135e-01_rb,1.312983e-01_rb,      &
         1.322964e-01_rb,1.333060e-01_rb,1.343252e-01_rb,1.353523e-01_rb,      &
         1.363855e-01_rb,1.374231e-01_rb,1.384632e-01_rb,1.395042e-01_rb,      &
         1.405441e-01_rb,1.415813e-01_rb,1.426140e-01_rb,1.436404e-01_rb,      &
         1.446587e-01_rb,1.456672e-01_rb,1.466640e-01_rb,1.476475e-01_rb,      &
         1.486157e-01_rb,1.495671e-01_rb,1.504997e-01_rb,1.514117e-01_rb,      &
         1.523016e-01_rb,1.531673e-01_rb,1.540073e-01_rb,1.548197e-01_rb,      &
         1.556026e-01_rb,1.563545e-01_rb,1.570734e-01_rb,1.577576e-01_rb,      &
         1.584054e-01_rb,1.590149e-01_rb,1.595843e-01_rb,1.601120e-01_rb,      &
         1.605962e-01_rb,1.610349e-01_rb,1.614266e-01_rb,1.617693e-01_rb,      &
         1.620614e-01_rb,1.623011e-01_rb /)
   fdlice3(:, 29) = (/                                                         &
! band 29
         1.006055e-01_rb,9.549582e-02_rb,9.063960e-02_rb,8.602900e-02_rb,      &
         8.165612e-02_rb,7.751308e-02_rb,7.359199e-02_rb,6.988496e-02_rb,      &
         6.638412e-02_rb,6.308156e-02_rb,5.996942e-02_rb,5.703979e-02_rb,      &
         5.428481e-02_rb,5.169657e-02_rb,4.926719e-02_rb,4.698880e-02_rb,      &
         4.485349e-02_rb,4.285339e-02_rb,4.098061e-02_rb,3.922727e-02_rb,      &
         3.758547e-02_rb,3.604733e-02_rb,3.460497e-02_rb,3.325051e-02_rb,      &
         3.197604e-02_rb,3.077369e-02_rb,2.963558e-02_rb,2.855381e-02_rb,      &
         2.752050e-02_rb,2.652776e-02_rb,2.556772e-02_rb,2.463247e-02_rb,      &
         2.371415e-02_rb,2.280485e-02_rb,2.189670e-02_rb,2.098180e-02_rb,      &
         2.005228e-02_rb,1.910024e-02_rb,1.811781e-02_rb,1.709709e-02_rb,      &
         1.603020e-02_rb,1.490925e-02_rb,1.372635e-02_rb,1.247363e-02_rb,      &
         1.114319e-02_rb,9.727157e-03_rb /)
!
   end subroutine swcldpr
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_init_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_vrtqdr_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only: im => kind_im, rb => kind_rb
!  use parrrsw, only: ngptsw
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine vrtqdr_sw(klev, kw,                                              &
                           pref, prefd, ptra, ptrad,                           &
                           pdbt, prdnd, prup, prupd, ptdbt,                    &
                           pfd, pfu)
!-------------------------------------------------------------------------------
! 
! abstract: 
!   This routine performs the vertical quadrature integration
!
! Interface:  *vrtqdr_sw* is called from *spcvrt_sw* and *spcvmc_sw*
!
! history log 
! 
!           H. Barker  Original
! 2002-10  J.-J. Morcrette, ECMWF   Integrated with rrtmg_sw
! 2006-06  MJIacono, AER            Reformatted for consistency with rrtmg_lw  
!
! Input : 
!   klev : number of model layers
!   kw : g-point index
!   pref(nlayers+1) : direct beam reflectivity
!   prefd(nlayers+1) : diffuse beam reflectivity
!   ptra(nlayers+1) : direct beam transmissivity
!   ptrad(nlayers+1) : diffuse beam transmissivity
!   pdbt(nlayers+1) : layer mean direct beam transmittance
!   ptdbt(nlayers+1) : total direct beam transmittance at levels
!   prdnd(nlayers+1)   
!   prup(nlayers+1)
!   prupd(nlayers+1)
!
! Output :
!   pfd(nlayers+1,ngptsw) : downwelling flux (W/m2), 
!                           unadjusted for earth/sun distance or zenith angle
!   pfu(nlayers+1,ngptsw) : upwelling flux (W/m2)
!                           unadjusted for earth/sun distance or zenith angle
!     
!-------------------------------------------------------------------------------
!
! ------- Declarations -------
!
! Input
!
   integer(kind=im),  intent(in   ) :: klev                
   integer(kind=im),  intent(in   ) :: kw                  
   real(kind=rb), dimension(:), intent(in   ) :: pref                 
   real(kind=rb), dimension(:), intent(in   ) :: prefd                
   real(kind=rb), dimension(:), intent(in   ) :: ptra                 
   real(kind=rb), dimension(:), intent(in   ) :: ptrad                
   real(kind=rb), dimension(:), intent(in   ) :: pdbt
   real(kind=rb), dimension(:), intent(in   ) :: ptdbt
   real(kind=rb), dimension(:), intent(inout) :: prdnd
   real(kind=rb), dimension(:), intent(inout) :: prup
   real(kind=rb), dimension(:), intent(inout) :: prupd
!
! Output
!
   real(kind=rb), dimension(:,:), intent(  out) :: pfd
   real(kind=rb), dimension(:,:), intent(  out) :: pfu       
!
! Local
!
   integer(kind=im) :: ikp, ikx, jk
!
   real(kind=rb) :: zreflect
   real(kind=rb),dimension(klev+1) :: ztdn  
!
!-----------------------------------------------------------------------------
!                   
! Link lowest layer with surface
!             
   zreflect = 1._rb / (1._rb - prefd(klev+1) * prefd(klev))
   prup(klev) = pref(klev) + (ptrad(klev) *                                    &
                 ((ptra(klev) - pdbt(klev)) * prefd(klev+1) +                  &
                   pdbt(klev) * pref(klev+1))) * zreflect
   prupd(klev) = prefd(klev) + ptrad(klev) * ptrad(klev) *                     &
                    prefd(klev+1) * zreflect
!
! Pass from bottom to top 
!
   do jk = 1,klev-1
     ikp = klev+1-jk                       
     ikx = ikp-1
     zreflect = 1._rb / (1._rb -prupd(ikp) * prefd(ikx))
     prup(ikx) = pref(ikx) + (ptrad(ikx) *                                     &
                   ((ptra(ikx) - pdbt(ikx)) * prupd(ikp) +                     &
                     pdbt(ikx) * prup(ikp))) * zreflect
     prupd(ikx) = prefd(ikx) + ptrad(ikx) * ptrad(ikx) *                       &
                      prupd(ikp) * zreflect
   enddo
!
! Upper boundary conditions
!
   ztdn(1) = 1._rb
   prdnd(1) = 0._rb
   ztdn(2) = ptra(1)
   prdnd(2) = prefd(1)
!
! Pass from top to bottom
!
   do jk = 2,klev
     ikp = jk+1
     zreflect = 1._rb / (1._rb - prefd(jk) * prdnd(jk))
     ztdn(ikp) = ptdbt(jk) * ptra(jk) +                                        &
                    (ptrad(jk) * ((ztdn(jk) - ptdbt(jk)) +                     &
                     ptdbt(jk) * pref(jk) * prdnd(jk))) * zreflect
     prdnd(ikp) = prefd(jk) + ptrad(jk) * ptrad(jk) *                          &
                      prdnd(jk) * zreflect
   enddo
!
! Up and down-welling fluxes at levels
!
   do jk = 1,klev+1
     zreflect = 1._rb / (1._rb - prdnd(jk) * prupd(jk))
     pfu(jk,kw) = (ptdbt(jk) * prup(jk) +                                      &
                      (ztdn(jk) - ptdbt(jk)) * prupd(jk)) * zreflect
     pfd(jk,kw) = ptdbt(jk) + (ztdn(jk) - ptdbt(jk)+                           &
                      ptdbt(jk) * prup(jk) * prdnd(jk)) * zreflect
   enddo
!
   end subroutine vrtqdr_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_vrtqdr_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_spcvmc_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
! ------- Modules -------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use parrrsw_k, only : nbndsw, ngptsw, mxmol, jpband
   use rrsw_tbl_k, only : tblint, bpade, od_lo, exp_tbl
   use rrsw_vsn_k, only : hvrspc, hnamspc
   use rrsw_wvn_k, only : ngc, ngs
   use rrtmg_sw_reftra_k, only: reftra_sw
   use rrtmg_sw_taumol_k, only: taumol_sw
   use rrtmg_sw_vrtqdr_k, only: vrtqdr_sw
!
   implicit none
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine spcvmc_sw                                                        &
            (nlayers, istart, iend, icpr, iout,                                &
             pavel, tavel, pz, tz, tbound, palbd, palbp,                       &
             pcldfmc, ptaucmc, pasycmc, pomgcmc, ptaormc,                      &
             ptaua, pasya, pomga, prmu0, coldry, wkl, adjflux,                 &
             laytrop, layswtch, laylow, jp, jt, jt1,                           &
             co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3,    &
             fac00, fac01, fac10, fac11,                                       &
             selffac, selffrac, indself, forfac, forfrac, indfor,              &
             dtliq,dtice,dtsno,dwliq,dwice,dwsno,daliq,daice,dasno,            &
             pbbfd, pbbfu, pbbcd, pbbcu, puvfd, puvcd, pnifd, pnicd,           &
             pbbfddir, pbbcddir, puvfddir, puvcddir, pnifddir, pnicddir)
!-------------------------------------------------------------------------------
!
! abstract: 
!   Contains spectral loop to compute the shortwave radiative fluxes, 
!   using the two-stream method of H. Barker and McICA, the Monte-Carlo
!   Independent Column Approximation, for the representation of 
!   sub-grid cloud variability (i.e. cloud overlap).
!
! Interface:  *spcvmc_sw* is called from *rrtmg_sw.F90* or rrtmg_sw.1col.F90*
!
! Method:
!    Adapted from two-stream model of H. Barker;
!    Two-stream model options (selected with kmodts in rrtmg_sw_reftra.F90):
!        1: Eddington, 2: PIFM, Zdunkowski et al., 3: discret ordinates
!
! history log :
!
!           H. Barker        Original
!  2003-12  J.-J.Morcrette   Merge with RRTMG_SW
!  2003-10  MJIacono, AER    Add adjustment for Earth/Sun distance
!  2003-11  MJIacono, AER    Bug fix for use of PALBP and PALBD
!  2004-12  MJIacono, AER    Bug fix to apply delta scaling to clear sky
!  2005-01  MJIacono, AER    Code modified so that delta scaling is not done in
!                            cloudy profiles if routine cldprop is used; delta 
!                            scaling can be applied by swithcing code below if 
!                            cldprop is used; delta scaling can be applied by
!                            swithcing code below if cldprop is not used to get
!                            cloud properties. 
!  2005-11  MJIacono, AER    Modified to use McICA
!  2006-07  MJIacono, AER    Uniform formatting for RRTMG
!  2007-08  MJIacono, AER    se exponential lookup table for transmittance
!  2016-10  Sungghye Baek    Revised Two Stream Approximaiton (TSA)
! Input : 
!   indfor(nlayers)
!   indself(nlayers)
!   jp(nlayers)
!   jt(nlayers)
!   jt1(nlayers)
! 
!   pavel(nlayers) : layer pressure (hPa, mb)
!   tavel(nlayers) : layer temperature (K)
!   pz(0:nlayers) : level (interface) pressure (hPa, mb)
!   tz(0:nlayers) : level temperatures (hPa, mb)
!   tbound : surface temperature (K)
!   wkl(mxmol,nlayers) : molecular amounts (mol/cm2) 
!   coldry(nlayers) :  dry air column density (mol/cm2)
!   colmol(nlayers)
!   adjflux(jpband) : Earth/Sun distance adjustment
!
!   palbd(nbndsw) : surface albedo (diffuse)
!   palbp(nbndsw) : surface albedo (direct)
!   prmu0 : cosine of solar zenith angle
!   pcldfmc(nlayers,ngptsw) : cloud fraction [mcica]
!   ptaucmc(nlayers,ngptsw) : cloud optical depth [mcica]
!   pasycmc(nlayers,ngptsw) : cloud asymmetry parameter [mcica]
!   pomgcmc(nlayers,ngptsw) : cloud single scattering albedo [mcica] 
!   ptaormc(nlayers,ngptsw) : cloud optical depth, non-delta scaled [mcica]
!   ptaua(nlayers,nbndsw) : aerosol optical depth
!   pasya(nlayers,nbndsw) :  aerosol asymmetry parameter
!   pomga(nlayers,nbndsw) :  aerosol single scattering albedo
!
!   colh2o(nlayers)
!   colco2(nlayers)
!   colch4(nlayers)
!   co2mult(nlayers)
!   colo3(nlayers)
!   colo2(nlayers)
!   coln2o(nlayers)
! 
!   forfac(nlayers)
!   forfrac(nlayers)
!   selffac(nlayers)
!   selffrac(nlayers)
!   fac00(nlayers)
!   fac01(nlayers)
!   fac10(nlayers)
!   fac11(nlayers)
!
!   dtliq (ngptsw,nlayers) : delta-scaled liquid cloud optical depth
!   dtice (ngptsw,nlayers) : delta-scaled ice cloud optical depth
!   dtsno (ngptsw,nlayers) : delta-scaled snow cloud optical depth
!   dwliq (ngptsw,nlayers) : delta-scaled liquid cloud single scattering albedo
!   dwice (ngptsw,nlayers) : delta-scaled ice cloud single scattering albedo 
!   dwsno (ngptsw,nlayers) : delta-scaled snow cloud single scattering albedo
!   daliq (ngptsw,nlayers) : delta-scaled liquid cloud asymmetry factor
!   daice (ngptsw,nlayers) : delta-scaled ice cloud asymmetry factor
!   dasno (ngptsw,nlayers) : delta-scaled snow cloud asymmetry factor
! Output :
!   pbbcd(nlayers+1)
!   pbbcu(nlayers+1)
!   pbbfd(nlayers+1)
!   pbbfddir(nlayers+1)
!   pbbcddir(nlayers+1)
!   puvcd(nlayers+1)
!   puvfd(nlayers+1)
!   puvcddir(nlayers+1)
!   puvfddir(nlayers+1)
!   pnicd(nlayers+1)
!   pnifd(nlayers+1)
!   pnicddir(nlayers+1)
!   pnifddir(nlayers+1)
!-------------------------------------------------------------------------------
! ------- Declarations ------
!
! ------- Input -------
!
   integer(kind=im), intent(in   ) :: nlayers
   integer(kind=im), intent(in   ) :: istart
   integer(kind=im), intent(in   ) :: iend
   integer(kind=im), intent(in   ) :: icpr
   integer(kind=im), intent(in   ) :: iout
   integer(kind=im), intent(in   ) :: laytrop
   integer(kind=im), intent(in   ) :: layswtch
   integer(kind=im), intent(in   ) :: laylow
!
   integer(kind=im), dimension(:), intent(in   ) :: indfor
   integer(kind=im), dimension(:), intent(in   ) :: indself
   integer(kind=im), dimension(:), intent(in   ) :: jp
   integer(kind=im), dimension(:), intent(in   ) :: jt
   integer(kind=im), dimension(:), intent(in   ) :: jt1
!
   real(kind=rb), dimension(:),  intent(in   ) :: pavel             
   real(kind=rb), dimension(:),  intent(in   ) :: tavel                    
   real(kind=rb), dimension(0:), intent(in   ) :: pz                      
   real(kind=rb), dimension(0:), intent(in   ) :: tz                     
   real(kind=rb),                intent(in   ) :: tbound                   
   real(kind=rb), dimension(:,:),intent(in   ) :: wkl                
   real(kind=rb), dimension(:),  intent(in   ) :: coldry               
   real(kind=rb), dimension(:),  intent(in   ) :: colmol
   real(kind=rb), dimension(:),  intent(in   ) :: adjflux    
!
   real(kind=rb), dimension(:),   intent(in   ) :: palbd                    
   real(kind=rb), dimension(:),   intent(in   ) :: palbp                    
   real(kind=rb),                 intent(in   ) :: prmu0                      
   real(kind=rb), dimension(:,:), intent(in   ) :: pcldfmc              
   real(kind=rb), dimension(:,:), intent(in   ) :: ptaucmc             
   real(kind=rb), dimension(:,:), intent(in   ) :: pasycmc            
   real(kind=rb), dimension(:,:), intent(in   ) :: pomgcmc           
   real(kind=rb), dimension(:,:), intent(in   ) :: ptaormc          
   real(kind=rb), dimension(:,:), intent(in   ) :: ptaua         
   real(kind=rb), dimension(:,:), intent(in   ) :: pasya          
   real(kind=rb), dimension(:,:), intent(in   ) :: pomga         
!
   real(kind=rb), dimension(:),   intent(in   ) :: colh2o
   real(kind=rb), dimension(:),   intent(in   ) :: colco2
   real(kind=rb), dimension(:),   intent(in   ) :: colch4
   real(kind=rb), dimension(:),   intent(in   ) :: co2mult
   real(kind=rb), dimension(:),   intent(in   ) :: colo3
   real(kind=rb), dimension(:),   intent(in   ) :: colo2
   real(kind=rb), dimension(:),   intent(in   ) :: coln2o
!
   real(kind=rb), dimension(:),   intent(in   ) :: forfac
   real(kind=rb), dimension(:),   intent(in   ) :: forfrac
   real(kind=rb), dimension(:),   intent(in   ) :: selffac
   real(kind=rb), dimension(:),   intent(in   ) :: selffrac
   real(kind=rb), dimension(:),   intent(in   ) :: fac00
   real(kind=rb), dimension(:),   intent(in   ) :: fac01
   real(kind=rb), dimension(:),   intent(in   ) :: fac10
   real(kind=rb), dimension(:),   intent(in   ) :: fac11
!
   real(kind=rb), dimension(:,:), intent(in   ) :: dtliq, dtice, dtsno
   real(kind=rb), dimension(:,:), intent(in   ) :: dwliq, dwice, dwsno
   real(kind=rb), dimension(:,:), intent(in   ) :: daliq, daice, dasno
!
! ------- Output -------
!  All Dimensions: (nlayers+1)
!
   real(kind=rb), dimension(:),  intent(  out) :: pbbcd
   real(kind=rb), dimension(:),  intent(  out) :: pbbcu
   real(kind=rb), dimension(:),  intent(  out) :: pbbfd
   real(kind=rb), dimension(:),  intent(  out) :: pbbfu
   real(kind=rb), dimension(:),  intent(  out) :: pbbfddir
   real(kind=rb), dimension(:),  intent(  out) :: pbbcddir
!
   real(kind=rb), dimension(:),  intent(  out) :: puvcd
   real(kind=rb), dimension(:),  intent(  out) :: puvfd
   real(kind=rb), dimension(:),  intent(  out) :: puvcddir
   real(kind=rb), dimension(:),  intent(  out) :: puvfddir
!
   real(kind=rb), dimension(:),  intent(  out) :: pnicd
   real(kind=rb), dimension(:),  intent(  out) :: pnifd
   real(kind=rb), dimension(:),  intent(  out) :: pnicddir
   real(kind=rb), dimension(:),  intent(  out) :: pnifddir
!
! Output - inactive   All Dimensions: (nlayers+1)
!      real(kind=rb), intent(out) :: puvcu(:)
!      real(kind=rb), intent(out) :: puvfu(:)
!      real(kind=rb), intent(out) :: pnicu(:)
!      real(kind=rb), intent(out) :: pnifu(:)
!      real(kind=rb), intent(out) :: pvscd(:)
!      real(kind=rb), intent(out) :: pvscu(:)
!      real(kind=rb), intent(out) :: pvsfd(:)
!      real(kind=rb), intent(out) :: pvsfu(:)
!
! ------- Local -------
!
   logical, dimension(nlayers) :: lrtchkclr,lrtchkcld
!
   integer(kind=im)  :: klev
   integer(kind=im) :: ib1, ib2, ibm, igt, ikl, ikp, ikx
   integer(kind=im) :: iw, jb, jg, jl, jk
!  integer(kind=im), parameter :: nuv = ?? 
!  integer(kind=im), parameter :: nvs = ?? 
   integer(kind=im) :: itind
!
   real(kind=rb) :: tblind, ze1
   real(kind=rb) :: zclear, zcloud
   real(kind=rb), dimension(nlayers+1) :: zdbt, zdbt_nodel
   real(kind=rb), dimension(nlayers  ) :: zgc, zgcc, zgco
   real(kind=rb), dimension(nlayers  ) :: zomc, zomcc, zomco
   real(kind=rb), dimension(nlayers+1) :: zrdnd, zrdndc
   real(kind=rb), dimension(nlayers+1) :: zref, zrefc, zrefo
   real(kind=rb), dimension(nlayers+1) :: zrefd, zrefdc, zrefdo
   real(kind=rb), dimension(nlayers+1) :: zrup, zrupd
   real(kind=rb), dimension(nlayers+1) :: zrupc, zrupdc
   real(kind=rb), dimension(nlayers+1) :: zs1
   real(kind=rb), dimension(nlayers  ) :: ztauc, ztauo
   real(kind=rb), dimension(nlayers+1) :: ztdn, ztdnd, ztdbt
   real(kind=rb), dimension(nlayers  ) :: ztoc, ztor
   real(kind=rb), dimension(nlayers+1) :: ztra, ztrac, ztrao
   real(kind=rb), dimension(nlayers+1) :: ztrad, ztradc, ztrado
   real(kind=rb), dimension(nlayers+1) :: zdbtc, ztdbtc, zdbtc_nodel
   real(kind=rb), dimension(ngptsw   ) :: zincflx
   real(kind=rb), dimension(nlayers+1) :: ztdbt_nodel, ztdbtc_nodel
!
   real(kind=rb) :: zdbtmc, zdbtmo, zf, zgw, zreflect
   real(kind=rb) :: zwf, tauorig, repclc
!  real(kind=rb) :: zincflux                                   ! inactive
!
! Arrays from rrtmg_sw_taumoln routines
!
!  real(kind=rb) :: ztaug(nlayers,16), ztaur(nlayers,16)
!  real(kind=rb) :: zsflxzen(16)
   real(kind=rb), dimension(nlayers,ngptsw) :: ztaug, ztaur
   real(kind=rb), dimension(ngptsw) :: zsflxzen
!
! Arrays from rrtmg_sw_vrtqdr routine
!
   real(kind=rb), dimension(nlayers+1,ngptsw) :: zcd, zcu, zfd, zfu
!
! Inactive arrays
!  real(kind=rb) :: zbbcd(nlayers+1), zbbcu(nlayers+1)
!  real(kind=rb) :: zbbfd(nlayers+1), zbbfu(nlayers+1)
!  real(kind=rb) :: zbbfddir(nlayers+1), zbbcddir(nlayers+1)
!
   real(kind=rb), dimension(nlayers) :: al1c, al2c, al3c
   real(kind=rb), dimension(nlayers) :: al1o, al2o, al3o
!
   real(kind=rb), dimension(nlayers) :: f_ray, f_aer
!
   real(kind=rb), dimension(nlayers) :: ts_ray, ts_aer
   real(kind=rb), dimension(nlayers) :: ts_liq, ts_ice, ts_sno
   real(kind=rb), dimension(nlayers) :: ta_ray, ta_gas, ta_aer(nlayers)
   real(kind=rb), dimension(nlayers) :: ta_liq, ta_ice, ta_sno
   real(kind=rb), dimension(nlayers) :: tt_ray, tt_aer
!
   real(kind=rb), dimension(nlayers) :: w_ray, w_aer
   real(kind=rb), dimension(nlayers) :: g_ray, g_aer
!
   real(kind=rb), dimension(nlayers) :: b0_ray, b0_aer
   real(kind=rb), dimension(nlayers) :: b0_liq, b0_ice, b0_sno
!
   real(kind=rb), dimension(nlayers) :: b0mu_ray, b0mu_aer
   real(kind=rb), dimension(nlayers) :: b0mu_liq, b0mu_ice, b0mu_sno
!
   real(kind=rb) :: ua_aer=2., ua_gas=2., ua_ray=2.
   real(kind=rb) :: ua_liq=2., ua_ice=2., ua_sno=2.
!
   real(kind=rb) :: us_aer=2., us_gas=2., us_ray=2.
   real(kind=rb) :: us_liq=2., us_ice=2., us_sno=2.
!
   real(kind=rb) :: tt
   logical       :: kmodts_4 = .false. 
!
! ------------------------------------------------------------------------------
!
! Initializations
!
   ib1 = istart
   ib2 = iend
   klev = nlayers
   iw = 0
   repclc = 1.e-12_rb
!  zincflux = 0.0_rb
!
   do jk = 1,klev+1
     pbbcd(jk)=0._rb
     pbbcu(jk)=0._rb
     pbbfd(jk)=0._rb
     pbbfu(jk)=0._rb
     pbbcddir(jk)=0._rb
     pbbfddir(jk)=0._rb
     puvcd(jk)=0._rb
     puvfd(jk)=0._rb
     puvcddir(jk)=0._rb
     puvfddir(jk)=0._rb
     pnicd(jk)=0._rb
     pnifd(jk)=0._rb
     pnicddir(jk)=0._rb
     pnifddir(jk)=0._rb
   enddo
!
! Calculate the optical depths for gaseous absorption and Rayleigh scattering
!
   call taumol_sw(klev,                                                        &
                     colh2o, colco2, colch4, colo2, colo3, colmol,             &
                     laytrop, jp, jt, jt1,                                     &
                     fac00, fac01, fac10, fac11,                               &
                     selffac, selffrac, indself, forfac, forfrac, indfor,      &
                     zsflxzen, ztaug, ztaur)
!
! Top of shortwave spectral band loop, jb = 16 -> 29; ibm = 1 -> 14
!
   do jb = ib1,ib2
     ibm = jb-15
     igt = ngc(ibm)
!
! Reinitialize g-point counter for each band if output for each band is
! requested.
!
     if (iout.gt.0.and.ibm.ge.2) iw = ngs(ibm-1)
!
!    do jk = 1,klev+1
!      zbbcd(jk)=0.0_rb
!      zbbcu(jk)=0.0_rb
!      zbbfd(jk)=0.0_rb
!      zbbfu(jk)=0.0_rb
!    enddo
!
! Top of g-point interval loop within each band (iw is cumulative counter) 
!
     do jg = 1,igt
       iw = iw+1
!
! Apply adjustment for correct Earth/Sun distance and zenith angle to incoming 
! solar flux
!
       zincflx(iw) = adjflux(jb) * zsflxzen(iw) * prmu0
!      zincflux = zincflux + adjflux(jb) * zsflxzen(iw) * prmu0      ! inactive
!
! Compute layer reflectances and transmittances for direct and diffuse sources, 
! first clear then cloudy
!
! zrefc(jk)  direct albedo for clear
! zrefo(jk)  direct albedo for cloud
! zrefdc(jk) diffuse albedo for clear
! zrefdo(jk) diffuse albedo for cloud
! ztrac(jk)  direct transmittance for clear
! ztrao(jk)  direct transmittance for cloudy
! ztradc(jk) diffuse transmittance for clear
! ztrado(jk) diffuse transmittance for cloudy
!  
! zref(jk)   direct reflectance
! zrefd(jk)  diffuse reflectance
! ztra(jk)   direct transmittance
! ztrad(jk)  diffuse transmittance
!
! zdbtc(jk)  clear direct beam transmittance
! zdbto(jk)  cloudy direct beam transmittance
! zdbt(jk)   layer mean direct beam transmittance
! ztdbt(jk)  total direct beam transmittance at levels
!
! Clear-sky    
!
!   TOA direct beam    
!
       ztdbtc(1)=1.0_rb
       ztdbtc_nodel(1)=1.0_rb
!
!   Surface values
!
       zdbtc(klev+1) =0.0_rb
       ztrac(klev+1) =0.0_rb
       ztradc(klev+1)=0.0_rb
       zrefc(klev+1) =palbp(ibm)
       zrefdc(klev+1)=palbd(ibm)
       zrupc(klev+1) =palbp(ibm)
       zrupdc(klev+1)=palbd(ibm)
!
! Total sky    
!   TOA direct beam    
!
       ztdbt(1)=1.0_rb
       ztdbt_nodel(1)=1.0_rb
!
!   Surface values
!
       zdbt(klev+1) =0.0_rb
       ztra(klev+1) =0.0_rb
       ztrad(klev+1)=0.0_rb
       zref(klev+1) =palbp(ibm)
       zrefd(klev+1)=palbd(ibm)
       zrup(klev+1) =palbp(ibm)
       zrupd(klev+1)=palbd(ibm)
!
! Top of layer loop
!
       do jk = 1,klev
!
! Note: two-stream calculations proceed from top to bottom; 
!   RRTMG_SW quantities are given bottom to top and are reversed here
!
         ikl=klev+1-jk
!
! Set logical flag to do REFTRA calculation
!   Do REFTRA for all clear layers
!
         lrtchkclr(jk)=.true.
!
!   Do REFTRA only for cloudy layers in profile, since already done for clear
!   layers
!
         lrtchkcld(jk)=.false.
         lrtchkcld(jk)=(pcldfmc(ikl,iw) > repclc)
!
! Clear-sky optical parameters - this section inactive     
!   Original
!          ztauc(jk) = ztaur(ikl,iw) + ztaug(ikl,iw)
!          zomcc(jk) = ztaur(ikl,iw) / ztauc(jk)
!          zgcc(jk) = 0.0001_rb
!   Total sky optical parameters        
!          ztauo(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + ptaucmc(ikl,iw)
!          zomco(jk) = ptaucmc(ikl,iw) * pomgcmc(ikl,iw) + ztaur(ikl,iw)
!          zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) +  &
!                       ztaur(ikl,iw) * 0.0001_rb) / zomco(jk)
!          zomco(jk) = zomco(jk) / ztauo(jk)
!
! Clear-sky optical parameters including aerosols
!
         if(ztaug(ikl,iw)<=0.) ztaug(ikl,iw)=0.
         ztauc(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + ptaua(ikl,ibm)
         zomcc(jk) = ztaur(ikl,iw) * 1.0_rb + ptaua(ikl,ibm) * pomga(ikl,ibm)
         zgcc(jk) = pasya(ikl,ibm) * pomga(ikl,ibm) * ptaua(ikl,ibm) / zomcc(jk)
         zomcc(jk) = zomcc(jk) / ztauc(jk)
!
         zf           = pasya(ikl,ibm)*pasya(ikl,ibm)
         if(prmu0<0.5) zf = pasya(ikl,ibm)**1.5
         zwf          = pomga(ikl,ibm)*zf
         f_aer(jk)  = zf
!
         g_aer(jk)  = (pasya(ikl,ibm)-zf)/(1.e0-zf)
         tt_aer(jk) = (1.e0-zwf)*ptaua(ikl,ibm)
         w_aer(jk)  = (pomga(ikl,ibm)-zwf)/(1.e0-zwf)
!
         ts_aer(jk) = tt_aer(jk)*w_aer(jk)
         ta_aer(jk) = tt_aer(jk)*(1.e0-w_aer(jk))
         b0mu_aer(jk) = 0.25e0*(2.e0-3.e0*g_aer(jk)*prmu0)
!        b0_aer(jk)   = 0.375e0*(1.e0-g_aer(jk))
!
         ts_liq(jk) = dtliq(iw,ikl)*dwliq(iw,ikl)
         ta_liq(jk) = dtliq(iw,ikl)*(1.e0-dwliq(iw,ikl))
         b0mu_liq(jk) = 0.25e0*(2.e0-3.e0*daliq(iw,ikl)*prmu0)
         b0_liq(jk)   = 0.375e0*(1.e0-daliq(iw,ikl))
!
         ts_ice(jk) = dtice(iw,ikl)*dwice(iw,ikl)
         ta_ice(jk) = dtice(iw,ikl)*(1.e0-dwice(iw,ikl))
         b0mu_ice(jk) = 0.25e0*(2.e0-3.e0*daice(iw,ikl)*prmu0)
         b0_ice(jk)   = 0.375e0*(1.e0-daice(iw,ikl))
!
         ts_sno(jk) = dtsno(iw,ikl)*dwsno(iw,ikl)
         ta_sno(jk) = dtsno(iw,ikl)*(1.e0-dwsno(iw,ikl))
         b0mu_sno(jk) = 0.25e0*(2.e0-3.e0*dasno(iw,ikl)*prmu0)
         b0_sno(jk)   = 0.375e0*(1.e0-dasno(iw,ikl))
!
         !aerosol
         if(prmu0<0.3)then
           b0mu_aer(jk) = 0.35e0*(2.e0-3.e0*g_aer(jk)*prmu0)
         elseif(prmu0<0.5)then
           b0mu_aer(jk) = 0.3e0*(2.e0-3.e0*g_aer(jk)*prmu0)
         elseif(prmu0>0.7)then
           b0mu_aer(jk) = 0.22e0*(2.e0-3.e0*g_aer(jk)*prmu0)
         endif
         b0_aer(jk) = 0.5e0*(1.e0-g_aer(jk))
         !liquid
         if(prmu0<0.5)then
           b0mu_liq(jk) = 0.3e0*(2.e0-3.e0*daliq(iw,ikl)*prmu0)
         elseif(prmu0<0.8)then
           b0mu_liq(jk) = 0.28e0*(2.e0-3.e0*daliq(iw,ikl)*prmu0)
         endif
         !ice
         if(prmu0<0.4)then
           b0mu_ice(jk) =  0.3e0*(2.e0-3.e0*daice(iw,ikl)*prmu0)
         elseif(prmu0>0.9)then
           b0mu_ice(jk) = 0.18e0*(2.e0-3.e0*daice(iw,ikl)*prmu0)
         endif
         !snow
         if(prmu0<0.4)then
           b0mu_sno(jk) = 0.3e0*(2.e0-3.e0*dasno(iw,ikl)*prmu0)
         endif
!
!
! Pre-delta-scaling clear and cloudy direct beam transmittance 
! (must use 'orig', unscaled cloud OD)       
!   \/\/\/ This block of code is only needed for direct beam calculation
!     
         zclear = 1.0_rb - pcldfmc(ikl,iw)
         zcloud = pcldfmc(ikl,iw)
!
! Clear
!        zdbtmc = exp(-ztauc(jk) / prmu0)
!
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
!
         ze1 = ztauc(jk) / prmu0
         if (ze1 .le. od_lo) then
           zdbtmc = 1._rb - ze1 + 0.5_rb * ze1 * ze1
         else 
           tblind = ze1 / (bpade + ze1)
           itind = tblint * tblind + 0.5_rb
           zdbtmc = exp_tbl(itind)
         endif
!
         zdbtc_nodel(jk) = zdbtmc
         ztdbtc_nodel(jk+1) = zdbtc_nodel(jk) * ztdbtc_nodel(jk)
!
! Clear + Cloud
!
         tauorig = ztauc(jk) + ptaormc(ikl,iw)
!        zdbtmo = exp(-tauorig / prmu0)
!
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
!
         ze1 = tauorig / prmu0
         if (ze1 .le. od_lo) then
           zdbtmo = 1._rb - ze1 + 0.5_rb * ze1 * ze1
         else
           tblind = ze1 / (bpade + ze1)
           itind = tblint * tblind + 0.5_rb
           zdbtmo = exp_tbl(itind)
         endif
         zdbt_nodel(jk) = zclear*zdbtmc + zcloud*zdbtmo
         ztdbt_nodel(jk+1) = zdbt_nodel(jk) * ztdbt_nodel(jk)
!
!   /\/\/\ Above code only needed for direct beam calculation
!
! Delta scaling - clear   
!
         ztauc(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + tt_aer(jk)
         zomcc(jk) = ztaur(ikl,iw) * 1.0_rb + tt_aer(jk) * w_aer(jk)
         zgcc(jk)  = g_aer(jk) * w_aer(jk) * tt_aer(jk) / zomcc(jk)
         zomcc(jk) = zomcc(jk) / ztauc(jk)
!
         tt = 1.e0 / ztauc(jk)
         al1c(jk) = (ua_gas*ztaug(ikl,iw)                                      &
                    + ua_aer*ta_aer(jk) + us_ray*0.375e0*ztaur(ikl,iw)         &
                    + us_aer*b0_aer(jk)*ts_aer(jk)) * tt
         al2c(jk) = (us_ray*0.375e0*ztaur(ikl,iw)                              &
                    + us_aer*b0_aer(jk)*ts_aer(jk)) * tt
         al3c(jk) = (0.5e0*ztaur(ikl,iw) + b0mu_aer(jk)*ts_aer(jk)) * tt
!
! Total sky optical parameters (cloud properties already delta-scaled)
!   Use this code if cloud properties are derived in rrtmg_sw_cldprop       
!
         if (icpr .ge. 1) then
           ztauo(jk) = ztauc(jk) + ptaucmc(ikl,iw)
           zomco(jk) = ztauc(jk) * zomcc(jk) + ptaucmc(ikl,iw) * pomgcmc(ikl,iw)
           zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) +  &
                              ztauc(jk) * zomcc(jk) * zgcc(jk)) / zomco(jk)
           zomco(jk) = zomco(jk) / ztauo(jk)
           tt = 1.e0/ztauo(jk)
!
           al1o(jk) = (ua_gas*ztaug(ikl,iw) + ua_aer*ta_aer(jk)                &
                      + ua_liq*ta_liq(jk) + ua_ice*ta_ice(jk)                  &
                      + ua_sno*ta_sno(jk)                                      &
!
                      + us_ray*0.375e0*ztaur(ikl,iw)                           &
                      + us_aer*b0_aer(jk)*ts_aer(jk)                           &
                      + us_liq*b0_liq(jk)*ts_liq(jk)                           &
                      + us_ice*b0_ice(jk)*ts_ice(jk)                           &
                      + us_sno*b0_sno(jk)*ts_sno(jk)) * tt
! 
           al2o(jk) = (us_ray*0.375e0*ztaur(ikl,iw)                            &
                      + us_aer*b0_aer(jk)*ts_aer(jk)                           &
                      + us_liq*b0_liq(jk)*ts_liq(jk)                           &
                      + us_ice*b0_ice(jk)*ts_ice(jk)                           &
                      + us_sno*b0_sno(jk)*ts_sno(jk)) * tt
!
           al3o(jk) = (0.5e0*ztaur(ikl,iw) + b0mu_aer(jk)*ts_aer(jk)           &
                      + b0mu_liq(jk)*ts_liq(jk) + b0mu_ice(jk)*ts_ice(jk)      &
                      + b0mu_sno(jk)*ts_sno(jk) ) * tt 
!
! Total sky optical parameters (if cloud properties not delta scaled)
!   Use this code if cloud properties are not derived in rrtmg_sw_cldprop       
!
         elseif (icpr .eq. 0) then
           ztauo(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) +                         &
                         ptaua(ikl,ibm) + ptaucmc(ikl,iw)
           zomco(jk) = ptaua(ikl,ibm) * pomga(ikl,ibm) +                       &
                         ptaucmc(ikl,iw) * pomgcmc(ikl,iw) +                   &
                         ztaur(ikl,iw) * 1.0_rb
           zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) +  &
                       ptaua(ikl,ibm)*pomga(ikl,ibm)*pasya(ikl,ibm)) / zomco(jk)
           zomco(jk) = zomco(jk) / ztauo(jk)
!
! Delta scaling - clouds 
!   Use only if subroutine rrtmg_sw_cldprop is not used to get cloud properties 
!   and to apply delta scaling
!
           zf = zgco(jk) * zgco(jk)
           zwf = zomco(jk) * zf
           ztauo(jk) = (1._rb - zwf) * ztauo(jk)
           zomco(jk) = (zomco(jk) - zwf) / (1.0_rb - zwf)
           zgco (jk) = (zgco(jk) - zf) / (1.0_rb - zf)
         endif 
!
! End of layer loop
!
       enddo    
!
! Clear sky reflectivities
!
       call reftra_sw (klev,                                                   &
                            lrtchkclr, zgcc, prmu0, ztauc, zomcc,              &
                            al1c, al2c, al3c,                                  & 
                            zrefc, zrefdc, ztrac, ztradc)
!
! Total sky reflectivities      
!
       call reftra_sw (klev,                                                   &
                       lrtchkcld, zgco, prmu0, ztauo, zomco,                   &
                       al1o, al2o, al3o,                                       &    
                       zrefo, zrefdo, ztrao, ztrado)
!
       do jk = 1,klev
!
! Combine clear and cloudy contributions for total sky
!
         ikl = klev+1-jk 
         zclear = 1.0_rb - pcldfmc(ikl,iw)
         zcloud = pcldfmc(ikl,iw)
!
         zref(jk)  = zclear*zrefc(jk) + zcloud*zrefo(jk)
         zrefd(jk) = zclear*zrefdc(jk) + zcloud*zrefdo(jk)
         ztra(jk)  = zclear*ztrac(jk) + zcloud*ztrao(jk)
         ztrad(jk) = zclear*ztradc(jk) + zcloud*ztrado(jk)
!
! Direct beam transmittance        
!
! Clear
!                zdbtmc = exp(-ztauc(jk) / prmu0)
!
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
!
         ze1 = ztauc(jk) / prmu0
         if (ze1 .le. od_lo) then
           zdbtmc = 1._rb - ze1 + 0.5_rb * ze1 * ze1
         else
           tblind = ze1 / (bpade + ze1)
           itind = tblint * tblind + 0.5_rb
           zdbtmc = exp_tbl(itind)
         endif
!
         zdbtc(jk) = zdbtmc
         ztdbtc(jk+1) = zdbtc(jk)*ztdbtc(jk)
!
! Clear + Cloud
!                zdbtmo = exp(-ztauo(jk) / prmu0)
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
!
         ze1 = ztauo(jk) / prmu0
         if (ze1 .le. od_lo) then
           zdbtmo = 1._rb - ze1 + 0.5_rb * ze1 * ze1
         else
           tblind = ze1 / (bpade + ze1)
           itind = tblint * tblind + 0.5_rb
           zdbtmo = exp_tbl(itind)
         endif
         zdbt(jk) = zclear*zdbtmc + zcloud*zdbtmo
         ztdbt(jk+1) = zdbt(jk)*ztdbt(jk)
       enddo           
!
! Vertical quadrature for clear-sky fluxes
!
       call vrtqdr_sw(klev, iw,                                                &
                           zrefc, zrefdc, ztrac, ztradc,                       &
                           zdbtc, zrdndc, zrupc, zrupdc, ztdbtc,               &
                           zcd, zcu)
!
! Vertical quadrature for cloudy fluxes
!
       call vrtqdr_sw(klev, iw,                                                &
                           zref, zrefd, ztra, ztrad,                           &
                           zdbt, zrdnd, zrup, zrupd, ztdbt,                    &
                           zfd, zfu)
!
! Upwelling and downwelling fluxes at levels
!   Two-stream calculations go from top to bottom; 
!   layer indexing is reversed to go bottom to top for output arrays
!
       do jk = 1,klev+1
         ikl = klev+2-jk
!
! Accumulate spectral fluxes over bands - inactive
!               zbbfu(ikl) = zbbfu(ikl) + zincflx(iw)*zfu(jk,iw)  
!               zbbfd(ikl) = zbbfd(ikl) + zincflx(iw)*zfd(jk,iw)
!               zbbcu(ikl) = zbbcu(ikl) + zincflx(iw)*zcu(jk,iw)
!               zbbcd(ikl) = zbbcd(ikl) + zincflx(iw)*zcd(jk,iw)
!               zbbfddir(ikl) = zbbfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
!               zbbcddir(ikl) = zbbcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
!
! Accumulate spectral fluxes over whole spectrum  
!
         pbbfu(ikl) = pbbfu(ikl) + zincflx(iw)*zfu(jk,iw)
         pbbfd(ikl) = pbbfd(ikl) + zincflx(iw)*zfd(jk,iw)
         pbbcu(ikl) = pbbcu(ikl) + zincflx(iw)*zcu(jk,iw)
         pbbcd(ikl) = pbbcd(ikl) + zincflx(iw)*zcd(jk,iw)
         pbbfddir(ikl) = pbbfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
         pbbcddir(ikl) = pbbcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
!
! Accumulate direct fluxes for UV/visible bands
!
         if (ibm >= 10 .and. ibm <= 13) then
           puvcd(ikl) = puvcd(ikl) + zincflx(iw)*zcd(jk,iw)
           puvfd(ikl) = puvfd(ikl) + zincflx(iw)*zfd(jk,iw)
           puvcddir(ikl) = puvcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
           puvfddir(ikl) = puvfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
!
! Accumulate direct fluxes for near-IR bands
!
         else if (ibm == 14 .or. ibm <= 9) then  
           pnicd(ikl) = pnicd(ikl) + zincflx(iw)*zcd(jk,iw)
           pnifd(ikl) = pnifd(ikl) + zincflx(iw)*zfd(jk,iw)
           pnicddir(ikl) = pnicddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
           pnifddir(ikl) = pnifddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
         endif
       enddo
!
! End loop on jg, g-point interval
!
     enddo             
!
! End loop on jb, spectral band
!
   enddo                    
!
   end subroutine spcvmc_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_spcvmc_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module rrtmg_sw_rad_k
!-------------------------------------------------------------------------------
!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
!
! ****************************************************************************
! *                                                                          *
! *                             RRTMG_SW                                     *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                 a rapid radiative transfer model                         *
! *                  for the solar spectral region                           *
! *           for application to general circulation models                  *
! *                                                                          *
! *                                                                          *
! *           Atmospheric and Environmental Research, Inc.                   *
! *                       131 Hartwell Avenue                                *
! *                       Lexington, MA 02421                                *
! *                                                                          *
! *                                                                          *
! *                          Eli J. Mlawer                                   *
! *                       Jennifer S. Delamere                               *
! *                        Michael J. Iacono                                 *
! *                        Shepard A. Clough                                 *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                      email:  miacono@aer.com                             *
! *                      email:  emlawer@aer.com                             *
! *                      email:  jdelamer@aer.com                            *
! *                                                                          *
! *       The authors wish to acknowledge the contributions of the           *
! *       following people:  Steven J. Taubman, Patrick D. Brown,            *
! *       Ronald E. Farren, Luke Chen, Robert Bergstrom.                     *
! *                                                                          *
! ****************************************************************************
!
! --------- Modules ---------
!
   use parkind_k, only : im => kind_im, rb => kind_rb
   use rrsw_vsn_k
   use rrtmg_sw_cldprmc_k, only: cldprmc_sw
!
! *** Move the required call to rrtmg_sw_ini below and the following 
! use association to GCM initialization area ***
!      use rrtmg_sw_init, only: rrtmg_sw_ini
!
   use rrtmg_sw_setcoef_k, only: setcoef_sw
   use rrtmg_sw_spcvmc_k, only: spcvmc_sw
!
   implicit none
!
! public interfaces/functions/subroutines
!
   public :: rrtmg_sw, inatm_sw, earth_sun
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
! Public subroutines
!-------------------------------------------------------------------------------
   subroutine rrtmg_sw                                                         &
            ( ncol    ,nlay    ,icld    ,                                      &
             play    ,plev    ,tlay    ,tlev    ,tsfc   ,                      &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr ,               &
             asdir   ,asdif   ,aldir   ,aldif   ,                              &
             coszen  ,adjes   ,dyofyr  ,scon    ,                              &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl ,                              &
             taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl ,                              &
             ciwpmcl ,clwpmcl ,reicmcl ,relqmcl ,                              &
             cswpmcl, resnmcl,                                                 &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   ,                              &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc,               &
             visdir  ,visdif  ,nirdir  ,nirdif                                 &
                 )
!-------------------------------------------------------------------------------
!
! ------- Description -------
! This program is the driver for RRTMG_SW, the AER SW radiation model for 
!  application to GCMs, that has been adapted from RRTM_SW for improved
!  efficiency and to provide fractional cloudiness and cloud overlap
!  capability using McICA.
!
! Note: The call to RRTMG_SW_INI should be moved to the GCM initialization 
!  area, since this has to be called only once. 
!
! This routine
!    b) calls INATM_SW to read in the atmospheric profile;
!       all layering in RRTMG is ordered from surface to toa. 
!    c) calls CLDPRMC_SW to set cloud optical depth for McICA based
!       on input cloud properties
!    d) calls SETCOEF_SW to calculate various quantities needed for 
!       the radiative transfer algorithm
!    e) calls SPCVMC to call the two-stream model that in turn 
!       calls TAUMOL to calculate gaseous optical depths for each 
!       of the 16 spectral bands and to perform the radiative transfer
!       using McICA, the Monte-Carlo Independent Column Approximation,
!       to represent sub-grid scale cloud variability
!    f) passes the calculated fluxes and cooling rates back to GCM
!
! Two modes of operation are possible:
!     The mode is chosen by using either rrtmg_sw.nomcica.f90 (to not use
!     McICA) or rrtmg_sw.f90 (to use McICA) to interface with a GCM.
!
!    1) Standard, single forward model calculation (imca = 0); this is 
!       valid only for clear sky or fully overcast clouds
!    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al., 
!       JC, 2003) method is applied to the forward model calculation (imca = 1)
!       This method is valid for clear sky or partial cloud conditions.
!
! This call to RRTMG_SW must be preceeded by a call to the module
!     mcica_subcol_gen_sw.f90 to run the McICA sub-column cloud generator,
!     which will provide the cloud physical or cloud optical properties
!     on the RRTMG quadrature point (ngptsw) dimension.
!
! Two methods of cloud property input are possible:
!     Cloud properties can be input in one of two ways (controlled by input 
!     flags inflag, iceflag and liqflag; see text file rrtmg_sw_instructions
!     and subroutine rrtmg_sw_cldprop.f90 for further details):
!
!    1) Input cloud fraction, cloud optical depth, single scattering albedo 
!       and asymmetry parameter directly (inflgsw = 0)
!    2) Input cloud fraction and cloud physical properties: ice fracion,
!       ice and liquid particle sizes (inflgsw = 1 or 2);  
!       cloud optical properties are calculated by cldprop or cldprmc based
!       on input settings of iceflgsw and liqflgsw
!
! Two methods of aerosol property input are possible:
!     Aerosol properties can be input in one of two ways (controlled by input 
!     flag iaer, see text file rrtmg_sw_instructions for further details):
!
!    1) Input aerosol optical depth, single scattering albedo and asymmetry
!       parameter directly by layer and spectral band (iaer=10)
!    2) Input aerosol optical depth and 0.55 micron directly by layer and use
!       one or more of six ECMWF aerosol types (iaer=6)
!
!
! ------- Modifications -------
!
! This version of RRTMG_SW has been modified from RRTM_SW to use a reduced
! set of g-point intervals and a two-stream model for application to GCMs. 
!
!-- Original version (derived from RRTM_SW)
!     2002: AER. Inc.
!-- Conversion to F90 formatting; addition of 2-stream radiative transfer
!     Feb 2003: J.-J. Morcrette, ECMWF
!-- Additional modifications for GCM application
!     Aug 2003: M. J. Iacono, AER Inc.
!-- Total number of g-points reduced from 224 to 112.  Original
!   set of 224 can be restored by exchanging code in module parrrsw.f90 
!   and in file rrtmg_sw_init.f90.
!     Apr 2004: M. J. Iacono, AER, Inc.
!-- Modifications to include output for direct and diffuse 
!   downward fluxes.  There are output as "true" fluxes without
!   any delta scaling applied.  Code can be commented to exclude
!   this calculation in source file rrtmg_sw_spcvrt.f90.
!     Jan 2005: E. J. Mlawer, M. J. Iacono, AER, Inc.
!-- Revised to add McICA capability.
!     Nov 2005: M. J. Iacono, AER, Inc.
!-- Reformatted for consistency with rrtmg_lw.
!     Feb 2007: M. J. Iacono, AER, Inc.
!-- Modifications to formatting to use assumed-shape arrays. 
!     Aug 2007: M. J. Iacono, AER, Inc.
!
!  Input : 
!    ncol -  Number of horizontal columns
!    nlay -  Number of model layers
!    icld - Cloud overlap method
!           0: Clear only  1: Random   2: Maximum/random  3: Maximum
!    play(ncol,nlay) - Layer pressures (hPa, mb)
!    plev(ncol,nlay+1) - Interface pressures (hPa, mb)
!    tlay(ncol,nlay) -  Layer temperatures (K)
!    tlev(ncol,nlay+1) - Interface temperatures (K)
!    tsfc(ncol) - Surface temperature (K)
!    h2ovmr(ncol,nlay) - H2O volume mixing ratio
!    o3vmr(ncol,nlay) -  O3 volume mixing ratio
!    co2vmr(ncol,nlay) - co2 volume mixing ratio
!    ch4vmr(ncol,nlay) - ch4 volume mixing ratio
!    n2ovmr(ncol,nlay) - n2o volume mixing ratio
!    o2vmr(ncol,nlay) - o2 volume mixing ratio
!    asdir(ncol) - UV/vis surface albedo direct rad
!    aldir(ncol) - Near-IR surface albedo direct rad
!    asdif(ncol) - UV/vis surface albedo diffuse rad
!    aldif(ncol) - Near-IR surface albedo diffuse rad     
!    dyofyr - Day of the year (used to get Earth/Sun distance if
!             adjflx not provided)
!    
!    adjes - Flux adjustment for Earth/Sun distance
!    coszen(ncol) - Cosine of solar zenith angle
!    scon - Solar constant (W/m2)
!    inflgsw - Flag for cloud optical properties
!    iceflgsw - Flag for ice particle specification
!    liqflgsw - Flag for liquid droplet specification
!    
!    cldfmcl(ngptsw,ncol,nlay) - Cloud fraction  
!    taucmcl(ngptsw,ncol,nlay) - In-cloud optical depth
!    ssacmcl(ngptsw,ncol,nlay) - In-cloud single scattering albedo
!    asmcmcl(ngptsw,ncol,nlay) - In-cloud asymmetry parameter
!    fsfcmcl(ngptsw,ncol,nlay) - In-cloud  forward scattering fraction
!    ciwpmcl(ngptsw,ncol,nlay) - In-cloud  water path (g/m2)
!    clwpmcl(ngptsw,ncol,nlay) - In-cloud  liquid water path (g/m2) 
!    cswpmcl(ngptsw,ncol,nlay) - In-cloud snow water path (g/m2)
!    reicmcl(ncol,nlay) -  Cloud ice effective radius (microns)
!
!    specific definition of reicmcl depends on setting of iceflglw:
!    iceflglw = 0: ice effective radius, r_ec, (Ebert and Curry, 1992),
!                  r_ec must be >= 10.0 microns
!    iceflglw = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
!                  r_ec range is limited to 13.0 to 130.0 microns
!    iceflglw = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
!                  r_k range is limited to 5.0 to 131.0 microns
!    iceflglw = 3: generalized effective size, dge, (Fu, 1996),
!                  dge range is limited to 5.0 to 140.0 microns
!                  [dge = 1.0315 * r_ec]
!
!    relqmcl(ncol,nlay) - Cloud water drop effective radius (microns) 
!    resnmcl(ncol,nlay) - Cloud snow effective radius (microns)
!    tauaer(ncol,nlay,nbndsw) - Aerosol optical depth (iaer=10 only) 
!                               (non-delta scaled) 
!    ssaaer(ncol,nlay,nbndsw) - Aerosol Aerosol single scattering albedo 
!                               (iaer=10 only) (non-delta scaled)
!    asmaer(ncol,nlay,nbndsw) - Aerosol Aerosol asymmetry parameters
!                               (iaer=10 only) (non-delta scaled)
!    ecaer(ncol,nlay,naerec) - Aerosol optical depth at 0.55 micron 
!                               (iaer=6 only) (non-delta scaled)
!
!  output :
!    swuflx(ncol,nlay+1) - Total sky shortwave upward flux (W/m2)
!    swdflx(ncol,nlay+1) - Total sky shortwave downward flux (W/m2)
!    swhr(ncol,nlay) - Total sky shortwave radiative heating rate (K/d)
!    swuflxc(ncol,nlay+1) -  Clear sky shortwave upward flux (W/m2)
!    swdflxc(ncol,nlay+1) - Clear sky shortwave downward flux (W/m2)
!    swhrc(ncol,nlay) - Clear sky shortwave radiative heating rate (K/d)
!    sibvisdir(ncol,nlay+1) - visible direct downward flux  (W/m2)
!    sibvisdif(ncol,nlay+1) - visible diffusion downward flux  (W/m2)
!    sibnirdir(ncol,nlay+1) - Near IR direct downward flux  (W/m2)
!    sibnirdif(ncol,nlay+1) - Near IR diffusion downward flux  (W/m2)
!    swdkdir(ncol,nlay) - Total shortwave downward direct flux (W/m2)
!    swdkdif(ncol,nlay) - Total shortwave downward diffuse flux (W/m2) 
!    visdir(ncol) - Direct downward surface shortwave flux, UV/vis
!    visdif(ncol) - Diffuse downward surface shortwave flux, UV/vis
!    nirdir(ncol) - Direct downward surface shortwave flux, Near-IR
!    nirdif(ncol) - Diffuse downward surface shortwave flux, Near-IR
!
! local : 
!   nlayers - total number of layers
!   istart - beginning band of calculation
!   iend - ending band of calculation
!   icpr - cldprop/cldprmc use flag
!   iout - output option flag (inactive)
!   iaer - aerosol option flag
!   idelm - delta-m scaling flag (inactive)
!   isccos - instrumental cosine response flag (inactive)
!   iplon - column loop index
!   i - layer loop index                       ! jk
!   ib - band loop index                        ! jsw
!   ia, ig -  indices
!   k - layer loop index
!   ims - value for changing mcica permute seed
!   imca - flag for mcica [0=off, 1=on]
!   zepsec, zepzen-  epsilon 
!   zdpgcp - flux to heating conversion ratio
!   
!   pavel(nlay+1)   - layer pressures (mb) 
!   tavel(nlay+1)   - layer temperatures (K)
!   pz(0:nlay+1)    - level (interface) pressures (hPa, mb)
!   tz(0:nlay+1)    - level (interface) temperatures (K)
!   tbound          - surface temperature (K)
!   pdp(nlay+1)     - layer pressure thickness (hPa, mb)
!   coldry(nlay+1)   - dry air column amount
!   wkl(mxmol,nlay+1)- molecular amounts (mol/cm-2)
!   cossza              -  Cosine of solar zenith angle
!   adjflux(jpband)     -  adjustment for current Earth/Sun distance
!   solvar(jpband)      -  solar constant scaling factor from rrtmg_sw
!                       -    default value of 1368.22 Wm-2 at 1 AU
!   albdir(nbndsw)      -  surface albedo, direct          ! zalbp
!   albdif(nbndsw)      -  surface albedo, diffuse         ! zalbd
!
!   taua(nlay+1,nbndsw) -  Aerosol optical depth
!   ssaa(nlay+1,nbndsw) -  Aerosol single scattering albedo
!   asma(nlay+1,nbndsw) -  Aerosol asymmetry parameter
!
!   laytrop             - tropopause layer index
!   layswtch            - tropopause layer index
!   laylow              - tropopause layer index
!   jp(nlay+1)           
!   jt(nlay+1)          
!   jt1(nlay+1)         
!
!   colh2o(nlay+1)      -  column amount (h2o)
!   colco2(nlay+1)      -  column amount (co2)
!   colo3(nlay+1)       -  column amount (o3)
!   coln2o(nlay+1)      -  column amount (n2o)
!   colch4(nlay+1)      -  column amount (ch4)
!   colo2(nlay+1)       -  column amount (o2)
!   colmol(nlay+1)      -  column amount
!   co2mult(nlay+1)     -  column amount 
!
!   ncbands             - number of cloud spectral bands
!   inflag              - flag for cloud property method
!   iceflag             - flag for ice cloud properties
!   liqflag             - flag for liquid cloud properties
!
!   cldfrac(nlay+1)   -  layer cloud fraction
!   tauc(nlay+1)      -  in-cloud optical depth (non-delta scaled)
!   ssac(nlay+1)      -  in-cloud single scattering albedo (non-delta scaled)
!   asmc(nlay+1)      -  in-cloud asymmetry parameter (non-delta scaled)
!   fsfc(nlay+1)      -  in-cloud forward scattering fraction (non-delta scaled)
!   ciwp(nlay+1)      -  in-cloud ice water path
!   clwp(nlay+1)      -  in-cloud liquid water path
!   rei(nlay+1)       -  cloud ice particle size
!   rel(nlay+1)       -  cloud liquid particle size
!
!   taucloud(nlay+1,jpband)  - in-cloud optical depth
!   taucldorig(nlay+1,jpband)- in-cloud optical depth (non-delta scaled)
!   ssacloud(nlay+1,jpband)  - in-cloud single scattering albedo
!   asmcloud(nlay+1,jpband)  - in-cloud asymmetry parameter
!
!   cldfmc(ngptsw,nlay+1)    - cloud fraction [mcica]
!   ciwpmc(ngptsw,nlay+1)    - in-cloud ice water path [mcica]
!   clwpmc(ngptsw,nlay+1)    - in-cloud liquid water path [mcica]
!   cswpmc(ngptsw,nlay+1)    - in-cloud snow water path [mcica]
!   relqmc(nlay+1)           - liquid particle effective radius (microns)
!   reicmc(nlay+1)           - ice particle effective size (microns)
!   resnmc(nlay+1)           - snow particle effective size (microns)
!   taucmc(ngptsw,nlay+1)    - in-cloud optical depth [mcica]
!   taormc(ngptsw,nlay+1)    - unscaled in-cloud optical depth [mcica]
!   ssacmc(ngptsw,nlay+1)    - in-cloud single scattering albedo [mcica]
!   asmcmc(ngptsw,nlay+1)    - in-cloud asymmetry parameter [mcica]
!   fsfcmc(ngptsw,nlay+1)    - in-cloud forward scattering fraction [mcica]
!
!   ztauc(nlay+1,nbndsw)     - cloud optical depth
!   ztaucorig(nlay+1,nbndsw) - unscaled cloud optical depth
!   zasyc(nlay+1,nbndsw)     - cloud asymmetry parameter 
!                              (first moment of phase function)
!   zomgc(nlay+1,nbndsw)     - cloud single scattering albedo
!   ztaua(nlay+1,nbndsw)     - total aerosol optical depth
!   zasya(nlay+1,nbndsw)     - total aerosol asymmetry parameter 
!   zomga(nlay+1,nbndsw)     - total aerosol single scattering albedo
!
!   zcldfmc(nlay+1,ngptsw)    -cloud fraction [mcica]
!   ztaucmc(nlay+1,ngptsw)    -cloud optical depth [mcica]
!   ztaormc(nlay+1,ngptsw)    -unscaled cloud optical depth [mcica]
!   zasycmc(nlay+1,ngptsw)    -cloud asymmetry parameter [mcica] 
!   zomgcmc(nlay+1,ngptsw)    -cloud single scattering albedo [mcica]
!
!   zbbfu(nlay+2)   - temporary upward shortwave flux (w/m2)
!   zbbfd(nlay+2)   - temporary downward shortwave flux (w/m2)
!   zbbcu(nlay+2)   - temporary clear sky upward shortwave flux (w/m2)
!   zbbcd(nlay+2)   - temporary clear sky downward shortwave flux (w/m2)
!   zbbfddir(nlay+2)- temporary downward direct shortwave flux (w/m2)
!   zbbcddir(nlay+2)- temporary clear sky downward direct shortwave flux (w/m2)
!   zuvfd(nlay+2)   - temporary UV downward shortwave flux (w/m2)
!   zuvcd(nlay+2)   - temporary clear sky UV downward shortwave flux (w/m2)
!   zuvfddir(nlay+2)- temporary UV downward direct shortwave flux (w/m2)
!   zuvcddir(nlay+2)- temporary clear sky UV downward direct shortwave flux 
!   znifd(nlay+2)   - temporary near-IR downward shortwave flux (w/m2)
!   znicd(nlay+2)   - temporary clear sky near-IR downward shortwave flux (w/m2)
!   znifddir(nlay+2)- temporary near-IR downward direct shortwave flux (w/m2)
!   znicddir(nlay+2)- temporary clear sky near-IR downward direct shortwave flux
!
!   swnflx(nlay+2)         - Total sky shortwave net flux (W/m2)
!   swnflxc(nlay+2)        - Clear sky shortwave net flux (W/m2)
!   dirdflux(nlay+2)       - Direct downward shortwave surface flux
!   difdflux(nlay+2)       - Diffuse downward shortwave surface flux
!   uvdflx(nlay+2)         - Total sky downward shortwave flux, UV/vis  
!   nidflx(nlay+2)         - Total sky downward shortwave flux, near-IR 
!   dirdnuv(nlay+2)        - Direct downward shortwave flux, UV/vis
!   difdnuv(nlay+2)        - Diffuse downward shortwave flux, UV/vis
!   dirdnir(nlay+2)        - Direct downward shortwave flux, near-IR
!   difdnir(nlay+2)        - Diffuse downward shortwave flux, near-IR
!
!   zuvfu(nlay+2)   - temporary upward UV shortwave flux (w/m2)
!   zuvfd(nlay+2)   - temporary downward UV shortwave flux (w/m2)
!   zuvcu(nlay+2)   - temporary clear sky upward UV shortwave flux (w/m2)
!   zuvcd(nlay+2)   - temporary clear sky downward UV shortwave flux (w/m2)
!   zvsfu(nlay+2)   - temporary upward visible shortwave flux (w/m2)
!   zvsfd(nlay+2)   - temporary downward visible shortwave flux (w/m2)
!   zvscu(nlay+2)   - temporary clear sky upward visible shortwave flux (w/m2)
!   zvscd(nlay+2)   - temporary clear sky downward visible shortwave flux (w/m2)
!   znifu(nlay+2)   - temporary upward near-IR shortwave flux (w/m2)
!   znifd(nlay+2)   - temporary downward near-IR shortwave flux (w/m2)
!   znicu(nlay+2)   - temporary clear sky upward near-IR shortwave flux (w/m2)
!   znicd(nlay+2)   - temporary clear sky downward near-IR shortwave flux (w/m2)
!
!-------------------------------------------------------------------------------
! --------- Modules ---------
!
   use parrrsw_k, only : nbndsw, ngptsw, naerec, nstr, nmol, mxmol,            &
                          jpband, jpb1, jpb2
   use rrsw_aer_k, only : rsrtaua, rsrpiza, rsrasya
   use rrsw_con_k, only : heatfac, oneminus, pi
   use rrsw_wvn_k, only : wavenum1, wavenum2
!
! ------- Declarations
!
! ----- Input -----
!
   integer(kind=im),                 intent(in   ) :: ncol                
   integer(kind=im),                 intent(in   ) :: nlay           
   integer(kind=im),                 intent(inout) :: icld       
   real(kind=rb), dimension(:,:),    intent(in   ) :: play       
   real(kind=rb), dimension(:,:),    intent(in   ) :: plev      
   real(kind=rb), dimension(:,:),    intent(in   ) :: tlay     
   real(kind=rb), dimension(:,:),    intent(in   ) :: tlev          
   real(kind=rb), dimension(:),      intent(in   ) :: tsfc           
   real(kind=rb), dimension(:,:),    intent(in   ) :: h2ovmr      
   real(kind=rb), dimension(:,:),    intent(in   ) :: o3vmr      
   real(kind=rb), dimension(:,:),    intent(in   ) :: co2vmr    
   real(kind=rb), dimension(:,:),    intent(in   ) :: ch4vmr   
   real(kind=rb), dimension(:,:),    intent(in   ) :: n2ovmr  
   real(kind=rb), dimension(:,:),    intent(in   ) :: o2vmr   
   real(kind=rb), dimension(:),      intent(in   ) :: asdir     
   real(kind=rb), dimension(:),      intent(in   ) :: aldir   
   real(kind=rb), dimension(:),      intent(in   ) :: asdif   
   real(kind=rb), dimension(:),      intent(in   ) :: aldif   
   integer(kind=im),                 intent(in   ) :: dyofyr  
   real(kind=rb),                    intent(in   ) :: adjes      
   real(kind=rb), dimension(:),      intent(in   ) :: coszen 
   real(kind=rb),                    intent(in   ) :: scon     
   integer(kind=im),                 intent(in   ) :: inflgsw     
   integer(kind=im),                 intent(in   ) :: iceflgsw    
   integer(kind=im),                 intent(in   ) :: liqflgsw   
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: cldfmcl
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: taucmcl
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: ssacmcl     
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: asmcmcl    
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: fsfcmcl   
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: ciwpmcl  
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: clwpmcl 
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: cswpmcl
   real(kind=rb), dimension(:,:),    intent(in   ) :: reicmcl 
   real(kind=rb), dimension(:,:),    intent(in   ) :: relqmcl 
   real(kind=rb), dimension(:,:),    intent(in   ) :: resnmcl   
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: tauaer
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: ssaaer
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: asmaer
   real(kind=rb), dimension(:,:,:),  intent(in   ) :: ecaer
!
! ----- Output -----
!
   real(kind=rb), dimension(:,:), intent(  out) :: swuflx   
   real(kind=rb), dimension(:,:), intent(  out) :: swdflx   
   real(kind=rb), dimension(:,:), intent(  out) :: swhr         
   real(kind=rb), dimension(:,:), intent(  out) :: swuflxc     
   real(kind=rb), dimension(:,:), intent(  out) :: swdflxc    
   real(kind=rb), dimension(:,:), intent(  out) :: swhrc     
   real(kind=rb), dimension(:),   intent(  out) :: visdir   
   real(kind=rb), dimension(:),   intent(  out) :: visdif  
   real(kind=rb), dimension(:),   intent(  out) :: nirdir 
   real(kind=rb), dimension(:),   intent(  out) :: nirdif
!
! ----- Local -----
!
! Control
!
   integer(kind=im) :: nlayers            
   integer(kind=im) :: istart            
   integer(kind=im) :: iend             
   integer(kind=im) :: icpr            
   integer(kind=im) :: iout          
   integer(kind=im) :: iaer         
   integer(kind=im) :: idelm       
   integer(kind=im) :: isccos     
   integer(kind=im) :: iplon     
   integer(kind=im) :: i        
   integer(kind=im) :: ib     
   integer(kind=im) :: ia, ig
   integer(kind=im) :: k                
   integer(kind=im) :: ims            
   integer(kind=im) :: imca          
   real(kind=rb) :: zepsec, zepzen      
   real(kind=rb) :: zdpgcp             
!
! Atmosphere
!
   real(kind=rb), dimension(nlay+1)         :: pavel      
   real(kind=rb), dimension(nlay+1)         :: tavel
   real(kind=rb), dimension(0:nlay+1)       :: pz    
   real(kind=rb), dimension(0:nlay+1)       :: tz   
   real(kind=rb)                            :: tbound    
   real(kind=rb), dimension(nlay+1)         :: pdp         
   real(kind=rb), dimension(nlay+1)         :: coldry    
   real(kind=rb), dimension(mxmol,nlay+1)   :: wkl
!
!   real(kind=rb) :: earth_sun     
   real(kind=rb) :: cossza                
   real(kind=rb), dimension(jpband) :: adjflux      
   real(kind=rb), dimension(jpband) :: solvar      
   real(kind=rb), dimension(nbndsw) :: albdir   
   real(kind=rb), dimension(nbndsw) :: albdif  
!
   real(kind=rb), dimension(nlay+1,nbndsw) :: taua   
   real(kind=rb), dimension(nlay+1,nbndsw) :: ssaa  
   real(kind=rb), dimension(nlay+1,nbndsw) :: asma 
!
! Atmosphere - setcoef
!
   integer(kind=im) :: laytrop       
   integer(kind=im) :: layswtch    
   integer(kind=im) :: laylow     
   integer(kind=im), dimension(nlay+1) :: jp           
   integer(kind=im), dimension(nlay+1) :: jt          
   integer(kind=im), dimension(nlay+1) :: jt1        
!
   real(kind=rb), dimension(nlay+1) :: colh2o       
   real(kind=rb), dimension(nlay+1) :: colco2     
   real(kind=rb), dimension(nlay+1) :: colo3     
   real(kind=rb), dimension(nlay+1) :: coln2o         
   real(kind=rb), dimension(nlay+1) :: colch4       
   real(kind=rb), dimension(nlay+1) :: colo2      
   real(kind=rb), dimension(nlay+1) :: colmol    
   real(kind=rb), dimension(nlay+1) :: co2mult   
!
   integer(kind=im), dimension(nlay+1) :: indself
   integer(kind=im), dimension(nlay+1) :: indfor
   real(kind=rb), dimension(nlay+1) :: selffac
   real(kind=rb), dimension(nlay+1) :: selffrac
   real(kind=rb), dimension(nlay+1) :: forfac
   real(kind=rb), dimension(nlay+1):: forfrac
   real(kind=rb), dimension(nlay+1) :: fac00, fac01, fac10, fac11 
!
! Atmosphere/clouds - cldprop
!
   integer(kind=im) :: ncbands             
   integer(kind=im) :: inflag              
   integer(kind=im) :: iceflag             
   integer(kind=im) :: liqflag            
!
!  real(kind=rb) :: cldfrac(nlay+1)      
!  real(kind=rb) :: tauc(nlay+1)        
!  real(kind=rb) :: ssac(nlay+1)       
!  real(kind=rb) :: asmc(nlay+1)      
!  real(kind=rb) :: fsfc(nlay+1)     
!  real(kind=rb) :: ciwp(nlay+1)         
!  real(kind=rb) :: clwp(nlay+1)     
!  real(kind=rb) :: rei(nlay+1)   
!  real(kind=rb) :: rel(nlay+1) 
!
!  real(kind=rb) :: taucloud(nlay+1,jpband)  
!  real(kind=rb) :: taucldorig(nlay+1,jpband)
!  real(kind=rb) :: ssacloud(nlay+1,jpband)
!  real(kind=rb) :: asmcloud(nlay+1,jpband)
!
! Atmosphere/clouds - cldprmc [mcica]
!
   real(kind=rb), dimension(ngptsw,nlay+1) :: cldfmc
   real(kind=rb), dimension(ngptsw,nlay+1) :: ciwpmc  
   real(kind=rb), dimension(ngptsw,nlay+1) :: clwpmc 
   real(kind=rb), dimension(ngptsw,nlay+1) :: cswpmc
   real(kind=rb), dimension(nlay+1) :: relqmc      
   real(kind=rb), dimension(nlay+1) :: reicmc     
   real(kind=rb), dimension(nlay+1) :: resnmc   
   real(kind=rb), dimension(ngptsw,nlay+1) :: taucmc   
   real(kind=rb), dimension(ngptsw,nlay+1) :: taormc  
   real(kind=rb), dimension(ngptsw,nlay+1) :: ssacmc 
   real(kind=rb), dimension(ngptsw,nlay+1) :: asmcmc
   real(kind=rb), dimension(ngptsw,nlay+1) :: fsfcmc
   real(kind=rb), dimension(ngptsw,nlay+1) :: dtliq
   real(kind=rb), dimension(ngptsw,nlay+1) :: dtice
   real(kind=rb), dimension(ngptsw,nlay+1) :: dtsno
   real(kind=rb), dimension(ngptsw,nlay+1) :: dwliq
   real(kind=rb), dimension(ngptsw,nlay+1) :: dwice
   real(kind=rb), dimension(ngptsw,nlay+1) :: dwsno
   real(kind=rb), dimension(ngptsw,nlay+1) :: daliq
   real(kind=rb), dimension(ngptsw,nlay+1) :: daice
   real(kind=rb), dimension(ngptsw,nlay+1) :: dasno
!
! Atmosphere/clouds/aerosol - spcvrt,spcvmc
!
   real(kind=rb), dimension(nlay+1,nbndsw) :: ztauc     
   real(kind=rb), dimension(nlay+1,nbndsw) :: ztaucorig
   real(kind=rb), dimension(nlay+1,nbndsw) :: zasyc    
   real(kind=rb), dimension(nlay+1,nbndsw) :: zomgc 
   real(kind=rb), dimension(nlay+1,nbndsw) :: ztaua
   real(kind=rb), dimension(nlay+1,nbndsw) :: zasya 
   real(kind=rb), dimension(nlay+1,nbndsw) :: zomga
!
   real(kind=rb), dimension(nlay+1,ngptsw) :: zcldfmc  
   real(kind=rb), dimension(nlay+1,ngptsw) :: ztaucmc 
   real(kind=rb), dimension(nlay+1,ngptsw) :: ztaormc
   real(kind=rb), dimension(nlay+1,ngptsw) :: zasycmc   
   real(kind=rb), dimension(nlay+1,ngptsw) :: zomgcmc  
!
   real(kind=rb), dimension(nlay+2) :: zbbfu          
   real(kind=rb), dimension(nlay+2) :: zbbfd         
   real(kind=rb), dimension(nlay+2) :: zbbcu       
   real(kind=rb), dimension(nlay+2) :: zbbcd       
   real(kind=rb), dimension(nlay+2) :: zbbfddir   
   real(kind=rb), dimension(nlay+2) :: zbbcddir  
   real(kind=rb), dimension(nlay+2) :: zuvfd    
   real(kind=rb), dimension(nlay+2) :: zuvcd   
   real(kind=rb), dimension(nlay+2) :: zuvfddir
   real(kind=rb), dimension(nlay+2) :: zuvcddir
   real(kind=rb), dimension(nlay+2) :: znifd 
   real(kind=rb), dimension(nlay+2) :: znicd        
   real(kind=rb), dimension(nlay+2) :: znifddir  
   real(kind=rb), dimension(nlay+2) :: znicddir
!
! Optional output fields 
!
   real(kind=rb), dimension(nlay+2) :: swnflx      
   real(kind=rb), dimension(nlay+2) :: swnflxc   
   real(kind=rb), dimension(nlay+2) :: dirdflux 
   real(kind=rb), dimension(nlay+2) :: difdflux      
   real(kind=rb), dimension(nlay+2) :: uvdflx         
   real(kind=rb), dimension(nlay+2) :: nidflx   
   real(kind=rb), dimension(nlay+2) :: dirdnuv 
   real(kind=rb), dimension(nlay+2) :: difdnuv
   real(kind=rb), dimension(nlay+2) :: dirdnir   
   real(kind=rb), dimension(nlay+2) :: difdnir  
!
! Output - inactive
!
!  real(kind=rb) :: zuvfu(nlay+2)   
!  real(kind=rb) :: zuvfd(nlay+2)  
!  real(kind=rb) :: zuvcu(nlay+2)
!  real(kind=rb) :: zuvcd(nlay+2)
!  real(kind=rb) :: zvsfu(nlay+2)
!  real(kind=rb) :: zvsfd(nlay+2)
!  real(kind=rb) :: zvscu(nlay+2)
!  real(kind=rb) :: zvscd(nlay+2)
!  real(kind=rb) :: znifu(nlay+2)
!  real(kind=rb) :: znifd(nlay+2)
!  real(kind=rb) :: znicu(nlay+2)
!  real(kind=rb) :: znicd(nlay+2)
!
!
! Initializations
!
   zepsec = 1.e-06_rb
   zepzen = 1.e-10_rb
   oneminus = 1.0_rb - zepsec
   pi = 2._rb * asin(1._rb)
!
   istart = jpb1
   iend = jpb2
   icpr = 0
   ims = 2
!
! In a GCM with or without McICA, set nlon to the longitude dimension
!
! Set imca to select calculation type:
!  imca = 0, use standard forward model calculation (clear and overcast only)
!  imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
!            (clear, overcast or partial cloud conditions)
!
! *** This version uses McICA (imca = 1) ***
!
! Set icld to select of clear or cloud calculation and cloud 
! overlap method (read by subroutine readprof from input file INPUT_RRTM):  
! icld = 0, clear only
! icld = 1, with clouds using random cloud overlap (McICA only)
! icld = 2, with clouds using maximum/random cloud overlap (McICA only)
! icld = 3, with clouds using maximum cloud overlap (McICA only)
   if (icld.lt.0.or.icld.gt.3) icld = 2
!
! Set iaer to select aerosol option
! iaer = 0, no aerosols
! iaer = 6, use six ECMWF aerosol types
!           input aerosol optical depth at 0.55 microns for each 
!           aerosol type (ecaer)
! iaer = 10, input total aerosol optical depth, single scattering albedo 
!            and asymmetry parameter (tauaer, ssaaer, asmaer) directly
     iaer = 6
!
! Call model and data initialization, compute lookup tables, perform
! reduction of g-points from 224 to 112 for input absorption
! coefficient data and other arrays.
!
! In a GCM this call should be placed in the model initialization
! area, since this has to be called only once.  
!      call rrtmg_sw_ini(cpdair)
!
! This is the main longitude/column loop in RRTMG.
! Modify to loop over all columns (nlon) or over daylight columns
!
   do iplon = 1,ncol
     if(coszen(iplon).gt.0.0)then 
!
! Prepare atmosphere profile from GCM for use in RRTMG, and define
! other input parameters
!
     call inatm_sw (iplon, nlay, icld, iaer,                                   &
              play, plev, tlay, tlev, tsfc, h2ovmr,                            &
              o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,                            &
              adjes, dyofyr, scon, inflgsw, iceflgsw, liqflgsw,                &
              cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl,                     &
              ciwpmcl, clwpmcl, reicmcl, relqmcl,                              &
              cswpmcl, resnmcl,                                                &
              tauaer, ssaaer, asmaer,                                          &
              nlayers, pavel, pz, pdp, tavel, tz, tbound, coldry, wkl,         &
              adjflux, solvar, inflag, iceflag, liqflag, cldfmc, taucmc,       &
              ssacmc, asmcmc, fsfcmc,                                          &
              ciwpmc, clwpmc, reicmc, relqmc,                                  &
              cswpmc, resnmc,                                                  &
              taua, ssaa, asma)
!
!  For cloudy atmosphere, use cldprop to set cloud optical properties based on
!  input cloud physical properties.  Select method based on choices described
!  in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
!  effective radius must be passed in cldprop.  Cloud fraction and cloud
!  optical properties are transferred to rrtmg_sw arrays in cldprop.  
!
     dtliq=0._rb ; dtice=0._rb ; dtsno=0._rb
     dwliq=1._rb ; dwice=1._rb ; dwsno=1._rb
     daliq=0._rb ; daice=0._rb ; dasno=0._rb
     call cldprmc_sw(nlayers, inflag, iceflag, liqflag, cldfmc,                &
                         ciwpmc, clwpmc, reicmc, relqmc,                       &
                         cswpmc, resnmc,                                       &
                         dtliq, dtice, dtsno, dwliq, dwice, dwsno,             &
                         daliq, daice, dasno,                                  & 
                         taormc, taucmc, ssacmc, asmcmc, fsfcmc)
     icpr = 1
!
! Calculate coefficients for the temperature and pressure dependence of the 
! molecular absorption coefficients by interpolating data from stored
! reference atmospheres.
!
     call setcoef_sw(nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl,       &
                         laytrop, layswtch, laylow, jp, jt, jt1,               &
                         co2mult, colch4, colco2, colh2o, colmol, coln2o,      &
                         colo2, colo3, fac00, fac01, fac10, fac11,             &
                         selffac, selffrac, indself, forfac, forfrac, indfor)
!
! Cosine of the solar zenith angle 
!  Prevent using value of zero; ideally, 
!  SW model is not called from host model when sun 
!  is below horizon
!
     cossza = coszen(iplon)
     if (cossza .le. zepzen) cossza = zepzen
!
! Transfer albedo, cloud and aerosol properties into arrays for 
! 2-stream radiative transfer 
!
! Surface albedo
!  Near-IR bands 16-24 and 29 (1-9 and 14), 820-16000 cm-1, 0.625-12.195 microns
!
     do ib = 1,9
       albdir(ib) = aldir(iplon)
       albdif(ib) = aldif(iplon)
     enddo
     albdir(nbndsw) = aldir(iplon)
     albdif(nbndsw) = aldif(iplon)
!
!  UV/visible bands 25-28 (10-13), 16000-50000 cm-1, 0.200-0.625 micron
!
     do ib = 10,13
       albdir(ib) = asdir(iplon)
       albdif(ib) = asdif(iplon)
     enddo
!
! Clouds
!
     if (icld.eq.0) then
       zcldfmc(:,:) = 0._rb
       ztaucmc(:,:) = 0._rb
       ztaormc(:,:) = 0._rb
       zasycmc(:,:) = 0._rb
       zomgcmc(:,:) = 1._rb
     elseif (icld.ge.1) then
       do i = 1,nlayers
         do ig = 1,ngptsw
           zcldfmc(i,ig) = cldfmc(ig,i)
           ztaucmc(i,ig) = taucmc(ig,i)
           ztaormc(i,ig) = taormc(ig,i)
           zasycmc(i,ig) = asmcmc(ig,i)
           zomgcmc(i,ig) = ssacmc(ig,i)
         enddo
       enddo
     endif   
!
! Aerosol
! IAER = 0: no aerosols
!
     if(iaer.eq.0) then
       ztaua(:,:) = 0._rb
       zasya(:,:) = 0._rb
       zomga(:,:) = 1._rb
!
! IAER = 6: Use ECMWF six aerosol types. See rrsw_aer.f90 for details.
! Input aerosol optical thickness at 0.55 micron for each aerosol type (ecaer), 
! or set manually here for each aerosol and layer.
!
     elseif(iaer.eq.6) then
!      do i = 1,nlayers
!        do ia = 1,naerec
!          ecaer(iplon,i,ia) = 1.0e-15_rb
!        enddo
!      enddo
       do i = 1,nlayers
         do ib = 1,nbndsw
           ztaua(i,ib) = 0._rb
           zasya(i,ib) = 0._rb
           zomga(i,ib) = 0._rb
           do ia = 1,naerec
             ztaua(i,ib) = ztaua(i,ib)+rsrtaua(ib,ia)*ecaer(iplon,i,ia)
             zomga(i,ib) = zomga(i,ib)+rsrtaua(ib,ia)*ecaer(iplon,i,ia)*       &
                                   rsrpiza(ib,ia)
             zasya(i,ib) = zasya(i,ib)+rsrtaua(ib,ia)*ecaer(iplon,i,ia)*       &
                                   rsrpiza(ib,ia) * rsrasya(ib,ia)
           enddo
           if(zomga(i,ib).ne.0._rb) then
             zasya(i,ib) = zasya(i,ib)/zomga(i,ib)
           endif
           if(ztaua(i,ib).ne.0._rb) then
             zomga(i,ib) = zomga(i,ib)/ztaua(i,ib)
           endif
         enddo
       enddo
!
! IAER=10: Direct specification of aerosol optical properties from GCM
!
     elseif(iaer.eq.10) then
       do i = 1,nlayers
         do ib = 1,nbndsw
           ztaua(i,ib) = taua(i,ib)
           zasya(i,ib) = asma(i,ib)
           zomga(i,ib) = ssaa(i,ib)
         enddo
       enddo
     endif
!
! Call the 2-stream radiation transfer model
!
     do i = 1,nlayers+1
       zbbcu(i) = 0._rb
       zbbcd(i) = 0._rb
       zbbfu(i) = 0._rb
       zbbfd(i) = 0._rb
       zbbcddir(i) = 0._rb
       zbbfddir(i) = 0._rb
       zuvcd(i) = 0._rb
       zuvfd(i) = 0._rb
       zuvcddir(i) = 0._rb
       zuvfddir(i) = 0._rb
       znicd(i) = 0._rb
       znifd(i) = 0._rb
       znicddir(i) = 0._rb
       znifddir(i) = 0._rb
     enddo
!
     call spcvmc_sw                                                            &
             (nlayers, istart, iend, icpr, iout,                               &
              pavel, tavel, pz, tz, tbound, albdif, albdir,                    &
              zcldfmc, ztaucmc, zasycmc, zomgcmc, ztaormc,                     &
              ztaua, zasya, zomga, cossza, coldry, wkl, adjflux,               &
              laytrop, layswtch, laylow, jp, jt, jt1,                          &
              co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3,   &
              fac00, fac01, fac10, fac11,                                      &
              selffac, selffrac, indself, forfac, forfrac, indfor,             &
              dtliq,dtice,dtsno,dwliq,dwice,dwsno,daliq,daice,dasno,           & 
              zbbfd, zbbfu, zbbcd, zbbcu, zuvfd, zuvcd, znifd, znicd,          &
              zbbfddir, zbbcddir, zuvfddir, zuvcddir, znifddir, znicddir)
!
! Transfer up and down, clear and total sky fluxes to output arrays.
! Vertical indexing goes from bottom to top; reverse here for GCM if necessary.
!
     do i = 1,nlayers+1
       swuflxc(iplon,i) = zbbcu(i)
       swdflxc(iplon,i) = zbbcd(i)
       swuflx(iplon,i) = zbbfu(i)
       swdflx(iplon,i) = zbbfd(i)
       uvdflx(i) = zuvfd(i)
       nidflx(i) = znifd(i)
!
!  Direct/diffuse fluxes
!
       dirdflux(i) = zbbfddir(i)
       difdflux(i) = swdflx(iplon,i) - dirdflux(i)
!
!  UV/visible direct/diffuse fluxes
!
       dirdnuv(i) = zuvfddir(i)
       difdnuv(i) = zuvfd(i) - dirdnuv(i)
!
!  Near-IR direct/diffuse fluxes
!
       dirdnir(i) = znifddir(i)
       difdnir(i) = znifd(i) - dirdnir(i)
     enddo
!
!  Total and clear sky net fluxes
!
     do i = 1,nlayers+1
       swnflxc(i) = swdflxc(iplon,i) - swuflxc(iplon,i)
       swnflx(i) = swdflx(iplon,i) - swuflx(iplon,i)
     enddo
!
!  Total and clear sky heating rates
!
     do i = 1,nlayers
       zdpgcp = heatfac / pdp(i)
       swhrc(iplon,i) = (swnflxc(i+1) - swnflxc(i)) * zdpgcp
       swhr(iplon,i) = (swnflx(i+1) - swnflx(i)) * zdpgcp
     enddo
     swhrc(iplon,nlayers) = 0._rb
     swhr(iplon,nlayers) = 0._rb
!
! End longitude loop
!
     endif
   enddo
!
   end subroutine rrtmg_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   real(kind=rb) function earth_sun(idn)
!-------------------------------------------------------------------------------
!
!  abstract: 
!    Function to calculate the correction factor of Earth's orbit
!    for current day of the year
!
!  idn        : Day of the year
!  earth_sun  : square of the ratio of mean to actual Earth-Sun distance
!
!-------------------------------------------------------------------------------
! ------- Modules -------
!
   use rrsw_con_k, only : pi
!
   integer(kind=im), intent(in) :: idn
   real(kind=rb) :: gamma
!-------------------------------------------------------------------------------
   gamma = 2._rb*pi*(idn-1)/365._rb
!
! Use Iqbal's equation 1.2.1
!
   earth_sun = 1.000110_rb+.034221_rb*cos(gamma) +                             &
                .001289_rb*sin(gamma) +                                        &
                .000719_rb*cos(2._rb*gamma)+.000077_rb*sin(2._rb*gamma)
!
   end function earth_sun
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine inatm_sw (iplon, nlay, icld, iaer,                               &
            play, plev, tlay, tlev, tsfc, h2ovmr,                              &
            o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr,                              &
            adjes, dyofyr, scon, inflgsw, iceflgsw, liqflgsw,                  &
            cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl,                       &
            ciwpmcl, clwpmcl, reicmcl, relqmcl,                                &
            cswpmcl, resnmcl,                                                  &
            tauaer, ssaaer, asmaer,                                            &
            nlayers, pavel, pz, pdp, tavel, tz, tbound, coldry, wkl,           &
            adjflux, solvar, inflag, iceflag, liqflag, cldfmc, taucmc,         &
            ssacmc, asmcmc, fsfcmc,                                            &
            ciwpmc, clwpmc, reicmc, relqmc,                                    &
            cswpmc, resnmc,                                                    &
            taua, ssaa, asma)
!-------------------------------------------------------------------------------
!
!  abstract :
!    Input atmospheric profile from GCM, and prepare it for use in RRTMG_SW.
!    Set other RRTMG_SW input parameters.  
!
!  history log :
!
!  input :
!    iplon - column loop index
!    nlay - number of model layers
!    icld - clear/cloud and cloud overlap flag 
!    iaer - aerosol option flag
!    play(ncol,nlay) - Layer pressures (hPa, mb)
!    plev(ncol,nlay+1) - Interface pressures (hPa, mb)
!    tlay(ncol,nlay) - Layer temperatures (K)
!    tlev(ncol,nlay+1) - Interface  temperatures (K)
!    tsfc(ncol) - Surface temperature (K)
!    h2ovmr(ncol,nlay) - H2O volume mixing ratio   
!    o3vmr(ncol,nlay) - o3 volume mixing ratio  
!    co2vmr(ncol,nlay) - co2 volume mixing ratio 
!    ch4vmr(ncol,nlay) - ch4 volume mixing ratio 
!    n2ovmr(ncol,nlay) - n2o volume mixing ratio  
!    o2vmr(ncol,nlay) - o2 volume mixing ratio 
!    
!    dyofyr -  Day of the year (used to get Earth/Sun distance 
!              if adjflx not provided)
!    adjes - Flux adjustment for Earth/Sun distance
!    scon - Solar constant (W/m2)
!    
!    inflgsw - Flag for cloud optical properties
!    iceflgsw - Flag for ice particle specification
!    liqflgsw - Flag for liquid droplet specification
!
!    cldfmcl(ngptsw,ncol,nlay) - cloud fraction
!    taucmcl(ngptsw,ncol,nlay) - In-cloud optical depth (optional) 
!    ssacmcl(ngptsw,ncol,nlay) - In-cloud single scattering albedo (optional) 
!    asmcmcl(ngptsw,ncol,nlay) - In-cloud asymmetry parameter (optional) 
!    fsfcmcl(ngptsw,ncol,nlay) - In-cloud forward scattering fraction (optional)
!    ciwpmcl(ngptsw,ncol,nlay) - In-cloud ice water path (g/m2)
!    clwpmcl(ngptsw,ncol,nlay) - In-cloud liquid water path (g/m2)
!    cswpmcl(ngptsw,ncol,nlay) - In-cloud snow water path (g/m2)
!    reicmcl(ncol,nlay) - Cloud ice effective size (microns)
!    relqmcl(ncol,nlay) - Cloud water drop effective radius (microns) 
!    resnmcl(ncol,nlay) - Cloud snow effective radius (microns)
!    
!    tauaer(ncol,nlay,nbndsw) -  Aerosol optical depth
!    ssaaer(ncol,nlay,nbndsw) -  Aerosol single scattering albedo
!    asmaer(ncol,nlay,nbndsw) -  Aerosol asymmetry parameters
!
!    nlayers - number of layers
!    pavel(nlay) - layer pressures (mb) 
!    tavel(nlay) layer temperatures (K)
!    pz(0:) - level (interface) pressures (hPa, mb)
!    tz(0:) - level (interface) temperatures (K)
!    tbound - surface temperature(K)
!    pdp(nlay) - layer pressure thickness (hPa, mb)
!    coldry(nlay) - dry air column density (mol/cm2)
!    wkl(mxmol,nlay) - molecular amounts (mol/cm-2)
!    adjflux(jpband) - adjustment for current Earth/Sun distance
!    solvar(jpband) - solar constant scaling factor from rrtmg_sw
!                     default value of 1368.22 Wm-2 at 1 AU
!    taua(nlay,nbndsw) - Aerosol optical depth
!    ssaa(nlay,nbndsw) - Aerosol single scattering albedo
!    asma(nlay,nbndsw) - Aerosol asymmtry parameter
!
!    inflag -  flag for cloud property method
!    iceflag -  flag for ice cloud properties
!    liqflag - flag for liquid cloud properties
!
!    cldfmc(ngptsw,nlay) - layer cloud fraction
!    taucmc(ngptsw,nlay) - in-cloud optical depth (non-delta scaled)
!    ssacmc(ngptsw,nlay) - in-cloud single scattering albedo (non-delta scaled)
!    asmcmc(ngptsw,nlay) - in-cloud asymmetry parameter (non-delta scaled)
!    fsfcmc(ngptsw,nlay) - in-cloud forward scattering fraction 
!                          (non-delta scaled)
!    ciwpmc(ngptsw,nlay) - in-cloud ice water path
!    clwpmc(ngptsw,nlay) - in-cloud liquid water path 
!    cswpmc(ngptsw,nlay) - in-cloud snow water path
!    relqmc(nlay) - liquid particle effective radius (microns)
!    reicmc(nlay) - ice particle effective radius (microns)
!    rescmc(nlay) - snow particle effective radius (microns)
!
!  local :
!    amd - Effective molecular weight of dry air (g/mol) 
!    amw - Molecular weight of water vapor (g/mol) 
!    amc - Molecular weight of carbon dioxide (g/mol)
!    amo - Molecular weight of ozone (g/mol)
!    amo2 - Molecular weight of oxygen (g/mol)
!    amch4 - Molecular weight of methane (g/mol)
!    amn2o - Molecular weight of nitrous oxide (g/mol)
!    amdw - Molecular weight of dry air / water vapor
!    amdc - Molecular weight of dry air / carbon dioxide
!    amdo -  Molecular weight of dry air / ozone
!    amdm - Molecular weight of dry air / methane
!    amdn -  Molecular weight of dry air / nitrous oxide
!    amdo2 - Molecular weight of dry air / oxygen
!    sbc - Stefan-Boltzmann constant (W/m2K4)
!    isp, l, ix, n, imol, ib, ig - Loop indices
!    adjflx - flux adjustment for Earth/Sun distance
!    earth_sun - function for Earth/Sun distance adjustment
!
!-------------------------------------------------------------------------------
   use parrrsw_k, only : nbndsw, ngptsw, nstr, nmol, mxmol,                    &
                          jpband, jpb1, jpb2, rrsw_scon
   use rrsw_con_k, only : heatfac, oneminus, pi, grav, avogad
   use rrsw_wvn_k, only : ng, nspa, nspb, wavenum1, wavenum2, delwave
!
! ------- Declarations -------
!
! ----- Input -----
   integer(kind=im),              intent(in   ) :: iplon          
   integer(kind=im),              intent(in   ) :: nlay         
   integer(kind=im),              intent(in   ) :: icld        
   integer(kind=im),              intent(in   ) :: iaer     
   real(kind=rb), dimension(:,:), intent(in   ) :: play 
   real(kind=rb), dimension(:,:), intent(in   ) :: plev         
   real(kind=rb), dimension(:,:), intent(in   ) :: tlay        
   real(kind=rb), dimension(:,:), intent(in   ) :: tlev      
   real(kind=rb), dimension(:),   intent(in   ) :: tsfc        
   real(kind=rb), dimension(:,:), intent(in   ) :: h2ovmr   
   real(kind=rb), dimension(:,:), intent(in   ) :: o3vmr   
   real(kind=rb), dimension(:,:), intent(in   ) :: co2vmr        
   real(kind=rb), dimension(:,:), intent(in   ) :: ch4vmr       
   real(kind=rb), dimension(:,:), intent(in   ) :: n2ovmr     
   real(kind=rb), dimension(:,:), intent(in   ) :: o2vmr    
   integer(kind=im),              intent(in   ) :: dyofyr    
   real(kind=rb),                 intent(in   ) :: adjes      
   real(kind=rb),                 intent(in   ) :: scon      
!
   integer(kind=im),              intent(in   ) :: inflgsw         
   integer(kind=im),              intent(in   ) :: iceflgsw       
   integer(kind=im),              intent(in   ) :: liqflgsw      
!
   real(kind=rb), dimension(:,:,:), intent(in   ) :: cldfmcl    
   real(kind=rb), dimension(:,:,:), intent(in   ) :: taucmcl   
   real(kind=rb), dimension(:,:,:), intent(in   ) :: ssacmcl  
   real(kind=rb), dimension(:,:,:), intent(in   ) :: asmcmcl 
   real(kind=rb), dimension(:,:,:), intent(in   ) :: fsfcmcl     
   real(kind=rb), dimension(:,:,:), intent(in   ) :: ciwpmcl  
   real(kind=rb), dimension(:,:,:), intent(in   ) :: clwpmcl   
   real(kind=rb), dimension(:,:,:), intent(in   ) :: cswpmcl  
   real(kind=rb), dimension(:,:),   intent(in   ) :: reicmcl   
   real(kind=rb), dimension(:,:),   intent(in   ) :: relqmcl  
   real(kind=rb), dimension(:,:),   intent(in   ) :: resnmcl       
!
   real(kind=rb), dimension(:,:,:), intent(in   ) :: tauaer    
   real(kind=rb), dimension(:,:,:), intent(in   ) :: ssaaer   
   real(kind=rb), dimension(:,:,:), intent(in   ) :: asmaer
!
! Atmosphere
!
   integer(kind=im),                intent(  out) :: nlayers 
   real(kind=rb), dimension(:),     intent(  out) :: pavel   
   real(kind=rb), dimension(:),     intent(  out) :: tavel 
   real(kind=rb), dimension(0:),    intent(  out) :: pz  
   real(kind=rb), dimension(0:),    intent(  out) :: tz            
   real(kind=rb),                   intent(  out) :: tbound           
   real(kind=rb), dimension(:),     intent(  out) :: pdp            
   real(kind=rb), dimension(:),     intent(  out) :: coldry        
   real(kind=rb), dimension(:,:),   intent(  out) :: wkl        
   real(kind=rb), dimension(:),     intent(  out) :: adjflux     
   real(kind=rb), dimension(:),     intent(  out) :: solvar         
   real(kind=rb), dimension(:,:),   intent(  out) :: taua        
   real(kind=rb), dimension(:,:),   intent(  out) :: ssaa      
   real(kind=rb), dimension(:,:),   intent(  out) :: asma     
!
! Atmosphere/clouds - cldprop
!
   integer(kind=im),                intent(  out) :: inflag       
   integer(kind=im),                intent(  out) :: iceflag     
   integer(kind=im),                intent(  out) :: liqflag  
!
   real(kind=rb), dimension(:,:),   intent(  out) :: cldfmc     
   real(kind=rb), dimension(:,:),   intent(  out) :: taucmc 
   real(kind=rb), dimension(:,:),   intent(  out) :: ssacmc   
   real(kind=rb), dimension(:,:),   intent(  out) :: asmcmc  
   real(kind=rb), dimension(:,:),   intent(  out) :: fsfcmc
   real(kind=rb), dimension(:,:),   intent(  out) :: ciwpmc   
   real(kind=rb), dimension(:,:),   intent(  out) :: clwpmc    
   real(kind=rb), dimension(:,:),   intent(  out) :: cswpmc
   real(kind=rb), dimension(:),     intent(  out) :: relqmc   
   real(kind=rb), dimension(:),     intent(  out) :: reicmc  
   real(kind=rb), dimension(:),     intent(  out) :: resnmc 
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
!
! Set molecular weight ratios (for converting mmr to vmr)
!  e.g. h2ovmr = h2ommr * amdw)
!
   real(kind=rb), parameter :: amdw = 1.607793_rb
   real(kind=rb), parameter :: amdc = 0.658114_rb 
   real(kind=rb), parameter :: amdo = 0.603428_rb  
   real(kind=rb), parameter :: amdm = 1.805423_rb 
   real(kind=rb), parameter :: amdn = 0.658090_rb 
   real(kind=rb), parameter :: amdo2 = 0.905140_rb
!
   real(kind=rb), parameter :: sbc = 5.67e-08_rb  
!
   integer(kind=im) :: isp, l, ix, n, imol, ib, ig
   real(kind=rb) :: amm, summol                     
   real(kind=rb) :: adjflx                       
!  real(kind=rb) :: earth_sun                 
!-------------------------------------------------------------------------------
   nlayers = nlay
!
!  Initialize all molecular amounts to zero here, then pass input amounts
!  into RRTM array WKL below.
!
   wkl(:,:) = 0.0_rb
   cldfmc(:,:) = 0.0_rb
   taucmc(:,:) = 0.0_rb
   ssacmc(:,:) = 1.0_rb
   asmcmc(:,:) = 0.0_rb
   fsfcmc(:,:) = 0.0_rb
   ciwpmc(:,:) = 0.0_rb
   clwpmc(:,:) = 0.0_rb
   cswpmc(:,:) = 0.0_rb
   reicmc(:) = 0.0_rb
   relqmc(:) = 0.0_rb
   resnmc(:) = 0.0_rb
   taua(:,:) = 0.0_rb
   ssaa(:,:) = 1.0_rb
   asma(:,:) = 0.0_rb
! 
! Set flux adjustment for current Earth/Sun distance (two options).
! 1) Use Earth/Sun distance flux adjustment provided by GCM (input as adjes);
!
   adjflx = adjes
!
! 2) Calculate Earth/Sun distance from DYOFYR, the cumulative day of the year.
!    (Set adjflx to 1. to use constant Earth/Sun distance of 1 AU). 
!
   if (dyofyr .gt. 0) then
     adjflx = earth_sun(dyofyr)
   endif
!
! Set incoming solar flux adjustment to include adjustment for
! current Earth/Sun distance (ADJFLX) and scaling of default internal
! solar constant (rrsw_scon = 1368.22 Wm-2) by band (SOLVAR).  SOLVAR can be set
! to a single scaling factor as needed, or to a different value in each 
! band, which may be necessary for paleoclimate simulations. 
! 
   do ib = jpb1,jpb2
!    solvar(ib) = 1._rb
     solvar(ib) = scon / rrsw_scon 
     adjflux(ib) = adjflx * solvar(ib)
   enddo
!
!  Set surface temperature.
!
   tbound = tsfc(iplon)
!
!  Install input GCM arrays into RRTMG_SW arrays for pressure, temperature,
!  and molecular amounts.  
!  Pressures are input in mb, or are converted to mb here.
!  Molecular amounts are input in volume mixing ratio, or are converted from 
!  mass mixing ratio (or specific humidity for h2o) to volume mixing ratio
!  here. These are then converted to molecular amount (molec/cm2) below.  
!  The dry air column COLDRY (in molec/cm2) is calculated from the level 
!  pressures, pz (in mb), based on the hydrostatic equation and includes a 
!  correction to account for h2o in the layer.  The molecular weight of moist 
!  air (amm) is calculated for each layer.  
!  Note: In RRTMG, layer indexing goes from bottom to top, and coding below
!  assumes GCM input fields are also bottom to top. Input layer indexing
!  from GCM fields should be reversed here if necessary.
!
   pz(0) = plev(iplon,1)
   tz(0) = tlev(iplon,1)
   do l = 1,nlayers
     pavel(l) = play(iplon,l)
     tavel(l) = tlay(iplon,l)
     pz(l) = plev(iplon,l+1)
     tz(l) = tlev(iplon,l+1)
     pdp(l) = pz(l-1) - pz(l)
!
! For h2o input in vmr:
     wkl(1,l) = h2ovmr(iplon,l)
! For h2o input in mmr:
!    wkl(1,l) = h2o(iplon,l)*amdw
! For h2o input in specific humidity;
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
! The following section can be used to set values for an additional layer (from
! the GCM top level to 1.e-4 mb) for improved calculation of TOA fluxes. 
! Temperature and molecular amounts in the extra model layer are set to 
! their values in the top GCM model layer, though these can be modified
! here if necessary. 
! If this feature is utilized, increase nlayers by one above, limit the two
! loops above to (nlayers-1), and set the top most (nlayers) layer values here. 
!
!      pavel(nlayers) = 0.5_rb * pz(nlayers-1)
!      tavel(nlayers) = tavel(nlayers-1)
!      pz(nlayers) = 1.e-4_rb
!      tz(nlayers-1) = 0.5_rb * (tavel(nlayers)+tavel(nlayers-1))
!      tz(nlayers) = tz(nlayers-1)
!      pdp(nlayers) = pz(nlayers-1) - pz(nlayers)
!      wkl(1,nlayers) = wkl(1,nlayers-1)
!      wkl(2,nlayers) = wkl(2,nlayers-1)
!      wkl(3,nlayers) = wkl(3,nlayers-1)
!      wkl(4,nlayers) = wkl(4,nlayers-1)
!      wkl(6,nlayers) = wkl(6,nlayers-1)
!      wkl(7,nlayers) = wkl(7,nlayers-1)
!      amm = (1._rb - wkl(1,nlayers-1)) * amd + wkl(1,nlayers-1) * amw
!      coldry(nlayers) = (pz(nlayers-1)) * 1.e3_rb * avogad /                  &
!                        (1.e2_rb * grav * amm * (1._rb + wkl(1,nlayers-1)))
!
! At this point all molecular amounts in wkl are in volume mixing ratio; 
! convert to molec/cm2 based on coldry for use in rrtm.  
!
   do l = 1,nlayers
     do imol = 1,nmol
       wkl(imol,l) = coldry(l) * wkl(imol,l)
     enddo
   enddo
!
! Transfer aerosol optical properties to RRTM variables;
! modify to reverse layer indexing here if necessary.
!
   if (iaer .ge. 1) then 
     do l = 1,nlayers
       do ib = 1,nbndsw
         taua(l,ib) = tauaer(iplon,l,ib)
         ssaa(l,ib) = ssaaer(iplon,l,ib)
         asma(l,ib) = asmaer(iplon,l,ib)
       enddo
     enddo
   endif
!
! Transfer cloud fraction and cloud optical properties to RRTM variables;
! modify to reverse layer indexing here if necessary.
!
   if (icld .ge. 1) then 
     inflag = inflgsw
     iceflag = iceflgsw
     liqflag = liqflgsw
!
! Move incoming GCM cloud arrays to RRTMG cloud arrays.
! For GCM input, incoming reicmcl is defined based on selected ice 
! parameterization (inflgsw)
!
     do l = 1,nlayers
       do ig = 1,ngptsw
         cldfmc(ig,l) = cldfmcl(ig,iplon,l)
         taucmc(ig,l) = taucmcl(ig,iplon,l)
         ssacmc(ig,l) = ssacmcl(ig,iplon,l)
         asmcmc(ig,l) = asmcmcl(ig,iplon,l)
         fsfcmc(ig,l) = fsfcmcl(ig,iplon,l)
         ciwpmc(ig,l) = ciwpmcl(ig,iplon,l)
         clwpmc(ig,l) = clwpmcl(ig,iplon,l)
         if(iceflag.eq.5) then
           cswpmc(ig,l) = cswpmcl(ig,iplon,l)
         endif
       enddo
       reicmc(l) = reicmcl(iplon,l)
       relqmc(l) = relqmcl(iplon,l)
       if(iceflag.eq.5) then
         resnmc(l) = resnmcl(iplon,l)
       endif
     enddo
!
! If an extra layer is being used in RRTMG, set all cloud properties to zero in 
! the extra layer.
!
!         cldfmc(:,nlayers) = 0.0_rb
!         taucmc(:,nlayers) = 0.0_rb
!         ssacmc(:,nlayers) = 1.0_rb
!         asmcmc(:,nlayers) = 0.0_rb
!         fsfcmc(:,nlayers) = 0.0_rb
!         ciwpmc(:,nlayers) = 0.0_rb
!         clwpmc(:,nlayers) = 0.0_rb
!         reicmc(nlayers) = 0.0_rb
!         relqmc(nlayers) = 0.0_rb
!      
   endif
!
   end subroutine inatm_sw
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module rrtmg_sw_rad_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   module module_ra_rrtmg_swk
!-------------------------------------------------------------------------------
   use module_model_constants,  only: cp, rd=>r_d, t0c=>SVPT0
   use parrrsw_k, only : nbndsw, ngptsw, naerec
   use parrrtm_k, only : nbndlw, ngptlw
   use rrtmg_sw_init_k, only: rrtmg_sw_ini
   use rrtmg_sw_rad_k, only: rrtmg_sw
   use rrtmg_lw_rad_k, only :rrtmg_lw
   use mcica_subcol_gen_k, only: mcica_subcol
   use module_ra_rrtmg_lwk, only : inirad, relcalc, reicalc
   use module_ra_effective_radius
!
contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rad_rrtmg_driver(                                                &
                       rthratensw,rthratenlw,                                  &
                       lwupflx, lwupflxc, lwdnflx, lwdnflxc,                   &
                       swupflx, swupflxc, swdnflx, swdnflxc,                   &
                       lwupt, lwuptc, lwdnt, lwdntc,                           &
                       lwupb, lwupbc, lwdnb, lwdnbc,                           &
                       glw, olr, lwcf,                                         &
                       swupt, swuptc, swdnt, swdntc,                           &
                       swupb, swupbc, swdnb, swdnbc,                           &
                       gsw, swcf, cosz, solcon,                                &
                       albedo,                                                 &
                       emiss,                                                  &
                       t3d, t8w, tsk,                                          &
                       rho3d,                                                  &
                       p3d, p8w, cldfra3d, r, g,                               &
                       nc3d, xland,                                            &
                       f_qnc, f_qv, f_qc, f_qr, f_qi, f_qs, f_qg,              &
                       qv3d, qc3d, qr3d, qi3d, qs3d, qg3d,                     &
!sh
                       o3input, o33d,                                          &
                       aer_opt, aerod, no_src,                                 &
!sh        
                       ids,ide, jds,jde, kds,kde,                              &
                       ims,ime, jms,jme, kms,kme,                              &
                       its,ite, jts,jte, kts,kte                               )
!-------------------------------------------------------------------------------
! abstract : unified rrtmg sw lw driver
!
! history log : 
!  2016-3-10   sunghye baek initial setup 
!  2017-1-15   sunghye baek wrf format correction
!-------------------------------------------------------------------------------
!
   implicit none
!
   integer, parameter :: natype=5
   real, parameter    :: qmin=0.

   logical, intent(in   ) ::  f_qnc, f_qv, f_qc, f_qr, f_qi, f_qs, f_qg
   integer, intent(in   ) ::  ids,ide, jds,jde, kds,kde              
   integer, intent(in   ) ::  ims,ime, jms,jme, kms,kme
   integer, intent(in   ) ::  its,ite, jts,jte, kts,kte
   integer, intent(in   ) ::  no_src
   integer, optional, intent(in   ) :: o3input, aer_opt

   real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: rthratensw
   real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: rthratenlw
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: lwupflx
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: lwupflxc
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: lwdnflx
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: lwdnflxc
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: swupflx
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: swupflxc
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: swdnflx
   real, dimension(ims:ime,kms:kme+2,jms:jme), optional, intent(inout) :: swdnflxc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: lwupt, lwuptc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: lwdnt, lwdntc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: lwupb, lwupbc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: lwdnb, lwdnbc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: glw, olr, lwcf
   real, dimension(ims:ime,jms:jme),         intent(inout) :: swupt, swuptc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: swdnt, swdntc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: swupb, swupbc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: swdnb, swdnbc
   real, dimension(ims:ime,jms:jme),         intent(inout) :: gsw, swcf
   real, dimension(ims:ime,jms:jme),         intent(in   ) :: cosz
   real,                                     intent(in   ) :: solcon 
   real, dimension(ims:ime,jms:jme),         intent(in   ) :: albedo, emiss
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: t8w, p8w
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: t3d, p3d       
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: rho3d
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: nc3d
   real, dimension(ims:ime,jms:jme),         intent(in   ) :: xland, tsk
   real,                                     intent(in   ) :: r,g
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: cldfra3d
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: qv3d, qc3d, qr3d
   real, dimension(ims:ime,kms:kme,jms:jme), intent(in   ) :: qi3d, qs3d, qg3d
   real, dimension(ims:ime,kms:kme,jms:jme), optional, intent(in   ) :: o33d
   real, dimension(ims:ime,kms:kme,jms:jme,1:no_src), optional, intent(in   )&
                                                           :: aerod 


!  xland 1 for land 2 for water 

!   real, dimension(ims:ime,jms:jme),         intent(in   ) :: aluvb, aluvd 
!   real, dimension(ims:ime,jms:jme),         intent(in   ) :: alnirb, alnird           

!
! Added local arrays for RRTMG
!
   integer :: ncol, nlay, icld
   integer :: inflgsw, iceflgsw, liqflgsw
   integer :: inflglw, iceflglw, liqflglw
!
! Dimension with extra layer from model top to TOA
!
   real, dimension(1,kts:kte+2) :: plev
   real, dimension(1,kts:kte+2) :: tlev
   real, dimension(1,kts:kte+1) :: play
   real, dimension(1,kts:kte+1) :: tlay
   real, dimension(1,kts:kte+1) :: h2ovmr, o3vmr, co2vmr, o2vmr, ch4vmr, n2ovmr
   real, dimension(1,kts:kte+1) :: cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr
   real, dimension(kts:kte+1)   :: o3mmr
!
! Surface albedo (for UV/visible and near-IR spectral regions,
! and for direct and diffuse radiation)
!`
   real, dimension(1) :: asdir, asdif, aldir, aldif
   real, dimension(1) :: visdir, visdif, nirdir, nirdif 
!
! Surface emissivity (for 16 LW spectral bands)
!
   real, dimension(1,nbndlw)    :: emis
!
! Dimension with extra layer from model top to TOA, 
! though no clouds are allowed in extra layer
!
   real, dimension(1,kts:kte+1)  :: cldfrac
   real, dimension(1,kts:kte+1)  :: clwpth, ciwpth
   real, dimension(1,kts:kte+1)  :: rel, rei
   real, dimension(1,kts:kte+1)  :: cswpth, res
   real, dimension(ngptsw,1,kts:kte+1)  :: staucmcl, sssacmcl
   real, dimension(ngptsw,1,kts:kte+1)  :: sasmcmcl, sfsfcmcl
   real, dimension(ngptlw,1,kts:kte+1)  :: lcldfmcl, ltaucmcl
   real, dimension(ngptlw,1,kts:kte+1)  :: lclwpmcl, lciwpmcl 
   real, dimension(ngptlw,1,kts:kte+1)  :: lcswpmcl
   real, dimension(1,kts:kte+1,nbndsw)  :: stauaer, ssaaer, asmaer
   real, dimension(1,kts:kte+1,nbndlw)  :: ltauaer
   real, dimension(1,kts:kte+1)         :: qo31d
   real, dimension(1,kts:kte+1,naerec)  :: ecaer 
   real, dimension(1,kts:kte+1)           :: co2_t
   integer, parameter                   :: has_reqc = 1
   integer, parameter                   :: has_reqi = 1
   integer, parameter                   :: has_reqs = 1
   real                                 :: pi,third,relconst,lwpmin,rhoh2o
!
! Output arrays contain extra layer from model top to TOA
!
   real, dimension(1,kts:kte+2)         :: swuflx
   real, dimension(1,kts:kte+2)         :: swdflx
   real, dimension(1,kts:kte+2)         :: swuflxc
   real, dimension(1,kts:kte+2)         :: swdflxc
   real, dimension(1,kts:kte+1)         :: swhr
   real, dimension(1,kts:kte+1)         :: swhrc
   real, dimension(1)                   :: tsfc, ps, coszen
   real, dimension(1,kts:kte+2)         :: uflx
   real, dimension(1,kts:kte+2)         :: dflx
   real, dimension(1,kts:kte+2)         :: uflxc
   real, dimension(1,kts:kte+2)         :: dflxc
   real, dimension(1,kts:kte+1)         :: hr
   real, dimension(1,kts:kte+1)         :: hrc
   real                                 :: ro, dz, adjes
   real                                 :: landf, icef, snowd, scon
   real, dimension(kts:kte)             :: re_qc, re_qi, re_qs
   real, dimension(kts:kte)             :: o31d
   real                                 :: snow_mass_factor
   real, dimension(kts:kte)             :: qsum1d, qccps
   real, dimension(its:ite)             :: xice
   integer                              :: dyofyr


!
! Set trace gas volume mixing ratios, 2005 values, IPCC (2007)
! carbon dioxide (379 ppmv)
!
   real :: co2
   data co2 / 379.e-6 /
!
! methane (1774 ppbv)
!
   real :: ch4
   data ch4 / 1774.e-9 /
!
! nitrous oxide (319 ppbv)
!
   real :: n2o
   data n2o / 319.e-9 /
!
! cfc-11 (251 ppt)
!
   real :: cfc11
   data cfc11 / 0.251e-9 /
!
! cfc-12 (538 ppt)
   real :: cfc12
   data cfc12 / 0.538e-9 /
!
! cfc-22 (169 ppt)
!
   real :: cfc22
   data cfc22 / 0.169e-9 /
!
! ccl4 (93 ppt)
!
   real :: ccl4
   data ccl4 / 0.093e-9 /
!
! Set oxygen volume mixing ratio (for o2mmr=0.23143)
!
   real :: o2
   data o2 / 0.209488 /
!
   integer :: iplon, irng, permuteseed
   integer :: nb
!
! For old cloud property specification for rrtm_lw
! Cloud and precipitation absorption coefficients
!
   real :: abcw, abice, abrn, absn
   data abcw /0.144/
   data abice /0.0735/
   data abrn /0.330e-3/
   data absn /2.34e-3/
   real :: amdw     ! Molecular weight of dry air / water vapor  
   real :: amdo     ! Molecular weight of dry air / ozone
   real :: amdo2    ! Molecular weight of dry air / oxygen
   data amdw /  1.607793 /
   data amdo /  0.603461 /
   data amdo2 / 0.905190 /
!
   real, dimension(kts:kte) :: pdel  ! mb
   real, dimension(kts:kte) :: cicewp, cliqwp   
   real, dimension(kts:kte) :: csnowp
   real, dimension(kts:kte) :: cldfra1d
   real :: gsnowp, gliqwp, gicewp, gravmks, gravdvd
   real :: fp
   real :: coszrs                    
   logical, dimension(its:ite, jts:jte) :: dorrsw             
   integer :: pver
   real :: xt24, tloctm, hrang, xxlat
   integer :: i, j, k, na
   logical :: predicate

!-------------------------------------------------------------------------------
!
! Zero out cloud optical properties here, calculated in radiation 
!
   staucmcl = 0.0
   sssacmcl = 1.0
   sasmcmcl = 0.0
   sfsfcmcl = 0.0

!
! Aerosol effects. Added aerosol feedbacks from Chem , 03/2010 -czhao
!
   stauaer = 0.
   ssaaer = 1.
   asmaer = 0.
   ltauaer =0.
!
   ltaucmcl = 0.
!
   qo31d = 0.
   co2_t = 0.0
   ecaer = 0.0
   o31d = 0.0 
!
!
!---------------------------------------------------------------------------------

!
!-----CALCULATE SHORT & LONG WAVE RADIATION
!                                                              
! All fields are ordered vertically from bottom to top
! Pressures are in mb
!
!
! Set solar constant
!
   scon = solcon
!
! For Earth/Sun distance adjustment in RRTMG
!         dyofyr = julday
!         adjes = 0.0 
!  solar constant is already provided with eccentricity adjustment,
! so do not do this in RRTMG
!
   dyofyr = 0
   adjes = 1.0
   ncol = 0
   pver = kte - kts + 1
!SET CONST here for fast computation 
   gravmks = g
   gravdvd = 1000./g
   dorrsw = .true.
!
   do j = jts,jte
     do i = its,ite
      if(cosz(i,j).le.0.0) dorrsw(i,j) = .false.
     enddo
   enddo
!
! Add extra layer from top of model to top of atmosphere
!
   nlay = (kte - kts + 1) + 1
!
! For passing in cloud physical properties; cloud optics parameterized in RRTMG:
!
   icld = 2
   inflgsw = 2
   iceflgsw = 3
   liqflgsw = 1
!
   inflglw = 2
   iceflglw = 3
   liqflglw = 1
!
! START LOOP FOR SETTING CLOUD PROPERTY
!
   ncol=0
   do j = jts,jte 
     do i = its,ite
!
!-------------------------------------------------
!   1. SET SURFACE PROPERTY
!----------------- --------------------------------
!

!
!-------------------------------------------------
!   2. SET AEROSOL PROPERTY
!-------------------------------------------------
!
!       do na = 1,natype
!         aod_t(1,kts:kte,na) = aod_t3d(i,kts:kte,na)
!         aod_t(1,kte+1,na) = 0.
!       enddo
!
!-------------------------------------------------
!   3. SET OZONE PROPERTY
!-------------------------------------------------
!
!       do k = kts,kte
!         qo31d(1,k) = qo3_3d(i,k,j)
!       enddo
!
!       qo31d(1,kte+1) = qo31d(1,kte,j)
!
!-------------------------------------------------
!   4. SET CO2 PROPERTY
!-------------------------------------------------
!
!       do k = kts,kte
!         co2_t(1,k) = co2_3d(i,k)
!       enddo
!
!       co2_t(1,kte+1) = co2_t(1,kte)
!
!-------------------------------------------------
!   5. SET OPTICAL CLOUD PROPERTY
!-------------------------------------------------
!
!               
!
!-------------------------------------------------
!   5-1. SET EFFECTIVE RADIUS
!-------------------------------------------------
!
       do k = kts,kte
         qsum1d(k) = qi3d(i,k,j)+qc3d(i,k,j)+qs3d(i,k,j)
         cldfra1d(k) = cldfra3d(i,k,j)
       enddo
!
       re_qc = 5.0e-6
       re_qi = 10.0e-6
       re_qs = 25.0e-6
!

       if(f_qc) then
         inflgsw = 3  ;  inflglw = 3  
       endif

       if(f_qi) then
         inflgsw  = 4 ;  iceflgsw = 4
         inflglw  = 4 ;  iceflglw = 4
       endif

       if(f_qs) then
         inflgsw  = 5 ;  iceflgsw = 5
         inflglw  = 5 ;  iceflglw = 5 
       endif


       qccps = 0.e0

       call effectRad(t3d(i,kts:kte,j), qc3d(i,kts:kte,j), nc3d(i,kts:kte,j),  &
                      qi3d(i,kts:kte,j), qs3d(i,kts:kte,j), qg3d(i,kts:kte,j), &
                      rho3d(i,kts:kte,j), qmin, t0c, qccps, f_qnc,             &
                      re_qc, re_qi, re_qs, kts, kte) 

       re_qc = re_qc * 1.e+6
       re_qi = re_qi * 1.e+6
       re_qs = re_qs * 1.e+6

       re_qc(kts:kte)  = max(2.51,  min(re_qc(kts:kte),  50.))
       re_qi(kts:kte)  = max(10.01, min(re_qi(kts:kte), 125.))
       re_qs(kts:kte)  = max(25.,   min(re_qs(kts:kte), 999.))
 
!-------------------------------------------------
!   5-2. SET CLOUD HYDROMETEOR PROPERTY
!-------------------------------------------------
!
! From module_ra_cam: Convert liquid and ice mixing ratios to water paths;
! Water paths are in units of g/m2
! snow added as ice cloud (JD 091022)
!
!
! pdel is in Pa here <==========
!
       do k = kts,kte
         pdel(k) = p8w(i,k,j)-p8w(i,k+1,j)
       enddo
!
!       if (p8w(i,kte+1).eq.0.) pdel(kte) = p8w(i,kte) - 1.e-2
!
       do k = kts,kte
!        Grid box ice water path.
         gicewp = (qi3d(i,k,j)+qs3d(i,k,j)) * pdel(k) * gravdvd 
!        Grid box liquid water path.
         gliqwp = qc3d(i,k,j) * pdel(k) * gravdvd   
!        in-cloud ice water path.
         cicewp(k) = gicewp / max(0.01,cldfra1d(k)) 
!        In-cloud liquid water path.  
         cliqwp(k) = gliqwp / max(0.01,cldfra1d(k)) 
         if(cldfra1d(k).gt.0. .and. qsum1d(k).lt.1.e-9) then
           gliqwp = 1.e-9* pdel(k) * gravdvd
           cliqwp(k) = gliqwp     ! In-cloud liquid water path.
         endif
       enddo
! The ice water path is already sum of cloud ice and snow, but when we have 
! explicit ice effective radius, overwrite the ice path with only the cloud 
! ice variable, leaving out the snow for its own effect.
!
       if (iceflgsw .ge. 4) then
         do k = kts,kte
           gicewp = qi3d(i,k,j)*pdel(k)* gravdvd    ! Grid box ice water path.
           cicewp(k) = gicewp/max(0.01,cldfra1d(k))    ! In-cloud ice water path.
           if(cldfra1d(k).gt.0. .and. qsum1d(k).lt.1.e-9) then
             gicewp = 1.e-9*pdel(k)* gravdvd
             cicewp(k) = gicewp      ! In-cloud ice water path. 
           endif
         enddo
       end if
!
!
! Here the snow path is adjusted if (radiation) effective radius of snow is
! larger than what we currently have in the lookup tables.  Since mass goes
! rather close to diameter squared, adjust the mixing ratio of snow used
! to compute its water path in combination with the max diameter.  Not a
! perfect fix, but certainly better than using all snow mass when diameter is
! far larger than table currently contains and crystal sizes much larger than
! about 140 microns have lesser impact than those much smaller sizes.
!
!
       if (iceflgsw.eq.5) then
         do k = kts,kte
           snow_mass_factor = 1.0
           if (re_qs(k).gt.130.)then 
             snow_mass_factor =(130.0/re_qs(k))**2.  
             re_qs(k)=130.
           endif
           ! Grid box snow water path.
           gsnowp = (qs3d(i,k,j)+qg3d(i,k,j))*snow_mass_factor*pdel(k)* gravdvd
           if (cldfra1d(k).gt.0. .and. qsum1d(k).lt.1.e-9) then
             gsnowp = 0.0
           endif
           csnowp(k) = gsnowp/max(0.01,cldfra1d(k))
         enddo
       endif
!
! Limit upper bound of reice for Fu ice parameterization and convert
! from effective radius to generalized effective size (*1.0315; Fu, 1996)
!
       if (iceflgsw.eq.3) then
         do k = kts,kte
           re_qi(k) = re_qi(k)*1.0315
           re_qi(k) = min(140.0,re_qi(k))
         enddo
       endif
!
!-------------------------------------------------
!   6. FILLING ARRAYS FOR RRTMG_SW and RRTMG_LW
!-------------------------------------------------
       ncol=1
       play(ncol, kts:kte )   = p3d(i,kts:kte,j)*1.e-2
       plev(ncol, kts:kte+1 ) = p8w(i,kts:kte+1,j)*1.e-2
       tlay(ncol, kts:kte )   = t3d(i,kts:kte,j)
       tlev(ncol, kts:kte+1)  = t8w(i,kts:kte+1,j)
       tsfc(ncol)             = tsk(i,j)
       h2ovmr(ncol, kts:kte ) = amax1(max(0.,qv3d(i,kts:kte,j)),3.0e-6) * amdw
       co2vmr(ncol, kts:kte ) = co2
       o2vmr(ncol, kts:kte )  = o2
       ch4vmr(ncol, kts:kte ) = ch4
       n2ovmr(ncol, kts:kte ) = n2o
       cfc11vmr(ncol,kts:kte) = cfc11
       cfc12vmr(ncol,kts:kte) = cfc12
       cfc22vmr(ncol,kts:kte) = cfc22
       ccl4vmr(ncol,kts:kte)  = ccl4
!  Define profile values for extra layer from model top to top of atmosphere. 
!  The top layer temperature for all gridpoints is set to the top layer-1 
!  temperature plus a constant (0 K) that represents an isothermal layer    
!  above ptop.  Top layer interface temperatures are linearly interpolated 
!  from the layer temperatures.  
       play(ncol,kte+1) = 0.5 * plev(ncol,kte+1)
       tlay(ncol,kte+1) = tlev(ncol,kte+1) + 0.0
       plev(ncol,kte+2) = 1.0e-5
       tlev(ncol,kte+2) = tlev(ncol,kte+1) + 0.0
       h2ovmr(ncol,kte+1) = h2ovmr(ncol,kte)
       co2vmr(ncol,kte+1) = co2vmr(ncol,kte)
       o2vmr(ncol,kte+1) = o2vmr(ncol,kte)
       ch4vmr(ncol,kte+1) = ch4vmr(ncol,kte)
       n2ovmr(ncol,kte+1) = n2ovmr(ncol,kte)
       cfc11vmr(ncol,kte+1) = cfc11vmr(ncol,kte)
       cfc12vmr(ncol,kte+1) = cfc12vmr(ncol,kte)
       cfc22vmr(ncol,kte+1) = cfc22vmr(ncol,kte)
       ccl4vmr(ncol,kte+1) = ccl4vmr(ncol,kte)
!
!       do k = kts,kte
!         o3vmr(ncol,k) = qo31d(1,k)*amdo              ! convert mmr to vmr
!       enddo
!
!       o3vmr(ncol,kte+1) = o3vmr(ncol,kte)
!
       call inirad(o3mmr,plev,kts,kte)     
       do k = kts,kte+1
           o3vmr(ncol,k) = o3mmr(k)*amdo
       enddo

       if(present(o33d).and.o3input.eq.2) then
         do k = kts,kte
            o31d(k) = o33d(i,k,j)
            o3vmr(ncol,k) = o31d(k) 
         enddo

         o3vmr(ncol,kte+1) = o31d(kte) - o3mmr(kte)*amdo + o3mmr(kte+1)*amdo
         if(o3vmr(ncol,kte+1) .le. 0.) o3vmr(ncol,kte+1) = o3mmr(kte+1)*amdo
       endif


!       do k = kts,kte
!         co2vmr(ncol,k) = co2_t(1,k)
!       enddo
!       co2vmr(ncol,kte+1) = co2vmr(ncol,kte)

!
!
! Set surface albedo for direct and diffuse radiation in UV/visible and
! near-IR spectral regions
! 
       asdir(ncol) = albedo(i,j)
       asdif(ncol) = albedo(i,j)
       aldir(ncol) = albedo(i,j)
       aldif(ncol) = albedo(i,j)
!
! Set surface emissivity in each RRTMG longwave band
!
       emis(ncol,:) = emiss(i,j)
!
! Set cloud physical property arrays
!
       clwpth(ncol,kts:kte) = cliqwp(kts:kte)
       ciwpth(ncol,kts:kte) = cicewp(kts:kte)
       rel(ncol,kts:kte) = re_qc(kts:kte)
       rei(ncol,kts:kte) = re_qi(kts:kte)
!
       if (inflgsw .eq. 5) then
         cswpth(ncol,kts:kte) = csnowp(kts:kte)
         res(ncol,kts:kte) = re_qs(kts:kte)
       else
         cswpth(ncol,kts:kte) = 0.0
         res(ncol,kts:kte) = 10.0
       endif
! Set cosine of solar zenith angle
       coszen(ncol) = cosz(i,j)
! Layer indexing goes bottom to top here for all fields.
! Water vapor and ozone are converted from mmr to vmr. 
! Pressures are in units of mb here. 
       cldfrac(ncol,kts:kte) = cldfra1d(kts:kte)
!
! No clouds are allowed in the extra layer from model top to TOA
!
       clwpth(ncol,kte+1) = 0.
       ciwpth(ncol,kte+1) = 0.
       cswpth(ncol,kte+1) = 0.
       rel(ncol,kte+1) = 10.
       rei(ncol,kte+1) = 10.
       res(ncol,kte+1) = 10.
       cldfrac(ncol,kte+1) = 0.
       ecaer = 0.e0
       iplon = ncol
       irng = 0

       permuteseed = 1
       call mcica_subcol (iplon, ncol, nlay, icld, permuteseed, irng, play,    &
                                                 cldfrac,   ciwpth, clwpth,    &
                                                        lciwpmcl, lclwpmcl,    &
                                                         cswpth,  lcswpmcl,    &
                                                                  lcldfmcl)
 
!
       if (dorrsw(i,j)) then

         if (present(aerod)) then
           if (aer_opt.eq.1) then
             do na = 1,naerec
                do k = kts, kte
                 ecaer(ncol,k,na) = aerod(i,k,j,na)
                enddo
             enddo
           endif
         endif

         call rrtmg_sw                                                         &
                (ncol    ,nlay    ,icld    ,                                   &
                 play    ,plev    ,tlay    ,tlev    ,tsfc   ,                  &
                 h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr ,           &
                 asdir   ,asdif   ,aldir   ,aldif   ,                          &
                 coszen  ,adjes   ,dyofyr  ,scon    ,                          &
                 inflgsw ,iceflgsw,liqflgsw,lcldfmcl(1:ngptsw,:,:) ,           &
                 staucmcl ,sssacmcl ,sasmcmcl ,sfsfcmcl ,                      &
                 lciwpmcl(1:ngptsw,:,:) ,lclwpmcl(1:ngptsw,:,:), rei ,rel,     &
                                              lcswpmcl(1:ngptsw,:,:), res,     &
                 stauaer  ,ssaaer  ,asmaer  ,ecaer   ,                         &
                 swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc ,          &
                 visdir  ,visdif  ,nirdir  ,nirdif )

         rthratensw(i,kts:kte,j) = swhr(1,kts:kte)/86400.
         if(present(swupflx))then
           swupflx(i,kts:kte+2,j) = swuflx(1,kts:kte+2)
           swupflxc(i,kts:kte+2,j) = swuflxc(1,kts:kte+2)
           swdnflx(i,kts:kte+2,j) = swdflx(1,kts:kte+2)
           swdnflxc(i,kts:kte+2,j) = swdflxc(1,kts:kte+2)
         endif
         gsw(i,j) = swdflx(1,1) - swuflx(1,1)
         swcf(i,j) = (swdflx(1,kte+2) - swuflx(1,kte+2)) - (swdflxc(1,kte+2)   &
                     - swuflxc(1,kte+2)) 
         swupt(i,j)     = swuflx(1,kte+2)
         swuptc(i,j)    = swuflxc(1,kte+2)
         swdnt(i,j)     = swdflx(1,kte+2)
         swdntc(i,j)    = swdflxc(1,kte+2)
         swupb(i,j)     = swuflx(1,1)
         swupbc(i,j)    = swuflxc(1,1)
         swdnb(i,j)     = swdflx(1,1)
         swdnbc(i,j)    = swdflxc(1,1)
       endif ! do SW
!
! Call RRTMG longwave radiation model
!
       call rrtmg_lw                                                           &
                     (ncol, nlay, icld, play, plev , tlay,                     &
                      tlev ,tsfc, h2ovmr, o3vmr, co2vmr, ch4vmr,               &
                      n2ovmr, o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr,    &
                      emis, inflglw, iceflglw, liqflglw, lcldfmcl,             &
                      ltaucmcl, lciwpmcl, lclwpmcl, rei, rel,                  &
                      lcswpmcl, res,                                           &
                      ltauaer,                                                 &
                      uflx, dflx, hr, uflxc, dflxc, hrc)
!
       rthratenlw(i,kts:kte,j) = hr(1,kts:kte)/86400.
       if(present(lwupflx))then
         lwupflx(i,kts:kte+2,j) = uflx(1,kts:kte+2)
         lwupflxc(i,kts:kte+2,j) = uflxc(1,kts:kte+2)
         lwdnflx(i,kts:kte+2,j) = dflx(1,kts:kte+2)
         lwdnflxc(i,kts:kte+2,j) = dflxc(1,kts:kte+2)
       endif
       glw(i,j) = dflx(1,1) 
       olr(i,j) = uflx(1,kte+2)
       lwcf(i,j) = uflxc(1,kte+2) - uflx(1,kte+2)
       lwupt(i,j)     = uflx(1,kte+2)
       lwuptc(i,j)    = uflxc(1,kte+2)
       lwdnt(i,j)     = dflx(1,kte+2)
       lwdntc(i,j)    = dflxc(1,kte+2)
       lwupb(i,j)     = uflx(1,1)
       lwupbc(i,j)    = uflxc(1,1)
       lwdnb(i,j)     = dflx(1,1)
       lwdnbc(i,j)    = dflxc(1,1)
     enddo ! jts:jte end j LOOP 
   enddo ! its:ite end GRAND LOOP
   end subroutine rad_rrtmg_driver
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   subroutine rrtmg_swinit_k(                                                  &
                                allowed_to_read ,                              &
                                ids, ide, jds, jde, kds, kde,                  &
                                ims, ime, jms, jme, kms, kme,                  &
                                its, ite, jts, jte, kts, kte  )
!-------------------------------------------------------------------------------
   use rrtmg_sw_init_k
!
   implicit none
!
   logical, intent(in)            :: allowed_to_read
   integer, intent(in)             :: ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,             &
                                     its, ite, jts, jte, kts, kte
!-------------------------------------------------------------------------------
!
! Read in absorption coefficients and other data
!
   IF ( allowed_to_read ) then
     call rrtmg_swlookuptable
   endif
!
! Perform g-point reduction and other initializations
! Specific heat of dry air (cp) used in flux to heating rate conversion factor.
!
   call rrtmg_sw_ini(cp)
!
   end subroutine rrtmg_swinit_k
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine rrtmg_swlookuptable
!
   implicit none
!
! Local                                    
!
   integer :: i
   logical                 :: opened
   logical , external      :: wrf_dm_on_monitor
   character*80 errmess
   integer rrtmg_unit
!-------------------------------------------------------------------------------
   if ( wrf_dm_on_monitor() ) then
     do i = 10,99
       inquire ( i , opened = opened )
       if ( .not. opened ) then
         rrtmg_unit = i
         goto 2010
       endif
     enddo
     rrtmg_unit = -1
2010   continue
   endif
   call wrf_dm_bcast_bytes ( rrtmg_unit , 4 )


   if ( wrf_dm_on_monitor() ) then
     open(rrtmg_unit,file='RRTMG_SW_DATA',                                     &
           form='UNFORMATTED',status='OLD',err=9009)
   endif
   call sw_kgb16(rrtmg_unit)
   call sw_kgb17(rrtmg_unit)
   call sw_kgb18(rrtmg_unit)
   call sw_kgb19(rrtmg_unit)
   call sw_kgb20(rrtmg_unit)
   call sw_kgb21(rrtmg_unit)
   call sw_kgb22(rrtmg_unit)
   call sw_kgb23(rrtmg_unit)
   call sw_kgb24(rrtmg_unit)
   call sw_kgb25(rrtmg_unit)
   call sw_kgb26(rrtmg_unit)
   call sw_kgb27(rrtmg_unit)
   call sw_kgb28(rrtmg_unit)
   call sw_kgb29(rrtmg_unit)
!
   if ( wrf_dm_on_monitor() ) close (rrtmg_unit)
!
   return
9009 continue
   write( errmess , '(A,I4)' )                                                 &
        'module_ra_rrtmg_sw: error opening RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine rrtmg_swlookuptable
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
! **************************************************************************
!  RRTMG Shortwave Radiative Transfer Model
!  Atmospheric and Environmental Research, Inc., Cambridge, MA
!
!  Original by J.Delamere, Atmospheric & Environmental Research.
!  Reformatted for F90: JJMorcrette, ECMWF
!  Revision for GCMs:  Michael J. Iacono, AER, July 2002
!  Further F90 reformatting:  Michael J. Iacono, AER, June 2006
!
!  This file contains 14 READ statements that include the 
!  absorption coefficients and other data for each of the 14 shortwave
!  spectral bands used in RRTMG_SW.  Here, the data are defined for 16
!  g-points, or sub-intervals, per band.  These data are combined and
!  weighted using a mapping procedure in module RRTMG_SW_INIT to reduce
!  the total number of g-points from 224 to 112 for use in the GCM.
! **************************************************************************
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb16(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 2925 cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg16_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            rayl, strrat1, layreffr
!
  implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, strrat1, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_real ( strrat1 , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
     'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb16
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb17(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 3625 cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg17_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            rayl, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
         rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_real ( strrat , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
         'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb17
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb18(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 4325 cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
  use rrsw_kg18_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,              &
                            rayl, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_real ( strrat , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
       'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb18 
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb19(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 4900 cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg19_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            rayl, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_real ( strrat , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
         'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb19
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb20(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 5670 cm-1.
!
!     Array absch4o contains the absorption coefficients for methane.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg20_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absch4o, rayl, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, layreffr, absch4o, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( absch4o , size ( absch4o ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
       'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb20
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb21(rrtmg_unit)
!-------------------------------------------------------------------------------
!
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 6925 cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!
!-------------------------------------------------------------------------------
   use rrsw_kg21_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            rayl, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_real ( strrat , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
       'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb21
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb22(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at v = 8000 cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296_rb,260_rb,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg22_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            rayl, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_real ( rayl , 1 )
   call wrf_dm_bcast_real ( strrat , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
      'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb22
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb23(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array raylo contains the Rayleigh extinction coefficient at all v for 
!     this band
!
!     Array givfac is the average Giver et al. correction factor for this band. 
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg23_k, only : kao, selfrefo, forrefo, sfluxrefo,                  &
                            raylo, givfac, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     raylo, givfac, layreffr, kao, selfrefo, forrefo, sfluxrefo
   call wrf_dm_bcast_bytes ( raylo , size ( raylo ) * 4 )
   call wrf_dm_bcast_real ( givfac , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
     'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb23
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb24(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Arrays raylao and raylbo contain the Rayleigh extinction coefficient at 
!     all v for this band for the upper and lower atmosphere.
!
!     Arrays abso3ao and abso3bo contain the ozone absorption coefficient at 
!     all v for this band for the upper and lower atmosphere.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg24_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            raylao, raylbo, abso3ao, abso3bo, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     raylao, raylbo, strrat, layreffr, abso3ao, abso3bo, kao, kbo, selfrefo,   &
         forrefo, sfluxrefo
   call wrf_dm_bcast_bytes ( raylao , size ( raylao ) * 4 )
   call wrf_dm_bcast_bytes ( raylbo , size ( raylbo ) * 4 )
   call wrf_dm_bcast_real ( strrat , 1 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( abso3ao , size ( abso3ao ) * 4 )
   call wrf_dm_bcast_bytes ( abso3bo , size ( abso3bo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
   call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
   call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
     'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb24
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb25(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array raylo contains the Rayleigh extinction coefficient at all 
!     v = 2925 cm-1.
!
!     Arrays abso3ao and abso3bo contain the ozone absorption coefficient at 
!     all v for this band for the upper and lower atmosphere.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!-------------------------------------------------------------------------------
   use rrsw_kg25_k, only : kao, sfluxrefo,                                     &
                            raylo, abso3ao, abso3bo, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
         raylo, layreffr, abso3ao, abso3bo, kao, sfluxrefo
   call wrf_dm_bcast_bytes ( raylo , size ( raylo ) * 4 )
   call wrf_dm_bcast_integer ( layreffr , 1 )
   call wrf_dm_bcast_bytes ( abso3ao , size ( abso3ao ) * 4 )
   call wrf_dm_bcast_bytes ( abso3bo , size ( abso3bo ) * 4 )
   call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
   call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
        'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb25
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb26(rrtmg_unit)
!-------------------------------------------------------------------------------
   use rrsw_kg26_k, only : sfluxrefo, raylo
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array raylo contains the Rayleigh extinction coefficient at all v for 
!     this band.
!
!-------------------------------------------------------------------------------
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
         raylo, sfluxrefo
     call wrf_dm_bcast_bytes ( raylo , size ( raylo ) * 4 )
     call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
      'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb26
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb27(rrtmg_unit)
!-------------------------------------------------------------------------------
   use rrsw_kg27_k, only : kao, kbo, sfluxrefo, raylo,                         &
                            scalekur, layreffr
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!     The values in array sfluxrefo were obtained using the "low resolution"
!     version of the Kurucz solar source function.  For unknown reasons,
!     the total irradiance in this band differs from the corresponding
!     total in the "high-resolution" version of the Kurucz function.
!     Therefore, these values are scaled by the factor SCALEKUR.
!
!     Array raylo contains the Rayleigh extinction coefficient at all v = 2925 
!     cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!-------------------------------------------------------------------------------
   use rrsw_kg27_k, only : kao, kbo, sfluxrefo, raylo,                         &
                            scalekur, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
         raylo, scalekur, layreffr, kao, kbo, sfluxrefo
     call wrf_dm_bcast_bytes ( raylo , size ( raylo ) * 4 )
     call wrf_dm_bcast_real ( scalekur , 1 )
     call wrf_dm_bcast_integer ( layreffr , 1 )
     call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
     call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
     call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
       'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb27
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb28(rrtmg_unit)
!-------------------------------------------------------------------------------
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array raylo contains the Rayleigh extinction coefficient at 
!     all v = ???? cm-1.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!-------------------------------------------------------------------------------
   use rrsw_kg28_k, only : kao, kbo, sfluxrefo,                                &
                            rayl, strrat, layreffr
!
   implicit none
!
   save
!
! Input
!
   integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
   character*80 errmess
   logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
         rayl, strrat, layreffr, kao, kbo, sfluxrefo
     call wrf_dm_bcast_real ( rayl , 1 )
     call wrf_dm_bcast_real ( strrat , 1 )
     call wrf_dm_bcast_integer ( layreffr , 1 )
     call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
     call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
     call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
       'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb28
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine sw_kgb29(rrtmg_unit)
!-------------------------------------------------------------------------------
   use rrsw_kg29_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absh2oo, absco2o, rayl, layreffr
!-------------------------------------------------------------------------------
!
!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!
!     Array rayl contains the Rayleigh extinction coefficient at all 
!      v = 2200 cm-1.
!
!     Array absh2oo contains the water vapor absorption coefficient for
!      this band.
!
!     Array absco2o contains the carbon dioxide absorption coefficient for 
!     this band.
!
!     The array KAO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels> ~100mb, temperatures, and binary
!     species parameters (see taumol.f for definition).  The first 
!     index in the array, JS, runs from 1 to 9, and corresponds to 
!     different values of the binary species parameter.  For instance, 
!     JS=1 refers to dry air, JS = 2 corresponds to the paramter value 1/8, 
!     JS = 3 corresponds to the parameter value 2/8, etc.  The second index
!     in the array, JT, which runs from 1 to 5, corresponds to different
!     temperatures.  More specifically, JT = 3 means that the data are for
!     the reference temperature TREF for this  pressure level, JT = 2 refers
!     to TREF-15, JT = 1 is for TREF-30, JT = 4 is for TREF+15, and JT = 5
!     is for TREF+30.  The third index, JP, runs from 1 to 13 and refers
!     to the JPth reference pressure level (see taumol.f for these levels
!     in mb).  The fourth index, IG, goes from 1 to 16, and indicates
!     which g-interval the absorption coefficients are for.
!
!     The array KBO contains absorption coefs at the 16 chosen g-values 
!     for a range of pressure levels < ~100mb and temperatures. The first 
!     index in the array, JT, which runs from 1 to 5, corresponds to 
!     different temperatures.  More specifically, JT = 3 means that the 
!     data are for the reference temperature TREF for this pressure 
!     level, JT = 2 refers to the temperature TREF-15, JT = 1 is for
!     TREF-30, JT = 4 is for TREF+15, and JT = 5 is for TREF+30.  
!     The second index, JP, runs from 13 to 59 and refers to the JPth
!     reference pressure level (see taumol.f for the value of these
!     pressure levels in mb).  The third index, IG, goes from 1 to 16,
!     and tells us which g-interval the absorption coefficients are for.
!
!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).
!
!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).
!-------------------------------------------------------------------------------
   use rrsw_kg29_k, only : kao, kbo, selfrefo, forrefo, sfluxrefo,             &
                            absh2oo, absco2o, rayl, layreffr
!
   implicit none
!
   save
!
! Input
!
    integer, intent(in) :: rrtmg_unit
!
! Local                                    
!
    character*80 errmess
    logical, external  :: wrf_dm_on_monitor
!-------------------------------------------------------------------------------
!
   if ( wrf_dm_on_monitor() ) read (rrtmg_unit,err=9010)                       &
     rayl, layreffr, absh2oo, absco2o, kao, kbo, selfrefo, forrefo, sfluxrefo
     call wrf_dm_bcast_real ( rayl , 1 )
     call wrf_dm_bcast_integer ( layreffr , 1 )
     call wrf_dm_bcast_bytes ( absh2oo , size ( absh2oo ) * 4 )
     call wrf_dm_bcast_bytes ( absco2o , size ( absco2o ) * 4 )
     call wrf_dm_bcast_bytes ( kao , size ( kao ) * 4 )
     call wrf_dm_bcast_bytes ( kbo , size ( kbo ) * 4 )
     call wrf_dm_bcast_bytes ( selfrefo , size ( selfrefo ) * 4 )
     call wrf_dm_bcast_bytes ( forrefo , size ( forrefo ) * 4 )
     call wrf_dm_bcast_bytes ( sfluxrefo , size ( sfluxrefo ) * 4 )
!
   return
9010 continue
   write( errmess , '(A,I4)' )                                                 &
     'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
!
   end subroutine sw_kgb29
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module module_ra_rrtmg_swk
!-------------------------------------------------------------------------------
#endif
