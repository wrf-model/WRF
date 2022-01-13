!!MODULE module_ra_rrtmg_sw
                
      module parrrsw

      use parkind ,only : im => kind_im, rb => kind_rb

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw main parameters
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! mxlay  :  integer: maximum number of layers
! mg     :  integer: number of original g-intervals per spectral band
! nbndsw :  integer: number of spectral bands
! naerec :  integer: number of aerosols (iaer=6, ecmwf aerosol option)
! ngptsw :  integer: total number of reduced g-intervals for rrtmg_lw
! ngNN   :  integer: number of reduced g-intervals per spectral band
! ngsNN  :  integer: cumulative number of g-intervals per band
!------------------------------------------------------------------

      integer(kind=im), parameter :: mxlay  = 203    !jplay, klev
      integer(kind=im), parameter :: mg     = 16     !jpg
      integer(kind=im), parameter :: nbndsw = 14     !jpsw, ksw
      integer(kind=im), parameter :: naerec  = 6     !jpaer
      integer(kind=im), parameter :: mxmol  = 38
      integer(kind=im), parameter :: nstr   = 2
      integer(kind=im), parameter :: nmol   = 7
! Use for 112 g-point model   
      integer(kind=im), parameter :: ngptsw = 112    !jpgpt
! Use for 224 g-point model   
!      integer(kind=im), parameter :: ngptsw = 224   !jpgpt

! may need to rename these - from v2.6
      integer(kind=im), parameter :: jpband   = 29
      integer(kind=im), parameter :: jpb1     = 16   !istart
      integer(kind=im), parameter :: jpb2     = 29   !iend

      integer(kind=im), parameter :: jmcmu    = 32
      integer(kind=im), parameter :: jmumu    = 32
      integer(kind=im), parameter :: jmphi    = 3
      integer(kind=im), parameter :: jmxang   = 4
      integer(kind=im), parameter :: jmxstr   = 16

! Use for 112 g-point model   
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

! Use for 224 g-point model   
!      integer(kind=im), parameter :: ng16 = 16
!      integer(kind=im), parameter :: ng17 = 16
!      integer(kind=im), parameter :: ng18 = 16
!      integer(kind=im), parameter :: ng19 = 16
!      integer(kind=im), parameter :: ng20 = 16
!      integer(kind=im), parameter :: ng21 = 16
!      integer(kind=im), parameter :: ng22 = 16
!      integer(kind=im), parameter :: ng23 = 16
!      integer(kind=im), parameter :: ng24 = 16
!      integer(kind=im), parameter :: ng25 = 16
!      integer(kind=im), parameter :: ng26 = 16
!      integer(kind=im), parameter :: ng27 = 16
!      integer(kind=im), parameter :: ng28 = 16
!      integer(kind=im), parameter :: ng29 = 16

!      integer(kind=im), parameter :: ngs16 = 16
!      integer(kind=im), parameter :: ngs17 = 32
!      integer(kind=im), parameter :: ngs18 = 48
!      integer(kind=im), parameter :: ngs19 = 64
!      integer(kind=im), parameter :: ngs20 = 80
!      integer(kind=im), parameter :: ngs21 = 96
!      integer(kind=im), parameter :: ngs22 = 112
!      integer(kind=im), parameter :: ngs23 = 128
!      integer(kind=im), parameter :: ngs24 = 144
!      integer(kind=im), parameter :: ngs25 = 160
!      integer(kind=im), parameter :: ngs26 = 176
!      integer(kind=im), parameter :: ngs27 = 192
!      integer(kind=im), parameter :: ngs28 = 208
!      integer(kind=im), parameter :: ngs29 = 224

! Source function solar constant
      real(kind=rb), parameter :: rrsw_scon = 1.36822e+03     ! W/m2
 
      end module parrrsw

      module rrsw_aer

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only : nbndsw, naerec

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw aerosol optical properties
!
!  Data derived from six ECMWF aerosol types and defined for
!  the rrtmg_sw spectral intervals
!
! Initial: J.-J. Morcrette, ECMWF, mar2003
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------
!
!-- The six ECMWF aerosol types are respectively:
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
!------------------------------------------------------------------
!
!  name     type     purpose
! -----   : ----   : ----------------------------------------------
! rsrtaua : real   : ratio of average optical thickness in 
!                    spectral band to that at 0.55 micron
! rsrpiza : real   : average single scattering albedo (unitless)
! rsrasya : real   : average asymmetry parameter (unitless)
!------------------------------------------------------------------

      real(kind=rb) :: rsrtaua(nbndsw,naerec)
      real(kind=rb) :: rsrpiza(nbndsw,naerec)
      real(kind=rb) :: rsrasya(nbndsw,naerec)

      end module rrsw_aer

      module rrsw_cld

      use parkind, only : im => kind_im, rb => kind_rb

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw cloud property coefficients
!
! Initial: J.-J. Morcrette, ECMWF, oct1999
! Revised: J. Delamere/MJIacono, AER, aug2005
! Revised: MJIacono, AER, nov2005
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------
!
!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
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
!------------------------------------------------------------------

      real(kind=rb) :: extliq1(58,16:29), ssaliq1(58,16:29), asyliq1(58,16:29)
      real(kind=rb) :: extice2(43,16:29), ssaice2(43,16:29), asyice2(43,16:29)
      real(kind=rb) :: extice3(46,16:29), ssaice3(46,16:29), asyice3(46,16:29)
      real(kind=rb) :: fdlice3(46,16:29)
      real(kind=rb) :: abari(5),bbari(5),cbari(5),dbari(5),ebari(5),fbari(5)

      end module rrsw_cld

      module rrsw_con

      use parkind, only : im => kind_im, rb => kind_rb

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw constants

! Initial version: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
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
!------------------------------------------------------------------

      real(kind=rb) :: fluxfac, heatfac
      real(kind=rb) :: oneminus, pi, grav
      real(kind=rb) :: planck, boltz, clight
      real(kind=rb) :: avogad, alosmt, gascon
      real(kind=rb) :: radcn1, radcn2
      real(kind=rb) :: sbcnst, secdy

      end module rrsw_con

      module rrsw_kg16

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng16

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 16
! band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no16 = 16

      real(kind=rb) :: kao(9,5,13,no16)
      real(kind=rb) :: kbo(5,13:59,no16)
      real(kind=rb) :: selfrefo(10,no16), forrefo(3,no16)
      real(kind=rb) :: sfluxrefo(no16)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat1

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 16
! band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng16) , absa(585,ng16)
      real(kind=rb) :: kb(5,13:59,ng16), absb(235,ng16)
      real(kind=rb) :: selfref(10,ng16), forref(3,ng16)
      real(kind=rb) :: sfluxref(ng16)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg16

      module rrsw_kg17

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng17

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 17
! band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no17 = 16

      real(kind=rb) :: kao(9,5,13,no17)
      real(kind=rb) :: kbo(5,5,13:59,no17)
      real(kind=rb) :: selfrefo(10,no17), forrefo(4,no17)
      real(kind=rb) :: sfluxrefo(no17,5)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 17
! band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng17) , absa(585,ng17)
      real(kind=rb) :: kb(5,5,13:59,ng17), absb(1175,ng17)
      real(kind=rb) :: selfref(10,ng17), forref(4,ng17)
      real(kind=rb) :: sfluxref(ng17,5)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,1,13,1),absb(1,1))

      end module rrsw_kg17

      module rrsw_kg18

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng18

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 18
! band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no18 = 16

      real(kind=rb) :: kao(9,5,13,no18)
      real(kind=rb) :: kbo(5,13:59,no18)
      real(kind=rb) :: selfrefo(10,no18), forrefo(3,no18)
      real(kind=rb) :: sfluxrefo(no18,9)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 18
! band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng18), absa(585,ng18)
      real(kind=rb) :: kb(5,13:59,ng18), absb(235,ng18)
      real(kind=rb) :: selfref(10,ng18), forref(3,ng18)
      real(kind=rb) :: sfluxref(ng18,9)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg18

      module rrsw_kg19

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng19

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 19
! band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no19 = 16

      real(kind=rb) :: kao(9,5,13,no19)
      real(kind=rb) :: kbo(5,13:59,no19)
      real(kind=rb) :: selfrefo(10,no19), forrefo(3,no19)
      real(kind=rb) :: sfluxrefo(no19,9)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 19
! band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng19), absa(585,ng19)
      real(kind=rb) :: kb(5,13:59,ng19), absb(235,ng19)
      real(kind=rb) :: selfref(10,ng19), forref(3,ng19)
      real(kind=rb) :: sfluxref(ng19,9)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg19

      module rrsw_kg20

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng20

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 20
! band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
! absch4o : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no20 = 16

      real(kind=rb) :: kao(5,13,no20)
      real(kind=rb) :: kbo(5,13:59,no20)
      real(kind=rb) :: selfrefo(10,no20), forrefo(4,no20)
      real(kind=rb) :: sfluxrefo(no20)
      real(kind=rb) :: absch4o(no20)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl 

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 20
! band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(5,13,ng20), absa(65,ng20)
      real(kind=rb) :: kb(5,13:59,ng20), absb(235,ng20)
      real(kind=rb) :: selfref(10,ng20), forref(4,ng20)
      real(kind=rb) :: sfluxref(ng20)
      real(kind=rb) :: absch4(ng20)

      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg20

      module rrsw_kg21

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng21

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 21
! band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no21 = 16

      real(kind=rb) :: kao(9,5,13,no21)
      real(kind=rb) :: kbo(5,5,13:59,no21)
      real(kind=rb) :: selfrefo(10,no21), forrefo(4,no21)
      real(kind=rb) :: sfluxrefo(no21,9)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 21
! band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng21), absa(585,ng21)
      real(kind=rb) :: kb(5,5,13:59,ng21), absb(1175,ng21)
      real(kind=rb) :: selfref(10,ng21), forref(4,ng21)
      real(kind=rb) :: sfluxref(ng21,9)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,1,13,1),absb(1,1))

      end module rrsw_kg21

      module rrsw_kg22

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng22

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 22
! band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no22 = 16

      real(kind=rb) :: kao(9,5,13,no22)
      real(kind=rb) :: kbo(5,13:59,no22)
      real(kind=rb) :: selfrefo(10,no22), forrefo(3,no22)
      real(kind=rb) :: sfluxrefo(no22,9)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 22
! band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng22), absa(585,ng22)
      real(kind=rb) :: kb(5,13:59,ng22), absb(235,ng22)
      real(kind=rb) :: selfref(10,ng22), forref(3,ng22)
      real(kind=rb) :: sfluxref(ng22,9)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg22

      module rrsw_kg23

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng23

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 23
! band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no23 = 16

      real(kind=rb) :: kao(5,13,no23)
      real(kind=rb) :: selfrefo(10,no23), forrefo(3,no23)
      real(kind=rb) :: sfluxrefo(no23)
      real(kind=rb) :: raylo(no23)

      integer(kind=im) :: layreffr
      real(kind=rb) :: givfac

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 23
! band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(5,13,ng23), absa(65,ng23)
      real(kind=rb) :: selfref(10,ng23), forref(3,ng23)
      real(kind=rb) :: sfluxref(ng23), rayl(ng23)

      equivalence (ka(1,1,1),absa(1,1))

      end module rrsw_kg23

      module rrsw_kg24

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng24

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 24
! band 24: 12850-16000 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no24 = 16

      real(kind=rb) :: kao(9,5,13,no24)
      real(kind=rb) :: kbo(5,13:59,no24)
      real(kind=rb) :: selfrefo(10,no24), forrefo(3,no24)
      real(kind=rb) :: sfluxrefo(no24,9)
      real(kind=rb) :: abso3ao(no24), abso3bo(no24)
      real(kind=rb) :: raylao(no24,9), raylbo(no24)

      integer(kind=im) :: layreffr
      real(kind=rb) :: strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 24
! band 24: 12850-16000 cm-1 (low - h2o,o2; high - o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng24), absa(585,ng24)
      real(kind=rb) :: kb(5,13:59,ng24), absb(235,ng24)
      real(kind=rb) :: selfref(10,ng24), forref(3,ng24)
      real(kind=rb) :: sfluxref(ng24,9)
      real(kind=rb) :: abso3a(ng24), abso3b(ng24)
      real(kind=rb) :: rayla(ng24,9), raylb(ng24)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg24

      module rrsw_kg25

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng25

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 25
! band 25: 16000-22650 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
!sfluxrefo: real     
! abso3ao : real     
! abso3bo : real     
! raylo   : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no25 = 16

      real(kind=rb) :: kao(5,13,no25)
      real(kind=rb) :: sfluxrefo(no25)
      real(kind=rb) :: abso3ao(no25), abso3bo(no25)
      real(kind=rb) :: raylo(no25)

      integer(kind=im) :: layreffr

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 25
! band 25: 16000-22650 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! absa    : real
! sfluxref: real     
! abso3a  : real     
! abso3b  : real     
! rayl    : real     
!-----------------------------------------------------------------

      real(kind=rb) :: ka(5,13,ng25), absa(65,ng25)
      real(kind=rb) :: sfluxref(ng25)
      real(kind=rb) :: abso3a(ng25), abso3b(ng25)
      real(kind=rb) :: rayl(ng25)

      equivalence (ka(1,1,1),absa(1,1))

      end module rrsw_kg25

      module rrsw_kg26

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng26

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 26
! band 26: 22650-29000 cm-1 (low - nothing; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!sfluxrefo: real     
! raylo   : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no26 = 16

      real(kind=rb) :: sfluxrefo(no26)
      real(kind=rb) :: raylo(no26)

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 26
! band 26: 22650-29000 cm-1 (low - nothing; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! sfluxref: real     
! rayl    : real     
!-----------------------------------------------------------------

      real(kind=rb) :: sfluxref(ng26)
      real(kind=rb) :: rayl(ng26)

      end module rrsw_kg26

      module rrsw_kg27

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng27

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 27
! band 27: 29000-38000 cm-1 (low - o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
!sfluxrefo: real     
! raylo   : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no27 = 16

      real(kind=rb) :: kao(5,13,no27)
      real(kind=rb) :: kbo(5,13:59,no27)
      real(kind=rb) :: sfluxrefo(no27)
      real(kind=rb) :: raylo(no27)

      integer(kind=im) :: layreffr
      real(kind=rb) :: scalekur

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 27
! band 27: 29000-38000 cm-1 (low - o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! sfluxref: real     
! rayl    : real     
!-----------------------------------------------------------------

      real(kind=rb) :: ka(5,13,ng27), absa(65,ng27)
      real(kind=rb) :: kb(5,13:59,ng27), absb(235,ng27)
      real(kind=rb) :: sfluxref(ng27)
      real(kind=rb) :: rayl(ng27)

      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg27

      module rrsw_kg28

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng28

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 28
! band 28: 38000-50000 cm-1 (low - o3, o2; high - o3, o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! kao     : real     
! kbo     : real     
!sfluxrefo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no28 = 16

      real(kind=rb) :: kao(9,5,13,no28)
      real(kind=rb) :: kbo(5,5,13:59,no28)
      real(kind=rb) :: sfluxrefo(no28,5)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl, strrat

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 28
! band 28: 38000-50000 cm-1 (low - o3, o2; high - o3, o2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! ka      : real     
! kb      : real     
! sfluxref: real     
!-----------------------------------------------------------------

      real(kind=rb) :: ka(9,5,13,ng28), absa(585,ng28)
      real(kind=rb) :: kb(5,5,13:59,ng28), absb(1175,ng28)
      real(kind=rb) :: sfluxref(ng28,5)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,1,13,1),absb(1,1))

      end module rrsw_kg28

      module rrsw_kg29

      use parkind ,only : im => kind_im, rb => kind_rb
      use parrrsw, only : ng29

!     implicit none
      save

!-----------------------------------------------------------------
! rrtmg_sw ORIGINAL abs. coefficients for interval 29
! band 29:  820-2600 cm-1 (low - h2o; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no29 = 16

      real(kind=rb) :: kao(5,13,no29)
      real(kind=rb) :: kbo(5,13:59,no29)
      real(kind=rb) :: selfrefo(10,no29), forrefo(4,no29)
      real(kind=rb) :: sfluxrefo(no29)
      real(kind=rb) :: absh2oo(no29), absco2o(no29)

      integer(kind=im) :: layreffr
      real(kind=rb) :: rayl

!-----------------------------------------------------------------
! rrtmg_sw COMBINED abs. coefficients for interval 29
! band 29:  820-2600 cm-1 (low - h2o; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, oct1999
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
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
!-----------------------------------------------------------------

      real(kind=rb) :: ka(5,13,ng29), absa(65,ng29)
      real(kind=rb) :: kb(5,13:59,ng29), absb(235,ng29)
      real(kind=rb) :: selfref(10,ng29), forref(4,ng29)
      real(kind=rb) :: sfluxref(ng29)
      real(kind=rb) :: absh2o(ng29), absco2(ng29)

      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrsw_kg29

      module rrsw_ref

      use parkind, only : im => kind_im, rb => kind_rb

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw reference atmosphere 
! Based on standard mid-latitude summer profile
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

!  name     type     purpose
! -----  :  ----   : ----------------------------------------------
! pref   :  real   : Reference pressure levels
! preflog:  real   : Reference pressure levels, ln(pref)
! tref   :  real   : Reference temperature levels for MLS profile
!------------------------------------------------------------------

      real(kind=rb) , dimension(59) :: pref
      real(kind=rb) , dimension(59) :: preflog
      real(kind=rb) , dimension(59) :: tref

      end module rrsw_ref

      module rrsw_tbl

      use parkind, only : im => kind_im, rb => kind_rb

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw lookup table arrays

! Initial version: MJIacono, AER, may2007
! Revised: MJIacono, AER, aug2007
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

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
!------------------------------------------------------------------

      integer(kind=im), parameter :: ntbl = 10000

      real(kind=rb), parameter :: tblint = 10000.0_rb

      real(kind=rb), parameter :: od_lo = 0.06_rb

      real(kind=rb) :: tau_tbl
      real(kind=rb) , dimension(0:ntbl) :: exp_tbl

      real(kind=rb), parameter :: pade = 0.278_rb
      real(kind=rb) :: bpade

      end module rrsw_tbl

      module rrsw_vsn

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw version information

! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

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
!------------------------------------------------------------------

      character*18 hvrrtm,hvrini,hvrcld,hvrclc,hvrrft,hvrspv, &
                   hvrspc,hvrset,hvrtau,hvrvqd,hvratm,hvrutl,hvrext
      character*20 hnamrtm,hnamini,hnamcld,hnamclc,hnamrft,hnamspv, &
                   hnamspc,hnamset,hnamtau,hnamvqd,hnamatm,hnamutl,hnamext

      character*18 hvrkg
      character*20 hnamkg

      end module rrsw_vsn

      module rrsw_wvn

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only : nbndsw, mg, ngptsw, jpb1, jpb2

!     implicit none
      save

!------------------------------------------------------------------
! rrtmg_sw spectral information

! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jul2006
! Revised: MJIacono, AER, aug2008
!------------------------------------------------------------------

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
!------------------------------------------------------------------

      integer(kind=im) :: ng(jpb1:jpb2)
      integer(kind=im) :: nspa(jpb1:jpb2)
      integer(kind=im) :: nspb(jpb1:jpb2)

      real(kind=rb) :: wavenum1(jpb1:jpb2)
      real(kind=rb) :: wavenum2(jpb1:jpb2)
      real(kind=rb) :: delwave(jpb1:jpb2)

      integer(kind=im) :: ngc(nbndsw)
      integer(kind=im) :: ngs(nbndsw)
      integer(kind=im) :: ngn(ngptsw)
      integer(kind=im) :: ngb(ngptsw)
      integer(kind=im) :: ngm(nbndsw*mg)

      real(kind=rb) :: wt(mg)
      real(kind=rb) :: rwgt(nbndsw*mg)

      end module rrsw_wvn

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
      module mcica_subcol_gen_sw

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2006-2008, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! Purpose: Create McICA stochastic arrays for cloud physical or optical properties.   
! Two options are possible:
! 1) Input cloud physical properties: cloud fraction, ice and liquid water
!    paths, ice fraction, and particle sizes.  Output will be stochastic
!    arrays of these variables.  (inflag = 1)
! 2) Input cloud optical properties directly: cloud optical depth, single
!    scattering albedo and asymmetry parameter.  Output will be stochastic
!    arrays of these variables.  (inflag = 0)

! --------- Modules ----------

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only : nbndsw, ngptsw
      use rrsw_con, only: grav
      use rrsw_wvn, only: ngb
      use rrsw_vsn

      implicit none

! public interfaces/functions/subroutines
      public :: mcica_subcol_sw, generate_stochastic_clouds_sw

      contains

!------------------------------------------------------------------
! Public subroutines
!------------------------------------------------------------------

! mji - Add height needed for exponential-ranom cloud overlap method (icld=4)
      subroutine mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, hgt, &
                       cldfrac, ciwp, clwp, cswp, rei, rel, res, tauc, ssac, asmc, fsfc, &
                       cldfmcl, ciwpmcl, clwpmcl, cswpmcl, reicmcl, relqmcl, resnmcl, &
                       taucmcl, ssacmcl, asmcmcl, fsfcmcl)

! ----- Input -----
! Control
      integer(kind=im), intent(in) :: iplon           ! column/longitude dimension
      integer(kind=im), intent(in) :: ncol            ! number of columns
      integer(kind=im), intent(in) :: nlay            ! number of model layers
      integer(kind=im), intent(in) :: icld            ! clear/cloud, cloud overlap flag
      integer(kind=im), intent(in) :: permuteseed     ! if the cloud generator is called multiple times,
                                                      ! permute the seed between each call;
                                                      ! between calls for LW and SW, recommended
                                                      ! permuteseed differs by 'ngpt'
      integer(kind=im), intent(inout) :: irng         ! flag for random number generator
                                                      !  0 = kissvec
                                                      !  1 = Mersenne Twister
        
! Atmosphere
      real(kind=rb), intent(in) :: play(:,:)          ! layer pressures (mb) 
                                                      !    Dimensions: (ncol,nlay)
! mji - Add height
      real(kind=rb), intent(in) :: hgt(:,:)           ! layer height (m)
                                                      !    Dimensions: (ncol,nlay)
! Atmosphere/clouds - cldprop
      real(kind=rb), intent(in) :: cldfrac(:,:)       ! layer cloud fraction
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tauc(:,:,:)        ! in-cloud optical depth
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: ssac(:,:,:)        ! in-cloud single scattering albedo (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: asmc(:,:,:)        ! in-cloud asymmetry parameter (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: fsfc(:,:,:)        ! in-cloud forward scattering fraction (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: ciwp(:,:)          ! in-cloud ice water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: clwp(:,:)          ! in-cloud liquid water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cswp(:,:)          ! in-cloud snow water path
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: rei(:,:)           ! cloud ice particle size
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: rel(:,:)           ! cloud liquid particle size
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: res(:,:)           ! cloud snow particle size
                                                      !    Dimensions: (ncol,nlay)

! ----- Output -----
! Atmosphere/clouds - cldprmc [mcica]
      real(kind=rb), intent(out) :: cldfmcl(:,:,:)    ! cloud fraction [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: ciwpmcl(:,:,:)    ! in-cloud ice water path [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: clwpmcl(:,:,:)    ! in-cloud liquid water path [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: cswpmcl(:,:,:)    ! in-cloud snow water path [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: relqmcl(:,:)      ! liquid particle size (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: reicmcl(:,:)      ! ice partcle size (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: resnmcl(:,:)      ! snow partcle size (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: taucmcl(:,:,:)    ! in-cloud optical depth [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: ssacmcl(:,:,:)    ! in-cloud single scattering albedo [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: asmcmcl(:,:,:)    ! in-cloud asymmetry parameter [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: fsfcmcl(:,:,:)    ! in-cloud forward scattering fraction [mcica]
                                                      !    Dimensions: (ngptsw,ncol,nlay)

! ----- Local -----

! Stochastic cloud generator variables [mcica]
      integer(kind=im), parameter :: nsubcsw = ngptsw ! number of sub-columns (g-point intervals)
      integer(kind=im) :: ilev                        ! loop index

      real(kind=rb) :: pmid(ncol,nlay)                ! layer pressures (Pa) 
!      real(kind=rb) :: pdel(ncol,nlay)               ! layer pressure thickness (Pa) 
!      real(kind=rb) :: qi(ncol,nlay)                 ! ice water (specific humidity)
!      real(kind=rb) :: ql(ncol,nlay)                 ! liq water (specific humidity)


! Return if clear sky
      if (icld.eq.0) return

! NOTE: For GCM mode, permuteseed must be offset between LW and SW by at least number of subcolumns


! Pass particle sizes to new arrays, no subcolumns for these properties yet
! Convert pressures from mb to Pa

      reicmcl(:ncol,:nlay) = rei(:ncol,:nlay)
      relqmcl(:ncol,:nlay) = rel(:ncol,:nlay)
      resnmcl(:ncol,:nlay) = res(:ncol,:nlay)
      pmid(:ncol,:nlay) = play(:ncol,:nlay)*1.e2_rb

! Convert input ice and liquid cloud water paths to specific humidity ice and liquid components 

!      cwp =  (q * pdel * 1000.) / gravit)
!           = (kg/kg * kg m-1 s-2 *1000.) / m s-2
!           = (g m-2)
!
!      q  = (cwp * gravit) / (pdel *1000.)
!         = (g m-2 * m s-2) / (kg m-1 s-2 * 1000.)
!         =  kg/kg

!      do ilev = 1, nlay
!         qi(ilev) = (ciwp(ilev) * grav) / (pdel(ilev) * 1000._rb)
!         ql(ilev) = (clwp(ilev) * grav) / (pdel(ilev) * 1000._rb)
!      enddo

!  Generate the stochastic subcolumns of cloud optical properties for the shortwave;
      call generate_stochastic_clouds_sw (ncol, nlay, nsubcsw, icld, irng, pmid, hgt, cldfrac, clwp, ciwp, cswp, &
                               tauc, ssac, asmc, fsfc, cldfmcl, clwpmcl, ciwpmcl, cswpmcl, &
                               taucmcl, ssacmcl, asmcmcl, fsfcmcl, permuteseed)

      end subroutine mcica_subcol_sw


!-------------------------------------------------------------------------------------------------
      subroutine generate_stochastic_clouds_sw(ncol, nlay, nsubcol, icld, irng, pmid, hgt, cld, clwp, ciwp, cswp, &
                               tauc, ssac, asmc, fsfc, cld_stoch, clwp_stoch, ciwp_stoch, cswp_stoch,        &
                               tauc_stoch, ssac_stoch, asmc_stoch, fsfc_stoch, changeSeed) 
!-------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------
  ! ---------------------
  ! Contact: Cecile Hannay (hannay@ucar.edu)
  ! 
  ! Original code: Based on Raisanen et al., QJRMS, 2004.
  !
  ! Modifications: Generalized for use with RRTMG and added Mersenne Twister as the default
  !   random number generator, which can be changed to the optional kissvec random number generator
  !   with flag 'irng'. Some extra functionality has been commented or removed.  
  !   Michael J. Iacono, AER, Inc., February 2007
  !
  ! Given a profile of cloud fraction, cloud water and cloud ice, we produce a set of subcolumns.
  ! Each layer within each subcolumn is homogeneous, with cloud fraction equal to zero or one 
  ! and uniform cloud liquid and cloud ice concentration.
  ! The ensemble as a whole reproduces the probability function of cloud liquid and ice within each layer 
  ! and obeys an overlap assumption in the vertical.   
  ! 
  ! Overlap assumption:
  !  The cloud are consistent with 4 overlap assumptions: random, maximum, maximum-random and exponential. 
  !  The default option is maximum-random (option 3)
  !  The options are: 1=random overlap, 2=max/random, 3=maximum overlap, 4=exponential overlap
  !  This is set with the variable "overlap" 
  !mji - Exponential overlap option (overlap=4) has been deactivated in this version
  !  The exponential overlap uses also a length scale, Zo. (real,    parameter  :: Zo = 2500. ) 
  ! 
  ! Seed:
  !  If the stochastic cloud generator is called several times during the same timestep, 
  !  one should change the seed between the call to insure that the subcolumns are different.
  !  This is done by changing the argument 'changeSeed'
  !  For example, if one wants to create a set of columns for the shortwave and another set for the longwave ,
  !  use 'changeSeed = 1' for the first call and'changeSeed = 2' for the second call 
  !
  ! PDF assumption:
  !  We can use arbitrary complicated PDFS. 
  !  In the present version, we produce homogeneuous clouds (the simplest case).  
  !  Future developments include using the PDF scheme of Ben Johnson. 
  !
  ! History file:
  !  Option to add diagnostics variables in the history file. (using FINCL in the namelist)
  !  nsubcol = number of subcolumns
  !  overlap = overlap type (1-3)
  !  Zo = length scale 
  !  CLOUD_S = mean of the subcolumn cloud fraction ('_S" means Stochastic)
  !  CLDLIQ_S = mean of the subcolumn cloud water
  !  CLDICE_S = mean of the subcolumn cloud ice 
  !
  ! Note:
  !   Here: we force that the cloud condensate to be consistent with the cloud fraction 
  !   i.e we only have cloud condensate when the cell is cloudy. 
  !   In CAM: The cloud condensate and the cloud fraction are obtained from 2 different equations 
  !   and the 2 quantities can be inconsistent (i.e. CAM can produce cloud fraction 
  !   without cloud condensate or the opposite).
  !---------------------------------------------------------------------------------------------------------------

      use mcica_random_numbers
! The Mersenne Twister random number engine
      use MersenneTwister, only: randomNumberSequence, &   
                                 new_RandomNumberSequence, getRandomReal

      type(randomNumberSequence) :: randomNumbers

! -- Arguments

      integer(kind=im), intent(in) :: ncol            ! number of layers
      integer(kind=im), intent(in) :: nlay            ! number of layers
      integer(kind=im), intent(in) :: icld            ! clear/cloud, cloud overlap flag
      integer(kind=im), intent(inout) :: irng         ! flag for random number generator
                                                      !  0 = kissvec
                                                      !  1 = Mersenne Twister
      integer(kind=im), intent(in) :: nsubcol         ! number of sub-columns (g-point intervals)
      integer(kind=im), optional, intent(in) :: changeSeed     ! allows permuting seed

! Column state (cloud fraction, cloud water, cloud ice) + variables needed to read physics state 
      real(kind=rb), intent(in) :: pmid(:,:)          ! layer pressure (Pa)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: hgt(:,:)           ! layer height (m)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cld(:,:)           ! cloud fraction 
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: clwp(:,:)          ! in-cloud liquid water path (g/m2)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ciwp(:,:)          ! in-cloud ice water path (g/m2)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: cswp(:,:)          ! in-cloud snow water path (g/m2)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tauc(:,:,:)        ! in-cloud optical depth (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: ssac(:,:,:)        ! in-cloud single scattering albedo (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: asmc(:,:,:)        ! in-cloud asymmetry parameter (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)
      real(kind=rb), intent(in) :: fsfc(:,:,:)        ! in-cloud forward scattering fraction (non-delta scaled)
                                                      !    Dimensions: (nbndsw,ncol,nlay)

      real(kind=rb), intent(out) :: cld_stoch(:,:,:)  ! subcolumn cloud fraction 
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: clwp_stoch(:,:,:) ! subcolumn in-cloud liquid water path
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: ciwp_stoch(:,:,:) ! subcolumn in-cloud ice water path
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: cswp_stoch(:,:,:) ! subcolumn in-cloud snow water path
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: tauc_stoch(:,:,:) ! subcolumn in-cloud optical depth
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: ssac_stoch(:,:,:) ! subcolumn in-cloud single scattering albedo
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: asmc_stoch(:,:,:) ! subcolumn in-cloud asymmetry parameter
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(out) :: fsfc_stoch(:,:,:) ! subcolumn in-cloud forward scattering fraction
                                                      !    Dimensions: (ngptsw,ncol,nlay)

! -- Local variables
      real(kind=rb) :: cldf(ncol,nlay)                ! cloud fraction 
                                                      !    Dimensions: (ncol,nlay)

! Mean over the subcolumns (cloud fraction, cloud water , cloud ice) - inactive
!      real(kind=rb) :: mean_cld_stoch(ncol,nlay)     ! cloud fraction 
!      real(kind=rb) :: mean_clwp_stoch(ncol,nlay)    ! cloud water
!      real(kind=rb) :: mean_ciwp_stoch(ncol,nlay)    ! cloud ice
!      real(kind=rb) :: mean_tauc_stoch(ncol,nlay)    ! cloud optical depth
!      real(kind=rb) :: mean_ssac_stoch(ncol,nlay)    ! cloud single scattering albedo
!      real(kind=rb) :: mean_asmc_stoch(ncol,nlay)    ! cloud asymmetry parameter
!      real(kind=rb) :: mean_fsfc_stoch(ncol,nlay)    ! cloud forward scattering fraction

! Set overlap
      integer(kind=im) :: overlap                     ! 1 = random overlap, 2 = maximum-random,
                                                      ! 3 = maximum overlap, 4 = exponential,
                                                      ! 5 = exponential-random
      real(kind=rb), parameter  :: Zo = 2500._rb      ! length scale (m) 
      real(kind=rb), dimension(ncol,nlay) :: alpha    ! overlap parameter

! Constants (min value for cloud fraction and cloud water and ice)
      real(kind=rb), parameter :: cldmin = 1.0e-20_rb ! min cloud fraction
!      real(kind=rb), parameter :: qmin   = 1.0e-10_rb   ! min cloud water and cloud ice (not used)

! Variables related to random number and seed 
      real(kind=rb), dimension(nsubcol, ncol, nlay) :: CDF, CDF2       ! random numbers
      integer(kind=im), dimension(ncol) :: seed1, seed2, seed3, seed4  ! seed to create random number
      real(kind=rb), dimension(ncol) :: rand_num       ! random number (kissvec)
      integer(kind=im) :: iseed                        ! seed to create random number (Mersenne Twister)
      real(kind=rb) :: rand_num_mt                     ! random number (Mersenne Twister)

! Flag to identify cloud fraction in subcolumns
      logical,  dimension(nsubcol, ncol, nlay) :: isCloudy   ! flag that says whether a gridbox is cloudy

! Indices
      integer(kind=im) :: ilev, isubcol, i, n, ngbm    ! indices

!------------------------------------------------------------------------------------------ 

! Check that irng is in bounds; if not, set to default
      if (irng .ne. 0) irng = 1

! Pass input cloud overlap setting to local variable
      overlap = icld

! Ensure that cloud fractions are in bounds 
      do ilev = 1, nlay
         do i = 1, ncol
            cldf(i,ilev) = cld(i,ilev)
            if (cldf(i,ilev) < cldmin) then
               cldf(i,ilev) = 0._rb
            endif
         enddo
      enddo

! ----- Create seed  --------
   
! Advance randum number generator by changeseed values
      if (irng.eq.0) then   
! For kissvec, create a seed that depends on the state of the columns. Maybe not the best way, but it works.  
! Must use pmid from bottom four layers. 
         do i=1,ncol
            if (pmid(i,1).lt.pmid(i,2)) then
               stop 'MCICA_SUBCOL: KISSVEC SEED GENERATOR REQUIRES PMID FROM BOTTOM FOUR LAYERS.'
            endif
            seed1(i) = (pmid(i,1) - int(pmid(i,1)))  * 1000000000_im
            seed2(i) = (pmid(i,2) - int(pmid(i,2)))  * 1000000000_im
            seed3(i) = (pmid(i,3) - int(pmid(i,3)))  * 1000000000_im
            seed4(i) = (pmid(i,4) - int(pmid(i,4)))  * 1000000000_im
          enddo
         do i=1,changeSeed
            call kissvec(seed1, seed2, seed3, seed4, rand_num)
         enddo
      elseif (irng.eq.1) then
         randomNumbers = new_RandomNumberSequence(seed = changeSeed)
      endif 


! ------ Apply overlap assumption --------

! generate the random numbers  

      select case (overlap)

      case(1) 
! Random overlap
! i) pick a random value at every level
  
         if (irng.eq.0) then 
            do isubcol = 1,nsubcol
               do ilev = 1,nlay
                  call kissvec(seed1, seed2, seed3, seed4, rand_num)
                  CDF(isubcol,:,ilev) = rand_num
               enddo
            enddo
         elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  do ilev = 1, nlay
                     rand_num_mt = getRandomReal(randomNumbers)
                     CDF(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
             enddo
         endif

      case(2) 
! Maximum-Random overlap
! i) pick  a random number for top layer.
! ii) walk down the column: 
!    - if the layer above is cloudy, we use the same random number than in the layer above
!    - if the layer above is clear, we use a new random number 

         if (irng.eq.0) then 
            do isubcol = 1,nsubcol
               do ilev = 1,nlay
                  call kissvec(seed1, seed2, seed3, seed4, rand_num)
                  CDF(isubcol,:,ilev) = rand_num
               enddo
            enddo
         elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  do ilev = 1, nlay
                     rand_num_mt = getRandomReal(randomNumbers)
                     CDF(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
             enddo
         endif

         do ilev = 2,nlay
            do i = 1, ncol
               do isubcol = 1, nsubcol
                  if (CDF(isubcol, i, ilev-1) > 1._rb - cldf(i,ilev-1) ) then
                     CDF(isubcol,i,ilev) = CDF(isubcol,i,ilev-1) 
                  else
                     CDF(isubcol,i,ilev) = CDF(isubcol,i,ilev) * (1._rb - cldf(i,ilev-1)) 
                  endif
               enddo
            enddo
         enddo

      case(3) 
! Maximum overlap
! i) pick same random numebr at every level  

         if (irng.eq.0) then 
            do isubcol = 1,nsubcol
               call kissvec(seed1, seed2, seed3, seed4, rand_num)
               do ilev = 1,nlay
                  CDF(isubcol,:,ilev) = rand_num
               enddo
            enddo
         elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  rand_num_mt = getRandomReal(randomNumbers)
                  do ilev = 1, nlay
                     CDF(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
             enddo
         endif

! mji - Activate exponential cloud overlap option
         case(4)
            ! Exponential overlap: weighting between maximum and random overlap increases with the distance.
            ! The random numbers for exponential overlap verify:
            ! j=1   RAN(j)=RND1
            ! j>1   if RND1 < alpha(j,j-1) => RAN(j) = RAN(j-1)
            !                                 RAN(j) = RND2
            ! alpha is obtained from the equation
            ! alpha = exp(-(Z(j)-Z(j-1))/Zo) where Zo is a characteristic length scale

            ! compute alpha
            do i = 1, ncol
               alpha(i, 1) = 0._rb
               do ilev = 2,nlay
                  alpha(i, ilev) = exp( -( hgt (i, ilev) -  hgt (i, ilev-1)) / Zo)
               enddo
            enddo

            ! generate 2 streams of random numbers
            if (irng.eq.0) then
               do isubcol = 1,nsubcol
                  do ilev = 1,nlay
                     call kissvec(seed1, seed2, seed3, seed4, rand_num)
                     CDF(isubcol, :, ilev) = rand_num
                     call kissvec(seed1, seed2, seed3, seed4, rand_num)
                     CDF2(isubcol, :, ilev) = rand_num
                  enddo
               enddo
            elseif (irng.eq.1) then
            do isubcol = 1, nsubcol
               do i = 1, ncol
                  do ilev = 1, nlay
                     rand_num_mt = getRandomReal(randomNumbers)
                     CDF(isubcol,i,ilev) = rand_num_mt
                     rand_num_mt = getRandomReal(randomNumbers)
                     CDF2(isubcol,i,ilev) = rand_num_mt
                  enddo
               enddo
            enddo
         endif

         ! generate random numbers
         do ilev = 2,nlay
            where (CDF2(:, :, ilev) < spread(alpha (:,ilev), dim=1, nCopies=nsubcol) )
               CDF(:,:,ilev) = CDF(:,:,ilev-1)
            end where
         end do

! mji - Exponential-random cloud overlap option
         case(5)
            ! Exponential-random overlap:
            call wrf_error_fatal("Cloud Overlap case 5: ER has not yet been implemented. Stopping...") 

      end select

 
! -- generate subcolumns for homogeneous clouds -----
      do ilev = 1, nlay
         isCloudy(:,:,ilev) = (CDF(:,:,ilev) >= 1._rb - spread(cldf(:,ilev), dim=1, nCopies=nsubcol) )
      enddo

! where the subcolumn is cloudy, the subcolumn cloud fraction is 1;
! where the subcolumn is not cloudy, the subcolumn cloud fraction is 0;
! where there is a cloud, define the subcolumn cloud properties,
! otherwise set these to zero

      ngbm = ngb(1) - 1
      do ilev = 1,nlay
         do i = 1, ncol
            do isubcol = 1, nsubcol
               if ( iscloudy(isubcol,i,ilev) ) then
                  cld_stoch(isubcol,i,ilev) = 1._rb
                  clwp_stoch(isubcol,i,ilev) = clwp(i,ilev)
                  ciwp_stoch(isubcol,i,ilev) = ciwp(i,ilev)
                  cswp_stoch(isubcol,i,ilev) = cswp(i,ilev)
                  n = ngb(isubcol) - ngbm
                  tauc_stoch(isubcol,i,ilev) = tauc(n,i,ilev)
                  ssac_stoch(isubcol,i,ilev) = ssac(n,i,ilev)
                  asmc_stoch(isubcol,i,ilev) = asmc(n,i,ilev)
                  fsfc_stoch(isubcol,i,ilev) = fsfc(n,i,ilev)
               else
                  cld_stoch(isubcol,i,ilev) = 0._rb
                  clwp_stoch(isubcol,i,ilev) = 0._rb
                  ciwp_stoch(isubcol,i,ilev) = 0._rb
                  cswp_stoch(isubcol,i,ilev) = 0._rb
                  tauc_stoch(isubcol,i,ilev) = 0._rb
                  ssac_stoch(isubcol,i,ilev) = 1._rb
                  asmc_stoch(isubcol,i,ilev) = 0._rb
                  fsfc_stoch(isubcol,i,ilev) = 0._rb
               endif
            enddo
         enddo
      enddo

! -- compute the means of the subcolumns ---
!      mean_cld_stoch(:,:) = 0._rb
!      mean_clwp_stoch(:,:) = 0._rb
!      mean_ciwp_stoch(:,:) = 0._rb
!      mean_tauc_stoch(:,:) = 0._rb
!      mean_ssac_stoch(:,:) = 0._rb
!      mean_asmc_stoch(:,:) = 0._rb
!      mean_fsfc_stoch(:,:) = 0._rb
!      do i = 1, nsubcol
!         mean_cld_stoch(:,:) =  cld_stoch(i,:,:) + mean_cld_stoch(:,:) 
!         mean_clwp_stoch(:,:) =  clwp_stoch( i,:,:) + mean_clwp_stoch(:,:) 
!         mean_ciwp_stoch(:,:) =  ciwp_stoch( i,:,:) + mean_ciwp_stoch(:,:) 
!         mean_tauc_stoch(:,:) =  tauc_stoch( i,:,:) + mean_tauc_stoch(:,:) 
!         mean_ssac_stoch(:,:) =  ssac_stoch( i,:,:) + mean_ssac_stoch(:,:) 
!         mean_asmc_stoch(:,:) =  asmc_stoch( i,:,:) + mean_asmc_stoch(:,:) 
!         mean_fsfc_stoch(:,:) =  fsfc_stoch( i,:,:) + mean_fsfc_stoch(:,:) 
!      end do
!      mean_cld_stoch(:,:) = mean_cld_stoch(:,:) / nsubcol
!      mean_clwp_stoch(:,:) = mean_clwp_stoch(:,:) / nsubcol
!      mean_ciwp_stoch(:,:) = mean_ciwp_stoch(:,:) / nsubcol
!      mean_tauc_stoch(:,:) = mean_tauc_stoch(:,:) / nsubcol
!      mean_ssac_stoch(:,:) = mean_ssac_stoch(:,:) / nsubcol
!      mean_asmc_stoch(:,:) = mean_asmc_stoch(:,:) / nsubcol
!      mean_fsfc_stoch(:,:) = mean_fsfc_stoch(:,:) / nsubcol

      end subroutine generate_stochastic_clouds_sw


!-------------------------------------------------------------------------------------------------- 
      subroutine kissvec(seed1,seed2,seed3,seed4,ran_arr)
!-------------------------------------------------------------------------------------------------- 

! public domain code
! made available from http://www.fortran.com/
! downloaded by pjr on 03/16/04 for NCAR CAM
! converted to vector form, functions inlined by pjr,mvr on 05/10/2004

! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
!  Overall period>2^123; 
!
      real(kind=rb), dimension(:), intent(inout)  :: ran_arr
      integer(kind=im), dimension(:), intent(inout) :: seed1,seed2,seed3,seed4
      integer(kind=im) :: i,sz,kiss
      integer(kind=im) :: m, k, n

! inline function 
      m(k, n) = ieor (k, ishft (k, n) )

      sz = size(ran_arr)
      do i = 1, sz
         seed1(i) = 69069_im * seed1(i) + 1327217885_im
         seed2(i) = m (m (m (seed2(i), 13_im), - 17_im), 5_im)
         seed3(i) = 18000_im * iand (seed3(i), 65535_im) + ishft (seed3(i), - 16_im)
         seed4(i) = 30903_im * iand (seed4(i), 65535_im) + ishft (seed4(i), - 16_im)
         kiss = seed1(i) + seed2(i) + ishft (seed3(i), 16_im) + seed4(i)
         ran_arr(i) = kiss*2.328306e-10_rb + 0.5_rb
      end do
    
      end subroutine kissvec

      end module mcica_subcol_gen_sw

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $

      module rrtmg_sw_cldprmc

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

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only : ngptsw, jpband, jpb1, jpb2
      use rrsw_cld, only : extliq1, ssaliq1, asyliq1, &
                           extice2, ssaice2, asyice2, &
                           extice3, ssaice3, asyice3, fdlice3, &
                           abari, bbari, cbari, dbari, ebari, fbari
      use rrsw_wvn, only : wavenum1, wavenum2, ngb
      use rrsw_vsn, only : hvrclc, hnamclc

      implicit none

      contains

! ----------------------------------------------------------------------------
      subroutine cldprmc_sw(nlayers, inflag, iceflag, liqflag, cldfmc, &
                            ciwpmc, clwpmc, cswpmc, reicmc, relqmc, resnmc, &
                            taormc, taucmc, ssacmc, asmcmc, fsfcmc)
! ----------------------------------------------------------------------------

! Purpose: Compute the cloud optical properties for each cloudy layer
! and g-point interval for use by the McICA method.  
! Note: Only inflag = 0 and inflag=2/liqflag=1/iceflag=2,3 are available;
! (Hu & Stamnes, Key, and Fu) are implemented. 

! ------- Input -------

      integer(kind=im), intent(in) :: nlayers         ! total number of layers
      integer(kind=im), intent(in) :: inflag          ! see definitions
      integer(kind=im), intent(in) :: iceflag         ! see definitions
      integer(kind=im), intent(in) :: liqflag         ! see definitions

      real(kind=rb), intent(in) :: cldfmc(:,:)        ! cloud fraction [mcica]
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(in) :: ciwpmc(:,:)        ! cloud ice water path [mcica]
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(in) :: clwpmc(:,:)        ! cloud liquid water path [mcica]
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(in) :: cswpmc(:,:)        ! cloud snow water path [mcica]
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(in) :: resnmc(:)          ! cloud snow particle effective radius (microns)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: relqmc(:)          ! cloud liquid particle effective radius (microns)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: reicmc(:)          ! cloud ice particle effective radius (microns)
                                                      !    Dimensions: (nlayers)
                                                      ! specific definition of reicmc depends on setting of iceflag:
                                                      ! iceflag = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !              r_ec range is limited to 13.0 to 130.0 microns
                                                      ! iceflag = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
                                                      !              r_k range is limited to 5.0 to 131.0 microns
                                                      ! iceflag = 3: generalized effective size, dge, (Fu, 1996),
                                                      !              dge range is limited to 5.0 to 140.0 microns
                                                      !              [dge = 1.0315 * r_ec]
      real(kind=rb), intent(in) :: fsfcmc(:,:)        ! cloud forward scattering fraction
                                                      !    Dimensions: (ngptsw,nlayers)

! ------- Output -------

      real(kind=rb), intent(inout) :: taucmc(:,:)     ! cloud optical depth (delta scaled)
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(inout) :: ssacmc(:,:)     ! single scattering albedo (delta scaled)
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(inout) :: asmcmc(:,:)     ! asymmetry parameter (delta scaled)
                                                      !    Dimensions: (ngptsw,nlayers)
      real(kind=rb), intent(out) :: taormc(:,:)       ! cloud optical depth (non-delta scaled)
                                                      !    Dimensions: (ngptsw,nlayers)

! ------- Local -------

!      integer(kind=im) :: ncbands
      integer(kind=im) :: ib, lay, istr, index, icx, ig

      real(kind=rb), parameter :: eps = 1.e-06_rb     ! epsilon
      real(kind=rb), parameter :: cldmin = 1.e-20_rb  ! minimum value for cloud quantities
      real(kind=rb) :: cwp                            ! total cloud water path
      real(kind=rb) :: radliq                         ! cloud liquid droplet radius (microns)
      real(kind=rb) :: radice                         ! cloud ice effective size (microns)
      real(kind=rb) :: radsno                         ! cloud snow effective size (microns)
      real(kind=rb) :: factor
      real(kind=rb) :: fint

      real(kind=rb) :: taucldorig_a, taucloud_a, ssacloud_a, ffp, ffp1, ffpssa
      real(kind=rb) :: tauiceorig, scatice, ssaice, tauice, tauliqorig, scatliq, ssaliq, tauliq
      real(kind=rb) :: tausnoorig, scatsno, ssasno, tausno

      real(kind=rb) :: fdelta(ngptsw)
      real(kind=rb) :: extcoice(ngptsw), gice(ngptsw)
      real(kind=rb) :: ssacoice(ngptsw), forwice(ngptsw)
      real(kind=rb) :: extcoliq(ngptsw), gliq(ngptsw)
      real(kind=rb) :: ssacoliq(ngptsw), forwliq(ngptsw)
      real(kind=rb) :: extcosno(ngptsw), gsno(ngptsw)
      real(kind=rb) :: ssacosno(ngptsw), forwsno(ngptsw)

      CHARACTER*80 errmess

! Initialize

!jm not thread safe      hvrclc = '$Revision: 1.3 $'

! Some of these initializations are done elsewhere
      do lay = 1, nlayers
         do ig = 1, ngptsw
            taormc(ig,lay) = taucmc(ig,lay)
!            taucmc(ig,lay) = 0.0_rb
!            ssacmc(ig,lay) = 1.0_rb
!            asmcmc(ig,lay) = 0.0_rb
         enddo
      enddo

! Main layer loop
      do lay = 1, nlayers

! Main g-point interval loop
         do ig = 1, ngptsw 
            cwp = ciwpmc(ig,lay) + clwpmc(ig,lay) + cswpmc(ig,lay)

            if (cldfmc(ig,lay) .ge. cldmin .and. &
               (cwp .ge. cldmin .or. taucmc(ig,lay) .ge. cldmin)) then

! (inflag=0): Cloud optical properties input directly
               if (inflag .eq. 0) then
! Cloud optical properties already defined in taucmc, ssacmc, asmcmc are unscaled;
! Apply delta-M scaling here (using Henyey-Greenstein approximation)
                  taucldorig_a = taucmc(ig,lay)
                  ffp = fsfcmc(ig,lay)
                  ffp1 = 1.0_rb - ffp
                  ffpssa = 1.0_rb - ffp * ssacmc(ig,lay)
                  ssacloud_a = ffp1 * ssacmc(ig,lay) / ffpssa
                  taucloud_a = ffpssa * taucldorig_a

                  taormc(ig,lay) = taucldorig_a
                  ssacmc(ig,lay) = ssacloud_a
                  taucmc(ig,lay) = taucloud_a
                  asmcmc(ig,lay) = (asmcmc(ig,lay) - ffp) / (ffp1)

               elseif (inflag .eq. 1) then 
                  stop 'INFLAG = 1 OPTION NOT AVAILABLE WITH MCICA'

! (inflag=2): Separate treatement of ice clouds and water clouds.
               elseif (inflag .ge. 2) then
                  radice = reicmc(lay)

! Calculation of absorption coefficients due to ice clouds.
                  if ((ciwpmc(ig,lay)+cswpmc(ig,lay)) .eq. 0.0_rb) then
                     extcoice(ig) = 0.0_rb
                     ssacoice(ig) = 0.0_rb
                     gice(ig)     = 0.0_rb
                     forwice(ig)  = 0.0_rb

                     extcosno(ig) = 0.0_rb
                     ssacosno(ig) = 0.0_rb
                     gsno(ig)     = 0.0_rb
                     forwsno(ig)  = 0.0_rb

! (iceflag = 1): 
! Note: This option uses Ebert and Curry approach for all particle sizes similar to
! CAM3 implementation, though this is somewhat unjustified for large ice particles
                  elseif (iceflag .eq. 1) then
                     ib = ngb(ig)
                     if (wavenum2(ib) .gt. 1.43e04_rb) then
                        icx = 1
                     elseif (wavenum2(ib) .gt. 7.7e03_rb) then
                        icx = 2
                     elseif (wavenum2(ib) .gt. 5.3e03_rb) then
                        icx = 3
                     elseif (wavenum2(ib) .gt. 4.0e03_rb) then
                        icx = 4
                     elseif (wavenum2(ib) .ge. 2.5e03_rb) then
                        icx = 5
                     endif
                     extcoice(ig) = (abari(icx) + bbari(icx)/radice)
                     ssacoice(ig) = 1._rb - cbari(icx) - dbari(icx) * radice
                     gice(ig) = ebari(icx) + fbari(icx) * radice
! Check to ensure upper limit of gice is within physical limits for large particles
                     if (gice(ig).ge.1._rb) gice(ig) = 1._rb - eps
                     forwice(ig) = gice(ig)*gice(ig)
! Check to ensure all calculated quantities are within physical limits.
                     if (extcoice(ig) .lt. 0.0_rb) stop 'ICE EXTINCTION LESS THAN 0.0'
                     if (ssacoice(ig) .gt. 1.0_rb) stop 'ICE SSA GRTR THAN 1.0'
                     if (ssacoice(ig) .lt. 0.0_rb) stop 'ICE SSA LESS THAN 0.0'
                     if (gice(ig) .gt. 1.0_rb) stop 'ICE ASYM GRTR THAN 1.0'
                     if (gice(ig) .lt. 0.0_rb) stop 'ICE ASYM LESS THAN 0.0'

! For iceflag=2 option, ice particle effective radius is limited to 5.0 to 131.0 microns

                  elseif (iceflag .eq. 2) then
                     if (radice .lt. 5.0_rb .or. radice .gt. 131.0_rb) stop 'ICE RADIUS OUT OF BOUNDS'
                     factor = (radice - 2._rb)/3._rb
                     index = int(factor)
                     if (index .eq. 43) index = 42
                     fint = factor - float(index)
                     ib = ngb(ig)
                     extcoice(ig) = extice2(index,ib) + fint * &
                                   (extice2(index+1,ib) -  extice2(index,ib))
                     ssacoice(ig) = ssaice2(index,ib) + fint * &
                                   (ssaice2(index+1,ib) -  ssaice2(index,ib))
                     gice(ig) = asyice2(index,ib) + fint * &
                                   (asyice2(index+1,ib) -  asyice2(index,ib))
                     forwice(ig) = gice(ig)*gice(ig)
! Check to ensure all calculated quantities are within physical limits.
                     if (extcoice(ig) .lt. 0.0_rb) stop 'ICE EXTINCTION LESS THAN 0.0'
                     if (ssacoice(ig) .gt. 1.0_rb) stop 'ICE SSA GRTR THAN 1.0'
                     if (ssacoice(ig) .lt. 0.0_rb) stop 'ICE SSA LESS THAN 0.0'
                     if (gice(ig) .gt. 1.0_rb) stop 'ICE ASYM GRTR THAN 1.0'
                     if (gice(ig) .lt. 0.0_rb) stop 'ICE ASYM LESS THAN 0.0'

! For iceflag=3 option, ice particle generalized effective size is limited to 5.0 to 140.0 microns

                  elseif (iceflag .ge. 3) then
                     if (radice .lt. 5.0_rb .or. radice .gt. 140.0_rb) then
                         write(errmess,'(A,i5,i5,f8.2,f8.2)' )         &
               'ERROR: ICE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'   &
               ,ig, lay, ciwpmc(ig,lay), radice
                         call wrf_error_fatal(errmess)
                     end if
                     factor = (radice - 2._rb)/3._rb
                     index = int(factor)
                     if (index .eq. 46) index = 45
                     fint = factor - float(index)
                     ib = ngb(ig)
                     extcoice(ig) = extice3(index,ib) + fint * &
                                   (extice3(index+1,ib) - extice3(index,ib))
                     ssacoice(ig) = ssaice3(index,ib) + fint * &
                                   (ssaice3(index+1,ib) - ssaice3(index,ib))
                     gice(ig) = asyice3(index,ib) + fint * &
                               (asyice3(index+1,ib) - asyice3(index,ib))
                     fdelta(ig) = fdlice3(index,ib) + fint * &
                                 (fdlice3(index+1,ib) - fdlice3(index,ib))
                     if (fdelta(ig) .lt. 0.0_rb) then
                      write(errmess, *) 'FDELTA LESS THAN 0.0'
                      call wrf_error_fatal(errmess)
                     end if
                     if (fdelta(ig) .gt. 1.0_rb) then
                      write(errmess, *) 'FDELTA GT THAN 1.0'
                      call wrf_error_fatal(errmess)
                     end if
                     forwice(ig) = fdelta(ig) + 0.5_rb / ssacoice(ig)
! See Fu 1996 p. 2067 
                     if (forwice(ig) .gt. gice(ig)) forwice(ig) = gice(ig)
! Check to ensure all calculated quantities are within physical limits.  
                     if (extcoice(ig) .lt. 0.0_rb) stop 'ICE EXTINCTION LESS THAN 0.0'
                     if (ssacoice(ig) .gt. 1.0_rb) stop 'ICE SSA GRTR THAN 1.0'
                     if (ssacoice(ig) .lt. 0.0_rb) stop 'ICE SSA LESS THAN 0.0'
                     if (gice(ig) .gt. 1.0_rb) stop 'ICE ASYM GRTR THAN 1.0'
                     if (gice(ig) .lt. 0.0_rb) stop 'ICE ASYM LESS THAN 0.0'

                  endif

!!!!!!!!!!!!!!!!!! Mukul !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!  INSERT THE EQUIVALENT SNOW VARIABLE CODE HERE
!!!! Although far from perfect, the snow will utilize the
!!!! same lookup table constants as cloud ice.  Changes
!!!! to those constants for larger particle snow would be
!!!! an improvement.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                  if (cswpmc(ig,lay).gt.0.0_rb .and. iceflag .eq. 5) then
                     radsno = resnmc(lay)
                     if (radsno .lt. 5.0_rb .or. radsno .gt. 140.0_rb) then
                         write(errmess,'(A,i5,i5,f8.2,f8.2)' )         &
               'ERROR: SNOW GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'   &
               ,ig, lay, cswpmc(ig,lay), radsno
                         call wrf_error_fatal(errmess)
                     end if
                     factor = (radsno - 2._rb)/3._rb
                     index = int(factor)
                     if (index .eq. 46) index = 45
                     fint = factor - float(index)
                     ib = ngb(ig)
                     extcosno(ig) = extice3(index,ib) + fint * &
                                   (extice3(index+1,ib) - extice3(index,ib))
                     ssacosno(ig) = ssaice3(index,ib) + fint * &
                                   (ssaice3(index+1,ib) - ssaice3(index,ib))
                     gsno(ig) = asyice3(index,ib) + fint * &
                               (asyice3(index+1,ib) - asyice3(index,ib))
                     fdelta(ig) = fdlice3(index,ib) + fint * &
                                 (fdlice3(index+1,ib) - fdlice3(index,ib))
                     if (fdelta(ig) .lt. 0.0_rb) then
                      write(errmess, *) 'FDELTA LESS THAN 0.0'
                      call wrf_error_fatal(errmess)
                     end if
                     if (fdelta(ig) .gt. 1.0_rb) then
                      write(errmess, *) 'FDELTA GT THAN 1.0'
                      call wrf_error_fatal(errmess)
                     end if
                     forwsno(ig) = fdelta(ig) + 0.5_rb / ssacosno(ig)
! See Fu 1996 p. 2067
                     if (forwsno(ig) .gt. gsno(ig)) forwsno(ig) = gsno(ig)
! Check to ensure all calculated quantities are within physical limits.  
                     if (extcosno(ig) .lt. 0.0_rb) then
                      write(errmess, *) 'SNOW EXTINCTION LESS THAN 0.0'
                      call wrf_error_fatal(errmess)
                     end if
                     if (ssacosno(ig) .gt. 1.0_rb) then
                      write(errmess, *) 'SNOW SSA GRTR THAN 1.0'
                      call wrf_error_fatal(errmess)
                     end if
                     if (ssacosno(ig) .lt. 0.0_rb)  then
                      write(errmess, *) 'SNOW SSA LESS THAN 0.0'
                      call wrf_error_fatal(errmess)
                     end if
                     if (gsno(ig) .gt. 1.0_rb)  then
                      write(errmess, *) 'SNOW ASYM GRTR THAN 1.0'
                      call wrf_error_fatal(errmess)
                     end if
                     if (gsno(ig) .lt. 0.0_rb)  then
                      write(errmess, *) 'SNOW ASYM LESS THAN 0.0'
                      call wrf_error_fatal(errmess)
                     end if
                  else
                     extcosno(ig) = 0.0_rb
                     ssacosno(ig) = 0.0_rb
                     gsno(ig)     = 0.0_rb
                     forwsno(ig)  = 0.0_rb
                  endif


! Calculation of absorption coefficients due to water clouds.
                  if (clwpmc(ig,lay) .eq. 0.0_rb) then
                     extcoliq(ig) = 0.0_rb
                     ssacoliq(ig) = 0.0_rb
                     gliq(ig) = 0.0_rb
                     forwliq(ig) = 0.0_rb

                  elseif (liqflag .eq. 1) then
                     radliq = relqmc(lay)
                     if (radliq .lt. 1.5_rb .or. radliq .gt. 60._rb) stop &
                        'liquid effective radius out of bounds'
                     index = int(radliq - 1.5_rb)
                     if (index .eq. 0) index = 1
                     if (index .eq. 58) index = 57
                     fint = radliq - 1.5_rb - float(index)
                     ib = ngb(ig)
                     extcoliq(ig) = extliq1(index,ib) + fint * &
                                   (extliq1(index+1,ib) - extliq1(index,ib))
                     ssacoliq(ig) = ssaliq1(index,ib) + fint * &
                                   (ssaliq1(index+1,ib) - ssaliq1(index,ib))
                     if (fint .lt. 0._rb .and. ssacoliq(ig) .gt. 1._rb) &
                                    ssacoliq(ig) = ssaliq1(index,ib)
                     gliq(ig) = asyliq1(index,ib) + fint * &
                               (asyliq1(index+1,ib) - asyliq1(index,ib))
                     forwliq(ig) = gliq(ig)*gliq(ig)
! Check to ensure all calculated quantities are within physical limits.
                     if (extcoliq(ig) .lt. 0.0_rb) stop 'LIQUID EXTINCTION LESS THAN 0.0'
                     if (ssacoliq(ig) .gt. 1.0_rb) stop 'LIQUID SSA GRTR THAN 1.0'
                     if (ssacoliq(ig) .lt. 0.0_rb) stop 'LIQUID SSA LESS THAN 0.0'
                     if (gliq(ig) .gt. 1.0_rb) stop 'LIQUID ASYM GRTR THAN 1.0'
                     if (gliq(ig) .lt. 0.0_rb) stop 'LIQUID ASYM LESS THAN 0.0'
                  endif
   

                  if (iceflag .lt. 5) then
                      tauliqorig = clwpmc(ig,lay) * extcoliq(ig)
                      tauiceorig = ciwpmc(ig,lay) * extcoice(ig)
                      taormc(ig,lay) = tauliqorig + tauiceorig
  
                      ssaliq = ssacoliq(ig) * (1._rb - forwliq(ig)) / &
                               (1._rb - forwliq(ig) * ssacoliq(ig))
                      tauliq = (1._rb - forwliq(ig) * ssacoliq(ig)) * tauliqorig
                      ssaice = ssacoice(ig) * (1._rb - forwice(ig)) / &
                               (1._rb - forwice(ig) * ssacoice(ig))
                      tauice = (1._rb - forwice(ig) * ssacoice(ig)) * tauiceorig
                      scatliq = ssaliq * tauliq
                      scatice = ssaice * tauice
                      scatsno = 0.0_rb 
                      taucmc(ig,lay) = tauliq + tauice
                  else
                      tauliqorig = clwpmc(ig,lay) * extcoliq(ig)
                      tauiceorig = ciwpmc(ig,lay) * extcoice(ig)
                      tausnoorig = cswpmc(ig,lay) * extcosno(ig)
                      taormc(ig,lay) = tauliqorig + tauiceorig + tausnoorig

                      ssaliq = ssacoliq(ig) * (1._rb - forwliq(ig)) / &
                               (1._rb - forwliq(ig) * ssacoliq(ig))
                      tauliq = (1._rb - forwliq(ig) * ssacoliq(ig)) * tauliqorig
                      ssaice = ssacoice(ig) * (1._rb - forwice(ig)) / &
                               (1._rb - forwice(ig) * ssacoice(ig))
                      tauice = (1._rb - forwice(ig) * ssacoice(ig)) * tauiceorig
                      ssasno = ssacosno(ig) * (1._rb - forwsno(ig)) / &
                               (1._rb - forwsno(ig) * ssacosno(ig))
                      tausno = (1._rb - forwsno(ig) * ssacosno(ig)) * tausnoorig
                      scatliq = ssaliq * tauliq
                      scatice = ssaice * tauice
                      scatsno = ssasno * tausno
                      taucmc(ig,lay) = tauliq + tauice + tausno
                  endif

! Ensure non-zero taucmc and scatice
                  if(taucmc(ig,lay).eq.0.) taucmc(ig,lay) = cldmin
                  if(scatice.eq.0.) scatice = cldmin
                  if(scatsno.eq.0.) scatsno = cldmin

                  if (iceflag .lt. 5) then
                      ssacmc(ig,lay) = (scatliq + scatice) / taucmc(ig,lay)
                  else
                      ssacmc(ig,lay) = (scatliq + scatice + scatsno) / taucmc(ig,lay)
                  endif

                  if (iceflag .eq. 3 .or. iceflag.eq.4) then
! In accordance with the 1996 Fu paper, equation A.3, 
! the moments for ice were calculated depending on whether using spheres
! or hexagonal ice crystals.
! Set asymetry parameter to first moment (istr=1)
                     istr = 1
                     asmcmc(ig,lay) = (1.0_rb/(scatliq+scatice))* &
                        (scatliq*(gliq(ig)**istr - forwliq(ig)) / &
                        (1.0_rb - forwliq(ig)) + scatice * ((gice(ig)-forwice(ig))/ &
                        (1.0_rb - forwice(ig)))**istr)
                  elseif (iceflag .eq. 5) then
                     istr = 1
                     asmcmc(ig,lay) = (1.0_rb/(scatliq+scatice+scatsno))                               &
                                    *  (scatliq*(gliq(ig)**istr - forwliq(ig))/(1.0_rb - forwliq(ig))  &
                                    + scatice * ((gice(ig)-forwice(ig))/(1.0_rb - forwice(ig)))        &
                                    + scatsno * ((gsno(ig)-forwsno(ig))/(1.0_rb - forwsno(ig)))**istr)

                  else 
! This code is the standard method for delta-m scaling. 
! Set asymetry parameter to first moment (istr=1)
                     istr = 1
                     asmcmc(ig,lay) = (scatliq *  &
                        (gliq(ig)**istr - forwliq(ig)) / &
                        (1.0_rb - forwliq(ig)) + scatice * (gice(ig)**istr - forwice(ig)) / &
                        (1.0_rb - forwice(ig)))/(scatliq + scatice)
                  endif 

               endif

            endif

! End g-point interval loop
         enddo

! End layer loop
      enddo

      end subroutine cldprmc_sw

      end module rrtmg_sw_cldprmc

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $

      module rrtmg_sw_reftra

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

      use parkind, only : im => kind_im, rb => kind_rb
      use rrsw_tbl, only : tblint, bpade, od_lo, exp_tbl
      use rrsw_vsn, only : hvrrft, hnamrft

      implicit none

      contains

! --------------------------------------------------------------------
      subroutine reftra_sw(nlayers, lrtchk, pgg, prmuz, ptau, pw, &
                           pref, prefd, ptra, ptrad)
! --------------------------------------------------------------------
  
! Purpose: computes the reflectivity and transmissivity of a clear or 
!   cloudy layer using a choice of various approximations.
!
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
!
!
! Modifications:
! --------------
! Original: J-JMorcrette, ECMWF, Feb 2003
! Revised for F90 reformatting: MJIacono, AER, Jul 2006
! Revised to add exponential lookup table: MJIacono, AER, Aug 2007
! Reformulated some code to avoid potential fpes: MJIacono, AER, Nov 2008
!
! ------------------------------------------------------------------

! ------- Declarations ------

! ------- Input -------

      integer(kind=im), intent(in) :: nlayers

      logical, intent(in) :: lrtchk(:)                         ! Logical flag for reflectivity and
                                                               ! and transmissivity calculation; 
                                                               !   Dimensions: (nlayers)

      real(kind=rb), intent(in) :: pgg(:)                      ! asymmetry parameter
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: ptau(:)                     ! optical depth
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: pw(:)                       ! single scattering albedo 
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: prmuz                       ! cosine of solar zenith angle

! ------- Output -------

      real(kind=rb), intent(inout) :: pref(:)                  ! direct beam reflectivity
                                                               !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: prefd(:)                 ! diffuse beam reflectivity
                                                               !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: ptra(:)                  ! direct beam transmissivity
                                                               !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: ptrad(:)                 ! diffuse beam transmissivity
                                                               !   Dimensions: (nlayers+1)

! ------- Local -------

      integer(kind=im) :: jk, jl, kmodts
      integer(kind=im) :: itind

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

      real(kind=rb), parameter :: eps = 1.e-08_rb

!     ------------------------------------------------------------------

! Initialize

!jm not thread safe      hvrrft = '$Revision: 1.3 $'

      zsr3=sqrt(3._rb)
      zwcrit=0.9999995_rb
      kmodts=2

      do jk=1, nlayers
         if (.not.lrtchk(jk)) then
            pref(jk) =0._rb
            ptra(jk) =1._rb
            prefd(jk)=0._rb
            ptrad(jk)=1._rb
         else
            zto1=ptau(jk)
            zw  =pw(jk)
            zg  =pgg(jk)  

! General two-stream expressions

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
            end if
            zgamma4= 1._rb - zgamma3
    
! Recompute original s.s.a. to test for conservative solution
            zwo = 0._rb
            denom = 1._rb
            if (zg .ne. 1._rb) denom = (1._rb - (1._rb - zw) * (zg / (1._rb - zg))**2)
            if (zw .gt. 0._rb .and. denom .ne. 0._rb) zwo = zw / denom

            if (zwo >= zwcrit) then
! Conservative scattering

               za  = zgamma1 * prmuz 
               za1 = za - zgamma3
               zgt = zgamma1 * zto1
        
! Homogeneous reflectance and transmittance,
! collimated beam

               ze1 = min ( zto1 / prmuz , 500._rb)
!               ze2 = exp( -ze1 )

! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
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

! isotropic incidence

               prefd(jk) = zgt / (1._rb + zgt)
               ptrad(jk) = 1._rb - prefd(jk)        

! This is applied for consistency between total (delta-scaled) and direct (unscaled) 
! calculations at very low optical depths (tau < 1.e-4) when the exponential lookup
! table returns a transmittance of 1.0.
               if (ze2 .eq. 1.0_rb) then 
                  pref(jk) = 0.0_rb
                  ptra(jk) = 1.0_rb
                  prefd(jk) = 0.0_rb
                  ptrad(jk) = 1.0_rb
               endif

            else
! Non-conservative scattering

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

! mji - reformulated code to avoid potential floating point exceptions
!               zbeta = - zr5 / zr4
               zbeta = (zgamma1 - zrk) / zrkg
!!
        
! Homogeneous reflectance and transmittance

               ze1 = min ( zrk * zto1, 500._rb)
               ze2 = min ( zto1 / prmuz , 500._rb)
!
! Original
!              zep1 = exp( ze1 )
!              zem1 = exp(-ze1 )
!              zep2 = exp( ze2 )
!              zem2 = exp(-ze2 )
!
! Revised original, to reduce exponentials
!              zep1 = exp( ze1 )
!              zem1 = 1._rb / zep1
!              zep2 = exp( ze2 )
!              zem2 = 1._rb / zep2
!
! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
               if (ze1 .le. od_lo) then 
                  zem1 = 1._rb - ze1 + 0.5_rb * ze1 * ze1
                  zep1 = 1._rb / zem1
               else
                  tblind = ze1 / (bpade + ze1)
                  itind = tblint * tblind + 0.5_rb
                  zem1 = exp_tbl(itind)
                  zep1 = 1._rb / zem1
               endif

               if (ze2 .le. od_lo) then 
                  zem2 = 1._rb - ze2 + 0.5_rb * ze2 * ze2
                  zep2 = 1._rb / zem2
               else
                  tblind = ze2 / (bpade + ze2)
                  itind = tblint * tblind + 0.5_rb
                  zem2 = exp_tbl(itind)
                  zep2 = 1._rb / zem2
               endif

! collimated beam

! mji - reformulated code to avoid potential floating point exceptions
!               zdenr = zr4*zep1 + zr5*zem1
!               pref(jk) = zw * (zr1*zep1 - zr2*zem1 - zr3*zem2) / zdenr
!               zdent = zt4*zep1 + zt5*zem1
!               ptra(jk) = zem2 - zem2 * zw * (zt1*zep1 - zt2*zem1 - zt3*zep2) / zdent

               zdenr = zr4*zep1 + zr5*zem1
               zdent = zt4*zep1 + zt5*zem1
               if (zdenr .ge. -eps .and. zdenr .le. eps) then
                  pref(jk) = eps
                  ptra(jk) = zem2
               else
                  pref(jk) = zw * (zr1*zep1 - zr2*zem1 - zr3*zem2) / zdenr
                  ptra(jk) = zem2 - zem2 * zw * (zt1*zep1 - zt2*zem1 - zt3*zep2) / zdent
               endif
!!

! diffuse beam

               zemm = zem1*zem1
               zdend = 1._rb / ( (1._rb - zbeta*zemm ) * zrkg)
               prefd(jk) =  zgamma2 * (1._rb - zemm) * zdend
               ptrad(jk) =  zrk2*zem1*zdend

            endif

         endif         

      enddo    

      end subroutine reftra_sw

      end module rrtmg_sw_reftra

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $

      module rrtmg_sw_setcoef

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

      use parkind, only : im => kind_im, rb => kind_rb
      use parrrsw, only : mxmol
      use rrsw_ref, only : pref, preflog, tref
      use rrsw_vsn, only : hvrset, hnamset

      implicit none

      contains

!----------------------------------------------------------------------------
      subroutine setcoef_sw(nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl, &
                            laytrop, layswtch, laylow, jp, jt, jt1, &
                            co2mult, colch4, colco2, colh2o, colmol, coln2o, &
                            colo2, colo3, fac00, fac01, fac10, fac11, &
                            selffac, selffrac, indself, forfac, forfrac, indfor)
!----------------------------------------------------------------------------
!
! Purpose:  For a given atmosphere, calculate the indices and
! fractions related to the pressure and temperature interpolations.

! Modifications:
! Original: J. Delamere, AER, Inc. (version 2.5, 02/04/01)
! Revised: Rewritten and adapted to ECMWF F90, JJMorcrette 030224
! Revised: For uniform rrtmg formatting, MJIacono, Jul 2006

! ------ Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: nlayers         ! total number of layers

      real(kind=rb), intent(in) :: pavel(:)           ! layer pressures (mb) 
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: tavel(:)           ! layer temperatures (K)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: pz(0:)             ! level (interface) pressures (hPa, mb)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: tz(0:)             ! level (interface) temperatures (K)
                                                      !    Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: tbound             ! surface temperature (K)
      real(kind=rb), intent(in) :: coldry(:)          ! dry air column density (mol/cm2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(in) :: wkl(:,:)           ! molecular amounts (mol/cm-2)
                                                      !    Dimensions: (mxmol,nlayers)

! ----- Output -----
      integer(kind=im), intent(out) :: laytrop        ! tropopause layer index
      integer(kind=im), intent(out) :: layswtch       ! 
      integer(kind=im), intent(out) :: laylow         ! 

      integer(kind=im), intent(out) :: jp(:)          ! 
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: jt(:)          !
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: jt1(:)         !
                                                      !    Dimensions: (nlayers)

      real(kind=rb), intent(out) :: colh2o(:)         ! column amount (h2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colco2(:)         ! column amount (co2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colo3(:)          ! column amount (o3)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: coln2o(:)         ! column amount (n2o)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colch4(:)         ! column amount (ch4)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colo2(:)          ! column amount (o2)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: colmol(:)         ! 
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: co2mult(:)        !
                                                      !    Dimensions: (nlayers)

      integer(kind=im), intent(out) :: indself(:)
                                                      !    Dimensions: (nlayers)
      integer(kind=im), intent(out) :: indfor(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: selffac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: selffrac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: forfac(:)
                                                      !    Dimensions: (nlayers)
      real(kind=rb), intent(out) :: forfrac(:)
                                                      !    Dimensions: (nlayers)

      real(kind=rb), intent(out) :: &                 !
                         fac00(:), fac01(:), &        !    Dimensions: (nlayers)
                         fac10(:), fac11(:) 

! ----- Local -----

      integer(kind=im) :: indbound
      integer(kind=im) :: indlev0
      integer(kind=im) :: lay
      integer(kind=im) :: jp1

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


! Initializations
      stpfac = 296._rb/1013._rb

      indbound = tbound - 159._rb
      tbndfrac = tbound - int(tbound)
      indlev0  = tz(0) - 159._rb
      t0frac   = tz(0) - int(tz(0))

      laytrop  = 0
      layswtch = 0
      laylow   = 0

! Begin layer loop
      do lay = 1, nlayers
! Find the two reference pressures on either side of the
! layer pressure.  Store them in JP and JP1.  Store in FP the
! fraction of the difference (in ln(pressure)) between these
! two values that the layer pressure lies.

         plog = log(pavel(lay))
         jp(lay) = int(36._rb - 5*(plog+0.04_rb))
         if (jp(lay) .lt. 1) then
            jp(lay) = 1
         elseif (jp(lay) .gt. 58) then
            jp(lay) = 58
         endif
         jp1 = jp(lay) + 1
         fp = 5._rb * (preflog(jp(lay)) - plog)

! Determine, for each reference pressure (JP and JP1), which
! reference temperature (these are different for each  
! reference pressure) is nearest the layer temperature but does
! not exceed it.  Store these indices in JT and JT1, resp.
! Store in FT (resp. FT1) the fraction of the way between JT
! (JT1) and the next highest reference temperature that the 
! layer temperature falls.

         jt(lay) = int(3._rb + (tavel(lay)-tref(jp(lay)))/15._rb)
         if (jt(lay) .lt. 1) then
            jt(lay) = 1
         elseif (jt(lay) .gt. 4) then
            jt(lay) = 4
         endif
         ft = ((tavel(lay)-tref(jp(lay)))/15._rb) - float(jt(lay)-3)
         jt1(lay) = int(3._rb + (tavel(lay)-tref(jp1))/15._rb)
         if (jt1(lay) .lt. 1) then
            jt1(lay) = 1
         elseif (jt1(lay) .gt. 4) then
            jt1(lay) = 4
         endif
         ft1 = ((tavel(lay)-tref(jp1))/15._rb) - float(jt1(lay)-3)

         water = wkl(1,lay)/coldry(lay)
         scalefac = pavel(lay) * stpfac / tavel(lay)

! If the pressure is less than ~100mb, perform a different
! set of species interpolations.

         if (plog .le. 4.56_rb) go to 5300
         laytrop =  laytrop + 1
         if (plog .ge. 6.62_rb) laylow = laylow + 1

! Set up factors needed to separately include the water vapor
! foreign-continuum in the calculation of absorption coefficient.

         forfac(lay) = scalefac / (1.+water)
         factor = (332.0_rb-tavel(lay))/36.0_rb
         indfor(lay) = min(2, max(1, int(factor)))
         forfrac(lay) = factor - float(indfor(lay))

! Set up factors needed to separately include the water vapor
! self-continuum in the calculation of absorption coefficient.

         selffac(lay) = water * forfac(lay)
         factor = (tavel(lay)-188.0_rb)/7.2_rb
         indself(lay) = min(9, max(1, int(factor)-7))
         selffrac(lay) = factor - float(indself(lay) + 7)

! Calculate needed column amounts.

         colh2o(lay) = 1.e-20_rb * wkl(1,lay)
         colco2(lay) = 1.e-20_rb * wkl(2,lay)
         colo3(lay) = 1.e-20_rb * wkl(3,lay)
!           colo3(lay) = 0._rb
!           colo3(lay) = colo3(lay)/1.16_rb
         coln2o(lay) = 1.e-20_rb * wkl(4,lay)
         colch4(lay) = 1.e-20_rb * wkl(6,lay)
         colo2(lay) = 1.e-20_rb * wkl(7,lay)
         colmol(lay) = 1.e-20_rb * coldry(lay) + colh2o(lay)
!           colco2(lay) = 0._rb
!           colo3(lay) = 0._rb
!           coln2o(lay) = 0._rb
!           colch4(lay) = 0._rb
!           colo2(lay) = 0._rb
!           colmol(lay) = 0._rb
         if (colco2(lay) .eq. 0._rb) colco2(lay) = 1.e-32_rb * coldry(lay)
         if (coln2o(lay) .eq. 0._rb) coln2o(lay) = 1.e-32_rb * coldry(lay)
         if (colch4(lay) .eq. 0._rb) colch4(lay) = 1.e-32_rb * coldry(lay)
         if (colo2(lay) .eq. 0._rb) colo2(lay) = 1.e-32_rb * coldry(lay)
! Using E = 1334.2 cm-1.
         co2reg = 3.55e-24_rb * coldry(lay)
         co2mult(lay)= (colco2(lay) - co2reg) * &
               272.63_rb*exp(-1919.4_rb/tavel(lay))/(8.7604e-4_rb*tavel(lay))
         goto 5400

! Above laytrop.
 5300    continue

! Set up factors needed to separately include the water vapor
! foreign-continuum in the calculation of absorption coefficient.

         forfac(lay) = scalefac / (1.+water)
         factor = (tavel(lay)-188.0_rb)/36.0_rb
         indfor(lay) = 3
         forfrac(lay) = factor - 1.0_rb

! Calculate needed column amounts.

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
         co2mult(lay)= (colco2(lay) - co2reg) * &
               272.63_rb*exp(-1919.4_rb/tavel(lay))/(8.7604e-4_rb*tavel(lay))

         selffac(lay) = 0._rb
         selffrac(lay)= 0._rb
         indself(lay) = 0

 5400    continue

! We have now isolated the layer ln pressure and temperature,
! between two reference pressures and two reference temperatures 
! (for each reference pressure).  We multiply the pressure 
! fraction FP with the appropriate temperature fractions to get 
! the factors that will be needed for the interpolation that yields
! the optical depths (performed in routines TAUGBn for band n).

         compfp = 1._rb - fp
         fac10(lay) = compfp * ft
         fac00(lay) = compfp * (1._rb - ft)
         fac11(lay) = fp * ft1
         fac01(lay) = fp * (1._rb - ft1)

! End layer loop
      enddo

      end subroutine setcoef_sw

!***************************************************************************
      subroutine swatmref
!***************************************************************************

      save
 
! These pressures are chosen such that the ln of the first pressure
! has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
! each subsequent ln(pressure) differs from the previous one by 0.2.

      pref(:) = (/ &
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

      preflog(:) = (/ &
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

! These are the temperatures associated with the respective 
! pressures for the MLS standard atmosphere. 

      tref(:) = (/ &
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

      end subroutine swatmref

      end module rrtmg_sw_setcoef

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $

      module rrtmg_sw_taumol

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

      use parkind, only : im => kind_im, rb => kind_rb
!      use parrrsw, only : mg, jpband, nbndsw, ngptsw
      use rrsw_con, only: oneminus
      use rrsw_wvn, only: nspa, nspb
      use rrsw_vsn, only: hvrtau, hnamtau

      implicit none

      contains

!----------------------------------------------------------------------------
      subroutine taumol_sw(nlayers, &
                           colh2o, colco2, colch4, colo2, colo3, colmol, &
                           laytrop, jp, jt, jt1, &
                           fac00, fac01, fac10, fac11, &
                           selffac, selffrac, indself, forfac, forfrac, indfor, &
                           sfluxzen, taug, taur)
!----------------------------------------------------------------------------

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
! Modifications
!
! Revised: Adapted to F90 coding, J.-J.Morcrette, ECMWF, Feb 2003
! Revised: Modified for g-point reduction, MJIacono, AER, Dec 2003
! Revised: Reformatted for consistency with rrtmg_lw, MJIacono, AER, Jul 2006
!
! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: nlayers            ! total number of layers

      integer(kind=im), intent(in) :: laytrop            ! tropopause layer index
      integer(kind=im), intent(in) :: jp(:)              ! 
                                                         !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jt(:)              !
                                                         !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jt1(:)             !
                                                         !   Dimensions: (nlayers)

      real(kind=rb), intent(in) :: colh2o(:)             ! column amount (h2o)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colco2(:)             ! column amount (co2)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colo3(:)              ! column amount (o3)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colch4(:)             ! column amount (ch4)
                                                         !   Dimensions: (nlayers)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colo2(:)              ! column amount (o2)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colmol(:)             ! 
                                                         !   Dimensions: (nlayers)

      integer(kind=im), intent(in) :: indself(:)    
                                                         !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: indfor(:)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: selffac(:)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: selffrac(:)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: forfac(:)
                                                         !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: forfrac(:)
                                                         !   Dimensions: (nlayers)

      real(kind=rb), intent(in) :: &                     !
                       fac00(:), fac01(:), &             !   Dimensions: (nlayers)
                       fac10(:), fac11(:) 

! ----- Output -----
      real(kind=rb), intent(out) :: sfluxzen(:)          ! solar source function
                                                         !   Dimensions: (ngptsw)
      real(kind=rb), intent(out) :: taug(:,:)            ! gaseous optical depth 
                                                         !   Dimensions: (nlayers,ngptsw)
      real(kind=rb), intent(out) :: taur(:,:)            ! Rayleigh 
                                                         !   Dimensions: (nlayers,ngptsw)
!      real(kind=rb), intent(out) :: ssa(:,:)            ! single scattering albedo (inactive)
                                                         !   Dimensions: (nlayers,ngptsw)

!jm not thread safe      hvrtau = '$Revision: 1.3 $'

                       rthratenswc,                               &
                       swupt, swuptc, swuptcln, swdnt, swdntc, swdntcln, &
                       swupb, swupbc, swupbcln, swdnb, swdnbc, swdnbcln, &
!                      swupflx, swupflxc, swdnflx, swdnflxc,      &
                       swcf, gsw,                                 &
                       xtime, gmt, xlat, xlong,                   &
                       radt, degrad, declin,                      &
                       coszr, julday, solcon,                     &
                       albedo, t3d, t8w, tsk,                     &
                       p3d, p8w, pi3d, rho3d,                     &
                       dz8w, cldfra3d, lradius, iradius,          & 
                       is_cammgmp_used, r, g,                     &
                       re_cloud,re_ice,re_snow,                   &
                       has_reqc,has_reqi,has_reqs,                &
                       icloud, warm_rain,                         &
                       cldovrlp,                                  & ! J. Henderson AER: cldovrlp namelist value
                       f_ice_phy, f_rain_phy,                     &
                       xland, xice, snow,                         &
                       qv3d, qc3d, qr3d,                          &
                       qi3d, qs3d, qg3d,                          &
                       o3input, o33d,                             &
                       aer_opt, aerod, no_src,                    &
                       alswvisdir, alswvisdif,                    &  !Zhenxin ssib alb comp (06/20/2011)
                       alswnirdir, alswnirdif,                    &  !Zhenxin ssib alb comp (06/20/2011)
                       swvisdir, swvisdif,                        &  !Zhenxin ssib swr comp (06/20/2011)
                       swnirdir, swnirdif,                        &  !Zhenxin ssib swi comp (06/20/2011)
                       sf_surface_physics,                        &  !Zhenxin
                       f_qv, f_qc, f_qr, f_qi, f_qs, f_qg,        &
                       tauaer300,tauaer400,tauaer600,tauaer999,   & ! czhao 
                       gaer300,gaer400,gaer600,gaer999,           & ! czhao 
                       waer300,waer400,waer600,waer999,           & ! czhao 
                       aer_ra_feedback,                           &
!jdfcz                 progn,prescribe,                           &
                       progn,calc_clean_atm_diag,                 &
                       qndrop3d,f_qndrop,                         & !czhao
                       mp_physics,                                & !wang 2014/12
                       ids,ide, jds,jde, kds,kde,                 & 
                       ims,ime, jms,jme, kms,kme,                 &
                       its,ite, jts,jte, kts,kte,                 &
                       swupflx, swupflxc,                         &
                       swdnflx, swdnflxc,                         &
                       tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw,       & ! jararias 2013/11
                       swddir, swddni, swddif,                    & ! jararias 2013/08
                       swdownc, swddnic, swddirc,                 & ! PAJ
                       xcoszen,yr,julian,                          & ! jararias 2013/08
                       obscur                                     & ! amontornes-bcodina 2015/09 solar eclipses
                       )
!------------------------------------------------------------------
   USE MODULE_RA_CLWRF_SUPPORT, ONLY : read_CAMgases
   IMPLICIT NONE
!------------------------------------------------------------------
   LOGICAL, INTENT(IN )      ::        warm_rain
   LOGICAL, INTENT(IN )      ::   is_CAMMGMP_used ! Added for CAM5 RRTMG<->CAMMGMP
!
   INTEGER, INTENT(IN )      ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN )      ::        ICLOUD
   INTEGER, INTENT(IN )      ::        MP_PHYSICS
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(IN   ) ::                                   dz8w, &
                                                             t3d, &
                                                             t8w, &
                                                             p3d, &
                                                             p8w, &
                                                            pi3d, &
                                                           rho3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(INOUT)  ::                            RTHRATENSW, &
                                                      RTHRATENSWC

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(INOUT)  ::                                   GSW, &
                                                            SWCF, &
                                                           COSZR

   INTEGER, INTENT(IN  )     ::                           JULDAY
   REAL, INTENT(IN    )      ::                      RADT,DEGRAD, &
                                         XTIME,DECLIN,SOLCON,GMT

   REAL, DIMENSION( ims:ime, jms:jme )                          , &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                           XLAND, &
                                                            XICE, &
                                                            SNOW, &
                                                             TSK, &
                                                          ALBEDO
!
!!! -------------------  Zhenxin (2011-06/20) ------------------
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL                                               , &
         INTENT(IN)     ::                            ALSWVISDIR, &     ! ssib albedo of sw and lw
                                                      ALSWVISDIF, &
                                                      ALSWNIRDIR, &
                                                      ALSWNIRDIF

   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL                                               , &
         INTENT(OUT)    ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF        ! ssib sw dir and diff rad
   INTEGER, INTENT(IN) :: sf_surface_physics                            ! ssib para

!  ----------------------- end Zhenxin --------------------------
!

! ------------------------ jararias 2013/08/10 -----------------
   real, dimension(ims:ime,jms:jme), intent(out) :: &
            swddir,  &  ! All-sky broadband surface direct horiz irradiance
            swddni,  &  ! All-sky broadband surface direct normal irradiance
            swddif,  &  ! All-sky broadband surface diffuse irradiance
            swdownc, & ! Clear sky GHI
            swddnic, & ! Clear ski DNI
            swddirc    ! Clear ski direct horizontal irradiance

   integer, intent(in)        :: yr
   real, optional, intent(in) :: &
            julian      ! julian day (1-366)
   real, dimension(ims:ime,jms:jme), intent(in) :: &
            xcoszen     ! cosine of the solar zenith angle
   real, dimension(:,:,:,:), pointer :: tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw
! ------------------------ jararias end snippet -----------------

   REAL, INTENT(IN  )   ::                                   R,G
!
! Optional
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                        CLDFRA3D, &
                                                         LRADIUS, &
                                                         IRADIUS, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        QNDROP3D

!..Added by G. Thompson to couple cloud physics effective radii.
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::       &
                                                        RE_CLOUD, &
                                                          RE_ICE, &
                                                         RE_SNOW
   INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs

   real pi,third,relconst,lwpmin,rhoh2o

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) ::                                         &
                                                       F_ICE_PHY, &
                                                      F_RAIN_PHY

   LOGICAL, OPTIONAL, INTENT(IN)   ::                             &
                                F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,F_QNDROP

! Optional
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN    ) :: tauaer300,tauaer400,tauaer600,tauaer999, & ! czhao 
                                 gaer300,gaer400,gaer600,gaer999, & ! czhao 
                                 waer300,waer400,waer600,waer999    ! czhao 

   INTEGER,    INTENT(IN  ), OPTIONAL   ::       aer_ra_feedback
!jdfcz   INTEGER,    INTENT(IN  ), OPTIONAL   ::       progn,prescribe
   INTEGER,    INTENT(IN  ), OPTIONAL   ::       progn
   INTEGER,    INTENT(IN  )             ::       calc_clean_atm_diag

!  Ozone
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         OPTIONAL                                               , &
         INTENT(IN   ) :: O33D
   INTEGER, OPTIONAL, INTENT(IN ) :: o3input
!  EC aerosol: no_src = naerec = 6
   INTEGER,           INTENT(IN ) :: no_src
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:no_src )       , &
         OPTIONAL                                               , &
         INTENT(IN   ) :: aerod
   INTEGER, OPTIONAL, INTENT(IN ) :: aer_opt

      !wavelength corresponding to wavenum1 and wavenum2 (cm-1)
      real, save :: wavemin(nbndsw) ! Min wavelength (um) of 14 intervals
      data wavemin /3.077,2.500,2.150,1.942,1.626,1.299, &
      1.242,0.778,0.625,0.442,0.345,0.263,0.200,3.846/
      real, save :: wavemax(nbndsw) ! Max wavelength (um) of interval
      data wavemax/3.846,3.077,2.500,2.150,1.942,1.626, &
      1.299,1.242,0.778,0.625,0.442,0.345,0.263,12.195/
      real wavemid(nbndsw) ! Mid wavelength (um) of interval
      real, parameter :: thresh=1.e-9
      real ang,slope
      character(len=200) :: msg

! Top of atmosphere and surface shortwave fluxes (W m-2)
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL, INTENT(INOUT) ::                               &
                    SWUPT,SWUPTC,SWUPTCLN,SWDNT,SWDNTC,SWDNTCLN,  &
                    SWUPB,SWUPBC,SWUPBCLN,SWDNB,SWDNBC,SWDNBCLN

! Layer shortwave fluxes (including extra layer above model top)
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               SWUPFLX,SWUPFLXC,                  &
                               SWDNFLX,SWDNFLXC

#ifdef CLWRFGHG
! Using data from CAMtr_volume_mixing_ratio data file
    real(kind=8)                                 :: co2, n2o, ch4, cfc11, cfc12
#else
! Set trace gas volume mixing ratios, 2005 values, IPCC (2007)
! carbon dioxide (379 ppmv) - this is being replaced by an annual function in v4.2
    real :: co2
!   data co2 / 379.e-6 /
! methane (1774 ppbv)
    real :: ch4
    data ch4 / 1774.e-9 / 
! nitrous oxide (319 ppbv)
    real :: n2o
    data n2o / 319.e-9 / 
#ifdef CLWRFGHG
! Read time-varying trace gases concentrations and interpolate them to run date
   CALL read_CAMgases(yr,julian,"RRTMG",co2,n2o,ch4,cfc11,cfc12)
#endif

! All fields are ordered vertically from bottom to top
! Pressures are in mb

! latitude loop
  j_loop: do j = jts,jte

! longitude loop
     i_loop: do i = its,ite
         rho1d(kts:kte)=rho3d(i,kts:kte,j) ! BUG FIX (SGT): this was uninitialized
!
! Do shortwave by default, deactivate below if sun below horizon
         dorrsw = .true.

! Cosine solar zenith angle for current time step
!
          ! jararias, 14/08/2013
          coszr(i,j)=xcoszen(i,j)
          coszrs=xcoszen(i,j)

! Set flag to prevent shortwave calculation when sun below horizon
         if (coszrs.le.0.0) dorrsw = .false.
! Perform shortwave calculation if sun above horizon
         if (dorrsw) then

         do k=kts,kte+1
            Pw1D(K) = p8w(I,K,J)/100.
            Tw1D(K) = t8w(I,K,J)
         enddo

         DO K=kts,kte
            QV1D(K)=0.
            QC1D(K)=0.
            QR1D(K)=0.
            QI1D(K)=0.
            QS1D(K)=0.
            CLDFRA1D(k)=0.
            QNDROP1D(k)=0.
         ENDDO

         DO K=kts,kte
            QV1D(K)=QV3D(I,K,J)
            QV1D(K)=max(0.,QV1D(K))
         ENDDO

         IF (PRESENT(O33D)) THEN
            DO K=kts,kte
               O31D(K)=O33D(I,K,J)
            ENDDO
         ELSE
            DO K=kts,kte
               O31D(K)=0.0
            ENDDO
         ENDIF

         DO K=kts,kte
            TTEN1D(K)=0.
            T1D(K)=t3d(I,K,J)
            P1D(K)=p3d(I,K,J)/100.
            DZ1D(K)=dz8w(I,K,J)
         ENDDO

! moist variables

         IF (ICLOUD .ne. 0) THEN
            IF ( PRESENT( CLDFRA3D ) ) THEN
              DO K=kts,kte
                 CLDFRA1D(k)=CLDFRA3D(I,K,J)
              ENDDO
            ENDIF

            IF (PRESENT(F_QC) .AND. PRESENT(QC3D)) THEN
              IF ( F_QC) THEN
                 DO K=kts,kte
                    QC1D(K)=QC3D(I,K,J)
                    QC1D(K)=max(0.,QC1D(K))
                 ENDDO
              ENDIF
            ENDIF

            IF (PRESENT(F_QR) .AND. PRESENT(QR3D)) THEN
              IF ( F_QR) THEN
                 DO K=kts,kte
                    QR1D(K)=QR3D(I,K,J)
                    QR1D(K)=max(0.,QR1D(K))
                 ENDDO
              ENDIF
            ENDIF

            IF ( PRESENT(F_QNDROP).AND.PRESENT(QNDROP3D)) THEN
             IF (F_QNDROP) THEN
              DO K=kts,kte
               qndrop1d(K)=qndrop3d(I,K,J)
              ENDDO
             ENDIF
            ENDIF

! This logic is tortured because cannot test F_QI unless
! it is present, and order of evaluation of expressions
! is not specified in Fortran

            IF ( PRESENT ( F_QI ) ) THEN
              predicate = F_QI
            ELSE
              predicate = .FALSE.
            ENDIF

! For MP option 3
            IF (.NOT. predicate .and. .not. warm_rain) THEN
               DO K=kts,kte
                  IF (T1D(K) .lt. 273.15) THEN
                  QI1D(K)=QC1D(K)
                  QS1D(K)=QR1D(K)
                  QC1D(K)=0.
                  QR1D(K)=0.
                  ENDIF
               ENDDO
            ENDIF

            IF (PRESENT(F_QI) .AND. PRESENT(QI3D)) THEN
               IF (F_QI) THEN
                  DO K=kts,kte
                     QI1D(K)=QI3D(I,K,J)
                     QI1D(K)=max(0.,QI1D(K))
                  ENDDO
               ENDIF
            ENDIF

            IF (PRESENT(F_QS) .AND. PRESENT(QS3D)) THEN
               IF (F_QS) THEN
                  DO K=kts,kte
                     QS1D(K)=QS3D(I,K,J)
                     QS1D(K)=max(0.,QS1D(K))
                  ENDDO
               ENDIF
            ENDIF

            IF (PRESENT(F_QG) .AND. PRESENT(QG3D)) THEN
               IF (F_QG) THEN
                  DO K=kts,kte
                     QG1D(K)=QG3D(I,K,J)
                     QG1D(K)=max(0.,QG1D(K))
                  ENDDO
               ENDIF
            ENDIF

! mji - For MP option 5
            IF ( PRESENT(F_QI) .and. PRESENT(F_QC) .and. PRESENT(F_QS) .and. PRESENT(F_ICE_PHY) ) THEN
               IF ( F_QC .and. .not. F_QI .and. F_QS ) THEN
                  DO K=kts,kte
                     qi1d(k) = 0.1*qs3d(i,k,j)
                     qs1d(k) = 0.9*qs3d(i,k,j)
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
               ENDIF
            ENDIF

         ENDIF
! For mp option=5 or 85  (new Ferrier- Aligo or called fer_hires
! scheme), QI3D saves all frozen water (ice+snow)
#if (HWRF == 1)
           IF ( mp_physics == FER_MP_HIRES .OR. &
                mp_physics == FER_MP_HIRES_ADVECT .OR. &
                mp_physics == ETAMP_HWRF ) THEN
#else
           IF ( mp_physics == FER_MP_HIRES .OR. &
                mp_physics == FER_MP_HIRES_ADVECT) THEN
#endif
                  DO K=kts,kte
                     qi1d(k) = qi3d(i,k,j)
                     qs1d(k) = 0.0
                     qc1d(k) = qc3d(i,k,j)
                     qi1d(k) = max(0.,qi1d(k))
                     qc1d(k) = max(0.,qc1d(k))
                  ENDDO
           ENDIF
!
!         EMISS0=EMISS(I,J)
!         GLW0=0. 
!         OLR0=0. 
!         TSFC=TSK(I,J)
         DO K=kts,kte
            QV1D(K)=AMAX1(QV1D(K),1.E-12) 
         ENDDO

! Set up input for shortwave
         ncol = 1
! Add extra layer from top of model to top of atmosphere
         nlay = (kte - kts + 1) + 1

! Select cloud overlap assumption (1 = random, 2 = maximum-random, 3 = maximum, 4 = exponential, 5 = exponential-random
         icld=cldovrlp ! J. Henderson AER assign namelist variable cldovrlp to existing icld

! Select cloud liquid and ice optics parameterization options
! For passing in cloud optical properties directly:
!         inflgsw = 0
!         iceflgsw = 0
!         liqflgsw = 0
! For passing in cloud physical properties; cloud optics parameterized in RRTMG:
         inflgsw = 2
         iceflgsw = 3
         liqflgsw = 1

!Mukul change the flags here with reference to the new effective cloud/ice/snow radius
         IF (ICLOUD .ne. 0) THEN
            IF ( has_reqc .ne. 0) THEN
               inflgsw = 3
               DO K=kts,kte
                  recloud1D(ncol,K) = MAX(2.5, re_cloud(I,K,J)*1.E6)
                  if (recloud1D(ncol,K).LE.2.5.AND.cldfra3d(i,k,j).gt.0. &
     &                            .AND. (XLAND(I,J)-1.5).GT.0.) then      !--- Ocean
                     recloud1D(ncol,K) = 10.5
                  elseif (recloud1D(ncol,K).LE.2.5.AND.cldfra3d(i,k,j).gt.0. &
     &                            .AND. (XLAND(I,J)-1.5).LT.0.) then      !--- Land
                     recloud1D(ncol,K) = 7.5
                  endif
               ENDDO
            ELSE
               DO K=kts,kte
#if (EM_CORE==1)
                  recloud1D(ncol,K) = 5.0
#else
                  recloud1D(ncol,K) = 10.0  ! was 5.0
#endif
               ENDDO
            ENDIF

            IF ( has_reqi .ne. 0) THEN
               inflgsw  = 4
               iceflgsw = 4
               DO K=kts,kte
                  reice1D(ncol,K) = MAX(5., re_ice(I,K,J)*1.E6)
                  if (reice1D(ncol,K).LE.5..AND.cldfra3d(i,k,j).gt.0.) then
                     idx_rei = int(t3d(i,k,j)-179.)
                     idx_rei = min(max(idx_rei,1),75)
                     corr = t3d(i,k,j) - int(t3d(i,k,j))
                     reice1D(ncol,K) = retab(idx_rei)*(1.-corr) +      &
     &                                 retab(idx_rei+1)*corr
                     reice1D(ncol,K) = MAX(reice1D(ncol,K), 5.0)
                  endif
               ENDDO
            ELSE
               DO K=kts,kte
                  reice1D(ncol,K) = 10.
               ENDDO
            ENDIF

            IF ( has_reqs .ne. 0) THEN
               inflgsw  = 5
               iceflgsw = 5
               DO K=kts,kte
                  resnow1D(ncol,K) = MAX(10., re_snow(I,K,J)*1.E6)
               ENDDO
            ELSE
               DO K=kts,kte
#if (EM_CORE==1) 
                  resnow1D(ncol,K) = 10.0
#else
                 tem2 = 25.0  !- was 10.0
                 tem3=1.e3*rho1d(k)*qi1d(k)  !- IWC (g m^-3)
                 if (tem3>thresh) then       !- Only when IWC>1.e-9 g m^-3
                   tem1=t1d(k)-273.15
                   if (tem1 < -50.0) then
                     tem2 = re_50C*tem3**0.109
                   elseif (tem1 < -40.0) then
                      tem2 = re_40C*tem3**0.08
                   elseif (tem1 < -30.0) then
                      tem2 = re_30C*tem3**0.055
                   else
                      tem2 = re_20C*tem3**0.031
                   endif
                    tem2 = max(25.,tem2)
                 endif
                 reice1D(ncol,K) = min(tem2, 135.72)   !- 1.0315*reice <= 140 microns 
#endif
               ENDDO
            ENDIF

! special case for P3 microphysics
! put ice into snow category for optics, then set ice to zero
            IF ( has_reqs .eq. 0 .and. has_reqi .ne. 0 .and. has_reqc .ne. 0) THEN
               inflgsw  = 5
               iceflgsw = 5
               DO K=kts,kte
                  resnow1D(ncol,K) = MAX(10., re_ice(I,K,J)*1.E6)
                  QS1D(K)=QI3D(I,K,J)
                  QI1D(K)=0.
                  reice1D(ncol,K)=10.
               END DO

            END IF

         ENDIF

! Set cosine of solar zenith angle
         coszen(ncol) = coszrs
! Set solar constant (original) amontornes-bcodina 2015/09
!         scon = solcon
! amontornes-bcodina 2015/09 solar eclipses
         scon = solcon*(1-obscur(i,j))
         
! For Earth/Sun distance adjustment in RRTMG
!         dyofyr = julday
!         adjes = 0.0 
! For WRF, solar constant is already provided with eccentricity adjustment,
! so do not do this in RRTMG
         dyofyr = 0
         adjes = 1.0 

! Layer indexing goes bottom to top here for all fields.
! Water vapor and ozone are converted from mmr to vmr. 
! Pressures are in units of mb here. 
         plev(ncol,1) = pw1d(1)
         tlev(ncol,1) = tw1d(1)
         tsfc(ncol) = tsk(i,j)
         do k = kts, kte
            play(ncol,k) = p1d(k)
            plev(ncol,k+1) = pw1d(k+1)
            pdel(ncol,k) = plev(ncol,k) - plev(ncol,k+1)
            tlay(ncol,k) = t1d(k)
            tlev(ncol,k+1) = tw1d(k+1)
            h2ovmr(ncol,k) = qv1d(k) * amdw
            co2vmr(ncol,k) = co2
            o2vmr(ncol,k) = o2
            ch4vmr(ncol,k) = ch4
            n2ovmr(ncol,k) = n2o
         enddo

! mji - Derive height of each layer mid-point from layer thickness.
! Needed for exponential (icld=4) and exponential-random overlap option (icld=5) only.
         dzsum = 0.0
         do k = kts, kte
            dz = dz1d(k)
            hgt(ncol,k) = dzsum + 0.5*dz
            dzsum = dzsum + dz
         enddo

!  Define profile values for extra layer from model top to top of atmosphere. 
!  The top layer temperature for all gridpoints is set to the top layer-1 
!  temperature plus a constant (0 K) that represents an isothermal layer    
!  above ptop.  Top layer interface temperatures are linearly interpolated 
!  from the layer temperatures.  

         play(ncol,kte+1) = 0.5 * plev(ncol,kte+1)
         tlay(ncol,kte+1) = tlev(ncol,kte+1) + 0.0
         plev(ncol,kte+2) = 1.0e-5
         tlev(ncol,kte+2) = tlev(ncol,kte+1) + 0.0
         tlev(ncol,kte+2) = tlev(ncol,kte+1) + 0.0
         h2ovmr(ncol,kte+1) = h2ovmr(ncol,kte) 
         co2vmr(ncol,kte+1) = co2vmr(ncol,kte) 
         o2vmr(ncol,kte+1) = o2vmr(ncol,kte) 
         ch4vmr(ncol,kte+1) = ch4vmr(ncol,kte) 
         n2ovmr(ncol,kte+1) = n2ovmr(ncol,kte) 

! mji - Fill in height array above model top to top of atmosphere using
! dz from model top layer for completeness, though this information is not
! likely to be used by the exponential-random cloud overlap method.
         hgt(ncol,kte+1) = dzsum + 0.5*dz

! Get ozone profile including amount in extra layer above model top
         call inirad (o3mmr,plev,kts,kte)

        if(present(o33d)) then
         do k = kts, kte+1
            o3vmr(ncol,k) = o3mmr(k) * amdo
            IF ( PRESENT( O33D ) ) THEN
            if(o3input .eq. 2)then
               if(k.le.kte)then
                 o3vmr(ncol,k) = o31d(k)
               else
! apply shifted climatology profile above model top
                 o3vmr(ncol,k) = o31d(kte) - o3mmr(kte)*amdo + o3mmr(k)*amdo
                 if(o3vmr(ncol,k) .le. 0.)o3vmr(ncol,k) = o3mmr(k)*amdo
               endif
            endif
            ENDIF
         enddo
        else
         do k = kts, kte+1
            o3vmr(ncol,k) = o3mmr(k) * amdo
         enddo
        endif

! Set surface albedo for direct and diffuse radiation in UV/visible and
! near-IR spectral regions
! -------------- Zhenxin 2011-06-20 ----------- !

! ------- 1.  Commented by Zhenxin 2011-06-20 for SSiB coupling modified ---- !
!         asdir(ncol) = albedo(i,j)
!         asdif(ncol) = albedo(i,j)
!         aldir(ncol) = albedo(i,j)
!         aldif(ncol) = albedo(i,j)
! -------    End of Comments    ------ !

! ------- 2. New Addiation  ------ !
    IF ( sf_surface_physics .eq. 8 .AND. XLAND(i,j) .LT. 1.5) THEN
         asdir(ncol) = ALSWVISDIR(I,J)
         asdif(ncol) = ALSWVISDIF(I,J)
         aldir(ncol) = ALSWNIRDIR(I,J)
         aldif(ncol) = ALSWNIRDIF(I,J)
    ELSE
         asdir(ncol) = albedo(i,j)
         asdif(ncol) = albedo(i,j)
         aldir(ncol) = albedo(i,j)
         aldif(ncol) = albedo(i,j)
    ENDIF

! ---------- End of Addiation ------!
! ----------  End of fds_Zhenxin 2011-06-20   --------------!

! Define cloud optical properties for radiation (inflgsw = 0)
! This option is not currently active
! Cloud and precipitation paths in g/m2 
! qi=0 if no ice phase
! qs=0 if no ice phase
         if (inflgsw .eq. 0) then

! Set cloud fraction and cloud optical properties here; not yet active
            do k = kts, kte
               cldfrac(ncol,k) = cldfra1d(k)
               do nb = 1, nbndsw
                  taucld(nb,ncol,k) = 0.0
                  ssacld(nb,ncol,k) = 1.0
                  asmcld(nb,ncol,k) = 0.0
                  fsfcld(nb,ncol,k) = 0.0
               enddo
            enddo

! Zero out cloud physical property arrays; not used when passing optical properties
! into radiation
            do k = kts, kte
               clwpth(ncol,k) = 0.0
               ciwpth(ncol,k) = 0.0
               rel(ncol,k) = 10.0
               rei(ncol,k) = 10.
            enddo
         endif

! Define cloud physical properties for radiation (inflgsw = 1 or 2)
! Cloud fraction
! Set cloud arrays if passing cloud physical properties into radiation
         if (inflgsw .gt. 0) then 
            do k = kts, kte
               cldfrac(ncol,k) = cldfra1d(k)
            enddo

! Compute cloud water/ice paths and particle sizes for input to radiation (CAM method)
            pcols = ncol
            pver = kte - kts + 1
            gravmks = g
            landfrac(ncol) = 2.-XLAND(I,J)
            landm(ncol) = landfrac(ncol)
            snowh(ncol) = 0.001*SNOW(I,J)
            icefrac(ncol) = XICE(I,J)

! From module_ra_cam: Convert liquid and ice mixing ratios to water paths;
! pdel is in mb here; convert back to Pa (*100.)
! Water paths are in units of g/m2
! snow added as ice cloud (JD 091022)
            do k = kts, kte
               gicewp = (qi1d(k)+qs1d(k)) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box ice water path.
               gliqwp = qc1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box liquid water path.
               cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))               ! In-cloud ice water path.
               cliqwp(ncol,k) = gliqwp / max(0.01,cldfrac(ncol,k))               ! In-cloud liquid water path.
            end do

! Mukul
!..The ice water path is already sum of cloud ice and snow, but when we have explicit
!.. ice effective radius, overwrite the ice path with only the cloud ice variable,
!.. leaving out the snow for its own effect.
           if(iceflgsw.ge.4)then 
              do k = kts, kte
                     gicewp = qi1d(k) * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box ice water path.
                     cicewp(ncol,k) = gicewp / max(0.01,cldfrac(ncol,k))               ! In-cloud ice water path.
              end do
           end if

!..Here the snow path is adjusted if (radiation) effective radius of snow is
!.. larger than what we currently have in the lookup tables.  Since mass goes
!.. rather close to diameter squared, adjust the mixing ratio of snow used
!.. to compute its water path in combination with the max diameter.  Not a
!.. perfect fix, but certainly better than using all snow mass when diameter is
!.. far larger than table currently contains and crystal sizes much larger than
!.. about 140 microns have lesser impact than those much smaller sizes.

           if(iceflgsw.eq.5)then
              do k = kts, kte
                 snow_mass_factor = 1.0                 
                 if (resnow1d(ncol,k) .gt. 130.)then 
                     snow_mass_factor = (130.0/resnow1d(ncol,k))*(130.0/resnow1d(ncol,k))
                     resnow1d(ncol,k)   = 130.0
                 endif
                 gsnowp = qs1d(k) * snow_mass_factor * pdel(ncol,k)*100.0 / gravmks * 1000.0     ! Grid box snow water path.
                 csnowp(ncol,k) = gsnowp / max(0.01,cldfrac(ncol,k))
              end do
           end if


!link the aerosol feedback to cloud  -czhao
  if( PRESENT( progn ) ) then
    if (progn == 1) then
!jdfcz     if(prescribe==0) then

      pi = 4.*atan(1.0)
      third=1./3.
      rhoh2o=1.e3
      relconst=3/(4.*pi*rhoh2o)
!     minimun liquid water path to calculate rel
!     corresponds to optical depth of 1.e-3 for radius 4 microns.
      lwpmin=3.e-5
      do k = kts, kte
         reliq(ncol,k) = 10.
         if( PRESENT( F_QNDROP ) ) then
            if( F_QNDROP ) then
              if ( qc1d(k)*pdel(ncol,k).gt.lwpmin.and. &
                   qndrop1d(k).gt.1000. ) then
               reliq(ncol,k)=(relconst*qc1d(k)/qndrop1d(k))**third ! effective radius in m
!           apply scaling from Martin et al., JAS 51, 1830.
               reliq(ncol,k)=1.1*reliq(ncol,k)
               reliq(ncol,k)=reliq(ncol,k)*1.e6 ! convert from m to microns
               reliq(ncol,k)=max(reliq(ncol,k),4.)
               reliq(ncol,k)=min(reliq(ncol,k),20.)
              end if
            end if
         end if
      end do
!jdfcz     else ! prescribe 
! following Kiehl
!     call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
!      write(0,*) 'sw prescribe aerosol',maxval(qndrop3d)
!jdfcz     endif
    else  ! progn   (progn=1)
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
    endif
  else   !progn   (PRESENT)
      call relcalc(ncol, pcols, pver, tlay, landfrac, landm, icefrac, reliq, snowh)
  endif

! following Kristjansson and Mitchell
      call reicalc(ncol, pcols, pver, tlay, reice)



!..If we already have effective radius of cloud and ice, then just overwrite what
!.. was computed in the relcalc and reicalc subroutines above.

      if (inflgsw .ge. 3) then
         do k = kts, kte
            reliq(ncol,k) = recloud1d(ncol,k)
         end do
      endif
#if (EM_CORE==1) 
      if (iceflgsw .ge. 4) then
#else
      if (iceflgsw .ge. 3) then   !BSF: was .ge. 4
#endif
         do k = kts, kte
            reice(ncol,k) = reice1d(ncol,k)
         end do
      endif


#if 0
      if (i==80.and.j==30) then
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
      if( PRESENT( progn ) ) write(0,*) 'aerosol indirect',progn
      write(0,*)'sw water eff radius',reliq(ncol,10),reliq(ncol,20),reliq(ncol,25)
      write(0,*)'sw ice eff radius',reice(ncol,10),reice(ncol,20),reice(ncol,25)
#endif
      endif
#endif


! Limit upper bound of reice for Fu ice parameterization and convert
! from effective radius to generalized effective size (*1.0315; Fu, 1996)
            if (iceflgsw .eq. 3) then
               do k = kts, kte
                  reice(ncol,k) = reice(ncol,k) * 1.0315
                  reice(ncol,k) = min(140.0,reice(ncol,k))
               end do
            endif
            
!if CAMMGMP is used, use output from CAMMGMP            
!PMA
            if(is_CAMMGMP_used) then
               do k = kts, kte
                  if ( qi1d(k) .gt. 1.e-20 .or. qs1d(k) .gt. 1.e-20) then
                     reice(ncol,k) = iradius(i,k,j)
                  else
                     reice(ncol,k) = 25.
                  end if
                  reice(ncol,k) = max(5., min(140.0,reice(ncol,k)))
                  if ( qc1d(k) .gt. 1.e-20) then
                     reliq(ncol,k) = lradius(i,k,j)
                  else
                     reliq(ncol,k) = 10.
                  end if
                  reliq(ncol,k) = max(2.5, min(60.0,reliq(ncol,k)))
               enddo
            endif

! Set cloud physical property arrays
            do k = kts, kte
               clwpth(ncol,k) = cliqwp(ncol,k)
               ciwpth(ncol,k) = cicewp(ncol,k)
               rel(ncol,k) = reliq(ncol,k)
               rei(ncol,k) = reice(ncol,k)
            enddo

!Mukul
            if (inflgsw .eq. 5) then
               do k = kts, kte
                  cswpth(ncol,k) = csnowp(ncol,k)
                  res(ncol,k) = resnow1d(ncol,k)
               end do
            else
               do k = kts, kte
                  cswpth(ncol,k) = 0.0
                  res(ncol,k) = 10.0
               end do
            endif

! Zero out cloud optical properties here, calculated in radiation 
            do k = kts, kte
               do nb = 1, nbndsw
                  taucld(nb,ncol,k) = 0.0
                  ssacld(nb,ncol,k) = 1.0
                  asmcld(nb,ncol,k) = 0.0
                  fsfcld(nb,ncol,k) = 0.0
               enddo
            enddo
         endif

! No clouds are allowed in the extra layer from model top to TOA
         clwpth(ncol,kte+1) = 0.
         ciwpth(ncol,kte+1) = 0.
         cswpth(ncol,kte+1) = 0.
         rel(ncol,kte+1) = 10.
         rei(ncol,kte+1) = 10.
         res(ncol,kte+1) = 10.
         cldfrac(ncol,kte+1) = 0.
         do nb = 1, nbndsw
            taucld(nb,ncol,kte+1) = 0.
            ssacld(nb,ncol,kte+1) = 1.
            asmcld(nb,ncol,kte+1) = 0.
            fsfcld(nb,ncol,kte+1) = 0.
         enddo

         iplon = 1
         irng = 0
         permuteseed = 1

! Sub-column generator for McICA
! mji - Add layer height needed for exponential (icld=4) and exponential-random (icld=5) overlap options
         call mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, hgt, &
                       cldfrac, ciwpth, clwpth, cswpth, rei, rel, res, taucld, ssacld, asmcld, fsfcld, &
                       cldfmcl, ciwpmcl, clwpmcl, cswpmcl, reicmcl, relqmcl, resnmcl, &
                       taucmcl, ssacmcl, asmcmcl, fsfcmcl)


!--------------------------------------------------------------------------
! Aerosol optical depth, single scattering albedo and asymmetry parameter -czhao 03/2010
!--------------------------------------------------------------------------
! by layer for each RRTMG shortwave band
! No aerosols in top layer above model top (kte+1).
!cz        do nb = 1, nbndsw
!cz           do k = kts, kte+1
!cz              tauaer(ncol,k,nb) = 0.
!cz              ssaaer(ncol,k,nb) = 1.
!cz              asmaer(ncol,k,nb) = 0.
!cz           enddo
!cz        enddo

! ... Aerosol effects. Added aerosol feedbacks from Chem , 03/2010 -czhao
!
      do nb = 1, nbndsw
      do k = kts,kte+1
         tauaer(ncol,k,nb) = 0.
         ssaaer(ncol,k,nb) = 1.
         asmaer(ncol,k,nb) = 0.
      end do
      end do

      if ( associated (tauaer3d_sw) ) then
! ---- jararias 11/2012
            do nb=1,nbndsw
               do k=kts,kte
                  tauaer(ncol,k,nb)=tauaer3d_sw(i,k,j,nb)
                  ssaaer(ncol,k,nb)=ssaaer3d_sw(i,k,j,nb)
                  asmaer(ncol,k,nb)=asyaer3d_sw(i,k,j,nb)
               end do
            end do
      end if

#if ( WRF_CHEM == 1 )
   IF ( AER_RA_FEEDBACK == 1) then
      do nb = 1, nbndsw
         wavemid(nb)=0.5*(wavemin(nb)+wavemax(nb))  ! um
      do k = kts,kte      !wig

! convert optical properties at 300,400,600, and 999 to conform to the band wavelengths
! tauaer - use angstrom exponent
        if(tauaer300(i,k,j).gt.thresh .and. tauaer999(i,k,j).gt.thresh) then
           ang=alog(tauaer300(i,k,j)/tauaer999(i,k,j))/alog(999./300.)
           tauaer(ncol,k,nb)=tauaer400(i,k,j)*(0.4/wavemid(nb))**ang
           !tauaer(ncol,k,nb)=tauaer600(i,k,j)*(0.6/wavemid(nb))**ang 
           if (i==30.and.j==49.and.k==2.and.nb==12) then
            write(0,*) 'TAU from 600 vs 400 in RRTMG',tauaer600(i,k,j),tauaer400(i,k,j)
            print*, 'TAU from 600 vs 400 in RRTMG',tauaer600(i,k,j),tauaer400(i,k,j)
            write(0,*) tauaer600(i,k,j)*(0.6/wavemid(nb))**ang,tauaer400(i,k,j)*(0.4/wavemid(nb))**ang
            print*, tauaer600(i,k,j)*(0.6/wavemid(nb))**ang,tauaer400(i,k,j)*(0.4/wavemid(nb))**ang
           endif
! ssa - linear interpolation; extrapolation
           slope=(waer600(i,k,j)-waer400(i,k,j))/.2
           ssaaer(ncol,k,nb) = slope*(wavemid(nb)-.6)+waer600(i,k,j)
           if(ssaaer(ncol,k,nb).lt.0.4) ssaaer(ncol,k,nb)=0.4
           if(ssaaer(ncol,k,nb).ge.1.0) ssaaer(ncol,k,nb)=1.0
! g - linear interpolation;extrapolation
           slope=(gaer600(i,k,j)-gaer400(i,k,j))/.2
           asmaer(ncol,k,nb) = slope*(wavemid(nb)-.6)+gaer600(i,k,j) ! notice reversed varaibles
           if(asmaer(ncol,k,nb).lt.0.5) asmaer(ncol,k,nb)=0.5
           if(asmaer(ncol,k,nb).ge.1.0) asmaer(ncol,k,nb)=1.0
        endif
      end do ! k
      end do ! nb

!wig beg
      do nb = 1, nbndsw
         slope = 0.  !use slope as a sum holder
         do k = kts,kte
            slope = slope + tauaer(ncol,k,nb)
         end do
         if( slope < 0. ) then
            write(msg,'("ERROR: Negative total optical depth of ",f8.2," at point i,j,nb=",3i5)') slope,i,j,nb
            call wrf_error_fatal(msg)
         else if( slope > 6. ) then
            call wrf_message("-------------------------")
            write(msg,'("WARNING: Large total sw optical depth of ",f8.2," at point i,j,nb=",3i5)') slope,i,j,nb
            call wrf_message(msg)

            call wrf_message("Diagnostics 1: k, tauaer300, tauaer400, tauaer600, tauaer999, tauaer")
            do k=kts,kte
               write(msg,'(i4,5f8.2)') k, tauaer300(i,k,j), tauaer400(i,k,j), &
                    tauaer600(i,k,j), tauaer999(i,k,j),tauaer(ncol,k,nb)
               call wrf_message(msg)
               !czhao set an up-limit here to avoid segmentation fault 
               !from extreme AOD
               tauaer(ncol,k,nb)=tauaer(ncol,k,nb)*6.0/slope 
            end do

            call wrf_message("Diagnostics 2: k, gaer300, gaer400, gaer600, gaer999")
            do k=kts,kte
               write(msg,'(i4,4f8.2)') k, gaer300(i,k,j), gaer400(i,k,j), &
                    gaer600(i,k,j), gaer999(i,k,j)
               call wrf_message(msg)
            end do

            call wrf_message("Diagnostics 3: k, waer300, waer400, waer600, waer999")
            do k=kts,kte
               write(msg,'(i4,4f8.2)') k, waer300(i,k,j), waer400(i,k,j), &
                    waer600(i,k,j), waer999(i,k,j)
               call wrf_message(msg)
            end do

            call wrf_message("Diagnostics 4: k, ssaal, asyal, taual")
            do k=kts-1,kte
               write(msg,'(i4,3f8.2)') k, ssaaer(i,k,nb), asmaer(i,k,nb), tauaer(i,k,nb)
               call wrf_message(msg)
            end do
            call wrf_message("-------------------------")
         endif
      enddo  ! nb
      endif  ! aer_ra_feedback
#endif


! Zero array for input of aerosol optical thickness for use with
! ECMWF aerosol types (not used)
         do na = 1, naerec
            do k = kts, kte+1
               ecaer(ncol,k,na) = 0.
            enddo
         enddo

      IF ( PRESENT( aerod ) ) THEN
      if ( aer_opt .eq. 0 ) then
         do na = 1, naerec
            do k = kts, kte+1
               ecaer(ncol,k,na) = 0.
            enddo
         enddo
      else if ( aer_opt .eq. 1 ) then
         do na = 1, naerec
            do k = kts, kte
               ecaer(ncol,k,na) = aerod(i,k,j,na)
            enddo
! assuming 0 or same value at the top?
!           ecaer(ncol,kte+1,na) = ecaer(ncol,kte,na)
            ecaer(ncol,kte+1,na) = 0.
         enddo
      endif
      ENDIF

! Call RRTMG shortwave radiation model

         call rrtmg_sw &
            (ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl , &
             taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl , &
             ciwpmcl ,clwpmcl ,cswpmcl, reicmcl ,relqmcl ,resnmcl, &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, swuflxcln, swdflxcln, aer_opt, &
! -----      Zhenxin added for ssib coupiling 2011-06-20 --------!
             sibvisdir, sibvisdif, sibnirdir, sibnirdif,         &
! --------------------   End of addiation by Zhenxin 2011-06-20 ------!
             swdkdir, swdkdif,                     &  ! jararias, 2012/08/10
             swdkdirc                              &  ! PAJ
             ,calc_clean_atm_diag                 &
                                                   )


! Output net absorbed shortwave surface flux and shortwave cloud forcing
! at the top of atmosphere (W/m2)
         gsw(i,j) = swdflx(1,1) - swuflx(1,1)
         swcf(i,j) = (swdflx(1,kte+2) - swuflx(1,kte+2)) - (swdflxc(1,kte+2) - swuflxc(1,kte+2))

         if (present(swupt)) then 
! Output up and down toa fluxes for total and clear sky
            swupt(i,j)     = swuflx(1,kte+2)
            swuptc(i,j)    = swuflxc(1,kte+2)
            swdnt(i,j)     = swdflx(1,kte+2)
            swdntc(i,j)    = swdflxc(1,kte+2)
! Output up and down surface fluxes for total and clear sky
            swupb(i,j)     = swuflx(1,1)
            swupbc(i,j)    = swuflxc(1,1)
            swdnb(i,j)     = swdflx(1,1)
! Added by Zhenxin for 4 compenants of swdown radiation
            swvisdir(i,j)  = sibvisdir(1,1)
            swvisdif(i,j)  = sibvisdif(1,1)
            swnirdir(i,j)  = sibnirdir(1,1)
            swnirdif(i,j)  = sibnirdif(1,1)
!  Ended, Zhenxin (2011/06/20)
            swdnbc(i,j)    = swdflxc(1,1)
            if(calc_clean_atm_diag .gt. 0)then
            	swuptcln(i,j)  = swuflxcln(1,kte+2)
            	swdntcln(i,j)  = swdflxcln(1,kte+2)
            	swupbcln(i,j)  = swuflxcln(1,1)
            	swdnbcln(i,j)  = swdflxcln(1,1)
            end if
         endif
            swddir(i,j)    = swdkdir(1,1)          ! jararias 2013/08/10
            swddni(i,j)    = swddir(i,j) / coszrs  ! jararias 2013/08/10
            swddif(i,j)    = swdkdif(1,1)          ! jararias 2013/08/10
            swdownc(i, j)  = swdflxc(1,1)          ! PAJ: clear-sky GHI
            swddirc(i,j)   = swdkdirc(1,1)         ! PAJ: clear-sky direct normal irradiance
            swddnic(i,j)   = swddirc(i,j) / coszrs ! PAJ: clear-sky direct normal irradiance

! Output up and down layer fluxes for total and clear sky.
! Vertical ordering is from bottom to top in units of W m-2. 
         if ( present (swupflx) ) then
         do k=kts,kte+2
            swupflx(i,k,j)  = swuflx(1,k)
            swupflxc(i,k,j) = swuflxc(1,k)
            swdnflx(i,k,j)  = swdflx(1,k)
            swdnflxc(i,k,j) = swdflxc(1,k)
         enddo
         endif

! Output heating rate tendency; convert heating rate from K/d to K/s
! Heating rate arrays are ordered vertically from bottom to top here. 
         do k=kts,kte 
            tten1d(k) = swhr(ncol,k)/86400.
            rthratensw(i,k,j) = tten1d(k)/pi3d(i,k,j)
            tten1d(k) = swhrc(ncol,k)/86400.
            rthratenswc(i,k,j) = tten1d(k)/pi3d(i,k,j)
         enddo
      else
         if (present(swupt)) then 
! Output up and down toa fluxes for total and clear sky
            swupt(i,j)     = 0.
            swuptc(i,j)    = 0.
            swdnt(i,j)     = 0.
            swdntc(i,j)    = 0.
! Output up and down surface fluxes for total and clear sky
            swupb(i,j)     = 0.
            swupbc(i,j)    = 0.
            swdnb(i,j)     = 0.
            swdnbc(i,j)    = 0.
            swvisdir(i,j)  = 0.  ! Add by Zhenxin (2011/06/20)
            swvisdif(i,j)  = 0.
            swnirdir(i,j)  = 0.
            swnirdif(i,j)  = 0.  ! Add by Zhenxin (2011/06/20)
            if(calc_clean_atm_diag .gt. 0)then
				swuptcln(i,j)  = 0.
				swdntcln(i,j)  = 0.
				swupbcln(i,j)  = 0.
				swdnbcln(i,j)  = 0.
            end if
         endif
            swddir(i,j)    = 0.  ! jararias 2013/08/10
            swddni(i,j)    = 0.  ! jararias 2013/08/10
            swddif(i,j)    = 0.  ! jararias 2013/08/10
            swdownc(i, j)  = 0.0 ! PAJ
            swddnic(i,j)   = 0.0 ! PAJ
            swddirc(i,j)   = 0.0 ! PAJ
            swcf(i,j)      = 0.

      endif
!
      end do i_loop
   end do j_loop                                           


!-------------------------------------------------------------------

   END SUBROUTINE RRTMG_SWRAD

 
!====================================================================
   SUBROUTINE rrtmg_swinit(                                         &
                       allowed_to_read ,                            &
                       ids, ide, jds, jde, kds, kde,                &
                       ims, ime, jms, jme, kms, kme,                &
                       its, ite, jts, jte, kts, kte                 )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------

   LOGICAL , INTENT(IN)           :: allowed_to_read
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte

! Read in absorption coefficients and other data
   IF ( allowed_to_read ) THEN
     CALL rrtmg_swlookuptable
   ENDIF

! Perform g-point reduction and other initializations
! Specific heat of dry air (cp) used in flux to heating rate conversion factor.
   call rrtmg_sw_ini(cp)

   END SUBROUTINE rrtmg_swinit


! **************************************************************************     
      SUBROUTINE rrtmg_swlookuptable
! **************************************************************************     

IMPLICIT NONE

! Local                                    
      INTEGER :: i
      LOGICAL                 :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

      CHARACTER*80 errmess
      INTEGER rrtmg_unit

      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 10,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            rrtmg_unit = i
            GOTO 2010
          ENDIF
        ENDDO
        rrtmg_unit = -1
 2010   CONTINUE
      ENDIF
      CALL wrf_dm_bcast_bytes ( rrtmg_unit , IWORDSIZE )
      IF ( rrtmg_unit < 0 ) THEN
        CALL wrf_error_fatal ( 'module_ra_rrtmg_sw: rrtm_swlookuptable: Can not '// &
                               'find unused fortran unit to read in lookup table.' )
      ENDIF

      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(rrtmg_unit,FILE='RRTMG_SW_DATA',                  &
             FORM='UNFORMATTED',STATUS='OLD',ERR=9009)
      ENDIF

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

     IF ( wrf_dm_on_monitor() ) CLOSE (rrtmg_unit)

     RETURN
9009 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error opening RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

     END SUBROUTINE rrtmg_swlookuptable

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

! **************************************************************************
      subroutine sw_kgb16(rrtmg_unit)
! **************************************************************************

      use rrsw_kg16, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            rayl, strrat1, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 2925 cm-1.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat1, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat1)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb16

! **************************************************************************
      subroutine sw_kgb17(rrtmg_unit)
! **************************************************************************

      use rrsw_kg17, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            rayl, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 3625 cm-1.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb17

! **************************************************************************
      subroutine sw_kgb18(rrtmg_unit)
! **************************************************************************

      use rrsw_kg18, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            rayl, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 4325 cm-1.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb18 

! **************************************************************************
      subroutine sw_kgb19(rrtmg_unit)
! **************************************************************************

      use rrsw_kg19, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            rayl, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 4900 cm-1.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb19

! **************************************************************************
      subroutine sw_kgb20(rrtmg_unit)
! **************************************************************************

      use rrsw_kg20, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absch4o, rayl, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 5670 cm-1.

!     Array absch4o contains the absorption coefficients for methane.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, layreffr, absch4o, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(absch4o)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb20

! **************************************************************************
      subroutine sw_kgb21(rrtmg_unit)
! **************************************************************************

      use rrsw_kg21, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            rayl, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 6925 cm-1.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb21

! **************************************************************************
      subroutine sw_kgb22(rrtmg_unit)
! **************************************************************************

      use rrsw_kg22, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            rayl, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at v = 8000 cm-1.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296_rb,260_rb,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat, layreffr, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb22

! **************************************************************************
      subroutine sw_kgb23(rrtmg_unit)
! **************************************************************************

      use rrsw_kg23, only : kao, selfrefo, forrefo, sfluxrefo, &
                            raylo, givfac, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array raylo contains the Rayleigh extinction coefficient at all v for this band

!     Array givfac is the average Giver et al. correction factor for this band. 

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         raylo, givfac, layreffr, kao, selfrefo, forrefo, sfluxrefo
      DM_BCAST_MACRO(raylo)
      DM_BCAST_REAL(givfac)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb23

! **************************************************************************
      subroutine sw_kgb24(rrtmg_unit)
! **************************************************************************

      use rrsw_kg24, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            raylao, raylbo, abso3ao, abso3bo, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Arrays raylao and raylbo contain the Rayleigh extinction coefficient at 
!     all v for this band for the upper and lower atmosphere.

!     Arrays abso3ao and abso3bo contain the ozone absorption coefficient at 
!     all v for this band for the upper and lower atmosphere.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         raylao, raylbo, strrat, layreffr, abso3ao, abso3bo, kao, kbo, selfrefo, &
         forrefo, sfluxrefo
      DM_BCAST_MACRO(raylao)
      DM_BCAST_MACRO(raylbo)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(abso3ao)
      DM_BCAST_MACRO(abso3bo)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb24

! **************************************************************************
      subroutine sw_kgb25(rrtmg_unit)
! **************************************************************************

      use rrsw_kg25, only : kao, sfluxrefo, &
                            raylo, abso3ao, abso3bo, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array raylo contains the Rayleigh extinction coefficient at all v = 2925 cm-1.

!     Arrays abso3ao and abso3bo contain the ozone absorption coefficient at 
!     all v for this band for the upper and lower atmosphere.

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

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         raylo, layreffr, abso3ao, abso3bo, kao, sfluxrefo
      DM_BCAST_MACRO(raylo)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(abso3ao)
      DM_BCAST_MACRO(abso3bo)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb25

! **************************************************************************
      subroutine sw_kgb26(rrtmg_unit)
! **************************************************************************

      use rrsw_kg26, only : sfluxrefo, raylo

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array raylo contains the Rayleigh extinction coefficient at all v for this band.

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         raylo, sfluxrefo
      DM_BCAST_MACRO(raylo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb26

! **************************************************************************
      subroutine sw_kgb27(rrtmg_unit)
! **************************************************************************

      use rrsw_kg27, only : kao, kbo, sfluxrefo, raylo, &
                            scalekur, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 
!     The values in array sfluxrefo were obtained using the "low resolution"
!     version of the Kurucz solar source function.  For unknown reasons,
!     the total irradiance in this band differs from the corresponding
!     total in the "high-resolution" version of the Kurucz function.
!     Therefore, these values are scaled by the factor SCALEKUR.

!     Array raylo contains the Rayleigh extinction coefficient at all v = 2925 cm-1.

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

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         raylo, scalekur, layreffr, kao, kbo, sfluxrefo
      DM_BCAST_MACRO(raylo)
      DM_BCAST_REAL(scalekur)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb27

! **************************************************************************
      subroutine sw_kgb28(rrtmg_unit)
! **************************************************************************

      use rrsw_kg28, only : kao, kbo, sfluxrefo, &
                            rayl, strrat, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array raylo contains the Rayleigh extinction coefficient at all v = ???? cm-1.

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

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, strrat, layreffr, kao, kbo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_REAL(strrat)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb28

! **************************************************************************
      subroutine sw_kgb29(rrtmg_unit)
! **************************************************************************

      use rrsw_kg29, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absh2oo, absco2o, rayl, layreffr

      implicit none
      save

! Input
      integer, intent(in) :: rrtmg_unit

! Local                                    
      character*80 errmess
      logical, external  :: wrf_dm_on_monitor

!     Array sfluxrefo contains the Kurucz solar source function for this band. 

!     Array rayl contains the Rayleigh extinction coefficient at all v = 2200 cm-1.

!     Array absh2oo contains the water vapor absorption coefficient for this band.

!     Array absco2o contains the carbon dioxide absorption coefficient for this band.

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

!     The array FORREFO contains the coefficient of the water vapor
!     foreign-continuum (including the energy term).  The first 
!     index refers to reference temperature (296,260,224,260) and 
!     pressure (970,475,219,3 mbar) levels.  The second index 
!     runs over the g-channel (1 to 16).

!     The array SELFREFO contains the coefficient of the water vapor
!     self-continuum (including the energy term).  The first index
!     refers to temperature in 7.2 degree increments.  For instance,
!     JT = 1 refers to a temperature of 245.6, JT = 2 refers to 252.8,
!     etc.  The second index runs over the g-channel (1 to 16).

#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * RWORDSIZE )
#define DM_BCAST_REAL(A) CALL wrf_dm_bcast_real ( A , 1 )
#define DM_BCAST_INTEGER(A) CALL wrf_dm_bcast_integer ( A , 1 )

      IF ( wrf_dm_on_monitor() ) READ (rrtmg_unit,ERR=9010) &
         rayl, layreffr, absh2oo, absco2o, kao, kbo, selfrefo, forrefo, sfluxrefo
      DM_BCAST_REAL(rayl)
      DM_BCAST_INTEGER(layreffr)
      DM_BCAST_MACRO(absh2oo)
      DM_BCAST_MACRO(absco2o)
      DM_BCAST_MACRO(kao)
      DM_BCAST_MACRO(kbo)
      DM_BCAST_MACRO(selfrefo)
      DM_BCAST_MACRO(forrefo)
      DM_BCAST_MACRO(sfluxrefo)

     RETURN
9010 CONTINUE
     WRITE( errmess , '(A,I4)' ) 'module_ra_rrtmg_sw: error reading RRTMG_SW_DATA on unit ',rrtmg_unit
     CALL wrf_error_fatal(errmess)

      end subroutine sw_kgb29

!------------------------------------------------------------------

END MODULE module_ra_rrtmg_sw
!***********************************************************************
