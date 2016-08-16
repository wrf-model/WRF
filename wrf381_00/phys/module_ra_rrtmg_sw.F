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

      subroutine mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, &
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


! Return if clear sky; or stop if icld out of range
      if (icld.eq.0) return
      if (icld.lt.0.or.icld.gt.3) then 
         stop 'MCICA_SUBCOL: INVALID ICLD'
      endif 

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
      call generate_stochastic_clouds_sw (ncol, nlay, nsubcsw, icld, irng, pmid, cldfrac, clwp, ciwp, cswp, &
                               tauc, ssac, asmc, fsfc, cldfmcl, clwpmcl, ciwpmcl, cswpmcl, &
                               taucmcl, ssacmcl, asmcmcl, fsfcmcl, permuteseed)

      end subroutine mcica_subcol_sw


!-------------------------------------------------------------------------------------------------
      subroutine generate_stochastic_clouds_sw(ncol, nlay, nsubcol, icld, irng, pmid, cld, clwp, ciwp, cswp, &
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
      integer(kind=im) :: overlap                     ! 1 = random overlap, 2 = maximum/random,
                                                      ! 3 = maximum overlap, 
!      real(kind=rb), parameter  :: Zo = 2500._rb        ! length scale (m) 
!      real(kind=rb) :: zm(ncon,nlay)                    ! Height of midpoints (above surface)
!      real(kind=rb), dimension(nlay) :: alpha=0.0_rb    ! overlap parameter  

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

!    case(4)  - inactive
!       ! Exponential overlap: weighting between maximum and random overlap increases with the distance. 
!       ! The random numbers for exponential overlap verify:
!       ! j=1   RAN(j)=RND1
!       ! j>1   if RND1 < alpha(j,j-1) => RAN(j) = RAN(j-1)
!       !                                 RAN(j) = RND2
!       ! alpha is obtained from the equation
!       ! alpha = exp(- (Zi-Zj-1)/Zo) where Zo is a characteristic length scale    


!       ! compute alpha
!       zm    = state%zm     
!       alpha(:, 1) = 0._rb
!       do ilev = 2,nlay
!          alpha(:, ilev) = exp( -( zm (:, ilev-1) -  zm (:, ilev)) / Zo)
!       end do
       
!       ! generate 2 streams of random numbers
!       do isubcol = 1,nsubcol
!          do ilev = 1,nlay
!             call kissvec(seed1, seed2, seed3, seed4, rand_num)
!             CDF(isubcol, :, ilev) = rand_num
!             call kissvec(seed1, seed2, seed3, seed4, rand_num)
!             CDF2(isubcol, :, ilev) = rand_num
!          end do
!       end do

!       ! generate random numbers
!       do ilev = 2,nlay
!          where (CDF2(:, :, ilev) < spread(alpha (:,ilev), dim=1, nCopies=nsubcol) )
!             CDF(:,:,ilev) = CDF(:,:,ilev-1) 
!          end where
!       end do

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
            !Balwinder.Singh@pnnl.gov: Code added to avoid 'divide by zero' error in zwo computation
            denom = max((1._rb - (1._rb - zw) * (zg / (1._rb - zg))**2),1.0E-30_rb)
            zwo= zw / denom

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

! Calculate gaseous optical depth and planck fractions for each spectral band.

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

!-------------
      contains
!-------------

!----------------------------------------------------------------------------
      subroutine taumol16
!----------------------------------------------------------------------------
!
!     band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng16
      use rrsw_kg16, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat1

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
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

         do ig = 1, ng16
            taug(lay,ig) = speccomb * &
                (fac000 * absa(ind0   ,ig) + &
                 fac100 * absa(ind0 +1,ig) + &
                 fac010 * absa(ind0 +9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1   ,ig) + &
                 fac101 * absa(ind1 +1,ig) + &
                 fac011 * absa(ind1 +9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colh2o(lay) * &
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!            ssa(lay,ig) = tauray/taug(lay,ig)
            taur(lay,ig) = tauray
         enddo
      enddo

      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr) &
            laysolfr = lay
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(16) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(16) + 1
         tauray = colmol(lay) * rayl

         do ig = 1, ng16
            taug(lay,ig) = colch4(lay) * &
                (fac00(lay) * absb(ind0  ,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1  ,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) 
!            ssa(lay,ig) = tauray/taug(lay,ig)
            if (lay .eq. laysolfr) sfluxzen(ig) = sfluxref(ig) 
            taur(lay,ig) = tauray  
         enddo
      enddo

      end subroutine taumol16

!----------------------------------------------------------------------------
      subroutine taumol17
!----------------------------------------------------------------------------
!
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng17, ngs16
      use rrsw_kg17, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
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

         do ig = 1, ng17
            taug(lay,ngs16+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colh2o(lay) * &
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!             ssa(lay,ngs16+ig) = tauray/taug(lay,ngs16+ig)
            taur(lay,ngs16+ig) = tauray
         enddo
      enddo

      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr) &
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

         do ig = 1, ng17
            taug(lay,ngs16+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                 fac100 * absb(ind0+1,ig) + &
                 fac010 * absb(ind0+5,ig) + &
                 fac110 * absb(ind0+6,ig) + &
                 fac001 * absb(ind1,ig) + &
                 fac101 * absb(ind1+1,ig) + &
                 fac011 * absb(ind1+5,ig) + &
                 fac111 * absb(ind1+6,ig)) + &
                 colh2o(lay) * &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))) 
!            ssa(lay,ngs16+ig) = tauray/taug(lay,ngs16+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs16+ig) = sfluxref(ig,js) &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs16+ig) = tauray
         enddo
      enddo

      end subroutine taumol17

!----------------------------------------------------------------------------
      subroutine taumol18
!----------------------------------------------------------------------------
!
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng18, ngs17
      use rrsw_kg18, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop
      
! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
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

         do ig = 1, ng18
            taug(lay,ngs17+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colh2o(lay) * &
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!            ssa(lay,ngs17+ig) = tauray/taug(lay,ngs17+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs17+ig) = sfluxref(ig,js) &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs17+ig) = tauray
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(18) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(18) + 1
         tauray = colmol(lay) * rayl

         do ig = 1, ng18
            taug(lay,ngs17+ig) = colch4(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &	  
                 fac11(lay) * absb(ind1+1,ig)) 
!           ssa(lay,ngs17+ig) = tauray/taug(lay,ngs17+ig)
           taur(lay,ngs17+ig) = tauray
         enddo
       enddo

       end subroutine taumol18

!----------------------------------------------------------------------------
      subroutine taumol19
!----------------------------------------------------------------------------
!
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng19, ngs18
      use rrsw_kg19, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop      
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
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

         do ig = 1 , ng19
            taug(lay,ngs18+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colh2o(lay) * &
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + & 
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!            ssa(lay,ngs18+ig) = tauray/taug(lay,ngs18+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs18+ig) = sfluxref(ig,js) &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs18+ig) = tauray   
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(19) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(19) + 1
         tauray = colmol(lay) * rayl

         do ig = 1 , ng19
            taug(lay,ngs18+ig) = colco2(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) 
!            ssa(lay,ngs18+ig) = tauray/taug(lay,ngs18+ig) 
            taur(lay,ngs18+ig) = tauray   
         enddo
      enddo

      end subroutine taumol19

!----------------------------------------------------------------------------
      subroutine taumol20
!----------------------------------------------------------------------------
!
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng20, ngs19
      use rrsw_kg20, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, absch4, rayl, layreffr

      implicit none

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
            laysolfr = min(lay+1,laytrop)
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(20) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(20) + 1
         inds = indself(lay)
         indf = indfor(lay)
         tauray = colmol(lay) * rayl

         do ig = 1, ng20
            taug(lay,ngs19+ig) = colh2o(lay) * &
               ((fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) + &
                 selffac(lay) * (selfref(inds,ig) + & 
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) &
                 + colch4(lay) * absch4(ig)
!            ssa(lay,ngs19+ig) = tauray/taug(lay,ngs19+ig)
            taur(lay,ngs19+ig) = tauray 
            if (lay .eq. laysolfr) sfluxzen(ngs19+ig) = sfluxref(ig) 
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(20) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(20) + 1
         indf = indfor(lay)
         tauray = colmol(lay) * rayl

         do ig = 1, ng20
            taug(lay,ngs19+ig) = colh2o(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) + &
                 colch4(lay) * absch4(ig)
!            ssa(lay,ngs19+ig) = tauray/taug(lay,ngs19+ig)
            taur(lay,ngs19+ig) = tauray 
         enddo
      enddo

      end subroutine taumol20

!----------------------------------------------------------------------------
      subroutine taumol21
!----------------------------------------------------------------------------
!
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng21, ngs20
      use rrsw_kg21, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop
      
! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
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

         do ig = 1, ng21
            taug(lay,ngs20+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colh2o(lay) * &
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))))
!            ssa(lay,ngs20+ig) = tauray/taug(lay,ngs20+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs20+ig) = sfluxref(ig,js) &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs20+ig) = tauray
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
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

         do ig = 1, ng21
            taug(lay,ngs20+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                 fac100 * absb(ind0+1,ig) + &
                 fac010 * absb(ind0+5,ig) + &
                 fac110 * absb(ind0+6,ig) + &
                 fac001 * absb(ind1,ig) + &
                 fac101 * absb(ind1+1,ig) + &
                 fac011 * absb(ind1+5,ig) + &
                 fac111 * absb(ind1+6,ig)) + &
                 colh2o(lay) * &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))
!            ssa(lay,ngs20+ig) = tauray/taug(lay,ngs20+ig)
            taur(lay,ngs20+ig) = tauray
         enddo
      enddo

      end subroutine taumol21

!----------------------------------------------------------------------------
      subroutine taumol22
!----------------------------------------------------------------------------
!
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng22, ngs21
      use rrsw_kg22, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray, o2adj, o2cont

! The following factor is the ratio of total O2 band intensity (lines 
! and Mate continuum) to O2 band intensity (line only).  It is needed
! to adjust the optical depths since the k's include only lines.
      o2adj = 1.6_rb
      
! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
            laysolfr = min(lay+1,laytrop)
         o2cont = 4.35e-4_rb*colo2(lay)/(350.0_rb*2.0_rb)
         speccomb = colh2o(lay) + o2adj*strrat*colo2(lay)
         specparm = colh2o(lay)/speccomb 
         if (specparm .ge. oneminus) specparm = oneminus
         specmult = 8._rb*(specparm)
!         odadj = specparm + o2adj * (1._rb - specparm)
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

         do ig = 1, ng22
            taug(lay,ngs21+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colh2o(lay) * &
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                  (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) &
                 + o2cont
!            ssa(lay,ngs21+ig) = tauray/taug(lay,ngs21+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs21+ig) = sfluxref(ig,js) &
                + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs21+ig) = tauray
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         o2cont = 4.35e-4_rb*colo2(lay)/(350.0_rb*2.0_rb)
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(22) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(22) + 1
         tauray = colmol(lay) * rayl

         do ig = 1, ng22
            taug(lay,ngs21+ig) = colo2(lay) * o2adj * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) + &
                 o2cont
!            ssa(lay,ngs21+ig) = tauray/taug(lay,ngs21+ig)
            taur(lay,ngs21+ig) = tauray
         enddo
      enddo

      end subroutine taumol22

!----------------------------------------------------------------------------
      subroutine taumol23
!----------------------------------------------------------------------------
!
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng23, ngs22
      use rrsw_kg23, only : absa, ka, forref, selfref, &
                            sfluxref, rayl, layreffr, givfac

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
            laysolfr = min(lay+1,laytrop)
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(23) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(23) + 1
         inds = indself(lay)
         indf = indfor(lay)

         do ig = 1, ng23
            tauray = colmol(lay) * rayl(ig)
            taug(lay,ngs22+ig) = colh2o(lay) * &
                (givfac * (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) + &
                 selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + &
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) 
!            ssa(lay,ngs22+ig) = tauray/taug(lay,ngs22+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs22+ig) = sfluxref(ig) 
            taur(lay,ngs22+ig) = tauray
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng23
!            taug(lay,ngs22+ig) = colmol(lay) * rayl(ig)
!            ssa(lay,ngs22+ig) = 1.0_rb
            taug(lay,ngs22+ig) = 0._rb
            taur(lay,ngs22+ig) = colmol(lay) * rayl(ig) 
         enddo
      enddo

      end subroutine taumol23

!----------------------------------------------------------------------------
      subroutine taumol24
!----------------------------------------------------------------------------
!
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng24, ngs23
      use rrsw_kg24, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, abso3a, abso3b, rayla, raylb, &
                            layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
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

         do ig = 1, ng24
            tauray = colmol(lay) * (rayla(ig,js) + &
               fs * (rayla(ig,js+1) - rayla(ig,js)))
            taug(lay,ngs23+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) + &
                 colo3(lay) * abso3a(ig) + &
                 colh2o(lay) * & 
                 (selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + & 
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig))))
!            ssa(lay,ngs23+ig) = tauray/taug(lay,ngs23+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs23+ig) = sfluxref(ig,js) &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs23+ig) = tauray
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(24) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(24) + 1

         do ig = 1, ng24
            tauray = colmol(lay) * raylb(ig)
            taug(lay,ngs23+ig) = colo2(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) + &
                 colo3(lay) * abso3b(ig)
!            ssa(lay,ngs23+ig) = tauray/taug(lay,ngs23+ig)
            taur(lay,ngs23+ig) = tauray
         enddo
      enddo

      end subroutine taumol24

!----------------------------------------------------------------------------
      subroutine taumol25
!----------------------------------------------------------------------------
!
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng25, ngs24
      use rrsw_kg25, only : absa, ka, &
                            sfluxref, abso3a, abso3b, rayl, layreffr

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop
      do lay = 1, laytrop
         if (jp(lay) .lt. layreffr .and. jp(lay+1) .ge. layreffr) &
            laysolfr = min(lay+1,laytrop)
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(25) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(25) + 1

         do ig = 1, ng25
            tauray = colmol(lay) * rayl(ig)
            taug(lay,ngs24+ig) = colh2o(lay) * &
                (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) + &
                 colo3(lay) * abso3a(ig) 
!            ssa(lay,ngs24+ig) = tauray/taug(lay,ngs24+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs24+ig) = sfluxref(ig) 
            taur(lay,ngs24+ig) = tauray
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng25
            tauray = colmol(lay) * rayl(ig)
            taug(lay,ngs24+ig) = colo3(lay) * abso3b(ig) 
!            ssa(lay,ngs24+ig) = tauray/taug(lay,ngs24+ig)
            taur(lay,ngs24+ig) = tauray
         enddo
      enddo

      end subroutine taumol25

!----------------------------------------------------------------------------
      subroutine taumol26
!----------------------------------------------------------------------------
!
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng26, ngs25
      use rrsw_kg26, only : sfluxref, rayl

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

      laysolfr = laytrop

! Lower atmosphere loop
      do lay = 1, laytrop
         do ig = 1, ng26 
!            taug(lay,ngs25+ig) = colmol(lay) * rayl(ig)
!            ssa(lay,ngs25+ig) = 1.0_rb
            if (lay .eq. laysolfr) sfluxzen(ngs25+ig) = sfluxref(ig) 
            taug(lay,ngs25+ig) = 0._rb
            taur(lay,ngs25+ig) = colmol(lay) * rayl(ig) 
         enddo
      enddo

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         do ig = 1, ng26
!            taug(lay,ngs25+ig) = colmol(lay) * rayl(ig)
!            ssa(lay,ngs25+ig) = 1.0_rb
            taug(lay,ngs25+ig) = 0._rb
            taur(lay,ngs25+ig) = colmol(lay) * rayl(ig) 
         enddo
      enddo

      end subroutine taumol26

!----------------------------------------------------------------------------
      subroutine taumol27
!----------------------------------------------------------------------------
!
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng27, ngs26
      use rrsw_kg27, only : absa, ka, absb, kb, &
                            sfluxref, rayl, layreffr, scalekur

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(27) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(27) + 1

         do ig = 1, ng27
            tauray = colmol(lay) * rayl(ig)
            taug(lay,ngs26+ig) = colo3(lay) * &
                (fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig))
!            ssa(lay,ngs26+ig) = tauray/taug(lay,ngs26+ig)
            taur(lay,ngs26+ig) = tauray
         enddo
      enddo

      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr) &
            laysolfr = lay
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(27) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(27) + 1

         do ig = 1, ng27
            tauray = colmol(lay) * rayl(ig)
            taug(lay,ngs26+ig) = colo3(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + & 
                 fac11(lay) * absb(ind1+1,ig))
!            ssa(lay,ngs26+ig) = tauray/taug(lay,ngs26+ig)
            if (lay.eq.laysolfr) sfluxzen(ngs26+ig) = scalekur * sfluxref(ig) 
            taur(lay,ngs26+ig) = tauray
         enddo
      enddo

      end subroutine taumol27

!----------------------------------------------------------------------------
      subroutine taumol28
!----------------------------------------------------------------------------
!
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng28, ngs27
      use rrsw_kg28, only : absa, ka, absb, kb, &
                            sfluxref, rayl, layreffr, strrat

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
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

         do ig = 1, ng28
            taug(lay,ngs27+ig) = speccomb * &
                (fac000 * absa(ind0,ig) + &
                 fac100 * absa(ind0+1,ig) + &
                 fac010 * absa(ind0+9,ig) + &
                 fac110 * absa(ind0+10,ig) + &
                 fac001 * absa(ind1,ig) + &
                 fac101 * absa(ind1+1,ig) + &
                 fac011 * absa(ind1+9,ig) + &
                 fac111 * absa(ind1+10,ig)) 
!            ssa(lay,ngs27+ig) = tauray/taug(lay,ngs27+ig)
            taur(lay,ngs27+ig) = tauray
         enddo
      enddo

      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr) &
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

         do ig = 1, ng28
            taug(lay,ngs27+ig) = speccomb * &
                (fac000 * absb(ind0,ig) + &
                 fac100 * absb(ind0+1,ig) + &
                 fac010 * absb(ind0+5,ig) + &
                 fac110 * absb(ind0+6,ig) + &
                 fac001 * absb(ind1,ig) + &
                 fac101 * absb(ind1+1,ig) + &
                 fac011 * absb(ind1+5,ig) + &
                 fac111 * absb(ind1+6,ig)) 
!            ssa(lay,ngs27+ig) = tauray/taug(lay,ngs27+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs27+ig) = sfluxref(ig,js) &
               + fs * (sfluxref(ig,js+1) - sfluxref(ig,js))
            taur(lay,ngs27+ig) = tauray
         enddo
      enddo

      end subroutine taumol28

!----------------------------------------------------------------------------
      subroutine taumol29
!----------------------------------------------------------------------------
!
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)
!
!----------------------------------------------------------------------------

! ------- Modules -------

      use parrrsw, only : ng29, ngs28
      use rrsw_kg29, only : absa, ka, absb, kb, forref, selfref, &
                            sfluxref, absh2o, absco2, rayl, layreffr

! ------- Declarations -------

! Local

      integer(kind=im) :: ig, ind0, ind1, inds, indf, js, lay, laysolfr
      real(kind=rb) :: fac000, fac001, fac010, fac011, fac100, fac101, &
                       fac110, fac111, fs, speccomb, specmult, specparm, &
                       tauray

! Compute the optical depth by interpolating in ln(pressure), 
! temperature, and appropriate species.  Below LAYTROP, the water
! vapor self-continuum is interpolated (in temperature) separately.  

! Lower atmosphere loop
      do lay = 1, laytrop
         ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(29) + 1
         ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(29) + 1
         inds = indself(lay)
         indf = indfor(lay)
         tauray = colmol(lay) * rayl

         do ig = 1, ng29
            taug(lay,ngs28+ig) = colh2o(lay) * &
               ((fac00(lay) * absa(ind0,ig) + &
                 fac10(lay) * absa(ind0+1,ig) + &
                 fac01(lay) * absa(ind1,ig) + &
                 fac11(lay) * absa(ind1+1,ig)) + &
                 selffac(lay) * (selfref(inds,ig) + &
                 selffrac(lay) * &
                 (selfref(inds+1,ig) - selfref(inds,ig))) + &
                 forfac(lay) * (forref(indf,ig) + & 
                 forfrac(lay) * &
                 (forref(indf+1,ig) - forref(indf,ig)))) &
                 + colco2(lay) * absco2(ig) 
!            ssa(lay,ngs28+ig) = tauray/taug(lay,ngs28+ig)
            taur(lay,ngs28+ig) = tauray
         enddo
      enddo

      laysolfr = nlayers

! Upper atmosphere loop
      do lay = laytrop+1, nlayers
         if (jp(lay-1) .lt. layreffr .and. jp(lay) .ge. layreffr) &
            laysolfr = lay
         ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(29) + 1
         ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(29) + 1
         tauray = colmol(lay) * rayl

         do ig = 1, ng29
            taug(lay,ngs28+ig) = colco2(lay) * &
                (fac00(lay) * absb(ind0,ig) + &
                 fac10(lay) * absb(ind0+1,ig) + &
                 fac01(lay) * absb(ind1,ig) + &
                 fac11(lay) * absb(ind1+1,ig)) &  
                 + colh2o(lay) * absh2o(ig) 
!            ssa(lay,ngs28+ig) = tauray/taug(lay,ngs28+ig)
            if (lay .eq. laysolfr) sfluxzen(ngs28+ig) = sfluxref(ig) 
            taur(lay,ngs28+ig) = tauray
         enddo
      enddo

      end subroutine taumol29

      end subroutine taumol_sw

      end module rrtmg_sw_taumol

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $

      module rrtmg_sw_init

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
      use rrsw_wvn
      use rrtmg_sw_setcoef, only: swatmref

      implicit none

      contains

! **************************************************************************
      subroutine rrtmg_sw_ini(cpdair)
! **************************************************************************
!
!  Original version:   Michael J. Iacono; February, 2004
!  Revision for F90 formatting:  M. J. Iacono, July, 2006
!
!  This subroutine performs calculations necessary for the initialization
!  of the shortwave model.  Lookup tables are computed for use in the SW
!  radiative transfer, and input absorption coefficient data for each
!  spectral band are reduced from 224 g-point intervals to 112.
! **************************************************************************

      use parrrsw, only : mg, nbndsw, ngptsw
      use rrsw_tbl, only: ntbl, tblint, pade, bpade, tau_tbl, exp_tbl
      use rrsw_vsn, only: hvrini, hnamini

      real(kind=rb), intent(in) :: cpdair     ! Specific heat capacity of dry air
                                              ! at constant pressure at 273 K
                                              ! (J kg-1 K-1)

! ------- Local -------

      integer(kind=im) :: ibnd, igc, ig, ind, ipr
      integer(kind=im) :: igcsm, iprsm
      integer(kind=im) :: itr

      real(kind=rb) :: wtsum, wtsm(mg)
      real(kind=rb) :: tfn

      real(kind=rb), parameter :: expeps = 1.e-20   ! Smallest value for exponential table

! ------- Definitions -------
!     Arrays for 10000-point look-up tables:
!     TAU_TBL  Clear-sky optical depth 
!     EXP_TBL  Exponential lookup table for transmittance
!     PADE     Pade approximation constant (= 0.278)
!     BPADE    Inverse of the Pade approximation constant
!

!jm not thread safe      hvrini = '$Revision: 1.3 $'

! Initialize model data
      call swdatinit(cpdair)
      call swcmbdat              ! g-point interval reduction data
      call swaerpr               ! aerosol optical properties
      call swcldpr               ! cloud optical properties
      call swatmref              ! reference MLS profile
! Moved to module_ra_rrtmg_sw for WRF
!      call sw_kgb16              ! molecular absorption coefficients
!      call sw_kgb17
!      call sw_kgb18
!      call sw_kgb19
!      call sw_kgb20
!      call sw_kgb21
!      call sw_kgb22
!      call sw_kgb23
!      call sw_kgb24
!      call sw_kgb25
!      call sw_kgb26
!      call sw_kgb27
!      call sw_kgb28
!      call sw_kgb29

! Define exponential lookup tables for transmittance. Tau is
! computed as a function of the tau transition function, and transmittance 
! is calculated as a function of tau.  All tables are computed at intervals 
! of 0.0001.  The inverse of the constant used in the Pade approximation to 
! the tau transition function is set to bpade.

      exp_tbl(0) = 1.0_rb
      exp_tbl(ntbl) = expeps
      bpade = 1.0_rb / pade
      do itr = 1, ntbl-1
         tfn = float(itr) / float(ntbl)
         tau_tbl = bpade * tfn / (1._rb - tfn)
         exp_tbl(itr) = exp(-tau_tbl)
         if (exp_tbl(itr) .le. expeps) exp_tbl(itr) = expeps
      enddo

! Perform g-point reduction from 16 per band (224 total points) to
! a band dependent number (112 total points) for all absorption
! coefficient input data and Planck fraction input data.
! Compute relative weighting for new g-point combinations.

      igcsm = 0
      do ibnd = 1,nbndsw
         iprsm = 0
         if (ngc(ibnd).lt.mg) then
            do igc = 1,ngc(ibnd)
               igcsm = igcsm + 1
               wtsum = 0.
               do ipr = 1, ngn(igcsm)
                  iprsm = iprsm + 1
                  wtsum = wtsum + wt(iprsm)
               enddo
               wtsm(igc) = wtsum
            enddo
            do ig = 1, ng(ibnd+15)
               ind = (ibnd-1)*mg + ig
               rwgt(ind) = wt(ig)/wtsm(ngm(ind))
            enddo
         else
            do ig = 1, ng(ibnd+15)
               igcsm = igcsm + 1
               ind = (ibnd-1)*mg + ig
               rwgt(ind) = 1.0_rb
            enddo
         endif
      enddo

! Reduce g-points for absorption coefficient data in each LW spectral band.

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

      end subroutine rrtmg_sw_ini

!***************************************************************************
      subroutine swdatinit(cpdair)
!***************************************************************************

! --------- Modules ----------

      use rrsw_con, only: heatfac, grav, planck, boltz, &
                          clight, avogad, alosmt, gascon, radcn1, radcn2, &
                          sbcnst, secdy, oneminus, pi
      use rrsw_vsn

      save 
 
      real(kind=rb), intent(in) :: cpdair     ! Specific heat capacity of dry air
                                              ! at constant pressure at 273 K
                                              ! (J kg-1 K-1)

! Shortwave spectral band limits (wavenumbers)
      wavenum1(:) = (/2600._rb, 3250._rb, 4000._rb, 4650._rb, 5150._rb, 6150._rb, 7700._rb, &
                      8050._rb,12850._rb,16000._rb,22650._rb,29000._rb,38000._rb,  820._rb/)
      wavenum2(:) = (/3250._rb, 4000._rb, 4650._rb, 5150._rb, 6150._rb, 7700._rb, 8050._rb, &
                     12850._rb,16000._rb,22650._rb,29000._rb,38000._rb,50000._rb, 2600._rb/)
      delwave(:) =  (/ 650._rb,  750._rb,  650._rb,  500._rb, 1000._rb, 1550._rb,  350._rb, &
                      4800._rb, 3150._rb, 6650._rb, 6350._rb, 9000._rb,12000._rb, 1780._rb/)

! Spectral band information
      ng(:) = (/16,16,16,16,16,16,16,16,16,16,16,16,16,16/)
      nspa(:) = (/9,9,9,9,1,9,9,1,9,1,0,1,9,1/)
      nspb(:) = (/1,5,1,1,1,5,1,0,1,0,0,1,5,1/)

! Fundamental physical constants from NIST 2002

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

!jm 20141107 moved here for thread safety
      oneminus = 1.0_rb - 1.e-06_rb ! zepsec
      pi = 2._rb * asin(1._rb)

!
!     units are generally cgs
!
!     The first and second radiation constants are taken from NIST.
!     They were previously obtained from the relations:
!          radcn1 = 2.*planck*clight*clight*1.e-07
!          radcn2 = planck*clight/boltz

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
      heatfac = grav * secdy / (cpdair * 1.e2_rb)

      end subroutine swdatinit

!***************************************************************************
      subroutine swcmbdat
!***************************************************************************

      save
 
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

! Use this set for 112 quadrature point (g-point) model
! ------- Data statements -------
      ngc(:) = (/ 6,12, 8, 8,10,10, 2,10, 8, 6, 6, 8, 6,12 /)
      ngs(:) = (/ 6,18,26,34,44,54,56,66,74,80,86,94,100,112 /)
      ngm(:) = (/ 1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6, &           ! band 16
                  1,2,3,4,5,6,6,7,8,8,9,10,10,11,12,12, &      ! band 17
                  1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8, &           ! band 18
                  1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8, &           ! band 19
                  1,2,3,4,5,6,7,8,9,9,10,10,10,10,10,10, &     ! band 20
                  1,2,3,4,5,6,7,8,9,9,10,10,10,10,10,10, &     ! band 21
                  1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2, &           ! band 22
                  1,1,2,2,3,4,5,6,7,8,9,9,10,10,10,10, &       ! band 23
                  1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8, &           ! band 24
                  1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6, &           ! band 25
                  1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6, &           ! band 26
                  1,2,3,4,5,6,7,7,7,7,8,8,8,8,8,8, &           ! band 27
                  1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6, &           ! band 28
                  1,2,3,4,5,5,6,6,7,7,8,8,9,10,11,12 /)        ! band 29
      ngn(:) = (/ 2,2,2,2,4,4, &                               ! band 16
                  1,1,1,1,1,2,1,2,1,2,1,2, &                   ! band 17
                  1,1,1,1,2,2,4,4, &                           ! band 18
                  1,1,1,1,2,2,4,4, &                           ! band 19
                  1,1,1,1,1,1,1,1,2,6, &                       ! band 20
                  1,1,1,1,1,1,1,1,2,6, &                       ! band 21
                  8,8, &                                       ! band 22
                  2,2,1,1,1,1,1,1,2,4, &                       ! band 23
                  2,2,2,2,2,2,2,2, &                           ! band 24
                  1,1,2,2,4,6, &                               ! band 25
                  1,1,2,2,4,6, &                               ! band 26
                  1,1,1,1,1,1,4,6, &                           ! band 27
                  1,1,2,2,4,6, &                               ! band 28
                  1,1,1,1,2,2,2,2,1,1,1,1 /)                   ! band 29
      ngb(:) = (/ 16,16,16,16,16,16, &                         ! band 16
                  17,17,17,17,17,17,17,17,17,17,17,17, &       ! band 17
                  18,18,18,18,18,18,18,18, &                   ! band 18
                  19,19,19,19,19,19,19,19, &                   ! band 19
                  20,20,20,20,20,20,20,20,20,20, &             ! band 20
                  21,21,21,21,21,21,21,21,21,21, &             ! band 21
                  22,22, &                                     ! band 22
                  23,23,23,23,23,23,23,23,23,23, &             ! band 23
                  24,24,24,24,24,24,24,24, &                   ! band 24
                  25,25,25,25,25,25, &                         ! band 25
                  26,26,26,26,26,26, &                         ! band 26
                  27,27,27,27,27,27,27,27, &                   ! band 27
                  28,28,28,28,28,28, &                         ! band 28
                  29,29,29,29,29,29,29,29,29,29,29,29 /)       ! band 29

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
!      ngb(:) = (/ 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16, &   ! band 16
!                  17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17, &   ! band 17
!                  18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18, &   ! band 18
!                  19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19, &   ! band 19
!                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20, &   ! band 20
!                  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21, &   ! band 21
!                  22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22, &   ! band 22
!                  23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23, &   ! band 23
!                  24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24, &   ! band 24
!                  25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25, &   ! band 25
!                  26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26, &   ! band 26
!                  27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27, &   ! band 27
!                  28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28, &   ! band 28
!                  29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29 /)   ! band 29


      wt(:) =  (/ 0.1527534276_rb, 0.1491729617_rb, 0.1420961469_rb, &
                  0.1316886544_rb, 0.1181945205_rb, 0.1019300893_rb, &
                  0.0832767040_rb, 0.0626720116_rb, 0.0424925000_rb, &
                  0.0046269894_rb, 0.0038279891_rb, 0.0030260086_rb, &
                  0.0022199750_rb, 0.0014140010_rb, 0.0005330000_rb, &
                  0.0000750000_rb /)

      end subroutine swcmbdat

!***************************************************************************
      subroutine swaerpr
!***************************************************************************

! Purpose: Define spectral aerosol properties for six ECMWF aerosol types
! as used in the ECMWF IFS model (see module rrsw_aer.F90 for details)
!
! Original: Defined for rrtmg_sw 14 spectral bands, JJMorcrette, ECMWF Feb 2003
! Revision: Reformatted for consistency with rrtmg_lw, MJIacono, AER, Jul 2006

      use rrsw_aer, only : rsrtaua, rsrpiza, rsrasya

      save

      rsrtaua( 1, :) = (/ &
        0.10849_rb, 0.66699_rb, 0.65255_rb, 0.11600_rb, 0.06529_rb, 0.04468_rb/)
      rsrtaua( 2, :) = (/ &
        0.10849_rb, 0.66699_rb, 0.65255_rb, 0.11600_rb, 0.06529_rb, 0.04468_rb/)
      rsrtaua( 3, :) = (/ &
        0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
      rsrtaua( 4, :) = (/ &
        0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
      rsrtaua( 5, :) = (/ &
        0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
      rsrtaua( 6, :) = (/ &
        0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
      rsrtaua( 7, :) = (/ &
        0.20543_rb, 0.84642_rb, 0.84958_rb, 0.21673_rb, 0.28270_rb, 0.10915_rb/)
      rsrtaua( 8, :) = (/ &
        0.52838_rb, 0.93285_rb, 0.93449_rb, 0.53078_rb, 0.67148_rb, 0.46608_rb/)
      rsrtaua( 9, :) = (/ &
        0.52838_rb, 0.93285_rb, 0.93449_rb, 0.53078_rb, 0.67148_rb, 0.46608_rb/)
      rsrtaua(10, :) = (/ &
        1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
      rsrtaua(11, :) = (/ &
        1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
      rsrtaua(12, :) = (/ &
        1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
      rsrtaua(13, :) = (/ &
        1.69446_rb, 1.11855_rb, 1.09212_rb, 1.72145_rb, 1.03858_rb, 1.12044_rb/)
      rsrtaua(14, :) = (/ &
        0.10849_rb, 0.66699_rb, 0.65255_rb, 0.11600_rb, 0.06529_rb, 0.04468_rb/)
 
      rsrpiza( 1, :) = (/ &
        .5230504_rb, .7868518_rb, .8531531_rb, .4048149_rb, .8748231_rb, .2355667_rb/)
      rsrpiza( 2, :) = (/ &
        .5230504_rb, .7868518_rb, .8531531_rb, .4048149_rb, .8748231_rb, .2355667_rb/)
      rsrpiza( 3, :) = (/ &
        .8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb, .9467578_rb, .9955938_rb/)
      rsrpiza( 4, :) = (/ &
        .8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb, .9467578_rb, .9955938_rb/)
      rsrpiza( 5, :) = (/ &
        .8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb, .9467578_rb, .9955938_rb/)
      rsrpiza( 6, :) = (/ &
        .8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb, .9467578_rb, .9955938_rb/)
      rsrpiza( 7, :) = (/ &
        .8287144_rb, .9949396_rb, .9279543_rb, .6765051_rb, .9467578_rb, .9955938_rb/)
      rsrpiza( 8, :) = (/ &
        .8970131_rb, .9984940_rb, .9245594_rb, .7768385_rb, .9532763_rb, .9999999_rb/)
      rsrpiza( 9, :) = (/ &
        .8970131_rb, .9984940_rb, .9245594_rb, .7768385_rb, .9532763_rb, .9999999_rb/)
      rsrpiza(10, :) = (/ &
        .9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb, .9401905_rb, .9999999_rb/)
      rsrpiza(11, :) = (/ &
        .9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb, .9401905_rb, .9999999_rb/)
      rsrpiza(12, :) = (/ &
        .9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb, .9401905_rb, .9999999_rb/)
      rsrpiza(13, :) = (/ &
        .9148907_rb, .9956173_rb, .7504584_rb, .8131335_rb, .9401905_rb, .9999999_rb/)
      rsrpiza(14, :) = (/ &
        .5230504_rb, .7868518_rb, .8531531_rb, .4048149_rb, .8748231_rb, .2355667_rb/)

      rsrasya( 1, :) = (/ &
        0.700610_rb, 0.818871_rb, 0.702399_rb, 0.689886_rb, .4629866_rb, .1907639_rb/)
      rsrasya( 2, :) = (/ &
        0.700610_rb, 0.818871_rb, 0.702399_rb, 0.689886_rb, .4629866_rb, .1907639_rb/)
      rsrasya( 3, :) = (/ &
        0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb, .6105750_rb, .4760794_rb/)
      rsrasya( 4, :) = (/ &
        0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb, .6105750_rb, .4760794_rb/)
      rsrasya( 5, :) = (/ &
        0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb, .6105750_rb, .4760794_rb/)
      rsrasya( 6, :) = (/ &
        0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb, .6105750_rb, .4760794_rb/)
      rsrasya( 7, :) = (/ &
        0.636342_rb, 0.802467_rb, 0.691305_rb, 0.627497_rb, .6105750_rb, .4760794_rb/)
      rsrasya( 8, :) = (/ &
        0.668431_rb, 0.788530_rb, 0.698682_rb, 0.657422_rb, .6735182_rb, .6519706_rb/)
      rsrasya( 9, :) = (/ &
        0.668431_rb, 0.788530_rb, 0.698682_rb, 0.657422_rb, .6735182_rb, .6519706_rb/)
      rsrasya(10, :) = (/ &
        0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb, .7008249_rb, .7270548_rb/)
      rsrasya(11, :) = (/ &
        0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb, .7008249_rb, .7270548_rb/)
      rsrasya(12, :) = (/ &
        0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb, .7008249_rb, .7270548_rb/)
      rsrasya(13, :) = (/ &
        0.729019_rb, 0.803129_rb, 0.784592_rb, 0.712208_rb, .7008249_rb, .7270548_rb/)
      rsrasya(14, :) = (/ &
        0.700610_rb, 0.818871_rb, 0.702399_rb, 0.689886_rb, .4629866_rb, .1907639_rb/)

      end subroutine swaerpr
 
!***************************************************************************
      subroutine cmbgb16s
!***************************************************************************
!
!  Original version:       MJIacono; July 1998
!  Revision for RRTM_SW:   MJIacono; November 2002
!  Revision for RRTMG_SW:  MJIacono; December 2003
!  Revision for F90 reformatting:  MJIacono; July 2006
!
!  The subroutines CMBGB16->CMBGB29 input the absorption coefficient
!  data for each band, which are defined for 16 g-points and 14 spectral
!  bands. The data are combined with appropriate weighting following the
!  g-point mapping arrays specified in RRTMG_SW_INIT.  Solar source 
!  function data in array SFLUXREF are combined without weighting.  All
!  g-point reduced data are put into new arrays for use in RRTMG_SW.
!
!  band 16:  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!
!-----------------------------------------------------------------------

      use rrsw_kg16, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(1)
                  sumk = 0.
                  do ipr = 1, ngn(igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(1)
               sumk = 0.
               do ipr = 1, ngn(igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(1)
            sumk = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(1)
            sumk = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(1)
         sumf = 0.
         do ipr = 1, ngn(igc)
            iprsm = iprsm + 1
            sumf = sumf + sfluxrefo(iprsm)
         enddo
         sfluxref(igc) = sumf
      enddo

      end subroutine cmbgb16s

!***************************************************************************
      subroutine cmbgb17
!***************************************************************************
!
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!-----------------------------------------------------------------------

      use rrsw_kg17, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(2)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(1)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+16)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(2)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(1)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+16)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(2)
            sumk = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+16)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(2)
            sumk = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+16)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(2)
            sumf = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb17

!***************************************************************************
      subroutine cmbgb18
!***************************************************************************
!
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!-----------------------------------------------------------------------

      use rrsw_kg18, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(3)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(2)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+32)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(3)
               sumk = 0.
               do ipr = 1, ngn(ngs(2)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+32)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(3)
            sumk = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+32)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(3)
            sumk = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+32)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(3)
            sumf = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb18

!***************************************************************************
      subroutine cmbgb19
!***************************************************************************
!
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!-----------------------------------------------------------------------

      use rrsw_kg19, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(4)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(3)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+48)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(4)
               sumk = 0.
               do ipr = 1, ngn(ngs(3)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+48)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(4)
            sumk = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+48)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(4)
            sumk = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+48)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(4)
            sumf = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb19

!***************************************************************************
      subroutine cmbgb20
!***************************************************************************
!
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!-----------------------------------------------------------------------

      use rrsw_kg20, only : kao, kbo, selfrefo, forrefo, sfluxrefo, absch4o, &
                            absa, ka, absb, kb, selfref, forref, sfluxref, absch4

! ------- Local -------
      integer(kind=im) :: jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf1, sumf2


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(5)
               sumk = 0.
               do ipr = 1, ngn(ngs(4)+igc)
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
               do ipr = 1, ngn(ngs(4)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+64)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(5)
            sumk = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+64)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(5)
            sumk = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+64)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(5)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(4)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + absch4o(iprsm)*rwgt(iprsm+64)
         enddo
         sfluxref(igc) = sumf1
         absch4(igc) = sumf2
      enddo

      end subroutine cmbgb20

!***************************************************************************
      subroutine cmbgb21
!***************************************************************************
!
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!-----------------------------------------------------------------------

      use rrsw_kg21, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(6)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(5)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+80)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(6)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(5)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+80)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+80)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+80)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(6)
            sumf = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb21

!***************************************************************************
      subroutine cmbgb22
!***************************************************************************
!
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!-----------------------------------------------------------------------

      use rrsw_kg22, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(7)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(6)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+96)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(7)
               sumk = 0.
               do ipr = 1, ngn(ngs(6)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+96)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+96)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+96)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(7)
            sumf = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb22

!***************************************************************************
      subroutine cmbgb23
!***************************************************************************
!
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!-----------------------------------------------------------------------

      use rrsw_kg23, only : kao, selfrefo, forrefo, sfluxrefo, raylo, &
                            absa, ka, selfref, forref, sfluxref, rayl

! ------- Local -------
      integer(kind=im) :: jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf1, sumf2


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(8)
               sumk = 0.
               do ipr = 1, ngn(ngs(7)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+112)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(8)
            sumk = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+112)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(8)
            sumk = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+112)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(8)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(7)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + raylo(iprsm)*rwgt(iprsm+112)
         enddo
         sfluxref(igc) = sumf1
         rayl(igc) = sumf2
      enddo

      end subroutine cmbgb23

!***************************************************************************
      subroutine cmbgb24
!***************************************************************************
!
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)
!-----------------------------------------------------------------------

      use rrsw_kg24, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            abso3ao, abso3bo, raylao, raylbo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref, &
                            abso3a, abso3b, rayla, raylb

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf1, sumf2, sumf3


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(9)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(8)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+128)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(9)
               sumk = 0.
               do ipr = 1, ngn(ngs(8)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+128)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+128)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+128)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(9)
         sumf1 = 0.
         sumf2 = 0.
         sumf3 = 0.
         do ipr = 1, ngn(ngs(8)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + raylbo(iprsm)*rwgt(iprsm+128)
            sumf2 = sumf2 + abso3ao(iprsm)*rwgt(iprsm+128)
            sumf3 = sumf3 + abso3bo(iprsm)*rwgt(iprsm+128)
         enddo
         raylb(igc) = sumf1
         abso3a(igc) = sumf2
         abso3b(igc) = sumf3
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(9)
            sumf1 = 0.
            sumf2 = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumf1 = sumf1 + sfluxrefo(iprsm,jp)
               sumf2 = sumf2 + raylao(iprsm,jp)*rwgt(iprsm+128)
            enddo
            sfluxref(igc,jp) = sumf1
            rayla(igc,jp) = sumf2
         enddo
      enddo

      end subroutine cmbgb24

!***************************************************************************
      subroutine cmbgb25
!***************************************************************************
!
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)
!-----------------------------------------------------------------------

      use rrsw_kg25, only : kao, sfluxrefo, &
                            abso3ao, abso3bo, raylo, &
                            absa, ka, sfluxref, &
                            abso3a, abso3b, rayl

! ------- Local -------
      integer(kind=im) :: jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf1, sumf2, sumf3, sumf4


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(10)
               sumk = 0.
               do ipr = 1, ngn(ngs(9)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+144)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(10)
         sumf1 = 0.
         sumf2 = 0.
         sumf3 = 0.
         sumf4 = 0.
         do ipr = 1, ngn(ngs(9)+igc)
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

      end subroutine cmbgb25

!***************************************************************************
      subroutine cmbgb26
!***************************************************************************
!
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!-----------------------------------------------------------------------

      use rrsw_kg26, only : sfluxrefo, raylo, &
                            sfluxref, rayl

! ------- Local -------
      integer(kind=im) :: igc, ipr, iprsm
      real(kind=rb) :: sumf1, sumf2


      iprsm = 0
      do igc = 1,ngc(11)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(10)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + raylo(iprsm)*rwgt(iprsm+160)
            sumf2 = sumf2 + sfluxrefo(iprsm)
         enddo
         rayl(igc) = sumf1
         sfluxref(igc) = sumf2
      enddo

      end subroutine cmbgb26

!***************************************************************************
      subroutine cmbgb27
!***************************************************************************
!
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)
!-----------------------------------------------------------------------

      use rrsw_kg27, only : kao, kbo, sfluxrefo, raylo, &
                            absa, ka, absb, kb, sfluxref, rayl

! ------- Local -------
      integer(kind=im) :: jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf1, sumf2


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(12)
               sumk = 0.
               do ipr = 1, ngn(ngs(11)+igc)
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
               do ipr = 1, ngn(ngs(11)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+176)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(12)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(11)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + raylo(iprsm)*rwgt(iprsm+176)
         enddo
         sfluxref(igc) = sumf1
         rayl(igc) = sumf2
      enddo

      end subroutine cmbgb27

!***************************************************************************
      subroutine cmbgb28
!***************************************************************************
!
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)
!-----------------------------------------------------------------------

      use rrsw_kg28, only : kao, kbo, sfluxrefo, &
                            absa, ka, absb, kb, sfluxref

! ------- Local -------
      integer(kind=im) :: jn, jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(13)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(12)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+192)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(13)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(12)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+192)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(13)
            sumf = 0.
            do ipr = 1, ngn(ngs(12)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb28

!***************************************************************************
      subroutine cmbgb29
!***************************************************************************
!
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)
!-----------------------------------------------------------------------

      use rrsw_kg29, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absh2oo, absco2o, &
                            absa, ka, absb, kb, selfref, forref, sfluxref, &
                            absh2o, absco2

! ------- Local -------
      integer(kind=im) :: jt, jp, igc, ipr, iprsm
      real(kind=rb) :: sumk, sumf1, sumf2, sumf3


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(14)
               sumk = 0.
               do ipr = 1, ngn(ngs(13)+igc)
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
               do ipr = 1, ngn(ngs(13)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+208)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(14)
            sumk = 0.
            do ipr = 1, ngn(ngs(13)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+208)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(14)
            sumk = 0.
            do ipr = 1, ngn(ngs(13)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+208)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(14)
         sumf1 = 0.
         sumf2 = 0.
         sumf3 = 0.
         do ipr = 1, ngn(ngs(13)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + absco2o(iprsm)*rwgt(iprsm+208)
            sumf3 = sumf3 + absh2oo(iprsm)*rwgt(iprsm+208)
         enddo
         sfluxref(igc) = sumf1
         absco2(igc) = sumf2
         absh2o(igc) = sumf3
      enddo

      end subroutine cmbgb29

!***********************************************************************
      subroutine swcldpr

! Purpose: Define cloud extinction coefficient, single scattering albedo
!          and asymmetry parameter data.
!

! ------- Modules -------

      use rrsw_cld, only : extliq1, ssaliq1, asyliq1, &
                           extice2, ssaice2, asyice2, &
                           extice3, ssaice3, asyice3, fdlice3, &
                           abari, bbari, cbari, dbari, ebari, fbari

      save

!-----------------------------------------------------------------------
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
!    as in Hu and Stamnes, J., Clim., 6, 728-742, (1993) with
!    modified coefficients derived from Mie scattering calculations. 
!    The values for absorption coefficients appropriate for
!    the spectral bands in RRTM/RRTMG have been obtained for a 
!    range of effective radii by an averaging procedure 
!    based on the work of J. Pinto (private communication).
!    Linear interpolation is used to get the absorption 
!    coefficients for the input effective radius.
!
!..Updated tables suggested by Peter Blossey (Univ. Washington) that came from RRTM v3.9 from AER, Inc.
!
!     ------------------------------------------------------------------

! Everything below is for INFLAG = 2.

! Coefficients for Ebert and Curry method
      abari(:) = (/ &
        & 3.448e-03_rb,3.448e-03_rb,3.448e-03_rb,3.448e-03_rb,3.448e-03_rb /)
      bbari(:) = (/ &
        & 2.431e+00_rb,2.431e+00_rb,2.431e+00_rb,2.431e+00_rb,2.431e+00_rb /)
      cbari(:) = (/ &
        & 1.000e-05_rb,1.100e-04_rb,1.240e-02_rb,3.779e-02_rb,4.666e-01_rb /)
      dbari(:) = (/ &
        & 0.000e+00_rb,1.405e-05_rb,6.867e-04_rb,1.284e-03_rb,2.050e-05_rb /)
      ebari(:) = (/ &
        & 7.661e-01_rb,7.730e-01_rb,7.865e-01_rb,8.172e-01_rb,9.595e-01_rb /)
      fbari(:) = (/ &
        & 5.851e-04_rb,5.665e-04_rb,7.204e-04_rb,7.463e-04_rb,1.076e-04_rb /)

! LIQFLAG==1 extinction coefficients, single scattering albedos, and asymmetry parameters
!   Derived from on Mie scattering computations; based on Hu & Stamnes coefficients
!     BAND  16
      extliq1(:, 16) = (/ &
        & 9.004493E-01_rb,6.366723E-01_rb,4.542354E-01_rb,3.468253E-01_rb,2.816431E-01_rb,&
        & 2.383415E-01_rb,2.070854E-01_rb,1.831854E-01_rb,1.642115E-01_rb,1.487539E-01_rb,&
        & 1.359169E-01_rb,1.250900E-01_rb,1.158354E-01_rb,1.078400E-01_rb,1.008646E-01_rb,&
        & 9.472307E-02_rb,8.928000E-02_rb,8.442308E-02_rb,8.005924E-02_rb,7.612231E-02_rb,&
        & 7.255153E-02_rb,6.929539E-02_rb,6.631769E-02_rb,6.358153E-02_rb,6.106231E-02_rb,&
        & 5.873077E-02_rb,5.656924E-02_rb,5.455769E-02_rb,5.267846E-02_rb,5.091923E-02_rb,&
        & 4.926692E-02_rb,4.771154E-02_rb,4.623923E-02_rb,4.484385E-02_rb,4.351539E-02_rb,&
        & 4.224615E-02_rb,4.103385E-02_rb,3.986538E-02_rb,3.874077E-02_rb,3.765462E-02_rb,&
        & 3.660077E-02_rb,3.557384E-02_rb,3.457615E-02_rb,3.360308E-02_rb,3.265000E-02_rb,&
        & 3.171770E-02_rb,3.080538E-02_rb,2.990846E-02_rb,2.903000E-02_rb,2.816461E-02_rb,&
        & 2.731539E-02_rb,2.648231E-02_rb,2.566308E-02_rb,2.485923E-02_rb,2.407000E-02_rb,&
        & 2.329615E-02_rb,2.253769E-02_rb,2.179615E-02_rb /)
!     BAND  17
      extliq1(:, 17) = (/ &
       & 6.741200e-01_rb,5.390739e-01_rb,4.198767e-01_rb,3.332553e-01_rb,2.735633e-01_rb,&
       & 2.317727e-01_rb,2.012760e-01_rb,1.780400e-01_rb,1.596927e-01_rb,1.447980e-01_rb,&
       & 1.324480e-01_rb,1.220347e-01_rb,1.131327e-01_rb,1.054313e-01_rb,9.870534e-02_rb,&
       & 9.278200e-02_rb,8.752599e-02_rb,8.282933e-02_rb,7.860600e-02_rb,7.479133e-02_rb,&
       & 7.132800e-02_rb,6.816733e-02_rb,6.527401e-02_rb,6.261266e-02_rb,6.015934e-02_rb,&
       & 5.788867e-02_rb,5.578134e-02_rb,5.381667e-02_rb,5.198133e-02_rb,5.026067e-02_rb,&
       & 4.864466e-02_rb,4.712267e-02_rb,4.568066e-02_rb,4.431200e-02_rb,4.300867e-02_rb,&
       & 4.176600e-02_rb,4.057400e-02_rb,3.942534e-02_rb,3.832066e-02_rb,3.725068e-02_rb,&
       & 3.621400e-02_rb,3.520533e-02_rb,3.422333e-02_rb,3.326400e-02_rb,3.232467e-02_rb,&
       & 3.140535e-02_rb,3.050400e-02_rb,2.962000e-02_rb,2.875267e-02_rb,2.789800e-02_rb,&
       & 2.705934e-02_rb,2.623667e-02_rb,2.542667e-02_rb,2.463200e-02_rb,2.385267e-02_rb,&
       & 2.308667e-02_rb,2.233667e-02_rb,2.160067e-02_rb /)
!     BAND  18
      extliq1(:, 18) = (/ &
       & 9.250861e-01_rb,6.245692e-01_rb,4.347038e-01_rb,3.320208e-01_rb,2.714869e-01_rb,&
       & 2.309516e-01_rb,2.012592e-01_rb,1.783315e-01_rb,1.600369e-01_rb,1.451000e-01_rb,&
       & 1.326838e-01_rb,1.222069e-01_rb,1.132554e-01_rb,1.055146e-01_rb,9.876000e-02_rb,&
       & 9.281386e-02_rb,8.754000e-02_rb,8.283078e-02_rb,7.860077e-02_rb,7.477769e-02_rb,&
       & 7.130847e-02_rb,6.814461e-02_rb,6.524615e-02_rb,6.258462e-02_rb,6.012847e-02_rb,&
       & 5.785462e-02_rb,5.574231e-02_rb,5.378000e-02_rb,5.194461e-02_rb,5.022462e-02_rb,&
       & 4.860846e-02_rb,4.708462e-02_rb,4.564154e-02_rb,4.427462e-02_rb,4.297231e-02_rb,&
       & 4.172769e-02_rb,4.053693e-02_rb,3.939000e-02_rb,3.828462e-02_rb,3.721692e-02_rb,&
       & 3.618000e-02_rb,3.517077e-02_rb,3.418923e-02_rb,3.323077e-02_rb,3.229154e-02_rb,&
       & 3.137154e-02_rb,3.047154e-02_rb,2.959077e-02_rb,2.872308e-02_rb,2.786846e-02_rb,&
       & 2.703077e-02_rb,2.620923e-02_rb,2.540077e-02_rb,2.460615e-02_rb,2.382693e-02_rb,&
       & 2.306231e-02_rb,2.231231e-02_rb,2.157923e-02_rb /)
!     BAND  19
      extliq1(:, 19) = (/ &
       & 9.298960e-01_rb,5.776460e-01_rb,4.083450e-01_rb,3.211160e-01_rb,2.666390e-01_rb,&
       & 2.281990e-01_rb,1.993250e-01_rb,1.768080e-01_rb,1.587810e-01_rb,1.440390e-01_rb,&
       & 1.317720e-01_rb,1.214150e-01_rb,1.125540e-01_rb,1.048890e-01_rb,9.819600e-02_rb,&
       & 9.230201e-02_rb,8.706900e-02_rb,8.239698e-02_rb,7.819500e-02_rb,7.439899e-02_rb,&
       & 7.095300e-02_rb,6.780700e-02_rb,6.492900e-02_rb,6.228600e-02_rb,5.984600e-02_rb,&
       & 5.758599e-02_rb,5.549099e-02_rb,5.353801e-02_rb,5.171400e-02_rb,5.000500e-02_rb,&
       & 4.840000e-02_rb,4.688500e-02_rb,4.545100e-02_rb,4.409300e-02_rb,4.279700e-02_rb,&
       & 4.156100e-02_rb,4.037700e-02_rb,3.923800e-02_rb,3.813800e-02_rb,3.707600e-02_rb,&
       & 3.604500e-02_rb,3.504300e-02_rb,3.406500e-02_rb,3.310800e-02_rb,3.217700e-02_rb,&
       & 3.126600e-02_rb,3.036800e-02_rb,2.948900e-02_rb,2.862400e-02_rb,2.777500e-02_rb,&
       & 2.694200e-02_rb,2.612300e-02_rb,2.531700e-02_rb,2.452800e-02_rb,2.375100e-02_rb,&
       & 2.299100e-02_rb,2.224300e-02_rb,2.151201e-02_rb /)
!     BAND  20
      extliq1(:, 20) = (/ &
       & 8.780964e-01_rb,5.407031e-01_rb,3.961100e-01_rb,3.166645e-01_rb,2.640455e-01_rb,&
       & 2.261070e-01_rb,1.974820e-01_rb,1.751775e-01_rb,1.573415e-01_rb,1.427725e-01_rb,&
       & 1.306535e-01_rb,1.204195e-01_rb,1.116650e-01_rb,1.040915e-01_rb,9.747550e-02_rb,&
       & 9.164800e-02_rb,8.647649e-02_rb,8.185501e-02_rb,7.770200e-02_rb,7.394749e-02_rb,&
       & 7.053800e-02_rb,6.742700e-02_rb,6.457999e-02_rb,6.196149e-02_rb,5.954450e-02_rb,&
       & 5.730650e-02_rb,5.522949e-02_rb,5.329450e-02_rb,5.148500e-02_rb,4.979000e-02_rb,&
       & 4.819600e-02_rb,4.669301e-02_rb,4.527050e-02_rb,4.391899e-02_rb,4.263500e-02_rb,&
       & 4.140500e-02_rb,4.022850e-02_rb,3.909500e-02_rb,3.800199e-02_rb,3.694600e-02_rb,&
       & 3.592000e-02_rb,3.492250e-02_rb,3.395050e-02_rb,3.300150e-02_rb,3.207250e-02_rb,&
       & 3.116250e-02_rb,3.027100e-02_rb,2.939500e-02_rb,2.853500e-02_rb,2.768900e-02_rb,&
       & 2.686000e-02_rb,2.604350e-02_rb,2.524150e-02_rb,2.445350e-02_rb,2.368049e-02_rb,&
       & 2.292150e-02_rb,2.217800e-02_rb,2.144800e-02_rb /)
!     BAND  21
      extliq1(:, 21) = (/ &
       & 7.937480e-01_rb,5.123036e-01_rb,3.858181e-01_rb,3.099622e-01_rb,2.586829e-01_rb,&
       & 2.217587e-01_rb,1.939755e-01_rb,1.723397e-01_rb,1.550258e-01_rb,1.408600e-01_rb,&
       & 1.290545e-01_rb,1.190661e-01_rb,1.105039e-01_rb,1.030848e-01_rb,9.659387e-02_rb,&
       & 9.086775e-02_rb,8.577807e-02_rb,8.122452e-02_rb,7.712711e-02_rb,7.342193e-02_rb,&
       & 7.005387e-02_rb,6.697840e-02_rb,6.416000e-02_rb,6.156903e-02_rb,5.917484e-02_rb,&
       & 5.695807e-02_rb,5.489968e-02_rb,5.298097e-02_rb,5.118806e-02_rb,4.950645e-02_rb,&
       & 4.792710e-02_rb,4.643581e-02_rb,4.502484e-02_rb,4.368547e-02_rb,4.241001e-02_rb,&
       & 4.118936e-02_rb,4.002193e-02_rb,3.889711e-02_rb,3.781322e-02_rb,3.676387e-02_rb,&
       & 3.574549e-02_rb,3.475548e-02_rb,3.379033e-02_rb,3.284678e-02_rb,3.192420e-02_rb,&
       & 3.102032e-02_rb,3.013484e-02_rb,2.926258e-02_rb,2.840839e-02_rb,2.756742e-02_rb,&
       & 2.674258e-02_rb,2.593064e-02_rb,2.513258e-02_rb,2.435000e-02_rb,2.358064e-02_rb,&
       & 2.282581e-02_rb,2.208548e-02_rb,2.135936e-02_rb /)
!     BAND  22
      extliq1(:, 22) = (/ &
       & 7.533129e-01_rb,5.033129e-01_rb,3.811271e-01_rb,3.062757e-01_rb,2.558729e-01_rb,&
       & 2.196828e-01_rb,1.924372e-01_rb,1.711714e-01_rb,1.541086e-01_rb,1.401114e-01_rb,&
       & 1.284257e-01_rb,1.185200e-01_rb,1.100243e-01_rb,1.026529e-01_rb,9.620142e-02_rb,&
       & 9.050714e-02_rb,8.544428e-02_rb,8.091714e-02_rb,7.684000e-02_rb,7.315429e-02_rb,&
       & 6.980143e-02_rb,6.673999e-02_rb,6.394000e-02_rb,6.136000e-02_rb,5.897715e-02_rb,&
       & 5.677000e-02_rb,5.472285e-02_rb,5.281286e-02_rb,5.102858e-02_rb,4.935429e-02_rb,&
       & 4.778000e-02_rb,4.629714e-02_rb,4.489142e-02_rb,4.355857e-02_rb,4.228715e-02_rb,&
       & 4.107285e-02_rb,3.990857e-02_rb,3.879000e-02_rb,3.770999e-02_rb,3.666429e-02_rb,&
       & 3.565000e-02_rb,3.466286e-02_rb,3.370143e-02_rb,3.276143e-02_rb,3.184143e-02_rb,&
       & 3.094000e-02_rb,3.005714e-02_rb,2.919000e-02_rb,2.833714e-02_rb,2.750000e-02_rb,&
       & 2.667714e-02_rb,2.586714e-02_rb,2.507143e-02_rb,2.429143e-02_rb,2.352428e-02_rb,&
       & 2.277143e-02_rb,2.203429e-02_rb,2.130857e-02_rb /)
!     BAND  23
      extliq1(:, 23) = (/ &
       & 7.079894e-01_rb,4.878198e-01_rb,3.719852e-01_rb,3.001873e-01_rb,2.514795e-01_rb,&
       & 2.163013e-01_rb,1.897100e-01_rb,1.689033e-01_rb,1.521793e-01_rb,1.384449e-01_rb,&
       & 1.269666e-01_rb,1.172326e-01_rb,1.088745e-01_rb,1.016224e-01_rb,9.527085e-02_rb,&
       & 8.966240e-02_rb,8.467543e-02_rb,8.021144e-02_rb,7.619344e-02_rb,7.255676e-02_rb,&
       & 6.924996e-02_rb,6.623030e-02_rb,6.346261e-02_rb,6.091499e-02_rb,5.856325e-02_rb,&
       & 5.638385e-02_rb,5.435930e-02_rb,5.247156e-02_rb,5.070699e-02_rb,4.905230e-02_rb,&
       & 4.749499e-02_rb,4.602611e-02_rb,4.463581e-02_rb,4.331543e-02_rb,4.205647e-02_rb,&
       & 4.085241e-02_rb,3.969978e-02_rb,3.859033e-02_rb,3.751877e-02_rb,3.648168e-02_rb,&
       & 3.547468e-02_rb,3.449553e-02_rb,3.354072e-02_rb,3.260732e-02_rb,3.169438e-02_rb,&
       & 3.079969e-02_rb,2.992146e-02_rb,2.905875e-02_rb,2.821201e-02_rb,2.737873e-02_rb,&
       & 2.656052e-02_rb,2.575586e-02_rb,2.496511e-02_rb,2.418783e-02_rb,2.342500e-02_rb,&
       & 2.267646e-02_rb,2.194177e-02_rb,2.122146e-02_rb /)
!     BAND  24
      extliq1(:, 24) = (/ &
       & 6.850164e-01_rb,4.762468e-01_rb,3.642001e-01_rb,2.946012e-01_rb,2.472001e-01_rb,&
       & 2.128588e-01_rb,1.868537e-01_rb,1.664893e-01_rb,1.501142e-01_rb,1.366620e-01_rb,&
       & 1.254147e-01_rb,1.158721e-01_rb,1.076732e-01_rb,1.005530e-01_rb,9.431306e-02_rb,&
       & 8.879891e-02_rb,8.389232e-02_rb,7.949714e-02_rb,7.553857e-02_rb,7.195474e-02_rb,&
       & 6.869413e-02_rb,6.571444e-02_rb,6.298286e-02_rb,6.046779e-02_rb,5.814474e-02_rb,&
       & 5.599141e-02_rb,5.399114e-02_rb,5.212443e-02_rb,5.037870e-02_rb,4.874321e-02_rb,&
       & 4.720219e-02_rb,4.574813e-02_rb,4.437160e-02_rb,4.306460e-02_rb,4.181810e-02_rb,&
       & 4.062603e-02_rb,3.948252e-02_rb,3.838256e-02_rb,3.732049e-02_rb,3.629192e-02_rb,&
       & 3.529301e-02_rb,3.432190e-02_rb,3.337412e-02_rb,3.244842e-02_rb,3.154175e-02_rb,&
       & 3.065253e-02_rb,2.978063e-02_rb,2.892367e-02_rb,2.808221e-02_rb,2.725478e-02_rb,&
       & 2.644174e-02_rb,2.564175e-02_rb,2.485508e-02_rb,2.408303e-02_rb,2.332365e-02_rb,&
       & 2.257890e-02_rb,2.184824e-02_rb,2.113224e-02_rb /)
!     BAND  25
      extliq1(:, 25) = (/ &
       & 6.673017e-01_rb,4.664520e-01_rb,3.579398e-01_rb,2.902234e-01_rb,2.439904e-01_rb,&
       & 2.104149e-01_rb,1.849277e-01_rb,1.649234e-01_rb,1.488087e-01_rb,1.355515e-01_rb,&
       & 1.244562e-01_rb,1.150329e-01_rb,1.069321e-01_rb,9.989310e-02_rb,9.372070e-02_rb,&
       & 8.826450e-02_rb,8.340622e-02_rb,7.905378e-02_rb,7.513109e-02_rb,7.157859e-02_rb,&
       & 6.834588e-02_rb,6.539114e-02_rb,6.268150e-02_rb,6.018621e-02_rb,5.788098e-02_rb,&
       & 5.574351e-02_rb,5.375699e-02_rb,5.190412e-02_rb,5.017099e-02_rb,4.854497e-02_rb,&
       & 4.701490e-02_rb,4.557030e-02_rb,4.420249e-02_rb,4.290304e-02_rb,4.166427e-02_rb,&
       & 4.047820e-02_rb,3.934232e-02_rb,3.824778e-02_rb,3.719236e-02_rb,3.616931e-02_rb,&
       & 3.517597e-02_rb,3.420856e-02_rb,3.326566e-02_rb,3.234346e-02_rb,3.144122e-02_rb,&
       & 3.055684e-02_rb,2.968798e-02_rb,2.883519e-02_rb,2.799635e-02_rb,2.717228e-02_rb,&
       & 2.636182e-02_rb,2.556424e-02_rb,2.478114e-02_rb,2.401086e-02_rb,2.325657e-02_rb,&
       & 2.251506e-02_rb,2.178594e-02_rb,2.107301e-02_rb /)
!     BAND  26
      extliq1(:, 26) = (/ &
       & 6.552414e-01_rb,4.599454e-01_rb,3.538626e-01_rb,2.873547e-01_rb,2.418033e-01_rb,&
       & 2.086660e-01_rb,1.834885e-01_rb,1.637142e-01_rb,1.477767e-01_rb,1.346583e-01_rb,&
       & 1.236734e-01_rb,1.143412e-01_rb,1.063148e-01_rb,9.933905e-02_rb,9.322026e-02_rb,&
       & 8.780979e-02_rb,8.299230e-02_rb,7.867554e-02_rb,7.478450e-02_rb,7.126053e-02_rb,&
       & 6.805276e-02_rb,6.512143e-02_rb,6.243211e-02_rb,5.995541e-02_rb,5.766712e-02_rb,&
       & 5.554484e-02_rb,5.357246e-02_rb,5.173222e-02_rb,5.001069e-02_rb,4.839505e-02_rb,&
       & 4.687471e-02_rb,4.543861e-02_rb,4.407857e-02_rb,4.278577e-02_rb,4.155331e-02_rb,&
       & 4.037322e-02_rb,3.924302e-02_rb,3.815376e-02_rb,3.710172e-02_rb,3.608296e-02_rb,&
       & 3.509330e-02_rb,3.412980e-02_rb,3.319009e-02_rb,3.227106e-02_rb,3.137157e-02_rb,&
       & 3.048950e-02_rb,2.962365e-02_rb,2.877297e-02_rb,2.793726e-02_rb,2.711500e-02_rb,&
       & 2.630666e-02_rb,2.551206e-02_rb,2.473052e-02_rb,2.396287e-02_rb,2.320861e-02_rb,&
       & 2.246810e-02_rb,2.174162e-02_rb,2.102927e-02_rb /)
!     BAND  27
      extliq1(:, 27) = (/ &
       & 6.430901e-01_rb,4.532134e-01_rb,3.496132e-01_rb,2.844655e-01_rb,2.397347e-01_rb,&
       & 2.071236e-01_rb,1.822976e-01_rb,1.627640e-01_rb,1.469961e-01_rb,1.340006e-01_rb,&
       & 1.231069e-01_rb,1.138441e-01_rb,1.058706e-01_rb,9.893678e-02_rb,9.285166e-02_rb,&
       & 8.746871e-02_rb,8.267411e-02_rb,7.837656e-02_rb,7.450257e-02_rb,7.099318e-02_rb,&
       & 6.779929e-02_rb,6.487987e-02_rb,6.220168e-02_rb,5.973530e-02_rb,5.745636e-02_rb,&
       & 5.534344e-02_rb,5.337986e-02_rb,5.154797e-02_rb,4.983404e-02_rb,4.822582e-02_rb,&
       & 4.671228e-02_rb,4.528321e-02_rb,4.392997e-02_rb,4.264325e-02_rb,4.141647e-02_rb,&
       & 4.024259e-02_rb,3.911767e-02_rb,3.803309e-02_rb,3.698782e-02_rb,3.597140e-02_rb,&
       & 3.498774e-02_rb,3.402852e-02_rb,3.309340e-02_rb,3.217818e-02_rb,3.128292e-02_rb,&
       & 3.040486e-02_rb,2.954230e-02_rb,2.869545e-02_rb,2.786261e-02_rb,2.704372e-02_rb,&
       & 2.623813e-02_rb,2.544668e-02_rb,2.466788e-02_rb,2.390313e-02_rb,2.315136e-02_rb,&
       & 2.241391e-02_rb,2.168921e-02_rb,2.097903e-02_rb /)
!     BAND  28
      extliq1(:, 28) = (/ &
       & 6.367074e-01_rb,4.495768e-01_rb,3.471263e-01_rb,2.826149e-01_rb,2.382868e-01_rb,&
       & 2.059640e-01_rb,1.813562e-01_rb,1.619881e-01_rb,1.463436e-01_rb,1.334402e-01_rb,&
       & 1.226166e-01_rb,1.134096e-01_rb,1.054829e-01_rb,9.858838e-02_rb,9.253790e-02_rb,&
       & 8.718582e-02_rb,8.241830e-02_rb,7.814482e-02_rb,7.429212e-02_rb,7.080165e-02_rb,&
       & 6.762385e-02_rb,6.471838e-02_rb,6.205388e-02_rb,5.959726e-02_rb,5.732871e-02_rb,&
       & 5.522402e-02_rb,5.326793e-02_rb,5.144230e-02_rb,4.973440e-02_rb,4.813188e-02_rb,&
       & 4.662283e-02_rb,4.519798e-02_rb,4.384833e-02_rb,4.256541e-02_rb,4.134253e-02_rb,&
       & 4.017136e-02_rb,3.904911e-02_rb,3.796779e-02_rb,3.692364e-02_rb,3.591182e-02_rb,&
       & 3.492930e-02_rb,3.397230e-02_rb,3.303920e-02_rb,3.212572e-02_rb,3.123278e-02_rb,&
       & 3.035519e-02_rb,2.949493e-02_rb,2.864985e-02_rb,2.781840e-02_rb,2.700197e-02_rb,&
       & 2.619682e-02_rb,2.540674e-02_rb,2.462966e-02_rb,2.386613e-02_rb,2.311602e-02_rb,&
       & 2.237846e-02_rb,2.165660e-02_rb,2.094756e-02_rb /)
!     BAND  29
      extliq1(:, 29) = (/ &
       & 4.298416e-01_rb,4.391639e-01_rb,3.975030e-01_rb,3.443028e-01_rb,2.957345e-01_rb,&
       & 2.556461e-01_rb,2.234755e-01_rb,1.976636e-01_rb,1.767428e-01_rb,1.595611e-01_rb,&
       & 1.452636e-01_rb,1.332156e-01_rb,1.229481e-01_rb,1.141059e-01_rb,1.064208e-01_rb,&
       & 9.968527e-02_rb,9.373833e-02_rb,8.845221e-02_rb,8.372112e-02_rb,7.946667e-02_rb,&
       & 7.561807e-02_rb,7.212029e-02_rb,6.893166e-02_rb,6.600944e-02_rb,6.332277e-02_rb,&
       & 6.084277e-02_rb,5.854721e-02_rb,5.641361e-02_rb,5.442639e-02_rb,5.256750e-02_rb,&
       & 5.082499e-02_rb,4.918556e-02_rb,4.763694e-02_rb,4.617222e-02_rb,4.477861e-02_rb,&
       & 4.344861e-02_rb,4.217999e-02_rb,4.096111e-02_rb,3.978638e-02_rb,3.865361e-02_rb,&
       & 3.755473e-02_rb,3.649028e-02_rb,3.545361e-02_rb,3.444361e-02_rb,3.345666e-02_rb,&
       & 3.249167e-02_rb,3.154722e-02_rb,3.062083e-02_rb,2.971250e-02_rb,2.882083e-02_rb,&
       & 2.794611e-02_rb,2.708778e-02_rb,2.624500e-02_rb,2.541750e-02_rb,2.460528e-02_rb,&
       & 2.381194e-02_rb,2.303250e-02_rb,2.226833e-02_rb /)
!     BAND  16
      ssaliq1(:, 16) = (/ &
       & 8.362119e-01_rb,8.098460e-01_rb,7.762291e-01_rb,7.486042e-01_rb,7.294172e-01_rb,&
       & 7.161000e-01_rb,7.060656e-01_rb,6.978387e-01_rb,6.907193e-01_rb,6.843551e-01_rb,&
       & 6.785668e-01_rb,6.732450e-01_rb,6.683191e-01_rb,6.637264e-01_rb,6.594307e-01_rb,&
       & 6.554033e-01_rb,6.516115e-01_rb,6.480295e-01_rb,6.446429e-01_rb,6.414306e-01_rb,&
       & 6.383783e-01_rb,6.354750e-01_rb,6.327068e-01_rb,6.300665e-01_rb,6.275376e-01_rb,&
       & 6.251245e-01_rb,6.228136e-01_rb,6.205944e-01_rb,6.184720e-01_rb,6.164330e-01_rb,&
       & 6.144742e-01_rb,6.125962e-01_rb,6.108004e-01_rb,6.090740e-01_rb,6.074200e-01_rb,&
       & 6.058381e-01_rb,6.043209e-01_rb,6.028681e-01_rb,6.014836e-01_rb,6.001626e-01_rb,&
       & 5.988957e-01_rb,5.976864e-01_rb,5.965390e-01_rb,5.954379e-01_rb,5.943972e-01_rb,&
       & 5.934019e-01_rb,5.924624e-01_rb,5.915579e-01_rb,5.907025e-01_rb,5.898913e-01_rb,&
       & 5.891213e-01_rb,5.883815e-01_rb,5.876851e-01_rb,5.870158e-01_rb,5.863868e-01_rb,&
       & 5.857821e-01_rb,5.852111e-01_rb,5.846579e-01_rb /)
!     BAND  17
      ssaliq1(:, 17) = (/ &
       & 6.995459e-01_rb,7.158012e-01_rb,7.076001e-01_rb,6.927244e-01_rb,6.786434e-01_rb,&
       & 6.673545e-01_rb,6.585859e-01_rb,6.516314e-01_rb,6.459010e-01_rb,6.410225e-01_rb,&
       & 6.367574e-01_rb,6.329554e-01_rb,6.295119e-01_rb,6.263595e-01_rb,6.234462e-01_rb,&
       & 6.207274e-01_rb,6.181755e-01_rb,6.157678e-01_rb,6.134880e-01_rb,6.113173e-01_rb,&
       & 6.092495e-01_rb,6.072689e-01_rb,6.053717e-01_rb,6.035507e-01_rb,6.018001e-01_rb,&
       & 6.001134e-01_rb,5.984951e-01_rb,5.969294e-01_rb,5.954256e-01_rb,5.939698e-01_rb,&
       & 5.925716e-01_rb,5.912265e-01_rb,5.899270e-01_rb,5.886771e-01_rb,5.874746e-01_rb,&
       & 5.863185e-01_rb,5.852077e-01_rb,5.841460e-01_rb,5.831249e-01_rb,5.821474e-01_rb,&
       & 5.812078e-01_rb,5.803173e-01_rb,5.794616e-01_rb,5.786443e-01_rb,5.778617e-01_rb,&
       & 5.771236e-01_rb,5.764191e-01_rb,5.757400e-01_rb,5.750971e-01_rb,5.744842e-01_rb,&
       & 5.739012e-01_rb,5.733482e-01_rb,5.728175e-01_rb,5.723214e-01_rb,5.718383e-01_rb,&
       & 5.713827e-01_rb,5.709471e-01_rb,5.705330e-01_rb /)
!     BAND  18
      ssaliq1(:, 18) = (/ &
       & 9.929711e-01_rb,9.896942e-01_rb,9.852408e-01_rb,9.806820e-01_rb,9.764512e-01_rb,&
       & 9.725375e-01_rb,9.688677e-01_rb,9.653832e-01_rb,9.620552e-01_rb,9.588522e-01_rb,&
       & 9.557475e-01_rb,9.527265e-01_rb,9.497731e-01_rb,9.468756e-01_rb,9.440270e-01_rb,&
       & 9.412230e-01_rb,9.384592e-01_rb,9.357287e-01_rb,9.330369e-01_rb,9.303778e-01_rb,&
       & 9.277502e-01_rb,9.251546e-01_rb,9.225907e-01_rb,9.200553e-01_rb,9.175521e-01_rb,&
       & 9.150773e-01_rb,9.126352e-01_rb,9.102260e-01_rb,9.078485e-01_rb,9.055057e-01_rb,&
       & 9.031978e-01_rb,9.009306e-01_rb,8.987010e-01_rb,8.965177e-01_rb,8.943774e-01_rb,&
       & 8.922869e-01_rb,8.902430e-01_rb,8.882551e-01_rb,8.863182e-01_rb,8.844373e-01_rb,&
       & 8.826143e-01_rb,8.808499e-01_rb,8.791413e-01_rb,8.774940e-01_rb,8.759019e-01_rb,&
       & 8.743650e-01_rb,8.728941e-01_rb,8.714712e-01_rb,8.701065e-01_rb,8.688008e-01_rb,&
       & 8.675409e-01_rb,8.663295e-01_rb,8.651714e-01_rb,8.640637e-01_rb,8.629943e-01_rb,&
       & 8.619762e-01_rb,8.609995e-01_rb,8.600581e-01_rb /)
!     BAND  19
      ssaliq1(:, 19) = (/ &
       & 9.910612e-01_rb,9.854226e-01_rb,9.795008e-01_rb,9.742920e-01_rb,9.695996e-01_rb,&
       & 9.652274e-01_rb,9.610648e-01_rb,9.570521e-01_rb,9.531397e-01_rb,9.493086e-01_rb,&
       & 9.455413e-01_rb,9.418362e-01_rb,9.381902e-01_rb,9.346016e-01_rb,9.310718e-01_rb,&
       & 9.275957e-01_rb,9.241757e-01_rb,9.208038e-01_rb,9.174802e-01_rb,9.142058e-01_rb,&
       & 9.109753e-01_rb,9.077895e-01_rb,9.046433e-01_rb,9.015409e-01_rb,8.984784e-01_rb,&
       & 8.954572e-01_rb,8.924748e-01_rb,8.895367e-01_rb,8.866395e-01_rb,8.837864e-01_rb,&
       & 8.809819e-01_rb,8.782267e-01_rb,8.755231e-01_rb,8.728712e-01_rb,8.702802e-01_rb,&
       & 8.677443e-01_rb,8.652733e-01_rb,8.628678e-01_rb,8.605300e-01_rb,8.582593e-01_rb,&
       & 8.560596e-01_rb,8.539352e-01_rb,8.518782e-01_rb,8.498915e-01_rb,8.479790e-01_rb,&
       & 8.461384e-01_rb,8.443645e-01_rb,8.426613e-01_rb,8.410229e-01_rb,8.394495e-01_rb,&
       & 8.379428e-01_rb,8.364967e-01_rb,8.351117e-01_rb,8.337820e-01_rb,8.325091e-01_rb,&
       & 8.312874e-01_rb,8.301169e-01_rb,8.289985e-01_rb /)
!     BAND  20
      ssaliq1(:, 20) = (/ &
       & 9.969802e-01_rb,9.950445e-01_rb,9.931448e-01_rb,9.914272e-01_rb,9.898652e-01_rb,&
       & 9.884250e-01_rb,9.870637e-01_rb,9.857482e-01_rb,9.844558e-01_rb,9.831755e-01_rb,&
       & 9.819068e-01_rb,9.806477e-01_rb,9.794000e-01_rb,9.781666e-01_rb,9.769461e-01_rb,&
       & 9.757386e-01_rb,9.745459e-01_rb,9.733650e-01_rb,9.721953e-01_rb,9.710398e-01_rb,&
       & 9.698936e-01_rb,9.687583e-01_rb,9.676334e-01_rb,9.665192e-01_rb,9.654132e-01_rb,&
       & 9.643208e-01_rb,9.632374e-01_rb,9.621625e-01_rb,9.611003e-01_rb,9.600518e-01_rb,&
       & 9.590144e-01_rb,9.579922e-01_rb,9.569864e-01_rb,9.559948e-01_rb,9.550239e-01_rb,&
       & 9.540698e-01_rb,9.531382e-01_rb,9.522280e-01_rb,9.513409e-01_rb,9.504772e-01_rb,&
       & 9.496360e-01_rb,9.488220e-01_rb,9.480327e-01_rb,9.472693e-01_rb,9.465333e-01_rb,&
       & 9.458211e-01_rb,9.451344e-01_rb,9.444732e-01_rb,9.438372e-01_rb,9.432268e-01_rb,&
       & 9.426391e-01_rb,9.420757e-01_rb,9.415308e-01_rb,9.410102e-01_rb,9.405115e-01_rb,&
       & 9.400326e-01_rb,9.395716e-01_rb,9.391313e-01_rb /)
!     BAND  21
      ssaliq1(:, 21) = (/ &
       & 9.980034e-01_rb,9.968572e-01_rb,9.958696e-01_rb,9.949747e-01_rb,9.941241e-01_rb,&
       & 9.933043e-01_rb,9.924971e-01_rb,9.916978e-01_rb,9.909023e-01_rb,9.901046e-01_rb,&
       & 9.893087e-01_rb,9.885146e-01_rb,9.877195e-01_rb,9.869283e-01_rb,9.861379e-01_rb,&
       & 9.853523e-01_rb,9.845715e-01_rb,9.837945e-01_rb,9.830217e-01_rb,9.822567e-01_rb,&
       & 9.814935e-01_rb,9.807356e-01_rb,9.799815e-01_rb,9.792332e-01_rb,9.784845e-01_rb,&
       & 9.777424e-01_rb,9.770042e-01_rb,9.762695e-01_rb,9.755416e-01_rb,9.748152e-01_rb,&
       & 9.740974e-01_rb,9.733873e-01_rb,9.726813e-01_rb,9.719861e-01_rb,9.713010e-01_rb,&
       & 9.706262e-01_rb,9.699647e-01_rb,9.693144e-01_rb,9.686794e-01_rb,9.680596e-01_rb,&
       & 9.674540e-01_rb,9.668657e-01_rb,9.662926e-01_rb,9.657390e-01_rb,9.652019e-01_rb,&
       & 9.646820e-01_rb,9.641784e-01_rb,9.636945e-01_rb,9.632260e-01_rb,9.627743e-01_rb,&
       & 9.623418e-01_rb,9.619227e-01_rb,9.615194e-01_rb,9.611341e-01_rb,9.607629e-01_rb,&
       & 9.604057e-01_rb,9.600622e-01_rb,9.597322e-01_rb /)
!     BAND  22
      ssaliq1(:, 22) = (/ &
       & 9.988219e-01_rb,9.981767e-01_rb,9.976168e-01_rb,9.971066e-01_rb,9.966195e-01_rb,&
       & 9.961566e-01_rb,9.956995e-01_rb,9.952481e-01_rb,9.947982e-01_rb,9.943495e-01_rb,&
       & 9.938955e-01_rb,9.934368e-01_rb,9.929825e-01_rb,9.925239e-01_rb,9.920653e-01_rb,&
       & 9.916096e-01_rb,9.911552e-01_rb,9.907067e-01_rb,9.902594e-01_rb,9.898178e-01_rb,&
       & 9.893791e-01_rb,9.889453e-01_rb,9.885122e-01_rb,9.880837e-01_rb,9.876567e-01_rb,&
       & 9.872331e-01_rb,9.868121e-01_rb,9.863938e-01_rb,9.859790e-01_rb,9.855650e-01_rb,&
       & 9.851548e-01_rb,9.847491e-01_rb,9.843496e-01_rb,9.839521e-01_rb,9.835606e-01_rb,&
       & 9.831771e-01_rb,9.827975e-01_rb,9.824292e-01_rb,9.820653e-01_rb,9.817124e-01_rb,&
       & 9.813644e-01_rb,9.810291e-01_rb,9.807020e-01_rb,9.803864e-01_rb,9.800782e-01_rb,&
       & 9.797821e-01_rb,9.794958e-01_rb,9.792179e-01_rb,9.789509e-01_rb,9.786940e-01_rb,&
       & 9.784460e-01_rb,9.782090e-01_rb,9.779789e-01_rb,9.777553e-01_rb,9.775425e-01_rb,&
       & 9.773387e-01_rb,9.771420e-01_rb,9.769529e-01_rb /)
!     BAND  23
      ssaliq1(:, 23) = (/ &
       & 9.998902e-01_rb,9.998395e-01_rb,9.997915e-01_rb,9.997442e-01_rb,9.997016e-01_rb,&
       & 9.996600e-01_rb,9.996200e-01_rb,9.995806e-01_rb,9.995411e-01_rb,9.995005e-01_rb,&
       & 9.994589e-01_rb,9.994178e-01_rb,9.993766e-01_rb,9.993359e-01_rb,9.992948e-01_rb,&
       & 9.992533e-01_rb,9.992120e-01_rb,9.991723e-01_rb,9.991313e-01_rb,9.990906e-01_rb,&
       & 9.990510e-01_rb,9.990113e-01_rb,9.989716e-01_rb,9.989323e-01_rb,9.988923e-01_rb,&
       & 9.988532e-01_rb,9.988140e-01_rb,9.987761e-01_rb,9.987373e-01_rb,9.986989e-01_rb,&
       & 9.986597e-01_rb,9.986239e-01_rb,9.985861e-01_rb,9.985485e-01_rb,9.985123e-01_rb,&
       & 9.984762e-01_rb,9.984415e-01_rb,9.984065e-01_rb,9.983722e-01_rb,9.983398e-01_rb,&
       & 9.983078e-01_rb,9.982758e-01_rb,9.982461e-01_rb,9.982157e-01_rb,9.981872e-01_rb,&
       & 9.981595e-01_rb,9.981324e-01_rb,9.981068e-01_rb,9.980811e-01_rb,9.980580e-01_rb,&
       & 9.980344e-01_rb,9.980111e-01_rb,9.979908e-01_rb,9.979690e-01_rb,9.979492e-01_rb,&
       & 9.979316e-01_rb,9.979116e-01_rb,9.978948e-01_rb /)
!     BAND  24
      ssaliq1(:, 24) = (/ &
       & 9.999978e-01_rb,9.999948e-01_rb,9.999915e-01_rb,9.999905e-01_rb,9.999896e-01_rb,&
       & 9.999887e-01_rb,9.999888e-01_rb,9.999888e-01_rb,9.999870e-01_rb,9.999854e-01_rb,&
       & 9.999855e-01_rb,9.999856e-01_rb,9.999839e-01_rb,9.999834e-01_rb,9.999829e-01_rb,&
       & 9.999809e-01_rb,9.999816e-01_rb,9.999793e-01_rb,9.999782e-01_rb,9.999779e-01_rb,&
       & 9.999772e-01_rb,9.999764e-01_rb,9.999756e-01_rb,9.999744e-01_rb,9.999744e-01_rb,&
       & 9.999736e-01_rb,9.999729e-01_rb,9.999716e-01_rb,9.999706e-01_rb,9.999692e-01_rb,&
       & 9.999690e-01_rb,9.999675e-01_rb,9.999673e-01_rb,9.999660e-01_rb,9.999654e-01_rb,&
       & 9.999647e-01_rb,9.999647e-01_rb,9.999625e-01_rb,9.999620e-01_rb,9.999614e-01_rb,&
       & 9.999613e-01_rb,9.999607e-01_rb,9.999604e-01_rb,9.999594e-01_rb,9.999589e-01_rb,&
       & 9.999586e-01_rb,9.999567e-01_rb,9.999550e-01_rb,9.999557e-01_rb,9.999542e-01_rb,&
       & 9.999546e-01_rb,9.999539e-01_rb,9.999536e-01_rb,9.999526e-01_rb,9.999523e-01_rb,&
       & 9.999508e-01_rb,9.999534e-01_rb,9.999507e-01_rb /)
!     BAND  25
      ssaliq1(:, 25) = (/ &
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,&
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,&
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,&
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,9.999995e-01_rb,&
       & 9.999995e-01_rb,9.999990e-01_rb,9.999991e-01_rb,9.999991e-01_rb,9.999990e-01_rb,&
       & 9.999989e-01_rb,9.999988e-01_rb,9.999988e-01_rb,9.999986e-01_rb,9.999988e-01_rb,&
       & 9.999986e-01_rb,9.999987e-01_rb,9.999986e-01_rb,9.999985e-01_rb,9.999985e-01_rb,&
       & 9.999985e-01_rb,9.999985e-01_rb,9.999983e-01_rb,9.999983e-01_rb,9.999981e-01_rb,&
       & 9.999981e-01_rb,9.999986e-01_rb,9.999985e-01_rb,9.999983e-01_rb,9.999984e-01_rb,&
       & 9.999982e-01_rb,9.999983e-01_rb,9.999982e-01_rb,9.999980e-01_rb,9.999981e-01_rb,&
       & 9.999978e-01_rb,9.999979e-01_rb,9.999985e-01_rb,9.999985e-01_rb,9.999983e-01_rb,&
       & 9.999983e-01_rb,9.999983e-01_rb,9.999983e-01_rb /)
!     BAND  26
      ssaliq1(:, 26) = (/ &
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,&
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,&
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,&
       & 1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,1.000000e+00_rb,9.999991e-01_rb,&
       & 9.999990e-01_rb,9.999992e-01_rb,9.999995e-01_rb,9.999986e-01_rb,9.999994e-01_rb,&
       & 9.999985e-01_rb,9.999980e-01_rb,9.999984e-01_rb,9.999983e-01_rb,9.999979e-01_rb,&
       & 9.999969e-01_rb,9.999977e-01_rb,9.999971e-01_rb,9.999969e-01_rb,9.999969e-01_rb,&
       & 9.999965e-01_rb,9.999970e-01_rb,9.999985e-01_rb,9.999973e-01_rb,9.999961e-01_rb,&
       & 9.999968e-01_rb,9.999952e-01_rb,9.999970e-01_rb,9.999974e-01_rb,9.999965e-01_rb,&
       & 9.999969e-01_rb,9.999970e-01_rb,9.999970e-01_rb,9.999960e-01_rb,9.999923e-01_rb,&
       & 9.999958e-01_rb,9.999937e-01_rb,9.999960e-01_rb,9.999953e-01_rb,9.999946e-01_rb,&
       & 9.999946e-01_rb,9.999957e-01_rb,9.999951e-01_rb /)
!     BAND  27
      ssaliq1(:, 27) = (/ &
       & 1.000000e+00_rb,1.000000e+00_rb,9.999983e-01_rb,9.999979e-01_rb,9.999965e-01_rb,&
       & 9.999949e-01_rb,9.999948e-01_rb,9.999918e-01_rb,9.999917e-01_rb,9.999923e-01_rb,&
       & 9.999908e-01_rb,9.999889e-01_rb,9.999902e-01_rb,9.999895e-01_rb,9.999881e-01_rb,&
       & 9.999882e-01_rb,9.999876e-01_rb,9.999866e-01_rb,9.999866e-01_rb,9.999858e-01_rb,&
       & 9.999860e-01_rb,9.999852e-01_rb,9.999836e-01_rb,9.999831e-01_rb,9.999818e-01_rb,&
       & 9.999808e-01_rb,9.999816e-01_rb,9.999800e-01_rb,9.999783e-01_rb,9.999780e-01_rb,&
       & 9.999763e-01_rb,9.999746e-01_rb,9.999731e-01_rb,9.999713e-01_rb,9.999762e-01_rb,&
       & 9.999740e-01_rb,9.999670e-01_rb,9.999703e-01_rb,9.999687e-01_rb,9.999666e-01_rb,&
       & 9.999683e-01_rb,9.999667e-01_rb,9.999611e-01_rb,9.999635e-01_rb,9.999600e-01_rb,&
       & 9.999635e-01_rb,9.999594e-01_rb,9.999601e-01_rb,9.999586e-01_rb,9.999559e-01_rb,&
       & 9.999569e-01_rb,9.999558e-01_rb,9.999523e-01_rb,9.999535e-01_rb,9.999529e-01_rb,&
       & 9.999553e-01_rb,9.999495e-01_rb,9.999490e-01_rb /)
!     BAND  28
      ssaliq1(:, 28) = (/ &
       & 9.999920e-01_rb,9.999873e-01_rb,9.999855e-01_rb,9.999832e-01_rb,9.999807e-01_rb,&
       & 9.999778e-01_rb,9.999754e-01_rb,9.999721e-01_rb,9.999692e-01_rb,9.999651e-01_rb,&
       & 9.999621e-01_rb,9.999607e-01_rb,9.999567e-01_rb,9.999546e-01_rb,9.999521e-01_rb,&
       & 9.999491e-01_rb,9.999457e-01_rb,9.999439e-01_rb,9.999403e-01_rb,9.999374e-01_rb,&
       & 9.999353e-01_rb,9.999315e-01_rb,9.999282e-01_rb,9.999244e-01_rb,9.999234e-01_rb,&
       & 9.999189e-01_rb,9.999130e-01_rb,9.999117e-01_rb,9.999073e-01_rb,9.999020e-01_rb,&
       & 9.998993e-01_rb,9.998987e-01_rb,9.998922e-01_rb,9.998893e-01_rb,9.998869e-01_rb,&
       & 9.998805e-01_rb,9.998778e-01_rb,9.998751e-01_rb,9.998708e-01_rb,9.998676e-01_rb,&
       & 9.998624e-01_rb,9.998642e-01_rb,9.998582e-01_rb,9.998547e-01_rb,9.998546e-01_rb,&
       & 9.998477e-01_rb,9.998487e-01_rb,9.998466e-01_rb,9.998403e-01_rb,9.998412e-01_rb,&
       & 9.998406e-01_rb,9.998342e-01_rb,9.998326e-01_rb,9.998333e-01_rb,9.998328e-01_rb,&
       & 9.998290e-01_rb,9.998276e-01_rb,9.998249e-01_rb /)
!     BAND  29
      ssaliq1(:, 29) = (/ &
       & 8.383753e-01_rb,8.461471e-01_rb,8.373325e-01_rb,8.212889e-01_rb,8.023834e-01_rb,&
       & 7.829501e-01_rb,7.641777e-01_rb,7.466000e-01_rb,7.304023e-01_rb,7.155998e-01_rb,&
       & 7.021259e-01_rb,6.898840e-01_rb,6.787615e-01_rb,6.686479e-01_rb,6.594414e-01_rb,&
       & 6.510417e-01_rb,6.433668e-01_rb,6.363335e-01_rb,6.298788e-01_rb,6.239398e-01_rb,&
       & 6.184633e-01_rb,6.134055e-01_rb,6.087228e-01_rb,6.043786e-01_rb,6.003439e-01_rb,&
       & 5.965910e-01_rb,5.930917e-01_rb,5.898280e-01_rb,5.867798e-01_rb,5.839264e-01_rb,&
       & 5.812576e-01_rb,5.787592e-01_rb,5.764163e-01_rb,5.742189e-01_rb,5.721598e-01_rb,&
       & 5.702286e-01_rb,5.684182e-01_rb,5.667176e-01_rb,5.651237e-01_rb,5.636253e-01_rb,&
       & 5.622228e-01_rb,5.609074e-01_rb,5.596713e-01_rb,5.585089e-01_rb,5.574223e-01_rb,&
       & 5.564002e-01_rb,5.554411e-01_rb,5.545397e-01_rb,5.536914e-01_rb,5.528967e-01_rb,&
       & 5.521495e-01_rb,5.514457e-01_rb,5.507818e-01_rb,5.501623e-01_rb,5.495750e-01_rb,&
       & 5.490192e-01_rb,5.484980e-01_rb,5.480046e-01_rb /)
!     BAND  16
      asyliq1(:, 16) = (/ &
       & 8.038165e-01_rb,8.014154e-01_rb,7.942381e-01_rb,7.970521e-01_rb,8.086621e-01_rb,&
       & 8.233392e-01_rb,8.374127e-01_rb,8.495742e-01_rb,8.596945e-01_rb,8.680497e-01_rb,&
       & 8.750005e-01_rb,8.808589e-01_rb,8.858749e-01_rb,8.902403e-01_rb,8.940939e-01_rb,&
       & 8.975379e-01_rb,9.006450e-01_rb,9.034741e-01_rb,9.060659e-01_rb,9.084561e-01_rb,&
       & 9.106675e-01_rb,9.127198e-01_rb,9.146332e-01_rb,9.164194e-01_rb,9.180970e-01_rb,&
       & 9.196658e-01_rb,9.211421e-01_rb,9.225352e-01_rb,9.238443e-01_rb,9.250841e-01_rb,&
       & 9.262541e-01_rb,9.273620e-01_rb,9.284081e-01_rb,9.294002e-01_rb,9.303395e-01_rb,&
       & 9.312285e-01_rb,9.320715e-01_rb,9.328716e-01_rb,9.336271e-01_rb,9.343427e-01_rb,&
       & 9.350219e-01_rb,9.356647e-01_rb,9.362728e-01_rb,9.368495e-01_rb,9.373956e-01_rb,&
       & 9.379113e-01_rb,9.383987e-01_rb,9.388608e-01_rb,9.392986e-01_rb,9.397132e-01_rb,&
       & 9.401063e-01_rb,9.404776e-01_rb,9.408299e-01_rb,9.411641e-01_rb,9.414800e-01_rb,&
       & 9.417787e-01_rb,9.420633e-01_rb,9.423364e-01_rb /)
!     BAND  17
      asyliq1(:, 17) = (/ &
       & 8.941000e-01_rb,9.054049e-01_rb,9.049510e-01_rb,9.027216e-01_rb,9.021636e-01_rb,&
       & 9.037878e-01_rb,9.069852e-01_rb,9.109817e-01_rb,9.152013e-01_rb,9.193040e-01_rb,&
       & 9.231177e-01_rb,9.265712e-01_rb,9.296606e-01_rb,9.324048e-01_rb,9.348419e-01_rb,&
       & 9.370131e-01_rb,9.389529e-01_rb,9.406954e-01_rb,9.422727e-01_rb,9.437088e-01_rb,&
       & 9.450221e-01_rb,9.462308e-01_rb,9.473488e-01_rb,9.483830e-01_rb,9.493492e-01_rb,&
       & 9.502541e-01_rb,9.510999e-01_rb,9.518971e-01_rb,9.526455e-01_rb,9.533554e-01_rb,&
       & 9.540249e-01_rb,9.546571e-01_rb,9.552551e-01_rb,9.558258e-01_rb,9.563603e-01_rb,&
       & 9.568713e-01_rb,9.573569e-01_rb,9.578141e-01_rb,9.582485e-01_rb,9.586604e-01_rb,&
       & 9.590525e-01_rb,9.594218e-01_rb,9.597710e-01_rb,9.601052e-01_rb,9.604181e-01_rb,&
       & 9.607159e-01_rb,9.609979e-01_rb,9.612655e-01_rb,9.615184e-01_rb,9.617564e-01_rb,&
       & 9.619860e-01_rb,9.622009e-01_rb,9.624031e-01_rb,9.625957e-01_rb,9.627792e-01_rb,&
       & 9.629530e-01_rb,9.631171e-01_rb,9.632746e-01_rb /)
!     BAND  18
      asyliq1(:, 18) = (/ &
       & 8.574638e-01_rb,8.351383e-01_rb,8.142977e-01_rb,8.083068e-01_rb,8.129284e-01_rb,&
       & 8.215827e-01_rb,8.307238e-01_rb,8.389963e-01_rb,8.460481e-01_rb,8.519273e-01_rb,&
       & 8.568153e-01_rb,8.609116e-01_rb,8.643892e-01_rb,8.673941e-01_rb,8.700248e-01_rb,&
       & 8.723707e-01_rb,8.744902e-01_rb,8.764240e-01_rb,8.782057e-01_rb,8.798593e-01_rb,&
       & 8.814063e-01_rb,8.828573e-01_rb,8.842261e-01_rb,8.855196e-01_rb,8.867497e-01_rb,&
       & 8.879164e-01_rb,8.890316e-01_rb,8.900941e-01_rb,8.911118e-01_rb,8.920832e-01_rb,&
       & 8.930156e-01_rb,8.939091e-01_rb,8.947663e-01_rb,8.955888e-01_rb,8.963786e-01_rb,&
       & 8.971350e-01_rb,8.978617e-01_rb,8.985590e-01_rb,8.992243e-01_rb,8.998631e-01_rb,&
       & 9.004753e-01_rb,9.010602e-01_rb,9.016192e-01_rb,9.021542e-01_rb,9.026644e-01_rb,&
       & 9.031535e-01_rb,9.036194e-01_rb,9.040656e-01_rb,9.044894e-01_rb,9.048933e-01_rb,&
       & 9.052789e-01_rb,9.056481e-01_rb,9.060004e-01_rb,9.063343e-01_rb,9.066544e-01_rb,&
       & 9.069604e-01_rb,9.072512e-01_rb,9.075290e-01_rb /)
!     BAND  19
      asyliq1(:, 19) = (/ &
       & 8.349569e-01_rb,8.034579e-01_rb,7.932136e-01_rb,8.010156e-01_rb,8.137083e-01_rb,&
       & 8.255339e-01_rb,8.351938e-01_rb,8.428286e-01_rb,8.488944e-01_rb,8.538187e-01_rb,&
       & 8.579255e-01_rb,8.614473e-01_rb,8.645338e-01_rb,8.672908e-01_rb,8.697947e-01_rb,&
       & 8.720843e-01_rb,8.742015e-01_rb,8.761718e-01_rb,8.780160e-01_rb,8.797479e-01_rb,&
       & 8.813810e-01_rb,8.829250e-01_rb,8.843907e-01_rb,8.857822e-01_rb,8.871059e-01_rb,&
       & 8.883724e-01_rb,8.895810e-01_rb,8.907384e-01_rb,8.918456e-01_rb,8.929083e-01_rb,&
       & 8.939284e-01_rb,8.949060e-01_rb,8.958463e-01_rb,8.967486e-01_rb,8.976129e-01_rb,&
       & 8.984463e-01_rb,8.992439e-01_rb,9.000094e-01_rb,9.007438e-01_rb,9.014496e-01_rb,&
       & 9.021235e-01_rb,9.027699e-01_rb,9.033859e-01_rb,9.039772e-01_rb,9.045419e-01_rb,&
       & 9.050819e-01_rb,9.055975e-01_rb,9.060907e-01_rb,9.065607e-01_rb,9.070093e-01_rb,&
       & 9.074389e-01_rb,9.078475e-01_rb,9.082388e-01_rb,9.086117e-01_rb,9.089678e-01_rb,&
       & 9.093081e-01_rb,9.096307e-01_rb,9.099410e-01_rb /)
!     BAND  20
      asyliq1(:, 20) = (/ &
       & 8.109692e-01_rb,7.846657e-01_rb,7.881928e-01_rb,8.009509e-01_rb,8.131208e-01_rb,&
       & 8.230400e-01_rb,8.309448e-01_rb,8.372920e-01_rb,8.424837e-01_rb,8.468166e-01_rb,&
       & 8.504947e-01_rb,8.536642e-01_rb,8.564256e-01_rb,8.588513e-01_rb,8.610011e-01_rb,&
       & 8.629122e-01_rb,8.646262e-01_rb,8.661720e-01_rb,8.675752e-01_rb,8.688582e-01_rb,&
       & 8.700379e-01_rb,8.711300e-01_rb,8.721485e-01_rb,8.731027e-01_rb,8.740010e-01_rb,&
       & 8.748499e-01_rb,8.756564e-01_rb,8.764239e-01_rb,8.771542e-01_rb,8.778523e-01_rb,&
       & 8.785211e-01_rb,8.791601e-01_rb,8.797725e-01_rb,8.803589e-01_rb,8.809173e-01_rb,&
       & 8.814552e-01_rb,8.819705e-01_rb,8.824611e-01_rb,8.829311e-01_rb,8.833791e-01_rb,&
       & 8.838078e-01_rb,8.842148e-01_rb,8.846044e-01_rb,8.849756e-01_rb,8.853291e-01_rb,&
       & 8.856645e-01_rb,8.859841e-01_rb,8.862904e-01_rb,8.865801e-01_rb,8.868551e-01_rb,&
       & 8.871182e-01_rb,8.873673e-01_rb,8.876059e-01_rb,8.878307e-01_rb,8.880462e-01_rb,&
       & 8.882501e-01_rb,8.884453e-01_rb,8.886339e-01_rb /)
!     BAND  21
      asyliq1(:, 21) = (/ &
       & 7.838510e-01_rb,7.803151e-01_rb,7.980477e-01_rb,8.144160e-01_rb,8.261784e-01_rb,&
       & 8.344240e-01_rb,8.404278e-01_rb,8.450391e-01_rb,8.487593e-01_rb,8.518741e-01_rb,&
       & 8.545484e-01_rb,8.568890e-01_rb,8.589560e-01_rb,8.607983e-01_rb,8.624504e-01_rb,&
       & 8.639408e-01_rb,8.652945e-01_rb,8.665301e-01_rb,8.676634e-01_rb,8.687121e-01_rb,&
       & 8.696855e-01_rb,8.705933e-01_rb,8.714448e-01_rb,8.722454e-01_rb,8.730014e-01_rb,&
       & 8.737180e-01_rb,8.743982e-01_rb,8.750436e-01_rb,8.756598e-01_rb,8.762481e-01_rb,&
       & 8.768089e-01_rb,8.773427e-01_rb,8.778532e-01_rb,8.783434e-01_rb,8.788089e-01_rb,&
       & 8.792530e-01_rb,8.796784e-01_rb,8.800845e-01_rb,8.804716e-01_rb,8.808411e-01_rb,&
       & 8.811923e-01_rb,8.815276e-01_rb,8.818472e-01_rb,8.821504e-01_rb,8.824408e-01_rb,&
       & 8.827155e-01_rb,8.829777e-01_rb,8.832269e-01_rb,8.834631e-01_rb,8.836892e-01_rb,&
       & 8.839034e-01_rb,8.841075e-01_rb,8.843021e-01_rb,8.844866e-01_rb,8.846631e-01_rb,&
       & 8.848304e-01_rb,8.849910e-01_rb,8.851425e-01_rb /)
!     BAND  22
      asyliq1(:, 22) = (/ &
       & 7.760783e-01_rb,7.890215e-01_rb,8.090192e-01_rb,8.230252e-01_rb,8.321369e-01_rb,&
       & 8.384258e-01_rb,8.431529e-01_rb,8.469558e-01_rb,8.501499e-01_rb,8.528899e-01_rb,&
       & 8.552899e-01_rb,8.573956e-01_rb,8.592570e-01_rb,8.609098e-01_rb,8.623897e-01_rb,&
       & 8.637169e-01_rb,8.649184e-01_rb,8.660097e-01_rb,8.670096e-01_rb,8.679338e-01_rb,&
       & 8.687896e-01_rb,8.695880e-01_rb,8.703365e-01_rb,8.710422e-01_rb,8.717092e-01_rb,&
       & 8.723378e-01_rb,8.729363e-01_rb,8.735063e-01_rb,8.740475e-01_rb,8.745661e-01_rb,&
       & 8.750560e-01_rb,8.755275e-01_rb,8.759731e-01_rb,8.764000e-01_rb,8.768071e-01_rb,&
       & 8.771942e-01_rb,8.775628e-01_rb,8.779126e-01_rb,8.782483e-01_rb,8.785626e-01_rb,&
       & 8.788610e-01_rb,8.791482e-01_rb,8.794180e-01_rb,8.796765e-01_rb,8.799207e-01_rb,&
       & 8.801522e-01_rb,8.803707e-01_rb,8.805777e-01_rb,8.807749e-01_rb,8.809605e-01_rb,&
       & 8.811362e-01_rb,8.813047e-01_rb,8.814647e-01_rb,8.816131e-01_rb,8.817588e-01_rb,&
       & 8.818930e-01_rb,8.820230e-01_rb,8.821445e-01_rb /)
!     BAND  23
      asyliq1(:, 23) = (/ &
       & 7.847907e-01_rb,8.099917e-01_rb,8.257428e-01_rb,8.350423e-01_rb,8.411971e-01_rb,&
       & 8.457241e-01_rb,8.493010e-01_rb,8.522565e-01_rb,8.547660e-01_rb,8.569311e-01_rb,&
       & 8.588181e-01_rb,8.604729e-01_rb,8.619296e-01_rb,8.632208e-01_rb,8.643725e-01_rb,&
       & 8.654050e-01_rb,8.663363e-01_rb,8.671835e-01_rb,8.679590e-01_rb,8.686707e-01_rb,&
       & 8.693308e-01_rb,8.699433e-01_rb,8.705147e-01_rb,8.710490e-01_rb,8.715497e-01_rb,&
       & 8.720219e-01_rb,8.724669e-01_rb,8.728849e-01_rb,8.732806e-01_rb,8.736550e-01_rb,&
       & 8.740099e-01_rb,8.743435e-01_rb,8.746601e-01_rb,8.749610e-01_rb,8.752449e-01_rb,&
       & 8.755143e-01_rb,8.757688e-01_rb,8.760095e-01_rb,8.762375e-01_rb,8.764532e-01_rb,&
       & 8.766579e-01_rb,8.768506e-01_rb,8.770323e-01_rb,8.772049e-01_rb,8.773690e-01_rb,&
       & 8.775226e-01_rb,8.776679e-01_rb,8.778062e-01_rb,8.779360e-01_rb,8.780587e-01_rb,&
       & 8.781747e-01_rb,8.782852e-01_rb,8.783892e-01_rb,8.784891e-01_rb,8.785824e-01_rb,&
       & 8.786705e-01_rb,8.787546e-01_rb,8.788336e-01_rb /)
!     BAND  24
      asyliq1(:, 24) = (/ &
       & 8.054324e-01_rb,8.266282e-01_rb,8.378075e-01_rb,8.449848e-01_rb,8.502166e-01_rb,&
       & 8.542268e-01_rb,8.573477e-01_rb,8.598022e-01_rb,8.617689e-01_rb,8.633859e-01_rb,&
       & 8.647536e-01_rb,8.659354e-01_rb,8.669807e-01_rb,8.679143e-01_rb,8.687577e-01_rb,&
       & 8.695222e-01_rb,8.702207e-01_rb,8.708591e-01_rb,8.714446e-01_rb,8.719836e-01_rb,&
       & 8.724812e-01_rb,8.729426e-01_rb,8.733689e-01_rb,8.737665e-01_rb,8.741373e-01_rb,&
       & 8.744834e-01_rb,8.748070e-01_rb,8.751131e-01_rb,8.754011e-01_rb,8.756676e-01_rb,&
       & 8.759219e-01_rb,8.761599e-01_rb,8.763857e-01_rb,8.765984e-01_rb,8.767999e-01_rb,&
       & 8.769889e-01_rb,8.771669e-01_rb,8.773373e-01_rb,8.774969e-01_rb,8.776469e-01_rb,&
       & 8.777894e-01_rb,8.779237e-01_rb,8.780505e-01_rb,8.781703e-01_rb,8.782820e-01_rb,&
       & 8.783886e-01_rb,8.784894e-01_rb,8.785844e-01_rb,8.786736e-01_rb,8.787584e-01_rb,&
       & 8.788379e-01_rb,8.789130e-01_rb,8.789849e-01_rb,8.790506e-01_rb,8.791141e-01_rb,&
       & 8.791750e-01_rb,8.792324e-01_rb,8.792867e-01_rb /)
!     BAND  25
      asyliq1(:, 25) = (/ &
       & 8.249534e-01_rb,8.391988e-01_rb,8.474107e-01_rb,8.526860e-01_rb,8.563983e-01_rb,&
       & 8.592389e-01_rb,8.615144e-01_rb,8.633790e-01_rb,8.649325e-01_rb,8.662504e-01_rb,&
       & 8.673841e-01_rb,8.683741e-01_rb,8.692495e-01_rb,8.700309e-01_rb,8.707328e-01_rb,&
       & 8.713650e-01_rb,8.719432e-01_rb,8.724676e-01_rb,8.729498e-01_rb,8.733922e-01_rb,&
       & 8.737981e-01_rb,8.741745e-01_rb,8.745225e-01_rb,8.748467e-01_rb,8.751512e-01_rb,&
       & 8.754315e-01_rb,8.756962e-01_rb,8.759450e-01_rb,8.761774e-01_rb,8.763945e-01_rb,&
       & 8.766021e-01_rb,8.767970e-01_rb,8.769803e-01_rb,8.771511e-01_rb,8.773151e-01_rb,&
       & 8.774689e-01_rb,8.776147e-01_rb,8.777533e-01_rb,8.778831e-01_rb,8.780050e-01_rb,&
       & 8.781197e-01_rb,8.782301e-01_rb,8.783323e-01_rb,8.784312e-01_rb,8.785222e-01_rb,&
       & 8.786096e-01_rb,8.786916e-01_rb,8.787688e-01_rb,8.788411e-01_rb,8.789122e-01_rb,&
       & 8.789762e-01_rb,8.790373e-01_rb,8.790954e-01_rb,8.791514e-01_rb,8.792018e-01_rb,&
       & 8.792517e-01_rb,8.792990e-01_rb,8.793429e-01_rb /)
!     BAND  26
      asyliq1(:, 26) = (/ &
       & 8.323091e-01_rb,8.429776e-01_rb,8.498123e-01_rb,8.546929e-01_rb,8.584295e-01_rb,&
       & 8.613489e-01_rb,8.636324e-01_rb,8.654303e-01_rb,8.668675e-01_rb,8.680404e-01_rb,&
       & 8.690174e-01_rb,8.698495e-01_rb,8.705666e-01_rb,8.711961e-01_rb,8.717556e-01_rb,&
       & 8.722546e-01_rb,8.727063e-01_rb,8.731170e-01_rb,8.734933e-01_rb,8.738382e-01_rb,&
       & 8.741590e-01_rb,8.744525e-01_rb,8.747295e-01_rb,8.749843e-01_rb,8.752210e-01_rb,&
       & 8.754437e-01_rb,8.756524e-01_rb,8.758472e-01_rb,8.760288e-01_rb,8.762030e-01_rb,&
       & 8.763603e-01_rb,8.765122e-01_rb,8.766539e-01_rb,8.767894e-01_rb,8.769130e-01_rb,&
       & 8.770310e-01_rb,8.771422e-01_rb,8.772437e-01_rb,8.773419e-01_rb,8.774355e-01_rb,&
       & 8.775221e-01_rb,8.776047e-01_rb,8.776802e-01_rb,8.777539e-01_rb,8.778216e-01_rb,&
       & 8.778859e-01_rb,8.779473e-01_rb,8.780031e-01_rb,8.780562e-01_rb,8.781097e-01_rb,&
       & 8.781570e-01_rb,8.782021e-01_rb,8.782463e-01_rb,8.782845e-01_rb,8.783235e-01_rb,&
       & 8.783610e-01_rb,8.783953e-01_rb,8.784273e-01_rb /)
!     BAND  27
      asyliq1(:, 27) = (/ &
       & 8.396448e-01_rb,8.480172e-01_rb,8.535934e-01_rb,8.574145e-01_rb,8.600835e-01_rb,&
       & 8.620347e-01_rb,8.635500e-01_rb,8.648003e-01_rb,8.658758e-01_rb,8.668248e-01_rb,&
       & 8.676697e-01_rb,8.684220e-01_rb,8.690893e-01_rb,8.696807e-01_rb,8.702046e-01_rb,&
       & 8.706676e-01_rb,8.710798e-01_rb,8.714478e-01_rb,8.717778e-01_rb,8.720747e-01_rb,&
       & 8.723431e-01_rb,8.725889e-01_rb,8.728144e-01_rb,8.730201e-01_rb,8.732129e-01_rb,&
       & 8.733907e-01_rb,8.735541e-01_rb,8.737100e-01_rb,8.738533e-01_rb,8.739882e-01_rb,&
       & 8.741164e-01_rb,8.742362e-01_rb,8.743485e-01_rb,8.744530e-01_rb,8.745512e-01_rb,&
       & 8.746471e-01_rb,8.747373e-01_rb,8.748186e-01_rb,8.748973e-01_rb,8.749732e-01_rb,&
       & 8.750443e-01_rb,8.751105e-01_rb,8.751747e-01_rb,8.752344e-01_rb,8.752902e-01_rb,&
       & 8.753412e-01_rb,8.753917e-01_rb,8.754393e-01_rb,8.754843e-01_rb,8.755282e-01_rb,&
       & 8.755662e-01_rb,8.756039e-01_rb,8.756408e-01_rb,8.756722e-01_rb,8.757072e-01_rb,&
       & 8.757352e-01_rb,8.757653e-01_rb,8.757932e-01_rb /)
!     BAND  28
      asyliq1(:, 28) = (/ &
       & 8.374590e-01_rb,8.465669e-01_rb,8.518701e-01_rb,8.547627e-01_rb,8.565745e-01_rb,&
       & 8.579065e-01_rb,8.589717e-01_rb,8.598632e-01_rb,8.606363e-01_rb,8.613268e-01_rb,&
       & 8.619560e-01_rb,8.625340e-01_rb,8.630689e-01_rb,8.635601e-01_rb,8.640084e-01_rb,&
       & 8.644180e-01_rb,8.647885e-01_rb,8.651220e-01_rb,8.654218e-01_rb,8.656908e-01_rb,&
       & 8.659294e-01_rb,8.661422e-01_rb,8.663334e-01_rb,8.665037e-01_rb,8.666543e-01_rb,&
       & 8.667913e-01_rb,8.669156e-01_rb,8.670242e-01_rb,8.671249e-01_rb,8.672161e-01_rb,&
       & 8.672993e-01_rb,8.673733e-01_rb,8.674457e-01_rb,8.675103e-01_rb,8.675713e-01_rb,&
       & 8.676267e-01_rb,8.676798e-01_rb,8.677286e-01_rb,8.677745e-01_rb,8.678178e-01_rb,&
       & 8.678601e-01_rb,8.678986e-01_rb,8.679351e-01_rb,8.679693e-01_rb,8.680013e-01_rb,&
       & 8.680334e-01_rb,8.680624e-01_rb,8.680915e-01_rb,8.681178e-01_rb,8.681428e-01_rb,&
       & 8.681654e-01_rb,8.681899e-01_rb,8.682103e-01_rb,8.682317e-01_rb,8.682498e-01_rb,&
       & 8.682677e-01_rb,8.682861e-01_rb,8.683041e-01_rb /)
!     BAND  29
      asyliq1(:, 29) = (/ &
       & 7.877069e-01_rb,8.244281e-01_rb,8.367971e-01_rb,8.409074e-01_rb,8.429859e-01_rb,&
       & 8.454386e-01_rb,8.489350e-01_rb,8.534141e-01_rb,8.585814e-01_rb,8.641267e-01_rb,&
       & 8.697999e-01_rb,8.754223e-01_rb,8.808785e-01_rb,8.860944e-01_rb,8.910354e-01_rb,&
       & 8.956837e-01_rb,9.000392e-01_rb,9.041091e-01_rb,9.079071e-01_rb,9.114479e-01_rb,&
       & 9.147462e-01_rb,9.178234e-01_rb,9.206903e-01_rb,9.233663e-01_rb,9.258668e-01_rb,&
       & 9.282006e-01_rb,9.303847e-01_rb,9.324288e-01_rb,9.343418e-01_rb,9.361356e-01_rb,&
       & 9.378176e-01_rb,9.393939e-01_rb,9.408736e-01_rb,9.422622e-01_rb,9.435670e-01_rb,&
       & 9.447900e-01_rb,9.459395e-01_rb,9.470199e-01_rb,9.480335e-01_rb,9.489852e-01_rb,&
       & 9.498782e-01_rb,9.507168e-01_rb,9.515044e-01_rb,9.522470e-01_rb,9.529409e-01_rb,&
       & 9.535946e-01_rb,9.542071e-01_rb,9.547838e-01_rb,9.553256e-01_rb,9.558351e-01_rb,&
       & 9.563139e-01_rb,9.567660e-01_rb,9.571915e-01_rb,9.575901e-01_rb,9.579685e-01_rb,&
       & 9.583239e-01_rb,9.586602e-01_rb,9.589766e-01_rb /)


! Spherical Ice Particle Parameterization
! extinction units (ext coef/iwc): [(m^-1)/(g m^-3)]
      extice2(:, 16) = (/ &
! band 16
        & 4.101824e-01_rb,2.435514e-01_rb,1.713697e-01_rb,1.314865e-01_rb,1.063406e-01_rb,&
        & 8.910701e-02_rb,7.659480e-02_rb,6.711784e-02_rb,5.970353e-02_rb,5.375249e-02_rb,&
        & 4.887577e-02_rb,4.481025e-02_rb,4.137171e-02_rb,3.842744e-02_rb,3.587948e-02_rb,&
        & 3.365396e-02_rb,3.169419e-02_rb,2.995593e-02_rb,2.840419e-02_rb,2.701091e-02_rb,&
        & 2.575336e-02_rb,2.461293e-02_rb,2.357423e-02_rb,2.262443e-02_rb,2.175276e-02_rb,&
        & 2.095012e-02_rb,2.020875e-02_rb,1.952199e-02_rb,1.888412e-02_rb,1.829018e-02_rb,&
        & 1.773586e-02_rb,1.721738e-02_rb,1.673144e-02_rb,1.627510e-02_rb,1.584579e-02_rb,&
        & 1.544122e-02_rb,1.505934e-02_rb,1.469833e-02_rb,1.435654e-02_rb,1.403251e-02_rb,&
        & 1.372492e-02_rb,1.343255e-02_rb,1.315433e-02_rb /)
      extice2(:, 17) = (/ &
! band 17
        & 3.836650e-01_rb,2.304055e-01_rb,1.637265e-01_rb,1.266681e-01_rb,1.031602e-01_rb,&
        & 8.695191e-02_rb,7.511544e-02_rb,6.610009e-02_rb,5.900909e-02_rb,5.328833e-02_rb,&
        & 4.857728e-02_rb,4.463133e-02_rb,4.127880e-02_rb,3.839567e-02_rb,3.589013e-02_rb,&
        & 3.369280e-02_rb,3.175027e-02_rb,3.002079e-02_rb,2.847121e-02_rb,2.707493e-02_rb,&
        & 2.581031e-02_rb,2.465962e-02_rb,2.360815e-02_rb,2.264363e-02_rb,2.175571e-02_rb,&
        & 2.093563e-02_rb,2.017592e-02_rb,1.947015e-02_rb,1.881278e-02_rb,1.819901e-02_rb,&
        & 1.762463e-02_rb,1.708598e-02_rb,1.657982e-02_rb,1.610330e-02_rb,1.565390e-02_rb,&
        & 1.522937e-02_rb,1.482768e-02_rb,1.444706e-02_rb,1.408588e-02_rb,1.374270e-02_rb,&
        & 1.341619e-02_rb,1.310517e-02_rb,1.280857e-02_rb /)
      extice2(:, 18) = (/ &
! band 18
        & 4.152673e-01_rb,2.436816e-01_rb,1.702243e-01_rb,1.299704e-01_rb,1.047528e-01_rb,&
        & 8.756039e-02_rb,7.513327e-02_rb,6.575690e-02_rb,5.844616e-02_rb,5.259609e-02_rb,&
        & 4.781531e-02_rb,4.383980e-02_rb,4.048517e-02_rb,3.761891e-02_rb,3.514342e-02_rb,&
        & 3.298525e-02_rb,3.108814e-02_rb,2.940825e-02_rb,2.791096e-02_rb,2.656858e-02_rb,&
        & 2.535869e-02_rb,2.426297e-02_rb,2.326627e-02_rb,2.235602e-02_rb,2.152164e-02_rb,&
        & 2.075420e-02_rb,2.004613e-02_rb,1.939091e-02_rb,1.878296e-02_rb,1.821744e-02_rb,&
        & 1.769015e-02_rb,1.719741e-02_rb,1.673600e-02_rb,1.630308e-02_rb,1.589615e-02_rb,&
        & 1.551298e-02_rb,1.515159e-02_rb,1.481021e-02_rb,1.448726e-02_rb,1.418131e-02_rb,&
        & 1.389109e-02_rb,1.361544e-02_rb,1.335330e-02_rb /)
      extice2(:, 19) = (/ &
! band 19
        & 3.873250e-01_rb,2.331609e-01_rb,1.655002e-01_rb,1.277753e-01_rb,1.038247e-01_rb,&
        & 8.731780e-02_rb,7.527638e-02_rb,6.611873e-02_rb,5.892850e-02_rb,5.313885e-02_rb,&
        & 4.838068e-02_rb,4.440356e-02_rb,4.103167e-02_rb,3.813804e-02_rb,3.562870e-02_rb,&
        & 3.343269e-02_rb,3.149539e-02_rb,2.977414e-02_rb,2.823510e-02_rb,2.685112e-02_rb,&
        & 2.560015e-02_rb,2.446411e-02_rb,2.342805e-02_rb,2.247948e-02_rb,2.160789e-02_rb,&
        & 2.080438e-02_rb,2.006139e-02_rb,1.937238e-02_rb,1.873177e-02_rb,1.813469e-02_rb,&
        & 1.757689e-02_rb,1.705468e-02_rb,1.656479e-02_rb,1.610435e-02_rb,1.567081e-02_rb,&
        & 1.526192e-02_rb,1.487565e-02_rb,1.451020e-02_rb,1.416396e-02_rb,1.383546e-02_rb,&
        & 1.352339e-02_rb,1.322657e-02_rb,1.294392e-02_rb /)
      extice2(:, 20) = (/ &
! band 20
        & 3.784280e-01_rb,2.291396e-01_rb,1.632551e-01_rb,1.263775e-01_rb,1.028944e-01_rb,&
        & 8.666975e-02_rb,7.480952e-02_rb,6.577335e-02_rb,5.866714e-02_rb,5.293694e-02_rb,&
        & 4.822153e-02_rb,4.427547e-02_rb,4.092626e-02_rb,3.804918e-02_rb,3.555184e-02_rb,&
        & 3.336440e-02_rb,3.143307e-02_rb,2.971577e-02_rb,2.817912e-02_rb,2.679632e-02_rb,&
        & 2.554558e-02_rb,2.440903e-02_rb,2.337187e-02_rb,2.242173e-02_rb,2.154821e-02_rb,&
        & 2.074249e-02_rb,1.999706e-02_rb,1.930546e-02_rb,1.866212e-02_rb,1.806221e-02_rb,&
        & 1.750152e-02_rb,1.697637e-02_rb,1.648352e-02_rb,1.602010e-02_rb,1.558358e-02_rb,&
        & 1.517172e-02_rb,1.478250e-02_rb,1.441413e-02_rb,1.406498e-02_rb,1.373362e-02_rb,&
        & 1.341872e-02_rb,1.311911e-02_rb,1.283371e-02_rb /)
      extice2(:, 21) = (/ &
! band 21
        & 3.719909e-01_rb,2.259490e-01_rb,1.613144e-01_rb,1.250648e-01_rb,1.019462e-01_rb,&
        & 8.595358e-02_rb,7.425064e-02_rb,6.532618e-02_rb,5.830218e-02_rb,5.263421e-02_rb,&
        & 4.796697e-02_rb,4.405891e-02_rb,4.074013e-02_rb,3.788776e-02_rb,3.541071e-02_rb,&
        & 3.324008e-02_rb,3.132280e-02_rb,2.961733e-02_rb,2.809071e-02_rb,2.671645e-02_rb,&
        & 2.547302e-02_rb,2.434276e-02_rb,2.331102e-02_rb,2.236558e-02_rb,2.149614e-02_rb,&
        & 2.069397e-02_rb,1.995163e-02_rb,1.926272e-02_rb,1.862174e-02_rb,1.802389e-02_rb,&
        & 1.746500e-02_rb,1.694142e-02_rb,1.644994e-02_rb,1.598772e-02_rb,1.555225e-02_rb,&
        & 1.514129e-02_rb,1.475286e-02_rb,1.438515e-02_rb,1.403659e-02_rb,1.370572e-02_rb,&
        & 1.339124e-02_rb,1.309197e-02_rb,1.280685e-02_rb /)
      extice2(:, 22) = (/ &
! band 22
        & 3.713158e-01_rb,2.253816e-01_rb,1.608461e-01_rb,1.246718e-01_rb,1.016109e-01_rb,&
        & 8.566332e-02_rb,7.399666e-02_rb,6.510199e-02_rb,5.810290e-02_rb,5.245608e-02_rb,&
        & 4.780702e-02_rb,4.391478e-02_rb,4.060989e-02_rb,3.776982e-02_rb,3.530374e-02_rb,&
        & 3.314296e-02_rb,3.123458e-02_rb,2.953719e-02_rb,2.801794e-02_rb,2.665043e-02_rb,&
        & 2.541321e-02_rb,2.428868e-02_rb,2.326224e-02_rb,2.232173e-02_rb,2.145688e-02_rb,&
        & 2.065899e-02_rb,1.992067e-02_rb,1.923552e-02_rb,1.859808e-02_rb,1.800356e-02_rb,&
        & 1.744782e-02_rb,1.692721e-02_rb,1.643855e-02_rb,1.597900e-02_rb,1.554606e-02_rb,&
        & 1.513751e-02_rb,1.475137e-02_rb,1.438586e-02_rb,1.403938e-02_rb,1.371050e-02_rb,&
        & 1.339793e-02_rb,1.310050e-02_rb,1.281713e-02_rb /)
      extice2(:, 23) = (/ &
! band 23
        & 3.605883e-01_rb,2.204388e-01_rb,1.580431e-01_rb,1.229033e-01_rb,1.004203e-01_rb,&
        & 8.482616e-02_rb,7.338941e-02_rb,6.465105e-02_rb,5.776176e-02_rb,5.219398e-02_rb,&
        & 4.760288e-02_rb,4.375369e-02_rb,4.048111e-02_rb,3.766539e-02_rb,3.521771e-02_rb,&
        & 3.307079e-02_rb,3.117277e-02_rb,2.948303e-02_rb,2.796929e-02_rb,2.660560e-02_rb,&
        & 2.537086e-02_rb,2.424772e-02_rb,2.322182e-02_rb,2.228114e-02_rb,2.141556e-02_rb,&
        & 2.061649e-02_rb,1.987661e-02_rb,1.918962e-02_rb,1.855009e-02_rb,1.795330e-02_rb,&
        & 1.739514e-02_rb,1.687199e-02_rb,1.638069e-02_rb,1.591845e-02_rb,1.548276e-02_rb,&
        & 1.507143e-02_rb,1.468249e-02_rb,1.431416e-02_rb,1.396486e-02_rb,1.363318e-02_rb,&
        & 1.331781e-02_rb,1.301759e-02_rb,1.273147e-02_rb /)
      extice2(:, 24) = (/ &
! band 24
        & 3.527890e-01_rb,2.168469e-01_rb,1.560090e-01_rb,1.216216e-01_rb,9.955787e-02_rb,&
        & 8.421942e-02_rb,7.294827e-02_rb,6.432192e-02_rb,5.751081e-02_rb,5.199888e-02_rb,&
        & 4.744835e-02_rb,4.362899e-02_rb,4.037847e-02_rb,3.757910e-02_rb,3.514351e-02_rb,&
        & 3.300546e-02_rb,3.111382e-02_rb,2.942853e-02_rb,2.791775e-02_rb,2.655584e-02_rb,&
        & 2.532195e-02_rb,2.419892e-02_rb,2.317255e-02_rb,2.223092e-02_rb,2.136402e-02_rb,&
        & 2.056334e-02_rb,1.982160e-02_rb,1.913258e-02_rb,1.849087e-02_rb,1.789178e-02_rb,&
        & 1.733124e-02_rb,1.680565e-02_rb,1.631187e-02_rb,1.584711e-02_rb,1.540889e-02_rb,&
        & 1.499502e-02_rb,1.460354e-02_rb,1.423269e-02_rb,1.388088e-02_rb,1.354670e-02_rb,&
        & 1.322887e-02_rb,1.292620e-02_rb,1.263767e-02_rb /)
      extice2(:, 25) = (/ &
! band 25
        & 3.477874e-01_rb,2.143515e-01_rb,1.544887e-01_rb,1.205942e-01_rb,9.881779e-02_rb,&
        & 8.366261e-02_rb,7.251586e-02_rb,6.397790e-02_rb,5.723183e-02_rb,5.176908e-02_rb,&
        & 4.725658e-02_rb,4.346715e-02_rb,4.024055e-02_rb,3.746055e-02_rb,3.504080e-02_rb,&
        & 3.291583e-02_rb,3.103507e-02_rb,2.935891e-02_rb,2.785582e-02_rb,2.650042e-02_rb,&
        & 2.527206e-02_rb,2.415376e-02_rb,2.313142e-02_rb,2.219326e-02_rb,2.132934e-02_rb,&
        & 2.053122e-02_rb,1.979169e-02_rb,1.910456e-02_rb,1.846448e-02_rb,1.786680e-02_rb,&
        & 1.730745e-02_rb,1.678289e-02_rb,1.628998e-02_rb,1.582595e-02_rb,1.538835e-02_rb,&
        & 1.497499e-02_rb,1.458393e-02_rb,1.421341e-02_rb,1.386187e-02_rb,1.352788e-02_rb,&
        & 1.321019e-02_rb,1.290762e-02_rb,1.261913e-02_rb /)
      extice2(:, 26) = (/ &
! band 26
        & 3.453721e-01_rb,2.130744e-01_rb,1.536698e-01_rb,1.200140e-01_rb,9.838078e-02_rb,&
        & 8.331940e-02_rb,7.223803e-02_rb,6.374775e-02_rb,5.703770e-02_rb,5.160290e-02_rb,&
        & 4.711259e-02_rb,4.334110e-02_rb,4.012923e-02_rb,3.736150e-02_rb,3.495208e-02_rb,&
        & 3.283589e-02_rb,3.096267e-02_rb,2.929302e-02_rb,2.779560e-02_rb,2.644517e-02_rb,&
        & 2.522119e-02_rb,2.410677e-02_rb,2.308788e-02_rb,2.215281e-02_rb,2.129165e-02_rb,&
        & 2.049602e-02_rb,1.975874e-02_rb,1.907365e-02_rb,1.843542e-02_rb,1.783943e-02_rb,&
        & 1.728162e-02_rb,1.675847e-02_rb,1.626685e-02_rb,1.580401e-02_rb,1.536750e-02_rb,&
        & 1.495515e-02_rb,1.456502e-02_rb,1.419537e-02_rb,1.384463e-02_rb,1.351139e-02_rb,&
        & 1.319438e-02_rb,1.289246e-02_rb,1.260456e-02_rb /)
      extice2(:, 27) = (/ &
! band 27
        & 3.417883e-01_rb,2.113379e-01_rb,1.526395e-01_rb,1.193347e-01_rb,9.790253e-02_rb,&
        & 8.296715e-02_rb,7.196979e-02_rb,6.353806e-02_rb,5.687024e-02_rb,5.146670e-02_rb,&
        & 4.700001e-02_rb,4.324667e-02_rb,4.004894e-02_rb,3.729233e-02_rb,3.489172e-02_rb,&
        & 3.278257e-02_rb,3.091499e-02_rb,2.924987e-02_rb,2.775609e-02_rb,2.640859e-02_rb,&
        & 2.518695e-02_rb,2.407439e-02_rb,2.305697e-02_rb,2.212303e-02_rb,2.126273e-02_rb,&
        & 2.046774e-02_rb,1.973090e-02_rb,1.904610e-02_rb,1.840801e-02_rb,1.781204e-02_rb,&
        & 1.725417e-02_rb,1.673086e-02_rb,1.623902e-02_rb,1.577590e-02_rb,1.533906e-02_rb,&
        & 1.492634e-02_rb,1.453580e-02_rb,1.416571e-02_rb,1.381450e-02_rb,1.348078e-02_rb,&
        & 1.316327e-02_rb,1.286082e-02_rb,1.257240e-02_rb /)
      extice2(:, 28) = (/ &
! band 28
        & 3.416111e-01_rb,2.114124e-01_rb,1.527734e-01_rb,1.194809e-01_rb,9.804612e-02_rb,&
        & 8.310287e-02_rb,7.209595e-02_rb,6.365442e-02_rb,5.697710e-02_rb,5.156460e-02_rb,&
        & 4.708957e-02_rb,4.332850e-02_rb,4.012361e-02_rb,3.736037e-02_rb,3.495364e-02_rb,&
        & 3.283879e-02_rb,3.096593e-02_rb,2.929589e-02_rb,2.779751e-02_rb,2.644571e-02_rb,&
        & 2.522004e-02_rb,2.410369e-02_rb,2.308271e-02_rb,2.214542e-02_rb,2.128195e-02_rb,&
        & 2.048396e-02_rb,1.974429e-02_rb,1.905679e-02_rb,1.841614e-02_rb,1.781774e-02_rb,&
        & 1.725754e-02_rb,1.673203e-02_rb,1.623807e-02_rb,1.577293e-02_rb,1.533416e-02_rb,&
        & 1.491958e-02_rb,1.452727e-02_rb,1.415547e-02_rb,1.380262e-02_rb,1.346732e-02_rb,&
        & 1.314830e-02_rb,1.284439e-02_rb,1.255456e-02_rb /)
      extice2(:, 29) = (/ &
! band 29
        & 4.196611e-01_rb,2.493642e-01_rb,1.761261e-01_rb,1.357197e-01_rb,1.102161e-01_rb,&
        & 9.269376e-02_rb,7.992985e-02_rb,7.022538e-02_rb,6.260168e-02_rb,5.645603e-02_rb,&
        & 5.139732e-02_rb,4.716088e-02_rb,4.356133e-02_rb,4.046498e-02_rb,3.777303e-02_rb,&
        & 3.541094e-02_rb,3.332137e-02_rb,3.145954e-02_rb,2.978998e-02_rb,2.828419e-02_rb,&
        & 2.691905e-02_rb,2.567559e-02_rb,2.453811e-02_rb,2.349350e-02_rb,2.253072e-02_rb,&
        & 2.164042e-02_rb,2.081464e-02_rb,2.004652e-02_rb,1.933015e-02_rb,1.866041e-02_rb,&
        & 1.803283e-02_rb,1.744348e-02_rb,1.688894e-02_rb,1.636616e-02_rb,1.587244e-02_rb,&
        & 1.540539e-02_rb,1.496287e-02_rb,1.454295e-02_rb,1.414392e-02_rb,1.376423e-02_rb,&
        & 1.340247e-02_rb,1.305739e-02_rb,1.272784e-02_rb /)

! single-scattering albedo: unitless
      ssaice2(:, 16) = (/ &
! band 16
        & 6.630615e-01_rb,6.451169e-01_rb,6.333696e-01_rb,6.246927e-01_rb,6.178420e-01_rb,&
        & 6.121976e-01_rb,6.074069e-01_rb,6.032505e-01_rb,5.995830e-01_rb,5.963030e-01_rb,&
        & 5.933372e-01_rb,5.906311e-01_rb,5.881427e-01_rb,5.858395e-01_rb,5.836955e-01_rb,&
        & 5.816896e-01_rb,5.798046e-01_rb,5.780264e-01_rb,5.763429e-01_rb,5.747441e-01_rb,&
        & 5.732213e-01_rb,5.717672e-01_rb,5.703754e-01_rb,5.690403e-01_rb,5.677571e-01_rb,&
        & 5.665215e-01_rb,5.653297e-01_rb,5.641782e-01_rb,5.630643e-01_rb,5.619850e-01_rb,&
        & 5.609381e-01_rb,5.599214e-01_rb,5.589328e-01_rb,5.579707e-01_rb,5.570333e-01_rb,&
        & 5.561193e-01_rb,5.552272e-01_rb,5.543558e-01_rb,5.535041e-01_rb,5.526708e-01_rb,&
        & 5.518551e-01_rb,5.510561e-01_rb,5.502729e-01_rb /)
      ssaice2(:, 17) = (/ &
! band 17
        & 7.689749e-01_rb,7.398171e-01_rb,7.205819e-01_rb,7.065690e-01_rb,6.956928e-01_rb,&
        & 6.868989e-01_rb,6.795813e-01_rb,6.733606e-01_rb,6.679838e-01_rb,6.632742e-01_rb,&
        & 6.591036e-01_rb,6.553766e-01_rb,6.520197e-01_rb,6.489757e-01_rb,6.461991e-01_rb,&
        & 6.436531e-01_rb,6.413075e-01_rb,6.391375e-01_rb,6.371221e-01_rb,6.352438e-01_rb,&
        & 6.334876e-01_rb,6.318406e-01_rb,6.302918e-01_rb,6.288315e-01_rb,6.274512e-01_rb,&
        & 6.261436e-01_rb,6.249022e-01_rb,6.237211e-01_rb,6.225953e-01_rb,6.215201e-01_rb,&
        & 6.204914e-01_rb,6.195055e-01_rb,6.185592e-01_rb,6.176492e-01_rb,6.167730e-01_rb,&
        & 6.159280e-01_rb,6.151120e-01_rb,6.143228e-01_rb,6.135587e-01_rb,6.128177e-01_rb,&
        & 6.120984e-01_rb,6.113993e-01_rb,6.107189e-01_rb /)
      ssaice2(:, 18) = (/ &
! band 18
        & 9.956167e-01_rb,9.814770e-01_rb,9.716104e-01_rb,9.639746e-01_rb,9.577179e-01_rb,&
        & 9.524010e-01_rb,9.477672e-01_rb,9.436527e-01_rb,9.399467e-01_rb,9.365708e-01_rb,&
        & 9.334672e-01_rb,9.305921e-01_rb,9.279118e-01_rb,9.253993e-01_rb,9.230330e-01_rb,&
        & 9.207954e-01_rb,9.186719e-01_rb,9.166501e-01_rb,9.147199e-01_rb,9.128722e-01_rb,&
        & 9.110997e-01_rb,9.093956e-01_rb,9.077544e-01_rb,9.061708e-01_rb,9.046406e-01_rb,&
        & 9.031598e-01_rb,9.017248e-01_rb,9.003326e-01_rb,8.989804e-01_rb,8.976655e-01_rb,&
        & 8.963857e-01_rb,8.951389e-01_rb,8.939233e-01_rb,8.927370e-01_rb,8.915785e-01_rb,&
        & 8.904464e-01_rb,8.893392e-01_rb,8.882559e-01_rb,8.871951e-01_rb,8.861559e-01_rb,&
        & 8.851373e-01_rb,8.841383e-01_rb,8.831581e-01_rb /)
      ssaice2(:, 19) = (/ &
! band 19
        & 9.723177e-01_rb,9.452119e-01_rb,9.267592e-01_rb,9.127393e-01_rb,9.014238e-01_rb,&
        & 8.919334e-01_rb,8.837584e-01_rb,8.765773e-01_rb,8.701736e-01_rb,8.643950e-01_rb,&
        & 8.591299e-01_rb,8.542942e-01_rb,8.498230e-01_rb,8.456651e-01_rb,8.417794e-01_rb,&
        & 8.381324e-01_rb,8.346964e-01_rb,8.314484e-01_rb,8.283687e-01_rb,8.254408e-01_rb,&
        & 8.226505e-01_rb,8.199854e-01_rb,8.174348e-01_rb,8.149891e-01_rb,8.126403e-01_rb,&
        & 8.103808e-01_rb,8.082041e-01_rb,8.061044e-01_rb,8.040765e-01_rb,8.021156e-01_rb,&
        & 8.002174e-01_rb,7.983781e-01_rb,7.965941e-01_rb,7.948622e-01_rb,7.931795e-01_rb,&
        & 7.915432e-01_rb,7.899508e-01_rb,7.884002e-01_rb,7.868891e-01_rb,7.854156e-01_rb,&
        & 7.839779e-01_rb,7.825742e-01_rb,7.812031e-01_rb /)
      ssaice2(:, 20) = (/ &
! band 20
        & 9.933294e-01_rb,9.860917e-01_rb,9.811564e-01_rb,9.774008e-01_rb,9.743652e-01_rb,&
        & 9.718155e-01_rb,9.696159e-01_rb,9.676810e-01_rb,9.659531e-01_rb,9.643915e-01_rb,&
        & 9.629667e-01_rb,9.616561e-01_rb,9.604426e-01_rb,9.593125e-01_rb,9.582548e-01_rb,&
        & 9.572607e-01_rb,9.563227e-01_rb,9.554347e-01_rb,9.545915e-01_rb,9.537888e-01_rb,&
        & 9.530226e-01_rb,9.522898e-01_rb,9.515874e-01_rb,9.509130e-01_rb,9.502643e-01_rb,&
        & 9.496394e-01_rb,9.490366e-01_rb,9.484542e-01_rb,9.478910e-01_rb,9.473456e-01_rb,&
        & 9.468169e-01_rb,9.463039e-01_rb,9.458056e-01_rb,9.453212e-01_rb,9.448499e-01_rb,&
        & 9.443910e-01_rb,9.439438e-01_rb,9.435077e-01_rb,9.430821e-01_rb,9.426666e-01_rb,&
        & 9.422607e-01_rb,9.418638e-01_rb,9.414756e-01_rb /)
      ssaice2(:, 21) = (/ &
! band 21
        & 9.900787e-01_rb,9.828880e-01_rb,9.779258e-01_rb,9.741173e-01_rb,9.710184e-01_rb,&
        & 9.684012e-01_rb,9.661332e-01_rb,9.641301e-01_rb,9.623352e-01_rb,9.607083e-01_rb,&
        & 9.592198e-01_rb,9.578474e-01_rb,9.565739e-01_rb,9.553856e-01_rb,9.542715e-01_rb,&
        & 9.532226e-01_rb,9.522314e-01_rb,9.512919e-01_rb,9.503986e-01_rb,9.495472e-01_rb,&
        & 9.487337e-01_rb,9.479549e-01_rb,9.472077e-01_rb,9.464897e-01_rb,9.457985e-01_rb,&
        & 9.451322e-01_rb,9.444890e-01_rb,9.438673e-01_rb,9.432656e-01_rb,9.426826e-01_rb,&
        & 9.421173e-01_rb,9.415684e-01_rb,9.410351e-01_rb,9.405164e-01_rb,9.400115e-01_rb,&
        & 9.395198e-01_rb,9.390404e-01_rb,9.385728e-01_rb,9.381164e-01_rb,9.376707e-01_rb,&
        & 9.372350e-01_rb,9.368091e-01_rb,9.363923e-01_rb /)
      ssaice2(:, 22) = (/ &
! band 22
        & 9.986793e-01_rb,9.985239e-01_rb,9.983911e-01_rb,9.982715e-01_rb,9.981606e-01_rb,&
        & 9.980562e-01_rb,9.979567e-01_rb,9.978613e-01_rb,9.977691e-01_rb,9.976798e-01_rb,&
        & 9.975929e-01_rb,9.975081e-01_rb,9.974251e-01_rb,9.973438e-01_rb,9.972640e-01_rb,&
        & 9.971855e-01_rb,9.971083e-01_rb,9.970322e-01_rb,9.969571e-01_rb,9.968830e-01_rb,&
        & 9.968099e-01_rb,9.967375e-01_rb,9.966660e-01_rb,9.965951e-01_rb,9.965250e-01_rb,&
        & 9.964555e-01_rb,9.963867e-01_rb,9.963185e-01_rb,9.962508e-01_rb,9.961836e-01_rb,&
        & 9.961170e-01_rb,9.960508e-01_rb,9.959851e-01_rb,9.959198e-01_rb,9.958550e-01_rb,&
        & 9.957906e-01_rb,9.957266e-01_rb,9.956629e-01_rb,9.955997e-01_rb,9.955367e-01_rb,&
        & 9.954742e-01_rb,9.954119e-01_rb,9.953500e-01_rb /)
      ssaice2(:, 23) = (/ &
! band 23
        & 9.997944e-01_rb,9.997791e-01_rb,9.997664e-01_rb,9.997547e-01_rb,9.997436e-01_rb,&
        & 9.997327e-01_rb,9.997219e-01_rb,9.997110e-01_rb,9.996999e-01_rb,9.996886e-01_rb,&
        & 9.996771e-01_rb,9.996653e-01_rb,9.996533e-01_rb,9.996409e-01_rb,9.996282e-01_rb,&
        & 9.996152e-01_rb,9.996019e-01_rb,9.995883e-01_rb,9.995743e-01_rb,9.995599e-01_rb,&
        & 9.995453e-01_rb,9.995302e-01_rb,9.995149e-01_rb,9.994992e-01_rb,9.994831e-01_rb,&
        & 9.994667e-01_rb,9.994500e-01_rb,9.994329e-01_rb,9.994154e-01_rb,9.993976e-01_rb,&
        & 9.993795e-01_rb,9.993610e-01_rb,9.993422e-01_rb,9.993230e-01_rb,9.993035e-01_rb,&
        & 9.992837e-01_rb,9.992635e-01_rb,9.992429e-01_rb,9.992221e-01_rb,9.992008e-01_rb,&
        & 9.991793e-01_rb,9.991574e-01_rb,9.991352e-01_rb /)
      ssaice2(:, 24) = (/ &
! band 24
        & 9.999949e-01_rb,9.999947e-01_rb,9.999943e-01_rb,9.999939e-01_rb,9.999934e-01_rb,&
        & 9.999927e-01_rb,9.999920e-01_rb,9.999913e-01_rb,9.999904e-01_rb,9.999895e-01_rb,&
        & 9.999885e-01_rb,9.999874e-01_rb,9.999863e-01_rb,9.999851e-01_rb,9.999838e-01_rb,&
        & 9.999824e-01_rb,9.999810e-01_rb,9.999795e-01_rb,9.999780e-01_rb,9.999764e-01_rb,&
        & 9.999747e-01_rb,9.999729e-01_rb,9.999711e-01_rb,9.999692e-01_rb,9.999673e-01_rb,&
        & 9.999653e-01_rb,9.999632e-01_rb,9.999611e-01_rb,9.999589e-01_rb,9.999566e-01_rb,&
        & 9.999543e-01_rb,9.999519e-01_rb,9.999495e-01_rb,9.999470e-01_rb,9.999444e-01_rb,&
        & 9.999418e-01_rb,9.999392e-01_rb,9.999364e-01_rb,9.999336e-01_rb,9.999308e-01_rb,&
        & 9.999279e-01_rb,9.999249e-01_rb,9.999219e-01_rb /)
      ssaice2(:, 25) = (/ &
! band 25
        & 9.999997e-01_rb,9.999997e-01_rb,9.999997e-01_rb,9.999996e-01_rb,9.999996e-01_rb,&
        & 9.999995e-01_rb,9.999994e-01_rb,9.999993e-01_rb,9.999993e-01_rb,9.999992e-01_rb,&
        & 9.999991e-01_rb,9.999989e-01_rb,9.999988e-01_rb,9.999987e-01_rb,9.999986e-01_rb,&
        & 9.999984e-01_rb,9.999983e-01_rb,9.999981e-01_rb,9.999980e-01_rb,9.999978e-01_rb,&
        & 9.999976e-01_rb,9.999974e-01_rb,9.999972e-01_rb,9.999971e-01_rb,9.999969e-01_rb,&
        & 9.999966e-01_rb,9.999964e-01_rb,9.999962e-01_rb,9.999960e-01_rb,9.999957e-01_rb,&
        & 9.999955e-01_rb,9.999953e-01_rb,9.999950e-01_rb,9.999947e-01_rb,9.999945e-01_rb,&
        & 9.999942e-01_rb,9.999939e-01_rb,9.999936e-01_rb,9.999934e-01_rb,9.999931e-01_rb,&
        & 9.999928e-01_rb,9.999925e-01_rb,9.999921e-01_rb /)
      ssaice2(:, 26) = (/ &
! band 26
        & 9.999997e-01_rb,9.999996e-01_rb,9.999996e-01_rb,9.999995e-01_rb,9.999994e-01_rb,&
        & 9.999993e-01_rb,9.999992e-01_rb,9.999991e-01_rb,9.999990e-01_rb,9.999989e-01_rb,&
        & 9.999987e-01_rb,9.999986e-01_rb,9.999984e-01_rb,9.999982e-01_rb,9.999980e-01_rb,&
        & 9.999978e-01_rb,9.999976e-01_rb,9.999974e-01_rb,9.999972e-01_rb,9.999970e-01_rb,&
        & 9.999967e-01_rb,9.999965e-01_rb,9.999962e-01_rb,9.999959e-01_rb,9.999956e-01_rb,&
        & 9.999954e-01_rb,9.999951e-01_rb,9.999947e-01_rb,9.999944e-01_rb,9.999941e-01_rb,&
        & 9.999938e-01_rb,9.999934e-01_rb,9.999931e-01_rb,9.999927e-01_rb,9.999923e-01_rb,&
        & 9.999920e-01_rb,9.999916e-01_rb,9.999912e-01_rb,9.999908e-01_rb,9.999904e-01_rb,&
        & 9.999899e-01_rb,9.999895e-01_rb,9.999891e-01_rb /)
      ssaice2(:, 27) = (/ &
! band 27
        & 9.999987e-01_rb,9.999987e-01_rb,9.999985e-01_rb,9.999984e-01_rb,9.999982e-01_rb,&
        & 9.999980e-01_rb,9.999978e-01_rb,9.999976e-01_rb,9.999973e-01_rb,9.999970e-01_rb,&
        & 9.999967e-01_rb,9.999964e-01_rb,9.999960e-01_rb,9.999956e-01_rb,9.999952e-01_rb,&
        & 9.999948e-01_rb,9.999944e-01_rb,9.999939e-01_rb,9.999934e-01_rb,9.999929e-01_rb,&
        & 9.999924e-01_rb,9.999918e-01_rb,9.999913e-01_rb,9.999907e-01_rb,9.999901e-01_rb,&
        & 9.999894e-01_rb,9.999888e-01_rb,9.999881e-01_rb,9.999874e-01_rb,9.999867e-01_rb,&
        & 9.999860e-01_rb,9.999853e-01_rb,9.999845e-01_rb,9.999837e-01_rb,9.999829e-01_rb,&
        & 9.999821e-01_rb,9.999813e-01_rb,9.999804e-01_rb,9.999796e-01_rb,9.999787e-01_rb,&
        & 9.999778e-01_rb,9.999768e-01_rb,9.999759e-01_rb /)
      ssaice2(:, 28) = (/ &
! band 28
        & 9.999989e-01_rb,9.999989e-01_rb,9.999987e-01_rb,9.999986e-01_rb,9.999984e-01_rb,&
        & 9.999982e-01_rb,9.999980e-01_rb,9.999978e-01_rb,9.999975e-01_rb,9.999972e-01_rb,&
        & 9.999969e-01_rb,9.999966e-01_rb,9.999962e-01_rb,9.999958e-01_rb,9.999954e-01_rb,&
        & 9.999950e-01_rb,9.999945e-01_rb,9.999941e-01_rb,9.999936e-01_rb,9.999931e-01_rb,&
        & 9.999925e-01_rb,9.999920e-01_rb,9.999914e-01_rb,9.999908e-01_rb,9.999902e-01_rb,&
        & 9.999896e-01_rb,9.999889e-01_rb,9.999883e-01_rb,9.999876e-01_rb,9.999869e-01_rb,&
        & 9.999861e-01_rb,9.999854e-01_rb,9.999846e-01_rb,9.999838e-01_rb,9.999830e-01_rb,&
        & 9.999822e-01_rb,9.999814e-01_rb,9.999805e-01_rb,9.999796e-01_rb,9.999787e-01_rb,&
        & 9.999778e-01_rb,9.999769e-01_rb,9.999759e-01_rb /)
      ssaice2(:, 29) = (/ &
! band 29
        & 7.042143e-01_rb,6.691161e-01_rb,6.463240e-01_rb,6.296590e-01_rb,6.166381e-01_rb,&
        & 6.060183e-01_rb,5.970908e-01_rb,5.894144e-01_rb,5.826968e-01_rb,5.767343e-01_rb,&
        & 5.713804e-01_rb,5.665256e-01_rb,5.620867e-01_rb,5.579987e-01_rb,5.542101e-01_rb,&
        & 5.506794e-01_rb,5.473727e-01_rb,5.442620e-01_rb,5.413239e-01_rb,5.385389e-01_rb,&
        & 5.358901e-01_rb,5.333633e-01_rb,5.309460e-01_rb,5.286277e-01_rb,5.263988e-01_rb,&
        & 5.242512e-01_rb,5.221777e-01_rb,5.201719e-01_rb,5.182280e-01_rb,5.163410e-01_rb,&
        & 5.145062e-01_rb,5.127197e-01_rb,5.109776e-01_rb,5.092766e-01_rb,5.076137e-01_rb,&
        & 5.059860e-01_rb,5.043911e-01_rb,5.028266e-01_rb,5.012904e-01_rb,4.997805e-01_rb,&
        & 4.982951e-01_rb,4.968326e-01_rb,4.953913e-01_rb /)

! asymmetry factor: unitless
      asyice2(:, 16) = (/ &
! band 16
        & 7.946655e-01_rb,8.547685e-01_rb,8.806016e-01_rb,8.949880e-01_rb,9.041676e-01_rb,&
        & 9.105399e-01_rb,9.152249e-01_rb,9.188160e-01_rb,9.216573e-01_rb,9.239620e-01_rb,&
        & 9.258695e-01_rb,9.274745e-01_rb,9.288441e-01_rb,9.300267e-01_rb,9.310584e-01_rb,&
        & 9.319665e-01_rb,9.327721e-01_rb,9.334918e-01_rb,9.341387e-01_rb,9.347236e-01_rb,&
        & 9.352551e-01_rb,9.357402e-01_rb,9.361850e-01_rb,9.365942e-01_rb,9.369722e-01_rb,&
        & 9.373225e-01_rb,9.376481e-01_rb,9.379516e-01_rb,9.382352e-01_rb,9.385010e-01_rb,&
        & 9.387505e-01_rb,9.389854e-01_rb,9.392070e-01_rb,9.394163e-01_rb,9.396145e-01_rb,&
        & 9.398024e-01_rb,9.399809e-01_rb,9.401508e-01_rb,9.403126e-01_rb,9.404670e-01_rb,&
        & 9.406144e-01_rb,9.407555e-01_rb,9.408906e-01_rb /)
      asyice2(:, 17) = (/ &
! band 17
        & 9.078091e-01_rb,9.195850e-01_rb,9.267250e-01_rb,9.317083e-01_rb,9.354632e-01_rb,&
        & 9.384323e-01_rb,9.408597e-01_rb,9.428935e-01_rb,9.446301e-01_rb,9.461351e-01_rb,&
        & 9.474555e-01_rb,9.486259e-01_rb,9.496722e-01_rb,9.506146e-01_rb,9.514688e-01_rb,&
        & 9.522476e-01_rb,9.529612e-01_rb,9.536181e-01_rb,9.542251e-01_rb,9.547883e-01_rb,&
        & 9.553124e-01_rb,9.558019e-01_rb,9.562601e-01_rb,9.566904e-01_rb,9.570953e-01_rb,&
        & 9.574773e-01_rb,9.578385e-01_rb,9.581806e-01_rb,9.585054e-01_rb,9.588142e-01_rb,&
        & 9.591083e-01_rb,9.593888e-01_rb,9.596569e-01_rb,9.599135e-01_rb,9.601593e-01_rb,&
        & 9.603952e-01_rb,9.606219e-01_rb,9.608399e-01_rb,9.610499e-01_rb,9.612523e-01_rb,&
        & 9.614477e-01_rb,9.616365e-01_rb,9.618192e-01_rb /)
      asyice2(:, 18) = (/ &
! band 18
        & 8.322045e-01_rb,8.528693e-01_rb,8.648167e-01_rb,8.729163e-01_rb,8.789054e-01_rb,&
        & 8.835845e-01_rb,8.873819e-01_rb,8.905511e-01_rb,8.932532e-01_rb,8.955965e-01_rb,&
        & 8.976567e-01_rb,8.994887e-01_rb,9.011334e-01_rb,9.026221e-01_rb,9.039791e-01_rb,&
        & 9.052237e-01_rb,9.063715e-01_rb,9.074349e-01_rb,9.084245e-01_rb,9.093489e-01_rb,&
        & 9.102154e-01_rb,9.110303e-01_rb,9.117987e-01_rb,9.125253e-01_rb,9.132140e-01_rb,&
        & 9.138682e-01_rb,9.144910e-01_rb,9.150850e-01_rb,9.156524e-01_rb,9.161955e-01_rb,&
        & 9.167160e-01_rb,9.172157e-01_rb,9.176959e-01_rb,9.181581e-01_rb,9.186034e-01_rb,&
        & 9.190330e-01_rb,9.194478e-01_rb,9.198488e-01_rb,9.202368e-01_rb,9.206126e-01_rb,&
        & 9.209768e-01_rb,9.213301e-01_rb,9.216731e-01_rb /)
      asyice2(:, 19) = (/ &
! band 19
        & 8.116560e-01_rb,8.488278e-01_rb,8.674331e-01_rb,8.788148e-01_rb,8.865810e-01_rb,&
        & 8.922595e-01_rb,8.966149e-01_rb,9.000747e-01_rb,9.028980e-01_rb,9.052513e-01_rb,&
        & 9.072468e-01_rb,9.089632e-01_rb,9.104574e-01_rb,9.117713e-01_rb,9.129371e-01_rb,&
        & 9.139793e-01_rb,9.149174e-01_rb,9.157668e-01_rb,9.165400e-01_rb,9.172473e-01_rb,&
        & 9.178970e-01_rb,9.184962e-01_rb,9.190508e-01_rb,9.195658e-01_rb,9.200455e-01_rb,&
        & 9.204935e-01_rb,9.209130e-01_rb,9.213067e-01_rb,9.216771e-01_rb,9.220262e-01_rb,&
        & 9.223560e-01_rb,9.226680e-01_rb,9.229636e-01_rb,9.232443e-01_rb,9.235112e-01_rb,&
        & 9.237652e-01_rb,9.240074e-01_rb,9.242385e-01_rb,9.244594e-01_rb,9.246708e-01_rb,&
        & 9.248733e-01_rb,9.250674e-01_rb,9.252536e-01_rb /)
      asyice2(:, 20) = (/ &
! band 20
        & 8.047113e-01_rb,8.402864e-01_rb,8.570332e-01_rb,8.668455e-01_rb,8.733206e-01_rb,&
        & 8.779272e-01_rb,8.813796e-01_rb,8.840676e-01_rb,8.862225e-01_rb,8.879904e-01_rb,&
        & 8.894682e-01_rb,8.907228e-01_rb,8.918019e-01_rb,8.927404e-01_rb,8.935645e-01_rb,&
        & 8.942943e-01_rb,8.949452e-01_rb,8.955296e-01_rb,8.960574e-01_rb,8.965366e-01_rb,&
        & 8.969736e-01_rb,8.973740e-01_rb,8.977422e-01_rb,8.980820e-01_rb,8.983966e-01_rb,&
        & 8.986889e-01_rb,8.989611e-01_rb,8.992153e-01_rb,8.994533e-01_rb,8.996766e-01_rb,&
        & 8.998865e-01_rb,9.000843e-01_rb,9.002709e-01_rb,9.004474e-01_rb,9.006146e-01_rb,&
        & 9.007731e-01_rb,9.009237e-01_rb,9.010670e-01_rb,9.012034e-01_rb,9.013336e-01_rb,&
        & 9.014579e-01_rb,9.015767e-01_rb,9.016904e-01_rb /)
      asyice2(:, 21) = (/ &
! band 21
        & 8.179122e-01_rb,8.480726e-01_rb,8.621945e-01_rb,8.704354e-01_rb,8.758555e-01_rb,&
        & 8.797007e-01_rb,8.825750e-01_rb,8.848078e-01_rb,8.865939e-01_rb,8.880564e-01_rb,&
        & 8.892765e-01_rb,8.903105e-01_rb,8.911982e-01_rb,8.919689e-01_rb,8.926446e-01_rb,&
        & 8.932419e-01_rb,8.937738e-01_rb,8.942506e-01_rb,8.946806e-01_rb,8.950702e-01_rb,&
        & 8.954251e-01_rb,8.957497e-01_rb,8.960477e-01_rb,8.963223e-01_rb,8.965762e-01_rb,&
        & 8.968116e-01_rb,8.970306e-01_rb,8.972347e-01_rb,8.974255e-01_rb,8.976042e-01_rb,&
        & 8.977720e-01_rb,8.979298e-01_rb,8.980784e-01_rb,8.982188e-01_rb,8.983515e-01_rb,&
        & 8.984771e-01_rb,8.985963e-01_rb,8.987095e-01_rb,8.988171e-01_rb,8.989195e-01_rb,&
        & 8.990172e-01_rb,8.991104e-01_rb,8.991994e-01_rb /)
      asyice2(:, 22) = (/ &
! band 22
        & 8.169789e-01_rb,8.455024e-01_rb,8.586925e-01_rb,8.663283e-01_rb,8.713217e-01_rb,&
        & 8.748488e-01_rb,8.774765e-01_rb,8.795122e-01_rb,8.811370e-01_rb,8.824649e-01_rb,&
        & 8.835711e-01_rb,8.845073e-01_rb,8.853103e-01_rb,8.860068e-01_rb,8.866170e-01_rb,&
        & 8.871560e-01_rb,8.876358e-01_rb,8.880658e-01_rb,8.884533e-01_rb,8.888044e-01_rb,&
        & 8.891242e-01_rb,8.894166e-01_rb,8.896851e-01_rb,8.899324e-01_rb,8.901612e-01_rb,&
        & 8.903733e-01_rb,8.905706e-01_rb,8.907545e-01_rb,8.909265e-01_rb,8.910876e-01_rb,&
        & 8.912388e-01_rb,8.913812e-01_rb,8.915153e-01_rb,8.916419e-01_rb,8.917617e-01_rb,&
        & 8.918752e-01_rb,8.919829e-01_rb,8.920851e-01_rb,8.921824e-01_rb,8.922751e-01_rb,&
        & 8.923635e-01_rb,8.924478e-01_rb,8.925284e-01_rb /)
      asyice2(:, 23) = (/ &
! band 23
        & 8.387642e-01_rb,8.569979e-01_rb,8.658630e-01_rb,8.711825e-01_rb,8.747605e-01_rb,&
        & 8.773472e-01_rb,8.793129e-01_rb,8.808621e-01_rb,8.821179e-01_rb,8.831583e-01_rb,&
        & 8.840361e-01_rb,8.847875e-01_rb,8.854388e-01_rb,8.860094e-01_rb,8.865138e-01_rb,&
        & 8.869634e-01_rb,8.873668e-01_rb,8.877310e-01_rb,8.880617e-01_rb,8.883635e-01_rb,&
        & 8.886401e-01_rb,8.888947e-01_rb,8.891298e-01_rb,8.893477e-01_rb,8.895504e-01_rb,&
        & 8.897393e-01_rb,8.899159e-01_rb,8.900815e-01_rb,8.902370e-01_rb,8.903833e-01_rb,&
        & 8.905214e-01_rb,8.906518e-01_rb,8.907753e-01_rb,8.908924e-01_rb,8.910036e-01_rb,&
        & 8.911094e-01_rb,8.912101e-01_rb,8.913062e-01_rb,8.913979e-01_rb,8.914856e-01_rb,&
        & 8.915695e-01_rb,8.916498e-01_rb,8.917269e-01_rb /)
      asyice2(:, 24) = (/ &
! band 24
        & 8.522208e-01_rb,8.648132e-01_rb,8.711224e-01_rb,8.749901e-01_rb,8.776354e-01_rb,&
        & 8.795743e-01_rb,8.810649e-01_rb,8.822518e-01_rb,8.832225e-01_rb,8.840333e-01_rb,&
        & 8.847224e-01_rb,8.853162e-01_rb,8.858342e-01_rb,8.862906e-01_rb,8.866962e-01_rb,&
        & 8.870595e-01_rb,8.873871e-01_rb,8.876842e-01_rb,8.879551e-01_rb,8.882032e-01_rb,&
        & 8.884316e-01_rb,8.886425e-01_rb,8.888380e-01_rb,8.890199e-01_rb,8.891895e-01_rb,&
        & 8.893481e-01_rb,8.894968e-01_rb,8.896366e-01_rb,8.897683e-01_rb,8.898926e-01_rb,&
        & 8.900102e-01_rb,8.901215e-01_rb,8.902272e-01_rb,8.903276e-01_rb,8.904232e-01_rb,&
        & 8.905144e-01_rb,8.906014e-01_rb,8.906845e-01_rb,8.907640e-01_rb,8.908402e-01_rb,&
        & 8.909132e-01_rb,8.909834e-01_rb,8.910507e-01_rb /)
      asyice2(:, 25) = (/ &
! band 25
        & 8.578202e-01_rb,8.683033e-01_rb,8.735431e-01_rb,8.767488e-01_rb,8.789378e-01_rb,&
        & 8.805399e-01_rb,8.817701e-01_rb,8.827485e-01_rb,8.835480e-01_rb,8.842152e-01_rb,&
        & 8.847817e-01_rb,8.852696e-01_rb,8.856949e-01_rb,8.860694e-01_rb,8.864020e-01_rb,&
        & 8.866997e-01_rb,8.869681e-01_rb,8.872113e-01_rb,8.874330e-01_rb,8.876360e-01_rb,&
        & 8.878227e-01_rb,8.879951e-01_rb,8.881548e-01_rb,8.883033e-01_rb,8.884418e-01_rb,&
        & 8.885712e-01_rb,8.886926e-01_rb,8.888066e-01_rb,8.889139e-01_rb,8.890152e-01_rb,&
        & 8.891110e-01_rb,8.892017e-01_rb,8.892877e-01_rb,8.893695e-01_rb,8.894473e-01_rb,&
        & 8.895214e-01_rb,8.895921e-01_rb,8.896597e-01_rb,8.897243e-01_rb,8.897862e-01_rb,&
        & 8.898456e-01_rb,8.899025e-01_rb,8.899572e-01_rb /)
      asyice2(:, 26) = (/ &
! band 26
        & 8.625615e-01_rb,8.713831e-01_rb,8.755799e-01_rb,8.780560e-01_rb,8.796983e-01_rb,&
        & 8.808714e-01_rb,8.817534e-01_rb,8.824420e-01_rb,8.829953e-01_rb,8.834501e-01_rb,&
        & 8.838310e-01_rb,8.841549e-01_rb,8.844338e-01_rb,8.846767e-01_rb,8.848902e-01_rb,&
        & 8.850795e-01_rb,8.852484e-01_rb,8.854002e-01_rb,8.855374e-01_rb,8.856620e-01_rb,&
        & 8.857758e-01_rb,8.858800e-01_rb,8.859759e-01_rb,8.860644e-01_rb,8.861464e-01_rb,&
        & 8.862225e-01_rb,8.862935e-01_rb,8.863598e-01_rb,8.864218e-01_rb,8.864800e-01_rb,&
        & 8.865347e-01_rb,8.865863e-01_rb,8.866349e-01_rb,8.866809e-01_rb,8.867245e-01_rb,&
        & 8.867658e-01_rb,8.868050e-01_rb,8.868423e-01_rb,8.868778e-01_rb,8.869117e-01_rb,&
        & 8.869440e-01_rb,8.869749e-01_rb,8.870044e-01_rb /)
      asyice2(:, 27) = (/ &
! band 27
        & 8.587495e-01_rb,8.684764e-01_rb,8.728189e-01_rb,8.752872e-01_rb,8.768846e-01_rb,&
        & 8.780060e-01_rb,8.788386e-01_rb,8.794824e-01_rb,8.799960e-01_rb,8.804159e-01_rb,&
        & 8.807660e-01_rb,8.810626e-01_rb,8.813175e-01_rb,8.815390e-01_rb,8.817335e-01_rb,&
        & 8.819057e-01_rb,8.820593e-01_rb,8.821973e-01_rb,8.823220e-01_rb,8.824353e-01_rb,&
        & 8.825387e-01_rb,8.826336e-01_rb,8.827209e-01_rb,8.828016e-01_rb,8.828764e-01_rb,&
        & 8.829459e-01_rb,8.830108e-01_rb,8.830715e-01_rb,8.831283e-01_rb,8.831817e-01_rb,&
        & 8.832320e-01_rb,8.832795e-01_rb,8.833244e-01_rb,8.833668e-01_rb,8.834071e-01_rb,&
        & 8.834454e-01_rb,8.834817e-01_rb,8.835164e-01_rb,8.835495e-01_rb,8.835811e-01_rb,&
        & 8.836113e-01_rb,8.836402e-01_rb,8.836679e-01_rb /)
      asyice2(:, 28) = (/ &
! band 28
        & 8.561110e-01_rb,8.678583e-01_rb,8.727554e-01_rb,8.753892e-01_rb,8.770154e-01_rb,&
        & 8.781109e-01_rb,8.788949e-01_rb,8.794812e-01_rb,8.799348e-01_rb,8.802952e-01_rb,&
        & 8.805880e-01_rb,8.808300e-01_rb,8.810331e-01_rb,8.812058e-01_rb,8.813543e-01_rb,&
        & 8.814832e-01_rb,8.815960e-01_rb,8.816956e-01_rb,8.817839e-01_rb,8.818629e-01_rb,&
        & 8.819339e-01_rb,8.819979e-01_rb,8.820560e-01_rb,8.821089e-01_rb,8.821573e-01_rb,&
        & 8.822016e-01_rb,8.822425e-01_rb,8.822801e-01_rb,8.823150e-01_rb,8.823474e-01_rb,&
        & 8.823775e-01_rb,8.824056e-01_rb,8.824318e-01_rb,8.824564e-01_rb,8.824795e-01_rb,&
        & 8.825011e-01_rb,8.825215e-01_rb,8.825408e-01_rb,8.825589e-01_rb,8.825761e-01_rb,&
        & 8.825924e-01_rb,8.826078e-01_rb,8.826224e-01_rb /)
      asyice2(:, 29) = (/ &
! band 29
        & 8.311124e-01_rb,8.688197e-01_rb,8.900274e-01_rb,9.040696e-01_rb,9.142334e-01_rb,&
        & 9.220181e-01_rb,9.282195e-01_rb,9.333048e-01_rb,9.375689e-01_rb,9.412085e-01_rb,&
        & 9.443604e-01_rb,9.471230e-01_rb,9.495694e-01_rb,9.517549e-01_rb,9.537224e-01_rb,&
        & 9.555057e-01_rb,9.571316e-01_rb,9.586222e-01_rb,9.599952e-01_rb,9.612656e-01_rb,&
        & 9.624458e-01_rb,9.635461e-01_rb,9.645756e-01_rb,9.655418e-01_rb,9.664513e-01_rb,&
        & 9.673098e-01_rb,9.681222e-01_rb,9.688928e-01_rb,9.696256e-01_rb,9.703237e-01_rb,&
        & 9.709903e-01_rb,9.716280e-01_rb,9.722391e-01_rb,9.728258e-01_rb,9.733901e-01_rb,&
        & 9.739336e-01_rb,9.744579e-01_rb,9.749645e-01_rb,9.754546e-01_rb,9.759294e-01_rb,&
        & 9.763901e-01_rb,9.768376e-01_rb,9.772727e-01_rb /)

! Hexagonal Ice Particle Parameterization
! extinction units (ext coef/iwc): [(m^-1)/(g m^-3)]
      extice3(:, 16) = (/ &
! band 16
        & 5.194013e-01_rb,3.215089e-01_rb,2.327917e-01_rb,1.824424e-01_rb,1.499977e-01_rb,&
        & 1.273492e-01_rb,1.106421e-01_rb,9.780982e-02_rb,8.764435e-02_rb,7.939266e-02_rb,&
        & 7.256081e-02_rb,6.681137e-02_rb,6.190600e-02_rb,5.767154e-02_rb,5.397915e-02_rb,&
        & 5.073102e-02_rb,4.785151e-02_rb,4.528125e-02_rb,4.297296e-02_rb,4.088853e-02_rb,&
        & 3.899690e-02_rb,3.727251e-02_rb,3.569411e-02_rb,3.424393e-02_rb,3.290694e-02_rb,&
        & 3.167040e-02_rb,3.052340e-02_rb,2.945654e-02_rb,2.846172e-02_rb,2.753188e-02_rb,&
        & 2.666085e-02_rb,2.584322e-02_rb,2.507423e-02_rb,2.434967e-02_rb,2.366579e-02_rb,&
        & 2.301926e-02_rb,2.240711e-02_rb,2.182666e-02_rb,2.127551e-02_rb,2.075150e-02_rb,&
        & 2.025267e-02_rb,1.977725e-02_rb,1.932364e-02_rb,1.889035e-02_rb,1.847607e-02_rb,&
        & 1.807956e-02_rb /)
      extice3(:, 17) = (/ &
! band 17
        & 4.901155e-01_rb,3.065286e-01_rb,2.230800e-01_rb,1.753951e-01_rb,1.445402e-01_rb,&
        & 1.229417e-01_rb,1.069777e-01_rb,9.469760e-02_rb,8.495824e-02_rb,7.704501e-02_rb,&
        & 7.048834e-02_rb,6.496693e-02_rb,6.025353e-02_rb,5.618286e-02_rb,5.263186e-02_rb,&
        & 4.950698e-02_rb,4.673585e-02_rb,4.426164e-02_rb,4.203904e-02_rb,4.003153e-02_rb,&
        & 3.820932e-02_rb,3.654790e-02_rb,3.502688e-02_rb,3.362919e-02_rb,3.234041e-02_rb,&
        & 3.114829e-02_rb,3.004234e-02_rb,2.901356e-02_rb,2.805413e-02_rb,2.715727e-02_rb,&
        & 2.631705e-02_rb,2.552828e-02_rb,2.478637e-02_rb,2.408725e-02_rb,2.342734e-02_rb,&
        & 2.280343e-02_rb,2.221264e-02_rb,2.165242e-02_rb,2.112043e-02_rb,2.061461e-02_rb,&
        & 2.013308e-02_rb,1.967411e-02_rb,1.923616e-02_rb,1.881783e-02_rb,1.841781e-02_rb,&
        & 1.803494e-02_rb /)
      extice3(:, 18) = (/ &
! band 18
        & 5.056264e-01_rb,3.160261e-01_rb,2.298442e-01_rb,1.805973e-01_rb,1.487318e-01_rb,&
        & 1.264258e-01_rb,1.099389e-01_rb,9.725656e-02_rb,8.719819e-02_rb,7.902576e-02_rb,&
        & 7.225433e-02_rb,6.655206e-02_rb,6.168427e-02_rb,5.748028e-02_rb,5.381296e-02_rb,&
        & 5.058572e-02_rb,4.772383e-02_rb,4.516857e-02_rb,4.287317e-02_rb,4.079990e-02_rb,&
        & 3.891801e-02_rb,3.720217e-02_rb,3.563133e-02_rb,3.418786e-02_rb,3.285686e-02_rb,&
        & 3.162569e-02_rb,3.048352e-02_rb,2.942104e-02_rb,2.843018e-02_rb,2.750395e-02_rb,&
        & 2.663621e-02_rb,2.582160e-02_rb,2.505539e-02_rb,2.433337e-02_rb,2.365185e-02_rb,&
        & 2.300750e-02_rb,2.239736e-02_rb,2.181878e-02_rb,2.126937e-02_rb,2.074699e-02_rb,&
        & 2.024968e-02_rb,1.977567e-02_rb,1.932338e-02_rb,1.889134e-02_rb,1.847823e-02_rb,&
        & 1.808281e-02_rb /)
      extice3(:, 19) = (/ &
! band 19
        & 4.881605e-01_rb,3.055237e-01_rb,2.225070e-01_rb,1.750688e-01_rb,1.443736e-01_rb,&
        & 1.228869e-01_rb,1.070054e-01_rb,9.478893e-02_rb,8.509997e-02_rb,7.722769e-02_rb,&
        & 7.070495e-02_rb,6.521211e-02_rb,6.052311e-02_rb,5.647351e-02_rb,5.294088e-02_rb,&
        & 4.983217e-02_rb,4.707539e-02_rb,4.461398e-02_rb,4.240288e-02_rb,4.040575e-02_rb,&
        & 3.859298e-02_rb,3.694016e-02_rb,3.542701e-02_rb,3.403655e-02_rb,3.275444e-02_rb,&
        & 3.156849e-02_rb,3.046827e-02_rb,2.944481e-02_rb,2.849034e-02_rb,2.759812e-02_rb,&
        & 2.676226e-02_rb,2.597757e-02_rb,2.523949e-02_rb,2.454400e-02_rb,2.388750e-02_rb,&
        & 2.326682e-02_rb,2.267909e-02_rb,2.212176e-02_rb,2.159253e-02_rb,2.108933e-02_rb,&
        & 2.061028e-02_rb,2.015369e-02_rb,1.971801e-02_rb,1.930184e-02_rb,1.890389e-02_rb,&
        & 1.852300e-02_rb /)
      extice3(:, 20) = (/ &
! band 20
        & 5.103703e-01_rb,3.188144e-01_rb,2.317435e-01_rb,1.819887e-01_rb,1.497944e-01_rb,&
        & 1.272584e-01_rb,1.106013e-01_rb,9.778822e-02_rb,8.762610e-02_rb,7.936938e-02_rb,&
        & 7.252809e-02_rb,6.676701e-02_rb,6.184901e-02_rb,5.760165e-02_rb,5.389651e-02_rb,&
        & 5.063598e-02_rb,4.774457e-02_rb,4.516295e-02_rb,4.284387e-02_rb,4.074922e-02_rb,&
        & 3.884792e-02_rb,3.711438e-02_rb,3.552734e-02_rb,3.406898e-02_rb,3.272425e-02_rb,&
        & 3.148038e-02_rb,3.032643e-02_rb,2.925299e-02_rb,2.825191e-02_rb,2.731612e-02_rb,&
        & 2.643943e-02_rb,2.561642e-02_rb,2.484230e-02_rb,2.411284e-02_rb,2.342429e-02_rb,&
        & 2.277329e-02_rb,2.215686e-02_rb,2.157231e-02_rb,2.101724e-02_rb,2.048946e-02_rb,&
        & 1.998702e-02_rb,1.950813e-02_rb,1.905118e-02_rb,1.861468e-02_rb,1.819730e-02_rb,&
        & 1.779781e-02_rb /)
      extice3(:, 21) = (/ &
! band 21
        & 5.031161e-01_rb,3.144511e-01_rb,2.286942e-01_rb,1.796903e-01_rb,1.479819e-01_rb,&
        & 1.257860e-01_rb,1.093803e-01_rb,9.676059e-02_rb,8.675183e-02_rb,7.861971e-02_rb,&
        & 7.188168e-02_rb,6.620754e-02_rb,6.136376e-02_rb,5.718050e-02_rb,5.353127e-02_rb,&
        & 5.031995e-02_rb,4.747218e-02_rb,4.492952e-02_rb,4.264544e-02_rb,4.058240e-02_rb,&
        & 3.870979e-02_rb,3.700242e-02_rb,3.543933e-02_rb,3.400297e-02_rb,3.267854e-02_rb,&
        & 3.145345e-02_rb,3.031691e-02_rb,2.925967e-02_rb,2.827370e-02_rb,2.735203e-02_rb,&
        & 2.648858e-02_rb,2.567798e-02_rb,2.491555e-02_rb,2.419710e-02_rb,2.351893e-02_rb,&
        & 2.287776e-02_rb,2.227063e-02_rb,2.169491e-02_rb,2.114821e-02_rb,2.062840e-02_rb,&
        & 2.013354e-02_rb,1.966188e-02_rb,1.921182e-02_rb,1.878191e-02_rb,1.837083e-02_rb,&
        & 1.797737e-02_rb /)
      extice3(:, 22) = (/ &
! band 22
        & 4.949453e-01_rb,3.095918e-01_rb,2.253402e-01_rb,1.771964e-01_rb,1.460446e-01_rb,&
        & 1.242383e-01_rb,1.081206e-01_rb,9.572235e-02_rb,8.588928e-02_rb,7.789990e-02_rb,&
        & 7.128013e-02_rb,6.570559e-02_rb,6.094684e-02_rb,5.683701e-02_rb,5.325183e-02_rb,&
        & 5.009688e-02_rb,4.729909e-02_rb,4.480106e-02_rb,4.255708e-02_rb,4.053025e-02_rb,&
        & 3.869051e-02_rb,3.701310e-02_rb,3.547745e-02_rb,3.406631e-02_rb,3.276512e-02_rb,&
        & 3.156153e-02_rb,3.044494e-02_rb,2.940626e-02_rb,2.843759e-02_rb,2.753211e-02_rb,&
        & 2.668381e-02_rb,2.588744e-02_rb,2.513839e-02_rb,2.443255e-02_rb,2.376629e-02_rb,&
        & 2.313637e-02_rb,2.253990e-02_rb,2.197428e-02_rb,2.143718e-02_rb,2.092649e-02_rb,&
        & 2.044032e-02_rb,1.997694e-02_rb,1.953478e-02_rb,1.911241e-02_rb,1.870855e-02_rb,&
        & 1.832199e-02_rb /)
      extice3(:, 23) = (/ &
! band 23
        & 5.052816e-01_rb,3.157665e-01_rb,2.296233e-01_rb,1.803986e-01_rb,1.485473e-01_rb,&
        & 1.262514e-01_rb,1.097718e-01_rb,9.709524e-02_rb,8.704139e-02_rb,7.887264e-02_rb,&
        & 7.210424e-02_rb,6.640454e-02_rb,6.153894e-02_rb,5.733683e-02_rb,5.367116e-02_rb,&
        & 5.044537e-02_rb,4.758477e-02_rb,4.503066e-02_rb,4.273629e-02_rb,4.066395e-02_rb,&
        & 3.878291e-02_rb,3.706784e-02_rb,3.549771e-02_rb,3.405488e-02_rb,3.272448e-02_rb,&
        & 3.149387e-02_rb,3.035221e-02_rb,2.929020e-02_rb,2.829979e-02_rb,2.737397e-02_rb,&
        & 2.650663e-02_rb,2.569238e-02_rb,2.492651e-02_rb,2.420482e-02_rb,2.352361e-02_rb,&
        & 2.287954e-02_rb,2.226968e-02_rb,2.169136e-02_rb,2.114220e-02_rb,2.062005e-02_rb,&
        & 2.012296e-02_rb,1.964917e-02_rb,1.919709e-02_rb,1.876524e-02_rb,1.835231e-02_rb,&
        & 1.795707e-02_rb /)
      extice3(:, 24) = (/ &
! band 24
        & 5.042067e-01_rb,3.151195e-01_rb,2.291708e-01_rb,1.800573e-01_rb,1.482779e-01_rb,&
        & 1.260324e-01_rb,1.095900e-01_rb,9.694202e-02_rb,8.691087e-02_rb,7.876056e-02_rb,&
        & 7.200745e-02_rb,6.632062e-02_rb,6.146600e-02_rb,5.727338e-02_rb,5.361599e-02_rb,&
        & 5.039749e-02_rb,4.754334e-02_rb,4.499500e-02_rb,4.270580e-02_rb,4.063815e-02_rb,&
        & 3.876135e-02_rb,3.705016e-02_rb,3.548357e-02_rb,3.404400e-02_rb,3.271661e-02_rb,&
        & 3.148877e-02_rb,3.034969e-02_rb,2.929008e-02_rb,2.830191e-02_rb,2.737818e-02_rb,&
        & 2.651279e-02_rb,2.570039e-02_rb,2.493624e-02_rb,2.421618e-02_rb,2.353650e-02_rb,&
        & 2.289390e-02_rb,2.228541e-02_rb,2.170840e-02_rb,2.116048e-02_rb,2.063950e-02_rb,&
        & 2.014354e-02_rb,1.967082e-02_rb,1.921975e-02_rb,1.878888e-02_rb,1.837688e-02_rb,&
        & 1.798254e-02_rb /)
      extice3(:, 25) = (/ &
! band 25
        & 5.022507e-01_rb,3.139246e-01_rb,2.283218e-01_rb,1.794059e-01_rb,1.477544e-01_rb,&
        & 1.255984e-01_rb,1.092222e-01_rb,9.662516e-02_rb,8.663439e-02_rb,7.851688e-02_rb,&
        & 7.179095e-02_rb,6.612700e-02_rb,6.129193e-02_rb,5.711618e-02_rb,5.347351e-02_rb,&
        & 5.026796e-02_rb,4.742530e-02_rb,4.488721e-02_rb,4.260724e-02_rb,4.054790e-02_rb,&
        & 3.867866e-02_rb,3.697435e-02_rb,3.541407e-02_rb,3.398029e-02_rb,3.265824e-02_rb,&
        & 3.143535e-02_rb,3.030085e-02_rb,2.924551e-02_rb,2.826131e-02_rb,2.734130e-02_rb,&
        & 2.647939e-02_rb,2.567026e-02_rb,2.490919e-02_rb,2.419203e-02_rb,2.351509e-02_rb,&
        & 2.287507e-02_rb,2.226903e-02_rb,2.169434e-02_rb,2.114862e-02_rb,2.062975e-02_rb,&
        & 2.013578e-02_rb,1.966496e-02_rb,1.921571e-02_rb,1.878658e-02_rb,1.837623e-02_rb,&
        & 1.798348e-02_rb /)
      extice3(:, 26) = (/ &
! band 26
        & 5.068316e-01_rb,3.166869e-01_rb,2.302576e-01_rb,1.808693e-01_rb,1.489122e-01_rb,&
        & 1.265423e-01_rb,1.100080e-01_rb,9.728926e-02_rb,8.720201e-02_rb,7.900612e-02_rb,&
        & 7.221524e-02_rb,6.649660e-02_rb,6.161484e-02_rb,5.739877e-02_rb,5.372093e-02_rb,&
        & 5.048442e-02_rb,4.761431e-02_rb,4.505172e-02_rb,4.274972e-02_rb,4.067050e-02_rb,&
        & 3.878321e-02_rb,3.706244e-02_rb,3.548710e-02_rb,3.403948e-02_rb,3.270466e-02_rb,&
        & 3.146995e-02_rb,3.032450e-02_rb,2.925897e-02_rb,2.826527e-02_rb,2.733638e-02_rb,&
        & 2.646615e-02_rb,2.564920e-02_rb,2.488078e-02_rb,2.415670e-02_rb,2.347322e-02_rb,&
        & 2.282702e-02_rb,2.221513e-02_rb,2.163489e-02_rb,2.108390e-02_rb,2.056002e-02_rb,&
        & 2.006128e-02_rb,1.958591e-02_rb,1.913232e-02_rb,1.869904e-02_rb,1.828474e-02_rb,&
        & 1.788819e-02_rb /)
      extice3(:, 27) = (/ &
! band 27
        & 5.077707e-01_rb,3.172636e-01_rb,2.306695e-01_rb,1.811871e-01_rb,1.491691e-01_rb,&
        & 1.267565e-01_rb,1.101907e-01_rb,9.744773e-02_rb,8.734125e-02_rb,7.912973e-02_rb,&
        & 7.232591e-02_rb,6.659637e-02_rb,6.170530e-02_rb,5.748120e-02_rb,5.379634e-02_rb,&
        & 5.055367e-02_rb,4.767809e-02_rb,4.511061e-02_rb,4.280423e-02_rb,4.072104e-02_rb,&
        & 3.883015e-02_rb,3.710611e-02_rb,3.552776e-02_rb,3.407738e-02_rb,3.274002e-02_rb,&
        & 3.150296e-02_rb,3.035532e-02_rb,2.928776e-02_rb,2.829216e-02_rb,2.736150e-02_rb,&
        & 2.648961e-02_rb,2.567111e-02_rb,2.490123e-02_rb,2.417576e-02_rb,2.349098e-02_rb,&
        & 2.284354e-02_rb,2.223049e-02_rb,2.164914e-02_rb,2.109711e-02_rb,2.057222e-02_rb,&
        & 2.007253e-02_rb,1.959626e-02_rb,1.914181e-02_rb,1.870770e-02_rb,1.829261e-02_rb,&
        & 1.789531e-02_rb /)
      extice3(:, 28) = (/ &
! band 28
        & 5.062281e-01_rb,3.163402e-01_rb,2.300275e-01_rb,1.807060e-01_rb,1.487921e-01_rb,&
        & 1.264523e-01_rb,1.099403e-01_rb,9.723879e-02_rb,8.716516e-02_rb,7.898034e-02_rb,&
        & 7.219863e-02_rb,6.648771e-02_rb,6.161254e-02_rb,5.740217e-02_rb,5.372929e-02_rb,&
        & 5.049716e-02_rb,4.763092e-02_rb,4.507179e-02_rb,4.277290e-02_rb,4.069649e-02_rb,&
        & 3.881175e-02_rb,3.709331e-02_rb,3.552008e-02_rb,3.407442e-02_rb,3.274141e-02_rb,&
        & 3.150837e-02_rb,3.036447e-02_rb,2.930037e-02_rb,2.830801e-02_rb,2.738037e-02_rb,&
        & 2.651132e-02_rb,2.569547e-02_rb,2.492810e-02_rb,2.420499e-02_rb,2.352243e-02_rb,&
        & 2.287710e-02_rb,2.226604e-02_rb,2.168658e-02_rb,2.113634e-02_rb,2.061316e-02_rb,&
        & 2.011510e-02_rb,1.964038e-02_rb,1.918740e-02_rb,1.875471e-02_rb,1.834096e-02_rb,&
        & 1.794495e-02_rb /)
      extice3(:, 29) = (/ &
! band 29
        & 1.338834e-01_rb,1.924912e-01_rb,1.755523e-01_rb,1.534793e-01_rb,1.343937e-01_rb,&
        & 1.187883e-01_rb,1.060654e-01_rb,9.559106e-02_rb,8.685880e-02_rb,7.948698e-02_rb,&
        & 7.319086e-02_rb,6.775669e-02_rb,6.302215e-02_rb,5.886236e-02_rb,5.517996e-02_rb,&
        & 5.189810e-02_rb,4.895539e-02_rb,4.630225e-02_rb,4.389823e-02_rb,4.171002e-02_rb,&
        & 3.970998e-02_rb,3.787493e-02_rb,3.618537e-02_rb,3.462471e-02_rb,3.317880e-02_rb,&
        & 3.183547e-02_rb,3.058421e-02_rb,2.941590e-02_rb,2.832256e-02_rb,2.729724e-02_rb,&
        & 2.633377e-02_rb,2.542675e-02_rb,2.457136e-02_rb,2.376332e-02_rb,2.299882e-02_rb,&
        & 2.227443e-02_rb,2.158707e-02_rb,2.093400e-02_rb,2.031270e-02_rb,1.972091e-02_rb,&
        & 1.915659e-02_rb,1.861787e-02_rb,1.810304e-02_rb,1.761055e-02_rb,1.713899e-02_rb,&
        & 1.668704e-02_rb /)

! single-scattering albedo: unitless
      ssaice3(:, 16) = (/ &
! band 16
        & 6.749442e-01_rb,6.649947e-01_rb,6.565828e-01_rb,6.489928e-01_rb,6.420046e-01_rb,&
        & 6.355231e-01_rb,6.294964e-01_rb,6.238901e-01_rb,6.186783e-01_rb,6.138395e-01_rb,&
        & 6.093543e-01_rb,6.052049e-01_rb,6.013742e-01_rb,5.978457e-01_rb,5.946030e-01_rb,&
        & 5.916302e-01_rb,5.889115e-01_rb,5.864310e-01_rb,5.841731e-01_rb,5.821221e-01_rb,&
        & 5.802624e-01_rb,5.785785e-01_rb,5.770549e-01_rb,5.756759e-01_rb,5.744262e-01_rb,&
        & 5.732901e-01_rb,5.722524e-01_rb,5.712974e-01_rb,5.704097e-01_rb,5.695739e-01_rb,&
        & 5.687747e-01_rb,5.679964e-01_rb,5.672238e-01_rb,5.664415e-01_rb,5.656340e-01_rb,&
        & 5.647860e-01_rb,5.638821e-01_rb,5.629070e-01_rb,5.618452e-01_rb,5.606815e-01_rb,&
        & 5.594006e-01_rb,5.579870e-01_rb,5.564255e-01_rb,5.547008e-01_rb,5.527976e-01_rb,&
        & 5.507005e-01_rb /)
      ssaice3(:, 17) = (/ &
! band 17
        & 7.628550e-01_rb,7.567297e-01_rb,7.508463e-01_rb,7.451972e-01_rb,7.397745e-01_rb,&
        & 7.345705e-01_rb,7.295775e-01_rb,7.247881e-01_rb,7.201945e-01_rb,7.157894e-01_rb,&
        & 7.115652e-01_rb,7.075145e-01_rb,7.036300e-01_rb,6.999044e-01_rb,6.963304e-01_rb,&
        & 6.929007e-01_rb,6.896083e-01_rb,6.864460e-01_rb,6.834067e-01_rb,6.804833e-01_rb,&
        & 6.776690e-01_rb,6.749567e-01_rb,6.723397e-01_rb,6.698109e-01_rb,6.673637e-01_rb,&
        & 6.649913e-01_rb,6.626870e-01_rb,6.604441e-01_rb,6.582561e-01_rb,6.561163e-01_rb,&
        & 6.540182e-01_rb,6.519554e-01_rb,6.499215e-01_rb,6.479099e-01_rb,6.459145e-01_rb,&
        & 6.439289e-01_rb,6.419468e-01_rb,6.399621e-01_rb,6.379686e-01_rb,6.359601e-01_rb,&
        & 6.339306e-01_rb,6.318740e-01_rb,6.297845e-01_rb,6.276559e-01_rb,6.254825e-01_rb,&
        & 6.232583e-01_rb /)
      ssaice3(:, 18) = (/ &
! band 18
        & 9.924147e-01_rb,9.882792e-01_rb,9.842257e-01_rb,9.802522e-01_rb,9.763566e-01_rb,&
        & 9.725367e-01_rb,9.687905e-01_rb,9.651157e-01_rb,9.615104e-01_rb,9.579725e-01_rb,&
        & 9.544997e-01_rb,9.510901e-01_rb,9.477416e-01_rb,9.444520e-01_rb,9.412194e-01_rb,&
        & 9.380415e-01_rb,9.349165e-01_rb,9.318421e-01_rb,9.288164e-01_rb,9.258373e-01_rb,&
        & 9.229027e-01_rb,9.200106e-01_rb,9.171589e-01_rb,9.143457e-01_rb,9.115688e-01_rb,&
        & 9.088263e-01_rb,9.061161e-01_rb,9.034362e-01_rb,9.007846e-01_rb,8.981592e-01_rb,&
        & 8.955581e-01_rb,8.929792e-01_rb,8.904206e-01_rb,8.878803e-01_rb,8.853562e-01_rb,&
        & 8.828464e-01_rb,8.803488e-01_rb,8.778616e-01_rb,8.753827e-01_rb,8.729102e-01_rb,&
        & 8.704421e-01_rb,8.679764e-01_rb,8.655112e-01_rb,8.630445e-01_rb,8.605744e-01_rb,&
        & 8.580989e-01_rb /)
      ssaice3(:, 19) = (/ &
! band 19
        & 9.629413e-01_rb,9.517182e-01_rb,9.409209e-01_rb,9.305366e-01_rb,9.205529e-01_rb,&
        & 9.109569e-01_rb,9.017362e-01_rb,8.928780e-01_rb,8.843699e-01_rb,8.761992e-01_rb,&
        & 8.683536e-01_rb,8.608204e-01_rb,8.535873e-01_rb,8.466417e-01_rb,8.399712e-01_rb,&
        & 8.335635e-01_rb,8.274062e-01_rb,8.214868e-01_rb,8.157932e-01_rb,8.103129e-01_rb,&
        & 8.050336e-01_rb,7.999432e-01_rb,7.950294e-01_rb,7.902798e-01_rb,7.856825e-01_rb,&
        & 7.812250e-01_rb,7.768954e-01_rb,7.726815e-01_rb,7.685711e-01_rb,7.645522e-01_rb,&
        & 7.606126e-01_rb,7.567404e-01_rb,7.529234e-01_rb,7.491498e-01_rb,7.454074e-01_rb,&
        & 7.416844e-01_rb,7.379688e-01_rb,7.342485e-01_rb,7.305118e-01_rb,7.267468e-01_rb,&
        & 7.229415e-01_rb,7.190841e-01_rb,7.151628e-01_rb,7.111657e-01_rb,7.070811e-01_rb,&
        & 7.028972e-01_rb /)
      ssaice3(:, 20) = (/ &
! band 20
        & 9.942270e-01_rb,9.909206e-01_rb,9.876775e-01_rb,9.844960e-01_rb,9.813746e-01_rb,&
        & 9.783114e-01_rb,9.753049e-01_rb,9.723535e-01_rb,9.694553e-01_rb,9.666088e-01_rb,&
        & 9.638123e-01_rb,9.610641e-01_rb,9.583626e-01_rb,9.557060e-01_rb,9.530928e-01_rb,&
        & 9.505211e-01_rb,9.479895e-01_rb,9.454961e-01_rb,9.430393e-01_rb,9.406174e-01_rb,&
        & 9.382288e-01_rb,9.358717e-01_rb,9.335446e-01_rb,9.312456e-01_rb,9.289731e-01_rb,&
        & 9.267255e-01_rb,9.245010e-01_rb,9.222980e-01_rb,9.201147e-01_rb,9.179496e-01_rb,&
        & 9.158008e-01_rb,9.136667e-01_rb,9.115457e-01_rb,9.094359e-01_rb,9.073358e-01_rb,&
        & 9.052436e-01_rb,9.031577e-01_rb,9.010763e-01_rb,8.989977e-01_rb,8.969203e-01_rb,&
        & 8.948423e-01_rb,8.927620e-01_rb,8.906778e-01_rb,8.885879e-01_rb,8.864907e-01_rb,&
        & 8.843843e-01_rb /)
      ssaice3(:, 21) = (/ &
! band 21
        & 9.934014e-01_rb,9.899331e-01_rb,9.865537e-01_rb,9.832610e-01_rb,9.800523e-01_rb,&
        & 9.769254e-01_rb,9.738777e-01_rb,9.709069e-01_rb,9.680106e-01_rb,9.651862e-01_rb,&
        & 9.624315e-01_rb,9.597439e-01_rb,9.571212e-01_rb,9.545608e-01_rb,9.520605e-01_rb,&
        & 9.496177e-01_rb,9.472301e-01_rb,9.448954e-01_rb,9.426111e-01_rb,9.403749e-01_rb,&
        & 9.381843e-01_rb,9.360370e-01_rb,9.339307e-01_rb,9.318629e-01_rb,9.298313e-01_rb,&
        & 9.278336e-01_rb,9.258673e-01_rb,9.239302e-01_rb,9.220198e-01_rb,9.201338e-01_rb,&
        & 9.182700e-01_rb,9.164258e-01_rb,9.145991e-01_rb,9.127874e-01_rb,9.109884e-01_rb,&
        & 9.091999e-01_rb,9.074194e-01_rb,9.056447e-01_rb,9.038735e-01_rb,9.021033e-01_rb,&
        & 9.003320e-01_rb,8.985572e-01_rb,8.967766e-01_rb,8.949879e-01_rb,8.931888e-01_rb,&
        & 8.913770e-01_rb /)
      ssaice3(:, 22) = (/ &
! band 22
        & 9.994833e-01_rb,9.992055e-01_rb,9.989278e-01_rb,9.986500e-01_rb,9.983724e-01_rb,&
        & 9.980947e-01_rb,9.978172e-01_rb,9.975397e-01_rb,9.972623e-01_rb,9.969849e-01_rb,&
        & 9.967077e-01_rb,9.964305e-01_rb,9.961535e-01_rb,9.958765e-01_rb,9.955997e-01_rb,&
        & 9.953230e-01_rb,9.950464e-01_rb,9.947699e-01_rb,9.944936e-01_rb,9.942174e-01_rb,&
        & 9.939414e-01_rb,9.936656e-01_rb,9.933899e-01_rb,9.931144e-01_rb,9.928390e-01_rb,&
        & 9.925639e-01_rb,9.922889e-01_rb,9.920141e-01_rb,9.917396e-01_rb,9.914652e-01_rb,&
        & 9.911911e-01_rb,9.909171e-01_rb,9.906434e-01_rb,9.903700e-01_rb,9.900967e-01_rb,&
        & 9.898237e-01_rb,9.895510e-01_rb,9.892784e-01_rb,9.890062e-01_rb,9.887342e-01_rb,&
        & 9.884625e-01_rb,9.881911e-01_rb,9.879199e-01_rb,9.876490e-01_rb,9.873784e-01_rb,&
        & 9.871081e-01_rb /)
      ssaice3(:, 23) = (/ &
! band 23
        & 9.999343e-01_rb,9.998917e-01_rb,9.998492e-01_rb,9.998067e-01_rb,9.997642e-01_rb,&
        & 9.997218e-01_rb,9.996795e-01_rb,9.996372e-01_rb,9.995949e-01_rb,9.995528e-01_rb,&
        & 9.995106e-01_rb,9.994686e-01_rb,9.994265e-01_rb,9.993845e-01_rb,9.993426e-01_rb,&
        & 9.993007e-01_rb,9.992589e-01_rb,9.992171e-01_rb,9.991754e-01_rb,9.991337e-01_rb,&
        & 9.990921e-01_rb,9.990505e-01_rb,9.990089e-01_rb,9.989674e-01_rb,9.989260e-01_rb,&
        & 9.988846e-01_rb,9.988432e-01_rb,9.988019e-01_rb,9.987606e-01_rb,9.987194e-01_rb,&
        & 9.986782e-01_rb,9.986370e-01_rb,9.985959e-01_rb,9.985549e-01_rb,9.985139e-01_rb,&
        & 9.984729e-01_rb,9.984319e-01_rb,9.983910e-01_rb,9.983502e-01_rb,9.983094e-01_rb,&
        & 9.982686e-01_rb,9.982279e-01_rb,9.981872e-01_rb,9.981465e-01_rb,9.981059e-01_rb,&
        & 9.980653e-01_rb /)
      ssaice3(:, 24) = (/ &
! band 24
        & 9.999978e-01_rb,9.999965e-01_rb,9.999952e-01_rb,9.999939e-01_rb,9.999926e-01_rb,&
        & 9.999913e-01_rb,9.999900e-01_rb,9.999887e-01_rb,9.999873e-01_rb,9.999860e-01_rb,&
        & 9.999847e-01_rb,9.999834e-01_rb,9.999821e-01_rb,9.999808e-01_rb,9.999795e-01_rb,&
        & 9.999782e-01_rb,9.999769e-01_rb,9.999756e-01_rb,9.999743e-01_rb,9.999730e-01_rb,&
        & 9.999717e-01_rb,9.999704e-01_rb,9.999691e-01_rb,9.999678e-01_rb,9.999665e-01_rb,&
        & 9.999652e-01_rb,9.999639e-01_rb,9.999626e-01_rb,9.999613e-01_rb,9.999600e-01_rb,&
        & 9.999587e-01_rb,9.999574e-01_rb,9.999561e-01_rb,9.999548e-01_rb,9.999535e-01_rb,&
        & 9.999522e-01_rb,9.999509e-01_rb,9.999496e-01_rb,9.999483e-01_rb,9.999470e-01_rb,&
        & 9.999457e-01_rb,9.999444e-01_rb,9.999431e-01_rb,9.999418e-01_rb,9.999405e-01_rb,&
        & 9.999392e-01_rb /)
      ssaice3(:, 25) = (/ &
! band 25
        & 9.999994e-01_rb,9.999993e-01_rb,9.999991e-01_rb,9.999990e-01_rb,9.999989e-01_rb,&
        & 9.999987e-01_rb,9.999986e-01_rb,9.999984e-01_rb,9.999983e-01_rb,9.999982e-01_rb,&
        & 9.999980e-01_rb,9.999979e-01_rb,9.999977e-01_rb,9.999976e-01_rb,9.999975e-01_rb,&
        & 9.999973e-01_rb,9.999972e-01_rb,9.999970e-01_rb,9.999969e-01_rb,9.999967e-01_rb,&
        & 9.999966e-01_rb,9.999965e-01_rb,9.999963e-01_rb,9.999962e-01_rb,9.999960e-01_rb,&
        & 9.999959e-01_rb,9.999957e-01_rb,9.999956e-01_rb,9.999954e-01_rb,9.999953e-01_rb,&
        & 9.999952e-01_rb,9.999950e-01_rb,9.999949e-01_rb,9.999947e-01_rb,9.999946e-01_rb,&
        & 9.999944e-01_rb,9.999943e-01_rb,9.999941e-01_rb,9.999940e-01_rb,9.999939e-01_rb,&
        & 9.999937e-01_rb,9.999936e-01_rb,9.999934e-01_rb,9.999933e-01_rb,9.999931e-01_rb,&
        & 9.999930e-01_rb /)
      ssaice3(:, 26) = (/ &
! band 26
        & 9.999997e-01_rb,9.999995e-01_rb,9.999992e-01_rb,9.999990e-01_rb,9.999987e-01_rb,&
        & 9.999985e-01_rb,9.999983e-01_rb,9.999980e-01_rb,9.999978e-01_rb,9.999976e-01_rb,&
        & 9.999973e-01_rb,9.999971e-01_rb,9.999969e-01_rb,9.999967e-01_rb,9.999965e-01_rb,&
        & 9.999963e-01_rb,9.999960e-01_rb,9.999958e-01_rb,9.999956e-01_rb,9.999954e-01_rb,&
        & 9.999952e-01_rb,9.999950e-01_rb,9.999948e-01_rb,9.999946e-01_rb,9.999944e-01_rb,&
        & 9.999942e-01_rb,9.999939e-01_rb,9.999937e-01_rb,9.999935e-01_rb,9.999933e-01_rb,&
        & 9.999931e-01_rb,9.999929e-01_rb,9.999927e-01_rb,9.999925e-01_rb,9.999923e-01_rb,&
        & 9.999920e-01_rb,9.999918e-01_rb,9.999916e-01_rb,9.999914e-01_rb,9.999911e-01_rb,&
        & 9.999909e-01_rb,9.999907e-01_rb,9.999905e-01_rb,9.999902e-01_rb,9.999900e-01_rb,&
        & 9.999897e-01_rb /)
      ssaice3(:, 27) = (/ &
! band 27
        & 9.999991e-01_rb,9.999985e-01_rb,9.999980e-01_rb,9.999974e-01_rb,9.999968e-01_rb,&
        & 9.999963e-01_rb,9.999957e-01_rb,9.999951e-01_rb,9.999946e-01_rb,9.999940e-01_rb,&
        & 9.999934e-01_rb,9.999929e-01_rb,9.999923e-01_rb,9.999918e-01_rb,9.999912e-01_rb,&
        & 9.999907e-01_rb,9.999901e-01_rb,9.999896e-01_rb,9.999891e-01_rb,9.999885e-01_rb,&
        & 9.999880e-01_rb,9.999874e-01_rb,9.999869e-01_rb,9.999863e-01_rb,9.999858e-01_rb,&
        & 9.999853e-01_rb,9.999847e-01_rb,9.999842e-01_rb,9.999836e-01_rb,9.999831e-01_rb,&
        & 9.999826e-01_rb,9.999820e-01_rb,9.999815e-01_rb,9.999809e-01_rb,9.999804e-01_rb,&
        & 9.999798e-01_rb,9.999793e-01_rb,9.999787e-01_rb,9.999782e-01_rb,9.999776e-01_rb,&
        & 9.999770e-01_rb,9.999765e-01_rb,9.999759e-01_rb,9.999754e-01_rb,9.999748e-01_rb,&
        & 9.999742e-01_rb /)
      ssaice3(:, 28) = (/ &
! band 28
        & 9.999975e-01_rb,9.999961e-01_rb,9.999946e-01_rb,9.999931e-01_rb,9.999917e-01_rb,&
        & 9.999903e-01_rb,9.999888e-01_rb,9.999874e-01_rb,9.999859e-01_rb,9.999845e-01_rb,&
        & 9.999831e-01_rb,9.999816e-01_rb,9.999802e-01_rb,9.999788e-01_rb,9.999774e-01_rb,&
        & 9.999759e-01_rb,9.999745e-01_rb,9.999731e-01_rb,9.999717e-01_rb,9.999702e-01_rb,&
        & 9.999688e-01_rb,9.999674e-01_rb,9.999660e-01_rb,9.999646e-01_rb,9.999631e-01_rb,&
        & 9.999617e-01_rb,9.999603e-01_rb,9.999589e-01_rb,9.999574e-01_rb,9.999560e-01_rb,&
        & 9.999546e-01_rb,9.999532e-01_rb,9.999517e-01_rb,9.999503e-01_rb,9.999489e-01_rb,&
        & 9.999474e-01_rb,9.999460e-01_rb,9.999446e-01_rb,9.999431e-01_rb,9.999417e-01_rb,&
        & 9.999403e-01_rb,9.999388e-01_rb,9.999374e-01_rb,9.999359e-01_rb,9.999345e-01_rb,&
        & 9.999330e-01_rb /)
      ssaice3(:, 29) = (/ &
! band 29
        & 4.526500e-01_rb,5.287890e-01_rb,5.410487e-01_rb,5.459865e-01_rb,5.485149e-01_rb,&
        & 5.498914e-01_rb,5.505895e-01_rb,5.508310e-01_rb,5.507364e-01_rb,5.503793e-01_rb,&
        & 5.498090e-01_rb,5.490612e-01_rb,5.481637e-01_rb,5.471395e-01_rb,5.460083e-01_rb,&
        & 5.447878e-01_rb,5.434946e-01_rb,5.421442e-01_rb,5.407514e-01_rb,5.393309e-01_rb,&
        & 5.378970e-01_rb,5.364641e-01_rb,5.350464e-01_rb,5.336582e-01_rb,5.323140e-01_rb,&
        & 5.310283e-01_rb,5.298158e-01_rb,5.286914e-01_rb,5.276704e-01_rb,5.267680e-01_rb,&
        & 5.260000e-01_rb,5.253823e-01_rb,5.249311e-01_rb,5.246629e-01_rb,5.245946e-01_rb,&
        & 5.247434e-01_rb,5.251268e-01_rb,5.257626e-01_rb,5.266693e-01_rb,5.278653e-01_rb,&
        & 5.293698e-01_rb,5.312022e-01_rb,5.333823e-01_rb,5.359305e-01_rb,5.388676e-01_rb,&
        & 5.422146e-01_rb /)

! asymmetry factor: unitless
      asyice3(:, 16) = (/ &
! band 16
        & 8.340752e-01_rb,8.435170e-01_rb,8.517487e-01_rb,8.592064e-01_rb,8.660387e-01_rb,&
        & 8.723204e-01_rb,8.780997e-01_rb,8.834137e-01_rb,8.882934e-01_rb,8.927662e-01_rb,&
        & 8.968577e-01_rb,9.005914e-01_rb,9.039899e-01_rb,9.070745e-01_rb,9.098659e-01_rb,&
        & 9.123836e-01_rb,9.146466e-01_rb,9.166734e-01_rb,9.184817e-01_rb,9.200886e-01_rb,&
        & 9.215109e-01_rb,9.227648e-01_rb,9.238661e-01_rb,9.248304e-01_rb,9.256727e-01_rb,&
        & 9.264078e-01_rb,9.270505e-01_rb,9.276150e-01_rb,9.281156e-01_rb,9.285662e-01_rb,&
        & 9.289806e-01_rb,9.293726e-01_rb,9.297557e-01_rb,9.301435e-01_rb,9.305491e-01_rb,&
        & 9.309859e-01_rb,9.314671e-01_rb,9.320055e-01_rb,9.326140e-01_rb,9.333053e-01_rb,&
        & 9.340919e-01_rb,9.349861e-01_rb,9.360000e-01_rb,9.371451e-01_rb,9.384329e-01_rb,&
        & 9.398744e-01_rb /)
      asyice3(:, 17) = (/ &
! band 17
        & 8.728160e-01_rb,8.777333e-01_rb,8.823754e-01_rb,8.867535e-01_rb,8.908785e-01_rb,&
        & 8.947611e-01_rb,8.984118e-01_rb,9.018408e-01_rb,9.050582e-01_rb,9.080739e-01_rb,&
        & 9.108976e-01_rb,9.135388e-01_rb,9.160068e-01_rb,9.183106e-01_rb,9.204595e-01_rb,&
        & 9.224620e-01_rb,9.243271e-01_rb,9.260632e-01_rb,9.276788e-01_rb,9.291822e-01_rb,&
        & 9.305817e-01_rb,9.318853e-01_rb,9.331012e-01_rb,9.342372e-01_rb,9.353013e-01_rb,&
        & 9.363013e-01_rb,9.372450e-01_rb,9.381400e-01_rb,9.389939e-01_rb,9.398145e-01_rb,&
        & 9.406092e-01_rb,9.413856e-01_rb,9.421511e-01_rb,9.429131e-01_rb,9.436790e-01_rb,&
        & 9.444561e-01_rb,9.452517e-01_rb,9.460729e-01_rb,9.469270e-01_rb,9.478209e-01_rb,&
        & 9.487617e-01_rb,9.497562e-01_rb,9.508112e-01_rb,9.519335e-01_rb,9.531294e-01_rb,&
        & 9.544055e-01_rb /)
      asyice3(:, 18) = (/ &
! band 18
        & 7.897566e-01_rb,7.948704e-01_rb,7.998041e-01_rb,8.045623e-01_rb,8.091495e-01_rb,&
        & 8.135702e-01_rb,8.178290e-01_rb,8.219305e-01_rb,8.258790e-01_rb,8.296792e-01_rb,&
        & 8.333355e-01_rb,8.368524e-01_rb,8.402343e-01_rb,8.434856e-01_rb,8.466108e-01_rb,&
        & 8.496143e-01_rb,8.525004e-01_rb,8.552737e-01_rb,8.579384e-01_rb,8.604990e-01_rb,&
        & 8.629597e-01_rb,8.653250e-01_rb,8.675992e-01_rb,8.697867e-01_rb,8.718916e-01_rb,&
        & 8.739185e-01_rb,8.758715e-01_rb,8.777551e-01_rb,8.795734e-01_rb,8.813308e-01_rb,&
        & 8.830315e-01_rb,8.846799e-01_rb,8.862802e-01_rb,8.878366e-01_rb,8.893534e-01_rb,&
        & 8.908350e-01_rb,8.922854e-01_rb,8.937090e-01_rb,8.951099e-01_rb,8.964925e-01_rb,&
        & 8.978609e-01_rb,8.992192e-01_rb,9.005718e-01_rb,9.019229e-01_rb,9.032765e-01_rb,&
        & 9.046369e-01_rb /)
      asyice3(:, 19) = (/ &
! band 19
        & 7.812615e-01_rb,7.887764e-01_rb,7.959664e-01_rb,8.028413e-01_rb,8.094109e-01_rb,&
        & 8.156849e-01_rb,8.216730e-01_rb,8.273846e-01_rb,8.328294e-01_rb,8.380166e-01_rb,&
        & 8.429556e-01_rb,8.476556e-01_rb,8.521258e-01_rb,8.563753e-01_rb,8.604131e-01_rb,&
        & 8.642481e-01_rb,8.678893e-01_rb,8.713455e-01_rb,8.746254e-01_rb,8.777378e-01_rb,&
        & 8.806914e-01_rb,8.834948e-01_rb,8.861566e-01_rb,8.886854e-01_rb,8.910897e-01_rb,&
        & 8.933779e-01_rb,8.955586e-01_rb,8.976402e-01_rb,8.996311e-01_rb,9.015398e-01_rb,&
        & 9.033745e-01_rb,9.051436e-01_rb,9.068555e-01_rb,9.085185e-01_rb,9.101410e-01_rb,&
        & 9.117311e-01_rb,9.132972e-01_rb,9.148476e-01_rb,9.163905e-01_rb,9.179340e-01_rb,&
        & 9.194864e-01_rb,9.210559e-01_rb,9.226505e-01_rb,9.242784e-01_rb,9.259476e-01_rb,&
        & 9.276661e-01_rb /)
      asyice3(:, 20) = (/ &
! band 20
        & 7.640720e-01_rb,7.691119e-01_rb,7.739941e-01_rb,7.787222e-01_rb,7.832998e-01_rb,&
        & 7.877304e-01_rb,7.920177e-01_rb,7.961652e-01_rb,8.001765e-01_rb,8.040551e-01_rb,&
        & 8.078044e-01_rb,8.114280e-01_rb,8.149294e-01_rb,8.183119e-01_rb,8.215791e-01_rb,&
        & 8.247344e-01_rb,8.277812e-01_rb,8.307229e-01_rb,8.335629e-01_rb,8.363046e-01_rb,&
        & 8.389514e-01_rb,8.415067e-01_rb,8.439738e-01_rb,8.463560e-01_rb,8.486568e-01_rb,&
        & 8.508795e-01_rb,8.530274e-01_rb,8.551039e-01_rb,8.571122e-01_rb,8.590558e-01_rb,&
        & 8.609378e-01_rb,8.627618e-01_rb,8.645309e-01_rb,8.662485e-01_rb,8.679178e-01_rb,&
        & 8.695423e-01_rb,8.711251e-01_rb,8.726697e-01_rb,8.741792e-01_rb,8.756571e-01_rb,&
        & 8.771065e-01_rb,8.785307e-01_rb,8.799331e-01_rb,8.813169e-01_rb,8.826854e-01_rb,&
        & 8.840419e-01_rb /)
      asyice3(:, 21) = (/ &
! band 21
        & 7.602598e-01_rb,7.651572e-01_rb,7.699014e-01_rb,7.744962e-01_rb,7.789452e-01_rb,&
        & 7.832522e-01_rb,7.874205e-01_rb,7.914538e-01_rb,7.953555e-01_rb,7.991290e-01_rb,&
        & 8.027777e-01_rb,8.063049e-01_rb,8.097140e-01_rb,8.130081e-01_rb,8.161906e-01_rb,&
        & 8.192645e-01_rb,8.222331e-01_rb,8.250993e-01_rb,8.278664e-01_rb,8.305374e-01_rb,&
        & 8.331153e-01_rb,8.356030e-01_rb,8.380037e-01_rb,8.403201e-01_rb,8.425553e-01_rb,&
        & 8.447121e-01_rb,8.467935e-01_rb,8.488022e-01_rb,8.507412e-01_rb,8.526132e-01_rb,&
        & 8.544210e-01_rb,8.561675e-01_rb,8.578554e-01_rb,8.594875e-01_rb,8.610665e-01_rb,&
        & 8.625951e-01_rb,8.640760e-01_rb,8.655119e-01_rb,8.669055e-01_rb,8.682594e-01_rb,&
        & 8.695763e-01_rb,8.708587e-01_rb,8.721094e-01_rb,8.733308e-01_rb,8.745255e-01_rb,&
        & 8.756961e-01_rb /)
      asyice3(:, 22) = (/ &
! band 22
        & 7.568957e-01_rb,7.606995e-01_rb,7.644072e-01_rb,7.680204e-01_rb,7.715402e-01_rb,&
        & 7.749682e-01_rb,7.783057e-01_rb,7.815541e-01_rb,7.847148e-01_rb,7.877892e-01_rb,&
        & 7.907786e-01_rb,7.936846e-01_rb,7.965084e-01_rb,7.992515e-01_rb,8.019153e-01_rb,&
        & 8.045011e-01_rb,8.070103e-01_rb,8.094444e-01_rb,8.118048e-01_rb,8.140927e-01_rb,&
        & 8.163097e-01_rb,8.184571e-01_rb,8.205364e-01_rb,8.225488e-01_rb,8.244958e-01_rb,&
        & 8.263789e-01_rb,8.281993e-01_rb,8.299586e-01_rb,8.316580e-01_rb,8.332991e-01_rb,&
        & 8.348831e-01_rb,8.364115e-01_rb,8.378857e-01_rb,8.393071e-01_rb,8.406770e-01_rb,&
        & 8.419969e-01_rb,8.432682e-01_rb,8.444923e-01_rb,8.456706e-01_rb,8.468044e-01_rb,&
        & 8.478952e-01_rb,8.489444e-01_rb,8.499533e-01_rb,8.509234e-01_rb,8.518561e-01_rb,&
        & 8.527528e-01_rb /)
      asyice3(:, 23) = (/ &
! band 23
        & 7.575066e-01_rb,7.606912e-01_rb,7.638236e-01_rb,7.669035e-01_rb,7.699306e-01_rb,&
        & 7.729046e-01_rb,7.758254e-01_rb,7.786926e-01_rb,7.815060e-01_rb,7.842654e-01_rb,&
        & 7.869705e-01_rb,7.896211e-01_rb,7.922168e-01_rb,7.947574e-01_rb,7.972428e-01_rb,&
        & 7.996726e-01_rb,8.020466e-01_rb,8.043646e-01_rb,8.066262e-01_rb,8.088313e-01_rb,&
        & 8.109796e-01_rb,8.130709e-01_rb,8.151049e-01_rb,8.170814e-01_rb,8.190001e-01_rb,&
        & 8.208608e-01_rb,8.226632e-01_rb,8.244071e-01_rb,8.260924e-01_rb,8.277186e-01_rb,&
        & 8.292856e-01_rb,8.307932e-01_rb,8.322411e-01_rb,8.336291e-01_rb,8.349570e-01_rb,&
        & 8.362244e-01_rb,8.374312e-01_rb,8.385772e-01_rb,8.396621e-01_rb,8.406856e-01_rb,&
        & 8.416476e-01_rb,8.425479e-01_rb,8.433861e-01_rb,8.441620e-01_rb,8.448755e-01_rb,&
        & 8.455263e-01_rb /)
      asyice3(:, 24) = (/ &
! band 24
        & 7.568829e-01_rb,7.597947e-01_rb,7.626745e-01_rb,7.655212e-01_rb,7.683337e-01_rb,&
        & 7.711111e-01_rb,7.738523e-01_rb,7.765565e-01_rb,7.792225e-01_rb,7.818494e-01_rb,&
        & 7.844362e-01_rb,7.869819e-01_rb,7.894854e-01_rb,7.919459e-01_rb,7.943623e-01_rb,&
        & 7.967337e-01_rb,7.990590e-01_rb,8.013373e-01_rb,8.035676e-01_rb,8.057488e-01_rb,&
        & 8.078802e-01_rb,8.099605e-01_rb,8.119890e-01_rb,8.139645e-01_rb,8.158862e-01_rb,&
        & 8.177530e-01_rb,8.195641e-01_rb,8.213183e-01_rb,8.230149e-01_rb,8.246527e-01_rb,&
        & 8.262308e-01_rb,8.277483e-01_rb,8.292042e-01_rb,8.305976e-01_rb,8.319275e-01_rb,&
        & 8.331929e-01_rb,8.343929e-01_rb,8.355265e-01_rb,8.365928e-01_rb,8.375909e-01_rb,&
        & 8.385197e-01_rb,8.393784e-01_rb,8.401659e-01_rb,8.408815e-01_rb,8.415240e-01_rb,&
        & 8.420926e-01_rb /)
      asyice3(:, 25) = (/ &
! band 25
        & 7.548616e-01_rb,7.575454e-01_rb,7.602153e-01_rb,7.628696e-01_rb,7.655067e-01_rb,&
        & 7.681249e-01_rb,7.707225e-01_rb,7.732978e-01_rb,7.758492e-01_rb,7.783750e-01_rb,&
        & 7.808735e-01_rb,7.833430e-01_rb,7.857819e-01_rb,7.881886e-01_rb,7.905612e-01_rb,&
        & 7.928983e-01_rb,7.951980e-01_rb,7.974588e-01_rb,7.996789e-01_rb,8.018567e-01_rb,&
        & 8.039905e-01_rb,8.060787e-01_rb,8.081196e-01_rb,8.101115e-01_rb,8.120527e-01_rb,&
        & 8.139416e-01_rb,8.157764e-01_rb,8.175557e-01_rb,8.192776e-01_rb,8.209405e-01_rb,&
        & 8.225427e-01_rb,8.240826e-01_rb,8.255585e-01_rb,8.269688e-01_rb,8.283117e-01_rb,&
        & 8.295856e-01_rb,8.307889e-01_rb,8.319198e-01_rb,8.329767e-01_rb,8.339579e-01_rb,&
        & 8.348619e-01_rb,8.356868e-01_rb,8.364311e-01_rb,8.370930e-01_rb,8.376710e-01_rb,&
        & 8.381633e-01_rb /)
      asyice3(:, 26) = (/ &
! band 26
        & 7.491854e-01_rb,7.518523e-01_rb,7.545089e-01_rb,7.571534e-01_rb,7.597839e-01_rb,&
        & 7.623987e-01_rb,7.649959e-01_rb,7.675737e-01_rb,7.701303e-01_rb,7.726639e-01_rb,&
        & 7.751727e-01_rb,7.776548e-01_rb,7.801084e-01_rb,7.825318e-01_rb,7.849230e-01_rb,&
        & 7.872804e-01_rb,7.896020e-01_rb,7.918862e-01_rb,7.941309e-01_rb,7.963345e-01_rb,&
        & 7.984951e-01_rb,8.006109e-01_rb,8.026802e-01_rb,8.047009e-01_rb,8.066715e-01_rb,&
        & 8.085900e-01_rb,8.104546e-01_rb,8.122636e-01_rb,8.140150e-01_rb,8.157072e-01_rb,&
        & 8.173382e-01_rb,8.189063e-01_rb,8.204096e-01_rb,8.218464e-01_rb,8.232148e-01_rb,&
        & 8.245130e-01_rb,8.257391e-01_rb,8.268915e-01_rb,8.279682e-01_rb,8.289675e-01_rb,&
        & 8.298875e-01_rb,8.307264e-01_rb,8.314824e-01_rb,8.321537e-01_rb,8.327385e-01_rb,&
        & 8.332350e-01_rb /)
      asyice3(:, 27) = (/ &
! band 27
        & 7.397086e-01_rb,7.424069e-01_rb,7.450955e-01_rb,7.477725e-01_rb,7.504362e-01_rb,&
        & 7.530846e-01_rb,7.557159e-01_rb,7.583283e-01_rb,7.609199e-01_rb,7.634888e-01_rb,&
        & 7.660332e-01_rb,7.685512e-01_rb,7.710411e-01_rb,7.735009e-01_rb,7.759288e-01_rb,&
        & 7.783229e-01_rb,7.806814e-01_rb,7.830024e-01_rb,7.852841e-01_rb,7.875246e-01_rb,&
        & 7.897221e-01_rb,7.918748e-01_rb,7.939807e-01_rb,7.960380e-01_rb,7.980449e-01_rb,&
        & 7.999995e-01_rb,8.019000e-01_rb,8.037445e-01_rb,8.055311e-01_rb,8.072581e-01_rb,&
        & 8.089235e-01_rb,8.105255e-01_rb,8.120623e-01_rb,8.135319e-01_rb,8.149326e-01_rb,&
        & 8.162626e-01_rb,8.175198e-01_rb,8.187025e-01_rb,8.198089e-01_rb,8.208371e-01_rb,&
        & 8.217852e-01_rb,8.226514e-01_rb,8.234338e-01_rb,8.241306e-01_rb,8.247399e-01_rb,&
        & 8.252599e-01_rb /)
      asyice3(:, 28) = (/ &
! band 28
        & 7.224533e-01_rb,7.251681e-01_rb,7.278728e-01_rb,7.305654e-01_rb,7.332444e-01_rb,&
        & 7.359078e-01_rb,7.385539e-01_rb,7.411808e-01_rb,7.437869e-01_rb,7.463702e-01_rb,&
        & 7.489291e-01_rb,7.514616e-01_rb,7.539661e-01_rb,7.564408e-01_rb,7.588837e-01_rb,&
        & 7.612933e-01_rb,7.636676e-01_rb,7.660049e-01_rb,7.683034e-01_rb,7.705612e-01_rb,&
        & 7.727767e-01_rb,7.749480e-01_rb,7.770733e-01_rb,7.791509e-01_rb,7.811789e-01_rb,&
        & 7.831556e-01_rb,7.850791e-01_rb,7.869478e-01_rb,7.887597e-01_rb,7.905131e-01_rb,&
        & 7.922062e-01_rb,7.938372e-01_rb,7.954044e-01_rb,7.969059e-01_rb,7.983399e-01_rb,&
        & 7.997047e-01_rb,8.009985e-01_rb,8.022195e-01_rb,8.033658e-01_rb,8.044357e-01_rb,&
        & 8.054275e-01_rb,8.063392e-01_rb,8.071692e-01_rb,8.079157e-01_rb,8.085768e-01_rb,&
        & 8.091507e-01_rb /)
      asyice3(:, 29) = (/ &
! band 29
        & 8.850026e-01_rb,9.005489e-01_rb,9.069242e-01_rb,9.121799e-01_rb,9.168987e-01_rb,&
        & 9.212259e-01_rb,9.252176e-01_rb,9.289028e-01_rb,9.323000e-01_rb,9.354235e-01_rb,&
        & 9.382858e-01_rb,9.408985e-01_rb,9.432734e-01_rb,9.454218e-01_rb,9.473557e-01_rb,&
        & 9.490871e-01_rb,9.506282e-01_rb,9.519917e-01_rb,9.531904e-01_rb,9.542374e-01_rb,&
        & 9.551461e-01_rb,9.559298e-01_rb,9.566023e-01_rb,9.571775e-01_rb,9.576692e-01_rb,&
        & 9.580916e-01_rb,9.584589e-01_rb,9.587853e-01_rb,9.590851e-01_rb,9.593729e-01_rb,&
        & 9.596632e-01_rb,9.599705e-01_rb,9.603096e-01_rb,9.606954e-01_rb,9.611427e-01_rb,&
        & 9.616667e-01_rb,9.622826e-01_rb,9.630060e-01_rb,9.638524e-01_rb,9.648379e-01_rb,&
        & 9.659788e-01_rb,9.672916e-01_rb,9.687933e-01_rb,9.705014e-01_rb,9.724337e-01_rb,&
        & 9.746084e-01_rb /)

! fdelta: unitless
      fdlice3(:, 16) = (/ &
! band 16
        & 4.959277e-02_rb,4.685292e-02_rb,4.426104e-02_rb,4.181231e-02_rb,3.950191e-02_rb,&
        & 3.732500e-02_rb,3.527675e-02_rb,3.335235e-02_rb,3.154697e-02_rb,2.985578e-02_rb,&
        & 2.827395e-02_rb,2.679666e-02_rb,2.541909e-02_rb,2.413640e-02_rb,2.294378e-02_rb,&
        & 2.183639e-02_rb,2.080940e-02_rb,1.985801e-02_rb,1.897736e-02_rb,1.816265e-02_rb,&
        & 1.740905e-02_rb,1.671172e-02_rb,1.606585e-02_rb,1.546661e-02_rb,1.490917e-02_rb,&
        & 1.438870e-02_rb,1.390038e-02_rb,1.343939e-02_rb,1.300089e-02_rb,1.258006e-02_rb,&
        & 1.217208e-02_rb,1.177212e-02_rb,1.137536e-02_rb,1.097696e-02_rb,1.057210e-02_rb,&
        & 1.015596e-02_rb,9.723704e-03_rb,9.270516e-03_rb,8.791565e-03_rb,8.282026e-03_rb,&
        & 7.737072e-03_rb,7.151879e-03_rb,6.521619e-03_rb,5.841467e-03_rb,5.106597e-03_rb,&
        & 4.312183e-03_rb /)
      fdlice3(:, 17) = (/ &
! band 17
        & 5.071224e-02_rb,5.000217e-02_rb,4.933872e-02_rb,4.871992e-02_rb,4.814380e-02_rb,&
        & 4.760839e-02_rb,4.711170e-02_rb,4.665177e-02_rb,4.622662e-02_rb,4.583426e-02_rb,&
        & 4.547274e-02_rb,4.514007e-02_rb,4.483428e-02_rb,4.455340e-02_rb,4.429544e-02_rb,&
        & 4.405844e-02_rb,4.384041e-02_rb,4.363939e-02_rb,4.345340e-02_rb,4.328047e-02_rb,&
        & 4.311861e-02_rb,4.296586e-02_rb,4.282024e-02_rb,4.267977e-02_rb,4.254248e-02_rb,&
        & 4.240640e-02_rb,4.226955e-02_rb,4.212995e-02_rb,4.198564e-02_rb,4.183462e-02_rb,&
        & 4.167494e-02_rb,4.150462e-02_rb,4.132167e-02_rb,4.112413e-02_rb,4.091003e-02_rb,&
        & 4.067737e-02_rb,4.042420e-02_rb,4.014854e-02_rb,3.984840e-02_rb,3.952183e-02_rb,&
        & 3.916683e-02_rb,3.878144e-02_rb,3.836368e-02_rb,3.791158e-02_rb,3.742316e-02_rb,&
        & 3.689645e-02_rb /)
      fdlice3(:, 18) = (/ &
! band 18
        & 1.062938e-01_rb,1.065234e-01_rb,1.067822e-01_rb,1.070682e-01_rb,1.073793e-01_rb,&
        & 1.077137e-01_rb,1.080693e-01_rb,1.084442e-01_rb,1.088364e-01_rb,1.092439e-01_rb,&
        & 1.096647e-01_rb,1.100970e-01_rb,1.105387e-01_rb,1.109878e-01_rb,1.114423e-01_rb,&
        & 1.119004e-01_rb,1.123599e-01_rb,1.128190e-01_rb,1.132757e-01_rb,1.137279e-01_rb,&
        & 1.141738e-01_rb,1.146113e-01_rb,1.150385e-01_rb,1.154534e-01_rb,1.158540e-01_rb,&
        & 1.162383e-01_rb,1.166045e-01_rb,1.169504e-01_rb,1.172741e-01_rb,1.175738e-01_rb,&
        & 1.178472e-01_rb,1.180926e-01_rb,1.183080e-01_rb,1.184913e-01_rb,1.186405e-01_rb,&
        & 1.187538e-01_rb,1.188291e-01_rb,1.188645e-01_rb,1.188580e-01_rb,1.188076e-01_rb,&
        & 1.187113e-01_rb,1.185672e-01_rb,1.183733e-01_rb,1.181277e-01_rb,1.178282e-01_rb,&
        & 1.174731e-01_rb /)
      fdlice3(:, 19) = (/ &
! band 19
        & 1.076195e-01_rb,1.065195e-01_rb,1.054696e-01_rb,1.044673e-01_rb,1.035099e-01_rb,&
        & 1.025951e-01_rb,1.017203e-01_rb,1.008831e-01_rb,1.000808e-01_rb,9.931116e-02_rb,&
        & 9.857151e-02_rb,9.785939e-02_rb,9.717230e-02_rb,9.650774e-02_rb,9.586322e-02_rb,&
        & 9.523623e-02_rb,9.462427e-02_rb,9.402484e-02_rb,9.343544e-02_rb,9.285358e-02_rb,&
        & 9.227675e-02_rb,9.170245e-02_rb,9.112818e-02_rb,9.055144e-02_rb,8.996974e-02_rb,&
        & 8.938056e-02_rb,8.878142e-02_rb,8.816981e-02_rb,8.754323e-02_rb,8.689919e-02_rb,&
        & 8.623517e-02_rb,8.554869e-02_rb,8.483724e-02_rb,8.409832e-02_rb,8.332943e-02_rb,&
        & 8.252807e-02_rb,8.169175e-02_rb,8.081795e-02_rb,7.990419e-02_rb,7.894796e-02_rb,&
        & 7.794676e-02_rb,7.689809e-02_rb,7.579945e-02_rb,7.464834e-02_rb,7.344227e-02_rb,&
        & 7.217872e-02_rb /)
      fdlice3(:, 20) = (/ &
! band 20
        & 1.119014e-01_rb,1.122706e-01_rb,1.126690e-01_rb,1.130947e-01_rb,1.135456e-01_rb,&
        & 1.140199e-01_rb,1.145154e-01_rb,1.150302e-01_rb,1.155623e-01_rb,1.161096e-01_rb,&
        & 1.166703e-01_rb,1.172422e-01_rb,1.178233e-01_rb,1.184118e-01_rb,1.190055e-01_rb,&
        & 1.196025e-01_rb,1.202008e-01_rb,1.207983e-01_rb,1.213931e-01_rb,1.219832e-01_rb,&
        & 1.225665e-01_rb,1.231411e-01_rb,1.237050e-01_rb,1.242561e-01_rb,1.247926e-01_rb,&
        & 1.253122e-01_rb,1.258132e-01_rb,1.262934e-01_rb,1.267509e-01_rb,1.271836e-01_rb,&
        & 1.275896e-01_rb,1.279669e-01_rb,1.283134e-01_rb,1.286272e-01_rb,1.289063e-01_rb,&
        & 1.291486e-01_rb,1.293522e-01_rb,1.295150e-01_rb,1.296351e-01_rb,1.297104e-01_rb,&
        & 1.297390e-01_rb,1.297189e-01_rb,1.296480e-01_rb,1.295244e-01_rb,1.293460e-01_rb,&
        & 1.291109e-01_rb /)
      fdlice3(:, 21) = (/ &
! band 21
        & 1.133298e-01_rb,1.136777e-01_rb,1.140556e-01_rb,1.144615e-01_rb,1.148934e-01_rb,&
        & 1.153492e-01_rb,1.158269e-01_rb,1.163243e-01_rb,1.168396e-01_rb,1.173706e-01_rb,&
        & 1.179152e-01_rb,1.184715e-01_rb,1.190374e-01_rb,1.196108e-01_rb,1.201897e-01_rb,&
        & 1.207720e-01_rb,1.213558e-01_rb,1.219389e-01_rb,1.225194e-01_rb,1.230951e-01_rb,&
        & 1.236640e-01_rb,1.242241e-01_rb,1.247733e-01_rb,1.253096e-01_rb,1.258309e-01_rb,&
        & 1.263352e-01_rb,1.268205e-01_rb,1.272847e-01_rb,1.277257e-01_rb,1.281415e-01_rb,&
        & 1.285300e-01_rb,1.288893e-01_rb,1.292173e-01_rb,1.295118e-01_rb,1.297710e-01_rb,&
        & 1.299927e-01_rb,1.301748e-01_rb,1.303154e-01_rb,1.304124e-01_rb,1.304637e-01_rb,&
        & 1.304673e-01_rb,1.304212e-01_rb,1.303233e-01_rb,1.301715e-01_rb,1.299638e-01_rb,&
        & 1.296983e-01_rb /)
      fdlice3(:, 22) = (/ &
! band 22
        & 1.145360e-01_rb,1.153256e-01_rb,1.161453e-01_rb,1.169929e-01_rb,1.178666e-01_rb,&
        & 1.187641e-01_rb,1.196835e-01_rb,1.206227e-01_rb,1.215796e-01_rb,1.225522e-01_rb,&
        & 1.235383e-01_rb,1.245361e-01_rb,1.255433e-01_rb,1.265579e-01_rb,1.275779e-01_rb,&
        & 1.286011e-01_rb,1.296257e-01_rb,1.306494e-01_rb,1.316703e-01_rb,1.326862e-01_rb,&
        & 1.336951e-01_rb,1.346950e-01_rb,1.356838e-01_rb,1.366594e-01_rb,1.376198e-01_rb,&
        & 1.385629e-01_rb,1.394866e-01_rb,1.403889e-01_rb,1.412678e-01_rb,1.421212e-01_rb,&
        & 1.429469e-01_rb,1.437430e-01_rb,1.445074e-01_rb,1.452381e-01_rb,1.459329e-01_rb,&
        & 1.465899e-01_rb,1.472069e-01_rb,1.477819e-01_rb,1.483128e-01_rb,1.487976e-01_rb,&
        & 1.492343e-01_rb,1.496207e-01_rb,1.499548e-01_rb,1.502346e-01_rb,1.504579e-01_rb,&
        & 1.506227e-01_rb /)
      fdlice3(:, 23) = (/ &
! band 23
        & 1.153263e-01_rb,1.161445e-01_rb,1.169932e-01_rb,1.178703e-01_rb,1.187738e-01_rb,&
        & 1.197016e-01_rb,1.206516e-01_rb,1.216217e-01_rb,1.226099e-01_rb,1.236141e-01_rb,&
        & 1.246322e-01_rb,1.256621e-01_rb,1.267017e-01_rb,1.277491e-01_rb,1.288020e-01_rb,&
        & 1.298584e-01_rb,1.309163e-01_rb,1.319736e-01_rb,1.330281e-01_rb,1.340778e-01_rb,&
        & 1.351207e-01_rb,1.361546e-01_rb,1.371775e-01_rb,1.381873e-01_rb,1.391820e-01_rb,&
        & 1.401593e-01_rb,1.411174e-01_rb,1.420540e-01_rb,1.429671e-01_rb,1.438547e-01_rb,&
        & 1.447146e-01_rb,1.455449e-01_rb,1.463433e-01_rb,1.471078e-01_rb,1.478364e-01_rb,&
        & 1.485270e-01_rb,1.491774e-01_rb,1.497857e-01_rb,1.503497e-01_rb,1.508674e-01_rb,&
        & 1.513367e-01_rb,1.517554e-01_rb,1.521216e-01_rb,1.524332e-01_rb,1.526880e-01_rb,&
        & 1.528840e-01_rb /)
      fdlice3(:, 24) = (/ &
! band 24
        & 1.160842e-01_rb,1.169118e-01_rb,1.177697e-01_rb,1.186556e-01_rb,1.195676e-01_rb,&
        & 1.205036e-01_rb,1.214616e-01_rb,1.224394e-01_rb,1.234349e-01_rb,1.244463e-01_rb,&
        & 1.254712e-01_rb,1.265078e-01_rb,1.275539e-01_rb,1.286075e-01_rb,1.296664e-01_rb,&
        & 1.307287e-01_rb,1.317923e-01_rb,1.328550e-01_rb,1.339149e-01_rb,1.349699e-01_rb,&
        & 1.360179e-01_rb,1.370567e-01_rb,1.380845e-01_rb,1.390991e-01_rb,1.400984e-01_rb,&
        & 1.410803e-01_rb,1.420429e-01_rb,1.429840e-01_rb,1.439016e-01_rb,1.447936e-01_rb,&
        & 1.456579e-01_rb,1.464925e-01_rb,1.472953e-01_rb,1.480642e-01_rb,1.487972e-01_rb,&
        & 1.494923e-01_rb,1.501472e-01_rb,1.507601e-01_rb,1.513287e-01_rb,1.518511e-01_rb,&
        & 1.523252e-01_rb,1.527489e-01_rb,1.531201e-01_rb,1.534368e-01_rb,1.536969e-01_rb,&
        & 1.538984e-01_rb /)
      fdlice3(:, 25) = (/ &
! band 25
        & 1.168725e-01_rb,1.177088e-01_rb,1.185747e-01_rb,1.194680e-01_rb,1.203867e-01_rb,&
        & 1.213288e-01_rb,1.222923e-01_rb,1.232750e-01_rb,1.242750e-01_rb,1.252903e-01_rb,&
        & 1.263187e-01_rb,1.273583e-01_rb,1.284069e-01_rb,1.294626e-01_rb,1.305233e-01_rb,&
        & 1.315870e-01_rb,1.326517e-01_rb,1.337152e-01_rb,1.347756e-01_rb,1.358308e-01_rb,&
        & 1.368788e-01_rb,1.379175e-01_rb,1.389449e-01_rb,1.399590e-01_rb,1.409577e-01_rb,&
        & 1.419389e-01_rb,1.429007e-01_rb,1.438410e-01_rb,1.447577e-01_rb,1.456488e-01_rb,&
        & 1.465123e-01_rb,1.473461e-01_rb,1.481483e-01_rb,1.489166e-01_rb,1.496492e-01_rb,&
        & 1.503439e-01_rb,1.509988e-01_rb,1.516118e-01_rb,1.521808e-01_rb,1.527038e-01_rb,&
        & 1.531788e-01_rb,1.536037e-01_rb,1.539764e-01_rb,1.542951e-01_rb,1.545575e-01_rb,&
        & 1.547617e-01_rb /)
      fdlice3(:, 26) = (/ &
!band 26
        & 1.180509e-01_rb,1.189025e-01_rb,1.197820e-01_rb,1.206875e-01_rb,1.216171e-01_rb,&
        & 1.225687e-01_rb,1.235404e-01_rb,1.245303e-01_rb,1.255363e-01_rb,1.265564e-01_rb,&
        & 1.275888e-01_rb,1.286313e-01_rb,1.296821e-01_rb,1.307392e-01_rb,1.318006e-01_rb,&
        & 1.328643e-01_rb,1.339284e-01_rb,1.349908e-01_rb,1.360497e-01_rb,1.371029e-01_rb,&
        & 1.381486e-01_rb,1.391848e-01_rb,1.402095e-01_rb,1.412208e-01_rb,1.422165e-01_rb,&
        & 1.431949e-01_rb,1.441539e-01_rb,1.450915e-01_rb,1.460058e-01_rb,1.468947e-01_rb,&
        & 1.477564e-01_rb,1.485888e-01_rb,1.493900e-01_rb,1.501580e-01_rb,1.508907e-01_rb,&
        & 1.515864e-01_rb,1.522428e-01_rb,1.528582e-01_rb,1.534305e-01_rb,1.539578e-01_rb,&
        & 1.544380e-01_rb,1.548692e-01_rb,1.552494e-01_rb,1.555767e-01_rb,1.558490e-01_rb,&
        & 1.560645e-01_rb /)
      fdlice3(:, 27) = (/ &
! band 27
        & 1.200480e-01_rb,1.209267e-01_rb,1.218304e-01_rb,1.227575e-01_rb,1.237059e-01_rb,&
        & 1.246739e-01_rb,1.256595e-01_rb,1.266610e-01_rb,1.276765e-01_rb,1.287041e-01_rb,&
        & 1.297420e-01_rb,1.307883e-01_rb,1.318412e-01_rb,1.328988e-01_rb,1.339593e-01_rb,&
        & 1.350207e-01_rb,1.360813e-01_rb,1.371393e-01_rb,1.381926e-01_rb,1.392396e-01_rb,&
        & 1.402783e-01_rb,1.413069e-01_rb,1.423235e-01_rb,1.433263e-01_rb,1.443134e-01_rb,&
        & 1.452830e-01_rb,1.462332e-01_rb,1.471622e-01_rb,1.480681e-01_rb,1.489490e-01_rb,&
        & 1.498032e-01_rb,1.506286e-01_rb,1.514236e-01_rb,1.521863e-01_rb,1.529147e-01_rb,&
        & 1.536070e-01_rb,1.542614e-01_rb,1.548761e-01_rb,1.554491e-01_rb,1.559787e-01_rb,&
        & 1.564629e-01_rb,1.568999e-01_rb,1.572879e-01_rb,1.576249e-01_rb,1.579093e-01_rb,&
        & 1.581390e-01_rb /)
      fdlice3(:, 28) = (/ &
! band 28
        & 1.247813e-01_rb,1.256496e-01_rb,1.265417e-01_rb,1.274560e-01_rb,1.283905e-01_rb,&
        & 1.293436e-01_rb,1.303135e-01_rb,1.312983e-01_rb,1.322964e-01_rb,1.333060e-01_rb,&
        & 1.343252e-01_rb,1.353523e-01_rb,1.363855e-01_rb,1.374231e-01_rb,1.384632e-01_rb,&
        & 1.395042e-01_rb,1.405441e-01_rb,1.415813e-01_rb,1.426140e-01_rb,1.436404e-01_rb,&
        & 1.446587e-01_rb,1.456672e-01_rb,1.466640e-01_rb,1.476475e-01_rb,1.486157e-01_rb,&
        & 1.495671e-01_rb,1.504997e-01_rb,1.514117e-01_rb,1.523016e-01_rb,1.531673e-01_rb,&
        & 1.540073e-01_rb,1.548197e-01_rb,1.556026e-01_rb,1.563545e-01_rb,1.570734e-01_rb,&
        & 1.577576e-01_rb,1.584054e-01_rb,1.590149e-01_rb,1.595843e-01_rb,1.601120e-01_rb,&
        & 1.605962e-01_rb,1.610349e-01_rb,1.614266e-01_rb,1.617693e-01_rb,1.620614e-01_rb,&
        & 1.623011e-01_rb /)
      fdlice3(:, 29) = (/ &
! band 29
        & 1.006055e-01_rb,9.549582e-02_rb,9.063960e-02_rb,8.602900e-02_rb,8.165612e-02_rb,&
        & 7.751308e-02_rb,7.359199e-02_rb,6.988496e-02_rb,6.638412e-02_rb,6.308156e-02_rb,&
        & 5.996942e-02_rb,5.703979e-02_rb,5.428481e-02_rb,5.169657e-02_rb,4.926719e-02_rb,&
        & 4.698880e-02_rb,4.485349e-02_rb,4.285339e-02_rb,4.098061e-02_rb,3.922727e-02_rb,&
        & 3.758547e-02_rb,3.604733e-02_rb,3.460497e-02_rb,3.325051e-02_rb,3.197604e-02_rb,&
        & 3.077369e-02_rb,2.963558e-02_rb,2.855381e-02_rb,2.752050e-02_rb,2.652776e-02_rb,&
        & 2.556772e-02_rb,2.463247e-02_rb,2.371415e-02_rb,2.280485e-02_rb,2.189670e-02_rb,&
        & 2.098180e-02_rb,2.005228e-02_rb,1.910024e-02_rb,1.811781e-02_rb,1.709709e-02_rb,&
        & 1.603020e-02_rb,1.490925e-02_rb,1.372635e-02_rb,1.247363e-02_rb,1.114319e-02_rb,&
        & 9.727157e-03_rb /)

      end subroutine swcldpr

      end module rrtmg_sw_init

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
      module rrtmg_sw_vrtqdr

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

      use parkind, only: im => kind_im, rb => kind_rb
!      use parrrsw, only: ngptsw

      implicit none

      contains

! --------------------------------------------------------------------------
      subroutine vrtqdr_sw(klev, kw, &
                           pref, prefd, ptra, ptrad, &
                           pdbt, prdnd, prup, prupd, ptdbt, &
                           pfd, pfu)
! --------------------------------------------------------------------------
 
! Purpose: This routine performs the vertical quadrature integration
!
! Interface:  *vrtqdr_sw* is called from *spcvrt_sw* and *spcvmc_sw*
!
! Modifications.
! 
! Original: H. Barker
! Revision: Integrated with rrtmg_sw, J.-J. Morcrette, ECMWF, Oct 2002
! Revision: Reformatted for consistency with rrtmg_lw: MJIacono, AER, Jul 2006
!
!-----------------------------------------------------------------------

! ------- Declarations -------

! Input

      integer(kind=im), intent (in) :: klev                   ! number of model layers
      integer(kind=im), intent (in) :: kw                     ! g-point index

      real(kind=rb), intent(in) :: pref(:)                    ! direct beam reflectivity
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: prefd(:)                   ! diffuse beam reflectivity
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: ptra(:)                    ! direct beam transmissivity
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: ptrad(:)                   ! diffuse beam transmissivity
                                                              !   Dimensions: (nlayers+1)

      real(kind=rb), intent(in) :: pdbt(:)
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(in) :: ptdbt(:)
                                                              !   Dimensions: (nlayers+1)

      real(kind=rb), intent(inout) :: prdnd(:)
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: prup(:)
                                                              !   Dimensions: (nlayers+1)
      real(kind=rb), intent(inout) :: prupd(:)
                                                              !   Dimensions: (nlayers+1)

! Output
      real(kind=rb), intent(out) :: pfd(:,:)                  ! downwelling flux (W/m2)
                                                              !   Dimensions: (nlayers+1,ngptsw)
                                                              ! unadjusted for earth/sun distance or zenith angle
      real(kind=rb), intent(out) :: pfu(:,:)                  ! upwelling flux (W/m2)
                                                              !   Dimensions: (nlayers+1,ngptsw)
                                                              ! unadjusted for earth/sun distance or zenith angle

! Local

      integer(kind=im) :: ikp, ikx, jk

      real(kind=rb) :: zreflect
      real(kind=rb) :: ztdn(klev+1)  

! Definitions
!
! pref(jk)   direct reflectance
! prefd(jk)  diffuse reflectance
! ptra(jk)   direct transmittance
! ptrad(jk)  diffuse transmittance
!
! pdbt(jk)   layer mean direct beam transmittance
! ptdbt(jk)  total direct beam transmittance at levels
!
!-----------------------------------------------------------------------------
                   
! Link lowest layer with surface
             
      zreflect = 1._rb / (1._rb - prefd(klev+1) * prefd(klev))
      prup(klev) = pref(klev) + (ptrad(klev) * &
                 ((ptra(klev) - pdbt(klev)) * prefd(klev+1) + &
                   pdbt(klev) * pref(klev+1))) * zreflect
      prupd(klev) = prefd(klev) + ptrad(klev) * ptrad(klev) * &
                    prefd(klev+1) * zreflect

! Pass from bottom to top 

      do jk = 1,klev-1
         ikp = klev+1-jk                       
         ikx = ikp-1
         zreflect = 1._rb / (1._rb -prupd(ikp) * prefd(ikx))
         prup(ikx) = pref(ikx) + (ptrad(ikx) * &
                   ((ptra(ikx) - pdbt(ikx)) * prupd(ikp) + &
                     pdbt(ikx) * prup(ikp))) * zreflect
         prupd(ikx) = prefd(ikx) + ptrad(ikx) * ptrad(ikx) * &
                      prupd(ikp) * zreflect
      enddo
    
! Upper boundary conditions

      ztdn(1) = 1._rb
      prdnd(1) = 0._rb
      ztdn(2) = ptra(1)
      prdnd(2) = prefd(1)

! Pass from top to bottom

      do jk = 2,klev
         ikp = jk+1
         zreflect = 1._rb / (1._rb - prefd(jk) * prdnd(jk))
         ztdn(ikp) = ptdbt(jk) * ptra(jk) + &
                    (ptrad(jk) * ((ztdn(jk) - ptdbt(jk)) + &
                     ptdbt(jk) * pref(jk) * prdnd(jk))) * zreflect
         prdnd(ikp) = prefd(jk) + ptrad(jk) * ptrad(jk) * &
                      prdnd(jk) * zreflect
      enddo
    
! Up and down-welling fluxes at levels

      do jk = 1,klev+1
         zreflect = 1._rb / (1._rb - prdnd(jk) * prupd(jk))
         pfu(jk,kw) = (ptdbt(jk) * prup(jk) + &
                      (ztdn(jk) - ptdbt(jk)) * prupd(jk)) * zreflect
         pfd(jk,kw) = ptdbt(jk) + (ztdn(jk) - ptdbt(jk)+ &
                      ptdbt(jk) * prup(jk) * prdnd(jk)) * zreflect
      enddo

      end subroutine vrtqdr_sw

      end module rrtmg_sw_vrtqdr

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $

      module rrtmg_sw_spcvmc

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
      use parrrsw, only : nbndsw, ngptsw, mxmol, jpband
      use rrsw_tbl, only : tblint, bpade, od_lo, exp_tbl
      use rrsw_vsn, only : hvrspc, hnamspc
      use rrsw_wvn, only : ngc, ngs
      use rrtmg_sw_reftra, only: reftra_sw
      use rrtmg_sw_taumol, only: taumol_sw
      use rrtmg_sw_vrtqdr, only: vrtqdr_sw

      implicit none

      contains

! ---------------------------------------------------------------------------
      subroutine spcvmc_sw &
            (nlayers, istart, iend, icpr, iout, &
             pavel, tavel, pz, tz, tbound, palbd, palbp, &
             pcldfmc, ptaucmc, pasycmc, pomgcmc, ptaormc, &
             ptaua, pasya, pomga, prmu0, coldry, wkl, adjflux, &
             laytrop, layswtch, laylow, jp, jt, jt1, &
             co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, &
             fac00, fac01, fac10, fac11, &
             selffac, selffrac, indself, forfac, forfrac, indfor, &
             pbbfd, pbbfu, pbbcd, pbbcu, puvfd, puvcd, pnifd, pnicd, &
             pbbfddir, pbbcddir, puvfddir, puvcddir, pnifddir, pnicddir)
! ---------------------------------------------------------------------------
!
! Purpose: Contains spectral loop to compute the shortwave radiative fluxes, 
!          using the two-stream method of H. Barker and McICA, the Monte-Carlo
!          Independent Column Approximation, for the representation of 
!          sub-grid cloud variability (i.e. cloud overlap).
!
! Interface:  *spcvmc_sw* is called from *rrtmg_sw.F90* or rrtmg_sw.1col.F90*
!
! Method:
!    Adapted from two-stream model of H. Barker;
!    Two-stream model options (selected with kmodts in rrtmg_sw_reftra.F90):
!        1: Eddington, 2: PIFM, Zdunkowski et al., 3: discret ordinates
!
! Modifications:
!
! Original: H. Barker
! Revision: Merge with RRTMG_SW: J.-J.Morcrette, ECMWF, Feb 2003
! Revision: Add adjustment for Earth/Sun distance : MJIacono, AER, Oct 2003
! Revision: Bug fix for use of PALBP and PALBD: MJIacono, AER, Nov 2003
! Revision: Bug fix to apply delta scaling to clear sky: AER, Dec 2004
! Revision: Code modified so that delta scaling is not done in cloudy profiles
!           if routine cldprop is used; delta scaling can be applied by swithcing
!           code below if cldprop is not used to get cloud properties. 
!           AER, Jan 2005
! Revision: Modified to use McICA: MJIacono, AER, Nov 2005
! Revision: Uniform formatting for RRTMG: MJIacono, AER, Jul 2006 
! Revision: Use exponential lookup table for transmittance: MJIacono, AER, 
!           Aug 2007 
!
! ------------------------------------------------------------------

! ------- Declarations ------

! ------- Input -------

      integer(kind=im), intent(in) :: nlayers
      integer(kind=im), intent(in) :: istart
      integer(kind=im), intent(in) :: iend
      integer(kind=im), intent(in) :: icpr
      integer(kind=im), intent(in) :: iout
      integer(kind=im), intent(in) :: laytrop
      integer(kind=im), intent(in) :: layswtch
      integer(kind=im), intent(in) :: laylow

      integer(kind=im), intent(in) :: indfor(:)
                                                               !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: indself(:)
                                                               !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jp(:)
                                                               !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jt(:)
                                                               !   Dimensions: (nlayers)
      integer(kind=im), intent(in) :: jt1(:)
                                                               !   Dimensions: (nlayers)

      real(kind=rb), intent(in) :: pavel(:)                    ! layer pressure (hPa, mb) 
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: tavel(:)                    ! layer temperature (K)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: pz(0:)                      ! level (interface) pressure (hPa, mb)
                                                               !   Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: tz(0:)                      ! level temperatures (hPa, mb)
                                                               !   Dimensions: (0:nlayers)
      real(kind=rb), intent(in) :: tbound                      ! surface temperature (K)
      real(kind=rb), intent(in) :: wkl(:,:)                    ! molecular amounts (mol/cm2) 
                                                               !   Dimensions: (mxmol,nlayers)
      real(kind=rb), intent(in) :: coldry(:)                   ! dry air column density (mol/cm2)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colmol(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: adjflux(:)                  ! Earth/Sun distance adjustment
                                                               !   Dimensions: (jpband)

      real(kind=rb), intent(in) :: palbd(:)                    ! surface albedo (diffuse)
                                                               !   Dimensions: (nbndsw)
      real(kind=rb), intent(in) :: palbp(:)                    ! surface albedo (direct)
                                                               !   Dimensions: (nbndsw)
      real(kind=rb), intent(in) :: prmu0                       ! cosine of solar zenith angle
      real(kind=rb), intent(in) :: pcldfmc(:,:)                ! cloud fraction [mcica]
                                                               !   Dimensions: (nlayers,ngptsw)
      real(kind=rb), intent(in) :: ptaucmc(:,:)                ! cloud optical depth [mcica]
                                                               !   Dimensions: (nlayers,ngptsw)
      real(kind=rb), intent(in) :: pasycmc(:,:)                ! cloud asymmetry parameter [mcica]
                                                               !   Dimensions: (nlayers,ngptsw)
      real(kind=rb), intent(in) :: pomgcmc(:,:)                ! cloud single scattering albedo [mcica]
                                                               !   Dimensions: (nlayers,ngptsw)
      real(kind=rb), intent(in) :: ptaormc(:,:)                ! cloud optical depth, non-delta scaled [mcica]
                                                               !   Dimensions: (nlayers,ngptsw)
      real(kind=rb), intent(in) :: ptaua(:,:)                  ! aerosol optical depth
                                                               !   Dimensions: (nlayers,nbndsw)
      real(kind=rb), intent(in) :: pasya(:,:)                  ! aerosol asymmetry parameter
                                                               !   Dimensions: (nlayers,nbndsw)
      real(kind=rb), intent(in) :: pomga(:,:)                  ! aerosol single scattering albedo
                                                               !   Dimensions: (nlayers,nbndsw)

      real(kind=rb), intent(in) :: colh2o(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colco2(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colch4(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: co2mult(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colo3(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: colo2(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: coln2o(:)
                                                               !   Dimensions: (nlayers)

      real(kind=rb), intent(in) :: forfac(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: forfrac(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: selffac(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: selffrac(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: fac00(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: fac01(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: fac10(:)
                                                               !   Dimensions: (nlayers)
      real(kind=rb), intent(in) :: fac11(:)
                                                               !   Dimensions: (nlayers)

! ------- Output -------
                                                               !   All Dimensions: (nlayers+1)
      real(kind=rb), intent(out) :: pbbcd(:)
      real(kind=rb), intent(out) :: pbbcu(:)
      real(kind=rb), intent(out) :: pbbfd(:)
      real(kind=rb), intent(out) :: pbbfu(:)
      real(kind=rb), intent(out) :: pbbfddir(:)
      real(kind=rb), intent(out) :: pbbcddir(:)

      real(kind=rb), intent(out) :: puvcd(:)
      real(kind=rb), intent(out) :: puvfd(:)
      real(kind=rb), intent(out) :: puvcddir(:)
      real(kind=rb), intent(out) :: puvfddir(:)

      real(kind=rb), intent(out) :: pnicd(:)
      real(kind=rb), intent(out) :: pnifd(:)
      real(kind=rb), intent(out) :: pnicddir(:)
      real(kind=rb), intent(out) :: pnifddir(:)

! Output - inactive                                            !   All Dimensions: (nlayers+1)
!      real(kind=rb), intent(out) :: puvcu(:)
!      real(kind=rb), intent(out) :: puvfu(:)
!      real(kind=rb), intent(out) :: pnicu(:)
!      real(kind=rb), intent(out) :: pnifu(:)
!      real(kind=rb), intent(out) :: pvscd(:)
!      real(kind=rb), intent(out) :: pvscu(:)
!      real(kind=rb), intent(out) :: pvsfd(:)
!      real(kind=rb), intent(out) :: pvsfu(:)

! ------- Local -------

      logical :: lrtchkclr(nlayers),lrtchkcld(nlayers)

      integer(kind=im)  :: klev
      integer(kind=im) :: ib1, ib2, ibm, igt, ikl, ikp, ikx
      integer(kind=im) :: iw, jb, jg, jl, jk
!      integer(kind=im), parameter :: nuv = ?? 
!      integer(kind=im), parameter :: nvs = ?? 
      integer(kind=im) :: itind

      real(kind=rb) :: tblind, ze1
      real(kind=rb) :: zclear, zcloud
      real(kind=rb) :: zdbt(nlayers+1), zdbt_nodel(nlayers+1)
      real(kind=rb) :: zgc(nlayers), zgcc(nlayers), zgco(nlayers)
      real(kind=rb) :: zomc(nlayers), zomcc(nlayers), zomco(nlayers)
      real(kind=rb) :: zrdnd(nlayers+1), zrdndc(nlayers+1)
      real(kind=rb) :: zref(nlayers+1), zrefc(nlayers+1), zrefo(nlayers+1)
      real(kind=rb) :: zrefd(nlayers+1), zrefdc(nlayers+1), zrefdo(nlayers+1)
      real(kind=rb) :: zrup(nlayers+1), zrupd(nlayers+1)
      real(kind=rb) :: zrupc(nlayers+1), zrupdc(nlayers+1)
      real(kind=rb) :: zs1(nlayers+1)
      real(kind=rb) :: ztauc(nlayers), ztauo(nlayers)
      real(kind=rb) :: ztdn(nlayers+1), ztdnd(nlayers+1), ztdbt(nlayers+1)
      real(kind=rb) :: ztoc(nlayers), ztor(nlayers)
      real(kind=rb) :: ztra(nlayers+1), ztrac(nlayers+1), ztrao(nlayers+1)
      real(kind=rb) :: ztrad(nlayers+1), ztradc(nlayers+1), ztrado(nlayers+1)
      real(kind=rb) :: zdbtc(nlayers+1), ztdbtc(nlayers+1)
      real(kind=rb) :: zincflx(ngptsw), zdbtc_nodel(nlayers+1) 
      real(kind=rb) :: ztdbt_nodel(nlayers+1), ztdbtc_nodel(nlayers+1)

      real(kind=rb) :: zdbtmc, zdbtmo, zf, zgw, zreflect
      real(kind=rb) :: zwf, tauorig, repclc
!     real(kind=rb) :: zincflux                                   ! inactive

! Arrays from rrtmg_sw_taumoln routines

!      real(kind=rb) :: ztaug(nlayers,16), ztaur(nlayers,16)
!      real(kind=rb) :: zsflxzen(16)
      real(kind=rb) :: ztaug(nlayers,ngptsw), ztaur(nlayers,ngptsw)
      real(kind=rb) :: zsflxzen(ngptsw)

! Arrays from rrtmg_sw_vrtqdr routine

      real(kind=rb) :: zcd(nlayers+1,ngptsw), zcu(nlayers+1,ngptsw)
      real(kind=rb) :: zfd(nlayers+1,ngptsw), zfu(nlayers+1,ngptsw)

! Inactive arrays
!     real(kind=rb) :: zbbcd(nlayers+1), zbbcu(nlayers+1)
!     real(kind=rb) :: zbbfd(nlayers+1), zbbfu(nlayers+1)
!     real(kind=rb) :: zbbfddir(nlayers+1), zbbcddir(nlayers+1)

! ------------------------------------------------------------------

! Initializations

      ib1 = istart
      ib2 = iend
      klev = nlayers
      iw = 0
      repclc = 1.e-12_rb
!      zincflux = 0.0_rb

      do jk=1,klev+1
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


! Calculate the optical depths for gaseous absorption and Rayleigh scattering

      call taumol_sw(klev, &
                     colh2o, colco2, colch4, colo2, colo3, colmol, &
                     laytrop, jp, jt, jt1, &
                     fac00, fac01, fac10, fac11, &
                     selffac, selffrac, indself, forfac, forfrac, indfor, &
                     zsflxzen, ztaug, ztaur)

! Top of shortwave spectral band loop, jb = 16 -> 29; ibm = 1 -> 14

      do jb = ib1, ib2
         ibm = jb-15
         igt = ngc(ibm)

! Reinitialize g-point counter for each band if output for each band is requested.
         if (iout.gt.0.and.ibm.ge.2) iw = ngs(ibm-1)

!        do jk=1,klev+1
!           zbbcd(jk)=0.0_rb
!           zbbcu(jk)=0.0_rb
!           zbbfd(jk)=0.0_rb
!           zbbfu(jk)=0.0_rb
!        enddo

! Top of g-point interval loop within each band (iw is cumulative counter) 
         do jg = 1,igt
            iw = iw+1

! Apply adjustment for correct Earth/Sun distance and zenith angle to incoming solar flux
            zincflx(iw) = adjflux(jb) * zsflxzen(iw) * prmu0
!             zincflux = zincflux + adjflux(jb) * zsflxzen(iw) * prmu0           ! inactive

! Compute layer reflectances and transmittances for direct and diffuse sources, 
! first clear then cloudy

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

! Clear-sky    
!   TOA direct beam    
            ztdbtc(1)=1.0_rb
            ztdbtc_nodel(1)=1.0_rb
!   Surface values
            zdbtc(klev+1) =0.0_rb
            ztrac(klev+1) =0.0_rb
            ztradc(klev+1)=0.0_rb
            zrefc(klev+1) =palbp(ibm)
            zrefdc(klev+1)=palbd(ibm)
            zrupc(klev+1) =palbp(ibm)
            zrupdc(klev+1)=palbd(ibm)
           
! Total sky    
!   TOA direct beam    
            ztdbt(1)=1.0_rb
            ztdbt_nodel(1)=1.0_rb
!   Surface values
            zdbt(klev+1) =0.0_rb
            ztra(klev+1) =0.0_rb
            ztrad(klev+1)=0.0_rb
            zref(klev+1) =palbp(ibm)
            zrefd(klev+1)=palbd(ibm)
            zrup(klev+1) =palbp(ibm)
            zrupd(klev+1)=palbd(ibm)
    
! Top of layer loop
            do jk=1,klev

! Note: two-stream calculations proceed from top to bottom; 
!   RRTMG_SW quantities are given bottom to top and are reversed here

               ikl=klev+1-jk

! Set logical flag to do REFTRA calculation
!   Do REFTRA for all clear layers
               lrtchkclr(jk)=.true.

!   Do REFTRA only for cloudy layers in profile, since already done for clear layers
               lrtchkcld(jk)=.false.
               lrtchkcld(jk)=(pcldfmc(ikl,iw) > repclc)

! Clear-sky optical parameters - this section inactive     
!   Original
!               ztauc(jk) = ztaur(ikl,iw) + ztaug(ikl,iw)
!               zomcc(jk) = ztaur(ikl,iw) / ztauc(jk)
!               zgcc(jk) = 0.0001_rb
!   Total sky optical parameters        
!               ztauo(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + ptaucmc(ikl,iw)
!               zomco(jk) = ptaucmc(ikl,iw) * pomgcmc(ikl,iw) + ztaur(ikl,iw)
!               zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) + &
!                           ztaur(ikl,iw) * 0.0001_rb) / zomco(jk)
!               zomco(jk) = zomco(jk) / ztauo(jk)

! Clear-sky optical parameters including aerosols
               ztauc(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + ptaua(ikl,ibm)
               zomcc(jk) = ztaur(ikl,iw) * 1.0_rb + ptaua(ikl,ibm) * pomga(ikl,ibm)
               zgcc(jk) = pasya(ikl,ibm) * pomga(ikl,ibm) * ptaua(ikl,ibm) / zomcc(jk)
               zomcc(jk) = zomcc(jk) / ztauc(jk)

! Pre-delta-scaling clear and cloudy direct beam transmittance (must use 'orig', unscaled cloud OD)       
!   \/\/\/ This block of code is only needed for direct beam calculation
!     
               zclear = 1.0_rb - pcldfmc(ikl,iw)
               zcloud = pcldfmc(ikl,iw)

! Clear
!                zdbtmc = exp(-ztauc(jk) / prmu0)

! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
               ze1 = ztauc(jk) / prmu0
               if (ze1 .le. od_lo) then
                  zdbtmc = 1._rb - ze1 + 0.5_rb * ze1 * ze1
               else 
                  tblind = ze1 / (bpade + ze1)
                  itind = tblint * tblind + 0.5_rb
                  zdbtmc = exp_tbl(itind)
               endif

               zdbtc_nodel(jk) = zdbtmc
               ztdbtc_nodel(jk+1) = zdbtc_nodel(jk) * ztdbtc_nodel(jk)

! Clear + Cloud
               tauorig = ztauc(jk) + ptaormc(ikl,iw)
!                zdbtmo = exp(-tauorig / prmu0)

! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
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
!   /\/\/\ Above code only needed for direct beam calculation


! Delta scaling - clear   
               zf = zgcc(jk) * zgcc(jk)
               zwf = zomcc(jk) * zf
               ztauc(jk) = (1.0_rb - zwf) * ztauc(jk)
               zomcc(jk) = (zomcc(jk) - zwf) / (1.0_rb - zwf)
               zgcc (jk) = (zgcc(jk) - zf) / (1.0_rb - zf)

! Total sky optical parameters (cloud properties already delta-scaled)
!   Use this code if cloud properties are derived in rrtmg_sw_cldprop       
               if (icpr .ge. 1) then
                  ztauo(jk) = ztauc(jk) + ptaucmc(ikl,iw)
                  zomco(jk) = ztauc(jk) * zomcc(jk) + ptaucmc(ikl,iw) * pomgcmc(ikl,iw) 
                  zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) + &
                              ztauc(jk) * zomcc(jk) * zgcc(jk)) / zomco(jk)
                  zomco(jk) = zomco(jk) / ztauo(jk)

! Total sky optical parameters (if cloud properties not delta scaled)
!   Use this code if cloud properties are not derived in rrtmg_sw_cldprop       
               elseif (icpr .eq. 0) then
                  ztauo(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + ptaua(ikl,ibm) + ptaucmc(ikl,iw)
                  zomco(jk) = ptaua(ikl,ibm) * pomga(ikl,ibm) + ptaucmc(ikl,iw) * pomgcmc(ikl,iw) + &
                              ztaur(ikl,iw) * 1.0_rb
                  zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) + &
                              ptaua(ikl,ibm)*pomga(ikl,ibm)*pasya(ikl,ibm)) / zomco(jk)
                  zomco(jk) = zomco(jk) / ztauo(jk)

! Delta scaling - clouds 
!   Use only if subroutine rrtmg_sw_cldprop is not used to get cloud properties and to apply delta scaling
                  zf = zgco(jk) * zgco(jk)
                  zwf = zomco(jk) * zf
                  ztauo(jk) = (1._rb - zwf) * ztauo(jk)
                  zomco(jk) = (zomco(jk) - zwf) / (1.0_rb - zwf)
                  zgco (jk) = (zgco(jk) - zf) / (1.0_rb - zf)
               endif 

! End of layer loop
            enddo    

! Clear sky reflectivities
            call reftra_sw (klev, &
                            lrtchkclr, zgcc, prmu0, ztauc, zomcc, &
                            zrefc, zrefdc, ztrac, ztradc)

! Total sky reflectivities      
            call reftra_sw (klev, &
                            lrtchkcld, zgco, prmu0, ztauo, zomco, &
                            zrefo, zrefdo, ztrao, ztrado)

            do jk=1,klev

! Combine clear and cloudy contributions for total sky
               ikl = klev+1-jk 
               zclear = 1.0_rb - pcldfmc(ikl,iw)
               zcloud = pcldfmc(ikl,iw)

               zref(jk) = zclear*zrefc(jk) + zcloud*zrefo(jk)
               zrefd(jk)= zclear*zrefdc(jk) + zcloud*zrefdo(jk)
               ztra(jk) = zclear*ztrac(jk) + zcloud*ztrao(jk)
               ztrad(jk)= zclear*ztradc(jk) + zcloud*ztrado(jk)

! Direct beam transmittance        

! Clear
!                zdbtmc = exp(-ztauc(jk) / prmu0)

! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
               ze1 = ztauc(jk) / prmu0
               if (ze1 .le. od_lo) then
                  zdbtmc = 1._rb - ze1 + 0.5_rb * ze1 * ze1
               else
                  tblind = ze1 / (bpade + ze1)
                  itind = tblint * tblind + 0.5_rb
                  zdbtmc = exp_tbl(itind)
               endif

               zdbtc(jk) = zdbtmc
               ztdbtc(jk+1) = zdbtc(jk)*ztdbtc(jk)

! Clear + Cloud
!                zdbtmo = exp(-ztauo(jk) / prmu0)

! Use exponential lookup table for transmittance, or expansion of 
! exponential for low tau
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
                 
! Vertical quadrature for clear-sky fluxes

            call vrtqdr_sw(klev, iw, &
                           zrefc, zrefdc, ztrac, ztradc, &
                           zdbtc, zrdndc, zrupc, zrupdc, ztdbtc, &
                           zcd, zcu)
      
! Vertical quadrature for cloudy fluxes

            call vrtqdr_sw(klev, iw, &
                           zref, zrefd, ztra, ztrad, &
                           zdbt, zrdnd, zrup, zrupd, ztdbt, &
                           zfd, zfu)

! Upwelling and downwelling fluxes at levels
!   Two-stream calculations go from top to bottom; 
!   layer indexing is reversed to go bottom to top for output arrays

            do jk=1,klev+1
               ikl=klev+2-jk

! Accumulate spectral fluxes over bands - inactive
!               zbbfu(ikl) = zbbfu(ikl) + zincflx(iw)*zfu(jk,iw)  
!               zbbfd(ikl) = zbbfd(ikl) + zincflx(iw)*zfd(jk,iw)
!               zbbcu(ikl) = zbbcu(ikl) + zincflx(iw)*zcu(jk,iw)
!               zbbcd(ikl) = zbbcd(ikl) + zincflx(iw)*zcd(jk,iw)
!               zbbfddir(ikl) = zbbfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
!               zbbcddir(ikl) = zbbcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)

! Accumulate spectral fluxes over whole spectrum  
               pbbfu(ikl) = pbbfu(ikl) + zincflx(iw)*zfu(jk,iw)
               pbbfd(ikl) = pbbfd(ikl) + zincflx(iw)*zfd(jk,iw)
               pbbcu(ikl) = pbbcu(ikl) + zincflx(iw)*zcu(jk,iw)
               pbbcd(ikl) = pbbcd(ikl) + zincflx(iw)*zcd(jk,iw)
               pbbfddir(ikl) = pbbfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
               pbbcddir(ikl) = pbbcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)

! Accumulate direct fluxes for UV/visible bands
               if (ibm >= 10 .and. ibm <= 13) then
                  puvcd(ikl) = puvcd(ikl) + zincflx(iw)*zcd(jk,iw)
                  puvfd(ikl) = puvfd(ikl) + zincflx(iw)*zfd(jk,iw)
                  puvcddir(ikl) = puvcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
                  puvfddir(ikl) = puvfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
! Accumulate direct fluxes for near-IR bands
               else if (ibm == 14 .or. ibm <= 9) then  
                  pnicd(ikl) = pnicd(ikl) + zincflx(iw)*zcd(jk,iw)
                  pnifd(ikl) = pnifd(ikl) + zincflx(iw)*zfd(jk,iw)
                  pnicddir(ikl) = pnicddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
                  pnifddir(ikl) = pnifddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
               endif

            enddo

! End loop on jg, g-point interval
         enddo             

! End loop on jb, spectral band
      enddo                    

      end subroutine spcvmc_sw

      end module rrtmg_sw_spcvmc

!     path:      $Source: /cvsroot/NWP/WRFV3/phys/module_ra_rrtmg_sw.F,v $
!     author:    $Author: trn $
!     revision:  $Revision: 1.3 $
!     created:   $Date: 2009/04/16 19:54:22 $
!
       module rrtmg_sw_rad

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

! --------- Modules ---------

      use parkind, only : im => kind_im, rb => kind_rb
      use rrsw_vsn
      use mcica_subcol_gen_sw, only: mcica_subcol_sw
      use rrtmg_sw_cldprmc, only: cldprmc_sw
! *** Move the required call to rrtmg_sw_ini below and the following 
! use association to GCM initialization area ***
!      use rrtmg_sw_init, only: rrtmg_sw_ini
      use rrtmg_sw_setcoef, only: setcoef_sw
      use rrtmg_sw_spcvmc, only: spcvmc_sw

      implicit none

! public interfaces/functions/subroutines
      public :: rrtmg_sw, inatm_sw, earth_sun

!------------------------------------------------------------------
      contains
!------------------------------------------------------------------

!------------------------------------------------------------------
! Public subroutines
!------------------------------------------------------------------

      subroutine rrtmg_sw &
            (ncol    ,nlay    ,icld    , &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cldfmcl , &
             taucmcl ,ssacmcl ,asmcmcl ,fsfcmcl , &
             ciwpmcl ,clwpmcl ,cswpmcl ,reicmcl ,relqmcl ,resnmcl, &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, aer_opt,  &
! --------- Add the following four compenants for ssib shortwave down radiation ---!
! -------------------      by Zhenxin 2011-06-20      --------------------------------!
             sibvisdir, sibvisdif, sibnirdir, sibnirdif,         &
! ----------------------  End,  Zhenxin 2011-06-20    --------------------------------!
             swdkdir,swdkdif                                & ! jararias, 2013/08/10
                                                                )


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

! --------- Modules ---------

      use parrrsw, only : nbndsw, ngptsw, naerec, nstr, nmol, mxmol, &
                          jpband, jpb1, jpb2
      use rrsw_aer, only : rsrtaua, rsrpiza, rsrasya
      use rrsw_con, only : heatfac, oneminus, pi
      use rrsw_wvn, only : wavenum1, wavenum2

! ------- Declarations

! ----- Input -----

      integer(kind=im), intent(in) :: ncol            ! Number of horizontal columns     
      integer(kind=im), intent(in) :: nlay            ! Number of model layers
      integer(kind=im), intent(inout) :: icld         ! Cloud overlap method
                                                      !    0: Clear only
                                                      !    1: Random
                                                      !    2: Maximum/random
                                                      !    3: Maximum

      real(kind=rb), intent(in) :: play(:,:)          ! Layer pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: plev(:,:)          ! Interface pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tlay(:,:)          ! Layer temperatures (K)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tlev(:,:)          ! Interface temperatures (K)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tsfc(:)            ! Surface temperature (K)
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: h2ovmr(:,:)        ! H2O volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o3vmr(:,:)         ! O3 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: co2vmr(:,:)        ! CO2 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ch4vmr(:,:)        ! Methane volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: n2ovmr(:,:)        ! Nitrous oxide volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o2vmr(:,:)         ! Oxygen volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: asdir(:)           ! UV/vis surface albedo direct rad
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: aldir(:)           ! Near-IR surface albedo direct rad
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: asdif(:)           ! UV/vis surface albedo: diffuse rad
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: aldif(:)           ! Near-IR surface albedo: diffuse rad
                                                      !    Dimensions: (ncol)

      integer(kind=im), intent(in) :: dyofyr          ! Day of the year (used to get Earth/Sun
                                                      !  distance if adjflx not provided)
      real(kind=rb), intent(in) :: adjes              ! Flux adjustment for Earth/Sun distance
      real(kind=rb), intent(in) :: coszen(:)          ! Cosine of solar zenith angle
                                                      !    Dimensions: (ncol)
      real(kind=rb), intent(in) :: scon               ! Solar constant (W/m2)

      integer(kind=im), intent(in) :: inflgsw         ! Flag for cloud optical properties
      integer(kind=im), intent(in) :: iceflgsw        ! Flag for ice particle specification
      integer(kind=im), intent(in) :: liqflgsw        ! Flag for liquid droplet specification

      real(kind=rb), intent(in) :: cldfmcl(:,:,:)     ! Cloud fraction
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: taucmcl(:,:,:)     ! In-cloud optical depth
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: ssacmcl(:,:,:)     ! In-cloud single scattering albedo
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: asmcmcl(:,:,:)     ! In-cloud asymmetry parameter
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: fsfcmcl(:,:,:)     ! In-cloud forward scattering fraction
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: ciwpmcl(:,:,:)     ! In-cloud ice water path (g/m2)
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: clwpmcl(:,:,:)     ! In-cloud liquid water path (g/m2)
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: cswpmcl(:,:,:)     ! In-cloud snow water path (g/m2)
                                                      !    Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: reicmcl(:,:)       ! Cloud ice effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)
                                                      ! specific definition of reicmcl depends on setting of iceflglw:
                                                      ! iceflglw = 0: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !               r_ec must be >= 10.0 microns
                                                      ! iceflglw = 1: ice effective radius, r_ec, (Ebert and Curry, 1992),
                                                      !               r_ec range is limited to 13.0 to 130.0 microns
                                                      ! iceflglw = 2: ice effective radius, r_k, (Key, Streamer Ref. Manual, 1996)
                                                      !               r_k range is limited to 5.0 to 131.0 microns
                                                      ! iceflglw = 3: generalized effective size, dge, (Fu, 1996),
                                                      !               dge range is limited to 5.0 to 140.0 microns
                                                      !               [dge = 1.0315 * r_ec]
      real(kind=rb), intent(in) :: relqmcl(:,:)       ! Cloud water drop effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: resnmcl(:,:)       ! Cloud snow effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tauaer(:,:,:)      ! Aerosol optical depth (iaer=10 only)
                                                      !    Dimensions: (ncol,nlay,nbndsw)
                                                      ! (non-delta scaled)      
      real(kind=rb), intent(in) :: ssaaer(:,:,:)      ! Aerosol single scattering albedo (iaer=10 only)
                                                      !    Dimensions: (ncol,nlay,nbndsw)
                                                      ! (non-delta scaled)      
      real(kind=rb), intent(in) :: asmaer(:,:,:)      ! Aerosol asymmetry parameter (iaer=10 only)
                                                      !    Dimensions: (ncol,nlay,nbndsw)
                                                      ! (non-delta scaled)      
      real(kind=rb), intent(in) :: ecaer(:,:,:)       ! Aerosol optical depth at 0.55 micron (iaer=6 only)
                                                      !    Dimensions: (ncol,nlay,naerec)
                                                      ! (non-delta scaled)      

! ----- Output -----

      real(kind=rb), intent(out) :: swuflx(:,:)       ! Total sky shortwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: swdflx(:,:)       ! Total sky shortwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: sibvisdir(:,:)    ! visible direct downward flux  (W/m2)
                                                      !    Dimensions: (ncol,nlay+1) Zhenxin (2011/06/20)
      real(kind=rb), intent(out) :: sibvisdif(:,:)    ! visible diffusion downward flux  (W/m2)
                                                      !    Dimensions: (ncol,nlay+1) Zhenxin (2011/06/20)
      real(kind=rb), intent(out) :: sibnirdir(:,:)    ! Near IR direct downward flux  (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)  Zhenxin (2011/06/20)
      real(kind=rb), intent(out) :: sibnirdif(:,:)    ! Near IR diffusion downward flux  (W/m2)
                                                      !    Dimensions: (ncol,nlay+1) Zhenxin (2011/06/20)
      real(kind=rb), intent(out) :: swhr(:,:)         ! Total sky shortwave radiative heating rate (K/d)
                                                      !    Dimensions: (ncol,nlay)
      real(kind=rb), intent(out) :: swuflxc(:,:)      ! Clear sky shortwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: swdflxc(:,:)      ! Clear sky shortwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(out) :: swhrc(:,:)        ! Clear sky shortwave radiative heating rate (K/d)
                                                      !    Dimensions: (ncol,nlay)

      integer, intent(in)        :: aer_opt
      real(kind=rb), intent(out) :: &
        swdkdir(:,:), & ! Total shortwave downward direct flux (W/m2),  Dimensions: (ncol,nlay) jararias, 2013/08/10
        swdkdif(:,:)    ! Total shortwave downward diffuse flux (W/m2), Dimensions: (ncol,nlay) jararias, 2013/08/10





! ----- Local -----

! Control
      integer(kind=im) :: nlayers             ! total number of layers
      integer(kind=im) :: istart              ! beginning band of calculation
      integer(kind=im) :: iend                ! ending band of calculation
      integer(kind=im) :: icpr                ! cldprop/cldprmc use flag
      integer(kind=im) :: iout                ! output option flag (inactive)
      integer(kind=im) :: iaer                ! aerosol option flag
      integer(kind=im) :: idelm               ! delta-m scaling flag (inactive)
      integer(kind=im) :: isccos              ! instrumental cosine response flag (inactive)
      integer(kind=im) :: iplon               ! column loop index
      integer(kind=im) :: i                   ! layer loop index                       ! jk
      integer(kind=im) :: ib                  ! band loop index                        ! jsw
      integer(kind=im) :: ia, ig              ! indices
      integer(kind=im) :: k                   ! layer loop index
      integer(kind=im) :: ims                 ! value for changing mcica permute seed
      integer(kind=im) :: imca                ! flag for mcica [0=off, 1=on]

      real(kind=rb) :: zepsec, zepzen         ! epsilon
      real(kind=rb) :: zdpgcp                 ! flux to heating conversion ratio

! Atmosphere
      real(kind=rb) :: pavel(nlay+1)          ! layer pressures (mb) 
      real(kind=rb) :: tavel(nlay+1)          ! layer temperatures (K)
      real(kind=rb) :: pz(0:nlay+1)           ! level (interface) pressures (hPa, mb)
      real(kind=rb) :: tz(0:nlay+1)           ! level (interface) temperatures (K)
      real(kind=rb) :: tbound                 ! surface temperature (K)
      real(kind=rb) :: pdp(nlay+1)            ! layer pressure thickness (hPa, mb)
      real(kind=rb) :: coldry(nlay+1)         ! dry air column amount
      real(kind=rb) :: wkl(mxmol,nlay+1)      ! molecular amounts (mol/cm-2)

!      real(kind=rb) :: earth_sun             ! function for Earth/Sun distance factor
      real(kind=rb) :: cossza                 ! Cosine of solar zenith angle
      real(kind=rb) :: adjflux(jpband)        ! adjustment for current Earth/Sun distance
      real(kind=rb) :: solvar(jpband)         ! solar constant scaling factor from rrtmg_sw
                                              !  default value of 1368.22 Wm-2 at 1 AU
      real(kind=rb) :: albdir(nbndsw)         ! surface albedo, direct          ! zalbp
      real(kind=rb) :: albdif(nbndsw)         ! surface albedo, diffuse         ! zalbd

      real(kind=rb) :: taua(nlay+1,nbndsw)    ! Aerosol optical depth
      real(kind=rb) :: ssaa(nlay+1,nbndsw)    ! Aerosol single scattering albedo
      real(kind=rb) :: asma(nlay+1,nbndsw)    ! Aerosol asymmetry parameter

! Atmosphere - setcoef
      integer(kind=im) :: laytrop             ! tropopause layer index
      integer(kind=im) :: layswtch            ! tropopause layer index
      integer(kind=im) :: laylow              ! tropopause layer index
      integer(kind=im) :: jp(nlay+1)          ! 
      integer(kind=im) :: jt(nlay+1)          !
      integer(kind=im) :: jt1(nlay+1)         !

      real(kind=rb) :: colh2o(nlay+1)         ! column amount (h2o)
      real(kind=rb) :: colco2(nlay+1)         ! column amount (co2)
      real(kind=rb) :: colo3(nlay+1)          ! column amount (o3)
      real(kind=rb) :: coln2o(nlay+1)         ! column amount (n2o)
      real(kind=rb) :: colch4(nlay+1)         ! column amount (ch4)
      real(kind=rb) :: colo2(nlay+1)          ! column amount (o2)
      real(kind=rb) :: colmol(nlay+1)         ! column amount
      real(kind=rb) :: co2mult(nlay+1)        ! column amount 

      integer(kind=im) :: indself(nlay+1)
      integer(kind=im) :: indfor(nlay+1)
      real(kind=rb) :: selffac(nlay+1)
      real(kind=rb) :: selffrac(nlay+1)
      real(kind=rb) :: forfac(nlay+1)
      real(kind=rb) :: forfrac(nlay+1)

      real(kind=rb) :: &                      !
                         fac00(nlay+1), fac01(nlay+1), &
                         fac10(nlay+1), fac11(nlay+1) 

! Atmosphere/clouds - cldprop
      integer(kind=im) :: ncbands             ! number of cloud spectral bands
      integer(kind=im) :: inflag              ! flag for cloud property method
      integer(kind=im) :: iceflag             ! flag for ice cloud properties
      integer(kind=im) :: liqflag             ! flag for liquid cloud properties

!      real(kind=rb) :: cldfrac(nlay+1)        ! layer cloud fraction
!      real(kind=rb) :: tauc(nlay+1)           ! in-cloud optical depth (non-delta scaled)
!      real(kind=rb) :: ssac(nlay+1)           ! in-cloud single scattering albedo (non-delta scaled)
!      real(kind=rb) :: asmc(nlay+1)           ! in-cloud asymmetry parameter (non-delta scaled)
!      real(kind=rb) :: fsfc(nlay+1)           ! in-cloud forward scattering fraction (non-delta scaled)
!      real(kind=rb) :: ciwp(nlay+1)           ! in-cloud ice water path
!      real(kind=rb) :: clwp(nlay+1)           ! in-cloud liquid water path
!      real(kind=rb) :: rei(nlay+1)            ! cloud ice particle size
!      real(kind=rb) :: rel(nlay+1)            ! cloud liquid particle size

!      real(kind=rb) :: taucloud(nlay+1,jpband)  ! in-cloud optical depth
!      real(kind=rb) :: taucldorig(nlay+1,jpband)! in-cloud optical depth (non-delta scaled)
!      real(kind=rb) :: ssacloud(nlay+1,jpband)  ! in-cloud single scattering albedo
!      real(kind=rb) :: asmcloud(nlay+1,jpband)  ! in-cloud asymmetry parameter

! Atmosphere/clouds - cldprmc [mcica]
      real(kind=rb) :: cldfmc(ngptsw,nlay+1)    ! cloud fraction [mcica]
      real(kind=rb) :: ciwpmc(ngptsw,nlay+1)    ! in-cloud ice water path [mcica]
      real(kind=rb) :: clwpmc(ngptsw,nlay+1)    ! in-cloud liquid water path [mcica]
      real(kind=rb) :: cswpmc(ngptsw,nlay+1)    ! in-cloud snow water path [mcica]
      real(kind=rb) :: relqmc(nlay+1)           ! liquid particle effective radius (microns)
      real(kind=rb) :: reicmc(nlay+1)           ! ice particle effective size (microns)
      real(kind=rb) :: resnmc(nlay+1)           ! snow particle effective size (microns)
      real(kind=rb) :: taucmc(ngptsw,nlay+1)    ! in-cloud optical depth [mcica]
      real(kind=rb) :: taormc(ngptsw,nlay+1)    ! unscaled in-cloud optical depth [mcica]
      real(kind=rb) :: ssacmc(ngptsw,nlay+1)    ! in-cloud single scattering albedo [mcica]
      real(kind=rb) :: asmcmc(ngptsw,nlay+1)    ! in-cloud asymmetry parameter [mcica]
      real(kind=rb) :: fsfcmc(ngptsw,nlay+1)    ! in-cloud forward scattering fraction [mcica]

! Atmosphere/clouds/aerosol - spcvrt,spcvmc
      real(kind=rb) :: ztauc(nlay+1,nbndsw)     ! cloud optical depth
      real(kind=rb) :: ztaucorig(nlay+1,nbndsw) ! unscaled cloud optical depth
      real(kind=rb) :: zasyc(nlay+1,nbndsw)     ! cloud asymmetry parameter 
                                                !  (first moment of phase function)
      real(kind=rb) :: zomgc(nlay+1,nbndsw)     ! cloud single scattering albedo
      real(kind=rb) :: ztaua(nlay+1,nbndsw)     ! total aerosol optical depth
      real(kind=rb) :: zasya(nlay+1,nbndsw)     ! total aerosol asymmetry parameter 
      real(kind=rb) :: zomga(nlay+1,nbndsw)     ! total aerosol single scattering albedo

      real(kind=rb) :: zcldfmc(nlay+1,ngptsw)   ! cloud fraction [mcica]
      real(kind=rb) :: ztaucmc(nlay+1,ngptsw)   ! cloud optical depth [mcica]
      real(kind=rb) :: ztaormc(nlay+1,ngptsw)   ! unscaled cloud optical depth [mcica]
      real(kind=rb) :: zasycmc(nlay+1,ngptsw)   ! cloud asymmetry parameter [mcica] 
      real(kind=rb) :: zomgcmc(nlay+1,ngptsw)   ! cloud single scattering albedo [mcica]

      real(kind=rb) :: zbbfu(nlay+2)          ! temporary upward shortwave flux (w/m2)
      real(kind=rb) :: zbbfd(nlay+2)          ! temporary downward shortwave flux (w/m2)
      real(kind=rb) :: zbbcu(nlay+2)          ! temporary clear sky upward shortwave flux (w/m2)
      real(kind=rb) :: zbbcd(nlay+2)          ! temporary clear sky downward shortwave flux (w/m2)
      real(kind=rb) :: zbbfddir(nlay+2)       ! temporary downward direct shortwave flux (w/m2)
      real(kind=rb) :: zbbcddir(nlay+2)       ! temporary clear sky downward direct shortwave flux (w/m2)
      real(kind=rb) :: zuvfd(nlay+2)          ! temporary UV downward shortwave flux (w/m2)
      real(kind=rb) :: zuvcd(nlay+2)          ! temporary clear sky UV downward shortwave flux (w/m2)
      real(kind=rb) :: zuvfddir(nlay+2)       ! temporary UV downward direct shortwave flux (w/m2)
      real(kind=rb) :: zuvcddir(nlay+2)       ! temporary clear sky UV downward direct shortwave flux (w/m2)
      real(kind=rb) :: znifd(nlay+2)          ! temporary near-IR downward shortwave flux (w/m2)
      real(kind=rb) :: znicd(nlay+2)          ! temporary clear sky near-IR downward shortwave flux (w/m2)
      real(kind=rb) :: znifddir(nlay+2)       ! temporary near-IR downward direct shortwave flux (w/m2)
      real(kind=rb) :: znicddir(nlay+2)       ! temporary clear sky near-IR downward direct shortwave flux (w/m2)

! Optional output fields 
      real(kind=rb) :: swnflx(nlay+2)         ! Total sky shortwave net flux (W/m2)
      real(kind=rb) :: swnflxc(nlay+2)        ! Clear sky shortwave net flux (W/m2)
      real(kind=rb) :: dirdflux(nlay+2)       ! Direct downward shortwave surface flux
      real(kind=rb) :: difdflux(nlay+2)       ! Diffuse downward shortwave surface flux
      real(kind=rb) :: uvdflx(nlay+2)         ! Total sky downward shortwave flux, UV/vis  
      real(kind=rb) :: nidflx(nlay+2)         ! Total sky downward shortwave flux, near-IR 
      real(kind=rb) :: dirdnuv(nlay+2)        ! Direct downward shortwave flux, UV/vis
      real(kind=rb) :: difdnuv(nlay+2)        ! Diffuse downward shortwave flux, UV/vis
      real(kind=rb) :: dirdnir(nlay+2)        ! Direct downward shortwave flux, near-IR
      real(kind=rb) :: difdnir(nlay+2)        ! Diffuse downward shortwave flux, near-IR

! Output - inactive
!      real(kind=rb) :: zuvfu(nlay+2)         ! temporary upward UV shortwave flux (w/m2)
!      real(kind=rb) :: zuvfd(nlay+2)         ! temporary downward UV shortwave flux (w/m2)
!      real(kind=rb) :: zuvcu(nlay+2)         ! temporary clear sky upward UV shortwave flux (w/m2)
!      real(kind=rb) :: zuvcd(nlay+2)         ! temporary clear sky downward UV shortwave flux (w/m2)
!      real(kind=rb) :: zvsfu(nlay+2)         ! temporary upward visible shortwave flux (w/m2)
!      real(kind=rb) :: zvsfd(nlay+2)         ! temporary downward visible shortwave flux (w/m2)
!      real(kind=rb) :: zvscu(nlay+2)         ! temporary clear sky upward visible shortwave flux (w/m2)
!      real(kind=rb) :: zvscd(nlay+2)         ! temporary clear sky downward visible shortwave flux (w/m2)
!      real(kind=rb) :: znifu(nlay+2)         ! temporary upward near-IR shortwave flux (w/m2)
!      real(kind=rb) :: znifd(nlay+2)         ! temporary downward near-IR shortwave flux (w/m2)
!      real(kind=rb) :: znicu(nlay+2)         ! temporary clear sky upward near-IR shortwave flux (w/m2)
!      real(kind=rb) :: znicd(nlay+2)         ! temporary clear sky downward near-IR shortwave flux (w/m2)


! Initializations

      iout = 0 !BSINGH(PNNL) initializing iout to zero(Might be wrong!) as this variable is never initialized but used in spcvmc_sw
      zepsec = 1.e-06_rb
      zepzen = 1.e-10_rb
!jm not thread safe      oneminus = 1.0_rb - zepsec
!jm not thread safe      pi = 2._rb * asin(1._rb)

      istart = jpb1
      iend = jpb2
      icpr = 0
      ims = 2

! In a GCM with or without McICA, set nlon to the longitude dimension
!
! Set imca to select calculation type:
!  imca = 0, use standard forward model calculation (clear and overcast only)
!  imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
!            (clear, overcast or partial cloud conditions)

! *** This version uses McICA (imca = 1) ***

! Set icld to select of clear or cloud calculation and cloud 
! overlap method (read by subroutine readprof from input file INPUT_RRTM):  
! icld = 0, clear only
! icld = 1, with clouds using random cloud overlap (McICA only)
! icld = 2, with clouds using maximum/random cloud overlap (McICA only)
! icld = 3, with clouds using maximum cloud overlap (McICA only)
      if (icld.lt.0.or.icld.gt.3) icld = 2

! Set iaer to select aerosol option
! iaer = 0, no aerosols
! iaer = 6, use six ECMWF aerosol types
!           input aerosol optical depth at 0.55 microns for each aerosol type (ecaer)
! iaer = 10, input total aerosol optical depth, single scattering albedo 
!            and asymmetry parameter (tauaer, ssaaer, asmaer) directly
      if ( aer_opt.eq.0 .or. aer_opt.eq.2 .or. aer_opt.eq.3) then
      iaer = 10
      else if ( aer_opt .eq. 1 ) then
      iaer = 6
      endif

! Call model and data initialization, compute lookup tables, perform
! reduction of g-points from 224 to 112 for input absorption
! coefficient data and other arrays.
!
! In a GCM this call should be placed in the model initialization
! area, since this has to be called only once.  
!      call rrtmg_sw_ini(cpdair)

! This is the main longitude/column loop in RRTMG.
! Modify to loop over all columns (nlon) or over daylight columns

      do iplon = 1, ncol

! Prepare atmosphere profile from GCM for use in RRTMG, and define
! other input parameters

         call inatm_sw (iplon, nlay, icld, iaer, &
              play, plev, tlay, tlev, tsfc, h2ovmr, &
              o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, &
              adjes, dyofyr, scon, inflgsw, iceflgsw, liqflgsw, &
              cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, cswpmcl, &
              reicmcl, relqmcl, resnmcl, tauaer, ssaaer, asmaer, &
              nlayers, pavel, pz, pdp, tavel, tz, tbound, coldry, wkl, &
              adjflux, solvar, inflag, iceflag, liqflag, cldfmc, taucmc, &
              ssacmc, asmcmc, fsfcmc, ciwpmc, clwpmc, cswpmc, reicmc, relqmc, resnmc, &
              taua, ssaa, asma)

!  For cloudy atmosphere, use cldprop to set cloud optical properties based on
!  input cloud physical properties.  Select method based on choices described
!  in cldprop.  Cloud fraction, water path, liquid droplet and ice particle
!  effective radius must be passed in cldprop.  Cloud fraction and cloud
!  optical properties are transferred to rrtmg_sw arrays in cldprop.  

         call cldprmc_sw(nlayers, inflag, iceflag, liqflag, cldfmc, &
                         ciwpmc, clwpmc, cswpmc, reicmc, relqmc, resnmc, &
                         taormc, taucmc, ssacmc, asmcmc, fsfcmc)
         icpr = 1

! Calculate coefficients for the temperature and pressure dependence of the 
! molecular absorption coefficients by interpolating data from stored
! reference atmospheres.

         call setcoef_sw(nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl, &
                         laytrop, layswtch, laylow, jp, jt, jt1, &
                         co2mult, colch4, colco2, colh2o, colmol, coln2o, &
                         colo2, colo3, fac00, fac01, fac10, fac11, &
                         selffac, selffrac, indself, forfac, forfrac, indfor)


! Cosine of the solar zenith angle 
!  Prevent using value of zero; ideally, SW model is not called from host model when sun 
!  is below horizon

         cossza = coszen(iplon)
         if (cossza .le. zepzen) cossza = zepzen

! Transfer albedo, cloud and aerosol properties into arrays for 2-stream radiative transfer 

! Surface albedo
!  Near-IR bands 16-24 and 29 (1-9 and 14), 820-16000 cm-1, 0.625-12.195 microns
         do ib=1,9
            albdir(ib) = aldir(iplon)
            albdif(ib) = aldif(iplon)
         enddo
         albdir(nbndsw) = aldir(iplon)
         albdif(nbndsw) = aldif(iplon)
!  UV/visible bands 25-28 (10-13), 16000-50000 cm-1, 0.200-0.625 micron
         do ib=10,13
            albdir(ib) = asdir(iplon)
            albdif(ib) = asdif(iplon)
         enddo


! Clouds
         if (icld.eq.0) then

            zcldfmc(:,:) = 0._rb
            ztaucmc(:,:) = 0._rb
            ztaormc(:,:) = 0._rb
            zasycmc(:,:) = 0._rb
            zomgcmc(:,:) = 1._rb

         elseif (icld.ge.1) then
            do i=1,nlayers
               do ig=1,ngptsw
                  zcldfmc(i,ig) = cldfmc(ig,i)
                  ztaucmc(i,ig) = taucmc(ig,i)
                  ztaormc(i,ig) = taormc(ig,i)
                  zasycmc(i,ig) = asmcmc(ig,i)
                  zomgcmc(i,ig) = ssacmc(ig,i)
               enddo
            enddo

         endif   

! Aerosol
! IAER = 0: no aerosols
         if (iaer.eq.0) then

            ztaua(:,:) = 0._rb
            zasya(:,:) = 0._rb
            zomga(:,:) = 1._rb

! IAER = 6: Use ECMWF six aerosol types. See rrsw_aer.f90 for details.
! Input aerosol optical thickness at 0.55 micron for each aerosol type (ecaer), 
! or set manually here for each aerosol and layer.
         elseif (iaer.eq.6) then

!            do i = 1, nlayers
!               do ia = 1, naerec
!                  ecaer(iplon,i,ia) = 1.0e-15_rb
!               enddo
!            enddo

            do i = 1, nlayers
               do ib = 1, nbndsw
                  ztaua(i,ib) = 0._rb
                  zasya(i,ib) = 0._rb
                  zomga(i,ib) = 0._rb
                  do ia = 1, naerec
                     ztaua(i,ib) = ztaua(i,ib) + rsrtaua(ib,ia) * ecaer(iplon,i,ia)
                     zomga(i,ib) = zomga(i,ib) + rsrtaua(ib,ia) * ecaer(iplon,i,ia) * &
                                   rsrpiza(ib,ia)
                     zasya(i,ib) = zasya(i,ib) + rsrtaua(ib,ia) * ecaer(iplon,i,ia) * &
                                   rsrpiza(ib,ia) * rsrasya(ib,ia)
                  enddo
                  if (zomga(i,ib) /= 0._rb) then
                     zasya(i,ib) = zasya(i,ib) / zomga(i,ib)
                  endif
                  if (ztaua(i,ib) /= 0._rb) then
                     zomga(i,ib) = zomga(i,ib) / ztaua(i,ib)
                  endif
               enddo
            enddo

! IAER=10: Direct specification of aerosol optical properties from GCM
         elseif (iaer.eq.10) then

            do i = 1 ,nlayers
               do ib = 1 ,nbndsw
                  ztaua(i,ib) = taua(i,ib)
                  zasya(i,ib) = asma(i,ib)
                  zomga(i,ib) = ssaa(i,ib)
               enddo
            enddo

         endif


! Call the 2-stream radiation transfer model

         do i=1,nlayers+1
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

         call spcvmc_sw &
             (nlayers, istart, iend, icpr, iout, &
              pavel, tavel, pz, tz, tbound, albdif, albdir, &
              zcldfmc, ztaucmc, zasycmc, zomgcmc, ztaormc, &
              ztaua, zasya, zomga, cossza, coldry, wkl, adjflux, &	 
              laytrop, layswtch, laylow, jp, jt, jt1, &
              co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, &
              fac00, fac01, fac10, fac11, &
              selffac, selffrac, indself, forfac, forfrac, indfor, &
              zbbfd, zbbfu, zbbcd, zbbcu, zuvfd, zuvcd, znifd, znicd, &
              zbbfddir, zbbcddir, zuvfddir, zuvcddir, znifddir, znicddir)

! Transfer up and down, clear and total sky fluxes to output arrays.
! Vertical indexing goes from bottom to top; reverse here for GCM if necessary.

         do i = 1, nlayers+1
            swuflxc(iplon,i) = zbbcu(i)
            swdflxc(iplon,i) = zbbcd(i)
            swuflx(iplon,i) = zbbfu(i)
            swdflx(iplon,i) = zbbfd(i)
            uvdflx(i) = zuvfd(i)
            nidflx(i) = znifd(i)

!  Direct/diffuse fluxes
            dirdflux(i) = zbbfddir(i)
            difdflux(i) = swdflx(iplon,i) - dirdflux(i)
            swdkdir(iplon,i) = dirdflux(i) ! all-sky direct flux   jararias, 2013/08/10
            swdkdif(iplon,i) = difdflux(i) ! all-sky diffuse flux  jararias, 2013/08/10

!  UV/visible direct/diffuse fluxes
            dirdnuv(i) = zuvfddir(i)
            difdnuv(i) = zuvfd(i) - dirdnuv(i)
!  ------- Zhenxin add vis/uv downwards dir or dif here --!
            sibvisdir(iplon,i) = dirdnuv(i)
            sibvisdif(iplon,i) = difdnuv(i)
!  ----- End of Zhenxin addition  ------------!
!  Near-IR direct/diffuse fluxes
            dirdnir(i) = znifddir(i)
            difdnir(i) = znifd(i) - dirdnir(i)
!  ---------Zhenxin add nir downwards dir and dif here --!
            sibnirdir(iplon,i) = dirdnir(i)
            sibnirdif(iplon,i) = difdnir(i)
!  --------    End of Zhenxin addition 2011-05  ---------!
         enddo

!  Total and clear sky net fluxes
         do i = 1, nlayers+1
            swnflxc(i) = swdflxc(iplon,i) - swuflxc(iplon,i)
            swnflx(i) = swdflx(iplon,i) - swuflx(iplon,i)
         enddo

!  Total and clear sky heating rates
         do i = 1, nlayers
            zdpgcp = heatfac / pdp(i)
            swhrc(iplon,i) = (swnflxc(i+1) - swnflxc(i)) * zdpgcp
            swhr(iplon,i) = (swnflx(i+1) - swnflx(i)) * zdpgcp
         enddo
         swhrc(iplon,nlayers) = 0._rb
         swhr(iplon,nlayers) = 0._rb

! End longitude loop
      enddo

      end subroutine rrtmg_sw

!*************************************************************************
      real(kind=rb) function earth_sun(idn)
!*************************************************************************
!
!  Purpose: Function to calculate the correction factor of Earth's orbit
!  for current day of the year

!  idn        : Day of the year
!  earth_sun  : square of the ratio of mean to actual Earth-Sun distance

! ------- Modules -------

      use rrsw_con, only : pi

      integer(kind=im), intent(in) :: idn

      real(kind=rb) :: gamma

      gamma = 2._rb*pi*(idn-1)/365._rb

! Use Iqbal's equation 1.2.1

      earth_sun = 1.000110_rb + .034221_rb * cos(gamma) + .001289_rb * sin(gamma) + &
                   .000719_rb * cos(2._rb*gamma) + .000077_rb * sin(2._rb*gamma)

      end function earth_sun

!***************************************************************************
      subroutine inatm_sw (iplon, nlay, icld, iaer, &
            play, plev, tlay, tlev, tsfc, h2ovmr, &
            o3vmr, co2vmr, ch4vmr, n2ovmr, o2vmr, &
            adjes, dyofyr, scon, inflgsw, iceflgsw, liqflgsw, &
            cldfmcl, taucmcl, ssacmcl, asmcmcl, fsfcmcl, ciwpmcl, clwpmcl, cswpmcl, &
            reicmcl, relqmcl, resnmcl, tauaer, ssaaer, asmaer, &
            nlayers, pavel, pz, pdp, tavel, tz, tbound, coldry, wkl, &
            adjflux, solvar, inflag, iceflag, liqflag, cldfmc, taucmc, &
            ssacmc, asmcmc, fsfcmc, ciwpmc, clwpmc, cswpmc, reicmc, relqmc, resnmc, &
            taua, ssaa, asma)                                       
!***************************************************************************
!
!  Input atmospheric profile from GCM, and prepare it for use in RRTMG_SW.
!  Set other RRTMG_SW input parameters.  
!
!***************************************************************************

! --------- Modules ----------

      use parrrsw, only : nbndsw, ngptsw, nstr, nmol, mxmol, &
                          jpband, jpb1, jpb2, rrsw_scon
      use rrsw_con, only : heatfac, oneminus, pi, grav, avogad
      use rrsw_wvn, only : ng, nspa, nspb, wavenum1, wavenum2, delwave

! ------- Declarations -------

! ----- Input -----
      integer(kind=im), intent(in) :: iplon           ! column loop index
      integer(kind=im), intent(in) :: nlay            ! number of model layers
      integer(kind=im), intent(in) :: icld            ! clear/cloud and cloud overlap flag
      integer(kind=im), intent(in) :: iaer            ! aerosol option flag

      real(kind=rb), intent(in) :: play(:,:)          ! Layer pressures (hPa, mb)
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: plev(:,:)          ! Interface pressures (hPa, mb)
                                                      ! Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tlay(:,:)          ! Layer temperatures (K)
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: tlev(:,:)          ! Interface temperatures (K)
                                                      ! Dimensions: (ncol,nlay+1)
      real(kind=rb), intent(in) :: tsfc(:)            ! Surface temperature (K)
                                                      ! Dimensions: (ncol)
      real(kind=rb), intent(in) :: h2ovmr(:,:)        ! H2O volume mixing ratio
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o3vmr(:,:)         ! O3 volume mixing ratio
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: co2vmr(:,:)        ! CO2 volume mixing ratio
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: ch4vmr(:,:)        ! Methane volume mixing ratio
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: n2ovmr(:,:)        ! Nitrous oxide volume mixing ratio
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: o2vmr(:,:)         ! Oxygen volume mixing ratio
                                                      ! Dimensions: (ncol,nlay)

      integer(kind=im), intent(in) :: dyofyr          ! Day of the year (used to get Earth/Sun
                                                      !  distance if adjflx not provided)
      real(kind=rb), intent(in) :: adjes              ! Flux adjustment for Earth/Sun distance
      real(kind=rb), intent(in) :: scon               ! Solar constant (W/m2)

      integer(kind=im), intent(in) :: inflgsw         ! Flag for cloud optical properties
      integer(kind=im), intent(in) :: iceflgsw        ! Flag for ice particle specification
      integer(kind=im), intent(in) :: liqflgsw        ! Flag for liquid droplet specification

      real(kind=rb), intent(in) :: cldfmcl(:,:,:)     ! Cloud fraction
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: taucmcl(:,:,:)     ! In-cloud optical depth (optional)
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: ssacmcl(:,:,:)     ! In-cloud single scattering albedo
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: asmcmcl(:,:,:)     ! In-cloud asymmetry parameter
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: fsfcmcl(:,:,:)     ! In-cloud forward scattering fraction
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: ciwpmcl(:,:,:)     ! In-cloud ice water path (g/m2)
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: clwpmcl(:,:,:)     ! In-cloud liquid water path (g/m2)
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: cswpmcl(:,:,:)     ! In-cloud snow water path (g/m2)
                                                      ! Dimensions: (ngptsw,ncol,nlay)
      real(kind=rb), intent(in) :: reicmcl(:,:)       ! Cloud ice effective size (microns)
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: relqmcl(:,:)       ! Cloud water drop effective radius (microns)
                                                      ! Dimensions: (ncol,nlay)
      real(kind=rb), intent(in) :: resnmcl(:,:)       ! Cloud snow effective radius (microns)
                                                      ! Dimensions: (ncol,nlay)

      real(kind=rb), intent(in) :: tauaer(:,:,:)      ! Aerosol optical depth
                                                      ! Dimensions: (ncol,nlay,nbndsw)
      real(kind=rb), intent(in) :: ssaaer(:,:,:)      ! Aerosol single scattering albedo
                                                      ! Dimensions: (ncol,nlay,nbndsw)
      real(kind=rb), intent(in) :: asmaer(:,:,:)      ! Aerosol asymmetry parameter
                                                      ! Dimensions: (ncol,nlay,nbndsw)

! Atmosphere
      integer(kind=im), intent(out) :: nlayers        ! number of layers

      real(kind=rb), intent(out) :: pavel(:)          ! layer pressures (mb) 
                                                      ! Dimensions: (nlay)
      real(kind=rb), intent(out) :: tavel(:)          ! layer temperatures (K)
                                                      ! Dimensions: (nlay)
      real(kind=rb), intent(out) :: pz(0:)            ! level (interface) pressures (hPa, mb)
                                                      ! Dimensions: (0:nlay)
      real(kind=rb), intent(out) :: tz(0:)            ! level (interface) temperatures (K)
                                                      ! Dimensions: (0:nlay)
      real(kind=rb), intent(out) :: tbound            ! surface temperature (K)
      real(kind=rb), intent(out) :: pdp(:)            ! layer pressure thickness (hPa, mb)
                                                      ! Dimensions: (nlay)
      real(kind=rb), intent(out) :: coldry(:)         ! dry air column density (mol/cm2)
                                                      ! Dimensions: (nlay)
      real(kind=rb), intent(out) :: wkl(:,:)          ! molecular amounts (mol/cm-2)
                                                      ! Dimensions: (mxmol,nlay)

      real(kind=rb), intent(out) :: adjflux(:)        ! adjustment for current Earth/Sun distance
                                                      ! Dimensions: (jpband)
      real(kind=rb), intent(out) :: solvar(:)         ! solar constant scaling factor from rrtmg_sw
                                                      ! Dimensions: (jpband)
                                                      !  default value of 1368.22 Wm-2 at 1 AU
      real(kind=rb), intent(out) :: taua(:,:)         ! Aerosol optical depth
                                                      ! Dimensions: (nlay,nbndsw)
      real(kind=rb), intent(out) :: ssaa(:,:)         ! Aerosol single scattering albedo
                                                      ! Dimensions: (nlay,nbndsw)
      real(kind=rb), intent(out) :: asma(:,:)         ! Aerosol asymmetry parameter
                                                      ! Dimensions: (nlay,nbndsw)

! Atmosphere/clouds - cldprop
      integer(kind=im), intent(out) :: inflag         ! flag for cloud property method
      integer(kind=im), intent(out) :: iceflag        ! flag for ice cloud properties
      integer(kind=im), intent(out) :: liqflag        ! flag for liquid cloud properties

      real(kind=rb), intent(out) :: cldfmc(:,:)       ! layer cloud fraction
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: taucmc(:,:)       ! in-cloud optical depth (non-delta scaled)
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: ssacmc(:,:)       ! in-cloud single scattering albedo (non-delta-scaled)
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: asmcmc(:,:)       ! in-cloud asymmetry parameter (non-delta scaled)
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: fsfcmc(:,:)       ! in-cloud forward scattering fraction (non-delta scaled)
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: ciwpmc(:,:)       ! in-cloud ice water path
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: clwpmc(:,:)       ! in-cloud liquid water path
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: cswpmc(:,:)       ! in-cloud snow path
                                                      ! Dimensions: (ngptsw,nlay)
      real(kind=rb), intent(out) :: relqmc(:)         ! liquid particle effective radius (microns)
                                                      ! Dimensions: (nlay)
      real(kind=rb), intent(out) :: reicmc(:)         ! ice particle effective size (microns)
                                                      ! Dimensions: (nlay)
      real(kind=rb), intent(out) :: resnmc(:)         ! snow particle effective size (microns)
                                                      ! Dimensions: (nlay)

! ----- Local -----
      real(kind=rb), parameter :: amd = 28.9660_rb    ! Effective molecular weight of dry air (g/mol)
      real(kind=rb), parameter :: amw = 18.0160_rb    ! Molecular weight of water vapor (g/mol)
!      real(kind=rb), parameter :: amc = 44.0098_rb   ! Molecular weight of carbon dioxide (g/mol)
!      real(kind=rb), parameter :: amo = 47.9998_rb   ! Molecular weight of ozone (g/mol)
!      real(kind=rb), parameter :: amo2 = 31.9999_rb  ! Molecular weight of oxygen (g/mol)
!      real(kind=rb), parameter :: amch4 = 16.0430_rb ! Molecular weight of methane (g/mol)
!      real(kind=rb), parameter :: amn2o = 44.0128_rb ! Molecular weight of nitrous oxide (g/mol)

! Set molecular weight ratios (for converting mmr to vmr)
!  e.g. h2ovmr = h2ommr * amdw)
      real(kind=rb), parameter :: amdw = 1.607793_rb  ! Molecular weight of dry air / water vapor
      real(kind=rb), parameter :: amdc = 0.658114_rb  ! Molecular weight of dry air / carbon dioxide
      real(kind=rb), parameter :: amdo = 0.603428_rb  ! Molecular weight of dry air / ozone
      real(kind=rb), parameter :: amdm = 1.805423_rb  ! Molecular weight of dry air / methane
      real(kind=rb), parameter :: amdn = 0.658090_rb  ! Molecular weight of dry air / nitrous oxide
      real(kind=rb), parameter :: amdo2 = 0.905140_rb ! Molecular weight of dry air / oxygen

      real(kind=rb), parameter :: sbc = 5.67e-08_rb   ! Stefan-Boltzmann constant (W/m2K4)

      integer(kind=im) :: isp, l, ix, n, imol, ib, ig   ! Loop indices
      real(kind=rb) :: amm, summol                      ! 
      real(kind=rb) :: adjflx                           ! flux adjustment for Earth/Sun distance
!      real(kind=rb) :: earth_sun                        ! function for Earth/Sun distance adjustment

      nlayers = nlay

!  Initialize all molecular amounts to zero here, then pass input amounts
!  into RRTM array WKL below.

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
 
! Set flux adjustment for current Earth/Sun distance (two options).
! 1) Use Earth/Sun distance flux adjustment provided by GCM (input as adjes);
      adjflx = adjes
!
! 2) Calculate Earth/Sun distance from DYOFYR, the cumulative day of the year.
!    (Set adjflx to 1. to use constant Earth/Sun distance of 1 AU). 
      if (dyofyr .gt. 0) then
         adjflx = earth_sun(dyofyr)
      endif

! Set incoming solar flux adjustment to include adjustment for
! current Earth/Sun distance (ADJFLX) and scaling of default internal
! solar constant (rrsw_scon = 1368.22 Wm-2) by band (SOLVAR).  SOLVAR can be set 
! to a single scaling factor as needed, or to a different value in each 
! band, which may be necessary for paleoclimate simulations. 
! 
      do ib = jpb1,jpb2
!         solvar(ib) = 1._rb
         solvar(ib) = scon / rrsw_scon 
         adjflux(ib) = adjflx * solvar(ib)
      enddo

!  Set surface temperature.
      tbound = tsfc(iplon)

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

      pz(0) = plev(iplon,1)
      tz(0) = tlev(iplon,1)
      do l = 1, nlayers
         pavel(l) = play(iplon,l)
         tavel(l) = tlay(iplon,l)
         pz(l) = plev(iplon,l+1)
         tz(l) = tlev(iplon,l+1)
         pdp(l) = pz(l-1) - pz(l)
! For h2o input in vmr:
         wkl(1,l) = h2ovmr(iplon,l)
! For h2o input in mmr:
!         wkl(1,l) = h2o(iplon,l)*amdw
! For h2o input in specific humidity;
!         wkl(1,l) = (h2o(iplon,l)/(1._rb - h2o(iplon,l)))*amdw
         wkl(2,l) = co2vmr(iplon,l)
         wkl(3,l) = o3vmr(iplon,l)
         wkl(4,l) = n2ovmr(iplon,l)
         wkl(6,l) = ch4vmr(iplon,l)
         wkl(7,l) = o2vmr(iplon,l)
         amm = (1._rb - wkl(1,l)) * amd + wkl(1,l) * amw            
         coldry(l) = (pz(l-1)-pz(l)) * 1.e3_rb * avogad / &
                     (1.e2_rb * grav * amm * (1._rb + wkl(1,l)))
      enddo

! The following section can be used to set values for an additional layer (from
! the GCM top level to 1.e-4 mb) for improved calculation of TOA fluxes. 
! Temperature and molecular amounts in the extra model layer are set to 
! their values in the top GCM model layer, though these can be modified
! here if necessary. 
! If this feature is utilized, increase nlayers by one above, limit the two
! loops above to (nlayers-1), and set the top most (nlayers) layer values here. 

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
!      coldry(nlayers) = (pz(nlayers-1)) * 1.e3_rb * avogad / &
!                        (1.e2_rb * grav * amm * (1._rb + wkl(1,nlayers-1)))

! At this point all molecular amounts in wkl are in volume mixing ratio; 
! convert to molec/cm2 based on coldry for use in rrtm.  

      do l = 1, nlayers
         do imol = 1, nmol
            wkl(imol,l) = coldry(l) * wkl(imol,l)
         enddo
      enddo

! Transfer aerosol optical properties to RRTM variables;
! modify to reverse layer indexing here if necessary.

      if (iaer .ge. 1) then 
         do l = 1, nlayers
            do ib = 1, nbndsw
               taua(l,ib) = tauaer(iplon,l,ib)
               ssaa(l,ib) = ssaaer(iplon,l,ib)
               asma(l,ib) = asmaer(iplon,l,ib)
            enddo
         enddo
      endif

! Transfer cloud fraction and cloud optical properties to RRTM variables;
! modify to reverse layer indexing here if necessary.

      if (icld .ge. 1) then 
         inflag = inflgsw
         iceflag = iceflgsw
         liqflag = liqflgsw

! Move incoming GCM cloud arrays to RRTMG cloud arrays.
! For GCM input, incoming reicmcl is defined based on selected ice parameterization (inflgsw)

         do l = 1, nlayers
            do ig = 1, ngptsw
               cldfmc(ig,l) = cldfmcl(ig,iplon,l)
               taucmc(ig,l) = taucmcl(ig,iplon,l)
               ssacmc(ig,l) = ssacmcl(ig,iplon,l)
               asmcmc(ig,l) = asmcmcl(ig,iplon,l)
               fsfcmc(ig,l) = fsfcmcl(ig,iplon,l)
               ciwpmc(ig,l) = ciwpmcl(ig,iplon,l)
               clwpmc(ig,l) = clwpmcl(ig,iplon,l)
               if (iceflag.eq.5) then
                  cswpmc(ig,l)=cswpmcl(ig,iplon,l)
               endif 
            enddo
            reicmc(l) = reicmcl(iplon,l)
            relqmc(l) = relqmcl(iplon,l)
            if (iceflag.eq.5) then
               resnmc(l) = resnmcl(iplon,l)
            endif 
         enddo

! If an extra layer is being used in RRTMG, set all cloud properties to zero in the extra layer.

!         cldfmc(:,nlayers) = 0.0_rb
!         taucmc(:,nlayers) = 0.0_rb
!         ssacmc(:,nlayers) = 1.0_rb
!         asmcmc(:,nlayers) = 0.0_rb
!         fsfcmc(:,nlayers) = 0.0_rb
!         ciwpmc(:,nlayers) = 0.0_rb
!         clwpmc(:,nlayers) = 0.0_rb
!         reicmc(nlayers) = 0.0_rb
!         relqmc(nlayers) = 0.0_rb
      
      endif

      end subroutine inatm_sw

      end module rrtmg_sw_rad

!------------------------------------------------------------------
MODULE module_ra_rrtmg_sw

use module_model_constants, only : cp
USE module_wrf_error
#if (HWRF == 1)
USE module_state_description, ONLY : FER_MP_HIRES, FER_MP_HIRES_ADVECT, ETAMP_HWRF 
#else
USE module_state_description, ONLY : FER_MP_HIRES, FER_MP_HIRES_ADVECT
#endif
!USE module_dm

use parrrsw, only : nbndsw, ngptsw, naerec
use rrtmg_sw_init, only: rrtmg_sw_ini
use rrtmg_sw_rad, only: rrtmg_sw
use mcica_subcol_gen_sw, only: mcica_subcol_sw

use module_ra_rrtmg_lw, only : inirad, o3data, relcalc, reicalc, retab
!                               mcica_random_numbers, randomNumberSequence, &
!                               new_RandomNumberSequence, getRandomReal

CONTAINS

!------------------------------------------------------------------
   SUBROUTINE RRTMG_SWRAD(                                        &
                       rthratensw,                                &
                       swupt, swuptc, swdnt, swdntc,              &
                       swupb, swupbc, swdnb, swdnbc,              &
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
                       progn,                                     &
                       qndrop3d,f_qndrop,                         & !czhao
                       mp_physics,                                & !wang 2014/12
                       ids,ide, jds,jde, kds,kde,                 & 
                       ims,ime, jms,jme, kms,kme,                 &
                       its,ite, jts,jte, kts,kte,                 &
                       swupflx, swupflxc, swdnflx, swdnflxc,      &
                       tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw,       & ! jararias 2013/11
                       swddir, swddni, swddif,                    & ! jararias 2013/08
                       xcoszen,julian                             & ! jararias 2013/08
                                                                  )
!------------------------------------------------------------------
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
         INTENT(INOUT)  ::                            RTHRATENSW

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
            swddif      ! All-sky broadband surface diffuse irradiance
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
                                       SWUPT,SWUPTC,SWDNT,SWDNTC, &
                                       SWUPB,SWUPBC,SWDNB,SWDNBC

! Layer shortwave fluxes (including extra layer above model top)
! Vertical ordering is from bottom to top (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(OUT) ::                                 &
                               SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC

!  LOCAL VARS
 
   REAL, DIMENSION( kts:kte+1 ) ::                          Pw1D, &
                                                            Tw1D

   REAL, DIMENSION( kts:kte ) ::                          TTEN1D, &
                                                        CLDFRA1D, &
                                                            DZ1D, &
                                                             P1D, &
                                                             T1D, &
                                                            QV1D, &
                                                            QC1D, &
                                                            QR1D, &
                                                            QI1D, &
                                                           RHO1D, &
                                                            QS1D, &
                                                            QG1D, &
                                                            O31D, &
                                                          qndrop1d 

!BSF: From eq. (5) on p. 2434 in McFarquhar & Heymsfield (1996)
      real, parameter :: re_50C=1250.0/9.917, re_40C=1250.0/9.337,     &
                         re_30C=1250.0/9.208, re_20C=1250.0/9.387

! Added local arrays for RRTMG
    integer ::                                              ncol, &
                                                            nlay, &
                                                            icld, &
                                                         inflgsw, &
                                                        iceflgsw, &
                                                        liqflgsw
! Dimension with extra layer from model top to TOA
    real, dimension( 1, kts:kte+2 )  ::                     plev, &
                                                            tlev
    real, dimension( 1, kts:kte+1 )  ::                     play, &
                                                            tlay, &
                                                          h2ovmr, &
                                                           o3vmr, &
                                                          co2vmr, &
                                                           o2vmr, &
                                                          ch4vmr, &
                                                          n2ovmr
    real, dimension( kts:kte+1 )  ::                       o3mmr
! Surface albedo (for UV/visible and near-IR spectral regions,
! and for direct and diffuse radiation)
    real, dimension( 1 )  ::                               asdir, &
                                                           asdif, &
                                                           aldir, &
                                                           aldif
! Dimension with extra layer from model top to TOA, 
! though no clouds are allowed in extra layer
    real, dimension( 1, kts:kte+1 )  ::                   clwpth, &
                                                          ciwpth, &
                                                          cswpth, &
                                                             rel, &
                                                             rei, &
                                                             res, &
                                                         cldfrac, &
                                                         relqmcl, &
                                                         reicmcl, &
                                                         resnmcl
    real, dimension( nbndsw, 1, kts:kte+1 )  ::           taucld, &
                                                          ssacld, &
                                                          asmcld, &
                                                          fsfcld
    real, dimension( ngptsw, 1, kts:kte+1 )  ::          cldfmcl, &
                                                         clwpmcl, &
                                                         ciwpmcl, &
                                                         cswpmcl, &
                                                         taucmcl, &
                                                         ssacmcl, &
                                                         asmcmcl, &
                                                         fsfcmcl
    real, dimension( 1, kts:kte+1, nbndsw )  ::           tauaer, &
                                                          ssaaer, &
                                                          asmaer   
    real, dimension( 1, kts:kte+1, naerec )  ::            ecaer

! Output arrays contain extra layer from model top to TOA
    real, dimension( 1, kts:kte+2 )  ::                   swuflx, &
                                                          swdflx, &
                                                         swuflxc, &
                                                         swdflxc, &
                                                       sibvisdir, &  ! Zhenxin 2011-06-20
                                                       sibvisdif, &
                                                       sibnirdir, &
                                                       sibnirdif     ! Zhenxin 2011-06-20

    real, dimension( 1, kts:kte+2 ) ::                   swdkdir, &  ! jararias, 2013/08/10
                                                         swdkdif     ! jararias, 2013/08/10

    real, dimension( 1, kts:kte+1 )  ::                     swhr, &
                                                           swhrc

    real, dimension ( 1 ) ::                                tsfc, &
                                                              ps, &
                                                          coszen
    real ::                                                   ro, &
                                                              dz, &
                                                           adjes, &
                                                            scon, &  
                                                  snow_mass_factor
    integer ::                                            dyofyr

    integer:: idx_rei
    real:: corr

! Set trace gas volume mixing ratios, 2005 values, IPCC (2007)
! carbon dioxide (379 ppmv)
    real :: co2
    data co2 / 379.e-6 / 
! methane (1774 ppbv)
    real :: ch4
    data ch4 / 1774.e-9 / 
! nitrous oxide (319 ppbv)
    real :: n2o
    data n2o / 319.e-9 / 
! Set oxygen volume mixing ratio (for o2mmr=0.23143)
    real :: o2
    data o2 / 0.209488 /

    integer :: iplon, irng, permuteseed
    integer :: nb

! For old lw cloud property specification
! Cloud and precipitation absorption coefficients
!    real :: abcw,abice,abrn,absn
!    data abcw /0.144/
!    data abice /0.0735/
!    data abrn /0.330e-3/
!    data absn /2.34e-3/

! Molecular weights and ratios for converting mmr to vmr units
!    real :: amd       ! Effective molecular weight of dry air (g/mol)  
!    real :: amw       ! Molecular weight of water vapor (g/mol)        
!    real :: amo       ! Molecular weight of ozone (g/mol)              
!    real :: amo2      ! Molecular weight of oxygen (g/mol)              
! Atomic weights for conversion from mass to volume mixing ratios                
!    data amd   /  28.9660   /                                                  
!    data amw   /  18.0160   /                                                  
!    data amo   /  47.9998   /                                                  
!    data amo2  /  31.9999   /
                                                                                 
    real :: amdw     ! Molecular weight of dry air / water vapor  
    real :: amdo     ! Molecular weight of dry air / ozone
    real :: amdo2    ! Molecular weight of dry air / oxygen
    data amdw /  1.607793 /                                                    
    data amdo /  0.603461 /
    data amdo2 / 0.905190 /
    
!!
    real, dimension(1, 1:kte-kts+1 )  :: pdel          ! Layer pressure thickness (mb)

    real, dimension(1, 1:kte-kts+1) ::   cicewp, &     ! in-cloud cloud ice water path
                                         cliqwp, &     ! in-cloud cloud liquid water path
                                         csnowp, &     ! in-cloud snow water path
                                          reliq, &     ! effective drop radius (microns)
                                          reice        ! ice effective drop size (microns)
    real, dimension(1, 1:kte-kts+1):: recloud1d, &
                                        reice1d, &
                                       resnow1d
    real :: gliqwp, gicewp, gsnowp, gravmks, tem1,tem2,tem3

!
!    REAL   ::  TSFC,GLW0,OLR0,EMISS0,FP
    REAL   ::  FP

!    real, dimension(1:ite-its+1 )          ::   clat     ! latitude in radians for columns
    real :: coszrs                      ! Cosine of solar zenith angle for present latitude 
    logical :: dorrsw                   ! Flag to allow shortwave calculation

    real, dimension (1) :: landfrac, landm, snowh, icefrac

    integer :: pcols, pver

    INTEGER :: i,j,K, na
    LOGICAL :: predicate

    REAL :: da, eot ! jararias, 14/08/2013

!------------------------------------------------------------------
#if ( WRF_CHEM == 1 )
      IF ( aer_ra_feedback == 1) then
      IF ( .NOT. &
      ( PRESENT(tauaer300) .AND. &
        PRESENT(tauaer400) .AND. &
        PRESENT(tauaer600) .AND. &
        PRESENT(tauaer999) .AND. &
        PRESENT(gaer300) .AND. &
        PRESENT(gaer400) .AND. &
        PRESENT(gaer600) .AND. &
        PRESENT(gaer999) .AND. &
        PRESENT(waer300) .AND. &
        PRESENT(waer400) .AND. &
        PRESENT(waer600) .AND. &
        PRESENT(waer999) ) ) THEN
      CALL wrf_error_fatal  &
      ('Warning: missing fields required for aerosol radiation' )
      ENDIF
      ENDIF
#endif

!-----CALCULATE SHORT WAVE RADIATION
!                                                              
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

! Select cloud liquid and ice optics parameterization options
! For passing in cloud optical properties directly:
!         icld = 2
!         inflgsw = 0
!         iceflgsw = 0
!         liqflgsw = 0
! For passing in cloud physical properties; cloud optics parameterized in RRTMG:
         icld = 2
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
         ENDIF

! Set cosine of solar zenith angle
         coszen(ncol) = coszrs
! Set solar constant
         scon = solcon
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

         call mcica_subcol_sw(iplon, ncol, nlay, icld, permuteseed, irng, play, &
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
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, aer_opt, &
! -----      Zhenxin added for ssib coupiling 2011-06-20 --------!
             sibvisdir, sibvisdif, sibnirdir, sibnirdif,         &
! --------------------   End of addiation by Zhenxin 2011-06-20 ------!
             swdkdir, swdkdif                      &  ! jararias, 2012/08/10
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
         endif
            swddir(i,j)    = swdkdir(1,1)          ! jararias 2013/08/10
            swddni(i,j)    = swddir(i,j) / coszrs  ! jararias 2013/08/10
            swddif(i,j)    = swdkdif(1,1)          ! jararias 2013/08/10

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
         endif
            swddir(i,j)    = 0.  ! jararias 2013/08/10
            swddni(i,j)    = 0.  ! jararias 2013/08/10
            swddif(i,j)    = 0.  ! jararias 2013/08/10
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
