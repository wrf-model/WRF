!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
! ------------------ Goddard Radiation Code   ---------------------------------
!
! Purpose : 
!   Drive Goddard radiative transfer to compute radiative heating rate and surface radiation.
!
!
! Method (Calling sequence) :
! goddardrad(preparation for radiative transfer)
!     |-- o3_profile (intepolate ozone profile)
!     |-- swrad(compute SW radiative transfer)
!     |     |-- sw_uvpar(compute SW radiative transfer for UV band)
!     |     |     |-cloud_scale(scale cloud optical thickness for diffuse/direct radiation)
!     |     |     |-delta_eddington(delta-eddington approximation for the bulk scattering properties)
!     |     |     |-twostream_adding(compute updown fluxes using two-stream adding method)
!     |     |-- sw_ir(compute SW radiative transfer for IR band)
!     |     |     |-cloud_scale(scale cloud optical thickness for diffuse/direct radiation)
!     |     |     |-delta_eddington(delta-eddington approximation for the bulk scattering properties)
!     |     |     |-twostream_adding(compute updown fluxes using two-stream adding method)
!     |     |-- rflx(compute the reduction of clear-sky downward solar flux)
!     |
!     |-- lwrad(compute LW radiative transfer)
!           |-- column(compute column-integrated amount of absorber)
!           |-- h2oexps(compute exponentials for water vapor line absorption)
!           |-- conexps(compute exponentials for continuum absorption)
!           |-- co2exps(compute co2 exponentials)
!           |-- ch4exps(compute ch4 exponentials)
!           |-- comexps(compute co2-minor exponentials)
!           |-- cfcexps(compute cfc exponentials)
!           |-- b10exps(compute band3a exponentials)
!           |-- tablup(compute water vapor, co2 and o3 transmittances)
!           |-- h2okdis(compute water vapor transmittance using kdistirubion method)
!           |-- co2kdis(compute co2 transmittances using k-distribution method)
!           |-- n2okdis(compute n2o transmittances using k-distribution method)
!           |-- ch4kdis(compute ch4 transmittances using k-distribution method)
!           |-- comkdis(compute co2-minor transmittances using k-distribution method)
!           |-- cfckdis(compute cfc transmittances using k-distribution method)
!           |-- b10kdis(compute band3a transmittances using k-distribution method)
!           |-- cldovlp(compute the fractional clear line-of-sight)
!
!
! History :
!   Aug 2007    , Toshi Matsui : One-dimensinalized swrad to skip swrad in night.
!   Jun 2007    , Toshi Matsui ; Revise broadband flux reduction to avoid negative SW heating.
!   May 2007    , Toshi Matsui ; Made driver to plug WRF, GOCCART module, and F90 revision.
!   Apr 2007    , Toshi Matsui ; SW radiation routine is optimized (add fast_overcast option for CRM).
!   1996 ~ 2002 , Ming-dah Chou and Max Suarez ; initial and misc changes
!
!
! Refferences :
!  Chou M.-D., and M. J. Suarez, 1999: A solar radiation parameterization for atmospheric studies. 
!      NASA Tech. Rep. NASA/TM-1999-10460, vol. 15, 38 pp
!  Chou M.-D., and M. J. Suarez, 2001: A thermal infrared radiation parameterization for atmospheric
!      studies. NASA/TM-2001-104606, vol. 19, 55pp
!  Matsui, T., W.-K. Tao, and J. J. Shi: 2007: Goddard Radiation and Aerosol Direct Effect in Goddard 
!      WRF, NASA/UMD WRF Meeting, Sep 14 2007.
!  Shi, J. J., W.-K. Tao, T. Matsui, R. Cifelli, A. Hou, S. Lang, A. Tokey, N.-Y. Wang, 
!      C. Peters-Lidard, G. Jackson, S. Rutledge, W. Petersen, 2010: WRF Simulations of the 
!      20-22 January 2007 Snow Events over Eastern Canada: Comparison with in-situ and Satellite 
!      Observations. J. Applied Meteor. Climatol, 49, 2246-2266.
!
!
!  Bug report: 
!      If you find any bug, please report to
!      Toshi Matsui @ NASA GSFC, email:toshihisa.matsui-1@nasa.gov
!      Jainn J. Shi @ NASA GSFC, email:jainn.j.shi@nasa.gov
!
!      We will report bugs to  
!      Ming-Dah Chou @ National Taiwa University mdchou@webmail.as.ntu.edu.tw 
!---------------------------------------------------------------------------------------------------------
 module module_ra_goddard

! encapsulation control
  private  !-> privatize all variables in this module excepting public parameter (subroutine) below. 
  public :: goddardrad  !only this subroutine can be called from other module

  integer,parameter :: fp_kind = selected_real_kind(15) !Real precision (6-single 15-double 20-quad)

!-------------------------------------------------------------------------------
!---------------------    User Modification  -----------------------------------    

! (Both SW and LW radiation option)
! there is an option of providing either cloud ice/water mixing ratio
!  (cwc) or optical thickness (taucld).  if the former is provided, set
!  cldwater=.true., and taucld is computed from cwc and reff as a
!  function of spectra band. otherwise, set cldwater=.false., and
!  specify taucld, independent of spectral band.
  
   logical,parameter :: cldwater = .true.  !ALWAYS  .true.

! (Both SW and LW radiation option)
! in a high spatial-resolution atmospheric model, fractional cloud cover
!  might be computed to be either 0 or 1.  in such a case, scaling of the
!  cloud optical thickness is not necessary, and the computation can be
!  made faster by setting overcast=.true.  otherwise, set the option
!  overcast=.false. (-> hardwire with WRF module)

   logical,parameter :: overcast = .true.  ! recommend true for CRM or LES 

! (SW radition option)
! For overcast (=true) option, this option (fast_overcast=.true.) skip the clear-sky
! two-stream radiative transfer computation, and only compute cloudy ski option. 
! Fclear/Fcloud is estimated from pre-computed look-up table for the below cloud
! flux reduction due to CO2 and O2.  So, this option make SW radiation faster 1.5 time.
! Difference in surface downwelling radiation betweeen true and false option is less than 1.W/m^2. 

   logical,parameter :: fast_overcast = .true. ! recommend true 

! (LW radiation option)
!  if high = .true., transmission functions in the co2, o3, and the
!  three water vapor bands with strong absorption are computed using
!  table look-up.  cooling rates are computed accurately from the
!  surface up to 0.01 mb. But slightly computationally expensive.
!
!  if high = .false., transmission functions in LW radiation are computed using the
!  k-distribution method with linear pressure scaling for all spectral
!  bands and gases.  cooling rates are not accurately calculated for
!  pressures less than 10 mb. But, the computationally slightly cheap. 

   logical,parameter :: high = .false.  !recommend false for CRM
                                        !recommend true if you include storatosphere (<10mb)

! (LW radiation option)
!   if trace = .true., absorption due to n2o, ch4, cfcs, and the
!   two minor co2 bands in the LW window region is included.
!   if trace = .false., absorption in those minor bands is neglected.

   logical,parameter :: trace = .true.  !recommend true 

   real(Kind=fp_kind),    parameter ::   co2    = 336.77e-6  ! co2 concentration [ppv] (Y1850 = 285.43e-6)
   real(Kind=fp_kind),    parameter ::   n2o    = 0.32e-6    ! n2o concentration [ppv] (Y1850 = 0.28e-6)
   real(Kind=fp_kind),    parameter ::   ch4    = 1.79e-6    ! ch4 concentration [ppv] (Y1850 = 0.86e-6)
   real(Kind=fp_kind),    parameter ::   cfc11  = 268.0e-12  ! cfc11 concentration [ppv] (Y1850 = 0.)
   real(Kind=fp_kind),    parameter ::   cfc12  = 503.0e-12  ! cfc12 concentration [ppv] (Y1850 = 0.)
   real(Kind=fp_kind),    parameter ::   cfc22  = 105.0e-12  ! cfc22 concentration [ppv] (Y1850 = 0.)

! some tuning parameters
   real(Kind=fp_kind),    parameter ::   re = 10.    ! cloud droplet effective radius [micron]

!---------------------   End of User Modification  -----------------------------
!-------------------------------------------------------------------------------

! number of radiation bands
   integer, parameter :: ib_sw = 11  !number of shortwave band
   integer, parameter :: ib_lw = 10  !number of longwave band 

! Threshold values 
!  - Do not change. These values are the lowest threshold, while avoiding numerical instability. 
   real(Kind=fp_kind),    parameter ::   cosz_min = 0.0001 ! threshold of minimum cosizne of solar zenith angle for SW rad
   real(Kind=fp_kind),    parameter ::   fcld_min = 0.01   ! threshold of minimum cloud fraction for account clooud
   real(Kind=fp_kind),    parameter ::   taux_min = 0.0001 ! threshold of minimum optical depth for accouting cloud (0.02) 
   real(Kind=fp_kind),    parameter ::   opt_min  = 1.e-6  ! threshold of optical properties to avoid numerical instability. 

   integer :: jloop
contains

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

   subroutine goddardrad( rthraten, gsf, xlat,xlong               &
                   ,dz8w,t8w,rho_phy,sw_or_lw                     &
                   ,alb,emiss,t3d,qv3d,qc3d,qr3d                  &
                   ,qi3d,qs3d,qg3d                                &
                   ,p3d,p8w3d,pi3d,cldfra3d                       &
                   ,gmt,cp,g,julday,xtime,declin,solcon           &
                   ,center_lat                                    &
                   ,radfrq,degrad,taucldi,taucldc,warm_rain       &
                   ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg                 &
                   ,ids,ide, jds,jde, kds,kde                     & 
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     & 
!                   ,cosz_urb2d,omg_urb2d                          & !optional urban
                   ,ERBE_out                                      &  !optional sdsu
                   ,tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw           & ! jararias 2013/11
                   ,swddir,swddni,swddif                          & ! jararias 2013/08
                   ,coszen,julian                                 & ! jararias 2013/08
                   ,aer_opt                                       &
                                                                  )
   implicit none

!------- I / O variables ----------------------------------------------

 character(len=2), intent(in) :: &
   sw_or_lw   ! 'sw'=shortwave radiation,'lw'=longwave radiation,'is'=ISCCP simulator

 integer,    intent(in) :: &
    ids, &    ! start index for i in domain
    ide, &    ! end index for i in domain
    jds, &    ! start index for j in domain
    jde, &    ! end index for j in domain
    kds, &    ! start index for k in domain
    kde, &    ! end index for k in domain 
    ims, &    ! start index for i in memory
    ime, &    ! end index for i in memory
    jms, &    ! start index for j in memory
    jme, &    ! end index for j in memory
    kms, &    ! start index for k in memory
    kme, &    ! end index for k in memory
    its, &    ! start index for i in tile
    ite, &    ! end index for i in tile
    jts, &    ! start index for j in tile
    jte, &    ! end index for j in tile
    kts, &    ! start index for k in tile
    kte, &    ! end index for k in tile
   julday     ! integer of julian date  

 logical,    intent(in) ::  &
   warm_rain  ! logics for warm_rain only or not

 real, intent(in)      ::  &
   radfrq, &  ! time for calling radiation (min)                
   degrad, &  ! conversion factor for degrees to radians (pi/180.) (rad/deg)
   xtime,  &  ! time since simulation start (min)
   declin, &  ! solar declination angle (rad)
   solcon, &  ! solar constant (W/m^2) 
      gmt, &  ! greenwich mean time hour of model start (hour)
       cp, &  ! heat capacity at constant pressure for dry air (J/kg/K)
        g     ! acceleration due to gravity (m/s^2)

 real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
       p3d, &  ! pressure (Pa) 
     p8w3d, &  ! pressure at full levels (Pa)
      pi3d, &  ! exner function (-)
       t3d, &  ! temperature (K)
       t8w, &  ! temperature at full levels (K)
      dz8w, &  ! dz between full levels (m)
   rho_phy, &  ! density (kg/m^3)
  cldfra3d     ! cloud fraction (0-1)

 real, dimension( ims:ime, jms:jme ), intent(in)  :: &
       xlat, &  ! latitude, south is negative (degree)
      xlong, &  ! longitude, west is negative (degree)
        alb, &  ! broadband surface albedo (between 0 and 1)
       emiss    ! broadband surface emissivity (between 0 and 1)

 real, dimension( ims:ime, kms:kme, jms:jme ),optional,intent(in) :: &
     qv3d,   & ! water vapor mixing ratio (kg/kg)  (in)
     qc3d,   & ! cloud water mixing ratio (kg/kg)  (in)
     qr3d,   & ! rain water mixing ratio (kg/kg)   (in)
     qi3d,   & ! cloud ice mixing ratio (kg/kg)    (in)
     qs3d,   & ! snow mixing ratio (kg/kg)         (in)
     qg3d      ! graupel mixing ration (kg/kg)     (in)

 logical, optional, intent(in )  ::  f_qv,f_qc,f_qr,f_qi,f_qs,f_qg  !hydrometeor flags 
!
 real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout)  :: &
  rthraten, &  ! theta tendency due to radiative heating (K/sec)
   taucldi, &  ! ice cloud optical thickness for visible broadband
   taucldc     ! liquid cloud optical thickness for visible braodband

 real, dimension( ims:ime, jms:jme ), intent(inout)  ::  &
         gsf    ! (for SW) : net short wave flux at ground surface (W/m^2) 
                ! (for LW) : downward long wave flux at ground surface (W/m^2)
! 
! Extra 3D variables (last dimension 1-TOA LW down, 2-TOA LW up, 3-surface LW down, 4-surface LW up)
!                                    5-TOA SW down, 6-TOA SW up, 7-surface SW down, 8-surface SW up)

   real, optional,dimension(ims:ime, jms:jme, 1:8),intent(out) :: ERBE_out  !extra output for SDSU

! jararias, 14/08/2013
   real, optional, dimension(ims:ime, jms:jme), intent(out) :: &
         swddir, & ! All-sky broadband surface direct horizontal irradiance
         swddni, & ! All-sky broadband surface direct normal irradiance
         swddif    ! All-sky braodband surface diffuse irradiance
   real, optional, dimension(ims:ime, jms:jme), intent(in) :: &
         coszen    ! cosine of the solar zenith angle
   real, optional, intent(in) :: &
         julian    ! julian day (1-365) ! jararias, 14/08/2013

! jararias, 2013/11
!  real, dimension( ims:ime, kms:kme, jms:jme, ib_sw ), optional, intent(in) :: &
!        tauaer3d_sw, & ! 3D aerosol optical depth for SW bands
!        ssaaer3d_sw, & ! 3D single scattering albedo for SW bands
!        asyaer3d_sw    ! 3D asymmetry factor for SW bands
   real, dimension(:,:,:,:), pointer :: tauaer3d_sw,ssaaer3d_sw,asyaer3d_sw
   integer, intent(in) :: aer_opt

!------- Local variables ----------------------------------------------

 integer :: i,j,k,nk,ib,is ! loop indice
 integer :: mix, mkx !maximum dimension for i and k


 real(Kind=fp_kind), dimension( its:ite ) :: &
    cosz, &  ! cosine of solar zenith angle (0~1)
  rsuvbm, &  ! surface albedo for direct UV-VIS radiation
  rsuvdf, &  ! surface albedo for diffuse UV-VIS radiation
  rsirbm, &  ! surface albedo for direct NIR radiation
  rsirdf, &  ! surface albedo for diffuse NIR radiation
    tsfc, &  ! surface air temperature (K)
   tskin, &  ! surface skin (radiative) temperature (K)
    p400, &  ! pressure criteria for upper
    p700     ! pressure criteir for middle

 real(Kind=fp_kind), dimension( its:ite, ib_lw ) :: emis2d !emissivity

 integer, dimension( its:ite ) ::  &
   ict, & ! 400mb level indice
   icb    ! 700mb level indice

 real(Kind=fp_kind), dimension( its:ite, kts-1:kte+1 )  :: &
   flx, & !flux fraction (-) or actual flux (W/m^2) 
  flxd, & !donwelling flux fraction [-] (for shortwave) , but actual flux [W/m2] (for longwave)
  flxu    !upwelling flux fraction [-] (for shortwave) , but actual flux [W/m2] (for longwave) 


 real(Kind=fp_kind), dimension( its:ite, kts-1:kte ) :: o3  !ozone profile

 real(Kind=fp_kind), dimension( its:ite, kts-1:kte, ib_sw ) :: &
  taual_sw, &  ! aerosol optical depth for SW bands
  ssaal_sw, &  ! aerosol single scattering albedo for SW bands
  asyal_sw     ! aerosol asymetry factor for SW bands

 real(Kind=fp_kind), dimension( its:ite, kts-1:kte, ib_lw ) ::  &
  taual_lw, &  ! aerosol optical depth for LW bands
  ssaal_lw, &  ! aerosol single scattering albedo for LW bands
  asyal_lw     ! aerosol asymetry factor for LW bands

 real(Kind=fp_kind), dimension( its:ite, kts-1:kte, 3 ) :: &  !1-ice cloud, 2-liquid cloud, 3-rain
      reff, &  !particle effective size (micron)
       cwc     !hydrometer mixing ratio (kg/kg) or (g/g) 

 real(Kind=fp_kind), dimension( its: ite, kts-1:kte+1 ) :: &
     p8w2d, &  ! pressure in full level (mb)
     t8w2d     ! temperature in full level (K)

 real(Kind=fp_kind), dimension( its: ite, kts-1:kte ) ::   &
     tten2d, &  ! temperature tendency (K/sec)
       sh2d, &  ! specific humidity (-)
        p2d, &  ! pressure (mb)
        t2d, &  ! temperature (K)
       dz2d, &  ! layer depth
     fcld2d     ! cloud fraction

   real(Kind=fp_kind), dimension( its:ite , kts:kte+1 ) :: phyd    ! pressure (Pa)
   real(Kind=fp_kind), dimension( its:ite , kts:kte   ) :: phydmid ! pressure in middle (Pa)

  ! jararias, 14/08/2013
  real(Kind=fp_kind), dimension( its:ite ) :: &
    fdiruv, & ! SW UV direct downwelling flux at surface (-)
    fdifuv, & ! SW UV diffuse downwelling flux at surface (-)
   fdirpar, & ! SW PAR direct downwelling flux at surface (-)
   fdifpar, & ! SW PAR diffuse downwelling flux at surface (-)
    fdirir, & ! SW IF direct downwelling flux at surface (-)
    fdifir    ! SW IF diffuse downwelling flux at surface (-)
  real(Kind=fp_kind) :: da,eot

!
! ozone table
!
   integer,    parameter    ::  np    = 75
   real(Kind=fp_kind), dimension( np, 5 ) ::  pres, ozone
   real(Kind=fp_kind), dimension( np )    ::  p

  integer  ::  iprof !index for porfile
  integer,parameter :: is_summer = 80 !summer start indice 
  integer,parameter :: ie_summer = 265 !summer end indice
!
   real(Kind=fp_kind)    :: fac,x
   real(Kind=fp_kind)    :: xt24,tloctm,hrang,xxlat
   real, intent(in)    :: center_lat                ! center of latitude

!urban
!   real, optional, dimension( ims:ime, jms:jme ), intent(out)  :: cosz_urb2d !urban
!   real, optional, dimension( ims:ime, jms:jme ), intent(out)  :: omg_urb2d  !urban

!--------------------------------------------------------------------------------
!   data set 1
!     mid-latitude summer (75 levels) :  p(mb)  o3(g/g)
!     surface temp = 294.0
!
      data (pres(i,1),i=1,np)/ &
          0.0006244,   0.0008759,   0.0012286,   0.0017234,   0.0024174, &
          0.0033909,   0.0047565,   0.0066720,   0.0093589,   0.0131278, &
          0.0184145,   0.0258302,   0.0362323,   0.0508234,   0.0712906, &
          0.1000000,   0.1402710,   0.1967600,   0.2759970,   0.3871430, &
          0.5430,    0.7617,    1.0685,    1.4988,    2.1024,    2.9490, &
          4.1366,    5.8025,    8.1392,   11.4170,   16.0147,   22.4640, &
         31.5105,   44.2001,   62.0000,   85.7750,  109.5500,  133.3250, &
        157.1000,  180.8750,  204.6500,  228.4250,  252.2000,  275.9750, &
        299.7500,  323.5250,  347.3000,  371.0750,  394.8500,  418.6250, &
        442.4000,  466.1750,  489.9500,  513.7250,  537.5000,  561.2750, &
        585.0500,  608.8250,  632.6000,  656.3750,  680.1500,  703.9250, &
        727.7000,  751.4750,  775.2500,  799.0250,  822.8000,  846.5750, &
        870.3500,  894.1250,  917.9000,  941.6750,  965.4500,  989.2250, &
       1013.0000/
!
      data (ozone(i,1),i=1,np)/ &
        0.1793e-06,  0.2228e-06,  0.2665e-06,  0.3104e-06,  0.3545e-06, &
        0.3989e-06,  0.4435e-06,  0.4883e-06,  0.5333e-06,  0.5786e-06, &
        0.6241e-06,  0.6698e-06,  0.7157e-06,  0.7622e-06,  0.8557e-06, &
        0.1150e-05,  0.1462e-05,  0.1793e-05,  0.2143e-05,  0.2512e-05, &
        0.2902e-05,  0.3313e-05,  0.4016e-05,  0.5193e-05,  0.6698e-05, &
        0.8483e-05,  0.9378e-05,  0.9792e-05,  0.1002e-04,  0.1014e-04, &
        0.9312e-05,  0.7834e-05,  0.6448e-05,  0.5159e-05,  0.3390e-05, &
        0.1937e-05,  0.1205e-05,  0.8778e-06,  0.6935e-06,  0.5112e-06, &
        0.3877e-06,  0.3262e-06,  0.2770e-06,  0.2266e-06,  0.2020e-06, &
        0.1845e-06,  0.1679e-06,  0.1519e-06,  0.1415e-06,  0.1317e-06, &
        0.1225e-06,  0.1137e-06,  0.1055e-06,  0.1001e-06,  0.9487e-07, &
        0.9016e-07,  0.8641e-07,  0.8276e-07,  0.7930e-07,  0.7635e-07, &
        0.7347e-07,  0.7065e-07,  0.6821e-07,  0.6593e-07,  0.6368e-07, &
        0.6148e-07,  0.5998e-07,  0.5859e-07,  0.5720e-07,  0.5582e-07, &
        0.5457e-07,  0.5339e-07,  0.5224e-07,  0.5110e-07,  0.4999e-07/

!--------------------------------------------------------------------------------
!   data set 2
!   mid-latitude winter (75 levels) :  p(mb)  o3(g/g)
!   surface temp = 272.2
!
      data (pres(i,2),i=1,np)/ &
          0.0006244,   0.0008759,   0.0012286,   0.0017234,   0.0024174, &
          0.0033909,   0.0047565,   0.0066720,   0.0093589,   0.0131278, &
          0.0184145,   0.0258302,   0.0362323,   0.0508234,   0.0712906, &
          0.1000000,   0.1402710,   0.1967600,   0.2759970,   0.3871430, &
          0.5430,    0.7617,    1.0685,    1.4988,    2.1024,    2.9490, &
          4.1366,    5.8025,    8.1392,   11.4170,   16.0147,   22.4640, &
         31.5105,   44.2001,   62.0000,   85.9000,  109.8000,  133.7000, &
        157.6000,  181.5000,  205.4000,  229.3000,  253.2000,  277.1000, &
        301.0000,  324.9000,  348.8000,  372.7000,  396.6000,  420.5000, &
        444.4000,  468.3000,  492.2000,  516.1000,  540.0000,  563.9000, &
        587.8000,  611.7000,  635.6000,  659.5000,  683.4000,  707.3000, &
        731.2000,  755.1000,  779.0000,  802.9000,  826.8000,  850.7000, &
        874.6000,  898.5000,  922.4000,  946.3000,  970.2000,  994.1000, &
       1018.0000/
!
      data (ozone(i,2),i=1,np)/ &
        0.2353e-06,  0.3054e-06,  0.3771e-06,  0.4498e-06,  0.5236e-06, &
        0.5984e-06,  0.6742e-06,  0.7511e-06,  0.8290e-06,  0.9080e-06, &
        0.9881e-06,  0.1069e-05,  0.1152e-05,  0.1319e-05,  0.1725e-05, &
        0.2145e-05,  0.2581e-05,  0.3031e-05,  0.3497e-05,  0.3980e-05, &
        0.4478e-05,  0.5300e-05,  0.6725e-05,  0.8415e-05,  0.1035e-04, &
        0.1141e-04,  0.1155e-04,  0.1143e-04,  0.1093e-04,  0.1060e-04, &
        0.9720e-05,  0.8849e-05,  0.7424e-05,  0.6023e-05,  0.4310e-05, &
        0.2820e-05,  0.1990e-05,  0.1518e-05,  0.1206e-05,  0.9370e-06, &
        0.7177e-06,  0.5450e-06,  0.4131e-06,  0.3277e-06,  0.2563e-06, &
        0.2120e-06,  0.1711e-06,  0.1524e-06,  0.1344e-06,  0.1199e-06, &
        0.1066e-06,  0.9516e-07,  0.8858e-07,  0.8219e-07,  0.7598e-07, &
        0.6992e-07,  0.6403e-07,  0.5887e-07,  0.5712e-07,  0.5540e-07, &
        0.5370e-07,  0.5214e-07,  0.5069e-07,  0.4926e-07,  0.4785e-07, &
        0.4713e-07,  0.4694e-07,  0.4676e-07,  0.4658e-07,  0.4641e-07, &
        0.4634e-07,  0.4627e-07,  0.4619e-07,  0.4612e-07,  0.4605e-07/


!--------------------------------------------------------------------------------
!   data set 3
!   sub-arctic summer (75 levels) :  p(mb)  o3(g/g)
!   surface temp = 287.0
!
      data (pres(i,3),i=1,np)/ &
          0.0006244,   0.0008759,   0.0012286,   0.0017234,   0.0024174, &
          0.0033909,   0.0047565,   0.0066720,   0.0093589,   0.0131278, &
          0.0184145,   0.0258302,   0.0362323,   0.0508234,   0.0712906, &
          0.1000000,   0.1402710,   0.1967600,   0.2759970,   0.3871430, &
          0.5430,    0.7617,    1.0685,    1.4988,    2.1024,    2.9490, &
          4.1366,    5.8025,    8.1392,   11.4170,   16.0147,   22.4640, &
         31.5105,   44.2001,   62.0000,   85.7000,  109.4000,  133.1000, &
        156.8000,  180.5000,  204.2000,  227.9000,  251.6000,  275.3000, &
        299.0000,  322.7000,  346.4000,  370.1000,  393.8000,  417.5000, &
        441.2000,  464.9000,  488.6000,  512.3000,  536.0000,  559.7000, &
        583.4000,  607.1000,  630.8000,  654.5000,  678.2000,  701.9000, &
        725.6000,  749.3000,  773.0000,  796.7000,  820.4000,  844.1000, &
        867.8000,  891.5000,  915.2000,  938.9000,  962.6000,  986.3000, &
       1010.0000/
!
      data (ozone(i,3),i=1,np)/ &
        0.1728e-06,  0.2131e-06,  0.2537e-06,  0.2944e-06,  0.3353e-06, &
        0.3764e-06,  0.4176e-06,  0.4590e-06,  0.5006e-06,  0.5423e-06, &
        0.5842e-06,  0.6263e-06,  0.6685e-06,  0.7112e-06,  0.7631e-06, &
        0.1040e-05,  0.1340e-05,  0.1660e-05,  0.2001e-05,  0.2362e-05, &
        0.2746e-05,  0.3153e-05,  0.3762e-05,  0.4988e-05,  0.6518e-05, &
        0.8352e-05,  0.9328e-05,  0.9731e-05,  0.8985e-05,  0.7632e-05, &
        0.6814e-05,  0.6384e-05,  0.5718e-05,  0.4728e-05,  0.4136e-05, &
        0.3033e-05,  0.2000e-05,  0.1486e-05,  0.1121e-05,  0.8680e-06, &
        0.6474e-06,  0.5164e-06,  0.3921e-06,  0.2996e-06,  0.2562e-06, &
        0.2139e-06,  0.1723e-06,  0.1460e-06,  0.1360e-06,  0.1267e-06, &
        0.1189e-06,  0.1114e-06,  0.1040e-06,  0.9678e-07,  0.8969e-07, &
        0.8468e-07,  0.8025e-07,  0.7590e-07,  0.7250e-07,  0.6969e-07, &
        0.6694e-07,  0.6429e-07,  0.6208e-07,  0.5991e-07,  0.5778e-07, &
        0.5575e-07,  0.5403e-07,  0.5233e-07,  0.5067e-07,  0.4904e-07, &
        0.4721e-07,  0.4535e-07,  0.4353e-07,  0.4173e-07,  0.3997e-07/


!--------------------------------------------------------------------------------
!   data set 3
!   sub-arctic winter (75 levels) :   p(mb)  o3(g/g)
!   surface temp = 257.1
!
      data (pres(i,4),i=1,np)/ &
          0.0006244,   0.0008759,   0.0012286,   0.0017234,   0.0024174, &
          0.0033909,   0.0047565,   0.0066720,   0.0093589,   0.0131278, &
          0.0184145,   0.0258302,   0.0362323,   0.0508234,   0.0712906, &
          0.1000000,   0.1402710,   0.1967600,   0.2759970,   0.3871430, &
          0.5430,    0.7617,    1.0685,    1.4988,    2.1024,    2.9490, &
          4.1366,    5.8025,    8.1392,   11.4170,   16.0147,   22.4640, &
         31.5105,   44.2001,   62.0000,   85.7750,  109.5500,  133.3250, &
        157.1000,  180.8750,  204.6500,  228.4250,  252.2000,  275.9750, &
        299.7500,  323.5250,  347.3000,  371.0750,  394.8500,  418.6250, &
        442.4000,  466.1750,  489.9500,  513.7250,  537.5000,  561.2750, &
        585.0500,  608.8250,  632.6000,  656.3750,  680.1500,  703.9250, &
        727.7000,  751.4750,  775.2500,  799.0250,  822.8000,  846.5750, &
        870.3500,  894.1250,  917.9000,  941.6750,  965.4500,  989.2250, &
       1013.0000/
!
      data (ozone(i,4),i=1,np)/ &
        0.2683e-06,  0.3562e-06,  0.4464e-06,  0.5387e-06,  0.6333e-06, &
        0.7301e-06,  0.8291e-06,  0.9306e-06,  0.1034e-05,  0.1140e-05, &
        0.1249e-05,  0.1360e-05,  0.1474e-05,  0.1855e-05,  0.2357e-05, &
        0.2866e-05,  0.3383e-05,  0.3906e-05,  0.4437e-05,  0.4975e-05, &
        0.5513e-05,  0.6815e-05,  0.8157e-05,  0.1008e-04,  0.1200e-04, &
        0.1242e-04,  0.1250e-04,  0.1157e-04,  0.1010e-04,  0.9063e-05, &
        0.8836e-05,  0.8632e-05,  0.8391e-05,  0.7224e-05,  0.6054e-05, &
        0.4503e-05,  0.3204e-05,  0.2278e-05,  0.1833e-05,  0.1433e-05, &
        0.9996e-06,  0.7440e-06,  0.5471e-06,  0.3944e-06,  0.2852e-06, &
        0.1977e-06,  0.1559e-06,  0.1333e-06,  0.1126e-06,  0.9441e-07, &
        0.7678e-07,  0.7054e-07,  0.6684e-07,  0.6323e-07,  0.6028e-07, &
        0.5746e-07,  0.5468e-07,  0.5227e-07,  0.5006e-07,  0.4789e-07, &
        0.4576e-07,  0.4402e-07,  0.4230e-07,  0.4062e-07,  0.3897e-07, &
        0.3793e-07,  0.3697e-07,  0.3602e-07,  0.3506e-07,  0.3413e-07, &
        0.3326e-07,  0.3239e-07,  0.3153e-07,  0.3069e-07,  0.2987e-07/ 

!--------------------------------------------------------------------------------
!   data set 4
!   tropical (75 levels) :   p(mb)  o3(g/g)
!   surface temp = 300.0
!
      data (pres(i,5),i=1,np)/ &
          0.0006244,   0.0008759,   0.0012286,   0.0017234,   0.0024174, &
          0.0033909,   0.0047565,   0.0066720,   0.0093589,   0.0131278, &
          0.0184145,   0.0258302,   0.0362323,   0.0508234,   0.0712906, &
          0.1000000,   0.1402710,   0.1967600,   0.2759970,   0.3871430, &
          0.5430,    0.7617,    1.0685,    1.4988,    2.1024,    2.9490, &
          4.1366,    5.8025,    8.1392,   11.4170,   16.0147,   22.4640, &
         31.5105,   44.2001,   62.0000,   85.7750,  109.5500,  133.3250, &
        157.1000,  180.8750,  204.6500,  228.4250,  252.2000,  275.9750, &
        299.7500,  323.5250,  347.3000,  371.0750,  394.8500,  418.6250, &
        442.4000,  466.1750,  489.9500,  513.7250,  537.5000,  561.2750, &
        585.0500,  608.8250,  632.6000,  656.3750,  680.1500,  703.9250, &
        727.7000,  751.4750,  775.2500,  799.0250,  822.8000,  846.5750, &
        870.3500,  894.1250,  917.9000,  941.6750,  965.4500,  989.2250, &
       1013.0000/
!
      data (ozone(i,5),i=1,np)/ &
        0.1993e-06,  0.2521e-06,  0.3051e-06,  0.3585e-06,  0.4121e-06, &
        0.4661e-06,  0.5203e-06,  0.5748e-06,  0.6296e-06,  0.6847e-06, &
        0.7402e-06,  0.7959e-06,  0.8519e-06,  0.9096e-06,  0.1125e-05, &
        0.1450e-05,  0.1794e-05,  0.2156e-05,  0.2538e-05,  0.2939e-05, &
        0.3362e-05,  0.3785e-05,  0.4753e-05,  0.6005e-05,  0.7804e-05, &
        0.9635e-05,  0.1023e-04,  0.1067e-04,  0.1177e-04,  0.1290e-04, &
        0.1134e-04,  0.9223e-05,  0.6667e-05,  0.3644e-05,  0.1545e-05, &
        0.5355e-06,  0.2523e-06,  0.2062e-06,  0.1734e-06,  0.1548e-06, &
        0.1360e-06,  0.1204e-06,  0.1074e-06,  0.9707e-07,  0.8960e-07, &
        0.8419e-07,  0.7962e-07,  0.7542e-07,  0.7290e-07,  0.7109e-07, &
        0.6940e-07,  0.6786e-07,  0.6635e-07,  0.6500e-07,  0.6370e-07, &
        0.6244e-07,  0.6132e-07,  0.6022e-07,  0.5914e-07,  0.5884e-07, &
        0.5855e-07,  0.5823e-07,  0.5772e-07,  0.5703e-07,  0.5635e-07, &
        0.5570e-07,  0.5492e-07,  0.5412e-07,  0.5335e-07,  0.5260e-07, &
        0.5167e-07,  0.5063e-07,  0.4961e-07,  0.4860e-07,  0.4761e-07/

!--------------------------------------------------------------------------------
  mix = ite-its+1  ! maximum dimension for i
  mkx = kte-kts+1  ! maximum dimension for k


! testing, need to change iprof, which is function of lat and julian day
!  iprof = 1  :  mid-latitude summer profile
!        = 2  :  mid-latitude winter profile
!        = 3  :  sub-arctic   summer profile
!        = 4  :  sub-arctic   winter profile
!        = 5  :  tropical profile

!toshi
!   center_lat = xlat ( mix/2, (jte-jts+1)/2 )

   if (abs(center_lat) .le. 30. ) then ! tropic
      iprof = 5
   else
      if (center_lat .gt.  0.) then
         if (center_lat .gt. 60. ) then !  arctic
            if (julday .gt. is_summer .and. julday .lt. ie_summer ) then
               ! arctic summer
               iprof = 3
            else
               ! arctic winter
               iprof = 4
            endif
         else        ! midlatitude
            if (julday .gt. is_summer .and. julday .lt. ie_summer ) then
               ! north midlatitude summer
               iprof = 1
            else
               ! north midlatitude winter
               iprof = 2
            endif
         endif

      else
         if (center_lat .lt. -60. ) then !  antarctic
            if (julday .lt. is_summer .or. julday .gt. ie_summer ) then
               ! antarctic summer
               iprof = 3
            else
               ! antarctic winter
               iprof = 4
            endif
         else        ! midlatitude
            if (julday .lt. is_summer .or. julday .gt. ie_summer ) then
               ! south midlatitude summer
               iprof = 1
            else
               ! south midlatitude winter
               iprof = 2
            endif
         endif

      endif
   endif

   j_loop: do j=jts,jte
     jloop=j 
!-----------------------------------------------------------------------------

      do k=1,np
         p(k)=pres(k,iprof)
      enddo

      do k = kts,kte+1
         do i = its,ite
            if(k.eq.kts)then
              phyd(i,k)=p8w3d(i,kts,j)
            else
             ! phyd(i,k)=phyd(i,k-1) - g*rho_phy(i,k-1,j)*dz8w(i,k-1,j) !keep this old one consistent to IDS ctl run.
              phyd(i,k)=phyd(i,k-1)*exp( -9.8/287.04*dz8w(i,k-1,j)/t3d(i,k-1,j) ) !suggested by K. Suzuki

              phydmid(i,k-1)=0.5*(phyd(i,k-1)+phyd(i,k))

            endif
         enddo
      enddo

!
! reverse vars 
!
      do k=kts,kte+1
         do i=its,ite
            nk=kme-k+kms
            p8w2d(i,k)=phyd(i,nk)*0.01   ! p8w2d is in mb
            t8w2d(i,k) = t8w(i,nk,j)
         enddo
      enddo

      do i=its,ite
         p8w2d(i,0)=.0
      enddo

!
      do k=kts,kte
         do i=its,ite
            nk=kme-1-k+kms
            tten2d(i,k)=0.
            t2d(i,k)=t3d(i,nk,j)
            sh2d(i,k)=qv3d(i,nk,j)/(1.+qv3d(i,nk,j))
            sh2d(i,k)=max(0._fp_kind,sh2d(i,k))
            p2d(i,k)=phydmid(i,nk)*0.01      ! p2d is in mb
            fcld2d(i,k)=cldfra3d(i,nk,j)
            dz2d(i,k)  = dz8w(i,nk,j)
         enddo
      enddo


! this logic is tortured because cannot test f_qi unless
! it is present, and order of evaluation of expressions
! is not specified in fortran
!      if (.not. warm_rain .and. .not. present ( f_qi ) ) then
!        do k=kts,kte
!         do i=its,ite
!            if (t2d(i,k) .lt. 273.15) then
!               cwc(i,k,1)=cwc(i,k,2)  ! ice cloud water mixing ratio
!               cwc(i,k,2)=0.          ! liquid cloud water mixing ratio
!            endif
!         enddo
!         enddo
!      endif

!
! intilize TOA value -> 0. 
!
      do i=its,ite
         tten2d(i,0)=0.
         t2d(i,0)=t2d(i,1)
         sh2d(i,0)=0.5*sh2d(i,1)
         dz2d(i,0)=dz2d(i,1)
         cwc(i,0,3)=0.
         cwc(i,0,2)=0.
         cwc(i,0,1)=0.
         p2d(i,0)=0.5*(p8w2d(i,0)+p8w2d(i,1))
         fcld2d(i,0)=0.
      enddo

!
! vertical profiles for ozone
!
      call o3_profile (np, p, ozone(1,iprof), its, ite, kts-1, kte, p2d, o3)

!
! cloud water mixing ratio
!
      if ( present( f_qc ) .and. present( qc3d)  ) then
         do k=kts,kte
            do i=its,ite
               nk=kme-1-k+kms
               if ( (f_qc) ) then
                  cwc(i,k,2)=max(0.,qc3d(i,nk,j))
                  reff(i,k,2) = re !fixed value
                  x=1.02*10000.*( p8w2d(i,k+1)-p8w2d(i,k) )
                  taucldc(i,nk,j) = x * cwc(i,k,2) * ( 3.33e-4 + 2.52/reff(i,k,2) ) !output purpose
               else
                  cwc(i,k,2) = 0.
                  reff(i,k,2) =0.
                  taucldc(i,nk,j) = 0.
               endif
            enddo
         enddo
      endif

!
! cloud ice mixing ratio (currently adding the snow.)
!
      if ( present( f_qi ) .and. present( qi3d)  ) then ! ice
         do k=kts,kte          
            do i=its,ite          
               nk=kme-1-k+kms
	       if ( (f_qi) ) then
                  cwc(i,k,1)=max(0.,qi3d(i,nk,j))
                  reff(i,k,1) = 125. + (t2d(i,k)-243.16)*5.  ! ice effective radius depends on temp
                  reff(i,k,1) = min(125._fp_kind,max(25._fp_kind,reff(i,k,1)))
                  x=1.02*10000.*( p8w2d(i,k+1)-p8w2d(i,k) )
                  taucldi(i,nk,j) = x * cwc(i,k,1) * ( -6.59e-3 + 1.65/reff(i,k,1) ) ! output purpose
               else
                  cwc(i,k,1)= 0.
                  reff(i,k,1) = 0.
                  taucldi(i,nk,j) = 0.
               endif
            enddo
         enddo
      endif

      if ( present( f_qs ) .and. present( qs3d )  ) then ! snow
         do k=kts,kte
            do i=its,ite
               nk=kme-1-k+kms
               if ( (f_qs) ) then
                  if ( cwc(i,k,1) > 0. ) then !ice exist
                    cwc(i,k,1)=cwc(i,k,1) + max(0.,qs3d(i,nk,j)) ! ice + snow
                    reff(i,k,1) = 125. + (t2d(i,k)-243.16)*5.    ! ice + snow (use ice definition) 
                    reff(i,k,1) = min(125._fp_kind,max(25._fp_kind,reff(i,k,1)))
                  else
                    cwc(i,k,1)= max(0.,qs3d(i,nk,j))  ! snow only (define snow is large aggrefated ice)
                    reff(i,k,1) = 125.                ! snow only (use largest reff of ice)
                  endif 
                  x=1.02*10000.*( p8w2d(i,k+1)-p8w2d(i,k) )
                  taucldi(i,nk,j) = x * cwc(i,k,1) * ( -6.59e-3 + 1.65/reff(i,k,1) ) ! output purpose
               else
                  ! cwc(i,k,1)=cwc(i,k,1)
               endif
            enddo
         enddo
      endif

!
! rain mixing ratio (currently adding graupel here, since it's size are much larger than ice and snow)
!

      if ( present( f_qr ) .and. present( qr3d)  ) then  ! rain
         do k=kts,kte
            do i=its,ite
               nk=kme-1-k+kms
               if ( (f_qr) ) then
                   cwc(i,k,3)  = max(0.,qr3d(i,nk,j))
               else
                   cwc(i,k,3)  = 0.
               endif
            enddo
         enddo
      endif


      if ( present( f_qg ) .and. present( qg3d)  ) then ! graupel (-> change)
         do k=kts,kte
            do i=its,ite
               nk=kme-1-k+kms
               if ( (f_qg) ) then
                  if( cwc(i,k,3) > 0.) then
                    cwc(i,k,3)=cwc(i,k,3) + max(0.,qg3d(i,nk,j))
                  else
                    cwc(i,k,3)= max(0.,qg3d(i,nk,j))
                  endif
               else
                  cwc(i,k,3)=cwc(i,k,3)
               endif
            enddo
         enddo
      endif

!
! Vertical profiles for effective particle size (old)
!
!      do k = kts-1, kte
!      do i = its, ite
!         reff(i,k,1) = 80.
!         reff(i,k,2) = 10.
!         reff(i,k,3) = 500.  !dummy not used
!      end do
!      end do

!
! filter min and max value of effective size 
!
   do k = kts-1, kte
      do i = its, ite
         reff(i,k,1) = max( min(reff(i,k,1),125._fp_kind) ,   25._fp_kind) !ice
         reff(i,k,2) = max( min(reff(i,k,2), 20._fp_kind) ,    4._fp_kind) !liquid 
         reff(i,k,3) = max( min(reff(i,k,3), 60._fp_kind) , 1800._fp_kind) !rain (dummy not used)
      end do
   end do

!
! level indices separating high, middle and low clouds
!
      do i = its, ite
         p400(i) = 1.e5
         p700(i) = 1.e5
      enddo

      do k = kts-1,kte+1
         do i = its, ite
            if (abs(p8w2d(i,k) - 400.) .lt. p400(i)) then
               p400(i) = abs(p8w2d(i,k) - 400.)
               ict(i) = k
            endif
            if (abs(p8w2d(i,k) - 700.) .lt. p700(i)) then
               p700(i) = abs(p8w2d(i,k) - 700.)
               icb(i) = k
            endif
        end do
      end do


 rad_select: select case(sw_or_lw)

! ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP
! ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP
! ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP ISCCP

     case('is') 

! in case of isccp simulation, all we need is cloud optical depth. So return.

! SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW 
! SW SW SW SW SW SW SW SW  Shortwave scheme SW SW SW SW SW SW SW SW SW 
! SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW SW 
     
      case ('sw')

!
! solar zenith angle and surface albedo
!
      if (present(coszen)) then ! jararias, 14/08/2013
         call wrf_debug(100,'using coszen from radiation driver')
         do i=its,ite
            cosz(i)=coszen(i,j)
         end do
      else
!        da=6.2831853071795862*(julian-1)/365.
!        eot=(0.000075+0.001868*cos(da)-0.032077*sin(da) &
!           -0.014615*cos(2*da)-0.04089*sin(2*da))*(229.18)
         xt24 = mod(xtime + radfrq * 0.5, 1440.) + eot
         do i = its,ite
           tloctm = gmt + xt24 / 60. + xlong(i,j) / 15.
           hrang = 15. * (tloctm - 12.) * degrad
           xxlat = xlat(i,j) * degrad
           cosz(i) = sin(xxlat) * sin(declin) + &
                     cos(xxlat) * cos(declin) * cos(hrang)
         end do
      end if

      do i = its,ite
!       xt24 = mod(xtime + radfrq * 0.5, 1440.)
!       tloctm = gmt + xt24 / 60. + xlong(i,j) / 15.
!       hrang = 15. * (tloctm - 12.) * degrad
!       xxlat = xlat(i,j) * degrad
!       cosz(i) = sin(xxlat) * sin(declin) + &
!                 cos(xxlat) * cos(declin) * cos(hrang)

!
! surface spectrum albedo for direct and diffuse radiation
!
        rsuvbm(i) = alb(i,j)
        rsuvdf(i) = alb(i,j)
        rsirbm(i) = alb(i,j)
        rsirdf(i) = alb(i,j)
!
! urban
!
!       if(present(cosz_urb2d)) cosz_urb2d(i,j)=cosz(i)  !urban
!       if(present(omg_urb2d)) omg_urb2d(i,j)=hrang     !urban
      end do

!
! aerosol effects -> gocart aerosol module
!

   do ib = 1, ib_sw
      do k = kts-1,kte
         do i = its,ite
            taual_sw(i,k,ib) = 0.
            ssaal_sw(i,k,ib) = 0.
            asyal_sw(i,k,ib) = 0.
         end do
      end do
   end do

   if ( associated (tauaer3d_sw) ) then
   ! jararias 2013/11
!  if ( present (tauaer3d_sw) ) then
!     if ( aer_opt .eq. 2 ) then
         do ib=1,ib_sw
            do i=its,ite
               taual_sw(i,kts-1,ib)=0.
               ssaal_sw(i,kts-1,ib)=0.
               asyal_sw(i,kts-1,ib)=0.
               do k=kts,kte
                  nk=kte+kts-k
                  taual_sw(i,nk,ib)=tauaer3d_sw(i,k,j,ib)
                  ssaal_sw(i,nk,ib)=ssaaer3d_sw(i,k,j,ib)
                  asyal_sw(i,nk,ib)=asyaer3d_sw(i,k,j,ib)
               end do
            end do
         end do
!     end if
   end if
!
! 1-dimension driver of shortwave radiative transfer scheme
!
     do i = its,ite
      if (cosz(i) .gt. cosz_min) then !for daytime only
         call swrad ( m=1, np=mkx+1, pl=p8w2d(i,:), ta=t2d(i,:), wa=sh2d(i,:), oa=o3(i,:),                   &
                      cwc=cwc(i,:,:), reff=reff(i,:,:), fcld=fcld2d(i,:), ict=ict(i), icb=icb(i),            &
                      taual=taual_sw(i,:,:), ssaal=ssaal_sw(i,:,:), asyal=asyal_sw(i,:,:),                   &
                      cosz=cosz(i), rsuvbm=rsuvbm(i), rsuvdf=rsuvdf(i), rsirbm=rsirbm(i), rsirdf=rsirdf(i),  &
                      flx=flx(i,:), flxd=flxd(i,:),flxu=flxu(i,:),                                           &
                      ! -- jararias, 14/08/2013
                      fdiruv=fdiruv(i),fdifuv=fdifuv(i),                                                     &
                      fdirpar=fdirpar(i),fdifpar=fdifpar(i),                                                 &
                      fdirir=fdirir(i),fdifir=fdifir(i)                                                      )
      endif
     enddo
!
! convert the units of flx and flc from fraction to w/m^2
!
      do k = kts, kte+1
        do i = its, ite
          if (cosz(i) .le. cosz_min) then
            flx(i,k) = 0.
            swddir(i,j) = 0. !jararias, 14/08/2013
            swddif(i,j) = 0. !jararias, 14/08/2013
            swddni(i,j) = 0. !jararias, 14/08/2013
          else
            flx(i,k) = flx(i,k) * solcon * cosz(i)
            swddni(i,j) = ( fdiruv(i) + fdirpar(i) + fdirir(i) ) * solcon           !jararias, 14/08/2013
            swddir(i,j) = swddni(i,j) * cosz(i)                                     !jararias, 14/08/2013
            swddif(i,j) = ( fdifuv(i) + fdifpar(i) + fdifir(i) ) * solcon * cosz(i) !jararias, 14/08/2013
          endif
        end do
      end do

!
! calculate heating rate (deg/sec)
!
      fac = .01 * g / cp
   do k = kts, kte
      do i = its, ite
             tten2d(i,k) = - fac * (flx(i,k) - flx(i,k+1))/ (p8w2d(i,k)-p8w2d(i,k+1))
      end do
   end do


      do k=kts,kte          
         nk=kme-1-k+kms
         do i=its,ite
             rthraten(i,k,j)=rthraten(i,k,j)+tten2d(i,nk)/pi3d(i,k,j)  
         enddo
      enddo

      do i = its, ite
        if (cosz(i) .le. cosz_min) then
          gsf(i,j) = 0.
          if(present(ERBE_out)) then
             ERBE_out(i,j,5) = 0.   
             ERBE_out(i,j,6) = 0.  
             ERBE_out(i,j,7) = 0.  
             ERBE_out(i,j,8) = 0. 
          endif
        else
!
! absorbed part in surface energy budget [W/m^2] (Note that flxd in SW is flux fraction)
!
          gsf(i,j) = (1. - rsuvbm(i)) * flxd(i,kte+1) * solcon * cosz(i)

!
! extra output for SDSU
!       
          if(present(ERBE_out)) then
             ERBE_out(i,j,5) = flxd(i,1)     * solcon * cosz(i)  ! TOA SW downwelling flux [W/m2] 
             ERBE_out(i,j,6) = flxu(i,1)     * solcon * cosz(i)  ! TOA SW upwelling flux   [W/m2]
             ERBE_out(i,j,7) = flxd(i,kte+1) * solcon * cosz(i)  ! surface SW downwelling flux [W/m2]
             ERBE_out(i,j,8) = flxu(i,kte+1) * solcon * cosz(i)  ! surface SW upwelling flux   [W/m2]
          endif
        endif
      enddo

! LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW 
! LW LW LW LW LW LW LW LW  Longwave scheme  LW LW LW LW LW LW LW LW LW 
! LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW LW 

  case ('lw')

!
! aerosol effects -> gocart aerosol module
!

   do ib = 1, ib_lw
      do k = kts-1,kte
         do i = its,ite
            taual_lw(i,k,ib) = 0.
            ssaal_lw(i,k,ib) = 0.
            asyal_lw(i,k,ib) = 0.
         end do
      end do
   end do

!
! surface parameters
!
      do i = its,ite
        emis2d(i,1:ib_lw) = emiss(i,j)
!toshi
        tsfc(i)=0.5*t8w2d(i,kme) + 0.5*t2d(i,kte)  !make sure
        tskin(i)=t8w2d(i,kme)
      end do

!
! drive goddard longwave radiative transfer scheme
!
      call lwrad (m=mix, np=mkx+1, pl=p8w2d, ta=t2d, wa=sh2d, oa=o3, tb=tsfc, ts=tskin, &
                  cwc=cwc, emiss=emis2d, reff=reff, fcld=fcld2d, ict=ict, icb=icb,      &
                   taual=taual_lw, ssaal=ssaal_lw, asyal=asyal_lw,                      &
                   flx=flx, acflxd=flxd , acflxu=flxu )

!
! calculate heating rate (deg/sec)
!
      fac = .01 * g / cp
   do k = kts, kte
      do i = its, ite
         tten2d(i,k) = - fac * (flx(i,k) - flx(i,k+1))/(p8w2d(i,k)-p8w2d(i,k+1)) 
      end do
   end do

      do k=kts,kte
         nk=kme-1-k+kms
         do i=its,ite
            rthraten(i,k,j)=rthraten(i,k,j)+tten2d(i,nk)/pi3d(i,k,j)  !<- potential temperature
         enddo
      enddo

!
! downwelling/upwelling longwave radiation [W/m^2] (Note that flxd/flxu of LW is actual flux [W/m2])
!

      do i = its, ite
          gsf(i,j) = flxd(i,kte+1)  !surface downwelling LW radiation

!
! extra output for SDSU
!       
          if(present(ERBE_out)) then
             ERBE_out(i,j,1) = flxd(i,1)      ! TOA LW downwelling flux [W/m2] 
             ERBE_out(i,j,2) = flxu(i,1)      ! TOA LW upwelling flux   [W/m2]
             ERBE_out(i,j,3) = flxd(i,kte+1)  ! surface LW downwelling flux   [W/m2]
             ERBE_out(i,j,4) = flxu(i,kte+1)  ! surface LW upwelling flux   [W/m2]
          endif
      end do


  case default

       stop 'the option does not exist: sw_or_lw '

  end select rad_select

!
!-----------------------------------------------------------------------
!
   enddo j_loop                                          

 end subroutine goddardrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

  subroutine swrad (m,np,pl,ta,wa,oa, cwc,reff,fcld,ict,icb,             &
                    taual,ssaal,asyal, cosz,rsuvbm,rsuvdf,rsirbm,rsirdf, &
                    flx,flxd,flxu,                                       &
                    fdiruv,fdifuv,fdirpar,fdifpar,fdirir,fdifir          ) !jararias, 14/08/2013

!------------     corrections for bugs      ----------------------------
!
!     a bug was found in "ntop=nctop(m)", it has been corrected to be
!     "ntop=nctop(i)".   dated march 28, 2000.
!
!
!-----------------------------------------------------------------------
!
! following the nasa technical memorandum (nasa/tm-1999-104606, vol. 15)
!  of chou and suarez (1999), this routine computes solar fluxes due to
!  absorption by water vapor, ozone, co2, o2, clouds, and aerosols and
!  due to scattering by clouds, aerosols, and gases.
!
! this code computes fluxes simultaneously for m soundings.
!
! cloud ice, liquid, and rain particles are allowed to co-exist in a layer.
!
! if no information is available for the effective particle size, reff,
!  default values of 10 micron for liquid water and 75 micron for ice may be
!  used.
!  the size of raindrops, reff(3), is irrelevant in this code. it can be
!  set to any values.
!  for a clear layer, reff can be set to any values except zero.
!
! the maximum-random assumption is applied for treating cloud
!  overlapping. clouds are grouped into high, middle, and low clouds
!  separated by the level indices ict and icb.  for detail, see
!  subroutine "cloud_scale".
!
! aerosol optical thickness, single-scattering albedo, and asymmetry
!  factor can be specified as functions of height and spectral band.
!
!----- input parameters:
!                                                   units      size
!  number of soundings (m)                          n/d         1
!  number of atmospheric layers (np)                n/d         1
!  level pressure (pl)                              mb        m*(np+1)
!  layer temperature (ta)                           k         m*np
!  layer specific humidity (wa)                     gm/gm     m*np
!  layer ozone concentration (oa)                   gm/gm     m*np
!  co2 mixing ratio by volume (co2)                 pppv        1
!  option for scaling cloud optical thickness       n/d         1
!        overcast="true" if scaling is not required
!        overcast="fasle" if scaling is required
!  option for cloud optical thickness               n/d         1
!        cldwater="true" if cwc is provided
!        cldwater="false" if taucld is provided
!  cloud water mixing ratio (cwc)                  gm/gm      m*np*3
!        index 1 for ice particles
!        index 2 for liquid drops
!        index 3 for rain drops
!  cloud optical thickness (taucld)                 n/d       m*np*3
!        index 1 for ice particles
!        index 2 for liquid drops
!        index 3 for rain drops
!  effective cloud-particle size (reff)          micrometer   m*np*3
!        index 1 for ice particles
!        index 2 for liquid drops
!        index 3 for rain drops
!  cloud amount (fcld)                            fraction    m*np
!  level index separating high and middle           n/d         1
!        clouds (ict)
!  level index separating middle and low            n/d         1
!        clouds (icb)
!  aerosol optical thickness (taual)                n/d       m*np*11
!  aerosol single-scattering albedo (ssaal)         n/d       m*np*11
!  aerosol asymmetry factor (asyal)                 n/d       m*np*11
!        in the uv region :
!           index  1 for the 0.175-0.225 micron band
!           index  2 for the 0.225-0.245; 0.260-0.280 micron band
!           index  3 for the 0.245-0.260 micron band
!           index  4 for the 0.280-0.295 micron band
!           index  5 for the 0.295-0.310 micron band
!           index  6 for the 0.310-0.320 micron band
!           index  7 for the 0.325-0.400 micron band
!        in the par region :
!           index  8 for the 0.400-0.700 micron band
!        in the infrared region :
!           index  9 for the 0.700-1.220 micron band
!           index 10 for the 1.220-2.270 micron band
!           index 11 for the 2.270-10.00 micron band
!   cosine of solar zenith angle (cosz)              n/d      m
!   uv+visible sfc albedo for beam radiation
!        for wavelengths<0.7 micron (rsuvbm)       fraction   m
!   uv+visible sfc albedo for diffuse radiation
!        for wavelengths<0.7 micron (rsuvdf)       fraction   m
!   ir sfc albedo for beam radiation
!        for wavelengths>0.7 micron  (rsirbm)      fraction   m
!   ir sfc albedo for diffuse radiation (rsirdf)   fraction   m
!
!----- output parameters
!
!   all-sky flux divergence (downward minus upward) (flx)     fraction   m*(np+1)
!   clear-sky flux divergence (downward minus upward) (flc)   fraction   m*(np+1)
! 
!   all-sky direct downward uv (0.175-0.4 micron)
!                flux at the surface (fdiruv)      fraction   m
!   all-sky diffuse downward uv flux at
!                the surface (fdifuv)              fraction   m
!   all-sky direct downward par (0.4-0.7 micron)
!                flux at the surface (fdirpar)     fraction   m
!   all-sky diffuse downward par flux at
!                the surface (fdifpar)             fraction   m
!   all-sky direct downward ir (0.7-10 micron)
!                flux at the surface (fdirir)      fraction   m
!   all-sky diffuse downward ir flux at
!                the surface (fdifir)              fraction   m
!
!----- notes:
!
!    (1) the unit of output fluxes (flx,flc,etc.) is fraction of the
!        insolation at the top of the atmosphere.  therefore, fluxes
!        are the output fluxes multiplied by the extra-terrestrial solar
!        flux and the cosine of the solar zenith angle.
!    (2) pl( ,1) is the pressure at the top of the model, and
!        pl( ,np+1) is the surface pressure.
!    (3) the pressure levels ict and icb correspond approximately
!        to 400 and 700 mb.
!
!-----if coding errors are found, please notify ming-dah chou at
!     chou@climate.gsfc.nasa.gov
!
!*************************************************************************
 implicit none

!-----IO parameters
 integer,intent(in) :: m,np,ict(m),icb(m)
 real(Kind=fp_kind),intent(in) :: pl(m,np+1),ta(m,np),wa(m,np),oa(m,np)
 real(Kind=fp_kind),intent(in) :: cwc(m,np,3),reff(m,np,3),fcld(m,np)
 real(Kind=fp_kind),intent(in) :: taual(m,np,ib_sw),ssaal(m,np,ib_sw),asyal(m,np,ib_sw)
 real(Kind=fp_kind),intent(in) :: cosz(m),rsuvbm(m),rsuvdf(m),rsirbm(m),rsirdf(m)

 real(Kind=fp_kind),intent(inout) :: flx(m,np+1)    !flux divergence (down-up) []
 real(Kind=fp_kind),intent(inout) :: flxd(m,np+1)   !downward flux fraction []
 real(Kind=fp_kind),intent(inout) :: flxu(m,np+1)   !upwelling flux fraction []
!-----IO parameter used to be-----
 real(Kind=fp_kind) flc(m,np+1)
 real(Kind=fp_kind),intent(inout) :: fdiruv (m),fdifuv (m)
 real(Kind=fp_kind),intent(inout) :: fdirpar(m),fdifpar(m)
 real(Kind=fp_kind),intent(inout) :: fdirir (m),fdifir (m)

!-----temporary array
 integer i,j,k,ntop
 integer :: nctop(m)
 real(Kind=fp_kind) x
 real(Kind=fp_kind) :: taucld(m,np,3)
 real(Kind=fp_kind) :: taux(m,np)     ! total condensates optical depth
 real(Kind=fp_kind) :: cwp(m,np,3)
 real(Kind=fp_kind) :: dp(m,np)
 real(Kind=fp_kind) :: wh(m,np)
 real(Kind=fp_kind) :: oh(m,np)
 real(Kind=fp_kind) :: scal(m,np)
 real(Kind=fp_kind) :: swu(m,np+1)
 real(Kind=fp_kind) :: swh(m,np+1)
 real(Kind=fp_kind) :: so2(m,np+1)    !scaled o2 conc
 real(Kind=fp_kind) :: df(m,np+1)     !integrated flux refuction rate []
 real(Kind=fp_kind) :: df_sub(m,np+1) !sub-layer flux reduction rate []
 real(Kind=fp_kind) :: df_cld(m,np+1) !integrated clear-sky flux reduction rate []
 real(Kind=fp_kind) :: df_clr(m,np+1) !integrated all-sky flux reduction rate []
 real(Kind=fp_kind) :: snt(m)         !inverse of cosz
 real(Kind=fp_kind) :: cnt(m)

! new look-up table for (Fclr/Fall) ratio (overcast_fast option)
  real(Kind=fp_kind) :: ratio, cld_alb
  integer :: i_cos, i_tau
  real(Kind=fp_kind) :: ratio_lut(10,10)
   data ((ratio_lut(i,j),i=1,10),j=1,10)/ &  !i cosin  j albedo
   0.796, 0.559, 0.523, 0.474, 0.439, 0.377, 0.298, 0.239, 0.154, 0.086, &
   0.845, 0.628, 0.566, 0.508, 0.457, 0.392, 0.315, 0.242, 0.156, 0.087, &
   0.894, 0.697, 0.609, 0.542, 0.475, 0.407, 0.332, 0.245, 0.158, 0.088, &
   0.924, 0.759, 0.662, 0.581, 0.511, 0.432, 0.350, 0.269, 0.173, 0.092, &
   0.944, 0.809, 0.713, 0.634, 0.552, 0.471, 0.381, 0.288, 0.183, 0.097, &
   0.961, 0.848, 0.760, 0.689, 0.602, 0.516, 0.425, 0.323, 0.208, 0.116, &
   0.971, 0.882, 0.808, 0.730, 0.650, 0.556, 0.456, 0.355, 0.233, 0.134, &
   0.978, 0.910, 0.844, 0.776, 0.695, 0.601, 0.499, 0.387, 0.256, 0.141, &
   0.984, 0.934, 0.876, 0.810, 0.731, 0.637, 0.533, 0.405, 0.275, 0.151, &
   0.988, 0.944, 0.897, 0.844, 0.773, 0.683, 0.561, 0.421, 0.277, 0.156/

!-----parameters for co2 transmission tables---------------------------
      integer nu,nw,nx2,ny2 ! cccshie 9/15/04
      parameter (nu=43,nw=37,nx2=62,ny2=101)
      real(Kind=fp_kind) w1,dw,u1,du,coa(nx2,ny2),cah(nu,nw)
!-----cah is the co2 absorptance in band 10
      data ((cah(i,j),i=1,43),j=  1,  1)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002575,  0.0003384,  0.0004400,  0.0005662,  0.0007219, &
        0.0009131,  0.0011470,  0.0014327,  0.0017806,  0.0022021, &
        0.0027093,  0.0033141,  0.0040280,  0.0048609,  0.0058217, &
        0.0069177,  0.0081559,  0.0095430/
      data ((cah(i,j),i=1,43),j=  2,  2)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002575,  0.0003384,  0.0004400,  0.0005662,  0.0007219, &
        0.0009130,  0.0011470,  0.0014326,  0.0017805,  0.0022020, &
        0.0027091,  0.0033139,  0.0040276,  0.0048605,  0.0058211, &
        0.0069170,  0.0081551,  0.0095420/
      data ((cah(i,j),i=1,43),j=  3,  3)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002574,  0.0003384,  0.0004399,  0.0005661,  0.0007218, &
        0.0009129,  0.0011468,  0.0014325,  0.0017803,  0.0022017, &
        0.0027088,  0.0033135,  0.0040271,  0.0048599,  0.0058204, &
        0.0069161,  0.0081539,  0.0095406/
      data ((cah(i,j),i=1,43),j=  4,  4)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001936, &
        0.0002574,  0.0003384,  0.0004399,  0.0005661,  0.0007217, &
        0.0009128,  0.0011467,  0.0014323,  0.0017800,  0.0022014, &
        0.0027084,  0.0033130,  0.0040265,  0.0048591,  0.0058194, &
        0.0069148,  0.0081524,  0.0095387/
      data ((cah(i,j),i=1,43),j=  5,  5)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001935, &
        0.0002574,  0.0003383,  0.0004398,  0.0005660,  0.0007216, &
        0.0009127,  0.0011465,  0.0014320,  0.0017797,  0.0022010, &
        0.0027078,  0.0033123,  0.0040256,  0.0048580,  0.0058180, &
        0.0069132,  0.0081503,  0.0095361/
      data ((cah(i,j),i=1,43),j=  6,  6)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001059,  0.0001439,  0.0001935, &
        0.0002573,  0.0003383,  0.0004398,  0.0005659,  0.0007215, &
        0.0009125,  0.0011462,  0.0014317,  0.0017792,  0.0022004, &
        0.0027071,  0.0033113,  0.0040244,  0.0048565,  0.0058162, &
        0.0069109,  0.0081476,  0.0095328/
      data ((cah(i,j),i=1,43),j=  7,  7)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000053,  0.0000074, &
        0.0000104,  0.0000147,  0.0000206,  0.0000288,  0.0000402, &
        0.0000559,  0.0000772,  0.0001058,  0.0001438,  0.0001935, &
        0.0002573,  0.0003382,  0.0004396,  0.0005657,  0.0007213, &
        0.0009122,  0.0011459,  0.0014312,  0.0017786,  0.0021996, &
        0.0027061,  0.0033100,  0.0040228,  0.0048545,  0.0058137, &
        0.0069079,  0.0081439,  0.0095283/
      data ((cah(i,j),i=1,43),j=  8,  8)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000206,  0.0000288,  0.0000402, &
        0.0000558,  0.0000772,  0.0001058,  0.0001438,  0.0001934, &
        0.0002572,  0.0003381,  0.0004395,  0.0005655,  0.0007210, &
        0.0009119,  0.0011454,  0.0014306,  0.0017778,  0.0021985, &
        0.0027047,  0.0033084,  0.0040207,  0.0048519,  0.0058105, &
        0.0069040,  0.0081391,  0.0095225/
      data ((cah(i,j),i=1,43),j=  9,  9)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000206,  0.0000288,  0.0000402, &
        0.0000558,  0.0000771,  0.0001058,  0.0001437,  0.0001933, &
        0.0002571,  0.0003379,  0.0004393,  0.0005652,  0.0007206, &
        0.0009114,  0.0011447,  0.0014297,  0.0017767,  0.0021971, &
        0.0027030,  0.0033061,  0.0040180,  0.0048485,  0.0058064, &
        0.0068989,  0.0081329,  0.0095149/
      data ((cah(i,j),i=1,43),j= 10, 10)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000288,  0.0000402, &
        0.0000558,  0.0000771,  0.0001057,  0.0001437,  0.0001932, &
        0.0002569,  0.0003377,  0.0004390,  0.0005649,  0.0007201, &
        0.0009107,  0.0011439,  0.0014286,  0.0017753,  0.0021953, &
        0.0027006,  0.0033032,  0.0040144,  0.0048441,  0.0058009, &
        0.0068922,  0.0081248,  0.0095051/
      data ((cah(i,j),i=1,43),j= 11, 11)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000287,  0.0000401, &
        0.0000558,  0.0000770,  0.0001056,  0.0001436,  0.0001931, &
        0.0002567,  0.0003375,  0.0004387,  0.0005644,  0.0007195, &
        0.0009098,  0.0011428,  0.0014271,  0.0017734,  0.0021929, &
        0.0026976,  0.0032995,  0.0040097,  0.0048384,  0.0057939, &
        0.0068837,  0.0081145,  0.0094926/
      data ((cah(i,j),i=1,43),j= 12, 12)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000287,  0.0000401, &
        0.0000557,  0.0000770,  0.0001055,  0.0001434,  0.0001929, &
        0.0002565,  0.0003371,  0.0004382,  0.0005637,  0.0007186, &
        0.0009087,  0.0011413,  0.0014252,  0.0017709,  0.0021898, &
        0.0026937,  0.0032946,  0.0040038,  0.0048311,  0.0057850, &
        0.0068729,  0.0081013,  0.0094768/
      data ((cah(i,j),i=1,43),j= 13, 13)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000287,  0.0000400, &
        0.0000556,  0.0000769,  0.0001054,  0.0001432,  0.0001926, &
        0.0002561,  0.0003366,  0.0004376,  0.0005629,  0.0007175, &
        0.0009073,  0.0011394,  0.0014228,  0.0017678,  0.0021859, &
        0.0026888,  0.0032885,  0.0039963,  0.0048218,  0.0057738, &
        0.0068592,  0.0080849,  0.0094570/
      data ((cah(i,j),i=1,43),j= 14, 14)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000074, &
        0.0000104,  0.0000146,  0.0000205,  0.0000286,  0.0000400, &
        0.0000556,  0.0000767,  0.0001052,  0.0001430,  0.0001923, &
        0.0002557,  0.0003361,  0.0004368,  0.0005619,  0.0007161, &
        0.0009054,  0.0011370,  0.0014197,  0.0017639,  0.0021809, &
        0.0026826,  0.0032809,  0.0039869,  0.0048103,  0.0057597, &
        0.0068422,  0.0080643,  0.0094323/
      data ((cah(i,j),i=1,43),j= 15, 15)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000103,  0.0000145,  0.0000204,  0.0000286,  0.0000399, &
        0.0000554,  0.0000766,  0.0001050,  0.0001427,  0.0001919, &
        0.0002552,  0.0003353,  0.0004358,  0.0005605,  0.0007144, &
        0.0009032,  0.0011340,  0.0014159,  0.0017590,  0.0021748, &
        0.0026750,  0.0032715,  0.0039752,  0.0047961,  0.0057424, &
        0.0068212,  0.0080389,  0.0094019/
      data ((cah(i,j),i=1,43),j= 16, 16)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000019,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000103,  0.0000145,  0.0000204,  0.0000285,  0.0000398, &
        0.0000553,  0.0000764,  0.0001047,  0.0001423,  0.0001914, &
        0.0002545,  0.0003344,  0.0004345,  0.0005589,  0.0007122, &
        0.0009003,  0.0011304,  0.0014112,  0.0017531,  0.0021673, &
        0.0026656,  0.0032598,  0.0039609,  0.0047786,  0.0057211, &
        0.0067954,  0.0080078,  0.0093646/
      data ((cah(i,j),i=1,43),j= 17, 17)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000103,  0.0000145,  0.0000203,  0.0000284,  0.0000397, &
        0.0000551,  0.0000761,  0.0001044,  0.0001419,  0.0001908, &
        0.0002536,  0.0003332,  0.0004330,  0.0005568,  0.0007095, &
        0.0008968,  0.0011259,  0.0014054,  0.0017458,  0.0021123, &
        0.0026542,  0.0032457,  0.0039435,  0.0047573,  0.0056951, &
        0.0067640,  0.0079700,  0.0093194/
      data ((cah(i,j),i=1,43),j= 18, 18)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000007,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000037,  0.0000052,  0.0000073, &
        0.0000102,  0.0000144,  0.0000202,  0.0000283,  0.0000395, &
        0.0000549,  0.0000758,  0.0001040,  0.0001413,  0.0001900, &
        0.0002525,  0.0003318,  0.0004311,  0.0005543,  0.0007063, &
        0.0008926,  0.0011204,  0.0013985,  0.0017370,  0.0021470, &
        0.0026404,  0.0032285,  0.0039224,  0.0047315,  0.0056637, &
        0.0067260,  0.0079245,  0.0092651/
      data ((cah(i,j),i=1,43),j= 19, 19)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000036,  0.0000051,  0.0000072, &
        0.0000102,  0.0000143,  0.0000201,  0.0000282,  0.0000393, &
        0.0000546,  0.0000754,  0.0001034,  0.0001406,  0.0001890, &
        0.0002512,  0.0003300,  0.0004287,  0.0005513,  0.0007023, &
        0.0008875,  0.0011139,  0.0013901,  0.0017264,  0.0021337, &
        0.0026238,  0.0032080,  0.0038971,  0.0047005,  0.0056261, &
        0.0066806,  0.0078701,  0.0092003/
      data ((cah(i,j),i=1,43),j= 20, 20)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000026,  0.0000036,  0.0000051,  0.0000072, &
        0.0000101,  0.0000142,  0.0000200,  0.0000280,  0.0000391, &
        0.0000543,  0.0000750,  0.0001028,  0.0001397,  0.0001878, &
        0.0002496,  0.0003279,  0.0004259,  0.0005476,  0.0006975, &
        0.0008813,  0.0011060,  0.0013802,  0.0017138,  0.0021179, &
        0.0026040,  0.0031835,  0.0038670,  0.0046637,  0.0055814, &
        0.0066267,  0.0078055,  0.0091235/
      data ((cah(i,j),i=1,43),j= 21, 21)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000005,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000025,  0.0000036,  0.0000051,  0.0000071, &
        0.0000100,  0.0000141,  0.0000198,  0.0000278,  0.0000388, &
        0.0000539,  0.0000744,  0.0001020,  0.0001386,  0.0001863, &
        0.0002477,  0.0003253,  0.0004226,  0.0005432,  0.0006918, &
        0.0008740,  0.0010966,  0.0013683,  0.0016988,  0.0020991, &
        0.0025806,  0.0031545,  0.0038313,  0.0046201,  0.0055285, &
        0.0065630,  0.0077294,  0.0090332/
      data ((cah(i,j),i=1,43),j= 22, 22)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000025,  0.0000036,  0.0000050,  0.0000071, &
        0.0000100,  0.0000140,  0.0000197,  0.0000275,  0.0000384, &
        0.0000534,  0.0000737,  0.0001011,  0.0001373,  0.0001846, &
        0.0002453,  0.0003222,  0.0004185,  0.0005265,  0.0006850, &
        0.0008652,  0.0010855,  0.0013541,  0.0016809,  0.0020768, &
        0.0025528,  0.0031202,  0.0037892,  0.0045688,  0.0054664, &
        0.0064883,  0.0076402,  0.0089277/
      data ((cah(i,j),i=1,43),j= 23, 23)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000013, &
        0.0000018,  0.0000025,  0.0000035,  0.0000050,  0.0000070, &
        0.0000098,  0.0000138,  0.0000194,  0.0000272,  0.0000380, &
        0.0000528,  0.0000729,  0.0000999,  0.0001357,  0.0001825, &
        0.0002425,  0.0003185,  0.0004137,  0.0005316,  0.0006769, &
        0.0008548,  0.0010722,  0.0013373,  0.0016599,  0.0020504, &
        0.0025201,  0.0030799,  0.0037398,  0.0045087,  0.0053938, &
        0.0064013,  0.0075366,  0.0088053/
      data ((cah(i,j),i=1,43),j= 24, 24)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000012, &
        0.0000017,  0.0000025,  0.0000035,  0.0000049,  0.0000069, &
        0.0000097,  0.0000137,  0.0000192,  0.0000268,  0.0000375, &
        0.0000520,  0.0000719,  0.0000986,  0.0001339,  0.0001800, &
        0.0002392,  0.0003142,  0.0004079,  0.0005242,  0.0006673, &
        0.0008426,  0.0010567,  0.0013177,  0.0016352,  0.0020196, &
        0.0024820,  0.0030330,  0.0036825,  0.0044391,  0.0053098, &
        0.0063007,  0.0074172,  0.0084815/
      data ((cah(i,j),i=1,43),j= 25, 25)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000009,  0.0000012, &
        0.0000017,  0.0000024,  0.0000034,  0.0000048,  0.0000068, &
        0.0000096,  0.0000134,  0.0000189,  0.0000264,  0.0000369, &
        0.0000512,  0.0000708,  0.0000970,  0.0001318,  0.0001772, &
        0.0002354,  0.0003091,  0.0004013,  0.0005156,  0.0006562, &
        0.0008284,  0.0010386,  0.0012949,  0.0016066,  0.0019840, &
        0.0024379,  0.0029788,  0.0036164,  0.0043590,  0.0052135, &
        0.0061857,  0.0072808,  0.0085042/
      data ((cah(i,j),i=1,43),j= 26, 26)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000002,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000012, &
        0.0000017,  0.0000024,  0.0000034,  0.0000047,  0.0000067, &
        0.0000094,  0.0000132,  0.0000185,  0.0000259,  0.0000362, &
        0.0000503,  0.0000695,  0.0000952,  0.0001294,  0.0001739, &
        0.0002310,  0.0003033,  0.0003937,  0.0005057,  0.0006435, &
        0.0008121,  0.0010180,  0.0012688,  0.0015739,  0.0019434, &
        0.0023877,  0.0029172,  0.0035413,  0.0042681,  0.0051043, &
        0.0060554,  0.0071267,  0.0083234/
      data ((cah(i,j),i=1,43),j= 27, 27)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000012, &
        0.0000016,  0.0000023,  0.0000033,  0.0000046,  0.0000065, &
        0.0000092,  0.0000129,  0.0000181,  0.0000254,  0.0000355, &
        0.0000493,  0.0000680,  0.0000933,  0.0001267,  0.0001702, &
        0.0002261,  0.0002968,  0.0003852,  0.0004946,  0.0006291, &
        0.0007937,  0.0009946,  0.0012394,  0.0015370,  0.0018975, &
        0.0023310,  0.0028478,  0.0034568,  0.0041660,  0.0049818, &
        0.0059096,  0.0069544,  0.0081215/
      data ((cah(i,j),i=1,43),j= 28, 28)/ &
        0.0000001,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000011, &
        0.0000016,  0.0000023,  0.0000032,  0.0000045,  0.0000064, &
        0.0000090,  0.0000126,  0.0000177,  0.0000248,  0.0000346, &
        0.0000481,  0.0000664,  0.0000910,  0.0001236,  0.0001661, &
        0.0002206,  0.0002895,  0.0003755,  0.0004821,  0.0006130, &
        0.0007731,  0.0009685,  0.0012065,  0.0014959,  0.0018463, &
        0.0022680,  0.0027705,  0.0033629,  0.0040526,  0.0048459, &
        0.0057480,  0.0067639,  0.0078987/
      data ((cah(i,j),i=1,43),j= 29, 29)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000006,  0.0000008,  0.0000011, &
        0.0000016,  0.0000022,  0.0000031,  0.0000044,  0.0000062, &
        0.0000087,  0.0000123,  0.0000173,  0.0000242,  0.0000330, &
        0.0000468,  0.0000646,  0.0000886,  0.0001203,  0.0001616, &
        0.0002145,  0.0002814,  0.0003649,  0.0004682,  0.0005951, &
        0.0007503,  0.0009396,  0.0011701,  0.0014505,  0.0017900, &
        0.0021986,  0.0026857,  0.0032598,  0.0039283,  0.0046971, &
        0.0055713,  0.0065558,  0.0076557/
      data ((cah(i,j),i=1,43),j= 30, 30)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000005,  0.0000008,  0.0000011, &
        0.0000015,  0.0000021,  0.0000030,  0.0000043,  0.0000060, &
        0.0000085,  0.0000119,  0.0000167,  0.0000234,  0.0000327, &
        0.0000454,  0.0000627,  0.0000859,  0.0001166,  0.0001566, &
        0.0002078,  0.0002724,  0.0003531,  0.0004529,  0.0005755, &
        0.0007253,  0.0009079,  0.0011304,  0.0014010,  0.0017287, &
        0.0021232,  0.0025935,  0.0031480,  0.0037936,  0.0045361, &
        0.0053805,  0.0063314,  0.0073941/
      data ((cah(i,j),i=1,43),j= 31, 31)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000005,  0.0000007,  0.0000010, &
        0.0000015,  0.0000021,  0.0000029,  0.0000041,  0.0000058, &
        0.0000082,  0.0000115,  0.0000162,  0.0000226,  0.0000316, &
        0.0000438,  0.0000605,  0.0000829,  0.0001125,  0.0001510, &
        0.0002004,  0.0002626,  0.0003402,  0.0004362,  0.0005540, &
        0.0006980,  0.0008736,  0.0010874,  0.0013476,  0.0016627, &
        0.0020421,  0.0024947,  0.0030283,  0.0036497,  0.0043644, &
        0.0051772,  0.0060928,  0.0071164/
      data ((cah(i,j),i=1,43),j= 32, 32)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000003,  0.0000004,  0.0000005,  0.0000007,  0.0000010, &
        0.0000014,  0.0000020,  0.0000028,  0.0000040,  0.0000056, &
        0.0000079,  0.0000111,  0.0000155,  0.0000218,  0.0000303, &
        0.0000421,  0.0000582,  0.0000797,  0.0001081,  0.0001450, &
        0.0001923,  0.0002519,  0.0003262,  0.0004180,  0.0005308, &
        0.0006686,  0.0008367,  0.0010414,  0.0012905,  0.0015925, &
        0.0019561,  0.0023900,  0.0029017,  0.0034978,  0.0041836, &
        0.0049638,  0.0058430,  0.0068264/
      data ((cah(i,j),i=1,43),j= 33, 33)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000002,  0.0000003,  0.0000005,  0.0000007,  0.0000010, &
        0.0000014,  0.0000019,  0.0000027,  0.0000038,  0.0000053, &
        0.0000075,  0.0000106,  0.0000149,  0.0000208,  0.0000290, &
        0.0000403,  0.0000556,  0.0000761,  0.0001032,  0.0001384, &
        0.0001834,  0.0002402,  0.0003110,  0.0003985,  0.0005059, &
        0.0006372,  0.0007974,  0.0009926,  0.0012302,  0.0015185, &
        0.0018657,  0.0022803,  0.0027696,  0.0033398,  0.0039960, &
        0.0047430,  0.0055851,  0.0065278/
      data ((cah(i,j),i=1,43),j= 34, 34)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000002,  0.0000003,  0.0000005,  0.0000006,  0.0000009, &
        0.0000013,  0.0000018,  0.0000026,  0.0000036,  0.0000051, &
        0.0000071,  0.0000100,  0.0000141,  0.0000197,  0.0000275, &
        0.0000382,  0.0000527,  0.0000722,  0.0000979,  0.0001312, &
        0.0001739,  0.0002277,  0.0002947,  0.0003775,  0.0004793, &
        0.0006038,  0.0007558,  0.0009412,  0.0011671,  0.0014412, &
        0.0017717,  0.0021208,  0.0026329,  0.0031768,  0.0038033, &
        0.0045168,  0.0053220,  0.0062240/
      data ((cah(i,j),i=1,43),j= 35, 35)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000002, &
        0.0000002,  0.0000003,  0.0000004,  0.0000006,  0.0000009, &
        0.0000012,  0.0000017,  0.0000024,  0.0000034,  0.0000048, &
        0.0000067,  0.0000095,  0.0000133,  0.0000186,  0.0000259, &
        0.0000360,  0.0000496,  0.0000679,  0.0000921,  0.0001235, &
        0.0001637,  0.0002143,  0.0002773,  0.0003554,  0.0004513, &
        0.0005688,  0.0007124,  0.0008876,  0.0011014,  0.0013610, &
        0.0016745,  0.0020493,  0.0024925,  0.0030099,  0.0036066, &
        0.0042868,  0.0050553,  0.0059171/
      data ((cah(i,j),i=1,43),j= 36, 36)/ &
        0.0000000,  0.0000001,  0.0000001,  0.0000001,  0.0000001, &
        0.0000002,  0.0000003,  0.0000004,  0.0000006,  0.0000008, &
        0.0000011,  0.0000016,  0.0000022,  0.0000032,  0.0000045, &
        0.0000063,  0.0000088,  0.0000124,  0.0000173,  0.0000242, &
        0.0000336,  0.0000463,  0.0000634,  0.0000860,  0.0001153, &
        0.0001528,  0.0002001,  0.0002591,  0.0003322,  0.0004221, &
        0.0005323,  0.0006672,  0.0008322,  0.0010335,  0.0012785, &
        0.0015746,  0.0019293,  0.0023491,  0.0028399,  0.0034067, &
        0.0040539,  0.0047860,  0.0056083/
      data ((cah(i,j),i=1,43),j= 37, 37)/ &
        0.0000000,  0.0000000,  0.0000001,  0.0000001,  0.0000001, &
        0.0000002,  0.0000003,  0.0000004,  0.0000005,  0.0000007, &
        0.0000010,  0.0000015,  0.0000021,  0.0000029,  0.0000041, &
        0.0000058,  0.0000082,  0.0000114,  0.0000160,  0.0000223, &
        0.0000310,  0.0000428,  0.0000586,  0.0000795,  0.0001067, &
        0.0001414,  0.0001853,  0.0002401,  0.0003081,  0.0003918, &
        0.0004947,  0.0006208,  0.0007751,  0.0009639,  0.0011940, &
        0.0014726,  0.0018069,  0.0022032,  0.0026674,  0.0032043, &
        0.0038186,  0.0045147,  0.0052979/
!-----coa is the co2 absorptance in strong absorption regions of band 11
      data ((coa(i,j),i=1,62),j=  1,  1)/ &
        0.0000080,  0.0000089,  0.0000098,  0.0000106,  0.0000114, &
        0.0000121,  0.0000128,  0.0000134,  0.0000140,  0.0000146, &
        0.0000152,  0.0000158,  0.0000163,  0.0000168,  0.0000173, &
        0.0000178,  0.0000182,  0.0000186,  0.0000191,  0.0000195, &
        0.0000199,  0.0000202,  0.0000206,  0.0000210,  0.0000213, &
        0.0000217,  0.0000220,  0.0000223,  0.0000226,  0.0000229, &
        0.0000232,  0.0000235,  0.0000238,  0.0000241,  0.0000244, &
        0.0000246,  0.0000249,  0.0000252,  0.0000254,  0.0000257, &
        0.0000259,  0.0000261,  0.0000264,  0.0000266,  0.0000268, &
        0.0000271,  0.0000273,  0.0000275,  0.0000277,  0.0000279, &
        0.0000281,  0.0000283,  0.0000285,  0.0000287,  0.0000289, &
        0.0000291,  0.0000293,  0.0000295,  0.0000297,  0.0000298, &
        0.0000300,  0.0000302/
      data ((coa(i,j),i=1,62),j=  2,  2)/ &
        0.0000085,  0.0000095,  0.0000104,  0.0000113,  0.0000121, &
        0.0000128,  0.0000136,  0.0000143,  0.0000149,  0.0000155, &
        0.0000161,  0.0000167,  0.0000172,  0.0000178,  0.0000183, &
        0.0000187,  0.0000192,  0.0000196,  0.0000201,  0.0000205, &
        0.0000209,  0.0000213,  0.0000217,  0.0000220,  0.0000224, &
        0.0000227,  0.0000231,  0.0000234,  0.0000237,  0.0000240, &
        0.0000243,  0.0000246,  0.0000249,  0.0000252,  0.0000255, &
        0.0000258,  0.0000260,  0.0000263,  0.0000266,  0.0000268, &
        0.0000271,  0.0000273,  0.0000275,  0.0000278,  0.0000280, &
        0.0000282,  0.0000285,  0.0000287,  0.0000289,  0.0000291, &
        0.0000293,  0.0000295,  0.0000297,  0.0000299,  0.0000301, &
        0.0000303,  0.0000305,  0.0000307,  0.0000309,  0.0000311, &
        0.0000313,  0.0000314/
      data ((coa(i,j),i=1,62),j=  3,  3)/ &
        0.0000095,  0.0000106,  0.0000116,  0.0000125,  0.0000134, &
        0.0000143,  0.0000150,  0.0000158,  0.0000165,  0.0000171, &
        0.0000178,  0.0000184,  0.0000189,  0.0000195,  0.0000200, &
        0.0000205,  0.0000210,  0.0000215,  0.0000219,  0.0000223, &
        0.0000228,  0.0000232,  0.0000235,  0.0000239,  0.0000243, &
        0.0000247,  0.0000250,  0.0000253,  0.0000257,  0.0000260, &
        0.0000263,  0.0000266,  0.0000269,  0.0000272,  0.0000275, &
        0.0000278,  0.0000281,  0.0000283,  0.0000286,  0.0000289, &
        0.0000291,  0.0000294,  0.0000296,  0.0000299,  0.0000301, &
        0.0000303,  0.0000306,  0.0000308,  0.0000310,  0.0000312, &
        0.0000315,  0.0000317,  0.0000319,  0.0000321,  0.0000323, &
        0.0000325,  0.0000327,  0.0000329,  0.0000331,  0.0000333, &
        0.0000335,  0.0000329/
      data ((coa(i,j),i=1,62),j=  4,  4)/ &
        0.0000100,  0.0000111,  0.0000122,  0.0000131,  0.0000141, &
        0.0000149,  0.0000157,  0.0000165,  0.0000172,  0.0000179, &
        0.0000185,  0.0000191,  0.0000197,  0.0000203,  0.0000208, &
        0.0000213,  0.0000218,  0.0000223,  0.0000227,  0.0000232, &
        0.0000236,  0.0000240,  0.0000244,  0.0000248,  0.0000252, &
        0.0000255,  0.0000259,  0.0000262,  0.0000266,  0.0000269, &
        0.0000272,  0.0000275,  0.0000278,  0.0000281,  0.0000284, &
        0.0000287,  0.0000290,  0.0000293,  0.0000295,  0.0000298, &
        0.0000300,  0.0000303,  0.0000306,  0.0000308,  0.0000310, &
        0.0000313,  0.0000315,  0.0000317,  0.0000320,  0.0000322, &
        0.0000324,  0.0000326,  0.0000328,  0.0000331,  0.0000333, &
        0.0000335,  0.0000330,  0.0000339,  0.0000341,  0.0000343, &
        0.0000345,  0.0000346/
      data ((coa(i,j),i=1,62),j=  5,  5)/ &
        0.0000109,  0.0000121,  0.0000132,  0.0000143,  0.0000152, &
        0.0000161,  0.0000170,  0.0000178,  0.0000185,  0.0000192, &
        0.0000199,  0.0000205,  0.0000211,  0.0000217,  0.0000222, &
        0.0000228,  0.0000233,  0.0000238,  0.0000242,  0.0000247, &
        0.0000251,  0.0000255,  0.0000259,  0.0000263,  0.0000267, &
        0.0000271,  0.0000275,  0.0000278,  0.0000282,  0.0000285, &
        0.0000288,  0.0000291,  0.0000295,  0.0000298,  0.0000301, &
        0.0000304,  0.0000307,  0.0000309,  0.0000312,  0.0000315, &
        0.0000318,  0.0000320,  0.0000323,  0.0000325,  0.0000328, &
        0.0000330,  0.0000333,  0.0000335,  0.0000330,  0.0000340, &
        0.0000342,  0.0000344,  0.0000346,  0.0000348,  0.0000351, &
        0.0000353,  0.0000355,  0.0000357,  0.0000359,  0.0000361, &
        0.0000363,  0.0000365/
      data ((coa(i,j),i=1,62),j=  6,  6)/ &
        0.0000117,  0.0000130,  0.0000142,  0.0000153,  0.0000163, &
        0.0000173,  0.0000181,  0.0000190,  0.0000197,  0.0000204, &
        0.0000211,  0.0000218,  0.0000224,  0.0000230,  0.0000235, &
        0.0000241,  0.0000246,  0.0000251,  0.0000256,  0.0000260, &
        0.0000265,  0.0000269,  0.0000273,  0.0000277,  0.0000281, &
        0.0000285,  0.0000289,  0.0000293,  0.0000296,  0.0000299, &
        0.0000303,  0.0000306,  0.0000309,  0.0000313,  0.0000316, &
        0.0000319,  0.0000322,  0.0000324,  0.0000327,  0.0000330, &
        0.0000333,  0.0000336,  0.0000331,  0.0000341,  0.0000343, &
        0.0000346,  0.0000348,  0.0000351,  0.0000353,  0.0000355, &
        0.0000358,  0.0000360,  0.0000362,  0.0000365,  0.0000367, &
        0.0000369,  0.0000371,  0.0000373,  0.0000375,  0.0000377, &
        0.0000379,  0.0000381/
      data ((coa(i,j),i=1,62),j=  7,  7)/ &
        0.0000125,  0.0000139,  0.0000151,  0.0000163,  0.0000173, &
        0.0000183,  0.0000192,  0.0000200,  0.0000208,  0.0000216, &
        0.0000223,  0.0000229,  0.0000236,  0.0000242,  0.0000247, &
        0.0000253,  0.0000258,  0.0000263,  0.0000268,  0.0000273, &
        0.0000277,  0.0000282,  0.0000286,  0.0000290,  0.0000294, &
        0.0000298,  0.0000302,  0.0000306,  0.0000309,  0.0000313, &
        0.0000316,  0.0000320,  0.0000323,  0.0000326,  0.0000329, &
        0.0000332,  0.0000335,  0.0000331,  0.0000341,  0.0000344, &
        0.0000347,  0.0000350,  0.0000352,  0.0000355,  0.0000358, &
        0.0000360,  0.0000363,  0.0000365,  0.0000368,  0.0000370, &
        0.0000372,  0.0000375,  0.0000377,  0.0000379,  0.0000382, &
        0.0000384,  0.0000386,  0.0000388,  0.0000390,  0.0000392, &
        0.0000394,  0.0000396/
      data ((coa(i,j),i=1,62),j=  8,  8)/ &
        0.0000132,  0.0000147,  0.0000160,  0.0000172,  0.0000183, &
        0.0000193,  0.0000202,  0.0000210,  0.0000218,  0.0000226, &
        0.0000233,  0.0000240,  0.0000246,  0.0000252,  0.0000258, &
        0.0000264,  0.0000269,  0.0000274,  0.0000279,  0.0000284, &
        0.0000289,  0.0000293,  0.0000298,  0.0000302,  0.0000306, &
        0.0000310,  0.0000314,  0.0000318,  0.0000321,  0.0000325, &
        0.0000328,  0.0000332,  0.0000335,  0.0000331,  0.0000342, &
        0.0000345,  0.0000348,  0.0000351,  0.0000354,  0.0000357, &
        0.0000360,  0.0000363,  0.0000365,  0.0000368,  0.0000371, &
        0.0000373,  0.0000376,  0.0000378,  0.0000381,  0.0000383, &
        0.0000386,  0.0000388,  0.0000391,  0.0000393,  0.0000395, &
        0.0000397,  0.0000400,  0.0000402,  0.0000404,  0.0000406, &
        0.0000408,  0.0000411/
      data ((coa(i,j),i=1,62),j=  9,  9)/ &
        0.0000143,  0.0000158,  0.0000172,  0.0000184,  0.0000195, &
        0.0000206,  0.0000215,  0.0000224,  0.0000232,  0.0000240, &
        0.0000247,  0.0000254,  0.0000261,  0.0000267,  0.0000273, &
        0.0000279,  0.0000284,  0.0000290,  0.0000295,  0.0000300, &
        0.0000305,  0.0000309,  0.0000314,  0.0000318,  0.0000322, &
        0.0000326,  0.0000330,  0.0000334,  0.0000331,  0.0000342, &
        0.0000345,  0.0000349,  0.0000352,  0.0000356,  0.0000359, &
        0.0000362,  0.0000365,  0.0000368,  0.0000371,  0.0000374, &
        0.0000377,  0.0000380,  0.0000383,  0.0000386,  0.0000389, &
        0.0000391,  0.0000394,  0.0000397,  0.0000399,  0.0000402, &
        0.0000404,  0.0000407,  0.0000409,  0.0000412,  0.0000414, &
        0.0000416,  0.0000419,  0.0000421,  0.0000423,  0.0000426, &
        0.0000428,  0.0000430/
      data ((coa(i,j),i=1,62),j= 10, 10)/ &
        0.0000153,  0.0000169,  0.0000183,  0.0000196,  0.0000207, &
        0.0000218,  0.0000227,  0.0000236,  0.0000245,  0.0000253, &
        0.0000260,  0.0000267,  0.0000274,  0.0000281,  0.0000287, &
        0.0000293,  0.0000298,  0.0000304,  0.0000309,  0.0000314, &
        0.0000319,  0.0000324,  0.0000328,  0.0000333,  0.0000330, &
        0.0000341,  0.0000345,  0.0000349,  0.0000353,  0.0000357, &
        0.0000361,  0.0000364,  0.0000368,  0.0000371,  0.0000375, &
        0.0000378,  0.0000381,  0.0000384,  0.0000387,  0.0000391, &
        0.0000394,  0.0000397,  0.0000399,  0.0000402,  0.0000405, &
        0.0000408,  0.0000411,  0.0000413,  0.0000416,  0.0000419, &
        0.0000421,  0.0000424,  0.0000426,  0.0000429,  0.0000431, &
        0.0000434,  0.0000436,  0.0000439,  0.0000441,  0.0000443, &
        0.0000446,  0.0000448/
      data ((coa(i,j),i=1,62),j= 11, 11)/ &
        0.0000165,  0.0000182,  0.0000196,  0.0000209,  0.0000221, &
        0.0000232,  0.0000242,  0.0000251,  0.0000260,  0.0000268, &
        0.0000276,  0.0000283,  0.0000290,  0.0000297,  0.0000303, &
        0.0000309,  0.0000315,  0.0000321,  0.0000326,  0.0000331, &
        0.0000336,  0.0000341,  0.0000346,  0.0000350,  0.0000355, &
        0.0000359,  0.0000363,  0.0000367,  0.0000371,  0.0000375, &
        0.0000379,  0.0000383,  0.0000386,  0.0000390,  0.0000394, &
        0.0000397,  0.0000400,  0.0000404,  0.0000407,  0.0000410, &
        0.0000413,  0.0000416,  0.0000419,  0.0000422,  0.0000425, &
        0.0000428,  0.0000431,  0.0000434,  0.0000437,  0.0000439, &
        0.0000442,  0.0000445,  0.0000447,  0.0000450,  0.0000453, &
        0.0000455,  0.0000458,  0.0000460,  0.0000463,  0.0000465, &
        0.0000468,  0.0000470/
      data ((coa(i,j),i=1,62),j= 12, 12)/ &
        0.0000173,  0.0000190,  0.0000205,  0.0000219,  0.0000231, &
        0.0000242,  0.0000252,  0.0000262,  0.0000271,  0.0000279, &
        0.0000287,  0.0000294,  0.0000301,  0.0000308,  0.0000314, &
        0.0000320,  0.0000326,  0.0000332,  0.0000330,  0.0000343, &
        0.0000348,  0.0000353,  0.0000358,  0.0000362,  0.0000367, &
        0.0000371,  0.0000376,  0.0000380,  0.0000384,  0.0000388, &
        0.0000392,  0.0000396,  0.0000399,  0.0000403,  0.0000407, &
        0.0000410,  0.0000414,  0.0000417,  0.0000420,  0.0000424, &
        0.0000427,  0.0000430,  0.0000433,  0.0000436,  0.0000439, &
        0.0000442,  0.0000445,  0.0000448,  0.0000451,  0.0000454, &
        0.0000457,  0.0000459,  0.0000462,  0.0000465,  0.0000468, &
        0.0000470,  0.0000473,  0.0000475,  0.0000478,  0.0000481, &
        0.0000483,  0.0000486/
      data ((coa(i,j),i=1,62),j= 13, 13)/ &
        0.0000186,  0.0000204,  0.0000219,  0.0000233,  0.0000246, &
        0.0000257,  0.0000268,  0.0000277,  0.0000286,  0.0000295, &
        0.0000303,  0.0000311,  0.0000318,  0.0000325,  0.0000331, &
        0.0000331,  0.0000344,  0.0000350,  0.0000355,  0.0000361, &
        0.0000366,  0.0000371,  0.0000376,  0.0000381,  0.0000386, &
        0.0000390,  0.0000395,  0.0000399,  0.0000403,  0.0000407, &
        0.0000412,  0.0000416,  0.0000419,  0.0000423,  0.0000427, &
        0.0000431,  0.0000434,  0.0000438,  0.0000441,  0.0000445, &
        0.0000448,  0.0000451,  0.0000455,  0.0000458,  0.0000461, &
        0.0000464,  0.0000467,  0.0000470,  0.0000473,  0.0000476, &
        0.0000479,  0.0000482,  0.0000485,  0.0000488,  0.0000491, &
        0.0000494,  0.0000497,  0.0000499,  0.0000502,  0.0000505, &
        0.0000507,  0.0000510/
      data ((coa(i,j),i=1,62),j= 14, 14)/ &
        0.0000198,  0.0000216,  0.0000232,  0.0000246,  0.0000259, &
        0.0000271,  0.0000281,  0.0000291,  0.0000301,  0.0000310, &
        0.0000318,  0.0000326,  0.0000333,  0.0000340,  0.0000347, &
        0.0000354,  0.0000360,  0.0000366,  0.0000372,  0.0000377, &
        0.0000383,  0.0000388,  0.0000393,  0.0000398,  0.0000403, &
        0.0000408,  0.0000412,  0.0000417,  0.0000421,  0.0000425, &
        0.0000430,  0.0000434,  0.0000438,  0.0000442,  0.0000446, &
        0.0000449,  0.0000453,  0.0000457,  0.0000461,  0.0000464, &
        0.0000468,  0.0000471,  0.0000475,  0.0000478,  0.0000481, &
        0.0000485,  0.0000488,  0.0000491,  0.0000494,  0.0000498, &
        0.0000501,  0.0000504,  0.0000507,  0.0000510,  0.0000513, &
        0.0000516,  0.0000519,  0.0000522,  0.0000524,  0.0000527, &
        0.0000530,  0.0000533/
      data ((coa(i,j),i=1,62),j= 15, 15)/ &
        0.0000209,  0.0000228,  0.0000244,  0.0000258,  0.0000271, &
        0.0000283,  0.0000294,  0.0000305,  0.0000314,  0.0000323, &
        0.0000332,  0.0000340,  0.0000347,  0.0000354,  0.0000361, &
        0.0000368,  0.0000375,  0.0000381,  0.0000387,  0.0000392, &
        0.0000398,  0.0000404,  0.0000409,  0.0000414,  0.0000419, &
        0.0000424,  0.0000429,  0.0000433,  0.0000438,  0.0000442, &
        0.0000447,  0.0000451,  0.0000455,  0.0000459,  0.0000463, &
        0.0000467,  0.0000471,  0.0000475,  0.0000479,  0.0000483, &
        0.0000486,  0.0000490,  0.0000493,  0.0000497,  0.0000501, &
        0.0000504,  0.0000507,  0.0000511,  0.0000514,  0.0000518, &
        0.0000521,  0.0000524,  0.0000527,  0.0000530,  0.0000534, &
        0.0000537,  0.0000540,  0.0000543,  0.0000546,  0.0000549, &
        0.0000552,  0.0000555/
      data ((coa(i,j),i=1,62),j= 16, 16)/ &
        0.0000221,  0.0000240,  0.0000257,  0.0000272,  0.0000285, &
        0.0000297,  0.0000308,  0.0000319,  0.0000329,  0.0000331, &
        0.0000347,  0.0000355,  0.0000363,  0.0000370,  0.0000377, &
        0.0000384,  0.0000391,  0.0000397,  0.0000404,  0.0000409, &
        0.0000415,  0.0000421,  0.0000426,  0.0000432,  0.0000437, &
        0.0000442,  0.0000447,  0.0000452,  0.0000456,  0.0000461, &
        0.0000466,  0.0000470,  0.0000475,  0.0000479,  0.0000483, &
        0.0000487,  0.0000491,  0.0000496,  0.0000500,  0.0000503, &
        0.0000507,  0.0000511,  0.0000515,  0.0000519,  0.0000523, &
        0.0000526,  0.0000530,  0.0000533,  0.0000537,  0.0000540, &
        0.0000544,  0.0000547,  0.0000551,  0.0000554,  0.0000558, &
        0.0000561,  0.0000564,  0.0000567,  0.0000571,  0.0000574, &
        0.0000577,  0.0000580/
      data ((coa(i,j),i=1,62),j= 17, 17)/ &
        0.0000234,  0.0000254,  0.0000271,  0.0000286,  0.0000300, &
        0.0000312,  0.0000324,  0.0000335,  0.0000345,  0.0000354, &
        0.0000363,  0.0000372,  0.0000380,  0.0000387,  0.0000395, &
        0.0000402,  0.0000409,  0.0000415,  0.0000422,  0.0000428, &
        0.0000434,  0.0000440,  0.0000446,  0.0000451,  0.0000457, &
        0.0000462,  0.0000467,  0.0000472,  0.0000477,  0.0000482, &
        0.0000487,  0.0000492,  0.0000496,  0.0000501,  0.0000505, &
        0.0000510,  0.0000514,  0.0000518,  0.0000523,  0.0000527, &
        0.0000531,  0.0000535,  0.0000539,  0.0000543,  0.0000547, &
        0.0000551,  0.0000555,  0.0000559,  0.0000562,  0.0000566, &
        0.0000570,  0.0000573,  0.0000577,  0.0000581,  0.0000584, &
        0.0000588,  0.0000591,  0.0000595,  0.0000598,  0.0000602, &
        0.0000605,  0.0000608/
      data ((coa(i,j),i=1,62),j= 18, 18)/ &
        0.0000248,  0.0000268,  0.0000285,  0.0000301,  0.0000315, &
        0.0000328,  0.0000340,  0.0000351,  0.0000362,  0.0000371, &
        0.0000381,  0.0000389,  0.0000398,  0.0000406,  0.0000413, &
        0.0000421,  0.0000428,  0.0000435,  0.0000442,  0.0000448, &
        0.0000454,  0.0000460,  0.0000466,  0.0000472,  0.0000478, &
        0.0000484,  0.0000489,  0.0000494,  0.0000500,  0.0000505, &
        0.0000510,  0.0000515,  0.0000520,  0.0000525,  0.0000530, &
        0.0000534,  0.0000539,  0.0000544,  0.0000548,  0.0000553, &
        0.0000557,  0.0000561,  0.0000566,  0.0000570,  0.0000574, &
        0.0000578,  0.0000582,  0.0000586,  0.0000590,  0.0000594, &
        0.0000598,  0.0000602,  0.0000606,  0.0000610,  0.0000614, &
        0.0000618,  0.0000621,  0.0000625,  0.0000629,  0.0000633, &
        0.0000636,  0.0000640/
      data ((coa(i,j),i=1,62),j= 19, 19)/ &
        0.0000260,  0.0000281,  0.0000299,  0.0000315,  0.0000330, &
        0.0000343,  0.0000355,  0.0000367,  0.0000377,  0.0000388, &
        0.0000397,  0.0000406,  0.0000415,  0.0000423,  0.0000431, &
        0.0000439,  0.0000446,  0.0000453,  0.0000460,  0.0000467, &
        0.0000474,  0.0000480,  0.0000487,  0.0000493,  0.0000499, &
        0.0000505,  0.0000510,  0.0000516,  0.0000522,  0.0000527, &
        0.0000533,  0.0000538,  0.0000543,  0.0000548,  0.0000553, &
        0.0000558,  0.0000563,  0.0000568,  0.0000573,  0.0000578, &
        0.0000582,  0.0000587,  0.0000591,  0.0000596,  0.0000601, &
        0.0000605,  0.0000609,  0.0000614,  0.0000618,  0.0000622, &
        0.0000626,  0.0000631,  0.0000635,  0.0000639,  0.0000643, &
        0.0000647,  0.0000651,  0.0000655,  0.0000659,  0.0000663, &
        0.0000667,  0.0000670/
      data ((coa(i,j),i=1,62),j= 20, 20)/ &
        0.0000275,  0.0000296,  0.0000315,  0.0000332,  0.0000347, &
        0.0000360,  0.0000373,  0.0000385,  0.0000396,  0.0000407, &
        0.0000417,  0.0000426,  0.0000435,  0.0000444,  0.0000452, &
        0.0000460,  0.0000468,  0.0000476,  0.0000483,  0.0000490, &
        0.0000497,  0.0000504,  0.0000511,  0.0000517,  0.0000524, &
        0.0000530,  0.0000536,  0.0000542,  0.0000548,  0.0000554, &
        0.0000560,  0.0000566,  0.0000571,  0.0000577,  0.0000582, &
        0.0000587,  0.0000593,  0.0000598,  0.0000603,  0.0000608, &
        0.0000613,  0.0000618,  0.0000623,  0.0000628,  0.0000633, &
        0.0000638,  0.0000642,  0.0000647,  0.0000652,  0.0000656, &
        0.0000661,  0.0000665,  0.0000670,  0.0000674,  0.0000678, &
        0.0000683,  0.0000687,  0.0000691,  0.0000695,  0.0000700, &
        0.0000704,  0.0000708/
      data ((coa(i,j),i=1,62),j= 21, 21)/ &
        0.0000290,  0.0000312,  0.0000331,  0.0000349,  0.0000364, &
        0.0000379,  0.0000392,  0.0000404,  0.0000416,  0.0000427, &
        0.0000437,  0.0000447,  0.0000457,  0.0000466,  0.0000475, &
        0.0000483,  0.0000492,  0.0000500,  0.0000507,  0.0000515, &
        0.0000523,  0.0000530,  0.0000537,  0.0000544,  0.0000551, &
        0.0000558,  0.0000564,  0.0000571,  0.0000577,  0.0000583, &
        0.0000589,  0.0000596,  0.0000602,  0.0000607,  0.0000613, &
        0.0000619,  0.0000625,  0.0000630,  0.0000636,  0.0000641, &
        0.0000647,  0.0000652,  0.0000657,  0.0000663,  0.0000668, &
        0.0000673,  0.0000678,  0.0000683,  0.0000688,  0.0000693, &
        0.0000698,  0.0000702,  0.0000707,  0.0000712,  0.0000716, &
        0.0000721,  0.0000726,  0.0000730,  0.0000735,  0.0000739, &
        0.0000744,  0.0000748/
      data ((coa(i,j),i=1,62),j= 22, 22)/ &
        0.0000306,  0.0000329,  0.0000349,  0.0000366,  0.0000383, &
        0.0000398,  0.0000411,  0.0000424,  0.0000436,  0.0000448, &
        0.0000459,  0.0000469,  0.0000479,  0.0000489,  0.0000499, &
        0.0000508,  0.0000516,  0.0000525,  0.0000533,  0.0000542, &
        0.0000549,  0.0000557,  0.0000565,  0.0000572,  0.0000580, &
        0.0000587,  0.0000594,  0.0000601,  0.0000608,  0.0000615, &
        0.0000621,  0.0000628,  0.0000634,  0.0000640,  0.0000647, &
        0.0000653,  0.0000659,  0.0000665,  0.0000671,  0.0000677, &
        0.0000683,  0.0000688,  0.0000694,  0.0000700,  0.0000705, &
        0.0000711,  0.0000716,  0.0000721,  0.0000727,  0.0000732, &
        0.0000737,  0.0000742,  0.0000747,  0.0000752,  0.0000757, &
        0.0000762,  0.0000767,  0.0000772,  0.0000777,  0.0000782, &
        0.0000786,  0.0000791/
      data ((coa(i,j),i=1,62),j= 23, 23)/ &
        0.0000323,  0.0000347,  0.0000368,  0.0000386,  0.0000403, &
        0.0000419,  0.0000433,  0.0000447,  0.0000459,  0.0000472, &
        0.0000483,  0.0000494,  0.0000505,  0.0000516,  0.0000526, &
        0.0000535,  0.0000545,  0.0000554,  0.0000563,  0.0000572, &
        0.0000580,  0.0000589,  0.0000597,  0.0000605,  0.0000613, &
        0.0000621,  0.0000628,  0.0000636,  0.0000643,  0.0000650, &
        0.0000657,  0.0000664,  0.0000671,  0.0000678,  0.0000685, &
        0.0000692,  0.0000698,  0.0000705,  0.0000711,  0.0000717, &
        0.0000724,  0.0000730,  0.0000736,  0.0000742,  0.0000748, &
        0.0000754,  0.0000760,  0.0000765,  0.0000771,  0.0000777, &
        0.0000782,  0.0000788,  0.0000793,  0.0000799,  0.0000804, &
        0.0000809,  0.0000815,  0.0000820,  0.0000825,  0.0000830, &
        0.0000835,  0.0000840/
      data ((coa(i,j),i=1,62),j= 24, 24)/ &
        0.0000341,  0.0000365,  0.0000387,  0.0000406,  0.0000424, &
        0.0000440,  0.0000456,  0.0000470,  0.0000483,  0.0000496, &
        0.0000509,  0.0000521,  0.0000532,  0.0000543,  0.0000554, &
        0.0000564,  0.0000574,  0.0000584,  0.0000594,  0.0000603, &
        0.0000613,  0.0000622,  0.0000630,  0.0000639,  0.0000648, &
        0.0000656,  0.0000664,  0.0000672,  0.0000680,  0.0000688, &
        0.0000696,  0.0000703,  0.0000711,  0.0000718,  0.0000725, &
        0.0000732,  0.0000739,  0.0000746,  0.0000753,  0.0000760, &
        0.0000767,  0.0000773,  0.0000780,  0.0000786,  0.0000793, &
        0.0000799,  0.0000805,  0.0000811,  0.0000817,  0.0000823, &
        0.0000829,  0.0000835,  0.0000841,  0.0000847,  0.0000853, &
        0.0000858,  0.0000864,  0.0000870,  0.0000875,  0.0000881, &
        0.0000886,  0.0000892/
      data ((coa(i,j),i=1,62),j= 25, 25)/ &
        0.0000359,  0.0000385,  0.0000408,  0.0000428,  0.0000447, &
        0.0000464,  0.0000480,  0.0000495,  0.0000510,  0.0000524, &
        0.0000537,  0.0000550,  0.0000562,  0.0000574,  0.0000585, &
        0.0000597,  0.0000608,  0.0000618,  0.0000629,  0.0000639, &
        0.0000649,  0.0000658,  0.0000668,  0.0000677,  0.0000686, &
        0.0000695,  0.0000704,  0.0000713,  0.0000721,  0.0000730, &
        0.0000738,  0.0000746,  0.0000754,  0.0000762,  0.0000770, &
        0.0000777,  0.0000785,  0.0000792,  0.0000800,  0.0000807, &
        0.0000814,  0.0000821,  0.0000828,  0.0000835,  0.0000842, &
        0.0000849,  0.0000856,  0.0000862,  0.0000869,  0.0000875, &
        0.0000882,  0.0000888,  0.0000894,  0.0000900,  0.0000907, &
        0.0000913,  0.0000919,  0.0000925,  0.0000931,  0.0000936, &
        0.0000942,  0.0000948/
      data ((coa(i,j),i=1,62),j= 26, 26)/ &
        0.0000380,  0.0000407,  0.0000431,  0.0000453,  0.0000473, &
        0.0000491,  0.0000508,  0.0000525,  0.0000540,  0.0000555, &
        0.0000569,  0.0000583,  0.0000596,  0.0000609,  0.0000622, &
        0.0000634,  0.0000646,  0.0000657,  0.0000668,  0.0000679, &
        0.0000690,  0.0000700,  0.0000711,  0.0000721,  0.0000731, &
        0.0000740,  0.0000750,  0.0000759,  0.0000769,  0.0000778, &
        0.0000786,  0.0000795,  0.0000804,  0.0000812,  0.0000821, &
        0.0000829,  0.0000837,  0.0000845,  0.0000853,  0.0000861, &
        0.0000869,  0.0000876,  0.0000884,  0.0000891,  0.0000899, &
        0.0000906,  0.0000913,  0.0000920,  0.0000927,  0.0000934, &
        0.0000941,  0.0000948,  0.0000955,  0.0000961,  0.0000968, &
        0.0000974,  0.0000981,  0.0000987,  0.0000994,  0.0001000, &
        0.0001006,  0.0001012/
      data ((coa(i,j),i=1,62),j= 27, 27)/ &
        0.0000403,  0.0000431,  0.0000456,  0.0000479,  0.0000500, &
        0.0000520,  0.0000538,  0.0000556,  0.0000573,  0.0000589, &
        0.0000604,  0.0000619,  0.0000633,  0.0000647,  0.0000661, &
        0.0000674,  0.0000686,  0.0000699,  0.0000711,  0.0000723, &
        0.0000734,  0.0000746,  0.0000757,  0.0000768,  0.0000778, &
        0.0000789,  0.0000799,  0.0000809,  0.0000819,  0.0000829, &
        0.0000838,  0.0000848,  0.0000857,  0.0000866,  0.0000875, &
        0.0000884,  0.0000893,  0.0000902,  0.0000910,  0.0000919, &
        0.0000927,  0.0000935,  0.0000943,  0.0000951,  0.0000959, &
        0.0000967,  0.0000974,  0.0000982,  0.0000990,  0.0000997, &
        0.0001004,  0.0001012,  0.0001019,  0.0001026,  0.0001033, &
        0.0001040,  0.0001047,  0.0001054,  0.0001061,  0.0001067, &
        0.0001074,  0.0001080/
      data ((coa(i,j),i=1,62),j= 28, 28)/ &
        0.0000426,  0.0000456,  0.0000482,  0.0000507,  0.0000529, &
        0.0000550,  0.0000570,  0.0000589,  0.0000607,  0.0000624, &
        0.0000641,  0.0000657,  0.0000672,  0.0000687,  0.0000702, &
        0.0000716,  0.0000730,  0.0000743,  0.0000756,  0.0000769, &
        0.0000781,  0.0000794,  0.0000806,  0.0000817,  0.0000829, &
        0.0000840,  0.0000851,  0.0000862,  0.0000873,  0.0000883, &
        0.0000893,  0.0000904,  0.0000913,  0.0000923,  0.0000933, &
        0.0000943,  0.0000952,  0.0000961,  0.0000970,  0.0000979, &
        0.0000988,  0.0000997,  0.0001006,  0.0001014,  0.0001023, &
        0.0001031,  0.0001039,  0.0001047,  0.0001055,  0.0001063, &
        0.0001071,  0.0001079,  0.0001087,  0.0001094,  0.0001102, &
        0.0001109,  0.0001116,  0.0001124,  0.0001131,  0.0001138, &
        0.0001145,  0.0001152/
      data ((coa(i,j),i=1,62),j= 29, 29)/ &
        0.0000451,  0.0000482,  0.0000511,  0.0000537,  0.0000561, &
        0.0000584,  0.0000605,  0.0000626,  0.0000645,  0.0000664, &
        0.0000682,  0.0000699,  0.0000715,  0.0000732,  0.0000747, &
        0.0000763,  0.0000777,  0.0000792,  0.0000806,  0.0000820, &
        0.0000833,  0.0000846,  0.0000859,  0.0000872,  0.0000884, &
        0.0000896,  0.0000908,  0.0000920,  0.0000931,  0.0000942, &
        0.0000953,  0.0000964,  0.0000975,  0.0000986,  0.0000996, &
        0.0001006,  0.0001016,  0.0001026,  0.0001036,  0.0001046, &
        0.0001055,  0.0001064,  0.0001074,  0.0001083,  0.0001092, &
        0.0001101,  0.0001110,  0.0001118,  0.0001127,  0.0001135, &
        0.0001144,  0.0001152,  0.0001160,  0.0001168,  0.0001176, &
        0.0001184,  0.0001192,  0.0001200,  0.0001207,  0.0001215, &
        0.0001222,  0.0001230/
      data ((coa(i,j),i=1,62),j= 30, 30)/ &
        0.0000478,  0.0000512,  0.0000543,  0.0000571,  0.0000597, &
        0.0000621,  0.0000644,  0.0000666,  0.0000687,  0.0000708, &
        0.0000727,  0.0000746,  0.0000764,  0.0000781,  0.0000798, &
        0.0000814,  0.0000830,  0.0000846,  0.0000861,  0.0000876, &
        0.0000891,  0.0000905,  0.0000919,  0.0000932,  0.0000945, &
        0.0000958,  0.0000971,  0.0000984,  0.0000996,  0.0001008, &
        0.0001020,  0.0001032,  0.0001043,  0.0001055,  0.0001066, &
        0.0001077,  0.0001088,  0.0001098,  0.0001109,  0.0001119, &
        0.0001129,  0.0001139,  0.0001149,  0.0001159,  0.0001168, &
        0.0001178,  0.0001187,  0.0001197,  0.0001206,  0.0001215, &
        0.0001224,  0.0001233,  0.0001241,  0.0001250,  0.0001258, &
        0.0001267,  0.0001275,  0.0001283,  0.0001292,  0.0001300, &
        0.0001308,  0.0001316/
      data ((coa(i,j),i=1,62),j= 31, 31)/ &
        0.0000508,  0.0000544,  0.0000577,  0.0000607,  0.0000635, &
        0.0000661,  0.0000686,  0.0000710,  0.0000733,  0.0000754, &
        0.0000775,  0.0000795,  0.0000815,  0.0000834,  0.0000852, &
        0.0000870,  0.0000887,  0.0000904,  0.0000920,  0.0000936, &
        0.0000952,  0.0000967,  0.0000982,  0.0000996,  0.0001011, &
        0.0001025,  0.0001038,  0.0001052,  0.0001065,  0.0001078, &
        0.0001091,  0.0001103,  0.0001116,  0.0001128,  0.0001140, &
        0.0001151,  0.0001163,  0.0001174,  0.0001186,  0.0001197, &
        0.0001207,  0.0001218,  0.0001229,  0.0001239,  0.0001249, &
        0.0001260,  0.0001270,  0.0001279,  0.0001289,  0.0001299, &
        0.0001308,  0.0001318,  0.0001327,  0.0001336,  0.0001317, &
        0.0001325,  0.0001363,  0.0001372,  0.0001380,  0.0001389, &
        0.0001397,  0.0001406/
      data ((coa(i,j),i=1,62),j= 32, 32)/ &
        0.0000540,  0.0000579,  0.0000615,  0.0000647,  0.0000677, &
        0.0000706,  0.0000733,  0.0000758,  0.0000783,  0.0000806, &
        0.0000829,  0.0000851,  0.0000872,  0.0000892,  0.0000912, &
        0.0000931,  0.0000950,  0.0000968,  0.0000985,  0.0001003, &
        0.0001020,  0.0001036,  0.0001052,  0.0001068,  0.0001083, &
        0.0001098,  0.0001113,  0.0001127,  0.0001142,  0.0001156, &
        0.0001169,  0.0001183,  0.0001196,  0.0001209,  0.0001222, &
        0.0001234,  0.0001246,  0.0001259,  0.0001270,  0.0001282, &
        0.0001294,  0.0001305,  0.0001317,  0.0001328,  0.0001339, &
        0.0001321,  0.0001360,  0.0001371,  0.0001381,  0.0001391, &
        0.0001401,  0.0001411,  0.0001421,  0.0001431,  0.0001440, &
        0.0001450,  0.0001459,  0.0001469,  0.0001478,  0.0001487, &
        0.0001496,  0.0001505/
      data ((coa(i,j),i=1,62),j= 33, 33)/ &
        0.0000575,  0.0000617,  0.0000655,  0.0000690,  0.0000723, &
        0.0000754,  0.0000783,  0.0000810,  0.0000837,  0.0000862, &
        0.0000887,  0.0000910,  0.0000933,  0.0000955,  0.0000976, &
        0.0000997,  0.0001017,  0.0001036,  0.0001055,  0.0001074, &
        0.0001092,  0.0001110,  0.0001127,  0.0001144,  0.0001160, &
        0.0001176,  0.0001192,  0.0001208,  0.0001223,  0.0001238, &
        0.0001252,  0.0001267,  0.0001281,  0.0001295,  0.0001308, &
        0.0001322,  0.0001335,  0.0001319,  0.0001360,  0.0001373, &
        0.0001385,  0.0001397,  0.0001409,  0.0001421,  0.0001433, &
        0.0001444,  0.0001456,  0.0001467,  0.0001478,  0.0001489, &
        0.0001499,  0.0001510,  0.0001520,  0.0001531,  0.0001541, &
        0.0001551,  0.0001561,  0.0001571,  0.0001581,  0.0001590, &
        0.0001600,  0.0001609/
      data ((coa(i,j),i=1,62),j= 34, 34)/ &
        0.0000613,  0.0000659,  0.0000700,  0.0000738,  0.0000773, &
        0.0000806,  0.0000838,  0.0000868,  0.0000896,  0.0000924, &
        0.0000950,  0.0000976,  0.0001000,  0.0001024,  0.0001047, &
        0.0001069,  0.0001091,  0.0001112,  0.0001132,  0.0001152, &
        0.0001172,  0.0001191,  0.0001209,  0.0001227,  0.0001245, &
        0.0001262,  0.0001279,  0.0001296,  0.0001312,  0.0001328, &
        0.0001344,  0.0001359,  0.0001374,  0.0001389,  0.0001403, &
        0.0001417,  0.0001432,  0.0001445,  0.0001459,  0.0001472, &
        0.0001485,  0.0001498,  0.0001511,  0.0001524,  0.0001536, &
        0.0001548,  0.0001560,  0.0001572,  0.0001584,  0.0001595, &
        0.0001607,  0.0001618,  0.0001629,  0.0001640,  0.0001651, &
        0.0001661,  0.0001672,  0.0001682,  0.0001693,  0.0001703, &
        0.0001713,  0.0001723/
      data ((coa(i,j),i=1,62),j= 35, 35)/ &
        0.0000654,  0.0000703,  0.0000747,  0.0000789,  0.0000827, &
        0.0000863,  0.0000897,  0.0000929,  0.0000960,  0.0000990, &
        0.0001018,  0.0001046,  0.0001072,  0.0001098,  0.0001123, &
        0.0001147,  0.0001170,  0.0001193,  0.0001214,  0.0001236, &
        0.0001257,  0.0001277,  0.0001297,  0.0001316,  0.0001335, &
        0.0001325,  0.0001372,  0.0001389,  0.0001407,  0.0001424, &
        0.0001440,  0.0001457,  0.0001473,  0.0001488,  0.0001504, &
        0.0001519,  0.0001534,  0.0001548,  0.0001563,  0.0001577, &
        0.0001591,  0.0001605,  0.0001618,  0.0001631,  0.0001645, &
        0.0001658,  0.0001670,  0.0001683,  0.0001695,  0.0001707, &
        0.0001720,  0.0001732,  0.0001743,  0.0001755,  0.0001767, &
        0.0001778,  0.0001789,  0.0001800,  0.0001811,  0.0001822, &
        0.0001833,  0.0001844/
      data ((coa(i,j),i=1,62),j= 36, 36)/ &
        0.0000699,  0.0000752,  0.0000800,  0.0000844,  0.0000886, &
        0.0000925,  0.0000962,  0.0000997,  0.0001030,  0.0001062, &
        0.0001093,  0.0001123,  0.0001151,  0.0001179,  0.0001205, &
        0.0001231,  0.0001256,  0.0001280,  0.0001304,  0.0001327, &
        0.0001321,  0.0001371,  0.0001392,  0.0001413,  0.0001433, &
        0.0001453,  0.0001472,  0.0001491,  0.0001509,  0.0001527, &
        0.0001545,  0.0001562,  0.0001579,  0.0001596,  0.0001612, &
        0.0001629,  0.0001644,  0.0001660,  0.0001675,  0.0001690, &
        0.0001705,  0.0001720,  0.0001734,  0.0001749,  0.0001762, &
        0.0001776,  0.0001790,  0.0001803,  0.0001817,  0.0001830, &
        0.0001842,  0.0001855,  0.0001868,  0.0001880,  0.0001892, &
        0.0001905,  0.0001917,  0.0001928,  0.0001940,  0.0001952, &
        0.0001963,  0.0001975/
      data ((coa(i,j),i=1,62),j= 37, 37)/ &
        0.0000748,  0.0000805,  0.0000858,  0.0000906,  0.0000951, &
        0.0000993,  0.0001033,  0.0001071,  0.0001107,  0.0001142, &
        0.0001175,  0.0001207,  0.0001238,  0.0001267,  0.0001296, &
        0.0001323,  0.0001322,  0.0001376,  0.0001401,  0.0001426, &
        0.0001450,  0.0001473,  0.0001496,  0.0001518,  0.0001539, &
        0.0001560,  0.0001581,  0.0001601,  0.0001620,  0.0001640, &
        0.0001659,  0.0001677,  0.0001695,  0.0001713,  0.0001731, &
        0.0001748,  0.0001765,  0.0001781,  0.0001798,  0.0001814, &
        0.0001830,  0.0001845,  0.0001861,  0.0001876,  0.0001891, &
        0.0001905,  0.0001920,  0.0001934,  0.0001948,  0.0001962, &
        0.0001976,  0.0001990,  0.0002003,  0.0002017,  0.0002030, &
        0.0002043,  0.0002056,  0.0002068,  0.0002081,  0.0002093, &
        0.0002106,  0.0002118/
      data ((coa(i,j),i=1,62),j= 38, 38)/ &
        0.0000802,  0.0000863,  0.0000920,  0.0000972,  0.0001021, &
        0.0001067,  0.0001110,  0.0001151,  0.0001190,  0.0001227, &
        0.0001263,  0.0001297,  0.0001330,  0.0001362,  0.0001393, &
        0.0001422,  0.0001451,  0.0001479,  0.0001506,  0.0001532, &
        0.0001557,  0.0001582,  0.0001606,  0.0001630,  0.0001653, &
        0.0001675,  0.0001697,  0.0001719,  0.0001740,  0.0001760, &
        0.0001780,  0.0001800,  0.0001819,  0.0001839,  0.0001857, &
        0.0001876,  0.0001894,  0.0001911,  0.0001929,  0.0001946, &
        0.0001963,  0.0001980,  0.0001996,  0.0002012,  0.0002028, &
        0.0002044,  0.0002060,  0.0002075,  0.0002090,  0.0002105, &
        0.0002120,  0.0002135,  0.0002149,  0.0002164,  0.0002178, &
        0.0002192,  0.0002205,  0.0002219,  0.0002233,  0.0002246, &
        0.0002259,  0.0002273/
      data ((coa(i,j),i=1,62),j= 39, 39)/ &
        0.0000859,  0.0000926,  0.0000987,  0.0001044,  0.0001097, &
        0.0001146,  0.0001193,  0.0001237,  0.0001279,  0.0001319, &
        0.0001358,  0.0001395,  0.0001430,  0.0001464,  0.0001497, &
        0.0001528,  0.0001559,  0.0001589,  0.0001617,  0.0001645, &
        0.0001673,  0.0001699,  0.0001725,  0.0001750,  0.0001774, &
        0.0001798,  0.0001822,  0.0001845,  0.0001867,  0.0001889, &
        0.0001911,  0.0001932,  0.0001953,  0.0001973,  0.0001993, &
        0.0002013,  0.0002032,  0.0002051,  0.0002070,  0.0002088, &
        0.0002107,  0.0002124,  0.0002142,  0.0002160,  0.0002177, &
        0.0002194,  0.0002211,  0.0002227,  0.0002243,  0.0002260, &
        0.0002276,  0.0002291,  0.0002307,  0.0002322,  0.0002338, &
        0.0002353,  0.0002368,  0.0002382,  0.0002397,  0.0002412, &
        0.0002426,  0.0002440/
      data ((coa(i,j),i=1,62),j= 40, 40)/ &
        0.0000922,  0.0000995,  0.0001061,  0.0001122,  0.0001179, &
        0.0001233,  0.0001283,  0.0001331,  0.0001376,  0.0001419, &
        0.0001460,  0.0001500,  0.0001538,  0.0001574,  0.0001609, &
        0.0001643,  0.0001676,  0.0001707,  0.0001738,  0.0001768, &
        0.0001797,  0.0001825,  0.0001853,  0.0001880,  0.0001906, &
        0.0001932,  0.0001957,  0.0001981,  0.0002006,  0.0002029, &
        0.0002052,  0.0002075,  0.0002097,  0.0002119,  0.0002141, &
        0.0002162,  0.0002183,  0.0002203,  0.0002223,  0.0002243, &
        0.0002263,  0.0002282,  0.0002301,  0.0002320,  0.0002339, &
        0.0002357,  0.0002375,  0.0002393,  0.0002411,  0.0002428, &
        0.0002446,  0.0002463,  0.0002480,  0.0002496,  0.0002513, &
        0.0002529,  0.0002546,  0.0002562,  0.0002578,  0.0002593, &
        0.0002609,  0.0002625/
      data ((coa(i,j),i=1,62),j= 41, 41)/ &
        0.0000990,  0.0001069,  0.0001141,  0.0001207,  0.0001268, &
        0.0001326,  0.0001380,  0.0001431,  0.0001480,  0.0001526, &
        0.0001570,  0.0001612,  0.0001653,  0.0001692,  0.0001729, &
        0.0001766,  0.0001801,  0.0001835,  0.0001868,  0.0001900, &
        0.0001931,  0.0001961,  0.0001991,  0.0002019,  0.0002048, &
        0.0002075,  0.0002102,  0.0002129,  0.0002154,  0.0002180, &
        0.0002205,  0.0002229,  0.0002253,  0.0002277,  0.0002300, &
        0.0002323,  0.0002346,  0.0002368,  0.0002390,  0.0002411, &
        0.0002432,  0.0002453,  0.0002474,  0.0002494,  0.0002515, &
        0.0002535,  0.0002554,  0.0002574,  0.0002593,  0.0002612, &
        0.0002631,  0.0002649,  0.0002668,  0.0002686,  0.0002704, &
        0.0002722,  0.0002740,  0.0002757,  0.0002775,  0.0002792, &
        0.0002809,  0.0002826/
      data ((coa(i,j),i=1,62),j= 42, 42)/ &
        0.0001063,  0.0001148,  0.0001226,  0.0001297,  0.0001363, &
        0.0001425,  0.0001483,  0.0001538,  0.0001590,  0.0001639, &
        0.0001687,  0.0001732,  0.0001775,  0.0001817,  0.0001857, &
        0.0001896,  0.0001933,  0.0001970,  0.0002005,  0.0002039, &
        0.0002073,  0.0002105,  0.0002137,  0.0002168,  0.0002198, &
        0.0002228,  0.0002257,  0.0002286,  0.0002314,  0.0002341, &
        0.0002368,  0.0002394,  0.0002420,  0.0002446,  0.0002471, &
        0.0002496,  0.0002520,  0.0002544,  0.0002568,  0.0002591, &
        0.0002615,  0.0002637,  0.0002660,  0.0002682,  0.0002704, &
        0.0002726,  0.0002747,  0.0002768,  0.0002789,  0.0002810, &
        0.0002831,  0.0002851,  0.0002871,  0.0002891,  0.0002911, &
        0.0002930,  0.0002950,  0.0002969,  0.0002988,  0.0003007, &
        0.0003025,  0.0003044/
      data ((coa(i,j),i=1,62),j= 43, 43)/ &
        0.0001141,  0.0001233,  0.0001316,  0.0001393,  0.0001464, &
        0.0001531,  0.0001593,  0.0001652,  0.0001707,  0.0001760, &
        0.0001811,  0.0001859,  0.0001905,  0.0001950,  0.0001993, &
        0.0002035,  0.0002075,  0.0002114,  0.0002152,  0.0002189, &
        0.0002225,  0.0002260,  0.0002294,  0.0002328,  0.0002360, &
        0.0002393,  0.0002424,  0.0002455,  0.0002485,  0.0002515, &
        0.0002544,  0.0002573,  0.0002601,  0.0002629,  0.0002656, &
        0.0002683,  0.0002709,  0.0002736,  0.0002762,  0.0002787, &
        0.0002812,  0.0002837,  0.0002862,  0.0002886,  0.0002910, &
        0.0002934,  0.0002957,  0.0002980,  0.0003003,  0.0003026, &
        0.0003048,  0.0003071,  0.0003093,  0.0003114,  0.0003136, &
        0.0003157,  0.0003179,  0.0003200,  0.0003221,  0.0003241, &
        0.0003262,  0.0003282/
      data ((coa(i,j),i=1,62),j= 44, 44)/ &
        0.0001224,  0.0001323,  0.0001413,  0.0001496,  0.0001572, &
        0.0001643,  0.0001709,  0.0001772,  0.0001832,  0.0001888, &
        0.0001943,  0.0001994,  0.0002044,  0.0002092,  0.0002138, &
        0.0002183,  0.0002226,  0.0002269,  0.0002309,  0.0002349, &
        0.0002388,  0.0002426,  0.0002463,  0.0002499,  0.0002535, &
        0.0002570,  0.0002604,  0.0002637,  0.0002670,  0.0002702, &
        0.0002734,  0.0002765,  0.0002796,  0.0002826,  0.0002856, &
        0.0002886,  0.0002915,  0.0002943,  0.0002972,  0.0002999, &
        0.0003027,  0.0003054,  0.0003081,  0.0003108,  0.0003134, &
        0.0003160,  0.0003185,  0.0003211,  0.0003236,  0.0003261, &
        0.0003286,  0.0003310,  0.0003334,  0.0003358,  0.0003382, &
        0.0003405,  0.0003428,  0.0003451,  0.0003474,  0.0003497, &
        0.0003519,  0.0003542/
      data ((coa(i,j),i=1,62),j= 45, 45)/ &
        0.0001312,  0.0001419,  0.0001515,  0.0001603,  0.0001685, &
        0.0001761,  0.0001832,  0.0001899,  0.0001963,  0.0002024, &
        0.0002082,  0.0002138,  0.0002191,  0.0002243,  0.0002292, &
        0.0002341,  0.0002387,  0.0002433,  0.0002477,  0.0002520, &
        0.0002562,  0.0002603,  0.0002644,  0.0002683,  0.0002722, &
        0.0002759,  0.0002796,  0.0002833,  0.0002869,  0.0002904, &
        0.0002939,  0.0002973,  0.0003006,  0.0003039,  0.0003072, &
        0.0003104,  0.0003136,  0.0003167,  0.0003198,  0.0003228, &
        0.0003259,  0.0003288,  0.0003318,  0.0003347,  0.0003376, &
        0.0003404,  0.0003432,  0.0003460,  0.0003487,  0.0003515, &
        0.0003542,  0.0003568,  0.0003595,  0.0003621,  0.0003647, &
        0.0003673,  0.0003698,  0.0003724,  0.0003749,  0.0003773, &
        0.0003798,  0.0003822/
      data ((coa(i,j),i=1,62),j= 46, 46)/ &
        0.0001406,  0.0001520,  0.0001623,  0.0001718,  0.0001805, &
        0.0001886,  0.0001963,  0.0002035,  0.0002103,  0.0002168, &
        0.0002231,  0.0002291,  0.0002348,  0.0002404,  0.0002458, &
        0.0002510,  0.0002561,  0.0002610,  0.0002658,  0.0002705, &
        0.0002750,  0.0002795,  0.0002839,  0.0002882,  0.0002924, &
        0.0002965,  0.0003005,  0.0003045,  0.0003084,  0.0003123, &
        0.0003161,  0.0003198,  0.0003235,  0.0003271,  0.0003307, &
        0.0003342,  0.0003376,  0.0003411,  0.0003445,  0.0003478, &
        0.0003511,  0.0003544,  0.0003576,  0.0003608,  0.0003639, &
        0.0003670,  0.0003701,  0.0003731,  0.0003762,  0.0003791, &
        0.0003821,  0.0003850,  0.0003879,  0.0003908,  0.0003936, &
        0.0003965,  0.0003992,  0.0004020,  0.0004047,  0.0004075, &
        0.0004102,  0.0004128/
      data ((coa(i,j),i=1,62),j= 47, 47)/ &
        0.0001506,  0.0001628,  0.0001739,  0.0001840,  0.0001934, &
        0.0002021,  0.0002103,  0.0002180,  0.0002254,  0.0002324, &
        0.0002391,  0.0002456,  0.0002518,  0.0002579,  0.0002637, &
        0.0002694,  0.0002749,  0.0002802,  0.0002854,  0.0002905, &
        0.0002955,  0.0003004,  0.0003052,  0.0003099,  0.0003145, &
        0.0003190,  0.0003234,  0.0003278,  0.0003320,  0.0003362, &
        0.0003404,  0.0003445,  0.0003485,  0.0003524,  0.0003564, &
        0.0003602,  0.0003640,  0.0003678,  0.0003715,  0.0003751, &
        0.0003787,  0.0003823,  0.0003858,  0.0003893,  0.0003928, &
        0.0003962,  0.0003995,  0.0004029,  0.0004062,  0.0004094, &
        0.0004127,  0.0004159,  0.0004190,  0.0004222,  0.0004253, &
        0.0004283,  0.0004314,  0.0004344,  0.0004374,  0.0004404, &
        0.0004433,  0.0004462/
      data ((coa(i,j),i=1,62),j= 48, 48)/ &
        0.0001613,  0.0001744,  0.0001863,  0.0001971,  0.0002072, &
        0.0002165,  0.0002254,  0.0002337,  0.0002417,  0.0002493, &
        0.0002565,  0.0002636,  0.0002703,  0.0002769,  0.0002832, &
        0.0002894,  0.0002954,  0.0003013,  0.0003070,  0.0003125, &
        0.0003180,  0.0003233,  0.0003286,  0.0003337,  0.0003387, &
        0.0003436,  0.0003485,  0.0003533,  0.0003579,  0.0003625, &
        0.0003671,  0.0003716,  0.0003760,  0.0003803,  0.0003846, &
        0.0003888,  0.0003929,  0.0003971,  0.0004011,  0.0004051, &
        0.0004091,  0.0004130,  0.0004168,  0.0004206,  0.0004244, &
        0.0004281,  0.0004318,  0.0004354,  0.0004390,  0.0004426, &
        0.0004461,  0.0004496,  0.0004530,  0.0004565,  0.0004598, &
        0.0004632,  0.0004665,  0.0004698,  0.0004730,  0.0004763, &
        0.0004795,  0.0004826/
      data ((coa(i,j),i=1,62),j= 49, 49)/ &
        0.0001728,  0.0001868,  0.0001996,  0.0002112,  0.0002220, &
        0.0002321,  0.0002417,  0.0002507,  0.0002593,  0.0002676, &
        0.0002755,  0.0002831,  0.0002905,  0.0002977,  0.0003046, &
        0.0003113,  0.0003179,  0.0003243,  0.0003305,  0.0003366, &
        0.0003426,  0.0003484,  0.0003542,  0.0003598,  0.0003653, &
        0.0003707,  0.0003760,  0.0003812,  0.0003863,  0.0003914, &
        0.0003963,  0.0004012,  0.0004060,  0.0004108,  0.0004154, &
        0.0004201,  0.0004246,  0.0004291,  0.0004335,  0.0004379, &
        0.0004422,  0.0004464,  0.0004506,  0.0004548,  0.0004589, &
        0.0004629,  0.0004669,  0.0004709,  0.0004748,  0.0004787, &
        0.0004825,  0.0004863,  0.0004900,  0.0004937,  0.0004974, &
        0.0005010,  0.0005046,  0.0005082,  0.0005117,  0.0005152, &
        0.0005187,  0.0005221/
      data ((coa(i,j),i=1,62),j= 50, 50)/ &
        0.0001851,  0.0002003,  0.0002139,  0.0002265,  0.0002382, &
        0.0002491,  0.0002595,  0.0002693,  0.0002787,  0.0002877, &
        0.0002963,  0.0003047,  0.0003127,  0.0003205,  0.0003281, &
        0.0003355,  0.0003427,  0.0003497,  0.0003565,  0.0003632, &
        0.0003697,  0.0003761,  0.0003824,  0.0003885,  0.0003945, &
        0.0004004,  0.0004062,  0.0004119,  0.0004175,  0.0004230, &
        0.0004285,  0.0004338,  0.0004390,  0.0004442,  0.0004493, &
        0.0004543,  0.0004593,  0.0004641,  0.0004689,  0.0004737, &
        0.0004784,  0.0004830,  0.0004875,  0.0004920,  0.0004965, &
        0.0005009,  0.0005052,  0.0005095,  0.0005138,  0.0005179, &
        0.0005221,  0.0005262,  0.0005302,  0.0005342,  0.0005268, &
        0.0005421,  0.0005460,  0.0005499,  0.0005537,  0.0005574, &
        0.0005612,  0.0005649/
      data ((coa(i,j),i=1,62),j= 51, 51)/ &
        0.0001985,  0.0002149,  0.0002297,  0.0002433,  0.0002559, &
        0.0002679,  0.0002791,  0.0002898,  0.0003001,  0.0003099, &
        0.0003193,  0.0003285,  0.0003373,  0.0003459,  0.0003542, &
        0.0003622,  0.0003701,  0.0003778,  0.0003853,  0.0003926, &
        0.0003997,  0.0004067,  0.0004135,  0.0004202,  0.0004268, &
        0.0004333,  0.0004396,  0.0004458,  0.0004519,  0.0004579, &
        0.0004638,  0.0004696,  0.0004753,  0.0004809,  0.0004864, &
        0.0004919,  0.0004972,  0.0005025,  0.0005077,  0.0005129, &
        0.0005179,  0.0005229,  0.0005279,  0.0005327,  0.0005375, &
        0.0005423,  0.0005470,  0.0005516,  0.0005562,  0.0005607, &
        0.0005652,  0.0005696,  0.0005739,  0.0005782,  0.0005825, &
        0.0005867,  0.0005909,  0.0005951,  0.0005991,  0.0006032, &
        0.0006072,  0.0006112/
      data ((coa(i,j),i=1,62),j= 52, 52)/ &
        0.0002132,  0.0002309,  0.0002469,  0.0002617,  0.0002755, &
        0.0002885,  0.0003008,  0.0003125,  0.0003237,  0.0003345, &
        0.0003449,  0.0003549,  0.0003645,  0.0003739,  0.0003830, &
        0.0003918,  0.0004004,  0.0004088,  0.0004170,  0.0004250, &
        0.0004328,  0.0004404,  0.0004478,  0.0004551,  0.0004623, &
        0.0004693,  0.0004762,  0.0004829,  0.0004895,  0.0004960, &
        0.0005024,  0.0005087,  0.0005149,  0.0005210,  0.0005269, &
        0.0005328,  0.0005272,  0.0005443,  0.0005500,  0.0005555, &
        0.0005609,  0.0005663,  0.0005717,  0.0005769,  0.0005821, &
        0.0005872,  0.0005922,  0.0005972,  0.0006021,  0.0006070, &
        0.0006118,  0.0006165,  0.0006212,  0.0006258,  0.0006304, &
        0.0006349,  0.0006394,  0.0006438,  0.0006482,  0.0006525, &
        0.0006568,  0.0006611/
      data ((coa(i,j),i=1,62),j= 53, 53)/ &
        0.0002293,  0.0002485,  0.0002660,  0.0002821,  0.0002972, &
        0.0003114,  0.0003249,  0.0003377,  0.0003500,  0.0003618, &
        0.0003732,  0.0003841,  0.0003947,  0.0004049,  0.0004149, &
        0.0004245,  0.0004339,  0.0004430,  0.0004520,  0.0004606, &
        0.0004691,  0.0004774,  0.0004855,  0.0004934,  0.0005012, &
        0.0005087,  0.0005162,  0.0005235,  0.0005306,  0.0005377, &
        0.0005446,  0.0005513,  0.0005580,  0.0005646,  0.0005710, &
        0.0005773,  0.0005836,  0.0005897,  0.0005957,  0.0006017, &
        0.0006076,  0.0006134,  0.0006191,  0.0006247,  0.0006302, &
        0.0006357,  0.0006411,  0.0006464,  0.0006517,  0.0006569, &
        0.0006620,  0.0006671,  0.0006721,  0.0006771,  0.0006820, &
        0.0006868,  0.0006916,  0.0006963,  0.0007010,  0.0007056, &
        0.0007102,  0.0007147/
      data ((coa(i,j),i=1,62),j= 54, 54)/ &
        0.0002471,  0.0002680,  0.0002871,  0.0003048,  0.0003214, &
        0.0003369,  0.0003517,  0.0003658,  0.0003792,  0.0003921, &
        0.0004045,  0.0004165,  0.0004281,  0.0004392,  0.0004501, &
        0.0004606,  0.0004708,  0.0004807,  0.0004903,  0.0004998, &
        0.0005089,  0.0005179,  0.0005267,  0.0005352,  0.0005436, &
        0.0005518,  0.0005598,  0.0005677,  0.0005754,  0.0005829, &
        0.0005903,  0.0005976,  0.0006048,  0.0006118,  0.0006187, &
        0.0006255,  0.0006322,  0.0006388,  0.0006453,  0.0006516, &
        0.0006579,  0.0006641,  0.0006702,  0.0006762,  0.0006822, &
        0.0006880,  0.0006938,  0.0006995,  0.0007051,  0.0007106, &
        0.0007161,  0.0007215,  0.0007269,  0.0007321,  0.0007374, &
        0.0007425,  0.0007476,  0.0007527,  0.0007576,  0.0007626, &
        0.0007674,  0.0007723/
      data ((coa(i,j),i=1,62),j= 55, 55)/ &
        0.0002669,  0.0002898,  0.0003107,  0.0003300,  0.0003482, &
        0.0003653,  0.0003815,  0.0003969,  0.0004116,  0.0004257, &
        0.0004392,  0.0004522,  0.0004648,  0.0004769,  0.0004887, &
        0.0005001,  0.0005111,  0.0005218,  0.0005323,  0.0005425, &
        0.0005524,  0.0005620,  0.0005714,  0.0005807,  0.0005897, &
        0.0005985,  0.0006071,  0.0006155,  0.0006238,  0.0006319, &
        0.0006398,  0.0006476,  0.0006553,  0.0006628,  0.0006702, &
        0.0006775,  0.0006846,  0.0006917,  0.0006986,  0.0007054, &
        0.0007121,  0.0007187,  0.0007252,  0.0007316,  0.0007379, &
        0.0007442,  0.0007503,  0.0007564,  0.0007624,  0.0007683, &
        0.0007741,  0.0007798,  0.0007855,  0.0007911,  0.0007967, &
        0.0008022,  0.0008076,  0.0008129,  0.0008182,  0.0008235, &
        0.0008286,  0.0008337/
      data ((coa(i,j),i=1,62),j= 56, 56)/ &
        0.0002889,  0.0003140,  0.0003369,  0.0003582,  0.0003780, &
        0.0003967,  0.0004144,  0.0004312,  0.0004473,  0.0004626, &
        0.0004773,  0.0004914,  0.0005050,  0.0005182,  0.0005309, &
        0.0005432,  0.0005551,  0.0005666,  0.0005779,  0.0005888, &
        0.0005995,  0.0006098,  0.0006200,  0.0006298,  0.0006395, &
        0.0006489,  0.0006581,  0.0006672,  0.0006760,  0.0006847, &
        0.0006932,  0.0007015,  0.0007097,  0.0007177,  0.0007256, &
        0.0007333,  0.0007409,  0.0007484,  0.0007558,  0.0007630, &
        0.0007702,  0.0007772,  0.0007841,  0.0007909,  0.0007976, &
        0.0008043,  0.0008108,  0.0008172,  0.0008236,  0.0008298, &
        0.0008360,  0.0008421,  0.0008481,  0.0008541,  0.0008600, &
        0.0008658,  0.0008715,  0.0008772,  0.0008828,  0.0008883, &
        0.0008938,  0.0008992/
      data ((coa(i,j),i=1,62),j= 57, 57)/ &
        0.0003135,  0.0003410,  0.0003662,  0.0003895,  0.0004112, &
        0.0004316,  0.0004509,  0.0004692,  0.0004866,  0.0005032, &
        0.0005191,  0.0005344,  0.0005491,  0.0005632,  0.0005769, &
        0.0005901,  0.0006029,  0.0006153,  0.0006274,  0.0006391, &
        0.0006505,  0.0006616,  0.0006725,  0.0006830,  0.0006933, &
        0.0007034,  0.0007132,  0.0007229,  0.0007323,  0.0007415, &
        0.0007506,  0.0007595,  0.0007682,  0.0007767,  0.0007851, &
        0.0007933,  0.0008014,  0.0008093,  0.0008172,  0.0008249, &
        0.0008324,  0.0008399,  0.0008472,  0.0008544,  0.0008615, &
        0.0008685,  0.0008755,  0.0008823,  0.0008890,  0.0008956, &
        0.0009021,  0.0009086,  0.0009149,  0.0009212,  0.0009274, &
        0.0009335,  0.0009396,  0.0009455,  0.0009514,  0.0009573, &
        0.0009630,  0.0009687/
      data ((coa(i,j),i=1,62),j= 58, 58)/ &
        0.0003409,  0.0003711,  0.0003987,  0.0004241,  0.0004478, &
        0.0004700,  0.0004909,  0.0005107,  0.0005295,  0.0005474, &
        0.0005645,  0.0005810,  0.0005968,  0.0006120,  0.0006267, &
        0.0006408,  0.0006545,  0.0006678,  0.0006807,  0.0006932, &
        0.0007054,  0.0007173,  0.0007288,  0.0007401,  0.0007510, &
        0.0007618,  0.0007722,  0.0007825,  0.0007925,  0.0008023, &
        0.0008119,  0.0008213,  0.0008306,  0.0008396,  0.0008485, &
        0.0008572,  0.0008658,  0.0008742,  0.0008824,  0.0008906, &
        0.0008986,  0.0009064,  0.0009142,  0.0009218,  0.0009293, &
        0.0009367,  0.0009439,  0.0009511,  0.0009582,  0.0009652, &
        0.0009720,  0.0009788,  0.0009855,  0.0009921,  0.0009986, &
        0.0010050,  0.0010113,  0.0010176,  0.0010238,  0.0010299, &
        0.0010359,  0.0010419/
      data ((coa(i,j),i=1,62),j= 59, 59)/ &
        0.0003715,  0.0004046,  0.0004346,  0.0004623,  0.0004880, &
        0.0005120,  0.0005346,  0.0005560,  0.0005762,  0.0005955, &
        0.0006139,  0.0006315,  0.0006485,  0.0006648,  0.0006804, &
        0.0006956,  0.0007102,  0.0007244,  0.0007382,  0.0007515, &
        0.0007645,  0.0007771,  0.0007893,  0.0008012,  0.0008129, &
        0.0008243,  0.0008354,  0.0008463,  0.0008569,  0.0008673, &
        0.0008774,  0.0008874,  0.0008971,  0.0009067,  0.0009160, &
        0.0009252,  0.0009342,  0.0009431,  0.0009518,  0.0009604, &
        0.0009688,  0.0009770,  0.0009851,  0.0009931,  0.0010010, &
        0.0010088,  0.0010164,  0.0010239,  0.0010313,  0.0010386, &
        0.0010458,  0.0010529,  0.0010598,  0.0010667,  0.0010735, &
        0.0010802,  0.0010869,  0.0010934,  0.0010998,  0.0011062, &
        0.0011125,  0.0011187/
      data ((coa(i,j),i=1,62),j= 60, 60)/ &
        0.0004055,  0.0004415,  0.0004742,  0.0005042,  0.0005320, &
        0.0005579,  0.0005822,  0.0006052,  0.0006269,  0.0006476, &
        0.0006673,  0.0006862,  0.0007042,  0.0007216,  0.0007383, &
        0.0007545,  0.0007701,  0.0007851,  0.0007997,  0.0008139, &
        0.0008276,  0.0008410,  0.0008540,  0.0008666,  0.0008789, &
        0.0008910,  0.0009027,  0.0009141,  0.0009253,  0.0009362, &
        0.0009469,  0.0009574,  0.0009676,  0.0009777,  0.0009875, &
        0.0009972,  0.0010066,  0.0010159,  0.0010250,  0.0010339, &
        0.0010427,  0.0010514,  0.0010599,  0.0010682,  0.0010764, &
        0.0010845,  0.0010925,  0.0011003,  0.0011080,  0.0011156, &
        0.0011231,  0.0011304,  0.0011377,  0.0011449,  0.0011519, &
        0.0011589,  0.0011658,  0.0011726,  0.0011793,  0.0011859, &
        0.0011924,  0.0011989/
      data ((coa(i,j),i=1,62),j= 61, 61)/ &
        0.0004429,  0.0004821,  0.0005175,  0.0005499,  0.0005798, &
        0.0006076,  0.0006337,  0.0006583,  0.0006816,  0.0007037, &
        0.0007247,  0.0007448,  0.0007640,  0.0007825,  0.0008003, &
        0.0008174,  0.0008339,  0.0008499,  0.0008653,  0.0008803, &
        0.0008948,  0.0009089,  0.0009226,  0.0009359,  0.0009488, &
        0.0009615,  0.0009738,  0.0009858,  0.0009975,  0.0010089, &
        0.0010201,  0.0010311,  0.0010418,  0.0010523,  0.0010625, &
        0.0010726,  0.0010825,  0.0010921,  0.0011016,  0.0011109, &
        0.0011201,  0.0011291,  0.0011379,  0.0011466,  0.0011551, &
        0.0011635,  0.0011718,  0.0011799,  0.0011879,  0.0011958, &
        0.0012035,  0.0012112,  0.0012187,  0.0012261,  0.0012335, &
        0.0012407,  0.0012478,  0.0012548,  0.0012618,  0.0012686, &
        0.0012754,  0.0012821/
      data ((coa(i,j),i=1,62),j= 62, 62)/ &
        0.0004840,  0.0005264,  0.0005646,  0.0005994,  0.0006316, &
        0.0006614,  0.0006893,  0.0007155,  0.0007403,  0.0007638, &
        0.0007862,  0.0008075,  0.0008279,  0.0008475,  0.0008663, &
        0.0008844,  0.0009018,  0.0009186,  0.0009349,  0.0009506, &
        0.0009658,  0.0009806,  0.0009950,  0.0010089,  0.0010225, &
        0.0010356,  0.0010485,  0.0010610,  0.0010733,  0.0010852, &
        0.0010968,  0.0011082,  0.0011194,  0.0011303,  0.0011409, &
        0.0011514,  0.0011616,  0.0011717,  0.0011815,  0.0011912, &
        0.0012007,  0.0012100,  0.0012191,  0.0012281,  0.0012370, &
        0.0012457,  0.0012542,  0.0012627,  0.0012709,  0.0012791, &
        0.0012871,  0.0012951,  0.0013029,  0.0013106,  0.0013181, &
        0.0013256,  0.0013330,  0.0013403,  0.0013475,  0.0013546, &
        0.0013616,  0.0013685/
      data ((coa(i,j),i=1,62),j= 63, 63)/ &
        0.0005290,  0.0005747,  0.0006157,  0.0006530,  0.0006874, &
        0.0007192,  0.0007490,  0.0007769,  0.0008032,  0.0008281, &
        0.0008518,  0.0008743,  0.0008959,  0.0009165,  0.0009362, &
        0.0009552,  0.0009735,  0.0009911,  0.0010082,  0.0010246, &
        0.0010405,  0.0010559,  0.0010709,  0.0010854,  0.0010995, &
        0.0011132,  0.0011266,  0.0011396,  0.0011523,  0.0011647, &
        0.0011768,  0.0011886,  0.0012002,  0.0012115,  0.0012225, &
        0.0012334,  0.0012440,  0.0012544,  0.0012646,  0.0012746, &
        0.0012844,  0.0012941,  0.0013036,  0.0013129,  0.0013220, &
        0.0013310,  0.0013399,  0.0013486,  0.0013572,  0.0013657, &
        0.0013740,  0.0013823,  0.0013904,  0.0013983,  0.0014062, &
        0.0014140,  0.0014217,  0.0014292,  0.0014367,  0.0014441, &
        0.0014514,  0.0014586/
      data ((coa(i,j),i=1,62),j= 64, 64)/ &
        0.0005778,  0.0006269,  0.0006708,  0.0007107,  0.0007473, &
        0.0007812,  0.0008127,  0.0008423,  0.0008701,  0.0008964, &
        0.0009213,  0.0009450,  0.0009676,  0.0009892,  0.0010099, &
        0.0010297,  0.0010488,  0.0010671,  0.0010848,  0.0011020, &
        0.0011185,  0.0011345,  0.0011500,  0.0011651,  0.0011798, &
        0.0011940,  0.0012078,  0.0012213,  0.0012345,  0.0012473, &
        0.0012599,  0.0012721,  0.0012841,  0.0012958,  0.0013073, &
        0.0013185,  0.0013295,  0.0013403,  0.0013509,  0.0013612, &
        0.0013714,  0.0013815,  0.0013913,  0.0014010,  0.0014105, &
        0.0014199,  0.0014291,  0.0014382,  0.0014471,  0.0014559, &
        0.0014646,  0.0014732,  0.0014816,  0.0014900,  0.0014982, &
        0.0015063,  0.0015143,  0.0015222,  0.0015300,  0.0015377, &
        0.0015453,  0.0015528/
      data ((coa(i,j),i=1,62),j= 65, 65)/ &
        0.0006307,  0.0006832,  0.0007301,  0.0007725,  0.0008114, &
        0.0008472,  0.0008805,  0.0009116,  0.0009409,  0.0009684, &
        0.0009945,  0.0010193,  0.0010428,  0.0010653,  0.0010868, &
        0.0011075,  0.0011273,  0.0011464,  0.0011647,  0.0011825, &
        0.0011996,  0.0012162,  0.0012323,  0.0012480,  0.0012631, &
        0.0012779,  0.0012922,  0.0013062,  0.0013199,  0.0013332, &
        0.0013462,  0.0013589,  0.0013713,  0.0013835,  0.0013954, &
        0.0014071,  0.0014186,  0.0014298,  0.0014408,  0.0014516, &
        0.0014623,  0.0014727,  0.0014830,  0.0014931,  0.0015030, &
        0.0015128,  0.0015225,  0.0015319,  0.0015413,  0.0015505, &
        0.0015596,  0.0015686,  0.0015774,  0.0015862,  0.0015948, &
        0.0016033,  0.0016117,  0.0016200,  0.0016282,  0.0016363, &
        0.0016443,  0.0016522/
      data ((coa(i,j),i=1,62),j= 66, 66)/ &
        0.0006876,  0.0007436,  0.0007934,  0.0008383,  0.0008793, &
        0.0009170,  0.0009520,  0.0009846,  0.0010150,  0.0010439, &
        0.0010710,  0.0010968,  0.0011213,  0.0011446,  0.0011669, &
        0.0011883,  0.0012089,  0.0012287,  0.0012477,  0.0012661, &
        0.0012839,  0.0013011,  0.0013178,  0.0013340,  0.0013498, &
        0.0013651,  0.0013800,  0.0013946,  0.0014088,  0.0014227, &
        0.0014362,  0.0014495,  0.0014624,  0.0014751,  0.0014876, &
        0.0014998,  0.0015117,  0.0015235,  0.0015350,  0.0015464, &
        0.0015575,  0.0015685,  0.0015792,  0.0015899,  0.0016003, &
        0.0016106,  0.0016207,  0.0016307,  0.0016405,  0.0016502, &
        0.0016598,  0.0016693,  0.0016786,  0.0016878,  0.0016969, &
        0.0017059,  0.0017147,  0.0017235,  0.0017322,  0.0017407, &
        0.0017492,  0.0017575/
      data ((coa(i,j),i=1,62),j= 67, 67)/ &
        0.0007485,  0.0008080,  0.0008606,  0.0009079,  0.0009509, &
        0.0009904,  0.0010269,  0.0010608,  0.0010926,  0.0011225, &
        0.0011507,  0.0011774,  0.0012028,  0.0012270,  0.0012501, &
        0.0012723,  0.0012936,  0.0013142,  0.0013339,  0.0013531, &
        0.0013716,  0.0013895,  0.0014069,  0.0014238,  0.0014402, &
        0.0014562,  0.0014718,  0.0014870,  0.0015019,  0.0015164, &
        0.0015306,  0.0015445,  0.0015581,  0.0015714,  0.0015845, &
        0.0015973,  0.0016099,  0.0016223,  0.0016344,  0.0016464, &
        0.0016581,  0.0016697,  0.0016811,  0.0016922,  0.0017033, &
        0.0017141,  0.0017249,  0.0017354,  0.0017458,  0.0017561, &
        0.0017662,  0.0017762,  0.0017861,  0.0017959,  0.0018055, &
        0.0018150,  0.0018244,  0.0018337,  0.0018429,  0.0018520, &
        0.0018610,  0.0018698/
      data ((coa(i,j),i=1,62),j= 68, 68)/ &
        0.0008135,  0.0008762,  0.0009315,  0.0009811,  0.0010259, &
        0.0010670,  0.0011050,  0.0011402,  0.0011732,  0.0012042, &
        0.0012334,  0.0012611,  0.0012874,  0.0013126,  0.0013366, &
        0.0013597,  0.0013819,  0.0014033,  0.0014239,  0.0014439, &
        0.0014632,  0.0014820,  0.0015002,  0.0015179,  0.0015351, &
        0.0015519,  0.0015683,  0.0015843,  0.0016000,  0.0016153, &
        0.0016302,  0.0016449,  0.0016592,  0.0016733,  0.0016871, &
        0.0017007,  0.0017140,  0.0017271,  0.0017400,  0.0017526, &
        0.0017651,  0.0017773,  0.0017894,  0.0018012,  0.0018129, &
        0.0018245,  0.0018358,  0.0018471,  0.0018581,  0.0018690, &
        0.0018798,  0.0018904,  0.0019009,  0.0019113,  0.0019215, &
        0.0019316,  0.0019416,  0.0019515,  0.0019613,  0.0019709, &
        0.0019805,  0.0019899/
      data ((coa(i,j),i=1,62),j= 69, 69)/ &
        0.0008823,  0.0009481,  0.0010059,  0.0010575,  0.0011042, &
        0.0011468,  0.0011862,  0.0012227,  0.0012569,  0.0012891, &
        0.0013195,  0.0013483,  0.0013757,  0.0014020,  0.0014271, &
        0.0014512,  0.0014744,  0.0014968,  0.0015185,  0.0015395, &
        0.0015598,  0.0015795,  0.0015987,  0.0016174,  0.0016356, &
        0.0016533,  0.0016707,  0.0016876,  0.0017041,  0.0017203, &
        0.0017362,  0.0017517,  0.0017669,  0.0017819,  0.0017965, &
        0.0018109,  0.0018251,  0.0018390,  0.0018527,  0.0018661, &
        0.0018793,  0.0018924,  0.0019052,  0.0019178,  0.0019303, &
        0.0019425,  0.0019546,  0.0019665,  0.0019783,  0.0019899, &
        0.0020014,  0.0020127,  0.0020238,  0.0020348,  0.0020457, &
        0.0020565,  0.0020671,  0.0020776,  0.0020880,  0.0020983, &
        0.0021084,  0.0021185/
      data ((coa(i,j),i=1,62),j= 70, 70)/ &
        0.0009546,  0.0010233,  0.0010834,  0.0011370,  0.0011854, &
        0.0012297,  0.0012705,  0.0013085,  0.0013441,  0.0013776, &
        0.0014093,  0.0014395,  0.0014682,  0.0014957,  0.0015221, &
        0.0015475,  0.0015720,  0.0015956,  0.0016185,  0.0016406, &
        0.0016621,  0.0016830,  0.0017033,  0.0017231,  0.0017424, &
        0.0017613,  0.0017797,  0.0017976,  0.0018152,  0.0018324, &
        0.0018493,  0.0018658,  0.0018820,  0.0018979,  0.0019135, &
        0.0019288,  0.0019439,  0.0019587,  0.0019732,  0.0019875, &
        0.0020016,  0.0020155,  0.0020291,  0.0020425,  0.0020558, &
        0.0020688,  0.0020817,  0.0020944,  0.0021069,  0.0021192, &
        0.0021314,  0.0021434,  0.0021095,  0.0021670,  0.0021786, &
        0.0021900,  0.0022013,  0.0022125,  0.0022235,  0.0022344, &
        0.0022452,  0.0022558/
      data ((coa(i,j),i=1,62),j= 71, 71)/ &
        0.0010302,  0.0011016,  0.0011640,  0.0012195,  0.0012698, &
        0.0013159,  0.0013584,  0.0013981,  0.0014354,  0.0014705, &
        0.0015038,  0.0015355,  0.0015658,  0.0015948,  0.0016227, &
        0.0016496,  0.0016755,  0.0017005,  0.0017248,  0.0017483, &
        0.0017712,  0.0017934,  0.0018150,  0.0018360,  0.0018566, &
        0.0018766,  0.0018962,  0.0019153,  0.0019340,  0.0019524, &
        0.0019703,  0.0019879,  0.0020052,  0.0020221,  0.0020387, &
        0.0020550,  0.0020710,  0.0020867,  0.0021022,  0.0021174, &
        0.0021324,  0.0021471,  0.0021159,  0.0021759,  0.0021900, &
        0.0022039,  0.0022175,  0.0022310,  0.0022443,  0.0022574, &
        0.0022703,  0.0022830,  0.0022956,  0.0023080,  0.0023203, &
        0.0023324,  0.0023443,  0.0023562,  0.0023678,  0.0023794, &
        0.0023908,  0.0024020/
      data ((coa(i,j),i=1,62),j= 72, 72)/ &
        0.0011087,  0.0011828,  0.0012476,  0.0013054,  0.0013579, &
        0.0014060,  0.0014506,  0.0014923,  0.0015315,  0.0015685, &
        0.0016038,  0.0016373,  0.0016694,  0.0017002,  0.0017298, &
        0.0017583,  0.0017859,  0.0018125,  0.0018384,  0.0018634, &
        0.0018877,  0.0019114,  0.0019344,  0.0019568,  0.0019787, &
        0.0020000,  0.0020209,  0.0020412,  0.0020612,  0.0020807, &
        0.0020998,  0.0021185,  0.0021368,  0.0021090,  0.0021725, &
        0.0021898,  0.0022068,  0.0022235,  0.0022399,  0.0022561, &
        0.0022720,  0.0022876,  0.0023029,  0.0023181,  0.0023330, &
        0.0023477,  0.0023621,  0.0023764,  0.0023904,  0.0024042, &
        0.0024179,  0.0024314,  0.0024446,  0.0024577,  0.0024707, &
        0.0024834,  0.0024960,  0.0025085,  0.0025208,  0.0025329, &
        0.0025449,  0.0025568/
      data ((coa(i,j),i=1,62),j= 73, 73)/ &
        0.0011902,  0.0012672,  0.0013347,  0.0013952,  0.0014502, &
        0.0015008,  0.0015478,  0.0015919,  0.0016334,  0.0016727, &
        0.0017101,  0.0017457,  0.0017799,  0.0018126,  0.0018442, &
        0.0018746,  0.0019039,  0.0019323,  0.0019598,  0.0019865, &
        0.0020124,  0.0020376,  0.0020621,  0.0020859,  0.0021092, &
        0.0021319,  0.0021083,  0.0021757,  0.0021969,  0.0022176, &
        0.0022379,  0.0022577,  0.0022772,  0.0022962,  0.0023149, &
        0.0023333,  0.0023513,  0.0023690,  0.0023863,  0.0024034, &
        0.0024202,  0.0024366,  0.0024529,  0.0024688,  0.0024845, &
        0.0025000,  0.0025152,  0.0025302,  0.0025450,  0.0025595, &
        0.0025739,  0.0025880,  0.0026019,  0.0026157,  0.0026293, &
        0.0026426,  0.0026557,  0.0026689,  0.0026817,  0.0026944, &
        0.0027070,  0.0027194/
      data ((coa(i,j),i=1,62),j= 74, 74)/ &
        0.0012749,  0.0013552,  0.0014259,  0.0014895,  0.0015475, &
        0.0016010,  0.0016509,  0.0016977,  0.0017418,  0.0017836, &
        0.0018234,  0.0018614,  0.0018978,  0.0019328,  0.0019664, &
        0.0019987,  0.0020300,  0.0020602,  0.0020895,  0.0021179, &
        0.0021454,  0.0021722,  0.0021982,  0.0022235,  0.0022482, &
        0.0022723,  0.0022958,  0.0023187,  0.0023411,  0.0023630, &
        0.0023844,  0.0024054,  0.0024259,  0.0024460,  0.0024658, &
        0.0024851,  0.0025040,  0.0025226,  0.0025409,  0.0025588, &
        0.0025764,  0.0025937,  0.0026107,  0.0026275,  0.0026439, &
        0.0026601,  0.0026760,  0.0026917,  0.0027071,  0.0027223, &
        0.0027373,  0.0027520,  0.0027665,  0.0027808,  0.0027950, &
        0.0028089,  0.0028226,  0.0028361,  0.0028495,  0.0028627, &
        0.0028757,  0.0028885/
      data ((coa(i,j),i=1,62),j= 75, 75)/ &
        0.0013631,  0.0014474,  0.0015220,  0.0015892,  0.0016507, &
        0.0017076,  0.0017607,  0.0018105,  0.0018575,  0.0019021, &
        0.0019445,  0.0019850,  0.0020238,  0.0020610,  0.0020967, &
        0.0021312,  0.0021186,  0.0021965,  0.0022276,  0.0022577, &
        0.0022868,  0.0023152,  0.0023427,  0.0023695,  0.0023956, &
        0.0024210,  0.0024457,  0.0024699,  0.0024935,  0.0025165, &
        0.0025390,  0.0025610,  0.0025826,  0.0026037,  0.0026243, &
        0.0026445,  0.0026643,  0.0026838,  0.0027028,  0.0027215, &
        0.0027399,  0.0027579,  0.0027756,  0.0027930,  0.0028101, &
        0.0028269,  0.0028434,  0.0028596,  0.0028756,  0.0028913, &
        0.0029068,  0.0029220,  0.0029370,  0.0029518,  0.0029664, &
        0.0029807,  0.0029949,  0.0030088,  0.0030225,  0.0030361, &
        0.0030494,  0.0030626/
      data ((coa(i,j),i=1,62),j= 76, 76)/ &
        0.0014557,  0.0015446,  0.0016236,  0.0016950,  0.0017605, &
        0.0018211,  0.0018777,  0.0019308,  0.0019809,  0.0020284, &
        0.0020736,  0.0021167,  0.0021121,  0.0021973,  0.0022353, &
        0.0022718,  0.0023069,  0.0023409,  0.0023737,  0.0024055, &
        0.0024362,  0.0024661,  0.0024950,  0.0025232,  0.0025506, &
        0.0025772,  0.0026031,  0.0026284,  0.0026531,  0.0026771, &
        0.0027006,  0.0027235,  0.0027460,  0.0027679,  0.0027893, &
        0.0028103,  0.0028309,  0.0028510,  0.0028707,  0.0028900, &
        0.0029090,  0.0029276,  0.0029458,  0.0029637,  0.0029813, &
        0.0029986,  0.0030156,  0.0030322,  0.0030486,  0.0030647, &
        0.0030806,  0.0030962,  0.0031115,  0.0031266,  0.0031414, &
        0.0031560,  0.0031704,  0.0031846,  0.0031986,  0.0032123, &
        0.0032259,  0.0032392/
      data ((coa(i,j),i=1,62),j= 77, 77)/ &
        0.0015532,  0.0016476,  0.0017317,  0.0018078,  0.0018775, &
        0.0019422,  0.0020024,  0.0020590,  0.0021123,  0.0021169, &
        0.0022106,  0.0022563,  0.0022999,  0.0023416,  0.0023817, &
        0.0024202,  0.0024572,  0.0024929,  0.0025273,  0.0025607, &
        0.0025929,  0.0026241,  0.0026543,  0.0026837,  0.0027122, &
        0.0027399,  0.0027668,  0.0027931,  0.0028186,  0.0028435, &
        0.0028678,  0.0028915,  0.0029146,  0.0029372,  0.0029592, &
        0.0029808,  0.0030019,  0.0030225,  0.0030427,  0.0030625, &
        0.0030819,  0.0031009,  0.0031195,  0.0031377,  0.0031556, &
        0.0031732,  0.0031904,  0.0032073,  0.0032239,  0.0032402, &
        0.0032563,  0.0032720,  0.0032875,  0.0033027,  0.0033177, &
        0.0033324,  0.0033468,  0.0033611,  0.0033751,  0.0033889, &
        0.0034025,  0.0034159/
      data ((coa(i,j),i=1,62),j= 78, 78)/ &
        0.0016566,  0.0017571,  0.0018467,  0.0019278,  0.0020021, &
        0.0020708,  0.0021349,  0.0021949,  0.0022514,  0.0023047, &
        0.0023554,  0.0024035,  0.0024494,  0.0024932,  0.0025352, &
        0.0025755,  0.0026142,  0.0026515,  0.0026874,  0.0027220, &
        0.0027555,  0.0027878,  0.0028191,  0.0028495,  0.0028789, &
        0.0029075,  0.0029352,  0.0029621,  0.0029883,  0.0030138, &
        0.0030387,  0.0030629,  0.0030864,  0.0031094,  0.0031319, &
        0.0031538,  0.0031752,  0.0031961,  0.0032166,  0.0032365, &
        0.0032561,  0.0032752,  0.0032940,  0.0033123,  0.0033303, &
        0.0033479,  0.0033652,  0.0033821,  0.0033988,  0.0034151, &
        0.0034310,  0.0034467,  0.0034622,  0.0034773,  0.0034922, &
        0.0035068,  0.0035211,  0.0035353,  0.0035491,  0.0035628, &
        0.0035762,  0.0035894/
      data ((coa(i,j),i=1,62),j= 79, 79)/ &
        0.0017664,  0.0018736,  0.0019689,  0.0020552,  0.0021342, &
        0.0022071,  0.0022749,  0.0023383,  0.0023978,  0.0024539, &
        0.0025070,  0.0025574,  0.0026054,  0.0026511,  0.0026948, &
        0.0027366,  0.0027767,  0.0028153,  0.0028523,  0.0028880, &
        0.0029224,  0.0029556,  0.0029877,  0.0030187,  0.0030487, &
        0.0030778,  0.0031060,  0.0031334,  0.0031599,  0.0031857, &
        0.0032108,  0.0032352,  0.0032590,  0.0032821,  0.0033047, &
        0.0033267,  0.0033481,  0.0033690,  0.0033894,  0.0034094, &
        0.0034289,  0.0034479,  0.0034665,  0.0034847,  0.0035025, &
        0.0035200,  0.0035371,  0.0035538,  0.0035702,  0.0035862, &
        0.0036020,  0.0036174,  0.0036325,  0.0036474,  0.0036620, &
        0.0036763,  0.0036903,  0.0037041,  0.0037177,  0.0037310, &
        0.0037441,  0.0037569/
      data ((coa(i,j),i=1,62),j= 80, 80)/ &
        0.0018832,  0.0019973,  0.0020987,  0.0021902,  0.0022737, &
        0.0023505,  0.0024220,  0.0024885,  0.0025508,  0.0026093, &
        0.0026646,  0.0027169,  0.0027666,  0.0028138,  0.0028589, &
        0.0029018,  0.0029430,  0.0029824,  0.0030202,  0.0030565, &
        0.0030915,  0.0031252,  0.0031576,  0.0031890,  0.0032192, &
        0.0032485,  0.0032768,  0.0033042,  0.0033308,  0.0033566, &
        0.0033816,  0.0034059,  0.0034295,  0.0034525,  0.0034748, &
        0.0034966,  0.0035177,  0.0035384,  0.0035585,  0.0035781, &
        0.0035972,  0.0036159,  0.0036341,  0.0036520,  0.0036694, &
        0.0036864,  0.0037031,  0.0037193,  0.0037353,  0.0037509, &
        0.0037662,  0.0037811,  0.0037958,  0.0038102,  0.0038243, &
        0.0038381,  0.0038517,  0.0038650,  0.0038780,  0.0038908, &
        0.0039034,  0.0039158/
      data ((coa(i,j),i=1,62),j= 81, 81)/ &
        0.0020072,  0.0021282,  0.0022356,  0.0023321,  0.0024200, &
        0.0025006,  0.0025751,  0.0026443,  0.0027089,  0.0027695, &
        0.0028265,  0.0028803,  0.0029311,  0.0029794,  0.0030252, &
        0.0030689,  0.0031106,  0.0031504,  0.0031886,  0.0032251, &
        0.0032602,  0.0032939,  0.0033263,  0.0033575,  0.0033876, &
        0.0034166,  0.0034447,  0.0034718,  0.0034980,  0.0035234, &
        0.0035480,  0.0035719,  0.0035950,  0.0036175,  0.0036393, &
        0.0036605,  0.0036811,  0.0037012,  0.0037207,  0.0037398, &
        0.0037583,  0.0037764,  0.0037940,  0.0038112,  0.0038280, &
        0.0038444,  0.0038604,  0.0038761,  0.0038914,  0.0039063, &
        0.0039210,  0.0039353,  0.0039494,  0.0039631,  0.0039766, &
        0.0039898,  0.0040027,  0.0040154,  0.0040278,  0.0040400, &
        0.0040520,  0.0040638/
      data ((coa(i,j),i=1,62),j= 82, 82)/ &
        0.0021381,  0.0022661,  0.0023791,  0.0024803,  0.0025719, &
        0.0026557,  0.0027328,  0.0028041,  0.0028705,  0.0029325, &
        0.0029905,  0.0030451,  0.0030966,  0.0031453,  0.0031914, &
        0.0032353,  0.0032769,  0.0033166,  0.0033545,  0.0033908, &
        0.0034255,  0.0034588,  0.0034907,  0.0035214,  0.0035509, &
        0.0035793,  0.0036067,  0.0036331,  0.0036586,  0.0036833, &
        0.0037071,  0.0037302,  0.0037526,  0.0037743,  0.0037953, &
        0.0038157,  0.0038356,  0.0038548,  0.0038736,  0.0038916, &
        0.0039095,  0.0039268,  0.0039436,  0.0039600,  0.0039761, &
        0.0039917,  0.0040069,  0.0040218,  0.0040364,  0.0040506, &
        0.0040645,  0.0040781,  0.0040914,  0.0041044,  0.0041172, &
        0.0041296,  0.0041419,  0.0041539,  0.0041656,  0.0041772, &
        0.0041885,  0.0041996/
      data ((coa(i,j),i=1,62),j= 83, 83)/ &
        0.0022756,  0.0024100,  0.0025280,  0.0026332,  0.0027280, &
        0.0028142,  0.0028931,  0.0029658,  0.0030331,  0.0030957, &
        0.0031541,  0.0032089,  0.0032603,  0.0033088,  0.0033545, &
        0.0033978,  0.0034389,  0.0034780,  0.0035151,  0.0035506, &
        0.0035844,  0.0036168,  0.0036478,  0.0036775,  0.0037061, &
        0.0037335,  0.0037599,  0.0037853,  0.0038098,  0.0038335, &
        0.0038563,  0.0038784,  0.0038998,  0.0039205,  0.0039405, &
        0.0039599,  0.0039788,  0.0039971,  0.0040149,  0.0040322, &
        0.0040490,  0.0040653,  0.0040813,  0.0040968,  0.0041119, &
        0.0041267,  0.0041411,  0.0041552,  0.0041689,  0.0041823, &
        0.0041954,  0.0042082,  0.0042208,  0.0042331,  0.0042451, &
        0.0042569,  0.0042684,  0.0042797,  0.0042908,  0.0043017, &
        0.0043123,  0.0043228/
      data ((coa(i,j),i=1,62),j= 84, 84)/ &
        0.0024185,  0.0025586,  0.0026808,  0.0027890,  0.0028859, &
        0.0029735,  0.0030533,  0.0031264,  0.0031938,  0.0032562, &
        0.0033142,  0.0033683,  0.0034190,  0.0034665,  0.0035113, &
        0.0035535,  0.0035934,  0.0036313,  0.0036672,  0.0037014, &
        0.0037340,  0.0037651,  0.0037948,  0.0038232,  0.0038505, &
        0.0038767,  0.0039018,  0.0039260,  0.0039493,  0.0039717, &
        0.0039934,  0.0040143,  0.0040345,  0.0040540,  0.0040730, &
        0.0040913,  0.0041091,  0.0041264,  0.0041432,  0.0041595, &
        0.0041753,  0.0041907,  0.0042057,  0.0042204,  0.0042346, &
        0.0042485,  0.0042621,  0.0042754,  0.0042883,  0.0043009, &
        0.0043133,  0.0043254,  0.0043372,  0.0043488,  0.0043601, &
        0.0043712,  0.0043821,  0.0043928,  0.0044032,  0.0044135, &
        0.0044236,  0.0044335/
      data ((coa(i,j),i=1,62),j= 85, 85)/ &
        0.0025653,  0.0027099,  0.0028350,  0.0029450,  0.0030428, &
        0.0031307,  0.0032102,  0.0032828,  0.0033493,  0.0034106, &
        0.0034673,  0.0035200,  0.0035692,  0.0036152,  0.0036583, &
        0.0036990,  0.0037373,  0.0037735,  0.0038078,  0.0038404, &
        0.0038714,  0.0039009,  0.0039291,  0.0039560,  0.0039818, &
        0.0040065,  0.0040303,  0.0040531,  0.0040751,  0.0040962, &
        0.0041166,  0.0041363,  0.0041553,  0.0041738,  0.0041916, &
        0.0042089,  0.0042256,  0.0042419,  0.0042577,  0.0042730, &
        0.0042879,  0.0043025,  0.0043166,  0.0043304,  0.0043439, &
        0.0043570,  0.0043698,  0.0043823,  0.0043945,  0.0044064, &
        0.0044181,  0.0044296,  0.0044407,  0.0044517,  0.0044624, &
        0.0044730,  0.0044833,  0.0044934,  0.0045033,  0.0045130, &
        0.0045226,  0.0045320/
      data ((coa(i,j),i=1,62),j= 86, 86)/ &
        0.0027124,  0.0028597,  0.0029861,  0.0030963,  0.0031936, &
        0.0032805,  0.0033586,  0.0034295,  0.0034941,  0.0035534, &
        0.0036081,  0.0036587,  0.0037058,  0.0037497,  0.0037908, &
        0.0038294,  0.0038657,  0.0039000,  0.0039324,  0.0039631, &
        0.0039923,  0.0040201,  0.0040467,  0.0040720,  0.0040963, &
        0.0041195,  0.0041418,  0.0041633,  0.0041839,  0.0042038, &
        0.0042230,  0.0042415,  0.0042594,  0.0042768,  0.0042935, &
        0.0043098,  0.0043256,  0.0043409,  0.0043558,  0.0043703, &
        0.0043844,  0.0043982,  0.0044115,  0.0044246,  0.0044373, &
        0.0044497,  0.0044619,  0.0044737,  0.0044853,  0.0044966, &
        0.0045077,  0.0045186,  0.0045292,  0.0045397,  0.0045499, &
        0.0045599,  0.0045697,  0.0045793,  0.0045888,  0.0045981, &
        0.0046072,  0.0046162/
      data ((coa(i,j),i=1,62),j= 87, 87)/ &
        0.0028481,  0.0029956,  0.0031209,  0.0032292,  0.0033241, &
        0.0034083,  0.0034836,  0.0035515,  0.0036132,  0.0036696, &
        0.0037214,  0.0037692,  0.0038136,  0.0038548,  0.0038934, &
        0.0039296,  0.0039636,  0.0039956,  0.0040260,  0.0040547, &
        0.0040820,  0.0041080,  0.0041328,  0.0041565,  0.0041792, &
        0.0042010,  0.0042219,  0.0042419,  0.0042613,  0.0042800, &
        0.0042980,  0.0043154,  0.0043322,  0.0043485,  0.0043643, &
        0.0043796,  0.0043945,  0.0044090,  0.0044231,  0.0044367, &
        0.0044501,  0.0044631,  0.0044757,  0.0044881,  0.0045001, &
        0.0045119,  0.0045234,  0.0045347,  0.0045457,  0.0045564, &
        0.0045670,  0.0045773,  0.0045874,  0.0045973,  0.0046070, &
        0.0046166,  0.0046259,  0.0046351,  0.0046441,  0.0046530, &
        0.0046616,  0.0046702/
      data ((coa(i,j),i=1,62),j= 88, 88)/ &
        0.0029341,  0.0030768,  0.0031968,  0.0032997,  0.0033892, &
        0.0034681,  0.0035383,  0.0036014,  0.0036584,  0.0037104, &
        0.0037581,  0.0038020,  0.0038427,  0.0038805,  0.0039159, &
        0.0039490,  0.0039801,  0.0040095,  0.0040373,  0.0040637, &
        0.0040888,  0.0041127,  0.0041355,  0.0041573,  0.0041782, &
        0.0041983,  0.0042175,  0.0042361,  0.0042540,  0.0042713, &
        0.0042880,  0.0043041,  0.0043197,  0.0043349,  0.0043495, &
        0.0043638,  0.0043777,  0.0043911,  0.0044042,  0.0044170, &
        0.0044294,  0.0044416,  0.0044534,  0.0044649,  0.0044762, &
        0.0044872,  0.0044980,  0.0045085,  0.0045188,  0.0045289, &
        0.0045387,  0.0045484,  0.0045579,  0.0045672,  0.0045763, &
        0.0045852,  0.0045940,  0.0046026,  0.0046110,  0.0046193, &
        0.0046275,  0.0046355/
      data ((coa(i,j),i=1,62),j= 89, 89)/ &
        0.0029122,  0.0030427,  0.0031513,  0.0032438,  0.0033237, &
        0.0033938,  0.0034559,  0.0035116,  0.0035619,  0.0036076, &
        0.0036495,  0.0036882,  0.0037238,  0.0037573,  0.0037884, &
        0.0038176,  0.0038451,  0.0038711,  0.0038957,  0.0039191, &
        0.0039413,  0.0039626,  0.0039829,  0.0040023,  0.0040209, &
        0.0040389,  0.0040561,  0.0040727,  0.0040887,  0.0041042, &
        0.0041192,  0.0041337,  0.0041477,  0.0041613,  0.0041745, &
        0.0041874,  0.0041998,  0.0042120,  0.0042238,  0.0042353, &
        0.0042465,  0.0042574,  0.0042681,  0.0042785,  0.0042887, &
        0.0042986,  0.0043084,  0.0043179,  0.0043272,  0.0043363, &
        0.0043452,  0.0043539,  0.0043625,  0.0043709,  0.0043791, &
        0.0043872,  0.0043951,  0.0044029,  0.0044105,  0.0044180, &
        0.0044254,  0.0044326/
      data ((coa(i,j),i=1,62),j= 90, 90)/ &
        0.0027405,  0.0028512,  0.0029426,  0.0030199,  0.0030864, &
        0.0031447,  0.0031962,  0.0032424,  0.0032841,  0.0033221, &
        0.0033569,  0.0033891,  0.0034190,  0.0034468,  0.0034729, &
        0.0034974,  0.0035206,  0.0035424,  0.0035632,  0.0035830, &
        0.0036018,  0.0036198,  0.0036370,  0.0036535,  0.0036694, &
        0.0036847,  0.0036993,  0.0037135,  0.0037272,  0.0037404, &
        0.0037532,  0.0037656,  0.0037776,  0.0037892,  0.0038005, &
        0.0038115,  0.0038222,  0.0038326,  0.0038427,  0.0038526, &
        0.0038622,  0.0038716,  0.0038808,  0.0038897,  0.0038984, &
        0.0039070,  0.0039153,  0.0039235,  0.0039314,  0.0039393, &
        0.0039469,  0.0039544,  0.0039618,  0.0039690,  0.0039761, &
        0.0039830,  0.0039898,  0.0039965,  0.0040030,  0.0040095, &
        0.0040158,  0.0040220/
      data ((coa(i,j),i=1,62),j= 91, 91)/ &
        0.0024633,  0.0025514,  0.0026239,  0.0026851,  0.0027377, &
        0.0027838,  0.0028247,  0.0028613,  0.0028946,  0.0029249, &
        0.0029529,  0.0029787,  0.0030028,  0.0030253,  0.0030464, &
        0.0030663,  0.0030851,  0.0031029,  0.0031199,  0.0031360, &
        0.0031514,  0.0031661,  0.0031803,  0.0031938,  0.0032068, &
        0.0032194,  0.0032314,  0.0032431,  0.0032544,  0.0032652, &
        0.0032758,  0.0032860,  0.0032959,  0.0033055,  0.0033148, &
        0.0033239,  0.0033327,  0.0033413,  0.0033497,  0.0033578, &
        0.0033658,  0.0033735,  0.0033811,  0.0033885,  0.0033957, &
        0.0034028,  0.0034097,  0.0034164,  0.0034230,  0.0034295, &
        0.0034359,  0.0034421,  0.0034482,  0.0034541,  0.0034600, &
        0.0034657,  0.0034714,  0.0034769,  0.0034823,  0.0034877, &
        0.0034929,  0.0034981/
      data ((coa(i,j),i=1,62),j= 92, 92)/ &
        0.0021142,  0.0022278,  0.0022837,  0.0023309,  0.0023717, &
        0.0024075,  0.0024394,  0.0024681,  0.0024943,  0.0025182, &
        0.0025404,  0.0025609,  0.0025801,  0.0025980,  0.0026149, &
        0.0026308,  0.0026459,  0.0026602,  0.0026738,  0.0026868, &
        0.0026992,  0.0027111,  0.0027225,  0.0027334,  0.0027439, &
        0.0027541,  0.0027638,  0.0027733,  0.0027824,  0.0027912, &
        0.0027997,  0.0028080,  0.0028160,  0.0028238,  0.0028314, &
        0.0028387,  0.0028459,  0.0028529,  0.0028597,  0.0028663, &
        0.0028727,  0.0028791,  0.0028852,  0.0028912,  0.0028971, &
        0.0029028,  0.0029084,  0.0029139,  0.0029193,  0.0029246, &
        0.0029297,  0.0029348,  0.0029398,  0.0029446,  0.0029494, &
        0.0029541,  0.0029587,  0.0029632,  0.0029676,  0.0029720, &
        0.0029761,  0.0029805/
      data ((coa(i,j),i=1,62),j= 93, 93)/ &
        0.0018726,  0.0019238,  0.0019660,  0.0020019,  0.0020331, &
        0.0020606,  0.0020852,  0.0021074,  0.0021278,  0.0021464, &
        0.0021179,  0.0021798,  0.0021948,  0.0022089,  0.0022221, &
        0.0022347,  0.0022466,  0.0022578,  0.0022686,  0.0022789, &
        0.0022887,  0.0022981,  0.0023071,  0.0023158,  0.0023241, &
        0.0023321,  0.0023399,  0.0023474,  0.0023546,  0.0023616, &
        0.0023684,  0.0023750,  0.0023814,  0.0023876,  0.0023936, &
        0.0023995,  0.0024052,  0.0024108,  0.0024162,  0.0024215, &
        0.0024266,  0.0024317,  0.0024366,  0.0024414,  0.0024461, &
        0.0024507,  0.0024552,  0.0024596,  0.0024639,  0.0024681, &
        0.0024722,  0.0024763,  0.0024802,  0.0024841,  0.0024880, &
        0.0024917,  0.0024954,  0.0024990,  0.0025026,  0.0025060, &
        0.0025095,  0.0025129/
      data ((coa(i,j),i=1,62),j= 94, 94)/ &
        0.0016337,  0.0016718,  0.0017033,  0.0017303,  0.0017537, &
        0.0017745,  0.0017931,  0.0018100,  0.0018254,  0.0018397, &
        0.0018529,  0.0018651,  0.0018766,  0.0018874,  0.0018976, &
        0.0019073,  0.0019164,  0.0019251,  0.0019334,  0.0019413, &
        0.0019489,  0.0019562,  0.0019631,  0.0019698,  0.0019763, &
        0.0019825,  0.0019885,  0.0019944,  0.0020000,  0.0020054, &
        0.0020107,  0.0020158,  0.0020208,  0.0020256,  0.0020303, &
        0.0020349,  0.0020394,  0.0020437,  0.0020479,  0.0020521, &
        0.0020561,  0.0020600,  0.0020639,  0.0020676,  0.0020713, &
        0.0020749,  0.0020784,  0.0020819,  0.0020852,  0.0020886, &
        0.0020918,  0.0020950,  0.0020981,  0.0021011,  0.0021041, &
        0.0021071,  0.0021100,  0.0021128,  0.0021156,  0.0021183, &
        0.0021210,  0.0021236/
      data ((coa(i,j),i=1,62),j= 95, 95)/ &
        0.0014740,  0.0015024,  0.0015259,  0.0015460,  0.0015636, &
        0.0015791,  0.0015931,  0.0016058,  0.0016174,  0.0016282, &
        0.0016381,  0.0016474,  0.0016561,  0.0016643,  0.0016720, &
        0.0016793,  0.0016863,  0.0016929,  0.0016992,  0.0017052, &
        0.0017110,  0.0017165,  0.0017219,  0.0017270,  0.0017319, &
        0.0017367,  0.0017413,  0.0017458,  0.0017501,  0.0017543, &
        0.0017584,  0.0017623,  0.0017661,  0.0017699,  0.0017735, &
        0.0017770,  0.0017804,  0.0017838,  0.0017870,  0.0017902, &
        0.0017933,  0.0017964,  0.0017993,  0.0018023,  0.0018051, &
        0.0018079,  0.0018106,  0.0018132,  0.0018158,  0.0018184, &
        0.0018209,  0.0018233,  0.0018258,  0.0018281,  0.0018304, &
        0.0018327,  0.0018349,  0.0018371,  0.0018393,  0.0018414, &
        0.0018434,  0.0018455/
      data ((coa(i,j),i=1,62),j= 96, 96)/ &
        0.0013895,  0.0014110,  0.0014289,  0.0014441,  0.0014574, &
        0.0014692,  0.0014798,  0.0014894,  0.0014982,  0.0015064, &
        0.0015139,  0.0015210,  0.0015277,  0.0015338,  0.0015398, &
        0.0015454,  0.0015508,  0.0015558,  0.0015607,  0.0015653, &
        0.0015698,  0.0015740,  0.0015782,  0.0015821,  0.0015859, &
        0.0015896,  0.0015932,  0.0015966,  0.0016000,  0.0016032, &
        0.0016064,  0.0016094,  0.0016124,  0.0016153,  0.0016181, &
        0.0016208,  0.0016235,  0.0016261,  0.0016286,  0.0016311, &
        0.0016335,  0.0016358,  0.0016381,  0.0016404,  0.0016426, &
        0.0016447,  0.0016468,  0.0016489,  0.0016509,  0.0016529, &
        0.0016548,  0.0016567,  0.0016586,  0.0016604,  0.0016622, &
        0.0016639,  0.0016657,  0.0016673,  0.0016690,  0.0016706, &
        0.0016722,  0.0016738/
      data ((coa(i,j),i=1,62),j= 97, 97)/ &
        0.0013502,  0.0013669,  0.0013807,  0.0013924,  0.0014027, &
        0.0014118,  0.0014200,  0.0014274,  0.0014343,  0.0014406, &
        0.0014465,  0.0014520,  0.0014571,  0.0014620,  0.0014666, &
        0.0014710,  0.0014751,  0.0014791,  0.0014829,  0.0014865, &
        0.0014900,  0.0014933,  0.0014966,  0.0014997,  0.0015027, &
        0.0015055,  0.0015083,  0.0015109,  0.0015136,  0.0015162, &
        0.0015186,  0.0015210,  0.0015234,  0.0015256,  0.0015278, &
        0.0015299,  0.0015320,  0.0015340,  0.0015360,  0.0015380, &
        0.0015398,  0.0015417,  0.0015435,  0.0015452,  0.0015469, &
        0.0015486,  0.0015503,  0.0015519,  0.0015534,  0.0015550, &
        0.0015565,  0.0015580,  0.0015594,  0.0015608,  0.0015622, &
        0.0015636,  0.0015649,  0.0015663,  0.0015676,  0.0015688, &
        0.0015701,  0.0015713/
      data ((coa(i,j),i=1,62),j= 98, 98)/ &
        0.0013341,  0.0013476,  0.0013588,  0.0013683,  0.0013766, &
        0.0013840,  0.0013907,  0.0013967,  0.0014023,  0.0014074, &
        0.0014122,  0.0014167,  0.0014209,  0.0014248,  0.0014286, &
        0.0014321,  0.0014355,  0.0014387,  0.0014418,  0.0014447, &
        0.0014476,  0.0014503,  0.0014529,  0.0014554,  0.0014578, &
        0.0014602,  0.0014624,  0.0014646,  0.0014667,  0.0014688, &
        0.0014708,  0.0014727,  0.0014746,  0.0014764,  0.0014782, &
        0.0014799,  0.0014816,  0.0014833,  0.0014849,  0.0014864, &
        0.0014879,  0.0014894,  0.0014909,  0.0014923,  0.0014937, &
        0.0014950,  0.0014964,  0.0014977,  0.0014989,  0.0015002, &
        0.0015014,  0.0015026,  0.0015038,  0.0015049,  0.0015060, &
        0.0015071,  0.0015082,  0.0015093,  0.0015103,  0.0015114, &
        0.0015124,  0.0015134/
      data ((coa(i,j),i=1,62),j= 99, 99)/ &
        0.0013255,  0.0013373,  0.0013470,  0.0013554,  0.0013626, &
        0.0013691,  0.0013749,  0.0013803,  0.0013851,  0.0013896, &
        0.0013938,  0.0013977,  0.0014014,  0.0014049,  0.0014082, &
        0.0014113,  0.0014142,  0.0014171,  0.0014197,  0.0014223, &
        0.0014248,  0.0014272,  0.0014294,  0.0014316,  0.0014337, &
        0.0014358,  0.0014378,  0.0014397,  0.0014415,  0.0014433, &
        0.0014450,  0.0014467,  0.0014483,  0.0014499,  0.0014515, &
        0.0014530,  0.0014544,  0.0014559,  0.0014573,  0.0014586, &
        0.0014599,  0.0014612,  0.0014625,  0.0014637,  0.0014649, &
        0.0014661,  0.0014672,  0.0014684,  0.0014695,  0.0014705, &
        0.0014716,  0.0014726,  0.0014736,  0.0014746,  0.0014756, &
        0.0014766,  0.0014775,  0.0014784,  0.0014793,  0.0014802, &
        0.0014811,  0.0014820/
      data ((coa(i,j),i=1,62),j=100,100)/ &
        0.0013126,  0.0013234,  0.0013324,  0.0013401,  0.0013469, &
        0.0013529,  0.0013583,  0.0013632,  0.0013677,  0.0013719, &
        0.0013758,  0.0013795,  0.0013829,  0.0013861,  0.0013891, &
        0.0013920,  0.0013947,  0.0013974,  0.0013998,  0.0014022, &
        0.0014045,  0.0014067,  0.0014088,  0.0014108,  0.0014127, &
        0.0014146,  0.0014164,  0.0014181,  0.0014198,  0.0014215, &
        0.0014230,  0.0014246,  0.0014261,  0.0014275,  0.0014289, &
        0.0014303,  0.0014316,  0.0014329,  0.0014341,  0.0014354, &
        0.0014366,  0.0014377,  0.0014389,  0.0014400,  0.0014411, &
        0.0014421,  0.0014432,  0.0014442,  0.0014452,  0.0014462, &
        0.0014471,  0.0014480,  0.0014490,  0.0014499,  0.0014507, &
        0.0014516,  0.0014525,  0.0014533,  0.0014541,  0.0014549, &
        0.0014557,  0.0014565/
      data ((coa(i,j),i=1,62),j=101,101)/ &
        0.0012882,  0.0012983,  0.0013066,  0.0013138,  0.0013202, &
        0.0013258,  0.0013309,  0.0013355,  0.0013398,  0.0013437, &
        0.0013473,  0.0013507,  0.0013539,  0.0013569,  0.0013598, &
        0.0013625,  0.0013650,  0.0013674,  0.0013697,  0.0013719, &
        0.0013740,  0.0013760,  0.0013780,  0.0013798,  0.0013816, &
        0.0013833,  0.0013850,  0.0013865,  0.0013881,  0.0013896, &
        0.0013910,  0.0013924,  0.0013938,  0.0013951,  0.0013963, &
        0.0013976,  0.0013988,  0.0013999,  0.0014011,  0.0014022, &
        0.0014033,  0.0014043,  0.0014054,  0.0014064,  0.0014073, &
        0.0014083,  0.0014092,  0.0014101,  0.0014110,  0.0014119, &
        0.0014128,  0.0014136,  0.0014145,  0.0014153,  0.0014161, &
        0.0014168,  0.0014176,  0.0014183,  0.0014191,  0.0014198, &
        0.0014205,  0.0014212/

!-----
      do i=1,m
         swh(i,1)=0.
         so2(i,1)=0.
!-----snt is the secant of the solar zenith angle
!
         snt(i)=1.0  /cosz(i)
      enddo


      do k=1,np
       do i=1,m
!
!-----compute layer thickness. indices for the surface level and
!     surface layer are np+1 and np, respectively.
          dp(i,k)=pl(i,k+1)-pl(i,k)
!
!-----compute scaled water vapor amount following eqs. (3.3) and (3.5)
!     unit is g/cm**2
!
          scal(i,k)=dp(i,k)*(.5*(pl(i,k)+pl(i,k+1))/300.)**.8
          wh(i,k)=1.02*wa(i,k)*scal(i,k) &
                    *(1.+0.00135*(ta(i,k)-240.)) +1.e-11
          swh(i,k+1)=swh(i,k)+wh(i,k)
!-----compute ozone amount, unit is (cm-atm)stp
!     the number 466.7 is the unit conversion factor
!     from g/cm**2 to (cm-atm)stp
          oh(i,k)=1.02*oa(i,k)*dp(i,k)*466.7 +1.e-11
!-----compute layer cloud water amount (gm/m**2)
!     the index is 1 for ice crystals, 2 for liquid drops, and
!     3 for rain drops
          x=1.02*10000.*dp(i,k)
          cwp(i,k,1)=x*cwc(i,k,1)
          cwp(i,k,2)=x*cwc(i,k,2)
          cwp(i,k,3)=x*cwc(i,k,3)
       enddo
      enddo


!-----initialize fluxes for all-sky (flx), clear-sky (flc), and
!     flux reduction (df)
      do k=1,np+1
       do i=1,m
          flx(i,k)=0.
          flc(i,k)=0.
          flxu(i,k)=0.
          flxd(i,k)=0.
          df(i,k)=0.
          df_sub(i,k)=0.
          df_cld(i,k)=0.
          df_clr(i,k)=0.
       enddo
      enddo


!-----compute solar uv and par fluxes
!ccshie 9/18/04
      call sw_uvpar (m,np,wh,oh,dp, &
                  cwp,taucld,reff,ict,icb,fcld,cosz, &
                  taual,ssaal,asyal,taux,rsuvbm,rsuvdf, &
                  flx,flc,flxd,fdiruv,fdifuv,fdirpar,fdifpar)
!-----compute and update solar ir fluxes
      call sw_ir (m,np,wh,dp, &
                  cwp,taucld,reff,ict,icb,fcld,cosz, &
                  taual,ssaal,asyal,rsirbm,rsirdf, &
                  flx,flc,flxd,fdirir,fdifir)

!-----compute pressure-scaled o2 amount following eq. (3.5) with
!     f=1. unit is (cm-atm)stp.
!     the constant 165.22 equals (1000/980)*23.14%*(22400/32)
       do i= 1, m
        cnt(i)=165.22*snt(i)
       enddo
      do k=1,np
       do i=1,m
          so2(i,k+1)=so2(i,k)+scal(i,k)*cnt(i)
       enddo
      enddo
!-----compute flux reduction due to o2 following eq. (3.18)
!     the constant 0.0633 is the fraction of insolation contained
!     in the oxygen bands 
       do k= 2, np+1
        do i= 1, m
          x=so2(i,k)
          df(i,k)=0.0633*(1.-exp(-0.000145*sqrt(x)))

        enddo
       enddo

!-----for solar heating due to co2
       do i= 1, m
        cnt(i)=co2*snt(i)
       enddo
!-----scale co2 amounts following eq. (3.5) with f=1.
!     unit is (cm-atm)stp.
!     the constant 789 equals (1000/980)*(44/28.97)*(22400/44)
      do k=1,np
       do i=1,m
         x=789.*cnt(i)
         so2(i,k+1)=so2(i,k)+x*scal(i,k)+1.e-11
       enddo
      enddo
!-----for co2 absorption in band 10 where absorption due to
!     water vapor and co2 are both moderate
        u1=-3.0
        du=0.15
        w1=-4.0
        dw=0.15
!-----so2 and swh are the co2 and water vapor amounts integrated
!     from the top of the atmosphere
      do k= 2, np+1
       do i= 1, m
        swu(i,k)=log10(so2(i,k))
        swh(i,k)=log10(swh(i,k)*snt(i))
       enddo
      enddo
!-----df is the updated flux reduction given by the second term on the
!     right-hand-side of eq. (3.24) divided by so
        call reduce_flux(m,np,swu,u1,du,nu,swh,w1,dw,nw,cah,df)
!-----for co2 absorption in band 11 where the co2 absorption has
!     a large impact on the heating of middle atmosphere.
        u1=0.000250
        du=0.000050
        w1=-2.0
        dw=0.05
        do i= 1, m
         swu(i,1)=co2*snt(i)
        enddo
!-----co2 mixing ratio is independent of space (spatially homogeneous)
        do k= 2, np+1
         do i= 1, m
          swu(i,k)=swu(i,1)
         enddo
        enddo
!-----swh is the logarithm of pressure
       do k= 2, np+1
        do i= 1, m
          swh(i,k)=log10(pl(i,k))
        enddo
       enddo
!-----df is the updated flux reduction derived from the table given by
!     eq. (3.19)
        call reduce_flux(m,np,swu,u1,du,nx2,swh,w1,dw,ny2,coa,df)

! compute layer sub df (also filter negative values)
   do i=1,m 
      do k = 2, np+1
         df_sub(i,k) = max(df(i,k) - df(i,k-1), 0._fp_kind)  !df for each layer (remove negative df_sub) 
      enddo
   enddo

! compute clear-sky df
   do i=1,m
      do k = 2, np+1
         df_clr(i,k) = df_clr(i,k-1)+df_sub(i,k)  
      enddo
   enddo

!-----adjustment for the effect of o2 cnd co2 on clear-sky fluxes.
!     both flc and df_clr are positive quantities
       do k=1,np+1
         do i=1,m
          flc(i,k)=max(flc(i,k)-df_clr(i,k),0._fp_kind)  !this filter is for small cosine zenith angle.
         enddo
       enddo

!-----identify top cloud-layer

       do i= 1, m
         nctop(i)=np+1
       enddo

       do k=1,np
        do i= 1, m
         if (fcld(i,k).gt.fcld_min .and. nctop(i).eq.np+1) then
          nctop(i)=k
         endif
        enddo
       enddo

! adjust df_sub for below cloud
       do i= 1,m
         ntop=nctop(i) 
         if(overcast .and. fast_overcast) then !compute cloud albedo
!           cld_alb = sum(taux(i,ntop+1:np+1))/(6.7+sum(taux(i,ntop+1:np+1)))
           cld_alb = sum(taux(i,ntop+1:np))/(6.7+sum(taux(i,ntop+1:np)))
         endif
        if (ntop.lt.np+1) then
         do k= ntop+1,np+1 !cloud top -> surface
          if(overcast .and. fast_overcast) then  !use ratio in LUT
            i_cos = int(cosz(i)*10.)+1 !1~10
            i_tau = int(cld_alb*10.)+1 !1~10
            ratio = ratio_lut(i_tau,i_cos)
          else !use computed clear and cloudy flux ratio (not fast_overcast)
            ratio = max(0.01_fp_kind, min(1._fp_kind,(flx(i,k)/flc(i,k))))
          endif
          df_sub(i,k)  = df_sub(i,k)*ratio  !compute cloudy-sky df_sub
         enddo !k
        endif
       enddo !i

!update df for cloudy-sky
   do i=1,m
      do k = 2, np+1
         df_cld(i,k) = df_cld(i,k-1)+df_sub(i,k) 
      enddo
   enddo

!-----adjustment for the effect of o2 cnd co2 on all-sky fluxes.
!      max statement filter negative value in flx for small cosz (for df_cld > flx)
   do i=1,m
      do k = 1, np+1
         flx(i,k)  = max(flx(i,k)-df_cld(i,k) , 0._fp_kind)  !this max is for small cosz
         flxd(i,k) = max(flxd(i,k)-df_cld(i,k), 0._fp_kind)  !this max is for small cosz
         flxu(i,k) = flx(i,k)-flxd(i,k)
      enddo 
    enddo

!-----adjustment for the direct downward flux
       do i= 1,m
        fdirir(i)=fdirir(i)-df_cld(i,np+1)
        if (fdirir(i) .lt. 0.0) fdirir(i)=0.0
       enddo

      return

      end subroutine swrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine sw_uvpar (m,np,wh,oh,dp, &
                cwp,taucld,reff,ict,icb,fcld,cosz, &
                taual,ssaal,asyal,taux,rsuvbm,rsuvdf, &
                flx,flc,flxd,fdiruv,fdifuv,fdirpar,fdifpar)

!******************************************************************
!  compute solar fluxes in the uv+par region. the spectrum is
!  grouped into 8 bands:
!
!              band     micrometer
!
!       uv-c    1.     .175 - .225
!               2.     .225 - .245
!                      .260 - .280
!               3.     .245 - .260
!
!       uv-b    4.     .280 - .295
!               5.     .295 - .310
!               6.     .310 - .320
!
!       uv-a    7.     .320 - .400
!
!       par     8.     .400 - .700
!
!----- input parameters:                            units      size
!
!  number of soundings (m)                          n/d         1
!  number of atmospheric layers (np)                n/d         1
!  layer scaled-water vapor content (wh)          gm/cm^2      m*np
!  layer ozone content (oh)                      (cm-atm)stp   m*np
!  layer pressure thickness (dp)                    mb         m*np
!  option for scaling cloud optical thickness       n/d         1
!        overcast="true" if scaling is not required
!        overcast="fasle" if scaling is required
!  input option for cloud optical thickness         n/d         1
!        cldwater="true" if taucld is provided
!        cldwater="false" if cwp is provided
!  cloud water amount (cwp)                        gm/m**2     m*np*3
!        index 1 for ice particles
!        index 2 for liquid drops
!        index 3 for rain drops
!  cloud optical thickness (taucld)                 n/d        m*np*3
!       index 1 for ice particles
!       index 2 for liquid drops
!       index 3 for rain drops
!  effective cloud-particle size (reff)          micrometer    m*np*3
!       index 1 for ice paticles
!       index 2 for liquid drops
!       index 3 for rain drops
!  level index separating high and                  n/d        m
!       middle clouds (ict)
!  level indiex separating middle and               n/d        m
!       low clouds (icb)
!  cloud amount (fcld)                            fraction     m*np
!  cosine of solar zenith angle (cosz)              n/d        m
!  aerosol optical thickness (taual)                n/d        m*np*11
!  aerosol single-scattering albedo (ssaal)         n/d        m*np*11
!  aerosol asymmetry factor (asyal)                 n/d        m*np*11
!  uv+par surface albedo for beam                 fraction     m
!       radiation (rsuvbm)
!  uv+par surface albedo for diffuse              fraction     m
!       radiation (rsuvdf)
!
!---- temporary array
!
!  scaled cloud optical thickness                   n/d        m*np
!       for beam radiation (tauclb)
!  scaled cloud optical thickness                   n/d        m*np
!       for diffuse radiation  (tauclf)
!
!----- output (updated) parameters:
!
!  all-sky flux divergence (downward-upward) (flx)               fraction      m*(np+1)
!  clear-sky flux divergence (downward-upward) (flc)             fraction      m*(np+1)
!  all-sky direct downward uv flux at
!       the surface (fdiruv)                     fraction      m
!  all-sky diffuse downward uv flux at
!       the surface (fdifuv)                     fraction      m
!  all-sky direct downward par flux at
!       the surface (fdirpar)                    fraction      m
!  all-sky diffuse downward par flux at
!       the surface (fdifpar)                    fraction      m
!
!***********************************************************************
!ccshie 8/19/04
     implicit none
!-----input parameters
      integer m,np,ict(m),icb(m)
      real(Kind=fp_kind) taucld(m,np,3),reff(m,np,3),fcld(m,np)
      real(Kind=fp_kind) cwp(m,np,3),wh(m,np),oh(m,np),dp(m,np)
      real(Kind=fp_kind) taual(m,np,ib_sw),ssaal(m,np,ib_sw),asyal(m,np,ib_sw)
      real(Kind=fp_kind) rsuvbm(m),rsuvdf(m),cosz(m)
!-----output (updated) parameter
      real(Kind=fp_kind) flx(m,np+1),flc(m,np+1)
      real(Kind=fp_kind) flxd(m,np+1)
      real(Kind=fp_kind) fdiruv (m),fdifuv (m)
      real(Kind=fp_kind) fdirpar(m),fdifpar(m)
      real(Kind=fp_kind) taux(m,np)
!-----static parameters
      integer nband
      parameter (nband=8)
      real(Kind=fp_kind) hk(nband),wk(nband),zk(nband),ry(nband)
      real(Kind=fp_kind) aig(3),awg(3),arg(3)
      real(Kind=fp_kind) aib(2),awb(2),arb(2)
!-----temporary array
      integer i,k,ib
      integer ih1,ih2,im1,im2,is1,is2
      real(Kind=fp_kind) taurs,tauoz,tauwv
      real(Kind=fp_kind) :: g1,g2,g3
      real(Kind=fp_kind) :: dsm(m)
      real(Kind=fp_kind) :: tauclb(m,np)
      real(Kind=fp_kind) :: tauclf(m,np)
      real(Kind=fp_kind) :: asycl(m,np)
      real(Kind=fp_kind) :: tausto(m,np)
      real(Kind=fp_kind) :: ssatau(m,np)
      real(Kind=fp_kind) :: asysto(m,np)
      real(Kind=fp_kind) :: tautob(m,np)
      real(Kind=fp_kind) :: ssatob(m,np)
      real(Kind=fp_kind) :: asytob(m,np)
      real(Kind=fp_kind) :: tautof(m,np)
      real(Kind=fp_kind) :: ssatof(m,np)
      real(Kind=fp_kind) :: asytof(m,np)
      real(Kind=fp_kind) :: rr(m,np+1,2)
      real(Kind=fp_kind) :: tt(m,np+1,2)
      real(Kind=fp_kind) :: td(m,np+1,2)
      real(Kind=fp_kind) :: rs(m,np+1,2)
      real(Kind=fp_kind) :: ts(m,np+1,2)
      real(Kind=fp_kind) :: fall(m,np+1)
      real(Kind=fp_kind) :: falld(m,np+1)
      real(Kind=fp_kind) :: fclr(m,np+1)
      real(Kind=fp_kind) :: fsdir(m)
      real(Kind=fp_kind) :: fsdif(m)
      real(Kind=fp_kind) :: asyclt(m)
      real(Kind=fp_kind) :: cc(m,3)
      real(Kind=fp_kind) :: rrt(m,np)
      real(Kind=fp_kind) :: ttt(m,np)
      real(Kind=fp_kind) :: tdt(m,np)
      real(Kind=fp_kind) :: rst(m,np)
      real(Kind=fp_kind) :: tst(m,np)
      real(Kind=fp_kind) :: dum1(m,np+1)
      real(Kind=fp_kind) :: dum2(m)
      real(Kind=fp_kind) :: dum3(m)
      real(Kind=fp_kind) :: dum(m,np)
!-----hk is the fractional extra-terrestrial solar flux in each
!     of the 8 bands. the sum of hk is 0.47074. (table 3)
      data hk/.00057, .00367, .00083, .00417, &
              .00600, .00556, .05913, .39081/
!-----zk is the ozone absorption coefficient. unit: /(cm-atm)stp
!     (table 3)
      data zk /30.47, 187.2,  301.9,   42.83, &
               7.09,  1.25,   0.0345,  0.0572/
!-----wk is the water vapor absorption coefficient. unit: cm**2/g
!     (table 3)
      data wk /7*0.0, 0.00075/
!-----ry is the extinction coefficient for rayleigh scattering.
!     unit: /mb. (table 3)
      data ry /.00604, .00170, .00222, .00132, &
               .00107, .00091, .00055, .00012/
!-----coefficients for computing the extinction coefficients of ice,
!     water, and rain particles, independent of spectral band. (table 4)
      data aib/ 3.33e-4,2.52/
      data awb/-6.59e-3,1.65/
      data arb/ 3.07e-3,0.00/
!-----coefficients for computing the asymmetry factor of ice, water,
!     and rain particles, independent of spectral band. (table 6)
      data aig/.74625,.0010541,-.00000264/
      data awg/.82562,.0052900,-.00014866/
      data arg/.883,0.0,0.0/
!-----initialize fdiruv, fdifuv, surface reflectances and transmittances.
!     the reflectance and transmittance of the clear and cloudy portions
!     of a layer are denoted by 1 and 2, respectively.
!     cc is the maximum cloud cover in each of the high, middle, and low
!     cloud groups.
!     1/dsm=1/cos(53) = 1.66
      do i=1,m
         dsm(i)=0.602
         fdiruv(i)=0.0
         fdifuv(i)=0.0
         rr(i,np+1,1)=rsuvbm(i)
         rr(i,np+1,2)=rsuvbm(i)
         rs(i,np+1,1)=rsuvdf(i)
         rs(i,np+1,2)=rsuvdf(i)
         td(i,np+1,1)=0.0
         td(i,np+1,2)=0.0
         tt(i,np+1,1)=0.0
         tt(i,np+1,2)=0.0
         ts(i,np+1,1)=0.0
         ts(i,np+1,2)=0.0
         cc(i,1)=0.0
         cc(i,2)=0.0
         cc(i,3)=0.0
      enddo

      if (cldwater) then
       do k=1,np
        do i=1,m
          taucld(i,k,1)=cwp(i,k,1)*(aib(1)+aib(2)/reff(i,k,1))
          taucld(i,k,2)=cwp(i,k,2)*(awb(1)+awb(2)/reff(i,k,2))
          taucld(i,k,3)=cwp(i,k,3)* arb(1)
        enddo
       enddo
      endif
!-----options for scaling cloud optical thickness

      if (overcast) then

       do k=1,np
        do i=1,m
          tauclb(i,k)=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          tauclf(i,k)=tauclb(i,k)
        enddo
       enddo

       do k=1,3
        do i=1,m
           cc(i,k)=1.0
        enddo
       enddo

      else

!-----scale cloud optical thickness in each layer from taucld (with
!     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
!     tauclb is the scaled optical thickness for beam radiation and
!     tauclf is for diffuse radiation (see section 7).

         call cloud_scale (m,np,cosz,fcld,taucld,ict,icb, &
                       cc,tauclb,tauclf)

      endif

!-----cloud asymmetry factor for a mixture of liquid and ice particles.
!     unit of reff is micrometers. eqs. (4.8) and (6.4)
      do k=1,np
       do i=1,m
           asyclt(i)=1.0
           taux(i,k)=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          if (taux(i,k).gt.taux_min .and. fcld(i,k).gt.fcld_min) then
           g1=(aig(1)+(aig(2)+aig(3)*reff(i,k,1))*reff(i,k,1))*taucld(i,k,1)
           g2=(awg(1)+(awg(2)+awg(3)*reff(i,k,2))*reff(i,k,2))*taucld(i,k,2)
           g3= arg(1)*taucld(i,k,3)
           asyclt(i)=(g1+g2+g3)/taux(i,k)
         endif
       enddo
        do i=1,m
          asycl(i,k)=asyclt(i)
        enddo
      enddo


!-----integration over spectral bands---------------------------------

  do 100 ib=1,nband


!-----compute reflectance and transmittance of the clear portion of a layer

       do k=1,np
        do i=1,m

!-----compute rayleigh, ozone and water vapor optical thicknesses

          taurs=ry(ib)*dp(i,k)
          tauoz=zk(ib)*oh(i,k)
          tauwv=wk(ib)*wh(i,k)

!-----compute clear-sky optical thickness, single scattering albedo,
!     and asymmetry factor (eqs. 6.2-6.4)

          tausto(i,k)=max(taurs+tauoz+tauwv+taual(i,k,ib),opt_min)
          ssatau(i,k)=max(ssaal(i,k,ib)*taual(i,k,ib)+taurs,opt_min) 
          asysto(i,k)=max(asyal(i,k,ib)*ssaal(i,k,ib)*taual(i,k,ib),opt_min)

       if (overcast .and. fast_overcast ) then ; else

          tautob(i,k)=tausto(i,k)
          ssatob(i,k)=max(ssatau(i,k)/tautob(i,k),opt_min)
          ssatob(i,k)=min(ssatob(i,k),0.999999_fp_kind)
          asytob(i,k)=max(asysto(i,k)/(ssatob(i,k)*tautob(i,k)),opt_min)

!-----Compute delta-eddington approximation of scattering properties
!     for direct incident radiation

         call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), cosz(i) , &
                       rrt(i,k),ttt(i,k), tdt(i,k) )

!     
!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs. (6.5) and (6.6)

         call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), dsm(i) , &
                       rst(i,k),tst(i,k), dum(i,k) )

           rr(i,k,1)=rrt(i,k)
           tt(i,k,1)=ttt(i,k)
           td(i,k,1)=tdt(i,k)
           rs(i,k,1)=rst(i,k)
           ts(i,k,1)=tst(i,k)

         endif ! not overcast .and. not fast_overcast

!-----compute reflectance and transmittance of the cloudy portion of a layer
        if ( (tauclb(i,k).ge.taux_min .or. fcld(i,k).ge.fcld_min ) .or. &  !cloud exists
             (overcast .and. fast_overcast ) ) then                        ! overcast .and. fast_overcast


!-----for direct incident radiation
!     the effective layer optical properties. eqs. (6.2)-(6.4)
           tautob(i,k)=tausto(i,k) + max(tauclb(i,k),opt_min)
           ssatob(i,k)=max((ssatau(i,k)+tauclb(i,k))/tautob(i,k),opt_min)
           ssatob(i,k)=min(ssatob(i,k),0.999999_fp_kind)
           asytob(i,k)=max((asysto(i,k)+asycl(i,k)*tauclb(i,k)) &
                      /(ssatob(i,k)*tautob(i,k)),opt_min)

!-----for diffuse incident radiation
           tautof(i,k)=tausto(i,k)+max(tauclf(i,k),opt_min)

           ssatof(i,k)=max((ssatau(i,k)+tauclf(i,k))/tautof(i,k),opt_min)
           ssatof(i,k)=min(ssatof(i,k),0.999999_fp_kind)
           asytof(i,k)=max((asysto(i,k)+asycl(i,k)*tauclf(i,k)) &
                      /(ssatof(i,k)*tautof(i,k)),opt_min)

!-----Compute delta-eddington approximation of scattering properties
!     for direct incident radiation
!     note that the cloud optical thickness is scaled differently for direct
!     and diffuse insolation, eqs. (7.3) and (7.4).

         call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), cosz(i) , &
                       rrt(i,k),ttt(i,k), tdt(i,k) )

!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs. (6.5) and (6.6)

         call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), dsm(i) , &
                       rst(i,k),tst(i,k), dum(i,k) )

        endif 

           rr(i,k,2)=rrt(i,k)
           tt(i,k,2)=ttt(i,k)
           td(i,k,2)=tdt(i,k)
           rs(i,k,2)=rst(i,k)
           ts(i,k,2)=tst(i,k)



        enddo !i loop
       enddo !k loop


!-----flux calculations

      if (overcast) then   !cloud fraction 0 or 1 -> CRM 

       if( .not. fast_overcast ) then
!-----for clear-sky fluxes only (This is needed for equation 6.18 flux redtion due to CO2 and O2 below cloud)
         ih1=1 ; im1=1 ; is1=1
         ih2=1 ; im2=1 ; is2=1 
         call twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,dum1,falld,dum2,dum3) 
       endif

!-----for cloudy-sky fluxes only
         ih1=2 ; im1=2 ; is1=2
         ih2=2 ; im2=2 ; is2=2     
         call twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,dum1,fall,falld,fsdir,fsdif) 

      else  ! non overcast (cloud fraction 0.~1. GCM or RCM ) 

!-----for clear- and all-sky fluxes the all-sky flux, fall is the summation inside the brackets of eq. (7.11)
         ih1=1 ; im1=1 ; is1=1
         ih2=2 ; im2=2 ; is2=2   
         call twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif) 

      endif  ! overcast  


!-----flux integration, eq. (6.1)

       do k=1,np+1
        do i=1,m
          flx(i,k)=flx(i,k)+fall(i,k)*hk(ib)
          flxd(i,k)=flxd(i,k)+falld(i,k)*hk(ib)

          if(overcast .and. fast_overcast) then ; else 
          flc(i,k)=flc(i,k)+fclr(i,k)*hk(ib)
          endif

        enddo


       enddo

!-----compute direct and diffuse downward surface fluxes in the uv
!     and par regions
       if(ib.lt.8) then
        do i=1,m
          fdiruv(i)=fdiruv(i)+fsdir(i)*hk(ib)
          fdifuv(i)=fdifuv(i)+fsdif(i)*hk(ib)
        enddo
       else
        do i=1,m
          fdirpar(i)=fsdir(i)*hk(ib)
          fdifpar(i)=fsdif(i)*hk(ib)
        enddo
       endif

 100  continue

     return
      end subroutine sw_uvpar

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine sw_ir (m,np,wh,dp, &
                        cwp,taucld,reff,ict,icb,fcld,cosz, &
                        taual,ssaal,asyal, &
                        rsirbm,rsirdf,flx,flc,flxd,fdirir,fdifir)
!************************************************************************
!  compute solar flux in the infrared region. the spectrum is divided
!   into three bands:
!
!          band   wavenumber(/cm)  wavelength (micron)
!          1( 9)    14280-8200         0.70-1.22
!          2(10)     8200-4400         1.22-2.27
!          3(11)     4400-1000         2.27-10.0
!
!----- input parameters:                            units      size
!
!  number of soundings (m)                          n/d         1
!  number of atmospheric layers (np)                n/d         1
!  layer scaled-water vapor content (wh)          gm/cm^2      m*np
!  option for scaling cloud optical thickness       n/d         1
!        overcast="true" if scaling is not required
!        overcast="fasle" if scaling is required
!  input option for cloud optical thickness         n/d         1
!        cldwater="true" if taucld is provided
!        cldwater="false" if cwp is provided
!  cloud water concentration (cwp)                gm/m**2      m*np*3
!        index 1 for ice particles
!        index 2 for liquid drops
!        index 3 for rain drops
!  cloud optical thickness (taucld)                 n/d        m*np*3
!        index 1 for ice paticles
!        index 2 for liquid drops
!        index 3 for rain drops
!  effective cloud-particle size (reff)           micrometer   m*np*3
!        index 1 for ice paticles
!        index 2 for liquid drops
!        index 3 for rain drops
!  level index separating high and                  n/d        m
!        middle clouds (ict)
!  level index separating middle and                n/d        m
!        low clouds (icb)
!  cloud amount (fcld)                            fraction     m*np
!  aerosol optical thickness (taual)                n/d        m*np*11
!  aerosol single-scattering albedo (ssaal)         n/d        m*np*11
!  aerosol asymmetry factor (asyal)                 n/d        m*np*11
!  near ir surface albedo for beam                fraction     m
!        radiation (rsirbm)
!  near ir surface albedo for diffuse             fraction     m
!        radiation (rsirdf)
!
!---- temporary array
!
!  scaled cloud optical thickness                   n/d        m*np
!          for beam radiation (tauclb)
!  scaled cloud optical thickness                   n/d        m*np
!          for diffuse radiation  (tauclf)
!
!----- output (updated) parameters:
!
!  all-sky flux divergence (downward-upward) (flx)           fraction     m*(np+1)
!  clear-sky flux divergence (downward-upward) (flc)         fraction     m*(np+1)
!  all-sky direct downward ir flux at
!          the surface (fdirir)                   fraction     m
!  all-sky diffuse downward ir flux at
!          the surface (fdifir)                   fraction     m
!
!**********************************************************************
     implicit none
!-----input parameters
      integer m,np,ict(m),icb(m)
      integer ih1,ih2,im1,im2,is1,is2
      real(Kind=fp_kind) cwp(m,np,3),taucld(m,np,3),reff(m,np,3)
      real(Kind=fp_kind) fcld(m,np),cosz(m)
      real(Kind=fp_kind) rsirbm(m),rsirdf(m)
      real(Kind=fp_kind) taual(m,np,ib_sw),ssaal(m,np,ib_sw),asyal(m,np,ib_sw)
      real(Kind=fp_kind) dp(m,np),wh(m,np)
!-----output (updated) parameters
      real(Kind=fp_kind) flx(m,np+1),flc(m,np+1)
      real(Kind=fp_kind) flxd(m,np+1)
      real(Kind=fp_kind) fdirir(m),fdifir(m)
!-----static parameters
      integer nk,nband
      parameter (nk=10,nband=3)
      real(Kind=fp_kind) :: taux
      real(Kind=fp_kind) :: w1, w2, w3 !expansion stuff
      real(Kind=fp_kind) :: g1, g2, g3 !asymetry stuff
      real(Kind=fp_kind) hk(nband,nk),xk(nk),ry(nband)
      real(Kind=fp_kind) aib(nband,2),awb(nband,2),arb(nband,2)
      real(Kind=fp_kind) aia(nband,3),awa(nband,3),ara(nband,3)
      real(Kind=fp_kind) aig(nband,3),awg(nband,3),arg(nband,3)
!-----temporary array
      integer ib,iv,ik,i,k
      real(Kind=fp_kind) taurs,tauwv
      real(Kind=fp_kind) :: dsm(m)
      real(Kind=fp_kind) :: tauclb(m,np)
      real(Kind=fp_kind) :: tauclf(m,np)
      real(Kind=fp_kind) :: cc(m,3)
      real(Kind=fp_kind) :: ssacl(m,np)
      real(Kind=fp_kind) :: asycl(m,np)
      real(Kind=fp_kind) :: rr(m,np+1,2)
      real(Kind=fp_kind) :: tt(m,np+1,2)
      real(Kind=fp_kind) :: td(m,np+1,2)
      real(Kind=fp_kind) :: rs(m,np+1,2)
      real(Kind=fp_kind) :: ts(m,np+1,2)
      real(Kind=fp_kind) :: fall(m,np+1)
      real(Kind=fp_kind) :: falld(m,np+1)
      real(Kind=fp_kind) :: fclr(m,np+1)
      real(Kind=fp_kind) :: fsdir(m)
      real(Kind=fp_kind) :: fsdif(m)
      real(Kind=fp_kind) :: tausto(m,np)
      real(Kind=fp_kind) :: ssatau(m,np)
      real(Kind=fp_kind) :: asysto(m,np)
      real(Kind=fp_kind) :: tautob(m,np)
      real(Kind=fp_kind) :: ssatob(m,np)
      real(Kind=fp_kind) :: asytob(m,np)
      real(Kind=fp_kind) :: tautof(m,np)
      real(Kind=fp_kind) :: ssatof(m,np)
      real(Kind=fp_kind) :: asytof(m,np)
      real(Kind=fp_kind) :: ssaclt(m)
      real(Kind=fp_kind) :: asyclt(m)
      real(Kind=fp_kind) :: rrt(m,np)
      real(Kind=fp_kind) :: ttt(m,np)
      real(Kind=fp_kind) :: tdt(m,np)
      real(Kind=fp_kind) :: rst(m,np)
      real(Kind=fp_kind) :: tst(m,np)
      real(Kind=fp_kind) :: dum1(m,np+1)
      real(Kind=fp_kind) :: dum2(m)
      real(Kind=fp_kind) :: dum3(m)
      real(Kind=fp_kind) :: dum(m,np)
!-----water vapor absorption coefficient for 10 k-intervals.
!     unit: cm^2/gm (table 2)
      data xk/ &
        0.0010, 0.0133, 0.0422, 0.1334, 0.4217, &
        1.334,  5.623,  31.62,  177.8,  1000.0/
!-----water vapor k-distribution function,
!     the sum of hk is 0.52926. unit: fraction (table 2)
!      data hk/ &
!       .20673,.08236,.01074,  .03497,.01157,.00360, &
!       .03011,.01133,.00411,  .02260,.01143,.00421, &
!       .01336,.01240,.00389,  .00696,.01258,.00326, &
!       .00441,.01381,.00499,  .00115,.00650,.00465, &
!       .00026,.00244,.00245,  .00000,.00094,.00145/
!added by ucsd, 2016/03, to account for water vapor continuum absorption based on data from Table 1 in Tarasova and Fomin (2000)
      data hk/ &
       .19310,.06924,.00310,  .05716,.01960,.00637, &
       .02088,.00795,.00526,  .02407,.01716,.00641, &
       .01402,.01118,.00542,  .00582,.01377,.00312, &
       .00246,.02008,.00368,  .00163,.00265,.00346, &
       .00101,.00282,.00555,  .00041,.00092,.00098/

!-----ry is the extinction coefficient for rayleigh scattering.
!     unit: /mb (table 3)
      data ry /.0000156, .0000018, .000000/
!-----coefficients for computing the extinction coefficients of
!     ice, water, and rain particles (table 4)
      data aib/ &
        .000333, .000333, .000333, &
           2.52,    2.52,    2.52/
      data awb/ &
        -0.0101, -0.0166, -0.0339, &
           1.72,    1.85,    2.16/
      data arb/ &
         0.00307, 0.00307, 0.00307, &
         0.0    , 0.0    , 0.0    /
!-----coefficients for computing the single-scattering co-albedo of
!     ice, water, and rain particles (table 5)
      data aia/ &
       -.00000260, .00215346, .08938331, &
        .00000746, .00073709, .00299387, &
        .00000000,-.00000134,-.00001038/
      data awa/ &
        .00000007,-.00019934, .01209318, &
        .00000845, .00088757, .01784739, &
       -.00000004,-.00000650,-.00036910/
      data ara/ &
        .029,      .342,      .466, &
        .0000,     .000,      .000, &
        .0000,     .000,      .000/
!-----coefficients for computing the asymmetry factor of
!     ice, water, and rain particles (table 6)
      data aig/ &
        .74935228, .76098937, .84090400, &
        .00119715, .00141864, .00126222, &
       -.00000367,-.00000396,-.00000385/
      data awg/ &
        .79375035, .74513197, .83530748, &
        .00832441, .01370071, .00257181, &
       -.00023263,-.00038203, .00005519/
      data arg/ &
        .891,      .948,      .971, &
        .0000,     .000,      .000, &
        .0000,     .000,      .000/
!-----initialize surface fluxes, reflectances, and transmittances.
!     the reflectance and transmittance of the clear and cloudy portions
!     of a layer are denoted by 1 and 2, respectively.
!     cc is the maximum cloud cover in each of the high, middle, and low
!     cloud groups.
!     1/dsm=1/cos(53)=1.66
      do i=1,m
         dsm(i)=0.602
         fdirir(i)=0.0
         fdifir(i)=0.0
         rr(i,np+1,1)=rsirbm(i)
         rr(i,np+1,2)=rsirbm(i)
         rs(i,np+1,1)=rsirdf(i)
         rs(i,np+1,2)=rsirdf(i)
         td(i,np+1,1)=0.0
         td(i,np+1,2)=0.0
         tt(i,np+1,1)=0.0
         tt(i,np+1,2)=0.0
         ts(i,np+1,1)=0.0
         ts(i,np+1,2)=0.0
         cc(i,1)=0.0
         cc(i,2)=0.0
         cc(i,3)=0.0
      enddo
!-----integration over spectral bands
      do 100 ib=1,nband
       iv=ib+8
!-----compute cloud optical thickness. eqs. (4.6) and (4.11)
      if (cldwater) then
       do k=1,np
        do i=1,m
          taucld(i,k,1)=cwp(i,k,1)*(aib(ib,1) &
                          +aib(ib,2)/reff(i,k,1))
          taucld(i,k,2)=cwp(i,k,2)*(awb(ib,1) &
                          +awb(ib,2)/reff(i,k,2))
          taucld(i,k,3)=cwp(i,k,3)*arb(ib,1)
        enddo
       enddo
      endif
!-----options for scaling cloud optical thickness

      if (overcast) then
       do k=1,np
        do i=1,m
          tauclb(i,k)=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          tauclf(i,k)=tauclb(i,k)
        enddo
       enddo
       do k=1,3
        do i=1,m
          cc(i,k)=1.0
        enddo
       enddo
      else
!-----scale cloud optical thickness in each layer from taucld (with
!     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
!     tauclb is the scaled optical thickness for beam radiation and
!     tauclf is for diffuse radiation.
       call cloud_scale (m,np,cosz,fcld,taucld,ict,icb, &
                    cc,tauclb,tauclf)
      endif


!-----compute cloud single scattering albedo and asymmetry factor
!     for a mixture of ice and liquid particles.
!     eqs.(4.6)-(4.8), (6.2)-(6.4)
       do k=1,np
        do i=1,m
           ssaclt(i)=0.99999
           asyclt(i)=1.0
           taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          if (taux.gt.taux_min .and. fcld(i,k).gt.fcld_min) then
           w1=(1.-(aia(ib,1)+(aia(ib,2)+ &
               aia(ib,3)*reff(i,k,1))*reff(i,k,1)))*taucld(i,k,1)
           w2=(1.-(awa(ib,1)+(awa(ib,2)+ &
               awa(ib,3)*reff(i,k,2))*reff(i,k,2)))*taucld(i,k,2)
           w3=(1.- ara(ib,1))*taucld(i,k,3)
           ssaclt(i)=(w1+w2+w3)/taux
           g1=(aig(ib,1)+(aig(ib,2)+aig(ib,3)*reff(i,k,1))*reff(i,k,1))*w1
           g2=(awg(ib,1)+(awg(ib,2)+awg(ib,3)*reff(i,k,2))*reff(i,k,2))*w2
           g3= arg(ib,1)*w3
           asyclt(i)=(g1+g2+g3)/(w1+w2+w3)
          endif
        enddo
         do i=1,m
           ssacl(i,k)=ssaclt(i)
         enddo
         do i=1,m
           asycl(i,k)=asyclt(i)
         enddo
       enddo


!-----integration over the k-distribution function
       do 200 ik=1,nk


!-----compute clear-sky optical thickness, single scattering albedo,
!     and asymmetry factor. eqs.(6.2)-(6.4)
        do k=1,np
         do i=1,m
           taurs=ry(ib)*dp(i,k)
           tauwv=xk(ik)*wh(i,k)

           tausto(i,k)=max(taurs+tauwv+taual(i,k,iv),opt_min)
           ssatau(i,k)=max(ssaal(i,k,iv)*taual(i,k,iv)+taurs,opt_min) !add for stability 
           asysto(i,k)=max(asyal(i,k,iv)*ssaal(i,k,iv)*taual(i,k,iv),opt_min)

       if (overcast .and. fast_overcast ) then
       else

!-----compute reflectance and transmittance of the clear portion of a layer
           tautob(i,k)=tausto(i,k)
           ssatob(i,k)=max(ssatau(i,k)/tautob(i,k),opt_min)
           ssatob(i,k)=min(ssatob(i,k),0.999999_fp_kind)
           asytob(i,k)=max(asysto(i,k)/(ssatob(i,k)*tautob(i,k)),opt_min)

!     delta-eddington approximation for optical propeties
!-----for direct incident radiation

          call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), cosz(i) , &
                       rrt(i,k),ttt(i,k), tdt(i,k) )


!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs. (6.5) and (6.6)

         call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), dsm(i) , &
                       rst(i,k),tst(i,k), dum(i,k) )

            rr(i,k,1)=rrt(i,k)
            tt(i,k,1)=ttt(i,k)
            td(i,k,1)=tdt(i,k)
            rs(i,k,1)=rst(i,k)
            ts(i,k,1)=tst(i,k)

         endif ! overcast .and. fast_overcast

!-----compute reflectance and transmittance of the cloudy portion of a layer
        if ( (tauclb(i,k).ge.taux_min .or. fcld(i,k).ge.fcld_min ) .or. &  !cloud exists
             (overcast .and. fast_overcast ) ) then                        ! overcast .and. fast_overcast


!-----for direct incident radiation. eqs.(6.2)-(6.4)
           tautob(i,k)=tausto(i,k)+max(tauclb(i,k),opt_min)
           ssatob(i,k)=max((ssatau(i,k)+ssacl(i,k)*tauclb(i,k))/tautob(i,k),opt_min)
           ssatob(i,k)=min(ssatob(i,k),0.999999_fp_kind)
           asytob(i,k)=max((asysto(i,k)+asycl(i,k)*ssacl(i,k)*tauclb(i,k)) &
                      /(ssatob(i,k)*tautob(i,k)),opt_min)
!-----for diffuse incident radiation
           tautof(i,k)=tausto(i,k)+max(tauclf(i,k),opt_min)
           ssatof(i,k)=max((ssatau(i,k)+ssacl(i,k)*tauclf(i,k))/tautof(i,k),opt_min) 
           ssatof(i,k)=min(ssatof(i,k),0.999999_fp_kind)
           asytof(i,k)=max((asysto(i,k)+asycl(i,k)*ssacl(i,k)*tauclf(i,k)) &
                      /(ssatof(i,k)*tautof(i,k)),opt_min)

!     delta-eddington approximation for optical propeties
!-----for direct incident radiation

          call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), cosz(i) , &
                       rrt(i,k),ttt(i,k), tdt(i,k) )

!-----diffuse incident radiation is approximated by beam radiation with
!     an incident angle of 53 degrees, eqs.(6.5) and (6.6)

         call delta_eddington(tautob(i,k), ssatob(i,k), asytob(i,k), dsm(i) , &
                       rst(i,k),tst(i,k), dum(i,k) )
        endif


            rr(i,k,2)=rrt(i,k)
            tt(i,k,2)=ttt(i,k)
            td(i,k,2)=tdt(i,k)
            rs(i,k,2)=rst(i,k)
            ts(i,k,2)=tst(i,k)

         enddo
        enddo


!-----flux calculations
    if (overcast) then  ! overcast  (LES or CRM)

       if( .not. fast_overcast ) then
!-----for clear-sky fluxes only
         ih1=1 ; im1=1 ; is1=1
         ih2=1 ; im2=1 ; is2=1
         call twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,dum1,falld,dum2,dum3) 
       endif

!-----for cloudy-sky fluxes only
        ih1=2 ; im1=2 ; is1=2
        ih2=2 ; im2=2 ; is2=2
        call twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,dum1,fall,falld,fsdir,fsdif) 

    else  ! NON overcast (GCM or RCM)

!-----for clear- and all-sky fluxes
!     the all-sky flux, fall is the summation inside the brackets
!     of eq. (7.11)
         ih1=1 ; im1=1 ; is1=1
         ih2=2 ; im2=2 ; is2=2
         call twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                      cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif) 

  endif  ! ovrercast 


!-----flux integration following eq. (6.1)
       do k=1,np+1
        do i=1,m
          flx(i,k) = flx(i,k)+fall(i,k)*hk(ib,ik)
          flxd(i,k) = flxd(i,k)+falld(i,k)*hk(ib,ik)
          if(overcast .and. fast_overcast) then 
          else
            flc(i,k) = flc(i,k)+fclr(i,k)*hk(ib,ik)
          endif
        enddo
       enddo
!-----compute downward surface fluxes in the ir region
       do i=1,m
          fdirir(i) = fdirir(i)+fsdir(i)*hk(ib,ik)
          fdifir(i) = fdifir(i)+fsdif(i)*hk(ib,ik)
       enddo

  200 continue !k integration
  100 continue !iband


      return
      end subroutine sw_ir

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cloud_scale (m,np,cosz,fcld,taucld,ict,icb, &
                           cc,tauclb,tauclf)
!********************************************************************
!
!   this subroutine computes the high, middle, and low cloud
!    amounts and scales the cloud optical thickness (section 7)
!
!   to simplify calculations in a cloudy atmosphere, clouds are
!    grouped into high, middle and low clouds separated by the levels
!    ict and icb (level 1 is the top of the model atmosphere).
!
!   within each of the three groups, clouds are assumed maximally
!    overlapped, and the cloud cover (cc) of a group is the maximum
!    cloud cover of all the layers in the group.  the optical thickness
!    (taucld) of a given layer is then scaled to new values (tauclb and
!    tauclf) so that the layer reflectance corresponding to the cloud
!    cover cc is the same as the original reflectance with optical
!    thickness taucld and cloud cover fcld.
!
!---input parameters
!
!    number of atmospheric soundings (m)
!    number of atmospheric layers (np)
!    cosine of the solar zenith angle (cosz)
!    fractional cloud cover (fcld)
!    cloud optical thickness (taucld)
!    index separating high and middle clouds (ict)
!    index separating middle and low clouds (icb)
!
!---output parameters
!
!    fractional cover of high, middle, and low cloud groups (cc)
!    scaled cloud optical thickness for direct  radiation (tauclb)
!    scaled cloud optical thickness for diffuse radiation (tauclf)
!
!********************************************************************
      implicit none
!-----input parameters
      integer m,np,ict(m),icb(m)
      real(Kind=fp_kind) cosz(m),fcld(m,np),taucld(m,np,3)
!-----output parameters
      real(Kind=fp_kind) cc(m,3),tauclb(m,np),tauclf(m,np)
!-----temporary variables
      integer i,j,k,im,it,ia,kk
      real(Kind=fp_kind)  fm,ft,fa,xai,taux
!-----pre-computed table
!     size of cosz-interval:         dm
!     size of taucld-interval:       dt
!     size of cloud amount-interval: da
      integer   nm,nt,na
      parameter (nm=11,nt=9,na=11)
      real(Kind=fp_kind)  dm,dt,da,t1,caib(nm,nt,na),caif(nt,na)
      parameter (dm=0.1,dt=0.30103,da=0.1,t1=-0.9031)
!-----include the pre-computed table of mcai for scaling the cloud optical
!     thickness under the assumption that clouds are maximally overlapped
!
!     caib is for scaling the cloud optical thickness for direct radiation
!     caif is for scaling the cloud optical thickness for diffuse radiation
!     include "mcai.data"
      data ((caib(1,i,j),j=1,11),i=1,9)/ &
       .000,0.068,0.140,0.216,0.298,0.385,0.481,0.586,0.705,0.840,1.000, &
       .000,0.052,0.106,0.166,0.230,0.302,0.383,0.478,0.595,0.752,1.000, &
       .000,0.038,0.078,0.120,0.166,0.218,0.276,0.346,0.438,0.582,1.000, &
       .000,0.030,0.060,0.092,0.126,0.164,0.206,0.255,0.322,0.442,1.000, &
       .000,0.025,0.051,0.078,0.106,0.136,0.170,0.209,0.266,0.462,1.000, &
       .000,0.023,0.046,0.070,0.095,0.122,0.150,0.187,0.278,0.577,1.000, &
       .000,0.022,0.043,0.066,0.089,0.114,0.141,0.187,0.354,0.603,1.000, &
       .000,0.021,0.042,0.063,0.086,0.108,0.135,0.214,0.349,0.565,1.000, &
       .000,0.021,0.041,0.062,0.083,0.105,0.134,0.202,0.302,0.479,1.000/
      data ((caib(2,i,j),j=1,11),i=1,9)/ &
       .000,0.088,0.179,0.272,0.367,0.465,0.566,0.669,0.776,0.886,1.000, &
       .000,0.079,0.161,0.247,0.337,0.431,0.531,0.637,0.749,0.870,1.000, &
       .000,0.065,0.134,0.207,0.286,0.372,0.466,0.572,0.692,0.831,1.000, &
       .000,0.049,0.102,0.158,0.221,0.290,0.370,0.465,0.583,0.745,1.000, &
       .000,0.037,0.076,0.118,0.165,0.217,0.278,0.354,0.459,0.638,1.000, &
       .000,0.030,0.061,0.094,0.130,0.171,0.221,0.286,0.398,0.631,1.000, &
       .000,0.026,0.052,0.081,0.111,0.146,0.189,0.259,0.407,0.643,1.000, &
       .000,0.023,0.047,0.072,0.098,0.129,0.170,0.250,0.387,0.598,1.000, &
       .000,0.022,0.044,0.066,0.090,0.118,0.156,0.224,0.328,0.508,1.000/
      data ((caib(3,i,j),j=1,11),i=1,9)/ &
       .000,0.094,0.189,0.285,0.383,0.482,0.582,0.685,0.788,0.894,1.000, &
       .000,0.088,0.178,0.271,0.366,0.465,0.565,0.669,0.776,0.886,1.000, &
       .000,0.079,0.161,0.247,0.337,0.431,0.531,0.637,0.750,0.870,1.000, &
       .000,0.066,0.134,0.209,0.289,0.375,0.470,0.577,0.697,0.835,1.000, &
       .000,0.050,0.104,0.163,0.227,0.300,0.383,0.483,0.606,0.770,1.000, &
       .000,0.038,0.080,0.125,0.175,0.233,0.302,0.391,0.518,0.710,1.000, &
       .000,0.031,0.064,0.100,0.141,0.188,0.249,0.336,0.476,0.689,1.000, &
       .000,0.026,0.054,0.084,0.118,0.158,0.213,0.298,0.433,0.638,1.000, &
       .000,0.023,0.048,0.074,0.102,0.136,0.182,0.254,0.360,0.542,1.000/
      data ((caib(4,i,j),j=1,11),i=1,9)/ &
       .000,0.096,0.193,0.290,0.389,0.488,0.589,0.690,0.792,0.896,1.000, &
       .000,0.092,0.186,0.281,0.378,0.477,0.578,0.680,0.785,0.891,1.000, &
       .000,0.086,0.174,0.264,0.358,0.455,0.556,0.660,0.769,0.882,1.000, &
       .000,0.074,0.153,0.235,0.323,0.416,0.514,0.622,0.737,0.862,1.000, &
       .000,0.061,0.126,0.195,0.271,0.355,0.449,0.555,0.678,0.823,1.000, &
       .000,0.047,0.098,0.153,0.215,0.286,0.370,0.471,0.600,0.770,1.000, &
       .000,0.037,0.077,0.120,0.170,0.230,0.303,0.401,0.537,0.729,1.000, &
       .000,0.030,0.062,0.098,0.138,0.187,0.252,0.343,0.476,0.673,1.000, &
       .000,0.026,0.053,0.082,0.114,0.154,0.207,0.282,0.391,0.574,1.000/
      data ((caib(5,i,j),j=1,11),i=1,9)/ &
       .000,0.097,0.194,0.293,0.392,0.492,0.592,0.693,0.794,0.897,1.000, &
       .000,0.094,0.190,0.286,0.384,0.483,0.584,0.686,0.789,0.894,1.000, &
       .000,0.090,0.181,0.274,0.370,0.468,0.569,0.672,0.778,0.887,1.000, &
       .000,0.081,0.165,0.252,0.343,0.439,0.539,0.645,0.757,0.874,1.000, &
       .000,0.069,0.142,0.218,0.302,0.392,0.490,0.598,0.717,0.850,1.000, &
       .000,0.054,0.114,0.178,0.250,0.330,0.422,0.529,0.656,0.810,1.000, &
       .000,0.042,0.090,0.141,0.200,0.269,0.351,0.455,0.589,0.764,1.000, &
       .000,0.034,0.070,0.112,0.159,0.217,0.289,0.384,0.515,0.703,1.000, &
       .000,0.028,0.058,0.090,0.128,0.174,0.231,0.309,0.420,0.602,1.000/
      data ((caib(6,i,j),j=1,11),i=1,9)/ &
       .000,0.098,0.196,0.295,0.394,0.494,0.594,0.695,0.796,0.898,1.000, &
       .000,0.096,0.193,0.290,0.389,0.488,0.588,0.690,0.792,0.895,1.000, &
       .000,0.092,0.186,0.281,0.378,0.477,0.577,0.680,0.784,0.891,1.000, &
       .000,0.086,0.174,0.264,0.358,0.455,0.556,0.661,0.769,0.882,1.000, &
       .000,0.075,0.154,0.237,0.325,0.419,0.518,0.626,0.741,0.865,1.000, &
       .000,0.062,0.129,0.201,0.279,0.366,0.462,0.571,0.694,0.836,1.000, &
       .000,0.049,0.102,0.162,0.229,0.305,0.394,0.501,0.631,0.793,1.000, &
       .000,0.038,0.080,0.127,0.182,0.245,0.323,0.422,0.550,0.730,1.000, &
       .000,0.030,0.064,0.100,0.142,0.192,0.254,0.334,0.448,0.627,1.000/
      data ((caib(7,i,j),j=1,11),i=1,9)/ &
       .000,0.098,0.198,0.296,0.396,0.496,0.596,0.696,0.797,0.898,1.000, &
       .000,0.097,0.194,0.293,0.392,0.491,0.591,0.693,0.794,0.897,1.000, &
       .000,0.094,0.190,0.286,0.384,0.483,0.583,0.686,0.789,0.894,1.000, &
       .000,0.089,0.180,0.274,0.369,0.467,0.568,0.672,0.778,0.887,1.000, &
       .000,0.081,0.165,0.252,0.344,0.440,0.541,0.646,0.758,0.875,1.000, &
       .000,0.069,0.142,0.221,0.306,0.397,0.496,0.604,0.722,0.854,1.000, &
       .000,0.056,0.116,0.182,0.256,0.338,0.432,0.540,0.666,0.816,1.000, &
       .000,0.043,0.090,0.143,0.203,0.273,0.355,0.455,0.583,0.754,1.000, &
       .000,0.034,0.070,0.111,0.157,0.210,0.276,0.359,0.474,0.650,1.000/
      data ((caib(8,i,j),j=1,11),i=1,9)/ &
       .000,0.099,0.198,0.298,0.398,0.497,0.598,0.698,0.798,0.899,1.000, &
       .000,0.098,0.196,0.295,0.394,0.494,0.594,0.695,0.796,0.898,1.000, &
       .000,0.096,0.193,0.290,0.390,0.489,0.589,0.690,0.793,0.896,1.000, &
       .000,0.093,0.186,0.282,0.379,0.478,0.578,0.681,0.786,0.892,1.000, &
       .000,0.086,0.175,0.266,0.361,0.458,0.558,0.663,0.771,0.883,1.000, &
       .000,0.076,0.156,0.240,0.330,0.423,0.523,0.630,0.744,0.867,1.000, &
       .000,0.063,0.130,0.203,0.282,0.369,0.465,0.572,0.694,0.834,1.000, &
       .000,0.049,0.102,0.161,0.226,0.299,0.385,0.486,0.611,0.774,1.000, &
       .000,0.038,0.078,0.122,0.172,0.229,0.297,0.382,0.498,0.672,1.000/
      data ((caib(9,i,j),j=1,11),i=1,9)/ &
       .000,0.099,0.199,0.298,0.398,0.498,0.598,0.699,0.799,0.899,1.000, &
       .000,0.099,0.198,0.298,0.398,0.497,0.598,0.698,0.798,0.899,1.000, &
       .000,0.098,0.196,0.295,0.394,0.494,0.594,0.695,0.796,0.898,1.000, &
       .000,0.096,0.193,0.290,0.389,0.488,0.588,0.690,0.792,0.895,1.000, &
       .000,0.092,0.185,0.280,0.376,0.474,0.575,0.678,0.782,0.890,1.000, &
       .000,0.084,0.170,0.259,0.351,0.447,0.547,0.652,0.762,0.878,1.000, &
       .000,0.071,0.146,0.224,0.308,0.398,0.494,0.601,0.718,0.850,1.000, &
       .000,0.056,0.114,0.178,0.248,0.325,0.412,0.514,0.638,0.793,1.000, &
       .000,0.042,0.086,0.134,0.186,0.246,0.318,0.405,0.521,0.691,1.000/
      data ((caib(10,i,j),j=1,11),i=1,9)/ &
       .000,0.100,0.200,0.300,0.400,0.500,0.600,0.700,0.800,0.900,1.000, &
       .000,0.100,0.200,0.300,0.400,0.500,0.600,0.700,0.800,0.900,1.000, &
       .000,0.100,0.200,0.300,0.400,0.500,0.600,0.700,0.800,0.900,1.000, &
       .000,0.100,0.199,0.298,0.398,0.498,0.598,0.698,0.798,0.899,1.000, &
       .000,0.098,0.196,0.294,0.392,0.491,0.590,0.691,0.793,0.896,1.000, &
       .000,0.092,0.185,0.278,0.374,0.470,0.570,0.671,0.777,0.886,1.000, &
       .000,0.081,0.162,0.246,0.333,0.424,0.521,0.625,0.738,0.862,1.000, &
       .000,0.063,0.128,0.196,0.270,0.349,0.438,0.540,0.661,0.809,1.000, &
       .000,0.046,0.094,0.146,0.202,0.264,0.337,0.426,0.542,0.710,1.000/
      data ((caib(11,i,j),j=1,11),i=1,9)/ &
       .000,0.101,0.202,0.302,0.402,0.502,0.602,0.702,0.802,0.901,1.000, &
       .000,0.102,0.202,0.303,0.404,0.504,0.604,0.703,0.802,0.902,1.000, &
       .000,0.102,0.205,0.306,0.406,0.506,0.606,0.706,0.804,0.902,1.000, &
       .000,0.104,0.207,0.309,0.410,0.510,0.609,0.707,0.805,0.902,1.000, &
       .000,0.106,0.208,0.309,0.409,0.508,0.606,0.705,0.803,0.902,1.000, &
       .000,0.102,0.202,0.298,0.395,0.493,0.590,0.690,0.790,0.894,1.000, &
       .000,0.091,0.179,0.267,0.357,0.449,0.545,0.647,0.755,0.872,1.000, &
       .000,0.073,0.142,0.214,0.290,0.372,0.462,0.563,0.681,0.822,1.000, &
       .000,0.053,0.104,0.158,0.217,0.281,0.356,0.446,0.562,0.726,1.000/
      data ((caif(i,j),j=1,11),i=1,9)/ &
       .000,0.099,0.198,0.297,0.397,0.496,0.597,0.697,0.798,0.899,1.000, &
       .000,0.098,0.196,0.294,0.394,0.494,0.594,0.694,0.796,0.898,1.000, &
       .000,0.096,0.192,0.290,0.388,0.487,0.587,0.689,0.792,0.895,1.000, &
       .000,0.092,0.185,0.280,0.376,0.476,0.576,0.678,0.783,0.890,1.000, &
       .000,0.085,0.173,0.263,0.357,0.454,0.555,0.659,0.768,0.881,1.000, &
       .000,0.076,0.154,0.237,0.324,0.418,0.517,0.624,0.738,0.864,1.000, &
       .000,0.063,0.131,0.203,0.281,0.366,0.461,0.567,0.688,0.830,1.000, &
       .000,0.052,0.107,0.166,0.232,0.305,0.389,0.488,0.610,0.770,1.000, &
       .000,0.043,0.088,0.136,0.189,0.248,0.317,0.400,0.510,0.675,1.000/
!-----clouds within each of the high, middle, and low clouds are assumed
!     to be maximally overlapped, and the cloud cover (cc) for a group
!     (high, middle, or low) is the maximum cloud cover of all the layers
!     within a group
      do i=1,m
         cc(i,1)=0.0
         cc(i,2)=0.0
         cc(i,3)=0.0
      enddo
      do i=1,m
       do k=1,ict(i)-1
          cc(i,1)=max(cc(i,1),fcld(i,k))
       enddo
      enddo
      do i=1,m
        do k=ict(i),icb(i)-1
          cc(i,2)=max(cc(i,2),fcld(i,k))
       enddo
      enddo
      do i=1,m
       do k=icb(i),np
          cc(i,3)=max(cc(i,3),fcld(i,k))
       enddo
      enddo
!-----scale the cloud optical thickness.
!     taucld(i,k,1) is the optical thickness for ice particles
!     taucld(i,k,2) is the optical thickness for liquid particles
!     taucld(i,k,3) is the optical thickness for rain drops
      do i=1,m
       do k=1,np
         if(k.lt.ict(i)) then
            kk=1
         elseif(k.ge.ict(i) .and. k.lt.icb(i)) then
            kk=2
         else
            kk=3
         endif
         tauclb(i,k) = 0.0
         tauclf(i,k) = 0.0
         taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
         if (taux.gt.taux_min .and. fcld(i,k).gt.fcld_min) then
!-----normalize cloud cover following eq. (7.8)
           fa=fcld(i,k)/cc(i,kk)
!-----table look-up
           taux=min(taux,32._fp_kind)
           fm=cosz(i)/dm
           ft=(log10(taux)-t1)/dt
           fa=fa/da
           im=int(fm+1.5)
           it=int(ft+1.5)
           ia=int(fa+1.5)
           im=max(im,2)
           it=max(it,2)
           ia=max(ia,2)
           im=min(im,nm-1)
           it=min(it,nt-1)
           ia=min(ia,na-1)
           fm=fm-float(im-1)
           ft=ft-float(it-1)
           fa=fa-float(ia-1)
!-----scale cloud optical thickness for beam radiation following eq. (7.3)
!     the scaling factor, xai, is a function of the solar zenith
!     angle, optical thickness, and cloud cover.
           xai=    (-caib(im-1,it,ia)*(1.-fm)+ &
            caib(im+1,it,ia)*(1.+fm))*fm*.5+caib(im,it,ia)*(1.-fm*fm)
           xai=xai+(-caib(im,it-1,ia)*(1.-ft)+ &
            caib(im,it+1,ia)*(1.+ft))*ft*.5+caib(im,it,ia)*(1.-ft*ft)
           xai=xai+(-caib(im,it,ia-1)*(1.-fa)+ &
           caib(im,it,ia+1)*(1.+fa))*fa*.5+caib(im,it,ia)*(1.-fa*fa)
           xai= xai-2.*caib(im,it,ia)
           xai=max(xai,0.0_fp_kind)
           xai=min(xai,1.0_fp_kind)
           tauclb(i,k) = taux*xai
!-----scale cloud optical thickness for diffuse radiation following eq. (7.4)
!     the scaling factor, xai, is a function of the cloud optical
!     thickness and cover but not the solar zenith angle.
           xai=    (-caif(it-1,ia)*(1.-ft)+ &
            caif(it+1,ia)*(1.+ft))*ft*.5+caif(it,ia)*(1.-ft*ft)
           xai=xai+(-caif(it,ia-1)*(1.-fa)+ &
            caif(it,ia+1)*(1.+fa))*fa*.5+caif(it,ia)*(1.-fa*fa)
           xai= xai-caif(it,ia)
           xai=max(xai,0.0_fp_kind)
           xai=min(xai,1.0_fp_kind)
           tauclf(i,k) = taux*xai
         endif
       enddo
      enddo
      return
      end subroutine cloud_scale

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine delta_eddington(tau,ssc,g0,cza,rr,tt,td)

!*********************************************************************
!
!-----uses the delta-eddington approximation to compute the
!     bulk scattering properties of a single layer
!     coded following king and harshvardhan (jas, 1986)
!
!  inputs:
!
!     tau: the effective optical thickness
!     ssc: the effective single scattering albedo
!     g0:  the effective asymmetry factor
!     cza: cosine of solar zenith angle
!
!  outputs:
!
!     rr: the layer reflection of the direct beam
!     tt: the layer diffuse transmission of the direct beam
!     td: the layer direct transmission of the direct beam
!
!*********************************************************************
      implicit none
!*********************************************************************

      real(Kind=fp_kind) zero,one,two,three,four,fourth,seven,thresh
      parameter (one =1., three=3.)
      parameter (two =2., seven=7.)
      parameter (four=4., fourth=.25)
      parameter (zero=0., thresh=1.e-8)

!-----input parameters
      real(Kind=fp_kind) tau,ssc,g0,cza

!-----output parameters
      real(Kind=fp_kind) rr,tt,td

!-----temporary parameters

      real(Kind=fp_kind) zth,ff,xx,taup,sscp,gp,gm1,gm2,gm3,akk,alf1,alf2, &
           all,bll,st7,st8,cll,dll,fll,ell,st1,st2,st3,st4
       real(Kind=fp_kind) taupdzth,akkdtaup

!---------------------------------------------------------------------

                zth = cza 

!  delta-eddington scaling of single scattering albedo,
!  optical thickness, and asymmetry factor,
!  k & h eqs(27-29)

                ff  = g0*g0
                xx  = one-ff*ssc
                taup= tau*xx
                sscp= ssc*(one-ff)/xx
                gp  = g0/(one+g0)

!  gamma1, gamma2, and gamma3. see table 2 and eq(26) k & h
!  ssc and gp are the d-s single scattering
!  albedo and asymmetry factor.

                xx  =  three*gp
                gm1 =  (seven - sscp*(four+xx))*fourth
                gm2 = -(one   - sscp*(four-xx))*fourth

!  akk is k as defined in eq(25) of k & h

                akk = sqrt((gm1+gm2)*(gm1-gm2))

                xx  = akk * zth
           if (abs((one-xx)*(one+xx)) .lt. thresh) then
               zth = zth + 0.001
               xx  = akk * zth
           endif
                st7 = one - xx
                st8 = one + xx
                st3 = st7 * st8
!                if (abs(st3) .lt. thresh) then
!                    zth = zth + 0.001
!                    xx  = akk * zth
!                    st7 = one - xx
!                    st8 = one + xx
!                    st3 = st7 * st8
!                endif

!  extinction of the direct beam transmission
                td=0.
                taupdzth=taup/zth
                if (taupdzth .lt. 40. ) td  = exp(-taup/zth)
!  alf1 and alf2 are alpha1 and alpha2 from eqs (23) & (24) of k & h

                gm3  = (two - zth*three*gp)*fourth
                xx   = gm1 - gm2
                alf1 = gm1 - gm3 * xx
                alf2 = gm2 + gm3 * xx

!  all is last term in eq(21) of k & h
!  bll is last term in eq(22) of k & h

                xx  = akk * two
                all = (gm3 - alf2 * zth    )*xx*td
                bll = (one - gm3 + alf1*zth)*xx

                xx  = akk * gm3
                cll = (alf2 + xx) * st7
                dll = (alf2 - xx) * st8

                xx  = akk * (one-gm3)
                fll = (alf1 + xx) * st8
                ell = (alf1 - xx) * st7

                st2=0.
                akkdtaup=akk*taup
                if (akkdtaup.lt.40.) st2 = exp(-akkdtaup)
                st4 = st2 * st2

                st1 =  sscp / ((akk+gm1 + (akk-gm1)*st4) * st3)

!  rr is r-hat of eq(21) of k & h
!  tt is diffuse part of t-hat of eq(22) of k & h

                rr =   ( cll-dll*st4    -all*st2)*st1
                tt = - ((fll-ell*st4)*td-bll*st2)*st1

                rr = max(rr,zero)
                tt = max(tt,zero)
                tt = tt+td
      end subroutine delta_eddington

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 


      subroutine twostream_adding (m,np,ict,icb,ih1,ih2,im1,im2,is1,is2, &
                 cc,rr,tt,td,rs,ts,fclr,fall,falld,fsdir,fsdif)

!*******************************************************************
!  compute upward and downward fluxes using a two-stream adding method
!  following equations (6.9)-(6.16).
!
!  clouds are grouped into high, middle, and low clouds which are assumed
!  randomly overlapped. it involves a maximum of 8 sets of calculations.
!  in each set of calculations, each atmospheric layer is homogeneous,
!  either totally filled with clouds or without clouds.
!  input parameters:
!
!   m:   number of soundings
!   np:  number of atmospheric layers
!   ict: the level separating high and middle clouds
!   icb: the level separating middle and low clouds
!   ih1,ih2,im1,im2,is1,is2: indices for three group of clouds
!   cc:  effective cloud covers for high, middle and low clouds
!   rr:  reflection of a layer illuminated by beam radiation
!   tt:  total (direct+diffuse) transmission of a layer illuminated
!        by beam radiation
!   td:  direct beam transmission
!   rs:  reflection of a layer illuminated by diffuse radiation
!   ts:  transmission of a layer illuminated by diffuse radiation
!
!  output parameters:
!
!     fclr:  clear-sky flux divergence (downward minus upward)
!     fall:  all-sky flux divergence (downward minus upward)
!     fsdir: surface direct downward flux
!     fsdif: surface diffuse downward flux
!
!*********************************************************************c
!ccshie 8/19/04
     implicit none
!-----input parameters
      integer m,np,ict(m),icb(m),ih1,ih2,im1,im2,is1,is2
      real(Kind=fp_kind) rr(m,np+1,2),tt(m,np+1,2),td(m,np+1,2)
      real(Kind=fp_kind) rs(m,np+1,2),ts(m,np+1,2)
      real(Kind=fp_kind) cc(m,3)
!-----temporary array
      integer i,k,ih,im,is
      real(Kind=fp_kind) denm,xx,yy
      real(Kind=fp_kind) fupdif
      real(Kind=fp_kind) :: rra(m,np+1,2,2)
      real(Kind=fp_kind) :: tta(m,np+1,2,2)
      real(Kind=fp_kind) :: tda(m,np+1,2,2)
      real(Kind=fp_kind) :: rsa(m,np+1,2,2)
      real(Kind=fp_kind) :: rxa(m,np+1,2,2)
      real(Kind=fp_kind) :: ch(m)
      real(Kind=fp_kind) :: cm(m)
      real(Kind=fp_kind) :: ct(m)
      real(Kind=fp_kind) :: flxdn(m,np+1)
      real(Kind=fp_kind) :: fdndir(m)
      real(Kind=fp_kind) :: fdndif(m)
      real(Kind=fp_kind) flxdnu(m,np+1),flxdnd(m,np+1)

!-----output parameters
      real(Kind=fp_kind) fclr(m,np+1),fall(m,np+1)
      real(Kind=fp_kind) falld(m,np+1)
      real(Kind=fp_kind) fsdir(m),fsdif(m)


   if(overcast) then
!      ih1=2 ; im1=2 ; is1=2
   else
!      ih1=1 ; im1=1 ; is1=1
   endif
!      ih2=2 ; im2=2 ; is2=2



!-----initialize all-sky flux (fall) and surface downward fluxes
      do k=1,np+1
        do i=1,m
           fclr(i,k)=0.0
           fall(i,k)=0.0
           falld(i,k)=0.0
        enddo
      enddo

        do i=1,m
           fsdir(i)=0.0
           fsdif(i)=0.0
        enddo

!-----compute transmittances and reflectances for a composite of
!     layers. layers are added one at a time, going down from the top.
!     tda is the composite direct transmittance illuminated by beam radiation
!     tta is the composite total transmittance illuminated by
!         beam radiation
!     rsa is the composite reflectance illuminated from below
!         by diffuse radiation
!     tta and rsa are computed from eqs. (6.10) and (6.12)
!-----for high clouds
!     ih=1 for clear-sky condition, ih=2 for cloudy-sky condition

      do ih=ih1,ih2

       do i=1,m
          tda(i,1,ih,1)=td(i,1,ih)
          tta(i,1,ih,1)=tt(i,1,ih)
          rsa(i,1,ih,1)=rs(i,1,ih)
          tda(i,1,ih,2)=td(i,1,ih)
          tta(i,1,ih,2)=tt(i,1,ih)
          rsa(i,1,ih,2)=rs(i,1,ih)
       enddo

       do i=1,m
         do k=2,ict(i)-1
          denm = ts(i,k,ih)/( 1.-rsa(i,k-1,ih,1)*rs(i,k,ih))
          tda(i,k,ih,1)= tda(i,k-1,ih,1)*td(i,k,ih)
          tta(i,k,ih,1)= tda(i,k-1,ih,1)*tt(i,k,ih) &
                +(tda(i,k-1,ih,1)*rsa(i,k-1,ih,1)*rr(i,k,ih) &
                +tta(i,k-1,ih,1)-tda(i,k-1,ih,1))*denm    !additional -tda(i,k-1,ih,1)
          rsa(i,k,ih,1)= rs(i,k,ih)+ts(i,k,ih) &
                        *rsa(i,k-1,ih,1)*denm
          if(tda(i,k,ih,1).lt.1.e-10) tda(i,k,ih,1)=0. !!
          if(tta(i,k,ih,1).lt.1.e-10) tta(i,k,ih,1)=0. !!
          tda(i,k,ih,2)= tda(i,k,ih,1)
          tta(i,k,ih,2)= tta(i,k,ih,1)
          rsa(i,k,ih,2)= rsa(i,k,ih,1)
        enddo
       enddo

!-----for middle clouds
!     im=1 for clear-sky condition, im=2 for cloudy-sky condition

      do im=im1,im2
       do i=1,m
        do k=ict(i),icb(i)-1
          denm = ts(i,k,im)/( 1.-rsa(i,k-1,ih,im)*rs(i,k,im))
          tda(i,k,ih,im)= tda(i,k-1,ih,im)*td(i,k,im)
          tta(i,k,ih,im)= tda(i,k-1,ih,im)*tt(i,k,im) &
               +(tda(i,k-1,ih,im)*rsa(i,k-1,ih,im)*rr(i,k,im) &
               +tta(i,k-1,ih,im)-tda(i,k-1,ih,im))*denm   !additional -tda(i,k-1,ih,im)
          rsa(i,k,ih,im)= rs(i,k,im)+ts(i,k,im) &
                        *rsa(i,k-1,ih,im)*denm
          if(tda(i,k,ih,im).lt.1.e-10) tda(i,k,ih,im)=0. !!
          if(tta(i,k,ih,im).lt.1.e-10) tta(i,k,ih,im)=0. !!
        enddo
       enddo
      enddo                 ! end im loop

      enddo                 ! end ih loop

!-----layers are added one at a time, going up from the surface.
!     rra is the composite reflectance illuminated by beam radiation
!     rxa is the composite reflectance illuminated from above
!         by diffuse radiation
!     rra and rxa are computed from eqs. (6.9) and (6.11)
!-----for the low clouds
!     is=1 for clear-sky condition, is=2 for cloudy-sky condition

      do is=is1,is2

       do i=1,m
         rra(i,np+1,1,is)=rr(i,np+1,is)
         rxa(i,np+1,1,is)=rs(i,np+1,is)
         rra(i,np+1,2,is)=rr(i,np+1,is)
         rxa(i,np+1,2,is)=rs(i,np+1,is)
       enddo
       do i=1,m
         do k=np,icb(i),-1
          denm=ts(i,k,is)/( 1.-rs(i,k,is)*rxa(i,k+1,1,is) )
          rra(i,k,1,is)=rr(i,k,is)+(td(i,k,is)*rra(i,k+1,1,is) &
              +(tt(i,k,is)-td(i,k,is))*rxa(i,k+1,1,is))*denm  !additional -td(i,k,is)
          rxa(i,k,1,is)= rs(i,k,is)+ts(i,k,is) &
              *rxa(i,k+1,1,is)*denm
          rra(i,k,2,is)=rra(i,k,1,is)
          rxa(i,k,2,is)=rxa(i,k,1,is)
        enddo
       enddo
!-----for middle clouds
      do im=im1,im2
       do i=1,m
        do k=icb(i)-1,ict(i),-1
          denm=ts(i,k,im)/( 1.-rs(i,k,im)*rxa(i,k+1,im,is) )
          rra(i,k,im,is)= rr(i,k,im)+(td(i,k,im)*rra(i,k+1,im,is) &
              +(tt(i,k,im)-td(i,k,im))*rxa(i,k+1,im,is))*denm   !additiona -td(i,k,im)
          rxa(i,k,im,is)= rs(i,k,im)+ts(i,k,im) &
              *rxa(i,k+1,im,is)*denm
        enddo
       enddo
      enddo                 ! end im loop

      enddo                 ! end is loop

!-----integration over eight sky situations.
!     ih, im, is denotes high, middle and low cloud groups.

      do ih=ih1,ih2

!-----clear portion
         if(ih.eq.1) then
           do i=1,m
             ch(i)=1.0-cc(i,1)
           enddo
          else
!-----cloudy portion
           do i=1,m
             ch(i)=cc(i,1)
           enddo
          endif

      do im=im1,im2

!-----clear portion
         if(im.eq.1) then
           do i=1,m
              cm(i)=ch(i)*(1.0-cc(i,2))
           enddo
         else
!-----cloudy portion
           do i=1,m
              cm(i)=ch(i)*cc(i,2)
           enddo
         endif

      do is=is1,is2

!-----clear portion
         if(is.eq.1) then
           do i=1,m
             ct(i)=cm(i)*(1.0-cc(i,3))
           enddo
         else
!-----cloudy portion
           do i=1,m
             ct(i)=cm(i)*cc(i,3)
           enddo
         endif
!-----add one layer at a time, going down.
       do i=1,m
        do k=icb(i),np
          denm = ts(i,k,is)/( 1.-rsa(i,k-1,ih,im)*rs(i,k,is) )
          tda(i,k,ih,im)= tda(i,k-1,ih,im)*td(i,k,is)
          tta(i,k,ih,im)=  tda(i,k-1,ih,im)*tt(i,k,is) &
               +(tda(i,k-1,ih,im)*rr(i,k,is) &
               *rsa(i,k-1,ih,im)+tta(i,k-1,ih,im)-tda(i,k-1,ih,im))*denm   !additional -tda(i,k-1,ih,im)
          rsa(i,k,ih,im)= rs(i,k,is)+ts(i,k,is) &
               *rsa(i,k-1,ih,im)*denm
          if(tda(i,k,ih,im).lt.1.e-10) tda(i,k,ih,im)=0.  !!
          if(tta(i,k,ih,im).lt.1.e-10) tta(i,k,ih,im)=0.  !!
        enddo
       enddo
!-----add one layer at a time, going up.
       do i=1,m
        do k=ict(i)-1,1,-1
          denm =ts(i,k,ih)/(1.-rs(i,k,ih)*rxa(i,k+1,im,is))
          rra(i,k,im,is)= rr(i,k,ih)+(td(i,k,ih)*rra(i,k+1,im,is) &
              +(tt(i,k,ih)-td(i,k,ih))*rxa(i,k+1,im,is))*denm   !addittional -td(i,k,ih)
          rxa(i,k,im,is)= rs(i,k,ih)+ts(i,k,ih) &
              *rxa(i,k+1,im,is)*denm
        enddo
       enddo
!-----compute fluxes following eq. (6.15) for fupdif and
!     eq. (6.16) for (fdndir+fdndif)
!     fdndir is the direct  downward flux
!     fdndif is the diffuse downward flux
!     fupdif is the diffuse upward flux
      do k=2,np+1
       do i=1,m
         denm= 1./(1.-rsa(i,k-1,ih,im)*rxa(i,k,im,is))
         fdndir(i)= tda(i,k-1,ih,im)
         xx= tda(i,k-1,ih,im)*rra(i,k,im,is)
         yy= tta(i,k-1,ih,im)-tda(i,k-1,ih,im)    !additional -tda(i,k-1,ih,im)
         fdndif(i)= (xx*rsa(i,k-1,ih,im)+yy)*denm
         fupdif= (xx+yy*rxa(i,k,im,is))*denm
         flxdn(i,k)= fdndir(i)+fdndif(i)-fupdif
         flxdnu(i,k)=-fupdif
         flxdnd(i,k)=fdndir(i)+fdndif(i)
       enddo
      enddo
       do i=1,m
         flxdn(i,1)=1.0-rra(i,1,im,is)
         flxdnu(i,1)=-rra(i,1,im,is)
         flxdnd(i,1)=1.0
       enddo
!-----summation of fluxes over all sky situations;
!     the term in the brackets of eq. (7.11)
       do k=1,np+1
        do i=1,m
           if(ih.eq.1 .and. im.eq.1 .and. is.eq.1) then
             fclr(i,k)=flxdn(i,k)
           endif
             fall(i,k)=fall(i,k)+flxdn(i,k)*ct(i)
             falld(i,k)=falld(i,k)+flxdnd(i,k)*ct(i)
        enddo
       enddo

        do i=1,m
            fsdir(i)=fsdir(i)+fdndir(i)*ct(i)
            fsdif(i)=fsdif(i)+fdndif(i)*ct(i)
        enddo

       enddo                 ! end is loop

     enddo                 ! end im loop

   enddo                 ! end ih loop


      return
      end subroutine twostream_adding 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine reduce_flux (m,np,swc,u1,du,nu,swh,w1,dw,nw,tbl,df)
!*****************************************************************
!-----compute the reduction of clear-sky downward solar flux
!     due to co2 absorption.
      implicit none
!-----input parameters
      integer m,np,nu,nw
      real(Kind=fp_kind) u1,du,w1,dw
      real(Kind=fp_kind) swc(m,np+1),swh(m,np+1),tbl(nu,nw)
!-----output (undated) parameter
      real(Kind=fp_kind) df(m,np+1)
!-----temporary array
      integer i,k,ic,iw
      real(Kind=fp_kind) clog,wlog,dc,dd,x0,x1,x2,y0,y1,y2
!-----table look-up for the reduction of clear-sky solar
         x0=u1+float(nu)*du
         y0=w1+float(nw)*dw
         x1=u1-0.5*du
         y1=w1-0.5*dw
      do k= 2, np+1
       do i= 1, m
          clog=min(swc(i,k),x0)
          clog=max(swc(i,k),x1)
          wlog=min(swh(i,k),y0)
          wlog=max(swh(i,k),y1)
          ic=int( (clog-x1)/du+1.)
          iw=int( (wlog-y1)/dw+1.)
          if(ic.lt.2)ic=2
          if(iw.lt.2)iw=2
          if(ic.gt.nu)ic=nu
          if(iw.gt.nw)iw=nw
          dc=clog-float(ic-2)*du-u1
          dd=wlog-float(iw-2)*dw-w1
          x2=tbl(ic-1,iw-1)+(tbl(ic-1,iw)-tbl(ic-1,iw-1))/dw*dd
          y2=x2+(tbl(ic,iw-1)-tbl(ic-1,iw-1))/du*dc
          df(i,k)=df(i,k)+y2
       enddo
      enddo
      return
      end subroutine reduce_flux

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine o3_profile (np, pres, ozone, its, ite, kts, kte, p2d, o3)
      implicit none

      integer m,np,its,ite,kts,kte
      integer i,k,ko,kk
      real(Kind=fp_kind) pres(np),ozone(np)
      real(Kind=fp_kind) p(its:ite,kts:kte),o3(its:ite,kts:kte)
      real(Kind=fp_kind),intent(in) :: p2d(its:ite,kts:kte)
!     statement function 
      real(Kind=fp_kind) linear, x1, y1, x2, y2, x
      linear(x1, y1, x2, y2, x) =  &
            (y1 * (x2 - x) + y2 * (x - x1)) / (x2 - x1)

      p = p2d
!
      do k = 1,np
        pres(k) = log(pres(k))  !toshii
      enddo
      do k = kts,kte
        do i = its, ite
          p(i,k) = log(p(i,k))  !toshii
        end do
      end do

! assume the pressure at model top is greater than pres(1)
! if it is not, this part needs to change

      do i = its, ite
        ko = 1
        do k = kts+1, kte
          do while (ko .lt. np .and. p(i,k) .gt. pres(ko))
            ko = ko + 1
          end do
          o3(i,k) =  linear (pres(ko),   ozone(ko),    &
                             pres(ko-1), ozone(ko-1),  &
                             p(i,k))
          ko = ko - 1
        end do
      end do

! calculate top lay o3
      do i = its, ite
        ko = 1
        k = kts
        do while (ko .le. np .and. p(i,k) .gt. pres(ko))
           ko = ko + 1
        end do
        if (ko-1 .le. 1) then
           o3(i,k)=ozone(k)
        else
           o3(i,k)=0.
           do kk=ko-2,1,-1
              o3(i,k)=o3(i,k)+ozone(kk)*(pres(kk+1)-pres(kk))
           enddo
           o3(i,k)=o3(i,k)/(pres(ko-1)-pres(1))
        endif
      end do

      end subroutine o3_profile

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine lwrad (m,np,pl,ta,wa,oa,tb,ts,emiss, &
                        cwc,reff,fcld,ict,icb, &
                        taual,ssaal,asyal,&
                        flx,acflxd,acflxu)
!***********************************************************************
! this routine computes ir fluxes due to water vapor, co2, o3,
!   trace gases (n2o, ch4, cfc11, cfc12, cfc22, co2-minor),
!   clouds, and aerosols.
!
! this is a vectorized code.  it computes fluxes simultaneously for
!   m soundings.
!
! some detailed descriptions of the radiation routine are given in
!   chou and suarez (1994).
!
! ice and liquid cloud particles are allowed to co-exist in any of the
!  np layers.
!
! if no information is available for the effective cloud particle size,
!  reff, default values of 10 micron for liquid water and 75 micron
!  for ice can be used.
!
! the maximum-random assumption is applied for cloud overlapping.
!  clouds are grouped into high, middle, and low clouds separated by the
!  level indices ict and icb.  within each of the three groups, clouds
!  are assumed maximally overlapped, and the cloud cover of a group is
!  the maximum cloud cover of all the layers in the group.  clouds among
!  the three groups are assumed randomly overlapped. the indices ict and
!  icb correpond approximately to the 400 mb and 700 mb levels.
!
! aerosols are allowed to be in any of the np layers. aerosol optical
!  properties can be specified as functions of height and spectral band.
!
! the ir spectrum is divided into nine bands:
!
!   band     wavenumber (/cm)   absorber
!
!    1           0 - 340           h2o
!    2         340 - 540           h2o
!    3         540 - 800       h2o,cont,co2
!    4         800 - 980       h2o,cont
!                              co2,f11,f12,f22
!    5         980 - 1100      h2o,cont,o3
!                              co2,f11
!    6        1100 - 1215      h2o,cont
!                              n2o,ch4,f12,f22
!    7        1215 - 1380      h2o,cont
!                              n2o,ch4
!    8        1380 - 1900          h2o
!    9        1900 - 3000          h2o
!
! in addition, a narrow band in the 17 micrometer region is added to
!    compute flux reduction due to n2o
!
!    10        540 - 620       h2o,cont,co2,n2o
!
! band 3 (540-800/cm) is further divided into 3 sub-bands :
!
!   subband   wavenumber (/cm)
!
!    1          540 - 620
!    2          620 - 720
!    3          720 - 800
!
!---- input parameters                               units    size
!
!   number of soundings (m)                            --      1
!   number of atmospheric layers (np)                  --      1
!   level pressure (pl)                               mb      m*(np+1)
!   layer temperature (ta)                            k       m*np
!   layer specific humidity (wa)                      g/g     m*np
!   layer ozone mixing ratio by mass (oa)             g/g     m*np
!   surface air temperature (tb)                      k        m
!   surface temperature (ts)                          k        m
!   surface emissivity (emiss)                      fraction   m*10
!   input option for cloud fractional cover            --      1
!      (overcast)   (see explanation above)
!   input option for cloud optical thickness           --      1
!      (cldwater)   (see explanation above)
!   cloud water mixing ratio (cwc)                   gm/gm   m*np*3
!       index 1 for ice particles
!       index 2 for liquid drops
!       index 3 for rain drops
!   cloud optical thickness (taucl)                    --    m*np*3
!       index 1 for ice particles
!       index 2 for liquid drops
!       index 3 for rain drops
!   effective cloud-particle size (reff)          micrometer m*np*3
!       index 1 for ice particles
!       index 2 for liquid drops
!       index 3 for rain drops
!   cloud amount (fcld)                             fraction  m*np
!   level index separating high and middle             --      1
!       clouds (ict)
!   level index separating middle and low              --      1
!       clouds (icb)
!   aerosol optical thickness (taual)                  --   m*np*10
!   aerosol single-scattering albedo (ssaal)           --   m*np*10
!   aerosol asymmetry factor (asyal)                   --   m*np*10
!   high (see explanation above)                       --      1
!   trace (see explanation above)                      --      1
!
! data used in table look-up for transmittance calculations:
!
!   c1 , c2, c3: for co2 (band 3)
!   o1 , o2, o3: for  o3 (band 5)
!   h11,h12,h13: for h2o (band 1)
!   h21,h22,h23: for h2o (band 2)
!   h81,h82,h83: for h2o (band 8)
!
!---- output parameters
!
!   net downward flux, all-sky   (flx)             w/m**2  m*(np+1)
!   net downward flux, clear-sky (flc)             w/m**2  m*(np+1)
!   sensitivity of net downward flux
!       to surface temperature (dfdts)            w/m**2/k m*(np+1)
!   emission by the surface (sfcem)                 w/m**2     m
!
! notes:
!
!   (1) water vapor continuum absorption is included in 540-1380 /cm.
!   (2) scattering is parameterized for clouds and aerosols.
!   (3) diffuse cloud and aerosol transmissions are computed
!       from exp(-1.66*tau).
!   (4) if there are no clouds, flx=flc.
!   (5) plevel(1) is the pressure at the top of the model atmosphere,
!        and plevel(np+1) is the surface pressure.
!   (6) downward flux is positive and upward flux is negative.
!   (7) sfcem and dfdts are negative because upward flux is defined as negative.
!   (8) for questions and coding errors, plaese contact ming-dah chou,
!       code 913, nasa/goddard space flight center, greenbelt, md 20771.
!       phone: 301-614-6192, fax: 301-614-6307,
!       e-mail: chou@climate.gsfc.nasa.gov
!
!***************************************************************************
     implicit none
!---- input parameters ------
      integer,intent(in) ::  m,np,ict(m),icb(m)
      real(Kind=fp_kind) ,intent(in) :: pl(m,np+1),ta(m,np),wa(m,np),oa(m,np), &
           tb(m),ts(m), emiss(m,ib_lw)
      real(Kind=fp_kind) ,intent(in) :: cwc(m,np,3),reff(m,np,3), fcld(m,np)

      real(Kind=fp_kind) , intent(in) :: taual(m,np,ib_lw),ssaal(m,np,ib_lw),asyal(m,np,ib_lw)
!---- output parameters ------
      real(Kind=fp_kind),intent(out) :: flx(m,np+1)


      real(Kind=fp_kind) :: flc(m,np+1),dfdts(m,np+1), sfcem(m)
      real(Kind=fp_kind) :: acflxu(m,np+1),acflxd(m,np+1)  !upwelling and downwelling broadband LW flux [W/m2]

!---- static data -----
      real(Kind=fp_kind) cb(6,10),xkw(9),xke(9),aw(9),bw(9),pm(9),fkw(6,9),gkw(6,3)
      real(Kind=fp_kind) aib(3,10),awb(4,10),aiw(4,10),aww(4,10),aig(4,10),awg(4,10)
      integer ne(9),mw(9)
      real(Kind=fp_kind) :: taucl(m,np,3) 
!-----parameters defining the size of the pre-computed tables for
!     transmittance using table look-up.
!c    "nx" is the number of intervals in pressure
!     "nx2" is the number of intervals in pressure
!     "no" is the number of intervals in o3 amount
!     "nc" is the number of intervals in co2 amount
!     "nh" is the number of intervals in h2o amount
      integer nx2,no,nc,nh
      parameter (nx2=26,no=21,nc=30,nh=31) ! cccshie 9/15/04
      real(Kind=fp_kind) c1 (nx2,nc),c2 (nx2,nc),c3 (nx2,nc)
      real(Kind=fp_kind) o1 (nx2,no),o2 (nx2,no),o3 (nx2,no)
      real(Kind=fp_kind) h11(nx2,nh),h12(nx2,nh),h13(nx2,nh)
      real(Kind=fp_kind) h21(nx2,nh),h22(nx2,nh),h23(nx2,nh)
      real(Kind=fp_kind) h71(nx2,nh),h72(nx2,nh),h73(nx2,nh)
      real(Kind=fp_kind) h81(nx2,nh),h82(nx2,nh),h83(nx2,nh)
!---- temporary arrays -----
     real(Kind=fp_kind) pa(m,np),dt(m,np)
     real(Kind=fp_kind) sh2o(m,np+1),swpre(m,np+1),swtem(m,np+1)
     real(Kind=fp_kind) sco3(m,np+1),scopre(m,np+1),scotem(m,np+1)
     real(Kind=fp_kind) dh2o(m,np),dcont(m,np),dco2(m,np),do3(m,np)
     real(Kind=fp_kind) dn2o(m,np),dch4(m,np)
     real(Kind=fp_kind) df11(m,np),df12(m,np),df22(m,np)
     real(Kind=fp_kind) th2o(m,6),tcon(m,3),tco2(m,6,2)
     real(Kind=fp_kind) tn2o(m,4),tch4(m,4),tcom(m,6)
     real(Kind=fp_kind) tf11(m),tf12(m),tf22(m)
     real(Kind=fp_kind) h2oexp(m,np,6),conexp(m,np,3),co2exp(m,np,6,2)
     real(Kind=fp_kind) n2oexp(m,np,4),ch4exp(m,np,4),comexp(m,np,6)
     real(Kind=fp_kind) f11exp(m,np),f12exp(m,np),f22exp(m,np)
     real(Kind=fp_kind) blayer(m,0:np+1),blevel(m,np+1),dblayr(m,np+1),dbs(m)
     real(Kind=fp_kind) dp(m,np),cwp(m,np,3)
     real(Kind=fp_kind) trant(m),tranal(m),transfc(m,np+1),trantcr(m,np+1)
     real(Kind=fp_kind) flxu(m,np+1),flxd(m,np+1),flcu(m,np+1),flcd(m,np+1)
     real(Kind=fp_kind) rflx(m,np+1),rflc(m,np+1)
     integer it(m),im(m),ib(m)
     real(Kind=fp_kind) cldhi(m),cldmd(m),cldlw(m),tcldlyr(m,np),fclr(m)
     real(Kind=fp_kind) taerlyr(m,np)

!      real(Kind=fp_kind) ,  allocatable :: pa(:,:),dt(:,:)
!      real(Kind=fp_kind) ,  allocatable :: sh2o(:,:),swpre(:,:),swtem(:,:)
!      real(Kind=fp_kind) ,  allocatable :: sco3(:,:),scopre(:,:),scotem(:,:)
!      real(Kind=fp_kind) ,  allocatable :: dh2o(:,:),dcont(:,:),dco2(:,:),do3(:,:)
!      real(Kind=fp_kind) ,  allocatable :: dn2o(:,:),dch4(:,:)
!      real(Kind=fp_kind) ,  allocatable :: df11(:,:),df12(:,:),df22(:,:)
!      real(Kind=fp_kind) ,  allocatable :: th2o(:,:),tcon(:,:),tco2(:,:,:)
!      real(Kind=fp_kind) ,  allocatable :: tn2o(:,:),tch4(:,:),tcom(:,:)
!      real(Kind=fp_kind) ,  allocatable :: tf11(:),tf12(:),tf22(:)
!      real(Kind=fp_kind) ,  allocatable :: h2oexp(:,:,:),conexp(:,:,:),co2exp(:,:,:,:)
!      real(Kind=fp_kind) ,  allocatable :: n2oexp(:,:,:),ch4exp(:,:,:),comexp(:,:,:)
!      real(Kind=fp_kind) ,  allocatable :: f11exp(:,:),f12exp(:,:),f22exp(:,:)
!      real(Kind=fp_kind) ,  allocatable :: blayer(:,:),blevel(:,:),dblayr(:,:),dbs(:)
!      real(Kind=fp_kind) ,  allocatable :: dp(:,:),cwp(:,:,:)
!      real(Kind=fp_kind),allocatable :: trant(:),tranal(:),transfc(:,:),trantcr(:,:)
!      real(Kind=fp_kind) ,  allocatable :: flxu(:,:),flxd(:,:),flcu(:,:),flcd(:,:)
!      real(Kind=fp_kind) ,  allocatable :: rflx(:,:),rflc(:,:)
!      integer,  allocatable :: it(:),im(:),ib(:)
!      real(Kind=fp_kind),allocatable::cldhi(:),cldmd(:),cldlw(:),tcldlyr(:,:),fclr(:)
!      real(Kind=fp_kind) ,  allocatable :: taerlyr(:,:)

      integer i,j,k,ip,iw,ibn,ik,iq,isb,k1,k2
      real(Kind=fp_kind) xx,yy,p1,dwe,dpe,a1,b1,fk1,a2,b2,fk2,bu,bd
      real(Kind=fp_kind) w1,w2,w3,g1,g2,g3,ww,gg,ff,taux,reff1,reff2
      real(Kind=fp_kind) tauxa

      logical oznbnd,co2bnd,h2otbl,conbnd,n2obnd
      logical ch4bnd,combnd,f11bnd,f12bnd,f22bnd,b10bnd

!-----the following coefficients are given in table 2 for computing
!     spectrally integrated planck fluxes using eq. (3.11)
       data cb/ &
            5.3443e+0,  -2.0617e-1,   2.5333e-3, &
           -6.8633e-6,   1.0115e-8,  -6.2672e-12, &
            2.7148e+1,  -5.4038e-1,   2.9501e-3, &
            2.7228e-7,  -9.3384e-9,   9.9677e-12, &
           -3.4860e+1,   1.1132e+0,  -1.3006e-2, &
            6.4955e-5,  -1.1815e-7,   8.0424e-11, &
           -6.0513e+1,   1.4087e+0,  -1.2077e-2, &
            4.4050e-5,  -5.6735e-8,   2.5660e-11, &
           -2.6689e+1,   5.2828e-1,  -3.4453e-3, &
            6.0715e-6,   1.2523e-8,  -2.1550e-11, &
           -6.7274e+0,   4.2256e-2,   1.0441e-3, &
           -1.2917e-5,   4.7396e-8,  -4.4855e-11, &
            1.8786e+1,  -5.8359e-1,   6.9674e-3, &
           -3.9391e-5,   1.0120e-7,  -8.2301e-11, &
            1.0344e+2,  -2.5134e+0,   2.3748e-2, &
           -1.0692e-4,   2.1841e-7,  -1.3704e-10, &
           -1.0482e+1,   3.8213e-1,  -5.2267e-3, &
            3.4412e-5,  -1.1075e-7,   1.4092e-10, &
            1.6769e+0,   6.5397e-2,  -1.8125e-3, &
            1.2912e-5,  -2.6715e-8,   1.9792e-11/
!-----xkw is the absorption coefficient are given in table 4 for the
!     first k-distribution interval due to water vapor line absorption.
!     units are cm**2/g
      data xkw / 29.55  , 4.167e-1, 1.328e-2, 5.250e-4, &
                 5.25e-4, 9.369e-3, 4.719e-2, 1.320e-0, 5.250e-4/
!-----xke is the absorption coefficient given in table 9 for the first
!     k-distribution function due to water vapor continuum absorption
!     units are cm**2/g
      data xke /  0.00,   0.00,   27.40,   15.8, &
                  9.40,   7.75,    8.78,    0.0,   0.0/
!-----mw is the ratio between neighboring absorption coefficients
!     for water vapor line absorption (table 4).
      data mw /6,6,8,6,6,8,9,6,16/
!-----aw and bw (table 3) are the coefficients for temperature scaling
!     in eq. (4.2).
      data aw/ 0.0021, 0.0140, 0.0167, 0.0302, &
               0.0307, 0.0195, 0.0152, 0.0008, 0.0096/
      data bw/ -1.01e-5, 5.57e-5, 8.54e-5, 2.96e-4, &
                2.86e-4, 1.108e-4, 7.608e-5, -3.52e-6, 1.64e-5/
!-----pm is the pressure-scaling parameter for water vapor absorption
!     eq. (4.1) and table 3.
      data pm/ 1.0, 1.0, 1.0, 1.0, 1.0, 0.77, 0.5, 1.0, 1.0/
!-----fkw is the planck-weighted k-distribution function due to h2o
!     line absorption given in table 4.
!     the k-distribution function for the third band, fkw(*,3),
!     is not used (see the parameter gkw below).
      data fkw / 0.2747,0.2717,0.2752,0.1177,0.0352,0.0255, &
                 0.1521,0.3974,0.1778,0.1826,0.0374,0.0527, &
                 6*1.00, &
                 0.4654,0.2991,0.1343,0.0646,0.0226,0.0140, &
                 0.5543,0.2723,0.1131,0.0443,0.0160,0.0000, &
                 0.5955,0.2693,0.0953,0.0335,0.0064,0.0000, &
                 0.1958,0.3469,0.3147,0.1013,0.0365,0.0048, &
                 0.0740,0.1636,0.4174,0.1783,0.1101,0.0566, &
                 0.1437,0.2197,0.3185,0.2351,0.0647,0.0183/
!-----gkw is the planck-weighted k-distribution function due to h2o
!     line absorption in the 3 subbands (800-720,620-720,540-620 /cm)
!     of band 3 given in table 10.  note that the order of the sub-bands
!     is reversed.
      data gkw/  0.1782,0.0593,0.0215,0.0068,0.0022,0.0000, &
                 0.0923,0.1675,0.0923,0.0187,0.0178,0.0000, &
                 0.0000,0.1083,0.1581,0.0455,0.0274,0.0041/
!-----ne is the number of terms used in each band to compute water vapor
!     continuum transmittance (table 9).
      data ne /0,0,3,1,1,1,1,0,0/
!
!-----coefficients for computing the extinction coefficient
!     for cloud ice particles (table 11a, eq. 6.4a).
!
      data aib /  -0.44171,    0.62951,   0.06465, &
                  -0.13727,    0.61291,   0.28962, &
                  -0.01878,    1.67680,   0.79080, &
                  -0.01896,    1.06510,   0.69493, &
                  -0.04788,    0.88178,   0.54492, &
                  -0.02265,    1.57390,   0.76161, &
                  -0.01038,    2.15640,   0.89045, &
                  -0.00450,    2.51370,   0.95989, &
                  -0.00044,    3.15050,   1.03750, &
                  -0.02956,    1.44680,   0.71283/
!
!-----coefficients for computing the extinction coefficient
!     for cloud liquid drops. (table 11b, eq. 6.4b)
!
      data awb /   0.08641,    0.01769,    -1.5572e-3,   3.4896e-5, &
                   0.22027,    0.00997,    -1.8719e-3,   5.3112e-5, &
                   0.38074,   -0.03027,     1.0154e-3,  -1.1849e-5, &
                   0.15587,    0.00371,    -7.7705e-4,   2.0547e-5, &
                   0.05518,    0.04544,    -4.2067e-3,   1.0184e-4, &
                   0.12724,    0.04751,    -5.2037e-3,   1.3711e-4, &
                   0.30390,    0.01656,    -3.5271e-3,   1.0828e-4, &
                   0.63617,   -0.06287,     2.2350e-3,  -2.3177e-5, &
                   1.15470,   -0.19282,     1.2084e-2,  -2.5612e-4, &
                   0.34021,   -0.02805,     1.0654e-3,  -1.5443e-5/
!
!-----coefficients for computing the single-scattering albedo
!     for cloud ice particles. (table 12a, eq. 6.5)
!
      data aiw/    0.17201,    1.2229e-2,  -1.4837e-4,   5.8020e-7, &
                   0.81470,   -2.7293e-3,   9.7816e-8,   5.7650e-8, &
                   0.54859,   -4.8273e-4,   5.4353e-6,  -1.5679e-8, &
                   0.39218,    4.1717e-3, - 4.8869e-5,   1.9144e-7, &
                   0.71773,   -3.3640e-3,   1.9713e-5,  -3.3189e-8, &
                   0.77345,   -5.5228e-3,   4.8379e-5,  -1.5151e-7, &
                   0.74975,   -5.6604e-3,   5.6475e-5,  -1.9664e-7, &
                   0.69011,   -4.5348e-3,   4.9322e-5,  -1.8255e-7, &
                   0.83963,   -6.7253e-3,   6.1900e-5,  -2.0862e-7, &
                   0.64860,   -2.8692e-3,   2.7656e-5,  -8.9680e-8/
!
!-----coefficients for computing the single-scattering albedo
!     for cloud liquid drops. (table 12b, eq. 6.5)
!
      data aww/   -7.8566e-2,  8.0875e-2,  -4.3403e-3,   8.1341e-5, &
                  -1.3384e-2,  9.3134e-2,  -6.0491e-3,   1.3059e-4, &
                   3.7096e-2,  7.3211e-2,  -4.4211e-3,   9.2448e-5, &
                  -3.7600e-3,  9.3344e-2,  -5.6561e-3,   1.1387e-4, &
                   0.40212,    7.8083e-2,  -5.9583e-3,   1.2883e-4, &
                   0.57928,    5.9094e-2,  -5.4425e-3,   1.2725e-4, &
                   0.68974,    4.2334e-2,  -4.9469e-3,   1.2863e-4, &
                   0.80122,    9.4578e-3,  -2.8508e-3,   9.0078e-5, &
                   1.02340,   -2.6204e-2,   4.2552e-4,   3.2160e-6, &
                   0.05092,    7.5409e-2,  -4.7305e-3,   1.0121e-4/
!
!-----coefficients for computing the asymmetry factor for cloud ice
!     particles. (table 13a, eq. 6.6)
!
      data aig /   0.57867,    1.0135e-2,  -1.1142e-4,   4.1537e-7, &
                   0.72259,    3.1149e-3,  -1.9927e-5,   5.6024e-8, &
                   0.76109,    4.5449e-3,  -4.6199e-5,   1.6446e-7, &
                   0.86934,    2.7474e-3,  -3.1301e-5,   1.1959e-7, &
                   0.89103,    1.8513e-3,  -1.6551e-5,   5.5193e-8, &
                   0.86325,    2.1408e-3,  -1.6846e-5,   4.9473e-8, &
                   0.85064,    2.5028e-3,  -2.0812e-5,   6.3427e-8, &
                   0.86945,    2.4615e-3,  -2.3882e-5,   8.2431e-8, &
                   0.80122,    3.1906e-3,  -2.4856e-5,   7.2411e-8, &
                   0.73290,    4.8034e-3,  -4.4425e-5,   1.4839e-7/
!
!-----coefficients for computing the asymmetry factor for cloud liquid
!     drops. (table 13b, eq. 6.6)
!
      data awg /  -0.51930,    0.20290,    -1.1747e-2,   2.3868e-4, &
                  -0.22151,    0.19708,    -1.2462e-2,   2.6646e-4, &
                   0.14157,    0.14705,    -9.5802e-3,   2.0819e-4, &
                   0.41590,    0.10482,    -6.9118e-3,   1.5115e-4, &
                   0.55338,    7.7016e-2,  -5.2218e-3,   1.1587e-4, &
                   0.61384,    6.4402e-2,  -4.6241e-3,   1.0746e-4, &
                   0.67891,    4.8698e-2,  -3.7021e-3,   9.1966e-5, &
                   0.78169,    2.0803e-2,  -1.4749e-3,   3.9362e-5, &
                   0.93218,   -3.3425e-2,   2.9632e-3,  -6.9362e-5, &
                   0.01649,    0.16561,    -1.0723e-2,   2.3220e-4/
!
!-----include tables used in the table look-up for co2 (band 3),
!     o3 (band 5), and h2o (bands 1, 2, and 7) transmission functions.
!     "co2.tran4" is the new co2 transmission table applicable to a large
!     range of co2 amount (up to 100 times of the present-time value).
!     include 'h2o.tran3'
!     include 'co2.tran4'
!     include 'o3.tran3'
      data ((h11(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.99993843,  0.99990183,  0.99985260,  0.99979079,  0.99971771, &
         0.99963379,  0.99953848,  0.99942899,  0.99930018,  0.99914461, &
         0.99895102,  0.99870503,  0.99838799,  0.99797899,  0.99745202, &
         0.99677002,  0.99587703,  0.99469399,  0.99311298,  0.99097902, &
         0.98807001,  0.98409998,  0.97864997,  0.97114998,  0.96086001, &
         0.94682997,  0.92777002,  0.90200001,  0.86739999,  0.82169998, &
         0.76270002/
      data ((h12(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.2021e-06, -0.3628e-06, -0.5891e-06, -0.8735e-06, -0.1204e-05, &
        -0.1579e-05, -0.2002e-05, -0.2494e-05, -0.3093e-05, -0.3852e-05, &
        -0.4835e-05, -0.6082e-05, -0.7591e-05, -0.9332e-05, -0.1128e-04, &
        -0.1347e-04, -0.1596e-04, -0.1890e-04, -0.2241e-04, -0.2672e-04, &
        -0.3208e-04, -0.3884e-04, -0.4747e-04, -0.5854e-04, -0.7272e-04, &
        -0.9092e-04, -0.1146e-03, -0.1458e-03, -0.1877e-03, -0.2435e-03, &
        -0.3159e-03/
      data ((h13(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.5907e-09,  0.8541e-09,  0.1095e-08,  0.1272e-08,  0.1297e-08, &
         0.1105e-08,  0.6788e-09, -0.5585e-10, -0.1147e-08, -0.2746e-08, &
        -0.5001e-08, -0.7715e-08, -0.1037e-07, -0.1227e-07, -0.1287e-07, &
        -0.1175e-07, -0.8517e-08, -0.2920e-08,  0.4786e-08,  0.1407e-07, &
         0.2476e-07,  0.3781e-07,  0.5633e-07,  0.8578e-07,  0.1322e-06, &
         0.2013e-06,  0.3006e-06,  0.4409e-06,  0.6343e-06,  0.8896e-06, &
         0.1216e-05/
      data ((h11(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.99993837,  0.99990171,  0.99985230,  0.99979031,  0.99971670, &
         0.99963200,  0.99953520,  0.99942321,  0.99928987,  0.99912637, &
         0.99892002,  0.99865198,  0.99830002,  0.99783802,  0.99723297, &
         0.99643701,  0.99537897,  0.99396098,  0.99204701,  0.98944002, &
         0.98588002,  0.98098999,  0.97425997,  0.96502000,  0.95236999, &
         0.93515998,  0.91184998,  0.88040000,  0.83859998,  0.78429997, &
         0.71560001/
      data ((h12(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.2017e-06, -0.3620e-06, -0.5878e-06, -0.8713e-06, -0.1201e-05, &
        -0.1572e-05, -0.1991e-05, -0.2476e-05, -0.3063e-05, -0.3808e-05, &
        -0.4776e-05, -0.6011e-05, -0.7516e-05, -0.9272e-05, -0.1127e-04, &
        -0.1355e-04, -0.1620e-04, -0.1936e-04, -0.2321e-04, -0.2797e-04, &
        -0.3399e-04, -0.4171e-04, -0.5172e-04, -0.6471e-04, -0.8150e-04, &
        -0.1034e-03, -0.1321e-03, -0.1705e-03, -0.2217e-03, -0.2889e-03, &
        -0.3726e-03/
      data ((h13(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.5894e-09,  0.8519e-09,  0.1092e-08,  0.1267e-08,  0.1289e-08, &
         0.1093e-08,  0.6601e-09, -0.7831e-10, -0.1167e-08, -0.2732e-08, &
        -0.4864e-08, -0.7334e-08, -0.9581e-08, -0.1097e-07, -0.1094e-07, &
        -0.8999e-08, -0.4669e-08,  0.2391e-08,  0.1215e-07,  0.2424e-07, &
         0.3877e-07,  0.5711e-07,  0.8295e-07,  0.1218e-06,  0.1793e-06, &
         0.2621e-06,  0.3812e-06,  0.5508e-06,  0.7824e-06,  0.1085e-05, &
         0.1462e-05/
      data ((h11(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.99993825,  0.99990153,  0.99985188,  0.99978942,  0.99971509, &
         0.99962920,  0.99953020,  0.99941432,  0.99927431,  0.99909937, &
         0.99887401,  0.99857497,  0.99817699,  0.99764699,  0.99694097, &
         0.99599802,  0.99473000,  0.99301600,  0.99068397,  0.98749000, &
         0.98311001,  0.97707999,  0.96877003,  0.95738000,  0.94186002, &
         0.92079002,  0.89230001,  0.85420001,  0.80430001,  0.74049997, &
         0.66200000/
      data ((h12(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.2011e-06, -0.3609e-06, -0.5859e-06, -0.8680e-06, -0.1195e-05, &
        -0.1563e-05, -0.1975e-05, -0.2450e-05, -0.3024e-05, -0.3755e-05, &
        -0.4711e-05, -0.5941e-05, -0.7455e-05, -0.9248e-05, -0.1132e-04, &
        -0.1373e-04, -0.1659e-04, -0.2004e-04, -0.2431e-04, -0.2966e-04, &
        -0.3653e-04, -0.4549e-04, -0.5724e-04, -0.7259e-04, -0.9265e-04, &
        -0.1191e-03, -0.1543e-03, -0.2013e-03, -0.2633e-03, -0.3421e-03, &
        -0.4350e-03/
      data ((h13(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.5872e-09,  0.8484e-09,  0.1087e-08,  0.1259e-08,  0.1279e-08, &
         0.1077e-08,  0.6413e-09, -0.9334e-10, -0.1161e-08, -0.2644e-08, &
        -0.4588e-08, -0.6709e-08, -0.8474e-08, -0.9263e-08, -0.8489e-08, &
        -0.5553e-08,  0.1203e-09,  0.9035e-08,  0.2135e-07,  0.3689e-07, &
         0.5610e-07,  0.8097e-07,  0.1155e-06,  0.1649e-06,  0.2350e-06, &
         0.3353e-06,  0.4806e-06,  0.6858e-06,  0.9617e-06,  0.1315e-05, &
         0.1741e-05/
      data ((h11(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.99993813,  0.99990118,  0.99985123,  0.99978811,  0.99971271, &
         0.99962479,  0.99952239,  0.99940068,  0.99925101,  0.99905968, &
         0.99880803,  0.99846900,  0.99800998,  0.99738997,  0.99655402, &
         0.99542397,  0.99389100,  0.99180400,  0.98895001,  0.98501998, &
         0.97961003,  0.97215003,  0.96191001,  0.94791001,  0.92887998, &
         0.90311998,  0.86849999,  0.82270002,  0.76370001,  0.69000000, &
         0.60240000/
      data ((h12(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.2001e-06, -0.3592e-06, -0.5829e-06, -0.8631e-06, -0.1187e-05, &
        -0.1549e-05, -0.1953e-05, -0.2415e-05, -0.2975e-05, -0.3694e-05, &
        -0.4645e-05, -0.5882e-05, -0.7425e-05, -0.9279e-05, -0.1147e-04, &
        -0.1406e-04, -0.1717e-04, -0.2100e-04, -0.2580e-04, -0.3191e-04, &
        -0.3989e-04, -0.5042e-04, -0.6432e-04, -0.8261e-04, -0.1068e-03, &
        -0.1389e-03, -0.1820e-03, -0.2391e-03, -0.3127e-03, -0.4021e-03, &
        -0.5002e-03/
      data ((h13(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.5838e-09,  0.8426e-09,  0.1081e-08,  0.1249e-08,  0.1267e-08, &
         0.1062e-08,  0.6313e-09, -0.8241e-10, -0.1094e-08, -0.2436e-08, &
        -0.4100e-08, -0.5786e-08, -0.6992e-08, -0.7083e-08, -0.5405e-08, &
        -0.1259e-08,  0.6099e-08,  0.1732e-07,  0.3276e-07,  0.5256e-07, &
         0.7756e-07,  0.1103e-06,  0.1547e-06,  0.2159e-06,  0.3016e-06, &
         0.4251e-06,  0.6033e-06,  0.8499e-06,  0.1175e-05,  0.1579e-05, &
         0.2044e-05/
      data ((h11(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.99993789,  0.99990070,  0.99985009,  0.99978602,  0.99970889, &
         0.99961799,  0.99951053,  0.99938041,  0.99921662,  0.99900270, &
         0.99871498,  0.99832201,  0.99778402,  0.99704897,  0.99604702, &
         0.99468100,  0.99281400,  0.99025702,  0.98673999,  0.98189002, &
         0.97521001,  0.96600002,  0.95337999,  0.93620998,  0.91292000, &
         0.88150001,  0.83969998,  0.78530002,  0.71650004,  0.63330001, &
         0.53799999/
      data ((h12(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.1987e-06, -0.3565e-06, -0.5784e-06, -0.8557e-06, -0.1175e-05, &
        -0.1530e-05, -0.1923e-05, -0.2372e-05, -0.2919e-05, -0.3631e-05, &
        -0.4587e-05, -0.5848e-05, -0.7442e-05, -0.9391e-05, -0.1173e-04, &
        -0.1455e-04, -0.1801e-04, -0.2232e-04, -0.2779e-04, -0.3489e-04, &
        -0.4428e-04, -0.5678e-04, -0.7333e-04, -0.9530e-04, -0.1246e-03, &
        -0.1639e-03, -0.2164e-03, -0.2848e-03, -0.3697e-03, -0.4665e-03, &
        -0.5646e-03/
      data ((h13(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.5785e-09,  0.8338e-09,  0.1071e-08,  0.1239e-08,  0.1256e-08, &
         0.1057e-08,  0.6480e-09, -0.1793e-10, -0.9278e-09, -0.2051e-08, &
        -0.3337e-08, -0.4514e-08, -0.5067e-08, -0.4328e-08, -0.1545e-08, &
         0.4100e-08,  0.1354e-07,  0.2762e-07,  0.4690e-07,  0.7190e-07, &
         0.1040e-06,  0.1459e-06,  0.2014e-06,  0.2764e-06,  0.3824e-06, &
         0.5359e-06,  0.7532e-06,  0.1047e-05,  0.1424e-05,  0.1873e-05, &
         0.2356e-05/
      data ((h11(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.99993753,  0.99989992,  0.99984848,  0.99978292,  0.99970299, &
         0.99960762,  0.99949282,  0.99935049,  0.99916708,  0.99892199, &
         0.99858701,  0.99812400,  0.99748403,  0.99660099,  0.99538797, &
         0.99372399,  0.99143797,  0.98829001,  0.98395002,  0.97794998, &
         0.96968001,  0.95832998,  0.94283003,  0.92179000,  0.89330000, &
         0.85530001,  0.80519998,  0.74140000,  0.66280001,  0.57099998, &
         0.47049999/
      data ((h12(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.1964e-06, -0.3526e-06, -0.5717e-06, -0.8451e-06, -0.1158e-05, &
        -0.1504e-05, -0.1886e-05, -0.2322e-05, -0.2861e-05, -0.3576e-05, &
        -0.4552e-05, -0.5856e-05, -0.7529e-05, -0.9609e-05, -0.1216e-04, &
        -0.1528e-04, -0.1916e-04, -0.2408e-04, -0.3043e-04, -0.3880e-04, &
        -0.4997e-04, -0.6488e-04, -0.8474e-04, -0.1113e-03, -0.1471e-03, &
        -0.1950e-03, -0.2583e-03, -0.3384e-03, -0.4326e-03, -0.5319e-03, &
        -0.6244e-03/
      data ((h13(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.5713e-09,  0.8263e-09,  0.1060e-08,  0.1226e-08,  0.1252e-08, &
         0.1076e-08,  0.7149e-09,  0.1379e-09, -0.6043e-09, -0.1417e-08, &
        -0.2241e-08, -0.2830e-08, -0.2627e-08, -0.8950e-09,  0.3231e-08, &
         0.1075e-07,  0.2278e-07,  0.4037e-07,  0.6439e-07,  0.9576e-07, &
         0.1363e-06,  0.1886e-06,  0.2567e-06,  0.3494e-06,  0.4821e-06, &
         0.6719e-06,  0.9343e-06,  0.1280e-05,  0.1705e-05,  0.2184e-05, &
         0.2651e-05/
      data ((h11(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.99993700,  0.99989867,  0.99984592,  0.99977797,  0.99969423, &
         0.99959219,  0.99946660,  0.99930722,  0.99909681,  0.99880999, &
         0.99841303,  0.99786001,  0.99708802,  0.99601799,  0.99453998, &
         0.99250001,  0.98969001,  0.98580003,  0.98041999,  0.97299999, &
         0.96279001,  0.94881999,  0.92980999,  0.90407002,  0.86949998, &
         0.82370001,  0.76459998,  0.69089997,  0.60310000,  0.50479996, &
         0.40219998/
      data ((h12(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.1932e-06, -0.3467e-06, -0.5623e-06, -0.8306e-06, -0.1136e-05, &
        -0.1472e-05, -0.1842e-05, -0.2269e-05, -0.2807e-05, -0.3539e-05, &
        -0.4553e-05, -0.5925e-05, -0.7710e-05, -0.9968e-05, -0.1278e-04, &
        -0.1629e-04, -0.2073e-04, -0.2644e-04, -0.3392e-04, -0.4390e-04, &
        -0.5727e-04, -0.7516e-04, -0.9916e-04, -0.1315e-03, -0.1752e-03, &
        -0.2333e-03, -0.3082e-03, -0.3988e-03, -0.4982e-03, -0.5947e-03, &
        -0.6764e-03/
      data ((h13(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.5612e-09,  0.8116e-09,  0.1048e-08,  0.1222e-08,  0.1270e-08, &
         0.1141e-08,  0.8732e-09,  0.4336e-09, -0.6548e-10, -0.4774e-09, &
        -0.7556e-09, -0.6577e-09,  0.4377e-09,  0.3359e-08,  0.9159e-08, &
         0.1901e-07,  0.3422e-07,  0.5616e-07,  0.8598e-07,  0.1251e-06, &
         0.1752e-06,  0.2392e-06,  0.3228e-06,  0.4389e-06,  0.6049e-06, &
         0.8370e-06,  0.1150e-05,  0.1547e-05,  0.2012e-05,  0.2493e-05, &
         0.2913e-05/
      data ((h11(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.99993622,  0.99989682,  0.99984211,  0.99977070,  0.99968100, &
         0.99956948,  0.99942881,  0.99924588,  0.99899900,  0.99865800, &
         0.99818099,  0.99751103,  0.99657297,  0.99526602,  0.99345201, &
         0.99094099,  0.98746002,  0.98264998,  0.97599000,  0.96682000, &
         0.95423001,  0.93708003,  0.91380000,  0.88239998,  0.84060001, &
         0.78610003,  0.71730000,  0.63400000,  0.53859997,  0.43660003, &
         0.33510000/
      data ((h12(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.1885e-06, -0.3385e-06, -0.5493e-06, -0.8114e-06, -0.1109e-05, &
        -0.1436e-05, -0.1796e-05, -0.2219e-05, -0.2770e-05, -0.3535e-05, &
        -0.4609e-05, -0.6077e-05, -0.8016e-05, -0.1051e-04, -0.1367e-04, &
        -0.1768e-04, -0.2283e-04, -0.2955e-04, -0.3849e-04, -0.5046e-04, &
        -0.6653e-04, -0.8813e-04, -0.1173e-03, -0.1569e-03, -0.2100e-03, &
        -0.2794e-03, -0.3656e-03, -0.4637e-03, -0.5629e-03, -0.6512e-03, &
        -0.7167e-03/
      data ((h13(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.5477e-09,  0.8000e-09,  0.1039e-08,  0.1234e-08,  0.1331e-08, &
         0.1295e-08,  0.1160e-08,  0.9178e-09,  0.7535e-09,  0.8301e-09, &
         0.1184e-08,  0.2082e-08,  0.4253e-08,  0.8646e-08,  0.1650e-07, &
         0.2920e-07,  0.4834e-07,  0.7564e-07,  0.1125e-06,  0.1606e-06, &
         0.2216e-06,  0.2992e-06,  0.4031e-06,  0.5493e-06,  0.7549e-06, &
         0.1035e-05,  0.1400e-05,  0.1843e-05,  0.2327e-05,  0.2774e-05, &
         0.3143e-05/
      data ((h11(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.99993503,  0.99989408,  0.99983650,  0.99975997,  0.99966192, &
         0.99953687,  0.99937540,  0.99916059,  0.99886602,  0.99845397, &
         0.99787402,  0.99705601,  0.99590701,  0.99430102,  0.99206603, &
         0.98896003,  0.98465002,  0.97869003,  0.97044003,  0.95911002, &
         0.94363999,  0.92260998,  0.89419997,  0.85609996,  0.80610001, &
         0.74220002,  0.66359997,  0.57169998,  0.47100002,  0.36860001, &
         0.27079999/
      data ((h12(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.1822e-06, -0.3274e-06, -0.5325e-06, -0.7881e-06, -0.1079e-05, &
        -0.1398e-05, -0.1754e-05, -0.2184e-05, -0.2763e-05, -0.3581e-05, &
        -0.4739e-05, -0.6341e-05, -0.8484e-05, -0.1128e-04, -0.1490e-04, &
        -0.1955e-04, -0.2561e-04, -0.3364e-04, -0.4438e-04, -0.5881e-04, &
        -0.7822e-04, -0.1045e-03, -0.1401e-03, -0.1884e-03, -0.2523e-03, &
        -0.3335e-03, -0.4289e-03, -0.5296e-03, -0.6231e-03, -0.6980e-03, &
        -0.7406e-03/
      data ((h13(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.5334e-09,  0.7859e-09,  0.1043e-08,  0.1279e-08,  0.1460e-08, &
         0.1560e-08,  0.1618e-08,  0.1657e-08,  0.1912e-08,  0.2569e-08, &
         0.3654e-08,  0.5509e-08,  0.8964e-08,  0.1518e-07,  0.2560e-07, &
         0.4178e-07,  0.6574e-07,  0.9958e-07,  0.1449e-06,  0.2031e-06, &
         0.2766e-06,  0.3718e-06,  0.5022e-06,  0.6849e-06,  0.9360e-06, &
         0.1268e-05,  0.1683e-05,  0.2157e-05,  0.2625e-05,  0.3020e-05, &
         0.3364e-05/
      data ((h11(ip,iw),iw=1,31), ip=10,10)/ &
         0.99993336,  0.99989021,  0.99982840,  0.99974459,  0.99963468, &
         0.99949121,  0.99930137,  0.99904430,  0.99868703,  0.99818403, &
         0.99747300,  0.99646801,  0.99505299,  0.99307102,  0.99030602, &
         0.98645997,  0.98111999,  0.97372001,  0.96353000,  0.94957000, &
         0.93058997,  0.90486002,  0.87029999,  0.82449996,  0.76530004, &
         0.69159997,  0.60380000,  0.50529999,  0.40259999,  0.30269998, &
         0.21020001/
      data ((h12(ip,iw),iw=1,31), ip=10,10)/ &
        -0.1742e-06, -0.3134e-06, -0.5121e-06, -0.7619e-06, -0.1048e-05, &
        -0.1364e-05, -0.1725e-05, -0.2177e-05, -0.2801e-05, -0.3694e-05, &
        -0.4969e-05, -0.6748e-05, -0.9161e-05, -0.1236e-04, -0.1655e-04, &
        -0.2203e-04, -0.2927e-04, -0.3894e-04, -0.5192e-04, -0.6936e-04, &
        -0.9294e-04, -0.1250e-03, -0.1686e-03, -0.2271e-03, -0.3027e-03, &
        -0.3944e-03, -0.4951e-03, -0.5928e-03, -0.6755e-03, -0.7309e-03, &
        -0.7417e-03/
      data ((h13(ip,iw),iw=1,31), ip=10,10)/ &
         0.5179e-09,  0.7789e-09,  0.1071e-08,  0.1382e-08,  0.1690e-08, &
         0.1979e-08,  0.2297e-08,  0.2704e-08,  0.3466e-08,  0.4794e-08, &
         0.6746e-08,  0.9739e-08,  0.1481e-07,  0.2331e-07,  0.3679e-07, &
         0.5726e-07,  0.8716e-07,  0.1289e-06,  0.1837e-06,  0.2534e-06, &
         0.3424e-06,  0.4609e-06,  0.6245e-06,  0.8495e-06,  0.1151e-05, &
         0.1536e-05,  0.1991e-05,  0.2468e-05,  0.2891e-05,  0.3245e-05, &
         0.3580e-05/
      data ((h11(ip,iw),iw=1,31), ip=11,11)/ &
         0.99993110,  0.99988490,  0.99981719,  0.99972337,  0.99959719, &
         0.99942869,  0.99920130,  0.99888903,  0.99845201,  0.99783301, &
         0.99695599,  0.99571502,  0.99396503,  0.99150997,  0.98808002, &
         0.98329997,  0.97667003,  0.96750998,  0.95494002,  0.93779999, &
         0.91453999,  0.88319999,  0.84130001,  0.78689998,  0.71799999, &
         0.63470000,  0.53909999,  0.43699998,  0.33550000,  0.24010003, &
         0.15420002/
      data ((h12(ip,iw),iw=1,31), ip=11,11)/ &
        -0.1647e-06, -0.2974e-06, -0.4900e-06, -0.7358e-06, -0.1022e-05, &
        -0.1344e-05, -0.1721e-05, -0.2212e-05, -0.2901e-05, -0.3896e-05, &
        -0.5327e-05, -0.7342e-05, -0.1011e-04, -0.1382e-04, -0.1875e-04, &
        -0.2530e-04, -0.3403e-04, -0.4573e-04, -0.6145e-04, -0.8264e-04, &
        -0.1114e-03, -0.1507e-03, -0.2039e-03, -0.2737e-03, -0.3607e-03, &
        -0.4599e-03, -0.5604e-03, -0.6497e-03, -0.7161e-03, -0.7443e-03, &
        -0.7133e-03/
      data ((h13(ip,iw),iw=1,31), ip=11,11)/ &
         0.5073e-09,  0.7906e-09,  0.1134e-08,  0.1560e-08,  0.2046e-08, &
         0.2589e-08,  0.3254e-08,  0.4107e-08,  0.5481e-08,  0.7602e-08, &
         0.1059e-07,  0.1501e-07,  0.2210e-07,  0.3334e-07,  0.5055e-07, &
         0.7629e-07,  0.1134e-06,  0.1642e-06,  0.2298e-06,  0.3133e-06, &
         0.4225e-06,  0.5709e-06,  0.7739e-06,  0.1047e-05,  0.1401e-05, &
         0.1833e-05,  0.2308e-05,  0.2753e-05,  0.3125e-05,  0.3467e-05, &
         0.3748e-05/
      data ((h11(ip,iw),iw=1,31), ip=12,12)/ &
         0.99992824,  0.99987793,  0.99980247,  0.99969512,  0.99954712, &
         0.99934530,  0.99906880,  0.99868500,  0.99814498,  0.99738002, &
         0.99629498,  0.99475700,  0.99258602,  0.98953998,  0.98527998, &
         0.97934997,  0.97112000,  0.95981002,  0.94433999,  0.92332000, &
         0.89490002,  0.85680002,  0.80680001,  0.74290001,  0.66420001, &
         0.57220000,  0.47149998,  0.36900002,  0.27109998,  0.18159997, &
         0.10460001/
      data ((h12(ip,iw),iw=1,31), ip=12,12)/ &
        -0.1548e-06, -0.2808e-06, -0.4683e-06, -0.7142e-06, -0.1008e-05, &
        -0.1347e-05, -0.1758e-05, -0.2306e-05, -0.3083e-05, -0.4214e-05, &
        -0.5851e-05, -0.8175e-05, -0.1140e-04, -0.1577e-04, -0.2166e-04, &
        -0.2955e-04, -0.4014e-04, -0.5434e-04, -0.7343e-04, -0.9931e-04, &
        -0.1346e-03, -0.1826e-03, -0.2467e-03, -0.3283e-03, -0.4246e-03, &
        -0.5264e-03, -0.6211e-03, -0.6970e-03, -0.7402e-03, -0.7316e-03, &
        -0.6486e-03/
      data ((h13(ip,iw),iw=1,31), ip=12,12)/ &
         0.5078e-09,  0.8244e-09,  0.1255e-08,  0.1826e-08,  0.2550e-08, &
         0.3438e-08,  0.4532e-08,  0.5949e-08,  0.8041e-08,  0.1110e-07, &
         0.1534e-07,  0.2157e-07,  0.3116e-07,  0.4570e-07,  0.6747e-07, &
         0.9961e-07,  0.1451e-06,  0.2061e-06,  0.2843e-06,  0.3855e-06, &
         0.5213e-06,  0.7060e-06,  0.9544e-06,  0.1280e-05,  0.1684e-05, &
         0.2148e-05,  0.2609e-05,  0.3002e-05,  0.3349e-05,  0.3670e-05, &
         0.3780e-05/
      data ((h11(ip,iw),iw=1,31), ip=13,13)/ &
         0.99992472,  0.99986941,  0.99978399,  0.99965900,  0.99948251, &
         0.99923742,  0.99889702,  0.99842298,  0.99775398,  0.99680400, &
         0.99545598,  0.99354500,  0.99084800,  0.98706001,  0.98176998, &
         0.97439998,  0.96423000,  0.95029002,  0.93129998,  0.90557003, &
         0.87099999,  0.82520002,  0.76600003,  0.69220001,  0.60440004, &
         0.50580001,  0.40310001,  0.30299997,  0.21039999,  0.12860000, &
         0.06360000/
      data ((h12(ip,iw),iw=1,31), ip=13,13)/ &
        -0.1461e-06, -0.2663e-06, -0.4512e-06, -0.7027e-06, -0.1014e-05, &
        -0.1387e-05, -0.1851e-05, -0.2478e-05, -0.3373e-05, -0.4682e-05, &
        -0.6588e-05, -0.9311e-05, -0.1311e-04, -0.1834e-04, -0.2544e-04, &
        -0.3502e-04, -0.4789e-04, -0.6515e-04, -0.8846e-04, -0.1202e-03, &
        -0.1635e-03, -0.2217e-03, -0.2975e-03, -0.3897e-03, -0.4913e-03, &
        -0.5902e-03, -0.6740e-03, -0.7302e-03, -0.7415e-03, -0.6858e-03, &
        -0.5447e-03/
      data ((h13(ip,iw),iw=1,31), ip=13,13)/ &
         0.5236e-09,  0.8873e-09,  0.1426e-08,  0.2193e-08,  0.3230e-08, &
         0.4555e-08,  0.6200e-08,  0.8298e-08,  0.1126e-07,  0.1544e-07, &
         0.2130e-07,  0.2978e-07,  0.4239e-07,  0.6096e-07,  0.8829e-07, &
         0.1280e-06,  0.1830e-06,  0.2555e-06,  0.3493e-06,  0.4740e-06, &
         0.6431e-06,  0.8701e-06,  0.1169e-05,  0.1547e-05,  0.1992e-05, &
         0.2460e-05,  0.2877e-05,  0.3230e-05,  0.3569e-05,  0.3782e-05, &
         0.3591e-05/
      data ((h11(ip,iw),iw=1,31), ip=14,14)/ &
         0.99992090,  0.99985969,  0.99976218,  0.99961531,  0.99940270, &
         0.99910218,  0.99868101,  0.99809098,  0.99725902,  0.99607700, &
         0.99440002,  0.99202299,  0.98866999,  0.98395997,  0.97737998, &
         0.96825999,  0.95570999,  0.93857002,  0.91531003,  0.88389999, &
         0.84210002,  0.78759998,  0.71869999,  0.63530004,  0.53970003, &
         0.43750000,  0.33590001,  0.24040002,  0.15439999,  0.08300000, &
         0.03299999/
      data ((h12(ip,iw),iw=1,31), ip=14,14)/ &
        -0.1402e-06, -0.2569e-06, -0.4428e-06, -0.7076e-06, -0.1051e-05, &
        -0.1478e-05, -0.2019e-05, -0.2752e-05, -0.3802e-05, -0.5343e-05, &
        -0.7594e-05, -0.1082e-04, -0.1536e-04, -0.2166e-04, -0.3028e-04, &
        -0.4195e-04, -0.5761e-04, -0.7867e-04, -0.1072e-03, -0.1462e-03, &
        -0.1990e-03, -0.2687e-03, -0.3559e-03, -0.4558e-03, -0.5572e-03, &
        -0.6476e-03, -0.7150e-03, -0.7439e-03, -0.7133e-03, -0.6015e-03, &
        -0.4089e-03/
      data ((h13(ip,iw),iw=1,31), ip=14,14)/ &
         0.5531e-09,  0.9757e-09,  0.1644e-08,  0.2650e-08,  0.4074e-08, &
         0.5957e-08,  0.8314e-08,  0.1128e-07,  0.1528e-07,  0.2087e-07, &
         0.2874e-07,  0.4002e-07,  0.5631e-07,  0.7981e-07,  0.1139e-06, &
         0.1621e-06,  0.2275e-06,  0.3136e-06,  0.4280e-06,  0.5829e-06, &
         0.7917e-06,  0.1067e-05,  0.1419e-05,  0.1844e-05,  0.2310e-05, &
         0.2747e-05,  0.3113e-05,  0.3455e-05,  0.3739e-05,  0.3715e-05, &
         0.3125e-05/
      data ((h11(ip,iw),iw=1,31), ip=15,15)/ &
         0.99991709,  0.99984968,  0.99973857,  0.99956548,  0.99930853, &
         0.99893898,  0.99841601,  0.99768001,  0.99664098,  0.99516898, &
         0.99308002,  0.99012297,  0.98594999,  0.98009998,  0.97194999, &
         0.96066999,  0.94523001,  0.92421001,  0.89579999,  0.85769999, &
         0.80760002,  0.74360001,  0.66490000,  0.57290000,  0.47200000, &
         0.36940002,  0.27139997,  0.18180001,  0.10479999,  0.04699999, &
         0.01359999/
      data ((h12(ip,iw),iw=1,31), ip=15,15)/ &
        -0.1378e-06, -0.2542e-06, -0.4461e-06, -0.7333e-06, -0.1125e-05, &
        -0.1630e-05, -0.2281e-05, -0.3159e-05, -0.4410e-05, -0.6246e-05, &
        -0.8933e-05, -0.1280e-04, -0.1826e-04, -0.2589e-04, -0.3639e-04, &
        -0.5059e-04, -0.6970e-04, -0.9552e-04, -0.1307e-03, -0.1784e-03, &
        -0.2422e-03, -0.3237e-03, -0.4203e-03, -0.5227e-03, -0.6184e-03, &
        -0.6953e-03, -0.7395e-03, -0.7315e-03, -0.6487e-03, -0.4799e-03, &
        -0.2625e-03/
      data ((h13(ip,iw),iw=1,31), ip=15,15)/ &
         0.5891e-09,  0.1074e-08,  0.1885e-08,  0.3167e-08,  0.5051e-08, &
         0.7631e-08,  0.1092e-07,  0.1500e-07,  0.2032e-07,  0.2769e-07, &
         0.3810e-07,  0.5279e-07,  0.7361e-07,  0.1032e-06,  0.1450e-06, &
         0.2026e-06,  0.2798e-06,  0.3832e-06,  0.5242e-06,  0.7159e-06, &
         0.9706e-06,  0.1299e-05,  0.1701e-05,  0.2159e-05,  0.2612e-05, &
         0.2998e-05,  0.3341e-05,  0.3661e-05,  0.3775e-05,  0.3393e-05, &
         0.2384e-05/
      data ((h11(ip,iw),iw=1,31), ip=16,16)/ &
         0.99991363,  0.99984020,  0.99971467,  0.99951237,  0.99920303, &
         0.99874902,  0.99809903,  0.99717999,  0.99588197,  0.99404502, &
         0.99144298,  0.98776001,  0.98258001,  0.97533000,  0.96524000, &
         0.95135999,  0.93241000,  0.90667999,  0.87199998,  0.82620001, &
         0.76700002,  0.69309998,  0.60510004,  0.50650001,  0.40359998, &
         0.30350000,  0.21069998,  0.12870002,  0.06370002,  0.02200001, &
         0.00389999/
      data ((h12(ip,iw),iw=1,31), ip=16,16)/ &
        -0.1383e-06, -0.2577e-06, -0.4608e-06, -0.7793e-06, -0.1237e-05, &
        -0.1850e-05, -0.2652e-05, -0.3728e-05, -0.5244e-05, -0.7451e-05, &
        -0.1067e-04, -0.1532e-04, -0.2193e-04, -0.3119e-04, -0.4395e-04, &
        -0.6126e-04, -0.8466e-04, -0.1164e-03, -0.1596e-03, -0.2177e-03, &
        -0.2933e-03, -0.3855e-03, -0.4874e-03, -0.5870e-03, -0.6718e-03, &
        -0.7290e-03, -0.7411e-03, -0.6859e-03, -0.5450e-03, -0.3353e-03, &
        -0.1363e-03/
      data ((h13(ip,iw),iw=1,31), ip=16,16)/ &
         0.6217e-09,  0.1165e-08,  0.2116e-08,  0.3685e-08,  0.6101e-08, &
         0.9523e-08,  0.1400e-07,  0.1959e-07,  0.2668e-07,  0.3629e-07, &
         0.4982e-07,  0.6876e-07,  0.9523e-07,  0.1321e-06,  0.1825e-06, &
         0.2505e-06,  0.3420e-06,  0.4677e-06,  0.6416e-06,  0.8760e-06, &
         0.1183e-05,  0.1565e-05,  0.2010e-05,  0.2472e-05,  0.2882e-05, &
         0.3229e-05,  0.3564e-05,  0.3777e-05,  0.3589e-05,  0.2786e-05, &
         0.1487e-05/
      data ((h11(ip,iw),iw=1,31), ip=17,17)/ &
         0.99991077,  0.99983180,  0.99969262,  0.99945968,  0.99909151, &
         0.99853700,  0.99773198,  0.99658400,  0.99496001,  0.99266702, &
         0.98943001,  0.98484999,  0.97842997,  0.96945000,  0.95703000, &
         0.93998998,  0.91676998,  0.88540000,  0.84350002,  0.78890002, &
         0.71990001,  0.63639998,  0.54060000,  0.43820000,  0.33639997, &
         0.24080002,  0.15460002,  0.08310002,  0.03310001,  0.00770003, &
         0.00050002/
      data ((h12(ip,iw),iw=1,31), ip=17,17)/ &
        -0.1405e-06, -0.2649e-06, -0.4829e-06, -0.8398e-06, -0.1379e-05, &
        -0.2132e-05, -0.3138e-05, -0.4487e-05, -0.6353e-05, -0.9026e-05, &
        -0.1290e-04, -0.1851e-04, -0.2650e-04, -0.3772e-04, -0.5319e-04, &
        -0.7431e-04, -0.1031e-03, -0.1422e-03, -0.1951e-03, -0.2648e-03, &
        -0.3519e-03, -0.4518e-03, -0.5537e-03, -0.6449e-03, -0.7133e-03, &
        -0.7432e-03, -0.7133e-03, -0.6018e-03, -0.4092e-03, -0.1951e-03, &
        -0.5345e-04/
      data ((h13(ip,iw),iw=1,31), ip=17,17)/ &
         0.6457e-09,  0.1235e-08,  0.2303e-08,  0.4149e-08,  0.7120e-08, &
         0.1152e-07,  0.1749e-07,  0.2508e-07,  0.3462e-07,  0.4718e-07, &
         0.6452e-07,  0.8874e-07,  0.1222e-06,  0.1675e-06,  0.2276e-06, &
         0.3076e-06,  0.4174e-06,  0.5714e-06,  0.7837e-06,  0.1067e-05, &
         0.1428e-05,  0.1859e-05,  0.2327e-05,  0.2760e-05,  0.3122e-05, &
         0.3458e-05,  0.3739e-05,  0.3715e-05,  0.3126e-05,  0.1942e-05, &
         0.6977e-06/
      data ((h11(ip,iw),iw=1,31), ip=18,18)/ &
         0.99990851,  0.99982500,  0.99967349,  0.99941093,  0.99897999, &
         0.99831200,  0.99732101,  0.99589097,  0.99386197,  0.99099803, &
         0.98695999,  0.98128998,  0.97333002,  0.96227002,  0.94700998, &
         0.92614001,  0.89779997,  0.85969996,  0.80949998,  0.74540001, &
         0.66649997,  0.57420003,  0.47310001,  0.37029999,  0.27200001, &
         0.18220001,  0.10500002,  0.04710001,  0.01359999,  0.00169998, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=18,18)/ &
        -0.1431e-06, -0.2731e-06, -0.5072e-06, -0.9057e-06, -0.1537e-05, &
        -0.2460e-05, -0.3733e-05, -0.5449e-05, -0.7786e-05, -0.1106e-04, &
        -0.1574e-04, -0.2249e-04, -0.3212e-04, -0.4564e-04, -0.6438e-04, &
        -0.9019e-04, -0.1256e-03, -0.1737e-03, -0.2378e-03, -0.3196e-03, &
        -0.4163e-03, -0.5191e-03, -0.6154e-03, -0.6931e-03, -0.7384e-03, &
        -0.7313e-03, -0.6492e-03, -0.4805e-03, -0.2629e-03, -0.8897e-04, &
        -0.1432e-04/
      data ((h13(ip,iw),iw=1,31), ip=18,18)/ &
         0.6607e-09,  0.1282e-08,  0.2441e-08,  0.4522e-08,  0.8027e-08, &
         0.1348e-07,  0.2122e-07,  0.3139e-07,  0.4435e-07,  0.6095e-07, &
         0.8319e-07,  0.1139e-06,  0.1557e-06,  0.2107e-06,  0.2819e-06, &
         0.3773e-06,  0.5107e-06,  0.6982e-06,  0.9542e-06,  0.1290e-05, &
         0.1703e-05,  0.2170e-05,  0.2628e-05,  0.3013e-05,  0.3352e-05, &
         0.3669e-05,  0.3780e-05,  0.3397e-05,  0.2386e-05,  0.1062e-05, &
         0.2216e-06/
      data ((h11(ip,iw),iw=1,31), ip=19,19)/ &
         0.99990678,  0.99981970,  0.99965781,  0.99936831,  0.99887598, &
         0.99808502,  0.99687898,  0.99510998,  0.99257898,  0.98900002, &
         0.98398000,  0.97693998,  0.96711999,  0.95353001,  0.93484998, &
         0.90934002,  0.87479997,  0.82900000,  0.76960003,  0.69550002, &
         0.60720003,  0.50819999,  0.40490001,  0.30440003,  0.21130002, &
         0.12910002,  0.06389999,  0.02200001,  0.00389999,  0.00010002, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=19,19)/ &
        -0.1454e-06, -0.2805e-06, -0.5296e-06, -0.9685e-06, -0.1695e-05, &
        -0.2812e-05, -0.4412e-05, -0.6606e-05, -0.9573e-05, -0.1363e-04, &
        -0.1932e-04, -0.2743e-04, -0.3897e-04, -0.5520e-04, -0.7787e-04, &
        -0.1094e-03, -0.1529e-03, -0.2117e-03, -0.2880e-03, -0.3809e-03, &
        -0.4834e-03, -0.5836e-03, -0.6692e-03, -0.7275e-03, -0.7408e-03, &
        -0.6865e-03, -0.5459e-03, -0.3360e-03, -0.1365e-03, -0.2935e-04, &
        -0.2173e-05/
      data ((h13(ip,iw),iw=1,31), ip=19,19)/ &
         0.6693e-09,  0.1312e-08,  0.2538e-08,  0.4802e-08,  0.8778e-08, &
         0.1528e-07,  0.2501e-07,  0.3836e-07,  0.5578e-07,  0.7806e-07, &
         0.1069e-06,  0.1456e-06,  0.1970e-06,  0.2631e-06,  0.3485e-06, &
         0.4642e-06,  0.6268e-06,  0.8526e-06,  0.1157e-05,  0.1545e-05, &
         0.2002e-05,  0.2478e-05,  0.2897e-05,  0.3245e-05,  0.3578e-05, &
         0.3789e-05,  0.3598e-05,  0.2792e-05,  0.1489e-05,  0.4160e-06, &
         0.3843e-07/
      data ((h11(ip,iw),iw=1,31), ip=20,20)/ &
         0.99990559,  0.99981570,  0.99964547,  0.99933308,  0.99878299, &
         0.99786699,  0.99642301,  0.99425799,  0.99111998,  0.98667002, &
         0.98041999,  0.97170001,  0.95960999,  0.94295001,  0.92012000, &
         0.88900000,  0.84740001,  0.79280001,  0.72360003,  0.63960004, &
         0.54330003,  0.44029999,  0.33800000,  0.24190003,  0.15530002, &
         0.08350003,  0.03320003,  0.00770003,  0.00050002,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=20,20)/ &
        -0.1472e-06, -0.2866e-06, -0.5485e-06, -0.1024e-05, -0.1842e-05, &
        -0.3160e-05, -0.5136e-05, -0.7922e-05, -0.1171e-04, -0.1682e-04, &
        -0.2381e-04, -0.3355e-04, -0.4729e-04, -0.6673e-04, -0.9417e-04, &
        -0.1327e-03, -0.1858e-03, -0.2564e-03, -0.3449e-03, -0.4463e-03, &
        -0.5495e-03, -0.6420e-03, -0.7116e-03, -0.7427e-03, -0.7139e-03, &
        -0.6031e-03, -0.4104e-03, -0.1957e-03, -0.5358e-04, -0.6176e-05, &
        -0.1347e-06/
      data ((h13(ip,iw),iw=1,31), ip=20,20)/ &
         0.6750e-09,  0.1332e-08,  0.2602e-08,  0.5003e-08,  0.9367e-08, &
         0.1684e-07,  0.2863e-07,  0.4566e-07,  0.6865e-07,  0.9861e-07, &
         0.1368e-06,  0.1856e-06,  0.2479e-06,  0.3274e-06,  0.4315e-06, &
         0.5739e-06,  0.7710e-06,  0.1040e-05,  0.1394e-05,  0.1829e-05, &
         0.2309e-05,  0.2759e-05,  0.3131e-05,  0.3472e-05,  0.3755e-05, &
         0.3730e-05,  0.3138e-05,  0.1948e-05,  0.6994e-06,  0.1022e-06, &
         0.2459e-08/
      data ((h11(ip,iw),iw=1,31), ip=21,21)/ &
         0.99990469,  0.99981278,  0.99963617,  0.99930513,  0.99870503, &
         0.99766999,  0.99597800,  0.99336702,  0.98951000,  0.98399001, &
         0.97622001,  0.96543998,  0.95059001,  0.93019998,  0.90235001, &
         0.86470002,  0.81480002,  0.75059998,  0.67129999,  0.57840002, &
         0.47659999,  0.37279999,  0.27389997,  0.18339998,  0.10570002, &
         0.04740000,  0.01370001,  0.00169998,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=21,21)/ &
        -0.1487e-06, -0.2912e-06, -0.5636e-06, -0.1069e-05, -0.1969e-05, &
        -0.3483e-05, -0.5858e-05, -0.9334e-05, -0.1416e-04, -0.2067e-04, &
        -0.2936e-04, -0.4113e-04, -0.5750e-04, -0.8072e-04, -0.1139e-03, &
        -0.1606e-03, -0.2246e-03, -0.3076e-03, -0.4067e-03, -0.5121e-03, &
        -0.6110e-03, -0.6909e-03, -0.7378e-03, -0.7321e-03, -0.6509e-03, &
        -0.4825e-03, -0.2641e-03, -0.8936e-04, -0.1436e-04, -0.5966e-06, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=21,21)/ &
         0.6777e-09,  0.1344e-08,  0.2643e-08,  0.5138e-08,  0.9798e-08, &
         0.1809e-07,  0.3185e-07,  0.5285e-07,  0.8249e-07,  0.1222e-06, &
         0.1730e-06,  0.2351e-06,  0.3111e-06,  0.4078e-06,  0.5366e-06, &
         0.7117e-06,  0.9495e-06,  0.1266e-05,  0.1667e-05,  0.2132e-05, &
         0.2600e-05,  0.3001e-05,  0.3354e-05,  0.3679e-05,  0.3796e-05, &
         0.3414e-05,  0.2399e-05,  0.1067e-05,  0.2222e-06,  0.1075e-07, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=22,22)/ &
         0.99990410,  0.99981070,  0.99962938,  0.99928379,  0.99864298, &
         0.99750000,  0.99556601,  0.99247700,  0.98780000,  0.98100001, &
         0.97140002,  0.95810002,  0.93984997,  0.91491997,  0.88110000, &
         0.83570004,  0.77670002,  0.70239997,  0.61350000,  0.51349998, &
         0.40910000,  0.30750000,  0.21340001,  0.13040000,  0.06449997, &
         0.02219999,  0.00389999,  0.00010002,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=22,22)/ &
        -0.1496e-06, -0.2947e-06, -0.5749e-06, -0.1105e-05, -0.2074e-05, &
        -0.3763e-05, -0.6531e-05, -0.1076e-04, -0.1682e-04, -0.2509e-04, &
        -0.3605e-04, -0.5049e-04, -0.7012e-04, -0.9787e-04, -0.1378e-03, &
        -0.1939e-03, -0.2695e-03, -0.3641e-03, -0.4703e-03, -0.5750e-03, &
        -0.6648e-03, -0.7264e-03, -0.7419e-03, -0.6889e-03, -0.5488e-03, &
        -0.3382e-03, -0.1375e-03, -0.2951e-04, -0.2174e-05,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=22,22)/ &
         0.6798e-09,  0.1350e-08,  0.2667e-08,  0.5226e-08,  0.1010e-07, &
         0.1903e-07,  0.3455e-07,  0.5951e-07,  0.9658e-07,  0.1479e-06, &
         0.2146e-06,  0.2951e-06,  0.3903e-06,  0.5101e-06,  0.6693e-06, &
         0.8830e-06,  0.1168e-05,  0.1532e-05,  0.1968e-05,  0.2435e-05, &
         0.2859e-05,  0.3222e-05,  0.3572e-05,  0.3797e-05,  0.3615e-05, &
         0.2811e-05,  0.1500e-05,  0.4185e-06,  0.3850e-07,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=23,23)/ &
         0.99990374,  0.99980932,  0.99962449,  0.99926788,  0.99859399, &
         0.99736100,  0.99520397,  0.99163198,  0.98606002,  0.97779000, &
         0.96600002,  0.94963002,  0.92720997,  0.89670002,  0.85580003, &
         0.80170000,  0.73269999,  0.64840001,  0.55110002,  0.44669998, &
         0.34280002,  0.24529999,  0.15750003,  0.08469999,  0.03369999, &
         0.00779998,  0.00050002,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=23,23)/ &
        -0.1503e-06, -0.2971e-06, -0.5832e-06, -0.1131e-05, -0.2154e-05, &
        -0.3992e-05, -0.7122e-05, -0.1211e-04, -0.1954e-04, -0.2995e-04, &
        -0.4380e-04, -0.6183e-04, -0.8577e-04, -0.1191e-03, -0.1668e-03, &
        -0.2333e-03, -0.3203e-03, -0.4237e-03, -0.5324e-03, -0.6318e-03, &
        -0.7075e-03, -0.7429e-03, -0.7168e-03, -0.6071e-03, -0.4139e-03, &
        -0.1976e-03, -0.5410e-04, -0.6215e-05, -0.1343e-06,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=23,23)/ &
         0.6809e-09,  0.1356e-08,  0.2683e-08,  0.5287e-08,  0.1030e-07, &
         0.1971e-07,  0.3665e-07,  0.6528e-07,  0.1100e-06,  0.1744e-06, &
         0.2599e-06,  0.3650e-06,  0.4887e-06,  0.6398e-06,  0.8358e-06, &
         0.1095e-05,  0.1429e-05,  0.1836e-05,  0.2286e-05,  0.2716e-05, &
         0.3088e-05,  0.3444e-05,  0.3748e-05,  0.3740e-05,  0.3157e-05, &
         0.1966e-05,  0.7064e-06,  0.1030e-06,  0.2456e-08,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=24,24)/ &
         0.99990344,  0.99980831,  0.99962109,  0.99925637,  0.99855798, &
         0.99725199,  0.99489999,  0.99087203,  0.98436999,  0.97447002, &
         0.96012998,  0.94006002,  0.91254002,  0.87540001,  0.82609999, &
         0.76240003,  0.68299997,  0.58930004,  0.48589998,  0.38020003, &
         0.27920002,  0.18699998,  0.10769999,  0.04830003,  0.01400000, &
         0.00169998,  0.00000000,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=24,24)/ &
        -0.1508e-06, -0.2989e-06, -0.5892e-06, -0.1151e-05, -0.2216e-05, &
        -0.4175e-05, -0.7619e-05, -0.1333e-04, -0.2217e-04, -0.3497e-04, &
        -0.5238e-04, -0.7513e-04, -0.1049e-03, -0.1455e-03, -0.2021e-03, &
        -0.2790e-03, -0.3757e-03, -0.4839e-03, -0.5902e-03, -0.6794e-03, &
        -0.7344e-03, -0.7341e-03, -0.6557e-03, -0.4874e-03, -0.2674e-03, &
        -0.9059e-04, -0.1455e-04, -0.5986e-06,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=24,24)/ &
         0.6812e-09,  0.1356e-08,  0.2693e-08,  0.5328e-08,  0.1045e-07, &
         0.2021e-07,  0.3826e-07,  0.6994e-07,  0.1218e-06,  0.1997e-06, &
         0.3069e-06,  0.4428e-06,  0.6064e-06,  0.8015e-06,  0.1043e-05, &
         0.1351e-05,  0.1733e-05,  0.2168e-05,  0.2598e-05,  0.2968e-05, &
         0.3316e-05,  0.3662e-05,  0.3801e-05,  0.3433e-05,  0.2422e-05, &
         0.1081e-05,  0.2256e-06,  0.1082e-07,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=25,25)/ &
         0.99990326,  0.99980772,  0.99961871,  0.99924821,  0.99853098, &
         0.99716800,  0.99465698,  0.99022102,  0.98281002,  0.97118002, &
         0.95393997,  0.92948997,  0.89579999,  0.85070002,  0.79189998, &
         0.71759999,  0.62800002,  0.52639997,  0.41970003,  0.31559998, &
         0.21899998,  0.13370001,  0.06610000,  0.02280003,  0.00400001, &
         0.00010002,  0.00000000,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=25,25)/ &
        -0.1511e-06, -0.3001e-06, -0.5934e-06, -0.1166e-05, -0.2263e-05, &
        -0.4319e-05, -0.8028e-05, -0.1438e-04, -0.2460e-04, -0.3991e-04, &
        -0.6138e-04, -0.9005e-04, -0.1278e-03, -0.1778e-03, -0.2447e-03, &
        -0.3313e-03, -0.4342e-03, -0.5424e-03, -0.6416e-03, -0.7146e-03, &
        -0.7399e-03, -0.6932e-03, -0.5551e-03, -0.3432e-03, -0.1398e-03, &
        -0.3010e-04, -0.2229e-05,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=25,25)/ &
         0.6815e-09,  0.1358e-08,  0.2698e-08,  0.5355e-08,  0.1054e-07, &
         0.2056e-07,  0.3942e-07,  0.7349e-07,  0.1315e-06,  0.2226e-06, &
         0.3537e-06,  0.5266e-06,  0.7407e-06,  0.9958e-06,  0.1296e-05, &
         0.1657e-05,  0.2077e-05,  0.2512e-05,  0.2893e-05,  0.3216e-05, &
         0.3562e-05,  0.3811e-05,  0.3644e-05,  0.2841e-05,  0.1524e-05, &
         0.4276e-06,  0.3960e-07,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h11(ip,iw),iw=1,31), ip=26,26)/ &
         0.99990320,  0.99980718,  0.99961710,  0.99924242,  0.99851102, &
         0.99710602,  0.99446702,  0.98969001,  0.98144001,  0.96805000, &
         0.94762999,  0.91812998,  0.87730002,  0.82290000,  0.75319999, &
         0.66789997,  0.56879997,  0.46160001,  0.35450000,  0.25370002, &
         0.16280001,  0.08740002,  0.03479999,  0.00809997,  0.00059998, &
         0.00000000,  0.00000000,  0.00000000,  0.00000000,  0.00000000, &
         0.00000000/
      data ((h12(ip,iw),iw=1,31), ip=26,26)/ &
        -0.1513e-06, -0.3009e-06, -0.5966e-06, -0.1176e-05, -0.2299e-05, &
        -0.4430e-05, -0.8352e-05, -0.1526e-04, -0.2674e-04, -0.4454e-04, &
        -0.7042e-04, -0.1062e-03, -0.1540e-03, -0.2163e-03, -0.2951e-03, &
        -0.3899e-03, -0.4948e-03, -0.5983e-03, -0.6846e-03, -0.7332e-03, &
        -0.7182e-03, -0.6142e-03, -0.4209e-03, -0.2014e-03, -0.5530e-04, &
        -0.6418e-05, -0.1439e-06,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h13(ip,iw),iw=1,31), ip=26,26)/ &
         0.6817e-09,  0.1359e-08,  0.2702e-08,  0.5374e-08,  0.1061e-07, &
         0.2079e-07,  0.4022e-07,  0.7610e-07,  0.1392e-06,  0.2428e-06, &
         0.3992e-06,  0.6149e-06,  0.8893e-06,  0.1220e-05,  0.1599e-05, &
         0.2015e-05,  0.2453e-05,  0.2853e-05,  0.3173e-05,  0.3488e-05, &
         0.3792e-05,  0.3800e-05,  0.3210e-05,  0.2002e-05,  0.7234e-06, &
         0.1068e-06,  0.2646e-08,  0.0000e+00,  0.0000e+00,  0.0000e+00, &
         0.0000e+00/
      data ((h21(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991685,  0.99986511,  0.99979371,  0.99970162,  0.99958909, &
         0.99945778,  0.99931037,  0.99914628,  0.99895900,  0.99873799, &
         0.99846601,  0.99813002,  0.99771398,  0.99719697,  0.99655598, &
         0.99575800,  0.99475598,  0.99348903,  0.99186200,  0.98973000, &
         0.98688000,  0.98303002,  0.97777998,  0.97059000,  0.96077001, &
         0.94742000/
      data ((h22(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.5622e-07, -0.1071e-06, -0.1983e-06, -0.3533e-06, -0.5991e-06, &
        -0.9592e-06, -0.1444e-05, -0.2049e-05, -0.2764e-05, -0.3577e-05, &
        -0.4469e-05, -0.5467e-05, -0.6654e-05, -0.8137e-05, -0.1002e-04, &
        -0.1237e-04, -0.1528e-04, -0.1884e-04, -0.2310e-04, -0.2809e-04, &
        -0.3396e-04, -0.4098e-04, -0.4960e-04, -0.6058e-04, -0.7506e-04, &
        -0.9451e-04, -0.1207e-03, -0.1558e-03, -0.2026e-03, -0.2648e-03, &
        -0.3468e-03/
      data ((h23(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.2195e-09, -0.4031e-09, -0.7043e-09, -0.1153e-08, -0.1737e-08, &
        -0.2395e-08, -0.3020e-08, -0.3549e-08, -0.4034e-08, -0.4421e-08, &
        -0.4736e-08, -0.5681e-08, -0.8289e-08, -0.1287e-07, -0.1873e-07, &
        -0.2523e-07, -0.3223e-07, -0.3902e-07, -0.4409e-07, -0.4699e-07, &
        -0.4782e-07, -0.4705e-07, -0.4657e-07, -0.4885e-07, -0.5550e-07, &
        -0.6619e-07, -0.7656e-07, -0.8027e-07, -0.7261e-07, -0.4983e-07, &
        -0.1101e-07/
      data ((h21(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991679,  0.99986511,  0.99979353,  0.99970138,  0.99958861, &
         0.99945688,  0.99930882,  0.99914342,  0.99895400,  0.99872798, &
         0.99844801,  0.99809802,  0.99765801,  0.99710101,  0.99639499, &
         0.99549901,  0.99435198,  0.99287099,  0.99093699,  0.98837000, &
         0.98491001,  0.98019999,  0.97373998,  0.96490002,  0.95283002, &
         0.93649000/
      data ((h22(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.5622e-07, -0.1071e-06, -0.1983e-06, -0.3534e-06, -0.5992e-06, &
        -0.9594e-06, -0.1445e-05, -0.2050e-05, -0.2766e-05, -0.3580e-05, &
        -0.4476e-05, -0.5479e-05, -0.6677e-05, -0.8179e-05, -0.1009e-04, &
        -0.1251e-04, -0.1553e-04, -0.1928e-04, -0.2384e-04, -0.2930e-04, &
        -0.3588e-04, -0.4393e-04, -0.5403e-04, -0.6714e-04, -0.8458e-04, &
        -0.1082e-03, -0.1400e-03, -0.1829e-03, -0.2401e-03, -0.3157e-03, &
        -0.4147e-03/
      data ((h23(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.2195e-09, -0.4032e-09, -0.7046e-09, -0.1153e-08, -0.1738e-08, &
        -0.2395e-08, -0.3021e-08, -0.3550e-08, -0.4035e-08, -0.4423e-08, &
        -0.4740e-08, -0.5692e-08, -0.8314e-08, -0.1292e-07, -0.1882e-07, &
        -0.2536e-07, -0.3242e-07, -0.3927e-07, -0.4449e-07, -0.4767e-07, &
        -0.4889e-07, -0.4857e-07, -0.4860e-07, -0.5132e-07, -0.5847e-07, &
        -0.6968e-07, -0.8037e-07, -0.8400e-07, -0.7521e-07, -0.4830e-07, &
        -0.7562e-09/
      data ((h21(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991679,  0.99986500,  0.99979341,  0.99970102,  0.99958777, &
         0.99945557,  0.99930632,  0.99913889,  0.99894601,  0.99871302, &
         0.99842101,  0.99805099,  0.99757600,  0.99696302,  0.99617100, &
         0.99514598,  0.99381000,  0.99205798,  0.98974001,  0.98662001, &
         0.98238999,  0.97659999,  0.96866000,  0.95776999,  0.94296998, &
         0.92306000/
      data ((h22(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.5622e-07, -0.1071e-06, -0.1983e-06, -0.3535e-06, -0.5994e-06, &
        -0.9599e-06, -0.1446e-05, -0.2052e-05, -0.2769e-05, -0.3586e-05, &
        -0.4487e-05, -0.5499e-05, -0.6712e-05, -0.8244e-05, -0.1021e-04, &
        -0.1272e-04, -0.1591e-04, -0.1992e-04, -0.2489e-04, -0.3097e-04, &
        -0.3845e-04, -0.4782e-04, -0.5982e-04, -0.7558e-04, -0.9674e-04, &
        -0.1254e-03, -0.1644e-03, -0.2167e-03, -0.2863e-03, -0.3777e-03, &
        -0.4959e-03/
      data ((h23(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.2196e-09, -0.4033e-09, -0.7048e-09, -0.1154e-08, -0.1739e-08, &
        -0.2396e-08, -0.3022e-08, -0.3551e-08, -0.4036e-08, -0.4425e-08, &
        -0.4746e-08, -0.5710e-08, -0.8354e-08, -0.1300e-07, -0.1894e-07, &
        -0.2554e-07, -0.3265e-07, -0.3958e-07, -0.4502e-07, -0.4859e-07, &
        -0.5030e-07, -0.5053e-07, -0.5104e-07, -0.5427e-07, -0.6204e-07, &
        -0.7388e-07, -0.8477e-07, -0.8760e-07, -0.7545e-07, -0.4099e-07, &
         0.2046e-07/
      data ((h21(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995142, &
         0.99991673,  0.99986482,  0.99979299,  0.99970031,  0.99958658, &
         0.99945343,  0.99930239,  0.99913180,  0.99893302,  0.99869001, &
         0.99838102,  0.99798000,  0.99745703,  0.99676800,  0.99586397, &
         0.99467200,  0.99309403,  0.99099600,  0.98817998,  0.98438001, &
         0.97918999,  0.97206002,  0.96227002,  0.94888997,  0.93080997, &
         0.90671003/
      data ((h22(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.5623e-07, -0.1071e-06, -0.1984e-06, -0.3536e-06, -0.5997e-06, &
        -0.9606e-06, -0.1447e-05, -0.2055e-05, -0.2775e-05, -0.3596e-05, &
        -0.4504e-05, -0.5529e-05, -0.6768e-05, -0.8345e-05, -0.1039e-04, &
        -0.1304e-04, -0.1645e-04, -0.2082e-04, -0.2633e-04, -0.3322e-04, &
        -0.4187e-04, -0.5292e-04, -0.6730e-04, -0.8640e-04, -0.1122e-03, &
        -0.1472e-03, -0.1948e-03, -0.2585e-03, -0.3428e-03, -0.4523e-03, &
        -0.5915e-03/
      data ((h23(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.2196e-09, -0.4034e-09, -0.7050e-09, -0.1154e-08, -0.1740e-08, &
        -0.2398e-08, -0.3024e-08, -0.3552e-08, -0.4037e-08, -0.4428e-08, &
        -0.4756e-08, -0.5741e-08, -0.8418e-08, -0.1310e-07, -0.1910e-07, &
        -0.2575e-07, -0.3293e-07, -0.3998e-07, -0.4572e-07, -0.4980e-07, &
        -0.5211e-07, -0.5287e-07, -0.5390e-07, -0.5782e-07, -0.6650e-07, &
        -0.7892e-07, -0.8940e-07, -0.8980e-07, -0.7119e-07, -0.2452e-07, &
         0.5823e-07/
      data ((h21(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997294,  0.99995136, &
         0.99991661,  0.99986458,  0.99979252,  0.99969929,  0.99958479, &
         0.99945003,  0.99929619,  0.99912071,  0.99891400,  0.99865502, &
         0.99831998,  0.99787700,  0.99728799,  0.99650002,  0.99544799, &
         0.99404198,  0.99215603,  0.98961997,  0.98619002,  0.98153001, &
         0.97513002,  0.96634001,  0.95428002,  0.93791002,  0.91593999, &
         0.88700002/
      data ((h22(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.5623e-07, -0.1071e-06, -0.1985e-06, -0.3538e-06, -0.6002e-06, &
        -0.9618e-06, -0.1450e-05, -0.2059e-05, -0.2783e-05, -0.3611e-05, &
        -0.4531e-05, -0.5577e-05, -0.6855e-05, -0.8499e-05, -0.1066e-04, &
        -0.1351e-04, -0.1723e-04, -0.2207e-04, -0.2829e-04, -0.3621e-04, &
        -0.4636e-04, -0.5954e-04, -0.7690e-04, -0.1002e-03, -0.1317e-03, &
        -0.1746e-03, -0.2326e-03, -0.3099e-03, -0.4111e-03, -0.5407e-03, &
        -0.7020e-03/
      data ((h23(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.2197e-09, -0.4037e-09, -0.7054e-09, -0.1155e-08, -0.1741e-08, &
        -0.2401e-08, -0.3027e-08, -0.3553e-08, -0.4039e-08, -0.4431e-08, &
        -0.4775e-08, -0.5784e-08, -0.8506e-08, -0.1326e-07, -0.1931e-07, &
        -0.2600e-07, -0.3324e-07, -0.4048e-07, -0.4666e-07, -0.5137e-07, &
        -0.5428e-07, -0.5558e-07, -0.5730e-07, -0.6228e-07, -0.7197e-07, &
        -0.8455e-07, -0.9347e-07, -0.8867e-07, -0.5945e-07,  0.5512e-08, &
         0.1209e-06/
      data ((h21(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997288,  0.99995130, &
         0.99991649,  0.99986428,  0.99979180,  0.99969780,  0.99958187, &
         0.99944460,  0.99928659,  0.99910372,  0.99888301,  0.99860299, &
         0.99822998,  0.99773002,  0.99705303,  0.99613500,  0.99489301, &
         0.99321300,  0.99093801,  0.98785001,  0.98365998,  0.97790998, &
         0.97000998,  0.95916998,  0.94437003,  0.92440999,  0.89789999, &
         0.86360002/
      data ((h22(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.5624e-07, -0.1072e-06, -0.1986e-06, -0.3541e-06, -0.6010e-06, &
        -0.9636e-06, -0.1453e-05, -0.2067e-05, -0.2796e-05, -0.3634e-05, &
        -0.4572e-05, -0.5652e-05, -0.6987e-05, -0.8733e-05, -0.1107e-04, &
        -0.1418e-04, -0.1832e-04, -0.2378e-04, -0.3092e-04, -0.4017e-04, &
        -0.5221e-04, -0.6806e-04, -0.8916e-04, -0.1176e-03, -0.1562e-03, &
        -0.2087e-03, -0.2793e-03, -0.3724e-03, -0.4928e-03, -0.6440e-03, &
        -0.8270e-03/
      data ((h23(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.2198e-09, -0.4040e-09, -0.7061e-09, -0.1156e-08, -0.1744e-08, &
        -0.2405e-08, -0.3032e-08, -0.3556e-08, -0.4040e-08, -0.4444e-08, &
        -0.4800e-08, -0.5848e-08, -0.8640e-08, -0.1346e-07, -0.1957e-07, &
        -0.2627e-07, -0.3357e-07, -0.4114e-07, -0.4793e-07, -0.5330e-07, &
        -0.5676e-07, -0.5873e-07, -0.6152e-07, -0.6783e-07, -0.7834e-07, &
        -0.9023e-07, -0.9530e-07, -0.8162e-07, -0.3634e-07,  0.5638e-07, &
         0.2189e-06/
      data ((h21(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.99999607,  0.99999237,  0.99998546,  0.99997288,  0.99995124, &
         0.99991626,  0.99986368,  0.99979049,  0.99969530,  0.99957728, &
         0.99943632,  0.99927181,  0.99907762,  0.99883801,  0.99852502, &
         0.99810201,  0.99752498,  0.99673301,  0.99564600,  0.99416101, &
         0.99213398,  0.98936999,  0.98559999,  0.98043001,  0.97333997, &
         0.96359003,  0.95025003,  0.93216002,  0.90798998,  0.87639999, &
         0.83609998/
      data ((h22(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.5626e-07, -0.1072e-06, -0.1987e-06, -0.3545e-06, -0.6022e-06, &
        -0.9665e-06, -0.1460e-05, -0.2078e-05, -0.2817e-05, -0.3671e-05, &
        -0.4637e-05, -0.5767e-05, -0.7188e-05, -0.9080e-05, -0.1165e-04, &
        -0.1513e-04, -0.1981e-04, -0.2609e-04, -0.3441e-04, -0.4534e-04, &
        -0.5978e-04, -0.7897e-04, -0.1047e-03, -0.1396e-03, -0.1870e-03, &
        -0.2510e-03, -0.3363e-03, -0.4475e-03, -0.5888e-03, -0.7621e-03, &
        -0.9647e-03/
      data ((h23(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.2200e-09, -0.4045e-09, -0.7071e-09, -0.1159e-08, -0.1748e-08, &
        -0.2411e-08, -0.3040e-08, -0.3561e-08, -0.4046e-08, -0.4455e-08, &
        -0.4839e-08, -0.5941e-08, -0.8815e-08, -0.1371e-07, -0.1983e-07, &
        -0.2652e-07, -0.3400e-07, -0.4207e-07, -0.4955e-07, -0.5554e-07, &
        -0.5966e-07, -0.6261e-07, -0.6688e-07, -0.7454e-07, -0.8521e-07, &
        -0.9470e-07, -0.9275e-07, -0.6525e-07,  0.3686e-08,  0.1371e-06, &
         0.3623e-06/
      data ((h21(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.99999607,  0.99999237,  0.99998540,  0.99997282,  0.99995112, &
         0.99991590,  0.99986279,  0.99978858,  0.99969149,  0.99957019, &
         0.99942350,  0.99924922,  0.99903822,  0.99877101,  0.99841398, &
         0.99792302,  0.99724299,  0.99630302,  0.99500000,  0.99320602, &
         0.99074000,  0.98736000,  0.98272002,  0.97635001,  0.96758002, &
         0.95555997,  0.93919998,  0.91722000,  0.88819999,  0.85089999, &
         0.80439997/
      data ((h22(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.5628e-07, -0.1073e-06, -0.1990e-06, -0.3553e-06, -0.6042e-06, &
        -0.9710e-06, -0.1469e-05, -0.2096e-05, -0.2849e-05, -0.3728e-05, &
        -0.4738e-05, -0.5942e-05, -0.7490e-05, -0.9586e-05, -0.1247e-04, &
        -0.1644e-04, -0.2184e-04, -0.2916e-04, -0.3898e-04, -0.5205e-04, &
        -0.6948e-04, -0.9285e-04, -0.1244e-03, -0.1672e-03, -0.2251e-03, &
        -0.3028e-03, -0.4051e-03, -0.5365e-03, -0.6998e-03, -0.8940e-03, &
        -0.1112e-02/
      data ((h23(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.2204e-09, -0.4052e-09, -0.7088e-09, -0.1162e-08, -0.1755e-08, &
        -0.2422e-08, -0.3053e-08, -0.3572e-08, -0.4052e-08, -0.4474e-08, &
        -0.4898e-08, -0.6082e-08, -0.9046e-08, -0.1400e-07, -0.2009e-07, &
        -0.2683e-07, -0.3463e-07, -0.4334e-07, -0.5153e-07, -0.5811e-07, &
        -0.6305e-07, -0.6749e-07, -0.7346e-07, -0.8208e-07, -0.9173e-07, &
        -0.9603e-07, -0.8264e-07, -0.3505e-07,  0.6878e-07,  0.2586e-06, &
         0.5530e-06/
      data ((h21(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.99999607,  0.99999237,  0.99998540,  0.99997276,  0.99995089, &
         0.99991536,  0.99986148,  0.99978572,  0.99968570,  0.99955928, &
         0.99940401,  0.99921501,  0.99897999,  0.99867398,  0.99825603, &
         0.99767601,  0.99686497,  0.99573302,  0.99415499,  0.99196899, &
         0.98895001,  0.98479998,  0.97907001,  0.97119999,  0.96038002, &
         0.94559997,  0.92565000,  0.89910001,  0.86470002,  0.82130003, &
         0.76830000/
      data ((h22(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.5630e-07, -0.1074e-06, -0.1994e-06, -0.3564e-06, -0.6072e-06, &
        -0.9779e-06, -0.1484e-05, -0.2124e-05, -0.2900e-05, -0.3817e-05, &
        -0.4891e-05, -0.6205e-05, -0.7931e-05, -0.1031e-04, -0.1362e-04, &
        -0.1821e-04, -0.2454e-04, -0.3320e-04, -0.4493e-04, -0.6068e-04, &
        -0.8186e-04, -0.1104e-03, -0.1491e-03, -0.2016e-03, -0.2722e-03, &
        -0.3658e-03, -0.4873e-03, -0.6404e-03, -0.8253e-03, -0.1037e-02, &
        -0.1267e-02/
      data ((h23(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.2207e-09, -0.4061e-09, -0.7117e-09, -0.1169e-08, -0.1767e-08, &
        -0.2439e-08, -0.3074e-08, -0.3588e-08, -0.4062e-08, -0.4510e-08, &
        -0.4983e-08, -0.6261e-08, -0.9324e-08, -0.1430e-07, -0.2036e-07, &
        -0.2725e-07, -0.3561e-07, -0.4505e-07, -0.5384e-07, -0.6111e-07, &
        -0.6731e-07, -0.7355e-07, -0.8112e-07, -0.8978e-07, -0.9616e-07, &
        -0.9157e-07, -0.6114e-07,  0.1622e-07,  0.1694e-06,  0.4277e-06, &
         0.7751e-06/
      data ((h21(ip,iw),iw=1,31), ip=10,10)/ &
         0.99999607,  0.99999237,  0.99998540,  0.99997264,  0.99995059, &
         0.99991453,  0.99985939,  0.99978119,  0.99967682,  0.99954277, &
         0.99937469,  0.99916458,  0.99889499,  0.99853599,  0.99804002, &
         0.99734300,  0.99636298,  0.99498600,  0.99305803,  0.99037802, &
         0.98667002,  0.98154002,  0.97447002,  0.96473998,  0.95141000, &
         0.93333000,  0.90916002,  0.87750000,  0.83710003,  0.78729999, &
         0.72790003/
      data ((h22(ip,iw),iw=1,31), ip=10,10)/ &
        -0.5636e-07, -0.1076e-06, -0.2000e-06, -0.3582e-06, -0.6119e-06, &
        -0.9888e-06, -0.1507e-05, -0.2168e-05, -0.2978e-05, -0.3952e-05, &
        -0.5122e-05, -0.6592e-05, -0.8565e-05, -0.1132e-04, -0.1518e-04, &
        -0.2060e-04, -0.2811e-04, -0.3848e-04, -0.5261e-04, -0.7173e-04, &
        -0.9758e-04, -0.1326e-03, -0.1801e-03, -0.2442e-03, -0.3296e-03, &
        -0.4415e-03, -0.5840e-03, -0.7591e-03, -0.9636e-03, -0.1189e-02, &
        -0.1427e-02/
      data ((h23(ip,iw),iw=1,31), ip=10,10)/ &
        -0.2214e-09, -0.4080e-09, -0.7156e-09, -0.1178e-08, -0.1784e-08, &
        -0.2466e-08, -0.3105e-08, -0.3617e-08, -0.4087e-08, -0.4563e-08, &
        -0.5110e-08, -0.6492e-08, -0.9643e-08, -0.1461e-07, -0.2069e-07, &
        -0.2796e-07, -0.3702e-07, -0.4717e-07, -0.5662e-07, -0.6484e-07, &
        -0.7271e-07, -0.8079e-07, -0.8928e-07, -0.9634e-07, -0.9625e-07, &
        -0.7776e-07, -0.2242e-07,  0.9745e-07,  0.3152e-06,  0.6388e-06, &
         0.9992e-06/
      data ((h21(ip,iw),iw=1,31), ip=11,11)/ &
         0.99999607,  0.99999237,  0.99998534,  0.99997252,  0.99995011, &
         0.99991328,  0.99985629,  0.99977452,  0.99966347,  0.99951839, &
         0.99933177,  0.99909180,  0.99877602,  0.99834698,  0.99774700, &
         0.99690098,  0.99570400,  0.99401599,  0.99164802,  0.98834997, &
         0.98377001,  0.97742999,  0.96868002,  0.95666999,  0.94032001, &
         0.91833997,  0.88929999,  0.85189998,  0.80530000,  0.74909997, &
         0.68299997/
      data ((h22(ip,iw),iw=1,31), ip=11,11)/ &
        -0.5645e-07, -0.1079e-06, -0.2009e-06, -0.3610e-06, -0.6190e-06, &
        -0.1005e-05, -0.1541e-05, -0.2235e-05, -0.3096e-05, -0.4155e-05, &
        -0.5463e-05, -0.7150e-05, -0.9453e-05, -0.1269e-04, -0.1728e-04, &
        -0.2375e-04, -0.3279e-04, -0.4531e-04, -0.6246e-04, -0.8579e-04, &
        -0.1175e-03, -0.1604e-03, -0.2186e-03, -0.2964e-03, -0.3990e-03, &
        -0.5311e-03, -0.6957e-03, -0.8916e-03, -0.1112e-02, -0.1346e-02, &
        -0.1590e-02/
      data ((h23(ip,iw),iw=1,31), ip=11,11)/ &
        -0.2225e-09, -0.4104e-09, -0.7217e-09, -0.1192e-08, -0.1811e-08, &
        -0.2509e-08, -0.3155e-08, -0.3668e-08, -0.4138e-08, -0.4650e-08, &
        -0.5296e-08, -0.6785e-08, -0.9991e-08, -0.1494e-07, -0.2122e-07, &
        -0.2911e-07, -0.3895e-07, -0.4979e-07, -0.6002e-07, -0.6964e-07, &
        -0.7935e-07, -0.8887e-07, -0.9699e-07, -0.9967e-07, -0.8883e-07, &
        -0.4988e-07,  0.4156e-07,  0.2197e-06,  0.5081e-06,  0.8667e-06, &
         0.1212e-05/
      data ((h21(ip,iw),iw=1,31), ip=12,12)/ &
         0.99999607,  0.99999237,  0.99998528,  0.99997234,  0.99994951, &
         0.99991143,  0.99985188,  0.99976480,  0.99964428,  0.99948311, &
         0.99927050,  0.99899000,  0.99861199,  0.99809098,  0.99735999, &
         0.99632198,  0.99484903,  0.99276900,  0.98984998,  0.98576999, &
         0.98009998,  0.97224998,  0.96144003,  0.94667000,  0.92672002, &
         0.90020001,  0.86570001,  0.82220000,  0.76919997,  0.70640004, &
         0.63330001/
      data ((h22(ip,iw),iw=1,31), ip=12,12)/ &
        -0.5658e-07, -0.1083e-06, -0.2023e-06, -0.3650e-06, -0.6295e-06, &
        -0.1030e-05, -0.1593e-05, -0.2334e-05, -0.3273e-05, -0.4455e-05, &
        -0.5955e-05, -0.7935e-05, -0.1067e-04, -0.1455e-04, -0.2007e-04, &
        -0.2788e-04, -0.3885e-04, -0.5409e-04, -0.7503e-04, -0.1036e-03, &
        -0.1425e-03, -0.1951e-03, -0.2660e-03, -0.3598e-03, -0.4817e-03, &
        -0.6355e-03, -0.8218e-03, -0.1035e-02, -0.1267e-02, -0.1508e-02, &
        -0.1755e-02/
      data ((h23(ip,iw),iw=1,31), ip=12,12)/ &
        -0.2241e-09, -0.4142e-09, -0.7312e-09, -0.1214e-08, -0.1854e-08, &
        -0.2578e-08, -0.3250e-08, -0.3765e-08, -0.4238e-08, -0.4809e-08, &
        -0.5553e-08, -0.7132e-08, -0.1035e-07, -0.1538e-07, -0.2211e-07, &
        -0.3079e-07, -0.4142e-07, -0.5303e-07, -0.6437e-07, -0.7566e-07, &
        -0.8703e-07, -0.9700e-07, -0.1025e-06, -0.9718e-07, -0.6973e-07, &
        -0.5265e-09,  0.1413e-06,  0.3895e-06,  0.7321e-06,  0.1085e-05, &
         0.1449e-05/
      data ((h21(ip,iw),iw=1,31), ip=13,13)/ &
         0.99999607,  0.99999231,  0.99998522,  0.99997205,  0.99994856, &
         0.99990892,  0.99984580,  0.99975121,  0.99961728,  0.99943388, &
         0.99918568,  0.99884999,  0.99839199,  0.99775398,  0.99685299, &
         0.99557197,  0.99375200,  0.99117702,  0.98755997,  0.98250002, &
         0.97548002,  0.96575999,  0.95244002,  0.93436003,  0.91018999, &
         0.87849998,  0.83810002,  0.78820002,  0.72870004,  0.65910000, &
         0.57850003/
      data ((h22(ip,iw),iw=1,31), ip=13,13)/ &
        -0.5677e-07, -0.1090e-06, -0.2043e-06, -0.3709e-06, -0.6448e-06, &
        -0.1066e-05, -0.1669e-05, -0.2480e-05, -0.3532e-05, -0.4887e-05, &
        -0.6650e-05, -0.9017e-05, -0.1232e-04, -0.1701e-04, -0.2372e-04, &
        -0.3325e-04, -0.4664e-04, -0.6528e-04, -0.9095e-04, -0.1260e-03, &
        -0.1737e-03, -0.2381e-03, -0.3238e-03, -0.4359e-03, -0.5789e-03, &
        -0.7549e-03, -0.9607e-03, -0.1188e-02, -0.1427e-02, -0.1674e-02, &
        -0.1914e-02/
      data ((h23(ip,iw),iw=1,31), ip=13,13)/ &
        -0.2262e-09, -0.4191e-09, -0.7441e-09, -0.1244e-08, -0.1916e-08, &
        -0.2687e-08, -0.3407e-08, -0.3947e-08, -0.4432e-08, -0.5059e-08, &
        -0.5896e-08, -0.7538e-08, -0.1079e-07, -0.1606e-07, -0.2346e-07, &
        -0.3305e-07, -0.4459e-07, -0.5720e-07, -0.6999e-07, -0.8300e-07, &
        -0.9536e-07, -0.1039e-06, -0.1036e-06, -0.8526e-07, -0.3316e-07, &
         0.7909e-07,  0.2865e-06,  0.6013e-06,  0.9580e-06,  0.1303e-05, &
         0.1792e-05/
      data ((h21(ip,iw),iw=1,31), ip=14,14)/ &
         0.99999607,  0.99999231,  0.99998510,  0.99997163,  0.99994737, &
         0.99990571,  0.99983770,  0.99973333,  0.99958128,  0.99936771, &
         0.99907219,  0.99866599,  0.99810302,  0.99731499,  0.99620003, &
         0.99461198,  0.99235398,  0.98916000,  0.98466998,  0.97839999, &
         0.96969002,  0.95769000,  0.94133997,  0.91935998,  0.89029998, &
         0.85290003,  0.80620003,  0.74989998,  0.68369997,  0.60679996, &
         0.51899999/
      data ((h22(ip,iw),iw=1,31), ip=14,14)/ &
        -0.5703e-07, -0.1098e-06, -0.2071e-06, -0.3788e-06, -0.6657e-06, &
        -0.1116e-05, -0.1776e-05, -0.2687e-05, -0.3898e-05, -0.5493e-05, &
        -0.7607e-05, -0.1048e-04, -0.1450e-04, -0.2024e-04, -0.2845e-04, &
        -0.4014e-04, -0.5658e-04, -0.7947e-04, -0.1110e-03, -0.1541e-03, &
        -0.2125e-03, -0.2907e-03, -0.3936e-03, -0.5261e-03, -0.6912e-03, &
        -0.8880e-03, -0.1109e-02, -0.1346e-02, -0.1591e-02, -0.1837e-02, &
        -0.2054e-02/
      data ((h23(ip,iw),iw=1,31), ip=14,14)/ &
        -0.2288e-09, -0.4265e-09, -0.7627e-09, -0.1289e-08, -0.2011e-08, &
        -0.2861e-08, -0.3673e-08, -0.4288e-08, -0.4812e-08, -0.5475e-08, &
        -0.6365e-08, -0.8052e-08, -0.1142e-07, -0.1711e-07, -0.2533e-07, &
        -0.3597e-07, -0.4862e-07, -0.6259e-07, -0.7708e-07, -0.9150e-07, &
        -0.1035e-06, -0.1079e-06, -0.9742e-07, -0.5928e-07,  0.2892e-07, &
         0.1998e-06,  0.4789e-06,  0.8298e-06,  0.1172e-05,  0.1583e-05, &
         0.2329e-05/
      data ((h21(ip,iw),iw=1,31), ip=15,15)/ &
         0.99999607,  0.99999225,  0.99998498,  0.99997115,  0.99994600, &
         0.99990171,  0.99982780,  0.99971092,  0.99953562,  0.99928278, &
         0.99892598,  0.99842799,  0.99773300,  0.99675500,  0.99536800, &
         0.99339402,  0.99058902,  0.98662001,  0.98104000,  0.97325999, &
         0.96249002,  0.94773000,  0.92778003,  0.90125000,  0.86680001, &
         0.82319999,  0.77010000,  0.70730001,  0.63400000,  0.54960001, &
         0.45560002/
      data ((h22(ip,iw),iw=1,31), ip=15,15)/ &
        -0.5736e-07, -0.1109e-06, -0.2106e-06, -0.3890e-06, -0.6928e-06, &
        -0.1181e-05, -0.1917e-05, -0.2965e-05, -0.4396e-05, -0.6315e-05, &
        -0.8891e-05, -0.1242e-04, -0.1736e-04, -0.2442e-04, -0.3454e-04, &
        -0.4892e-04, -0.6916e-04, -0.9735e-04, -0.1362e-03, -0.1891e-03, &
        -0.2602e-03, -0.3545e-03, -0.4768e-03, -0.6310e-03, -0.8179e-03, &
        -0.1032e-02, -0.1265e-02, -0.1508e-02, -0.1757e-02, -0.1989e-02, &
        -0.2159e-02/
      data ((h23(ip,iw),iw=1,31), ip=15,15)/ &
        -0.2321e-09, -0.4350e-09, -0.7861e-09, -0.1347e-08, -0.2144e-08, &
        -0.3120e-08, -0.4107e-08, -0.4892e-08, -0.5511e-08, -0.6164e-08, &
        -0.7054e-08, -0.8811e-08, -0.1240e-07, -0.1862e-07, -0.2773e-07, &
        -0.3957e-07, -0.5371e-07, -0.6939e-07, -0.8564e-07, -0.1006e-06, &
        -0.1100e-06, -0.1066e-06, -0.8018e-07, -0.1228e-07,  0.1263e-06, &
         0.3678e-06,  0.7022e-06,  0.1049e-05,  0.1411e-05,  0.2015e-05, &
         0.3099e-05/
      data ((h21(ip,iw),iw=1,31), ip=16,16)/ &
         0.99999791,  0.99999589,  0.99999183,  0.99998391,  0.99996853, &
         0.99993920,  0.99988472,  0.99978709,  0.99961978,  0.99934620, &
         0.99892199,  0.99830103,  0.99742401,  0.99619502,  0.99445999, &
         0.99199599,  0.98851001,  0.98360002,  0.97671002,  0.96710002, &
         0.95378000,  0.93559003,  0.91112000,  0.87900001,  0.83780003, &
         0.78680003,  0.72549999,  0.65300000,  0.56830001,  0.47140002, &
         0.36650002/
      data ((h22(ip,iw),iw=1,31), ip=16,16)/ &
        -0.3122e-07, -0.6175e-07, -0.1214e-06, -0.2361e-06, -0.4518e-06, &
        -0.8438e-06, -0.1524e-05, -0.2643e-05, -0.4380e-05, -0.6922e-05, &
        -0.1042e-04, -0.1504e-04, -0.2125e-04, -0.2987e-04, -0.4200e-04, &
        -0.5923e-04, -0.8383e-04, -0.1186e-03, -0.1670e-03, -0.2328e-03, &
        -0.3204e-03, -0.4347e-03, -0.5802e-03, -0.7595e-03, -0.9703e-03, &
        -0.1205e-02, -0.1457e-02, -0.1720e-02, -0.1980e-02, -0.2191e-02, &
        -0.2290e-02/
      data ((h23(ip,iw),iw=1,31), ip=16,16)/ &
        -0.1376e-09, -0.2699e-09, -0.5220e-09, -0.9897e-09, -0.1819e-08, &
        -0.3186e-08, -0.5224e-08, -0.7896e-08, -0.1090e-07, -0.1349e-07, &
        -0.1443e-07, -0.1374e-07, -0.1386e-07, -0.1673e-07, -0.2237e-07, &
        -0.3248e-07, -0.5050e-07, -0.7743e-07, -0.1097e-06, -0.1369e-06, &
        -0.1463e-06, -0.1268e-06, -0.6424e-07,  0.5941e-07,  0.2742e-06, &
         0.5924e-06,  0.9445e-06,  0.1286e-05,  0.1819e-05,  0.2867e-05, &
         0.4527e-05/
      data ((h21(ip,iw),iw=1,31), ip=17,17)/ &
         0.99999756,  0.99999511,  0.99999028,  0.99998081,  0.99996233, &
         0.99992681,  0.99986011,  0.99973929,  0.99953061,  0.99918979, &
         0.99866599,  0.99790198,  0.99681997,  0.99528998,  0.99312103, &
         0.99004799,  0.98571002,  0.97961998,  0.97105998,  0.95915002, &
         0.94278002,  0.92061001,  0.89120001,  0.85320002,  0.80550003, &
         0.74759996,  0.67900002,  0.59829998,  0.50520003,  0.40219998, &
         0.29600000/
      data ((h22(ip,iw),iw=1,31), ip=17,17)/ &
        -0.3547e-07, -0.7029e-07, -0.1386e-06, -0.2709e-06, -0.5218e-06, &
        -0.9840e-06, -0.1799e-05, -0.3156e-05, -0.5272e-05, -0.8357e-05, &
        -0.1260e-04, -0.1827e-04, -0.2598e-04, -0.3667e-04, -0.5169e-04, &
        -0.7312e-04, -0.1037e-03, -0.1467e-03, -0.2060e-03, -0.2857e-03, &
        -0.3907e-03, -0.5257e-03, -0.6940e-03, -0.8954e-03, -0.1124e-02, &
        -0.1371e-02, -0.1632e-02, -0.1897e-02, -0.2131e-02, -0.2275e-02, &
        -0.2265e-02/
      data ((h23(ip,iw),iw=1,31), ip=17,17)/ &
        -0.1482e-09, -0.2910e-09, -0.5667e-09, -0.1081e-08, -0.2005e-08, &
        -0.3554e-08, -0.5902e-08, -0.8925e-08, -0.1209e-07, -0.1448e-07, &
        -0.1536e-07, -0.1565e-07, -0.1763e-07, -0.2088e-07, -0.2564e-07, &
        -0.3635e-07, -0.5791e-07, -0.8907e-07, -0.1213e-06, -0.1418e-06, &
        -0.1397e-06, -0.1000e-06, -0.4427e-08,  0.1713e-06,  0.4536e-06, &
         0.8086e-06,  0.1153e-05,  0.1588e-05,  0.2437e-05,  0.3905e-05, &
         0.5874e-05/
      data ((h21(ip,iw),iw=1,31), ip=18,18)/ &
         0.99999714,  0.99999428,  0.99998862,  0.99997741,  0.99995553, &
         0.99991333,  0.99983358,  0.99968803,  0.99943441,  0.99901879, &
         0.99837899,  0.99744099,  0.99609798,  0.99418801,  0.99147803, &
         0.98764998,  0.98227000,  0.97469997,  0.96410000,  0.94941998, &
         0.92940998,  0.90263999,  0.86769998,  0.82330000,  0.76880002, &
         0.70379996,  0.62720001,  0.53810000,  0.43769997,  0.33149999, &
         0.22839999/
      data ((h22(ip,iw),iw=1,31), ip=18,18)/ &
        -0.4064e-07, -0.8066e-07, -0.1593e-06, -0.3124e-06, -0.6049e-06, &
        -0.1148e-05, -0.2118e-05, -0.3751e-05, -0.6314e-05, -0.1006e-04, &
        -0.1526e-04, -0.2232e-04, -0.3196e-04, -0.4525e-04, -0.6394e-04, &
        -0.9058e-04, -0.1284e-03, -0.1812e-03, -0.2533e-03, -0.3493e-03, &
        -0.4740e-03, -0.6315e-03, -0.8228e-03, -0.1044e-02, -0.1286e-02, &
        -0.1544e-02, -0.1810e-02, -0.2061e-02, -0.2243e-02, -0.2291e-02, &
        -0.2152e-02/
      data ((h23(ip,iw),iw=1,31), ip=18,18)/ &
        -0.1630e-09, -0.3213e-09, -0.6266e-09, -0.1201e-08, -0.2248e-08, &
        -0.4030e-08, -0.6770e-08, -0.1033e-07, -0.1392e-07, -0.1640e-07, &
        -0.1768e-07, -0.1932e-07, -0.2229e-07, -0.2508e-07, -0.2940e-07, &
        -0.4200e-07, -0.6717e-07, -0.1002e-06, -0.1286e-06, -0.1402e-06, &
        -0.1216e-06, -0.5487e-07,  0.8418e-07,  0.3246e-06,  0.6610e-06, &
         0.1013e-05,  0.1394e-05,  0.2073e-05,  0.3337e-05,  0.5175e-05, &
         0.7255e-05/
      data ((h21(ip,iw),iw=1,31), ip=19,19)/ &
         0.99999672,  0.99999344,  0.99998701,  0.99997419,  0.99994916, &
         0.99990064,  0.99980861,  0.99963921,  0.99934143,  0.99884701, &
         0.99807602,  0.99692601,  0.99525797,  0.99287403,  0.98948997, &
         0.98474002,  0.97804999,  0.96867001,  0.95559001,  0.93761998, &
         0.91336000,  0.88139999,  0.84029996,  0.78920001,  0.72770000, &
         0.65499997,  0.56999999,  0.47280002,  0.36760002,  0.26220000, &
         0.16700000/
      data ((h22(ip,iw),iw=1,31), ip=19,19)/ &
        -0.4629e-07, -0.9195e-07, -0.1819e-06, -0.3572e-06, -0.6936e-06, &
        -0.1323e-05, -0.2456e-05, -0.4385e-05, -0.7453e-05, -0.1200e-04, &
        -0.1843e-04, -0.2731e-04, -0.3943e-04, -0.5606e-04, -0.7936e-04, &
        -0.1123e-03, -0.1588e-03, -0.2231e-03, -0.3101e-03, -0.4247e-03, &
        -0.5713e-03, -0.7522e-03, -0.9651e-03, -0.1202e-02, -0.1456e-02, &
        -0.1721e-02, -0.1983e-02, -0.2196e-02, -0.2296e-02, -0.2224e-02, &
        -0.1952e-02/
      data ((h23(ip,iw),iw=1,31), ip=19,19)/ &
        -0.1827e-09, -0.3607e-09, -0.7057e-09, -0.1359e-08, -0.2552e-08, &
        -0.4615e-08, -0.7854e-08, -0.1218e-07, -0.1670e-07, -0.2008e-07, &
        -0.2241e-07, -0.2516e-07, -0.2796e-07, -0.3015e-07, -0.3506e-07, &
        -0.4958e-07, -0.7627e-07, -0.1070e-06, -0.1289e-06, -0.1286e-06, &
        -0.8843e-07,  0.1492e-07,  0.2118e-06,  0.5155e-06,  0.8665e-06, &
         0.1220e-05,  0.1765e-05,  0.2825e-05,  0.4498e-05,  0.6563e-05, &
         0.8422e-05/
      data ((h21(ip,iw),iw=1,31), ip=20,20)/ &
         0.99999636,  0.99999279,  0.99998569,  0.99997163,  0.99994397, &
         0.99989033,  0.99978799,  0.99959832,  0.99926043,  0.99868900, &
         0.99777400,  0.99637598,  0.99431503,  0.99134803,  0.98714000, &
         0.98122001,  0.97290999,  0.96131998,  0.94528997,  0.92346001, &
         0.89429998,  0.85650003,  0.80879998,  0.75080001,  0.68190002, &
         0.60100001,  0.50740004,  0.40399998,  0.29729998,  0.19730002, &
         0.11479998/
      data ((h22(ip,iw),iw=1,31), ip=20,20)/ &
        -0.5164e-07, -0.1026e-06, -0.2031e-06, -0.3994e-06, -0.7771e-06, &
        -0.1488e-05, -0.2776e-05, -0.5001e-05, -0.8610e-05, -0.1411e-04, &
        -0.2209e-04, -0.3328e-04, -0.4860e-04, -0.6954e-04, -0.9861e-04, &
        -0.1393e-03, -0.1961e-03, -0.2738e-03, -0.3778e-03, -0.5132e-03, &
        -0.6831e-03, -0.8868e-03, -0.1118e-02, -0.1368e-02, -0.1632e-02, &
        -0.1899e-02, -0.2136e-02, -0.2282e-02, -0.2273e-02, -0.2067e-02, &
        -0.1679e-02/
      data ((h23(ip,iw),iw=1,31), ip=20,20)/ &
        -0.2058e-09, -0.4066e-09, -0.7967e-09, -0.1539e-08, -0.2904e-08, &
        -0.5293e-08, -0.9116e-08, -0.1447e-07, -0.2058e-07, -0.2608e-07, &
        -0.3053e-07, -0.3418e-07, -0.3619e-07, -0.3766e-07, -0.4313e-07, &
        -0.5817e-07, -0.8299e-07, -0.1072e-06, -0.1195e-06, -0.1031e-06, &
        -0.3275e-07,  0.1215e-06,  0.3835e-06,  0.7220e-06,  0.1062e-05, &
         0.1504e-05,  0.2367e-05,  0.3854e-05,  0.5842e-05,  0.7875e-05, &
         0.9082e-05/
      data ((h21(ip,iw),iw=1,31), ip=21,21)/ &
         0.99999619,  0.99999237,  0.99998480,  0.99996990,  0.99994045, &
         0.99988312,  0.99977320,  0.99956751,  0.99919540,  0.99855101, &
         0.99748802,  0.99581498,  0.99329299,  0.98961997,  0.98439002, &
         0.97702003,  0.96671999,  0.95245999,  0.93291998,  0.90660000, &
         0.87199998,  0.82780004,  0.77329999,  0.70819998,  0.63119996, &
         0.54159999,  0.44059998,  0.33380002,  0.23000002,  0.14029998, &
         0.07340002/
      data ((h22(ip,iw),iw=1,31), ip=21,21)/ &
        -0.5584e-07, -0.1110e-06, -0.2198e-06, -0.4329e-06, -0.8444e-06, &
        -0.1623e-05, -0.3049e-05, -0.5551e-05, -0.9714e-05, -0.1627e-04, &
        -0.2609e-04, -0.4015e-04, -0.5955e-04, -0.8603e-04, -0.1223e-03, &
        -0.1724e-03, -0.2413e-03, -0.3346e-03, -0.4578e-03, -0.6155e-03, &
        -0.8087e-03, -0.1033e-02, -0.1279e-02, -0.1540e-02, -0.1811e-02, &
        -0.2065e-02, -0.2251e-02, -0.2301e-02, -0.2163e-02, -0.1828e-02, &
        -0.1365e-02/
      data ((h23(ip,iw),iw=1,31), ip=21,21)/ &
        -0.2274e-09, -0.4498e-09, -0.8814e-09, -0.1708e-08, -0.3247e-08, &
        -0.5972e-08, -0.1045e-07, -0.1707e-07, -0.2545e-07, -0.3440e-07, &
        -0.4259e-07, -0.4822e-07, -0.5004e-07, -0.5061e-07, -0.5485e-07, &
        -0.6687e-07, -0.8483e-07, -0.9896e-07, -0.9646e-07, -0.5557e-07, &
         0.5765e-07,  0.2752e-06,  0.5870e-06,  0.9188e-06,  0.1291e-05, &
         0.1971e-05,  0.3251e-05,  0.5115e-05,  0.7221e-05,  0.8825e-05, &
         0.9032e-05/
      data ((h21(ip,iw),iw=1,31), ip=22,22)/ &
         0.99999607,  0.99999213,  0.99998438,  0.99996895,  0.99993849, &
         0.99987900,  0.99976391,  0.99954629,  0.99914569,  0.99843502, &
         0.99722600,  0.99526101,  0.99221897,  0.98771000,  0.98123002, &
         0.97209001,  0.95936000,  0.94187999,  0.91820002,  0.88679999, &
         0.84609997,  0.79530001,  0.73379999,  0.66090000,  0.57529998, &
         0.47740000,  0.37129998,  0.26490003,  0.16890001,  0.09340000, &
         0.04310000/
      data ((h22(ip,iw),iw=1,31), ip=22,22)/ &
        -0.5833e-07, -0.1160e-06, -0.2300e-06, -0.4540e-06, -0.8885e-06, &
        -0.1718e-05, -0.3256e-05, -0.6010e-05, -0.1072e-04, -0.1838e-04, &
        -0.3026e-04, -0.4772e-04, -0.7223e-04, -0.1057e-03, -0.1512e-03, &
        -0.2128e-03, -0.2961e-03, -0.4070e-03, -0.5514e-03, -0.7320e-03, &
        -0.9467e-03, -0.1187e-02, -0.1446e-02, -0.1717e-02, -0.1985e-02, &
        -0.2203e-02, -0.2307e-02, -0.2237e-02, -0.1965e-02, -0.1532e-02, &
        -0.1044e-02/
      data ((h23(ip,iw),iw=1,31), ip=22,22)/ &
        -0.2426e-09, -0.4805e-09, -0.9447e-09, -0.1841e-08, -0.3519e-08, &
        -0.6565e-08, -0.1172e-07, -0.1979e-07, -0.3095e-07, -0.4443e-07, &
        -0.5821e-07, -0.6868e-07, -0.7282e-07, -0.7208e-07, -0.7176e-07, &
        -0.7562e-07, -0.8110e-07, -0.7934e-07, -0.5365e-07,  0.2483e-07, &
         0.1959e-06,  0.4731e-06,  0.7954e-06,  0.1123e-05,  0.1652e-05, &
         0.2711e-05,  0.4402e-05,  0.6498e-05,  0.8392e-05,  0.9154e-05, &
         0.8261e-05/
      data ((h21(ip,iw),iw=1,31), ip=23,23)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996859,  0.99993771, &
         0.99987692,  0.99975860,  0.99953198,  0.99910772,  0.99833697, &
         0.99698901,  0.99473202,  0.99113101,  0.98566002,  0.97770000, &
         0.96640998,  0.95076001,  0.92943001,  0.90092003,  0.86370003, &
         0.81659997,  0.75889999,  0.69000000,  0.60870004,  0.51440001, &
         0.40990001,  0.30190003,  0.20060003,  0.11680001,  0.05760002, &
         0.02270001/
      data ((h22(ip,iw),iw=1,31), ip=23,23)/ &
        -0.5929e-07, -0.1180e-06, -0.2344e-06, -0.4638e-06, -0.9118e-06, &
        -0.1775e-05, -0.3401e-05, -0.6375e-05, -0.1160e-04, -0.2039e-04, &
        -0.3444e-04, -0.5575e-04, -0.8641e-04, -0.1287e-03, -0.1856e-03, &
        -0.2615e-03, -0.3618e-03, -0.4928e-03, -0.6594e-03, -0.8621e-03, &
        -0.1095e-02, -0.1349e-02, -0.1618e-02, -0.1894e-02, -0.2140e-02, &
        -0.2293e-02, -0.2290e-02, -0.2085e-02, -0.1696e-02, -0.1212e-02, &
        -0.7506e-03/
      data ((h23(ip,iw),iw=1,31), ip=23,23)/ &
        -0.2496e-09, -0.4954e-09, -0.9780e-09, -0.1915e-08, -0.3697e-08, &
        -0.6991e-08, -0.1279e-07, -0.2231e-07, -0.3653e-07, -0.5541e-07, &
        -0.7688e-07, -0.9614e-07, -0.1069e-06, -0.1065e-06, -0.9866e-07, &
        -0.8740e-07, -0.7192e-07, -0.4304e-07,  0.1982e-07,  0.1525e-06, &
         0.3873e-06,  0.6947e-06,  0.1000e-05,  0.1409e-05,  0.2253e-05, &
         0.3739e-05,  0.5744e-05,  0.7812e-05,  0.9067e-05,  0.8746e-05, &
         0.6940e-05/
      data ((h21(ip,iw),iw=1,31), ip=24,24)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996853,  0.99993742, &
         0.99987602,  0.99975550,  0.99952233,  0.99907869,  0.99825698, &
         0.99678302,  0.99424398,  0.99007100,  0.98356003,  0.97387999, &
         0.96004999,  0.94090003,  0.91503000,  0.88099998,  0.83740002, &
         0.78350002,  0.71869999,  0.64170003,  0.55149996,  0.44950002, &
         0.34100002,  0.23540002,  0.14380002,  0.07550001,  0.03240001, &
         0.01029998/
      data ((h22(ip,iw),iw=1,31), ip=24,24)/ &
        -0.5950e-07, -0.1185e-06, -0.2358e-06, -0.4678e-06, -0.9235e-06, &
        -0.1810e-05, -0.3503e-05, -0.6664e-05, -0.1236e-04, -0.2223e-04, &
        -0.3849e-04, -0.6396e-04, -0.1017e-03, -0.1545e-03, -0.2257e-03, &
        -0.3192e-03, -0.4399e-03, -0.5933e-03, -0.7824e-03, -0.1005e-02, &
        -0.1251e-02, -0.1516e-02, -0.1794e-02, -0.2060e-02, -0.2257e-02, &
        -0.2318e-02, -0.2186e-02, -0.1853e-02, -0.1386e-02, -0.9021e-03, &
        -0.5050e-03/
      data ((h23(ip,iw),iw=1,31), ip=24,24)/ &
        -0.2515e-09, -0.5001e-09, -0.9904e-09, -0.1951e-08, -0.3800e-08, &
        -0.7288e-08, -0.1362e-07, -0.2452e-07, -0.4184e-07, -0.6663e-07, &
        -0.9770e-07, -0.1299e-06, -0.1533e-06, -0.1584e-06, -0.1425e-06, &
        -0.1093e-06, -0.5972e-07,  0.1426e-07,  0.1347e-06,  0.3364e-06, &
         0.6209e-06,  0.9169e-06,  0.1243e-05,  0.1883e-05,  0.3145e-05, &
         0.5011e-05,  0.7136e-05,  0.8785e-05,  0.9048e-05,  0.7686e-05, &
         0.5368e-05/
      data ((h21(ip,iw),iw=1,31), ip=25,25)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996847,  0.99993724, &
         0.99987543,  0.99975342,  0.99951530,  0.99905682,  0.99819201, &
         0.99660802,  0.99381101,  0.98908001,  0.98148000,  0.96991003, &
         0.95317000,  0.92992002,  0.89880002,  0.85839999,  0.80799997, &
         0.74689996,  0.67420000,  0.58850002,  0.48979998,  0.38200003, &
         0.27329999,  0.17479998,  0.09700000,  0.04500002,  0.01620001, &
         0.00349998/
      data ((h22(ip,iw),iw=1,31), ip=25,25)/ &
        -0.5953e-07, -0.1187e-06, -0.2363e-06, -0.4697e-06, -0.9304e-06, &
        -0.1833e-05, -0.3578e-05, -0.6889e-05, -0.1299e-04, -0.2384e-04, &
        -0.4227e-04, -0.7201e-04, -0.1174e-03, -0.1825e-03, -0.2710e-03, &
        -0.3861e-03, -0.5313e-03, -0.7095e-03, -0.9203e-03, -0.1158e-02, &
        -0.1417e-02, -0.1691e-02, -0.1968e-02, -0.2200e-02, -0.2320e-02, &
        -0.2263e-02, -0.1998e-02, -0.1563e-02, -0.1068e-02, -0.6300e-03, &
        -0.3174e-03/
      data ((h23(ip,iw),iw=1,31), ip=25,25)/ &
        -0.2520e-09, -0.5016e-09, -0.9963e-09, -0.1971e-08, -0.3867e-08, &
        -0.7500e-08, -0.1427e-07, -0.2634e-07, -0.4656e-07, -0.7753e-07, &
        -0.1196e-06, -0.1683e-06, -0.2113e-06, -0.2309e-06, -0.2119e-06, &
        -0.1518e-06, -0.5200e-07,  0.9274e-07,  0.2973e-06,  0.5714e-06, &
         0.8687e-06,  0.1152e-05,  0.1621e-05,  0.2634e-05,  0.4310e-05, &
         0.6418e-05,  0.8347e-05,  0.9162e-05,  0.8319e-05,  0.6209e-05, &
         0.3844e-05/
      data ((h21(ip,iw),iw=1,31), ip=26,26)/ &
         0.99999601,  0.99999207,  0.99998420,  0.99996847,  0.99993718, &
         0.99987501,  0.99975210,  0.99951041,  0.99904078,  0.99814302, &
         0.99646801,  0.99344200,  0.98819000,  0.97952002,  0.96597999, &
         0.94600999,  0.91812003,  0.88099998,  0.83359998,  0.77569997, &
         0.70669997,  0.62529999,  0.53049999,  0.42449999,  0.31419998, &
         0.20969999,  0.12269998,  0.06089997,  0.02420002,  0.00660002, &
         0.00040001/
      data ((h22(ip,iw),iw=1,31), ip=26,26)/ &
        -0.5954e-07, -0.1187e-06, -0.2366e-06, -0.4709e-06, -0.9349e-06, &
        -0.1849e-05, -0.3632e-05, -0.7058e-05, -0.1350e-04, -0.2521e-04, &
        -0.4564e-04, -0.7958e-04, -0.1329e-03, -0.2114e-03, -0.3200e-03, &
        -0.4611e-03, -0.6353e-03, -0.8409e-03, -0.1072e-02, -0.1324e-02, &
        -0.1592e-02, -0.1871e-02, -0.2127e-02, -0.2297e-02, -0.2312e-02, &
        -0.2123e-02, -0.1738e-02, -0.1247e-02, -0.7744e-03, -0.4117e-03, &
        -0.1850e-03/
      data ((h23(ip,iw),iw=1,31), ip=26,26)/ &
        -0.2522e-09, -0.5025e-09, -0.9997e-09, -0.1983e-08, -0.3912e-08, &
        -0.7650e-08, -0.1474e-07, -0.2777e-07, -0.5055e-07, -0.8745e-07, &
        -0.1414e-06, -0.2095e-06, -0.2790e-06, -0.3241e-06, -0.3135e-06, &
        -0.2269e-06, -0.5896e-07,  0.1875e-06,  0.4996e-06,  0.8299e-06, &
         0.1115e-05,  0.1467e-05,  0.2236e-05,  0.3672e-05,  0.5668e-05, &
         0.7772e-05,  0.9094e-05,  0.8827e-05,  0.7041e-05,  0.4638e-05, &
         0.2539e-05/
      data ((h81(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967158,  0.99944150,  0.99910933,  0.99867302,  0.99814397, &
         0.99753898,  0.99686199,  0.99610198,  0.99523401,  0.99421698, &
         0.99299300,  0.99147898,  0.98958999,  0.98721999,  0.98430002, &
         0.98071998,  0.97639000,  0.97115999,  0.96480000,  0.95695001, &
         0.94713998,  0.93469000,  0.91873002,  0.89810002,  0.87129998, &
         0.83679998/
      data ((h82(ip,iw),iw=1,31), ip= 1, 1)/ &
        -0.5685e-08, -0.1331e-07, -0.3249e-07, -0.8137e-07, -0.2048e-06, &
        -0.4973e-06, -0.1118e-05, -0.2246e-05, -0.3982e-05, -0.6290e-05, &
        -0.9040e-05, -0.1215e-04, -0.1567e-04, -0.1970e-04, -0.2449e-04, &
        -0.3046e-04, -0.3798e-04, -0.4725e-04, -0.5831e-04, -0.7123e-04, &
        -0.8605e-04, -0.1028e-03, -0.1212e-03, -0.1413e-03, -0.1635e-03, &
        -0.1884e-03, -0.2160e-03, -0.2461e-03, -0.2778e-03, -0.3098e-03, &
        -0.3411e-03/
      data ((h83(ip,iw),iw=1,31), ip= 1, 1)/ &
         0.2169e-10,  0.5237e-10,  0.1296e-09,  0.3204e-09,  0.7665e-09, &
         0.1691e-08,  0.3222e-08,  0.5110e-08,  0.6779e-08,  0.7681e-08, &
         0.7378e-08,  0.5836e-08,  0.3191e-08, -0.1491e-08, -0.1022e-07, &
        -0.2359e-07, -0.3957e-07, -0.5553e-07, -0.6927e-07, -0.7849e-07, &
        -0.8139e-07, -0.7853e-07, -0.7368e-07, -0.7220e-07, -0.7780e-07, &
        -0.9091e-07, -0.1038e-06, -0.9929e-07, -0.5422e-07,  0.5379e-07, &
         0.2350e-06/
      data ((h81(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967158,  0.99944139,  0.99910921,  0.99867302,  0.99814397, &
         0.99753797,  0.99686003,  0.99609798,  0.99522603,  0.99420297, &
         0.99296701,  0.99142998,  0.98949999,  0.98706001,  0.98400998, &
         0.98021001,  0.97552001,  0.96976000,  0.96262002,  0.95367002, &
         0.94234002,  0.92781997,  0.90903997,  0.88459998,  0.85290003, &
         0.81200004/
      data ((h82(ip,iw),iw=1,31), ip= 2, 2)/ &
        -0.5684e-08, -0.1331e-07, -0.3248e-07, -0.8133e-07, -0.2047e-06, &
        -0.4971e-06, -0.1117e-05, -0.2245e-05, -0.3981e-05, -0.6287e-05, &
        -0.9035e-05, -0.1215e-04, -0.1565e-04, -0.1967e-04, -0.2444e-04, &
        -0.3036e-04, -0.3780e-04, -0.4694e-04, -0.5779e-04, -0.7042e-04, &
        -0.8491e-04, -0.1013e-03, -0.1196e-03, -0.1399e-03, -0.1625e-03, &
        -0.1879e-03, -0.2163e-03, -0.2474e-03, -0.2803e-03, -0.3140e-03, &
        -0.3478e-03/
      data ((h83(ip,iw),iw=1,31), ip= 2, 2)/ &
         0.2168e-10,  0.5242e-10,  0.1295e-09,  0.3201e-09,  0.7662e-09, &
         0.1690e-08,  0.3220e-08,  0.5106e-08,  0.6776e-08,  0.7673e-08, &
         0.7362e-08,  0.5808e-08,  0.3138e-08, -0.1595e-08, -0.1041e-07, &
        -0.2390e-07, -0.4010e-07, -0.5636e-07, -0.7045e-07, -0.7972e-07, &
        -0.8178e-07, -0.7677e-07, -0.6876e-07, -0.6381e-07, -0.6583e-07, &
        -0.7486e-07, -0.8229e-07, -0.7017e-07, -0.1497e-07,  0.1051e-06, &
         0.2990e-06/
      data ((h81(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967152,  0.99944133,  0.99910891,  0.99867201,  0.99814302, &
         0.99753499,  0.99685597,  0.99609101,  0.99521297,  0.99418002, &
         0.99292499,  0.99135399,  0.98935997,  0.98681003,  0.98356998, &
         0.97947001,  0.97430998,  0.96784997,  0.95972002,  0.94941002, &
         0.93620002,  0.91912001,  0.89690000,  0.86790001,  0.83020002, &
         0.78210002/
      data ((h82(ip,iw),iw=1,31), ip= 3, 3)/ &
        -0.5682e-08, -0.1330e-07, -0.3247e-07, -0.8129e-07, -0.2046e-06, &
        -0.4968e-06, -0.1117e-05, -0.2244e-05, -0.3978e-05, -0.6283e-05, &
        -0.9027e-05, -0.1213e-04, -0.1563e-04, -0.1963e-04, -0.2436e-04, &
        -0.3021e-04, -0.3754e-04, -0.4649e-04, -0.5709e-04, -0.6940e-04, &
        -0.8359e-04, -0.9986e-04, -0.1182e-03, -0.1388e-03, -0.1620e-03, &
        -0.1882e-03, -0.2175e-03, -0.2498e-03, -0.2843e-03, -0.3203e-03, &
        -0.3573e-03/
      data ((h83(ip,iw),iw=1,31), ip= 3, 3)/ &
         0.2167e-10,  0.5238e-10,  0.1294e-09,  0.3198e-09,  0.7656e-09, &
         0.1688e-08,  0.3217e-08,  0.5104e-08,  0.6767e-08,  0.7661e-08, &
         0.7337e-08,  0.5764e-08,  0.3051e-08, -0.1752e-08, -0.1068e-07, &
        -0.2436e-07, -0.4081e-07, -0.5740e-07, -0.7165e-07, -0.8046e-07, &
        -0.8082e-07, -0.7289e-07, -0.6141e-07, -0.5294e-07, -0.5134e-07, &
        -0.5552e-07, -0.5609e-07, -0.3464e-07,  0.3275e-07,  0.1669e-06, &
         0.3745e-06/
      data ((h81(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990171,  0.99981678, &
         0.99967140,  0.99944109,  0.99910849,  0.99867100,  0.99814099, &
         0.99753201,  0.99685001,  0.99607998,  0.99519402,  0.99414498, &
         0.99286002,  0.99123698,  0.98914999,  0.98644000,  0.98293000, &
         0.97842002,  0.97263998,  0.96529001,  0.95592999,  0.94392002, &
         0.92839003,  0.90815997,  0.88169998,  0.84720004,  0.80269998, &
         0.74629998/
      data ((h82(ip,iw),iw=1,31), ip= 4, 4)/ &
        -0.5680e-08, -0.1329e-07, -0.3243e-07, -0.8121e-07, -0.2044e-06, &
        -0.4963e-06, -0.1115e-05, -0.2242e-05, -0.3974e-05, -0.6276e-05, &
        -0.9015e-05, -0.1211e-04, -0.1559e-04, -0.1956e-04, -0.2423e-04, &
        -0.2999e-04, -0.3716e-04, -0.4588e-04, -0.5618e-04, -0.6818e-04, &
        -0.8218e-04, -0.9847e-04, -0.1171e-03, -0.1382e-03, -0.1621e-03, &
        -0.1892e-03, -0.2197e-03, -0.2535e-03, -0.2902e-03, -0.3293e-03, &
        -0.3700e-03/
      data ((h83(ip,iw),iw=1,31), ip= 4, 4)/ &
         0.2166e-10,  0.5229e-10,  0.1294e-09,  0.3193e-09,  0.7644e-09, &
         0.1686e-08,  0.3213e-08,  0.5092e-08,  0.6753e-08,  0.7640e-08, &
         0.7302e-08,  0.5696e-08,  0.2917e-08, -0.1984e-08, -0.1108e-07, &
        -0.2497e-07, -0.4171e-07, -0.5849e-07, -0.7254e-07, -0.8017e-07, &
        -0.7802e-07, -0.6662e-07, -0.5153e-07, -0.3961e-07, -0.3387e-07, &
        -0.3219e-07, -0.2426e-07,  0.8700e-08,  0.9027e-07,  0.2400e-06, &
         0.4623e-06/
      data ((h81(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990165,  0.99981672, &
         0.99967128,  0.99944091,  0.99910778,  0.99866998,  0.99813801, &
         0.99752700,  0.99684101,  0.99606299,  0.99516302,  0.99408901, &
         0.99276000,  0.99105698,  0.98882997,  0.98588997,  0.98202002, &
         0.97696000,  0.97039002,  0.96192998,  0.95104003,  0.93691999, &
         0.91851997,  0.89440000,  0.86290002,  0.82200003,  0.76969999, &
         0.70420003/
      data ((h82(ip,iw),iw=1,31), ip= 5, 5)/ &
        -0.5675e-08, -0.1328e-07, -0.3239e-07, -0.8110e-07, -0.2040e-06, &
        -0.4954e-06, -0.1114e-05, -0.2238e-05, -0.3968e-05, -0.6265e-05, &
        -0.8996e-05, -0.1208e-04, -0.1553e-04, -0.1945e-04, -0.2404e-04, &
        -0.2966e-04, -0.3663e-04, -0.4508e-04, -0.5508e-04, -0.6686e-04, &
        -0.8082e-04, -0.9732e-04, -0.1165e-03, -0.1382e-03, -0.1630e-03, &
        -0.1913e-03, -0.2234e-03, -0.2593e-03, -0.2989e-03, -0.3417e-03, &
        -0.3857e-03/
      data ((h83(ip,iw),iw=1,31), ip= 5, 5)/ &
         0.2163e-10,  0.5209e-10,  0.1291e-09,  0.3186e-09,  0.7626e-09, &
         0.1682e-08,  0.3203e-08,  0.5078e-08,  0.6730e-08,  0.7606e-08, &
         0.7246e-08,  0.5592e-08,  0.2735e-08, -0.2325e-08, -0.1162e-07, &
        -0.2576e-07, -0.4268e-07, -0.5938e-07, -0.7262e-07, -0.7827e-07, &
        -0.7297e-07, -0.5786e-07, -0.3930e-07, -0.2373e-07, -0.1295e-07, &
        -0.3728e-08,  0.1465e-07,  0.6114e-07,  0.1590e-06,  0.3257e-06, &
         0.5622e-06/
      data ((h81(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990165,  0.99981672, &
         0.99967122,  0.99944037,  0.99910682,  0.99866802,  0.99813402, &
         0.99751902,  0.99682599,  0.99603701,  0.99511498,  0.99400300, &
         0.99260598,  0.99078500,  0.98835999,  0.98510998,  0.98075998, &
         0.97499001,  0.96741998,  0.95757002,  0.94476998,  0.92804998, &
         0.90613002,  0.87739998,  0.83990002,  0.79159999,  0.73049998, &
         0.65540004/
      data ((h82(ip,iw),iw=1,31), ip= 6, 6)/ &
        -0.5671e-08, -0.1326e-07, -0.3234e-07, -0.8091e-07, -0.2035e-06, &
        -0.4941e-06, -0.1111e-05, -0.2232e-05, -0.3958e-05, -0.6247e-05, &
        -0.8966e-05, -0.1202e-04, -0.1544e-04, -0.1929e-04, -0.2377e-04, &
        -0.2921e-04, -0.3593e-04, -0.4409e-04, -0.5385e-04, -0.6555e-04, &
        -0.7965e-04, -0.9656e-04, -0.1163e-03, -0.1390e-03, -0.1649e-03, &
        -0.1947e-03, -0.2288e-03, -0.2675e-03, -0.3109e-03, -0.3575e-03, &
        -0.4039e-03/
      data ((h83(ip,iw),iw=1,31), ip= 6, 6)/ &
         0.2155e-10,  0.5188e-10,  0.1288e-09,  0.3175e-09,  0.7599e-09, &
         0.1675e-08,  0.3190e-08,  0.5059e-08,  0.6699e-08,  0.7551e-08, &
         0.7154e-08,  0.5435e-08,  0.2452e-08, -0.2802e-08, -0.1235e-07, &
        -0.2668e-07, -0.4353e-07, -0.5962e-07, -0.7134e-07, -0.7435e-07, &
        -0.6551e-07, -0.4676e-07, -0.2475e-07, -0.4876e-08,  0.1235e-07, &
         0.3092e-07,  0.6192e-07,  0.1243e-06,  0.2400e-06,  0.4247e-06, &
         0.6755e-06/
      data ((h81(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.99998659,  0.99997360,  0.99994862,  0.99990165,  0.99981660, &
         0.99967092,  0.99943972,  0.99910510,  0.99866402,  0.99812698, &
         0.99750602,  0.99680197,  0.99599499,  0.99504000,  0.99387002, &
         0.99237198,  0.99038202,  0.98768002,  0.98400998,  0.97903001, &
         0.97236001,  0.96354002,  0.95196998,  0.93681002,  0.91688001, &
         0.89069998,  0.85640001,  0.81190002,  0.75520003,  0.68470001, &
         0.59990001/
      data ((h82(ip,iw),iw=1,31), ip= 7, 7)/ &
        -0.5665e-08, -0.1322e-07, -0.3224e-07, -0.8063e-07, -0.2027e-06, &
        -0.4921e-06, -0.1106e-05, -0.2223e-05, -0.3942e-05, -0.6220e-05, &
        -0.8920e-05, -0.1194e-04, -0.1530e-04, -0.1905e-04, -0.2337e-04, &
        -0.2860e-04, -0.3505e-04, -0.4296e-04, -0.5259e-04, -0.6439e-04, &
        -0.7884e-04, -0.9635e-04, -0.1170e-03, -0.1407e-03, -0.1681e-03, &
        -0.1998e-03, -0.2366e-03, -0.2790e-03, -0.3265e-03, -0.3763e-03, &
        -0.4235e-03/
      data ((h83(ip,iw),iw=1,31), ip= 7, 7)/ &
         0.2157e-10,  0.5178e-10,  0.1283e-09,  0.3162e-09,  0.7558e-09, &
         0.1665e-08,  0.3169e-08,  0.5027e-08,  0.6645e-08,  0.7472e-08, &
         0.7017e-08,  0.5212e-08,  0.2059e-08, -0.3443e-08, -0.1321e-07, &
        -0.2754e-07, -0.4389e-07, -0.5869e-07, -0.6825e-07, -0.6819e-07, &
        -0.5570e-07, -0.3343e-07, -0.7592e-08,  0.1778e-07,  0.4322e-07, &
         0.7330e-07,  0.1190e-06,  0.1993e-06,  0.3348e-06,  0.5376e-06, &
         0.8030e-06/
      data ((h81(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.99998659,  0.99997360,  0.99994856,  0.99990159,  0.99981642, &
         0.99967051,  0.99943858,  0.99910247,  0.99865901,  0.99811602, &
         0.99748600,  0.99676597,  0.99592900,  0.99492502,  0.99366802, &
         0.99202400,  0.98979002,  0.98672003,  0.98249000,  0.97671002, &
         0.96890998,  0.95854002,  0.94483000,  0.92675000,  0.90289998, &
         0.87160003,  0.83069998,  0.77829999,  0.71239996,  0.63209999, &
         0.53839999/
      data ((h82(ip,iw),iw=1,31), ip= 8, 8)/ &
        -0.5652e-08, -0.1318e-07, -0.3210e-07, -0.8018e-07, -0.2014e-06, &
        -0.4888e-06, -0.1099e-05, -0.2210e-05, -0.3918e-05, -0.6179e-05, &
        -0.8849e-05, -0.1182e-04, -0.1509e-04, -0.1871e-04, -0.2284e-04, &
        -0.2782e-04, -0.3403e-04, -0.4177e-04, -0.5145e-04, -0.6354e-04, &
        -0.7853e-04, -0.9681e-04, -0.1185e-03, -0.1437e-03, -0.1729e-03, &
        -0.2072e-03, -0.2475e-03, -0.2942e-03, -0.3457e-03, -0.3973e-03, &
        -0.4434e-03/
      data ((h83(ip,iw),iw=1,31), ip= 8, 8)/ &
         0.2153e-10,  0.5151e-10,  0.1273e-09,  0.3136e-09,  0.7488e-09, &
         0.1649e-08,  0.3142e-08,  0.4980e-08,  0.6559e-08,  0.7346e-08, &
         0.6813e-08,  0.4884e-08,  0.1533e-08, -0.4209e-08, -0.1409e-07, &
        -0.2801e-07, -0.4320e-07, -0.5614e-07, -0.6312e-07, -0.5976e-07, &
        -0.4369e-07, -0.1775e-07,  0.1280e-07,  0.4534e-07,  0.8106e-07, &
         0.1246e-06,  0.1874e-06,  0.2873e-06,  0.4433e-06,  0.6651e-06, &
         0.9477e-06/
      data ((h81(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.99998659,  0.99997360,  0.99994856,  0.99990153,  0.99981618, &
         0.99966979,  0.99943691,  0.99909842,  0.99865001,  0.99809903, &
         0.99745399,  0.99670798,  0.99582899,  0.99475002,  0.99336600, &
         0.99151403,  0.98896003,  0.98540002,  0.98044997,  0.97364998, &
         0.96445000,  0.95213997,  0.93579000,  0.91412002,  0.88550001, &
         0.84810001,  0.79970002,  0.73850000,  0.66280001,  0.57309997, &
         0.47189999/
      data ((h82(ip,iw),iw=1,31), ip= 9, 9)/ &
        -0.5629e-08, -0.1310e-07, -0.3186e-07, -0.7948e-07, -0.1995e-06, &
        -0.4837e-06, -0.1088e-05, -0.2188e-05, -0.3880e-05, -0.6115e-05, &
        -0.8743e-05, -0.1165e-04, -0.1480e-04, -0.1824e-04, -0.2216e-04, &
        -0.2691e-04, -0.3293e-04, -0.4067e-04, -0.5057e-04, -0.6314e-04, &
        -0.7885e-04, -0.9813e-04, -0.1212e-03, -0.1482e-03, -0.1799e-03, &
        -0.2175e-03, -0.2622e-03, -0.3135e-03, -0.3678e-03, -0.4193e-03, &
        -0.4627e-03/
      data ((h83(ip,iw),iw=1,31), ip= 9, 9)/ &
         0.2121e-10,  0.5076e-10,  0.1257e-09,  0.3091e-09,  0.7379e-09, &
         0.1623e-08,  0.3097e-08,  0.4904e-08,  0.6453e-08,  0.7168e-08, &
         0.6534e-08,  0.4458e-08,  0.8932e-09, -0.5026e-08, -0.1469e-07, &
        -0.2765e-07, -0.4103e-07, -0.5169e-07, -0.5585e-07, -0.4913e-07, &
        -0.2954e-07,  0.6372e-09,  0.3738e-07,  0.7896e-07,  0.1272e-06, &
         0.1867e-06,  0.2682e-06,  0.3895e-06,  0.5672e-06,  0.8091e-06, &
         0.1114e-05/
      data ((h81(ip,iw),iw=1,31), ip=10,10)/ &
         0.99998659,  0.99997360,  0.99994850,  0.99990141,  0.99981582, &
         0.99966878,  0.99943417,  0.99909198,  0.99863601,  0.99807203, &
         0.99740499,  0.99662101,  0.99567503,  0.99448699,  0.99292302, &
         0.99078500,  0.98780000,  0.98360002,  0.97773999,  0.96968001, &
         0.95872998,  0.94401997,  0.92440999,  0.89840001,  0.86409998, &
         0.81959999,  0.76279998,  0.69190001,  0.60650003,  0.50839996, &
         0.40249997/
      data ((h82(ip,iw),iw=1,31), ip=10,10)/ &
        -0.5597e-08, -0.1300e-07, -0.3148e-07, -0.7838e-07, -0.1964e-06, &
        -0.4759e-06, -0.1071e-05, -0.2155e-05, -0.3822e-05, -0.6019e-05, &
        -0.8586e-05, -0.1139e-04, -0.1439e-04, -0.1764e-04, -0.2134e-04, &
        -0.2591e-04, -0.3188e-04, -0.3978e-04, -0.5011e-04, -0.6334e-04, &
        -0.7998e-04, -0.1006e-03, -0.1253e-03, -0.1547e-03, -0.1895e-03, &
        -0.2315e-03, -0.2811e-03, -0.3363e-03, -0.3917e-03, -0.4413e-03, &
        -0.4809e-03/
      data ((h83(ip,iw),iw=1,31), ip=10,10)/ &
         0.2109e-10,  0.5017e-10,  0.1235e-09,  0.3021e-09,  0.7217e-09, &
         0.1585e-08,  0.3028e-08,  0.4796e-08,  0.6285e-08,  0.6910e-08, &
         0.6178e-08,  0.3945e-08,  0.2436e-09, -0.5632e-08, -0.1464e-07, &
        -0.2596e-07, -0.3707e-07, -0.4527e-07, -0.4651e-07, -0.3644e-07, &
        -0.1296e-07,  0.2250e-07,  0.6722e-07,  0.1202e-06,  0.1831e-06, &
         0.2605e-06,  0.3627e-06,  0.5062e-06,  0.7064e-06,  0.9725e-06, &
         0.1304e-05/
      data ((h81(ip,iw),iw=1,31), ip=11,11)/ &
         0.99998659,  0.99997354,  0.99994850,  0.99990124,  0.99981529, &
         0.99966723,  0.99942988,  0.99908209,  0.99861503,  0.99803102, &
         0.99732900,  0.99648702,  0.99544603,  0.99409997,  0.99228698, &
         0.98977000,  0.98620999,  0.98120999,  0.97421998,  0.96458000, &
         0.95146000,  0.93378001,  0.91017002,  0.87889999,  0.83810002, &
         0.78549999,  0.71930003,  0.63859999,  0.54409999,  0.43970001, &
         0.33249998/
      data ((h82(ip,iw),iw=1,31), ip=11,11)/ &
        -0.5538e-08, -0.1280e-07, -0.3089e-07, -0.7667e-07, -0.1917e-06, &
        -0.4642e-06, -0.1045e-05, -0.2106e-05, -0.3736e-05, -0.5878e-05, &
        -0.8363e-05, -0.1104e-04, -0.1387e-04, -0.1692e-04, -0.2044e-04, &
        -0.2493e-04, -0.3101e-04, -0.3926e-04, -0.5020e-04, -0.6429e-04, &
        -0.8213e-04, -0.1044e-03, -0.1314e-03, -0.1637e-03, -0.2027e-03, &
        -0.2498e-03, -0.3042e-03, -0.3617e-03, -0.4163e-03, -0.4625e-03, &
        -0.4969e-03/
      data ((h83(ip,iw),iw=1,31), ip=11,11)/ &
         0.2067e-10,  0.4903e-10,  0.1200e-09,  0.2917e-09,  0.6965e-09, &
         0.1532e-08,  0.2925e-08,  0.4632e-08,  0.6054e-08,  0.6590e-08, &
         0.5746e-08,  0.3436e-08, -0.2251e-09, -0.5703e-08, -0.1344e-07, &
        -0.2256e-07, -0.3120e-07, -0.3690e-07, -0.3520e-07, -0.2164e-07, &
         0.6510e-08,  0.4895e-07,  0.1037e-06,  0.1702e-06,  0.2502e-06, &
         0.3472e-06,  0.4710e-06,  0.6379e-06,  0.8633e-06,  0.1159e-05, &
         0.1514e-05/
      data ((h81(ip,iw),iw=1,31), ip=12,12)/ &
         0.99998659,  0.99997354,  0.99994838,  0.99990094,  0.99981439, &
         0.99966472,  0.99942350,  0.99906689,  0.99858302,  0.99796802, &
         0.99721497,  0.99628800,  0.99510801,  0.99354398,  0.99139601, &
         0.98838001,  0.98409998,  0.97807997,  0.96967000,  0.95806998, &
         0.94226003,  0.92093998,  0.89249998,  0.85510004,  0.80659997, &
         0.74510002,  0.66909999,  0.57870001,  0.47680002,  0.36919999, &
         0.26520002/
      data ((h82(ip,iw),iw=1,31), ip=12,12)/ &
        -0.5476e-08, -0.1257e-07, -0.3008e-07, -0.7418e-07, -0.1848e-06, &
        -0.4468e-06, -0.1006e-05, -0.2032e-05, -0.3611e-05, -0.5679e-05, &
        -0.8058e-05, -0.1059e-04, -0.1324e-04, -0.1612e-04, -0.1956e-04, &
        -0.2411e-04, -0.3046e-04, -0.3925e-04, -0.5098e-04, -0.6619e-04, &
        -0.8562e-04, -0.1100e-03, -0.1399e-03, -0.1761e-03, -0.2202e-03, &
        -0.2726e-03, -0.3306e-03, -0.3885e-03, -0.4404e-03, -0.4820e-03, &
        -0.5082e-03/
      data ((h83(ip,iw),iw=1,31), ip=12,12)/ &
         0.2041e-10,  0.4771e-10,  0.1149e-09,  0.2782e-09,  0.6614e-09, &
         0.1451e-08,  0.2778e-08,  0.4401e-08,  0.5736e-08,  0.6189e-08, &
         0.5315e-08,  0.3087e-08, -0.2518e-09, -0.4806e-08, -0.1071e-07, &
        -0.1731e-07, -0.2346e-07, -0.2659e-07, -0.2184e-07, -0.4261e-08, &
         0.2975e-07,  0.8112e-07,  0.1484e-06,  0.2308e-06,  0.3296e-06, &
         0.4475e-06,  0.5942e-06,  0.7859e-06,  0.1041e-05,  0.1369e-05, &
         0.1726e-05/
      data ((h81(ip,iw),iw=1,31), ip=13,13)/ &
         0.99998653,  0.99997348,  0.99994826,  0.99990052,  0.99981320, &
         0.99966109,  0.99941391,  0.99904412,  0.99853402,  0.99787498, &
         0.99704498,  0.99599600,  0.99462402,  0.99276501,  0.99017602, &
         0.98651999,  0.98133999,  0.97403997,  0.96386999,  0.94984001, &
         0.93071002,  0.90495998,  0.87080002,  0.82620001,  0.76910001, &
         0.69790000,  0.61199999,  0.51320004,  0.40640002,  0.29970002, &
         0.20359999/
      data ((h82(ip,iw),iw=1,31), ip=13,13)/ &
        -0.5362e-08, -0.1223e-07, -0.2895e-07, -0.7071e-07, -0.1748e-06, &
        -0.4219e-06, -0.9516e-06, -0.1928e-05, -0.3436e-05, -0.5409e-05, &
        -0.7666e-05, -0.1005e-04, -0.1254e-04, -0.1533e-04, -0.1880e-04, &
        -0.2358e-04, -0.3038e-04, -0.3988e-04, -0.5264e-04, -0.6934e-04, &
        -0.9083e-04, -0.1179e-03, -0.1515e-03, -0.1927e-03, -0.2424e-03, &
        -0.2994e-03, -0.3591e-03, -0.4155e-03, -0.4634e-03, -0.4982e-03, &
        -0.5096e-03/
      data ((h83(ip,iw),iw=1,31), ip=13,13)/ &
         0.1976e-10,  0.4551e-10,  0.1086e-09,  0.2601e-09,  0.6126e-09, &
         0.1345e-08,  0.2583e-08,  0.4112e-08,  0.5365e-08,  0.5796e-08, &
         0.5031e-08,  0.3182e-08,  0.5970e-09, -0.2547e-08, -0.6172e-08, &
        -0.1017e-07, -0.1388e-07, -0.1430e-07, -0.6118e-08,  0.1624e-07, &
         0.5791e-07,  0.1205e-06,  0.2025e-06,  0.3032e-06,  0.4225e-06, &
         0.5619e-06,  0.7322e-06,  0.9528e-06,  0.1243e-05,  0.1592e-05, &
         0.1904e-05/
      data ((h81(ip,iw),iw=1,31), ip=14,14)/ &
         0.99998653,  0.99997348,  0.99994808,  0.99989992,  0.99981129, &
         0.99965578,  0.99939990,  0.99901080,  0.99846399,  0.99773800, &
         0.99680001,  0.99558002,  0.99394703,  0.99169999,  0.98853999, &
         0.98408002,  0.97776002,  0.96888000,  0.95652002,  0.93949002, &
         0.91631001,  0.88529998,  0.84439999,  0.79159999,  0.72510004, &
         0.64390004,  0.54890001,  0.44379997,  0.33560002,  0.23449999, &
         0.15009999/
      data ((h82(ip,iw),iw=1,31), ip=14,14)/ &
        -0.5210e-08, -0.1172e-07, -0.2731e-07, -0.6598e-07, -0.1615e-06, &
        -0.3880e-06, -0.8769e-06, -0.1787e-05, -0.3204e-05, -0.5066e-05, &
        -0.7197e-05, -0.9451e-05, -0.1185e-04, -0.1465e-04, -0.1831e-04, &
        -0.2346e-04, -0.3088e-04, -0.4132e-04, -0.5545e-04, -0.7410e-04, &
        -0.9820e-04, -0.1288e-03, -0.1670e-03, -0.2140e-03, -0.2692e-03, &
        -0.3293e-03, -0.3886e-03, -0.4417e-03, -0.4840e-03, -0.5073e-03, &
        -0.4944e-03/
      data ((h83(ip,iw),iw=1,31), ip=14,14)/ &
         0.1880e-10,  0.4271e-10,  0.9966e-10,  0.2352e-09,  0.5497e-09, &
         0.1205e-08,  0.2334e-08,  0.3765e-08,  0.4993e-08,  0.5532e-08, &
         0.5148e-08,  0.4055e-08,  0.2650e-08,  0.1326e-08,  0.2019e-09, &
        -0.1124e-08, -0.2234e-08,  0.2827e-09,  0.1247e-07,  0.4102e-07, &
         0.9228e-07,  0.1682e-06,  0.2676e-06,  0.3885e-06,  0.5286e-06, &
         0.6904e-06,  0.8871e-06,  0.1142e-05,  0.1466e-05,  0.1800e-05, &
         0.2004e-05/
      data ((h81(ip,iw),iw=1,31), ip=15,15)/ &
         0.99998653,  0.99997336,  0.99994785,  0.99989909,  0.99980879, &
         0.99964851,  0.99938041,  0.99896401,  0.99836302,  0.99754399, &
         0.99645603,  0.99500400,  0.99302697,  0.99027801,  0.98640001, &
         0.98092002,  0.97319001,  0.96234000,  0.94727999,  0.92657000, &
         0.89850003,  0.86119998,  0.81260002,  0.75080001,  0.67429996, &
         0.58350003,  0.48089999,  0.37250000,  0.26760000,  0.17650002, &
         0.10610002/
      data ((h82(ip,iw),iw=1,31), ip=15,15)/ &
        -0.5045e-08, -0.1113e-07, -0.2540e-07, -0.6008e-07, -0.1449e-06, &
        -0.3457e-06, -0.7826e-06, -0.1609e-05, -0.2920e-05, -0.4665e-05, &
        -0.6691e-05, -0.8868e-05, -0.1127e-04, -0.1422e-04, -0.1820e-04, &
        -0.2389e-04, -0.3213e-04, -0.4380e-04, -0.5975e-04, -0.8092e-04, &
        -0.1083e-03, -0.1433e-03, -0.1873e-03, -0.2402e-03, -0.2997e-03, &
        -0.3607e-03, -0.4178e-03, -0.4662e-03, -0.4994e-03, -0.5028e-03, &
        -0.4563e-03/
      data ((h83(ip,iw),iw=1,31), ip=15,15)/ &
         0.1804e-10,  0.3983e-10,  0.9045e-10,  0.2080e-09,  0.4786e-09, &
         0.1046e-08,  0.2052e-08,  0.3413e-08,  0.4704e-08,  0.5565e-08, &
         0.5887e-08,  0.5981e-08,  0.6202e-08,  0.6998e-08,  0.8493e-08, &
         0.1002e-07,  0.1184e-07,  0.1780e-07,  0.3483e-07,  0.7122e-07, &
         0.1341e-06,  0.2259e-06,  0.3446e-06,  0.4866e-06,  0.6486e-06, &
         0.8343e-06,  0.1063e-05,  0.1356e-05,  0.1690e-05,  0.1951e-05, &
         0.2005e-05/
      data ((h81(ip,iw),iw=1,31), ip=16,16)/ &
         0.99998647,  0.99997330,  0.99994755,  0.99989808,  0.99980563, &
         0.99963909,  0.99935490,  0.99890202,  0.99822801,  0.99728203, &
         0.99599099,  0.99423301,  0.99181002,  0.98842001,  0.98364002, &
         0.97689003,  0.96740001,  0.95414001,  0.93575001,  0.91060001, &
         0.87680000,  0.83219999,  0.77490002,  0.70330000,  0.61689997, &
         0.51750004,  0.40990001,  0.30239999,  0.20539999,  0.12750000, &
         0.07150000/
      data ((h82(ip,iw),iw=1,31), ip=16,16)/ &
        -0.4850e-08, -0.1045e-07, -0.2334e-07, -0.5367e-07, -0.1265e-06, &
        -0.2980e-06, -0.6750e-06, -0.1406e-05, -0.2601e-05, -0.4239e-05, &
        -0.6201e-05, -0.8389e-05, -0.1091e-04, -0.1413e-04, -0.1859e-04, &
        -0.2500e-04, -0.3432e-04, -0.4761e-04, -0.6595e-04, -0.9030e-04, &
        -0.1219e-03, -0.1624e-03, -0.2126e-03, -0.2708e-03, -0.3327e-03, &
        -0.3926e-03, -0.4458e-03, -0.4871e-03, -0.5045e-03, -0.4777e-03, &
        -0.3954e-03/
      data ((h83(ip,iw),iw=1,31), ip=16,16)/ &
         0.1717e-10,  0.3723e-10,  0.8093e-10,  0.1817e-09,  0.4100e-09, &
         0.8932e-09,  0.1791e-08,  0.3126e-08,  0.4634e-08,  0.6095e-08, &
         0.7497e-08,  0.9170e-08,  0.1136e-07,  0.1453e-07,  0.1892e-07, &
         0.2369e-07,  0.2909e-07,  0.3922e-07,  0.6232e-07,  0.1083e-06, &
         0.1847e-06,  0.2943e-06,  0.4336e-06,  0.5970e-06,  0.7815e-06, &
         0.9959e-06,  0.1263e-05,  0.1583e-05,  0.1880e-05,  0.2009e-05, &
         0.1914e-05/
      data ((h81(ip,iw),iw=1,31), ip=17,17)/ &
         0.99998647,  0.99997318,  0.99994719,  0.99989688,  0.99980187, &
         0.99962789,  0.99932390,  0.99882400,  0.99805701,  0.99694502, &
         0.99538797,  0.99323398,  0.99023998,  0.98604000,  0.98013997, &
         0.97182000,  0.96016002,  0.94391000,  0.92149997,  0.89100003, &
         0.85049999,  0.79769999,  0.73089999,  0.64919996,  0.55350000, &
         0.44760001,  0.33870000,  0.23670000,  0.15149999,  0.08810002, &
         0.04570001/
      data ((h82(ip,iw),iw=1,31), ip=17,17)/ &
        -0.4673e-08, -0.9862e-08, -0.2135e-07, -0.4753e-07, -0.1087e-06, &
        -0.2512e-06, -0.5671e-06, -0.1199e-05, -0.2281e-05, -0.3842e-05, &
        -0.5804e-05, -0.8110e-05, -0.1088e-04, -0.1452e-04, -0.1961e-04, &
        -0.2696e-04, -0.3768e-04, -0.5311e-04, -0.7444e-04, -0.1028e-03, &
        -0.1397e-03, -0.1865e-03, -0.2427e-03, -0.3047e-03, -0.3667e-03, &
        -0.4237e-03, -0.4712e-03, -0.5003e-03, -0.4921e-03, -0.4286e-03, &
        -0.3188e-03/
      data ((h83(ip,iw),iw=1,31), ip=17,17)/ &
         0.1653e-10,  0.3436e-10,  0.7431e-10,  0.1605e-09,  0.3548e-09, &
         0.7723e-09,  0.1595e-08,  0.2966e-08,  0.4849e-08,  0.7169e-08, &
         0.1003e-07,  0.1366e-07,  0.1825e-07,  0.2419e-07,  0.3186e-07, &
         0.4068e-07,  0.5064e-07,  0.6618e-07,  0.9684e-07,  0.1536e-06, &
         0.2450e-06,  0.3730e-06,  0.5328e-06,  0.7184e-06,  0.9291e-06, &
         0.1180e-05,  0.1484e-05,  0.1798e-05,  0.1992e-05,  0.1968e-05, &
         0.1736e-05/
      data ((h81(ip,iw),iw=1,31), ip=18,18)/ &
         0.99998647,  0.99997312,  0.99994683,  0.99989569,  0.99979800, &
         0.99961591,  0.99928999,  0.99873698,  0.99785602,  0.99653602, &
         0.99464101,  0.99198103,  0.98825997,  0.98306000,  0.97574002, &
         0.96548998,  0.95117003,  0.93129998,  0.90407002,  0.86739999, &
         0.81910002,  0.75720000,  0.68040001,  0.58880001,  0.48530000, &
         0.37610000,  0.27029997,  0.17830002,  0.10720003,  0.05790001, &
         0.02740002/
      data ((h82(ip,iw),iw=1,31), ip=18,18)/ &
        -0.4532e-08, -0.9395e-08, -0.1978e-07, -0.4272e-07, -0.9442e-07, &
        -0.2124e-06, -0.4747e-06, -0.1017e-05, -0.2003e-05, -0.3524e-05, &
        -0.5567e-05, -0.8108e-05, -0.1127e-04, -0.1547e-04, -0.2138e-04, &
        -0.2996e-04, -0.4251e-04, -0.6059e-04, -0.8563e-04, -0.1190e-03, &
        -0.1623e-03, -0.2156e-03, -0.2767e-03, -0.3403e-03, -0.4006e-03, &
        -0.4530e-03, -0.4912e-03, -0.4995e-03, -0.4563e-03, -0.3592e-03, &
        -0.2383e-03/
      data ((h83(ip,iw),iw=1,31), ip=18,18)/ &
         0.1593e-10,  0.3276e-10,  0.6896e-10,  0.1476e-09,  0.3190e-09, &
         0.6944e-09,  0.1474e-08,  0.2935e-08,  0.5300e-08,  0.8697e-08, &
         0.1336e-07,  0.1946e-07,  0.2707e-07,  0.3637e-07,  0.4800e-07, &
         0.6187e-07,  0.7806e-07,  0.1008e-06,  0.1404e-06,  0.2089e-06, &
         0.3153e-06,  0.4613e-06,  0.6416e-06,  0.8506e-06,  0.1095e-05, &
         0.1387e-05,  0.1708e-05,  0.1956e-05,  0.2003e-05,  0.1836e-05, &
         0.1483e-05/
      data ((h81(ip,iw),iw=1,31), ip=19,19)/ &
         0.99998641,  0.99997300,  0.99994648,  0.99989462,  0.99979430, &
         0.99960452,  0.99925661,  0.99864697,  0.99763900,  0.99607199, &
         0.99376297,  0.99046898,  0.98584002,  0.97937000,  0.97031999, &
         0.95766997,  0.94010001,  0.91588002,  0.88300002,  0.83920002, &
         0.78230000,  0.71060002,  0.62360001,  0.52320004,  0.41450000, &
         0.30589998,  0.20789999,  0.12900001,  0.07239997,  0.03590000, &
         0.01539999/
      data ((h82(ip,iw),iw=1,31), ip=19,19)/ &
        -0.4448e-08, -0.9085e-08, -0.1877e-07, -0.3946e-07, -0.8472e-07, &
        -0.1852e-06, -0.4074e-06, -0.8791e-06, -0.1789e-05, -0.3314e-05, &
        -0.5521e-05, -0.8425e-05, -0.1215e-04, -0.1711e-04, -0.2407e-04, &
        -0.3421e-04, -0.4905e-04, -0.7032e-04, -0.9985e-04, -0.1394e-03, &
        -0.1897e-03, -0.2491e-03, -0.3132e-03, -0.3763e-03, -0.4332e-03, &
        -0.4786e-03, -0.5005e-03, -0.4775e-03, -0.3970e-03, -0.2794e-03, &
        -0.1652e-03/
      data ((h83(ip,iw),iw=1,31), ip=19,19)/ &
         0.1566e-10,  0.3219e-10,  0.6635e-10,  0.1400e-09,  0.2999e-09, &
         0.6513e-09,  0.1406e-08,  0.2953e-08,  0.5789e-08,  0.1037e-07, &
         0.1709e-07,  0.2623e-07,  0.3777e-07,  0.5159e-07,  0.6823e-07, &
         0.8864e-07,  0.1134e-06,  0.1461e-06,  0.1960e-06,  0.2761e-06, &
         0.3962e-06,  0.5583e-06,  0.7580e-06,  0.9957e-06,  0.1282e-05, &
         0.1607e-05,  0.1898e-05,  0.2020e-05,  0.1919e-05,  0.1623e-05, &
         0.1171e-05/
      data ((h81(ip,iw),iw=1,31), ip=20,20)/ &
         0.99998641,  0.99997294,  0.99994624,  0.99989372,  0.99979132, &
         0.99959481,  0.99922693,  0.99856299,  0.99742502,  0.99558598, &
         0.99278802,  0.98872000,  0.98295999,  0.97491002,  0.96368998, &
         0.94812000,  0.92662001,  0.89719999,  0.85780001,  0.80599999, &
         0.73969996,  0.65779996,  0.56130004,  0.45410001,  0.34369999, &
         0.24030000,  0.15390003,  0.08950001,  0.04640001,  0.02090001, &
         0.00800002/
      data ((h82(ip,iw),iw=1,31), ip=20,20)/ &
        -0.4403e-08, -0.8896e-08, -0.1818e-07, -0.3751e-07, -0.7880e-07, &
        -0.1683e-06, -0.3640e-06, -0.7852e-06, -0.1640e-05, -0.3191e-05, &
        -0.5634e-05, -0.9046e-05, -0.1355e-04, -0.1953e-04, -0.2786e-04, &
        -0.3995e-04, -0.5752e-04, -0.8256e-04, -0.1174e-03, -0.1638e-03, &
        -0.2211e-03, -0.2854e-03, -0.3507e-03, -0.4116e-03, -0.4633e-03, &
        -0.4966e-03, -0.4921e-03, -0.4309e-03, -0.3215e-03, -0.2016e-03, &
        -0.1061e-03/
      data ((h83(ip,iw),iw=1,31), ip=20,20)/ &
         0.1551e-10,  0.3147e-10,  0.6419e-10,  0.1356e-09,  0.2860e-09, &
         0.6178e-09,  0.1353e-08,  0.2934e-08,  0.6095e-08,  0.1174e-07, &
         0.2067e-07,  0.3346e-07,  0.5014e-07,  0.7024e-07,  0.9377e-07, &
         0.1226e-06,  0.1592e-06,  0.2056e-06,  0.2678e-06,  0.3584e-06, &
         0.4892e-06,  0.6651e-06,  0.8859e-06,  0.1160e-05,  0.1488e-05, &
         0.1814e-05,  0.2010e-05,  0.1984e-05,  0.1748e-05,  0.1338e-05, &
         0.8445e-06/
      data ((h81(ip,iw),iw=1,31), ip=21,21)/ &
         0.99998641,  0.99997288,  0.99994606,  0.99989301,  0.99978900, &
         0.99958712,  0.99920273,  0.99849200,  0.99723101,  0.99511403, &
         0.99177098,  0.98677999,  0.97962999,  0.96961999,  0.95573002, &
         0.93658000,  0.91036999,  0.87500000,  0.82800001,  0.76730001, &
         0.69110000,  0.59930003,  0.49479997,  0.38370001,  0.27590001, &
         0.18210000,  0.10949999,  0.05919999,  0.02800000,  0.01130003, &
         0.00389999/
      data ((h82(ip,iw),iw=1,31), ip=21,21)/ &
        -0.4379e-08, -0.8801e-08, -0.1782e-07, -0.3642e-07, -0.7536e-07, &
        -0.1581e-06, -0.3366e-06, -0.7227e-06, -0.1532e-05, -0.3106e-05, &
        -0.5810e-05, -0.9862e-05, -0.1540e-04, -0.2279e-04, -0.3292e-04, &
        -0.4738e-04, -0.6817e-04, -0.9765e-04, -0.1384e-03, -0.1918e-03, &
        -0.2551e-03, -0.3226e-03, -0.3876e-03, -0.4452e-03, -0.4883e-03, &
        -0.5005e-03, -0.4598e-03, -0.3633e-03, -0.2416e-03, -0.1349e-03, &
        -0.6278e-04/
      data ((h83(ip,iw),iw=1,31), ip=21,21)/ &
         0.1542e-10,  0.3111e-10,  0.6345e-10,  0.1310e-09,  0.2742e-09, &
         0.5902e-09,  0.1289e-08,  0.2826e-08,  0.6103e-08,  0.1250e-07, &
         0.2355e-07,  0.4041e-07,  0.6347e-07,  0.9217e-07,  0.1256e-06, &
         0.1658e-06,  0.2175e-06,  0.2824e-06,  0.3607e-06,  0.4614e-06, &
         0.6004e-06,  0.7880e-06,  0.1034e-05,  0.1349e-05,  0.1698e-05, &
         0.1965e-05,  0.2021e-05,  0.1857e-05,  0.1500e-05,  0.1015e-05, &
         0.5467e-06/
      data ((h81(ip,iw),iw=1,31), ip=22,22)/ &
         0.99998635,  0.99997288,  0.99994594,  0.99989259,  0.99978727, &
         0.99958128,  0.99918407,  0.99843502,  0.99706697,  0.99468601, &
         0.99077803,  0.98474997,  0.97593999,  0.96350998,  0.94633001, &
         0.92282999,  0.89100003,  0.84860003,  0.79330003,  0.72299999, &
         0.63670003,  0.53600001,  0.42580003,  0.31470001,  0.21410000, &
         0.13300002,  0.07470000,  0.03710002,  0.01580000,  0.00580001, &
         0.00169998/
      data ((h82(ip,iw),iw=1,31), ip=22,22)/ &
        -0.4366e-08, -0.8749e-08, -0.1761e-07, -0.3578e-07, -0.7322e-07, &
        -0.1517e-06, -0.3189e-06, -0.6785e-06, -0.1446e-05, -0.3014e-05, &
        -0.5933e-05, -0.1069e-04, -0.1755e-04, -0.2683e-04, -0.3936e-04, &
        -0.5675e-04, -0.8137e-04, -0.1160e-03, -0.1630e-03, -0.2223e-03, &
        -0.2899e-03, -0.3589e-03, -0.4230e-03, -0.4755e-03, -0.5031e-03, &
        -0.4834e-03, -0.4036e-03, -0.2849e-03, -0.1687e-03, -0.8356e-04, &
        -0.3388e-04/
      data ((h83(ip,iw),iw=1,31), ip=22,22)/ &
         0.1536e-10,  0.3086e-10,  0.6248e-10,  0.1288e-09,  0.2664e-09, &
         0.5637e-09,  0.1222e-08,  0.2680e-08,  0.5899e-08,  0.1262e-07, &
         0.2527e-07,  0.4621e-07,  0.7678e-07,  0.1165e-06,  0.1640e-06, &
         0.2199e-06,  0.2904e-06,  0.3783e-06,  0.4787e-06,  0.5925e-06, &
         0.7377e-06,  0.9389e-06,  0.1216e-05,  0.1560e-05,  0.1879e-05, &
         0.2025e-05,  0.1940e-05,  0.1650e-05,  0.1194e-05,  0.6981e-06, &
         0.3103e-06/
      data ((h81(ip,iw),iw=1,31), ip=23,23)/ &
         0.99998635,  0.99997282,  0.99994588,  0.99989229,  0.99978608, &
         0.99957722,  0.99917048,  0.99839097,  0.99693698,  0.99432403, &
         0.98987001,  0.98273998,  0.97201002,  0.95668000,  0.93548000, &
         0.90671998,  0.86830002,  0.81800002,  0.75330001,  0.67299998, &
         0.57720000,  0.46920002,  0.35659999,  0.25010002,  0.16049999, &
         0.09350002,  0.04850000,  0.02179998,  0.00840002,  0.00269997, &
         0.00070000/
      data ((h82(ip,iw),iw=1,31), ip=23,23)/ &
        -0.4359e-08, -0.8720e-08, -0.1749e-07, -0.3527e-07, -0.7175e-07, &
        -0.1473e-06, -0.3062e-06, -0.6451e-06, -0.1372e-05, -0.2902e-05, &
        -0.5936e-05, -0.1133e-04, -0.1971e-04, -0.3143e-04, -0.4715e-04, &
        -0.6833e-04, -0.9759e-04, -0.1379e-03, -0.1907e-03, -0.2542e-03, &
        -0.3239e-03, -0.3935e-03, -0.4559e-03, -0.4991e-03, -0.5009e-03, &
        -0.4414e-03, -0.3306e-03, -0.2077e-03, -0.1093e-03, -0.4754e-04, &
        -0.1642e-04/
      data ((h83(ip,iw),iw=1,31), ip=23,23)/ &
         0.1531e-10,  0.3070e-10,  0.6184e-10,  0.1257e-09,  0.2578e-09, &
         0.5451e-09,  0.1159e-08,  0.2526e-08,  0.5585e-08,  0.1225e-07, &
         0.2576e-07,  0.5017e-07,  0.8855e-07,  0.1417e-06,  0.2078e-06, &
         0.2858e-06,  0.3802e-06,  0.4946e-06,  0.6226e-06,  0.7572e-06, &
         0.9137e-06,  0.1133e-05,  0.1438e-05,  0.1772e-05,  0.1994e-05, &
         0.1994e-05,  0.1779e-05,  0.1375e-05,  0.8711e-06,  0.4273e-06, &
         0.1539e-06/
      data ((h81(ip,iw),iw=1,31), ip=24,24)/ &
         0.99998635,  0.99997282,  0.99994582,  0.99989212,  0.99978542, &
         0.99957442,  0.99916071,  0.99835902,  0.99683702,  0.99403203, &
         0.98908001,  0.98084998,  0.96805000,  0.94933999,  0.92330998, &
         0.88830000,  0.84219998,  0.78270000,  0.70809996,  0.61759996, &
         0.51330000,  0.40079999,  0.29000002,  0.19239998,  0.11619997, &
         0.06300002,  0.02980000,  0.01200002,  0.00410002,  0.00120002, &
         0.00019997/
      data ((h82(ip,iw),iw=1,31), ip=24,24)/ &
        -0.4354e-08, -0.8703e-08, -0.1742e-07, -0.3499e-07, -0.7074e-07, &
        -0.1441e-06, -0.2971e-06, -0.6195e-06, -0.1309e-05, -0.2780e-05, &
        -0.5823e-05, -0.1165e-04, -0.2152e-04, -0.3616e-04, -0.5604e-04, &
        -0.8230e-04, -0.1173e-03, -0.1635e-03, -0.2211e-03, -0.2868e-03, &
        -0.3567e-03, -0.4260e-03, -0.4844e-03, -0.5097e-03, -0.4750e-03, &
        -0.3779e-03, -0.2522e-03, -0.1409e-03, -0.6540e-04, -0.2449e-04, &
        -0.6948e-05/
      data ((h83(ip,iw),iw=1,31), ip=24,24)/ &
         0.1529e-10,  0.3060e-10,  0.6142e-10,  0.1241e-09,  0.2535e-09, &
         0.5259e-09,  0.1107e-08,  0.2383e-08,  0.5243e-08,  0.1161e-07, &
         0.2523e-07,  0.5188e-07,  0.9757e-07,  0.1657e-06,  0.2553e-06, &
         0.3629e-06,  0.4878e-06,  0.6323e-06,  0.7923e-06,  0.9575e-06, &
         0.1139e-05,  0.1381e-05,  0.1687e-05,  0.1952e-05,  0.2029e-05, &
         0.1890e-05,  0.1552e-05,  0.1062e-05,  0.5728e-06,  0.2280e-06, &
         0.6762e-07/
      data ((h81(ip,iw),iw=1,31), ip=25,25)/ &
         0.99998635,  0.99997282,  0.99994582,  0.99989200,  0.99978489, &
         0.99957252,  0.99915391,  0.99833602,  0.99676299,  0.99380499, &
         0.98843998,  0.97920001,  0.96427000,  0.94182003,  0.91018999, &
         0.86769998,  0.81260002,  0.74300003,  0.65770000,  0.55750000, &
         0.44660002,  0.33310002,  0.22860003,  0.14319998,  0.08090001, &
         0.04030001,  0.01719999,  0.00620002,  0.00190002,  0.00040001, &
         0.00000000/
      data ((h82(ip,iw),iw=1,31), ip=25,25)/ &
        -0.4352e-08, -0.8693e-08, -0.1738e-07, -0.3483e-07, -0.7006e-07, &
        -0.1423e-06, -0.2905e-06, -0.6008e-06, -0.1258e-05, -0.2663e-05, &
        -0.5638e-05, -0.1165e-04, -0.2270e-04, -0.4044e-04, -0.6554e-04, &
        -0.9855e-04, -0.1407e-03, -0.1928e-03, -0.2534e-03, -0.3197e-03, &
        -0.3890e-03, -0.4563e-03, -0.5040e-03, -0.4998e-03, -0.4249e-03, &
        -0.3025e-03, -0.1794e-03, -0.8860e-04, -0.3575e-04, -0.1122e-04, &
        -0.2506e-05/
      data ((h83(ip,iw),iw=1,31), ip=25,25)/ &
         0.1527e-10,  0.3053e-10,  0.6115e-10,  0.1230e-09,  0.2492e-09, &
         0.5149e-09,  0.1068e-08,  0.2268e-08,  0.4932e-08,  0.1089e-07, &
         0.2408e-07,  0.5156e-07,  0.1028e-06,  0.1859e-06,  0.3028e-06, &
         0.4476e-06,  0.6124e-06,  0.7932e-06,  0.9879e-06,  0.1194e-05, &
         0.1417e-05,  0.1673e-05,  0.1929e-05,  0.2064e-05,  0.1997e-05, &
         0.1725e-05,  0.1267e-05,  0.7464e-06,  0.3312e-06,  0.1066e-06, &
         0.2718e-07/
      data ((h81(ip,iw),iw=1,31), ip=26,26)/ &
         0.99998635,  0.99997282,  0.99994576,  0.99989188,  0.99978459, &
         0.99957132,  0.99914938,  0.99831998,  0.99670899,  0.99363601, &
         0.98794001,  0.97781998,  0.96087998,  0.93456000,  0.89670002, &
         0.84560001,  0.78020000,  0.69920003,  0.60299999,  0.49400002, &
         0.37910002,  0.26889998,  0.17460001,  0.10280001,  0.05379999, &
         0.02429998,  0.00929999,  0.00300002,  0.00080001,  0.00010002, &
         0.00000000/
      data ((h82(ip,iw),iw=1,31), ip=26,26)/ &
        -0.4351e-08, -0.8688e-08, -0.1736e-07, -0.3473e-07, -0.6966e-07, &
        -0.1405e-06, -0.2857e-06, -0.5867e-06, -0.1218e-05, -0.2563e-05, &
        -0.5435e-05, -0.1144e-04, -0.2321e-04, -0.4379e-04, -0.7487e-04, &
        -0.1163e-03, -0.1670e-03, -0.2250e-03, -0.2876e-03, -0.3535e-03, &
        -0.4215e-03, -0.4826e-03, -0.5082e-03, -0.4649e-03, -0.3564e-03, &
        -0.2264e-03, -0.1188e-03, -0.5128e-04, -0.1758e-04, -0.4431e-05, &
        -0.7275e-06/
      data ((h83(ip,iw),iw=1,31), ip=26,26)/ &
         0.1525e-10,  0.3048e-10,  0.6097e-10,  0.1223e-09,  0.2466e-09, &
         0.5021e-09,  0.1032e-08,  0.2195e-08,  0.4688e-08,  0.1027e-07, &
         0.2279e-07,  0.4999e-07,  0.1046e-06,  0.2009e-06,  0.3460e-06, &
         0.5335e-06,  0.7478e-06,  0.9767e-06,  0.1216e-05,  0.1469e-05, &
         0.1735e-05,  0.1977e-05,  0.2121e-05,  0.2103e-05,  0.1902e-05, &
         0.1495e-05,  0.9541e-06,  0.4681e-06,  0.1672e-06,  0.4496e-07, &
         0.9859e-08/
      data ((c1(ip,iw),iw=1,30), ip= 1, 1)/ &
         0.99985647,  0.99976432,  0.99963892,  0.99948031,  0.99927652, &
         0.99899602,  0.99860001,  0.99804801,  0.99732202,  0.99640399, &
         0.99526399,  0.99384302,  0.99204999,  0.98979002,  0.98694998, &
         0.98334998,  0.97878999,  0.97307003,  0.96592999,  0.95722002, &
         0.94660002,  0.93366003,  0.91777998,  0.89819998,  0.87419999, &
         0.84500003,  0.81029999,  0.76989996,  0.72440004,  0.67490000/
      data ((c2(ip,iw),iw=1,30), ip= 1, 1)/ &
        -0.1841e-06, -0.4666e-06, -0.1050e-05, -0.2069e-05, -0.3601e-05, &
        -0.5805e-05, -0.8863e-05, -0.1291e-04, -0.1806e-04, -0.2460e-04, &
        -0.3317e-04, -0.4452e-04, -0.5944e-04, -0.7884e-04, -0.1036e-03, &
        -0.1346e-03, -0.1727e-03, -0.2186e-03, -0.2728e-03, -0.3364e-03, &
        -0.4102e-03, -0.4948e-03, -0.5890e-03, -0.6900e-03, -0.7930e-03, &
        -0.8921e-03, -0.9823e-03, -0.1063e-02, -0.1138e-02, -0.1214e-02/
      data ((c3(ip,iw),iw=1,30), ip= 1, 1)/ &
         0.5821e-10,  0.5821e-10, -0.3201e-09, -0.1804e-08, -0.4336e-08, &
        -0.7829e-08, -0.1278e-07, -0.1847e-07, -0.2827e-07, -0.4495e-07, &
        -0.7126e-07, -0.1071e-06, -0.1524e-06, -0.2160e-06, -0.3014e-06, &
        -0.4097e-06, -0.5349e-06, -0.6718e-06, -0.8125e-06, -0.9755e-06, &
        -0.1157e-05, -0.1339e-05, -0.1492e-05, -0.1563e-05, -0.1485e-05, &
        -0.1210e-05, -0.7280e-06, -0.1107e-06,  0.5369e-06,  0.1154e-05/
      data ((c1(ip,iw),iw=1,30), ip= 2, 2)/ &
         0.99985647,  0.99976432,  0.99963868,  0.99947977,  0.99927580, &
         0.99899501,  0.99859601,  0.99804401,  0.99731201,  0.99638498, &
         0.99523097,  0.99378198,  0.99194402,  0.98961002,  0.98664999, &
         0.98286998,  0.97807002,  0.97200000,  0.96439999,  0.95503998, &
         0.94352001,  0.92931998,  0.91175002,  0.88989997,  0.86300004, &
         0.83039999,  0.79159999,  0.74710000,  0.69790000,  0.64579999/
      data ((c2(ip,iw),iw=1,30), ip= 2, 2)/ &
        -0.1831e-06, -0.4642e-06, -0.1048e-05, -0.2067e-05, -0.3596e-05, &
        -0.5797e-05, -0.8851e-05, -0.1289e-04, -0.1802e-04, -0.2454e-04, &
        -0.3307e-04, -0.4435e-04, -0.5916e-04, -0.7842e-04, -0.1031e-03, &
        -0.1342e-03, -0.1725e-03, -0.2189e-03, -0.2739e-03, -0.3386e-03, &
        -0.4138e-03, -0.5003e-03, -0.5968e-03, -0.7007e-03, -0.8076e-03, &
        -0.9113e-03, -0.1007e-02, -0.1096e-02, -0.1181e-02, -0.1271e-02/
      data ((c3(ip,iw),iw=1,30), ip= 2, 2)/ &
         0.5821e-10,  0.5821e-10, -0.3347e-09, -0.1746e-08, -0.4366e-08, &
        -0.7858e-08, -0.1262e-07, -0.1866e-07, -0.2849e-07, -0.4524e-07, &
        -0.7176e-07, -0.1077e-06, -0.1531e-06, -0.2166e-06, -0.3018e-06, &
        -0.4090e-06, -0.5327e-06, -0.6670e-06, -0.8088e-06, -0.9714e-06, &
        -0.1151e-05, -0.1333e-05, -0.1483e-05, -0.1548e-05, -0.1467e-05, &
        -0.1192e-05, -0.7159e-06, -0.1032e-06,  0.5571e-06,  0.1217e-05/
      data ((c1(ip,iw),iw=1,30), ip= 3, 3)/ &
         0.99985671,  0.99976432,  0.99963838,  0.99947912,  0.99927449, &
         0.99899203,  0.99859202,  0.99803501,  0.99729699,  0.99635702, &
         0.99518001,  0.99369103,  0.99178600,  0.98935002,  0.98623002, &
         0.98223001,  0.97711003,  0.97060001,  0.96243000,  0.95222998, &
         0.93957001,  0.92379999,  0.90411001,  0.87959999,  0.84930003, &
         0.81270003,  0.76980001,  0.72140002,  0.66909999,  0.61539996/
      data ((c2(ip,iw),iw=1,30), ip= 3, 3)/ &
        -0.1831e-06, -0.4623e-06, -0.1048e-05, -0.2065e-05, -0.3589e-05, &
        -0.5789e-05, -0.8833e-05, -0.1286e-04, -0.1797e-04, -0.2446e-04, &
        -0.3292e-04, -0.4412e-04, -0.5880e-04, -0.7795e-04, -0.1027e-03, &
        -0.1340e-03, -0.1728e-03, -0.2199e-03, -0.2759e-03, -0.3419e-03, &
        -0.4194e-03, -0.5081e-03, -0.6078e-03, -0.7156e-03, -0.8270e-03, &
        -0.9365e-03, -0.1040e-02, -0.1137e-02, -0.1235e-02, -0.1339e-02/
      data ((c3(ip,iw),iw=1,30), ip= 3, 3)/ &
         0.2910e-10,  0.5821e-10, -0.3201e-09, -0.1732e-08, -0.4307e-08, &
        -0.7843e-08, -0.1270e-07, -0.1882e-07, -0.2862e-07, -0.4571e-07, &
        -0.7225e-07, -0.1082e-06, -0.1535e-06, -0.2171e-06, -0.3021e-06, &
        -0.4084e-06, -0.5302e-06, -0.6615e-06, -0.8059e-06, -0.9668e-06, &
        -0.1146e-05, -0.1325e-05, -0.1468e-05, -0.1530e-05, -0.1448e-05, &
        -0.1168e-05, -0.6907e-06, -0.7148e-07,  0.6242e-06,  0.1357e-05/
      data ((c1(ip,iw),iw=1,30), ip= 4, 4)/ &
         0.99985629,  0.99976349,  0.99963838,  0.99947798,  0.99927282, &
         0.99898797,  0.99858499,  0.99802202,  0.99727303,  0.99631298, &
         0.99510002,  0.99355298,  0.99155599,  0.98898000,  0.98566002, &
         0.98136997,  0.97584999,  0.96880001,  0.95986998,  0.94862998, &
         0.93452001,  0.91681999,  0.89459997,  0.86680001,  0.83270001, &
         0.79189998,  0.74479997,  0.69290000,  0.63839996,  0.58410001/
      data ((c2(ip,iw),iw=1,30), ip= 4, 4)/ &
        -0.1808e-06, -0.4642e-06, -0.1045e-05, -0.2058e-05, -0.3581e-05, &
        -0.5776e-05, -0.8801e-05, -0.1281e-04, -0.1789e-04, -0.2433e-04, &
        -0.3273e-04, -0.4382e-04, -0.5840e-04, -0.7755e-04, -0.1024e-03, &
        -0.1342e-03, -0.1737e-03, -0.2217e-03, -0.2791e-03, -0.3473e-03, &
        -0.4272e-03, -0.5191e-03, -0.6227e-03, -0.7354e-03, -0.8526e-03, &
        -0.9688e-03, -0.1081e-02, -0.1189e-02, -0.1300e-02, -0.1417e-02/
      data ((c3(ip,iw),iw=1,30), ip= 4, 4)/ &
         0.1019e-09,  0.1601e-09, -0.4075e-09, -0.1746e-08, -0.4366e-08, &
        -0.7960e-08, -0.1294e-07, -0.1898e-07, -0.2899e-07, -0.4594e-07, &
        -0.7267e-07, -0.1088e-06, -0.1536e-06, -0.2164e-06, -0.3002e-06, &
        -0.4055e-06, -0.5260e-06, -0.6571e-06, -0.8022e-06, -0.9624e-06, &
        -0.1139e-05, -0.1315e-05, -0.1456e-05, -0.1512e-05, -0.1420e-05, &
        -0.1137e-05, -0.6483e-06,  0.6679e-08,  0.7652e-06,  0.1574e-05/
      data ((c1(ip,iw),iw=1,30), ip= 5, 5)/ &
         0.99985641,  0.99976403,  0.99963748,  0.99947661,  0.99926913, &
         0.99898303,  0.99857402,  0.99800003,  0.99723399,  0.99624503, &
         0.99498397,  0.99335301,  0.99123502,  0.98847997,  0.98488998, &
         0.98023999,  0.97421998,  0.96648002,  0.95659000,  0.94404000, &
         0.92815000,  0.90802002,  0.88270003,  0.85119998,  0.81290001, &
         0.76770002,  0.71679997,  0.66219997,  0.60670000,  0.55250001/
      data ((c2(ip,iw),iw=1,30), ip= 5, 5)/ &
        -0.1827e-06, -0.4608e-06, -0.1042e-05, -0.2053e-05, -0.3565e-05, &
        -0.5745e-05, -0.8758e-05, -0.1273e-04, -0.1778e-04, -0.2417e-04, &
        -0.3250e-04, -0.4347e-04, -0.5801e-04, -0.7729e-04, -0.1025e-03, &
        -0.1349e-03, -0.1755e-03, -0.2249e-03, -0.2842e-03, -0.3549e-03, &
        -0.4380e-03, -0.5340e-03, -0.6428e-03, -0.7613e-03, -0.8854e-03, &
        -0.1009e-02, -0.1131e-02, -0.1252e-02, -0.1376e-02, -0.1502e-02/
      data ((c3(ip,iw),iw=1,30), ip= 5, 5)/ &
         0.4366e-10, -0.1455e-10, -0.4075e-09, -0.1804e-08, -0.4293e-08, &
        -0.8178e-08, -0.1301e-07, -0.1915e-07, -0.2938e-07, -0.4664e-07, &
        -0.7365e-07, -0.1090e-06, -0.1539e-06, -0.2158e-06, -0.2992e-06, &
        -0.4033e-06, -0.5230e-06, -0.6537e-06, -0.7976e-06, -0.9601e-06, &
        -0.1135e-05, -0.1305e-05, -0.1440e-05, -0.1490e-05, -0.1389e-05, &
        -0.1087e-05, -0.5646e-06,  0.1475e-06,  0.9852e-06,  0.1853e-05/
      data ((c1(ip,iw),iw=1,30), ip= 6, 6)/ &
         0.99985617,  0.99976331,  0.99963629,  0.99947429,  0.99926388, &
         0.99897301,  0.99855602,  0.99796802,  0.99717802,  0.99614400, &
         0.99480897,  0.99306899,  0.99078500,  0.98778999,  0.98387998, &
         0.97876000,  0.97211999,  0.96350002,  0.95240998,  0.93821001, &
         0.92009002,  0.89709997,  0.86820000,  0.83249998,  0.78970003, &
         0.74039996,  0.68630004,  0.63010001,  0.57459998,  0.52069998/
      data ((c2(ip,iw),iw=1,30), ip= 6, 6)/ &
        -0.1798e-06, -0.4580e-06, -0.1033e-05, -0.2039e-05, -0.3544e-05, &
        -0.5709e-05, -0.8696e-05, -0.1264e-04, -0.1763e-04, -0.2395e-04, &
        -0.3220e-04, -0.4311e-04, -0.5777e-04, -0.7732e-04, -0.1032e-03, &
        -0.1365e-03, -0.1784e-03, -0.2295e-03, -0.2914e-03, -0.3653e-03, &
        -0.4527e-03, -0.5541e-03, -0.6689e-03, -0.7947e-03, -0.9265e-03, &
        -0.1060e-02, -0.1192e-02, -0.1326e-02, -0.1460e-02, -0.1586e-02/
      data ((c3(ip,iw),iw=1,30), ip= 6, 6)/ &
         0.8731e-10,  0.0000e+00, -0.3492e-09, -0.1892e-08, -0.4322e-08, &
        -0.8367e-08, -0.1318e-07, -0.1962e-07, -0.3024e-07, -0.4708e-07, &
        -0.7359e-07, -0.1087e-06, -0.1534e-06, -0.2152e-06, -0.2978e-06, &
        -0.4008e-06, -0.5207e-06, -0.6509e-06, -0.7968e-06, -0.9584e-06, &
        -0.1128e-05, -0.1297e-05, -0.1425e-05, -0.1461e-05, -0.1342e-05, &
        -0.1009e-05, -0.4283e-06,  0.3666e-06,  0.1272e-05,  0.2171e-05/
      data ((c1(ip,iw),iw=1,30), ip= 7, 7)/ &
         0.99985600,  0.99976230,  0.99963462,  0.99947017,  0.99925607, &
         0.99895698,  0.99852800,  0.99791902,  0.99709100,  0.99599499, &
         0.99456000,  0.99267203,  0.99017102,  0.98688000,  0.98255002, &
         0.97685999,  0.96941000,  0.95969999,  0.94709998,  0.93085998, &
         0.91001999,  0.88360000,  0.85060000,  0.81040001,  0.76319999, &
         0.71029997,  0.65400004,  0.59740001,  0.54229999,  0.48839998/
      data ((c2(ip,iw),iw=1,30), ip= 7, 7)/ &
        -0.1784e-06, -0.4551e-06, -0.1023e-05, -0.2019e-05, -0.3507e-05, &
        -0.5651e-05, -0.8608e-05, -0.1250e-04, -0.1744e-04, -0.2370e-04, &
        -0.3189e-04, -0.4289e-04, -0.5777e-04, -0.7787e-04, -0.1045e-03, &
        -0.1392e-03, -0.1828e-03, -0.2365e-03, -0.3015e-03, -0.3797e-03, &
        -0.4723e-03, -0.5803e-03, -0.7026e-03, -0.8365e-03, -0.9772e-03, &
        -0.1120e-02, -0.1265e-02, -0.1409e-02, -0.1547e-02, -0.1665e-02/
      data ((c3(ip,iw),iw=1,30), ip= 7, 7)/ &
         0.5821e-10,  0.8731e-10, -0.4366e-09, -0.1935e-08, -0.4555e-08, &
        -0.8455e-08, -0.1356e-07, -0.2024e-07, -0.3079e-07, -0.4758e-07, &
        -0.7352e-07, -0.1078e-06, -0.1520e-06, -0.2139e-06, -0.2964e-06, &
        -0.3997e-06, -0.5185e-06, -0.6493e-06, -0.7943e-06, -0.9568e-06, &
        -0.1127e-05, -0.1288e-05, -0.1405e-05, -0.1425e-05, -0.1275e-05, &
        -0.8809e-06, -0.2158e-06,  0.6597e-06,  0.1610e-05,  0.2524e-05/
      data ((c1(ip,iw),iw=1,30), ip= 8, 8)/ &
         0.99985582,  0.99976122,  0.99963123,  0.99946368,  0.99924308, &
         0.99893397,  0.99848598,  0.99784499,  0.99696398,  0.99577999, &
         0.99421299,  0.99212801,  0.98935997,  0.98569000,  0.98083001, &
         0.97442001,  0.96595001,  0.95486999,  0.94040000,  0.92163002, &
         0.89760000,  0.86720002,  0.82969999,  0.78499997,  0.73370004, &
         0.67799997,  0.62070000,  0.56439996,  0.50960004,  0.45539999/
      data ((c2(ip,iw),iw=1,30), ip= 8, 8)/ &
        -0.1760e-06, -0.4451e-06, -0.1004e-05, -0.1989e-05, -0.3457e-05, &
        -0.5574e-05, -0.8470e-05, -0.1230e-04, -0.1721e-04, -0.2344e-04, &
        -0.3168e-04, -0.4286e-04, -0.5815e-04, -0.7898e-04, -0.1070e-03, &
        -0.1434e-03, -0.1892e-03, -0.2460e-03, -0.3152e-03, -0.3985e-03, &
        -0.4981e-03, -0.6139e-03, -0.7448e-03, -0.8878e-03, -0.1038e-02, &
        -0.1193e-02, -0.1348e-02, -0.1499e-02, -0.1631e-02, -0.1735e-02/
      data ((c3(ip,iw),iw=1,30), ip= 8, 8)/ &
        -0.1455e-10,  0.4366e-10, -0.3929e-09, -0.2081e-08, -0.4700e-08, &
        -0.8804e-08, -0.1417e-07, -0.2068e-07, -0.3143e-07, -0.4777e-07, &
        -0.7336e-07, -0.1070e-06, -0.1517e-06, -0.2134e-06, -0.2967e-06, &
        -0.3991e-06, -0.5164e-06, -0.6510e-06, -0.7979e-06, -0.9575e-06, &
        -0.1123e-05, -0.1279e-05, -0.1382e-05, -0.1374e-05, -0.1166e-05, &
        -0.6893e-06,  0.7339e-07,  0.1013e-05,  0.1982e-05,  0.2896e-05/
      data ((c1(ip,iw),iw=1,30), ip= 9, 9)/ &
         0.99985498,  0.99975908,  0.99962622,  0.99945402,  0.99922228, &
         0.99889803,  0.99842203,  0.99773699,  0.99677801,  0.99547797, &
         0.99373603,  0.99140298,  0.98829001,  0.98413998,  0.97863001, &
         0.97127002,  0.96156001,  0.94875997,  0.93197000,  0.91017997, &
         0.88230002,  0.84749997,  0.80540001,  0.75620002,  0.70159996, &
         0.64429998,  0.58710003,  0.53130001,  0.47640002,  0.42189997/
      data ((c2(ip,iw),iw=1,30), ip= 9, 9)/ &
        -0.1717e-06, -0.4327e-06, -0.9759e-06, -0.1943e-05, -0.3391e-05, &
        -0.5454e-05, -0.8297e-05, -0.1209e-04, -0.1697e-04, -0.2322e-04, &
        -0.3163e-04, -0.4318e-04, -0.5910e-04, -0.8111e-04, -0.1108e-03, &
        -0.1493e-03, -0.1982e-03, -0.2588e-03, -0.3333e-03, -0.4237e-03, &
        -0.5312e-03, -0.6562e-03, -0.7968e-03, -0.9496e-03, -0.1110e-02, &
        -0.1276e-02, -0.1439e-02, -0.1588e-02, -0.1708e-02, -0.1796e-02/
      data ((c3(ip,iw),iw=1,30), ip= 9, 9)/ &
         0.0000e+00,  0.1455e-10, -0.3638e-09, -0.2299e-08, -0.4744e-08, &
        -0.9284e-08, -0.1445e-07, -0.2141e-07, -0.3162e-07, -0.4761e-07, &
        -0.7248e-07, -0.1065e-06, -0.1501e-06, -0.2140e-06, -0.2981e-06, &
        -0.3994e-06, -0.5201e-06, -0.6549e-06, -0.8009e-06, -0.9627e-06, &
        -0.1125e-05, -0.1266e-05, -0.1348e-05, -0.1292e-05, -0.1005e-05, &
        -0.4166e-06,  0.4279e-06,  0.1401e-05,  0.2379e-05,  0.3278e-05/
      data ((c1(ip,iw),iw=1,30), ip=10,10)/ &
         0.99985462,  0.99975640,  0.99961889,  0.99943668,  0.99919188, &
         0.99884301,  0.99832898,  0.99757999,  0.99651998,  0.99506402, &
         0.99309200,  0.99044400,  0.98689002,  0.98215997,  0.97579002, &
         0.96730000,  0.95603001,  0.94110000,  0.92149001,  0.89609998, &
         0.86399996,  0.82449996,  0.77759999,  0.72459996,  0.66769999, &
         0.61000001,  0.55340004,  0.49769998,  0.44250000,  0.38810003/
      data ((c2(ip,iw),iw=1,30), ip=10,10)/ &
        -0.1607e-06, -0.4160e-06, -0.9320e-06, -0.1872e-05, -0.3281e-05, &
        -0.5286e-05, -0.8097e-05, -0.1187e-04, -0.1677e-04, -0.2320e-04, &
        -0.3190e-04, -0.4402e-04, -0.6081e-04, -0.8441e-04, -0.1162e-03, &
        -0.1576e-03, -0.2102e-03, -0.2760e-03, -0.3571e-03, -0.4558e-03, &
        -0.5730e-03, -0.7082e-03, -0.8591e-03, -0.1022e-02, -0.1194e-02, &
        -0.1368e-02, -0.1533e-02, -0.1671e-02, -0.1775e-02, -0.1843e-02/
      data ((c3(ip,iw),iw=1,30), ip=10,10)/ &
        -0.1164e-09, -0.7276e-10, -0.5530e-09, -0.2270e-08, -0.5093e-08, &
        -0.9517e-08, -0.1502e-07, -0.2219e-07, -0.3171e-07, -0.4712e-07, &
        -0.7123e-07, -0.1042e-06, -0.1493e-06, -0.2156e-06, -0.2999e-06, &
        -0.4027e-06, -0.5243e-06, -0.6616e-06, -0.8125e-06, -0.9691e-06, &
        -0.1126e-05, -0.1251e-05, -0.1294e-05, -0.1163e-05, -0.7639e-06, &
        -0.7395e-07,  0.8279e-06,  0.1819e-05,  0.2795e-05,  0.3647e-05/
      data ((c1(ip,iw),iw=1,30), ip=11,11)/ &
         0.99985212,  0.99975210,  0.99960798,  0.99941242,  0.99914628, &
         0.99876302,  0.99819702,  0.99736100,  0.99616700,  0.99450397, &
         0.99225003,  0.98920000,  0.98510998,  0.97961998,  0.97220999, &
         0.96231002,  0.94909000,  0.93155003,  0.90856999,  0.87910002, &
         0.84219998,  0.79790002,  0.74669999,  0.69080001,  0.63300002, &
         0.57570004,  0.51950002,  0.46359998,  0.40829998,  0.35450000/
      data ((c2(ip,iw),iw=1,30), ip=11,11)/ &
        -0.1531e-06, -0.3864e-06, -0.8804e-06, -0.1776e-05, -0.3131e-05, &
        -0.5082e-05, -0.7849e-05, -0.1164e-04, -0.1669e-04, -0.2340e-04, &
        -0.3261e-04, -0.4546e-04, -0.6380e-04, -0.8932e-04, -0.1237e-03, &
        -0.1687e-03, -0.2262e-03, -0.2984e-03, -0.3880e-03, -0.4964e-03, &
        -0.6244e-03, -0.7705e-03, -0.9325e-03, -0.1107e-02, -0.1288e-02, &
        -0.1466e-02, -0.1623e-02, -0.1746e-02, -0.1831e-02, -0.1875e-02/
      data ((c3(ip,iw),iw=1,30), ip=11,11)/ &
         0.1019e-09, -0.2037e-09, -0.8004e-09, -0.2387e-08, -0.5326e-08, &
        -0.9764e-08, -0.1576e-07, -0.2256e-07, -0.3180e-07, -0.4616e-07, &
        -0.7026e-07, -0.1031e-06, -0.1520e-06, -0.2181e-06, -0.3037e-06, &
        -0.4109e-06, -0.5354e-06, -0.6740e-06, -0.8241e-06, -0.9810e-06, &
        -0.1126e-05, -0.1221e-05, -0.1200e-05, -0.9678e-06, -0.4500e-06, &
         0.3236e-06,  0.1256e-05,  0.2259e-05,  0.3206e-05,  0.3978e-05/
      data ((c1(ip,iw),iw=1,30), ip=12,12)/ &
         0.99985027,  0.99974507,  0.99959022,  0.99937689,  0.99907988, &
         0.99865198,  0.99801201,  0.99706602,  0.99569201,  0.99377203, &
         0.99115402,  0.98762000,  0.98286003,  0.97640002,  0.96771997, &
         0.95604998,  0.94045001,  0.91979003,  0.89289999,  0.85879999, &
         0.81700003,  0.76800001,  0.71340001,  0.65579998,  0.59810001, &
         0.54139996,  0.48519999,  0.42909998,  0.37410003,  0.32190001/
      data ((c2(ip,iw),iw=1,30), ip=12,12)/ &
        -0.1340e-06, -0.3478e-06, -0.8189e-06, -0.1653e-05, -0.2944e-05, &
        -0.4852e-05, -0.7603e-05, -0.1150e-04, -0.1682e-04, -0.2400e-04, &
        -0.3390e-04, -0.4799e-04, -0.6807e-04, -0.9596e-04, -0.1338e-03, &
        -0.1833e-03, -0.2471e-03, -0.3275e-03, -0.4268e-03, -0.5466e-03, &
        -0.6862e-03, -0.8439e-03, -0.1017e-02, -0.1201e-02, -0.1389e-02, &
        -0.1563e-02, -0.1706e-02, -0.1809e-02, -0.1872e-02, -0.1890e-02/
      data ((c3(ip,iw),iw=1,30), ip=12,12)/ &
        -0.1455e-10, -0.1892e-09, -0.8295e-09, -0.2547e-08, -0.5544e-08, &
        -0.1014e-07, -0.1605e-07, -0.2341e-07, -0.3156e-07, -0.4547e-07, &
        -0.6749e-07, -0.1034e-06, -0.1550e-06, -0.2230e-06, -0.3130e-06, &
        -0.4219e-06, -0.5469e-06, -0.6922e-06, -0.8448e-06, -0.9937e-06, &
        -0.1118e-05, -0.1166e-05, -0.1054e-05, -0.6926e-06, -0.7180e-07, &
         0.7515e-06,  0.1709e-05,  0.2703e-05,  0.3593e-05,  0.4232e-05/
      data ((c1(ip,iw),iw=1,30), ip=13,13)/ &
         0.99984729,  0.99973530,  0.99956691,  0.99932659,  0.99898797, &
         0.99849701,  0.99776399,  0.99667102,  0.99507397,  0.99283201, &
         0.98977000,  0.98563999,  0.98001999,  0.97241002,  0.96213001, &
         0.94830000,  0.92980999,  0.90546000,  0.87409997,  0.83510000, &
         0.78850001,  0.73549998,  0.67850000,  0.62049997,  0.56340003, &
         0.50699997,  0.45050001,  0.39450002,  0.34060001,  0.29079998/
      data ((c2(ip,iw),iw=1,30), ip=13,13)/ &
        -0.1163e-06, -0.3048e-06, -0.7186e-06, -0.1495e-05, -0.2726e-05, &
        -0.4588e-05, -0.7396e-05, -0.1152e-04, -0.1725e-04, -0.2514e-04, &
        -0.3599e-04, -0.5172e-04, -0.7403e-04, -0.1051e-03, -0.1469e-03, &
        -0.2023e-03, -0.2735e-03, -0.3637e-03, -0.4746e-03, -0.6067e-03, &
        -0.7586e-03, -0.9281e-03, -0.1112e-02, -0.1304e-02, -0.1491e-02, &
        -0.1653e-02, -0.1777e-02, -0.1860e-02, -0.1896e-02, -0.1891e-02/
      data ((c3(ip,iw),iw=1,30), ip=13,13)/ &
        -0.1455e-09, -0.2765e-09, -0.9750e-09, -0.2794e-08, -0.5413e-08, &
        -0.1048e-07, -0.1625e-07, -0.2344e-07, -0.3105e-07, -0.4304e-07, &
        -0.6608e-07, -0.1057e-06, -0.1587e-06, -0.2308e-06, -0.3235e-06, &
        -0.4373e-06, -0.5687e-06, -0.7156e-06, -0.8684e-06, -0.1007e-05, &
        -0.1094e-05, -0.1062e-05, -0.8273e-06, -0.3485e-06,  0.3463e-06, &
         0.1206e-05,  0.2173e-05,  0.3132e-05,  0.3919e-05,  0.4370e-05/
      data ((c1(ip,iw),iw=1,30), ip=14,14)/ &
         0.99984348,  0.99972272,  0.99953479,  0.99926043,  0.99886698, &
         0.99829400,  0.99744201,  0.99615997,  0.99429500,  0.99166000, &
         0.98806000,  0.98316997,  0.97649997,  0.96748000,  0.95525998, &
         0.93878001,  0.91687000,  0.88830000,  0.85220003,  0.80820000, &
         0.75699997,  0.70099998,  0.64300001,  0.58550000,  0.52890003, &
         0.47219998,  0.41560000,  0.36040002,  0.30849999,  0.26169997/
      data ((c2(ip,iw),iw=1,30), ip=14,14)/ &
        -0.8581e-07, -0.2557e-06, -0.6103e-06, -0.1305e-05, -0.2472e-05, &
        -0.4334e-05, -0.7233e-05, -0.1167e-04, -0.1806e-04, -0.2679e-04, &
        -0.3933e-04, -0.5705e-04, -0.8194e-04, -0.1165e-03, -0.1637e-03, &
        -0.2259e-03, -0.3068e-03, -0.4082e-03, -0.5318e-03, -0.6769e-03, &
        -0.8415e-03, -0.1023e-02, -0.1216e-02, -0.1410e-02, -0.1588e-02, &
        -0.1733e-02, -0.1837e-02, -0.1894e-02, -0.1904e-02, -0.1881e-02/
      data ((c3(ip,iw),iw=1,30), ip=14,14)/ &
        -0.2037e-09, -0.4220e-09, -0.1091e-08, -0.2896e-08, -0.5821e-08, &
        -0.1052e-07, -0.1687e-07, -0.2353e-07, -0.3193e-07, -0.4254e-07, &
        -0.6685e-07, -0.1072e-06, -0.1638e-06, -0.2427e-06, -0.3421e-06, &
        -0.4600e-06, -0.5946e-06, -0.7472e-06, -0.8958e-06, -0.1009e-05, &
        -0.1032e-05, -0.8919e-06, -0.5224e-06,  0.5218e-07,  0.7886e-06, &
         0.1672e-05,  0.2626e-05,  0.3513e-05,  0.4138e-05,  0.4379e-05/
      data ((c1(ip,iw),iw=1,30), ip=15,15)/ &
         0.99983788,  0.99970680,  0.99949580,  0.99917668,  0.99871200, &
         0.99803603,  0.99703097,  0.99552703,  0.99333203,  0.99023402, &
         0.98597997,  0.98013997,  0.97223002,  0.96145999,  0.94686002, &
         0.92727000,  0.90142000,  0.86820000,  0.82700002,  0.77820003, &
         0.72350001,  0.66569996,  0.60769999,  0.55089998,  0.49430001, &
         0.43739998,  0.38110000,  0.32749999,  0.27840000,  0.23479998/
      data ((c2(ip,iw),iw=1,30), ip=15,15)/ &
        -0.8246e-07, -0.2070e-06, -0.4895e-06, -0.1106e-05, -0.2216e-05, &
        -0.4077e-05, -0.7150e-05, -0.1202e-04, -0.1920e-04, -0.2938e-04, &
        -0.4380e-04, -0.6390e-04, -0.9209e-04, -0.1310e-03, -0.1843e-03, &
        -0.2554e-03, -0.3468e-03, -0.4611e-03, -0.5982e-03, -0.7568e-03, &
        -0.9340e-03, -0.1126e-02, -0.1324e-02, -0.1514e-02, -0.1676e-02, &
        -0.1801e-02, -0.1881e-02, -0.1911e-02, -0.1900e-02, -0.1867e-02/
      data ((c3(ip,iw),iw=1,30), ip=15,15)/ &
        -0.1601e-09, -0.3492e-09, -0.1019e-08, -0.2634e-08, -0.5632e-08, &
        -0.1065e-07, -0.1746e-07, -0.2542e-07, -0.3206e-07, -0.4390e-07, &
        -0.6956e-07, -0.1093e-06, -0.1729e-06, -0.2573e-06, -0.3612e-06, &
        -0.4904e-06, -0.6342e-06, -0.7834e-06, -0.9175e-06, -0.9869e-06, &
        -0.9164e-06, -0.6386e-06, -0.1544e-06,  0.4798e-06,  0.1252e-05, &
         0.2137e-05,  0.3043e-05,  0.3796e-05,  0.4211e-05,  0.4332e-05/
      data ((c1(ip,iw),iw=1,30), ip=16,16)/ &
         0.99983227,  0.99968958,  0.99945217,  0.99907941,  0.99852598, &
         0.99772000,  0.99652398,  0.99475902,  0.99218899,  0.98856002, &
         0.98348999,  0.97653997,  0.96708000,  0.95420998,  0.93677002, &
         0.91352999,  0.88330001,  0.84509999,  0.79900002,  0.74599999, &
         0.68879998,  0.63049996,  0.57319999,  0.51660001,  0.45969999, &
         0.40289998,  0.34780002,  0.29650003,  0.25070000,  0.20959997/
      data ((c2(ip,iw),iw=1,30), ip=16,16)/ &
        -0.7004e-07, -0.1592e-06, -0.3936e-06, -0.9145e-06, -0.1958e-05, &
        -0.3850e-05, -0.7093e-05, -0.1252e-04, -0.2066e-04, -0.3271e-04, &
        -0.4951e-04, -0.7268e-04, -0.1045e-03, -0.1487e-03, -0.2092e-03, &
        -0.2899e-03, -0.3936e-03, -0.5215e-03, -0.6729e-03, -0.8454e-03, &
        -0.1035e-02, -0.1235e-02, -0.1432e-02, -0.1608e-02, -0.1751e-02, &
        -0.1854e-02, -0.1907e-02, -0.1913e-02, -0.1888e-02, -0.1857e-02/
      data ((c3(ip,iw),iw=1,30), ip=16,16)/ &
        -0.2328e-09, -0.3347e-09, -0.9750e-09, -0.2314e-08, -0.5166e-08, &
        -0.1052e-07, -0.1726e-07, -0.2605e-07, -0.3532e-07, -0.4949e-07, &
        -0.7229e-07, -0.1133e-06, -0.1799e-06, -0.2725e-06, -0.3881e-06, &
        -0.5249e-06, -0.6763e-06, -0.8227e-06, -0.9279e-06, -0.9205e-06, &
        -0.7228e-06, -0.3109e-06,  0.2583e-06,  0.9390e-06,  0.1726e-05, &
         0.2579e-05,  0.3376e-05,  0.3931e-05,  0.4161e-05,  0.4369e-05/
      data ((c1(ip,iw),iw=1,30), ip=17,17)/ &
         0.99982637,  0.99967217,  0.99940813,  0.99897701,  0.99831802, &
         0.99734300,  0.99592501,  0.99385202,  0.99086499,  0.98659998, &
         0.98057997,  0.97229999,  0.96098000,  0.94555002,  0.92479002, &
         0.89740002,  0.86240000,  0.81919998,  0.76859999,  0.71249998, &
         0.65419996,  0.59630001,  0.53950000,  0.48259997,  0.42549998, &
         0.36940002,  0.31629997,  0.26810002,  0.22520000,  0.18580002/
      data ((c2(ip,iw),iw=1,30), ip=17,17)/ &
        -0.6526e-07, -0.1282e-06, -0.3076e-06, -0.7454e-06, -0.1685e-05, &
        -0.3600e-05, -0.7071e-05, -0.1292e-04, -0.2250e-04, -0.3665e-04, &
        -0.5623e-04, -0.8295e-04, -0.1195e-03, -0.1696e-03, -0.2385e-03, &
        -0.3298e-03, -0.4465e-03, -0.5887e-03, -0.7546e-03, -0.9408e-03, &
        -0.1141e-02, -0.1345e-02, -0.1533e-02, -0.1691e-02, -0.1813e-02, &
        -0.1889e-02, -0.1916e-02, -0.1904e-02, -0.1877e-02, -0.1850e-02/
      data ((c3(ip,iw),iw=1,30), ip=17,17)/ &
        -0.1746e-09, -0.2037e-09, -0.8149e-09, -0.2095e-08, -0.4889e-08, &
        -0.9517e-08, -0.1759e-07, -0.2740e-07, -0.4147e-07, -0.5774e-07, &
        -0.7909e-07, -0.1199e-06, -0.1877e-06, -0.2859e-06, -0.4137e-06, &
        -0.5649e-06, -0.7218e-06, -0.8516e-06, -0.9022e-06, -0.7905e-06, &
        -0.4531e-06,  0.6917e-07,  0.7009e-06,  0.1416e-05,  0.2194e-05, &
         0.2963e-05,  0.3578e-05,  0.3900e-05,  0.4094e-05,  0.4642e-05/
      data ((c1(ip,iw),iw=1,30), ip=18,18)/ &
         0.99982101,  0.99965781,  0.99936712,  0.99887502,  0.99809802, &
         0.99692702,  0.99523401,  0.99281400,  0.98935997,  0.98435003, &
         0.97728002,  0.96740997,  0.95381999,  0.93539000,  0.91082001, &
         0.87889999,  0.83889997,  0.79100001,  0.73660004,  0.67879999, &
         0.62049997,  0.56330001,  0.50629997,  0.44900000,  0.39209998, &
         0.33749998,  0.28729999,  0.24229997,  0.20150000,  0.16280001/
      data ((c2(ip,iw),iw=1,30), ip=18,18)/ &
        -0.6477e-07, -0.1243e-06, -0.2536e-06, -0.6173e-06, -0.1495e-05, &
        -0.3353e-05, -0.6919e-05, -0.1337e-04, -0.2418e-04, -0.4049e-04, &
        -0.6354e-04, -0.9455e-04, -0.1367e-03, -0.1942e-03, -0.2717e-03, &
        -0.3744e-03, -0.5042e-03, -0.6609e-03, -0.8416e-03, -0.1041e-02, &
        -0.1249e-02, -0.1448e-02, -0.1622e-02, -0.1760e-02, -0.1857e-02, &
        -0.1906e-02, -0.1911e-02, -0.1892e-02, -0.1870e-02, -0.1844e-02/
      data ((c3(ip,iw),iw=1,30), ip=18,18)/ &
        -0.5821e-10, -0.2328e-09, -0.6985e-09, -0.1368e-08, -0.4351e-08, &
        -0.8993e-08, -0.1579e-07, -0.2916e-07, -0.4904e-07, -0.7010e-07, &
        -0.9623e-07, -0.1332e-06, -0.1928e-06, -0.2977e-06, -0.4371e-06, &
        -0.5992e-06, -0.7586e-06, -0.8580e-06, -0.8238e-06, -0.5811e-06, &
        -0.1298e-06,  0.4702e-06,  0.1162e-05,  0.1905e-05,  0.2632e-05, &
         0.3247e-05,  0.3609e-05,  0.3772e-05,  0.4166e-05,  0.5232e-05/
      data ((c1(ip,iw),iw=1,30), ip=19,19)/ &
         0.99981648,  0.99964571,  0.99933147,  0.99878597,  0.99787998, &
         0.99649400,  0.99448699,  0.99166602,  0.98762000,  0.98181999, &
         0.97352999,  0.96183002,  0.94558001,  0.92363000,  0.89480001, &
         0.85799998,  0.81309998,  0.76100004,  0.70420003,  0.64590001, &
         0.58840001,  0.53139997,  0.47380000,  0.41619998,  0.36030000, &
         0.30809999,  0.26109999,  0.21880001,  0.17909998,  0.14080000/
      data ((c2(ip,iw),iw=1,30), ip=19,19)/ &
        -0.7906e-07, -0.1291e-06, -0.2430e-06, -0.5145e-06, -0.1327e-05, &
        -0.3103e-05, -0.6710e-05, -0.1371e-04, -0.2561e-04, -0.4405e-04, &
        -0.7051e-04, -0.1070e-03, -0.1560e-03, -0.2217e-03, -0.3090e-03, &
        -0.4228e-03, -0.5657e-03, -0.7371e-03, -0.9322e-03, -0.1142e-02, &
        -0.1352e-02, -0.1541e-02, -0.1697e-02, -0.1813e-02, -0.1883e-02, &
        -0.1906e-02, -0.1898e-02, -0.1882e-02, -0.1866e-02, -0.1832e-02/
      data ((c3(ip,iw),iw=1,30), ip=19,19)/ &
         0.2910e-10,  0.1455e-10, -0.2765e-09, -0.1426e-08, -0.2576e-08, &
        -0.5923e-08, -0.1429e-07, -0.3159e-07, -0.5441e-07, -0.8367e-07, &
        -0.1161e-06, -0.1526e-06, -0.2060e-06, -0.3007e-06, -0.4450e-06, &
        -0.6182e-06, -0.7683e-06, -0.8170e-06, -0.6754e-06, -0.3122e-06, &
         0.2234e-06,  0.8828e-06,  0.1632e-05,  0.2373e-05,  0.3002e-05, &
         0.3384e-05,  0.3499e-05,  0.3697e-05,  0.4517e-05,  0.6117e-05/
      data ((c1(ip,iw),iw=1,30), ip=20,20)/ &
         0.99981302,  0.99963689,  0.99930489,  0.99870700,  0.99768901, &
         0.99608499,  0.99373102,  0.99039900,  0.98566997,  0.97895002, &
         0.96930999,  0.95548999,  0.93621999,  0.91029000,  0.87669998, &
         0.83490002,  0.78549999,  0.73019999,  0.67240000,  0.61469996, &
         0.55779999,  0.50029999,  0.44220001,  0.38489997,  0.33069998, &
         0.28149998,  0.23760003,  0.19690001,  0.15759999,  0.11989999/
      data ((c2(ip,iw),iw=1,30), ip=20,20)/ &
        -0.7762e-07, -0.1319e-06, -0.2315e-06, -0.4780e-06, -0.1187e-05, &
        -0.2750e-05, -0.6545e-05, -0.1393e-04, -0.2645e-04, -0.4652e-04, &
        -0.7657e-04, -0.1190e-03, -0.1766e-03, -0.2520e-03, -0.3499e-03, &
        -0.4751e-03, -0.6307e-03, -0.8160e-03, -0.1024e-02, -0.1240e-02, &
        -0.1443e-02, -0.1619e-02, -0.1757e-02, -0.1849e-02, -0.1892e-02, &
        -0.1896e-02, -0.1886e-02, -0.1878e-02, -0.1861e-02, -0.1807e-02/
      data ((c3(ip,iw),iw=1,30), ip=20,20)/ &
         0.8731e-10, -0.7276e-10, -0.2328e-09, -0.6403e-09, -0.1455e-08, &
        -0.3827e-08, -0.1270e-07, -0.3014e-07, -0.5594e-07, -0.9677e-07, &
        -0.1422e-06, -0.1823e-06, -0.2296e-06, -0.3094e-06, -0.4399e-06, &
        -0.6008e-06, -0.7239e-06, -0.7014e-06, -0.4562e-06, -0.7778e-08, &
         0.5785e-06,  0.1291e-05,  0.2072e-05,  0.2783e-05,  0.3247e-05, &
         0.3358e-05,  0.3364e-05,  0.3847e-05,  0.5194e-05,  0.7206e-05/
      data ((c1(ip,iw),iw=1,30), ip=21,21)/ &
         0.99981070,  0.99962878,  0.99928439,  0.99864298,  0.99752903, &
         0.99573100,  0.99301797,  0.98905998,  0.98354000,  0.97570997, &
         0.96449000,  0.94837999,  0.92576003,  0.89539999,  0.85680002, &
         0.81000000,  0.75660002,  0.69949996,  0.64199996,  0.58529997, &
         0.52829999,  0.47020000,  0.41200000,  0.35570002,  0.30400002, &
         0.25800002,  0.21609998,  0.17610002,  0.13709998,  0.10020000/
      data ((c2(ip,iw),iw=1,30), ip=21,21)/ &
        -0.1010e-06, -0.1533e-06, -0.2347e-06, -0.4535e-06, -0.1029e-05, &
        -0.2530e-05, -0.6335e-05, -0.1381e-04, -0.2681e-04, -0.4777e-04, &
        -0.8083e-04, -0.1296e-03, -0.1966e-03, -0.2836e-03, -0.3937e-03, &
        -0.5313e-03, -0.6995e-03, -0.8972e-03, -0.1113e-02, -0.1327e-02, &
        -0.1520e-02, -0.1681e-02, -0.1800e-02, -0.1867e-02, -0.1887e-02, &
        -0.1884e-02, -0.1881e-02, -0.1879e-02, -0.1849e-02, -0.1764e-02/
      data ((c3(ip,iw),iw=1,30), ip=21,21)/ &
         0.8731e-10,  0.1310e-09, -0.2474e-09, -0.2619e-09,  0.8295e-09, &
        -0.1979e-08, -0.1141e-07, -0.2621e-07, -0.5799e-07, -0.1060e-06, &
        -0.1621e-06, -0.2281e-06, -0.2793e-06, -0.3335e-06, -0.4277e-06, &
        -0.5429e-06, -0.5970e-06, -0.4872e-06, -0.1775e-06,  0.3028e-06, &
         0.9323e-06,  0.1680e-05,  0.2452e-05,  0.3063e-05,  0.3299e-05, &
         0.3219e-05,  0.3369e-05,  0.4332e-05,  0.6152e-05,  0.8413e-05/
      data ((c1(ip,iw),iw=1,30), ip=22,22)/ &
         0.99980962,  0.99962330,  0.99926400,  0.99858999,  0.99741602, &
         0.99547201,  0.99236798,  0.98776001,  0.98124999,  0.97210997, &
         0.95902997,  0.94033003,  0.91415000,  0.87919998,  0.83529997, &
         0.78380001,  0.72749996,  0.66990000,  0.61339998,  0.55720001, &
         0.49980003,  0.44129997,  0.38360000,  0.32929999,  0.28070003, &
         0.23710001,  0.19620001,  0.15619999,  0.11769998,  0.08200002/
      data ((c2(ip,iw),iw=1,30), ip=22,22)/ &
        -0.1258e-06, -0.1605e-06, -0.2581e-06, -0.4286e-06, -0.8321e-06, &
        -0.2392e-05, -0.6163e-05, -0.1358e-04, -0.2646e-04, -0.4792e-04, &
        -0.8284e-04, -0.1369e-03, -0.2138e-03, -0.3141e-03, -0.4393e-03, &
        -0.5917e-03, -0.7731e-03, -0.9796e-03, -0.1195e-02, -0.1399e-02, &
        -0.1579e-02, -0.1725e-02, -0.1822e-02, -0.1867e-02, -0.1877e-02, &
        -0.1879e-02, -0.1886e-02, -0.1879e-02, -0.1825e-02, -0.1706e-02/
      data ((c3(ip,iw),iw=1,30), ip=22,22)/ &
        -0.8731e-10,  0.2910e-10,  0.7276e-10,  0.1281e-08,  0.1222e-08, &
        -0.1935e-08, -0.8004e-08, -0.2258e-07, -0.5428e-07, -0.1085e-06, &
        -0.1835e-06, -0.2716e-06, -0.3446e-06, -0.3889e-06, -0.4203e-06, &
        -0.4394e-06, -0.3716e-06, -0.1677e-06,  0.1622e-06,  0.6327e-06, &
         0.1275e-05,  0.2018e-05,  0.2716e-05,  0.3137e-05,  0.3136e-05, &
         0.3078e-05,  0.3649e-05,  0.5152e-05,  0.7315e-05,  0.9675e-05/
      data ((c1(ip,iw),iw=1,30), ip=23,23)/ &
         0.99980921,  0.99961692,  0.99924570,  0.99854898,  0.99734801, &
         0.99527103,  0.99182302,  0.98655999,  0.97895002,  0.96814001, &
         0.95284998,  0.93124998,  0.90130001,  0.86170000,  0.81290001, &
         0.75740004,  0.69920003,  0.64199996,  0.58640003,  0.53020000, &
         0.47240001,  0.41399997,  0.35780001,  0.30650002,  0.26069999, &
         0.21850002,  0.17750001,  0.13739997,  0.09950000,  0.06540000/
      data ((c2(ip,iw),iw=1,30), ip=23,23)/ &
        -0.1434e-06, -0.1676e-06, -0.2699e-06, -0.2859e-06, -0.7542e-06, &
        -0.2273e-05, -0.5898e-05, -0.1292e-04, -0.2538e-04, -0.4649e-04, &
        -0.8261e-04, -0.1405e-03, -0.2259e-03, -0.3407e-03, -0.4845e-03, &
        -0.6561e-03, -0.8524e-03, -0.1062e-02, -0.1266e-02, -0.1456e-02, &
        -0.1621e-02, -0.1748e-02, -0.1823e-02, -0.1854e-02, -0.1868e-02, &
        -0.1886e-02, -0.1899e-02, -0.1876e-02, -0.1790e-02, -0.1636e-02/
      data ((c3(ip,iw),iw=1,30), ip=23,23)/ &
        -0.1892e-09, -0.2474e-09,  0.1892e-09,  0.2561e-08,  0.4366e-09, &
        -0.1499e-08, -0.4336e-08, -0.1740e-07, -0.5233e-07, -0.1055e-06, &
        -0.1940e-06, -0.3113e-06, -0.4161e-06, -0.4620e-06, -0.4316e-06, &
        -0.3031e-06, -0.5438e-07,  0.2572e-06,  0.5773e-06,  0.1008e-05, &
         0.1609e-05,  0.2290e-05,  0.2817e-05,  0.2940e-05,  0.2803e-05, &
         0.3061e-05,  0.4235e-05,  0.6225e-05,  0.8615e-05,  0.1095e-04/
      data ((c1(ip,iw),iw=1,30), ip=24,24)/ &
         0.99980992,  0.99961102,  0.99922198,  0.99852699,  0.99732202, &
         0.99510902,  0.99140203,  0.98550999,  0.97672999,  0.96399999, &
         0.94602001,  0.92101002,  0.88709998,  0.84310001,  0.79020000, &
         0.73189998,  0.67299998,  0.61619997,  0.56060004,  0.50400001, &
         0.44610000,  0.38880002,  0.33530003,  0.28740001,  0.24390000, &
         0.20179999,  0.16009998,  0.11979997,  0.08260000,  0.05049998/
      data ((c2(ip,iw),iw=1,30), ip=24,24)/ &
        -0.1529e-06, -0.2005e-06, -0.2861e-06, -0.1652e-06, -0.6334e-06, &
        -0.1965e-05, -0.5437e-05, -0.1182e-04, -0.2344e-04, -0.4384e-04, &
        -0.7982e-04, -0.1398e-03, -0.2321e-03, -0.3616e-03, -0.5274e-03, &
        -0.7239e-03, -0.9363e-03, -0.1142e-02, -0.1328e-02, -0.1499e-02, &
        -0.1645e-02, -0.1748e-02, -0.1804e-02, -0.1834e-02, -0.1867e-02, &
        -0.1903e-02, -0.1914e-02, -0.1866e-02, -0.1746e-02, -0.1558e-02/
      data ((c3(ip,iw),iw=1,30), ip=24,24)/ &
        -0.3638e-09, -0.9313e-09,  0.1703e-08,  0.2081e-08, -0.1251e-08, &
        -0.1208e-08, -0.6883e-08, -0.1608e-07, -0.4559e-07, -0.1047e-06, &
        -0.2040e-06, -0.3312e-06, -0.4624e-06, -0.5198e-06, -0.4326e-06, &
        -0.1452e-06,  0.3003e-06,  0.7455e-06,  0.1102e-05,  0.1470e-05, &
         0.1957e-05,  0.2474e-05,  0.2691e-05,  0.2484e-05,  0.2414e-05, &
         0.3232e-05,  0.5050e-05,  0.7455e-05,  0.9997e-05,  0.1217e-04/
      data ((c1(ip,iw),iw=1,30), ip=25,25)/ &
         0.99980998,  0.99960178,  0.99920201,  0.99852800,  0.99729002, &
         0.99498200,  0.99102801,  0.98461998,  0.97465998,  0.95982999, &
         0.93866003,  0.90968001,  0.87140000,  0.82340002,  0.76770002, &
         0.70860004,  0.64999998,  0.59290004,  0.53610003,  0.47860003, &
         0.42110002,  0.36610001,  0.31639999,  0.27200001,  0.22960001, &
         0.18690002,  0.14429998,  0.10380000,  0.06739998,  0.03740001/
      data ((c2(ip,iw),iw=1,30), ip=25,25)/ &
        -0.1453e-06, -0.2529e-06, -0.1807e-06, -0.1109e-06, -0.4469e-06, &
        -0.1885e-05, -0.4590e-05, -0.1043e-04, -0.2057e-04, -0.3951e-04, &
        -0.7466e-04, -0.1356e-03, -0.2341e-03, -0.3783e-03, -0.5688e-03, &
        -0.7935e-03, -0.1021e-02, -0.1219e-02, -0.1388e-02, -0.1535e-02, &
        -0.1653e-02, -0.1726e-02, -0.1768e-02, -0.1813e-02, -0.1874e-02, &
        -0.1925e-02, -0.1927e-02, -0.1851e-02, -0.1697e-02, -0.1478e-02/
      data ((c3(ip,iw),iw=1,30), ip=25,25)/ &
        -0.6257e-09, -0.1382e-08,  0.2095e-08,  0.1863e-08, -0.1834e-08, &
        -0.2125e-08, -0.6985e-08, -0.1634e-07, -0.4128e-07, -0.9924e-07, &
        -0.1938e-06, -0.3275e-06, -0.4556e-06, -0.5046e-06, -0.3633e-06, &
         0.2484e-07,  0.6195e-06,  0.1249e-05,  0.1731e-05,  0.2053e-05, &
         0.2358e-05,  0.2569e-05,  0.2342e-05,  0.1883e-05,  0.2103e-05, &
         0.3570e-05,  0.5973e-05,  0.8752e-05,  0.1140e-04,  0.1328e-04/
      data ((c1(ip,iw),iw=1,30), ip=26,26)/ &
         0.99980712,  0.99958581,  0.99919039,  0.99854302,  0.99724799, &
         0.99486500,  0.99071401,  0.98379999,  0.97279000,  0.95585001, &
         0.93112999,  0.89749998,  0.85460001,  0.80320001,  0.74660003, &
         0.68869996,  0.63100004,  0.57249999,  0.51320004,  0.45450002, &
         0.39810002,  0.34649998,  0.30119997,  0.25950003,  0.21740001, &
         0.17379999,  0.13029999,  0.08950001,  0.05400002,  0.02640003/
      data ((c2(ip,iw),iw=1,30), ip=26,26)/ &
        -0.1257e-06, -0.2495e-06, -0.1334e-06, -0.8414e-07, -0.1698e-06, &
        -0.1346e-05, -0.3692e-05, -0.8625e-05, -0.1750e-04, -0.3483e-04, &
        -0.6843e-04, -0.1305e-03, -0.2362e-03, -0.3971e-03, -0.6127e-03, &
        -0.8621e-03, -0.1101e-02, -0.1297e-02, -0.1452e-02, -0.1570e-02, &
        -0.1647e-02, -0.1688e-02, -0.1727e-02, -0.1797e-02, -0.1887e-02, &
        -0.1947e-02, -0.1935e-02, -0.1833e-02, -0.1647e-02, -0.1401e-02/
      data ((c3(ip,iw),iw=1,30), ip=26,26)/ &
        -0.1222e-08, -0.1164e-09,  0.2285e-08,  0.2037e-09,  0.5675e-09, &
        -0.5239e-08, -0.9211e-08, -0.1483e-07, -0.3981e-07, -0.9641e-07, &
        -0.1717e-06, -0.2796e-06, -0.3800e-06, -0.3762e-06, -0.1936e-06, &
         0.1920e-06,  0.8335e-06,  0.1691e-05,  0.2415e-05,  0.2767e-05, &
         0.2823e-05,  0.2551e-05,  0.1839e-05,  0.1314e-05,  0.1960e-05, &
         0.4003e-05,  0.6909e-05,  0.1004e-04,  0.1273e-04,  0.1423e-04/
      data ((o1(ip,iw),iw=1,21), ip= 1, 1)/ &
         0.99999344,  0.99998689,  0.99997336,  0.99994606,  0.99989170, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99712098, &
         0.99489498,  0.99144602,  0.98655999,  0.98008001,  0.97165000, &
         0.96043998,  0.94527000,  0.92462999,  0.89709997,  0.86180001, &
         0.81800002/
      data ((o2(ip,iw),iw=1,21), ip= 1, 1)/ &
         0.6531e-10,  0.5926e-10, -0.1646e-09, -0.1454e-08, -0.7376e-08, &
        -0.2968e-07, -0.1071e-06, -0.3584e-06, -0.1125e-05, -0.3289e-05, &
        -0.8760e-05, -0.2070e-04, -0.4259e-04, -0.7691e-04, -0.1264e-03, &
        -0.1957e-03, -0.2895e-03, -0.4107e-03, -0.5588e-03, -0.7300e-03, &
        -0.9199e-03/
      data ((o3(ip,iw),iw=1,21), ip= 1, 1)/ &
        -0.2438e-10, -0.4826e-10, -0.9474e-10, -0.1828e-09, -0.3406e-09, &
        -0.6223e-09, -0.1008e-08, -0.1412e-08, -0.1244e-08,  0.8485e-09, &
         0.6343e-08,  0.1201e-07,  0.2838e-08, -0.4024e-07, -0.1257e-06, &
        -0.2566e-06, -0.4298e-06, -0.6184e-06, -0.7657e-06, -0.8153e-06, &
        -0.7552e-06/
      data ((o1(ip,iw),iw=1,21), ip= 2, 2)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989170, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99712098, &
         0.99489498,  0.99144298,  0.98654997,  0.98006999,  0.97162998, &
         0.96042001,  0.94520003,  0.92449999,  0.89690000,  0.86140001, &
         0.81739998/
      data ((o2(ip,iw),iw=1,21), ip= 2, 2)/ &
         0.6193e-10,  0.5262e-10, -0.1774e-09, -0.1478e-08, -0.7416e-08, &
        -0.2985e-07, -0.1071e-06, -0.3584e-06, -0.1124e-05, -0.3287e-05, &
        -0.8753e-05, -0.2069e-04, -0.4256e-04, -0.7686e-04, -0.1264e-03, &
        -0.1956e-03, -0.2893e-03, -0.4103e-03, -0.5580e-03, -0.7285e-03, &
        -0.9171e-03/
      data ((o3(ip,iw),iw=1,21), ip= 2, 2)/ &
        -0.2436e-10, -0.4822e-10, -0.9466e-10, -0.1827e-09, -0.3404e-09, &
        -0.6220e-09, -0.1008e-08, -0.1414e-08, -0.1247e-08,  0.8360e-09, &
         0.6312e-08,  0.1194e-07,  0.2753e-08, -0.4040e-07, -0.1260e-06, &
        -0.2571e-06, -0.4307e-06, -0.6202e-06, -0.7687e-06, -0.8204e-06, &
        -0.7636e-06/
      data ((o1(ip,iw),iw=1,21), ip= 3, 3)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989170, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99712098, &
         0.99489301,  0.99143898,  0.98654997,  0.98005998,  0.97158998, &
         0.96035999,  0.94509000,  0.92431998,  0.89660001,  0.86080003, &
         0.81639999/
      data ((o2(ip,iw),iw=1,21), ip= 3, 3)/ &
         0.5658e-10,  0.4212e-10, -0.1977e-09, -0.1516e-08, -0.7481e-08, &
        -0.2995e-07, -0.1072e-06, -0.3583e-06, -0.1123e-05, -0.3283e-05, &
        -0.8744e-05, -0.2067e-04, -0.4252e-04, -0.7679e-04, -0.1262e-03, &
        -0.1953e-03, -0.2889e-03, -0.4096e-03, -0.5567e-03, -0.7263e-03, &
        -0.9130e-03/
      data ((o3(ip,iw),iw=1,21), ip= 3, 3)/ &
        -0.2433e-10, -0.4815e-10, -0.9453e-10, -0.1825e-09, -0.3400e-09, &
        -0.6215e-09, -0.1007e-08, -0.1415e-08, -0.1253e-08,  0.8143e-09, &
         0.6269e-08,  0.1186e-07,  0.2604e-08, -0.4067e-07, -0.1264e-06, &
        -0.2579e-06, -0.4321e-06, -0.6229e-06, -0.7732e-06, -0.8277e-06, &
        -0.7752e-06/
      data ((o1(ip,iw),iw=1,21), ip= 4, 4)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989200, &
         0.99978632,  0.99957907,  0.99918377,  0.99844402,  0.99711901, &
         0.99489301,  0.99143499,  0.98653001,  0.98003000,  0.97153997, &
         0.96026999,  0.94493997,  0.92404002,  0.89609998,  0.85990000, &
         0.81480002/
      data ((o2(ip,iw),iw=1,21), ip= 4, 4)/ &
         0.4814e-10,  0.2552e-10, -0.2298e-09, -0.1576e-08, -0.7579e-08, &
        -0.3009e-07, -0.1074e-06, -0.3581e-06, -0.1122e-05, -0.3278e-05, &
        -0.8729e-05, -0.2063e-04, -0.4245e-04, -0.7667e-04, -0.1260e-03, &
        -0.1950e-03, -0.2883e-03, -0.4086e-03, -0.5549e-03, -0.7229e-03, &
        -0.9071e-03/
      data ((o3(ip,iw),iw=1,21), ip= 4, 4)/ &
        -0.2428e-10, -0.4805e-10, -0.9433e-10, -0.1821e-09, -0.3394e-09, &
        -0.6206e-09, -0.1008e-08, -0.1416e-08, -0.1261e-08,  0.7860e-09, &
         0.6188e-08,  0.1171e-07,  0.2389e-08, -0.4109e-07, -0.1271e-06, &
        -0.2591e-06, -0.4344e-06, -0.6267e-06, -0.7797e-06, -0.8378e-06, &
        -0.7901e-06/
      data ((o1(ip,iw),iw=1,21), ip= 5, 5)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989200, &
         0.99978638,  0.99957907,  0.99918377,  0.99844402,  0.99711901, &
         0.99488801,  0.99142599,  0.98650998,  0.97999001,  0.97148001, &
         0.96011001,  0.94467002,  0.92356998,  0.89530003,  0.85860002, &
         0.81250000/
      data ((o2(ip,iw),iw=1,21), ip= 5, 5)/ &
         0.3482e-10, -0.6492e-12, -0.2805e-09, -0.1671e-08, -0.7740e-08, &
        -0.3032e-07, -0.1076e-06, -0.3582e-06, -0.1120e-05, -0.3270e-05, &
        -0.8704e-05, -0.2058e-04, -0.4235e-04, -0.7649e-04, -0.1257e-03, &
        -0.1945e-03, -0.2874e-03, -0.4070e-03, -0.5521e-03, -0.7181e-03, &
        -0.8990e-03/
      data ((o3(ip,iw),iw=1,21), ip= 5, 5)/ &
        -0.2419e-10, -0.4788e-10, -0.9401e-10, -0.1815e-09, -0.3385e-09, &
        -0.6192e-09, -0.1006e-08, -0.1417e-08, -0.1273e-08,  0.7404e-09, &
         0.6068e-08,  0.1148e-07,  0.2021e-08, -0.4165e-07, -0.1281e-06, &
        -0.2609e-06, -0.4375e-06, -0.6323e-06, -0.7887e-06, -0.8508e-06, &
        -0.8067e-06/
      data ((o1(ip,iw),iw=1,21), ip= 6, 6)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994606,  0.99989200, &
         0.99978638,  0.99957931,  0.99918377,  0.99844301,  0.99711698, &
         0.99488401,  0.99141300,  0.98648000,  0.97992003,  0.97135001, &
         0.95989001,  0.94428003,  0.92286998,  0.89410001,  0.85640001, &
         0.80890000/
      data ((o2(ip,iw),iw=1,21), ip= 6, 6)/ &
         0.1388e-10, -0.4180e-10, -0.3601e-09, -0.1820e-08, -0.7993e-08, &
        -0.3068e-07, -0.1081e-06, -0.3580e-06, -0.1117e-05, -0.3257e-05, &
        -0.8667e-05, -0.2049e-04, -0.4218e-04, -0.7620e-04, -0.1253e-03, &
        -0.1937e-03, -0.2860e-03, -0.4047e-03, -0.5481e-03, -0.7115e-03, &
        -0.8885e-03/
      data ((o3(ip,iw),iw=1,21), ip= 6, 6)/ &
        -0.2406e-10, -0.4762e-10, -0.9351e-10, -0.1806e-09, -0.3370e-09, &
        -0.6170e-09, -0.1004e-08, -0.1417e-08, -0.1297e-08,  0.6738e-09, &
         0.5895e-08,  0.1113e-07,  0.1466e-08, -0.4265e-07, -0.1298e-06, &
        -0.2636e-06, -0.4423e-06, -0.6402e-06, -0.8005e-06, -0.8658e-06, &
        -0.8222e-06/
      data ((o1(ip,iw),iw=1,21), ip= 7, 7)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994630,  0.99989200, &
         0.99978638,  0.99957931,  0.99918360,  0.99844301,  0.99711502, &
         0.99487501,  0.99138802,  0.98642999,  0.97982001,  0.97114998, &
         0.95954001,  0.94363999,  0.92176998,  0.89219999,  0.85329998, &
         0.80379999/
      data ((o2(ip,iw),iw=1,21), ip= 7, 7)/ &
        -0.1889e-10, -0.1062e-09, -0.4847e-09, -0.2053e-08, -0.8389e-08, &
        -0.3140e-07, -0.1089e-06, -0.3577e-06, -0.1112e-05, -0.3236e-05, &
        -0.8607e-05, -0.2035e-04, -0.4192e-04, -0.7576e-04, -0.1245e-03, &
        -0.1925e-03, -0.2840e-03, -0.4013e-03, -0.5427e-03, -0.7029e-03, &
        -0.8756e-03/
      data ((o3(ip,iw),iw=1,21), ip= 7, 7)/ &
        -0.2385e-10, -0.4722e-10, -0.9273e-10, -0.1791e-09, -0.3348e-09, &
        -0.6121e-09, -0.9974e-09, -0.1422e-08, -0.1326e-08,  0.5603e-09, &
         0.5604e-08,  0.1061e-07,  0.6106e-09, -0.4398e-07, -0.1321e-06, &
        -0.2676e-06, -0.4490e-06, -0.6507e-06, -0.8145e-06, -0.8801e-06, &
        -0.8311e-06/
      data ((o1(ip,iw),iw=1,21), ip= 8, 8)/ &
         0.99999344,  0.99998689,  0.99997348,  0.99994630,  0.99989229, &
         0.99978650,  0.99957931,  0.99918288,  0.99844098,  0.99711001, &
         0.99486202,  0.99135500,  0.98635000,  0.97965997,  0.97083998, &
         0.95898998,  0.94266999,  0.92009997,  0.88929999,  0.84860003, &
         0.79640001/
      data ((o2(ip,iw),iw=1,21), ip= 8, 8)/ &
        -0.6983e-10, -0.2063e-09, -0.6785e-09, -0.2416e-08, -0.9000e-08, &
        -0.3243e-07, -0.1100e-06, -0.3574e-06, -0.1104e-05, -0.3205e-05, &
        -0.8516e-05, -0.2014e-04, -0.4151e-04, -0.7508e-04, -0.1234e-03, &
        -0.1907e-03, -0.2811e-03, -0.3966e-03, -0.5355e-03, -0.6924e-03, &
        -0.8613e-03/
      data ((o3(ip,iw),iw=1,21), ip= 8, 8)/ &
        -0.2353e-10, -0.4659e-10, -0.9153e-10, -0.1769e-09, -0.3313e-09, &
        -0.6054e-09, -0.9899e-09, -0.1430e-08, -0.1375e-08,  0.3874e-09, &
         0.5171e-08,  0.9807e-08, -0.7345e-09, -0.4604e-07, -0.1356e-06, &
        -0.2731e-06, -0.4577e-06, -0.6632e-06, -0.8284e-06, -0.8894e-06, &
        -0.8267e-06/
      data ((o1(ip,iw),iw=1,21), ip= 9, 9)/ &
         0.99999344,  0.99998689,  0.99997360,  0.99994630,  0.99989229, &
         0.99978650,  0.99957961,  0.99918252,  0.99843901,  0.99710202, &
         0.99484003,  0.99130303,  0.98623002,  0.97940999,  0.97038001, &
         0.95815003,  0.94119000,  0.91755998,  0.88510001,  0.84189999, &
         0.78610003/
      data ((o2(ip,iw),iw=1,21), ip= 9, 9)/ &
        -0.1481e-09, -0.3601e-09, -0.9762e-09, -0.2973e-08, -0.1014e-07, &
        -0.3421e-07, -0.1121e-06, -0.3569e-06, -0.1092e-05, -0.3156e-05, &
        -0.8375e-05, -0.1981e-04, -0.4090e-04, -0.7405e-04, -0.1218e-03, &
        -0.1881e-03, -0.2770e-03, -0.3906e-03, -0.5269e-03, -0.6810e-03, &
        -0.8471e-03/
      data ((o3(ip,iw),iw=1,21), ip= 9, 9)/ &
        -0.2304e-10, -0.4564e-10, -0.8969e-10, -0.1735e-09, -0.3224e-09, &
        -0.5933e-09, -0.9756e-09, -0.1428e-08, -0.1446e-08,  0.1156e-09, &
         0.4499e-08,  0.8469e-08, -0.2720e-08, -0.4904e-07, -0.1401e-06, &
        -0.2801e-06, -0.4681e-06, -0.6761e-06, -0.8387e-06, -0.8879e-06, &
        -0.8040e-06/
      data ((o1(ip,iw),iw=1,21), ip=10,10)/ &
         0.99999344,  0.99998689,  0.99997360,  0.99994630,  0.99989259, &
         0.99978650,  0.99957931,  0.99918163,  0.99843597,  0.99709100, &
         0.99480897,  0.99122101,  0.98604000,  0.97902000,  0.96965003, &
         0.95684999,  0.93896997,  0.91386002,  0.87910002,  0.83249998, &
         0.77200001/
      data ((o2(ip,iw),iw=1,21), ip=10,10)/ &
        -0.2661e-09, -0.5923e-09, -0.1426e-08, -0.3816e-08, -0.1159e-07, &
        -0.3654e-07, -0.1143e-06, -0.3559e-06, -0.1074e-05, -0.3083e-05, &
        -0.8159e-05, -0.1932e-04, -0.3998e-04, -0.7253e-04, -0.1194e-03, &
        -0.1845e-03, -0.2718e-03, -0.3833e-03, -0.5176e-03, -0.6701e-03, &
        -0.8354e-03/
      data ((o3(ip,iw),iw=1,21), ip=10,10)/ &
        -0.2232e-10, -0.4421e-10, -0.8695e-10, -0.1684e-09, -0.3141e-09, &
        -0.5765e-09, -0.9606e-09, -0.1434e-08, -0.1551e-08, -0.2663e-09, &
         0.3515e-08,  0.6549e-08, -0.5479e-08, -0.5312e-07, -0.1460e-06, &
        -0.2883e-06, -0.4787e-06, -0.6863e-06, -0.8399e-06, -0.8703e-06, &
        -0.7602e-06/
      data ((o1(ip,iw),iw=1,21), ip=11,11)/ &
         0.99999356,  0.99998701,  0.99997360,  0.99994630,  0.99989289, &
         0.99978679,  0.99957907,  0.99917960,  0.99843001,  0.99707502, &
         0.99475998,  0.99109501,  0.98575002,  0.97843999,  0.96855003, &
         0.95494002,  0.93572998,  0.90853000,  0.87070000,  0.81970000, &
         0.75380003/
      data ((o2(ip,iw),iw=1,21), ip=11,11)/ &
        -0.4394e-09, -0.9330e-09, -0.2086e-08, -0.5054e-08, -0.1373e-07, &
        -0.3971e-07, -0.1178e-06, -0.3546e-06, -0.1049e-05, -0.2976e-05, &
        -0.7847e-05, -0.1860e-04, -0.3864e-04, -0.7038e-04, -0.1162e-03, &
        -0.1798e-03, -0.2654e-03, -0.3754e-03, -0.5091e-03, -0.6621e-03, &
        -0.8286e-03/
      data ((o3(ip,iw),iw=1,21), ip=11,11)/ &
        -0.2127e-10, -0.4216e-10, -0.8300e-10, -0.1611e-09, -0.3019e-09, &
        -0.5597e-09, -0.9431e-09, -0.1450e-08, -0.1694e-08, -0.7913e-09, &
         0.2144e-08,  0.3990e-08, -0.9282e-08, -0.5810e-07, -0.1525e-06, &
        -0.2965e-06, -0.4869e-06, -0.6894e-06, -0.8281e-06, -0.8350e-06, &
        -0.6956e-06/
      data ((o1(ip,iw),iw=1,21), ip=12,12)/ &
         0.99999368,  0.99998701,  0.99997377,  0.99994630,  0.99989259, &
         0.99978709,  0.99957848,  0.99917740,  0.99842203,  0.99704897, &
         0.99468797,  0.99090999,  0.98532999,  0.97758001,  0.96693999, &
         0.95213997,  0.93109000,  0.90110999,  0.85930002,  0.80290002, &
         0.73019999/
      data ((o2(ip,iw),iw=1,21), ip=12,12)/ &
        -0.6829e-09, -0.1412e-08, -0.3014e-08, -0.6799e-08, -0.1675e-07, &
        -0.4450e-07, -0.1235e-06, -0.3538e-06, -0.1014e-05, -0.2827e-05, &
        -0.7407e-05, -0.1759e-04, -0.3676e-04, -0.6744e-04, -0.1120e-03, &
        -0.1742e-03, -0.2585e-03, -0.3683e-03, -0.5034e-03, -0.6594e-03, &
        -0.8290e-03/
      data ((o3(ip,iw),iw=1,21), ip=12,12)/ &
        -0.1985e-10, -0.3937e-10, -0.7761e-10, -0.1511e-09, -0.2855e-09, &
        -0.5313e-09, -0.9251e-09, -0.1470e-08, -0.1898e-08, -0.1519e-08, &
         0.2914e-09,  0.5675e-09, -0.1405e-07, -0.6359e-07, -0.1584e-06, &
        -0.3020e-06, -0.4893e-06, -0.6821e-06, -0.8021e-06, -0.7834e-06, &
        -0.6105e-06/
      data ((o1(ip,iw),iw=1,21), ip=13,13)/ &
         0.99999368,  0.99998701,  0.99997389,  0.99994695,  0.99989289, &
         0.99978721,  0.99957782,  0.99917412,  0.99840999,  0.99701297, &
         0.99458599,  0.99064600,  0.98471999,  0.97632003,  0.96464998, &
         0.94819999,  0.92467999,  0.89109999,  0.84430003,  0.78139997, &
         0.70070004/
      data ((o2(ip,iw),iw=1,21), ip=13,13)/ &
        -0.1004e-08, -0.2043e-08, -0.4239e-08, -0.9104e-08, -0.2075e-07, &
        -0.5096e-07, -0.1307e-06, -0.3520e-06, -0.9671e-06, -0.2630e-05, &
        -0.6825e-05, -0.1624e-04, -0.3429e-04, -0.6369e-04, -0.1069e-03, &
        -0.1680e-03, -0.2520e-03, -0.3635e-03, -0.5029e-03, -0.6647e-03, &
        -0.8390e-03/
      data ((o3(ip,iw),iw=1,21), ip=13,13)/ &
        -0.1807e-10, -0.3587e-10, -0.7085e-10, -0.1385e-09, -0.2648e-09, &
        -0.4958e-09, -0.8900e-09, -0.1473e-08, -0.2112e-08, -0.2399e-08, &
        -0.2002e-08, -0.3646e-08, -0.1931e-07, -0.6852e-07, -0.1618e-06, &
        -0.3021e-06, -0.4828e-06, -0.6634e-06, -0.7643e-06, -0.7177e-06, &
        -0.5054e-06/
      data ((o1(ip,iw),iw=1,21), ip=14,14)/ &
         0.99999368,  0.99998713,  0.99997389,  0.99994725,  0.99989289, &
         0.99978679,  0.99957597,  0.99916971,  0.99839503,  0.99696702, &
         0.99444997,  0.99028301,  0.98387003,  0.97457999,  0.96148002, &
         0.94284999,  0.91613001,  0.87809998,  0.82520002,  0.75489998, &
         0.66520000/
      data ((o2(ip,iw),iw=1,21), ip=14,14)/ &
        -0.1387e-08, -0.2798e-08, -0.5706e-08, -0.1187e-07, -0.2564e-07, &
        -0.5866e-07, -0.1398e-06, -0.3516e-06, -0.9148e-06, -0.2398e-05, &
        -0.6122e-05, -0.1459e-04, -0.3125e-04, -0.5923e-04, -0.1013e-03, &
        -0.1620e-03, -0.2473e-03, -0.3631e-03, -0.5098e-03, -0.6800e-03, &
        -0.8603e-03/
      data ((o3(ip,iw),iw=1,21), ip=14,14)/ &
        -0.1610e-10, -0.3200e-10, -0.6337e-10, -0.1245e-09, -0.2408e-09, &
        -0.4533e-09, -0.8405e-09, -0.1464e-08, -0.2337e-08, -0.3341e-08, &
        -0.4467e-08, -0.8154e-08, -0.2436e-07, -0.7128e-07, -0.1604e-06, &
        -0.2945e-06, -0.4666e-06, -0.6357e-06, -0.7187e-06, -0.6419e-06, &
        -0.3795e-06/
      data ((o1(ip,iw),iw=1,21), ip=15,15)/ &
         0.99999410,  0.99998724,  0.99997455,  0.99994725,  0.99989331, &
         0.99978632,  0.99957472,  0.99916393,  0.99837703,  0.99690801, &
         0.99427801,  0.98982000,  0.98277998,  0.97232002,  0.95731997, &
         0.93585998,  0.90521002,  0.86180001,  0.80190003,  0.72290003, &
         0.62380004/
      data ((o2(ip,iw),iw=1,21), ip=15,15)/ &
        -0.1788e-08, -0.3588e-08, -0.7244e-08, -0.1479e-07, -0.3083e-07, &
        -0.6671e-07, -0.1497e-06, -0.3519e-06, -0.8607e-06, -0.2154e-05, &
        -0.5364e-05, -0.1276e-04, -0.2785e-04, -0.5435e-04, -0.9573e-04, &
        -0.1570e-03, -0.2455e-03, -0.3682e-03, -0.5253e-03, -0.7065e-03, &
        -0.8938e-03/
      data ((o3(ip,iw),iw=1,21), ip=15,15)/ &
        -0.1429e-10, -0.2843e-10, -0.5645e-10, -0.1115e-09, -0.2181e-09, &
        -0.4200e-09, -0.7916e-09, -0.1460e-08, -0.2542e-08, -0.4168e-08, &
        -0.6703e-08, -0.1215e-07, -0.2821e-07, -0.7073e-07, -0.1530e-06, &
        -0.2791e-06, -0.4426e-06, -0.6027e-06, -0.6707e-06, -0.5591e-06, &
        -0.2328e-06/
      data ((o1(ip,iw),iw=1,21), ip=16,16)/ &
         0.99999434,  0.99998778,  0.99997467,  0.99994761,  0.99989331, &
         0.99978602,  0.99957269,  0.99915779,  0.99835497,  0.99684399, &
         0.99408400,  0.98929000,  0.98148000,  0.96954000,  0.95212001, &
         0.92719001,  0.89170003,  0.84200001,  0.77420002,  0.68620002, &
         0.57780004/
      data ((o2(ip,iw),iw=1,21), ip=16,16)/ &
        -0.2141e-08, -0.4286e-08, -0.8603e-08, -0.1737e-07, -0.3548e-07, &
        -0.7410e-07, -0.1590e-06, -0.3537e-06, -0.8142e-06, -0.1935e-05, &
        -0.4658e-05, -0.1099e-04, -0.2444e-04, -0.4948e-04, -0.9067e-04, &
        -0.1538e-03, -0.2474e-03, -0.3793e-03, -0.5495e-03, -0.7439e-03, &
        -0.9383e-03/
      data ((o3(ip,iw),iw=1,21), ip=16,16)/ &
        -0.1295e-10, -0.2581e-10, -0.5136e-10, -0.1019e-09, -0.2011e-09, &
        -0.3916e-09, -0.7585e-09, -0.1439e-08, -0.2648e-08, -0.4747e-08, &
        -0.8301e-08, -0.1499e-07, -0.3024e-07, -0.6702e-07, -0.1399e-06, &
        -0.2564e-06, -0.4117e-06, -0.5669e-06, -0.6239e-06, -0.4748e-06, &
        -0.7013e-07/
      data ((o1(ip,iw),iw=1,21), ip=17,17)/ &
         0.99999434,  0.99998778,  0.99997479,  0.99994791,  0.99989331, &
         0.99978608,  0.99957120,  0.99915212,  0.99833500,  0.99677801, &
         0.99388403,  0.98873001,  0.98005998,  0.96639001,  0.94606000, &
         0.91689998,  0.87580001,  0.81889999,  0.74280000,  0.64559996, &
         0.52869999/
      data ((o2(ip,iw),iw=1,21), ip=17,17)/ &
        -0.2400e-08, -0.4796e-08, -0.9599e-08, -0.1927e-07, -0.3892e-07, &
        -0.7954e-07, -0.1661e-06, -0.3540e-06, -0.7780e-06, -0.1763e-05, &
        -0.4092e-05, -0.9512e-05, -0.2142e-04, -0.4502e-04, -0.8640e-04, &
        -0.1525e-03, -0.2526e-03, -0.3955e-03, -0.5805e-03, -0.7897e-03, &
        -0.9899e-03/
      data ((o3(ip,iw),iw=1,21), ip=17,17)/ &
        -0.1220e-10, -0.2432e-10, -0.4845e-10, -0.9640e-10, -0.1912e-09, &
        -0.3771e-09, -0.7392e-09, -0.1420e-08, -0.2702e-08, -0.5049e-08, &
        -0.9214e-08, -0.1659e-07, -0.3101e-07, -0.6162e-07, -0.1235e-06, &
        -0.2287e-06, -0.3755e-06, -0.5274e-06, -0.5790e-06, -0.3947e-06, &
         0.1003e-06/
      data ((o1(ip,iw),iw=1,21), ip=18,18)/ &
         0.99999464,  0.99998808,  0.99997497,  0.99994791,  0.99989331, &
         0.99978518,  0.99957031,  0.99914658,  0.99831802,  0.99671799, &
         0.99370098,  0.98821002,  0.97867000,  0.96313000,  0.93948001, &
         0.90534002,  0.85769999,  0.79310000,  0.70840001,  0.60290003, &
         0.47930002/
      data ((o2(ip,iw),iw=1,21), ip=18,18)/ &
        -0.2557e-08, -0.5106e-08, -0.1020e-07, -0.2043e-07, -0.4103e-07, &
        -0.8293e-07, -0.1697e-06, -0.3531e-06, -0.7531e-06, -0.1645e-05, &
        -0.3690e-05, -0.8411e-05, -0.1902e-04, -0.4118e-04, -0.8276e-04, &
        -0.1525e-03, -0.2601e-03, -0.4147e-03, -0.6149e-03, -0.8384e-03, &
        -0.1042e-02/
      data ((o3(ip,iw),iw=1,21), ip=18,18)/ &
        -0.1189e-10, -0.2372e-10, -0.4729e-10, -0.9421e-10, -0.1873e-09, &
        -0.3713e-09, -0.7317e-09, -0.1437e-08, -0.2764e-08, -0.5243e-08, &
        -0.9691e-08, -0.1751e-07, -0.3122e-07, -0.5693e-07, -0.1076e-06, &
        -0.1981e-06, -0.3324e-06, -0.4785e-06, -0.5280e-06, -0.3174e-06, &
         0.2672e-06/
      data ((o1(ip,iw),iw=1,21), ip=19,19)/ &
         0.99999464,  0.99998820,  0.99997509,  0.99994779,  0.99989331, &
         0.99978518,  0.99956989,  0.99914283,  0.99830401,  0.99667197, &
         0.99355298,  0.98776001,  0.97741997,  0.96007001,  0.93285000, &
         0.89310002,  0.83819997,  0.76520002,  0.67250001,  0.56000000, &
         0.43199998/
      data ((o2(ip,iw),iw=1,21), ip=19,19)/ &
        -0.2630e-08, -0.5249e-08, -0.1048e-07, -0.2096e-07, -0.4198e-07, &
        -0.8440e-07, -0.1710e-06, -0.3513e-06, -0.7326e-06, -0.1562e-05, &
        -0.3416e-05, -0.7637e-05, -0.1719e-04, -0.3795e-04, -0.7926e-04, &
        -0.1524e-03, -0.2680e-03, -0.4344e-03, -0.6486e-03, -0.8838e-03, &
        -0.1089e-02/
      data ((o3(ip,iw),iw=1,21), ip=19,19)/ &
        -0.1188e-10, -0.2369e-10, -0.4725e-10, -0.9417e-10, -0.1875e-09, &
        -0.3725e-09, -0.7365e-09, -0.1445e-08, -0.2814e-08, -0.5384e-08, &
        -0.1008e-07, -0.1816e-07, -0.3179e-07, -0.5453e-07, -0.9500e-07, &
        -0.1679e-06, -0.2819e-06, -0.4109e-06, -0.4555e-06, -0.2283e-06, &
         0.4283e-06/
      data ((o1(ip,iw),iw=1,21), ip=20,20)/ &
         0.99999487,  0.99998832,  0.99997520,  0.99994791,  0.99989331, &
         0.99978459,  0.99956900,  0.99913990,  0.99829400,  0.99663699, &
         0.99344099,  0.98741001,  0.97643000,  0.95743001,  0.92672002, &
         0.88099998,  0.81809998,  0.73660004,  0.63620001,  0.51880002, &
         0.38880002/
      data ((o2(ip,iw),iw=1,21), ip=20,20)/ &
        -0.2651e-08, -0.5291e-08, -0.1056e-07, -0.2110e-07, -0.4221e-07, &
        -0.8462e-07, -0.1705e-06, -0.3466e-06, -0.7155e-06, -0.1501e-05, &
        -0.3223e-05, -0.7079e-05, -0.1581e-04, -0.3517e-04, -0.7553e-04, &
        -0.1510e-03, -0.2746e-03, -0.4528e-03, -0.6789e-03, -0.9214e-03, &
        -0.1124e-02/
      data ((o3(ip,iw),iw=1,21), ip=20,20)/ &
        -0.1193e-10, -0.2380e-10, -0.4748e-10, -0.9465e-10, -0.1886e-09, &
        -0.3751e-09, -0.7436e-09, -0.1466e-08, -0.2872e-08, -0.5508e-08, &
        -0.1038e-07, -0.1891e-07, -0.3279e-07, -0.5420e-07, -0.8711e-07, &
        -0.1403e-06, -0.2248e-06, -0.3221e-06, -0.3459e-06, -0.1066e-06, &
         0.5938e-06/
      data ((o1(ip,iw),iw=1,21), ip=21,21)/ &
         0.99999487,  0.99998873,  0.99997509,  0.99994779,  0.99989349, &
         0.99978501,  0.99956918,  0.99913877,  0.99828798,  0.99661303, &
         0.99335998,  0.98715001,  0.97566003,  0.95530999,  0.92153001, &
         0.87000000,  0.79869998,  0.70819998,  0.60109997,  0.48110002, &
         0.35140002/
      data ((o2(ip,iw),iw=1,21), ip=21,21)/ &
        -0.2654e-08, -0.5296e-08, -0.1057e-07, -0.2111e-07, -0.4219e-07, &
        -0.8445e-07, -0.1696e-06, -0.3428e-06, -0.7013e-06, -0.1458e-05, &
        -0.3084e-05, -0.6678e-05, -0.1476e-04, -0.3284e-04, -0.7173e-04, &
        -0.1481e-03, -0.2786e-03, -0.4688e-03, -0.7052e-03, -0.9506e-03, &
        -0.1148e-02/
      data ((o3(ip,iw),iw=1,21), ip=21,21)/ &
        -0.1195e-10, -0.2384e-10, -0.4755e-10, -0.9482e-10, -0.1890e-09, &
        -0.3761e-09, -0.7469e-09, -0.1476e-08, -0.2892e-08, -0.5603e-08, &
        -0.1060e-07, -0.1942e-07, -0.3393e-07, -0.5508e-07, -0.8290e-07, &
        -0.1182e-06, -0.1657e-06, -0.2170e-06, -0.1997e-06,  0.6227e-07, &
         0.7847e-06/
      data ((o1(ip,iw),iw=1,21), ip=22,22)/ &
         0.99999541,  0.99998873,  0.99997497,  0.99994737,  0.99989349, &
         0.99978501,  0.99956882,  0.99913770,  0.99828303,  0.99659699, &
         0.99330199,  0.98697001,  0.97510999,  0.95372999,  0.91742998, &
         0.86080003,  0.78139997,  0.68220001,  0.56920004,  0.44809997, &
         0.32080001/
      data ((o2(ip,iw),iw=1,21), ip=22,22)/ &
        -0.2653e-08, -0.5295e-08, -0.1057e-07, -0.2110e-07, -0.4215e-07, &
        -0.8430e-07, -0.1690e-06, -0.3403e-06, -0.6919e-06, -0.1427e-05, &
        -0.2991e-05, -0.6399e-05, -0.1398e-04, -0.3099e-04, -0.6824e-04, &
        -0.1441e-03, -0.2795e-03, -0.4814e-03, -0.7282e-03, -0.9739e-03, &
        -0.1163e-02/
      data ((o3(ip,iw),iw=1,21), ip=22,22)/ &
        -0.1195e-10, -0.2384e-10, -0.4756e-10, -0.9485e-10, -0.1891e-09, &
        -0.3765e-09, -0.7483e-09, -0.1481e-08, -0.2908e-08, -0.5660e-08, &
        -0.1075e-07, -0.1980e-07, -0.3472e-07, -0.5626e-07, -0.8149e-07, &
        -0.1027e-06, -0.1136e-06, -0.1071e-06, -0.2991e-07,  0.2743e-06, &
         0.1017e-05/
      data ((o1(ip,iw),iw=1,21), ip=23,23)/ &
         0.99999595,  0.99998885,  0.99997479,  0.99994725,  0.99989331, &
         0.99978518,  0.99956882,  0.99913692,  0.99827999,  0.99658602, &
         0.99326497,  0.98685002,  0.97474003,  0.95260000,  0.91441000, &
         0.85360003,  0.76719999,  0.65990001,  0.54190004,  0.42119998, &
         0.29699999/
      data ((o2(ip,iw),iw=1,21), ip=23,23)/ &
        -0.2653e-08, -0.5294e-08, -0.1057e-07, -0.2109e-07, -0.4212e-07, &
        -0.8420e-07, -0.1686e-06, -0.3388e-06, -0.6858e-06, -0.1406e-05, &
        -0.2928e-05, -0.6206e-05, -0.1344e-04, -0.2961e-04, -0.6533e-04, &
        -0.1399e-03, -0.2780e-03, -0.4904e-03, -0.7488e-03, -0.9953e-03, &
        -0.1175e-02/
      data ((o3(ip,iw),iw=1,21), ip=23,23)/ &
        -0.1195e-10, -0.2384e-10, -0.4756e-10, -0.9485e-10, -0.1891e-09, &
        -0.3767e-09, -0.7492e-09, -0.1485e-08, -0.2924e-08, -0.5671e-08, &
        -0.1084e-07, -0.2009e-07, -0.3549e-07, -0.5773e-07, -0.8208e-07, &
        -0.9394e-07, -0.7270e-07, -0.3947e-08,  0.1456e-06,  0.5083e-06, &
         0.1270e-05/
      data ((o1(ip,iw),iw=1,21), ip=24,24)/ &
         0.99999630,  0.99998873,  0.99997401,  0.99994725,  0.99989349, &
         0.99978501,  0.99956959,  0.99913663,  0.99827701,  0.99658000, &
         0.99324101,  0.98676997,  0.97447002,  0.95185000,  0.91232002, &
         0.84850001,  0.75660002,  0.64230001,  0.52030003,  0.40090001, &
         0.27980000/
      data ((o2(ip,iw),iw=1,21), ip=24,24)/ &
        -0.2653e-08, -0.5294e-08, -0.1056e-07, -0.2109e-07, -0.4210e-07, &
        -0.8413e-07, -0.1684e-06, -0.3379e-06, -0.6820e-06, -0.1393e-05, &
        -0.2889e-05, -0.6080e-05, -0.1307e-04, -0.2861e-04, -0.6310e-04, &
        -0.1363e-03, -0.2758e-03, -0.4969e-03, -0.7681e-03, -0.1017e-02, &
        -0.1186e-02/
      data ((o3(ip,iw),iw=1,21), ip=24,24)/ &
        -0.1195e-10, -0.2384e-10, -0.4756e-10, -0.9485e-10, -0.1891e-09, &
        -0.3768e-09, -0.7497e-09, -0.1487e-08, -0.2933e-08, -0.5710e-08, &
        -0.1089e-07, -0.2037e-07, -0.3616e-07, -0.5907e-07, -0.8351e-07, &
        -0.8925e-07, -0.4122e-07,  0.8779e-07,  0.3143e-06,  0.7281e-06, &
         0.1500e-05/
      data ((o1(ip,iw),iw=1,21), ip=25,25)/ &
         0.99999648,  0.99998897,  0.99997377,  0.99994749,  0.99989331, &
         0.99978501,  0.99956989,  0.99913692,  0.99827600,  0.99657297, &
         0.99322498,  0.98672003,  0.97431999,  0.95137000,  0.91095001, &
         0.84500003,  0.74909997,  0.62979996,  0.50510001,  0.38679999, &
         0.26789999/
      data ((o2(ip,iw),iw=1,21), ip=25,25)/ &
        -0.2653e-08, -0.5293e-08, -0.1056e-07, -0.2108e-07, -0.4209e-07, &
        -0.8409e-07, -0.1682e-06, -0.3373e-06, -0.6797e-06, -0.1383e-05, &
        -0.2862e-05, -0.5993e-05, -0.1283e-04, -0.2795e-04, -0.6158e-04, &
        -0.1338e-03, -0.2743e-03, -0.5030e-03, -0.7863e-03, -0.1038e-02, &
        -0.1196e-02/
      data ((o3(ip,iw),iw=1,21), ip=25,25)/ &
        -0.1195e-10, -0.2383e-10, -0.4755e-10, -0.9484e-10, -0.1891e-09, &
        -0.3768e-09, -0.7499e-09, -0.1489e-08, -0.2939e-08, -0.5741e-08, &
        -0.1100e-07, -0.2066e-07, -0.3660e-07, -0.6002e-07, -0.8431e-07, &
        -0.8556e-07, -0.1674e-07,  0.1638e-06,  0.4525e-06,  0.8949e-06, &
         0.1669e-05/
      data ((o1(ip,iw),iw=1,21), ip=26,26)/ &
         0.99999672,  0.99998909,  0.99997377,  0.99994695,  0.99989349, &
         0.99978518,  0.99956989,  0.99913692,  0.99827498,  0.99657100, &
         0.99321902,  0.98668998,  0.97421002,  0.95106000,  0.91009998, &
         0.84280002,  0.74430001,  0.62180001,  0.49519998,  0.37800002, &
         0.25999999/
      data ((o2(ip,iw),iw=1,21), ip=26,26)/ &
        -0.2652e-08, -0.5292e-08, -0.1056e-07, -0.2108e-07, -0.4208e-07, &
        -0.8406e-07, -0.1681e-06, -0.3369e-06, -0.6784e-06, -0.1378e-05, &
        -0.2843e-05, -0.5944e-05, -0.1269e-04, -0.2759e-04, -0.6078e-04, &
        -0.1326e-03, -0.2742e-03, -0.5088e-03, -0.8013e-03, -0.1054e-02, &
        -0.1202e-02/
      data ((o3(ip,iw),iw=1,21), ip=26,26)/ &
        -0.1194e-10, -0.2383e-10, -0.4754e-10, -0.9482e-10, -0.1891e-09, &
        -0.3768e-09, -0.7499e-09, -0.1489e-08, -0.2941e-08, -0.5752e-08, &
        -0.1104e-07, -0.2069e-07, -0.3661e-07, -0.6012e-07, -0.8399e-07, &
        -0.8183e-07,  0.1930e-08,  0.2167e-06,  0.5434e-06,  0.9990e-06, &
         0.1787e-05/

!xp
!      allocate(pa(m,np))
!      allocate(dt(m,np))
!      allocate(sh2o(m,np+1))
!      allocate(swpre(m,np+1))
!      allocate(swtem(m,np+1))
!      allocate(sco3(m,np+1))
!      allocate(scopre(m,np+1))
!      allocate(scotem(m,np+1))
!      allocate(dh2o(m,np))
!      allocate(dcont(m,np))
!      allocate(dco2(m,np))
!      allocate(do3(m,np))
!      allocate(dn2o(m,np))
!      allocate(dch4(m,np))
!      allocate(df11(m,np))
!      allocate(df12(m,np))
!      allocate(df22(m,np))
!      allocate(th2o(m,6))
!      allocate(tcon(m,3))
!      allocate(tco2(m,6,2))
!      allocate(tn2o(m,4))
!      allocate(tch4(m,4))
!      allocate(tcom(m,6))
!      allocate(tf11(m))
!      allocate(tf12(m))
!      allocate(tf22(m))
!      allocate(h2oexp(m,np,6))
!      allocate(conexp(m,np,3))
!      allocate(co2exp(m,np,6,2))
!      allocate(n2oexp(m,np,4))
!      allocate(ch4exp(m,np,4))
!      allocate(comexp(m,np,6))
!      allocate(f11exp(m,np))
!      allocate(f12exp(m,np))
!      allocate(f22exp(m,np))
!      allocate(blayer(m,0:np+1))
!      allocate(blevel(m,np+1))
!      allocate(dblayr(m,np+1))
!      allocate(dbs(m))
!      allocate(dp(m,np))
!      allocate(cwp(m,np,3))
!      allocate(trant(m))
!      allocate(tranal(m))
!      allocate(transfc(m,np+1))
!      allocate(trantcr(m,np+1))
!      allocate(flxu(m,np+1))
!      allocate(flxd(m,np+1))
!      allocate(flcu(m,np+1))
!      allocate(flcd(m,np+1))
!      allocate(rflx(m,np+1))
!      allocate(rflc(m,np+1))
!      allocate(it(m))
!      allocate(im(m))
!      allocate(ib(m))
!      allocate(cldhi(m))
!      allocate(cldmd(m))
!      allocate(cldlw(m))
!      allocate(tcldlyr(m,np))
!      allocate(fclr(m))
!      allocate(taerlyr(m,np))

!-----compute layer pressure (pa) and layer temperature minus 250k (dt)
      do k=1,np
       do i=1,m
         pa(i,k)=0.5*(pl(i,k)+pl(i,k+1))
         dt(i,k)=ta(i,k)-250.0
       enddo
      enddo

!-----compute layer absorber amount
!     dh2o : water vapor amount (g/cm**2)
!     dcont: scaled water vapor amount for continuum absorption
!            (g/cm**2)
!     dco2 : co2 amount (cm-atm)stp
!     do3  : o3 amount (cm-atm)stp
!     dn2o : n2o amount (cm-atm)stp
!     dch4 : ch4 amount (cm-atm)stp
!     df11 : cfc11 amount (cm-atm)stp
!     df12 : cfc12 amount (cm-atm)stp
!     df22 : cfc22 amount (cm-atm)stp
!     the factor 1.02 is equal to 1000/980
!     factors 789 and 476 are for unit conversion
!     the factor 0.001618 is equal to 1.02/(.622*1013.25)
!     the factor 6.081 is equal to 1800/296
      do k=1,np
       do i=1,m
         dp   (i,k) = pl(i,k+1)-pl(i,k)
         dh2o (i,k) = 1.02*wa(i,k)*dp(i,k)+1.e-10
         do3  (i,k) = 476.*oa(i,k)*dp(i,k)+1.e-10
         dco2 (i,k) = 789.*co2*dp(i,k)+1.e-10
         dch4 (i,k) = 789.*ch4*dp(i,k)+1.e-10
         dn2o (i,k) = 789.*n2o*dp(i,k)+1.e-10
         df11 (i,k) = 789.*cfc11*dp(i,k)+1.e-10
         df12 (i,k) = 789.*cfc12*dp(i,k)+1.e-10
         df22 (i,k) = 789.*cfc22*dp(i,k)+1.e-10
!-----compute scaled water vapor amount for h2o continuum absorption
!     following eq. (4.21).
         xx=pa(i,k)*0.001618*wa(i,k)*wa(i,k)*dp(i,k)
         dcont(i,k) = xx*exp(1800./ta(i,k)-6.081)+1.e-10
       enddo
      enddo
!-----compute column-integrated h2o amoumt (sh2o), h2o-weighted pressure
!     (swpre) and temperature (swtem). it follows eqs. (4.13) and (4.14).

       if (high) then
        call column(m,np,pa,dt,dh2o,sh2o,swpre,swtem)
       endif

!-----compute layer cloud water amount (gm/m**2)
!     index is 1 for ice, 2 for waterdrops and 3 for raindrops.

       if (cldwater) then
        do k=1,np
         do i=1,m
             xx=1.02*10000.*(pl(i,k+1)-pl(i,k))
             cwp(i,k,1)=xx*cwc(i,k,1)
             cwp(i,k,2)=xx*cwc(i,k,2)
             cwp(i,k,3)=xx*cwc(i,k,3)
         enddo
        enddo
       endif

!-----the surface (np+1) is treated as a layer filled with black clouds.
!     transfc is the transmttance between the surface and a pressure level.
!     trantcr is the clear-sky transmttance between the surface and a
!     pressure level.

      do i=1,m
        sfcem(i)       =0.0
        transfc(i,np+1)=1.0
        trantcr(i,np+1)=1.0
      enddo

!-----initialize fluxes

      do k=1,np+1
       do i=1,m
         flx(i,k)  = 0.0
         flc(i,k)  = 0.0
         dfdts(i,k)= 0.0
         rflx(i,k) = 0.0
         rflc(i,k) = 0.0
         acflxu(i,k) = 0.0
         acflxd(i,k) = 0.0
       enddo
      enddo

!-----integration over spectral bands

      do 1000 ibn=1,ib_lw

!-----if h2otbl, compute h2o (line) transmittance using table look-up.
!     if conbnd, compute h2o (continuum) transmittance in bands 3-7.
!     if co2bnd, compute co2 transmittance in band 3.
!     if oznbnd, compute  o3 transmittance in band 5.
!     if n2obnd, compute n2o transmittance in bands 6 and 7.
!     if ch4bnd, compute ch4 transmittance in bands 6 and 7.
!     if combnd, compute co2-minor transmittance in bands 4 and 5.
!     if f11bnd, compute cfc11 transmittance in bands 4 and 5.
!     if f12bnd, compute cfc12 transmittance in bands 4 and 6.
!     if f22bnd, compute cfc22 transmittance in bands 4 and 6.
!     if b10bnd, compute flux reduction due to n2o in band 10.
       h2otbl=high.and.(ibn.eq.1.or.ibn.eq.2.or.ibn.eq.8)
       conbnd=ibn.ge.3.and.ibn.le.7
       co2bnd=ibn.eq.3
       oznbnd=ibn.eq.5
       n2obnd=ibn.eq.6.or.ibn.eq.7
       ch4bnd=ibn.eq.6.or.ibn.eq.7
       combnd=ibn.eq.4.or.ibn.eq.5
       f11bnd=ibn.eq.4.or.ibn.eq.5
       f12bnd=ibn.eq.4.or.ibn.eq.6
       f22bnd=ibn.eq.4.or.ibn.eq.6
       b10bnd=ibn.eq.10
       if (.not. b10bnd .or. trace) then          ! skip b10 and .not.trace
!-----blayer is the spectrally integrated planck flux of the mean layer
!     temperature derived from eq. (3.11)
!     the fitting for the planck flux is valid for the range 160-345 k.
       do k=1,np
        do i=1,m
          blayer(i,k)=ta(i,k)*(ta(i,k)*(ta(i,k)*(ta(i,k) &
                     *(ta(i,k)*cb(6,ibn)+cb(5,ibn))+cb(4,ibn)) &
                     +cb(3,ibn))+cb(2,ibn))+cb(1,ibn)
        enddo
       enddo
       do i=1,m
!-----the earth's surface, with index "np+1", is treated as a layer.
!     index "0" is the layer above the top of the atmosphere.
         blayer(i,np+1)=(ts(i)*(ts(i)*(ts(i)*(ts(i) &
                       *(ts(i)*cb(6,ibn)+cb(5,ibn))+cb(4,ibn)) &
                       +cb(3,ibn))+cb(2,ibn))+cb(1,ibn))*emiss(i,ibn)
         blayer(i,0)   = 0.0
!-----dbs is the derivative of the surface emission with respect to
!     surface temperature eq. (3.12).
        dbs(i)=(ts(i)*(ts(i)*(ts(i)*(ts(i)*5.*cb(6,ibn)+4.*cb(5,ibn)) &
              +3.*cb(4,ibn))+2.*cb(3,ibn))+cb(2,ibn))*emiss(i,ibn)
       enddo
!-----difference in planck functions between adjacent layers.
       do k=1,np+1
        do i=1,m
         dblayr(i,k)=blayer(i,k-1)-blayer(i,k)
        enddo
       enddo
!------interpolate planck function at model levels
       do k=2,np
        do i=1,m
         blevel(i,k)=(blayer(i,k-1)*dp(i,k)+blayer(i,k)*dp(i,k-1))/ &
                     (dp(i,k-1)+dp(i,k))
        enddo
       enddo
       do i=1,m
         blevel(i,1)=blayer(i,1)+(blayer(i,1)-blayer(i,2))*dp(i,1)/ &
                     (dp(i,1)+dp(i,2))
         blevel(i,np+1)=tb(i)*(tb(i)*(tb(i)*(tb(i) &
                       *(tb(i)*cb(6,ibn)+cb(5,ibn))+cb(4,ibn)) &
                       +cb(3,ibn))+cb(2,ibn))+cb(1,ibn)
       enddo

!-----compute column-integrated absorber amoumt, absorber-weighted
!     pressure and temperature for co2 (band 3) and o3 (band 5).
!     it follows eqs. (4.13) and (4.14).
!-----this is in the band loop to save storage

      if (high .and. co2bnd) then
        call column(m,np,pa,dt,dco2,sco3,scopre,scotem)
      endif
      if (oznbnd) then
        call column(m,np,pa,dt,do3,sco3,scopre,scotem)
      endif

!-----compute cloud optical thickness following eqs. (6.4a,b) and (6.7)
!     rain optical thickness is set to 0.00307 /(gm/m**2).
!     it is for a specific drop size distribution provided by q. fu.

      if (cldwater) then
       do k=1,np
        do i=1,m
          taucl(i,k,1)=cwp(i,k,1)*(aib(1,ibn)+aib(2,ibn)/ &
            reff(i,k,1)**aib(3,ibn))
          taucl(i,k,2)=cwp(i,k,2)*(awb(1,ibn)+(awb(2,ibn)+ &
            (awb(3,ibn)+awb(4,ibn)*reff(i,k,2))*reff(i,k,2)) &
            *reff(i,k,2))
          taucl(i,k,3)=0.00307*cwp(i,k,3)
        enddo
       enddo
      endif

!-----compute cloud single-scattering albedo and asymmetry factor for
!     a mixture of ice particles and liquid drops following
!     eqs. (6.5), (6.6), (6.11) and (6.12).
!     single-scattering albedo and asymmetry factor of rain are set
!     to 0.54 and 0.95, respectively, based on the information provided
!     by prof. qiang fu.

       do k=1,np
        do i=1,m
           tcldlyr(i,k) = 1.0
           taux=taucl(i,k,1)+taucl(i,k,2)+taucl(i,k,3)
          if (taux.gt.0.02 .and. fcld(i,k).gt.0.01) then
            reff1=min(reff(i,k,1),150._fp_kind)
            reff2=min(reff(i,k,2),20.0_fp_kind)
           w1=taucl(i,k,1)*(aiw(1,ibn)+(aiw(2,ibn)+(aiw(3,ibn) &
             +aiw(4,ibn)*reff1)*reff1)*reff1)
           w2=taucl(i,k,2)*(aww(1,ibn)+(aww(2,ibn)+(aww(3,ibn) &
             +aww(4,ibn)*reff2)*reff2)*reff2)
           w3=taucl(i,k,3)*0.54
           ww=(w1+w2+w3)/taux
           g1=w1*(aig(1,ibn)+(aig(2,ibn)+(aig(3,ibn) &
            +aig(4,ibn)*reff1)*reff1)*reff1)
           g2=w2*(awg(1,ibn)+(awg(2,ibn)+(awg(3,ibn) &
            +awg(4,ibn)*reff2)*reff2)*reff2)
           g3=w3*0.95
           gg=(g1+g2+g3)/(w1+w2+w3)
!-----parameterization of lw scattering following eqs. (6.8) and (6.9).
           ff=0.5+(0.3739+(0.0076+0.1185*gg)*gg)*gg
           taux=taux*(1.-ww*ff)
!-----compute cloud diffuse transmittance. it is approximated by using
!     a diffusivity factor of 1.66.
           tauxa=max(0._fp_kind,1.66*taux)
           tcldlyr(i,k)=0.
           if(tauxa.lt.80.)tcldlyr(i,k)=exp(-tauxa)
          endif
        enddo
       enddo
!-----for aerosol diffuse transmittance
!     the same scaling of cloud optical thickness is applied to aerosols
       do k=1,np
        do i=1,m
           taerlyr(i,k)=1.0
          if (taual(i,k,ibn).gt.0.01) then
           ff=0.5+(0.3739+(0.0076+0.1185*asyal(i,k,ibn)) &
            *asyal(i,k,ibn))*asyal(i,k,ibn)
           taux=taual(i,k,ibn)*(1.-ssaal(i,k,ibn)*ff)
           taerlyr(i,k)=exp(-1.66*taux)
          endif
        enddo
       enddo
!-----compute the exponential terms (eq. 8.18) at each layer due to
!     water vapor line absorption when k-distribution is used
      if (.not.h2otbl .and. .not.b10bnd) then
        call h2oexps(ibn,m,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp)
      endif
!-----compute the exponential terms (eq. 8.18) at each layer due to
!     water vapor continuum absorption
      if (conbnd) then
        call conexps(ibn,m,np,dcont,xke,conexp)
      endif
!-----compute the exponential terms (eq. 8.18) at each layer due to
!     co2 absorption
      if (.not.high .and. co2bnd) then
        call co2exps(m,np,dco2,pa,dt,co2exp)
      endif
!***** for trace gases *****
      if (trace) then
!-----compute the exponential terms at each layer due to n2o absorption
       if (n2obnd) then
        call n2oexps(ibn,m,np,dn2o,pa,dt,n2oexp)
       endif
!-----compute the exponential terms at each layer due to ch4 absorption
       if (ch4bnd) then
        call ch4exps(ibn,m,np,dch4,pa,dt,ch4exp)
       endif
!-----compute the exponential terms due to co2 minor absorption
       if (combnd) then
        call comexps(ibn,m,np,dco2,dt,comexp)
       endif
!-----compute the exponential terms due to cfc11 absorption.
!     the values of the parameters are given in table 7.
       if (f11bnd) then
            a1  = 1.26610e-3
            b1  = 3.55940e-6
            fk1 = 1.89736e+1
            a2  = 8.19370e-4
            b2  = 4.67810e-6
            fk2 = 1.01487e+1
        call cfcexps(ibn,m,np,a1,b1,fk1,a2,b2,fk2,df11,dt,f11exp)
       endif
!-----compute the exponential terms due to cfc12 absorption.
       if (f12bnd) then
            a1  = 8.77370e-4
            b1  =-5.88440e-6
            fk1 = 1.58104e+1
            a2  = 8.62000e-4
            b2  =-4.22500e-6
            fk2 = 3.70107e+1
        call cfcexps(ibn,m,np,a1,b1,fk1,a2,b2,fk2,df12,dt,f12exp)
       endif
!-----compute the exponential terms due to cfc22 absorption.
       if (f22bnd) then
            a1  = 9.65130e-4
            b1  = 1.31280e-5
            fk1 = 6.18536e+0
            a2  =-3.00010e-5
            b2  = 5.25010e-7
            fk2 = 3.27912e+1
        call cfcexps(ibn,m,np,a1,b1,fk1,a2,b2,fk2,df22,dt,f22exp)
       endif
!-----compute the exponential terms at each layer in band 10 due to
!     h2o line and continuum, co2, and n2o absorption
       if (b10bnd) then
        call b10exps(m,np,dh2o,dcont,dco2,dn2o,pa,dt &
                    ,h2oexp,conexp,co2exp,n2oexp)
       endif
      endif
!-----compute transmittances for regions between levels k1 and k2
!     and update fluxes at the two levels.
!-----initialize fluxes
      do k=1,np+1
       do i=1,m
         flxu(i,k) = 0.0
         flxd(i,k) = 0.0
         flcu(i,k) = 0.0
         flcd(i,k) = 0.0
       enddo
      enddo
      do 2000 k1=1,np
!-----initialization
!
!     it, im, and ib are the numbers of cloudy layers in the high,
!     middle, and low cloud groups between levels k1 and k2.
!     cldlw, cldmd, and cldhi are the equivalent black-cloud fractions
!     of low, middle, and high troposphere.
!     tranal is the aerosol transmission function
        do i=1,m
          it(i) = 0
          im(i) = 0
          ib(i) = 0
          cldlw(i) = 0.0
          cldmd(i) = 0.0
          cldhi(i) = 0.0
          tranal(i)= 1.0
        enddo
!-----for h2o line transmission
      if (.not. h2otbl) then
        do ik=1,6
         do i=1,m
           th2o(i,ik)=1.0
         enddo
        enddo
      endif
!-----for h2o continuum transmission
         do iq=1,3
          do i=1,m
            tcon(i,iq)=1.0
          enddo
         enddo
!-----for co2 transmission using k-distribution method.
!     band 3 is divided into 3 sub-bands, but sub-bands 3a and 3c
!     are combined in computing the co2 transmittance.
       if (.not.high .and. co2bnd) then
         do isb=1,2
          do ik=1,6
           do i=1,m
             tco2(i,ik,isb)=1.0
           enddo
          enddo
         enddo
       endif
!***** for trace gases *****
      if (trace) then
!-----for n2o transmission using k-distribution method.
       if (n2obnd) then
          do ik=1,4
           do i=1,m
             tn2o(i,ik)=1.0
           enddo
          enddo
       endif
!-----for ch4 transmission using k-distribution method.
       if (ch4bnd) then
          do ik=1,4
           do i=1,m
             tch4(i,ik)=1.0
           enddo
          enddo
       endif
!-----for co2-minor transmission using k-distribution method.
       if (combnd) then
          do ik=1,6
           do i=1,m
             tcom(i,ik)=1.0
           enddo
          enddo
       endif
!-----for cfc-11 transmission using k-distribution method.
       if (f11bnd) then
           do i=1,m
             tf11(i)=1.0
           enddo
       endif
!-----for cfc-12 transmission using k-distribution method.
       if (f12bnd) then
           do i=1,m
             tf12(i)=1.0
           enddo
       endif
!-----for cfc-22 transmission when using k-distribution method.
       if (f22bnd) then
           do i=1,m
             tf22(i)=1.0
           enddo
       endif
!-----for the transmission in band 10 using k-distribution method.
       if (b10bnd) then
          do ik=1,5
           do i=1,m
              th2o(i,ik)=1.0
           enddo
          enddo
          do ik=1,6
           do i=1,m
              tco2(i,ik,1)=1.0
           enddo
          enddo
          do i=1,m
             tcon(i,1)=1.0
          enddo
          do ik=1,2
            do i=1,m
              tn2o(i,ik)=1.0
           enddo
          enddo
       endif
      endif
!***** end trace gases *****
       do i=1,m
        fclr(i)=1.0
       enddo
!-----loop over the bottom level of the region (k2)
      do 3000 k2=k1+1,np+1
!-----trant is the total transmittance between levels k1 and k2.
          do i=1,m
           trant(i)=1.0
          enddo
      if (h2otbl) then
!-----compute water vapor transmittance using table look-up.
!     the following values are taken from table 8.
          w1=-8.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          if (ibn.eq.1) then
           call tablup(k1,k2,m,np,nx2,nh,sh2o,swpre,swtem, &
                       w1,p1,dwe,dpe,h11,h12,h13,trant)
          endif
          if (ibn.eq.2) then
           call tablup(k1,k2,m,np,nx2,nh,sh2o,swpre,swtem, &
                       w1,p1,dwe,dpe,h21,h22,h23,trant)
          endif
          if (ibn.eq.8) then
           call tablup(k1,k2,m,np,nx2,nh,sh2o,swpre,swtem, &
                       w1,p1,dwe,dpe,h81,h82,h83,trant)
          endif
      else
!-----compute water vapor transmittance using k-distribution
       if (.not.b10bnd) then
        call h2okdis(ibn,m,np,k2-1,fkw,gkw,ne,h2oexp,conexp, &
                     th2o,tcon,trant)
       endif
      endif
      if (co2bnd) then
        if (high) then
!-----compute co2 transmittance using table look-up method.
!     the following values are taken from table 8.
          w1=-4.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          call tablup(k1,k2,m,np,nx2,nc,sco3,scopre,scotem, &
                      w1,p1,dwe,dpe,c1,c2,c3,trant)
       else
!-----compute co2 transmittance using k-distribution method
          call co2kdis(m,np,k2-1,co2exp,tco2,trant)
        endif
      endif
!-----always use table look-up to compute o3 transmittance.
!     the following values are taken from table 8.
      if (oznbnd) then
          w1=-6.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          call tablup(k1,k2,m,np,nx2,no,sco3,scopre,scotem, &
                      w1,p1,dwe,dpe,o1,o2,o3,trant)
      endif
!***** for trace gases *****
      if (trace) then
!-----compute n2o transmittance using k-distribution method
       if (n2obnd) then
          call n2okdis(ibn,m,np,k2-1,n2oexp,tn2o,trant)
       endif
!-----compute ch4 transmittance using k-distribution method
       if (ch4bnd) then
          call ch4kdis(ibn,m,np,k2-1,ch4exp,tch4,trant)
       endif
!-----compute co2-minor transmittance using k-distribution method
       if (combnd) then
          call comkdis(ibn,m,np,k2-1,comexp,tcom,trant)
       endif
!-----compute cfc11 transmittance using k-distribution method
       if (f11bnd) then
          call cfckdis(m,np,k2-1,f11exp,tf11,trant)
       endif
!-----compute cfc12 transmittance using k-distribution method
       if (f12bnd) then
          call cfckdis(m,np,k2-1,f12exp,tf12,trant)
       endif
!-----compute cfc22 transmittance using k-distribution method
       if (f22bnd) then
          call cfckdis(m,np,k2-1,f22exp,tf22,trant)
       endif
!-----compute transmittance in band 10 using k-distribution method.
!     for band 10, trant is the change in transmittance due to n2o
!     absorption.
       if (b10bnd) then
          call b10kdis(m,np,k2-1,h2oexp,conexp,co2exp,n2oexp &
                      ,th2o,tcon,tco2,tn2o,trant)
       endif
      endif
!*****   end trace gases  *****
!-----include aerosol effect
      do i=1,m
         tranal(i)=tranal(i)*taerlyr(i,k2-1)
         trant (i)=trant(i) *tranal(i)
      enddo
!***** cloud overlapping *****
      if (.not. overcast) then
        call cldovlp (m,np,k2,ict,icb,it,im,ib, &
                 cldhi,cldmd,cldlw,fcld,tcldlyr,fclr)
      else
       do i=1,m
        fclr(i)=fclr(i)*tcldlyr(i,k2-1)
       enddo
      endif
!-----compute upward and downward fluxes (bands 1-9). it follows
!     eqs. (8.14) and (8.15). downward fluxes are positive.
      if (.not. b10bnd) then
!-----contribution from the "adjacent layer"
       if (k2 .eq. k1+1) then
        do i=1,m
         yy=min(0.999_fp_kind,trant(i))
         yy=max(0.001_fp_kind,yy)
!-hmhj use log instead of alog for default intrinsic function
         xx=(blevel(i,k1)-blevel(i,k2))/ log(yy)
         bu=(blevel(i,k1)-blevel(i,k2)*yy)/(1.0-yy)+xx
         bd=(blevel(i,k2)-blevel(i,k1)*yy)/(1.0-yy)-xx
!                bu=blayer(i,k1)
!                bd=blayer(i,k1)
!-----for clear-sky situation
         flcu(i,k1)=flcu(i,k1)-bu+(bu-blayer(i,k2))*trant(i)
         flcd(i,k2)=flcd(i,k2)+bd-(bd-blayer(i,k1-1))*trant(i)
!-----for all-sky situation
         flxu(i,k1)=flxu(i,k1)-bu+(bu-blayer(i,k2))*trant(i)*fclr(i)
         flxd(i,k2)=flxd(i,k2)+bd-(bd-blayer(i,k1-1))*trant(i)*fclr(i)
        enddo
       else
!-----contribution from distant layers.
         do i=1,m
          xx=trant(i)*dblayr(i,k2)
          flcu(i,k1) =flcu(i,k1)+xx
          flxu(i,k1) =flxu(i,k1)+xx*fclr(i)
          xx=trant(i)*dblayr(i,k1)
          flcd(i,k2) =flcd(i,k2)+xx
          flxd(i,k2) =flxd(i,k2)+xx*fclr(i)
         enddo
        endif
       else
!-----flux reduction due to n2o in band 10 (eqs. 5.1 and 5.2)
!     trant is the transmittance change due to n2o absorption (eq. 5.3).
       do i=1,m
        rflx(i,k1) = rflx(i,k1)+trant(i)*fclr(i)*dblayr(i,k2)
        rflx(i,k2) = rflx(i,k2)+trant(i)*fclr(i)*dblayr(i,k1)
        rflc(i,k1) = rflc(i,k1)+trant(i)*dblayr(i,k2)
        rflc(i,k2) = rflc(i,k2)+trant(i)*dblayr(i,k1)
       enddo
      endif
 3000 continue
!-----here, fclr and trant are, respectively, the clear line-of-sight
!     and the transmittance between k1 and the surface.
       do i=1,m
         trantcr(i,k1) =trant(i)
         transfc(i,k1) =trant(i)*fclr(i)
       enddo
!-----compute the partial derivative of fluxes with respect to
!     surface temperature (eq. 3.12). note: upward flux is negative.
       do i=1,m
         dfdts(i,k1) =dfdts(i,k1)-dbs(i)*transfc(i,k1)
       enddo
 2000 continue
      if (.not. b10bnd) then
!-----for surface emission.
!     note: blayer(i,np+1) and dbs include the surface emissivity effect.
        do i=1,m
          flcu(i,np+1)=-blayer(i,np+1)
          flxu(i,np+1)=-blayer(i,np+1)
          sfcem(i)=sfcem(i)-blayer(i,np+1)
          dfdts(i,np+1)=dfdts(i,np+1)-dbs(i)
        enddo
!-----add the flux reflected by the surface. (last term on the
!     rhs of eq. 3.10)
        do k=1,np+1
         do i=1,m
           flcu(i,k)=flcu(i,k)- &
                flcd(i,np+1)*trantcr(i,k)*(1.-emiss(i,ibn))
           flxu(i,k)=flxu(i,k)- &
                flxd(i,np+1)*transfc(i,k)*(1.-emiss(i,ibn))
         enddo
        enddo
      endif
!-----summation of fluxes over spectral bands
      do k=1,np+1
       do i=1,m
         flc(i,k)=flc(i,k)+flcd(i,k)+flcu(i,k)
         flx(i,k)=flx(i,k)+flxd(i,k)+flxu(i,k)
         acflxu(i,k)=acflxu(i,k)+flxu(i,k)   ! (LW upward must hold negative values)
         acflxd(i,k)=acflxd(i,k)+flxd(i,k)   ! (LW downward must hold postive values, and should=0 a
       enddo
      enddo
!-----adjustment due to n2o absorption in band 10. eqs. (5.4) and (5.5)
       if (b10bnd) then
        do k=1,np+1
         do i=1,m
          flc(i,k)=flc(i,k)+rflc(i,k)
          flx(i,k)=flx(i,k)+rflx(i,k)
          if(rflx(i,k).ge.0.0) acflxd(i,k)=acflxd(i,k)+rflx(i,k)
          if(rflx(i,k).lt.0.0) acflxu(i,k)=acflxu(i,k)+rflx(i,k)
         enddo
        enddo
       endif
      endif                            ! endif (.not. b10bnd .or. trace)
 1000 continue

!ccshie 8/19/04 based on D. Johnson GCSS Workshop
!      do i=1,m
!         (i)=acflxd(i,np+1)    ! downward LW surface
!        rflux(i,5)=acflxu(i,1)       ! upward LW TOA
!        rflux(i,7)=acflxu(i,np+1)    ! upward LW surface
!        rflux(i,8)=acflxd(i,1)       ! downward LW TOA
!      enddo

!xp
!      deallocate(pa)
!      deallocate(dt)
!      deallocate(sh2o)
!      deallocate(swpre)
!      deallocate(swtem)
!      deallocate(sco3)
!      deallocate(scopre)
!      deallocate(scotem)
!      deallocate(dh2o)
!      deallocate(dcont)
!      deallocate(dco2)
!      deallocate(do3)
!      deallocate(dn2o)
!      deallocate(dch4)
!      deallocate(df11)
!      deallocate(df12)
!      deallocate(df22)
!      deallocate(th2o)
!      deallocate(tcon)
!      deallocate(tco2)
!      deallocate(tn2o)
!      deallocate(tch4)
!      deallocate(tcom)
!      deallocate(tf11)
!      deallocate(tf12)
!      deallocate(tf22)
!      deallocate(h2oexp)
!      deallocate(conexp)
!      deallocate(co2exp)
!      deallocate(n2oexp)
!      deallocate(ch4exp)
!      deallocate(comexp)
!      deallocate(f11exp)
!      deallocate(f12exp)
!      deallocate(f22exp)
!      deallocate(blayer)
!      deallocate(blevel)
!      deallocate(dblayr)
!      deallocate(dbs)
!      deallocate(dp)
!      deallocate(cwp)
!      deallocate(trant)
!      deallocate(tranal)
!      deallocate(transfc)
!      deallocate(trantcr)
!      deallocate(flxu)
!      deallocate(flxd)
!      deallocate(flcu)
!      deallocate(flcd)
!      deallocate(rflx)
!      deallocate(rflc)
!      deallocate(it)
!      deallocate(im)
!      deallocate(ib)
!      deallocate(cldhi)
!      deallocate(cldmd)
!      deallocate(cldlw)
!      deallocate(tcldlyr)
!      deallocate(fclr)
!      deallocate(taerlyr)
    return
  end subroutine lwrad

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine column (m,np,pa,dt,sabs0,sabs,spre,stem)
!***********************************************************************
!-----compute column-integrated (from top of the model atmosphere)
!     absorber amount (sabs), absorber-weighted pressure (spre) and
!     temperature (stem).
!     computations follow eqs. (8.24) - (8.26).
!
!--- input parameters
!   number of soundings (m)
!   number of atmospheric layers (np)
!   layer pressure (pa)
!   layer temperature minus 250k (dt)
!   layer absorber amount (sabs0)
!
!--- output parameters
!   column-integrated absorber amount (sabs)
!   column absorber-weighted pressure (spre)
!   column absorber-weighted temperature (stem)
!
!--- units of pa and dt are mb and k, respectively.
!    units of sabs are g/cm**2 for water vapor and (cm-atm)stp
!    for co2 and o3
!***********************************************************************
      implicit none
      integer m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) pa(m,np),dt(m,np),sabs0(m,np)
!---- output parameters -----
      real(Kind=fp_kind) sabs(m,np+1),spre(m,np+1),stem(m,np+1)
!*********************************************************************
        do i=1,m
          sabs(i,1)=0.0
          spre(i,1)=0.0
          stem(i,1)=0.0
        enddo
        do k=1,np
         do i=1,m
           sabs(i,k+1)=sabs(i,k)+sabs0(i,k)
           spre(i,k+1)=spre(i,k)+pa(i,k)*sabs0(i,k)
           stem(i,k+1)=stem(i,k)+dt(i,k)*sabs0(i,k)
         enddo
        enddo
       return
       end subroutine column

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine h2oexps(ib,m,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp)
!**********************************************************************
!   compute exponentials for water vapor line absorption
!   in individual layers using eqs. (8.18) and (8.19).
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer water vapor amount for line absorption (dh2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!  absorption coefficients for the first k-distribution
!     function due to h2o line absorption (xkw)
!  coefficients for the temperature and pressure scaling (aw,bw,pm)
!  ratios between neighboring absorption coefficients for
!     h2o line absorption (mw)
!
!---- output parameters
!  6 exponentials for each layer  (h2oexp)
!**********************************************************************
      implicit none
      integer ib,m,np,i,k,ik
!---- input parameters ------
      real(Kind=fp_kind) dh2o(m,np),pa(m,np),dt(m,np)
!---- output parameters -----
      real(Kind=fp_kind) h2oexp(m,np,6)
!---- static data -----
      integer mw(9)
      real(Kind=fp_kind) xkw(9),aw(9),bw(9),pm(9)
!---- temporary arrays -----
      real(Kind=fp_kind) xh
!**********************************************************************
!    note that the 3 sub-bands in band 3 use the same set of xkw, aw,
!    and bw,  therefore, h2oexp for these sub-bands are identical.
!**********************************************************************
        do k=1,np
         do i=1,m
!-----xh is the scaled water vapor amount for line absorption
!     computed from eq. (4.4).
           xh = dh2o(i,k)*(pa(i,k)/500.)**pm(ib) &
              * ( 1.+(aw(ib)+bw(ib)* dt(i,k))*dt(i,k) )
!-----h2oexp is the water vapor transmittance of the layer k
!     due to line absorption
           h2oexp(i,k,1) = exp(-xh*xkw(ib))
         enddo
        enddo
!-----compute transmittances from eq. (8.19)
         if (mw(ib).eq.6) then
        do ik=2,6
          do k=1,np
           do i=1,m
             xh = h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
               if(xh.lt.1.e-4)xh=0.
             h2oexp(i,k,ik) = xh*xh*xh
           enddo
          enddo
        enddo
        elseif (mw(ib).eq.8) then
        do ik=2,6
          do k=1,np
           do i=1,m
             xh = h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             if(xh.lt.1.e-3)xh=0.
             xh = xh*xh
             h2oexp(i,k,ik) = xh*xh
           enddo
          enddo
        enddo
        elseif (mw(ib).eq.9) then
        do ik=2,6
          do k=1,np
           do i=1,m
             xh=h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             if(xh.lt.1.e-4)xh=0.
             h2oexp(i,k,ik) = xh*xh*xh
           enddo
          enddo
        enddo
        else
        do ik=2,6
          do k=1,np
           do i=1,m
             xh = h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             if(xh.lt.2.e-2)xh=0.
             xh = xh*xh
             xh = xh*xh
             h2oexp(i,k,ik) = xh*xh
           enddo
          enddo
        enddo
        endif
      return
      end subroutine h2oexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine conexps(ib,m,np,dcont,xke,conexp)
!**********************************************************************
!   compute exponentials for continuum absorption in individual layers.
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer scaled water vapor amount for continuum absorption (dcont)
!  absorption coefficients for the first k-distribution function
!     due to water vapor continuum absorption (xke)
!
!---- output parameters
!  1 or 3 exponentials for each layer (conexp)
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters ------
      real(Kind=fp_kind) dcont(m,np)
!---- updated parameters -----
      real(Kind=fp_kind) conexp(m,np,3)
!---- static data -----
      real(Kind=fp_kind) xke(9)
!****************************************************************
        do k=1,np
         do i=1,m
           conexp(i,k,1) = exp(-dcont(i,k)*xke(ib))
         enddo
        enddo
       if (ib .eq. 3) then
!-----the absorption coefficients for sub-bands 3b and 3a are, respectively,
!     two and four times the absorption coefficient for sub-band 3c (table 9).
!     note that conexp(i,k,3) is for sub-band 3a.
         do k=1,np
          do i=1,m
            conexp(i,k,2) = conexp(i,k,1) *conexp(i,k,1)
            conexp(i,k,3) = conexp(i,k,2) *conexp(i,k,2)
          enddo
         enddo
       endif
      return
      end subroutine conexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine co2exps(m,np,dco2,pa,dt,co2exp)
!**********************************************************************
!   compute co2 exponentials for individual layers.
!
!---- input parameters
!  number of grid intervals (m)
!  number of layers (np)
!  layer co2 amount (dco2)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  6 exponentials for each layer (co2exp)
!**********************************************************************
      implicit none
      integer m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) dco2(m,np),pa(m,np),dt(m,np)
!---- output parameters -----
      real(Kind=fp_kind) co2exp(m,np,6,2)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!**********************************************************************
        do k=1,np
         do i=1,m
!-----the scakubg oaraneters are given in table 3, and values of
!     the absorption coefficient are given in table 10.
!     scaled co2 amount for band-wings (sub-bands 3a and 3c)
           xc = dco2(i,k)*(pa(i,k)/300.0)**0.5 &
                   *(1.+(0.0182+1.07e-4*dt(i,k))*dt(i,k))
!-----six exponentials by powers of 8 (see eqs. 8.18, 8.19 and table 10).
           co2exp(i,k,1,1)=exp(-xc*2.656e-5)
           xc=co2exp(i,k,1,1)*co2exp(i,k,1,1)
           xc=xc*xc
           co2exp(i,k,2,1)=xc*xc
           xc=co2exp(i,k,2,1)*co2exp(i,k,2,1)
           xc=xc*xc
           co2exp(i,k,3,1)=xc*xc
           xc=co2exp(i,k,3,1)*co2exp(i,k,3,1)
           xc=xc*xc
           co2exp(i,k,4,1)=xc*xc
           xc=co2exp(i,k,4,1)*co2exp(i,k,4,1)
           xc=xc*xc
           co2exp(i,k,5,1)=xc*xc
           xc=co2exp(i,k,5,1)*co2exp(i,k,5,1)
           xc=xc*xc
           co2exp(i,k,6,1)=xc*xc
!-----for band-center region (sub-band 3b)
           xc = dco2(i,k)*(pa(i,k)/30.0)**0.85 &
                   *(1.+(0.0042+2.00e-5*dt(i,k))*dt(i,k))
           co2exp(i,k,1,2)=exp(-xc*2.656e-3)
           xc=co2exp(i,k,1,2)*co2exp(i,k,1,2)
           xc=xc*xc
           co2exp(i,k,2,2)=xc*xc
           xc=co2exp(i,k,2,2)*co2exp(i,k,2,2)
           xc=xc*xc
           co2exp(i,k,3,2)=xc*xc
           xc=co2exp(i,k,3,2)*co2exp(i,k,3,2)
           xc=xc*xc
           co2exp(i,k,4,2)=xc*xc
           xc=co2exp(i,k,4,2)*co2exp(i,k,4,2)
           xc=xc*xc
           co2exp(i,k,5,2)=xc*xc
           xc=co2exp(i,k,5,2)*co2exp(i,k,5,2)
           xc=xc*xc
           co2exp(i,k,6,2)=xc*xc
         enddo
        enddo
      return
      end subroutine co2exps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine n2oexps(ib,m,np,dn2o,pa,dt,n2oexp)
!**********************************************************************
!   compute n2o exponentials for individual layers
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer n2o amount (dn2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  2 or 4 exponentials for each layer (n2oexp)
!**********************************************************************
      implicit none
      integer ib,m,np,i,k

!---- input parameters -----

      real(Kind=fp_kind) dn2o(m,np),pa(m,np),dt(m,np)

!---- output parameters -----

      real(Kind=fp_kind) n2oexp(m,np,4)

!---- temporary arrays -----

      real(Kind=fp_kind) xc,xc1,xc2

!-----scaling and absorpton data are given in table 5.
!     transmittances are computed using eqs. (8.18) and (8.19).

       do k=1,np
        do i=1,m

!-----four exponential by powers of 21 for band 6.

          if (ib.eq.6) then

           xc=dn2o(i,k)*(1.+(1.9297e-3+4.3750e-6*dt(i,k))*dt(i,k))
           n2oexp(i,k,1)=exp(-xc*6.31582e-2)

           xc=n2oexp(i,k,1)*n2oexp(i,k,1)*n2oexp(i,k,1)
           xc1=xc*xc
           xc2=xc1*xc1
           n2oexp(i,k,2)=xc*xc1*xc2

!-----four exponential by powers of 8 for band 7

          else

           xc=dn2o(i,k)*(pa(i,k)/500.0)**0.48 &
              *(1.+(1.3804e-3+7.4838e-6*dt(i,k))*dt(i,k))
           n2oexp(i,k,1)=exp(-xc*5.35779e-2)

           xc=n2oexp(i,k,1)*n2oexp(i,k,1)
           xc=xc*xc
           n2oexp(i,k,2)=xc*xc
           xc=n2oexp(i,k,2)*n2oexp(i,k,2)
           xc=xc*xc
           n2oexp(i,k,3)=xc*xc
           xc=n2oexp(i,k,3)*n2oexp(i,k,3)
           xc=xc*xc
           n2oexp(i,k,4)=xc*xc

          endif

        enddo
       enddo

      return
      end subroutine n2oexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine ch4exps(ib,m,np,dch4,pa,dt,ch4exp)
!**********************************************************************
!   compute ch4 exponentials for individual layers
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer ch4 amount (dch4)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  1 or 4 exponentials for each layer (ch4exp)
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) dch4(m,np),pa(m,np),dt(m,np)
!---- output parameters -----
      real(Kind=fp_kind) ch4exp(m,np,4)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!*****  scaling and absorpton data are given in table 5  *****
       do k=1,np
        do i=1,m
!-----four exponentials for band 6
          if (ib.eq.6) then
           xc=dch4(i,k)*(1.+(1.7007e-2+1.5826e-4*dt(i,k))*dt(i,k))
           ch4exp(i,k,1)=exp(-xc*5.80708e-3)
!-----four exponentials by powers of 12 for band 7
          else
           xc=dch4(i,k)*(pa(i,k)/500.0)**0.65 &
             *(1.+(5.9590e-4-2.2931e-6*dt(i,k))*dt(i,k))
           ch4exp(i,k,1)=exp(-xc*6.29247e-2)
           xc=ch4exp(i,k,1)*ch4exp(i,k,1)*ch4exp(i,k,1)
           xc=xc*xc
           ch4exp(i,k,2)=xc*xc
           xc=ch4exp(i,k,2)*ch4exp(i,k,2)*ch4exp(i,k,2)
           xc=xc*xc
           ch4exp(i,k,3)=xc*xc
           xc=ch4exp(i,k,3)*ch4exp(i,k,3)*ch4exp(i,k,3)
           xc=xc*xc
           ch4exp(i,k,4)=xc*xc
          endif
        enddo
       enddo
      return
      end subroutine ch4exps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine comexps(ib,m,np,dcom,dt,comexp)
!**********************************************************************
!   compute co2-minor exponentials for individual layers using
!   eqs. (8.18) and (8.19).
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  layer co2 amount (dcom)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  6 exponentials for each layer (comexp)
!**********************************************************************
      implicit none
      integer ib,m,np,i,k,ik
!---- input parameters -----
      real(Kind=fp_kind) dcom(m,np),dt(m,np)
!---- output parameters -----
      real(Kind=fp_kind) comexp(m,np,6)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!*****  scaling and absorpton data are given in table 6  *****
       do k=1,np
        do i=1,m
          if (ib.eq.4) then
           xc=dcom(i,k)*(1.+(3.5775e-2+4.0447e-4*dt(i,k))*dt(i,k))
          endif
          if (ib.eq.5) then
           xc=dcom(i,k)*(1.+(3.4268e-2+3.7401e-4*dt(i,k))*dt(i,k))
          endif
           comexp(i,k,1)=exp(-xc*1.922e-7)
          do ik=2,6
           xc=comexp(i,k,ik-1)*comexp(i,k,ik-1)
           xc=xc*xc
           comexp(i,k,ik)=xc*comexp(i,k,ik-1)
          enddo
        enddo
       enddo
      return
      end subroutine comexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cfcexps(ib,m,np,a1,b1,fk1,a2,b2,fk2,dcfc,dt,cfcexp)
!**********************************************************************
!   compute cfc(-11, -12, -22) exponentials for individual layers.
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of layers (np)
!  parameters for computing the scaled cfc amounts
!             for temperature scaling (a1,b1,a2,b2)
!  the absorption coefficients for the
!     first k-distribution function due to cfcs (fk1,fk2)
!  layer cfc amounts (dcfc)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!  1 exponential for each layer (cfcexp)
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) dcfc(m,np),dt(m,np)
!---- output parameters -----
      real(Kind=fp_kind) cfcexp(m,np)
!---- static data -----
      real(Kind=fp_kind) a1,b1,fk1,a2,b2,fk2
!---- temporary arrays -----
      real(Kind=fp_kind) xf
!**********************************************************************
       do k=1,np
        do i=1,m
!-----compute the scaled cfc amount (xf) and exponential (cfcexp)
          if (ib.eq.4) then
           xf=dcfc(i,k)*(1.+(a1+b1*dt(i,k))*dt(i,k))
           cfcexp(i,k)=exp(-xf*fk1)
          else
           xf=dcfc(i,k)*(1.+(a2+b2*dt(i,k))*dt(i,k))
           cfcexp(i,k)=exp(-xf*fk2)
          endif
        enddo
       enddo
      return
      end subroutine cfcexps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine b10exps(m,np,dh2o,dcont,dco2,dn2o,pa,dt &
                ,h2oexp,conexp,co2exp,n2oexp)
!**********************************************************************
!   compute band3a exponentials for individual layers
!
!---- input parameters
!  number of grid intervals (m)
!  number of layers (np)
!  layer h2o amount for line absorption (dh2o)
!  layer h2o amount for continuum absorption (dcont)
!  layer co2 amount (dco2)
!  layer n2o amount (dn2o)
!  layer pressure (pa)
!  layer temperature minus 250k (dt)
!
!---- output parameters
!
!  exponentials for each layer (h2oexp,conexp,co2exp,n2oexp)
!**********************************************************************
      implicit none
      integer m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) dh2o(m,np),dcont(m,np),dn2o(m,np)
      real(Kind=fp_kind) dco2(m,np),pa(m,np),dt(m,np)
!---- output parameters -----
      real(Kind=fp_kind) h2oexp(m,np,6),conexp(m,np,3),co2exp(m,np,6,2) &
          ,n2oexp(m,np,4)
!---- temporary arrays -----
      real(Kind=fp_kind) xx,xx1,xx2,xx3
!**********************************************************************
        do k=1,np
         do i=1,m
!-----compute scaled h2o-line amount for band 10 (eq. 4.4 and table 3).
           xx=dh2o(i,k)*(pa(i,k)/500.0) &
                 *(1.+(0.0149+6.20e-5*dt(i,k))*dt(i,k))
!-----six exponentials by powers of 8
!     the constant 0.10624 is equal to 1.66*0.064
           h2oexp(i,k,1)=exp(-xx*0.10624)
           xx=h2oexp(i,k,1)*h2oexp(i,k,1)
           xx=xx*xx
           h2oexp(i,k,2)=xx*xx
           xx=h2oexp(i,k,2)*h2oexp(i,k,2)
           xx=xx*xx
           h2oexp(i,k,3)=xx*xx
           xx=h2oexp(i,k,3)*h2oexp(i,k,3)
           xx=xx*xx
           h2oexp(i,k,4)=xx*xx
           xx=h2oexp(i,k,4)*h2oexp(i,k,4)
           xx=xx*xx
           h2oexp(i,k,5)=xx*xx
           xx=h2oexp(i,k,5)*h2oexp(i,k,5)
           xx=xx*xx
!          h2oexp(i,k,6)=xx*xx
!-----compute scaled co2 amount for the band 10 (eq. 4.4 and table 6).
           xx=dco2(i,k)*(pa(i,k)/300.0)**0.5 &
                 *(1.+(0.0179+1.02e-4*dt(i,k))*dt(i,k))
!-----six exponentials by powers of 8
!     the constant 2.656e-5 is equal to 1.66*1.60e-5
           co2exp(i,k,1,1)=exp(-xx*2.656e-5)
           xx=co2exp(i,k,1,1)*co2exp(i,k,1,1)
           xx=xx*xx
           co2exp(i,k,2,1)=xx*xx
           xx=co2exp(i,k,2,1)*co2exp(i,k,2,1)
           xx=xx*xx
           co2exp(i,k,3,1)=xx*xx
           xx=co2exp(i,k,3,1)*co2exp(i,k,3,1)
           xx=xx*xx
           co2exp(i,k,4,1)=xx*xx
           xx=co2exp(i,k,4,1)*co2exp(i,k,4,1)
           xx=xx*xx
           co2exp(i,k,5,1)=xx*xx
           xx=co2exp(i,k,5,1)*co2exp(i,k,5,1)
           xx=xx*xx
           co2exp(i,k,6,1)=xx*xx
!-----one exponential of h2o continuum for sub-band 3a (table 9).
            conexp(i,k,1)=exp(-dcont(i,k)*1.04995e+2)
!-----compute the scaled n2o amount for band 10 (table 5).
           xx=dn2o(i,k)*(1.+(1.4476e-3+3.6656e-6*dt(i,k))*dt(i,k))
!-----two exponentials by powers of 58
           n2oexp(i,k,1)=exp(-xx*0.25238)
           xx=n2oexp(i,k,1)*n2oexp(i,k,1)
           xx1=xx*xx
           xx1=xx1*xx1
           xx2=xx1*xx1
           xx3=xx2*xx2
           n2oexp(i,k,2)=xx*xx1*xx2*xx3
         enddo
        enddo
      return
      end subroutine b10exps

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine tablup(k1,k2,m,np,nx,nh,sabs,spre,stem,w1,p1, &
                        dwe,dpe,coef1,coef2,coef3,tran)
!**********************************************************************
!   compute water vapor, co2 and o3 transmittances between level
!   k1 and and level k2 for m soundings, using table look-up.
!
!   calculations follow eq. (4.16).
!
!---- input ---------------------
!  indices for layer (k1) and level (k2)
!  number of grid intervals (m)
!  number of atmospheric layers (np)
!  number of pressure intervals in the table (nx)
!  number of absorber amount intervals in the table (nh)
!  column-integrated absorber amount (sabs)
!  column absorber amount-weighted pressure (spre)
!  column absorber amount-weighted temperature (stem)
!  first value of absorber amount (log10) in the table (w1)
!  first value of pressure (log10) in the table (p1)
!  size of the interval of absorber amount (log10) in the table (dwe)
!  size of the interval of pressure (log10) in the table (dpe)
!  pre-computed coefficients (coef1, coef2, and coef3)
!
!---- updated ---------------------
!  transmittance (tran)
!
!  note:
!   (1) units of sabs are g/cm**2 for water vapor and
!       (cm-atm)stp for co2 and o3.
!   (2) units of spre and stem are, respectively, mb and k.
!
!**********************************************************************
      implicit none
      integer k1,k2,m,np,nx,nh,i
!---- input parameters -----
      real(Kind=fp_kind) w1,p1,dwe,dpe
      real(Kind=fp_kind) sabs(m,np+1),spre(m,np+1),stem(m,np+1)
      real(Kind=fp_kind) coef1(nx,nh),coef2(nx,nh),coef3(nx,nh)
!---- update parameter -----
      real(Kind=fp_kind) tran(m)
!---- temporary variables -----
      real(Kind=fp_kind) x1,x2,x3,we,pe,fw,fp,pa,pb,pc,ax,ba,bb,t1,ca,cb,t2,xxx
      integer iw,ip
!**********************************************************************
      do i=1,m
        x1=sabs(i,k2)-sabs(i,k1)
        we=(log10(x1)-w1)/dwe
       if (we .ge. (w1-2.)) then
        x2=(spre(i,k2)-spre(i,k1))/x1
        x3=(stem(i,k2)-stem(i,k1))/x1
!-----normalize we and pe
        pe=(log10(x2)-p1)/dpe
!-----restrict the magnitudes of the normalized we and pe.
!        we=min(we,real(nh-1))
        xxx=real(nh-1)
        we=min(we,xxx)
        pe=max(pe,0.0_fp_kind)
!        pe=min(pe,real(nx-1))
        xxx=real(nx-1)
        pe=min(pe,xxx)
!-----assign iw and ip and compute the distance of we and pe
!     from iw and ip.
        iw=int(we+1.0)
        iw=min(iw,nh-1)
        iw=max(iw, 2)
        fw=we-float(iw-1)
        ip=int(pe+1.0)
        ip=min(ip,nx-1)
        ip=max(ip, 1)
        fp=pe-float(ip-1)
!-----linear interpolation in pressure
        pa = coef1(ip,iw-1)*(1.-fp)+coef1(ip+1,iw-1)*fp
        pb = coef1(ip,  iw)*(1.-fp)+coef1(ip+1,  iw)*fp
        pc = coef1(ip,iw+1)*(1.-fp)+coef1(ip+1,iw+1)*fp
!-----quadratic interpolation in absorber amount for coef1
        ax = (-pa*(1.-fw)+pc*(1.+fw)) *fw*0.5 + pb*(1.-fw*fw)
!-----linear interpolation in absorber amount for coef2 and coef3
        ba = coef2(ip,  iw)*(1.-fp)+coef2(ip+1,  iw)*fp
        bb = coef2(ip,iw+1)*(1.-fp)+coef2(ip+1,iw+1)*fp
        t1 = ba*(1.-fw) + bb*fw
        ca = coef3(ip,  iw)*(1.-fp)+coef3(ip+1,  iw)*fp
        cb = coef3(ip,iw+1)*(1.-fp)+coef3(ip+1,iw+1)*fp
        t2 = ca*(1.-fw) + cb*fw
!-----update the total transmittance between levels k1 and k2
        tran(i)= (ax + (t1+t2*x3) * x3)*tran(i)
        tran(i)=min(tran(i),0.9999999_fp_kind)
        tran(i)=max(tran(i),0.0000001_fp_kind)
       else
        tran(i)=0.9999999
       endif
      enddo
      return
      end subroutine tablup

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine h2okdis(ib,m,np,k,fkw,gkw,ne,h2oexp,conexp, &
                         th2o,tcon,tran)
!**********************************************************************
!   compute water vapor transmittance between levels k1 and k2 for
!   m soundings, using the k-distribution method.
!
!---- input parameters
!  spectral band (ib)
!  number of grid intervals (m)
!  number of levels (np)
!  current level (k)
!  planck-weighted k-distribution function due to
!    h2o line absorption (fkw)
!  planck-weighted k-distribution function due to
!    h2o continuum absorption (gkw)
!  number of terms used in each band to compute water vapor
!     continuum transmittance (ne)
!  exponentials for line absorption (h2oexp)
!  exponentials for continuum absorption (conexp)
!
!---- updated parameters
!  transmittance between levels k1 and k2 due to
!    water vapor line absorption (th2o)
!  transmittance between levels k1 and k2 due to
!    water vapor continuum absorption (tcon)
!  total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters ------
      real(Kind=fp_kind) conexp(m,np,3),h2oexp(m,np,6)
      integer ne(9)
      real(Kind=fp_kind)  fkw(6,9),gkw(6,3)
!---- updated parameters -----
      real(Kind=fp_kind) th2o(m,6),tcon(m,3),tran(m)
!---- temporary arrays -----
      real(Kind=fp_kind) trnth2o
!-----tco2 are the six exp factors between levels k1 and k2
!     tran is the updated total transmittance between levels k1 and k2
!-----th2o is the 6 exp factors between levels k1 and k2 due to
!     h2o line absorption.
!-----tcon is the 3 exp factors between levels k1 and k2 due to
!     h2o continuum absorption.
!-----trnth2o is the total transmittance between levels k1 and k2 due
!     to both line and continuum absorption.
!-----comoute th2o following eq. (8.20).
         do i=1,m
           th2o(i,1) = th2o(i,1)*h2oexp(i,k,1)
           th2o(i,2) = th2o(i,2)*h2oexp(i,k,2)
           th2o(i,3) = th2o(i,3)*h2oexp(i,k,3)
           th2o(i,4) = th2o(i,4)*h2oexp(i,k,4)
           th2o(i,5) = th2o(i,5)*h2oexp(i,k,5)
           th2o(i,6) = th2o(i,6)*h2oexp(i,k,6)
         enddo
      if (ne(ib).eq.0) then
!-----comoute trnh2o following eq. (8.22). fkw is given in table 4.
         do i=1,m
           trnth2o      =(fkw(1,ib)*th2o(i,1) &
                        + fkw(2,ib)*th2o(i,2) &
                        + fkw(3,ib)*th2o(i,3) &
                        + fkw(4,ib)*th2o(i,4) &
                        + fkw(5,ib)*th2o(i,5) &
                        + fkw(6,ib)*th2o(i,6))
          tran(i)=tran(i)*trnth2o
         enddo
      elseif (ne(ib).eq.1) then
!-----comoute trnh2o following eq. (8.22) and (4.27).
         do i=1,m
           tcon(i,1)= tcon(i,1)*conexp(i,k,1)
           trnth2o      =(fkw(1,ib)*th2o(i,1) &
                        + fkw(2,ib)*th2o(i,2) &
                        + fkw(3,ib)*th2o(i,3) &
                        + fkw(4,ib)*th2o(i,4) &
                        + fkw(5,ib)*th2o(i,5) &
                        + fkw(6,ib)*th2o(i,6))*tcon(i,1)
          tran(i)=tran(i)*trnth2o
         enddo
      else
!-----for band 3. this band is divided into 3 subbands.
         do i=1,m
           tcon(i,1)= tcon(i,1)*conexp(i,k,1)
           tcon(i,2)= tcon(i,2)*conexp(i,k,2)
           tcon(i,3)= tcon(i,3)*conexp(i,k,3)
!-----comoute trnh2o following eq. (4.29).
           trnth2o      = (  gkw(1,1)*th2o(i,1) &
                           + gkw(2,1)*th2o(i,2) &
                           + gkw(3,1)*th2o(i,3) &
                           + gkw(4,1)*th2o(i,4) &
                           + gkw(5,1)*th2o(i,5) &
                           + gkw(6,1)*th2o(i,6) ) * tcon(i,1) &
                        + (  gkw(1,2)*th2o(i,1) &
                           + gkw(2,2)*th2o(i,2) &
                           + gkw(3,2)*th2o(i,3) &
                           + gkw(4,2)*th2o(i,4) &
                           + gkw(5,2)*th2o(i,5) &
                           + gkw(6,2)*th2o(i,6) ) * tcon(i,2) &
                        + (  gkw(1,3)*th2o(i,1) &
                           + gkw(2,3)*th2o(i,2) &
                           + gkw(3,3)*th2o(i,3) &
                           + gkw(4,3)*th2o(i,4) &
                           + gkw(5,3)*th2o(i,5) &
                           + gkw(6,3)*th2o(i,6) ) * tcon(i,3)
          tran(i)=tran(i)*trnth2o
         enddo
      endif
      return
      end subroutine h2okdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine co2kdis(m,np,k,co2exp,tco2,tran)
!**********************************************************************
!   compute co2 transmittances between levels k1 and k2 for
!    m soundings, using the k-distribution method with linear
!    pressure scaling.
!
!---- input parameters
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for co2 absorption (co2exp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to co2 absorption
!     for the various values of the absorption coefficient (tco2)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) co2exp(m,np,6,2)
!---- updated parameters -----
      real(Kind=fp_kind) tco2(m,6,2),tran(m)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!-----tco2 is the 6 exp factors between levels k1 and k2.
!     xc is the total co2 transmittance given by eq. (4.30).
!     the k-distribution functions are given in table 10.
         do i=1,m
!-----band-wings
           tco2(i,1,1)=tco2(i,1,1)*co2exp(i,k,1,1)
           xc=   0.1395 *tco2(i,1,1)
           tco2(i,2,1)=tco2(i,2,1)*co2exp(i,k,2,1)
           xc=xc+0.1407 *tco2(i,2,1)
           tco2(i,3,1)=tco2(i,3,1)*co2exp(i,k,3,1)
           xc=xc+0.1549 *tco2(i,3,1)
           tco2(i,4,1)=tco2(i,4,1)*co2exp(i,k,4,1)
           xc=xc+0.1357 *tco2(i,4,1)
           tco2(i,5,1)=tco2(i,5,1)*co2exp(i,k,5,1)
           xc=xc+0.0182 *tco2(i,5,1)
           tco2(i,6,1)=tco2(i,6,1)*co2exp(i,k,6,1)
           xc=xc+0.0220 *tco2(i,6,1)
!-----band-center region
           tco2(i,1,2)=tco2(i,1,2)*co2exp(i,k,1,2)
           xc=xc+0.0766 *tco2(i,1,2)
           tco2(i,2,2)=tco2(i,2,2)*co2exp(i,k,2,2)
           xc=xc+0.1372 *tco2(i,2,2)
           tco2(i,3,2)=tco2(i,3,2)*co2exp(i,k,3,2)
           xc=xc+0.1189 *tco2(i,3,2)
           tco2(i,4,2)=tco2(i,4,2)*co2exp(i,k,4,2)
           xc=xc+0.0335 *tco2(i,4,2)
           tco2(i,5,2)=tco2(i,5,2)*co2exp(i,k,5,2)
           xc=xc+0.0169 *tco2(i,5,2)
           tco2(i,6,2)=tco2(i,6,2)*co2exp(i,k,6,2)
           xc=xc+0.0059 *tco2(i,6,2)
           tran(i)=tran(i)*xc
         enddo
      return
      end subroutine co2kdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine n2okdis(ib,m,np,k,n2oexp,tn2o,tran)
!**********************************************************************
!   compute n2o transmittances between levels k1 and k2 for
!    m soundings, using the k-distribution method with linear
!    pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for n2o absorption (n2oexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to n2o absorption
!     for the various values of the absorption coefficient (tn2o)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) n2oexp(m,np,4)
!---- updated parameters -----
      real(Kind=fp_kind) tn2o(m,4),tran(m)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!-----tn2o is computed from eq. (8.20).
!     xc is the total n2o transmittance computed from (8.22)
!     the k-distribution functions are given in table 5.
        do i=1,m
!-----band 6
          if (ib.eq.6) then
           tn2o(i,1)=tn2o(i,1)*n2oexp(i,k,1)
           xc=   0.940414*tn2o(i,1)
           tn2o(i,2)=tn2o(i,2)*n2oexp(i,k,2)
           xc=xc+0.059586*tn2o(i,2)
!-----band 7
          else
           tn2o(i,1)=tn2o(i,1)*n2oexp(i,k,1)
           xc=   0.561961*tn2o(i,1)
           tn2o(i,2)=tn2o(i,2)*n2oexp(i,k,2)
           xc=xc+0.138707*tn2o(i,2)
           tn2o(i,3)=tn2o(i,3)*n2oexp(i,k,3)
           xc=xc+0.240670*tn2o(i,3)
           tn2o(i,4)=tn2o(i,4)*n2oexp(i,k,4)
           xc=xc+0.058662*tn2o(i,4)
          endif
           tran(i)=tran(i)*xc
        enddo
      return
      end subroutine n2okdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine ch4kdis(ib,m,np,k,ch4exp,tch4,tran)
!**********************************************************************
!   compute ch4 transmittances between levels k1 and k2 for
!    m soundings, using the k-distribution method with
!    linear pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for ch4 absorption (ch4exp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to ch4 absorption
!     for the various values of the absorption coefficient (tch4)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) ch4exp(m,np,4)
!---- updated parameters -----
      real(Kind=fp_kind) tch4(m,4),tran(m)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!-----tch4 is computed from eq. (8.20).
!     xc is the total ch4 transmittance computed from (8.22)
!     the k-distribution functions are given in table 5.
        do i=1,m
!-----band 6
          if (ib.eq.6) then
           tch4(i,1)=tch4(i,1)*ch4exp(i,k,1)
           xc= tch4(i,1)
!-----band 7
          else
           tch4(i,1)=tch4(i,1)*ch4exp(i,k,1)
           xc=   0.610650*tch4(i,1)
           tch4(i,2)=tch4(i,2)*ch4exp(i,k,2)
           xc=xc+0.280212*tch4(i,2)
           tch4(i,3)=tch4(i,3)*ch4exp(i,k,3)
           xc=xc+0.107349*tch4(i,3)
           tch4(i,4)=tch4(i,4)*ch4exp(i,k,4)
           xc=xc+0.001789*tch4(i,4)
          endif
           tran(i)=tran(i)*xc
        enddo
      return
      end subroutine ch4kdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine comkdis(ib,m,np,k,comexp,tcom,tran)
!**********************************************************************
!  compute co2-minor transmittances between levels k1 and k2
!   for m soundings, using the k-distribution method
!   with linear pressure scaling.
!
!---- input parameters
!   spectral band (ib)
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for co2-minor absorption (comexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to co2-minor absorption
!     for the various values of the absorption coefficient (tcom)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer ib,m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) comexp(m,np,6)
!---- updated parameters -----
      real(Kind=fp_kind) tcom(m,6),tran(m)
!---- temporary arrays -----
      real(Kind=fp_kind) xc
!-----tcom is computed from eq. (8.20).
!     xc is the total co2 transmittance computed from (8.22)
!     the k-distribution functions are given in table 6.
         do i=1,m
!-----band 4
           if (ib.eq.4) then
            tcom(i,1)=tcom(i,1)*comexp(i,k,1)
            xc=   0.12159*tcom(i,1)
            tcom(i,2)=tcom(i,2)*comexp(i,k,2)
            xc=xc+0.24359*tcom(i,2)
            tcom(i,3)=tcom(i,3)*comexp(i,k,3)
            xc=xc+0.24981*tcom(i,3)
            tcom(i,4)=tcom(i,4)*comexp(i,k,4)
            xc=xc+0.26427*tcom(i,4)
            tcom(i,5)=tcom(i,5)*comexp(i,k,5)
            xc=xc+0.07807*tcom(i,5)
            tcom(i,6)=tcom(i,6)*comexp(i,k,6)
            xc=xc+0.04267*tcom(i,6)
!-----band 5
           else
            tcom(i,1)=tcom(i,1)*comexp(i,k,1)
            xc=   0.06869*tcom(i,1)
            tcom(i,2)=tcom(i,2)*comexp(i,k,2)
            xc=xc+0.14795*tcom(i,2)
            tcom(i,3)=tcom(i,3)*comexp(i,k,3)
            xc=xc+   0.19512*tcom(i,3)
            tcom(i,4)=tcom(i,4)*comexp(i,k,4)
            xc=xc+   0.33446*tcom(i,4)
            tcom(i,5)=tcom(i,5)*comexp(i,k,5)
            xc=xc+   0.17199*tcom(i,5)
            tcom(i,6)=tcom(i,6)*comexp(i,k,6)
            xc=xc+   0.08179*tcom(i,6)
           endif
            tran(i)=tran(i)*xc
         enddo
      return
      end subroutine comkdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cfckdis(m,np,k,cfcexp,tcfc,tran)
!**********************************************************************
!  compute cfc-(11,12,22) transmittances between levels k1 and k2
!   for m soundings, using the k-distribution method with
!   linear pressure scaling.
!
!---- input parameters
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for cfc absorption (cfcexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to cfc absorption
!     for the various values of the absorption coefficient (tcfc)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) cfcexp(m,np)
!---- updated parameters -----
      real(Kind=fp_kind) tcfc(m),tran(m)
!-----tcfc is the exp factors between levels k1 and k2.
         do i=1,m
            tcfc(i)=tcfc(i)*cfcexp(i,k)
            tran(i)=tran(i)*tcfc(i)
         enddo
      return
      end subroutine cfckdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine b10kdis(m,np,k,h2oexp,conexp,co2exp,n2oexp &
                ,th2o,tcon,tco2,tn2o,tran)
!**********************************************************************
!
!   compute h2o (line and continuum),co2,n2o transmittances between
!   levels k1 and k2 for m soundings, using the k-distribution
!   method with linear pressure scaling.
!
!---- input parameters
!   number of grid intervals (m)
!   number of levels (np)
!   current level (k)
!   exponentials for h2o line absorption (h2oexp)
!   exponentials for h2o continuum absorption (conexp)
!   exponentials for co2 absorption (co2exp)
!   exponentials for n2o absorption (n2oexp)
!
!---- updated parameters
!   transmittance between levels k1 and k2 due to h2o line absorption
!     for the various values of the absorption coefficient (th2o)
!   transmittance between levels k1 and k2 due to h2o continuum
!     absorption for the various values of the absorption
!     coefficient (tcon)
!   transmittance between levels k1 and k2 due to co2 absorption
!     for the various values of the absorption coefficient (tco2)
!   transmittance between levels k1 and k2 due to n2o absorption
!     for the various values of the absorption coefficient (tn2o)
!   total transmittance (tran)
!
!**********************************************************************
      implicit none
      integer m,np,i,k
!---- input parameters -----
      real(Kind=fp_kind) h2oexp(m,np,6),conexp(m,np,3),co2exp(m,np,6,2) &
          ,n2oexp(m,np,4)
!---- updated parameters -----
      real(Kind=fp_kind) th2o(m,6),tcon(m,3),tco2(m,6,2),tn2o(m,4) &
          ,tran(m)
!---- temporary arrays -----
      real(Kind=fp_kind) xx
!-----for h2o line. the k-distribution functions are given in table 4.
        do i=1,m
           th2o(i,1)=th2o(i,1)*h2oexp(i,k,1)
           xx=   0.3153*th2o(i,1)
           th2o(i,2)=th2o(i,2)*h2oexp(i,k,2)
           xx=xx+0.4604*th2o(i,2)
           th2o(i,3)=th2o(i,3)*h2oexp(i,k,3)
           xx=xx+0.1326*th2o(i,3)
           th2o(i,4)=th2o(i,4)*h2oexp(i,k,4)
           xx=xx+0.0798*th2o(i,4)
           th2o(i,5)=th2o(i,5)*h2oexp(i,k,5)
           xx=xx+0.0119*th2o(i,5)
           tran(i)=xx
        enddo
!-----for h2o continuum. note that conexp(i,k,3) is for subband 3a.
        do i=1,m
           tcon(i,1)=tcon(i,1)*conexp(i,k,1)
           tran(i)=tran(i)*tcon(i,1)
        enddo
!-----for co2 (table 6)
        do i=1,m
           tco2(i,1,1)=tco2(i,1,1)*co2exp(i,k,1,1)
           xx=    0.2673*tco2(i,1,1)
           tco2(i,2,1)=tco2(i,2,1)*co2exp(i,k,2,1)
           xx=xx+ 0.2201*tco2(i,2,1)
           tco2(i,3,1)=tco2(i,3,1)*co2exp(i,k,3,1)
           xx=xx+ 0.2106*tco2(i,3,1)
           tco2(i,4,1)=tco2(i,4,1)*co2exp(i,k,4,1)
           xx=xx+ 0.2409*tco2(i,4,1)
           tco2(i,5,1)=tco2(i,5,1)*co2exp(i,k,5,1)
           xx=xx+ 0.0196*tco2(i,5,1)
           tco2(i,6,1)=tco2(i,6,1)*co2exp(i,k,6,1)
           xx=xx+ 0.0415*tco2(i,6,1)
           tran(i)=tran(i)*xx
        enddo
!-----for n2o (table 5)
        do i=1,m
           tn2o(i,1)=tn2o(i,1)*n2oexp(i,k,1)
           xx=   0.970831*tn2o(i,1)
           tn2o(i,2)=tn2o(i,2)*n2oexp(i,k,2)
           xx=xx+0.029169*tn2o(i,2)
           tran(i)=tran(i)*(xx-1.0)
        enddo
      return
      end subroutine b10kdis

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

      subroutine cldovlp (m,np,k2,ict,icb,it,im,ib, &
                     cldhi,cldmd,cldlw,fcld,tcldlyr,fclr)
!***********************************************************************
!     compute the fractional clear line-of-sight between levels k1
!     and k2
!
! input parameters
!
!  m:       number of soundings
!  np:      number of layers
!  k2:      index for the level
!  ict:     the level separating high and middle clouds
!  icb:     the level separating middle and low clouds
!  it:      number of cloudy layers in the high-cloud group
!  im:      number of cloudy layers in the middle-cloud group
!  ib:      number of cloudy layers in the low-cloud group
!  fcld:    fractional cloud cover of a layer
!  tcldlyr: transmittance of a cloud layer
!
! output parameter
!
!  fclr:    clear line-of-sight between levels k1 and k2
!***********************************************************************
      implicit none
      integer m,np,k2,ict(m),icb(m)
      integer i,j,k,ii,it(m),im(m),ib(m),itx(m,np),imx(m,np),ibx(m,np)
      real(Kind=fp_kind) cldhi(m),cldmd(m),cldlw(m)
      real(Kind=fp_kind) fcld(m,np),tcldlyr(m,np),fclr(m)
!***********************************************************************
       do i=1,m
!-----for high clouds
!     "it" is the number of high-cloud layers
        if (k2.le.ict(i)) then
         if(fcld(i,k2-1).gt.0.001) then
          it(i)=it(i)+1
          ii=it(i)
          itx(i,ii)=k2-1
         if (ii .eq. 1) go to 11
!-----rearrange the order of cloud layers with increasing cloud amount
         do k=1,ii-1
           j=itx(i,k)
          if(fcld(i,j).gt.fcld(i,k2-1)) then
           do j=ii-1,k,-1
            itx(i,j+1)=itx(i,j)
           enddo
            itx(i,k)=k2-1
            go to 11
          endif
         enddo
   11   continue
!-----compute equivalent black-body high cloud amount
           cldhi(i)=0.0
          do k=1,ii
           j=itx(i,k)
           cldhi(i)=fcld(i,j)-tcldlyr(i,j)*(fcld(i,j)-cldhi(i))
          enddo
        endif
       endif
!-----for middle clouds
!     "im" is the number of middle-cloud layers
       if (k2.gt.ict(m) .and. k2.le.icb(m)) then
        if(fcld(i,k2-1).gt.0.001) then
         im(i)=im(i)+1
         ii=im(i)
         imx(i,ii)=k2-1
        if (ii .eq. 1) go to 21
!-----rearrange the order of cloud layers with increasing cloud amount
         do k=1,ii-1
            j=imx(i,k)
           if(fcld(i,j).gt.fcld(i,k2-1)) then
            do j=ii-1,k,-1
             imx(i,j+1)=imx(i,j)
            enddo
             imx(i,k)=k2-1
             go to 21
           endif
          enddo
   21   continue
!-----compute equivalent black-body middle cloud amount
           cldmd(i)=0.0
          do k=1,ii
           j=imx(i,k)
           cldmd(i)=fcld(i,j)-tcldlyr(i,j)*(fcld(i,j)-cldmd(i))
          enddo
        endif
       endif
!-----for low clouds
!     "ib" is the number of low-cloud layers
       if (k2.gt.icb(m)) then
        if(fcld(i,k2-1).gt.0.001) then
         ib(i)=ib(i)+1
         ii=ib(i)
         ibx(i,ii)=k2-1
        if (ii .eq. 1) go to 31
!-----rearrange the order of cloud layers with increasing cloud amount
         do k=1,ii-1
          j=ibx(i,k)
           if(fcld(i,j).gt.fcld(i,k2-1)) then
            do j=ii-1,k,-1
             ibx(i,j+1)=ibx(i,j)
            enddo
             ibx(i,k)=k2-1
             go to 31
           endif
          enddo
   31    continue
!-----compute equivalent black-body low cloud amount
           cldlw(i)=0.0
          do k=1,ii
           j=ibx(i,k)
           cldlw(i)=fcld(i,j)-tcldlyr(i,j)*(fcld(i,j)-cldlw(i))
          enddo
        endif
       endif
!-----fclr is the equivalent clear fraction between levels k1 and k2
!     assuming the three cloud groups are randomly overlapped.
!     it follows eqs. (10) and (12).
        fclr(i)=(1.0-cldhi(i))*(1.0-cldmd(i))*(1.0-cldlw(i))
      enddo !i loop
      return
      end subroutine cldovlp

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD

!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 
!GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD GODDARD 

end module module_ra_goddard


