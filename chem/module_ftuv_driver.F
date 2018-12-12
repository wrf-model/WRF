      module module_ftuv_driver

      use module_wave_data, only : nw

      implicit none

      save

      integer, parameter :: dp = selected_real_kind(14,300)
!----------------------------------------------------
!     private variables
!----------------------------------------------------
      integer, private, parameter  :: nref = 51
      real(dp), private, parameter :: m2s    = 60._dp
      real(dp), private, parameter :: Pa2hPa = .01_dp

      integer, private  :: nz, kcon
      integer, private  :: next, last
      integer, private  :: curjulday = 0

!++alma 2012-12-01 modis landuse from sw
      integer, private, allocatable :: luse2usgs(:)
!--

      real(dp), private :: esfact = 1.0_dp
      real(dp), private :: dobson

      real(dp), private :: dels

      real(dp), private :: zref(nref), tref(nref), airref(nref), o3ref(nref)
      real(dp), private :: albedo(nw-1,2)

      type column_density
        integer           :: ncoldens_levs
        integer           :: ndays_of_year
        real(dp), pointer :: col_levs(:)
        real(dp), pointer :: day_of_year(:)
        real(dp), pointer :: o3_col_dens(:,:,:,:)
        real(dp), pointer :: o2_col_dens(:,:,:,:)
        logical           :: is_allocated
      end type column_density

      type(column_density), private, allocatable :: col_dens(:)
      logical, private :: photo_inti_initialized = .false.


      data zref/ 00.0, 01.0, 02.0, 03.0, 04.0, 05.0, 06.0, 07.0, 08.0, 09.0,   &
                 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,   &
                 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0,   &
                 30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0,   &
                 40.0, 41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0, 49.0,   &
                 50.0 / 

     data tref/ 288.150, 281.651, 275.154, 268.659, 262.166, 255.676, 249.187, &
                242.700, 236.215, 229.733, 223.252, 216.774, 216.650, 216.650, &
                216.650, 216.650, 216.650, 216.650, 216.650, 216.650, 216.650, &
                217.581, 218.574, 219.567, 220.560, 221.552, 222.544, 223.536, &
                224.527, 225.518, 226.509, 227.500, 228.490, 230.973, 233.743, &
                236.513, 239.282, 242.050, 244.818, 247.584, 250.350, 253.114, &
                255.878, 258.641, 261.403, 264.164, 266.925, 269.684, 270.650, &
                270.650, 270.650 /

     data airref/ 2.55E+19, 2.31E+19, 2.09E+19, 1.89E+19, 1.70E+19, 1.53E+19,  &
                  1.37E+19, 1.23E+19, 1.09E+19, 9.71E+18, 8.60E+18, 7.59E+18,  &
                  6.49E+18, 5.54E+18, 4.74E+18, 4.05E+18, 3.46E+18, 2.96E+18,  &
                  2.53E+18, 2.16E+18, 1.85E+18, 1.57E+18, 1.34E+18, 1.14E+18,  &
                  9.76E+17, 8.33E+17, 7.12E+17, 6.09E+17, 5.21E+17, 4.47E+17,  &
                  3.83E+17, 3.28E+17, 2.82E+17, 2.41E+17, 2.06E+17, 1.76E+17,  &
                  1.51E+17, 1.30E+17, 1.12E+17, 9.62E+16, 8.31E+16, 7.19E+16,  &
                  6.23E+16, 5.40E+16, 4.70E+16, 4.09E+16, 3.56E+16, 3.11E+16,  &
                  2.74E+16, 2.42E+16, 2.14E+16 /

      data o3ref/ 8.00E+11, 7.39E+11, 6.90E+11, 6.22E+11, 5.80E+11, 5.67E+11,  &
                  5.70E+11, 5.86E+11, 6.50E+11, 8.23E+11, 1.13E+12, 1.57E+12,  &
                  2.02E+12, 2.24E+12, 2.35E+12, 2.57E+12, 2.95E+12, 3.47E+12,  &
                  4.04E+12, 4.49E+12, 4.77E+12, 4.88E+12, 4.86E+12, 4.73E+12,  &
                  4.54E+12, 4.32E+12, 4.03E+12, 3.65E+12, 3.24E+12, 2.85E+12,  &
                  2.52E+12, 2.26E+12, 2.03E+12, 1.80E+12, 1.58E+12, 1.40E+12,  &
                  1.22E+12, 1.04E+12, 8.73E+11, 7.31E+11, 6.07E+11, 4.95E+11,  &
                  3.98E+11, 3.18E+11, 2.54E+11, 2.03E+11, 1.62E+11, 1.30E+11,  &
                  1.04E+11, 8.27E+10, 6.61E+10/


!----------------------------------------------------
!     private parameters
!----------------------------------------------------
      integer, private, parameter :: pid_no2   = 4
      integer, private, parameter :: pid_n2o   = 9
      integer, private, parameter :: pid_n2o5  = 8
      integer, private, parameter :: pid_o31d  = 2
      integer, private, parameter :: pid_o33p  = 3
      integer, private, parameter :: pid_hono  = 12
      integer, private, parameter :: pid_hno3  = 13
      integer, private, parameter :: pid_hno4  = 14
      integer, private, parameter :: pid_no3o2 = 5
      integer, private, parameter :: pid_no3o  = 6
      integer, private, parameter :: pid_h2o2  = 11
      integer, private, parameter :: pid_ch2om = 16
      integer, private, parameter :: pid_ch2or = 15
      integer, private, parameter :: pid_ald   = 17
      integer, private, parameter :: pid_ch3choa = 17
      integer, private, parameter :: pid_ch3chob = 18
      integer, private, parameter :: pid_ch3choc = 19
      integer, private, parameter :: pid_op1   = 24
      integer, private, parameter :: pid_op2   = 1    ! unknown
      integer, private, parameter :: pid_paa   = 16
      integer, private, parameter :: pid_ket   = 23
      integer, private, parameter :: pid_glya  = 21
      integer, private, parameter :: pid_glyb  = 21
      integer, private, parameter :: pid_mgly  = 22
      integer, private, parameter :: pid_dcb   = 1     ! unknown
      integer, private, parameter :: pid_onit  = 25
      integer, private, parameter :: pid_pan   = 26
      integer, private, parameter :: pid_mvk   = 27
      integer, private, parameter :: pid_macr  = 28
      integer, private, parameter :: pid_hyac  = 29
      integer, private, parameter :: pid_glyald= 30

! added for CB05CL
! These numbers are referred to from module_wave_data.F (sjref(ii,XX))
      integer, private, parameter :: pid_cl2   = 56
      integer, private, parameter :: pid_hocl  = 57
      integer, private, parameter :: pid_fmcl  = 58

      contains

      subroutine ftuv_driver( id, curr_secs, dtstep, config_flags,     &
                              gmt, julday,                             &
                              p_phy, t_phy, rho_phy, p8w, t8w,         &
                              xlat, xlong,    z_at_w,                  &
                              moist, chem,gd_cloud,gd_cloud2,          &
                              ph_no2,ph_o31d,ph_o33p,ph_hono,          &
                              ph_hno3,ph_hno4,ph_no3o2,ph_no3o,        &
                              ph_h2o2,ph_ch2om,ph_ch2or,ph_ald,        &
                              ph_op1,ph_op2,ph_paa,ph_ket,ph_glya,     &
                              ph_glyb,ph_mgly,ph_dcb,ph_onit,          &
                              ph_macr,ph_ch3coc2h5,ph_n2o,             &
                              ph_pan,ph_mpan,ph_acetol,ph_gly,         &
                              ph_bigald,ph_mek,ph_c2h5ooh,ph_c3h7ooh,  &
                              ph_pooh,ph_rooh,ph_xooh,ph_isopooh,      &
                              ph_alkooh,ph_mekooh,ph_tolooh,           &
                              ph_terpooh,ph_n2o5,ph_mvk,ph_glyald,     &
                              ph_hyac,                                 &
                              ph_cl2,ph_hocl,ph_fmcl,                  &
                              ivgtyp,                                  &
                              ntuv_lev, ntuv_bin, ntuv_pht,            &
                              ph_radfld, ph_adjcoe, ph_prate,          &
                              wc_x, zref_x,                            &
                              tauaer1, tauaer2, tauaer3, tauaer4,      &    !rajesh
                              waer1, waer2, waer3, waer4,              &    !rajesh
                              gaer1, gaer2, gaer3, gaer4,              &    !rajesh
                              ids,ide, jds,jde, kds,kde,               &
                              ims,ime, jms,jme, kms,kme,               &
                              its,ite, jts,jte, kts,kte                )

      use module_configure
      use module_state_description
      use module_model_constants
      use module_data_sorgam, only : mw_so4_aer

      use module_wave_data, only : tuv_jmax, nw, wc
      use module_ftuv_subs, only : calc_zenith

      implicit none

!----------------------------------------------------
!     input arguments
!----------------------------------------------------
      integer, intent(in   ) :: id, julday,                      &
                                ids,ide, jds,jde, kds,kde,       &
                                ims,ime, jms,jme, kms,kme,       &
                                its,ite, jts,jte, kts,kte
      integer, intent(in   ) :: ntuv_lev, ntuv_bin, ntuv_pht
      integer, intent(in)    :: ivgtyp(ims:ime,jms:jme)                         

      real,     intent(in  ) :: dtstep, gmt
      real(dp), intent(in  ) :: curr_secs

      type(grid_config_rec_type),  intent(in   ) :: config_flags

      real, intent(in   ) :: p_phy(ims:ime,kms:kme,jms:jme),     &
                             t_phy(ims:ime,kms:kme,jms:jme),     &
                             rho_phy(ims:ime,kms:kme,jms:jme),   &
                             p8w(ims:ime,kms:kme,jms:jme),       &
                             t8w(ims:ime,kms:kme,jms:jme),       &
                             z_at_w(ims:ime,kms:kme,jms:jme)

!----------------------------------------------------
! these arrays are for cloudwater/ice coming from 
! grell convection, optional
!----------------------------------------------------
      real, optional,                                           &
            intent(in   ) :: gd_cloud(ims:ime,kms:kme,jms:jme), &
                             gd_cloud2(ims:ime,kms:kme,jms:jme)
      real, intent(in   ) :: xlat(ims:ime,jms:jme),             &
                             xlong(ims:ime,jms:jme)
      real, intent(in   ) :: moist(ims:ime,kms:kme,jms:jme,num_moist)
      real, intent(in   ) :: chem(ims:ime,kms:kme,jms:jme,num_chem)
!rajesh: add arrays for aerosol optical properties
      real, dimension( ims:ime, kms:kme, jms:jme ),              &
            intent(in   ) :: tauaer1, tauaer2, tauaer3, tauaer4, &
                             waer1, waer2, waer3, waer4,         &
                             gaer1, gaer2, gaer3, gaer4

!----------------------------------------------------
!     output arguments
!----------------------------------------------------
      real, dimension( ims:ime, kms:kme, jms:jme ),                    &
            intent(out  ) :: ph_no2, ph_o31d, ph_o33p, ph_hono,        &
                             ph_hno3, ph_hno4, ph_no3o2, ph_no3o,      &
                             ph_h2o2, ph_ch2om, ph_ch2or, ph_ald,      &
                             ph_op1, ph_op2, ph_paa, ph_ket, ph_glya,  &
                             ph_glyb, ph_mgly, ph_dcb,                 &
                             ph_onit, ph_macr, ph_ch3coc2h5, ph_n2o,   &
                             ph_pan, ph_mpan, ph_acetol, ph_gly,       &
                             ph_bigald, ph_mek, ph_c2h5ooh, ph_c3h7ooh,&
                             ph_pooh,ph_rooh,ph_xooh,ph_isopooh,       &
                             ph_alkooh,ph_mekooh,ph_tolooh,ph_terpooh, &
                             ph_n2o5,ph_mvk,ph_glyald,ph_hyac,         &
                             ph_cl2,ph_hocl,ph_fmcl

      real, dimension( ims:ime, ntuv_lev, jms:jme, ntuv_bin ),         &
            intent(out  ) :: ph_radfld
      real, dimension( ims:ime, ntuv_lev, jms:jme, ntuv_pht ),         &
            intent(out  ) :: ph_adjcoe
      real, dimension( ims:ime, ntuv_lev, jms:jme, ntuv_pht ),         &
            intent(out  ) :: ph_prate
      real, dimension(ntuv_bin),             &
            intent(out  ) :: wc_x
      real, dimension(ntuv_lev),             &
            intent(out  ) :: zref_x

!----------------------------------------------------
!     local parameters
!----------------------------------------------------
      real(dp), parameter :: xair_kg = 1.0e3_dp/28.97_dp*6.023e23_dp*1.0e-6_dp
      real(dp), parameter :: ph_no3o_factor = 1.1236_dp
      real(dp), parameter :: kg_per_amu     = 1.65979e-27_dp

!----------------------------------------------------
!     local scalars
!----------------------------------------------------
      integer     :: i, j, k, kp1, m
      integer     :: astat, istat
      integer     :: isorg
      integer     :: lu
      real(dp)    :: xtime, xhour, xmin, gmtp
      real(dp)    :: wrk, qs, es
      real(dp)    :: alat, along, azim, zen
      real(dp)    :: o3top
      real(dp)    :: o2top
      real(dp)    :: atm_mass_den
      logical     :: chm_is_mozart

!----------------------------------------------------
!     local arrays
!----------------------------------------------------
      real(dp) :: zen_angle(its:ite,jts:jte)
      real(dp) :: temp(kts:kte+1), o3(kts:kte+1), air(kts:kte+1)
      real(dp) :: zh(kts:kte+1), rh(kts:kte+1), xlwc(kts:kte+1)

      real(dp) :: acb1(kts:kte+1), acb2(kts:kte+1), aoc1(kts:kte+1), aoc2(kts:kte+1)
      real(dp) :: aant(kts:kte+1), aso4(kts:kte+1), asal(kts:kte+1)
! rajesh: add arrays
      real(dp) :: tauaer300(kts:kte), tauaer400(kts:kte+1),        &
                  tauaer600(kts:kte), tauaer999(kts:kte+1)
      real(dp) :: waer300(kts:kte), waer400(kts:kte),            &
                  waer600(kts:kte), waer999(kts:kte)
      real(dp) :: gaer300(kts:kte), gaer400(kts:kte),            &
                  gaer600(kts:kte), gaer999(kts:kte)

      real(dp) :: p_jtop(its:ite,jts:jte)
      real(dp) :: o2_exo_col(its:ite,jts:jte)
      real(dp) :: o3_exo_col(its:ite,jts:jte)
      real(dp) :: prate(kts:kte+1,tuv_jmax)
      real(dp) :: radfld(nz,nw-1)
      real(dp) :: adjcoe(nz,tuv_jmax)
      real(dp) :: prate0(nz,tuv_jmax)

      chm_is_mozart = config_flags%chem_opt == MOZART_KPP .or. &
                      config_flags%chem_opt == MOZCART_KPP .or. &
                      config_flags%chem_opt == T1_MOZCART_KPP .or. &
                      config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
                      config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP
!----------------------------------------------------
!     set o3,o2 column densities at model top
!----------------------------------------------------
      if( chm_is_mozart ) then
         p_jtop(its:ite,jts:jte) = Pa2hPa * p_phy(its:ite,kte,jts:jte)
         call p_interp( o2_exo_col, o3_exo_col, p_jtop, &
                        id, its, ite, jts, jte )
      else
         o2top  = 3.60906e+21_dp
         o3top  = 2.97450e+16_dp
      endif

      isorg=0
      aer_select: SELECT CASE(config_flags%chem_opt)
         CASE (RADM2SORG,RADM2SORG_KPP,RACMSORG_KPP,RADM2SORG_AQCHEM,RACM_ESRLSORG_KPP,RACMSORG_AQ,RACMSORG_AQCHEM_KPP, &
               RACM_ESRLSORG_AQCHEM_KPP,CBMZSORG,CBMZSORG_AQ, &
               CB05_SORG_AQ_KPP,CB05_SORG_VBS_AQ_KPP)
             isorg=1
             CALL wrf_debug(15,'SORGAM aerosols initialization ')
         CASE (MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP)
             CALL wrf_debug(15,'MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP aerosols initialization ')
         CASE DEFAULT
             CALL wrf_debug(15,'no aerosols initialization yet')
             CALL wrf_message('no aerosols initialization yet')
   END SELECT aer_select

!----------------------------------------------------
!     calculate the GMT
!----------------------------------------------------
      xtime = curr_secs/60._dp
      xhour = real( int(gmt + .01_dp) + int(xtime/m2s),kind=dp )
      xmin  = m2s*gmt + ( xtime - xhour*m2s )
      gmtp  = mod( xhour, 24.0_dp )
      gmtp  = gmtp + xmin/m2s

      xlwc(:) = 0._dp  !initialize in case no cloud water is present

!----------------------------------------------------
!     set photolysis rates
!----------------------------------------------------
      J_TILE_LOOP : do j = jts, jte
      I_TILE_LOOP : do i = its, ite

!----------------------------------------------------
!     set land use type
!----------------------------------------------------
        if( chm_is_mozart ) then
!++alma 2012-12-01 modis landuse from sw
!          if( ivgtyp(i,j) /= 16 ) then
           if( luse2usgs( ivgtyp(i,j) ) /= 16 ) then
!--

              lu = 1
           else
              lu = 2
           endif
        else
           lu = 1
        endif

        if( chm_is_mozart ) then
           o3top = o3_exo_col(i,j)
           o2top = o2_exo_col(i,j)
        endif

level_loop : &
        do k = kts, kte
          kp1       = k + 1
          temp(kp1) = t_phy(i,k,j)                            ! temperature(K)
          air(kp1)  = xair_kg*rho_phy(i,k,j)                  ! air num.(molecules/cm^3)
          o3(kp1)   = chem(i,k,j,p_o3)*1.0e-6_dp              ! ozone(VMR)
       
          wrk       = t_phy(i,k,j) - 273._dp
          es        = 6.11_dp*10.0_dp**(7.63_dp*wrk/(241.9_dp + wrk))     ! Magnus EQ
          qs        = 0.622_dp*es/(p_phy(i,k,j)*0.01_dp)            ! sat mass mix (H2O)
          wrk       = moist(i,k,j,p_qv)/qs
          rh(kp1)   = max( 0.0_dp, min( 1.0_dp, wrk) )              ! relative huminity
       
          if( p_qc > 1 ) then
             xlwc(kp1) = 1.0e3_dp*moist(i,k,j,p_qc)*rho_phy(i,k,j)  ! cloud water(g/m^3)
          end if
          if( xlwc(kp1) < 1.0e-6_dp ) then
             xlwc(kp1) = 0.0_dp
          end if
       
          zh(kp1) = .5_dp*(z_at_w(i,kp1,j)+z_at_w(i,k,j))*0.001_dp - z_at_w(i,kts,j)*0.001_dp      ! height(km)
!         zh(k+1) = z(i,k,j)*0.001 - z_at_w(i,1,j)*0.001      ! height(km)

!===============================================================================
! XUEXI 
!      ADD the aerosol effect on J
!      chem is in the unit of ug/m3
!      cb1 = p_ecj * 1.0
!      cb2 = p_ecj * 0.0
!      oc1 = p_orgpaj      ( Primary organic conc. Aitken mode)
!          + p_p25aj       ( Unknown PM25 conc. Acc. mode )
!      ant = p_no3aj       ( Nitrate conc. Acc. mode )
!          + p_nh4aj       ( Ammonium conc. Acc. mode )
!      so4 = p_so4aj       ( Sulfate conc. Acc. mode )
!      sa1 = p_seas        ( Coarse soil-derived aero sols )
!
!=======================================================
          
          acb1(kp1) = 0._dp
          acb2(kp1) = 0._dp
          aoc1(kp1) = 0._dp
          aoc2(kp1) = 0._dp
          aant(kp1) = 0._dp
          aso4(kp1) = 0._dp
          asal(kp1) = 0._dp
! rajesh: initialize aerosol optical properties to zero
          tauaer300(k) = 0._dp
          tauaer400(k) = 0._dp
          tauaer600(k) = 0._dp
          tauaer999(k) = 0._dp
          waer300(k) = 0._dp
          waer400(k) = 0._dp
          waer600(k) = 0._dp
          waer999(k) = 0._dp
          gaer300(k) = 0._dp
          gaer400(k) = 0._dp
          gaer600(k) = 0._dp
          gaer999(k) = 0._dp

          if( isorg == 1 ) then
             acb1(kp1) = chem(i,k,j,p_ecj)
!            acb2(kp1) = 0.0_dp
!            aoc1(kp1) = chem(i,k,j,p_orgpaj) + chem(i,k,j,p_orgbaj) + chem(i,k,j,p_orgaj)

             if(config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) then
             aoc1(kp1) = chem(i,k,j,p_orgpaj)+chem(i,k,j,p_asoa1j)+chem(i,k,j,p_asoa2j) &
                         + chem(i,k,j,p_asoa3j)+chem(i,k,j,p_asoa4j) &
                         + chem(i,k,j,p_bsoa1j)+chem(i,k,j,p_bsoa2j) &
                         + chem(i,k,j,p_bsoa3j)+chem(i,k,j,p_bsoa4j)
             else
             aoc1(kp1) = chem(i,k,j,p_orgpaj)
             endif
!             aoc1(kp1) = chem(i,k,j,p_orgpaj)
!             aoc2(kp1) = 0.0_dp
             aant(kp1) = chem(i,k,j,p_no3aj) + chem(i,k,j,p_nh4aj)
             aso4(kp1) = chem(i,k,j,p_so4aj)
!             asal(kp1) = chem(i,k,j,p_seas)
             asal(kp1) = chem(i,k,j,p_seas) + chem(i,k,j,p_naaj) + chem(i,k,j,p_claj)
          elseif( config_flags%chem_opt == MOZART_KPP .or. &
                  config_flags%chem_opt == MOZCART_KPP .or. &
                  config_flags%chem_opt == T1_MOZCART_KPP ) then
             aso4(kp1) = chem(i,k,j,p_sulf)*air(kp1)*real(mw_so4_aer,kind=dp)*kg_per_amu*1.e-9_dp
            if( config_flags%chem_opt == MOZCART_KPP .or. &
                config_flags%chem_opt == T1_MOZCART_KPP ) then
             atm_mass_den = rho_phy(i,k,j)
             acb1(kp1) = chem(i,k,j,p_bc1)*atm_mass_den
             acb2(kp1) = chem(i,k,j,p_bc2)*atm_mass_den
             aoc1(kp1) = chem(i,k,j,p_oc1)*atm_mass_den
             aoc2(kp1) = chem(i,k,j,p_oc2)*atm_mass_den
!            aant(kp1) = 0._dp
             asal(kp1) = (chem(i,k,j,p_seas_1) + chem(i,k,j,p_seas_2) &
                          + chem(i,k,j,p_seas_3) + chem(i,k,j,p_seas_4))*atm_mass_den
            endif
          endif
! rajesh: Extract column of aerosol optical properties
          if(config_flags%aer_ra_feedback == 1) then   
             tauaer300(k) = tauaer1(i,k,j)
             tauaer400(k) = tauaer2(i,k,j)
             tauaer600(k) = tauaer3(i,k,j)
             tauaer999(k) = tauaer4(i,k,j)
             waer300(k)   = waer1(i,k,j)
             waer400(k)   = waer2(i,k,j)
             waer600(k)   = waer3(i,k,j)
             waer999(k)   = waer4(i,k,j)
             gaer300(k)   = gaer1(i,k,j)
             gaer400(k)   = gaer2(i,k,j)
             gaer600(k)   = gaer3(i,k,j)
             gaer999(k)   = gaer4(i,k,j)
          endif
!         else
!            acb1(kp1) = 0._dp
!            acb2(kp1) = 0._dp
!            aoc1(kp1) = 0._dp
!            aoc2(kp1) = 0._dp
!            aant(kp1) = 0._dp
!            aso4(kp1) = 0._dp
!            asal(kp1) = 0._dp
        enddo level_loop

        temp(1) = t8w(i,kts,j)
        air(1)  = xair_kg*( p8w(i,kts,j)/t8w(i,kts,j)/r_d )
        o3(1)   = o3(2)
        rh(1)   = rh(2)
        xlwc(1) = 0._dp
        zh(1)   = 0._dp 
        acb1(1) = acb1(2)
        acb2(1) = acb2(2)
        aoc1(1) = aoc1(2)
        aoc2(1) = aoc2(2)
        aant(1) = aant(2)
        aso4(1) = aso4(2)
        asal(1) = asal(2)
! rajesh
!        tauaer300(1) = tauaer300(2)
!        tauaer400(1) = tauaer400(2)
!        tauaer600(1) = tauaer600(2)
!        tauaer999(1) = tauaer999(2)
!        waer300(1)   = waer300(2)
!        waer400(1)   = waer400(2)
!        waer600(1)   = waer600(2)
!        waer999(1)   = waer999(2)
!        gaer300(1)   = gaer300(2)
!        gaer400(1)   = gaer400(2)
!        gaer600(1)   = gaer600(2)
!        gaer999(1)   = gaer999(2)

!----------------------------------------------------
!     smooth air density ...
!----------------------------------------------------
        do k = kts+1, kte-1
          air(k) = .5_dp*air(k) + .25_dp*( air(k-1) + air(k+1) )
        enddo

       alat  = real(xlat(i,j),kind=dp)
       along = -real(xlong(i,j),kind=dp)
       call calc_zenith( alat, along, julday, gmtp, azim, zen )
       zen_angle(i,j) = zen

       call photo( config_flags%chem_opt,&
                   kte+1,                &
                   tuv_jmax,             &
                   julday,               &
                   gmtp,                 &
                   alat, along,          &
                   o3top, o2top, dobson, &
                   lu,                   &
                   zh,                   &
                   temp,                 &
                   air,                  &
                   rh,                   &
                   xlwc,                 &
                   o3,                   &
                   acb1,                 &
                   acb2,                 &
                   aoc1,                 &
                   aoc2,                 &
                   aant,                 &
                   aso4,                 &
                   asal,                 &
! rajesh: Add aerosol optical property columns as arguments
                   tauaer300, tauaer400, &
                   tauaer600, tauaer999, &
                   waer300, waer400,     &
                   waer600, waer999,     &
                   gaer300, gaer400,     &
                   gaer600, gaer999,     &
                   config_flags%aer_ra_feedback, &
                   prate,                &
                   radfld,               &
                   adjcoe,               &
                   prate0 )

        do k = kts, kte
          ph_no2(i,k,j)   = max( 0._dp,prate(k,pid_no2)*m2s )
          ph_o31d(i,k,j)  = max( 0._dp,prate(k,pid_o31d)*m2s )
          ph_o33p(i,k,j)  = max( 0._dp,prate(k,pid_o33p)*m2s )
          ph_hono(i,k,j)  = max( 0._dp,prate(k,pid_hono)*m2s )
          ph_hno3(i,k,j)  = max( 0._dp,prate(k,pid_hno3)*m2s )
          ph_hno4(i,k,j)  = max( 0._dp,prate(k,pid_hno4)*m2s )
          ph_no3o2(i,k,j) = max( 0._dp,prate(k,pid_no3o2)*m2s )
          ph_no3o(i,k,j)  = max( 0._dp,prate(k,pid_no3o)*m2s*ph_no3o_factor )
          ph_h2o2(i,k,j)  = max( 0._dp,prate(k,pid_h2o2)*m2s )
          ph_ch2om(i,k,j) = max( 0._dp,prate(k,pid_ch2om)*m2s )
          ph_ch2or(i,k,j) = max( 0._dp,prate(k,pid_ch2or)*m2s )
          ph_ald(i,k,j)   = max( 0._dp,prate(k,pid_ald)*m2s )
          ph_op1(i,k,j)   = max( 0._dp,prate(k,pid_op1)*m2s )
          ph_op2(i,k,j)   = max( 0._dp,prate(k,pid_op2)*m2s )
          ph_paa(i,k,j)   = max( 0._dp,prate(k,pid_paa)*m2s )
          ph_ket(i,k,j)   = max( 0._dp,prate(k,pid_ket)*m2s )
          ph_glya(i,k,j)  = max( 0._dp,prate(k,pid_glya)*m2s )
          ph_glyb(i,k,j)  = max( 0._dp,prate(k,pid_glyb)*m2s )
          ph_mgly(i,k,j)  = max( 0._dp,prate(k,pid_mgly)*m2s )
          ph_dcb(i,k,j)   = max( 0._dp,prate(k,pid_dcb)*m2s )
          ph_onit(i,k,j)  = max( 0._dp,prate(k,pid_onit)*m2s )
          ph_macr(i,k,j)  = max( 0._dp,prate(k,pid_macr) *m2s )
          ph_ch3coc2h5(i,k,j) = max( 0._dp,prate(k,23)*m2s )
          ph_cl2(i,k,j)   = max( 0._dp,prate(k,pid_cl2)*m2s )
          ph_hocl(i,k,j)  = max( 0._dp,prate(k,pid_hocl)*m2s )
          ph_fmcl(i,k,j)  = max( 0._dp,prate(k,pid_fmcl)*m2s )
          ph_pan(i,k,j)   = max( 0._dp,prate(k,pid_pan)*m2s )
          ph_n2o5(i,k,j)  = max( 0._dp,prate(k,pid_n2o5)*m2s )
        enddo

        do m = 1,min(nw-1,ntuv_bin)
          do k = 1,min(nz,ntuv_lev)
            ph_radfld(i,k,j,m) = radfld(k,m)
          end do
        end do

        do m = 1,min(ntuv_pht,tuv_jmax)
          do k = 1,min(nz,ntuv_lev)
            ph_adjcoe(i,k,j,m) = adjcoe(k,m)
            ph_prate (i,k,j,m) = prate0(k,m)
          end do
        end do

        wc_x(1:min(ntuv_bin,nw-1)) = wc(1:min(ntuv_bin,nw-1))
        zref_x(1:min(nz,ntuv_lev)) = zref(1:min(nz,ntuv_lev))

        if( chm_is_mozart ) then
          do k = kts, kte
            ph_n2o(i,k,j)  = max( 0._dp,prate(k,pid_n2o)*m2s )
            ph_n2o5(i,k,j) = max( 0._dp,prate(k,pid_n2o5)*m2s )
            ph_mvk(i,k,j)  = max( 0._dp,prate(k,pid_mvk)*m2s )
            ph_pan(i,k,j)  = max( 0._dp,prate(k,pid_pan)*m2s )
            ph_ald(i,k,j)  = max( 0._dp,(prate(k,pid_ch3choa)+prate(k,pid_ch3chob)+prate(k,pid_ch3choc))*m2s )
            ph_gly(i,k,j)  = ph_mgly(i,k,j)
            ph_mek(i,k,j)  = ph_ket(i,k,j)
            ph_acetol(i,k,j) = ph_op1(i,k,j)
            ph_pooh(i,k,j)   = ph_op1(i,k,j)
            ph_glyald(i,k,j) = max( 0._dp,prate(k,pid_glyald) *m2s )
            ph_hyac(i,k,j)   = max( 0._dp,2._dp*prate(k,pid_hyac) *m2s )
            ph_hno4(i,k,j)   = ph_hno4(i,k,j) + 1.e-5_dp*m2s
          enddo
        end if

      enddo I_TILE_LOOP
      enddo J_TILE_LOOP

      end subroutine ftuv_driver

!++alma 2012-12-01 modis landse from
!     subroutine ftuv_init( id,ips, ipe, jps, jpe, kte, &
!                            ide, jde, config_flags )
      subroutine ftuv_init( id,ips, ipe, jps, jpe, kte, &
                            ide, jde, config_flags, num_land_cat, mminlu_loc)
!--

!---------------------------------------------------------------------
!	... new initialization routine for ftuv
!---------------------------------------------------------------------

      use module_state_description, only : mozart_kpp, mozcart_kpp, &
                                           t1_mozcart_kpp,&
                                           mozart_mosaic_4bin_kpp,&
                                           mozart_mosaic_4bin_aq_kpp
      use module_ftuv_subs, only         : aer_init
      use module_configure, only         : grid_config_rec_type

      implicit none

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: id
      integer, intent(in) :: ips, ipe, jps, jpe, kte
      integer, intent(in) :: ide, jde

!++alma 2012-12-01 modis landuse from sw
      integer, intent(in) :: num_land_cat
      character(len=*), intent(in) :: mminlu_loc
      type(grid_config_rec_type),  intent(in   ) :: config_flags
!--


!---------------------------------------------------------------------
!	... local variables
!---------------------------------------------------------------------
      integer :: astat
      integer :: ncid
      integer :: dimid
      integer :: varid
      integer :: max_dom
      integer :: cpos
      integer :: iend, jend
      integer :: lon_e, lat_e
      integer :: ncoldens_levs
      integer :: ndays_of_year
      integer :: i
      real, allocatable :: coldens(:,:,:,:)
      character(len=128) :: err_msg
      character(len=64)  :: filename
      character(len=3)   :: id_num

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

#ifdef NETCDF
include 'netcdf.inc'
#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * DWORDSIZE )
#else
      if( config_flags%chem_opt == MOZART_KPP .or. &
          config_flags%chem_opt == MOZCART_KPP .or. &
          config_flags%chem_opt == T1_MOZCART_KPP .or. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
         call wrf_message( 'ftuv_init: mozart,mozcart,mozart_mosaic_* chem option requires netcdf' )
         call wrf_abort
      end if
#endif

!---------------------------------------------------------------------
!	... general initializer
!---------------------------------------------------------------------
      if( id == 1 .and. .not. photo_inti_initialized ) then
        write(err_msg,*) 'ftuv_init: calling photo_inti for id = ',id
        call wrf_message( trim(err_msg) )
        call photo_inti( config_flags%chem_opt, kte, 20._dp )
      endif
#ifdef NETCDF
is_mozart : &
      if( config_flags%chem_opt == MOZART_KPP .or. &
          config_flags%chem_opt == MOZCART_KPP .or. &
          config_flags%chem_opt == T1_MOZCART_KPP .or. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
!---------------------------------------------------------------------
!	... allocate column_density type
!---------------------------------------------------------------------
        if( id == 1 .and. .not. allocated(col_dens) ) then
          CALL nl_get_max_dom( 1,max_dom )
          allocate( col_dens(max_dom),stat=astat )
          if( astat /= 0 ) then
            CALL wrf_message( 'ftuv_init: failed to allocate column_density type col_dens' )
            CALL wrf_abort
          end if
          write(err_msg,*) 'ftuv_init: intializing ',max_dom,' domains'
          call wrf_message( trim(err_msg) )
          col_dens(:)%is_allocated = .false.
        endif
!---------------------------------------------------------------------
!	... open column density netcdf file
!---------------------------------------------------------------------
col_dens_allocated : &
        if( .not. col_dens(id)%is_allocated ) then
          if( wrf_dm_on_monitor() ) then
            cpos = index( config_flags%exo_coldens_inname, '<domain>' )
            if( cpos > 0 ) then
              write(id_num,'(i3)') 100+id
              filename = config_flags%exo_coldens_inname(:cpos-1) // 'd' // id_num(2:3)
            else
              filename = trim( config_flags%exo_coldens_inname )
            endif
            err_msg = 'ftuv_init: intializing domain ' // id_num(2:3)
            call wrf_message( trim(err_msg) )
            err_msg = 'ftuv_init: failed to open file ' // trim(filename)
            call handle_ncerr( nf_open( trim(filename), nf_noclobber, ncid ), trim(err_msg) )
!---------------------------------------------------------------------
!	... get dimensions
!---------------------------------------------------------------------
             err_msg = 'ftuv_init: failed to get col_dens levels id'
             call handle_ncerr( nf_inq_dimid( ncid, 'coldens_levs', dimid ), trim(err_msg) ) 
             err_msg = 'ftuv_init: failed to get col_dens levels'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, ncoldens_levs ), trim(err_msg) )
             err_msg = 'ftuv_init: failed to get number of days in year id'
             call handle_ncerr( nf_inq_dimid( ncid, 'ndays_of_year', dimid ), trim(err_msg) )
             err_msg = 'ftuv_init: failed to get number of days in year'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, ndays_of_year ), trim(err_msg) )
             err_msg = 'ftuv_init: failed to get west_east id'
             call handle_ncerr( nf_inq_dimid( ncid, 'west_east', dimid ), trim(err_msg) ) 
             err_msg = 'ftuv_init: failed to get west_east'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, lon_e ), trim(err_msg) )
             err_msg = 'ftuv_init: failed to get south_north id'
             call handle_ncerr( nf_inq_dimid( ncid, 'south_north', dimid ), trim(err_msg) ) 
             err_msg = 'ftuv_init: failed to get south_north'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, lat_e ), trim(err_msg) )
          end IF
!---------------------------------------------------------------------
!	... bcast the dimensions
!---------------------------------------------------------------------
          CALL wrf_dm_bcast_bytes ( ncoldens_levs , IWORDSIZE )
          CALL wrf_dm_bcast_bytes ( ndays_of_year , IWORDSIZE )
          CALL wrf_dm_bcast_bytes ( lon_e , IWORDSIZE )
          CALL wrf_dm_bcast_bytes ( lat_e , IWORDSIZE )
!---------------------------------------------------------------------
!	... allocate local arrays
!---------------------------------------------------------------------
          iend = min( ipe,ide-1 )
          jend = min( jpe,jde-1 )
          allocate( coldens(lon_e,lat_e,ncoldens_levs,ndays_of_year), stat=astat )
          if( astat /= 0 ) then
            call wrf_message( 'ftuv_init: failed to allocate coldens' )
            call wrf_abort
          end if
!---------------------------------------------------------------------
!	... allocate column_density type component arrays
!---------------------------------------------------------------------
          col_dens(id)%ncoldens_levs = ncoldens_levs
          col_dens(id)%ndays_of_year = ndays_of_year
          allocate( col_dens(id)%col_levs(ncoldens_levs), &
                    col_dens(id)%day_of_year(ndays_of_year), stat=astat )
          if( astat /= 0 ) then
            call wrf_message( 'ftuv_init: failed to allocate col_levs,day_of_year' )
            call wrf_abort
          end if
          allocate( col_dens(id)%o3_col_dens(ips:iend,jps:jend,ncoldens_levs,ndays_of_year), &
                    col_dens(id)%o2_col_dens(ips:iend,jps:jend,ncoldens_levs,ndays_of_year), stat=astat )
          if( astat /= 0 ) then
            call wrf_message( 'ftuv_init: failed to allocate o3_col_dens,o2_col_dens' )
            call wrf_abort
          end if
          col_dens(id)%is_allocated = .true.
!---------------------------------------------------------------------
!	... read arrays
!---------------------------------------------------------------------
            IF ( wrf_dm_on_monitor() ) THEN
            err_msg = 'ftuv_init: failed to get col_levs variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'coldens_levs', varid ), trim(err_msg) )
            err_msg = 'ftuv_init: failed to read col_levs variable'
            call handle_ncerr( nf_get_var_double( ncid, varid, col_dens(id)%col_levs ), trim(err_msg) )
            err_msg = 'ftuv_init: failed to get days_of_year variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'days_of_year', varid ), trim(err_msg) )
            err_msg = 'ftuv_init: failed to read days_of_year variable'
            call handle_ncerr( nf_get_var_double( ncid, varid, col_dens(id)%day_of_year ), trim(err_msg) )
            err_msg = 'ftuv_init: failed to col_dens variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'o3_column_density', varid ), trim(err_msg) )
            err_msg = 'ftuv_init: failed to read col_dens variable'
            call handle_ncerr( nf_get_var_real( ncid, varid, coldens ), trim(err_msg) )
          end if

          DM_BCAST_MACRO(col_dens(id)%col_levs)
          write(*,*) 'ftuv_init: bcast col_levs'
          DM_BCAST_MACRO(col_dens(id)%day_of_year)
          write(*,*) 'ftuv_init: bcast day_of_year'
          CALL wrf_dm_bcast_bytes ( coldens, size(coldens)*RWORDSIZE )
          write(*,*) 'ftuv_init: bcast o3_col_dens'

          col_dens(id)%o3_col_dens(ips:iend,jps:jend,:ncoldens_levs,:ndays_of_year) = &
                        coldens(ips:iend,jps:jend,:ncoldens_levs,:ndays_of_year)

          IF ( wrf_dm_on_monitor() ) THEN
            err_msg = 'ftuv_init: failed to col_dens variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'o2_column_density', varid ), trim(err_msg) )
            err_msg = 'ftuv_init: failed to read col_dens variable'
            call handle_ncerr( nf_get_var_real( ncid, varid, coldens ), trim(err_msg) )
!---------------------------------------------------------------------
!	... close column density netcdf file
!---------------------------------------------------------------------
            err_msg = 'ftuv_init: failed to close file ' // trim(filename)
            call handle_ncerr( nf_close( ncid ), trim(err_msg) )
          end if

          CALL wrf_dm_bcast_bytes ( coldens, size(coldens)*RWORDSIZE )
          write(*,*) 'ftuv_init: bcast o2_col_dens'

          col_dens(id)%o2_col_dens(ips:iend,jps:jend,:ncoldens_levs,:ndays_of_year) = &
                        coldens(ips:iend,jps:jend,:ncoldens_levs,:ndays_of_year)

          deallocate( coldens )

          write(*,*) ' '
          write(*,*) 'ftuv_init: coldens variables for domain ',id
          write(*,*) 'ftuv_init: days_of_year'
          write(*,'(1p,5g15.7)') col_dens(id)%day_of_year(:)
          write(*,*) 'ftuv_init: coldens levels'
          write(*,'(1p,5g15.7)') col_dens(id)%col_levs(:)
          write(*,*) ' '
        endif col_dens_allocated

!++alma 2012-12-01 modis landuse from sw
!       if( id == 1 ) then
        if( id == 1 .and. .not. allocated(luse2usgs) ) then
          !allocate( luse2usgs(config_flags%num_land_cat),stat=astat )
          print*,"num_land_cat: ", num_land_cat
          allocate( luse2usgs(num_land_cat),stat=astat )
          if( astat /= 0 ) then
            CALL wrf_message( 'ftuv_init: failed to allocate luse2usgs')
            CALL wrf_abort
          end if
          if( trim(mminlu_loc) == 'USGS' ) then
            !luse2usgs(:) = (/ (i,i=1,config_flags%num_land_cat) /)
            luse2usgs(:) = (/ (i,i=1,num_land_cat) /)
          elseif( trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' ) then
            luse2usgs(:) = (/ 14,13,12,11,15,8,9,10,10,7, &
                              17,4,1,5,24,19,16,21,22,23 /)
          endif
        endif
!--


      endif is_mozart
#endif
!---------------------------------------------------------------------
!	... initialize aerosol routine
!---------------------------------------------------------------------
      if( id == 1 .and. .not. photo_inti_initialized ) then
        write(*,*) 'ftuv_init: initializing aerosol routine'
        call aer_init
        write(*,*) 'ftuv_init: finished'
        photo_inti_initialized = .true.
      endif

      end subroutine ftuv_init

#ifdef NETCDF
      subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: ret
      character(len=*), intent(in) :: mes

include 'netcdf.inc'

      if( ret /= nf_noerr ) then
         call wrf_message( trim(mes) )
         call wrf_message( trim(nf_strerror(ret)) )
         call wrf_abort
      end if

      end subroutine handle_ncerr
#endif

      subroutine photo_inti( chem_opt, nlev, ztopin ) 

      use module_wave_data, only : nw, wl, wave_data_inti
      use module_ftuv_subs, only : nwint, wlint, schu_inti, inter_inti
      use module_state_description, only : mozart_kpp, mozcart_kpp, &
                                           t1_mozcart_kpp,&
                                           mozart_mosaic_4bin_kpp,&
                                           mozart_mosaic_4bin_aq_kpp

      implicit none

!---------------------------------------------------------------------
!     input arguments
!---------------------------------------------------------------------
      integer, intent(in)  :: chem_opt
      integer, intent(in)  :: nlev
      real(dp), intent(in) :: ztopin

!---------------------------------------------------------------------
!     local arguments
!---------------------------------------------------------------------
      integer  :: k, kdis, nabv, iw
      real(dp) :: ztop

      if( chem_opt /= MOZART_KPP .and. &
          chem_opt /= MOZCART_KPP .and. &
          chem_opt /= T1_MOZCART_KPP .and. &
          chem_opt /= MOZART_MOSAIC_4BIN_KPP .and. &
          chem_opt /= MOZART_MOSAIC_4BIN_AQ_KPP ) then
!---------------------------------------------------------------------
!     change unit from m to km
!---------------------------------------------------------------------
         ztop = ztopin
!---------------------------------------------------------------------
!     set the top exo model level
!---------------------------------------------------------------------
         do k = 1,nref
           if( ztop < zref(k) ) then
              exit
           end if
         enddo
         if( k == 1 .or. k > nref ) then
           call wrf_message( 'photo_inti: ztop is not in zref range' )
           call wrf_abort
         endif

         kcon = k + 1
         kdis = nref - kcon
         nabv = kdis/2 + 1
         if( mod(kdis,2) /= 0 ) then
           nabv = nabv + 1
         endif
         nz = nlev + nabv
         write(*,*) '******************************************'
         write(*,*) 'photo_inti: kcon,kdis,nabv,nlev,nz = ',kcon,kdis,nabv,nlev,nz
         write(*,*) '******************************************'
         dobson = 265.0_dp
      else
         nz = nlev
         dobson = 0._dp
      endif

!---------------------------------------------------------------------
!     set albedo for land
!---------------------------------------------------------------------
      do iw = 1, nw-1
        if( wl(iw)<400.0_dp ) then
           albedo(iw,1) = 0.05_dp
        else if( (wl(iw)>=400.0_dp ) .and. (wl(iw)<450.0_dp) ) then
           albedo(iw,1) = 0.06_dp
        else if( (wl(iw)>=450.0_dp ) .and. (wl(iw)<500.0_dp) ) then
           albedo(iw,1) = 0.08_dp
        else if( (wl(iw)>=500.0_dp ) .and. (wl(iw)<550.0_dp) ) then
           albedo(iw,1) = 0.10_dp
        else if( (wl(iw)>=550.0_dp ) .and. (wl(iw)<600.0_dp) ) then
           albedo(iw,1) = 0.11_dp
        else if( (wl(iw)>=600.0_dp ) .and. (wl(iw)<640.0_dp) ) then
           albedo(iw,1) = 0.12_dp
        else if( (wl(iw)>=640.0_dp ) .and. (wl(iw)<660.0_dp) ) then
           albedo(iw,1) = 0.135_dp
        else if( wl(iw)>=660.0_dp ) then
           albedo(iw,1) = 0.15_dp
        end if
      end do

!---------------------------------------------------------------------
!     set albedo for sea water
!
!     For mozart_kpp these are interpolated from reflectance values
!     for IGBP type 17 (ocean water). Source: NASA LARC
!     (http://www-surf.larc.nasa.gov/surf/pages/explan.html)
!---------------------------------------------------------------------
      if( chem_opt == MOZART_KPP .or. &
          chem_opt == MOZCART_KPP .or. &
          chem_opt == T1_MOZCART_KPP .or. &
          chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
          chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) then
         albedo(1:17,2) = (/ 0.0747_dp, 0.0755_dp, 0.0767_dp, 0.0783_dp, 0.0802_dp,   &
                             0.0825_dp, 0.0852_dp, 0.0882_dp, 0.0914_dp, 0.0908_dp,   &
                             0.0763_dp, 0.0725_dp, 0.0689_dp, 0.0632_dp, 0.0570_dp,   &
                             0.0585_dp, 0.0535_dp /)
      else
         albedo(1:17,2) = (/ 0.8228_dp, 0.8140_dp, 0.8000_dp, 0.7820_dp, 0.7600_dp,   &
                             0.7340_dp, 0.7040_dp, 0.6700_dp, 0.6340_dp, 0.4782_dp,   &
                             0.3492_dp, 0.3000_dp, 0.2700_dp, 0.2400_dp, 0.1700_dp,   &
                             0.0800_dp, 0.0600_dp /)
      endif

!---------------------------------------------------------------------
!     intialize other modules
!---------------------------------------------------------------------
      call schu_inti
      call wave_data_inti( nz )
      call inter_inti( nw, wl, nwint, wlint )

      end subroutine photo_inti

      subroutine photo( chem_opt, nlev, njout, julday, gmtp, alat, &
                        along, o3top, o2top, o3toms, lu, zin, tlevin, &
                        airlevin, rhin, xlwcin, o3in, acb1in, &
                        acb2in, aoc1in, aoc2in, aantin, aso4in, &
                        asalin, tauaer300in,tauaer400in,tauaer600in,tauaer999in, &
                        waer300in, waer400in, waer600in, waer999in,    &
                        gaer300in, gaer400in, gaer600in, gaer999in,    &
                        aer_ra_feedback, prate, radfld, adjcoe, prate0 )

      use module_ftuv_subs, only : calc_zenith, photoin

      implicit none

!----------------------------------------------------
!     input arguments
!----------------------------------------------------
      integer, intent(in)  :: chem_opt 
      integer, intent(in)  :: lu 
      integer, intent(in)  :: nlev, njout
      integer, intent(in)  :: julday
      real(dp), intent(in) :: gmtp
      real(dp), intent(in) :: alat, along
      real(dp), intent(in) :: o3toms
      real(dp), intent(in) :: o3top
      real(dp), intent(in) :: o2top

      real(dp), intent(in) :: zin(nlev)
      real(dp), intent(in) :: tlevin(nlev)
      real(dp), intent(in) :: airlevin(nlev)
      real(dp), intent(in) :: rhin(nlev)
      real(dp), intent(in) :: xlwcin(nlev)
      real(dp), intent(in) :: o3in(nlev)
      real(dp), intent(in) :: acb1in(nlev)
      real(dp), intent(in) :: acb2in(nlev)
      real(dp), intent(in) :: aoc1in(nlev)
      real(dp), intent(in) :: aoc2in(nlev)
      real(dp), intent(in) :: aantin(nlev)
      real(dp), intent(in) :: aso4in(nlev)
      real(dp), intent(in) :: asalin(nlev)

! rajesh: declare dust variables
      real(dp), intent(in) :: tauaer300in(nlev-1)
      real(dp), intent(in) :: tauaer400in(nlev-1)
      real(dp), intent(in) :: tauaer600in(nlev-1)
      real(dp), intent(in) :: tauaer999in(nlev-1)
      real(dp), intent(in) :: waer300in(nlev-1)
      real(dp), intent(in) :: waer400in(nlev-1)
      real(dp), intent(in) :: waer600in(nlev-1)
      real(dp), intent(in) :: waer999in(nlev-1)
      real(dp), intent(in) :: gaer300in(nlev-1)
      real(dp), intent(in) :: gaer400in(nlev-1)
      real(dp), intent(in) :: gaer600in(nlev-1)
      real(dp), intent(in) :: gaer999in(nlev-1)
      INTEGER, INTENT(IN)  :: aer_ra_feedback

!----------------------------------------------------
!     output arguments
!----------------------------------------------------
      real(dp), intent(out) :: prate(nlev,njout)
      real(dp), intent(out) :: radfld(nz,nw-1)
      real(dp), intent(out) :: adjcoe(nz,njout)
      real(dp), intent(out) :: prate0(nz,njout)

!----------------------------------------------------
!     local arugments
!----------------------------------------------------
      integer  :: astat, istat
      integer  :: k, n
      real(dp) :: azim, zen
      real(dp), allocatable :: z_ph(:), tlev_ph(:), tlay_ph(:), airlev_ph(:)
      real(dp), allocatable :: rh_ph(:), xlwc_ph(:)
      real(dp), allocatable :: o3_ph(:)
      real(dp), allocatable :: acb1_ph(:), acb2_ph(:)
      real(dp), allocatable :: aoc1_ph(:), aoc2_ph(:)
      real(dp), allocatable :: aant_ph(:), aso4_ph(:), asal_ph(:)

! rajesh: declare allocatable variables for aerosol optical properties
      real(dp), allocatable :: tauaer300_ph(:), tauaer400_ph(:),   &
                               tauaer600_ph(:), tauaer999_ph(:),   &
                               waer300_ph(:), waer400_ph(:),       &
                               waer600_ph(:), waer999_ph(:),       &
                               gaer300_ph(:), gaer400_ph(:),       &
                               gaer600_ph(:), gaer999_ph(:)
      real(dp), allocatable :: ftuv(:,:)

!-----------------------------------------------------------------------------
! Now xx-ftuv only considers the following 28 photolysis reactions ...
!-----------------------------------------------------------------------------
! 1 o2 + hv -> o + o
! 2 o3 -> o2 + o(1d)
! 3 o3 -> o2 + o(3p)
! 4 no2 -> no + o(3p)
! 5 no3 -> no + o2
! 6 no3 -> no2 + o(3p)
! 7 n2o5 -> no3 + no + o(3p)
! 8 n2o5 -> no3 + no2
! 9 n2o + hv -> n2 + o(1d)
! 10 ho2 + hv -> oh + o
! 11 h2o2 -> 2 oh
! 12 hno2 -> oh + no
! 13 hno3 -> oh + no2
! 14 hno4 -> ho2 + no2
! 15 ch2o -> h + hco
! 16 ch2o -> h2 + co
! 17 ch3cho -> ch3 + hco
! 18 ch3cho -> ch4 + co
! 19 ch3cho -> ch3co + h
! 20 c2h5cho -> c2h5 + hco
! 21 chocho -> products
! 22 ch3cocho -> products
! 23 ch3coch3
! 24 ch3ooh -> ch3o + oh
! 25 ch3ono2 -> ch3o+no2
! 26 pan + hv -> products
! 27 mvk + hv -> products
! 28 macr + hv -> products
! 29 hyac + hv -> products
! 30 glyald + hv -> products
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     solar zenith angle calculation
!-----------------------------------------------------------------------------
      call calc_zenith( alat, along, julday, gmtp, azim, zen )
      if( zen == 90.0_dp ) then
        zen = 89.0_dp
      elseif( zen >= 95.0_dp ) then
        prate(:,:) = 0.0_dp
        return
      endif

!---------------------------------------------------------------------
!     allocate memory space
!---------------------------------------------------------------------
      astat = 0
      allocate( ftuv(nz,njout), stat=istat )
      astat = astat + istat
      allocate( z_ph(nz), tlev_ph(nz), tlay_ph(nz-1), airlev_ph(nz), stat=istat )
      astat = astat + istat
      allocate( rh_ph(nz), xlwc_ph(nz), stat=istat )
      astat = astat + istat
      allocate( o3_ph(nz), stat=istat )
      astat = astat + istat
      allocate( acb1_ph(nz), acb2_ph(nz), aoc1_ph(nz), aoc2_ph(nz), stat=istat )
      astat = astat + istat
      allocate( aant_ph(nz), aso4_ph(nz), asal_ph(nz), stat=istat )
      astat = astat + istat

! rajesh: allocate memory space to aerosol optical property variables
      allocate( tauaer300_ph(nz-1), tauaer400_ph(nz-1), tauaer600_ph(nz-1),  &
                tauaer999_ph(nz-1), stat=istat)
      astat = astat + istat
      allocate( waer300_ph(nz-1), waer400_ph(nz-1), waer600_ph(nz-1),  &
                waer999_ph(nz-1), stat=istat)
      astat = astat + istat
      allocate( gaer300_ph(nz-1), gaer400_ph(nz-1), gaer600_ph(nz-1),  &
                gaer999_ph(nz-1), stat=istat)
      astat = astat + istat
   
      if( astat /= 0 ) then
         call wrf_message( 'ftuv_driver: failed to allocate _ph arrays' )
         call wrf_abort
      endif

      rh_ph(:)   = 0.0_dp
      xlwc_ph(:) = 0.0_dp
      acb1_ph(:) = 0.0_dp
      acb2_ph(:) = 0.0_dp
      aoc1_ph(:) = 0.0_dp
      aoc2_ph(:) = 0.0_dp
      aant_ph(:) = 0.0_dp
      aso4_ph(:) = 0.0_dp
      asal_ph(:) = 0.0_dp

! rajesh: Initialize variables to zero 
      tauaer300_ph(:) = 0.0_dp
      tauaer400_ph(:) = 0.0_dp
      tauaer600_ph(:) = 0.0_dp
      tauaer999_ph(:) = 0.0_dp
      waer300_ph(:) = 0.0_dp
      waer400_ph(:) = 0.0_dp
      waer600_ph(:) = 0.0_dp
      waer999_ph(:) = 0.0_dp
      gaer300_ph(:) = 0.0_dp
      gaer400_ph(:) = 0.0_dp
      gaer600_ph(:) = 0.0_dp
      gaer999_ph(:) = 0.0_dp

!----------------------------------------------------
!     assgin vertical profiles
!     model levels first
!----------------------------------------------------
      z_ph(1:nlev)      = zin(1:nlev)
      tlev_ph(1:nlev)   = tlevin(1:nlev)
      airlev_ph(1:nlev) = airlevin(1:nlev)
      rh_ph(1:nlev)     = rhin(1:nlev)
      xlwc_ph(1:nlev)   = xlwcin(1:nlev)
      o3_ph(1:nlev)     = o3in(1:nlev)
      acb1_ph(1:nlev)   = acb1in(1:nlev)
      acb2_ph(1:nlev)   = acb2in(1:nlev)
      aoc1_ph(1:nlev)   = aoc1in(1:nlev)
      aoc2_ph(1:nlev)   = aoc2in(1:nlev)
      aant_ph(1:nlev)   = aantin(1:nlev)
      aso4_ph(1:nlev)   = aso4in(1:nlev)
      asal_ph(1:nlev)   = asalin(1:nlev)

! rajesh:
      tauaer300_ph(1:nlev-1) = tauaer300in(1:nlev-1)
      tauaer400_ph(1:nlev-1) = tauaer400in(1:nlev-1)
      tauaer600_ph(1:nlev-1) = tauaer600in(1:nlev-1)
      tauaer999_ph(1:nlev-1) = tauaer999in(1:nlev-1)
      waer300_ph(1:nlev-1) = waer300in(1:nlev-1)
      waer400_ph(1:nlev-1) = waer400in(1:nlev-1)
      waer600_ph(1:nlev-1) = waer600in(1:nlev-1)
      waer999_ph(1:nlev-1) = waer999in(1:nlev-1)
      gaer300_ph(1:nlev-1) = gaer300in(1:nlev-1)
      gaer400_ph(1:nlev-1) = gaer400in(1:nlev-1)
      gaer600_ph(1:nlev-1) = gaer600in(1:nlev-1)
      gaer999_ph(1:nlev-1) = gaer999in(1:nlev-1)      

      tlay_ph(1:nlev-1) = 0.5_dp*(tlev_ph(1:nlev-1) + tlev_ph(2:nlev))

!----------------------------------------------------
!     exo model levels
!----------------------------------------------------
      if( nz > nlev ) then
         z_ph(nlev+1:nz-1) = zref(kcon:nref:2)
         tlev_ph(nlev+1:nz-1) = tref(kcon:nref:2)
         airlev_ph(nlev+1:nz-1) = airref(kcon:nref:2)
         o3_ph(nlev+1:nz-1) = o3ref(kcon:nref:2)/airref(kcon:nref:2)  ! Changed to vmr

         z_ph(nz) = zref(nref)
         tlev_ph(nz) = tref(nref)
         airlev_ph(nz) = airref(nref)
         o3_ph(nz) = o3ref(nref)/airref(nref)
         tlay_ph(nlev)    = 0.5_dp*(tlev_ph(nlev) + tref(kcon))
         tlay_ph(nlev+1:) = 0.5_dp*( tlev_ph(nlev+1:nz-1) + tlev_ph(nlev+2:) )
      end if


      call photoin( chem_opt, nz, zen, o3toms, esfact,           & 
                    o3top, o2top, albedo(:,lu), z_ph, tlev_ph,   & 
                    tlay_ph, airlev_ph, rh_ph, xlwc_ph, o3_ph,   &
                    acb1_ph, acb2_ph, aoc1_ph, aoc2_ph, aant_ph, & 
                    aso4_ph, asal_ph,                            &
                    tauaer300_ph, tauaer400_ph, tauaer600_ph,    &
                    tauaer999_ph, waer300_ph, waer400_ph,        &
                    waer600_ph, waer999_ph, gaer300_ph,          &
                    gaer400_ph, gaer600_ph, gaer999_ph,          &
                    aer_ra_feedback, ftuv, adjcoe, radfld )

      do n = 1,njout
        prate(1:nlev-1,n) = ftuv(2:nlev,n)
        prate0(1:nz,n)    = ftuv(1:nz,n) 
      enddo

      deallocate( ftuv, z_ph, tlev_ph, tlay_ph, airlev_ph, rh_ph, xlwc_ph, &
                  o3_ph, acb1_ph, acb2_ph, aoc1_ph, aoc2_ph, aant_ph, aso4_ph, &
                  asal_ph, tauaer300_ph, tauaer400_ph,tauaer600_ph, tauaer999_ph,  &
                  waer300_ph, waer400_ph, waer600_ph, waer999_ph, &
                  gaer300_ph, gaer400_ph, gaer600_ph, gaer999_ph )
       
      end subroutine photo

      subroutine ftuv_timestep_init( id, julday )
!-----------------------------------------------------------------------------
!	... setup the time interpolation
!-----------------------------------------------------------------------------

      use module_ftuv_subs, only : sundis

      implicit none

!-----------------------------------------------------------------------------
!	... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in)  ::  id             ! domain id
      integer, intent(in)  ::  julday         ! day of year at present time step

!-----------------------------------------------------------------------------
!	... local variables
!-----------------------------------------------------------------------------
      integer  :: m
      real(dp) :: calday

      calday = real( julday,kind=dp)
      if( calday < col_dens(id)%day_of_year(1) ) then
         next = 1
	 last = 12
	 dels = (365._dp + calday - col_dens(id)%day_of_year(12)) &
                / (365._dp + col_dens(id)%day_of_year(1) - col_dens(id)%day_of_year(12))
      else if( calday >= col_dens(id)%day_of_year(12) ) then
	 next = 1
	 last = 12
	 dels = (calday - col_dens(id)%day_of_year(12)) &
                / (365. + col_dens(id)%day_of_year(1) - col_dens(id)%day_of_year(12))
      else
         do m = 11,1,-1
	    if( calday >= col_dens(id)%day_of_year(m) ) then
	       exit
	    end if
         end do
	 last = m
	 next = m + 1
	 dels = (calday - col_dens(id)%day_of_year(m)) / (col_dens(id)%day_of_year(m+1) - col_dens(id)%day_of_year(m))
      end if
!-----------------------------------------------------------------------------
!     set solar distance factor
!-----------------------------------------------------------------------------
         if( curjulday /= julday ) then
            curjulday = julday
            call sundis( curjulday, esfact )
         endif

      end subroutine ftuv_timestep_init

      subroutine p_interp( o2_exo_col, o3_exo_col, ptop, &
                           id, its, ite, jts, jte )
!---------------------------------------------------------------
!     	... pressure interpolation for exo col density
!---------------------------------------------------------------

      implicit none

!---------------------------------------------------------------
!     	... dummy arguments
!---------------------------------------------------------------
      integer, intent(in)   :: id
      integer, intent(in)   :: its, ite
      integer, intent(in)   :: jts, jte
      real(dp), intent(in)  :: ptop(its:ite,jts:jte)             ! pressure at photolysis top (hPa)
      real(dp), intent(out) :: o2_exo_col(its:ite,jts:jte)       ! exo model o2 column density (molecules/cm^2)
      real(dp), intent(out) :: o3_exo_col(its:ite,jts:jte)       ! exo model o3 column density (molecules/cm^2)

!---------------------------------------------------------------
!     	... local variables
!---------------------------------------------------------------
      integer  :: i, j, k, ku, kl
      real(dp) :: pinterp
      real(dp) :: delp
      real(dp) :: tint_vals(2)

lat_loop : &
      do j = jts,jte
long_loop : &
         do i = its,ite
            pinterp = ptop(i,j)
            if( pinterp < col_dens(id)%col_levs(1) ) then
               ku = 1
               kl = 1
               delp = 0._dp
            else
	       do ku = 2,col_dens(id)%ncoldens_levs
                  if( pinterp <= col_dens(id)%col_levs(ku) ) then
                     kl = ku - 1
		     delp = log( pinterp/col_dens(id)%col_levs(kl) )/log( col_dens(id)%col_levs(ku)/col_dens(id)%col_levs(kl) )
		     exit
	          end if
	       end do
            end if
	    tint_vals(1) = col_dens(id)%o2_col_dens(i,j,kl,last) &
                           + delp * (col_dens(id)%o2_col_dens(i,j,ku,last) &
                                     - col_dens(id)%o2_col_dens(i,j,kl,last))
	    tint_vals(2) = col_dens(id)%o2_col_dens(i,j,kl,next) &
                           + delp * (col_dens(id)%o2_col_dens(i,j,ku,next) &
                                     - col_dens(id)%o2_col_dens(i,j,kl,next))
	    o2_exo_col(i,j) = tint_vals(1) + dels * (tint_vals(2) - tint_vals(1))
	    tint_vals(1) = col_dens(id)%o3_col_dens(i,j,kl,last) &
                           + delp * (col_dens(id)%o3_col_dens(i,j,ku,last) &
                                     - col_dens(id)%o3_col_dens(i,j,kl,last))
	    tint_vals(2) = col_dens(id)%o3_col_dens(i,j,kl,next) &
                           + delp * (col_dens(id)%o3_col_dens(i,j,ku,next) &
                                     - col_dens(id)%o3_col_dens(i,j,kl,next))
	    o3_exo_col(i,j) = tint_vals(1) + dels * (tint_vals(2) - tint_vals(1))
         end do long_loop
      end do lat_loop

      end subroutine p_interp
 
      end module module_ftuv_driver
