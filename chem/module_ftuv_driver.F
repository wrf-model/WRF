! FTUV has been updated to conform with the v3 vertical indexing
! method with kte=kde-1. "Saves" were also added to the module level
! variables that are needed between calls since they could in theory be
! lost between calls to FTUV without them. In reality, I do not think
! this was happening due to the "use" statements, but better safe then
! sorry. Finally, a critical bug was fixed where the aerosol arrays were
! not set to 0 with non-SORGAM aerosol settings leading to junk J rates.
!
!!!! NOTE TO FTUV DEVELOPERS:
!         The initialization routine needs to be changed to work using
!         variable terrain height since the model top is not constant
!         in time. It also is not constant in space, which is a bigger
!         issue for most cases. At the very least, one needs to consider
!         if the model top has been set at 50 vs. 100 mb, or something
!         else entirely.
!
! William.Gustafson@pnl.gov; 18-Sep-2008
!
      module module_ftuv_driver

      use module_wave_data, only : nw

      implicit none

!     private variables
      integer, private, parameter :: nref = 51
      integer, private, save :: nz, kcon
      integer, private, save :: curjulday = 0
      real(8), private, save :: esfact = 1.0

      real(8), private, save :: o3top = 2.97450e+16
      real(8), private, save :: o2top = 3.60906e+21

      real(8), private, save :: zref(nref), tref(nref), airref(nref), o3ref(nref)
      real(8), private, save :: albedo(nw-1,2)

      real(8), private, allocatable, save :: z_ph(:), tlev_ph(:), tlay_ph(:), airlev_ph(:)
      real(8), private, allocatable, save :: rh_ph(:), xlwc_ph(:)
      real(8), private, allocatable, save :: o3_ph(:)
      real(8), private, allocatable, save :: acb1_ph(:), acb2_ph(:)
      real(8), private, allocatable, save :: aoc1_ph(:), aoc2_ph(:)
      real(8), private, allocatable, save :: aant_ph(:), aso4_ph(:), asal_ph(:)

      real(8), private, allocatable, save :: ftuv(:,:)


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


!     private parameters
      integer, private, parameter :: pid_no2   = 4
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
      integer, private, parameter :: pid_op1   = 24
      integer, private, parameter :: pid_op2   = 1    ! unknown
      integer, private, parameter :: pid_paa   = 16
      integer, private, parameter :: pid_ket   = 23
      integer, private, parameter :: pid_glya  = 21
      integer, private, parameter :: pid_glyb  = 21
      integer, private, parameter :: pid_mgly  = 22
      integer, private, parameter :: pid_dcb   = 1     ! unknown
      integer, private, parameter :: pid_onit  = 25


      contains
      subroutine ftuv_radm2_driver(                                    &
                              id, curr_secs, dtstep, config_flags,     &
                              gmt, julday,                             &
                              p_phy, t_phy, rho_phy, p8w, t8w,         &
                              xlat, xlong,    z_at_w,                  &
                              moist, chem,gd_cloud,gd_cloud2,          &
                              ph_no2,ph_o31d,ph_o33p,ph_hono,          &
                              ph_hno3,ph_hno4,ph_no3o2,ph_no3o,        &
                              ph_h2o2,ph_ch2om,ph_ch2or,ph_ald,        &
                              ph_op1,ph_op2,ph_paa,ph_ket,ph_glya,     &
                              ph_glyb,ph_mgly,ph_dcb,ph_onit,          &
                              ph_macr,ph_ch3coc2h5,                    &
                              ids,ide, jds,jde, kds,kde,               &
                              ims,ime, jms,jme, kms,kme,               &
                              its,ite, jts,jte, kts,kte                )

      use module_configure
      use module_state_description
      use module_model_constants

      use module_wave_data, only : tuv_jmax

      implicit none

!     input arguments
      integer, intent(in   ) :: id, julday,                            &
                                ids,ide, jds,jde, kds,kde,             &
                                ims,ime, jms,jme, kms,kme,             &
                                its,ite, jts,jte, kts,kte
                               
      real(kind=8), intent(in) :: curr_secs

      real,    intent(in   ) :: dtstep, gmt

      type(grid_config_rec_type),  intent(in   ) :: config_flags

      real, dimension( ims:ime, kms:kme, jms:jme ),                    &
            intent(in   ) :: p_phy, t_phy, rho_phy, p8w, t8w, z_at_w
!
! these arrays are for cloudwater/ice coming from grell convection, optional
      real, dimension( ims:ime, kms:kme, jms:jme ),                    &
            intent(in   ) :: gd_cloud,gd_cloud2

      real, dimension( ims:ime, jms:jme ),                             &
            intent(in   ) :: xlat, xlong

      real, dimension( ims:ime, kms:kme, jms:jme, num_moist ),         &
            intent(in   ) :: moist

      real, dimension( ims:ime, kms:kme, jms:jme, num_chem ),          &
            intent(in   ) :: chem

!     output arguments
      real, dimension( ims:ime, kms:kme, jms:jme ),                    &
            intent(out  ) :: ph_no2, ph_o31d, ph_o33p, ph_hono,        &
                             ph_hno3, ph_hno4, ph_no3o2, ph_no3o,      &
                             ph_h2o2, ph_ch2om, ph_ch2or, ph_ald,      &
                             ph_op1, ph_op2, ph_paa, ph_ket, ph_glya,  &
                             ph_glyb, ph_mgly, ph_dcb,                 &
                             ph_onit, ph_macr, ph_ch3coc2h5

!     local parameters
      real(8), parameter :: xair_kg = 1.0e3/28.97*6.023e23*1.0e-6

!     local scalars
      integer :: i, j, k
      integer :: isorg
      real(8)    :: xtime, xhour, xmin, gmtp
      real(8)    :: dobson
      integer :: lu

      real(8)    :: wrk, qs, es

      logical, save :: first = .true.

!     local arrays
      real(8) :: temp(kts:kte), o3(kts:kte), air(kts:kte)
      real(8) :: zh(kts:kte), rh(kts:kte), xlwc(kts:kte)

      real(8) :: acb1(kts:kte), acb2(kts:kte), aoc1(kts:kte), aoc2(kts:kte)
      real(8) :: aant(kts:kte), aso4(kts:kte), asal(kts:kte)

      real(8) :: prate(kts:kte,tuv_jmax)

!     if the procedure is called first time, the initial subroutine is called
      if( first ) then
        call photo_inti( kte, 20.d0 )
        first = .false.
      endif

!     calculate the GMT
      isorg=0
      aer_select: SELECT CASE(config_flags%chem_opt)
         CASE (RADM2SORG,RADM2SORG_KPP,RACMSORG_KPP,RACMSORG_AQ)
             isorg=1
             CALL wrf_debug(15,'SORGAM aerosols initialization ')
         CASE DEFAULT
             CALL wrf_debug(15,'no aerosols initialization yet')
   END SELECT aer_select

      xtime = curr_secs/60._8
      xhour = real( int(gmt+0.01,8) + int(xtime/60._8,8),8 )
      xmin  = 60.0*gmt + ( xtime - xhour*60._8 )
      gmtp  = mod( xhour, 24.0d0 )
      gmtp  = gmtp + xmin/60.0

!     set total ozone and land use type
      dobson = 265.0; lu = 1

!     get photolysis rates
      J_TILE_LOOP : do j = jts, jte
      I_TILE_LOOP : do i = its, ite

        do k = kts, kte-1
       
          temp(k+1) = t_phy(i,k,j)                            ! temperature(K)
          air(k+1)  = xair_kg*rho_phy(i,k,j)                  ! air num.(molecules/cm^3)
          o3(k+1)   = chem(i,k,j,p_o3)*1.0e-6                 ! ozone(VMR)
       
          wrk       = t_phy(i,k,j) - 273.0
          es        = 6.11*10.0**(7.63*wrk/(241.9 + wrk))     ! Magnus EQ
          qs        = 0.622*es/(p_phy(i,k,j)*0.01)            ! sat mass mix (H2O)
          wrk       = moist(i,k,j,p_qv)/qs
          rh(k+1)   = max( 0.0d0, min( 1.0d0, wrk) )              ! relative huminity
       
          if(p_qc.gt.1)xlwc(k+1) = 1.0e3*moist(i,k,j,p_qc)*rho_phy(i,k,j)  ! cloud water(g/m^3)
          if( xlwc(k+1) < 1.0e-6 ) xlwc(k+1) = 0.0
       
          zh(k+1) = .5*(z_at_w(i,k+1,j)+z_at_w(i,k,j))*0.001 - z_at_w(i,kts,j)*0.001      ! height(km)
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
          if(isorg == 1 ) then
       
          acb1(k+1) = chem(i,k,j,p_ecj)
          acb2(k+1) = 0.0
!         aoc1(k+1) = chem(i,k,j,p_orgpaj) + chem(i,k,j,p_orgbaj) + chem(i,k,j,p_orgaj)
          aoc1(k+1) = chem(i,k,j,p_orgpaj)
          aoc2(k+1) = 0.0
          aant(k+1) = chem(i,k,j,p_no3aj) + chem(i,k,j,p_nh4aj)
          aso4(k+1) = chem(i,k,j,p_so4aj)
          asal(k+1) = chem(i,k,j,p_seas)

          else !Cannot do aerosol effect for non-SORGAM aerosol modules
               !This is a temporary hack; William.Gustafson@pnl.gov, 17-Sep-2008

          acb1(k+1) = 0.0
          acb2(k+1) = 0.0
          aoc1(k+1) = 0.0
          aoc2(k+1) = 0.0
          aant(k+1) = 0.0
          aso4(k+1) = 0.0
          asal(k+1) = 0.0

          endif

!==================================================================================
       
        enddo

        temp(1) = t8w(i,kts,j)
        air(1)  = xair_kg*( p8w(i,kts,j)/t8w(i,kts,j)/r_d )
        o3(1)   = o3(2)
        rh(1)   = rh(2)
        xlwc(1) = 0.
        zh(1)   = 0. 
        acb1(1) = acb1(2)
        acb2(1) = acb2(2)
        aoc1(1) = aoc1(2)
        aoc2(1) = aoc2(2)
        aant(1) = aant(2)
        aso4(1) = aso4(2)
        asal(1) = asal(2)

!     smooth air density ...
        do k = kts+1, kte-1
          air(k) = .5*air(k) + .25*( air(k-1) + air(k+1) )
        enddo

        call photo( kte,                  &
                    tuv_jmax,             &
                    julday,               &
                    gmtp,                 &
                    dble(xlat(i,j)),      &
                   -dble(xlong(i,j)),     &
                    dobson,               &
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
                    prate )

        do k = kts, kte-1 
          ph_no2(i,k,j)   = prate(k,pid_no2)*60.
          ph_o31d(i,k,j)  = prate(k,pid_o31d)*60.
          ph_o33p(i,k,j)  = prate(k,pid_o33p)*60.
          ph_hono(i,k,j)  = prate(k,pid_hono)*60.
          ph_hno3(i,k,j)  = prate(k,pid_hno3)*60.
          ph_hno4(i,k,j)  = prate(k,pid_hno4)*60.
          ph_no3o2(i,k,j) = prate(k,pid_no3o2)*60.
          ph_no3o(i,k,j)  = prate(k,pid_no3o)*60.
          ph_h2o2(i,k,j)  = prate(k,pid_h2o2)*60.
          ph_ch2om(i,k,j) = prate(k,pid_ch2om)*60.
          ph_ch2or(i,k,j) = prate(k,pid_ch2or)*60.
          ph_ald(i,k,j)   = prate(k,pid_ald)*60.
          ph_op1(i,k,j)   = prate(k,pid_op1)*60.
          ph_op2(i,k,j)   = prate(k,pid_op2)*60.
          ph_paa(i,k,j)   = prate(k,pid_paa)*60.
          ph_ket(i,k,j)   = prate(k,pid_ket)*60.
          ph_glya(i,k,j)  = prate(k,pid_glya)*60.
          ph_glyb(i,k,j)  = prate(k,pid_glyb)*60.
          ph_mgly(i,k,j)  = prate(k,pid_mgly)*60.
          ph_dcb(i,k,j)   = prate(k,pid_dcb)*60.
          ph_onit(i,k,j)  = prate(k,pid_onit)*60.
          ph_macr(i,k,j)  = prate(k,28) *60.  !
          ph_ch3coc2h5(i,k,j)=prate(k,23)*60. !
        enddo

      enddo I_TILE_LOOP
      enddo J_TILE_LOOP

      end subroutine ftuv_radm2_driver

      subroutine photo_inti( nlev, ztopin ) 

      use module_wave_data, only : nw, wl, tuv_jmax, wave_data_inti
      use module_ftuv_subs, only : nwint, wlint, schu_inti, inter_inti

      implicit none

!     input arguments
      integer, intent(in) :: nlev
      real(8), intent(in) :: ztopin

!     local arguments
      integer :: k, kdis, nabv, iw
      real(8) :: ztop

!     change unit from m to km
      ztop = ztopin

!     first, set the top levels height, temperature and o3 profile 
      do k = 1, nref
        if( ztop < zref(k) ) exit
      enddo
      if( k==1 .or. k>nref ) then
        print*, 'Cannot process these two conditions ...', ztop
        stop 'in photo_inti'
      endif

      kcon = k + 1
      kdis = nref - kcon
      nabv = kdis/2 + 1
      if( mod(kdis,2) /= 0 ) then
        nabv = nabv + 1
      endif
      nz = nlev + nabv

!     allocate memory space
      allocate( z_ph(nz), tlev_ph(nz), tlay_ph(nz-1), airlev_ph(nz) )
      allocate( rh_ph(nz), xlwc_ph(nz) )
      allocate( o3_ph(nz) )
      allocate( acb1_ph(nz), acb2_ph(nz), aoc1_ph(nz), aoc2_ph(nz) )
      allocate( aant_ph(nz), aso4_ph(nz), asal_ph(nz) )
      allocate( ftuv(nz,tuv_jmax) )

      z_ph(nlev+1:nz-1) = zref(kcon:nref:2)
      tlev_ph(nlev+1:nz-1) = tref(kcon:nref:2)
      airlev_ph(nlev+1:nz-1) = airref(kcon:nref:2)
      o3_ph(nlev+1:nz-1) = o3ref(kcon:nref:2)/airref(kcon:nref:2)  ! Changed to vmr

      z_ph(nz) = zref(nref)
      tlev_ph(nz) = tref(nref)
      airlev_ph(nz) = airref(nref)
      o3_ph(nz) = o3ref(nref)/airref(nref)

      tlay_ph(nlev+1:) = 0.5*( tlev_ph(nlev+1:nz-1) + tlev_ph(nlev+2:) )

      rh_ph(:)   = 0.0
      xlwc_ph(:) = 0.0
      acb1_ph(:) = 0.0
      acb2_ph(:) = 0.0
      aoc1_ph(:) = 0.0
      aoc2_ph(:) = 0.0
      aant_ph(:) = 0.0
      aso4_ph(:) = 0.0
      asal_ph(:) = 0.0

!     second, set albedo for land
      do iw = 1, nw-1
        if( wl(iw)<400.0 ) albedo(iw,1) = 0.05
        if( (wl(iw)>=400.0 ) .and. (wl(iw)<450.0) ) albedo(iw,1) = 0.06
        if( (wl(iw)>=450.0 ) .and. (wl(iw)<500.0) ) albedo(iw,1) = 0.08
        if( (wl(iw)>=500.0 ) .and. (wl(iw)<550.0) ) albedo(iw,1) = 0.10
        if( (wl(iw)>=550.0 ) .and. (wl(iw)<600.0) ) albedo(iw,1) = 0.11
        if( (wl(iw)>=600.0 ) .and. (wl(iw)<640.0) ) albedo(iw,1) = 0.12
        if( (wl(iw)>=640.0 ) .and. (wl(iw)<660.0) ) albedo(iw,1) = 0.135
        if( wl(iw)>=660.0 ) albedo(iw,1) = 0.15
      end do

!     set albedo for sea water
      albedo(1:17,2) = (/ 0.8228, 0.8140, 0.8000, 0.7820, 0.7600,   &
                          0.7340, 0.7040, 0.6700, 0.6340, 0.4782,   &
                          0.3492, 0.3000, 0.2700, 0.2400, 0.1700,   &
                          0.0800, 0.0600 /)

!     third, intialize other modules
      call schu_inti
      call wave_data_inti( nz )
      call inter_inti( nw, wl, nwint, wlint )

      print*, 'finish initialization ...'

      end subroutine photo_inti

      subroutine photo( nlev, njout, julday, gmtp, alat, along, o3toms, lu,     &
                        zin, tlevin, airlevin, rhin, xlwcin, o3in,              &
                        acb1in, acb2in, aoc1in, aoc2in,                         &
                        aantin, aso4in, asalin,                                 &
                        prate )

      use module_ftuv_subs, only : sundis, calc_zenith, photoin

      implicit none

!     input arguments
      integer, intent(in) :: nlev, njout
      integer, intent(in) :: julday
      real(8), intent(in) :: gmtp
      real(8), intent(in) :: alat, along
      real(8), intent(in) :: o3toms
      integer, intent(in) :: lu 

      real(8), intent(in) :: zin(nlev)
      real(8), intent(in) :: tlevin(nlev)
      real(8), intent(in) :: airlevin(nlev)
      real(8), intent(in) :: rhin(nlev)
      real(8), intent(in) :: xlwcin(nlev)
      real(8), intent(in) :: o3in(nlev)
      real(8), intent(in) :: acb1in(nlev)
      real(8), intent(in) :: acb2in(nlev)
      real(8), intent(in) :: aoc1in(nlev)
      real(8), intent(in) :: aoc2in(nlev)
      real(8), intent(in) :: aantin(nlev)
      real(8), intent(in) :: aso4in(nlev)
      real(8), intent(in) :: asalin(nlev)

!     output arguments
      real(8), intent(out) :: prate(nlev,njout)

!     local arugments
      integer :: k, n
      real(8) :: azim, zen

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
!-----------------------------------------------------------------------------
      if( curjulday /= julday ) then
        curjulday = julday
        call sundis( curjulday, esfact )
      endif

!     solar zenith angle calculation:
      call calc_zenith( alat, along, julday, gmtp, azim, zen )
      if( zen == 90.0 ) then
        zen = 89.0
      elseif( zen >= 95.0 ) then
        prate(:,:) = 0.0
        return
      endif

!     assgin vertical profiles
      z_ph(1:nlev) = zin(1:nlev)
      tlev_ph(1:nlev) = tlevin(1:nlev)
      airlev_ph(1:nlev) = airlevin(1:nlev)
      rh_ph(1:nlev) = rhin(1:nlev)
      xlwc_ph(1:nlev) = xlwcin(1:nlev)
      o3_ph(1:nlev) = o3in(1:nlev)
      acb1_ph(1:nlev) = acb1in(1:nlev)
      acb2_ph(1:nlev) = acb2in(1:nlev)
      aoc1_ph(1:nlev) = aoc1in(1:nlev)
      aoc2_ph(1:nlev) = aoc2in(1:nlev)
      aant_ph(1:nlev) = aantin(1:nlev)
      aso4_ph(1:nlev) = aso4in(1:nlev)
      asal_ph(1:nlev) = asalin(1:nlev)

      tlay_ph(1:nlev-1) = 0.5*( tlev_ph(1:nlev-1) + tlev_ph(2:nlev) )
      tlay_ph(nlev) = 0.5*( tlev_ph(nlev) + tref(kcon) )

      call photoin( nz,             & 
                    zen,            & 
                    o3toms,         & 
                    esfact,         & 
                    o3top,          & 
                    o2top,          & 
                    albedo(:,lu),   &
                    z_ph,           &  
                    tlev_ph,        & 
                    tlay_ph,        & 
                    airlev_ph,      &
                    rh_ph,          & 
                    xlwc_ph,        & 
                    o3_ph,          &
                    acb1_ph,        & 
                    acb2_ph,        & 
                    aoc1_ph,        & 
                    aoc2_ph,        & 
                    aant_ph,        & 
                    aso4_ph,        & 
                    asal_ph,        & 
                    ftuv )

      do k = 1, nlev-1
        prate(k,1:njout) = ftuv(nz-k,1:njout)
      enddo
       
      end subroutine photo
 
      end module module_ftuv_driver
