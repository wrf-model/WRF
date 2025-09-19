MODULE module_diag_cordex
! WRF module to compute atmospheric model diagnostics required by CORDEX
! L. Fita, CIMA. December 2017
!
! This module computes that variables required by CORDEX which usually are post-processed
!
! Subroutines/Functions
!
! module_diag_cdx: Main subroutine to compute CORDEX required variables

! Available diagnostics

!! Instantaneous diagnostics (only computed on output times)
!!! prw: Total water path [kgm-2]
!!! clwvi: Total liquid water path (QCLOUD + QRAIN + QSNOW + QICE) [kgm-2]
!!! clivi: Total ice water path (QSNOW+QICE+GRAUPEL+QHAIL) [kgm-2]
!!! clgvi: Total graupel water path (QGRAUPEL) [kgm-2]
!!! clhvi: Total hail water path (QHAIL) [kgm-2]
!!! ua: 3D earth-rotated eastward wind [ms-1]
!!! va: 3D earth-rotated northward wind [ms-1]
!!! ws: 3D wind speed [ms-1]
!!! ta: 3D air-temperature [K]
!!! press: 3D air pressure [Pa]
!!! zg: 3D geopotential height [m]
!!! hur: 3D relative humidty [1]
!!! hus: 3D specific humidty [1]
!!! uas: 10m earth-rotated eastward wind [ms-1]
!!! vas: 10m earth-rotated northward wind [ms-1]
!!! wss: 10m earth-rotated northward wind [ms-1]
!!! wbds: 10m earth-rotated blowing wind direction [rad]
!!! hurs: 2m relative humidty [1]
!!! huss: 2m specific humidty [1]
!!! psl: sea level pressure [Pa]
!!! tas_hm: 2-m air temperature using corrector-calculator (Hess-McAvaney, 1995) method [K]
!!! qv_hm: 2-m water vapour mixing ratio using corrector-calculator (Hess-McAvaney, 1995) method [kgkg-1]
!!! cape: CAPE [Jkg-1]
!!! cin: CIN [Jkg-1]
!!! zlfc: Height at LFC [m]
!!! plfc: Pressure at LFC [Pa]
!!! lidx: Lifted index [1]
!!! zmla: pbl height following a generic method [m]
!!! ws100: 100m wind speed [ms-1]
!!! uz100: 100m wind x-direction [ms-1]
!!! vz100: 100m wind y-direction [ms-1]
!!! ws50: 50m wind speed [ms-1]
!!! uz50: 50m wind x-direction [ms-1]
!!! vz50: 50m wind y-direction [ms-1]
!!! taz50: 50m air temperature [K]
!!! qvz50: 50m water vapour [kgkg-1]
!!! tauu, tauuv: components of the downward wind stress at 10 m [m2s-2] (might be 
!!!   zero if sf_sfclay_physics /= 1, 5)
!!! tauugen, tauuvgen: generic components of the downward wind stress at 10 m [m2s-2]
!!! cdcdx: drag coefficient [-] (might be zero if sf_sfclay_physics /= 1, 5)
!!! cdgen: generic drag coefficient [-]
!!! fog: whether point is inside fog (vis < 1km) [1]
!!! vis: visibility inside fog [km]
!!! tds: 2m dew point temperature [K]
!!! tws: wet-bulb temperature [K]
!!! ps: surface pressure [Pa]
!!! ts: skin temperature [K]
!!! zeroith: 0-isotherm [m]
!!! mrsol: moisture content of soil layer [kgm-2]
!!! mrsos: first layer soil moisture content (0-10 cm) [kgm-2]
!!! mrso: total soil moisture content [kgm-2]
!!! mrsll: liquid water content of soil layer [kgm-2]
!!! mrlso: total liquid water content of soil layer [kgm-2]
!!! mrlsos: liquid water content in Upper Portion of soil column (0-10cm) [kgm-2]
!!! mrsfl: frozen water content of soil layer [kgm-2]
!!! mrfso: total frozen water content of soil layer [kgm-2]
!!! mrfsos: frozen water content in Upper Portion of soil column (0-10cm) [kgm-2]

!! Accumulated or similar time dependency (computed at every time-step)
!!!
!!! cdgen: generic drag coefficient (instantaneous values) [-]
!!! clt: total cloud cover [%]                              !!!!!  NOTE: CLDFRAC is computed by the radiative scheme thus,
!!! cll: low-level (p >= 68000 Pa) cloud cover [%]          !!  bear in mind that:
!!! clm: mid-level (44000 <= p < 68000 Pa) cloud cover [%]  !!    auxhist9_interval > radt
!!! clh: high-level (p < 44000 Pa) cloud cover [%]          !!  otherwise one obtains repeated values !! 
!!! cltmean: mean clt [%]
!!! cllmean: mean cll [%]
!!! clmmean: mean clm [%]
!!! clhmean: mean clh [%]
!!! wsgsmax: maximum surface wind gust [ms-1]
!!! ugsmax: x maximum surface gust wind direction [ms-1]
!!! vgsmax: y maximum surface gust wind direction [ms-1]
!!! percengust: percentage of time where the grid point got gust winds [%]
!!! totwsgsmax: total (TKE + heavy precip) maximum surface wind gust [ms-1]
!!! totugsmax: total x maximum surface gust wind direction [ms-1]
!!! totvgsmax: total y maximum surface gust wind direction [ms-1]
!!! totpercengust: percentage of time where the grid point got total gust winds [%]
!!! wsz100max: maximum 100m wind [ms-1]
!!! uz100max: x maximum 100m wind direction [ms-1]
!!! vz100max: y maximum 100m wind direction [ms-1]
!!! sunshine: ac. time with swdown > 120. Wm-2 [s]
!!! rsds: mean surface Downwelling Shortwave Radiation [Wm-2]
!!! rlds: mean surface Downwelling Longwave Radiation [Wm-2]
!!! hfls: mean surface Upward Latent Heat Flux [Wm-2]
!!! hfss: mean surface Upward Sensible Heat Flux [Wm-2]
!!! rsus: mean surface Upwelling Shortwave Radiation [Wm-2]
!!! rlus: mean surface Upwelling Longwave Radiation [Wm-2]
!!! rsusgen: generic mean surface Upwelling Shortwave Radiation [Wm-2]
!!! rlusgen: generic mean surface Upwelling Longwave Radiation [Wm-2]
!!! evspsbl: mean evaporation [kgm-2s-1]
!!! evspsblpot: mean potential evapotranspiration [kgm-2s-1]
!!! evspsblpotgen: mean generic potential evapotranspiration [kgm-2s-1]
!!! cdgen: generic drag coefficient [-]
!!! snc: mean snow area fraction [%]
!!! snd: mean snow depth [m]
!!! mrros: mean surface Runoff [kgm-2s-1]
!!! mrro: mean total  Runoff [kgm-2s-1]
!!! mrsolmean: mean moisture content of soil layer [kgm-2]
!!! mrsosmean: mean first layer soil moisture content (0-10 cm) [kgm-2]
!!! mrsomean: mean total soil moisture content [kgm-2]
!!! mrsllmean: mean liquid water content of soil layer [kgm-2]
!!! mrlsomean: mean total liquid water content of soil layer [kgm-2]
!!! mrlsosmean: mean liquid water content in Upper Portion of soil column (0-10cm) [kgm-2]
!!! mrsflmean: mean frozen water content of soil layer [kgm-2]
!!! mrfsomean: mean total frozen water content of soil layer [kgm-2]
!!! mrfsosmean: mean frozen water content in Upper Portion of soil column (0-10cm) [kgm-2]
!!! flxpr: precipitation flux [kgm-2s-1]
!!! flxprl: large scale precipitation flux [kgm-2s-1]
!!! flxprc: convective precipitation flux [kgm-2s-1]
!!! flxprsh: shallow-cumulus precipitation flux [kgm-2s-1]
!!! flxprsn: solid precipitation flux [kgm-2s-1]
!!! snw: accumulated snow  [ksm-2]
!!! rsdt: mean top of the Atmosphere (TOA) incident shortwave radiation [kgm-2]
!!! rsut: mean top of the Atmosphere (TOA) outgoing shortwave radiation [kgm-2]
!!! rlut: mean top of the Atmosphere (TOA) outgoing Longwave radiation [kgm-2]
!!! tas_hm: 2-m air temperature using corrector-calculator (Hess-McAvaney, 1995) method [K]
!!! qv_hm: 2-m water vapour mixing ratio using corrector-calculator (Hess-McAvaney, 1995) method [kgkg-1]
!!! capemin: minimum CAPE [Jkg-1]
!!! cinmin: minimum CIN [Jkg-1]
!!! zlfcmin: minimum height at LFC [m]
!!! plfcmin: minimum Pressure at LFC [Pa]
!!! lidxmin: minimum Lifted index [1]
!!! capemax: maximum CAPE [Jkg-1]
!!! cinmax: maximum CIN [Jkg-1]
!!! zlfcmax: maximum height at LFC [m]
!!! plfcmax: maximum Pressure at LFC [Pa]
!!! lidxmax: maximum Lifted index [1]
!!! capemean: mean CAPE [Jkg-1]
!!! cinmean: mean CIN [Jkg-1]
!!! zlfcmean: mean height at LFC [m]
!!! plfcmean: mean Pressure at LFC [Pa]
!!! lidxmean: mean Lifted index [1]
!!! tfog: time of presence of fog [s]
!!! fogvisbltymin: minimun visibility inside fog [km]
!!! fogvisbltymax: maximun visibility inside fog [km]
!!! fogvisbltymean: mean visibility inside fog [km]
!!! tdsmin: minimum 2m dew point temperature [K]
!!! tdsmax: maximum 2m dew point temperature [K]
!!! tdsmean: mean 2m dew point temperature [K]
!!! twsmin: minimum 2m wet-bulb temperature [K]
!!! twsmax: maximum 2m wet-bulb temperature [K]
!!! twsmean: mean 2m wet-bulb temperature [K]
!!! tashurstreshighres: high resolution of simultaneous temporal residence of 2-meter temperature 
!!!   and relative humidity
!!! tashurstreslowres: low resolution of simultaneous temporal residence of 2-meter temperature 
!!!   and relative humidity
!!! wbdswsstres: simultaneous temporal residence of 10-meter wind direction (from where it blows) 
!!!   and wind speed
!!! wbacdh: Water-budget vertically integrated accumulated diabatic heating [K]
!!! wbacpw, wbacpw[c/r/s/i/g/h]: Water-budget vertically integrated accumulated total tendency for water 
!!!   vapour, cloud, rain, snow, ice, graupel, hail [mm]
!!! wbacf, wbacf[c/r/s/i/g/h]: Water-budget vertically integrated accumulated horizontal advection for 
!!!   water vapour, cloud, rain, snow, ice, graupel, hail [mm]
!!! wbacz, wbacz[c/r/s/i/g/h]: Water-budget vertically integrated accumulated vertical advection for 
!!!   water vapour, cloud, rain, snow, ice, graupel, hail [mm]
!!! wbacdh{l/m/h}: Water-budget vertically integrated accumulated diabatic heating at low, medimum and high levels [K]
!!! wbacpw[v/c/r/s/i/g/h]{l/m/h}: Water-budget vertically integrated accumulated total 
!!!   tendency for water vapour, cloud, rain, snow, ice, graupel, hail at low, medium and high levels 
!!!   (same as cloudiness) [mm]
!!! wbacf[v/c/r/s/i/g/h]{l/m/h}: Water-budget vertically integrated accumulated horizontal 
!!!   advection for water vapour, cloud, rain, snow, ice, graupel, hail at low, medium and high levels 
!!!   (same as cloudiness) [mm]
!!! wbacz[v/c/r/s/i/g/h]{l/m/h}: Water-budget vertically integrated accumulated vertical 
!!!   advection for water vapour, cloud, rain, snow, ice, graupel, hail at low, medium and high levels 
!!!   (same as cloudiness) [mm]

  ! sea level pressure methods
  INTEGER, PARAMETER                                     :: dpslshuell = 1
  INTEGER, PARAMETER                                     :: dpslptarget = 2
  INTEGER, PARAMETER                                     :: dpslecmwf = 3
  ! generic boundary layer
  INTEGER, PARAMETER                                     :: zmlagenthetae = 1
  INTEGER, PARAMETER                                     :: zmlagenbulk = 2  
  ! surface wind-gust  methods
  INTEGER, PARAMETER                                     :: dwsgsbrasseur01 = 1
  INTEGER, PARAMETER                                     :: dwsgsheavyprecip = 2
  ! Methods for vertical wind extrapolation
  INTEGER, PARAMETER                                     :: dwsz100powerlaw = 1
  INTEGER, PARAMETER                                     :: dwsz100loglaw = 2
  INTEGER, PARAMETER                                     :: dwsz100MO = 3
  ! Methods for potential evapotranspiration
  INTEGER, PARAMETER                                     :: dpotevaporbulk = 1
  INTEGER, PARAMETER                                     :: dpotevaporMilly92 = 2
  ! Methods for fog and visibility
  INTEGER, PARAMETER                                     :: dfogK84 = 1
  INTEGER, PARAMETER                                     :: dfogRUC = 2
  INTEGER, PARAMETER                                     :: dfogFRAML50 = 3
  CHARACTER(len=256)                                     :: fname

CONTAINS

  SUBROUTINE cordex_output_calc(                                                                      &
                      is_restart, is_output_time, nsteps,                                             & 
                      dt, xtime, curr_secs2, radt,                                                    &
                      cdxH,                                                                           &
                      psl_diag, ptarget, psmooth,                                                     &
                      wsgs_diag,                                                                      &
                      wbout,                                                                          &
                      wsz100_diag, zhgtzw, z50_diag,                                                  &
                      dqv, dtheta,                                                                    &
                      potevap_diag,                                                                   &
                      convxtrm_diag,                                                                  &
                      visibility_diag, fogvars, zmlagen_diag,                                         &
                      timedbg, wgsts_wbdswsstres,                                                     &
                      londbg, latdbg,                                                                 &
                      idbg, jdbg,                                                                     &
!#ifdef CDXWRF
!#if CDXWRF>=3
                      Nhtasrng, Nhhursrng, Nltasrng, Nlhursrng, Nwbdsrng, Nwssrng,                    &
                      Nhtashursrng, Nltashursrng, Nwbdswssrng,                                        &
                      tashrng, hurshrng, taslrng, hurslrng,  wbdsrng, wssrng,                         &
!#endif
!#endif
                   ! Input
                      lon, lat,                                                                       &
                      cldfra, p8w, p, pb,                                                             &
                      pclt, pcll, pclm, pclh,                                                         &
                      qv, qc, qr, qs, qi, qg, qh,                                                     &
                      mu, mub, deta,                                                                  &
                      sina, cosa,                                                                     &
                      u, v, t, thp, ph, phb, p_phy,                                                   &
                      u10, v10, q2, t2, th2, ps, qsfc,                                                &
                      hgt, z,                                                                         &
                      dzs, smois, sh2o,                                                               &
                      tke, pblh,                                                                      &
                      prc, prnc, prsh,                                                                &
                      ust, znt, rol,                                                                  &
                      swdown,                                                                         &
                      Cd,                                                                             &
                      glw, lh, hfx, swupb, lwupb, qfx,                                                &
                      snow, snowh, snowc,                                                             &
                      sfcrunoff, udrunoff,                                                            &
                      rho,                                                                            &
                      swdnt, swupt, lwupt,                                                            &
                      ts, albedo, emiss,                                                              &
                      sr,                                                                             &
                      chs, cdm, br, zol,                                                              &
                      fracsoil10cm,                                                                   &
!#ifdef CDXWRF
!#if CDXWRF>=1
!#endif

!#if CDXWRF>=4
                      diabh,                                                                          &
                      qvttend, qcttend, qrttend, qsttend, qittend, qgttend, qhttend,                  &
                      qvhadv, qchadv, qrhadv, qshadv, qihadv, qghadv, qhhadv,                         &
                      qvzadv, qczadv, qrzadv, qszadv, qizadv, qgzadv, qhzadv,                         &
!#endif
!#endif
                   ! Output
                      cltmean, cllmean, clmmean, clhmean,                                             &
                      prw, clwvi, clivi,                                                              &
                      uas, vas, wss, wbds, hurs, huss,                                                &
                      psl,                                                                            &
                      wsgsmax, ugsmax, vgsmax, percengust,                                            &
                      totwsgsmax, totugsmax, totvgsmax, totpercengust,                                &
                      wsz100, uz100wind, vz100wind,                                                   &
                      wsz100max, uz100max, vz100max,                                                  &
                      wsz50, uz50wind, vz50wind,                                                      &
                      taz50, qvz50,                                                                   &                      
                      sunshine,                                                                       &
                      tauu, tauv,                                                                     &
                      tauugen, tauvgen,                                                               &
                      rsds, rlds, hfls, hfss, rsus, rlus, rsusgen, rlusgen,                           &
                      evspsbl, evspsblpot, evspsblpotgen,                                             &
                      cdcdx, cdgen,                                                                   &
                      snc, snd,                                                                       &
                      pmrros, pmrro,                                                                  &
                      mrros, mrro, mrso, mrsol, mrsos,                                                &
                      mrsll, mrlso, mrlsos,                                                           &
                      mrsfl, mrfso, mrfsos,                                                           &
                      mrsomean, mrsolmean, mrsosmean,                                                 &
                      mrsllmean, mrlsomean, mrlsosmean,                                               &
                      mrsflmean, mrfsomean, mrfsosmean,                                               &
                      flxpr, flxprl, flxprc, flxprsh, flxprsn, snw,                                   &
                      rsdt, rsut, rlut,                                                               &
                      tas_hm, qvs_hm,                                                                 &
!#ifdef CDXWRF
!#if CDXWRF>=1
                      clgvi, clhvi,                                                                   &
                      zmla,                                                                           &
                      capemin, cinmin, zlfcmin, plfcmin,                                              &
                      capemax, cinmax, zlfcmax, plfcmax,                                              &
                      capemean, cinmean, zlfcmean, plfcmean,                                          &
                      iutmean, ivtmean,                                                               &
                      zeroith,                                                                        &
                      lidxmin, lidxmax, lidxmean,                                                     &
!#endif
!#if CDXWRF>=2
                      ua, va, ws, zg, ta, press, hur, hus,                                            &
                      tfog, fogvisbltymin, fogvisbltymax, fogvisbltymean,                             &
                      tdsmin, tdsmax, tdsmean,                                                        &
                      twsmin, twsmax, twsmean,                                                        &
!#endif
!#if CDXWRF>=4
                   ! Water-budget
                      wbacdh,                                                                         &
                      wbacpw, wbacpwc, wbacpwr, wbacpws, wbacpwi, wbacpwg, wbacpwh,                   &
                      wbacf, wbacfc, wbacfr, wbacfs, wbacfi, wbacfg, wbacfh,                          &
                      wbacz, wbaczc, wbaczr, wbaczs, wbaczi, wbaczg, wbaczh,                          &
                      wbacdhl, wbacdhm, wbacdhh,                                                      &
                      wbacpwlv,wbacpwmv,wbacpwhv,wbacflv,wbacfmv, wbacfhv, wbaczlv, wbaczmv, wbaczhv, &
                      wbacpwlc,wbacpwmc,wbacpwhc,wbacflc,wbacfmc, wbacfhc, wbaczlc, wbaczmc, wbaczhc, &
                      wbacpwlr,wbacpwmr,wbacpwhr,wbacflr,wbacfmr, wbacfhr, wbaczlr, wbaczmr, wbaczhr, &
                      wbacpwls,wbacpwms,wbacpwhs,wbacfls,wbacfms, wbacfhs, wbaczls, wbaczms, wbaczhs, &
                      wbacpwli,wbacpwmi,wbacpwhi,wbacfli,wbacfmi, wbacfhi, wbaczli, wbaczmi, wbaczhi, &
                      wbacpwlg,wbacpwmg,wbacpwhg,wbacflg,wbacfmg, wbacfhg, wbaczlg, wbaczmg, wbaczhg, &
                      wbacpwlh,wbacpwmh,wbacpwhh,wbacflh,wbacfmh, wbacfhh, wbaczlh, wbaczmh, wbaczhh, &
!#endif
!#if CDXWRF>=3
                      tashurstreshighres, tashurstreslowres, wbdswsstres,                             &
!#endif
!#endif
                      ids,ide, jds,jde, kds,kde,                                                      &
                      ims,ime, jms,jme, kms,kme,                                                      &
                      ips,ipe, jps,jpe, kps,kpe,                                                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles,                                  &
                      num_soil_layers, n_moist,                                                       &
                      m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh                                        &
                    !! INSTVALS: Instantaneous values (uncomment and re-compile)
!                      clt, cll, clm, clh,                                                             &
!                      iut, ivt,                                                                       &
!                      wsgs, ugustwind, vgustwind, gustpoint,                                          &
!                      totwsgs, totugustwind, totvgustwind, gustpoint,                                 &
!                      potevap, potevapogen,                                                           & 
!#ifdef CDXWRF
!#if CDXWRF>=1
!                      cape, cin, zlfc, plfc, lidx,                                                    &
!#endif
!#if CDXWRF>=2
!                      fog, fogvisblty,                                                                & 
!                      tds, tws                                                                        & 
!#endif
!#endif
!! End of INSTVALS
                    )

!----------------------------------------------------------------------
! Main subroutine to compute CORDEX required variables

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 
  USE module_diagvar_cordex

  IMPLICIT NONE

!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!-- curr_secs2    current time in seconds since simulation restart
!-- radt          frequency of radiative calls (minutes)
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!-- num_soil_layers number of soil layers
!-- n_moist     number of water species
!-- m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh: moist indices for each specie: vapour, cloud, rain, snow, ice, graupel, ice and hail
!
!-----------
! is_restart: whether if simulation is a restart
! is_output_time: whether if this iteration is an output time-step
! cdxH: period of time [minute] of calculations between outputs
! psl_diag: diagnostic to use to compute sea level pressure
! ptarget: pressure target to use for psl_diag=2 (default 70000. Pa)
! psmooth: number of passses of nearest 3x3 grid-point mean filter on psfc on psl_diag=2 (default 5)
! wsgs_diag: diagnostic to compute surface windgust
! wbout: whether Water-Budget terms shoud be computed or not
! wsz100_diag: extrapolation of wind at 100m
! zhgtzw: height at which 100 m winds should be extrapolated [m] (default 100 m)
! z50_diag: height at which 50 m variables (wind, ta, qv) should be extrapolated [m] (default 50 m)
! dqv: percentage of variation of qv to determine mixed layer depth [1]
! dtheta: increment in potential temperature [K] to determine zmla [K]
! potevap_diag: diagnostic of the potential evapotranspiration
! convxtrm_diag: diagnostic of extremes from convection indices
! visibility_diag: diagnostic of visibilty inside fog
! fogvars: variables to use to diagnose fog (1: 3D, 2: sfc)
! zmlagen_diag: diagnostic of generic boundary layer height, zmla (1: thetae-stability, 2: bulk 
!   Richardson number [default])
!
! ------- INPUT vars -------
! lon: longitude
! lat: latitude
! cldfra: cloud fraction [1]
! pclt: previous radt call total cloudiness [1]
! pcll: previous radt call low level cloudiness (p >= 68000 Pa) [1]
! pclm: previous radt call mid-level cloudiness ( 68000 < p >= 44000 Pa) [1]
! pclh: previous radt call high level cloudiness (p < 44000 Pa) [1]
! p8w: 3D pressure array at full eta levels [Pa]
! P: anomaly pressure half eta levels [Pa]
! PB: base pressure half eta levels [Pa]
! qv: water vapour mixing ratio [kgkg-1]
! qc: cloud mixing ratio [kgkg-1]
! qr: rain mixing ratio [kgkg-1]
! qs: snow mixing ratio [kgkg-1]
! qi: ice mixing ratio [kgkg-1]
! qg: graupel mixing ratio [kgkg-1]
! qh: hail mixing ratio [kgkg-1]
! mu: perturbation dry air mass in column [Pa]
! mub: base dry air mass in column [Pa]
! deta: d(eta) values between full (w) levels
! sina: local sine of map rotation [1]
! cosa: local cosine of map rotation [1]
! u: 3D easthward wind direction [ms-1]
! v: 3D northward wind direction [ms-1]
! t: 3D air temperature [K]
! th: 3D air potential temperature [K]
! ph: 3D gepotential perturbation [m2s-2]
! phb: 3D gepotential base state [m2s-2]
! p_phy: 3D total pressure (p+pb) [Pa]
! u10: 10m easthward wind direction [ms-1]
! v10: 10m northward wind direction [ms-1]
! t2: 2m air temperature [K]
! th2: 2m potential air temperature [K]
! q2: 2m water vapour [kgkg-1]
! qsfc: specific humidity at the boundary [kgkg-1]
! hgt: orography [m]
! z: heights of the half levels [gpm]
! dzs: depths of the soil levels [m]
! smois: soil moisture [m3m-3]
! sh2o: liquid soil water content [m3m-3]
! rho: air density [kgm-3]
! diabh: diabatic heating from micro-physics [Ks-1]
! q[v/c/r/s/i/g/h]ttend: internal time-step tendency of water species for water vapour, cloud, rain, 
!   snow, ice, graupel, hail [kgkg-1s-1]
! q[v/c/r/s/i/g/h]hadv: horizontal advection of water species for water vapour, cloud, rain, snow, 
!   ice, graupel, hail [kgkg-1]
! q[v/c/r/s/i/g/h]zadv: vertical advection of water species for water vapour, cloud, rain, snow, ice, 
!   graupel, hail [kgkg-1]
! tke: turbulent kinetic energy [m2s-2]
! pblh: boundary layer height [m]
! prc: convective (from cumulus scheme) precipitation [kgm-2]
! prnc: non-convective (from micro-physics scheme) precipitation [kgm-2]
! prsh: shallow-cumulus (from shallow-cumulus scheme) precipitation [kgm-2]
! ust: u* in similarity theory [ms-1]
! znt: thermal time-varying roughness length [m]
! rol: inverse of Obukhov length [m-1]
! swdown: downward short-wave radiation [Wm-2]
! Cd: Drag coefficient [-]
! glw: downward long wave flux at ground surface [Wm-2]
! lh: latent heat flux at the surface [Wm-2]
! hfx: sensible heat flux at the surface [Wm-2]
! swupb: upwelling shortwave flux at bottom [Wm-2]
! lwupb: upwelling longwave flux at bottom [Wm-2]
! qfx: upward moisture flux at the surface [kgm-2s-1]
! snow: snow water equivalent [kgm-2]
! snowh: physical snow height [m]
! snowc: snow cover [1]
! sfcrunoff: surface runoff [kgm-2]
! udrunoff: underground runoff [kgm-2]
! swdnt: incident short wave at top of the Atmosphere (TOA) [Wm-2]
! swupt: outgoing short wave at top of the Atmosphere (TOA) [Wm-2]
! lwupt: outgoing long wave at top of the Atmosphere (TOA) [Wm-2]
! ts: skin temperature [k]
! albedo: surface land albedo [1]
! emiss: surface emissivity [1]
! sr: fraction of frozen precipitation [1]
! chs: surface exchange coeeficient for heat [-]
! cdm: surface exchange coefficient for momentum [-]
! br: Bulk Richardson number [-]
! zol: Characteristic length (z/L) [-]
! fracsoil10cm: fraction of the soil layer used to compute 0-10 vertical accumulations
! timedbg: frecuency in minutes of time debugging
! londbg, latdbg: coordinates of the debugging grid point (-999. if not set-up)
! idbg, jdbg: grid point coordinates of the debugging grid point
! wgsts_wbdswsstres: whether wind gusts should be used for wbds-wss residence-time (0: no, 1: yes)

!-------- OUTPUT vars ------- [defined at registry.cordex] and included on Registry.EM_COMMON on auxhist9
! nsteps: counter of time-steps passed for computation of statistics
! clt: total cloudiness [%]
! cll: low level cloudiness (p >= 68000 Pa) [%]
! clm: mid-level cloudiness ( 68000 < p >= 44000 Pa) [%]
! clh: high level cloudiness (p < 44000 Pa) [%]
! cltmean: mean clt [%]
! cllmean: mean cll [%]
! clmmean: mean clm [%]
! clhmean: mean clh [%]
! prw: Total water path [kgm-2]
! clwvi: Total condensated water path [kgm-2]
! clivi: Total ice water path [kgm-2]
! clgvi: Total graupel water path [kgm-2]
! clhvi: Total hail water path [kgm-2]
! ua: 3D earth-rotated eastward wind [ms-1]
! va: 3D earth-rotated northward wind [ms-1]
! ta: 3D air-temperature [K]
! press: 3D air pressure [Pa]
! zg: 3D geopotential height [m]
! hur: 3D relative humidty [1]
! hus: 3D specific humidty [1]
! uas: 10m earth-rotated eastward wind [ms-1]
! vas: 10m earth-rotated northward wind [ms-1]
! hurs: 2m relative humidty [1]
! huss: 2m specific humidty [1]
! psl: sea level pressure [Pa]
! cape: Convective Available Potential Energy [Jkg-1]
! cin: Convective inhibition [Jkg-1]
! zlfc: Height at the Level of free convection [m]
! plfc: Pressure at the Level of free convection [Pa]
! lidx: Lifted index [1]
! mrsol: moisture content of soil layer [kgm-2]
! mrso: total soil moisture content [kgm-2]
! mrsos: first layer soil moisture content (0-10 cm) [kgm-2]
! mrsll: liquid water content of soil layer [kgm-2]
! mrlso: total liquid water content of soil layer [kgm-2]
! mrlsos: liquid water content in Upper Portion of soil column (0-10cm) [kgm-2]
! mrsfl: frozen water content of soil layer [kgm-2]
! mrfso: total frozen water content of soil layer [kgm-2]
! mrfsos: frozen water content in Upper Portion of soil column (0-10cm) [kgm-2]
! mrsolmean: mean moisture content of soil layer [kgm-2]
! mrsosmean: mean first layer soil moisture content (0-10 cm) [kgm-2]
! mrsomean: mean total soil moisture content [kgm-2]
! mrsllmean: mean liquid water content of soil layer [kgm-2]
! mrlsomean: mean total liquid water content of soil layer [kgm-2]
! mrlsosmean: mean liquid water content in Upper Portion of soil column (0-10cm) [kgm-2]
! mrsflmean: mean frozen water content of soil layer [kgm-2]
! mrfsomean: mean total frozen water content of soil layer [kgm-2]
! mrfsosmean: mean frozen water content in Upper Portion of soil column (0-10cm) [kgm-2]
! wbacpw, wbacpw[c/r/s/i/g/h]: Water-budget vertically integrated accumulated total tendency for water 
!   vapour, cloud, rain, snow, ice, graupel, hail [mm]
! wbacf, wbacf[c/r/s/i/g/h]: Water-budget vertically integrated accumulated horizontal advection for 
!   water vapour, cloud, rain, snow, ice, graupel, hail [mm]
! wbacz, wbacz[c/r/s/i/g/h]: Water-budget vertically integrated accumulated vertical advection for 
!   water vapour, cloud, rain, snow, ice, graupel, hail [mm]
! wbacpw{l/m/h}, wbacpw[c/r/s/i/g/h]{l/m/h}: Water-budget vertically integrated accumulated total tendency for water 
!   vapour, cloud, rain, snow, ice, graupel, hail at low, medium and high levels (same as cloudiness) [mm]
! wbacf{l/m/h}, wbacf[c/r/s/i/g/h]{l/m/h}: Water-budget vertically integrated accumulated horizontal advection for 
!   water vapour, cloud, rain, snow, ice, graupel, hail at low, medium and high levels (same as cloudiness) [mm]
! wbacz{l/m/h}, wbacz[c/r/s/i/g/h]{l/m/h}: Water-budget vertically integrated accumulated vertical advection for 
!   water vapour, cloud, rain, snow, ice, graupel, hail at low, medium and high levels (same as cloudiness) [mm]
! acq[v/c/i]blten: accumulated tendency due to boundary layer scheme for water vapour, cloud and ice [mm]
! acq[v/c/i/r/s]cuten: accumulated tendency due to cumulus scheme for water vapour, cloud, ice, rain and snow [mm]
! wsgsmax: maximum surface wind gust [ms-1]
! [u/v]gsmax: x/y maximum surface gust wind direction [ms-1]
! precengust: percenteage of times where grid point got wind gust
! totwsgsmax: total (TKE + heavy precip. methods )maximum surface wind gust [ms-1]
! tot[u/v]gsmax: total x/y maximum surface gust wind direction [ms-1]
! totprecengust: percenteage of times where grid point got total wind gust
! wsz100max: maximum 100m wind [ms-1]
! uz100max: x maximum 100m wind direction [ms-1]
! vz100max: y maximum 100m wind direction [ms-1]
! sunshine: length of sunshine [s]
! tauu, tauuv: components of the downward wind stress at 10 m [m2s-2]
! tauugen, tauuvgen: generic components of the downward wind stress at 10 m [m2s-2]
! cdcdx: drag coefficient [-]
! cdgen: generic drag coefficient [-]
! rsds: mean surface Downwelling Shortwave Radiation [Wm-2]
! rlds: mean surface Downwelling Longwave Radiation [Wm-2]
! hfls: mean surface Upward Latent Heat Flux [Wm-2]
! hfss: mean surface Upward Sensible Heat Flux [Wm-2]
! rsus: mean surface Upwelling Shortwave Radiation [Wm-2]
! rlus: mean surface Upwelling Longwave Radiation [Wm-2]
! rsusgen: mean generic surface Upwelling Shortwave Radiation [Wm-2]
! rlusgen: mean generic surface Upwelling Longwave Radiation [Wm-2]
! evspsbl: mean evaporation [kgm-2s-1]
! evspsblpot: mean potential evapotranspiration [kgm-2s-1]
! evspsblpotgen: mean generic potential evapotranspiration [kgm-2s-1]
! snc: mean snow area fraction [%]
! snd: mean snow depth [m]
! pmrros: previous accumulated surface Runoff [kgm-2]
! pmrro: previous accumulated mean total  Runoff [kgm-2]
! mrros: mean surface Runoff [kgm-2s-1]
! mrro: mean total  Runoff [kgm-2s-1]
! flxpr: precipitation flux [kgm-2s-1]
! flxprl: large scale precipitation flux [kgm-2s-1]
! flxprc: convective precipitation flux [kgm-2s-1]
! flxprsh: shallow-cumulus precipitation flux [kgm-2s-1]
! flxprsn: solid precipitation flux [kgm-2s-1]
! snw: accumulated snow  [ksm-2]
! rsdt: TOA incident shortwave radiation [kgm-2]
! rsut: TOA outgoing shortwave radiation [kgm-2]
! rlut: TOA outgoing Longwave radiation [kgm-2]
! capemin: minimum CAPE [Jkg-1]
! cinmin: minimum CIN [Jkg-1]
! zlfcmin: minimum height at LFC [m]
! plfcmin: minimum Pressure at LFC [Pa]
! lidxmin: minimum Lifted index [1]
! capemax: maximum CAPE [Jkg-1]
! cinmax: maximum CIN [Jkg-1]
! zlfcmax: maximum height at LFC [m]
! plfcmax: maximum Pressure at LFC [Pa]
! lidxmax: maximum Lifted index [1]
! capemean: mean CAPE [Jkg-1]
! cinmean: mean CIN [Jkg-1]
! zlfcmean: mean height at LFC [m]
! plfcmean: mean Pressure at LFC [Pa]
! lidxmean: mean Lifted index [1]
! fog: whether point is inside fog (vis < 1km) [1]
! visblty: visibility inside fog [km]
! tfog: time of presence of fog [s]
! fogvisbltymin: minimun visibility inside fog [km]
! fogvisbltymax: maximun visibility inside fog [km]
! fogvisbltymean: mean visibility inside fog [km]
! tdsmin: minimum 2m dew point temperature [K]
! tdsmax: maximum 2m dew point temperature [K]
! tdsmean: mean 2m dew point temperature [K]
! twsmin: minimum 2m wet-bulb temperature [K]
! twsmax: maximum 2m wet-bulb temperature [K]
! twsmean: mean 2m wet-bulb temperature [K]
! tashurstreshighres: high-resolution of simultaneous temporal residence of 2-m temperature and 
!   relative humidity
! tashurstreslowres: low-resolution of simultaneous temporal residence of 2-m temperature and 
!   relative humidity
! wbdswsstres: simultaneous temporal residence of 10-m wind direction (from where it blows) and 
!   wind speed

!!!!!!! Structure
! 1.: Compute such variables needed for internal accumulations, extremes
! 2.: Compute the variables from clouds (only actualized at every radt)
! 3.: If the time step is the first after writting the auxhist9 file inittialize the variables of (1., 2.)
! 4.: If the time step is exactly the previous one before the writting of the auxhist9 file:
!   a: Compute all other variables 
!   b: Finish calculation of 1., 2. variables 

!======================================================================

  INTEGER,      INTENT(in)                               ::                                           &
                                            ids,ide, jds,jde, kds,kde,                                &
                                            ims,ime, jms,jme, kms,kme,                                &
                                            ips,ipe, jps,jpe, kps,kpe,                                &
                                            kts,kte,                                                  &
                                            num_tiles,                                                &
                                            num_soil_layers, n_moist


  INTEGER, DIMENSION(num_tiles), INTENT(in)              :: i_start, i_end, j_start, j_end
  INTEGER, INTENT(in)                                    :: m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh

  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: cldfra, p8w, p, pb, qv, qc, qr, qs, qi,   &
    qg, qh, t, thp, ph, phb, p_phy, u, v, z, rho
!#ifdef CDXWRF
!#if CDXWRF>=1
!#endif

!#if CDXWRF>=4
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: diabh
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: qvttend, qcttend, qrttend, qsttend,       &
    qittend, qgttend, qhttend
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: qvhadv, qchadv, qrhadv, qshadv, qihadv,   &
    qghadv, qhhadv
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: qvzadv, qczadv, qrzadv, qszadv, qizadv,   &
    qgzadv, qhzadv
!#endif
!#if CDXWRF>=3
  INTEGER, INTENT(in)                                    :: Nhtasrng, Nhhursrng, Nltasrng, Nlhursrng, &
    Nwbdsrng, Nwssrng
  INTEGER, INTENT(in)                                    :: Nhtashursrng, Nltashursrng, Nwbdswssrng
  REAL, DIMENSION(Nhtasrng), INTENT(in)                  :: tashrng
  REAL, DIMENSION(Nhhursrng), INTENT(in)                 :: hurshrng
  REAL, DIMENSION(Nltasrng), INTENT(in)                  :: taslrng
  REAL, DIMENSION(Nlhursrng), INTENT(in)                 :: hurslrng
  REAL, DIMENSION(Nwbdsrng), INTENT(in)                  :: wbdsrng
  REAL, DIMENSION(Nwssrng), INTENT(in)                   :: wssrng
!#endif
!#endif
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: lon, lat
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: mu, mub, sina, cosa
  REAL, DIMENSION(kms:kme), INTENT(in)                   :: deta
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: u10, v10, q2, t2, th2, ps, qsfc
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: hgt
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: tke
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: pblh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: prc, prnc, prsh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: ust, znt, rol
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: swdown
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: Cd
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: glw, lh, hfx, swupb, lwupb, qfx
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: snow, snowh, snowc
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: sfcrunoff, udrunoff
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: swdnt, swupt, lwupt
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: ts, albedo, emiss
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: sr
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: chs, cdm, br, zol

  REAL, INTENT(in)                                       :: DT, XTIME
  REAL, INTENT(in)                                       :: curr_secs2, radt
  INTEGER, INTENT(in)                                    :: cdxH
  LOGICAL, INTENT(in)                                    :: is_restart, is_output_time
  REAL, INTENT(in)                                       :: ptarget
  INTEGER, INTENT(in)                                    :: psl_diag, psmooth
  INTEGER, INTENT(in)                                    :: wsgs_diag
  INTEGER, INTENT(in)                                    :: wbout
  INTEGER, INTENT(in)                                    :: wsz100_diag
  REAL, INTENT(in)                                       :: zhgtzw
  REAL, INTENT(in)                                       :: z50_diag  
  INTEGER, INTENT(in)                                    :: potevap_diag
  REAL, INTENT(in)                                       :: dqv, dtheta
  INTEGER, INTENT(in)                                    :: convxtrm_diag
  INTEGER, INTENT(in)                                    :: visibility_diag, fogvars, zmlagen_diag
  REAL, DIMENSION(1:num_soil_layers), INTENT(in)         :: dzs, fracsoil10cm
  INTEGER, INTENT(in)                                    :: timedbg, wgsts_wbdswsstres
  REAL, INTENT(in)                                       :: londbg, latdbg
  INTEGER, INTENT(in)                                    :: idbg, jdbg

  INTEGER, INTENT(inout)                                 :: nsteps
  REAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme),                                                 & 
    INTENT(in)                                           :: smois, sh2o
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: pclt, pcll, pclm, pclh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: cltmean, cllmean, clmmean, clhmean
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: prw, clwvi, clivi
!#ifdef CDXWRF
!!#if CDXWRF>=1
!!  REAL, DIMENSION(ims:ime,kms:kme,jms:jme)               :: ta, press, zg, hur
!!#endif
!#if CDXWRF>=1
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: zmla
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: clgvi, clhvi
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: capemin, cinmin, zlfcmin, plfcmin
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: capemax, cinmax, zlfcmax, plfcmax
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: capemean, cinmean, zlfcmean, plfcmean,    &
    lidxmin, lidxmax, lidxmean
  REAL, DIMENSION(ims:ime,jms:jme)                       :: iutmean, ivtmean
  REAL, DIMENSION(ims:ime,jms:jme)                       :: zeroith
!#endif
!#if CDXWRF>=2
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(out)  :: ua, va, ta, press, zg, hur, ws, hus
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: tfog, fogvisbltymin, fogvisbltymax,       &
    fogvisbltymean
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: tdsmin, tdsmax, tdsmean
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: twsmin, twsmax, twsmean
!#endif
!#if CDXWRF>=4
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacdh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacpw, wbacpwc, wbacpwr, wbacpws,        &
    wbacpwi, wbacpwg, wbacpwh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacf, wbacfc, wbacfr, wbacfs, wbacfi,    &
    wbacfg, wbacfh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacz, wbaczc, wbaczr, wbaczs, wbaczi,    &
    wbaczg, wbaczh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacdhl, wbacdhm, wbacdhh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacpwlv, wbacpwmv, wbacpwhv,             &
    wbacpwlc, wbacpwmc, wbacpwhc, wbacpwlr, wbacpwmr, wbacpwhr, wbacpwls, wbacpwms, wbacpwhs,         &
    wbacpwli, wbacpwmi, wbacpwhi, wbacpwlg, wbacpwmg, wbacpwhg, wbacpwlh, wbacpwmh, wbacpwhh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbacflv, wbacfmv, wbacfhv,                &
    wbacflc, wbacfmc, wbacfhc, wbacflr, wbacfmr, wbacfhr, wbacfls, wbacfms, wbacfhs,                  &
    wbacfli, wbacfmi, wbacfhi, wbacflg, wbacfmg, wbacfhg, wbacflh, wbacfmh, wbacfhh
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wbaczlv, wbaczmv, wbaczhv,                &
    wbaczlc, wbaczmc, wbaczhc, wbaczlr, wbaczmr, wbaczhr, wbaczls, wbaczms, wbaczhs,                  &
    wbaczli, wbaczmi, wbaczhi, wbaczlg, wbaczmg, wbaczhg, wbaczlh, wbaczmh, wbaczhh
!#endif
!#if CDXWRF>=3
!!  REAL, DIMENSION(ims:ime,jms:jme,Nhtasrng,Nhhursrng),                                                &
!!    INTENT(inout)                                        :: tashurstreshighres
!!  REAL, DIMENSION(ims:ime,jms:jme,Nltasrng,Nlhursrng),                                                &
!!    INTENT(inout)                                        :: tashurstreslowres
  REAL, DIMENSION(ims:ime,Nhtashursrng,jms:jme),                                                      &
    INTENT(inout)                                        :: tashurstreshighres
  REAL, DIMENSION(ims:ime,Nltashursrng,jms:jme),                                                      &
    INTENT(inout)                                        :: tashurstreslowres
  REAL, DIMENSION(ims:ime,Nwbdswssrng,jms:jme),                                                       &
    INTENT(inout)                                        :: wbdswsstres
!#endif
!#else
!  REAL, DIMENSION(ims:ime,kms:kme,jms:jme)               :: ta, press, zg
!#endif
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: uas, vas, wss, wbds, hurs, huss
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: psl
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wsgsmax, ugsmax, vgsmax, percengust
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: totwsgsmax, totugsmax, totvgsmax,         &
    totpercengust
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: wsz100, uz100wind, vz100wind
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: wsz50, uz50wind, vz50wind
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: taz50, qvz50
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: wsz100max, uz100max, vz100max
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: sunshine
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: cdcdx, tauu, tauv, tauugen, tauvgen
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: rsds, rlds, hfls, hfss, rsus, rlus,       &
    rsusgen, rlusgen, evspsbl
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: cdgen, evspsblpot, evspsblpotgen
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: snc, snd
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: pmrros, pmrro
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: mrros, mrro, mrsos, mrso, mrlso, mrlsos,  &
    mrfso, mrfsos
  REAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme),                                                 &
    INTENT(out)                                          :: mrsol, mrsll, mrsfl
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: mrsosmean, mrsomean, mrlsomean,           &
    mrlsosmean, mrfsomean, mrfsosmean
  REAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme),                                                 &
    INTENT(inout)                                        :: mrsolmean, mrsllmean, mrsflmean
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: flxpr, flxprl, flxprc, flxprsh, flxprsn,  &
    snw
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: rsdt, rsut, rlut
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: tas_hm, qvs_hm

!!! INSTVALS: Instantaneous values (uncomment and recompile)
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: clt, cll, clm, clh
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: wsgs, ugustwind, vgustwind
!!  INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(out)       :: gustpoint
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: totwsgs, totugustwind, totvgustwind
!!  INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(out)       :: totgustpoint
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: potevap, potevapgen
!#ifdef CDXWRF
!#if CDXWRF>=1
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: cape, cin, zlfc, plfc, lidx
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: iut, ivt
!#endif
!#if CDXWRF>=2
!!  INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(out)       :: fog
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: vis
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: tds
!!  REAL, DIMENSION(ims:ime,jms:jme), INTENT(out)          :: tws
!#endif
!#endif
!! INSTVALS: comment these lines
  REAL, DIMENSION(ims:ime,jms:jme)                       :: clt, cll, clm, clh
  REAL, DIMENSION(ims:ime,jms:jme)                       :: wsgs, ugustwind, vgustwind
  INTEGER, DIMENSION(ims:ime,jms:jme)                    :: gustpoint
  REAL, DIMENSION(ims:ime,jms:jme)                       :: totwsgs, totugustwind, totvgustwind
  INTEGER, DIMENSION(ims:ime,jms:jme)                    :: totgustpoint
  REAL, DIMENSION(ims:ime,jms:jme)                       :: wsgsb01, ugustwindb01, vgustwindb01
  REAL, DIMENSION(ims:ime,jms:jme)                       :: wsgshp, ugustwindhp, vgustwindhp
  INTEGER, DIMENSION(ims:ime,jms:jme)                    :: gustpointb01, gustpointhp
  REAL, DIMENSION(ims:ime,jms:jme)                       :: potevap, potevapgen
!#ifdef CDXWRF
!#if CDXWRF>=1
  REAL, DIMENSION(ims:ime,jms:jme)                       :: cape, cin, zlfc, plfc, lidx
  REAL, DIMENSION(ims:ime,jms:jme)                       :: iut, ivt  
!#endif
!#if CDXWRF>=2
  INTEGER, DIMENSION(ims:ime,jms:jme)                    :: fog
  REAL, DIMENSION(ims:ime,jms:jme)                       :: vis
  REAL, DIMENSION(ims:ime,jms:jme)                       :: tds
  REAL, DIMENSION(ims:ime,jms:jme)                       :: tws
!#endif
!#if CDXWRF>=3
  INTEGER                                                :: itas, ihurs, iwbds, iwss
!#endif
!#if CDXWRF>=4
!#endif
!#endif
!!! End of INSTVALS

! LOCAL  VAR

  INTEGER                                                :: i, j, k, m, its, ite, jts, jte, ij
  !INTEGER                                                :: i_s, i_e, j_s, j_e
  INTEGER                                                :: idp, jdp, dimz, dimz1, dz1
  INTEGER                                                :: i1, i_1, j1, j_1, k2, tdbg
  INTEGER, DIMENSION(num_tiles)                          :: i2, j2

  REAL                                                   :: xtimep
  REAL                                                   :: hus_kts, zg_kts, huss_hm 
  REAL                                                   :: taz2, qcs
  REAL, DIMENSION(3)                                     :: cllmh, varlmh
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme)               :: unu, unv, th, geopoth
  REAL, DIMENSION(ims:ime,jms:jme)                       :: mutot
  REAL, DIMENSION(kms:kme)                               :: qvar, zagl, ri, qsolid
  REAL, DIMENSION(ims:ime,kms:kme+1,jms:jme)             :: stzg
  REAL, DIMENSION(ims:ime,jms:jme)                       :: smoothp
!#ifdef CDXWRF
!#if CDXWRF>=1

!#endif
!#if CDXWRF>=3
  REAL                                                   :: uuas, vvas
  REAL, DIMENSION(ims:ime,jms:jme)                       :: wbdsv, wssv
!#endif
!#if CDXWRF>=4
  REAL, DIMENSION(kms:kme,n_moist)                       :: qvarttend, qvarhadv, qvarzadv
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme,n_moist)       :: pwqvar, fqvar, zqvar
  REAL, DIMENSION(n_moist)                               :: acpwqvar, acfqvar, aczqvar
  REAL, DIMENSION(n_moist)                               :: acpwlqvar, acpwmqvar, acpwhqvar
  REAL, DIMENSION(n_moist)                               :: acflqvar, acfmqvar, acfhqvar
  REAL, DIMENSION(n_moist)                               :: aczlqvar, aczmqvar, aczhqvar
!#endif
!#endif
  INTEGER                                                :: ostat
  REAL, DIMENSION(kms:kme)                               :: zsfc
  REAL                                                   :: prv

  LOGICAL, EXTERNAL                                      :: wrf_dm_on_monitor
  CHARACTER(len=256)                                     :: msg
  LOGICAL                                                :: ijdbg, cdone
  INTEGER                                                :: dbg_level

  fname = 'cordex_output_calc'

  ! Showing version of the module
  IF (curr_secs2 == 0. ) THEN
    WRITE(msg,*) '  CORDEX-WRF module version: ' // TRIM(cdxwrfversion) // ' ...'
    PRINT *, TRIM(msg)
    CALL wrf_debug(0,msg)
  END IF

  ! timedbg
  !  when time-debug in minutes
  tdbg = MOD(xtime, timedbg*1.)

  ! Getting the overall debug level
  CALL get_wrf_debug_level( dbg_level ) 
  cdone = .FALSE.

  ! L. Fita: NOTE about efficiency
  !   I am not sure if this is the more efficient way:
  !     1.- IF --> 2.- repeat DO
  !     1.- DO -->  2.- IF

  dimz = kme - kms + 1
  dimz1 = dimz + 1
  dz1 = dimz - 1
  ! Using p8w
  !press = p8w
  mutot = mu + mub
  th = thp + 300.

  ! For test purposes
  IF ((londbg == -999.) .OR. (latdbg == -999.)) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      i2(ij) = INT(0.5*(i_start(ij) + i_end(ij)))
      j2(ij) = INT(0.5*(j_start(ij) + j_end(ij)))
    END DO
    !$OMP END PARALLEL DO
  ELSE
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      i2(ij) = idbg
      j2(ij) = jdbg
    END DO
    !$OMP END PARALLEL DO
  END IF
  k2 = 0.5*(kms+kme)

  ! Computing accumulative variables 
  ijdbg = .FALSE.
  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij )
  DO ij = 1 , num_tiles
    DO i = i_start(ij), i_end(ij)
      DO j = j_start(ij), j_end(ij)
        IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.
        ! Unstaggering certain variables
        unu(i,:,j) = 0.5*(u(i,:,j)+u(i+1,:,j))
        unv(i,:,j) = 0.5*(v(i,:,j)+v(i,:,j+1))

        ! air-pressure
        CALL var_press(p(i,:,j), pb(i,:,j), dimz, press(i,:,j))

        ! geopotential height
        CALL var_zg(ph(i,:,j), phb(i,:,j), dimz1, dimz, stzg(i,:,j), zg(i,:,j))
        ! geopotential height as [m2s-2]
        geopoth(i,:,j) = zg(i,:,j)*g

        ! air-temperature
        CALL var_ta(thp(i,:,j), press(i,:,j), dimz, ta(i,:,j))

        ! 2m relative humidity
        CALL var_hurs(t2(i,j), ps(i,j), q2(i,j), hurs(i,j))

        ! 10m Earth rotated winds
        CALL var_uasvas(u10(i,j), v10(i,j), sina(i,j), cosa(i,j), uas(i,j), vas(i,j))

        ! Not using var_vertint, since we have a full loop in soil_layers
        !! mrso
        !WRITE(msg,*) '  ' // TRIM(fname) // ': computing MRSO diagnostic'
        !IF (ijdbg) CALL wrf_debug(50,msg)
        !CALL var_vertint(smois(i,:,j), num_soil_layers, dzs(:), ABS(dzs(:)/dzs(:)), mrso(i,j))

        !! slw
        !WRITE(msg,*) '  ' // TRIM(fname) // ': computing SLW diagnostic'
        !IF (ijdbg) CALL wrf_debug(50,msg)
        !CALL var_vertint(sh2o(i,:,j), num_soil_layers, dzs(:), ABS(dzs(:)/dzs(:)), slw(i,j))

        ! Land water
        ! First layer soil
        ! FROM: https://wcrp-cordex.github.io/cordex-cmip6-data-request/)
        mrsos(i,j) = 0.
        mrso(i,j) = 0.
        mrlso(i,j) = 0.
        mrlsos(i,j) = 0.
        mrfso(i,j) = 0.
        mrfsos(i,j) = 0.
        DO k=1, num_soil_layers
          mrsol(i,k,j) = smois(i,k,j)*dzs(k)*1000.
          mrsos(i,j) = mrsos(i,j) + mrsol(i,k,j)*fracsoil10cm(k)
          mrso(i,j) = mrso(i,j) + mrsol(i,k,j)
          mrsll(i,k,j) = sh2o(i,k,j)*dzs(k)*1000.
          mrlso(i,j) = mrlso(i,j) + mrsll(i,k,j)
          mrlsos(i,j) = mrlsos(i,j) + mrsll(i,k,j)*fracsoil10cm(k)
          mrsfl(i,k,j) = (smois(i,k,j) - sh2o(i,k,j))*dzs(k)*1000.
          mrfso(i,j) = mrfso(i,j) + mrsfl(i,k,j)
          mrfsos(i,j) = mrfsos(i,j) + mrsfl(i,k,j)*fracsoil10cm(k)
        END DO
        
        ! Wind-gust

        ! Avoiding seg faults
        IF (curr_secs2 > 0. ) THEN
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing Wind-gust'

          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)
          ! Brasseur methodology 
          CALL gustwind_Brasseur01(dimz, geopoth(i,:,j), qv(i,:,j), qc(i,:,j), th(i,:,j), tke(i,:,j), &
            unu(i,:,j), unv(i,:,j), pblh(i,j), hgt(i,j), sina(i,j), cosa(i,j), ugustwind(i,j),        &
            vgustwind(i,j), gustpoint(i,j))
          ugustwindb01(i,j) = ugustwind(i,j)
          vgustwindb01(i,j) = vgustwind(i,j)
          wsgsb01(i,j) = SQRT(ugustwind(i,j)*ugustwind(i,j) + vgustwind(i,j)*vgustwind(i,j))
          gustpointb01(i,j) = gustpoint(i,j)
          ! Height above ground
          zagl = z(i,:,j)-hgt(i,j)
          ! Total precipitation
          prv = prc(i,j) + prnc(i,j) + prsh(i,j)
          IF (ijdbg .AND. tdbg==0) THEN
            WRITE(msg,*)'      gustwind_afwa values zagl k=1:', zagl(1), ' prv:', prv,                &
              ' prate_mm_per_hr:', (prv/dt)*3600., ' ua 1:', unu(i,1,j), ' va 1:', unv(i,1,j)
            CALL wrf_debug(100,msg)
          END IF
          CALL gustwind_afwa(dimz, dt, u10(i,j), v10(i,j), prv, zagl(:), unu(i,:,j), unv(i,:,j),      &
            sina(i,j), cosa(i,j), ugustwind(i,j), vgustwind(i,j), gustpoint(i,j))
          ugustwindhp(i,j) = ugustwind(i,j)
          vgustwindhp(i,j) = vgustwind(i,j)
          wsgshp(i,j) = SQRT(ugustwind(i,j)*ugustwind(i,j) + vgustwind(i,j)*vgustwind(i,j))
          gustpointhp(i,j) = gustpoint(i,j)

          ! L. Fita, CIMA. 17 Feburary 2018
          ! Brasseur (TKE) method accounts for the mechanically-induced wind gusts. 
          ! AFWA (heavy precipitation) accounts for the heavy convectivelly-induced wind gusts
          ! For the total wind-gustiness, might be necessary the sum of both methodologies
          ! Total wind-gust
          totugustwind(i,j) = ugustwindb01(i,j) + ugustwindhp(i,j)
          totvgustwind(i,j) = vgustwindb01(i,j) + vgustwindhp(i,j)
          totwsgs(i,j) = wsgsb01(i,j) + wsgshp(i,j)
          totgustpoint(i,j) = INT( MAXVAL( (/ gustpointb01(i,j)*1.,gustpointhp(i,j)*1. /) ) )

          wsgsdiag: SELECT CASE (wsgs_diag)
            CASE (dwsgsbrasseur01)
              WRITE(msg,*) '  ' // TRIM(fname) // ': wsgs diagnostic= ', wsgs_diag, ' Brasseur, 2001'// &
                  ' method'
              ugustwind(i,j) = ugustwindb01(i,j)
              vgustwind(i,j) = vgustwindb01(i,j)
              wsgs(i,j) = wsgsb01(i,j)
              gustpoint(i,j) = gustpointb01(i,j)

            CASE (dwsgsheavyprecip)
              WRITE(msg,*) '  ' // TRIM(fname) // ': wsgs diagnostic= ', wsgs_diag, 'AFWA method'
              ugustwind(i,j) = ugustwindhp(i,j)
              vgustwind(i,j) = vgustwindhp(i,j)
              wsgs(i,j) = wsgshp(i,j)
              gustpoint(i,j) = gustpointhp(i,j)

            CASE DEFAULT
              WRITE (msg,*) '  ' // TRIM(fname) // ": ERROR -- error -- ERROR -- error : type " //      &
                 "of wind gust diagnostic 'wsgs_diag=", wsgs_diag, "' not ready !! " // CHAR(10) //     &
                 "      available ones; ", dwsgsbrasseur01, " : Brasseur, 01", dwsgsheavyprecip,        &
                 ' : heavy precipitation'
              CALL wrf_error_fatal(msg)
          END SELECT wsgsdiag
          IF (ijdbg) CALL wrf_debug(50,msg)
        ELSE
          ugustwind(i,j) = 0.
          vgustwind(i,j) = 0.
          wsgs(i,j) = 0.
          gustpoint(i,j) = 0
          totugustwind(i,j) = 0.
          totvgustwind(i,j) = 0.
          totwsgs(i,j) = 0.
          totgustpoint(i,j) = 0
          ugustwindb01(i,j) = 0.
          vgustwindb01(i,j) = 0.
          wsgsb01(i,j) = 0.
          gustpointb01(i,j) = 0
          ugustwindhp(i,j) = 0.
          vgustwindhp(i,j) = 0.
          wsgshp(i,j) = 0.
          gustpointhp(i,j) = 0
        END IF

        ! z-wind
        ! Avoiding seg faults
        IF (curr_secs2 > 0. ) THEN
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing z-wind at:', zhgtzw, ' m'
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)

          ! Height above surface at half-mass levels
          zsfc = zg(i,:,j)-hgt(i,j)
          wsz100diag: SELECT CASE (wsz100_diag)
            CASE (dwsz100powerlaw)
              WRITE(msg,*) '  ' // TRIM(fname) // ': wsz100 diagnostic= ', wsz100_diag, ' power-law'
              CALL var_zwind(dimz, unu(i,:,j), unv(i,:,j), zsfc, uas(i,j), vas(i,j), sina(i,j),       &
                cosa(i,j), zhgtzw, uz100wind(i,j), vz100wind(i,j))
            CASE (dwsz100loglaw)
              WRITE(msg,*) '  ' // TRIM(fname) // ': wsz100 diagnostic= ', wsz100_diag, ' logarithmic-law'
              CALL var_zwind_log(dimz, unu(i,:,j), unv(i,:,j), zsfc, uas(i,j), vas(i,j), sina(i,j),   &
                cosa(i,j), zhgtzw, uz100wind(i,j), vz100wind(i,j))
            CASE (dwsz100MO)
              WRITE(msg,*) '  ' // TRIM(fname) // ': wsz100 diagnostic= ', wsz100_diag,               &
               ' Monin-Obukhov theory'
              CALL var_zwind_MOtheor(dimz, unu(i,:,j), unv(i,:,j), zg(i,:,j), hgt(i,j), ust(i,j),     &
                znt(i,j), rol(i,j), u10(i,j), v10(i,j), sina(i,j), cosa(i,j), zhgtzw, uz100wind(i,j), &
                vz100wind(i,j))
            CASE DEFAULT
              WRITE (msg,*) '  ' // TRIM(fname) // ": ERROR -- error -- ERROR -- error : type " //    &
                 "of z-wind diagnostic 'wsz100_diag=", wsz100_diag, "' not ready !! " // CHAR(10) //  &
                 "      available ones; ", dwsz100powerlaw, " : power-law", dwsz100loglaw, 'log-law', &
                 dwsz100MO, " : Monin-Obukhov"
              CALL wrf_error_fatal(msg)
          END SELECT wsz100diag
          wsz100(i,j) = SQRT(uz100wind(i,j)*uz100wind(i,j) + vz100wind(i,j)*vz100wind(i,j))

          ! 50 m
          CALL var_zwind_MOtheor(dimz, unu(i,:,j), unv(i,:,j), zg(i,:,j), hgt(i,j), ust(i,j),         &
            znt(i,j), rol(i,j), u10(i,j), v10(i,j), sina(i,j), cosa(i,j), z50_diag, uz50wind(i,j),    &
            vz50wind(i,j))
          wsz50(i,j) = SQRT(uz50wind(i,j)*uz50wind(i,j) + vz50wind(i,j)*vz50wind(i,j))

          CALL var_ztaqv_MOtheor(dimz, t(i,:,j), qv(i,:,j), zg(i,:,j), ust(i,j), znt(i,j), rol(i,j),  &
            hgt(i,j), z50_diag, taz50(i,j), qvz50(i,j))

        ELSE
          uz100wind(i,j) = 0.
          vz100wind(i,j) = 0.
          wsz100(i,j) = 0.
          uz50wind(i,j) = 0.
          vz50wind(i,j) = 0.
          wsz50(i,j) = 0.
          taz50(i,j) = 0.
          qvz50(i,j) = 0.
        END IF

        ! potential evapotranspiration
        ! Avoiding seg faults
        IF (curr_secs2 > 0. ) THEN
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing z-wind at:', zhgtzw, ' m'
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)

          ! Generic first-order approximation drag coefficient
          CALL Cdrag_0(ust(i,j), uas(i,j), vas(i,j), cdgen(i,j))

          ! potential evap
          potevapdiag: SELECT CASE (potevap_diag)
            CASE (dpotevaporbulk)
              WRITE(msg,*) '  ' // TRIM(fname) // ': potevap diagnostic= ', potevap_diag,             &
                ' bulk'
              CALL var_potevap_bulk(rho(i,1,j), cd(i,j), uas(i,j), vas(i,j), ts(i,j), ps(i,j),        &
                qv(i,1,j), potevap(i,j))
              CALL var_potevap_bulkgen(rho(i,1,j), ust(i,j), uas(i,j), vas(i,j), ts(i,j), ps(i,j),    &
                qv(i,1,j), potevapgen(i,j))
            CASE (dpotevaporMilly92)
              WRITE(msg,*) '  ' // TRIM(fname) // ': potevap diagnostic= ', potevap_diag,             &
                ' Milly92'
              CALL var_potevap_Milly92(rho(i,1,j), cd(i,j), uas(i,j), vas(i,j), ts(i,j), t(i,1,j),    &
                ps(i,j), qv(i,1,j), qfx(i,j), emiss(i,j), potevap(i,j))
              CALL var_potevap_Milly92gen(rho(i,1,j), ust(i,j), uas(i,j), vas(i,j), ts(i,j),t(i,1,j), &
                ps(i,j), qv(i,1,j), qfx(i,j), emiss(i,j), potevapgen(i,j))
            CASE DEFAULT
              WRITE (msg,*) '  ' // TRIM(fname) // ": ERROR -- error -- ERROR -- error : type " //    &
                 "of potevap diagnostic 'potevap_diag=", potevap_diag, "' not ready !! " //CHAR(10)// &
                 "      available ones; ", dpotevaporbulk, " : bulk (Manabe, 1969), ",                &
                 dpotevaporMilly92, " : Milly92 (Milly, 1992)"
              CALL wrf_error_fatal(msg)
          END SELECT potevapdiag
        ELSE
          cdgen(i,j) = 0.
          potevap(i,j) = 0.
          potevapgen(i,j) = 0.
        END IF

!#ifdef CDXWRF
!#if CDXWRF>=1
        ! relative humidity
        CALL var_hur(t(i,:,j), press(i,:,j), qv(i,:,j), dimz, hur(i,:,j))
        ! Diagnostics of relative humidity from AFWA's module
        !DO k=kms, kme
        !  hur(i,k,j) = calc_rh ( p_phy(i,k,j), t(i,k,j), qv(i,k,j) ) / 100.
        !END DO

        ! Extremes of convection indices
        IF (convxtrm_diag == 1) THEN
          IF (curr_secs2 > 0. ) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ': computing z-wind at:', zhgtzw, ' m'
            IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)
            ! cape (no need to reach top of atmosphere... dz1)
            WRITE(msg,*) '  ' // TRIM(fname) // ': computing CAPE diagnostic'
            IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)

            CALL var_cape_afwa(dz1, t(i,1:dz1,j), hur(i,1:dz1,j)*100., p_phy(i,1:dz1,j), z(i,1:dz1,j),&
              1, cape(i,j), cin(i,j), zlfc(i,j), plfc(i,j), lidx(i,j), 3)
            ! Giving it as height above ground
            IF (zlfc(i,j) /= -1.) zlfc(i,j) = zlfc(i,j) - hgt(i,j)
          END IF
        ELSE
          cape(i,j) = 0.
          cin(i,j) = 0.
          zlfc(i,j) = 0.
          plfc(i,j) = 0.
          lidx(i,j) = 0.
        END IF
          
        ! Horizontal transport of humidty
        IF (curr_secs2 > 0. ) THEN
          ! iut: eastward horizontal transport of humidity
          CALL var_massvertint(qv(i,:,j)*unu(i,:,j), mutot(i,j), dimz, deta, iut(i,j))
        
          ! ivt: northward horizontal transport of humidity
          CALL var_massvertint(qv(i,:,j)*unv(i,:,j), mutot(i,j), dimz, deta, ivt(i,j))
        ELSE
          iut(i,j) = 0.
          ivt(i,j) = 0.
        END IF
!#endif

!#if CDXWRF>=2
        ! 2m dew point temperature
        CALL var_tds(hurs(i,j), t2(i,j), tds(i,j))

        ! 2m wet-bulb temperature
        CALL var_tws_S11(t2(i,j), hurs(i,j), tws(i,j))

        ! fog
        ! Avoiding seg faults
        IF (curr_secs2 > 0. ) THEN
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing fog'
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)

          ! fog
          fogdiag: SELECT CASE (visibility_diag)
            CASE (dfogK84)
              WRITE(msg,*) '  ' // TRIM(fname) // ': visibility diagnostic= ', visibility_diag,       &
                ' from Kunkel (1984)'
              IF (fogvars == 2) THEN
                WRITE(msg,*)'diagnostics of visibility with visibility_diag=', visibility_diag,       &
                  ' can only be done with 3D variables (fogvars=1). Not with fogvars=', fogvars
                CALL wrf_error_fatal(msg)
              END IF  
              CALL var_fog_K84(qc(i,1,j), qc(i,1,j), fog(i,j), vis(i,j))
            CASE (dfogRUC)
              WRITE(msg,*) '  ' // TRIM(fname) // ': visibility diagnostic= ', visibility_diag,       &
                ' from RUC method Smirnova, (2000)'
              IF (fogvars == 1) THEN
                CALL var_fog_RUC(hur(i,1,j), fog(i,j), vis(i,j))
              ELSE IF (fogvars == 2) THEN
                CALL var_fog_RUC(hurs(i,j), fog(i,j), vis(i,j))
              ELSE
                WRITE(msg,*)'diagnostics of visibility can only be computed with either 3D ' //       &
                  'variables (fogvar=1) or sfc variables (fogvar=2), provided value=', fogvars
                CALL wrf_error_fatal(msg)
              END IF  
            CASE (dfogFRAML50)
              WRITE(msg,*) '  ' // TRIM(fname) // ': visibility diagnostic= ', visibility_diag,       &
                ' from FRAM-L 50 % probability method from Gultepe and Milbrandt, (2010)'
              IF (fogvars == 1) THEN
                CALL var_fog_FRAML50(hur(i,1,j), fog(i,j), vis(i,j))
              ELSE IF (fogvars == 2) THEN
                CALL var_fog_FRAML50(hurs(i,j), fog(i,j), vis(i,j))
              ELSE
                WRITE(msg,*)'diagnostics of visibility can only be computed with either 3D ' //       &
                  'variables (fogvar=1) or sfc variables (fogvar=2), provided value=', fogvars
                CALL wrf_error_fatal(msg)
              END IF  
            CASE DEFAULT
              WRITE (msg,*) '  ' // TRIM(fname) // ": ERROR -- error -- ERROR -- error : type " //    &
                 "of fog % visibility diagnostic 'visibility_diag=", visibility_diag,                 &
                 "' not ready !! " //CHAR(10)// "      available ones; ", dfogK84, " : Kunkel " //    &
                 "(1984); ", dfogRUC, ": RUC method Smirnova, (2000); ", dfogFRAML50,                 &
                 " : Gultepe and Milbrandt, (2010)"
              CALL wrf_error_fatal(msg)
          END SELECT fogdiag
        ELSE
          fog(i,j) = 0
          vis(i,j) = 0.
        END IF

!#endif
!#if CDXWRF>=3
        ! Computing wind direction from where it blows and speed
        ! Towards it blows
        ! Avoiding seg faults
        IF (curr_secs2 > 0. ) THEN
          wbds(i,j) = ATAN2(-vvas,-uuas)
          IF (wgsts_wbdswsstres == 1) THEN
            ! Should it be totgustwind or gustwind (either Brasseur or heavy precipitation)? 
            uuas = uas(i,j)+totugustwind(i,j)
            vvas = vas(i,j)+totvgustwind(i,j)
          ELSE
            uuas = uas(i,j)+0.
            vvas = vas(i,j)+0.          
          END IF
          ! From where it blows
          wbdsv(i,j) = ATAN2(-vvas,-uuas)
          ! From -pi,pi to 0,2pi
          IF (wbdsv(i,j) < 0.) wbdsv(i,j) = 2*piconst+wbdsv(i,j)
          wssv(i,j) = SQRT(uuas*uuas + vvas*vvas)
        ELSE
          wbdsv(i,j) = 0.
          wssv(i,j) = 0.
        END IF
        
        ! Getting first bin [ -pi/(2(Nwbdsrng-1)), pi/(2(Nwbdsrng-1)) )
        IF (wbdsv(i,j) >= piconst*(2.-1./(2.*(Nwbdsrng-1)))) wbdsv(i,j) = wbdsv(i,j) - 2.*piconst
        
!#endif
!#if CDXWRF>=4
        ! Water budget
        IF (wbout == 1) THEN
          WRITE(msg,*)'    ' // TRIM(fname) // ' Water species: vapor:', m_qv,' cloud:', m_qc,        &
            ' rain:', m_qr, ' snow:', m_qs,' ice:', m_qi, ' graupel:', m_qg,' hail:', m_qh
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(100,msg)

          ! Grouping variables         
          CALL moist_group2D(n_moist, dimz, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,                 &
            qvttend(i,:,j), qcttend(i,:,j), qrttend(i,:,j), qsttend(i,:,j), qittend(i,:,j),           &
            qgttend(i,:,j), qhttend(i,:,j), qvarttend)
          IF (ijdbg .AND. tdbg==0) THEN
            WRITE(message,*)'moist ttend 2: v', qvttend(i,2,j), ' c', qcttend(i,2,j), ' r',           &
              qrttend(i,2,j), ' s', qsttend(i,2,j), ' i', qittend(i,2,j), ' g', qgttend(i,2,j),       &
              ' h', qhttend(i,2,j)
            CALL wrf_debug(100,message)
            WRITE(message,*)'qvarttend:',  qvarttend(2,:)
            CALL wrf_debug(100,message)
          END IF
          CALL moist_group2D(n_moist, dimz, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,                 &
            qvhadv(i,:,j), qchadv(i,:,j), qrhadv(i,:,j), qshadv(i,:,j), qihadv(i,:,j), qghadv(i,:,j), &
            qhhadv(i,:,j), qvarhadv)
          IF (ijdbg .AND. tdbg==0) THEN
            WRITE(message,*)' 2 hadv: v', qvhadv(i,2,j), ' c', qchadv(i,2,j), ' r', qrhadv(i,2,j),    &
              ' s', qshadv(i,2,j), ' i', qihadv(i,2,j), ' g', qghadv(i,2,j), ' h', qhhadv(i,2,j)
            CALL wrf_debug(100,message)
            WRITE(message,*)'qvarhadv:',  qvarhadv(2,:)
            CALL wrf_debug(100,message)
          END IF
          CALL moist_group2D(n_moist, dimz, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,                 &
            qvzadv(i,:,j), qczadv(i,:,j), qrzadv(i,:,j), qszadv(i,:,j), qizadv(i,:,j), qgzadv(i,:,j), &
            qhzadv(i,:,j), qvarzadv)

          CALL water_budget(dimz, n_moist, mutot(i,j), deta, dt, qvarttend, qvarhadv, qvarzadv,       &
            pwqvar(i,:,j,:), fqvar(i,:,j,:), zqvar(i,:,j,:))
        END IF

!#endif
!#endif

        ijdbg = .FALSE.
      END DO
    END DO
  END DO
  !$OMP END PARALLEL DO

  ! cloud fraction is only updated when the radiative scheme is called !
  ijdbg = .FALSE.
  IF (( MOD(NINT((curr_secs2+dt)/dt),NINT(radt*60./dt)) == 0) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.
          ! Total cloudiness
          CALL var_clt(cldfra(i,:,j), dimz, clt(i,j))
          ! low-mid-high cloudiness
          CALL var_cllmh(cldfra(i,:,j), p8w(i,:,j), dimz, cllmh)
          cll(i,j) = cllmh(1)
          clm(i,j) = cllmh(2)
          clh(i,j) = cllmh(3)
          ! L. Fita, Feb. 2018. CIMA. 
          ! I'm not sure if it is necessary, because they might be re-initialized
          ! Let's do it in order to keep the values
          pclt(i,j) = clt(i,j)
          pcll(i,j) = cll(i,j)
          pclm(i,j) = clm(i,j)
          pclh(i,j) = clh(i,j)
        END DO
        ijdbg = .FALSE.
      END DO
    END DO
    !$OMP END PARALLEL DO
    
  ELSE
    clt = pclt
    cll = pcll
    clm = pclm
    clh = pclh
  END IF

  IF (( MOD(NINT(curr_secs2/dt),NINT(cdxH*60./dt)) == 0) ) THEN
    ! Initialization of accumulated variables between outputs
    IF  ( wrf_dm_on_monitor() ) THEN
      CALL wrf_debug(0, '  ' // TRIM(fname) // ': Re-initializing accumulation arrays')
    ENDIF

    nsteps = 1

    ijdbg = .FALSE.
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.
          cltmean(i,j) = clt(i,j)
          cllmean(i,j) = cll(i,j)
          clmmean(i,j) = clm(i,j)
          clhmean(i,j) = clh(i,j)

          ! Gust wind
          wsgsmax(i,j) = wsgs(i,j)
          ugsmax(i,j) = ugustwind(i,j)
          vgsmax(i,j) = vgustwind(i,j)
          percengust(i,j) = gustpoint(i,j)*1.

          totwsgsmax(i,j) = totwsgs(i,j)
          totugsmax(i,j) = totugustwind(i,j)
          totvgsmax(i,j) = totvgustwind(i,j)
          totpercengust(i,j) = totgustpoint(i,j)*1.

          ! 100 m wind
          wsz100max(i,j) = wsz100(i,j)
          uz100max(i,j) = uz100wind(i,j)
          vz100max(i,j) = vz100wind(i,j)

          ! sunshine
          IF (swdown(i,j) > 120. ) THEN
            sunshine(i,j) = dt
          ELSE
            sunshine(i,j) = 0.
          END IF

          ! Fluxes
          rsds(i,j) = swdown(i,j)
          rlds(i,j) = glw(i,j)
          hfls(i,j) = lh(i,j)
          hfss(i,j) = hfx(i,j)
          ! L. Fita, CIMA. March 2018
          !! Is this equivalent to rsus=-SWDOWN*Albedo ?
          rsus(i,j) = swupb(i,j)
          rsusgen(i,j) = -swdown(i,j)*albedo(i,j)
          ! L. Fita, CIMA. March 2018
          !! Is this equivalent to rlus=CtBoltzman*emiss*SKINTEMP**4 ?
          rlus(i,j) = lwupb(i,j)
          rlusgen(i,j) = STBOLT*emiss(i,j)*ts(i,j)**4
          evspsbl(i,j) = qfx(i,j)
          evspsblpot(i,j) = potevap(i,j)
          evspsblpotgen(i,j) = potevapgen(i,j)

          ! Snow
          snc(i,j) = snowc(i,j)*100.
          snd(i,j) = snowh(i,j)

          ! Run-off (already accumulations)
          pmrros(i,j) = sfcrunoff(i,j)
          pmrro(i,j) = sfcrunoff(i,j) + udrunoff(i,j)
          
          ! Precipitations
          flxpr(i,j) = prc(i,j) + prnc(i,j) + prsh(i,j)
          flxprl(i,j) = prnc(i,j)
          flxprc(i,j) = prc(i,j)
          flxprsh(i,j) = prsh(i,j)
          flxprsn(i,j) = flxpr(i,j)*sr(i,j)
          snw(i,j) = snow(i,j)

          ! Fluxes at TOA
          rsdt(i,j) = swdnt(i,j)
          rsut(i,j) = swupt(i,j)
          rlut(i,j) = lwupt(i,j)

          ! Soil
          mrsosmean(i,j) = mrsos(i,j)
          mrsomean(i,j) = mrso(i,j)
          mrlsomean(i,j) = mrlso(i,j)
          mrlsosmean(i,j) = mrlsos(i,j)
          mrfsomean(i,j) = mrfso(i,j)
          mrfsosmean(i,j) = mrfsos(i,j)
          DO k=1, num_soil_layers
            mrsolmean(i,k,j) = mrsol(i,k,j)
            mrsllmean(i,k,j) = mrsll(i,k,j)
            mrsflmean(i,k,j) = mrsfl(i,k,j)
          END DO

!#ifdef CDXWRF
!#if CDXWRF>=1
          ! Extremes of convection indices
          IF (convxtrm_diag == 1) THEN
            capemin(i,j) = cape(i,j)
            cinmin(i,j) = cin(i,j)
            zlfcmin(i,j) = zlfc(i,j)
            plfcmin(i,j) = plfc(i,j)
            lidxmin(i,j) = lidx(i,j)
            capemax(i,j) = cape(i,j)
            cinmax(i,j) = cin(i,j)
            zlfcmax(i,j) = zlfc(i,j)
            plfcmax(i,j) = plfc(i,j)
            lidxmax(i,j) = lidx(i,j)
            capemean(i,j) = cape(i,j)
            cinmean(i,j) = cin(i,j)
            zlfcmean(i,j) = zlfc(i,j)
            plfcmean(i,j) = plfc(i,j)
            lidxmean(i,j) = lidx(i,j)
          END IF
          
          ! Horizontal water transport
          iutmean(i,j) = iut(i,j)
          ivtmean(i,j) = ivt(i,j)

          ! fog
          IF (fog(i,j) == 1) THEN
            tfog(i,j) = dt
            fogvisbltymin(i,j) = vis(i,j)
            fogvisbltymax(i,j) = vis(i,j)
            fogvisbltymean(i,j) = vis(i,j)
          ELSE
            tfog(i,j) = 0.
            fogvisbltymin(i,j) = 0.
            fogvisbltymax(i,j) = 0.
            fogvisbltymean(i,j) = 0.
          END IF

          ! tds
          tdsmin(i,j) = tds(i,j)
          tdsmax(i,j) = tds(i,j)
          tdsmean(i,j) = tds(i,j)

          ! tws
          twsmin(i,j) = tws(i,j)
          twsmax(i,j) = tws(i,j)
          twsmean(i,j) = tws(i,j)
!#endif

!#if CDXWRF>=4
          IF (wbout == 1) THEN
            wbacdh(i,j) = SUM(diabh(i,:,j))*dt
            ! Vertical intervals
            CALL interval_agg(dimz, 2, 3, diabh(i,:,j), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
            wbacdhl(i,j) = varlmh(1)*dt
            wbacdhm(i,j) = varlmh(2)*dt
            wbacdhh(i,j) = varlmh(3)*dt
            DO m=1, n_moist
              acpwqvar(m) = SUM(pwqvar(i,:,j,m))
              acfqvar(m) = SUM(fqvar(i,:,j,m))
              aczqvar(m) = SUM(zqvar(i,:,j,m))

              ! Vertical intervals
              CALL interval_agg(dimz, 2, 3, pwqvar(i,:,j,m), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
              acpwlqvar(m) = varlmh(1)
              acpwmqvar(m) = varlmh(2)
              acpwhqvar(m) = varlmh(3)
              CALL interval_agg(dimz, 2, 3, fqvar(i,:,j,m), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
              acflqvar(m) = varlmh(1)
              acfmqvar(m) = varlmh(2)
              acfhqvar(m) = varlmh(3)
              CALL interval_agg(dimz, 2, 3, zqvar(i,:,j,m), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
              aczlqvar(m) = varlmh(1)
              aczmqvar(m) = varlmh(2)
              aczhqvar(m) = varlmh(3)
            END DO
            CALL moist_redistribute(n_moist, acpwqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbacpw(i,j), wbacpwc(i,j), wbacpwr(i,j), wbacpws(i,j), wbacpwi(i,j), wbacpwg(i,j),      &
              wbacpwh(i,j))
            CALL moist_redistribute(n_moist, acfqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,    &
              wbacf(i,j), wbacfc(i,j), wbacfr(i,j), wbacfs(i,j), wbacfi(i,j), wbacfg(i,j), wbacfh(i,j))
            CALL moist_redistribute(n_moist, aczqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi,m_qg, m_qh,     &
              wbacz(i,j), wbaczc(i,j), wbaczr(i,j), wbaczs(i,j), wbaczi(i,j), wbaczg(i,j), wbaczh(i,j))

            ! Low-medium-heigh
            ! Total tendency
            CALL moist_redistribute(n_moist, acpwlqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,  &
              wbacpwlv(i,j), wbacpwlc(i,j), wbacpwlr(i,j), wbacpwls(i,j), wbacpwli(i,j),              &
              wbacpwlg(i,j), wbacpwlh(i,j))
            CALL moist_redistribute(n_moist, acpwmqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,  &
              wbacpwmv(i,j), wbacpwmc(i,j), wbacpwmr(i,j), wbacpwms(i,j), wbacpwmi(i,j),              &
              wbacpwmg(i,j), wbacpwmh(i,j))
            CALL moist_redistribute(n_moist, acpwhqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,  &
              wbacpwhv(i,j), wbacpwhc(i,j), wbacpwhr(i,j), wbacpwhs(i,j), wbacpwhi(i,j),              &
              wbacpwhg(i,j), wbacpwhh(i,j))
            ! Horizontal advection
            CALL moist_redistribute(n_moist, acflqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbacflv(i,j), wbacflc(i,j), wbacflr(i,j), wbacfls(i,j), wbacfli(i,j),                   &
              wbacflg(i,j), wbacflh(i,j))
            CALL moist_redistribute(n_moist, acfmqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbacfmv(i,j), wbacfmc(i,j), wbacfmr(i,j), wbacfms(i,j), wbacfmi(i,j),                   &
              wbacfmg(i,j), wbacfmh(i,j))
            CALL moist_redistribute(n_moist, acfhqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbacfhv(i,j), wbacfhc(i,j), wbacfhr(i,j), wbacfhs(i,j), wbacfhi(i,j),                   &
              wbacfhg(i,j), wbacfhh(i,j))
            ! Vertical advection
            CALL moist_redistribute(n_moist, aczlqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbaczlv(i,j), wbaczlc(i,j), wbaczlr(i,j), wbaczls(i,j), wbaczli(i,j),                   &
              wbaczlg(i,j), wbaczlh(i,j))
            CALL moist_redistribute(n_moist, aczmqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbaczmv(i,j), wbaczmc(i,j), wbaczmr(i,j), wbaczms(i,j), wbaczmi(i,j),                   &
              wbaczmg(i,j), wbaczmh(i,j))
            CALL moist_redistribute(n_moist, aczhqvar(:), m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,   &
              wbaczhv(i,j), wbaczhc(i,j), wbaczhr(i,j), wbaczhs(i,j), wbaczhi(i,j),                   &
              wbaczhg(i,j), wbaczhh(i,j))
          END IF

!#endif
!#if CDXWRF>=3
          ! temporal residence
          ! Checking consistency
          ! L. Fita, January 2024. Removed for efficiency
          !   If you encounter problems with the variable, remove it and re-compile without clean -a
!          IF (ALL(tashurstreshighres(i,:,j) == 0.)) THEN
!            PRINT *,'  Empty highres residence time for i,j:', i,j
!          END IF
          CALL var_residencetime_2var2D(t2(i,j), hurs(i,j), Nhtasrng, Nhhursrng, tashrng, hurshrng,   &
            itas, ihurs)
          tashurstreshighres(i,:,j) = 0.
          tashurstreshighres(i,(itas-1)*Nhhursrng+ihurs,j) = dt

          ! Checking consistency
          ! L. Fita, January 2024. Removed for efficiency
          !   If you encounter problems with the variable, remove it and re-compile without clean -a
!          IF (ALL(tashurstreslowres(i,:,j) == 0.)) THEN
!            PRINT *,'  Empty lowres residence time for i,j:', i,j
!          END IF
          CALL var_residencetime_2var2D(t2(i,j), hurs(i,j), Nltasrng, Nlhursrng, taslrng, hurslrng,   &
            itas,ihurs)
          tashurstreslowres(i,:,j) = 0.            
          tashurstreslowres(i,(itas-1)*Nlhursrng+ihurs,j) = dt          
          
          ! wind direction and speed
          CALL var_residencetime_2var2D(wbdsv(i,j), wssv(i,j), Nwbdsrng, Nwssrng, wbdsrng, wssrng,    &
            iwbds,iwss)
          wbdswsstres(i,:,j) = 0.            
          wbdswsstres(i,(iwbds-1)*Nwssrng+iwss,j) = dt          

!#endif
!#endif

          IF (dbg_level >= 75 .AND. ijdbg .AND. tdbg==0) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ' ',i,' ,',j,' : first value checks _______' 
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    longitude:', lon(i,j),' latitude:',lat(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    nsteps:', nsteps
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    clt:', clt(i,j), ' mean:', cltmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    cll:', cll(i,j), ' mean:', cllmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    clm:', clm(i,j), ' mean:', clmmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    clh:', clh(i,j), ' mean:', clhmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    surface wind gust:', wsgs(i,j), ' x-component:', ugustwind(i,j),         &
              ' y-component:', vgustwind(i,j), ' N gusts:', percengust(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    maximum surface wind gust:', wsgsmax(i,j), ' x-component:', ugsmax(i,j), &
              ' y-component:', vgsmax(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    total surface wind gust:', totwsgs(i,j), ' x-component:',                &
              totugustwind(i,j), ' y-component:', totvgustwind(i,j), ' N gusts:', totpercengust(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    total maximum surface wind gust:', totwsgsmax(i,j), ' x-component:',     &
              totugsmax(i,j), ' y-component:', totvgsmax(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    100m wind:', wsz100(i,j), ' x-component:', uz100wind(i,j),               &
              ' y-component:', vz100wind(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    maximum 100m wind:', wsz100max(i,j), ' x-component:', uz100max(i,j),     &
              ' y-component:', vz100max(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdown:', swdown(i,j), ' sunshine:', sunshine(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdown:', swdown(i,j), ' rsds:', rsds(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    glw:', glw(i,j), ' rlds:', rlds(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lh:', lh(i,j), ' hfls:', hfls(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    hfx:', hfx(i,j), ' hfss:', hfss(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swupb:', swupb(i,j), ' rsus:', rsus(i,j), ' rsusgen:', rsusgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lwupb:', lwupb(i,j), ' rlus:', rlus(i,j), ' rlusgen:', rlusgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    qfx:', qfx(i,j), ' evspsbl:', evspsbl(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    potevap:', potevap(i,j), ' evspsblpot:', evspsblpot(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    potevapgen:', potevapgen(i,j), ' evspsblrpotgen:', evspsblpotgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snowc:', snowc(i,j), ' snc:', snc(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snowh:', snowh(i,j), ' snd:', snd(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    smois (1,3):', smois(i,1,j), smois(i,2,j), smois(i,3,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    dzs (1,3):', dzs(1), dzs(2), dzs(3)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrsos:', mrsos(i,j), ' mrso:', mrso(i,j), ' mrsol (1,2):', mrsol(i,1,j), &
              mrsol(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrlso:', mrlso(i,j), ' mrlsos:', mrlsos(i,j), ' mrsll (1,2):',           &
              mrsll(i,1,j), mrsll(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrfso:', mrfso(i,j), ' mrfsos:', mrfsos(i,j), ' mrsfl (1,2):',           &
              mrsfl(i,1,j), mrsfl(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    fracsoil10cm (1,3):', fracsoil10cm(1), fracsoil10cm(2), fracsoil10cm(3)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrsosmean:', mrsosmean(i,j), ' mrsomean:', mrsomean(i,j),                &
              ' mrsolmean (1,2):', mrsolmean(i,1,j), mrsolmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrlsomean:', mrlsomean(i,j), ' mrlsosmean:', mrlsosmean(i,j),            &
              ' mrsllmean (1,2):', mrsllmean(i,1,j), mrsllmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrfsomean:', mrfsomean(i,j), ' mrfsosmean:', mrfsosmean(i,j),            &
              ' mrsflmean (1,2):', mrsflmean(i,1,j), mrsflmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    sfcrunoff:', sfcrunoff(i,j), ' pmrros:', pmrros(i,j), ' mrros:', mrros(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    udrunoff:', udrunoff(i,j), ' pmrro:', pmrro(i,j), ' mrro:', mrro(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    prc:', prc(i,j), ' prnc:', prnc(i,j), ' prsh:', prsh(i,j), ' flxpr:',    &
              flxpr(i,j), ' flxprl:', flxprl(i,j), ' flxprc:', flxprc(i,j), ' flxprsh:', flxprsh(i,j),&
              ' flxprsn:', flxprsn(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snow:', snow(i,j), ' snw:', snw(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdnt:', swdnt(i,j), ' rsdt:', rsdt(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swupt:', swupt(i,j), ' rsut:', rsut(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lwupt:', lwupt(i,j), ' rlut:', rlut(i,j)
            CALL wrf_debug(75,msg)
!#ifdef CDXWRF
!#if CDXWRF>=1
            IF (convxtrm_diag == 1) THEN
              WRITE(msg,*) '    cape:', cape(i,j), ' cin:', cin(i,j),' zlfc:', zlfc(i,j), ' plfc:',   &
                plfc(i,j), 'lidx:', lidx(i,j)
              CALL wrf_debug(75,msg)
            END IF
            WRITE(msg,*) '    iut:', iut(i,j), ' ivt:', ivt(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    fog:', fog(i,j), ' tfog:', tfog(i,j), ' vis:', vis(i,j), ' vismin:',     &
              fogvisbltymin(i,j), ' vismax:', fogvisbltymax(i,j), ' vismean:', fogvisbltymean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tds:', tds(i,j), ' min:', tdsmin(i,j), ' max:', tdsmax(i,j),             &
              ' mean:', tdsmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tws:', tws(i,j), ' min:', twsmin(i,j), ' max:', twsmax(i,j),             &
              ' mean:', twsmean(i,j)
!#endif

!#if CDXWRF>=4
            WRITE(msg,*) '    diabh:', SUM(diabh(i,:,j))*dt, ' wbacdh:', wbacdh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '      dhl:', wbacdhl(i,j), 'dhm:', wbacdhm(i,j), 'dhh:', wbacdhh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    WB only vater wapour m_qv:', m_qv 
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: pw', SUM(pwqvar(i,:,j,m_qv)), ' acpw:', wbacpw(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: f', SUM(fqvar(i,:,j,m_qv)), ' acf:', wbacf(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: z', SUM(zqvar(i,:,j,m_qv)), ' acz:', wbacz(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: zl', varlmh(1), ' aczlv:', wbaczlv(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: zm', varlmh(2), ' aczmv:', wbaczmv(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: zh', varlmh(3), ' aczhv:', wbaczhv(i,j)
            CALL wrf_debug(75,msg)

!#endif
!#if CDXWRF>=3
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tashurstreshighres: ',tashurstreshighres(i,INT(Nhtasrng/2*Nhhursrng/2),j),&
              ' tashurstreslowres: ', tashurstreslowres(i,INT(Nltasrng/2*Nlhursrng/2),j),              &
              ' wbdswsstres: ', wbdswsstres(i,INT(Nwbdsrng/2*Nwssrng/2),j)
            CALL wrf_debug(75,msg)
!#endif
!#endif
            !CALL wrf_error_fatal(msg)
          END IF

          ijdbg = .FALSE.
        ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO

  ELSE
    xtimep = xtime + dt/60.   ! value at end of timestep for time info
    nsteps=nsteps+1
    ijdbg = .FALSE.
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.
          cltmean(i,j) = cltmean(i,j) + clt(i,j)
          cllmean(i,j) = cllmean(i,j) + cll(i,j)
          clmmean(i,j) = clmmean(i,j) + clm(i,j)
          clhmean(i,j) = clhmean(i,j) + clh(i,j)

          ! Wind gust
          ! Looking for the maximum
          IF (wsgs(i,j) > wsgsmax(i,j)) THEN
            wsgsmax(i,j) = wsgs(i,j)
            ugsmax(i,j) = ugustwind(i,j)
            vgsmax(i,j) = vgustwind(i,j)
          END IF
          percengust(i,j) = percengust(i,j) + gustpoint(i,j)*1.

          IF (totwsgs(i,j) > totwsgsmax(i,j)) THEN
            totwsgsmax(i,j) = totwsgs(i,j)
            totugsmax(i,j) = totugustwind(i,j)
            totvgsmax(i,j) = totvgustwind(i,j)
          END IF
          totpercengust(i,j) = totpercengust(i,j) + totgustpoint(i,j)*1.

          ! 100 m wind
          ! Looking for the maximum
          IF (wsz100(i,j) > wsz100max(i,j)) THEN
            wsz100max(i,j) = wsz100(i,j)
            uz100max(i,j) = uz100wind(i,j)
            vz100max(i,j) = vz100wind(i,j)
          END IF

          ! sunshine
          IF (swdown(i,j) > 120. ) sunshine(i,j) = sunshine(i,j) + dt

          ! Fluxes
          rsds(i,j) = rsds(i,j) + swdown(i,j)
          rlds(i,j) = rlds(i,j) + glw(i,j)
          hfls(i,j) = hfls(i,j) + lh(i,j)
          hfss(i,j) = hfss(i,j) + hfx(i,j)
          rsus(i,j) = rsus(i,j) + swupb(i,j)
          rlus(i,j) = rlus(i,j) + lwupb(i,j)
          rsusgen(i,j) = rsusgen(i,j) - swdown(i,j)*albedo(i,j)
          rlusgen(i,j) = rlusgen(i,j) + STBOLT*emiss(i,j)*ts(i,j)**4
          evspsbl(i,j) = evspsbl(i,j) + qfx(i,j)
          evspsblpot(i,j) = evspsblpot(i,j) + potevap(i,j)
          evspsblpotgen(i,j) = evspsblpotgen(i,j) + potevapgen(i,j)

          ! Snow
          snc(i,j) = snc(i,j) + snowc(i,j)*100.
          snd(i,j) = snd(i,j) + snowh(i,j)

          ! Run-off (already accumulations)
          mrros(i,j) = sfcrunoff(i,j)
          mrro(i,j) = sfcrunoff(i,j) + udrunoff(i,j)

          ! Soil
          mrsosmean(i,j) = mrsosmean(i,j) + mrsos(i,j)
          mrsomean(i,j) = mrsomean(i,j) + mrso(i,j)
          mrlsomean(i,j) = mrlsomean(i,j) + mrlso(i,j)
          mrlsosmean(i,j) = mrlsosmean(i,j) + mrlsos(i,j)
          mrfsomean(i,j) = mrfsomean(i,j) + mrfso(i,j)
          mrfsosmean(i,j) = mrfsosmean(i,j) + mrfsos(i,j)
          DO k=1, num_soil_layers
            mrsolmean(i,k,j) = mrsolmean(i,k,j) + mrsol(i,k,j)
            mrsllmean(i,k,j) = mrsllmean(i,k,j) + mrsll(i,k,j)
            mrsflmean(i,k,j) = mrsflmean(i,k,j) + mrsfl(i,k,j)
          END DO

          ! Precipitations
          flxpr(i,j) = flxpr(i,j) + prc(i,j) + prnc(i,j) + prsh(i,j)
          flxprl(i,j) = flxprl(i,j) + prnc(i,j)
          flxprc(i,j) = flxprc(i,j) + prc(i,j)
          flxprsh(i,j) = flxprsh(i,j) + prsh(i,j)
          flxprsn(i,j) = flxprsn(i,j) + flxpr(i,j)*sr(i,j)
          snw(i,j) = snw(i,j) + snow(i,j)

          ! Fluxes at TOA
          rsdt(i,j) = rsdt(i,j) + swdnt(i,j)
          rsut(i,j) = rsut(i,j) + swupt(i,j)
          rlut(i,j) = rlut(i,j) + lwupt(i,j)

!#ifdef CDXWRF
!#if CDXWRF>=1
          ! Extremes of convection indices
          IF (convxtrm_diag == 1) THEN
            IF (cape(i,j) < capemin(i,j)) capemin(i,j) = cape(i,j)
            IF (cin(i,j) < cinmin(i,j)) cinmin(i,j) = cin(i,j)
            IF (zlfc(i,j) < zlfcmin(i,j)) zlfcmin(i,j) = zlfc(i,j)
            IF (plfc(i,j) < plfcmin(i,j)) plfcmin(i,j) = plfc(i,j)
            IF (lidx(i,j) < lidxmin(i,j)) lidxmin(i,j) = lidx(i,j)
            IF (cape(i,j) > capemax(i,j)) capemax(i,j) = cape(i,j)
            IF (cin(i,j) > cinmax(i,j)) cinmax(i,j) = cin(i,j)
            IF (zlfc(i,j) > zlfcmax(i,j)) zlfcmax(i,j) = zlfc(i,j)
            IF (plfc(i,j) > plfcmax(i,j)) plfcmax(i,j) = plfc(i,j)
            IF (lidx(i,j) > lidxmax(i,j)) lidxmax(i,j) = lidx(i,j)
            capemean(i,j) = capemean(i,j) + cape(i,j)
            cinmean(i,j) = cinmean(i,j) + cin(i,j)
            zlfcmean(i,j) = zlfcmean(i,j) + zlfc(i,j)
            plfcmean(i,j) = plfcmean(i,j) + plfc(i,j)
            lidxmean(i,j) = lidxmean(i,j) + lidx(i,j)
          END IF
          
          ! Horizontal water transport
          iutmean(i,j) = iutmean(i,j) + iut(i,j)
          ivtmean(i,j) = ivtmean(i,j) + ivt(i,j)

          ! fog
          IF (fog(i,j) == 1) THEN
            tfog(i,j) = tfog(i,j) + dt
            IF (fogvisbltymin(i,j) > vis(i,j)) fogvisbltymin(i,j) = vis(i,j)
            IF (fogvisbltymax(i,j) < vis(i,j)) fogvisbltymax(i,j) = vis(i,j)
            fogvisbltymean(i,j) = fogvisbltymean(i,j) + vis(i,j)
          END IF

          ! tds
          IF (tdsmin(i,j) > tds(i,j)) tdsmin(i,j) = tds(i,j)
          IF (tdsmax(i,j) < tds(i,j)) tdsmax(i,j) = tds(i,j)
          tdsmean(i,j) = tdsmean(i,j) + tds(i,j)

          ! tws
          IF (twsmin(i,j) > tws(i,j)) twsmin(i,j) = tws(i,j)
          IF (twsmax(i,j) < tws(i,j)) twsmax(i,j) = tws(i,j)
          twsmean(i,j) = twsmean(i,j) + tws(i,j)
!#endif

!#if CDXWRF>=4

          IF (wbout == 1) THEN
            wbacdh(i,j) = wbacdh(i,j) + SUM(diabh(i,:,j))*dt
            ! Vertical intervals
            CALL interval_agg(dimz, 2, 3, diabh(i,:,j), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
            wbacdhl(i,j) = wbacdhl(i,j) + varlmh(1)*dt
            wbacdhm(i,j) = wbacdhm(i,j) + varlmh(2)*dt
            wbacdhh(i,j) = wbacdhh(i,j) + varlmh(3)*dt
            ! First get previous values
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacpw(i,j),          &
              wbacpwc(i,j), wbacpwr(i,j), wbacpws(i,j), wbacpwi(i,j), wbacpwg(i,j), wbacpwh(i,j),     &
              acpwqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacf(i,j),           &
              wbacfc(i,j), wbacfr(i,j), wbacfs(i,j), wbacfi(i,j), wbacfg(i,j), wbacfh(i,j), acfqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacz(i,j),           &
              wbaczc(i,j), wbaczr(i,j), wbaczs(i,j), wbaczi(i,j), wbaczg(i,j), wbaczh(i,j), aczqvar)
            ! low-mid-high
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacpwlv(i,j),        &
              wbacpwlc(i,j), wbacpwlr(i,j), wbacpwls(i,j), wbacpwli(i,j), wbacpwlg(i,j),wbacpwlh(i,j),&
              acpwlqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacpwmv(i,j),        &
              wbacpwmc(i,j), wbacpwmr(i,j), wbacpwms(i,j), wbacpwmi(i,j), wbacpwmg(i,j),wbacpwmh(i,j),&
              acpwmqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacpwhv(i,j),        &
              wbacpwhc(i,j), wbacpwhr(i,j), wbacpwhs(i,j), wbacpwhi(i,j), wbacpwhg(i,j),wbacpwhh(i,j),&
              acpwhqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacflv(i,j),         &
              wbacflc(i,j), wbacflr(i,j), wbacfls(i,j),wbacfli(i,j),wbacflg(i,j),wbacflh(i,j),acflqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacfmv(i,j),         &
              wbacfmc(i,j), wbacfmr(i,j), wbacfms(i,j),wbacfmi(i,j),wbacfmg(i,j),wbacfmh(i,j),acfmqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbacfhv(i,j),         &
              wbacfhc(i,j), wbacfhr(i,j), wbacfhs(i,j),wbacfhi(i,j),wbacfhg(i,j),wbacfhh(i,j),acfhqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbaczlv(i,j),         &
              wbaczlc(i,j), wbaczlr(i,j), wbaczls(i,j),wbaczli(i,j),wbaczlg(i,j),wbaczlh(i,j),aczlqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbaczmv(i,j),         &
              wbaczmc(i,j), wbaczmr(i,j), wbaczms(i,j),wbaczmi(i,j),wbaczmg(i,j),wbaczmh(i,j),aczmqvar)
            CALL moist_group(n_moist, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh, wbaczhv(i,j),         &
              wbaczhc(i,j), wbaczhr(i,j), wbaczhs(i,j),wbaczhi(i,j),wbaczhg(i,j),wbaczhh(i,j),aczhqvar)
            DO m=1, n_moist
              acpwqvar(m) = acpwqvar(m) + SUM(pwqvar(i,:,j,m))
              acfqvar(m) = acfqvar(m) + SUM(fqvar(i,:,j,m))
              aczqvar(m) = aczqvar(m) + SUM(zqvar(i,:,j,m))
              ! Vertical intervals
              CALL interval_agg(dimz, 2, 3, pwqvar(i,:,j,m), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
              acpwlqvar(m) = acpwlqvar(m) + varlmh(1)
              acpwmqvar(m) = acpwmqvar(m) + varlmh(2)
              acpwhqvar(m) = acpwhqvar(m) + varlmh(3)
              CALL interval_agg(dimz, 2, 3, fqvar(i,:,j,m), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
              acflqvar(m) = acflqvar(m) + varlmh(1)
              acfmqvar(m) = acfmqvar(m) + varlmh(2)
              acfhqvar(m) = acfhqvar(m) + varlmh(3)
              CALL interval_agg(dimz, 2, 3, zqvar(i,:,j,m), p8w(i,:,j), (/ prmlcR, prmhcR /), varlmh)
              aczlqvar(m) = aczlqvar(m) + varlmh(1)
              aczmqvar(m) = aczmqvar(m) + varlmh(2)
              aczhqvar(m) = aczhqvar(m) + varlmh(3)
            END DO

            CALL moist_redistribute(n_moist, acpwqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbacpw(i,j), wbacpwc(i,j), wbacpwr(i,j), wbacpws(i,j), wbacpwi(i,j),                    &
              wbacpwg(i,j), wbacpwh(i,j))
            CALL moist_redistribute(n_moist, acfqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,       &
              wbacf(i,j), wbacfc(i,j), wbacfr(i,j), wbacfs(i,j), wbacfi(i,j), wbacfg(i,j),            &
              wbacfh(i,j))
            CALL moist_redistribute(n_moist, aczqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,       &
              wbacz(i,j), wbaczc(i,j), wbaczr(i,j), wbaczs(i,j), wbaczi(i,j), wbaczg(i,j),            &
              wbaczh(i,j))
            ! Low-medium-heigh
            ! Total tendency
            CALL moist_redistribute(n_moist, acpwlqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,     &
              wbacpwlv(i,j), wbacpwlc(i,j), wbacpwlr(i,j), wbacpwls(i,j), wbacpwli(i,j),              &
              wbacpwlg(i,j), wbacpwlh(i,j))
            CALL moist_redistribute(n_moist, acpwmqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,     &
              wbacpwmv(i,j), wbacpwmc(i,j), wbacpwmr(i,j), wbacpwms(i,j), wbacpwmi(i,j),              &
              wbacpwmg(i,j), wbacpwmh(i,j))
            CALL moist_redistribute(n_moist, acpwhqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,     &
              wbacpwhv(i,j), wbacpwhc(i,j), wbacpwhr(i,j), wbacpwhs(i,j), wbacpwhi(i,j),              &
              wbacpwhg(i,j), wbacpwhh(i,j))
            ! Horizontal advection
            CALL moist_redistribute(n_moist, acflqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbacflv(i,j), wbacflc(i,j), wbacflr(i,j), wbacfls(i,j), wbacfli(i,j),                   &
              wbacflg(i,j), wbacflh(i,j))
            CALL moist_redistribute(n_moist, acfmqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbacfmv(i,j), wbacfmc(i,j), wbacfmr(i,j), wbacfms(i,j), wbacfmi(i,j),                   &
              wbacfmg(i,j), wbacfmh(i,j))
            CALL moist_redistribute(n_moist, acfhqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbacfhv(i,j), wbacfhc(i,j), wbacfhr(i,j), wbacfhs(i,j), wbacfhi(i,j),                   &
              wbacfhg(i,j), wbacfhh(i,j))
            ! Vertical advection
            CALL moist_redistribute(n_moist, aczlqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbaczlv(i,j), wbaczlc(i,j), wbaczlr(i,j), wbaczls(i,j), wbaczli(i,j),                   &
              wbaczlg(i,j), wbaczlh(i,j))
            CALL moist_redistribute(n_moist, aczmqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbaczmv(i,j), wbaczmc(i,j), wbaczmr(i,j), wbaczms(i,j), wbaczmi(i,j),                   &
              wbaczmg(i,j), wbaczmh(i,j))
            CALL moist_redistribute(n_moist, aczhqvar, m_qv, m_qc, m_qr, m_qs, m_qi, m_qg, m_qh,      &
              wbaczhv(i,j), wbaczhc(i,j), wbaczhr(i,j), wbaczhs(i,j), wbaczhi(i,j),                   &
              wbaczhg(i,j), wbaczhh(i,j))
          END IF
!#endif
!#if CDXWRF>=3
          ! temporal residence
          ! High res tas, hurs
          CALL var_residencetime_2var2D(t2(i,j), hurs(i,j), Nhtasrng, Nhhursrng, tashrng, hurshrng,   &
            itas, ihurs)
          ! Checking consistency
          ! L. Fita, January 2024. Removed for efficiency
          !   If you encounter problems with the variable, remove it and re-compile without clean -a
          IF (ijdbg .AND. tdbg==0) THEN
            PRINT *,'  longitude:', lon(i,j),' latitude:',lat(i,j)
            PRINT *,'  highres i,j:', i,j, ' t2:', t2(i,j), ' ps:', ps(i,j), ' q2:', q2(i,j),         &
              '=> hurs:', hurs(i,j)
            PRINT *,'  highres tas bin:', tashrng(itas-1), '- ', tashrng(itas)
            PRINT *,'  highres hurs bin:', hurshrng(ihurs-1), '- ', hurshrng(ihurs)
            PRINT *,'  highres Nhtasrng:', Nhtasrng, ' Nhhursrng:', Nhhursrng
            PRINT *,'  highres itas, ihurs:', itas, ihurs, 'khighrestashurs:', (itas-1)*Nhhursrng+ihurs
            PRINT *,'  current value prior dt:', tashurstreshighres(i,(itas-1)*Nhhursrng+ihurs,j)
!            PRINT *,'  all values:', tashurstreshighres(i,:,j)
          END IF
          tashurstreshighres(i,(itas-1)*Nhhursrng+ihurs,j) =                                          &
            tashurstreshighres(i,(itas-1)*Nhhursrng+ihurs,j) + dt

          ! Low res tas, hurs
          CALL var_residencetime_2var2D(t2(i,j), hurs(i,j), Nltasrng, Nlhursrng, taslrng, hurslrng,   &
            itas, ihurs)
          ! Checking consistency
          ! L. Fita, January 2024. Removed for efficiency
          !   If you encounter problems with the variable, remove it and re-compile without clean -a
          IF (ijdbg .AND. tdbg==0) THEN
            PRINT *,'  lowres i,j:', i,j, ' t2:', t2(i,j),' hurs:', hurs(i,j)
            PRINT *,'  lowres tas bin:', taslrng(itas-1), '- ', taslrng(itas)
            PRINT *,'  lowres hurs bin:', hurslrng(ihurs-1), '- ', hurslrng(ihurs)
            PRINT *,'  lowres Nltasrng:', Nltasrng, ' Nlhursrng:', Nlhursrng
            PRINT *,'  lowres itas, ihurs:', itas, ihurs, 'klowrestashurs:', (itas-1)*Nlhursrng+ihurs
            PRINT *,'  current value prior dt:', tashurstreslowres(i,(itas-1)*Nlhursrng+ihurs,j)
!            PRINT *,'  all values:', tashurstreslowres(i,:,j)
          END IF
          tashurstreslowres(i,(itas-1)*Nlhursrng+ihurs,j) =                                           &
            tashurstreslowres(i,(itas-1)*Nlhursrng+ihurs,j) + dt

          ! wind direction and speed
          CALL var_residencetime_2var2D(wbdsv(i,j), wssv(i,j), Nwbdsrng, Nwssrng, wbdsrng, wssrng,    &
            iwbds,iwss)
          wbdswsstres(i,(iwbds-1)*Nwssrng+iwss,j) = wbdswsstres(i,(iwbds-1)*Nwssrng+iwss,j) + dt          
          IF (ijdbg .AND. tdbg==0) THEN
            PRINT *,'  i,j:', i,j, ' wbds:', wbdsv(i,j),' wss:', wssv(i,j)
            PRINT *,'  wbds bin:', wbdsrng(iwbds-1), '- ', wbdsrng(iwbds)
            PRINT *,'  wss bin:', wssrng(iwss-1), '- ', wssrng(iwss)
            PRINT *,'  Nwbdsrng:', Nwbdsrng, ' Nwssrng:', Nwssrng
            PRINT *,'  iwbds, iwss:', iwbds, iwss, 'kwbdswss:', (iwbds-1)*Nwssrng+iwss
            PRINT *,'  current value prior dt:', wbdswsstres(i,(iwbds-1)*Nwssrng+iwss,j)
!            PRINT *,'  all values:', tashurstreslowres(i,:,j)
          END IF

!#endif
!#endif

          IF (dbg_level >= 75 .AND. ijdbg .AND. tdbg==0) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ' ',i,' ,',j,' : intermediate checks _______' 
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    longitude:', lon(i,j),' latitude:',lat(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    nsteps:', nsteps
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    clt:', clt(i,j), ' mean:', cltmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    cll:', cll(i,j), ' mean:', cllmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    clm:', clm(i,j), ' mean:', clmmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    clh:', clh(i,j), ' mean:', clhmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    surface wind gust:', wsgs(i,j), ' x-component:', ugustwind(i,j),         &
              ' y-component:', vgustwind(i,j), ' N gusts:', percengust(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    maximum surface wind gust:', wsgsmax(i,j), ' x-component:', ugsmax(i,j), &
              ' y-component:', vgsmax(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    100m wind:', wsz100(i,j), ' x-component:', uz100wind(i,j),               &
              ' y-component:', vz100wind(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    maximum 100m wind:', wsz100max(i,j), ' x-component:', uz100max(i,j),     &
              ' y-component:', vz100max(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdown:', swdown(i,j), ' sunshine:', sunshine(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdown:', swdown(i,j), ' rsds:', rsds(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    glw:', glw(i,j), ' rlds:', rlds(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lh:', lh(i,j), ' hfls:', hfls(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    hfx:', hfx(i,j), ' hfss:', hfss(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swupb:', swupb(i,j), ' rsus:', rsus(i,j), ' rsusgen:', rsusgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lwupb:', lwupb(i,j), ' rlus:', rlus(i,j), ' rlusgen:', rlusgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    qfx:', qfx(i,j), ' evspsbl:', evspsbl(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    potevap:', potevap(i,j), ' evspsbptot:', evspsblpot(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    potevapgen:', potevapgen(i,j), ' evspsblpotgen:', evspsblpotgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snowc:', snowc(i,j), ' snc:', snc(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snowh:', snowh(i,j), ' snd:', snd(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    smois (1,3):', smois(i,1,j), smois(i,2,j), smois(i,3,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    dzs (1,3):', dzs(1), dzs(2), dzs(3)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrsos:', mrsos(i,j), ' mrso:', mrso(i,j), ' mrsol (1,2):', mrsol(i,1,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrlso:', mrlso(i,j), ' mrlsos:', mrlsos(i,j), ' mrsll (1,2):',           &
              mrsll(i,1,j), mrsll(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrfso:', mrfso(i,j), ' mrfsos:', mrfsos(i,j), ' mrsfl (1,2):',           &
              mrsfl(i,1,j), mrsfl(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    fracsoil10cm (1,3):', fracsoil10cm(1), fracsoil10cm(2), fracsoil10cm(3)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrsosmean:', mrsosmean(i,j), ' mrsomean:', mrsomean(i,j),                &
              ' mrsolmean (1,2):', mrsolmean(i,1,j), mrsolmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrlsomean:', mrlsomean(i,j), ' mrlsosmean:', mrlsosmean(i,j),            &
              ' mrsllmean (1,2):', mrsllmean(i,1,j), mrsllmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrfsomean:', mrfsomean(i,j), ' mrfsosmean:', mrfsosmean(i,j),            &
              ' mrsflmean (1,2):', mrsflmean(i,1,j), mrsflmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    sfcrunoff:', sfcrunoff(i,j), ' mrros:', mrros(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    udrunoff:', udrunoff(i,j), ' mrro:', mrro(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    prc:', prc(i,j), ' prnc:', prnc(i,j), ' prsh:', prsh(i,j), ' flxpr:',    &
              flxpr(i,j), ' flxprl:', flxprl(i,j), ' flxprc:', flxprc(i,j), ' flxprsh:', flxprsh(i,j),&
              ' flxprsn:', flxprsn(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snow:', snow(i,j), ' snw:', snw(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdnt:', swdnt(i,j), ' rsdt:', rsdt(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swupt:', swupt(i,j), ' rsut:', rsut(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lwupt:', lwupt(i,j), ' rlut:', rlut(i,j)
            CALL wrf_debug(75,msg)

!#ifdef CDXWRF
!#if CDXWRF>=1
            IF (convxtrm_diag == 1) THEN
              WRITE(msg,*) '    cape:', cape(i,j), ' capemin:', capemin(i,j), ' capemax:',            &
                capemax(i,j), ' capemean:', capemean(i,j)
              CALL wrf_debug(75,msg)
              WRITE(msg,*) '    cin:', cin(i,j), ' cinmin:', cinmin(i,j), ' cinmax:', cinmax(i,j),    &
                ' capemean:', capemean(i,j)
              CALL wrf_debug(75,msg)
              WRITE(msg,*) '    zlfc:', zlfc(i,j), ' zlfcmin:', zlfcmin(i,j), ' zlfcmax:',            &
                zlfcmax(i,j), ' zlfcmean:', zlfcmean(i,j)
              CALL wrf_debug(75,msg)
              WRITE(msg,*) '    plfc:', plfc(i,j), ' plfcmin:', plfcmin(i,j), ' plfcmax:',            &
                plfcmax(i,j), ' plfcmean:', plfcmean(i,j)
              CALL wrf_debug(75,msg)
              WRITE(msg,*) '    lidx:',lidx(i,j), ' lidxmin:', lidxmin(i,j), ' lidxmax:',lidxmax(i,j),&
                ' lidxmean:', lidxmean(i,j)
              CALL wrf_debug(75,msg)
            END IF
            WRITE(msg,*) '    iutmean:', iutmean(i,j), ' ivtmean:', ivtmean(i,j)
            CALL wrf_debug(75,msg)            
            WRITE(msg,*)'    fog:', fog(i,j), ' tfog:', tfog(i,j), ' vis:', vis(i,j), ' vismin:',     &
              fogvisbltymin(i,j), ' vismax:', fogvisbltymax(i,j), ' vismean:', fogvisbltymean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tds:', tds(i,j), ' min:', tdsmin(i,j), ' max:', tdsmax(i,j),             &
              ' mean:', tdsmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tws:', tws(i,j), ' min:', twsmin(i,j), ' max:', twsmax(i,j),             &
              ' mean:', twsmean(i,j)
            CALL wrf_debug(75,msg)
!#endif
!#if CDXWRF>=4
            WRITE(msg,*) '    diabh:', SUM(diabh(i,:,j))*dt, ' wbacdh:', wbacdh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '      dhl:', wbacdhl(i,j), 'dhm:', wbacdhm(i,j), 'dhh:', wbacdhh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    WB only vater wapour m_qv:', m_qv 
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: pw', SUM(pwqvar(i,:,j,m_qv)), ' acpw:', wbacpw(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: f', SUM(fqvar(i,:,j,m_qv)), ' acf:', wbacf(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: z', SUM(zqvar(i,:,j,m_qv)), ' acz:', wbacz(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: zl', varlmh(1), ' aczlv:', wbaczlv(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: zm', varlmh(2), ' aczmv:', wbaczmv(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'       qv: zh', varlmh(3), ' aczhv:', wbaczhv(i,j)
            CALL wrf_debug(75,msg)
!#endif
!#if CDXWRF>=3
            WRITE(msg,*)'    tashurstreshighres: ',tashurstreshighres(i,INT(Nhtasrng/2*Nhhursrng/2),j),&
              ' tashurstreslowres: ', tashurstreslowres(i,INT(Nltasrng/2*Nlhursrng/2),j),              &
              ' wbdswsstres: ', wbdswsstres(i,INT(Nwbdsrng/2*Nwssrng/2),j)
            CALL wrf_debug(75,msg)
!#endif
!#endif

            !CALL wrf_error_fatal(msg)
          END IF
          ijdbg = .FALSE.
        ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO

  END IF 

  ! Some of CORDEX variables tend to be quite computational intensive, thus, just compute them when
  !   it is necessary (only valid for instantaneous variables)
  ijdbg = .FALSE.
  IF (is_output_time) THEN
    CALL wrf_debug(10,'  ' // TRIM(fname) // ': Computing instantaneous CORDEX variables')

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      IF (psl_diag == dpslptarget) THEN
        WRITE(msg,*) '  ' // TRIM(fname) // ': Smoothing surface pressure ', psmooth, ' times' 
        CALL wrf_debug(10,msg)
        ! Horizontally Smoothing
        smoothp = ps
        DO k=1, psmooth
          DO i = i_start(ij), i_end(ij)
            DO j = j_start(ij), j_end(ij)
              i1 = MIN(i+1,i_end(ij))
              i_1 = MAX(i-1,i_start(ij))
              j1 = MIN(j+1,j_end(ij))
              j_1 = MAX(j-1,j_start(ij))
              !i1 = i + 1
              !i_1 = i - 1
              !j1 = j + 1
              !j_1 = j - 1

              ! Smoothing surface pressure
              smoothp(i,j)= smoothp(i,j) + smoothp(i_1,j) + smoothp(i_1,j1) + smoothp(i,j1) +         &
                smoothp(i1,j1) + smoothp(i1,j) + smoothp(i1,j_1) + smoothp(i,j_1) + smoothp(i_1,j_1)
              smoothp(i,j)= smoothp(i,j) / 9.
            END DO
          END DO
          WRITE(msg,*) '  ' //TRIM(fname)// ':   Smoothed', k,' surface pressure i2, j2:',            &
            smoothp(i2(ij),j2(ij))
          CALL wrf_debug(100,msg)
        END DO
        WRITE(msg,*) '  ' //TRIM(fname)// ': Finally Smoothed !!'
        CALL wrf_debug(100,msg)
      END IF

      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.
          i1 = MIN(i+1,i_end(ij))
          i_1 = MAX(i-1,i_start(ij))
          j1 = MIN(j+1,j_end(ij))
          j_1 = MAX(j-1,j_start(ij))
          !WRITE(msg, *)'i:',i,' i+1', i+1, ' j:', j, ' j+1:', j+1, ' ilims:', i_start(ij), i_end(ij), ' jlims:', j_start(ij), j_end(ij)
          !CALL wrf_debug(10,'  ' // TRIM(msg) )

          ! Height above surface at half-mass levels
          zsfc = zg(i,:,j)-hgt(i,j)

          ! Unstaggering certain variables

          ! prw: Total water path 
          CALL var_massvertint(qv(i,:,j), mutot(i,j), dimz, deta, prw(i,j))

          ! clwvi: Total condensated water path 
          qvar = qc(i,:,j)+qr(i,:,j)+qs(i,:,j)+qi(i,:,j)
          CALL var_massvertint(qvar, mutot(i,j), dimz, deta, clwvi(i,j))

          ! clivi: Total ice water path
          qvar = qs(i,:,j)+qi(i,:,j)+qg(i,:,j)+qh(i,:,j)
          CALL var_massvertint(qvar, mutot(i,j), dimz, deta, clivi(i,j))

          ! 2m specific humidity
          CALL var_huss(q2(i,j), huss(i,j))

          ! 10m wind speed
          wss(i,j) = SQRT(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))

          ! sea level pressure
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing sea level pressure diagnostic'
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)
          psldiag: SELECT CASE (psl_diag)
            CASE (dpslshuell)
              WRITE(msg,*) '  ' // TRIM(fname) // ': psl diagnostic= ', psl_diag, '  hydrostatic ' // &
                'Shuell corrected method'
              CALL var_psl_shuell(hgt(i,j), ps(i,j), zg(i,1,j), qv(i,1,j), t(i,1,j), psl(i,j))
            CASE (dpslptarget)
              WRITE(msg,*) '  ' // TRIM(fname) // ': psl diagnostic= ', psl_diag, '  ptarget method'//&
                ' press. target=', ptarget, ' Pa'
              CALL var_psl_ptarget(press(i,:,j), smoothp(i,j), hgt(i,j), t(i,:,j), qv(i,:,j), dimz,  &
                ptarget, psl(i,j))
            CASE (dpslecmwf)
              WRITE(msg,*) '  ' // TRIM(fname) // ': psl diagnostic= ', psl_diag, '  ECMWF method'
              CALL var_psl_ecmwf(ps(i,j), hgt(i,j), t(i,1,j), press(i,1,j), p8w(i,2,j), psl(i,j))
            CASE DEFAULT
              WRITE (msg,*) '  ' // TRIM(fname) // ": ERROR -- error -- ERROR -- error : type " //    &
                 "of sea level pressure 'psl_diag=", psl_diag, "' not ready !! " // CHAR(10) //       &
                 "      available ones; ", dpslshuell, " : shuell", dpslptarget, ' : ptarget, ',      & 
                 dpslecmwf,' : ecmwf '
              CALL wrf_error_fatal(msg)
          END SELECT psldiag
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(100,msg)

          ! tauu, tauv
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing tauu,tauv diagnostic'
          IF (ijdbg) CALL wrf_debug(50,msg)
          cdcdx(i,j) = Cd(i,j)
          tauu(i,j) = cdcdx(i,j)*uas(i,j)*uas(i,j)
          tauv(i,j) = cdcdx(i,j)*vas(i,j)*vas(i,j)
          tauugen(i,j) = cdgen(i,j)*uas(i,j)*uas(i,j)
          tauvgen(i,j) = cdgen(i,j)*vas(i,j)*vas(i,j)
          
          ! ORCHIDEE t2 & q2 diagnostics
          hus_kts = qv(i,kts,j)/(1.0+qv(i,kts,j))
          
          ! zg_kts = zg(i,kts,j)/g (It should be gravity maybe from model_constants)
          zg_kts = zg(i,kts,j)
          CALL SFCDIAGS_ORCHIDEE1D( u(i,kms:kme,j), v(i,kms:kme,j), t(i,kms:kme,j), hus_kts,          &
            zg_kts, p_phy(i,kms:kme,j),                                                               &
            ts(i,j), qsfc(i,j), znt(i,j), ps(i,j),                                                    &
            chs(i,j)/(uas(i,j)**2+vas(i,j)**2), cdgen(i,j), br(i,j), ust(i,j), zol(i,j),              &
            kms, kme, kts, kte,                                                                       &
            tas_hm(i,j), huss_hm)
          qvs_hm(i,j) = huss_hm/(1.0-huss_hm)

          ! Compute statistics (no need when XIOS or similar is used....)
          cltmean(i,j) = cltmean(i,j)/nsteps
          cllmean(i,j) = cllmean(i,j)/nsteps
          clmmean(i,j) = clmmean(i,j)/nsteps
          clhmean(i,j) = clhmean(i,j)/nsteps
          percengust(i,j) = percengust(i,j)*100./nsteps
          totpercengust(i,j) = totpercengust(i,j)*100./nsteps

          ! Fluxes
          rsds(i,j) = rsds(i,j)/nsteps
          rlds(i,j) = rlds(i,j)/nsteps
          hfls(i,j) = hfls(i,j)/nsteps
          hfss(i,j) = hfss(i,j)/nsteps
          rsus(i,j) = rsus(i,j)/nsteps
          rlus(i,j) = rlus(i,j)/nsteps
          rsusgen(i,j) = rsusgen(i,j)/nsteps
          rlusgen(i,j) = rlusgen(i,j)/nsteps
          evspsbl(i,j) = evspsbl(i,j)/nsteps
          evspsblpot(i,j) = evspsblpot(i,j)/nsteps
          evspsblpotgen(i,j) = evspsblpotgen(i,j)/nsteps

          ! Snow
          ! Maybe something more complete... ideas welcome !!
          snc(i,j) = snc(i,j)/nsteps
          snd(i,j) = snd(i,j)/nsteps

          ! Run-off (de-accumulation)
          mrros(i,j) = (mrros(i,j)-pmrros(i,j))/(dt*nsteps)
          mrro(i,j) = (mrro(i,j)-pmrro(i,j))/(dt*nsteps)

          ! soil
          mrsomean(i,j) = mrsomean(i,j)/nsteps
          mrsosmean(i,j) = mrsosmean(i,j)/nsteps
          mrlsomean(i,j) = mrlsomean(i,j)/nsteps
          mrlsosmean(i,j) = mrlsosmean(i,j)/nsteps
          mrfsomean(i,j) = mrfsomean(i,j)/nsteps
          mrfsosmean(i,j) = mrfsosmean(i,j)/nsteps
          mrsolmean(i,:,j) = mrsolmean(i,:,j)/nsteps
          mrsllmean(i,:,j) = mrsllmean(i,:,j)/nsteps
          mrsflmean(i,:,j) = mrsflmean(i,:,j)/nsteps

          ! Precipitations
          flxpr(i,j) = flxpr(i,j)/(dt*nsteps)
          flxprl(i,j) = flxprl(i,j)/(dt*nsteps)
          flxprc(i,j) = flxprc(i,j)/(dt*nsteps)
          flxprsh(i,j) = flxprsh(i,j)/(dt*nsteps)
          flxprsn(i,j) = flxprsn(i,j)/(dt*nsteps)

          ! Fluxes at TOA
          rsdt(i,j) = rsdt(i,j)/nsteps
          rsut(i,j) = rsut(i,j)/nsteps
          rlut(i,j) = rlut(i,j)/nsteps

!#ifdef CDXWRF
!#if CDXWRF>=1
          ! clgvi: Total graupel water path
          qvar = qg(i,:,j)
          CALL var_massvertint(qvar, mutot(i,j), dimz, deta, clgvi(i,j))

          ! clhvi: Total hail water path
          qvar = qh(i,:,j)
          CALL var_massvertint(qvar, mutot(i,j), dimz, deta, clhvi(i,j))

          ! zmla_gen
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing zmla generic diagnostic'
          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)
          IF (zmlagen_diag == zmlagenthetae) THEN
            WRITE(msg,*) '     use theta-based method (Nielsen-Gammon et al., 2008)'
            IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)      
            CALL var_zmla_generic(dimz, qv(i,:,j), th(i,:,j), z(i,:,j), hgt(i,j), dqv, dtheta,        &
              zmla(i,j))
          ELSE IF (zmlagen_diag == zmlagenbulk) THEN
            WRITE(msg,*) '     use bulk-Richardson number method (Vogelezang and Holtslag, 1996; Seidel et al., 2004)'
            IF (ijdbg) CALL wrf_debug(50,msg)
            !   here is used qcs = from Monin-Obukhov similarity theory 
            qsolid = qs(i,:,j) + qi(i,:,j) + qg(i,:,j) + qh(i,:,j)
            CALL var_bulk_Ri(dimz, th(i,:,j), qv(i,:,j), qc(i,:,j), qsolid, z(i,:,j), unu(i,:,j),     &
              unv(i,:,j), t(i,:,j), ust(i,j), ri)
            CALL var_zmla_RIgen(dimz, ri, zg(i,:,j), hgt(i,j), 0.25, zmla(i,j))
          END IF

          ! cape (no need to reach top of atmosphere... dz1)
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing CAPE diagnostic'
          IF (ijdbg) CALL wrf_debug(50,msg)
! L. Fita, January 2018. CIMA
!! Changing to subroutine
          IF (convxtrm_diag == 0) THEN
            CALL var_cape_afwa(dz1, t(i,1:dz1,j), hur(i,1:dz1,j)*100., press(i,1:dz1,j), z(i,1:dz1,j),&
              1, cape(i,j), cin(i,j), zlfc(i,j), plfc(i,j), lidx(i,j), 3)
            ! Giving it as height above ground
            IF (zlfc(i,j) /= -1.) zlfc(i,j) = zlfc(i,j) - hgt(i,j)
          END IF

          ! Extremes of convection indices
          IF (convxtrm_diag == 1) THEN
            capemean(i,j) = capemean(i,j)/nsteps
            cinmean(i,j) = cinmean(i,j)/nsteps
            zlfcmean(i,j) = zlfcmean(i,j)/nsteps
            plfcmean(i,j) = plfcmean(i,j)/nsteps
            lidxmean(i,j) = lidxmean(i,j)/nsteps
          END IF
          
          ! Horizontal water vapour transport
          iutmean(i,j) = iutmean(i,j)/nsteps
          ivtmean(i,j) = ivtmean(i,j)/nsteps
          
          ! 0-isotherm
          ! Height above ground
          zagl = z(i,:,j)-hgt(i,j)
          zeroith(i,j) = zero_isotherm1D(dimz, t(i,:,j), zagl(:))
          
!#endif
!#if CDXWRF>=2
          ! specific humidity
          CALL var_hus(qv(i,:,j), dimz, hus(i,:,j))

          ! Earth rotated winds
          CALL var_uava(unu(i,:,j), unv(i,:,j), sina(i,j), cosa(i,j), dimz, ua(i,:,j), va(i,:,j))

          ! Wind speed
          ws(i,:,j) = SQRT(unu(i,:,j)*unu(i,:,j)+unv(i,:,j)*unv(i,:,j))

          ! fog
          fogvisbltymean(i,j) = fogvisbltymean(i,j)/nsteps

          ! tds
          tdsmean(i,j) = tdsmean(i,j)/nsteps

          ! tws
          twsmean(i,j) = twsmean(i,j)/nsteps

!#endif
!#endif

          IF (dbg_level >= 75 .AND. ijdbg) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ' ',i,' ,',j,' : final checks _______' 
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    nsteps:', nsteps
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    tas_hm:', tas_hm(i,j), '  qvs_hm:', qvs_hm(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    cltmean:', cltmean(i,j), ' cllmean:', cllmean(i,j), ' clmmean:',        &
              clmmean(i,j), ' clhmean', clhmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    prw:', prw(i,j), ' clwvi:', clwvi(i,j), ' clivi:', clivi(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    uas', uas(i,j), ' vas:', vas(i,j), ' wss:', wss(i,j), ' wbds:',         &
              wbds(i,j), '  hurs', hurs(i,j), ' huss:', huss(i,j), ' psl:', psl(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    mrso:', mrso(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    maximum surface wind gust:', wsgsmax(i,j), ' x-component:', ugsmax(i,j), &
              ' y-component:', vgsmax(i,j), ' %:', percengust(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    total maximum surface wind gust:', totwsgsmax(i,j), ' x-component:',     &
              totugsmax(i,j), ' y-component:', totvgsmax(i,j), ' %:', totpercengust(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    100m wind:', wsz100(i,j), ' x-component:', uz100wind(i,j),               &
              ' y-component:', vz100wind(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    maximum 100m wind:', wsz100max(i,j), ' x-component:', uz100max(i,j),     &
              ' y-component:', vz100max(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swdown:', swdown(i,j), ' sunshine:', sunshine(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    Cd:', Cd(i,j), 'Cdcdx:', cdcdx(i,j), ' tauu:', tauu(i,j), ' tauv:',      &
              tauv(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    Cdgen:', cdgen(i,j), ' tauugen:', tauugen(i,j),         &
              ' tauvgen:', tauvgen(i,j)
            WRITE(msg,*)'    swdown:', swdown(i,j), ' rsds:', rsds(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    glw:', glw(i,j), ' rlds:', rlds(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lh:', lh(i,j), ' hfls:', hfls(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    hfx:', hfx(i,j), ' hfss:', hfss(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    swupb:', swupb(i,j), ' rsus:', rsus(i,j), ' rsusgen:', rsusgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    lwupb:', lwupb(i,j), ' rlus:', rlus(i,j), ' rlusgen:', rlusgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    qfx:', qfx(i,j), ' evspsbl:', evspsbl(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    potevap:', potevap(i,j), ' evspsbltot:', evspsblpot(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    potevapgen:', potevapgen(i,j), ' evspsbltotgen:', evspsblpotgen(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snowc:', snowc(i,j), ' snc:', snc(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snowh:', snowh(i,j), ' snd:', snd(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    smois (1,3):', smois(i,1,j), smois(i,2,j), smois(i,3,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    dzs (1,3):', dzs(1), dzs(2), dzs(3)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrsos:', mrsos(i,j), ' mrso:', mrso(i,j), ' mrsol (1,2):', mrsol(i,1,j), &
              mrsol(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrlso:', mrlso(i,j), ' mrlsos:', mrlsos(i,j), ' mrsll (1,2):',           &
              mrsll(i,1,j), mrsll(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrfso:', mrfso(i,j), ' mrfsos:', mrfsos(i,j), ' mrsfl (1,2):',           &
              mrsfl(i,1,j), mrsfl(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    fracsoil10cm (1,3):', fracsoil10cm(1), fracsoil10cm(2), fracsoil10cm(3)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrsosmean:', mrsosmean(i,j), ' mrsomean:', mrsomean(i,j),                &
              ' mrsolmean (1,2):', mrsolmean(i,1,j), mrsolmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrlsomean:', mrlsomean(i,j), ' mrlsosmean:', mrlsosmean(i,j),            &
              ' mrsllmean (1,2):', mrsllmean(i,1,j), mrsllmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    mrfsomean:', mrfsomean(i,j), ' mrfsosmean:', mrfsosmean(i,j),            &
              ' mrsflmean (1,2):', mrsflmean(i,1,j), mrsflmean(i,2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    sfcrunoff:', sfcrunoff(i,j), ' mrros:', mrros(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    udrunoff:', udrunoff(i,j), ' mrro:', mrro(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    prc:', prc(i,j), ' prnc:', prnc(i,j), ' prsh:', prsh(i,j), ' flxpr:',    &
              flxpr(i,j), ' flxprl:', flxprl(i,j), ' flxprc:', flxprc(i,j), ' flxprsh:', flxprsh(i,j),&
              ' flxprsn:', flxprsn(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    snw:', snw(i,j), ' rsdt:', rsdt(i,j), ' rsut:', rsut(i,j), ' rlut:',     &
              rlut(i,j)
            CALL wrf_debug(75,msg)
!#ifdef CDXWRF
!#if CDXWRF>=1
            WRITE(msg,*) '    clgvi:', clgvi(i,j), ' clhvi:', clhvi(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    zmla_gen:', zmla(i,j)
            CALL wrf_debug(75,msg)
            IF (convxtrm_diag == 1) THEN
              WRITE(msg,*) '    capemean:', capemean(i,j), ' cinmean:', cinmean(i,j), ' zlfcmean:',   &
                zlfcmean(i,j), ' plfcmean:', plfcmean(i,j), ' lidxmean:', lidxmean(i,j)
              CALL wrf_debug(75,msg)
            END IF
            WRITE(msg,*) '    iutmean:', iutmean(i,j), ' ivtmean:', ivtmean(i,j)
            CALL wrf_debug(75,msg)            
            WRITE(msg,*) '    0-isotherm:', zeroith(i,j)
            CALL wrf_debug(75,msg)            
!#endif
!#if CDXWRF>=2
            WRITE(msg,*) '    ua', ua(i,k2,j), ' va:', va(i,k2,j), ' ws:', ws(i,k2,j),' ta:',         &
              ta(i,k2,j), ' press:', press(i,k2,j), ' zg:', zg(i,k2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    hur', hur(i,k2,j), ' hus:', hus(i,k2,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    cape:', cape(i,j), ' cin:', cin(i,j),' zlfc:', zlfc(i,j), ' plfc:',     &
              plfc(i,j), 'lidx:', lidx(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    fog:', fog(i,j), ' tfog:', tfog(i,j), ' vis:', vis(i,j), ' vismin:',     &
              fogvisbltymin(i,j), ' vismax:', fogvisbltymax(i,j), ' vismean:', fogvisbltymean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tds:', tds(i,j), ' min:', tdsmin(i,j), ' max:', tdsmax(i,j),             &
              ' mean:', tdsmean(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    tws:', tws(i,j), ' min:', twsmin(i,j), ' max:', twsmax(i,j),             &
              ' mean:', twsmean(i,j)
            CALL wrf_debug(75,msg)
!#endif
!#if CDXWRF >=4
            WRITE(msg,*) '    acdh:', wbacdh(i,j), ' dhl:', wbacdhl(i,j), 'dhm:', wbacdhm(i,j),       &
              'dhh:', wbacdhh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    WB acpw:', wbacpw(i,j), 'acpwc:', wbacpwc(i,j), 'acpwr:', wbacpwr(i,j), &
              'acpws:', wbacpws(i,j), 'acpwi:', wbacpwi(i,j), 'acpwg:', wbacpwg(i,j), 'acpwh:',       &
               wbacpwh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    WB acf:', wbacf(i,j), 'acfc:', wbacfc(i,j), 'acfr:', wbacfr(i,j),       &
              'acfs:', wbacfs(i,j), 'acfi:', wbacfi(i,j), 'acfg:', wbacfg(i,j), 'acfh:', wbacfh(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    WB acz:', wbacz(i,j), 'aczc:', wbaczc(i,j), 'aczr:', wbaczr(i,j),       &
              'acfs:', wbaczs(i,j), 'acfi:', wbaczi(i,j), 'aczg:', wbaczg(i,j), 'aczh:', wbaczh(i,j)
            CALL wrf_debug(75,msg)
!#endif
!#endif
          END IF

          ijdbg = .FALSE.
        ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO

  END IF

  END SUBROUTINE cordex_output_calc

!#ifdef CDXWRF
! CDXWRF - high frequency
!! 
  SUBROUTINE cordex_hf_output_calc(                                                                   &
                      is_restart, is_output_time, cdxH, nsteps,                                       &
                      dt, xtime, curr_secs2,                                                          &

                   ! Input
                      lon, lat,                                                                       &
                      timedbg,                                                                        &
                      londbg, latdbg,                                                                 &
                      idbg, jdbg,                                                                     &
                      qr, qs, qg,                                                                     &
                      t,                                                                              &
                      prc, prnc, prsh,                                                                &
                      rho,                                                                            &
                   ! Output
!#if CDXWRF>=1
                      flxpr, colmax,                                                                  &
!#endif
                      ids,ide, jds,jde, kds,kde,                                                      &
                      ims,ime, jms,jme, kms,kme,                                                      &
                      ips,ipe, jps,jpe, kps,kpe,                                                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles                                   &
                    )

!----------------------------------------------------------------------
! Main subroutine to compute CORDEX high-frequency variables

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 
  USE module_diagvar_cordex

  IMPLICIT NONE

!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!-- curr_secs2    current time in seconds since simulation restart
!-- nsteps        amount of time-steps between outputs
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles

!
!-----------
! is_restart: whether if simulation is a restart
! is_output_time: whether if this iteration is an output time-step
! cdxH: period of time [minute] of calculations between outputs
!
! ------- INPUT vars -------
! lon: longitude
! lat: latitude
! timedbg: frecuency in minutes of time debugging
! londbg, latdbg: coordinates of the debugging grid point (-999. if not set-up)
! idbg, jdbg: grid point coordinates of the debugging grid point
! qr: rain mixing ratio [kgkg-1]
! qs: snow mixing ratio [kgkg-1]
! qg: graupel mixing ratio [kgkg-1]
! t: 3D air temperature [K]
! rho: air density [kgm-3]
! prc: convective (from cumulus scheme) precipitation [kgm-2]
! prnc: non-convective (from micro-physics scheme) precipitation [kgm-2]
! prsh: shallow-cumulus (from shallow-cumulus scheme) precipitation [kgm-2]

!-------- OUTPUT vars ------- [defined at registry.cordex] and included on Registry.EM_COMMON on auxhist8
! flxpr: precipitation flux [kgm-2s-1]
! colmax: maximum radar reflectivity in the column [dBz]

!!!!!!! Structure
! 1.: Compute such variables needed for internal accumulations, extremes
! 2.: Compute the variables from clouds (only actualized at every radt)
! 3.: If the time step is the first after writting the auxhist8 file inittialize the variables of (1., 2.)
! 4.: If the time step is exactly the previous one before the writting of the auxhist8 file:
!   a: Compute all other variables 
!   b: Finish calculation of 1., 2. variables 

!======================================================================

  INTEGER,      INTENT(in)                               ::                                           &
                                            ids,ide, jds,jde, kds,kde,                                &
                                            ims,ime, jms,jme, kms,kme,                                &
                                            ips,ipe, jps,jpe, kps,kpe,                                &
                                            kts,kte,                                                  &
                                            num_tiles


  INTEGER, DIMENSION(num_tiles), INTENT(in)              :: i_start, i_end, j_start, j_end

  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(in)   :: qr, qs, qg, t, rho
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: lon, lat
  INTEGER, INTENT(in)                                    :: timedbg
  REAL, INTENT(in)                                       :: londbg, latdbg
  INTEGER, INTENT(in)                                    :: idbg, jdbg
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(in)           :: prc, prnc, prsh

  REAL, INTENT(in)                                       :: DT, XTIME
  REAL, INTENT(in)                                       :: curr_secs2
  INTEGER, INTENT(in)                                    :: cdxH
  LOGICAL, INTENT(in)                                    :: is_restart, is_output_time
  INTEGER, INTENT(inout)                                 :: nsteps
!#if CDXWRF>=1
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(inout)        :: flxpr, colmax
!#endif

! LOCAL  VAR

  INTEGER                                                :: i, j, k, m, its, ite, jts, jte, ij
  INTEGER                                                :: idp, jdp, dimz, dimz1, dz1
  INTEGER                                                :: i1, i_1, j1, j_1, k2, tdbg
  INTEGER, DIMENSION(num_tiles)                          :: i2, j2

  REAL                                                   :: xtimep
  REAL                                                   :: prv

  LOGICAL, EXTERNAL                                      :: wrf_dm_on_monitor
  CHARACTER(len=256)                                     :: msg
  LOGICAL                                                :: ijdbg, cdone
  INTEGER                                                :: dbg_level

  fname = 'cordex_hf_output_calc'

  ! Getting the overall debug level
  CALL get_wrf_debug_level( dbg_level ) 
  cdone = .FALSE.

  ! timedbg
  !  when time-debug in minutes
  tdbg = MOD(xtime, timedbg*1.)

  ! L. Fita: NOTE about efficiency
  !   I am not sure if this is the more efficient way:
  !     1.- IF --> 2.- repeat DO
  !     1.- DO -->  2.- IF

  dimz = kme - kms + 1
  dimz1 = dimz + 1
  dz1 = dimz - 1

  ! For test purposes
  IF ((londbg == -999.) .OR. (latdbg == -999.)) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      i2(ij) = INT(0.5*(i_start(ij) + i_end(ij)))
      j2(ij) = INT(0.5*(j_start(ij) + j_end(ij)))
    END DO
    !$OMP END PARALLEL DO
  ELSE
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      i2(ij) = idbg
      j2(ij) = jdbg
    END DO
    !$OMP END PARALLEL DO
  END IF
  k2 = 0.5*(kms+kme)

  ! Computing accumulative variables 
  ijdbg = .FALSE.
  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij )
  DO ij = 1 , num_tiles
    DO i = i_start(ij), i_end(ij)
      DO j = j_start(ij), j_end(ij)
        IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.

        ! Avoiding seg faults
        IF (curr_secs2 > 0. ) THEN
          WRITE(msg,*) '  ' // TRIM(fname) // ': computing colmax'

          IF (ijdbg .AND. tdbg==0) CALL wrf_debug(50,msg)
          ! Total precipitation
          prv = prc(i,j) + prnc(i,j) + prsh(i,j)

        END IF


        ijdbg = .FALSE.
      END DO
    END DO
  END DO
  !$OMP END PARALLEL DO

  IF (( MOD(NINT(curr_secs2/dt),NINT(cdxH*60./dt)) == 0) ) THEN
    ! Initialization of accumulated variables between outputs
    IF  ( wrf_dm_on_monitor() ) THEN
      CALL wrf_debug(0, '  ' // TRIM(fname) // ': Re-initializing accumulation arrays')
    ENDIF

    nsteps = 1

    ijdbg = .FALSE.
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.

!#if CDXWRF>=1
          ! Precipitations
          flxpr(i,j) = prc(i,j) + prnc(i,j) + prsh(i,j)
!#endif

          IF (dbg_level >= 75 .AND. ijdbg .AND. tdbg==0) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ' ',i,' ,',j,' : first value checks _______' 
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    longitude:', lon(i,j),' latitude:',lat(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    nsteps:', nsteps
            CALL wrf_debug(75,msg)
!#if CDXWRF>=1
            WRITE(msg,*)'    prc:', prc(i,j), ' prnc:', prnc(i,j), ' prsh:', prsh(i,j), ' flxpr:',    &
              flxpr(i,j)
!#endif
            CALL wrf_debug(75,msg)
          END IF

          ijdbg = .FALSE.
        ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO

  ELSE
    xtimep = xtime + dt/60.   ! value at end of timestep for time info
    nsteps=nsteps+1
    ijdbg = .FALSE.
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.

!#if CDXWRF>=1
          ! Precipitations
          flxpr(i,j) = flxpr(i,j) + prc(i,j) + prnc(i,j) + prsh(i,j)
!#endif
          IF (dbg_level >= 75 .AND. ijdbg .AND. tdbg==0) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ' ',i,' ,',j,' : intermediate checks _______' 
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    longitude:', lon(i,j),' latitude:',lat(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    nsteps:', nsteps
            CALL wrf_debug(75,msg)
!#if CDXWRF>=1
            WRITE(msg,*)'    prc:', prc(i,j), ' prnc:', prnc(i,j), ' prsh:', prsh(i,j), ' flxpr:',    &
              flxpr(i,j)
!#endif
            CALL wrf_debug(75,msg)
          END IF
          ijdbg = .FALSE.
        ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO

  END IF

  ! Some of CORDEX variables tend to be quite computational intensive, thus, just compute them when
  !   it is necessary (only valid for instantaneous variables)
  ijdbg = .FALSE.
  IF (is_output_time) THEN
    CALL wrf_debug(10,'  ' // TRIM(fname) // ': Computing instantaneous CORDEX variables')

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , num_tiles
      DO i = i_start(ij), i_end(ij)
        DO j = j_start(ij), j_end(ij)
          IF (ij == 1 .AND. i == i2(ij) .AND. j == j2(ij)) ijdbg = .TRUE.
          i1 = MIN(i+1,i_end(ij))
          i_1 = MAX(i-1,i_start(ij))
          j1 = MIN(j+1,j_end(ij))
          j_1 = MAX(j-1,j_start(ij))

!#if CDXWRF>=1
          ! colmax
          CALL var_colmax1D(dimz, rho(i,:,j), t(i,:,j), qr(i,:,j), qs(i,:,j), qg(i,:,j), colmax(i,j))

          ! Precipitation
          flxpr(i,j) = flxpr(i,j)/(dt*nsteps)
!#endif
          IF (dbg_level >= 75 .AND. ijdbg .AND. tdbg==0) THEN
            WRITE(msg,*) '  ' // TRIM(fname) // ' ',i,' ,',j,' : final checks _______' 
            CALL wrf_debug(75,msg)
            WRITE(msg,*) '    nsteps:', nsteps
            CALL wrf_debug(75,msg)
!#if CDXWRF>=1
            WRITE(msg,*)'    prc:', prc(i,j), ' prnc:', prnc(i,j), ' prsh:', prsh(i,j), ' flxpr:',    &
              flxpr(i,j)
            CALL wrf_debug(75,msg)
            WRITE(msg,*)'    colmax:', colmax(i,j)
            CALL wrf_debug(75,msg)
!#endif
          END IF

          ijdbg = .FALSE.
        ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO

  END IF

  END SUBROUTINE cordex_hf_output_calc
!#endif

END MODULE module_diag_cordex
