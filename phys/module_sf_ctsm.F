module module_sf_ctsm

  implicit none
  private

#ifdef WRF_USE_CTSM

  public :: ctsm_init
  public :: ctsm_run

  ! FIXME(wjs, 2020-01-01) Introduce a lilac_kinds module, and get this from there
  integer, parameter :: r8 = selected_real_kind(12)  ! 8 byte real

contains

  !-----------------------------------------------------------------------
  subroutine get_num_points(ide, jde, its, ite, jts, jte, &
       num_points, ite_limited, jte_limited)
    ! Return the number of points owned by this task
    !
    ! If ite_limited and/or jte_limited are provided, then return those values, too.
    ! These are the task end indices on the mass point grid.
    integer, intent(in) :: ide  ! domain end index, i
    integer, intent(in) :: jde  ! domain end index, j
    integer, intent(in) :: its  ! task start index, i
    integer, intent(in) :: ite  ! task end index, i
    integer, intent(in) :: jts  ! task start index, j
    integer, intent(in) :: jte  ! task end index, j
    integer, intent(out) :: num_points  ! number of points owned by this task
    integer, optional, intent(out) :: ite_limited  ! task end index on the mass point grid, i
    integer, optional, intent(out) :: jte_limited  ! task end index on the mass point grid, j

    integer :: my_ite_limited  ! task end index on the mass point grid, i
    integer :: my_jte_limited  ! task end index on the mass point grid, j

    ! The very last index in both row & column space is just used on the momentum grid.
    ! Here we are just working with the mass point grid, so we need to ignore that last
    ! index.
    my_ite_limited = min(ite, ide-1)
    my_jte_limited = min(jte, jde-1)

    num_points = ((my_ite_limited - its + 1) * (my_jte_limited - jts + 1))

    if (present(ite_limited)) then
       ite_limited = my_ite_limited
    end if
    if (present(jte_limited)) then
       jte_limited = my_jte_limited
    end if

  end subroutine get_num_points

  subroutine create_gindex(ide, jde, its, ite, jts, jte, gindex)
    ! Create a gindex array on each task. This gives the list of global indices owned by
    ! each processor, on the mass point grid.
    integer, intent(in) :: ide  ! domain end index, i
    integer, intent(in) :: jde  ! domain end index, j
    integer, intent(in) :: its  ! task start index, i
    integer, intent(in) :: ite  ! task end index, i
    integer, intent(in) :: jts  ! task start index, j
    integer, intent(in) :: jte  ! task end index, j
    integer, allocatable, intent(out) :: gindex(:)

    integer :: ite_limited  ! task end index on the mass point grid, i
    integer :: jte_limited  ! task end index on the mass point grid, j
    integer :: num_points
    integer :: i, j, n

    call get_num_points( &
         ide=ide, jde=jde, &
         its=its, ite=ite, &
         jts=jts, jte=jte, &
         num_points = num_points, &
         ite_limited = ite_limited, &
         jte_limited = jte_limited)

    allocate(gindex(num_points))

    n = 0
    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          ! In the following, note that we use ide-1 rather than ide for the same reason
          ! that we need ite_limited: ide gives the domain end index on the momentum grid,
          ! but here we're just dealing with the mass point grid, which has one less point
          ! in each direction.
          gindex(n) = (j-1)*(ide-1) + i
       end do
    end do
  end subroutine create_gindex
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine convert_2d_to_1d (ide, jde, ims, ime, jms, jme, its, ite, jts, jte, var_2d, var_1d)
    ! Convert a 2-d native WRF array to a 1-d array appropriate for LILAC
    !
    ! Allocates var_1d here

    use module_wrf_error

    ! input/output variables
    integer , intent(in)  :: ide ! domain end index, i
    integer , intent(in)  :: jde ! domain end index, j
    integer , intent(in)  :: ims ! memory start index (includes halo cells), i
    integer , intent(in)  :: ime ! memory end index (includes halo cells), i
    integer , intent(in)  :: jms ! memory start index (includes halo cells), j
    integer , intent(in)  :: jme ! memory end index (includes halo cells), j
    integer , intent(in)  :: its ! task start index, i
    integer , intent(in)  :: ite ! task end index, i
    integer , intent(in)  :: jts ! task start index, j
    integer , intent(in)  :: jte ! task end index, j
    real    , intent(in)  :: var_2d(ims: , jms: )
    real(r8), allocatable, intent(out) :: var_1d(:)

    ! local variables
    integer :: ite_limited  ! task end index on the mass point grid, i
    integer :: jte_limited  ! task end index on the mass point grid, j
    integer :: num_points
    integer :: i, j, n

    if (.not. all(ubound(var_2d) == [ime, jme])) then
       call wrf_error_fatal('convert_2d_to_1d: incorrect bounds for var_2d')
    end if

    call get_num_points( &
         ide=ide, jde=jde, &
         its=its, ite=ite, &
         jts=jts, jte=jte, &
         num_points = num_points, &
         ite_limited = ite_limited, &
         jte_limited = jte_limited)

    allocate (var_1d (num_points))

    n = 0

    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          var_1d(n) = var_2d(i,j)
       end do
    end do

  end subroutine convert_2d_to_1d
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine convert_1d_to_2d (ide, jde, ims, ime, jms, jme, its, ite, jts, jte, var_1d, var_2d)
    ! Convert a 1-d array from lilac to a 2-d native WRF array

    use module_wrf_error

    ! input/output variables
    integer , intent(in)  :: ide ! domain end index, i
    integer , intent(in)  :: jde ! domain end index, j
    integer , intent(in)  :: ims ! memory start index (includes halo cells), i
    integer , intent(in)  :: ime ! memory end index (includes halo cells), i
    integer , intent(in)  :: jms ! memory start index (includes halo cells), j
    integer , intent(in)  :: jme ! memory end index (includes halo cells), j
    integer , intent(in)  :: its ! task start index, i
    integer , intent(in)  :: ite ! task end index, i
    integer , intent(in)  :: jts ! task start index, j
    integer , intent(in)  :: jte ! task end index, j
    real(r8), intent(in)  :: var_1d(:)
    real    , intent(inout) :: var_2d(ims: , jms: )

    ! local variables
    integer :: ite_limited  ! task end index on the mass point grid, i
    integer :: jte_limited  ! task end index on the mass point grid, j
    integer :: num_points
    integer :: i, j, n

    if (.not. all(ubound(var_2d) == [ime, jme])) then
       call wrf_error_fatal('convert_1d_to_2d: incorrect bounds for var_2d')
    end if

    call get_num_points( &
         ide=ide, jde=jde, &
         its=its, ite=ite, &
         jts=jts, jte=jte, &
         num_points = num_points, &
         ite_limited = ite_limited, &
         jte_limited = jte_limited)

    if (.not. (ubound(var_1d, 1) == num_points)) then
       call wrf_error_fatal('convert_1d_to_2d: incorrect size of var_1d')
    end if

    n = 0

    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          var_2d(i,j) = var_1d(n)
       end do
    end do

  end subroutine convert_1d_to_2d
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine ctsm_init( &
       ids, ide, jds, jde, &
       ims, ime, jms, jme, &
       its, ite, jts, jte, &
       dt, xlat, xlong)
    ! Initialize CTSM via LILAC

    use module_wrf_error
    use lilac_mod, only : lilac_init

    integer, intent(in) :: ids
    integer, intent(in) :: ide
    integer, intent(in) :: jds
    integer, intent(in) :: jde
    integer, intent(in) :: ims
    integer, intent(in) :: ime
    integer, intent(in) :: jms
    integer, intent(in) :: jme
    integer, intent(in) :: its
    integer, intent(in) :: ite
    integer, intent(in) :: jts
    integer, intent(in) :: jte

    real, intent(in) :: dt       ! time step (seconds)
    real, intent(in) :: xlat(ims: , jms: )  ! latitudes (deg)
    real, intent(in) :: xlong(ims: , jms: ) ! longitudes (deg)

    integer :: comp_comm
    integer , allocatable :: atm_global_index(:)
    real                  :: xlong_0360(ims:ime, jms:jme)
    real(r8) , allocatable    :: atm_lons(:)
    real(r8) , allocatable    :: atm_lats(:)

    character(len=128) :: atm_calendar
    character(len=128) :: atm_starttype
    integer            :: atm_timestep
    integer            :: atm_start_year ! (yyyy)
    integer            :: atm_start_mon  ! (mm)
    integer            :: atm_start_day
    integer            :: atm_start_hour
    integer            :: atm_start_minute
    integer            :: atm_start_second ! seconds after the minute
    integer            :: atm_start_secs_since_midnight ! total seconds since midnight
    integer            :: atm_stop_year  ! (yyyy)
    integer            :: atm_stop_mon   ! (mm)
    integer            :: atm_stop_day
    integer            :: atm_stop_hour
    integer            :: atm_stop_minute
    integer            :: atm_stop_second ! seconds after the minute
    integer            :: atm_stop_secs_since_midnight ! total seconds since midnight

    integer            :: atm_global_nx
    integer            :: atm_global_ny

    character(len=512) :: message

    ! TODO(wjs, 2019-12-31) Is this the correct way to get the communicator?
    call wrf_get_dm_communicator(comp_comm)

    call create_gindex(ide, jde, its, ite, jts, jte, atm_global_index)

    ! Convert longitude from -180..180 to 0..360
    xlong_0360 = xlong
    where (xlong_0360 < 0)
       xlong_0360 = 360. + xlong_0360
    end where

    ! reshape lats and lons to 1d for lilac
    call convert_2d_to_1d ( &
         ide=ide, jde=jde, &
         ims=ims, ime=ime, &
         jms=jms, jme=jme, &
         its=its, ite=ite, &
         jts=jts, jte=jte, &
         var_2d=xlong_0360, var_1d=atm_lons)
    call convert_2d_to_1d ( &
         ide=ide, jde=jde, &
         ims=ims, ime=ime, &
         jms=jms, jme=jme, &
         its=its, ite=ite, &
         jts=jts, jte=jte, &
         var_2d=xlat, var_1d=atm_lats)

    atm_global_nx = ide-ids
    atm_global_ny = jde-jds

    ! calendar stuff

    ! TODO(wjs, 2019-12-31) Is this the appropriate way to get the start and end times?
    !
    ! Some specific questions:
    ! - Should we use some passed-in argument rather than getting the namelist values
    !   directly here?
    ! - nl_get_start_year vs. nl_get_simulation_start_year (and similar for other units):
    !   should we be using nl_get_simulation_* here?
    ! - Is it correct to form the total seconds from hour, minute & second like this?
    ! - I'm using 1 for the first argument; I think this gives the domain id; is this an
    !   okay thing to do for now, or can we somehow determine the actual domain id?
    call nl_get_start_year(1, atm_start_year)
    call nl_get_start_month(1, atm_start_mon)
    call nl_get_start_day(1, atm_start_day)
    call nl_get_start_hour(1, atm_start_hour)
    call nl_get_start_minute(1, atm_start_minute)
    call nl_get_start_second(1, atm_start_second)
    atm_start_secs_since_midnight = 60*(60*atm_start_hour + atm_start_minute) + atm_start_second
    write(message, '("CTSM start time: ", I4, 1X, I2, 1X, I2, 1X, I5)') &
         atm_start_year, atm_start_mon, atm_start_day, atm_start_secs_since_midnight
    call wrf_message(message)
    call nl_get_end_year(1, atm_stop_year)
    call nl_get_end_month(1, atm_stop_mon)
    call nl_get_end_day(1, atm_stop_day)
    call nl_get_end_hour(1, atm_stop_hour)
    call nl_get_end_minute(1, atm_stop_minute)
    call nl_get_end_second(1, atm_stop_second)
    atm_stop_secs_since_midnight = 60*(60*atm_stop_hour + atm_stop_minute) + atm_stop_second
    write(message, '("CTSM stop time: ", I4, 1X, I2, 1X, I2, 1X, I5)') &
         atm_stop_year, atm_stop_mon, atm_stop_day, atm_stop_secs_since_midnight
    call wrf_message(message)

    atm_calendar      = 'GREGORIAN'
    atm_starttype     = 'startup'

    ! TODO(wjs, 2019-12-31) Is there a way to directly get dt as an integer, rather than
    ! relying on converting this real-valued dt to an integer?
    atm_timestep      = nint(dt)
    if (abs(atm_timestep - dt) > (1.e-5 * dt)) then
       call wrf_error_fatal('ctsm_init: expect dt representable as integer')
    end if

    call lilac_init(  &
         mpicom           = comp_comm,        &
         atm_global_index = atm_global_index, &
         ! FIXME(wjs, 2020-01-01) the following conversions to default real won't be
         ! needed once we fix lilac to expect double-precision
         atm_lons         = real(atm_lons),         &
         atm_lats         = real(atm_lats),         &
         atm_global_nx    = atm_global_nx,    &
         atm_global_ny    = atm_global_ny,    &
         atm_calendar     = atm_calendar,     &
         atm_timestep     = atm_timestep,     &
         atm_start_year   = atm_start_year,   &
         atm_start_mon    = atm_start_mon,    &
         atm_start_day    = atm_start_day,    &
         atm_start_secs   = atm_start_secs_since_midnight, &
         atm_stop_year    = atm_stop_year,    &
         atm_stop_mon     = atm_stop_mon,     &
         atm_stop_day     = atm_stop_day,     &
         atm_stop_secs    = atm_stop_secs_since_midnight,  &
         starttype_in     = atm_starttype)

  end subroutine ctsm_init
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine ctsm_run( &
       ! bounds
       ids, ide, jds, jde, &
       ims, ime, jms, jme, kms, &
       its, ite, jts, jte, &

       ! general information
       dt, &

       ! atm -> lnd variables
       dz8w, ht, u_phy, v_phy, p8w, t_phy, qv_curr, &
       rainbl, sr, &
       glw, swvisdir, swvisdif, swnirdir, swnirdif, &

       ! lnd -> atm variables
       tsk, qsfc, albedo, snow, &
       hfx, lh, qfx)

    use lilac_mod, only : lilac_run

    integer, intent(in) :: ids
    integer, intent(in) :: ide
    integer, intent(in) :: jds
    integer, intent(in) :: jde
    integer, intent(in) :: ims
    integer, intent(in) :: ime
    integer, intent(in) :: jms
    integer, intent(in) :: jme
    integer, intent(in) :: kms
    integer, intent(in) :: its
    integer, intent(in) :: ite
    integer, intent(in) :: jts
    integer, intent(in) :: jte

    real, intent(in) :: dt ! timestep [s]

    real, intent(in) :: dz8w(ims: , kms: , jms: ) ! thickness of atmo layers [m]
    real, intent(in) :: ht(ims: , jms: )  ! terrain height [m]
    real, intent(in) :: u_phy(ims: , kms: , jms: ) ! 3D U wind component [m/s]
    real, intent(in) :: v_phy(ims: , kms: , jms: ) ! 3D V wind component [m/s]
    real, intent(in) :: p8w(ims: , kms: , jms: ) ! 3D pressure, valid at interface [Pa]
    real, intent(in) :: t_phy(ims: , kms: , jms: ) ! 3D atmospheric temperature valid at mid-levels [K]
    real, intent(in) :: qv_curr(ims: , kms: , jms: ) ! 3D water vapor mixing ratio [kg/kg_dry]
    real, intent(in) :: rainbl(ims: , jms: ) ! total input precipitation [mm]
    real, intent(in) :: sr(ims: , jms: ) ! frozen precipitation ratio [-]
    real, intent(in) :: glw(ims: , jms: ) ! longwave down at surface [W m-2]
    real, intent(in) :: swvisdir(ims: , jms: ) ! vis direct beam solar rad onto surface [W m-2]
    real, intent(in) :: swvisdif(ims: , jms: ) ! vis diffuse solar rad onto surface [W m-2]
    real, intent(in) :: swnirdir(ims: , jms: ) ! nir direct beam solar rad onto surface [W m-2]
    real, intent(in) :: swnirdif(ims: , jms: ) ! nir diffuse solar rad onto surface [W m-2]

    real, intent(inout) :: tsk(ims: , jms: ) ! surface temperature [K]
    real, intent(inout) :: qsfc(ims: , jms: ) ! bulk surface specific humidity
    real, intent(inout) :: albedo(ims: , jms: ) ! total grid albedo [-]
    real, intent(inout) :: snow(ims: , jms: ) ! snow water equivalent [mm]
    real, intent(inout) :: hfx(ims: , jms: ) ! sensible heat flux [W m-2]
    real, intent(inout) :: lh(ims: , jms: ) ! latent heat flux [W m-2]
    real, intent(inout) :: qfx(ims: , jms: ) ! latent heat flux [kg s-1 m-2]

    integer :: i, j

    real :: zlvl(ims:ime, jms:jme)   ! mid-point of bottom atm layer [m]
    real :: forc_u(ims:ime, jms:jme) ! u wind component, bottom atm layer [m/s]
    real :: forc_v(ims:ime, jms:jme) ! v wind component, bottom atm layer [m/s]
    real :: pbot(ims:ime, jms:jme)   ! surface pressure [Pa]
    real :: forc_th(ims:ime, jms:jme) ! potential temperature [K]
    real :: forc_t(ims:ime, jms:jme) ! temperature [K]
    real :: forc_q(ims:ime, jms:jme) ! specific humidity [kg/kg]
    real :: precip_rate(ims:ime, jms:jme) ! total precipitation rate [mm/s]
    real :: rain_convective_rate(ims:ime, jms:jme) ! rate of convective rain [mm/s]
    real :: rain_largescale_rate(ims:ime, jms:jme) ! rate of large-scale rain [mm/s]
    real :: snow_convective_rate(ims:ime, jms:jme) ! rate of convective snow [mm/s]
    real :: snow_largescale_rate(ims:ime, jms:jme) ! rate of large-scale snow [mm/s]

    real :: qref(ims:ime, jms:jme) ! 2m surface specific humidity calculated by CTSM [kg/kg]
    real :: albedo_visdir(ims:ime, jms:jme) ! albedo calculated by CTSM, visible direct [-]
    real :: albedo_visdif(ims:ime, jms:jme) ! albedo calculated by CTSM, visible diffuse [-]
    real :: albedo_nirdir(ims:ime, jms:jme) ! albedo calculated by CTSM, near IR direct [-]
    real :: albedo_nirdif(ims:ime, jms:jme) ! albedo calculated by CTSM, near IR diffuse [-]
    real :: snow_m(ims:ime, jms:jme) ! snow water equivalent in meters [m]

    ! ------------------------------------------------------------------------
    ! Calculate derived variables, and 2-d versions of 3-d fields
    ! ------------------------------------------------------------------------

    ! dz8w = thickness of full levels; we want the mid-point of the bottom-most level
    zlvl(:,:) = 0.5 * dz8w(:,1,:)

    forc_u(:,:) = u_phy(:,1,:)
    forc_v(:,:) = v_phy(:,1,:)

    ! TODO(wjs, 2020-01-01) Is this correct? Or does this give the pressure at the
    ! interface, whereas we may want the pressure at the mid-point???
    pbot(:,:) = p8w(:,1,:)

    ! TODO(wjs, 2020-01-01) For now, I'm setting potential temperature equal to
    ! temperature. Line 58470 of module_sf_clm has:
    !
    !     clm_a2l%forc_th(g)  = clm_a2l%forc_t(g) * (clm_a2l%forc_psrf(g) &
    !          / clm_a2l%forc_pbot(g))**(rair/cpair)
    !
    ! but it looks like it is using the same WRF variable for both forc_psrf and
    ! forc_pbot, so the adjustment from forc_t to forc_th isn't actually accomplishing
    ! anything. Should we do some sort of adjustment here?
    forc_th(:,:) = t_phy(:,1,:)

    forc_t(:,:) = t_phy(:,1,:)

    ! Convert from mixing ratio to specific humidity
    ! TODO(wjs, 2020-01-01) Confirm that this is correct
    forc_q(:,:) = qv_curr(:,1,:)/(1.0 + qv_curr(:,1,:))

    ! Separate total precip into rain and snow. Arbitrarily assign all precipitation to
    ! convective (CTSM requires separate convective vs. large-scale precipitation, but
    ! then just adds the two together).
    precip_rate(:,:) = rainbl(:,:)/dt
    snow_convective_rate(:,:) = precip_rate(:,:) * sr(:,:)
    snow_largescale_rate(:,:) = 0.
    rain_convective_rate(:,:) = precip_rate(:,:) * (1. - sr(:,:))
    rain_largescale_rate(:,:) = 0.

    ! ------------------------------------------------------------------------
    ! Export data to ctsm via lilac
    ! ------------------------------------------------------------------------

    call export_to_lilac('Sa_z', zlvl)
    call export_to_lilac('Sa_topo', ht)
    call export_to_lilac('Sa_u', forc_u)
    call export_to_lilac('Sa_v', forc_v)
    call export_to_lilac('Sa_ptem', forc_th)
    call export_to_lilac('Sa_pbot', pbot)
    call export_to_lilac('Sa_tbot', forc_t)
    call export_to_lilac('Sa_shum', forc_q)
    call export_to_lilac('Faxa_lwdn', glw)
    call export_to_lilac('Faxa_rainc', rain_convective_rate)
    call export_to_lilac('Faxa_rainl', rain_largescale_rate)
    call export_to_lilac('Faxa_snowc', snow_convective_rate)
    call export_to_lilac('Faxa_snowl', snow_largescale_rate)
    call export_to_lilac('Faxa_swndr', swnirdir)
    call export_to_lilac('Faxa_swvdr', swvisdir)
    call export_to_lilac('Faxa_swndf', swnirdif)
    call export_to_lilac('Faxa_swvdf', swvisdif)

    ! ------------------------------------------------------------------------
    ! Run ctsm via lilac
    ! ------------------------------------------------------------------------

    ! FIXME(wjs, 2020-01-01) Use correct values for restart and stop alarms
    call lilac_run( &
         restart_alarm_is_ringing = .false., &
         stop_alarm_is_ringing = .false.)

    ! ------------------------------------------------------------------------
    ! Import data from ctsm via lilac
    ! ------------------------------------------------------------------------

    call import_from_lilac('Sl_t', tsk)

    ! TODO(wjs, 2020-01-02) Do we need to retrieve Sl_tref (2m surface air temperature)?
    ! If so, what variable should that go into?

    ! TODO(wjs, 2020-01-02) Confirm: Does QSFC correspond to Sl_qref (2m specific
    ! humidity)? And is this the correct conversion from CTSM's units to WRF's units?
    call import_from_lilac('Sl_qref', qref)
    qsfc(its:ite, jts:jte) = qref(its:ite, jts:jte) / (1. - qref(its:ite, jts:jte))

    ! TODO(wjs, 2020-01-02) Do we need to retrieve Sl_qref (2m specific humidity)? If so,
    ! what variable should that go into? QSFC?

    ! TODO(wjs, 2020-01-02) Note that we retrieve all four components of albedo, but only
    ! deliver the total albedo back to WRF. Is it sufficient to send total albedo, or
    ! should we also send the four components? In clmdrv, the individual components are
    ! alswvisdir, alswvisdif, alswnirdir, alswnirdif. These come from alswvisdir,
    ! alswvisdif, alswnirdir, alswnirdif in surface_driver, but it looks like these aren't
    ! set by most surface schemes. So, okay to only send total albedo?
    call import_from_lilac('Sl_avsdr', albedo_visdir)
    call import_from_lilac('Sl_anidr', albedo_nirdir)
    call import_from_lilac('Sl_avsdf', albedo_visdif)
    call import_from_lilac('Sl_anidf', albedo_nirdif)
    call calculate_total_albedo( &
         ims=ims, ime=ime, jms=jms, jme=jme, &
         its=its, ite=ite, jts=jts, jte=jte, &
         albedo_visdir = albedo_visdir, &
         albedo_nirdir = albedo_nirdir, &
         albedo_visdif = albedo_visdif, &
         albedo_nirdif = albedo_nirdif, &
         swvisdir      = swvisdir, &
         swnirdir      = swnirdir, &
         swvisdif      = swvisdif, &
         swnirdif      = swnirdif, &
         albedo        = albedo)

    ! TODO(wjs, 2020-01-02) Do we need to provide any other snow-related variables
    ! (physical snow depth, snow cover fraction, etc.)? And is this one (SWE) actually
    ! needed?
    call import_from_lilac('Sl_snowh', snow_m)
    snow(its:ite, jts:jte) = snow_m(its:ite, jts:jte) * 1000.

    ! TODO(wjs, 2020-01-02) Do we need to retrieve Sl_u10 (10-m wind speed)? If so, what
    ! variable should that go into?

    ! TODO(wjs, 2020-01-02) Do we need to retrieve Fall_taux and Fall_tauy (wind
    ! stresses)? I see taux and tauy in surface_driver, but they appear to only be used
    ! in sf_gfdl.

    ! TODO(wjs, 2020-01-02) Check sign on these
    call import_from_lilac('Fall_sen', hfx)
    call import_from_lilac('Fall_lat', lh)
    call import_from_lilac('Fall_evap', qfx)

    ! TODO(wjs, 2020-01-02) Do we need to retrieve Fall_lwup (longwave radiation)? If so,
    ! what variable should that go into? Or do we need to set EMISS instead?

    ! TODO(wjs, 2020-01-02) Do we need to retrieve swnet (solar rad absorbed)? If so,
    ! what variable should that go into? (It seems like that could be another way to
    ! calculate albedo. I'm not sure if the results would differ in any way.)

    ! TODO(wjs, 2020-01-02) Should we retrieve any of the dust fluxes (flxdst1, flxdst2,
    ! flxdst3, flxdst4)?

    ! TODO(wjs, 2020-01-02) What other variables need to be set in WRF that we are not
    ! setting here? For example:
    ! - ZNT and/or Z0?
    ! - GRDFLX?
    ! - EMISS?
    ! - others?

  contains

    subroutine export_to_lilac(var_name, var_2d)
      ! Reshape var_2d to 1d for LILAC, then set the appropriate atm2lnd variable in LILAC

      use lilac_atmcap, only : lilac_atmcap_atm2lnd

      character(len=*), intent(in) :: var_name  ! name of variable in LILAC
      real, intent(in) :: var_2d(ims: , jms: )

      real(r8), allocatable :: var_1d(:)

      call convert_2d_to_1d( &
           ide=ide, jde=jde, &
           ims=ims, ime=ime, &
           jms=jms, jme=jme, &
           its=its, ite=ite, &
           jts=jts, jte=jte, &
           var_2d=var_2d, var_1d=var_1d)

      call lilac_atmcap_atm2lnd( &
           fldname = var_name, &
           data = var_1d)

    end subroutine export_to_lilac

    subroutine import_from_lilac(var_name, var_2d)
      ! Get the appropriate lnd2atm variable from LILAC, then reshape 1d LILAC variable to
      ! fill var_2d

      use lilac_atmcap, only : lilac_atmcap_lnd2atm

      character(len=*), intent(in) :: var_name  ! name of variable in LILAC
      real, intent(inout) :: var_2d(ims: , jms: )

      real(r8), allocatable :: var_1d(:)
      integer :: num_points

      call get_num_points( &
           ide=ide, jde=jde, &
           its=its, ite=ite, &
           jts=jts, jte=jte, &
           num_points = num_points)

      allocate(var_1d(num_points))

      call lilac_atmcap_lnd2atm( &
           fldname = var_name, &
           data = var_1d)

      call convert_1d_to_2d( &
           ide=ide, jde=jde, &
           ims=ims, ime=ime, &
           jms=jms, jme=jme, &
           its=its, ite=ite, &
           jts=jts, jte=jte, &
           var_1d=var_1d, var_2d=var_2d)

    end subroutine import_from_lilac

  end subroutine ctsm_run
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine calculate_total_albedo( &
       ims, ime, jms, jme, its, ite, jts, jte, &
       albedo_visdir, albedo_nirdir, albedo_visdif, albedo_nirdif, &
       swvisdir, swnirdir, swvisdif, swnirdif, &
       albedo)
    ! Calculate total albedo from its 4 components, based on ratio of incoming SW in the
    ! 4 components.

    integer, intent(in) :: ims
    integer, intent(in) :: ime
    integer, intent(in) :: jms
    integer, intent(in) :: jme
    integer, intent(in) :: its
    integer, intent(in) :: ite
    integer, intent(in) :: jts
    integer, intent(in) :: jte

    real, intent(in) :: albedo_visdir(ims: , jms: ) ! albedo, visible direct [-]
    real, intent(in) :: albedo_nirdir(ims: , jms: ) ! albedo, near IR direct [-]
    real, intent(in) :: albedo_visdif(ims: , jms: ) ! albedo, visible diffuse [-]
    real, intent(in) :: albedo_nirdif(ims: , jms: ) ! albedo, near IR diffuse [-]
    real, intent(in) :: swvisdir(ims: , jms: ) ! solar rad onto surface, visible direct [W m-2]
    real, intent(in) :: swnirdir(ims: , jms: ) ! solar rad onto surface, near IR direct [W m-2]
    real, intent(in) :: swvisdif(ims: , jms: ) ! solar rad onto surface, visible diffuse [W m-2]
    real, intent(in) :: swnirdif(ims: , jms: ) ! solar rad onto surface, near IR diffuse [W m-2]

    real, intent(inout) :: albedo(ims: , jms: ) ! total surface albedo [-]

    integer :: i, j
    real :: sw_tot  ! total solar rad onto surface [W m-2]

    do j = jms, jme
       do i = ims, ime
          ! TODO(wjs, 2020-01-02) Confirm that this is the right way to determine total
          ! albedo. (This is what seems to be done in module_sf_clm.)
          sw_tot = swvisdir(i,j) + swnirdir(i,j) + swvisdif(i,j) + swnirdif(i,j)
          if (sw_tot > 0.) then
             albedo(i,j) = &
                  albedo_visdir * (swvisdir / sw_tot) + &
                  albedo_nirdir * (swnirdir / sw_tot) + &
                  albedo_visdif * (swvisdif / sw_tot) + &
                  albedo_nirdif * (swnirdif / sw_tot)
          else
             ! Night; albedo shouldn't matter; use coefficients from module_sf_clm
             albedo(i,j) = &
                  albedo_visdir * 0.35 + &
                  albedo_nirdir * 0.35 + &
                  albedo_visdif * 0.15 + &
                  albedo_nirdif * 0.15
          end if

          ! TODO(wjs, 2020-01-02) There was a correction like this in clmdrv (line 4799).
          ! Do we want this here?
          if (abs(albedo(i,j) - 1.) < 1.e-5) then
             albedo(i,j) = 0.991
          end if
       end do
    end do
  end subroutine calculate_total_albedo

#endif
  ! endif WRF_USE_CTSM

end module module_sf_ctsm
