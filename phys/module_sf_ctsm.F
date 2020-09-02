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
  logical function is_land(xland, xice, xice_threshold)
    ! Returns true if the given point is land, false if ocean or sea ice
    real , intent(in) :: xland          ! land mask (1 for land, 2 for water)
    real , intent(in) :: xice           ! fraction of grid that is seaice
    real , intent(in) :: xice_threshold ! fraction of grid determining seaice

    if (xland >= 1.5) then
       ! ocean
       is_land = .false.
    else if (xice >= xice_threshold) then
       ! sea ice
       is_land = .false.
    else
       is_land = .true.
    end if
  end function is_land
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
  subroutine convert_1d_to_2d (ide, jde, ims, ime, jms, jme, its, ite, jts, jte, &
       xland, xice, xice_threshold, &
       var_1d, var_2d)
    ! Convert a 1-d array from lilac to a 2-d native WRF array
    !
    ! The output array is only set for land points (as determined by xland, xice, xice_threshold)

    use module_wrf_error

    ! input/output variables
    integer  , intent(in) :: ide                 ! domain end index, i
    integer  , intent(in) :: jde                 ! domain end index, j
    integer  , intent(in) :: ims                 ! memory start index (includes halo cells), i
    integer  , intent(in) :: ime                 ! memory end index (includes halo cells), i
    integer  , intent(in) :: jms                 ! memory start index (includes halo cells), j
    integer  , intent(in) :: jme                 ! memory end index (includes halo cells), j
    integer  , intent(in) :: its                 ! task start index, i
    integer  , intent(in) :: ite                 ! task end index, i
    integer  , intent(in) :: jts                 ! task start index, j
    integer  , intent(in) :: jte                 ! task end index, j
    real     , intent(in) :: xland(ims: , jms: ) ! land mask (1 for land, 2 for water)
    real     , intent(in) :: xice(ims: , jms: )  ! fraction of grid that is seaice
    real     , intent(in) :: xice_threshold      ! fraction of grid determining seaice
    real(r8) , intent(in) :: var_1d(:)
    real     , intent(inout) :: var_2d(ims: , jms: )

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
          if (is_land(xland(i,j), xice(i,j), xice_threshold)) then
             var_2d(i,j) = var_1d(n)
          end if
       end do
    end do

  end subroutine convert_1d_to_2d
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine ctsm_init( &
       ids, ide, jds, jde, &
       ims, ime, jms, jme, &
       its, ite, jts, jte, &
       dt, xlat, xlong,    &
       atm_restart)
    ! Initialize CTSM via LILAC

    use module_wrf_error
    use lilac_mod, only : lilac_init1, lilac_init2
    use ctsm_LilacCouplingFieldIndices

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

    logical, intent(in) :: atm_restart
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

    integer            :: atm_global_nx
    integer            :: atm_global_ny

    character(len=512) :: message

    ! TODO(wjs, 2019-12-31) Is this the correct way to get the communicator?
    ! (See https://github.com/ESCOMP/CTSM/issues/1078)
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
    !
    ! (See https://github.com/ESCOMP/CTSM/issues/1078)
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

    atm_calendar      = 'GREGORIAN'

    atm_starttype     = 'startup'
    if (atm_restart) then
        atm_starttype = 'continue'
    endif


    ! TODO(wjs, 2019-12-31) Is there a way to directly get dt as an integer, rather than
    ! relying on converting this real-valued dt to an integer?
    ! (See https://github.com/ESCOMP/CTSM/issues/1078)
    atm_timestep      = nint(dt)
    if (abs(atm_timestep - dt) > (1.e-5 * dt)) then
       call wrf_error_fatal('ctsm_init: expect dt representable as integer')
    end if

    call lilac_init1()
    call lilac_init2(  &
         mpicom           = comp_comm,        &
         atm_global_index = atm_global_index, &
         atm_lons         = atm_lons,         &
         atm_lats         = atm_lats,         &
         atm_global_nx    = atm_global_nx,    &
         atm_global_ny    = atm_global_ny,    &
         atm_calendar     = atm_calendar,     &
         atm_timestep     = atm_timestep,     &
         atm_start_year   = atm_start_year,   &
         atm_start_mon    = atm_start_mon,    &
         atm_start_day    = atm_start_day,    &
         atm_start_secs   = atm_start_secs_since_midnight, &
         starttype_in     = atm_starttype, &
         fields_needed_from_data = [ &
         lilac_a2l_Faxa_bcphidry, lilac_a2l_Faxa_bcphodry, lilac_a2l_Faxa_bcphiwet, &
         lilac_a2l_Faxa_ocphidry, lilac_a2l_Faxa_ocphodry, lilac_a2l_Faxa_ocphiwet, &
         lilac_a2l_Faxa_dstwet1, lilac_a2l_Faxa_dstdry1, &
         lilac_a2l_Faxa_dstwet2, lilac_a2l_Faxa_dstdry2, &
         lilac_a2l_Faxa_dstwet3, lilac_a2l_Faxa_dstdry3, &
         lilac_a2l_Faxa_dstwet4, lilac_a2l_Faxa_dstdry4])

  end subroutine ctsm_init
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine ctsm_run( &
       ! bounds
       ids, ide, jds, jde, &
       ims, ime, jms, jme, kms, &
       its, ite, jts, jte, &

       ! restart flag
       restart_flag, &

       ! general information
       dt, xland, xice, xice_threshold, &

       ! atm -> lnd variables
       dz8w, ht, u_phy, v_phy, p8w, t_phy, th_phy, &
       qv_curr, rainbl, sr, &
       glw, swvisdir, swvisdif, swnirdir, swnirdif, &

       ! lnd -> atm variables
       tsk, t2, qsfc, albedo, &
       ust, hfx, lh, qfx, emiss, z0, znt)

    use lilac_mod, only : lilac_run
    use ctsm_LilacCouplingFieldIndices

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


    logical, intent(in) :: restart_flag      ! restart flag sent from WRF to CTSM

    real , intent(in) :: dt                  ! timestep [s]
    real , intent(in) :: xland(ims: , jms: ) ! land mask (1 for land, 2 for water)
    real , intent(in) :: xice(ims: , jms: )  ! fraction of grid that is seaice
    real , intent(in) :: xice_threshold      ! fraction of grid determining seaice

    real, intent(in) :: dz8w(ims: , kms: , jms: ) ! thickness of atmo layers [m]
    real, intent(in) :: ht(ims: , jms: )  ! terrain height [m]
    real, intent(in) :: u_phy(ims: , kms: , jms: ) ! 3D U wind component [m/s]
    real, intent(in) :: v_phy(ims: , kms: , jms: ) ! 3D V wind component [m/s]
    real, intent(in) :: p8w(ims: , kms: , jms: ) ! 3D pressure, valid at interface [Pa]
    real, intent(in) :: t_phy(ims: , kms: , jms: ) ! 3D atmospheric temperature valid at mid-levels [K]
    real, intent(in) :: th_phy(ims: , kms: , jms: ) ! 3D atmospheric temperature valid at mid-levels [K]
    real, intent(in) :: qv_curr(ims: , kms: , jms: ) ! 3D water vapor mixing ratio [kg/kg_dry]
    real, intent(in) :: rainbl(ims: , jms: ) ! total input precipitation [mm]
    real, intent(in) :: sr(ims: , jms: ) ! frozen precipitation ratio [-]
    real, intent(in) :: glw(ims: , jms: ) ! longwave down at surface [W m-2]
    real, intent(in) :: swvisdir(ims: , jms: ) ! vis direct beam solar rad onto surface [W m-2]
    real, intent(in) :: swvisdif(ims: , jms: ) ! vis diffuse solar rad onto surface [W m-2]
    real, intent(in) :: swnirdir(ims: , jms: ) ! nir direct beam solar rad onto surface [W m-2]
    real, intent(in) :: swnirdif(ims: , jms: ) ! nir diffuse solar rad onto surface [W m-2]

    real, intent(inout) :: tsk(ims: , jms: ) ! surface temperature [K]
    real, intent(inout) :: t2(ims: , jms: ) ! diagnostic 2-m temperature [K]
    real, intent(inout) :: qsfc(ims: , jms: ) ! bulk surface specific humidity
    real, intent(inout) :: albedo(ims: , jms: ) ! total grid albedo [-]
    real, intent(inout) :: ust(ims: , jms: ) ! u* in similarity theory [m/s]
    real, intent(inout) :: hfx(ims: , jms: ) ! sensible heat flux [W m-2]
    real, intent(inout) :: lh(ims: , jms: ) ! latent heat flux [W m-2]
    real, intent(inout) :: qfx(ims: , jms: ) ! latent heat flux [kg s-1 m-2]
    real, intent(inout) :: emiss(ims: , jms: ) ! surface emissivity [between 0 and 1]
    real, intent(inout) :: z0(ims: , jms: ) ! background roughness length [m]
    real, intent(inout) :: znt(ims: , jms: ) ! thermal time-varying roughness length [m]

    logical, save :: first_call = .true. ! true if and only if this is the first time this subroutine has been called in this run

    integer :: i, j

    real :: landmask(ims:ime, jms:jme) ! 1 over land, 0 over non-land (ocean, sea ice)
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

    logical  :: no_negative   = .true.      ! flag for converting negative values to zero

    ! ------------------------------------------------------------------------
    ! Calculate landmask and export it to ctsm via lilac.
    !
    ! For efficiency, only do this the first time this subroutine is called: we assume
    ! landmask stays constant throughout the run (there can be transitions between open
    ! ocean and sea ice, but the land itself stays fixed).
    ! ------------------------------------------------------------------------

    if (first_call) then
       do j = jts, jte
          do i = its, ite
             if (is_land(xland(i,j), xice(i,j), xice_threshold)) then
                landmask(i,j) = 1.
             else
                landmask(i,j) = 0.
             end if
          end do
       end do
       call export_to_lilac(lilac_a2l_Sa_landfrac, landmask)
    end if

    ! ------------------------------------------------------------------------
    ! Calculate derived variables, and 2-d versions of 3-d fields
    ! ------------------------------------------------------------------------

    ! dz8w = thickness of full levels; we want the mid-point of the bottom-most level
    zlvl(its:ite, jts:jte) = 0.5 * dz8w(its:ite, 1, jts:jte)

    forc_u(its:ite, jts:jte) = u_phy(its:ite, 1, jts:jte)
    forc_v(its:ite, jts:jte) = v_phy(its:ite, 1, jts:jte)

    pbot(its:ite, jts:jte) = p8w(its:ite, 1, jts:jte)

    forc_th(its:ite, jts:jte) = th_phy(its:ite, 1, jts:jte)

    forc_t(its:ite, jts:jte) = t_phy(its:ite, 1, jts:jte)

    ! Convert from mixing ratio to specific humidity
    forc_q(its:ite, jts:jte) = qv_curr(its:ite, 1, jts:jte)/(1.0 + qv_curr(its:ite, 1, jts:jte))

    ! Separate total precip into rain and snow. Arbitrarily assign all precipitation to
    ! convective (CTSM requires separate convective vs. large-scale precipitation, but
    ! then just adds the two together).
    precip_rate(its:ite, jts:jte) = rainbl(its:ite, jts:jte)/dt
    snow_convective_rate(its:ite, jts:jte) = precip_rate(its:ite, jts:jte) * sr(its:ite, jts:jte)
    snow_largescale_rate(its:ite, jts:jte) = 0.
    rain_convective_rate(its:ite, jts:jte) = precip_rate(its:ite, jts:jte) * (1. - sr(its:ite, jts:jte))
    rain_largescale_rate(its:ite, jts:jte) = 0.

    ! ------------------------------------------------------------------------
    ! Export data to ctsm via lilac
    ! ------------------------------------------------------------------------

    call export_to_lilac(lilac_a2l_Sa_z, zlvl)
    call export_to_lilac(lilac_a2l_Sa_topo, ht)
    call export_to_lilac(lilac_a2l_Sa_u, forc_u)
    call export_to_lilac(lilac_a2l_Sa_v, forc_v)
    call export_to_lilac(lilac_a2l_Sa_ptem, forc_th)
    call export_to_lilac(lilac_a2l_Sa_pbot, pbot)
    call export_to_lilac(lilac_a2l_Sa_tbot, forc_t)
    call export_to_lilac(lilac_a2l_Sa_shum, forc_q)
    call export_to_lilac(lilac_a2l_Faxa_lwdn, glw)
    call export_to_lilac(lilac_a2l_Faxa_rainc, rain_convective_rate)
    call export_to_lilac(lilac_a2l_Faxa_rainl, rain_largescale_rate)
    call export_to_lilac(lilac_a2l_Faxa_snowc, snow_convective_rate)
    call export_to_lilac(lilac_a2l_Faxa_snowl, snow_largescale_rate)
    call export_to_lilac(lilac_a2l_Faxa_swndr, swnirdir, no_negative)
    call export_to_lilac(lilac_a2l_Faxa_swvdr, swvisdir, no_negative)
    call export_to_lilac(lilac_a2l_Faxa_swndf, swnirdif, no_negative)
    call export_to_lilac(lilac_a2l_Faxa_swvdf, swvisdif, no_negative)

    ! ------------------------------------------------------------------------
    ! Run ctsm via lilac
    ! ------------------------------------------------------------------------

    ! FIXME(wjs, 2020-01-01) Use correct values for restart and stop alarms
    call lilac_run( &
         write_restarts_now = restart_flag, &
         stop_now = .false.)

    ! ------------------------------------------------------------------------
    ! Import data from ctsm via lilac
    ! ------------------------------------------------------------------------

    call import_from_lilac(lilac_l2a_Sl_t, tsk)
    call import_from_lilac(lilac_l2a_Sl_tref, t2)
    call import_from_lilac(lilac_l2a_Sl_qref, qref)

    call import_from_lilac(lilac_l2a_Sl_avsdr, albedo_visdir)
    call import_from_lilac(lilac_l2a_Sl_anidr, albedo_nirdir)
    call import_from_lilac(lilac_l2a_Sl_avsdf, albedo_visdif)
    call import_from_lilac(lilac_l2a_Sl_anidf, albedo_nirdif)
    call calculate_total_albedo( &
         ims=ims, ime=ime, jms=jms, jme=jme, &
         its=its, ite=ite, jts=jts, jte=jte, &
         xland          = xland, &
         xice           = xice, &
         xice_threshold = xice_threshold, &
         albedo_visdir  = albedo_visdir, &
         albedo_nirdir  = albedo_nirdir, &
         albedo_visdif  = albedo_visdif, &
         albedo_nirdif  = albedo_nirdif, &
         swvisdir       = swvisdir, &
         swnirdir       = swnirdir, &
         swvisdif       = swvisdif, &
         swnirdif       = swnirdif, &
         albedo         = albedo)

    call import_from_lilac(lilac_l2a_Sl_fv, ust)

    call import_from_lilac(lilac_l2a_Fall_sen, hfx)
    call import_from_lilac(lilac_l2a_Fall_lat, lh)
    call import_from_lilac(lilac_l2a_Fall_evap, qfx)

    ! At least for now, use CTSM's Sl_z0m (momentum roughness length) for both background
    ! roughness length and thermal time-varying roughness length, even though it's
    ! possible that those two should differ in some way.
    call import_from_lilac(lilac_l2a_Sl_z0m, z0)

    ! ------------------------------------------------------------------------
    ! Calculate derived variables
    ! ------------------------------------------------------------------------

    do j = jts, jte
       do i = its, ite
          if (is_land(xland(i,j), xice(i,j), xice_threshold)) then

             ! Convert from specific humidity to mixing ratio. Note that qref is specific
             ! humidity at 2m, whereas qsfc is supposed to be specified right at the
             ! surface. So there isn't a perfect correspondence between the two, but
             ! given that qsfc is just being used as a diagnostic quantity when using
             ! CTSM (for now), we won't worry about this.
             qsfc(i,j) = qref(i,j) / (1. - qref(i,j))

             ! CTSM assumes an emissivity of 1
             emiss(i,j) = 1.

             ! At least for now, use CTSM's Sl_z0m (momentum roughness length) for both
             ! background roughness length and thermal time-varying roughness length, even
             ! though it's possible that those two should differ in some way.
             znt(i,j) = z0(i,j)

             ! Flip signs of heat and moisture fluxes: CTSM sends as positive downwards;
             ! WRF expects them to be positive upwards
             hfx(i,j) = -hfx(i,j)
             lh(i,j) = -lh(i,j)
             qfx(i,j) = -qfx(i,j)
          end if
       end do
    end do

    first_call = .false.

  contains

    subroutine export_to_lilac(field_index, var_2d , no_negative)
      ! Reshape var_2d to 1d for LILAC, then set the appropriate atm2lnd variable in LILAC

      use ctsm_LilacCouplingFields, only : lilac_atm2lnd

      integer, intent(in) :: field_index  ! one of the field indices defined in ctsm_LilacCouplingFieldIndices
      real, intent(in) :: var_2d(ims: , jms: )

      real(r8), allocatable :: var_1d(:)

      logical , optional, intent(in) :: no_negative  ! convert negative values to zero

      call convert_2d_to_1d( &
           ide=ide, jde=jde, &
           ims=ims, ime=ime, &
           jms=jms, jme=jme, &
           its=its, ite=ite, &
           jts=jts, jte=jte, &
           var_2d=var_2d, var_1d=var_1d)

      ! zero-ing out negative value when we no_negative flag.
      if (present (no_negative)) then
        if (no_negative) then
            var_1d = 0.5* (var_1d +abs(var_1d))
        end if
      end if

      call lilac_atm2lnd( &
           field_index = field_index, &
           data = var_1d)

    end subroutine export_to_lilac

    subroutine import_from_lilac(field_index, var_2d)
      ! Get the appropriate lnd2atm variable from LILAC, then reshape 1d LILAC variable to
      ! fill var_2d

      use ctsm_LilacCouplingFields, only : lilac_lnd2atm

      integer, intent(in) :: field_index  ! one of the field indices defined in ctsm_LilacCouplingFieldIndices
      real, intent(inout) :: var_2d(ims: , jms: )

      real(r8), allocatable :: var_1d(:)
      integer :: num_points

      call get_num_points( &
           ide=ide, jde=jde, &
           its=its, ite=ite, &
           jts=jts, jte=jte, &
           num_points = num_points)

      allocate(var_1d(num_points))

      call lilac_lnd2atm( &
           field_index = field_index, &
           data = var_1d)

      call convert_1d_to_2d( &
           ide=ide, jde=jde, &
           ims=ims, ime=ime, &
           jms=jms, jme=jme, &
           its=its, ite=ite, &
           jts=jts, jte=jte, &
           xland=xland, xice=xice, xice_threshold=xice_threshold, &
           var_1d=var_1d, var_2d=var_2d)

    end subroutine import_from_lilac

  end subroutine ctsm_run
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine calculate_total_albedo( &
       ims, ime, jms, jme, its, ite, jts, jte, &
       xland, xice, xice_threshold, &
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

    real, intent(in) :: xland(ims: , jms: ) ! land mask (1 for land, 2 for water)
    real, intent(in) :: xice(ims: , jms: )  ! fraction of grid that is seaice
    real, intent(in) :: xice_threshold      ! fraction of grid determining seaice

    real, intent(in) :: albedo_visdir(ims: , jms: ) ! albedo, visible direct [-]
    real, intent(in) :: albedo_nirdir(ims: , jms: ) ! albedo, near IR direct [-]
    real, intent(in) :: albedo_visdif(ims: , jms: ) ! albedo, visible diffuse [-]
    real, intent(in) :: albedo_nirdif(ims: , jms: ) ! albedo, near IR diffuse [-]
    real, intent(in) :: swvisdir(ims: , jms: ) ! solar rad onto surface, visible direct [W m-2]
    real, intent(in) :: swnirdir(ims: , jms: ) ! solar rad onto surface, near IR direct [W m-2]
    real, intent(in) :: swvisdif(ims: , jms: ) ! solar rad onto surface, visible diffuse [W m-2]
    real, intent(in) :: swnirdif(ims: , jms: ) ! solar rad onto surface, near IR diffuse [W m-2]

    real, intent(inout) :: albedo(ims: , jms: ) ! total surface albedo [-]

    real, parameter :: albedo_default = 0.3

    integer :: i, j
    real :: sw_tot  ! total solar rad onto surface [W m-2]

    do j = jms, jme
       do i = ims, ime
          if (is_land(xland(i,j), xice(i,j), xice_threshold)) then
             sw_tot = swvisdir(i,j) + swnirdir(i,j) + swvisdif(i,j) + swnirdif(i,j)
             if (sw_tot > 0.) then
                albedo(i,j) = &
                     albedo_visdir(i,j) * (swvisdir(i,j) / sw_tot) + &
                     albedo_nirdir(i,j) * (swnirdir(i,j) / sw_tot) + &
                     albedo_visdif(i,j) * (swvisdif(i,j) / sw_tot) + &
                     albedo_nirdif(i,j) * (swnirdif(i,j) / sw_tot)
             else
                ! Night; albedo shouldn't matter; use coefficients from module_sf_clm
                albedo(i,j) = &
                     albedo_visdir(i,j) * 0.35 + &
                     albedo_nirdir(i,j) * 0.35 + &
                     albedo_visdif(i,j) * 0.15 + &
                     albedo_nirdif(i,j) * 0.15
             end if

             if (abs(albedo(i,j) - 1.) < 1.e-5) then
                ! CTSM gives albedo values of 1 at night. To avoid problems (in case CTSM
                ! thinks it's night but the albedo is still needed in WRF), use some more
                ! reasonable value in this case.
                albedo(i,j) = albedo_default
             end if
          end if
       end do
    end do
  end subroutine calculate_total_albedo

#endif
  ! endif WRF_USE_CTSM

end module module_sf_ctsm
