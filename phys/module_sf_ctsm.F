module module_sf_ctsm

  implicit none
  private

  public :: ctsm_init

contains

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

    ! The very last index in both row & column space is just used on the momentum grid.
    ! Here we are just working with the mass point grid, so we need to ignore that last
    ! index.
    ite_limited = min(ite, ide-1)
    jte_limited = min(jte, jde-1)

    num_points = ((ite_limited - its + 1) * (jte_limited - jts + 1))

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

  subroutine convert_2d_to_1d (ide, jde, ims, ime, jms, jme, its, ite, jts, jte, var_2d, var_1d)
    ! Convert a 2-d native WRF array to a 1-d array appropriate for LILAC

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
    real    , intent(in)  :: var_2d (ims: , jms: )
    real    , allocatable, intent(out) :: var_1d(:)

    ! local variables
    integer :: ite_limited  ! task end index on the mass point grid, i
    integer :: jte_limited  ! task end index on the mass point grid, j
    integer :: num_points
    integer :: i, j, n

    if (.not. all(ubound(var_2d) == [ime, jme])) then
       call wrf_error_fatal('convert_2d_to_1d: incorrect bounds for var_2d')
    end if

    ite_limited = min(ite, ide-1)
    jte_limited = min(jte, jde-1)

    num_points  = ((ite_limited - its + 1) * (jte_limited - jts + 1))

    allocate (var_1d (num_points))

    n = 0

    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          var_1d(n) = var_2d(i,j)
       end do
    end do

  end subroutine convert_2d_to_1d

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
    real , allocatable    :: atm_lons(:)
    real , allocatable    :: atm_lats(:)

    character(len=128) :: atm_calendar
    character(len=128) :: atm_starttype
    integer            :: atm_timestep
    integer            :: atm_start_year ! (yyyy)
    integer            :: atm_start_mon  ! (mm)
    integer            :: atm_start_day
    integer            :: atm_start_secs
    integer            :: atm_stop_year  ! (yyyy)
    integer            :: atm_stop_mon   ! (mm)
    integer            :: atm_stop_day
    integer            :: atm_stop_secs

    integer            :: atm_global_nx
    integer            :: atm_global_ny

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

    atm_start_year    = 2000
    atm_start_mon     = 1
    atm_start_day     = 1
    atm_start_secs    = 0! 0
    ! TODO(NS) : make sure start_second is a correct input for atm_start_secs 

    ! TODO (NS) How are we going to pass atm_stop_year here?? !
    atm_stop_year     = 2000
    atm_stop_mon      = 1
    atm_stop_day      = 2
    atm_stop_secs     = 0

    atm_calendar      = 'GREGORIAN'
    atm_starttype     = 'startup'

    atm_timestep      = nint(dt)
    if (abs(atm_timestep - dt) > 1.e-5) then
       call wrf_error_fatal('ctsm_init: expect dt representable as integer')
    end if

    call lilac_init(  &
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
         atm_start_secs   = atm_start_secs,   &
         atm_stop_year    = atm_stop_year,    &
         atm_stop_mon     = atm_stop_mon,     &
         atm_stop_day     = atm_stop_day,     &
         atm_stop_secs    = atm_stop_secs,    &
         starttype_in     = atm_starttype)

  end subroutine ctsm_init

end module module_sf_ctsm
