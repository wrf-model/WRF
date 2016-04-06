
 type gw_field
      integer :: ix, jx
      integer :: allo_status = -99

      real :: dx, dt

      integer, allocatable, dimension(:,:) ::  ltype     ! land-sfc type
      real,    allocatable, dimension(:,:) ::  &
        elev,           &  ! elev/bathymetry of sfc rel to sl (m)
        bot,            &  ! elev. aquifer bottom rel to sl (m)
        hycond,         &  ! hydraulic conductivity (m/s per m/m)
        poros,          &  ! porosity (m3/m3)
        compres,        &  ! compressibility (1/Pa)
        ho                 ! head at start of timestep (m)

      real,    allocatable, dimension(:,:) ::  &
        h,              &  ! head, after ghmcompute (m)
        convgw             ! convergence due to gw flow (m/s)

      real  :: ebot, eocn
      integer ::istep = 0


  end type gw_field

