!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
!
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
!
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
!
!  User controllable options: <if applicable>

module module_gw_gw2d_data
   implicit none
   integer, parameter :: max_domain=5

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
           convgw,         &  ! convergence due to gw flow (m/s)
           excess             ! surface exceeding groundwater (mm)

      real,    allocatable, dimension(:,:) ::  &
           qdarcyRT,       &  ! approximated flux between soil and groundwater for coupled simulations on routing grid
           qsgwrt,         &  ! flux between soil and groundwater for coupled simulations on routing grid
           qsgw,           &  ! flux between soil and groundwater for coupled simulations on lsm grid
           qgw_chanrt         ! flux between groundwater and channel

      real  :: ebot, eocn
      integer ::istep = 0

      integer :: its, ite, jts, jte
   end type gw_field

   type (gw_field) :: gw2d(max_domain)
   save gw2d
end module module_gw_gw2d_data
