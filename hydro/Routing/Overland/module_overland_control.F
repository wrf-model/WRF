! module overland_control_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the control variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson, Nels Frazier

module overland_control
   !type that holds the variables that are inputs to or outputs of the routing code
   ! along with book keeping variables

   !TODO change output integers to variables that are set dependant on the running system
   ! example WCOSS

   type overland_control_struct
      ! outputs
      !FIXME surface_water_head* should be moved to surface_water_depth since head != depth.  head is elevation + pressure/energy, not depth (FRED and TREY meeting feb 27, 2018)
      ! replaced with surface_water_head_lsm
      !real, allocatable, dimension(:,:) :: sfcheadrt
      ! depth of water on the surface (after routing), passed to the land surface model (mm) on the land surface grid.
      ! LSM combines this with canopy water for infiltration excess at the next time step.
      real, allocatable, dimension(:,:) :: surface_water_head_lsm
      
      ! replaced with surface_water_head_routing
      !real, allocatable, dimension(:,:) :: sfcheadsubrt
      ! surface head on the routing grid during integration, input to surface_water_head_lsm for the next time step
      real, allocatable, dimension(:,:) :: surface_water_head_routing
      
      !PROPOSED Decouple the surface_water_head_lsm and surface_water_head_routing into a cleaner interface (only one surface_water_head in overland)
      !This will have to be done once the land surface interface is better hashed out.

      ! inputs
      !FIXME infiltration_excess is a DEPTH (mm) so be explicit and call it infiltration_excess_depth (FRED and TREY meeting feb 27, 2018)
      ! replaced with infiltration_excess
      !real, allocatable, dimension(:,:) :: infxsubrt
      ! infiltration excess from the land surface model (mm) on the routing grid
      real, allocatable, dimension(:,:) :: infiltration_excess
      
      ! miscellaneous bookkeeping
      
      ! DEPRECATE TODO NJF and DJG Feb 13, 2018.  Remove for fall 2019 release
      ! Is passed around overland routing as an output var, renamed to qsfc in route_overland<1,2>
      ! qsfc in overland1 is intent(in), overland2 intent(inout). qsfc NEVER USED
      real, allocatable, dimension(:,:) :: dhrt
      
      ! replaced with boundary_flux
      !FIXME NOT A FLUX!!!! rename to a better descriptor (FRED and TREY meeting feb 27, 2018)
      !real, allocatable, dimension(:,:) :: qbdryrt
      ! flux of boundary cells at a given time step, + into the domain, - out of the domain (mm)
      real, allocatable, dimension(:,:) :: boundary_flux             

      ! replaced with boundary_flux_total
      !real :: qbdrytrt
      ! accumulation of all boundary cell fluxes per time step (<mm>)
      real :: boundary_flux_total                             


      contains
      procedure :: init => overland_control_init
      procedure :: destroy => overland_control_destroy
      end type overland_control_struct

    contains

! this procedure allocates memory for an overland_control structure that has not been allocated
! if the structure has been allocated an error will be logged

subroutine overland_control_init(this,lsm_ix,lsm_jx,rt_ix,rt_jx)
    implicit none
    class(overland_control_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: lsm_ix                    ! land surface x size
    integer, intent(in) :: lsm_jx                    ! land surface y size
    integer, intent(in) :: rt_ix                     ! routing grid x size
    integer, intent(in) :: rt_jx                     ! routing grid y size

    logical :: allocation_error = .false.

   this%boundary_flux_total = 0.0

    ! allocate surface head
    if ( .not. allocated(this%surface_water_head_lsm) ) then
        allocate( this%surface_water_head_lsm(lsm_ix,lsm_jx) )
        this%surface_water_head_lsm = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate surface head

    if ( .not. allocated(this%surface_water_head_routing) ) then
        allocate( this%surface_water_head_routing(rt_ix,rt_jx) )
        this%surface_water_head_routing = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate inflitration excess
    if ( .not. allocated(this%infiltration_excess) ) then
        allocate( this%infiltration_excess(rt_ix,rt_jx) )
        this%infiltration_excess = 0.0
    else
        allocation_error = .true.
    end if

    ! DEPRECATE TODO NJF and DJG Feb 13, 2018.  Remove for fall 2019 release
    ! Is passed around overland routing as an output var, renamed to qsfc in route_overland<1,2>
    ! qsfc in overland1 is intent(in), overland2 intent(inout). qsfc NEVER USED
    ! allocate dhrt
    if ( .not. allocated(this%dhrt) ) then
         allocate( this%dhrt(rt_ix,rt_jx) )
         this%dhrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate qbdryrt
    if ( .not. allocated(this%boundary_flux) ) then
         allocate( this%boundary_flux(rt_ix,rt_jx) )          ! allocate qbdryrt
         this%boundary_flux = 0.0
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to allocate data in members of overland control structure&
        &that where allready allocated. The allocated members where not changed"

end subroutine overland_control_init

! this procedure deallocates and overland_control structure that was initalized with
! overland_control_init

subroutine overland_control_destroy(this)
    implicit none
    class(overland_control_struct), intent(inout) :: this ! the type object being destroyed

    logical :: allocation_error = .false.

    ! deallocate surface head
    if ( allocated(this%surface_water_head_lsm) ) then
        deallocate( this%surface_water_head_lsm )
    else
        allocation_error = .true.
    end if

    ! deallocate surface head
    if ( allocated(this%surface_water_head_routing) ) then
        deallocate( this%surface_water_head_routing )
    else
        allocation_error = .true.
    end if

    ! deallocate inflitration excess
    if ( allocated(this%infiltration_excess) ) then
        deallocate( this%infiltration_excess)
    else
        allocation_error = .true.
    end if

    ! deallocate dhrt
    if ( allocated(this%dhrt) ) then
         deallocate( this%dhrt )
    else
        allocation_error = .true.
    end if

    ! deallocate qbdryrt
    if ( allocated(this%boundary_flux) ) then
         deallocate( this%boundary_flux )          ! deallocate boundary_flux
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to deallocate data in members of overland control structure&
        &that where not allready allocated. The unallocated members where not changed"    
end subroutine overland_control_destroy
end module overland_control
