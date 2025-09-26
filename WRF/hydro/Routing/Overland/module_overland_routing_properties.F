! module overland_routing_properties_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the physical property variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson, Nels Frazier

module overland_routing_properties
    implicit none

   ! holds proprties of the the routing grid needed by overland routing algs
   type overland_routing_properties_struct
      integer :: ixrt                                            ! number of cells in x direction on the routing grid
      integer :: jxrt                                            ! number of cells in y direction on the routing grid

      ! PROPOSED a more logical representation of the grid that is relative to the module would be nice.
      ! Currently, we are leaving the legacy ixrt/jxrt names to simplify the interaction of these variables in other places in the code.
      ! At the very least, we need to drop the rt quantifier when we can, since these are clearly ix/jx in the overland_routing structure
      ! Once the subsurface interface is identified, I think we should revisit these.  And if possible, get away from the x notation, a more clear
      ! name might be something like these:
      !integer :: rows                                           ! number of cells in x direction on the routing grid
      !integer :: columns                                        ! number of cells in y direction on the routing grid
      
      ! replaced with surface_slope_x
      !real, allocatable, dimension(:,:) :: soxrt
      ! terrian slope in the x direction (m/m)
      real, allocatable, dimension(:,:) :: surface_slope_x 

      ! replaced with surface_slope_y
      !real, allocatable, dimension(:,:) :: soyrt
      ! terrian slope in the y direction (m/m)
      real, allocatable, dimension(:,:) :: surface_slope_y      

      ! reaplaced with roughness
      !real, allocatable, dimension(:,:) :: ovroughrt
      ! surface roughness parameter for Manning's equation; dissagregated from the land surface model, with adjustment factor applied (none) 
      real, allocatable, dimension(:,:) :: roughness
      
      ! replaced with retention_depth
      !real, allocatable, dimension(:,:) :: retdeprt
      ! minimum amount of surface water required before water is routed as overland flow (mm)
      real, allocatable, dimension(:,:) :: retention_depth     
      
      ! replaced with water_surface_slope
      !real, allocatable, dimension(:,:,:) :: so8rt
      ! water surface slope in 8 ordinal directions (m/m)                                                                  !
      ! TODO verify this correct, check with Wei? 
      !                      1
      !                      |
      !                  8       2
      !                    \   /
      !                 7__     __ 3
      !
      !                    /   \
      !                   6     4 
      !                      |  
      !                      5
      !
      real, allocatable, dimension(:,:,:) :: water_surface_slope 
      
      ! replaced with max_water_surface_slope_index
      !integer, allocatable, dimension(:,:,:) :: so8rt_d
      ! index of neighboring cell in the direction of steepest water surface slope, used with water_surface_slope
      integer, allocatable, dimension(:,:,:) :: max_water_surface_slope_index                                   

      ! replaced with distance_to_neighbor
      !real, allocatable, dimension(:,:,:) :: dist
      ! centerpoint distance to each neighbor (m)
      real, allocatable, dimension(:,:,:) :: distance_to_neighbor
      ! PROPOSED
      ! For a regular grid, distance_to_neighbor should be pretty static, right?
      ! neighbors 1,3,5,7 dist = grid_size
      ! neighbors 2,4,6,8 dist = sqrt( grid_size^2 + grid_size^2)
      ! would suggest eliminating this and using two static variables for square grids.
      ! i.e. direct_neighbor_distance = grid_size
      !      diagonal_neighbor_distance = sqrt( 2*(grid_size^2) )

      contains
         procedure :: init => overland_properties_init
         procedure :: destroy => overland_properties_destory
   end type overland_routing_properties_struct

   contains

! this procedure allocates memory for an overland_routing_properties structure that has not been allocated
! if the structure has been allocated an error will be logged

subroutine overland_properties_init(this,ix,jx)
    implicit none
    class(overland_routing_properties_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size

    logical :: allocation_error = .false.

    ! record the grid dimensions
    ! TODO find a better place for this to be stored
    this%ixrt = ix
    this%jxrt = jx

    ! allocate x slope
    if ( .not. allocated(this%surface_slope_x) ) then
        allocate( this%surface_slope_x(ix,jx) )
        this%surface_slope_x = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate y slope
    if ( .not. allocated(this%surface_slope_y) ) then
        allocate( this%surface_slope_y(ix,jx) )
        this%surface_slope_y = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate 8 directional slope
    if ( .not. allocated(this%water_surface_slope) ) then
        allocate( this%water_surface_slope(ix,jx,8) )
        this%water_surface_slope = -999
    else
        allocation_error = .true.
    end if

    ! allocate slope index
    if ( .not. allocated(this%max_water_surface_slope_index) ) then
        allocate( this%max_water_surface_slope_index(ix,jx,3) )
        this%max_water_surface_slope_index = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate surface roughness
    if ( .not. allocated(this%roughness) ) then
        allocate( this%roughness(ix,jx) )
        this%roughness = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate retention depth
    if ( .not. allocated(this%retention_depth) ) then
        allocate( this%retention_depth(ix,jx) )
        this%retention_depth = 0.001   ! units (mm)
    else
        allocation_error = .true.
    end if

    ! allocate dist
    if ( .not. allocated(this%distance_to_neighbor) ) then
        allocate( this%distance_to_neighbor(ix,jx,9) )
        this%distance_to_neighbor = -999
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to allocate data in members of overland properties structure&
        &that where allready allocated. The allocated members where not changed"      
end subroutine overland_properties_init

! this procedure deallocates and overland_routing_properties structure that was initalized with
! overland_properties_init

subroutine overland_properties_destory(this)
    implicit none
    class(overland_routing_properties_struct), intent(inout) :: this ! the type object being destroyed

    logical :: allocation_error = .false.

    ! deallocate x slope
    if ( allocated(this%surface_slope_x) ) then
        deallocate( this%surface_slope_x )
    else
        allocation_error = .true.
    end if

    ! allocate y slope
    if ( allocated(this%surface_slope_y) ) then
        deallocate( this%surface_slope_y )
    else
        allocation_error = .true.
    end if

    ! allocate water surface slope
    if ( allocated(this%water_surface_slope) ) then
        deallocate( this%water_surface_slope )
    else
        allocation_error = .true.
    end if

    ! allocate slope index
    if ( allocated(this%max_water_surface_slope_index) ) then
        deallocate( this%max_water_surface_slope_index )
    else
        allocation_error = .true.
    end if

    ! allocate surface roughness
    if ( allocated(this%roughness) ) then
        deallocate( this%roughness )
    else
        allocation_error = .true.
    end if

    ! allocate retention depth
    if ( allocated(this%retention_depth ) ) then
        deallocate( this%retention_depth )
    else
        allocation_error = .true.
    end if

    ! allocate dist
    if ( allocated(this%distance_to_neighbor) ) then
        deallocate( this%distance_to_neighbor )
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to deallocate data in members of overland properties structure&
        &that where not allocated. The unallocated members where not changed"      
end subroutine overland_properties_destory


end module overland_routing_properties
