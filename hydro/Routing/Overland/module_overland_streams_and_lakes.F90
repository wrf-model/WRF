! module overland_stream_and_lake_interface_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the lakes and stream related variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson, Nels Frazier

module overland_stream_and_lake_interface
    implicit none

    ! type that hold inputs and outputs for stream and channels as well as
    ! variables used to interface with channels and lakes
    type overland_stream_and_lake_interface_struct
        !real :: qstrmvoltrt     ! total of qstrmvolrt
        !Accumulated water contribution form surface cells to channel cells throughout the simulation (mm)
        real :: accumulated_surface_water_to_channel
         !FIXME maybe move ^^^ to mass balance

        !real :: lake_inflotrt   ! lake inflow from surface head
        !Accumulated water contribution from surface cells to lake cells throught the simulation (mm)
        real :: accumulated_surface_water_to_lake
        !FIXME maybe move ^^^ to mass balance

        integer, allocatable, dimension(:,:) :: ch_netrt      ! keeps trake of the 0-1 channel network
        !Mask of the grid cells to indicate which cells are part of the channel network, 1 for channel, 0 for not
        !If the mask value is negative on gridded channel routing, then no channel routing occurs for that cell
        integer, allocatable, dimension(:,:) :: channel_mask

        !integer, allocatable, dimension(:,:) :: lake_mskrt ! mask for identifing lake elements in channel network
        !Mask for the grid cells to indicate which cells are part of lakes, 0 for no lake
        !other values range from 1-N, indicating the index to the lake objects represented by the cells with the
        !same values (i.e, all lask_mask cells with a value of 1 make up the gridded representation of lake object 1)
        integer, allocatable, dimension(:,:) :: lake_mask

        !real, allocatable, dimension(:,:) :: qstrmvolrt     ! accumulated channel inflow
        !Depth of water on the surface cell that will go into a channel cell from overland routing, depth (mm)
        real, allocatable, dimension(:,:) :: surface_water_to_channel


        !real, allocatable, dimension(:,:) :: lake_inflort   ! NEED VARIABLE INFO
        !Depth of water on the surface cell that will go into a lake cell from overland routing, depth (mm)
        real, allocatable, dimension(:,:) :: surface_water_to_lake
    contains
        procedure :: init => overland_stream_and_lake_interface_init
        procedure :: destroy => overland_stream_and_lake_interface_destroy
    end type overland_stream_and_lake_interface_struct

    contains

! this structure allocates and initalizes the members of an overland_stream_and_lake_interface strucutre
! if members have allready been initalized they will not be altered and an error will be logged

subroutine overland_stream_and_lake_interface_init(this,ix,jx)
    implicit none
    class(overland_stream_and_lake_interface_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size

    logical :: allocation_error = .false.

    this%accumulated_surface_water_to_channel = 0.0
    this%accumulated_surface_water_to_lake = 0.0

    ! allocate the stream network
    if ( .not. allocated(this%ch_netrt) ) then
        allocate( this%ch_netrt(ix,jx) )
        this%ch_netrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate the lake mask
    if ( .not. allocated(this%lake_mask) ) then
        allocate( this%lake_mask(ix,jx) )
        this%lake_mask = -9999
    else
        allocation_error = .true.
    end if

    ! allocate qstrmvolrt
    if ( .not. allocated(this%surface_water_to_channel) ) then
        allocate( this%surface_water_to_channel(ix,jx) )
        this%surface_water_to_channel = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate lake_inflort
    if ( .not. allocated(this%surface_water_to_lake) ) then
        allocate( this%surface_water_to_lake(ix,jx) )
        this%surface_water_to_lake = 0.0
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to allocate data in members of overland lakes and streams structure&
        &that where allready allocated. The allocated members where not changed"    
end subroutine overland_stream_and_lake_interface_init

! this procedure deallocates and overland_stream_and_lake_interface structure that was initalized with
! overland_stream_and_lake_interface_init

subroutine overland_stream_and_lake_interface_destroy(this)
    implicit none
    class(overland_stream_and_lake_interface_struct), intent(inout) :: this ! the type object being destroyed

    logical :: allocation_error = .false.

    ! deallocate channel network
    if ( allocated(this%ch_netrt) ) then
        deallocate( this%ch_netrt )
    else
        allocation_error = .true.
    end if

    ! deallocate the lake mask
    if ( allocated(this%lake_mask) ) then
        deallocate( this%lake_mask)
    else
        allocation_error = .true.
    end if

    ! deallocate qstrmvolrt
    if ( allocated(this%surface_water_to_channel) ) then
         deallocate( this%surface_water_to_channel )
    else
        allocation_error = .true.
    end if

    ! deallocate qbdryrt
    if ( allocated(this%surface_water_to_lake) ) then
         deallocate( this%surface_water_to_lake )          ! allocate qbdryrt
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to deallocate data in members of overland streams and lakes structure&
        &that where not allready allocated. The unallocated members where not changed"   
end subroutine overland_stream_and_lake_interface_destroy

end module overland_stream_and_lake_interface
