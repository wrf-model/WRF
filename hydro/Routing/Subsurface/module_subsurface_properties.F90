module module_subsurface_properties
    use overland_data
    implicit none

    type subsurface_properties_interface
        ! disaggregated lateral hydraulic conductivity, on the routing grid, with adjustment factor applied (Noah_distr_routing.F: 2641*) LKSAT*LKSATFAC*[0..1]
        real, allocatable, dimension(:,:) :: lksatrt

        ! water table depth (meters) (module_HYDRO_io.F: 1962*)
        real, allocatable, dimension(:,:) :: zwattablrt

        ! soil depth on routing grid
        real, allocatable, dimension(:,:) :: soldeprt

        !soil depth by layer
        real, allocatable, dimension(:) :: sldpth

        ! need info
        real, dimension(100) :: zsoil

        ! shared properties -- the following variables are shared from overland properties module

        ! terrian slope in the x direction (m/m)
        real, pointer, dimension(:,:) :: surface_slope_x => null()

        ! terrian slope in the y direction (m/m)
        real, pointer, dimension(:,:) :: surface_slope_y => null()

        ! terrain surface slope in 8 ordinal directions (m/m)                                                                  !
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
        real, pointer, dimension(:,:,:) :: surface_slope => null()

        ! index of neighboring cell in the direction of steepest terrain surface slope, used with surface_slope
        integer, pointer, dimension(:,:,:) :: max_surface_slope_index => null()

        ! centerpoint distance to each neighbor (m)
        real, pointer, dimension(:,:,:) :: distance_to_neighbor => null()

        ! disaggregated lksat decay exponent
        real, allocatable, dimension(:,:) :: nexprt

    contains

        procedure :: init => subsurface_properties_init
        procedure :: destroy => subsurface_properties_destroy
    end type subsurface_properties_interface

contains

    subroutine subsurface_properties_init(this,ix,jx,nsoil,overland_data)
        implicit none
        class(subsurface_properties_interface), intent(inout) :: this ! the type object being initalized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size
        integer, intent(in) :: nsoil                  ! number of soil layers
        class(overland_struct), intent(inout) :: overland_data

        logical :: allocation_error = .false.

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%lksatrt) ) then
            allocate( this%lksatrt(ix,jx) )
            this%lksatrt = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%zwattablrt) ) then
            allocate( this%zwattablrt(ix,jx) )
            this%zwattablrt = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%soldeprt) ) then
            allocate( this%soldeprt(ix,jx) )
            this%soldeprt = 0.0
        else
            allocation_error = .true.
        end if

        !allocate storage for sldpth
        if ( .not. allocated(this%sldpth) ) then
            allocate( this%sldpth(nsoil) )
            this%sldpth = 0.0
        else
            allocation_error = .true.
        endif

        this%zsoil = 0.0

        ! now initalize the shared properties from overland data

        if ( associated(overland_data%properties%surface_slope_x) ) then
            this%surface_slope_x => overland_data%properties%surface_slope_x
        else
            allocation_error = .true.
        end if

        if ( associated(overland_data%properties%surface_slope_y) ) then
            this%surface_slope_y => overland_data%properties%surface_slope_y
        else
            allocation_error = .true.
        end if

        if ( associated(overland_data%properties%surface_slope) ) then
            this%surface_slope => overland_data%properties%surface_slope
        else
            allocation_error = .true.
        end if

        if ( associated(overland_data%properties%max_surface_slope_index) ) then
            this%max_surface_slope_index => overland_data%properties%max_surface_slope_index
        else
            allocation_error = .true.
        end if

        if ( associated(overland_data%properties%distance_to_neighbor) ) then
            this%distance_to_neighbor => overland_data%properties%distance_to_neighbor
        else
            allocation_error = .true.
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%nexprt) ) then
            allocate( this%nexprt(ix,jx) )
            this%nexprt = 1.0
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of subsurface properties structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine subsurface_properties_init

    subroutine subsurface_properties_destroy(this)
        implicit none
        class(subsurface_properties_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! only deallocated if already allocated
        if ( allocated(this%lksatrt) ) then
            deallocate( this%lksatrt)
        else
            allocation_error = .true.
        end if

        ! only deallocated if already allocated
        if ( allocated(this%zwattablrt) ) then
            deallocate( this%zwattablrt)
        else
            allocation_error = .true.
        end if

        ! only deallocated if already allocated
        if ( allocated(this%soldeprt) ) then
            deallocate( this%soldeprt)
        else
            allocation_error = .true.
        end if

        ! only deallocated if already allocated
        if ( allocated(this%sldpth) ) then
            deallocate( this%sldpth)
        else
            allocation_error = .true.
        end if


        ! now release the shared properties from overland data

        if ( associated(this%surface_slope_x) ) then
            this%surface_slope_x => null()
        else
            allocation_error = .true.
        end if

        if ( associated(this%surface_slope_y) ) then
            this%surface_slope_y => null()
        else
            allocation_error = .true.
        end if

        if ( associated(this%surface_slope) ) then
            this%surface_slope => null()
        else
            allocation_error = .true.
        end if

        if ( associated(this%max_surface_slope_index) ) then
            this%max_surface_slope_index => null()
        else
            allocation_error = .true.
        end if

        if ( associated(this%distance_to_neighbor) ) then
            this%distance_to_neighbor => null()
        else
            allocation_error = .true.
        end if

        ! only deallocated if already allocated
        if ( allocated(this%nexprt) ) then
            deallocate( this%nexprt)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of subsurface properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine subsurface_properties_destroy

end module module_subsurface_properties
