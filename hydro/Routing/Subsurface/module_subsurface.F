module module_subsurface_data

    use module_subsurface_state
    use module_subsurface_properties
    use module_subsurface_grid_transform

    ! included to allow properties to be shared between overland and subsurface code
    use overland_data

    implicit none

    type subsurface_struct

        type ( subsurface_state_interface), pointer :: state => null()
        type ( subsurface_properties_interface), pointer :: properties => null()
        type ( subsurface_grid_transform_interface), pointer :: grid_transform => null()

        ! unused pointer are in an undefined state
        ! this means the result of calling associated(<pointer>)
        ! on a pointer that has not been set is unknown
        ! therefore associated can not be used as a guard
        ! in inital pointer allocation
        logical, private :: pointer_allocation_guard = .false.

        contains

        procedure :: init => subsurface_struct_init
        procedure :: destroy => subsurface_struct_destroy

    end type subsurface_struct

    contains

    subroutine subsurface_struct_init(this,ix,jx,nsoil,overland_data)
        implicit none
        class(subsurface_struct), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size
        integer, intent(in) :: nsoil                  ! number of soil layers
        type(overland_struct), intent(inout) :: overland_data        ! overland data strucuture

    if (this%pointer_allocation_guard .eqv. .false. ) then
        this%pointer_allocation_guard = .true.
        ! allocate the io interface
        allocate( this%state )
        if ( .not. associated( this%state) ) then
            write(0,*) "Failure to allocate subsurface io interface"
        else
            !write(0,*) "Allocating io structure"
            call this%state%init(ix,jx)
        end if

        ! allocate the properties interface
        allocate( this%properties )
        if ( .not. associated( this%properties) ) then
            write(0,*) "Failure to allocate subsurface io interface"
        else
            !write(0,*) "Allocating properties structure"
            call this%properties%init(ix,jx,nsoil,overland_data)
        end if

        ! allocate the grid_transfrom interface
        allocate( this%grid_transform )
        if ( .not. associated( this%grid_transform) ) then
            write(0,*) "Failure to allocate grid transform interface"
        else
            !write(0,*) "Allocating grid transform structure"
            call this%grid_transform%init(ix,jx,nsoil)
        end if
    else
        write(0,*)  "Attempt to double allocate subsurface_struct"
    end if

    end subroutine subsurface_struct_init

    subroutine subsurface_struct_destroy(this)
        implicit none
        class(subsurface_struct), intent(inout) :: this ! the type object being initialized

        if ( this%pointer_allocation_guard .eqv. .true.) then
            !write(0,*) "Testing pointers for deallocation"
            if ( associated( this%state ) ) then
                call this%state%destroy
                deallocate( this%state )
            end if

            if (associated( this%properties ) )then
                call this%properties%destroy
                deallocate( this%properties )
            end if

            if (associated( this%grid_transform ) ) then
                call this%grid_transform%destroy
                deallocate( this%grid_transform )
            end if

            this%pointer_allocation_guard = .false.
        else
            write(0,*)  "Attempt to double delete subsurface_struct"
        end if

    end subroutine subsurface_struct_destroy

end module module_subsurface_data
