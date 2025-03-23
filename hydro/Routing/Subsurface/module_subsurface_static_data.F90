module module_subsurface_static_data
    implicit none

    type subsurface_static_interface

    integer :: ixrt
    integer :: jxrt
    integer :: nsoil
    real :: dt
    integer :: rt_option

    contains
        procedure :: init => subsurface_static_data_init
        procedure :: destroy => subsurface_static_data_destroy
    end type subsurface_static_interface

    contains

    subroutine subsurface_static_data_init(this, ixrt, jxrt, nsoil, dt, rt_option)
        implicit none
        class(subsurface_static_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ixrt                     ! x routing grid size
        integer, intent(in) :: jxrt                     ! y routing grid size
        integer, intent(in) :: nsoil                    ! nsoil
        real, intent(in) :: dt
        integer, intent(in) :: rt_option

        logical :: allocation_error = .false.

        this%ixrt = ixrt
        this%jxrt = jxrt
        this%nsoil = nsoil
        this%dt = dt
        this%rt_option = rt_option

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of subsurface io structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine subsurface_static_data_init

    subroutine subsurface_static_data_destroy(this)
        implicit none
        class(subsurface_static_interface), intent(inout) :: this ! the type object being initialized

        logical :: allocation_error = .false.

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of subsurface io structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine subsurface_static_data_destroy

end module module_subsurface_static_data
