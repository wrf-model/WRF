module module_subsurface_input
    implicit none

    type subsurface_input_interface

        ! infiltration excess from the land surface model (mm) on the routing grid
        real, pointer, dimension(:,:) :: infiltration_excess
    contains
        procedure :: init => subsurface_input_init
        procedure :: destroy => subsurface_input_destroy
    end type subsurface_input_interface

    contains

    subroutine subsurface_input_init(this, ix, jx, infiltration_excess)
        implicit none
        class(subsurface_input_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size
        real, pointer, dimension(:,:) :: infiltration_excess

        this%infiltration_excess => infiltration_excess

    end subroutine subsurface_input_init

    subroutine subsurface_input_destroy(this)
        implicit none
        class(subsurface_input_interface), intent(inout) :: this ! the type object being initialized

    end subroutine subsurface_input_destroy

end module module_subsurface_input
