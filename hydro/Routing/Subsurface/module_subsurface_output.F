module module_subsurface_output
    implicit none

    type subsurface_output_interface

        ! infiltration excess from the land surface model (mm) on the routing grid
        real, pointer, dimension(:,:) :: infiltration_excess
    contains
        procedure :: init => subsurface_output_init
        procedure :: destroy => subsurface_output_destroy
    end type subsurface_output_interface

    contains

    subroutine subsurface_output_init(this, ix, jx, infiltration_excess)
        implicit none
        class(subsurface_output_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size
        real, pointer, dimension(:,:) :: infiltration_excess

        this%infiltration_excess => infiltration_excess                   ! y grid size

    end subroutine subsurface_output_init

    subroutine subsurface_output_destroy(this)
        implicit none
        class(subsurface_output_interface), intent(inout) :: this ! the type object being initialized

    end subroutine subsurface_output_destroy

end module module_subsurface_output
