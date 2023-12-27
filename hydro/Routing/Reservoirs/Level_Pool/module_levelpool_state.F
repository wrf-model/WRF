! This module defines and instantiates objects
! for a level pool type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given level pool
! reservoir object and not other modules or areas
! of the system.
module module_levelpool_state

    use module_reservoir, only: reservoir_state
    implicit none

    ! Extend/derive level pool state from the abstract base
    ! type for reservoir state.
    type, extends(reservoir_state) :: levelpool_state_interface
        real :: water_elevation                 ! meters AMSL

    contains

        procedure :: init => levelpool_state_init
        procedure :: destroy => levelpool_state_destroy

    end type levelpool_state_interface

contains

    !Level Pool State Constructor
    subroutine levelpool_state_init(this, water_elevation)
        implicit none
        class(levelpool_state_interface), intent(inout) :: this ! the type object being initialized
        real, intent(inout) :: water_elevation     ! meters AMSL

        ! Assign the water elevation value passed in to a particular level pool reservoir
        ! state object's variable for water elevation
        this%water_elevation = water_elevation

    end subroutine levelpool_state_init

    !Level Pool State Destructor
    subroutine levelpool_state_destroy(this)
        implicit none
        class(levelpool_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine levelpool_state_destroy

end module module_levelpool_state
