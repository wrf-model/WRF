! This module defines and instantiates objects
! for a hybrid type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given hybrid
! reservoir object and not other modules or areas
! of the system.

module module_persistence_levelpool_hybrid_state

    use module_levelpool, only: levelpool
    use module_reservoir, only: reservoir_state
    implicit none

    ! Extend/derive hybrid state from the abstract base
    ! type for reservoir state.
    type, extends(reservoir_state) :: hybrid_state_interface
        real    :: water_elevation              ! meters AMSL
        real*4  :: current_storage              ! cubic meters
        real    :: gage_discharge               ! cubic meters per second (cms)
        real    :: persisted_outflow            ! cubic meters per second (cms)
        integer :: weight_update_time           ! seconds
        integer :: timeslice_update_time        ! seconds
        integer :: current_time                 ! seconds
        integer :: persistence_weight_index
        real    :: levelpool_current_weight
        real    :: persistence_current_weight
        integer :: dynamic_reservoir_type       ! dynamic reservoir type sent to lake out files
        real    :: assimilated_value
        character(len=256) :: assimilated_source_file
        integer :: levelpool_reservoir_type     ! reservoir type for levelpool
        real    :: levelpool_assimilated_value  ! levelpool assimilated sentinel value
        character(len=256) :: levelpool_assimilated_source_file ! none for levelpool assimilated source

        type (levelpool), pointer :: levelpool_ptr   ! pointer to levelpool object

    contains

        procedure :: init => hybrid_state_init
        procedure :: destroy => hybrid_state_destroy

    end type hybrid_state_interface

    integer, parameter :: seconds_in_day = 86400

contains

    !Hybrid State Constructor
    subroutine hybrid_state_init(this, water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, &
        initial_fractional_depth, reservoir_type)
        implicit none
        class(hybrid_state_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in)    :: water_elevation           ! meters AMSL
        real, intent(in)    :: lake_area                 ! area of lake (km^2)
        real, intent(in)    :: lake_max_water_elevation  ! max water elevation (meters)
        real, intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real, intent(in)    :: initial_fractional_depth  ! initial fraction water depth
        integer, intent(in) :: reservoir_type            ! reservoir type

        ! Initialize the state water elevation in same manner as in module_RT.F
        this%water_elevation = orifice_elevation + ((lake_max_water_elevation - orifice_elevation) * initial_fractional_depth)

        this%current_storage = (this%water_elevation - orifice_elevation) * lake_area * 1.0E6

        this%gage_discharge = 0.0
        this%persisted_outflow = 0.0

        this%weight_update_time = 0
        this%timeslice_update_time = 0
        this%current_time = 0

        this%persistence_weight_index = 0
        this%levelpool_current_weight = 0.0
        this%persistence_current_weight = 0.0

        ! If reservoir_type is set to 2 for USGS type, then set the dynamic_reservoir_type also to 2.
        if (reservoir_type == 2) then
            this%dynamic_reservoir_type = 2

        ! If reservoir_type is set to 3 for USACE type, then set the dynamic_reservoir_type also to 3.
        else if (reservoir_type == 3) then
            this%dynamic_reservoir_type = 3
        end if

        ! Initialize to default sentinel, -9999.0
        this%assimilated_value = -9999.0

        ! Initialize to default empty string
        this%assimilated_source_file = ""

        ! Levelpool reservoir type set to 1
        this%levelpool_reservoir_type = 1

        ! Set to default sentinel, -999.0
        this%levelpool_assimilated_value = -9999.0

        ! Set to default string
        this%levelpool_assimilated_source_file = ""

    end subroutine hybrid_state_init

    !Hybrid State Destructor
    subroutine hybrid_state_destroy(this)
        implicit none
        class(hybrid_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine hybrid_state_destroy

end module module_persistence_levelpool_hybrid_state
