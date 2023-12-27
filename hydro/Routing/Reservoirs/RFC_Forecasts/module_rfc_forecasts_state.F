! This module defines and instantiates objects
! for an rfc forecasts type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given rfc forecasts
! reservoir object and not other modules or areas
! of the system.

module module_rfc_forecasts_state

    use module_levelpool, only: levelpool
    use module_reservoir, only: reservoir_state
    implicit none

    ! Extend/derive rfc forecasts state from the abstract base
    ! type for reservoir state.
    type, extends(reservoir_state) :: rfc_forecasts_state_interface
        real                               :: water_elevation           ! meters AMSL
        real                               :: levelpool_water_elevation ! meters AMSL
        real, allocatable, dimension(:)    :: discharges
        logical, allocatable, dimension(:) :: synthetic_values
        integer                         :: time_series_update_time  ! seconds
        integer                         :: current_time             ! seconds
        integer                         :: time_series_index
        logical                         :: forecast_found
        logical                         :: all_discharges_synthetic
        integer                         :: dynamic_reservoir_type    ! dynamic reservoir type sent to lake out files
        real                            :: assimilated_value
        character(len=256)              :: assimilated_source_file
        integer                         :: levelpool_reservoir_type    ! reservoir type for levelpool
        real                            :: levelpool_assimilated_value ! levelpool assimilated sentinel value
        character(len=256)              :: levelpool_assimilated_source_file ! empty string for levelpool assimilated source

        type (levelpool), pointer :: levelpool_ptr   ! pointer to levelpool object

    contains

        procedure :: init => rfc_forecasts_state_init
        procedure :: destroy => rfc_forecasts_state_destroy

    end type rfc_forecasts_state_interface

contains

    ! RFC Forecasts State Constructor
    subroutine rfc_forecasts_state_init(this, water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, initial_fractional_depth)
        implicit none
        class(rfc_forecasts_state_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in) :: water_elevation           ! meters AMSL
        real, intent(in) :: lake_area                 ! area of lake (km^2)
        real, intent(in) :: lake_max_water_elevation  ! max water elevation (meters)
        real, intent(in) :: orifice_elevation         ! orifice elevation (meters AMSL)
        real, intent(in) :: initial_fractional_depth  ! initial fraction water depth

        ! Initialize the state water elevation in same manner as in module_RT.F
        this%water_elevation = orifice_elevation + ((lake_max_water_elevation - orifice_elevation) * initial_fractional_depth)
        this%levelpool_water_elevation = this%water_elevation
        this%time_series_update_time = 0
        this%current_time = 0
        this%time_series_index = 1
        this%forecast_found = .FALSE.
        this%all_discharges_synthetic = .FALSE.

        ! Initialize dynamic_reservoir_type to 4 for RFC type reservoir
        this%dynamic_reservoir_type = 4

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

    end subroutine rfc_forecasts_state_init

    ! RFC Forecasts State Destructor
    subroutine rfc_forecasts_state_destroy(this)
        implicit none
        class(rfc_forecasts_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine rfc_forecasts_state_destroy

end module module_rfc_forecasts_state
