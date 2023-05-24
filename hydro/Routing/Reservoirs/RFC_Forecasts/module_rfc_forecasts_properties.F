! This module defines and instantiates objects
! for an rfc forecasts type reservoir's
! parameters/properties. Properties holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.

module module_rfc_forecasts_properties
    use module_reservoir_utilities, only: read_netcdf_lake_id, &
                                          read_reservoir_parameters_netcdf_rfc_gage_id, &
                                          read_reservoir_parameters_netcdf_rfc_integer, &
                                          handle_err
    use module_reservoir, only: reservoir_properties
    use netcdf
    use iso_fortran_env, only: int64

    implicit none

    ! Extend/derive rfc forecasts properties from the abstract base
    ! type for reservoir properties.
    type, extends(reservoir_properties) :: rfc_forecasts_properties_interface
        real              :: lake_area                    ! area of reservoir (meters^2)
        real              :: max_water_elevation          ! max water elevation (meters)
        integer           :: lake_number                  ! lake number
        integer           :: reservoir_type               ! reservoir type
        character(len=5)  :: rfc_gage_id
        integer           :: lookback_seconds
        integer           :: total_counts
        integer           :: observed_counts
        integer           :: time_step_seconds
        integer           :: rfc_forecast_persist_days
        integer           :: rfc_forecast_persist_seconds
        integer           :: rfc_timeslice_offset_hours
        integer           :: observation_hours
        character(len=256):: time_series_file_name

    contains

        procedure :: init => rfc_forecasts_properties_init
        procedure :: destroy => rfc_forecasts_properties_destroy

    end type rfc_forecasts_properties_interface

    integer, parameter :: seconds_in_day = 86400
    integer, parameter :: observation_hours = 48

contains

    ! RFC Forecasts Properties Constructor
    subroutine rfc_forecasts_properties_init(this, lake_area, lake_max_water_elevation, lake_number, reservoir_type, reservoir_parameter_file)
        implicit none
        class(rfc_forecasts_properties_interface), intent(inout) :: this ! the type object being initialized
        real,    intent(in)          :: lake_area                    ! area of lake (km^2)
        real,    intent(in)          :: lake_max_water_elevation     ! max water elevation (meters)
        integer(kind=int64), intent(in)          :: lake_number                  ! lake number
        integer, intent(in)          :: reservoir_type                ! reservoir type
        character(len=*), intent(in) :: reservoir_parameter_file
        integer                      :: ncid, var_id, lake_id_index
        integer                      :: status                       ! status of reading NetCDF

        ! Convert from km^2 to meters^2
        this%lake_area = lake_area * 1.0E6

        this%max_water_elevation = lake_max_water_elevation

        this%lake_number = lake_number

        ! reservoir_type set to 4 for CONUS RFC type forecasts
        ! reservoir_type set to 5 for Alaska RFC type glacier forecasts
        this%reservoir_type = reservoir_type

        this%observation_hours = observation_hours

        this%time_series_file_name = ""

        ! Open Reservoir Parameter NetCDF file
        status = nf90_open(path = reservoir_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        ! Read relevant properties from Persistence Parameter NetCDF
        call read_netcdf_lake_id(ncid, lake_number, "rfc_lake_id", reservoir_parameter_file, lake_id_index)

        call read_reservoir_parameters_netcdf_rfc_gage_id(ncid, lake_id_index, "rfc_gage_id", &
        reservoir_parameter_file, this%rfc_gage_id)

        call read_reservoir_parameters_netcdf_rfc_integer(ncid, lake_id_index, "rfc_forecast_persist", &
        reservoir_parameter_file, this%rfc_forecast_persist_days)

        this%rfc_forecast_persist_seconds = this%rfc_forecast_persist_days * seconds_in_day

        call read_reservoir_parameters_netcdf_rfc_integer(ncid, lake_id_index, "rfc_timeslice_offset", &
        reservoir_parameter_file, this%rfc_timeslice_offset_hours)

        ! Close Reservoir Parameter NetCDF file
        status = nf90_close(ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not close reservoir parameter file" &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

    end subroutine rfc_forecasts_properties_init

    ! RFC Forecasts Properties Destructor
    subroutine rfc_forecasts_properties_destroy(this)
        implicit none
        class(rfc_forecasts_properties_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine rfc_forecasts_properties_destroy

end module module_rfc_forecasts_properties
