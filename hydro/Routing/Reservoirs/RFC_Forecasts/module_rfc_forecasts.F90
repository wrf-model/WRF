! This module defines and instantiates objects
! for a RFC Forecasts type reservoir. The RFC
! Forecasts reservoir type inherits input and
! output types from the reservoir base module
! and calls instantiation of these into
! sub-objects. The RFC Forecasts reservoir
! type also points to types for RFC Forecasts
! properties and state and calls instantiation
! of these into sub-objects. There is also a
! subroutine to run RFC Forecasts reservoir
! that is derived from the reservoir base
! type interface to run reservoir.

module module_rfc_forecasts

    use module_rfc_forecasts_properties, only: rfc_forecasts_properties_interface
    use module_rfc_forecasts_state, only: rfc_forecasts_state_interface
    use module_levelpool, only: levelpool
    use module_reservoir_utilities, only: create_rfc_forecasts_diagnostic_log_file, &
                                          log_rfc_forecasts_diagnostic_data
    use module_reservoir, only: reservoir, reservoir_input, reservoir_output
    use module_reservoir_read_rfc_time_series_data, only: time_series_data
    use module_hydro_stop, only: HYDRO_stop
    use iso_fortran_env, only: int64
    implicit none

    ! Extend/derive rfc forecasts type from the abstract base
    ! type for reservoirs.
    type, extends(reservoir) :: rfc_forecasts

        ! Define pointers to sub-types / sub-objects to and
        ! held by an rfc reservoir object.
        type (rfc_forecasts_properties_interface), pointer :: properties => null()
        type (rfc_forecasts_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => rfc_forecasts_init
        procedure :: destroy => rfc_forecasts_destroy
        procedure :: run => run_rfc_forecasts_reservoir

    end type rfc_forecasts

#ifndef NCEP_WCOSS
    integer, parameter :: log_warning = 6
#else
    integer, parameter :: log_warning = 78
#endif

contains

    ! RFC Forecasts Constructor
    subroutine rfc_forecasts_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, &
        orifice_area, lake_max_water_elevation, initial_fractional_depth, &
        lake_number, reservoir_type, reservoir_parameter_file, start_date, &
        time_series_path, forecast_lookback_hours)

        implicit none
        class(rfc_forecasts), intent(inout) :: this ! object being initialized
        real,    intent(inout) :: water_elevation           ! meters AMSL
        real,    intent(in)    :: lake_area      		    ! area of lake (km^2)
        real,    intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real,    intent(in)    :: weir_coeffecient          ! weir coefficient
        real,    intent(in)    :: weir_length               ! weir length (meters)
        real,    intent(in)    :: dam_length                ! dam length (meters)
        real,    intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real,    intent(in)    :: orifice_coefficient       ! orifice coefficient
        real,    intent(in)    :: orifice_area              ! orifice area (meters^2)
        real,    intent(in)    :: lake_max_water_elevation  ! max water elevation (meters)
        real,    intent(in)    :: initial_fractional_depth  ! initial fraction water depth
        integer(kind=int64), intent(in)    :: lake_number               ! lake number
        integer, intent(in)    :: reservoir_type            ! reservoir type
        character(len=*),   intent(in) :: reservoir_parameter_file
        character(len=19),  intent(in) :: start_date
        character(len=256), intent(in) :: time_series_path
        integer,            intent(in) :: forecast_lookback_hours
        integer                        :: update_offset_seconds
        character(len=15)              :: lake_number_string
        character(len=15)              :: reservoir_type_string

#ifdef RESERVOIR_D
        ! Create diagnostic log file only for development/debugging purposes
        call create_rfc_forecasts_diagnostic_log_file(lake_number)
#endif

        if (this%pointer_allocation_guard .eqv. .false. ) then

            if (reservoir_type .ne. 4 .and. reservoir_type .ne. 5) then
                ! Call hydro_stop if reservoir_type is not RFC Forecast.
                write(lake_number_string, "(I15)") lake_number
                write(reservoir_type_string, "(I15)") reservoir_type
                call hydro_stop("ERROR: Incorrect reservoir type for reservoir " // trim(ADJUSTL(lake_number_string)) // &
                ". Expected reservoir type 4 or 5 and instead received reservoir type " // reservoir_type_string // "." )
            end if

            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created, call hydro_stop.
                write(lake_number_string, "(I15)") lake_number
                call hydro_stop("ERROR: Failure to allocate rfc forecasts input structure for reservoir " &
                // trim(ADJUSTL(lake_number_string)) // ".")
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created, call hydro_stop.
                write(lake_number_string, "(I15)") lake_number
                call hydro_stop("ERROR: Failure to allocate rfc forecasts output structure for reservoir " &
                // trim(ADJUSTL(lake_number_string)) // ".")
            else
                ! initialize the output structure
                call this%output%init()
            end if

            ! try to allocate properties
            allocate ( this%properties )
            if ( .not. associated(this%properties) ) then
                ! if the properties structure could not be created, call hydro_stop.
                write(lake_number_string, "(I15)") lake_number
                call hydro_stop("ERROR: Failure to allocate rfc forecasts properties structure for reservoir " &
                // trim(ADJUSTL(lake_number_string)) // ".")
            else
                ! initialize rfc forecasts properties
                call this%properties%init(lake_area, lake_max_water_elevation, lake_number, reservoir_type, reservoir_parameter_file)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created, call hydro_stop.
                write(lake_number_string, "(I15)") lake_number
                call hydro_stop("ERROR: Failure to allocate rfc forecasts state structure for reservoir " &
                // trim(ADJUSTL(lake_number_string)) // ".")
            else
                ! initialize rfc forecasts state
                call this%state%init(water_elevation, lake_area, lake_max_water_elevation, &
                orifice_elevation, initial_fractional_depth)
            end if
            this%pointer_allocation_guard = .true.

        end if

        ! Allocate a single level pool reservoir
        allocate(levelpool :: this%state%levelpool_ptr)

        ! Initialize level pool reservoir
        call this%state%levelpool_ptr%init(water_elevation, lake_area, &
        weir_elevation, weir_coeffecient, weir_length, dam_length, orifice_elevation, &
        orifice_coefficient, orifice_area, lake_max_water_elevation, lake_number)

        ! Call to initialize time series data object
        call time_series_data%init(start_date, time_series_path, forecast_lookback_hours, &
        this%properties%rfc_timeslice_offset_hours, this%properties%rfc_gage_id, &
        this%properties%lake_number, this%properties%lookback_seconds, this%properties%total_counts, &
        this%properties%observed_counts, this%properties%time_step_seconds, this%state%discharges, &
        this%state%forecast_found, this%state%all_discharges_synthetic, this%properties%time_series_file_name)

        ! If the forecast was found and returned a discharge array
        if (this%state%forecast_found) then

            ! Set the assimilated_source_file
            this%state%assimilated_source_file = this%properties%time_series_file_name

            ! Use modulo to calculate the offset seconds in between the hourly time-step to set the update time
            ! for shifting values in the discharge array.
            update_offset_seconds = mod(this%properties%lookback_seconds, this%properties%time_step_seconds)

            this%state%time_series_update_time = this%properties%time_step_seconds - update_offset_seconds

            ! Set starting index in the time series discharge array depending on the lookback seconds to when
            ! the RFC time series file was found, the number of observed values to offset from the beginning of
            ! discharge array to reach the T0 issue time, and the offset hours that the model searched in the future
            ! of model start time to for the time series file issue time.
            this%state%time_series_index = this%properties%lookback_seconds / this%properties%time_step_seconds + 1 &
            + this%properties%observed_counts - this%properties%rfc_timeslice_offset_hours

            ! Check if starting index is out of range of discharge array and set forecast_found to false if out of range.
            if (this%state%time_series_index .le. 0 .or. this%state%time_series_index .ge. this%properties%total_counts) then
                this%state%forecast_found = .false.

                this%state%time_series_index = 1
            end if
        end if

    end subroutine rfc_forecasts_init

    ! RFC Forecasts Destructor
    subroutine rfc_forecasts_destroy(this)
        implicit none
        class(rfc_forecasts), intent(inout) :: this ! object being destroyed
    end subroutine rfc_forecasts_destroy

    ! Subroutine for running reservoir for a rfc forecasts reservoir
    subroutine run_rfc_forecasts_reservoir(this, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period, dynamic_reservoir_type, &
        assimilated_value, assimilated_source_file)
        implicit none
        class(rfc_forecasts), intent(inout) :: this
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        integer, intent(out):: dynamic_reservoir_type   ! dynamic reservoir type sent to lake out files
        real, intent(out)   :: assimilated_value        ! value assimilated from observation or forecast
        character(len=256), intent(out) :: assimilated_source_file ! source file of assimilated value
        real                :: levelpool_outflow        ! cubic meters per second (cms)
        integer		        :: missing_outflow_index
        character(len=15)   :: lake_number_string

        ! If first timestep, then check forecast aspects
        if (this%state%current_time == 0) then
            if (this%state%forecast_found) then

                ! If all of the values are synthetic or any of the values are less than 0.0
                ! or greater than or equal to the MS river historical peak, then
                ! do not use the time series discharge array and revert to level pool.
                ! Otherwise, if forecast was not found, also revert to level pool.
                if (this%state%all_discharges_synthetic) then
                    this%state%forecast_found = .false.

                    write(lake_number_string, "(I15)") this%properties%lake_number
                    write(log_warning,*) "WARNING: RFC Forecast Time Series discharges for reservoir ", &
                    trim(ADJUSTL(lake_number_string)), " using time series file ", &
                    trim(ADJUSTL(this%properties%time_series_file_name)), &
                    " are all synthetic. This reservoir will use level pool calculations instead."

                else if (minval(this%state%discharges) < 0.0) then
                    this%state%forecast_found = .false.

                    write(lake_number_string, "(I15)") this%properties%lake_number
                    write(log_warning,*) "WARNING: RFC Forecast Time Series discharges for reservoir ", &
                    trim(ADJUSTL(lake_number_string)), " using time series file ", &
                    trim(ADJUSTL(this%properties%time_series_file_name)), &
                    " contains missing or negative values. This reservoir will use level pool calculations instead."

                !! peak flow on MS river *2
                !! http://nwis.waterdata.usgs.gov/nwis/peak?site_no=07374000&agency_cd=USGS&format=html
                !! baton rouge 1945: 1,473,000cfs=41,711cms, multiply it roughly by 2
                else if (maxval(this%state%discharges) .ge. 90000.0) then
                    this%state%forecast_found = .false.

                    write(lake_number_string, "(I15)") this%properties%lake_number
                    write(log_warning,*) "WARNING: RFC Forecast Time Series discharges for reservoir ", &
                    trim(ADJUSTL(lake_number_string)), " using time series file ", &
                    trim(ADJUSTL(this%properties%time_series_file_name)), &
                    " contain one or more values greater than or equal to 90,000 Cubic Meters per Second", &
                    " (twice the Mississippi River historical peak flow). These values are assumed to be incorrect.", &
                    " This reservoir will use level pool calculations instead."
                end if

            else
                write(lake_number_string, "(I15)") this%properties%lake_number
                write(log_warning,*) "WARNING: RFC Forecast Time Series file for reservoir ", trim(ADJUSTL(lake_number_string)), &
                " is not available. This reservoir will use level pool calculations instead."
            end if
        end if

        ! Update the current time
        this%state%current_time = this%state%current_time + int(routing_period)

        ! Update input variables
        this%input%inflow = inflow
        this%input%lateral_inflow = lateral_inflow

        ! Update state water elevation
        this%state%water_elevation = water_elevation
        this%state%levelpool_water_elevation = water_elevation

        ! Run levelpool reservoir
        call this%state%levelpool_ptr%run(previous_timestep_inflow, inflow, &
        lateral_inflow, this%state%levelpool_water_elevation, levelpool_outflow, routing_period, this%state%levelpool_reservoir_type, &
        this%state%levelpool_assimilated_value, this%state%levelpool_assimilated_source_file)

        ! Check if Routing Period is greater than 1 hour, and if true, set forecast_found to false.
        if (routing_period .gt. 3600) then
            this%state%forecast_found = .false.

            write(lake_number_string, "(I15)") this%properties%lake_number
            write(log_warning,*) "WARNING: the routing period is greater than one hour. Therefore, the RFC forecasts", &
            " cannot be used for reservoir ", trim(ADJUSTL(lake_number_string)), &
            ". This reservoir will use level pool calculations instead."
        end if

        ! If a forecast is found and the current time is less than the max time an RFC Forecast is persisted
        if (this%state%forecast_found .and. this%state%current_time .le. this%properties%rfc_forecast_persist_seconds) then

            ! If current time is at or past the update time to cycle to the next value in the discharge array and the number
            ! forecast counts has not yet been reached
            if (this%state%current_time .ge. this%state%time_series_update_time .and. this%state%time_series_index &
            .lt. this%properties%total_counts) then

                ! Increment time series index
                this%state%time_series_index = this%state%time_series_index + 1

                ! Set next update time
                this%state%time_series_update_time = this%state%time_series_update_time + this%properties%time_step_seconds

            end if

            ! If reservoir_type is 4 for CONUS RFC reservoirs
            if (this%properties%reservoir_type == 4) then

                ! Set outflow to corresponding discharge from array
                this%output%outflow = this%state%discharges(this%state%time_series_index)

            ! Else reservoir_type 5 for for Alaska RFC glacier outflows
            else

                ! Set outflow to sum inflow and corresponding discharge from array
                this%output%outflow = this%input%inflow + this%state%discharges(this%state%time_series_index)
            end if

            ! Update water elevation
            this%state%water_elevation = this%state%water_elevation + &
            ((this%input%inflow - this%output%outflow) / this%properties%lake_area) * routing_period

            ! Ensure that the water elevation is within the minimum and maximum elevation
            if (this%state%water_elevation < 0.0) then
                this%state%water_elevation = 0.0

            else if (this%state%water_elevation > this%properties%max_water_elevation) then
                this%state%water_elevation = this%properties%max_water_elevation

            end if

            ! Set dynamic_reservoir_type to RFC Forecasts Type
            this%state%dynamic_reservoir_type = this%properties%reservoir_type

            ! Set the assimilated_value to corresponding discharge from array
            this%state%assimilated_value = this%state%discharges(this%state%time_series_index)

            ! Check for outflows less than 0 and cycle backwards in the array until a
            ! non-negative value is found. If all previous values are negative, then
            ! use level pool outflow.
            if (this%output%outflow < 0) then
                missing_outflow_index = this%state%time_series_index

                do while (this%output%outflow < 0 .and. missing_outflow_index > 1)
                    missing_outflow_index = missing_outflow_index - 1

                    this%output%outflow = this%state%discharges(missing_outflow_index)
                end do

                if (this%output%outflow < 0) then

                    ! If reservoir_type is 4 for CONUS RFC reservoirs
                    if (this%properties%reservoir_type == 4) then
                        this%output%outflow = levelpool_outflow

                    ! Else reservoir_type 5 for for Alaska RFC glacier outflows
                    else
                        this%output%outflow = this%input%inflow
                    end if

                    ! Update water elevation to levelpool water elevation
                    this%state%water_elevation = this%state%levelpool_water_elevation

                    ! Set dynamic_reservoir_type to levelpool type
                    this%state%dynamic_reservoir_type = this%state%levelpool_reservoir_type

                    ! Set the assimilated_value to sentinel, -9999.0
                    this%state%assimilated_value = -9999.0

                    ! Set the assimilated_source_file to empty string
                    this%state%assimilated_source_file = ""
                end if

            end if

        else
            ! If reservoir_type is 4 for CONUS RFC reservoirs
            if (this%properties%reservoir_type == 4) then
                this%output%outflow = levelpool_outflow

            ! Else reservoir_type 5 for for Alaska RFC glacier outflows
            else
                this%output%outflow = this%input%inflow
            end if

            ! Update water elevation to levelpool water elevation
            this%state%water_elevation = this%state%levelpool_water_elevation

            ! Set dynamic_reservoir_type to levelpool type
            this%state%dynamic_reservoir_type = this%state%levelpool_reservoir_type

            ! Set the assimilated_value to sentinel, -9999.0
            this%state%assimilated_value = -9999.0

            ! Set the assimilated_source_file to empty string
            this%state%assimilated_source_file = ""
        end if

        ! Update output variable returned from this subroutine
        outflow = this%output%outflow

        ! Set current inflow to previous_timestep_inflow
        this%input%previous_timestep_inflow = inflow

        ! Update water_elevation variable returned from this subroutine
        water_elevation = this%state%water_elevation

        ! Update the dynamic_reservoir_type returned from this subroutine
        dynamic_reservoir_type = this%state%dynamic_reservoir_type

        ! Update the assimilated_value returned from this subroutine
        assimilated_value = this%state%assimilated_value

        ! Update the assimilated_source_file returned from this subroutine
        assimilated_source_file = this%state%assimilated_source_file

#ifdef RESERVOIR_D
        ! Log diagnostic data only for development/debugging purposes
        call log_rfc_forecasts_diagnostic_data(this%properties%lake_number, this%state%current_time, &
        this%properties%time_step_seconds, this%state%time_series_update_time, this%properties%lookback_seconds, &
        this%state%time_series_index, this%properties%rfc_gage_id, this%input%inflow, this%state%water_elevation, &
        levelpool_outflow, this%output%outflow)
#endif

    end subroutine run_rfc_forecasts_reservoir

end module module_rfc_forecasts
