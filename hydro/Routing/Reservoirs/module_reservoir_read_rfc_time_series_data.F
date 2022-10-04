! This module searches for and reads RFC Time Series files
! to get an array of observed and forecasted discharges that
! will be used by reservoirs. The forecast lookback hours
! period is passed in to determine how far back in time from
! the current model time plus another offset time to search
! ahead of model time the module will look for time series files.
! The forecast_file_resolution_minutes determines the time
! increments the module will look back.
module module_reservoir_read_rfc_time_series_data
    use module_reservoir_utilities, only: read_timeslice_netcdf_integer_variables, &
                                          read_timeslice_netcdf_real_1D_variables, &
                                          read_timeslice_netcdf_integer_1D_variables, &
                                          geth_newdate, &
                                          handle_err
    use netcdf
    implicit none

    type :: time_series_data_type

        character(len=19)                  :: start_date
        character(len=19)                  :: current_date
        character(len=256)                 :: time_series_path
        character(len=256)                 :: time_series_file_name
        integer                            :: forecast_lookback_hours
        integer                            :: timeslice_offset_hours
        integer                            :: current_lookback_seconds
        character(len=5)                   :: rfc_gage_id
        integer                            :: total_counts
        integer                            :: observed_counts
        integer                            :: time_step_seconds
        real, allocatable, dimension(:)    :: discharges
        integer, allocatable, dimension(:) :: synthetic_values
        logical, allocatable, dimension(:) :: synthetic_values_logical
        logical                            :: forecast_found
        logical                            :: all_discharges_synthetic

    contains

        procedure :: init => time_series_data_init
        procedure :: destroy => time_series_data_destroy
        procedure :: setup_read_time_series => setup_read_time_series
        procedure :: read_time_series_file => read_time_series_file

    end type time_series_data_type

    type (time_series_data_type) :: time_series_data

    integer, parameter :: forecast_file_resolution_minutes = 60

contains

   ! Time Series Data Type Constructor
    subroutine time_series_data_init(this, start_date, time_series_path, forecast_lookback_hours, &
        timeslice_offset_hours, rfc_gage_id, lake_number, lookback_seconds, total_counts, &
        observed_counts, time_step_seconds, discharges, forecast_found, all_discharges_synthetic, &
        time_series_file_name)
        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being initialized
        character(len=19),  intent(in)                  :: start_date
        character(len=256), intent(in)                  :: time_series_path
        integer,            intent(in)                  :: forecast_lookback_hours
        integer,            intent(in)                  :: timeslice_offset_hours
        character(len=5),   intent(in)                  :: rfc_gage_id
        integer,            intent(in)                  :: lake_number
        integer,            intent(out)                 :: lookback_seconds
        integer,            intent(out)                 :: total_counts
        integer,            intent(out)                 :: observed_counts
        integer,            intent(out)                 :: time_step_seconds
        real, allocatable, dimension(:), intent(inout)  :: discharges
        logical,            intent(inout)               :: forecast_found
        logical,            intent(inout)               :: all_discharges_synthetic
        character(len=256), intent(out)                 :: time_series_file_name
        character(len=15)                               :: lake_number_string

        this%forecast_found = .false.
        this%all_discharges_synthetic = .false.
        this%start_date = start_date
        this%current_date = start_date
        this%time_series_path = time_series_path
        this%forecast_lookback_hours = forecast_lookback_hours
        this%timeslice_offset_hours = timeslice_offset_hours
        this%rfc_gage_id = rfc_gage_id
        this%time_series_file_name = ""

        this%current_lookback_seconds = 0
        this%total_counts = 0
        this%observed_counts = 0
        this%time_step_seconds = 0

        ! Call subroutine to search for and read an RFC time series file
        call this%setup_read_time_series()

        lookback_seconds = this%current_lookback_seconds
        total_counts = this%total_counts
        observed_counts = this%observed_counts
        time_step_seconds = this%time_step_seconds

        ! If the time series forecast is found
        if (this%forecast_found) then
            allocate(discharges(total_counts))

            discharges = this%discharges

            ! Check if all of the values are synthetic
            if ( all(this%synthetic_values_logical) .eqv. .true.) then
                this%all_discharges_synthetic = .true.
            end if

            ! Deallocate forecast arrays
            if(allocated(this%discharges)) deallocate(this%discharges)
            if(allocated(this%synthetic_values)) deallocate(this%synthetic_values)
            if(allocated(this%synthetic_values_logical)) deallocate(this%synthetic_values_logical)

        end if

        forecast_found = this%forecast_found
        all_discharges_synthetic = this%all_discharges_synthetic
        time_series_file_name = this%time_series_file_name

    end subroutine time_series_data_init


    ! Time Series Data Type Destructor
    subroutine time_series_data_destroy(this)

        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being destroyed

    end subroutine time_series_data_destroy


    ! Set up list of time series files to read
    subroutine setup_read_time_series(this)
        implicit none

        class(time_series_data_type), intent(inout) :: this
        character(len=256)   :: time_series_file_name
        character(len=19)    :: old_date, new_date
        character(len=13)    :: old_date_trimmed
        character(len=2)     :: forecast_file_resolution_string
        integer :: total_time_series_file_periods, time_series_file_index
        integer :: forecast_file_minute, forecast_file_resolution_seconds, timeslice_offset_seconds
        logical :: file_exists

        this%current_lookback_seconds = 0

        ! Determine the total number of time series files to search for
        total_time_series_file_periods = this%forecast_lookback_hours * (60 / forecast_file_resolution_minutes)

        timeslice_offset_seconds = this%timeslice_offset_hours * 3600

        old_date = this%current_date

        ! Get the new date-time ahead of model start time to start searching
        ! for time series files if the timeslice_offset_seconds is greater than 0.
        call geth_newdate(new_date, old_date, timeslice_offset_seconds)
        old_date = new_date

        write(forecast_file_resolution_string, "(I2)") forecast_file_resolution_minutes

        ! Negative for going back in time
        forecast_file_resolution_seconds = forecast_file_resolution_minutes * 60 * (-1)

        ! Loop through the total time series periods to look for and read time series files
        do time_series_file_index = 1, total_time_series_file_periods

            old_date_trimmed = trim(old_date(:13))

            time_series_file_name = trim(this%time_series_path) // "/" // old_date_trimmed // "." // &
                            forecast_file_resolution_string // 'min.' // this%rfc_gage_id // '.RFCTimeSeries.ncdf'

            ! Check if file exists
            inquire(FILE = time_series_file_name, EXIST = file_exists)
            if (file_exists) then

                ! Call subroutine to read a particular time series file
                call this%read_time_series_file(time_series_file_name)

                this%time_series_file_name = old_date_trimmed // "." // &
                            forecast_file_resolution_string // 'min.' // this%rfc_gage_id // '.RFCTimeSeries.ncdf'

                exit
            end if

            ! Call subroutine to get the date from one forecast file resolution back in time
            call geth_newdate(new_date, old_date, forecast_file_resolution_seconds)
            old_date = new_date

            ! Lookback seconds to account for how long back a time series file was read
            this%current_lookback_seconds = this%current_lookback_seconds - forecast_file_resolution_seconds

        end do

    end subroutine setup_read_time_series

    ! Read given time series file to get gage discharges
    subroutine read_time_series_file(this, time_series_file)
        implicit none
        class(time_series_data_type), intent(inout) :: this
        character(len=256), intent(in) :: time_series_file
        integer*8, allocatable, dimension(:) :: gage_ids_integer_array
        integer :: reservoir_rfc_gage_index, time_series_rfc_gage_index
        integer :: ncid, status

        ! Open Time Series NetCDF file
        status = nf90_open(path = trim(time_series_file), mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open RFC time series file " &
        // trim(ADJUSTL(time_series_file)) // ".")

        call read_timeslice_netcdf_integer_variables(ncid, 'totalCounts', time_series_file, this%total_counts)

        allocate (this%discharges(this%total_counts))

        allocate (this%synthetic_values(this%total_counts))

        allocate (this%synthetic_values_logical(this%total_counts))

        call read_timeslice_netcdf_integer_variables(ncid, 'observedCounts', time_series_file, this%observed_counts)

        call read_timeslice_netcdf_integer_variables(ncid, 'timeSteps', time_series_file, this%time_step_seconds)

        call read_timeslice_netcdf_real_1D_variables(ncid, 'discharges', time_series_file, this%discharges)

        call read_timeslice_netcdf_integer_1D_variables(ncid, 'synthetic_values', time_series_file, this%synthetic_values)

        this%synthetic_values_logical = (this%synthetic_values /= 0)

        this%forecast_found = .true.

        ! Close Time Series NetCDF file
        status = nf90_close(ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not close RFC time series file " &
        // trim(ADJUSTL(time_series_file)) // ".")

    end subroutine read_time_series_file

end module module_reservoir_read_rfc_time_series_data
