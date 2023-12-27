! This module contains a variety of subroutines/functions
! used by reservoir objects.

module module_reservoir_utilities
    use netcdf
    use module_hydro_stop, only: HYDRO_stop
    use iso_fortran_env, only: int64
    implicit none

#ifndef NCEP_WCOSS
    integer, parameter :: log_warning = 6
#else
    integer, parameter :: log_warning = 78
#endif

contains

    ! Checks and warns for boundary conditions of negative inflow
    subroutine warn_negative_inflow(inflow, lake_number, current_time)
        real, intent(in)    :: inflow
        integer, intent(in) :: lake_number, current_time

        if (inflow < 0.0) then
            write(log_warning,*) "WARNING: Current inflow ", inflow, &
            " cubic meters per second has reached below zero for reservoir ", &
            lake_number, " at ", current_time, " seconds after model start time."
        end if

    end subroutine warn_negative_inflow


    ! Checks and warns for boundary conditions of exceeding max or min storage
    subroutine warn_storage_boundaries(inflow, end_time_storage, min_storage, max_storage, lake_number, current_time)
        real, intent(in)    :: inflow, end_time_storage, min_storage, max_storage
        integer, intent(in) :: lake_number, current_time

        ! Also have option to set the end_time_storage to the max_storage and change the outflow accordingly.
        if (end_time_storage > max_storage) then

            write(log_warning,*) "WARNING: Calculated storage for this timestep, ", end_time_storage, &
            " cubic meters, will exceed the maximum storage boundary of: ", max_storage, &
            " cubic meters, for reservoir ", lake_number," at ", current_time, " seconds after model start time."
            write(log_warning,*) "Current inflow is ", inflow, " cubic meters per second."

        ! Also have option to set the end_time_storage to the min_storage and change the outflow accordingly.
        else if (end_time_storage < min_storage .and. end_time_storage > 0.0) then

            write(log_warning,*) "WARNING: Calculated storage for this timestep, ", end_time_storage, &
            " cubic meters, will fall below the minimum storage requirement of: ", min_storage, &
            " cubic meters, for reservoir ", lake_number, " at ", current_time, " seconds after model start time."
            write(log_warning,*) "Current inflow is ", inflow, " cubic meters per second."

        end if

    end subroutine warn_storage_boundaries


    ! Checks and modifies for boundary conditions of negative storage
    subroutine modify_negative_storage(inflow, previous_time_storage, routing_period, lake_number, current_time, &
        update_time, end_time_storage, outflow)
        real, intent(in)    :: inflow, previous_time_storage, routing_period
        integer, intent(in) :: lake_number, current_time, update_time
        real, intent(inout) :: end_time_storage, outflow

        ! Note to user:
        ! In order to conserve mass, when a reservoir's storage would become 0.0 or less, the release
        ! must be adjusted to avoid releasing water that is not available in the reservoir. To determine
        ! conditions when this could happen, this function calculates the projected by the end of the
        ! timestep (dt) based on the latest known conditions of the storage, inflow, and release values.

        ! If the current conditions would cause the storage to become less than or equal to 0.0 cubic meters,
        ! then the outflow/release will be altered to provide the available storage over the timestep
        ! (given as R = (S + I*dt)/dt ). This will cause storage at the end of the timestep to be 0.0. From
        ! this point, the outflow/release is bounded by the inflow provided until the next update time when
        ! the machine learning model will be run again.

        ! Therefore, the user's choice of time_interval has a significant impact on this simulated outflow/release
        ! behavior in this edge case.  Release cannot increase again until the next update time occurs. This does,
        ! however, allow the reservoir to fill back up once inflow increases to exceed current outflow/release
        ! since storage would then become > 0.0.

        ! If one wants to allow the release to continue to rise as inflow rises, you can set R = I*dt once storage
        ! has been depleted. This passes on inflow to outflow until an update time occurs, in which case the ML
        ! model and its post conditions are responsible for calculating an appropriate release.

        ! The logical 'and' below is not necessary because if otherwise this check updates outflow and
        ! end_time_storage before an update time, those variables would be overwritten after a new release
        ! is calculated from the machine learning model. The logical 'and' is left here for emphasis on
        ! the importance it has on timesteps that are not also update times.
        if (end_time_storage <= 0.0 .AND. current_time < update_time) then

            write(log_warning,*) "WARNING: End time storage, ", end_time_storage, &
            " cubic meters, has reached at or below 0.0 cubic meters for reservoir ", lake_number, &
            " at ", current_time, " seconds after model start time."
            write(log_warning,*) "Current inflow is ", inflow, " cubic meters per second."

            outflow = (previous_time_storage + inflow * routing_period) / routing_period

            end_time_storage = 0.0

            if (outflow < 0.0) outflow = 0.0

        end if

    end subroutine modify_negative_storage


    ! Checks and modifies for boundary conditions of negative inflow
    subroutine modify_negative_inflow(inflow)
        real, intent(inout) :: inflow

        if (inflow < 0.0) inflow = 0.0

    end subroutine modify_negative_inflow


    ! Checks and modifies for boundary conditions of negative outflow or exceeding min or max storage
    subroutine modify_for_projected_storage(inflow, previous_time_storage, min_storage, max_storage, &
        lake_number, current_time, time_interval, outflow, max_storage_reached)
        real, intent(in)        :: inflow, previous_time_storage, min_storage, max_storage
        integer, intent(in)     :: lake_number, current_time, time_interval
        real, intent(inout)     :: outflow
        logical, intent(inout)  :: max_storage_reached
        real                    :: excess, projected_next_time_interval_storage

        if (outflow < 0.0) then
            write(log_warning,*) "WARNING: Calculations return a negative outflow for reservoir ", lake_number
            write(log_warning,*) "at ", current_time, " seconds after model start time."
            outflow = 0.0
        end if

        ! The projected_next_time_interval_storage is only calculated as a boundary condition to update the release
        ! and this storage is calculated assuming a constant flow rate over the time interval. If the time interval
        ! is too large relative to the channel routing period, this may not be an appropriate calculation.
        projected_next_time_interval_storage = previous_time_storage + (inflow - outflow) * time_interval

        if (projected_next_time_interval_storage > max_storage) then

            max_storage_reached = .true.

            write(log_warning,*) "WARNING: Modified release to prevent maximum storage exceedance", &
            " for reservoir ", lake_number
            write(log_warning,*) "at ", current_time, " seconds after model start time."

        else if (projected_next_time_interval_storage < min_storage &
            .AND. projected_next_time_interval_storage > 0.0) then

            outflow = (projected_next_time_interval_storage - min_storage) / time_interval + outflow

            write(log_warning,*) "WARNING: Modified release to maintain minimum storage for reservoir ", lake_number
            write(log_warning,*) "at ", current_time, " seconds after model start time."

        else if (projected_next_time_interval_storage <= 0.0) then

            outflow = inflow

            write(log_warning,*)  "WARNING: Modified release to prevent storage deficit for reservoir ", lake_number
            write(log_warning,*) "at ", current_time, " seconds after model start time."

        end if

        if (outflow < 0.0) outflow = 0.0

    end subroutine modify_for_projected_storage

    ! Reads the reservoir_type form the reservoir parameter file. This subroutine first checks to ensure that
    ! the lake_id array in the reservoir parameter file matches the lake_id array in the lake parameter file and
    ! calls hydro_stop if they do not match.
    subroutine read_reservoir_type(reservoir_parameter_file, lake_parameter_lake_ids, number_of_lake_parameter_lakes, &
        reservoir_types)
        character(len=*), intent(in) :: reservoir_parameter_file
        integer(kind=int64), dimension(:), intent(in) :: lake_parameter_lake_ids
        integer, intent(in) :: number_of_lake_parameter_lakes
        integer, dimension(:), intent(inout) :: reservoir_types
        integer(kind=int64), allocatable, dimension(:) :: reservoir_parameter_lake_ids

        integer, dimension(nf90_max_var_dims) :: dim_ids
        integer :: ncid, var_id, lake_index, number_of_reservoirs, number_of_reservoir_types
        integer :: status                        ! status of reading NetCDF

        ! Open Reservoir Parameter NetCDF file
        status = nf90_open(path = reservoir_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inq_varid(ncid, "lake_id", var_id)
        if (status /= nf90_noerr) call handle_err(status, "lake_id read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, "lake_id read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_reservoirs)
        if(status /= nf90_NoErr) call handle_err(status, "lake_id read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        allocate(reservoir_parameter_lake_ids(number_of_reservoirs))

        status = nf90_get_var(ncid, var_id, reservoir_parameter_lake_ids)
        if (status /= nf90_noerr) call handle_err(status, "lake_id read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        ! Check to ensure that the lake_id array size in the reservoir parameter file matches the lake_id array
        ! size in the lake parameter file
        if (number_of_lake_parameter_lakes .ne. number_of_reservoirs) then
            call hydro_stop("ERROR: Reservoir ID array in the reservoir parameter file does not match the Lake ID array &
            in the lake parameter file.")

        else
            ! Check to ensure that each lake id in the lake_id array in the reservoir parameter file matches
            ! each lake id in the lake_id array in the lake parameter file
            do lake_index = 1, number_of_reservoirs

                if (lake_parameter_lake_ids(lake_index) .ne. reservoir_parameter_lake_ids(lake_index)) then
                    call hydro_stop("Reservoir ID array in the reservoir parameter file does not match the Lake ID array &
                    in the lake parameter file.")
                end if
            end do
        end if

        status = nf90_inq_varid(ncid, "reservoir_type", var_id)
        if (status /= nf90_noerr) call handle_err(status, "reservoir_type read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, "reservoir_type read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_reservoir_types)
        if(status /= nf90_NoErr) call handle_err(status, "reservoir_type read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        ! Check to ensure that the reservoir_type array size matches the lake_id array size.
        if (number_of_reservoir_types .ne. number_of_lake_parameter_lakes) then
            call hydro_stop("ERROR: Reservoir Type array size in the reservoir parameter file does not match the Lake ID array size &
            in the lake parameter file.")
        end if

        status = nf90_get_var(ncid, var_id, reservoir_types)
        if (status /= nf90_noerr) call handle_err(status, "reservoir_type read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        ! Close Reservoir Parameter NetCDF file
        status = nf90_close(ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not close reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        if(allocated(reservoir_parameter_lake_ids)) deallocate(reservoir_parameter_lake_ids)

    end subroutine read_reservoir_type


    ! Gets the dimensions of the gage arrays from the timeslice file
    subroutine get_timeslice_array_dimension(ncid, netcdf_array_name, timeslice_file, number_of_gages)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        character(len=256), intent(in) :: timeslice_file
        integer, intent(out) :: number_of_gages
        integer :: var_id, status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_gages)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

    end subroutine get_timeslice_array_dimension


    ! Read gage id array from the timeslice NetCDF
    subroutine read_timeslice_gage_ids(ncid, netcdf_array_name, timeslice_file, gage_id_var_array)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        character(len=256), intent(in) :: timeslice_file
        character(len=*), dimension(:), intent(out) :: gage_id_var_array
        integer :: var_id, status

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

        status = nf90_get_var(ncid, var_id, gage_id_var_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

    end subroutine read_timeslice_gage_ids


    ! Read real one dimension parameter arrays from the timeslice NetCDF
    subroutine read_timeslice_netcdf_real_1D_variables(ncid, netcdf_array_name, timeslice_file, real_var_array)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        character(len=256), intent(in) :: timeslice_file
        real, dimension(:), intent(out) :: real_var_array
        integer :: var_id, status

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

        status = nf90_get_var(ncid, var_id, real_var_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

    end subroutine read_timeslice_netcdf_real_1D_variables


    ! Read integer one dimension parameter arrays from the timeslice NetCDF
    subroutine read_timeslice_netcdf_integer_1D_variables(ncid, netcdf_array_name, timeslice_file, integer_var_array)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        character(len=256), intent(in) :: timeslice_file
        integer, dimension(:), intent(out) :: integer_var_array
        integer :: var_id, status

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

        status = nf90_get_var(ncid, var_id, integer_var_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

    end subroutine read_timeslice_netcdf_integer_1D_variables


    ! Read all gage ids from reservoir parameter file
    subroutine read_persistence_netcdf_gage_ids_all(ncid, netcdf_array_name, reservoir_parameter_file, var_id, number_of_gages)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        character(len=*), intent(in) :: reservoir_parameter_file
        integer, intent(out) ::  var_id, number_of_gages
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_gages)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

    end subroutine read_persistence_netcdf_gage_ids_all


    ! Read lake id from reservoir parameter file and determine the lake_id_index, which
    ! is the index of the particular reservoir's values in all of the parameter arrays.
    subroutine read_netcdf_lake_id(ncid, lake_number, netcdf_array_name, reservoir_parameter_file, lake_id_index)
        integer, intent(in)          :: ncid
        integer(kind=int64), intent(in)  :: lake_number
        character(len=*), intent(in) :: netcdf_array_name
        character(len=*), intent(in) :: reservoir_parameter_file
        integer, intent(out) :: lake_id_index
        integer, allocatable, dimension(:) :: temp_lake_id_array
        integer :: var_id, status, number_of_lakes, lake_index
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        allocate(temp_lake_id_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_lake_id_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        do lake_index = 1, number_of_lakes
            if (temp_lake_id_array(lake_index) == lake_number) then
                lake_id_index = lake_index
            end if
        end do

        if(allocated(temp_lake_id_array)) deallocate(temp_lake_id_array)

    end subroutine read_netcdf_lake_id


    ! Read gage id array from reservoir parameter file
    subroutine read_persistence_netcdf_gage_id(ncid, lake_id_index, netcdf_array_name, reservoir_parameter_file, gage_id)
        integer, intent(in) :: ncid, lake_id_index
        character(len=*), intent(in) :: netcdf_array_name
        character(len=*), intent(in) :: reservoir_parameter_file
        character(len=*), intent(out) :: gage_id
        character(len=15) :: gage_id_string, gage_id_string_trimmed
        character(len=15), allocatable, dimension(:) :: temp_gage_id_array
        integer :: var_id, status, number_of_lakes
        integer, dimension(nf90_max_var_dims) :: dim_ids
        integer        :: char_index, number_counter, temp_val
        character(len=1)   :: temp_char

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        allocate(temp_gage_id_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_gage_id_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        gage_id_string = temp_gage_id_array(lake_id_index)

        do char_index = 1, 15
            temp_val = ichar(gage_id_string(char_index:char_index))

            ! Check if null character, then pad with a space
            if (temp_val == 0) then
                gage_id_string(char_index:char_index) = ' '
            end if
        end do

        gage_id_string_trimmed = ADJUSTL(trim(gage_id_string))

        gage_id = gage_id_string_trimmed

        if(allocated(temp_gage_id_array)) deallocate(temp_gage_id_array)

    end subroutine read_persistence_netcdf_gage_id


    ! Read rfc gage id array from the reservoir parameters file
    subroutine read_reservoir_parameters_netcdf_rfc_gage_id(ncid, lake_id_index, &
        netcdf_array_name, reservoir_parameter_file, rfc_gage_id)
        integer, intent(in) :: ncid, lake_id_index
        character(len=*), intent(in) :: netcdf_array_name
        character(len=*), intent(in) :: reservoir_parameter_file
        character(len=5), intent(out) :: rfc_gage_id
        character(len=5) :: gage_id_string, gage_id_string_trimmed
        character(len=5), allocatable, dimension(:) :: temp_gage_id_array
        integer :: var_id, status, number_of_lakes
        integer, dimension(nf90_max_var_dims) :: dim_ids
        integer        :: char_index, number_counter
        character(len=1)   :: temp_char

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
         // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        allocate(temp_gage_id_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_gage_id_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        gage_id_string = temp_gage_id_array(lake_id_index)

        gage_id_string_trimmed = ADJUSTL(trim(gage_id_string))

        rfc_gage_id = gage_id_string_trimmed

        if(allocated(temp_gage_id_array)) deallocate(temp_gage_id_array)

    end subroutine read_reservoir_parameters_netcdf_rfc_gage_id


    ! Read an RFC forecast integer variable for a corresponding lake id from the reservoir parameter file
    subroutine read_reservoir_parameters_netcdf_rfc_integer(ncid, lake_id_index, &
        netcdf_array_name, reservoir_parameter_file, integer_var)
        integer, intent(in) :: ncid, lake_id_index
        character(len=*), intent(in) :: netcdf_array_name
        character(len=*), intent(in) :: reservoir_parameter_file
        integer, intent(out) :: integer_var
        integer, allocatable, dimension(:) :: temp_integer_array
        integer :: var_id, status, number_of_lakes
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        allocate(temp_integer_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_integer_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        integer_var = temp_integer_array(lake_id_index)

        if(allocated(temp_integer_array)) deallocate(temp_integer_array)

    end subroutine read_reservoir_parameters_netcdf_rfc_integer


    ! Read real two dimension parameter arrays from the reservoir parameter file
    subroutine read_persistence_netcdf_real_2D_parameters(ncid, lake_id_index, &
        netcdf_array_name, reservoir_parameter_file, var_id, number_of_weights, number_of_lakes)
        integer, intent(in) :: ncid, lake_id_index
        character(len=*), intent(in) :: netcdf_array_name
        character(len=*), intent(in) :: reservoir_parameter_file
        integer, intent(out) :: var_id, number_of_weights, number_of_lakes
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_weights)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name // " read error in reservoir parameter file " &
        // trim(ADJUSTL(reservoir_parameter_file)) // ".")

    end subroutine read_persistence_netcdf_real_2D_parameters


    ! Read integer parameters from the RFC Time Series NetCDF
    subroutine read_timeslice_netcdf_integer_variables(ncid, netcdf_variable_name, timeslice_file, integer_variable)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_variable_name
        character(len=256), intent(in) :: timeslice_file
        integer, intent(out) :: integer_variable
        integer :: var_id, status

        status = nf90_inq_varid(ncid, netcdf_variable_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_variable_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

        status = nf90_get_var(ncid, var_id, integer_variable)
        if (status /= nf90_noerr) call handle_err(status, netcdf_variable_name // " read error in timeslice file " &
        // trim(ADJUSTL(timeslice_file)) // ".")

    end subroutine read_timeslice_netcdf_integer_variables


    ! Handle NetCDF error
    subroutine handle_err(status, variable_error)
        implicit none
        integer, intent (in) :: status
        character (len=*), intent(in) :: variable_error
        if(status /= nf90_noerr) then
            call hydro_stop("ERROR: " // trim(nf90_strerror(status)) // ': ' // variable_error)
        end if

    end subroutine handle_err


    ! Create Persistence Levelpool Hybrid Reservoir Function Diagnostic Log CSV File
    subroutine create_hybrid_diagnostic_log_file(lake_number)
        integer, intent(in) :: lake_number
        character(len=15)   :: lake_number_string, lake_number_string_trimmed
        character(len=50)   :: filename_string

        write(lake_number_string, "(I15)") lake_number
        lake_number_string_trimmed = ADJUSTL(trim(lake_number_string))
        filename_string = "hybrid_logs_"//ADJUSTL(trim(lake_number_string_trimmed))//".csv"

        open (113, file=filename_string, status="unknown")

        write (113, "(A256)") "Current Time, Timeslice Update Time, Weight Update Time, &
        Gage Lookback Seconds, Gage ID, Current Persistence Weight Index, &
        Current Persistence Weight, Inflow, Storage, Water Elevation, Gage Discharge, &
        Persisted Outflow, Levelpool Outflow, Weighted Outflow"

        close(113)

    end subroutine create_hybrid_diagnostic_log_file


    ! Log data to Persistence Levelpool Hybrid Reservoir Function Diagnostic Log CSV File
    subroutine log_hybrid_diagnostic_data(lake_number, current_time, timeslice_update_time, &
        weight_update_time, gage_lookback_seconds, gage_id, persistence_weight_index, &
        persistence_current_weight, inflow, current_storage, water_elevation, gage_discharge, &
        persisted_outflow, levelpool_outflow, outflow)
        integer, intent(in) :: lake_number, current_time, timeslice_update_time
        integer, intent(in) :: weight_update_time, gage_lookback_seconds
        integer, intent(in) :: persistence_weight_index
        real,    intent(in) :: persistence_current_weight, inflow, current_storage
        real,    intent(in) :: water_elevation, gage_discharge, persisted_outflow
        real,    intent(in) :: levelpool_outflow, outflow
        character(len=*), intent(in) :: gage_id
        character(len=15)   :: lake_number_string, lake_number_string_trimmed
        character(len=50)   :: filename_string

        write(lake_number_string, "(I15)") lake_number
        lake_number_string_trimmed = ADJUSTL(trim(lake_number_string))
        filename_string = "hybrid_logs_"//ADJUSTL(trim(lake_number_string_trimmed))//".csv"

        open (113, file=filename_string, status="unknown", position = 'append')

        write (113, "(I20, A1, I20, A1, I20, A1, I20, A1, A15, A1, I20, A1, F15.5, A1, F15.5, &
        A1, F15.15, A1, F15.5, A1, F15.5, A1, F15.5, A1, F15.5, A1, F15.5)") &
        current_time, ',', timeslice_update_time, ',', weight_update_time, ',', &
        gage_lookback_seconds, ',', gage_id, ',', persistence_weight_index, ',', &
        persistence_current_weight, ',', inflow, ',', current_storage, ',', &
        water_elevation, ',', gage_discharge, ',', persisted_outflow, ',', levelpool_outflow, &
        ',', outflow

        close (113)

    end subroutine log_hybrid_diagnostic_data


    ! Create RFC Forecasts Reservoir Function Diagnostic Log CSV File
    subroutine create_rfc_forecasts_diagnostic_log_file(lake_number)
        integer, intent(in) :: lake_number
        character(len=15)   :: lake_number_string, lake_number_string_trimmed
        character(len=50)   :: filename_string

        write(lake_number_string, "(I15)") lake_number
        lake_number_string_trimmed = ADJUSTL(trim(lake_number_string))
        filename_string = "rfc_forecasts_logs_"//ADJUSTL(trim(lake_number_string_trimmed))//".csv"

        open (113, file=filename_string, status="unknown")

        write (113, "(A256)") "Current_Time, Time_Step, Time_Series_Update_Time, &
        Lookback_Seconds, Time_Series_Index, Gage_ID, Inflow, Water_Elevation, &
        Levelpool_Outflow, Outflow"

        close(113)

    end subroutine create_rfc_forecasts_diagnostic_log_file


    ! Log data to RFC Forecasts Reservoir Function Diagnostic Log CSV File
    subroutine log_rfc_forecasts_diagnostic_data(lake_number, current_time, time_step, &
        time_series_update_time, lookback_seconds, time_series_index, gage_id,  &
        inflow, water_elevation, levelpool_outflow, outflow)
        integer, intent(in) :: lake_number, current_time, time_step, time_series_update_time
        integer, intent(in) :: lookback_seconds, time_series_index
        character(len=*), intent(in) :: gage_id
        real,    intent(in) :: inflow, water_elevation, levelpool_outflow, outflow
        character(len=15)   :: lake_number_string, lake_number_string_trimmed
        character(len=50)   :: filename_string

        write(lake_number_string, "(I15)") lake_number
        lake_number_string_trimmed = ADJUSTL(trim(lake_number_string))
        filename_string = "rfc_forecasts_logs_"//ADJUSTL(trim(lake_number_string_trimmed))//".csv"

        open (113, file=filename_string, status="unknown", position = 'append')

        write (113, "(I20, A1, I20, A1, I20, A1, I20, A1, I20, A1, A5, A1, F15.5, A1, &
        F15.5, A1, F15.5, A1, F15.5)") &
        current_time, ',', time_step, ',', time_series_update_time, ',', &
        lookback_seconds, ',', time_series_index, ',', gage_id, ',', &
        inflow, ',', water_elevation, ',', levelpool_outflow, ',', outflow

        close (113)

    end subroutine log_rfc_forecasts_diagnostic_data


    ! Create Levelpool Reservoir Function Diagnostic Log CSV File
    subroutine create_levelpool_diagnostic_log_file(lake_number)
        integer, intent(in) :: lake_number
        character(len=15)   :: lake_number_string, lake_number_string_trimmed
        character(len=50)   :: filename_string

        write(lake_number_string, "(I15)") lake_number
        lake_number_string_trimmed = ADJUSTL(trim(lake_number_string))

        filename_string = "levelpool_logs_"//ADJUSTL(trim(lake_number_string_trimmed))//".csv"

        open (113, file=filename_string, status="unknown")

        write (113, "(A256)") "Inflow, Water Elevation, Outflow"

        close(113)

    end subroutine create_levelpool_diagnostic_log_file


    ! Log data to Levelpool Reservoir Function Diagnostic Log CSV File
    subroutine log_levelpool_diagnostic_data(lake_number, inflow, water_elevation, outflow)
        integer, intent(in) :: lake_number
        real,    intent(in) :: inflow
        real,    intent(in) :: water_elevation
        real,    intent(in) :: outflow
        character(len=15)   :: lake_number_string, lake_number_string_trimmed
        character(len=50)   :: filename_string

        write(lake_number_string, "(I15)") lake_number
        lake_number_string_trimmed = ADJUSTL(trim(lake_number_string))
        filename_string = "levelpool_logs_"//ADJUSTL(trim(lake_number_string_trimmed))//".csv"

        open (113, file=filename_string, status="unknown", position = 'append')

        write (113, "(F15.8, A1, F15.8, A1, F15.8)") &
        inflow, ',', water_elevation, ',', outflow

        close (113)

    end subroutine log_levelpool_diagnostic_data


    ! Get a new date given an existing date and time interval
    subroutine geth_newdate (ndate, odate, idt)
    implicit none

    !  From old date ("YYYY-MM-DD HH:MM:SS.ffff" or "YYYYMMDDHHMMSSffff") and
    !  delta-time, compute the new date.

    !  on entry     -  odate  -  the old hdate.
    !                  idt    -  the change in time

    !  on exit      -  ndate  -  the new hdate.

    integer, intent(in)           :: idt
    character (len=*), intent(out) :: ndate
    character (len=*), intent(in)  :: odate

    !  Local Variables

    !  yrold    -  indicates the year associated with "odate"
    !  moold    -  indicates the month associated with "odate"
    !  dyold    -  indicates the day associated with "odate"
    !  hrold    -  indicates the hour associated with "odate"
    !  miold    -  indicates the minute associated with "odate"
    !  scold    -  indicates the second associated with "odate"

    !  yrnew    -  indicates the year associated with "ndate"
    !  monew    -  indicates the month associated with "ndate"
    !  dynew    -  indicates the day associated with "ndate"
    !  hrnew    -  indicates the hour associated with "ndate"
    !  minew    -  indicates the minute associated with "ndate"
    !  scnew    -  indicates the second associated with "ndate"

    !  mday     -  a list assigning the number of days in each month

    !  i        -  loop counter
    !  nday     -  the integer number of days represented by "idt"
    !  nhour    -  the integer number of hours in "idt" after taking out
    !              all the whole days
    !  nmin     -  the integer number of minutes in "idt" after taking out
    !              all the whole days and whole hours.
    !  nsec     -  the integer number of minutes in "idt" after taking out
    !              all the whole days, whole hours, and whole minutes.

    integer :: newlen, oldlen
    integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
    integer :: yrold, moold, dyold, hrold, miold, scold, frold
    integer :: nday, nhour, nmin, nsec, nfrac, i, ifrc
    logical :: opass
    character (len=10) :: hfrc
    character (len=1) :: sp
    logical :: punct
    integer :: yrstart, yrend, mostart, moend, dystart, dyend
    integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
    integer :: units
    integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

    ! Determine if odate is "YYYY-MM-DD_HH ... " or "YYYYMMDDHH...."
    if (odate(5:5) == "-") then
       punct = .TRUE.
    else
       punct = .FALSE.
    endif

    !  Break down old hdate into parts

    hrold = 0
    miold = 0
    scold = 0
    frold = 0
    oldlen = LEN(odate)
    if (punct) then
       yrstart = 1
       yrend = 4
       mostart = 6
       moend = 7
       dystart = 9
       dyend = 10
       hrstart = 12
       hrend = 13
       mistart = 15
       miend = 16
       scstart = 18
       scend = 19
       frstart = 21
       select case (oldlen)
       case (10)
          ! Days
          units = 1
       case (13)
          ! Hours
          units = 2
       case (16)
          ! Minutes
          units = 3
       case (19)
          ! Seconds
          units = 4
       case (21)
          ! Tenths
          units = 5
       case (22)
          ! Hundredths
          units = 6
       case (23)
          ! Thousandths
          units = 7
       case (24)
          ! Ten thousandths
          units = 8
       case default

          write(*,*) 'FATAL ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
          call hydro_stop("geth_newdate")

       end select

       if (oldlen.ge.11) then
          sp = odate(11:11)
       else
          sp = ' '
       end if

    else

       yrstart = 1
       yrend = 4
       mostart = 5
       moend = 6
       dystart = 7
       dyend = 8
       hrstart = 9
       hrend = 10
       mistart = 11
       miend = 12
       scstart = 13
       scend = 14
       frstart = 15

       select case (oldlen)
       case (8)
          ! Days
          units = 1
       case (10)
          ! Hours
          units = 2
       case (12)
          ! Minutes
          units = 3
       case (14)
          ! Seconds
          units = 4
       case (15)
          ! Tenths
          units = 5
       case (16)
          ! Hundredths
          units = 6
       case (17)
          ! Thousandths
          units = 7
       case (18)
          ! Ten thousandths
          units = 8
       case default

          write(*,*) 'FATAL ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
          call hydro_stop("geth_newdate")

       end select
    endif

    !  Use internal READ statements to convert the CHARACTER string
    !  date into INTEGER components.

    read(odate(yrstart:yrend),  '(i4)') yrold
    read(odate(mostart:moend),  '(i2)') moold
    read(odate(dystart:dyend), '(i2)') dyold
    if (units.ge.2) then
       read(odate(hrstart:hrend),'(i2)') hrold
       if (units.ge.3) then
          read(odate(mistart:miend),'(i2)') miold
          if (units.ge.4) then
             read(odate(scstart:scend),'(i2)') scold
             if (units.ge.5) then
                read(odate(frstart:oldlen),*) frold
             end if
          end if
       end if
    end if

    !  Set the number of days in February for that year.

    mday(2) = nfeb(yrold)

    !  Check that ODATE makes sense.

    opass = .TRUE.

    !  Check that the month of ODATE makes sense.

    if ((moold.gt.12).or.(moold.lt.1)) then
       write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
       opass = .FALSE.
    end if

    !  Check that the day of ODATE makes sense.

    if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
       write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
       opass = .FALSE.
    end if

    !  Check that the hour of ODATE makes sense.

    if ((hrold.gt.23).or.(hrold.lt.0)) then
       write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
       opass = .FALSE.
    end if

    !  Check that the minute of ODATE makes sense.

    if ((miold.gt.59).or.(miold.lt.0)) then
       write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
       opass = .FALSE.
    end if

    !  Check that the second of ODATE makes sense.

    if ((scold.gt.59).or.(scold.lt.0)) then
       write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
       opass = .FALSE.
    end if

    !  Check that the fractional part  of ODATE makes sense.


    if (.not.opass) then

       write(*,*) 'FATAL ERROR: Crazy ODATE: ', odate(1:oldlen), oldlen
       stop

    end if

    !  Date Checks are completed.  Continue.


    !  Compute the number of days, hours, minutes, and seconds in idt

    if (units.ge.5) then !idt should be in fractions of seconds
       ifrc = oldlen-(frstart)+1
       ifrc = 10**ifrc
       nday   = abs(idt)/(86400*ifrc)
       nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
       nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
       nsec   = mod(abs(idt),60*ifrc)/(ifrc)
       nfrac = mod(abs(idt), ifrc)
    else if (units.eq.4) then  !idt should be in seconds
       ifrc = 1
       nday   = abs(idt)/86400 ! integer number of days in delta-time
       nhour  = mod(abs(idt),86400)/3600
       nmin   = mod(abs(idt),3600)/60
       nsec   = mod(abs(idt),60)
       nfrac  = 0
    else if (units.eq.3) then !idt should be in minutes
       ifrc = 1
       nday   = abs(idt)/1440 ! integer number of days in delta-time
       nhour  = mod(abs(idt),1440)/60
       nmin   = mod(abs(idt),60)
       nsec   = 0
       nfrac  = 0
    else if (units.eq.2) then !idt should be in hours
       ifrc = 1
       nday   = abs(idt)/24 ! integer number of days in delta-time
       nhour  = mod(abs(idt),24)
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else if (units.eq.1) then !idt should be in days
       ifrc = 1
       nday   = abs(idt)    ! integer number of days in delta-time
       nhour  = 0
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else

       write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
            oldlen
       write(*,*) '#'//odate(1:oldlen)//'#'
       call hydro_stop("geth_newdate")

    end if

    if (idt.ge.0) then

       frnew = frold + nfrac
       if (frnew.ge.ifrc) then
          frnew = frnew - ifrc
          nsec = nsec + 1
       end if

       scnew = scold + nsec
       if (scnew .ge. 60) then
          scnew = scnew - 60
          nmin  = nmin + 1
       end if

       minew = miold + nmin
       if (minew .ge. 60) then
          minew = minew - 60
          nhour  = nhour + 1
       end if

       hrnew = hrold + nhour
       if (hrnew .ge. 24) then
          hrnew = hrnew - 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew + 1
          if (dynew.gt.mday(monew)) then
             dynew = dynew - mday(monew)
             monew = monew + 1
             if (monew .gt. 12) then
                monew = 1
                yrnew = yrnew + 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb(yrnew)
             end if
          end if
       end do

    else if (idt.lt.0) then

       frnew = frold - nfrac
       if (frnew .lt. 0) then
          frnew = frnew + ifrc
          nsec = nsec + 1
       end if

       scnew = scold - nsec
       if (scnew .lt. 00) then
          scnew = scnew + 60
          nmin  = nmin + 1
       end if

       minew = miold - nmin
       if (minew .lt. 00) then
          minew = minew + 60
          nhour  = nhour + 1
       end if

       hrnew = hrold - nhour
       if (hrnew .lt. 00) then
          hrnew = hrnew + 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew - 1
          if (dynew.eq.0) then
             monew = monew - 1
             if (monew.eq.0) then
                monew = 12
                yrnew = yrnew - 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb(yrnew)
             end if
             dynew = mday(monew)
          end if
       end do
    end if

    !  Now construct the new mdate

    newlen = LEN(ndate)

    if (punct) then

       if (newlen.gt.frstart) then
          write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

       else if (newlen.eq.scend) then
          write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
    19    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

       else if (newlen.eq.miend) then
          write(ndate,16) yrnew, monew, dynew, hrnew, minew
    16    format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

       else if (newlen.eq.hrend) then
          write(ndate,13) yrnew, monew, dynew, hrnew
    13    format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

       else if (newlen.eq.dyend) then
          write(ndate,10) yrnew, monew, dynew
    10    format(i4,'-',i2.2,'-',i2.2)

       end if

    else

       if (newlen.gt.frstart) then
          write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

       else if (newlen.eq.scend) then
          write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
    119   format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

       else if (newlen.eq.miend) then
          write(ndate,116) yrnew, monew, dynew, hrnew, minew
    116   format(i4,i2.2,i2.2,i2.2,i2.2)

       else if (newlen.eq.hrend) then
          write(ndate,113) yrnew, monew, dynew, hrnew
    113   format(i4,i2.2,i2.2,i2.2)

       else if (newlen.eq.dyend) then
          write(ndate,110) yrnew, monew, dynew
    110   format(i4,i2.2,i2.2)

       end if

    endif

    if (punct .and. (oldlen.ge.11) .and. (newlen.ge.11)) ndate(11:11) = sp

    end subroutine geth_newdate

    integer function nfeb(year)
    !
    ! Compute the number of days in February for the given year.
    !
    implicit none
    integer, intent(in) :: year ! Four-digit year

    nfeb = 28 ! By default, February has 28 days ...
    if (mod(year,4).eq.0) then
       nfeb = 29  ! But every four years, it has 29 days ...
       if (mod(year,100).eq.0) then
          nfeb = 28  ! Except every 100 years, when it has 28 days ...
          if (mod(year,400).eq.0) then
             nfeb = 29  ! Except every 400 years, when it has 29 days ...
             if (mod(year,3600).eq.0) then
                nfeb = 28  ! Except every 3600 years, when it has 28 days.
             endif
          endif
       endif
    endif
    end function nfeb

end module module_reservoir_utilities
