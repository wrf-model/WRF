! This module defines and instantiates objects
! for a hybrid persistence levelpool type
! reservoir. The hybrid reservoir type
! inherits input and output types from the
! reservoir base module and calls instantiation
! of these into sub-objects. The hybrid
! reservoir type also points to types for
! hybrid properties and state and calls
! instantiation of these into sub-objects.
! A pointer to a levelpool reservoir object
! is also held in state, and this module
! instantiates that levelpool object. There
! is also a subroutine to run hybrid reservoir
! that is derived from the reservoir base
! type interface to run reservoir.

module module_persistence_levelpool_hybrid

    use module_persistence_levelpool_hybrid_properties, only: hybrid_properties_interface
    use module_persistence_levelpool_hybrid_state, only: hybrid_state_interface
    use module_levelpool, only: levelpool
    use module_reservoir_utilities, only: modify_for_projected_storage, warn_negative_inflow, &
                                          create_hybrid_diagnostic_log_file, &
                                          log_hybrid_diagnostic_data
    use module_reservoir, only: reservoir, reservoir_input, reservoir_output
    use module_reservoir_read_timeslice_data, only: usgs_timeslice_data, usace_timeslice_data, timeslice_data_type
    use module_hydro_stop, only: HYDRO_stop
    use iso_fortran_env, only: int64
    implicit none

    ! Extend/derive hybrid type from the abstract base
    ! type for reservoirs.
    type, extends(reservoir) :: persistence_levelpool_hybrid

        ! Define pointers to sub-types / sub-objects to and
        ! held by a level pool reservoir object.
        type (hybrid_properties_interface), pointer :: properties => null()
        type (hybrid_state_interface), pointer :: state => null()
        type (timeslice_data_type), pointer :: timeslice_data => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => hybrid_init
        procedure :: destroy => hybrid_destroy
        procedure :: run => run_hybrid_reservoir

    end type persistence_levelpool_hybrid

#ifndef NCEP_WCOSS
    integer, parameter :: log_warning = 6
#else
    integer, parameter :: log_warning = 78
#endif

contains

    ! Hybrid Constructor
    subroutine hybrid_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, &
        orifice_area, lake_max_water_elevation, initial_fractional_depth, &
        lake_number, reservoir_type, reservoir_parameter_file, start_date, &
        usgs_timeslice_path, usace_timeslice_path, observation_lookback_hours, &
        observation_update_time_interval_seconds)
        implicit none
        class(persistence_levelpool_hybrid), intent(inout) :: this ! object being initialized
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
        character(len=256), intent(in) :: usgs_timeslice_path
        character(len=256), intent(in) :: usace_timeslice_path
        integer,            intent(in) :: observation_lookback_hours
        integer,            intent(in) :: observation_update_time_interval_seconds
        character(len=15)              :: lake_number_string
        character(len=15)              :: reservoir_type_string


#ifdef RESERVOIR_D
        ! Create diagnostic log file only for development/debugging purposes
        call create_hybrid_diagnostic_log_file(lake_number)
#endif

        if (this%pointer_allocation_guard .eqv. .false. ) then

            ! Call to initialize timeslice data object. This object is a singleton for either
            ! USGS (U.S. Geological Survey) or USACE (U.S. Army Corps of Engineers) data source,
            ! so if another reservoir has already initialized the object of a particular data
            ! source, it will return immediately.
            if (reservoir_type == 2) then
                call usgs_timeslice_data%init(start_date, usgs_timeslice_path, &
                reservoir_parameter_file, "usgs", observation_lookback_hours)

                ! Assign pointer to usgs_timeslice_data object
                this%timeslice_data=>usgs_timeslice_data

            else if (reservoir_type == 3) then
                call usace_timeslice_data%init(start_date, usace_timeslice_path, &
                reservoir_parameter_file, "usace", observation_lookback_hours)

                ! Assign pointer to usace_timeslice_data object
                this%timeslice_data=>usace_timeslice_data

            else
                ! Call hydro_stop if reservoir_type is not USGS or USACE because the
                ! data source is not available.
                write(lake_number_string, "(I15)") lake_number
                write(reservoir_type_string, "(I15)") reservoir_type
                call hydro_stop("ERROR: Incorrect reservoir type for reservoir " // trim(ADJUSTL(lake_number_string)) // &
                ". Expected reservoir type 2 or 3 and instead received reservoir type "  &
                // trim(ADJUSTL(reservoir_type_string)) // "." )
            end if

            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created, call hydro_stop.
                write(lake_number_string, "(I15)") lake_number
                call hydro_stop("ERROR: Failure to allocate hybrid input structure for reservoir " &
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
                call hydro_stop("ERROR: Failure to allocate hybrid output structure for reservoir " &
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
                call hydro_stop("ERROR: Failure to allocate hybrid properties structure for reservoir " &
                // trim(ADJUSTL(lake_number_string)) // ".")
            else
                ! initialize hybrid properties
                call this%properties%init(lake_area, lake_max_water_elevation, orifice_elevation, lake_number, &
                reservoir_type, observation_lookback_hours, observation_update_time_interval_seconds, &
                reservoir_parameter_file)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created, call hydro_stop.
                write(lake_number_string, "(I15)") lake_number
                call hydro_stop("ERROR: Failure to allocate hybrid state structure for reservoir " &
                // trim(ADJUSTL(lake_number_string)) // ".")
            else
                ! initialize hybrid state
                call this%state%init(water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, &
                initial_fractional_depth, reservoir_type)
            end if
            this%pointer_allocation_guard = .true.

            ! Initialize persistence weight index to be out of bounds of the persistence weights array
            this%state%persistence_weight_index = SIZE(this%properties%persistence_weighted_coefficients) + 1

            ! Output warning if the initial storage is greater than max storage or less than zero.
            if (this%state%current_storage > this%properties%max_storage) then
                this%state%current_storage = this%properties%max_storage
                write(lake_number_string, "(I15)") lake_number
                write(log_warning,*) "WARNING: initial storage exceeds max storage for reservoir ", lake_number_string, &
                ". Setting initial storage to max storage."

            else if (this%state%current_storage < this%properties%min_storage) then
                this%state%current_storage = this%properties%min_storage
                write(lake_number_string, "(I15)") lake_number
                write(log_warning,*) "WARNING: initial storage is less than zero for reservoir ", lake_number_string, &
                ". Setting initial storage to zero."

            end if

            ! Allocate a single level pool reservoir
            allocate(levelpool :: this%state%levelpool_ptr)

            ! Initialize level pool reservoir
            call this%state%levelpool_ptr%init(water_elevation, lake_area, &
            weir_elevation, weir_coeffecient, weir_length, dam_length, orifice_elevation, &
            orifice_coefficient, orifice_area, lake_max_water_elevation, lake_number)

        end if
    end subroutine hybrid_init


    ! Hybrid Destructor
    subroutine hybrid_destroy(this)
        implicit none
        class(persistence_levelpool_hybrid), intent(inout) :: this ! object being destroyed
    end subroutine hybrid_destroy


    ! Subroutine for running reservoir for a hybrid reservoir
    subroutine run_hybrid_reservoir(this, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period, dynamic_reservoir_type, &
        assimilated_value, assimilated_source_file)
        implicit none
        class(persistence_levelpool_hybrid), intent(inout) :: this
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        integer, intent(out):: dynamic_reservoir_type   ! dynamic reservoir type sent to lake out files
        real, intent(out)   :: assimilated_value        ! value assimilated from observation
        character(len=256), intent(out) :: assimilated_source_file ! source file of assimilated value
        real                :: delta_storage            ! timestep change in storage (cubic meters)
        real                :: local_water_elevation    ! water elevation passed to levelpool (meters AMSL)
        real                :: levelpool_outflow        ! cubic meters per second (cms)
        logical             :: max_storage_reached      ! flag for when max storage is reached
        integer             :: gage_lookback_seconds    ! seconds between current model
                                                        ! time and the time a gage
                                                        ! discharge is read

        max_storage_reached = .false.
        gage_lookback_seconds = 0

        ! Update input variables
        this%input%inflow = inflow
        this%input%lateral_inflow = lateral_inflow

        ! The initialization and management of water elevation might diverge between the global state of the model
        ! and the internal state. The global state water elevation is passed to/from this subroutine at every
        ! timestep. This check is to help guard against an external piece of code adjusting the global state water
        ! elevation, which would cause it to no longer be synchronized with internal state water elevation. This
        ! check also allows this module to borrow from the existing restart capability by initializing reservoirs
        ! to some default water elevation and storage and then waiting for the first timestep to receive the
        ! initial condition, so that the reservoir would not need to manage its own restart. If water elevation
        ! is refactored to no longer be a global state, then this check is not necessary.
        if (this%state%water_elevation .ne. water_elevation) then

            ! Update state variables
            this%state%water_elevation = water_elevation

            this%state%current_storage = (this%state%water_elevation - this%properties%orifice_elevation) * &
            this%properties%lake_area

        end if

        local_water_elevation = this%state%water_elevation

        ! If update time to read new timeslice gage discharges. In forecast modes, the update time automatically is set to
        ! 1,000,000 to ensure that gage discharges are only read at the first timestep. For analysis runs, a timeslice
        ! update time is set as a namelist parameter. A typical update time for an analysis run would be 3600 seconds
        ! (1 hour), at which time new observations would be retrieved. The first reservoir for each processor to reach an
        ! update time will call the function to read a new timeslice file, and these timeslice discharges will be held in
        ! memory for the subsequent reservoirs to use at that timestep.
        if (this%state%current_time >= this%state%timeslice_update_time) then

            ! Read new timeslice gage discharges
            call this%timeslice_data%setup_read_timeslice(this%properties%observation_update_time_interval_seconds, &
            this%state%current_time, this%properties%gage_id, gage_lookback_seconds, this%state%gage_discharge, &
            this%state%assimilated_source_file)

            ! If no good quality gage discharge was found in the given lookback period, the gage discharge is returned
            ! as -1.0. The persistence weight index will be set out of bounds larger than the array. This causes the
            ! the weight update logic to set the persistence weight to 0.0. Therefore, only levelpool calculations will be used.
            if (this%state%gage_discharge < 0.0) then

                ! If at weight update time, then increment persistence weight index.
                ! Otherwise, continue using previous persistence weight index.
                if (this%state%current_time >= this%state%weight_update_time) then
                    ! Increment weight index
                    this%state%persistence_weight_index = this%state%persistence_weight_index + 1

                    ! Set next weight update time
                    this%state%weight_update_time = this%state%weight_update_time + this%properties%weight_update_time_interval
                end if

                ! If out of bounds of array, then set persistence weight to 0.0. Otherwise, set persistence weight from array's indexed value.
                if (this%state%persistence_weight_index > SIZE(this%properties%persistence_weighted_coefficients)) then
                    this%state%persistence_current_weight = 0.0

                else
                    this%state%persistence_current_weight = this%properties%persistence_weighted_coefficients(this%state%persistence_weight_index)
                end if

            else

                ! Set persisted outflow to gage discharge
                this%state%persisted_outflow = this%state%gage_discharge

                ! Set assimilated value to gage discharge
                this%state%assimilated_value = this%state%gage_discharge

                ! Start persistence weight index at 1
                this%state%persistence_weight_index = 1

                ! Grab first persistence weight at index 1 from array
                this%state%persistence_current_weight = this%properties%persistence_weighted_coefficients(this%state%persistence_weight_index)

                ! Set weight update time offset by how long back a timeslice discharge was read. For instance, if the weight update interval is
                ! 24 hours, and the gage discharge was read 2 hours back, then the weight update time will be set to 22 hours after the current time.
                this%state%weight_update_time = this%state%current_time + this%properties%weight_update_time_interval - gage_lookback_seconds

            end if

            ! Calculate levelpool weight
            this%state%levelpool_current_weight = 1.0 - this%state%persistence_current_weight

            ! Set timeslice update time
            this%state%timeslice_update_time = this%state%timeslice_update_time + this%properties%observation_update_time_interval_seconds

        ! If update time to change persistence weights
        else if (this%state%current_time >= this%state%weight_update_time) then

            ! Increment weight index
            this%state%persistence_weight_index = this%state%persistence_weight_index + 1

            ! Boundary check to not exceed the size of the persistence weights array
            if (this%state%persistence_weight_index <= SIZE(this%properties%persistence_weighted_coefficients)) then

                ! Grab indexed persistence weight from array
                this%state%persistence_current_weight = this%properties%persistence_weighted_coefficients(this%state%persistence_weight_index)

            else
                ! If boundary of persistence weights array has been exceeded, then set all persistence weights to 0.0
                this%state%persistence_current_weight = 0.0

            end if

            ! Calculate levelpool weight
            this%state%levelpool_current_weight = 1.0 - this%state%persistence_current_weight

            ! Set next weight update time
            this%state%weight_update_time = this%state%weight_update_time + this%properties%weight_update_time_interval

        end if

        ! Referencing issue https://github.com/NCAR/wrf_hydro_nwm_public/issues/326 for the uncertainty about
        ! previous_timestep_inflow. Further understanding of the exact timing of inflow and previous timestep
        ! inflow might affect which values are passed to this calling of levelpool and subsequent mass/balance
        ! calculations.
        ! Run levelpool reservoir
        call this%state%levelpool_ptr%run(previous_timestep_inflow, inflow, &
        lateral_inflow, local_water_elevation, levelpool_outflow, routing_period, this%state%levelpool_reservoir_type, &
        this%state%levelpool_assimilated_value, this%state%levelpool_assimilated_source_file)

        ! If the levelpool weight is within epsilon of 1.0
        if (this%state%levelpool_current_weight .ge. 1.0 - epsilon(1.0)) then

            ! Set dynamic_reservoir_type to levelpool type
            this%state%dynamic_reservoir_type = this%state%levelpool_reservoir_type

            ! Set the assimilated_value to sentinel, -9999.0
            this%state%assimilated_value = -9999.0

            ! Set the assimilated_source_file to empty string
            this%state%assimilated_source_file = ""
        else

            ! Set dynamic_reservoir_type to given USGS or USACE type
            this%state%dynamic_reservoir_type = this%properties%reservoir_type
        end if

        ! Calculate outflow weighted between persistence and levelpool
        this%output%outflow = this%state%persistence_current_weight &
        * this%state%persisted_outflow + this%state%levelpool_current_weight &
        * levelpool_outflow

        ! Warn if there is a negative inflow
        call warn_negative_inflow(this%input%inflow, this%properties%lake_number, this%state%current_time)

        ! Modify if exceeding storage boundary conditions
        call modify_for_projected_storage(this%input%inflow, &
        this%state%current_storage, this%properties%min_storage, &
        this%properties%max_storage, this%properties%lake_number, &
        this%state%current_time, int(routing_period), this%output%outflow, max_storage_reached)

        ! If max storage has been reached, release greater of persistence or levelpool
        if (max_storage_reached .and. this%output%outflow < levelpool_outflow) then
            this%output%outflow = levelpool_outflow

            ! Set dynamic_reservoir_type to levelpool type
            this%state%dynamic_reservoir_type = this%state%levelpool_reservoir_type

            ! Set the assimilated_value to sentinel, -9999.0
            this%state%assimilated_value = -9999.0

            ! Set the assimilated_source_file to empty string
            this%state%assimilated_source_file = ""
        end if

        ! Calculate change in storage
        delta_storage = (this%input%inflow - this%output%outflow) * routing_period

        ! Update storage from the most recent model states
        this%state%current_storage = this%state%current_storage + delta_storage

        ! Calculate new water elevation. Delta storage is used as opposed to calculating from current storage in
        ! order to minimize floating point error because current storage is a much larger magnitude than water elevation.
        this%state%water_elevation = this%state%water_elevation + delta_storage / this%properties%lake_area

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

        ! Update the current time
        this%state%current_time = this%state%current_time + int(routing_period)

#ifdef RESERVOIR_D
        ! Log diagnostic data only for development/debugging purposes
        call log_hybrid_diagnostic_data(this%properties%lake_number, this%state%current_time, &
        this%state%timeslice_update_time, this%state%weight_update_time, gage_lookback_seconds, &
        this%properties%gage_id, this%state%persistence_weight_index, &
        this%state%persistence_current_weight, this%input%inflow, this%state%current_storage, &
        this%state%water_elevation, this%state%gage_discharge, this%state%persisted_outflow, &
        levelpool_outflow, this%output%outflow)
#endif

    end subroutine run_hybrid_reservoir

end module module_persistence_levelpool_hybrid
