! This module holds tests for initialization of
! various attributes of a persistence levelpool
! hybrid reservoir.
module module_persistence_levelpool_hybrid_tests
    use module_persistence_levelpool_hybrid

contains

    ! Testing
    function persistence_levelpool_hybrid_data_info(hybrid_data) result(rv)
        implicit none
        type (persistence_levelpool_hybrid) :: hybrid_data
        logical :: rv
        logical, dimension(8) :: ptr_state
        logical, dimension(8) :: data_state

        rv = .false.

        ! Check to see if the hybrid_state data structure exists
        print *, "Checking pointer association on hybrid_data%state "
        if ( associated(hybrid_data%state) ) then
            print *, "PASSED"
            ptr_state(1) = .true.
       else
            print *, "FAILED"
            ptr_state(1) = .false.
        end if
        print *, " "

        ! Check to see if the hybrid_properties data structure exists
        print *, "Checking pointer association on hybrid_data%properties "
        if ( associated(hybrid_data%properties) ) then
            print *, "PASSED"
            ptr_state(2) = .true.
        else
            print *, "FAILED"
            ptr_state(2) = .false.
        end if
        print *, " "

        ! Check to see if the hybrid_input data structure exists
        print *, "Checking pointer association on hybrid_data%input "
        if ( associated(hybrid_data%input) ) then
            print *, "PASSED"
            ptr_state(3) = .true.
        else
            print *, "FAILED"
            ptr_state(3) = .false.
        end if
        print *, " "

        ! Check to see if the hybrid_output data structure exists
        print *, "Checking pointer association on hybrid_data%output "
        if ( associated(hybrid_data%output) ) then
            print *, "PASSED"
            ptr_state(4) = .true.
        else
            print *, "FAILED"
            ptr_state(4) = .false.
        end if
        print *, " "


        ! Test levelpool structure
        ! Check to see if the hybrid levelpool_state data structure exists
        print *, "Checking pointer association on hybrid_data%levelpool_state for hybrid reservoirs"
        if ( associated(hybrid_data%state%levelpool_ptr%state) ) then
            print *, "PASSED"
            ptr_state(5) = .true.
       else
            print *, "FAILED"
            ptr_state(5) = .false.
        end if
        print *, " "

        ! Check to see if the hybrid levelpool_properties data structure exists
        print *, "Checking pointer association on data%levelpool_properties for hybrid reservoirs"
        if ( associated(hybrid_data%state%levelpool_ptr%properties) ) then
            print *, "PASSED"
            ptr_state(6) = .true.
        else
            print *, "FAILED"
            ptr_state(6) = .false.
        end if
        print *, " "

        ! Check to see if the hybrid levelpool_input data structure exists
        print *, "Checking pointer association on data%levelpool_input for hybrid reservoirs"
        if ( associated(hybrid_data%state%levelpool_ptr%input) ) then
            print *, "PASSED"
            ptr_state(7) = .true.
        else
            print *, "FAILED"
            ptr_state(7) = .false.
        end if
        print *, " "

        ! Check to see if the hybrid levelpool_output data structure exists
        print *, "Checking pointer association on data%levelpool_output for hybrid reservoirs"
        if ( associated(hybrid_data%state%levelpool_ptr%output) ) then
            print *, "PASSED"
            ptr_state(8) = .true.
        else
            print *, "FAILED"
            ptr_state(8) = .false.
        end if
        print *, " "


        ! Now check the data members of each substructure
        if ( ptr_state(1) ) then
            data_state(1) = test_persistence_levelpool_hybrid_state(hybrid_data%state)
        end if

        if ( ptr_state(2) ) then
            data_state(2) = test_persistence_levelpool_hybrid_properties(hybrid_data%properties)
        end if

        if ( ptr_state(3) ) then
            data_state(3) = test_input_persistence_levelpool_hybrid(hybrid_data%input)
        end if

        if ( ptr_state(4) ) then
            data_state(4) = test_output_persistence_levelpool_hybrid(hybrid_data%output)
        end if

        ! Test Levelpool Substructures
        if ( ptr_state(5) ) then
            data_state(5) = test_hybrid_levelpool_state(hybrid_data%state)
        end if

        if ( ptr_state(6) ) then
            data_state(6) = test_hybrid_levelpool_properties(hybrid_data%state)
        end if

        if ( ptr_state(7) ) then
            data_state(7) = test_hybrid_levelpool_input(hybrid_data%state%levelpool_ptr%input)
        end if

        if ( ptr_state(8) ) then
            data_state(8) = test_hybrid_levelpool_output(hybrid_data%state%levelpool_ptr%output)
        end if


        if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All Hybrid Object Tests Passed"
            print *, "========================================================================"
            rv = .true.

        else
            print *, "========================================================================"
            print *, "Not All Hybrid Object Tests Passed"
            print *, "========================================================================"

        end if

        flush(6)
    end function persistence_levelpool_hybrid_data_info


    ! Test to see that each member of the input structure is correctly allocated and readable
    function test_input_persistence_levelpool_hybrid(o) result(rv)
        type (reservoir_input), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the input structure"
        print *, " "

        print *, "Checking read on inflow"
        print *, o%inflow
        if ( o%inflow .lt. 0.0 - epsilon(0.0) .or. o%inflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lateral_inflow"
        print *, o%lateral_inflow
        if ( o%lateral_inflow .lt. 0.0 - epsilon(0.0) .or. o%lateral_inflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on previous_timestep_inflow"
        print *, o%previous_timestep_inflow
        if ( o%previous_timestep_inflow .lt. 0.0 - epsilon(0.0) .or. o%previous_timestep_inflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_input_persistence_levelpool_hybrid

    ! Test to see that each member of the output structure is correctly allocated and readable
    function test_output_persistence_levelpool_hybrid(o) result(rv)
        type (reservoir_output), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the output structure"
        print *, " "

        print *, "Checking read on outflow"
        print *, o%outflow
        if ( o%outflow .lt. 0.0 - epsilon(0.0) .or. o%outflow .gt. 0.0 + epsilon(0.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_output_persistence_levelpool_hybrid

    ! Test to see that each member of the state structure is correctly allocated and readable
    function test_persistence_levelpool_hybrid_state(o) result(rv)
        type (hybrid_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the hybrid state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%water_elevation
        if ( o%water_elevation .lt. 1333.10938 - epsilon(1333.109380) .or. o%water_elevation .gt. 1333.10938 + epsilon(1333.10938) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on current_storage"
        print *, o%current_storage
        if ( o%current_storage .lt. 3.90669926E+09 - epsilon(3.90669926E+09) .or. o%current_storage .gt. 3.90669926E+09 + epsilon(3.90669926E+09) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on gage_discharge"
        print *, o%gage_discharge
        if ( o%gage_discharge .lt. 0.0 - epsilon(0.0) .or. o%gage_discharge .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on persisted_outflow"
        print *, o%persisted_outflow
        if ( o%persisted_outflow .lt. 0.0 - epsilon(0.0) .or. o%persisted_outflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weight_update_time"
        print *, o%weight_update_time
        if ( o%weight_update_time .ne. 0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on timeslice_update_time"
        print *, o%timeslice_update_time
        if ( o%timeslice_update_time .ne. 0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on current_time"
        print *, o%current_time
        if ( o%current_time .ne. 0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on persistence_weight_index"
        print *, o%persistence_weight_index
        if ( o%persistence_weight_index .ne. 12) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on levelpool_current_weight"
        print *, o%levelpool_current_weight
        if ( o%levelpool_current_weight .lt. 0.0 - epsilon(0.0) .or. o%levelpool_current_weight .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on persistence_current_weight"
        print *, o%persistence_current_weight
        if ( o%persistence_current_weight .lt. 0.0 - epsilon(0.0) .or. o%persistence_current_weight .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_persistence_levelpool_hybrid_state


    ! Test to see that each member of the properties structure is correctly allocated and readable
    function test_persistence_levelpool_hybrid_properties(o) result(rv)
        type (hybrid_properties_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the hybrid properties data structure"
        print *, " "

        print *, "Checking read on min_storage"
        print *, o%min_storage
        if ( o%min_storage .lt. 0.0 - epsilon(0.0) .or. o%min_storage .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on max_storage"
        print *, o%max_storage
        if ( o%max_storage .lt. 4.34078003E+09 - epsilon(4.34078003E+09) .or. o%max_storage .gt. 4.34078003E+09 + epsilon(4.34078003E+09) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lake_area"
        print *, o%lake_area
        if ( o%lake_area .lt. 209632000.0 - epsilon(209632000.0) .or. o%lake_area .gt. 209632000.0 + epsilon(209632000.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_elevation"
        print *, o%orifice_elevation
        if ( o%orifice_elevation .lt. 1314.47339 - epsilon(1314.47339) .or. o%orifice_elevation .gt. 1314.47339 + epsilon(1314.47339) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%lake_number
        if ( o%lake_number .ne. 402142) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on gage_id"
        print *, o%gage_id
        if ( ADJUSTL(trim(o%gage_id)) .ne. '07198000') then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on observation_lookback_hours"
        print *, o%observation_lookback_hours
        if ( o%observation_lookback_hours .ne. 12) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on observation_update_time_interval_seconds"
        print *, o%observation_update_time_interval_seconds
        if ( o%observation_update_time_interval_seconds .ne. 1000000000) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weight_update_time_interval"
        print *, o%weight_update_time_interval
        if ( o%weight_update_time_interval .ne. 86400) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on persistence_weighted_coefficients"
        print *, o%persistence_weighted_coefficients
        if ( o%persistence_weighted_coefficients(1) .lt. 1.0 - epsilon(1.0) .or. o%persistence_weighted_coefficients(1) .gt. 1.0 + epsilon(1.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_persistence_levelpool_hybrid_properties


    ! Levelpool data tests
    ! Test to see that each member of the levelpool input structure is correctly allocated and readable
    function test_hybrid_levelpool_input(o) result(rv)
        type (reservoir_input), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the hybrid levelpool input structure"
        print *, " "

        print *, "Checking read on inflow"
        print *, o%inflow
        if ( o%inflow .lt. 0.0 - epsilon(0.0) .or. o%inflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lateral_inflow"
        print *, o%lateral_inflow
        if ( o%lateral_inflow .lt. 0.0 - epsilon(0.0) .or. o%lateral_inflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on previous_timestep_inflow"
        print *, o%previous_timestep_inflow
        if ( o%previous_timestep_inflow .lt. 0.0 - epsilon(0.0) .or. o%previous_timestep_inflow .gt. 0.0 + epsilon(0.0) )then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_hybrid_levelpool_input

    ! Test to see that each member of the levelpool output structure is correctly allocated and readable
    function test_hybrid_levelpool_output(o) result(rv)
        type (reservoir_output), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the hybrid levelpool output structure"
        print *, " "

        print *, "Checking read on outflow"
        print *, o%outflow
        if ( o%outflow .lt. 0.0 - epsilon(0.0) .or. o%outflow .gt. 0.0 + epsilon(0.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_hybrid_levelpool_output

    ! Test to see that each member of the levelpool state structure is correctly allocated and readable
    function test_hybrid_levelpool_state(o) result(rv)
        type (hybrid_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the hybrid levelpool state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%levelpool_ptr%state%water_elevation
        if ( o%levelpool_ptr%state%water_elevation .lt. 0.0 - epsilon(0.0) .or. o%levelpool_ptr%state%water_elevation .gt. 0.0 + epsilon(0.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_hybrid_levelpool_state


    ! Test to see that each member of the levelpool properties structure is correctly allocated and readable
    function test_hybrid_levelpool_properties(o) result(rv)
        type (hybrid_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the hybrid levelpool properties data structure"
        print *, " "

        print *, "Checking read on lake_area"
        print *, o%levelpool_ptr%properties%lake_area
        if ( o%levelpool_ptr%properties%lake_area .lt. 209.632004 - epsilon(209.632004) .or. o%levelpool_ptr%properties%lake_area .gt. 209.632004 + epsilon(209.632004) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_elevation"
        print *, o%levelpool_ptr%properties%weir_elevation
        if ( o%levelpool_ptr%properties%weir_elevation .lt. 1332.07410 - epsilon(1332.07410) .or. o%levelpool_ptr%properties%weir_elevation .gt. 1332.07410 + epsilon(1332.07410) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_coeffecient"
        print *, o%levelpool_ptr%properties%weir_coeffecient
        if ( o%levelpool_ptr%properties%weir_coeffecient .lt. 0.400000006 - epsilon(0.400000006) .or. o%levelpool_ptr%properties%weir_coeffecient .gt. 0.400000006 + epsilon(0.400000006) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_length"
        print *, o%levelpool_ptr%properties%weir_length
        if ( o%levelpool_ptr%properties%weir_length .lt. 10.0 - epsilon(10.0) .or. o%levelpool_ptr%properties%weir_length .gt. 10.0 + epsilon(10.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_elevation"
        print *, o%levelpool_ptr%properties%orifice_elevation
        if ( o%levelpool_ptr%properties%orifice_elevation .lt. 1314.47339 - epsilon(1314.47339) .or. o%levelpool_ptr%properties%orifice_elevation .gt. 1314.47339 + epsilon(1314.47339) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_coefficient"
        print *, o%levelpool_ptr%properties%orifice_coefficient
        if ( o%levelpool_ptr%properties%orifice_coefficient .lt. 0.100000001 - epsilon(0.100000001) .or. o%levelpool_ptr%properties%orifice_coefficient .gt. 0.100000001 + epsilon(0.100000001) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_area"
        print *, o%levelpool_ptr%properties%orifice_area
        if ( o%levelpool_ptr%properties%orifice_area .lt. 1.0 - epsilon(1.0) .or. o%levelpool_ptr%properties%orifice_area .gt. 1.0 + epsilon(1.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on max_depth"
        print *, o%levelpool_ptr%properties%max_depth
        if ( o%levelpool_ptr%properties%max_depth .lt. 1335.18005 - epsilon(1335.18005) .or. o%levelpool_ptr%properties%max_depth .gt. 1335.18005 + epsilon(1335.18005) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%levelpool_ptr%properties%lake_number
        if ( o%levelpool_ptr%properties%lake_number .ne. 402142) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_hybrid_levelpool_properties

end module
