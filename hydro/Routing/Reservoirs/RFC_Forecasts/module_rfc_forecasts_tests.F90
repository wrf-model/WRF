! This module holds tests for initialization of
! various attributes of an RFC forecasts reservoir.
module module_rfc_forecasts_tests
    use module_rfc_forecasts
    use module_reservoir_read_rfc_time_series_data

contains

    ! Testing
    function rfc_forecasts_data_info(rfc_forecasts_reservoir_data) result(data_info_result)
        implicit none
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        logical :: data_info_result
        logical, dimension(8) :: ptr_state
        logical, dimension(8) :: data_state

        data_info_result = .false.

        ! Check to see if the rfc_forecasts_state data structure exists
        print *, "Checking pointer association on data%state for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%state) ) then
            print *, "PASSED"
            ptr_state(1) = .true.
       else
            print *, "FAILED"
            ptr_state(1) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts_properties data structure exists
        print *, "Checking pointer association on data%properties for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%properties) ) then
            print *, "PASSED"
            ptr_state(2) = .true.
        else
            print *, "FAILED"
            ptr_state(2) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts_input data structure exists
        print *, "Checking pointer association on data%input for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%input) ) then
            print *, "PASSED"
            ptr_state(3) = .true.
        else
            print *, "FAILED"
            ptr_state(3) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts_output data structure exists
        print *, "Checking pointer association on data%output for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%output) ) then
            print *, "PASSED"
            ptr_state(4) = .true.
        else
            print *, "FAILED"
            ptr_state(4) = .false.
        end if
        print *, " "

        ! Test levelpool structure
        ! Check to see if the rfc_forecasts levelpool_state data structure exists
        print *, "Checking pointer association on data%levelpool_state for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%state%levelpool_ptr%state) ) then
            print *, "PASSED"
            ptr_state(5) = .true.
       else
            print *, "FAILED"
            ptr_state(5) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts levelpool_properties data structure exists
        print *, "Checking pointer association on data%levelpool_properties for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%state%levelpool_ptr%properties) ) then
            print *, "PASSED"
            ptr_state(6) = .true.
        else
            print *, "FAILED"
            ptr_state(6) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts levelpool_input data structure exists
        print *, "Checking pointer association on data%levelpool_input for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%state%levelpool_ptr%input) ) then
            print *, "PASSED"
            ptr_state(7) = .true.
        else
            print *, "FAILED"
            ptr_state(7) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts levelpool_output data structure exists
        print *, "Checking pointer association on data%levelpool_output for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%state%levelpool_ptr%output) ) then
            print *, "PASSED"
            ptr_state(8) = .true.
        else
            print *, "FAILED"
            ptr_state(8) = .false.
        end if
        print *, " "


        ! Now check the data members of each substructure
        if ( ptr_state(1) ) then
            data_state(1) = test_rfc_forecasts_state(rfc_forecasts_reservoir_data%state)
        end if

        if ( ptr_state(2) ) then
            data_state(2) = test_rfc_forecasts_properties(rfc_forecasts_reservoir_data%properties)
        end if

        if ( ptr_state(3) ) then
            data_state(3) = test_input_rfc_forecasts(rfc_forecasts_reservoir_data%input)
        end if

        if ( ptr_state(4) ) then
            data_state(4) = test_output_rfc_forecasts(rfc_forecasts_reservoir_data%output)
        end if

        ! Test Levelpool Substructures
        if ( ptr_state(5) ) then
            data_state(5) = test_rfc_forecasts_levelpool_state(rfc_forecasts_reservoir_data%state)
        end if

        if ( ptr_state(6) ) then
            data_state(6) = test_rfc_forecasts_levelpool_properties(rfc_forecasts_reservoir_data%state)
        end if

        if ( ptr_state(7) ) then
            data_state(7) = test_rfc_forecasts_levelpool_input(rfc_forecasts_reservoir_data%state%levelpool_ptr%input)
        end if

        if ( ptr_state(8) ) then
            data_state(8) = test_rfc_forecasts_levelpool_output(rfc_forecasts_reservoir_data%state%levelpool_ptr%output)
        end if

        if ( all(ptr_state) .and. all(data_state) ) then
            data_info_result = .true.
            print *, "========================================================================"
            print *, "All RFC Forecast Reservoir Object Tests Passed"
            print *, "========================================================================"

        else
            data_info_result = .false.
            print *, "========================================================================"
            print *, "Not All RFC Forecast Reservoir Object Tests Passed"
            print *, "========================================================================"

        end if

    end function rfc_forecasts_data_info

    ! Test to see that each member of the input structure is correctly allocated and readable
    function test_input_rfc_forecasts(o) result(rv)
        type (reservoir_input), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the input structure"
        print *, " "

        print *, "Checking read on inflow"
        print *, o%inflow
        if ( o%inflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lateral_inflow"
        print *, o%lateral_inflow
        if ( o%lateral_inflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on previous_timestep_inflow"
        print *, o%previous_timestep_inflow
        if ( o%previous_timestep_inflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_input_rfc_forecasts

    ! Test to see that each member of the output structure is correctly allocated and readable
    function test_output_rfc_forecasts(o) result(rv)
        type (reservoir_output), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the output structure"
        print *, " "

        print *, "Checking read on outflow"
        print *, o%outflow
        if ( o%outflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_output_rfc_forecasts

    ! Test to see that each member of the state structure is correctly allocated and readable
    function test_rfc_forecasts_state(o) result(rv)
        type (rfc_forecasts_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc_forecasts state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%water_elevation
        if ( o%water_elevation .ne. 17.3999996) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_rfc_forecasts_state


    ! Test to see that each member of the properties structure is correctly allocated and readable
    function test_rfc_forecasts_properties(o) result(rv)
        type (rfc_forecasts_properties_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc_forecasts properties data structure"
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%lake_number
        if ( o%lake_number .ne. 3745478) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on rfc_gage_id"
        print *, o%rfc_gage_id
        if ( o%rfc_gage_id .ne. "DIEA4") then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_rfc_forecasts_properties


    ! Levelpool data tests
    ! Test to see that each member of the levelpool input structure is correctly allocated and readable
    function test_rfc_forecasts_levelpool_input(o) result(rv)
        type (reservoir_input), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc forecasts levelpool input structure"
        print *, " "

        print *, "Checking read on inflow"
        print *, o%inflow
        if ( o%inflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lateral_inflow"
        print *, o%lateral_inflow
        if ( o%lateral_inflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on previous_timestep_inflow"
        print *, o%previous_timestep_inflow
        if ( o%previous_timestep_inflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_rfc_forecasts_levelpool_input

    ! Test to see that each member of the levelpool output structure is correctly allocated and readable
    function test_rfc_forecasts_levelpool_output(o) result(rv)
        type (reservoir_output), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc forecasts levelpool output structure"
        print *, " "

        print *, "Checking read on outflow"
        print *, o%outflow
        if ( o%outflow .ne. 0.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_rfc_forecasts_levelpool_output

    ! Test to see that each member of the levelpool state structure is correctly allocated and readable
    function test_rfc_forecasts_levelpool_state(o) result(rv)
        type (rfc_forecasts_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc forecasts levelpool state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%levelpool_ptr%state%water_elevation
        if ( o%levelpool_ptr%state%water_elevation .ne. 2.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_rfc_forecasts_levelpool_state


    ! Test to see that each member of the levelpool properties structure is correctly allocated and readable
    function test_rfc_forecasts_levelpool_properties(o) result(rv)
        type (rfc_forecasts_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc forecasts levelpool properties data structure"
        print *, " "

        print *, "Checking read on lake_area"
        print *, o%levelpool_ptr%properties%lake_area
        if ( o%levelpool_ptr%properties%lake_area .ne. 4.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_elevation"
        print *, o%levelpool_ptr%properties%weir_elevation
        if ( o%levelpool_ptr%properties%weir_elevation .ne. 6.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_coeffecient"
        print *, o%levelpool_ptr%properties%weir_coeffecient
        if ( o%levelpool_ptr%properties%weir_coeffecient .ne. 8.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_length"
        print *, o%levelpool_ptr%properties%weir_length
        if ( o%levelpool_ptr%properties%weir_length .ne. 10.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_elevation"
        print *, o%levelpool_ptr%properties%orifice_elevation
        if ( o%levelpool_ptr%properties%orifice_elevation .ne. 12.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_coefficient"
        print *, o%levelpool_ptr%properties%orifice_coefficient
        if ( o%levelpool_ptr%properties%orifice_coefficient .ne. 14.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_area"
        print *, o%levelpool_ptr%properties%orifice_area
        if ( o%levelpool_ptr%properties%orifice_area .ne. 16.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on max_depth"
        print *, o%levelpool_ptr%properties%max_depth
        if ( o%levelpool_ptr%properties%max_depth .ne. 18.0) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%levelpool_ptr%properties%lake_number
        if ( o%levelpool_ptr%properties%lake_number .ne. 3745478) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_rfc_forecasts_levelpool_properties

end module
