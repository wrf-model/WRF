! This module holds tests for initialization of
! various attributes of a levelpool reservoir.
module module_levelpool_tests
    use module_levelpool

contains

    function levelpool_data_info(levelpool_data) result(data_info_result)
    implicit none
    type (levelpool) :: levelpool_data
    logical  :: data_info_result
    logical, dimension(4) :: ptr_state
    logical, dimension(4) :: data_state

    data_info_result = .false.

    ! Check to see if the levelpool_state data structure exists
    print *, "Checking pointer association on data%state "
    if ( associated(levelpool_data%state) ) then
        print *, "PASSED"
        ptr_state(1) = .true.
    else
        print *, "FAILED"
        ptr_state(1) = .false.
    end if

    ! Check to see if the levelpool_properties data structure exists
    print *, "Checking pointer association on data%properties "
    if ( associated(levelpool_data%properties) ) then
        print *, "PASSED"
        ptr_state(2) = .true.
    else
        print *, "FAILED"
        ptr_state(2) = .false.
    end if

    ! Check to see if the levelpool_input data structure exists
    print *, "Checking pointer association on data%input "
    if ( associated(levelpool_data%input) ) then
        print *, "PASSED"
        ptr_state(3) = .true.
    else
        print *, "FAILED"
        ptr_state(3) = .false.
    end if

    ! Check to see if the levelpool_output data structure exists
    print *, "Checking pointer association on data%output "
    if ( associated(levelpool_data%output) ) then
        print *, "PASSED"
        ptr_state(4) = .true.
    else
        print *, "FAILED"
        ptr_state(4) = .false.
    end if

    ! Now check the data members of each substructure
    if ( ptr_state(1) ) then
        data_state(1) = test_levelpool_state(levelpool_data%state)
    end if

    if ( ptr_state(2) ) then
        data_state(2) = test_levelpool_properties(levelpool_data%properties)
    end if

    if ( ptr_state(3) ) then
        data_state(3) = test_input(levelpool_data%input)
    end if

    if ( ptr_state(4) ) then
        data_state(4) = test_output(levelpool_data%output)
    end if


    if ( all(ptr_state) .and. all(data_state) ) then
        data_info_result = .true.
        print *, "========================================================================"
        print *, "All Level Pool Tests Passed"
        print *, "========================================================================"

    else
        data_info_result = .false.
        print *, "========================================================================"
        print *, "Not All Level Pool Tests Passed"
        print *, "========================================================================"
    end if

    end function levelpool_data_info


    ! Test to see that each member of the input structure is correctly allocated and readable
    function test_input(o) result(rv)
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

    end function test_input

    ! Test to see that each member of the output structure is correctly allocated and readable
    function test_output(o) result(rv)
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

    end function test_output

    ! Test to see that each member of the state structure is correctly allocated and readable
    function test_levelpool_state(o) result(rv)
        type (levelpool_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the level pool state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%water_elevation
        if ( o%water_elevation .lt. 2.0 - epsilon(2.0) .or. o%water_elevation .gt. 2.0 + epsilon(2.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_levelpool_state


    ! Test to see that each member of the properties structure is correctly allocated and readable
    function test_levelpool_properties(o) result(rv)
        type (levelpool_properties_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the level pool properties data structure"
        print *, " "

        print *, "Checking read on lake_area"
        print *, o%lake_area
        if ( o%lake_area .lt. 4.0 - epsilon(4.0) .or. o%lake_area .gt. 4.0 + epsilon(4.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_elevation"
        print *, o%weir_elevation
        if ( o%weir_elevation .lt. 6.0 - epsilon(6.0) .or. o%weir_elevation .gt. 6.0 + epsilon(6.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_coeffecient"
        print *, o%weir_coeffecient
        if ( o%weir_coeffecient .lt. 8.0 - epsilon(8.0) .or. o%weir_coeffecient .gt. 8.0 + epsilon(8.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on weir_length"
        print *, o%weir_length
        if ( o%weir_length .lt. 10.0 - epsilon(10.0) .or. o%weir_length .gt. 10.0 + epsilon(10.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_elevation"
        print *, o%orifice_elevation
        if ( o%orifice_elevation .lt. 12.0 - epsilon(12.0) .or. o%orifice_elevation .gt. 12.0 + epsilon(12.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_coefficient"
        print *, o%orifice_coefficient
        if ( o%orifice_coefficient .lt. 14.0 - epsilon(14.0) .or. o%orifice_coefficient .gt. 14.0 + epsilon(14.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on orifice_area"
        print *, o%orifice_area
        if ( o%orifice_area .lt. 16.0 - epsilon(16.0) .or. o%orifice_area .gt. 16.0 + epsilon(16.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on max_depth"
        print *, o%max_depth
        if ( o%max_depth .lt. 18.0 - epsilon(18.0) .or. o%max_depth .gt. 18.0 + epsilon(18.0) ) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%lake_number
        if ( o%lake_number .ne. 20) then
            rv = .false.
            print *, "FAILED"
        end if
        print *, " "

    end function test_levelpool_properties

end module
