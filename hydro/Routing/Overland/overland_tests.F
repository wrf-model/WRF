! overland_tests.F
! Purpose: This program contains unit tests for the module overland_data
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson

program overland_tests
    use overland_data
    implicit none

    logical :: rv

    write(6,*) "Running Test 1"
    write(6,*) "Test that the allocation and deallocation functions work correctly"
    rv = init_delete_test(100,100)
    write(6,*) "Test Complete"

    write(6,*) "Running Test 2"
    write(6,*) "Testing scaling"
    rv = scale_tests()
    write(6,*) "Test Complete"

    write(6,*) "Running Test 3"
    write(6,*) "Testing double init"
    rv = double_init_test()
    write(6,*) "Test Complete"

    write(6,*) "Running Test 4"
    write(6,*) "Testing double destroy"
    rv = double_destroy_test()
    write(6,*) "Test Complete"
    contains

    function init_delete_test(ix,jx) result(rv)
        implicit none

        integer, intent(in) :: ix
        integer, intent(in) :: jx
        logical:: rv

        logical :: status = .true.

        type (overland_struct) :: overland_data

        ! initalize the structure
        call overland_data%init(ix,jx,ix,jx)

        ! test to see if control was associated
        if ( associated(overland_data%control) ) then
            !write(6,*) "control type was associated"
        else
            !write(6,*) "control type not associated"
            status = .false.
        end if

        ! test to see if streams_and_lakes was associated
        if ( associated(overland_data%streams_and_lakes) ) then
            !write(6,*) "streams and lakes type was associated"
        else
            !write(6,*) "streams and lakes type not associated"
            status = .false.
        end if

        ! test to see if streams_and_lakes was associated
        if ( associated(overland_data%properties) ) then
            !write(6,*) "properties type was associated"
        else
            !write(6,*) "properties type not associated"
            status = .false.
        end if

        ! test to see if streams_and_lakes was associated
        if ( associated(overland_data%mass_balance) ) then
            !write(6,*) "mass_balance type was associated"
        else
            !write(6,*) "mass_balance type not associated"
            status = .false.
        end if

        ! destroy the structure
        call overland_data%destroy

        ! test to see if control was associated
        if ( .not. associated(overland_data%control) ) then
            !write(6,*) "control type was disassociated"
        else
            !write(6,*) "control type not disassociated"
            status = .false.
        end if

        ! test to see if streams_and_lakes was associated
        if ( .not. associated(overland_data%streams_and_lakes) ) then
            !write(6,*) "streams and lakes type was disassociated"
        else
            !write(6,*) "streams and lakes type not disassociated"
            status = .false.
        end if

        ! test to see if streams_and_lakes was associated
        if ( .not. associated(overland_data%properties) ) then
            !write(6,*) "properties type was disassociated"
        else
            !write(6,*) "properties type not disassociated"
            status = .false.
        end if

        ! test to see if streams_and_lakes was associated
        if ( .not. associated(overland_data%mass_balance) ) then
            !write(6,*) "mass_balance type was disassociated"
        else
            !write(6,*) "mass_balance type not disassociated"
            status = .false.
        end if

        ! write final test results
        if ( status ) then
            write(6,*) "Test Passed"
        else
            write(6,*) "Test Failed"
        end if

        rv = status
    end function init_delete_test

    function scale_tests() result(rv)
        logical :: rv

        logical, dimension(4) :: results
        results = .false.

        write(6,*) "Running Test for (10,10)"
        results(1) = init_delete_test(10,10)
        write(6,*) "Running Test for (100,100)"
        results(2) = init_delete_test(100,100)
        write(6,*) "Running Test for (1000,1000)"
        results(3) = init_delete_test(1000,1000)
        write(6,*) "Running Test for (5000,5000)"
        results(4) = init_delete_test(5000,5000)

        if ( all(results) ) then
            rv = .true.
            write(6,*) "All Sub-Test Passed"
        else
            rv = .false.
            write(6,*) "At Least One Sub-Test Failed"
        end if

    end function scale_tests

    function double_init_test() result(rv)
        implicit none
        logical :: rv

        type (overland_struct) :: overland_data

        call overland_data%init(100,100,100,100)
        call overland_data%init(100,100,100,100)
        call overland_data%destroy

        write(6,*) "Test Passed"
        rv = .true.
    end function double_init_test

    function double_destroy_test() result(rv)
        implicit none
        logical :: rv

        type (overland_struct) :: overland_data

        call overland_data%init(100,100,100,100)
        call overland_data%destroy
        call overland_data%destroy

        write(6,*) "Test Passed"
        rv = .true.
    end function double_destroy_test

end program overland_tests
