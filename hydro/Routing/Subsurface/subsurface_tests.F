
program subsurface_tests
    use module_subsurface_data

    implicit none

    logical rv

    write(6,*) "Running Test 1"
    write(6,*) "Test that the allocation and deallocation functions work correctly"
    rv = init_delete_test(100,100,5)
    write(6,*) "Test Complete"

    write(6,*) "Running Test 2"
    write(6,*) "Test Scaling"
    rv = scale_tests()
    write(6,*) "Test Complete"

    write(6,*) "Running Test 3"
    write(6,*) "Testing Double Delete"
    rv = double_delete_test()
    write(6,*) "Test Complete"

    write(6,*) "Running Test 4"
    write(6,*) "Testing Double Init"
    rv = double_init_test()
    write(6,*) "Test Complete"
    contains

    function init_delete_test(ix,jx,nsoil) result(rv)
        use module_subsurface_data
        use overland_data

        implicit none

        integer, intent(in) :: ix ! horizontal resolution
        integer, intent(in) :: jx ! vertical resolution
        integer, intent(in) :: nsoil ! number of soil layers
        logical :: rv ! test result

        logical :: status_val = .true.

        type (subsurface_struct) :: subsurface_data
        type (overland_struct) :: ov_data

        ! end of variable declarations

        write(0,*) "ix = ", ix
        write(0,*) "jx = ", jx
        write(0,*) "nsoil = ", nsoil

        call ov_data%init(ix,jx,ix,jx)
        call subsurface_data%init(ix,jx,nsoil,ov_data)

        ! check to see that io was allocated
        if ( associated(subsurface_data%io) ) then
            write(6,*) "io type was associated"
        else
            write(6,*) "io type was not associated"
            status_val = .false.
        end if

        ! check to see that properties was allocated
        if ( associated(subsurface_data%properties) ) then
            write(6,*) "properties type was associated"
        else
            write(6,*) "properties type was not associated"
            status_val = .false.
        end if

        ! check to see that grid transform was allocated
        if ( associated(subsurface_data%grid_transform) ) then
            write(6,*) "grid_transfrom type was associated"
        else
            write(6,*) "grid_transfrom type was not associated"
            status_val = .false.
        end if

        call subsurface_data%destroy

         ! check to see that io was deallocated
        if ( .not. associated(subsurface_data%io) ) then
            write(6,*) "io type was disassociated"
        else
            write(6,*) "io type was not disassociated"
            status_val = .false.
        end if

        ! check to see that properties was allocated
        if ( .not. associated(subsurface_data%properties) ) then
            write(6,*) "properties type was disassociated"
        else
            write(6,*) "properties type was not disassociated"
            status_val = .false.
        end if

        ! check to see that grid transform was allocated
        if ( .not. associated(subsurface_data%grid_transform) ) then
            write(6,*) "grid_transfrom type was disassociated"
        else
            write(6,*) "grid_transfrom type was not disassociated"
            status_val = .false.
        end if

        ! write final test results
        if ( status_val ) then
            write(6,*) "Test Passed"
        else
            write(6,*) "Test Failed"
        end if

        rv = status_val

    end function init_delete_test

    function scale_tests() result(rv)
        logical :: rv

        logical, dimension(4) :: results
        results = .false.

        write(6,*) "Running Test for (10,10,5)"
        results(1) = init_delete_test(10,10,5)
        write(6,*) "Running Test for (100,100,5)"
        results(2) = init_delete_test(100,100,5)
        write(6,*) "Running Test for (1000,1000)"
        results(3) = init_delete_test(1000,1000,5)
        write(6,*) "Running Test for (5000,5000,5)"
        results(4) = init_delete_test(5000,5000,5)

        if ( all(results) ) then
            rv = .true.
            write(6,*) "All Sub-Test Passed"
        else
            rv = .false.
            write(6,*) "At Least One Sub-Test Failed"
        end if

    end function scale_tests

    function double_delete_test() result(rv)
        logical :: rv

        type (subsurface_struct) :: subsurface_data
        type (overland_struct) :: ov_data

        call ov_data%init(100,100,100,100)
        call subsurface_data%init(100,100,5,ov_data)
        call subsurface_data%destroy
        call subsurface_data%destroy

        rv = .true.

    end function double_delete_test

    function double_init_test() result(rv)
        logical :: rv

        type (subsurface_struct) :: subsurface_data
        type (overland_struct) :: ov_data

        call ov_data%init(100,100,100,100)
        call subsurface_data%init(100,100,5,ov_data)
        call subsurface_data%init(100,100,5,ov_data)
        call subsurface_data%destroy

        rv = .true.

    end function double_init_test

end program
