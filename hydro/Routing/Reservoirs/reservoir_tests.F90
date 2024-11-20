! This program unit tests the various reservoir implementations along with edge cases.
! It is important to run these unit tests whenever making any changes to code in this
! module to ensure nothing is broken. If nothing is broken, the user will see
! "All Reservoir Tests Passed". To compile and run these tests, go to the Reservoir
! directory in the terminal and type "make" and then "make test". Then type "./reservoir_tests".

program reservoir_unit_tests
    use module_levelpool_tests
    use module_persistence_levelpool_hybrid_tests
    use module_rfc_forecasts_tests

    implicit none

    logical :: rv1 = .false.
    logical :: rv2 = .false.
    logical :: rv3 = .false.
    logical :: rv4 = .false.
    logical :: rv5 = .false.
    logical :: rv6 = .false.
    logical :: rv7 = .false.
    logical :: rv8 = .false.
    logical :: rv9 = .false.
    logical :: rv10 = .false.
    logical :: rv11 = .false.
    logical :: rv12 = .false.
    logical :: rv13 = .false.
    logical :: rv14 = .false.
    logical :: rv15 = .false.
    logical :: rv16 = .false.
    logical :: rv17 = .false.

    real, dimension(120) :: inflow_array

    inflow_array = (/189.22899, 189.27005, 189.31049, 189.35042, 189.38965, 189.42819, 189.46588, 189.50273, &
    189.53859, 189.57346, 189.60719, 189.63979, 189.6711, 189.7011, 189.72968, &
    189.75679, 189.7823, 189.80617, 189.82822, 189.84842, 189.86653, 189.88255, 189.89622, &
    189.90752, 189.91612, 189.922, 189.92482, 189.92447, 189.92067, 189.91319, 189.90175, &
    189.88611, 189.86592, 189.84088, 189.81064, 189.77487, 189.73317, 189.6852, 189.63051, &
    189.56873, 189.49939, 189.42207, 189.33635, 189.24176, 189.13782, 189.02408, &
    188.90009, 188.76535, 188.61945, 188.46188, 188.29224, 188.11006, 187.91493, 187.70644, &
    187.48419, 187.24779, 186.9969, 186.73119, 186.45035, 186.15407, 185.84213, 185.51424, &
    185.17023, 184.80989, 184.43312, 184.03975, 183.62973, 183.20296, 182.75943, 182.29909, &
    181.82205, 181.32828, 180.81792, 80.29099, 179.74774, 179.1882, 178.61267, 178.02129, &
    177.41437, 176.79207, 176.15475, 175.50269, 174.83627, 174.15576, 173.46162, &
    172.75417, 172.03389, 171.3011, 170.55634, 169.79997, 169.03255, 168.25441, 167.46616, &
    166.66815, 165.86099, 165.04509, 164.22101, 163.38913, 162.55011, 161.70428, 160.85229, &
    159.99452, 159.13156, 158.26382, 157.39188, 156.51611, 155.63715, 154.75531, 153.8712, 152.98517, &
    152.09779, 151.2094, 150.32057, 149.43166, 148.54315, 147.6554, 146.76892, 145.88405, 145.00128, 144.12091/)

    rv1 = test_levelpool()

    rv2 = test_levelpool_overflow_max_height()

    rv3 = test_persistence_levelpool_hybrid_usgs()

    rv4 = test_persistence_levelpool_hybrid_usace()

    rv5 = test_rfc_forecasts_object()

    rv6 = test_rfc_forecasts_time_series_object()

    rv7 = test_rfc_forecasts_levelpool_fallback()

    rv8 = test_rfc_forecasts_time_series_output_with_lookback_and_offset()

    rv9 = test_rfc_forecasts_time_series_output_with_negative_data()

    rv10 = test_rfc_forecasts_time_series_output_all_synthetic_data()

    rv11 = test_rfc_forecasts_data_exceeding_max_range()

    rv12 = test_rfc_forecasts_with_offset_for_extended_AnA()

    rv13 = test_persistence_levelpool_hybrid_usace_overtop()

    rv14 = test_persistence_levelpool_hybrid_usace_no_timeslice_available()

    rv15 = test_ak_rfc_forecasts_pass_through_fallback()

    rv16 = test_ak_rfc_forecasts_time_series_with_lookback_and_offset()

    rv17 = test_rfc_forecasts_over_max_water_elevation()

    if (rv1 .and. rv2 .and. rv3 .and. rv4 .and. rv5 .and. rv6 .and. rv7 .and. rv8 .and. rv9 .and. &
    rv10 .and. rv11 .and. rv12 .and. rv13 .and. rv14 .and. rv15 .and. rv16 .and. rv17) then
        print *, "========================================================================"
        print *, 'All Reservoir Tests Passed'
        print *, "========================================================================"

    else
        print *, "========================================================================"
        print *, 'Not All Reservoir Tests Passed'
        print *, "========================================================================"
    end if

    contains

    !------------------------------------------------------------------------------!
    !                              test_levelpool()                                !
    ! this function verifies that the constructor for the levelpool type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_levelpool() result(rv)
        implicit none

        logical rv                        ! test result
        logical :: call_status

        type (levelpool) :: levelpool_reservoir_data
        real :: water_elevation = 2.

        integer(kind=int64):: lake_number = 20

        call_status = .false.

        print *, "calling init for levelpool"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 11., 12., 14., 16., 18., lake_number)

        print *, "testing data in levelpool"
        call_status = levelpool_data_info(levelpool_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_levelpool


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function for USGS reservoirs.
    ! It also reads the USGS persistence parameters from the provided reservoir parameter file and
    ! a gage discharge from the provided USGS Timeslice file.
    function test_persistence_levelpool_hybrid_usgs() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv1 = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 402142
        reservoir_type = 2

        print *, "calling init for persistence_levelpool_hybrid USGS type reservoir"

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coefficient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, &
        initial_fractional_depth, lake_number, reservoir_type, "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", &
        "2010-10-01_07:00:00", cwd_full, cwd_full, 12, 1000000000)

        print *, "testing data in persistence_levelpool_hybrid"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)

        print *, "calling reservoir run for persistence_levelpool_hybrid USGS type reservoir"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 13.73367 - epsilon(13.73367) .and. outflow .le. 13.73367 + epsilon(13.73367) &
        .and. dynamic_reservoir_type == 2 .and. assimilated_value .ge. 13.73367 - epsilon(13.73367) &
        .and. assimilated_value .le. 13.73367 + epsilon(13.73367) .and. &
        assimilated_source_file == "2010-10-01_06:00:00.15min.usgsTimeSlice.ncdf") then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USGS Reservoir Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USGS reservoir Test Failed'
            print *, 'Outflow should be 13.7336712'
            print *, 'dynamic_reservoir_type needs to be 2 for USGS persistence output'
            print *, "========================================================================"
            print *, outflow
        end if

        if (rv1 .and. rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid_usgs


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function for U.S. Army Corps of Engineers (USACE)
    ! type reservoirs. It also reads the USACE persistence parameters from the provided reservoir parameter file
    ! and a gage discharge from the provided USACE Timeslice file.
    function test_persistence_levelpool_hybrid_usace() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 4672717
        reservoir_type = 3

        print *, "calling init for persistence_levelpool_hybrid USACE type reservoir"

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coefficient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, &
        max_depth, initial_fractional_depth, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", &
        "2016-10-10_02:00:00", cwd_full, cwd_full, 12, 1000000000)

        print *, "calling reservoir run for persistence_levelpool_hybrid for USACE type reservoir"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 16.0820713 - epsilon(16.0820713) .and. outflow .le. 16.0820713 + epsilon(16.0820713) &
        .and. dynamic_reservoir_type == 3 .and. assimilated_value .ge. 16.082071 - epsilon(16.082071) &
        .and. assimilated_value .le. 16.082071 + epsilon(16.082071) .and. &
        assimilated_source_file == "2016-10-10_00:00:00.15min.usaceTimeSlice.ncdf") then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir Test Failed'
            print *, 'Outflow should be 16.08207'
            print *, 'dynamic_reservoir_type needs to be 3 for USACE persistence output'
            print *, "========================================================================"
            print *, outflow
        end if

        if (rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid_usace


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function USACE
    ! type reservoirs to output level pool whenever it reaches the overtop condition.
    function test_persistence_levelpool_hybrid_usace_overtop() result(rv)

        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coeffecient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 4672717
        reservoir_type = 3

        print *, "calling init for persistence_levelpool_hybrid USACE type reservoir"

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, &
        max_depth, initial_fractional_depth, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", &
        "2016-10-10_02:00:00", cwd_full, cwd_full, 12, 1000000000)

        print *, "calling reservoir run for persistence_levelpool_hybrid for USACE type reservoir"

        ! This initial water elevation sets to reservoir overtop
        water_elevation = 1338.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 229.226059 - epsilon(229.226059) .and. outflow .le. 229.226059 + epsilon(229.226059) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir Overflow Max Height Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir Overflow Max Height Test Failed'
            print *, 'Outflow should be 16.08207'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of USACE persistence'
            print *, "========================================================================"
            print *, outflow
        end if

        if (rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid_usace_overtop


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function USACE
    ! type reservoirs to output level pool whenever no timeslices are available.
    function test_persistence_levelpool_hybrid_usace_no_timeslice_available() result(rv)

        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coeffecient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 347987  ! the corresponding USACE gage id is CA00265
        reservoir_type = 3

        print *, "calling init for persistence_levelpool_hybrid USACE type reservoir"

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, &
        max_depth, initial_fractional_depth, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", &
        "2016-10-12_02:00:00", cwd_full, cwd_full, 12, 1000000000)

        print *, "calling reservoir run for persistence_levelpool_hybrid for USACE type reservoir"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir No Timeslice Available Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir No Timeslice Available Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of USACE persistence'
            print *, "========================================================================"
            print *, outflow
        end if

        if (rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid_usace_no_timeslice_available


    ! This tests the reservoir function of the level pool module under the specific condition
    ! where the water elevation reaches the max height.
    function test_levelpool_overflow_max_height() result(rv)

        implicit none
        logical rv                       ! test result
        type (levelpool) :: levelpool_reservoir_data
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation
        real :: orifice_coefficient, orifice_area, max_depth
        integer(kind=int64):: lake_number
        real :: inflow, prev_time_inflow, outflow, water_elevation
        real, dimension(108) :: inflow_array_overflow
        integer :: timestep_count
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        rv = .false.
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0

        lake_area = 1.509490013122558594e+01
        weir_elevation = 9.626000022888183238e+00
        weir_coefficient = 0.4
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 7.733333269755045869e+00
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 9.960000038146972656e+00
        lake_number = 16944276

        inflow_array_overflow = (/91.27196, 91.7394, 92.15904, 92.1518, 91.84663, &
        91.38554, 90.86131, 90.32736, 89.81273, 89.3325, 88.89427, 88.5025, 88.16228, &
        87.41539, 86.80043, 86.03979, 85.3849, 85.33451, 86.84274, 91.6084, 101.81398, &
        118.85916, 143.99232, 177.7355, 219.2348, 267.22351, 319.90402, 374.54324, 428.86066, &
        480.92096, 529.23584, 572.77673, 610.93237, 643.4389, 670.28516, 691.67767, 707.96088, &
        719.57312, 726.96997, 730.63269, 731.03186, 728.61438, 723.79578, 716.9549, 708.43268, &
        698.53247, 687.52112, 675.63123, 663.06421, 649.99976, 636.57898, 622.92926, 609.1745, &
        595.40369, 581.68799, 568.08588, 554.64484, 541.4032, 528.39185, 515.63513, 503.14838, &
        490.95123, 479.05109, 467.45493, 456.16663, 445.18753, 434.51706, 424.15311,414.0921, &
        404.32956, 394.86014, 385.67789, 376.77621, 368.14966, 359.78958, 351.68875, 343.83972, &
        336.23505, 328.86719, 321.7287, 314.81219, 308.11047, 301.61646, 295.32312, 289.22369, &
        283.31207, 277.5813, 272.02521, 266.63776, 261.41315, 256.34564, 251.42978, 246.66023, &
        242.03192, 237.53989, 233.17944, 228.94595, 224.83511, 220.84265, 216.96449, 213.19672, &
        209.53554, 205.97734, 202.51857, 199.1559, 195.88605, 192.70595, 189.61255 /)

        call levelpool_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coefficient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, lake_number)

        water_elevation = 9.73733330

        print *, "outflow"

        do timestep_count = 1, 108
            inflow = inflow_array_overflow(timestep_count)
            call levelpool_reservoir_data%run(inflow, inflow, 0.0, water_elevation, outflow, 300.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 17.0451450 - epsilon(17.0451450) .and. outflow .le. 17.0451450 + epsilon(17.0451450) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv = .true.
            print *, "========================================================================"
            print *, 'Levelpool Overflow Max Height Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Levelpool Overflow Max Height Test Failed'
            print *, 'Final outflow should be 17.0451450'
             print *, 'dynamic_reservoir_type needs to be 1 for level pool type'
            print *, "========================================================================"
        end if

    end function test_levelpool_overflow_max_height


    ! This tests the basic object creation of an RFC Forecast type reservoir.
    function test_rfc_forecasts_object() result(rv)
        implicit none

        logical rv                        ! test result
        logical :: call_status

        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        type (time_series_data_type) :: time_series_data
        character*256 :: cwd_full
        integer(kind=int64):: lake_number

        real :: water_elevation = 2.

        lake_number = 3745478

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call_status = .false.

        print *, "calling init for rfc_forecasts"
        call rfc_forecasts_reservoir_data%init(water_elevation, 4., 6., 8., 10., 11., 12., 14., 16., 18., 0.9, lake_number, 4, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-08-18_07:00:00", cwd_full, 24)

        print *, "testing data in rfc_forecasts"

        call_status = rfc_forecasts_data_info(rfc_forecasts_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_rfc_forecasts_object


    ! This tests the object creation and function of the RFC Forecasts Time Series Reader.
    function test_rfc_forecasts_time_series_object() result(rv)
        implicit none

        logical rv                        ! test result
        logical :: call_status
        type (time_series_data_type) :: time_series_data, time_series_data_second
        character*256 :: cwd_full
        integer :: lookback_seconds, total_counts, observed_counts, time_step_seconds
        integer :: timeslice_offset_hours
        real, allocatable, dimension(:) :: forecast_discharges
        logical :: forecast_found
        logical :: all_discharges_synthetic
        character(len=256):: time_series_file_name
        integer :: lake_number

        lake_number = 3745478

        call_status = .false.

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        forecast_found = .false.

        all_discharges_synthetic = .false.

        timeslice_offset_hours = 3

        call time_series_data%init("2019-08-18_07:00:00", cwd_full, 24, timeslice_offset_hours, "CCHC1", lake_number, &
        lookback_seconds, total_counts, observed_counts, time_step_seconds, forecast_discharges, forecast_found, &
        all_discharges_synthetic, time_series_file_name)

        print *, "Checking reading of time series file"
        if ( forecast_discharges(8) .ge. 0.8 - epsilon(0.8) .and. forecast_discharges(8) .le. 0.8 + epsilon(0.8) ) then
            print *, "========================================================================"
            print *, "All RFC Forecast Reservoir Time Series Object Tests Passed"
            print *, "========================================================================"
            rv = .true.
        else
            print *, "========================================================================"
            print *, "Not All RFC Forecast Reservoir Time Series Object Tests Passed"
            print *, '8th discharge in the discharge array should be 0.8'
            print *, "========================================================================"
        end if

    end function test_rfc_forecasts_time_series_object


    ! This tests an RFC Forecasts Reservoir functionality to run levelpool whenever it does
    ! not find its corresponding time series file within a given lookback window.
    function test_rfc_forecasts_levelpool_fallback() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 3745478
        reservoir_type = 4

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-05-01_22:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Levelpool Fallback Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Levelpool Fallback Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of RFC forecast'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_levelpool_fallback


    ! This tests an RFC Forecast Reservoirs functionality to offset 3 hours in the future
    ! and look back up to 24 hours to find a time series file and output the appropriate
    ! discharge in the array that matches up with model time as would be done in a standard
    ! analysis and assimilation run.
    function test_rfc_forecasts_time_series_output_with_lookback_and_offset() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-08-18_09:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 80

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 3.6 - epsilon(3.6) .and. outflow .le. 3.6 + epsilon(3.6) .and. &
        water_elevation .ge. 1331.43604 - epsilon(1331.43604) .and. water_elevation .le. 1331.43604 + epsilon(1331.43604) .and. &
        dynamic_reservoir_type == 4 .and. assimilated_value .ge. 3.6 - epsilon(3.6) &
        .and. assimilated_value .le. 3.6 + epsilon(3.6) .and. &
        assimilated_source_file == "2019-08-18_00.60min.CCHC1.RFCTimeSeries.ncdf") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Test Failed'
            print *, 'Outflow should be 3.6'
            print *, 'dynamic_reservoir_type needs to be 4 for RFC forecast output'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_time_series_output_with_lookback_and_offset


    ! This tests an RFC Forecast Reservoir's functionality to run levelpool if
    ! it receives any values from a Time Series Forecast file that are negative.
    function test_rfc_forecasts_time_series_output_with_negative_data() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4
        cwd_full = "../../../../tests/local/reservoir_testing_files/"
        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-09-18_09:00:00", cwd_full, 24)
        water_elevation = 1331.18005
        do timestep_count = 1, 120
            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)
            prev_time_inflow = inflow
            print *, outflow
        end do
        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type
        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Negative Data Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Negative Data Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of RFC forecast'
            print *, "========================================================================"
        end if
    end function test_rfc_forecasts_time_series_output_with_negative_data
    ! This tests an RFC Forecast Reservoir's functionality to run levelpool if
    ! it receives all synthetic values from a Time Series Forecast file.
    function test_rfc_forecasts_time_series_output_all_synthetic_data() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-10-18_09:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output All Synthetic Data Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output All Synthetic Data Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of RFC forecast'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_time_series_output_all_synthetic_data


    ! This tests an RFC Forecast Reservoir's functionality to run levelpool if
    ! it receives any values from a Time Series Forecast file that are greater than the max
    ! historical MS river flow.
    function test_rfc_forecasts_data_exceeding_max_range() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4
        cwd_full = "../../../../tests/local/reservoir_testing_files/"
        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-11-18_09:00:00", cwd_full, 24)
        water_elevation = 1331.18005
        do timestep_count = 1, 120
            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)
            prev_time_inflow = inflow
            print *, outflow
        end do
        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type
        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Data Exceeding Max Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Data Exceeding Max Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of RFC forecast'
            print *, "========================================================================"
        end if
    end function test_rfc_forecasts_data_exceeding_max_range
    ! This tests an RFC Forecast Reservoirs functionality to offset 28 hours in the future
    ! and look back up to 24 hours to find a time series file and output the appropriate
    ! discharge in the array that matches up with model time as would be done in an extended
    ! analysis and assimilation run.
    function test_rfc_forecasts_with_offset_for_extended_AnA() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4
        cwd_full = "../../../../tests/local/reservoir_testing_files/"
        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_AnA_represents_Extended_MR.nc", &
        "2019-12-18_03:00:00", cwd_full, 24)
        water_elevation = 1331.18005
        do timestep_count = 1, 35
            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)
            prev_time_inflow = inflow
            print *, outflow
        end do
        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type
        if (outflow .ge. 7.8 - epsilon(7.8) .and. outflow .le. 7.8 + epsilon(7.8) &
        .and. dynamic_reservoir_type == 4 .and. assimilated_value .ge. 7.8 - epsilon(7.8) &
        .and. assimilated_value .le. 7.8 + epsilon(7.8) .and. &
        assimilated_source_file == "2019-12-19_05.60min.CCHC1.RFCTimeSeries.ncdf") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output For Extended AnA Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output For Extended AnA Test Failed'
            print *, 'Outflow should be 7.8'
            print *, 'dynamic_reservoir_type needs to be 4 for RFC forecast output'
            print *, "========================================================================"
        end if
    end function test_rfc_forecasts_with_offset_for_extended_AnA

    ! This tests an Alaska RFC Forecasts Reservoir/Glacier functionality to pass inflow directly to
    ! outflow whenever it does not find its corresponding time series file within a given
    ! lookback window.
    function test_ak_rfc_forecasts_pass_through_fallback() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.
        lake_area = 0.0
        weir_elevation = 0.0
        weir_coefficient = 0.0
        weir_length = 0.0
        dam_length = 0.0
        orifice_elevation = 0.0
        orifice_coefficient = 0.0
        orifice_area = 0.0
        max_depth = 0.0
        initial_fractional_depth = 0.0
        lake_number = 1
        reservoir_type = 5
        cwd_full = "../../../../tests/local/reservoir_testing_files/"
        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.0, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_Standard_AnA_APRFC_GDLs.nc", "2019-05-01_22:00:00", cwd_full, 24)
        water_elevation = 0.0
        do timestep_count = 1, 120
            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)
            prev_time_inflow = inflow
            print *, outflow
        end do
        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type
        if (outflow .ge. inflow - epsilon(inflow) .and. outflow .le. inflow + epsilon(inflow) &
        .and. water_elevation .ge. 0.0 - epsilon(0.0) .and. water_elevation .le. 0.0 + epsilon(0.0) &
        .and. dynamic_reservoir_type == 1 .and. assimilated_value .ge. -9999.0 - epsilon(-9999.0) &
        .and. assimilated_value .le. -9999.0 + epsilon(-9999.0) .and. &
        assimilated_source_file == "") then
            rv = .true.
            print *, "========================================================================"
            print *, 'AK RFC Forecasts Pass Through Fallback Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'AK RFC Forecasts Pass Throug Fallback Test Failed'
            print *, 'Outflow should be 144.120911'
            print *, 'dynamic_reservoir_type needs to be 1 because reservoir outputs level pool instead of RFC forecast'
            print *, "========================================================================"
        end if
    end function test_ak_rfc_forecasts_pass_through_fallback

    ! This tests an Alaska RFC Forecast Reservoir/Glacier functionality to offset 3 hours in the future
    ! and look back up to 24 hours to find a time series file and output the appropriate
    ! discharge in the array that matches up with model time as would be done in a standard
    ! analysis and assimilation run.
    function test_ak_rfc_forecasts_time_series_with_lookback_and_offset() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer(kind=int64):: lake_number
        integer :: reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 0.0
        weir_elevation = 0.0
        weir_coefficient = 0.0
        weir_length = 0.0
        dam_length = 0.0
        orifice_elevation = 0.0
        orifice_coefficient = 0.0
        orifice_area = 0.0
        max_depth = 0.0
        initial_fractional_depth = 0.0
        lake_number = 1
        reservoir_type = 5
        cwd_full = "../../../../tests/local/reservoir_testing_files/"
        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.0, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_Standard_AnA_APRFC_GDLs.nc", "2019-08-18_09:00:00", cwd_full, 24)
        water_elevation = 0.0
        do timestep_count = 1, 80
            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)
            prev_time_inflow = inflow
            print *, outflow
        end do
        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type
        if (outflow .ge. 180.392075 - epsilon(180.392075) .and. outflow .le. 180.392075 + epsilon(180.392075) &
        .and. water_elevation .ge. 0.0 - epsilon(0.0) .and. water_elevation .le. 0.0 + epsilon(0.0) &
        .and. dynamic_reservoir_type == 5 .and. assimilated_value .ge. 3.6 - epsilon(3.6) &
        .and. assimilated_value .le. 3.6 + epsilon(3.6) .and. &
        assimilated_source_file == "2019-08-18_00.60min.SNOA2.RFCTimeSeries.ncdf") then
            rv = .true.
            print *, "========================================================================"
            print *, 'AK RFC Forecasts Time Series Output Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'AK RFC Forecasts Time Series Output Test Failed'
            print *, 'Outflow should be 180.392075'
            print *, 'dynamic_reservoir_type needs to be 5 for AK RFC forecast output'
            print *, "========================================================================"
        end if
    end function test_ak_rfc_forecasts_time_series_with_lookback_and_offset

    ! This tests an RFC Forecast Reservoirs functionality to correct water elevation going
    ! over max water elevation
    function test_rfc_forecasts_over_max_water_elevation() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number, reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", "2019-08-18_09:00:00", cwd_full, 24)

        !water_elevation = 1331.18005

        water_elevation = 1.335180053710937500e+03

        do timestep_count = 1, 80

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 3.6 - epsilon(3.6) .and. outflow .le. 3.6 + epsilon(3.6) .and. &
        water_elevation .ge. 1335.18005 - epsilon(1335.18005) .and. water_elevation .le. 1335.18005 + epsilon(1335.18005) .and. &
        dynamic_reservoir_type == 4 .and. assimilated_value .ge. 3.6 - epsilon(3.6) &
        .and. assimilated_value .le. 3.6 + epsilon(3.6) .and. &
        assimilated_source_file == "2019-08-18_00.60min.CCHC1.RFCTimeSeries.ncdf") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Over Max Water Elevation Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Over Max Water Elevation Test Failed'
            print *, 'Outflow should be 3.6'
            print *, 'Water Elevation should be 1335.18005'
            print *, 'dynamic_reservoir_type needs to be 4 for RFC forecast output'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_over_max_water_elevation


    ! This tests an RFC Forecast Reservoirs functionality to offset 12 hours in the future
    ! and look back up to 24 hours to find a time series file and output the appropriate
    ! discharge in the array that matches up with model time as would be done in a long range
    ! analysis and assimilation run.
    function test_rfc_forecasts_with_offset_for_long_range_AnA() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number, reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full
        integer :: dynamic_reservoir_type
        real :: assimilated_value
        character(len=256) :: assimilated_source_file

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 17609317
        reservoir_type = 4

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, &
        weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, lake_number, reservoir_type, &
        "../../../../tests/local/reservoir_testing_files/reservoir_index_Long_Range_AnA.nc", &
        "2019-12-18_19:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 19

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0, dynamic_reservoir_type, &
            assimilated_value, assimilated_source_file)

            prev_time_inflow = inflow

            print *, outflow
        end do

        print *, 'dynamic_reservoir_type: ', dynamic_reservoir_type

        if (outflow .ge. 7.8 - epsilon(7.8) .and. outflow .le. 7.8 + epsilon(7.8) &
        .and. dynamic_reservoir_type == 4 .and. assimilated_value .ge. 7.8 - epsilon(7.8) &
        .and. assimilated_value .le. 7.8 + epsilon(7.8) .and. &
        assimilated_source_file == "2019-12-19_05.60min.CCHC1.RFCTimeSeries.ncdf") then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output For Long Range AnA Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output For Long Range AnA Test Failed'
            print *, 'Outflow should be 7.8'
            print *, 'dynamic_reservoir_type needs to be 4 for RFC forecast output'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_with_offset_for_long_range_AnA

end program
