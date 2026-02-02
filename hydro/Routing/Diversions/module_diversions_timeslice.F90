module module_diversions_timeslice
    use fortglob, only: glob_t, globfiles
    use module_hydro_stop, only: hydro_stop

    use netcdf
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
    implicit none

    integer, parameter :: PATH_MAX = 4096
    type(glob_t) :: timeslice_files

    character(*), parameter :: free = '(*(g0,1x))'

    contains

    integer function init_timeslices(timeslice_path) result(ierr)
        character(*), intent(in) :: timeslice_path
        character(len=PATH_MAX) :: tslice_glob

        ierr = 0
        tslice_glob = trim(adjustl(timeslice_path)) // '/' // "*.15min.usgsTimeSlice.ncdf"
        timeslice_files = globfiles(tslice_glob)

        if (timeslice_files%nfiles == 0) ierr = 1
    end function

    real function get_flow_for_gage(gage) result(flow)
        character(len=15), intent(in) :: gage

        integer :: i, ierr=0, ncid, dimid, varid, num_stns, found(1)
        real    :: discharge(1)
        character(len=15), allocatable :: gage_ids(:)

        flow = ieee_value(flow, ieee_quiet_nan)

        if (gage(1:4) == 'None') then
            return
        end if

        ! start looking at files, going backward from most recent
        do i = timeslice_files%nfiles, 1, -1
            ierr = nf90_open(trim(timeslice_files%filenames(i)), NF90_NOWRITE, ncid)

            ! look for gage
            ierr = ierr + nf90_inq_dimid(ncid, 'stationIdInd', dimid)
            ierr = ierr + nf90_inquire_dimension(ncid, dimid, len=num_stns)

            allocate(gage_ids(num_stns))
            ierr = ierr + nf90_inq_varid(ncid, 'stationId', varid)
            ierr = ierr + nf90_get_var(ncid, varid, gage_ids)

            if (ierr /= 0) call hydro_stop("Error occurred reading gage data from " // trim(timeslice_files%filenames(i)))

            found = findloc(gage_ids, gage)
            if (found(1) /= 0) then
#ifdef HYDRO_D
                print free, "DEBUG: Reading diversion discharge for gage " // trim(adjustl(gage)) // " from " // trim(timeslice_files%filenames(i))
#endif
                ierr = ierr + nf90_inq_varid(ncid, 'discharge', varid)
                ierr = ierr + nf90_get_var(ncid, varid, discharge, start=found, count=(/1/))
                if (ierr /= 0) call hydro_stop("Error occurred reading gage data from " // trim(timeslice_files%filenames(i)))

                if (discharge(1) >= 0) then
                    flow = discharge(1)
                    deallocate(gage_ids)
                    return
#ifdef HYDRO_D
                else
                    print free, "DEBUG: Diversion discharge value invalid, continuing search if able"
#endif
                end if
            end if

            deallocate(gage_ids)
            ierr = nf90_close(ncid)
        end do

        print free, "WARNING: Valid gage discharge not found in any timeslice file, falling back to fractional diversion"
    end function

end module