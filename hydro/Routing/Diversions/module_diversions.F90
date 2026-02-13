module module_channel_diversions
    use netcdf
    use iso_fortran_env, only: int8, int16, int64
    use ieee_arithmetic, only: ieee_is_nan

    use module_diversions_timeslice, only: get_flow_for_gage, init_timeslices
    use module_hydro_stop, only: hydro_stop

    implicit none

    type diversion_t
        character(len=128) :: name

        character(len=16)  :: da_src, da_dest
        integer(kind=int8)  :: type_div, type_src, type_dest
        integer(kind=int64) :: id_src, id_dest, src_index, dest_index
        real :: capacity, fraction
        integer(kind=int16) :: lookback

        real :: persisted_flow_src, persisted_flow_dest
    end type

    logical :: diversions_active = .false.
    integer :: ndivs = 0

    type(diversion_t), allocatable :: diversions(:)

    character(*), parameter :: free = '(*(g0,1x))'

contains
    subroutine init_diversions(diversions_file, timeslice_path)
        character(*), intent(in) :: diversions_file
        character(*), intent(in) :: timeslice_path

        integer :: g, i, ierr = 0
        character(len=20) :: istr
        character(len=256) :: char_tmp

        integer :: ncid, dimid
        integer :: name_vid, type_div_vid, type_src_vid, type_dest_vid, da_src_vid, da_dest_vid
        integer :: id_src_vid, id_dest_vid, capacity_vid, fraction_vid, lookback_vid

        if (len_trim(diversions_file) > 0) then
            print *, "Loading diversions data from " // trim(diversions_file)
            ierr = nf90_open(trim(diversions_file), NF90_NOWRITE, ncid)
            if (ierr /= 0) call hydro_stop("Could not open diversions file: " // trim(diversions_file))
            ierr = nf90_inq_dimid(ncid, "diversion", dimid)
            if (ierr /= 0) call hydro_stop("Error reading diversions file: " // trim(diversions_file))
            ierr = nf90_inquire_dimension(ncid, dimid, len=ndivs)
            if (ierr /= 0) call hydro_stop("Error reading diversions file: " // trim(diversions_file))

            write (istr, *) ndivs
            print *, "Diversions file has " // trim(adjustl(istr)) // " diversions"

            ! get fields
            ierr = 0
            ierr = ierr + nf90_inq_varid(ncid, "Diversion_Name", name_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DivType", type_div_vid)
            ierr = ierr + nf90_inq_varid(ncid, "FromType", type_src_vid)
            ierr = ierr + nf90_inq_varid(ncid, "ToType", type_dest_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DA_Src", da_src_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DA_Dest", da_dest_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DivFrom", id_src_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DivTo", id_dest_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DivCap", capacity_vid)
            ierr = ierr + nf90_inq_varid(ncid, "DivFrac", fraction_vid)
            ierr = ierr + nf90_inq_varid(ncid, "Lookback", lookback_vid)

            if (ierr /= 0) then
                print free, "WARNING: error occurred accessing diversion file variables, will disable diversions"
                return
            end if

            if (ndivs > 0) then
                ! Read the timeslice data
                ierr = init_timeslices(timeslice_path)
                if (ierr /= 0) then
                    print free, "WARNING: No timeslice files available when initializing diversions, will disable diversions"
                    return
                end if

                diversions_active = .true.

                allocate(diversions(ndivs))
                do i = 1, ndivs
                    associate (div => diversions(i))
                        div = diversion_t('', '', '', -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)

                        ierr = 0
                        !ierr = ierr + nf_get_var1(ncid, name_vid, i, div%name)  !! can't read string with Fortran :-(

                        ierr = ierr + nf90_get_var(ncid, da_src_vid, div%da_src, start=(/i/), count=(/15/))
                        div%persisted_flow_src = get_flow_for_gage(div%da_src)
                        ierr = ierr + nf90_get_var(ncid, da_dest_vid, div%da_dest, start=(/i/), count=(/15/))
                        div%persisted_flow_dest = get_flow_for_gage(div%da_dest)

                        ierr = ierr + nf90_get_var(ncid, type_div_vid, div%type_div, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, type_src_vid, div%type_src, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, type_dest_vid, div%type_dest, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, id_src_vid, div%id_src, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, id_dest_vid, div%id_dest, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, capacity_vid, div%capacity, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, fraction_vid, div%fraction, start=(/i/))
                        ierr = ierr + nf90_get_var(ncid, lookback_vid, div%lookback, start=(/i/))

                        if (ierr /= 0) then
                            print free, "WARNING: error occurred reading diversion variables from diversion file, will disable diversions"
                            diversions_active = .false.
                            return
                        end if
                    end associate
                end do

            end if
        end if

    end subroutine

    subroutine calculate_diversion(src_link_in, qlink_src_in, diversion_quantity_out, diversion_quantity_in)
        integer(kind=int64), intent(in) :: src_link_in
        ! integer(kind=int64), intent(out) :: dst_out
        real, intent(in) :: qlink_src_in
        real, intent(out) :: diversion_quantity_out, diversion_quantity_in

        integer :: i

        diversion_quantity_out = 0
        diversion_quantity_in = 0

        ! bail if we're inactive
        if (.not. diversions_active) return

        ! link to gage
        ! look to see what type of diversion it is
        ! call into sub-procedure to handle type=1, type=2, type=3, etc

        do i = 1, ndivs
            if (src_link_in == diversions(i)%id_src) then
                if (diversions(i)%type_div /= 3) then
                    print free, "!!! UNSUPPORTED DIVERSION TYPE (", diversions(i)%type_div, "), skipping"
                else
                    call gage_assisted_diversion(src_link_in, diversions(i), qlink_src_in, diversion_quantity_out)
                    ! dst_out = diversions(i)%id_dest
                end if
            end if

            if (src_link_in == diversions(i)%id_dest) then
                if (diversions(i)%type_div /= 3) then
                    print free, "!!! UNSUPPORTED DIVERSION TYPE (", diversions(i)%type_div, "), skipping"
                else
                    if (.not. ieee_is_nan(diversions(i)%persisted_flow_dest)) &
                        diversion_quantity_in = diversions(i)%persisted_flow_dest
                end if
            end if
        end do

        ! subtract dst_out from source gage

    end subroutine

    subroutine gage_assisted_diversion(src_link, diversion, qlink_src, div_gage_flow)
        integer(kind=int64), intent(in) :: src_link
        type(diversion_t), intent(in) :: diversion
        real, intent(in) :: qlink_src
        real, intent(out) :: div_gage_flow

        real :: fraction

        ! This is the so-called "Type 3" diversion. We take the observed flow from div_gage,
        ! and subtract it from the upstream qlink_src, if it's a valid flow (not-NaN).
        !
        ! If it's not a valid flow, we try to use the Fraction property of the diversion,
        ! and if -that's- not available, we just leave the flow untouched.

        div_gage_flow = diversion%persisted_flow_dest
        if (ieee_is_nan(div_gage_flow)) then
            fraction = diversion%fraction
            if (fraction == -1) then
                print free, "WARNING: No fractional diversion value specified for diversion at gage '" // trim(adjustl(diversion%da_dest)) // "', skipping"
                fraction = 0
            else
                print free, "INFO: No gage discharge available for diversion '" // trim(adjustl(diversion%da_dest)) // "', using fixed fractional diversion of", fraction
            end if
            div_gage_flow = qlink_src * fraction
        end if
    end subroutine

end module
