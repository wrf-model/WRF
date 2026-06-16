subroutine da_qc_qscat (it, ob, grid, iv, num_qcstat_conv)

    !---------------------------------------------------------------------------
    ! Purpose: perform quality control for scatterometer data.
    !
    ! METHOD:  separated QC for each sensor
    !---------------------------------------------------------------------------

    implicit none

    integer      ,  intent(in)      :: it         ! outer loop count
    type (y_type),  intent(in)      :: ob         ! Observation structure.
    type(domain),     intent(in)    :: grid    ! first guess state.
    type (iv_type), intent(inout)   :: iv         ! O-B structure.
    integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

    integer :: n        ! Loop counter.
    real, allocatable :: model_rainc(:)           ! interpolated rainc
    real, allocatable :: model_rainnc(:)          ! interpolated rainnc
    real :: rr        ! rain rate
    real*8                       :: obs_time, analysis_time
    integer                      :: iyear, imonth, iday, ihour, imin
    real :: ub, vb, sb, db, uo, vo, so, do        !

    if (trace_use_dull) call da_trace_entry("da_qc_qscat")

    allocate (model_rainc(iv%info(qscat)%n1:iv%info(qscat)%n2))
    allocate (model_rainnc(iv%info(qscat)%n1:iv%info(qscat)%n2))

    call da_interp_lin_2d (grid%rainc, iv%info(qscat), 1, model_rainc)
    call da_interp_lin_2d (grid%rainnc, iv%info(qscat), 1, model_rainnc)

    ! read (analysis_date,'(i4,4(1x,i2))') iyear, imonth, iday, ihour, imin
    ! call da_get_julian_time (iyear,imonth,iday,ihour,imin,analysis_time)

    do n=iv%info(qscat)%n1, iv%info(qscat)%n2
        ! if ( TRIM(iv%info(qscat)%id(n)) .ne. "CFOSAT" ) cycle ! only for CFOSAT
        if ( iv%qscat(n)%u%qc < 0 ) cycle
        
        ! read (iv%info(qscat)%date_char(n),'(i4,4(1x,i2))') iyear, imonth, iday, ihour, imin
        ! call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time) ! get obs time 
        ! if ( obs_time > analysis_time) cycle ! if obs time greater then ana time, pass
        if (grid%xtime <= 0) then
            rr = 0
        else
            rr = (model_rainc(n) + model_rainnc(n)) / (grid%xtime / 60.0) ! rain rate (mm/h) = total/time
        end if
        ! print *, "rr=",rr
        if ( rr > qc_qscat_rr ) then
            iv%qscat(n)%u%qc = -16
            iv%qscat(n)%u%inv = 0.0
            iv%qscat(n)%v%qc = -16
            iv%qscat(n)%v%inv = 0.0
            num_qcstat_conv(2,qscat,1,1) = num_qcstat_conv(2,qscat,1,1) + 1
            num_qcstat_conv(2,qscat,2,1) = num_qcstat_conv(2,qscat,2,1) + 1

            if ( write_rej_obs_conv ) write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,f12.2)')&
                'qscat',ob_vars(1),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n), rr
            if ( write_rej_obs_conv ) write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,f12.2)')&
                'qscat',ob_vars(2),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n), rr
        endif
    end do

    deallocate (model_rainc)
    deallocate (model_rainnc)
    
    if (trace_use_dull) call da_trace_exit("da_qc_qscat")

end subroutine da_qc_qscat