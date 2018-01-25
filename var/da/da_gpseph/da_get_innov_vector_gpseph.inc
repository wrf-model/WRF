subroutine da_get_innov_vector_gpseph(it, ob, iv)
!==========================================================================
!  Purpose: Calculate invovation (O-B) for gpseph
!  Author:  Shu-Ya Chen/Shu-Hua Chen        Date: 01/27/2011
!  History: December 2015: re-implementation (Jamie Bresch)
!=========================================================================
   implicit none

   integer,          intent(in)    :: it       ! External iteration
   type(y_type),     intent(inout) :: ob       ! Observation structure
   type(iv_type),    intent(inout) :: iv       ! O-B structure

   integer :: n        ! Loop counter
   integer :: i, j, k  ! Index dimension
   real, dimension(:,:), allocatable :: model_eph !Model gpseph at ob loc
   real, dimension(:,:), allocatable :: model_ref !Model gpsref at ob loc
   real, dimension(kds:kde)          :: obs_ref   !obs gpsref at ob loc
   real, dimension(kds:kde)          :: obs_eph   !obs gpseph at ob loc
   real, dimension(kds:kde)          :: mean_h
   real, dimension(:), allocatable   :: err_height,err_percent,intp_err_percent
   real, dimension(:,:), allocatable :: cc
   real    :: tmp_height, tmp_percent
   integer :: kk, count_lev, err_lev
   integer :: nlevels
   integer :: n1, n2
   integer :: iunit, junit, iost
   character(len=256) :: fname
   logical :: write_iv_gpseph = .true. !.false.

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpseph")
!=============================================================================

   mean_h(:) = global_h_mean(:) !km

   if (iv%info(gpseph)%nlocal < 1) return

   nlevels = kde-kds+1
   allocate (model_eph(nlevels,iv%info(gpseph)%n1:iv%info(gpseph)%n2))
   model_eph(:,:) = 0.0

   do n=iv%info(gpseph)%n1,iv%info(gpseph)%n2
      do k=1, iv%info(gpseph)%levels(n)
         if( iv%gpseph(n)%eph(k)%qc == fails_error_max .and. it > 1 ) &
             iv%gpseph(n)%eph(k)%qc = 0
      end do
   end do

! Before gross error check, fill in eph data into iv and ob

   if ( it > 1 ) then
      if ( allocated (gps_rays ) ) then
         n1 = Lbound ( gps_rays, 1 )
         n2 = ubound ( gps_rays, 1 )
         do n = n1, n2
            if ( allocated (gps_rays(n)%je2) ) deallocate (gps_rays(n)%je2)
            if ( gps_rays(n)%nbot == 0 .and. gps_rays(n)%ntop == 0 ) cycle
            do k = gps_rays(n)%nbot, gps_rays(n)%ntop
               deallocate ( gps_rays(n)%ip123(k)%i1 )
               deallocate ( gps_rays(n)%ip123(k)%i2 )
               deallocate ( gps_rays(n)%ip123(k)%i3 )
               deallocate ( gps_rays(n)%ip123(k)%w1 )
               deallocate ( gps_rays(n)%ip123(k)%w2 )
            enddo
            deallocate (gps_rays(n)%ip123)
         enddo
         deallocate (gps_rays)
      endif
   endif

   if ( .not. allocated ( gps_rays ) ) then
      allocate ( gps_rays(iv%info(gpseph)%n1:iv%info(gpseph)%n2) )
      gps_rays%nbot = 0 !initialize
      gps_rays%ntop = 0 !initialize
   end if

   obs_loop: do n=iv%info(gpseph)%n1,iv%info(gpseph)%n2

      if ( iv%info(gpseph)%levels(n) < 1 ) cycle

      call da_gpseph_rays(gps_rays(n), iv%gpseph(n))

      obs_ref(:) = iv%gpseph(n)%ref(:)%inv
      call da_obs_ref_to_eph(gps_rays(n), obs_ref, obs_eph)

      call da_mdl_ref_to_eph(gps_rays(n), global_ref_mean_h, model_eph(:,n))

      do k=kds,kde
         ob%gpseph(n)%eph(k)=obs_eph(k)
      enddo

      if (iv%gpseph(n)%level1 .gt. iv%gpseph(n)%level2) cycle obs_loop

      ! set quality control (QC) ---------------------------------------------------
      do k = kds, kde
         if ((k >= iv%gpseph(n)%level1) .and. (k <= iv%gpseph(n)%level2)) then
            iv%gpseph(n)%eph(k)%qc = 0
         else
            iv%gpseph(n)%eph(k)%qc = missing_data
         endif
      enddo

      do k = kds, kde
         iv%gpseph(n)%eph(k)%inv = 0.0
         if (ob%gpseph(n)%eph(k) > missing_r .AND. &
             iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer) then
            iv%gpseph(n)%eph(k)%inv = ob%gpseph(n)%eph(k) - model_eph(k,n)
         end if
      end do

   end do obs_loop

   !----------------------------------------------------------------
   ! Reading observation errors from an external table.
   ! The percentage values in the table were generated using
   ! Hollingsworth-Lonngberg method from one-month
   ! (2003/08/15~2003/09/15) statistics
   !----------------------------------------------------------------
   ! OBSERR = percentage(%) * Excess Phase
   call da_get_unit(iunit)
   open (unit=iunit, file='OBSERROR_GPSEPH.TBL', form='formatted', &
      status='old', iostat=iost)
   if ( iost /= 0 ) then
      call da_free_unit(iunit)
      call da_error(__FILE__,__LINE__, (/"Error opening OBSERROR_GPSEPH.TBL"/))
   else
      allocate(err_height(nlevels))
      allocate(err_percent(nlevels))
      err_percent(:) = missing_r
      read(iunit,*)  !the header line
      count_lev = 0
      read_loop: do
         read(iunit,*,iostat=iost) err_lev, tmp_height, tmp_percent
         if ( iost /=0 ) exit read_loop
         count_lev = count_lev + 1
         err_height(count_lev)  = tmp_height
         err_percent(count_lev) = tmp_percent*0.01
      end do read_loop
      call da_free_unit(iunit)
      close(iunit)
      if ( count_lev < 1 ) then
         call da_error(__FILE__,__LINE__, (/"Error reading OBSERROR_GPSEPH.TBL"/))
      end if
   end if

   allocate(intp_err_percent(kde-kds+1))
   allocate(cc(3,count_lev))
   call da_splinx(count_lev,err_height(1:count_lev),err_percent(1:count_lev),kde-kds+1,mean_h(kds:kde),cc,intp_err_percent)

   do n=iv%info(gpseph)%n1,iv%info(gpseph)%n2
      if ( iv%info(gpseph)%levels(n) < 2 ) cycle
      iv%gpseph(n)%eph(:)%error = missing_r
      do kk = iv%gpseph(n)%level1, iv%gpseph(n)%level2
         iv%gpseph(n)%eph(kk)%error = intp_err_percent(kk)*ob%gpseph(n)%eph(kk)
      enddo
   enddo
   deallocate(intp_err_percent,err_height,err_percent,cc)

   ! Quality check: Gross error(departure from the background) check

   if ( check_max_iv ) then
      call da_check_max_iv_gpseph(iv, it)
   end if

   if ( write_iv_gpseph ) then
      if (.not. anal_type_verify) then
         ! Write out GPS EPH data:
         call da_get_unit(junit)
         do n=iv%info(gpseph)%n1,iv%info(gpseph)%n2
            if ( iv%info(gpseph)%levels(n) < 2 ) cycle
            write(fname,'(a,i2.2)') 'RO_Innov_'//iv%info(gpseph)%date_char(n)//'_', it
            open (unit=junit, file=trim(fname), form='formatted', status='unknown')
            write(unit=junit, fmt='(/i5,2x,a,2x,a,2x,4f10.3,i5)') n, &
               iv%info(gpseph)%date_char(n), iv%info(gpseph)%id(n),  &
               iv%info(gpseph)%lat(1,n)  , iv%info(gpseph)%lon(1,n), &
               iv%info(gpseph)%x(1,n)  , iv%info(gpseph)%y(1,n), &
               iv%info(gpseph)%levels(n)
            write(unit=junit, fmt='(a5,3x,6a14)') 'level','     height   ', &
               '    Obs_eph   ','  model_eph   ','  Innov_eph   ',          &
               '  error_eph   ',' qc_eph       '
            do k = kds,kde
               write(unit=junit, fmt='(i5,1x,5f14.3,i10)')  k, &
                  mean_h(k)*1000., ob%gpseph(n)%eph(k),        &
                  model_eph(k,n), iv%gpseph(n)%eph(k)%inv,     &
                  iv%gpseph(n)%eph(k)%error, iv%gpseph(n)%eph(k)%qc
            end do
         end do
         call da_free_unit(junit)
         close(junit)
      end if  ! end of verify check
   end if ! write_iv_gpseph

   deallocate (model_eph)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpseph")

end subroutine da_get_innov_vector_gpseph
