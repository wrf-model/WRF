subroutine da_qc_gpsref(it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: quality control for gpsro refractivity
   !          this subroutine is only called when anal_type_verify=false
   ! History:
   !   2020-06: QC code previously in da_get_innov_vector_gpsref
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter
   integer :: i, j, k  ! Index dimension
   real, allocatable :: model_ref(:,:) ! model gpsref at ob loc
   real, allocatable :: zdiff(:)       ! dz for dndz qc
   real, allocatable :: model_t(:,:)   ! model value t at ob location
   real, allocatable :: int_t(:,:,:)   ! xb%t for interpolating t for gsi qc

   real :: max_iv_thresh

   integer :: top_level
   real, allocatable :: dndz_obs(:),dndz_mod(:)
   real, allocatable :: dndz2_obs(:),dndz2_mod(:)

   real :: h_1, h_2
   real :: pcnt1, pcnt2, pcnt3
   integer :: nn, na, nb
   real    :: percnt
   real    :: height_below(5000)
   character(len=40) :: name_qc(5000)

   ! for QC cutoff GSI
   real :: g_height, g_lat, cutoff, stddev

   ! for QC statistics
   integer :: nrej_maxiv, nrej_dndz, nrej_dndz2, nrej_cutoff
   integer :: nrej_pcnt_below, nrej_pcnt_above, nrej_pcnt_middle
   integer, parameter   :: nhh = 13
   real, dimension(nhh) :: hh = (/  500.0, 1000.0, 1500.0, 2000.0, 3000.0,  &
                                   5000.0, 6000.0, 7000.0, 8000.0, 10000.0, &
                                  12000.0, 15000.0, 18000.0 /)
   integer, dimension(nhh+1) :: nrej_allqc, nrej_height, ngood, ntotal
   integer :: ihh, khh
   integer :: iunit, ios
   character(len=30) :: filename

   if (trace_use_dull) call da_trace_entry("da_qc_gpsref")

!   if (iv%info(gpsref)%nlocal < 1) then
!      if (trace_use_dull) call da_trace_exit("da_qc_gpsref")
!      return
!   end if

   write(unit=message(1),fmt='(A)') '   calling da_qc_gpsref'
   call da_message(message(1:1))

   nrej_maxiv       = 0
   nrej_dndz        = 0
   nrej_dndz2       = 0
   nrej_pcnt_below  = 0
   nrej_pcnt_above  = 0
   nrej_pcnt_middle = 0
   nrej_cutoff      = 0
   nrej_allqc(:)    = 0
   nrej_height(:)   = 0
   ngood(:)         = 0
   ntotal(:)        = 0

if ( iv%info(gpsref)%n2 >= iv%info(gpsref)%n1 ) then
   allocate (model_ref(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))
   model_ref(:,:) = 0.0

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

      do k = 1, iv%info(gpsref)%levels(n)
         if ( iv%gpsref(n)%ref(k)%qc /= missing_data ) then
            model_ref(k,n) = ob%gpsref(n)%ref(k) - iv%gpsref(n)%ref(k)%inv
         end if
      end do

   end do

!==========================================
! Quality check 1: Gross error check
!==========================================
   ! Quality check : Gross error(departure from the background) check
   if ( check_max_iv ) then
      do n = iv%info(gpsref)%n1, iv%info(gpsref)%n2
         do k = 1, iv%info(gpsref)%levels(n)
            max_iv_thresh = iv%gpsref(n)%ref(k)%error * max_error_ref
            if ( iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer ) then
               if ( abs(iv%gpsref(n)%ref(k)%inv) > max_iv_thresh ) then
                  iv%gpsref(n)%ref(k)%qc = fails_error_max
                  if ( iv%info(gpsref)%proc_domain(1,n) ) nrej_maxiv = nrej_maxiv + 1
               end if
            end if
         end do
      end do
   end if

!==========================================
!  Quality check 2.1: dNdz
!  Quality check 2.2: d2N_dz2
!==========================================
   if ( gpsref_qc_dndz_opt==1 .or. gpsref_qc_dndz2_opt==1 ) then

! refer to Poli et al. (2009) ------------------------------------------------
! flag if fit in each of these two qc steps for both of obs. and model
! qc_dndz: dN/dz < -50 km^-1
! qc_dndz2: abs(d^2N/dz^2) > 100 km^-2
!                                  Shu-Ya Chen (2009-07-29)
   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      if (iv%info(gpsref)%levels(n) <= 2) cycle
      top_level = 0 ! initialize
      do k=1,iv%info(gpsref)%levels(n)
         if (model_ref(k,n) > 0.0) top_level=k
      end do
      if ( top_level <= 2 ) cycle

      ! zdiff values are used for both gpsref_qc_dndz_opt and gpsref_qc_dndz2_opt
      allocate (zdiff(1:top_level))
      zdiff(1)         = iv%gpsref(n)%h(2) - iv%gpsref(n)%h(1)
      zdiff(top_level) = iv%gpsref(n)%h(top_level) - iv%gpsref(n)%h(top_level-1)
      do k = 2, top_level-1
         zdiff(k) = iv%gpsref(n)%h(k+1) - iv%gpsref(n)%h(k-1)
      end do
      zdiff(:) = zdiff(:) * 0.001  ! meter to km

      ! dndz values are used for both gpsref_qc_dndz_opt and gpsref_qc_dndz2_opt calculation
      allocate(dndz_obs(top_level))
      allocate(dndz_mod(top_level))

      ! check for bottom boundary (Forward Difference)
      dndz_obs(1) = (ob%gpsref(n)%ref(2)-ob%gpsref(n)%ref(1)) / zdiff(1)
      dndz_mod(1) = (model_ref(2,n)-model_ref(1,n)) / zdiff(1)
      ! check for upper boundary (Backward Difference)
      dndz_obs(top_level) = &
         (ob%gpsref(n)%ref(top_level)-ob%gpsref(n)%ref(top_level-1)) / zdiff(top_level)
      dndz_mod(top_level) = &
         (model_ref(top_level,n)-model_ref(top_level-1,n))/ zdiff(top_level)
      ! check for middle levels (Central Difference)
      do k = 2, top_level-1
         dndz_obs(k) = (ob%gpsref(n)%ref(k+1)-ob%gpsref(n)%ref(k-1))/zdiff(k)
         dndz_mod(k) = (model_ref(k+1,n)-model_ref(k-1,n))/zdiff(k)
      end do

      if ( gpsref_qc_dndz_opt==1 ) then
         ! setting qc flags according to dndz values
         do k = 1, top_level
            if (iv%gpsref(n)%ref(k)%qc /= missing_data) then
               ! default gpsref_qc_dndz_thresh = -50.0
               if ( (dndz_obs(k) < gpsref_qc_dndz_thresh) .or. &
                    (dndz_mod(k) < gpsref_qc_dndz_thresh) ) then
                  iv%gpsref(n)%ref(k)%qc = qcflag_dndz
                  if ( iv%info(gpsref)%proc_domain(1,n) ) nrej_dndz = nrej_dndz + 1
               end if
            end if
         end do
      end if ! gpsref_qc_dndz_opt

      if ( gpsref_qc_dndz2_opt==1 ) then

         allocate(dndz2_obs(top_level))
         allocate(dndz2_mod(top_level))

         ! check for bottom boundary
         dndz2_obs(1) = (dndz_obs(2)-dndz_obs(1)) / zdiff(1)
         dndz2_mod(1) = (dndz_mod(2)-dndz_mod(1)) / zdiff(1)
         ! check for upper boundary
         dndz2_obs(top_level) = (dndz_obs(top_level)-dndz_obs(top_level-1)) / zdiff(top_level)
         dndz2_mod(top_level) = (dndz_mod(top_level)-dndz_mod(top_level-1)) / zdiff(top_level)
         ! check for middle levels
         do k = 2, top_level-1
            dndz2_obs(k) = (dndz_obs(k+1)-dndz_obs(k-1)) / zdiff(k)
            dndz2_mod(k) = (dndz_mod(k+1)-dndz_mod(k-1)) / zdiff(k)
         end do

         ! setting qc flags according to dndz2 values
         do k=1, top_level
            if (iv%gpsref(n)%ref(k)%qc /= missing_data) then
               ! default gpsref_qc_dndz_thresh = -100.0
               if ( (abs(dndz2_obs(k)) > gpsref_qc_dndz2_thresh) .or. &
                    (abs(dndz2_mod(k)) > gpsref_qc_dndz2_thresh) ) then
                  iv%gpsref(n)%ref(k)%qc = qcflag_dndz2
                  if ( iv%info(gpsref)%proc_domain(1,n) ) nrej_dndz2 = nrej_dndz2 + 1
               end if
            end if
         end do
         deallocate(dndz2_obs,dndz2_mod)
      end if ! gpsref_qc_dndz2_opt

      deallocate(dndz_obs,dndz_mod)
      deallocate (zdiff)
   end do  ! end of do iv%info(gpsref)%n1~n2
   end if ! gpsref_qc_dndz_opt or gpsref_qc_dndz2_opt
!
! End of Poli's check. (2009) -------------------------------------------------
!

!==========================================
! Quality check 3: GSI's regional QC
!==========================================
   if ( gpsref_qc_gsi_opt == 1 ) then

! t_iwabuchi 20111216 GSI's regional QC
! START GSI-QC REGIONAL TI-GSI -------------------------------------------------
! ------------------------------------------------------------------------------
! GSI-QC Implementation, Ted Iwabuchi
!   First release: 2011-03-04
!   Review 3.3.1 : 2011-12-16
!
! HISTORY:
!   2011-05-04 : bug in formula fixed
!   2011-05-11 : add abs, and validated
!   2011-06-06 : reviewd, added comments
!   2011-12-16 : implemented for 3.3.1
!
! SUMMARY:
! Modified version of Cucurull, 2010 for NCEP GSI-Global DA
! NCEP-GSI regional DA  setupref.f90
!
!   O-B cutoff depending on assigned std dev for each height and latitude
!
!  > 30                                           all observation is rejected
!  11 - 30 km    0.25 + 0.5 cos (lambda)   [c1]   3
!   9 - 11 km    (11-h)/2 x c2 + (h-9)/2 x c1     3
!   6 -  9 km    0.5 (if T<= 240)          [c2]   3
!                0.01 x T^2 - 0.455 T + 52.075 (if T>240)
!   4 -  6 km    (6-h)/2 x c3 + (h-4)/2 x c2    3
!   0 -  4 km    1+2.5 cos (lambda)        [c3]   1
!
!   h, lambda, T
! ------------------------------------------------------------------------------
!
! REFERENCES:
!   GSI code: comGSI_v3Beta/src/main/setupref.f90
!
!
! NOTES:
!  See TI-GSI blocks, and don't forget to declare type of variables
!  used in this block:
!
! 1  real :: g_height, g_lat, cutoff, stddev     ! QC cutoff GSI
! 2  real, allocatable :: model_t(:,:)  ! Model value t at ob location
! 3  integer, parameter :: qc_cutoff = -36
!

! t_iwabuchi 20111216 (hmn) allocate model_t for GSI regional QC
   allocate (model_t(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))
   model_t(:,:) = 0.0

   allocate (int_t(ims:ime, jms:jme, kms:kme))
   int_t = grid%xb%t
   call da_interp_lin_3d (int_t, iv%info(gpsref), model_t)
   deallocate(int_t)

!  process refracticity data n from n1 to n2
!
   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
       do k=1,iv%info(gpsref)%levels(n)

         g_height = iv%gpsref(n)% h(k)
         g_lat    = cos ( radian * iv%info(gpsref)%lat(k,n) )

         if (     g_height >= 0.0     .and. g_height < 4000.0) then
           cutoff = 1.0
           stddev = 1.0 + 2.5 * g_lat

         else if (g_height >= 4000.0  .and. g_height < 6000.0) then
           cutoff = 3.0
           if (model_t(k,n) <= 240.0) then
             stddev = 0.5
           else
             stddev = 0.001 * model_t(k,n) * model_t(k,n) - 0.455 * model_t(k,n) + 52.075
           endif
           stddev = (6000.0 - g_height)/2000.0 * (1.0 + 2.5 * g_lat)  &
                    + (g_height - 4000.0)/2000.0 * stddev

         else if (g_height >= 6000.0  .and. g_height < 9000.0) then
           cutoff = 3.0
           if (model_t(k,n) <= 240.0) then
             stddev = 0.5
           else
!            0.01 x T^2 - 0.455 T + 52.075 (if T>240)
             stddev = 0.001 * model_t(k,n) * model_t(k,n) - 0.455 * model_t(k,n) + 52.075
           end if

         else if (g_height >= 9000.0  .and. g_height < 11000.0) then
           cutoff = 3.0
           if (model_t(k,n) <= 240.0) then
             stddev = 0.5
           else
             stddev = 0.001 * model_t(k,n) * model_t(k,n) - 0.455 * model_t(k,n) + 52.075
           endif
           stddev = (11000.0 - g_height)/2000.0 * stddev  &
                   + (g_height - 9000)/2000.0 * (0.25 + 0.5 * g_lat)


         else if (g_height >= 11000.0 .and. g_height < 30000.0) then
           cutoff = 3.0
           stddev = 0.25 + 0.5 * g_lat

         else if (g_height >= 30000.0 ) then
           cutoff = 0.0

         endif

         if (iv%gpsref(n)%ref(k)%qc /= missing_data) then
            ! Check innovation, stddev, and cutoff
            if (abs(iv%gpsref(n)%ref(k)%inv) >= stddev * cutoff) then
              iv%gpsref(n)%ref(k)%qc = qcflag_cutoff
              if ( iv%info(gpsref)%proc_domain(1,n) ) nrej_cutoff = nrej_cutoff + 1
            endif
         end if

      end do ! level
   end do ! n

   deallocate (model_t)

   end if ! gpsref_qc_gsi_opt
!
! End   GSI-QC REGIONAL TI-GSI ------------------------------------------------
!
! t_iwabuchi END

!==========================================
! Quality check 4: Error percentage check and Low levels quality control
!==========================================
   if ( gpsref_qc_pcnt_opt == 1 ) then

      ! Quality check : Error percentage check.

      ! assign namelist settings to local variables
      h_1 = gpsref_qc_pcnt_h1        ! default 7000.0
      h_2 = gpsref_qc_pcnt_h2        ! default 25000.0
      pcnt1 = gpsref_qc_pcnt_below   ! default 0.05  ! test 0.02
      pcnt2 = gpsref_qc_pcnt_above   ! default 0.04  ! test 0.01
      pcnt3 = gpsref_qc_pcnt_middle  ! default 0.10  ! test 0.03

      do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

         do k=1, iv%info(gpsref)%levels(n)

            ! incremetal refractivity or the relative error:
            !   abs[(O-B)/{(O+B)/2}]              (Lidia Cucurull 2005)

            percnt = 2.0 * abs(iv%gpsref(n)%ref(k)%inv / &
                     (ob%gpsref(n)%ref(k) + model_ref(k,n)))

            if (iv%gpsref(n)%ref(k)%qc /= missing_data) then

               if (iv%gpsref(n)%h(k) < h_1) then
                  if (percnt > pcnt1) iv%gpsref(n)%ref(k)%qc = qcflag_pcnt_below
               else if (iv%gpsref(n)%h(k) > h_2) then
                  if (percnt > pcnt3) iv%gpsref(n)%ref(k)%qc = qcflag_pcnt_above
               else
                  if (percnt > pcnt2) iv%gpsref(n)%ref(k)%qc = qcflag_pcnt_middle
               end if
            end if
         end do
      end do

      ! Quality check : Low levels quality control

      ! Search for the GPS RO's name with the 'qcflag_pcnt_below':

      if ( maxval(iv%info(gpsref)%levels(:)) > 1 ) then ! gpsref in profiles
         nn = 0
         height_below = 0.0
         name_qc      = '                                       '

         do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
             nn = nn + 1
             iv%info(gpsref)%levels(n) = iv%info(gpsref)%levels(n)
             do k=1, iv%info(gpsref)%levels(n)
                if (iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_below) then
                   name_qc(nn) = iv%info(gpsref)%name(n)
                   height_below(nn) = max(iv%gpsref(n)%h(k),height_below(nn))
                end if
             end do
             if (height_below(nn) == 0.0) nn = nn - 1
         end do

         ! Set the flag qcflag_pcnt_below to the levels below percnt < pcnt1::

         do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
            do na = 1,nn
               if (iv%info(gpsref)%name(n) == name_qc(na)) then
                  do k=1, iv%info(gpsref)%levels(n)
                     if (iv%gpsref(n)%h(k) < height_below(na) .and. &
                         iv%gpsref(n)%ref(k)%qc /= missing_data) then
                       iv%gpsref(n)%ref(k)%qc = qcflag_pcnt_below
                     end if
                  end do
                  exit
               end if
            end do

         end do
      else    ! gpsref not in profiles
         do na = iv%info(gpsref)%n1, iv%info(gpsref)%n2
            if ( iv%gpsref(na)%ref(1)%qc == qcflag_pcnt_below) then
               do nb = iv%info(gpsref)%n1, iv%info(gpsref)%n2
                  if ( iv%info(gpsref)%id(nb) == iv%info(gpsref)%id(na)     .and. &
                       iv%info(gpsref)%name(nb) == iv%info(gpsref)%name(na) .and. &
                       iv%gpsref(nb)%ref(1)%qc /= missing_data              .and. &
                       iv%gpsref(nb)%h(1) <= iv%gpsref(na)%h(1)   ) then
                     iv%gpsref(nb)%ref(1)%qc = qcflag_pcnt_below
                  end if
               end do
            end if
         end do
      end if  ! end of if gpsref profiles

      do n = iv%info(gpsref)%n1, iv%info(gpsref)%n2
         if ( .not. iv%info(gpsref)%proc_domain(1,n) ) cycle
         do k = 1, iv%info(gpsref)%levels(n)
            if (iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_middle) &
               nrej_pcnt_middle = nrej_pcnt_middle + 1
            if (iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_below)  &
               nrej_pcnt_below = nrej_pcnt_below + 1
            if (iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_above)  &
               nrej_pcnt_above = nrej_pcnt_above + 1
         end do
      end do

   end if ! gpsref_qc_pcnt_opt
! ------------------------------------------------------------------------------

   deallocate (model_ref)


   ! hcl-todo: add other qc counts and prints

   nlocal_loop: do n = iv%info(gpsref)%n1, iv%info(gpsref)%n2
      if ( .not. iv%info(gpsref)%proc_domain(1,n) ) cycle nlocal_loop
      do k = 1, iv%info(gpsref)%levels(n)
         ! find the index of height bins for grouping statistics
         ihh = 0 ! initialize
         if ( iv%gpsref(n)%h(k) <= hh(1) ) then
            ihh = 1
         else if ( iv%gpsref(n)%h(k) > hh(nhh) ) then
            ihh = nhh+1
         else
            nhh_loop: do khh = 2, nhh
               if ( iv%gpsref(n)%h(k) >  hh(khh-1) .and. &
                    iv%gpsref(n)%h(k) <= hh(khh) ) then
                  ihh = khh
                  exit nhh_loop
               end if
            end do nhh_loop
         end if
         if ( ihh < 1 ) cycle nlocal_loop ! no valid h index found
         if ( iv%gpsref(n)%ref(k)%qc /= missing_data ) then
            ntotal(ihh) = ntotal(ihh) + 1
            if ( iv%gpsref(n)%ref(k)%qc < obs_qc_pointer .and. &
                 iv%gpsref(n)%ref(k)%qc /= qcflag_height ) then
               nrej_allqc(ihh) = nrej_allqc(ihh) + 1
            end if
            if ( iv%gpsref(n)%ref(k)%qc == qcflag_height ) then
               nrej_height(ihh) = nrej_height(ihh) + 1
            end if
            if ( iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer ) then
               ngood(ihh) = ngood(ihh) + 1
            end if
         end if
      end do
   end do nlocal_loop

end if ! n2 >= n1

   call da_proc_sum_int (nrej_maxiv)
   call da_proc_sum_int (nrej_dndz)
   call da_proc_sum_int (nrej_dndz2)
   call da_proc_sum_int (nrej_pcnt_below)
   call da_proc_sum_int (nrej_pcnt_above)
   call da_proc_sum_int (nrej_pcnt_middle)
   call da_proc_sum_int (nrej_cutoff)
   call da_proc_sum_ints (nrej_allqc(:))
   call da_proc_sum_ints (nrej_height(:))
   call da_proc_sum_ints (ntotal(:))
   call da_proc_sum_ints (ngood(:))

   if ( rootproc ) then
      if (num_fgat_time > 1) then
         write(filename,'(a,i2.2,a,i2.2)') 'qcstat_gpsref_', it, '_t', iv%time
      else
         write(filename,'(a,i2.2)') 'qcstat_gpsref_', it
      end if
      call da_get_unit(iunit)
      open(iunit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         write(unit=message(1),fmt='(a,a)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(iunit, fmt='(/a/)') &
         'Quality Control Statistics for GPSRO Refractivity'   ! exclude missing_data
      write(iunit,'(20x,a,13i7,a,i5,a)') '  TOTAL', nint(hh(:)), ' >', nint(hh(nhh)), ' meter'
      write(iunit,'(a20,20i7)') ' ntotal           = ', sum(ntotal), ntotal(:)
      write(iunit,'(a20,20i7)') ' ngood            = ', sum(ngood), ngood(:)
      write(iunit,'(a20,20i7)') ' nrej_height      = ', sum(nrej_height), nrej_height(:)
      write(iunit,'(a20,20i7)') ' nrej_allqc       = ', sum(nrej_allqc), nrej_allqc(:)
      write(iunit,'(a)')
      write(iunit,'(a20,i7)') ' nrej_maxiv       = ', nrej_maxiv
      write(iunit,'(a20,i7)') ' nrej_dndz        = ', nrej_dndz
      write(iunit,'(a20,i7)') ' nrej_dndz2       = ', nrej_dndz2
      write(iunit,'(a20,i7)') ' nrej_pcnt_below  = ', nrej_pcnt_below
      write(iunit,'(a20,i7)') ' nrej_pcnt_middle = ', nrej_pcnt_middle
      write(iunit,'(a20,i7)') ' nrej_pcnt_above  = ', nrej_pcnt_above
      write(iunit,'(a20,i7)') ' nrej_cutoff      = ', nrej_cutoff

      close(iunit)
      call da_free_unit(iunit)
   end if

   if (trace_use_dull) call da_trace_exit("da_qc_gpsref")

end subroutine da_qc_gpsref
