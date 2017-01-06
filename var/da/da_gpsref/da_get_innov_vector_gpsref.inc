subroutine da_get_innov_vector_gpsref(it, num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)       

   integer :: n        ! Loop counter.
   integer :: i, j, k, kk  ! Index dimension.
   real    :: dx, dxm, dz, dzm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real,allocatable :: model_ref(:,:) !Model gpsref at ob loc
   real    :: v_h(kms:kme), v_p(kms:kme)     ! Model value h at ob

   integer :: Iu_ref, l
   integer,allocatable :: used_lev(:,:) ! for obs. data thinning
                                        ! record the closest level with model
   integer,allocatable :: qc(:)         ! record iv%gpsref(n)%ref(k)%qc
                               ! hor. location.
   real    :: distance_h       ! cal. h-difference between obs and model
   real,allocatable :: min_dis(:)   ! minimum difference
                                               ! hor. location.
! t_iwabuchi 20111216 T for QC 
   real, allocatable :: int_t(:,:,:)     ! for T

   ! For quality control

   real   , parameter :: h_1 = 7000.0, h_2 = 25000.0
   ! Lidia Cucurull values:
   real   , parameter :: pcnt1 = 0.05, pcnt2 = 0.04, pcnt3 = 0.10
   ! testing values:
   ! real   , parameter :: pcnt1 = 0.02, pcnt2 = 0.01, pcnt3 = 0.03
   integer, parameter :: qc_below = -31, qc_middle = -32, qc_above = -33

   integer, parameter :: qc_step1 = -34, qc_step2  = -35  ! refer to Poli et al. (2009)
   integer :: top_level
   real, allocatable :: dndz_obs(:),dndz_mod(:)
   real, allocatable :: dndz2_obs(:),dndz2_mod(:)

   integer :: nn, na, nb, ntotal, nqc0, nqc1, nqc2, nqc3, n_rej
   real    :: percnt
   real    :: height_below(5000)
   character(len=40) :: name_qc(5000)
   character(len=2) :: c_it
   character(len=7) :: c_nt

! t_iwabuchii 20111216 GSI regional QC scheme
   real :: g_height, g_lat, cutoff, stddev     ! QC cutoff GSI
   integer, parameter :: qc_cutoff = -36
   real, allocatable :: model_t(:,:)  ! Model value t at ob location


   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpsref")

   n_rej = 0
!
   if (iv%info(gpsref)%nlocal < 1) then
      if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")
      return
   end if

   ntotal = 0

   allocate (model_ref(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))

   model_ref(:,:) = 0.0

! t_iwabuchi 20111216 (hmn) allocate model_t for GSI regional QC
   allocate (model_t(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))
   model_t(:,:) = 0.0


   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

      do k=1, iv%info(gpsref)%levels(n)
!sychen        if( iv%gpsref(n)%ref(k)%qc == fails_error_max .and. it > 1 ) &
!sychen            iv%gpsref(n)%ref(k)%qc = 0
            if ( ( iv%gpsref(n)%ref(k)%qc == fails_error_max ) .or. &
                 ( iv%gpsref(n)%ref(k)%qc == qc_below ) .or. &
                 ( iv%gpsref(n)%ref(k)%qc == qc_middle ) .or. &
                 ( iv%gpsref(n)%ref(k)%qc == qc_above ) .or. &
                 ( iv%gpsref(n)%ref(k)%qc == qc_step1 ) .or. &
                 ( iv%gpsref(n)%ref(k)%qc == qc_step2 ) ) then
               if( it > 1 ) iv%gpsref(n)%ref(k)%qc = 0
            endif
      end do

      ! Get cross pt. horizontal interpolation weights:

      i   = iv%info(gpsref)%i(1,n)
      j   = iv%info(gpsref)%j(1,n)
      dx  = iv%info(gpsref)%dx(1,n)
      dy  = iv%info(gpsref)%dy(1,n)
      dxm = iv%info(gpsref)%dxm(1,n)
      dym = iv%info(gpsref)%dym(1,n)

      if ( .not. pseudo_ref ) then

         ! Get the zk from gpsref%h:

         do k=kts,kte
            v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                   + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
!
            v_p(k) = dym*(dxm*grid%xb%p(i,j  ,k) + dx*grid%xb%p(i+1,j  ,k)) &
                   + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
         end do
         do k=1, iv%info(gpsref)%levels(n)
            if (iv%gpsref(n)%h(k) > 0.0) &
               call da_to_zk(iv%gpsref(n)%h(k), v_h, v_interp_h, iv%info(gpsref)%zk(k,n))
            if (iv%info(gpsref)%zk(k,n) < 0.0 .and. .not. anal_type_verify) then
               iv%gpsref(n)%ref(k)%qc = missing_data
            end if
         end do
      else
         iv%info(gpsref)%zk(:,n) = pseudo_z
      end if

!
! To assign the retrieved pressure to GPSREF data (YRG, 06/15/2011)............
   do k=1, iv%info(gpsref)%levels(n)
      kk = int (iv%info(gpsref)%zk(k,n))
      if (kk >= kts .and. kk+1 <= kte) then
        dz  = iv%info(gpsref)%zk(k,n) - real(kk)
        dzm = 1.0 - dz
        iv%gpsref(n)%p(k)%inv = v_p(kk  ) * dzm + v_p(kk+1) * dz 
        ob%gpsref(n)%p(k) = iv%gpsref(n)%p(k)%inv
        iv%gpsref(n)%p(k)%qc = -5
      else
        n_rej = n_rej + 1
      endif
   end do
!..............................................................................

   end do

   call da_convert_zk (iv%info(gpsref))

! t_iwabuchi 20111216 (hmn's update) linear interpolation of log(n) 
   call da_interp_lin_3d (grid%xb%reflog, iv%info(gpsref), model_ref)
   allocate (int_t(ims:ime, jms:jme, kms:kme))
   int_t = grid%xb%t
   call da_interp_lin_3d (int_t, iv%info(gpsref), model_t)
   deallocate(int_t)
! t_iwabuchi END 


   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

! t_iwabuchi 20111216 compute exp of log (N)
   do k = 1, iv%info(gpsref)%levels(n)
     model_ref(k,n)=exp(model_ref(k,n))
!     model_t(k,n)=exp(model_t(k,n))
   end do
! t_iwabuchi END

      if ( ( .not. pseudo_ref ) .or. it > 1 ) then
         do k = 1, iv%info(gpsref)%levels(n)
            iv%gpsref(n)%ref(k)%inv = 0.0

            if (ob%gpsref(n)%ref(k) > missing_r .AND. &
                 iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer) then
                 iv%gpsref(n)%ref(k)%inv = ob%gpsref(n)%ref(k) - model_ref(k,n)
            end if
         end do
      else
         ob % gpsref(1)%ref(1) = model_ref(1,n) + iv %gpsref(1)%ref(1)%inv 
      end if
   end do

   if ( pseudo_ref ) then
      ! Done for pseudo_ref after getting iv%gpsref(n)%ref(k)%inv
      deallocate (model_ref)
      deallocate (model_t)
      if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")
      return
   end if

   ! Quality check 1: Gross error(departure from the background) check 
    if ( check_max_iv ) &
       call da_check_max_iv_gpsref(iv, it, num_qcstat_conv, 1)

! refer to Poli et al. (2009) ------------------------------------------------
! flag if fit in each of these two qc steps for both of obs. and model
! qc_step1: dN/dz < -50 km^-1
! qc_step2: abs(d^2N/dz^2) > 100 km^-2
!                                  Shu-Ya Chen (2009-07-29)
   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      if (iv%info(gpsref)%levels(n) <= 2) cycle 
      do k=1,iv%info(gpsref)%levels(n)
         if (model_ref(k,n) > 0.0) top_level=k
      end do
      allocate(dndz_obs(top_level))
      allocate(dndz_mod(top_level))
      allocate(dndz2_obs(top_level))
      allocate(dndz2_mod(top_level))

      ! QC_STEP1

        if (.not. anal_type_verify) then

          ! check for bottom boundary (Forward Difference)
               dndz_obs(1)=(ob%gpsref(n)%ref(2)-ob%gpsref(n)%ref(1))/  &
                           ((iv%gpsref(n)%h(2)-iv%gpsref(n)%h(1))/1000.)
               dndz_mod(1)=(model_ref(2,n)-model_ref(1,n))/  &
                           ((iv%gpsref(n)%h(2)-iv%gpsref(n)%h(1))/1000.)
          ! check for upper boundary (Backward Difference)
               dndz_obs(top_level)= &
                       (ob%gpsref(n)%ref(top_level)-ob%gpsref(n)%ref(top_level-1))/  &
                       ((iv%gpsref(n)%h(top_level)-iv%gpsref(n)%h(top_level-1))/1000.)
               dndz_mod(top_level)= &
                       (model_ref(top_level,n)-model_ref(top_level-1,n))/  &
                       ((iv%gpsref(n)%h(top_level)-iv%gpsref(n)%h(top_level-1))/1000.)
          ! check for middle levels (Central Difference)
            do k=2, top_level-1
               dndz_obs(k)=(ob%gpsref(n)%ref(k+1)-ob%gpsref(n)%ref(k-1))/  &
                           ((iv%gpsref(n)%h(k+1)-iv%gpsref(n)%h(k-1))/1000.)
               dndz_mod(k)=(model_ref(k+1,n)-model_ref(k-1,n))/  &
                           ((iv%gpsref(n)%h(k+1)-iv%gpsref(n)%h(k-1))/1000.)
            end do
            do k=1, top_level
! hmn 20111206
               if (iv%gpsref(n)%ref(k)%qc /= missing_data) then
               if ((dndz_obs(k) < -50.) .or. (dndz_mod(k) < -50.)) then
               iv%gpsref(n)%ref(k)%qc = qc_step1
               end if
! hmn 20111206 
               end if
            end do

      ! QC_STEP2

          ! check for bottom boundary
               dndz2_obs(1)=(dndz_obs(2)-dndz_obs(1))/  &
                            ((iv%gpsref(n)%h(2)-iv%gpsref(n)%h(1))/1000.)
               dndz2_mod(1)=(dndz_mod(2)-dndz_mod(1))/   &
                            ((iv%gpsref(n)%h(2)-iv%gpsref(n)%h(1))/1000.)
          ! check for upper boundary
               dndz2_obs(top_level)=(dndz_obs(top_level)-dndz_obs(top_level-1))/  &
                            ((iv%gpsref(n)%h(top_level)-iv%gpsref(n)%h(top_level-1))/1000.)
               dndz2_mod(top_level)=(dndz_mod(top_level)-dndz_mod(top_level-1))/   &
                            ((iv%gpsref(n)%h(top_level)-iv%gpsref(n)%h(top_level-1))/1000.)
          ! check for middle levels
            do k=2, top_level-1
               dndz2_obs(k)=(dndz_obs(k+1)-dndz_obs(k-1))/  &
                            ((iv%gpsref(n)%h(k+1)-iv%gpsref(n)%h(k-1))/1000.)
               dndz2_mod(k)=(dndz_mod(k+1)-dndz_mod(k-1))/   &
                            ((iv%gpsref(n)%h(k+1)-iv%gpsref(n)%h(k-1))/1000.)
            end do
            do k=1, top_level
! hmn 20111206
               if (iv%gpsref(n)%ref(k)%qc /= missing_data) then
               if ((abs(dndz2_obs(k)) > 100.) .or. (abs(dndz2_mod(k)) > 100.)) then
               iv%gpsref(n)%ref(k)%qc = qc_step2
               end if
               end if

            end do

        end if  ! end of if verify check
   deallocate(dndz_obs,dndz_mod)
   deallocate(dndz2_obs,dndz2_mod)
   end do  ! end of do iv%info(gpsref)%n1~n2
!
! End of Poli's check. (2009) -------------------------------------------------
! 


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

!  process refracticity data n from n1 to n2
!
if (.not. anal_type_verify) then
   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
       do k=1,iv%info(gpsref)%levels(n)
         if (model_ref(k,n) > 0.0) top_level=k

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
! hmn 20111206
               if (iv%gpsref(n)%ref(k)%qc /= missing_data) then
! Check innovation, stddev, and cutoff
         if (abs(iv%gpsref(n)%ref(k)%inv) >= stddev * cutoff) then
           iv%gpsref(n)%ref(k)%qc = qc_cutoff
! hmn 20111202
!           qc_36(k,n)=1
! qc flag
         endif
! hmn 20111206
               end if

       end do
   end do
end if

   deallocate (model_t)
!
! End   GSI-QC REGIONAL TI-GSI ------------------------------------------------
! 
! t_iwabuchi END 




   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      ! Quality check 2: Error percentage check.

      if (.not. anal_type_verify) then
            do k=1, iv%info(gpsref)%levels(n)

               ! incremetal refractivity or the relative error:
               !   abs[(O-B)/{(O+B)/2}]              (Lidia Cucurull 2005)

               ntotal = ntotal + 1
               percnt = 2.0 * abs(iv%gpsref(n)%ref(k)%inv / &
                 (ob%gpsref(n)%ref(k) + model_ref(k,n)))

! hmn 20111206
!              if (iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer) then
               if (iv%gpsref(n)%ref(k)%qc /= missing_data) then

                  if (iv%gpsref(n)%h(k) < h_1) then
                     if (percnt > pcnt1) iv%gpsref(n)%ref(k)%qc = qc_below
                  else if (iv%gpsref(n)%h(k) > h_2) then
                     if (percnt > pcnt3) iv%gpsref(n)%ref(k)%qc = qc_above
                  else
                     if (percnt > pcnt2) iv%gpsref(n)%ref(k)%qc = qc_middle
                  end if
               end if
            end do
      end if  ! end of if verify check
   end do

   ! Quality check 3: Low levels quality control

   if (.not. anal_type_verify) then
         ! Search for the GPS RO's name with the 'qc_below':

       if ( maxval(iv%info(gpsref)%levels(:)) > 1 ) then ! gpsref in profiles
         nn = 0
         height_below = 0.0
         name_qc      = '                                       '

         do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
             nn = nn + 1
             iv%info(gpsref)%levels(n) = iv%info(gpsref)%levels(n)
             do k=1, iv%info(gpsref)%levels(n)
                if (iv%gpsref(n)%ref(k)%qc == qc_below) then
                   name_qc(nn) = iv%info(gpsref)%name(n)
                   height_below(nn) = max(iv%gpsref(n)%h(k),height_below(nn))
                end if
             end do
             if (height_below(nn) == 0.0) nn = nn - 1
         end do

         ! Set the flag qc_below to the levels below percnt < pcnt1::

         ntotal = 0
         nqc0   = 0
         nqc1   = 0
         nqc2   = 0
         nqc3   = 0

         do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
            do na = 1,nn
               if (iv%info(gpsref)%name(n) == name_qc(na)) then
                  do k=1, iv%info(gpsref)%levels(n)
! hmn 20111202
                     if (iv%gpsref(n)%h(k) < height_below(na) .and. &
!                        iv%gpsref(n)%ref(k)%qc >= 0) then
! hmn 20111206
                         iv%gpsref(n)%ref(k)%qc /= missing_data) then
                       iv%gpsref(n)%ref(k)%qc = qc_below
                     end if

                  end do
                  exit
               end if
            end do

            do k=1, iv%info(gpsref)%levels(n)
               ntotal = ntotal + 1
               if (iv%gpsref(n)%ref(k)%qc == fails_error_max) nqc0 = nqc0 + 1
               if (iv%gpsref(n)%ref(k)%qc == qc_middle) nqc1 = nqc1 + 1
               if (iv%gpsref(n)%ref(k)%qc == qc_below) nqc2 = nqc2 + 1
               if (iv%gpsref(n)%ref(k)%qc == qc_above) nqc3 = nqc3 + 1
            end do
         end do
       else    ! gpsref not in profiles
          do na = iv%info(gpsref)%n1, iv%info(gpsref)%n2
             if ( iv%gpsref(na)%ref(1)%qc == qc_below) then
                do nb = iv%info(gpsref)%n1, iv%info(gpsref)%n2
                   if ( iv%info(gpsref)%id(nb) == iv%info(gpsref)%id(na) .and. &
                        iv%info(gpsref)%name(nb) == iv%info(gpsref)%name(na) .and. &
! hmn 20111206
!                        iv%gpsref(nb)%ref(1)%qc >= obs_qc_pointer .and.        &
                        iv%gpsref(nb)%ref(1)%qc /= missing_data .and.            &

                        iv%gpsref(nb)%h(1) <= iv%gpsref(na)%h(1)   ) then
                      iv%gpsref(nb)%ref(1)%qc = qc_below
                   end if
                end do
             end if
          end do
       end if  ! end of if gpsref profiles
   end if  ! end of if verify check
12221 continue

! print out the amounts of obs. rejection   

    if ( check_max_iv ) &
       call da_check_max_iv_gpsref(iv, it, num_qcstat_conv, 2)

! ------------------------------------------------------------------------------
!   GPSRO thinning  (Shu-Ya Chen 20090701)
   if (.not. anal_type_verify) then
    IF ( gpsref_thinning ) THEN
      allocate (used_lev(kms:kme,iv%info(gpsref)%n1:iv%info(gpsref)%n2))
      used_lev(:,:) = missing_data

       DO n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
       allocate(min_dis(kms:kme))
       allocate(qc(iv%info(gpsref)%levels(n)))
       i   = iv%info(gpsref)%i(1,n)
       j   = iv%info(gpsref)%j(1,n)
       dx  = iv%info(gpsref)%dx(1,n)
       dy  = iv%info(gpsref)%dy(1,n)
       dxm = iv%info(gpsref)%dxm(1,n)
       dym = iv%info(gpsref)%dym(1,n)

       ! Get the zk from gpsref%h:
          do k=kts,kte
             v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                    + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
          end do
          do k=kts,kte 
          min_dis(k)=1.0E10
             do l=1, iv%info(gpsref)%levels(n)
                if ( iv%gpsref(n)%ref(l)%qc >= obs_qc_pointer ) then
                distance_h=abs(iv%gpsref(n)%h(l)-v_h(k))
                min_dis(k)=min(min_dis(k),distance_h)
                if ( min_dis(k) == distance_h ) used_lev(k,n)=l
                end if
             end do
          end do

          write(533,*) 'obs_qc_pointer=',obs_qc_pointer,'missing_data=',missing_data
          do k=kts,kte
          write(533,*) n,k,'used_lev=',used_lev(k,n)
          enddo

          do l=1, iv%info(gpsref)%levels(n)
          write(533,*) n,l,iv%gpsref(n)%ref(l)%qc
          enddo

          do l=1, iv%info(gpsref)%levels(n)
          qc(l)=iv%gpsref(n)%ref(l)%qc
          end do
          do k=kts,kte
           qc(used_lev(k,n))=1   ! which level is closest to model level
          end do
       !  data thinning (set thinned levels to be -99)
          do l=1, iv%info(gpsref)%levels(n)
          if ( iv%gpsref(n)%ref(l)%qc >= obs_qc_pointer &
               .and. qc(l) /= 1 ) then
          iv%gpsref(n)%ref(l)%qc = -99
          end if
          end do
       deallocate(min_dis)
       deallocate(qc)
       END DO
       deallocate (used_lev)
    END IF

    goto 12345

! Write out GPS Ref data:

     DO n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
     Iu_ref = 336
     write(c_it,'(I2.2)') it
     c_nt=iv%info(gpsref)%name(n)(8:11)//iv%info(gpsref)%name(n)(28:30)
     open (unit=Iu_ref, file='RO_Innov_'//iv%info(gpsref)%date_char(n)//'_'//c_nt//'.'//c_it, &
           form='formatted')
           write(unit=Iu_ref, fmt='(/i5,2x,a,2x,a,2x,4f10.3,i5)') n, &
              iv%info(gpsref)%date_char(n), iv%info(gpsref)%id(n),  &
              iv%info(gpsref)%lat(1,n)  , iv%info(gpsref)%lon(1,n), &
              iv%info(gpsref)%x(1,n)  , iv%info(gpsref)%y(1,n), &
              iv%info(gpsref)%levels(n)
           write(unit=Iu_ref, fmt='(a5,3x,6a14)') 'level','     height   ', &
                       '    Obs_ref   ','  model_ref   ','  Innov_ref   ', &
                       '  error_ref   ',' qc_ref       '
           do k = 1, iv%info(gpsref)%levels(n)
!             if ( gpsref_thinning ) then
!               if ( iv%gpsref(n)%ref(l)%qc >= obs_qc_pointer ) then
!                  write(unit=Iu_ref, fmt='(i3,1x,5f14.3,i10)')  k, &
!                  iv%gpsref(n)%h(k), ob%gpsref(n)%ref(k),   &
!                  model_ref(k,n), iv%gpsref(n)%ref(k)%inv,   &
!                  iv%gpsref(n)%ref(k)%error, iv%gpsref(n)%ref(k)%qc
!               end if
!             else
               write(unit=Iu_ref, fmt='(i3,1x,5f14.3,i10)')  k, &
                  iv%gpsref(n)%h(k), ob%gpsref(n)%ref(k),   &
                  model_ref(k,n), iv%gpsref(n)%ref(k)%inv,   &
                  iv%gpsref(n)%ref(k)%error, iv%gpsref(n)%ref(k)%qc
!             end if
           end do
     close(Iu_ref)
     END DO
12345 continue
   ! .........................................................................
   end if  ! end of verify check

   deallocate (model_ref)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")

end subroutine da_get_innov_vector_gpsref
