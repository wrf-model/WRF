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
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real,allocatable :: model_ref(:,:) !Model gpsref at ob loc
   real    :: v_h(kms:kme)     ! Model value h at ob

   integer :: Iu_ref, l
   integer,allocatable :: used_lev(:,:) ! for obs. data thinning
                                        ! record the closest level with model
   integer,allocatable :: qc(:)         ! record iv%gpsref(n)%ref(k)%qc
                               ! hor. location.
   real    :: distance_h       ! cal. h-difference between obs and model
   real,allocatable :: min_dis(:)   ! minimum difference
                                               ! hor. location.
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

   integer :: nn, na, nb, ntotal, nqc0, nqc1, nqc2, nqc3
   real    :: percnt
   real    :: height_below(5000)
   character(len=40) :: name_qc(5000)

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpsref")

   ! GPS REF Pseudo OBS test:

   if ( (pseudo_var(1:3) == 'ref' .and. num_pseudo > 0) .and. &
        it == 1 ) then

      ! Deallocate:
      if (iv%info(gpsref)%nlocal > 0) then
         do n = 1, iv%info(gpsref)%nlocal
            deallocate(iv % gpsref(n) % ref)
            deallocate(iv % gpsref(n) %  h)
            deallocate(iv % gpsref(n) %  p)
            deallocate(iv % gpsref(n) %  t)
            deallocate(iv % gpsref(n) %  q)
            deallocate(ob % gpsref(n) % ref)
         end do
         deallocate(iv % gpsref)
         deallocate(ob % gpsref)
      end if

      use_gpsrefobs = .true.

      iv%info(gpsref)%nlocal = num_pseudo
      iv%info(gpsref)%plocal(1) = num_pseudo
      iv%info(gpsref)%ntotal   = num_pseudo
      iv%info(gpsref)%max_lev = 1
      iv%info(pseudo)%nlocal = 0

      call da_allocate_observations (iv)
      iv%info(gpsref)%n1 = 1
      iv%info(gpsref)%n2 = 1

      allocate(iv%gpsref(num_pseudo)%ref(1:1))
      allocate(iv%gpsref(num_pseudo)%  h(1:1))
      allocate(iv%gpsref(num_pseudo)%  p(1:1))
      allocate(iv%gpsref(num_pseudo)%  t(1:1))
      allocate(iv%gpsref(num_pseudo)%  q(1:1))
      allocate(ob%gpsref(1:num_pseudo))
      allocate(ob%gpsref(num_pseudo)%ref(1:1))

      write(stdout,'(a,i2)') '==> GPS REF pseudo OBS test: num_pseudo=',num_pseudo

      iv%info(gpsref)%levels(1) = 1

      iv%info(gpsref)%x(:,1) = pseudo_x
      iv%info(gpsref)%y(:,1) = pseudo_y

      iv%info(gpsref)%i(:,1) = int(pseudo_x)
      iv%info(gpsref)%j(:,1) = int(pseudo_y)
      iv % gpsref(1) %  h(1) = pseudo_z

      iv%info(gpsref)%dx(:,1) = pseudo_x-real(iv%info(gpsref)%i(1,1))
      iv%info(gpsref)%dy(:,1) = pseudo_y-real(iv%info(gpsref)%j(1,1))
      iv%info(gpsref)%dxm(:,1)=1.0-iv%info(gpsref)%dx(1,1)
      iv%info(gpsref)%dym(:,1)=1.0-iv%info(gpsref)%dy(1,1)

      iv % gpsref(1) %ref(1) % inv = pseudo_val
      iv % gpsref(1) %ref(1) % error = pseudo_err
      iv % gpsref(1) %ref(1) % qc = 0

      ! Set halo:
      if ((iv%info(gpsref)%i(1,1) < its-1) .or.(iv%info(gpsref)%i(1,1) > ite) .or. &
          (iv%info(gpsref)%j(1,1) < jts-1) .or.(iv%info(gpsref)%j(1,1) > jte)) then
         iv%info(gpsref)%proc_domain(:,1) = .false.
      else
         iv%info(gpsref)%proc_domain(:,1) = .false. 

         if (iv%info(gpsref)%i(1,1) >= its .and. iv%info(gpsref)%i(1,1) <= ite .and. & 
             iv%info(gpsref)%j(1,1) >= jts .and. iv%info(gpsref)%j(1,1) <= jte) then 
            iv%info(gpsref)%proc_domain(:,1) = .true. 
         end if 
      end if

      write(stdout,'(a4,2f15.5)') pseudo_var, pseudo_val, pseudo_err
      write(stdout,'(3f15.5)')    pseudo_x, pseudo_y, pseudo_z
   end if

   if (iv%info(gpsref)%nlocal < 1) return

   ntotal = 0

   allocate (model_ref(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))

   model_ref(:,:) = 0.0

   allocate (used_lev(kms:kme,iv%info(gpsref)%n1:iv%info(gpsref)%n2))
   used_lev(:,:) = missing_data 

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

      do k=1, iv%info(gpsref)%levels(n)
         if( iv%gpsref(n)%ref(k)%qc == fails_error_max .and. it > 1 ) &
             iv%gpsref(n)%ref(k)%qc = 0
      end do

      ! Get cross pt. horizontal interpolation weights:

      i   = iv%info(gpsref)%i(1,n)
      j   = iv%info(gpsref)%j(1,n)
      dx  = iv%info(gpsref)%dx(1,n)
      dy  = iv%info(gpsref)%dy(1,n)
      dxm = iv%info(gpsref)%dxm(1,n)
      dym = iv%info(gpsref)%dym(1,n)

      if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

         ! Get the zk from gpsref%h:

         do k=kts,kte
            v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                   + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
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
   end do

   call da_convert_zk (iv%info(gpsref))

   call da_interp_lin_3d (grid%xb%ref, iv%info(gpsref), model_ref)

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      if ( (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) .or. &
           it > 1 ) then
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
!     print*,'top_level=',top_level
      allocate(dndz_obs(top_level))
      allocate(dndz_mod(top_level))
      allocate(dndz2_obs(top_level))
      allocate(dndz2_mod(top_level))

      ! QC_STEP1

        if (.not. anal_type_verify) then
          if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then

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
               if ((dndz_obs(k) < -50.) .or. (dndz_mod(k) < -50.)) then
               iv%gpsref(n)%ref(k)%qc = qc_step1
!               print*,'QC_STEP1:',n,k,iv%gpsref(n)%h(k)/1000.,dndz_obs(k),dndz_mod(k)
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
               if ((abs(dndz2_obs(k)) > 100.) .or. (abs(dndz2_mod(k)) > 100.)) then
               iv%gpsref(n)%ref(k)%qc = qc_step2
!               print*,'QC_STEP2:',n,k,iv%gpsref(n)%h(k)/1000.,dndz2_obs(k),dndz2_mod(k)
               end if
            end do
          end if ! end of if pseudo check

        end if  ! end of if verify check
   deallocate(dndz_obs,dndz_mod)
   deallocate(dndz2_obs,dndz2_mod)
   end do  ! end of do iv%info(gpsref)%n1~n2
!
! End of Poli's check. (2009) -------------------------------------------------
! 
   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
      ! Quality check 2: Error percentage check.

      if (.not. anal_type_verify) then
         if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then
            do k=1, iv%info(gpsref)%levels(n)

               ! incremetal refractivity or the relative error:
               !   abs[(O-B)/{(O+B)/2}]              (Lidia Cucurull 2005)

               ntotal = ntotal + 1
               percnt = 2.0 * abs(iv%gpsref(n)%ref(k)%inv / &
                 (ob%gpsref(n)%ref(k) + model_ref(k,n)))

               if (iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer) then

                  if (iv%gpsref(n)%h(k) < h_1) then
                     if (percnt > pcnt1) iv%gpsref(n)%ref(k)%qc = qc_below
                  else if (iv%gpsref(n)%h(k) > h_2) then
                     if (percnt > pcnt3) iv%gpsref(n)%ref(k)%qc = qc_above
                  else
                     if (percnt > pcnt2) iv%gpsref(n)%ref(k)%qc = qc_middle
                  end if
               end if
            end do
         end if
      end if  ! end of if verify check
   end do

   ! Quality check 3: Low levels quality control

   if (.not. anal_type_verify) then
      if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then
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
                     if (iv%gpsref(n)%h(k) < height_below(na) .and. &
                         iv%gpsref(n)%ref(k)%qc >= 0) iv%gpsref(n)%ref(k)%qc = qc_below
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
                        iv%gpsref(nb)%ref(1)%qc >= obs_qc_pointer .and.        &
                        iv%gpsref(nb)%h(1) <= iv%gpsref(na)%h(1)   ) then
                      iv%gpsref(nb)%ref(1)%qc = qc_below
                   end if
                end do
             end if
          end do
       end if  ! end of if gpsref profiles
      end if
   end if  ! end of if verify check

!   ! Quality check 1: Gross error(departure from the background) check 

    if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0) .and. check_max_iv ) &
       call da_check_max_iv_gpsref(iv, it, num_qcstat_conv)

! ------------------------------------------------------------------------------
!   GPSRO thinning  (Shu-Ya Chen 20090701)
   if (.not. anal_type_verify) then
   if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then
    IF ( gpsref_thinning ) THEN
       DO n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
       allocate(min_dis(kms:kme))
       allocate(qc(iv%info(gpsref)%levels(n)))
       i   = iv%info(gpsref)%i(1,n)
       j   = iv%info(gpsref)%j(1,n)
       dx  = iv%info(gpsref)%dx(1,n)
       dy  = iv%info(gpsref)%dy(1,n)
       dxm = iv%info(gpsref)%dxm(1,n)
       dym = iv%info(gpsref)%dym(1,n)

       if (.not.(pseudo_var(1:3) == 'ref' .and. num_pseudo > 0)) then
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
       end if
       deallocate(min_dis)
       deallocate(qc)
       END DO
    END IF

     goto 12345

! Write out GPS Ref data:

     DO n=iv%info(gpsref)%n1,iv%info(gpsref)%n2
     Iu_ref = 336
     open (unit=Iu_ref, file='RO_Innov_'//iv%info(gpsref)%date_char(n), &
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
   end if  ! end of pseudo test
   end if  ! end of verify check

   deallocate (used_lev)
   deallocate (model_ref)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")

end subroutine da_get_innov_vector_gpsref
