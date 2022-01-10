subroutine da_get_innov_vector_gpsref(it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: calculate innovation vectors of gpsro refractivity
   ! History:
   !   2020-06: move QC out of da_get_innov_vector_gpsref
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

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

   character(len=2) :: c_it
   character(len=7) :: c_nt


   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpsref")

! all processors (even when nlocal=0) have to call da_qc_gpsref later in this subroutine
!   if (iv%info(gpsref)%nlocal < 1) then
!      if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")
!      return
!   end if

   allocate (model_ref(iv%info(gpsref)%max_lev,iv%info(gpsref)%n1:iv%info(gpsref)%n2))

   model_ref(:,:) = 0.0

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

      do k=1, iv%info(gpsref)%levels(n)
         if ( ( iv%gpsref(n)%ref(k)%qc == fails_error_max )    .or. &
              ( iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_below )  .or. &
              ( iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_middle ) .or. &
              ( iv%gpsref(n)%ref(k)%qc == qcflag_pcnt_above )  .or. &
              ( iv%gpsref(n)%ref(k)%qc == qcflag_dndz )        .or. &
              ( iv%gpsref(n)%ref(k)%qc == qcflag_dndz2 )       .or. &
              ( iv%gpsref(n)%ref(k)%qc == qcflag_cutoff ) ) then
            if( it > 1 ) iv%gpsref(n)%ref(k)%qc = 0
         end if
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
         endif
      end do
!..............................................................................

   end do

   call da_convert_zk (iv%info(gpsref))

! t_iwabuchi 20111216 (hmn's update) linear interpolation of log(n) 
   call da_interp_lin_3d (grid%xb%reflog, iv%info(gpsref), model_ref)

   do n=iv%info(gpsref)%n1,iv%info(gpsref)%n2

! t_iwabuchi 20111216 compute exp of log (N)
      do k = 1, iv%info(gpsref)%levels(n)
        model_ref(k,n)=exp(model_ref(k,n))
      end do

      if ( ( .not. pseudo_ref ) .or. it > 1 ) then
         do k = 1, iv%info(gpsref)%levels(n)
            iv%gpsref(n)%ref(k)%inv = 0.0

            if (ob%gpsref(n)%ref(k) > missing_r .AND. &
                 !iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer) then
                 iv%gpsref(n)%ref(k)%qc /= missing_data) then
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
      if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")
      return
   end if

   if ( .not. anal_type_verify ) then
      call da_qc_gpsref(it, grid, ob, iv)
   end if

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

! Write out GPS Ref data:

   if ( write_iv_gpsref ) then
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
   end if ! write_iv_gpsref
   ! .........................................................................
   end if  ! end of verify check

   deallocate (model_ref)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpsref")

end subroutine da_get_innov_vector_gpsref
