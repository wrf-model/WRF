subroutine da_allocate_obs_info (iv, ob_index)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate info components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv       ! Observation structure.
   integer, intent(in), optional :: ob_index ! ob type index as defined in da_control.f90

   integer :: i
   integer :: istart, iend

   if (trace_use) call da_trace_entry("da_allocate_obs_info")

   ! default loop through all types
   istart = 1
   iend   = num_ob_indexes

   if ( present(ob_index) ) then
      ! allocate for specific ob type
      istart = ob_index
      iend   = ob_index
   end if

   do i = istart, iend

      if (iv%info(i)%nlocal > 0) then
         allocate (iv%info(i)%name(iv%info(i)%nlocal))
         allocate (iv%info(i)%platform(iv%info(i)%nlocal))
         allocate (iv%info(i)%id(iv%info(i)%nlocal))
         allocate (iv%info(i)%date_char(iv%info(i)%nlocal))
         allocate (iv%info(i)%levels(iv%info(i)%nlocal))
         allocate (iv%info(i)%lat(iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%lon(iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%elv(iv%info(i)%nlocal))
         allocate (iv%info(i)%pstar(iv%info(i)%nlocal))

         allocate (iv%info(i)%slp(iv%info(i)%nlocal))
         allocate (iv%info(i)%pw(iv%info(i)%nlocal))

         allocate (iv%info(i)%x  (kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%y  (kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%i  (kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%j  (kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%dx (kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%dxm(kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%dy (kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%dym(kms:kme,iv%info(i)%nlocal))
         allocate (iv%info(i)%k  (iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%dz (iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%dzm(iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%zk (iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%proc_domain(iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%thinned(iv%info(i)%max_lev,iv%info(i)%nlocal))
         allocate (iv%info(i)%obs_global_index(iv%info(i)%nlocal))

         iv%info(i)%proc_domain(:,:)  = .false.
         iv%info(i)%thinned(:,:)      = .false.
         iv%info(i)%zk(:,:)           = missing_r
      end if
   end do

   if (trace_use) call da_trace_exit("da_allocate_obs_info")

end subroutine da_allocate_obs_info


