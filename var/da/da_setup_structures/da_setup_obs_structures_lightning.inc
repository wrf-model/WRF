subroutine da_setup_obs_structures_lightning( grid, ob, iv )

   !-------------------------------------------------------------------------
   ! Purpose: Define, allocate and read lightning observation structure.
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)    
   !-------------------------------------------------------------------------

   implicit none

   type (y_type),  intent(out)   :: ob  ! Observation structure.
   type (iv_type), intent(inout) :: iv  ! O-B structure.
   type (domain),  intent(inout) :: grid  ! First guess structure

   character(len=filename_len)  :: filename
   integer                      :: n, i, j, k
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)

   if (trace_use) call da_trace_entry("da_setup_obs_structures_lightning")

   call init_constants_derived

   !--------------------------------------------------------------------------
   ! [1.0] Scan lightning observation header and get number of obs:
   !--------------------------------------------------------------------------
   if (num_fgat_time > 1) then
      do n=1, num_fgat_time

         iv%time = n
         filename = ' '

         ! scan lightning observation file
         write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.lightning'
         call da_scan_obs_lightning(iv, filename, grid)

         iv%info(lightning)%plocal(n) = iv%info(lightning)%nlocal
         iv%info(lightning)%ptotal(n) = iv%info(lightning)%ntotal
      end do
   else
      iv%time = 1
      ! scan main body of lightning observation file
      call da_scan_obs_lightning(iv, 'ob.lightning', grid)
      iv%info(lightning)%plocal(iv%time) = iv%info(lightning)%nlocal
      iv%info(lightning)%ptotal(iv%time) = iv%info(lightning)%ntotal
   end if

   !--------------------------------------------------------------------------
   ! Allocate based on input number of obs:
   !--------------------------------------------------------------------------
   ! This logic was originally found in da_allocate_observations; moved here
   if (iv%info(lightning)%nlocal > 0) allocate(iv%lightning (1:iv%info(lightning)%nlocal))
   if (iv%info(lightning)%nlocal > 0) then
      allocate (iv%info(lightning)%name(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%platform(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%id(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%date_char(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%levels(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%lat(iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%lon(iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%elv(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%pstar(iv%info(lightning)%nlocal))

      allocate (iv%info(lightning)%slp(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%pw(iv%info(lightning)%nlocal))

      allocate (iv%info(lightning)%x  (kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%y  (kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%i  (kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%j  (kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%dx (kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%dxm(kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%dy (kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%dym(kms:kme,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%k  (iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%dz (iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%dzm(iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%zk (iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%proc_domain(iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%obs_global_index(iv%info(lightning)%nlocal))
      allocate (iv%info(lightning)%thinned(iv%info(lightning)%max_lev,iv%info(lightning)%nlocal))

      iv%info(lightning)%proc_domain(:,:)  = .false.
      iv%info(lightning)%thinned(:,:)      = .false.
      iv%info(lightning)%zk(:,:)           = missing_r
   end if

   if (num_fgat_time > 1) then

      do n=1, num_fgat_time
         iv%time = n
         filename = ' '  

         ! read lightning observation file
         write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.lightning'
         call da_read_obs_lightning(iv, filename, grid)

      end do
   else
      iv%time = 1

      ! read lightning observation file
      call da_read_obs_lightning(iv, 'ob.lightning', grid)
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures_lightning(iv, ob)

   iv%time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_lightning")
end subroutine da_setup_obs_structures_lightning


