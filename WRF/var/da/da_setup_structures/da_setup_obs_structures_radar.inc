subroutine da_setup_obs_structures_radar( grid, ob, iv )

   !-------------------------------------------------------------------------
   ! Purpose: Define, allocate and read radar observation structure.
   !-------------------------------------------------------------------------

   implicit none

   type (y_type),  intent(out)   :: ob  ! Observation structure.
   type (iv_type), intent(inout) :: iv  ! O-B structure.
   type (domain),  intent(inout) :: grid  ! First guess structure

   character(len=filename_len)  :: filename
   integer                      :: n, i, j, k
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)

   if (trace_use) call da_trace_entry("da_setup_obs_structures_radar")

   call init_constants_derived

   !--------------------------------------------------------------------------
   ! [1.0] Scan RADAR observation header and get number of obs:
   !--------------------------------------------------------------------------
   if (num_fgat_time > 1) then
      do n=1, num_fgat_time

         iv%time = n
         filename = ' '

         ! scan radar observation file
         write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.radar'
         call da_scan_obs_radar(iv, filename, grid)

         iv%info(radar)%plocal(n) = iv%info(radar)%nlocal
         iv%info(radar)%ptotal(n) = iv%info(radar)%ntotal
      end do
   else
      iv%time = 1
      ! scan main body of radar observation file
      call da_scan_obs_radar(iv, 'ob.radar', grid)
      iv%info(radar)%plocal(iv%time) = iv%info(radar)%nlocal
      iv%info(radar)%ptotal(iv%time) = iv%info(radar)%ntotal
   end if

   !--------------------------------------------------------------------------
   ! Allocate based on input number of obs:
   !--------------------------------------------------------------------------
   ! This logic was originally found in da_allocate_observations; moved here
   if (iv%info(radar)%nlocal > 0) allocate(iv%radar (1:iv%info(radar)%nlocal))
   if (iv%info(radar)%nlocal > 0) then
      allocate (iv%info(radar)%name(iv%info(radar)%nlocal))
      allocate (iv%info(radar)%platform(iv%info(radar)%nlocal))
      allocate (iv%info(radar)%id(iv%info(radar)%nlocal))
      allocate (iv%info(radar)%date_char(iv%info(radar)%nlocal))
      allocate (iv%info(radar)%levels(iv%info(radar)%nlocal))
      allocate (iv%info(radar)%lat(iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%lon(iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%elv(iv%info(radar)%nlocal))

      allocate (iv%info(radar)%x  (kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%y  (kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%i  (kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%j  (kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%dx (kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%dxm(kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%dy (kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%dym(kms:kme,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%k  (iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%dz (iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%dzm(iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%zk (iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%proc_domain(iv%info(radar)%max_lev,iv%info(radar)%nlocal))
      allocate (iv%info(radar)%obs_global_index(iv%info(radar)%nlocal))
      allocate (iv%info(radar)%thinned(iv%info(radar)%max_lev,iv%info(radar)%nlocal))

      iv%info(radar)%proc_domain(:,:)  = .false.
      iv%info(radar)%thinned(:,:)      = .false.
      iv%info(radar)%zk(:,:)           = missing_r
   end if

   if (num_fgat_time > 1) then

      do n=1, num_fgat_time
         iv%time = n
         filename = ' '  

         ! read radar observation file
         write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.radar'
         call da_read_obs_radar(iv, filename, grid)

      end do
   else
      iv%time = 1

      ! read radar observation file
      call da_read_obs_radar(iv, 'ob.radar', grid)
   end if


   ! Calculate DT for RF DA

   if (use_radar_rf) then
      if (.not. DT_cloud_model) then
         do j = jts,jte
            do i = its, ite
               do k = kts, kte
                   grid%xb%delt(i,j,k) = 0.0
               end do
            end do
         end do

         do n = 1, iv%info(radar)%nlocal
            do i=int(iv%info(radar)%i(1,n)), int(iv%info(radar)%i(1,n))+1
               do j=int(iv%info(radar)%j(1,n)), int(iv%info(radar)%j(1,n))+1
                  do k=kts, kte
                     grid%xb%delt(i,j,k) = 1800.0
                     grid%xb%qrn(i,j,k) = amax1(5.0E-8, grid%xb%qrn(i,j,k))
                     grid%xb%qcw(i,j,k) = amax1(5.0E-12, grid%xb%qcw(i,j,k))
                  end do
               end do
            end do
         end do
      end if
   end if

  
   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures_radar(iv, ob)

   iv%time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_radar")
end subroutine da_setup_obs_structures_radar

