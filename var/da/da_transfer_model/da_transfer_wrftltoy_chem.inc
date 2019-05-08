subroutine da_transfer_wrftltoy_chem( grid, iv, y )
   !-------------------------------------------------------------------------
   ! Purpose: Convert analysis increments into WRFAD increments 
   !
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: y           ! y = h (grid%xa) (linear)

   integer :: n, ichem, iplt        ! Loop counters
   integer :: iloc, jloc

   real, allocatable :: model_chem(:)

   if (trace_use) call da_trace_entry("da_transfer_wrftltoy_chem")

   !Surface
   grid%g_surf_hx(:,:) = 0.D0
   if (iv%info(chem_surf)%nlocal > 0 .and. .not. crossval_chem_surfobs) then
      allocate (model_chem(iv%info(chem_surf)%n1:iv%info(chem_surf)%n2))
      grid%g_surf_hx(:,:) = 0.D0
      do ichem = 1 ,num_chem_surf-1
         do n=iv%info(chem_surf)%n1,iv%info(chem_surf)%n2
            iloc = iv%info(chem_surf)%i(iplt,n)
            jloc = iv%info(chem_surf)%j(iplt,n)
            if ( iv%info(chem_surf)%proc_domain(1,n) ) then
               grid%g_surf_hx(n,ichem+1) = model_grid%g_surf_hx(n,ichem+1)
               model_chem(n) = grid%g_surf_hx(n,ichem+1)
               y%chem_surf(n)%chem(ichem) = model_chem(n)
            end if
         end do
      end do
      model_grid%g_surf_hx(:,:) = 0.D0
      deallocate(model_chem)
   end if

   ! Aircraft
   grid%g_acft_hx(:,:,:) = 0.D0
   if (iv%info(chem_acft)%nlocal > 0 .and. .not. crossval_chem_acftobs) then
      allocate (model_chem(iv%info(chem_acft)%n1:iv%info(chem_acft)%n2))
      grid%g_acft_hx(:,:,:) = 0.D0
      do ichem = 1 ,num_chem_acft-1
         do iplt = 1 ,num_platform
            grid%g_acft_hx(:,iplt,ichem+1) = model_grid%g_acft_hx(:,iplt,ichem+1)
            do n=iv%info(chem_acft)%n1,iv%info(chem_acft)%n2
               iloc = iv%info(chem_acft)%i(iplt,n)
               jloc = iv%info(chem_acft)%j(iplt,n)
               if ( iv%info(chem_acft)%proc_domain(iplt,n) ) then
                  model_chem(n) = grid%g_acft_hx(n,iplt,ichem+1)
                  y%chem_acft(n)%chem(ichem,iplt) = model_chem(n)
               end if
            end do
         end do
      end do
      model_grid%g_acft_hx(:,:,:) = 0.D0
      deallocate(model_chem)
   end if

   if (trace_use) call da_trace_exit("da_transfer_wrftltoy_chem")

end subroutine da_transfer_wrftltoy_chem
