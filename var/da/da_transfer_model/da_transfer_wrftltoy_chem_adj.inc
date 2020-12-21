subroutine da_transfer_wrftltoy_chem_adj( grid, iv, jo_grad_y )
   !-------------------------------------------------------------------------
   ! Purpose: Convert analysis increments into WRFAD increments 
   !
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)

   integer :: n, ichem, iplt, ij        ! Loop counters
   integer :: iloc, jloc

   real, allocatable :: model_chem(:)

   if (trace_use) call da_trace_entry("da_transfer_wrftltoy_chem_adj")

   !Surface
   grid%a_surf_hx(:,:) = 0.D0
   model_grid%a_surf_hx(:,:) = 0.D0
   if (iv%info(chem_surf)%nlocal > 0 .and. .not. crossval_chem_surfobs) then
      allocate (model_chem(iv%info(chem_surf)%n1:iv%info(chem_surf)%n2))
      do ichem = 1 ,num_chem_surf-1
         do n=iv%info(chem_surf)%n1,iv%info(chem_surf)%n2
            iloc = iv%info(chem_surf)%i(iplt,n)
            jloc = iv%info(chem_surf)%j(iplt,n)
            if ( iv%info(chem_surf)%proc_domain(1,n) ) then
               model_chem(n) = jo_grad_y%chem_surf(n)%chem(ichem) * iv%chem_surf(n)%weight(ichem)
               grid%a_surf_hx(n,ichem+1) = model_chem(n)
               model_grid%a_surf_hx(n,ichem+1) = grid%a_surf_hx(n,ichem+1)
            end if
         end do
      end do

      deallocate(model_chem)
   end if

   ! Aircraft
   grid%a_acft_hx(:,:,:) = 0.D0
   model_grid%a_acft_hx(:,:,:) = 0.D0
   if (iv%info(chem_acft)%nlocal > 0 .and. .not. crossval_chem_acftobs) then
      allocate (model_chem(iv%info(chem_acft)%n1:iv%info(chem_acft)%n2))
      do ichem = 1 ,num_chem_acft-1
         !$OMP PARALLEL DO &
         !$OMP PRIVATE( ij, iplt, n, iloc, jloc, model_chem )
         do ij = 1 , grid%num_tiles
         do iplt = 1 ,num_platform
            do n=iv%info(chem_acft)%n1,iv%info(chem_acft)%n2
               iloc = iv%info(chem_acft)%i(iplt,n)
               jloc = iv%info(chem_acft)%j(iplt,n)

               if ( iv%info(chem_acft)%proc_domain(iplt,n) ) then
                  model_chem(n) = jo_grad_y%chem_acft(n)%chem(ichem,iplt) * iv%chem_acft(n)%weight(ichem,iplt)
                  grid%a_acft_hx(n,iplt,ichem+1) = model_chem(n)
               end if
            end do
            model_grid%a_acft_hx(:,iplt,ichem+1) = grid%a_acft_hx(:,iplt,ichem+1)
         end do
         end do
         !$OMP END PARALLEL DO
      end do

      deallocate(model_chem)
   end if

   if (trace_use) call da_trace_exit("da_transfer_wrftltoy_chem_adj")

end subroutine da_transfer_wrftltoy_chem_adj
