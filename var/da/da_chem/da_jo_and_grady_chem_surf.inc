subroutine da_jo_and_grady_chem_surf(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type),intent(inout) :: jo          ! Obs cost function.

   integer :: n, ichem

   if (trace_use) call da_trace_entry("da_jo_and_grady_chem_surf")

   jo % chem_surf = 0.0
   jo % chem_surf_cross = 0.0

   do n=1, iv%info(chem_surf)%nlocal
      do ichem = 1, num_chem_surf-1 
         jo_grad_y%chem_surf(n)%chem(ichem) = -re%chem_surf(n)%chem(ichem) / &
            (iv%chem_surf(n)%chem(ichem)%error * iv%chem_surf(n)%chem(ichem)%error)

         if (iv%info(chem_surf)%proc_domain(1,n)) then
            if ( ( adtl_run_hours .eq. 0 .or. &
                 (iv%chem_surf(n)%tf .le. REAL(adtl_run_hours, 8) ) .and. &
                 (iv%chem_surf(n)%ti .ge. 0.D0 ) ) &
                 .and. .not. crossval_chem_surfobs ) then
               jo % chem_surf = jo % chem_surf - &
                  re%chem_surf(n)%chem(ichem) * jo_grad_y%chem_surf(n)%chem(ichem)
            else
               jo % chem_surf_cross = jo % chem_surf_cross - &
                  re%chem_surf(n)%chem(ichem) * jo_grad_y%chem_surf(n)%chem(ichem)
            end if
         end if
      end do
   end do

   jo % chem_surf = 0.5 * jo % chem_surf
   jo % chem_surf_cross = 0.5 * jo % chem_surf_cross

   if (trace_use) call da_trace_exit("da_jo_and_grady_chem_surf")

end subroutine da_jo_and_grady_chem_surf

