subroutine da_jo_and_grady_chem_acft(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type),intent(inout) :: jo          ! Obs cost function.

   integer :: n, ichem, iplt

   if (trace_use) call da_trace_entry("da_jo_and_grady_chem_acft")

   jo % chem_acft = 0.0
   jo % chem_acft_cross = 0.0

   do n=1, iv%info(chem_acft)%nlocal
      do ichem = 1, num_chem_acft-1
         do iplt = 1, num_platform

            jo_grad_y%chem_acft(n)%chem(ichem,iplt) = -re%chem_acft(n)%chem(ichem,iplt) / &
               (iv%chem_acft(n)%chem(ichem,iplt)%error * iv%chem_acft(n)%chem(ichem,iplt)%error)

            if (iv%info(chem_acft)%proc_domain(iplt,n)) then
            if ( ( adtl_run_hours .eq. 0 .or. &
                 (iv%chem_acft(n)%tf .le. REAL(adtl_run_hours, 8) ) .and. &
                 (iv%chem_acft(n)%ti .ge. 0.D0 ) ) &
                    .and. .not. crossval_chem_acftobs ) then
                  jo % chem_acft = jo % chem_acft - &
                     re%chem_acft(n)%chem(ichem,iplt) * jo_grad_y%chem_acft(n)%chem(ichem,iplt)
               else
                  jo % chem_acft_cross = jo % chem_acft_cross - &
                     re%chem_acft(n)%chem(ichem,iplt) * jo_grad_y%chem_acft(n)%chem(ichem,iplt)
               end if
            end if
         end do
      end do
   end do

   jo % chem_acft = 0.5 * jo % chem_acft
   jo % chem_acft_cross = 0.5 * jo % chem_acft_cross

   if (trace_use) call da_trace_exit("da_jo_and_grady_chem_acft")

end subroutine da_jo_and_grady_chem_acft

