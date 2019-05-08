subroutine da_calculate_w_chem( iv, re )
   !-------------------------------------------------------------------------
   ! Purpose: Convert analysis increments into WRFAD increments 
   !
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: re          ! o-b
   
   real    :: resi, sigi
   integer :: n, ichem, iplt        ! Loop counters


   if (trace_use) call da_trace_entry("da_calculate_w_chem")

   if (iv%info(chem_surf)%nlocal > 0 ) then
      do n=iv%info(chem_surf)%n1,iv%info(chem_surf)%n2
         if ( weight_signif .gt. 0 ) then
            do ichem = 1 ,num_chem_surf-1
               resi = - re%chem_surf(n)%chem(ichem)
               sigi = iv%chem_surf(n)%chem(ichem)%error
               iv%chem_surf(n)%weight(ichem) = &
                         ( ( erf( abs( resi ) / ( sigma_r_surf*sqrt(2.D0) ) ) )**0.5D0 * &
                           ( erf( abs( resi ) / ( sigi*sqrt(2.D0) ) ) )**0.5D0 )**2
            end do
         else
            iv%chem_surf(n)%weight(:) = 1.D0
         end if
      end do
   end if

   if (iv%info(chem_acft)%nlocal > 0 ) then
      do n=iv%info(chem_acft)%n1,iv%info(chem_acft)%n2
         if ( weight_signif .gt. 0 ) then
            do ichem = 1 ,num_chem_acft-1
               do iplt = 1 ,num_platform
                  resi = - re%chem_acft(n)%chem(ichem,iplt)
                  sigi = iv%chem_acft(n)%chem(ichem,iplt)%error
                  iv%chem_acft(n)%weight(ichem,iplt) = &
                            ( ( erf( abs( resi ) / ( sigma_r_acft*sqrt(2.D0) ) ) )**0.5D0 * &
                              ( erf( abs( resi ) / ( sigi*sqrt(2.D0) ) ) )**0.5D0 )**2
               end do
            end do
         else
            iv%chem_acft(n)%weight(:,:) = 1.D0
         end if
      end do
   end if

   if (trace_use) call da_trace_exit("da_calculate_w_chem")

end subroutine da_calculate_w_chem 

