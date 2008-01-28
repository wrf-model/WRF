subroutine da_check_rh(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Remove RH over 100% or under 10%
   !          Made the modification to those levels, which RH are less than 95%
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   integer   :: imod(kts:kte)
   real      :: rhtol(kts:kte)
   real      :: x_qs(kts:kte)
   real      :: dz(kts:kte)

   integer :: i, j, k
   real    :: tol_adjust_moist, tol_moist, oldrha, each_moist, es, weight
   real    :: upper_modify_rh, lower_modify_rh

   if (trace_use) call da_trace_entry("da_check_rh")


   upper_modify_rh = 95.0
   lower_modify_rh = 11.0

   ! To get the grid%xa%rh for the moisture adjustments

   if (cv_options_hum == cv_options_hum_specific_humidity) then
      call da_tpq_to_rh_lin (grid)
   end if

   do i=its,ite
      do j=jts,jte

         tol_adjust_moist = 0.0
         tol_moist        = 0.0

         do k=kts,kte
            dz(k)=grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)

            imod(k)           = 0
            x_qs(k)           = 0.0
            rhtol(k)          = grid%xb%rh(i,j,k)+grid%xa%rh(i,j,k)
         end do

         do k=kts,kte
            if (rhtol(k) .gt. maximum_rh) then
               oldrha       = grid%xa%rh(i,j,k)
               ! modify grid%xa%rh
               grid%xa%rh(i,j,k) = maximum_rh - grid%xb%rh(i,j,k)

               call da_tp_to_qs(grid%xb%t(i,j,k)+grid%xa%t(i,j,k), &
                  grid%xb%p(i,j,k)+grid%xa%p(i,j,k), es, x_qs(k))

               ! calculate grid%xa%q
               call da_tprh_to_q_lin1(grid%xb%t(i,j,k), grid%xb%p(i,j,k), &
                  grid%xb%es(i,j,k), grid%xb%q(i,j,k), grid%xb%rh(i,j,k),  grid%xa%t(i,j,k), &
                  grid%xa%p(i,j,k), grid%xa%rh(i,j,k), grid%xa%q(i,j,k))

               tol_adjust_moist = tol_adjust_moist + x_qs(k)*(oldrha - &
                  grid%xa%rh(i,j,k))* dz(k)*(grid%xb%rho(i,j,k)+grid%xa%rho(i,j,k))
               imod(k)=-1
            end if

            if (rhtol(k) .lt. minimum_rh) then
               oldrha           = grid%xa%rh(i,j,k)
               grid%xa%rh(i,j,k)     = minimum_rh - grid%xb%rh(i,j,k)
               call da_tp_to_qs(grid%xb%t(i,j,k)+grid%xa%t(i,j,k), &
                  grid%xb%p(i,j,k)+grid%xa%p(i,j,k), es, x_qs(k))

               call da_tprh_to_q_lin1(grid%xb%t(i,j,k), grid%xb%p(i,j,k), &
                  grid%xb%es(i,j,k), grid%xb%q(i,j,k), grid%xb%rh(i,j,k),  grid%xa%t(i,j,k), &
                  grid%xa%p(i,j,k), grid%xa%rh(i,j,k), grid%xa%q(i,j,k))


               tol_adjust_moist = tol_adjust_moist + x_qs(k)*(oldrha - &
                  grid%xa%rh(i,j,k))* dz(k)*(grid%xb%rho(i,j,k)+grid%xa%rho(i,j,k))
               imod(k)=-1
            end if
         end do

         if (tol_adjust_moist .gt. 0.0) then
            do k=kts,kte
               if (rhtol(k) .lt. upper_modify_rh .and. imod(k) .eq. 0) then
                  call da_tp_to_qs(grid%xb%t(i,j,k)+grid%xa%t(i,j,k), &
                                    grid%xb%p(i,j,k)+grid%xa%p(i,j,k),es,x_qs(k))

                  each_moist   = rhtol(k)*x_qs(k)* &
                                 dz(k)*(grid%xb%rho(i,j,k)+grid%xa%rho(i,j,k))
                  tol_moist    = tol_moist + each_moist
                  imod(k)      = 1
               end if
            end do
         end if

         if (tol_adjust_moist .lt. 0.0) then
            do k=kts,kte
               if (rhtol(k) .gt. lower_modify_rh .and. imod(k) .eq. 0) then
                  call da_tp_to_qs(grid%xb%t(i,j,k)+grid%xa%t(i,j,k), &
                                    grid%xb%p(i,j,k)+grid%xa%p(i,j,k), es, x_qs(k))

                  each_moist   = rhtol(k)*x_qs(k)* &
                                 dz(k)*(grid%xb%rho(i,j,k)+grid%xa%rho(i,j,k))
                  tol_moist    = tol_moist + each_moist
                  imod(k)      = 1
               end if
            end do
         end if

         if (tol_moist > 0) then
           weight       = tol_adjust_moist/tol_moist
           do k=kts,kte
             if (imod(k) .eq. 1) then
               grid%xa%rh(i,j,k) = grid%xa%rh(i,j,k)+(grid%xb%rh(i,j,k)+grid%xa%rh(i,j,k))*weight
               call da_tprh_to_q_lin1(grid%xb%t(i,j,k), grid%xb%p(i,j,k), grid%xb%es(i,j,k), &
                                      grid%xb%q(i,j,k), grid%xb%rh(i,j,k),  grid%xa%t(i,j,k), &
                                      grid%xa%p(i,j,k), grid%xa%rh(i,j,k), grid%xa%q(i,j,k))

             end if
           end do
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_check_rh")

end subroutine da_check_rh


