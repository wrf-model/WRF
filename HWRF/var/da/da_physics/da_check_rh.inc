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

   integer :: i, j, k, ij
   real    :: tol_adjust_moist, tol_moist, oldrha, each_moist, es, weight
   real    :: upper_modify_rh, lower_modify_rh
   real    :: t_tropp(ims:ime,jms:jme)
   integer :: k_tropp(ims:ime,jms:jme)

   if (trace_use) call da_trace_entry("da_check_rh")


   upper_modify_rh = 95.0
   lower_modify_rh = 11.0

   ! To get the grid%xa%rh for the moisture adjustments
   call da_tpq_to_rh_lin (grid)

   ! find the k index of tropopause
   t_tropp = 999.0  ! initialize
   k_tropp = kte    ! initialize
   do k = kte, kts, -1
      do j = jts, jte
         do i = its, ite
            if ( grid%xb%t(i,j,k) < t_tropp(i,j) .and.  &
                 grid%xb%p(i,j,k) > 5000.0 ) then  ! tropopause should not
                                                   ! be higher than 50 hPa
               t_tropp(i,j) = grid%xb%t(i,j,k)
               k_tropp(i,j) = k
            end if
         end do
      end do
   end do

   !$OMP PARALLEL DO  SCHEDULE (DYNAMIC, 1) &
   !$OMP PRIVATE ( i, j, k, tol_adjust_moist, tol_moist) &
   !$OMP PRIVATE ( weight, oldrha, each_moist, imod, dz, x_qs, rhtol, es)
!  do ij = 1 , grid%num_tiles

   do i=its,ite
      !do j=grid%j_start(ij), grid%j_end(ij)
      do j=jts, jte

         tol_adjust_moist = 0.0
         tol_moist        = 0.0

         do k=kts,kte
            dz(k)=grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)

            imod(k)           = 0
            x_qs(k)           = 0.0
            rhtol(k)          = grid%xb%rh(i,j,k)+grid%xa%rh(i,j,k)
         end do

         do k=kts, k_tropp(i,j)
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
            do k=kts, k_tropp(i,j)
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
            do k=kts, k_tropp(i,j)
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
           do k=kts, k_tropp(i,j)
             if (imod(k) .eq. 1) then
               grid%xa%rh(i,j,k) = grid%xa%rh(i,j,k)+(grid%xb%rh(i,j,k)+grid%xa%rh(i,j,k))*weight

! To guarantee the adjusted relative humidity will not be out of the range (YRG, 10/21/2008):
               oldrha = grid%xa%rh(i,j,k)+grid%xb%rh(i,j,k)
               if ( (oldrha > maximum_rh) ) then
                  grid%xa%rh(i,j,k) = maximum_rh - grid%xb%rh(i,j,k)
                  if (print_detail_xa ) &
                     write(unit=stdout, fmt='(3I5," Warning=== Adjusted RH > maximum_rh=",f10.2,&
                            & " total_rh, xb%rh, xa%rh:",3f10.2)') &
                          i, j, k, maximum_rh, oldrha, grid%xb%rh(i,j,k), grid%xa%rh(i,j,k)
               else if ( oldrha < minimum_rh ) then
                  grid%xa%rh(i,j,k) = minimum_rh - grid%xb%rh(i,j,k)
                  if (print_detail_xa ) &
                     write(unit=stdout, fmt='(3I5," Warning=== Adjusted RH < minimum_rh=",f10.2,&
                            & " total_rh, xb%rh, xa%rh:",3f10.2)') &
                          i, j, k, minimum_rh, oldrha, grid%xb%rh(i,j,k), grid%xa%rh(i,j,k)
               endif
! ...........................................................................................

               call da_tprh_to_q_lin1(grid%xb%t(i,j,k), grid%xb%p(i,j,k), grid%xb%es(i,j,k), &
                                      grid%xb%q(i,j,k), grid%xb%rh(i,j,k),  grid%xa%t(i,j,k), &
                                      grid%xa%p(i,j,k), grid%xa%rh(i,j,k), grid%xa%q(i,j,k))

             end if
           end do
         end if
      end do
   end do

!  end do
   !$OMP END PARALLEL DO

   if (trace_use) call da_trace_exit("da_check_rh")

end subroutine da_check_rh


