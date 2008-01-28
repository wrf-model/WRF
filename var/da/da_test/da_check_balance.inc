subroutine da_check_balance(phi, phi_u)

   !---------------------------------------------------------------------------
   ! Purpose: Compare balanced mass (phi_b - function of wind) and actual phi.
   !
   ! Method:  Calculate correlation between balanced and actual phi.
   !---------------------------------------------------------------------------

   implicit none
      
   real, intent(in)             :: phi(:,:,:)      ! Total phi.
   real, intent(in)             :: phi_u(:,:,:)    ! Unbalanced phi.

   integer                      :: iy              ! Size of 1st dimension.
   integer                      :: jx              ! Size of 2nd dimension.
   integer                      :: kz              ! Size of 3rd dimension.
   integer                      :: i, k            ! Loop counters
   real                         :: corr_coef       ! Correlation coefficient.
   real                         :: accurac         ! Accuracy.
   real, allocatable            :: phi_b1(:)       ! 1D balanced phi.
   real, allocatable            :: phi_b2(:,:)     ! 2D balanced phi.
   real, allocatable            :: corr_coeff(:,:) ! Correlation coefficient.
   real, allocatable            :: accuracy(:,:)   ! Accuracy.

   if (trace_use) call da_trace_entry("da_check_balance")
          
   if (balance_type == balance_geo) then
      write(unit=stdout, fmt='(a)') ' da_check_balance: Balance is geostrophic.'
   else if (balance_type == balance_cyc) then
      write(unit=stdout, fmt='(a)') &
         ' da_check_balance: Balance is cyclostrophic.'
   else if (balance_type == balance_geocyc) then
      write(unit=stdout, fmt='(a)') &
         ' da_check_balance: Balance is geo/cyclostrophic.'
   end if
      
   write(unit=stdout, fmt='(a)') ' da_check_balance: Correlation/accuracy: '
      
   !-------------------------------------------------------------------------
   ! [1.0]: Initialise:
   !-------------------------------------------------------------------------  

   iy = size(phi_u, DIM=1)
   jx = size(phi_u, DIM=2)
   kz = size(phi_u, DIM=3)
      
   allocate(phi_b1(1:jx))
   allocate(phi_b2(1:iy,1:jx))

   allocate(corr_coeff(1:kz,1:iy))
   corr_coeff(1:kz,1:iy) = 0.0

   allocate(accuracy(1:kz,1:iy))
   accuracy(1:kz,1:iy) = 0.0
      
   !-------------------------------------------------------------------------
   ! [2.0]: Calculate correlations/accuracy:
   !-------------------------------------------------------------------------  

   do k = 1, kz
      do i = 1, iy

         phi_b1(2:jx-1) = phi(i,2:jx-1,k) - phi_u(i,2:jx-1,k)
            
         call da_correlation_coeff1d(phi_b1(2:jx-1), phi(i,2:jx-1,k), &
                                      corr_coeff(k,i), accuracy(k,i))
     
         ! write(58,*) corr_coeff(k,i), accuracy(k,i)
      end do
         
      phi_b2(2:iy-1,2:jx-1) = phi(2:iy-1,2:jx-1,k) - phi_u(2:iy-1,2:jx-1,k)
      call da_correlation_coeff2d(phi_b2(2:iy-1,2:jx-1), &
                                   phi(2:iy-1,2:jx-1,k), &
                                   corr_coef, accurac)

      write(unit=stdout, fmt='(i6,1pe9.2,1pe9.2)') &
            k, corr_coef, accurac

   end do

   !-------------------------------------------------------------------------
   ! [3.0]: Tidy up:
   !-------------------------------------------------------------------------  

   deallocate(phi_b1)
   deallocate(phi_b2)
   deallocate(corr_coeff)
   deallocate(accuracy)

   if (trace_use) call da_trace_exit("da_check_balance")

end subroutine da_check_balance


