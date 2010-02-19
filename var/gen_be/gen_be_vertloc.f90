program gen_be_vertloc

   use da_gen_be, only : da_eof_decomposition

   implicit none

   integer, parameter    :: ni = 1000 
   integer, parameter    :: nk = 44
   integer, parameter    :: ktarget = 44
   integer, parameter    :: stdout = 6 
 
   real, parameter       :: rscale = 0.1
   real, parameter       :: var_threshold = 0.99

   integer               :: i, k, k1, m
   integer               :: nm
   real                  :: ni1_inv
   real                  :: nk_inv, nk3_inv
   real                  :: kscale, kscale_invsq
   real                  :: kdist
   real                  :: r
   real                  :: totvar, totvar_inv, cumvar
   real                  :: rho(1:nk,1:nk)
   real                  :: e(1:ni,1:nk)
   real                  :: cov(1:nk,1:nk)
   real*8                :: eval(1:nk)
   real*8                :: evec(1:nk,1:nk)
   real                  :: v(1:nk)
   real                  :: x(1:nk)

   ni1_inv = 1.0 / real (ni - 1)
   nk_inv = 1.0 / real (nk)
   nk3_inv = 1.0 / real (nk-3)

!  Specify probability densities:
   do k = 1, nk
      kscale = 10.0 * real(k) * nk_inv  ! Very simple parametrization of vertical variation of localization scale.
!      kscale = 3.0 + 7.0 * real(k-3) * nk3_inv  ! Very simple parametrization of vertical variation of localization scale.
!      kscale = 10.0
      kscale_invsq = 1.0 / ( kscale * kscale )
      do k1 = k, nk
         kdist = k1 - k
         rho(k,k1) = exp ( -real(kdist * kdist) * kscale_invsq )
         rho(k1,k) = rho(k,k1)
      end do
   end do
   cov = rho

!  Create random variables:
!   k = 1
!   do i = 1, ni
!      do k1 = 1, nk
!         call da_gauss_noise( r )
!         r = rscale * r
!         e(i,k1) = rho(k,k1) + r
!         write(17,'(i5,f15.5)')k1, e(i,k1)
!      end do
!      k = k + 1
!      if ( k > nk ) k = 1
!   end do

!  Calculate covariance matrix:

!   do k = 1, nk
!      do k1 = k, nk
!         cov(k,k1) = ni1_inv * sum( e(1:ni,k) * e(1:ni,k1) )
!         cov(k,k1) = rho(k,k1)
!         cov(k1,k) = cov(k,k1)
!      end do
!   end do

!------------------------------------------------------------------------------------------------
! Calculate eigenvectors/values:
!------------------------------------------------------------------------------------------------

   call da_eof_decomposition( nk, cov, evec, eval )

!  Eliminate negative eigenvalues:
   totvar = sum(eval(1:nk))
   do k = 1, nk
     if ( eval(k) < 0.0 ) eval(k) = 0.0
   end do
   totvar_inv = 1.0 / sum(eval(1:nk))
   eval(:) = eval(:) * totvar * totvar_inv ! Preserve total variance before -ve values removed.

!  Calculate truncation:
   nm = 0
   totvar_inv = 1.0 / sum(eval(1:nk))
   do k = 1, nk
      cumvar = sum(eval(1:k)) * totvar_inv
      if ( nm == 0 .and. cumvar >= var_threshold ) nm = k
   end do

   write(stdout,'(a,f15.5,i6)')' Threshold, truncation = ', var_threshold, nm

   open(10, file = 'be.vertloc.dat', status = 'unknown', form='unformatted')
   write(10) nk
   eval(:) = sqrt(eval(:)) ! Write out L^{1/2} for use in WRF-Var
   write(10)eval(1:nk)
   write(10)evec(1:nk,1:nk)

   close(10)

!------------------------------------------------------------------------------------------------
! Calculate B = E L E^T = U U^T [..00100..]
!------------------------------------------------------------------------------------------------

   x(:) = 0.0
   x(ktarget) = 1.0

!  v = L^1/2 E^T x:
   do m = 1, nm
      v(m) = eval(m) * sum(evec(1:nk,m) * x(1:nk))
   end do

!  x = E L^1/2 v:
   do k = 1, nk
      x(k) = sum(evec(k,1:nm) * eval(1:nm) * v(1:nm))
!      write(22,'(i5,2f15.5)')k, rho(ktarget,k), x(k)
   end do

end program gen_be_vertloc

