program gen_be_vertloc

   use da_gen_be, only : da_eof_decomposition

   implicit none

   integer, parameter    :: ni = 1000 
   integer, parameter    :: stdout = 6 
   character (len=3)     :: cnk                       ! vertical level 
   real*8, parameter       :: rscale = 0.1
   real*8, parameter       :: var_threshold = 0.99

   integer               :: i, k, k1, m,nk,nk2
   integer               :: nm
   integer               :: ktarget
   real*8                  :: ni1_inv
   real*8                  :: nk_inv, nk3_inv
   real*8                  :: kscale, kscale_invsq
   real*8                  :: kdist
   real*8                  :: r
   real*8                  :: totvar, totvar_inv, cumvar
   real*8,allocatable      :: rho(:,:)
   real*8,allocatable      :: e(:,:)
   real,allocatable      :: cov(:,:)
   real*8,allocatable      :: eval(:)
   real*8,allocatable      :: evec(:,:)
   real*8,allocatable      :: v(:)
   real*8,allocatable      :: x(:)
   ! variables for log-P method
   real*8,allocatable      :: znw(:)
   real*8,allocatable      :: p_w(:) ! pressure at full levels
   real*8,allocatable      :: ln_p_w(:)
   real*8,allocatable      :: dlnp(:)
   real*8                  :: p00, ptop, s_ens_v, grd
   ! if set use_logp =.true., need to edit ,s_ens_v, ptop and znw below in the code.
   logical                 :: use_logp = .false.

  cnk=""
  call getarg( 1, cnk )
  read(cnk,'(i3)')nk2
  nk=nk2-1
  write(stdout,'(a,i6)') ' vertical level = ', nk2

  if ( use_logp ) then
     allocate(znw(1:nk+1))
     allocate(p_w(1:nk+1))
     allocate(ln_p_w(1:nk+1))
     allocate(dlnp(1:nk))
     znw(:) = -99.0
     p00 = 100000.0

     ! empirical settings
     s_ens_v = 0.388

     ! HRRR settings
     !ptop = 1500.0
     !znw(1:nk+1) = (/ 1.0, 0.998, 0.994, 0.987, 0.975, 0.959, 0.939, 0.916, 0.892, 0.865, 0.835, &
     !   0.802, 0.766, 0.727, 0.685, 0.64, 0.592, 0.542, 0.497, 0.4565, 0.4205,  &
     !   0.3877, 0.3582, 0.3317, 0.3078, 0.2863, 0.267, 0.2496, 0.2329, 0.2188,  &
     !   0.2047, 0.1906, 0.1765, 0.1624, 0.1483, 0.1342, 0.1201, 0.106, 0.0919,  &
     !   0.0778, 0.0657, 0.0568, 0.0486, 0.0409, 0.0337, 0.0271, 0.0209, 0.0151, &
     !   0.0097, 0.0047, 0.0 /)

     ! CWB settings
     ptop = 2000.0
     znw(1:nk+1) = (/ 1.0, 0.9965, 0.992, 0.9865, 0.9795, 0.971, 0.961, 0.9495, 0.9365, 0.9216, &
        0.905, 0.887, 0.8675, 0.8465, 0.824, 0.7995, 0.773, 0.745, 0.715, 0.683,  &
        0.65, 0.616, 0.581, 0.546, 0.5115, 0.4775, 0.4445, 0.413, 0.3835, 0.3558, &
        0.3299, 0.3057, 0.2831, 0.262, 0.2422, 0.2235, 0.2058, 0.1892, 0.1735,    &
        0.1585, 0.1442, 0.1306, 0.1177, 0.1055, 0.094, 0.0828, 0.0719, 0.061,     &
        0.05, 0.038, 0.021, 0.0 /)

     if ( any(znw(:) < 0.0) ) then
        write(stdout,'(a,i3)') '--- ERROR: znw is not set properly for nlevel = ', nk+1
        write(stdout,'(a)')    '           Please edit var/gen_be/gen_be_vertloc.f90 and recompile, or'
        write(stdout,'(a)')    '           run var/build/gen_be_vertloc.exe with a proper nk.'
        stop
     end if

     do k = 1, nk+1
        p_w(k) = znw(k)*(p00 - ptop) + ptop
        ln_p_w(k) = log(max(p_w(k),0.0001))
     end do
     do k = 1, nk
        dlnp(k) = abs(ln_p_w(k)-ln_p_w(k+1))
     end do
  end if

  allocate(rho(1:nk,1:nk))
  allocate(e(1:ni,1:nk))
  allocate(cov(1:nk,1:nk))
  allocate(eval(1:nk))
  allocate(evec(1:nk,1:nk))
  allocate(v(1:nk))
  allocate(x(1:nk))
   ni1_inv = 1.0 / real (ni - 1)
   nk_inv = 1.0 / real (nk)
   nk3_inv = 1.0 / real (nk-3)

!  Specify probability densities:
   do k = 1, nk
      ! (a) is the default before v4.1. It suppresses increments at low levels above surface.
      ! It is not recommended and commented out in v4.1.
      ! kscale = 10.0 * real(k) * nk_inv  ! (a)
      ! (b) can still reduce near-surface correlation than that from ensembles.
      ! kscale = 3.0 + 7.0 * real(k-3) * nk3_inv  ! (b)
      ! (c) can be an OK option
      ! kscale = 10.0  ! (c)
      ! (d) is the default as of v4.1. A simple parameterization of vertical variation of localization scale
      kscale = 5.0 + real(nk)/real(k+3)  ! (d)
      if ( use_logp ) then
         grd = abs(s_ens_v)/dlnp(k)
         kscale = min(real(nk), grd)
      end if
      print*, k, kscale
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

   ktarget = nk
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
   
  deallocate(rho)
  deallocate(e)
  deallocate(cov)
  deallocate(eval)
  deallocate(evec)
  deallocate(v)
  deallocate(x)

end program gen_be_vertloc

