subroutine da_get_gausslats( nj, glats, gwgts, sinlat, coslat)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   !  Calculates nj Gaussian latitudes i.e. latitudes at which the Legendre
   !  polynomial Pn(sin(lat)) = 0.0, n=nj, m=0.
   !  The integral from -1 to +1 of f(x)*Pn(x) where f is a polynomial
   !  of degree <= 2n-1 can be calculated using
   !  0.5 * sum(GaussWgts(:)*Pn(:)*f(:)) with the values at Gaussian latitudes.
   !  See eqns 77-79 of 'The Spectral Technique' M.Jarraud and A.J.Simmons
   ! (1983 ECMWF Seminar or 1990 ECMWF Lecture Notes).
   !  The orthogonality and normalisation of the Legendre polynomials
   !  checked in this way are very accurate on the Cray, but somewhat
   !  less accurate on the HPs(32-bit arithmetic).
   !  Starting with a regular latitude grid, use Newton-Raphson interpolation
   ! (with bisection steps to add robustness)
   !  to find the zeros of the Legendre polynomial Pn(x), 0 <= x < 1,
   !  the negative roots(-1 < x < 0) are set by symmetry.
   !  ASin(x) gives the Gaussian latitudes.
   !  This gives slightly better results than finding the roots of Pn(sin(lat))
   ! (Algorithm from Numerical Recipies(Fortran version), 1989, p 258)

   integer, intent(in)            :: nj           ! Gridpoints in N-S direction.
   real,    intent(out)           :: glats(1:nj)  ! Gaussian latitudes(S->N, radians).
   real,    intent(out)           :: gwgts(1:nj)  ! Gaussian weights.
   real,    intent(out), optional :: sinlat(1:nj) ! sin(Latitude).
   real,    intent(out), optional :: coslat(1:nj) ! cos(Latitude).

   integer, parameter     :: maxiter = 100     ! Maximum number of iterations.
   integer                :: i, j, k           ! Loop counters.

   real                   :: fj, fjold         ! Pn(x) on search grid
   real                   :: xj, xjold         ! search grid
   real                   :: x1, x2            ! bounds on root
   real                   :: x                 ! iterated values of x
   real                   :: z                 ! = sqrt(1-x*x)
   real                   :: fn                ! Pn(x)
   real                   :: fn1               ! Pn-1(x)
   real                   :: dfr               ! 1/Pn'(x)
   real                   :: dx, dxold         ! step size, previous step

   if (trace_use) call da_trace_entry("da_get_gausslats")

   k =(nj + 2) / 2
   xj = 0.0
   z  = 1.0

   call da_asslegpol(nj, 0, xj, z, fj)

   if (mod(nj,2) == 1) then
      call da_asslegpol(nj-1,0,xj,z,fn1)
      glats(k) = 0.0
      gwgts(k) = 2.0 *(1.0 - xj * xj) /(real(nj) * fn1)**2
      k = k+1
   end if

   ! Search interval 0 < x <= 1 for zeros of Legendre polynomials:
   do j = 2, nj * 2
      xjold = xj
      fjold = fj

      ! Roots are approximately equally spaced in asin(x)
      xj = Sin(real(j)*Pi/real(nj*4))
      z  = sqrt(1.0-xj*xj)
      call da_asslegpol(nj, 0, xj, z, fj)

      if (fj >= 0.0 .AND. fjold < 0.0 .OR. fj <  0.0 .AND. fjold >= 0.0) then

         ! Perform simple interpolation to improve roots(find Gaussian latitudes)
         if (fjold < 0.0) then  ! Orient the search so that fn(x1) < 0
            x1 = xjold
            x2 = xj
         else
            x1 = xj
            x2 = xjold
         end if

         x = 0.5*(x1 + x2)     ! Initialise the guess for the root
         dxold = ABS(x1 - x2)  ! the step size before last
         dx    = dxold         ! and the last step
         z = sqrt(1.0-x*x)
         call da_asslegpol(nj, 0, x, z, fn)
         call da_asslegpol(nj-1,0,x,z,fn1)
         dfr =(1.0 - x * x) /(real(nj)*(fn1 - x * fn))

         do i = 1, maxiter

            ! Bisect if Newton out of range or not decreasing fast enough
            if (((x-x1)-fn*dfr)*((x-x2)-fn*dfr) > 0.0 &
               .OR. ABS(2.0*fn) > ABS(dxold/dfr)) then
               dxold = dx
               dx = 0.5 *(x1 - x2)
               x = x2 + dx
            else ! Newton-Raphson step
               dxold  = dx
               dx = fn * dfr
               x = x - dx
            end if

            if (ABS(dx) < 2.0*SPACinG(x)) exit
            z = sqrt(1.0-x*x)
            call da_asslegpol(nj,0,x,z,fn)
            call da_asslegpol(nj-1,0,x,z,fn1)
            dfr =(1.0 - x * x) /(real(nj)*(fn1 - x * fn))

            if (fn < 0.0) then   ! Maintain the bracket on the root
               x1 = x
            else
               x2 = x
            end if
         end do

         if (i >= MaxIter) then
            call da_error(__FILE__,__LINE__, &
             (/"No convergence finding Gaussian latitudes"/))
         end if

         glats(k) = ASin(x)
         z = sqrt(1.0-x*x)
         call da_asslegpol(nj-1,0,x,z,fn1)
         gwgts(k) = 2.0*(1.0 - x * x) /(real(nj) * fn1)**2
         glats(nj+1-k) = -glats(k)
         gwgts(nj+1-k) = gwgts(k)
         k=k+1
      end if
   end do

   if (k /= nj+1) then
      call da_error(__FILE__,__LINE__,(/"Not all roots found"/))
   end if

   ! Calculate sin, cosine:

   do j = 1, nj / 2
      sinlat(j) = sin(glats(j))
      coslat(j) = cos(glats(j))

      ! use symmetry for northern hemisphere:
      sinlat(nj+1-j) = -sinlat(j)
      coslat(nj+1-j) = coslat(j)
   end do

   if ((nj+1) / 2 == nj/2 + 1) then  ! Odd, then equator point:
      glats(nj/2+1) = 0.0
      sinlat(nj/2+1) = 0.0
      coslat(nj/2+1) = 1.0
   end if

   if (trace_use) call da_trace_exit("da_get_gausslats")

end subroutine da_get_gausslats


