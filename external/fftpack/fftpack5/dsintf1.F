subroutine dsintf1 ( n, inc, x, wsave, xh, work, ier )

!*****************************************************************************80
!
!! DSINTF1 is an FFTPACK5 auxiliary routine.
!
!
!
!  Modified:
!
!    27 March 2009
!
!  Author:
!
!    Original real single precision by Paul Swarztrauber, Richard Valent.
!    Real double precision version by John Burkardt.
!
!  Reference:
!
!    Paul Swarztrauber,
!    Vectorizing the Fast Fourier Transforms,
!    in Parallel Computations,
!    edited by G. Rodrigue,
!    Academic Press, 1982.
!
!    Paul Swarztrauber,
!    Fast Fourier Transform Algorithms for Vector Computers,
!    Parallel Computing, pages 45-63, 1984.
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) inc

  real ( kind = 8 ) dsum
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) lnwk
  integer ( kind = 4 ) lnxh
  integer ( kind = 4 ) modn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) ns2
  real ( kind = 8 ) sfnp1
  real ( kind = 8 ) ssqrt3
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) work(*)
  real ( kind = 8 ) wsave(*)
  real ( kind = 8 ) x(inc,*)
  real ( kind = 8 ) xh(*)
  real ( kind = 8 ) xhold

  ier = 0

  if ( n < 2 ) then
    return
  end if

  if ( n == 2 ) then
    ssqrt3 = 1.0D+00 / sqrt ( 3.0D+00 )
    xhold = ssqrt3 * ( x(1,1) + x(1,2) )
    x(1,2) = ssqrt3 * ( x(1,1) - x(1,2) )
    x(1,1) = xhold
    return
  end if

  np1 = n + 1
  ns2 = n / 2

  do k = 1, ns2
    kc = np1 - k
    t1 = x(1,k) - x(1,kc)
    t2 = wsave(k) * ( x(1,k) + x(1,kc) )
    xh(k+1) = t1 + t2
    xh(kc+1) = t2 - t1
  end do

  modn = mod ( n, 2 )

  if ( modn /= 0 ) then
    xh(ns2+2) = 4.0D+00 * x(1,ns2+1)
  end if

  xh(1) = 0.0D+00
  lnxh = np1
  lnsv = np1 + int ( log ( real ( np1, kind = 8 ) ) ) + 4
  lnwk = np1

  call dfft1f ( np1, 1, xh, lnxh, wsave(ns2+1), lnsv, work, lnwk, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'DSINTF1', -5 )
    return
  end if

  if ( mod ( np1, 2 ) == 0 ) then
    xh(np1) = xh(np1) + xh(np1)
  end if

  sfnp1 = 1.0D+00 / real ( np1, kind = 8 )
  x(1,1) = 0.5D+00 * xh(1)
  dsum = x(1,1)

  do i = 3, n, 2
    x(1,i-1) = 0.5D+00 * xh(i)
    dsum = dsum + 0.5D+00 * xh(i-1)
    x(1,i) = dsum
  end do

  if ( modn == 0 ) then
    x(1,n) = 0.5D+00 * xh(n+1)
  end if

  return
end
