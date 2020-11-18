subroutine dcostf1 ( n, inc, x, wsave, work, ier )

!*****************************************************************************80
!
!! DCOSTF1 is an FFTPACK5 auxiliary routine.
!
!
!
!  Modified:
!
!    07 February 2006
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
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) lnwk
  integer ( kind = 4 ) modn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) ns2
  real ( kind = 8 ) snm1
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) tx2
  real ( kind = 8 ) work(*)
  real ( kind = 8 ) wsave(*)
  real ( kind = 8 ) x(inc,*)
  real ( kind = 8 ) x1h
  real ( kind = 8 ) x1p3
  real ( kind = 8 ) xi

  ier = 0
  nm1 = n - 1
  np1 = n + 1
  ns2 = n / 2

  if ( n < 2 ) then
    return
  end if

  if ( n == 2 ) then
    x1h = x(1,1) + x(1,2)
    x(1,2) = 0.5D+00 * ( x(1,1) - x(1,2) )
    x(1,1) = 0.5D+00 * x1h
    return
  end if

  if ( n == 3 ) then
    x1p3 = x(1,1) + x(1,3)
    tx2 = x(1,2) + x(1,2)
    x(1,2) = 0.5D+00 * ( x(1,1) - x(1,3) )
    x(1,1) = 0.25D+00 * ( x1p3 + tx2 )
    x(1,3) = 0.25D+00 * ( x1p3 - tx2 )
    return
  end if

  dsum = x(1,1) - x(1,n)
  x(1,1) = x(1,1) + x(1,n)
  do k = 2, ns2
    kc = np1 - k
    t1 = x(1,k) + x(1,kc)
    t2 = x(1,k) - x(1,kc)
    dsum = dsum + wsave(kc) * t2
    t2 = wsave(k) * t2
    x(1,k) = t1 - t2
    x(1,kc) = t1 + t2
  end do

  modn = mod ( n, 2 )

  if ( modn /= 0 ) then
    x(1,ns2+1) = x(1,ns2+1) + x(1,ns2+1)
  end if

  lenx = inc * ( nm1 - 1 )  + 1
  lnsv = nm1 + int ( log ( real ( nm1, kind = 8 ) ) ) + 4
  lnwk = nm1

  call dfft1f ( nm1, inc, x, lenx, wsave(n+1), lnsv, work, lnwk, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'DCOSTF1', -5 )
    return
  end if

  snm1 = 1.0D+00 / real ( nm1, kind = 8 )
  dsum = snm1 * dsum

  if ( mod ( nm1, 2 ) == 0 ) then
    x(1,nm1) = x(1,nm1) + x(1,nm1)
  end if

  do i = 3, n, 2
    xi = 0.5D+00 * x(1,i)
    x(1,i) = 0.5D+00 * x(1,i-1)
    x(1,i-1) = dsum
    dsum = dsum + xi
  end do

  if ( modn == 0 ) then
    x(1,n) = dsum
  end if

  x(1,1) = 0.5D+00 * x(1,1)
  x(1,n) = 0.5D+00 * x(1,n)

  return
end
