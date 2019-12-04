subroutine cosqf1 ( n, inc, x, wsave, work, ier )

!*****************************************************************************80
!
!! COSQF1 is an FFTPACK5 auxiliary routine.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    27 March 2009
!
!  Author:
!
!    Paul Swarztrauber
!    Richard Valent
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
  integer ( kind = 4 ) np2
  integer ( kind = 4 ) ns2
  real ( kind = 4 ) work(*)
  real ( kind = 4 ) wsave(*)
  real ( kind = 4 ) x(inc,*)
  real ( kind = 4 ) xim1

  ier = 0
  ns2 = ( n + 1 ) / 2
  np2 = n + 2

  do k = 2, ns2
    kc = np2 - k
    work(k)  = x(1,k) + x(1,kc)
    work(kc) = x(1,k) - x(1,kc)
  end do

  modn = mod ( n, 2 )

  if ( modn == 0 ) then
    work(ns2+1) = x(1,ns2+1) + x(1,ns2+1)
  end if

  do k = 2, ns2
    kc = np2 - k
    x(1,k)  = wsave(k-1) * work(kc) + wsave(kc-1) * work(k)
    x(1,kc) = wsave(k-1) * work(k)  - wsave(kc-1) * work(kc)
  end do

  if ( modn == 0 ) then
    x(1,ns2+1) = wsave(ns2) * work(ns2+1)
  end if

  lenx = inc * ( n - 1 ) + 1
  lnsv = n + int ( log ( real ( n, kind = 4 ) ) ) + 4
  lnwk = n

  call rfft1f ( n, inc, x, lenx, wsave(n+1), lnsv, work, lnwk, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'cosqf1', -5 )
    return
  end if

  do i = 3, n, 2
    xim1   = 0.5E+00 * ( x(1,i-1) + x(1,i) )
    x(1,i) = 0.5E+00 * ( x(1,i-1) - x(1,i) )
    x(1,i-1) = xim1
  end do

  return
end
