subroutine mcsqb1 ( lot, jump, n, inc, x, wsave, work, ier )

!*****************************************************************************80
!
!! MCSQB1 is an FFTPACK5 auxiliary routine.
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
  integer ( kind = 4 ) lot

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) jump
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) lj
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) lnwk
  integer ( kind = 4 ) m
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) modn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) np2
  integer ( kind = 4 ) ns2
  real ( kind = 4 ) work(lot,*)
  real ( kind = 4 ) wsave(*)
  real ( kind = 4 ) x(inc,*)
  real ( kind = 4 ) xim1

  ier = 0
  lj = ( lot - 1 ) * jump + 1
  ns2 = ( n + 1 ) / 2
  np2 = n + 2

  do i = 3, n, 2
    do m = 1, lj, jump
      xim1 = x(m,i-1) + x(m,i)
      x(m,i) = 0.5E+00 * ( x(m,i-1) - x(m,i) )
      x(m,i-1) = 0.5E+00 * xim1
    end do
  end do

  do m = 1, lj, jump
    x(m,1) = 0.5E+00 * x(m,1)
  end do

  modn = mod ( n, 2 )
  if ( modn == 0 ) then
    do m = 1, lj, jump
      x(m,n) = 0.5E+00 * x(m,n)
    end do
  end if

  lenx = ( lot - 1 ) * jump + inc * ( n - 1 )  + 1
  lnsv = n + int ( log ( real ( n, kind = 4 ) ) ) + 4
  lnwk = lot * n

  call rfftmb ( lot, jump, n, inc, x, lenx, wsave(n+1), lnsv, &
    work, lnwk, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'mcsqb1', -5 )
    return
  end if

  do k = 2, ns2
    kc = np2 - k
    m1 = 0
    do m = 1, lj, jump
      m1 = m1 + 1
      work(m1,k) = wsave(k-1) * x(m,kc) + wsave(kc-1) * x(m,k)
      work(m1,kc) = wsave(k-1) * x(m,k) - wsave(kc-1) * x(m,kc)
    end do
  end do

  if ( modn == 0 ) then
    do m = 1, lj, jump
      x(m,ns2+1) = wsave(ns2) * ( x(m,ns2+1) + x(m,ns2+1) )
    end do
  end if

  do k = 2, ns2
    kc = np2 - k
    m1 = 0
    do m = 1, lj, jump
      m1 = m1 + 1
      x(m,k) = work(m1,k) + work(m1,kc)
      x(m,kc) = work(m1,k) - work(m1,kc)
    end do
  end do

  do m = 1, lj, jump
    x(m,1) = x(m,1) + x(m,1)
  end do

  return
end
