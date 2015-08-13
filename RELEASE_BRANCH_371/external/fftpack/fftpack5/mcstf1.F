subroutine mcstf1 ( lot, jump, n, inc, x, wsave, dsum, work, ier )

!*****************************************************************************80
!
!! MCSTF1 is an FFTPACK5 auxiliary routine.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
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

  real ( kind = 8 ) dsum(*)
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
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) m
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) modn
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) ns2
  real ( kind = 4 ) snm1
  real ( kind = 4 ) t1
  real ( kind = 4 ) t2
  real ( kind = 4 ) tx2
  real ( kind = 4 ) work(*)
  real ( kind = 4 ) wsave(*)
  real ( kind = 4 ) x(inc,*)
  real ( kind = 4 ) x1h
  real ( kind = 4 ) x1p3
  real ( kind = 4 ) xi

  ier = 0
  nm1 = n - 1
  np1 = n + 1
  ns2 = n / 2
  lj = ( lot - 1 ) * jump + 1

  if ( n < 2 ) then
    return
  end if

  if ( n == 2 ) then

    do m = 1, lj, jump
      x1h = x(m,1) + x(m,2)
      x(m,2) = 0.5E+00 * ( x(m,1) - x(m,2) )
      x(m,1) = 0.5E+00 * x1h
    end do

    return

  end if

  if ( n == 3 ) then

    do m = 1, lj, jump
      x1p3 = x(m,1) + x(m,3)
      tx2 = x(m,2) + x(m,2)
      x(m,2) = 0.5E+00 * ( x(m,1) - x(m,3) )
      x(m,1) = 0.25E+00 * ( x1p3 + tx2 )
      x(m,3) = 0.25E+00 * ( x1p3 - tx2 )
    end do

    return
  end if

  m1 = 0
  do m = 1, lj, jump
    m1 = m1 + 1
    dsum(m1) = x(m,1) - x(m,n)
    x(m,1) = x(m,1) + x(m,n)
  end do

  do k = 2, ns2
    m1 = 0
    do m = 1, lj, jump
      m1 = m1 + 1
      kc = np1 - k
      t1 = x(m,k) + x(m,kc)
      t2 = x(m,k) - x(m,kc)
      dsum(m1) = dsum(m1) + wsave(kc) * t2
      t2 = wsave(k) * t2
      x(m,k) = t1 - t2
      x(m,kc) = t1 + t2
    end do
  end do

  modn = mod ( n, 2 )

  if ( modn /= 0 ) then
    do m = 1, lj, jump
      x(m,ns2+1) = x(m,ns2+1) + x(m,ns2+1)
    end do
  end if

  lenx = ( lot - 1 ) * jump + inc * ( nm1 - 1 )  + 1
  lnsv = nm1 + int ( log ( real ( nm1, kind = 4 ) ) ) + 4
  lnwk = lot * nm1

  call rfftmf ( lot, jump, nm1, inc, x, lenx, wsave(n+1), lnsv, work, &
    lnwk, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'mcstf1', -5 )
    return
  end if

  snm1 = 1.0E+00 / real ( nm1, kind = 4 )
  do m = 1, lot
    dsum(m) = snm1 * dsum(m)
  end do

  if ( mod ( nm1, 2 ) == 0 ) then
    do m = 1, lj, jump
      x(m,nm1) = x(m,nm1) + x(m,nm1)
    end do
  end if

  do i = 3, n, 2
    m1 = 0
    do m = 1, lj, jump
      m1 = m1 + 1
      xi = 0.5E+00 * x(m,i)
      x(m,i) = 0.5E+00 * x(m,i-1)
      x(m,i-1) = dsum(m1)
      dsum(m1) = dsum(m1) + xi
    end do
  end do

  if ( modn == 0 ) then

    m1 = 0
    do m = 1, lj, jump
      m1 = m1 + 1
      x(m,n) = dsum(m1)
    end do

  end if

  do m = 1, lj, jump
    x(m,1) = 0.5E+00 * x(m,1)
    x(m,n) = 0.5E+00 * x(m,n)
  end do

  return
end
