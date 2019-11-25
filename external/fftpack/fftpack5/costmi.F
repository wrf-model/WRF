subroutine costmi ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! COSTMI: initialization for COSTMB and COSTMF.
!
!  Discussion:
!
!    COSTMI initializes array WSAVE for use in its companion routines
!    COSTMF and COSTMB.  The prime factorization of N together with a
!    tabulation of the trigonometric functions are computed and stored
!    in array WSAVE.  Separate WSAVE arrays are required for different
!    values of N.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    29 March 2005
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
!    Input, integer ( kind = 4 ) N, the length of each sequence to be
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4
!
!    Output, real ( kind = 4 ) WSAVE(LENSAV), containing the prime factors of N
!    and also containing certain trigonometric values which will be used
!    in routines COSTMB or COSTMF.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) lensav

  real ( kind = 4 ) dt
  real ( kind = 4 ) fk
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) ns2
  real ( kind = 4 ) pi
  real ( kind = 4 ) wsave(lensav)

  ier = 0

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'costmi', 3 )
    return
  end if

  if ( n <= 3 ) then
    return
  end if

  nm1 = n - 1
  np1 = n + 1
  ns2 = n / 2
  pi = 4.0E+00 * atan ( 1.0E+00 )
  dt = pi / real ( nm1, kind = 4 )

  fk = 0.0E+00
  do k = 2, ns2
    kc = np1 - k
    fk = fk + 1.0E+00
    wsave(k) = 2.0E+00 * sin ( fk * dt )
    wsave(kc) = 2.0E+00 * cos ( fk * dt )
  end do

  lnsv = nm1 + int ( log ( real ( nm1, kind = 4 ) ) ) + 4

  call rfftmi ( nm1, wsave(n+1), lnsv, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'costmi', -5 )
    return
  end if

  return
end
