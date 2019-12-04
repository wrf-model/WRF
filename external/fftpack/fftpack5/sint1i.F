subroutine sint1i ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! SINT1I: initialization for SINT1B and SINT1F.
!
!  Discussion:
!
!    SINT1I initializes array WSAVE for use in its companion routines
!    SINT1F and SINT1B.  The prime factorization of N together with a
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
!    30 March 2005
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
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.  The transform is most efficient when N+1 is a product
!    of small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least N/2 + N + INT(LOG(REAL(N))) + 4.
!
!    Output, real ( kind = 4 ) WSAVE(LENSAV), containing the prime factors
!    of N and also containing certain trigonometric values which will be used
!    in routines SINT1B or SINT1F.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) lensav

  real ( kind = 4 ) dt
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) n
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) ns2
  real ( kind = 4 ) pi
  real ( kind = 4 ) wsave(lensav)

  ier = 0

  if ( lensav < n / 2 + n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'SINT1I', 3 )
    return
  end if

  pi = 4.0E+00 * atan ( 1.0E+00 )

  if ( n <= 1 ) then
    return
  end if

  ns2 = n / 2
  np1 = n + 1
  dt = pi / real ( np1, kind = 4 )

  do k = 1, ns2
    wsave(k) = 2.0E+00 * sin ( real ( k, kind = 4 ) * dt )
  end do

  lnsv = np1 + int ( log ( real ( np1, kind = 4 ) ) ) + 4

  call rfft1i ( np1, wsave(ns2+1), lnsv, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'SINT1I', -5 )
    return
  end if

  return
end
