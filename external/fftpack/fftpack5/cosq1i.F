subroutine cosq1i ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! COSQ1I: initialization for COSQ1B and COSQ1F.
!
!  Discussion:
!
!    COSQ1I initializes array WSAVE for use in its companion routines
!    COSQ1F and COSQ1B.  The prime factorization of N together with a
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
!    31 March 2005
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
!    transformed.  The transform is most efficient when N is a product
!    of small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Output, real ( kind = 4 ) WSAVE(LENSAV), containing the prime factors of N
!    and also containing certain trigonometric values which will be used
!    in routines COSQ1B or COSQ1F.
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
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) n
  real ( kind = 4 ) pih
  real ( kind = 4 ) wsave(lensav)

  ier = 0

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'cosq1i', 3 )
    return
  end if

  pih = 2.0E+00 * atan ( 1.0E+00 )
  dt = pih / real ( n, kind = 4 )
  fk = 0.0E+00

  do k = 1, n
    fk = fk + 1.0E+00
    wsave(k) = cos ( fk * dt )
  end do

  lnsv = n + int ( log ( real ( n, kind = 4 ) ) ) + 4

  call rfft1i ( n, wsave(n+1), lnsv, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'cosq1i', -5 )
    return
  end if

  return
end
