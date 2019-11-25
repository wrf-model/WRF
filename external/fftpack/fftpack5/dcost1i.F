subroutine dcost1i ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! DCOST1I: initialization for DCOST1B and DCOST1F.
!
!  Discussion:
!
!    DCOST1I initializes array WSAVE for use in its companion routines
!    DCOST1F and DCOST1B.  The prime factorization of N together with a
!    tabulation of the trigonometric functions are computed and stored
!    in array WSAVE.  Separate WSAVE arrays are required for different
!    values of N.
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
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.  The transform is most efficient when N-1 is a product
!    of small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, dimension of WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Output, real ( kind = 8 ) WSAVE(LENSAV), containing the prime factors
!    of N and also containing certain trigonometric values which will be used
!    in routines DCOST1B or DCOST1F.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) lensav

  real ( kind = 8 ) dt
  real ( kind = 8 ) fk
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) lnsv
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  integer ( kind = 4 ) np1
  integer ( kind = 4 ) ns2
  real ( kind = 8 ) pi
  real ( kind = 8 ) wsave(lensav)

  ier = 0

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 8 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'DCOST1I', 3 )
    return
  end if

  if ( n <= 3 ) then
    return
  end if

  nm1 = n - 1
  np1 = n + 1
  ns2 = n / 2
  pi = 4.0E+00 * atan ( 1.0D+00 )
  dt = pi / real ( nm1, kind = 8 )
  fk = 0.0E+00
  do k = 2, ns2
    kc = np1 - k
    fk = fk + 1.0D+00
    wsave(k) = 2.0D+00 * sin ( fk * dt )
    wsave(kc) = 2.0D+00 * cos ( fk * dt )
  end do

  lnsv = nm1 + int ( log ( real ( nm1, kind = 8 ) ) ) + 4

  call dfft1i ( nm1, wsave(n+1), lnsv, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'DCOST1I', -5 )
    return
  end if

  return
end
