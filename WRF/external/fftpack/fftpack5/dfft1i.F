subroutine dfft1i ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! DFFT1I: initialization for DFFT1B and DFFT1F.
!
!  Discussion:
!
!    DFFT1I initializes array WSAVE for use in its companion routines
!    DFFT1B and DFFT1F.  The prime factorization of N together with a
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
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least N + INT(LOG(REAL(N))) + 4.
!
!    Output, real ( kind = 8 ) WSAVE(LENSAV), containing the prime factors
!    of N and also containing certain trigonometric values which will be used
!    in routines DFFT1B or DFFT1F.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough.
!
  implicit none

  integer ( kind = 4 ) lensav

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) n
  real ( kind = 8 ) wsave(lensav)

  ier = 0

  if ( lensav < n + int ( log ( real ( n, kind = 8 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'DFFT1I ', 3 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  call dffti1 ( n, wsave(1), wsave(n+1) )

  return
end
