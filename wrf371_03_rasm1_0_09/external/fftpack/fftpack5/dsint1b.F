subroutine dsint1b ( n, inc, x, lenx, wsave, lensav, work, lenwrk, ier )

!*****************************************************************************80
!
!! DSINT1B: real double precision backward sine transform, 1D.
!
!  Discussion:
!
!    DSINT1B computes the one-dimensional Fourier transform of an odd
!    sequence within a real array.  This transform is referred to as
!    the backward transform or Fourier synthesis, transforming the
!    sequence from spectral to physical space.
!
!    This transform is normalized since a call to DSINT1B followed
!    by a call to DSINT1F (or vice-versa) reproduces the original
!    array within roundoff error.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
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
!    transformed.  The transform is most efficient when N+1 is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations,
!    in array R, of two consecutive elements within the sequence.
!
!    Input/output, real ( kind = 8 ) R(LENR), on input, contains the
!    sequence to be transformed, and on output, the transformed sequence.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least INC*(N-1)+ 1.
!
!    Input, real ( kind = 8 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to DSINT1I before the first call to routine
!    SINT1F or SINT1B for a given transform length N.  WSAVE's contents
!    may be re-used for subsequent calls to DSINT1F and DSINT1B with the same N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least N/2 + N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 8 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least 2*N+2.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    1, input parameter LENR   not big enough;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) inc
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) n
  real ( kind = 8 ) work(lenwrk)
  real ( kind = 8 ) wsave(lensav)
  real ( kind = 8 ) x(inc,*)

  ier = 0

  if ( lenx < inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'DSINT1B', 6 )
    return
  end if

  if ( lensav < n / 2 + n + int ( log ( real ( n, kind = 8 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'DSINT1B', 8 )
    return
  end if

  if ( lenwrk < 2 * n + 2 ) then
    ier = 3
    call xerfft ( 'DSINT1B', 10 )
    return
  end if

  call dsintb1 ( n, inc, x, wsave, work, work(n+2), ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'DSINT1B', -5 )
    return
  end if

  return
end
