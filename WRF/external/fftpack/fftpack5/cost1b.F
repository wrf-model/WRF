subroutine cost1b ( n, inc, x, lenx, wsave, lensav, work, lenwrk, ier )

!*****************************************************************************80
!
!! COST1B: real single precision backward cosine transform, 1D.
!
!  Discussion:
!
!    COST1B computes the one-dimensional Fourier transform of an even
!    sequence within a real array.  This transform is referred to as
!    the backward transform or Fourier synthesis, transforming the sequence
!    from spectral to physical space.
!
!    This transform is normalized since a call to COST1B followed
!    by a call to COST1F (or vice-versa) reproduces the original array
!    within roundoff error.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    28 March 2005
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
!    transformed.  The transform is most efficient when N-1 is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations,
!    in array R, of two consecutive elements within the sequence.
!
!    Input/output, real ( kind = 4 ) R(LENR), containing the sequence to
!     be transformed.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least INC*(N-1)+ 1.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to COST1I before the first call to routine COST1F
!    or COST1B for a given transform length N.  WSAVE's contents may be re-used
!    for subsequent calls to COST1F and COST1B with the same N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least N-1.
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
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)
  real ( kind = 4 ) x(inc,*)

  ier = 0

  if ( lenx < inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'cost1b', 6 )
    return
  end if

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'cost1b', 8 )
    return
  end if

  if ( lenwrk < n - 1 ) then
    ier = 3
    call xerfft ( 'cost1b', 10 )
    return
  end if

  call costb1 ( n, inc, x, wsave, work, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'cost1b', -5 )
    return
  end if

  return
end
