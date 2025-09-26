subroutine rfft1b ( n, inc, r, lenr, wsave, lensav, work, lenwrk, ier )

!*****************************************************************************80
!
!! RFFT1B: real single precision backward fast Fourier transform, 1D.
!
!  Discussion:
!
!    RFFT1B computes the one-dimensional Fourier transform of a periodic
!    sequence within a real array.  This is referred to as the backward
!    transform or Fourier synthesis, transforming the sequence from
!    spectral to physical space.  This transform is normalized since a
!    call to RFFT1B followed by a call to RFFT1F (or vice-versa) reproduces
!    the original array within roundoff error.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    25 March 2005
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
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations,
!    in array R, of two consecutive elements within the sequence.
!
!    Input/output, real ( kind = 4 ) R(LENR), on input, the data to be
!    transformed, and on output, the transformed data.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least INC*(N-1) + 1.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to RFFT1I before the first call to routine
!    RFFT1F or RFFT1B for a given transform length N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least N.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    1, input parameter LENR not big enough;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough.
!
  implicit none

  integer ( kind = 4 ) lenr
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) n
  real ( kind = 4 ) r(lenr)
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)

  ier = 0

  if ( lenr < inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'rfft1b ', 6 )
    return
  end if

  if ( lensav < n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'rfft1b ', 8 )
    return
  end if

  if ( lenwrk < n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RFFT1B - Fatal error!'
    write ( *, '(a)' ) '  LENWRK < N:'
    write ( *, '(a,i6)' ) '  LENWRK = ', lenwrk
    write ( *, '(a,i6)' ) '  N = ', n
    ier = 3
    call xerfft ( 'rfft1b ', 10 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  call rfftb1 ( n, inc, r, work, wsave, wsave(n+1) )

  return
end
