subroutine rfftmf ( lot, jump, n, inc, r, lenr, wsave, lensav, &
  work, lenwrk, ier )

!*****************************************************************************80
!
!! RFFTMF: real single precision forward FFT, 1D, multiple vectors.
!
!  Discussion:
!
!    RFFTMF computes the one-dimensional Fourier transform of multiple
!    periodic sequences within a real array.  This transform is referred
!    to as the forward transform or Fourier analysis, transforming the
!    sequences from physical to spectral space.
!
!    This transform is normalized since a call to RFFTMF followed
!    by a call to RFFTMB (or vice-versa) reproduces the original array
!    within roundoff error.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    27 March 2005
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
!    Input, integer ( kind = 4 ) LOT, the number of sequences to be transformed
!    within array R.
!
!    Input, integer ( kind = 4 ) JUMP, the increment between the locations, in
!    array R, of the first elements of two consecutive sequences to be
!    transformed.
!
!    Input, integer ( kind = 4 ) N, the length of each sequence to be
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations,
!    in array R, of two consecutive elements within the same sequence.
!
!    Input/output, real ( kind = 4 ) R(LENR), real array containing LOT
!    sequences, each having length N.  R can have any number of dimensions, but
!    the total number of locations must be at least LENR.  On input, the
!    physical data to be transformed, on output the spectral data.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least (LOT-1)*JUMP + INC*(N-1) + 1.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to RFFTMI before the first call to routine RFFTMF
!    or RFFTMB for a given transform length N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least LOT*N.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    1, input parameter LENR not big enough;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough;
!    4, input parameters INC, JUMP, N, LOT are not consistent.
!
  implicit none

  integer ( kind = 4 ) lenr
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) jump
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) n
  real ( kind = 4 ) r(lenr)
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)
  logical              xercon

  ier = 0

  if ( lenr < ( lot - 1 ) * jump + inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'rfftmf ', 6 )
    return
  end if

  if ( lensav < n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'rfftmf ', 8 )
    return
  end if

  if ( lenwrk < lot * n ) then
    ier = 3
    call xerfft ( 'rfftmf ', 10 )
    return
  end if

  if ( .not. xercon ( inc, jump, n, lot ) ) then
    ier = 4
    call xerfft ( 'rfftmf ', -1 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  call mrftf1 ( lot, jump, n, inc, r, work, wsave, wsave(n+1) )

  return
end
