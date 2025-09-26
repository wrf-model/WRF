subroutine sintmf ( lot, jump, n, inc, x, lenx, wsave, lensav, &
  work, lenwrk, ier )

!*****************************************************************************80
!
!! SINTMF: real single precision forward sine transform, multiple vectors.
!
!  Discussion:
!
!    SINTMF computes the one-dimensional Fourier transform of multiple
!    odd sequences within a real array.  This transform is referred to as
!    the forward transform or Fourier analysis, transforming the sequences
!    from physical to spectral space.
!
!    This transform is normalized since a call to SINTMF followed
!    by a call to SINTMB (or vice-versa) reproduces the original
!    array within roundoff error.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    02 April 2005
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
!    Input, integer ( kind = 4 ) LOT, the number of sequences to be
!    transformed within.
!
!    Input, integer ( kind = 4 ) JUMP, the increment between the locations,
!    in array R, of the first elements of two consecutive sequences.
!
!    Input, integer ( kind = 4 ) N, the length of each sequence to be
!    transformed.  The transform is most efficient when N+1 is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations, in
!    array R, of two consecutive elements within the same sequence.
!
!    Input/output, real ( kind = 4 ) R(LENR), containing LOT sequences, each
!    having length N.  R can have any number of dimensions, but the total
!    number of locations must be at least LENR.  On input, R contains the data
!    to be transformed, and on output, the transformed data.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least (LOT-1)*JUMP + INC*(N-1)+ 1.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to SINTMI before the first call to routine SINTMF
!    or SINTMB for a given transform length N.  WSAVE's contents may be re-used
!    for subsequent calls to SINTMF and SINTMB with the same N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least N/2 + N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least LOT*(2*N+4).
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    1, input parameter LENR   not big enough;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough;
!    4, input parameters INC,JUMP,N,LOT are not consistent;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) inc
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) iw1
  integer ( kind = 4 ) iw2
  integer ( kind = 4 ) jump
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) n
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)
  real ( kind = 4 ) x(inc,*)
  logical              xercon

  ier = 0

  if ( lenx < ( lot - 1 ) * jump + inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'sintmf', 6 )
    return
  end if

  if ( lensav < n / 2 + n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'sintmf', 8 )
    return
  end if

  if ( lenwrk < lot * ( 2 * n + 4 ) ) then
    ier = 3
    call xerfft ( 'sintmf', 10 )
    return
  end if

  if ( .not. xercon ( inc, jump, n, lot ) ) then
    ier = 4
    call xerfft ( 'sintmf', -1 )
    return
  end if

  iw1 = lot + lot + 1
  iw2 = iw1 + lot * ( n + 1 )

  call msntf1 ( lot, jump, n, inc, x, wsave, work,work(iw1), work(iw2), ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'sintmf', -5 )
    return
  end if

  return
end
