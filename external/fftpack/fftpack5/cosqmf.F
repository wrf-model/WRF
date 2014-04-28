subroutine cosqmf ( lot, jump, n, inc, x, lenx, wsave, lensav, work, &
  lenwrk, ier )

!*****************************************************************************80
!
!! COSQMF: real single precision forward cosine quarter wave, multiple vectors.
!
!  Discussion:
!
!    COSQMF computes the one-dimensional Fourier transform of multiple
!    sequences within a real array, where each of the sequences is a
!    cosine series with odd wave numbers.  This transform is referred to
!    as the forward transform or Fourier synthesis, transforming the
!    sequences from spectral to physical space.
!
!    This transform is normalized since a call to COSQMF followed
!    by a call to COSQMB (or vice-versa) reproduces the original
!    array within roundoff error.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    01 April 2005
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
!    Input/output, real ( kind = 4 ) R(LENR), array containing LOT sequences,
!    each having length N.  R can have any number of dimensions, but the total
!    number of locations must be at least LENR.  On input, R contains the data
!    to be transformed, and on output, the transformed data.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least (LOT-1)*JUMP + INC*(N-1)+ 1.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to COSQMI before the first call to routine COSQMF
!    or COSQMB for a given transform length N.  WSAVE's contents may be re-used
!    for subsequent calls to COSQMF and COSQMB with the same N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least LOT*N.
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
  integer ( kind = 4 ) jump
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) lj
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 4 ) ssqrt2
  real ( kind = 4 ) tsqx
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)
  real ( kind = 4 ) x(inc,*)
  logical              xercon

  ier = 0

  if ( lenx < ( lot - 1 ) * jump + inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'cosqmf', 6 )
    return
  end if

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'cosqmf', 8 )
    return
  end if

  if ( lenwrk < lot * n ) then
    ier = 3
    call xerfft ( 'cosqmf', 10 )
    return
  end if

  if ( .not. xercon ( inc, jump, n, lot ) ) then
    ier = 4
    call xerfft ( 'cosqmf', -1 )
    return
  end if

  lj = ( lot - 1 ) * jump + 1

  if ( n < 2 ) then
    return
  end if

  if ( n == 2 ) then

    ssqrt2 = 1.0E+00 / sqrt ( 2.0E+00 )

    do m = 1, lj, jump
      tsqx = ssqrt2 * x(m,2)
      x(m,2) = 0.5E+00 * x(m,1) - tsqx
      x(m,1) = 0.5E+00 * x(m,1) + tsqx
    end do

    return
  end if

  call mcsqf1 ( lot, jump, n, inc, x, wsave, work, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'cosqmf', -5 )
    return
  end if

  return
end
