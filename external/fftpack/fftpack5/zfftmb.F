subroutine zfftmb ( lot, jump, n, inc, c, lenc, wsave, lensav, work, &
  lenwrk, ier )

!*****************************************************************************80
!
!! ZFFTMB: complex double precision backward FFT, 1D, multiple vectors.
!
!  Discussion:
!
!    ZFFTMB computes the one-dimensional Fourier transform of multiple
!    periodic sequences within a complex array.  This transform is referred
!    to as the backward transform or Fourier synthesis, transforming the
!    sequences from spectral to physical space.  This transform is
!    normalized since a call to ZFFTMF followed by a call to ZFFTMB (or
!    vice-versa) reproduces the original array within roundoff error.
!
!    The parameters INC, JUMP, N and LOT are consistent if equality
!    I1*INC + J1*JUMP = I2*INC + J2*JUMP for I1,I2 < N and J1,J2 < LOT
!    implies I1=I2 and J1=J2.  For multiple FFTs to execute correctly,
!    input variables INC, JUMP, N and LOT must be consistent, otherwise
!    at least one array element mistakenly is transformed more than once.
!
!
!
!  Modified:
!
!    26 Ausust 2009
!
!  Author:
!
!    Original complex single precision by Paul Swarztrauber, Richard Valent.
!    Complex double precision version by John Burkardt.
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
!    within array C.
!
!    Input, integer ( kind = 4 ) JUMP, the increment between the locations, in
!    array C, of the first elements of two consecutive sequences to
!    be transformed.
!
!    Input, integer ( kind = 4 ) N, the length of each sequence to be
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations, in
!    array C, of two consecutive elements within the same sequence to be
!    transformed.
!
!    Input/output, complex ( kind = 8 ) C(LENC), an array containing LOT
!    sequences, each having length N, to be transformed.  C can have any
!    number of dimensions, but the total number of locations must be at least
!    LENC.  On output, C contains the transformed sequences.
!
!    Input, integer ( kind = 4 ) LENC, the dimension of the C array.
!    LENC must be at least (LOT-1)*JUMP + INC*(N-1) + 1.
!
!    Input, real ( kind = 8 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to ZFFTMI before the first call to routine ZFFTMF
!    or ZFFTMB for a given transform length N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 8 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least 2*LOT*N.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit
!    1, input parameter LENC not big enough;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough;
!    4, input parameters INC, JUMP, N, LOT are not consistent.
!
  implicit none

  integer ( kind = 4 ) lenc
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk

  complex ( kind = 8 ) c(lenc)
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) iw1
  integer ( kind = 4 ) jump
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) n
  real ( kind = 8 ) work(lenwrk)
  real ( kind = 8 ) wsave(lensav)
  logical              xercon

  ier = 0

  if ( lenc < ( lot - 1 ) * jump + inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'ZFFTMB', 6 )
    return
  end if

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 8 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'ZFFTMB', 8 )
    return
  end if

  if ( lenwrk < 2 * lot * n ) then
    ier = 3
    call xerfft ( 'ZFFTMB', 10 )
    return
  end if

  if ( .not. xercon ( inc, jump, n, lot ) ) then
    ier = 4
    call xerfft ( 'ZFFTMB', -1 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  iw1 = n + n + 1

  call zmfm1b ( lot, jump, n, inc, c, work, wsave, wsave(iw1), &
    wsave(iw1+1) )

  return
end
