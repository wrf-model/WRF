subroutine sinq1f ( n, inc, x, lenx, wsave, lensav, work, lenwrk, ier )

!*****************************************************************************80
!
!! SINQ1F: real single precision forward sine quarter wave transform, 1D.
!
!  Discussion:
!
!    SINQ1F computes the one-dimensional Fourier transform of a sequence
!    which is a sine series of odd wave numbers.  This transform is
!    referred to as the forward transform or Fourier analysis, transforming
!    the sequence from physical to spectral space.
!
!    This transform is normalized since a call to SINQ1F followed
!    by a call to SINQ1B (or vice-versa) reproduces the original
!    array within roundoff error.
!
!
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
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) INC, the increment between the locations,
!    in array R, of two consecutive elements within the sequence.
!
!    Input/output, real ( kind = 4 ) R(LENR), on input, the sequence to be
!    transformed.  On output, the transformed sequence.
!
!    Input, integer ( kind = 4 ) LENR, the dimension of the R array.
!    LENR must be at least INC*(N-1)+ 1.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to SINQ1I before the first call to routine SINQ1F
!    or SINQ1B for a given transform length N.  WSAVE's contents may be re-used
!    for subsequent calls to SINQ1F and SINQ1B with the same N.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least N.
!
!    Output, integer ( kind = 4 ) IER, the error flag.
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
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns2
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)
  real ( kind = 4 ) x(inc,*)
  real ( kind = 4 ) xhold

  ier = 0

  if ( lenx < inc * ( n - 1 ) + 1 ) then
    ier = 1
    call xerfft ( 'sinq1f', 6 )
    return
  end if

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'sinq1f', 8 )
    return
  end if

  if ( lenwrk < n ) then
    ier = 3
    call xerfft ( 'sinq1f', 10 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  ns2 = n / 2

  do k = 1, ns2
    kc = n - k
    xhold = x(1,k)
    x(1,k) = x(1,kc+1)
    x(1,kc+1) = xhold
  end do

  call cosq1f ( n, inc, x, lenx, wsave, lensav, work, lenwrk, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'sinq1f', -5 )
    return
  end if

  do k = 2, n, 2
    x(1,k) = -x(1,k)
  end do

  return
end
