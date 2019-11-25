subroutine cfftmi ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! CFFTMI: initialization for CFFTMB and CFFTMF.
!
!  Discussion:
!
!    CFFTMI initializes array WSAVE for use in its companion routines
!    CFFTMB and CFFTMF.  CFFTMI must be called before the first call
!    to CFFTMB or CFFTMF, and after whenever the value of integer N changes.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    24 March 2005
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
!    Input, integer ( kind = 4 ) N, the length of each sequence to be
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Output, real ( kind = 4 ) WSAVE(LENSAV), containing the prime factors
!    of N and also containing certain trigonometric values which will be used in
!    routines CFFTMB or CFFTMF.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough.
!
  implicit none

  integer ( kind = 4 ) lensav

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) iw1
  integer ( kind = 4 ) n
  real ( kind = 4 ) wsave(lensav)

  ier = 0

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 4 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'cfftmi ', 3 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  iw1 = n + n + 1
  call r4_mcfti1 ( n, wsave, wsave(iw1), wsave(iw1+1) )

  return
end
