subroutine zfftmi ( n, wsave, lensav, ier )

!*****************************************************************************80
!
!! ZFFTMI: initialization for ZFFTMB and ZFFTMF.
!
!  Discussion:
!
!    ZFFTMI initializes array WSAVE for use in its companion routines
!    ZFFTMB and ZFFTMF.  ZFFTMI must be called before the first call
!    to ZFFTMB or ZFFTMF, and after whenever the value of integer N changes.
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
!    Input, integer ( kind = 4 ) N, the length of each sequence to be
!    transformed.  The transform is most efficient when N is a product of
!    small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*N + INT(LOG(REAL(N))) + 4.
!
!    Output, real ( kind = 8 ) WSAVE(LENSAV), containing the prime factors
!    of N and also containing certain trigonometric values which will be used in
!    routines ZFFTMB or ZFFTMF.
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
  real ( kind = 8 ) wsave(lensav)

  ier = 0

  if ( lensav < 2 * n + int ( log ( real ( n, kind = 8 ) ) ) + 4 ) then
    ier = 2
    call xerfft ( 'cfftmi ', 3 )
    return
  end if

  if ( n == 1 ) then
    return
  end if

  iw1 = n + n + 1
  call r8_mcfti1 ( n, wsave, wsave(iw1), wsave(iw1+1) )

  return
end
