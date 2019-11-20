subroutine rfft2i ( l, m, wsave, lensav, ier )

!*****************************************************************************80
!
!! RFFT2I: initialization for RFFT2B and RFFT2F.
!
!  Discussion:
!
!    RFFT2I initializes real array WSAVE for use in its companion routines
!    RFFT2F and RFFT2B for computing the two-dimensional fast Fourier
!    transform of real data.  Prime factorizations of L and M, together with
!    tabulations of the trigonometric functions, are computed and stored in
!    array WSAVE.  RFFT2I must be called prior to the first call to RFFT2F
!    or RFFT2B.  Separate WSAVE arrays are required for different values of
!    L or M.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    26 March 2005
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
!    Input, integer ( kind = 4 ) L, the number of elements to be transformed
!    in the first dimension.  The transform is most efficient when L is a
!    product of small primes.
!
!    Input, integer ( kind = 4 ) M, the number of elements to be transformed
!    in the second dimension.  The transform is most efficient when M is a
!    product of small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the number of elements in the WSAVE
!    array.  LENSAV must be at least L + M + INT(LOG(REAL(L)))
!    + INT(LOG(REAL(M))) + 8.
!
!    Output, real ( kind = 4 ) WSAVE(LENSAV), containing the prime factors
!    of L and M, and also containing certain trigonometric values which
!    will be used in routines RFFT2B or RFFT2F.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) lensav

  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lwsav
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mwsav
  real ( kind = 4 ) wsave(lensav)

  ier = 0
!
!  Verify LENSAV.
!
  lwsav = l + int ( log ( real ( l, kind = 4 ) ) ) + 4
  mwsav = 2 * m + int ( log ( real ( m, kind = 4 ) ) ) + 4

  if ( lensav < lwsav + mwsav ) then
    ier = 2
    call xerfft ( 'rfft2i', 4 )
    return
  end if

  call rfftmi ( l, wsave(1), l + int(log( real ( l, kind = 4 ))) + 4, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'rfft2i', -5 )
    return
  end if

  call cfftmi ( m, wsave(l+int(log( real ( l, kind = 4 )))+5), &
    2*m+int(log( real ( m, kind = 4 )))+4, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'rfft2i', -5 )
    return
  end if

  return
end
