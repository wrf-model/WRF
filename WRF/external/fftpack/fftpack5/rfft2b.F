subroutine rfft2b ( ldim, l, m, r, wsave, lensav, work, lenwrk, ier )

!*****************************************************************************80
!
!! RFFT2B: real single precision backward fast Fourier transform, 2D.
!
!  Discussion:
!
!    RFFT2B computes the two-dimensional discrete Fourier transform of the
!    complex Fourier coefficients a real periodic array.  This transform is
!    known as the backward transform or Fourier synthesis, transforming from
!    spectral to physical space.  Routine RFFT2B is normalized: a call to
!    RFFT2B followed by a call to RFFT2F (or vice-versa) reproduces the
!    original array within roundoff error.
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
!    Input, integer ( kind = 4 ) LDIM, the first dimension of the 2D real
!    array R, which must be at least 2*(L/2+1).
!
!    Input, integer ( kind = 4 ) L, the number of elements to be transformed
!    in the first dimension of the two-dimensional real array R.  The value of
!    L must be less than or equal to that of LDIM.  The transform is most
!    efficient when L is a product of small primes.
!
!    Input, integer ( kind = 4 ) M, the number of elements to be transformed
!    in the second dimension of the two-dimensional real array R.  The transform
!    is most efficient when M is a product of small primes.
!
!    Input/output, real ( kind = 4 ) R(LDIM,M), the real array of two
!    dimensions.  On input, R contains the L/2+1-by-M complex subarray of
!    spectral coefficients, on output, the physical coefficients.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV).  WSAVE's contents must be
!    initialized with a call to RFFT2I before the first call to routine RFFT2F
!    or RFFT2B with lengths L and M.  WSAVE's contents may be re-used for
!    subsequent calls to RFFT2F and RFFT2B with the same transform lengths
!    L and M.
!
!    Input, integer ( kind = 4 ) LENSAV, the number of elements in the WSAVE
!    array.  LENSAV must be at least L + M + INT(LOG(REAL(L)))
!    + INT(LOG(REAL(M))) + 8.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).  WORK provides workspace, and
!    its contents need not be saved between calls to routines RFFT2B and RFFT2F.
!
!    Input, integer ( kind = 4 )  LENWRK, the number of elements in the WORK
!    array.  LENWRK must be at least LDIM*M.
!
!    Output, integer ( kind = 4 ) IER, the error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough;
!    6, input parameter LDIM < 2*(L/2+1);
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) ldim
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk
  integer ( kind = 4 ) m

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lwsav
  integer ( kind = 4 ) mwsav
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)
  real ( kind = 4 ) r(ldim,m)

  ier = 0
!
!  Verify LENSAV.
!
  lwsav = l + int ( log ( real ( l, kind = 4 ) ) ) + 4
  mwsav = 2 * m + int ( log ( real ( m, kind = 4 ) ) ) + 4

  if ( lensav < lwsav + mwsav ) then
    ier = 2
    call xerfft ( 'rfft2b', 6 )
    return
  end if
!
!  Verify LENWRK.
!
  if ( lenwrk < 2 * ( l / 2 + 1 ) * m ) then
    ier = 3
    call xerfft ( 'rfft2b', 8 )
    return
  end if
!
!  Verify LDIM is as big as L.
!
  if ( ldim < 2 * ( l / 2 + 1 ) ) then
    ier = 5
    call xerfft ( 'rfft2b', -6 )
    return
  end if
!
!  Transform second dimension of array.
!
  call cfftmb ( l/2+1, 1, m, ldim/2, r, m*ldim/2, &
    wsave(l+int(log( real ( l, kind = 4 )))+5), &
    2*m+int(log( real ( m, kind = 4 )))+4, work, &
    2*(l/2+1)*m, ier1 )

  if ( ier1 /= 0 ) then
     ier = 20
     call xerfft ( 'rfft2b', -5 )
     return
  end if
!
!  Reshuffle.
!
  do j = 1, m
    do i = 2, l
      r(i,j) = r(i+1,j)
    end do
  end do
!
!  Transform first dimension of array.
!
  call rfftmb ( m, ldim, l, 1, r, m*ldim, wsave(1), &
    l+int(log( real ( l, kind = 4 )))+4, work, 2*(l/2+1)*m, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'rfft2f', -5 )
    return
  end if

  return
end
