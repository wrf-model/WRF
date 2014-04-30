subroutine cfft2f ( ldim, l, m, c, wsave, lensav, work, lenwrk, ier )

!*****************************************************************************80
!
!! CFFT2F: complex single precision forward fast Fourier transform, 2D.
!
!  Discussion:
!
!    CFFT2F computes the two-dimensional discrete Fourier transform of
!    a complex periodic array. This transform is known as the forward
!    transform or Fourier analysis, transforming from physical to
!    spectral space. Routine CFFT2F is normalized, in that a call to
!    CFFT2F followed by a call to CFFT2B (or vice-versa) reproduces the
!    original array within roundoff error.
!
!    On 10 May 2010, this code was modified by changing the value
!    of an index into the WSAVE array.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    10 May 2010
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
!    Input, integer ( kind = 4 ) LDIM, the first dimension of the array C.
!
!    Input, integer ( kind = 4 ) L, the number of elements to be transformed
!    in the first dimension of the two-dimensional complex array C.  The value
!    of L must be less than or equal to that of LDIM.  The transform is most
!    efficient when L is a product of small primes.
!
!    Input, integer ( kind = 4 ) M, the number of elements to be transformed
!    in the second dimension of the two-dimensional complex array C.  The
!    transform is most efficient when M is a product of small primes.
!
!    Input/output, complex ( kind = 4 ) C(LDIM,M), on input, the array of two
!    dimensions containing the (L,M) subarray to be transformed.  On output, the
!    transformed data.
!
!    Input, real ( kind = 4 ) WSAVE(LENSAV). WSAVE's contents must be
!    initialized with a call to CFFT2I before the first call to routine CFFT2F
!    or CFFT2B with transform lengths L and M.  WSAVE's contents may be re-used
!    for subsequent calls to CFFT2F and CFFT2B having those same
!    transform lengths.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*(L+M) + INT(LOG(REAL(L)))
!    + INT(LOG(REAL(M))) + 8.
!
!    Workspace, real ( kind = 4 ) WORK(LENWRK).
!
!    Input, integer ( kind = 4 ) LENWRK, the dimension of the WORK array.
!    LENWRK must be at least 2*L*M.
!
!    Output, integer ( kind = 4 ) IER, error flag.
!    0, successful exit;
!    2, input parameter LENSAV not big enough;
!    3, input parameter LENWRK not big enough;
!    5, input parameter LDIM < L;
!    20, input error returned by lower level routine.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) ldim
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk

  complex ( kind = 4 ) c(ldim,m)
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) ier1
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) l
  real ( kind = 4 ) work(lenwrk)
  real ( kind = 4 ) wsave(lensav)

  ier = 0

  if ( ldim < l ) then
    ier = 5
    call xerfft ( 'CFFT2F', -2 )
    return
  end if

  if ( lensav < 2 * l + int ( log ( real ( l, kind = 4 ) ) ) + &
                2 * m + int ( log ( real ( m, kind = 4 ) ) ) + 8 ) then
    ier = 2
    call xerfft ( 'CFFT2F', 6 )
    return
  end if

  if ( lenwrk < 2 * l * m ) then
    ier = 3
    call xerfft ( 'CFFT2F', 8 )
    return
  end if
!
!  Transform the X lines of the C array.
!
!  The value of IW was modified on 10 May 2010.
!
  iw = 2 * l + int ( log ( real ( l, kind = 4 ) ) ) + 5

  call cfftmf ( l, 1, m, ldim, c, (l-1) + ldim*(m-1) +1, wsave(iw), &
    2*m + int(log( real ( m, kind = 4 ))) + 4, work, 2*l*m, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'CFFT2F', -5 )
    return
  end if
!
!  Transform the Y lines of the C array.
!
  iw = 1

  call cfftmf ( m, ldim, l, 1, c, (m-1)*ldim + l, wsave(iw), &
    2*l + int(log( real ( l, kind = 4 ))) + 4, work, 2*m*l, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'CFFT2F', -5 )
    return
  end if

  return
end
