subroutine zfft2i ( l, m, wsave, lensav, ier )

!*****************************************************************************80
!
!! ZFFT2I: initialization for ZFFT2B and ZFFT2F.
!
!  Discussion:
!
!    ZFFT2I initializes real array WSAVE for use in its companion
!    routines ZFFT2F and ZFFT2B for computing two-dimensional fast
!    Fourier transforms of complex data.  Prime factorizations of L and M,
!    together with tabulations of the trigonometric functions, are
!    computed and stored in array WSAVE.
!
!    On 10 May 2010, this code was modified by changing the value
!    of an index into the WSAVE array.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
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
!    Input, integer ( kind = 4 ) L, the number of elements to be transformed
!    in the first dimension.  The transform is most efficient when L is a
!    product of small primes.
!
!    Input, integer ( kind = 4 ) M, the number of elements to be transformed
!    in the second dimension.  The transform is most efficient when M is a
!    product of small primes.
!
!    Input, integer ( kind = 4 ) LENSAV, the dimension of the WSAVE array.
!    LENSAV must be at least 2*(L+M) + INT(LOG(REAL(L)))
!    + INT(LOG(REAL(M))) + 8.
!
!    Output, real ( kind = 8 ) WSAVE(LENSAV), contains the prime factors of L
!    and M, and also certain trigonometric values which will be used in
!    routines ZFFT2B or ZFFT2F.
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
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  real ( kind = 8 ) wsave(lensav)

  ier = 0

  if ( lensav < 2 * l + int ( log ( real ( l, kind = 8 ) ) ) + &
                2 * m + int ( log ( real ( m, kind = 8 ) ) ) + 8 ) then
    ier = 2
    call xerfft ( 'ZFFT2I', 4 )
    return
  end if

  call zfftmi ( l, wsave(1), 2*l + int(log(real ( l, kind = 8 ))) + 4, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'ZFFT2I', -5 )
    return
  end if
!
!  On 10 May 2010, the value of IW was modified.
!
  iw = 2 * l + int ( log ( real ( l, kind = 8 ) ) ) + 5

  call zfftmi ( m, wsave(iw), 2*m + int(log(real ( m, kind = 8 ))) + 4, ier1 )

  if ( ier1 /= 0 ) then
    ier = 20
    call xerfft ( 'ZFFT2I', -5 )
    return
  end if

  return
end
