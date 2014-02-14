subroutine r8_mcfti1 ( n, wa, fnf, fac )

!*****************************************************************************80
!
!! R8_MCFTI1 sets up factors and tables, real double precision arithmetic.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
!
!  Modified:
!
!    27 August 2009
!
!  Author:
!
!    Original real single precision version by Paul Swarztrauber, Dick Valent.
!    Real double precision version by John Burkardt.
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
  implicit none

  real ( kind = 8 ) fac(*)
  real ( kind = 8 ) fnf
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nf
  real ( kind = 8 ) wa(*)
!
!  Get the factorization of N.
!
  call r8_factor ( n, nf, fac )
  fnf = real ( nf, kind = 8 )
  iw = 1
  l1 = 1
!
!  Set up the trigonometric tables.
!
  do k1 = 1, nf
    ip = int ( fac(k1) )
    l2 = l1 * ip
    ido = n / l2
    call r8_tables ( ido, ip, wa(iw) )
    iw = iw + ( ip - 1 ) * ( ido + ido )
    l1 = l2
  end do

  return
end
