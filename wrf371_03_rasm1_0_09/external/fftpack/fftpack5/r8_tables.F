subroutine r8_tables ( ido, ip, wa )

!*****************************************************************************80
!
!! R8_TABLES computes trigonometric tables, real double precision arithmetic.
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

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip

  real ( kind = 8 ) arg1
  real ( kind = 8 ) arg2
  real ( kind = 8 ) arg3
  real ( kind = 8 ) arg4
  real ( kind = 8 ) argz
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tpi
  real ( kind = 8 ) wa(ido,ip-1,2)

  tpi = 8.0D+00 * atan ( 1.0D+00 )
  argz = tpi / real ( ip, kind = 8 )
  arg1 = tpi / real ( ido * ip, kind = 8 )

  do j = 2, ip

    arg2 = real ( j - 1, kind = 8 ) * arg1

    do i = 1, ido
      arg3 = real ( i - 1, kind = 8 ) * arg2
      wa(i,j-1,1) = cos ( arg3 )
      wa(i,j-1,2) = sin ( arg3 )
    end do

    if ( 5 < ip ) then
      arg4 = real ( j - 1, kind = 8 ) * argz
      wa(1,j-1,1) = cos ( arg4 )
      wa(1,j-1,2) = sin ( arg4 )
    end if

  end do

  return
end
