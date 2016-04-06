subroutine r4_tables ( ido, ip, wa )

!*****************************************************************************80
!
!! R4_TABLES computes trigonometric tables, real single precision arithmetic.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    27 August 2009
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
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip

  real ( kind = 4 ) arg1
  real ( kind = 4 ) arg2
  real ( kind = 4 ) arg3
  real ( kind = 4 ) arg4
  real ( kind = 4 ) argz
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 4 ) tpi
  real ( kind = 4 ) wa(ido,ip-1,2)

  tpi = 8.0E+00 * atan ( 1.0E+00 )
  argz = tpi / real ( ip, kind = 4 )
  arg1 = tpi / real ( ido * ip, kind = 4 )

  do j = 2, ip

    arg2 = real ( j - 1, kind = 4 ) * arg1

    do i = 1, ido
      arg3 = real ( i - 1, kind = 4 ) * arg2
      wa(i,j-1,1) = cos ( arg3 )
      wa(i,j-1,2) = sin ( arg3 )
    end do

    if ( 5 < ip ) then
      arg4 = real ( j - 1, kind = 4 ) * argz
      wa(1,j-1,1) = cos ( arg4 )
      wa(1,j-1,2) = sin ( arg4 )
    end if

  end do

  return
end
