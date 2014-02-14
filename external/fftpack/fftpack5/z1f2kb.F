subroutine z1f2kb ( ido, l1, na, cc, in1, ch, in2, wa )

!*****************************************************************************80
!
!! Z1F2KB is an FFTPACK5 auxiliary routine.
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
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) l1

  real ( kind = 8 ) cc(in1,l1,ido,2)
  real ( kind = 8 ) ch(in2,l1,2,ido)
  real ( kind = 8 ) chold1
  real ( kind = 8 ) chold2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) na
  real ( kind = 8 ) ti2
  real ( kind = 8 ) tr2
  real ( kind = 8 ) wa(ido,1,2)

  if ( 1 < ido .or. na == 1 ) then

    do k = 1, l1
      ch(1,k,1,1) = cc(1,k,1,1) + cc(1,k,1,2)
      ch(1,k,2,1) = cc(1,k,1,1) - cc(1,k,1,2)
      ch(2,k,1,1) = cc(2,k,1,1) + cc(2,k,1,2)
      ch(2,k,2,1) = cc(2,k,1,1) - cc(2,k,1,2)
    end do

    do i = 2, ido
      do k = 1, l1

        ch(1,k,1,i) = cc(1,k,i,1) + cc(1,k,i,2)
        tr2         = cc(1,k,i,1) - cc(1,k,i,2)
        ch(2,k,1,i) = cc(2,k,i,1) + cc(2,k,i,2)
        ti2         = cc(2,k,i,1) - cc(2,k,i,2)

        ch(2,k,2,i) = wa(i,1,1) * ti2 + wa(i,1,2) * tr2
        ch(1,k,2,i) = wa(i,1,1) * tr2 - wa(i,1,2) * ti2

      end do
    end do

  else

    do k = 1, l1

      chold1      = cc(1,k,1,1) + cc(1,k,1,2)
      cc(1,k,1,2) = cc(1,k,1,1) - cc(1,k,1,2)
      cc(1,k,1,1) = chold1

      chold2      = cc(2,k,1,1) + cc(2,k,1,2)
      cc(2,k,1,2) = cc(2,k,1,1) - cc(2,k,1,2)
      cc(2,k,1,1) = chold2

    end do

  end if

  return
end
