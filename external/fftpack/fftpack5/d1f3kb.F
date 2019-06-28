subroutine d1f3kb ( ido, l1, cc, in1, ch, in2, wa1, wa2 )

!*****************************************************************************80
!
!! D1F3KB is an FFTPACK5 auxiliary routine.
!
!
!
!  Modified:
!
!    27 March 2009
!
!  Author:
!
!    Original real single precision by Paul Swarztrauber, Richard Valent.
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
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) l1

  real ( kind = 8 ) arg
  real ( kind = 8 ) cc(in1,ido,3,l1)
  real ( kind = 8 ) ch(in2,ido,l1,3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) idp2
  integer ( kind = 4 ) k
  real ( kind = 8 ) taui
  real ( kind = 8 ) taur
  real ( kind = 8 ) wa1(ido)
  real ( kind = 8 ) wa2(ido)

  arg = 2.0D+00 * 4.0D+00 * atan ( 1.0D+00 ) / 3.0D+00
  taur = cos ( arg )
  taui = sin ( arg )

  do k = 1, l1
    ch(1,1,k,1) = cc(1,1,1,k) + 2.0D+00 * cc(1,ido,2,k)
    ch(1,1,k,2) = cc(1,1,1,k) + 2.0D+00 * taur * cc(1,ido,2,k) &
                              - 2.0D+00 * taui * cc(1,1,3,k)
    ch(1,1,k,3) = cc(1,1,1,k) + 2.0D+00 * taur * cc(1,ido,2,k) &
                              + 2.0D+00 * taui * cc(1,1,3,k)
  end do

  if ( ido == 1 ) then
    return
  end if

  idp2 = ido + 2

  do k = 1, l1
    do i = 3, ido, 2
      ic = idp2 - i
      ch(1,i-1,k,1) = cc(1,i-1,1,k)+(cc(1,i-1,3,k)+cc(1,ic-1,2,k))
      ch(1,i,k,1) = cc(1,i,1,k)+(cc(1,i,3,k)-cc(1,ic,2,k))
      ch(1,i-1,k,2) = wa1(i-2)* &
        ((cc(1,i-1,1,k)+taur*(cc(1,i-1,3,k)+cc(1,ic-1,2,k)))- &
        (taui*(cc(1,i,3,k)+cc(1,ic,2,k)))) -wa1(i-1)* &
        ((cc(1,i,1,k)+taur*(cc(1,i,3,k)-cc(1,ic,2,k)))+ &
        (taui*(cc(1,i-1,3,k)-cc(1,ic-1,2,k))))
      ch(1,i,k,2) = wa1(i-2)* &
        ((cc(1,i,1,k)+taur*(cc(1,i,3,k)-cc(1,ic,2,k)))+ &
        (taui*(cc(1,i-1,3,k)-cc(1,ic-1,2,k)))) +wa1(i-1)* &
        ((cc(1,i-1,1,k)+taur*(cc(1,i-1,3,k)+cc(1,ic-1,2,k)))- &
        (taui*(cc(1,i,3,k)+cc(1,ic,2,k))))
      ch(1,i-1,k,3) = wa2(i-2)* &
        ((cc(1,i-1,1,k)+taur*(cc(1,i-1,3,k)+cc(1,ic-1,2,k)))+ &
        (taui*(cc(1,i,3,k)+cc(1,ic,2,k)))) -wa2(i-1)* &
        ((cc(1,i,1,k)+taur*(cc(1,i,3,k)-cc(1,ic,2,k)))- &
        (taui*(cc(1,i-1,3,k)-cc(1,ic-1,2,k))))
      ch(1,i,k,3) = wa2(i-2)* &
        ((cc(1,i,1,k)+taur*(cc(1,i,3,k)-cc(1,ic,2,k)))- &
        (taui*(cc(1,i-1,3,k)-cc(1,ic-1,2,k)))) +wa2(i-1)* &
        ((cc(1,i-1,1,k)+taur*(cc(1,i-1,3,k)+cc(1,ic-1,2,k)))+ &
        (taui*(cc(1,i,3,k)+cc(1,ic,2,k))))
    end do
  end do

  return
end
