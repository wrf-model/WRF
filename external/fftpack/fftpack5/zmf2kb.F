subroutine zmf2kb ( lot, ido, l1, na, cc, im1, in1, ch, im2, in2, wa )

!*****************************************************************************80
!
!! ZMF2KB is an FFTPACK5 auxiliary routine.
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
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) l1

  real ( kind = 8 ) cc(2,in1,l1,ido,2)
  real ( kind = 8 ) ch(2,in2,l1,2,ido)
  real ( kind = 8 ) chold1
  real ( kind = 8 ) chold2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) im2
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m1d
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) m2s
  integer ( kind = 4 ) na
  real ( kind = 8 ) ti2
  real ( kind = 8 ) tr2
  real ( kind = 8 ) wa(ido,1,2)

  m1d = ( lot - 1 ) * im1 + 1
  m2s = 1 - im2

  if ( 1 < ido .or. na == 1 ) then

    do k = 1, l1
      m2 = m2s
      do m1 = 1, m1d, im1
        m2 = m2 + im2
        ch(1,m2,k,1,1) = cc(1,m1,k,1,1) + cc(1,m1,k,1,2)
        ch(1,m2,k,2,1) = cc(1,m1,k,1,1) - cc(1,m1,k,1,2)
        ch(2,m2,k,1,1) = cc(2,m1,k,1,1) + cc(2,m1,k,1,2)
        ch(2,m2,k,2,1) = cc(2,m1,k,1,1) - cc(2,m1,k,1,2)
      end do
    end do

    do i = 2, ido
      do k = 1, l1
        m2 = m2s
        do m1 = 1, m1d, im1
          m2 = m2 + im2
          ch(1,m2,k,1,i) = cc(1,m1,k,i,1)+cc(1,m1,k,i,2)
          tr2 = cc(1,m1,k,i,1)-cc(1,m1,k,i,2)
          ch(2,m2,k,1,i) = cc(2,m1,k,i,1)+cc(2,m1,k,i,2)
          ti2 = cc(2,m1,k,i,1)-cc(2,m1,k,i,2)

          ch(2,m2,k,2,i) = wa(i,1,1) * ti2 + wa(i,1,2) * tr2
          ch(1,m2,k,2,i) = wa(i,1,1) * tr2 - wa(i,1,2) * ti2

        end do
      end do
    end do

  else

    do k = 1, l1
      do m1 = 1, m1d, im1

        chold1         = cc(1,m1,k,1,1) + cc(1,m1,k,1,2)
        cc(1,m1,k,1,2) = cc(1,m1,k,1,1) - cc(1,m1,k,1,2)
        cc(1,m1,k,1,1) = chold1

        chold2         = cc(2,m1,k,1,1) + cc(2,m1,k,1,2)
        cc(2,m1,k,1,2) = cc(2,m1,k,1,1) - cc(2,m1,k,1,2)
        cc(2,m1,k,1,1) = chold2

      end do
    end do

  end if

  return
end
