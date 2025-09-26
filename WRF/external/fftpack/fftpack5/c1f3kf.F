subroutine c1f3kf ( ido, l1, na, cc, in1, ch, in2, wa )

!*****************************************************************************80
!
!! C1F3KF is an FFTPACK5 auxiliary routine.
!
!
!    Copyright (C) 1995-2004, Scientific Computing Division,
!    University Corporation for Atmospheric Research
!
!  Modified:
!
!    27 March 2009
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
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) l1

  real ( kind = 4 ) cc(in1,l1,ido,3)
  real ( kind = 4 ) ch(in2,l1,3,ido)
  real ( kind = 4 ) ci2
  real ( kind = 4 ) ci3
  real ( kind = 4 ) cr2
  real ( kind = 4 ) cr3
  real ( kind = 4 ) di2
  real ( kind = 4 ) di3
  real ( kind = 4 ) dr2
  real ( kind = 4 ) dr3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) na
  real ( kind = 4 ) sn
  real ( kind = 4 ), parameter :: taui = -0.866025403784439E+00
  real ( kind = 4 ), parameter :: taur = -0.5E+00
  real ( kind = 4 ) ti2
  real ( kind = 4 ) tr2
  real ( kind = 4 ) wa(ido,2,2)

  if ( 1 < ido ) then

    do k = 1, l1

      tr2 = cc(1,k,1,2)+cc(1,k,1,3)
      cr2 = cc(1,k,1,1)+taur*tr2
      ch(1,k,1,1) = cc(1,k,1,1)+tr2
      ti2 = cc(2,k,1,2)+cc(2,k,1,3)
      ci2 = cc(2,k,1,1)+taur*ti2
      ch(2,k,1,1) = cc(2,k,1,1)+ti2
      cr3 = taui*(cc(1,k,1,2)-cc(1,k,1,3))
      ci3 = taui*(cc(2,k,1,2)-cc(2,k,1,3))

      ch(1,k,2,1) = cr2 - ci3
      ch(1,k,3,1) = cr2 + ci3
      ch(2,k,2,1) = ci2 + cr3
      ch(2,k,3,1) = ci2 - cr3

    end do

    do i = 2, ido
      do k = 1, l1

        tr2 = cc(1,k,i,2)+cc(1,k,i,3)
        cr2 = cc(1,k,i,1)+taur*tr2
        ch(1,k,1,i) = cc(1,k,i,1)+tr2
        ti2 = cc(2,k,i,2)+cc(2,k,i,3)
        ci2 = cc(2,k,i,1)+taur*ti2
        ch(2,k,1,i) = cc(2,k,i,1)+ti2
        cr3 = taui*(cc(1,k,i,2)-cc(1,k,i,3))
        ci3 = taui*(cc(2,k,i,2)-cc(2,k,i,3))

        dr2 = cr2 - ci3
        dr3 = cr2 + ci3
        di2 = ci2 + cr3
        di3 = ci2 - cr3

        ch(2,k,2,i) = wa(i,1,1) * di2 - wa(i,1,2) * dr2
        ch(1,k,2,i) = wa(i,1,1) * dr2 + wa(i,1,2) * di2
        ch(2,k,3,i) = wa(i,2,1) * di3 - wa(i,2,2) * dr3
        ch(1,k,3,i) = wa(i,2,1) * dr3 + wa(i,2,2) * di3

       end do
    end do

  else if ( na == 1 ) then

    sn = 1.0E+00 / real ( 3 * l1, kind = 4 )

    do k = 1, l1
      tr2 = cc(1,k,1,2)+cc(1,k,1,3)
      cr2 = cc(1,k,1,1)+taur*tr2
      ch(1,k,1,1) = sn*(cc(1,k,1,1)+tr2)
      ti2 = cc(2,k,1,2)+cc(2,k,1,3)
      ci2 = cc(2,k,1,1)+taur*ti2
      ch(2,k,1,1) = sn*(cc(2,k,1,1)+ti2)
      cr3 = taui*(cc(1,k,1,2)-cc(1,k,1,3))
      ci3 = taui*(cc(2,k,1,2)-cc(2,k,1,3))

      ch(1,k,2,1) = sn*(cr2-ci3)
      ch(1,k,3,1) = sn*(cr2+ci3)
      ch(2,k,2,1) = sn*(ci2+cr3)
      ch(2,k,3,1) = sn*(ci2-cr3)

    end do

  else

    sn = 1.0E+00 / real ( 3 * l1, kind = 4 )

    do k = 1, l1

      tr2 = cc(1,k,1,2)+cc(1,k,1,3)
      cr2 = cc(1,k,1,1)+taur*tr2
      cc(1,k,1,1) = sn*(cc(1,k,1,1)+tr2)
      ti2 = cc(2,k,1,2)+cc(2,k,1,3)
      ci2 = cc(2,k,1,1)+taur*ti2
      cc(2,k,1,1) = sn*(cc(2,k,1,1)+ti2)
      cr3 = taui*(cc(1,k,1,2)-cc(1,k,1,3))
      ci3 = taui*(cc(2,k,1,2)-cc(2,k,1,3))

      cc(1,k,1,2) = sn*(cr2-ci3)
      cc(1,k,1,3) = sn*(cr2+ci3)
      cc(2,k,1,2) = sn*(ci2+cr3)
      cc(2,k,1,3) = sn*(ci2-cr3)

    end do

  end if

  return
end
