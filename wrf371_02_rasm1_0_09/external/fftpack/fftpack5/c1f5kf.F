subroutine c1f5kf ( ido, l1, na, cc, in1, ch, in2, wa )

!*****************************************************************************80
!
!! C1F5KF is an FFTPACK5 auxiliary routine.
!
!  License:
!
!    Licensed under the GNU General Public License (GPL).
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

  real ( kind = 4 ) cc(in1,l1,ido,5)
  real ( kind = 4 ) ch(in2,l1,5,ido)
  real ( kind = 4 ) chold1
  real ( kind = 4 ) chold2
  real ( kind = 4 ) ci2
  real ( kind = 4 ) ci3
  real ( kind = 4 ) ci4
  real ( kind = 4 ) ci5
  real ( kind = 4 ) cr2
  real ( kind = 4 ) cr3
  real ( kind = 4 ) cr4
  real ( kind = 4 ) cr5
  real ( kind = 4 ) di2
  real ( kind = 4 ) di3
  real ( kind = 4 ) di4
  real ( kind = 4 ) di5
  real ( kind = 4 ) dr2
  real ( kind = 4 ) dr3
  real ( kind = 4 ) dr4
  real ( kind = 4 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) na
  real ( kind = 4 ) sn
  real ( kind = 4 ) ti2
  real ( kind = 4 ) ti3
  real ( kind = 4 ) ti4
  real ( kind = 4 ) ti5
  real ( kind = 4 ), parameter :: ti11 = -0.9510565162951536E+00
  real ( kind = 4 ), parameter :: ti12 = -0.5877852522924731E+00
  real ( kind = 4 ) tr2
  real ( kind = 4 ) tr3
  real ( kind = 4 ) tr4
  real ( kind = 4 ) tr5
  real ( kind = 4 ), parameter :: tr11 =  0.3090169943749474E+00
  real ( kind = 4 ), parameter :: tr12 = -0.8090169943749474E+00
  real ( kind = 4 ) wa(ido,4,2)

  if ( 1 < ido ) then

    do k = 1, l1

      ti5 = cc(2,k,1,2)-cc(2,k,1,5)
      ti2 = cc(2,k,1,2)+cc(2,k,1,5)
      ti4 = cc(2,k,1,3)-cc(2,k,1,4)
      ti3 = cc(2,k,1,3)+cc(2,k,1,4)
      tr5 = cc(1,k,1,2)-cc(1,k,1,5)
      tr2 = cc(1,k,1,2)+cc(1,k,1,5)
      tr4 = cc(1,k,1,3)-cc(1,k,1,4)
      tr3 = cc(1,k,1,3)+cc(1,k,1,4)

      ch(1,k,1,1) = cc(1,k,1,1)+tr2+tr3
      ch(2,k,1,1) = cc(2,k,1,1)+ti2+ti3
      cr2 = cc(1,k,1,1)+tr11*tr2+tr12*tr3
      ci2 = cc(2,k,1,1)+tr11*ti2+tr12*ti3
      cr3 = cc(1,k,1,1)+tr12*tr2+tr11*tr3
      ci3 = cc(2,k,1,1)+tr12*ti2+tr11*ti3
      cr5 = ti11*tr5+ti12*tr4
      ci5 = ti11*ti5+ti12*ti4
      cr4 = ti12*tr5-ti11*tr4
      ci4 = ti12*ti5-ti11*ti4
      ch(1,k,2,1) = cr2-ci5
      ch(1,k,5,1) = cr2+ci5
      ch(2,k,2,1) = ci2+cr5
      ch(2,k,3,1) = ci3+cr4
      ch(1,k,3,1) = cr3-ci4
      ch(1,k,4,1) = cr3+ci4
      ch(2,k,4,1) = ci3-cr4
      ch(2,k,5,1) = ci2-cr5
    end do

    do i = 2, ido
      do k = 1, l1

        ti5 = cc(2,k,i,2)-cc(2,k,i,5)
        ti2 = cc(2,k,i,2)+cc(2,k,i,5)
        ti4 = cc(2,k,i,3)-cc(2,k,i,4)
        ti3 = cc(2,k,i,3)+cc(2,k,i,4)
        tr5 = cc(1,k,i,2)-cc(1,k,i,5)
        tr2 = cc(1,k,i,2)+cc(1,k,i,5)
        tr4 = cc(1,k,i,3)-cc(1,k,i,4)
        tr3 = cc(1,k,i,3)+cc(1,k,i,4)

        ch(1,k,1,i) = cc(1,k,i,1)+tr2+tr3
        ch(2,k,1,i) = cc(2,k,i,1)+ti2+ti3
        cr2 = cc(1,k,i,1)+tr11*tr2+tr12*tr3
        ci2 = cc(2,k,i,1)+tr11*ti2+tr12*ti3
        cr3 = cc(1,k,i,1)+tr12*tr2+tr11*tr3
        ci3 = cc(2,k,i,1)+tr12*ti2+tr11*ti3
        cr5 = ti11*tr5+ti12*tr4
        ci5 = ti11*ti5+ti12*ti4
        cr4 = ti12*tr5-ti11*tr4
        ci4 = ti12*ti5-ti11*ti4
        dr3 = cr3-ci4
        dr4 = cr3+ci4
        di3 = ci3+cr4
        di4 = ci3-cr4
        dr5 = cr2+ci5
        dr2 = cr2-ci5
        di5 = ci2-cr5
        di2 = ci2+cr5
        ch(1,k,2,i) = wa(i,1,1)*dr2+wa(i,1,2)*di2
        ch(2,k,2,i) = wa(i,1,1)*di2-wa(i,1,2)*dr2
        ch(1,k,3,i) = wa(i,2,1)*dr3+wa(i,2,2)*di3
        ch(2,k,3,i) = wa(i,2,1)*di3-wa(i,2,2)*dr3
        ch(1,k,4,i) = wa(i,3,1)*dr4+wa(i,3,2)*di4
        ch(2,k,4,i) = wa(i,3,1)*di4-wa(i,3,2)*dr4
        ch(1,k,5,i) = wa(i,4,1)*dr5+wa(i,4,2)*di5
        ch(2,k,5,i) = wa(i,4,1)*di5-wa(i,4,2)*dr5
      end do
    end do

  else if ( na == 1 ) then

    sn = 1.0E+00 / real ( 5 * l1, kind = 4 )

    do k = 1, l1

      ti5 = cc(2,k,1,2)-cc(2,k,1,5)
      ti2 = cc(2,k,1,2)+cc(2,k,1,5)
      ti4 = cc(2,k,1,3)-cc(2,k,1,4)
      ti3 = cc(2,k,1,3)+cc(2,k,1,4)
      tr5 = cc(1,k,1,2)-cc(1,k,1,5)
      tr2 = cc(1,k,1,2)+cc(1,k,1,5)
      tr4 = cc(1,k,1,3)-cc(1,k,1,4)
      tr3 = cc(1,k,1,3)+cc(1,k,1,4)

      ch(1,k,1,1) = sn*(cc(1,k,1,1)+tr2+tr3)
      ch(2,k,1,1) = sn*(cc(2,k,1,1)+ti2+ti3)

      cr2 = cc(1,k,1,1)+tr11*tr2+tr12*tr3
      ci2 = cc(2,k,1,1)+tr11*ti2+tr12*ti3
      cr3 = cc(1,k,1,1)+tr12*tr2+tr11*tr3
      ci3 = cc(2,k,1,1)+tr12*ti2+tr11*ti3
      cr5 = ti11*tr5+ti12*tr4
      ci5 = ti11*ti5+ti12*ti4
      cr4 = ti12*tr5-ti11*tr4
      ci4 = ti12*ti5-ti11*ti4

      ch(1,k,2,1) = sn*(cr2-ci5)
      ch(1,k,5,1) = sn*(cr2+ci5)
      ch(2,k,2,1) = sn*(ci2+cr5)
      ch(2,k,3,1) = sn*(ci3+cr4)
      ch(1,k,3,1) = sn*(cr3-ci4)
      ch(1,k,4,1) = sn*(cr3+ci4)
      ch(2,k,4,1) = sn*(ci3-cr4)
      ch(2,k,5,1) = sn*(ci2-cr5)

    end do

  else

    sn = 1.0E+00 / real ( 5 * l1, kind = 4 )

    do k = 1, l1

      ti5 = cc(2,k,1,2)-cc(2,k,1,5)
      ti2 = cc(2,k,1,2)+cc(2,k,1,5)
      ti4 = cc(2,k,1,3)-cc(2,k,1,4)
      ti3 = cc(2,k,1,3)+cc(2,k,1,4)
      tr5 = cc(1,k,1,2)-cc(1,k,1,5)
      tr2 = cc(1,k,1,2)+cc(1,k,1,5)
      tr4 = cc(1,k,1,3)-cc(1,k,1,4)
      tr3 = cc(1,k,1,3)+cc(1,k,1,4)

      chold1 = sn*(cc(1,k,1,1)+tr2+tr3)
      chold2 = sn*(cc(2,k,1,1)+ti2+ti3)

      cr2 = cc(1,k,1,1)+tr11*tr2+tr12*tr3
      ci2 = cc(2,k,1,1)+tr11*ti2+tr12*ti3
      cr3 = cc(1,k,1,1)+tr12*tr2+tr11*tr3
      ci3 = cc(2,k,1,1)+tr12*ti2+tr11*ti3

      cc(1,k,1,1) = chold1
      cc(2,k,1,1) = chold2

      cr5 = ti11*tr5+ti12*tr4
      ci5 = ti11*ti5+ti12*ti4
      cr4 = ti12*tr5-ti11*tr4
      ci4 = ti12*ti5-ti11*ti4

      cc(1,k,1,2) = sn*(cr2-ci5)
      cc(1,k,1,5) = sn*(cr2+ci5)
      cc(2,k,1,2) = sn*(ci2+cr5)
      cc(2,k,1,3) = sn*(ci3+cr4)
      cc(1,k,1,3) = sn*(cr3-ci4)
      cc(1,k,1,4) = sn*(cr3+ci4)
      cc(2,k,1,4) = sn*(ci3-cr4)
      cc(2,k,1,5) = sn*(ci2-cr5)

    end do

  end if

  return
end
