subroutine z1f5kb ( ido, l1, na, cc, in1, ch, in2, wa )

!*****************************************************************************80
!
!! Z1F5KB is an FFTPACK5 auxiliary routine.
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

  real ( kind = 8 ) cc(in1,l1,ido,5)
  real ( kind = 8 ) ch(in2,l1,5,ido)
  real ( kind = 8 ) chold1
  real ( kind = 8 ) chold2
  real ( kind = 8 ) ci2
  real ( kind = 8 ) ci3
  real ( kind = 8 ) ci4
  real ( kind = 8 ) ci5
  real ( kind = 8 ) cr2
  real ( kind = 8 ) cr3
  real ( kind = 8 ) cr4
  real ( kind = 8 ) cr5
  real ( kind = 8 ) di2
  real ( kind = 8 ) di3
  real ( kind = 8 ) di4
  real ( kind = 8 ) di5
  real ( kind = 8 ) dr2
  real ( kind = 8 ) dr3
  real ( kind = 8 ) dr4
  real ( kind = 8 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) na
  real ( kind = 8 ) ti2
  real ( kind = 8 ) ti3
  real ( kind = 8 ) ti4
  real ( kind = 8 ) ti5
  real ( kind = 8 ), parameter :: ti11 =  0.9510565162951536D+00
  real ( kind = 8 ), parameter :: ti12 =  0.5877852522924731D+00
  real ( kind = 8 ) tr2
  real ( kind = 8 ) tr3
  real ( kind = 8 ) tr4
  real ( kind = 8 ) tr5
  real ( kind = 8 ), parameter :: tr11 =  0.3090169943749474D+00
  real ( kind = 8 ), parameter :: tr12 = -0.8090169943749474D+00
  real ( kind = 8 ) wa(ido,4,2)

  if ( 1 < ido .or. na == 1 ) then

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
        ch(1,k,2,i) = wa(i,1,1)*dr2-wa(i,1,2)*di2
        ch(2,k,2,i) = wa(i,1,1)*di2+wa(i,1,2)*dr2
        ch(1,k,3,i) = wa(i,2,1)*dr3-wa(i,2,2)*di3
        ch(2,k,3,i) = wa(i,2,1)*di3+wa(i,2,2)*dr3
        ch(1,k,4,i) = wa(i,3,1)*dr4-wa(i,3,2)*di4
        ch(2,k,4,i) = wa(i,3,1)*di4+wa(i,3,2)*dr4
        ch(1,k,5,i) = wa(i,4,1)*dr5-wa(i,4,2)*di5
        ch(2,k,5,i) = wa(i,4,1)*di5+wa(i,4,2)*dr5
      end do
    end do

  else

    do k = 1, l1
      ti5 = cc(2,k,1,2)-cc(2,k,1,5)
      ti2 = cc(2,k,1,2)+cc(2,k,1,5)
      ti4 = cc(2,k,1,3)-cc(2,k,1,4)
      ti3 = cc(2,k,1,3)+cc(2,k,1,4)
      tr5 = cc(1,k,1,2)-cc(1,k,1,5)
      tr2 = cc(1,k,1,2)+cc(1,k,1,5)
      tr4 = cc(1,k,1,3)-cc(1,k,1,4)
      tr3 = cc(1,k,1,3)+cc(1,k,1,4)
      chold1 = cc(1,k,1,1)+tr2+tr3
      chold2 = cc(2,k,1,1)+ti2+ti3
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
      cc(1,k,1,2) = cr2-ci5
      cc(1,k,1,5) = cr2+ci5
      cc(2,k,1,2) = ci2+cr5
      cc(2,k,1,3) = ci3+cr4
      cc(1,k,1,3) = cr3-ci4
      cc(1,k,1,4) = cr3+ci4
      cc(2,k,1,4) = ci3-cr4
      cc(2,k,1,5) = ci2-cr5
    end do

  end if

  return
end
