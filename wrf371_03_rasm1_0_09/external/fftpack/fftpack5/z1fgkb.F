subroutine z1fgkb ( ido, ip, l1, lid, na, cc, cc1, in1, ch, ch1, in2, wa )

!*****************************************************************************80
!
!! Z1FGKB is an FFTPACK5 auxiliary routine.
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
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) lid

  real ( kind = 8 ) cc(in1,l1,ip,ido)
  real ( kind = 8 ) cc1(in1,lid,ip)
  real ( kind = 8 ) ch(in2,l1,ido,ip)
  real ( kind = 8 ) ch1(in2,lid,ip)
  real ( kind = 8 ) chold1
  real ( kind = 8 ) chold2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idlj
  integer ( kind = 4 ) ipp2
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ki
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) na
  real ( kind = 8 ) wa(ido,ip-1,2)
  real ( kind = 8 ) wai
  real ( kind = 8 ) war

  ipp2 = ip + 2
  ipph = ( ip + 1 ) / 2
  do ki = 1, lid
    ch1(1,ki,1) = cc1(1,ki,1)
    ch1(2,ki,1) = cc1(2,ki,1)
  end do

  do j = 2, ipph
    jc = ipp2 - j
    do ki = 1, lid
      ch1(1,ki,j) =  cc1(1,ki,j) + cc1(1,ki,jc)
      ch1(1,ki,jc) = cc1(1,ki,j) - cc1(1,ki,jc)
      ch1(2,ki,j) =  cc1(2,ki,j) + cc1(2,ki,jc)
      ch1(2,ki,jc) = cc1(2,ki,j) - cc1(2,ki,jc)
    end do
  end do

  do j = 2, ipph
    do ki = 1, lid
      cc1(1,ki,1) = cc1(1,ki,1)+ch1(1,ki,j)
      cc1(2,ki,1) = cc1(2,ki,1)+ch1(2,ki,j)
    end do
  end do

  do l = 2, ipph

     lc = ipp2 - l
     do ki = 1, lid
       cc1(1,ki,l) = ch1(1,ki,1)+wa(1,l-1,1)*ch1(1,ki,2)
       cc1(1,ki,lc) = wa(1,l-1,2)*ch1(1,ki,ip)
       cc1(2,ki,l) = ch1(2,ki,1)+wa(1,l-1,1)*ch1(2,ki,2)
       cc1(2,ki,lc) = wa(1,l-1,2)*ch1(2,ki,ip)
     end do

     do j = 3, ipph
       jc = ipp2 - j
       idlj = mod ( ( l - 1 ) * ( j - 1 ), ip )
       war = wa(1,idlj,1)
       wai = wa(1,idlj,2)
       do ki = 1, lid
         cc1(1,ki,l) = cc1(1,ki,l)+war*ch1(1,ki,j)
         cc1(1,ki,lc) = cc1(1,ki,lc)+wai*ch1(1,ki,jc)
         cc1(2,ki,l) = cc1(2,ki,l)+war*ch1(2,ki,j)
         cc1(2,ki,lc) = cc1(2,ki,lc)+wai*ch1(2,ki,jc)
       end do
     end do

  end do

  if ( 1 < ido .or. na == 1 ) then

    do ki = 1, lid
      ch1(1,ki,1) = cc1(1,ki,1)
      ch1(2,ki,1) = cc1(2,ki,1)
    end do

    do j = 2, ipph
      jc = ipp2 - j
      do ki = 1, lid
        ch1(1,ki,j) = cc1(1,ki,j)-cc1(2,ki,jc)
        ch1(1,ki,jc) = cc1(1,ki,j)+cc1(2,ki,jc)
        ch1(2,ki,jc) = cc1(2,ki,j)-cc1(1,ki,jc)
        ch1(2,ki,j) = cc1(2,ki,j)+cc1(1,ki,jc)
      end do
    end do

    if ( ido == 1 ) then
      return
    end if

    do i = 1, ido
      do k = 1, l1
        cc(1,k,1,i) = ch(1,k,i,1)
        cc(2,k,1,i) = ch(2,k,i,1)
      end do
    end do

    do j = 2, ip
      do k = 1, l1
        cc(1,k,j,1) = ch(1,k,1,j)
        cc(2,k,j,1) = ch(2,k,1,j)
      end do
    end do

    do j = 2, ip
      do i = 2, ido
        do k = 1, l1
          cc(1,k,j,i) = wa(i,j-1,1)*ch(1,k,i,j) &
                       -wa(i,j-1,2)*ch(2,k,i,j)
          cc(2,k,j,i) = wa(i,j-1,1)*ch(2,k,i,j) &
                       +wa(i,j-1,2)*ch(1,k,i,j)
        end do
      end do
    end do

  else

    do j = 2, ipph
      jc = ipp2 - j
      do ki = 1, lid
        chold1 = cc1(1,ki,j)-cc1(2,ki,jc)
        chold2 = cc1(1,ki,j)+cc1(2,ki,jc)
        cc1(1,ki,j) = chold1
        cc1(2,ki,jc) = cc1(2,ki,j)-cc1(1,ki,jc)
        cc1(2,ki,j) = cc1(2,ki,j)+cc1(1,ki,jc)
        cc1(1,ki,jc) = chold2
      end do
    end do

  end if

  return
end
