subroutine mradbg ( m, ido, ip, l1, idl1, cc, c1, c2, im1, in1, &
  ch, ch2, im2, in2, wa )

!*****************************************************************************80
!
!! MRADBG is an FFTPACK5 auxiliary routine.
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

  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) in1
  integer ( kind = 4 ) in2
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1

  real ( kind = 4 ) ai1
  real ( kind = 4 ) ai2
  real ( kind = 4 ) ar1
  real ( kind = 4 ) ar1h
  real ( kind = 4 ) ar2
  real ( kind = 4 ) ar2h
  real ( kind = 4 ) arg
  real ( kind = 4 ) c1(in1,ido,l1,ip)
  real ( kind = 4 ) c2(in1,idl1,ip)
  real ( kind = 4 ) cc(in1,ido,ip,l1)
  real ( kind = 4 ) ch(in2,ido,l1,ip)
  real ( kind = 4 ) ch2(in2,idl1,ip)
  real ( kind = 4 ) dc2
  real ( kind = 4 ) dcp
  real ( kind = 4 ) ds2
  real ( kind = 4 ) dsp
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) idij
  integer ( kind = 4 ) idp2
  integer ( kind = 4 ) ik
  integer ( kind = 4 ) im1
  integer ( kind = 4 ) im2
  integer ( kind = 4 ) ipp2
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) m
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m1d
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) m2s
  integer ( kind = 4 ) nbd
  real ( kind = 4 ) tpi
  real ( kind = 4 ) wa(ido)

  m1d = ( m - 1 ) * im1 + 1
  m2s = 1 - im2
  tpi = 2.0E+00 * 4.0E+00 * atan ( 1.0E+00 )
  arg = tpi / real ( ip, kind = 4 )
  dcp = cos ( arg )
  dsp = sin ( arg )
  idp2 = ido + 2
  nbd = ( ido - 1 ) / 2
  ipp2 = ip + 2
  ipph = ( ip + 1 ) / 2

  if ( ido < l1 ) then

    do i = 1, ido
      do k = 1, l1
        m2 = m2s
        do m1 = 1, m1d, im1
          m2 = m2 + im2
          ch(m2,i,k,1) = cc(m1,i,1,k)
        end do
      end do
    end do

  else

    do k = 1, l1
      do i = 1, ido
        m2 = m2s
        do m1 = 1, m1d, im1
          m2 = m2 + im2
          ch(m2,i,k,1) = cc(m1,i,1,k)
        end do
      end do
    end do

  end if

  do j = 2, ipph
    jc = ipp2 - j
    j2 = j + j
    do k = 1, l1
      m2 = m2s
      do m1 = 1, m1d, im1
        m2 = m2 + im2
        ch(m2,1,k,j) = cc(m1,ido,j2-2,k) + cc(m1,ido,j2-2,k)
        ch(m2,1,k,jc) = cc(m1,1,j2-1,k) + cc(m1,1,j2-1,k)
      end do
    end do
  end do

  if ( ido == 1 ) then

  else if ( nbd < l1 ) then

    do j = 2, ipph
      jc = ipp2 - j
      do i = 3, ido, 2
        ic = idp2 - i
        do k = 1, l1
          m2 = m2s
          do m1 = 1, m1d, im1
            m2 = m2 + im2
            ch(m2,i-1,k,j)  = cc(m1,i-1,2*j-1,k) + cc(m1,ic-1,2*j-2,k)
            ch(m2,i-1,k,jc) = cc(m1,i-1,2*j-1,k) - cc(m1,ic-1,2*j-2,k)
            ch(m2,i,k,j)    = cc(m1,i,2*j-1,k)   - cc(m1,ic,2*j-2,k)
            ch(m2,i,k,jc)   = cc(m1,i,2*j-1,k)   + cc(m1,ic,2*j-2,k)
          end do
        end do
      end do
    end do

  else

    do j = 2, ipph
      jc = ipp2 - j
      do k = 1, l1
        do i = 3, ido, 2
          ic = idp2 - i
          m2 = m2s
          do m1 = 1, m1d, im1
            m2 = m2 + im2
            ch(m2,i-1,k,j)  = cc(m1,i-1,2*j-1,k) + cc(m1,ic-1,2*j-2,k)
            ch(m2,i-1,k,jc) = cc(m1,i-1,2*j-1,k) - cc(m1,ic-1,2*j-2,k)
            ch(m2,i,k,j)    = cc(m1,i,2*j-1,k)   - cc(m1,ic,2*j-2,k)
            ch(m2,i,k,jc)   = cc(m1,i,2*j-1,k)   + cc(m1,ic,2*j-2,k)
          end do
        end do
      end do
    end do

  end if

  ar1 = 1.0E+00
  ai1 = 0.0E+00

  do l = 2, ipph

    lc = ipp2 - l
    ar1h = dcp * ar1 - dsp * ai1
    ai1 =  dcp * ai1 + dsp * ar1
    ar1 = ar1h

    do ik = 1, idl1
      m2 = m2s
      do m1 = 1, m1d, im1
        m2 = m2 + im2
        c2(m1,ik,l)  = ch2(m2,ik,1) + ar1 * ch2(m2,ik,2)
        c2(m1,ik,lc) =                ai1 * ch2(m2,ik,ip)
      end do
    end do

    dc2 = ar1
    ds2 = ai1
    ar2 = ar1
    ai2 = ai1

    do j = 3, ipph
      jc = ipp2 - j
      ar2h = dc2 * ar2 - ds2 * ai2
      ai2  = dc2 * ai2 + ds2 * ar2
      ar2 = ar2h
      do ik = 1, idl1
        m2 = m2s
        do m1 = 1, m1d, im1
          m2 = m2 + im2
          c2(m1,ik,l)  = c2(m1,ik,l)  + ar2 * ch2(m2,ik,j)
          c2(m1,ik,lc) = c2(m1,ik,lc) + ai2 * ch2(m2,ik,jc)
        end do
      end do
    end do

  end do

  do j = 2, ipph
    do ik = 1, idl1
      m2 = m2s
      do m1 = 1, m1d, im1
        m2 = m2 + im2
        ch2(m2,ik,1) = ch2(m2,ik,1) + ch2(m2,ik,j)
      end do
    end do
  end do

  do j = 2, ipph
    jc = ipp2 - j
    do k = 1, l1
      m2 = m2s
      do m1 = 1, m1d, im1
        m2 = m2 + im2
        ch(m2,1,k,j)  = c1(m1,1,k,j) - c1(m1,1,k,jc)
        ch(m2,1,k,jc) = c1(m1,1,k,j) + c1(m1,1,k,jc)
      end do
    end do
  end do

  if ( ido == 1 ) then

  else if ( nbd < l1 ) then

    do j = 2, ipph
      jc = ipp2 - j
      do i = 3, ido, 2
        do k = 1, l1
          m2 = m2s
          do m1 = 1, m1d, im1
            m2 = m2 + im2
            ch(m2,i-1,k,j)  = c1(m1,i-1,k,j) - c1(m1,i,k,jc)
            ch(m2,i-1,k,jc) = c1(m1,i-1,k,j) + c1(m1,i,k,jc)
            ch(m2,i,k,j)    = c1(m1,i,k,j)   + c1(m1,i-1,k,jc)
            ch(m2,i,k,jc)   = c1(m1,i,k,j)   - c1(m1,i-1,k,jc)
          end do
        end do
      end do
    end do

  else

    do j = 2, ipph
      jc = ipp2 - j
      do k = 1, l1
        do i = 3, ido, 2
          m2 = m2s
          do m1 = 1, m1d, im1
            m2 = m2 + im2
            ch(m2,i-1,k,j)  = c1(m1,i-1,k,j) - c1(m1,i,k,jc)
            ch(m2,i-1,k,jc) = c1(m1,i-1,k,j) + c1(m1,i,k,jc)
            ch(m2,i,k,j)    = c1(m1,i,k,j)   + c1(m1,i-1,k,jc)
            ch(m2,i,k,jc)   = c1(m1,i,k,j)   - c1(m1,i-1,k,jc)
          end do
        end do
      end do
    end do

  end if

  if ( ido == 1 ) then
    return
  end if

  do ik = 1, idl1
    m2 = m2s
    do m1 = 1, m1d, im1
      m2 = m2 + im2
      c2(m1,ik,1) = ch2(m2,ik,1)
    end do
  end do

  do j = 2, ip
    do k = 1, l1
      m2 = m2s
      do m1 = 1, m1d, im1
        m2 = m2 + im2
        c1(m1,1,k,j) = ch(m2,1,k,j)
      end do
    end do
  end do

  if ( l1 < nbd ) then

    is = -ido

    do j = 2, ip
      is = is + ido
      do k = 1, l1
        idij = is
        do i = 3, ido, 2
          idij = idij + 2
          m2 = m2s
          do m1 = 1, m1d, im1
            m2 = m2 + im2
            c1(m1,i-1,k,j) = wa(idij-1) * ch(m2,i-1,k,j) &
                           - wa(idij)   * ch(m2,i,k,j)
            c1(m1,i,k,j) =   wa(idij-1) * ch(m2,i,k,j) &
                           + wa(idij)   * ch(m2,i-1,k,j)
          end do
        end do
      end do
    end do

  else

    is = -ido

    do j = 2, ip
      is = is + ido
      idij = is
      do i = 3, ido, 2
        idij = idij + 2
        do k = 1, l1
          m2 = m2s
          do m1 = 1, m1d, im1
            m2 = m2 + im2
            c1(m1,i-1,k,j) = wa(idij-1) * ch(m2,i-1,k,j) &
                           - wa(idij)   * ch(m2,i,k,j)
            c1(m1,i,k,j) =   wa(idij-1) * ch(m2,i,k,j) &
                           + wa(idij)   * ch(m2,i-1,k,j)
          end do
        end do
      end do
    end do

  end if

  return
end
