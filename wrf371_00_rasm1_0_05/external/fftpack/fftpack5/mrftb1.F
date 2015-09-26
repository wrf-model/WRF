subroutine mrftb1 ( m, im, n, in, c, ch, wa, fac )

!*****************************************************************************80
!
!! MRFTB1 is an FFTPACK5 auxiliary routine.
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

  integer ( kind = 4 ) in
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 4 ) c(in,*)
  real ( kind = 4 ) ch(m,*)
  real ( kind = 4 ) fac(15)
  real ( kind = 4 ) half
  real ( kind = 4 ) halfm
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) im
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) ix2
  integer ( kind = 4 ) ix3
  integer ( kind = 4 ) ix4
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) modn
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nf
  integer ( kind = 4 ) nl
  real ( kind = 4 ) wa(n)

  nf = int ( fac(2) )
  na = 0

  do k1 = 1, nf

    ip = int ( fac(k1+2) )
    na = 1 - na

    if ( 5 < ip ) then
      if ( k1 /= nf ) then
        na = 1 - na
      end if
    end if

  end do

  half = 0.5E+00
  halfm = -0.5E+00
  modn = mod ( n, 2 )
  nl = n - 2
  if ( modn /= 0 ) then
    nl = n - 1
  end if

  if ( na == 0 ) then

    do j = 2, nl, 2
      m2 = 1 - im
      do i = 1, m
        m2 = m2 + im
        c(m2,j) = half * c(m2,j)
        c(m2,j+1) = halfm * c(m2,j+1)
      end do
    end do

  else

    m2 = 1 - im

    do i = 1, m
      m2 = m2 + im
      ch(i,1) = c(m2,1)
      ch(i,n) = c(m2,n)
    end do

    do j = 2, nl, 2
      m2 = 1 - im
      do i = 1, m
        m2 = m2 + im
        ch(i,j) = half * c(m2,j)
        ch(i,j+1) = halfm * c(m2,j+1)
      end do
    end do

  end if

  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = int ( fac(k1+2) )
    l2 = ip * l1
    ido = n / l2
    idl1 = ido * l1

    if ( ip == 4 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido

      if ( na == 0 ) then
        call mradb4 ( m, ido, l1, c, im, in, ch, 1, m, wa(iw), wa(ix2), &
          wa(ix3) )
      else
        call mradb4 ( m, ido, l1, ch, 1, m, c, im, in, wa(iw), wa(ix2), &
          wa(ix3) )
      end if

      na = 1 - na

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call mradb2 ( m, ido, l1, c, im, in, ch, 1, m, wa(iw) )
      else
        call mradb2 ( m, ido, l1, ch, 1, m, c, im, in, wa(iw) )
      end if

      na = 1 - na

    else if ( ip == 3 ) then

      ix2 = iw + ido

      if ( na == 0 ) then
        call mradb3 ( m, ido, l1, c, im, in, ch, 1, m, wa(iw), wa(ix2) )
      else
        call mradb3 ( m, ido, l1, ch, 1, m, c, im, in, wa(iw), wa(ix2) )
      end if

      na = 1 - na

    else if ( ip == 5 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido
      ix4 = ix3 + ido

      if ( na == 0 ) then
        call mradb5 ( m, ido, l1, c, im, in, ch, 1, m, wa(iw), wa(ix2), &
          wa(ix3), wa(ix4) )
      else
        call mradb5 ( m, ido, l1, ch, 1, m, c, im, in, wa(iw), wa(ix2), &
          wa(ix3), wa(ix4) )
      end if

      na = 1 - na

    else

      if ( na == 0 ) then
        call mradbg ( m, ido, ip, l1, idl1, c, c, c, im, in, ch, ch, 1, &
          m, wa(iw) )
      else
        call mradbg ( m, ido, ip, l1, idl1, ch, ch, ch, 1, m, c, c, im, &
          in, wa(iw) )
      end if

      if ( ido == 1 ) then
        na = 1 - na
      end if

    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * ido

  end do

  return
end
