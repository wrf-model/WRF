subroutine mrfti1 ( n, wa, fac )

!*****************************************************************************80
!
!! MRFTI1 is an FFTPACK5 auxiliary routine.
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
!    Input, integer ( kind = 4 ) N, the number for which factorization and
!    other information is needed.
!
!    Output, real ( kind = 4 ) WA(N), trigonometric information.
!
!    Output, real ( kind = 4 ) FAC(15), factorization information.  FAC(1) is
!    N, FAC(2) is NF, the number of factors, and FAC(3:NF+2) are the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) arg
  real ( kind = 8 ) argh
  real ( kind = 8 ) argld
  real ( kind = 4 ) fac(15)
  real ( kind = 4 ) fi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ib
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) ipm
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) ld
  integer ( kind = 4 ) nf
  integer ( kind = 4 ) nfm1
  integer ( kind = 4 ) nl
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) ntry
  real ( kind = 8 ) tpi
  real ( kind = 4 ) wa(n)

  nl = n
  nf = 0
  j = 0

  do while ( 1 < nl )

    j = j + 1

    if ( j == 1 ) then
      ntry = 4
    else if ( j == 2 ) then
      ntry = 2
    else if ( j == 3 ) then
      ntry = 3
    else if ( j == 4 ) then
      ntry = 5
    else
      ntry = ntry + 2
    end if

    do

      nq = nl / ntry
      nr = nl - ntry * nq

      if ( nr /= 0 ) then
        exit
      end if

      nf = nf + 1
      fac(nf+2) = real ( ntry, kind = 4 )
      nl = nq
!
!  If 2 is a factor, make sure it appears first in the list of factors.
!
      if ( ntry == 2 ) then
        if ( nf /= 1 ) then
          do i = 2, nf
            ib = nf - i + 2
            fac(ib+2) = fac(ib+1)
          end do
          fac(3) = 2.0E+00
        end if
      end if

    end do

  end do

  fac(1) = real ( n, kind = 4 )
  fac(2) = real ( nf, kind = 4 )
  tpi = 8.0D+00 * atan ( 1.0D+00 )
  argh = tpi / real ( n, kind = 4 )
  is = 0
  nfm1 = nf - 1
  l1 = 1

  do k1 = 1, nfm1
    ip = int ( fac(k1+2) )
    ld = 0
    l2 = l1 * ip
    ido = n / l2
    ipm = ip - 1
    do j = 1, ipm
      ld = ld + l1
      i = is
      argld = real ( ld, kind = 4 ) * argh
      fi = 0.0E+00
      do ii = 3, ido, 2
        i = i + 2
        fi = fi + 1.0E+00
        arg = fi * argld
        wa(i-1) = cos ( arg )
        wa(i) = sin ( arg )
      end do
      is = is + ido
    end do
    l1 = l2
  end do

  return
end
