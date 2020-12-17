subroutine cmfm1f ( lot, jump, n, inc, c, ch, wa, fnf, fac )

!*****************************************************************************80
!
!! CMFM1F is an FFTPACK5 auxiliary routine.
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

  complex ( kind = 4 ) c(*)
  real ( kind = 4 ) ch(*)
  real ( kind = 4 ) fac(*)
  real ( kind = 4 ) fnf
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) jump
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) lid
  integer ( kind = 4 ) lot
  integer ( kind = 4 ) n
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nbr
  integer ( kind = 4 ) nf
  real ( kind = 4 ) wa(*)

  nf = int ( fnf )
  na = 0
  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = int ( fac(k1) )
    l2 = ip * l1
    ido = n / l2
    lid = l1 * ido
    nbr = 1 + na + 2 * min ( ip - 2, 4 )

    if ( nbr == 1 ) then
      call cmf2kf ( lot, ido, l1, na, c, jump, inc, ch, 1, lot, wa(iw) )
    else if ( nbr == 2 ) then
      call cmf2kf ( lot, ido, l1, na, ch, 1, lot, c, jump, inc, wa(iw) )
    else if ( nbr == 3 ) then
      call cmf3kf ( lot, ido, l1, na, c, jump, inc, ch, 1, lot, wa(iw) )
    else if ( nbr == 4 ) then
      call cmf3kf ( lot, ido, l1, na, ch, 1, lot, c, jump, inc, wa(iw) )
    else if ( nbr == 5 ) then
      call cmf4kf ( lot, ido, l1, na, c, jump, inc, ch, 1, lot, wa(iw) )
    else if ( nbr == 6 ) then
      call cmf4kf ( lot, ido, l1, na, ch, 1, lot, c, jump, inc, wa(iw) )
    else if ( nbr == 7 ) then
      call cmf5kf ( lot, ido, l1, na, c, jump, inc, ch, 1, lot, wa(iw) )
    else if ( nbr == 8 ) then
      call cmf5kf ( lot, ido, l1, na, ch, 1, lot, c, jump, inc, wa(iw) )
    else if ( nbr == 9 ) then
      call cmfgkf ( lot, ido, ip, l1, lid, na, c, c, jump, inc, ch, ch, &
        1, lot, wa(iw) )
    else if ( nbr == 10 ) then
      call cmfgkf ( lot, ido, ip, l1, lid, na, ch, ch, 1, lot, c, c, &
        jump, inc, wa(iw) )
    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * ( ido + ido )

    if ( ip <= 5 ) then
      na = 1 - na
    end if

  end do

  return
end
