subroutine r1f2kb ( ido, l1, cc, in1, ch, in2, wa1 )

!*****************************************************************************80
!
!! R1F2KB is an FFTPACK5 auxiliary routine.
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

  real ( kind = 4 ) cc(in1,ido,2,l1)
  real ( kind = 4 ) ch(in2,ido,l1,2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) idp2
  integer ( kind = 4 ) k
  real ( kind = 4 ) wa1(ido)

  do k = 1, l1
    ch(1,1,k,1) = cc(1,1,1,k) + cc(1,ido,2,k)
    ch(1,1,k,2) = cc(1,1,1,k) - cc(1,ido,2,k)
  end do

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    idp2 = ido + 2
    do k = 1, l1
      do i = 3, ido, 2
        ic = idp2 - i

        ch(1,i-1,k,1) = cc(1,i-1,1,k) + cc(1,ic-1,2,k)
        ch(1,i,k,1)   = cc(1,i,1,k)   - cc(1,ic,2,k)

        ch(1,i-1,k,2) = wa1(i-2) * ( cc(1,i-1,1,k) - cc(1,ic-1,2,k) ) &
                      - wa1(i-1) * ( cc(1,i,1,k)   + cc(1,ic,2,k) )
        ch(1,i,k,2)   = wa1(i-2) * ( cc(1,i,1,k)   + cc(1,ic,2,k) ) &
                      + wa1(i-1) * ( cc(1,i-1,1,k) - cc(1,ic-1,2,k) )

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  do k = 1, l1
    ch(1,ido,k,1) =     cc(1,ido,1,k) + cc(1,ido,1,k)
    ch(1,ido,k,2) = - ( cc(1,1,2,k)   + cc(1,1,2,k) )
  end do

  return
end
