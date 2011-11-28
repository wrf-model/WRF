subroutine da_asslegpol (l, m, sinlat, coslat, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: l       ! Legendre wavenumber.
   integer, intent(in)            :: m       ! Fourier wavenumber.
   real,    intent(in)            :: sinlat  ! sin(latitude).
   real,    intent(in)            :: coslat  ! cos(latitude).
   real,    intent(out)           :: alp     ! Associated Legendre Polynomial.

   integer                        :: i, loop
   real                           :: half_co
   real                           :: alp1, alp2

   half_co = 0.5 * coslat

   ! Calculate ALP:

   if (l < m) then
      alp = 0.0
   else
      alp = 1.0
      do i = m+1, 2*m
         alp = alp * real(i) * half_co
      end do
      if (mod(m,2) /= 0) then
         alp = -alp
      end if

      if (l > m) then
         alp1 = alp
         alp = real(2*m+1) * sinlat * alp1
         if (l /= m+1) then
            do loop = m+2,l
               alp2 = alp1
               alp1 = alp
               alp = (real(2*loop-1) * sinlat * alp1 - real(loop-1+m) * alp2) &
                  / real(loop-m)
            end do
         end if
      end if
   end if

end subroutine da_asslegpol


