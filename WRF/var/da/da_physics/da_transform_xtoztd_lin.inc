SUBROUTINE DA_Transform_XToZTD_Lin( grid )

!------------------------------------------------------------------------
!  Purpose: to compute the Zenith Total Delay, and save it to grid%xa%ztd.
!
!                                Yong-Run Guo  05/20/2008
!------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   integer :: i, j, K

   real    :: const, part, parta, term1, term2, wzd, hzd, zf

!--WEIGHTED SUM OF VERTICAL COLUMN
   do j=jts, jte
   do i=its, ite

! Wet delay:
      wzd = 0.0
      do k=kts, kte
        const  = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)) / a_ew
        part   = grid%xb%p(i,j,k)*grid%xb%q(i,j,k) / grid%xb%t(i,j,k)

        if ( consider_xap4ztd ) then
           parta  = (grid%xb%q(i,j,k)*grid%xa%p(i,j,k) + grid%xb%p(i,j,k)*grid%xa%q(i,j,k) &
                      - grid%xb%p(i,j,k)*grid%xb%q(i,j,k)*grid%xa%t(i,j,k) / grid%xb%t(i,j,k)) &
                     / grid%xb%t(i,j,k)
        else
           parta  = ( grid%xb%p(i,j,k)*grid%xa%q(i,j,k) &
                      - grid%xb%p(i,j,k)*grid%xb%q(i,j,k)*grid%xa%t(i,j,k) / grid%xb%t(i,j,k)) &
                     / grid%xb%t(i,j,k)
        end if
        term1 = parta * const * wdk1
        term2 = (parta * const * wdk2                                &
                 - part * const * wdk2 * grid%xa%t(i,j,k) / grid%xb%t(i,j,k))  &
                / grid%xb%t(i,j,k)
        wzd   = wzd + term1 + term2
      enddo

! Hydrostatic delay (Saastamoinen 1972):
       zf = (1.0 - zdk2*cos(2.0*grid%xb%lat(i,j)*radian) - zdk3*grid%xb%terr(i,j))
      hzd = zdk1 * grid%xa%psfc(i,j) / zf

!-----To save the ZTD in cm to ztd:
      grid%xa%ztd(i,j) = (wzd + hzd) * 1.e2
    enddo
   enddo
 
END SUBROUTINE DA_Transform_XToZTD_Lin

