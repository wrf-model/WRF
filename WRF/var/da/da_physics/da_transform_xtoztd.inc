subroutine da_transform_xtoztd(grid)

!------------------------------------------------------------------------
!  Purpose: to compute the Zenith Total Delay, and save it to xb%ztd.
!
!  Both of the wet and dry delay are computed based on Vedel and Huang,
!          J. Meteor. Soc., 82, 459-472, 2004.
!
!       ** Equation (3) in Vedel and Huang is wrong.
! 
!                 ported by Yong-Run Guo  05/12/2008 from wrf3dvar.
!------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout) :: grid

   integer :: i, j, k

   real    :: const, part, term1, term2, wzd, hzd, zf

   if (trace_use) call da_trace_entry("da_transform_xtoztd")

!--WEIGHTED SUM OF VERTICAL COLUMN
   do j=jts, jte
      do i=its, ite

! Wet delay:
      wzd = 0.0
      do k=kts, kte
        const  = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)) / a_ew
        part   = grid%xb%p(i,j,k)*grid%xb%q(i,j,k) / grid%xb%t(i,j,k)
        term1  = part * const * wdk1
        term2  = part * const * wdk2 / grid%xb%t(i,j,k)
        wzd    = wzd + term1 + term2
      enddo

! Hydrostatic delay (Saastamoinen 1972):
       zf = (1.0 - zdk2*cos(2.0*grid%xb%lat(i,j)*radian) - zdk3*grid%xb%terr(i,j))
      hzd = zdk1 * grid%xb%psfc(i,j) / zf

!-----To save the ZTD in cm to ztd:
      grid%xb%ztd(i,j) = (wzd + hzd) * 1.e2
    enddo
   enddo

end subroutine da_transform_xtoztd

