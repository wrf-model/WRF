SUBROUTINE DA_Transform_XToZTD_Adj( grid )

!------------------------------------------------------------------------
!  Purpose: to compute the adjoint of the Zenith Total Delay, 
!           and save it to grid%xa.
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
     wzd = grid%xa%ztd(i,j) * 1.e2
     hzd = grid%xa%ztd(i,j) * 1.e2
     grid%xa%ztd(i,j) = 0.0
!
      zf = (1.0 - zdk2*cos(2.0*grid%xb%lat(i,j)*radian) - zdk3*grid%xb%terr(i,j))
      grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + zdk1 * hzd / zf
!
      do k=kts, kte
        const  = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)) / a_ew
        part   = grid%xb%p(i,j,k)*grid%xb%q(i,j,k) / grid%xb%t(i,j,k)

        term1  = wzd
        term2  = wzd

        parta  = term2 * const * wdk2 / grid%xb%t(i,j,k)
        grid%xa%t(i,j,k) = grid%xa%t(i,j,k) - part * const * wdk2 * term2 &
                                    / (grid%xb%t(i,j,k)*grid%xb%t(i,j,k))

        parta  = parta + term1 * const * wdk1

        if ( consider_xap4ztd ) then
           grid%xa%p(i,j,k) = grid%xa%p(i,j,k) + grid%xb%q(i,j,k)*parta/grid%xb%t(i,j,k)
        end if
        grid%xa%q(i,j,k) = grid%xa%q(i,j,k) + grid%xb%p(i,j,k)*parta/grid%xb%t(i,j,k)
        grid%xa%t(i,j,k) = grid%xa%t(i,j,k) - grid%xb%p(i,j,k)*grid%xb%q(i,j,k)*parta &
                                    / (grid%xb%t(i,j,k)*grid%xb%t(i,j,k))
      enddo

    enddo
   enddo
 
END SUBROUTINE DA_Transform_XToZTD_Adj

