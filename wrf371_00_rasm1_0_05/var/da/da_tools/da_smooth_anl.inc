subroutine da_smooth_anl(slab,imx,jmx,kx,npass,icrsdot)

   !-----------------------------------------------------------------------
   ! Purpose: spatially smooth (usually slab) to remove high
   ! frequency waves
   !-----------------------------------------------------------------------

   implicit none
   
   real,    intent(inout) :: SLAB(:,:,:)
   integer, intent(in)    :: imx, jmx, kx
   integer, intent(in)    :: npass
   integer, intent(in)    :: icrsdot
   
   real, allocatable :: SLABNEW(:,:)
   real              :: XNU(1:2)
   integer           :: ie, je, k 
   integer           :: loop, n, i, j

   if (trace_use) call da_trace_entry("da_smooth_anl")
   
   allocate (slabnew(imx,jmx))

   ie=imx-1-icrsdot
   je=jmx-1-icrsdot
   xnu(1)=0.50
   xnu(2)=-0.52
   do k=1,kx
      do loop=1,npass*2
         n=2-mod(loop,2)
 
         ! first smooth in the imx direction
 
         do i=2,ie
            do j=2,je
               slabnew(i,j)=slab(i,j,k)+xnu(n) * &
               ((slab(i,j+1,k)+slab(i,j-1,k))*0.5-slab(i,j,k))
            end do
         end do
         do i=2,ie
            do j=2,je
               slab(i,j,k)=slabnew(i,j)
            end do
         end do
 
         ! now smooth in the jmx direction
 
         do j=2,je
            do i=2,ie
               slabnew(i,j)=slab(i,j,k)+xnu(n) * &
               ((slab(i+1,j,k)+slab(i-1,j,k))*0.5-slab(i,j,k))
            end do
         end do

         do i=2,ie
            do j=2,je
               slab(i,j,k)=slabnew(i,j)
            end do
         end do
      end do
   end do

   deallocate (slabnew)

   if (trace_use) call da_trace_exit("da_smooth_anl")

end subroutine da_smooth_anl


