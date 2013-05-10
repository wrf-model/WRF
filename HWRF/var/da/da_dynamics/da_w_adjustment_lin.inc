subroutine da_w_adjustment_lin(xb,W_a,WZ_a)

   !---------------------------------------------------------------------------
   ! Purpose: Adjust vertical velocity increments
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb                ! first guess structure.
   real,           intent(out)   :: w_a(ims:ime,jms:jme,kms:kme)
   real,           intent(inout) :: wz_a(ims:ime,jms:jme,kms:kme)

   integer :: i,j,k
   real    :: wz_b(ims:ime,jms:jme,kms:kme)
   real    :: ebxl1,ebxl2
   real    :: ebxl19,ebxl29

   if (trace_use) call da_trace_entry("da_w_adjustment_lin")

   call da_wz_base(xb,WZ_b)

   do j=jts,jte
      do i=its,ite
         ebxl1=0.0
         ebxl19=0.0
         ebxl2=0.0
         ebxl29=0.0
         do k=kte,kts,-1
            ebxl1=ebxl1+wz_a(i,j,k)*(xb%hf(i,j,k)-xb%hf(i,j,k+1))
            ebxl19=ebxl19+wz_b(i,j,k)*(xb%hf(i,j,k)-xb%hf(i,j,k+1))
            ebxl2=ebxl2+wz_a(i,j,k)*(xb%hf(i,j,k)-xb%hf(i,j,k+1))   &
                  *sign(1.0,wz_b(i,j,k))
            ebxl29=ebxl29+abs(wz_b(i,j,k))*(xb%hf(i,j,k)-xb%hf(i,j,k+1))
         end do

         do k=kts,kte
            wz_a(i,j,k)=wz_a(i,j,k)*(1.-ebxl19/ebxl29*sign(1.0,wz_b(i,j,k)))-  &
                        ebxl1*abs(wz_b(i,j,k))/ebxl29+                        &
                        ebxl2*abs(wz_b(i,j,k))*ebxl19/ebxl29**2
         end do

      end do
   end do

   if (trace_use) call da_trace_exit("da_w_adjustment_lin")

end subroutine da_w_adjustment_lin


