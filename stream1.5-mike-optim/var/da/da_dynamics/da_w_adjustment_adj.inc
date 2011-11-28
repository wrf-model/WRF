subroutine da_w_adjustment_adj(xb,WZ_a)

   !---------------------------------------------------------------------------
   ! Purpose: Adjust vertical velocity increments
   !
   ! Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb                ! First guess structure.

   real, dimension(ims:ime,jms:jme,kms:kme), intent(inout) :: WZ_a

   integer :: I,J,K

   real, dimension(ims:ime,jms:jme,kms:kme)   :: WZ_b

   real :: EBXL1,EBXL2
   real :: EBXL19,EBXL29

   if (trace_use) call da_trace_entry("da_w_adjustment_adj")

   call da_wz_base(xb,WZ_b)

   do J=jts,jte
      do I=its,ite
         EBXL19=0.0
         EBXL29=0.0

         do K=kte,kts,-1
            EBXL19=EBXL19+WZ_b(I,J,K)*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
            EBXL29=EBXL29+ABS(WZ_b(I,J,K))*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
         end do

         EBXL1=0.0
         EBXL2=0.0

         do K=kts,kte
            EBXL1=EBXL1-WZ_a(I,J,K)*ABS(WZ_b(I,J,K))/EBXL29
            EBXL2=EBXL2-WZ_a(I,J,K)*   &
                  ABS(WZ_b(I,J,K))*(-EBXL19)/EBXL29**2
            WZ_a(I,J,K)=WZ_a(I,J,K)*(1.0-EBXL19/EBXL29   &
                                   *SIGN(1.0,WZ_b(I,J,K)))
         end do

         do K=kte,kts,-1
            WZ_a(I,J,K)=WZ_a(I,J,K)+EBXL2*(xb%hf(I,J,K)-xb%hf(I,J,K+1))   &
                          *SIGN(1.0,WZ_b(I,J,K))
            WZ_a(I,J,K)=WZ_a(I,J,K)+EBXL1*(xb%hf(I,J,K)-xb%hf(I,J,K+1))
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_w_adjustment_adj")

end subroutine da_w_adjustment_adj


