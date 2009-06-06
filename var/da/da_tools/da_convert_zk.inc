subroutine da_convert_zk (info)

   !-----------------------------------------------------------------------
   ! Purpose: Transfer obs. x to grid i and calculate its
   ! distance to grid i and i+1
   !-----------------------------------------------------------------------

   implicit none

   type(infa_type), intent(inout) :: info

   if (trace_use) call da_trace_entry("da_convert_zk")

   where ( (info%zk(:,info%n1:info%n2) > 0.0 .or. anal_type_verify) &
            .and. info%zk(:,info%n1:info%n2) .ne. missing_r) 


      info%k(:,info%n1:info%n2) = int ( info%zk(:,info%n1:info%n2))

      where(info%k(:,info%n1:info%n2) < kts)  info%k(:,info%n1:info%n2) = kts
      where(info%k(:,info%n1:info%n2) >= kte) info%k(:,info%n1:info%n2) = kte-1

      info%dz(:,info%n1:info%n2) = info%zk(:,info%n1:info%n2) - real(info%k(:,info%n1:info%n2))
      info%dzm(:,info%n1:info%n2)= 1.0 - info%dz(:,info%n1:info%n2)
   elsewhere 
      info%k(:,info%n1:info%n2) = 0
      info%dz(:,info%n1:info%n2) = 0.0
      info%dzm(:,info%n1:info%n2) = 0.0
   end where

   if (trace_use) call da_trace_exit("da_convert_zk")

end subroutine da_convert_zk


