subroutine da_convert_zk (info)

   !-----------------------------------------------------------------------
   ! Purpose: Transfer obs. x to grid i and calculate its
   ! distance to grid i and i+1
   !-----------------------------------------------------------------------

   implicit none

   type(infa_type), intent(inout) :: info
   integer :: k, n

   if (trace_use) call da_trace_entry("da_convert_zk")

   do n = info%n1, info%n2

      do k = 1, info%levels(n)

         if ( (info%zk(k,n) > 0.0 .or. anal_type_verify) &
               .and. info%zk(k,n) .ne. missing_r) then 

            info%k(k,n) = int ( info%zk(k,n))
  
            if (info%k(k,n) < kts)  info%k(k,n) = kts
            if (info%k(k,n) >= kte) info%k(k,n) = kte-1

            info%dz(k,n) = info%zk(k,n) - real(info%k(k,n))
            info%dzm(k,n)= 1.0 - info%dz(k,n)

         else
            info%k(k,n) = 0
            info%dz(k,n) = 0.0
            info%dzm(k,n) = 0.0
         endif

      enddo

   enddo

   if (trace_use) call da_trace_exit("da_convert_zk")

end subroutine da_convert_zk


