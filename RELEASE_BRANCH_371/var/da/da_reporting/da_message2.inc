subroutine da_message2(strs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: strs(:)
   integer :: i

   do i=1,size(strs) 
      write(0,*) strs(i)
   end do

end subroutine da_message2


