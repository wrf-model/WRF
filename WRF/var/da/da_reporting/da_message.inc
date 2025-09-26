subroutine da_message(strs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: strs(:)
   integer :: i

   do i=1,size(strs) 
      write(unit=stdout,fmt='(a)') trim(strs(i))
   end do
   write(unit=stdout,fmt=*) " "

end subroutine da_message


