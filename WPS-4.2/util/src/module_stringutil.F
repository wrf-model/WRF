module stringutil

!BUG: STRSIZE should be as large as the longest string length used in WPS
   integer, parameter :: STRSIZE = 1024

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: despace
   !
   ! Purpose: Returns a string containing the path to the file specified by s.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function get_path(s)

      implicit none

      ! Arguments
      character (len=*) :: s

      ! Return value
      character (len=STRSIZE) :: get_path

      ! Local variables
      integer :: n, i

      n = len(s)

      if (n > STRSIZE) then
         write(6,*) 'ERROR: Maximum string length exceeded in get_path()'
         stop
      end if

      write(get_path,'(a)') './'
  
      do i=n,1,-1
         if (s(i:i) == '/') then
            write(get_path,'(a)') s(1:i)
            exit
         end if
      end do

   end function get_path


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: despace
   !
   ! Purpose: Remove all space and tab characters from a string, thus compressing
   !          the string to the left.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine despace(string)
   
      implicit none
   
      ! Arguments
      character (len=*), intent(inout) :: string
   
      ! Local variables
      integer :: i, j, length, iquoted
   
      length = len(string)
   
      iquoted = 0
      j = 1
      do i=1,length
         ! Check for a quote mark
         if (string(i:i) == '"' .or. string(i:i) == '''') iquoted = mod(iquoted+1,2)
   
         ! Check for non-space, non-tab character, or if we are inside quoted text
         if ((string(i:i) /= ' ' .and. string(i:i) /= achar(9)) .or. iquoted == 1) then
            string(j:j) = string(i:i)
            j = j + 1
         end if
      end do
   
      do i=j,length
         string(i:i) = ' '
      end do
   
   end subroutine despace


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: right_justify
   !
   ! Purpose: The non-space characters in s are shifted so that they end at 
   !          position n. The argument s is modified, so if the original string
   !          must be preserved, a copy should be passed to right_justify.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine right_justify(s,n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      character (len=*), intent(inout) :: s

      ! Local variables
      integer :: i, l

      l = len_trim(s)

      if (l >= n) return

      do i=l,1,-1
         s(i+n-l:i+n-l) = s(i:i)
      end do

      do i=1,n-l
         s(i:i) = ' '
      end do

   end subroutine right_justify

end module stringutil
