subroutine da_error( file_str, line, errors)

   !-----------------------------------------------------------------------
   ! Purpose: Standardised error reporting
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: file_str
   integer ,         intent(in) :: line  ! only print file and line if line > 0
   character(len=*), intent(in) :: errors(:)
   character*256 :: line_str
   character*256 :: html_file
   integer       :: i

   write(line_str,'(i6)') line

   html_file=file_str(1:LEN_trim(file_str)-4)//'.html'
   
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
   call wrf_message( &
      '---------------------------- FATAL ERROR -----------------------' )
   ! only print file and line if line is positive
   if (line > 0) then
      if (use_html) then
         call wrf_message( 'Fatal error in file: <A HREF="'// &
            trim(documentation_url)//'/'//trim(html_file)//'">'//file_str// &
            '</a>  LINE:  '//trim(line_str) )
      else
         call wrf_message( 'Fatal error in file:  '//trim(file_str)// &
            '  LINE:  '//trim(line_str) )
      end if
   end if
   do i=1,size(errors)
      call wrf_message(errors(i))
   end do
   call wrf_message( &
      '----------------------------------------------------------------' )
#else
   call wrf_message2( &
      '---------------------------- FATAL ERROR -----------------------' )
   ! only print file and line if line is positive
   if ( line > 0 ) then
      if (use_html) then
         call wrf_message( 'Fatal error in file: <A HREF="'// &
            trim(documentation_url)//'/'//trim(html_file)//'">'// &
            trim(file_str)//'</a>  LINE:  '//trim(line_str) )
      else
         call wrf_message2( 'Fatal error in file:  '//trim(file_str)// &
            '  LINE:  '//trim(line_str) )
      end if
   end if
   do i=1,size(errors)
      call wrf_message(errors(i))
   end do
   call wrf_message2( &
      '----------------------------------------------------------------' )
#endif
   call wrf_abort
end subroutine da_error


