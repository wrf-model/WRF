subroutine da_warning(file_str, line, warnings)

   !--------------------------------------------------------------------
   ! Purpose: Standard interface for warning messages
   !--------------------------------------------------------------------

   implicit none

   character(len=*), intent(in) :: file_str
   integer,          intent(in) :: line
   character(len=*), intent(in) :: warnings(:)
   character*256 :: line_str
   character*256 :: html_file
#if defined(DM_PARALLEL) && ! defined(STUBMPI)
   integer :: i
#endif

   if (warnings_are_fatal) then
      call da_error(file_str, line, warnings)
   else
      write(line_str,'(i6)') line

      html_file=file_str(1:LEN_trim(file_str)-4)//'.html'

#if defined(DM_PARALLEL) && ! defined(STUBMPI)
      call wrf_message( &
         '--------------------------- WARNING ---------------------------')
      ! only print file and line if line is positive
      if (line > 0) then
         if (use_html) then
            call wrf_message('WARNING FROM FILE: <A HREF="'// &
               trim(documentation_url)//'/'//trim(html_file)//'">'// &
               trim(file_str)// &
                  '</a>  LINE:  '//trim(line_str))
         else
            call wrf_message('WARNING FROM FILE:  '//trim(file_str)// &
               '  LINE:  '//trim(line_str))
         end if
      end if
      do i=1,size(warnings) 
         call wrf_message(warnings(i))
      end do
      call wrf_message( &
         '---------------------------------------------------------------')
#else
      call wrf_message2( &
         '---------------------------- WARNING --------------------------')
      ! only print file and line if line is positive
      if (line > 0) then
         if (use_html) then
            call wrf_message2('WARNING FROM FILE: <A HREF="'// &
               trim(documentation_url)//'/'//trim(html_file)//'">'// &
                  trim(file_str)//'</a>  LINE:  '//trim(line_str))
         else
            call wrf_message2('WARNING FROM FILE:  '//trim(file_str)// &
               '  LINE:  '//trim(line_str))
         end if
      end if
      call da_message(warnings)
      call wrf_message2( &
         '---------------------------------------------------------------')
#endif
   end if

end subroutine da_warning


