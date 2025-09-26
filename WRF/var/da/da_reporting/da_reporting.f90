module da_reporting

   use da_control, only : stdout, use_html, documentation_url, &
      warnings_are_fatal
   ! Do not include the wrf_interfaces as it generates a cascade of
   ! code required for gen_be
   ! use da_wrf_interfaces, only : wrf_message, wrf_abort

   implicit none

   interface
      subroutine wrf_message(str)
         character(len=*), intent(in) :: str
      end subroutine wrf_message
   end interface

   interface
      subroutine wrf_abort
      end subroutine wrf_abort
   end interface 

   character(len=10000) :: message(50)

contains

#include "da_error.inc"
#include "da_warning.inc"
#include "da_message.inc"
#include "da_message2.inc"

end module da_reporting
