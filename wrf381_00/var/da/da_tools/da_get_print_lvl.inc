subroutine da_get_print_lvl(prs,ipr)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,          intent(in)  :: prs                    
   integer,       intent(out) :: ipr                    

   integer       :: k                      

   if (trace_use) call da_trace_entry("da_get_print_lvl")
   ipr = 1
   do k =2, npres_print
 ! Avoid the pressure is unavailable (ex. -8888.88) Shu-Ya Chen (Dec. 2011)
     if( (prs*.01 .ge. pptop(k-1)) .or. (prs < 0.)) exit
     ipr = k
   end do

   if (trace_use) call da_trace_exit("da_get_print_lvl")

end subroutine da_get_print_lvl


