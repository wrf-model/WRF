subroutine da_get_time_slots(nt,tmin,tana,tmax,time_slots)

   !------------------------------------------------------------------------------
   ! Purpose: Calculate time slots for FGAT option.
   !------------------------------------------------------------------------------

   implicit none

   integer,           intent (in)      :: nt    ! number of time slots
   character(len=19), intent (in)      :: tmin  ! begin of time window
   character(len=19), intent (in)      :: tana  ! center of time window
   character(len=19), intent (in)      :: tmax  ! end of time window
   real*8,           intent (out)      :: time_slots(0:nt) !

   integer   :: min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   integer   :: max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   character :: s
   real      :: dt
   integer   :: it

   if (trace_use) call da_trace_entry("da_get_time_slots")

   read(unit=tmin,fmt='(i4,5(a1,i2))') min_yyyy,s,min_mm,s,min_dd,s,min_hh,s,min_mn,s,min_ss
   read(unit=tmax,fmt='(i4,5(a1,i2))') max_yyyy,s,max_mm,s,max_dd,s,max_hh,s,max_mn,s,max_ss

   if (print_detail_obs) then
      write(unit=stdout,fmt='(3X,A,I4,5(1X,I2))') 'Start julian time : ',min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
      write(unit=stdout,fmt='(3X,A,I4,5(1X,I2))') 'End julian time   : ',max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   end if

   call da_get_julian_time(min_yyyy,min_mm,min_dd,min_hh,min_mn,time_slots(0))
   call da_get_julian_time(max_yyyy,max_mm,max_dd,max_hh,max_mn,time_slots(nt))

   if (nt > 1) then
      dt = (time_slots(nt)-time_slots(0))/float(nt-1)
      time_slots(1)  = time_slots(0)+dt*0.5
      do it=2,nt-1
         time_slots(it) = time_slots(it-1)+dt
      end do
   end if

   if (print_detail_obs) then
      write(unit=stdout,fmt='(3x,a,12f10.0)') 'Time_slots ', time_slots(0:nt)
      write (unit=stdout,fmt='(A)') " "
   end if

   if (trace_use) call da_trace_exit("da_get_time_slots")

end subroutine da_get_time_slots


