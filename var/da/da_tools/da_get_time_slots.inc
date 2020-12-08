subroutine da_get_time_slots(nt,tmin,tana,tmax,time_slots,itime_ana)

   !------------------------------------------------------------------------------
   ! Purpose: Calculate time slots for FGAT option.
   !------------------------------------------------------------------------------

   implicit none

   integer,           intent (in)      :: nt    ! number of time slots
   character(len=19), intent (in)      :: tmin  ! begin of time window
   character(len=19), intent (in)      :: tana  ! analysis time
   character(len=19), intent (in)      :: tmax  ! end of time window
   real*8,           intent (out)      :: time_slots(0:nt) !
   integer,          intent (out)      :: itime_ana ! first guess index of analysis time

   integer   :: ana_yyyy,ana_mm,ana_dd,ana_hh,ana_mn,ana_ss
   integer   :: min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
   integer   :: max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   character :: s
   real      :: dt
   real*8    :: time_ana, time_tmp
   integer   :: it

   if (trace_use) call da_trace_entry("da_get_time_slots")

   read(unit=tana,fmt='(i4,5(a1,i2))') ana_yyyy,s,ana_mm,s,ana_dd,s,ana_hh,s,ana_mn,s,ana_ss
   read(unit=tmin,fmt='(i4,5(a1,i2))') min_yyyy,s,min_mm,s,min_dd,s,min_hh,s,min_mn,s,min_ss
   read(unit=tmax,fmt='(i4,5(a1,i2))') max_yyyy,s,max_mm,s,max_dd,s,max_hh,s,max_mn,s,max_ss

   if (print_detail_obs) then
      write(unit=stdout,fmt='(3X,A,I4,5(1X,I2))') 'Analysis julian time : ',ana_yyyy,ana_mm,ana_dd,ana_hh,ana_mn,ana_ss
      write(unit=stdout,fmt='(3X,A,I4,5(1X,I2))') 'Start julian time : ',min_yyyy,min_mm,min_dd,min_hh,min_mn,min_ss
      write(unit=stdout,fmt='(3X,A,I4,5(1X,I2))') 'End julian time   : ',max_yyyy,max_mm,max_dd,max_hh,max_mn,max_ss
   end if

   call da_get_julian_time(ana_yyyy,ana_mm,ana_dd,ana_hh,ana_mn,time_ana)
   call da_get_julian_time(min_yyyy,min_mm,min_dd,min_hh,min_mn,time_slots(0))
   call da_get_julian_time(max_yyyy,max_mm,max_dd,max_hh,max_mn,time_slots(nt))

   ! dt/2 between time_slots 0 and 1
   ! dt/2 between time_slots nt-1 and nt
   ! dt   between time_slots 1 and 2, 2 and 3, ..., nt-2 and nt-1
   if (nt > 1) then
      dt = (time_slots(nt)-time_slots(0))/float(nt-1)
      time_slots(1)  = time_slots(0)+dt*0.5
      do it=2,nt-1
         time_slots(it) = time_slots(it-1)+dt
      end do
   end if

   ! find the first guess index of analysis time
   ! usually it is at the beginning of the time window for 4dvar
   ! and at the center time window for 3dvar
   ! tmin i.e. time_slots(0)  corresponds to fg index 1
   ! tmax i.e. time_slots(nt) corresponds to fg index nt
   ! (time_slots(i)-time_slots(i-1))/2 corresponds to fg index i
   if ( nt == 1 .or.  abs(time_ana-time_slots(0)) < 0.1 ) then
      itime_ana = 1
   else if ( abs(time_ana-time_slots(nt)) < 0.1 ) then
      itime_ana = nt
   else
      do it = 2, nt-1
         time_tmp = time_slots(0) + (it-1) * dt
         if ( abs(time_ana-time_tmp) < 0.1 ) then
            itime_ana = it
            exit
         end if
      end do
   end if

   if (print_detail_obs) then
      write(unit=stdout,fmt='(3x,a,240f10.0)') 'Time_slots ', time_slots(0:nt)
      write (unit=stdout,fmt='(A)') " "
   end if

   if (trace_use) call da_trace_exit("da_get_time_slots")

end subroutine da_get_time_slots


