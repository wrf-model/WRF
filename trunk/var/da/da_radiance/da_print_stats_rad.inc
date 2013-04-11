subroutine da_print_stats_rad( stats_unit, nchan, rad )

   !---------------------------------------------------------------------------
   !  Purpose: print out statistics of omb, oma for radiance data.
   !
   !  METHOD:  print out average, rms, minimum, maximum of iv, re
   !---------------------------------------------------------------------------

   implicit none

   integer,           intent(in)    :: stats_unit, nchan
   type (stats_rad_type), intent(in)    :: rad
   
   integer    :: k, n, nmin, nmax
   integer    :: used_nchan 

   if (trace_use) call da_trace_entry("da_print_stats_rad")

   used_nchan = 0
   do k=1, nchan                  !! loop for channels
      if(rad%ichan(k)%num > 0) used_nchan = used_nchan + 1
   end do
   write(unit=stats_unit, fmt='((a,i5))')  ' used_nchan: ', used_nchan 

   write(unit=stats_unit, fmt='(6a)') &
        ' Channel ', &
        ' num  ', &
        ' ave  ', &
        ' rms  ', &
        ' min  ', &
        ' max  '

   do k=1, nchan                  !! loop for channels
      if (rad%ichan(k)%num > 0) then
         n    = rad%ichan(k)%num
         nmin = rad%ichan(k)%minimum%n
         nmax = rad%ichan(k)%maximum%n

         write(unit=stats_unit, fmt='((i3,i7,4f8.2))') &
            k, rad%ichan(k)%num, rad%ichan(k)%ave/real(n), &
            sqrt(rad%ichan(k)%rms/real(n)), &
            rad%ichan(k)%minimum%value, rad%ichan(k)%maximum%value
      end if
   end do

   if (trace_use) call da_trace_exit("da_print_stats_rad")

end subroutine da_print_stats_rad


