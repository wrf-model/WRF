subroutine da_print_be_stats_h_global(outunit, variable, k, max_wavenumber, &
  total_power)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none 

   integer,      intent(inout) :: outunit        ! Output file unit.
   character*10, intent(in)    :: variable       ! Variable name.
   integer,      intent(in)    :: k              ! Vertical index.
   integer,      intent(in)    :: max_wavenumber ! Smallest scale required (ni/2-1)

   ! Total Power spectrum (averaged over time/members)
   real,         intent(in) :: total_power(0:max_wavenumber)

   integer :: n                     ! Loop counter.
   real    :: accum_power, variance ! Accumulated power, variance.

   if (trace_use) call da_trace_entry("da_print_be_stats_h_global")

   accum_power = 0.0
   variance = sum(total_power(0:max_wavenumber))

   write(unit=stdout,fmt='(3a,i5,a,i5)')' Power spectra for variable ', trim(variable), &
                          ' and level ', k, ' in unit ', outunit

   do n = 0, max_wavenumber
      accum_power = accum_power + total_power(n)
      write(unit=outunit,fmt='(2i4,2f18.5,f8.3)')k, n, total_power(n), accum_power, &
                                        accum_power / variance
   end do

   outunit = outunit + 1
   write(unit=stdout,fmt=*) ''

   if (trace_use) call da_trace_exit("da_print_be_stats_h_global")

end subroutine da_print_be_stats_h_global


