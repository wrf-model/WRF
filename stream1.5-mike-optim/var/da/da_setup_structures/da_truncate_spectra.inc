subroutine da_truncate_spectra(max_wave, nw, power_trunc, power, max_wave_trunc)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: max_wave         ! Smallest wave for domain.
   integer, intent(in)  :: nw               ! Dimension of input power spectrum.
   real,    intent(in)  :: power_trunc      ! Power truncation (fraction).
   real*8,  intent(in)  :: power(0:nw)      ! Power spectrum.
   integer, intent(out) :: max_wave_trunc   ! Smallest wave after truncation.

   integer :: l                ! Loop counter.
   real    :: truncated_power  ! Truncated power.
   real    :: cumul_power      ! Cumulative power.

   if (trace_use) call da_trace_entry("da_truncate_spectra")

   truncated_power = power_trunc * sum(power(0:nw))

   cumul_power = 0.0
   max_wave_trunc = max_wave
   do l = 0, nw - 1 
      cumul_power = cumul_power + power(l)
      if (cumul_power > truncated_power) then
         max_wave_trunc = l - 1
         exit
      end if
   end do

   if (max_wave_trunc > max_wave) then
      write(unit=message(1),fmt='(a)') &
         'da_truncate_spectra: Power requested needs higher resolution.'     
      write(unit=message(2),fmt='(a,i8)') &
         'Maximum grid wavenumber =  ', max_wave
      write(unit=message(3),fmt='(a,i8)') &
         'Truncating to wavenumber = ', max_wave_trunc
      call da_warning(__FILE__,__LINE__,message(1:3))
      max_wave_trunc = max_wave
   end if

   if (trace_use) call da_trace_exit("da_truncate_spectra")

end subroutine da_truncate_spectra


