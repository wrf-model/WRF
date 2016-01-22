subroutine da_data_distribution(ob_name, num_obs, min_val, max_val, bin_width, ob)
   
   !---------------------------------------------------------------------------
   ! Purpose: Bin ob data to get distribution.
   !---------------------------------------------------------------------------

   implicit none
   
   character (len=*), intent(in) :: ob_name       ! Data description.
   integer,           intent(in) :: num_obs       ! Number of obs.
   real,              intent(in) :: min_val       ! Minimum bin value.
   real,              intent(in) :: max_val       ! Maximum bin value.
   real,              intent(in) :: bin_width     ! Bin width.
   real,              intent(in) :: ob(:)         ! Ob data.
   
   integer              :: num_bins      ! Number of bins
   integer              :: bin           ! Bin counter.
   integer              :: n             ! Data counter.
   integer              :: num_missing   ! Number of missing data.
   
   real, allocatable    :: bin_val(:)    ! Central value of bin.
   real, allocatable    :: bin_min(:)    ! Minimum value of bin.
   integer, allocatable :: num_in_bin(:) ! Number of values in bin. 

   if (trace_use) call da_trace_entry("da_data_distribution")      
   
   !---------------------------------------------------------------------------
   ! [1.0] Initialise bins:
   !---------------------------------------------------------------------------

   num_bins = int((max_val - min_val) / bin_width) + 1

   allocate(bin_val(1:num_bins))
   bin_val(1) = min_val
   do bin = 2, num_bins
      bin_val(bin) = bin_val(bin-1) + bin_width
   end do
   
   allocate(bin_min(1:num_bins+1))
   bin_min(1:num_bins) = bin_val(1:num_bins) - 0.5 * bin_width
   bin_min(num_bins+1) = bin_val(num_bins) + 0.5 * bin_width

   allocate(num_in_bin(0:num_bins+1))
   num_in_bin(0:num_bins+1) = 0
   num_missing = 0
   
   !---------------------------------------------------------------------------
   ! [2.0] Assign data to bins:
   !---------------------------------------------------------------------------
   
   do n = 1, num_obs
      if (ob(n) == missing_r) then
         num_missing = num_missing + 1
      else if (ob(n) < bin_min(1) .AND. ob(n) /= missing_r) then
         num_in_bin(0) = num_in_bin(0) + 1
      else if (ob(n) >= bin_min(num_bins+1)) then
         num_in_bin(num_bins+1) = num_in_bin(num_bins+1) + 1
      else
         do bin = 1, num_bins
            if (ob(n) >= bin_min(bin) .AND. ob(n) < bin_min(bin+1)) then
                 num_in_bin(bin) = num_in_bin(bin) + 1
               exit
            end if
         end do
      end if
   end do

   !---------------------------------------------------------------------------
   ! [3.0] Output statistics:
   !---------------------------------------------------------------------------
   
   write(unit=stdout,fmt='(A,A,A,I8)')' Number of ', trim(ob_name), &
      ' obs = ', num_obs
   write(unit=stdout,fmt='(A,I8)')' Number with missing data indicator = ', &
      num_missing
   write(unit=stdout,fmt='(A,f12.5,A,I8)')' Number below minimum O-B(', &
      min_val-0.5*bin_width, ') = ', num_in_bin(0)
   do bin = 1, num_bins
      write(unit=stdout,fmt='(I4,f12.5,I8)')bin, bin_val(bin), num_in_bin(bin)
   end do
   write(unit=stdout,fmt='(A,f12.5,A,I8)')' Number above maximum O-B(', &
      max_val+0.5*bin_width, ') = ', num_in_bin(num_bins+1)
                               
   !---------------------------------------------------------------------------
   ! [4.0] Tidy up:
   !---------------------------------------------------------------------------

   deallocate(bin_val)
   deallocate(bin_min)
   deallocate(num_in_bin)

   if (trace_use) call da_trace_exit("da_data_distribution")      

end subroutine da_data_distribution


