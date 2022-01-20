subroutine da_setup_obs_structures_rain(grid, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain) , intent(in)    :: grid        ! model data
   type (y_type),  intent(out)   :: ob          ! Observation structure.
   type (iv_type), intent(inout) :: iv          ! O-B structure.

   character(len=filename_len)  :: filename
   integer                     :: n,i,j,k

   if (trace_use) call da_trace_entry("da_setup_obs_structures_rain")

   !-------------------------------
   ! 0.0 Make thinning grids
   !------------------------------

   if ( thin_rainobs ) then
      allocate(thinning_grid(num_ob_indexes,num_fgat_time))
      do n=1, num_fgat_time
         call makegrids (rain,thin_mesh_conv(rain), n)
      end do
   end if

   !--------------------------------------------------------------------------
   ! [1.0] Scan data
   !--------------------------------------------------------------------------

   do n=2, num_fgat_time
      if ( .not. fgat_rain_flags(n) ) cycle
      iv%time = n
      filename = ' '

      ! scan rainfall observation file
      write(filename(1:9), fmt='(a, i2.2, a)') 'ob', n,'.rain'
      call da_scan_obs_rain(iv, filename, iv%time)

      iv%info(rain)%plocal(n) = iv%info(rain)%nlocal
      iv%info(rain)%ptotal(n) = iv%info(rain)%ntotal
   end do

   !--------------------------------------------------------------------------
   ! Allocate the ob based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations_rain (iv)

   !--------------------------------------------------------------------------
   ! [2.0] Read data
   !--------------------------------------------------------------------------
    
   do n=2, num_fgat_time
      if ( .not. fgat_rain_flags(n) ) cycle
      iv%time = n
      filename = ' '

      ! read rainfall observation file
      write(filename(1:9), fmt='(a, i2.2, a)') 'ob', n,'.rain'
     call da_read_obs_rain(iv, filename, iv%time)
   end do

   if ( thin_rainobs ) then
     do n = 1, num_fgat_time
        call destroygrids (rain,n)
     end do
     deallocate(thinning_grid)
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures_rain(iv, ob)

   iv%time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_rain")

end subroutine da_setup_obs_structures_rain


