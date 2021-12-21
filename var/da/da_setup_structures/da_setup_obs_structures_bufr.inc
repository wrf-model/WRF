subroutine da_setup_obs_structures_bufr(grid, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain) , intent(in)    :: grid        ! model data
   type (y_type),  intent(out)   :: ob          ! Observation structure.
   type (iv_type), intent(inout) :: iv          ! O-B structure.

#ifdef BUFR

   character(len=filename_len) :: filename
   integer                     :: n,i,j

   if (trace_use) call da_trace_entry("da_setup_obs_structures_bufr")

   !-------------------------------
   ! 0.0 Make thinning grids
   !------------------------------

   if ( thin_conv ) then
      allocate(thinning_grid_conv(num_ob_indexes))
      do n = 1, num_ob_indexes
         if ( thin_conv_opt(n) > no_thin ) then
            call make3grids (n,thin_mesh_conv(n))
         end if
      end do
   end if

   !--------------------------------------------------------------------------
   ! [1.0] Read data
   !--------------------------------------------------------------------------
    
      iv%time = 1
      call da_read_obs_bufr(iv)
! 
!for gps

   if ( use_gpsrefobs ) then
      call da_read_obs_bufrgpsro(iv)
   end if

   if ( use_gpsephobs ) then
      call da_read_obs_bufrgpsro_eph(iv)
   end if

   if ( thin_conv ) then
      do n = 1, num_ob_indexes
         if ( thin_conv_opt(n) > no_thin ) then
            call destroygrids_conv (n)
         end if
      end do
      deallocate(thinning_grid_conv)
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures(iv, ob)

   iv%time = 1

#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a BUFR library"/)) 
#endif

   if (trace_use) call da_trace_exit("da_setup_obs_structures_bufr")

end subroutine da_setup_obs_structures_bufr


