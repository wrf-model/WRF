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
   integer :: nplev
   logical :: has_errtable
   integer :: junit, iost, itype, ivar, k
   integer :: isize

   if (trace_use) call da_trace_entry("da_setup_obs_structures_bufr")

   !-------------------------------
   ! 0.0 Make thinning grids
   !------------------------------

   if ( thin_conv ) then
      allocate(thinning_grid_conv(num_ob_indexes))
      do n = 1, num_ob_indexes
         if ( use_satwnd_bufr .and. n==polaramv ) cycle ! handled separately below
         if ( thin_conv_opt(n) > no_thin ) then
            call make3grids (n,thin_mesh_conv(n))
         end if
      end do
      if ( rootproc ) then ! for satwnd_bufr, thinning is done on rootproc
         if ( use_satwnd_bufr .and. thin_conv_opt(polaramv) > no_thin ) then
            if ( thin_conv_opt(polaramv) /= thin_superob_hv ) then
               call make3grids (polaramv, thin_mesh_conv(polaramv))
            else
               ! thin_superob_hv
               nplev = int(1200/thin_mesh_vert_conv(polaramv))
               call make3grids (polaramv, thin_mesh_conv(polaramv), thin_3d=.true., nlev=nplev)
            end if
         end if
      end if ! rootproc
   end if

   ! check for external obs error table
   inquire(file='obs_errtable', exist=has_errtable)
   if ( has_errtable ) then
      allocate (oetab(300,33,6))
   end if
   if ( rootproc ) then ! only rootproc reads the file
     if ( has_errtable ) then
       call da_get_unit(junit)
       open(unit=junit, file='obs_errtable', form='formatted', status='old', &
            iostat=iost)
       write(unit=message(1),fmt='(A)') "Reading obs_errtable file"
       call da_message(message(1:1))
       read_loop: do while ( iost == 0 )
         read (junit,'(1x,i3)',iostat=iost) itype
         if ( itype < 100 .or. itype > 300 ) then
            call da_warning(__FILE__,__LINE__, (/'Error reading obs_errtable'/))
         end if
         do k = 1, 33 ! levels
            ! 6 columns (p coord in hPa, t, q, uv, p, pw)
            read (junit,'(1x,6e12.5)',iostat=iost) (oetab(itype,k,ivar),ivar=1,6)
         end do
       end do read_loop
       close(junit)
       call da_free_unit(junit)
     end if ! has_errtable
   end if ! rootproc
   if ( has_errtable ) then
     isize = 300*33*6
     call wrf_dm_bcast_real(oetab, isize)
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

   if ( use_satwnd_bufr ) then
      call da_read_obs_bufr_satwnd('satwnd.bufr', iv, ob)
   end if

   if ( has_errtable ) then
      deallocate (oetab)
   end if

   if ( thin_conv ) then
      do n = 1, num_ob_indexes
         if ( use_satwnd_bufr .and. n==polaramv ) cycle ! handled separately below
         if ( thin_conv_opt(n) > no_thin ) then
            call destroygrids_conv (n)
         end if
      end do
      if ( rootproc ) then ! for satwnd_bufr, thinning is done on rootproc
         if ( use_satwnd_bufr .and. thin_conv_opt(polaramv) > no_thin ) then
            if ( thin_conv_opt(polaramv) /= thin_superob_hv ) then
               call destroygrids_conv (polaramv)
            else
               ! thin_superob_hv
               call destroygrids_conv (polaramv, thin_3d=.true.)
            end if
         end if
      end if ! rootproc
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


