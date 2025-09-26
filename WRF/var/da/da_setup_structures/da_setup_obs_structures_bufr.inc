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
   
   ! thinning variables
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)

   if (trace_use) call da_trace_entry("da_setup_obs_structures_bufr")

   !-------------------------------
   ! 0.0 Make thinning grids
   !------------------------------
   call init_constants_derived

   if ( thin_conv ) then
      rlat_min =  r999
      rlat_max = -r999
      rlon_min =  r999
      rlon_max = -r999

      istart=MINVAL( grid%i_start(1:grid%num_tiles) )
      iend  =MAXVAL( grid%i_end  (1:grid%num_tiles) )
      jstart=MINVAL( grid%j_start(1:grid%num_tiles) )
      jend  =MAXVAL( grid%j_end  (1:grid%num_tiles) ) 

      do i = istart, iend
         do j = jstart, jend
            rlat_min=min(rlat_min, grid%xb%lat(i,j))
            rlat_max=max(rlat_max, grid%xb%lat(i,j))
            if( grid%xb%lon(i,j) < 0.0) then
              rlon_min=min(rlon_min, (r360+grid%xb%lon(i,j)))
              rlon_max=max(rlon_max, (r360+grid%xb%lon(i,j)))
            else
              rlon_min=min(rlon_min, grid%xb%lon(i,j))
              rlon_max=max(rlon_max, grid%xb%lon(i,j))
            endif
         enddo
      enddo

#ifdef DM_PARALLEL
      call mpi_reduce(rlat_min, rlonlat(1), 1, true_mpi_real, mpi_min, root, comm, ierr)
      call mpi_reduce(rlon_min, rlonlat(2), 1, true_mpi_real, mpi_min, root, comm, ierr)
      call mpi_reduce(rlat_max, rlonlat(3), 1, true_mpi_real, mpi_max, root, comm, ierr)
      call mpi_reduce(rlon_max, rlonlat(4), 1, true_mpi_real, mpi_max, root, comm, ierr)

      CALL mpi_bcast( rlonlat, 4 , true_mpi_real , root , comm, ierr )

      rlat_min = rlonlat(1)
      rlon_min = rlonlat(2)
      rlat_max = rlonlat(3)
      rlon_max = rlonlat(4)
#endif

      dlat_grid = rlat_max - rlat_min
      dlon_grid = rlon_max - rlon_min

      allocate(thinning_grid_conv(num_ob_indexes))
      do n = 1, num_ob_indexes
         call make3grids (n,thin_mesh_conv(n))
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
         call destroygrids_conv (n)
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


