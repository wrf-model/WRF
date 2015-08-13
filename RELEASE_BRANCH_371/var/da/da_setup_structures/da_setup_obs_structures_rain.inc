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
   
   ! thinning variables
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)

   if (trace_use) call da_trace_entry("da_setup_obs_structures_rain")

   !-------------------------------
   ! 0.0 Make thinning grids
   !------------------------------
   call init_constants_derived

   if ( thin_rainobs ) then
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


