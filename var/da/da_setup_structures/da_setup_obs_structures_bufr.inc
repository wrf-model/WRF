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

      istart=grid%i_start(1)
      iend  =grid%i_end  (1)
      jstart=grid%j_start(1)
      jend  =grid%j_end  (1)

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
   ! [1.0] Scan BUFR observation header and get idea of number of obs:
   !--------------------------------------------------------------------------

   if (num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%time = n

         write(filename(1:10), fmt='(a, i2.2,a)') 'ob', n,'.bufr'

         ! scan prepbufr observation file
         call da_scan_obs_bufr (iv, filename)
        iv%info(:)%plocal(n) = iv%info(:)%nlocal
        iv%info(:)%ptotal(n) = iv%info(:)%ntotal
      end do
   else
      iv%time = 1
      filename="ob.bufr"
      call da_scan_obs_bufr(iv, filename)
      do i=1,num_ob_indexes
         iv%info(i)%plocal(iv%time) = iv%info(i)%nlocal
         iv%info(i)%ptotal(iv%time) = iv%info(i)%ntotal
      end do

      ! scan main body of radar observation file
      ! if (use_radarobs) &
      !    call da_scan_bufr_radar(iv, 'radar.dat')
   end if

   !-------------------------------------------------------------------------
   ! Allocate the iv based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations (iv)

   if (num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%time = n

         write(filename(1:10), fmt='(a, i2.2)') 'ob.', n

         ! read prepbufr observation file
         call da_read_obs_bufr (iv, filename)

         ! if (use_radarobs) then
         !    ! read radar observation file
         !    write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
         !    call da_read_bufr_radar(iv, filename)
         ! end if
      end do
   else

      iv%time = 1

      filename="ob.bufr"
      call da_read_obs_bufr(iv, filename)

      ! if (use_radarobs) then
      !    ! read radar observation file
      !    call da_read_bufr_radar(iv)
      ! end if
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


