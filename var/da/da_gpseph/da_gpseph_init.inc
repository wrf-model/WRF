subroutine da_gpseph_init (grid)

! ---------------------------------------------------------------------------
! Purpose: Allocate domain-sized arrays for gpseph operator and
!          let every processor have the information of the domain-mean height
!          and refractivity on the mean height.
! ---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)      :: grid       ! first guess state

   integer :: i, j, k, ij, ijk
   integer :: nk
   real    :: temp
   real, dimension(kds:kde)   :: tmp_ref
   real, dimension(kds:kde)   :: refm
   real, dimension(3,kds:kde) :: cc

   if (trace_use_dull) call da_trace_entry("da_gpseph_init")

   if ( .not. allocated(global_lat) )        allocate ( global_lat (ids:ide,jds:jde) )
   if ( .not. allocated(global_lon) )        allocate ( global_lon (ids:ide,jds:jde) )
   if ( .not. allocated(global_terr) )       allocate ( global_terr (ids:ide,jds:jde) )
   if ( .not. allocated(global_h) )          allocate ( global_h (ids:ide,jds:jde,kds:kde) )
   if ( .not. allocated(global_ref) )        allocate ( global_ref (ids:ide,jds:jde,kds:kde) )
   if ( .not. allocated(global_xa_ref) )     allocate ( global_xa_ref (ids:ide,jds:jde,kds:kde) )
   if ( .not. allocated(global_adj_ref) )    allocate ( global_adj_ref (ids:ide,jds:jde,kds:kde) )
   if ( .not. allocated(global_h_mean) )     allocate ( global_h_mean (kds:kde) )
   if ( .not. allocated(global_ref_mean_h) ) allocate ( global_ref_mean_h (ids:ide,jds:jde,kds:kde) )
   global_lat = 0.0
   global_lon = 0.0
   global_terr = 0.0
   global_h = 0.0
   global_ref = 0.0
   global_xa_ref = 0.0
   global_adj_ref = 0.0
   global_h_mean = 0.0
   global_ref_mean_h = 0.0

   nk = kde-kts+1
   ij = ( ide-ids+1)*( jde-jds+1)

   ! Gather lat, lon, ref, h and terr for gpseph data
#ifdef DM_PARALLEL
   ijk = ( ide-ids+1)*( jde-jds+1)*(kde-kds+1)
   !  Collect xb component of h into global buffer.
   call da_patch_to_global( grid, grid%xb%h, global_h )
   call wrf_dm_bcast_real( global_h, ijk )
   !  Collect xb component of ref into global buffer.
   call da_patch_to_global( grid, grid%xb%ref, global_ref )
   call wrf_dm_bcast_real( global_ref, ijk )
   !  Collect xb component of lat into global buffer.
   call da_patch_to_global( grid, grid%xb%lat, global_lat )
   call wrf_dm_bcast_real( global_lat, ij )
   !  Collect xb component of lon into global buffer.
   call da_patch_to_global( grid, grid%xb%lon, global_lon )
   call wrf_dm_bcast_real( global_lon, ij )
   !  Collect xb component of terr into global buffer.
   call da_patch_to_global( grid, grid%xb%terr, global_terr )
   call wrf_dm_bcast_real( global_terr, ij )
#else
   do k = kds, kde
      do j = jds, jde
         do i = ids, ide
            global_h(i,j,k) = grid%xb%h(i,j,k)
            global_ref(i,j,k) = grid%xb%ref(i,j,k)
         enddo
      enddo
   enddo
   do j = jds, jde
      do i = ids, ide
         global_lat(i,j) = grid%xb%lat(i,j)
         global_lon(i,j) = grid%xb%lon(i,j)
         global_terr(i,j) = grid%xb%terr(i,j)
      enddo
   enddo
#endif

   ! Calculate the grid mean altitude
   do k = kds, kde
      temp = 0.0
      do j = jds, jde
         do i = ids, ide
            temp = temp + global_h(i,j,k)
         end do
      end do
      global_h_mean(k) = temp/float(ij)   ! in m
   enddo

   ! Interpolate refractivity to be on the grid mean altitude
   do j = jds, jde
      do i = ids, ide
         tmp_ref(kds:kde) = log(global_ref(i,j,kds:kde))
         call da_splinx(nk,global_h(i,j,kds:kde),tmp_ref(kds:kde),nk,global_h_mean(kds:kde),cc,refm)
         global_ref_mean_h(i,j,kds:kde) = exp(refm(kds:kde))
      end do
   end do

   global_h_mean(:) = global_h_mean * 0.001 !m to km

   if (trace_use_dull) call da_trace_exit("da_gpseph_init")

end subroutine da_gpseph_init

