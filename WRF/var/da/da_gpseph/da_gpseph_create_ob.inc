SUBROUTINE da_gpseph_create_ob ( nlevel, h, ref, lat, lon, azim, pseudo_ob, lowest_level )

!---------------------------------------------------------------------------------------
!  Purpose: calculate pseudo obs (refractivity) at the mean height of each model level.
!           (Chen et al., 2009, doi:10.3319/TAO.2007.11.29.01(F3C))
!           It is called by da_read_obs_bufrgpsro_eph (when ob_format_gpsro=1)
!           or da_read_obs_ascii (when ob_format_gpsro=2).
!---------------------------------------------------------------------------------------

   implicit none

   type (ob_in_mean_h), intent(inout) :: pseudo_ob
   integer,             intent(out)   :: lowest_level
   integer,             intent(in)    :: nlevel
   real,                intent(in)    :: h(nlevel)
   real,                intent(in)    :: ref(nlevel)
   real,                intent(in)    :: lat(nlevel)
   real,                intent(in)    :: lon(nlevel)
   real,                intent(in)    :: azim(nlevel)

   integer :: i, j, k
   integer :: nlevel_valid, icount
   real    :: nstart, nstop

   real, dimension(kds:kde) :: mean_h

   real, allocatable :: log_ref(:)   !nlevel_valid
   real, allocatable :: h_km(:)      !nlevel_valid
   real, allocatable :: lat_used(:)  !nlevel_valid
   real, allocatable :: lon_used(:)  !nlevel_valid
   real, allocatable :: azim_used(:) !nlevel_valid

   real, dimension(interpolate_level) :: tmp_z   ! obs. levels are interpolated
                                                 ! into levels of interpolate_level
   real, dimension(interpolate_level) :: tmp_ref
   real, dimension(interpolate_level) :: tmp_lat
   real, dimension(interpolate_level) :: tmp_lon
   real, dimension(interpolate_level) :: tmp_azim

   lowest_level = -1

   pseudo_ob%ref(:) = 0.0
   pseudo_ob%lat(:) = 0.0
   pseudo_ob%lon(:) = 0.0
   pseudo_ob%azim(:)= 0.0

   ! exclude the levels with invalid ref and h data
   ! first find out the nlevel_valid for allocating the working arrays
   icount = 0
   do k = 1, nlevel
      if ( (ref(k) > 0.0) .and. (h(k) > 0.0) .and. &
           (abs(lat(k)) <= 90.0)  .and.            &
           (abs(lon(k)) <= 180.0) .and.            &
           (abs(azim(k)) <= 180.0) ) then
         icount = icount + 1
      end if
   end do
   nlevel_valid = icount
   if ( nlevel_valid < 1 ) return
   allocate (log_ref  (nlevel_valid))
   allocate (h_km     (nlevel_valid))
   allocate (lat_used (nlevel_valid))
   allocate (lon_used (nlevel_valid))
   allocate (azim_used(nlevel_valid))

   ! copy the valid levels to the working arrays
   icount = 0
   do k = 1, nlevel
      if ( (ref(k) > 0.0) .and. (h(k) > 0.0) .and. &
           (abs(lat(k)) <= 90.0)  .and.            &
           (abs(lon(k)) <= 180.0) .and.            &
           (abs(azim(k)) <= 180.0) ) then
         icount = icount + 1
         log_ref(icount)   = log(ref(k))
         h_km(icount)      = h(k)*0.001  !m to km, observation height
         lat_used(icount)  = lat(k)
         lon_used(icount)  = lon(k)
         azim_used(icount) = azim(k)
      end if
   end do

   mean_h(:) = global_h_mean(:) !km, model mean height

   do k=kts,kte
      if ( mean_h(k) > h_km(1) ) exit
   end do
   lowest_level = k

   ! interpolate_level =2000, tmp_z(k) increase with constant interval(0.01 km) in the vertical direction
   !hcl-todo make 0.01 and 20km_top (used to derive interpolate_level) namelist options
   do k=1,interpolate_level
      tmp_z(k) = h_km(1)+0.01*(k-1)
   end do

   ! interpolate variables and smooth them on the standard height grid
   CALL lintp( nlevel_valid, h_km, log_ref,          &
               interpolate_level, tmp_z, tmp_ref )
   CALL lintp( nlevel_valid, h_km, lat_used,         &
               interpolate_level, tmp_z, tmp_lat )
   CALL lintp( nlevel_valid, h_km, lon_used,         &
               interpolate_level, tmp_z, tmp_lon )
   CALL lintp( nlevel_valid, h_km, azim_used,        &
               interpolate_level, tmp_z, tmp_azim )

   do i = lowest_level+1, kte-1

      do j = 1, interpolate_level
         if (tmp_z(j) < (mean_h(i)+mean_h(i-1))/2.0) nstart = j
         if (tmp_z(j) < (mean_h(i)+mean_h(i+1))/2.0) nstop  = j
      end do

      pseudo_ob%ref(i) = 0.0
      pseudo_ob%lat(i) = 0.0
      pseudo_ob%lon(i) = 0.0
      pseudo_ob%azim(i)= 0.0

      do j = nstart, nstop
         pseudo_ob%ref(i)  = pseudo_ob%ref(i)  + tmp_ref(j)
         pseudo_ob%lat(i)  = pseudo_ob%lat(i)  + tmp_lat(j)
         pseudo_ob%lon(i)  = pseudo_ob%lon(i)  + tmp_lon(j)
         pseudo_ob%azim(i) = pseudo_ob%azim(i) + tmp_azim(j)
      end do

      pseudo_ob%ref(i)  = pseudo_ob%ref(i) / (nstop-nstart+1)
      pseudo_ob%lat(i)  = pseudo_ob%lat(i) / (nstop-nstart+1)
      pseudo_ob%lon(i)  = pseudo_ob%lon(i) / (nstop-nstart+1)
      pseudo_ob%azim(i) = pseudo_ob%azim(i)/ (nstop-nstart+1)

   end do

   do k = lowest_level+1, kte-1
      pseudo_ob%ref(k) = exp(pseudo_ob%ref(k))
      if ( pseudo_ob%lon(k) < 0.0 ) then
         pseudo_ob%lon(k) = pseudo_ob%lon(k) + 360.0
      end if
   end do

   deallocate (log_ref  )
   deallocate (h_km     )
   deallocate (lat_used )
   deallocate (lon_used )
   deallocate (azim_used)

END SUBROUTINE da_gpseph_create_ob

! ................................................................

SUBROUTINE lintp ( n1, x1, y1, n2, x2, y2 )

! **********************************************************
!  This subroutine provides linear interpolation of function
!  y(x) from one grid x1(i), onto another grid x2(i)
! 
!  y1(i), i=1,n1 are input grid function values
!  y2(i), i=1,n2 are output grid function values
! **********************************************************
!  both x1(i) and x2(i) should increase with 'i'
! **********************************************************
      implicit none
      integer :: i, j, k, l, m, n
      integer, intent(in)::n1
      integer, intent(in)::n2
      real,dimension(n1),intent(in)::x1
      real,dimension(n2),intent(in)::x2
      real,dimension(n1),intent(in)::y1
      real,dimension(n2),intent(out)::y2

      do i=1,n2
      if (x2(i).le.x1(1)) then
      y2(i)=y1(1)+(x2(i)-x1(1))*(y1(2)-y1(1))/(x1(2)-x1(1))
      else if (x2(i).ge.x1(n1)) then
      y2(i)=y1(n1)+(x2(i)-x1(n1))*(y1(n1)-y1(n1-1))/(x1(n1)-x1(n1-1))
      else

      do j=1,n1
      k=j
      if ((x1(j).le.x2(i)).and.(x1(j+1).ge.x2(i))) goto 1
      end do

1     y2(i)=y1(k)+(x2(i)-x1(k))*(y1(k+1)-y1(k))/(x1(k+1)-x1(k))

      end if
      end do

END SUBROUTINE lintp

