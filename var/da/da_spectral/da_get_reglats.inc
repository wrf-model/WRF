subroutine da_get_reglats( nj, lat, sinlat, coslat, int_wgts)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nj             ! Number of latitudes
   real,    intent(out) :: lat(1:nj)      ! Latitude(radians, from south).
   real,    intent(out) :: sinlat(1:nj)   ! sin(Latitude).
   real,    intent(out) :: coslat(1:nj)   ! cos(Latitude).
   real,    intent(out) :: int_wgts(1:nj) ! Legendre Integration weights. 

   integer :: j              ! Loop counter.
   real    :: delta_phi      ! Regular latitude interval.

   if (trace_use) call da_trace_entry("da_get_reglats")

   delta_phi = pi / real(nj-1) 

   do j = 1, nj / 2
      lat(j) = -0.5 * pi + delta_phi * real(j - 1)
      sinlat(j) = sin(lat(j))
      coslat(j) = cos(lat(j))
      int_wgts(j) = coslat(j) * delta_phi

      ! use symmetry for northern hemisphere:
      lat(nj+1-j) = -lat(j)
      sinlat(nj+1-j) = -sinlat(j)
      coslat(nj+1-j) = coslat(j)
      int_wgts(nj+1-j) = int_wgts(j)
   end do

   if ((nj+1) / 2 == nj/2 + 1) then  ! Odd, then equator point:
      lat(nj/2+1) = 0.0
      sinlat(nj/2+1) = 0.0
      coslat(nj/2+1) = 1.0
      int_wgts(nj/2+1) = delta_phi
   end if

   if (trace_use) call da_trace_exit("da_get_reglats")

end subroutine da_get_reglats


