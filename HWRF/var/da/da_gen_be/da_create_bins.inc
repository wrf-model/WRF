subroutine da_create_bins(ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
   lat_min, lat_max, binwidth_lat, hgt_min, hgt_max, binwidth_hgt, latitude, height)

   !----------------------------------------------------------------------------
   !
   ! Purpose: To create the bins for calculation of statistics.
   !
   ! Input:
   ! ni, nj, nk   Dimensions
   ! bin_type     0: No binning; 
   !              1: bins for X-direction mean; 
   !              2: bins for each of binwidth_lat/binwidth_hgt.  
   !              3: bins for each of binwidth_lat/nk.  
   !              4: num_hor_bins horizontal bins /nk.  
   !              5: Average over all horizontal points (nk bins for 3D fields)
   !              6: Average over all points (only 1 bin).
   ! Optional for bin_type = 2:
   ! lat_min, lat_max Minimum/maximum latitude ranges for bin_type = 2
   ! binwidth_lat interval between bins for latitude in degree for bin_type = 2
   ! binwidth_hgt interval between bins for height in meter for bin_type = 2
   ! num_bins_hgt the number of height bins for bin_type = 2
   ! latitude     3d field latitude in degree for bin_type = 2
   ! height       3d field height in meter for bin_type = 2
   !
   ! Output:
   ! num_bins,num_bins2d ---- the number of bins for 3d and 2d fields
   ! bin     ,     bin2d ---- Assigned bin to a gridpoints for 3d and 2d fields
   !
   !----------------------------------------------------------------------------

   implicit none

   integer,        intent(in)  :: ni, nj, nk          ! Dimensions read in.
   integer,        intent(in)  :: bin_type            ! Type of bin to average over
   integer,        intent(out) :: num_bins            ! Number of bins.
   integer,        intent(out) :: num_bins2d          ! Number of bins for 2D fields
   integer,        intent(out) :: bin(1:ni,1:nj,1:nk) ! Bin at each 3D point
   integer,        intent(out) :: bin2d(1:ni,1:nj)    ! Bin at each 2D point

   real, optional, intent(in)  :: lat_min, lat_max    ! Used if bin_type = 2 (deg)
   real, optional, intent(in)  :: binwidth_lat        ! Used if bin_type = 2 (deg)
   real, optional, intent(in)  :: hgt_min, hgt_max    ! Used if bin_type = 2 (deg)
   real, optional, intent(in)  :: binwidth_hgt        ! Used if bin_type = 2 (m).
   real, optional, intent(in)  :: latitude(1:ni,1:nj) ! Latitude (degrees).
   real, optional, intent(in)  :: height(1:ni,1:nj,1:nk)     ! Height (m).

   integer           :: b, i, j, k                 ! Loop counters.
   integer           :: count                      ! Counter
   integer           :: num_bins_lat               ! Used if bin_type = 2.
   integer           :: num_bins_hgt               ! Used if bin_type = 2.
   integer           :: bin_lat                    ! Latitude bin.
   integer           :: bin_hgt                    ! Height bin.
   integer           :: num_bins_i, num_bins_j     ! Used if bin_type = 4.
   integer           :: nii, njj                   ! Used if bin_type = 4.
   integer           :: bin_i(1:ni), bin_j(1:nj)   ! Used if bin_type = 4.
   real, allocatable :: binstart_lat(:)            ! Used if bin_type = 2 (deg)
   real, allocatable :: binstart_hgt(:)            ! Used if bin_type = 2 (deg)

   if (trace_use) call da_trace_entry("da_create_bins")

   if (bin_type == 0) then         ! No averaging in space

      num_bins = nk * nj * ni
      num_bins2d = nj * ni    ! Equals number of horizontal points.

      count = 1
      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               bin(i,j,k) = count
               count = count + 1
            end do
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if (bin_type == 1) then    ! Average over x-direction.

      num_bins = nj * nk
      num_bins2d = nj

      count = 1
      do k = 1, nk
         do j = 1, nj
            bin(1:ni,j,k) = count
            count = count + 1
         end do
      end do
      bin2d(:,:) = bin(:,:,1)

   else if (bin_type == 2) then    ! Global latitude/height bins:

      ! Setup latitude bins:
      write(unit=stdout,fmt='(/a,f12.5)')'   Minimum latitude = ', lat_min
      write(unit=stdout,fmt='(a,f12.5)')'    Maximum latitude = ', lat_max
      write(unit=stdout,fmt='(a,f12.5)') &
         '    Latitude bin width = ', binwidth_lat
      num_bins_lat = nint((lat_max - lat_min) / binwidth_lat)
      write(unit=stdout,fmt='(a,i8)') &
         '    Number of latitude bins = ', num_bins_lat
   
      allocate(binstart_lat(1:num_bins_lat))
      do b = 1, num_bins_lat ! Assume south to north (as in WRF).
         binstart_lat(b) = lat_min + real(b-1) * binwidth_lat
      end do

      ! Setup height bins:
      write(unit=stdout,fmt='(/a,f12.5)')'    Minimum height = ', hgt_min
      write(unit=stdout,fmt='(a,f12.5)')'    Maximum height = ', hgt_max
      write(unit=stdout,fmt='(a,f12.5)')'    Height bin width = ', binwidth_hgt
      num_bins_hgt = nint((hgt_max - hgt_min) / binwidth_hgt)
      write(unit=stdout,fmt='(a,i8)') &
         '    Number of height bins = ', num_bins_hgt

      allocate(binstart_hgt(1:num_bins_hgt))
      do b = 1, num_bins_hgt
         binstart_hgt(b) = hgt_min + real(b-1) * binwidth_hgt
      end do

      num_bins = num_bins_lat * num_bins_hgt
      num_bins2d = num_bins_lat

      ! Select height bins:
      do j = 1, nj
         do i = 1, ni
            do k = 1, nk
               if (height(i,j,k) < binstart_hgt(1)) then 
                  bin_hgt = 1 ! In first bin.
               else if (height(i,j,k) >= binstart_hgt(num_bins_hgt)) then
                  bin_hgt = num_bins_hgt ! In final bin.
               else 
                  do b = 1, num_bins_hgt-1
                     if (height(i,j,k) >= binstart_hgt(b) .and. &
                          height(i,j,k) <  binstart_hgt(b+1)) then
                        bin_hgt = b
                        exit
                     end if
                  end do
               end if

               ! Select latitude bin that point falls in:
               if (k == 1) then
                  do b = 1, num_bins_lat-1
                     if (latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1)) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if (latitude(i,j) >= binstart_lat(num_bins_lat)) then
                     ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * (bin_hgt - 1)
            end do
         end do
      end do

      deallocate(binstart_lat)
      deallocate(binstart_hgt)

   else if (bin_type == 3) then    ! Latitude/nk bins:

      ! Setup latitude bins:
      write(unit=stdout,fmt='(/a,f12.5)')'   Minimum latitude = ', lat_min
      write(unit=stdout,fmt='(a,f12.5)')'    Maximum latitude = ', lat_max
      write(unit=stdout,fmt='(a,f12.5)')'    Latitude bin width = ',binwidth_lat
      num_bins_lat = nint((lat_max - lat_min) / binwidth_lat)
      write(unit=stdout,fmt='(a,i8)') &
         '    Number of latitude bins = ', num_bins_lat
   
      allocate(binstart_lat(1:num_bins_lat))
      do b = 1, num_bins_lat ! Assume south to north (as in WRF).
         binstart_lat(b) = lat_min + real(b-1) * binwidth_lat
      end do

      num_bins = num_bins_lat * nk
      num_bins2d = num_bins_lat

      ! Select bins:
      do j = 1, nj
         do i = 1, ni
            do k = 1, nk
               ! Select latitude bin that point falls in:
               if (k == 1) then
                  do b = 1, num_bins_lat-1
                     if (latitude(i,j) >= binstart_lat(b) .and. &
                        latitude(i,j) < binstart_lat(b+1)) then
                        bin_lat = b
                        exit
                     end if
                  end do
                  if (latitude(i,j) >= binstart_lat(num_bins_lat)) then
                     ! In final bin.
                     bin_lat = num_bins_lat
                  end if
                  bin2d(i,j) = bin_lat
               end if
               bin(i,j,k) = bin_lat + num_bins_lat * (k - 1)
            end do
         end do
      end do

      deallocate(binstart_lat)

   else if (bin_type == 4) then    ! binwidth_lat/nk bins:

      ! Setup horizontal bins:
      write(unit=stdout,fmt='(/a,f12.5)') &
         '   Number of grid-cells to average over = ', binwidth_lat
      ! use binwidth_lat, but actually an integer number of points.
 
      num_bins_j = int(real(nj) / real(binwidth_lat))
      njj = int(binwidth_lat) * num_bins_j
      do j = 1, njj
         bin_j(j) = 1 + int(real(j-1) / binwidth_lat)
      end do
      if (nj > njj) bin_j(njj+1:nj) = bin_j(njj)

      num_bins_i = int(real(ni) / real(binwidth_lat))
      nii = int(binwidth_lat) * num_bins_i
      do i = 1, nii
         bin_i(i) = 1 + int(real(i-1) / binwidth_lat)
      end do
      if (ni > nii) bin_i(nii+1:ni) = bin_i(nii)

      num_bins2d = num_bins_i * num_bins_j
      num_bins = num_bins2d * nk

      do j = 1, nj
         do i = 1, ni
            bin2d(i,j) = bin_i(i) + (bin_j(j) - 1) * num_bins_i
            do k = 1, nk
               bin(i,j,k) = bin2d(i,j) + (k - 1) * num_bins2d
            end do
         end do
      end do

   else if (bin_type == 5) then    ! Average over all horizontal points.

      num_bins = nk
      num_bins2d = 1

      do k = 1, nk
         bin(:,:,k) = k
      end do
      bin2d(:,:) = 1

   else if (bin_type == 6) then    ! Average over all points.

      num_bins = 1
      num_bins2d = 1
      bin(:,:,:) = 1
      bin2d(:,:) = 1
   end if

   if (trace_use) call da_trace_exit("da_create_bins")

end subroutine da_create_bins


