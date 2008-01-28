subroutine da_readwrite_be_stage2(outunit, nk)

   ! ----------------------------------------------------------------------
   ! Purpose: Read and write the dimensions, bin information, and 
   !          regression coefficients.
   !
   ! ----------------------------------------------------------------------

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(out)     :: nk                         ! Number of vertical levels/modes.

   character(len=filename_len) :: filename                   ! Input filename.

   integer                  :: ni, nj                     ! Number of points in x/y direction.
   integer                  :: bin_type                   ! Type of bin to average over. !!!DALE ADD.
   integer                  :: num_bins                   ! Number of 3D bins.
   integer                  :: num_bins2d                 ! Number of 2D bins.

   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m). !!!DALE ADD..
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.

   real, allocatable   :: regcoeff1(:)                    ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)                  ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)                ! psi/T regression cooefficient.

   integer :: iunit

   ! Read in the coefficients:

   call da_get_unit(iunit)
   filename = 'gen_be_stage2.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   read(iunit)num_bins, num_bins2d

   allocate(regcoeff1(1:num_bins))
   allocate(regcoeff2(1:nk,1:num_bins2d))
   allocate(regcoeff3(1:nk,1:nk,1:num_bins2d))

   read(iunit)regcoeff1 ! psi/chi regression coefficient.
   read(iunit)regcoeff2 ! psi/ps  regression coefficient.
   read(iunit)regcoeff3 ! psi/T   regression coefficient.
   close(iunit)

   ! Read in the bin information:

   allocate(bin(1:ni,1:nj,1:nk))
   allocate(bin2d(1:ni,1:nj))

   filename = 'bin.data'
   open (iunit, file = filename, form='unformatted')

   read(iunit)bin_type
   read(iunit)lat_min, lat_max, binwidth_lat
   read(iunit)hgt_min, hgt_max, binwidth_hgt
   read(iunit)num_bins, num_bins2d
   read(iunit)bin(1:ni,1:nj,1:nk)
   read(iunit)bin2d(1:ni,1:nj)
   close(iunit)
   call da_free_unit(iunit)

   ! Write out the dimensions and bin information:

   write(outunit)ni, nj, nk

   write(outunit)bin_type
   write(outunit)lat_min, lat_max, binwidth_lat
   write(outunit)hgt_min, hgt_max, binwidth_hgt
   write(outunit)num_bins, num_bins2d
   write(outunit)bin(1:ni,1:nj,1:nk)
   write(outunit)bin2d(1:ni,1:nj)

   deallocate(bin)
   deallocate(bin2d)

   ! Write out the coefficients:

   write(outunit)regcoeff1
   write(outunit)regcoeff2
   write(outunit)regcoeff3

   deallocate(regcoeff1)
   deallocate(regcoeff2)
   deallocate(regcoeff3)

end subroutine da_readwrite_be_stage2


