subroutine da_readwrite_be_stage1(outunit, nk)

   ! ----------------------------------------------------------------------
   ! Purpose: Read and write the dimensions and bin information from stage 1
   !
   !  Note: Please acknowledge author/institute in work that uses this code.
   !
   ! ----------------------------------------------------------------------

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(out)     :: nk                         ! Number of vertical levels/modes.
   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date                       ! Current date (ccyymmddhh).
   character*10        :: be_method                  ! BE Method (NMC or ENS)
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
 
   character(len=filename_len) :: filename                   ! Input filename.
   character(len=filename_len) :: dat_dir                    ! Input data directory.

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

   logical             :: allow_missing_dates        ! If data from stage 0 is not contiguous, attempt to continue

   integer :: iunit, namelist_unit

   namelist / gen_be_stage1_nl / start_date, end_date, interval, &
                                 be_method, ne, bin_type, cv_options, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, &
                                 dat_dir, allow_missing_dates

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   be_method = 'NMC'
   ne = 1
   bin_type = 5         ! 0 = Every pt, 1 = x direction, 2 = latitude, ....
   cv_options = 5
   lat_min = -90.0
   lat_max = 90.0
   binwidth_lat = 10.0
   hgt_min = 0.0
   hgt_max = 20000.0
   binwidth_hgt = 1000.0
   dat_dir = 'NO_DIRECTORY_SPECIFIED'
   allow_missing_dates = .false.

   call da_get_unit(namelist_unit)

   ! Reading Namelist:            
   open(unit=namelist_unit, file='gen_be_stage1_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage1_nl)
   close(namelist_unit)
   call da_free_unit(namelist_unit)
  
   ! Read domain info from stage 1 since we skipped stage 2
   call da_get_unit(iunit)
   filename = trim(dat_dir)//'/pert.'//start_date(1:10)//'.e001'
   open (iunit, file = trim(filename), form='unformatted')
   read(iunit)date, ni, nj, nk
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

end subroutine da_readwrite_be_stage1


