program gen_be_read_regcoeffs

!-------------------------------------------------------------------------------------------
!  Purpose: Read regression coefficients from BE file and write out in format
!  required for input to gen_be_stage2 (used for ep calculation).
!
!  Owner: Dale Barker
!
!-------------------------------------------------------------------------------------------

   usa da_control, only : filename_len

   implicit none

   integer, parameter  :: unit = 10                  ! I/O unit.

   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: be_method                  ! Be method ('NMC', or 'ENS')
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: bin_type                   ! Type of bin to average over. !!!DALE ADD.
   integer             :: num_bins                   ! Number of 3D bins.
   integer             :: num_bins2d                 ! Number of 2D bins.
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m). !!!DALE ADD..
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).

   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   real, allocatable   :: regcoeff1(:)               ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)             ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)           ! psi/T regression cooefficient.

   stderr = 0
   stdout = 6

   be_method = "ENS" ! Hardwired for now!

!----------------------------------------------------------------------------
!   [1] Read regression coefficients.
!----------------------------------------------------------------------------

   filename = 'gen_be.'//trim(be_method)//'.dat'
   open (unit, file = filename, form='unformatted')

!  Read the dimensions:
   read(unit)ni, nj, nk

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )

!  Read bin info:
   read(unit)bin_type
   read(unit)lat_min, lat_max, binwidth_lat
   read(unit)hgt_min, hgt_max, binwidth_hgt
   read(unit)num_bins, num_bins2d
   read(unit)bin(1:ni,1:nj,1:nk)
   read(unit)bin2d(1:ni,1:nj)

!  Read the regression coefficients:
   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )

   read(unit)regcoeff1
   read(unit)regcoeff2
   read(unit)regcoeff3

   close(unit)

!----------------------------------------------------------------------------
!   [2] Write regression coefficients.
!----------------------------------------------------------------------------

   filename = 'gen_be_stage2.'//trim(be_method)//'.dat'
   open (unit, file = filename, form='unformatted')
   write(unit)ni, nj, nk
   write(unit)num_bins, num_bins2d
   write(unit)regcoeff1
   write(unit)regcoeff2
   write(unit)regcoeff3
   close(unit)

   deallocate( bin )
   deallocate( bin2d )
   deallocate( regcoeff1 )
   deallocate( regcoeff2 )
   deallocate( regcoeff3 )

end program gen_be_read_regcoeffs

