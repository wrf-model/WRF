subroutine da_readwrite_be_stage2(outunit, nk)

   ! ----------------------------------------------------------------------
   ! Purpose: Read and write the dimensions, bin information, and 
   !          regression coefficients.
   !  Update: Multivariate BE option (cv_options=6)
   !          Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
   !
   !  Note: Please acknowledge author/institute in work that uses this code.
   !
   ! ----------------------------------------------------------------------

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(out)     :: nk                         ! Number of vertical levels/modes.
   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   real                :: rf_scale                   ! Recursive filter scale.
   integer             :: num_passes                 ! Recursive filter passes.
 
   character(len=filename_len) :: filename                   ! Input filename.
   character*80             :: variable_psi_chi
   character*80             :: variable_psi_t
   character*80             :: variable_psi_ps
   character*80             :: variable_psi_rh
   character*80             :: variable_chi_u_t
   character*80             :: variable_chi_u_ps
   character*80             :: variable_chi_u_rh
   character*80             :: variable_t_u_rh
   character*80             :: variable_ps_u_rh

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

   real, allocatable   :: regcoeff_psi_chi(:)        ! chi/psi regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)       ! ps/psi regression cooefficient.
   real, allocatable   :: regcoeff_psi_t(:,:,:)      ! t/psi regression cooefficient.
   real, allocatable   :: regcoeff_psi_rh(:,:,:)     ! rh/psi regression cooefficient.

   real, allocatable   :: regcoeff_chi_u_ps(:,:)     ! ps/chi_u regression coefficient
   real, allocatable   :: regcoeff_chi_u_t(:,:,:)    ! t/chi_u regression coefficient
   real, allocatable   :: regcoeff_ps_u_rh(:,:)      ! rh/ps_u regression coefficient
   real, allocatable   :: regcoeff_chi_u_rh(:,:,:)   ! rh/chi_u regression coefficient
   real, allocatable   :: regcoeff_t_u_rh(:,:,:)     ! rh/t_u regression coefficient

   integer :: iunit, namelist_unit

   namelist / gen_be_stage2_nl / start_date, end_date, interval, &
                                 ne, testing_eofs, num_passes, rf_scale, cv_options, allow_missing_dates

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   ne = 1
   testing_eofs = .true.
   num_passes = 0
   rf_scale = 1.0
   cv_options = 5
   allow_missing_dates = .false.

   call da_get_unit(namelist_unit)

   ! Reading Namelist:            
   open(unit=namelist_unit, file='gen_be_stage2_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage2_nl)
   close(namelist_unit)
   call da_free_unit(namelist_unit)
  
   ! Read in the coefficients:

   call da_get_unit(iunit)
   filename = 'gen_be_stage2.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   read(iunit)num_bins, num_bins2d

   allocate( regcoeff_psi_chi(1:num_bins) )
   allocate( regcoeff_psi_ps(1:nk,1:num_bins2d) )
   allocate( regcoeff_psi_t(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff_psi_rh(1:nk,1:nk,1:num_bins2d) )

   allocate( regcoeff_chi_u_ps(1:nk,1:num_bins2d) )
   allocate( regcoeff_chi_u_t(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff_chi_u_rh(1:nk,1:nk,1:num_bins2d) )

   allocate( regcoeff_ps_u_rh(1:nk,1:num_bins2d) )
   allocate( regcoeff_t_u_rh(1:nk,1:nk,1:num_bins2d) )

   if( cv_options == 5) then
   read(iunit)regcoeff_psi_chi
   read(iunit)regcoeff_psi_ps
   read(iunit)regcoeff_psi_t
   end if

   if( cv_options == 7) then
   read(iunit)regcoeff_psi_chi
   read(iunit)regcoeff_psi_ps
   read(iunit)regcoeff_psi_t
   end if

   if( cv_options == 6) then
   read(iunit)variable_psi_chi
   read(iunit)regcoeff_psi_chi

   read(iunit)variable_psi_t
   read(iunit)regcoeff_psi_t

   read(iunit)variable_psi_ps
   read(iunit)regcoeff_psi_ps

   read(iunit)variable_psi_rh
   read(iunit)regcoeff_psi_rh

   read(iunit)variable_chi_u_t
   read(iunit)regcoeff_chi_u_t

   read(iunit)variable_chi_u_ps
   read(iunit)regcoeff_chi_u_ps

   read(iunit)variable_chi_u_rh
   read(iunit)regcoeff_chi_u_rh

   read(iunit)variable_t_u_rh
   read(iunit)regcoeff_t_u_rh

   read(iunit)variable_ps_u_rh
   read(iunit)regcoeff_ps_u_rh

   end if
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

   if( cv_options == 5) then
   write(outunit)regcoeff_psi_chi
   write(outunit)regcoeff_psi_ps
   write(outunit)regcoeff_psi_t
   end if

   if( cv_options == 7) then
   write(outunit)regcoeff_psi_chi
   write(outunit)regcoeff_psi_ps
   write(outunit)regcoeff_psi_t
   end if

   if( cv_options == 6) then
   write(outunit)variable_psi_chi
   write(outunit)regcoeff_psi_chi

   write(outunit)variable_psi_t
   write(outunit)regcoeff_psi_t

   write(outunit)variable_psi_ps
   write(outunit)regcoeff_psi_ps

   write(outunit)variable_psi_rh
   write(outunit)regcoeff_psi_rh

   write(outunit)variable_chi_u_t
   write(outunit)regcoeff_chi_u_t

   write(outunit)variable_chi_u_ps
   write(outunit)regcoeff_chi_u_ps

   write(outunit)variable_chi_u_rh
   write(outunit)regcoeff_chi_u_rh

   write(outunit)variable_t_u_rh
   write(outunit)regcoeff_t_u_rh

   write(outunit)variable_ps_u_rh
   write(outunit)regcoeff_ps_u_rh
   end if

   deallocate(regcoeff_psi_chi)
   deallocate(regcoeff_psi_ps)
   deallocate(regcoeff_psi_t)
   deallocate(regcoeff_psi_rh)

   deallocate( regcoeff_chi_u_ps)
   deallocate( regcoeff_chi_u_t)
   deallocate( regcoeff_chi_u_rh)

   deallocate( regcoeff_ps_u_rh)
   deallocate( regcoeff_t_u_rh)


end subroutine da_readwrite_be_stage2


