program gen_be_stage1_1dvar
!
!---------------------------------------------------------------------- 
! Purpose : to remove the binned mean from the difference fields.
!
! Inputs
! 1. Standard diff files (as used in 3D-Var).
! 2. q_diff files - containing q' forecast differences.
! 3. sfc_var_diff files containing t2, q2, u10, v10.
! The Point (1,1) for All of data is  south pole and lontitude is 0 degree
!
! Output : binary files for use of the gen_be_stage2_1dvar:
!
!----------------------------------------------------------------------
!
   use da_control, only : filename_len,gaussian_lats
   use da_tools_serial, only : da_get_unit, da_advance_cymdh
   use da_gen_be, only : da_create_bins

   implicit none

   real, parameter     :: t_factor = 1.0             ! Normalization factor.
   real, parameter     :: q_factor = 1000.0          ! Normalization factor.
   real, parameter     :: ps_factor = 0.01           ! Normalization factor.
   real, parameter     :: u_factor = 1.0             ! Normalization factor.

   character*10        :: start_date, end_date       ! Starting and ending dates (ccyymmddhh).
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name.
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*3         :: ce                         ! Ensemble member index.
   character(len=filename_len)        :: dat_dir                    ! Input data directory.
   character*80        :: expt                       ! Experiment ID.
   character(len=filename_len)        :: filename                   ! Input filename.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: member, b, i, j, k         ! Loop counters.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Interval between file times (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   integer             :: bin_pts_total              ! Total number of pts.
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: first_time                 ! True if first file.
   logical             :: remove_mean                ! Remove time/ensemble/area mean.

   real, allocatable   :: t_prime(:,:,:)             ! Temperature Perturbation.
   real, allocatable   :: q_prime(:,:,:)             ! Specific Humidity Perturbation.
   real, allocatable   :: dummy(:,:,:)               ! Dummy field.
   real, allocatable   :: ps_prime(:,:)              ! Surface pressure perturbation.
   real, allocatable   :: t2_prime(:,:)              ! 2m temperature perturbation.
   real, allocatable   :: q2_prime(:,:)              ! 2m humidity perturbation.
   real, allocatable   :: u10_prime(:,:)             ! 10m wind perturbation.
   real, allocatable   :: v10_prime(:,:)             ! 10m wind perturbation.
   real, allocatable   :: height(:,:,:)              ! Geopotential height.
   real, allocatable   :: latitude(:,:)              ! Latitude (radians)
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: t_mean(:)                  ! Mean field in bin.
   real, allocatable   :: q_mean(:)                  ! Mean field in bin.
   real, allocatable   :: ps_mean(:)                 ! Mean field in bin.
   real, allocatable   :: t2_mean(:)                 ! Mean field in bin.
   real, allocatable   :: q2_mean(:)                 ! Mean field in bin.
   real, allocatable   :: u10_mean(:)                ! Mean field in bin.
   real, allocatable   :: v10_mean(:)                ! Mean field in bin.
   real, allocatable   :: t_ms(:)                   ! Mean square field in bin.
   real, allocatable   :: q_ms(:)                   ! Mean square field in bin.
   real, allocatable   :: ps_ms(:)                  ! Mean square field in bin.
   real, allocatable   :: t2_ms(:)                  ! Mean square field in bin.
   real, allocatable   :: q2_ms(:)                  ! Mean square field in bin.
   real, allocatable   :: u10_ms(:)                 ! Mean square field in bin.
   real, allocatable   :: v10_ms(:)                 ! Mean square field in bin.

   namelist / gen_be_stage1_nl / start_date, end_date, interval, &
                                 be_method, ne, bin_type, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, &
                                 remove_mean, gaussian_lats, expt, dat_dir

  integer :: ounit,iunit,namelist_unit

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   be_method = 'NMC'
   ne = 1
   bin_type = 5         ! 0 = Every pt, 1 = x direction, 2 = latitude, ....
   lat_min = -90.0
   lat_max = 90.0
   binwidth_lat = 10.0
   hgt_min = 0.0
   hgt_max = 20000.0
   binwidth_hgt = 1000.0
   remove_mean = .true.
   gaussian_lats = .false.
   expt = 'gen_be_stage1'
   dat_dir = '/data2/hcshin/youn/DIFF63'

   open(unit=namelist_unit, file='gen_be_stage1_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage1_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   first_time = .true.

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields from 1D-Var files, and calculate mean fields'
!---------------------------------------------------------------------------------------------

   do while ( cdate <= edate )
      do member = 1, ne
         if ( member < 10 ) write(ce,'(I1)') member
         if ( member >= 10 .and. member < 100 ) write(ce,'(I2)') member

!        Get t, ps data from "standard" diff files:
         write(6,'(a,a)')'    Processing 1D-Var data for date ', date

         filename = 'diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if ( be_method == 'ENS' ) filename = trim(filename)//'.'//trim(ce)

         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
         read(iunit)date, ni, nj, nk

         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk

            allocate( t_prime(1:ni,1:nj,1:nk) )
            allocate( q_prime(1:ni,1:nj,1:nk) )
            allocate( dummy(1:ni,1:nj,1:nk) )
            allocate( ps_prime(1:ni,1:nj) )
            allocate( t2_prime(1:ni,1:nj) )
            allocate( q2_prime(1:ni,1:nj) )
            allocate( u10_prime(1:ni,1:nj) )
            allocate( v10_prime(1:ni,1:nj) )
            allocate( height(1:ni,1:nj,1:nk) )
            allocate( latitude(1:ni,1:nj) )
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )

            allocate( t_mean(1:num_bins) )
            allocate( q_mean(1:num_bins) )
            allocate( ps_mean(1:num_bins2d) )
            allocate( t2_mean(1:num_bins2d) )
            allocate( q2_mean(1:num_bins2d) )
            allocate( u10_mean(1:num_bins2d) )
            allocate( v10_mean(1:num_bins2d) )
            allocate( t_ms(1:num_bins) )
            allocate( q_ms(1:num_bins) )
            allocate( ps_ms(1:num_bins2d) )
            allocate( t2_ms(1:num_bins2d) )
            allocate( q2_ms(1:num_bins2d) )
            allocate( u10_ms(1:num_bins2d) )
            allocate( v10_ms(1:num_bins2d) )
            allocate( bin_pts(1:num_bins) )
            allocate( bin_pts2d(1:num_bins2d) )
            t_mean(:) = 0.0
            q_mean(:) = 0.0
            ps_mean(:) = 0.0
            t2_mean(:) = 0.0
            q2_mean(:) = 0.0
            u10_mean(:) = 0.0
            v10_mean(:) = 0.0
            t_ms(:) = 0.0
            q_ms(:) = 0.0
            ps_ms(:) = 0.0
            t2_ms(:) = 0.0
            q2_ms(:) = 0.0
            u10_ms(:) = 0.0
            v10_ms(:) = 0.0
            bin_pts(:) = 0
            bin_pts2d(:) = 0

         end if

         read(iunit)dummy ! Read psi
         read(iunit)dummy ! Read chi
         read(iunit)t_prime ! Finally, get to T!
         read(iunit)dummy ! Read rh
         read(iunit)ps_prime
         read(iunit)height
         read(iunit)latitude
         close(iunit)

         if ( first_time ) then
            call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, latitude, height )

!           Write bin info:
            filename = 'bin.data'
            open (ounit, file = filename, form='unformatted')
            write(ounit)bin_type
            write(ounit)lat_min, lat_max, binwidth_lat
            write(ounit)hgt_min, hgt_max, binwidth_hgt
            write(ounit)num_bins, num_bins2d
            write(ounit)bin(1:ni,1:nj,1:nk)
            write(ounit)bin2d(1:ni,1:nj)
            close(ounit)

            first_time = .false.
         end if
!        Get q data from q_diff files:
         filename = 'q_diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if ( be_method == 'ENS' ) filename = trim(filename)//'.'//trim(ce)

         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
         read(iunit)date, ni, nj, nk
         read(iunit)q_prime
         close(iunit)

!        Get surface data from sfc_var_diff files:
         filename = 'sfc_var_diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if ( be_method == 'ENS' ) then
            if ( member < 10 ) write(ce,'(I1)') member
            if ( member >= 10 .and. member < 100 ) write(ce,'(I2)') member
            filename = trim(filename)//'.'//trim(ce)
         endif
         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
         read(iunit)date, ni, nj
         read(iunit)t2_prime
         read(iunit)q2_prime
         read(iunit)u10_prime
         read(iunit)v10_prime
         close(iunit)

!        Apply scaling factors:
         t_prime = t_prime * t_factor
         q_prime = q_prime * q_factor
         ps_prime = ps_prime * ps_factor
         t2_prime = t2_prime * t_factor
         q2_prime = q2_prime * q_factor
         u10_prime = u10_prime * u_factor
         v10_prime = v10_prime * u_factor

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Calculate time/bin mean and mean square.
!---------------------------------------------------------------------------------------------

!        Calculate means of 3D fields:
         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  bin_pts(b) = bin_pts(b) + 1
                  coeffa = 1.0 / real(bin_pts(b))
                  coeffb = real(bin_pts(b)-1) * coeffa
                  t_mean(b) = coeffb * t_mean(b)  + coeffa * t_prime(i,j,k)
                  q_mean(b) = coeffb * q_mean(b)  + coeffa * q_prime(i,j,k)
                  t_ms(b) = coeffb * t_ms(b)  + coeffa * t_prime(i,j,k) * t_prime(i,j,k)
                  q_ms(b) = coeffb * q_ms(b)  + coeffa * q_prime(i,j,k) * q_prime(i,j,k)
               end do
            end do
         end do

!        Calculate means of 2D fields:
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               bin_pts2d(b) = bin_pts2d(b) + 1
               coeffa = 1.0 / real(bin_pts2d(b))
               coeffb = real(bin_pts2d(b)-1) * coeffa
               ps_mean(b) = coeffb * ps_mean(b) + coeffa * ps_prime(i,j)
               t2_mean(b) = coeffb * t2_mean(b) + coeffa * t2_prime(i,j)
               q2_mean(b) = coeffb * q2_mean(b) + coeffa * q2_prime(i,j)
               u10_mean(b) = coeffb * u10_mean(b) + coeffa * u10_prime(i,j)
               v10_mean(b) = coeffb * v10_mean(b) + coeffa * v10_prime(i,j)
               ps_ms(b) = coeffb * ps_ms(b) + coeffa * ps_prime(i,j) * ps_prime(i,j)
               t2_ms(b) = coeffb * t2_ms(b) + coeffa * t2_prime(i,j) * t2_prime(i,j)
               q2_ms(b) = coeffb * q2_ms(b) + coeffa * q2_prime(i,j) * q2_prime(i,j)
               u10_ms(b) = coeffb * u10_ms(b) + coeffa * u10_prime(i,j) * u10_prime(i,j)
               v10_ms(b) = coeffb * v10_ms(b) + coeffa * v10_prime(i,j) * v10_prime(i,j)
            end do
         end do

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   bin_pts_total = sum(bin_pts(1:num_bins))
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms for model level T: ', bin_pts_total, &
                              sum(t_mean(:)) / ( num_bins * t_factor ), &
                              sqrt(sum(t_ms(:)) / num_bins ) / t_factor
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms for model level q: ', bin_pts_total, &
                              sum(q_mean(:)) / ( num_bins * q_factor ), &
                              sqrt(sum(q_ms(:)) / num_bins ) / q_factor

   bin_pts_total = sum(bin_pts2d(1:num_bins2d))
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms for surface pressure: ', bin_pts_total, &
                              sum(ps_mean(:)) / ( num_bins2d * ps_factor ), &
                              sqrt(sum(ps_ms(:)) / num_bins2d ) / ps_factor
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms/factor for 2m Temperature: ', bin_pts_total, &
                              sum(t2_mean(:)) / ( num_bins2d * t_factor ), &
                              sqrt(sum(t2_ms(:)) / num_bins2d ) / t_factor
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms/factor for 2m Humidity: ', bin_pts_total, &
                              sum(q2_mean(:)) / ( num_bins2d * q_factor ), &
                              sqrt(sum(q2_ms(:)) / num_bins2d ) / q_factor
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms/factor for 10m u-wind: ', bin_pts_total, &
                              sum(u10_mean(:)) / ( num_bins2d * u_factor ), &
                              sqrt(sum(u10_ms(:)) / num_bins2d ) / u_factor
   write(6,'(a40,i10,2f18.8)') ' #pts/Mean/ms/factor for 10m v-wind: ', bin_pts_total, &
                              sum(v10_mean(:)) / ( num_bins2d * u_factor ), &
                              sqrt(sum(v10_ms(:)) / num_bins2d ) / u_factor

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields again, and remove time/ensemble/area mean'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      do member = 1, ne
         if ( member < 10 ) write(ce,'(I1)') member
         if ( member >= 10 .and. member < 100 ) write(ce,'(I2)') member

         write(6,'(a,a)')'    Removing mean for date ', date

         filename = 'diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if ( be_method == 'ENS' ) filename = trim(filename)//'.'//trim(ce)
         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
         read(iunit)date, ni, nj, nk
         read(iunit)dummy
         read(iunit)dummy
         read(iunit)t_prime
         read(iunit)dummy
         read(iunit)ps_prime
         close(iunit)

         filename = 'q_diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if ( be_method == 'ENS' ) filename = trim(filename)//'.'//trim(ce)
         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
         read(iunit)date, ni, nj, nk
         read(iunit)q_prime
         close(iunit)

         filename = 'sfc_var_diff.'//date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'_'//date(9:10)//':00:00'
         if ( be_method == 'ENS' ) filename = trim(filename)//'.'//trim(ce)
         open (iunit, file = trim(dat_dir)//'/'//filename, form='unformatted')
         read(iunit)date, ni, nj
         read(iunit)t2_prime
         read(iunit)q2_prime
         read(iunit)u10_prime
         read(iunit)v10_prime
         close(iunit)

!        Apply scaling factors:
         t_prime = t_prime * t_factor
         q_prime = q_prime * q_factor
         ps_prime = ps_prime * ps_factor
         t2_prime = t2_prime * t_factor
         q2_prime = q2_prime * q_factor
         u10_prime = u10_prime * u_factor
         v10_prime = v10_prime * u_factor

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Remove mean.
!---------------------------------------------------------------------------------------------

         if ( remove_mean ) then
!           3D fields:
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     b = bin(i,j,k)
                     t_prime(i,j,k) = t_prime(i,j,k) - t_mean(b)
                     q_prime(i,j,k) = q_prime(i,j,k) - q_mean(b)
                  end do
               end do
            end do

!           2D fields:
            do j = 1, nj
               do i = 1, ni
                  b = bin2d(i,j)
                  ps_prime(i,j) = ps_prime(i,j) - ps_mean(b)
                  t2_prime(i,j) = t2_prime(i,j) - t2_mean(b)
                  q2_prime(i,j) = q2_prime(i,j) - q2_mean(b)
                  u10_prime(i,j) = u10_prime(i,j) - u10_mean(b)
                  v10_prime(i,j) = v10_prime(i,j) - v10_mean(b)
               end do
            end do
         end if

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Write fields to file for further processing.
!---------------------------------------------------------------------------------------------

         write(ce,'(i3.3)')member

!        Write T:
         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)t_prime
         close(ounit)

!        Write q:
         variable = 'q'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)q_prime
         close(ounit)

!        Write ps:
         variable = 'ps' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit).true., .false.
         write(ounit)ps_prime
         close(ounit)

!        Write t2:
         variable = 't2' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit).true., .false.
         write(ounit)t2_prime
         close(ounit)

!        Write q2:
         variable = 'q2' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit).true., .false.
         write(ounit)q2_prime
         close(ounit)

!        Write u10:
         variable = 'u10' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit).true., .false.
         write(ounit)u10_prime
         close(ounit)

!        Write v10:
         variable = 'v10' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit).true., .false.
         write(ounit)v10_prime
         close(ounit)

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   write(6,'(a)')' Unbiased perturbations written out, scaled by the following factors:'
   write(6,'(a,f15.5)')' T scaling factor = ', t_factor
   write(6,'(a,f15.5)')' q scaling factor = ', q_factor
   write(6,'(a,f15.5)')' ps scaling factor = ', ps_factor
   write(6,'(a,f15.5)')' u scaling factor = ', u_factor

end program gen_be_stage1_1dvar

