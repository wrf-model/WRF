program gen_be_cov3d

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit,da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable1                  ! Variable name
   character*10        :: variable2                  ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk                 ! Grid dimensions.
   integer             :: i, j, k, member            ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (3D fields).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: first_time                 ! True if first file.

   real, allocatable   :: field1(:,:,:)              ! Field 1.
   real, allocatable   :: field2(:,:,:)              ! Field 2.
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   real, allocatable   :: covar(:)                   ! Covariance between input fields.
   real, allocatable   :: var(:)                     ! Autocovariance of field.

   namelist / gen_be_cov3d_nl / start_date, end_date, interval, &
                                ne, variable1, variable2

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)


   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   ne = 1
   variable1 = 'psi'
   variable2 = 'chi'

   open(unit=namelist_unit, file='gen_be_cov3d_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_cov3d_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing covariance for fields ', variable1 , ' and ', variable2
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read fields, and calculate correlations'
!--------------------------------------------------------------------------------------------- 

   first_time = .true.

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Processing data for date ', date

      do member = 1, ne

         write(ce,'(i3.3)')member

!        Read first field:
         filename = trim(variable1)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable1)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk

        if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            allocate( field1(1:ni,1:nj,1:nk) )
            allocate( field2(1:ni,1:nj,1:nk) )

!           Read bin info:
            filename = 'bin.data'
            open (iunit+1, file = filename, form='unformatted')
            read(iunit+1)bin_type
            read(iunit+1)lat_min, lat_max, binwidth_lat
            read(iunit+1)hgt_min, hgt_max, binwidth_hgt
            read(iunit+1)num_bins, num_bins2d
            read(iunit+1)bin(1:ni,1:nj,1:nk)
            read(iunit+1)bin2d(1:ni,1:nj)
            close(iunit+1)

            allocate( bin_pts(1:num_bins) )
            allocate( covar(1:num_bins) )
            allocate( var(1:num_bins) )
            bin_pts(:) = 0
            covar(:) = 0.0
            var(:) = 0.0
            first_time = .false.
         end if

         read(iunit)field1
         close(iunit)

!        Read second field:
         filename = trim(variable2)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable2)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)field2
         close(iunit)

!        Calculate covariances:

         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  bin_pts(b) = bin_pts(b) + 1
                  coeffa = 1.0 / real(bin_pts(b))
                  coeffb = real(bin_pts(b)-1) * coeffa
                  covar(b) = coeffb * covar(b) + coeffa * field1(i,j,k) * field2(i,j,k)
                  var(b) = coeffb * var(b) + coeffa * field2(i,j,k) * field2(i,j,k)
               end do
            end do
         end do
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   filename = trim(variable1)//'.'//trim(variable2)//'.dat'
   open (ounit, file = filename, status='unknown')

   do k = 1, nk
      do j = 1, nj
         do i = 1, ni
            b = bin(i,j,k) ! Take value from center of i dimension.
            if ( var(b) /= 0.0 ) then
               write(ounit,'(f16.8)')covar(b) / var(b)
            else
               write(ounit,'(f16.8)')0.0
            end if
         end do
      end do
   end do

end program gen_be_cov3d

