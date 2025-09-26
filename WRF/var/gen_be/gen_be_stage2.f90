program gen_be_stage2

   use da_control, only : stdout, stderr, filename_len
   use da_tools_serial, only : da_get_unit, da_advance_cymdh
   use da_reporting, only : da_error
   use da_gen_be, only : da_eof_decomposition,da_eof_decomposition_test

   implicit none

   real, parameter     :: variance_threshold = 1e-6  ! Percentage of <psi psi> variance discarded.

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk, nkdum          ! Grid dimensions.
   integer             :: i, j, k, member, k2, k3, m ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: mmax                       ! Maximum mode (after variance truncation)..
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   integer             :: ios                        ! I/O status for file read
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   real                :: total_variance             ! Total variance of <psi psi> matrix.
   real                :: cumul_variance             ! Cumulative variance of <psi psi> matrix.
   real                :: summ                       ! Summation dummy.
   logical             :: first_time                 ! True if first file.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: allow_missing_dates        ! If data from stage 1 is not contiguous, attempt to continue

   real, allocatable   :: latitude(:,:)              ! Latitude (degrees, from south).
   real, allocatable   :: height(:,:,:)              ! Height field.
   real, allocatable   :: psi(:,:,:)                 ! psi.
   real, allocatable   :: chi(:,:,:)                 ! chi.
   real, allocatable   :: temp(:,:,:)                ! Temperature.
   real, allocatable   :: ps(:,:)                    ! Surface pressure.
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: covar1(:)                  ! Covariance between input fields.
   real, allocatable   :: covar2(:,:)                ! Covariance between input fields.
   real, allocatable   :: covar3(:,:,:)              ! Covariance between input fields.
   real, allocatable   :: var1(:)                    ! Autocovariance of field.
   real, allocatable   :: var2(:,:,:)                ! Autocovariance of field.
   real, allocatable   :: var2_inv(:,:,:)            ! Inverse Autocovariance of field.

   real, allocatable   :: work(:,:)                  ! EOF work array.
   real*8, allocatable :: evec(:,:)                  ! Gridpoint eigenvectors.
   real*8, allocatable :: eval(:)                    ! Gridpoint sqrt(eigenvalues).
   real, allocatable   :: LamInvET(:,:)              ! ET/sqrt(Eigenvalue).
   real, allocatable   :: regcoeff1(:)               ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)             ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)           ! psi/T regression cooefficient.

   namelist / gen_be_stage2_nl / start_date, end_date, interval, &
                                 ne, testing_eofs, allow_missing_dates

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
   testing_eofs = .true.
   allow_missing_dates = .false.

   open(unit=namelist_unit, file='gen_be_stage2_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage2_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing regression coefficients'
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

!        Read Full-fields:
         variable = 'fullflds'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = trim(filename), form = 'unformatted')
         read(iunit, iostat=ios)ni, nj, nk
         if (ios /= 0) then
            if (allow_missing_dates) then
               write(stdout,'(a,a)')' WARNING: CAN NOT OPEN ',filename
               write(stdout,'(a)')' Attempting to continue since allow_missing_dates = .true.'
               cycle
            else
               call da_error(__FILE__,__LINE__,(/"Could not open "//trim(filename)/))
            endif
         endif
         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( latitude(1:ni,1:nj) )
            allocate( height(1:ni,1:nj,1:nk) )
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            allocate( psi(1:ni,1:nj,1:nk) )
            allocate( chi(1:ni,1:nj,1:nk) )
            allocate( temp(1:ni,1:nj,1:nk) )
            allocate( ps(1:ni,1:nj) )
         end if
         read(iunit)latitude
         read(iunit)height

         close(iunit)

         if ( first_time ) then

!           Read bin info:
            filename = 'bin.data'
            open (iunit, file = filename, form='unformatted')
            read(iunit)bin_type
            read(iunit)lat_min, lat_max, binwidth_lat
            read(iunit)hgt_min, hgt_max, binwidth_hgt
            read(iunit)num_bins, num_bins2d
            read(iunit)bin(1:ni,1:nj,1:nk)
            read(iunit)bin2d(1:ni,1:nj)
            close(iunit)

            allocate( bin_pts(1:num_bins) )
            allocate( bin_pts2d(1:num_bins2d) )
            allocate( covar1(1:num_bins) )
            allocate( covar2(1:nk,1:num_bins2d) )
            allocate( covar3(1:nk,1:nk,1:num_bins2d) )
            allocate( var1(1:num_bins) )
            allocate( var2(1:nk,1:nk,1:num_bins2d) )
            bin_pts(:) = 0
            bin_pts2d(:) = 0
            covar1(:) = 0.0
            covar2(:,:) = 0.0
            covar3(:,:,:) = 0.0
            var1(:) = 0.0
            var2(:,:,:) = 0.0
            first_time = .false.
         end if

!        Read psi:
         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)psi
         close(iunit)

!        Read chi:
         variable = 'chi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)chi
         close(iunit)

!        Read T:
         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)temp
         close(iunit)

!        Read ps:
         variable = 'ps'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)ps
         close(iunit)

!        Calculate psi/chi covariances:

         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  bin_pts(b) = bin_pts(b) + 1
                  coeffa = 1.0 / real(bin_pts(b))
                  coeffb = real(bin_pts(b)-1) * coeffa
                  covar1(b) = coeffb * covar1(b) + coeffa * psi(i,j,k) * chi(i,j,k)
                  var1(b) = coeffb * var1(b) + coeffa * psi(i,j,k) * psi(i,j,k)
               end do
            end do
         end do

!        Calculate psi/ps, and psi/T, and psi/psi covariances:

         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               bin_pts2d(b) = bin_pts2d(b) + 1
               coeffa = 1.0 / real(bin_pts2d(b))
               coeffb = real(bin_pts2d(b)-1) * coeffa
               do k = 1, nk
!                 psi/ps:
                  covar2(k,b) = coeffb * covar2(k,b) + coeffa * ps(i,j) * psi(i,j,k)

!                 psi/T:
                  do k2 = 1, nk
                     covar3(k,k2,b) = coeffb * covar3(k,k2,b) + &
                                      coeffa * temp(i,j,k) * psi(i,j,k2)
                  end do

!                 psi/psi (symmetric):
                  do k2 = 1, k
                     var2(k,k2,b) = coeffb * var2(k,k2,b) + &
                                    coeffa * psi(i,j,k) * psi(i,j,k2)
                  end do
               end do
            end do
         end do

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   deallocate ( latitude ) ! Not needed any more.
   deallocate ( height )   ! Not needed any more.

!  Fill in psi/psi covariance by symmetry:
   do b = 1, num_bins2d
      do k = 1, nk
         do k2 = k+1, nk ! Symmetry.
            var2(k,k2,b) = var2(k2,k,b)
         end do
      end do
   end do

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [3] Calculate eigenvectors, eigenvalues and inverse for psi/psi covariance '
!---------------------------------------------------------------------------------------------

   allocate( work(1:nk,1:nk) )
   allocate( evec(1:nk,1:nk) )
   allocate( eval(1:nk) )
   allocate( LamInvET(1:nk,1:nk) )
   allocate( var2_inv(1:nk,1:nk,1:num_bins2d) )

   do b = 1, num_bins2d   
      LamInvET(:,:) = 0.0
      work(1:nk,1:nk) = var2(1:nk,1:nk,b)
      call da_eof_decomposition( nk, work, evec, eval )

      if ( testing_eofs ) then
         call da_eof_decomposition_test( nk, work, evec, eval )
         testing_eofs = .false.
      end if

!     Truncate eigenvalues to ensure inverse is not dominated by rounding error:
      summ = 0.0
      do m = 1, nk
         summ = summ + eval(m)
      end do
      total_variance = summ

      cumul_variance = 0.0
      mmax = nk
      do m = 1, nk
         cumul_variance = cumul_variance + eval(m) / total_variance
         if ( cumul_variance > 1.0 - variance_threshold ) then
            mmax = m - 1
            exit
         end if
      end do
      write(6,'(2(a,i6),2(a,1pe11.5))') ' Bin = ', b, ', <psipsi> truncation = ', mmax, &
                                        ', Total Variance = ', total_variance, &
                                        ', Condition number = ', eval(1) / eval(nk-1)

!     Lam{-1} . E^T:
      do k = 1, nk
         do m = 1, mmax
            LamInvET(m,k) = evec(k,m) / eval(m)
         end do
      end do

!     <psi psi>^{-1} = E . Lam{-1} . E^T:

      do k = 1, nk
         do k2 = 1, k
            summ = 0.0
            do m = 1, nk
               summ = summ + evec(k,m) * LamInvET(m,k2)
            end do
            var2_inv(k,k2,b) = summ
         end do
      end do

      do k = 1, nk
         do k2 = k+1, nk ! Symmetry.
            var2_inv(k,k2,b) = var2_inv(k2,k,b)
         end do
      end do
   end do
!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [4] Calculate regression coefficients '
!---------------------------------------------------------------------------------------------

   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )

!  psi/chi:
   do b = 1, num_bins
      regcoeff1(b) = covar1(b) / var1(b)
   end do

!  psi/ps:
   do b = 1, num_bins2d
      do k = 1, nk
         summ = 0.0
         do k2 = 1, nk
            summ = summ + covar2(k2,b) * var2_inv(k2,k,b)
         end do
         regcoeff2(k,b) = summ
      end do
   end do

!  psi/T:
   do b = 1, num_bins2d
      do k = 1, nk
         do k2 = 1, nk
            summ = 0.0
            do k3 = 1, nk
               summ = summ + covar3(k,k3,b) * var2_inv(k3,k2,b)
            end do
            regcoeff3(k,k2,b) = summ
         end do
      end do
   end do

!  Output regression coefficients for use in 3/4D-Var:
   filename = 'gen_be_stage2.dat'
   open (ounit, file = filename, form='unformatted')
   write(ounit)ni, nj, nk
   write(ounit)num_bins, num_bins2d
   write(ounit)regcoeff1
   write(ounit)regcoeff2
   write(ounit)regcoeff3
   close(ounit)

   do k = 1, nk
      do j = 1, nj
         b = bin2d(1,j)
         write(61,'(3i6,1pe13.5)')k, j, b, covar2(k,b)
         write(62,'(3i6,1pe13.5)')k, j, b, var2(k,k,b)
         write(63,'(3i6,1pe13.5)')k, j, b, var2_inv(k,k,b)
      end do
   end do

end program gen_be_stage2
