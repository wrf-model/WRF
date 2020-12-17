program gen_be_hist
!------------------------------------------------------------------------
!  Purpose: Computes frequency distributiopn of BE statistics
!  Auothor: Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
!           & Monika Krysta, (CTBTO, Vienna, Austria)
!
!  Note: Please acknowledge author/institute in work that uses this code.
!------------------------------------------------------------------------

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit,da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: dat_dir     ! Input data directory.
   character(len=filename_len)        :: filename    ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk, nkdum          ! Grid dimensions.
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
   logical             :: first_time                 ! True if first file.
   real, allocatable   :: field(:,:,:)               ! Field 
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: hist(:,:)                  ! Binned error values
   real, allocatable   :: var_field(:)               ! variance
   real, allocatable   :: stdev_field(:)               ! standard deviation
   real, allocatable   :: class_hist(:,:)            ! Binned error values
   integer, allocatable   :: rain_class(:,:)         ! 2D rain class of perturbation
   character*10           :: rainclvar               ! Variable name 
   integer                :: Nstdev, N_dim_hist      ! Histogram parameters
   integer             :: intcl                      ! Histogram loop counter

   namelist / gen_be_hist_nl / start_date, end_date, interval, &
                                ne, variable, Nstdev, N_dim_hist

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
   variable = 'psi'
   dat_dir = '/mmmtmp1/dmbarker'
   Nstdev = 5
   N_dim_hist = 20

   open(unit=namelist_unit, file='gen_be_hist_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_hist_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(2a)')' Computing error histogram for field ', variable
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne
   write(6,'(2(a,i8))')' Parameters of the histogram Nstdev = ', Nstdev,&
        ' and N_dim_hist = ',N_dim_hist

   date = start_date
   cdate = sdate

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Accumulate values of errors per bin and vert. level'
!--------------------------------------------------------------------------------------------- 
   first_time = .true.

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Processing data for date ', date

      do member = 1, ne

         write(ce,'(i3.3)')member

!        Read field:
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
   
        if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            allocate( field(1:ni,1:nj,1:nk) )
!           Read bin info:
            filename = 'bin.data'
            open (iunit+1, file = filename, form='unformatted')
            read (iunit+1) bin_type
            read (iunit+1) lat_min, lat_max, binwidth_lat
            read (iunit+1) hgt_min, hgt_max, binwidth_hgt
            read (iunit+1) num_bins, num_bins2d
            read (iunit+1) bin(1:ni,1:nj,1:nk)
            read (iunit+1) bin2d(1:ni,1:nj)
            close(iunit+1)
         end if

         read(iunit)field
         close(iunit)

         if ( first_time ) then
            write(6,'(a)')' Setup of histogram parameters'
            allocate( class_hist(1:nk,1:N_dim_hist) )
            allocate( var_field(1:nk) )
            allocate( stdev_field(1:nk) )

            class_hist  = 0.0
            var_field   = 0.0
            stdev_field = 0.0

            ! finds out global stdev of first field to set up dimensions
            do k=1, nk
               var_field(k)   = 1.0/real(ni*nj-1.0)*sum(field(1:ni,1:nj,k)**2) &
                    -real(ni*nj)/real(ni*nj-1.0)*(sum(field(1:ni,1:nj,k))/real(ni*nj))**2
               stdev_field(k) = sqrt(var_field(k))
               write(6,'(a,i4,3(a,e13.5))')' var_field(',k,')=  ', var_field(k),&
                    ' lower=',-Nstdev*stdev_field(k),' upper=',Nstdev*stdev_field(k)
               do i=1,N_dim_hist
                  class_hist(k,i)=-Nstdev*stdev_field(k)+2*Nstdev*stdev_field(k)*real(i-1)/real(N_dim_hist-1)
!                  write(6,'(2(a,i4),a,e13.5') '       class_hist(',k,',',i,')=',class_hist(k,i)
               end do
            end do

            ! allocate histogram
            allocate( hist(1:N_dim_hist,1:num_bins))
            hist(:,:) = 0
            !--ym-- 2D rain class of perturbation    
            if (bin_type==7) allocate( rain_class(1:ni,1:nj) )
            !--ym-- 2D rain class of perturbation    
            first_time = .false.
         end if

         !--ym-- 2D rain class of perturbation     
         if (bin_type==7) then
            !        Read rain_class:
            rainclvar = 'raincl'
            filename = trim(rainclvar)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(rainclvar)//'.e'//ce//'.01'
            open (iunit, file = filename, form='unformatted')
            read(iunit)ni, nj, nkdum
            read(iunit)rain_class
            close(iunit)
  
            ! bin depends on rain class of perturbation!
            ! re-read bin info:
            filename = 'bin.data'
            open (iunit+1, file = filename, form='unformatted')
            read(iunit+1)bin_type
            read(iunit+1)lat_min, lat_max, binwidth_lat
            read(iunit+1)hgt_min, hgt_max, binwidth_hgt
            read(iunit+1)num_bins, num_bins2d
            read(iunit+1)bin(1:ni,1:nj,1:nk)
            read(iunit+1)bin2d(1:ni,1:nj)
            close(iunit+1)
            ! update it 2D
            do j = 1, nj
               do i = 1, ni
                  bin2d(i,j)=rain_class(i,j)*num_bins2d/4+bin2d(i,j)
               end do
            end do
            ! update it 3D
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     bin(i,j,k)=rain_class(i,j)*num_bins/4+bin(i,j,k)
                  end do
               end do
            end do
         end if
        !--ym-- 2D rain class of perturbation         
          
!        accumulate values in histogram
         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  ! finds out in which bin calls we fall
                  intcl=int(1+0.5*(N_dim_hist-1.0)*(1+field(i,j,k)/(Nstdev*stdev_field(k))))
                  if (intcl.ge.1 .and. intcl.le.N_dim_hist) then
                     hist(intcl,b)=hist(intcl,b)+1
                  else
                     ! finds out gross errors (to find bug in rh fields)
                     if (abs(field(i,j,k))>50*Nstdev*stdev_field(k)) then
                        write(6,'(3(a,i3),2(a,e20.5e3))') ' WARNING Gross error -> err_field(',i," ",j," ",k,&
                             ") =",field(i,j,k)," whereas stdev =",stdev_field(k)
                     end if
                  end if
               end do
            end do
         end do
      
      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [3] Write out computed histogram'
!--------------------------------------------------------------------------------------------- 

   ! write out results
   filename = 'hist.'//trim(variable)//'.dat'
   open (ounit, file = filename, form='unformatted')
   write(ounit)nk,num_bins,N_dim_hist
   ! split until I do find a working NCL script that reads large binairies
   do k=1,nk
      write(ounit)class_hist(k,:)
   end do
   do b=1,num_bins
      write(ounit)hist(:,b)
   end do
   close(ounit)

end program gen_be_hist

