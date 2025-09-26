program gen_be_stage1
!
!---------------------------------------------------------------------- 
! Purpose : to remove the binned mean from the perturbation fields.
!
! Input   : binary files: "pert.ccyymmddhh.e"ce for ENS or
!                         "pert.ccyymmddhh.e001" for NMC.
!
! Output : binary files for use of the gen_be_stage2:
!
!----------------------------------------------------------------------
!
   use da_control, only : stderr, stdout, filename_len
   use da_gen_be, only : da_create_bins
   use da_reporting, only : da_error
   use da_tools_serial, only : da_get_unit,da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates (ccyymmddhh).
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name.
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*3         :: ce                         ! Ensemble member index.
   character(len=filename_len)        :: dat_dir                    ! Input data directory.
   character(len=filename_len)        :: filename                   ! Input filename.
   integer             :: count                      ! Counter.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: member                     ! Loop counter
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Interval between file times (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   integer             :: cv_options                 ! Control variable option
   integer             :: ios                        ! I/O status for file read
   real                :: count_inv                  ! 1 / count.
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   logical             :: allow_missing_dates        ! If data from stage 0 is not contiguous, attempt to continue
   logical             :: first_time                 ! True if first file.

   real, allocatable   :: ps_prime(:,:)              ! Surface pressure perturbation.
   real, allocatable   :: t_prime(:,:,:)             ! Temperature perturbation.
   real, allocatable   :: psi_prime(:,:,:)           ! Streamfunction perturbation.
   real, allocatable   :: chi_prime(:,:,:)           ! Velocity Potential perturbation.
   real, allocatable   :: u_prime(:,:,:)             ! U-wind perturbation.
   real, allocatable   :: v_prime(:,:,:)             ! V-wind perturbation.
   real, allocatable   :: rh_prime(:,:,:)            ! Relative Humidity Perturbation.
   real, allocatable   :: height(:,:,:)              ! Geopotential height.
   real, allocatable   :: latitude(:,:)              ! Latitude (radians)
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   real, allocatable   :: psi_mean(:,:,:)            ! Mean field.
   real, allocatable   :: chi_mean(:,:,:)            ! Mean field.
   real, allocatable   :: u_mean(:,:,:)              ! Mean field.
   real, allocatable   :: v_mean(:,:,:)              ! Mean field.
   real, allocatable   :: t_mean(:,:,:)              ! Mean field.
   real, allocatable   :: rh_mean(:,:,:)             ! Mean field.
   real, allocatable   :: ps_mean(:,:)               ! Mean field.

   namelist / gen_be_stage1_nl / start_date, end_date, interval, &
                                 be_method, ne, bin_type, cv_options, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, &
                                 dat_dir, allow_missing_dates

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

   write(unit=stdout,fmt='(a)') &
      ' [1] Initialize namelist variables and other scalars.'

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

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
   dat_dir = '/data2/hcshin/youn/DIFF63'
   allow_missing_dates = .false.

   open(unit=namelist_unit, file='gen_be_stage1_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage1_nl)
   close(namelist_unit)

   if ( be_method /= "ENS" ) ne = 1

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate
   count = 0

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields from standard files, and calculate mean fields'
!---------------------------------------------------------------------------------------------

   first_time = .true.
   do while ( cdate <= edate )
      do member = 1, ne
         count = count + 1
         count_inv = 1.0 / real(count)

         write(6,'(a,a)')'    Processing data for date ', date

         if ( be_method == 'NMC' ) then
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e001'
         else
            write(UNIT=ce,FMT='(i3.3)')member
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         endif

         open (iunit, file = trim(filename), form = 'unformatted')
         read(iunit, iostat = ios)date, ni, nj, nk

         if ( ios /= 0 ) then
            if (allow_missing_dates) then
               write(stdout,'(a,a)')' WARNING: CAN NOT OPEN ',filename
               write(stdout,'(a)')' Attempting to continue since allow_missing_dates = .true.'
               count = count - 1
               cycle
            else
               call da_error(__FILE__,__LINE__,(/"Could not open "//trim(filename)/))
            endif
         endif

         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( ps_prime(1:ni,1:nj) )
            allocate( t_prime(1:ni,1:nj,1:nk) )
            if ( cv_options == 7 ) then
               allocate( u_prime(1:ni,1:nj,1:nk) )
               allocate( v_prime(1:ni,1:nj,1:nk) )
               allocate( u_mean(1:ni,1:nj,1:nk) )
               allocate( v_mean(1:ni,1:nj,1:nk) )
               u_mean(:,:,:) = 0.0
               v_mean(:,:,:) = 0.0
            else
               allocate( psi_prime(1:ni,1:nj,1:nk) )
               allocate( chi_prime(1:ni,1:nj,1:nk) )
               allocate( psi_mean(1:ni,1:nj,1:nk) )
               allocate( chi_mean(1:ni,1:nj,1:nk) )
               psi_mean(:,:,:) = 0.0
               chi_mean(:,:,:) = 0.0
            end if
            allocate( rh_prime(1:ni,1:nj,1:nk) )
            allocate( height(1:ni,1:nj,1:nk) )
            allocate( latitude(1:ni,1:nj) )
            allocate( t_mean(1:ni,1:nj,1:nk) )
            allocate( rh_mean(1:ni,1:nj,1:nk) )
            allocate( ps_mean(1:ni,1:nj) )
            allocate( bin(1:ni,1:nj,1:nk) )
            allocate( bin2d(1:ni,1:nj) )
            t_mean(:,:,:) = 0.0
            rh_mean(:,:,:) = 0.0
            ps_mean(:,:) = 0.0

         end if

         if ( cv_options == 7 ) then
            read(iunit)u_prime
            read(iunit)v_prime
         else
            read(iunit)psi_prime
            read(iunit)chi_prime
         end if
         read(iunit)t_prime
         read(iunit)rh_prime
         read(iunit)ps_prime
         read(iunit)height
         read(iunit)latitude
         close(iunit)


         if ( first_time ) then
            call da_create_bins( ni, nj, nk, bin_type, num_bins, num_bins2d, bin, bin2d, &
                                 lat_min, lat_max, binwidth_lat, &
                                 hgt_min, hgt_max, binwidth_hgt, latitude, height )
         end if

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Calculate time/ensemble mean.
!---------------------------------------------------------------------------------------------

         if ( cv_options == 7 ) then
            u_mean = ( real( count-1 ) * u_mean + u_prime ) * count_inv
            v_mean = ( real( count-1 ) * v_mean + v_prime ) * count_inv
         else
            psi_mean = ( real( count-1 ) * psi_mean + psi_prime ) * count_inv
            chi_mean = ( real( count-1 ) * chi_mean + chi_prime ) * count_inv
         end if
         t_mean = ( real( count-1 ) * t_mean + t_prime ) * count_inv
         rh_mean = ( real( count-1 ) * rh_mean + rh_prime ) * count_inv
         ps_mean = ( real( count-1 ) * ps_mean + ps_prime ) * count_inv

         first_time = .false.

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Read fields again, and remove time/ensemble/area mean'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      do member = 1, ne

         write(6,'(a,a)')'    Removing mean for date ', date

         if ( be_method == 'NMC' ) then
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e001'
         else
            write(UNIT=ce,FMT='(i3.3)')member
            filename = trim(dat_dir)//'/pert.'//date(1:10)//'.e'//trim(ce)
         endif

         open (iunit, file = trim(filename), form = 'unformatted')
         read(iunit, iostat = ios)date, ni, nj, nk
         if (ios /= 0) then
            if (allow_missing_dates) then
               write(stdout,'(a,a)')' WARNING: CAN NOT OPEN ',filename
               write(stdout,'(a)')' Attempting to continue since allow_missing_dates = .true.'
               cycle
            else
               call da_error(__FILE__,__LINE__,(/"Could not open "//trim(filename)/))
            endif
         endif
         if ( cv_options == 7 ) then
            read(iunit)u_prime
            read(iunit)v_prime
         else
            read(iunit)psi_prime
            read(iunit)chi_prime
         end if
         read(iunit)t_prime
         read(iunit)rh_prime
         read(iunit)ps_prime
         read(iunit)height
         read(iunit)latitude
         close(iunit)

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Remove mean.
!---------------------------------------------------------------------------------------------

         if ( cv_options == 7 ) then
            u_prime = u_prime - u_mean
            v_prime = v_prime - v_mean
         else
            psi_prime = psi_prime - psi_mean
            chi_prime = chi_prime - chi_mean
         end if
         t_prime = t_prime - t_mean
         rh_prime = rh_prime - rh_mean
         ps_prime = ps_prime - ps_mean

!---------------------------------------------------------------------------------------------
!        write(6,(2a)) [2] Write fields to file for further processing.
!---------------------------------------------------------------------------------------------

         write(ce,'(i3.3)')member

!        Write necessary full-fields:
         variable = 'fullflds'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)latitude
         write(ounit)height
         close(ounit)

         if ( cv_options == 7 ) then
!           Write u:
            variable = 'u'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, nk
            write(ounit)u_prime
            close(ounit)

!           Write v:
            variable = 'v'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, nk
            write(ounit)v_prime
            close(ounit)
         else
!           Write psi:
            variable = 'psi'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, nk
            write(ounit)psi_prime
            close(ounit)

!           Write chi:
            variable = 'chi'
            filename = trim(variable)//'/'//date(1:10)
            filename = trim(filename)//'.'//trim(variable)//'.e'//ce
            open (ounit, file = filename, form='unformatted')
            write(ounit)ni, nj, nk
            write(ounit)chi_prime
            close(ounit)
         end if

!        Write T:
         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)t_prime
         close(ounit)

!        Write RH:
         variable = 'rh'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)rh_prime
         close(ounit)

!        Write ps:
         variable = 'ps' ! 2D field
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit)ps_prime
         close(ounit)

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

!  Finally, write bin info:

   filename = 'bin.data'
   open (ounit, file = filename, form='unformatted')
   write(ounit)bin_type
   write(ounit)lat_min, lat_max, binwidth_lat
   write(ounit)hgt_min, hgt_max, binwidth_hgt
   write(ounit)num_bins, num_bins2d
   write(ounit)bin(1:ni,1:nj,1:nk)
   write(ounit)bin2d(1:ni,1:nj)
   close(ounit)

end program gen_be_stage1
