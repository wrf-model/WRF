program gen_be_stage4_global

   use da_control, only : stderr, stdout, filename_len, pi, gaussian_lats
   use da_be_spectral, only : da_vv_to_v_spectral, da_initialize_h, &
      da_calc_power
   use da_tools_serial, only : da_get_unit,da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*2         :: ck                         ! Loop index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: num_states                 ! Number of data times.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: k, member, n               ! Loop counters.
   integer             :: ne                         ! Number of ensemble members.
   integer             :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer             :: inc                        ! Jump between elements of vector in array.
   integer             :: lenr                       ! FFT array dimension (at least inc*(n-1)+1).
   integer             :: lensav                     ! wsave dimension (n+int(log(real(ni)))+4).
   integer             :: lenwrk                     ! Dimension of work array.
   integer             :: alp_size                   ! Ass. Leg. Pol. array size.
   integer             :: r_cvsize                   ! Real control variable array size.

   logical             :: first_time                 ! True if first time through loop.
   logical             :: testing_spectral           ! True if testing spectral transforms.
   real                :: pi_over_180                ! pi / 180
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   real                :: variance                   ! Variance (sum of power spectra) .

   real, allocatable   :: field(:,:)                 ! Gridpoint field to be decomposed.

   real, allocatable   :: lat(:)                     ! Latitude (radians).
   real, allocatable   :: sinlat(:)                  ! sine(latitude).
   real, allocatable   :: coslat(:)                  ! cosine(latitude).
   real, allocatable   :: int_wgts(:)                ! Legendre integration weights.
   real, allocatable   :: lon(:)                     ! Longitude (radians).
   real, allocatable   :: sinlon(:)                  ! sine(longitude).
   real, allocatable   :: coslon(:)                  ! cosine(longitude).
   real, allocatable   :: wsave(:)                   ! Prime values for fast Fourier transform.
   real, allocatable   :: alp(:)                     ! Associated Legendre Polynomial.
   real, allocatable   :: power(:)                   ! Power spectrum (n).
   real, allocatable   :: total_power(:)             ! Total Power spectrum (averaged over time/members).

   real, allocatable   :: rcv(:)                     ! Control variable vector.

   namelist / gen_be_stage4_global_nl / start_date, end_date, interval, variable, gaussian_lats, &
                                        testing_spectral, ne, k

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

   pi_over_180 = pi / 180.0

   write(unit=6,fmt='(a/)') "[1] Read in 2D perturbation fields for variable , variable"

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   gaussian_lats = .false.
   ne = 1
   k = 1

   open(unit=namelist_unit, file='gen_be_stage4_global_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage4_global_nl)
   close(namelist_unit)

   write(ck,'(i2)')k
   if ( k < 10 ) ck = '0'//ck(2:2)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(unit=6,fmt='(4a)') 'Computing horizontal power spectra for period' , start_date, 'to' , end_date
   write(unit=6,fmt='(a,i8,a)') 'Interval between dates =' , interval,' hours'
   write(unit=6,fmt='(a,i8)') 'Number of ensemble members at each time =', ne
   write(unit=6,fmt='(3a,i4)') 'Variable' , variable,' at level' , k

   date = start_date
   cdate = sdate
   first_time = .true.
   num_states = 1

   do while ( cdate <= edate )
      do member = 1, ne
         write(ce,'(i3.3)')member

         ! write(6,(5a,i4))    Calculate spectra for date , date, , variable , trim(variable), &
         !    and member , member

         ! Read in data for given variable/level/time/member:

         filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
         filename = trim(filename)//'.e'//ce//'.'//ck
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk ! nk not used.

         if ( first_time ) then
            write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
            allocate( field(1:ni,1:nj) )
         end if
         read(iunit)field
         close(iunit)

         if ( first_time ) then

            write(unit=6,fmt='(a)') "Initialize spectral transforms"

            inc = 1
            lenr = inc * (ni - 1 ) + 1
            lensav = ni + int(log(real(ni))) + 4
            lenwrk = ni

            allocate( lat(1:nj) )
            allocate( sinlat(1:nj) )
            allocate( coslat(1:nj) )
            allocate( int_wgts(1:nj) )
            allocate( lon(1:ni) )
            allocate( sinlon(1:ni) )
            allocate( coslon(1:ni) )
            allocate( wsave(1:lensav) )

            max_wavenumber =  ni / 2 - 1
            allocate ( power(0:max_wavenumber) )
            allocate ( total_power(0:max_wavenumber) )
            total_power(:) = 0.0

            alp_size = ( nj + 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 4
            allocate( alp( 1:alp_size) )

            call da_initialize_h( ni, nj, max_wavenumber, lensav, alp_size, &
                                  wsave, lon, sinlon, coslon, lat, sinlat, coslat, &
                                  int_wgts, alp )

            r_cvsize = ( max_wavenumber + 1 ) * ( max_wavenumber + 2 )
            allocate( rcv( 1:r_cvsize) )

            ! Test horizontal transforms:
            ! if ( testing_spectral ) then
            !    call da_test_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
            !       alp_size, r_cvsize, alp, wsave, int_wgts, field )
            ! end if
            first_time = .false.
         end if

         write(unit=6,fmt='(a)') "Perform gridpoint to spectral decomposition"

         call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                                   alp_size, r_cvsize, alp, wsave, int_wgts, rcv, field )

         write(unit=6,fmt='(a)') "Calculate power spectra"

         call da_calc_power( max_wavenumber, r_cvsize, rcv, power )

         coeffa = 1.0 / real(num_states)
         coeffb = real(num_states-1) * coeffa

         do n = 0, max_wavenumber
            total_power(n) = total_power(n) * coeffb + power(n) * coeffa
            ! write(6,(2i4,2f18.6))num_states, n, power(n), total_power(n)
         end do

         num_states = num_states + 1
      end do  ! End loop over ensemble members.

      ! Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   end do     ! End loop over times.

   variance = sum( total_power(0:max_wavenumber) )   
   write(6,'(3a,i2,a,1pe15.5)')' Variable = ', trim(variable), ', Vertical Index = ', &
                                k, ', Variance = ', variance

   filename = trim(variable)//'/'//trim(variable)
   filename = trim(filename)//'.'//ck//'.spectrum'
   open (ounit, file = filename, form='unformatted')
   write(ounit)variable
   write(ounit)max_wavenumber, k
   write(ounit)total_power

end program gen_be_stage4_global
