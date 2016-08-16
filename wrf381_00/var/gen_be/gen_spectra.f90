program gen_spectra

   use da_control
   use da_spectral
   use da_tracing

   implicit none

   integer, parameter  :: iunit = 11

   real, parameter     :: tolerance = 1.0e-6

   character(len=filename_len)        :: filename 
   character*10        :: date, cvar, ctime
   integer             :: time, member               ! Loop counters.
   integer             :: i, j, k, n, m              ! Loop counters.
   integer             :: jts, jte                   ! start, end indices.
   integer             :: nt                         ! Number of times to process.
   integer             :: ne                         ! Number of ensemble members.
   integer             :: ni, nj, nk                 ! Dimensions read in.
   integer             :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer             :: index_k, index_start, index_end ! Array indexer.
   integer             :: inc                        ! Jump between elements of vector in array.
   integer             :: lenr                       ! FFT array dimension (at least inc*(n-1)+1).
   integer             :: lensav                     ! wsave dimension (n+int(log(real(ni)))+4).
   integer             :: lenwrk                     ! Dimension of work array.
   integer             :: ier                        ! FFT error flag.
   integer             :: count                      ! Counter
   integer             :: index_m, index_j           ! Array indices.
   integer             :: alp_size                   ! Ass. Leg. Pol. array size.
   integer             :: cv_size                    ! Complex control variable array size.

   logical             :: first_time                 ! True if first time through loop.
   logical             :: testing_spectral           ! Test spectral transform.
   real                :: pi_over_180                ! pi / 180
   real                :: diff_rms                   ! RMS error measure.

   real, allocatable   :: ps_prime(:,:)              ! Surface pressure perturbation.
   real, allocatable   :: t_prime(:,:,:)             ! Temperature perturbation.
   real, allocatable   :: psi_prime(:,:,:)           ! Streamfunction perturbation.
   real, allocatable   :: chi_prime(:,:,:)           ! Velocity Potential perturbation.
   real, allocatable   :: rh_prime(:,:,:)            ! Relative Humidity Perturbation.
   real, allocatable   :: height(:,:,:)              ! Geopotential height.
   real, allocatable   :: latitude(:,:)              ! Latitude (radians)
   real, allocatable   :: field(:,:)                 ! Gridpoint field to be decomposed.
   real, allocatable   :: field_out(:,:)             ! Output test field.

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
   complex, allocatable:: cv(:)                      ! Control variable vector.
   complex, allocatable:: cv_out(:)                  ! Control variable vector copy for tests.

   if (trace_use) call da_trace_init
   if (trace_use) call da_trace_entry("gen_spectra")

   pi = 4.0 * atan(1.0)
   pi_over_180 = pi / 180.0
   gaussian_lats = .false.
   testing_spectral = .true.
   nt = 1
   ne = 1

!---------------------------------------------------------------------------------------------
!  gen_statistics2: Read standard fields, calculate regression coefficients, and create and
!  output "control variables".
!---------------------------------------------------------------------------------------------

!  Loop over times, ensemble members.

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Read in perturbation fields.'
!---------------------------------------------------------------------------------------------

   open (iunit, file='/data2/hcshin/youn/DIFF63/GHMD_DIFF.2004030212_old', form='unformatted')

   read(iunit)date, ni, nj, nk
   write(6,'(a,a10)')' Date = ', date
   write(6,'(a,3i8)')' i, j, k dimensions are ', ni, nj, nk

   allocate( ps_prime(1:ni,1:nj) )
   allocate( t_prime(1:ni,1:nj,1:nk) )
   allocate( psi_prime(1:ni,1:nj,1:nk) )
   allocate( chi_prime(1:ni,1:nj,1:nk) )
   allocate( rh_prime(1:ni,1:nj,1:nk) )
   allocate( height(1:ni,1:nj,1:nk) )
   allocate( latitude(1:ni,1:nj) )

   read(iunit)ps_prime
   read(iunit)t_prime
   read(iunit)psi_prime
   read(iunit)chi_prime
   read(iunit)rh_prime
   read(iunit)height
   read(iunit)latitude ! Not currently used.

   filename = "psi"//"date"//"member"//"mode"
   open (iunit, file=filename, form='unformatted')
   write(iunit)ni, nj
   write(iunit)gaussian_lats
   write(iunit)psi_prime(1:ni,1:nj,1)
   close(iunit)

!  Calculate regression coefficients.

!  Calculate control variables.

!  Output vp(i,j,k) = vp^-1 vs(i,j,k).

!---------------------------------------------------------------------------------------------
!  gen_statistics3: Calculate Vertical Eigenvectors, Eigenvalues for a given variable.
!---------------------------------------------------------------------------------------------

!  Loop-over/distribute control variables.

!  Loop over times, ensemble members.

!  Read control variables

!  Calculate/accumulate Bv(j,k1,k2).

!  End loop over times, ensemble members.

!  Calculate E, L (v,j,k,k).

!  Loop over times, ensemble members:
!  Output v(i,j,m) = vv^-1 vp(i,j,k)

!---------------------------------------------------------------------------------------------
!  gen_statistics4: Calculate Power spectra for given 2D field.
!---------------------------------------------------------------------------------------------

!  Loop over/distribute variables, vertical modes:

!  Loop over times, ensemble members:

   cvar = 'psi'

   first_time = .true.

   do time = 1, nt
      write(ctime,'(i4)')time

      do member = 1, ne
!         write(cmember,'(i4)')member


!        Read 2D fields.
         filename = trim(cvar)//"date"//"member"//"mode"
         open (iunit, file=filename, form='unformatted')
         read(iunit)ni, nj
         read(iunit)gaussian_lats

         if ( first_time ) then ! Initialize
            allocate( field(1:ni,1:nj) )
            read(iunit)field

!---------------------------------------------------------------------------------------------
            write(6,'(a)')' Initialize spectral transforms.'
!---------------------------------------------------------------------------------------------

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
            power(:) = 0.0

            alp_size = ( nj + 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 4
            allocate( alp( 1:alp_size) )

            call da_initialize_h( ni, nj, max_wavenumber, lensav, alp_size, &
                                  wsave, lon, sinlon, coslon, lat, sinlat, coslat, &
                                  int_wgts, alp )

            cv_size = ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 2
            allocate( cv( 1:cv_size) )

!           Test horizontal transforms:
            if ( testing_spectral ) then
               call da_test_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, cv_size, alp, wsave, int_wgts, field )
            end if
            first_time = .false.
         else ! Just read field
            read(iunit)field
         end if

!---------------------------------------------------------------------------------------------
         write(6,'(a)')' Perform gridpoint to spectral decomposition.'
!---------------------------------------------------------------------------------------------
         call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                                   alp_size, cv_size, alp, wsave, int_wgts, field, cv )

!---------------------------------------------------------------------------------------------
         write(6,'(a)')' Calculate power spectra.'
!---------------------------------------------------------------------------------------------

         call da_calc_power( max_wavenumber, cv_size, cv, power )
         do n = 0, max_wavenumber
            write(6,'(i4,1pe15.5)')n, power(n)
         end do

      end do  ! End loop over ensemble members.
   end do     ! End loop over times.

   if (trace_use) call da_trace_exit("gen_spectra")
   if (trace_use) call da_trace_report

end program gen_spectra
