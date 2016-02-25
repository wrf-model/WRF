program gen_be_stage0_gsi
!----------------------------------------------------------------------
! Author : Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
! Purpose: Converts WRF-ARW standard output files (wrfout) to  the
!          desired format required by Stage1 of BE statistics for GSI
!
!  Input  : WRF forecasts for a specified date (NETCDF format).
!
!  Output : Binary files for use in Stage1 of GSI  
!
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

   use da_control, only : num_fft_factors, pi, stdout, stderr, &
      filename_len, base_pres, base_temp, base_lapse, nrange
   use da_gen_be, only : da_get_field, da_get_trh, &
      da_stage0_initialize
   use da_tools_serial, only : da_get_unit, da_free_unit,da_find_fft_factors, &
      da_find_fft_trig_funcs
   use module_ffts, only : fft551, fft661
   use aero_mod

   implicit none

   integer :: gen_be_iunit, gen_be_ounit

   character (len=filename_len)   :: filestub                  ! General filename stub.
   character (len=filename_len)   :: input_file                ! Input file. 
   character (len=filename_len)   :: output_file               ! Output file. 
   character (len=10)    :: date                      ! Character date.
   character (len=10)    :: var                       ! Variable to search for.
   character (len=3)     :: be_method                 ! "NMC" or "ENS".
   character (len=3)     :: cne                       ! Ensemble size.
   character (len=3)     :: ce                        ! Member index -> character.

   integer, external     :: iargc
   integer               :: numarg
   integer               :: ne                        ! Ensemble size.
   integer               :: i, j, k, member           ! Loop counters.
   integer               :: dim1                      ! Dimensions of grid (T points).
   integer               :: dim1s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim2                      ! Dimensions of grid (T points).
   integer               :: dim2s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim3                      ! Dimensions of grid (T points).
   integer               :: n1, n2                    ! Padded dimensions (n=dim-1+pad).
   integer               :: n1s, n2s                  ! Padded dimensions (n=dim-1+pad).
   integer               :: poisson_method            ! 1=Spectral, 2=SOR.
   integer               :: fft_method                ! For poisson_method=1: 1=FCT, 2=FST.
   integer               :: ktest                     ! Test level.
   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution in meters.
   real                  :: dim12_inv_u               ! 1 / (dim1*dim2).
   real                  :: dim12_inv_v               ! 1 / (dim1*dim2).
   real                  :: dim12_inv                 ! 1 / (dim1*dim2).
   logical               :: test_inverse              ! Test conversion by performing inverse.

   integer               :: ifax1(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax2(1:num_fft_factors)  ! FFT factors.
   integer               :: ifax1s(1:num_fft_factors) ! FFT factors.
   integer               :: ifax2s(1:num_fft_factors) ! FFT factors.
   real, allocatable     :: xlat(:,:)                 ! Latitude  of mass points.
   real, allocatable     :: xlon(:,:)                 ! Longitude of mass points.
   real, allocatable     :: mapfac_m(:,:)             ! Map factor - mass pts.
   real, allocatable     :: mapfac_mx(:,:)            ! Map factor - mass pts. in X-Dir
   real, allocatable     :: mapfac_my(:,:)            ! Map factor - mass pts. in Y-Dir
   real, allocatable     :: mapfac_u(:,:)             ! Map factor - u points.
   real, allocatable     :: mapfac_v(:,:)             ! Map factor - v points.
   real, allocatable     :: znu(:)                    ! Half sigma levels      

   real, allocatable     :: u(:,:)                    ! u-wind.
   real, allocatable     :: v(:,:)                    ! v-wind.
   real, allocatable     :: div(:,:)                  ! Divergence.
   real, allocatable     :: vor(:,:)                  ! Vorticity.
   real, allocatable     :: psi2d(:,:)                ! Streamfunction copy. 
   real, allocatable     :: chi2d(:,:)                ! Velocity potential copy.
   real, allocatable     :: temp2d(:,:)               ! Temperature.
   real, allocatable     :: rh2d(:,:)                 ! Relative humidity.

   real, allocatable     :: aero2d(:,:)               ! Aerosols 

   real, allocatable     :: trigs1(:)                 ! FFT trig functions.
   real, allocatable     :: trigs2(:)                 ! FFT trig functions.
   real, allocatable     :: fft_coeffs(:,:)           ! FFT coefficients.
   real, allocatable     :: trigs1s(:)                ! FFT trig functions.
   real, allocatable     :: trigs2s(:)                ! FFT trig functions.
   real, allocatable     :: fft_coeffss(:,:)          ! FFT coefficients.

!  Standard fields:
   real, allocatable     :: psi(:,:,:)                ! Streamfunction.
   real, allocatable     :: chi(:,:,:)                ! Velocity Potential.
   real, allocatable     :: temp(:,:,:)               ! Temperature.
   real, allocatable     :: rh(:,:,:)                 ! Relative humidity.
   real, allocatable     :: rhm(:,:,:)                ! Mean Relative humidity.
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   real, allocatable     :: psi_mean(:,:,:)           ! Streamfunction.
   real, allocatable     :: chi_mean(:,:,:)           ! Velocity Potential.
   real, allocatable     :: temp_mean(:,:,:)          ! Temperature.
   real, allocatable     :: rh_mean(:,:,:)            ! Relative humidity.
   real, allocatable     :: rhm_mean(:,:,:)           ! Mean Relative humidity.
   real, allocatable     :: psfc_mean(:,:)            ! Surface pressure.
   real, allocatable     :: tmp2d(:,:)                ! Scratch 2d-array  

   real, allocatable     :: aero(:,:,:,:)             ! Aerosols
   real, allocatable     :: aero_mean(:,:,:,:)        ! Aerosols

   integer               :: jj, num_aeros
   integer, parameter    :: num_aeros_max = 200
   character (len=40)    :: aeros_to_process(1:num_aeros_max)
   logical               :: process_aero

   call get_aero_info(process_aero,aeros_to_process,num_aeros)

   stderr = 0
   stdout = 6

   write(6,'(/a)')' [1] Initialize information.'

   call da_get_unit(gen_be_iunit)
   call da_get_unit(gen_be_ounit)

   test_inverse = .true. 
   ktest = 1
   poisson_method = 1    
   fft_method = 2
   numarg = iargc()
   if ( numarg /= 4 )then
      write(UNIT=6,FMT='(a)') &
        "Usage: gen_be_stage0_wrf be_method date ne <wrf_file_stub> Stop"
      stop
   end if
   
   ! Initialise to stop false Cray compiler warnings
   be_method=""
   date=""
   cne=""
   filestub=""

   call getarg( 1, be_method )
   call getarg( 2, date )
   call getarg( 3, cne )
   read(cne,'(i3)')ne
   call getarg( 4, filestub )

   if ( be_method == "NMC" ) then
      write(6,'(a,a)')' Computing gen_be NMC-method forecast difference files for date ', date
      ne = 2                      ! NMC-method uses differences between 2 forecasts.
   else if ( be_method == "ENS" ) then
      write(6,'(a,a)')' Computing gen_be ensemble perturbation files for date ', date
      write(6,'(a,i4)')' Ensemble Size = ', ne
   else
      write(6,'(a,a)')trim(be_method), ' is an invalid value of be_method. Stop'
      stop
   end if
   write(6,'(a,a)')' Input filestub = ', trim(filestub)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Setup up ancillary fields using 1st member values.' 
!---------------------------------------------------------------------------------------------

   var = "T"
   input_file = trim(filestub)//'.e001'
   call da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds )
   dim1s = dim1+1 ! Vorticity/streamfunction array 1 larger.
   dim2s = dim2+1 ! Vorticity/streamfunction array 1 larger.
   dim12_inv_u = 1.0 / real((dim1+1) * dim2)
   dim12_inv_v = 1.0 / real(dim1 * (dim2+1))
   dim12_inv = 1.0 / real(dim1 * dim2)

!  Allocate arrays in output fields:
   allocate( psi(1:dim1,1:dim2,1:dim3) ) 
   allocate( chi(1:dim1,1:dim2,1:dim3) )
   allocate( temp(1:dim1,1:dim2,1:dim3) )
   allocate( rh(1:dim1,1:dim2,1:dim3) )
   allocate( rhm(1:dim1,1:dim2,1:dim3) )
   allocate( psfc(1:dim1,1:dim2) )

   allocate( psi_mean(1:dim1,1:dim2,1:dim3) ) 
   allocate( chi_mean(1:dim1,1:dim2,1:dim3) )
   allocate( temp_mean(1:dim1,1:dim2,1:dim3) )
   allocate( rh_mean(1:dim1,1:dim2,1:dim3) )
   allocate( rhm_mean(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_mean(1:dim1,1:dim2) )

   if ( process_aero ) then
      allocate( aero(1:num_aeros,1:dim1,1:dim2,1:dim3) )
      allocate( aero_mean(1:num_aeros,1:dim1,1:dim2,1:dim3) )
      aero_mean = 0.0
   end if

   psi_mean = 0.0
   chi_mean = 0.0
   temp_mean = 0.0
   rh_mean = 0.0
   rhm_mean = 0.0
   psfc_mean = 0.0

   allocate( xlat(1:dim1,1:dim2) )
   allocate( xlon(1:dim1,1:dim2) )
   allocate( mapfac_m(1:dim1,1:dim2) )
   allocate( mapfac_mx(1:dim1,1:dim2) )
   allocate( mapfac_my(1:dim1,1:dim2) )
   allocate( mapfac_u(1:dim1s,1:dim2) )
   allocate( mapfac_v(1:dim1,1:dim2s) )
   allocate( znu(1:dim3) )

   var = "XLAT"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, xlat )
   var = "XLONG"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, xlon )
   var = "MAPFAC_M"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, mapfac_m )
   var = "MAPFAC_MX"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, mapfac_mx )
   var = "MAPFAC_MY"
   call da_get_field( input_file, var, 2, dim1, dim2, 1, 1, mapfac_my )
   var = "MAPFAC_U"
   call da_get_field( input_file, var, 2, dim1+1, dim2, 1, 1, mapfac_u )
   var = "MAPFAC_V"
   call da_get_field( input_file, var, 2, dim1, dim2+1, 1, 1, mapfac_v )
   var = "ZNU"
   allocate( tmp2d(1:dim3,1:1) )
   call da_get_field( input_file, var, 1, dim3, 1, 1, 1, tmp2d )
   znu(:)=tmp2d(:,1)
   deallocate( tmp2d )

!  Initialize FFT coefficients:
   if ( poisson_method == 1 ) then
      call da_fft_initialize1( dim1, dim2, n1, n2, ifax1, ifax2 )
      call da_fft_initialize1( dim1s, dim2s, n1s, n2s, ifax1s, ifax2s )

      allocate( trigs1(1:3*n1) )
      allocate( trigs2(1:3*n2) )
      allocate( fft_coeffs(1:n1+1,1:n2+1) )
      call da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )
      allocate( trigs1s(1:3*n1s) )
      allocate( trigs2s(1:3*n2s) )
      allocate( fft_coeffss(1:n1s+1,1:n2s+1) )
      call da_fft_initialize2( n1s, n2s, ds, trigs1s, trigs2s, fft_coeffss )
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [3] Convert WRF forecast fields to standard fields and output' 
!---------------------------------------------------------------------------------------------

   ! Allocate temporary arrays:
   allocate( u(1:dim1s,1:dim2) )
   allocate( v(1:dim1,1:dim2s) )
   allocate( vor(1:dim1s,1:dim2s) )
   allocate( div(1:dim1,1:dim2) )
   allocate( psi2d(1:dim1s,1:dim2s) )
   allocate( chi2d(1:dim1,1:dim2) )
   allocate( temp2d(1:dim1,1:dim2) )
   allocate( rh2d(1:dim1,1:dim2) )

   if ( process_aero ) allocate( aero2d( 1:dim1,1:dim2) ) 

   do member = 1, ne

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(filestub)//'.e'//ce  

      do k = 1, dim3

         ! Read u, v:
         var = "U"
         call da_get_field( input_file, var, 3, dim1s, dim2, dim3, k, u )
         var = "V"
         call da_get_field( input_file, var, 3, dim1, dim2s, dim3, k, v )

         ! Calculate vorticity (in center of mass grid on WRF's Arakawa C-grid):
         call da_uv_to_vor_c( dim1, dim2, ds, &
                              mapfac_m, mapfac_u, mapfac_v, u, v, vor )

         ! Calculate divergence (at mass pts. on WRF's Arakawa C-grid):

         call da_uv_to_div_c( dim1, dim2, ds, &
                              mapfac_m, mapfac_u, mapfac_v, u, v, div )

         ! Calculate streamfunction and potential 
         ! Assumes vor/div converted to Del**2 psi/chi):

         if ( poisson_method == 1 ) then
            call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, &
                                trigs1s, trigs2s, fft_coeffss, vor, psi2d )
            call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, &
                                trigs1, trigs2, fft_coeffs, div, chi2d )
         else if ( poisson_method == 2 ) then
            call da_sor( dim1s, dim2s, ds, vor, psi2d )
            call da_sor( dim1, dim2, ds, div, chi2d )
         end if

         if ( test_inverse .and. k == ktest .and. member == 1 ) then
            call da_test_inverse( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                                  n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                                  n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                                  u, v, psi2d, chi2d )
         end if

!        Interpolate psi to mass pts ready for output:
         do j = 1, dim2
            do i = 2, dim1
               psi(i,j,k) = 0.25 * ( psi2d(i,j) + psi2d(i+1,j) + &
                                     psi2d(i,j+1) + psi2d(i+1,j+1) )
            end do
         end do

         chi(:,:,k) = chi2d(:,:)
!        Read mass fields, and convert to T and rh:

         call da_get_trh( input_file, dim1, dim2, dim3, k, temp2d, rh2d )
         temp(:,:,k) = temp2d(:,:)
         rh(:,:,k) = 0.01 * rh2d(:,:) ! *0.01 to conform with WRF-Var units.

         if ( process_aero ) then
             do jj = 1,num_aeros
                !write(unit=stdout,fmt='(a)') trim(aeros_to_process(jj))
                var = trim( adjustl(aeros_to_process(jj) ) )
                call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, aero2d)
                aero(jj,:,:,k) = aero2d
                if ( var.eq.'SULF' .or. var.eq.'sulf') then
                   !write(unit=stdout,fmt='(a)') 'converting SULF from ppmv to micro-gram/kg'
                   aero(jj,:,:,k) = aero2d*(96.06/28.964)*1000.0
                end if
             end do
         end if

      end do  ! end loop over dim3 (loop over "k")

!     Finally, extract surface pressure:
      var = "PSFC"
      call da_get_field( input_file, var, 2, dim1, dim2, dim3, 1, psfc )
! convert psfc in centibar     
      psfc = psfc*0.001

!     Write out ensemble forecasts for this member:
      output_file = 'tmp.e'//ce  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3
      write(gen_be_ounit)psi
      write(gen_be_ounit)chi
      write(gen_be_ounit)temp
      write(gen_be_ounit)rh
      write(gen_be_ounit)psfc
      if ( process_aero ) write(gen_be_ounit) aero
      close(gen_be_ounit)

   if ( be_method == "ENS" ) then
!     Calculate accumulating mean:
      member_inv = 1.0 / real(member)

      psi_mean = ( real( member-1 ) * psi_mean + psi ) * member_inv
      chi_mean = ( real( member-1 ) * chi_mean + chi ) * member_inv
      temp_mean = ( real( member-1 ) * temp_mean + temp ) * member_inv
      rh_mean = ( real( member-1 ) * rh_mean + rh ) * member_inv
      rhm_mean = ( real( member-1 ) * rhm_mean + rh ) * member_inv
      psfc_mean = ( real( member-1 ) * psfc_mean + psfc ) * member_inv
   end if

   end do

   deallocate( u )
   deallocate( v )
   deallocate( vor )
   deallocate( div )
   deallocate( psi2d )
   deallocate( chi2d )
   deallocate( temp2d )
   deallocate( rh2d )
   if ( process_aero ) deallocate(aero2d)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [4] Compute perturbations and output' 
!---------------------------------------------------------------------------------------------

   if ( be_method == "NMC" ) then
      write(6,'(/a)')'    Compute perturbation as a difference between two forecasts' 

!     Re-read input forecast standard fields (ne=2 hard-wired above for NMC-method):
      input_file = 'tmp.e001'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)psi
      read(gen_be_iunit)chi
      read(gen_be_iunit)temp
      read(gen_be_iunit)rh
      read(gen_be_iunit)psfc
      if ( process_aero ) read(gen_be_iunit) aero
      close(gen_be_iunit)
      call da_free_unit(gen_be_iunit)

      input_file = 'tmp.e002'
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)psi_mean
      read(gen_be_iunit)chi_mean
      read(gen_be_iunit)temp_mean
      read(gen_be_iunit)rh_mean
      read(gen_be_iunit)psfc_mean
      if ( process_aero ) read(gen_be_iunit) aero_mean
      close(gen_be_iunit)

      psi = psi - psi_mean
      chi = chi - chi_mean
      temp = temp - temp_mean
      rhm = (rh + rh_mean)*0.5
      rh  = rh - rh_mean
      psfc = psfc - psfc_mean
      if ( process_aero ) aero = aero - aero_mean

!     Write out NMC-method standard perturbations:
      output_file = 'pert.'//date(1:10)//'.e001'
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3, ds
      write(gen_be_ounit)znu 
      write(gen_be_ounit)xlat
      write(gen_be_ounit)xlon
      write(gen_be_ounit)mapfac_mx
      write(gen_be_ounit)mapfac_my
      write(gen_be_ounit)psi
      write(gen_be_ounit)chi
      write(gen_be_ounit)temp
      write(gen_be_ounit)rh
      write(gen_be_ounit)rhm
      write(gen_be_ounit)psfc
      if ( process_aero ) write(gen_be_ounit) aero
      close(gen_be_ounit)

   else ! be_method = "ENS"

      write(6,'(/a)') "     [4.1] Convert ensemble of standard fields to perturbations"

      do member = 1, ne
         write(UNIT=ce,FMT='(i3.3)')member

!        Re-read ensemble member standard fields:
         input_file = 'tmp.e'//ce  
         open (gen_be_iunit, file = input_file, form='unformatted')
         read(gen_be_iunit)date, dim1, dim2, dim3
         read(gen_be_iunit)psi
         read(gen_be_iunit)chi
         read(gen_be_iunit)temp
         read(gen_be_iunit)rh
         read(gen_be_iunit)rhm
         read(gen_be_iunit)psfc
         close(gen_be_iunit)

         psi = psi - psi_mean
         chi = chi - chi_mean
         temp = temp - temp_mean
         rh  = rh - rh_mean
         rhm = rhm_mean
         psfc = psfc - psfc_mean

!        Write out standard perturbations for this member:
         output_file = 'pert.'//date(1:10)//'.e'//ce  
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)date, dim1, dim2, dim3, ds
         write(gen_be_ounit)znu 
         write(gen_be_ounit)xlat
         write(gen_be_ounit)xlon
         write(gen_be_ounit)mapfac_mx
         write(gen_be_ounit)mapfac_my
         write(gen_be_ounit)psi
         write(gen_be_ounit)chi
         write(gen_be_ounit)temp
         write(gen_be_ounit)rh
         write(gen_be_ounit)rhm
         write(gen_be_ounit)psfc
         close(gen_be_ounit)
      end do

   end if

   deallocate( psi )
   deallocate( chi )
   deallocate( temp )
   deallocate( rh )
   deallocate( rhm )
   deallocate( psfc )
   deallocate( xlat )
   deallocate( xlon )
   deallocate( psi_mean )
   deallocate( chi_mean )
   deallocate( temp_mean )
   deallocate( rh_mean )
   deallocate( rhm_mean )
   deallocate( psfc_mean )
   deallocate( mapfac_my )
   deallocate( mapfac_mx )
   deallocate( znu )       
   if ( process_aero ) then 
      deallocate(aero)
      deallocate(aero_mean)
   endif

CONTAINS

!------------------------------------------------------------------------------

subroutine da_fft_initialize1( dim1, dim2, n1, n2, ifax1, ifax2 )

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(out):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(out):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(out):: ifax2(1:num_fft_factors)     ! FFT factors.

   integer            :: n                            ! n+1 is the length of the data.
   integer            :: fft_pad1, fft_pad2           ! Range to search for efficient FFT.
   logical            :: found_magic                  ! True if 2**p 3**p 5**r dimension found..

   integer            :: fft_factors(1:num_fft_factors)! FFT factors. 

!  Ensure efficient FFT dimensions by padding if necessary:
   n1 = dim1 - 1 
   do n = n1, n1 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors ) 
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad1 = n - n1
         ifax1 = fft_factors
         exit
      end if 
   end do
   n1 = n1 + fft_pad1

   n2 = dim2 - 1
   do n = n2, n2 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors )
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad2 = n - n2
         ifax2 = fft_factors
         exit
      end if
   end do
   n2 = n2 + fft_pad2

end subroutine da_fft_initialize1

!------------------------------------------------------------------------------

subroutine da_fft_initialize2( n1, n2, ds, trigs1, trigs2, fft_coeffs )

!  Need to split fft_initialize as array dimensions need to be calculated first.

   implicit none

   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   real, intent(in)   :: ds                           ! Grid resolution.
   real, intent(out)  :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(out)  :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(out)  :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.

   integer            :: i, j                         ! Loop counters.
   real               :: const                        ! Multiplicative constant.
   real               :: coeff_nx                     ! Multiplicative constant.
   real               :: coeff_ny                     ! Multiplicative constant.
   real               :: cos_coeff_nx                 ! Multiplicative constant.
   real               :: cos_coeff_ny                 ! Multiplicative constant.

   const = -0.5 * ds * ds
   coeff_nx = pi / real(n1)
   coeff_ny = pi / real(n2)

!  Calculate spectral Del**2 coefficients for C-grid (all pts. except i=j=1):
   fft_coeffs(1,1) = 0.0 ! Not used?
   do j = 2, n2+1
      cos_coeff_ny = cos(coeff_ny * real(j - 1))
      do i = 1, n1+1
         cos_coeff_nx = cos(coeff_nx * real(i - 1))
         fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
      end do
   end do
   j = 1
   cos_coeff_ny = cos(coeff_ny * real(j - 1))
   do i = 2, n1+1
      cos_coeff_nx = cos(coeff_nx * real(i - 1))
      fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
   end do

   call da_find_fft_trig_funcs( n1, trigs1 )
   call da_find_fft_trig_funcs( n2, trigs2 )

end subroutine da_fft_initialize2

!------------------------------------------------------------------------------

subroutine da_uv_to_div_c( dim1, dim2, ds, &
                           mapfac_m, mapfac_u, mapfac_v, & 
                           u, v, div )
   
!------------------------------------------------------------------------------
!  PURPOSE: Calculate divergence on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!  
!  NOTE: No boundary conditions required on the WRF Arakawa C-grid as
!        divergence (mass) points are all within the outer u/v pts.
!
!           Div = m^2 *[---(---) + ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2+1)           ! v wind.
   real, intent(out)  :: div(1:dim1,1:dim2)           ! Divergence.

   integer            :: i, j                         ! Loop counters.
   real               :: ds_inv                       ! 1/ds.
   real               :: coeff(1:dim1,1:dim2)         ! Coefficient.
   real               :: um(1:dim1+1,1:dim2)          ! u-wind copy.
   real               :: vm(1:dim1,1:dim2+1)          ! v-wind copy. 

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   ds_inv = 1.0 / ds
!   do j = 1, dim2
!      do i = 1, dim1
!         coeff(i,j) = ( mapfac_m(i,j) * mapfac_m(i,j) ) * ds_inv
!      end do
!   end do
   coeff(:,:) = ds_inv ! Calculate f.d. Del**2 Chi, rather than divergence.

!------------------------------------------------------------------------------
!  [2] Calculate divergence field:
!------------------------------------------------------------------------------

   do j = 1, dim2
      do i = 1, dim1+1
         um(i,j) = u(i,j) / mapfac_u(i,j)
      end do
   end do

   do j = 1, dim2+1
      do i = 1, dim1
         if (mapfac_v(i,j) > 0.000001) then
            vm(i,j) = v(i,j) / mapfac_v(i,j)
         else
            vm(i,j)=0.0
         end if
      end do
   end do

   do j = 1, dim2
      do i = 1, dim1
         div(i,j) = coeff(i,j) * ( um(i+1,j) - um(i,j) + vm(i,j+1) - vm(i,j) )
      end do
   end do

end subroutine da_uv_to_div_c

subroutine da_uv_to_vor_c( dim1, dim2, ds, &
                           mapfac_m, mapfac_u, mapfac_v, &
                           u, v, vor )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate vorticity on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!  
!  NOTE: Zero vorticity boundary conditions.
!
!                        d   V      d   U
!           Vor = m^2 *[---(---) - ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                ! Dimensions.
   real, intent(in)   :: ds                        ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)   ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2) ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1) ! Map factor - u points.
   real, intent(in)   :: u(1:dim1+1,1:dim2)        ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2+1)        ! v wind.
   real, intent(out)  :: vor(1:dim1+1,1:dim2+1)    ! Vorticity.

   integer            :: i, j                      ! Loop counters.
   real               :: ds_inv                    ! 1/ds.
   ! real               :: mapfac_vor               ! Map factor (vorticity pts)
   real               :: coeff(1:dim1,1:dim2)      ! Coefficient.
   real               :: um(1:dim1+1,1:dim2)       ! u-wind copy.
   real               :: vm(1:dim1,1:dim2+1)       ! v-wind copy. 

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   vor(:,:) = 0.0

   ds_inv = 1.0 / ds
!   do j = 1, dim2
!      do i = 1, dim1
!         mapfac_vor = 0.25 * ( mapfac_u(i,j-1) + mapfac_u(i,j) + &
!                               mapfac_v(i-1,j) + mapfac_v(i,j) ) ! Average.
!         coeff(i,j) = ( mapfac_vor * mapfac_vor ) * ds_inv
!      end do
!   end do
   coeff(:,:) = ds_inv ! Calculate f.d. Del**2 Chi, rather than divergence.

!------------------------------------------------------------------------------
!  [2] Calculate vorticity field:
!------------------------------------------------------------------------------

   do j = 1, dim2  
      do i = 1, dim1+1
         um(i,j) = u(i,j) / mapfac_u(i,j)
      end do
   end do

   do j = 1, dim2+1
      do i = 1, dim1  
         if (mapfac_v(i,j) > 0.000001) then
            vm(i,j) = v(i,j) / mapfac_v(i,j)
         else
            vm(i,j) = 0.0
         end if
      end do
   end do

   do j = 2, dim2
      do i = 2, dim1
         vor(i,j) = coeff(i,j) * ( vm(i,j) - vm(i-1,j) - um(i,j) + um(i,j-1) )
      end do
   end do

!  Boundary values (extrapolation):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
!   vor(1,1:dim2+1)      = 2.0 * vor(2,1:dim2+1) - vor(3,1:dim2+1)         ! West
!   vor(dim1+1,1:dim2+1) = 2.0 * vor(dim1,1:dim2+1) - vor(dim1-1,1:dim2+1) ! East
!   vor(1:dim1+1,1)      = 2.0 * vor(1:dim1+1,2) - vor(1:dim1+1,3)         ! South
!   vor(1:dim1+1,dim2+1) = 2.0 * vor(1:dim1+1,dim2) - vor(1:dim1+1,dim2-1) ! South

!  Boundary values (zero gradient):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
   vor(1,2:dim2)        = vor(2,2:dim2)      ! West
   vor(dim1+1,2:dim2)   = vor(dim1,2:dim2)   ! East
   vor(1:dim1+1,1)      = vor(1:dim1+1,2)    ! South
   vor(1:dim1+1,dim2+1) = vor(1:dim1+1,dim2) ! South

end subroutine da_uv_to_vor_c

subroutine da_psichi_to_uv_c( dim1, dim2, ds, &
                              mapfac_u, mapfac_v, &
                              psi, chi, u, v )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate u and v wind components on an Arakawa C-grid.
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velcoity potential.
   real, intent(out)  :: u(1:dim1+1,1:dim2)           ! v wind.
   real, intent(out)  :: v(1:dim1,1:dim2+1)           ! v wind.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! WRF dims (dummies for now).
   real               :: ds_inv                       ! 1/ds.
   real               :: one_third                    ! 1/3.

   ds_inv = 1.0 / ds
   one_third = 1.0 / 3.0

!  u-wind component:
   its = 2
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         u(i,j) = mapfac_u(i,j) * ds_inv * &
                  ( -psi(i,j+1) + psi(i,j) + chi(i,j) - chi(i-1,j) )
      end do
   end do

!  Remaining points on E/W boundaries (extrapolation):
   u(1,jts:jte) = 2.0 * u(2,jts:jte) - u(3,jts:jte)
   u(dim1+1,jts:jte) = 2.0 * u(dim1,jts:jte) - u(dim1-1,jts:jte)

!  v-wind component:
   its = 1
   ite = dim1
   jts = 2
   jte = dim2

   do j = jts, jte
      do i = its, ite
         v(i,j) = mapfac_v(i,j) * ds_inv * &
                  ( psi(i+1,j) - psi(i,j) + chi(i,j) - chi(i,j-1) )
      end do
   end do

!  Remaining points on S/N boundaries (extrapolation):
   v(its:ite,1) = 2.0 * v(its:ite,2) - v(its:ite,3)
   v(its:ite,dim2+1) = 2.0 * v(its:ite,dim2) - v(its:ite,dim2-1)

end subroutine da_psichi_to_uv_c

!------------------------------------------------------------------------------

subroutine da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                          fft_coeffs, del2a, a )

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
   integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.
   real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(in)   :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.
   real, intent(in)   :: del2a(1:dim1,1:dim2)         ! Del**2 a.
   real, intent(out)  :: a(1:dim1,1:dim2)             ! Field a.

   integer            :: i, j                         ! Loop counters.
   integer            :: ij                           ! 1D array counter.
   integer            :: isign                        ! -1=Grid>spec, 1=Spec>Grid.
   integer            :: inc                          ! Stride between data points.
   integer            :: jump                         ! Increment between start of data vectors.
   integer            :: lot                          ! Number of data vectors.
   integer            :: n                            ! n+1 is the length of the data.
   integer            :: work_area                    ! Dimension of workspace.
   real               :: a2d(1:n1+1,1:n2+1)           ! 2D data array.
   real               :: a1d(1:(n1+1)*(n2+1))         ! 1D data array.

   work_area = ( n1 + 1 ) * ( n2 + 1 )

!  Fill 2D array structure
   do j = 1, dim2
      do i = 1, dim1
         a2d(i,j) = del2a(i,j)
      end do

!     Fill pad zone (and force b.c.s to satisfy solution type):
      if ( fft_method == 1 ) then ! Cosine transform.
         a2d(1,j) = a2d(2,j)
         do i = dim1, n1+1
            a2d(i,j) = a2d(dim1-1,j)
         end do
      else if ( fft_method == 2 ) then ! Sine transform:
         a2d(1,j) = 0.0
         do i = dim1, n1+1
            a2d(i,j) = 0.0
         end do
      end if
   end do

   if ( fft_method == 1 ) then ! Cosine transform.
      do i = 1, n1+1
         a2d(i,1) = a2d(i,2)
         do j = dim2, n2+1
            a2d(i,j) = a2d(i,dim2-1)
         end do
      end do
   else if ( fft_method == 2 ) then ! Sine transform:
      do i = 1, n1+1
         a2d(i,1) = 0.0
         do j = dim2, n2+1
            a2d(i,j) = 0.0
         end do
      end do
   end if

!  Transfer to data array:
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = a2d(i,j)
      end do
   end do

!------------------------------------------------------------------------------
!     Perform double fast sine/cosine transform to get spectral del2a:
!------------------------------------------------------------------------------

   isign = -1 ! Grid to spectral

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.
   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax2, trigs2, a1d, work_area )
   end if

!------------------------------------------------------------------------------
!  Perform conversion from del2a to a in spectral space:
!------------------------------------------------------------------------------

!  Note fft_coeffs(1,1)=0 so a(k=0,l=0) is also 0.
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = fft_coeffs(i,j) * a1d(ij)
      end do
   end do

!------------------------------------------------------------------------------
!  Perform double fast sine/cosine transform to get gridpoint a:
!------------------------------------------------------------------------------

   isign = 1 ! Spectral to grid.

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                                     ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                                   ifax2, trigs2, a1d, work_area )
   end if

!  Transfer grid-point chi to 2D-array (throwing away pad):
   do j = 1, dim2
      do i = 1, dim1
         ij = (j-1) * (n1+1) + i
         a(i,j) = a1d(ij)
      end do
   end do

end subroutine da_del2a_to_a

!---------------------------------------------------------------------------------------------
subroutine da_test_inverse( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                            n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, fft_coeffs, &
                            n1s, n2s, ifax1s, ifax2s, trigs1s, trigs2s, fft_coeffss, &
                            u1, v1, psi, chi )

!------------------------------------------------------------------------------
!  PURPOSE: Test u, v -> psi, chi calculation by performing inverse test.
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.
   integer, intent(in):: n1, n2                       ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: fft_method                   ! 1=Cosine, 2=Sine transform.
   integer, intent(in):: ifax1(1:num_fft_factors)     ! FFT factors.
   integer, intent(in):: ifax2(1:num_fft_factors)     ! FFT factors.
   real, intent(in)   :: trigs1(1:3*n1)               ! FFT trig functions.
   real, intent(in)   :: trigs2(1:3*n2)               ! FFT trig functions.
   real, intent(in)   :: fft_coeffs(1:n1+1,1:n2+1)    ! FFT coefficients.
   integer, intent(in):: n1s, n2s                     ! Padded dimensions (n=dim-1+pad).
   integer, intent(in):: ifax1s(1:num_fft_factors)    ! FFT factors.
   integer, intent(in):: ifax2s(1:num_fft_factors)    ! FFT factors.
   real, intent(in)   :: trigs1s(1:3*n1)              ! FFT trig functions.
   real, intent(in)   :: trigs2s(1:3*n2)              ! FFT trig functions.
   real, intent(in)   :: fft_coeffss(1:n1+1,1:n2+1)   ! FFT coefficients.

   real, intent(in)   :: u1(1:dim1+1,1:dim2)          ! u
   real, intent(in)   :: v1(1:dim1,1:dim2+1)          ! v
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velocity potential.

   real               :: div(1:dim1,1:dim2)           ! Divergence.
   real               :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.

   real               :: u2(1:dim1+1,1:dim2)          ! u
   real               :: v2(1:dim1,1:dim2+1)          ! v
   real               :: u3(1:dim1+1,1:dim2)          ! u
   real               :: v3(1:dim1,1:dim2+1)          ! v
   real               :: psi1(1:dim1+1,1:dim2+1)      ! streamfunction
   real               :: chi1(1:dim1,1:dim2)          ! divergence

   write(6,'(a,i4)')' Using FFT method (1=Cosine, 2=Sine): ', fft_method


   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi, chi, u2, v2 )

   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field u = ', &
                         sqrt(sum( ( u1(:,:) -  u2(:,:) )**2 ) / sum( u1(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field v = ', &
                         sqrt(sum( ( v1(:,:) -  v2(:,:) )**2 ) / sum( v1(:,:)**2 ))

   call da_uv_to_div_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, div )
   call da_uv_to_vor_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, vor )

   call da_del2a_to_a( dim1, dim2, n1, n2, fft_method, ifax1, ifax2, trigs1, trigs2, &
                       fft_coeffs, div, chi1 )
   call da_del2a_to_a( dim1s, dim2s, n1s, n2s, fft_method, ifax1s, ifax2s, trigs1s, trigs2s, &
                       fft_coeffss, vor, psi1 )

   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field psi = ', &
                         sqrt(sum( ( psi(:,:) -  psi1(:,:) )**2 ) / sum( psi(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field chi = ', &
                         sqrt(sum( ( chi(:,:) -  chi1(:,:) )**2 ) / sum( chi(:,:)**2 ))

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi1, chi1, u3, v3 )

   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field u = ', &
                         sqrt(sum( ( u3(:,:) -  u2(:,:) )**2 ) / sum( u2(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field v = ', &
                         sqrt(sum( ( v3(:,:) -  v2(:,:) )**2 ) / sum( v2(:,:)**2 ))

end subroutine da_test_inverse

!---------------------------------------------------------------------------------------------

#if 0
! routine never used
subroutine da_test_inverse2( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                            u1, v1, psi, chi )

!------------------------------------------------------------------------------
!  PURPOSE: Test u, v -> psi, chi calculation by performing inverse test.
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Resolution.
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts.
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points.
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points.

   real, intent(in)   :: u1(1:dim1+1,1:dim2)          ! u
   real, intent(in)   :: v1(1:dim1,1:dim2+1)          ! v
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velocity potential.

   real               :: div(1:dim1,1:dim2)           ! Divergence.
   real               :: vor(1:dim1+1,1:dim2+1)       ! Vorticity.

   real               :: u2(1:dim1+1,1:dim2)          ! u
   real               :: v2(1:dim1,1:dim2+1)          ! v
   real               :: u3(1:dim1+1,1:dim2)          ! u
   real               :: v3(1:dim1,1:dim2+1)          ! v
   real               :: psi1(1:dim1+1,1:dim2+1)      ! streamfunction
   real               :: chi1(1:dim1,1:dim2)          ! divergence

   write(6,'(a,i4)')' Using SOR method'

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi, chi, u2, v2 )

   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field u = ', &
                         sqrt(sum( ( u1(:,:) -  u2(:,:) )**2 ) / sum( u1(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 1: Ratio error/field v = ', &
                         sqrt(sum( ( v1(:,:) -  v2(:,:) )**2 ) / sum( v1(:,:)**2 ))

   call da_uv_to_div_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, div )
   call da_uv_to_vor_c( dim1, dim2, ds, mapfac_m, mapfac_u, mapfac_v, &
                        u2, v2, vor )

   call da_sor( dim1, dim2, ds, div, chi1 )
   call da_sor( dim1s, dim2s, ds, vor, psi1 )

   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field psi = ', &
                         sqrt(sum( ( psi(:,:) -  psi1(:,:) )**2 ) / sum( psi(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 2: Ratio error/field chi = ', &
                         sqrt(sum( ( chi(:,:) -  chi1(:,:) )**2 ) / sum( chi(:,:)**2 ))

   call da_psichi_to_uv_c( dim1, dim2, ds, &
                           mapfac_u, mapfac_v, psi1, chi1, u3, v3 )

   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field u = ', &
                         sqrt(sum( ( u3(:,:) -  u2(:,:) )**2 ) / sum( u2(:,:)**2 ))
   write(6,'(a,1pe12.4)')' Inverse test 3: Ratio error/field v = ', &
                         sqrt(sum( ( v3(:,:) -  v2(:,:) )**2 ) / sum( v2(:,:)**2 ))

end subroutine da_test_inverse2
#endif

!----------------------------------------------------------------------------------------

!DALE: Use fortran 90 free-form, and lower case throughout?
!DALE: Provide short description of code here, including assumptions, ref., etc.

   SUBROUTINE da_sor ( dim1, dim2, ds, del2a, a )

      IMPLICIT NONE

      REAL,          PARAMETER    :: SMALLRES = 1.0E-4 ! DALE: Provide comments here.

      INTEGER,       PARAMETER    :: MM = 20000        ! DALE: And here, etc.
      REAL,          PARAMETER    :: ALPHA = 1.8
      REAL,          PARAMETER    :: ALPHAOV4 = alpha / 4.0

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: ds                           ! Grid resolution.
   real, intent(in)   :: del2a(1:dim1,1:dim2)         ! Del**2 a.
   real, intent(out)  :: a(1:dim1,1:dim2)             ! Field a.

   integer            :: i, j                         ! Loop counters.
   real               :: rd(1:dim1,1:dim2)            ! Solution residual.

   integer            :: ids, ide, jds, jde
      INTEGER                     :: ITER
      INTEGER                     :: MI
      REAL                        :: CHI         ( 1:dim1,1:dim2 )
      REAL                        :: CHIMX       ( 1:dim1 )
      REAL                        :: EPX
      REAL                        :: FAC
      REAL                        :: FF          ( 1:dim1 , 1:dim2 )
      REAL                        :: RDMAX       ( 1:dim1 )

      LOGICAL                     :: converged = .FALSE.

      ids = 1
      ide = dim1
      jds = 1
      jde = dim2
  
      rd  = 0.0
      chi = 0.0

      fac = 2.0 * ds * ds
      DO i = ids, ide
         DO j = jds, jde
               ff(i,j) = del2a(i,j) * fac
         END DO
      END DO

      iter_loop : DO iter = 1, mm
        mi = iter
         chimx = 0.0

! FIX? Crayx1 compiler does not like these, so comment out 
! for the present
!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               chimx(i) = MAX(ABS(chi(i,j)),chimx(i))
            END DO
         END DO
!!csd$ end parallel do

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO j = jds+1, jde-1
            DO i = ids+1, ide-1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!!csd$ end parallel do

!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO j = jde-1, jds+1, -1
            DO i = ide-1, ids+1, -1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!!csd$ end parallel do

         rdmax = 0.0

!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               rdmax(i) = MAX(ABS(rd(i,j)),rdmax(i))
            END DO
         END DO
!!csd$ end parallel do

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         ELSE
         END IF

      END DO iter_loop

      IF (converged ) THEN
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation did not converge in',mm,' iterations.'
!         STOP 'no_converge'
      END IF

      a(:,:) = chi(:,:)

   END SUBROUTINE da_sor

#if 0
! routine never used
   SUBROUTINE da_relax (psi, vor, residual, ids, ide, jds, jde, ds)

      IMPLICIT NONE

      INTEGER, intent(in) :: IDS, IDE
      INTEGER, intent(in) :: JDS, JDE
      REAL, intent(out)    :: PSI         ( ids:ide , jds:jde )
      REAL, intent(in)    :: VOR         ( ids:ide , jds:jde )
      REAL, intent(out)    :: residual        ( ids:ide , jds:jde )
      REAL, intent(in)    :: DS

      REAL,          PARAMETER    :: SMALLRES = 5.0E-7
!      REAL,          PARAMETER    :: SMALLRES = 1.0E-6
!      REAL,          PARAMETER    :: SMALLRES = 1.0E-4 ! DALE: Provide comments here.

      INTEGER,       PARAMETER    :: MM = 20000        ! DALE: And here, etc.
!      REAL,          PARAMETER    :: ALPHA = 1.8
      REAL,          PARAMETER    :: ALPHA = 1.2
      REAL,          PARAMETER    :: ALPHAOV4 = alpha / 4.0

      INTEGER                     :: I
      INTEGER                     :: ITER
      INTEGER                     :: J
      INTEGER                     :: MI

      REAL (kind=8)                        :: CHI         ( ids:ide , jds:jde )
      REAL (kind=8)                        :: CHIMX       ( ids:ide )
      REAL (kind=8)                       :: EPX
      REAL (kind=8)                       :: FAC
      REAL (kind=8)                        :: FF          ( ids:ide , jds:jde )
      REAL (kind=8)                        :: RD          ( ids:ide , jds:jde )
      REAL (kind=8)                       :: RDMAX       ( ids:ide )

      LOGICAL                     :: converged = .FALSE.

      rd  = 0.0
      chi = 0.0

      fac = 2.0 * ds * ds
      DO i = ids, ide
         DO j = jds, jde
               ff(i,j) = vor(i,j) * fac
         END DO
      END DO

      iter_loop : DO iter = 1, mm
         mi = iter
         chimx = 0.0


!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               chimx(i) = MAX(ABS(chi(i,j)),chimx(i))
            END DO
         END DO
!!csd$ end parallel do

         epx = MAXVAL(chimx) * SMALLRES * 4.0 / alpha

!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO j = jds+1, jde-1
            DO i = ids+1, ide-1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!!csd$ end parallel do

!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO j = jde-1, jds+1, -1
            DO i = ide-1, ids+1, -1
               rd(i,j) = chi(i+1,j+1) + chi(i-1,j+1) + chi(i+1,j-1) + chi(i-1,j-1) - 4.0 * chi(i,j) - ff(i,j)
               chi(i,j) = chi(i,j) + rd(i,j) * alphaov4
            END DO
         END DO
!!csd$ end parallel do

         rdmax = 0.0

!!$OMP PARALLEL DO DEFAULT ( SHARED ) PRIVATE ( i , j )
!!csd$ parallel do private(i,j)
         DO i = ids+1, ide-1
            DO j = jds+1, jde-1
               rdmax(i) = MAX(ABS(rd(i,j)),rdmax(i))
            END DO
         END DO
!!csd$ end parallel do

         IF (MAXVAL(rdmax) .lt. epx) THEN
            converged = .TRUE.
            EXIT iter_loop
         ELSE
         END IF

      END DO iter_loop

      IF (converged ) THEN
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation converged in ',mi,' iterations.'
      ELSE
         PRINT '(A,i3,A,I5,A)','k=',k,'  Relaxation did not converge in',mm,' iterations.'
!         STOP 'no_converge'
      END IF

      psi(:,:) = chi(:,:)
      residual(:,:) = rd(:,:)

   END SUBROUTINE da_relax
#endif

end program gen_be_stage0_gsi

