program gen_be_ep2
!
!---------------------------------------------------------------------- 
!  Purpose : To convert WRF ensemble to format required for use as 
!  flow-dependent perturbations in WRF-Var (alpha control variable, 
!  alphacv_method = 2).
!
!  Dale Barker (NCAR/MMM)      January 2007
!  Arthur P. Mizzi (NCAR/MMM)  February 2011  Modified to use .vari extension for
!                                             ensemble variance file output from
!                                             gen_be_ensmean.f90
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit, da_free_unit
   use da_gen_be, only : da_stage0_initialize, da_get_field, da_get_trh

   implicit none

   character (len=filename_len)   :: directory        ! General filename stub.
   character (len=filename_len)   :: filename         ! General filename stub.
   character (len=filename_len)   :: input_file       ! Input file.
   character (len=filename_len)   :: output_file      ! Output file.
   character (len=10)    :: date                      ! Character date.
   character (len=10)    :: var                       ! Variable to search for.
   character (len=3)     :: cne                       ! Ensemble size.
   character (len=3)     :: ce                        ! Member index -> character.
   character (len=filename_len)   :: moist_string

   integer, external     :: iargc
   integer               :: numarg
   integer               :: ne                        ! Ensemble size.
   integer               :: i, j, k, member           ! Loop counters.
   integer               :: dim1                      ! Dimensions of grid (T points).
   integer               :: dim1s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim2                      ! Dimensions of grid (T points).
   integer               :: dim2s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim3                      ! Dimensions of grid (T points).
   integer               :: mp_physics                ! microphysics option
   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution.
   logical               :: remove_mean               ! Remove mean from standard fields.
   logical               :: has_cloud, has_rain, has_ice, has_snow, has_graup

   real, allocatable     :: u(:,:,:)                  ! u-wind.
   real, allocatable     :: v(:,:,:)                  ! v-wind.
   real, allocatable     :: temp(:,:,:)               ! Temperature.
   real, allocatable     :: q(:,:,:)                  ! Specific humidity.
   real, allocatable     :: qcloud(:,:,:)             ! Cloud.
   real, allocatable     :: qrain(:,:,:)              ! Rain.
   real, allocatable     :: qice(:,:,:)               ! ice
   real, allocatable     :: qsnow(:,:,:)              ! snow
   real, allocatable     :: qgraup(:,:,:)             ! graupel
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   real, allocatable     :: u_mean(:,:,:)             ! u-wind.
   real, allocatable     :: v_mean(:,:,:)             ! v-wind.
   real, allocatable     :: temp_mean(:,:,:)          ! Temperature.
   real, allocatable     :: q_mean(:,:,:)             ! Specific humidity.
   real, allocatable     :: qcloud_mean(:,:,:)        ! Cloud.
   real, allocatable     :: qrain_mean(:,:,:)         ! Rain.
   real, allocatable     :: qice_mean(:,:,:)          ! ice
   real, allocatable     :: qsnow_mean(:,:,:)         ! snow
   real, allocatable     :: qgraup_mean(:,:,:)        ! graupel
   real, allocatable     :: psfc_mean(:,:)            ! Surface pressure.
   real, allocatable     :: u_mnsq(:,:,:)             ! u-wind.
   real, allocatable     :: v_mnsq(:,:,:)             ! v-wind.
   real, allocatable     :: temp_mnsq(:,:,:)          ! Temperature.
   real, allocatable     :: q_mnsq(:,:,:)             ! Specific humidity.
   real, allocatable     :: qcloud_mnsq(:,:,:)        ! Cloud.
   real, allocatable     :: qrain_mnsq(:,:,:)         ! Rain.
   real, allocatable     :: qice_mnsq(:,:,:)          ! ice
   real, allocatable     :: qsnow_mnsq(:,:,:)         ! snow
   real, allocatable     :: qgraup_mnsq(:,:,:)        ! graupel
   real, allocatable     :: psfc_mnsq(:,:)            ! Surface pressure.

   real, allocatable     :: utmp(:,:)                 ! u-wind.
   real, allocatable     :: vtmp(:,:)                 ! v-wind.
   real, allocatable     :: ttmp(:,:)                 ! temperature.
   real, allocatable     :: dummy(:,:)                ! dummy.

   integer :: gen_be_iunit, gen_be_ounit

   stderr = 0
   stdout = 6

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [1] Initialize information.'
!---------------------------------------------------------------------------------------------

   call da_get_unit(gen_be_iunit)
   call da_get_unit(gen_be_ounit)

   remove_mean = .true.

   numarg = iargc()
   if ( numarg /= 4 )then
      write(UNIT=6,FMT='(a)') &
        "Usage: gen_be_ep2 date ne <directory> <filename> Stop"
      stop
   end if

   ! Initialse to stop Cray compiler complaining
   date=""
   cne=""
   directory=""
   filename=""

   call getarg( 1, date )
   call getarg( 2, cne )
   read(cne,'(i3)')ne
   call getarg( 3, directory )
   call getarg( 4, filename )

   if ( remove_mean ) then
      write(6,'(a,a)')' Computing gen_be ensemble perturbation files for date ', date
   else
      write(6,'(a,a)')' Computing gen_be ensemble forecast files for date ', date
   end if
   write(6,'(a)')' Perturbations are in MODEL SPACE (u, v, t, q, ps)'
   write(6,'(a,i4)')' Ensemble Size = ', ne
   write(6,'(a,a)')' Directory = ', trim(directory)
   write(6,'(a,a)')' Filename = ', trim(filename)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Set up data dimensions and allocate arrays:' 
!---------------------------------------------------------------------------------------------

!  Get grid dimensions from first T field:
   var = "T"
   input_file = trim(directory)//'/'//trim(filename)//'.e001'
   call da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds, mp_physics )
   dim1s = dim1+1 ! u i dimension is 1 larger.
   dim2s = dim2+1 ! v j dimension is 1 larger.

!  Allocate arrays in output fields:
   allocate( u(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to mass pts for output.
   allocate( v(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to mass pts for output.
   allocate( temp(1:dim1,1:dim2,1:dim3) )
   allocate( q(1:dim1,1:dim2,1:dim3) )
   allocate( psfc(1:dim1,1:dim2) )
   allocate( u_mean(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( v_mean(1:dim1,1:dim2,1:dim3) )
   allocate( temp_mean(1:dim1,1:dim2,1:dim3) )
   allocate( q_mean(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_mean(1:dim1,1:dim2) )
   allocate( u_mnsq(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( v_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( temp_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( q_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_mnsq(1:dim1,1:dim2) )
   ! cloud variables
   has_cloud = .false.
   has_rain  = .false.
   has_ice   = .false.
   has_snow  = .false.
   has_graup = .false.
   moist_string = ''
   if ( mp_physics > 0 ) then
      has_cloud = .true.
      has_rain  = .true.
      allocate( qcloud(1:dim1,1:dim2,1:dim3) )
      allocate( qrain(1:dim1,1:dim2,1:dim3) )
      allocate( qcloud_mean(1:dim1,1:dim2,1:dim3) )
      allocate( qrain_mean(1:dim1,1:dim2,1:dim3) )
      allocate( qcloud_mnsq(1:dim1,1:dim2,1:dim3) )
      allocate( qrain_mnsq(1:dim1,1:dim2,1:dim3) )
      qcloud_mean = 0.0
      qrain_mean = 0.0
      qcloud_mnsq = 0.0
      qrain_mnsq = 0.0
      moist_string = trim(moist_string)//'qcloud, qrain '
      if ( mp_physics == 2 .or. mp_physics == 4 .or. &
           mp_physics >= 6 ) then
         has_ice   = .true.
         allocate( qice(1:dim1,1:dim2,1:dim3) )
         allocate( qice_mean(1:dim1,1:dim2,1:dim3) )
         allocate( qice_mnsq(1:dim1,1:dim2,1:dim3) )
         qice_mean = 0.0
         qice_mnsq = 0.0
         moist_string = trim(moist_string)//', qice '
      end if
      if ( mp_physics == 2 .or. mp_physics >= 4 ) then
         has_snow  = .true.
         allocate( qsnow(1:dim1,1:dim2,1:dim3) )
         allocate( qsnow_mean(1:dim1,1:dim2,1:dim3) )
         allocate( qsnow_mnsq(1:dim1,1:dim2,1:dim3) )
         qsnow_mean = 0.0
         qsnow_mnsq = 0.0
         moist_string = trim(moist_string)//', qsnow '
      end if
      if ( mp_physics == 2 .or. mp_physics >= 6 ) then
         if ( mp_physics /= 11 .and. mp_physics /= 13 .and. &
              mp_physics /= 14 ) then
            has_graup = .true.
            allocate( qgraup(1:dim1,1:dim2,1:dim3) )
            allocate( qgraup_mean(1:dim1,1:dim2,1:dim3) )
            allocate( qgraup_mnsq(1:dim1,1:dim2,1:dim3) )
            qgraup_mean = 0.0
            qgraup_mnsq = 0.0
            moist_string = trim(moist_string)//', qgraup '
         end if
      end if
      write(6,'(a)')' cloud variables are '//trim(moist_string)
   end if

   u_mean = 0.0
   v_mean = 0.0
   temp_mean = 0.0
   q_mean = 0.0
   psfc_mean = 0.0
   u_mnsq = 0.0
   v_mnsq = 0.0
   temp_mnsq = 0.0
   q_mnsq = 0.0
   psfc_mnsq = 0.0

!  Temporary arrays:
   allocate( utmp(1:dim1s,1:dim2) ) ! u on Arakawa C-grid.
   allocate( vtmp(1:dim1,1:dim2s) ) ! v on Arakawa C-grid.
   allocate( ttmp(1:dim1,1:dim2) )
   allocate( dummy(1:dim1,1:dim2) )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [3] Extract necessary fields from input NETCDF files and output.'
!---------------------------------------------------------------------------------------------

   do member = 1, ne

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(directory)//'/'//trim(filename)//'.e'//trim(ce)

      do k = 1, dim3

         ! Read u, v:
         var = "U"
         call da_get_field( input_file, var, 3, dim1s, dim2, dim3, k, utmp )
         var = "V"
         call da_get_field( input_file, var, 3, dim1, dim2s, dim3, k, vtmp )

!        Interpolate u to mass pts:
         do j = 1, dim2
            do i = 1, dim1
               u(i,j,k) = 0.5 * ( utmp(i,j) + utmp(i+1,j) )
               v(i,j,k) = 0.5 * ( vtmp(i,j) + vtmp(i,j+1) )
            end do
         end do

!        Read theta, and convert to temperature:
         call da_get_trh( input_file, dim1, dim2, dim3, k, ttmp, dummy )
         temp(:,:,k) = ttmp(:,:)

!        Read mixing ratio, and convert to specific humidity:
         var = "QVAPOR"
         call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
         q(:,:,k) = dummy(:,:) / ( 1.0 + dummy(:,:) )

!        Read hydrometeors
         if ( has_cloud ) then
            var = "QCLOUD"
            call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
            qcloud(:,:,k) = dummy(:,:)
         end if
         if ( has_rain ) then
            var = "QRAIN"
            call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
            qrain(:,:,k) = dummy(:,:)
         end if
         if ( has_ice ) then
            var = "QICE"
            call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
            qice(:,:,k) = dummy(:,:)
         end if
         if ( has_snow ) then
            var = "QSNOW"
            call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
            qsnow(:,:,k) = dummy(:,:)
         end if
         if ( has_graup ) then
            var = "QGRAUP"
            call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
            qgraup(:,:,k) = dummy(:,:)
         end if

      end do

!     Finally, extract surface pressure:
      var = "PSFC"
      call da_get_field( input_file, var, 2, dim1, dim2, dim3, 1, psfc )

!     Write out ensemble forecasts for this member:
      output_file = 'tmp.e'//ce  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3
      write(gen_be_ounit)u
      write(gen_be_ounit)v
      write(gen_be_ounit)temp
      write(gen_be_ounit)q
      if ( has_cloud ) write(gen_be_ounit)qcloud
      if ( has_rain )  write(gen_be_ounit)qrain
      if ( has_ice )   write(gen_be_ounit)qice
      if ( has_snow )  write(gen_be_ounit)qsnow
      if ( has_graup ) write(gen_be_ounit)qgraup
      write(gen_be_ounit)psfc
      close(gen_be_ounit)

!     Calculate accumulating mean and mean square:
      member_inv = 1.0 / real(member)
      u_mean = ( real( member-1 ) * u_mean + u ) * member_inv
      v_mean = ( real( member-1 ) * v_mean + v ) * member_inv
      temp_mean = ( real( member-1 ) * temp_mean + temp ) * member_inv
      q_mean = ( real( member-1 ) * q_mean + q ) * member_inv
      psfc_mean = ( real( member-1 ) * psfc_mean + psfc ) * member_inv
      u_mnsq = ( real( member-1 ) * u_mnsq + u * u ) * member_inv
      v_mnsq = ( real( member-1 ) * v_mnsq + v * v ) * member_inv
      temp_mnsq = ( real( member-1 ) * temp_mnsq + temp * temp ) * member_inv
      q_mnsq = ( real( member-1 ) * q_mnsq + q * q ) * member_inv
      psfc_mnsq = ( real( member-1 ) * psfc_mnsq + psfc * psfc ) * member_inv
      if ( has_cloud ) then
         qcloud_mean = ( real( member-1 ) * qcloud_mean + qcloud ) * member_inv
         qcloud_mnsq = ( real( member-1 ) * qcloud_mnsq + qcloud * qcloud ) * member_inv
      end if
      if ( has_rain ) then
         qrain_mean = ( real( member-1 ) * qrain_mean + qrain ) * member_inv
         qrain_mnsq = ( real( member-1 ) * qrain_mnsq + qrain * qrain ) * member_inv
      end if
      if ( has_ice ) then
         qice_mean = ( real( member-1 ) * qice_mean + qice ) * member_inv
         qice_mnsq = ( real( member-1 ) * qice_mnsq + qice * qice ) * member_inv
      end if
      if ( has_snow ) then
         qsnow_mean = ( real( member-1 ) * qsnow_mean + qsnow ) * member_inv
         qsnow_mnsq = ( real( member-1 ) * qsnow_mnsq + qsnow * qsnow ) * member_inv
      end if
      if ( has_graup ) then
         qgraup_mean = ( real( member-1 ) * qgraup_mean + qgraup ) * member_inv
         qgraup_mnsq = ( real( member-1 ) * qgraup_mnsq + qgraup * qgraup ) * member_inv
      end if

   end do

   deallocate( utmp )
   deallocate( vtmp )
   deallocate( ttmp )
   deallocate( dummy )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [4] Compute perturbations and output' 
!---------------------------------------------------------------------------------------------

   if ( remove_mean ) then
      write(6,'(a)') "    Calculate ensemble perturbations"
   else
      write(6,'(a)') "    WARNING: Not removing ensemble mean (outputs are full-fields)"
   end if

   do member = 1, ne
      write(UNIT=ce,FMT='(i3.3)')member

!     Re-read ensemble member standard fields:
      input_file = 'tmp.e'//ce  
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)u
      read(gen_be_iunit)v
      read(gen_be_iunit)temp
      read(gen_be_iunit)q
      if ( has_cloud ) read(gen_be_iunit)qcloud
      if ( has_rain )  read(gen_be_iunit)qrain
      if ( has_ice )   read(gen_be_iunit)qice
      if ( has_snow )  read(gen_be_iunit)qsnow
      if ( has_graup ) read(gen_be_iunit)qgraup
      read(gen_be_iunit)psfc
      close(gen_be_iunit)

      if ( remove_mean ) then
         u = u - u_mean
         v = v - v_mean
         temp = temp - temp_mean
         q = q - q_mean
         if ( has_cloud ) qcloud = qcloud - qcloud_mean
         if ( has_rain )  qrain  = qrain  - qrain_mean
         if ( has_ice )   qice   = qice   - qice_mean
         if ( has_snow )  qsnow  = qsnow  - qsnow_mean
         if ( has_graup ) qgraup = qgraup - qgraup_mean
         psfc = psfc - psfc_mean
      end if

!     Write out perturbations for this member:

      output_file = 'u.e'//trim(ce) ! Output u.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)u
      close(gen_be_ounit)

      output_file = 'v.e'//trim(ce) ! Output v.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)v
      close(gen_be_ounit)

      output_file = 't.e'//trim(ce) ! Output t.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)temp
      close(gen_be_ounit)

      output_file = 'q.e'//trim(ce) ! Output q.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)q
      close(gen_be_ounit)

      output_file = 'ps.e'//trim(ce) ! Output ps.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)psfc
      close(gen_be_ounit)

      if ( has_cloud ) then
         output_file = 'qcloud.e'//trim(ce) ! Output qcloud.
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)dim1, dim2, dim3
         write(gen_be_ounit)qcloud
         close(gen_be_ounit)
      end if

      if ( has_rain ) then
         output_file = 'qrain.e'//trim(ce) ! Output qrain.
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)dim1, dim2, dim3
         write(gen_be_ounit)qrain
         close(gen_be_ounit)
      end if

      if ( has_ice ) then
         output_file = 'qice.e'//trim(ce) ! Output qice.
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)dim1, dim2, dim3
         write(gen_be_ounit)qice
         close(gen_be_ounit)
      end if

      if ( has_snow ) then
         output_file = 'qsnow.e'//trim(ce) ! Output qsnow.
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)dim1, dim2, dim3
         write(gen_be_ounit)qsnow
         close(gen_be_ounit)
      end if

      if ( has_graup ) then
         output_file = 'qgraup.e'//trim(ce) ! Output qgraup.
         open (gen_be_ounit, file = output_file, form='unformatted')
         write(gen_be_ounit)dim1, dim2, dim3
         write(gen_be_ounit)qgraup
         close(gen_be_ounit)
      end if

   end do

!  Write out mean/stdv fields (stdv stored in mnsq arrays):
   u_mnsq = sqrt( u_mnsq - u_mean * u_mean )
   v_mnsq = sqrt( v_mnsq - v_mean * v_mean )
   temp_mnsq = sqrt( temp_mnsq - temp_mean * temp_mean )
   q_mnsq = sqrt( q_mnsq - q_mean * q_mean )
   psfc_mnsq = sqrt( psfc_mnsq - psfc_mean * psfc_mean )
   if ( has_cloud ) qcloud_mnsq = sqrt( qcloud_mnsq - qcloud_mean * qcloud_mean )
   if ( has_rain )  qrain_mnsq  = sqrt( qrain_mnsq - qrain_mean * qrain_mean )
   if ( has_ice )   qice_mnsq   = sqrt( qice_mnsq - qice_mean * qice_mean )
   if ( has_snow )  qsnow_mnsq  = sqrt( qsnow_mnsq - qsnow_mean * qsnow_mean )
   if ( has_graup ) qgraup_mnsq = sqrt( qgraup_mnsq - qgraup_mean * qgraup_mean )

   output_file = 'u.mean' ! Output u.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)u_mean
   close(gen_be_ounit)

   output_file = 'u.stdv' ! Output u.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)u_mnsq
   close(gen_be_ounit)

   output_file = 'v.mean' ! Output v.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)v_mean
   close(gen_be_ounit)

   output_file = 'v.stdv' ! Output v.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)v_mnsq
   close(gen_be_ounit)

   output_file = 't.mean' ! Output t.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)temp_mean
   close(gen_be_ounit)

   output_file = 't.stdv' ! Output t.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)temp_mnsq
   close(gen_be_ounit)

   output_file = 'q.mean' ! Output q.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)q_mean
   close(gen_be_ounit)

   output_file = 'q.stdv' ! Output q.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)q_mnsq
   close(gen_be_ounit)

   output_file = 'ps.mean' ! Output ps.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)psfc_mean
   close(gen_be_ounit)

   output_file = 'ps.stdv' ! Output ps.
   open (gen_be_ounit, file = output_file, form='unformatted')
   write(gen_be_ounit)dim1, dim2, dim3
   write(gen_be_ounit)psfc_mnsq
   close(gen_be_ounit)

   if ( has_cloud ) then
      output_file = 'qcloud.mean' ! Output qcloud.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qcloud_mean
      close(gen_be_ounit)

      output_file = 'qcloud.stdv' ! Output qcloud.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qcloud_mnsq
      close(gen_be_ounit)
   end if

   if ( has_rain ) then
      output_file = 'qrain.mean' ! Output qrain.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qrain_mean
      close(gen_be_ounit)

      output_file = 'qrain.stdv' ! Output qrain.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qrain_mnsq
      close(gen_be_ounit)
   end if

   if ( has_ice ) then
      output_file = 'qice.mean' ! Output qice.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qice_mean
      close(gen_be_ounit)

      output_file = 'qice.stdv' ! Output qice.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qice_mnsq
      close(gen_be_ounit)
   end if

   if ( has_snow ) then
      output_file = 'qsnow.mean' ! Output qsnow.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qsnow_mean
      close(gen_be_ounit)

      output_file = 'qsnow.stdv' ! Output qsnow.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qsnow_mnsq
      close(gen_be_ounit)
   end if

   if ( has_graup ) then
      output_file = 'qgraup.mean' ! Output qgraup.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qgraup_mean
      close(gen_be_ounit)

      output_file = 'qgraup.stdv' ! Output qgraup.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qgraup_mnsq
      close(gen_be_ounit)
   end if

   call da_free_unit(gen_be_iunit)
   call da_free_unit(gen_be_ounit)

#ifdef crayx1
contains

   subroutine getarg(i, harg)
     implicit none
     character(len=*) :: harg
     integer :: ierr, ilen, i

     call pxfgetarg(i, harg, ilen, ierr)
     return
   end subroutine getarg
#endif

end program gen_be_ep2

