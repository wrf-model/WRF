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

   character (len=filename_len)   :: directory                 ! General filename stub.
   character (len=filename_len)   :: filename                  ! General filename stub.
   character (len=filename_len)   :: input_file                ! Input file. 
   character (len=filename_len)   :: output_file               ! Output file. 
   character (len=10)    :: date                      ! Character date.
   character (len=10)    :: var                       ! Variable to search for.
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
   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution.
   logical               :: remove_mean               ! Remove mean from standard fields.

   real, allocatable     :: u(:,:,:)                  ! u-wind.
   real, allocatable     :: v(:,:,:)                  ! v-wind.
   real, allocatable     :: temp(:,:,:)               ! Temperature.
   real, allocatable     :: q(:,:,:)                  ! Specific humidity.
   real, allocatable     :: qcloud(:,:,:)             ! Cloud.
   real, allocatable     :: qrain(:,:,:)              ! Rain.
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   real, allocatable     :: u_mean(:,:,:)             ! u-wind.
   real, allocatable     :: v_mean(:,:,:)             ! v-wind.
   real, allocatable     :: temp_mean(:,:,:)          ! Temperature.
   real, allocatable     :: q_mean(:,:,:)             ! Specific humidity.
   real, allocatable     :: qcloud_mean(:,:,:)        ! Cloud.
   real, allocatable     :: qrain_mean(:,:,:)         ! Rain.
   real, allocatable     :: psfc_mean(:,:)            ! Surface pressure.
   real, allocatable     :: u_mnsq(:,:,:)             ! u-wind.
   real, allocatable     :: v_mnsq(:,:,:)             ! v-wind.
   real, allocatable     :: temp_mnsq(:,:,:)          ! Temperature.
   real, allocatable     :: q_mnsq(:,:,:)             ! Specific humidity.
   real, allocatable     :: qcloud_mnsq(:,:,:)        ! Cloud.
   real, allocatable     :: qrain_mnsq(:,:,:)         ! Rain.
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
   print *, 'apm in gen_be_ep2'
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
   write(6,'(a)')' Perturbations are in MODEL SPACE (u, v, t, q, qcloud, qrain, ps)'
   write(6,'(a,i4)')' Ensemble Size = ', ne
   write(6,'(a,a)')' Directory = ', trim(directory)
   write(6,'(a,a)')' Filename = ', trim(filename)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Set up data dimensions and allocate arrays:' 
!---------------------------------------------------------------------------------------------

!  Get grid dimensions from first T field:
   var = "T"
   input_file = trim(directory)//'/'//trim(filename)//'.e001'
   call da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds )
   dim1s = dim1+1 ! u i dimension is 1 larger.
   dim2s = dim2+1 ! v j dimension is 1 larger.

!  Allocate arrays in output fields:
   allocate( u(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to mass pts for output.
   allocate( v(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to mass pts for output.
   allocate( temp(1:dim1,1:dim2,1:dim3) )
   allocate( q(1:dim1,1:dim2,1:dim3) )
   allocate( qcloud(1:dim1,1:dim2,1:dim3) )
   allocate( qrain(1:dim1,1:dim2,1:dim3) )
   allocate( psfc(1:dim1,1:dim2) )
   allocate( u_mean(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( v_mean(1:dim1,1:dim2,1:dim3) )
   allocate( temp_mean(1:dim1,1:dim2,1:dim3) )
   allocate( q_mean(1:dim1,1:dim2,1:dim3) )
   allocate( qcloud_mean(1:dim1,1:dim2,1:dim3) )
   allocate( qrain_mean(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_mean(1:dim1,1:dim2) )
   allocate( u_mnsq(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( v_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( temp_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( q_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( qcloud_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( qrain_mnsq(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_mnsq(1:dim1,1:dim2) )
   u_mean = 0.0
   v_mean = 0.0
   temp_mean = 0.0
   q_mean = 0.0
   qcloud_mean = 0.0
   qrain_mean = 0.0
   psfc_mean = 0.0
   u_mnsq = 0.0
   v_mnsq = 0.0
   temp_mnsq = 0.0
   q_mnsq = 0.0
   qcloud_mnsq = 0.0
   qrain_mnsq = 0.0
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

!        Read hydrometeors (need better method to read all e.g. qsn automatically):
         var = "QCLOUD"
         call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
         qcloud(:,:,k) = dummy(:,:)
         var = "QRAIN"
         call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
         qrain(:,:,k) = dummy(:,:)

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
      write(gen_be_ounit)qcloud
      write(gen_be_ounit)qrain
      write(gen_be_ounit)psfc
      close(gen_be_ounit)

!     Calculate accumulating mean and mean square:
      member_inv = 1.0 / real(member)
      u_mean = ( real( member-1 ) * u_mean + u ) * member_inv
      v_mean = ( real( member-1 ) * v_mean + v ) * member_inv
      temp_mean = ( real( member-1 ) * temp_mean + temp ) * member_inv
      q_mean = ( real( member-1 ) * q_mean + q ) * member_inv
      qcloud_mean = ( real( member-1 ) * qcloud_mean + qcloud ) * member_inv
      qrain_mean = ( real( member-1 ) * qrain_mean + qrain ) * member_inv
      psfc_mean = ( real( member-1 ) * psfc_mean + psfc ) * member_inv
      u_mnsq = ( real( member-1 ) * u_mnsq + u * u ) * member_inv
      v_mnsq = ( real( member-1 ) * v_mnsq + v * v ) * member_inv
      temp_mnsq = ( real( member-1 ) * temp_mnsq + temp * temp ) * member_inv
      q_mnsq = ( real( member-1 ) * q_mnsq + q * q ) * member_inv
      qcloud_mnsq = ( real( member-1 ) * qcloud_mnsq + qcloud * qcloud ) * member_inv
      qrain_mnsq = ( real( member-1 ) * qrain_mnsq + qrain * qrain ) * member_inv
      psfc_mnsq = ( real( member-1 ) * psfc_mnsq + psfc * psfc ) * member_inv

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
      read(gen_be_iunit)qcloud
      read(gen_be_iunit)qrain
      read(gen_be_iunit)psfc
      close(gen_be_iunit)

      if ( remove_mean ) then
         u = u - u_mean
         v = v - v_mean
         temp = temp - temp_mean
         q = q - q_mean
         qcloud = qcloud - qcloud_mean
         qrain = qrain - qrain_mean
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

      output_file = 'qcloud.e'//trim(ce) ! Output qcloud.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qcloud
      close(gen_be_ounit)

      output_file = 'qrain.e'//trim(ce) ! Output qrain.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)qrain
      close(gen_be_ounit)

      output_file = 'ps.e'//trim(ce) ! Output ps.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)psfc
      close(gen_be_ounit)

   end do

!  Write out mean/stdv fields (stdv stored in mnsq arrays):
   u_mnsq = sqrt( u_mnsq - u_mean * u_mean )
   v_mnsq = sqrt( v_mnsq - v_mean * v_mean )
   temp_mnsq = sqrt( temp_mnsq - temp_mean * temp_mean )
   q_mnsq = sqrt( q_mnsq - q_mean * q_mean )
   qcloud_mnsq = sqrt( qcloud_mnsq - qcloud_mean * qcloud_mean )
   qrain_mnsq = sqrt( qrain_mnsq - qrain_mean * qrain_mean )
   psfc_mnsq = sqrt( psfc_mnsq - psfc_mean * psfc_mean )

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

