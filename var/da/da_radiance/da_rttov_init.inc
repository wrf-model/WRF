subroutine da_rttov_init(iv,ob,nsensor,nchan)

   !---------------------------------------------------------------------------
   !  Purpose: interface to the initialization subroutine of RTTOV.
   !
   !  METHOD:  1. read RTTOV coefs files; 2. allocate radiance structure
   !
   !  HISTORY: 07/28/2005 - Creation                               Zhiquan Liu
   !           03/22/2006   add error tuning factor read in        Zhiquan Liu
   !           10/24/2007   limit this routine to RTTOV init       Tom Auligne
   !
   !---------------------------------------------------------------------------

   implicit none 

   type (iv_type), intent (inout) :: iv
   type (y_type) , intent (inout) :: ob
   integer ,       intent (in)    :: nsensor
   integer ,       intent (in)    :: nchan(nsensor)

#include "rttov_setup.interface"
#include "rttov_readscattcoeffs.interface"

   !  local arguments
   !------------------- 
   integer   :: i, j,ichan,unit_factor_rad, n

   !
   !  input parameters of RTTOV_SETUP
   !----------------------------------
   integer :: err_unit        ! Logical error unit (<0 for default)
   integer :: verbosity_level ! (<0 for default)
   integer, allocatable :: sensor(:,:) ! instrument id
   !  integer, allocatable :: channels(:,:)   ! channel list per instrument

   !  output parameters of RTTOV_SETUP
   !-------------------------------------
   integer, allocatable :: errorstatus(:)  ! return code
   integer              :: coef_errorstatus, i_amsua, i_amsub
   !  type( rttov_coef ), allocatable :: coefs(:)         ! coefficients

   ! local variables
   !----------------
   integer             :: nprofiles, nfrequencies, nchannels, nbtout
   integer , allocatable :: lprofiles(:)
   real*8  , allocatable :: surfem(:), emissivity(:)
   integer , allocatable :: channels (:), polarisations(:,:)
   integer             :: mxchn
   integer , allocatable :: coefs_channels (:,:)
 
   integer             :: idum, wmo_sensor_id, sensor_type, iost, iunit

   ! local variables for tuning error factor
   character(len=20)   ::  rttovid_string
   integer             ::  num_tot
   real                ::  joa, jo, trace, factor 

   if (trace_use) call da_trace_entry("da_rttov_init")

   !--------------------------------------------------------------
   !  1.0 setup RTTOV instrument triplets from namelist parameter
   !--------------------------------------------------------------

   mxchn           =  MAXVAL(nchan)
   err_unit        =  stderr
   verbosity_level =  rtminit_print

   allocate (errorstatus(nsensor))
   allocate (coefs(nsensor))
   allocate (sensor(3,nsensor))
   allocate (coefs_channels(mxchn,nsensor))

   sensor (1,1:nsensor)  = rtminit_platform (1:nsensor) 
   sensor (2,1:nsensor)  = rtminit_satid    (1:nsensor)
   sensor (3,1:nsensor)  = rtminit_sensor   (1:nsensor)

   coefs_channels(:,:) = 0
   do n = 1, nsensor
      coefs_channels(1:nchan(n),n) = iv%instid(n)%ichan(1:nchan(n))
   end do

   if (print_detail_rad) then
      write(unit=message(1),fmt='(A,I5)') 'err_unit             = ', err_unit
      write(unit=message(2),fmt='(A,I5)') 'verbosity_level      = ', verbosity_level
      write(unit=message(3),fmt='(A,I5)') 'nsensor              = ', nsensor
      write(unit=message(4),fmt='(A,10I5)') 'sensor (1,1:nsensor) = ', sensor (1,1:nsensor)
      write(unit=message(5),fmt='(A,10I5)') 'sensor (2,1:nsensor) = ', sensor (2,1:nsensor)
      write(unit=message(6),fmt='(A,10I5)') 'sensor (3,1:nsensor) = ', sensor (3,1:nsensor)
      call da_message(message(1:6))
   end if

   !-----------------------------------------------------------
   ! 2.0 call rttov_setup for reading clear sky coefficients
   !-----------------------------------------------------------

   call rttov_setup (&
      errorstatus,      &! out
      err_unit,         &! in
      verbosity_level,  &! in
      nsensor,          &! in
      coefs,            &! out
      sensor,           &! in
      coefs_channels )   ! in (Optional)

   if (any(errorstatus(:) /= errorstatus_success)) then
      call da_error(__FILE__,__LINE__,(/"rttov_setup fatal error"/))
   end if

   !-------------------------------------------------------------
   ! 3.0 read coef file for cloud/rain absorption/scattering
   !-------------------------------------------------------------

   if (rttov_scatt) then
      i_amsua = 0
      i_amsub = 0
      do i=1,nsensor
         if (trim(rttov_inst_name(rtminit_sensor(i))) == 'amsua') &
            i_amsua = i
         if ( trim(rttov_inst_name(rtminit_sensor(i))) == 'amsub') &
            i_amsub = i 
      end do

      if (i_amsua /= 0 .and. i_amsub == 0) then
         n_scatt_coef = 1
         allocate (coefs_scatt(n_scatt_coef))
         allocate (coefs_scatt_instname(n_scatt_coef))
         coefs_scatt_instname(1) = 'amsua'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                                   coefs_scatt(1))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if
      end if
      if (i_amsua == 0 .and. i_amsub /= 0) then
         n_scatt_coef = 1
         allocate (coefs_scatt(n_scatt_coef))
         allocate (coefs_scatt_instname(n_scatt_coef))
         coefs_scatt_instname(1) = 'amsub'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                                    coefs_scatt(1))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if      
      end if

      if (i_amsua /= 0 .and. i_amsub /= 0) then
         n_scatt_coef = 2
         allocate (coefs_scatt(n_scatt_coef))
         allocate (coefs_scatt_instname(n_scatt_coef))
         coefs_scatt_instname(1) = 'amsua'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsua), &
                                  coefs_scatt(1))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if
         coefs_scatt_instname(2) = 'amsub'
         call rttov_readscattcoeffs(coef_errorstatus, coefs(i_amsub), &
                                  coefs_scatt(2))
         if (coef_errorstatus /= errorstatus_success ) then
            call da_error(__FILE__,__LINE__,(/"rttov_readscattcoeffs fatal error"/))
         end if
      end if

      if ( i_amsua == 0 .and. i_amsub == 0 ) n_scatt_coef = 0
   end if

   !----------------------------------------------------------------
   ! 4.0 set up some common variables for innovation/observation structure
   !----------------------------------------------------------------

   iv%instid(1:nsensor)%nlevels = coefs(1:nsensor)%nlevels

   call da_message((/"Read in the RTTOV coef files for the following sensors"/))

   loop_sensor2: do i = 1, nsensor

      !---------------------------------------------------
      ! 4.1 get more information about sensor
      !---------------------------------------------------

      nprofiles = 1
      write(unit=message(1),fmt='(2a,2x,a,i5)') "   ", &
         iv%instid(i)%rttovid_string, 'nchan=',nchan(i)

      call rttov_setupchan(nprofiles, nchan(i), coefs(i), &    ! in
                     nfrequencies, nchannels, nbtout )        ! out

      allocate (lprofiles(nfrequencies))
      allocate (channels (nfrequencies))
      allocate (polarisations(nchannels, 3))
      allocate (emissivity( nchannels))
      allocate (surfem ( nchannels))
      surfem(:) = 0.0

      call rttov_setupindex(nchan(i), nprofiles, nfrequencies, &    ! in
                     nchannels, nbtout, coefs(i), surfem,  &    ! in
                     lprofiles, channels, polarisations,     &    ! out
                     emissivity  )

      write (unit=message(2),fmt=*) &
          '  nprofiles = ', nprofiles, &
          'nchan = ', nchan(i), &
          'nfrequencies = ', nfrequencies, &
          'nchannels    = ', nchannels, &
          'nbtout       = ', nbtout
      ! FIX? problems with PGI
      ! write (unit=message(3),fmt=*) '  lprofiles    = ', lprofiles
      ! write (unit=message(4),fmt=*) '  channels     = ', channels

      call da_message(message(1:2))

      deallocate (lprofiles)
      deallocate (channels)
      deallocate (polarisations)
      deallocate (emissivity)
      deallocate (surfem)

   end do loop_sensor2

   deallocate (errorstatus)
   deallocate (sensor)
   deallocate (coefs_channels)

   if (trace_use) call da_trace_exit("da_rttov_init")

end subroutine da_rttov_init
