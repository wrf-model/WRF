subroutine da_radiance_init(iv,ob)
!------------------------------------------------------------------------------
!  PURPOSE: subroutine to initialize radiances.
!
!  METHOD:  
!  1.0 Set up from namelist parameter
!  2.0 Set up some common variables for innovation/observation
!  3.0 Initialize RTTOV / CRTM
!  4.0 Set up bias correction
!  5.0 Read error factor file
!  6.0 Get FGAT time slots
!
!  HISTORY: 10/24/2007 Created from da_crtm_init            Tom Auligne
!  HISTORY: 12/15/2008 getting FGAT time slots is moved to 
!                      da_setup_obs_structures.inc.         Hui-Chuan Lin
!------------------------------------------------------------------------------

 implicit none 

 type (iv_type), intent (inout) :: iv
 type (y_type) , intent (inout) :: ob

!
!  local arguments
!------------------- 
 integer   :: n, j, ichan, iret
 integer :: nsensor, unit_factor_rad
 integer     :: error
 integer, allocatable   ::  nscan(:), nchanl(:)

! local variables
!----------------
 integer             :: idum, wmo_sensor_id, sensor_type, iost
 integer             :: iunit
 character(len=filename_len)  :: filename
 character(len=20)   :: cdum
 real                :: error_cld

! local variables for tuning error factor
!----------------------------------------
 character(len=20)   ::  rttovid_string
 integer             ::  num_tot
 real                ::  joa, jo, trace, factor 

  call da_trace_entry("da_radiance_init")

!--------------------------------------------------------------
!  1.0 setup from namelist parameter
!--------------------------------------------------------------
  nsensor = rtminit_nsensor
  allocate (nscan(nsensor))
  allocate (nchanl(nsensor))

!----------------------------------------------------------------
!  2.0 set up some common variables for innovation/observation structure
!----------------------------------------------------------------
  iv % num_inst = nsensor
  ob % num_inst = nsensor

  allocate (iv%instid(1:nsensor))
  allocate (ob%instid(1:nsensor))
  allocate (satinfo(1:nsensor))

  iv%instid(1:nsensor)%num_rad = 0
  ob%instid(1:nsensor)%num_rad = 0

  loop_sensor: do n = 1, nsensor

   iv%instid(n)%platform_id  = rtminit_platform(n)
   iv%instid(n)%satellite_id = rtminit_satid(n)
   iv%instid(n)%sensor_id    = rtminit_sensor(n)
   if ( rtminit_satid(n) < 10 ) then
      write(iv%instid(n)%rttovid_string, '(a,i1,a)')  &
             trim( rttov_platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( rttov_inst_name(rtminit_sensor(n)) )
      write(iv%instid(n)%rttovid_string_coef, '(a,i1,a)')  &
             trim( rttov_platform_name(rtminit_platform(n)) )//'_',  &
             rtminit_satid(n),     &
             '_'//trim( rttov_inst_name(rtminit_sensor(n)) )
   else
      write(iv%instid(n)%rttovid_string, '(a,i2.2,a)')  &
             trim( rttov_platform_name(rtminit_platform(n)) )//'-',  &
             rtminit_satid(n),     &
             '-'//trim( rttov_inst_name(rtminit_sensor(n)) )
      write(iv%instid(n)%rttovid_string_coef, '(a,i2.2,a)')  &
             trim( rttov_platform_name(rtminit_platform(n)) )//'_',  &
             rtminit_satid(n),     &
             '_'//trim( rttov_inst_name(rtminit_sensor(n)) )
   end if

   if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'msu' ) then
      nchanl(n)  = 4
      nscan(n)   = 11
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'hirs' ) then
      nchanl(n)  = 19
      nscan(n)   = 56
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'amsua' ) then
      nchanl(n)  = 15
      nscan(n)   = 30
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'amsub' ) then
      nchanl(n)  = 5
      nscan(n)   = 90
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'airs' ) then
      nchanl(n)  = 281
      nscan(n)   = 90
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'hsb' ) then
      nchanl(n)  = 4
      nscan(n)   = 90
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'mhs' ) then
      nchanl(n)  = 5
      nscan(n)   = 90
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'ssmis' ) then
      nchanl(n)  = 24
      nscan(n)   = 60
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'mwts' ) then
      nchanl(n)  = 4
      nscan(n)   = 15
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'mwhs' ) then
      nchanl(n)  = 5
      nscan(n)   = 98
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'mwhs2' ) then
      nchanl(n)  = 15
      nscan(n)   = 98   
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'atms' ) then
      nchanl(n)  = 22
      nscan(n)   = 96
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'iasi' ) then
     nchanl(n)  = 616
      nscan(n)   = 60	  
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'seviri' ) then
     nchanl(n)  =  8 
     nscan(n)   = 90 
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'amsr2' ) then
      nchanl(n)  = 14
      nscan(n)   = 486
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'ahi' ) then
     !open the ahi info file 
      open(unit=1990,file='ahi_info',status='old',iostat=iret)
      if(iret /= 0)then
         call da_error(__FILE__,__LINE__,(/"Read ahi_info error: no such file"/))
      end if	  
     !read ahi information
      read(1990,*)  
      read(1990,*) 
      read(1990,*) 
      read(1990,*) nscan(n) 
      close(1990)     
      write(*,*) nscan(n)   
      nchanl(n)  = 10
   else if ( trim( crtm_sensor_name(rtminit_sensor(n))) == 'imgr' ) then
      nchanl(n)  = 4
      nscan(n)   = 60
   else
      write(unit=message(1),fmt='(A)') "Unrecognized instrument: "
      write(unit=message(2),fmt='(A,I4)') "rtminit_platform = ",rtminit_platform(n)
      write(unit=message(3),fmt='(A,I4)') "rtminit_satid    = ",rtminit_satid(n)
      write(unit=message(4),fmt='(A,I4)') "rtminit_sensor   = ",rtminit_sensor(n)
      write(unit=message(5),fmt='(A)') "Check your namelist settings"
      call da_error(__FILE__,__LINE__,message(1:5))
   end if

   iv%instid(n)%nchan  = nchanl(n)
   ob%instid(n)%nchan  = nchanl(n)

   allocate ( iv%instid(n)%ichan(1:nchanl(n)), stat = error )
   if( error /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"Memory allocation error to iv%instid(n)%ichan"/))
   end if

   allocate ( ob%instid(n)%ichan(1:nchanl(n)), stat = error )
   if( error /= 0 ) then
      call da_error(__FILE__,__LINE__, &                                                           
         (/"Memory allocation error to ob%instid(n)%ichan"/))
   end if

   call da_get_unit(iunit)
   filename='radiance_info/'//trim(adjustl(iv%instid(n)%rttovid_string))//'.info'
   open(unit=iunit,file=filename, form='formatted',iostat = iost, status='old')

   if (iost /= 0) then
      message(1)="Cannot open radiance info file "//adjustl(filename)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   allocate ( satinfo(n) % ichan(nchanl(n)) )
   allocate ( satinfo(n) % iuse (nchanl(n)) )
   allocate ( satinfo(n) % error(nchanl(n)) )
   allocate ( satinfo(n) % error_cld(nchanl(n)) )
   allocate ( satinfo(n) % polar(nchanl(n)) )

   satinfo(n) % error_cld(:) = 500.0 !initialize

   read(iunit,*)
   do j = 1, nchanl(n)
      read(iunit,'(1x,5i5,2e18.10,a20)') &
                     wmo_sensor_id, &
               satinfo(n)%ichan(j), &
                       sensor_type, &
               satinfo(n)%iuse(j) , &
                              idum, &
               satinfo(n)%error(j), &
               satinfo(n)%polar(j), &
               cdum
      !in the current radiance info files, the last column
      !can be either sensor_id_string or blank
      if ( len_trim(cdum) > 0 .and. index(cdum,'-') == 0 ) then
         ! read the line again to get error_cld when it is available
         backspace(iunit)
         read(iunit,'(1x,5i5,2e18.10,f10.5)')  &
                     wmo_sensor_id, &
               satinfo(n)%ichan(j), &
                       sensor_type, &
               satinfo(n)%iuse(j) , &
                              idum, &
               satinfo(n)%error(j), &
               satinfo(n)%polar(j), &
               error_cld
         if ( error_cld > 0.0 ) then
            satinfo(n)%error_cld(j) = error_cld
         end if
      end if
     iv%instid(n)%ichan(j) = satinfo(n)%ichan(j)
     ob%instid(n)%ichan(j) = satinfo(n)%ichan(j)
   end do
   call da_free_unit(iunit)

   if ( use_blacklist_rad ) then
      call da_blacklist_rad(trim(rttov_platform_name(rtminit_platform(n))), &
                            rtminit_satid(n), &
                            trim(rttov_inst_name(rtminit_sensor(n))), &
                            nchanl(n), &
                            satinfo(n)%iuse )
   end if

  end do loop_sensor

!---------------------------------------------------------------------
! 3.0 Interface to the initialization subroutine of RTTOV and CRTM
!---------------------------------------------------------------------

    if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
       call da_rttov_init(iv,ob,nsensor,nchanl)
#else
       call da_error(__FILE__,__LINE__, &
          (/"Must compile with $RTTOV option for radiances"/))
#endif
    end if

    if (rtm_option == rtm_option_crtm) then
#ifdef CRTM
       call da_crtm_init(iv,ob,nsensor)
#else
       call da_error(__FILE__,__LINE__, &
          (/"Must compile with $CRTM option for radiances"/))
#endif
    end if

!-------------------------------------------------------
!  4.0 read bias correction coefs files
!-------------------------------------------------------

 loop_sensor2: do n = 1, nsensor

   allocate ( satinfo(n) % scanbias  (nchanl(n),nscan(n)) )
   allocate ( satinfo(n) % scanbias_b(nchanl(n),nscan(n),18) )
   allocate ( satinfo(n) % bcoef     (nchanl(n),4) )
   allocate ( satinfo(n) % bcoef0    (nchanl(n)) )
   allocate ( satinfo(n) % error_std (nchanl(n)) )

   satinfo(n) % error_std(:) = 500.0
   satinfo(n) % scanbias(:,:) = 0.0
   satinfo(n) % scanbias_b(:,:,:) = 0.0
   satinfo(n) % bcoef(:,:) = 0.0
   satinfo(n) % bcoef0(:) = 0.0

  if (read_biascoef) then
  !  new bias coefs files
  !  use o-b standard deviation statistics from Harris and Kelly method as obs errors
  !----------------------------------
 
    if ( index(iv%instid(n)%rttovid_string,'eos')  > 0 ) cycle   ! not implemented
    if ( index(iv%instid(n)%rttovid_string,'hirs') > 0 ) cycle   ! not implemented

    call da_read_biascoef(iv%instid(n)%rttovid_string, &
                      nchanl(n),nscan(n),18,4,global, &
                      satinfo(n)%scanbias, &
                      satinfo(n)%scanbias_b, &
                      satinfo(n)%bcoef, &
                      satinfo(n)%bcoef0, &
                      satinfo(n)%error_std)
  else
    ! use values specified in radiance_info files as obs errors
    satinfo(n)%error_std = satinfo(n)%error
  end if
 end do loop_sensor2

!-------------------------------------------------------
!  5.0 read error factor file
!-------------------------------------------------------
 if (use_error_factor_rad) then

    do n=1, rtminit_nsensor
       allocate ( satinfo(n)%error_factor(1:nchanl(n)) )
       satinfo(n)%error_factor(:) = 1.0
    end do

    call da_get_unit(unit_factor_rad)
    open(unit_factor_rad, file='radiance_error.factor', &
         form='formatted',iostat = iost, status='old')

    if (iost /= 0) then
       call da_error(__FILE__,__LINE__, &
         (/"Cannot open radiance error factor file: radiance_error.factor"/))
    end if

    read(unit_factor_rad, *)
    do
      read(unit_factor_rad,fmt='(a15,i8,i8,3f15.5,f8.3)',iostat=iost)   &
          rttovid_string, ichan, num_tot, joa,jo,trace,factor
      if ( iost == 0 ) then
        do n=1, rtminit_nsensor
          if ( index(rttovid_string,trim(iv%instid(n)%rttovid_string))>0 ) then
             satinfo(n)%error_factor(ichan) = factor
             write(6,'(a,i5,a,f10.3)') trim(rttovid_string)//' Channel ', ichan, '  Error Factor = ', factor
             exit
          end if
        end do
      else
         exit
      end if
    end do
    close(unit_factor_rad)
    call da_free_unit(unit_factor_rad)

 end if

  deallocate(nscan)
  deallocate(nchanl)

  call da_trace_exit("da_radiance_init")
end subroutine da_radiance_init
