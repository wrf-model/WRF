subroutine da_read_pseudo_rad (iv)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr tovs 1b data to innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            so that da_scan_bufrtovs is not necessary any more.
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------

   use da_control

   implicit none

   type (iv_type),  intent (inout) :: iv

   ! Instrument triplet, follow the convension of RTTOV 
   integer   :: platform_id, satellite_id, sensor_id

   real, allocatable    :: tb_inv(:)  !  bright temperatures
   type (datalink_type) :: p

   logical :: outside, outside_all
   integer :: i,k,n,nchan,inst, alloc_stat
   type(info_type)       ::  info
   type(model_loc_type)  ::  loc

   call da_trace_entry("da_read_pseudo_rad")

   ! Initialize variables

   platform_id  = pseudo_rad_platid
   satellite_id = pseudo_rad_satid
   sensor_id    = pseudo_rad_senid
   if (sensor_id == 0) then
      nchan=19 !nchan_hirs
   else if (sensor_id == 1) then
      nchan=nchan_msu
   else if (sensor_id == 3) then
      nchan=nchan_amsua
   else if (sensor_id == 4)  then
      nchan=nchan_amsub
   else if (sensor_id == 15)  then
      nchan=nchan_mhs
   else if (sensor_id == 10)  then
      nchan=nchan_ssmis
   else if (sensor_id == 11)  then
      nchan=nchan_airs
   end if

   inst = 1    ! single instrument

   allocate (tb_inv(nchan))

   info%lat  =  pseudo_rad_lat ! in degree
   info%lon  =  pseudo_rad_lon
   info%date_char = "0000-00-00_00:00:00"
   call da_llxy (info, loc, outside, outside_all)

   if (outside) then
      iv%info(radiance)%nlocal          = 0
      iv%info(radiance)%ntotal          = 0
      iv%info(radiance)%ptotal(1)       = 0
      iv%instid(inst)%num_rad           = 0
      iv%instid(inst)%info%nlocal       = 0
   else  
      iv%info(radiance)%nlocal          = 1
      iv%info(radiance)%ntotal          = 1
      iv%info(radiance)%ptotal(1)       = 1
      iv%instid(inst)%num_rad           = 1
      iv%instid(inst)%info%nlocal       = 1
   end if

   do k = 1, nchan
      tb_inv(k) = missing_r
   end do
   tb_inv(pseudo_rad_ichan)=pseudo_rad_inv

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------  
   
   i = 1

   if ( iv%instid(inst)%num_rad > 0 ) then
      if (rtm_option == rtm_option_crtm) then
         iv%instid(inst)%nlevels = kme-kms+1
         iv%instid(inst)%nchannels=nchan
      end if
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         i, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad

#ifdef RTTOV
      if (rtm_option == rtm_option_rttov) then
         call rttov_setupchan(1, nchan, coefs(i), &   ! in
            iv%instid(inst)%nfrequencies,iv%instid(inst)%nchannels, &
            iv%instid(inst)%nbtout)      ! out
      end if
#endif

      call da_allocate_rad_iv(i,nchan,iv)

      !  6.0 assign sequential structure to innovation structure
      !-------------------------------------------------------------

      n = 1     ! single obs

      allocate (p%tb_inv(1:nchan), stat=alloc_stat)
      if ( alloc_stat /= 0 ) CALL da_error(__FILE__,__LINE__,(/"error allocating"/))

      p%info             = info
      p%loc              = loc
      p%landsea_mask     = 1
      p%scanpos          = 1
      p%satzen           = 0.0
      p%satazi           = 0.0
      p%solzen           = 0.0
      p%tb_inv(1:nchan)  = tb_inv(1:nchan)
      p%sensor_index     = inst          
      p%ifgat            = 1
     
      call da_initialize_rad_iv (inst, n, iv, p)

      iv%instid(inst)%tb_qc(:,n)                   = qc_bad
      iv%instid(inst)%tb_error(pseudo_rad_ichan,n) = pseudo_rad_err
      iv%instid(inst)%tb_qc(pseudo_rad_ichan,n)    = qc_good

      deallocate(p%tb_inv)
   end if

   deallocate(tb_inv)

   call da_trace_exit("da_read_pseudo_rad")

end subroutine da_read_pseudo_rad

