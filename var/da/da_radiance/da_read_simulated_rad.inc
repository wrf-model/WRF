subroutine da_read_simulated_rad (iv)

   !---------------------------------------------------------------------------
   !  Purpose: Generate simulated radiances for every model grid point and 
   !           every channel
   !
   !  Called from 
   !
   !  HISTORY: 12/12/2008 - Creation                        Tom Auligne
   !---------------------------------------------------------------------------

   use da_control

   implicit none

   type (iv_type),  intent (inout) :: iv

   ! Instrument triplet, follow the convension of RTTOV 
   integer   :: platform_id, satellite_id, sensor_id

   type (datalink_type) :: p

   logical :: outside, outside_all
   integer :: i,j,k,nchan,inst, alloc_stat
   type(info_type)       ::  info
   type(model_loc_type)  ::  loc
 
   call da_trace_entry("da_read_simulated_rad")

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

   iv%info(radiance)%ntotal    = (ide - ids + 1) * (jde - jds + 1)
   iv%info(radiance)%nlocal    = (min(ipe+simulated_rad_ngrid,ide) - max(ips-simulated_rad_ngrid,ids) + 1) * &
                                 (min(jpe+simulated_rad_ngrid,jde) - max(jps-simulated_rad_ngrid,jds) + 1)
   iv%info(radiance)%ptotal(1) = iv%info(radiance)%ntotal
   iv%instid(inst)%num_rad     = iv%info(radiance)%nlocal
   iv%instid(inst)%info%nlocal = iv%info(radiance)%nlocal

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------  
   
   if ( iv%instid(inst)%num_rad > 0 ) then

      call da_allocate_rad_iv(inst,nchan,iv)

      !  6.0 assign sequential structure to innovation structure
      !-------------------------------------------------------------

      allocate (p%tb_inv(1:nchan), stat=alloc_stat)
      if ( alloc_stat /= 0 ) CALL da_error(__FILE__,__LINE__,(/"error allocating"/))

      p%info%date_char   = "0000-00-00_00:00:00"         
      p%landsea_mask     = 1
      p%scanpos          = 1
      p%satzen           = 0.0
      p%satazi           = 0.0
      p%solzen           = 0.0
      p%tb_inv(1:nchan)  = pseudo_rad_inv
      p%sensor_index     = inst          
      p%ifgat            = 1
     
      k = 0
      do i = ids, ide
         do j = jds, jde
	    if ((i<ips-simulated_rad_ngrid).or.(i>ipe+simulated_rad_ngrid).or. &
	        (j<jps-simulated_rad_ngrid).or.(j>jpe+simulated_rad_ngrid)) cycle
	    k = k + 1
            call da_xyll(map_info, i*1.0, j*1.0, p%info%lat, p%info%lon)
	    p%loc%x   = float(i)
	    p%loc%y   = float(j)
	    p%loc%i   = i
	    p%loc%j   = j
	    p%loc%dx  = 0.0
	    p%loc%dxm = 1.0
	    p%loc%dy  = 0.0
	    p%loc%dym = 1.0	    
            call da_initialize_rad_iv (inst, k, iv, p)
	 end do
      end do

      iv%instid(inst)%tb_error(:,:) = pseudo_rad_err
      iv%instid(inst)%tb_qc(:,:)    = qc_good

      deallocate(p%tb_inv)
   end if

      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad

   call da_trace_exit("da_read_simulated_rad")
   

end subroutine da_read_simulated_rad
