subroutine da_write_oa_radar_ascii ( ob, iv, re, it )

   !---------------------------------------------------------------------------
   ! Purpose: write out OMB and OMA vector structure for radar data.
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.
   type (y_type),     intent(in)  :: re       ! O-A structure.
   integer,           intent(in)  :: it       ! external iteration counter

   integer                        :: n , num_obs       ! Loop counter.
   integer                        :: i, k     ! Index dimension.
   integer                        :: nlevelss ! Number of obs levels.

   integer            :: ios, oma_radar_unit, omb_radar_unit, omb_radar_iter_unit 
   character(len=filename_len)  :: filename , file 
   integer            :: ndomain
   character(len=filename_len), allocatable     ::filename1(:) 
   INTEGER :: m, kk, l 
   REAL :: rv_obs, rv_inv, rv_error , rf_obs, rf_inv, rf_error, rf_inc, rv_inc 
   INTEGER :: rv_qc, rf_qc, levels, num, error 
   REAL :: lat,lon,press, zk 
   CHARACTER(len=5) :: stn_id 

    if (trace_use) call da_trace_entry("da_write_oa_radar_ascii")

    write(unit=message(1),fmt='(A)') 'Writing radar OMA ascii file'
    call da_message(message(1:1))

#ifdef DM_PARALLEL
   write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'radar_omb_oma_',it,'.', myproc
#else
   write(unit=message(1),fmt='(A)') 'write_ao_radar does not work in serial mode'
   call da_warning(__FILE__,__LINE__,message(1:1))
   if (trace_use) call da_trace_exit("da_write_oa_radar_ascii")
   return
!   write(unit=filename, fmt='(a,i2.2,a)') 'radar_omb_oma_',it,'.0000'
#endif
   call da_get_unit(oma_radar_unit)
   open(unit=oma_radar_unit,file=trim(filename),form='formatted',iostat=ios)
   if (ios /= 0) Then
       call da_error(__FILE__,__LINE__, &
         (/"Cannot open oma radar file"//filename/))
   endif

   if (iv % info(radar)%nlocal  >0 ) then
      num_obs = 0
      do n = 1, iv% info(radar)%nlocal
         if (iv%info(radar)%proc_domain(1,n)) num_obs=num_obs+1    
      end do
      if (num_obs > 0) then
         write(oma_radar_unit,'(a20,i8)')'radar', num_obs
         num_obs = 0
        do n = 1, iv % info(radar)%nlocal
            if (iv%info(radar)%proc_domain(1,n)) then  
               num_obs = num_obs + 1
               write(oma_radar_unit,'(i8)') iv % info(radar) % levels(n)
               do k = 1, iv % info(radar) % levels(n)
                 write(oma_radar_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7),f17.7)')&
                    num_obs , k, 'RADAR', &  
                    iv % info (radar)% lat(1,n), &       ! Latitude
                    iv % info (radar) % lon(1,n), &       ! Longitude
                    iv % radar(n) % height(k), &           ! Obs height in m
                    ob%radar(n)%rv(k),&
                    iv%radar(n)%rv(k)%inv,iv%radar(n)%rv(k)%qc,iv%radar(n)%rv(k)%error,&
                    re%radar(n)%rv(k), &! O, O-B, O-A rv
                    ob%radar(n)%rf(k), &
                    iv%radar(n)%rf(k)%inv,iv%radar(n)%rf(k)%qc,iv%radar(n)%rf(k)%error,&
                    re%radar(n)%rf(k),iv%info(radar)%zk(k,n)   ! O, O-B, O-A rf
               end do
            end if
         end do
      end if
   end if


#ifdef DM_PARALLEL
   ! Wait to ensure all temporary files have been written
   call mpi_barrier(comm, ierr)
#endif


  close (oma_radar_unit)
  call da_free_unit(oma_radar_unit)

  
  IF (rootproc) THEN
  call da_get_unit(omb_radar_unit)
  allocate (filename1(0:num_procs-1)) 
      do k = 0,num_procs-1
         write(unit=filename1(k),fmt ='(a,i2.2,a,i4.4)')'radar_omb_oma_',it,'.',k
      end do
   call da_get_unit(omb_radar_iter_unit)
   write(unit=file,fmt ='(a,i2.2)')'radar_omb_oma_',it
   open(omb_radar_iter_unit,file=trim(file),form='formatted', status='replace', iostat=ios)
     if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//file/))
  ENDIF
 
  num_obs = 0
  IF (iv % info(radar)%nlocal  >0 ) then
      do n = 1, iv% info(radar)%nlocal
        if (iv%info(radar)%proc_domain(1,n)) num_obs=num_obs+1
      end do
  ENDIF
   call da_proc_sum_int(num_obs)    
    IF (num_obs > 0 .and. rootproc) then
      write(omb_radar_iter_unit,'(a20,i8)')'radar', num_obs  
      num_obs = 0
      num = 0
      do k = 0,num_procs-1

        open(omb_radar_unit,file=trim(filename1(k)),status='old',iostat=error)
          if (error /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//file/))

      read(omb_radar_unit, '(20x,i8)', iostat=error)num_obs
      IF(error /= 0)THEN
         write(unit=message(1),fmt='(A,A)') 'Nothing to read from ',filename1(k)
         call da_message(message(1:1))
         cycle
      ENDIF   
         if (num_obs > 0) then
           do  n = 1, num_obs    
               read(omb_radar_unit,'(i8)') levels
                  write(omb_radar_iter_unit,'(i8)')levels
                  num = num + 1 
               do m = 1, levels
                  read(omb_radar_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7),f17.7)')&
                      kk,l, stn_id, &          ! Station
                      lat, lon, press, &       ! Lat/lon, dummy    
                      rv_obs, rv_inv, rv_qc, rv_error, rv_inc, & 
                      rf_obs, rf_inv, rf_qc, rf_error, rf_inc, zk
                     write(omb_radar_iter_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7),f17.7)')&
                        num,m,stn_id, &          ! Station
                         lat, lon, press, &       ! Lat/lon, dummy    
                     rv_obs, rv_inv, rv_qc, rv_error, rv_inc, & 
                     rf_obs, rf_inv, rf_qc, rf_error, rf_inc, zk
               end do
            enddo
         endif                 
      end do
    ENDIF




   if (rootproc) then
      close(omb_radar_unit)
      close(omb_radar_iter_unit)
      call da_free_unit(omb_radar_unit)
      call da_free_unit(omb_radar_iter_unit)
      deallocate (filename1)
   end if


   if (trace_use) call da_trace_exit("da_write_oa_radar_ascii")

end subroutine da_write_oa_radar_ascii





