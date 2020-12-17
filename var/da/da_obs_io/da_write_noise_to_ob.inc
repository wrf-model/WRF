subroutine da_write_noise_to_ob (iv) 

   !-------------------------------------------------------------------------
   ! Purpose: Write consolidated obs noise created in da_add_noise_to_ob
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv   ! Obs and header structure.
   integer                       :: n, k,kk,i, iunit
   integer                       :: num_obs
   character(len=filename_len), allocatable     :: filename(:)   
   character*20                  :: ob_name   

   call da_trace_entry("da_write_noise_to_ob")

#ifdef DM_PARALLEL
   ! Ensure other processors have written their temporary files
   call mpi_barrier(comm, ierr)
#endif
   call da_get_unit (iunit)
   allocate (filename(0:num_procs-1))
   do k = 0,num_procs-1
      write(unit=filename(k), fmt='(a,i4.4)')'rand_obs_error.',k
   end do
   if (rootproc) then
      call da_get_unit (rand_unit)
      open(unit=rand_unit,file='rand_obs_error',form='formatted', &
         iostat=ierr,status='unknown')
!         iostat=ierr,status='new')
      if (ierr /= 0) &
         call da_error(__FILE__,__LINE__, (/"Cannot open file rand_obs_error"/))
   end if

   num_obs = 0
   if (iv%info(synop)%nlocal > 0) then
      do n = 1, iv%info(synop)%nlocal
         if (iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'synop',5)
      end do
   end if

   !------------------------------------------------------------------
   ! [2] writing Metar
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(metar)%nlocal > 0) then
      do n = 1, iv%info(metar)%nlocal
         if (iv%info(metar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'metar',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [3] writing Ships
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ships)%nlocal > 0) then
      do n = 1, iv%info(ships)%nlocal
         if (iv%info(ships)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ships',5)
      end do
   end if

   !------------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(geoamv)%nlocal > 0) then
      do n = 1, iv%info(geoamv)%nlocal
         if (iv%info(geoamv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'geoamv',6)
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing PolarAMV
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(polaramv)%nlocal > 0) then
      do n = 1, iv%info(polaramv)%nlocal
         if (iv%info(polaramv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      if (rootproc) write(rand_unit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'polaramv',8) 
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing GPSPW  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(gpspw)%nlocal > 0) then
      do n = 1, iv%info(gpspw)%nlocal
         if (iv%info(gpspw)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'gpspw',5)     
      end do
   end if

   !------------------------------------------------------------------
   ! [6] writing Sonde  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(sound)%nlocal > 0) then
      do n = 1, iv%info(sound)%nlocal
         if (iv%info(sound)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'sound', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'sound',5)     
      end do
      !  writing Sonde_sfc  
      if (rootproc) write(rand_unit,'(a20,i8)')'sonde_sfc', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'sonde_sfc',9)
      end do
   end if

   !------------------------------------------------------------------
   ! [7] writing Airep  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(airep)%nlocal > 0) then
      do n = 1, iv%info(airep)%nlocal
         if (iv%info(airep)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'airep',5)   
      end do
   end if

   !------------------------------------------------------------------
   ! [8] writing   
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
         if (iv%info(pilot)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'pilot',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [9] writing ssmi_rv
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(ssmi_rv)%nlocal > 0) then
      do n = 1, iv%info(ssmi_rv)%nlocal
         if (iv%info(ssmi_rv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmir',5)    
     end do
   end if

   !------------------------------------------------------------------
   ! [10] writing SSMITB
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ssmi_tb)%nlocal > 0) then
      do n = 1, iv%info(ssmi_tb)%nlocal
         if (iv%info(ssmi_tb)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmiT',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [11] writing SATEM  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(satem)%nlocal > 0) then
      do n = 1, iv%info(satem)%nlocal
         if (iv%info(satem)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'satem',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [12] writing SSMT1  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ssmt1)%nlocal > 0) then
      do n = 1, iv%info(ssmt1)%nlocal
         if (iv%info(ssmt1)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmt1',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [13] writing SSMT2  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(ssmt2)%nlocal > 0) then
      do n = 1, iv%info(ssmt2)%nlocal
         if (iv%info(ssmt2)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'ssmt2',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [14] writing QSCAT  
   !------------------------------------------------------------------
    
   num_obs = 0
   if (iv%info(qscat)%nlocal > 0) then
      do n = 1, iv%info(qscat)%nlocal
         if (iv%info(qscat)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'qscat',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [15] writing Profiler
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(profiler)%nlocal > 0) then
      do n = 1, iv%info(profiler)%nlocal
         if (iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'profiler',8)    
      end do
   end if

   !------------------------------------------------------------------
   ! [16] writing Buoy 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(buoy)%nlocal > 0) then
      do n = 1, iv%info(buoy)%nlocal
         if (iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'buoy',4)    
      end do
   end if

   !------------------------------------------------------------------
   ! [17] writing  Bogus 
   !------------------------------------------------------------------
  
   num_obs = 0
   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         if (iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'bogus',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [18] writing AIRS retrievals
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         if (iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
       write(rand_unit,'(a20,i8)')'airsr', num_obs  
       num_obs = 0
       do k = 0,num_procs-1
          call da_read_rand_unit(filename(k),iunit,num_obs,'airsr',5)    
       end do
    end if

   !------------------------------------------------------------------
   ! [19] writing gpsref
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(gpsref)%nlocal > 0) then
      do n = 1, iv%info(gpsref)%nlocal
         if (iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,20i8)')'gpsref', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'gpsref',6)    
      end do
   end if

   !------------------------------------------------------------------
   ! writing gpseph
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(gpseph)%nlocal > 0) then
      do n = 1, iv%info(gpseph)%nlocal
         if (iv%info(gpseph)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,20i8)')'gpseph', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'gpseph',6)
      end do
   end if

   !------------------------------------------------------------------
   ! [20] writing Radiance data:  
   !------------------------------------------------------------------

   if (iv%num_inst > 0) then
      do i = 1, iv%num_inst                 ! loop for sensor
         !if (iv%instid(i)%num_rad < 1) cycle ! may broken da_proc_sum_int(num_obs) if some PE num_rad=0
         do k = 1,iv%instid(i)%nchan        ! loop for channel
            ! Counting number of obs for channle k
            num_obs = 0
            do n = 1,iv%instid(i)%num_rad      ! loop for pixel
               if (iv%instid(i)%info%proc_domain(1,n) .and. &
                  (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            call da_proc_sum_int(num_obs)
            if (rootproc .and. num_obs > 0) then
               write (ob_name,'(a,a,i4.4)') &
                  trim(iv%instid(i)%rttovid_string),'-',iv%instid(i)%ichan(k)
               write (rand_unit,'(a20,i8)')  ob_name,num_obs
               num_obs = 0
               do kk = 0,num_procs-1
                  call da_read_rand_unit(filename(kk),iunit,num_obs, &
                     trim(ob_name),len(trim(ob_name)))
               end do
            end if
         end do                           ! end loop for channel
      end do                            ! end loop for sensor
   end if

   !------------------------------------------------------------------
   ! [21] writing RAIN 
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(rain)%nlocal > 0) then
      do n = 1, iv%info(rain)%nlocal
         if (iv%info(rain)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(rand_unit,'(a20,i8)')'rain', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_rand_unit(filename(k),iunit,num_obs,'rain',4)
      end do
   end if


   call da_free_unit (iunit)
!rizvi   call da_free_unit (rand_unit)
   if (rootproc ) call da_free_unit (rand_unit)
   deallocate (filename)
   call da_trace_exit("da_write_noise_to_ob")
   
end subroutine da_write_noise_to_ob  


