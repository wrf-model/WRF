subroutine da_final_write_y(iv)

   !-------------------------------------------------------------------------   
   ! Purpose: Writes full diagnostics for y=H(x_inc)    
   !-------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(in)    :: iv   ! O-B structure.
      
   integer                       :: n, k,kk,i, ounit
   integer                       :: sound_num_obs, num_obs, ios
   character(len=filename_len), allocatable     :: filename(:)
   character(len=filename_len)                  :: ob_name, file_prefix  

   if (trace_use_dull) call da_trace_entry("da_final_write_y")

#ifdef DM_PARALLEL
   ! Ensure other processors have written their temporary files
   call mpi_barrier(comm, ierr)
#endif

   if (omb_add_noise) then
      ! perturbed ob run.
      file_prefix='pert_obs'
   else
      ! unperturbed ob run.
      file_prefix='unpert_obs'
   end if

   if (rootproc) then
      allocate (filename(0:num_procs-1))
      do k = 0,num_procs-1
         write(unit=filename(k),fmt ='(a,a,i4.4)')trim(file_prefix),'.',k
      end do
      call da_get_unit(ounit)
      open(unit=ounit,file=trim(file_prefix),form='formatted', &
         status='replace' , iostat=ios)
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open random observation error file"//file_prefix/))
   end if

   !------------------------------------------------------------------
   ! [1] writing Surface
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(synop)%nlocal > 0) then
      do n = 1, iv%info(synop)%nlocal
         if (iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'synop',5)
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'metar',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ships',5)
      end do
   end if

   !---------------------------------------------------------------
   ! [4] writing GeoAMV
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(geoamv)%nlocal > 0) then
      do n = 1, iv%info(geoamv)%nlocal
         if (iv%info(geoamv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'geoamv',6)
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'polaramv',8) 
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'gpspw',5)     
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
   sound_num_obs = num_obs
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'sound', sound_num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'sound',5)     
      end do  
      !  writing Sonde_sfc  
      write(ounit,'(a20,i8)')'sonde_sfc', sound_num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'sonde_sfc',9)
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'airep',5)   
      end do
   end if

   !------------------------------------------------------------------
   ! [8] writing Pilot  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
        if (iv%info(pilot)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'pilot',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmir',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmiT',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'satem',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmt1',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'ssmt2',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'qscat',5)    
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'profiler',8)    
      end do
   end if

   !---------------------------------------------------------------
   ! [16] writing Buoy 
   !---------------------------------------------------------------

   num_obs = 0
   if (iv%info(buoy)%nlocal > 0) then
      do n = 1, iv%info(buoy)%nlocal
         if (iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'buoy',4)    
      end do
   end if

   !---------------------------------------------------------------
   ! [17] writing  Bogus  
   !---------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         if (iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'bogus',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [18] writing AIRS retrievals:
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         if (iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'airsr', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'airsr',5)    
      end do
   end if

   !------------------------------------------------------------------
   ! [19] writing Radiance data:
   !------------------------------------------------------------------

   if (iv%num_inst > 0) then
      do i = 1, iv%num_inst                 ! loop for sensor
         do k = 1,iv%instid(i)%nchan        ! loop for channel
            !  Counting number of obs for channel k
            num_obs = 0
            do n = 1,iv%instid(i)%num_rad      ! loop for pixel
               if (iv%instid(i)%info%proc_domain(1,n) .and. &
                 (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            call da_proc_sum_int(num_obs)
            if (rootproc .and. num_obs > 0) then
               write(ob_name,'(a,a,i4.4)') &
                  trim(iv%instid(i)%rttovid_string),'-', &
		  iv%instid(i)%ichan(k)
               write(ounit,'(a20,i8)')  ob_name,num_obs
               num_obs = 0
               do kk = 0,num_procs-1
                  call da_read_y_unit(trim(filename(kk)),ounit,num_obs, &
                     trim(ob_name),len(trim(ob_name)))    
               end do
            end if
         end do                                  ! end loop for Channel 
      end do                                   ! end loop for sensor
   end if
   !------------------------------------------------------------------
   ! [20] writing gpsref:
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(gpsref)%nlocal > 0) then
      do n = 1, iv%info(gpsref)%nlocal
         if (iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'gpsref', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'gpsref',6)    
      end do
   end if

   !------------------------------------------------------------------
   ! writing gpseph:
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(gpseph)%nlocal > 0) then
      do n = 1, iv%info(gpseph)%nlocal
         if (iv%info(gpseph)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'gpseph', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'gpseph',6)
      end do
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
   if (rootproc .and. num_obs > 0) then
      write(ounit,'(a20,i8)')'rain', num_obs
      num_obs = 0
      do k = 0,num_procs-1                        
         call da_read_y_unit(trim(filename(k)),ounit,num_obs,'rain',4)
      end do
   end if      


   !------------------------------------------------------------------

   if (rootproc) then
      close(ounit)
      call da_free_unit(ounit)
      deallocate (filename)
   end if

   if (trace_use_dull) call da_trace_exit("da_final_write_y")
   
end subroutine da_final_write_y


