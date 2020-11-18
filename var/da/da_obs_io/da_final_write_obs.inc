subroutine da_final_write_obs(it,iv)

   !-------------------------------------------------------------------------
   ! Purpose: Writes full diagnostics for O, (O-B) & OMA together
   !-------------------------------------------------------------------------   

   implicit none
 
   integer,        intent(in)    :: it
   type (iv_type), intent(in)    :: iv      ! O-B structure.
   integer                       :: n, k, iunit
   integer                       :: ios  ! Error code from MPI routines.
   integer                       :: num_obs
   logical                       :: if_wind_sd
   character(len=filename_len), allocatable     :: filename(:)
   character(len=filename_len)                  :: file


   if (trace_use) call da_trace_entry("da_final_write_obs")

#ifdef DM_PARALLEL
   ! Wait to ensure all temporary files have been written
   call mpi_barrier(comm, ierr)
#endif

   if (rootproc) then
      call da_get_unit(iunit)
      allocate (filename(0:num_procs-1))
      do k = 0,num_procs-1
         write(unit=filename(k),fmt ='(a,i2.2,a,i4.4)')'gts_omb_oma_',it,'.',k
      end do 
      call da_get_unit(omb_unit)
       write(unit=file,fmt ='(a,i2.2)')'gts_omb_oma_',it
      open(unit=omb_unit,file=trim(file),form='formatted', status='replace', iostat=ios) 
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//file/))
   end if

   num_obs = 0
   if (iv%info(synop)%nlocal > 0) then
      do n = 1, iv%info(synop)%nlocal
         if(iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_synop) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'synop', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'synop',5,if_wind_sd)
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
   if_wind_sd = .false.
   if (wind_sd_metar) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,20i8)')'metar', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'metar',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [3] writing Ships
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ships)%nlocal > 0) then
      do n = 1, iv%info(ships)%nlocal
         if(iv%info(ships)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_ships) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ships', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ships',5,if_wind_sd)
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
   if_wind_sd = .false.
   if (wind_sd_geoamv) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'geoamv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'geoamv',6,if_wind_sd)
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
   if_wind_sd = .false.
   if (wind_sd_polaramv) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'polaramv', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'polaramv',8,if_wind_sd) 
      end do
   end if

   !------------------------------------------------------------------
   ! [5] writing GPSPW  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(gpspw)%nlocal > 0) then
      do n = 1, iv%info(gpspw)%nlocal
         if(iv%info(gpspw)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'gpspw', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'gpspw',5,if_wind_sd)     
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
   if_wind_sd = .false.
   if (wind_sd_sound) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'sound', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'sound',5,if_wind_sd) 
      end do
   end if

!  Now sonde_sfc
   num_obs = 0
   if (iv%info(sonde_sfc)%nlocal > 0) then
      do n = 1, iv%info(sonde_sfc)%nlocal
         if(iv%info(sonde_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'sonde_sfc', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'sonde_sfc',9,if_wind_sd)
      end do
   end if

   !------------------------------------------------------------------
   ! [7] writing Airep  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(airep)%nlocal > 0) then
      do n = 1, iv%info(airep)%nlocal
         if(iv%info(airep)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_airep) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'airep', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'airep',5,if_wind_sd)
      end do
   end if

   !------------------------------------------------------------------
   ! [8] writing Pilot  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
         if(iv%info(pilot)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_pilot) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'pilot', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'pilot',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [9] writing ssmi_rv
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ssmi_rv)%nlocal > 0) then
      do n = 1, iv%info(ssmi_rv)%nlocal
         if(iv%info(ssmi_rv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmir', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmir',5,if_wind_sd)    
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
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmiT', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmiT',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [11] writing SATEM  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(satem)%nlocal > 0) then
      do n = 1, iv%info(satem)%nlocal
         if(iv%info(satem)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'satem', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'satem',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [12] writing SSMT1  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ssmt1)%nlocal > 0) then
      do n = 1, iv%info(ssmt1)%nlocal
         if(iv%info(ssmt1)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmt1', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmt1',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [13] writing SSMT2  
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(ssmt2)%nlocal > 0) then
      do n = 1, iv%info(ssmt2)%nlocal
         if(iv%info(ssmt2)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'ssmt2', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'ssmt2',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [14] writing QSCAT  
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(qscat)%nlocal > 0) then
      do n = 1, iv%info(qscat)%nlocal
         if(iv%info(qscat)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_qscat) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'qscat', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'qscat',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [15] writing Profiler
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(profiler)%nlocal > 0) then
      do n = 1, iv%info(profiler)%nlocal
         if(iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_profiler) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'profiler', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'profiler',8,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [16] writing Buoy 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(buoy)%nlocal > 0) then
      do n = 1, iv%info(buoy)%nlocal
         if(iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_buoy) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'buoy', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'buoy',4,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [17] writing Bogus 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         if(iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'bogus', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'bogus',5,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [18] writing Tamdar
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(tamdar)%nlocal > 0) then
      do n = 1, iv%info(tamdar)%nlocal
         if (iv%info(tamdar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (wind_sd_tamdar) if_wind_sd = .true.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'tamdar', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'tamdar',6,if_wind_sd)
      end do

   end if

!  Now tamdar_sfc
   num_obs = 0
   if (iv%info(tamdar_sfc)%nlocal > 0) then
      do n = 1, iv%info(tamdar_sfc)%nlocal
         if(iv%info(tamdar_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'tamdar_sfc', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'tamdar_sfc',10,if_wind_sd)
      end do
   end if

   !------------------------------------------------------------------
   ! [19] writing AIRS retrievals:
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         if(iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'airsr', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'airsr',5,if_wind_sd)
      end do
   end if

   !------------------------------------------------------------------
   ! [20] writing GPS refractivity
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(gpsref)%nlocal > 0) then
      do n = 1, iv%info(gpsref)%nlocal
         if(iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'gpsref', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'gpsref',6,if_wind_sd)    
      end do
   end if

   !------------------------------------------------------------------
   ! [20.1] writing GPS Excess Phase
   !------------------------------------------------------------------

   num_obs = 0
   if (iv%info(gpseph)%nlocal > 0) then
      do n = 1, iv%info(gpseph)%nlocal
         if(iv%info(gpseph)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'gpseph', num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'gpseph',6,if_wind_sd)
      end do
   end if

   !------------------------------------------------------------------
   ! [21] writing rain 
   !------------------------------------------------------------------
   
   num_obs = 0
   if (iv%info(rain)%nlocal > 0) then
      do n = 1, iv%info(rain)%nlocal
         if(iv%info(rain)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if_wind_sd = .false.
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)')'rain', num_obs  
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,'rain',4,if_wind_sd)    
      end do
   end if


   if (rootproc) then
      close(iunit)
      close(omb_unit)
      call da_free_unit(iunit)
      call da_free_unit(omb_unit)
      deallocate (filename)
   end if

   if (trace_use) call da_trace_exit("da_final_write_obs")
   
end subroutine da_final_write_obs


