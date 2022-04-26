subroutine da_final_write_obs_gas_sfc(it,iv)

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

   !local
   character(len=4) :: typestr
   typestr = 'gas'

   if (trace_use) call da_trace_entry("da_final_write_obs_gas_sfc")

#ifdef DM_PARALLEL
   ! Wait to ensure all temporary files have been written
   call mpi_barrier(comm, ierr)
#endif

   if (rootproc) then
      call da_get_unit(iunit)
      allocate (filename(0:num_procs-1))
      do k = 0,num_procs-1
         write(unit=filename(k),fmt ='(a,i2.2,a,i4.4)')'gas_omb_oma_',it,'.',k
      end do 
      call da_get_unit(omb_unit)
       write(unit=file,fmt ='(a,i2.2)')'gas_omb_oma_',it
      open(unit=omb_unit,file=trim(file),form='formatted', status='replace', iostat=ios) 
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//file/))
   end if

   num_obs = 0
   if (iv%info(chemic_surf)%nlocal > 0) then
      do n = 1, iv%info(chemic_surf)%nlocal
         if(iv%info(chemic_surf)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
   end if
   call da_proc_sum_int(num_obs)
   if (num_obs > 0 .and. rootproc) then
      write(omb_unit,'(a20,i8)') typestr, num_obs
      num_obs = 0
      do k = 0,num_procs-1
         call da_read_omb_tmp(filename(k),iunit,num_obs,typestr,3,if_wind_sd)
      end do
   end if


   if (rootproc) then
      close(iunit)
      close(omb_unit)
      call da_free_unit(iunit)
      call da_free_unit(omb_unit)
      deallocate (filename)
   end if

   if (trace_use) call da_trace_exit("da_final_write_obs_gas_sfc")
   
end subroutine da_final_write_obs_gas_sfc


