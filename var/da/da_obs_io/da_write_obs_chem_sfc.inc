subroutine da_write_obs_chem_sfc(it,ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   integer,        intent(in)    :: it
   type (y_type),  intent(in)    :: ob      ! Observation structure.
   type (iv_type), intent(in)    :: iv      ! O-B structure.
   type (y_type),  intent(inout) :: re      ! residual vector.
      
   integer                     :: n, k, num_obs, ios
   integer                     :: ounit     ! Output unit           
   character(len=filename_len) :: filename
   integer :: itime, ifgat

   if (trace_use) call da_trace_entry("da_write_obs_chem_sfc")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'chem_omb_oma_',it,'.', myproc
#else
    write(unit=filename, fmt='(a,i2.2,a)') 'chem_omb_oma_',it,'.0000'
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open conventional observation omb and oma file"//filename/))
   end if

   num_obs = 0
   do n = 1, iv%info(chemic_surf)%nlocal
      if (iv%info(chemic_surf)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'chem', num_obs  
      num_obs = 0
      do n = 1, iv%info(chemic_surf)%nlocal  
         do itime = 1, num_fgat_time
            if ( n >= iv%info(chemic_surf)%plocal(itime-1)+1 .and. &
                 n <= iv%info(chemic_surf)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do

         if (iv%info(chemic_surf)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
               ob%chemic_surf(n)%chem(p_chemsi_pm25), &
               iv%chemic_surf(n)%chem(p_chemsi_pm25)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm25)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm25)%error, &
               re%chemic_surf(n)%chem(p_chemsi_pm25)
         end if
      end do
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs_chem_sfc")

end subroutine da_write_obs_chem_sfc


