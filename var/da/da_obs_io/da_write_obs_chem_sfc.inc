subroutine da_write_obs_chem_sfc(it,ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   integer,        intent(in)    :: it
   type (y_type),  intent(in)    :: ob      ! Observation structure.
   type (iv_type), intent(in)    :: iv      ! O-B structure.
   type (y_type),  intent(inout) :: re      ! residual vector.
      
   integer                     :: n, k, num_obs, ios, ios2
   integer                     :: ounit, ounit2     ! Output unit           
   character(len=filename_len) :: filename, filename2
   integer :: itime, ifgat

   if (trace_use) call da_trace_entry("da_write_obs_chem_sfc")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   if (chemicda_opt == 5) then
     call da_get_unit(ounit)
     call da_get_unit(ounit2)
   else
     call da_get_unit(ounit)
   end if

#ifdef DM_PARALLEL
   if (chemicda_opt == 5) then
      write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'chem_omb_oma_',it,'.', myproc
      write(unit=filename2, fmt='(a,i2.2,a,i4.4)') 'gas_omb_oma_',it,'.', myproc
   else if (chemicda_opt == 4) then
      write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'gas_omb_oma_',it,'.', myproc
   else
      write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'chem_omb_oma_',it,'.', myproc
   end if
#else
   if (chemicda_opt == 5) then
      write(unit=filename, fmt='(a,i2.2,a)') 'chem_omb_oma_',it,'.0000'
      write(unit=filename2, fmt='(a,i2.2,a)') 'gas_omb_oma_',it,'.0000'
   else if (chemicda_opt == 4) then
      write(unit=filename, fmt='(a,i2.2,a)') 'gas_omb_oma_',it,'.0000'
   else
      write(unit=filename, fmt='(a,i2.2,a)') 'chem_omb_oma_',it,'.0000'
   end if
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open conventional observation omb and oma file"//filename/))
   end if

   if (chemicda_opt == 5) then
   open (unit=ounit2,file=trim(filename2),form='formatted',status='replace', &
      iostat=ios2)
   if (ios2 /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open conventional observation omb and oma file"//filename2/))
   end if
   end if

   num_obs = 0
   do n = 1, iv%info(chemic_surf)%nlocal
      if (iv%info(chemic_surf)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'chem', num_obs  
      if (chemicda_opt == 5) then
        write(ounit2,'(a20,i8)')'chem', num_obs  
      end if
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
            if (chemicda_opt == 5) then 
              write(ounit2,'(2i8)') 1, ifgat
            end if
            if (chemicda_opt == 1) then
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
                 ob%chemic_surf(n)%chem(p_chemsi_pm25), &
                 iv%chemic_surf(n)%chem(p_chemsi_pm25)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm25)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm25)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_pm25)
            else if (chemicda_opt == 2) then
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
                 ob%chemic_surf(n)%chem(p_chemsi_pm10), &
                 iv%chemic_surf(n)%chem(p_chemsi_pm10)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm10)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm10)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_pm10)

            else if (chemicda_opt == 3) then
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
                 ob%chemic_surf(n)%chem(p_chemsi_pm25), &
                 iv%chemic_surf(n)%chem(p_chemsi_pm25)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm25)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm25)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_pm25), &
                 ob%chemic_surf(n)%chem(p_chemsi_pm10), &
                 iv%chemic_surf(n)%chem(p_chemsi_pm10)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm10)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm10)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_pm10)  

            else if (chemicda_opt == 4) then
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
                 ob%chemic_surf(n)%chem(p_chemsi_so2), &
                 iv%chemic_surf(n)%chem(p_chemsi_so2)%inv, iv%chemic_surf(n)%chem(p_chemsi_so2)%qc, iv%chemic_surf(n)%chem(p_chemsi_so2)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_so2), &
                 ob%chemic_surf(n)%chem(p_chemsi_no2), &
                 iv%chemic_surf(n)%chem(p_chemsi_no2)%inv, iv%chemic_surf(n)%chem(p_chemsi_no2)%qc, iv%chemic_surf(n)%chem(p_chemsi_no2)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_no2), &
                 ob%chemic_surf(n)%chem(p_chemsi_o3), &
                 iv%chemic_surf(n)%chem(p_chemsi_o3)%inv, iv%chemic_surf(n)%chem(p_chemsi_o3)%qc, iv%chemic_surf(n)%chem(p_chemsi_o3)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_o3), &
                 ob%chemic_surf(n)%chem(p_chemsi_co), &
                 iv%chemic_surf(n)%chem(p_chemsi_co)%inv, iv%chemic_surf(n)%chem(p_chemsi_co)%qc, iv%chemic_surf(n)%chem(p_chemsi_co)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_co)  

            else if (chemicda_opt == 5) then
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
                 ob%chemic_surf(n)%chem(p_chemsi_pm25), &
                 iv%chemic_surf(n)%chem(p_chemsi_pm25)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm25)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm25)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_pm25), &
                 ob%chemic_surf(n)%chem(p_chemsi_pm10), &
                 iv%chemic_surf(n)%chem(p_chemsi_pm10)%inv, iv%chemic_surf(n)%chem(p_chemsi_pm10)%qc, iv%chemic_surf(n)%chem(p_chemsi_pm10)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_pm10)

            write(ounit2,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n), &       ! Latitude
               iv%info(chemic_surf)%lon(1,n), &       ! Longitude
               iv%info(chemic_surf)%lon(1,n),         &
                 ob%chemic_surf(n)%chem(p_chemsi_so2), &
                 iv%chemic_surf(n)%chem(p_chemsi_so2)%inv, iv%chemic_surf(n)%chem(p_chemsi_so2)%qc, iv%chemic_surf(n)%chem(p_chemsi_so2)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_so2), &
                 ob%chemic_surf(n)%chem(p_chemsi_no2), &
                 iv%chemic_surf(n)%chem(p_chemsi_no2)%inv, iv%chemic_surf(n)%chem(p_chemsi_no2)%qc, iv%chemic_surf(n)%chem(p_chemsi_no2)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_no2), &
                 ob%chemic_surf(n)%chem(p_chemsi_o3), &
                 iv%chemic_surf(n)%chem(p_chemsi_o3)%inv, iv%chemic_surf(n)%chem(p_chemsi_o3)%qc, iv%chemic_surf(n)%chem(p_chemsi_o3)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_o3), &
                 ob%chemic_surf(n)%chem(p_chemsi_co), &
                 iv%chemic_surf(n)%chem(p_chemsi_co)%inv, iv%chemic_surf(n)%chem(p_chemsi_co)%qc, iv%chemic_surf(n)%chem(p_chemsi_co)%error, &
                 re%chemic_surf(n)%chem(p_chemsi_co)  
            end if
         end if
      end do
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (chemicda_opt == 5) then
     close (ounit2)
     call da_free_unit(ounit2)
   end if

   if (trace_use) call da_trace_exit("da_write_obs_chem_sfc")

end subroutine da_write_obs_chem_sfc


