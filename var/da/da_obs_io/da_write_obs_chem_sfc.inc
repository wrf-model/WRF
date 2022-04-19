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

   character(len=120) :: fmt_chem1 = '(i8,2x,a6,2f11.6,2f11.2,i8,2f11.2)'     ! pm2.5 or pm10
   character(len=120) :: fmt_chem2 = '(i8,2x,a6,2f11.6,2(2f11.2,i8,2f11.2))'  ! pm2.5 and pm10
   character(len=120) :: fmt_chem4 = '(i8,2x,a6,2f11.6,4(2f12.3,i8,2f12.3))'  ! 4 gas species
   integer :: itime, ifgat
   integer :: ipcc, jpcc, ic
   integer, parameter       :: ngas = 4
   integer, dimension(ngas) :: ipc
   ipc = (/p_chemsi_so2, p_chemsi_no2, p_chemsi_o3, p_chemsi_co/)

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
         (/"Cannot open chemical observation omb and oma file"//filename/))
   end if

   if (chemicda_opt == 5) then
   open (unit=ounit2,file=trim(filename2),form='formatted',status='replace', &
      iostat=ios2)
   if (ios2 /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open gas observation omb and oma file"//filename2/))
   end if
   end if

   num_obs = 0
   do n = 1, iv%info(chemic_surf)%nlocal
      if (iv%info(chemic_surf)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      if (chemicda_opt == 4) then
          write(ounit,'(a20,i8)') 'gas', num_obs
      else
          write(ounit,'(a20,i8)') 'chem', num_obs
      endif
      if (chemicda_opt == 5) then
          write(ounit2,'(a20,i8)') 'gas', num_obs
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

            ipcc = p_chemsi_pm25
            jpcc = p_chemsi_pm10
            if (chemicda_opt <= 2) then
               if(chemicda_opt == 2) ipcc = p_chemsi_pm10
               write(ounit, fmt = fmt_chem1)         &
               num_obs , iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n),        &  ! Latitude
               iv%info(chemic_surf)%lon(1,n),        &  ! Longitude
               ob%chemic_surf(n)%chem(ipcc),         &  ! observation
               iv%chemic_surf(n)%chem(ipcc)%inv,     &  ! o-b
               iv%chemic_surf(n)%chem(ipcc)%qc,      &  ! qc
               iv%chemic_surf(n)%chem(ipcc)%error,   &  ! obs error
               re%chemic_surf(n)%chem(ipcc)             ! o-a
            else if (chemicda_opt == 4) then
               write(ounit, fmt = fmt_chem4)         &
               num_obs , iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n),        &  ! Latitude
               iv%info(chemic_surf)%lon(1,n),        &  ! Longitude
              (ob%chemic_surf(n)%chem(ipc(ic)),      &  ! observation
               iv%chemic_surf(n)%chem(ipc(ic))%inv,  &  ! o-b
               iv%chemic_surf(n)%chem(ipc(ic))%qc,   &  ! qc
               iv%chemic_surf(n)%chem(ipc(ic))%error,&  ! obs error
               re%chemic_surf(n)%chem(ipc(ic)), ic=1,ngas) ! o-a
            else  ! chemicda_opt == 3 or 5
               write(ounit, fmt = fmt_chem2)         &
               num_obs , iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n),        &  ! Latitude
               iv%info(chemic_surf)%lon(1,n),        &  ! Longitude
               ob%chemic_surf(n)%chem(ipcc),         &  ! observation
               iv%chemic_surf(n)%chem(ipcc)%inv,     &  ! o-b
               iv%chemic_surf(n)%chem(ipcc)%qc,      &  ! qc
               iv%chemic_surf(n)%chem(ipcc)%error,   &  ! obs error
               re%chemic_surf(n)%chem(ipcc),         &  ! o-a
               ob%chemic_surf(n)%chem(jpcc),         &  ! observation
               iv%chemic_surf(n)%chem(jpcc)%inv,     &  ! o-b
               iv%chemic_surf(n)%chem(jpcc)%qc,      &  ! qc
               iv%chemic_surf(n)%chem(jpcc)%error,   &  ! obs error
               re%chemic_surf(n)%chem(jpcc)             ! o-a
              if(chemicda_opt == 5) then
               write(ounit2, fmt = fmt_chem4)        &
               num_obs , iv%info(chemic_surf)%id(n), &  ! Station
               iv%info(chemic_surf)%lat(1,n),        &  ! Latitude
               iv%info(chemic_surf)%lon(1,n),        &  ! Longitude
              (ob%chemic_surf(n)%chem(ipc(ic)),      &  ! observation
               iv%chemic_surf(n)%chem(ipc(ic))%inv,  &  ! o-b
               iv%chemic_surf(n)%chem(ipc(ic))%qc,   &  ! qc
               iv%chemic_surf(n)%chem(ipc(ic))%error,&  ! obs error
               re%chemic_surf(n)%chem(ipc(ic)), ic=1,ngas) ! o-a
              endif  ! (chemicda_opt == 5)
            endif

         end if      ! if (iv%info(chemic_surf)%proc_domain(1,n)) then
      end do         ! do n = 1, iv%info(chemic_surf)%nlocal
   end if            ! if (num_obs > 0) then

   close (ounit)
   call da_free_unit(ounit)

   if (chemicda_opt == 5) then
     close (ounit2)
     call da_free_unit(ounit2)
   end if

   if (trace_use) call da_trace_exit("da_write_obs_chem_sfc")

end subroutine da_write_obs_chem_sfc
