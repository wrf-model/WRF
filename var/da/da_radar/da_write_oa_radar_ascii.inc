subroutine da_write_oa_radar_ascii ( ob, iv, re )

   !---------------------------------------------------------------------------
   ! Purpose: write out OMB and OMA vector structure for radar data.
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.
   type (y_type),     intent(in)  :: re       ! O-A structure.

   integer                        :: n , num_obs       ! Loop counter.
   integer                        :: i, k     ! Index dimension.
   integer                        :: nlevelss ! Number of obs levels.

   integer            :: ios, oma_radar_unit
   character(len=filename_len)  :: filename
   integer            :: ndomain


    if (trace_use) call da_trace_entry("da_write_oa_radar_ascii")
#ifdef DM_PARALLEL
      write(unit=filename, fmt='(a,i3.3)') 'radar_omb_oma.', myproc
#else
      write(unit=filename, fmt='(a)') 'radar_omb_oma.000'
#endif


   call da_get_unit(oma_radar_unit)
   open(unit=oma_radar_unit,file=trim(filename),form='formatted',iostat=ios)
   if (ios /= 0) Then
       call da_error(__FILE__,__LINE__, &
         (/"Cannot open oma radiance file"//filename/))
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
                 write(oma_radar_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7))')&
                    num_obs , k, 'RADAR', &  
                    iv % info (radar)% lat(1,n), &       ! Latitude
                    iv % info (radar) % lon(1,n), &       ! Longitude
                    iv % radar(n) % height(k), &           ! Obs height in m
                    ob%radar(n)%rv(k),&
                    iv%radar(n)%rv(k)%inv,iv%radar(n)%rv(k)%qc,iv%radar(n)%rv(k)%error,&
                    re%radar(n)%rv(k), &! O, O-B, O-A rv
                    ob%radar(n)%rf(k), &
                    iv%radar(n)%rf(k)%inv,iv%radar(n)%rf(k)%qc,iv%radar(n)%rf(k)%error,&
                    re%radar(n)%rf(k)   ! O, O-B, O-A rf
               end do
            end if
         end do
      end if
   end if

   close (oma_radar_unit)
   call da_free_unit(oma_radar_unit)

   if (trace_use) call da_trace_exit("da_write_oa_radar_ascii")

end subroutine da_write_oa_radar_ascii





