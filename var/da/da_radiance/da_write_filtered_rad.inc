subroutine da_write_filtered_rad(ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: write out QCed radiance data.
   !
   !  METHOD: write out for separate PEs 
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i        ! Index dimension.

   integer            :: ios, filtered_rad_unit
   character(len=50)  :: filename

   if (trace_use) call da_trace_entry("da_write_filtered_rad")

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

#ifdef DM_PARALLEL
      write(unit=filename, fmt='(a,i4.4)') &
         'filtered_'//trim(iv%instid(i)%rttovid_string)//'.', myproc
#else
      write(unit=filename, fmt='(a)') &
         'filtered_'//trim(iv%instid(i)%rttovid_string)
#endif
      
      call da_get_unit(filtered_rad_unit)
      open(unit=filtered_rad_unit,file=trim(filename), &
         form='unformatted',iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open filtered radiance file"//filename/))
      Endif

      write(unit=filtered_rad_unit) iv%instid(i)%num_rad

      do n =1,iv%instid(i)%num_rad
         write(unit=filtered_rad_unit) n, &
                   iv%instid(i)%info%date_char(n), &
                   iv%instid(i)%scanpos(n)        , &
                   iv%instid(i)%ifgat(n)          , &
                   iv%instid(i)%landsea_mask(n)   , &
                   iv%instid(i)%info%elv(n)   , &
                   iv%instid(i)%info%lat(1,n) , &
                   iv%instid(i)%info%lon(1,n) , &
                   iv%instid(i)%satzen(n)         , &
                   iv%instid(i)%satazi(n)         , &
                   ob%instid(i)%tb(:,n)           , &
                   iv%instid(i)%tb_error(:,n)     , &
                   iv%instid(i)%tb_qc(:,n)

      end do     ! end do pixels
      close(unit=filtered_rad_unit)
      call da_free_unit(filtered_rad_unit)
   end do    !! end do instruments

   if (trace_use) call da_trace_exit("da_write_filtered_rad")
 
end subroutine da_write_filtered_rad


