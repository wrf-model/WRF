subroutine da_read_filtered_rad( iv )

   !---------------------------------------------------------------------------
   ! Purpose: read in QCed radiance data from separated PEs 
   !---------------------------------------------------------------------------

   implicit none


   type (iv_type), intent(inout)  :: iv       ! O-B structure.

   integer              :: n        ! Loop counter.
   integer              :: i        ! Index dimension.

   integer              :: ios, filtered_rad_unit
   character(len=50)    :: filename
   integer              :: ndomain
   logical, allocatable :: outside(:,:)

   if (trace_use) call da_trace_entry("da_read_filtered_rad")

   do i = 1, iv%num_inst

#ifdef DM_PARALLEL
      write(filename, '(a,i4.4)') 'filtered_'//trim(iv%instid(i)%rttovid_string)//'.', myproc
#else
      write(filename, '(a)') 'filtered_'//trim(iv%instid(i)%rttovid_string)
#endif

      if (print_detail_rad) then
         write(unit=stdout,fmt='(2a)') 'Reading in ', trim(filename) 
      end if

      call da_get_unit(filtered_rad_unit)
      open(unit=filtered_rad_unit,file=trim(filename),form='unformatted',status='old',iostat=ios)
      if (ios /= 0) then
         write(unit=stdout,fmt='(2a)') 'Cannot open filtered radiance file ', filename
         iv%instid(i)%num_rad = 0
         cycle
      end if

      read(unit=filtered_rad_unit) iv%instid(i)%num_rad
      if (iv%instid(i)%num_rad < 1) cycle
      if (print_detail_rad) then
         write(unit=stdout,fmt='(a,i3,2x,a,3x,i10)')  ' allocating space for radiance innov structure', &
                               n, iv%instid(i)%rttovid_string, iv%instid(i)%num_rad
      end if

      allocate (iv%instid(i)%tb_inv(1:iv%instid(i)%nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_qc(1:iv%instid(i)%nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_error(1:iv%instid(i)%nchan,iv%instid(i)%num_rad))

      do n=1,iv%instid(i)%num_rad
         read(unit=filtered_rad_unit) ndomain, &
            iv%instid(i)%info%date_char(n), &
            iv%instid(i)%scanpos(n)        , &
            iv%instid(i)%ifgat(n)          , &
            iv%instid(i)%landsea_mask(n)   , &
            iv%instid(i)%info%elv(n)   , &
            iv%instid(i)%info%lat(1,n) , &
            iv%instid(i)%info%lon(1,n) , &
            iv%instid(i)%satzen(n)         , &
            iv%instid(i)%satazi(n)         , &
            iv%instid(i)%tb_inv(:,n)       , &
            iv%instid(i)%tb_error(:,n)     , &
            iv%instid(i)%tb_qc(:,n)
         iv%instid(i)%info%lat(2:,n) = iv%instid(i)%info%lat(1,n)
         iv%instid(i)%info%lon(2:,n) = iv%instid(i)%info%lon(1,n)

         allocate (outside(iv%instid(i)%nlevels, iv%instid(i)%num_rad))
         call da_llxy_new (iv%instid(i)%info, outside)
         deallocate (outside)

      end do     ! end do pixels
      close(filtered_rad_unit)
      call da_free_unit(filtered_rad_unit)

      iv%total_rad_pixel   = iv%total_rad_pixel + iv%instid(i)%num_rad
      iv%total_rad_channel = iv%total_rad_channel + iv%instid(i)%num_rad*iv%instid(i)%nchan
   end do      ! end do instruments

   if (trace_use) call da_trace_exit("da_read_filtered_rad")
 
end subroutine da_read_filtered_rad

