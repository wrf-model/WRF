subroutine da_read_iv_rad_for_multi_inc (it,ob, iv )

   !---------------------------------------------------------------------------
   ! Purpose: read out innovation vector structure for radiance data.
   !---------------------------------------------------------------------------

   implicit none

   integer      ,     intent(in)  :: it       ! outer loop count
   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, k, l, m, m1, m2,my,nobs_tot,nobs_in,iobs  ! Index dimension.
   integer                        :: nlevelss ! Number of obs levels.

   integer            :: ios, innov_rad_unit_in
   character(len=filename_len)  :: filename
   character(len=7)   :: surftype
   integer            :: ndomain
   logical            :: amsr2,fexist
   real, allocatable  :: data2d(:,:)
   real, allocatable  :: data3d(:,:,:)
   real, allocatable  :: data2d_g(:,:)
   real, allocatable  :: data3d_g(:,:,:)

   if (trace_use) call da_trace_entry("da_read_iv_rad_ascii")

   write(unit=message(1),fmt='(A)') 'Reading radiance OMB for multi_inc'
   call da_message(message(1:1))

   do i = 1, iv%num_inst
      
      nobs_tot = iv%instid(i)%info%ptotal(num_fgat_time) - iv%instid(i)%info%ptotal(0)
      do m=num_fgat_time,1,-1
         if ( nobs_tot > 0 ) then
            write(unit=filename, fmt='(a,i3.3)') 'inv_'//trim(iv%instid(i)%rttovid_string)//'_',m
            call da_get_unit(innov_rad_unit_in)
            inquire (file=filename, exist=fexist)
            if (.not. fexist) then
               exit
            else
            open(unit=innov_rad_unit_in,file=trim(filename),form='unformatted',status='old',iostat=ios)
            if (ios /= 0 ) then
               call da_error(__FILE__,__LINE__, &
               (/"Cannot open innovation radiance file"//filename/))
            Endif
            write(unit=message(1),fmt='(A)') filename
            call da_message(message(1:1))
            read(innov_rad_unit_in) nobs_in 
            if ( nobs_in /= nobs_tot ) then
                 call da_error(__FILE__,__LINE__, &
                              (/"Dimensions (nobs_tot) mismatch "/))
            end if
         iv%instid(i)%info%n1 = iv%instid(i)%info%plocal(m-1) + 1
         iv%instid(i)%info%n2=iv%instid(i)%info%plocal(m)
         ndomain = 0
         
      if ( amsr2 ) then
         my=3
      else
         my=2
      end if
      allocate( data2d(nobs_tot, my) )
      read(innov_rad_unit_in) data2d
      do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
!        iobs = iv%instid(i)%info%obs_global_index(n)
        iobs = n
      end do
      deallocate ( data2d )

!      read(unit=innov_rad_unit_in,fmt='(10i5)') iv%instid(i)%ichan

      allocate( data3d(nobs_tot, iv%instid(i)%nchan, 3) )
      read(innov_rad_unit_in) data3d

      do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
!         iobs = iv%instid(i)%info%obs_global_index(n)
         iobs = n
         iv%instid(i)%tb_inv(:,n) = data3d (iobs,:,1)
         iv%instid(i)%tb_error(:,n) = data3d (iobs,:,2)
         iv%instid(i)%tb_qc(:,n) = int(data3d (iobs,:,3))
      end do
      deallocate( data3d )

      close(unit=innov_rad_unit_in)
      call da_free_unit(innov_rad_unit_in)
    end if !fexist
 end if ! nobs_tot
 end do !num_fgat
end do ! end do instruments

if (trace_use) call da_trace_exit("da_read_iv_rad_ascii")

end subroutine da_read_iv_rad_for_multi_inc 

