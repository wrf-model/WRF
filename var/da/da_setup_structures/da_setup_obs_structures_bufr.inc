subroutine da_setup_obs_structures_bufr(ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (y_type),  intent(out)   :: ob          ! Observation structure.
   type (iv_type), intent(inout) :: iv          ! O-B structure.

   character(len=filename_len) :: filename
   integer                     :: n,i   

   if (trace_use) call da_trace_entry("da_setup_obs_structures_bufr")

   !--------------------------------------------------------------------------
   ! [1.0] Scan BUFR observation header and get idea of number of obs:
   !--------------------------------------------------------------------------

   if (num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%time = n

         write(filename(1:10), fmt='(a, i2.2,a)') 'ob', n,'.bufr'

         ! scan prepbufr observation file
         call da_scan_obs_bufr (iv, filename)
        iv%info(:)%plocal(n) = iv%info(:)%nlocal
        iv%info(:)%ptotal(n) = iv%info(:)%ntotal
      end do
   else
      iv%time = 1
      filename="ob.bufr"
      call da_scan_obs_bufr(iv, filename)
      do i=1,num_ob_indexes
         iv%info(i)%plocal(iv%time) = iv%info(i)%nlocal
         iv%info(i)%ptotal(iv%time) = iv%info(i)%ntotal
      end do

      ! scan main body of radar observation file
      ! if (use_radarobs) &
      !    call da_scan_bufr_radar(iv, 'radar.dat')
   end if

   !-------------------------------------------------------------------------
   ! Allocate the iv based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations (iv)

   if (num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%time = n

         write(filename(1:10), fmt='(a, i2.2)') 'ob.', n

         ! read prepbufr observation file
         call da_read_obs_bufr (iv, filename)

         ! if (use_radarobs) then
         !    ! read radar observation file
         !    write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
         !    call da_read_bufr_radar(iv, filename)
         ! end if
      end do
   else

      iv%time = 1

      filename="ob.bufr"
      call da_read_obs_bufr(iv, filename)

      ! if (use_radarobs) then
      !    ! read radar observation file
      !    call da_read_bufr_radar(iv)
      ! end if
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures(iv, ob)

   iv%time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_bufr")

end subroutine da_setup_obs_structures_bufr


