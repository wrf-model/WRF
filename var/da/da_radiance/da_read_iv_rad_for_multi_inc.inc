subroutine da_read_iv_rad_for_multi_inc (it, ob, iv )

   !---------------------------------------------------------------------------
   ! Purpose: read in innovation vector structure for radiance data.
   !---------------------------------------------------------------------------

   implicit none

   integer      ,     intent(in)  :: it       ! outer loop count
   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, k, l, m, my,nobs_tot,nobs_in,iobs  ! Index dimension.

   integer            :: ios, innov_rad_unit_in
   character(len=filename_len)  :: filename
   integer            :: ndomain
   logical            :: amsr2,fexist
   real, allocatable  :: data2d(:,:)
   real, allocatable  :: data3d(:,:,:)
   real, allocatable  :: data2d_g(:,:)
   real, allocatable  :: data3d_g(:,:,:)
   integer            :: ndomain_local,ndomain_sum
   integer, allocatable :: ndomain_global(:)

   real, allocatable :: lat(:),lon(:)

   if (trace_use) call da_trace_entry("da_read_iv_rad_for_multi_inc")

   write(unit=message(1),fmt='(A)') 'Reading binary radiance OMB for MRI-4DVar'
   call da_message(message(1:1))

!no thinning for coarse res.(setup in namelist), keep all the obs:
   do i = 1, iv%num_inst
      amsr2 = index(iv%instid(i)%rttovid_string,'amsr2') > 0  !jban  2020-08-22

      nobs_tot = iv%info(radiance)%ptotal(num_fgat_time) - iv%info(radiance)%ptotal(0)

      do m=num_fgat_time,1,-1

         iv%instid(i)%info%n1 = iv%instid(i)%info%plocal(m-1) + 1
         iv%instid(i)%info%n2 = iv%instid(i)%info%plocal(m)
         ndomain_local = 0
         do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
            if (iv%instid(i)%info%proc_domain(1,n)) then
               ndomain_local = ndomain_local + 1
               iv%instid(i)%tb_qc(:,n) = -1 
            end if
         end do

#ifdef DM_PARALLEL
   ! gather each tile's number of obs at one time slot into a global array
   ! and distribute combined 'ndomain_global' to all processors
         allocate (ndomain_global(0:num_procs-1))
         call mpi_allgather( ndomain_local, 1, mpi_integer, &
                          ndomain_global,   1, mpi_integer, comm, ierr )
         ndomain_sum = sum(ndomain_global) ! nobs over the entire domain in a time slot
#else
         allocate (ndomain_global(1))
         ndomain_global = ndomain_local
         ndomain_sum    = sum(ndomain_global)
#endif

         if ( ndomain_sum > 0 ) then
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
               endif
               write(unit=message(1),fmt='(A)') filename
               call da_message(message(1:1))
               read(innov_rad_unit_in) nobs_in 

               ndomain = 0
         
               if ( amsr2 ) then
                  my=3
               else
                  my=2
               end if

               allocate( data2d(nobs_in, my) )
               read(innov_rad_unit_in) data2d

               allocate( data3d(nobs_in, iv%instid(i)%nchan, 3) )
               read(innov_rad_unit_in) data3d

               do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
                  if (iv%instid(i)%info%proc_domain(1,n)) then
                     do iobs = 1, nobs_in      
                        if (iv%instid(i)%info%lat(1,n)==data2d(iobs, 1) .and. &
                                iv%instid(i)%info%lon(1,n)==data2d(iobs, 2)) then
                          iv%instid(i)%tb_inv(:,n)   = data3d (iobs,:,1)
                          iv%instid(i)%tb_error(:,n) = data3d (iobs,:,2)
                          iv%instid(i)%tb_qc(:,n)    = int(data3d (iobs,:,3))
                        end if
                     end do !if
                  end if !do
               end do

               deallocate(data2d)
               deallocate(data3d)

               call da_free_unit(innov_rad_unit_in)
            end if ! fexist
         end if ! ndomain_sum
         deallocate(ndomain_global)
      end do !num_fgat
   end do ! end do instruments

if (trace_use) call da_trace_exit("da_read_iv_rad_for_multi_inc")

end subroutine da_read_iv_rad_for_multi_inc 

