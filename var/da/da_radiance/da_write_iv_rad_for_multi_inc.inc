subroutine da_write_iv_rad_for_multi_inc (it,ob, iv )

   !---------------------------------------------------------------------------
   ! Purpose: write out innovation vector structure for radiance data.
   !---------------------------------------------------------------------------

   implicit none

   integer      ,     intent(in)  :: it       ! outer loop count
   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.

   integer            :: n        ! Loop counter.
   integer            :: i, k, l, m ! Index dimension.
   integer            :: my,iobs
   integer            :: ios, innov_rad_unit
   character(len=filename_len)  :: filename
   integer            :: ndomain_local,ndomain_sum
   logical            :: amsr2
   real, allocatable  :: data2d(:,:)
   real, allocatable  :: data3d(:,:,:)
   real, allocatable  :: data2d_g(:,:)
   real, allocatable  :: data3d_g(:,:,:)

   integer, allocatable :: ndomain_global(:)

   if (trace_use) call da_trace_entry("da_write_iv_rad_for_multi_inc")

   write(unit=message(1),fmt='(A)') 'Writing radiance OMB binary files for multi_inc'
   call da_message(message(1:1))

   do i = 1, iv%num_inst

      amsr2 = index(iv%instid(i)%rttovid_string,'amsr2') > 0
      if ( amsr2 ) then ! write out clw
          my=3
      else
          my=2
      end if

      do m=num_fgat_time,1,-1

         iv%instid(i)%info%n1 = iv%instid(i)%info%plocal(m-1) + 1
         iv%instid(i)%info%n2 = iv%instid(i)%info%plocal(m)
         ndomain_local = 0
         do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
            if (iv%instid(i)%info%proc_domain(1,n)) then
               ndomain_local = ndomain_local + 1
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
            write(unit=message(1),fmt='(A)') 'Begin to write binary radiance omb file for MRI-4DVar'
            call da_message(message(1:1))

            if (rootproc) then
               call da_get_unit(innov_rad_unit) ! one file per time slot
               write(unit=filename, fmt='(a,i3.3)') 'inv_'//trim(iv%instid(i)%rttovid_string)//'_',m

               open(unit=innov_rad_unit,file=trim(filename),form='unformatted',status='replace',iostat=ios)
               if (ios /= 0 ) then
                  call da_error(__FILE__,__LINE__, &
                  (/"Cannot open innovation radiance file"//filename/))
               endif
               write(innov_rad_unit) ndomain_sum
            end if ! root open ounit

            allocate( data2d(ndomain_sum, my) )
            data2d = 0.0

            if (myproc == 0) then ! global index of obs at each processor
               iobs = 0
            else
               iobs = sum (ndomain_global (0:myproc-1))
            end if

            do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
               if (iv%instid(i)%info%proc_domain(1,n)) then
                  iobs = iobs+1 ! global index
                ! fill in global array with each processor's local array
                ! note: data2d exists in each processor, initialized with zero
                ! so only current processor is filled with non-zero values.
                  if ( amsr2 ) then ! write out clw
                     data2d(iobs, 1) = iv%instid(i)%info%lat(1,n)
                     data2d(iobs, 2) = iv%instid(i)%info%lon(1,n)
                     data2d(iobs, 3) = iv%instid(i)%clw(n)
                  else ! no clw info
                     data2d(iobs, 1) = iv%instid(i)%info%lat(1,n)
                     data2d(iobs, 2) = iv%instid(i)%info%lon(1,n)
                  end if
               end if
            end do !n1,n2

            write(unit=message(1),fmt='(A)') 'begin to write data2d'
            call da_message(message(1:1))

            allocate( data2d_g(ndomain_sum, my) )
#ifdef DM_PARALLEL
          ! sum of data2d from each processor into rootprocessor's data2d_g (other processors' data2d has zeros)
            call mpi_reduce(data2d, data2d_g, ndomain_sum*my, true_mpi_real, mpi_sum, root, comm, ierr)
#else
            data2d_g = data2d
#endif
            deallocate( data2d )

            if (rootproc) then
               write(innov_rad_unit) data2d_g
            end if
            deallocate( data2d_g )

            if (myproc == 0) then
               iobs = 0
            else
               iobs = sum (ndomain_global (0:myproc-1))
            end if
 
            allocate( data3d(ndomain_sum, iv%instid(i)%nchan, 3) )
            data3d = 0.0
            do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
               if (iv%instid(i)%info%proc_domain(1,n)) then
                  iobs = iobs + 1
                  data3d(iobs,:, 1)=iv%instid(i)%tb_inv(:,n)
                  data3d(iobs,:, 2)=iv%instid(i)%tb_error(:,n)
                  data3d(iobs,:, 3)=iv%instid(i)%tb_qc(:,n) * 1.0
               end if
            end do
            allocate( data3d_g(ndomain_sum, iv%instid(i)%nchan, 3) )
#ifdef DM_PARALLEL
            call mpi_reduce(data3d, data3d_g, ndomain_sum*iv%instid(i)%nchan*3, true_mpi_real, mpi_sum, root, comm, ierr)
#else
            data3d_g = data3d
#endif
            deallocate( data3d )

            write(unit=message(1),fmt='(A)') 'begin to write data3d_g'
            call da_message(message(1:1))

            if (rootproc) then
               write(innov_rad_unit) data3d_g
            end if
            deallocate( data3d_g )

            if (rootproc) then
               close(unit=innov_rad_unit)
               call da_free_unit(innov_rad_unit) !jban
            end if
         end if ! ndomain_sum > 0
         deallocate (ndomain_global)
      end do !num_fgat
   end do ! end do instruments

if (trace_use) call da_trace_exit("da_write_iv_rad_for_multi_inc")

end subroutine da_write_iv_rad_for_multi_inc 

