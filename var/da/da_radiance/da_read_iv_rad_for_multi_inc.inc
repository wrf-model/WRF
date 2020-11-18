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
   integer, allocatable :: counts(:), displs(:)
   integer            :: nk,ndomain_local,num,ndomain_sum,proc
   integer, allocatable :: ndomain_global(:)

   real, allocatable :: lat(:),lon(:)

   if (trace_use) call da_trace_entry("da_read_iv_rad_ascii")

   write(unit=message(1),fmt='(A)') 'Reading radiance OMB for multi_inc'
   call da_message(message(1:1))

!no thinning for coarse res.(setup in namelist), keep all the obs:
   do i = 1, iv%num_inst
      amsr2 = index(iv%instid(i)%rttovid_string,'amsr2') > 0  !jban  2020-08-22
      !print*, "amsr2=",amsr2

      nobs_tot = iv%info(radiance)%ptotal(num_fgat_time) - iv%info(radiance)%ptotal(0)
      !print*, "nobs_tot=",nobs_tot
      !print*, "iv%instid(i)%num_rad=",iv%instid(i)%num_rad
      !print*, "iv%instid(i)%info%plocal=", iv%instid(i)%info%plocal(0:num_fgat_time)
      !print*, "iv%instid(i)%info%ptotal=", iv%instid(i)%info%ptotal(0:num_fgat_time)
      !print*, "iv%info(radiance)%plocal=", iv%info(radiance)%plocal(0:num_fgat_time)
      !print*, "iv%info(radiance)%ptotal=", iv%info(radiance)%ptotal(0:num_fgat_time)

      do m=num_fgat_time,1,-1

         iv%instid(i)%info%n1 = iv%instid(i)%info%plocal(m-1) + 1
         iv%instid(i)%info%n2=iv%instid(i)%info%plocal(m)
         ndomain_local = 0
!print*, "before read: timeslot,n1,n2=",m,iv%instid(i)%info%n1,iv%instid(i)%info%n2
         do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
            if (iv%instid(i)%info%proc_domain(1,n)) then
               ndomain_local = ndomain_local + 1
               iv%instid(i)%tb_qc(:,n) = -1 
!write(unit=stdout,fmt='(a,3i8,3(2x,f10.5))') 'bcheckforplot ',m,iv%instid(i)%tb_qc(6,n),n,iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n),iv%instid(i)%tb_inv(6,n)
            end if
         end do

         allocate (ndomain_global(0:num_procs-1))
         call mpi_allgather( ndomain_local, 1, mpi_integer, &
                          ndomain_global,   1, mpi_integer, comm, ierr )
         ndomain_sum = sum(ndomain_global)
!print*,"ndomain_global=",ndomain_global

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
               !print*, "nobs_in=",nobs_in
               !print*, "ndomain_sum=",ndomain_sum
               !always does not match, so comment it out
               !if ( nobs_in /= ndomain_sum ) then
               !     call da_error(__FILE__,__LINE__, &
               !                  (/"Dimensions (nobs_tot) mismatch "/))
               !end if

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
               !print*,'iv%instid(i)%nchan=',iv%instid(i)%nchan
            do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
               if (iv%instid(i)%info%proc_domain(1,n)) then
                  do iobs = 1, nobs_in      
                     if (iv%instid(i)%info%lat(1,n)==data2d(iobs, 1) .and. iv%instid(i)%info%lon(1,n)==data2d(iobs, 2)) then
!write(unit=stdout,fmt='(a,4i8,4(2x,f10.5))') 'acheckforplot ',m,iv%instid(i)%tb_qc(6,n),n,iobs,iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n),iv%instid(i)%tb_inv(6,n),data3d (iobs,6,1)
                     iv%instid(i)%tb_inv(:,n) = data3d (iobs,:,1)
                     iv%instid(i)%tb_error(:,n) = data3d (iobs,:,2)
                     iv%instid(i)%tb_qc(:,n) = int(data3d (iobs,:,3))
                     !print*, "matchiobs=",m,iobs
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

if (trace_use) call da_trace_exit("da_read_iv_rad_ascii")

end subroutine da_read_iv_rad_for_multi_inc 

