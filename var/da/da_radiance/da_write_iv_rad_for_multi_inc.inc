subroutine da_write_iv_rad_for_multi_inc (it,ob, iv )

   !---------------------------------------------------------------------------
   ! Purpose: write out innovation vector structure for radiance data.
   !---------------------------------------------------------------------------

   implicit none

   integer      ,     intent(in)  :: it       ! outer loop count
   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, k, l, m, m1, m2,nobs_tot  ! Index dimension.
   integer                        :: nlevelss ! Number of obs levels.
   integer                        :: my,iobs
   integer            :: ios, innov_rad_unit
   character(len=filename_len)  :: filename
   character(len=7)   :: surftype
   integer            :: ndomain
   logical            :: amsr2
   real, allocatable  :: data2d(:,:)
   real, allocatable  :: data3d(:,:,:)
   real, allocatable  :: data2d_g(:,:)
   real, allocatable  :: data3d_g(:,:,:)

   if (trace_use) call da_trace_entry("da_write_iv_rad_for_multi_inc")

   write(unit=message(1),fmt='(A)') 'Writing radiance OMB ascii file for multi_inc'
   call da_message(message(1:1))

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

      ! count number of obs within the loc%proc_domain
      ! ---------------------------------------------
      nobs_tot = iv%instid(i)%info%ptotal(num_fgat_time) - iv%instid(i)%info%ptotal(0)
!      write(unit=message(1),fmt='(A)') 'calculate nobs_tot'
      do m=num_fgat_time,1,-1
      if ( nobs_tot > 0 ) then
         write(unit=message(1),fmt='(A)') 'begin to write'
         if ( rootproc ) then
            write(unit=filename, fmt='(a,i3.3)') 'inv_'//trim(iv%instid(i)%rttovid_string)//'_',m
            open(unit=innov_rad_unit,file=trim(filename),form='unformatted',status='replace',iostat=ios)
            if (ios /= 0 ) then
               call da_error(__FILE__,__LINE__, &
               (/"Cannot open innovation radiance file"//filename/))
            Endif
            write(unit=message(1),fmt='(A)') filename
            call da_message(message(1:1)) 
            write(innov_rad_unit) nobs_tot 
         end if ! root open ounit
   
         iv%instid(i)%info%n1 = iv%instid(i)%info%plocal(m-1) + 1
         iv%instid(i)%info%n2=iv%instid(i)%info%plocal(m)
         ndomain = 0
         
         do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
 
           if (iv%instid(i)%info%proc_domain(1,n)) then
             ndomain = ndomain + 1
           end if
         end do
         if (ndomain < 1) cycle
  
      if ( amsr2 ) then ! write out clw
          my=3
      else
          my=2
      end if
      allocate( data2d(nobs_tot, my) )
      data2d = 0.0
      do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
         if (iv%instid(i)%info%proc_domain(1,n)) then
!            iobs = iv%instid(i)%info%obs_global_index(n)
            iobs = n
            write(unit=message(1),fmt='(I)') iobs
            call da_message(message(1:1)) 
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

     allocate( data2d_g(nobs_tot, my) )
#ifdef DM_PARALLEL
     call mpi_reduce(data2d, data2d_g, nobs_tot*my, true_mpi_real, mpi_sum, root, comm, ierr)
#else
     data2d_g = data2d
#endif
    deallocate( data2d )

    if ( rootproc ) then
         write(innov_rad_unit) data2d_g
    end if
    deallocate( data2d_g )
 
    allocate( data3d(nobs_tot, iv%instid(i)%nchan, 3) )
    data3d = 0.0
    do n = iv%instid(i)%info%n1,iv%instid(i)%info%n2
      if (iv%instid(i)%info%proc_domain(1,n)) then
      !   iobs = iv%instid(i)%info%obs_global_index(n)
         iobs = n 
         data3d(iobs,:, 1)=iv%instid(i)%tb_inv(:,n)
         data3d(iobs,:, 2)=iv%instid(i)%tb_error(:,n)
         data3d(iobs,:, 3)=iv%instid(i)%tb_qc(:,n) * 1.0
      end if
    end do 
    allocate( data3d_g(nobs_tot, iv%instid(i)%nchan, 3) )
#ifdef DM_PARALLEL
    call mpi_reduce(data3d, data3d_g, nobs_tot*iv%instid(i)%nchan*3, true_mpi_real, mpi_sum, root, comm, ierr)
#else
    data3d_g = data3d
#endif
    deallocate( data3d )
    if ( rootproc ) then
         write(innov_rad_unit) data3d_g
    end if
    deallocate( data3d_g )
   
    if ( rootproc ) then
      close(unit=innov_rad_unit)
    end if
    call da_free_unit(innov_rad_unit)
  end if ! nobs_tot > 0 
 end do !num_fgat
end do ! end do instruments

if (trace_use) call da_trace_exit("da_write_iv_rad_for_multi_inc")

end subroutine da_write_iv_rad_for_multi_inc 

