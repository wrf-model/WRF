subroutine da_print_qcstat(it, iv, num_qcstat_conv)                       

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv      ! innovation vector.
   integer,           intent(in) :: it
   integer,        intent(inout) ::  num_qcstat_conv(:,:,:,:)

   integer                       :: ii, i, j, k ,n, num, ounit, ios
   character(len=filename_len)   :: file


   logical   :: write_head
   character(1),dimension(115):: cline
   character(4),dimension(2):: typx
   integer,  allocatable :: count(:)
!
   if (trace_use) call da_trace_entry("da_print_qcstat")
   num = num_ob_indexes*num_ob_vars*(npres_print)
   allocate (count(2*num))

  do k=1,115
     cline(k) = '-'
  end do
  typx(1)='used'
  typx(2)='rej '
    count  = 0
    ii = 0    
        do k = 1, npres_print   
        do j = 1, num_ob_vars
        do i = 1, num_ob_indexes
          ii = ii + 1
           count(ii)     = num_qcstat_conv(1,i,j,k)  
           count(num+ii) = num_qcstat_conv(2,i,j,k)  
        end do
        end do
        end do

    call da_proc_sum_ints(count)

  if (rootproc) then
      call da_get_unit(ounit)
      write(unit=file,fmt ='(a,i2.2)')'qcstat_conv_',it
      open(unit=ounit,file=trim(file),form='formatted', status='replace', iostat=ios)
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
         (/"Cannot open file "//file/))
    num_qcstat_conv = 0
    ii = 0    
        do k = 1, npres_print    
        do j = 1, num_ob_vars
        do i = 1, num_ob_indexes
          ii = ii + 1
          num_qcstat_conv(1,i,j,k) = count(ii)  
          num_qcstat_conv(2,i,j,k) = count(num+ii)  
        end do
        end do
        end do

        do j = 1, num_ob_vars
        do i = 1, num_ob_indexes
         num_qcstat_conv(1,i,j,npres_print+1) = sum( num_qcstat_conv(1,i,j,1:npres_print) )  
         num_qcstat_conv(2,i,j,npres_print+1) = sum( num_qcstat_conv(2,i,j,1:npres_print) )  
        end do
        end do
      write_head = .false.
   do i = 1, num_ob_indexes
     if (.not. write_head) then
51   format(110a1)
     write(ounit,50)it
50   format(20x,'WRF-Var data utilization statistics for outer iteration ',i3,/)
     write(ounit,510)'ptop',(pptop(k),k=1,npres_print), 0.0
     write(ounit,511)'obs type','var','pbot',(ppbot(k),k=1,npres_print), 2000.0
510  format(15x,a8,1x,13(1x,f6.1))
511  format(1x,a8,1x,a3,6x,a4,1x,13(1x,f6.1))
     write(ounit,500) (cline(j),j=1,115)
500  format(115a1)
     write_head = .true.
     end if
    do j = 1, num_ob_vars
     if( num_qcstat_conv(1,i,j,npres_print+1) > 0 )  then            
      write(ounit,700) obs_names(i),ob_vars(j),typx(1),&
      ((num_qcstat_conv(1,i,j,k) - num_qcstat_conv(2,i,j,k)),k=1,npres_print+1)
      write(ounit,701) typx(2), (num_qcstat_conv(2,i,j,k),k=1,npres_print+1)
700 format(1x,a10,a3,2x,a4,3x, 25(1x,i6) )
701 format(16x,a4,3x,25(1x,i6) )
     end if
    end do
   end do
    write(ounit,500) (cline(j),j=1,115)
  close (ounit) 
  call da_free_unit(ounit)
  end if

   deallocate (count)
   if (trace_use) call da_trace_exit("da_print_qcstat")

end subroutine da_print_qcstat
