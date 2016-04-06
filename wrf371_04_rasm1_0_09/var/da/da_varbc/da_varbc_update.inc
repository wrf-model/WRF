  subroutine da_varbc_update (it, cv_size, cv, iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: Update VarBC parameters and write into file
   !
   ! [1] Update of VarBC parameters (including change of variable)
   !
   ! [2] Write VARBC.out file with VarBC information
   !
   !  Called from da_solve
   !
   !  HISTORY: 10/29/2007 - Creation                          Tom Auligne
   !           01/20/2010 - Update for multiple outer-loops   Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: it          !outer loop counting
   integer,        intent(in)    :: cv_size
   real,           intent(in)    :: cv(cv_size) ! Control variable structure.
   type (iv_type), intent(inout) :: iv          ! Obs. increment structure.

   !
   !  local arguments
   !------------------- 
   integer              :: inst, ichan, npred, i, npredmax, id
   integer              :: iunit, iost
   character(len=filename_len) :: filename
   character(len=120)   :: fparam
   real, allocatable    :: varbc_param_tl(:)
   logical, allocatable :: lvarbc_inst(:)

   if (trace_use) call da_trace_entry("da_varbc_update")

   write(unit=stdout,fmt='(A)') 'VARBC: Updating bias parameters'
      
   allocate( lvarbc_inst(iv%num_inst) )
   do inst = 1, iv % num_inst   
      lvarbc_inst(inst) = ANY(iv%instid(inst)%varbc(1:iv%instid(inst)%nchan)%npred>0)	
   end do   
   
   do inst = 1, iv % num_inst   
      if (.not. lvarbc_inst(inst)) cycle             !! VarBC instruments only   
      allocate(varbc_param_tl(iv%instid(inst)%varbc_info%npredmax))
      
      do ichan = 1, iv%instid(inst)%nchan
         npred    = iv%instid(inst)%varbc(ichan)%npred
         if (npred <= 0) cycle               !! VarBC channels only	 
	 if (iv%instid(inst)%varbc(ichan)%nobs >= varbc_nobsmin) then
	    where (iv%instid(inst)%varbc(ichan)%pred_use == 0) &
	           iv%instid(inst)%varbc(ichan)%pred_use = 1
	 else
            if ( satinfo(inst)%iuse(ichan) == 1) then
	       if (count(iv%instid(inst)%varbc(ichan)%pred_use == 0) > 0) &
	          write(unit=stdout,fmt='(A,A,I5)') &	   
	          'VARBC: Not enough data to keep statistics for ', &
	          trim(iv%instid(inst)%rttovid_string),             &
		  iv%instid(inst)%ichan(ichan)
	    end if
	 end if
	 
 	 if (.not. use_varbc) cycle
         
	!---------------------------------------------------------------
        ! Change of variable (preconditioning) for parameters increments
        !---------------------------------------------------------------
            
	 do i = 1, npred
            id = iv%instid(inst)%varbc(ichan)%index(i)
    	    varbc_param_tl(i) = &
	       SUM(cv(id) * iv%instid(inst)%varbc(ichan)%vtox(i,1:npred))
         end do	
	    
        !---------------------------------------------------------------
        ! Update VarBC parameters
        !---------------------------------------------------------------
	 iv%instid(inst)%varbc(ichan)%param = &
	 iv%instid(inst)%varbc(ichan)%param + varbc_param_tl(1:npred)
      end do
      
      deallocate(varbc_param_tl)
   end do
   
   if (.not. rootproc) then
      if (trace_use) call da_trace_exit("da_varbc_update")
      return
   end if

   !---------------------------------------------------------------
   ! Write VARBC.out file
   !---------------------------------------------------------------
   
   write(unit=stdout,fmt='(A)') 'VARBC: Writing information in VARBC.out file'

     
   call da_get_unit(iunit)
   if ( it == max_ext_its ) then
      filename = 'VARBC.out'
   else
      write(unit=filename, fmt='(a,i2.2)') 'VARBC.out_',it
   end if
   open(unit=iunit,file=filename,form='formatted',iostat = iost,status='replace')
	
   if (iost /= 0) then
      message(1)="Cannot open bias correction file "//adjustl(filename)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   write(iunit, *) ' VARBC version 1.0 - Number of instruments: '
   write(iunit, *) COUNT(lvarbc_inst)
   
   do inst = 1, iv % num_inst   
      if (.not. lvarbc_inst(inst)) cycle             !! VarBC instruments only   
      npredmax = iv%instid(inst)%varbc_info%npredmax

      write(iunit, *) '------------------------------------------------'
      write(iunit, *) 'Platform_id  Sat_id  Sensor_id  Nchanl  Npredmax'
      write(iunit, *) '------------------------------------------------'
      write(iunit, *) iv%instid(inst)%varbc_info%platform_id, &
                      iv%instid(inst)%varbc_info%satellite_id, &
		      iv%instid(inst)%varbc_info%sensor_id, &
      		      iv%instid(inst)%varbc_info%nchanl,&
		      iv%instid(inst)%varbc_info%npredmax

      write(iunit, *) ' -----> Bias predictor statistics:  Mean & Std & Nbgerr'
      write(iunit,'(30F10.1)') iv%instid(inst)%varbc_info%pred_mean(1:npredmax)
      write(iunit,'(30F10.1)') iv%instid(inst)%varbc_info%pred_std (1:npredmax)
      write(iunit,'(30I10)')   iv%instid(inst)%varbc_info%nbgerr   (1:npredmax)
            
      write(iunit, *) ' -----> Chanl_id Chanl_nb  Pred_use(-1/0/1)  Param'
      
      do ichan = 1, iv%instid(inst)%nchan
         npred    = iv%instid(inst)%varbc(ichan)%npred
         if (npred <= 0) cycle               !! VarBC channels only	 
	 write(fparam,*) '(I4,I6,',npredmax,'I3,',npred,'F8.3)'
         write(iunit, fmt=trim(adjustl(fparam))) &
	     ichan, iv%instid(inst)%varbc(ichan)%ichanl,   &
	            iv%instid(inst)%varbc(ichan)%pred_use, &
  	            iv%instid(inst)%varbc(ichan)%param
      end do
   end do

   deallocate(lvarbc_inst)
   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_varbc_update")


 end subroutine da_varbc_update
