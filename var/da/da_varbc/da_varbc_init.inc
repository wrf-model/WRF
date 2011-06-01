  subroutine da_varbc_init (iv, be)

   !---------------------------------------------------------------------------
   !  PURPOSE: Initialize Variational bias correction from VARBC.in file
   !
   !  Called from da_radiance_init routine
   !
   !  HISTORY: 10/26/2007 - Creation                     Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)  :: iv       ! O-B structure.
 
   type (be_type), intent(inout) :: be        ! background error.

   !
   !  local arguments
   !------------------- 
   integer   :: n, i, ii, j, k, nchan, npred, npred_ws, ipred, ichan, np, nsensor
   integer   :: iunit, iost
   integer   :: size_jp, jp_start
   integer   :: platform_id, satellite_id, sensor_id, nchanl, npredmax, num_rad
   integer, allocatable :: pred_use(:), ipred_ws(:)
   real, allocatable    :: pred_ws(:)
   character(len=filename_len) :: filename
   character(len=120) :: cline
   logical   :: lvarbc_read, limatch, lmatch
   
   if (trace_use) call da_trace_entry("da_varbc_init")

   !--------------------------------------------------------------
   ! [1] Initializations
   !--------------------------------------------------------------
   size_jp  = 0                            !! Jp Control Variable size
   jp_start = be % cv % size_jb + be % cv % size_je
 
   do n = 1, iv % num_inst
      nchan = iv%instid(n)%nchan
      iv%instid(n)%varbc_info%npredmax = 0
      allocate ( iv%instid(n)%varbc(nchan) )
      do k = 1, nchan
         iv%instid(n)%varbc(k)%npred = 0   ! by default, do not use VarBC
         iv%instid(n)%varbc(k)%nobs  = 0 
      end do
   end do   

   !--------------------------------------------------------------
   ! [2] Read VarBC.in file and match with obs data set 
   !--------------------------------------------------------------
   filename = 'VARBC.in'
   inquire(file=trim(adjustl(filename)), exist=lvarbc_read)

   if (lvarbc_read) then
    write(unit=stdout,fmt='(A)') 'VARBC: Reading VARBC.in file'
    call da_get_unit(iunit)
    open(unit=iunit,file=filename, form='formatted', status='old')
	
    read(iunit, *) 
    read(iunit, *) nsensor
   
    do j = 1, nsensor
      read(iunit, *) ; read(iunit, *) ;read(iunit, *)
      read(iunit, *) platform_id, satellite_id, sensor_id, nchanl, npredmax
      limatch = .false.
       do n = 1, iv % num_inst
         if ( (platform_id  == iv%instid(n)%platform_id) .and. &   !! found match !!
              (satellite_id == iv%instid(n)%satellite_id) .and. &  !! found match !!
	      (sensor_id    == iv%instid(n)%sensor_id) ) then      !! found match !!	      
	    limatch = .true.                                       !! found match !!

!            write(unit=stdout,fmt='(A,A)') &
!     	       'VARBC: Matching with obs for ', iv%instid(n)%rttovid_string
	       
            num_rad  = iv%instid(n)%num_rad    
            allocate ( iv%instid(n)%varbc_info%pred(npredmax, num_rad) )	 
	    
            iv%instid(n)%varbc_info%platform_id  = platform_id
            iv%instid(n)%varbc_info%satellite_id = satellite_id
            iv%instid(n)%varbc_info%sensor_id    = sensor_id
            iv%instid(n)%varbc_info%nchanl       = nchanl
            iv%instid(n)%varbc_info%npredmax     = npredmax
            iv%instid(n)%varbc_info%gammapred    = 9    ! CRTM Gamma Correction
            read(iunit, *)
            allocate ( iv%instid(n)%varbc_info%pred_mean(npredmax) )
            allocate ( iv%instid(n)%varbc_info%pred_std (npredmax) )
            allocate ( iv%instid(n)%varbc_info%nbgerr   (npredmax) )
            read(iunit, *) iv%instid(n)%varbc_info%pred_mean
            read(iunit, *) iv%instid(n)%varbc_info%pred_std
            read(iunit, *) iv%instid(n)%varbc_info%nbgerr
            read(iunit, *)

            allocate ( pred_use(npredmax), ipred_ws(npredmax), pred_ws(npredmax) )
            do i = 1, nchanl
	       lmatch = .false.
	       read(iunit, '(A)',iostat=iost) cline
               read(cline, *) ii, ichan, pred_use
               npred    = COUNT (pred_use >= 0)
               if (npred <= 0) cycle                !! VarBC channels only
	       
	       chan_loop: do k = 1, iv%instid(n)%nchan
	          if (iv%instid(n)%ichan(k) == ichan) then         !! found match !!		  
		     lmatch = .true.                               !! found match !!
	             iv%instid(n)%varbc(k)%ichanl = ichan
                     iv%instid(n)%varbc(k)%npred  = npred
	             allocate ( iv%instid(n)%varbc(k)%pred_use (npredmax) )
	             allocate ( iv%instid(n)%varbc(k)%ipred (npred) )
	             allocate ( iv%instid(n)%varbc(k)%index (npred) )
                     allocate ( iv%instid(n)%varbc(k)%param (npred) )
	             allocate ( iv%instid(n)%varbc(k)%bgerr (npred) )
                     allocate ( iv%instid(n)%varbc(k)%vtox    (npred,npred) )
		     iv%instid(n)%varbc(k)%pred_use = pred_use
                     iv%instid(n)%varbc(k)%vtox(1:npred, 1:npred)     = 0.0
		     do ipred = 1, npred
		        iv%instid(n)%varbc(k)%vtox(ipred, ipred)      = 1.0
                     end do
		     iv%instid(n)%varbc(k)%param(1:npred)             = 0.0
   	             iv%instid(n)%varbc(k)%bgerr(1:npred)             = 0.0
                     iv%instid(n)%varbc(k)%ipred(1:npred)             = &
                           PACK((/ (ipred, ipred=1,npredmax) /), mask = (pred_use >= 0))
		
		    ! VarBC warm-start parameters
   	      	    !-----------------------------
                     npred_ws = COUNT (pred_use > 0)
	             if (npred_ws > 0) then
		        read(cline, *) ii, ichan, pred_use, pred_ws(1:npred_ws)
 		        ipred_ws(1:npred_ws) = PACK((/ (ipred, ipred=1,npred) /), &
  			                       mask = (pred_use(iv%instid(n)%varbc(k)%ipred) >  0))
		        iv%instid(n)%varbc(k)%param(ipred_ws(1:npred_ws))= pred_ws(1:npred_ws)
                     end if
		     
                    ! Jp Control Variable size 
		    !--------------------------
                     do ipred = 1, npred
     	                 size_jp =  size_jp + 1
	                 iv%instid(n)%varbc(k)%index(ipred) = jp_start + size_jp
                     end do
		     
		     exit chan_loop
		  end if 
	       end do chan_loop
	       
	       if (.not. lmatch) write(unit=stdout,fmt='(A,I4)') &
	       			       'VARBC: no matching for channel:',ichan
            end do
            deallocate(pred_use, ipred_ws, pred_ws)   
         end if          
       end do
       if (.not. limatch) then
          read(iunit, *) ; read(iunit, *) ; read(iunit, *) ; read(iunit, *) ; read(iunit, *)
	  do i = 1, nchanl
	     read(iunit, *)
	  end do 
	  write(unit=stdout,fmt='(A,3I4)') &
	        'VARBC: no matching for platform/satid/sensor',platform_id, satellite_id, sensor_id
       end if
    end do
    close(iunit)
    call da_free_unit(iunit)
   else
      write(unit=stdout,fmt='(A)') 'VARBC: could not find VARBC.in file --> VARBC switched off'
      use_varbc    = .false.
      freeze_varbc = .false.
   end if 
   
   !--------------------------------------------------------------
   ! [3] Define VarBC control variable size:
   !--------------------------------------------------------------
   use_varbc = use_varbc.and.(.not.freeze_varbc)   
   if (use_varbc) then
      be % cv % size_jp = size_jp
      cv_size_domain_jp = size_jp
   end if
   
   if (trace_use) call da_trace_exit("da_varbc_init")

 end subroutine da_varbc_init 
