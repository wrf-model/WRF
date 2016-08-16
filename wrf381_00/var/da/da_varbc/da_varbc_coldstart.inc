  subroutine da_varbc_coldstart (iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: [1]: If a cold-start is needed, calculate mode of histogram 
   !                of (uncorrected) innovations for each channel
   !
   !           [2]: Calculate statistics (mean/std) for cold-started predictors
   !
   !           [3]: Normalize predictors
   !
   !  Called from da_get_innov_vector_radiance
   !
   !  HISTORY: 10/26/2007 - Creation                     Tom Auligne
   !           11/03/2008 - Bug-fix: exclude HALO data for
   !                        computation of predictor statistics    Tom/Hui-Chuan  
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent (inout)   :: iv             ! Innovation

   integer                          :: inst, k, n, i
   integer                          :: npredmax, npred, num_rad, num_rad_domain, num_rad_tot

   integer, parameter               :: nbins   = 200                       ! Number of Hist bins.
   real,    parameter               :: maxhist = 10.                       ! Maximum bin value.
   real,    parameter               :: zbin    = 2 * maxhist / real(nbins) ! Hist bin width.
   integer                          :: ibin                                ! Hist bin number.
   real                             :: modetmp(1), mode
   integer, allocatable             :: hist(:), hist_tot(:)
   
   real, allocatable                :: mean(:), rms(:), mean_tot(:), rms_tot(:)
   integer, allocatable             :: ipred(:)
   logical                          :: global_cs
   
   if ( iv%num_inst < 1 ) RETURN

   if (trace_use) call da_trace_entry("da_varbc_coldstart")   

   do inst = 1, iv%num_inst                                            !! loop for sensors

      npredmax       = iv%instid(inst)%varbc_info%npredmax
      num_rad        = iv%instid(inst)%num_rad
      if (num_rad > 0) then
         num_rad_domain = COUNT(iv%instid(inst)%info%proc_domain(1,1:num_rad)) !do not count HALO
      else
         num_rad_domain = 0
      end if
      num_rad_tot    = wrf_dm_sum_integer(num_rad_domain)
      
      if (npredmax <= 0) cycle                                         !! VarBC instr only
      
      allocate( ipred(npredmax) )
    
    !---------------------------------------------------------------------------
    ! [1]: Calculate mode of histogram of (uncorrected) innovations 
    !---------------------------------------------------------------------------
      do k = 1, iv%instid(inst)%nchan                                  !! loop for channels
         npred          = iv%instid(inst)%varbc(k)%npred
	 ipred(1:npred) = iv%instid(inst)%varbc(k)%ipred(1:npred)
	 if (npred <= 0) cycle                                         !! VarBC channels only
	 if (ALL(iv%instid(inst)%varbc(k)%pred_use /= 0)) cycle        !! Coldstart channels only
	 
	 where (iv%instid(inst)%varbc(k)%pred_use(ipred(1:npred)) == 0) &
	        iv%instid(inst)%varbc(k)%param(1:npred) = 0.0

         if (iv%instid(inst)%varbc(k)%pred_use(1) == 0) then
	    Allocate ( hist(nbins),  hist_tot(nbins))
            hist(:) = 0  
            mode    = 0.0
	 
           ! Accumulate statistics for histogram
           ! -----------------------------------
            do n = 1, num_rad      !! loop for pixel      
               if (iv%instid(inst)%info%proc_domain(1,n)) then ! do not count HALO
                  ibin = NINT( (iv%instid(inst)%tb_inv(k,n)+maxhist)/zbin )
 	          if ((ibin>0).AND.(ibin<=nbins)) &
	             hist(ibin) = hist(ibin) + 1
               end if          
            end do             ! end loop for pixels
	       
           ! Do inter-processor communication to gather statistics
           ! ------------------------------------------------------
	    do ibin = 1, nbins
	       hist_tot(ibin) = wrf_dm_sum_integer(hist(ibin))
	    end do
		   
           ! Determine mode of Histogram
           !----------------------------
            if ( SUM(hist_tot(:)) > 0 ) then
	      modetmp(1:1) = MAXLOC(hist_tot(:))*zbin - maxhist
              mode = modetmp(1)
	    end if
	 
	   ! Use mode to initialize VarBC 
	   !-----------------------------
	    if (iv%instid(inst)%varbc(k)%ipred(1) == 1) &
	        iv%instid(inst)%varbc(k)%param(1) = mode

	    Deallocate ( hist, hist_tot )
            if ( satinfo(inst)%iuse(k) == 1 ) &
               write(unit=stdout,fmt='(A,A,I5,A,F5.2)') 'VARBC: Cold-starting ', &
                  trim(adjustl(iv%instid(inst)%rttovid_string)),iv%instid(inst)%ichan(k),&
	          ' --> ',mode			       	
	 end if
      end do                                                              !  end loop for channels

    !---------------------------------------------------------------------------
    !  [2]: Calculate statistics for cold-started predictors 
    !---------------------------------------------------------------------------
      global_cs = .true.
      do k = 1, iv%instid(inst)%nchan 
	 if (iv%instid(inst)%varbc(k)%npred <= 0) cycle                   !! VarBC channels only      
	 if (ANY(iv%instid(inst)%varbc(k)%pred_use > 0)) global_cs = .false.      
      end do	 
    
      if (global_cs) then                                                 !! Instrument coldstart only

         allocate (mean(npredmax), rms(npredmax), mean_tot(npredmax), rms_tot(npredmax))

        ! Accumulate statistics for predictor mean/std
        ! ---------------------------------------------
	 if (num_rad > 0) then
            do i = 1, npredmax
               mean(i) = SUM( iv%instid(inst)%varbc_info%pred(i,1:num_rad),    &
                         MASK=iv%instid(inst)%info%proc_domain(1,1:num_rad))   ! do not count HALO  
               rms(i)  = SUM( iv%instid(inst)%varbc_info%pred(i,1:num_rad)**2, &
                         MASK=iv%instid(inst)%info%proc_domain(1,1:num_rad))   ! do not count HALO 
            end do
	 else
	    mean = 0.0
	    rms  = 0.0
	 end if
  
        ! Do inter-processor communication to gather statistics
        ! ------------------------------------------------------
	 call wrf_dm_sum_reals(mean, mean_tot)
  	 call wrf_dm_sum_reals(rms,  rms_tot)	 
         if (num_rad_tot >= varbc_nobsmin) then
	    mean_tot = mean_tot / num_rad_tot
            rms_tot  = rms_tot  / num_rad_tot
	 else
	    mean_tot = 0.0
	    rms_tot  = 1.0   
	 end if
	 
        ! Store statistics
        !------------------
	 iv%instid(inst)%varbc_info%pred_mean = mean_tot
         iv%instid(inst)%varbc_info%pred_std  = sqrt(rms_tot - mean_tot**2)
      
         deallocate(mean, rms, mean_tot, rms_tot)

      end if
      deallocate(ipred)  	           	     	

    !---------------------------------------------------------------------------
    !  [3]: Normalize predictors
    !---------------------------------------------------------------------------
      do i = 1,  npredmax
         if ( iv%instid(inst)%varbc_info%pred_std(i) <= 0.0 ) cycle
         do n = 1, num_rad      
            iv%instid(inst)%varbc_info%pred(i,n) = &
          ( iv%instid(inst)%varbc_info%pred(i,n) - &
            iv%instid(inst)%varbc_info%pred_mean(i) ) / &
            iv%instid(inst)%varbc_info%pred_std(i)
	 end do     
      end do
      
   end do                           !  end loop for sensor
   
   if (trace_use) call da_trace_exit("da_varbc_coldstart")

 end subroutine da_varbc_coldstart
