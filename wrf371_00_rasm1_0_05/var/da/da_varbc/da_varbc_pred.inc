subroutine da_varbc_pred(iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate bias predictors
   !
   ! pred(1) - 1 (Constant)
   ! pred(2) - 1000-300 thickness 43 1005.43-521.46 thickness
   ! pred(3) - 200-50 thickness   43  194.36-56.73  thickness
   ! pred(4) - T_skin
   ! pred(5) - total column precipitable water
   ! pred(6) - satellite scan position
   ! pred(7) - satellite scan position **2
   ! pred(8) - satellite scan position **3
   ! pred(9) - CRTM Gamma Correction
   !
   !  Called from da_varbc_coldstart
   !
   !  HISTORY: 10/26/2007 - Creation                  Tom Auligne
   !---------------------------------------------------------------------------
   
  implicit none

  integer,parameter   :: npred_hk = 4       ! Number of H&K bias predictors

  type (iv_type), intent(inout) :: iv       ! O-B structure.

  integer   :: inst, nlevels, i, npredmax, n, num_rad
  real      :: pred_hk(npred_hk)

  if ( iv%num_inst < 1 ) RETURN

  if (trace_use) call da_trace_entry("da_varbc_pred")

  do inst = 1, iv%num_inst                                  ! loop for sensor
    npredmax = iv%instid(inst)%varbc_info%npredmax
    if (npredmax <= 0) cycle                                ! VarBC instr only
       
    num_rad = iv%instid(inst)%num_rad
    if (iv%instid(inst)%info%n2 < iv%instid(inst)%info%n1) cycle
    do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2  ! loop for pixel
      ! get H&K predictors
      !--------------------
      if (rtm_option==rtm_option_rttov) then
#ifdef RTTOV
        nlevels=iv%instid(inst)%nlevels
        call da_predictor_rttov(inst, &
	     pred_hk(1:npred_hk), npred_hk, nlevels, &
  	     iv%instid(inst)%t(1:nlevels,n), &
             iv%instid(inst)%mr(1:nlevels,n)/q2ppmv, &
	     iv%instid(inst)%ts(n))
#endif
      else if (rtm_option==rtm_option_crtm) then
#ifdef CRTM
        nlevels=iv%instid(inst)%nlevels-1
        call da_predictor_crtm( &
	     pred_hk(1:npred_hk), npred_hk, nlevels,&
             iv%instid(inst)%tm(1:nlevels,n), &
             iv%instid(inst)%qm(1:nlevels,n), &
	     iv%instid(inst)%ts(n), &
	     iv%instid(inst)%pf(0:nlevels,n))
#endif
      end if     

      ! Populate predictors
      !---------------------  
      iv%instid(inst)%varbc_info%pred(1:npredmax,n) = 0.0
  
      ! Constant predictor
      iv%instid(inst)%varbc_info%pred(1,n) = 1.0 
  
      ! H&K predictors
      if (npredmax >= 2) iv%instid(inst)%varbc_info%pred(2,n) = pred_hk(1)
      if (npredmax >= 3) iv%instid(inst)%varbc_info%pred(3,n) = pred_hk(2)
      if (npredmax >= 4) iv%instid(inst)%varbc_info%pred(4,n) = pred_hk(3)
      if (npredmax >= 5) iv%instid(inst)%varbc_info%pred(5,n) = pred_hk(4)
		
      ! Scan predictors	
      if (npredmax >= 6) iv%instid(inst)%varbc_info%pred(6,n) = iv%instid(inst)%scanpos(n)
      if (npredmax >= 7) iv%instid(inst)%varbc_info%pred(7,n) = iv%instid(inst)%scanpos(n)**2
      if (npredmax >= 8) iv%instid(inst)%varbc_info%pred(8,n) = iv%instid(inst)%scanpos(n)**3

    end do                       ! pixel loop   
  end do                         ! sensor loop
   
   if (trace_use) call da_trace_exit("da_varbc_pred")

end subroutine da_varbc_pred
