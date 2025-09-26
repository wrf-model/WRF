subroutine da_obs_sfc_correction(info, sfc_obs, n, xb)

   !--------------------------------------------------------------------
   ! Purpose: correct the surface measurements (wind, 
   ! temperature, and pressure) from the observed height to the WRF     
   ! model's lowest half-zeta level before going to the minimization.   
   !                                                                    
   !   Wind       : based on the similarity theory                      
   !   Temperature: Frank Ruggiero's (1996) method                      
   !   Pressure   : Hydrostatic correction                              
   !                                                                    
   ! The order of the vertical index is "kts=1(bottom) and kte(top)".   
   ! With cv_options=2 and sfc_assi_option=1, this procedure must be    
   ! gone through, otherwise unrealistic results may be obtained.   
   !--------------------------------------------------------------------

   implicit none

   type(infa_type),  intent(in)    :: info
   type(synop_type), intent(inout) :: sfc_obs
   integer,          intent(in)    :: n
   type(xb_type),    intent(in)    :: xb

   real    :: roughness, psfc, mslp, dx, dxm, dy, dym, ho, po, to, qo
   real    :: hm, pm, tm, qm, um, vm, correc, val

   integer :: i, j, k
   real    :: t_mdl(kts:kte)
   real    :: q_mdl(kts:kte)
   real    :: u_mdl(kts:kte)
   real    :: v_mdl(kts:kte)
   real    :: height(kts:kte)
   real    :: pressure(kts:kte)

   if (trace_use_dull) call da_trace_entry("da_obs_sfc_correction")

   ! 1. Check if it needs to do the surface correction at the first level
 
   ! 1.1 Surface reports located at far below the lowest model level
 
   ! 2. Model profile at OBS site for surface correction

   i   = info%i(1,n)
   j   = info%j(1,n)
   dx  = info%dx(1,n)
   dy  = info%dy(1,n)
   dxm = info%dxm(1,n)
   dym = info%dym(1,n)

   ! Model roughness at the obs site

   roughness = dym*(dxm*xb%rough(i,j)   + dx*xb%rough(i+1,j)) &
      + dy *(dxm*xb%rough(i,j+1) + dx*xb%rough(i+1,j+1))

   do k = kts, kte
      pressure(k) = dym*(dxm*xb%p(i,j,k) + dx*xb%p(i+1,j,k)) + dy*(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
      height(k)   = dym*(dxm*xb%h(i,j,k) + dx*xb%h(i+1,j,k)) + dy*(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
      t_mdl(k)    = dym*(dxm*xb%t(i,j,k) + dx*xb%t(i+1,j,k)) + dy*(dxm*xb%t(i,j+1,k) + dx*xb%t(i+1,j+1,k))
      q_mdl(k)    = dym*(dxm*xb%q(i,j,k) + dx*xb%q(i+1,j,k)) + dy*(dxm*xb%q(i,j+1,k) + dx*xb%q(i+1,j+1,k))
      u_mdl(k)    = dym*(dxm*xb%u(i,j,k) + dx*xb%u(i+1,j,k)) + dy*(dxm*xb%u(i,j+1,k) + dx*xb%u(i+1,j+1,k))
      v_mdl(k)    = dym*(dxm*xb%v(i,j,k) + dx*xb%v(i+1,j,k)) + dy*(dxm*xb%v(i,j+1,k) + dx*xb%v(i+1,j+1,k))
   end do 

   ! 3. OBS data and recover the surface pressure from the
   ! mean sea level pressure (mslp)

   ho   = sfc_obs % h
   po   = sfc_obs % p % inv 
   to   = sfc_obs % t % inv
   qo   = sfc_obs % q % inv

   ! 3.1 Compute the surface OBS pressure from mean sea level pressure

   if ( psfc_from_slp ) then
      mslp = info%slp(n)%inv
      if (abs(mslp - missing_r) > 1.0) then
         psfc = missing_r
         if (abs(ho - missing_r) > 1.0) then
            if (abs(to - missing_r) > 1.0) then
               call da_sfcprs (kts, kte, pressure, t_mdl, q_mdl, height, psfc, mslp, ho, to)
            else
               call da_sfcprs (kts, kte, pressure, t_mdl, q_mdl, height, psfc, mslp, ho)
            end if
         end if
         sfc_obs % p % inv = psfc
         ! YRG: to allow assmilate the Psfc from mslp:
         sfc_obs % p % qc  = 0
      end if
      po = sfc_obs % p % inv
   end if

   if (sfc_obs % p % inv < 1.0) then
      sfc_obs % p % qc  = missing_data
   end if

   po = sfc_obs % p % inv

   ! 3.2 Check that obs pressure and height are present
   !     ----------------------------------------------

   if (abs(po - missing_r) < 1.0  .OR. abs(ho - missing_r) < 1.0) then
      if (trace_use_dull) call da_trace_exit("da_obs_sfc_correction")
      return

      ! write(unit=message(1), fmt='(/3(1x,a))') &
      !    'MISSinG HEIGHT OR PRESSURE OBSERVATION ID ', &
      !    trim (sfc_obs%info % id), trim (sfc_obs%info % name)

      ! write(unit=message(2), fmt='(2(A,F12.3,/))') &
      !                         ' height   = ',ho,&
      !                         ' pressure = ',po
      ! call da_error(__FILE__,__LINE__,message(1:2))

   end if

   ! 4.  Bring surface observation below model levels with good height quality
   ! ================================================

   if (sfc_obs % h < height(kts)) then

      ! 2.3 Make use of local variables for model  
      !     -------------------------------------

      um = u_mdl(kts)
      vm = v_mdl(kts)
      tm = t_mdl(kts)
      pm = pressure (kts)
      qm = q_mdl(kts)
      hm = height(kts)

      ! 3.2 Correction wind based on similarity laws
      !     -------------------------------------------

      if ((abs(sfc_obs%u%inv - missing_r) > 1.0) .AND. (abs(sfc_obs%v%inv - missing_r) > 1.0)) then
         ! 3.2.1 Correction factor

         ! temperature and moisture at sigma level:

         if (abs(to - missing_r) < 1.0) then
            correc = da_mo_correction(ho, po, tm, qo, hm, pm, tm, qm, um ,vm, roughness)
         else
            correc = da_mo_correction(ho, po, to, qo, hm, pm, tm, qm, um ,vm, roughness)
         end if

         ! 3.2.2 Wind correction 
         !       ---------------

         !  Correct data and replace any previous wind qcs
         !  with surface correction flag

         sfc_obs % u % inv = correc * sfc_obs % u % inv 
         if ( abs(correc-1.0)>0.0 ) sfc_obs % u % qc  = surface_correction

         sfc_obs % v % inv = correc * sfc_obs % v % inv
         if ( abs(correc-1.0)>0.0 ) sfc_obs % v % qc  = surface_correction
      end if

      ! 3.4 Correct pressure
      !     ----------------

      if (sfc_obs % p % qc >= 0) then
         !  Correct data
         if (abs(to  - missing_r) > 1.0 .and. abs(qo - missing_r) > 1.0) then
            call da_intpsfc_prs (val, ho, po, hm, tm, qm, to, qo)
         else if (abs(to  - missing_r) > 1.0) then
            call da_intpsfc_prs (val, ho, po, hm, tm, qm, to)
         else
            call da_intpsfc_prs (val, ho, po, hm, tm, qm)
         end if

         !  Replace any previous pressure qc by the surface correction

         sfc_obs % p % inv = val
         sfc_obs % p % qc  = surface_correction
      end if

      ! 3.5 Correct temperature
      !     -------------------

      if (abs(sfc_obs % t % inv - missing_r) > 1.0) then
         !  Correct data
         call da_intpsfc_tem (val, ho, po, to, height, pressure, t_mdl, kts, kte)

         sfc_obs % t % inv = val

         !  Replace any previous temperature qc by the surface correction
         sfc_obs % t % qc  = surface_correction
      end if

      ! 3.6 Assign model lowest level height + 1m to observation
      !      ----------------------------------------------------- 
      ! sfc_obs % h = height(kts) + 1.0

      ! 3.7 Height QC
      !     ---------
      ! sfc_obs % height_qc = surface_correction
   end if

   if (trace_use_dull) call da_trace_exit("da_obs_sfc_correction")

end  subroutine da_obs_sfc_correction


