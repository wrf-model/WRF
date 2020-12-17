subroutine da_intpsfc_tem (val, ho, po, to, height, pressure, temp, kts, kte)

   !-----------------------------------------------------------------------
   ! Purpose: Correct temperature between two levels.
   !
   ! Reference: 
   ! ---------
   ! The use of surface observations in four dimensional data assimilation
   !  Using a mesoscale model.
   !  Ruggiero et al., 1996, Monthly Weather Review, Volume 124, 1018-1033
   !
   !----------------------------------------------------------------------------

   implicit none

   real,    intent (out) :: val
   integer, intent (in)  :: kts, kte
   real,    intent (in)  :: ho, po, to
   real,    intent (in)  :: height(kts:kte)
   real,    intent (in)  :: pressure(kts:kte)
   real,    intent (in)  :: temp(kts:kte)

   real    :: prs_mb(kts:kte)
   ! calculated but never used
   !      real    :: dth_12, dth_21, dth_sfc, dth_obs
   !     real    :: dhe_12, dhe_21, dhe_sfc1, dhe_obs1, dhe_sfc2, dhe_obs2
   real    :: dth_21, dth_sfc, dth_obs
   real    :: dhe_12, dhe_sfc1, dhe_obs1, dhe_sfc2, dhe_obs2
   real    :: th_100mb, th_200mb, th_obs, th_sfc
   real    :: th_obs_int, th_sfc_int
   real    :: pdif, rcp
   integer :: k_100mb, k_200mb, jk
   integer :: inc_100mb, inc_200mb

   if (trace_use_dull) call da_trace_entry("da_intpsfc_tem")

   rcp = gas_constant/cp

   ! 1.Find levels: model surface + 100hpa and model surface + 200hpa ar obs loc
   ! ===========================================================================

   ! 1.1 Convert model pressure profile from Pa to hPa  

   prs_mb = pressure / 100.0

   ! 1.2  Find levels surface + 100hPA

   inc_100mb = 100.0
   k_100mb   = kts

   do jk =  kts+1, kte
      pdif = prs_mb (kts) - prs_mb (jk)
      if (pdif .GE. inc_100mb) then
         k_100mb = jk
         exit
      end if
   end do

   ! 1.2  Find levels surface + 200hPA

   inc_200mb = 200.0
   k_200mb   = kts

   do jk =  kts+1, kte
      pdif = prs_mb (kts) - prs_mb (jk)
      if (pdif .GE. inc_200mb) then
         k_200mb = jk
         exit
      end if
   end do

   ! 1.3  Check consistency 

   if ((k_100mb .LE. kts) .OR. (k_200mb .LE. kts) .OR. &
        (k_200mb .LT. k_100mb)) then
      write (unit=message(1),fmt='(A)') ' Cannot find sfc + 200hPa and sfc + 100hPa' 
      write (unit=message(2),fmt='(A,I2,A,F10.3)') ' P (',k_200mb,') = ',prs_mb (k_200mb) 
      write (unit=message(3),fmt='(A,I2,A,F10.3)') ' P (',k_100mb,') = ',prs_mb (k_100mb) 
      write (unit=message(4),fmt='(A,F10.3)')      ' P_SFC  = ',         prs_mb (kts) 
      call da_warning(__FILE__,__LINE__,message(1:4))
      val = missing_r
      if (trace_use_dull) call da_trace_exit("da_intpsfc_tem")
      return
   end if

   ! 2.  potential temperature 
   ! =========================

   ! 2.1 Potential temperature at 100hPa above model surface

   th_100mb = temp (k_100mb) * (1000.0 / prs_mb (k_100mb))**rcp

   ! 2.2 Potential temperature at 200hPa above model surface

   th_200mb = temp (K_200mb) * (1000.0 / prs_mb (k_200mb))**rcp

   ! 2.3 Potential temperature at observation location

   th_obs   = to * (1000.0 / (po/100.0)) ** rcp


   ! 3.  lapse rate between surface+200hpa and surface+100hpa
   ! =========================================================

   ! 3.1 Potential temperature increment
    
   dth_21 = th_100mb - th_200mb
   ! never used
   ! dth_12 = th_200mb - th_100mb

   ! 3.1 Height increments
    
   ! never used
   ! dhe_21   = height (k_100mb)- height (k_200mb)
   dhe_sfc1 = height (k_100mb)- height (kts)
   dhe_obs1 = height (k_100mb)- ho

   dhe_12   = height (k_200mb)- height (k_100mb)
   dhe_sfc2 = height (k_200mb)- height (kts)
   dhe_obs2 = height (k_200mb)- ho

   ! 3.2 Extrapolated potential temperature at model surface and observation loc

   th_sfc_int = th_100mb + (dth_21/dhe_12) * dhe_sfc1 
   th_obs_int = th_100mb + (dth_21/dhe_12) * dhe_obs1 


   ! 4.  Bring temperature onto model surface
   ! ========================================

   ! 4.1 Difference at observation locations

   dth_obs = th_obs_int - th_obs

   ! 4.2 Difference at model surface

   dth_sfc = (dhe_sfc2/dhe_obs2) * dth_obs

   ! 4.3 Potentiel temperature brought to model surface

   th_sfc = th_sfc_int - dth_sfc

   ! 4.3 Corresponding Temperature

   val  = th_sfc * (prs_mb (kts) / 1000.0)**rcp

   if (trace_use_dull) call da_trace_exit("da_intpsfc_tem")

end subroutine da_intpsfc_tem


