  subroutine da_deallocate_radiance ( ob, iv, j)

   !-----------------------------------------------------------------------
   ! Purpose: deallocate radiance related structures/arrays
   ! Extracted from da_solve.inc
   !-----------------------------------------------------------------------

   implicit none

   type (y_type),        intent(inout)  :: ob        ! Observation structure.
   type (iv_type),       intent(inout)  :: iv        ! Obs. increment structure.
   type (j_type),        intent(inout)  :: j         ! Cost function.

   integer                        :: i,n,ichan

   if (trace_use) call da_trace_entry("da_deallocate_radiance")

      do i =1, iv%num_inst
         deallocate (j % jo % rad(i) % jo_ichan)
         deallocate (j % jo % rad(i) % num_ichan)
         deallocate (satinfo(i) % ichan)
         deallocate (satinfo(i) % iuse)
         deallocate (satinfo(i) % error)
         deallocate (satinfo(i) % error_cld)
         deallocate (satinfo(i) % polar)

         deallocate (satinfo(i) % scanbias) 
         deallocate (satinfo(i) % scanbias_b)
         deallocate (satinfo(i) % bcoef)
         deallocate (satinfo(i) % bcoef0)
         deallocate (satinfo(i) % error_std)

         deallocate (ob%instid(i) % ichan)
         deallocate (iv%instid(i) % ichan)

         if (iv%instid(i)%num_rad > 0) then
 
            deallocate (iv%instid(i)%info%date_char)
            deallocate (iv%instid(i)%info%name)
            deallocate (iv%instid(i)%info%platform)
            deallocate (iv%instid(i)%info%id)
            deallocate (iv%instid(i)%info%levels)     
            deallocate (iv%instid(i)%info%lat)      
            deallocate (iv%instid(i)%info%lon)      
            deallocate (iv%instid(i)%info%elv)   

            deallocate (iv%instid(i)%info%pstar)
            deallocate (iv%instid(i)%info%i)
            deallocate (iv%instid(i)%info%j)
            deallocate (iv%instid(i)%info%k)
            deallocate (iv%instid(i)%info%zk)
            deallocate (iv%instid(i)%info%dx)
            deallocate (iv%instid(i)%info%dy)
            deallocate (iv%instid(i)%info%dz)
            deallocate (iv%instid(i)%info%dxm)
            deallocate (iv%instid(i)%info%dym)
            deallocate (iv%instid(i)%info%dzm)
            deallocate (iv%instid(i)%info%proc_domain)

            deallocate (iv%instid(i)%t)
            deallocate (iv%instid(i)%mr)
            deallocate (iv%instid(i)%tm)
            deallocate (iv%instid(i)%qm)
            deallocate (iv%instid(i)%qrn)
            deallocate (iv%instid(i)%qcw)
            if ( crtm_cloud ) then
               deallocate (iv%instid(i)%qci)
               deallocate (iv%instid(i)%qsn)
               deallocate (iv%instid(i)%qgr)
               deallocate (iv%instid(i)%qhl)
               deallocate (iv%instid(i)%rcw)
               deallocate (iv%instid(i)%rci)
               deallocate (iv%instid(i)%rrn)
               deallocate (iv%instid(i)%rsn)
               deallocate (iv%instid(i)%rgr)
               deallocate (iv%instid(i)%rhl)
            end if
            deallocate (iv%instid(i)%pm)
            deallocate (iv%instid(i)%pf)
            deallocate (iv%instid(i)%u10)
            deallocate (iv%instid(i)%v10)
            deallocate (iv%instid(i)%t2m)
            deallocate (iv%instid(i)%q2m)
            deallocate (iv%instid(i)%mr2m)
            deallocate (iv%instid(i)%psfc)
            deallocate (iv%instid(i)%ts)
            deallocate (iv%instid(i)%smois)
            deallocate (iv%instid(i)%tslb)
            deallocate (iv%instid(i)%snowh)
            deallocate (iv%instid(i)%isflg)
            deallocate (iv%instid(i)%soiltyp)
            deallocate (iv%instid(i)%landsea_mask)
            if (rtm_option == rtm_option_rttov) then
               deallocate (iv%instid(i)%surftype)
               deallocate (iv%instid(i)%snow_frac)
            end if
            deallocate (iv%instid(i)%elevation)
            deallocate (iv%instid(i)%vegfra)
            deallocate (iv%instid(i)%vegtyp)
            deallocate (iv%instid(i)%clwp)
            if ( index(iv%instid(i)%rttovid_string,'amsr2') > 0 ) then
               deallocate (iv%instid(i)%clw)
            end if
            if ( index(iv%instid(i)%rttovid_string, 'ahi') > 0 ) then
               deallocate (iv%instid(i)%cloudflag)
            end if			
            deallocate (iv%instid(i)%ps)
            deallocate (iv%instid(i)%tb_xb)
            if ( crtm_cloud ) then
               deallocate (iv%instid(i)%tb_xb_clr)
            end if
            deallocate (iv%instid(i)%tb_qc)
            deallocate (iv%instid(i)%tb_inv)
            deallocate (iv%instid(i)%tb_error)
            deallocate (iv%instid(i)%tb_sens)
            deallocate (iv%instid(i)%tb_imp)
            deallocate (iv%instid(i)%rad_xb)
            deallocate (iv%instid(i)%rad_obs)
            deallocate (iv%instid(i)%rad_ovc)
            deallocate (iv%instid(i)%emiss)
            deallocate (iv%instid(i)%scanpos)
            deallocate (iv%instid(i)%scanline)
            deallocate (iv%instid(i)%ifgat)
            deallocate (iv%instid(i)%cloud_flag)
            deallocate (iv%instid(i)%rain_flag)
            deallocate (iv%instid(i)%satzen)
            deallocate (iv%instid(i)%satazi)
            deallocate (iv%instid(i)%solzen)
            deallocate (iv%instid(i)%solazi)
            deallocate (iv%instid(i)%gamma_jacobian)

           if (ANY(use_satcv)) then
	      if (use_satcv(2)) then
                 do n = 1,iv%instid(i)%num_rad
                    deallocate (iv%instid(i)%cv_index(n)%cc)
                    deallocate (iv%instid(i)%cv_index(n)%vtox)
                 end do
	      end if
              deallocate (iv%instid(i)%cv_index)  
           end if
        
           if ( use_rttov_kmatrix .or. use_crtm_kmatrix ) then
              deallocate(iv%instid(i)%ts_jacobian)
              deallocate(iv%instid(i)%ps_jacobian)
              deallocate(iv%instid(i)%emiss_jacobian)
              deallocate(iv%instid(i)%windspeed_jacobian)
              deallocate(iv%instid(i)%t_jacobian)
              deallocate(iv%instid(i)%q_jacobian)
           end if
            if (rtm_option == rtm_option_crtm) then
               deallocate(iv%instid(i)%crtm_climat)
               deallocate(iv%instid(i)%water_coverage)
               deallocate(iv%instid(i)%land_coverage)
               deallocate(iv%instid(i)%ice_coverage)
               deallocate(iv%instid(i)%snow_coverage)
               if (use_crtm_kmatrix) then
                 if ( crtm_cloud ) then
                     deallocate(iv%instid(i)%water_jacobian)
                     deallocate(iv%instid(i)%ice_jacobian)
                     deallocate(iv%instid(i)%rain_jacobian)
                     deallocate(iv%instid(i)%snow_jacobian)
                     deallocate(iv%instid(i)%graupel_jacobian)
                     deallocate(iv%instid(i)%hail_jacobian)
                     deallocate(iv%instid(i)%water_r_jacobian)
                     deallocate(iv%instid(i)%ice_r_jacobian)
                     deallocate(iv%instid(i)%rain_r_jacobian)
                     deallocate(iv%instid(i)%snow_r_jacobian)
                     deallocate(iv%instid(i)%graupel_r_jacobian)
                     deallocate(iv%instid(i)%hail_r_jacobian)
                 end if
                 if ( calc_weightfunc ) then
                    deallocate(iv%instid(i)%lod)
                    deallocate(iv%instid(i)%lod_jacobian)
                    deallocate(iv%instid(i)%trans)
                    deallocate(iv%instid(i)%trans_jacobian)
                    deallocate(iv%instid(i)%der_trans)
                 end if
              end if
            end if

         end if

	  if ( use_rad .and. (use_varbc.or.freeze_varbc) ) then
	     if (iv%instid(i)%varbc_info%npredmax > 0) then
                deallocate (iv%instid(i)%varbc_info%pred)
                deallocate (iv%instid(i)%varbc_info%pred_mean)
                deallocate (iv%instid(i)%varbc_info%pred_std)
                deallocate (iv%instid(i)%varbc_info%nbgerr)
	     end if
	     do ichan = 1, iv%instid(i)%nchan
	        if (iv%instid(i)%varbc(ichan)%npred <= 0) cycle	 
	        deallocate (iv%instid(i)%varbc(ichan)%pred_use)
	        deallocate (iv%instid(i)%varbc(ichan)%ipred)
	        deallocate (iv%instid(i)%varbc(ichan)%index)
	        deallocate (iv%instid(i)%varbc(ichan)%param)
    	        deallocate (iv%instid(i)%varbc(ichan)%bgerr)
	        deallocate (iv%instid(i)%varbc(ichan)%vtox)
	     end do
   	     deallocate (iv%instid(i)%varbc)
	  end if
#ifdef RTTOV
         if (rtm_option == rtm_option_rttov) then
            call rttov_dealloc_coefs (ierr,coefs(i))
            if ( ierr /= 0 ) then
               call da_error(__FILE__,__LINE__,(/'failure in rttov_dealloc_coefs'/))
            end if
         end if
#endif
      end do
      deallocate (iv%instid)
      deallocate (j % jo % rad)     
      deallocate (satinfo)

   if (trace_use) call da_trace_exit ("da_deallocate_radiance")

end subroutine da_deallocate_radiance

