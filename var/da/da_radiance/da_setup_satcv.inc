subroutine da_setup_satcv(iv, be)

   !-----------------------------------------------------------------------
   ! Purpose: Set up satellite control variable
   !-----------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(inout) :: iv          ! Obs. increment structure.
   type (be_type), intent(inout) :: be          ! Background error structure.
   
   integer                      :: i, n, k, size_js, js_start, nclouds, ncv, kts_100hPa(1)
   integer                      :: satcv_size
   
   if (trace_use) call da_trace_entry("da_setup_satcv")
   
      size_js  = 0                        
      js_start = be % cv % size_jb + be % cv % size_je + be % cv % size_jp
      
      do i= 1, iv % num_inst
         allocate( iv%instid(i)%cv_index(iv%instid(i)%num_rad) )
         do n = 1, iv%instid(i)%num_rad
	    satcv_size = 0
	   
         ! Tskin
	 !------
	   if (use_satcv(1)) then
	      iv%instid(i)%cv_index(n)%ts = js_start + size_js + 1
              satcv_size = satcv_size + 1
	   end if
	      
	 ! Cloud Cover(s)  
	 !---------------
	   if (use_satcv(2)) then
              kts_100hPa = MAXLOC(iv%instid(i)%pm(kts:kte,n), &
                           MASK = iv%instid(i)%pm(kts:kte,n) < 100.0)
              nclouds    = kte - kts_100hPa(1) + 1
	      ncv        = nclouds !4
              allocate(iv%instid(i)%cv_index(n)%vtox(nclouds,nclouds))
              allocate(iv%instid(i)%cv_index(n)%cc(ncv))
	      iv%instid(i)%cv_index(n)%cc(:)   = (/ (js_start+size_js+satcv_size+k, k=1,ncv) /)
              iv%instid(i)%cv_index(n)%nclouds = nclouds
              iv%instid(i)%cv_index(n)%ncv     = ncv
              satcv_size = satcv_size + ncv
	   end if
	   
	   size_js = size_js + satcv_size
        end do
      end do
      
      be % cv % size_js = size_js
      cv_size_domain_js = size_js   
   
   if (trace_use) call da_trace_exit ("da_setup_satcv")

end subroutine da_setup_satcv
