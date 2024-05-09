subroutine da_get_innov_vector_lightning( it, grid, ob, iv)
   !-----------------------------------------------------------------------
   ! Purpose: TBD 
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)   
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.

   integer :: n              ! Loop counter.
   integer :: i, j, k        ! Index dimension.
   real    :: dx, dxm        ! Interpolation weights.
   real    :: dy, dym        ! Interpolation weights.
   integer :: num_levs       ! obs vertical levels
   real    :: div(kts:kte)   ! Model divergence at ob loc   
   real    :: w(kts:kte)     ! Model vertical velocity  

   if (trace_use) call da_trace_entry("da_get_innov_vector_lightning")

   if(it>1) then
      do n = iv%info(lightning)%n1, iv%info(lightning)%n2
         do k = 1, iv%info(lightning)%levels(n)	  
            if(iv%lightning(n)%  w(k)%qc == fails_error_max) iv%lightning(n)%  w(k)%qc = 0
            if(iv%lightning(n)%div(k)%qc == fails_error_max) iv%lightning(n)%div(k)%qc = 0
            if(iv%lightning(n)% qv(k)%qc == fails_error_max) iv%lightning(n)% qv(k)%qc = 0
         end do
      end do
   end if

   do n = iv%info(lightning)%n1, iv%info(lightning)%n2
      num_levs = iv%info(lightning)%levels(n)

      if(num_levs<1) cycle
	  
      div(:) = 0.0
      w(:) = 0.0
	  
      ! [1.0] Get cross pt. horizontal interpolation weights:

      i   = iv%info(lightning)%i(1,n)
      dy  = iv%info(lightning)%dy(1,n)
      dym = iv%info(lightning)%dym(1,n)
      j   = iv%info(lightning)%j(1,n)
      dx  = iv%info(lightning)%dx(1,n)
      dxm = iv%info(lightning)%dxm(1,n)

      ! [2.0] Calculate vertical profile of divergence and qv at obs pt.
	  	  	  	  
      do k = 1, num_levs 
         iv % lightning(n) %   w(k) % inv = 0.0
         iv % lightning(n) % div(k) % inv = 0.0
         iv % lightning(n) %  qv(k) % inv = 0.0

         if(use_lightning_w) then
           if(ob%lightning(n)%w(k) > missing_r .and. iv%lightning(n)%w(k)%qc >= obs_qc_pointer) then
             w(k) = dym*(dxm*grid%xb%w(i,j,k)+dx*grid%xb%w(i+1,j,k))+dy*(dxm*grid%xb%w(i,j+1,k)+dx*grid%xb%w(i+1,j+1,k))
             iv%lightning(n)%w(k)%qc  = obs_qc_pointer
             iv%lightning(n)%w(k)%inv = ob%lightning(n)%w(k) - w(k)
           end if
         end if

         if(use_lightning_div) then
           if(ob%lightning(n)%div(k) > missing_r .and. iv%lightning(n)%div(k)%qc >= obs_qc_pointer) then 
             iv%lightning(n)%div(k)%qc  = obs_qc_pointer
             call da_div_profile(grid, iv%info(lightning), n, k, div(k))                   
             iv%lightning(n)%div(k)%inv = ob%lightning(n)%div(k) - div(k)
           end if
         end if

         if(use_lightning_qv) then
           if(ob%lightning(n)%qv(k) > missing_r .and. iv%lightning(n)%qv(k)%qc >= obs_qc_pointer) then		
	     iv%lightning(n)%qv(k)%inv = ob%lightning(n)%qv(k) - grid%xb%q(i,j,k)
             iv%lightning(n)%qv(k)%inv = amax1(0.0, iv%lightning(n)%qv(k)%inv)
           end if
         end if

      end do
   end do   

   ! -----------------------------------------------------------------------
   ! [3.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if(check_max_iv ) &
     call da_check_max_iv_lightning(iv, ob, it)

   if (trace_use) call da_trace_exit("da_get_innov_vector_lightning")

end subroutine da_get_innov_vector_lightning
