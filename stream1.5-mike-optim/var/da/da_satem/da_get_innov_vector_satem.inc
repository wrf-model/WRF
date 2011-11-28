subroutine da_get_innov_vector_satem(it, num_qcstat_conv,grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !
   !  03/10/2008   Y.-R. Guo
   !
   !    To skip the iv % satem(n) % thickness(k) % inv calculation above the
   !    model lid.
   !
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(in)    :: grid
   integer,       intent(in)    :: it       ! External iteration.
   type(y_type),  intent(in)    :: ob       ! Observation structure.
   type(iv_type), intent(inout) :: iv       ! O-B structure.
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: model_thickness(1:max_ob_levels) !Model thickness at ob loc

   real    :: pre_ma(kts-1:kte+1)
   real    :: tv_ma(kts-1:kte+1)
   integer :: layer1,layer2,mm
   real    :: tv1,tv2,pres2
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_satem")

   if ( it > 1 ) then
      do n=iv%info(satem)%n1,iv%info(satem)%n2
         do k = 1, iv%info(satem)%levels(n)
            if (iv%satem(n)%thickness(k)%qc == fails_error_max) iv%satem(n)%thickness(k)%qc = 0
         end do
      end do
   end if

   do n=iv%info(satem)%n1,iv%info(satem)%n2
      num_levs = iv%info(satem)%levels(n)

      if (num_levs < 1) cycle

      model_thickness(:) = 0.0

      ! [1.0] Get cross pt. horizontal interpolation weights:

      i   = iv%info(satem)%i(1,n)
      dy  = iv%info(satem)%dy(1,n)
      dym = iv%info(satem)%dym(1,n)
      j   = iv%info(satem)%j(1,n)
      dx  = iv%info(satem)%dx(1,n)
      dxm = iv%info(satem)%dxm(1,n)

      !------------------------------------------------------------------------

      ! [2.0] Calculate vertical profile of virtual temperature at obs pt.

      call da_tv_profile(grid,iv%info(satem),n,pre_ma,tv_ma)

      ! [3.0] Find model vertical position of pressure and do interp.

      call da_find_layer(layer2,tv2,iv%satem(n)%ref_p,pre_ma,tv_ma,kts,kte)
      pres2 = iv%satem(n)%ref_p

      ! [4.0] Thickness innovations calculation

      do k = 1, num_levs
         iv % satem(n) % thickness(k) % inv = 0.0

      if ( iv%satem(n)%p(k) >= iv%ptop) then
         call da_find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma,kts,kte)
         call da_thickness(pre_ma,tv_ma,kts,kte,tv1,tv2,layer1,layer2,   &
            iv%satem(n)%p(k),pres2,model_thickness(k))

         if (ABS(ob % satem(n) % thickness(k) - missing_r) > 1.0 .and. &
              iv % satem(n) % thickness(k)%qc /= missing_data) then
            iv % satem(n) % thickness(k) % inv = ob % satem(n) % thickness(k) - model_thickness(k)
!                  mm = mm + 1
!                  write(101,'(A,3I6,2x,A, 2F10.3,F10.0,A,F5.0,2(A,F10.3),A,I8)') &
!                    "num, n,k:", mm, n, k, &
!                    "observed, model_thickness, layer = ", &
!                     ob%satem(n)%thickness(k), &
!                     model_thickness(k), 0.01*pres2, " -", &
!                     iv%satem(n)%p(k)*0.01,'hPa  ob_error:',  &
!                     iv%Satem(n)%thickness(k)%error, " inv=", &
!                     iv%Satem(n)%thickness(k)%inv,   " qc =", &
!                     iv%Satem(n)%thickness(k)%qc
         end if

         pres2 = iv%satem(n)%p(k)
         layer2 = layer1
         tv2 = tv1
      else

!   For other type of OBS, such as SOUND, the vertical range check was
!   complted in DA_Interpolation/to_zk.inc, but SATEM never used to_zk.
!   So it is need the check above the model lid (YRG, 02/21/2008):

               iv%Satem(n)%thickness(k)%qc = above_model_lid
      endif

      end do
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_satem(iv, it, num_qcstat_conv)        

   !------------------------------------------------------------------------
   ! [6.0] Perform land/ocean check
   !------------------------------------------------------------------------

   do n=iv%info(satem)%n1,iv%info(satem)%n2
      i   = iv%info(satem)%i(1,n)
      j   = iv%info(satem)%j(1,n)
      if (grid%xb%landmask(i,j ) /= 0.0 .or. grid%xb%landmask(i+1,j ) /= 0. .or.  &
          grid%xb%landmask(i,j+1) /= 0.0 .or. grid%xb%landmask(i+1,j+1) /= 0.0) then
         iv % satem(n) % thickness(1) % inv = 0.0
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_satem")

end subroutine da_get_innov_vector_satem


