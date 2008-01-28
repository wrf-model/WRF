subroutine da_get_innov_vector_satem(grid, it, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(in)    :: grid
   integer,       intent(in)    :: it       ! External iteration.
   type(y_type),  intent(in)    :: ob       ! Observation structure.
   type(iv_type), intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: model_thickness(1:max_ob_levels) !Model thickness at ob loc
   integer :: ithickness,ithicknessf

   real    :: pre_ma(kts-1:kte+1)
   real    :: tv_ma(kts-1:kte+1)
   integer :: layer1,layer2
   real    :: tv1,tv2,pres2
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_satem")

   ithickness = 0 ; ithicknessf = 0

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
         call da_find_layer(layer1,tv1,iv%satem(n)%p(k),pre_ma,tv_ma,kts,kte)
         call da_thickness(pre_ma,tv_ma,kts,kte,tv1,tv2,layer1,layer2,   &
            iv%satem(n)%p(k),pres2,model_thickness(k))

         if (ABS(ob % satem(n) % thickness(k) - missing_r) > 1.0 .and. &
              iv % satem(n) % thickness(k)%qc /= missing_data) then
            iv % satem(n) % thickness(k) % inv = ob % satem(n) % thickness(k) - model_thickness(k)
            ! write(unit=stdout,fmt="(A, 2F10.3,F10.0,A,F5.0,A)") &
            !    "observed, model_thickness, layer = ", &
            !    ob%satem(n)%thickness(k), &
            ! model_thickness(k), 0.01*pres2, " -", &
            ! iv%satem(n)%p(k)*0.01,'hPa'
         end if

         pres2 = iv%satem(n)%p(k)
         layer2 = layer1
         tv2 = tv1
      end do
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if (check_max_iv) then
      call da_check_max_iv_satem(iv, it, ithickness,ithicknessf)
   end if

   !------------------------------------------------------------------------
   ! [6.0] Perform land/ocean check
   !------------------------------------------------------------------------

   do n=iv%info(satem)%n1,iv%info(satem)%n2
      i   = iv%info(satem)%i(1,n)
      j   = iv%info(satem)%j(1,n)
      if (grid%xb%landmask(i,j ) /= 0.0 .or. grid%xb%landmask(i+1,j ) /= 0. .or.  &
          grid%xb%landmask(i,j+1) /= 0.0 .or. grid%xb%landmask(i+1,j+1) /= 0.0) then
         iv % satem(n) % thickness(1) % inv = 0.0
         ithicknessf = ithicknessf + 1
      end if
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
         ', Total Rejections for Satem follows:'
      write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
         'Number of failed thicknesses observations:     ',&
         ithicknessf, ' on ',ithickness
   end if
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_satem")

end subroutine da_get_innov_vector_satem


