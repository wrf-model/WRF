subroutine da_get_innov_vector_ssmi_rv (it,num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,       intent(in)    :: it         ! External iteration.
   type(domain),  intent(in)    :: grid       ! first guess state.
   type(y_type),  intent(in)    :: ob         ! Observation structure.
   type(iv_type), intent(inout) :: iv         ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n           ! Loop counter.
   integer :: i, j        ! Index dimension.
   real    :: dx, dxm     ! Interpolation weights.
   real    :: dy, dym     ! Interpolation weights.
   real    :: model_tpw   ! Model value tpw   at oblocation
   real    :: model_speed ! Model value speed at oblocation
   integer :: itpw, itpwf, ispeed, ispeedf 
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_ssmi_rv")

   if ( it > 1 ) then
      do n=iv%info(ssmi_rv)%n1,iv%info(ssmi_rv)%n2
         if ( iv % ssmi_rv(n) % speed % qc == fails_error_max) iv % ssmi_rv(n) % speed % qc = 0
         if ( iv % ssmi_rv(n) % tpw % qc == fails_error_max) iv % ssmi_rv(n) % tpw % qc = 0
      end do
   end if
   
   do n=iv%info(ssmi_rv)%n1,iv%info(ssmi_rv)%n2

      ! compute innovation vector
      ! =========================

      ! Obs coordinates on model grid

      ! TPW

      i   = iv%info(ssmi_rv)%i(1,n)
      j   = iv%info(ssmi_rv)%j(1,n)
      dx  = iv%info(ssmi_rv)%dx(1,n)
      dy  = iv%info(ssmi_rv)%dy(1,n)
      dxm = iv%info(ssmi_rv)%dxm(1,n)
      dym = iv%info(ssmi_rv)%dym(1,n)

      iv % ssmi_rv(n) % tpw % inv  = 0.0
      if (abs(ob % ssmi_rv(n) % tpw - missing_r) > 1.0  .and. iv % ssmi_rv(n) % tpw % qc >= obs_qc_pointer) then
         model_tpw = dym*(dxm*grid%xb%tpw(i,j) + dx*grid%xb%tpw(i+1,j)) + dy *(dxm*grid%xb%tpw(i,j+1) + dx*grid%xb%tpw(i+1,j+1))
         iv % ssmi_rv(n) % tpw % inv = ob % ssmi_rv(n) % tpw - model_tpw
      end if

      ! surface wind speed

      iv % ssmi_rv(n) % speed % inv  = 0.0
      if (abs(ob % ssmi_rv(n) % speed - missing_r) > 1.0 .and.     &
          iv % ssmi_rv(n) % speed % qc >= obs_qc_pointer) then

         model_speed = dym*(dxm*grid%xb%speed(i,j ) + dx*grid%xb%speed(i+1,j )) &
            + dy *(dxm*grid%xb%speed(i,j+1) + dx*grid%xb%speed(i+1,j+1))
         iv % ssmi_rv(n) % speed % inv = ob % ssmi_rv(n) % speed - model_speed
      end if
   end do

   !------------------------------------------------------------------
   ! Perform optional maximum error check:
   !------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_ssmi_rv(iv, it, num_qcstat_conv)           

   if (trace_use) call da_trace_exit("da_get_innov_vector_ssmi_rv")

end subroutine da_get_innov_vector_ssmi_rv


