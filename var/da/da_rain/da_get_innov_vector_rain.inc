subroutine da_get_innov_vector_rain( it, num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n        ! Loop counter.
   integer :: i, j     ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: model_rainc   ! Model value rainc at oblocation.
   real    :: model_rainnc  ! Model value rainnc at oblocation.

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_rain")

   if ( it > 1 ) then
      do n=iv%info(rain)%n1,iv%info(rain)%n2
            if (iv%rain(n)%rain%qc == fails_error_max) iv%rain(n)%rain%qc = 0
      end do
   end if

#ifdef DM_PARALLEL
#include "HALO_EM_RAIN.inc"
#endif

   do n=iv%info(rain)%n1,iv%info(rain)%n2
      
      ! [1.0] Get horizontal interpolation weights: 

      i   = iv%info(rain)%i(1,n)
      j   = iv%info(rain)%j(1,n)
      dx  = iv%info(rain)%dx(1,n)
      dy  = iv%info(rain)%dy(1,n)
      dxm = iv%info(rain)%dxm(1,n)
      dym = iv%info(rain)%dym(1,n)

      model_rainc = dym*(dxm*grid%rainc(i,j)   + dx*grid%rainc(i+1,j)) + &
                    dy *(dxm*grid%rainc(i,j+1) + dx*grid%rainc(i+1,j+1))
      model_rainnc = dym*(dxm*grid%rainnc(i,j)   + dx*grid%rainnc(i+1,j)) + &
                    dy *(dxm*grid%rainnc(i,j+1) + dx*grid%rainnc(i+1,j+1))

      ! [2.0] Initialise components of innovation vector: 

      iv % rain(n) % rain % inv = 0.0

      ! [3.0] To compute the 'inv':

      if (ob % rain(n) % rain > missing_r .and. iv % rain(n) % rain % qc >=  obs_qc_pointer) then
         iv % rain(n) % rain % inv = ob % rain(n) % rain - model_rainc - model_rainnc
      else
         iv % rain(n) % rain % inv = 0.0
      end if	 
     
   end do 
   ! -----------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_rain(iv,ob, it, num_qcstat_conv)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_rain")

end subroutine da_get_innov_vector_rain


