subroutine da_get_innov_vector_rain( it, num_qcstat_conv, grid, ob, iv, hr_rainc, hr_rainnc)
   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)
   real, dimension(ims:ime,jms:jme), intent(inout) :: hr_rainc, hr_rainnc

   integer :: n        ! Loop counter.

   real, allocatable :: model_rainc(:)
   real, allocatable :: model_rainnc(:)

   if (trace_use) call da_trace_entry("da_get_innov_vector_rain")

   if ( it > 1 ) then
      do n=iv%info(rain)%n1,iv%info(rain)%n2
            if (iv%rain(n)%rain%qc == fails_error_max) iv%rain(n)%rain%qc = 0
      end do
   end if

   ! [0.0]  Get hourly rainfall

   allocate (model_rainc(iv%info(rain)%n1:iv%info(rain)%n2))
   allocate (model_rainnc(iv%info(rain)%n1:iv%info(rain)%n2))
   model_rainc  = 0.0
   model_rainnc = 0.0

   ! [1.0] horizontal interpolation:

   call da_interp_lin_2d (hr_rainc,  iv%info(rain), 1, model_rainc)
   call da_interp_lin_2d (hr_rainnc, iv%info(rain), 1, model_rainnc)

   do n=iv%info(rain)%n1,iv%info(rain)%n2

      ! [2.0] Initialise components of innovation vector: 

      iv % rain(n) % rain % inv = 0.0

      ! [3.0] To compute the 'inv':

      if (ob % rain(n) % rain > missing_r .and. iv % rain(n) % rain % qc >=  obs_qc_pointer) then
         iv % rain(n) % rain % inv = ob % rain(n) % rain - model_rainc(n) - model_rainnc(n)
      else
         iv % rain(n) % rain % inv = 0.0
      end if	 
     
   end do

   deallocate(model_rainc)
   deallocate(model_rainnc)
 
   ! -----------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_rain(iv,ob, it, num_qcstat_conv)

   if (trace_use) call da_trace_exit("da_get_innov_vector_rain")

end subroutine da_get_innov_vector_rain


