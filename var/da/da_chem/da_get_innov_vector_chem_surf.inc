subroutine da_get_innov_vector_chem_surf( it, num_qcstat_conv, ob, iv)
   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
!   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n,ichem        ! Loop counters

   real, allocatable :: model_chem(:)

   if (trace_use) call da_trace_entry("da_get_innov_vector_chem_surf")

   if ( it > 1 ) then
      do n=iv%info(chem_surf)%n1,iv%info(chem_surf)%n2
         do ichem = 1, num_chem_surf-1
            if (iv%chem_surf(n)%chem(ichem)%qc == fails_error_max) iv%chem_surf(n)%chem(ichem)%qc = 0
         end do
      end do
   end if

   ! [0.0]  Get chem surf conc.

   allocate (model_chem(iv%info(chem_surf)%n1:iv%info(chem_surf)%n2))

   do ichem = 1 ,num_chem_surf-1
      do n=iv%info(chem_surf)%n1,iv%info(chem_surf)%n2
      
         ! [1.0] Get horizontal interpolation weights: 

!      call da_interp_lin_2d (chem,  iv%info(chem_surf), 1, model_chem) 

         model_chem(n) = head_grid%surf_hx(n,ichem+1)

         ! [2.0] Initialise components of innovation vector: 

         iv % chem_surf(n) % chem(ichem) % inv = 0.0

         ! [3.0] To compute the 'inv':

         if (ob % chem_surf(n) % chem(ichem) > missing_r .and. &
             iv % chem_surf(n) % chem(ichem) % qc >=  obs_qc_pointer) then

            iv % chem_surf(n) % chem(ichem) % inv = ob % chem_surf(n) % chem(ichem) - model_chem(n)
         else
            iv % chem_surf(n) % chem(ichem) % inv = 0.0
         end if	      
      end do
   end do

   deallocate(model_chem)
 
   ! -----------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_chem_surf(iv,ob, it, num_qcstat_conv)

   if (trace_use) call da_trace_exit("da_get_innov_vector_chem_surf")

end subroutine da_get_innov_vector_chem_surf


