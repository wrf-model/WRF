subroutine da_get_innov_vector_chem_acft( it, num_qcstat_conv, ob, iv)
   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n,ichem,iplt   ! Loop counters

   real, allocatable :: model_chem(:)

   if (trace_use) call da_trace_entry("da_get_innov_vector_chem_acft")

   if ( it > 1 ) then
      do n=iv%info(chem_acft)%n1,iv%info(chem_acft)%n2
         do ichem = 1, num_chem_acft-1
            do iplt = 1, num_platform
               if (iv%chem_acft(n)%chem(ichem,iplt)%qc == fails_error_max) iv%chem_acft(n)%chem(ichem,iplt)%qc = 0
            end do
         end do
      end do
   end if

   ! [0.0]  Get chem acft conc.

   allocate (model_chem(iv%info(chem_acft)%n1:iv%info(chem_acft)%n2))
   do ichem = 1 ,num_chem_acft-1
      do iplt = 1, num_platform
         do n=iv%info(chem_acft)%n1,iv%info(chem_acft)%n2
         
            ! [1.0] Get horizontal interpolation weights: 
   
!            call da_interp_lin_3d (chem,  iv%info(chem_acft), 1, model_chem) 

            model_chem(n) = head_grid%acft_hx(n,iplt,ichem+1)
   
            ! [2.0] Initialise components of innovation vector: 
   
            iv % chem_acft(n) % chem(ichem,iplt) % inv = 0.D0
   
            ! [3.0] To compute the 'inv':
   
            if (ob % chem_acft(n) % chem(ichem,iplt) > missing_r .and. &
                iv % chem_acft(n) % chem(ichem,iplt) % qc >=  obs_qc_pointer) then
   
               iv % chem_acft(n) % chem(ichem,iplt) % inv = ob % chem_acft(n) % chem(ichem,iplt) - model_chem(n)
            end if	      
         end do
      end do
   end do

   deallocate(model_chem)
 
   ! -----------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_chem_acft(iv,ob, it, num_qcstat_conv)

   if (trace_use) call da_trace_exit("da_get_innov_vector_chem_acft")

end subroutine da_get_innov_vector_chem_acft


