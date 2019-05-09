subroutine da_get_innov_vector_chem_sfc( it, num_qcstat_conv, grid, ob, iv)
   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n,ichem        ! Loop counters

   real, allocatable :: model_chemic(:,:)
   real, allocatable :: model_chemic_surf(:,:)
   real, allocatable :: model_rho(:,:)

   if (trace_use) call da_trace_entry("da_get_innov_vector_chem_sfc")

   if ( it > 1 ) then
      do n=iv%info(chemic_surf)%n1,iv%info(chemic_surf)%n2
         do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
            if (iv%chemic_surf(n)%chem(ichem)%qc == fails_error_max) iv%chemic_surf(n)%chem(ichem)%qc = 0
         end do
      end do
   end if

   ! [0.0]  Get chem surf conc.

   allocate (model_chemic(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chem))
   allocate (model_chemic_surf(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chemic_surf))
   allocate (model_rho(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chem))

   do ichem = PARAM_FIRST_SCALAR ,num_chem
         ! [1.0] Get horizontal interpolation weights: 
      call da_interp_lin_2d (grid%xbchem%chem_ic(:,:,1,ichem),  iv%info(chemic_surf), 1, model_chemic(:,ichem))
      call da_interp_lin_2d (grid%xb%rho(:,:,1),  iv%info(chemic_surf), 1, model_rho(:,ichem))
   end do

   model_chemic_surf(:,p_chemsi_pm25)=model_rho(:,p_chem_ic_p25)*(model_chemic(:,p_chem_ic_p25)                                               + &
        1.375*(96.06/28.964*1000)*model_chemic(:,p_chem_ic_sulf)                                                                                  + &
	model_chemic(:,p_chem_ic_bc1)+model_chemic(:,p_chem_ic_bc2)+1.8*(model_chemic(:,p_chem_ic_oc1)+model_chemic(:,p_chem_ic_oc2))       + &
        model_chemic(:,p_chem_ic_dust_1)+0.286*model_chemic(:,p_chem_ic_dust_2)+model_chemic(:,p_chem_ic_seas_1)                              + &
        0.942*model_chemic(:,p_chem_ic_seas_2))

!!! for pm10 !!!
!   model_chemic_surf(:,:,p_chemsi_pm10)=model_rho(:,:,p_chem_ic_p10)*(model_chemic(:,:,p_chem_ic_p25)+model_chemic(:,:,p_chem_ic_p10)               + &
!        1.375*(96.06/28.964*1000)*model_chemic(:,:,p_chem_ic_sulf)+model_chemic(:,:,p_chem_ic_bc1)+model_chemic(:,:,p_chem_ic_bc2)                  + &
!        1.8*(model_chemic(:,:,p_chem_ic_oc1)+model_chemic(:,:,p_chem_ic_oc2))+model_chemic(:,:,p_chem_ic_dust_1)+model_chemic(:,:,p_chem_ic_dust_2) + &
!        model_chemic(:,:,p_chem_ic_dust_3)+0.87*model_chemic(:,:,p_chem_ic_dust_4)+model_chemic(:,:,p_chem_ic_seas_1)                               + &
!        model_chemic(:,:,p_chem_ic_seas_2)+model_chemic(:,:,p_chem_ic_seas_3))

   do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
 
      do n=iv%info(chemic_surf)%n1,iv%info(chemic_surf)%n2

         ! [2.0] Initialise components of innovation vector: 

         iv % chemic_surf(n) % chem(ichem) % inv = 0.0

         ! [3.0] To compute the 'inv':
         if (ob % chemic_surf(n) % chem(ichem) > missing_r .and. &
            iv % chemic_surf(n) % chem(ichem) % qc >=  obs_qc_pointer) then
            iv % chemic_surf(n) % chem(ichem) % inv = ob % chemic_surf(n) % chem(ichem) - model_chemic_surf(n,ichem)

!	    iv % chemic_surf(n) % chem(ichem) % error = sqrt((1.0+0.0075*ob % chemic_surf(n) % chem(ichem))**2+ &
!                 0.5*(1.0+0.0075*ob % chemic_surf(n) % chem(ichem))*sqrt(grid%dx/2000.0))

             iv % chemic_surf(n) % chem(ichem) % error = 1.5*sqrt((1.0+0.0075*ob % chemic_surf(n) % chem(ichem))**2+ &
                  0.5*(1.0+0.0075*ob % chemic_surf(n) % chem(ichem))*sqrt(grid%dx/2000.0))

	    if (abs(iv % chemic_surf(n) % chem(ichem) % inv) .ge. 120) then
                iv % chemic_surf(n) % chem(ichem) % inv = missing_r
                iv % chemic_surf(n) % chem(ichem) % qc  = missing
		iv % chemic_surf(n) % chem(ichem) % error = missing_r
            end if
         else
            iv % chemic_surf(n) % chem(ichem) % inv = missing_r
         end if	      
      end do
   end do

   deallocate(model_chemic)
   deallocate(model_chemic_surf)
   deallocate(model_rho)
 
   ! -----------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_chem_sfc(iv,ob, it, num_qcstat_conv)

   if (trace_use) call da_trace_exit("da_get_innov_vector_chem_sfc")

end subroutine da_get_innov_vector_chem_sfc


