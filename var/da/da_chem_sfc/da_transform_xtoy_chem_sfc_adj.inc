subroutine da_transform_xtoy_chem_sfc_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (xchem_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n, ichem        ! Loop counter.

   real, allocatable :: model_chemic(:,:)
   real, allocatable :: model_chemic_surf(:,:)
   real, allocatable :: model_rho(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_chem_sfc_adj")

   if (sfc_assi_options == sfc_assi_options_1) then

      allocate (model_chemic(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chem))
      allocate (model_chemic_surf(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chemic_surf))
      allocate (model_rho(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chem))

     jo_grad_x%chem_ic = 0.0
     model_chemic = 0.0
     model_chemic_surf = 0.0
     model_rho = 0.0

      do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
      ! [1.2] Interpolate horizontally:
        do n=iv%info(chemic_surf)%n1,iv%info(chemic_surf)%n2
           model_chemic_surf(n,ichem)  = jo_grad_y%chemic_surf(n)%chem(ichem)
        end do
      end do

     do ichem = PARAM_FIRST_SCALAR ,num_chem
         call da_interp_lin_2d (grid%xb%rho(:,:,1),  iv%info(chemic_surf), 1, model_rho(:,ichem))
      end do

     if (chem_cv_options == 10) then
        model_chemic(:,p_chem_ic_p25)=model_rho(:,p_chem_ic_p25)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_sulf)=model_rho(:,p_chem_ic_sulf)*1.375*(model_chemic_surf(:,p_chemsi_pm25))*(96.06/28.964*1000)
        model_chemic(:,p_chem_ic_bc1)=model_rho(:,p_chem_ic_bc1)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc2)=model_rho(:,p_chem_ic_bc2)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc1)=model_rho(:,p_chem_ic_oc1)*1.8*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc2)=model_rho(:,p_chem_ic_oc2)*1.8*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_dust_1)=model_rho(:,p_chem_ic_dust_1)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_dust_2)=model_rho(:,p_chem_ic_dust_2)*0.286*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_seas_1)=model_rho(:,p_chem_ic_seas_1)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_seas_2)=model_rho(:,p_chem_ic_seas_2)*0.942*(model_chemic_surf(:,p_chemsi_pm25))

        do ichem = PARAM_FIRST_SCALAR, num_chem
          call da_interp_lin_2d_adj (jo_grad_x%chem_ic(:,:,1,ichem), iv%info(chemic_surf), 1, model_chemic(:,ichem))
        end do

     else if (chem_cv_options == 20) then
      if (chemicda_opt == 1 ) then
        model_chemic(:,p_chem_ic_bc_a01)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a02)=model_rho(:,p_chem_ic_bc_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a03)=model_rho(:,p_chem_ic_bc_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_oc_a01)=model_rho(:,p_chem_ic_oc_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                                   
        model_chemic(:,p_chem_ic_oc_a02)=model_rho(:,p_chem_ic_oc_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc_a03)=model_rho(:,p_chem_ic_oc_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_so4_a01)=model_rho(:,p_chem_ic_so4_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                    
        model_chemic(:,p_chem_ic_so4_a02)=model_rho(:,p_chem_ic_so4_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_so4_a03)=model_rho(:,p_chem_ic_so4_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_no3_a01)=model_rho(:,p_chem_ic_no3_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                     
        model_chemic(:,p_chem_ic_no3_a02)=model_rho(:,p_chem_ic_no3_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_no3_a03)=model_rho(:,p_chem_ic_no3_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_nh4_a01)=model_rho(:,p_chem_ic_nh4_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                          
        model_chemic(:,p_chem_ic_nh4_a02)=model_rho(:,p_chem_ic_nh4_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_nh4_a03)=model_rho(:,p_chem_ic_nh4_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_cl_a01)=model_rho(:,p_chem_ic_cl_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                                   
        model_chemic(:,p_chem_ic_cl_a02)=model_rho(:,p_chem_ic_cl_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a03)=model_rho(:,p_chem_ic_cl_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_na_a01)=model_rho(:,p_chem_ic_na_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                                   
        model_chemic(:,p_chem_ic_na_a02)=model_rho(:,p_chem_ic_na_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a03)=model_rho(:,p_chem_ic_na_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_oin_a01)=model_rho(:,p_chem_ic_oin_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                         
        model_chemic(:,p_chem_ic_oin_a02)=model_rho(:,p_chem_ic_oin_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a03)=model_rho(:,p_chem_ic_oin_a03)*(model_chemic_surf(:,p_chemsi_pm25))

        do ichem = PARAM_FIRST_SCALAR, num_chem
          call da_interp_lin_2d_adj (jo_grad_x%chem_ic(:,:,1,ichem), iv%info(chemic_surf), 1, model_chemic(:,ichem))
        end do

      else if (chemicda_opt == 2 ) then
        model_chemic(:,p_chem_ic_bc_a01)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_bc_a02)=model_rho(:,p_chem_ic_bc_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_bc_a03)=model_rho(:,p_chem_ic_bc_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_bc_a04)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_oc_a01)=model_rho(:,p_chem_ic_oc_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                                   
        model_chemic(:,p_chem_ic_oc_a02)=model_rho(:,p_chem_ic_oc_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_oc_a03)=model_rho(:,p_chem_ic_oc_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_oc_a04)=model_rho(:,p_chem_ic_oc_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_so4_a01)=model_rho(:,p_chem_ic_so4_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                    
        model_chemic(:,p_chem_ic_so4_a02)=model_rho(:,p_chem_ic_so4_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_so4_a03)=model_rho(:,p_chem_ic_so4_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_so4_a04)=model_rho(:,p_chem_ic_so4_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_no3_a01)=model_rho(:,p_chem_ic_no3_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                     
        model_chemic(:,p_chem_ic_no3_a02)=model_rho(:,p_chem_ic_no3_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_no3_a03)=model_rho(:,p_chem_ic_no3_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_no3_a04)=model_rho(:,p_chem_ic_no3_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_nh4_a01)=model_rho(:,p_chem_ic_nh4_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                          
        model_chemic(:,p_chem_ic_nh4_a02)=model_rho(:,p_chem_ic_nh4_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_nh4_a03)=model_rho(:,p_chem_ic_nh4_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_nh4_a04)=model_rho(:,p_chem_ic_nh4_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_cl_a01)=model_rho(:,p_chem_ic_cl_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                                   
        model_chemic(:,p_chem_ic_cl_a02)=model_rho(:,p_chem_ic_cl_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_cl_a03)=model_rho(:,p_chem_ic_cl_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_cl_a04)=model_rho(:,p_chem_ic_cl_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_na_a01)=model_rho(:,p_chem_ic_na_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                                   
        model_chemic(:,p_chem_ic_na_a02)=model_rho(:,p_chem_ic_na_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_na_a03)=model_rho(:,p_chem_ic_na_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_na_a04)=model_rho(:,p_chem_ic_na_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        model_chemic(:,p_chem_ic_oin_a01)=model_rho(:,p_chem_ic_oin_a01)*(model_chemic_surf(:,p_chemsi_pm10))                                         
        model_chemic(:,p_chem_ic_oin_a02)=model_rho(:,p_chem_ic_oin_a02)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_oin_a03)=model_rho(:,p_chem_ic_oin_a03)*(model_chemic_surf(:,p_chemsi_pm10))
        model_chemic(:,p_chem_ic_oin_a04)=model_rho(:,p_chem_ic_oin_a04)*(model_chemic_surf(:,p_chemsi_pm10))

        do ichem = PARAM_FIRST_SCALAR, num_chem
          call da_interp_lin_2d_adj (jo_grad_x%chem_ic(:,:,1,ichem), iv%info(chemic_surf), 1, model_chemic(:,ichem))
        end do

      else if (chemicda_opt == 3 ) then
        model_chemic(:,p_chem_ic_bc_a01)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a02)=model_rho(:,p_chem_ic_bc_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a03)=model_rho(:,p_chem_ic_bc_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a04)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_bc_a04)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_oc_a01)=model_rho(:,p_chem_ic_oc_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                                   
        model_chemic(:,p_chem_ic_oc_a02)=model_rho(:,p_chem_ic_oc_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc_a03)=model_rho(:,p_chem_ic_oc_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc_a04)=model_rho(:,p_chem_ic_oc_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_oc_a04)=model_rho(:,p_chem_ic_oc_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_so4_a01)=model_rho(:,p_chem_ic_so4_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                    
        model_chemic(:,p_chem_ic_so4_a02)=model_rho(:,p_chem_ic_so4_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_so4_a03)=model_rho(:,p_chem_ic_so4_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_so4_a04)=model_rho(:,p_chem_ic_so4_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_so4_a04)=model_rho(:,p_chem_ic_so4_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_no3_a01)=model_rho(:,p_chem_ic_no3_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                     
        model_chemic(:,p_chem_ic_no3_a02)=model_rho(:,p_chem_ic_no3_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_no3_a03)=model_rho(:,p_chem_ic_no3_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_no3_a04)=model_rho(:,p_chem_ic_no3_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_no3_a04)=model_rho(:,p_chem_ic_no3_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_nh4_a01)=model_rho(:,p_chem_ic_nh4_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                          
        model_chemic(:,p_chem_ic_nh4_a02)=model_rho(:,p_chem_ic_nh4_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_nh4_a03)=model_rho(:,p_chem_ic_nh4_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_nh4_a04)=model_rho(:,p_chem_ic_nh4_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_nh4_a04)=model_rho(:,p_chem_ic_nh4_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_cl_a01)=model_rho(:,p_chem_ic_cl_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a02)=model_rho(:,p_chem_ic_cl_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a03)=model_rho(:,p_chem_ic_cl_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a04)=model_rho(:,p_chem_ic_cl_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_cl_a04)=model_rho(:,p_chem_ic_cl_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_na_a01)=model_rho(:,p_chem_ic_na_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a02)=model_rho(:,p_chem_ic_na_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a03)=model_rho(:,p_chem_ic_na_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a04)=model_rho(:,p_chem_ic_na_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_na_a04)=model_rho(:,p_chem_ic_na_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_oin_a01)=model_rho(:,p_chem_ic_oin_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a02)=model_rho(:,p_chem_ic_oin_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a03)=model_rho(:,p_chem_ic_oin_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a04)=model_rho(:,p_chem_ic_oin_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_oin_a04)=model_rho(:,p_chem_ic_oin_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        do ichem = PARAM_FIRST_SCALAR, num_chem
          call da_interp_lin_2d_adj (jo_grad_x%chem_ic(:,:,1,ichem), iv%info(chemic_surf), 1, model_chemic(:,ichem))
        end do

      else if (chemicda_opt == 4 ) then
        model_chemic(:,p_chem_ic_so2)=model_rho(:,p_chem_ic_so2)*(model_chemic_surf(:,p_chemsi_so2))*(64.06/28.964*1000)
        model_chemic(:,p_chem_ic_no2)=model_rho(:,p_chem_ic_no2)*(model_chemic_surf(:,p_chemsi_no2))*(46.01/28.964*1000)
        model_chemic(:,p_chem_ic_o3)=model_rho(:,p_chem_ic_o3)*(model_chemic_surf(:,p_chemsi_o3))*(48.00/28.964*1000)
        model_chemic(:,p_chem_ic_co)=model_rho(:,p_chem_ic_co)*(model_chemic_surf(:,p_chemsi_co))*(28.01/28.964*1000)

        do ichem = PARAM_FIRST_SCALAR, num_chem
          call da_interp_lin_2d_adj (jo_grad_x%chem_ic(:,:,1,ichem), iv%info(chemic_surf), 1, model_chemic(:,ichem))                                          
        end do

      else if (chemicda_opt == 5 ) then                                                                                                  
        model_chemic(:,p_chem_ic_bc_a01)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a02)=model_rho(:,p_chem_ic_bc_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a03)=model_rho(:,p_chem_ic_bc_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_bc_a04)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_bc_a04)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_oc_a01)=model_rho(:,p_chem_ic_oc_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                                   
        model_chemic(:,p_chem_ic_oc_a02)=model_rho(:,p_chem_ic_oc_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc_a03)=model_rho(:,p_chem_ic_oc_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oc_a04)=model_rho(:,p_chem_ic_oc_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_oc_a04)=model_rho(:,p_chem_ic_oc_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_so4_a01)=model_rho(:,p_chem_ic_so4_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                    
        model_chemic(:,p_chem_ic_so4_a02)=model_rho(:,p_chem_ic_so4_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_so4_a03)=model_rho(:,p_chem_ic_so4_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_so4_a04)=model_rho(:,p_chem_ic_so4_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_so4_a04)=model_rho(:,p_chem_ic_so4_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_no3_a01)=model_rho(:,p_chem_ic_no3_a01)*(model_chemic_surf(:,p_chemsi_pm25))                                     
        model_chemic(:,p_chem_ic_no3_a02)=model_rho(:,p_chem_ic_no3_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_no3_a03)=model_rho(:,p_chem_ic_no3_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_no3_a04)=model_rho(:,p_chem_ic_no3_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_no3_a04)=model_rho(:,p_chem_ic_no3_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_nh4_a01)=model_rho(:,p_chem_ic_nh4_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_nh4_a02)=model_rho(:,p_chem_ic_nh4_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_nh4_a03)=model_rho(:,p_chem_ic_nh4_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_nh4_a04)=model_rho(:,p_chem_ic_nh4_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_nh4_a04)=model_rho(:,p_chem_ic_nh4_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_cl_a01)=model_rho(:,p_chem_ic_cl_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a02)=model_rho(:,p_chem_ic_cl_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a03)=model_rho(:,p_chem_ic_cl_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_cl_a04)=model_rho(:,p_chem_ic_cl_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_cl_a04)=model_rho(:,p_chem_ic_cl_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_na_a01)=model_rho(:,p_chem_ic_na_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a02)=model_rho(:,p_chem_ic_na_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a03)=model_rho(:,p_chem_ic_na_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_na_a04)=model_rho(:,p_chem_ic_na_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_na_a04)=model_rho(:,p_chem_ic_na_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_oin_a01)=model_rho(:,p_chem_ic_oin_a01)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a02)=model_rho(:,p_chem_ic_oin_a02)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a03)=model_rho(:,p_chem_ic_oin_a03)*(model_chemic_surf(:,p_chemsi_pm25))
        model_chemic(:,p_chem_ic_oin_a04)=model_rho(:,p_chem_ic_oin_a04)*(model_chemic_surf(:,p_chemsi_pm10))
!        model_chemic(:,p_chem_ic_oin_a04)=model_rho(:,p_chem_ic_oin_a04)*(model_chemic_surf(:,p_chemsi_pm10)-model_chemic_surf(:,p_chemsi_pm25))

        model_chemic(:,p_chem_ic_so2)=model_rho(:,p_chem_ic_so2)*(model_chemic_surf(:,p_chemsi_so2))*(64.06/28.964*1000)
        model_chemic(:,p_chem_ic_no2)=model_rho(:,p_chem_ic_no2)*(model_chemic_surf(:,p_chemsi_no2))*(46.01/28.964*1000)
        model_chemic(:,p_chem_ic_o3)=model_rho(:,p_chem_ic_o3)*(model_chemic_surf(:,p_chemsi_o3))*(48.00/28.964*1000)
        model_chemic(:,p_chem_ic_co)=model_rho(:,p_chem_ic_co)*(model_chemic_surf(:,p_chemsi_co))*(28.01/28.964*1000) 

        do ichem = PARAM_FIRST_SCALAR, num_chem
          call da_interp_lin_2d_adj (jo_grad_x%chem_ic(:,:,1,ichem), iv%info(chemic_surf), 1, model_chemic(:,ichem))
        end do

      end if

     end if

      deallocate(model_chemic)
      deallocate(model_chemic_surf)
      deallocate(model_rho)
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_chem_sfc_adj")

end subroutine da_transform_xtoy_chem_sfc_adj


