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

   integer :: n,ichem,ipm        ! Loop counters

   real, allocatable :: model_chemic_surf(:,:)
   real, allocatable :: model_chemic(:,:)
   real, allocatable :: model_rho(:,:)

!  Raidus of influence of ground observations at urban sites (Table 3. in Elbernet al. 2007)
!  FIXME: It would be better to put this parameter in the namelist.
   real, parameter  :: Lrepr = 3000. !2000.    ! Scaling factor for obs error [meters]

!  Max thresholds of abs(o-b) in pm25,  pm10,  so2,  no2,  o3,  co
   real,dimension(6):: maxomb = (/100., 100., 0.20, 0.20, 0.20, 20./)
   real             :: err_o, err_r

   if (trace_use) call da_trace_entry("da_get_innov_vector_chem_sfc")

   if ( it > 1 ) then
      do n=iv%info(chemic_surf)%n1,iv%info(chemic_surf)%n2
         do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
            if (iv%chemic_surf(n)%chem(ichem)%qc == fails_error_max) iv%chemic_surf(n)%chem(ichem)%qc = 0
         end do
      end do
   end if

   ! [0.0]  Get chem surf conc.

   allocate (model_chemic_surf(iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chemic_surf))
   allocate (model_chemic     (iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chem))
   allocate (model_rho        (iv%info(chemic_surf)%n1:iv%info(chemic_surf)%n2,num_chem))

   ! [1.0] Get horizontal interpolation weights:
   do ichem = PARAM_FIRST_SCALAR ,num_chem
      call da_interp_lin_2d (grid%xbchem%chem_ic(:,:,1,ichem),  iv%info(chemic_surf), 1, model_chemic(:,ichem))
      call da_interp_lin_2d (grid%xb    %rho    (:,:,1),        iv%info(chemic_surf), 1, model_rho   (:,ichem))
   end do

   ! [1.1] Compute prior observations at obs sites
   if (chem_cv_options == 10) then           ! gocart_test
       model_chemic_surf(:,p_chemsi_pm25)=model_rho(:,p_chem_ic_p25)*(model_chemic(:,p_chem_ic_p25)                                       + &
           1.375*(96.06/28.964*1000)*model_chemic(:,p_chem_ic_sulf)                                                                       + &
	   model_chemic(:,p_chem_ic_bc1)+model_chemic(:,p_chem_ic_bc2)+1.8*(model_chemic(:,p_chem_ic_oc1)+model_chemic(:,p_chem_ic_oc2))  + &
           model_chemic(:,p_chem_ic_dust_1)+0.286*model_chemic(:,p_chem_ic_dust_2)+model_chemic(:,p_chem_ic_seas_1)                       + &
           0.942*model_chemic(:,p_chem_ic_seas_2))

   else if (chem_cv_options == 20) then      ! mosaic_test

      if (chemicda_opt == 1 ) then
      model_chemic_surf(:,p_chemsi_pm25)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic(:,p_chem_ic_bc_a01)+model_chemic(:,p_chem_ic_bc_a02)        + &
          model_chemic(:,p_chem_ic_bc_a03)+model_chemic(:,p_chem_ic_oc_a01)+model_chemic(:,p_chem_ic_oc_a02)+model_chemic(:,p_chem_ic_oc_a03)    + &
          model_chemic(:,p_chem_ic_so4_a01)+model_chemic(:,p_chem_ic_so4_a02)+model_chemic(:,p_chem_ic_so4_a03)+model_chemic(:,p_chem_ic_no3_a01)+ &
          model_chemic(:,p_chem_ic_no3_a02)+model_chemic(:,p_chem_ic_no3_a03)+model_chemic(:,p_chem_ic_nh4_a01)+model_chemic(:,p_chem_ic_nh4_a02)+ &
          model_chemic(:,p_chem_ic_nh4_a03)+model_chemic(:,p_chem_ic_cl_a01)+model_chemic(:,p_chem_ic_cl_a02)+model_chemic(:,p_chem_ic_cl_a03)   + &
          model_chemic(:,p_chem_ic_na_a01)+model_chemic(:,p_chem_ic_na_a02)+model_chemic(:,p_chem_ic_na_a03)+model_chemic(:,p_chem_ic_oin_a01)   + &
          model_chemic(:,p_chem_ic_oin_a02)+model_chemic(:,p_chem_ic_oin_a03))
      else if (chemicda_opt == 2 ) then
      model_chemic_surf(:,p_chemsi_pm10)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic(:,p_chem_ic_bc_a01)+model_chemic(:,p_chem_ic_bc_a02)        + &
          model_chemic(:,p_chem_ic_bc_a03)+model_chemic(:,p_chem_ic_bc_a04)+model_chemic(:,p_chem_ic_oc_a01)+model_chemic(:,p_chem_ic_oc_a02)    + &
          model_chemic(:,p_chem_ic_oc_a03)+model_chemic(:,p_chem_ic_oc_a04)+model_chemic(:,p_chem_ic_so4_a01)+model_chemic(:,p_chem_ic_so4_a02)  + &
          model_chemic(:,p_chem_ic_so4_a03)+model_chemic(:,p_chem_ic_so4_a04)+model_chemic(:,p_chem_ic_no3_a01)+model_chemic(:,p_chem_ic_no3_a02)+ &
          model_chemic(:,p_chem_ic_no3_a03)+model_chemic(:,p_chem_ic_no3_a04)+model_chemic(:,p_chem_ic_nh4_a01)+model_chemic(:,p_chem_ic_nh4_a02)+ &
          model_chemic(:,p_chem_ic_nh4_a03)+model_chemic(:,p_chem_ic_nh4_a04)+model_chemic(:,p_chem_ic_cl_a01)+model_chemic(:,p_chem_ic_cl_a02)  + &
          model_chemic(:,p_chem_ic_cl_a03)+model_chemic(:,p_chem_ic_cl_a04)+model_chemic(:,p_chem_ic_na_a01)+model_chemic(:,p_chem_ic_na_a02)    + &
          model_chemic(:,p_chem_ic_na_a03)+model_chemic(:,p_chem_ic_na_a04)+model_chemic(:,p_chem_ic_oin_a01)+model_chemic(:,p_chem_ic_oin_a02)  + &
          model_chemic(:,p_chem_ic_oin_a03)+model_chemic(:,p_chem_ic_oin_a04))
      else if (chemicda_opt == 3 ) then                                                                                   
          model_chemic_surf(:,p_chemsi_pm25)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic(:,p_chem_ic_bc_a01)+model_chemic(:,p_chem_ic_bc_a02)    + &
          model_chemic(:,p_chem_ic_bc_a03)+model_chemic(:,p_chem_ic_oc_a01)+model_chemic(:,p_chem_ic_oc_a02)+model_chemic(:,p_chem_ic_oc_a03)    + &
          model_chemic(:,p_chem_ic_so4_a01)+model_chemic(:,p_chem_ic_so4_a02)+model_chemic(:,p_chem_ic_so4_a03)+model_chemic(:,p_chem_ic_no3_a01)+ &
          model_chemic(:,p_chem_ic_no3_a02)+model_chemic(:,p_chem_ic_no3_a03)+model_chemic(:,p_chem_ic_nh4_a01)+model_chemic(:,p_chem_ic_nh4_a02)+ &
          model_chemic(:,p_chem_ic_nh4_a03)+model_chemic(:,p_chem_ic_cl_a01)+model_chemic(:,p_chem_ic_cl_a02)+model_chemic(:,p_chem_ic_cl_a03)   + &
          model_chemic(:,p_chem_ic_na_a01)+model_chemic(:,p_chem_ic_na_a02)+model_chemic(:,p_chem_ic_na_a03)+model_chemic(:,p_chem_ic_oin_a01)   + &
          model_chemic(:,p_chem_ic_oin_a02)+model_chemic(:,p_chem_ic_oin_a03))
          model_chemic_surf(:,p_chemsi_pm10)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic(:,p_chem_ic_bc_a04)+model_chemic(:,p_chem_ic_oc_a04)    + &
          model_chemic(:,p_chem_ic_so4_a04)+model_chemic(:,p_chem_ic_no3_a04)+model_chemic(:,p_chem_ic_nh4_a04)+model_chemic(:,p_chem_ic_cl_a04) + &
          model_chemic(:,p_chem_ic_na_a04)+model_chemic(:,p_chem_ic_oin_a04))
      else if (chemicda_opt == 4 ) then
          model_chemic_surf(:,p_chemsi_so2)=model_rho(:,p_chem_ic_so2)*(64.06/28.964*1000)*(model_chemic(:,p_chem_ic_so2))
          model_chemic_surf(:,p_chemsi_no2)=model_rho(:,p_chem_ic_no2)*(46.01/28.964*1000)*(model_chemic(:,p_chem_ic_no2))
          model_chemic_surf(:,p_chemsi_o3)=model_rho(:,p_chem_ic_o3)*(48.00/28.964*1000)*(model_chemic(:,p_chem_ic_o3))
          model_chemic_surf(:,p_chemsi_co)=model_rho(:,p_chem_ic_co)*(28.01/28.964*1000)*(model_chemic(:,p_chem_ic_co))

      else if (chemicda_opt == 5 ) then                                            
          model_chemic_surf(:,p_chemsi_pm25)=model_rho(:,p_chem_ic_bc_a01)*(model_chemic(:,p_chem_ic_bc_a01)+model_chemic(:,p_chem_ic_bc_a02)    + &
          model_chemic(:,p_chem_ic_bc_a03)+model_chemic(:,p_chem_ic_oc_a01)+model_chemic(:,p_chem_ic_oc_a02)+model_chemic(:,p_chem_ic_oc_a03)    + &
          model_chemic(:,p_chem_ic_so4_a01)+model_chemic(:,p_chem_ic_so4_a02)+model_chemic(:,p_chem_ic_so4_a03)+model_chemic(:,p_chem_ic_no3_a01)+ &
          model_chemic(:,p_chem_ic_no3_a02)+model_chemic(:,p_chem_ic_no3_a03)+model_chemic(:,p_chem_ic_nh4_a01)+model_chemic(:,p_chem_ic_nh4_a02)+ &
          model_chemic(:,p_chem_ic_nh4_a03)+model_chemic(:,p_chem_ic_cl_a01)+model_chemic(:,p_chem_ic_cl_a02)+model_chemic(:,p_chem_ic_cl_a03)   + &
          model_chemic(:,p_chem_ic_na_a01)+model_chemic(:,p_chem_ic_na_a02)+model_chemic(:,p_chem_ic_na_a03)+model_chemic(:,p_chem_ic_oin_a01)   + &
          model_chemic(:,p_chem_ic_oin_a02)+model_chemic(:,p_chem_ic_oin_a03)) 
          model_chemic_surf(:,p_chemsi_pm10)=model_rho(:,p_chem_ic_bc_a04)*(model_chemic(:,p_chem_ic_bc_a04)+model_chemic(:,p_chem_ic_oc_a04)    + &
          model_chemic(:,p_chem_ic_so4_a04)+model_chemic(:,p_chem_ic_no3_a04)+model_chemic(:,p_chem_ic_nh4_a04)+model_chemic(:,p_chem_ic_cl_a04) + &
          model_chemic(:,p_chem_ic_na_a04)+model_chemic(:,p_chem_ic_oin_a04))

          model_chemic_surf(:,p_chemsi_so2)=model_rho(:,p_chem_ic_so2)*(64.06/28.964*1000)*(model_chemic(:,p_chem_ic_so2))
          model_chemic_surf(:,p_chemsi_no2)=model_rho(:,p_chem_ic_no2)*(46.01/28.964*1000)*(model_chemic(:,p_chem_ic_no2))
          model_chemic_surf(:,p_chemsi_o3)=model_rho(:,p_chem_ic_o3)*(48.00/28.964*1000)*(model_chemic(:,p_chem_ic_o3))
          model_chemic_surf(:,p_chemsi_co)=model_rho(:,p_chem_ic_co)*(28.01/28.964*1000)*(model_chemic(:,p_chem_ic_co))

      end if

   else if (chem_cv_options == 108) then   ! racm_soa_vbs_da

      if (chemicda_opt == 1 .or. chemicda_opt == 3 .or. chemicda_opt == 5) then   ! pm2.5

         model_chemic_surf(:,p_chemsi_pm25) = 0.0
         do ichem = p_chem_ic_so4aj, p_chem_ic_p25i
            model_chemic_surf(:,p_chemsi_pm25) = model_chemic_surf(:,p_chemsi_pm25) + &
                                                 model_rho(:,ichem)  * model_chemic(:,ichem)
         end do

      end if

      if (chemicda_opt == 2) then    ! pm10

         model_chemic_surf(:,p_chemsi_pm10) = 0.0
         do ichem = p_chem_ic_so4aj, p_chem_ic_soila
            model_chemic_surf(:,p_chemsi_pm10) = model_chemic_surf(:,p_chemsi_pm10) + &
                                                 model_rho(:,ichem) * model_chemic(:,ichem)
         end do

      end if !(chemicda_opt == 2) then

      if (chemicda_opt == 3 .or. chemicda_opt == 5) then   ! pm10 after pm2.5

         ! pm10 - pm2.5 residuals
         model_chemic_surf(:,p_chemsi_pm10) = 0.0
         do ichem = p_chem_ic_antha, p_chem_ic_soila
            model_chemic_surf(:,p_chemsi_pm10) = model_chemic_surf(:,p_chemsi_pm10) + &
                                                     model_rho(:,ichem) * model_chemic(:,ichem)
         end do

      end if !(chemicda_opt == 3 .or. chemicda_opt == 5)

      if (chemicda_opt >= 4) then

          model_chemic_surf(:,p_chemsi_so2)=model_chemic(:,p_chem_ic_so2)
          model_chemic_surf(:,p_chemsi_no2)=model_chemic(:,p_chem_ic_no2)
          model_chemic_surf(:,p_chemsi_o3) =model_chemic(:,p_chem_ic_o3 )
          model_chemic_surf(:,p_chemsi_co) =model_chemic(:,p_chem_ic_co )

      end if !(chemicda_opt >= 4 ) then

   end if   !  (chem_cv_options >= 108)


   ! Innovations and obs errors for chemic_surf (e.g., surfce concentration)

   do ichem = PARAM_FIRST_SCALAR, num_chemic_surf

      ipm = ichem - PARAM_FIRST_SCALAR + 1
 
      do n=iv%info(chemic_surf)%n1,iv%info(chemic_surf)%n2

         ! [2.0] Initialise components of innovation vector: 
         iv % chemic_surf(n) % chem(ichem) % inv   = missing_r
         iv % chemic_surf(n) % chem(ichem) % error = missing_r

         ! [3.0] To compute the innovation (inv = o-f):
         if (ob % chemic_surf(n) % chem(ichem) > missing_r .and. &
             iv % chemic_surf(n) % chem(ichem) % qc >=  obs_qc_pointer) then

             iv % chemic_surf(n) % chem(ichem) % inv = &
             ob % chemic_surf(n) % chem(ichem) - model_chemic_surf(n,ichem)

             ! Observation error specification
             err_o = 1.5 + 0.0075 * ob % chemic_surf(n) % chem(ichem)
             err_r = 0.5 * err_o * sqrt(grid%dx/Lrepr)

             iv % chemic_surf(n) % chem(ichem) % error = 1.0*sqrt(err_o**2 + err_r**2) ! Ha (GMD2022)

            ! Quality Control
            ! ----------------
            if (abs(iv % chemic_surf(n) % chem(ichem) % inv) .ge. maxomb(ipm)) then
                iv % chemic_surf(n) % chem(ichem) % inv   = missing_r
                iv % chemic_surf(n) % chem(ichem) % qc    = missing
                iv % chemic_surf(n) % chem(ichem) % error = missing_r
            end if

         end if    ! good obs

      end do
   end do          ! do ichem = PARAM_FIRST_SCALAR, num_chemic_surf

   deallocate(model_rho)
   deallocate(model_chemic)
   deallocate(model_chemic_surf)
 
   ! -----------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_chem_sfc(iv,ob, it, num_qcstat_conv)

   if (trace_use) call da_trace_exit("da_get_innov_vector_chem_sfc")

end subroutine da_get_innov_vector_chem_sfc


