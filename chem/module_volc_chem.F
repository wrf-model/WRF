! 17 Oct. 2024. A. Ukhov (KAUST). Oxidation of SO2 into Sulf using OH and H2O2
! based on module_gocart_chem.
! 13 Dec. 2025. A. Ukhov (KAUST). Contribution of volcanic Sulf and ash into PM2.5 and PM10

MODULE MODULE_VOLC_CHEM

   CONTAINS
   
   ! Compute pm2_5 and pm10 from volcanic ash and sulfate
   subroutine sum_pm_volc (                                      &
      alt, chem,pm2_5_dry, pm10,                                 &
      ids,ide, jds,jde, kds,kde,                                 &
      ims,ime, jms,jme, kms,kme,                                 &
      its,ite, jts,jte, kts,kte                                  )
   USE module_configure
   USE module_state_description
   USE module_data_gocartchem, only: nh4_mfac
   USE module_model_constants, only: mwdry
   IMPLICIT NONE
   
   REAL, PARAMETER :: mwso4 = 96.0576
   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,     &
                            ims,ime, jms,jme, kms,kme,           &
                            its,ite, jts,jte, kts,kte
   
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT ) :: pm2_5_dry, pm10
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: alt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), INTENT(IN ) :: chem
   
   real sulfate
   integer i,j,k,ii,jj
   
      pm2_5_dry(its:ite, kts:kte, jts:jte) = 0.
      pm10(its:ite, kts:kte, jts:jte)    = 0.
   
      do j=jts,jte                    
      jj=min(jde-1,j)              
      do k=kts,kte
      do i=its,ite
         ii=min(ide-1,i)
         sulfate = chem(ii,k,jj,p_sulf) * (mwso4/mwdry) * 1.e3  !ppmv -> (ug/kg)
         
         !vash10:[0.0-3.906] diam. (um) => log(2.5-0)/log(3.906-0)=0.672
         pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + sulfate * nh4_mfac  &
                           + 0.672 * chem(ii,k,jj,p_vash_10)
   
         ! vash8:[7.812-15.625] diam. (um) => (log(10)-log(7.812))/(log(15.625)-log(7.812))=0.356
         pm10(i,k,j) = pm10(i,k,j) + sulfate * nh4_mfac        &
                     + chem(ii,k,jj,p_vash_10) + chem(ii,k,jj,p_vash_9)    &
                     + 0.356 * chem(ii,k,jj,p_vash_8)
   
         !Convert from mixing ratio (ug/kg) to concentration (ug m^-3)      
         pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) / alt(ii,k,jj)
         pm10(i,k,j) = pm10(i,k,j)/ alt(ii,k,jj)
      enddo
      enddo
      enddo
   
   end subroutine sum_pm_volc
   
   END MODULE MODULE_volc_CHEM
   