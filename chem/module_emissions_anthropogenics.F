MODULE module_emissions_anthropogenics
!WRF:MODEL_LAYER:CHEMICS
!
CONTAINS
!
! currently this only adds in the emissions...
! this may be done differently for different chemical mechanisms
! in the future. aerosols are already added somewhere else....
!
   subroutine add_anthropogenics(id,dtstep,dz8w,config_flags,rho_phy,chem, &
               e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,        &
               e_ol2,e_olt,  &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,  &
               e_nh3,ids,ide, jds,jde, kds,kde,                   &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2
   IMPLICIT NONE
   INTEGER,      INTENT(IN   ) :: id,                             &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
      REAL,      INTENT(IN   ) ::                                          &
                             dtstep
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
!
! emissions arrays
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
         INTENT(IN ) ::                                                    &
           e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,&
           e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,e_nh3
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
         INTENT(IN ) ::        rho_phy

!
! 
!


   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                      dz8w
    integer i,j,k
    real :: conv_rho
!--- deposition and emissions stuff
! .. Parameters ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags


! ..
! ..
! .. Intrinsic Functions ..

         call wrf_debug(15,'add_anhropogenics')
!       
! add emissions
!
      do 100 j=jts,jte  
      do 100 i=its,ite  

      DO k=kts,kte-1
        conv_rho=4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)

        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                         &
                         +e_csl(i,k,j)*conv_rho
        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                         +e_iso(i,k,j)*conv_rho
        chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                         &
                         +e_so2(i,k,j)*conv_rho
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                         &
                         +e_no(i,k,j)*conv_rho
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +e_ald(i,k,j)*conv_rho
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                         &
                         +e_hcho(i,k,j)*conv_rho
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                         &
                         +e_ora2(i,k,j)*conv_rho
        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                         &
                         +e_nh3(i,k,j)*conv_rho
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +e_hc3(i,k,j)*conv_rho
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +e_hc5(i,k,j)*conv_rho
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +e_hc8(i,k,j)*conv_rho
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +e_eth(i,k,j)*conv_rho
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                         & 
                         +e_co(i,k,j)*conv_rho
        chem(i,k,j,p_ol2)  = chem(i,k,j,p_ol2)                         &
                         +e_ol2(i,k,j)*conv_rho
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +e_olt(i,k,j)*conv_rho
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +e_oli(i,k,j)*conv_rho
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +e_tol(i,k,j)*conv_rho
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &      
                         +e_xyl(i,k,j)*conv_rho
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                         &     
                         +e_ket(i,k,j)*conv_rho
      END DO                                                          
 100  continue

!     STOP
    END subroutine add_anthropogenics
!
!
         subroutine add_biogenics(id,dtstep,dz8w,config_flags,rho_phy,chem,&
               e_bio,ne_area,                                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
  USE module_configure
  USE module_state_description                                  
  USE module_data_radm2                               
  USE module_aerosols_sorgam 
   IMPLICIT NONE             
   INTEGER,      INTENT(IN   ) :: id,ne_area,                              &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
      REAL,      INTENT(IN   ) ::                                          &
                             dtstep
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, jms:jme,ne_area ),                            &
         INTENT(IN ) ::                                                    &
                 e_bio
   REAL, DIMENSION( ims:ime, jms:jme ),                                    &
         INTENT(IN ) ::                                                    &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no

         
!
! 
!
      
      
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                          rho_phy,dz8w            
    integer i,j,k,n
    real :: conv_rho
!--- deposition and emissions stuff
! .. Parameters ..       
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags     
!  return
   bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
     CALL wrf_debug(15,'adding biogenic emissions: Gunther1')
!     DO n = 1, numchem-2
      do 100 j=jts,jte  
      do 100 i=its,ite  
        conv_rho=dtstep/(dz8w(i,kts,j)*60.)
        chem(i,kts,j,p_iso)=chem(i,kts,j,p_iso)+ &
                  e_bio(i,j,p_iso-1)*conv_rho
        chem(i,kts,j,p_oli)=chem(i,kts,j,p_oli)+ &
                  e_bio(i,j,p_oli-1)*conv_rho
        chem(i,kts,j,p_xyl)=chem(i,kts,j,p_xyl)+ &
                  e_bio(i,j,p_xyl-1)*conv_rho
        chem(i,kts,j,p_hc3)=chem(i,kts,j,p_hc3)+ &
                  e_bio(i,j,p_hc3-1)*conv_rho
        chem(i,kts,j,p_olt)=chem(i,kts,j,p_olt)+ &
                  e_bio(i,j,p_olt-1)*conv_rho
        chem(i,kts,j,p_ket)=chem(i,kts,j,p_ket)+ &
                  e_bio(i,j,p_ket-1)*conv_rho
        chem(i,kts,j,p_ald)=chem(i,kts,j,p_ald)+ &
                  e_bio(i,j,p_ald-1)*conv_rho
        chem(i,kts,j,p_hcho)=chem(i,kts,j,p_hcho)+ &
                  e_bio(i,j,p_hcho-1)*conv_rho
        chem(i,kts,j,p_eth)=chem(i,kts,j,p_eth)+ &
                  e_bio(i,j,p_eth-1)*conv_rho
        chem(i,kts,j,p_ora2)=chem(i,kts,j,p_ora2)+ &
                  e_bio(i,j,p_ora2-1)*conv_rho
        chem(i,kts,j,p_co)=chem(i,kts,j,p_co)+ &
                  e_bio(i,j,p_co-1)*conv_rho
        chem(i,kts,j,p_no)=chem(i,kts,j,p_no)+ &
                  e_bio(i,j,p_no-1)*conv_rho
!
! RADM only
!
        if(p_ol2.gt.1)chem(i,kts,j,p_ol2)=chem(i,kts,j,p_ol2)+ &
                  e_bio(i,j,p_ol2-1)*conv_rho
 100  continue
!     enddo
     CASE (BEIS311)
     CALL wrf_debug(100,'adding biogenic emissions: beis3.1.1')
      do j=jts,jte  
      do i=its,ite  
        conv_rho=4.828e-4/rho_phy(i,kts,j)*dtstep/(dz8w(i,kts,j)*60.)
        chem(i,kts,j,p_iso)=chem(i,kts,j,p_iso)+ &
                  ebio_iso(i,j)*conv_rho
        chem(i,kts,j,p_oli)=chem(i,kts,j,p_oli)+ &
                  ebio_oli(i,j)*conv_rho
        chem(i,kts,j,p_xyl)=chem(i,kts,j,p_xyl)+ &
                  ebio_xyl(i,j)*conv_rho
        chem(i,kts,j,p_hc3)=chem(i,kts,j,p_hc3)+ &
                  ebio_hc3(i,j)*conv_rho
        chem(i,kts,j,p_olt)=chem(i,kts,j,p_olt)+ &
                  ebio_olt(i,j)*conv_rho
        chem(i,kts,j,p_ket)=chem(i,kts,j,p_ket)+ &
                  ebio_ket(i,j)*conv_rho
        chem(i,kts,j,p_ald)=chem(i,kts,j,p_ald)+ &
                  ebio_ald(i,j)*conv_rho
        chem(i,kts,j,p_hcho)=chem(i,kts,j,p_hcho)+ &
                  ebio_hcho(i,j)*conv_rho
        chem(i,kts,j,p_eth)=chem(i,kts,j,p_eth)+ &
                  ebio_eth(i,j)*conv_rho
        chem(i,kts,j,p_ora2)=chem(i,kts,j,p_ora2)+ &
                  ebio_ora2(i,j)*conv_rho
        chem(i,kts,j,p_co)=chem(i,kts,j,p_co)+ &
                  ebio_co(i,j)*conv_rho
        chem(i,kts,j,p_no)=chem(i,kts,j,p_no)+ &
                  ebio_no(i,j)*conv_rho
!
! RADM only
!
        if(p_ol2.gt.1)chem(i,kts,j,p_ol2)=chem(i,kts,j,p_ol2)+ &
                  ebio_ete(i,j)*conv_rho
!
! RACM only
!
        if(p_api.gt.1)chem(i,kts,j,p_api)=chem(i,kts,j,p_api)+ &
                  ebio_api(i,j)*conv_rho
        if(p_lim.gt.1)chem(i,kts,j,p_lim)=chem(i,kts,j,p_lim)+ &
                  ebio_lim(i,j)*conv_rho
        if(p_ete.gt.1)chem(i,kts,j,p_ete)=chem(i,kts,j,p_ete)+ &
                  ebio_ete(i,j)*conv_rho
      enddo
      enddo
     CASE DEFAULT

   END SELECT bioem_select
    END subroutine add_biogenics


END MODULE module_emissions_anthropogenics
