!WRF:MODEL_LAYER:INITIALIZATION
!
   subroutine chem_init (id,chem,dt,bioemdt,photdt,chemdt,stepbioe,     &
     stepphot,stepchem,ph,phb,g,aerwrf,config_flags,                 &
           alt,t,pb,p,t0,p1000mb,rcp,                                &
         aer_dry,aer_water,chem_in_opt,                              &
               imicrogram,                                           &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

!  IMPLICIT NONE
   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_phot_mad
   USE module_aerosols_sorgam
   USE module_dep_simple
    real  , intent(in) :: bioemdt,photdt,chemdt,dt
   INTEGER,      INTENT(IN   ) :: chem_in_opt
   INTEGER,      INTENT(INOUT) :: imicrogram
   INTEGER,      INTENT(IN   ) :: id,                                &
                                  ids,ide, jds,jde, kds,kde,         &
                                  ims,ime, jms,jme, kms,kme,         &
                                  its,ite, jts,jte, kts,kte
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                     aer_dry,aer_water
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(IN   ) ::                                           &
                               ph,phb,t,pb,p,alt
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ) ,       &
          INTENT(INOUT   ) ::                                        &
                              chem 
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               aerwrf
   real, INTENT (IN) :: g,rcp,t0,p1000mb
    integer, intent(out) :: stepbioe,stepphot,stepchem
!
! local stuff
!
    integer :: i,j,k
   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
    stepbioe=nint(bioemdt*60./dt)
    stepphot=nint(photdt*60./dt)
    stepchem=nint(chemdt*60./dt)
    stepbioe=max(stepbioe,1)
    stepphot=max(stepphot,1)
    stepchem=max(stepchem,1)
    print *,'in chem_init'
    print *,'stepbioe,stepphot,stepchem',stepbioe,stepphot,stepchem
!
! we are dogin initialization here, first photolysis
!
   phot_select: SELECT CASE(config_flags%phot_opt)
     CASE (PHOTMAD)
     CALL wrf_debug(15,'call madronich phot initialization')
       call photmad_init(ph,phb,aerwrf,g,                            &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
     print *,'called madronich photolysis'
   END SELECT phot_select
!
! aerosols?
!
   aer_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2SORG)
     CALL wrf_debug(15,'call MADE/SORGAM aerosols initialization')
      call aerosols_sorgam_init(chem,                                &
               alt,t,pb,p,t0,p1000mb,rcp,                            &
               aer_dry,aer_water,chem_in_opt,                        &
               imicrogram,                                           &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
     CASE (RACMSORG)
     CALL wrf_debug(15,'call MADE/SORGAM aerosols initialization')
      call aerosols_sorgam_init(chem,                                &
               alt,t,pb,p,t0,p1000mb,rcp,                            &
               aer_dry,aer_water,chem_in_opt,                        &
               imicrogram,                                           &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
      do nv=1,num_chem
      do j=jms,jme
      do i=ims,ime
        chem(i,kde,j,nv)=chem(i,kde-1,j,nv)
      enddo
      enddo
      enddo

   END SELECT aer_select
   drydep_select: SELECT CASE(config_flags%drydep_opt)
     CASE (WESELY)
       CALL wrf_debug(15,'initializing dry dep (wesely)')
       call dep_init(id,config_flags)

   END SELECT drydep_select



    END SUBROUTINE chem_init
