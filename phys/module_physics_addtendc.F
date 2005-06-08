!WRF:MODEL_LAYER: PHYSICS
!
! note: this module really belongs in the dyn_em directory since it is 
!       specific only to the EM core. Leaving here for now, with an 
!       #if ( EM_CORE == 1 ) directive. JM 20031201
!

!  This MODULE holds the routines which are used to perform updates of the
!  model C-grid tendencies with physics A-grid tendencies
!  The module consolidates code that was (up to v1.2) duplicated in 
!  module_em and module_rk and in
!  module_big_step_utilities.F and module_big_step_utilities_em.F

!  This MODULE CONTAINS the following routines:
!  update_phy_ten, phy_ra_ten, phy_bl_ten, phy_cu_ten, advance_ppt,
!  add_a2a, add_a2c_u, and add_a2c_v


MODULE module_physics_addtendc

#if ( EM_CORE == 1 )

   USE module_state_description
   USE module_configure

CONTAINS

SUBROUTINE update_phy_ten(rt_tendf,ru_tendf,rv_tendf,moist_tendf,  &
                      RTHRATEN,RTHBLTEN,RTHCUTEN,RUBLTEN,RVBLTEN,  &
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,                  &
                      RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,&
                      n_moist,config_flags,rk_step,                &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte,   &
                                   n_moist,rk_step

   REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) ::   &
                                                         ru_tendf, &
                                                         rv_tendf, &
                                                         rt_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),           &
          INTENT(INOUT)     ::                        moist_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                       RTHRATEN, &
                                                       RTHBLTEN, &
                                                       RTHCUTEN, &
                                                        RUBLTEN, &
                                                        RVBLTEN, &
                                                       RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN, &
                                                       RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN
!------------------------------------------------------------------

!  set up loop bounds for this grid's boundary conditions

   if (config_flags%ra_lw_physics .gt. 0 .or.                  &
       config_flags%ra_sw_physics .gt. 0)                      &
      CALL phy_ra_ten(config_flags,rt_tendf,RTHRATEN,          &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%bl_pbl_physics .gt. 0)                     &
      CALL phy_bl_ten(config_flags,rk_step,n_moist,           &
                      rt_tendf,ru_tendf,rv_tendf,moist_tendf,  &
                      RTHBLTEN,RUBLTEN,RVBLTEN,                &
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,              &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%cu_physics .gt. 0)                         &
      CALL phy_cu_ten(config_flags,rk_step,n_moist,rt_tendf,  &
                      RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,     &
                      RQICUTEN,RQSCUTEN,moist_tendf,           &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

END SUBROUTINE update_phy_ten

!=================================================================
SUBROUTINE phy_ra_ten(config_flags,rt_tendf,RTHRATEN,            &
                      ids, ide, jds, jde, kds, kde,              &
                      ims, ime, jms, jme, kms, kme,              &
                      its, ite, jts, jte, kts, kte               )
!-----------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE(grid_config_rec_type  ) , INTENT(IN   ) :: config_flags

   INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                       RTHRATEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::  &
                                                       rt_tendf

! LOCAL VARS

   INTEGER :: i,j,k

   CALL add_a2a(rt_tendf,RTHRATEN,config_flags,                  &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

END SUBROUTINE phy_ra_ten

!=================================================================
SUBROUTINE phy_bl_ten(config_flags,rk_step,n_moist,             &
                      rt_tendf,ru_tendf,rv_tendf,moist_tendf,    &
                      RTHBLTEN,RUBLTEN,RVBLTEN,                  & 
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,                &
                      ids, ide, jds, jde, kds, kde,              &
                      ims, ime, jms, jme, kms, kme,              &
                      its, ite, jts, jte, kts, kte               )
!-----------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE(grid_config_rec_type) ,     INTENT(IN   ) :: config_flags

   INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte, &
                                   n_moist, rk_step

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),         &
          INTENT(INOUT)     ::                      moist_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   & 
                                                       RTHBLTEN, &
                                                        RUBLTEN, &
                                                        RVBLTEN, &
                                                       RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::   &
                                                       rt_tendf, &
                                                       ru_tendf, &
                                                       rv_tendf
! LOCAL VARS

   INTEGER :: i,j,k,IBGN,IEND,JBGN,JEND

!-----------------------------------------------------------------

   SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)

           CALL add_a2a(rt_tendf,RTHBLTEN,config_flags,          &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_u(ru_tendf,RUBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_v(rv_tendf,RVBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE (MRFSCHEME)

           CALL add_a2a(rt_tendf,RTHBLTEN,config_flags,          &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_u(ru_tendf,RUBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_v(rv_tendf,RVBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE (MYJPBLSCHEME)

           CALL add_a2a(rt_tendf,RTHBLTEN,config_flags,          &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_u(ru_tendf,RUBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2c_v(rv_tendf,RVBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE (GFSSCHEME)
                                                                                                                                        
           CALL add_a2a(rt_tendf,RTHBLTEN,config_flags,          &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
           CALL add_a2c_u(ru_tendf,RUBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
           CALL add_a2c_v(rv_tendf,RVBLTEN,config_flags,         &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
      CASE DEFAULT

       print*,'phy_bl_ten: The pbl scheme does not exist'

   END SELECT

END SUBROUTINE phy_bl_ten

!=================================================================
SUBROUTINE phy_cu_ten(config_flags,rk_step,n_moist,rt_tendf,    &
                      RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,       &
                      RQICUTEN,RQSCUTEN,moist_tendf,             &
                      ids, ide, jds, jde, kds, kde,              &
                      ims, ime, jms, jme, kms, kme,              &
                      its, ite, jts, jte, kts, kte               )
!-----------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE(grid_config_rec_type  ) , INTENT(IN   ) :: config_flags

   INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte, &
                                   n_moist, rk_step

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),         &
          INTENT(INOUT)     ::                      moist_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                       RTHCUTEN, &
                                                       RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::  &
                                                       rt_tendf

! LOCAL VARS

   INTEGER :: i,j,k

   SELECT CASE (config_flags%cu_physics)   

   CASE (KFSCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QR .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QR),RQRCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQICUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QS .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QS),RQSCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE (BMJSCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,                          &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE (KFETASCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QR .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QR),RQRCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQICUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QS .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QS),RQSCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE (GDSCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQICUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE (SASSCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
          
        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
          
        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQICUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE DEFAULT

   END SELECT

END SUBROUTINE phy_cu_ten

!----------------------------------------------------------------------
SUBROUTINE advance_ppt(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,           &
                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,              &
                     CUPPT, config_flags,                             &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     its,ite, jts,jte, kts,kte                        )     
!----------------------------------------------------------------------
   USE module_state_description
   USE module_cu_kf
   USE module_cu_kfeta
!----------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                             RTHCUTEN, &
                                                       RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC, &
                                                         RAINCV, &
                                                            NCA, &
                                                          CUPPT

! LOCAL  VAR

   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,k_start,k_end

!-----------------------------------------------------------------

   IF (config_flags%cu_physics .eq. 0) return

! SET START AND END POINTS FOR TILES

   i_start = its
   i_end   = min( ite,ide-1 )
   j_start = jts
   j_end   = min( jte,jde-1 )
!
!  IF( config_flags%nested .or. config_flags%specified ) THEN
!    i_start = max( its,ids+1 )
!    i_end   = min( ite,ide-2 )
!    j_start = max( jts,jds+1 )
!    j_end   = min( jte,jde-2 )
!  ENDIF
!
   k_start = kts
   k_end = min( kte, kde-1 )

! Update total cumulus scheme precipitation

! in mm  

   DO J = j_start,j_end
   DO i = i_start,i_end
      RAINC(I,J)=RAINC(I,J)+RAINCV(I,J)
      CUPPT(I,J)=CUPPT(I,J)+RAINCV(I,J)/1000.
   ENDDO
   ENDDO

   SELECT CASE (config_flags%cu_physics)

   CASE (KFSCHEME)

        DO J = j_start,j_end
        DO i = i_start,i_end

           IF ( NINT(NCA(I,J)).GT. 0 ) THEN

              IF ( NINT(NCA(I,J)) .eq. 1 ) THEN

              ! set tendency to zero
                 RAINCV(I,J)=0.
                 DO k = k_start,k_end
                    RTHCUTEN(i,k,j)=0.
                    RQVCUTEN(i,k,j)=0.
                    RQCCUTEN(i,k,j)=0.
                    RQRCUTEN(i,k,j)=0.
                    if (P_QI .ge. PARAM_FIRST_SCALAR) RQICUTEN(i,k,j)=0.
                    if (P_QS .ge. PARAM_FIRST_SCALAR) RQSCUTEN(i,k,j)=0.
                 ENDDO
              ENDIF

              NCA(I,J)=NCA(I,J)-1. ! Decrease NCA

           ENDIF
!
        ENDDO
        ENDDO

   CASE (KFETASCHEME)

        DO J = j_start,j_end
        DO i = i_start,i_end

           IF ( NINT(NCA(I,J)).GT. 0 ) THEN

              IF ( NINT(NCA(I,J)) .eq. 1 ) THEN

              ! set tendency to zero
                 RAINCV(I,J)=0.
                 DO k = k_start,k_end
                    RTHCUTEN(i,k,j)=0.
                    RQVCUTEN(i,k,j)=0.
                    RQCCUTEN(i,k,j)=0.
                    RQRCUTEN(i,k,j)=0.
                    if (P_QI .ge. PARAM_FIRST_SCALAR) RQICUTEN(i,k,j)=0.
                    if (P_QS .ge. PARAM_FIRST_SCALAR) RQSCUTEN(i,k,j)=0.
                 ENDDO
              ENDIF

              NCA(I,J)=NCA(I,J)-1. ! Decrease NCA

           ENDIF
!
        ENDDO
        ENDDO

   CASE DEFAULT

   END SELECT

END SUBROUTINE advance_ppt

SUBROUTINE add_a2a(lvar,rvar,config_flags,                  &
                   ids,ide, jds, jde, kds, kde,             &
                   ims, ime, jms, jme, kms, kme,            &
                   its, ite, jts, jte, kts, kte             )
!------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------
   TYPE(grid_config_rec_type),  INTENT(IN) :: config_flags

   INTEGER , INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(IN   ) ::&
                                                      rvar
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(INOUT) ::&
                                                      lvar

! LOCAL VARS
   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,ktf

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)
   ktf = min(kte,kde-1)

   IF ( config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)

   DO j = j_start,j_end
   DO k = kts,ktf
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + rvar(i,k,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_a2a

!------------------------------------------------------------
SUBROUTINE add_a2c_u(lvar,rvar,config_flags,                &
                   ids,ide, jds, jde, kds, kde,             &
                   ims, ime, jms, jme, kms, kme,            &
                   its, ite, jts, jte, kts, kte             )
!------------------------------------------------------------
!------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER , INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(IN   ) ::&
                                                      rvar
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(INOUT) ::&
                                                      lvar

! LOCAL VARS

   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,ktf

   ktf=min(kte,kde-1)

   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-1,ite)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)

   DO j = j_start,j_end
   DO k = kts,ktf
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + &
                       0.5*(rvar(i,k,j)+rvar(i-1,k,j))
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_a2c_u

!------------------------------------------------------------
SUBROUTINE add_a2c_v(lvar,rvar,config_flags,                &
                   ids,ide, jds, jde, kds, kde,             &
                   ims, ime, jms, jme, kms, kme,            &
                   its, ite, jts, jte, kts, kte             )
!------------------------------------------------------------
!------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER , INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(IN   ) ::&
                                                      rvar
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(INOUT) ::&
                                                      lvar

! LOCAL VARS

   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,ktf

   ktf=min(kte,kde-1)

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = jte

   IF ( config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-1,jte)

   DO j = j_start,j_end
   DO k = kts,kte
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + &
                     0.5*(rvar(i,k,j)+rvar(i,k,j-1))
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_a2c_v

! end of ifdef for EM_CORE == 1 -- this module actually belongs in the dyn_em directory
#endif

END MODULE module_physics_addtendc
