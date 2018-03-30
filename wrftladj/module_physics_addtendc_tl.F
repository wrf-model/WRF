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


MODULE g_module_physics_addtendc

#if ( EM_CORE == 1 )

   USE module_state_description
   USE module_configure

CONTAINS

SUBROUTINE g_update_phy_ten(rph_tendf,rt_tendf,g_rt_tendf,ru_tendf,g_ru_tendf,         &
                      rv_tendf,g_rv_tendf,moist_tendf,g_moist_tendf,        &
                      scalar_tendf,mu_tendf,                                &
                      RTHRATEN,RTHBLTEN,g_RTHBLTEN,RTHCUTEN,g_RTHCUTEN,RTHSHTEN,       &
                      RUBLTEN,g_RUBLTEN,RUCUTEN,RUSHTEN,                    &
                      RVBLTEN,g_RVBLTEN,RVCUTEN,RVSHTEN,                    &
                      RQVBLTEN,g_RQVBLTEN,RQCBLTEN,RQIBLTEN,                &
                      RQVCUTEN,g_RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,         &
                      RQVSHTEN,RQCSHTEN,RQRSHTEN,RQISHTEN,RQSSHTEN,RQGSHTEN,&
                      RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RPHNDGDTEN,            &
                      RQVNDGDTEN,RMUNDGDTEN,                                &
                      rthfrten,rqvfrten,                                    & !fire
                      n_moist,n_scalar,config_flags,rk_step,adv_moist_cond, &
                      ids, ide, jds, jde, kds, kde,                         &
                      ims, ime, jms, jme, kms, kme,                         &
                      its, ite, jts, jte, kts, kte                          )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte,   &
                                   n_moist,n_scalar,rk_step

   LOGICAL , INTENT(IN)        :: adv_moist_cond

   REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) ::   &
                                                         ru_tendf, &
                                                       g_ru_tendf, &
                                                         rv_tendf, &
                                                       g_rv_tendf, &
                                                         rt_tendf, &
                                                       g_rt_tendf, &
                                                         rph_tendf

   REAL , DIMENSION(ims:ime , jms:jme),INTENT(INOUT) ::  mu_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),           &
          INTENT(INOUT)     ::                        moist_tendf, &
                                                    g_moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),           &
          INTENT(INOUT)     ::                        scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                       RTHRATEN, &
                                                       RTHBLTEN, &
                                                     g_RTHBLTEN, &
                                                       RTHCUTEN, &
                                                     g_RTHCUTEN, &
                                                       RTHSHTEN, &
                                                        RUBLTEN, &
                                                      g_RUBLTEN, &
                                                        RUCUTEN, &
                                                        RUSHTEN, &
                                                        RVBLTEN, &
                                                      g_RVBLTEN, &
                                                        RVCUTEN, &
                                                        RVSHTEN, &
                                                       RQVBLTEN, &
                                                     g_RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN, &
                                                       RQVCUTEN, &
                                                     g_RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN, &
                                                       RQVSHTEN, &
                                                       RQCSHTEN, &
                                                       RQRSHTEN, &
                                                       RQISHTEN, &
                                                       RQSSHTEN, &
                                                       RQGSHTEN, &
                                                     RTHNDGDTEN, &
                                                     RPHNDGDTEN, &
                                                     RQVNDGDTEN, &
                                                      RUNDGDTEN, &
                                                      RVNDGDTEN

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN  ) :: RMUNDGDTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   & ! fire
                                                       rthfrten, &
                                                       rqvfrten   
!------------------------------------------------------------------
!  set up loop bounds for this grid's boundary conditions

   if (config_flags%ra_lw_physics .gt. 0 .or.                  &
       config_flags%ra_sw_physics .gt. 0)                      &
      CALL phy_ra_ten(config_flags,rt_tendf,RTHRATEN,          &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%bl_pbl_physics .gt. 0)                     &
      CALL g_phy_bl_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,g_rt_tendf,ru_tendf,g_ru_tendf,   &
                      rv_tendf,g_rv_tendf,                     &
                      moist_tendf, g_moist_tendf,              &
                      scalar_tendf,adv_moist_cond,             &
                      RTHBLTEN,g_RTHBLTEN,                     &
                      RUBLTEN,g_RUBLTEN,                       &
                      RVBLTEN,g_RVBLTEN,                       &
                      RQVBLTEN,g_RQVBLTEN,                     &
                      RQCBLTEN,RQIBLTEN,                       &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%cu_physics .gt. 0)                         &
      CALL g_phy_cu_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,g_rt_tendf,ru_tendf,rv_tendf,   &
                      RUCUTEN,RVCUTEN,RTHCUTEN,g_RTHCUTEN,     &
                      RQVCUTEN,g_RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,moist_tendf,g_moist_tendf,&
                      scalar_tendf,adv_moist_cond,             &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%shcu_physics .gt. 0)                       &
      CALL phy_shcu_ten(config_flags,rk_step,n_moist,          &
                      rt_tendf,ru_tendf,rv_tendf,              &
                      RUSHTEN,RVSHTEN,RTHSHTEN,                &
                      RQVSHTEN,RQCSHTEN,RQRSHTEN,              &
                      RQISHTEN,RQSSHTEN,RQGSHTEN,moist_tendf,  &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%grid_fdda .gt. 0)                          &
      CALL phy_fg_ten(config_flags,rk_step,n_moist,            &
                      rph_tendf,rt_tendf,ru_tendf,rv_tendf,    &
                      mu_tendf, moist_tendf,                   &
                      RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,          &
                      RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,        &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%ifire .gt. 0)                              & ! fire
      CALL phy_fr_ten(config_flags,rk_step,n_moist,            &
                      rt_tendf,ru_tendf,rv_tendf,              &
                      mu_tendf, moist_tendf,                   &
                      rthfrten,rqvfrten,                       &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

END SUBROUTINE g_update_phy_ten

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
SUBROUTINE g_phy_bl_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,g_rt_tendf,ru_tendf,g_ru_tendf,   &
                      rv_tendf,g_rv_tendf,moist_tendf,g_moist_tendf, &
                      scalar_tendf,adv_moist_cond,               &
                      RTHBLTEN,g_RTHBLTEN,                       &
                      RUBLTEN,g_RUBLTEN,                         &
                      RVBLTEN,g_RVBLTEN,                         & 
                      RQVBLTEN,g_RQVBLTEN,                       &
                      RQCBLTEN,RQIBLTEN,                         &
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
                                   n_moist, n_scalar, rk_step

   LOGICAL , INTENT(IN)     :: adv_moist_cond

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),         &
          INTENT(INOUT)     ::                      moist_tendf, &
                                                  g_moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),         &
          INTENT(INOUT)     ::                      scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   & 
                                                       RTHBLTEN, &
                                                     g_RTHBLTEN, &
                                                        RUBLTEN, &
                                                      g_RUBLTEN, &
                                                        RVBLTEN, &
                                                      g_RVBLTEN, &
                                                       RQVBLTEN, &
                                                     g_RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::   &
                                                       rt_tendf, &
                                                     g_rt_tendf, &
                                                       ru_tendf, &
                                                     g_ru_tendf, &
                                                       rv_tendf, &
                                                     g_rv_tendf
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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
       ENDIF

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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
       ENDIF

      CASE (ACMPBLSCHEME)

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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)THEN
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
        ENDIF
     
       ENDIF

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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
!       if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
!          CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQSBLTEN,  &
!               config_flags,                                    &
!               ids,ide, jds, jde, kds, kde,                     &
!               ims, ime, jms, jme, kms, kme,                    &
!               its, ite, jts, jte, kts, kte                     )
!    
!       if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
!          CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQRBLTEN,  &
!               config_flags,                                    &
!               ids,ide, jds, jde, kds, kde,                     &
!               ims, ime, jms, jme, kms, kme,                    &
!               its, ite, jts, jte, kts, kte                     )
!    
!       if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
!          CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQGBLTEN,  &
!               config_flags,                                    &
!               ids,ide, jds, jde, kds, kde,                     &
!               ims, ime, jms, jme, kms, kme,                    &
!               its, ite, jts, jte, kts, kte                     )
     
       ELSE

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
     
!       if (P_QS .ge. PARAM_FIRST_SCALAR)                                         &
!          CALL add_a2a(moist_tendf(ims,kms,jms,P_QS),RQSBLTEN,  &
!               config_flags,                                    &
!               ids,ide, jds, jde, kds, kde,                     &
!               ims, ime, jms, jme, kms, kme,                    &
!               its, ite, jts, jte, kts, kte                     )
!    
!       if (P_QR .ge. PARAM_FIRST_SCALAR)                                         &
!          CALL add_a2a(moist_tendf(ims,kms,jms,P_QR),RQRBLTEN,  &
!               config_flags,                                    &
!               ids,ide, jds, jde, kds, kde,                     &
!               ims, ime, jms, jme, kms, kme,                    &
!               its, ite, jts, jte, kts, kte                     )
!    
!       if (P_QG .ge. PARAM_FIRST_SCALAR)                                         &
!          CALL add_a2a(moist_tendf(ims,kms,jms,P_QG),RQGBLTEN,  &
!               config_flags,                                    &
!               ids,ide, jds, jde, kds, kde,                     &
!               ims, ime, jms, jme, kms, kme,                    &
!               its, ite, jts, jte, kts, kte                     )
     
       ENDIF

      CASE (QNSEPBLSCHEME)

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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
       ELSE

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF

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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
       ENDIF

      CASE (MYNNPBLSCHEME2,MYNNPBLSCHEME3)

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
     
       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
     
       ENDIF

       CASE (BOULACSCHEME)

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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ELSE

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
       ENDIF

      CASE (SURFDRAGSCHEME)

           CALL g_add_a2a(rt_tendf,g_rt_tendf,                   &
                RTHBLTEN,g_RTHBLTEN,config_flags,                &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL g_add_a2c_u(ru_tendf,g_ru_tendf,                 &
                RUBLTEN,g_RUBLTEN,config_flags,                  &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL g_add_a2c_v(rv_tendf,g_rv_tendf,                 &
                RVBLTEN,g_RVBLTEN,config_flags,                  &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
              CALL g_add_a2a(moist_tendf(ims,kms,jms,P_QV),      &
                   g_moist_tendf(ims,kms,jms,P_QV),              &
                   RQVBLTEN, g_RQVBLTEN,                         &
                   config_flags,                                 &
                   ids,ide, jds, jde, kds, kde,                  &
                   ims, ime, jms, jme, kms, kme,                 &
                   its, ite, jts, jte, kts, kte                  )

       CASE (CAMUWPBLSCHEME)
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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ELSE

        if (P_QC .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
       ENDIF


      CASE (TEMFPBLSCHEME)
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

       print*,'g_phy_bl_ten: The pbl scheme does not exist'

   END SELECT

END SUBROUTINE g_phy_bl_ten

!=================================================================
SUBROUTINE g_phy_cu_ten(config_flags,rk_step,n_moist,n_scalar,     &
                      rt_tendf,g_rt_tendf,ru_tendf,rv_tendf,     &
                      RUCUTEN,RVCUTEN,RTHCUTEN,g_RTHCUTEN,       &
                      RQVCUTEN,g_RQVCUTEN,RQCCUTEN,RQRCUTEN,     &
                      RQICUTEN,RQSCUTEN,moist_tendf,g_moist_tendf,&
                      scalar_tendf,adv_moist_cond,               &
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
                                   n_moist, n_scalar, rk_step

   LOGICAL , INTENT(IN)     :: adv_moist_cond

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),         &
          INTENT(INOUT)     ::                      moist_tendf, g_moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),         &
          INTENT(INOUT)     ::                      scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                        RUCUTEN, &
                                                        RVCUTEN, &
                                                       RTHCUTEN, &
                                                     g_RTHCUTEN, &
                                                       RQVCUTEN, &
                                                     g_RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::  &
                                                       rt_tendf, &
                                                     g_rt_tendf, &
                                                       ru_tendf, &
                                                       rv_tendf


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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQRCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQICUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQSCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF

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

   CASE (KFETASCHEME, MSKFSCHEME)
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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQRCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQICUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQSCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF
   CASE (GDSCHEME, G3SCHEME)
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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQICUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF

   CASE (NSASSCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2c_u(ru_tendf,RUCUTEN,config_flags,            &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2c_v(rv_tendf,RVCUTEN,config_flags,            &
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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQICUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF

   CASE (SASSCHEME,OSASSCHEME)
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

   CASE (DUCUSCHEME)
        CALL g_add_a2a(rt_tendf, g_rt_tendf,                     &
                RTHCUTEN, g_RTHCUTEN, config_flags,              &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL g_add_a2a(moist_tendf(ims,kms,jms,P_QV),            &
                g_moist_tendf(ims,kms,jms,P_QV), RQVCUTEN,       &
                g_RQVCUTEN,                                      &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE (CAMZMSCHEME)
        CALL add_a2c_u(ru_tendf,RUCUTEN,config_flags,            &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2c_v(rv_tendf,RVCUTEN,config_flags,            &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
                                                                                                                                        
        if (P_QV .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
          
        if (P_QC .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCCUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
          
        if (P_QI .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQICUTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQICUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF

   CASE (TIEDTKESCHEME, NTIEDTKESCHEME)
        CALL add_a2a(rt_tendf,RTHCUTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2c_u(ru_tendf,RUCUTEN,config_flags,            &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2c_v(rv_tendf,RVCUTEN,config_flags,            &
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

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCCUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQICUTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       ENDIF

   CASE DEFAULT

   END SELECT

END SUBROUTINE g_phy_cu_ten

!=================================================================
SUBROUTINE phy_shcu_ten(config_flags,rk_step,n_moist,            &
                      rt_tendf,ru_tendf,rv_tendf,                &
                      RUSHTEN,RVSHTEN,RTHSHTEN,                  &
                      RQVSHTEN,RQCSHTEN,RQRSHTEN,                &
                      RQISHTEN,RQSSHTEN,RQGSHTEN,moist_tendf,    &
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
                                                        RUSHTEN, &
                                                        RVSHTEN, &
                                                       RTHSHTEN, &
                                                       RQVSHTEN, &
                                                       RQCSHTEN, &
                                                       RQRSHTEN, &
                                                       RQISHTEN, &
                                                       RQSSHTEN, &
                                                       RQGSHTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::  &
                                                       rt_tendf, &
                                                       ru_tendf, &
                                                       rv_tendf

! LOCAL VARS

   INTEGER :: i,j,k

   SELECT CASE (config_flags%shcu_physics)

   CASE (CAMUWSHCUSCHEME)
        CALL add_a2c_u(ru_tendf,RUSHTEN,config_flags,            &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2c_v(rv_tendf,RVSHTEN,config_flags,            &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        CALL add_a2a(rt_tendf,RTHSHTEN,config_flags,             &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVSHTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCSHTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QR .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QR),RQRSHTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QI .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQISHTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QS .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QS),RQSSHTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QG .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QG),RQGSHTEN,     &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE DEFAULT

   END SELECT


END SUBROUTINE phy_shcu_ten

!=================================================================
SUBROUTINE phy_fg_ten(config_flags,rk_step,n_moist,            &
                      rph_tendf,rt_tendf,ru_tendf,rv_tendf,    &
                      mu_tendf, moist_tendf,                   &
                      RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,          &
                      RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,        &
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
                                                       RTHNDGDTEN, &
                                                       RPHNDGDTEN, &
                                                        RUNDGDTEN, &
                                                        RVNDGDTEN, &
                                                       RQVNDGDTEN

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN  ) ::  RMUNDGDTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::   &
                                                       rph_tendf,&
                                                       rt_tendf, &
                                                       ru_tendf, &
                                                       rv_tendf

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)::  mu_tendf

! LOCAL VARS

   INTEGER :: i,j,k,IBGN,IEND,JBGN,JEND

!-----------------------------------------------------------------

   SELECT CASE(config_flags%grid_fdda)

      CASE (PSUFDDAGD)

           CALL add_a2a(rt_tendf,RTHNDGDTEN,config_flags,        &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

! note fdda u and v tendencies are staggered
           CALL add_c2c_u(ru_tendf,RUNDGDTEN,config_flags,       &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_c2c_v(rv_tendf,RVNDGDTEN,config_flags,       &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2a(mu_tendf,RMUNDGDTEN,config_flags,      &
                ids,ide, jds, jde, kds, kds,                     &
                ims, ime, jms, jme, kms, kms,                    &
                its, ite, jts, jte, kts, kts                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVNDGDTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE (SPNUDGING)

! note fdda u and v tendencies are staggered
           CALL add_c2c_u(ru_tendf,RUNDGDTEN,config_flags,       &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_c2c_v(rv_tendf,RVNDGDTEN,config_flags,       &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2a(rt_tendf,RTHNDGDTEN,config_flags,        &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2a_ph(rph_tendf,RPHNDGDTEN,config_flags,        &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE DEFAULT

   END SELECT

END SUBROUTINE phy_fg_ten

!=================================================================
SUBROUTINE phy_fr_ten(config_flags,rk_step,n_moist,            &
                      rt_tendf,ru_tendf,rv_tendf,              &
                      mu_tendf, moist_tendf,                   &
                      rthfrten,rqvfrten,                       &
                      ids, ide, jds, jde, kds, kde,              &
                      ims, ime, jms, jme, kms, kme,              &
                      its, ite, jts, jte, kts, kte               )
!-----------------------------------------------------------------
   USE module_state_description, ONLY :                         &
                   FIRE_SFIRE
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
                                                       rthfrten, &
                                                       rqvfrten 

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::   &
                                                       rt_tendf, &
                                                       ru_tendf, &
                                                       rv_tendf

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)::  mu_tendf

! LOCAL VARS

   INTEGER :: i,j,k,IBGN,IEND,JBGN,JEND

!-----------------------------------------------------------------

   SELECT CASE(config_flags%ifire)

      CASE (FIRE_SFIRE)

           CALL add_a2a(rt_tendf,rthfrten,                       &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),rqvfrten,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE DEFAULT

   END SELECT

END SUBROUTINE phy_fr_ten

!--------------------------------------------------- -------------------

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of advance_ppt in forward (tangent) mode:
!   variations   of useful results: rqccuten rthcuten rqicuten
!                rqscuten rainc cuppt nca rqrcuten rqvcuten rainsh
!                htop hbot
!   with respect to varying inputs: pratesh rqccuten rthcuten rqicuten
!                rqscuten rainc pratec cuppt cutop cubot nca rqrcuten
!                rqvcuten rainsh htop hbot
!   RW status of diff variables: pratesh:in rqccuten:in-out rthcuten:in-out
!                rqicuten:in-out rqscuten:in-out rainc:in-out pratec:in
!                cuppt:in-out cutop:in cubot:in nca:in-out rqrcuten:in-out
!                rqvcuten:in-out rainsh:in-out htop:in-out hbot:in-out
SUBROUTINE G_ADVANCE_PPT(rthcuten, rthcutend, rqvcuten, rqvcutend, &
&  rqccuten, rqccutend, rqrcuten, rqrcutend, rqicuten, rqicutend, &
&  rqscuten, rqscutend, rainc, raincd, raincv, rainsh, rainshd, pratec, &
&  pratecd, pratesh, prateshd, nca, ncad, htop, htopd, hbot, hbotd, cutop&
&  , cutopd, cubot, cubotd, cuppt, cupptd, dt, config_flags, ids, ide, &
&  jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
&  kts, kte)
  IMPLICIT NONE
!
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rthcuten&
&  , rqvcuten, rqccuten, rqrcuten, rqicuten, rqscuten
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rthcutend&
&  , rqvcutend, rqccutend, rqrcutend, rqicutend, rqscutend
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: rainc, rainsh, &
&  raincv, pratec, pratesh, nca, htop, hbot, cutop, cubot, cuppt
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: raincd, rainshd, &
&  pratecd, prateshd, ncad, htopd, hbotd, cutopd, cubotd, cupptd
  REAL, INTENT(IN) :: dt
! LOCAL  VAR
  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: ncutop, ncubot
!-----------------------------------------------------------------
  IF (config_flags%cu_physics .EQ. 0) THEN
    RETURN
  ELSE
! SET START AND END POINTS FOR TILES
    i_start = its
    IF (ite .GT. ide - 1) THEN
      i_end = ide - 1
    ELSE
      i_end = ite
    END IF
    j_start = jts
    IF (jte .GT. jde - 1) THEN
      j_end = jde - 1
    ELSE
      j_end = jte
    END IF
!
!  IF( config_flags%nested .or. config_flags%specified ) THEN
!    i_start = max( its,ids+1 )
!    i_end   = min( ite,ide-2 )
!    j_start = max( jts,jds+1 )
!    j_end   = min( jte,jde-2 )
!  ENDIF
!
    k_start = kts
    IF (kte .GT. kde - 1) THEN
      k_end = kde - 1
    ELSE
      k_end = kte
    END IF
! Update total cumulus scheme precipitation
! in mm  
    DO j=j_start,j_end
      DO i=i_start,i_end
        raincd(i, j) = raincd(i, j) + dt*pratecd(i, j)
        rainc(i, j) = rainc(i, j) + pratec(i, j)*dt
        rainshd(i, j) = rainshd(i, j) + dt*prateshd(i, j)
        rainsh(i, j) = rainsh(i, j) + pratesh(i, j)*dt
        cupptd(i, j) = cupptd(i, j) + dt*(pratecd(i, j)+prateshd(i, j))/&
&          1000.
        cuppt(i, j) = cuppt(i, j) + (pratec(i, j)+pratesh(i, j))*dt/&
&          1000.
      END DO
    END DO
    SELECT CASE  (config_flags%cu_physics) 
    CASE (kfscheme) 
      DO j=j_start,j_end
        DO i=i_start,i_end
          IF (nca(i, j) .GT. 0) THEN
            IF (NINT(nca(i, j)/dt) .LE. 0) THEN
! set tendency to zero
!                PRATEC(I,J)=0.
!                RAINCV(I,J)=0.
              DO k=k_start,k_end
                rthcutend(i, k, j) = 0.0
                rthcuten(i, k, j) = 0.
                rqvcutend(i, k, j) = 0.0
                rqvcuten(i, k, j) = 0.
                rqccutend(i, k, j) = 0.0
                rqccuten(i, k, j) = 0.
                rqrcutend(i, k, j) = 0.0
                rqrcuten(i, k, j) = 0.
                IF (p_qi .GE. param_first_scalar) THEN
                  rqicutend(i, k, j) = 0.0
                  rqicuten(i, k, j) = 0.
                END IF
                IF (p_qs .GE. param_first_scalar) THEN
                  rqscutend(i, k, j) = 0.0
                  rqscuten(i, k, j) = 0.
                END IF
              END DO
            END IF
! Decrease NCA
            nca(i, j) = nca(i, j) - dt
          END IF
        END DO
      END DO
    CASE (ducuscheme) 
!
      DO j=j_start,j_end
        DO i=i_start,i_end
          IF (nca(i, j) .GT. 0) THEN
            IF (NINT(nca(i, j)/dt) .LE. 0) THEN
! set tendency to zero
!!                PRATEC(I,J)=0.
!!                RAINCV(I,J)=0.
              DO k=k_start,k_end
                rthcutend(i, k, j) = 0.0
                rthcuten(i, k, j) = 0.
                rqvcutend(i, k, j) = 0.0
                rqvcuten(i, k, j) = 0.
              END DO
            END IF
! Decrease NCA
            nca(i, j) = nca(i, j) - dt
          END IF
        END DO
      END DO
    CASE (bmjscheme, camzmscheme) 
!
      DO j=j_start,j_end
        DO i=i_start,i_end
! HTOP, HBOT FOR GFDL RADIATION
          ncutop = NINT(cutop(i, j))
          ncubot = NINT(cubot(i, j))
          IF (ncutop .GT. 1 .AND. ncutop .LT. kde) THEN
            IF (cutop(i, j) .LT. htop(i, j)) THEN
              htop(i, j) = htop(i, j)
            ELSE
              htopd(i, j) = cutopd(i, j)
              htop(i, j) = cutop(i, j)
            END IF
          END IF
          IF (ncubot .GT. 0 .AND. ncubot .LT. kde) THEN
            IF (cubot(i, j) .GT. hbot(i, j)) THEN
              hbot(i, j) = hbot(i, j)
            ELSE
              hbotd(i, j) = cubotd(i, j)
              hbot(i, j) = cubot(i, j)
            END IF
          END IF
        END DO
      END DO
    CASE (kfetascheme, MSKFSCHEME) 
      DO j=j_start,j_end
        DO i=i_start,i_end
! HTOP, HBOT FOR GFDL RADIATION
          ncutop = NINT(cutop(i, j))
          ncubot = NINT(cubot(i, j))
          IF (ncutop .GT. 1 .AND. ncutop .LT. kde) THEN
            IF (cutop(i, j) .LT. htop(i, j)) THEN
              htop(i, j) = htop(i, j)
            ELSE
              htopd(i, j) = cutopd(i, j)
              htop(i, j) = cutop(i, j)
            END IF
          END IF
          IF (ncubot .GT. 0 .AND. ncubot .LT. kde) THEN
            IF (cubot(i, j) .GT. hbot(i, j)) THEN
              hbot(i, j) = hbot(i, j)
            ELSE
              hbotd(i, j) = cubotd(i, j)
              hbot(i, j) = cubot(i, j)
            END IF
          END IF
          IF (nca(i, j) .GT. 0) THEN
            IF (NINT(nca(i, j)/dt) .LE. 1) THEN
! set tendency to zero
!                PRATEC(I,J)=0.
!                RAINCV(I,J)=0.
              DO k=k_start,k_end
                rthcutend(i, k, j) = 0.0
                rthcuten(i, k, j) = 0.
                rqvcutend(i, k, j) = 0.0
                rqvcuten(i, k, j) = 0.
                rqccutend(i, k, j) = 0.0
                rqccuten(i, k, j) = 0.
                rqrcutend(i, k, j) = 0.0
                rqrcuten(i, k, j) = 0.
                IF (p_qi .GE. param_first_scalar) THEN
                  rqicutend(i, k, j) = 0.0
                  rqicuten(i, k, j) = 0.
                END IF
                IF (p_qs .GE. param_first_scalar) THEN
                  rqscutend(i, k, j) = 0.0
                  rqscuten(i, k, j) = 0.
                END IF
              END DO
            END IF
! Decrease NCA
            nca(i, j) = nca(i, j) - dt
!              NCA(I,J)=NCA(I,J)-1. ! Decrease NCA
          END IF
        END DO
      END DO
    END SELECT
  END IF
END SUBROUTINE G_ADVANCE_PPT

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
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

   DO j = j_start,j_end
   DO k = kts,ktf
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + rvar(i,k,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_a2a

SUBROUTINE add_a2a_ph(lvar,rvar,config_flags,                  &
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
   INTEGER :: i,j,k,i_start,i_end,j_start,j_end

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

   DO j = j_start,j_end
   DO k = kts,kte
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + rvar(i,k,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_a2a_ph

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
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

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
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

   DO j = j_start,j_end
   DO k = kts,kte
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + &
                     0.5*(rvar(i,k,j)+rvar(i,k,j-1))
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_a2c_v


!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of add_a2a in forward (tangent) mode:
!   variations   of useful results: lvar
!   with respect to varying inputs: lvar rvar
!   RW status of diff variables: lvar:in-out rvar:in
SUBROUTINE G_ADD_A2A(lvar, lvard, rvar, rvard, config_flags, ids, ide, &
&  jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
&  kts, kte)
  IMPLICIT NONE
!------------------------------------------------------------
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvard
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvard
! LOCAL VARS
  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf
  INTRINSIC MAX
  INTRINSIC MIN
  i_start = its
  IF (ite .GT. ide - 1) THEN
    i_end = ide - 1
  ELSE
    i_end = ite
  END IF
  j_start = jts
  IF (jte .GT. jde - 1) THEN
    j_end = jde - 1
  ELSE
    j_end = jte
  END IF
  IF (kte .GT. kde - 1) THEN
    ktf = kde - 1
  ELSE
    ktf = kte
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (ids + 1 .LT. its) THEN
      i_start = its
    ELSE
      i_start = ids + 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (ide - 2 .GT. ite) THEN
      i_end = ite
    ELSE
      i_end = ide - 2
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (jds + 1 .LT. jts) THEN
      j_start = jts
    ELSE
      j_start = jds + 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (jde - 2 .GT. jte) THEN
      j_end = jte
    ELSE
      j_end = jde - 2
    END IF
  END IF
  IF (config_flags%periodic_x) i_start = its
  IF (config_flags%periodic_x) THEN
    IF (ite .GT. ide - 1) THEN
      i_end = ide - 1
    ELSE
      i_end = ite
    END IF
  END IF
  DO j=j_start,j_end
    DO k=kts,ktf
      DO i=i_start,i_end
        lvard(i, k, j) = lvard(i, k, j) + rvard(i, k, j)
        lvar(i, k, j) = lvar(i, k, j) + rvar(i, k, j)
      END DO
    END DO
  END DO
END SUBROUTINE G_ADD_A2A

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of add_a2c_u in forward (tangent) mode:
!   variations   of useful results: lvar
!   with respect to varying inputs: lvar rvar
!   RW status of diff variables: lvar:in-out rvar:in
SUBROUTINE G_ADD_A2C_U(lvar, lvard, rvar, rvard, config_flags, ids, ide&
&  , jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte&
&  , kts, kte)
  IMPLICIT NONE
!------------------------------------------------------------
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvard
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvard
! LOCAL VARS
  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf
  INTRINSIC MAX
  INTRINSIC MIN
  IF (kte .GT. kde - 1) THEN
    ktf = kde - 1
  ELSE
    ktf = kte
  END IF
  i_start = its
  i_end = ite
  j_start = jts
  IF (jte .GT. jde - 1) THEN
    j_end = jde - 1
  ELSE
    j_end = jte
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (ids + 1 .LT. its) THEN
      i_start = its
    ELSE
      i_start = ids + 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (ide - 1 .GT. ite) THEN
      i_end = ite
    ELSE
      i_end = ide - 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (jds + 1 .LT. jts) THEN
      j_start = jts
    ELSE
      j_start = jds + 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (jde - 2 .GT. jte) THEN
      j_end = jte
    ELSE
      j_end = jde - 2
    END IF
  END IF
  IF (config_flags%periodic_x) i_start = its
  IF (config_flags%periodic_x) i_end = ite
  DO j=j_start,j_end
    DO k=kts,ktf
      DO i=i_start,i_end
        lvard(i, k, j) = lvard(i, k, j) + 0.5*(rvard(i, k, j)+rvard(i-1&
&          , k, j))
        lvar(i, k, j) = lvar(i, k, j) + 0.5*(rvar(i, k, j)+rvar(i-1, k, &
&          j))
      END DO
    END DO
  END DO
END SUBROUTINE G_ADD_A2C_U

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of add_a2c_v in forward (tangent) mode:
!   variations   of useful results: lvar
!   with respect to varying inputs: lvar rvar
!   RW status of diff variables: lvar:in-out rvar:in
SUBROUTINE G_ADD_A2C_V(lvar, lvard, rvar, rvard, config_flags, ids, ide&
&  , jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte&
&  , kts, kte)
  IMPLICIT NONE
!------------------------------------------------------------
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvard
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvard
! LOCAL VARS
  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, ktf
  INTRINSIC MAX
  INTRINSIC MIN
  IF (kte .GT. kde - 1) THEN
    ktf = kde - 1
  ELSE
    ktf = kte
  END IF
  i_start = its
  IF (ite .GT. ide - 1) THEN
    i_end = ide - 1
  ELSE
    i_end = ite
  END IF
  j_start = jts
  j_end = jte
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (ids + 1 .LT. its) THEN
      i_start = its
    ELSE
      i_start = ids + 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (ide - 2 .GT. ite) THEN
      i_end = ite
    ELSE
      i_end = ide - 2
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (jds + 1 .LT. jts) THEN
      j_start = jts
    ELSE
      j_start = jds + 1
    END IF
  END IF
  IF (config_flags%specified .OR. config_flags%nested) THEN
    IF (jde - 1 .GT. jte) THEN
      j_end = jte
    ELSE
      j_end = jde - 1
    END IF
  END IF
  IF (config_flags%periodic_x) i_start = its
  IF (config_flags%periodic_x) THEN
    IF (ite .GT. ide - 1) THEN
      i_end = ide - 1
    ELSE
      i_end = ite
    END IF
  END IF
  DO j=j_start,j_end
    DO k=kts,kte
      DO i=i_start,i_end
        lvard(i, k, j) = lvard(i, k, j) + 0.5*(rvard(i, k, j)+rvard(i, k&
&          , j-1))
        lvar(i, k, j) = lvar(i, k, j) + 0.5*(rvar(i, k, j)+rvar(i, k, j-&
&          1))
      END DO
    END DO
  END DO
END SUBROUTINE G_ADD_A2C_V

!------------------------------------------------------------
SUBROUTINE add_c2c_u(lvar,rvar,config_flags,                &
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

!  write(*,'(a,6i4)') 'call c2cu, i_start, i_end, j_start, j_end=', i_start, i_end, j_start, j_end

   DO j = j_start,j_end
   DO k = kts,ktf
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + rvar(i,k,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_c2c_u

SUBROUTINE add_c2c_v(lvar,rvar,config_flags,                &
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

!  write(*,'(a,6i4)') 'call c2cv, i_start, i_end, j_start, j_end=', i_start, i_end, j_start, j_end

   DO j = j_start,j_end
   DO k = kts,kte
   DO i = i_start,i_end
      lvar(i,k,j) = lvar(i,k,j) + rvar(i,k,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE add_c2c_v

#endif

END MODULE g_module_physics_addtendc
