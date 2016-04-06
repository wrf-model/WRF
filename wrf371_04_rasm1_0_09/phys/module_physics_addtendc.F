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

SUBROUTINE update_phy_ten(rph_tendf,rt_tendf,ru_tendf,rv_tendf,moist_tendf, &
                      scalar_tendf,mu_tendf,                                &
                      RTHRATEN,RTHBLTEN,RTHCUTEN,RTHSHTEN,                  &
                      RUBLTEN,RUCUTEN,RUSHTEN,                              &
                      RVBLTEN,RVCUTEN,RVSHTEN,                              &
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,RQNIBLTEN,&!RQNCBLTEN,       & !JOE
                      RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,         &
                      RQCNCUTEN,RQINCUTEN,                                  &
                      RQVSHTEN,RQCSHTEN,RQRSHTEN,RQISHTEN,RQSSHTEN,RQGSHTEN,&
                      RQCNSHTEN,RQINSHTEN,                                  &
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
                                                         rv_tendf, &
                                                         rt_tendf, &
                                                         rph_tendf

   REAL , DIMENSION(ims:ime , jms:jme),INTENT(INOUT) ::  mu_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),           &
          INTENT(INOUT)     ::                        moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),           &
          INTENT(INOUT)     ::                        scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                       RTHRATEN, &
                                                       RTHBLTEN, &
                                                       RTHCUTEN, &
                                                       RTHSHTEN, &
                                                        RUBLTEN, &
                                                        RUCUTEN, &
                                                        RUSHTEN, &
                                                        RVBLTEN, &
                                                        RVCUTEN, &
                                                        RVSHTEN, &
                                                       RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN, &
                                                      RQNIBLTEN, & !For CAMUWPBL
                                                       RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN, &
                                                      RQCNCUTEN, &
                                                      RQINCUTEN, &
                                                       RQVSHTEN, &
                                                       RQCSHTEN, &
                                                       RQRSHTEN, &
                                                       RQISHTEN, &
                                                       RQSSHTEN, &
                                                       RQGSHTEN, &
                                                      RQCNSHTEN, &
                                                      RQINSHTEN, &
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
      CALL phy_bl_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,ru_tendf,rv_tendf,moist_tendf,  &
                      scalar_tendf,adv_moist_cond,             &
                      RTHBLTEN,RUBLTEN,RVBLTEN,                &
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,              &
                      RQNIBLTEN,                               &! For CAMUWPBL
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%cu_physics .gt. 0)                         &
      CALL phy_cu_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,ru_tendf,rv_tendf,              &
                      RUCUTEN,RVCUTEN,RTHCUTEN,                &
                      RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
                      RQICUTEN,RQSCUTEN,RQCNCUTEN,RQINCUTEN,   &
                      moist_tendf,                             &
                      scalar_tendf,adv_moist_cond,             &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

   if (config_flags%shcu_physics .gt. 0)                       &
      CALL phy_shcu_ten(config_flags,rk_step,n_moist,n_scalar, &
                      rt_tendf,ru_tendf,rv_tendf,              &
                      RUSHTEN,RVSHTEN,RTHSHTEN,                &
                      RQVSHTEN,RQCSHTEN,RQRSHTEN,              &
                      RQISHTEN,RQSSHTEN,RQGSHTEN,RQCNSHTEN,    &
                      RQINSHTEN,moist_tendf,scalar_tendf,      &
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
SUBROUTINE phy_bl_ten(config_flags,rk_step,n_moist,n_scalar,     &
                      rt_tendf,ru_tendf,rv_tendf,moist_tendf,    &
                      scalar_tendf,adv_moist_cond,               &
                      RTHBLTEN,RUBLTEN,RVBLTEN,                  & 
                      RQVBLTEN,RQCBLTEN,RQIBLTEN,RQNIBLTEN,      &
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
          INTENT(INOUT)     ::                      moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),         &
          INTENT(INOUT)     ::                      scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   & 
                                                       RTHBLTEN, &
                                                        RUBLTEN, &
                                                        RVBLTEN, &
                                                       RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN, &
                                                      RQNIBLTEN

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

      CASE (SHINHONGSCHEME)

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

        if (P_QV .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QV),RQVBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QC .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QC),RQCBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QI .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

       IF(.not. adv_moist_cond)THEN

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQCBLTEN, &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QT .ge. PARAM_FIRST_SCALAR)                        &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN, &
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

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQNIBLTEN,&
               config_flags,                                    &
               ids,ide, jds, jde, kds, kde,                     &
               ims, ime, jms, jme, kms, kme,                    &
               its, ite, jts, jte, kts, kte                     )

!        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
!           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQNCBLTEN,&
!                config_flags,                                    &
!                ids,ide, jds, jde, kds, kde,                     &
!                ims, ime, jms, jme, kms, kme,                    &
!                its, ite, jts, jte, kts, kte                     )

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

        if (P_QNI .ge. PARAM_FIRST_SCALAR)                                         &
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNI),RQNIBLTEN,&
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

!        if (P_QNC .ge. PARAM_FIRST_SCALAR)                                         &
!           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNC),RQNCBLTEN,&
!                config_flags,                                    &
!                ids,ide, jds, jde, kds, kde,                     &
!                ims, ime, jms, jme, kms, kme,                    &
!                its, ite, jts, jte, kts, kte                     )

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

        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
             !Balwinder.Singh@pnnl.gov : Diffuse or mix cloud ice mass mixing ratio
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
        if (P_QT .ge. PARAM_FIRST_SCALAR)                                         &
             !Balwinder.Singh@pnnl.gov : Diffuse or mix cloud ice  number mixing ratio
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QT),RQNIBLTEN,&
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
        if (P_QI .ge. PARAM_FIRST_SCALAR)                                         &
             !Balwinder.Singh@pnnl.gov : Diffuse or mix cloud ice mass mixing ratio
           CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQIBLTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
        if (P_QNI .ge. PARAM_FIRST_SCALAR)                                         &
             !Balwinder.Singh@pnnl.gov : Diffuse or mix cloud ice  number mixing ratio
           CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNI),RQNIBLTEN,&
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

      CASE (GBMPBLSCHEME)
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
SUBROUTINE phy_cu_ten(config_flags,rk_step,n_moist,n_scalar,     &
                      rt_tendf,ru_tendf,rv_tendf,                &
                      RUCUTEN,RVCUTEN,RTHCUTEN,                  &
                      RQVCUTEN,RQCCUTEN,RQRCUTEN,                &
                      RQICUTEN,RQSCUTEN,RQCNCUTEN,RQINCUTEN,     &
                      moist_tendf,                               &
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
          INTENT(INOUT)     ::                      moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),         &
          INTENT(INOUT)     ::                      scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                        RUCUTEN, &
                                                        RVCUTEN, &
                                                       RTHCUTEN, &
                                                       RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN, &
                                                      RQCNCUTEN, &
                                                      RQINCUTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::  &
                                                       rt_tendf, &
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

   CASE (KFETASCHEME, MSKFSCHEME, KFCUPSCHEME)!BSINGH -  Added KFCUPSCHEME for CuP
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
   CASE (GDSCHEME, G3SCHEME, GFSCHEME)
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

   CASE (SASSCHEME,OSASSCHEME,MESO_SAS)
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

        if (P_QNC .ge. PARAM_FIRST_SCALAR)                       &!BSINGH - QNC scalar
        CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNC),RQCNCUTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
        if (P_QNI .ge. PARAM_FIRST_SCALAR)                       &!BSINGH - QNI scalar
        CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNI),RQINCUTEN,  &
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

END SUBROUTINE phy_cu_ten

!=================================================================
SUBROUTINE phy_shcu_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,ru_tendf,rv_tendf,                &
                      RUSHTEN,RVSHTEN,RTHSHTEN,                  &
                      RQVSHTEN,RQCSHTEN,RQRSHTEN,                &
                      RQISHTEN,RQSSHTEN,RQGSHTEN,RQCNSHTEN,      &
                      RQINSHTEN,moist_tendf,scalar_tendf,        &
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

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),         &
          INTENT(INOUT)     ::                      moist_tendf 
 
   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),         &
          INTENT(INOUT)     ::                      scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                        RUSHTEN, &
                                                        RVSHTEN, &
                                                       RTHSHTEN, &
                                                       RQVSHTEN, &
                                                       RQCSHTEN, &
                                                       RQRSHTEN, &
                                                       RQISHTEN, &
                                                       RQSSHTEN, &
                                                       RQGSHTEN, &
                                                      RQCNSHTEN, &
                                                      RQINSHTEN

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
        if (P_QNC .ge. PARAM_FIRST_SCALAR)                       &
        CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNC),RQCNSHTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
        if (P_QNI .ge. PARAM_FIRST_SCALAR)                       &
        CALL add_a2a(scalar_tendf(ims,kms,jms,P_QNI),RQINSHTEN,  &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )
        

   CASE (GRIMSSHCUSCHEME)
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

        if (P_QI .ge. PARAM_FIRST_SCALAR)                        &
        CALL add_a2a(moist_tendf(ims,kms,jms,P_QI),RQISHTEN,     &
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

!----------------------------------------------------------------------
SUBROUTINE advance_ppt(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,           &
                     CLDFRA_CUP,                                      &     ! add LD 01/11/2012 !BSINGH -  Added for CuP
                     RQICUTEN,RQSCUTEN,                               &
                     RAINC,RAINCV,RAINSH,PRATEC,PRATESH,  &
                     NCA, HTOP,HBOT,CUTOP,CUBOT,                      &
                     CUPPT, DT, config_flags,                         &
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
                                                       RQSCUTEN, &
                                                       CLDFRA_CUP        ! add LD 01/11/2012 !BSINGH - For CuP

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC, &
                                                         RAINSH, &
                                                         RAINCV, &
                                                         PRATEC, &
                                                        PRATESH, &
                                                            NCA, &
                                                           HTOP, &
                                                           HBOT, &
                                                          CUTOP, &
                                                          CUBOT, &
                                                          CUPPT
   REAL, INTENT(IN) ::                                       DT

! LOCAL  VAR

   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,k_start,k_end
   INTEGER :: NCUTOP, NCUBOT

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
      RAINC(I,J)  = RAINC(I,J) + PRATEC(I,J)*DT
      RAINSH(I,J) = RAINSH(I,J) + PRATESH(I,J)*DT
      CUPPT(I,J)  = CUPPT(I,J) + (PRATEC(I,J)+PRATESH(I,J))*DT/1000.
   ENDDO
   ENDDO

   SELECT CASE (config_flags%cu_physics)

   CASE (KFSCHEME)

        DO J = j_start,j_end
        DO i = i_start,i_end

           IF ( NCA(I,J) .GT. 0 ) THEN

              IF ( NINT(NCA(I,J) / DT) .le. 0 ) THEN

              ! set tendency to zero
!                PRATEC(I,J)=0.
!                RAINCV(I,J)=0.
                 DO k = k_start,k_end
                    RTHCUTEN(i,k,j)=0.
                    RQVCUTEN(i,k,j)=0.
                    RQCCUTEN(i,k,j)=0.
                    RQRCUTEN(i,k,j)=0.
                    if (P_QI .ge. PARAM_FIRST_SCALAR) RQICUTEN(i,k,j)=0.
                    if (P_QS .ge. PARAM_FIRST_SCALAR) RQSCUTEN(i,k,j)=0.
                 ENDDO
              ENDIF

              NCA(I,J)=NCA(I,J)-DT ! Decrease NCA

           ENDIF
!
        ENDDO
        ENDDO

   CASE (BMJSCHEME, CAMZMSCHEME)
 
        DO J = j_start,j_end
        DO i = i_start,i_end

! HTOP, HBOT FOR GFDL RADIATION
           NCUTOP=NINT(CUTOP(I,J))
           NCUBOT=NINT(CUBOT(I,J))
           IF(NCUTOP>1.AND.NCUTOP<KDE)THEN
             HTOP(I,J)=MAX(CUTOP(I,J),HTOP(I,J))
           ENDIF
           IF(NCUBOT>0.AND.NCUBOT<KDE)THEN
             HBOT(I,J)=MIN(CUBOT(I,J),HBOT(I,J))
           ENDIF

        ENDDO
        ENDDO

   CASE (KFETASCHEME, MSKFSCHEME, KFCUPSCHEME)!BSINGH - added KFCUPSCHEME for CuP

        DO J = j_start,j_end
        DO i = i_start,i_end

! HTOP, HBOT FOR GFDL RADIATION
           NCUTOP=NINT(CUTOP(I,J))
           NCUBOT=NINT(CUBOT(I,J))
           IF(NCUTOP>1.AND.NCUTOP<KDE)THEN
             HTOP(I,J)=MAX(CUTOP(I,J),HTOP(I,J))
           ENDIF
           IF(NCUBOT>0.AND.NCUBOT<KDE)THEN
             HBOT(I,J)=MIN(CUBOT(I,J),HBOT(I,J))
           ENDIF

           IF ( NCA(I,J) .GT. 0 ) THEN

              IF ( NINT(NCA(I,J) / DT) .LE. 1 ) THEN

              ! set tendency to zero
!                PRATEC(I,J)=0.
!                RAINCV(I,J)=0.
                 DO k = k_start,k_end
                    RTHCUTEN(i,k,j)=0.
                    RQVCUTEN(i,k,j)=0.
                    RQCCUTEN(i,k,j)=0.
                    RQRCUTEN(i,k,j)=0.
                    if (P_QI .ge. PARAM_FIRST_SCALAR) RQICUTEN(i,k,j)=0.
                    if (P_QS .ge. PARAM_FIRST_SCALAR) RQSCUTEN(i,k,j)=0.
                 ENDDO
              ENDIF

              NCA(I,J)=NCA(I,J)-DT ! Decrease NCA
!              NCA(I,J)=NCA(I,J)-1. ! Decrease NCA

           ENDIF
!
        ENDDO
        ENDDO

        IF ( config_flags%cu_physics == kfcupscheme ) THEN
           DO J = j_start,j_end
           DO i = i_start,i_end
             IF ( NCA(I,J) .GT. 0 ) THEN
              IF ( NINT(NCA(I,J) / DT) .LE. 1 ) THEN
                 DO k = k_start,k_end
                    CLDFRA_CUP(i,k,j)=0. ! By LKB 12/22/11 01/11/2012 !BSINGH - For CuP
                 ENDDO
              ENDIF
             ENDIF
           ENDDO
           ENDDO
        ENDIF

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

END MODULE module_physics_addtendc
