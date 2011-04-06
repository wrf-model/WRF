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


MODULE a_module_physics_addtendc

#if ( EM_CORE == 1 )

   USE module_state_description
   USE module_configure

CONTAINS

SUBROUTINE a_update_phy_ten(rph_tendf,rt_tendf,a_rt_tendf,ru_tendf,a_ru_tendf,  &
                      rv_tendf,a_rv_tendf,moist_tendf,a_moist_tendf,        &
                      scalar_tendf,mu_tendf,                                &
                      RTHRATEN,RTHBLTEN,a_RTHBLTEN,RTHCUTEN,RTHSHTEN,       &
                      RUBLTEN,a_RUBLTEN,RUCUTEN,RUSHTEN,                    &
                      RVBLTEN,a_RVBLTEN,RVCUTEN,RVSHTEN,                    &
                      RQVBLTEN,a_RQVBLTEN,RQCBLTEN,RQIBLTEN,                &
                      RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,         &
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
                                                       a_ru_tendf, &
                                                         rv_tendf, &
                                                       a_rv_tendf, &
                                                         rt_tendf, &
                                                       a_rt_tendf, &
                                                         rph_tendf

   REAL , DIMENSION(ims:ime , jms:jme),INTENT(INOUT) ::  mu_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),           &
          INTENT(INOUT)     ::                        moist_tendf, &
                                                    a_moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),           &
          INTENT(INOUT)     ::                        scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   &
                                                       RTHRATEN, &
                                                       RTHBLTEN, &
                                                     a_RTHBLTEN, &
                                                       RTHCUTEN, &
                                                       RTHSHTEN, &
                                                        RUBLTEN, &
                                                      a_RUBLTEN, &
                                                        RUCUTEN, &
                                                        RUSHTEN, &
                                                        RVBLTEN, &
                                                      a_RVBLTEN, &
                                                        RVCUTEN, &
                                                        RVSHTEN, &
                                                       RQVBLTEN, &
                                                     a_RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN, &
                                                       RQVCUTEN, &
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

   if (config_flags%bl_pbl_physics .gt. 0)                     &
      CALL a_phy_bl_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,a_rt_tendf,ru_tendf,a_ru_tendf,   &
                      rv_tendf,a_rv_tendf,moist_tendf,a_moist_tendf, &
                      scalar_tendf,adv_moist_cond,             &
                      RTHBLTEN,a_RTHBLTEN,RUBLTEN,a_RUBLTEN,   &
                      RVBLTEN,a_RVBLTEN,                       &
                      RQVBLTEN,a_RQVBLTEN,RQCBLTEN,RQIBLTEN,   &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

END SUBROUTINE a_update_phy_ten

!=================================================================
SUBROUTINE a_phy_bl_ten(config_flags,rk_step,n_moist,n_scalar,     &
                      rt_tendf,a_rt_tendf,ru_tendf,a_ru_tendf,     &
                      rv_tendf,a_rv_tendf,moist_tendf,a_moist_tendf, &
                      scalar_tendf,adv_moist_cond,               &
                      RTHBLTEN,a_RTHBLTEN,RUBLTEN,a_RUBLTEN,     &
                      RVBLTEN,a_RVBLTEN,                         & 
                      RQVBLTEN,a_RQVBLTEN,RQCBLTEN,RQIBLTEN,     &
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
                                                  a_moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar),         &
          INTENT(INOUT)     ::                      scalar_tendf

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   & 
                                                       RTHBLTEN, &
                                                     a_RTHBLTEN, &
                                                        RUBLTEN, &
                                                      a_RUBLTEN, &
                                                        RVBLTEN, &
                                                      a_RVBLTEN, &
                                                       RQVBLTEN, &
                                                     a_RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::   &
                                                       rt_tendf, &
                                                     a_rt_tendf, &
                                                       ru_tendf, &
                                                     a_ru_tendf, &
                                                       rv_tendf, &
                                                     a_rv_tendf
! LOCAL VARS

   INTEGER :: i,j,k,IBGN,IEND,JBGN,JEND

!-----------------------------------------------------------------

   SELECT CASE(config_flags%bl_pbl_physics)

      CASE (SURFDRAGSCHEME)

           if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
              CALL a_add_a2a(moist_tendf(ims,kms,jms,P_QV),      &
                   a_moist_tendf(ims,kms,jms,P_QV),              &
                   RQVBLTEN, a_RQVBLTEN,                         &
                   config_flags,                                 &
                   ids,ide, jds, jde, kds, kde,                  &
                   ims, ime, jms, jme, kms, kme,                 &
                   its, ite, jts, jte, kts, kte                  )

           CALL a_add_a2c_v(rv_tendf,a_rv_tendf,                 &
                RVBLTEN,a_RVBLTEN,config_flags,                  &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL a_add_a2c_u(ru_tendf,a_ru_tendf,                 &
                RUBLTEN,a_RUBLTEN,config_flags,                  &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

           CALL a_add_a2a(rt_tendf,a_rt_tendf,                 &
                RTHBLTEN,a_RTHBLTEN,config_flags,                  &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

      CASE DEFAULT

       print*,'a_phy_bl_ten: The pbl scheme does not exist'

   END SELECT

END SUBROUTINE a_phy_bl_ten


!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of add_a2a in reverse (adjoint) mode:
!   gradient     of useful results: lvar rvar
!   with respect to varying inputs: lvar rvar
!   RW status of diff variables: lvar:in-out rvar:incr
SUBROUTINE A_ADD_A2A(lvar, lvarb, rvar, rvarb, config_flags, ids, ide, &
&  jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
&  kts, kte)
  IMPLICIT NONE
!------------------------------------------------------------
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rvarb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: lvarb
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
  DO j=j_end,j_start,-1
    DO k=ktf,kts,-1
      DO i=i_end,i_start,-1
        rvarb(i, k, j) = rvarb(i, k, j) + lvarb(i, k, j)
      END DO
    END DO
  END DO
END SUBROUTINE A_ADD_A2A

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of add_a2c_u in reverse (adjoint) mode:
!   gradient     of useful results: lvar rvar
!   with respect to varying inputs: lvar rvar
!   RW status of diff variables: lvar:in-out rvar:incr
SUBROUTINE A_ADD_A2C_U(lvar, lvarb, rvar, rvarb, config_flags, ids, ide&
&  , jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte&
&  , kts, kte)
  IMPLICIT NONE
!------------------------------------------------------------
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rvarb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: lvarb
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
  DO j=j_end,j_start,-1
    DO k=ktf,kts,-1
      DO i=i_end,i_start,-1
        rvarb(i, k, j) = rvarb(i, k, j) + 0.5*lvarb(i, k, j)
        rvarb(i-1, k, j) = rvarb(i-1, k, j) + 0.5*lvarb(i, k, j)
      END DO
    END DO
  END DO
END SUBROUTINE A_ADD_A2C_U

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of add_a2c_v in reverse (adjoint) mode:
!   gradient     of useful results: lvar rvar
!   with respect to varying inputs: lvar rvar
!   RW status of diff variables: lvar:in-out rvar:incr
SUBROUTINE A_ADD_A2C_V(lvar, lvarb, rvar, rvarb, config_flags, ids, ide&
&  , jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte&
&  , kts, kte)
  IMPLICIT NONE
!------------------------------------------------------------
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: rvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rvarb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: lvar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: lvarb
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
  DO j=j_end,j_start,-1
    DO k=kte,kts,-1
      DO i=i_end,i_start,-1
        rvarb(i, k, j) = rvarb(i, k, j) + 0.5*lvarb(i, k, j)
        rvarb(i, k, j-1) = rvarb(i, k, j-1) + 0.5*lvarb(i, k, j)
      END DO
    END DO
  END DO
END SUBROUTINE A_ADD_A2C_V

#endif

END MODULE a_module_physics_addtendc
