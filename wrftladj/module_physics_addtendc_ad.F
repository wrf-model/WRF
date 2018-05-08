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
                      RTHRATEN,RTHBLTEN,a_RTHBLTEN,RTHCUTEN,a_RTHCUTEN,RTHSHTEN,       &
                      RUBLTEN,a_RUBLTEN,RUCUTEN,RUSHTEN,                    &
                      RVBLTEN,a_RVBLTEN,RVCUTEN,RVSHTEN,                    &
                      RQVBLTEN,a_RQVBLTEN,RQCBLTEN,RQIBLTEN,                &
                      RQVCUTEN,a_RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,         &
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

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme) ::   &
                                                     a_RTHBLTEN, &
                                                     a_RTHCUTEN, &
                                                      a_RUBLTEN, &
                                                      a_RVBLTEN, &
                                                     a_RQVBLTEN, &
                                                     a_RQVCUTEN

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN  ) :: RMUNDGDTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN  ) ::   & ! fire
                                                       rthfrten, &
                                                       rqvfrten   
!------------------------------------------------------------------
!  set up loop bounds for this grid's boundary conditions

   if (config_flags%cu_physics .gt. 0)                         &
      CALL a_phy_cu_ten(config_flags,rk_step,n_moist,n_scalar,   &
                      rt_tendf,a_rt_tendf,ru_tendf,rv_tendf,   &
                      RUCUTEN,RVCUTEN,RTHCUTEN,a_RTHCUTEN,     &
                      RQVCUTEN,a_RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,moist_tendf,a_moist_tendf,&
                      scalar_tendf,adv_moist_cond,             &
                      ids, ide, jds, jde, kds, kde,            &
                      ims, ime, jms, jme, kms, kme,            &
                      its, ite, jts, jte, kts, kte             )

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
                                                        RUBLTEN, &
                                                        RVBLTEN, &
                                                       RQVBLTEN, &
                                                       RQCBLTEN, &
                                                       RQIBLTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme) ::   & 
                                                     a_RTHBLTEN, &
                                                      a_RUBLTEN, &
                                                      a_RVBLTEN, &
                                                     a_RQVBLTEN

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

SUBROUTINE a_phy_cu_ten(config_flags,rk_step,n_moist,n_scalar,     &
                      rt_tendf,a_rt_tendf,ru_tendf,rv_tendf,     &
                      RUCUTEN,RVCUTEN,RTHCUTEN,a_RTHCUTEN,       &
                      RQVCUTEN,a_RQVCUTEN,RQCCUTEN,RQRCUTEN,     &
                      RQICUTEN,RQSCUTEN,moist_tendf,a_moist_tendf,&
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
          INTENT(INOUT)     ::                      moist_tendf, a_moist_tendf

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
                                                       RQSCUTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme) ::   &
                                                     a_RTHCUTEN, &
                                                     a_RQVCUTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::  &
                                                       rt_tendf, &
                                                     a_rt_tendf, &
                                                       ru_tendf, &
                                                       rv_tendf


! LOCAL VARS

   INTEGER :: i,j,k

   SELECT CASE (config_flags%cu_physics)   

   CASE (DUCUSCHEME)
        CALL a_add_a2a(rt_tendf, a_rt_tendf,                     &
                RTHCUTEN, a_RTHCUTEN, config_flags,              &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

        if (P_QV .ge. PARAM_FIRST_SCALAR)                                         &
        CALL a_add_a2a(moist_tendf(ims,kms,jms,P_QV),            &
                a_moist_tendf(ims,kms,jms,P_QV), RQVCUTEN,       &
                a_RQVCUTEN,                                      &
                config_flags,                                    &
                ids,ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                    &
                its, ite, jts, jte, kts, kte                     )

   CASE DEFAULT

   END SELECT

END SUBROUTINE a_phy_cu_ten


!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of advance_ppt in reverse (adjoint) mode:
!   gradient     of useful results: pratesh rqccuten rthcuten rqicuten
!                rqscuten rainc pratec cuppt cutop cubot nca rqrcuten
!                rqvcuten rainsh htop hbot
!   with respect to varying inputs: pratesh rqccuten rthcuten rqicuten
!                rqscuten rainc pratec cuppt cutop cubot nca rqrcuten
!                rqvcuten rainsh htop hbot
!   RW status of diff variables: pratesh:incr rqccuten:in-out rthcuten:in-out
!                rqicuten:in-out rqscuten:in-out rainc:in-out pratec:incr
!                cuppt:in-out cutop:incr cubot:incr nca:in-out
!                rqrcuten:in-out rqvcuten:in-out rainsh:in-out
!                htop:in-out hbot:in-out
SUBROUTINE A_ADVANCE_PPT(rthcuten, rthcutenb, rqvcuten, rqvcutenb, &
&  rqccuten, rqccutenb, rqrcuten, rqrcutenb, rqicuten, rqicutenb, &
&  rqscuten, rqscutenb, rainc, raincb, raincv, rainsh, rainshb, pratec, &
&  pratecb, pratesh, prateshb, nca, ncab, htop, htopb, hbot, hbotb, cutop&
&  , cutopb, cubot, cubotb, cuppt, cupptb, dt, config_flags, ids, ide, &
&  jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, &
&  kts, kte)
  IMPLICIT NONE
!
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rthcuten&
&  , rqvcuten, rqccuten, rqrcuten, rqicuten, rqscuten
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rthcutenb, rqvcutenb, &
&  rqccutenb, rqrcutenb, rqicutenb, rqscutenb
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: rainc, rainsh, &
&  raincv, pratec, pratesh, nca, htop, hbot, cutop, cubot, cuppt
  REAL, DIMENSION(ims:ime, jms:jme) :: raincb, rainshb, pratecb, &
&  prateshb, ncab, htopb, hbotb, cutopb, cubotb, cupptb
  REAL, INTENT(IN) :: dt
! LOCAL  VAR
  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: ncutop, ncubot
  INTEGER :: branch
  REAL :: tempb
!-----------------------------------------------------------------
  IF (config_flags%cu_physics .NE. 0) THEN
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
                IF (p_qi .GE. param_first_scalar) THEN
                  CALL PUSHCONTROL1B(0)
                ELSE
                  CALL PUSHCONTROL1B(1)
                END IF
                IF (p_qs .GE. param_first_scalar) THEN
                  CALL PUSHCONTROL1B(1)
                ELSE
                  CALL PUSHCONTROL1B(0)
                END IF
              END DO
              CALL PUSHCONTROL2B(1)
            ELSE
              CALL PUSHCONTROL2B(2)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
        END DO
      END DO
      DO j=j_end,j_start,-1
        DO i=i_end,i_start,-1
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .EQ. 1) THEN
              DO k=k_end,k_start,-1
                CALL POPCONTROL1B(branch)
                IF (branch .NE. 0) rqscutenb(i, k, j) = 0.0
                CALL POPCONTROL1B(branch)
                IF (branch .EQ. 0) rqicutenb(i, k, j) = 0.0
                rqrcutenb(i, k, j) = 0.0
                rqccutenb(i, k, j) = 0.0
                rqvcutenb(i, k, j) = 0.0
                rthcutenb(i, k, j) = 0.0
              END DO
            END IF
          END IF
        END DO
      END DO
    CASE (ducuscheme) 
!
      DO j=j_start,j_end
        DO i=i_start,i_end
          IF (nca(i, j) .GT. 0) THEN
            IF (NINT(nca(i, j)/dt) .LE. 0) THEN
              CALL PUSHCONTROL2B(1)
            ELSE
              CALL PUSHCONTROL2B(2)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
        END DO
      END DO
      DO j=j_end,j_start,-1
        DO i=i_end,i_start,-1
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .EQ. 1) THEN
              DO k=k_end,k_start,-1
                rqvcutenb(i, k, j) = 0.0
                rthcutenb(i, k, j) = 0.0
              END DO
            END IF
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
              CALL PUSHCONTROL2B(2)
            ELSE
              CALL PUSHCONTROL2B(1)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
          IF (ncubot .GT. 0 .AND. ncubot .LT. kde) THEN
            IF (cubot(i, j) .GT. hbot(i, j)) THEN
              CALL PUSHCONTROL2B(1)
            ELSE
              CALL PUSHCONTROL2B(2)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
        END DO
      END DO
      DO j=j_end,j_start,-1
        DO i=i_end,i_start,-1
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .NE. 1) THEN
              cubotb(i, j) = cubotb(i, j) + hbotb(i, j)
              hbotb(i, j) = 0.0
            END IF
          END IF
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .EQ. 1) THEN
              cutopb(i, j) = cutopb(i, j) + htopb(i, j)
              htopb(i, j) = 0.0
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
              CALL PUSHCONTROL2B(2)
            ELSE
              CALL PUSHCONTROL2B(1)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
          IF (ncubot .GT. 0 .AND. ncubot .LT. kde) THEN
            IF (cubot(i, j) .GT. hbot(i, j)) THEN
              CALL PUSHCONTROL2B(2)
            ELSE
              CALL PUSHCONTROL2B(1)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
          IF (nca(i, j) .GT. 0) THEN
            IF (NINT(nca(i, j)/dt) .LE. 1) THEN
! set tendency to zero
!                PRATEC(I,J)=0.
!                RAINCV(I,J)=0.
              DO k=k_start,k_end
                IF (p_qi .GE. param_first_scalar) THEN
                  CALL PUSHCONTROL1B(0)
                ELSE
                  CALL PUSHCONTROL1B(1)
                END IF
                IF (p_qs .GE. param_first_scalar) THEN
                  CALL PUSHCONTROL1B(1)
                ELSE
                  CALL PUSHCONTROL1B(0)
                END IF
              END DO
              CALL PUSHCONTROL2B(1)
            ELSE
              CALL PUSHCONTROL2B(2)
            END IF
          ELSE
            CALL PUSHCONTROL2B(0)
          END IF
        END DO
      END DO
      DO j=j_end,j_start,-1
        DO i=i_end,i_start,-1
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .EQ. 1) THEN
              DO k=k_end,k_start,-1
                CALL POPCONTROL1B(branch)
                IF (branch .NE. 0) rqscutenb(i, k, j) = 0.0
                CALL POPCONTROL1B(branch)
                IF (branch .EQ. 0) rqicutenb(i, k, j) = 0.0
                rqrcutenb(i, k, j) = 0.0
                rqccutenb(i, k, j) = 0.0
                rqvcutenb(i, k, j) = 0.0
                rthcutenb(i, k, j) = 0.0
              END DO
            END IF
          END IF
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .EQ. 1) THEN
              cubotb(i, j) = cubotb(i, j) + hbotb(i, j)
              hbotb(i, j) = 0.0
            END IF
          END IF
          CALL POPCONTROL2B(branch)
          IF (branch .NE. 0) THEN
            IF (branch .EQ. 1) THEN
              cutopb(i, j) = cutopb(i, j) + htopb(i, j)
              htopb(i, j) = 0.0
            END IF
          END IF
        END DO
      END DO
    END SELECT
    DO j=j_end,j_start,-1
      DO i=i_end,i_start,-1
        tempb = dt*cupptb(i, j)/1000.
        pratecb(i, j) = pratecb(i, j) + dt*raincb(i, j) + tempb
        prateshb(i, j) = prateshb(i, j) + dt*rainshb(i, j) + tempb
      END DO
    END DO
  END IF
END SUBROUTINE A_ADVANCE_PPT

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
