!WRF:MEDIATION_LAYER:PHYSICS
!
! Contains initialization subroutine lightning_init and driver subroutine
! lightning_driver.
!
! History:
!   3.5?  - rewritten and added init, separate out flash rate
!           parameterization from emission
!   3.4.0 - Added cpm option
!   3.3.x - lightning_driver written by M. Barth called in
!           emission_driver in chem
!
! Contact: J. Wong <johnwong@ucar.edu>
!
!**********************************************************************

 MODULE module_lightning_driver
 CONTAINS

!**********************************************************************
!
! SUBROUTINE lightning_init
!
! Performs compatibility checks and zero out flash arrays at first timestep.
!
!**********************************************************************

 SUBROUTINE lightning_init ( &
                              id, itimestep, restart, dt, dx           &
                            ! Namelist control options
                             ,cu_physics,mp_physics,do_radar_ref       &
                             ,lightning_option, lightning_dt           &
                             ,lightning_start_seconds                  &
                             ,ltngacttime                              &
                             ,iccg_prescribed_num, iccg_prescribed_den &
                             ,cellcount_method                         &
                            ! Order dependent args for domain, mem, and tile dims
                             ,ids, ide, jds, jde, kds, kde             &
                             ,ims, ime, jms, jme, kms, kme             &
                             ,its, ite, jts, jte, kts, kte             &
                            ! IC and CG flash rates and accumulated flash count
                             ,ic_flashcount, ic_flashrate              &
                             ,cg_flashcount, cg_flashrate              &
#if ( WRF_CHEM == 1 )
                             ,lnox_opt,lnox_passive                    &
                            ! LNOx tracers (chemistry only)
                             ,lnox_total, lnox_ic, lnox_cg             &
#endif
                            )
!-----------------------------------------------------------------
 USE module_state_description
 USE module_wrf_error
 IMPLICIT NONE
!-----------------------------------------------------------------

 INTEGER,  INTENT(IN)        :: id
 INTEGER,  INTENT(IN)        :: itimestep
 LOGICAL,  INTENT(IN)        :: restart
 REAL,     INTENT(IN)        :: dt,dx
 INTEGER,  INTENT(IN)        :: cu_physics,mp_physics,do_radar_ref,lightning_option
 REAL,     INTENT(IN)        :: lightning_dt, lightning_start_seconds
 REAL,     INTENT(INOUT)     :: ltngacttime
 REAL,     INTENT(IN)        :: iccg_prescribed_num, iccg_prescribed_den
 INTEGER,  INTENT(INOUT)     :: cellcount_method
 INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte

! Making these optional just in case qualitative lightning indices get implemented
 REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), &
                 INTENT(OUT) :: ic_flashcount, ic_flashrate, &
                                cg_flashcount, cg_flashrate

#if ( WRF_CHEM == 1 )
 INTEGER, INTENT(IN)         :: lnox_opt
 LOGICAL, INTENT(IN)         :: lnox_passive
 REAL, OPTIONAL, DIMENSION( ims:ime,kms:kme,jms:jme ), &
                 INTENT(OUT) :: lnox_total, lnox_ic, lnox_cg
#endif

 CHARACTER (LEN=80) :: message

!-----------------------------------------------------------------

!-- do not reset unless it is the first timestep or lightning_option is on
 IF (lightning_option .eq. 0) THEN
   return
 ENDIF

!-- check to see if lightning_dt is less than zero
 IF ( lightning_dt <= 0. ) THEN
    CALL nl_set_lightning_dt( id, dt )
 ENDIF

!-- restarting?  Code after this point is only executed on the very
!                first time step of the simulation
 IF (itimestep .gt. 0 ) THEN
   return
 ENDIF

 ltngacttime = lightning_start_seconds

!--  check to see if the prescribed IC:CG ratio is valid (0/0 and -1 are not allowed)
 IF (iccg_prescribed_den .eq. 0. .and. iccg_prescribed_num .eq. 0.) THEN
    CALL wrf_error_fatal (' lightning_init: iccg_prescribed cannot be 0.0/0.0')
 ENDIF
 IF (iccg_prescribed_den .ne. 0.) THEN
    IF (iccg_prescribed_num/iccg_prescribed_den .eq. -1.) THEN
        CALL wrf_error_fatal (' lightning_init: iccg_prescribed cannot be -1')
    ENDIF
 ENDIF

!
!-- check to see if lightning_option is valid
!
!   Add new schemes here so it is recognized and proper checks are performed
!
 ltng_select: SELECT CASE(lightning_option)

    ! Convective resolved/permitted
    CASE (ltng_crm_PR92w,ltng_crm_PR92z)
        IF ( do_radar_ref .eq. 0 .or. mp_physics .eq. 0) THEN
          CALL wrf_error_fatal( ' lightning_init: Selected lightning option requires microphysics and do_radar_ref=1' )
        ENDIF

        WRITE(message, * ) ' lightning_init: CRM lightning option used: ', lightning_option
        CALL wrf_debug ( 100 , message )

    ! Convective parameterized
    CASE (ltng_cpm_PR92z)
        IF ( cu_physics .ne. GDSCHEME .and. cu_physics .ne. G3SCHEME .and. cu_physics .ne. GFSCHEME  ) THEN
          CALL wrf_error_fatal( ' lightning_init: Selected lightning option requires GD, G3, or GF convective parameterization' )
        ENDIF

        WRITE(message, * ) ' lightning_init: CPM lightning option selected: ', lightning_option
        CALL wrf_debug ( 100 , message )

#if (EM_CORE==1)
    CASE (ltng_lpi)

        WRITE(message, * ) ' lightning_init: LPIM lightning option selected: ', lightning_option
        CALL wrf_debug ( 100 , message )
#endif
    ! Non-existing options
    CASE DEFAULT
        CALL wrf_error_fatal ( ' lightning_init: invalid lightning_option')
 END SELECT ltng_select

!-- do not re-initialize for restarts
 IF (restart) return

!-- zero out arrays
 IF ( PRESENT( ic_flashcount ) .and. PRESENT( ic_flashrate ) .and. &
      PRESENT( cg_flashcount ) .and. PRESENT( cg_flashrate ) ) THEN
    CALL wrf_debug ( 100 , ' lightning_init: flash initializing lightning flash arrays' )

    ic_flashrate(:,:)  = 0.
    ic_flashcount(:,:) = 0.
    cg_flashrate(:,:)  = 0.
    cg_flashcount(:,:) = 0.
 ELSE
    CALL wrf_error_fatal ( ' lightning_init: flash arrays not present' )
 ENDIF

!-- Resolve auto-cellcount method option (cellcount_method=0)
 IF ( ( cellcount_method .eq. 0 ) .and. (lightning_option .eq. ltng_crm_PR92w )) THEN
   IF ( (ime-ims+1)*dx .gt. 1E4 ) THEN ! use patch only if path size > 10 km
     cellcount_method = 1
     WRITE(message, * ) ' lightning_init: setting auto cellcount_method to patch (cellcount_method=1'
   ELSE
     cellcount_method = 2
     WRITE(message, * ) ' lightning_init: setting auto cellcount_method to domain (cellcount_method=2'
   ENDIF
   CALL wrf_debug( 100, message )
 ENDIF

#if ( WRF_CHEM == 1 )

 CALL wrf_debug( 100, ' lightning_init: initializing and validating WRF-Chem only arrays and settings')

 IF ( lnox_opt .ne. lnox_opt_none .and. lightning_option .eq. 0 ) THEN
   CALL wrf_error_fatal ( ' lightning_init: cannot set LNOx without lightning_option')
 ENDIF

 IF ( lnox_opt .eq. lnox_opt_decaria .and. ( do_radar_ref .eq. 0 .or. mp_physics .eq. 0 ) ) THEN
   CALL wrf_error_fatal ( ' lightning_init: lnox_opt_decaria requires microphysics and do_radar_ref' )
 ENDIF

 IF (PRESENT( lnox_total )) lnox_total(:,:,:) = 0.
 IF (PRESENT( lnox_cg    )) lnox_cg(:,:,:)    = 0.
 IF (PRESENT( lnox_ic    )) lnox_ic(:,:,:)    = 0.

#endif

 CALL wrf_debug( 200, ' lightning_init: finishing')

 END SUBROUTINE lightning_init


!**********************************************************************
!
! SUBROUTINE lightning_driver
!
! Redirect to the appropriate lightning subroutine.
!
!**********************************************************************

 SUBROUTINE lightning_driver ( &
                          ! Frequently used prognostics
                            curr_secs, dt, dx, dy,                &
                            xlat, xlon, xland, ht,                &
                            t_phy, p_phy, rho, u, v, w,           &
                            th_phy, pi_phy,dz8w,                  &  
                            z, moist,                             &
                          ! Scheme specific prognostics
                            ktop_deep,                            &
                            refl,                                 &
                            current_time,                         &
                          ! Mandatory namelist inputs
                            lightning_option,                     &
                            lightning_dt,                         &
                            lightning_start_seconds,              &
                            ltngacttime,                          &
                            flashrate_factor,                     &
                          ! IC:CG namelist settings
                            iccg_method,                          &
                            iccg_prescribed_num,                  &
                            iccg_prescribed_den,                  &
                          ! IC:CG inputs
                            iccg_in_num, iccg_in_den,             &
                          ! Scheme specific namelist inputs
                            cellcount_method,                     &
                            cldtop_adjustment,                    &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte,         &
                          ! Mandatory outputs for all quantitative schemes
                            ic_flashcount, ic_flashrate,          &
                            cg_flashcount, cg_flashrate,           &
                            lpi                                   &
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description
 USE module_utility

! Model layer
 USE module_model_constants
 USE module_wrf_error

! Parameterization options
 USE module_ltng_crmpr92       ! lightning_option == 1,   ltng_crm_PR92w
                               ! lightning_option == 2,   ltng_crm_PR92z
 USE module_ltng_cpmpr92z      ! lightning_option == 11,  ltng_cpm_PR92z

! IC:CG methods
 USE module_ltng_iccg

! LPI 
#if (EM_CORE==1)
  USE module_ltng_lpi
#endif

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL(8), INTENT(IN   )    ::       curr_secs
 REAL,    INTENT(IN   )    ::       dt, dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: xlat, xlon, xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: t_phy, p_phy, rho
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: th_phy, pi_phy, dz8w  
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: u, v, w, z
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme, num_moist), INTENT(IN   ) :: moist

! Scheme specific prognostics
 INTEGER, DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: ktop_deep     ! model LNB from cu_physics
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: refl          ! reflectivity from mp_physics
 TYPE(WRFU_Time),                                           INTENT(IN   ) :: current_time  ! For use of IC:CG input

 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT), OPTIONAL :: LPI
! Mandatory namelist inputs
 INTEGER, INTENT(IN   )    ::       lightning_option
 REAL,    INTENT(IN   )    ::       lightning_dt, lightning_start_seconds, flashrate_factor
 REAL,    INTENT(INOUT)    ::       ltngacttime

! IC:CG namelist settings
 INTEGER, INTENT(IN   )    ::       iccg_method
 REAL,    INTENT(IN   )    ::       iccg_prescribed_num, iccg_prescribed_den
 REAL,    DIMENSION( ims:ime, jms:jme, 12), INTENT(IN   ) :: iccg_in_num, iccg_in_den

! Scheme specific namelist inputs
 INTEGER, INTENT(IN   )    ::       cellcount_method                    ! used in CRM
 REAL,    INTENT(IN   )    ::       cldtop_adjustment                   ! used in CPM

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       its,ite, jts,jte, kts,kte

! Mandatory outputs for all quantitative schemes
 REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ic_flashcount , cg_flashcount
 REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(  OUT) :: ic_flashrate  , cg_flashrate


! Local variables
 REAL(8) :: LtngActivationTime
 REAL(8) :: nextTime
 REAL, DIMENSION( ims:ime, jms:jme ) :: total_flashrate
 CHARACTER (LEN=80) :: message
 LOGICAL :: do_ltng

 REAL, PARAMETER            :: reflthreshold = 20. ! reflectivity threshold for CRM schemes
 REAL, DIMENSION( kms:kme ) :: cellcount

!-----------------------------------------------------------------

 IF ( lightning_option .eq. 0 ) RETURN

 nextTime = curr_secs + REAL(dt,8)
 LtngActivationTime = REAL(ltngacttime,8)
 do_ltng = LtngActivationTime >= curr_secs .and. LtngActivationTime <= nextTime

 IF( .not. do_ltng ) THEN
   RETURN
 ENDIF

!-----------------------------------------------------------------
! This driver performs several steps in order to produce lightning
! flash rate and flash count diagnostics:
!
! 1. Determine cloud extents for specific CRM schemes
! 2. Total flash rate assignment to 2D array
! 3. Partitioning of total lightning into IC & CG
! 4. Scale flash rate by flashrate_factor and lightning_dt
!
!-----------------------------------------------------------------

 IF ( lightning_option .eq. ltng_crm_PR92w .or. &
      lightning_option .eq. ltng_crm_PR92z ) THEN
   CALL wrf_debug ( 100, ' lightning_driver: determining cloud extents for CRM' )
   CALL countCells( &
          ! Inputs
            refl, reflthreshold, cellcount_method,     &
          ! Order dependent args for domain, mem, and tile dims
            ids, ide, jds, jde, kds, kde,              &
            ims, ime, jms, jme, kms, kme,              &
            its, ite, jts, jte, kts, kte,              &
          ! Outputs
            cellcount )
   WRITE(message, * ) ' lightning_driver: Max cell count = ', maxval(cellcount)
   CALL wrf_debug ( 100, message )
 ENDIF

!-----------------------------------------------------------------

 CALL wrf_debug ( 100, ' lightning_driver: calculating flash rate' )
 flashrate_select: SELECT CASE(lightning_option)

    ! CRM lightning options
    CASE( ltng_crm_PR92w )
        CALL wrf_debug ( 100, ' lightning_driver: calling Price and Rind 1992 (w_max, CRM)' )

        CALL ltng_crmpr92w ( &
                  ! Frequently used prognostics
                    dx, dy, xland, ht, z, t_phy,          &
                  ! Scheme specific prognostics
                    w, refl, reflthreshold, cellcount,    &
                  ! Scheme specific namelist inputs
                    cellcount_method,                     &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,         &
                    ims, ime, jms, jme, kms, kme,         &
                    its, ite, jts, jte, kts, kte,         &
                  ! Mandatory output for all quantitative schemes
                    total_flashrate                       &
                  )
    CASE( ltng_crm_PR92z )
        CALL wrf_debug ( 100, ' lightning_driver: calling Price and Rind 1992 (z_top, CRM)' )
        CALL ltng_crmpr92z ( &
                  ! Frequently used prognostics
                    dx, dy, xland, ht, z, t_phy,          &
                  ! Scheme specific prognostics
                    refl, reflthreshold, cellcount,       &
                  ! Scheme specific namelist inputs
                    cellcount_method,                     &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,         &
                    ims, ime, jms, jme, kms, kme,         &
                    its, ite, jts, jte, kts, kte,         &
                  ! Mandatory output for all quantitative schemes
                    total_flashrate                       &
                  )
!   CASE ( another_crm_option)
!       CALL ...

    ! CPM lightning options
    CASE( ltng_cpm_PR92z )
        CALL wrf_debug ( 100, ' lightning_driver: calling Price and Rind 1992 (z_top, CPM)' )

        CALL ltng_cpmpr92z ( &
                  ! Frequently used prognostics
                    dx, dy, xland, ht, z, t_phy,      &
                    ktop_deep, cldtop_adjustment,     &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,     &
                    ims, ime, jms, jme, kms, kme,     &
                    its, ite, jts, jte, kts, kte,     &
                  ! Mandatory output for all quantitative schemes
                    total_flashrate                   &
                  )

    ! LPI lightning options
#if (EM_CORE==1)
    CASE( ltng_lpi )
        CALL wrf_debug ( 100, ' lightning_driver: calling Light Potential Index' )
        IF(F_QG) THEN
        CALL   calclpi(W=w,                              &
                     Z=z,                              &
                     PI_PHY=pi_phy, RHO_PHY=rho,             &
                     TH_PHY=TH_PHY,P_PHY=p_phy,                  &
                     DZ8w=dz8w,                          &
                     QV=moist(ims,kms,jms,P_QV),         &   !Qv=qv_curr,                         &
                     QC=moist(ims,kms,jms,P_QC),         &   !Qc=qc_curr,                         &
                     QR=moist(ims,kms,jms,P_QR),         &   !QR=qr_curr,                         &
                     QI=moist(ims,kms,jms,P_QI),         &   !QI=qi_curr,                         &
                     QS=moist(ims,kms,jms,P_QS),         &   !qs_curr,                         &
                     QG=moist(ims,kms,jms,P_QG),         &   !qg_curr,                         &
                     QH=moist(ims,kms,jms,P_QH),         &   !qh_curr,                         &
                  lpi=lpi &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte)
        ELSE
        WRITE(wrf_err_message, * ) ' lightning_driver: LPI option needs Microphysics Option with Graupel '
        CALL wrf_error_fatal ( wrf_err_message )
        ENDIF
#endif
!   CASE ( another_cpm_option)

    ! Invalid lightning options
    CASE DEFAULT
        WRITE(wrf_err_message, * ) ' lightning_driver: The lightning option does not exist: lightning_opt = ', lightning_option
        CALL wrf_error_fatal ( wrf_err_message )

 END SELECT flashrate_select

!-----------------------------------------------------------------

 CALL wrf_debug ( 100, ' lightning_driver: partitioning IC:CG')
 iccg_select: SELECT CASE(iccg_method)
    ! Flash rate option defaults
    CASE( 0 ) iccg_select
        CALL wrf_debug( 100, ' lightning_driver: using option-default IC:CG method' )
        iccg_method_default: SELECT CASE(lightning_option)

            CASE( ltng_crm_PR92w, ltng_crm_PR92z, ltng_cpm_PR92z ) iccg_method_default
                CALL iccg_boccippio( &
                            xlat, xlon,                                &
                            iccg_prescribed_num, iccg_prescribed_den,  &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,              &
                            ims, ime, jms, jme, kms, kme,              &
                            its, ite, jts, jte, kts, kte,              &
                          ! Input
                            total_flashrate,                           &
                          ! Output
                            ic_flashrate, cg_flashrate                 &
                          )

            CASE DEFAULT iccg_method_default
                CALL wrf_debug ( 100, ' lightning_driver: no method-default IC:CG implemented, using user-prescribed constant')
                CALL iccg_user_prescribed( &
                            iccg_prescribed_num,                  &
                            iccg_prescribed_den,                  &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte,         &
                          ! Input
                            total_flashrate,                      &
                          ! Output
                            ic_flashrate, cg_flashrate            &
                          )

        END SELECT iccg_method_default

    ! Used-prescribed constant
    CASE( 1 ) iccg_select
        WRITE(message, * ) ' lightning_driver: using user-prescribed IC:CG ratio = ', iccg_prescribed_num/iccg_prescribed_den
        CALL wrf_debug ( 100, message )
        CALL iccg_user_prescribed( &
                    iccg_prescribed_num,                  &
                    iccg_prescribed_den,                  &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,         &
                    ims, ime, jms, jme, kms, kme,         &
                    its, ite, jts, jte, kts, kte,         &
                  ! Input
                    total_flashrate,                      &
                  ! Output
                    ic_flashrate, cg_flashrate            &
                  )

    ! Boccippio et al, 2001
    CASE( 2 ) iccg_select
        CALL wrf_debug ( 100, ' lightning_driver: using Boccippio 2001 IC:CG climatology')
        CALL iccg_boccippio( &
                    xlat, xlon,                                &
                    iccg_prescribed_num, iccg_prescribed_den,  &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  ! Input
                    total_flashrate,                           &
                  ! Output
                    ic_flashrate, cg_flashrate                 &
                  )

    ! Price and Rind, 1993
    CASE( 3 ) iccg_select
        iccg_pr93_select: SELECT CASE(lightning_option)
        CASE( ltng_crm_PR92w, ltng_crm_PR92z ) iccg_pr93_select
            CALL wrf_debug ( 100, ' lightning_driver: using Price and Rind 1993 IC:CG ratio (CRM)')
            CALL iccg_crm_pr93( &
                    refl, reflthreshold, t_phy, z,             &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  ! Input
                    total_flashrate,                           &
                  ! Output
                    ic_flashrate, cg_flashrate                 &
                )

        CASE DEFAULT iccg_pr93_select
            CALL wrf_debug ( 100, ' lightning_driver: using Price and Rind 1993 IC:CG ratio (CPM)')
            CALL iccg_pr93( &
                    ktop_deep, cldtop_adjustment, t_phy, z,    &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  ! Input
                    total_flashrate,                           &
                  ! Output
                    ic_flashrate, cg_flashrate                 &
                )
        END SELECT iccg_pr93_select

    CASE( 4 ) iccg_select
        CALL wrf_debug ( 100, ' lightning_driver: using input IC:CG ratio from iccg_in_(num|den)' )
        CALL iccg_input( &
                    iccg_prescribed_num, iccg_prescribed_den,  &
                    iccg_in_num, iccg_in_den, current_time,    &
                  ! Order dependent args for domain, mem, and tile dims
                    ids, ide, jds, jde, kds, kde,              &
                    ims, ime, jms, jme, kms, kme,              &
                    its, ite, jts, jte, kts, kte,              &
                  ! Input
                    total_flashrate,                           &
                  ! Output
                    ic_flashrate, cg_flashrate                 &
                  )

    ! Invalid IC:CG method
    CASE DEFAULT iccg_select
        WRITE(wrf_err_message, * ) ' lightning_driver: Invalid IC:CG method (iccg_method) = ', lightning_option
        CALL wrf_error_fatal ( wrf_err_message )

 END SELECT iccg_select

!-----------------------------------------------------------------

 CALL wrf_debug( 200, ' lightning_driver: converting flash rates to flash counts')

 ic_flashrate(its:ite,jts:jte) = ic_flashrate(its:ite,jts:jte) * flashrate_factor
 cg_flashrate(its:ite,jts:jte) = cg_flashrate(its:ite,jts:jte) * flashrate_factor

 ic_flashcount(its:ite,jts:jte) = ic_flashcount(its:ite,jts:jte) + ic_flashrate(its:ite,jts:jte) * lightning_dt
 cg_flashcount(its:ite,jts:jte) = cg_flashcount(its:ite,jts:jte) + cg_flashrate(its:ite,jts:jte) * lightning_dt
 
 do
   if( REAL(ltngacttime,8) <= nextTime ) then
     ltngacttime = ltngacttime + lightning_dt
   else
     exit
   endif
 enddo

!-----------------------------------------------------------------

 CALL wrf_debug ( 100, ' lightning_driver: returning from')

 END SUBROUTINE lightning_driver


!**********************************************************************
!
! SUBROUTINE countCells
!
! For counting number of cells where reflectivity exceeds a certain
! threshold. Typically used by CRM schemes to redistribute lightning
! within convective cores.
!
! Departure from original implementation:
! Output includes domain-wide cellcounts if cellcount_method = 2
!
!**********************************************************************

 SUBROUTINE countCells( &
          ! Inputs
            refl, reflthreshold, cellcount_method,     &
          ! Order dependent args for domain, mem, and tile dims
            ids, ide, jds, jde, kds, kde,              &
            ims, ime, jms, jme, kms, kme,              &
            its, ite, jts, jte, kts, kte,              &
          ! Outputs
            cellcount )

 USE module_dm, only: wrf_dm_sum_real

 IMPLICIT NONE
!-----------------------------------------------------------------

! Inputs
 REAL,    DIMENSION( ims:ime,kms:kme,jms:jme ), INTENT(IN   ) :: refl
 REAL,    INTENT(IN   ) :: reflthreshold
 INTEGER, INTENT(IN   ) :: cellcount_method

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       its,ite, jts,jte, kts,kte


! Outputs
 REAL,    DIMENSION( kms:kme ), INTENT(  OUT) :: cellcount

! Local vars
 INTEGER :: i,k,j

!-----------------------------------------------------------------

 cellcount(kts:kte) = 0.
 DO j=jts,jte
   DO k=kts,kte
     DO i=its,ite
       IF ( refl(i,k,j) .gt. reflthreshold ) THEN
         cellcount(k) = cellcount(k) + 1
       ENDIF
     ENDDO
   ENDDO
 ENDDO

 IF ( cellcount_method .eq. 2 ) THEN
   DO k=kts,kte
     cellcount(k) = wrf_dm_sum_real(cellcount(k))
   ENDDO
 ENDIF

 END SUBROUTINE

 END MODULE module_lightning_driver
