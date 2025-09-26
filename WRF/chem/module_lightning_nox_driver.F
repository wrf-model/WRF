!WRF:MODEL_LAYER:CHEMISTRY
!
! Calls lightning_nox_driver for calling subroutines that convert lightning
! flash rate predictions (2D, #/sec) into NO emission (3D, moles/sec)
!
! History:
!   3.4.1 - rewritten and added init, separate out flash rate
!           parameterization from emission
!   3.3.x - lightning_driver written by M. Barth and L. Ott. Called
!           in emission_driver in chem
!
! Contact: J. Wong <johnwong@ucar.edu>
!
!**********************************************************************
 MODULE module_lightning_nox_driver
  CONTAINS

 SUBROUTINE lightning_nox_driver ( &
                          ! Frequently used prognostics
                            curr_secs, dt, dx, dy,                &
                            xlat, xlon, xland, ht,                &
                            t_phy, p_phy, rho, u, v, w,           &
                            z, moist,                             &
                            ic_flashrate, cg_flashrate,           &
                          ! Scheme specific prognostics
                            refl,                                 &
                          ! Mandatory namelist inputs
                            lightning_option, lightning_dt,       &
                            lightning_start_seconds,              &
                            N_IC, N_CG,                           &
                            lnox_opt, lnox_passive,               &
                          ! Scheme specific namelist inputs
                            ltng_temp_upper,ltng_temp_lower,      &
                            cellcount_method,                     & ! This is in the physics namelist
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte,         &
                          ! outputs
                            c_no,                                 & ! NO concentration
                            lnox_total, lnox_ic, lnox_cg          &
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants
 USE module_wrf_error

! Methods
 USE module_lightning_nox_ott
 USE module_lightning_nox_decaria, only: lightning_nox_decaria

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL(8), INTENT(IN   )    ::       curr_secs
 REAL,    INTENT(IN   )    ::       dt, dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: xlat, xlon, xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: t_phy, p_phy, rho
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: u, v, w, z
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme, num_moist), INTENT(IN   ) :: moist

 REAL,    DIMENSION( ims:ime,          jms:jme ),           INTENT(IN   ) :: ic_flashrate  , cg_flashrate ! #/sec

! Scheme specific prognostics
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT(IN   ) :: refl

! Mandatory namelist inputs
 INTEGER, INTENT(IN   )    ::       lightning_option
 REAL,    INTENT(IN   )    ::       lightning_dt, lightning_start_seconds
 REAL,    INTENT(IN   )    ::       N_IC, N_CG
 INTEGER, INTENT(IN   )    ::       lnox_opt
 LOGICAL, INTENT(IN   )    ::       lnox_passive

! Scheme specific namelist inputs
 REAL,    INTENT(IN   )    ::       ltng_temp_upper, ltng_temp_lower
 INTEGER, INTENT(IN   )    ::       cellcount_method

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       its,ite, jts,jte, kts,kte

! Mandatory inputs for all quantitative schemes
 REAL,           DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: c_no
 REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: lnox_total,lnox_ic,lnox_cg

! Local variables
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: lnox_total_tend
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: lnox_ic_tend, lnox_cg_tend

 CHARACTER (LEN=80) :: message

!-----------------------------------------------------------------

 IF (lightning_option .eq. 0 .or. lnox_opt .eq. 0) RETURN

 IF ((curr_secs+REAL(dt,8)) .lt. REAL(lightning_start_seconds,8)) RETURN

 IF ( N_IC .eq. 0. .and. N_CG .eq. 0. ) RETURN
!-----------------------------------------------------------------

 CALL wrf_debug( 100, ' lightning_nox_driver: converting flash rate to emission')

 lnox_select: SELECT CASE(lnox_opt)
  
   CASE(lnox_opt_ott)
      CALL lightning_nox_ott ( &
              ! Frequently used prognostics
                dx, dy, xlat, xland, ht, rho, z,      &
                ic_flashrate, cg_flashrate,           & ! flashes (#/s)
              ! Namelist inputs
                N_IC, N_CG,                           &
              ! Order dependent args for domain, mem, and tile dims
                ids, ide, jds, jde, kds, kde,         &
                ims, ime, jms, jme, kms, kme,         &
                its, ite, jts, jte, kts, kte,         &
              ! outputs
                lnox_total_tend                       & ! tendency (ppmv/s)
              )

   CASE(lnox_opt_decaria)
      CALL lightning_nox_decaria ( &
              ! Frequently used prognostics
                dx, dy, xland, ht, t_phy, rho, z, p_phy,      &
                ic_flashrate, cg_flashrate,           & ! flashes (#/s)
              ! Scheme specific prognostics
                refl,                                 &
              ! Namelist inputs
                N_IC, N_CG,                           &
                ltng_temp_upper,ltng_temp_lower,      &
                cellcount_method,                     &
              ! Order dependent args for domain, mem, and tile dims
                ids, ide, jds, jde, kds, kde,         &
                ims, ime, jms, jme, kms, kme,         &
                its, ite, jts, jte, kts, kte,         &
              ! outputs
                lnox_ic_tend, lnox_cg_tend            & ! tendency (ppmv/s)
              )
    ! Invalid lightning NOx options
    CASE DEFAULT
        WRITE(wrf_err_message, * ) ' lightning_nox_driver: The lightning nox option does not exist: lnox_opt = ', lnox_opt
        CALL wrf_error_fatal ( wrf_err_message )

 END SELECT lnox_select


!-----------------------------------------------------------------
 lnox_add_select: SELECT CASE(lnox_opt)
  
   CASE(lnox_opt_ott)

     CALL wrf_debug( 100, ' lightning_nox_driver: adding total tendency to NO and passive tracers')
     WRITE(wrf_err_message, * )  'lightning_nox_driver: max lnox_total_tend = ', maxval(lnox_total_tend(its:ite,kts:kte,jts:jte))
     CALL wrf_debug( 100, wrf_err_message)

     lnox_total(its:ite,kts:kte,jts:jte) = lnox_total(its:ite,kts:kte,jts:jte) + &
                                               lnox_total_tend(its:ite,kts:kte,jts:jte) * lightning_dt
     IF ( .not.lnox_passive ) THEN
       c_no(its:ite,kts:kte,jts:jte) = c_no(its:ite,kts:kte,jts:jte) + &
                                               lnox_total_tend(its:ite,kts:kte,jts:jte) * lightning_dt
     ENDIF

   CASE(lnox_opt_decaria)
     CALL wrf_debug( 100, ' lightning_nox_driver: adding IC an& CG tendencies to NO and passive tracers')
     WRITE(wrf_err_message, * )  'lightning_nox_driver: max lnox_ic/cg_tend = ', maxval(lnox_ic_tend(its:ite,kts:kte,jts:jte)), &
                     maxval(lnox_cg_tend(its:ite,kts:kte,jts:jte))
     CALL wrf_debug( 100, wrf_err_message)

     lnox_ic(its:ite,kts:kte,jts:jte) = lnox_ic(its:ite,kts:kte,jts:jte) + lnox_ic_tend(its:ite,kts:kte,jts:jte) * lightning_dt
     lnox_cg(its:ite,kts:kte,jts:jte) = lnox_cg(its:ite,kts:kte,jts:jte) + lnox_cg_tend(its:ite,kts:kte,jts:jte) * lightning_dt
     IF ( .not.lnox_passive ) THEN
       c_no(its:ite,kts:kte,jts:jte) = c_no(its:ite,kts:kte,jts:jte) + &
                                        ( lnox_ic_tend(its:ite,kts:kte,jts:jte) + &
                                          lnox_cg_tend(its:ite,kts:kte,jts:jte) ) * lightning_dt
     ENDIF

   CASE DEFAULT
        WRITE(wrf_err_message, * ) ' lightning_nox_driver: The lightning nox option does not exist: lnox_opt = ', lnox_opt
        CALL wrf_error_fatal ( wrf_err_message )
 END SELECT lnox_add_select

 CALL wrf_debug( 100, ' lightning_nox_driver: finishing')

 END SUBROUTINE lightning_nox_driver

 END MODULE module_lightning_nox_driver
