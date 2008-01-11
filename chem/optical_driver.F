!WRF:MODEL_LAYER:CHEMISTRY
!
      SUBROUTINE optical_driver (id,ktau,dtstep,config_flags,haveaer,    &
               chem,dz8w,alt,                                            &
               h2oai,h2oaj,                                              &
               tauaer1,tauaer2,tauaer3,tauaer4,                          &
               gaer1,gaer2,gaer3,gaer4,                                  &
               waer1,waer2,waer3,waer4,                                  &
               bscoef1,bscoef2,bscoef3,bscoef4,                          &
               l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                      &
               ids,ide, jds,jde, kds,kde,                                &
               ims,ime, jms,jme, kms,kme,                                &
               its,ite, jts,jte, kts,kte                                 )

!------------------------------------------------------------------------
   USE module_configure
   USE module_state_description
   USE module_model_constants
   USE module_optical_averaging
   USE module_data_mosaic_therm, only: nbin_a
   INTEGER,      INTENT(IN   ) :: id,                                  &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: ktau
      REAL,      INTENT(IN   ) :: dtstep
!
! array that holds all advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &  
         INTENT(IN ) ::  dz8w, alt, h2oai, h2oaj
!
! arrays that hold the aerosol optical properties
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &  
         INTENT(INOUT ) ::                                             &
           tauaer1, tauaer2, tauaer3, tauaer4,                         &
           gaer1, gaer2, gaer3, gaer4,                                 &
           waer1, waer2, waer3, waer4,                                 &
           bscoef1, bscoef2, bscoef3, bscoef4                              
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:4 ),                  &  
         INTENT(INOUT ) ::                                             &
           l2aer, l3aer, l4aer, l5aer, l6aer, l7aer
!
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   LOGICAL, INTENT(IN) :: haveaer
!         
! local variables
!
      logical processingAerosols
      integer nbin_o
      integer option_method, option_mie

!-----------------------------------------------------------------
! compute only if simulating aerosols and aer_ra_feedback=1

   IF (config_flags%aer_ra_feedback .eq. 0) THEN
         call wrf_debug(15,'no feedback, return from optical driver')
     return
   ENDIF
   select case (config_flags%chem_opt)
   case ( RADM2SORG,           RADM2SORG_KPP,      &
          CBMZ_MOSAIC_4BIN,    CBMZ_MOSAIC_8BIN,   &
          CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ )
      processingAerosols = .true.
      call wrf_debug(15,'optical driver: process aerosols true')
   case default
      processingAerosols = .false.
      call wrf_debug(15,'optical driver: process aerosols false')
   end select

  if( processingAerosols ) then
!
! select aerosol optical property option
! VOLUME: volume averaging of refractive indicies
! * for MADE/SORGAM, assume same 8 size bins as MOSAIC by default
! SHELL: shell-core approach, placeholder
!
   select case (config_flags%chem_opt)
   case ( RADM2SORG, RADM2SORG_KPP)
     nbin_o = 8
   case (CBMZ_MOSAIC_4BIN,    CBMZ_MOSAIC_8BIN,   &
         CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ )
     nbin_o = nbin_a
   end select
!
     call wrf_debug(15,'optical averaging')
     chem_opt_select: SELECT CASE(config_flags%aer_op_opt)
     CASE (VOLUME_APPROX)
       option_method=1
       option_mie=1
     CASE (MAXWELL_APPROX)
       option_method=2
       option_mie=1
     CASE (VOLUME_EXACT)
       option_method=1
       option_mie=2
     CASE (MAXWELL_EXACT)
       option_method=2
       option_mie=2
     CASE (SHELL_EXACT)
       option_method=3
       option_mie=2
     CASE DEFAULT
     END SELECT chem_opt_select
     call wrf_debug(15,'optical driver: call optical averaging')
     call optical_averaging(id,ktau,dtstep,config_flags,             & 
             nbin_o,haveaer,option_method,option_mie,chem,dz8w,alt,  &
             h2oai,h2oaj,                                            &
             tauaer1,tauaer2,tauaer3,tauaer4,                        &
             gaer1,gaer2,gaer3,gaer4,                                &
             waer1,waer2,waer3,waer4,                                &
             bscoef1,bscoef2,bscoef3,bscoef4,                        &
             l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                    &
             ids,ide, jds,jde, kds,kde,                              &
             ims,ime, jms,jme, kms,kme,                              &
             its,ite, jts,jte, kts,kte                               )
!
   endif
   return

END SUBROUTINE optical_driver
