!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
!
! WRF-chem V3.0 : Original version of optical_driver written by Jerome Fast (PNNL)
!                 and James Barnard (PNNL)
!
!WRF:MODEL_LAYER:CHEMISTRY
!
      SUBROUTINE optical_driver(id,curr_secs,dtstep,config_flags,haveaer,&
               chem,dz8w,alt,relhum,                                     &
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
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL,         INTENT(IN   ) :: dtstep
!
! array that holds all advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &  
         INTENT(IN ) ::  relhum, dz8w, alt, h2oai, h2oaj
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
   case ( RADM2SORG,           RADM2SORG_KPP,      RADM2SORG_AQ, &
          GOCART_SIMPLE,       RACMSORG_KPP,       RACMSORG_AQ,  &
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
   case ( RADM2SORG,           RADM2SORG_KPP,      RADM2SORG_AQ, &
          GOCART_SIMPLE,       RACMSORG_KPP,       RACMSORG_AQ   )
     nbin_o = 8
   case (CBMZ_MOSAIC_4BIN,    CBMZ_MOSAIC_8BIN,   &
         CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ )
     nbin_o = nbin_a
   end select
!
     call wrf_debug(15,'optical averaging')
     aer_op_opt_select: SELECT CASE(config_flags%aer_op_opt)
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
        if( config_flags%aer_op_opt > 0 ) then
           call wrf_message('WARNING: Invalid aer_op_opt. Defaulting to VOLUME_APPROX.')
           option_method=1
           option_mie=1
        end if
     END SELECT aer_op_opt_select

     if( config_flags%aer_op_opt > 0 ) then
        call wrf_debug(15,'optical driver: call optical averaging')
        call optical_averaging(id,curr_secs,dtstep,config_flags,     &
             nbin_o,haveaer,option_method,option_mie,chem,dz8w,alt,  &
             relhum,h2oai,h2oaj,                                     &
             tauaer1,tauaer2,tauaer3,tauaer4,                        &
             gaer1,gaer2,gaer3,gaer4,                                &
             waer1,waer2,waer3,waer4,                                &
             bscoef1,bscoef2,bscoef3,bscoef4,                        &
             l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                    &
             ids,ide, jds,jde, kds,kde,                              &
             ims,ime, jms,jme, kms,kme,                              &
             its,ite, jts,jte, kts,kte                               )
     else
        !If aer_op_opt==0 then the optical arrays are already set to
        !zero in chemics_init so there will not be a problem if the
        !user has selected aer_ra_feedback=1.
     end if
!
   endif
   return

END SUBROUTINE optical_driver
