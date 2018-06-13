!************************************************************************
! This computer software was prepared by Battelle Memorial Institute,
! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! Module to Compute Aerosol Optical Properties
! * Author: Jerome D. Fast
! * Originators of parts of code:
!   Rahul A. Zaveri, Jim Barnard, Richard C. Easter, William I.
!   Gustafson Jr.
! Last update: February 2009
!
! Contact:
! Jerome D. Fast, PhD
! Staff Scientist
! Pacific Northwest National Laboratory
! P.O. Box 999, MSIN K9-30
! Richland, WA, 99352
! Phone: (509) 372-6116
! Email: Jerome.Fast@pnl.gov
!
! Please report any bugs or problems to Jerome Fast, the WRF-chem
! implmentation team leader for PNNL.
!
! Terms of Use:
!  1) Users are requested to consult the primary author prior to
!     modifying this module or incorporating it or its submodules in
!     another code. This is meant to ensure that the any linkages and/or
!     assumptions will not adversely affect the operation of this module.
!  2) The source code in this module is intended for research and
!     educational purposes. Users are requested to contact the primary
!     author regarding the use of the code for any commercial application.
!  3) Users preparing publications resulting from the usage of this code
!     are requested to cite one or more of the references below
!     (depending on the application) for proper acknowledgement.
!
! References: 
! * Fast, J.D., W.I. Gustafson Jr., R.C. Easter, R.A. Zaveri, J.C.
!   Barnard, E.G. Chapman, G.A. Grell, and S.E. Peckham (2005), Evolution
!   of ozone, particulates, and aerosol direct radiative forcing in the
!   vicinity of Houston using a fully-coupled meteorology-chemistry-
!   aerosol model. JGR, 111, doi:10.1029/2005JD006721.
! * Barnard, J.C., J.D. Fast, G. Paredes-Miranda, W.P. Arnott, and
!   A. Laskin (2010), Technical note: evaluation of the WRF-Chem
!   "aerosol chemical to aerosol optical properties" module using data
!   from the MILAGRO campaign, Atmos. Chem. Phys., 10, 7325-7340,
!   doi:10.5194/acp-10-7325-2010.
!
! Contact Jerome Fast for updates on the status of manuscripts under
! review.  
!
! Additional information:
! *  www.pnl.gov/atmospheric/research/wrf-chem
!
! Support:
! Funding for this code development was provided by the U.S. Department
! of Energy under the auspices of Atmospheric Science Program of the
! Office of Biological and Environmental Research the PNNL Laboratory
! Research and Directed Research and Development program.
!************************************************************************
	module module_optical_averaging
	
        USE module_data_rrtmgaeropt
        implicit none
 	integer, parameter, private :: lunerr = -1
	
	contains

!----------------------------------------------------------------------------------
! Aerosol optical properties computed using three methods (option_method):
! 1) volume averaging mixing rule: method that assumes internal-mixing of aerosol
!    composition that averages the refractive indices for each size bin
! 2) Maxwell-Garnett mixing rule: method that randomly distributes black carbon
!    within a particle
! 3) shell-core: method that assumes a "core" composed of black carbon surrounded 
!    by a "shell" composed of all other compositions
!
! There are two Mie routines included (option_mie):
! 1) subroutine mieaer: Employs a Chebyshev economization (Fast et al. 2006, Ghan
!    et al. (2001) so that full Mie computations are called only once and then
!    expansion coeffiecients are used for subsequent times to save CPU.  This
!    method is somewhat less accurate than full Mie calculation.
! 2) subroutine mieaer_sc: Full Mie calculation at each time step that also 
!    permits computation of shell-core method.
! 
! Sectional and modal size distributions are treated similary, but there is 
! separate code currrently to handle differences between MOSAIC and MADE/SORGAM.
!
! Methodology for sectional:
! * 3-D arrays for refractive index, wet radius, and aerosol number produced by
!   optical_prep_sectional are then passed into mieaer_sectional
! * subroutine mieaer produces vertical profiles of aerosol optical properties for
!   4 wavelengths that are put into 3-D arrays and passed back up to chem_driver.F
! * tauaer*, waer*, gaer* passed to module_ra_gsfcsw.F
! * tauaer*, waer*, gaer*, l2-l7 passed to module_phot_fastj.F
! Methodology for modal:
! * similar to sectional, except divide modal mass into discrete size bins first
! * currently assume same 8 size bins as MOSAIC, but other bins are possible
!   
! THIS CODE IS STILL BEING TESTED.  USERS ARE ENCOURAGED TO USE ONLY
! AER_OP_OPT=1
!
      subroutine optical_averaging(id,curr_secs,dtstep,config_flags,    &
                 nbin_o,haveaer,option_method,option_mie,chem,dz8w,alt, &
                 relhum,h2oai,h2oaj,                                    &
!                tauaer1,tauaer2,tauaer3,tauaer4,                       &
!                gaer1,gaer2,gaer3,gaer4,                               &
!                waer1,waer2,waer3,waer4,                               &
!                bscoef1,bscoef2,bscoef3,bscoef4,                       &
                 tauaersw,extaersw,gaersw,waersw,bscoefsw,              &
                 l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                   &
                 totoa_a01,totoa_a02,totoa_a03,totoa_a04,               &
                 totoa_a05,totoa_a06,totoa_a07,totoa_a08,               &
                 tauaerlw,extaerlw,                                     &
                 ids,ide, jds,jde, kds,kde,                             &
                 ims,ime, jms,jme, kms,kme,                             &
                 its,ite, jts,jte, kts,kte                              )
!----------------------------------------------------------------------------------
   USE module_configure
   USE module_state_description
   USE module_model_constants
!  USE module_data_mosaic_therm, only: nbin_a, nbin_a_maxd      
   USE module_peg_util, only:  peg_error_fatal, peg_message
   IMPLICIT NONE
!
   INTEGER,      INTENT(IN   ) :: id,                                  &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: nbin_o
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL,         INTENT(IN   ) :: dtstep
!
! array that holds all advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  relhum,dz8w, alt, h2oai, h2oaj,               &
                         totoa_a01, totoa_a02, totoa_a03, totoa_a04,   &
                         totoa_a05,totoa_a06,totoa_a07,totoa_a08

   integer nspint
   parameter ( nspint = 4 ) ! number of spectral interval shortwave bands 
!
! arrays that hold the aerosol optical properties
!
!  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
!        INTENT(INOUT ) ::                                             &
!          tauaer1, tauaer2, tauaer3, tauaer4,                         &
!          gaer1, gaer2, gaer3, gaer4,                                 &
!          waer1, waer2, waer3, waer4,                                 &
!          bscoef1, bscoef2, bscoef3, bscoef4
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme,1:nspint ),                   &
         INTENT(INOUT ) ::                                             &
           tauaersw,extaersw,gaersw,waersw,bscoefsw
!  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:4 ),                  &
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:nspint ),                  &
         INTENT(INOUT ) ::                                             &
           l2aer, l3aer, l4aer, l5aer, l6aer, l7aer
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme,1:nlwbands),                   &
         INTENT(INOUT ) ::                                             &
           tauaerlw,extaerlw
!
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL, INTENT(IN) :: haveaer
!
! local variables
!
   real, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o ) ::           &
           radius_wet, number_bin, radius_core
   real, dimension( 1:nbin_o, kts:kte) ::                              &
           radius_wet_col, number_bin_col, radius_core_col
   complex, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o ) :: &   !for gocart
           refindx0, refindx_core0, refindx_shell0
!  complex, dimension( 1:nbin_o, kts:kte) ::                           &
!          refindx_col, refindx_core_col, refindx_shell_col

   complex, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o,1:nspint) ::        &
           swrefindx, swrefindx_core, swrefindx_shell
   complex, dimension( 1:nbin_o, kts:kte,1:nspint) ::                           &
           swrefindx_col, swrefindx_core_col, swrefindx_shell_col
   complex, dimension( 1:nbin_o, kts:kte) ::                           &
           swrefindx_col1, swrefindx_core_col1, swrefindx_shell_col1
   complex, dimension( its:ite, kts:kte, jts:jte, 1:nbin_o,1:nlwbands) ::        &
           lwrefindx, lwrefindx_core, lwrefindx_shell
   complex, dimension( 1:nbin_o, kts:kte,1:nlwbands) ::                           &
           lwrefindx_col, lwrefindx_core_col, lwrefindx_shell_col

   real, dimension( kts:kte ) :: dz
!
!  integer nspint
   integer iclm, jclm, k, isize
   integer option_method, option_mie
!  parameter ( nspint = 4 ) ! number of spectral interval bands 
   real, dimension( nspint, kts:kte ) ::                               &
!          sizeaer,extaer,waer,gaer,tauaer,bscoef
           swsizeaer,swextaer,swwaer,swgaer,swtauaer,swbscoef
   real, dimension( nspint, kts:kte ) ::                               &
           l2, l3, l4, l5, l6, l7
   real, dimension( nlwbands, kts:kte ) ::                               &
           lwtauaer,lwextaer 
   real refr
   integer ns
   real fv
   complex aa, bb
   character*150 msg
   integer :: uoc_flag  ! flag for UoC dust emissions
!  save :: sizeaer,extaer,waer,gaer,tauaer,bscoef
!  save :: l2,l3,l4,l5,l6,l7
!----------------------------------------------------------------------------------
   uoc_flag = 0
   if (config_flags%dust_opt .eq. 4) uoc_flag = 1
!
!        write( msg, '(a, 6i4)' )	&
!                 'jdf ', ids, ide, jds, jde, kds, kde
!                 call peg_message( lunerr, msg )
!        write( msg, '(a, 6i4)' )	&
!                 'jdf ', ims, ime, jms, jme, kms, kme
!                 call peg_message( lunerr, msg )
!        write( msg, '(a, 6i4)' )	&
!                 'jdf ', its, ite, jts, jte, kts, kte
!                 call peg_message( lunerr, msg )
   chem_select: SELECT CASE(config_flags%chem_opt)
!
   CASE (RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP,     &
         RACM_ESRLSORG_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP,          &
         RACM_ESRLSORG_AQCHEM_KPP, RACMSORG_KPP,                       &
         CBMZSORG, CBMZSORG_AQ,                                        &
         CB05_SORG_AQ_KPP)
     call optical_prep_modal(nbin_o, chem, alt,                        &
!         h2oai, h2oaj, refindx, radius_wet, number_bin,               &
!         radius_core, refindx_core, refindx_shell,                    &
          h2oai, h2oaj, radius_core,radius_wet, number_bin,               &
          swrefindx,swrefindx_core, swrefindx_shell,                    &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                    &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
!!! TUCCELLA
   CASE (RACM_SOA_VBS_KPP, RACM_SOA_VBS_AQCHEM_KPP,RACM_SOA_VBS_HET_KPP)
     call optical_prep_modal_soa_vbs(nbin_o, chem, alt,                &
!         h2oai, h2oaj, refindx, radius_wet, number_bin,               &
!         radius_core, refindx_core, refindx_shell,                    &
          h2oai, h2oaj, radius_core,radius_wet, number_bin,            &
          swrefindx,swrefindx_core, swrefindx_shell,                   &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                   &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
   CASE (CB05_SORG_VBS_AQ_KPP)
     call optical_prep_modal_vbs(nbin_o, chem, alt,                        &
!         h2oai, h2oaj, refindx, radius_wet, number_bin,               &
!         radius_core, refindx_core, refindx_shell,                    &
          h2oai, h2oaj, radius_core,radius_wet, number_bin,               &
          swrefindx,swrefindx_core, swrefindx_shell,                    &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                    &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
   CASE (CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, &
         CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ  )
     call optical_prep_mam(nbin_o, chem, alt,                          &
          radius_core,radius_wet, number_bin,                          &
          swrefindx,swrefindx_core, swrefindx_shell,                   &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                   &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
!    call mieaer_modal()
!
   CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN  , CBMZ_MOSAIC_KPP,        &
         CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ,                     &
         CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN,                   &
         CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ,             &
         SAPRC99_MOSAIC_4BIN_VBS2_KPP,                                 &
         MOZART_MOSAIC_4BIN_KPP,MOZART_MOSAIC_4BIN_AQ_KPP,   &
         CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP,         &
         SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP)!BSINGH(12/05/2013): Added for SAPRC 8 bin vbs and non-aq on (04/07/2014)
     call optical_prep_sectional(nbin_o, chem, alt,                    &
          totoa_a01,totoa_a02,totoa_a03,totoa_a04,                     &
          totoa_a05,totoa_a06,totoa_a07,totoa_a08,                     &
!         refindx, radius_wet, number_bin,                             &
!         radius_core, refindx_core, refindx_shell,                    &
          radius_core, radius_wet, number_bin,                             &
          swrefindx,swrefindx_core,swrefindx_shell,                    &
          lwrefindx,lwrefindx_core,lwrefindx_shell,                    &
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
   CASE (GOCART_SIMPLE, GOCARTRACM_KPP, GOCARTRADM2,  &
         MOZCART_KPP,T1_MOZCART_KPP                                    )
     call optical_prep_gocart(nbin_o, chem, alt,relhum,                &
          radius_core,radius_wet, number_bin,                          &
          swrefindx,swrefindx_core, swrefindx_shell,                   &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                   &
          uoc_flag,                                                    & ! mklose
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
!gocart is now wavelength dependent --SAM  4/25/11
!and for shortwave and longwave - similar to modal --SAM  4/25/11
!    call optical_prep_gocart(nbin_o, chem, alt,relhum,                &
!                       refindx0, radius_wet, number_bin,               &
!         radius_core, refindx_core0, refindx_shell0,                    &
!         ids,ide, jds,jde, kds,kde,                                   &
!         ims,ime, jms,jme, kms,kme,                                   &
!         its,ite, jts,jte, kts,kte                                    )
!         do ns=1,nspint
!         swrefindx(:,:,:,:,ns)=refindx0
!         swrefindx_core(:,:,:,:,ns)=refindx_core0
!         swrefindx_shell(:,:,:,:,ns)=refindx_shell0
!         lwrefindx=0.0
!         lwrefindx_core=0.0
!         lwrefindx_shell=0.0
!         enddo
   END SELECT chem_select
     do jclm = jts, jte
     do iclm = its, ite
       do k = kts, kte
          dz(k) = dz8w(iclm, k, jclm)   ! cell depth (m)
       end do
       do k = kts, kte
       do isize = 1, nbin_o
          number_bin_col(isize,k) = number_bin(iclm,k,jclm,isize)
          radius_wet_col(isize,k) = radius_wet(iclm,k,jclm,isize)
!         refindx_col(isize,k)    = refindx(iclm,k,jclm,isize)
!         refr=real(refindx_col(isize,k))
          swrefindx_col(isize,k,:)    = swrefindx(iclm,k,jclm,isize,:)
          swrefindx_col1(isize,k)    = swrefindx(iclm,k,jclm,isize,3)  ! at 600 nm
          lwrefindx_col(isize,k,:)    = lwrefindx(iclm,k,jclm,isize,:)
          radius_core_col(isize,k)   = radius_core(iclm,k,jclm,isize)
!         refindx_core_col(isize,k)  = refindx_core(iclm,k,jclm,isize)
!         refindx_shell_col(isize,k) = refindx_shell(iclm,k,jclm,isize)
          swrefindx_core_col(isize,k,:)  = swrefindx_core(iclm,k,jclm,isize,:)
          swrefindx_shell_col(isize,k,:) = swrefindx_shell(iclm,k,jclm,isize,:)
          swrefindx_core_col1(isize,k)  = swrefindx_core(iclm,k,jclm,isize,3)
          swrefindx_shell_col1(isize,k) = swrefindx_shell(iclm,k,jclm,isize,3)
          lwrefindx_core_col(isize,k,:)  = lwrefindx_core(iclm,k,jclm,isize,:)
          lwrefindx_shell_col(isize,k,:) = lwrefindx_shell(iclm,k,jclm,isize,:)

! JCB, Feb. 20, 2008:  in the case of shell/core and the use of the Mie 
! routine, set the refractive index of the shell used in the printout
! equal to the actual refractive index of the shell
          if(option_method.eq.3.and.option_mie.eq.2) &
!              refindx_col(isize,k) = refindx_shell(iclm,k,jclm,isize) ! JCB
               swrefindx_col(isize,k,:) = swrefindx_shell(iclm,k,jclm,isize,:) ! JCB
               swrefindx_col1(isize,k) = swrefindx_shell(iclm,k,jclm,isize,3) ! JCB

! JCB, Feb. 20, 2008:  set core radius = 0 for very small cores; this
! prevents problems with full-blown Mie calculations that do not deal
! well with very small cores.  For very small cores, the amount of
! absorption is negligible, and therefore setting the core radius to zero
! has virtually no effect on calculated optical properties
          if(radius_wet_col(isize,k) < 1e-20) then
               radius_core_col(isize,k)=0.0             
          else if(radius_core_col(isize,k)/radius_wet_col(isize,k)**3.le.0.0001) then
               radius_core_col(isize,k)=0.0  ! JCB
          end if
       enddo
       enddo

       if (option_method .eq. 2) then
          do k = kts, kte
          do isize = 1, nbin_o
          do ns=1,nspint
           fv = (radius_core_col(isize,k)/radius_wet_col(isize,k))**3 ! volume fraction
!          aa=(refindx_core_col(isize,k)**2+2.0*refindx_shell(iclm,k,jclm,isize)**2)
!          bb=fv*(refindx_core_col(isize,k)**2-refindx_shell(iclm,k,jclm,isize)**2)
!          refindx_col(isize,k)= refindx_shell(iclm,k,jclm,isize)*sqrt((aa+2.0*bb)/(aa-bb))
!          refr=real(refindx_col(isize,k))
           aa=(swrefindx_core_col(isize,k,ns)**2+2.0*swrefindx_shell(iclm,k,jclm,isize,ns)**2)
           bb=fv*(swrefindx_core_col(isize,k,ns)**2-swrefindx_shell(iclm,k,jclm,isize,ns)**2)
           swrefindx_col(isize,k,ns)= swrefindx_shell(iclm,k,jclm,isize,ns)*sqrt((aa+2.0*bb)/(aa-bb))
           if (ns==3) then 
           swrefindx_col1(isize,k)= swrefindx_shell(iclm,k,jclm,isize,ns)*sqrt((aa+2.0*bb)/(aa-bb))
           endif
           !refr=real(refindx_col(isize,k))
          enddo
          enddo
          enddo
       endif

       if (option_method .le. 2) then
          do k = kts, kte
          do isize = 1, nbin_o
             radius_core_col(isize,k) = 0.0
!            refindx_core_col(isize,k) = cmplx(0.0,0.0)
             swrefindx_core_col(isize,k,:) = cmplx(0.0,0.0)
             swrefindx_core_col1(isize,k) = cmplx(0.0,0.0)
           enddo
          enddo
      endif
!
!!$       if(id.eq.1.and.iclm.eq.84.and.jclm.eq.52) then
!!$         print*,'jdf printout 1'
!!$         do isize = 1, nbin_o
!!$            write(*,888) isize,number_bin_col(isize,1),                &
!!$                   radius_wet_col(isize,1),radius_core_col(isize,1),   &
!!$                   real(refindx_col(isize,1)),                         &
!!$                   imag(refindx_col(isize,1)),                         &
!!$                   real(refindx_core_col(isize,1)),                    &
!!$                   imag(refindx_core_col(isize,1)),dz(1)
!!$         enddo
!!$       endif
!!$       if(id.eq.2.and.iclm.eq.59.and.jclm.eq.63) then
!!$         print*,'jdf printout 2'
!!$         do isize = 1, nbin_o
!!$            write(*,888) isize,number_bin_col(isize,1),                &
!!$                   radius_wet_col(isize,1),radius_core_col(isize,1),   &
!!$                   real(refindx_col(isize,1)),                         &
!!$                   imag(refindx_col(isize,1)),                         &
!!$                   real(refindx_core_col(isize,1)),                    &
!!$                   imag(refindx_core_col(isize,1)),dz(1)
!!$         enddo
!!$       endif
!!$  888  format(i3,9e12.5)
!
! Initialize LW vars as not all options compute it
       lwtauaer(:,:)=1.e-20
       lwextaer(:,:)=1.e-20

       if (option_mie .eq. 1) then 
          call mieaer(id, iclm, jclm, nbin_o,                          &
!            number_bin_col, radius_wet_col, refindx_col,              &
             number_bin_col, radius_wet_col,swrefindx_col,             &
             lwrefindx_col,     &
             dz, curr_secs, kts, kte,                                  &
!            sizeaer, extaer, waer, gaer, tauaer,                      &
             swsizeaer,swextaer,swwaer,swgaer,swtauaer,                &
             lwextaer,lwtauaer,                &
             l2, l3, l4, l5, l6, l7,swbscoef                            )
       endif
       if (option_mie .ge. 2 .and. option_method .le. 2) then 
          call mieaer_sc(id, iclm, jclm, nbin_o,                       &
!            number_bin_col, radius_wet_col, refindx_col,              &
!            radius_core_col, refindx_core_col,                        &
             number_bin_col, radius_wet_col, swrefindx_col1,              &
             radius_core_col, swrefindx_core_col1,                        &
             dz, curr_secs, kte,                                       &
!            sizeaer, extaer, waer, gaer, tauaer,                      &
!            l2, l3, l4, l5, l6, l7, bscoef                            )
             swsizeaer, swextaer, swwaer, swgaer, swtauaer,                      &
             l2, l3, l4, l5, l6, l7, swbscoef                            )
       endif
       if (option_mie .ge. 2 .and. option_method .eq. 3) then 
          call mieaer_sc(id, iclm, jclm, nbin_o,                       &
!            number_bin_col, radius_wet_col, refindx_shell_col,        &
!            radius_core_col, refindx_core_col,                        &
!            dz, curr_secs, kte,                                       &
!            sizeaer, extaer, waer, gaer, tauaer,                      &
!            l2, l3, l4, l5, l6, l7, bscoef                            )
             number_bin_col, radius_wet_col, swrefindx_shell_col1,        &
             radius_core_col, swrefindx_core_col1,                        &
             dz, curr_secs, kte,                                       &
             swsizeaer, swextaer, swwaer, swgaer, swtauaer,                      &
             l2, l3, l4, l5, l6, l7, swbscoef                            )
       endif
!

       do k=kts,kte
!jdf
!        write( msg, '(a, 5i4)' )	&
!                 'jdf sw k', k, kts, kte, iclm, jclm
!                 call peg_message( lunerr, msg )
!jdf
         !shortwave
!        tauaersw(iclm,k,jclm,:) = swtauaer(:,k)
!        extaersw(iclm,k,jclm,:) = swextaer(:,k)
!        gaersw(iclm,k,jclm,:)   = swgaer(:,k)
!        waersw(iclm,k,jclm,:)   = swwaer(:,k)
!        bscoefsw(iclm,k,jclm,:) = swbscoef(:,k)
         do ns=1,nspint
          tauaersw(iclm,k,jclm,ns) = amax1(swtauaer(ns,k),1.e-20)
          extaersw(iclm,k,jclm,ns) = amax1(swextaer(ns,k),1.e-20)
          gaersw(iclm,k,jclm,ns)   = amax1(amin1(swgaer(ns,k),1.0-1.e-8),1.e-20)
          waersw(iclm,k,jclm,ns)   = amax1(amin1(swwaer(ns,k),1.0-1.e-8),1.e-20)
          bscoefsw(iclm,k,jclm,ns) = amax1(swbscoef(ns,k),1.e-20)
         enddo
         l2aer(iclm,k,jclm,:) = l2(:,k)
         l3aer(iclm,k,jclm,:) = l3(:,k)
         l4aer(iclm,k,jclm,:) = l4(:,k)
         l5aer(iclm,k,jclm,:) = l5(:,k)
         l6aer(iclm,k,jclm,:) = l6(:,k)
         l7aer(iclm,k,jclm,:) = l7(:,k)

         !longwave
!        tauaerlw(iclm,k,jclm,1:nlwbands) = lwtauaer(1:nlwbands,k)
!        extaerlw(iclm,k,jclm,1:nlwbands) = lwextaer(1:nlwbands,k)
         do ns=1,nlwbands
          tauaerlw(iclm,k,jclm,ns) = amax1(lwtauaer(ns,k),1.e-20)
          extaerlw(iclm,k,jclm,ns) = amax1(lwextaer(ns,k),1.e-20)
         enddo

       enddo
!!$       if(id.eq.1.and.iclm.eq.84.and.jclm.eq.52) then
!!$          write(*,889) sizeaer(1,1),sizeaer(2,1),sizeaer(3,1),sizeaer(4,1)
!!$          write(*,889) extaer(1,1),extaer(2,1),extaer(3,1),extaer(4,1)
!!$          write(*,889) waer(1,1),waer(2,1),waer(3,1),waer(4,1)
!!$          write(*,889) gaer(1,1),gaer(2,1),gaer(3,1),gaer(4,1)
!!$          write(*,889) tauaer(1,1),tauaer(2,1),tauaer(3,1),tauaer(4,1)
!!$          write(*,889) bscoef(1,1),bscoef(2,1),bscoef(3,1),bscoef(4,1)
!!$          write(*,889) l2(1,1),l2(2,1),l2(3,1),l2(4,1)
!!$          write(*,889) l3(1,1),l3(2,1),l3(3,1),l3(4,1)
!!$          write(*,889) l4(1,1),l4(2,1),l4(3,1),l4(4,1)
!!$          write(*,889) l5(1,1),l5(2,1),l5(3,1),l5(4,1)
!!$          write(*,889) l6(1,1),l6(2,1),l6(3,1),l6(4,1)
!!$          write(*,889) l7(1,1),l7(2,1),l7(3,1),l7(4,1)
!!$       endif
!!$       if(id.eq.2.and.iclm.eq.59.and.jclm.eq.63) then
!!$          write(*,889) sizeaer(1,1),sizeaer(2,1),sizeaer(3,1),sizeaer(4,1)
!!$          write(*,889) extaer(1,1),extaer(2,1),extaer(3,1),extaer(4,1)
!!$          write(*,889) waer(1,1),waer(2,1),waer(3,1),waer(4,1)
!!$          write(*,889) gaer(1,1),gaer(2,1),gaer(3,1),gaer(4,1)
!!$          write(*,889) tauaer(1,1),tauaer(2,1),tauaer(3,1),tauaer(4,1)
!!$          write(*,889) bscoef(1,1),bscoef(2,1),bscoef(3,1),bscoef(4,1)
!!$          write(*,889) l2(1,1),l2(2,1),l2(3,1),l2(4,1)
!!$          write(*,889) l3(1,1),l3(2,1),l3(3,1),l3(4,1)
!!$          write(*,889) l4(1,1),l4(2,1),l4(3,1),l4(4,1)
!!$          write(*,889) l5(1,1),l5(2,1),l5(3,1),l5(4,1)
!!$          write(*,889) l6(1,1),l6(2,1),l6(3,1),l6(4,1)
!!$          write(*,889) l7(1,1),l7(2,1),l7(3,1),l7(4,1)
!!$       endif
!!$  889  format(4e12.5)
     enddo
     enddo
!
      return
!
      end subroutine optical_averaging
!----------------------------------------------------------------------------------
! This subroutine computes volume-averaged refractive index and wet radius needed 
! by the mie calculations. Aerosol number is also passed into the mie calculations
! in terms of other units.
!
      subroutine optical_prep_sectional(nbin_o, chem, alt,       &
        totoa_a01, totoa_a02, totoa_a03, totoa_a04,              &
        totoa_a05,totoa_a06,totoa_a07,totoa_a08,                 &
!       refindx, radius_wet, number_bin,                         &
!       radius_core, refindx_core,  refindx_shell,               &
        radius_core,radius_wet, number_bin,                         &
        swrefindx,swrefindx_core,swrefindx_shell,               &
        lwrefindx,lwrefindx_core,lwrefindx_shell,               &
        ids,ide, jds,jde, kds,kde,                               &
        ims,ime, jms,jme, kms,kme,                               &
        its,ite, jts,jte, kts,kte                                )
!
   USE module_configure
!  USE module_state_description
   USE module_model_constants
   USE module_data_mosaic_asect
   USE module_data_mosaic_other
   USE module_state_description, only:  param_first_scalar
!
   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt,                                          &
           totoa_a01, totoa_a02, totoa_a03, totoa_a04,                 &
           totoa_a05,totoa_a06,totoa_a07,totoa_a08
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
         INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
!  COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),           &
!        INTENT(OUT ) ::                                               &
!          refindx, refindx_core, refindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o,nswbands),      &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o,nlwbands),      &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell
!
! local variables
!
   integer i, j, k, l, isize, itype, iphase
   integer p1st
   complex  ref_index_lvcite  , ref_index_nh4hso4, &
            ref_index_nh4msa , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3   , ref_index_na2so4, &
            ref_index_na3hso4, ref_index_nahso4  , ref_index_namsa,  &
            ref_index_caso4  , ref_index_camsa2  , ref_index_cano3,  &
            ref_index_cacl2  , ref_index_caco3   , ref_index_h2so4,  &
            ref_index_hhso4  , ref_index_hno3    , ref_index_hcl,    &
            ref_index_msa    , ref_index_bc,     &
            ref_index_oin    , ref_index_aro1    , ref_index_aro2,   &
            ref_index_alk1   , ref_index_ole1    , ref_index_api1,   &
            ref_index_api2   , ref_index_im1     , ref_index_im2,    &
            ri_dum            , ri_ave_a
   COMPLEX, DIMENSION(nswbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    swref_index_nh4so4,swref_index_nacl,swref_index_oc,swref_index_dust,swref_index_h2o
   COMPLEX, DIMENSION(nlwbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    lwref_index_nh4so4,lwref_index_nacl,lwref_index_oc,lwref_index_dust,lwref_index_h2o
   real  dens_so4  , dens_no3  , dens_cl   , dens_msa  , dens_co3 ,  &
         dens_nh4  , dens_na   , dens_ca   , dens_oin  , dens_oc  ,  &
         dens_bc   , dens_aro1 , dens_aro2 , dens_alk1 , dens_ole1,  &
         dens_api1 , dens_api2 , dens_lim1 , dens_lim2 , dens_h2o,   & 
         dens_dust
   real  mass_so4  , mass_no3  , mass_cl   , mass_msa  , mass_co3 ,  &
         mass_nh4  , mass_na   , mass_ca   , mass_oin  , mass_oc  ,  &
         mass_bc   , mass_aro1 , mass_aro2 , mass_alk1 , mass_ole1,  &
         mass_api1 , mass_api2 , mass_lim1 , mass_lim2 , mass_h2o,   & 
         mass_dust
   real  vol_so4   , vol_no3   , vol_cl    , vol_msa   , vol_co3  ,  &
         vol_nh4   , vol_na    , vol_ca    , vol_oin   , vol_oc   ,  &
         vol_bc    , vol_aro1  , vol_aro2  , vol_alk1  , vol_ole1 ,  &
         vol_api1  , vol_api2  , vol_lim1  , vol_lim2  , vol_h2o,    & 
         vol_dust
   real  conv1a, conv1b
   real  mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
         dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a
   real  num_a_lo  , num_a_hi
   real  refr
   integer ns
!
! Define refractive indicies
! * assume na and cl are the same as nacl
! * assume so4, no3, and nh4 are the same as nh4no3
! * assume ca and co3 are the same as caco3
! * assume msa is just msa
! Further work:
! * to be more precise, need to compute electrolytes to apportion
!   so4, no3, nh4, na, cl, msa, ca, co3 among various componds
!   as was done previously in module_mosaic_therm.F
!
      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns) = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns) = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns) = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns) = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns) = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns) = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns) = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns) = cmplx(refrwlw(ns),refiwlw(ns))
      enddo
!     ref_index_nh4so4 = cmplx(1.52,0.)
      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     ! assumed
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)
!     ref_index_nacl   = cmplx(1.45,0.)
      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     ! assumed
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  ! assumed
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     ! assumed
!     ref_index_oc     = cmplx(1.45,0.)  ! JCB, Feb. 20, 2008: no complex part?
! JCB, Feb. 20, 2008:  set the refractive index of BC equal to the midpoint
! of ranges given in Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!      ref_index_bc     = cmplx(1.82,0.74), old value
      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  ! JCB, Feb. 20, 2008 
!     ref_index_dust    = cmplx(1.55,0.003)  ! czhao, dust, then this refractive index should be wavelength depedent 
      ref_index_aro1   = cmplx(1.45,0.)
      ref_index_aro2   = cmplx(1.45,0.)
      ref_index_alk1   = cmplx(1.45,0.)
      ref_index_ole1   = cmplx(1.45,0.)
      ref_index_api1   = cmplx(1.45,0.)
      ref_index_api2   = cmplx(1.45,0.)
      ref_index_im1   = cmplx(1.45,0.)
      ref_index_im2   = cmplx(1.45,0.)
!     ref_index_h2o    = cmplx(1.33,0.)
!
! densities in g/cc
!
      dens_so4   = 1.8        ! used
      dens_no3   = 1.8        ! used
      dens_cl    = 2.2        ! used
      dens_msa   = 1.8        ! used
      dens_co3   = 2.6        ! used
      dens_nh4   = 1.8        ! used
      dens_na    = 2.2        ! used
      dens_ca    = 2.6        ! used
      dens_oin   = 2.6        ! used
      dens_dust   = 2.6        ! used
      dens_oc    = 1.0        ! used
! JCB, Feb. 20, 2008:  the density of BC is updated to reflect values
! published by Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!      dens_bc    = 1.7        ! used, old value
      dens_bc    =  1.8        !  midpoint of Bond and Bergstrom value
      dens_aro1  = 1.0
      dens_aro2  = 1.0
      dens_alk1  = 1.0
      dens_ole1  = 1.0
      dens_api1  = 1.0
      dens_api2  = 1.0
      dens_lim1  = 1.0
      dens_lim2  = 1.0
      dens_h2o   = 1.0

      vol_so4    = 0.
      vol_no3    = 0.
      vol_cl     = 0.
      vol_msa    = 0.
      vol_co3    = 0.
      vol_nh4    = 0.
      vol_na     = 0.
      vol_ca     = 0.
      vol_oin    = 0.
      vol_oc     = 0.
      vol_bc     = 0.
      vol_aro1   = 0.
      vol_aro2   = 0.
      vol_alk1   = 0.
      vol_ole1   = 0.
      vol_api1   = 0.
      vol_api2   = 0.
      vol_lim1   = 0.
      vol_lim2   = 0.
      vol_h2o    = 0.
      vol_dust   = 0.
!
      p1st = param_first_scalar
!
!     do isize = 1, nbin_o
!     do j = jts, jte
!     do k = kts, kte
!     do i = its, ite
!       refindx(i,k,j,isize)=0.0
!       radius_wet(i,k,j,isize)=0.0
!       number_bin(i,k,j,isize)=0.0
!       radius_core(i,k,j,isize)=0.0
!       refindx_core(i,k,j,isize)=0.0
!       refindx_shell(i,k,j,isize)=0.0
!     enddo
!     enddo
!     enddo
!     enddo
        radius_wet=0.0
        number_bin=0.0
        radius_core=0.0
        swrefindx=0.0
        swrefindx_core=0.0
        swrefindx_shell=0.0
        lwrefindx=0.0
        lwrefindx_core=0.0
        lwrefindx_shell=0.0
!
! units:
! * mass     - g/cc(air)
! * number   - #/cc(air)
! * volume   - cc(air)/cc(air)
! * diameter - cm
!
      itype=1
      iphase=1
      do j = jts, jte
      do k = kts, kte
      do i = its, ite
        do isize = 1, nbin_o
          mass_so4 = 0.0
          mass_no3 = 0.0
          mass_nh4 = 0.0
          mass_oin = 0.0
          mass_dust = 0.0
          mass_oc = 0.0
          mass_bc = 0.0
          mass_na = 0.0
          mass_cl = 0.0
          mass_msa = 0.0
          mass_co3 = 0.0
          mass_ca = 0.0
          mass_h2o = 0.0
          mass_aro1=0.0
          mass_aro2=0.0
          mass_alk1=0.0
          mass_ole1=0.0
          mass_api1=0.0
          mass_api2=0.0
          mass_lim1=0.0
          mass_lim2=0.0 

! convert ug / kg dry air to g / cc  air
          conv1a = (1.0/alt(i,k,j)) * 1.0e-12
! convert # / kg dry air to # / cc  air
          conv1b = (1.0/alt(i,k,j)) * 1.0e-6
          l=lptr_so4_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_so4= chem(i,k,j,l)*conv1a
          l=lptr_no3_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_no3= chem(i,k,j,l)*conv1a
          l=lptr_nh4_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_nh4= chem(i,k,j,l)*conv1a
          l=lptr_oin_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_oin= chem(i,k,j,l)*conv1a
!
! if totoa from VBS available, use it
!jdfcz    l=lptr_dust_aer(isize,itype,iphase)
!jdfcz    if (l .ge. p1st)  mass_dust= chem(i,k,j,l)*conv1a
          l=lptr_oc_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_oc= chem(i,k,j,l)*conv1a
          if (totoa_a01(i,k,j) .gt. 1.0e-12 .and. isize .eq. 1) mass_oc=totoa_a01(i,k,j)*conv1a
          if (totoa_a02(i,k,j) .gt. 1.0e-12 .and. isize .eq. 2) mass_oc=totoa_a02(i,k,j)*conv1a
          if (totoa_a03(i,k,j) .gt. 1.0e-12 .and. isize .eq. 3) mass_oc=totoa_a03(i,k,j)*conv1a
          if (totoa_a04(i,k,j) .gt. 1.0e-12 .and. isize .eq. 4) mass_oc=totoa_a04(i,k,j)*conv1a
          if (totoa_a05(i,k,j) .gt. 1.0e-12 .and. isize .eq. 5) mass_oc=totoa_a05(i,k,j)*conv1a
          if (totoa_a06(i,k,j) .gt. 1.0e-12 .and. isize .eq. 6) mass_oc=totoa_a06(i,k,j)*conv1a
          if (totoa_a07(i,k,j) .gt. 1.0e-12 .and. isize .eq. 7) mass_oc=totoa_a07(i,k,j)*conv1a
          if (totoa_a08(i,k,j) .gt. 1.0e-12 .and. isize .eq. 8) mass_oc=totoa_a08(i,k,j)*conv1a
!
          l=lptr_bc_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_bc= chem(i,k,j,l)*conv1a
          l=lptr_na_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_na= chem(i,k,j,l)*conv1a
          l=lptr_cl_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_cl= chem(i,k,j,l)*conv1a
          l=lptr_msa_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_msa= chem(i,k,j,l)*conv1a
          l=lptr_co3_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_co3= chem(i,k,j,l)*conv1a
          l=lptr_ca_aer(isize,itype,iphase)
          if (l .ge. p1st)  mass_ca= chem(i,k,j,l)*conv1a
          l=waterptr_aer(isize,itype)
          if (l .ge. p1st)  mass_h2o= chem(i,k,j,l)*conv1a
          l=numptr_aer(isize,itype,iphase)
          if (l .ge. p1st)  num_a= chem(i,k,j,l)*conv1b
!
          vol_so4 = mass_so4 / dens_so4
          vol_no3 = mass_no3 / dens_no3
          vol_aro1= mass_aro1 / dens_aro1
          vol_aro2= mass_aro2 / dens_aro2
          vol_alk1= mass_alk1 / dens_alk1
          vol_ole1= mass_ole1 / dens_ole1
          vol_api1= mass_api1 / dens_api1
          vol_api2= mass_api2 / dens_api2
          vol_lim1= mass_lim1 / dens_lim1
          vol_lim2= mass_lim2 / dens_lim2
          vol_nh4 = mass_nh4 / dens_nh4
          vol_oin = mass_oin / dens_oin
!jdfcz    vol_dust = mass_dust / dens_dust
          vol_oc  = mass_oc  / dens_oc
          vol_bc  = mass_bc  / dens_bc
          vol_na  = mass_na  / dens_na
          vol_cl  = mass_cl  / dens_cl
          vol_msa = mass_msa / dens_msa
          vol_co3 = mass_co3 / dens_co3
          vol_ca  = mass_ca  / dens_ca
!         mass_h2o = 0.0 ! testing purposes only
          vol_h2o = mass_h2o / dens_h2o
          mass_dry_a = mass_so4 + mass_no3 + mass_nh4 + mass_oin + &
                       mass_oc  + mass_bc  + mass_na  + mass_cl  + &
                       mass_msa + mass_co3 + mass_ca + mass_aro1 + &
                       mass_aro2 + mass_alk1 + mass_ole1 +mass_api1 + &
!jdfcz                 mass_api2 + mass_lim1 + mass_lim2 + mass_dust
                       mass_api2 + mass_lim1 + mass_lim2
          mass_wet_a = mass_dry_a + mass_h2o 
          vol_dry_a  = vol_so4  + vol_no3  + vol_nh4  + vol_oin  + &
                       vol_oc   + vol_bc   + vol_na   + vol_cl   + &
                       vol_msa  + vol_co3  + vol_ca   + vol_aro1 + &
                       vol_aro2 + vol_alk1 + vol_ole1 + vol_api1 + &
!jdfcz                 vol_api2 + vol_lim1 + vol_lim2 + vol_dust
                       vol_api2 + vol_lim1 + vol_lim2

          vol_wet_a  = vol_dry_a + vol_h2o
          vol_shell  = vol_wet_a - vol_bc
! jdf: Adjustment of aerosol number if it falls outside of reasonable bounds.
! This is necessary since advection scheme will cause the mass and number
! prognostic equations to give inconsistent values, usually at sharp gradients
! of these quantities.
          num_a_lo=1.90985*vol_dry_a/(dlo_sect(isize,itype)**3)
          num_a_hi=1.90985*vol_dry_a/(dhi_sect(isize,itype)**3)

!czhao fixed the diameter when mass and number of aerosols drops to an extreme low values
        if (vol_dry_a.le.1.e-15.and.num_a.le.1.e-10) then  
          if(num_a.gt.num_a_lo) then
            num_a=num_a_lo
          elseif(num_a.lt.num_a_hi) then
            num_a=num_a_hi
          endif
          dp_dry_a   = dhi_sect(isize,itype) 
          dp_wet_a   = dhi_sect(isize,itype) 
          dp_bc_a    = dhi_sect(isize,itype) 
        else 
          if(num_a.gt.num_a_lo) then
            num_a=num_a_lo
          elseif(num_a.lt.num_a_hi) then
            num_a=num_a_hi
          endif
!
          dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
          dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
          dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
        endif


          !shortwave 
         do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns) * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns) * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (ref_index_caco3  * mass_ca  / dens_ca) +   &
                       (ref_index_caco3  * mass_co3 / dens_co3) +  &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o) 
          ri_ave_a   = ri_dum/vol_wet_a
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns) * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (ref_index_caco3  * mass_ca  / dens_ca) +   &
                       (ref_index_caco3  * mass_co3 / dens_co3) +  &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o) 
          if(dp_wet_a/2.0 .lt. dlo_sect(isize,itype)/2.0) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_sect(isize,itype)/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
!           refindx_core(i,k,j,isize) = ref_index_bc
!           refindx_shell(i,k,j,isize) = ref_index_oin
          elseif(vol_shell .lt. 1.0e-20) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_sect(isize,itype)/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
!           refindx_core(i,k,j,isize) = ref_index_bc
!           refindx_shell(i,k,j,isize) = ref_index_oin
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
!           refindx(i,k,j,isize)    =ri_ave_a
            swrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
!           refindx_core(i,k,j,isize) =ref_index_bc
!           refindx_shell(i,k,j,isize) =ri_dum/vol_shell
            swrefindx_core(i,k,j,isize,ns) =ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
!         refr=real(refindx(i,k,j,isize))
        enddo  ! ns shortwave 

        ! longwave 
        do ns = 1, nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns) * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (ref_index_caco3  * mass_ca  / dens_ca) +   &
                       (ref_index_caco3  * mass_co3 / dens_co3) +  &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
          ri_ave_a   = ri_dum/vol_wet_a
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns) * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (ref_index_caco3  * mass_ca  / dens_ca) +   &
                       (ref_index_caco3  * mass_co3 / dens_co3) +  &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
          if(dp_wet_a/2.0 .lt. dlo_sect(isize,itype)/2.0) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_sect(isize,itype)/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(vol_shell .lt. 1.0e-20) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_sect(isize,itype)/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            lwrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            lwrefindx_core(i,k,j,isize,ns) =ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
!          refr=real(refindx(i,k,j,isize))
        enddo ! ns longwave

        enddo ! isize 
      enddo ! i
      enddo ! j
      enddo ! k

      return

      end subroutine optical_prep_sectional
!
!----------------------------------------------------------------------------------
! This subroutine computes volume-averaged refractive index and wet radius needed 
! by the mie calculations. Aerosol number is also passed into the mie calculations
! in terms of other units.
!
      subroutine optical_prep_modal(nbin_o, chem, alt,           &
!       h2oai, h2oaj, refindx, radius_wet, number_bin,           &
!       radius_core, refindx_core, refindx_shell,                &
        h2oai, h2oaj, radius_core,radius_wet, number_bin,           &
        swrefindx, swrefindx_core, swrefindx_shell,                &
        lwrefindx, lwrefindx_core, lwrefindx_shell,                &
        ids,ide, jds,jde, kds,kde,                               &
        ims,ime, jms,jme, kms,kme,                               &
        its,ite, jts,jte, kts,kte                                )
!
   USE module_configure
!  USE module_state_description
   USE module_model_constants
   USE module_state_description, only:  param_first_scalar
   USE module_data_sorgam
!
   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt, h2oai, h2oaj
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
         INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
!  COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),           &
!        INTENT(OUT ) ::                                               &
!          refindx, refindx_core, refindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nswbands),   &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nlwbands),   &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell
!
! local variables
!
   integer i, j, k, l, isize, itype, iphase
   integer p1st
   complex  ref_index_lvcite  , ref_index_nh4hso4, &
            ref_index_nh4msa , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3   , ref_index_na2so4, &
            ref_index_na3hso4, ref_index_nahso4  , ref_index_namsa,  &
            ref_index_caso4  , ref_index_camsa2  , ref_index_cano3,  &
            ref_index_cacl2  , ref_index_caco3   , ref_index_h2so4,  &
            ref_index_hhso4  , ref_index_hno3    , ref_index_hcl,    &
            ref_index_msa    , ref_index_bc,     &
            ref_index_oin    , ref_index_aro1    , ref_index_aro2,   &
            ref_index_alk1   , ref_index_ole1    , ref_index_api1,   &
            ref_index_api2   , ref_index_lim1    , ref_index_lim2,    &
            ri_dum            , ri_ave_a
   COMPLEX, DIMENSION(nswbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    swref_index_oc , swref_index_dust , swref_index_nh4so4, swref_index_nacl,swref_index_h2o
   COMPLEX, DIMENSION(nlwbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    lwref_index_oc , lwref_index_dust , lwref_index_nh4so4, lwref_index_nacl,lwref_index_h2o

   real  dens_so4  , dens_no3  , dens_cl   , dens_msa  , dens_co3 ,  &
         dens_nh4  , dens_na   , dens_ca   , dens_oin  , dens_oc  ,  &
         dens_bc   , dens_aro1 , dens_aro2 , dens_alk1 , dens_ole1,  &
         dens_api1 , dens_api2 , dens_lim1 , dens_lim2 , dens_h2o ,  &
         dens_dust
   real  mass_so4  , mass_no3  , mass_cl   , mass_msa  , mass_co3 ,  &
         mass_nh4  , mass_na   , mass_ca   , mass_oin  , mass_oc  ,  &
         mass_bc   , mass_aro1 , mass_aro2 , mass_alk1 , mass_ole1,  &
         mass_api1 , mass_api2 , mass_lim1 , mass_lim2 , mass_h2o,   &
         mass_dust
   real  mass_so4i , mass_no3i , mass_cli  , mass_msai , mass_co3i,  &
         mass_nh4i , mass_nai  , mass_cai  , mass_oini , mass_oci ,  &
         mass_bci  , mass_aro1i, mass_aro2i, mass_alk1i, mass_ole1i, &
         mass_ba1i , mass_ba2i,  mass_ba3i , mass_ba4i , mass_pai,   &
         mass_h2oi , mass_dusti
   real  mass_so4j , mass_no3j , mass_clj  , mass_msaj , mass_co3j,  &
         mass_nh4j , mass_naj  , mass_caj  , mass_oinj , mass_ocj ,  &
         mass_bcj  , mass_aro1j, mass_aro2j, mass_alk1j, mass_ole1j, &
         mass_ba1j , mass_ba2j,  mass_ba3j , mass_ba4j , mass_paj,   &
         mass_h2oj , mass_dustj
   real  mass_antha, mass_seas, mass_soil
   real  num_ai, num_aj, num_ac,  vol_ai, vol_aj, vol_ac
   real  vol_so4   , vol_no3   , vol_cl    , vol_msa   , vol_co3  ,  &
         vol_nh4   , vol_na    , vol_ca    , vol_oin   , vol_oc   ,  &
         vol_bc    , vol_aro1  , vol_aro2  , vol_alk1  , vol_ole1 ,  &
         vol_api1  , vol_api2  , vol_lim1  , vol_lim2  , vol_h2o  ,  & 
         vol_dust
   real  conv1a, conv1b
   real  mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
         dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a
   real  ifac, jfac, cfac
   real  refr
   integer ns
   real  dgnum_um,drydens,duma,dlo_um,dhi_um,dgmin,sixpi,ss1,ss2,ss3,dtemp
   integer  iflag
   real, dimension(1:nbin_o) :: xnum_secti,xnum_sectj,xnum_sectc
   real, dimension(1:nbin_o) :: xmas_secti,xmas_sectj,xmas_sectc
   real, dimension(1:nbin_o) :: xdia_um, xdia_cm
!
!  real  sginin,sginia,sginic from module_data_sorgam.F
! 
! Mass from modal distribution is divided into individual sections before
! being passed back into the Mie routine.
! * currently use the same size bins as 8 default MOSAIC size bins
! * dlo_um and dhi_um define the lower and upper bounds of individual sections
!   used to compute optical properties
! * sigmas for 3 modes taken from module_sorgan_data.F
! * these parameters are needed by sect02 that is called later
! * sginin=1.7, sginia=2.0, sginic=2.5
!
      sixpi=6.0/3.14159265359
      dlo_um=0.0390625
      dhi_um=10.0
      drydens=1.8
      iflag=2
      duma=1.0
      dgmin=1.0e-07 ! in (cm)
      dtemp=dlo_um
      do isize=1,nbin_o
        xdia_um(isize)=(dtemp+dtemp*2.0)/2.0
        dtemp=dtemp*2.0
      enddo
!
! Define refractive indicies
! * assume na and cl are the same as nacl
! * assume so4, no3, and nh4 are the same as nh4no3
! * assume ca and co3 are the same as caco3
! * assume msa is just msa
! Further work:
! * to be more precise, need to compute electrolytes to apportion
!   so4, no3, nh4, na, cl, msa, ca, co3 among various componds
!   as was done previously in module_mosaic_therm.F
!
      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns) = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns) = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns) = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns) = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns) = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns) = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns) = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns) = cmplx(refrwlw(ns),refiwlw(ns))
      enddo

!     ref_index_nh4so4 = cmplx(1.52,0.)
      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     ! assumed
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)
!     ref_index_nacl   = cmplx(1.45,0.)
      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     ! assumed
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  ! assumed
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     ! assumed
!     ref_index_oc     = cmplx(1.45,0.)  ! JCB, Feb. 20, 2008: no complex part?
! JCB, Feb. 20, 2008:  set the refractive index of BC equal to the
! midpoint of ranges given in Bond and Bergstrom, Light absorption by
! carboneceous particles: an investigative review 2006, Aerosol Sci.
! and Tech., 40:27-67.
!     ref_index_bc     = cmplx(1.82,0.74) old value
      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  ! JCB, Feb. 20, 2008: "other inorganics" 
!     ref_index_dust   = cmplx(1.55,0.003)  ! czhao, this refractive index should be wavelength depedent
      ref_index_aro1   = cmplx(1.45,0.)
      ref_index_aro2   = cmplx(1.45,0.)
      ref_index_alk1   = cmplx(1.45,0.)
      ref_index_ole1   = cmplx(1.45,0.)
      ref_index_api1   = cmplx(1.45,0.)
      ref_index_api2   = cmplx(1.45,0.)
      ref_index_lim1   = cmplx(1.45,0.)
      ref_index_lim2   = cmplx(1.45,0.)
!     ref_index_h2o    = cmplx(1.33,0.)
!
! densities in g/cc
!
      dens_so4   = 1.8        ! used
      dens_no3   = 1.8        ! used
      dens_cl    = 2.2        ! used
      dens_msa   = 1.8        ! used
      dens_co3   = 2.6        ! used
      dens_nh4   = 1.8        ! used
      dens_na    = 2.2        ! used
      dens_ca    = 2.6        ! used
      dens_oin   = 2.6        ! used
      dens_dust  = 2.6        ! used
      dens_oc    = 1.0        ! used
! JCB, Feb. 20, 2008:  the density of BC is updated to reflect values
! published by Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!      dens_bc    = 1.7        ! used, old value
      dens_bc    =  1.8        !  midpoint of Bond and Bergstrom value
      dens_aro1  = 1.0
      dens_aro2  = 1.0
      dens_alk1  = 1.0
      dens_ole1  = 1.0
      dens_api1  = 1.0
      dens_api2  = 1.0
      dens_lim1  = 1.0
      dens_lim2  = 1.0
      dens_h2o   = 1.0
!
      p1st = param_first_scalar
!
      swrefindx=0.0
      lwrefindx=0.0
      radius_wet=0.0
      number_bin=0.0
      radius_core=0.0
      swrefindx_core=0.0
      swrefindx_shell=0.0
      lwrefindx_core=0.0
      lwrefindx_shell=0.0
!
! units:
! * mass     - g/cc(air)
! * number   - #/cc(air)
! * volume   - cc(air)/cc(air)
! * diameter - cm
!
      itype=1
      iphase=1
      do j = jts, jte
      do k = kts, kte
      do i = its, ite
        mass_so4i = 0.0
        mass_so4j = 0.0
        mass_no3i = 0.0
        mass_no3j = 0.0
        mass_nh4i = 0.0
        mass_nh4j = 0.0
        mass_oini = 0.0
        mass_oinj = 0.0
        mass_dusti = 0.0
        mass_dustj = 0.0
        mass_aro1i = 0.0
        mass_aro1j = 0.0
        mass_aro2i = 0.0
        mass_aro2j = 0.0
        mass_alk1i = 0.0
        mass_alk1j = 0.0
        mass_ole1i = 0.0
        mass_ole1j = 0.0
        mass_ba1i = 0.0
        mass_ba1j = 0.0
        mass_ba2i = 0.0
        mass_ba2j = 0.0
        mass_ba3i = 0.0
        mass_ba3j = 0.0
        mass_ba4i = 0.0
        mass_ba4j = 0.0
        mass_pai = 0.0
        mass_paj = 0.0
        mass_oci = 0.0
        mass_ocj = 0.0
        mass_bci = 0.0
        mass_bcj = 0.0
        mass_cai = 0.0
        mass_caj = 0.0
        mass_co3i = 0.0
        mass_co3j = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_msai = 0.0
        mass_msaj = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_h2oi = 0.0
        mass_h2oj = 0.0
        mass_antha = 0.0
        mass_seas = 0.0
        mass_soil = 0.0
        vol_aj = 0.0
        vol_ai = 0.0
        vol_ac = 0.0
        num_aj = 0.0
        num_ai = 0.0
        num_ac = 0.0

! convert ug / kg dry air to g / cc  air
        conv1a = (1.0/alt(i,k,j)) * 1.0e-12
! convert # / kg dry air to # / cc  air
        conv1b = (1.0/alt(i,k,j)) * 1.0e-6

! Accumulation mode...
!       isize = 1 ; itype = 1   ! before march-2008 ordering
        isize = 2 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4j= chem(i,k,j,l)*conv1a
        l=lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_no3j= chem(i,k,j,l)*conv1a
        l=lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nh4j= chem(i,k,j,l)*conv1a
        l=lptr_p25_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_oinj= chem(i,k,j,l)*conv1a
!jdfcz  l=lptr_dust_aer(isize,itype,iphase)
!jdfcz  if (l .ge. p1st)  mass_dustj= chem(i,k,j,l)*conv1a
        l=lptr_orgaro1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro1j= chem(i,k,j,l)*conv1a
        l=lptr_orgaro2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro2j= chem(i,k,j,l)*conv1a
        l=lptr_orgalk_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_alk1j= chem(i,k,j,l)*conv1a
        l=lptr_orgole_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ole1j= chem(i,k,j,l)*conv1a
        l=lptr_orgba1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba1j= chem(i,k,j,l)*conv1a
        l=lptr_orgba2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba2j= chem(i,k,j,l)*conv1a
        l=lptr_orgba3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba3j= chem(i,k,j,l)*conv1a
        l=lptr_orgba4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba4j= chem(i,k,j,l)*conv1a
        l=lptr_orgpa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_paj= chem(i,k,j,l)*conv1a
        l=lptr_ec_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bcj= chem(i,k,j,l)*conv1a
        l=lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_naj= chem(i,k,j,l)*conv1a
        l=lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_clj= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_aj= chem(i,k,j,l)*conv1b
        mass_h2oj= h2oaj(i,k,j) * 1.0e-12
        mass_ocj=mass_aro1j+mass_aro2j+mass_alk1j+mass_ole1j+ &
                 mass_ba1j+mass_ba2j+mass_ba3j+mass_ba4j+mass_paj

! Aitken mode...
!       isize = 1 ; itype = 2   ! before march-2008 ordering
        isize = 1 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4i= chem(i,k,j,l)*conv1a
        l=lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_no3i= chem(i,k,j,l)*conv1a
        l=lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nh4i= chem(i,k,j,l)*conv1a
        l=lptr_p25_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_oini= chem(i,k,j,l)*conv1a
!jdfcz  l=lptr_dust_aer(isize,itype,iphase)
!jdfcz  if (l .ge. p1st)  mass_dusti= chem(i,k,j,l)*conv1a
        l=lptr_orgaro1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro1i= chem(i,k,j,l)*conv1a
        l=lptr_orgaro2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro2i= chem(i,k,j,l)*conv1a
        l=lptr_orgalk_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_alk1i= chem(i,k,j,l)*conv1a
        l=lptr_orgole_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ole1i= chem(i,k,j,l)*conv1a
        l=lptr_orgba1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba1i= chem(i,k,j,l)*conv1a
        l=lptr_orgba2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba2i= chem(i,k,j,l)*conv1a
        l=lptr_orgba3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba3i= chem(i,k,j,l)*conv1a
        l=lptr_orgba4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba4i= chem(i,k,j,l)*conv1a
        l=lptr_orgpa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_pai= chem(i,k,j,l)*conv1a
        l=lptr_ec_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bci= chem(i,k,j,l)*conv1a
        l=lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nai= chem(i,k,j,l)*conv1a
        l=lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_cli= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_ai= chem(i,k,j,l)*conv1b
        mass_h2oi= h2oai(i,k,j) * 1.0e-12
        mass_oci=mass_aro1i+mass_aro2i+mass_alk1i+mass_ole1i+ &
                 mass_ba1i+mass_ba2i+mass_ba3i+mass_ba4i+mass_pai

! Coarse mode...
!       isize = 1 ; itype = 3   ! before march-2008 ordering
        isize = 1 ; itype = 2   ! after  march-2008 ordering
        l=lptr_anth_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_antha= chem(i,k,j,l)*conv1a
        l=lptr_seas_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_seas= chem(i,k,j,l)*conv1a
        l=lptr_soil_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_soil= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_ac= chem(i,k,j,l)*conv1b

        vol_ai = (mass_so4i/dens_so4)+(mass_no3i/dens_no3)+ &
                 (mass_nh4i/dens_nh4)+(mass_oini/dens_oin)+ &
                 (mass_aro1i/dens_oc)+(mass_alk1i/dens_oc)+ &
                 (mass_ole1i/dens_oc)+(mass_ba1i/dens_oc)+  &
                 (mass_ba2i/dens_oc)+(mass_ba3i/dens_oc)+   &
                 (mass_ba4i/dens_oc)+(mass_pai/dens_oc)+    &
                 (mass_aro2i/dens_oc)+(mass_bci/dens_bc)+   &
                 (mass_nai/dens_na)+(mass_cli/dens_cl)
!jdfcz           (mass_nai/dens_na)+(mass_cli/dens_cl) +    &
!jdfcz           (mass_dusti/dens_dust)
        vol_aj = (mass_so4j/dens_so4)+(mass_no3j/dens_no3)+ &
                 (mass_nh4j/dens_nh4)+(mass_oinj/dens_oin)+ &
                 (mass_aro1j/dens_oc)+(mass_alk1j/dens_oc)+ &
                 (mass_ole1j/dens_oc)+(mass_ba1j/dens_oc)+  &
                 (mass_ba2j/dens_oc)+(mass_ba3j/dens_oc)+   &
                 (mass_ba4j/dens_oc)+(mass_paj/dens_oc)+    &
                 (mass_aro2j/dens_oc)+(mass_bcj/dens_bc)+   &
                 (mass_naj/dens_na)+(mass_clj/dens_cl)
!jdfcz           (mass_naj/dens_na)+(mass_clj/dens_cl) +    &  
!jdfcz           (mass_dustj/dens_dust)
        vol_ac = (mass_antha/dens_oin)+ &
                 (mass_seas*(22.9898/58.4428)/dens_na)+ &
                 (mass_seas*(35.4530/58.4428)/dens_cl)+ &
                 (mass_soil/dens_dust)
!
! Now divide mass into sections which is done by sect02:
! * xmas_secti is for aiken mode
! * xmas_sectj is for accumulation mode
! * xmas_sectc is for coarse mode
! * sect02 expects input in um
! * pass in generic mass of 1.0 just to get a percentage distribution of mass among bins
!
        ss1=log(sginin)
        ss2=exp(ss1*ss1*36.0/8.0) 
        ss3=(sixpi*vol_ai/(num_ai*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginin,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_secti,xmas_secti)
        ss1=log(sginia)
        ss2=exp(ss1*ss1*36.0/8.0) 
        ss3=(sixpi*vol_aj/(num_aj*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginia,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectj,xmas_sectj)
        ss1=log(sginic)
        ss2=exp(ss1*ss1*36.0/8.0) 
        ss3=(sixpi*vol_ac/(num_ac*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginic,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectc,xmas_sectc)

        do isize = 1, nbin_o
          xdia_cm(isize)=xdia_um(isize)*1.0e-04
          mass_so4 = mass_so4i*xmas_secti(isize) + mass_so4j*xmas_sectj(isize)
          mass_no3 = mass_no3i*xmas_secti(isize) + mass_no3j*xmas_sectj(isize)
          mass_nh4 = mass_nh4i*xmas_secti(isize) + mass_nh4j*xmas_sectj(isize)
          mass_oin = mass_oini*xmas_secti(isize) + mass_oinj*xmas_sectj(isize) + &
                     mass_antha*xmas_sectc(isize)
!jdfcz    mass_dust= mass_dusti*xmas_secti(isize) + mass_dustj*xmas_sectj(isize) + &
!jdfcz               mass_soil*xmas_sectc(isize)
          mass_oc = (mass_pai+mass_aro1i+mass_aro2i+mass_alk1i+mass_ole1i+ &
                     mass_ba1i+mass_ba2i+mass_ba3i+mass_ba4i)*xmas_secti(isize) + &
                    (mass_paj+mass_aro1j+mass_aro2j+mass_alk1j+mass_ole1j+ &
                     mass_ba1j+mass_ba2j+mass_ba3j+mass_ba4j)*xmas_sectj(isize)
          mass_bc  = mass_bci*xmas_secti(isize) + mass_bcj*xmas_sectj(isize)
          mass_na  = mass_nai*xmas_secti(isize) + mass_naj*xmas_sectj(isize)+ &
                     mass_seas*xmas_sectc(isize)*(22.9898/58.4428)
          mass_cl  = mass_cli*xmas_secti(isize) + mass_clj*xmas_sectj(isize)+ &
                     mass_seas*xmas_sectc(isize)*(35.4530/58.4428)
          mass_h2o = mass_h2oi*xmas_secti(isize) + mass_h2oj*xmas_sectj(isize)
!         mass_h2o = 0.0 ! testing purposes only
          vol_so4 = mass_so4 / dens_so4
          vol_no3 = mass_no3 / dens_no3
          vol_nh4 = mass_nh4 / dens_nh4
          vol_oin = mass_oin / dens_oin
!jdfcz    vol_dust = mass_dust / dens_dust
          vol_oc  = mass_oc  / dens_oc
          vol_bc  = mass_bc  / dens_bc
          vol_na  = mass_na  / dens_na
          vol_cl  = mass_cl  / dens_cl
          vol_h2o = mass_h2o / dens_h2o
!!$          if(i.eq.50.and.j.eq.40.and.k.eq.1) then
!!$            print*,'jdf print bin',isize
!!$            print*,'so4',mass_so4,vol_so4
!!$            print*,'no3',mass_no3,vol_no3
!!$            print*,'nh4',mass_nh4,vol_nh4
!!$            print*,'oin',mass_oin,vol_oin
!!$!jdfcz      print*,'dust',mass_dust,vol_dust
!!$            print*,'oc ',mass_oc,vol_oc
!!$            print*,'bc ',mass_bc,vol_bc
!!$            print*,'na ',mass_na,vol_na
!!$            print*,'cl ',mass_cl,vol_cl
!!$            print*,'h2o',mass_h2o,vol_h2o
!!$          endif
          mass_dry_a = mass_so4 + mass_no3 + mass_nh4 + mass_oin + &
!jdfcz                 mass_oc  + mass_bc  + mass_na  + mass_cl  + mass_dust
                       mass_oc  + mass_bc  + mass_na  + mass_cl
          mass_wet_a = mass_dry_a + mass_h2o 
          vol_dry_a  = vol_so4  + vol_no3  + vol_nh4  + vol_oin  + &
!jdfcz                 vol_oc   + vol_bc   + vol_na   + vol_cl   + vol_dust
                       vol_oc   + vol_bc   + vol_na   + vol_cl
          vol_wet_a  = vol_dry_a + vol_h2o
          vol_shell  = vol_wet_a - vol_bc
          !num_a      = vol_wet_a / (0.52359877*xdia_cm(isize)*xdia_cm(isize)*xdia_cm(isize))
          !czhao 
          num_a      = num_ai*xnum_secti(isize)+num_aj*xnum_sectj(isize)+num_ac*xnum_sectc(isize) 


          !shortwave 
          do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)   * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns)   * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize) 
            dp_wet_a   = xdia_cm(isize) 
            dp_bc_a    = xdia_cm(isize) 
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)   * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns)   * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            swrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            swrefindx_core(i,k,j,isize,ns) =ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns shortwave

          !longwave 
          do ns=1,nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize) 
            dp_wet_a   = xdia_cm(isize) 
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            lwrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            lwrefindx_core(i,k,j,isize,ns) =ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns longwave

!          refr=real(refindx(i,k,j,isize))

        enddo  !isize
      enddo   !i
      enddo   !j
      enddo   !k

      return

      end subroutine optical_prep_modal

!!!! TUCCELLA
!----------------------------------------------------------------------------------

! 03/07/2014 added by Paolo Tuccella
!            It is a modification of optical_prep_modal subroutine for 
!            RACM_SOA_VBS_KPP aerosol model 

! This subroutine computes volume-averaged refractive index and wet radius
! needed 
! by the mie calculations. Aerosol number is also passed into the mie
! calculations
! in terms of other units.
!
  subroutine optical_prep_modal_soa_vbs(nbin_o, chem, alt,           &
!       h2oai, h2oaj, refindx, radius_wet, number_bin,           &
!       radius_core, refindx_core, refindx_shell,                &
        h2oai, h2oaj, radius_core,radius_wet, number_bin,        &
        swrefindx, swrefindx_core, swrefindx_shell,              &
        lwrefindx, lwrefindx_core, lwrefindx_shell,              &
        ids,ide, jds,jde, kds,kde,                               &
        ims,ime, jms,jme, kms,kme,                               &
        its,ite, jts,jte, kts,kte                                )
!
   USE module_configure
!  USE module_state_description
   USE module_model_constants
   USE module_state_description, only:  param_first_scalar
!   USE module_data_sorgam
   USE module_data_soa_vbs
!
   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt, h2oai, h2oaj
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
   INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
!  COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),           &
!        INTENT(OUT ) ::                                               &
!          refindx, refindx_core, refindx_shell
COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nswbands),      &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nlwbands),   &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell
!
! local variables
!
   integer i, j, k, l, isize, itype, iphase
   integer p1st
   complex  ref_index_lvcite  , ref_index_nh4hso4 ,                   &
            ref_index_nh4msa  , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3   , ref_index_na2so4  ,                   &
            ref_index_na3hso4 , ref_index_nahso4  , ref_index_namsa , &
            ref_index_caso4   , ref_index_camsa2  , ref_index_cano3 , &
            ref_index_cacl2   , ref_index_caco3   , ref_index_h2so4 , &
            ref_index_hhso4   , ref_index_hno3    , ref_index_hcl   , &
            ref_index_msa     , ref_index_bc      ,                   &
!            ref_index_oin     , ref_index_aro1    , ref_index_aro2  , &
!            ref_index_alk1    , ref_index_ole1    , ref_index_api1  , &
            ref_index_oin     , ref_index_soa1    , ref_index_soa2  , &
            ref_index_soa3    , ref_index_soa4    ,                   &
!            ref_index_api1  ,                                         &
!            ref_index_api2    , ref_index_lim1    , ref_index_lim2  , &
            ri_dum            , ri_ave_a
   COMPLEX, DIMENSION(nswbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    swref_index_oc , swref_index_dust , swref_index_nh4so4, swref_index_nacl,swref_index_h2o
   COMPLEX, DIMENSION(nlwbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    lwref_index_oc , lwref_index_dust , lwref_index_nh4so4, lwref_index_nacl,lwref_index_h2o

   real  dens_so4  , dens_no3  , dens_cl   , dens_msa  , dens_co3 ,  &
         dens_nh4  , dens_na   , dens_ca   , dens_oin  , dens_oc  ,  &
!         dens_bc   , dens_aro1 , dens_aro2 , dens_alk1 , dens_ole1,  &
!         dens_bc   , dens_soa1 , dens_soa2 , dens_soa3 , dens_soa4,  &
!         dens_api1 , dens_api2 , dens_lim1 , dens_lim2 , dens_h2o ,  &
         dens_bc   , dens_h2o  , dens_dust
   real  mass_so4  , mass_no3  , mass_cl   , mass_msa  , mass_co3 ,  &
         mass_nh4  , mass_na   , mass_ca   , mass_oin  , mass_oc  ,  &
         mass_bc   ,                                                 &
!         mass_aro1 , mass_aro2 , mass_alk1 , mass_ole1,              &
!         mass_api1 , mass_api2 , mass_lim1 , mass_lim2 , mass_h2o ,  &
         mass_h2o  , mass_dust
   real  mass_so4i , mass_no3i , mass_cli  , mass_msai , mass_co3i  ,   &
         mass_nh4i , mass_nai  , mass_cai  , mass_oini , mass_oci   ,   &
!         mass_bci  , mass_aro1i, mass_aro2i, mass_alk1i, mass_ole1i ,   &
!         mass_ba1i , mass_ba2i,  mass_ba3i , mass_ba4i , mass_pai   ,   &
         mass_bci  , mass_asoa1i, mass_asoa2i, mass_asoa3i, mass_asoa4i  , &
         mass_bsoa1i , mass_bsoa2i,  mass_bsoa3i , mass_bsoa4i , mass_pai, &
         mass_h2oi , mass_dusti
   real  mass_so4j , mass_no3j , mass_clj  , mass_msaj , mass_co3j,  &
         mass_nh4j , mass_naj  , mass_caj  , mass_oinj , mass_ocj ,  &
!         mass_bcj  , mass_aro1j, mass_aro2j, mass_alk1j, mass_ole1j, &
!         mass_ba1j , mass_ba2j,  mass_ba3j , mass_ba4j , mass_paj,   &
         mass_bcj  , mass_asoa1j, mass_asoa2j, mass_asoa3j, mass_asoa4j  , &
         mass_bsoa1j , mass_bsoa2j,  mass_bsoa3j , mass_bsoa4j , mass_paj, &
         mass_h2oj , mass_dustj
   real  mass_antha, mass_seas, mass_soil
   real  num_ai, num_aj, num_ac,  vol_ai, vol_aj, vol_ac
   real  vol_so4   , vol_no3   , vol_cl    , vol_msa   , vol_co3  ,  &
         vol_nh4   , vol_na    , vol_ca    , vol_oin   , vol_oc   ,  &
!         vol_bc    , vol_aro1  , vol_aro2  , vol_alk1  , vol_ole1 ,  &
!         vol_api1  , vol_api2  , vol_lim1  , vol_lim2  , vol_h2o  ,  &
         vol_bc    , vol_h2o   , vol_dust
   real  conv1a, conv1b
   real  mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
         dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a
   real  ifac, jfac, cfac
   real  refr
   integer ns
   real  dgnum_um,drydens,duma,dlo_um,dhi_um,dgmin,sixpi,ss1,ss2,ss3,dtemp
   integer  iflag
   real, dimension(1:nbin_o) :: xnum_secti,xnum_sectj,xnum_sectc
   real, dimension(1:nbin_o) :: xmas_secti,xmas_sectj,xmas_sectc
   real, dimension(1:nbin_o) :: xdia_um, xdia_cm

!
!  real  sginin,sginia,sginic from module_data_sorgam.F
! 
! Mass from modal distribution is divided into individual sections before
! being passed back into the Mie routine.
! * currently use the same size bins as 8 default MOSAIC size bins
! * dlo_um and dhi_um define the lower and upper bounds of individual sections
!   used to compute optical properties
! * sigmas for 3 modes taken from module_sorgan_data.F
! * these parameters are needed by sect02 that is called later
! * sginin=1.7, sginia=2.0, sginic=2.5
!
      sixpi=6.0/3.14159265359
      dlo_um=0.0390625
      dhi_um=10.0
      drydens=1.8
      iflag=2
      duma=1.0
      dgmin=1.0e-07 ! in (cm)
      dtemp=dlo_um
      do isize=1,nbin_o
        xdia_um(isize)=(dtemp+dtemp*2.0)/2.0
        dtemp=dtemp*2.0
      enddo
!
! Define refractive indicies
! * assume na and cl are the same as nacl
! * assume so4, no3, and nh4 are the same as nh4no3
! * assume ca and co3 are the same as caco3
! * assume msa is just msa
! Further work:
! * to be more precise, need to compute electrolytes to apportion
!   so4, no3, nh4, na, cl, msa, ca, co3 among various componds
!   as was done previously in module_mosaic_therm.F
!
      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns)     = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns)   = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns)   = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns)    = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns)     = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns)   = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns)   = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns)    = cmplx(refrwlw(ns),refiwlw(ns))
      enddo

!     ref_index_nh4so4 = cmplx(1.52,0.)
      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     ! assumed
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)
!     ref_index_nacl   = cmplx(1.45,0.)
      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     ! assumed
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  ! assumed
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     ! assumed
!     ref_index_oc     = cmplx(1.45,0.)  ! JCB, Feb. 20, 2008: no complex part?
! JCB, Feb. 20, 2008:  set the refractive index of BC equal to the
! midpoint of ranges given in Bond and Bergstrom, Light absorption by
! carboneceous particles: an investigative review 2006, Aerosol Sci.
! and Tech., 40:27-67.
!     ref_index_bc     = cmplx(1.82,0.74) old value
      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  ! JCB, Feb. 20, 2008: "other inorganics" 
!      ref_index_dust   = cmplx(1.55,0.003)  ! czhao, this refractive index
!      should be wavelength depedent
!      ref_index_aro1   = cmplx(1.45,0.)
!      ref_index_aro2   = cmplx(1.45,0.)
!      ref_index_alk1   = cmplx(1.45,0.)
!      ref_index_ole1   = cmplx(1.45,0.)
      ref_index_soa1   = cmplx(1.45,0.)
      ref_index_soa2   = cmplx(1.45,0.)
      ref_index_soa3   = cmplx(1.45,0.)
      ref_index_soa4   = cmplx(1.45,0.)
!      ref_index_api1   = cmplx(1.45,0.)
!      ref_index_api2   = cmplx(1.45,0.)
!      ref_index_lim1   = cmplx(1.45,0.)
!      ref_index_lim2   = cmplx(1.45,0.)
!      ref_index_h2o    = cmplx(1.33,0.)

!
! densities in g/cc
!
      dens_so4   = 1.8        ! used
      dens_no3   = 1.8        ! used
      dens_cl    = 2.2        ! used
      dens_msa   = 1.8        ! used
      dens_co3   = 2.6        ! used
      dens_nh4   = 1.8        ! used
      dens_na    = 2.2        ! used                          
      dens_ca    = 2.6        ! used
      dens_oin   = 2.6        ! used
      dens_dust  = 2.6        ! used
      !dens_oc    = 1.0        ! used
      dens_oc     = 1.6       ! used
! JCB, Feb. 20, 2008:  the density of BC is updated to reflect values
! published by Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!      dens_bc    = 1.7        ! used, old value
      dens_bc    =  1.8        !  midpoint of Bond and Bergstrom value
!      dens_aro1  = 1.0
!      dens_aro2  = 1.0
!      dens_alk1  = 1.0
!      dens_ole1  = 1.0
!      dens_api1  = 1.0
!      dens_api2  = 1.0
!      dens_lim1  = 1.0
!      dens_lim2  = 1.0
      dens_h2o   = 1.0
!
      p1st = param_first_scalar
!
      swrefindx=0.0
      lwrefindx=0.0
      radius_wet=0.0
      number_bin=0.0
      radius_core=0.0
      swrefindx_core=0.0
      swrefindx_shell=0.0
      lwrefindx_core=0.0
      lwrefindx_shell=0.0
!
! units:
! * mass     - g/cc(air)
! * number   - #/cc(air)
! * volume   - cc(air)/cc(air)
! * diameter - cm
!
      itype=1
      iphase=1

      do j = jts, jte
      do k = kts, kte
      do i = its, ite
        mass_so4i = 0.0
        mass_so4j = 0.0
        mass_no3i = 0.0
        mass_no3j = 0.0
        mass_nh4i = 0.0
        mass_nh4j = 0.0
        mass_oini = 0.0
        mass_oinj = 0.0
        mass_dusti = 0.0
        mass_dustj = 0.0
!        mass_aro1i = 0.0
!        mass_aro1j = 0.0
!        mass_aro2i = 0.0
!        mass_aro2j = 0.0
!        mass_alk1i = 0.0
!        mass_alk1j = 0.0
!        mass_ole1i = 0.0
!        mass_ole1j = 0.0
        mass_asoa1i= 0.0
        mass_asoa1j= 0.0
        mass_asoa2i= 0.0
        mass_asoa2j= 0.0
        mass_asoa3i= 0.0
        mass_asoa3j= 0.0
        mass_asoa4i= 0.0
        mass_asoa4j= 0.0
        mass_bsoa1i = 0.0
        mass_bsoa1j = 0.0
        mass_bsoa2i = 0.0
        mass_bsoa2j = 0.0
        mass_bsoa3i = 0.0
        mass_bsoa3j = 0.0
        mass_bsoa4i = 0.0
        mass_bsoa4j = 0.0
        mass_pai = 0.0
        mass_paj = 0.0
        mass_oci = 0.0
        mass_ocj = 0.0
        mass_bci = 0.0
        mass_bcj = 0.0
        mass_cai = 0.0
        mass_caj = 0.0
        mass_co3i = 0.0
        mass_co3j = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_msai = 0.0
        mass_msaj = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_h2oi = 0.0
        mass_h2oj = 0.0
        mass_antha = 0.0
        mass_seas = 0.0
        mass_soil = 0.0
        vol_aj = 0.0
        vol_ai = 0.0
        vol_ac = 0.0
        num_aj = 0.0
        num_ai = 0.0
        num_ac = 0.0

! convert ug / kg dry air to g / cc  air
        conv1a = (1.0/alt(i,k,j)) * 1.0e-12
! convert # / kg dry air to # / cc  air
        conv1b = (1.0/alt(i,k,j)) * 1.0e-6

! Accumulation mode...
!       isize = 1 ; itype = 1   ! before march-2008 ordering
        isize = 2 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4j= chem(i,k,j,l)*conv1a
        l=lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_no3j= chem(i,k,j,l)*conv1a
        l=lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nh4j= chem(i,k,j,l)*conv1a
        l=lptr_p25_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_oinj= chem(i,k,j,l)*conv1a
!jdfcz  l=lptr_dust_aer(isize,itype,iphase)
!jdfcz  if (l .ge. p1st)  mass_dustj= chem(i,k,j,l)*conv1a
!        l=lptr_orgaro1_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_aro1j= chem(i,k,j,l)*conv1a
!        l=lptr_orgaro2_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_aro2j= chem(i,k,j,l)*conv1a
!        l=lptr_orgalk_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_alk1j= chem(i,k,j,l)*conv1a
!        l=lptr_orgole_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ole1j= chem(i,k,j,l)*conv1a
!        l=lptr_orgba1_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba1j= chem(i,k,j,l)*conv1a
!        l=lptr_orgba2_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba2j= chem(i,k,j,l)*conv1a
!        l=lptr_orgba3_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba3j= chem(i,k,j,l)*conv1a
!        l=lptr_orgba4_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba4j= chem(i,k,j,l)*conv1a
        l=lptr_asoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa1j= chem(i,k,j,l)*conv1a
        l=lptr_asoa2_aer(isize,itype,iphase)
if (l .ge. p1st)  mass_asoa2j= chem(i,k,j,l)*conv1a
        l=lptr_asoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa3j= chem(i,k,j,l)*conv1a
        l=lptr_asoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa4j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa1j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa2j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa3j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa4j= chem(i,k,j,l)*conv1a
        l=lptr_orgpa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_paj= chem(i,k,j,l)*conv1a
        l=lptr_ec_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bcj= chem(i,k,j,l)*conv1a
        l=lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_naj= chem(i,k,j,l)*conv1a
        l=lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_clj= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_aj= chem(i,k,j,l)*conv1b
        mass_h2oj= h2oaj(i,k,j) * 1.0e-12
        mass_ocj=mass_asoa1j+mass_asoa2j+mass_asoa3j+mass_asoa4j+ &
                 mass_bsoa1j+mass_bsoa2j+mass_bsoa3j+mass_bsoa4j+mass_paj

! Aitken mode...
!       isize = 1 ; itype = 1   ! before march-2008 ordering
        isize = 1 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4i= chem(i,k,j,l)*conv1a
        l=lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_no3i= chem(i,k,j,l)*conv1a
        l=lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nh4i= chem(i,k,j,l)*conv1a
        l=lptr_p25_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_oini= chem(i,k,j,l)*conv1a
!jdfcz  l=lptr_dust_aer(isize,itype,iphase)
!jdfcz  if (l .ge. p1st)  mass_dusti= chem(i,k,j,l)*conv1a
!        l=lptr_orgaro1_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_aro1i= chem(i,k,j,l)*conv1a
!        l=lptr_orgaro2_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_aro2i= chem(i,k,j,l)*conv1a
!        l=lptr_orgalk_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_alk1i= chem(i,k,j,l)*conv1a
!        l=lptr_orgole_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ole1i= chem(i,k,j,l)*conv1a
!        l=lptr_orgba1_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba1i= chem(i,k,j,l)*conv1a
!        l=lptr_orgba2_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba2i= chem(i,k,j,l)*conv1a
!        l=lptr_orgba3_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba3i= chem(i,k,j,l)*conv1a
!        l=lptr_orgba4_aer(isize,itype,iphase)
!        if (l .ge. p1st)  mass_ba4i= chem(i,k,j,l)*conv1a
        l=lptr_asoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa1i= chem(i,k,j,l)*conv1a
        l=lptr_asoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa2i= chem(i,k,j,l)*conv1a
        l=lptr_asoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa3i= chem(i,k,j,l)*conv1a
        l=lptr_asoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_asoa4i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa1i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa2i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa3i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bsoa4i= chem(i,k,j,l)*conv1a
        l=lptr_orgpa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_pai= chem(i,k,j,l)*conv1a
        l=lptr_ec_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bci= chem(i,k,j,l)*conv1a
        l=lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nai= chem(i,k,j,l)*conv1a
        l=lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_cli= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_ai= chem(i,k,j,l)*conv1b
        mass_h2oi= h2oai(i,k,j) * 1.0e-12
        mass_oci=mass_asoa1i+mass_asoa2i+mass_asoa3i+mass_asoa4i+ &
                 mass_bsoa1i+mass_bsoa2i+mass_bsoa3i+mass_bsoa4i+mass_pai

! Coarse mode...
!       isize = 1 ; itype = 3   ! before march-2008 ordering
        isize = 1 ; itype = 2   ! after  march-2008 ordering
        l=lptr_anth_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_antha= chem(i,k,j,l)*conv1a
        l=lptr_seas_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_seas= chem(i,k,j,l)*conv1a
        l=lptr_soil_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_soil= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_ac= chem(i,k,j,l)*conv1b


        vol_ai = (mass_so4i/dens_so4) + (mass_no3i/dens_no3)  + &
                 (mass_nh4i/dens_nh4) + (mass_oini/dens_oin)  + &
                 (mass_asoa1i  + mass_asoa2i + &
                  mass_asoa3i  + mass_asoa4i + &
                  mass_bsoa1i  + mass_bsoa2i + &
                  mass_bsoa3i  + mass_bsoa4i + &
                  mass_pai)/dens_oc   + (mass_bci/dens_bc)    + &
                 (mass_nai/dens_na)   + (mass_cli/dens_cl)
!jdfcz           (mass_nai/dens_na)+(mass_cli/dens_cl) +    &
!jdfcz           (mass_dusti/dens_dust)

        vol_aj = (mass_so4j/dens_so4) + (mass_no3j/dens_no3)  + &
                 (mass_nh4j/dens_nh4) + (mass_oinj/dens_oin)  + &
                 (mass_asoa1j  +  mass_asoa2j + &
                  mass_asoa3j  +  mass_asoa4j + &
                  mass_bsoa1j  +  mass_bsoa2j + &
                  mass_bsoa3j  +  mass_bsoa4j + &
                  mass_paj)/dens_oc   + (mass_bcj/dens_bc)    + &
                 (mass_naj/dens_na)   + (mass_clj/dens_cl)
!jdfcz           (mass_naj/dens_na)+(mass_clj/dens_cl) +    &  
!jdfcz           (mass_dustj/dens_dust)
        vol_ac = (mass_antha/dens_oin)+ &
                 (mass_seas*(22.9898/58.4428)/dens_na)+ &
                 (mass_seas*(35.4530/58.4428)/dens_cl)+ &
                 (mass_soil/dens_dust)
!
! Now divide mass into sections which is done by sect02:
! * xmas_secti is for aiken mode
! * xmas_sectj is for accumulation mode
! * xmas_sectc is for coarse mode
! * sect02 expects input in um
! * pass in generic mass of 1.0 just to get a percentage distribution of mass
! among bins
!
        ss1=alog(sginin)
        ss2=exp(ss1*ss1*36.0/8.0)
        ss3=(sixpi*vol_ai/(num_ai*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginin,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_secti,xmas_secti)
        ss1=alog(sginia)
        ss2=exp(ss1*ss1*36.0/8.0)
        ss3=(sixpi*vol_aj/(num_aj*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginia,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectj,xmas_sectj)
        ss1=alog(sginic)
        ss2=exp(ss1*ss1*36.0/8.0)
        ss3=(sixpi*vol_ac/(num_ac*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginic,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectc,xmas_sectc)

        do isize = 1, nbin_o
          xdia_cm(isize)=xdia_um(isize)*1.0e-04
          mass_so4 = mass_so4i*xmas_secti(isize) + mass_so4j*xmas_sectj(isize)
          mass_no3 = mass_no3i*xmas_secti(isize) + mass_no3j*xmas_sectj(isize)
          mass_nh4 = mass_nh4i*xmas_secti(isize) + mass_nh4j*xmas_sectj(isize)
          mass_oin = mass_oini*xmas_secti(isize) + mass_oinj*xmas_sectj(isize) + &
                     mass_antha*xmas_sectc(isize)
!jdfcz    mass_dust= mass_dusti*xmas_secti(isize) + mass_dustj*xmas_sectj(isize)
!+ &
!jdfcz               mass_soil*xmas_sectc(isize)
          !mass_oc = (mass_pai+mass_aro1i+mass_aro2i+mass_alk1i+mass_ole1i+ &
          !           mass_ba1i+mass_ba2i+mass_ba3i+mass_ba4i)*xmas_secti(isize)
          !           + &
          !          (mass_paj+mass_aro1j+mass_aro2j+mass_alk1j+mass_ole1j+ &
          !           mass_ba1j+mass_ba2j+mass_ba3j+mass_ba4j)*xmas_sectj(isize)
          mass_oc  = mass_oci*xmas_secti(isize) + mass_ocj*xmas_sectj(isize)
          mass_bc  = mass_bci*xmas_secti(isize) + mass_bcj*xmas_sectj(isize)
          mass_na  = mass_nai*xmas_secti(isize) + mass_naj*xmas_sectj(isize)+ &
                     mass_seas*xmas_sectc(isize)*(22.9898/58.4428)
          mass_cl  = mass_cli*xmas_secti(isize) + mass_clj*xmas_sectj(isize)+ &
                     mass_seas*xmas_sectc(isize)*(35.4530/58.4428)
          mass_h2o = mass_h2oi*xmas_secti(isize) + mass_h2oj*xmas_sectj(isize)
!         mass_h2o = 0.0 ! testing purposes only
          vol_so4 = mass_so4 / dens_so4
          vol_no3 = mass_no3 / dens_no3
          vol_nh4 = mass_nh4 / dens_nh4
          vol_oin = mass_oin / dens_oin
!jdfcz    vol_dust = mass_dust / dens_dust
          vol_oc  = mass_oc  / dens_oc
          vol_bc  = mass_bc  / dens_bc
          vol_na  = mass_na  / dens_na
          vol_cl  = mass_cl  / dens_cl
          vol_h2o = mass_h2o / dens_h2o
!!$          if(i.eq.50.and.j.eq.40.and.k.eq.1) then
!!$            print*,'jdf print bin',isize
!!$            print*,'so4',mass_so4,vol_so4
!!$            print*,'no3',mass_no3,vol_no3
!!$            print*,'nh4',mass_nh4,vol_nh4
!!$            print*,'oin',mass_oin,vol_oin
!!$!jdfcz      print*,'dust',mass_dust,vol_dust
!!$            print*,'oc ',mass_oc,vol_oc
!!$            print*,'bc ',mass_bc,vol_bc
!!$            print*,'na ',mass_na,vol_na
!!$            print*,'cl ',mass_cl,vol_cl
!!$            print*,'h2o',mass_h2o,vol_h2o
!!$          endif
          mass_dry_a = mass_so4 + mass_no3 + mass_nh4 + mass_oin + &
!jdfcz                 mass_oc  + mass_bc  + mass_na  + mass_cl  + mass_dust
                       mass_oc  + mass_bc  + mass_na  + mass_cl
          mass_wet_a = mass_dry_a + mass_h2o
          vol_dry_a  = vol_so4  + vol_no3  + vol_nh4  + vol_oin  + &
!jdfcz                 vol_oc   + vol_bc   + vol_na   + vol_cl   + vol_dust
                       vol_oc   + vol_bc   + vol_na   + vol_cl
          vol_wet_a  = vol_dry_a + vol_h2o
          vol_shell  = vol_wet_a - vol_bc
          !num_a      = vol_wet_a /
          !(0.52359877*xdia_cm(isize)*xdia_cm(isize)*xdia_cm(isize))
          !czhao 
          num_a      = num_ai*xnum_secti(isize)+num_aj*xnum_sectj(isize)+num_ac*xnum_sectc(isize)


          !shortwave 
          do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)   * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns)   * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &

(swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize)
            dp_wet_a   = xdia_cm(isize)
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)   * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns)   * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            swrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            swrefindx_core(i,k,j,isize,ns) =ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns shortwave


          !longwave 
          do ns=1,nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize)
dp_wet_a   = xdia_cm(isize)
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            lwrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            lwrefindx_core(i,k,j,isize,ns) =ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns longwave

!          refr=real(refindx(i,k,j,isize))

        enddo  !isize
      enddo   !i
      enddo   !j
      enddo   !k

      return

END subroutine optical_prep_modal_soa_vbs

!----------------------------------------------------------------------------------
! This subroutine computes volume-averaged refractive index and wet radius needed
! by the mie calculations. Aerosol number is also passed into the mie calculations
! in terms of other units.
!
      subroutine optical_prep_modal_vbs(nbin_o, chem, alt,           &
!       h2oai, h2oaj, refindx, radius_wet, number_bin,           &
!       radius_core, refindx_core, refindx_shell,                &
        h2oai, h2oaj, radius_core,radius_wet, number_bin,           &
        swrefindx, swrefindx_core, swrefindx_shell,                &
        lwrefindx, lwrefindx_core, lwrefindx_shell,                &
        ids,ide, jds,jde, kds,kde,                               &
        ims,ime, jms,jme, kms,kme,                               &
        its,ite, jts,jte, kts,kte                                )
!
   USE module_configure
!  USE module_state_description
   USE module_model_constants
   USE module_state_description, only:  param_first_scalar
   USE module_data_sorgam_vbs
!
   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt, h2oai, h2oaj
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
         INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
!  COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),           &
!        INTENT(OUT ) ::                                               &
!          refindx, refindx_core, refindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nswbands),   &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nlwbands),   &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell
!
! local variables
!
   integer i, j, k, l, isize, itype, iphase
   integer p1st
   complex  ref_index_lvcite  , ref_index_nh4hso4, &
            ref_index_nh4msa , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3   , ref_index_na2so4, &
            ref_index_na3hso4, ref_index_nahso4  , ref_index_namsa,  &
            ref_index_caso4  , ref_index_camsa2  , ref_index_cano3,  &
            ref_index_cacl2  , ref_index_caco3   , ref_index_h2so4,  &
            ref_index_hhso4  , ref_index_hno3    , ref_index_hcl,    &
            ref_index_msa    , ref_index_bc,     &
            ref_index_oin    , ref_index_aro1    , ref_index_aro2,   &
            ref_index_alk1   , ref_index_ole1    , ref_index_api1,   &
            ref_index_api2   , ref_index_lim1    , ref_index_lim2,    &
            ri_dum            , ri_ave_a
   COMPLEX, DIMENSION(nswbands) ::     & ! now only 5 aerosols have wave-dependent refr
    swref_index_oc , swref_index_dust , swref_index_nh4so4, swref_index_nacl,swref_index_h2o
   COMPLEX, DIMENSION(nlwbands) ::     & ! now only 5 aerosols have wave-dependent refr
    lwref_index_oc , lwref_index_dust , lwref_index_nh4so4, lwref_index_nacl,lwref_index_h2o

   real  dens_so4  , dens_no3  , dens_cl   , dens_msa  , dens_co3 ,  &
         dens_nh4  , dens_na   , dens_ca   , dens_oin  , dens_oc  ,  &
         dens_bc   , dens_aro1 , dens_aro2 , dens_alk1 , dens_ole1,  &
         dens_api1 , dens_api2 , dens_lim1 , dens_lim2 , dens_h2o ,  &
         dens_dust
   real  mass_so4  , mass_no3  , mass_cl   , mass_msa  , mass_co3 ,  &
         mass_nh4  , mass_na   , mass_ca   , mass_oin  , mass_oc  ,  &
         mass_bc   , mass_aro1 , mass_aro2 , mass_alk1 , mass_ole1,  &
         mass_api1 , mass_api2 , mass_lim1 , mass_lim2 , mass_h2o,   &
         mass_dust
   real  mass_so4i , mass_no3i , mass_cli  , mass_msai , mass_co3i,  &
         mass_nh4i , mass_nai  , mass_cai  , mass_oini , mass_oci ,  &
         mass_bci  , mass_aro1i, mass_aro2i, mass_alk1i, mass_ole1i, &
         mass_ba1i , mass_ba2i,  mass_ba3i , mass_ba4i , mass_pai,   &
         mass_h2oi , mass_dusti
   real  mass_so4j , mass_no3j , mass_clj  , mass_msaj , mass_co3j,  &
         mass_nh4j , mass_naj  , mass_caj  , mass_oinj , mass_ocj ,  &
         mass_bcj  , mass_aro1j, mass_aro2j, mass_alk1j, mass_ole1j, &
         mass_ba1j , mass_ba2j,  mass_ba3j , mass_ba4j , mass_paj,   &
         mass_h2oj , mass_dustj
   real  mass_antha, mass_seas, mass_soil
   real  num_ai, num_aj, num_ac,  vol_ai, vol_aj, vol_ac
   real  vol_so4   , vol_no3   , vol_cl    , vol_msa   , vol_co3  ,  &
         vol_nh4   , vol_na    , vol_ca    , vol_oin   , vol_oc   ,  &
         vol_bc    , vol_aro1  , vol_aro2  , vol_alk1  , vol_ole1 ,  &
         vol_api1  , vol_api2  , vol_lim1  , vol_lim2  , vol_h2o  ,  &
         vol_dust
   real  conv1a, conv1b
   real  mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
         dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a
   real  ifac, jfac, cfac
   real  refr
   integer ns
   real  dgnum_um,drydens,duma,dlo_um,dhi_um,dgmin,sixpi,ss1,ss2,ss3,dtemp
   integer  iflag
   real, dimension(1:nbin_o) :: xnum_secti,xnum_sectj,xnum_sectc
   real, dimension(1:nbin_o) :: xmas_secti,xmas_sectj,xmas_sectc
   real, dimension(1:nbin_o) :: xdia_um, xdia_cm
!
!  real  sginin,sginia,sginic from module_data_sorgam.F
!
! Mass from modal distribution is divided into individual sections before
! being passed back into the Mie routine.
! * currently use the same size bins as 8 default MOSAIC size bins
! * dlo_um and dhi_um define the lower and upper bounds of individual sections
!   used to compute optical properties
! * sigmas for 3 modes taken from module_sorgan_data.F
! * these parameters are needed by sect02 that is called later
! * sginin=1.7, sginia=2.0, sginic=2.5
!
      sixpi=6.0/3.14159265359
      dlo_um=0.0390625
      dhi_um=10.0
      drydens=1.8
      iflag=2
      duma=1.0
      dgmin=1.0e-07 ! in (cm)
      dtemp=dlo_um
      do isize=1,nbin_o
        xdia_um(isize)=(dtemp+dtemp*2.0)/2.0
        dtemp=dtemp*2.0
      enddo
!
! Define refractive indicies
! * assume na and cl are the same as nacl
! * assume so4, no3, and nh4 are the same as nh4no3
! * assume ca and co3 are the same as caco3
! * assume msa is just msa
! Further work:
! * to be more precise, need to compute electrolytes to apportion
!   so4, no3, nh4, na, cl, msa, ca, co3 among various componds
!   as was done previously in module_mosaic_therm.F
!
      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns) = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns) = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns) = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns) = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns) = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns) = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns) = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns) = cmplx(refrwlw(ns),refiwlw(ns))
      enddo

!     ref_index_nh4so4 = cmplx(1.52,0.)
      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     ! assumed
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)
!     ref_index_nacl   = cmplx(1.45,0.)
      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     ! assumed
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  ! assumed
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     ! assumed
!     ref_index_oc     = cmplx(1.45,0.)  ! JCB, Feb. 20, 2008: no complex part?
! JCB, Feb. 20, 2008:  set the refractive index of BC equal to the
! midpoint of ranges given in Bond and Bergstrom, Light absorption by
! carboneceous particles: an investigative review 2006, Aerosol Sci.
! and Tech., 40:27-67.
!     ref_index_bc     = cmplx(1.82,0.74) old value
      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  ! JCB, Feb. 20, 2008: "other inorganics"
!     ref_index_dust   = cmplx(1.55,0.003)  ! czhao, this refractive index should be wavelength depedent
      ref_index_aro1   = cmplx(1.45,0.)
      ref_index_aro2   = cmplx(1.45,0.)
      ref_index_alk1   = cmplx(1.45,0.)
      ref_index_ole1   = cmplx(1.45,0.)
      ref_index_api1   = cmplx(1.45,0.)
      ref_index_api2   = cmplx(1.45,0.)
      ref_index_lim1   = cmplx(1.45,0.)
      ref_index_lim2   = cmplx(1.45,0.)
!     ref_index_h2o    = cmplx(1.33,0.)
!
! densities in g/cc
!
      dens_so4   = 1.8        ! used
      dens_no3   = 1.8        ! used
      dens_cl    = 2.2        ! used
      dens_msa   = 1.8        ! used
      dens_co3   = 2.6        ! used
      dens_nh4   = 1.8        ! used
      dens_na    = 2.2        ! used
      dens_ca    = 2.6        ! used
      dens_oin   = 2.6        ! used
      dens_dust  = 2.6        ! used
      dens_oc    = 1.0        ! used
! JCB, Feb. 20, 2008:  the density of BC is updated to reflect values
! published by Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!      dens_bc    = 1.7        ! used, old value
      dens_bc    =  1.8        !  midpoint of Bond and Bergstrom value
      dens_aro1  = 1.0
      dens_aro2  = 1.0
      dens_alk1  = 1.0
      dens_ole1  = 1.0
      dens_api1  = 1.0
      dens_api2  = 1.0
      dens_lim1  = 1.0
      dens_lim2  = 1.0
      dens_h2o   = 1.0
!
      p1st = param_first_scalar
!
      swrefindx=0.0
      lwrefindx=0.0
      radius_wet=0.0
      number_bin=0.0
      radius_core=0.0
      swrefindx_core=0.0
      swrefindx_shell=0.0
      lwrefindx_core=0.0
      lwrefindx_shell=0.0
!
! units:
! * mass     - g/cc(air)
! * number   - #/cc(air)
! * volume   - cc(air)/cc(air)
! * diameter - cm
!
      itype=1
      iphase=1
      do j = jts, jte
      do k = kts, kte
      do i = its, ite
        mass_so4i = 0.0
        mass_so4j = 0.0
        mass_no3i = 0.0
        mass_no3j = 0.0
        mass_nh4i = 0.0
        mass_nh4j = 0.0
        mass_oini = 0.0
        mass_oinj = 0.0
        mass_dusti = 0.0
        mass_dustj = 0.0
        mass_aro1i = 0.0
        mass_aro1j = 0.0
        mass_aro2i = 0.0
        mass_aro2j = 0.0
        mass_alk1i = 0.0
        mass_alk1j = 0.0
        mass_ole1i = 0.0
        mass_ole1j = 0.0
        mass_ba1i = 0.0
        mass_ba1j = 0.0
        mass_ba2i = 0.0
        mass_ba2j = 0.0
        mass_ba3i = 0.0
        mass_ba3j = 0.0
        mass_ba4i = 0.0
        mass_ba4j = 0.0
        mass_pai = 0.0
        mass_paj = 0.0
        mass_oci = 0.0
        mass_ocj = 0.0
        mass_bci = 0.0
        mass_bcj = 0.0
        mass_cai = 0.0
        mass_caj = 0.0
        mass_co3i = 0.0
        mass_co3j = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_msai = 0.0
        mass_msaj = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_h2oi = 0.0
        mass_h2oj = 0.0
        mass_antha = 0.0
        mass_seas = 0.0
        mass_soil = 0.0
        vol_aj = 0.0
        vol_ai = 0.0
        vol_ac = 0.0
        num_aj = 0.0
        num_ai = 0.0
        num_ac = 0.0

! convert ug / kg dry air to g / cc  air
        conv1a = (1.0/alt(i,k,j)) * 1.0e-12
! convert # / kg dry air to # / cc  air
        conv1b = (1.0/alt(i,k,j)) * 1.0e-6

! Accumulation mode...
!       isize = 1 ; itype = 1   ! before march-2008 ordering
        isize = 2 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4j= chem(i,k,j,l)*conv1a
        l=lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_no3j= chem(i,k,j,l)*conv1a
        l=lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nh4j= chem(i,k,j,l)*conv1a
        l=lptr_p25_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_oinj= chem(i,k,j,l)*conv1a
!jdfcz  l=lptr_dust_aer(isize,itype,iphase)
!jdfcz  if (l .ge. p1st)  mass_dustj= chem(i,k,j,l)*conv1a
        l=lptr_asoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro1j= chem(i,k,j,l)*conv1a
        l=lptr_asoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro2j= chem(i,k,j,l)*conv1a
        l=lptr_asoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_alk1j= chem(i,k,j,l)*conv1a
        l=lptr_asoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ole1j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba1j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba2j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba3j= chem(i,k,j,l)*conv1a
        l=lptr_bsoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba4j= chem(i,k,j,l)*conv1a
        l=lptr_orgpa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_paj= chem(i,k,j,l)*conv1a
        l=lptr_ec_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bcj= chem(i,k,j,l)*conv1a
        l=lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_naj= chem(i,k,j,l)*conv1a
        l=lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_clj= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_aj= chem(i,k,j,l)*conv1b
        mass_h2oj= h2oaj(i,k,j) * 1.0e-12
        mass_ocj=mass_aro1j+mass_aro2j+mass_alk1j+mass_ole1j+ &
                 mass_ba1j+mass_ba2j+mass_ba3j+mass_ba4j+mass_paj

! Aitken mode...
!       isize = 1 ; itype = 2   ! before march-2008 ordering
        isize = 1 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4i= chem(i,k,j,l)*conv1a
        l=lptr_no3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_no3i= chem(i,k,j,l)*conv1a
        l=lptr_nh4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nh4i= chem(i,k,j,l)*conv1a
        l=lptr_p25_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_oini= chem(i,k,j,l)*conv1a
!jdfcz  l=lptr_dust_aer(isize,itype,iphase)
!jdfcz  if (l .ge. p1st)  mass_dusti= chem(i,k,j,l)*conv1a
        l=lptr_asoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro1i= chem(i,k,j,l)*conv1a
        l=lptr_asoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_aro2i= chem(i,k,j,l)*conv1a
        l=lptr_asoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_alk1i= chem(i,k,j,l)*conv1a
        l=lptr_asoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ole1i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa1_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba1i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa2_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba2i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa3_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba3i= chem(i,k,j,l)*conv1a
        l=lptr_bsoa4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ba4i= chem(i,k,j,l)*conv1a
        l=lptr_orgpa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_pai= chem(i,k,j,l)*conv1a
        l=lptr_ec_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bci= chem(i,k,j,l)*conv1a
        l=lptr_na_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_nai= chem(i,k,j,l)*conv1a
        l=lptr_cl_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_cli= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_ai= chem(i,k,j,l)*conv1b
        mass_h2oi= h2oai(i,k,j) * 1.0e-12
        mass_oci=mass_aro1i+mass_aro2i+mass_alk1i+mass_ole1i+ &
                 mass_ba1i+mass_ba2i+mass_ba3i+mass_ba4i+mass_pai

! Coarse mode...
!       isize = 1 ; itype = 3   ! before march-2008 ordering
        isize = 1 ; itype = 2   ! after  march-2008 ordering
        l=lptr_anth_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_antha= chem(i,k,j,l)*conv1a
        l=lptr_seas_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_seas= chem(i,k,j,l)*conv1a
        l=lptr_soil_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_soil= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_ac= chem(i,k,j,l)*conv1b

        vol_ai = (mass_so4i/dens_so4)+(mass_no3i/dens_no3)+ &
                 (mass_nh4i/dens_nh4)+(mass_oini/dens_oin)+ &
                 (mass_aro1i/dens_oc)+(mass_alk1i/dens_oc)+ &
                 (mass_ole1i/dens_oc)+(mass_ba1i/dens_oc)+  &
                 (mass_ba2i/dens_oc)+(mass_ba3i/dens_oc)+   &
                 (mass_ba4i/dens_oc)+(mass_pai/dens_oc)+    &
                 (mass_aro2i/dens_oc)+(mass_bci/dens_bc)+   &
                 (mass_nai/dens_na)+(mass_cli/dens_cl)
!jdfcz           (mass_nai/dens_na)+(mass_cli/dens_cl) +    &
!jdfcz           (mass_dusti/dens_dust)
        vol_aj = (mass_so4j/dens_so4)+(mass_no3j/dens_no3)+ &
                 (mass_nh4j/dens_nh4)+(mass_oinj/dens_oin)+ &
                 (mass_aro1j/dens_oc)+(mass_alk1j/dens_oc)+ &
                 (mass_ole1j/dens_oc)+(mass_ba1j/dens_oc)+  &
                 (mass_ba2j/dens_oc)+(mass_ba3j/dens_oc)+   &
                 (mass_ba4j/dens_oc)+(mass_paj/dens_oc)+    &
                 (mass_aro2j/dens_oc)+(mass_bcj/dens_bc)+   &
                 (mass_naj/dens_na)+(mass_clj/dens_cl)
!jdfcz           (mass_naj/dens_na)+(mass_clj/dens_cl) +    &
!jdfcz           (mass_dustj/dens_dust)
        vol_ac = (mass_antha/dens_oin)+ &
                 (mass_seas*(22.9898/58.4428)/dens_na)+ &
                 (mass_seas*(35.4530/58.4428)/dens_cl)+ &
                 (mass_soil/dens_dust)

!
! Now divide mass into sections which is done by sect02:
! * xmas_secti is for aiken mode
! * xmas_sectj is for accumulation mode
! * xmas_sectc is for coarse mode
! * sect02 expects input in um
! * pass in generic mass of 1.0 just to get a percentage distribution of mass among bins
!
        ss1=alog(sginin)
        ss2=exp(ss1*ss1*36.0/8.0)
        ss3=(sixpi*vol_ai/(num_ai*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginin,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_secti,xmas_secti)
        ss1=alog(sginia)
        ss2=exp(ss1*ss1*36.0/8.0)
        ss3=(sixpi*vol_aj/(num_aj*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginia,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectj,xmas_sectj)
        ss1=alog(sginic)
        ss2=exp(ss1*ss1*36.0/8.0)
        ss3=(sixpi*vol_ac/(num_ac*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sginic,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectc,xmas_sectc)

        do isize = 1, nbin_o
          xdia_cm(isize)=xdia_um(isize)*1.0e-04
          mass_so4 = mass_so4i*xmas_secti(isize) + mass_so4j*xmas_sectj(isize)
          mass_no3 = mass_no3i*xmas_secti(isize) + mass_no3j*xmas_sectj(isize)
          mass_nh4 = mass_nh4i*xmas_secti(isize) + mass_nh4j*xmas_sectj(isize)
          mass_oin = mass_oini*xmas_secti(isize) + mass_oinj*xmas_sectj(isize) + &
                     mass_antha*xmas_sectc(isize)
!jdfcz    mass_dust= mass_dusti*xmas_secti(isize) + mass_dustj*xmas_sectj(isize) + &
!jdfcz               mass_soil*xmas_sectc(isize)
          mass_oc = (mass_pai+mass_aro1i+mass_aro2i+mass_alk1i+mass_ole1i+ &
                     mass_ba1i+mass_ba2i+mass_ba3i+mass_ba4i)*xmas_secti(isize) + &
                    (mass_paj+mass_aro1j+mass_aro2j+mass_alk1j+mass_ole1j+ &
                     mass_ba1j+mass_ba2j+mass_ba3j+mass_ba4j)*xmas_sectj(isize)
          mass_bc  = mass_bci*xmas_secti(isize) + mass_bcj*xmas_sectj(isize)
          mass_na  = mass_nai*xmas_secti(isize) + mass_naj*xmas_sectj(isize)+ &
                     mass_seas*xmas_sectc(isize)*(22.9898/58.4428)
          mass_cl  = mass_cli*xmas_secti(isize) + mass_clj*xmas_sectj(isize)+ &
                     mass_seas*xmas_sectc(isize)*(35.4530/58.4428)
          mass_h2o = mass_h2oi*xmas_secti(isize) + mass_h2oj*xmas_sectj(isize)
!         mass_h2o = 0.0 ! testing purposes only
          vol_so4 = mass_so4 / dens_so4
          vol_no3 = mass_no3 / dens_no3
          vol_nh4 = mass_nh4 / dens_nh4
          vol_oin = mass_oin / dens_oin
!jdfcz    vol_dust = mass_dust / dens_dust
          vol_oc  = mass_oc  / dens_oc
          vol_bc  = mass_bc  / dens_bc
          vol_na  = mass_na  / dens_na
          vol_cl  = mass_cl  / dens_cl
          vol_h2o = mass_h2o / dens_h2o
!!$          if(i.eq.50.and.j.eq.40.and.k.eq.1) then
!!$            print*,'jdf print bin',isize
!!$            print*,'so4',mass_so4,vol_so4
!!$            print*,'no3',mass_no3,vol_no3
!!$            print*,'nh4',mass_nh4,vol_nh4
!!$            print*,'oin',mass_oin,vol_oin
!!$!jdfcz      print*,'dust',mass_dust,vol_dust
!!$            print*,'oc ',mass_oc,vol_oc
!!$            print*,'bc ',mass_bc,vol_bc
!!$            print*,'na ',mass_na,vol_na
!!$            print*,'cl ',mass_cl,vol_cl
!!$            print*,'h2o',mass_h2o,vol_h2o
!!$          endif
          mass_dry_a = mass_so4 + mass_no3 + mass_nh4 + mass_oin + &
!jdfcz                 mass_oc  + mass_bc  + mass_na  + mass_cl  + mass_dust
                       mass_oc  + mass_bc  + mass_na  + mass_cl
          mass_wet_a = mass_dry_a + mass_h2o
          vol_dry_a  = vol_so4  + vol_no3  + vol_nh4  + vol_oin  + &
!jdfcz                 vol_oc   + vol_bc   + vol_na   + vol_cl   + vol_dust
                       vol_oc   + vol_bc   + vol_na   + vol_cl
          vol_wet_a  = vol_dry_a + vol_h2o
          vol_shell  = vol_wet_a - vol_bc
          !num_a      = vol_wet_a / (0.52359877*xdia_cm(isize)*xdia_cm(isize)*xdia_cm(isize))
          !czhao
          num_a      = num_ai*xnum_secti(isize)+num_aj*xnum_sectj(isize)+num_ac*xnum_sectc(isize)


          !shortwave
          do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)   * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns)   * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize)
            dp_wet_a   = xdia_cm(isize)
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (swref_index_dust(ns)   * mass_dust / dens_dust) +  &
                       (swref_index_dust(ns)   * mass_oin / dens_dust)  +  &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            swrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            swrefindx_core(i,k,j,isize,ns) =ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns shortwave

          !longwave
          do ns=1,nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize)
            dp_wet_a   = xdia_cm(isize)
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
!jdf                   (ref_index_oin    * mass_oin / dens_oin) +  &
!jdfcz                 (lwref_index_dust(ns)    * mass_dust / dens_dust) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            lwrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            lwrefindx_core(i,k,j,isize,ns) =ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns longwave

!          refr=real(refindx(i,k,j,isize))

        enddo  !isize
      enddo   !i
      enddo   !j
      enddo   !k

      return

      end subroutine optical_prep_modal_vbs

!------------------------------------------------------------------
      subroutine optical_prep_mam(nbin_o, chem, alt,             &
        radius_core,radius_wet, number_bin,                      &
        swrefindx, swrefindx_core, swrefindx_shell,              &
        lwrefindx, lwrefindx_core, lwrefindx_shell,              &
        ids,ide, jds,jde, kds,kde,                               &
        ims,ime, jms,jme, kms,kme,                               &
        its,ite, jts,jte, kts,kte                                )
!
   USE module_configure
!  USE module_state_description
   USE module_model_constants
   USE module_state_description, only:  param_first_scalar
   USE module_data_cam_mam_asect
!
   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt                
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
         INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
!  COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),           &
!        INTENT(OUT ) ::                                               &
!          refindx, refindx_core, refindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nswbands),   &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nlwbands),   &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell
!
! local variables
!
   integer i, j, k, l, isize, itype, iphase
   integer p1st
   complex  ref_index_lvcite  , ref_index_nh4hso4, &
            ref_index_nh4msa , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3   , ref_index_na2so4, &
            ref_index_na3hso4, ref_index_nahso4  , ref_index_namsa,  &
            ref_index_caso4  , ref_index_camsa2  , ref_index_cano3,  &
            ref_index_cacl2  , ref_index_caco3   , ref_index_h2so4,  &
            ref_index_hhso4  , ref_index_hno3    , ref_index_hcl,    &
            ref_index_msa    , ref_index_bc,     &
            ref_index_oin    ,                   &
            ri_dum            , ri_ave_a
   COMPLEX, DIMENSION(nswbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    swref_index_oc , swref_index_dust , swref_index_nh4so4, swref_index_nacl,swref_index_h2o
   COMPLEX, DIMENSION(nlwbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    lwref_index_oc , lwref_index_dust , lwref_index_nh4so4, lwref_index_nacl,lwref_index_h2o

   real  dens_so4  , dens_ncl  , dens_wtr  , dens_pom  , dens_soa ,  &
         dens_bc   , dens_dst
   real  mass_so4  , mass_ncl  , mass_wtr  , mass_pom  , mass_soa ,  &
         mass_bc   , mass_dst
   real  vol_so4  , vol_ncl  , vol_wtr  , vol_pom  , vol_soa ,  &
         vol_bc   , vol_dst
   real  mass_so4_a1 , mass_so4_a2 , mass_so4_a3, &
         mass_ncl_a1 , mass_ncl_a2 , mass_ncl_a3, &
         mass_wtr_a1 , mass_wtr_a2 , mass_wtr_a3, &
         mass_soa_a1 , mass_soa_a2 , mass_pom_a1, &
         mass_bc_a1  , mass_dst_a1 , mass_dst_a3, &
         num_a1      , num_a2      , num_a3,      &
         vol_a1      , vol_a2      , vol_a3
   real  conv1a, conv1b
   real  mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
         dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a
   real  ifac, jfac, cfac
   real  refr
   integer ns
   real  dgnum_um,drydens,duma,dlo_um,dhi_um,dgmin,sixpi,ss1,ss2,ss3,dtemp
   integer  iflag
   real, dimension(1:nbin_o) :: xnum_secti,xnum_sectj,xnum_sectc
   real, dimension(1:nbin_o) :: xmas_secti,xmas_sectj,xmas_sectc
   real, dimension(1:nbin_o) :: xdia_um, xdia_cm
!
!  real  sginin,sginia,sginic from module_data_sorgam.F
! 
! Mass from modal distribution is divided into individual sections before
! being passed back into the Mie routine.
! * currently use the same size bins as 8 default MOSAIC size bins
! * dlo_um and dhi_um define the lower and upper bounds of individual sections
!   used to compute optical properties
! * sigmas for 3 modes taken from module_sorgan_data.F
! * these parameters are needed by sect02 that is called later
! * sginin=1.7, sginia=2.0, sginic=2.5
!
      sixpi=6.0/3.14159265359
      dlo_um=0.0390625
      dhi_um=10.0
      drydens=1.8
      iflag=2
      duma=1.0
      dgmin=1.0e-07 ! in (cm)
      dtemp=dlo_um
      do isize=1,nbin_o
        xdia_um(isize)=(dtemp+dtemp*2.0)/2.0
        dtemp=dtemp*2.0
      enddo
!
! Define refractive indicies
! * assume na and cl are the same as nacl
! * assume so4, no3, and nh4 are the same as nh4no3
! * assume ca and co3 are the same as caco3
! * assume msa is just msa
! Further work:
! * to be more precise, need to compute electrolytes to apportion
!   so4, no3, nh4, na, cl, msa, ca, co3 among various componds
!   as was done previously in module_mosaic_therm.F
!
      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns) = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns) = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns) = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns) = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns) = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns) = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns) = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns) = cmplx(refrwlw(ns),refiwlw(ns))
      enddo

!     ref_index_nh4so4 = cmplx(1.52,0.)
      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     ! assumed
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)
!     ref_index_nacl   = cmplx(1.45,0.)
      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     ! assumed
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  ! assumed
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     ! assumed
!     ref_index_oc     = cmplx(1.45,0.)  ! JCB, Feb. 20, 2008: no complex part?
! JCB, Feb. 20, 2008:  set the refractive index of BC equal to the
! midpoint of ranges given in Bond and Bergstrom, Light absorption by
! carboneceous particles: an investigative review 2006, Aerosol Sci.
! and Tech., 40:27-67.
!     ref_index_bc     = cmplx(1.82,0.74) old value
      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  ! JCB, Feb. 20, 2008: "other inorganics" 
!     ref_index_dust   = cmplx(1.55,0.003)  ! czhao, this refractive index should be wavelength depedent
!     ref_index_h2o    = cmplx(1.33,0.)
!
! densities in g/cc
!
      dens_so4   = 1.8        ! used
      dens_ncl   = 2.2        ! used
      dens_dst   = 2.6        ! used
      dens_pom   = 1.0        ! used
      dens_soa   = 1.0        ! used
      dens_wtr   = 1.0
! JCB, Feb. 20, 2008:  the density of BC is updated to reflect values
! published by Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!     dens_bc    =  1.7        ! used, old value
      dens_bc    =  1.8        !  midpoint of Bond and Bergstrom value
!
      p1st = param_first_scalar
!
      swrefindx=0.0
      lwrefindx=0.0
      radius_wet=0.0
      number_bin=0.0
      radius_core=0.0
      swrefindx_core=0.0
      swrefindx_shell=0.0
      lwrefindx_core=0.0
      lwrefindx_shell=0.0
!
! units:
! * mass     - g/cc(air)
! * number   - #/cc(air)
! * volume   - cc(air)/cc(air)
! * diameter - cm
!
      itype=1
      iphase=1
      do j = jts, jte
      do k = kts, kte
      do i = its, ite
        mass_so4_a1 = 0.0
        mass_so4_a2 = 0.0
        mass_so4_a3 = 0.0
        mass_ncl_a1 = 0.0
        mass_ncl_a2 = 0.0
        mass_ncl_a3 = 0.0
        mass_wtr_a1 = 0.0
        mass_wtr_a2 = 0.0
        mass_wtr_a3 = 0.0
        mass_pom_a1 = 0.0
        mass_soa_a1 = 0.0
        mass_soa_a2 = 0.0
        mass_bc_a1  = 0.0
        mass_dst_a1 = 0.0
        mass_dst_a3 = 0.0
        vol_a1 = 0.0
        vol_a2 = 0.0
        vol_a3 = 0.0
        num_a1 = 0.0
        num_a2 = 0.0
        num_a3 = 0.0

! convert ug / kg dry air to g / cc  air
        conv1a = (1.0/alt(i,k,j)) * 1.0e-12
! convert # / kg dry air to # / cc  air
        conv1b = (1.0/alt(i,k,j)) * 1.0e-6

! Accumulation mode...
        isize = 1 ; itype = 1   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4_a1= chem(i,k,j,l)*conv1a
        l=lptr_seas_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ncl_a1= chem(i,k,j,l)*conv1a
        l=lptr_pom_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_pom_a1= chem(i,k,j,l)*conv1a
        l=lptr_soa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_soa_a1= chem(i,k,j,l)*conv1a
        l=lptr_bc_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_bc_a1= chem(i,k,j,l)*conv1a
        l=lptr_dust_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_dst_a1= chem(i,k,j,l)*conv1a
        l=waterptr_aer(isize,itype)
        if (l .ge. p1st)  mass_wtr_a1= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_a1= chem(i,k,j,l)*conv1b

! Aitken mode...
        isize = 1 ; itype = 2   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4_a2= chem(i,k,j,l)*conv1a
        l=lptr_seas_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ncl_a2= chem(i,k,j,l)*conv1a
        l=lptr_soa_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_soa_a2= chem(i,k,j,l)*conv1a
        l=waterptr_aer(isize,itype)
        if (l .ge. p1st)  mass_wtr_a2= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_a2= chem(i,k,j,l)*conv1b

! Coarse mode...
        isize = 1 ; itype = 3   ! after  march-2008 ordering
        l=lptr_so4_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_so4_a3= chem(i,k,j,l)*conv1a
        l=lptr_seas_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_ncl_a3= chem(i,k,j,l)*conv1a
        l=lptr_dust_aer(isize,itype,iphase)
        if (l .ge. p1st)  mass_dst_a3= chem(i,k,j,l)*conv1a
        l=waterptr_aer(isize,itype)
        if (l .ge. p1st)  mass_wtr_a3= chem(i,k,j,l)*conv1a
        l=numptr_aer(isize,itype,iphase)
        if (l .ge. p1st)  num_a3= chem(i,k,j,l)*conv1b

        vol_a1 = (mass_so4_a1/dens_so4)+(mass_ncl_a1/dens_ncl)+ &
                 (mass_pom_a1/dens_pom)+(mass_soa_a1/dens_soa)+ &
                 (mass_bc_a1/dens_bc)+(mass_dst_a1/dens_dst)
        vol_a2 = (mass_so4_a2/dens_so4)+(mass_ncl_a2/dens_ncl)+ &
                 (mass_soa_a2/dens_soa)
        vol_a3 = (mass_so4_a3/dens_so4)+(mass_ncl_a3/dens_ncl)+ &
                 (mass_dst_a3/dens_dst)
!
! Now divide mass into sections which is done by sect02:
! * xmas_secti is for aiken mode
! * xmas_sectj is for accumulation mode
! * xmas_sectc is for coarse mode
! * sect02 expects input in um
! * pass in generic mass of 1.0 just to get a percentage distribution of mass among bins
!
        ss1=log(sigmag_aer(1,2))
        ss2=exp(ss1*ss1*36.0/8.0) 
        ss3=(sixpi*vol_a2/(num_a2*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sigmag_aer(1,2),drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_secti,xmas_secti)
        ss1=log(sigmag_aer(1,1))
        ss2=exp(ss1*ss1*36.0/8.0) 
        ss3=(sixpi*vol_a1/(num_a1*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sigmag_aer(1,1),drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectj,xmas_sectj)
        ss1=log(sigmag_aer(1,3))
        ss2=exp(ss1*ss1*36.0/8.0) 
        ss3=(sixpi*vol_a3/(num_a3*ss2))**0.3333333
        dgnum_um=amax1(dgmin,ss3)*1.0e+04
        call sect02(dgnum_um,sigmag_aer(1,3),drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectc,xmas_sectc)

        do isize = 1, nbin_o
          xdia_cm(isize)=xdia_um(isize)*1.0e-04
          mass_so4 = mass_so4_a2*xmas_secti(isize) + mass_so4_a1*xmas_sectj(isize) + &
                     mass_so4_a3*xmas_sectc(isize)
          mass_ncl = mass_ncl_a2*xmas_secti(isize) + mass_ncl_a1*xmas_sectj(isize) + &
                     mass_ncl_a3*xmas_sectc(isize)
          mass_wtr = mass_wtr_a2*xmas_secti(isize) + mass_ncl_a1*xmas_sectj(isize) + &
                     mass_ncl_a3*xmas_sectc(isize)
          mass_dst = mass_dst_a1*xmas_sectj(isize) + mass_dst_a3*xmas_sectc(isize)
          mass_soa = mass_soa_a2*xmas_secti(isize) + mass_soa_a1*xmas_sectj(isize)
          mass_pom = mass_pom_a1*xmas_sectj(isize)
          mass_bc  = mass_bc_a1*xmas_sectj(isize)
          vol_so4 = mass_so4 / dens_so4
          vol_ncl = mass_ncl / dens_ncl
          vol_wtr = mass_wtr / dens_wtr
          vol_pom = mass_pom  / dens_pom
          vol_soa = mass_soa  / dens_soa
          vol_bc  = mass_bc  / dens_bc
          vol_dst = mass_dst / dens_dst
          mass_dry_a = mass_so4 + mass_ncl + mass_pom + mass_soa + &
                       mass_bc  + mass_dst
          mass_wet_a = mass_dry_a + mass_wtr 
          vol_dry_a  = vol_so4  + vol_ncl  + vol_pom  + vol_soa  + &
                       vol_bc   + vol_dst
          vol_wet_a  = vol_dry_a + vol_wtr
          vol_shell  = vol_wet_a - vol_bc
          num_a      = num_a2*xnum_secti(isize)+num_a1*xnum_sectj(isize)+num_a3*xnum_sectc(isize) 


          !shortwave 
          do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
!jdf                   (ref_index_oin          * mass_dst  / dens_dst) +   &
                       (swref_index_dust(ns)   * mass_dst  / dens_dst) +   &
!jdfcz                 (swref_index_dust(ns)   * mass_dst  / dens_dst) +   &
                       (swref_index_oc(ns)     * mass_pom  / dens_pom) +   &
                       (swref_index_oc(ns)     * mass_soa  / dens_soa) +   &
                       (ref_index_bc           * mass_bc  / dens_bc) +   &
                       (swref_index_nacl(ns)   * mass_ncl  / dens_ncl) +   &
                       (swref_index_h2o(ns)    * mass_wtr / dens_wtr)
!
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize) 
            dp_wet_a   = xdia_cm(isize) 
            dp_bc_a    = xdia_cm(isize) 
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
!jdf                     (ref_index_oin          * mass_dst  / dens_dst) +   &
                         (swref_index_dust(ns)   * mass_dst  / dens_dst) +   &
!jdfcz                   (swref_index_dust(ns)   * mass_dst  / dens_dst) +   &
                         (swref_index_oc(ns)     * mass_pom  / dens_pom) +   &
                         (swref_index_oc(ns)     * mass_soa  / dens_soa) +   &
                         (ref_index_bc           * mass_bc  / dens_bc) +   &
                         (swref_index_nacl(ns)   * mass_ncl  / dens_ncl) +   &
                         (swref_index_h2o(ns)    * mass_wtr / dens_wtr)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(vol_shell .lt. 1.0e-20) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            swrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            swrefindx_core(i,k,j,isize,ns) =ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns shortwave

          !longwave 
          do ns=1,nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
!jdf                   (ref_index_oin          * mass_dst  / dens_dst) +   &
                       (lwref_index_dust(ns)   * mass_dst  / dens_dst) +   &
!jdfcz                 (lwref_index_dust(ns)   * mass_dst  / dens_dst) +   &
                       (lwref_index_oc(ns)     * mass_pom  / dens_pom) +   &
                       (lwref_index_oc(ns)     * mass_soa  / dens_soa) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (lwref_index_nacl(ns)   * mass_ncl / dens_ncl) +   &
                       (lwref_index_h2o(ns)    * mass_wtr / dens_wtr)
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize) 
            dp_wet_a   = xdia_cm(isize) 
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
!jdf                     (ref_index_oin          * mass_dst  / dens_dst) +   &
                         (lwref_index_dust(ns)   * mass_dst  / dens_dst) +   &
!jdfcz                   (lwref_index_dust(ns)   * mass_dst  / dens_dst) +   &
                         (lwref_index_oc(ns)     * mass_pom  / dens_pom) +   &
                         (lwref_index_oc(ns)     * mass_soa  / dens_soa) +   &
                         (ref_index_bc     * mass_bc  / dens_bc) +   &
                         (lwref_index_nacl(ns)   * mass_ncl / dens_ncl) +   &
                         (lwref_index_h2o(ns)    * mass_wtr / dens_wtr)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(vol_shell .lt. 1.0e-20) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            lwrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            lwrefindx_core(i,k,j,isize,ns) =ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns longwave

!          refr=real(refindx(i,k,j,isize))

        enddo  !isize
      enddo   !i
      enddo   !j
      enddo   !k

      return

      end subroutine optical_prep_mam


!
!----------------------------------------------------------------------------------
! 4/26/11, SAM, added wavelength dependent refractive indexes for shortwave and longwave,
! 4/26/11, similar to what is done for modal CASE
! 9/21/09, SAM a modification of optical_prep_modal subroutine for GOCART aerosol model -
! SAM 7/18/09 - Modal parameters for OC1 (hydrophobic) OC2 (hydrophylic), BC1,BC2,
! and sulfate - just use dginia (meters) and sginia from module_data_sorgam.
! Not using accumulation mode from d'Almedia 1991 Table 7.1 and 7.2 global model
!
! This subroutine computes volume-averaged refractive index and wet radius needed
! by the mie calculations. Aerosol number is also passed into the mie calculations
! in terms of other units.
!
     subroutine optical_prep_gocart(nbin_o, chem, alt,relhum,          &
          radius_core,radius_wet, number_bin,                          &
          swrefindx,swrefindx_core, swrefindx_shell,                   &
          lwrefindx,lwrefindx_core, lwrefindx_shell,                   &
          uoc,                                                         & ! mklose
          ids,ide, jds,jde, kds,kde,                                   &
          ims,ime, jms,jme, kms,kme,                                   &
          its,ite, jts,jte, kts,kte                                    )
!
   USE module_configure
   USE module_model_constants
   USE module_data_sorgam
   USE module_data_gocart_seas
   USE module_data_gocartchem, only: oc_mfac,nh4_mfac
   USE module_data_mosaic_asect, only: hygro_msa_aer
! real*8, DIMENSION (4), PARAMETER :: ra(4)=(/1.d-1,5.d-1,1.5d0,5.0d0/)
! real*8, DIMENSION (4), PARAMETER :: rb(4)=(/5.d-1,1.5d0,5.d0,1.d1/)
! real*8, DIMENSION (4), PARAMETER :: den_seas(4)=(/2.2d3,2.2d3,2.2d3,2.2d3/)
! real*8, DIMENSION (4), PARAMETER :: reff_seas(4)=(/0.30D-6,1.00D-6,3.25D-6,7.50D-6/)
   USE module_data_gocart_dust, only:  ndust, reff_dust, den_dust
!  real*8, DIMENSION (5), PARAMETER :: den_dust(5)=(/2500.,2650.,2650.,2650.,2650./)
!  real*8, DIMENSION (5), PARAMETER :: reff_dust(5)=(/0.73D-6,1.4D-6,2.4D-6,4.5D-6,8.0D-6/)

!
   INTEGER, INTENT(IN   ) :: its,ite, jts,jte, kts,kte, nbin_o
   INTEGER, INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
   INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) ::  chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) ::  alt,relhum
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),              &
         INTENT(OUT ) ::                                               &
           radius_wet, number_bin, radius_core
!  COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte, 1:nbin_o),           &
!        INTENT(OUT ) ::                                               &
!          refindx, refindx_core, refindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nswbands),   &
         INTENT(OUT ) :: swrefindx, swrefindx_core, swrefindx_shell
   COMPLEX, DIMENSION( its:ite, kts:kte, jts:jte,1:nbin_o,nlwbands),   &
         INTENT(OUT ) :: lwrefindx, lwrefindx_core, lwrefindx_shell
!
! local variables
!
   integer i, j, k, l, m, n, isize, itype, iphase
   complex  ref_index_lvcite  , ref_index_nh4hso4, &
            ref_index_nh4msa , ref_index_nh4no3  , ref_index_nh4cl , &
            ref_index_nano3   , ref_index_na2so4, &
            ref_index_na3hso4, ref_index_nahso4  , ref_index_namsa,  &
            ref_index_caso4  , ref_index_camsa2  , ref_index_cano3,  &
            ref_index_cacl2  , ref_index_caco3   , ref_index_h2so4,  &
            ref_index_hhso4  , ref_index_hno3    , ref_index_hcl,    &
            ref_index_msa    , ref_index_bc,     &
            ref_index_oin    , ref_index_aro1    , ref_index_aro2,   &
            ref_index_alk1   , ref_index_ole1    , ref_index_api1,   &
            ref_index_api2   , ref_index_lim1     , ref_index_lim2,  &
            ri_dum            , ri_ave_a
   COMPLEX, DIMENSION(nswbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    swref_index_oc , swref_index_dust , swref_index_nh4so4, swref_index_nacl,swref_index_h2o
   COMPLEX, DIMENSION(nlwbands) ::     & ! now only 5 aerosols have wave-dependent refr 
    lwref_index_oc , lwref_index_dust , lwref_index_nh4so4, lwref_index_nacl,lwref_index_h2o
   real  dens_so4  , dens_no3  , dens_cl   , dens_msa  , dens_co3 ,  &
         dens_nh4  , dens_na   , dens_ca   , dens_oin  , dens_oc  ,  &
         dens_bc   , dens_aro1 , dens_aro2 , dens_alk1 , dens_ole1,  &
         dens_api1 , dens_api2 , dens_lim1 , dens_lim2 , dens_h2o ,  &
         dens_dust
   real  mass_so4  , mass_no3  , mass_cl   , mass_msa  , mass_co3 ,  &
         mass_nh4  , mass_na   , mass_ca   , mass_oin  , mass_oc  ,  &
         mass_bc   , mass_aro1 , mass_aro2 , mass_alk1 , mass_ole1,  &
         mass_api1 , mass_api2 , mass_lim1 , mass_lim2 , mass_h2o,   &
         mass_dust
   real  mass_so4i , mass_no3i , mass_cli  , mass_msai , mass_co3i,  &
         mass_nh4i , mass_nai  , mass_cai  , mass_oini , mass_oci ,  &
         mass_bci  , mass_aro1i, mass_aro2i, mass_alk1i, mass_ole1i, &
         mass_ba1i , mass_ba2i,  mass_ba3i , mass_ba4i , mass_pai,   &
         mass_h2oi , mass_dusti
   real  mass_so4j , mass_no3j , mass_clj  , mass_msaj , mass_co3j,  &
         mass_nh4j , mass_naj  , mass_caj  , mass_oinj , mass_ocj ,  &
         mass_bcj  , mass_aro1j, mass_aro2j, mass_alk1j, mass_ole1j, &
         mass_ba1j , mass_ba2j,  mass_ba3j , mass_ba4j , mass_paj,   &
         mass_h2oj , mass_dustj
   real  mass_antha, mass_seas, mass_soil
   real  vol_so4   , vol_no3   , vol_cl    , vol_msa   , vol_co3  ,  &
         vol_nh4   , vol_na    , vol_ca    , vol_oin   , vol_oc   ,  &
         vol_bc    , vol_aro1  , vol_aro2  , vol_alk1  , vol_ole1 ,  &
         vol_api1  , vol_api2  , vol_lim1  , vol_lim2  , vol_h2o  ,  & 
         vol_dust
   real  conv1a, conv1b, conv1sulf
   real  mass_dry_a, mass_wet_a, vol_dry_a , vol_wet_a , vol_shell,  &
         dp_dry_a  , dp_wet_a  , num_a     , dp_bc_a
   real  ifac, jfac, cfac
   integer ns
   real  dgnum_um,drydens,duma,dlo_um,dhi_um,dgmin,sixpi,ss1,ss2,ss3,dtemp
   integer  iflag
   real, dimension(1:nbin_o) :: xnum_secti,xnum_sectj,xnum_sectc
   real, dimension(1:nbin_o) :: xmas_secti,xmas_sectj,xmas_sectc
   real, dimension(1:nbin_o) :: xdia_um, xdia_cm
   REAL, PARAMETER :: FRAC2Aitken=0.25   ! Fraction of modal mass in Aitken mode - applied globally to each species

! 7/21/09  SAM variables needed to convert GOCART sectional dust and seasalt to MOZAIC sections
   real  dgnum, dhi,  dlo, xlo, xhi, dxbin, relh_frc
   real dlo_sectm(nbin_o), dhi_sectm(nbin_o)
   integer, parameter :: nbin_omoz=8
   real, save :: seasfrc_goc8bin(4,nbin_omoz)   ! GOCART seasalt size distibution - mass fracs in MOSAIC 8-bins
   real, save :: dustfrc_goc8bin(ndust,nbin_omoz)   ! GOCART dust size distibution - mass fracs in MOSAIC 8-bins
   real  mass_bc1 , mass_bc2 , vol_bc2  , mass_bc1j , mass_bc2j,  &
         mass_bc1i , mass_bc2i  , vol_soil
   real*8 dlogoc, dhigoc
   integer istop
   integer, save :: kcall
   data  kcall / 0 /
   integer :: uoc

  if (uoc == 1) then   ! mklose
     den_dust(1) = 2650.   ! change dust density in first bin for UoC dust emission schemes
  endif

!
!  real  sginin,sginia,sginic from module_data_sorgam.F
! 
! Mass from modal distribution is divided into individual sections before
! being passed back into the Mie routine.
! * currently use the same size bins as 8 default MOSAIC size bins
! * dlo_um and dhi_um define the lower and upper bounds of individual sections
!   used to compute optical properties
! * sigmas for 3 modes taken from module_sorgam_data.F
! * these parameters are needed by sect02 that is called later
! * sginin=1.7, sginia=2.0, sginic=2.5
!
      sixpi=6.0/3.14159265359
      dlo_um=0.0390625
      dhi_um=10.0
      drydens=1.8
      iflag=2
      duma=1.0
      dgmin=1.0e-07 ! in (cm)
      dtemp=dlo_um
      do isize=1,nbin_o
        xdia_um(isize)=(dtemp+dtemp*2.0)/2.0
        dtemp=dtemp*2.0
      enddo
        if (kcall .eq. 0) then
! 7/21/09 SAM calculate sectional contributions from GOCART seasalt and dust
        dlo = dlo_um*1.0e-6
        dhi = dhi_um*1.0e-6
        xlo = log( dlo )
        xhi = log( dhi )
        dxbin = (xhi - xlo)/nbin_o
        do n = 1, nbin_o
            dlo_sectm(n) = exp( xlo + dxbin*(n-1) )
            dhi_sectm(n) = exp( xlo + dxbin*n )
        end do
!  real, save :: seasfrc_goc8bin(4,nbin_o)   ! GOCART seasalt size distibution - mass fracs in MOSAIC 8-bins
!  real, save :: dustfrc_goc8bin(ndust,nbin_o)   ! GOCART dust size distibution - mass fracs in MOSAIC 8-bins
!  USE module_data_gocart_seas
! real*8, DIMENSION (4), PARAMETER :: ra(4)=(/1.d-1,5.d-1,1.5d0,5.0d0/)
! real*8, DIMENSION (4), PARAMETER :: rb(4)=(/5.d-1,1.5d0,5.d0,1.d1/)
! real*8, DIMENSION (4), PARAMETER :: den_seas(4)=(/2.2d3,2.2d3,2.2d3,2.2d3/)
! real*8, DIMENSION (4), PARAMETER :: reff_seas(4)=(/0.30D-6,1.00D-6,3.25D-6,7.50D-6/)
!  USE module_data_gocart_dust, only:  ndust, reff_dust, den_dust
!  real*8, DIMENSION (5), PARAMETER :: den_dust(5)=(/2500.,2650.,2650.,2650.,2650./)
!  real*8, DIMENSION (5), PARAMETER :: reff_dust(5)=(/0.73D-6,1.4D-6,2.4D-6,4.5D-6,8.0D-6/)
! Seasalt bin mass fractions
        seasfrc_goc8bin=0.
!      WRITE(*,*)'Seasalt mass fractions'
!      WRITE(*,*)'         ','       ',(dlo_sectm(n),n=1,nbin_o)
!      WRITE(*,*)'         ','       ',(dhi_sectm(n),n=1,nbin_o)
       do m =1, 4  ! loop over seasalt size bins
       dlogoc = ra(m)*2.E-6  ! low diameter limit (m)
       dhigoc = rb(m)*2.E-6  ! hi diameter limit (m)
        do n = 1, nbin_o
        seasfrc_goc8bin(m,n)=max(DBLE(0.),min(DBLE(dhi_sectm(n)),dhigoc)- &
                             max(dlogoc,DBLE(dlo_sectm(n))) )/(dhigoc-dlogoc)
       end do
!      WRITE(*,*)m,dlogoc,dhigoc,(seasfrc_goc8bin(m,n),n=1,nbin_o)
       end do
! Dust bin mass fractions
!      WRITE(*,*)'Dust mass fractions'
!      WRITE(*,*)'         ','       ',(dlo_sectm(n),n=1,nbin_o)
!      WRITE(*,*)'         ','       ',(dhi_sectm(n),n=1,nbin_o)
        dustfrc_goc8bin=0.
        dlogoc=0.46*2.E-6 ! Begin lower dust bin, makes upper limit diam 20 micron diameter 
       do m =1, ndust  ! loop over dust size bins
       dhigoc = 2.*2.*reff_dust(m)-dlogoc ! hi diameter limit (m)
        do n = 1, nbin_o
        dustfrc_goc8bin(m,n)=max(DBLE(0.),min(DBLE(dhi_sectm(n)),dhigoc)- &
                             max(dlogoc,DBLE(dlo_sectm(n))) )/(dhigoc-dlogoc)
       end do
!      WRITE(*,*)m,dlogoc,dhigoc,(dustfrc_goc8bin(m,n),n=1,nbin_o)
       dlogoc=dhigoc
       end do
        kcall=kcall+1
!       ISTOP=1
!       IF(ISTOP.EQ.1)THEN
!       STOP
!       ENDIF
        endif
!
! Define refractive indicies
! * assume na and cl are the same as nacl
! * assume so4, no3, and nh4 are the same as nh4no3
! * assume ca and co3 are the same as caco3
! * assume msa is just msa
! Further work:
! * to be more precise, need to compute electrolytes to apportion
!   so4, no3, nh4, na, cl, msa, ca, co3 among various componds
!   as was done previously in module_mosaic_therm.F
!
      do ns = 1, nswbands
      swref_index_nh4so4(ns) = cmplx(refrsw_sulf(ns),refisw_sulf(ns))
      swref_index_oc(ns) = cmplx(refrsw_oc(ns),refisw_oc(ns))
      swref_index_dust(ns) = cmplx(refrsw_dust(ns),refisw_dust(ns))
      swref_index_nacl(ns) = cmplx(refrsw_seas(ns),refisw_seas(ns))
      swref_index_h2o(ns) = cmplx(refrwsw(ns),refiwsw(ns))
      enddo
      do ns = 1, nlwbands
      lwref_index_nh4so4(ns) = cmplx(refrlw_sulf(ns),refilw_sulf(ns))
      lwref_index_oc(ns) = cmplx(refrlw_oc(ns),refilw_oc(ns))
      lwref_index_dust(ns) = cmplx(refrlw_dust(ns),refilw_dust(ns))
      lwref_index_nacl(ns) = cmplx(refrlw_seas(ns),refilw_seas(ns))
      lwref_index_h2o(ns) = cmplx(refrwlw(ns),refiwlw(ns))
      enddo
!     ref_index_nh4so4 = cmplx(1.52,0.)
      ref_index_lvcite = cmplx(1.50,0.)
      ref_index_nh4hso4= cmplx(1.47,0.)
      ref_index_nh4msa = cmplx(1.50,0.)     ! assumed
      ref_index_nh4no3 = cmplx(1.50,0.)
      ref_index_nh4cl  = cmplx(1.50,0.)
!     ref_index_nacl   = cmplx(1.45,0.)
      ref_index_nano3  = cmplx(1.50,0.)
      ref_index_na2so4 = cmplx(1.50,0.)
      ref_index_na3hso4= cmplx(1.50,0.)
      ref_index_nahso4 = cmplx(1.50,0.)
      ref_index_namsa  = cmplx(1.50,0.)     ! assumed
      ref_index_caso4  = cmplx(1.56,0.006)
      ref_index_camsa2 = cmplx(1.56,0.006)  ! assumed
      ref_index_cano3  = cmplx(1.56,0.006)
      ref_index_cacl2  = cmplx(1.52,0.006)
      ref_index_caco3  = cmplx(1.68,0.006)
      ref_index_h2so4  = cmplx(1.43,0.)
      ref_index_hhso4  = cmplx(1.43,0.)
      ref_index_hno3   = cmplx(1.50,0.)
      ref_index_hcl    = cmplx(1.50,0.)
      ref_index_msa    = cmplx(1.43,0.)     ! assumed
!     ref_index_oc     = cmplx(1.45,0.)  ! JCB, Feb. 20, 2008: no complex part?
! JCB, Feb. 20, 2008:  set the refractive index of BC equal to the
! midpoint of ranges given in Bond and Bergstrom, Light absorption by
! carboneceous particles: an investigative review 2006, Aerosol Sci.
! and Tech., 40:27-67.
!     ref_index_bc     = cmplx(1.82,0.74) old value
      ref_index_bc     = cmplx(1.85,0.71)
      ref_index_oin    = cmplx(1.55,0.006)  ! JCB, Feb. 20, 2008:  "other inorganics"
      ref_index_aro1   = cmplx(1.45,0.)
      ref_index_aro2   = cmplx(1.45,0.)
      ref_index_alk1   = cmplx(1.45,0.)
      ref_index_ole1   = cmplx(1.45,0.)
      ref_index_api1   = cmplx(1.45,0.)
      ref_index_api2   = cmplx(1.45,0.)
      ref_index_lim1   = cmplx(1.45,0.)
      ref_index_lim2   = cmplx(1.45,0.)
!     ref_index_h2o    = cmplx(1.33,0.)
!
! densities in g/cc
!
      dens_so4   = 1.8        ! used
      dens_no3   = 1.8        ! used
      dens_cl    = 2.2        ! used
      dens_msa   = 1.8        ! used
      dens_co3   = 2.6        ! used
      dens_nh4   = 1.8        ! used
      dens_na    = 2.2        ! used
      dens_ca    = 2.6        ! used
      dens_oin   = 2.6        ! used
      dens_dust  = 2.6        ! used
      dens_oc    = 1.0        ! used
! JCB, Feb. 20, 2008:  the density of BC is updated to reflect values
! published by Bond and Bergstrom, Light absorption by carboneceous
! particles: an investigative review 2006, Aerosol Sci. and Tech., 40:27-67.
!      dens_bc    = 1.7        ! used, old value
      dens_bc    =  1.8        !  midpoint of Bond and Bergstrom value
      dens_aro1  = 1.0
      dens_aro2  = 1.0
      dens_alk1  = 1.0
      dens_ole1  = 1.0
      dens_api1  = 1.0
      dens_api2  = 1.0
      dens_lim1  = 1.0
      dens_lim2  = 1.0
      dens_h2o   = 1.0
!
      swrefindx=0.0
      lwrefindx=0.0
      radius_wet=0.0
      number_bin=0.0
      radius_core=0.0
      swrefindx_core=0.0
      swrefindx_shell=0.0
      lwrefindx_core=0.0
      lwrefindx_shell=0.0
!
! units:
! * mass     - g/cc(air)
! * number   - #/cc(air)
! * volume   - cc(air)/cc(air)
! * diameter - cm
!
      do j = jts, jte
      do k = kts, kte
      do i = its, ite
        mass_so4i = 0.0
        mass_so4j = 0.0
        mass_no3i = 0.0
        mass_no3j = 0.0
        mass_nh4i = 0.0
        mass_nh4j = 0.0
        mass_oini = 0.0
        mass_oinj = 0.0
        mass_dusti = 0.0
        mass_dustj = 0.0
        mass_aro1i = 0.0
        mass_aro1j = 0.0
        mass_aro2i = 0.0
        mass_aro2j = 0.0
        mass_alk1i = 0.0
        mass_alk1j = 0.0
        mass_ole1i = 0.0
        mass_ole1j = 0.0
        mass_ba1i = 0.0
        mass_ba1j = 0.0
        mass_ba2i = 0.0
        mass_ba2j = 0.0
        mass_ba3i = 0.0
        mass_ba3j = 0.0
        mass_ba4i = 0.0
        mass_ba4j = 0.0
        mass_pai = 0.0
        mass_paj = 0.0
        mass_oci = 0.0
        mass_ocj = 0.0
        mass_bci = 0.0
        mass_bcj = 0.0
        mass_bc1i = 0.0
        mass_bc1j = 0.0
        mass_bc2i = 0.0
        mass_bc2j = 0.0
        mass_cai = 0.0
        mass_caj = 0.0
        mass_co3i = 0.0
        mass_co3j = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_msai = 0.0
        mass_msaj = 0.0
        mass_nai = 0.0
        mass_naj = 0.0
        mass_cli = 0.0
        mass_clj = 0.0
        mass_h2oi = 0.0
        mass_h2oj = 0.0
        mass_antha = 0.0
        mass_seas = 0.0
        mass_soil = 0.0
        mass_cl = 0.0
        mass_na = 0.0
        mass_msa = 0.0

! convert ug / kg dry air to g / cc  air
        conv1a = (1.0/alt(i,k,j)) * 1.0e-12
! convert # / kg dry air to # / cc  air
        conv1b = (1.0/alt(i,k,j)) * 1.0e-6
! convert ppmv sulfate (and coincidentally MSA) to g / cc  air
        conv1sulf = (1.0/alt(i,k,j)) * 1.0e-9 * 96./28.97

! Accumulation mode...
! SAM 7/18/09 - Put fraction of GOCART sulfate, organic, black carbon masses into modal accumulation mode
        mass_oinj = (1.-FRAC2Aitken)*chem(i,k,j,p_p25)*conv1a
        mass_so4j= (1.-FRAC2Aitken)*chem(i,k,j,p_sulf)*conv1sulf
        mass_nh4j= (1.-FRAC2Aitken)*chem(i,k,j,p_sulf)*conv1sulf*(nh4_mfac-1.)
        mass_aro1j= (1.-FRAC2Aitken)*chem(i,k,j,p_oc1)*conv1a*oc_mfac
        mass_aro2j= (1.-FRAC2Aitken)*chem(i,k,j,p_oc2)*conv1a*oc_mfac
        mass_bc1j= (1.-FRAC2Aitken)*chem(i,k,j,p_bc1)*conv1a
        mass_bc2j= (1.-FRAC2Aitken)*chem(i,k,j,p_bc2)*conv1a
        mass_bcj= mass_bc1j + mass_bc2j
        if( p_msa .gt. 1) mass_msaj= (1.-FRAC2Aitken)*chem(i,k,j,p_msa)*conv1sulf


! Aitken mode...
! SAM 7/18/09 - Put fraction of GOCART sulfate, organic, black carbon masses into modal Aitken mode
        mass_oini = FRAC2Aitken*chem(i,k,j,p_p25)*conv1a
        mass_so4i= FRAC2Aitken*chem(i,k,j,p_sulf)*conv1sulf
        mass_nh4i= FRAC2Aitken*chem(i,k,j,p_sulf)*conv1sulf*(nh4_mfac-1.)
        mass_aro1i= FRAC2Aitken*chem(i,k,j,p_oc1)*conv1a*oc_mfac
        mass_aro2i= FRAC2Aitken*chem(i,k,j,p_oc2)*conv1a*oc_mfac
        mass_bc1i= FRAC2Aitken*chem(i,k,j,p_bc1)*conv1a
        mass_bc2i= FRAC2Aitken*chem(i,k,j,p_bc2)*conv1a
        mass_bci= mass_bc1i + mass_bc2i
        if( p_msa .gt. 1) mass_msai= FRAC2Aitken*chem(i,k,j,p_msa)*conv1sulf

!
! Now divide mass into sections which is done by sect02:
! * xmas_secti is for aiken mode
! * xmas_sectj is for accumulation mode
! * xmas_sectc is for coarse mode
! * sect02 expects input in um
! * pass in generic mass of 1.0 just to get a percentage distribution of mass among bins
!
!!      ss1=log(sginin)
!!      ss2=exp(ss1*ss1*36.0/8.0) 
!!      ss3=(sixpi*vol_ai/(num_ai*ss2))**0.3333333
!!      dgnum_um=amax1(dgmin,ss3)*1.0e+04
        dgnum_um=dginin*1.E6
        call sect02(dgnum_um,sginin,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_secti,xmas_secti)
!!      ss1=log(sginia)
!!      ss2=exp(ss1*ss1*36.0/8.0) 
!!      ss3=(sixpi*vol_aj/(num_aj*ss2))**0.3333333
!!      dgnum_um=amax1(dgmin,ss3)*1.0e+04
        dgnum_um=dginia*1.E6
        call sect02(dgnum_um,sginia,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectj,xmas_sectj)
!!      ss1=log(sginic)
!!      ss2=exp(ss1*ss1*36.0/8.0) 
!!      ss3=(sixpi*vol_ac/(num_ac*ss2))**0.3333333
        dgnum_um=dginic*1.E6
        call sect02(dgnum_um,sginic,drydens,iflag,duma,nbin_o,dlo_um,dhi_um, &
          xnum_sectc,xmas_sectc)

        do isize = 1, nbin_o
          xdia_cm(isize)=xdia_um(isize)*1.0e-04
          mass_so4 = mass_so4i*xmas_secti(isize) + mass_so4j*xmas_sectj(isize)
          mass_no3 = mass_no3i*xmas_secti(isize) + mass_no3j*xmas_sectj(isize)
          mass_nh4 = mass_nh4i*xmas_secti(isize) + mass_nh4j*xmas_sectj(isize)
          mass_oin = mass_oini*xmas_secti(isize) + mass_oinj*xmas_sectj(isize) + &
                     mass_soil*xmas_sectc(isize) + mass_antha*xmas_sectc(isize)
          if( p_msa .gt. 1) then
             mass_msa = mass_msai*xmas_secti(isize) + mass_msaj*xmas_sectj(isize)
          endif
! GOCART OC mass_aero1 is hydrophobic, mass_aero2 is hydrophylic
          mass_aro1 = mass_aro1j*xmas_sectj(isize) + mass_aro1i*xmas_secti(isize)
          mass_aro2 = mass_aro2j*xmas_sectj(isize) + mass_aro2i*xmas_secti(isize)
          mass_oc = mass_aro1 + mass_aro2
! GOCART BC mass_bc1 is hydrophobic, mass_bc2 is hydrophylic
          mass_bc1  = mass_bc1i*xmas_secti(isize) + mass_bc1j*xmas_sectj(isize)
          mass_bc2  = mass_bc2i*xmas_secti(isize) + mass_bc2j*xmas_sectj(isize)
          mass_bc = mass_bc1 + mass_bc2
! Add in seasalt and dust from GOCART sectional distributions
       n = 0
       mass_seas = 0.0
       do m =p_seas_1,  p_seas_3 ! loop over seasalt size bins less than 10 um diam
       n = n+1
        mass_seas=mass_seas+seasfrc_goc8bin(n,isize)*chem(i,k,j,m)
       end do
       n = 0
       mass_soil = 0.0
       do m =p_dust_1,  p_dust_1+ndust-2 ! loop over dust size bins less than 10 um diam
       n = n+1
        mass_soil=mass_soil+dustfrc_goc8bin(n,isize)*chem(i,k,j,m)
       end do
       mass_cl=mass_seas*conv1a*35.4530/58.4428
       mass_na=mass_seas*conv1a*22.9898/58.4428
       mass_soil=mass_soil*conv1a
!         mass_h2o = 0.0 ! testing purposes only
          vol_so4 = mass_so4 / dens_so4
          vol_no3 = mass_no3 / dens_no3
          vol_nh4 = mass_nh4 / dens_nh4
          vol_oin = mass_oin / dens_oin
          vol_oc  = mass_oc  / dens_oc
          vol_aro2 = mass_aro2 / dens_oc
          vol_bc  = mass_bc  / dens_bc
          vol_bc2 = mass_bc2 / dens_bc
          vol_na  = mass_na  / dens_na
          vol_cl  = mass_cl  / dens_cl
          vol_soil  = mass_soil  / dens_dust
          vol_msa  = mass_msa  / dens_msa
!         vol_h2o = mass_h2o / dens_h2o
! 7/23/09 SAM calculate vol_h2o from kappas in Petters and Kreidenweis ACP, 2007, vol. 7, 1961-1971.
!  Their kappas are the hygroscopicities used in Abdul-Razzak and Ghan, 2004, JGR, V105, p. 6837-6844. 
!  These kappas are defined in module_data_sorgam and module_data_mosaic_asect.
!  Note that hygroscopicities are at 298K and specific surface tension - further refinement could
!  include temperature dependence in Petters and Kreidenweis
! Also, for hygroscopic BC part, assume kappa of OC (how can BC be hydrophylic?)
               relh_frc=amin1(.9,relhum(i,k,j)) !0.8   ! Put in fractional relative humidity, max of .9, here
                  vol_h2o = vol_so4*hygro_so4_aer + vol_aro2*hygro_oc_aer + &
                            vol_nh4*hygro_nh4_aer                         + &
                  vol_cl*hygro_cl_aer + vol_na*hygro_na_aer + vol_msa*hygro_msa_aer + &
                  vol_oin*hygro_oin_aer + vol_bc2*hygro_oc_aer  + vol_soil*hygro_dust_aer
                  vol_h2o = relh_frc*vol_h2o/(1.-relh_frc)
                  mass_h2o = vol_h2o*dens_h2o
          mass_dry_a = mass_so4 + mass_no3 + mass_nh4 + mass_oin + &
                       mass_oc  + mass_bc  + mass_na  + mass_cl  + &
                       mass_soil
          mass_wet_a = mass_dry_a + mass_h2o 
          vol_dry_a  = vol_so4  + vol_no3  + vol_nh4  + vol_oin  + &
                       vol_oc   + vol_bc   + vol_na   + vol_cl  + &
                       vol_soil
          vol_wet_a  = vol_dry_a + vol_h2o
          vol_shell  = vol_wet_a - vol_bc
          num_a      = vol_wet_a / (0.52359877*xdia_cm(isize)*xdia_cm(isize)*xdia_cm(isize))

          !shortwave 
          do ns=1,nswbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
                       (ref_index_oin    * mass_oin / dens_oin) +  &
                       (swref_index_dust(ns) * mass_soil / dens_dust) + &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (swref_index_h2o(ns) * mass_h2o / dens_h2o) 
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize)
            dp_wet_a   = xdia_cm(isize)
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (swref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (ref_index_oin    * mass_oin / dens_oin) +  &
                       (swref_index_dust(ns)    * mass_soil / dens_dust) + &
                       (swref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (swref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (swref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (swref_index_h2o(ns)    * mass_h2o / dens_h2o) 
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            swrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            swrefindx_core(i,k,j,isize,ns) = ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            swrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            swrefindx_core(i,k,j,isize,ns) =ref_index_bc
            swrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns shortwave

          !longwave 
          do ns=1,nlwbands
          ri_dum     = (0.0,0.0)
          ri_dum     = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
                       (ref_index_oin    * mass_oin / dens_oin) +  &
                       (lwref_index_dust(ns) * mass_soil / dens_dust) + &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (ref_index_bc     * mass_bc  / dens_bc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (ref_index_msa    * mass_msa / dens_msa) +  &
                       (lwref_index_h2o(ns) * mass_h2o / dens_h2o) 
!
! for some reason MADE/SORGAM occasionally produces zero aerosols so
! need to add a check here to avoid divide by zero
!
          IF(num_a .lt. 1.0e-20 .or. vol_wet_a .lt. 1.0e-20) then
            dp_dry_a   = xdia_cm(isize) 
            dp_wet_a   = xdia_cm(isize) 
            dp_bc_a    = xdia_cm(isize)
            ri_ave_a   = 0.0
            ri_dum     = 0.0
          else
            dp_dry_a   = (1.90985*vol_dry_a/num_a)**0.3333333
            dp_wet_a   = (1.90985*vol_wet_a/num_a)**0.3333333
            dp_bc_a    = (1.90985*vol_bc/num_a)**0.3333333
            ri_ave_a   = ri_dum/vol_wet_a
            ri_dum   = (lwref_index_nh4so4(ns) * mass_so4 / dens_so4) +  &
                       (ref_index_nh4no3 * mass_no3 / dens_no3) +  &
                       (ref_index_nh4no3 * mass_nh4 / dens_nh4) +  &
                       (ref_index_oin    * mass_oin / dens_oin) +  &
                       (lwref_index_dust(ns)    * mass_oin / dens_dust)  +  &
                       (lwref_index_oc(ns)     * mass_oc  / dens_oc) +   &
                       (lwref_index_nacl(ns)   * mass_na  / dens_na) +   &
                       (lwref_index_nacl(ns)   * mass_cl  / dens_cl) +   &
                       (lwref_index_h2o(ns)    * mass_h2o / dens_h2o)
          endif
          if(dp_wet_a/2.0 .lt. dlo_um*1.0e-4/2.0) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          elseif(num_a .lt. 1.e-20 .or. vol_shell .lt. 1.0e-20) then
            lwrefindx(i,k,j,isize,ns)    = (1.5,0.0)
            radius_wet(i,k,j,isize) =dlo_um*1.0e-4/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =0.0
            lwrefindx_core(i,k,j,isize,ns) = ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) = ref_index_oin
          else
            lwrefindx(i,k,j,isize,ns)    =ri_ave_a
            radius_wet(i,k,j,isize) =dp_wet_a/2.0
            number_bin(i,k,j,isize) =num_a
            radius_core(i,k,j,isize) =dp_bc_a/2.0
            lwrefindx_core(i,k,j,isize,ns) =ref_index_bc
            lwrefindx_shell(i,k,j,isize,ns) =ri_dum/vol_shell
          endif
        enddo  ! ns longwave
!  refr=real(refindx(i,k,j,isize))
        enddo  !isize
      enddo  !i
      enddo  !j
      enddo  !k

      return

      end subroutine optical_prep_gocart

!below is the detail calculation for MIE code
!czhao 

!
!***********************************************************************
! <1.> subr mieaer
!czhao made changes for doing both shortwave and longwave optical properties
! Purpose:  calculate aerosol optical depth, single scattering albedo,
!   asymmetry factor, extinction, Legendre coefficients, and average aerosol
!   size. parameterizes aerosol coefficients using chebychev polynomials
!   requires double precision on 32-bit machines
!   uses Wiscombe's (1979) mie scattering code
! INPUT
!       id -- grid id number
!   	iclm, jclm -- i,j of grid column being processed
!       nbin_a -- number of bins
!	number_bin(nbin_a,kmaxd) --   number density in layer, #/cm^3
!	radius_wet(nbin_a,kmaxd) -- wet radius, cm
!    	refindx(nbin_a,kmaxd) --volume averaged complex index of refraction
!	dz -- depth of individual cells in column, m
!	curr_secs -- time from start of run, sec
!	lpar -- number of grid cells in vertical (via module_fastj_cmnh)
!   kmaxd -- predefined maximum allowed levels from module_data_mosaic_other
!            passed here via module_fastj_cmnh
! OUTPUT: saved in module_fastj_cmnmie
!   real tauaer  ! aerosol optical depth
!        waer    ! aerosol single scattering albedo
!        gaer    ! aerosol asymmetery factor
!        extaer  ! aerosol extinction
!        l2,l3,l4,l5,l6,l7 ! Legendre coefficients, numbered 0,1,2,......
!        sizeaer ! average wet radius
!	 bscoef ! aerosol backscatter coefficient with units km-1 * steradian -1  JCB 2007/02/01
!***********************************************************************
        subroutine mieaer( &
	          id, iclm, jclm, nbin_a,   &
              number_bin_col, radius_wet_col, swrefindx_col,   &
              lwrefindx_col,   &
              dz, curr_secs, kts,kte, &
!             sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7,bscoef)  ! added bscoef JCB 2007/02/01
              swsizeaer,swextaer,swwaer,swgaer,swtauaer,lwextaer,lwtauaer, & 
              l2,l3,l4,l5,l6,l7,swbscoef)  ! added bscoef JCB 2007/02/01

!       USE module_data_mosaic_other, only : kmaxd
!       USE module_data_mosaic_therm, ONLY: nbin_a_maxd
        USE module_peg_util, only:  peg_error_fatal, peg_message
        
        IMPLICIT NONE

        integer,parameter :: nspint = 4 ! Num of spectral for FAST-J
        integer, intent(in) :: kts,kte 
        integer, intent(in) :: id, iclm, jclm, nbin_a
        real(kind=8), intent(in) :: curr_secs

        real, dimension (1:nspint,kts:kte),intent(out) :: swsizeaer,swextaer,swwaer,swgaer,swtauaer
        real, dimension (1:nlwbands,kts:kte),intent(out) :: lwextaer,lwtauaer
        real, dimension (1:nspint,kts:kte),intent(out) :: l2,l3,l4,l5,l6,l7
        real, dimension (1:nspint,kts:kte),intent(out) :: swbscoef  !JCB 2007/02/01
        real, intent(in), dimension(1:nbin_a, kts:kte) :: number_bin_col
        real, intent(inout), dimension(1:nbin_a,kts:kte) :: radius_wet_col
        complex, intent(in),dimension(1:nbin_a,kts:kte,nspint) :: swrefindx_col
        complex, intent(in),dimension(1:nbin_a,kts:kte,nlwbands) :: lwrefindx_col
        real, intent(in),dimension(kts:kte)   :: dz

        !fitting variables
        integer ltype ! total number of indicies of refraction
        parameter (ltype = 1)  ! bracket refractive indices based on information from Rahul, 2002/11/07
        integer nrefr,nrefi,nr,ni
        save nrefr,nrefi
        complex*16 sforw,sback,tforw(2),tback(2)
        real*8 pmom(0:7,1)
        logical, save :: ini_fit  ! initial mie fit only for the first time step
        data ini_fit/.true./
        ! nsiz = number of wet particle sizes
        integer, parameter ::  nsiz=200,nlog=30 !,ncoef=5
        real p2(nsiz),p3(nsiz),p4(nsiz),p5(nsiz)
        real p6(nsiz),p7(nsiz)
        logical perfct,anyang,prnt(2)
        real*8 xmu(1)
        data xmu/1./,anyang/.false./
        data prnt/.false.,.false./
        integer numang,nmom,ipolzn,momdim
        data numang/0/
        complex*16 s1(1),s2(1)
        real*8 mimcut
        data perfct/.false./,mimcut/0.0/
        data nmom/7/,ipolzn/0/,momdim/7/
        integer n
        real*8 thesize    ! 2 pi radpart / waveleng = size parameter
        real*8 qext(nsiz) ! array of extinction efficiencies
        real*8 qsca(nsiz) ! array of scattering efficiencies
        real*8 gqsc(nsiz) ! array of asymmetry factor * scattering efficiency
        real qext4(nsiz)          !  extinction, real*4
        real qsca4(nsiz)          !  extinction, real*4
        real qabs4(nsiz)          !  extinction, real*4
        real asymm(nsiz)  ! array of asymmetry factor
        real sb2(nsiz)     ! JCB 2007/02/01 - 4*abs(sback)^2/(size parameter)^2 backscattering efficiency
        complex*16 crefin,crefd,crefw
        save crefw
        real, save :: rmin,rmax   ! min, max aerosol size bin
        real bma,bpa
        real refr     ! real part of refractive index
        real refi     ! imaginary part of refractive index
        real refrmin ! minimum of real part of refractive index
        real refrmax ! maximum of real part of refractive index
        real refimin ! minimum of imag part of refractive index
        real refimax ! maximum of imag part of refractive index
        real drefr ! increment in real part of refractive index
        real drefi ! increment in imag part of refractive index
        complex specrefndx(ltype) ! refractivr indices
        integer, parameter ::  naerosols=5

        !parameterization variables
	real weighte, weights,weighta
	real x
	real thesum ! for normalizing things
	real sizem ! size in microns
        integer m, j, nc, klevel
        real pext           ! parameterized specific extinction (cm2/g)
        real pscat      !scattering cross section
        real pabs           ! parameterized specific extinction (cm2/g)
        real pasm       ! parameterized asymmetry factor
        real ppmom2     ! 2 Lengendre expansion coefficient (numbered 0,1,2,...)
        real ppmom3     ! 3     ...
        real ppmom4     ! 4     ...
        real ppmom5     ! 5     ...
        real ppmom6     ! 6     ...
        real ppmom7     ! 7     ...
        real sback2     ! JCB 2007/02/01 sback*conjg(sback)
        real cext(ncoef),casm(ncoef),cpmom2(ncoef),cabs(ncoef)
        real cscat(ncoef)  ! JCB 2004/02/09
        real cpmom3(ncoef),cpmom4(ncoef),cpmom5(ncoef)
        real cpmom6(ncoef),cpmom7(ncoef)
        real cpsback2p(ncoef) ! JCB 2007/02/09  - backscatter
        integer itab,jtab
        real ttab,utab
        real, save :: xrmin,xrmax,xr
        real rs(nsiz) ! surface mode radius (cm)
        real xrad ! normalized aerosol radius
        real ch(ncoef) ! chebychev polynomial


        !others
        integer i,k,l,ns  
        real pie,third
        integer ibin
        character*150 msg
        integer kcallmieaer,kcallmieaer2


#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec  diagnostics
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec run_out.25 has aerosol physical parameter info for bins 1-8
!ec and vertical cells 1 to kmaxd.
	if (iclm .eq. CHEM_DBG_I) then
	  if (jclm .eq. CHEM_DBG_J) then
!   initial entry
	   if (kcallmieaer2 .eq. 0) then
	      write(*,9099)iclm, jclm
 9099	format('for cell i = ', i3, 2x, 'j = ', i3)	
              write(*,9100)
 9100     format(   &
               'curr_secs', 3x, 'i', 3x, 'j', 3x,'k', 3x,   &
      	       'ibin', 3x,   &
               'refindx_col(ibin,k)', 3x,   &
               'radius_wet_col(ibin,k)', 3x,   &
               'number_bin_col(ibin,k)'   &
               )
	   end if
!ec output for run_out.25
            do k = 1,kte 
            do ibin = 1, nbin_a
              write(*, 9120)   &
                 curr_secs,iclm, jclm, k, ibin,   &
                 swrefindx_col(ibin,k),   &
                 radius_wet_col(ibin,k),   &
                 number_bin_col(ibin,k)
9120    format( i7,3(2x,i4),2x,i4, 4x, 4(e14.6,2x))
            end do
            end do
        kcallmieaer2 = kcallmieaer2 + 1
        end if
        end if
!ec end print of aerosol physical parameter diagnostics	
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#endif
!
! assign fast-J wavelength, these values are in cm
!      wavmid(1)=0.30e-4
!      wavmid(2)=0.40e-4
!      wavmid(3)=0.60e-4
!      wavmid(4)=0.999e-04
!
      pie=4.*atan(1.)
      third=1./3.
      rmin=rmmin
      rmax=rmmax

!######################################################################
!initial fitting to mie calculation based on Ghan et al. 2002 and 2007
!#####################################################################
      if(ini_fit)then
        ini_fit=.false.

  !----------------------------------------------------------------------
  !shortwave
  !---------------------------------------------------------------------
   ! wavelength loop
     do 200 ns=1,nspint
     ! parameterize aerosol radiative properties in terms of
     ! relative humidity, surface mode wet radius, aerosol species,
     ! and wavelength
     ! first find min,max of real and imaginary parts of refractive index
     ! real and imaginary parts of water refractive index

        !crefw=cmplx(1.33,0.0)
        crefwsw(ns)=cmplx(refrwsw(ns),refiwsw(ns))
        refrmin=real(crefwsw(ns))
        refrmax=real(crefwsw(ns))
     !change imaginary part of the refractive index from positive to negative
        refimin=-imag(crefwsw(ns))
        refimax=-imag(crefwsw(ns))
        !specrefndx(1)=cmplx(1.85,-0.71) ! max values from Bond and Bergstrom
!        do i=1,ltype ! loop over all possible refractive indices
!          refrmin=amin1(refrmin,real(specrefndx(ltype)))
!          refrmax=amax1(refrmax,real(specrefndx(ltype)))
!          refimin=amin1(refimin,aimag(specrefndx(ltype)))
!          refimax=amax1(refimax,aimag(specrefndx(ltype)))
!        enddo
!           aerosol species loop (dust, BC, OC, Sea Salt, and Sulfate)

        do l=1,naerosols
          if (l==1) refr=refrsw_dust(ns)
          if (l==1) refi=-refisw_dust(ns)
          if (l==2) refr=refrsw_bc(ns)
          if (l==2) refi=-refisw_bc(ns)
          if (l==3) refr=refrsw_oc(ns)
          if (l==3) refi=-refisw_oc(ns)
          if (l==4) refr=refrsw_seas(ns)
          if (l==4) refi=-refisw_seas(ns)
          if (l==5) refr=refrsw_sulf(ns)
          if (l==5) refi=-refisw_sulf(ns)
          refrmin=min(refrmin,refr)
          refrmax=max(refrmax,refr)
          refimin=min(refimin,refi)
          refimax=max(refimax,refi)
        enddo

         drefr=(refrmax-refrmin)
         if(drefr.gt.1.e-4)then
            nrefr=prefr
            drefr=drefr/(nrefr-1)
         else
            nrefr=1
         endif

         drefi=(refimax-refimin)
         if(drefi.gt.1.e-4)then
            nrefi=prefi
            drefi=drefi/(nrefi-1)
         else
            nrefi=1
         endif

         bma=0.5*log(rmax/rmin) ! JCB
         bpa=0.5*log(rmax*rmin) ! JCB

           do 120 nr=1,nrefr
           do 120 ni=1,nrefi

               refrtabsw(nr,ns)=refrmin+(nr-1)*drefr
               refitabsw(ni,ns)=refimin/0.2*(0.2**real(ni))  !slightly different from Ghan and Zaveri
               if(ni.eq.nrefi) refitabsw(ni,ns)=-1.0e-20  ! JCB change
               crefd=cmplx(refrtabsw(nr,ns),refitabsw(ni,ns))

!              mie calculations of optical efficiencies
               do n=1,nsiz
                 xr=cos(pie*(float(n)-0.5)/float(nsiz))
                 rs(n)=exp(xr*bma+bpa)

!                size parameter and weighted refractive index
                 thesize=2.*pie*rs(n)/wavmidsw(ns)
                 thesize=min(thesize,10000.d0)

                  call miev0(thesize,crefd,perfct,mimcut,anyang,   &
                     numang,xmu,nmom,ipolzn,momdim,prnt,   &
                     qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,   &
                     s2,tforw,tback )
                  qext4(n)=qext(n)
                  qsca4(n)=min(qsca(n),qext(n)) 
                  qabs4(n)=qext4(n)-qsca4(n)
                  qabs4(n)=max(qabs4(n),1.e-20) ! avoid 0 
                  asymm(n)=gqsc(n)/qsca4(n) ! assume always greater than zero
! coefficients of phase function expansion; note modification by JCB of miev0 coefficients
                  p2(n)=pmom(2,1)/pmom(0,1)*5.0
                  p3(n)=pmom(3,1)/pmom(0,1)*7.0
                  p4(n)=pmom(4,1)/pmom(0,1)*9.0
                  p5(n)=pmom(5,1)/pmom(0,1)*11.0
                  p6(n)=pmom(6,1)/pmom(0,1)*13.0
                  p7(n)=pmom(7,1)/pmom(0,1)*15.0
! backscattering efficiency, Bohren and Huffman, page 122
! as stated by Bohren and Huffman, this is 4*pie times what is should be
! may need to be smoothed - a very rough function - for the time being we won't apply smoothing
! and let the integration over the size distribution be the smoothing
                  sb2(n)=4.0*sback*dconjg(sback)/(thesize*thesize) ! JCB 2007/02/01  
!
!PMA makes sure it is greater than zero
!              
                  qext4(n)=max(qext4(n),1.e-20)
                  qabs4(n)=max(qabs4(n),1.e-20)
                  qsca4(n)=max(qsca4(n),1.e-20)
                  asymm(n)=max(asymm(n),1.e-20)
                  sb2(n)=max(sb2(n),1.e-20)

                enddo

               call fitcurv(rs,qext4,extpsw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qabs4,abspsw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qsca4,ascatpsw(1,nr,ni,ns),ncoef,nsiz) ! scattering efficiency
               call fitcurv(rs,asymm,asmpsw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,sb2,sbackpsw(1,nr,ni,ns),ncoef,nsiz) ! backscattering efficiency             
               call fitcurv_nolog(rs,p2,pmom2psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p3,pmom3psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p4,pmom4psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p5,pmom5psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p6,pmom6psw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv_nolog(rs,p7,pmom7psw(1,nr,ni,ns),ncoef,nsiz)

  120       continue
  200    continue     ! ns for shortwave


  !----------------------------------------------------------------------
  !longwave
  !---------------------------------------------------------------------
   ! wavelength loop
     do 201 ns=1,nlwbands
        !wavelength for longwave 1/cm --> cm
        wavmidlw(ns) = 0.5*(1./wavenumber1_longwave(ns) + 1./wavenumber2_longwave(ns))

        crefwlw(ns)=cmplx(refrwlw(ns),refiwlw(ns))
        refrmin=real(crefwlw(ns))
        refrmax=real(crefwlw(ns))
        refimin=-imag(crefwlw(ns))
        refimax=-imag(crefwlw(ns))

        !aerosol species loop (dust, BC, OC, Sea Salt, and Sulfate)
        do l=1,naerosols
          if (l==1) refr=refrlw_dust(ns)
          if (l==1) refi=-refilw_dust(ns)
          if (l==2) refr=refrlw_bc(ns)
          if (l==2) refi=-refilw_bc(ns)
          if (l==3) refr=refrlw_oc(ns)
          if (l==3) refi=-refilw_oc(ns)
          if (l==4) refr=refrlw_seas(ns)
          if (l==4) refi=-refilw_seas(ns)
          if (l==5) refr=refrlw_sulf(ns)
          if (l==5) refi=-refilw_sulf(ns)
          refrmin=min(refrmin,refr)
          refrmax=max(refrmax,refr)
          refimin=min(refimin,refi)
          refimax=max(refimax,refi)
        enddo

         drefr=(refrmax-refrmin)
         if(drefr.gt.1.e-4)then
            nrefr=prefr
            drefr=drefr/(nrefr-1)
         else
            nrefr=1
         endif

         drefi=(refimax-refimin)
         if(drefi.gt.1.e-4)then
            nrefi=prefi
            drefi=drefi/(nrefi-1)
         else
            nrefi=1
         endif

         bma=0.5*log(rmax/rmin) ! JCB
         bpa=0.5*log(rmax*rmin) ! JCB

           do 121 nr=1,nrefr
           do 121 ni=1,nrefi

               refrtablw(nr,ns)=refrmin+(nr-1)*drefr
               refitablw(ni,ns)=refimin/0.2*(0.2**real(ni))  !slightly different from Ghan and Zaveri
               if(ni.eq.nrefi) refitablw(nrefi,ns)=-1.0e-21  ! JCB change
               crefd=cmplx(refrtablw(nr,ns),refitablw(ni,ns))

!              mie calculations of optical efficiencies
               do n=1,nsiz
                 xr=cos(pie*(float(n)-0.5)/float(nsiz))
                 rs(n)=exp(xr*bma+bpa)

!                size parameter and weighted refractive index
                 thesize=2.*pie*rs(n)/wavmidlw(ns)
                 thesize=min(thesize,10000.d0)

                  call miev0(thesize,crefd,perfct,mimcut,anyang,   &
                     numang,xmu,nmom,ipolzn,momdim,prnt,   &
                     qext(n),qsca(n),gqsc(n),pmom,sforw,sback,s1,   &
                     s2,tforw,tback )
                  qext4(n)=qext(n)
                  qext4(n)=max(qext4(n),1.e-20) ! avoid 0 
                  qsca4(n)=min(qsca(n),qext(n))
                  qsca4(n)=max(qsca4(n),1.e-20) ! avoid 0 
                  qabs4(n)=qext4(n)-qsca4(n)
                  qabs4(n)=max(qabs4(n),1.e-20) ! avoid 0 
                  asymm(n)=gqsc(n)/qsca4(n) ! assume always greater than zero
                  asymm(n)=max(asymm(n),1.e-20) !PMA makes sure it is greater than zero
                enddo
!
               !if (nr==1.and.ni==1) then
               !endif
               call fitcurv(rs,qext4,extplw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qabs4,absplw(1,nr,ni,ns),ncoef,nsiz)
               call fitcurv(rs,qsca4,ascatplw(1,nr,ni,ns),ncoef,nsiz) ! scattering efficiency
               call fitcurv(rs,asymm,asmplw(1,nr,ni,ns),ncoef,nsiz)
  121       continue
  201    continue     ! ns for longwave


      endif !ini_fit 


         xrmin=log(rmin)
         xrmax=log(rmax)

!######################################################################
!parameterization of mie calculation for shortwave 
!#####################################################################

! begin level loop
        do 2000 klevel=1,kte
! sum densities for normalization
        thesum=0.0
        do m=1,nbin_a
        thesum=thesum+number_bin_col(m,klevel)
        enddo
! Begin shortwave spectral loop
      do 1000 ns=1,nswbands

!        aerosol optical properties
             swtauaer(ns,klevel)=0.
             swwaer(ns,klevel)=0.
             swgaer(ns,klevel)=0.
             swsizeaer(ns,klevel)=0.0
             swextaer(ns,klevel)=0.0
             l2(ns,klevel)=0.0
             l3(ns,klevel)=0.0
             l4(ns,klevel)=0.0
             l5(ns,klevel)=0.0
             l6(ns,klevel)=0.0
             l7(ns,klevel)=0.0
             swbscoef(ns,klevel)=0.0  ! JCB 2007/02/01 - backscattering coefficient
             if(thesum.le.1e-21)goto 1000 ! set everything = 0 if no aerosol !wig changed 0.0 to 1e-21, 31-Oct-2005

! loop over the bins
               do m=1,nbin_a ! nbin_a is number of bins
! here's the size
                sizem=radius_wet_col(m,klevel) ! radius in cm

          !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          !ec  diagnostics
          !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          ! check limits of particle size
          ! rce 2004-dec-07 - use klevel in write statements
                if(radius_wet_col(m,klevel).le.rmin)then
                  radius_wet_col(m,klevel)=rmin
                  write( msg, '(a, 5i4,1x, e11.4)' )	&
                  'mieaer: radius_wet set to rmin,'  //	&
                  'id,i,j,k,m,rm(m,k)', id, iclm, jclm, klevel, m, radius_wet_col(m,klevel)
                  call peg_message( lunerr, msg )
                endif
                if(radius_wet_col(m,klevel).gt.rmax)then
                 radius_wet_col(m,klevel)=rmax
                 !only print when the number is significant
                 if (number_bin_col(m,klevel).ge.1.e-10) then 
                   write( msg, '(a, 5i4,1x, 2e11.4)' )	&
                'mieaer: radius_wet set to rmax,'  //	&
                'id,i,j,k,m,rm(m,k),number', &
                id, iclm, jclm, klevel, m, radius_wet_col(m,klevel),number_bin_col(m,klevel)
                  call peg_message( lunerr, msg )
                 endif
                endif
          !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                x=log(radius_wet_col(m,klevel)) ! radius in cm
                crefin=swrefindx_col(m,klevel,ns)
                refr=real(crefin)
                refi=-imag(crefin)
                xrad=x
                thesize=2.0*pie*exp(x)/wavmidsw(ns)
                ! normalize size parameter
                xrad=(2*xrad-xrmax-xrmin)/(xrmax-xrmin)

          !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          !ec  diagnostics
          !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          ! retain this diagnostic code
                if(abs(refr).gt.10.0.or.abs(refr).le.0.001)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refr/ outside range 1e-3 - 10 ' //  &
           'refr= ', refr
                     call peg_error_fatal( lunerr, msg )
                endif
                if(abs(refi).gt.10.)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refi/ >10 '  //  &
            'refi', refi
                     call peg_error_fatal( lunerr, msg )                  
                endif
          !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! interpolate coefficients linear in refractive index
! first call calcs itab,jtab,ttab,utab
                  itab=0
                  call binterp(extpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cext)

! JCB 2004/02/09  -- new code for scattering cross section
                  call binterp(ascatpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cscat)
                  call binterp(asmpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,casm)
                  call binterp(pmom2psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom2)
                  call binterp(pmom3psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom3)
                  call binterp(pmom4psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom4)
                  call binterp(pmom5psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom5)
                  call binterp(pmom6psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom6)
                  call binterp(pmom7psw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpmom7)
                  call binterp(sbackpsw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtabsw(1,ns),refitabsw(1,ns),itab,jtab,   &
                               ttab,utab,cpsback2p)

!                 chebyshev polynomials
                  ch(1)=1.
                  ch(2)=xrad
                  do nc=3,ncoef
                     ch(nc)=2.*xrad*ch(nc-1)-ch(nc-2)
                  enddo
!                 parameterized optical properties

                  pext=0.5*cext(1)
                  do nc=2,ncoef
                     pext=pext+ch(nc)*cext(nc)
                  enddo
                  pext=exp(pext)
        
! JCB 2004/02/09 -- for scattering efficiency
                  pscat=0.5*cscat(1)
                  do nc=2,ncoef
                     pscat=pscat+ch(nc)*cscat(nc)
                  enddo
                  pscat=exp(pscat)
!
                  pasm=0.5*casm(1)
                  do nc=2,ncoef
                     pasm=pasm+ch(nc)*casm(nc)
                  enddo
                  pasm=exp(pasm)
!
                  ppmom2=0.5*cpmom2(1)
                  do nc=2,ncoef
                     ppmom2=ppmom2+ch(nc)*cpmom2(nc)
                  enddo
                  if(ppmom2.le.0.0)ppmom2=0.0
!
                  ppmom3=0.5*cpmom3(1)
                  do nc=2,ncoef
                     ppmom3=ppmom3+ch(nc)*cpmom3(nc)
                  enddo
                  if(ppmom3.le.0.0)ppmom3=0.0
!
                  ppmom4=0.5*cpmom4(1)
                  do nc=2,ncoef
                     ppmom4=ppmom4+ch(nc)*cpmom4(nc)
                  enddo
                  if(ppmom4.le.0.0.or.sizem.le.0.03e-04)ppmom4=0.0
!
                  ppmom5=0.5*cpmom5(1)
                  do nc=2,ncoef
                     ppmom5=ppmom5+ch(nc)*cpmom5(nc)
                  enddo
                  if(ppmom5.le.0.0.or.sizem.le.0.03e-04)ppmom5=0.0
!
                  ppmom6=0.5*cpmom6(1)
                  do nc=2,ncoef
                     ppmom6=ppmom6+ch(nc)*cpmom6(nc)
                  enddo
                  if(ppmom6.le.0.0.or.sizem.le.0.03e-04)ppmom6=0.0
!
                  ppmom7=0.5*cpmom7(1)
                  do nc=2,ncoef
                     ppmom7=ppmom7+ch(nc)*cpmom7(nc)
                  enddo
                  if(ppmom7.le.0.0.or.sizem.le.0.03e-04)ppmom7=0.0
!
                  sback2=0.5*cpsback2p(1) ! JCB 2007/02/01 - backscattering efficiency
                  do nc=2,ncoef
                     sback2=sback2+ch(nc)*cpsback2p(nc)
                  enddo
                     sback2=exp(sback2)
                  if(sback2.le.0.0)sback2=0.0
!
!
! weights:
        pscat=min(pscat,pext)   !czhao
        weighte=pext*pie*exp(x)**2 ! JCB, extinction cross section
        weights=pscat*pie*exp(x)**2 ! JCB, scattering cross section
        swtauaer(ns,klevel)=swtauaer(ns,klevel)+weighte*number_bin_col(m,klevel)  ! must be multiplied by deltaZ
!      if (iclm==30.and.jclm==49.and.klevel==2.and.m==5) then 
!      write(0,*) 'czhao check swtauaer calculation in MIE',ns,m,weighte,number_bin_col(m,klevel),swtauaer(ns,klevel)*dz(klevel)*100
!      print*, 'czhao check swtauaer calculation in MIE',ns,m,weighte,number_bin_col(m,klevel),swtauaer(ns,klevel)*dz(klevel)*100
!      endif
        swsizeaer(ns,klevel)=swsizeaer(ns,klevel)+exp(x)*10000.0*   &
        number_bin_col(m,klevel)
        swwaer(ns,klevel)=swwaer(ns,klevel)+weights*number_bin_col(m,klevel) !JCB
        swgaer(ns,klevel)=swgaer(ns,klevel)+pasm*weights*number_bin_col(m,klevel) !JCB
! need weighting by scattering cross section ?  JCB 2004/02/09
        l2(ns,klevel)=l2(ns,klevel)+weights*ppmom2*number_bin_col(m,klevel)
        l3(ns,klevel)=l3(ns,klevel)+weights*ppmom3*number_bin_col(m,klevel)
        l4(ns,klevel)=l4(ns,klevel)+weights*ppmom4*number_bin_col(m,klevel)
        l5(ns,klevel)=l5(ns,klevel)+weights*ppmom5*number_bin_col(m,klevel)
        l6(ns,klevel)=l6(ns,klevel)+weights*ppmom6*number_bin_col(m,klevel)
        l7(ns,klevel)=l7(ns,klevel)+weights*ppmom7*number_bin_col(m,klevel)	
! convert backscattering efficiency to backscattering coefficient, units (cm)^-1
        swbscoef(ns,klevel)=swbscoef(ns,klevel)+pie*exp(x)**2*sback2*number_bin_col(m,klevel)! backscatter

        end do ! end of nbin_a loop

! take averages - weighted by cross section - new code JCB 2004/02/09
        swsizeaer(ns,klevel)=swsizeaer(ns,klevel)/thesum
        swgaer(ns,klevel)=swgaer(ns,klevel)/swwaer(ns,klevel) ! JCB removed *3 factor 2/9/2004
! because factor is applied in subroutine opmie, file zz01fastj_mod.f
        l2(ns,klevel)=l2(ns,klevel)/swwaer(ns,klevel)
        l3(ns,klevel)=l3(ns,klevel)/swwaer(ns,klevel)
        l4(ns,klevel)=l4(ns,klevel)/swwaer(ns,klevel)
        l5(ns,klevel)=l5(ns,klevel)/swwaer(ns,klevel)
        l6(ns,klevel)=l6(ns,klevel)/swwaer(ns,klevel)
        l7(ns,klevel)=l7(ns,klevel)/swwaer(ns,klevel)
! backscatter coef, divide by 4*Pie to get units of (km*ster)^-1 JCB 2007/02/01
        swbscoef(ns,klevel)=swbscoef(ns,klevel)*1.0e5  ! units are now (km)^-1
        swextaer(ns,klevel)=swtauaer(ns,klevel)*1.0e5  ! now true extincion, units (km)^-1
! this must be last!! 
        swwaer(ns,klevel)=swwaer(ns,klevel)/swtauaer(ns,klevel) ! JCB

!70   continue ! bail out if no aerosol;go on to next wavelength bin

1000   continue  ! end of wavelength loop

2000   continue  ! end of klevel loop
!
! before returning, multiply tauaer by depth of individual cells.
! tauaer is in cm-1, dz in m; multiply dz by 100 to convert from m to cm.
        do ns = 1, nswbands
        do klevel = 1, kte 
           swtauaer(ns,klevel) = swtauaer(ns,klevel) * dz(klevel)* 100.   
        end do
        end do  

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec  fastj diagnostics
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        if (iclm .eq. CHEM_DBG_I) then
          if (jclm .eq. CHEM_DBG_J) then
!   initial entry
           if (kcallmieaer .eq. 0) then
               write(*,909) CHEM_DBG_I, CHEM_DBG_J
 909    format(	' for cell i = ', i3, ' j = ', i3)          	
               write(*,910)
 910     format(   &
               'curr_secs', 3x, 'i', 3x, 'j', 3x,'k', 3x,   &
                'dzmfastj', 8x,   &
               'tauaer(1,k)',1x, 'tauaer(2,k)',1x,'tauaer(3,k)',3x,   &
               'tauaer(4,k)',5x,   &
               'waer(1,k)', 7x, 'waer(2,k)', 7x,'waer(3,k)', 7x,   &
               'waer(4,k)', 7x,   &
               'gaer(1,k)', 7x, 'gaer(2,k)', 7x,'gaer(3,k)', 7x,   &
               'gaer(4,k)', 7x,   &
               'extaer(1,k)',5x, 'extaer(2,k)',5x,'extaer(3,k)',5x,   &
               'extaer(4,k)',5x,   &
               'sizeaer(1,k)',4x, 'sizeaer(2,k)',4x,'sizeaer(3,k)',4x,   &
               'sizeaer(4,k)'  )
           end if
!ec output for run_out.30
         do k = 1,kte 
         write(*, 912)   &
           curr_secs,iclm, jclm, k,   &
           dz(k) ,   &
           (swtauaer(n,k),   n=1,4),   &
           (swwaer(n,k),     n=1,4),   &
           (swgaer(n,k),     n=1,4),   &
           (swextaer(n,k),   n=1,4),   &
           (swsizeaer(n,k),  n=1,4)
 912    format( i7,3(2x,i4),2x,21(e14.6,2x))
         end do
        kcallmieaer = kcallmieaer + 1
        end if
         end if
!ec end print of fastj diagnostics	
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#endif


!######################################################################
!parameterization of mie calculation for longwave 
!#####################################################################

! begin level loop
        do 2001 klevel=1,kte
! sum densities for normalization
        thesum=0.0
        do m=1,nbin_a
        thesum=thesum+number_bin_col(m,klevel)
        enddo
! Begin longwave spectral loop
      do 1001 ns=1,nlwbands

!        aerosol optical properties
             lwtauaer(ns,klevel)=0.
             lwextaer(ns,klevel)=0.0
             if(thesum.le.1e-21)goto 1001 ! set everything = 0 if no aerosol !wig changed 0.0 to 1e-21, 31-Oct-2005

! loop over the bins
               do m=1,nbin_a ! nbin_a is number of bins
! here's the size
                sizem=radius_wet_col(m,klevel) ! radius in cm
                x=log(radius_wet_col(m,klevel)) ! radius in cm
                crefin=lwrefindx_col(m,klevel,ns)
                refr=real(crefin)
                refi=-imag(crefin)
                xrad=x
                thesize=2.0*pie*exp(x)/wavmidlw(ns)
                ! normalize size parameter
                xrad=(2*xrad-xrmax-xrmin)/(xrmax-xrmin)

          !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          !ec  diagnostics
          !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          ! retain this diagnostic code
                if(abs(refr).gt.10.0.or.abs(refr).le.0.001)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refr/ outside range 1e-3 - 10 ' //  &
           'refr= ', refr
                     call peg_error_fatal( lunerr, msg )
                endif
                if(abs(refi).gt.10.)then
                     write ( msg, '(a,1x, e14.5)' )  &
           'mieaer /refi/ >10 '  //  &
            'refi', refi
                     call peg_error_fatal( lunerr, msg )
                endif
          !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! interpolate coefficients linear in refractive index
! first call calcs itab,jtab,ttab,utab
                  itab=0
                  call binterp(absplw(1,1,1,ns),ncoef,nrefr,nrefi,   &
                               refr,refi,refrtablw(1,ns),refitablw(1,ns),itab,jtab,   &
                               ttab,utab,cabs)

!                 chebyshev polynomials
                  ch(1)=1.
                  ch(2)=xrad
                  do nc=3,ncoef
                     ch(nc)=2.*xrad*ch(nc-1)-ch(nc-2)
                  enddo
!                 parameterized optical properties
                  pabs=0.5*cabs(1)
                  do nc=2,ncoef
                     pabs=pabs+ch(nc)*cabs(nc)
                  enddo
                  pabs=exp(pabs)

!
! weights:
        weighta=pabs*pie*exp(x)**2 ! JCB, extinction cross section
        !weighta: cm2 and number_bin_col #/cm3 -> /cm
        lwtauaer(ns,klevel)=lwtauaer(ns,klevel)+weighta*number_bin_col(m,klevel) ! must be multiplied by deltaZ

        end do ! end of nbin_a loop

! take averages - weighted by cross section - new code JCB 2004/02/09
        lwextaer(ns,klevel)=lwtauaer(ns,klevel)*1.0e5  ! now true extincion, units (km)^-1

1001   continue  ! end of wavelength loop

2001   continue  ! end of klevel loop
!
! before returning, multiply tauaer by depth of individual cells.
! tauaer is in cm-1, dz in m; multiply dz by 100 to convert from m to cm.
        do ns = 1, nlwbands
        do klevel = 1, kte
           lwtauaer(ns,klevel) = lwtauaer(ns,klevel) * dz(klevel)* 100.
        end do
        end do

      return
      end subroutine mieaer
!****************************************************************

!****************************************************************

      subroutine fitcurv(rs,yin,coef,ncoef,maxm)

!     fit y(x) using Chebychev polynomials
!     wig 7-Sep-2004: Removed dependency on pre-determined maximum
!                     array size and replaced with f90 array info.

      USE module_peg_util, only:  peg_message

      IMPLICIT NONE
!      integer nmodes, nrows, maxm, ncoef
!      parameter (nmodes=500,nrows=8)
      integer, intent(in) :: maxm, ncoef

!      real rs(nmodes),yin(nmodes),coef(ncoef)
!      real x(nmodes),y(nmodes)
      real, dimension(ncoef) :: coef
      real, dimension(:) :: rs, yin
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg

!!$      if(maxm.gt.nmodes)then
!!$        write ( msg, '(a, 1x,i6)' )  &
!!$           'FASTJ mie nmodes too small in fitcurv, '  //  &
!!$           'maxm ', maxm
!!$!        write(*,*)'nmodes too small in fitcurv',maxm
!!$        call peg_error_fatal( lunerr, msg )
!!$      endif

      do 100 m=1,maxm
! To prevent the log of 0 or negative values, as the code was blowing up when compile with intel
! Added by Manish Shrivastava
! Need to be checked
      x(m)=log(max(rs(m),1d-20))
      y(m)=log(max(yin(m),1d-20))
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv                        
!**************************************************************
      subroutine fitcurv_nolog(rs,yin,coef,ncoef,maxm)

!     fit y(x) using Chebychev polynomials
!     wig 7-Sep-2004: Removed dependency on pre-determined maximum
!                     array size and replaced with f90 array info.

      USE module_peg_util, only:  peg_message
      IMPLICIT NONE

!      integer nmodes, nrows, maxm, ncoef
!      parameter (nmodes=500,nrows=8)
      integer, intent(in) :: maxm, ncoef

!      real rs(nmodes),yin(nmodes),coef(ncoef)
      real, dimension(:) :: rs, yin
      real, dimension(ncoef) :: coef(ncoef)
      real x(size(rs)),y(size(yin))

      integer m
      real xmin, xmax
      character*80 msg
           
!!$      if(maxm.gt.nmodes)then
!!$        write ( msg, '(a,1x, i6)' )  &
!!$           'FASTJ mie nmodes too small in fitcurv '  //  &
!!$           'maxm ', maxm
!!$!        write(*,*)'nmodes too small in fitcurv',maxm
!!$        call peg_error_fatal( lunerr, msg )
!!$      endif

      do 100 m=1,maxm
      x(m)=log(rs(m))
      y(m)=yin(m) ! note, no "log" here
  100 continue

      xmin=x(1)
      xmax=x(maxm)
      do 110 m=1,maxm
      x(m)=(2*x(m)-xmax-xmin)/(xmax-xmin)
  110 continue

      call chebft(coef,ncoef,maxm,y)

      return
      end subroutine fitcurv_nolog                        
!************************************************************************
      subroutine chebft(c,ncoef,n,f)
!     given a function f with values at zeroes x_k of Chebychef polynomial
!     T_n(x), calculate coefficients c_j such that
!     f(x)=sum(k=1,n) c_k t_(k-1)(y) - 0.5*c_1
!     where y=(x-0.5*(xmax+xmin))/(0.5*(xmax-xmin))
!     See Numerical Recipes, pp. 148-150.

      IMPLICIT NONE
      real pi
      integer ncoef, n
      parameter (pi=3.14159265)
      real c(ncoef),f(n)

! local variables      
      real fac, thesum
      integer j, k
      
      fac=2./n
      do j=1,ncoef
         thesum=0
         do k=1,n
            thesum=thesum+f(k)*cos((pi*(j-1))*((k-0.5)/n))
         enddo
         c(j)=fac*thesum
      enddo
      return
      end subroutine chebft             
!*************************************************************************
      subroutine binterp(table,km,im,jm,x,y,xtab,ytab,ix,jy,t,u,out)

!     bilinear interpolation of table
!
      implicit none
      integer im,jm,km
      real table(km,im,jm),xtab(im),ytab(jm),out(km)
      integer i,ix,ip1,j,jy,jp1,k
      real x,dx,t,y,dy,u,tu,  tuc,tcu,tcuc

      if(ix.gt.0)go to 30
      if(im.gt.1)then
        do i=1,im
          if(x.lt.xtab(i))go to 10
        enddo
   10   ix=max0(i-1,1)
        ip1=min0(ix+1,im)
        dx=(xtab(ip1)-xtab(ix))
        if(abs(dx).gt.1.e-20)then
           t=(x-xtab(ix))/(xtab(ix+1)-xtab(ix))
        else
           t=0
        endif
      else
        ix=1
        ip1=1
        t=0
      endif
      if(jm.gt.1)then
        do j=1,jm
          if(y.lt.ytab(j))go to 20
        enddo
   20   jy=max0(j-1,1)
        jp1=min0(jy+1,jm)
        dy=(ytab(jp1)-ytab(jy))
        if(abs(dy).gt.1.e-20)then
           u=(y-ytab(jy))/dy
        else
           u=0
        endif
      else
        jy=1
        jp1=1
        u=0
      endif
   30 continue
      jp1=min(jy+1,jm)
      ip1=min(ix+1,im)
      tu=t*u
      tuc=t-tu
      tcuc=1-tuc-u
      tcu=u-tu
      do k=1,km
         out(k)=tcuc*table(k,ix,jy)+tuc*table(k,ip1,jy)   &
               +tu*table(k,ip1,jp1)+tcu*table(k,ix,jp1)
      enddo
      return
      end subroutine binterp                                            
!***************************************************************
      subroutine  miev0 ( xx, crefin, perfct, mimcut, anyang,   &
                          numang, xmu, nmom, ipolzn, momdim, prnt,   &
                          qext, qsca, gqsc, pmom, sforw, sback, s1,   &
                          s2, tforw, tback )
!
!    computes mie scattering and extinction efficiencies; asymmetry
!    factor;  forward- and backscatter amplitude;  scattering
!    amplitudes for incident polarization parallel and perpendicular
!    to the plane of scattering, as functions of scattering angle;
!    coefficients in the legendre polynomial expansions of either the
!    unpolarized phase function or the polarized phase matrix;
!    and some quantities needed in polarized radiative transfer.
!
!      calls :  biga, ckinmi, small1, small2, testmi, miprnt,
!               lpcoef, errmsg
!
!      i n t e r n a l   v a r i a b l e s
!      -----------------------------------
!
!  an,bn           mie coefficients  little-a-sub-n, little-b-sub-n
!                     ( ref. 1, eq. 16 )
!  anm1,bnm1       mie coefficients  little-a-sub-(n-1),
!                     little-b-sub-(n-1);  used in -gqsc- sum
!  anp             coeffs. in s+ expansion ( ref. 2, p. 1507 )
!  bnp             coeffs. in s- expansion ( ref. 2, p. 1507 )
!  anpm            coeffs. in s+ expansion ( ref. 2, p. 1507 )
!                     when  mu  is replaced by  - mu
!  bnpm            coeffs. in s- expansion ( ref. 2, p. 1507 )
!                     when  mu  is replaced by  - mu
!  calcmo(k)       true, calculate moments for k-th phase quantity
!                     (derived from -ipolzn-; used only in 'lpcoef')
!  cbiga(n)        bessel function ratio capital-a-sub-n (ref. 2, eq. 2)
!                     ( complex version )
!  cior            complex index of refraction with negative
!                     imaginary part (van de hulst convention)
!  cioriv          1 / cior
!  coeff           ( 2n + 1 ) / ( n ( n + 1 ) )
!  fn              floating point version of index in loop performing
!                     mie series summation
!  lita,litb(n)    mie coefficients -an-, -bn-, saved in arrays for
!                     use in calculating legendre moments *pmom*
!  maxtrm          max. possible no. of terms in mie series
!  mm              + 1 and  - 1,  alternately.
!  mim             magnitude of imaginary refractive index
!  mre             real part of refractive index
!  maxang          max. possible value of input variable -numang-
!  nangd2          (numang+1)/2 ( no. of angles in 0-90 deg; anyang=f )
!  noabs           true, sphere non-absorbing (determined by -mimcut-)
!  np1dn           ( n + 1 ) / n
!  npquan          highest-numbered phase quantity for which moments are
!                     to be calculated (the largest digit in -ipolzn-
!                     if  ipolzn .ne. 0)
!  ntrm            no. of terms in mie series
!  pass1           true on first entry, false thereafter; for self-test
!  pin(j)          angular function little-pi-sub-n ( ref. 2, eq. 3 )
!                     at j-th angle
!  pinm1(j)        little-pi-sub-(n-1) ( see -pin- ) at j-th angle
!  psinm1          ricatti-bessel function psi-sub-(n-1), argument -xx-
!  psin            ricatti-bessel function psi-sub-n of argument -xx-
!                     ( ref. 1, p. 11 ff. )
!  rbiga(n)        bessel function ratio capital-a-sub-n (ref. 2, eq. 2)
!                     ( real version, for when imag refrac index = 0 )
!  rioriv          1 / mre
!  rn              1 / n
!  rtmp            (real) temporary variable
!  sp(j)           s+  for j-th angle  ( ref. 2, p. 1507 )
!  sm(j)           s-  for j-th angle  ( ref. 2, p. 1507 )
!  sps(j)          s+  for (numang+1-j)-th angle ( anyang=false )
!  sms(j)          s-  for (numang+1-j)-th angle ( anyang=false )
!  taun            angular function little-tau-sub-n ( ref. 2, eq. 4 )
!                     at j-th angle
!  tcoef           n ( n+1 ) ( 2n+1 ) (for summing tforw,tback series)
!  twonp1          2n + 1
!  yesang          true if scattering amplitudes are to be calculated
!  zetnm1          ricatti-bessel function  zeta-sub-(n-1) of argument
!                     -xx-  ( ref. 2, eq. 17 )
!  zetn            ricatti-bessel function  zeta-sub-n of argument -xx-
!
! ----------------------------------------------------------------------
! --------  i / o specifications for subroutine miev0  -----------------
! ----------------------------------------------------------------------
      implicit none
      logical  anyang, perfct, prnt(*)
      integer  ipolzn, momdim, numang, nmom
      real*8     gqsc, mimcut, pmom( 0:momdim, * ), qext, qsca,   &
               xmu(*), xx
      complex*16  crefin, sforw, sback, s1(*), s2(*), tforw(*),   &
               tback(*)
      integer maxang,mxang2,maxtrm
      real*8 onethr
! ----------------------------------------------------------------------
!
      parameter ( maxang = 501, mxang2 = maxang/2 + 1 )
!
!                                  ** note --  maxtrm = 10100  is neces-
!                                  ** sary to do some of the test probs,
!                                  ** but 1100 is sufficient for most
!                                  ** conceivable applications
      parameter ( maxtrm = 1100 )
      parameter ( onethr = 1./3. )
!
      logical   anysav, calcmo(4), noabs, ok, persav, yesang
      integer   npquan
      integer i,j,n,nmosav,iposav,numsav,ntrm,nangd2
      real*8      mim, mimsav, mre, mm, np1dn
      real*8 rioriv,xmusav,xxsav,sq,fn,rn,twonp1,tcoef, coeff
      real*8 xinv,psinm1,chinm1,psin,chin,rtmp,taun
      real*8      rbiga( maxtrm ), pin( maxang ), pinm1( maxang )
      complex*16   an, bn, anm1, bnm1, anp, bnp, anpm, bnpm, cresav,   &
                cior, cioriv, ctmp, zet, zetnm1, zetn
      complex*16   cbiga( maxtrm ), lita( maxtrm ), litb( maxtrm ),   &
                sp( maxang ), sm( maxang ), sps( mxang2 ), sms( mxang2 )
      equivalence  ( cbiga, rbiga )
      logical, save :: pass1
      data  pass1 / .true. /
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      if ( pass1 )  then
!                                   ** save certain user input values
         xxsav  = xx
         cresav = crefin
         mimsav = mimcut
         persav = perfct
         anysav = anyang
         nmosav = nmom
         iposav = ipolzn
         numsav = numang
         xmusav = xmu( 1 )
!                              ** reset input values for test case
         xx      = 10.0
         crefin  = ( 1.5, - 0.1 )
         perfct  = .false.
         mimcut  = 0.0
         anyang  = .true.
         numang  = 1
         xmu( 1 )= - 0.7660444
         nmom    = 1
         ipolzn  = - 1
!
      end if
!                                        ** check input and calculate
!                                        ** certain variables from input
!
   10 call  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   &
                    nmom, ipolzn, anyang, xmu, calcmo, npquan )
!
      if ( perfct .and. xx .le. 0.1 )  then
!                                            ** use totally-reflecting
!                                            ** small-particle limit
!
         call  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   &
                        sback, s1, s2, tforw, tback, lita, litb )
         ntrm = 2
         go to 200
      end if
!
      if ( .not.perfct )  then
!
         cior = crefin
         if ( dimag( cior ) .gt. 0.0 )  cior = dconjg( cior )
         mre =     dble( cior )
         mim =  - dimag( cior )
         noabs = mim .le. mimcut
         cioriv = 1.0 / cior
         rioriv = 1.0 / mre
!
         if ( xx * dmax1( 1.d0, cdabs(cior) ) .le. 0.d1 ) then
!
!                                    ** use general-refractive-index
!                                    ** small-particle limit
!                                    ** ( ref. 2, p. 1508 )
!
            call  small2 ( xx, cior, .not.noabs, numang, xmu, qext,   &
                           qsca, gqsc, sforw, sback, s1, s2, tforw,   &
                           tback, lita, litb )
            ntrm = 2
            go to 200
         end if
!
      end if
!
      nangd2 = ( numang + 1 ) / 2
      yesang = numang .gt. 0
!                              ** estimate number of terms in mie series
!                              ** ( ref. 2, p. 1508 )
      if ( xx.le.8.0 )  then
         ntrm = xx + 4. * xx**onethr + 1.
      else if ( xx.lt.4200. )  then
         ntrm = xx + 4.05 * xx**onethr + 2.
      else
         ntrm = xx + 4. * xx**onethr + 2.
      end if
      if ( ntrm+1 .gt. maxtrm )   &
           call errmsg( 'miev0--parameter maxtrm too small', .true. )
!
!                            ** calculate logarithmic derivatives of
!                            ** j-bessel-fcn., big-a-sub-(1 to ntrm)
      if ( .not.perfct )   &
           call  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )
!
!                            ** initialize ricatti-bessel functions
!                            ** (psi,chi,zeta)-sub-(0,1) for upward
!                            ** recurrence ( ref. 1, eq. 19 )
      xinv = 1.0 / xx
      psinm1   = dsin( xx )
      chinm1   = dcos( xx )
      psin = psinm1 * xinv - chinm1
      chin = chinm1 * xinv + psinm1
      zetnm1 = dcmplx( psinm1, chinm1 )
      zetn   = dcmplx( psin, chin )
!                                     ** initialize previous coeffi-
!                                     ** cients for -gqsc- series
      anm1 = ( 0.0, 0.0 )
      bnm1 = ( 0.0, 0.0 )
!                             ** initialize angular function little-pi
!                             ** and sums for s+, s- ( ref. 2, p. 1507 )
      if ( anyang )  then
         do  60  j = 1, numang
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
   60    continue
      else
         do  70  j = 1, nangd2
            pinm1( j ) = 0.0
            pin( j )   = 1.0
            sp ( j ) = ( 0.0, 0.0 )
            sm ( j ) = ( 0.0, 0.0 )
            sps( j ) = ( 0.0, 0.0 )
            sms( j ) = ( 0.0, 0.0 )
   70    continue
      end if
!                         ** initialize mie sums for efficiencies, etc.
      qsca = 0.0
      gqsc = 0.0
      sforw      = ( 0., 0. )
      sback      = ( 0., 0. )
      tforw( 1 ) = ( 0., 0. )
      tback( 1 ) = ( 0., 0. )
!
!
! ---------  loop to sum mie series  -----------------------------------
!
      mm = + 1.0
      do  100  n = 1, ntrm
!                           ** compute various numerical coefficients
         fn     = n
         rn     = 1.0 / fn
         np1dn  = 1.0 + rn
         twonp1 = 2 * n + 1
         coeff  = twonp1 / ( fn * ( n + 1 ) )
         tcoef  = twonp1 * ( fn * ( n + 1 ) )
!
!                              ** calculate mie series coefficients
         if ( perfct )  then
!                                   ** totally-reflecting case
!
            an = ( ( fn*xinv ) * psin - psinm1 ) /   &
                 ( ( fn*xinv ) * zetn - zetnm1 )
            bn = psin / zetn
!
         else if ( noabs )  then
!                                      ** no-absorption case
!
            an =  ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                / ( ( rioriv*rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn =  ( (  mre * rbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                / ( (  mre * rbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
         else
!                                       ** absorptive case
!
            an = ( ( cioriv * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                /( ( cioriv * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            bn = ( (   cior * cbiga(n) + ( fn*xinv ) ) * psin - psinm1 )   &
                /( (   cior * cbiga(n) + ( fn*xinv ) ) * zetn - zetnm1 )
            qsca = qsca + twonp1 * ( sq( an ) + sq( bn ) )
!
         end if
!                       ** save mie coefficients for *pmom* calculation
         lita( n ) = an
         litb( n ) = bn
!                            ** increment mie sums for non-angle-
!                            ** dependent quantities
!
         sforw      = sforw      + twonp1 * ( an + bn )
         tforw( 1 ) = tforw( 1 ) + tcoef  * ( an - bn )
         sback      = sback      + ( mm * twonp1 ) * ( an - bn )
         tback( 1 ) = tback( 1 ) + ( mm * tcoef )  * ( an + bn )
         gqsc = gqsc + ( fn - rn ) * dble( anm1 * dconjg( an )   &
                                         + bnm1 * dconjg( bn ) )   &
                + coeff * dble( an * dconjg( bn ) )
!
         if ( yesang )  then
!                                      ** put mie coefficients in form
!                                      ** needed for computing s+, s-
!                                      ** ( ref. 2, p. 1507 )
            anp = coeff * ( an + bn )
            bnp = coeff * ( an - bn )
!                                      ** increment mie sums for s+, s-
!                                      ** while upward recursing
!                                      ** angular functions little pi
!                                      ** and little tau
            if ( anyang )  then
!                                         ** arbitrary angles
!
!                                              ** vectorizable loop
               do  80  j = 1, numang
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp( j )  = sp( j ) + anp * ( pin( j ) + taun )
                  sm( j )  = sm( j ) + bnp * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   80          continue
!
            else
!                                  ** angles symmetric about 90 degrees
               anpm = mm * anp
               bnpm = mm * bnp
!                                          ** vectorizable loop
               do  90  j = 1, nangd2
                  rtmp = ( xmu( j ) * pin( j ) ) - pinm1( j )
                  taun =  fn * rtmp - pinm1( j )
                  sp ( j ) = sp ( j ) +  anp * ( pin( j ) + taun )
                  sms( j ) = sms( j ) + bnpm * ( pin( j ) + taun )
                  sm ( j ) = sm ( j ) +  bnp * ( pin( j ) - taun )
                  sps( j ) = sps( j ) + anpm * ( pin( j ) - taun )
                  pinm1( j ) = pin( j )
                  pin( j ) = ( xmu( j ) * pin( j ) ) + np1dn * rtmp
   90          continue
!
            end if
         end if
!                          ** update relevant quantities for next
!                          ** pass through loop
         mm   =  - mm
         anm1 = an
         bnm1 = bn
!                           ** upward recurrence for ricatti-bessel
!                           ** functions ( ref. 1, eq. 17 )
!
         zet    = ( twonp1 * xinv ) * zetn - zetnm1
         zetnm1 = zetn
         zetn   = zet
         psinm1 = psin
         psin   = dble( zetn )
  100 continue
!
! ---------- end loop to sum mie series --------------------------------
!
!
      qext = 2. / xx**2 * dble( sforw )
      if ( perfct .or. noabs )  then
         qsca = qext
      else
         qsca = 2. / xx**2 * qsca
      end if
!
      gqsc = 4. / xx**2 * gqsc
      sforw = 0.5 * sforw
      sback = 0.5 * sback
      tforw( 2 ) = 0.5 * (   sforw + 0.25 * tforw( 1 ) )
      tforw( 1 ) = 0.5 * (   sforw - 0.25 * tforw( 1 ) )
      tback( 2 ) = 0.5 * (   sback + 0.25 * tback( 1 ) )
      tback( 1 ) = 0.5 * ( - sback + 0.25 * tback( 1 ) )
!
      if ( yesang )  then
!                                ** recover scattering amplitudes
!                                ** from s+, s- ( ref. 1, eq. 11 )
         if ( anyang )  then
!                                         ** vectorizable loop
            do  110  j = 1, numang
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  110       continue
!
         else
!                                         ** vectorizable loop
            do  120  j = 1, nangd2
               s1( j ) = 0.5 * ( sp( j ) + sm( j ) )
               s2( j ) = 0.5 * ( sp( j ) - sm( j ) )
  120       continue
!                                         ** vectorizable loop
            do  130  j = 1, nangd2
               s1( numang+1 - j ) = 0.5 * ( sps( j ) + sms( j ) )
               s2( numang+1 - j ) = 0.5 * ( sps( j ) - sms( j ) )
  130       continue
         end if
!
      end if
!                                         ** calculate legendre moments
  200 if ( nmom.gt.0 )   &
           call lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,   &
                         lita, litb, pmom )
!
      if ( dimag(crefin) .gt. 0.0 )  then
!                                         ** take complex conjugates
!                                         ** of scattering amplitudes
         sforw = dconjg( sforw )
         sback = dconjg( sback )
         do  210  i = 1, 2
            tforw( i ) = dconjg( tforw(i) )
            tback( i ) = dconjg( tback(i) )
  210    continue
!
         do  220  j = 1, numang
            s1( j ) = dconjg( s1(j) )
            s2( j ) = dconjg( s2(j) )
  220    continue
!
      end if
!
      if ( pass1 )  then
!                             ** compare test case results with
!                             ** correct answers and abort if bad
!
         call  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,   &
                        tforw, tback, pmom, momdim, ok )
         if ( .not. ok )  then
            prnt(1) = .false.
            prnt(2) = .false.
            call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   &
                          qsca, gqsc, nmom, ipolzn, momdim, calcmo,   &
                          pmom, sforw, sback, tforw, tback, s1, s2 )
            call errmsg( 'miev0 -- self-test failed', .true. )
         end if
!                                       ** restore user input values
         xx     = xxsav
         crefin = cresav
         mimcut = mimsav
         perfct = persav
         anyang = anysav
         nmom   = nmosav
         ipolzn = iposav
         numang = numsav
         xmu(1) = xmusav
         pass1 = .false.
         go to 10
!
      end if
!
      if ( prnt(1) .or. prnt(2) )   &
         call  miprnt( prnt, xx, perfct, crefin, numang, xmu, qext,   &
                       qsca, gqsc, nmom, ipolzn, momdim, calcmo,   &
                       pmom, sforw, sback, tforw, tback, s1, s2 )
!
      return
!
      end subroutine  miev0 
!****************************************************************************                                           
      subroutine  ckinmi( numang, maxang, xx, perfct, crefin, momdim,   &
                          nmom, ipolzn, anyang, xmu, calcmo, npquan )
!
!        check for bad input to 'miev0' and calculate -calcmo,npquan-
!
      implicit none
      logical  perfct, anyang, calcmo(*)
      integer  numang, maxang, momdim, nmom, ipolzn, npquan
      real*8    xx, xmu(*)
      integer i,l,j,ip
      complex*16  crefin
!
      character*4  string
      logical  inperr
!
      inperr = .false.
!
      if ( numang.gt.maxang )  then
         call errmsg( 'miev0--parameter maxang too small', .true. )
         inperr = .true.
      end if
      if ( numang.lt.0 )  call  wrtbad( 'numang', inperr )
      if ( xx.lt.0. )     call  wrtbad( 'xx', inperr )
      if ( .not.perfct .and. dble(crefin).le.0. )   &
           call wrtbad( 'crefin', inperr )
      if ( momdim.lt.1 )  call wrtbad( 'momdim', inperr )
!
      if ( nmom.ne.0 )  then
         if ( nmom.lt.0 .or. nmom.gt.momdim ) call wrtbad('nmom',inperr)
         if ( iabs(ipolzn).gt.4444 )  call  wrtbad( 'ipolzn', inperr )
         npquan = 0
         do 5  l = 1, 4
            calcmo( l ) = .false.
    5    continue
         if ( ipolzn.ne.0 )  then
!                                 ** parse out -ipolzn- into its digits
!                                 ** to find which phase quantities are
!                                 ** to have their moments calculated
!
            write( string, '(i4)' )  iabs(ipolzn)
            do 10  j = 1, 4
               ip = ichar( string(j:j) ) - ichar( '0' )
               if ( ip.ge.1 .and. ip.le.4 )  calcmo( ip ) = .true.
               if ( ip.eq.0 .or. (ip.ge.5 .and. ip.le.9) )   &
                    call  wrtbad( 'ipolzn', inperr )
               npquan = max0( npquan, ip )
   10       continue
         end if
      end if
!
      if ( anyang )  then
!                                ** allow for slight imperfections in
!                                ** computation of cosine
          do  20  i = 1, numang
             if ( xmu(i).lt.-1.00001 .or. xmu(i).gt.1.00001 )   &
                  call wrtbad( 'xmu', inperr )
   20     continue
      else
          do  22  i = 1, ( numang + 1 ) / 2
             if ( xmu(i).lt.-0.00001 .or. xmu(i).gt.1.00001 )   &
                  call wrtbad( 'xmu', inperr )
   22     continue
      end if
!
      if ( inperr )   &
           call errmsg( 'miev0--input error(s).  aborting...', .true. )
!
      if ( xx.gt.20000.0 .or. dble(crefin).gt.10.0 .or.   &
           dabs( dimag(crefin) ).gt.10.0 )  call  errmsg(   &
           'miev0--xx or crefin outside tested range', .false. )
!
      return
      end subroutine  ckinmi
!***********************************************************************                                                   
      subroutine  lpcoef ( ntrm, nmom, ipolzn, momdim, calcmo, npquan,   &
                           a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities ( ref. 5 formulation )
!
!     input:  ntrm                    number terms in mie series
!             nmom, ipolzn, momdim    'miev0' arguments
!             calcmo                  flags calculated from -ipolzn-
!             npquan                  defined in 'miev0'
!             a, b                    mie series coefficients
!
!     output: pmom                   legendre moments ('miev0' argument)
!
!     *** notes ***
!
!         (1)  eqs. 2-5 are in error in dave, appl. opt. 9,
!         1888 (1970).  eq. 2 refers to m1, not m2;  eq. 3 refers to
!         m2, not m1.  in eqs. 4 and 5, the subscripts on the second
!         term in square brackets should be interchanged.
!
!         (2)  the general-case logic in this subroutine works correctly
!         in the two-term mie series case, but subroutine  'lpco2t'
!         is called instead, for speed.
!
!         (3)  subroutine  'lpco1t', to do the one-term case, is never
!         called within the context of 'miev0', but is included for
!         complete generality.
!
!         (4)  some improvement in speed is obtainable by combining the
!         310- and 410-loops, if moments for both the third and fourth
!         phase quantities are desired, because the third phase quantity
!         is the real part of a complex series, while the fourth phase
!         quantity is the imaginary part of that very same series.  but
!         most users are not interested in the fourth phase quantity,
!         which is related to circular polarization, so the present
!         scheme is usually more efficient.
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom, ntrm, npquan
      real*8    pmom( 0:momdim, * )
      complex*16  a(*), b(*)
!
!           ** specification of local variables
!
!      am(m)       numerical coefficients  a-sub-m-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bi(i)       numerical coefficients  b-sub-i-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bidel(i)    1/2 bi(i) times factor capital-del in dave
!
!      cm,dm()     arrays c and d in dave, eqs. 16-17 (mueller form),
!                     calculated using recurrence derived in ref. 5
!
!      cs,ds()     arrays c and d in ref. 4, eqs. a5-a6 (sekera form),
!                     calculated using recurrence derived in ref. 5
!
!      c,d()       either -cm,dm- or -cs,ds-, depending on -ipolzn-
!
!      evenl       true for even-numbered moments;  false otherwise
!
!      idel        1 + little-del  in dave
!
!      maxtrm      max. no. of terms in mie series
!
!      maxmom      max. no. of non-zero moments
!
!      nummom      number of non-zero moments
!
!      recip(k)    1 / k
!
      integer maxtrm,maxmom,mxmom2,maxrcp
      parameter  ( maxtrm = 1102, maxmom = 2*maxtrm, mxmom2 = maxmom/2,   &
                   maxrcp = 4*maxtrm + 2 )
      real*8      am( 0:maxtrm ), bi( 0:mxmom2 ), bidel( 0:mxmom2 )
      real*8, save :: recip( maxrcp )
      complex*16 cm( maxtrm ), dm( maxtrm ), cs( maxtrm ), ds( maxtrm ),   &
                 c( maxtrm ), d( maxtrm )
      integer k,j,l,nummom,ld2,idel,m,i,mmax,imax
      real*8 thesum
      equivalence  ( c, cm ),  ( d, dm )
      logical evenl
      logical, save :: pass1
      data  pass1 / .true. /
!
!
      if ( pass1 )  then
!
         do  1  k = 1, maxrcp
            recip( k ) = 1.0 / k
    1    continue
         pass1 = .false.
!
      end if
!
      do  5  j = 1, max0( 1, npquan )
         do  5  l = 0, nmom
            pmom( l, j ) = 0.0
    5 continue
!
      if ( ntrm.eq.1 )  then
         call  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      else if ( ntrm.eq.2 )  then
         call  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
         return
      end if
!
      if ( ntrm+2 .gt. maxtrm )   &
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )
!
!                                     ** calculate mueller c, d arrays
      cm( ntrm+2 ) = ( 0., 0. )
      dm( ntrm+2 ) = ( 0., 0. )
      cm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * b( ntrm )
      dm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * a( ntrm )
      cm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * a( ntrm )   &
                   + ( 1. - recip(ntrm) ) * b( ntrm-1 )
      dm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * b( ntrm )   &
                   + ( 1. - recip(ntrm) ) * a( ntrm-1 )
!
      do  10  k = ntrm-1, 2, -1
         cm( k ) = cm( k+2 ) - ( 1. + recip(k+1) ) * b( k+1 )   &
                             + ( recip(k) + recip(k+1) ) * a( k )   &
                             + ( 1. - recip(k) ) * b( k-1 )
         dm( k ) = dm( k+2 ) - ( 1. + recip(k+1) ) * a( k+1 )   &
                             + ( recip(k) + recip(k+1) ) * b( k )   &
                             + ( 1. - recip(k) ) * a( k-1 )
   10 continue
      cm( 1 ) = cm( 3 ) + 1.5 * ( a( 1 ) - b( 2 ) )
      dm( 1 ) = dm( 3 ) + 1.5 * ( b( 1 ) - a( 2 ) )
!
      if ( ipolzn.ge.0 )  then
!
         do  20  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cm( k )
            d( k ) = ( 2*k - 1 ) * dm( k )
   20    continue
!
      else
!                                    ** compute sekera c and d arrays
         cs( ntrm+2 ) = ( 0., 0. )
         ds( ntrm+2 ) = ( 0., 0. )
         cs( ntrm+1 ) = ( 0., 0. )
         ds( ntrm+1 ) = ( 0., 0. )
!
         do  30  k = ntrm, 1, -1
            cs( k ) = cs( k+2 ) + ( 2*k + 1 ) * ( cm( k+1 ) - b( k ) )
            ds( k ) = ds( k+2 ) + ( 2*k + 1 ) * ( dm( k+1 ) - a( k ) )
   30    continue
!
         do  40  k = 1, ntrm + 2
            c( k ) = ( 2*k - 1 ) * cs( k )
            d( k ) = ( 2*k - 1 ) * ds( k )
   40    continue
!
      end if
!
!
      if( ipolzn.lt.0 )  nummom = min0( nmom, 2*ntrm - 2 )
      if( ipolzn.ge.0 )  nummom = min0( nmom, 2*ntrm )
      if ( nummom .gt. maxmom )   &
           call errmsg( 'lpcoef--parameter maxtrm too small', .true. )
!
!                               ** loop over moments
      do  500  l = 0, nummom
         ld2 = l / 2
         evenl = mod( l,2 ) .eq. 0
!                                    ** calculate numerical coefficients
!                                    ** a-sub-m and b-sub-i in dave
!                                    ** double-sums for moments
         if( l.eq.0 )  then
!
            idel = 1
            do  60  m = 0, ntrm
               am( m ) = 2.0 * recip( 2*m + 1 )
   60       continue
            bi( 0 ) = 1.0
!
         else if( evenl )  then
!
            idel = 1
            do  70  m = ld2, ntrm
               am( m ) = ( 1. + recip( 2*m-l+1 ) ) * am( m )
   70       continue
            do  75  i = 0, ld2-1
               bi( i ) = ( 1. - recip( l-2*i ) ) * bi( i )
   75       continue
            bi( ld2 ) = ( 2. - recip( l ) ) * bi( ld2-1 )
!
         else
!
            idel = 2
            do  80  m = ld2, ntrm
               am( m ) = ( 1. - recip( 2*m+l+2 ) ) * am( m )
   80       continue
            do  85  i = 0, ld2
               bi( i ) = ( 1. - recip( l+2*i+1 ) ) * bi( i )
   85       continue
!
         end if
!                                     ** establish upper limits for sums
!                                     ** and incorporate factor capital-
!                                     ** del into b-sub-i
         mmax = ntrm - idel
         if( ipolzn.ge.0 )  mmax = mmax + 1
         imax = min0( ld2, mmax - ld2 )
         if( imax.lt.0 )  go to 600
         do  90  i = 0, imax
            bidel( i ) = bi( i )
   90    continue
         if( evenl )  bidel( 0 ) = 0.5 * bidel( 0 )
!
!                                    ** perform double sums just for
!                                    ** phase quantities desired by user
         if( ipolzn.eq.0 )  then
!
            do  110  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  100  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dble( c(m-i+1) * dconjg( c(m+i+idel) ) )   &
                            + dble( d(m-i+1) * dconjg( d(m+i+idel) ) ) )
  100          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  110       continue
            pmom( l,1 ) = 0.5 * pmom( l,1 )
            go to 500
!
         end if
!
         if ( calcmo(1) )  then
            do  160  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  150  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                              dble( c(m-i+1) * dconjg( c(m+i+idel) ) )
  150          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * thesum
  160       continue
         end if
!
!
         if ( calcmo(2) )  then
            do  210  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  200  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                              dble( d(m-i+1) * dconjg( d(m+i+idel) ) )
  200          continue
               pmom( l,2 ) = pmom( l,2 ) + bidel( i ) * thesum
  210       continue
         end if
!
!
         if ( calcmo(3) )  then
            do  310  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  300  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dble( c(m-i+1) * dconjg( d(m+i+idel) ) )   &
                            + dble( c(m+i+idel) * dconjg( d(m-i+1) ) ) )
  300          continue
               pmom( l,3 ) = pmom( l,3 ) + bidel( i ) * thesum
  310       continue
            pmom( l,3 ) = 0.5 * pmom( l,3 )
         end if
!
!
         if ( calcmo(4) )  then
            do  410  i = 0, imax
!                                           ** vectorizable loop (cray)
               thesum = 0.0
               do  400  m = ld2, mmax - i
                  thesum = thesum + am( m ) *   &
                            ( dimag( c(m-i+1) * dconjg( d(m+i+idel) ) )   &
                            + dimag( c(m+i+idel) * dconjg( d(m-i+1) ) ))
  400          continue
               pmom( l,4 ) = pmom( l,4 ) + bidel( i ) * thesum
  410       continue
            pmom( l,4 ) = - 0.5 * pmom( l,4 )
         end if
!
  500 continue
!
!
  600 return
      end subroutine  lpcoef
!*********************************************************************                                                    
      subroutine  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities in special case where
!         no. terms in mie series = 1
!
!        input:  nmom, ipolzn, momdim     'miev0' arguments
!                calcmo                   flags calculated from -ipolzn-
!                a(1), b(1)               mie series coefficients
!
!        output: pmom                     legendre moments
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,nummom,l
      real*8    pmom( 0:momdim, * ),sq,a1sq,b1sq
      complex*16  a(*), b(*), ctmp, a1b1c
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      a1sq = sq( a(1) )
      b1sq = sq( b(1) )
      a1b1c = a(1) * dconjg( b(1) )
!
      if( ipolzn.lt.0 )  then
!
         if( calcmo(1) )  pmom( 0,1 ) = 2.25 * b1sq
         if( calcmo(2) )  pmom( 0,2 ) = 2.25 * a1sq
         if( calcmo(3) )  pmom( 0,3 ) = 2.25 * dble( a1b1c )
         if( calcmo(4) )  pmom( 0,4 ) = 2.25 *dimag( a1b1c )
!
      else
!
         nummom = min0( nmom, 2 )
!                                   ** loop over moments
         do  100  l = 0, nummom
!
            if( ipolzn.eq.0 )  then
               if( l.eq.0 )  pmom( l,1 ) = 1.5 * ( a1sq + b1sq )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.15 * ( a1sq + b1sq )
               go to 100
            end if
!
            if( calcmo(1) )  then
               if( l.eq.0 )  pmom( l,1 ) = 2.25 * ( a1sq + b1sq / 3. )
               if( l.eq.1 )  pmom( l,1 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,1 ) = 0.3 * b1sq
            end if
!
            if( calcmo(2) )  then
               if( l.eq.0 )  pmom( l,2 ) = 2.25 * ( b1sq + a1sq / 3. )
               if( l.eq.1 )  pmom( l,2 ) = 1.5 * dble( a1b1c )
               if( l.eq.2 )  pmom( l,2 ) = 0.3 * a1sq
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 )  pmom( l,3 ) = 3.0 * dble( a1b1c )
               if( l.eq.1 )  pmom( l,3 ) = 0.75 * ( a1sq + b1sq )
               if( l.eq.2 )  pmom( l,3 ) = 0.3 * dble( a1b1c )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 )  pmom( l,4 ) = - 1.5 * dimag( a1b1c )
               if( l.eq.1 )  pmom( l,4 ) = 0.0
               if( l.eq.2 )  pmom( l,4 ) = 0.3 * dimag( a1b1c )
            end if
!
  100    continue
!
      end if
!
      return
      end subroutine  lpco1t 
!********************************************************************                                                  
      subroutine  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities in special case where
!         no. terms in mie series = 2
!
!        input:  nmom, ipolzn, momdim     'miev0' arguments
!                calcmo                   flags calculated from -ipolzn-
!                a(1-2), b(1-2)           mie series coefficients
!
!        output: pmom                     legendre moments
!
      implicit none
      logical  calcmo(*)
      integer  ipolzn, momdim, nmom,l,nummom
      real*8    pmom( 0:momdim, * ),sq,pm1,pm2,a2sq,b2sq
      complex*16  a(*), b(*)
      complex*16  a2c, b2c, ctmp, ca, cac, cat, cb, cbc, cbt, cg, ch
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      ca = 3. * a(1) - 5. * b(2)
      cat= 3. * b(1) - 5. * a(2)
      cac = dconjg( ca )
      a2sq = sq( a(2) )
      b2sq = sq( b(2) )
      a2c = dconjg( a(2) )
      b2c = dconjg( b(2) )
!
      if( ipolzn.lt.0 )  then
!                                   ** loop over sekera moments
         nummom = min0( nmom, 2 )
         do  50  l = 0, nummom
!
            if( calcmo(1) )  then
               if( l.eq.0 ) pmom( l,1 ) = 0.25 * ( sq(cat) +   &
                                                   (100./3.) * b2sq )
               if( l.eq.1 ) pmom( l,1 ) = (5./3.) * dble( cat * b2c )
               if( l.eq.2 ) pmom( l,1 ) = (10./3.) * b2sq
            end if
!
            if( calcmo(2) )  then
               if( l.eq.0 ) pmom( l,2 ) = 0.25 * ( sq(ca) +   &
                                                   (100./3.) * a2sq )
               if( l.eq.1 ) pmom( l,2 ) = (5./3.) * dble( ca * a2c )
               if( l.eq.2 ) pmom( l,2 ) = (10./3.) * a2sq
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cat*cac +   &
                                                 (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,3 ) = 5./6. * dble( b(2)*cac +   &
                                                        cat*a2c )
               if( l.eq.2 ) pmom( l,3 ) = 10./3. * dble( b(2) * a2c )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = -0.25 * dimag( cat*cac +   &
                                                 (100./3.)*b(2)*a2c )
               if( l.eq.1 ) pmom( l,4 ) = -5./6. * dimag( b(2)*cac +   &
                                                        cat*a2c )
               if( l.eq.2 ) pmom( l,4 ) = -10./3. * dimag( b(2) * a2c )
            end if
!
   50    continue
!
      else
!
         cb = 3. * b(1) + 5. * a(2)
         cbt= 3. * a(1) + 5. * b(2)
         cbc = dconjg( cb )
         cg = ( cbc*cbt + 10.*( cac*a(2) + b2c*cat) ) / 3.
         ch = 2.*( cbc*a(2) + b2c*cbt )
!
!                                   ** loop over mueller moments
         nummom = min0( nmom, 4 )
         do  100  l = 0, nummom
!
            if( ipolzn.eq.0 .or. calcmo(1) )  then
               if( l.eq.0 ) pm1 = 0.25 * sq(ca) + sq(cb) / 12.   &
                                  + (5./3.) * dble(ca*b2c) + 5.*b2sq
               if( l.eq.1 ) pm1 = dble( cb * ( cac/6. + b2c ) )
               if( l.eq.2 ) pm1 = sq(cb)/30. + (20./7.) * b2sq   &
                                  + (2./3.) * dble( ca * b2c )
               if( l.eq.3 ) pm1 = (2./7.) * dble( cb * b2c )
               if( l.eq.4 ) pm1 = (40./63.) * b2sq
               if ( calcmo(1) )  pmom( l,1 ) = pm1
            end if
!
            if( ipolzn.eq.0 .or. calcmo(2) )  then
               if( l.eq.0 ) pm2 = 0.25*sq(cat) + sq(cbt) / 12.   &
                                  + (5./3.) * dble(cat*a2c) + 5.*a2sq
               if( l.eq.1 ) pm2 = dble( cbt * ( dconjg(cat)/6. + a2c) )
               if( l.eq.2 ) pm2 = sq(cbt)/30. + (20./7.) * a2sq   &
                                  + (2./3.) * dble( cat * a2c )
               if( l.eq.3 ) pm2 = (2./7.) * dble( cbt * a2c )
               if( l.eq.4 ) pm2 = (40./63.) * a2sq
               if ( calcmo(2) )  pmom( l,2 ) = pm2
            end if
!
            if( ipolzn.eq.0 )  then
               pmom( l,1 ) = 0.5 * ( pm1 + pm2 )
               go to 100
            end if
!
            if( calcmo(3) )  then
               if( l.eq.0 ) pmom( l,3 ) = 0.25 * dble( cac*cat + cg +   &
                                                       20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,3 ) = dble( cac*cbt + cbc*cat +   &
                                                3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,3 ) = 0.1 * dble( cg + (200./7.) *   &
                                                      b2c * a(2) )
               if( l.eq.3 ) pmom( l,3 ) = dble( ch ) / 14.
               if( l.eq.4 ) pmom( l,3 ) = 40./63. * dble( b2c * a(2) )
            end if
!
            if( calcmo(4) )  then
               if( l.eq.0 ) pmom( l,4 ) = 0.25 * dimag( cac*cat + cg +   &
                                                        20.*b2c*a(2) )
               if( l.eq.1 ) pmom( l,4 ) = dimag( cac*cbt + cbc*cat +   &
                                                 3.*ch ) / 12.
               if( l.eq.2 ) pmom( l,4 ) = 0.1 * dimag( cg + (200./7.) *   &
                                                       b2c * a(2) )
               if( l.eq.3 ) pmom( l,4 ) = dimag( ch ) / 14.
               if( l.eq.4 ) pmom( l,4 ) = 40./63. * dimag( b2c * a(2) )
            end if
!
  100    continue
!
      end if
!
      return
      end subroutine  lpco2t 
!*********************************************************************                                                  
      subroutine  biga( cior, xx, ntrm, noabs, yesang, rbiga, cbiga )
!
!        calculate logarithmic derivatives of j-bessel-function
!
!     input :  cior, xx, ntrm, noabs, yesang  (defined in 'miev0')
!
!    output :  rbiga or cbiga  (defined in 'miev0')
!
!    internal variables :
!
!       confra     value of lentz continued fraction for -cbiga(ntrm)-,
!                     used to initialize downward recurrence.
!       down       = true, use down-recurrence.  false, do not.
!       f1,f2,f3   arithmetic statement functions used in determining
!                     whether to use up-  or down-recurrence
!                     ( ref. 2, eqs. 6-8 )
!       mre        real refractive index
!       mim        imaginary refractive index
!       rezinv     1 / ( mre * xx ); temporary variable for recurrence
!       zinv       1 / ( cior * xx ); temporary variable for recurrence
!
      implicit none
      logical  down, noabs, yesang
      integer  ntrm,n
      real*8    mre, mim, rbiga(*), xx, rezinv, rtmp, f1,f2,f3
!      complex*16  cior, ctmp, confra, cbiga(*), zinv
      complex*16  cior, ctmp,  cbiga(*), zinv
      f1( mre ) =  - 8.0 + mre**2 * ( 26.22 + mre * ( - 0.4474   &
                   + mre**3 * ( 0.00204 - 0.000175 * mre ) ) )
      f2( mre ) = 3.9 + mre * ( - 10.8 + 13.78 * mre )
      f3( mre ) =  - 15.04 + mre * ( 8.42 + 16.35 * mre )
!
!                                  ** decide whether 'biga' can be
!                                  ** calculated by up-recurrence
      mre =  dble( cior )
      mim =  dabs( dimag( cior ) )
      if ( mre.lt.1.0 .or. mre.gt.10.0 .or. mim.gt.10.0 )  then
         down = .true.
      else if ( yesang )  then
         down = .true.
         if ( mim*xx .lt. f2( mre ) )  down = .false.
      else
         down = .true.
         if ( mim*xx .lt. f1( mre ) )  down = .false.
      end if
!
      zinv  = 1.0 / ( cior * xx )
      rezinv = 1.0 / ( mre * xx )
!
      if ( down )  then
!                          ** compute initial high-order 'biga' using
!                          ** lentz method ( ref. 1, pp. 17-20 )
!
         ctmp = confra( ntrm, zinv, xx )
!
!                                   *** downward recurrence for 'biga'
!                                   *** ( ref. 1, eq. 22 )
         if ( noabs )  then
!                                            ** no-absorption case
            rbiga( ntrm ) = dble( ctmp )
            do  25  n = ntrm, 2, - 1
               rbiga( n-1 ) = (n*rezinv)   &
                               - 1.0 / ( (n*rezinv) + rbiga( n ) )
   25       continue
!
         else
!                                            ** absorptive case
            cbiga( ntrm ) = ctmp
            do  30  n = ntrm, 2, - 1
               cbiga( n-1 ) = (n*zinv) - 1.0 / ( (n*zinv) + cbiga( n ) )
   30       continue
!
         end if
!
      else
!                              *** upward recurrence for 'biga'
!                              *** ( ref. 1, eqs. 20-21 )
         if ( noabs )  then
!                                            ** no-absorption case
            rtmp = dsin( mre*xx )
            rbiga( 1 ) =  - rezinv   &
                           + rtmp / ( rtmp*rezinv - dcos( mre*xx ) )
            do  40  n = 2, ntrm
               rbiga( n ) = - ( n*rezinv )   &
                             + 1.0 / ( ( n*rezinv ) - rbiga( n-1 ) )
   40       continue
!
         else
!                                                ** absorptive case
            ctmp = cdexp( - dcmplx(0.d0,2.d0) * cior * xx )
            cbiga( 1 ) = - zinv + (1.-ctmp) / ( zinv * (1.-ctmp) -   &
                           dcmplx(0.d0,1.d0)*(1.+ctmp) )
            do  50  n = 2, ntrm
               cbiga( n ) = - (n*zinv) + 1.0 / ((n*zinv) - cbiga( n-1 ))
   50       continue
         end if
!
      end if
!
      return
      end subroutine  biga  
!**********************************************************************                                                   
      complex*16 function  confra( n, zinv, xx )
!
!         compute bessel function ratio capital-a-sub-n from its
!         continued fraction using lentz method ( ref. 1, pp. 17-20 )
!
!         zinv = reciprocal of argument of capital-a
!
!    i n t e r n a l    v a r i a b l e s
!    ------------------------------------
!
!    cak      term in continued fraction expansion of capital-a
!                ( ref. 1, eq. 25 )
!    capt     factor used in lentz iteration for capital-a
!                ( ref. 1, eq. 27 )
!    cdenom   denominator in -capt-  ( ref. 1, eq. 28b )
!    cnumer   numerator   in -capt-  ( ref. 1, eq. 28a )
!    cdtd     product of two successive denominators of -capt-
!                factors  ( ref. 1, eq. 34c )
!    cntn     product of two successive numerators of -capt-
!                factors  ( ref. 1, eq. 34b )
!    eps1     ill-conditioning criterion
!    eps2     convergence criterion
!    kk       subscript k of -cak-  ( ref. 1, eq. 25b )
!    kount    iteration counter ( used only to prevent runaway )
!    maxit    max. allowed no. of iterations
!    mm       + 1  and - 1, alternately
!
      implicit none
      integer   n,mm,kk,kount
      integer, save :: maxit
      data  maxit / 10000 /
      real*8     xx
      real*8, save :: eps1,eps2
      data  eps1 / 1.d-2 /, eps2 / 1.d-8 /
      complex*16   zinv
      complex*16   cak, capt, cdenom, cdtd, cnumer, cntn
!
!                                      *** ref. 1, eqs. 25a, 27
      confra = ( n + 1 ) * zinv
      mm     =  - 1
      kk     = 2 * n + 3
      cak    = ( mm * kk ) * zinv
      cdenom = cak
      cnumer = cdenom + 1.0 / confra
      kount  = 1
!
   20 kount = kount + 1
      if ( kount.gt.maxit )   &
           call errmsg( 'confra--iteration failed to converge$', .true.)
!
!                                         *** ref. 2, eq. 25b
      mm  =  - mm
      kk  = kk + 2
      cak = ( mm * kk ) * zinv
!                                         *** ref. 2, eq. 32
      if (      cdabs( cnumer/cak ).le.eps1   &
           .or. cdabs( cdenom/cak ).le.eps1 )  then
!
!                                  ** ill-conditioned case -- stride
!                                  ** two terms instead of one
!
!                                         *** ref. 2, eqs. 34
         cntn   = cak * cnumer + 1.0
         cdtd   = cak * cdenom + 1.0
         confra = ( cntn / cdtd ) * confra
!                                             *** ref. 2, eq. 25b
         mm  =  - mm
         kk  = kk + 2
         cak = ( mm * kk ) * zinv
!                                         *** ref. 2, eqs. 35
         cnumer = cak + cnumer / cntn
         cdenom = cak + cdenom / cdtd
         kount  = kount + 1
         go to 20
!
      else
!                                ** well-conditioned case
!
!                                        *** ref. 2, eqs. 26, 27
         capt   = cnumer / cdenom
         confra = capt * confra
!                                    ** check for convergence
!                                    ** ( ref. 2, eq. 31 )
!
         if (      dabs( dble(capt) - 1.0 ).ge.eps2   &
              .or. dabs( dimag(capt) )      .ge.eps2 )  then
!
!                                        *** ref. 2, eqs. 30a-b
            cnumer = cak + 1.0 / cnumer
            cdenom = cak + 1.0 / cdenom
            go to 20
         end if
      end if
!
      return
!
      end function confra
!********************************************************************      
      subroutine  miprnt( prnt, xx, perfct, crefin, numang, xmu,   &
                          qext, qsca, gqsc, nmom, ipolzn, momdim,   &
                          calcmo, pmom, sforw, sback, tforw, tback,   &
                          s1, s2 )
!
!         print scattering quantities of a single particle
!
      implicit none
      logical  perfct, prnt(*), calcmo(*)
      integer  ipolzn, momdim, nmom, numang,i,m,j
      real*8    gqsc, pmom( 0:momdim, * ), qext, qsca, xx, xmu(*)
      real*8 fi1,fi2,fnorm
      complex*16  crefin, sforw, sback, tforw(*), tback(*), s1(*), s2(*)
      character*22  fmt
!
!
      if ( perfct )  write ( *, '(''1'',10x,a,1p,e11.4)' )   &
                      'perfectly conducting case, size parameter =', xx
      if ( .not.perfct )  write ( *, '(''1'',10x,3(a,1p,e11.4))' )   &
                        'refractive index:  real ', dble(crefin),   &
                   '  imag ', dimag(crefin), ',   size parameter =', xx
!
      if ( prnt(1) .and. numang.gt.0 )  then
!
         write ( *, '(/,a)' )   &
          '    cos(angle)  ------- s1 ---------  ------- s2 ---------'//   &
          '  --- s1*conjg(s2) ---   i1=s1**2   i2=s2**2  (i1+i2)/2'//   &
          '  deg polzn'
         do  10  i = 1, numang
            fi1 = dble( s1(i) ) **2 + dimag( s1(i) )**2
            fi2 = dble( s2(i) ) **2 + dimag( s2(i) )**2
            write( *, '( i4, f10.6, 1p,10e11.3 )'   )   &
                    i, xmu(i), s1(i), s2(i), s1(i)*dconjg(s2(i)),   &
                    fi1, fi2, 0.5*(fi1+fi2), (fi2-fi1)/(fi2+fi1)
   10    continue
!
      end if
!
!
      if ( prnt(2) )  then
!
         write ( *, '(/,a,9x,a,17x,a,17x,a,/,(0p,f7.2, 1p,6e12.3) )' )   &
                 '  angle', 's-sub-1', 't-sub-1', 't-sub-2',   &
                     0.0,     sforw,    tforw(1),  tforw(2),   &
                    180.,     sback,    tback(1),  tback(2)
         write ( *, '(/,4(a,1p,e11.4))' )   &
                 ' efficiency factors,  extinction:', qext,   &
                                    '   scattering:', qsca,   &
                                    '   absorption:', qext-qsca,   &
                                 '   rad. pressure:', qext-gqsc
!
         if ( nmom.gt.0 )  then
!
            write( *, '(/,a)' )  ' normalized moments of :'
            if ( ipolzn.eq.0 ) write ( *, '(''+'',27x,a)' ) 'phase fcn'
            if ( ipolzn.gt.0 )  write ( *, '(''+'',33x,a)' )   &
               'm1           m2          s21          d21'
            if ( ipolzn.lt.0 )  write ( *, '(''+'',33x,a)' )   &
               'r1           r2           r3           r4'
!
            fnorm = 4. / ( xx**2 * qsca )
            do  20  m = 0, nmom
               write ( *, '(a,i4)' )  '      moment no.', m
               do 20  j = 1, 4
                  if( calcmo(j) )  then
                     write( fmt, 98 )  24 + (j-1)*13
                     write ( *,fmt )  fnorm * pmom(m,j)
                  end if
   20       continue
         end if
!
      end if
!
      return
!
   98 format( '( ''+'', t', i2, ', 1p,e13.4 )' )
      end subroutine  miprnt  
!**************************************************************************                                            
      subroutine  small1 ( xx, numang, xmu, qext, qsca, gqsc, sforw,   &
                           sback, s1, s2, tforw, tback, a, b )
!
!       small-particle limit of mie quantities in totally reflecting
!       limit ( mie series truncated after 2 terms )
!
!        a,b       first two mie coefficients, with numerator and
!                  denominator expanded in powers of -xx- ( a factor
!                  of xx**3 is missing but is restored before return
!                  to calling program )  ( ref. 2, p. 1508 )
!
      implicit none
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,fivnin,sq,rtmp
      complex*16  a( 2 ), b( 2 ), sforw, sback, s1(*), s2(*),   &
               tforw(*), tback(*)
!
      parameter  ( twothr = 2./3., fivthr = 5./3., fivnin = 5./9. )
      complex*16    ctmp
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      a( 1 ) = dcmplx ( 0.d0, twothr * ( 1. - 0.2 * xx**2 ) )   &
             / dcmplx ( 1.d0 - 0.5 * xx**2, twothr * xx**3 )
!
      b( 1 ) = dcmplx ( 0.d0, - ( 1. - 0.1 * xx**2 ) / 3. )   &
             / dcmplx ( 1.d0 + 0.5 * xx**2, - xx**3 / 3. )
!
      a( 2 ) = dcmplx ( 0.d0,   xx**2 / 30. )
      b( 2 ) = dcmplx ( 0.d0, - xx**2 / 45. )
!
      qsca = 6. * xx**4 * ( sq( a(1) ) + sq( b(1) )   &
                            + fivthr * ( sq( a(2) ) + sq( b(2) ) ) )
      qext = qsca
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) )   &
                          + ( b(1) + fivnin * a(2) ) * dconjg( b(2) ) )
!
      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * ( a(2) + b(2) ) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * ( a(2) - b(2) ) )
      tforw( 1 ) = rtmp * ( b(1) + fivthr * ( 2.*b(2) - a(2) ) )
      tforw( 2 ) = rtmp * ( a(1) + fivthr * ( 2.*a(2) - b(2) ) )
      tback( 1 ) = rtmp * ( b(1) - fivthr * ( 2.*b(2) + a(2) ) )
      tback( 2 ) = rtmp * ( a(1) - fivthr * ( 2.*a(2) + b(2) ) )
!
      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + b(1) * xmu(j) + fivthr *   &
                    ( a(2) * xmu(j) + b(2) * ( 2.*xmu(j)**2 - 1. )) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr *   &
                    ( b(2) * xmu(j) + a(2) * ( 2.*xmu(j)**2 - 1. )) )
   10 continue
!                                     ** recover actual mie coefficients
      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = xx**3 * b( 2 )
!
      return
      end subroutine  small1 
!*************************************************************************                                                 
      subroutine  small2 ( xx, cior, calcqe, numang, xmu, qext, qsca,   &
                           gqsc, sforw, sback, s1, s2, tforw, tback,   &
                           a, b )
!
!       small-particle limit of mie quantities for general refractive
!       index ( mie series truncated after 2 terms )
!
!        a,b       first two mie coefficients, with numerator and
!                  denominator expanded in powers of -xx- ( a factor
!                  of xx**3 is missing but is restored before return
!                  to calling program )  ( ref. 2, p. 1508 )
!
!        ciorsq    square of refractive index
!
      implicit none
      logical  calcqe
      integer  numang,j
      real*8    gqsc, qext, qsca, xx, xmu(*)
      real*8 twothr,fivthr,sq,rtmp
      complex*16  a( 2 ), b( 2 ), cior, sforw, sback, s1(*), s2(*),   &
               tforw(*), tback(*)
!
      parameter  ( twothr = 2./3., fivthr = 5./3. )
      complex*16  ctmp, ciorsq
      sq( ctmp ) = dble( ctmp )**2 + dimag( ctmp )**2
!
!
      ciorsq = cior**2
      ctmp = dcmplx( 0.d0, twothr ) * ( ciorsq - 1.0 )
      a(1) = ctmp * ( 1.0 - 0.1 * xx**2 + (ciorsq/350. + 1./280.)*xx**4)   &
             / ( ciorsq + 2.0 + ( 1.0 - 0.7 * ciorsq ) * xx**2   &
                 - ( ciorsq**2/175. - 0.275 * ciorsq + 0.25 ) * xx**4   &
                 + xx**3 * ctmp * ( 1.0 - 0.1 * xx**2 ) )
!
      b(1) = (xx**2/30.) * ctmp * ( 1.0 + (ciorsq/35. - 1./14.) *xx**2 )   &
             / ( 1.0 - ( ciorsq/15. - 1./6. ) * xx**2 )
!
      a(2) = ( 0.1 * xx**2 ) * ctmp * ( 1.0 - xx**2 / 14. )   &
             / ( 2. * ciorsq + 3. - ( ciorsq/7. - 0.5 ) * xx**2 )
!
      qsca = 6. * xx**4 * ( sq(a(1)) + sq(b(1)) + fivthr * sq(a(2)) )
      gqsc = 6. * xx**4 * dble( a(1) * dconjg( a(2) + b(1) ) )
      qext = qsca
      if ( calcqe ) qext = 6. * xx * dble( a(1) + b(1) + fivthr * a(2) )
!
      rtmp = 1.5 * xx**3
      sforw      = rtmp * ( a(1) + b(1) + fivthr * a(2) )
      sback      = rtmp * ( a(1) - b(1) - fivthr * a(2) )
      tforw( 1 ) = rtmp * ( b(1) - fivthr * a(2) )
      tforw( 2 ) = rtmp * ( a(1) + 2. * fivthr * a(2) )
      tback( 1 ) = tforw( 1 )
      tback( 2 ) = rtmp * ( a(1) - 2. * fivthr * a(2) )
!
      do  10  j = 1, numang
         s1( j ) = rtmp * ( a(1) + ( b(1) + fivthr * a(2) ) * xmu(j) )
         s2( j ) = rtmp * ( b(1) + a(1) * xmu(j) + fivthr * a(2)   &
                            * ( 2. * xmu(j)**2 - 1. ) )
   10 continue
!                                     ** recover actual mie coefficients
      a( 1 ) = xx**3 * a( 1 )
      a( 2 ) = xx**3 * a( 2 )
      b( 1 ) = xx**3 * b( 1 )
      b( 2 ) = ( 0., 0. )
!
      return
      end subroutine  small2  
!***********************************************************************                                                 
      subroutine  testmi ( qext, qsca, gqsc, sforw, sback, s1, s2,   &
                           tforw, tback, pmom, momdim, ok )
!
!         compare mie code test case results with correct answers
!         and return  ok=false  if even one result is inaccurate.
!
!         the test case is :  mie size parameter = 10
!                             refractive index   = 1.5 - 0.1 i
!                             scattering angle = 140 degrees
!                             1 sekera moment
!
!         results for this case may be found among the test cases
!         at the end of reference (1).
!
!         *** note *** when running on some computers, esp. in single
!         precision, the 'accur' criterion below may have to be relaxed.
!         however, if 'accur' must be set larger than 10**-3 for some
!         size parameters, your computer is probably not accurate
!         enough to do mie computations for those size parameters.
!
      implicit none
      integer momdim,m,n
      real*8    qext, qsca, gqsc, pmom( 0:momdim, * )
      complex*16  sforw, sback, s1(*), s2(*), tforw(*), tback(*)
      logical  ok, wrong
!
      real*8    accur, testqe, testqs, testgq, testpm( 0:1 )
      complex*16 testsf, testsb,tests1,tests2,testtf(2), testtb(2)
      data   testqe / 2.459791 /,  testqs / 1.235144 /,   &
             testgq / 1.139235 /,  testsf / ( 61.49476, -3.177994 ) /,   &
             testsb / ( 1.493434, 0.2963657 ) /,   &
             tests1 / ( -0.1548380, -1.128972) /,   &
             tests2 / ( 0.05669755, 0.5425681) /,   &
             testtf / ( 12.95238, -136.6436 ), ( 48.54238, 133.4656 ) /,   &
             testtb / ( 41.88414, -15.57833 ), ( 43.37758, -15.28196 )/,   &
             testpm / 227.1975, 183.6898 /
      real*8 calc,exact
!      data   accur / 1.e-5 /
      data   accur / 1.e-4 /
      wrong( calc, exact ) = dabs( (calc - exact) / exact ) .gt. accur
!
!
      ok = .true.
      if ( wrong( qext,testqe ) )   &
           call  tstbad( 'qext', abs((qext - testqe) / testqe), ok )
      if ( wrong( qsca,testqs ) )   &
           call  tstbad( 'qsca', abs((qsca - testqs) / testqs), ok )
      if ( wrong( gqsc,testgq ) )   &
           call  tstbad( 'gqsc', abs((gqsc - testgq) / testgq), ok )
!
      if ( wrong(  dble(sforw),  dble(testsf) ) .or.   &
           wrong( dimag(sforw), dimag(testsf) ) )   &
           call  tstbad( 'sforw', cdabs((sforw - testsf) / testsf), ok )
!
      if ( wrong(  dble(sback),  dble(testsb) ) .or.   &
           wrong( dimag(sback), dimag(testsb) ) )   &
           call  tstbad( 'sback', cdabs((sback - testsb) / testsb), ok )
!
      if ( wrong(  dble(s1(1)),  dble(tests1) ) .or.   &
           wrong( dimag(s1(1)), dimag(tests1) ) )   &
           call  tstbad( 's1', cdabs((s1(1) - tests1) / tests1), ok )
!
      if ( wrong(  dble(s2(1)),  dble(tests2) ) .or.   &
           wrong( dimag(s2(1)), dimag(tests2) ) )   &
           call  tstbad( 's2', cdabs((s2(1) - tests2) / tests2), ok )
!
      do  20  n = 1, 2
         if ( wrong(  dble(tforw(n)),  dble(testtf(n)) ) .or.   &
              wrong( dimag(tforw(n)), dimag(testtf(n)) ) )   &
              call  tstbad( 'tforw', cdabs( (tforw(n) - testtf(n)) /   &
                                           testtf(n) ), ok )
         if ( wrong(  dble(tback(n)),  dble(testtb(n)) ) .or.   &
              wrong( dimag(tback(n)), dimag(testtb(n)) ) )   &
              call  tstbad( 'tback', cdabs( (tback(n) - testtb(n)) /   &
                                            testtb(n) ), ok )
   20 continue
!
      do  30  m = 0, 1
         if ( wrong( pmom(m,1), testpm(m) ) )   &
              call  tstbad( 'pmom', dabs( (pmom(m,1)-testpm(m)) /   &
                                         testpm(m) ), ok )
   30 continue
!
      return
!
      end subroutine  testmi  
!**************************************************************************                                              
      subroutine  errmsg( messag, fatal )
!
!        print out a warning or error message;  abort if error
!
      USE module_peg_util, only:  peg_message, peg_error_fatal

      implicit none
      logical       fatal
      logical, save :: once
      data once / .false. /
      character*80 msg
      character*(*) messag
      integer, save :: maxmsg, nummsg
      data nummsg / 0 /,  maxmsg / 100 /
!
!
      if ( fatal )  then
   	  write( msg, '(a)' )   &
                  'optical averaging mie fatal error ' //   &
                  messag                  
                  call peg_message( lunerr, msg )             
                  call peg_error_fatal( lunerr, msg )
      end if
!
      nummsg = nummsg + 1
      if ( nummsg.gt.maxmsg )  then
!         if ( .not.once )  write ( *,99 )
	 if ( .not.once )then
	    write( msg, '(a)' )   &
             'optical averaging mie: too many warning messages -- no longer printing '
            call peg_message( lunerr, msg )
         end if    
         once = .true.
      else
         msg =   'optical averaging mie warning '  // messag
         call peg_message( lunerr, msg )  
!         write ( *, '(2a)' )  ' ******* warning >>>>>>  ', messag
      endif
!
      return
!
!   99 format( ///,' >>>>>>  too many warning messages --  ',   &
!         'they will no longer be printed  <<<<<<<', /// )
      end subroutine  errmsg  
!********************************************************************                     
      subroutine  wrtbad ( varnam, erflag )
!
!          write names of erroneous variables
!
!      input :   varnam = name of erroneous variable to be written
!                         ( character, any length )
!
!      output :  erflag = logical flag, set true by this routine
! ----------------------------------------------------------------------
      USE module_peg_util, only:  peg_message
      
      implicit none
      character*(*)  varnam
      logical        erflag
      character*80 msg
      integer, save :: maxmsg, nummsg
      data  nummsg / 0 /,  maxmsg / 50 /     
!
!
      nummsg = nummsg + 1
!      write ( *, '(3a)' )  ' ****  input variable  ', varnam,   &
!                           '  in error  ****'
        msg = 'optical averaging mie input variable in error ' // varnam                   
      call peg_message( lunerr, msg )
      erflag = .true.
      if ( nummsg.eq.maxmsg )   &     
         call  errmsg ( 'too many input variable errors.  aborting...$', .true. )
      return
!
      end subroutine  wrtbad 
!******************************************************************                        
      subroutine  tstbad( varnam, relerr, ok )
!
!       write name (-varnam-) of variable failing self-test and its
!       percent error from the correct value.  return  ok = false.
!
      implicit none
      character*(*)  varnam
      logical        ok
      real*8          relerr
!
!
      ok = .false.
      write( *, '(/,3a,1p,e11.2,a)' )   &
             ' output variable  ', varnam,'  differed by', 100.*relerr,   &
             '  per cent from correct value.  self-test failed.'
      return
!
      end subroutine  tstbad                      
!******************************************************************
!
      subroutine sect02(dgnum_um,sigmag,drydens,iflag,duma,nbin,dlo_um,dhi_um, &
        xnum_sect,xmas_sect)
!
!   user specifies a single log-normal mode and a set of section boundaries
!   prog calculates mass and number for each section
!
        implicit none
        REAL, DIMENSION(nbin), INTENT(OUT) :: xnum_sect, xmas_sect
        integer iflag, n, nbin 
        real &
          dgnum, dgnum_um, dhi, dhi_um, dlo, dlo_um,   &
          drydens, dstar, duma, dumfrac, dx,   &
          sigmag, sumnum, summas,   &
          sx, sxroot2, thi, tlo, vtot,   &
          x0, x3, xhi, xlo, xmtot, xntot, xvtot
        real dlo_sect(nbin), dhi_sect(nbin)
!       real erfc_num_recipes
        real pi
        parameter (pi = 3.1415926536)
!
        if (iflag .le. 1) then
            xntot = duma
        else
            xmtot = duma
            xntot = duma   !czhao
        end if
!   compute total volume and number for mode
!       dgnum = dgnum_um*1.0e-4
!       sx = log( sigmag )
!       x0 = log( dgnum )
!       x3 = x0 + 3.*sx*sx
!       dstar = dgnum * exp(1.5*sx*sx)
!       if (iflag .le. 1) then
!           xvtot = xntot*(pi/6.0)*dstar*dstar*dstar
!           xmtot = xvtot*drydens*1.0e12
!       else
!           xvtot = xmtot/(drydens*1.0e12)
!           xntot = xvtot/((pi/6.0)*dstar*dstar*dstar)
!       end if
!   compute section boundaries
        dlo = dlo_um*1.0e-4
        dhi = dhi_um*1.0e-4
        xlo = log( dlo )
        xhi = log( dhi )
        dx = (xhi - xlo)/nbin
        do n = 1, nbin
            dlo_sect(n) = exp( xlo + dx*(n-1) )
            dhi_sect(n) = exp( xlo + dx*n )
        end do
!   compute modal "working" parameters including total num/vol/mass
        dgnum = dgnum_um*1.0e-4
        sx = log( sigmag )
        x0 = log( dgnum )
        x3 = x0 + 3.*sx*sx
        dstar = dgnum * exp(1.5*sx*sx)
        if (iflag .le. 1) then
            xvtot = xntot*(pi/6.0)*dstar*dstar*dstar
            xmtot = xvtot*drydens*1.0e12
        else
!czhao      xvtot = xmtot/(drydens*1.0e12)
!czhao      xntot = xvtot/((pi/6.0)*dstar*dstar*dstar)
        end if
!   compute number and mass for each section
        sxroot2 = sx * sqrt( 2.0 )
        sumnum = 0.
        summas = 0.
!       write(22,*)
!       write(22,*) 'dgnum_um, sigmag = ', dgnum_um, sigmag
!       write(22,*) 'drydens =', drydens
!       write(22,*) 'ntot (#/cm3), mtot (ug/m3) = ', xntot, xmtot
!        write(22,9220)
!9220    format( /   &
!        '  n   dlo(um)   dhi(um)       number         mass' / )
!9225    format(   i3, 2f10.6, 2(1pe13.4) )
!9230    format( / 'sum over all sections  ', 2(1pe13.4) )
!9231    format(   'modal totals           ', 2(1pe13.4) )
        do n = 1, nbin
            xlo = log( dlo_sect(n) )
            xhi = log( dhi_sect(n) )
            tlo = (xlo - x0)/sxroot2
            thi = (xhi - x0)/sxroot2
            if (tlo .le. 0.) then
                dumfrac = 0.5*( erfc_num_recipes(-thi) - erfc_num_recipes(-tlo) )
            else
                dumfrac = 0.5*( erfc_num_recipes(tlo) - erfc_num_recipes(thi) )
            end if
            xnum_sect(n) = xntot*dumfrac
            tlo = (xlo - x3)/sxroot2
            thi = (xhi - x3)/sxroot2
            if (tlo .le. 0.) then
                dumfrac = 0.5*( erfc_num_recipes(-thi) - erfc_num_recipes(-tlo) )
            else
                dumfrac = 0.5*( erfc_num_recipes(tlo) - erfc_num_recipes(thi) )
            end if
            xmas_sect(n) = xmtot*dumfrac
            sumnum = sumnum + xnum_sect(n)
            summas = summas + xmas_sect(n)
!           write(22,9225) n, 1.e4*dlo_sect(n), 1.e4*dhi_sect(n),   &
!               xnum_sect(n), xmas_sect(n)
        end do
!       write(22,9230) sumnum, summas
!       write(22,9231) xntot, xmtot

      end subroutine  sect02
!-----------------------------------------------------------------------
        real function erfc_num_recipes( x )
!
!   from press et al, numerical recipes, 1990, page 164
!
        implicit none
        real x
        double precision erfc_dbl, dum, t, z
        z = abs(x)
        t = 1.0/(1.0 + 0.5*z)
!       erfc_num_recipes =
!     &   t*exp( -z*z - 1.26551223 + t*(1.00002368 + t*(0.37409196 +
!     &   t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
!     &                                    t*(-1.13520398 +
!     &   t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))
        dum =  ( -z*z - 1.26551223 + t*(1.00002368 + t*(0.37409196 +   &
          t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +   &
                                           t*(-1.13520398 +   &
          t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))
        erfc_dbl = t * exp(dum)
        if (x .lt. 0.0) erfc_dbl = 2.0d0 - erfc_dbl
        erfc_num_recipes = erfc_dbl
        return

        end function erfc_num_recipes
!-----------------------------------------------------------------------

!****************************************************************************
! <1.> subr mieaer_sc
! Purpose:  calculate aerosol optical depth, single scattering albedo,
!   asymmetry factor, extinction, Legendre coefficients, and average aerosol
!   size. parameterizes aerosol coefficients using a full-blown Mie code
! Calculates these properties for either (1) an aerosol internally mixed in a
! shell/core configuration or (2) internally mixed aerosol represented by
! volume averaging of refractive indices
!  Uses the ACKMIE code developed eons ago by Tom Ackerman  (Ackerman and Toon, 1981:
! absorption of visible radiation in the atmosphere containing mixtures of absorbing and 
! non-absorbing particles, Appl. Opt., 20, 3661-3668.
!
! INPUT
!       id -- grid id number
!   	iclm, jclm -- i,j of grid column being processed
!       nbin_a -- number of bins
!	number_bin_col(nbin_a,kmaxd) --   number density in layer, #/cm^3
!	radius_wet_col(nbin_a,kmaxd) -- wet radius, shell, cm
!       radius_core_col(nbin_a,kmaxd) -- core radius, cm; NOTE:
!		if this is set to zero, the code will assumed a volume averaging
!		of refractive indices
!    	refindx_col(nbin_a,kmaxd) -- volume complex index of refraction for shell, or
!       	volume averaged complex index of refraction for the whole aerosol
!		in volume averaged mode
!	refindx_core_col(nbin_a,kmaxd) -- complex index of refraction for core
!	dz -- depth of individual cells in column, m
!	curr_secs -- time from start of run, sec
!	lpar -- number of grid cells in vertical (via module_fastj_cmnh)
!   kmaxd -- predefined maximum allowed levels from module_data_mosaic_other
!            passed here via module_fastj_cmnh
! OUTPUT: saved in module_fastj_cmnmie
!   real tauaer  ! aerosol optical depth
!        waer    ! aerosol single scattering albedo
!        gaer    ! aerosol asymmetery factor
!        extaer  ! aerosol extinction
!        l2,l3,l4,l5,l6,l7 ! Legendre coefficients, numbered 0,1,2,......
!        sizeaer ! average wet radius
!	 bscoef ! aerosol backscatter coefficient with units km-1 * steradian -1  JCB 2007/02/01
!***********************************************************************


        subroutine mieaer_sc( &
	          id, iclm, jclm, nbin_a,   &
              number_bin_col, radius_wet_col, refindx_col,   &
              radius_core_col, refindx_core_col, &  ! jcb, 2007/07/25; for shell/core implementation, set radius_cor_col=0 for volume-average configuration
              dz, curr_secs, lpar, &
              sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7,bscoef)  ! added bscoef JCB 2007/02/01
!
	USE module_data_mosaic_other, only : kmaxd
	USE module_data_mosaic_therm, only : nbin_a_maxd
	USE module_peg_util, only : peg_message


        IMPLICIT NONE
!   subr arguments
!jdf
        integer,parameter :: nspint = 4 ! Num of spectral intervals across
                                        ! solar spectrum for FAST-J
        integer, intent(in) :: lpar
!jdf    real, dimension (nspint, kmaxd+1),intent(out) :: sizeaer,extaer,waer,gaer,tauaer
!jdf    real, dimension (nspint, kmaxd+1),intent(out) :: l2,l3,l4,l5,l6,l7
!jdf    real, dimension (nspint, kmaxd+1),intent(out) :: bscoef  !JCB 2007/02/01
        real, dimension (nspint, lpar+1),intent(out) :: sizeaer,extaer,waer,gaer,tauaer
        real, dimension (nspint, lpar+1),intent(out) :: l2,l3,l4,l5,l6,l7
        real, dimension (nspint, lpar+1),intent(out) :: bscoef  !JCB 2007/02/01
        real, dimension (nspint),save :: wavmid !cm
        data wavmid     &
            / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf
    integer, intent(in) :: id, iclm, jclm, nbin_a
    real(kind=8), intent(in) :: curr_secs
!jdf    real, intent(in), dimension(nbin_a, kmaxd) :: number_bin_col
!jdf    real, intent(inout), dimension(nbin_a, kmaxd) :: radius_wet_col
!jdf    complex, intent(in) :: refindx_col(nbin_a, kmaxd)
    real, intent(in), dimension(nbin_a, lpar+1) :: number_bin_col
    real, intent(inout), dimension(nbin_a, lpar+1) :: radius_wet_col, radius_core_col  ! jcb  2007/07/25
    complex, intent(in) :: refindx_col(nbin_a, lpar+1), refindx_core_col(nbin_a,lpar+1)  ! jcb  2007/07/25, 
    real, intent(in)    :: dz(lpar)
	real thesum, sum ! for normalizing things and testing
!
      integer m,l,j,nl,ll,nc,klevel
      integer       ns, &       ! Spectral loop index
                    i,  &      ! Longitude loop index
                    k         ! Level loop index

      real*8 dp_wet_a,dp_core_a
      complex*16 ri_shell_a,ri_core_a
      real*8 qextc,qscatc,qbackc,extc,scatc,backc,gscac
      real*8 vlambc
      integer n,kkk,jjj
	integer, save :: kcallmieaer
        data  kcallmieaer / 0 /
      real*8 pmom(0:7,1)
      real weighte, weights, pscat
      real pie,sizem
      real ratio
!
	real,save ::rmin,rmax  ! min, max aerosol size bin
!       data rmin /0.005e-04/   ! rmin in cm, 5e-3 microns min allowable size
!       data rmax /50.0e-04/    ! rmax in cm. 50 microns, big particle, max allowable size
	data rmin /0.010e-04/   ! rmin in cm, 5e-3 microns min allowable size
	data rmax /7.0e-04/    ! rmax in cm. 50 microns, big particle, max allowable size
! diagnostic declarations
	integer, save :: kcallmieaer2
        data  kcallmieaer2 / 0 /
      integer ibin
      character*150 msg

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec  diagnostics
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec run_out.25 has aerosol physical parameter info for bins 1-8
!ec and vertical cells 1 to kmaxd.
!        ilaporte = 33
!        jlaporte = 34
         kcallmieaer2=0
	if (iclm .eq. CHEM_DBG_I) then
	  if (jclm .eq. CHEM_DBG_J) then
!   initial entry
	   if (kcallmieaer2 .eq. 0) then
	      write(*,9099)iclm, jclm
 9099	format('for cell i = ', i3, 2x, 'j = ', i3)	
              write(*,9100)
 9100     format(   &
               'curr_secs', 3x, 'i', 3x, 'j', 3x,'k', 3x,   &
      	       'ibin', 3x,   &
               'refindx_col(ibin,k)', 3x,   &
               'radius_wet_col(ibin,k)', 3x,   &
               'number_bin_col(ibin,k)'   &
               )
	   end if
!ec output for run_out.25
            do k = 1, lpar
            do ibin = 1, nbin_a
              write(*, 9120)   &
                 curr_secs,iclm, jclm, k, ibin,   &
                 refindx_col(ibin,k),   &
                 radius_wet_col(ibin,k),   &
                 number_bin_col(ibin,k)
9120    format( i7,3(2x,i4),2x,i4, 4x, 4(e14.6,2x))
            end do
            end do
	kcallmieaer2 = kcallmieaer2 + 1
        end if
        end if
!ec end print of aerosol physical parameter diagnostics	
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#endif
!
! loop over levels
	do 2000 klevel=1,lpar
	thesum=0.0
	do m=1,nbin_a
	thesum=thesum+number_bin_col(m,klevel)
	enddo
      pie=4.*atan(1.)
! Begin spectral loop
      do 1000 ns=1,nspint
!        aerosol optical properties
               tauaer(ns,klevel)=0.
               waer(ns,klevel)=0.
               gaer(ns,klevel)=0.
	       sizeaer(ns,klevel)=0.0
		extaer(ns,klevel)=0.0
		l2(ns,klevel)=0.0
		l3(ns,klevel)=0.0
		l4(ns,klevel)=0.0
		l5(ns,klevel)=0.0
		l6(ns,klevel)=0.0
		l7(ns,klevel)=0.0
		bscoef(ns,klevel)=0.0
		if(thesum.le.1.e-21)goto 1000  ! set everything = 0 if no aerosol ! wig changed 0.0 to 1e-21

! loop over the bins, nbin_a is the number of bins
               do m=1,nbin_a
! check to see if there's any aerosol
!jdf   		if(number_bin_col(m,klevel).le.1e-21)goto 70  ! no aerosol wig changed 0.0 to 1e-21, 31-Oct-2005
! here's the size
		sizem=radius_wet_col(m,klevel) ! radius in cm
                ratio=radius_core_col(m,klevel)/radius_wet_col(m,klevel)
! check limits of particle size
! rce 2004-dec-07 - use klevel in write statements
		if(radius_wet_col(m,klevel).le.rmin)then
		  radius_wet_col(m,klevel)=rmin
                  radius_core_col(m,klevel)=rmin*ratio
		  write( msg, '(a, 5i4,1x, e11.4)' )	&
		  'mieaer_sc: radius_wet set to rmin,'  //	&
		  'id,i,j,k,m,rm(m,k)', id, iclm, jclm, klevel, m, radius_wet_col(m,klevel)
		  call peg_message( lunerr, msg )
!		   write(6,'('' particle size too small '')')
		endif
!
		if(radius_wet_col(m,klevel).gt.rmax)then
		   write( msg, '(a, 5i4,1x, e11.4)' )	&
                'mieaer_sc: radius_wet set to rmax,'  //	&
                'id,i,j,k,m,rm(m,k)', &
                id, iclm, jclm, klevel, m, radius_wet_col(m,klevel)
           call peg_message( lunerr, msg )
           radius_wet_col(m,klevel)=rmax
           radius_core_col(m,klevel)=rmax*ratio
!           write(6,'('' particle size too large '')')
		endif
!
                  ri_shell_a=dcmplx(real(refindx_col(m,klevel)),abs(aimag(refindx_col(m,klevel)))) ! need positive complex part of refractive index here
                  ri_core_a=dcmplx(real(refindx_core_col(m,klevel)),abs(aimag(refindx_core_col(m,klevel))))  ! need positive complex part of refractive index here
!
		dp_wet_a= 2.0*radius_wet_col(m,klevel)*1.0e04  ! radius_wet is in cm,dp_wet_a should be in microns
		dp_core_a=2.0*radius_core_col(m,klevel)*1.0e04
		vlambc=wavmid(ns)*1.0e04
!
!	write(6,*)dp_wet_a
!	write(6,*)ri_shell_a
!	write(6,*)vlambc
		call miedriver(dp_wet_a,dp_core_a,ri_shell_a,ri_core_a, vlambc, &
         	qextc,qscatc,gscac,extc,scatc,qbackc,backc,pmom)
! check, note that pmom(1,1)/pmom(0,1) is indeed the asymmetry parameter as calculated by Tom's code, jcb, July 7, 2007
! correct in the Rayleigh limit, July 3, 2007: jcb
!     	write(6,*)
!     	do ii=0,7
!	write(6,*)pmom(ii,1),pmom(ii,1)/pmom(0,1)
!	enddo
!   	write(6,*)qextc,qscatc,gscac,extc,scatc
!	write(6,*)
!
	weighte=extc*1.0e-08 ! extinction cross section, converted to cm^2
	weights=scatc*1.0e-08 ! scattering cross section, converted to cm^2
       	tauaer(ns,klevel)=tauaer(ns,klevel)+weighte* &
        number_bin_col(m,klevel)  ! must be multiplied by deltaZ
	sizeaer(ns,klevel)=sizeaer(ns,klevel)+radius_wet_col(m,klevel)*10000.0*  &
        number_bin_col(m,klevel)
        waer(ns,klevel)=waer(ns,klevel)+weights*number_bin_col(m,klevel)
        gaer(ns,klevel)=gaer(ns,klevel)+gscac*weights*  &
        number_bin_col(m,klevel)
	l2(ns,klevel)=l2(ns,klevel)+weights*pmom(2,1)/pmom(0,1)*5.0*number_bin_col(m,klevel)
	l3(ns,klevel)=l3(ns,klevel)+weights*pmom(3,1)/pmom(0,1)*7.0*number_bin_col(m,klevel)
	l4(ns,klevel)=l4(ns,klevel)+weights*pmom(4,1)/pmom(0,1)*9.0*number_bin_col(m,klevel)
	l5(ns,klevel)=l5(ns,klevel)+weights*pmom(5,1)/pmom(0,1)*11.0*number_bin_col(m,klevel)
	l6(ns,klevel)=l6(ns,klevel)+weights*pmom(6,1)/pmom(0,1)*13.0*number_bin_col(m,klevel)
	l7(ns,klevel)=l7(ns,klevel)+weights*pmom(7,1)/pmom(0,1)*15.0*number_bin_col(m,klevel)
! the 4*pi gives the correct value in the Rayleigh limit compared with the old core, which we assume is correct
	bscoef(ns,klevel)=bscoef(ns,klevel)+backc*1.0e-08*number_bin_col(m,klevel)*4.0*pie ! converting cross-section from microns ^2 to cm^2, 4*pie needed
2001	continue
        end do ! end of nbin loop
! take averages
	sizeaer(ns,klevel)=sizeaer(ns,klevel)/thesum
	gaer(ns,klevel)=gaer(ns,klevel)/waer(ns,klevel)
	l2(ns,klevel)=l2(ns,klevel)/waer(ns,klevel)
	l3(ns,klevel)=l3(ns,klevel)/waer(ns,klevel)
	l4(ns,klevel)=l4(ns,klevel)/waer(ns,klevel)
	l5(ns,klevel)=l5(ns,klevel)/waer(ns,klevel)
	l6(ns,klevel)=l6(ns,klevel)/waer(ns,klevel)
	l7(ns,klevel)=l7(ns,klevel)/waer(ns,klevel)
!	write(6,*)ns,klevel,l4(ns,klevel)
! this is beta, not beta/(4*pie)
	bscoef(ns,klevel)=bscoef(ns,klevel)*1.0e5 ! unit (km)^-1
! SSA checked by comparson with Travis and Hansen, get exact result
	waer(ns,klevel)=waer(ns,klevel)/tauaer(ns,klevel)  ! must be last
	extaer(ns,klevel)=tauaer(ns,klevel)*1.0e5 ! unit (km)^-1
 70   continue ! end of nbin_a loop
 1000 continue  ! end of wavelength loop
2000   continue  ! end of klevel loop
! before returning, multiply tauaer by depth of individual cells.
! tauaer is in cm-1, dz in m; multiply dz by 100 to convert from m to cm.
  	do ns = 1, nspint
  	do klevel = 1, lpar
  	   tauaer(ns,klevel) = tauaer(ns,klevel) * dz(klevel)* 100.   
        end do
        end do 	
!
#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec  fastj diagnostics
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ec run_out.30 has aerosol optical info for cells 1 to kmaxd.
!        ilaporte = 33
!         jlaporte = 34
 	if (iclm .eq. CHEM_DBG_I) then
 	  if (jclm .eq. CHEM_DBG_J) then
!   initial entry
 	   if (kcallmieaer .eq. 0) then
 	       write(*,909) CHEM_DBG_I, CHEM_DBG_J
 909	format(	' for cell i = ', i3, ' j = ', i3)          	
               write(*,910)
 910     format(   &
               'curr_secs', 3x, 'i', 3x, 'j', 3x,'k', 3x,   &
      	        'dzmfastj', 8x,   &
               'tauaer(1,k)',1x, 'tauaer(2,k)',1x,'tauaer(3,k)',3x,   &
               'tauaer(4,k)',5x,   &
               'waer(1,k)', 7x, 'waer(2,k)', 7x,'waer(3,k)', 7x,   &
               'waer(4,k)', 7x,   &
               'gaer(1,k)', 7x, 'gaer(2,k)', 7x,'gaer(3,k)', 7x,   &
               'gaer(4,k)', 7x,   &
               'extaer(1,k)',5x, 'extaer(2,k)',5x,'extaer(3,k)',5x,   &
               'extaer(4,k)',5x,   &
               'sizeaer(1,k)',4x, 'sizeaer(2,k)',4x,'sizeaer(3,k)',4x,   &
               'sizeaer(4,k)'  )
 	   end if
!ec output for run_out.30
         do k = 1, lpar
         write(*, 912)   &
           curr_secs,iclm, jclm, k,   &
           dz(k) ,   &
           (tauaer(n,k),   n=1,4),   &
           (waer(n,k),     n=1,4),   &
           (gaer(n,k),     n=1,4),   &
           (extaer(n,k),   n=1,4),   &
           (sizeaer(n,k),  n=1,4)
 912    format( i7,3(2x,i4),2x,21(e14.6,2x))
         end do
 	kcallmieaer = kcallmieaer + 1
        end if
         end if
!ec end print of fastj diagnostics	
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#endif
!
      return
      end subroutine mieaer_sc
!
	subroutine miedriver(dp_wet_a,dp_core_a,ri_shell_a,ri_core_a, vlambc, &
        qextc,qscatc,gscac,extc,scatc,qbackc,backc,pmom)
! MOSAIC INPUTS
!	dp_wet_a = diameter (cm) of aerosol
!	dp_core_a = diameter (cm) of the aerosol's core
!	ri_shell_a = refractive index (complex) of shell
!	ri_core_a = refractve index (complex ) of core (usually assumed to be LAC)
!	vlambc = wavelength of calculation (um, convert to cm)
! MOSAIC outputs
! 	qextc = scattering efficiency
!	qscac = scattering efficiency
!	gscac = asymmetry parameter
!	extc = extinction cross section  (cm^2)
!	scac = scattering cross section (cm^2)
! drives concentric sphere program
! /*---------------------------------------------------------------*/
! /* INPUTS:                                                       */
! /*---------------------------------------------------------------*/

!   VLAMBc: Wavelength of the radiation
!   NRGFLAGc: Flag to indicate a number density of volume radius
!   RGc: Number (RGN = Rm) or volume (RGV) weighted mean radius of
!        the particle size distribution
!   SIGMAGc: Geometric standard deviation of the distribution
!   SHELRc: Real part of the index of refraction for the shell
!   SHELIc: Imaginary part of the index of refraction for the shell
!   RINc: Inner core radius as a fraction of outer shell radius
!   CORERc: Real part of the index of refraction for the core
!   COREIc: Imaginary part of the index of refraction for the core
!   NANG: Number of scattering angles between 0 and 90 degrees,
!         inclusive

! /*---------------------------------------------------------------*/
! /* OUTPUTS:                                                      */
! /*---------------------------------------------------------------*/

!   QEXTc: Extinction efficiency of the particle
!   QSCAc: Scattering efficiency of the particle
!   QBACKc: Backscatter efficiency of the particle
!   EXTc: Extinction cross section of the particle
!   SCAc: Scattering cross section of the particle
!   BACKc: Backscatter cross section of the particle
!   GSCA: Asymmetry parameter of the particles phase function
!   ANGLES(NAN): Scattering angles in degrees
!   S1R(NAN): Real part of the amplitude scattering matrix
!   S1C(NAN): Complex part of the amplitude scattering matrix
!   S2R(NAN): Real part of the amplitude scattering matrix
!   S2C(NAN): Complex part of the amplitude scattering matrix
!   S11N: Normalization coefficient of the scattering matrix
!   S11(NAN): S11 scattering coefficients
!   S12(NAN): S12 scattering coefficients
!   S33(NAN): S33 scattering coefficients
!   S34(NAN): S34 scattering coefficients
!   SPOL(NAN): Degree of polarization of unpolarized, incident light
!   SP(NAN): Phase function
!
! NOTE: NAN=2*NANG-1 is the number of scattering angles between
!       0 and 180 degrees, inclusive.
! /*---------------------------------------------------------------*/
      REAL*8 VLAMBc,RGcmin,RGcmax,RGc,SIGMAGc,SHELRc,SHELIc
      REAL*8 RINc,CORERc,COREIc
      INTEGER*4 NRGFLAGc,NANG
      REAL*8 QEXTc,QSCATc,QBACKc,EXTc,SCATc,BACKc,GSCAc
      REAL*8 ANGLESc(200),S1R(200),S1C(200),S2R(200),S2C(200)
      REAL*8 S11N,S11(200),S12(200),S33(200),S34(200),SPOL(200),SP(200)
      real*8 pmom(0:7,1)
      real*8 dp_wet_a,dp_core_a
      complex*16 ri_shell_a,ri_core_a
!
	nang=2 ! only one angle
	nrgflagc=0 ! size distribution
!
	rgc=dp_wet_a/2.0 ! radius of particle
	rinc=dp_core_a/dp_wet_a ! fraction of radius that is the core
	rgcmin=0.001
	rgcmax=5.0
	sigmagc=1.0  ! no particle size dispersion
	shelrc=real(ri_shell_a)
	shelic=aimag(ri_shell_a)
	corerc=real(ri_core_a)
	coreic=aimag(ri_core_a)
       CALL ACKMIEPARTICLE( VLAMBc,NRGFLAGc,RGcmin,RGcmax, &
                  RGc,SIGMAGc,SHELRc, &
                  SHELIc, RINc,CORERc,COREIc,NANG,QEXTc,QSCATc, &
                  QBACKc, EXTc,SCATc,BACKc, GSCAc, &
               ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP,pmom)  ! jcb
!	write(6,1010)rgc,qextc,qscatc,qscatc/qextc,gscac
1010	format(5f20.12)
1020	format(2f12.6)
	end subroutine miedriver
!
!     /*--------------------------------------------------------*/
!     /* The Toon-Ackerman SUBROUTINE DMIESS for calculating the*/
!     /* scatter off of a coated sphere of some sort.           */
!     /* Toon and Ackerman, Applied Optics, Vol. 20, Pg. 3657   */
!     /*--------------------------------------------------------*/

!**********************************************

      SUBROUTINE DMIESS(  RO,      RFR,     RFI,     THETD,     JX,  &
                         QEXT,    QSCAT,   CTBRQS,  ELTRMX,    PIE, &
                         TAU,     CSTHT,   SI2THT,  ACAP, QBS, IT,  &
                         LL,      R,       RE2,     TMAG2,     WVNO, an,bn, ntrm  )
!
! **********************************************************************
!    THIS SUBROUTINE COMPUTES MIE SCATTERING BY A STRATIFIED SPHERE,
!    I.E. A PARTICLE CONSISTING OF A SPHERICAL CORE SURROUNDED BY A
!    SPHERICAL SHELL.  THE BASIC CODE USED WAS THAT DESCRIBED IN THE
!    REPORT: SUBROUTINES FOR COMPUTING THE PARAMETERS OF THE
!    ELECTROMAGNETIC RADIATION SCATTERED BY A SPHERE J.V. DAVE,
!    I B M SCIENTIFIC CENTER, PALO ALTO , CALIFORNIA.
!    REPORT NO. 320 - 3236 .. MAY 1968 .
!
!    THE MODIFICATIONS FOR STRATIFIED SPHERES ARE DESCRIBED IN
!        TOON AND ACKERMAN, APPL. OPTICS, IN PRESS, 1981
!
!    THE PARAMETERS IN THE CALLING STATEMENT ARE DEFINED AS FOLLOWS :
!      RO IS THE OUTER (SHELL) RADIUS;
!      R  IS THE CORE RADIUS;
!      RFR, RFI  ARE THE REAL AND IMAGINARY PARTS OF THE SHELL INDEX
!          OF REFRACTION IN THE FORM (RFR - I* RFI);
!      RE2, TMAG2  ARE THE INDEX PARTS FOR THE CORE;
!          ( WE ASSUME SPACE HAS UNIT INDEX. )
!      THETD(J): ANGLE IN DEGREES BETWEEN THE DIRECTIONS OF THE INCIDENT
!          AND THE SCATTERED RADIATION.  THETD(J) IS< OR= 90.0
!          IF THETD(J) SHOULD HAPPEN TO BE GREATER THAN 90.0, ENTER WITH
!          SUPPLEMENTARY VALUE, SEE COMMENTS BELOW ON ELTRMX;
!      JX: TOTAL NUMBER OF THETD FOR WHICH THE COMPUTATIONS ARE
!          REQUIRED.  JX SHOULD NOT EXCEED IT UNLESS THE DIMENSIONS
!          STATEMENTS ARE APPROPRIATEDLY MODIFIED;
!
!      THE DEFINITIONS FOR THE FOLLOWING SYMBOLS CAN BE FOUND IN LIGHT
!          SCATTERING BY SMALL PARTICLES, H.C.VAN DE HULST, JOHN WILEY
!          SONS, INC., NEW YORK, 1957.
!      QEXT: EFFIECIENCY FACTOR FOR EXTINCTION,VAN DE HULST,P.14 127.
!      QSCAT: EFFIECINCY FACTOR FOR SCATTERING,V.D. HULST,P.14 127.
!      CTBRQS: AVERAGE(COSINE THETA) * QSCAT,VAN DE HULST,P.128
!      ELTRMX(I,J,K): ELEMENTS OF THE TRANSFORMATION MATRIX F,V.D.HULST
!          ,P.34,45 125. I=1: ELEMENT M SUB 2..I=2: ELEMENT M SUB 1..
!          I = 3: ELEMENT S SUB 21.. I = 4: ELEMENT D SUB 21..
!      ELTRMX(I,J,1) REPRESENTS THE ITH ELEMENT OF THE MATRIX FOR
!          THE ANGLE THETD(J).. ELTRMX(I,J,2) REPRESENTS THE ITH ELEMENT
!          OF THE MATRIX FOR THE ANGLE 180.0 - THETD(J) ..
!      QBS IS THE BACK SCATTER CROSS SECTION.
!
!      IT: IS THE DIMENSION OF THETD, ELTRMX, CSTHT, PIE, TAU, SI2THT,
!          IT MUST CORRESPOND EXACTLY TO THE SECOND DIMENSION OF ELTRMX.
!      LL: IS THE DIMENSION OF ACAP
!          IN THE ORIGINAL PROGRAM THE DIMENSION OF ACAP WAS 7000.
!          FOR CONSERVING SPACE THIS SHOULD BE NOT MUCH HIGHER THAN
!          THE VALUE, N=1.1*(NREAL**2 + NIMAG**2)**.5 * X + 1
!      WVNO: 2*PIE / WAVELENGTH
!
!    ALSO THE SUBROUTINE COMPUTES THE CAPITAL A FUNCTION BY MAKING USE O
!    DOWNWARD RECURRENCE RELATIONSHIP.
!
!      TA(1): REAL PART OF WFN(1).  TA(2): IMAGINARY PART OF WFN(1).
!      TA(3): REAL PART OF WFN(2).  TA(4): IMAGINARY PART OF WFN(2).
!      TB(1): REAL PART OF FNA.     TB(2): IMAGINARY PART OF FNA.
!      TC(1): REAL PART OF FNB.     TC(2): IMAGINARY PART OF FNB.
!      TD(1): REAL PART OF FNAP.    TD(2): IMAGINARY PART OF FNAP.
!      TE(1): REAL PART OF FNBP.    TE(2): IMAGINARY PART OF FNBP.
!      FNAP, FNBP  ARE THE PRECEDING VALUES OF FNA, FNB RESPECTIVELY.
! **********************************************************************

!     /*--------------------------------------------------------------*/
!     /* Initially, make all types undefined.                         */
!     /*--------------------------------------------------------------*/

!      IMPLICIT UNDEFINED(A-Z)

!     /*--------------------------------------------------------*/
!     /* Dimension statements.                                  */
!     /*--------------------------------------------------------*/

      INTEGER*4  JX, IT, LL

      REAL*8     RO, RFR, RFI, THETD(IT), QEXT, QSCAT, CTBRQS, &
                ELTRMX(4,IT,2), PIE(3,IT), TAU(3,IT), CSTHT(IT), &
                SI2THT(IT), QBS, R,  RE2, TMAG2, WVNO

      COMPLEX*16 ACAP(LL)

!     /*--------------------------------------------------------*/
!     /* Variables used in the calculations below.              */
!     /*--------------------------------------------------------*/

      INTEGER*4  IFLAG, J, K, M, N, NN, NMX1, NMX2

      REAL*8     T(5), TA(4), TB(2), TC(2), TD(2), TE(2), X, &
                RX, X1, Y1, X4, Y4, SINX1, SINX4, COSX1, COSX4, &
                EY1, E2Y1, EY4, EY1MY4, EY1PY4, AA, BB, &
                CC, DD, DENOM, REALP, AMAGP, QBSR, QBSI, RMM, &
                PIG, RXP4
!
      COMPLEX*16 FNAP,      FNBP,      W,  &
                FNA,       FNB,       RF,           RRF, &
                RRFX,      WM1,       FN1,          FN2,   &
                TC1,       TC2,       WFN(2),       Z(4), &
                K1,        K2,        K3,    &
                RC,        U(8),      DH1, &
                DH2,       DH4,       P24H24,       P24H21, &
                PSTORE,    HSTORE,    DUMMY,        DUMSQ
! jcb
      complex*16 an(500),bn(500) ! a,b Mie coefficients, jcb  Hansen and Travis, eqn 2.44
      integer*4 ntrm
!
!     /*--------------------------------------------------------*/
!     /* Define the common block.                               */
!     /*--------------------------------------------------------*/

      COMMON / WARRAY / W(3,9000)



!
!      EQUIVALENCE   (FNA,TB(1)),(FNB,TC(1)),(FNAP,TD(1)),(FNBP,TE(1))
!
!   IF THE CORE IS SMALL SCATTERING IS COMPUTED FOR THE SHELL ONLY
!

!     /*--------------------------------------------------------*/
!     /* Begin the Mie calculations.                            */
!     /*--------------------------------------------------------*/
      IFLAG = 1
      ntrm=0 ! jcb
      IF ( R/RO .LT. 1.0D-06 )   IFLAG = 2                              
      IF ( JX .LE. IT )   GO TO 20
         WRITE( 6,7 )                                                   
         WRITE( 6,6 )
	 call errmsg( 'DMIESS: 30', .true.)
   20 RF =  CMPLX( RFR,  -RFI )
      RC =  CMPLX( RE2,-TMAG2 )
      X  =  RO * WVNO
      K1 =  RC * WVNO
      K2 =  RF * WVNO
      K3 =  CMPLX( WVNO, 0.0D0 )                                          
      Z(1) =  K2 * RO
      Z(2) =  K3 * RO                                                   
      Z(3) =  K1 * R
      Z(4) =  K2 * R                                                    
      X1   =  DREAL( Z(1) )
      Y1   =  DIMAG( Z(1) )                                              
      X4   =  DREAL( Z(4) )
      Y4   =  DIMAG( Z(4) )                                              
      RRF  =  1.0D0 / RF
      RX   =  1.0D0 / X                                                   
      RRFX =  RRF * RX
      T(1) =  ( X**2 ) * ( RFR**2 + RFI**2 )                            
      T(1) =  DSQRT( T(1) )
      NMX1 =  1.30D0* T(1)
!
      IF ( NMX1 .LE. LL-1 )   GO TO 21
         WRITE(6,8)
	 call errmsg( 'DMIESS: 32', .true.)
   21 NMX2 = T(1) * 1.2
	nmx1=min(nmx1+5,150)  ! jcb
	nmx2=min(nmx2+5,135)  ! jcb
!   	write(6,*)x,nmx1,nmx2,ll  ! jcb
!	stop
      IF ( NMX1 .GT.  150 )   GO TO 22
!        NMX1 = 150
!        NMX2 = 135
!
   22 ACAP( NMX1+1 )  =  ( 0.0D0,0.0D0 )
      IF ( IFLAG .EQ. 2 )   GO TO 26
         DO 29   N = 1,3                                                
   29    W( N,NMX1+1 )  =  ( 0.0D0,0.0D0 )
   26 CONTINUE                                                          
      DO 23   N = 1,NMX1
         NN = NMX1 - N + 1
         ACAP(NN) = (NN+1)*RRFX - 1.0D0 / ((NN+1)*RRFX + ACAP(NN+1))
         IF ( IFLAG .EQ. 2 )   GO TO 23                                 
            DO 31   M = 1,3
   31       W( M,NN ) = (NN+1) / Z(M+1)  -   &
                        1.0D0 / ((NN+1) / Z(M+1) + W( M,NN+1 ))
   23 CONTINUE                                                          
!
      DO 30   J = 1,JX                                                  
      IF ( THETD(J) .LT. 0.0D0 )  THETD(J) =  DABS( THETD(J) )
      IF ( THETD(J) .GT. 0.0D0 )  GO TO 24                                
      CSTHT(J)  = 1.0D0
      SI2THT(J) = 0.0D0                                                   
      GO TO 30
   24 IF ( THETD(J) .GE. 90.0D0 )  GO TO 25                               
      T(1)      =  ( 3.14159265359 * THETD(J) ) / 180.0D0
      CSTHT(J)  =  DCOS( T(1) )                                          
      SI2THT(J) =  1.0D0 - CSTHT(J)**2
      GO TO 30                                                          
   25 IF ( THETD(J) .GT. 90.0 )  GO TO 28
      CSTHT(J)  =  0.0D0                                               
      SI2THT(J) =  1.0D0
      GO TO 30                                                          
   28 WRITE( 6,5 )  THETD(J)
      WRITE( 6,6 )                                                      
      call errmsg( 'DMIESS: 34', .true.)
   30 CONTINUE                                                          
!
      DO 35  J = 1,JX                                                   
      PIE(1,J) =  0.0D0
      PIE(2,J) =  1.0D0
      TAU(1,J) =  0.0D0
      TAU(2,J) =  CSTHT(J)                                              
   35 CONTINUE
!
! INITIALIZATION OF HOMOGENEOUS SPHERE
!
      T(1)   =  DCOS(X)
      T(2)   =  DSIN(X)                                                  
      WM1    =  CMPLX( T(1),-T(2) )
      WFN(1) =  CMPLX( T(2), T(1) )                                     
      TA(1)  =  T(2)
      TA(2)  =  T(1)                                                    
      WFN(2) =  RX * WFN(1) - WM1
      TA(3)  =  DREAL(WFN(2))                                           
      TA(4)  =  DIMAG(WFN(2))
!
	n=1 ! jcb, bug???
      IF ( IFLAG .EQ. 2 )   GO TO 560
      N = 1                                                             
!
! INITIALIZATION PROCEDURE FOR STRATIFIED SPHERE BEGINS HERE
!
      SINX1   =  DSIN( X1 )                                              
      SINX4   =  DSIN( X4 )
      COSX1   =  DCOS( X1 )                                              
      COSX4   =  DCOS( X4 )
      EY1     =  DEXP( Y1 )                                              
      E2Y1    =  EY1 * EY1
      EY4     =  DEXP( Y4 )                                              
      EY1MY4  =  DEXP( Y1 - Y4 )
      EY1PY4  =  EY1 * EY4                                              
      EY1MY4  =  DEXP( Y1 - Y4 )
      AA  =  SINX4 * ( EY1PY4 + EY1MY4 )                                
      BB  =  COSX4 * ( EY1PY4 - EY1MY4 )
      CC  =  SINX1 * ( E2Y1 + 1.0D0 )
      DD  =  COSX1 * ( E2Y1 - 1.0D0 )
      DENOM   =  1.0D0  +  E2Y1 * (4.0D0*SINX1*SINX1 - 2.0D0 + E2Y1)    
      REALP   =  ( AA * CC  +  BB * DD ) / DENOM
      AMAGP   =  ( BB * CC  -  AA * DD ) / DENOM
      DUMMY   =  CMPLX( REALP, AMAGP )
      AA  =  SINX4 * SINX4 - 0.5D0                                        
      BB  =  COSX4 * SINX4
      P24H24  =  0.5D0 + CMPLX( AA,BB ) * EY4 * EY4                       
      AA  =  SINX1 * SINX4  -  COSX1 * COSX4
      BB  =  SINX1 * COSX4  +  COSX1 * SINX4                            
      CC  =  SINX1 * SINX4  +  COSX1 * COSX4
      DD  = -SINX1 * COSX4  +  COSX1 * SINX4                            
      P24H21  =  0.5D0 * CMPLX( AA,BB ) * EY1 * EY4  + &
                0.5D0 * CMPLX( CC,DD ) * EY1MY4
      DH4  =  Z(4) / (1.0D0 + (0.0D0,1.0D0) * Z(4))  -  1.0D0 / Z(4)
      DH1  =  Z(1) / (1.0D0 + (0.0D0,1.0D0) * Z(1))  -  1.0D0 / Z(1)        
      DH2  =  Z(2) / (1.0D0 + (0.0D0,1.0D0) * Z(2))  -  1.0D0 / Z(2)
      PSTORE  =  ( DH4 + N / Z(4) )  *  ( W(3,N) + N / Z(4) )           
      P24H24  =  P24H24 / PSTORE
      HSTORE  =  ( DH1 + N / Z(1) )  *  ( W(3,N) + N / Z(4) )           
      P24H21  =  P24H21 / HSTORE
      PSTORE  =  ( ACAP(N) + N / Z(1) )  /  ( W(3,N) + N / Z(4) )       
      DUMMY   =  DUMMY * PSTORE
      DUMSQ   =  DUMMY * DUMMY                                          
!
! NOTE:  THE DEFINITIONS OF U(I) IN THIS PROGRAM ARE NOT THE SAME AS
!        THE USUBI DEFINED IN THE ARTICLE BY TOON AND ACKERMAN.  THE
!        CORRESPONDING TERMS ARE:
!          USUB1 = U(1)                       USUB2 = U(5)
!          USUB3 = U(7)                       USUB4 = DUMSQ
!          USUB5 = U(2)                       USUB6 = U(3)
!          USUB7 = U(6)                       USUB8 = U(4)
!          RATIO OF SPHERICAL BESSEL FTN TO SPHERICAL HENKAL FTN = U(8)
!
      U(1) =  K3 * ACAP(N)  -  K2 * W(1,N)
      U(2) =  K3 * ACAP(N)  -  K2 * DH2                                 
      U(3) =  K2 * ACAP(N)  -  K3 * W(1,N)
      U(4) =  K2 * ACAP(N)  -  K3 * DH2                                 
      U(5) =  K1 *  W(3,N)  -  K2 * W(2,N)
      U(6) =  K2 *  W(3,N)  -  K1 * W(2,N)                              
      U(7) =  ( 0.0D0,-1.0D0 )  *  ( DUMMY * P24H21 - P24H24 )
      U(8) =  TA(3) / WFN(2)                                            
!
      FNA  =  U(8) * ( U(1)*U(5)*U(7)  +  K1*U(1)  -  DUMSQ*K3*U(5) ) / &
                    ( U(2)*U(5)*U(7)  +  K1*U(2)  -  DUMSQ*K3*U(5) )
      FNB  =  U(8) * ( U(3)*U(6)*U(7)  +  K2*U(3)  -  DUMSQ*K2*U(6) ) / &
                    ( U(4)*U(6)*U(7)  +  K2*U(4)  -  DUMSQ*K2*U(6) )
!
!  Explicit equivalences added by J. Francis

      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)
      GO TO 561                                                         
  560 TC1  =  ACAP(1) * RRF  +  RX
      TC2  =  ACAP(1) * RF   +  RX                                      
      FNA  =  ( TC1 * TA(3)  -  TA(1) ) / ( TC1 * WFN(2)  -  WFN(1) )
      FNB  =  ( TC2 * TA(3)  -  TA(1) ) / ( TC2 * WFN(2)  -  WFN(1) )   
      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)
!
  561 CONTINUE
! jcb
	ntrm=ntrm+1
	an(n)=fna
	bn(n)=fnb
!	write(6,1010)ntrm,n,an(n),bn(n)
1010	format(2i5,4e15.6)
! jcb
      FNAP = FNA
      FNBP = FNB                                                        
      TD(1) = DREAL(FNAP)
      TD(2) = DIMAG(FNAP)
      TE(1) = DREAL(FNBP)
      TE(2) = DIMAG(FNBP)
      T(1) = 1.50D0
!
!    FROM HERE TO THE STATMENT NUMBER 90, ELTRMX(I,J,K) HAS
!    FOLLOWING MEANING:
!    ELTRMX(1,J,K): REAL PART OF THE FIRST COMPLEX AMPLITUDE.
!    ELTRMX(2,J,K): IMAGINARY PART OF THE FIRST COMPLEX AMPLITUDE.
!    ELTRMX(3,J,K): REAL PART OF THE SECOND COMPLEX AMPLITUDE.
!    ELTRMX(4,J,K): IMAGINARY PART OF THE SECOND COMPLEX AMPLITUDE.
!    K = 1 : FOR THETD(J) AND K = 2 : FOR 180.0 - THETD(J)
!    DEFINITION OF THE COMPLEX AMPLITUDE: VAN DE HULST,P.125.
!
      TB(1) = T(1) * TB(1)                                              
      TB(2) = T(1) * TB(2)
      TC(1) = T(1) * TC(1)                                              
      TC(2) = T(1) * TC(2)
!      DO 60 J = 1,JX
!          ELTRMX(1,J,1) = TB(1) * PIE(2,J) + TC(1) * TAU(2,J)
!          ELTRMX(2,J,1) = TB(2) * PIE(2,J) + TC(2) * TAU(2,J)
!          ELTRMX(3,J,1) = TC(1) * PIE(2,J) + TB(1) * TAU(2,J)
!          ELTRMX(4,J,1) = TC(2) * PIE(2,J) + TB(2) * TAU(2,J)
!          ELTRMX(1,J,2) = TB(1) * PIE(2,J) - TC(1) * TAU(2,J)
!          ELTRMX(2,J,2) = TB(2) * PIE(2,J) - TC(2) * TAU(2,J)
!          ELTRMX(3,J,2) = TC(1) * PIE(2,J) - TB(1) * TAU(2,J)
!          ELTRMX(4,J,2) = TC(2) * PIE(2,J) - TB(2) * TAU(2,J)
   60 CONTINUE
!
      QEXT   = 2.0D0 * ( TB(1) + TC(1))
      QSCAT  = ( TB(1)**2 + TB(2)**2 + TC(1)**2 + TC(2)**2 ) / 0.75D0     
      CTBRQS = 0.0D0
      QBSR   = -2.0D0*(TC(1) - TB(1))                                     
      QBSI   = -2.0D0*(TC(2) - TB(2))
      RMM    = -1.0D0                                                     
      N = 2
   65 T(1) = 2*N - 1   ! start of loop, JCB
      T(2) =   N - 1
      T(3) = 2*N + 1
      DO 70  J = 1,JX
          PIE(3,J) = ( T(1)*PIE(2,J)*CSTHT(J) - N*PIE(1,J) ) / T(2) 
          TAU(3,J) = CSTHT(J) * ( PIE(3,J) - PIE(1,J) ) - &
                               T(1)*SI2THT(J)*PIE(2,J) + TAU(1,J)
   70 CONTINUE
!
! HERE SET UP HOMOGENEOUS SPHERE
!
      WM1    =  WFN(1)
      WFN(1) =  WFN(2)                                                  
      TA(1)  =  DREAL(WFN(1))
      TA(2)  =  DIMAG(WFN(1))                                            
      WFN(2) =  T(1) * RX * WFN(1)  -  WM1
      TA(3)  =  DREAL(WFN(2))                                            
      TA(4)  =  DIMAG(WFN(2))
!
      IF ( IFLAG .EQ. 2 )   GO TO 1000
!
! HERE SET UP STRATIFIED SPHERE
!
      DH2  =  - N / Z(2)  +  1.0D0 / ( N / Z(2) - DH2 )
      DH4  =  - N / Z(4)  +  1.0D0 / ( N / Z(4) - DH4 )                   
      DH1  =  - N / Z(1)  +  1.0D0 / ( N / Z(1) - DH1 )
      PSTORE  =  ( DH4 + N / Z(4) )  *  ( W(3,N) + N / Z(4) )           
      P24H24  =  P24H24 / PSTORE
      HSTORE  =  ( DH1 + N / Z(1) )  *  ( W(3,N) + N / Z(4) )           
      P24H21  =  P24H21 / HSTORE
      PSTORE  =  ( ACAP(N) + N / Z(1) )  /  ( W(3,N) + N / Z(4) )       
      DUMMY   =  DUMMY * PSTORE
      DUMSQ   =  DUMMY * DUMMY                                          
!
      U(1) =  K3 * ACAP(N)  -  K2 * W(1,N)                              
      U(2) =  K3 * ACAP(N)  -  K2 * DH2
      U(3) =  K2 * ACAP(N)  -  K3 * W(1,N)
      U(4) =  K2 * ACAP(N)  -  K3 * DH2
      U(5) =  K1 *  W(3,N)  -  K2 * W(2,N)                              
      U(6) =  K2 *  W(3,N)  -  K1 * W(2,N)
      U(7) =  ( 0.0D0,-1.0D0 )  *  ( DUMMY * P24H21 - P24H24 )              
      U(8) =  TA(3) / WFN(2)
!
      FNA  =  U(8) * ( U(1)*U(5)*U(7)  +  K1*U(1)  -  DUMSQ*K3*U(5) ) / &
                    ( U(2)*U(5)*U(7)  +  K1*U(2)  -  DUMSQ*K3*U(5) )
      FNB  =  U(8) * ( U(3)*U(6)*U(7)  +  K2*U(3)  -  DUMSQ*K2*U(6) ) / &
                    ( U(4)*U(6)*U(7)  +  K2*U(4)  -  DUMSQ*K2*U(6) )
      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)
!
 1000 CONTINUE                                                          
      TC1  =  ACAP(N) * RRF  +  N * RX
      TC2  =  ACAP(N) * RF   +  N * RX                                  
      FN1  =  ( TC1 * TA(3)  -  TA(1) ) /  ( TC1 * WFN(2) - WFN(1) )
      FN2  =  ( TC2 * TA(3)  -  TA(1) ) /  ( TC2 * WFN(2) - WFN(1) )    
      M    =  WVNO * R
      IF ( N .LT. M )   GO TO 1002                                      
      IF ( IFLAG .EQ. 2 )   GO TO 1001
      IF ( ABS(  ( FN1-FNA ) / FN1  )  .LT. 1.0D-09   .AND.    &
          ABS(  ( FN2-FNB ) / FN2  )  .LT. 1.0D-09  )     IFLAG = 2
      IF ( IFLAG .EQ. 1 )   GO TO 1002                                  
 1001 FNA  =  FN1
      FNB  =  FN2                                                       
      TB(1) = DREAL(FNA)
      TB(2) = DIMAG(FNA)
      TC(1) = DREAL(FNB)
      TC(2) = DIMAG(FNB)
!
 1002 CONTINUE
! jcb
 	ntrm=ntrm+1
 	an(n)=fna
	bn(n)=fnb
!	write(6,1010)ntrm,n,an(n),bn(n)
! jcb
      T(5)  =  N
      T(4)  =  T(1) / ( T(5) * T(2) )                                   
      T(2)  =  (  T(2) * ( T(5) + 1.0D0 )  ) / T(5)
!
      CTBRQS  =  CTBRQS  +  T(2) * ( TD(1) * TB(1)  +  TD(2) * TB(2)  &
                        +           TE(1) * TC(1)  +  TE(2) * TC(2) )  &
                        +  T(4) * ( TD(1) * TE(1)  +  TD(2) * TE(2) )
      QEXT    =   QEXT  +  T(3) * ( TB(1) + TC(1) )                     
      T(4)    =  TB(1)**2 + TB(2)**2 + TC(1)**2 + TC(2)**2
      QSCAT   =  QSCAT  +  T(3) * T(4)                                  
      RMM     =  -RMM
      QBSR    =  QBSR + T(3)*RMM*(TC(1) - TB(1))                        
      QBSI    =  QBSI + T(3)*RMM*(TC(2) - TB(2))
!
      T(2)    =  N * (N+1)
      T(1)    =  T(3) / T(2)                                            
      K = (N/2)*2
!      DO 80 J = 1,JX
!       ELTRMX(1,J,1)=ELTRMX(1,J,1)+T(1)*(TB(1)*PIE(3,J)+TC(1)*TAU(3,J))
!       ELTRMX(2,J,1)=ELTRMX(2,J,1)+T(1)*(TB(2)*PIE(3,J)+TC(2)*TAU(3,J))
!       ELTRMX(3,J,1)=ELTRMX(3,J,1)+T(1)*(TC(1)*PIE(3,J)+TB(1)*TAU(3,J))
!       ELTRMX(4,J,1)=ELTRMX(4,J,1)+T(1)*(TC(2)*PIE(3,J)+TB(2)*TAU(3,J))
!      IF ( K .EQ. N )  THEN
!       ELTRMX(1,J,2)=ELTRMX(1,J,2)+T(1)*(-TB(1)*PIE(3,J)+TC(1)*TAU(3,J))
!       ELTRMX(2,J,2)=ELTRMX(2,J,2)+T(1)*(-TB(2)*PIE(3,J)+TC(2)*TAU(3,J))
!       ELTRMX(3,J,2)=ELTRMX(3,J,2)+T(1)*(-TC(1)*PIE(3,J)+TB(1)*TAU(3,J))
!       ELTRMX(4,J,2)=ELTRMX(4,J,2)+T(1)*(-TC(2)*PIE(3,J)+TB(2)*TAU(3,J))
!      ELSE
!       ELTRMX(1,J,2)=ELTRMX(1,J,2)+T(1)*(TB(1)*PIE(3,J)-TC(1)*TAU(3,J))
!       ELTRMX(2,J,2)=ELTRMX(2,J,2)+T(1)*(TB(2)*PIE(3,J)-TC(2)*TAU(3,J))
!       ELTRMX(3,J,2)=ELTRMX(3,J,2)+T(1)*(TC(1)*PIE(3,J)-TB(1)*TAU(3,J))
!       ELTRMX(4,J,2)=ELTRMX(4,J,2)+T(1)*(TC(2)*PIE(3,J)-TB(2)*TAU(3,J))
!      END IF
!   80 CONTINUE
!
!      IF ( T(4) .LT. 1.0D-14 )   GO TO 100         ! bail out of loop
      IF ( T(4) .LT. 1.0D-10 .OR. N .GE. NMX2)   GO TO 100         ! bail out of loop, JCB
      N = N + 1
!      DO 90 J = 1,JX
!         PIE(1,J)  =   PIE(2,J)
!         PIE(2,J)  =   PIE(3,J)
!         TAU(1,J)  =  TAU(2,J)
!         TAU(2,J)  =  TAU(3,J)
   90 CONTINUE                                                          
      FNAP  =  FNA
      FNBP  =  FNB                                                      
      TD(1) = DREAL(FNAP)
      TD(2) = DIMAG(FNAP)
      TE(1) = DREAL(FNBP)
      TE(2) = DIMAG(FNBP)
      IF ( N .LE. NMX2 )   GO TO 65
         WRITE( 6,8 )                                                   
	 call errmsg( 'DMIESS: 36', .true.)
  100 CONTINUE
!      DO 120 J = 1,JX
!      DO 120 K = 1,2
!         DO  115  I= 1,4
!         T(I)  =  ELTRMX(I,J,K)
!  115    CONTINUE
!         ELTRMX(2,J,K)  =      T(1)**2  +  T(2)**2
!         ELTRMX(1,J,K)  =      T(3)**2  +  T(4)**2
!         ELTRMX(3,J,K)  =  T(1) * T(3)  +  T(2) * T(4)
!         ELTRMX(4,J,K)  =  T(2) * T(3)  -  T(4) * T(1)
!  120 CONTINUE
      T(1)    =  2.0D0 * RX**2
      QEXT    =   QEXT * T(1)                                           
      QSCAT   =  QSCAT * T(1)
      CTBRQS  =  2.0D0 * CTBRQS * T(1)                                    
!
! QBS IS THE BACK SCATTER CROSS SECTION
!
      PIG   = DACOS(-1.0D0)                                                
      RXP4  = RX*RX/(4.0D0*PIG)
      QBS   = RXP4*(QBSR**2 + QBSI**2)                                  
!
    5 FORMAT( 10X,' THE VALUE OF THE SCATTERING ANGLE IS GREATER THAN 90.0 DEGREES. IT IS ', E15.4 )
    6 FORMAT( // 10X, 'PLEASE READ COMMENTS.' // )
    7 FORMAT( // 10X, 'THE VALUE OF THE ARGUMENT JX IS GREATER THAN IT')
    8 FORMAT( // 10X, 'THE UPPER LIMIT FOR ACAP IS NOT ENOUGH. SUGGEST GET DETAILED OUTPUT AND MODIFY SUBROUTINE' // )
!
      RETURN
      END SUBROUTINE DMIESS
!
! /*****************************************************************/
! /* SUBROUTINE ACKMIEPARICLE                                      */
! /*****************************************************************/

! THIS PROGRAM COMPUTES SCATTERING PROPERTIES FOR DISTRIBUTIONS OF
! PARTICLES COMPOSED OF A CORE OF ONE MATERIAL AND A SHELL OF ANOTHER.

! /*---------------------------------------------------------------*/
! /* INPUTS:                                                       */
! /*---------------------------------------------------------------*/

!   VLAMBc: Wavelength of the radiation
!   NRGFLAGc: Flag to indicate a number density of volume radius
!   RGc: Geometric mean radius of the particle distribution
!   SIGMAGc: Geometric standard deviation of the distribution
!   SHELRc: Real part of the index of refraction for the shell
!   SHELIc: Imaginary part of the index of refraction for the shell
!   RINc: Inner core radius as a fraction of outer shell radius
!   CORERc: Real part of the index of refraction for the core
!   COREIc: Imaginary part of the index of refraction for the core
!   NANG: Number of scattering angles between 0 and 90 degrees,
!         inclusive

! /*---------------------------------------------------------------*/
! /* OUTPUTS:                                                      */
! /*---------------------------------------------------------------*/

!   QEXTc: Extinction efficiency of the particle
!   QSCAc: Scattering efficiency of the particle
!   QBACKc: Backscatter efficiency of the particle
!   EXTc: Extinction cross section of the particle
!   SCAc: Scattering cross section of the particle
!   BACKc: Backscatter cross section of the particle
!   GSCA: Asymmetry parameter of the particle's phase function
!   ANGLES(NAN): Scattering angles in degrees
!   S1R(NAN): Real part of the amplitude scattering matrix
!   S1C(NAN): Complex part of the amplitude scattering matrix
!   S2R(NAN): Real part of the amplitude scattering matrix
!   S2C(NAN): Complex part of the amplitude scattering matrix
!   S11N: Normalization coefficient of the scattering matrix
!   S11(NAN): S11 scattering coefficients
!   S12(NAN): S12 scattering coefficients
!   S33(NAN): S33 scattering coefficients
!   S34(NAN): S34 scattering coefficients
!   SPOL(NAN): Degree of polarization of unpolarized, incident light
!   SP(NAN): Phase function
!
! NOTE: NAN=2*NANG-1 is the number of scattering angles between
!       0 and 180 degrees, inclusive.
! /*---------------------------------------------------------------*/

      SUBROUTINE ACKMIEPARTICLE( VLAMBc,NRGFLAGc,RGcmin,RGcmax, &
                  RGc,SIGMAGc,SHELRc, &
                  SHELIc, RINc,CORERc,COREIc,NANG,QEXTc,QSCATc, &
                  QBACKc, EXTc,SCATc,BACKc, GSCAc, &
               ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP,pmom)  ! jcb

!     /*--------------------------------------------------------*/
!     /* Set reals to 8 bytes, i.e., double precision.          */
!     /*--------------------------------------------------------*/

      IMPLICIT REAL*8 (A-H, O-Z)

!     /*--------------------------------------------------------*/
!     /* Parameter statements.                                  */
!     /*--------------------------------------------------------*/

      integer*4 mxnang

      PARAMETER(MXNANG=501)

!     /*--------------------------------------------------------*/
!     /* Dimension statements.                                  */
!     /*--------------------------------------------------------*/

      REAL*8 VLAMBc,RGcmin,RGcmax,RGc,SIGMAGc,SHELRc,SHELIc
      REAL*8 RINc,CORERc,COREIc
      INTEGER*4 NANG,NRGFLAGc,NSCATH
      REAL*8 QEXTc,QSCATc,QBACKc,EXTc,SCATc,BACKc,GSCAc
      REAL*8 ANGLESc(*),S1R(*),S1C(*),S2R(*),S2C(*)
      REAL*8 S11N,S11(*),S12(*),S33(*),S34(*),SPOL(*),SP(*)

!     /*--------------------------------------------------------*/
!     /* Define the types of the common block.                  */
!     /*--------------------------------------------------------*/

      INTEGER*4 IPHASEmie

      REAL*8 ALAMB, RGmin, RGmax, RGV, SIGMAG, RGCFRAC, RFRS,RFIS, RFRC, RFIC

! for calculating the Legendre coefficient, jcb
        complex*16 an(500),bn(500) ! a,b Mie coefficients, jcb  Hansen and Travis, eqn 2.44
	integer*4 ntrmj,ntrm,nmom,ipolzn,momdim
	real*8 pmom(0:7,1)

!     /*--------------------------------------------------------*/
!     /* Set reals to 8 bytes, i.e., double precision.          */
!     /*--------------------------------------------------------*/

!      IMPLICIT REAL*8 (A-H, O-Z)

!     /*--------------------------------------------------------*/
!     /* Input common block for scattering calculations.        */
!     /*--------------------------------------------------------*/

!jdf  COMMON / PHASE  / IPHASEmie

!jdf  COMMON / INPUTS / ALAMB, RGmin, RGmax, RGV, SIGMAG, &
!jdf                    RGCFRAC, RFRS,  RFIS,  RFRC,  RFIC

!     /*--------------------------------------------------------*/
!     /* Output common block for scattering calculations.       */
!     /*--------------------------------------------------------*/

!jdf  COMMON / OUTPUTS / QEXT, QSCAT, QBS, EXT, SCAT, BSCAT, ASY

!     /*--------------------------------------------------------*/
!     /* Arrays to hold the results of the scattering and       */
!     /* moment calculations.                                   */
!     /*--------------------------------------------------------*/

      REAL*8 COSPHI(2*MXNANG-1), SCTPHS(2*MXNANG-1)


!     /*--------------------------------------------------------*/
!     /* Copy the input parameters into the common block INPUTS */
!     /*--------------------------------------------------------*/

         NSCATH  = NANG

         IPHASEmie = 0
         ALAMB   = VLAMBc
         RGmin   = RGcmin
         RGmax   = RGcmax
         RGV     = RGc
         SIGMAG  = SIGMAGc
         RGCFRAC = RINc
         RFRS    = SHELRc
         RFIS    = SHELIc
         RFRC    = CORERc
         RFIC    = COREIc
 
!     /*--------------------------------------------------------*/
!     /* Calculate the particle scattering properties for the   */
!     /* given wavelength, particle distribution and indices of */
!     /* refraction of inner and outer material.                */
!     /*--------------------------------------------------------*/

         CALL PFCNPARTICLE(NSCATH, COSPHI, SCTPHS, &
            ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP, an, bn, ntrm,  & ! jcb
            ALAMB,RGmin,RGmax,RGV,SIGMAG,RGCFRAC,RFRS,RFIS,RFRC,RFIC,           & ! jdf
            QEXT,QSCAT,QBS,EXT,SCAT,BSCAT,ASY,                                  & ! jdf
            IPHASEmie)                                                            ! jdf

!     /*--------------------------------------------------------*/
!     /* If IPHASE = 1, then the full phase function was        */
!     /* calculated; now, go calculate its moments.             */
!     /*--------------------------------------------------------*/

!        IF (IPHASE .gt. 0) CALL DISMOM (NSCATA,COSPHI,SCTPHS,RMOMS)

!     /*--------------------------------------------------------*/
!     /* Copy the variables in the common block OUTPUTS to the  */
!     /* variable addresses passed into this routine.           */
!     /*--------------------------------------------------------*/

         QEXTc  = QEXT
         QSCATc = QSCAT
         QBACKc = QBS

         EXTc  = EXT
         SCATc = SCAT
         BACKc = BSCAT

         GSCAc  = ASY
! jcb
!	ntrmj = number of terms in Mie series, jcb
	nmom= 7 ! largest Legendre coefficient to calculate 0:7  (8 total), jcb
	ipolzn=0 ! unpolarized light, jcb
	momdim=7 ! dimension of pmom, pmom(0:7), jcb
! a, b = Mie coefficients
! pmom = output of Legendre coefficients, pmom(0:7)
!	write(6,*)ntrm
!	do ii=1,ntrm
!	write(6,1030)ii,an(ii),bn(ii)
1030	format(i5,4e15.6)
!	enddo

	call lpcoefjcb(ntrm,nmom,ipolzn,momdim,an,bn,pmom)
!	do ii=0,7
!	write(6,1040)ii,pmom(ii,1),pmom(ii,1)/pmom(0,1)
!1040	format(i5,2e15.6)
!	enddo
!     /*--------------------------------------------------------*/
!     /* FORMAT statements.                                     */
!     /*--------------------------------------------------------*/

  107 FORMAT ( ///, 1X, I6, ' IS AN INVALID MEAN RADIUS FLAG')

!     /*--------------------------------------------------------*/
!     /* DONE with this subroutine so exit.                     */
!     /*--------------------------------------------------------*/

  END SUBROUTINE ACKMIEPARTICLE

! /*****************************************************************/
! /* SUBROUTINE PFCNPARTICLE                                       */
! /*****************************************************************/
!
! THIS SUBROUTINE COMPUTES THE PHASE FUNCTION SCTPHS(I) AT NSCATA
! ANGLES BETWEEN 0.0 AND 180.0 DEGREES SPECIFIED BY COSPHI(I) WHICH
! CONTAINS THE ANGLE COSINES. THESE VALUES ARE RETURNED TO FUNCTION
! PHASFN FOR AZIMUTHAL AVERAGING.
! INPUT DATA FOR THIS ROUTINE IS PASSED THROUGH COMMON /SIZDIS/
! AND CONSISTS OF THE FOLLOWING PARAMETERS
! NEWSD  = 1 IF SIZE DIS VALUES HAVE NOT PREVIOUSLY BEEN USED IN
!          THIS ROUTINE,  = 0 OTHERWISE.
! RGV    = GEOMETRIC MEAN RADIUS FOR THE VOLUME DISTRIBUTION OF THE
!          SPECIFIED PARTICLES
! SIGMAG = GEOMETRIC STANDARD DEVIATION
! RFR,I  = REAL AND IMAGINARY INDEX OF REFRACTION OF PARTICLES
! ALAMB  = WAVELENGTH AT WHICH CALCULATIONS ARE TO BE PERFORMED
!
! /*---------------------------------------------------------------*/

      SUBROUTINE PFCNPARTICLE( NSCATH, COSPHI, SCTPHS, &
          ANGLESc,S1R,S1C,S2R,S2C,S11N,S11,S12,S33,S34,SPOL,SP,an,bn,ntrm, & ! jcb
          ALAMB,RGmin,RGmax,RGV,SIGMAG,RGCFRAC,RFRS,RFIS,RFRC,RFIC,        & ! jdf
          QEXT,QSCAT,QBS,EXT,SCAT,BSCAT,ASY,                               & ! jdf
          IPHASEmie)                                                         ! jdf

!     /*--------------------------------------------------------*/
!     /* Set reals to 8 bytes, i.e., double precision.          */
!     /*--------------------------------------------------------*/

      IMPLICIT REAL*8 (A-H, O-Z)

!     /*--------------------------------------------------------*/
!     /* Parameter statements.                                  */
!     /*--------------------------------------------------------*/

	integer*4 MXNANG, MXNWORK, JX,LL,IT,IT2

      PARAMETER(MXNANG=501, MXNWORK=500000)

!     /*--------------------------------------------------------*/
!     /* Dimension statements.                                  */
!     /*--------------------------------------------------------*/

      REAL*8 ANGLESc(*),S1R(*),S1C(*),S2R(*),S2C(*)
      REAL*8 S11N,S11(*),S12(*),S33(*),S34(*),SPOL(*),SP(*)

!     /*--------------------------------------------------------*/
!     /* Define the types of the common block.                  */
!     /*--------------------------------------------------------*/

      INTEGER*4 IPHASEmie,NSCATH

      REAL*8 ALAMB, RGmin, RGmax, RGV, SIGMAG, RGCFRAC, RFRS, &
           RFIS, RFRC, RFIC

      complex*16 an(500),bn(500)
      integer*4 ntrm

!     /*--------------------------------------------------------*/
!     /* Set reals to 8 bytes, i.e., double precision.          */
!     /*--------------------------------------------------------*/

!      IMPLICIT REAL*8 (A-H, O-Z)

!     /*--------------------------------------------------------*/
!     /* Input common block for scattering calculations.        */
!     /*--------------------------------------------------------*/

!jdf  COMMON / PHASE  / IPHASEmie

!jdf  COMMON / INPUTS / ALAMB, RGmin, RGmax, RGV, SIGMAG, &
!jdf                   RGCFRAC, RFRS,  RFIS,  RFRC,  RFIC

!     /*--------------------------------------------------------*/
!     /* Output common block for scattering calculations.       */
!     /*--------------------------------------------------------*/

!jdf  COMMON / OUTPUTS / QEXT, QSCAT, QBS, EXT, SCAT, BSCAT, ASY

!     /*--------------------------------------------------------*/
!     /* Arrays to perform the scattering calculations and to   */
!     /* hold the subsequent results.                           */
!     /*--------------------------------------------------------*/

      REAL*8  THETA(MXNANG), ELTRMX(4,MXNANG,2), PII(3,MXNANG), &
             TAU(3,MXNANG), CSTHT(MXNANG),      SI2THT(MXNANG)

      REAL*8   ROUT, RFRO, RFIO, DQEXT, DQSCAT, CTBRQS, DQBS, &
              RIN,  RFRI, RFII, WNUM
 
      COMPLEX*16  ACAP(MXNWORK)
 
      REAL*8  COSPHI(2*MXNANG-1), SCTPHS(2*MXNANG-1)

      INTEGER J, JJ, NINDEX, NSCATA

!     /*--------------------------------------------------------*/
!     /* Obvious variable initializations.                      */
!     /*--------------------------------------------------------*/

         PIE    = DACOS( -1.0D0 )

!     /*--------------------------------------------------------*/
!     /* Maximum number of scattering angles between 0 and 90   */
!     /* degrees, inclusive.                                    */
!     /*--------------------------------------------------------*/

         IT = MXNANG

!     /*--------------------------------------------------------*/
!     /* Maximum number of scattering angles between 0 and 180  */
!     /* degrees, inclusive.                                    */
!     /*--------------------------------------------------------*/

         IT2 = 2 * IT - 1

!     /*--------------------------------------------------------*/
!     /* Dimension of the work array ACAP.                      */
!     /*--------------------------------------------------------*/

         LL = MXNWORK
 
!     /*--------------------------------------------------------*/
!     /* NSCATA is the actual user-requested number of          */
!     /* scattering angles between 0 and 90 degrees, inclusive. */
!     /*--------------------------------------------------------*/

         NSCATA = 2 * NSCATH - 1

!     /*--------------------------------------------------------*/
!     /* If the user did not request a phase function, then we  */
!     /* can set NSCATA and NSCATH to 0.                        */
!     /*--------------------------------------------------------*/

         IF ( IPHASEmie  .le.  0 )  then
              NSCATH  =  0
              NSCATA  =  0
         ENDIF

!     /*--------------------------------------------------------*/
!     /* Check to make sure that the user-requested number of   */
!     /* scattering angles does not excede the current maximum  */
!     /* limit.                                                 */
!     /*--------------------------------------------------------*/

         IF ( NSCATA .gt. IT2  .OR.  NSCATH .gt. IT)  then
              WRITE( 6,105 )  NSCATA, NSCATH, IT2, IT
              call errmsg( 'PFCNPARTICLE: 11', .true.)
         ENDIF

!     /*--------------------------------------------------------*/
!     /* Subroutine SCATANGLES was added by EEC[0495] in order  */
!     /* to facilitate changing the scattering angle locations  */
!     /* output by the Ackerman and Toon Mie code.              */
!     /*--------------------------------------------------------*/

!         CALL SCATANGLES(NSCATH,THETA,COSPHI)

!     /*--------------------------------------------------------*/
!     /* COMPUTE SCATTERING PROPERTIES OF THE PARTICLE.         */
!     /*--------------------------------------------------------*/

!     /*--------------------------------------------------------*/
!     /* DMIESS expects a wavenumber.                           */
!     /*--------------------------------------------------------*/

      WNUM = (2.D0*PIE) / ALAMB

!     /*--------------------------------------------------------*/
!     /* DMIESS assignments of the indices of refraction of the */
!     /* core and shell materials.                              */
!     /*--------------------------------------------------------*/

      RFRO = RFRS
      RFIO = RFIS
      RFRI = RFRC
      RFII = RFIC

!     /*--------------------------------------------------------*/
!     /* DMIESS core and shell radii.                           */
!     /*--------------------------------------------------------*/

       ROUT = RGV
       RIN  = RGCFRAC * ROUT

!     /*--------------------------------------------------------*/
!     /* Scattering angles are symmetric about 90 degrees.      */
!     /*--------------------------------------------------------*/

      IF ( NSCATH  .eq.  0.0 )  THEN
           JX  =  1
      ELSE
           JX   = NSCATH
      ENDIF

!     /*--------------------------------------------------------*/
!     /* Compute the scattering properties for this particle.   */
!     /*--------------------------------------------------------*/

      CALL DMIESS(  ROUT,    RFRO,    RFIO,    THETA,   JX,  &
                   DQEXT,   DQSCAT,  CTBRQS,  ELTRMX,  PII, &
                   TAU,     CSTHT,   SI2THT,  ACAP,    DQBS,  IT, &
                   LL,      RIN,     RFRI,    RFII,    WNUM, an, bn, ntrm   )  ! jcb
 
!     /*--------------------------------------------------------*/
!     /* Compute total cross-sectional area of the particle.    */
!     /*--------------------------------------------------------*/

      X = PIE * RGV * RGV

!     /*--------------------------------------------------------*/
!     /* Assign the final extinction efficiency.                */
!     /*--------------------------------------------------------*/

      QEXT = DQEXT

!     /*--------------------------------------------------------*/
!     /* Compute total extinction cross-section due to particle.*/
!     /*--------------------------------------------------------*/

      EXT = DQEXT * X

!     /*--------------------------------------------------------*/
!     /* Assign the final scattering efficiency.                */
!     /*--------------------------------------------------------*/

      QSCAT = DQSCAT

!     /*--------------------------------------------------------*/
!     /* Compute total scattering cross-section due to particle.*/
!     /*--------------------------------------------------------*/

      SCAT = DQSCAT * X

!     /*--------------------------------------------------------*/
!     /* Assign the final backscatter efficiency.               */
!     /*--------------------------------------------------------*/

      QBS = DQBS

!     /*--------------------------------------------------------*/
!     /* Compute backscatter due to particle.                   */
!     /*--------------------------------------------------------*/

      BSCAT = DQBS * X

!     /*--------------------------------------------------------*/
!     /* Compute asymmetry parameter due to particle.           */
!     /*--------------------------------------------------------*/

      ASY = (CTBRQS * X) / SCAT

!     /*--------------------------------------------------------*/
!     /* If IPHASE is 1, compute the phase function.            */
!     /* S33 and S34 matrix elements are normalized by S11. S11 */
!     /* is normalized to 1.0 in the forward direction.  The    */
!     /* variable SPOL is the degree of polarization for        */
!     /* incident unpolarized light.                            */
!     /*--------------------------------------------------------*/

      IF ( IPHASEmie  .gt.  0 )  THEN

        DO 355 J=1,NSCATA

          IF (J .LE. JX)  THEN
             JJ = J
             NINDEX = 1
          ELSE
             JJ = NSCATA - J + 1
             NINDEX = 2
          ENDIF

          ANGLESc(J) = COSPHI(J)

          S1R(J) = ELTRMX(1,JJ,NINDEX)
          S1C(J) = ELTRMX(2,JJ,NINDEX)
          S2R(J) = ELTRMX(3,JJ,NINDEX)
          S2C(J) = ELTRMX(4,JJ,NINDEX)

          S11(J) = 0.5D0*(S1R(J)**2+S1C(J)**2+S2R(J)**2+S2C(J)**2)
          S12(J) = 0.5D0*(S2R(J)**2+S2C(J)**2-S1R(J)**2-S1C(J)**2)
          S33(J) = S2R(J)*S1R(J) + S2C(J)*S1C(J)
          S34(J) = S2R(J)*S1C(J) - S1R(J)*S2C(J)

          SPOL(J) = -S12(J) / S11(J)

          SP(J) = (4.D0*PIE)*(S11(J) / (SCAT*WNUM**2))

  355   CONTINUE

!        /*-----------------------------------------------------*/
!        /* DONE with the phase function so exit the IF.        */
!        /*-----------------------------------------------------*/

      ENDIF

!     /*--------------------------------------------------------*/
!     /* END of the computations so exit the routine.           */
!     /*--------------------------------------------------------*/


      RETURN

!     /*--------------------------------------------------------*/
!     /* FORMAT statements.                                     */
!     /*--------------------------------------------------------*/

  100 FORMAT ( 7X, I3 )
  105 FORMAT ( ///, 1X,'NUMBER OF ANGLES SPECIFIED =',2I6, / &
                  10X,'EXCEEDS ARRAY DIMENSIONS =',2I6 )

  120 FORMAT (/10X,'INTEGRATED VOLUME',           T40,'=',1PE14.5,/ &
              15X,'PERCENT VOLUME IN CORE',      T40,'=',0PF10.5,/ &
              15X,'PERCENT VOLUME IN SHELL',     T40,'=',0PF10.5,/ &
              10X,'INTEGRATED SURFACE AREA',     T40,'=',1PE14.5,/ &
              10X,'INTEGRATED NUMBER DENSITY',   T40,'=',1PE14.5 )
  125 FORMAT ( 10X,'CORE RADIUS COMPUTED FROM :', /, 20X, 9A8, /  )

  150 FORMAT ( ///,1X,'* * * WARNING * * *', / &
              10X,'PHASE FUNCTION CALCULATION MAY NOT HAVE CONVERGED'/ &
              10X,'VALUES OF S1 AT NSDI-1 AND NSDI ARE :', 2E14.6, /  &
              10X,'VALUE OF X AT NSDI =', E14.6 )
 
!     /*--------------------------------------------------------*/
!     /* DONE with this subroutine so exit.                     */
!     /*--------------------------------------------------------*/

      END SUBROUTINE PFCNPARTICLE

! /*****************************************************************/
! /*****************************************************************/
	subroutine  lpcoefjcb ( ntrm, nmom, ipolzn, momdim,a, b, pmom )
!
!         calculate legendre polynomial expansion coefficients (also
!         called moments) for phase quantities ( ref. 5 formulation )
!
!     input:  ntrm                    number terms in mie series
!             nmom, ipolzn, momdim    'miev0' arguments
!             a, b                    mie series coefficients
!
!     output: pmom                   legendre moments ('miev0' argument)
!
!     *** notes ***
!
!         (1)  eqs. 2-5 are in error in dave, appl. opt. 9,
!         1888 (1970).  eq. 2 refers to m1, not m2;  eq. 3 refers to
!         m2, not m1.  in eqs. 4 and 5, the subscripts on the second
!         term in square brackets should be interchanged.
!
!         (2)  the general-case logic in this subroutine works correctly
!         in the two-term mie series case, but subroutine  'lpco2t'
!         is called instead, for speed.
!
!         (3)  subroutine  'lpco1t', to do the one-term case, is never
!         called within the context of 'miev0', but is included for
!         complete generality.
!
!         (4)  some improvement in speed is obtainable by combining the
!         310- and 410-loops, if moments for both the third and fourth
!         phase quantities are desired, because the third phase quantity
!         is the real part of a complex series, while the fourth phase
!         quantity is the imaginary part of that very same series.  but
!         most users are not interested in the fourth phase quantity,
!         which is related to circular polarization, so the present
!         scheme is usually more efficient.
!
      implicit none
      integer  ipolzn, momdim, nmom, ntrm
      real*8    pmom( 0:momdim,1 )  ! the ",1" dimension is for historical reasons
      complex*16  a(500), b(500)
!
!           ** specification of local variables
!
!      am(m)       numerical coefficients  a-sub-m-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bi(i)       numerical coefficients  b-sub-i-super-l
!                     in dave, eqs. 1-15, as simplified in ref. 5.
!
!      bidel(i)    1/2 bi(i) times factor capital-del in dave
!
!      cm,dm()     arrays c and d in dave, eqs. 16-17 (mueller form),
!                     calculated using recurrence derived in ref. 5
!
!      cs,ds()     arrays c and d in ref. 4, eqs. a5-a6 (sekera form),
!                     calculated using recurrence derived in ref. 5
!
!      c,d()       either -cm,dm- or -cs,ds-, depending on -ipolzn-
!
!      evenl       true for even-numbered moments;  false otherwise
!
!      idel        1 + little-del  in dave
!
!      maxtrm      max. no. of terms in mie series
!
!      maxmom      max. no. of non-zero moments
!
!      nummom      number of non-zero moments
!
!      recip(k)    1 / k
!
      integer maxtrm,maxmom,mxmom2,maxrcp
      parameter  ( maxtrm = 1102, maxmom = 2*maxtrm, mxmom2 = maxmom/2, maxrcp = 4*maxtrm + 2 )
      real*8      am( 0:maxtrm ), bi( 0:mxmom2 ), bidel( 0:mxmom2 ), recip( maxrcp )
      complex*16 cm( maxtrm ), dm( maxtrm ), cs( maxtrm ), ds( maxtrm )
      integer k,j,l,nummom,ld2,idel,m,i,mmax,imax
      real*8 sum
      logical    pass1, evenl
      save  pass1, recip
      data  pass1 / .true. /
!
!
      if ( pass1 )  then
!
         do  1  k = 1, maxrcp
            recip( k ) = 1.0 / k
    1    continue
         pass1 = .false.
!
      end if
!
         do  l = 0, nmom
            pmom( l, 1 ) = 0.0
	 enddo
! these will never be called
!      if ( ntrm.eq.1 )  then
!         call  lpco1t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!         return
!      else if ( ntrm.eq.2 )  then
!         call  lpco2t ( nmom, ipolzn, momdim, calcmo, a, b, pmom )
!         return
!      end if
!
      if ( ntrm+2 .gt. maxtrm ) &
          write(6,1010)
1010    format( ' lpcoef--parameter maxtrm too small' )
!
!                                     ** calculate mueller c, d arrays
      cm( ntrm+2 ) = ( 0., 0. )
      dm( ntrm+2 ) = ( 0., 0. )
      cm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * b( ntrm )
      dm( ntrm+1 ) = ( 1. - recip( ntrm+1 ) ) * a( ntrm )
      cm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * a( ntrm ) &
                  + ( 1. - recip(ntrm) ) * b( ntrm-1 )
      dm( ntrm ) = ( recip(ntrm) + recip(ntrm+1) ) * b( ntrm ) &
                  + ( 1. - recip(ntrm) ) * a( ntrm-1 )
!
      do  10  k = ntrm-1, 2, -1
         cm( k ) = cm( k+2 ) - ( 1. + recip(k+1) ) * b( k+1 ) &
                            + ( recip(k) + recip(k+1) ) * a( k ) &
                            + ( 1. - recip(k) ) * b( k-1 )
         dm( k ) = dm( k+2 ) - ( 1. + recip(k+1) ) * a( k+1 ) &
                            + ( recip(k) + recip(k+1) ) * b( k ) &
                            + ( 1. - recip(k) ) * a( k-1 )
   10 continue
      cm( 1 ) = cm( 3 ) + 1.5 * ( a( 1 ) - b( 2 ) )
      dm( 1 ) = dm( 3 ) + 1.5 * ( b( 1 ) - a( 2 ) )
!
      if ( ipolzn.ge.0 )  then
!
         do  20  k = 1, ntrm + 2
            cm( k ) = ( 2*k - 1 ) * cm( k )
            dm( k ) = ( 2*k - 1 ) * dm( k )
   20    continue
!
      else
!                                    ** compute sekera c and d arrays
         cs( ntrm+2 ) = ( 0., 0. )
         ds( ntrm+2 ) = ( 0., 0. )
         cs( ntrm+1 ) = ( 0., 0. )
         ds( ntrm+1 ) = ( 0., 0. )
!
         do  30  k = ntrm, 1, -1
            cs( k ) = cs( k+2 ) + ( 2*k + 1 ) * ( cm( k+1 ) - b( k ) )
            ds( k ) = ds( k+2 ) + ( 2*k + 1 ) * ( dm( k+1 ) - a( k ) )
   30    continue
!
         do  40  k = 1, ntrm + 2
            cm( k ) = ( 2*k - 1 ) * cs( k )
            dm( k ) = ( 2*k - 1 ) * ds( k )
   40    continue
!
      end if
!
!
      if( ipolzn.lt.0 )  nummom = min0( nmom, 2*ntrm - 2 )
      if( ipolzn.ge.0 )  nummom = min0( nmom, 2*ntrm )
      if ( nummom .gt. maxmom ) &
          write(6,1020)
1020	format( ' lpcoef--parameter maxtrm too small')
!
!                               ** loop over moments
      do  500  l = 0, nummom
         ld2 = l / 2
         evenl = mod( l,2 ) .eq. 0
!                                    ** calculate numerical coefficients
!                                    ** a-sub-m and b-sub-i in dave
!                                    ** double-sums for moments
         if( l.eq.0 )  then
!
            idel = 1
            do  60  m = 0, ntrm
               am( m ) = 2.0 * recip( 2*m + 1 )
   60       continue
            bi( 0 ) = 1.0
!
         else if( evenl )  then
!
            idel = 1
            do  70  m = ld2, ntrm
               am( m ) = ( 1. + recip( 2*m-l+1 ) ) * am( m )
   70       continue
            do  75  i = 0, ld2-1
               bi( i ) = ( 1. - recip( l-2*i ) ) * bi( i )
   75       continue
            bi( ld2 ) = ( 2. - recip( l ) ) * bi( ld2-1 )
!
         else
!
            idel = 2
            do  80  m = ld2, ntrm
               am( m ) = ( 1. - recip( 2*m+l+2 ) ) * am( m )
   80       continue
            do  85  i = 0, ld2
               bi( i ) = ( 1. - recip( l+2*i+1 ) ) * bi( i )
   85       continue
!
         end if
!                                     ** establish upper limits for sums
!                                     ** and incorporate factor capital-
!                                     ** del into b-sub-i
         mmax = ntrm - idel
         if( ipolzn.ge.0 )  mmax = mmax + 1
         imax = min0( ld2, mmax - ld2 )
         if( imax.lt.0 )  go to 600
         do  90  i = 0, imax
            bidel( i ) = bi( i )
   90    continue
         if( evenl )  bidel( 0 ) = 0.5 * bidel( 0 )
!
!                                    ** perform double sums just for
!                                    ** phase quantities desired by user
         if( ipolzn.eq.0 )  then
!
            do  110  i = 0, imax
!                                           ** vectorizable loop (cray)
               sum = 0.0
               do  100  m = ld2, mmax - i
                  sum = sum + am( m ) * &
                           ( dble( cm(m-i+1) * dconjg( cm(m+i+idel) ) ) &
                           + dble( dm(m-i+1) * dconjg( dm(m+i+idel) ) ) )
  100          continue
               pmom( l,1 ) = pmom( l,1 ) + bidel( i ) * sum
  110       continue
            pmom( l,1 ) = 0.5 * pmom( l,1 )
            go to 500
!
         end if
!
  500 continue
!
!
  600 return
      end subroutine lpcoefjcb
!
	end module module_optical_averaging
