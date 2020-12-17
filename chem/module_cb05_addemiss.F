! **************************************************************************************
!   This computer software was developed by Dr. Yang Zhang and her research group      *
!   at North Carolina State University (NCSU) with support  from the NSF Career Award  *
!   No. Atm-0348819, and the Memorandum of Understanding between the                   *
!   U.S. Environmental Protection Agency (EPA) and the U.S. Department of              *
!   Commerce's National Oceanic and Atmospheric Administration (NOAA)		       *
!   and under agreement number DW13921548, and the U.S. EPA/Office of                  *
!   Air Quality Planning & Standards via RTI International contract #4-321-0210288.    *
!                                                                                      *
!   NEITHER ANY COSPONSORS, NCSU, NOR ANY PERSON ACTING ON BEHALF                      *
!   OF ANY OF THEM MAKES ANY WARRANTY OR REPRESENTATION                                *
!   WHATSOEVER, EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR                       *
!   THE USE OF THIS SOFTWARE.  THIS SOFTWARE OR PART OF IT MAY BE                      *
!   COPYRIGHTED AND IS PERMITTED BY ORIGINAL CODE DEVELOPERS FOR                       *
!   NONPROFIT USE AND SUBJECTED TO RESTRICTIONS                                        *
!                                                                                      *
!  Contact information: 							       *
!  Dr. Yang Zhang		                                                       * 
!  Principal Investigator						               *
!  Department of Marine, Earth, and Atmospheric Sciences 			       *
!  North Carolina State University						       *
!  Campus Box 8208 								       *
!  Room 5151, Jordan Hall, 2800 Faucette Drive 					       *
!  Raleigh, NC 27695-8208, USA 							       *
!  Tel:  (919) 515-9688 (Office) 						       *
!  Fax:  (919) 515-7802								       *
!  E-Mail:  yang_zhang@ncsu.edu 						       *
!***************************************************************************************

   MODULE module_cb05_addemiss

!***************************************************************************************
!  FUNCTION: ADD EMISSIONS FOR CB05 GAS SPECIES                                        *
!  PRECONDITION REQUIRED:  use for CB05 gas-phase mechanism      	               *
!  RETURN VALUES:								       *			         
!  KEY SUBROUTINES AND FUNCTIONS CALLED:  None                                         *
!  REVISION HISTORY:                                                                   *
!        This code was based on module_cbmz_addemiss.F, developed by PNNL, 2005        * 
!        Revised by J.P. HUANG AND Y. ZHANG, Air Quality Forecasting Laboratory,       *
!               North Carolina State University, Raleigh, NC 27695                     *
!               for incorporation of CB05 into WRF/Chem under several projects         *
!               led by Dr. Yang Zhang (contact: (919) 515-9688, yang_zhang@ncsu.edu)   *
!               March-Oct., 2006                                                       *
!        Revised by Y. ZHANG, NCSU, clean up, May 6, 2007                              *
!        Revised by Ying Pan and Yang Zhang, NCSU, Nov. 2007-Nov. 2008                 *
!               to couple MADRID with CB05 gas-phase mechanism                         *
!        Revised by Yang Zhang, Xiao-Ming Hu, and Ying Pan, NCSU, Sept.-Nov., 2008     *
!         	Code cleanup for NOAA WRF/Chem repository checkin     	               *               
!        Revised by Ying Pan and Yang Zhang, NCSU, Sep. 2009                           *
!               to transfer code to WRF/Chem v3.1.1                                    *
!        Revised by Kai Wang, NCSU, Oct 2014 to transfer to WRF/Chem v3.6.1            *
!                                                                                      * 
!  REFERENCES:                                                                         *
! 										       *
!***************************************************************************************

!WRF:MODEL_LAYER:CHEMICS
!
CONTAINS
!
! currently this only adds in the emissions...
! this may be done differently for different chemical mechanisms
! in the future. aerosols are already added somewhere else....
!
   subroutine cb05_addemiss_anthro( id, dtstep, dz8w, config_flags,          &
                rho_phy, chem, emis_ant,                                     &
                ids,ide, jds,jde, kds, kde,                                  &
                ims,ime, jms,jme, kms, kme,                                  &
                its,ite, jts,jte, kts, kte )

!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2

   IMPLICIT NONE

! .. Parameters ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   REAL,      INTENT(IN   ) ::                                             &
                             dtstep

! trace species mixing ratios (gases=ppm) 
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem
!
! emissions arrays
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                          &
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme,num_emis_ant),&
         INTENT(IN ) ::                                                    &
                               emis_ant

! layer thickness (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   dz8w, rho_phy

! local variables
    integer i,j,k
    real, parameter :: efact1 = 1.0/60.0
    real :: conv
   double precision :: chem_sum(num_chem)

!--- deposition and emissions stuff
! 
! .. Intrinsic Functions ..

         call wrf_debug(15,'cb05_addemiss_anthro')
!       
! add emissions
!
      do 100 j=jts,jte  
      do 100 i=its,ite  

      DO k=kts,min(config_flags%kemit,kte)
!v1 units:        conv = dtstep/(dz8w(i,k,j)*60.)
!v2 units:
        conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (i <= CHEM_DBG_I .and. i >= CHEM_DBG_I) .and. &
           (j <= CHEM_DBG_J .and. j >= CHEM_DBG_J) .and. &
           (k <= CHEM_DBG_K .and. k >= CHEM_DBG_K)  ) then
          print*
          print*,"Converted emissions for CB05:"
       end if
#endif
        chem(i,k,j,p_no2)  = chem(i,k,j,p_no2)                        &
                           + emis_ant(i,k,j,p_e_no2)*conv
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                        &
                           + emis_ant(i,k,j,p_e_xyl)*conv
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                        &
                           + emis_ant(i,k,j,p_e_tol)*conv
        chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                        &
                           + emis_ant(i,k,j,p_e_so2)*conv
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                         &
                           + emis_ant(i,k,j,p_e_no)*conv
        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                        &
                           + emis_ant(i,k,j,p_e_nh3)*conv
        chem(i,k,j,p_hcl)  = chem(i,k,j,p_hcl)                        &
                           + emis_ant(i,k,j,p_e_hcl)*conv
        chem(i,k,j,p_co)   = chem(i,k,j,p_co)                         &
                           + emis_ant(i,k,j,p_e_co)*conv
        chem(i,k,j,p_aldx) = chem(i,k,j,p_aldx)                       &
                           + emis_ant(i,k,j,p_e_aldx)*conv
! when biogenic emissions are off, terpene emissions are read from offline
        if (config_flags%bio_emiss_opt == 0) then
            chem(i,k,j,p_terp) = chem(i,k,j,p_terp)                       &
                           + emis_ant(i,k,j,p_e_terp)*conv
        end if
! when emissions inventory is based on RADM2 speciation, which requires emiss_opt = 14
! with emiss_inpt_opt = 102
        if ( (config_flags%emiss_opt == 14) ) then
            chem(i,k,j,p_par) = chem(i,k,j,p_par)             &
                + conv*                                       &
                  ( 2.9*emis_ant(i,k,j,p_e_hc3)       &
                  + 4.8*emis_ant(i,k,j,p_e_hc5) + 7.9*emis_ant(i,k,j,p_e_hc8)       &
                  + 0.9*emis_ant(i,k,j,p_e_ket) )
            chem(i,k,j,p_aacd) = chem(i,k,j,p_aacd)                       &
                           + emis_ant(i,k,j,p_e_ora2)*conv
            chem(i,k,j,p_ole)  = chem(i,k,j,p_ole)                        &
                           + emis_ant(i,k,j,p_e_olt)*conv
            chem(i,k,j,p_iole) = chem(i,k,j,p_iole)                       &
                           + emis_ant(i,k,j,p_e_oli)*conv
            chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                        &
                           + emis_ant(i,k,j,p_e_ol2)*conv
            chem(i,k,j,p_form) = chem(i,k,j,p_form)                       &
                           + emis_ant(i,k,j,p_e_hcho)*conv
            chem(i,k,j,p_etha) = chem(i,k,j,p_etha)                       &
                           + emis_ant(i,k,j,p_e_eth)*conv
            chem(i,k,j,p_cres) = chem(i,k,j,p_cres)                       &
                           + emis_ant(i,k,j,p_e_csl)*conv
            chem(i,k,j,p_meoh) = chem(i,k,j,p_meoh)                       &
                           + emis_ant(i,k,j,p_e_ch3oh)*conv
            chem(i,k,j,p_etoh) = chem(i,k,j,p_etoh)                       &
                           + emis_ant(i,k,j,p_e_c2h5oh)*conv
            chem(i,k,j,p_ald2) = chem(i,k,j,p_ald2)                       &
                           + emis_ant(i,k,j,p_e_ald)*conv
! when biogenic emissions are off, isoprene emissions are read from offline
            if (config_flags%bio_emiss_opt == 0) then
             chem(i,k,j,p_isop) = chem(i,k,j,p_isop)                       &
                           + emis_ant(i,k,j,p_e_iso)*conv
            end if
! when emissions inventory is based on CBM speciation, which requires emiss_opt = 15
! with emiss_inpt_opt = 101
        else
            chem(i,k,j,p_par) = chem(i,k,j,p_par)             &
                + conv*emis_ant(i,k,j,p_e_par)
            chem(i,k,j,p_ole)  = chem(i,k,j,p_ole)                        &
                           + emis_ant(i,k,j,p_e_ole)*conv
            chem(i,k,j,p_iole) = chem(i,k,j,p_iole)                       &
                           + emis_ant(i,k,j,p_e_iole)*conv
            chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                        &
                           + emis_ant(i,k,j,p_e_eth)*conv
            chem(i,k,j,p_form) = chem(i,k,j,p_form)                       &
                           + emis_ant(i,k,j,p_e_form)*conv
            chem(i,k,j,p_etha) = chem(i,k,j,p_etha)                       &
                           + emis_ant(i,k,j,p_e_etha)*conv
            chem(i,k,j,p_cres) = chem(i,k,j,p_cres)                       &
                           + emis_ant(i,k,j,p_e_cres)*conv                &
                           + emis_ant(i,k,j,p_e_phen)*conv 
            chem(i,k,j,p_meoh) = chem(i,k,j,p_meoh)                       &
                           + emis_ant(i,k,j,p_e_meoh)*conv
            chem(i,k,j,p_etoh) = chem(i,k,j,p_etoh)                       &
                           + emis_ant(i,k,j,p_e_etoh)*conv
            chem(i,k,j,p_ald2) = chem(i,k,j,p_ald2)                       &
                           + emis_ant(i,k,j,p_e_ald2)*conv
            chem(i,k,j,p_meo2) = chem(i,k,j,p_meo2)                       &
                           + emis_ant(i,k,j,p_e_meo2)*conv
            chem(i,k,j,p_sulf) = chem(i,k,j,p_sulf)                       &
                           + emis_ant(i,k,j,p_e_psulf)*conv
            chem(i,k,j,p_mgly) = chem(i,k,j,p_mgly)                       &
                           + emis_ant(i,k,j,p_e_mgly)*conv
            chem(i,k,j,p_facd) = chem(i,k,j,p_facd)                       &
                           + emis_ant(i,k,j,p_e_hcooh)*conv
            chem(i,k,j,p_aacd) = chem(i,k,j,p_aacd)                       &
                           + emis_ant(i,k,j,p_e_ccooh)*conv
            chem(i,k,j,p_ispd) = chem(i,k,j,p_ispd)                       &
                           + emis_ant(i,k,j,p_e_iprod)*conv


! when biogenic emissions are off, isoprene emissions are read from offline
            if (config_flags%bio_emiss_opt == 0) then
             chem(i,k,j,p_isop) = chem(i,k,j,p_isop)                       &
                           + emis_ant(i,k,j,p_e_isop)*conv
            end if
        end if

      END DO                                                          
 100  continue

    END subroutine cb05_addemiss_anthro




!----------------------------------------------------------------------
  subroutine cb05_addemiss_bio( id, dtstep, dz8w, config_flags,       &
        rho_phy, chem, e_bio, ne_area, e_iso,                         &
        ids,ide, jds,jde, kds,kde,                                    &
        ims,ime, jms,jme, kms,kme,                                    &
        its,ite, jts,jte, kts,kte                                     )

  USE module_configure
  USE module_state_description
  USE module_data_radm2
  USE module_aerosols_sorgam

  IMPLICIT NONE

! subr arguments
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id, ne_area,                             &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL,      INTENT(IN   ) ::    dtstep

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::        chem

   REAL, DIMENSION( ims:ime, jms:jme,ne_area ),                            &
         INTENT(IN ) ::           e_bio
         
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme ),            &
         INTENT(IN ) ::           e_iso

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::        dz8w, rho_phy            


! local variables
   integer i,j,k,n
   real, parameter :: efact1 = 1.0/60.0
   double precision :: chem_sum(num_chem)


!
! apply gunther online biogenic gas emissions when bio_emiss_opt == GUNTHER1
! only incoporated isoprene and terpene at the current stage for CB05
!
   if (config_flags%bio_emiss_opt == GUNTHER1) then

      do j=jts,jte  
      do i=its,ite  
        chem(i,kts,j,p_isop) = chem(i,kts,j,p_isop)    &
                          + e_bio(i,j,liso)/(dz8w(i,kts,j)*60.)*dtstep
! tpan is used to be the place holder of terpene in Gunther scheme
        chem(i,kts,j,p_terp) = chem(i,kts,j,p_terp)    &
                          + e_bio(i,j,ltpan)/(dz8w(i,kts,j)*60.)*dtstep
      end do
      end do

   end if


   END subroutine cb05_addemiss_bio

!
END MODULE module_cb05_addemiss
