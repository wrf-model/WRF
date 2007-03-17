!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! CBMZ module: see module_cbmz.F for information and terms of use
!**********************************************************************************  

MODULE module_cbmz_addemiss
!WRF:MODEL_LAYER:CHEMICS



   integer, parameter :: cbmz_addemiss_masscheck = -1
                       ! only do emissions masscheck calcs when this is positive



CONTAINS



!----------------------------------------------------------------------
   subroutine cbmz_addemiss_anthro( id, dtstep, dz8w, config_flags,       &
               rho_phy, chem,                                             &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
               e_no2,e_ch3oh,e_c2h5oh,                                    &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!
! adds emissions for cbmz trace gas species
! (i.e., emissions tendencies over time dtstep are applied 
! to the trace gas concentrations)
!

  USE module_configure
  USE module_state_description
  USE module_data_radm2

  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   ) ::    dtstep

! trace species mixing ratios (gases=ppm)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem
!
! emissions arrays (v.1: ppm m/min; v.2: mole km^-2 hr^-1)
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                          &
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme ),            &
         INTENT(IN ) ::                                                    &
          e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,       &
          e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,               &
          e_no2,e_ch3oh,e_c2h5oh

! layer thickness (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   dz8w, rho_phy

! local variables
   integer :: i,j,k
   real, parameter :: efact1 = 1.0/60.0
   real :: conv
   double precision :: chem_sum(num_chem)


! do mass check initial calc
      if (cbmz_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 1, 'cbmz_addemiss',                       &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               21,                                                         &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,  &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,          &
               e_no2,e_ch3oh,e_c2h5oh                                      )


!       
! add emissions
!
      do 100 j=jts,jte  
      do 100 i=its,ite  

      DO k=kts,min(config_flags%kemit,kte-1)
!v1 units:        conv = dtstep/(dz8w(i,k,j)*60.)
!v2 units:
        conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (i <= CHEM_DBG_I .and. i >= CHEM_DBG_I) .and. &
           (j <= CHEM_DBG_J .and. j >= CHEM_DBG_J) .and. &
           (k <= CHEM_DBG_K .and. k >= CHEM_DBG_K)  ) then
          print*
          print*,"Converted emissions for CBMZ:"
          print*,"e_csl=",e_csl(i,k,j)*conv
          print*,"e_so2=",e_so2(i,k,j)*conv
          print*,"e_no=",e_no(i,k,j)*conv
          print*,"e_ald=",e_ald(i,k,j)*conv
          print*,"e_hcho=",e_hcho(i,k,j)*conv
          print*,"e_ora2=",e_ora2(i,k,j)*conv
          print*,"e_nh3=",e_nh3(i,k,j)*conv
          print*,"e_hc3=",e_hc3(i,k,j)*conv
          print*,"e_hc5=",e_hc5(i,k,j)*conv
          print*,"e_hc8=",e_hc8(i,k,j)*conv
          print*,"e_eth=",e_eth(i,k,j)*conv
          print*,"e_co=",e_co(i,k,j)*conv
          print*,"e_ol2=",e_ol2(i,k,j)*conv
          print*,"e_olt=",e_olt(i,k,j)*conv
          print*,"e_oli=",e_oli(i,k,j)*conv
          print*,"e_tol=",e_tol(i,k,j)*conv
          print*,"e_xyl=",e_xyl(i,k,j)*conv
          print*,"e_ket=",e_ket(i,k,j)*conv
       end if
#endif

        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
                         +e_csl(i,k,j)*conv 
        chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                         &
                         +e_so2(i,k,j)*conv
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
                         +e_no(i,k,j)*conv
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +e_ald(i,k,j)*conv
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
                         +e_hcho(i,k,j)*conv
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
                         +e_ora2(i,k,j)*conv 
        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                         &
                         +e_nh3(i,k,j)*conv
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +e_hc3(i,k,j)*conv
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +e_hc5(i,k,j)*conv
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +e_hc8(i,k,j)*conv
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +e_eth(i,k,j)*conv
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
                         +e_co(i,k,j)*conv
        chem(i,k,j,p_ol2)  = chem(i,k,j,p_ol2)                         &
                         +e_ol2(i,k,j)*conv
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +e_olt(i,k,j)*conv
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +e_oli(i,k,j)*conv
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +e_tol(i,k,j)*conv
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &
                         +e_xyl(i,k,j)*conv       
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &
                         +e_ket(i,k,j)*conv       

! when emissions input file is "radm2sorg" variety, calc par emissions as a 
!   combination of the anthropogenic emissions for radm2 primary voc species
        if ( (config_flags%emiss_inpt_opt == EMISS_INPT_DEFAULT) .or.   &
             (config_flags%emiss_inpt_opt == EMISS_INPT_PNNL_RS) ) then
            chem(i,k,j,p_par) = chem(i,k,j,p_par)             &
                + conv*                                       &
                  ( 0.4*e_ald(i,k,j) + 2.9*e_hc3(i,k,j)       &
                  + 4.8*e_hc5(i,k,j) + 7.9*e_hc8(i,k,j)       &
                  + 0.9*e_ket(i,k,j) + 2.8*e_oli(i,k,j)       &
                  + 1.8*e_olt(i,k,j) + 1.0*e_ora2(i,k,j) )

! when emissions input file is "cbmzmosaic" variety, 
!   the par emissions are read into e_hc5
!   and there are emissions for other species
        else
            chem(i,k,j,p_par) = chem(i,k,j,p_par)             &
                + conv*e_hc5(i,k,j)

            chem(i,k,j,p_no2) = chem(i,k,j,p_no2)             &
                + conv*e_no2(i,k,j)
            chem(i,k,j,p_ch3oh)  = chem(i,k,j,p_ch3oh)        &
                + conv*e_ch3oh(i,k,j)
            chem(i,k,j,p_c2h5oh) = chem(i,k,j,p_c2h5oh)       &
                + conv*e_c2h5oh(i,k,j)
        end if

      END DO                                                          
 100  continue


! do mass check final calc
      if (cbmz_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 2, 'cbmz_addemiss',                       &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               21,                                                         &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,  &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,          &
               e_no2,e_ch3oh,e_c2h5oh                                      )


   END subroutine cbmz_addemiss_anthro



!----------------------------------------------------------------------
  subroutine cbmz_addemiss_bio( id, dtstep, dz8w, config_flags,       &
        rho_phy, chem, e_bio, ne_area, e_iso,                         &
        numgas,                                                       &
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
                                  numgas,                                  &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL,      INTENT(IN   ) ::    dtstep

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::        chem

   REAL, DIMENSION( ims:ime, jms:jme,ne_area ),                            &
         INTENT(IN ) ::           e_bio
         
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                          &
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
!
   if (config_flags%bio_emiss_opt == GUNTHER1) then

      if (cbmz_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 1, 'cbmz_addemiss_bioaa',                 &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               kms, 13,                                                    &
               e_bio(ims,jms,p_ald-1),  e_bio(ims,jms,p_hc3-1),            &
               e_bio(ims,jms,p_hc5-1),  e_bio(ims,jms,p_hc8-1),            &
               e_bio(ims,jms,p_hcho-1), e_bio(ims,jms,p_iso-1),            &
               e_bio(ims,jms,p_ket-1),  e_bio(ims,jms,p_no-1),             &
               e_bio(ims,jms,p_oli-1),  e_bio(ims,jms,p_olt-1),            &
               e_bio(ims,jms,p_ora1-1), e_bio(ims,jms,p_ora2-1),           &
               e_bio(ims,jms,p_xyl-1),                                     &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1),            &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1),            &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1),            &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1)             )

      DO n = 1, ne_area-2 !Assumes CBMZ and RADM2 species locations match up to p_iso
      do 100 j=jts,jte  
      do 100 i=its,ite  
        chem(i,kts,j,n+1) = chem(i,kts,j,n+1)    &
                          + e_bio(i,j,n)/(dz8w(i,kts,j)*60.)*dtstep
 100  continue
      enddo

! calc par emissions as a combination of the biogenic emissions
! for radm2 primary voc species
      do j = jts, jte
      do i = its, ite
         chem(i,kts,j,p_par)  =  chem(i,kts,j,p_par)               &
             + (dtstep/(dz8w(i,kts,j)*60.))*                       &
               ( 0.4*e_bio(i,j,p_ald-1) + 2.9*e_bio(i,j,p_hc3-1)   &
               + 4.8*e_bio(i,j,p_hc5-1) + 7.9*e_bio(i,j,p_hc8-1)   &
               + 0.9*e_bio(i,j,p_ket-1) + 2.8*e_bio(i,j,p_oli-1)   &
               + 1.8*e_bio(i,j,p_olt-1) + 1.0*e_bio(i,j,p_ora2-1) )
      end do
      end do

      if (cbmz_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 2, 'cbmz_addemiss_bioaa',                 &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               kms, 13,                                                    &
               e_bio(ims,jms,p_ald-1),  e_bio(ims,jms,p_hc3-1),            &
               e_bio(ims,jms,p_hc5-1),  e_bio(ims,jms,p_hc8-1),            &
               e_bio(ims,jms,p_hcho-1), e_bio(ims,jms,p_iso-1),            &
               e_bio(ims,jms,p_ket-1),  e_bio(ims,jms,p_no-1),             &
               e_bio(ims,jms,p_oli-1),  e_bio(ims,jms,p_olt-1),            &
               e_bio(ims,jms,p_ora1-1), e_bio(ims,jms,p_ora2-1),           &
               e_bio(ims,jms,p_xyl-1),                                     &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1),            &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1),            &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1),            &
               e_bio(ims,jms,p_xyl-1),  e_bio(ims,jms,p_xyl-1)             )

   end if


!
! apply offline isoprene emissions when bio_emiss_opt /= GUNTHER1
!
   if (config_flags%bio_emiss_opt /= GUNTHER1) then

      if (cbmz_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 1, 'cbmz_addemiss_biobb',                 &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               1,                                                          &
               e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,                  &
               e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,                  &
               e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,e_iso                   )

      do j = jts, jte
      do k = kts, min(config_flags%kemit,kte-1)
      do i = its, ite
         chem(i,k,j,p_iso) = chem(i,k,j,p_iso) + e_iso(i,k,j)              &
              *4.828e-4/rho_phy(i,k,j)*(dtstep/(dz8w(i,k,j)*60.))
      end do
      end do
      end do

      if (cbmz_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 2, 'cbmz_addemiss_biobb',                 &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               1,                                                          &
               e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,                  &
               e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,                  &
               e_iso,e_iso,e_iso,e_iso,e_iso,e_iso,e_iso                   )

   end if


   END subroutine cbmz_addemiss_bio


END MODULE module_cbmz_addemiss



