MODULE module_bioemi_megan2

  ! MEGAN v2.04 Emissions Module for WRF-Chem
  !
  !  Reference: 
  !
  !    Estimates of global terrestial isoprene emissions using MEGAN
  !    (Model of Emissions of Gases and Aerosols from Nature )
  !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, P.I. Palmer, and C. Geron
  !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006.
  !
  !    MEGAN v2.0 Documentation
  !
  !
  ! August 2007
  !
  ! Serena H. Chung          Washington State University
  ! Tan Sakulyanontvittaya   University of Colorado
  ! Christine Wiedinmyer     National Center for Atmospheric Research
  !
  !
  !  11/08/2007 SHC Took out some "if (ktau ==1) then ... end if " statements
  !

CONTAINS

  SUBROUTINE bio_emissions_megan2(id,config_flags,ktau,dtstep,         &
       curr_secs,julday,gmt,xlat,xlong,p_phy,rho_phy,dz8w,             &
       chem, ne_area,                                                  &
       current_month,                                                  &
       T2,swdown,                                                      &
       nmegan, EFmegan, msebio_isop,                                   &
       mlai,                                                           &
       pftp_bt, pftp_nt, pftp_sb, pftp_hb,                             &
       mtsa,                                                           &
       mswdown,                                                        &
       mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                 &
       mebio_acet, mebio_mbo, mebio_no,                                &
       ebio_iso,ebio_oli,ebio_api,ebio_lim,                            &
       ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                   &
       ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_no,                   &
       ebio_c10h16,ebio_tol,ebio_bigalk, ebio_ch3oh,ebio_acet,         &
       ebio_nh3,ebio_no2,ebio_c2h5oh,ebio_ch3cooh,ebio_mek,            &
       ebio_bigene,ebio_c2h6,ebio_c2h4,ebio_c3h6,ebio_c3h8,ebio_so2,   &
       ebio_dms,ebio_hcn,                                              &
       ebio_c5h8,ebio_apinene,ebio_bpinene,ebio_toluene,               &
       ebio_ch3cho,ebio_ch3co2h,ebio_tbut2ene,ebio_c2h5cho, &
       ebio_nc4h10, &
       ebio_sesq, ebio_mbo, ebio_bpi, ebio_myrc,                       &
       ebio_alk3, ebio_alk4, ebio_alk5, ebio_ole1, ebio_ole2,          &    
       ebio_aro1, ebio_aro2, ebio_ccho, ebio_meoh,                     &    
       ebio_ethene, ebio_hcooh, ebio_terp, ebio_bald,                  &    
       ebio_cco_oh, ebio_rco_oh,                                       &    
       e_bio,                                                          &
       ids,ide, jds,jde, kds,kde,                                      &
       ims,ime, jms,jme, kms,kme,                                      &
       its,ite, jts,jte, kts,kte                                       )

    USE module_configure
    USE module_state_description
    USE module_data_megan2
    USE module_data_mgn2mech
!    USE module_bioemi_beis313, ONLY : getpar, calc_zenithb

    IMPLICIT NONE

    ! Subroutine arguments

    ! ...simulation parameters
    TYPE(grid_config_rec_type),  INTENT(IN)    :: config_flags

    ! ...domain id, current time step counter, xyz indices ..
    INTEGER,   INTENT(IN   ) :: id,ktau,                               &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte

    ! ...current julian day
    INTEGER, INTENT (IN) :: julday   
    !...GTM hour of start of simulation, time step in seconds
    REAL, INTENT(IN) :: gmt,dtstep

    ! ...number of seconds into the simulation
    REAL(KIND=8), INTENT(IN) :: curr_secs

    ! ...3rd dimension size of array e_bio
    INTEGER, INTENT (IN) :: ne_area

    !...pressure (Pa)
    REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                   &
         INTENT(IN) :: p_phy

    !...latitude and longitude (degrees)
    REAL,  DIMENSION( ims:ime , jms:jme ),                             &
         INTENT(IN) :: xlat, xlong

    !... air density (kg air/m3)
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                      &
         INTENT(IN) :: rho_phy

    !...full layer height (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,           &
          INTENT(IN) :: dz8w

    !...2-meter temperature (K)
    REAL,  DIMENSION( ims:ime , jms:jme ),                             &
         INTENT(IN) :: T2

    !...downward shortwave surface flux (W/m2)
    REAL,  DIMENSION( ims:ime , jms:jme ),                             &
         INTENT(IN) :: swdown                                    

    !...Number of MEGAN v2.04 species as specified by the namelist 
    !...variable nmegan; nmegan should equal n_spca_spc (this will
    !...be checked later.)  Currently nmegan=n_spca_spc=138.
    INTEGER, INTENT(IN) :: nmegan
    
    !...Emissions factors for nmegan=n_spca_spc=138 MEGAN v2.04 species
    REAL, DIMENSION (ims:ime, jms:jme , nmegan) ,                      &
         INTENT(INOUT) :: EFmegan

    !...Emission factor for isoprene (read in from file
    !...(wrfbiochemi_d<domain>)
    !...(moles compound/km^2/hr)
    REAL,  DIMENSION( ims:ime , jms:jme ),                             &
         INTENT(IN ) :: msebio_isop

    !...Plant functional group percentage  (read in from file
    !...(wrfbiochemi_d<domain>)
    REAL, DIMENSION ( ims:ime , jms:jme ),                             &
         INTENT(IN) ::                                                 &
         pftp_bt, pftp_nt, pftp_sb, pftp_hb

    !..."Climatological" Leaf area index  (read in from file
    !...(wrfbiochemi_d<domain>)
    REAL,  DIMENSION( ims:ime , jms:jme , 12 ),                        &
         INTENT(IN) :: mlai

    !..."Climatological" surface air temperature (K) (read in from file
    !...(wrfbiochemi_d<domain>)
    REAL,  DIMENSION( ims:ime , jms:jme , 12 ),                        &
         INTENT(IN) :: mtsa

    !..."Climatological" downward radiation (W/m2) (read in from file
    !...(wrfbiochemi_d<domain>)
    REAL, DIMENSION ( ims:ime , jms:jme , 12 ),                        &
         INTENT(IN) :: mswdown

    !...Actual emissions for a few selected species as diagnostics, using
    !...MEGAN v2.0 classes of species classification
    !...(mol km-2 hr-1)
    REAL,  DIMENSION( ims:ime , jms:jme ),                             &
         INTENT(INOUT) ::                                              &
         mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,               &
         mebio_acet, mebio_mbo, mebio_no

    !...Actual biogenic emissions, converted to mechanisms species.
    !...(ppm m/min)
   REAL, DIMENSION( ims:ime, jms:jme, ne_area ),                       &
         INTENT(INOUT ) :: e_bio

    !...Actual biogenic emissions, converted to mechanisms species.
    !...These variables were originally for BEIS3.11 biogenic emissions
    !...modules.
    !...(moles compound/km^2/hr)
    REAL,  DIMENSION( ims:ime , jms:jme ),                             &
         INTENT(INOUT  ) ::                                            &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,                          &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                 &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_no,                 &
         ebio_c10h16,ebio_tol,ebio_bigalk, ebio_ch3oh,ebio_acet,       &
         ebio_nh3,ebio_no2,ebio_c2h5oh,ebio_ch3cooh,ebio_mek,          &
         ebio_bigene,ebio_c2h6,ebio_c2h4,ebio_c3h6,ebio_c3h8,ebio_so2, &
         ebio_dms,ebio_hcn,                                            &
         ebio_c5h8,ebio_apinene,ebio_bpinene,ebio_toluene,             &
         ebio_ch3cho,ebio_ch3co2h,ebio_tbut2ene,ebio_c2h5cho,          &
         ebio_nc4h10,                                                  &
         ebio_sesq,ebio_mbo,ebio_bpi,ebio_myrc,                        &
         ebio_alk3, ebio_alk4, ebio_alk5, ebio_ole1, ebio_ole2,        &    
         ebio_aro1, ebio_aro2, ebio_ccho, ebio_meoh,                   &    
         ebio_ethene, ebio_hcooh, ebio_terp, ebio_bald,                &    
         ebio_cco_oh, ebio_rco_oh

    !...Array of chemical concentrations
    !...  in  - concentrations before biogenic emissions
    !...  out - concentrations after biogeniec emissions
    !... gas-phace concentrations are in ppm
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),            &
         INTENT(INOUT) :: chem

    !...Current month
    INTEGER, INTENT(IN) :: current_month

    ! Local parameters

    !...Below which set emissions rate to zero (mol km-2 hr-1)
    REAL, PARAMETER :: min_emis = 0.001
    

    !...number of days in each month
    INTEGER, PARAMETER :: DaysInMonth(12) = (/   &
         31,28,31,30,31,30,31,31,30,31,30,31 /)
    !...conversion between radians and degrees
    REAL, PARAMETER :: PI = 3.14159 
    REAL, PARAMETER :: D2RAD = PI/180.0 


    ! Local Scalars

    CHARACTER(len=256)   ::   mesg
    INTEGER :: i,j,k,i_class, i_spc, icount, p_in_chem
    INTEGER :: previous_month

    !...minutes since start of run to the middle of the
    !...current times step (seconds included as decimals)
    REAL(KIND=8) :: xtime
 
    !...the GMT hour of the middle of the current time step
    !...(can be greater than 24)
    INTEGER :: ixhour
    REAL(KIND=8) :: xhour

    !...minutes past the previous hour mark, at the
    !...middle of the current time step
    REAL :: xmin

    !...the GMT hour of the middle of the current time step
    !...(between 0 and 24)
    REAL :: gmtp

    !...GMT hour plus minutes (in fractaionl hour) of the middle
    !...of the current time step
    REAL :: tmidh

    !...Current and previous month leaf area index
    REAL :: LAIc, LAIp

    !...temperature((K) and pressure (mb)
    REAL :: tsa, tsa24, pres

    !...latitude and longitude (degrees)
    REAL :: lat, lon

    !...downward solar radiation, current and some 24-hour mean (W/m2)
    REAL :: swd, swd24
    !...photosynthetic photon flux density (i.e. PPDF or PAR)  
    !...(micromole m-2 s-1)
    REAL :: par, par24, pardb, pardif

    !...solar zenith angle (radians), cosine of zenith angle
    REAL :: zen , coszen

    !...days in the previous month
    REAL :: tstlen

    !...emissions factor (microgram m-2 hr-1)
    REAL :: epsilon

    !...MEGAN v2.04 emissions adjustment factors for leaf area, temperature,
    !...light, leaf age, and soil moisture
    !...(dimensionless)
    REAL :: gam_LHT, gam_TMP, gam_PHO,gam_AGE, gam_SMT

    !...normalized ratio accounting for production and loss within 
    !...plant canopies (dimensionless)
    REAL :: rho

    !...Some light-dependent factor (dimensionless)
    REAL :: ldf

    !...conversion factor from mol km-2 hr-1 to ppm m min-1
    REAL :: convert2

    !...emission rate converted to mechanism species in mol km-2 hr-1
    REAL :: gas_emis

    ! Local Arrays

    !...emissions adjustment factors for n_mgn_spc=20 classes of
    !...MEGAN v2.04 specie.
    !...adjust_factor = [GAMMA]*[rho] (see comments later)
    !...(dimensionless)
    REAL, DIMENSION(n_mgn_spc) :: adjust_factor

    !...plant functional type fractions
    REAL :: pft_frac(n_pft)

    !...actually emissions rates of n_spca_spc=138 MEGAN v2.04 species
    !...(mol km-2 hr-2)
    REAL, DIMENSION ( n_spca_spc ) :: E_megan2

    ! End header ------------------------------------------------------


    ! MEGAN v2.04 has nmegan=n_spca_spc=138 species, which are lumped 
    ! into n_mgn_spc=20 classes.  The number, names and indices of
    ! these classes and species are defined in module_data_megan2.F.
    ! They need to follow a few rules
    
    IF ( ktau .EQ. 1 ) THEN

       ! The size of variable EFmegan(:,:,nmegan) is allocated based on
       ! the value of namelist variable nmegan.  nmegan should be equal
       ! to n_spca_spc (though can be greater than to n_spca_spc).
       IF ( nmegan .NE. n_spca_spc ) THEN
          WRITE(mesg,*)'namelist variable nmegan does not match n_spca_spc'
          CALL wrf_error_fatal(mesg)          
       END IF

       ! For programming, the ordering of the  species or classes of
       ! species should not matter, except that isoprene should always
       ! be first; therefore, imgn_isop=1 and is_isoprene=1 always.
       IF ( (imgn_isop .NE. 1) .OR. (is_isoprene .NE. 1) ) THEN
          WRITE(mesg,*)'imgn_isop and is_isoprene in bio_emissions_megan should be 1'
          CALL wrf_error_fatal(mesg)          
       END IF

    END IF


    ! Initialize diagnostic output
    ebio_iso  ( its:ite , jts:jte ) = 0.0
    ebio_oli  ( its:ite , jts:jte ) = 0.0
    ebio_api  ( its:ite , jts:jte ) = 0.0
    ebio_lim  ( its:ite , jts:jte ) = 0.0
    ebio_hc3  ( its:ite , jts:jte ) = 0.0
    ebio_ete  ( its:ite , jts:jte ) = 0.0
    ebio_olt  ( its:ite , jts:jte ) = 0.0
    ebio_ket  ( its:ite , jts:jte ) = 0.0
    ebio_ald  ( its:ite , jts:jte ) = 0.0
    ebio_hcho ( its:ite , jts:jte ) = 0.0
    ebio_eth  ( its:ite , jts:jte ) = 0.0
    ebio_ora2 ( its:ite , jts:jte ) = 0.0
    ebio_co   ( its:ite , jts:jte ) = 0.0
    ebio_no   ( its:ite , jts:jte ) = 0.0
    ebio_c10h16( its:ite , jts:jte ) = 0.0
    ebio_tol  ( its:ite , jts:jte ) = 0.0
    ebio_bigalk( its:ite , jts:jte ) = 0.0
    ebio_ch3oh ( its:ite , jts:jte ) = 0.0
    ebio_acet  ( its:ite , jts:jte ) = 0.0
    ebio_nh3   ( its:ite , jts:jte ) = 0.0
    ebio_no2   ( its:ite , jts:jte ) = 0.0
    ebio_c2h5oh( its:ite , jts:jte ) = 0.0
    ebio_ch3cooh( its:ite , jts:jte ) = 0.0
    ebio_mek   ( its:ite , jts:jte ) = 0.0
    ebio_bigene( its:ite , jts:jte ) = 0.0
    ebio_c2h4  ( its:ite , jts:jte ) = 0.0
    ebio_c2h6  ( its:ite , jts:jte ) = 0.0
    ebio_c3h6  ( its:ite , jts:jte ) = 0.0
    ebio_c3h8  ( its:ite , jts:jte ) = 0.0
    ebio_so2   ( its:ite , jts:jte ) = 0.0
    ebio_dms   ( its:ite , jts:jte ) = 0.0
    ebio_terp  ( its:ite , jts:jte ) = 0.0
    ebio_c5h8   ( its:ite , jts:jte ) = 0.0
    ebio_apinene   ( its:ite , jts:jte ) = 0.0
    ebio_bpinene   ( its:ite , jts:jte ) = 0.0
    ebio_toluene   ( its:ite , jts:jte ) = 0.0
    ebio_hcooh   ( its:ite , jts:jte ) = 0.0
    ebio_ch3cho   ( its:ite , jts:jte ) = 0.0
    ebio_c2h5oh   ( its:ite , jts:jte ) = 0.0
    ebio_ch3co2h   ( its:ite , jts:jte ) = 0.0
    ebio_tbut2ene   ( its:ite , jts:jte ) = 0.0
    ebio_c2h5cho   ( its:ite , jts:jte ) = 0.0
    ebio_nc4h10   ( its:ite , jts:jte ) = 0.0
    ebio_sesq  ( its:ite , jts:jte ) = 0.0
    ebio_mbo   ( its:ite , jts:jte ) = 0.0
    ebio_bpi   ( its:ite , jts:jte ) = 0.0
    ebio_myrc  ( its:ite , jts:jte ) = 0.0
    e_bio     ( its:ite , jts:jte , 1:ne_area) = 0.0
    
    !...the following is redundant if there is no
    !...bug in the subroutine
    mebio_isop ( its:ite , jts:jte ) = 0.0
    mebio_apin ( its:ite , jts:jte ) = 0.0
    mebio_bpin ( its:ite , jts:jte ) = 0.0
    mebio_bcar ( its:ite , jts:jte ) = 0.0
    mebio_acet ( its:ite , jts:jte ) = 0.0 
    mebio_mbo  ( its:ite , jts:jte ) = 0.0
    mebio_no   ( its:ite , jts:jte ) = 0.0


    ! Extract climatological values for relevant months.
    !
    !  In MEGAN v2.04, emissions rates dependent on ambient conditions
    !  of the past 24 hours to the past month or so. The implementation
    !  of MEGAN v2.04 here uses monthly-mean values of the previous
    !  month for any past history required by the model.  The monthly-
    !  -mean values should be provided as input in the
    !  wrfbiochemi_d<domain> file.  Fully implementation (not done here)
    !  require online calculations of 24-hour and 240-hour mean of
    !  surface air temperature and downward PAR
    !
    !  MEGAN v2.04 also requires time-dependent leaf area index to
    !  estimate leaf age.  Here, leaf area indices of the current
    !  and the previous months are used.  The data should be
    !  provided in wrfbiochemi_d<domain> file.

    IF (current_month > 1) THEN
       previous_month = current_month -1
    ELSE
       previous_month = 12
    END IF


    ! Following module_phot_fastj.F, determine current
    ! time of day in GMT at the middle of the current 
    ! time step, tmidh.
    !     ktau  - time step counter
    !     dstep - time step in seconds
    !     gmt   - starting hour (in GMT) of the simulation

    !...minutes since start of run to the middle of the
    !...current times step (seconds included as decimals)
    !(old way in r4 this will fail in about 2 yrs)...
!    xtime=(ktau-1)*dtstep/60. + dtstep/120.
    xtime = curr_secs/60._8 + real(dtstep/120.,8)
    !...the GMT hour of the middle of the current time step
    !...(can be greater than 24)
    ixhour = int(gmt + 0.01) + int(xtime/60._8)
    xhour=real(ixhour,8)
    !...minutes past the previous hour mark, at the
    !...middle of the current time step
    xmin = 60.*gmt + real(xtime-xhour*60._8,8)
    !...the GMT hour of the middle of the current time step
    !...(between 0 and 24)
    gmtp=MOD(xhour,24._8)
    !...GMT hour plus minutes (in fractaionl hour) of the middle
    !...of the current time step
    tmidh= gmtp + xmin/60.

    WRITE(mesg,*) 'calculate MEGAN emissions at ktau, gmtp, tmidh = ',ktau, gmtp, tmidh
    CALL wrf_message(mesg)


    ! Get the mechanism converstion table
    ! ( Even though the mechanism converstion table is time-independent,
    ! do this for all time steps to be sure there will be no issue with 
    ! restart runs.  This should be edited eventually to reduce 
    ! redundant calculations.)
    ! SHC  (11/08/2007)
    GAS_MECH_SELECT1: SELECT CASE (config_flags%chem_opt)
             
    CASE (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP,GOCARTRADM2)
       ! get p_of_radm2cbmz(:), p_of_radm2(:), and radm2_per_megan(:)
       CALL get_megan2radm2_table

    CASE (RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACM_KPP, GOCARTRACM_KPP, RACMSORG_KPP, &
          RACM_MIM_KPP, RACMPM_KPP)
             
       ! get p_of_megan2racm(:), p_of_racm(:), and racm_per_megan(:)
       CALL get_megan2racm_table
    CASE (RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP,RACM_SOA_VBS_HET_KPP)

        !get p_of_megan2racm(:), p_of_racm(:), and racm_per_megan(:)
        CALL get_megan2racmSOA_table

    CASE (CBMZ, CBMZ_BB, CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, &
          CBMZ_MOSAIC_4BIN, & 
          CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
          CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, &
          CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, CBMZSORG, CBMZSORG_AQ, &
          CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)
        
       ! get p_of_megan2cbmz(:), p_of_cbmz(:), and cbmz_per_megan(:)
       CALL get_megan2cbmz_table

    CASE (CB05_SORG_AQ_KPP)
       CALL get_megan2cb05_table

    CASE ( CB05_SORG_VBS_AQ_KPP)
       CALL get_megan2cb05vbs_table

    CASE ( MOZART_KPP, MOZCART_KPP )
       ! get p_of_megan2mozcart(:), p_of_mozcart(:), and mozcart_per_megan(:)
       CALL get_megan2mozcart_table
    CASE (  T1_MOZCART_KPP )
       CALL get_megan2t1_mozc_table
    CASE (  MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP )
       CALL get_megan2mozm_table

    CASE (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
         SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP)!BSINGH(12/03/2013): Added SAPRC 8 bin and non-aq on (04/07/2014) ! FIX FOR SAPRC07A
       CALL get_megan2saprcnov_table

    CASE ( CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP )
       ! get p_of_megan2crimech(:), p_of_crimech(:), and crimech_per_megan(:)
       CALL get_megan2crimech_table

    CASE DEFAULT
       
       CALL wrf_error_fatal('Species conversion table for MEGAN v2.04 not available. ')

    END SELECT GAS_MECH_SELECT1

    ! Calcuate biogenic emissions grid by grid

    j_loop: DO j = jts, jte
       i_loop: DO i = its, ite


          ! Put variables of ambient conditions into scalar variables

          tsa   = T2(i,j)                     ! air temperature at 2-meter (K)
          pres  = 0.01*p_phy(i,kts,j)         ! surface pressure (mb)
          lat   = xlat(i,j)                   ! latitude (degree) 
          lon   = xlong(i,j)                  ! longitude (degress)
          swd   = swdown(i,j)                 ! downward solar radiation (W/m2)
          LAIc  = mlai(i,j,current_month)     ! current month leaf area index
          LAIp  = mlai(i,j,previous_month)    ! previous month leaf area index

          !...Emissions are dependent on the ambient conditions in the last
          !...24 to 240 hours; here, use input data for monthly mean of the
          !...the previous month
          tsa24 = mtsa    (i,j,previous_month) ! [=]K
          swd24 = mswdown (i,j,previous_month) ! [=] W/m2

          !...Perform checks on max and min bounds for temperature
          IF (tsa .LT. 200.0) THEN
             WRITE (mesg,'("temperature too low at i=",i3," ,j=",i3 )')i,j
             CALL wrf_message(mesg)
          END IF
          IF (tsa .GT. 315.0 ) THEN
             WRITE (mesg,'("temperature too high at i=",i3," ,j=",i3," ;resetting to 315K" )')i,j
             CALL wrf_message(mesg)
             tsa = 315.0
          END IF

!          !...Calculate zenith angle (in radians)
!          !...(NOTE: nonstandard longitude input here: >0 for W, <0 for E!!)
!          !...(subroutine calc_zenithb is in module_bioemis_beis313.F)
!          CALL calc_zenithb(lat,-lon,julday,tmidh,zen)
!          coszen = COS(zen)

          !....Convert downward solar radiation to photosynthetically
          !....active radiation

!          !......(subroutine getpar is in module_bioemis_beis313.F)
!          CALL getpar( swd, pres, zen, pardb, pardif )
!          par = pardb + pardif ! micro-mole/m2/s

          !......assume 4.766 (umol m-2 s-1) per (W m-2)
          !......assume 1/2 of swd is in 400-700 nm band
          par = 4.766 * 0.5 * swd

          !......Check max/min bounds of PAR
          IF ( par .LT. 0.00 .OR. par .GT. 2600.0 ) THEN
             WRITE (mesg,'("par out of range at i=",i3," ,j=",i3," par =",f8.3 )')i,j,par
             CALL wrf_message(mesg)
          END IF

          !......For the 24-avg PAR, 
          !......assume 4.766 (umol m-2 s-1) per (W m-2)
          !......assume 1/2 of swd is in 400-700 band
          par24 = swd24 * 4.766 * 0.5

          ! ------------------------------------------------------------
          !
          !  MEGAN v2.04 Box Model
          !
          !  Reference: 
          !
          !    Estimates of global terrestial isoprene emissions using MEGAN
          !    (Model of Emissions of Gases and Aerosols from Nature )
          !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, 
          !    P.I. Palmer, and C. Geron
          !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006      
          !
          !    MEGAN v2.0 Documentation
          !
          !
          !  The following code is based on Tan's megan.F dated 11/21/2006
          !
          !   Scientific algorithm
          !
          !           Emission = [epsilon][gamma][rho]
          !
          !         where [epsilon] = emission factor (usually um m-2 hr or mole km-2 hr-1)
          !               [gamma]   = emission activity factor (dimensionless)
          !               [rho]     = production and loss within plant canopies
          !                         (dimensionless)
          !
          !            [gamma]  = [gamma_CE][gamma_age][gamma_SM]
          !
          !         where [gamma_CE]  = canopy correction factor
          !               [gamma_age] = leaf age correction factor
          !               [gamma_SM]  = soil moisture correction factor
          !
          !            [gamma_CE] = [gamma_LAI][gamma_T]((1-LDF) + LDF*[gamma_P] )
          !                      
          !         where [gamma_LAI] = leaf area index factor
          !               [gamma_P]   = PPFD emission activity factor
          !               [gamma_T]   = temperature response factor
          !               LDF         = 
          !
          !           Emission = [epsilon][gamma_LAI][gamma_T][gamma_age]
          !                      x ((1-LDF) + LDF*[gamma_P] )[rho]
          !
          !     or
          ! 
          !           Emission = adjust_factor [epsilon]
          !
          !         where
          !
          !           adjust_fact = [gamma_LAI][gamma_T][gamma_age]((1-LDF) + LDF*[gamma_P] )[rho]


          !
          ! Calculate the dimensionless emission adjustment factor. 
          ! MEGAN v2.04 has n_spca_spc = 138 species.  These species are 
          ! lumped into n_mgn_spc=20 classes.  The emission adjustment
          ! factors are different for the 20 classes of species.
          !
          ! NOTE: This version of the code contains the corrected equation for 
          ! gamma P (based on a revised version of equation 11b from Guenther et al., 2006)
          ! CW (08/16/2007)
          !
          ! NOTE: This version of the code contains the updated emission factors (static)
          ! and beta values based on Alex's V2.1 notes sent out on August 13, 2007
          ! CW (08/16/2007)
          ! 
          ! NOTE: This version of the code applies the same gamma T equation to the 
          ! emissions of all compounds other than isoprene. This occurs regardless 
          ! of whether the emissions are light dependent or not. This is NOT the same 
          ! as what Alex has in his code. In his code, the light-dependent emissions
          ! are also given the isoprene gamma T. Because all emissions (other than isoprene
          ! are assigned the same gamma T, this could lead to overestimates of emissions
          ! at high temperatures (>40C). Light-dependent emisisons (e.g., SQTs) should
          ! fall off at high temperatures. (CW, 08/16/2007)

          !...Calculate adjustment factor components that are species-independent

          !......Get the leaf area index factor gam_LHT
          CALL GAMMA_LAI( LAIc, gam_LHT)

          !......Get the light emission activity factor gam_PHO
          CALL GAMMA_P( julday, tmidh, lat, lon, par, par24, gam_PHO )

          !......Get the soil moisture factor gam_SMT
          !......for now, set = 1.0
          gam_SMT = 1.0

          !...Calculate the overall emissions adjustment factors, for
          !...each of the n_mgn_spc=20 classes of compounds

          DO i_class = 1, n_mgn_spc
             
             ! Get the temperature response factor gam_TMP
             !  One algorithm for isoprene, and one for non-isoprene

             IF ( i_class == imgn_isop ) THEN
                CALL GAMMA_TISOP( tsa, tsa24, gam_TMP )
             ELSE
                CALL GAMMA_TNISP( i_class , tsa, gam_TMP  )
             END IF

             ! Get the leaf age correction factor gam_AGE

             !...Time step (days) between LAIc and LAIp:
             !...Since monthly mean LAI is used,
             !...use # of days in the previous month
             tstlen = REAL(DaysInMonth(previous_month),KIND(1.0))

             CALL GAMMA_A( i_class , LAIp, LAIc, TSTLEN, tsa24, gam_AGE )

             ! rho - normalized ratio accounting for production and
             ! oss within plant canopies; rho_fct is defined in 
             ! module_data_megan2.F; currently rho_fct = 1.0 for all 
             ! species (dimensionless)
             rho = rho_fct(i_class)

             ! Fraction of emission to apply light-dependence factor
             ! ldf_fct is defined in module_data_megan2.F
             ! (dimensionless)
             ldf = ldf_fct(i_class)

             ! The overall emissions adjustment factor
             ! (dimensionless)

             adjust_factor(i_class) = gam_TMP * gam_AGE * gam_LHT * gam_SMT * rho * &
                  ( (1.0-LDF) + gam_PHO*LDF )

          END DO !i_class = 1, n_mgn_spc (loop over classes of MEGAN species )


          ! For isoprene, the emission factor is already read in from
          ! wrfbiochemi_d<domain> file; therefore, actual emissions rate
          ! can be calculated here already.
          ! (mol km-2 hr-1)
          E_megan2(is_isoprene) = adjust_factor(imgn_isop)*msebio_isop(i,j)
          IF ( E_megan2(is_isoprene) .LT. min_emis ) E_megan2(is_isoprene)=0.


          ! Calculate emissions for all n_spca_spc=nmegan=138 MEGAN v2.04
          ! species, except for isoprene.  For non-isoprene emissions,
          ! the emission factor [epsilon] has to be calculated
          ! for the first time step.

          !...Loop over species, because emission factor [epsilon] is 
          !...different for each species
          !...( i_spc = 1 is skipped in the do loop below to skip 
          !...isoprene; this works because is_isoprene = 1 )
          DO i_spc = 2, n_spca_spc 

             ! The lumped class in which the current species is a member
             i_class = mg20_map (i_spc)

             ! Calculate emission factor (microgram m-2 hr-1) for 
             ! species i_spc 
             ! ( Even though EFmegan is time invariant, for now calculate
             ! EFmegan for every time step to be sure there will be
             ! no issue with restart runs.
             ! SHC  (11/08/2007)
!             IF ( ktau .EQ. 1 ) THEN

                ! Grab plant functional type fractions for current grid
                ! cell (pftp_* is the plant functional type % and was
                ! read in from wrfbiochemi_d<domain> file.)
                pft_frac(k_bt) = 0.01*pftp_bt(i,j)
                pft_frac(k_nt) = 0.01*pftp_nt(i,j)
                pft_frac(k_sb) = 0.01*pftp_sb(i,j)
                pft_frac(k_hb) = 0.01*pftp_hb(i,j)

                ! Sum up emissions factor over plant functional types
                epsilon = 0.0
                DO k = 1, n_pft !loop over plant functional types
                   epsilon = epsilon +                             &
                        pft_frac(k)*EF(i_class,k)*EF_frac(i_spc,k)
                END DO

                ! Store emission factor to variable EFmegan (which is
                ! declared in Registry/registry.chem)
                ! (migrogram m-2 hr-1)
                EFmegan(i,j,i_spc) = epsilon

!             END IF ! ( ktau .EQ. 1 )

             ! Calculate actual emission rate for species i_spc;
             ! also, convert units from (microgram m-2 hr-1) to 
             ! (mol km-2 hr-1)
             E_megan2(i_spc) = EFmegan(i,j,i_spc)*        &
                  adjust_factor(i_class)/spca_mwt(i_spc)
             IF ( E_megan2(i_spc) .LT. min_emis ) E_megan2(i_spc)=0.

          END DO !i_spc = 2, n_spca_spc, loop over all non-isoprene species


          ! Output emissions for some species as diagnostics
          ! (mol km-2 hr-1)
!          print*,'is_isoprene',is_isoprene
!          print*,'is_pinene_a',is_pinene_a
!          print*,'is_pinene_b',is_pinene_b

!          if (E_megan2 (is_isoprene).gt.0) print*,'E_megan2 (is_isoprene)',E_megan2 (is_isoprene)
!          if (E_megan2 (is_pinene_a).gt.0) print*,'E_megan2 (is_pinene_a)',E_megan2 (is_pinene_a)

          mebio_isop  (i,j) = E_megan2 ( is_isoprene        )
          mebio_apin  (i,j) = E_megan2 ( is_pinene_a        )
          mebio_bpin  (i,j) = E_megan2 ( is_pinene_b        )
          mebio_bcar  (i,j) = E_megan2 ( is_caryophyllene_b )
          mebio_acet  (i,j) = E_megan2 ( is_acetone         )
          mebio_mbo   (i,j) = E_megan2 ( is_MBO_2m3e2ol     )
          mebio_no    (i,j) = E_megan2 ( is_nitric_OXD      )


          ! Speciate the n_spca_spc=nmegan=138 species into
          ! the gas-phase mechanism species


          !...conversion factor from mol km-2 hr-1 to ppm m min-1
          !...(e_bio is in units of ppm m min-1)
          convert2 = 0.02897/(rho_phy(i,kts,j)*60.)


          !...
          GAS_MECH_SELECT: SELECT CASE (config_flags%chem_opt)

          CASE ( MOZART_KPP, MOZCART_KPP )

             DO icount = 1, n_megan2mozcart
!-----------------------------------------------------------------------
! Get index to chem array for the corresponding MOZCART species.  
!-----------------------------------------------------------------------
                p_in_chem = p_of_mozcart(icount)
use_megan_emission : &
                IF ( p_in_chem /= non_react ) THEN
!-----------------------------------------------------------------------
! Check if the species is actually in the mechanism
!-----------------------------------------------------------------------
is_mozcart_species : &
                   IF ( p_in_chem >= param_first_scalar ) THEN
!-----------------------------------------------------------------------
! Emission rate for mechanism species in mol km-2 hr-1
!-----------------------------------------------------------------------
                      gas_emis = mozcart_per_megan(icount) * E_megan2(p_of_megan2mozcart(icount))
!-----------------------------------------------------------------------
! Add emissions to diagnostic output variables.
! ebio_xxx (mol km-2 hr-1) were originally used by the 
! BEIS3.11 biogenic emissions module. 
! I have also borrowed variable e_bio (ppm m min-1).
!-----------------------------------------------------------------------
                      IF ( p_in_chem == p_isopr ) THEN
                         ebio_iso(i,j) = ebio_iso(i,j) + gas_emis
                         e_bio(i,j,p_isopr-1)   = e_bio(i,j,p_isopr-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_no ) THEN
                         ebio_no(i,j)  = ebio_no(i,j) + gas_emis
                         e_bio(i,j,p_no-1)   = e_bio(i,j,p_no-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_no2 ) THEN
                         ebio_no2(i,j)  = ebio_no2(i,j) + gas_emis
                         e_bio(i,j,p_no2-1)   = e_bio(i,j,p_no2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_co ) THEN
                         ebio_co(i,j)  = ebio_co(i,j) + gas_emis
                         e_bio(i,j,p_co-1)   = e_bio(i,j,p_co-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_hcho ) THEN
                         ebio_hcho(i,j) = ebio_hcho(i,j) + gas_emis
                         e_bio(i,j,p_hcho-1)   = e_bio(i,j,p_hcho-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ald ) THEN
                         ebio_ald(i,j) = ebio_ald(i,j) + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_acet ) THEN
                         ebio_acet(i,j) = ebio_acet(i,j) + gas_emis
                         e_bio(i,j,p_acet-1)   = e_bio(i,j,p_acet-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_tol ) THEN
                         ebio_tol(i,j) = ebio_tol(i,j) + gas_emis
                         e_bio(i,j,p_tol-1)   = e_bio(i,j,p_tol-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c10h16 ) THEN
                         ebio_c10h16(i,j) = ebio_c10h16(i,j) + gas_emis
                         e_bio(i,j,p_c10h16-1)   = e_bio(i,j,p_c10h16-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_so2 ) THEN
                         ebio_so2(i,j) = ebio_so2(i,j) + gas_emis
                         e_bio(i,j,p_so2-1)   = e_bio(i,j,p_so2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_dms ) THEN
                         ebio_dms(i,j) = ebio_dms(i,j) + gas_emis
                         e_bio(i,j,p_dms-1)   = e_bio(i,j,p_dms-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bigalk ) THEN
                         ebio_bigalk(i,j) = ebio_bigalk(i,j) + gas_emis
                         e_bio(i,j,p_bigalk-1)   = e_bio(i,j,p_bigalk-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bigene ) THEN
                         ebio_bigene(i,j) = ebio_bigene(i,j) + gas_emis
                         e_bio(i,j,p_bigene-1)   = e_bio(i,j,p_bigene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_nh3 ) THEN
                         ebio_nh3(i,j) = ebio_nh3(i,j) + gas_emis
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3oh ) THEN
                         ebio_ch3oh(i,j) = ebio_ch3oh(i,j) + gas_emis
                         e_bio(i,j,p_ch3oh-1)   = e_bio(i,j,p_ch3oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h5oh ) THEN
                         ebio_c2h5oh(i,j) = ebio_c2h5oh(i,j) + gas_emis
                         e_bio(i,j,p_c2h5oh-1)   = e_bio(i,j,p_c2h5oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3cooh ) THEN
                         ebio_ch3cooh(i,j) = ebio_ch3cooh(i,j) + gas_emis
                         e_bio(i,j,p_ch3cooh-1)   = e_bio(i,j,p_ch3cooh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_mek ) THEN
                         ebio_mek(i,j) = ebio_mek(i,j) + gas_emis
                         e_bio(i,j,p_mek-1)   = e_bio(i,j,p_mek-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h4 ) THEN
                         ebio_c2h4(i,j) = ebio_c2h4(i,j) + gas_emis
                         e_bio(i,j,p_c2h4-1)   = e_bio(i,j,p_c2h4-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h6 ) THEN
                         ebio_c2h6(i,j) = ebio_c2h6(i,j) + gas_emis
                         e_bio(i,j,p_c2h6-1)   = e_bio(i,j,p_c2h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h6 ) THEN
                         ebio_c3h6(i,j) = ebio_c3h6(i,j) + gas_emis
                         e_bio(i,j,p_c3h6-1)   = e_bio(i,j,p_c3h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h8 ) THEN
                         ebio_c3h8(i,j) = ebio_c3h8(i,j) + gas_emis
                         e_bio(i,j,p_c3h8-1)   = e_bio(i,j,p_c3h8-1)  + gas_emis*convert2
                      END IF
                   END IF is_mozcart_species
                END IF use_megan_emission
             END DO

          CASE ( T1_MOZCART_KPP )

             DO icount = 1, n_megan2t1_mozc
!-----------------------------------------------------------------------
! Get index to chem array for the corresponding T1_MOZCART species.  
!-----------------------------------------------------------------------
                p_in_chem = p_of_t1_mozc(icount)
use_megan_emis_a : &
                IF ( p_in_chem /= non_react ) THEN
!-----------------------------------------------------------------------
! Check if the species is actually in the mechanism
!-----------------------------------------------------------------------
is_t1_mozc_species : &
                   IF ( p_in_chem >= param_first_scalar ) THEN
!-----------------------------------------------------------------------
! Emission rate for mechanism species in mol km-2 hr-1
!-----------------------------------------------------------------------
                      gas_emis = t1_mozc_per_megan(icount) * E_megan2(p_of_megan2t1_mozc(icount))
!-----------------------------------------------------------------------
! Add emissions to diagnostic output variables.
! ebio_xxx (mol km-2 hr-1) were originally used by the 
! BEIS3.11 biogenic emissions module. 
! I have also borrowed variable e_bio (ppm m min-1).
!-----------------------------------------------------------------------
                      IF ( p_in_chem == p_isopr ) THEN
                         ebio_iso(i,j) = ebio_iso(i,j) + gas_emis
                         e_bio(i,j,p_isopr-1)   = e_bio(i,j,p_isopr-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_apin ) THEN
                         ebio_api(i,j) = ebio_api(i,j) + gas_emis
                         e_bio(i,j,p_apin-1)   = e_bio(i,j,p_apin-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bpin ) THEN
                         ebio_bpi(i,j) = ebio_bpi(i,j) + gas_emis
                         e_bio(i,j,p_bpin-1)   = e_bio(i,j,p_bpin-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_limon ) THEN
                         ebio_lim(i,j) = ebio_lim(i,j) + gas_emis
                         e_bio(i,j,p_limon-1)   = e_bio(i,j,p_limon-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_myrc ) THEN
                         ebio_myrc(i,j) = ebio_myrc(i,j) + gas_emis
                         e_bio(i,j,p_myrc-1)   = e_bio(i,j,p_myrc-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bcary ) THEN
                         ebio_sesq(i,j) = ebio_sesq(i,j) + gas_emis
                         e_bio(i,j,p_bcary-1)   = e_bio(i,j,p_bcary-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_mbo ) THEN
                         ebio_mbo(i,j) = ebio_mbo(i,j) + gas_emis
                         e_bio(i,j,p_mbo-1)   = e_bio(i,j,p_mbo-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3oh ) THEN
                         ebio_ch3oh(i,j) = ebio_ch3oh(i,j) + gas_emis
                         e_bio(i,j,p_ch3oh-1)   = e_bio(i,j,p_ch3oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h5oh ) THEN
                         ebio_c2h5oh(i,j) = ebio_c2h5oh(i,j) + gas_emis
                         e_bio(i,j,p_c2h5oh-1)   = e_bio(i,j,p_c2h5oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_hcho ) THEN
                         ebio_hcho(i,j) = ebio_hcho(i,j) + gas_emis
                         e_bio(i,j,p_hcho-1)   = e_bio(i,j,p_hcho-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ald ) THEN
                         ebio_ald(i,j) = ebio_ald(i,j) + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3cooh ) THEN
                         ebio_ch3cooh(i,j) = ebio_ch3cooh(i,j) + gas_emis
                         e_bio(i,j,p_ch3cooh-1)   = e_bio(i,j,p_ch3cooh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_hcooh ) THEN
                         ebio_hcooh(i,j) = ebio_hcooh(i,j) + gas_emis
                         e_bio(i,j,p_hcooh-1)   = e_bio(i,j,p_hcooh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_hcn ) THEN
                         ebio_hcn(i,j) = ebio_hcn(i,j) + gas_emis
                         e_bio(i,j,p_hcn-1)   = e_bio(i,j,p_hcn-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_nh3 ) THEN
                         ebio_nh3(i,j) = ebio_nh3(i,j) + gas_emis
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_co ) THEN
                         ebio_co(i,j)  = ebio_co(i,j) + gas_emis
                         e_bio(i,j,p_co-1)   = e_bio(i,j,p_co-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h4 ) THEN
                         ebio_c2h4(i,j) = ebio_c2h4(i,j) + gas_emis
                         e_bio(i,j,p_c2h4-1)   = e_bio(i,j,p_c2h4-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h6 ) THEN
                         ebio_c2h6(i,j) = ebio_c2h6(i,j) + gas_emis
                         e_bio(i,j,p_c2h6-1)   = e_bio(i,j,p_c2h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h6 ) THEN
                         ebio_c3h6(i,j) = ebio_c3h6(i,j) + gas_emis
                         e_bio(i,j,p_c3h6-1)   = e_bio(i,j,p_c3h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h8 ) THEN
                         ebio_c3h8(i,j) = ebio_c3h8(i,j) + gas_emis
                         e_bio(i,j,p_c3h8-1)   = e_bio(i,j,p_c3h8-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bigalk ) THEN
                         ebio_bigalk(i,j) = ebio_bigalk(i,j) + gas_emis
                         e_bio(i,j,p_bigalk-1)   = e_bio(i,j,p_bigalk-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bigene ) THEN
                         ebio_bigene(i,j) = ebio_bigene(i,j) + gas_emis
                         e_bio(i,j,p_bigene-1)   = e_bio(i,j,p_bigene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_tol ) THEN
                         ebio_tol(i,j) = ebio_tol(i,j) + gas_emis
                         e_bio(i,j,p_tol-1)   = e_bio(i,j,p_tol-1)  + gas_emis*convert2
                      END IF
                   END IF is_t1_mozc_species
                END IF use_megan_emis_a
             END DO

          CASE ( MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP )

             DO icount = 1, n_megan2mozm
!-----------------------------------------------------------------------
! Get index to chem array for the corresponding MOZCART species.  
!-----------------------------------------------------------------------
                p_in_chem = p_of_mozm(icount)
use_megan_emis : &
                IF ( p_in_chem /= non_react ) THEN
!-----------------------------------------------------------------------
! Check if the species is actually in the mechanism
!-----------------------------------------------------------------------
is_mozm_species : &
                   IF ( p_in_chem >= param_first_scalar ) THEN
!-----------------------------------------------------------------------
! Emission rate for mechanism species in mol km-2 hr-1
!-----------------------------------------------------------------------
                      gas_emis = mozm_per_megan(icount) * E_megan2(p_of_megan2mozm(icount))
!-----------------------------------------------------------------------
! Add emissions to diagnostic output variables.
! ebio_xxx (mol km-2 hr-1) were originally used by the 
! BEIS3.11 biogenic emissions module. 
! I have also borrowed variable e_bio (ppm m min-1).
!-----------------------------------------------------------------------
                      IF ( p_in_chem == p_isopr ) THEN
                         ebio_iso(i,j) = ebio_iso(i,j) + gas_emis
                         e_bio(i,j,p_isopr-1)   = e_bio(i,j,p_isopr-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_no ) THEN
                         ebio_no(i,j)  = ebio_no(i,j) + gas_emis
                         e_bio(i,j,p_no-1)   = e_bio(i,j,p_no-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_no2 ) THEN
                         ebio_no2(i,j)  = ebio_no2(i,j) + gas_emis
                         e_bio(i,j,p_no2-1)   = e_bio(i,j,p_no2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_co ) THEN
                         ebio_co(i,j)  = ebio_co(i,j) + gas_emis
                         e_bio(i,j,p_co-1)   = e_bio(i,j,p_co-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_hcho ) THEN
                         ebio_hcho(i,j) = ebio_hcho(i,j) + gas_emis
                         e_bio(i,j,p_hcho-1)   = e_bio(i,j,p_hcho-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ald ) THEN
                         ebio_ald(i,j) = ebio_ald(i,j) + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_acet ) THEN
                         ebio_acet(i,j) = ebio_acet(i,j) + gas_emis
                         e_bio(i,j,p_acet-1)   = e_bio(i,j,p_acet-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_tol ) THEN
                         ebio_tol(i,j) = ebio_tol(i,j) + gas_emis
                         e_bio(i,j,p_tol-1)   = e_bio(i,j,p_tol-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_apin ) THEN
                         ebio_api(i,j) = ebio_api(i,j) + gas_emis
                         e_bio(i,j,p_apin-1)   = e_bio(i,j,p_apin-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bpin ) THEN
                         ebio_bpi(i,j) = ebio_bpi(i,j) + gas_emis
                         e_bio(i,j,p_bpin-1)   = e_bio(i,j,p_bpin-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_limon ) THEN
                         ebio_lim(i,j) = ebio_lim(i,j) + gas_emis
                         e_bio(i,j,p_limon-1)   = e_bio(i,j,p_limon-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_mbo ) THEN
                         ebio_mbo(i,j) = ebio_mbo(i,j) + gas_emis
                         e_bio(i,j,p_mbo-1)   = e_bio(i,j,p_mbo-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_myrc ) THEN
                         ebio_myrc(i,j) = ebio_myrc(i,j) + gas_emis
                         e_bio(i,j,p_myrc-1)   = e_bio(i,j,p_myrc-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bcary ) THEN
                         ebio_sesq(i,j) = ebio_sesq(i,j) + gas_emis
                         e_bio(i,j,p_bcary-1)   = e_bio(i,j,p_bcary-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_so2 ) THEN
                         ebio_so2(i,j) = ebio_so2(i,j) + gas_emis
                         e_bio(i,j,p_so2-1)   = e_bio(i,j,p_so2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_dms ) THEN
                         ebio_dms(i,j) = ebio_dms(i,j) + gas_emis
                         e_bio(i,j,p_dms-1)   = e_bio(i,j,p_dms-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bigalk ) THEN
                         ebio_bigalk(i,j) = ebio_bigalk(i,j) + gas_emis
                         e_bio(i,j,p_bigalk-1)   = e_bio(i,j,p_bigalk-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bigene ) THEN
                         ebio_bigene(i,j) = ebio_bigene(i,j) + gas_emis
                         e_bio(i,j,p_bigene-1)   = e_bio(i,j,p_bigene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_nh3 ) THEN
                         ebio_nh3(i,j) = ebio_nh3(i,j) + gas_emis
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3oh ) THEN
                         ebio_ch3oh(i,j) = ebio_ch3oh(i,j) + gas_emis
                         e_bio(i,j,p_ch3oh-1)   = e_bio(i,j,p_ch3oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h5oh ) THEN
                         ebio_c2h5oh(i,j) = ebio_c2h5oh(i,j) + gas_emis
                         e_bio(i,j,p_c2h5oh-1)   = e_bio(i,j,p_c2h5oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3cooh ) THEN
                         ebio_ch3cooh(i,j) = ebio_ch3cooh(i,j) + gas_emis
                         e_bio(i,j,p_ch3cooh-1)   = e_bio(i,j,p_ch3cooh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_mek ) THEN
                         ebio_mek(i,j) = ebio_mek(i,j) + gas_emis
                         e_bio(i,j,p_mek-1)   = e_bio(i,j,p_mek-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h4 ) THEN
                         ebio_c2h4(i,j) = ebio_c2h4(i,j) + gas_emis
                         e_bio(i,j,p_c2h4-1)   = e_bio(i,j,p_c2h4-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h6 ) THEN
                         ebio_c2h6(i,j) = ebio_c2h6(i,j) + gas_emis
                         e_bio(i,j,p_c2h6-1)   = e_bio(i,j,p_c2h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h6 ) THEN
                         ebio_c3h6(i,j) = ebio_c3h6(i,j) + gas_emis
                         e_bio(i,j,p_c3h6-1)   = e_bio(i,j,p_c3h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h8 ) THEN
                         ebio_c3h8(i,j) = ebio_c3h8(i,j) + gas_emis
                         e_bio(i,j,p_c3h8-1)   = e_bio(i,j,p_c3h8-1)  + gas_emis*convert2
                      END IF
                   END IF is_mozm_species
                END IF use_megan_emis
             END DO

          CASE (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP,GOCARTRADM2)

             DO icount = 1, n_megan2radm2

                IF ( p_of_radm2(icount) .NE. non_react ) THEN
                
                   ! Get index to chem array for the corresponding RADM2
                   ! species.  
                   p_in_chem = p_of_radm2(icount)

                   ! Check if the species is actually in the mechanism
                   IF ( p_in_chem >= param_first_scalar ) THEN
                      
                      ! Emission rate for mechanism species in mol km-2 hr-1
                      gas_emis = radm2_per_megan(icount) * E_megan2(p_of_megan2radm2(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the 
                      ! BEIS3.11 biogenic emissions module. 
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_iso ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_iso-1)   = e_bio(i,j,p_iso-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_oli) THEN
                         ebio_oli(i,j)        = ebio_oli(i,j)       + gas_emis
                         e_bio(i,j,p_oli-1)   = e_bio(i,j,p_oli-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc3) THEN
                         ebio_hc3(i,j)        = ebio_hc3(i,j)       + gas_emis
                         e_bio(i,j,p_hc3-1)   = e_bio(i,j,p_hc3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_olt) THEN
                         ebio_olt(i,j)        = ebio_olt(i,j)       + gas_emis
                         e_bio(i,j,p_olt-1)   = e_bio(i,j,p_olt-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ket) THEN
                         ebio_ket(i,j)        = ebio_ket(i,j)       + gas_emis
                         e_bio(i,j,p_ket-1)   = e_bio(i,j,p_ket-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ald) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hcho) THEN
                         ebio_hcho(i,j)       = ebio_hcho(i,j)      + gas_emis
                         e_bio(i,j,p_hcho-1)  = e_bio(i,j,p_hcho-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_eth) THEN
                         ebio_eth(i,j)        = ebio_eth(i,j)       + gas_emis
                         e_bio(i,j,p_eth-1)   = e_bio(i,j,p_eth-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora2) THEN
                         ebio_ora2(i,j)       = ebio_ora2(i,j)      + gas_emis
                         e_bio(i,j,p_ora2-1)  = e_bio(i,j,p_ora2-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_co) THEN
                         ebio_co(i,j)         = ebio_co(i,j)        + gas_emis
                         e_bio(i,j,p_co-1)    = e_bio(i,j,p_co-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_no) THEN
                         ebio_no(i,j)         = ebio_no(i,j)        + gas_emis   
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ol2) THEN
                          e_bio(i,j,p_ol2-1)  = e_bio(i,j,p_ol2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc5) THEN
                          e_bio(i,j,p_hc5-1)  = e_bio(i,j,p_hc5-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc8) THEN
                          e_bio(i,j,p_hc8-1)  = e_bio(i,j,p_hc8-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora1) THEN
                          e_bio(i,j,p_ora1-1) = e_bio(i,j,p_ora1-1) + gas_emis*convert2
                      END IF

                   END IF !( p_in_chem >= param_first_scalar )

                END IF !( p_of_ramd2(icount) .NE. non_react )
                
             END DO

           CASE (RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACM_KPP, GOCARTRACM_KPP, &
                 RACMSORG_KPP, RACM_MIM_KPP, RACMPM_KPP)

             DO icount = 1, n_megan2racm

                IF ( p_of_racm(icount) .NE. non_react ) THEN

                   ! Get index to chem array for the corresponding RACM
                   ! species.  
                   p_in_chem = p_of_racm(icount)
                   
                   ! Check if the species is actually in the mechanism
                   IF( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate of mechanism species in mol km-2 hr-1
                      gas_emis =  racm_per_megan(icount) * E_megan2(p_of_megan2racm(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the 
                      ! BEIS3.11 biogenic emissions module. 
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_iso ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_iso-1)   = e_bio(i,j,p_iso-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_oli) THEN
                         ebio_oli(i,j)        = ebio_oli(i,j)       + gas_emis
                         e_bio(i,j,p_oli-1)   = e_bio(i,j,p_oli-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_api) THEN
                         ebio_api(i,j)        = ebio_api(i,j)       + gas_emis
                         e_bio(i,j,p_api-1)   = e_bio(i,j,p_api-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_lim) THEN
                         ebio_lim(i,j)        = ebio_lim(i,j)       + gas_emis
                         e_bio(i,j,p_lim-1)   = e_bio(i,j,p_lim-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc3) THEN
                         ebio_hc3(i,j)        = ebio_hc3(i,j)       + gas_emis
                         e_bio(i,j,p_hc3-1)   = e_bio(i,j,p_hc3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ete) THEN
                         ebio_ete(i,j)        = ebio_ete(i,j)       + gas_emis
                         e_bio(i,j,p_ete-1)   = e_bio(i,j,p_ete-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_olt) THEN
                         ebio_olt(i,j)        = ebio_olt(i,j)       + gas_emis
                         e_bio(i,j,p_olt-1)   = e_bio(i,j,p_olt-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ket) THEN
                         ebio_ket(i,j)        = ebio_ket(i,j)       + gas_emis
                         e_bio(i,j,p_ket-1)   = e_bio(i,j,p_ket-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ald) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hcho) THEN
                         ebio_hcho(i,j)       = ebio_hcho(i,j)      + gas_emis
                         e_bio(i,j,p_hcho-1)  = e_bio(i,j,p_hcho-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_eth) THEN
                         ebio_eth(i,j)        = ebio_eth(i,j)       + gas_emis
                         e_bio(i,j,p_eth-1)   = e_bio(i,j,p_eth-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora2) THEN
                         ebio_ora2(i,j)       = ebio_ora2(i,j)      + gas_emis
                         e_bio(i,j,p_ora2-1)  = e_bio(i,j,p_ora2-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_co) THEN
                         ebio_co(i,j)         = ebio_co(i,j)        + gas_emis
                         e_bio(i,j,p_co-1)    = e_bio(i,j,p_co-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_no) THEN
                         ebio_no(i,j)         = ebio_no(i,j)        + gas_emis   
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc5) THEN
                          e_bio(i,j,p_hc5-1)  = e_bio(i,j,p_hc5-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc8) THEN
                          e_bio(i,j,p_hc8-1)  = e_bio(i,j,p_hc8-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora1) THEN
                          e_bio(i,j,p_ora1-1) = e_bio(i,j,p_ora1-1) + gas_emis*convert2
                      END IF

                   END IF !( p_in_chem > param_first_scalar )
                   

                END IF !( p_of_racm(icount) .NE. non_react )

             END DO

          CASE (CB05_SORG_AQ_KPP)

             DO icount = 1, n_megan2cb05
                IF ( p_of_cb05 (icount) .NE. non_react ) THEN
                   ! Get index to chem array for the corresponding CB05
                   ! species.
                   p_in_chem = p_of_cb05(icount)

                   ! Check if the species is actually in the mechanism
                   ! (e.g., NH3 is in the mechanism only if aerosols
                   ! are simulated)
                   ! Check if the species is actually in the mechanism
                   IF ( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate for mechanism species in mol km-2 hr-1
                      gas_emis = cb05_per_megan(icount) * E_megan2(p_of_megan2cb05(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the
                      ! BEIS3.11 biogenic emissions module.
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_isop ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_isop-1)   = e_bio(i,j,p_isop-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_aacd ) THEN
                         e_bio(i,j,p_aacd-1)  = e_bio(i,j,p_aacd-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ald2 ) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_ald2-1)  = e_bio(i,j,p_ald2-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_aldx ) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_aldx-1)  = e_bio(i,j,p_aldx-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_apin ) THEN
                         ebio_api(i,j)        = ebio_api(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)  = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_bpin ) THEN
                         e_bio(i,j,p_terp-1)  = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ch4 ) THEN
                         e_bio(i,j,p_ch4-1)   = e_bio(i,j,p_ch4-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_co ) THEN
                         ebio_co(i,j)        = ebio_co(i,j)       + gas_emis
                         e_bio(i,j,p_co-1)    = e_bio(i,j,p_co-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_eth ) THEN
                         e_bio(i,j,p_eth-1)   = e_bio(i,j,p_eth-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_etha ) THEN
                         e_bio(i,j,p_etha-1)  = e_bio(i,j,p_etha-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_etoh ) THEN
                         e_bio(i,j,p_etoh-1)  = e_bio(i,j,p_etoh-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_facd ) THEN
                         e_bio(i,j,p_facd-1)  = e_bio(i,j,p_facd-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_form ) THEN
                         ebio_hcho(i,j)        = ebio_hcho(i,j)       + gas_emis
                         e_bio(i,j,p_form-1)  = e_bio(i,j,p_form-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_hum ) THEN
                         e_bio(i,j,p_terp-1)  = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_iole ) THEN
                         e_bio(i,j,p_iole-1)  = e_bio(i,j,p_iole-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_lim ) THEN
                         ebio_lim(i,j)        = ebio_lim(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)  = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_meoh ) THEN
                         e_bio(i,j,p_meoh-1)  = e_bio(i,j,p_meoh-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_nh3 ) THEN
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_no ) THEN
                         ebio_no(i,j)        = ebio_no(i,j)       + gas_emis
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_oci ) THEN
                         e_bio(i,j,p_terp-1)  = e_bio(i,j,p_terp-1) + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ole ) THEN
                         e_bio(i,j,p_ole-1)   = e_bio(i,j,p_ole-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_par ) THEN
                         e_bio(i,j,p_par-1)   = e_bio(i,j,p_par-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_terp ) THEN
                         ebio_terp(i,j)        = ebio_terp(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ter ) THEN
                         ebio_terp(i,j)        = ebio_terp(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_tol ) THEN
                         e_bio(i,j,p_tol-1)   = e_bio(i,j,p_tol-1)  + gas_emis*convert2
                      END IF

                   END IF !( p_in_chem > param_first_scalar )

                END IF
             END DO

          CASE (CB05_SORG_VBS_AQ_KPP)

             DO icount = 1, n_megan2cb05vbs
                IF ( p_of_cb05vbs (icount) .NE. non_react ) THEN
                   ! Get index to chem array for the corresponding CB05
                   ! species.
                   p_in_chem = p_of_cb05vbs(icount)

                   ! Check if the species is actually in the mechanism
                   ! (e.g., NH3 is in the mechanism only if aerosols
                   ! are simulated)
                   ! Check if the species is actually in the mechanism
                   IF ( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate for mechanism species in mol km-2 hr-1
                      gas_emis = cb05vbs_per_megan(icount) * E_megan2(p_of_megan2cb05vbs(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the
                      ! BEIS3.11 biogenic emissions module.
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_isop ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_isop-1)   = e_bio(i,j,p_isop-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_aacd ) THEN
                         e_bio(i,j,p_aacd-1)  = e_bio(i,j,p_aacd-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ald2 ) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_ald2-1)  = e_bio(i,j,p_ald2-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_aldx ) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_aldx-1)  = e_bio(i,j,p_aldx-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_apin ) THEN
                         ebio_api(i,j)        = ebio_api(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_bpin ) THEN
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ch4 ) THEN
                         e_bio(i,j,p_ch4-1)   = e_bio(i,j,p_ch4-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_co ) THEN
                         ebio_co(i,j)        = ebio_co(i,j)       + gas_emis
                         e_bio(i,j,p_co-1)    = e_bio(i,j,p_co-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_eth ) THEN
                         e_bio(i,j,p_eth-1)   = e_bio(i,j,p_eth-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_etha ) THEN
                         e_bio(i,j,p_etha-1)  = e_bio(i,j,p_etha-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_etoh ) THEN
                         e_bio(i,j,p_etoh-1)  = e_bio(i,j,p_etoh-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_facd ) THEN
                         e_bio(i,j,p_facd-1)  = e_bio(i,j,p_facd-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_form ) THEN
                         ebio_hcho(i,j)        = ebio_hcho(i,j)       + gas_emis
                         e_bio(i,j,p_form-1)  = e_bio(i,j,p_form-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_hum ) THEN
                          e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_iole ) THEN
                         e_bio(i,j,p_iole-1)  = e_bio(i,j,p_iole-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_lim ) THEN
                         ebio_lim(i,j)        = ebio_lim(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_meoh ) THEN
                         e_bio(i,j,p_meoh-1)  = e_bio(i,j,p_meoh-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_nh3 ) THEN
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_no ) THEN
                         ebio_no(i,j)        = ebio_no(i,j)       + gas_emis
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_oci ) THEN
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ole ) THEN
                         e_bio(i,j,p_ole-1)   = e_bio(i,j,p_ole-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_par ) THEN
                         e_bio(i,j,p_par-1)   = e_bio(i,j,p_par-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_terp ) THEN
                         ebio_terp(i,j)        = ebio_terp(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_ter ) THEN
                         ebio_terp(i,j)        = ebio_terp(i,j)       + gas_emis
                         e_bio(i,j,p_terp-1)   = e_bio(i,j,p_terp-1)  + gas_emis*convert2
                      END IF
                      IF ( p_in_chem .EQ. p_tol ) THEN
                         e_bio(i,j,p_tol-1)   = e_bio(i,j,p_tol-1)  + gas_emis*convert2
                      END IF

                   END IF !( p_in_chem > param_first_scalar )

                END IF
             END DO

          CASE (RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP,RACM_SOA_VBS_HET_KPP)

          DO icount = 1, n_megan2racmSOA

                IF ( p_of_racmSOA(icount) .NE. non_react ) THEN

                   ! Get index to chem array for the corresponding RACM-SOA-VBS-KPP
                   ! species.  
                   p_in_chem = p_of_racmSOA(icount)

                   ! Check if the species is actually in the mechanism
                   IF( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate of mechanism species in mol km-2 hr-1
                      gas_emis =  racmSOA_per_megan(icount) * E_megan2(p_of_megan2racmSOA(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the 
                      ! BEIS3.11 biogenic emissions module. 
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_iso ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_iso-1)   = e_bio(i,j,p_iso-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_oli) THEN
                         ebio_oli(i,j)        = ebio_oli(i,j)       + gas_emis
                         e_bio(i,j,p_oli-1)   = e_bio(i,j,p_oli-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_api) THEN
                         ebio_api(i,j)        = ebio_api(i,j)       + gas_emis
                         e_bio(i,j,p_api-1)   = e_bio(i,j,p_api-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_lim) THEN
                         ebio_lim(i,j)        = ebio_lim(i,j)       + gas_emis
                         e_bio(i,j,p_lim-1)   = e_bio(i,j,p_lim-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc3) THEN
                         ebio_hc3(i,j)        = ebio_hc3(i,j)       + gas_emis
                         e_bio(i,j,p_hc3-1)   = e_bio(i,j,p_hc3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ete) THEN
                         ebio_ete(i,j)        = ebio_ete(i,j)       + gas_emis
                         e_bio(i,j,p_ete-1)   = e_bio(i,j,p_ete-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_olt) THEN
                         ebio_olt(i,j)        = ebio_olt(i,j)       + gas_emis
                         e_bio(i,j,p_olt-1)   = e_bio(i,j,p_olt-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ket) THEN
                         ebio_ket(i,j)        = ebio_ket(i,j)       + gas_emis
                         e_bio(i,j,p_ket-1)   = e_bio(i,j,p_ket-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ald) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hcho) THEN
                         ebio_hcho(i,j)       = ebio_hcho(i,j)      + gas_emis
                         e_bio(i,j,p_hcho-1)  = e_bio(i,j,p_hcho-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_eth) THEN
                         ebio_eth(i,j)        = ebio_eth(i,j)       + gas_emis
                         e_bio(i,j,p_eth-1)   = e_bio(i,j,p_eth-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora2) THEN
                         ebio_ora2(i,j)       = ebio_ora2(i,j)      + gas_emis
                         e_bio(i,j,p_ora2-1)  = e_bio(i,j,p_ora2-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_co) THEN
                         ebio_co(i,j)         = ebio_co(i,j)        + gas_emis
                         e_bio(i,j,p_co-1)    = e_bio(i,j,p_co-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_no) THEN
                         ebio_no(i,j)         = ebio_no(i,j)        + gas_emis
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc5) THEN
                          e_bio(i,j,p_hc5-1)  = e_bio(i,j,p_hc5-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hc8) THEN
                          e_bio(i,j,p_hc8-1)  = e_bio(i,j,p_hc8-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora1) THEN
                          e_bio(i,j,p_ora1-1) = e_bio(i,j,p_ora1-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_sesq) THEN
                          ebio_sesq(i,j)      = ebio_sesq(i,j)      + gas_emis
                          e_bio(i,j,p_sesq-1) = e_bio(i,j,p_sesq-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_mbo) THEN
                          ebio_mbo(i,j)       = ebio_mbo(i,j)        + gas_emis
                          e_bio(i,j,p_mbo-1)  = e_bio(i,j,p_mbo-1)   + gas_emis*convert2
                      END IF

                   END IF !( p_in_chem > param_first_scalar )


                END IF !( p_of_racm(icount) .NE. non_react )

             END DO
          CASE (CBMZ, CBMZ_BB, CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, &
                CBMZ_MOSAIC_4BIN, &
                CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
                CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, &
                CBMZ_MOSAIC_DMS_4BIN_AQ,CBMZ_MOSAIC_DMS_8BIN_AQ,CBMZSORG, CBMZSORG_AQ, &
                CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)

             DO icount = 1, n_megan2cbmz

                IF ( p_of_cbmz (icount) .NE. non_react ) THEN

                   ! Get index to chem array for the corresponding CBMZ
                   ! species.  
                   p_in_chem = p_of_cbmz(icount)

                   ! Check if the species is actually in the mechanism
                   ! (e.g., NH3 is in the mechanism only if aerosols
                   ! are simulated)
                   IF( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate of mechanism species in mol km-2 hr-1
                      gas_emis = cbmz_per_megan(icount) * E_megan2(p_of_megan2cbmz(icount))


                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the 
                      ! BEIS3.11 biogenic emissions module. 
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_iso ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_iso-1)   = e_bio(i,j,p_iso-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_oli) THEN
                         ebio_oli(i,j)        = ebio_oli(i,j)       + gas_emis
                         e_bio(i,j,p_oli-1)   = e_bio(i,j,p_oli-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_olt) THEN
                         ebio_olt(i,j)        = ebio_olt(i,j)       + gas_emis
                         e_bio(i,j,p_olt-1)   = e_bio(i,j,p_olt-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ket) THEN
                         ebio_ket(i,j)        = ebio_ket(i,j)       + gas_emis
                         e_bio(i,j,p_ket-1)   = e_bio(i,j,p_ket-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ald) THEN
                         ebio_ald(i,j)        = ebio_ald(i,j)       + gas_emis
                         e_bio(i,j,p_ald-1)   = e_bio(i,j,p_ald-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hcho) THEN
                         ebio_hcho(i,j)       = ebio_hcho(i,j)      + gas_emis
                         e_bio(i,j,p_hcho-1)  = e_bio(i,j,p_hcho-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_eth) THEN
                         ebio_eth(i,j)        = ebio_eth(i,j)       + gas_emis
                         e_bio(i,j,p_eth-1)   = e_bio(i,j,p_eth-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora2) THEN
                         ebio_ora2(i,j)       = ebio_ora2(i,j)      + gas_emis
                         e_bio(i,j,p_ora2-1)  = e_bio(i,j,p_ora2-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_co) THEN
                         ebio_co(i,j)         = ebio_co(i,j)        + gas_emis
                         e_bio(i,j,p_co-1)    = e_bio(i,j,p_co-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_no) THEN
                         ebio_no(i,j)         = ebio_no(i,j)        + gas_emis   
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ol2) THEN
                          e_bio(i,j,p_ol2-1)  = e_bio(i,j,p_ol2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ora1) THEN
                          e_bio(i,j,p_ora1-1) = e_bio(i,j,p_ora1-1) + gas_emis*convert2
                      
                      ! SAN, 08/11/13 - adding missing CBMZ species to be mapped: 
                      ! missing: p_par, p_ch3oh, p_c2h5oh, p_nh3, p_tol
                      ELSE IF ( p_in_chem .EQ. p_par) THEN 
                         !ebio_par(i,j)        = ebio_par(i,j)       + gas_emis
                         e_bio(i,j,p_par-1)   = e_bio(i,j,p_par-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ch3oh) THEN	
                         ebio_ch3oh(i,j)      = ebio_ch3oh(i,j)     + gas_emis
                         e_bio(i,j,p_ch3oh-1) = e_bio(i,j,p_ch3oh-1)+ gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_c2h5oh) THEN	
                         ebio_c2h5oh(i,j)     = ebio_c2h5oh(i,j)      + gas_emis
                         e_bio(i,j,p_c2h5oh-1)= e_bio(i,j,p_c2h5oh-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_nh3) THEN	
                         ebio_nh3(i,j)        = ebio_nh3(i,j)       + gas_emis
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_tol) THEN	
                         ebio_tol(i,j)       = ebio_tol(i,j)        + gas_emis
                         e_bio(i,j,p_tol-1)  = e_bio(i,j,p_tol-1)   + gas_emis*convert2

                      END IF


                   END IF !( p_in_chem > param_first_scalar )
                   
                   
                END IF ! ( p_of_cbmz (icount) .NE. non_react )

             END DO
            
          CASE (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
               SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP)!BSINGH(12/03/2013): Added SAPRC 8 bin and non-aq on (04/07/2014) ! FIX FOR SAPRC99 AND SAPRC07

             DO icount = 1, n_megan2saprcnov

                IF ( p_of_saprcnov(icount) .NE. non_react ) THEN

                   ! Get index to chem array for the corresponding RADM2
                   ! species.
                   p_in_chem = p_of_saprcnov(icount)

                   ! Check if the species is actually in the mechanism
                   IF ( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate for mechanism species in mol km-2 hr-1
                      gas_emis = saprcnov_per_megan(icount) * E_megan2(p_of_megan2saprcnov(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the
                      ! BEIS3.11 biogenic emissions module.
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      IF ( p_in_chem .EQ. p_isoprene ) THEN
                         ebio_iso(i,j)        = ebio_iso(i,j)       + gas_emis
                         e_bio(i,j,p_isoprene-1)   = e_bio(i,j,p_isoprene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_terp) THEN
                         ebio_api(i,j)       = ebio_api(i,j)      + gas_emis
                         e_bio(i,j,p_terp-1)  = e_bio(i,j,p_terp-1) + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_sesq) THEN
                         ebio_lim(i,j)         = ebio_lim(i,j)        + gas_emis
                         e_bio(i,j,p_sesq-1)    = e_bio(i,j,p_sesq-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_no) THEN
                         ebio_no(i,j)         = ebio_no(i,j)        + gas_emis
                         e_bio(i,j,p_no-1)    = e_bio(i,j,p_no-1)   + gas_emis*convert2
!jdf
                      ELSE IF ( p_in_chem .EQ. p_alk3) THEN
                         ebio_alk3(i,j)         = ebio_alk3(i,j)        + gas_emis
                         e_bio(i,j,p_alk3-1)    = e_bio(i,j,p_alk3-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_alk4) THEN
                         ebio_alk4(i,j)         = ebio_alk4(i,j)        + gas_emis
                         e_bio(i,j,p_alk4-1)    = e_bio(i,j,p_alk4-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_alk5) THEN
                         ebio_alk5(i,j)         = ebio_alk5(i,j)        + gas_emis
                         e_bio(i,j,p_alk5-1)    = e_bio(i,j,p_alk5-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ole1) THEN
                         ebio_ole1(i,j)         = ebio_ole1(i,j)        + gas_emis
                         e_bio(i,j,p_ole1-1)    = e_bio(i,j,p_ole1-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ole2) THEN
                         ebio_ole2(i,j)         = ebio_ole2(i,j)        + gas_emis
                         e_bio(i,j,p_ole2-1)    = e_bio(i,j,p_ole2-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_aro1) THEN
                         ebio_aro1(i,j)         = ebio_aro1(i,j)        + gas_emis
                         e_bio(i,j,p_aro1-1)    = e_bio(i,j,p_aro1-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_aro2) THEN
                         ebio_aro2(i,j)         = ebio_aro2(i,j)        + gas_emis
                         e_bio(i,j,p_aro2-1)    = e_bio(i,j,p_aro2-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_acet) THEN
                         ebio_acet(i,j)         = ebio_acet(i,j)        + gas_emis
                         e_bio(i,j,p_acet-1)    = e_bio(i,j,p_acet-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hcho) THEN
                         ebio_hcho(i,j)         = ebio_hcho(i,j)        + gas_emis
                         e_bio(i,j,p_hcho-1)    = e_bio(i,j,p_hcho-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ccho) THEN
                         ebio_ccho(i,j)         = ebio_ccho(i,j)        + gas_emis
                         e_bio(i,j,p_ccho-1)    = e_bio(i,j,p_ccho-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_mek) THEN
                         ebio_mek(i,j)         = ebio_mek(i,j)        + gas_emis
                         e_bio(i,j,p_mek-1)    = e_bio(i,j,p_mek-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_c2h6) THEN
                         ebio_c2h6(i,j)         = ebio_c2h6(i,j)        + gas_emis
                         e_bio(i,j,p_c2h6-1)    = e_bio(i,j,p_c2h6-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_c3h6) THEN
                         ebio_c3h6(i,j)         = ebio_c3h6(i,j)        + gas_emis
                         e_bio(i,j,p_c3h6-1)    = e_bio(i,j,p_c3h6-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_c3h8) THEN
                         ebio_c3h8(i,j)         = ebio_c3h8(i,j)        + gas_emis
                         e_bio(i,j,p_c3h8-1)    = e_bio(i,j,p_c3h8-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_ethene) THEN
                         ebio_ethene(i,j)         = ebio_ethene(i,j)        + gas_emis
                         e_bio(i,j,p_ethene-1)    = e_bio(i,j,p_ethene-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_bald) THEN
                         ebio_bald(i,j)         = ebio_bald(i,j)        + gas_emis
                         e_bio(i,j,p_bald-1)    = e_bio(i,j,p_bald-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_meoh) THEN
                         ebio_meoh(i,j)         = ebio_meoh(i,j)        + gas_emis
                         e_bio(i,j,p_meoh-1)    = e_bio(i,j,p_meoh-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_hcooh) THEN
                         ebio_hcooh(i,j)         = ebio_hcooh(i,j)        + gas_emis
                         e_bio(i,j,p_hcooh-1)    = e_bio(i,j,p_hcooh-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_rco_oh) THEN
                         ebio_rco_oh(i,j)         = ebio_rco_oh(i,j)        + gas_emis
                         e_bio(i,j,p_rco_oh-1)    = e_bio(i,j,p_rco_oh-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_terp) THEN
                         ebio_terp(i,j)         = ebio_terp(i,j)        + gas_emis
                         ebio_api(i,j)         = ebio_api(i,j)        + gas_emis
                         e_bio(i,j,p_terp-1)    = e_bio(i,j,p_terp-1)   + gas_emis*convert2
                      ELSE IF ( p_in_chem .EQ. p_sesq) THEN
                         ebio_sesq(i,j)         = ebio_sesq(i,j)        + gas_emis
                         ebio_lim(i,j)         = ebio_lim(i,j)        + gas_emis
                         e_bio(i,j,p_sesq-1)    = e_bio(i,j,p_sesq-1)   + gas_emis*convert2
!jdf
                      END IF

                   END IF !( p_in_chem > param_first_scalar )

                END IF !( p_of_saprcnov(icount) .NE. non_react )

             END DO

          CASE ( CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP )

             DO icount = 1, n_megan2crimech
                IF ( p_of_crimech(icount) .NE. non_react ) THEN

                   ! Get index to chem array for the corresponding crimech
                   ! species.  
                   p_in_chem = p_of_crimech(icount)
                   
                   ! Check if the species is actually in the mechanism
                   IF( p_in_chem >= param_first_scalar ) THEN

                      ! Emission rate of mechanism species in mol km-2 hr-1
                      gas_emis =  crimech_per_megan(icount) * E_megan2(p_of_megan2crimech(icount))

                      ! Add emissions to diagnostic output variables.
                      ! ebio_xxx (mol km-2 hr-1) were originally used by the 
                      ! BEIS3.11 biogenic emissions module. 
                      ! I have also borrowed variable e_bio (ppm m min-1).
                      
                      IF ( p_in_chem == p_c5h8 ) THEN
                         ebio_c5h8(i,j) = ebio_c5h8(i,j) + gas_emis
                         e_bio(i,j,p_c5h8-1)   = e_bio(i,j,p_c5h8-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_no ) THEN
                         ebio_no(i,j)  = ebio_no(i,j) + gas_emis
                         e_bio(i,j,p_no-1)   = e_bio(i,j,p_no-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_no2 ) THEN
                         ebio_no2(i,j)  = ebio_no2(i,j) + gas_emis
                         e_bio(i,j,p_no2-1)   = e_bio(i,j,p_no2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_co ) THEN
                         ebio_co(i,j)  = ebio_co(i,j) + gas_emis
                         e_bio(i,j,p_co-1)   = e_bio(i,j,p_co-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_hcho ) THEN
                         ebio_hcho(i,j) = ebio_hcho(i,j) + gas_emis
                         e_bio(i,j,p_hcho-1)   = e_bio(i,j,p_hcho-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ket ) THEN
                         ebio_ket(i,j) = ebio_ket(i,j) + gas_emis
                         e_bio(i,j,p_ket-1)   = e_bio(i,j,p_ket-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_toluene ) THEN
                         ebio_toluene(i,j) = ebio_toluene(i,j) + gas_emis
                         e_bio(i,j,p_toluene-1)   = e_bio(i,j,p_toluene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_apinene ) THEN
                         ebio_apinene(i,j) = ebio_apinene(i,j) + gas_emis
                         e_bio(i,j,p_apinene-1)   = e_bio(i,j,p_apinene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_bpinene ) THEN
                         ebio_bpinene(i,j) = ebio_bpinene(i,j) + gas_emis
                         e_bio(i,j,p_bpinene-1)   = e_bio(i,j,p_bpinene-1)  + gas_emis*convert2                         
                      ELSE IF ( p_in_chem == p_so2 ) THEN
                         ebio_so2(i,j) = ebio_so2(i,j) + gas_emis
                         e_bio(i,j,p_so2-1)   = e_bio(i,j,p_so2-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_dms ) THEN
                         ebio_dms(i,j) = ebio_dms(i,j) + gas_emis
                         e_bio(i,j,p_dms-1)   = e_bio(i,j,p_dms-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_nc4h10 ) THEN
                         ebio_nc4h10(i,j) = ebio_nc4h10(i,j) + gas_emis
                         e_bio(i,j,p_nc4h10-1)   = e_bio(i,j,p_nc4h10-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_tbut2ene ) THEN
                         ebio_tbut2ene(i,j) = ebio_tbut2ene(i,j) + gas_emis
                         e_bio(i,j,p_tbut2ene-1)   = e_bio(i,j,p_tbut2ene-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_nh3 ) THEN
                         ebio_nh3(i,j) = ebio_nh3(i,j) + gas_emis
                         e_bio(i,j,p_nh3-1)   = e_bio(i,j,p_nh3-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3oh ) THEN
                         ebio_ch3oh(i,j) = ebio_ch3oh(i,j) + gas_emis
                         e_bio(i,j,p_ch3oh-1)   = e_bio(i,j,p_ch3oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h5oh ) THEN
                         ebio_c2h5oh(i,j) = ebio_c2h5oh(i,j) + gas_emis
                         e_bio(i,j,p_c2h5oh-1)   = e_bio(i,j,p_c2h5oh-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_ch3co2h ) THEN
                         ebio_ch3co2h(i,j) = ebio_ch3co2h(i,j) + gas_emis
                         e_bio(i,j,p_ch3co2h-1)   = e_bio(i,j,p_ch3co2h-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_mek ) THEN
                         ebio_mek(i,j) = ebio_mek(i,j) + gas_emis
                         e_bio(i,j,p_mek-1)   = e_bio(i,j,p_mek-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h4 ) THEN
                         ebio_c2h4(i,j) = ebio_c2h4(i,j) + gas_emis
                         e_bio(i,j,p_c2h4-1)   = e_bio(i,j,p_c2h4-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c2h6 ) THEN
                         ebio_c2h6(i,j) = ebio_c2h6(i,j) + gas_emis
                         e_bio(i,j,p_c2h6-1)   = e_bio(i,j,p_c2h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h6 ) THEN
                         ebio_c3h6(i,j) = ebio_c3h6(i,j) + gas_emis
                         e_bio(i,j,p_c3h6-1)   = e_bio(i,j,p_c3h6-1)  + gas_emis*convert2
                      ELSE IF ( p_in_chem == p_c3h8 ) THEN
                         ebio_c3h8(i,j) = ebio_c3h8(i,j) + gas_emis
                         e_bio(i,j,p_c3h8-1)   = e_bio(i,j,p_c3h8-1)  + gas_emis*convert2                         
                      ELSE IF ( p_in_chem == p_ch3cho ) THEN
                         ebio_ch3cho(i,j) = ebio_ch3cho(i,j) + gas_emis
                         e_bio(i,j,p_ch3cho-1)   = e_bio(i,j,p_ch3cho-1)  + gas_emis*convert2                        
                      ELSE IF ( p_in_chem == p_hcooh ) THEN
                         ebio_hcooh(i,j) = ebio_hcooh(i,j) + gas_emis
                         e_bio(i,j,p_hcooh-1)   = e_bio(i,j,p_hcooh-1)  + gas_emis*convert2                         
                      END IF

                   END IF !( p_in_chem > param_first_scalar )
                   

                END IF !( p_of_crimech(icount) .NE. non_react )

             END DO


 
             CASE DEFAULT

                CALL wrf_error_fatal('Species conversion table for MEGAN v2.04 not available. ')

             END SELECT GAS_MECH_SELECT



       END DO i_loop ! i = its, ite
    END DO j_loop    ! j = jts, jte


  CONTAINS

    ! -----------------------------------------------------------------
    !  SUBROUTINE GAMMA_TISOP returns the GAMMA_T value for isoprene
    !  Orginally from Tan's gamma_etc.F
    ! -----------------------------------------------------------------

    SUBROUTINE GAMMA_TISOP( TEMP, D_TEMP, gam_T )
      !
      !  Description :
      !
      !    MEGAN biogenic emissions adjustment factor for temperature
      !    for isoprene
      !
      !  Reference: 
      !
      !    Estimates of global terrestial isoprene emissions using MEGAN
      !    (Model of Emissions of Gases and Aerosols from Nature )
      !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, 
      !    P.I. Palmer, and C. Geron
      !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006      !
      !

      IMPLICIT NONE

      ! hourly surface air temperature (K)
      ! (here use instantaneous temperature
      REAL, INTENT(IN)  :: TEMP
      ! daily-mean surface airtemperature (K)
      ! (here use the previous month's monthly mean)
      REAL, INTENT(IN)  :: D_TEMP
      !temperature adjustment factor
      REAL, INTENT(OUT) :: gam_T

      ! Local parameters
      REAL :: Eopt, Topt, X
      REAL :: AAA, BBB
      REAL, PARAMETER :: CT1 = 80.0
      REAL, PARAMETER :: CT2 = 200.0
      
      ! End header ----------------------------------------------------

      ! Below Eqn (14) of Guenther et al. [2006]
      ! (assuming T_daily = D_TEMP)
      Eopt = 1.75 * EXP(0.08*(D_TEMP-297.0))

      ! Eqn (8) of Guenther et al. [2006]
      ! (assuming T_daily = D_TEMP)
      Topt = 313.0 + ( 0.6*(D_TEMP-297.0) )

      ! Eqn (5) of Guenther et al. [2006]
      X = ( (1.0/Topt)-(1.0/TEMP) ) / 0.00831
      AAA = Eopt*CT2*EXP(CT1*X)
      BBB = (  CT2-CT1*( 1.0-EXP(CT2*X) )  )
      gam_T = AAA/BBB

    END SUBROUTINE GAMMA_TISOP

    ! -----------------------------------------------------------------
    ! SUBROUITINE GAMMA_TNISP returns the GAMMA_T value for 
    ! non-isoprene species
    ! Originally from Tan's gamma_etc.F
    !------------------------------------------------------------------

    SUBROUTINE GAMMA_TNISP( SPCNUM, TEMP, gam_T )
      !
      !  Description :
      !
      !    MEGAN biogenic emissions adjustment factor for temperature
      !    for non-isoprene species.
      !
      !  Reference:
      !
      !    MEGAN v2.0 Documentation
      !
      ! Method:
      !
      !    GAMMA_T =  exp[BETA*(T-Ts)]
      !      where BETA   = temperature dependent parameter
      !            Ts     = standard temperature (normally 303K, 30C)
      !

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: SPCNUM               ! Species number
      REAL, INTENT(IN)    :: TEMP
      REAL, INTENT(OUT)   :: gam_T
      REAL, PARAMETER     :: Ts = 303.0
      
      ! End header ----------------------------------------------------

      ! TDF_PRM is defined in module_data_megan2.F
      gam_T = EXP( TDF_PRM(SPCNUM)*(TEMP-Ts) )

    END SUBROUTINE GAMMA_TNISP


    ! --------------------------------------------------------------------
    ! SUBROUTINE GAMMA_LAI
    ! Originally from Tan's gamma_etc.F
    ! --------------------------------------------------------------------

    SUBROUTINE GAMMA_LAI(LAI, gam_L )
      !  Description :
      !
      !    MEGAN biogenic emissions adjustment factor for leaf area
      !    index
      !
      !  Reference: 
      !
      !    Estimates of global terrestial isoprene emissions using MEGAN
      !    (Model of Emissions of Gases and Aerosols from Nature )
      !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, 
      !    P.I. Palmer, and C. Geron
      !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006      !
      !
      ! Method:
      !                       0.49[LAI]
      !        GAMMA_LAI = ----------------    (dimensionless)
      !                    (1+0.2LAI^2)^0.5
      !

      IMPLICIT NONE
      REAL, INTENT(IN)  ::  LAI 
      REAL, INTENT(OUT) :: gam_L

      ! End header ----------------------------------------------------

      
      ! Eqn (15) of Guenther et al. [2006]
      gam_L = (0.49*LAI) / ( SQRT(1.0+0.2*(LAI**2)) )

      RETURN
    END SUBROUTINE GAMMA_LAI

    !-------------------------------------------------------------------
    ! SUBROUTINE GAMMA_P 
    ! Originally from Tan's gamma_etc.F
    !-------------------------------------------------------------------
    SUBROUTINE GAMMA_P(             &
         DOY_in, tmidh, LAT, LONG,  &                    
         PPFD, D_PPFD, gam_P        &
         )
      !
      !  Description :
      !
      !    MEGAN biogenic emissions adjustment factor for
      !    photosynthetic photon flux density (PPFD or PAR)
      !
      !  Reference: 
      !
      !    Estimates of global terrestial isoprene emissions using MEGAN
      !    (Model of Emissions of Gases and Aerosols from Nature )
      !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, 
      !    P.I. Palmer, and C. Geron
      !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006      
      !  
      !  Method:
      !
      !    GAMMA_P = 0.0         sin(a)<=0
      !
      !    GAMMA_P = sin(a)[2.46*0.9*PHI^3*(1+0.0005(Pdaily-400))]
      !                                  0<a<180
      !           where PHI = above canopy PPFD transmission (non-dimension)
      !           Pdaily    = daily average above canopy PPFD (umol/m2s)
      !              a      = solar angle (degree)
      !
      !         Note: AAA = 2.46*BBB*PHI-0.9*PHI^2
      !               BBB = (1+0.0005(Pdaily-400))
      !           GAMMA_P = sin(a)*AAA
      !
      !                       Pac
      !             PHI = -----------
      !                   sin(a)*Ptoa
      !
      !     where Pac  = above canopy PPFD (umol/m2s)
      !                 Ptoa = PPFD at the top of atmosphere (umol/m2s)
      !
      !             Pac =  SRAD * 4.766 mmmol/m2-s * 0.5
      !
      !             Ptoa = 3000 + 99*cos[2*3.14-( DOY-10)/365 )]
      !        where DOY = day of year (julian day)
      !
      ! NOTE: This code has been corrected. The gamma P equation as defined in the
      ! original Guenther et al., 2006 (equation 11b) is incorrect. This has the
      ! corrected algorithm. (CW, 08/16/2007)
      !-----------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: DOY_in ! julian day at GMT

      ! GMT hour plus minutes (in fractaionl hour) of the middle
      ! of the current time step
      REAL, INTENT(IN)  :: tmidh
      REAL, INTENT(IN)  ::  LAT    ! Latitude [=] degrees
      REAL, INTENT(IN)  ::  LONG   ! Longitude [=] degrees
      REAL, INTENT(IN)  ::  PPFD   ! Photosynthetic Photon Flus Density
      REAL, INTENT(IN)  ::  D_PPFD ! Daily PPFD
      REAL, INTENT(OUT) ::  gam_P  ! GAMMA_P


      !...  Local scalars
      INTEGER :: DOY                 ! local julian day
      REAL :: HOUR                   ! solar hour
      REAL :: AAA, BBB
      REAL :: SIN_solarangle         ! sin(solar angle)
      REAL :: Ptoa, Pac, PHI

      ! End header ----------------------------------------------------

      ! Convert time of the middle of the current time step
      ! from GMT to solar hour (include minutes in decimals)
      DOY = DOY_in
      HOUR = tmidh + long/15.
      IF ( HOUR .LT. 0.0 ) THEN
         HOUR = HOUR + 24.0
         DOY  = DOY - 1
      ENDIF

      ! Above canopy photosynthetic photo flux density (PPFD)
      ! ( micromole/m2/s )
      Pac = PPFD
 
      ! Get sin of solar elevation angle
      CALL SOLARANGLE( DOY, HOUR, LAT, SIN_solarangle )

      ! Calculate gamma_p in Eqn (10) of Guenther et al. [2006]
      IF ( SIN_solarangle .LE. 0.0 ) THEN
         ! Eqn (11a) of Guenther et al. [2006]
         gam_P = 0.0
      ELSE
         ! PPFD at top of the atmosphere
         ! Eqn (13) of Guenther et al. [2006]
         ! ( micromole/m2/s )
         Ptoa = 3000.0 + 99.0 * COS( 2.*3.14*(DOY-10.)/365. )
         ! Above canopy PPFD transmission
         ! Eqn (12) of Guenther et al. [2006]
         ! (nondimensional)
         PHI = Pac/(SIN_solarangle * Ptoa)
         ! Eqn (11b) of Guenther et al. [2006]
         ! (Note: typo in the paper; correction made 08/06/2007)
         BBB = 1. + 0.0005*( D_PPFD-400. )
         AAA = 2.46 * BBB * PHI - 0.9 * (PHI**2)
         gam_P = SIN_solarangle * AAA

      ENDIF
      ! Screening the unforced errors
      ! IF solar elevation angle is less than 1 THEN
      ! gamma_p can not be greater than 0.1.
      IF (SIN_solarangle .LE. 0.0175 .AND. gam_P .GT. 0.1) THEN
         gam_P = 0.1
      ENDIF


    END SUBROUTINE GAMMA_P

    ! ----------------------------------------------------------------
    ! SUBROUTINE GAMMA_A returns GAMMA_A
    ! Originally from Tan's gamma_etc.F
    !------------------------------------------------------------------
    SUBROUTINE GAMMA_A( i_spc, LAIp, LAIc, TSTLEN, D_TEMP, gam_A )
      !  Description :
      !
      !    MEGAN biogenic emissions adjustment factor for leaf age
      !
      !  Reference: 
      !
      !    Estimates of global terrestial isoprene emissions using MEGAN
      !    (Model of Emissions of Gases and Aerosols from Nature )
      !    A. Guenther, T. Karl, P. Harley, C. Wiedinmyer, 
      !    P.I. Palmer, and C. Geron
      !    Atmospheric Chemistry and Physics, 6, 3181-3210, 2006
      !
      !    MEGAN v2.0 Documentation
      !
      !
      ! Method:
      !
      !     GAMMA_age = Fnew*Anew + Fgro*Agro + Fmat*Amat + Fold*Aold
      !      where Fnew = new foliage fraction
      !            Fgro = growing foliage fraction
      !                 Fmat = mature foliage fraction
      !                 Fold = old foliage fraction
      !                 Anew = relative emission activity for new foliage
      !                 Agro = relative emission activity for growing foliage
      !                 Amat = relative emission activity for mature foliage
      !                 Aold = relative emission activity for old foliage
      !
      !
      !             For foliage fraction
      !             Case 1) LAIc = LAIp
      !             Fnew = 0.0  , Fgro = 0.1  , Fmat = 0.8  , Fold = 0.1
      !
      !             Case 2) LAIp > LAIc
      !             Fnew = 0.0  , Fgro = 0.0
      !             Fmat = 1-Fold
      !             Fold = (LAIp-LAIc)/LAIp
      !
      !             Case 3) LAIp < LAIc
      !             Fnew = 1-(LAIp/LAIc)                       t <= ti
      !                  = (ti/t) * ( 1-(LAIp/LAIc) )          t >  ti
      !
      !             Fmat = LAIp/LAIc                           t <= tm
      !                  = (LAIp/LAIc) +
      !                      ( (t-tm)/t ) * ( 1-(LAIp/LAIc) )  t >  tm
      !
      !             Fgro = 1 - Fnew - Fmat
      !             Fold = 0.0
      !
      !           where
      !             ti = 5 + (0.7*(300-Tt))                   Tt <= 303
      !                = 2.9                                  Tt >  303
      !             tm = 2.3*ti
      !
      !             t  = length of the time step (days)
      !             ti = number of days between budbreak and the induction of
      !                  emission
      !             tm = number of days between budbreak and the initiation of
      !                  peak emissions rates
      !             Tt = average temperature (K) near top of the canopy during
      !                  current time period (daily ave temp for this case)
      !
      !
      !             For relative emission activity
      !             Case 1) Constant
      !             Anew = 1.0  , Agro = 1.0  , Amat = 1.0  , Aold = 1.0
      !
      !             Case 2) Monoterpenes
      !             Anew = 2.0  , Agro = 1.8  , Amat = 0.95 , Aold = 1.0
      !
      !             Case 3) Sesquiterpenes
      !             Anew = 0.4  , Agro = 0.6  , Amat = 1.075, Aold = 1.0
      !
      !             Case 4) Methanol
      !             Anew = 3.0  , Agro = 2.6  , Amat = 0.85 , Aold = 1.0
      !
      !             Case 5) Isoprene
      !             Anew = 0.05 , Agro = 0.6  , Amat = 1.125, Aold = 1.0


      IMPLICIT NONE

      ! SUBROUTINE arguments

      !..."Pointer" for class of species
      INTEGER, INTENT(IN) :: i_spc
      !...average temperature of the previous 24-hours
      REAL, INTENT(IN) :: D_TEMP
      !...leaf area index of the current and previous
      !...month
      REAL, INTENT(IN) :: LAIp, LAIc
      !...time step between LAIc and LAIp (days)
      REAL, INTENT(IN) ::     TSTLEN
      !...emissions adjustment factor accounting for leaf age
      REAL, INTENT(OUT) :: gam_A

      ! Local scalars

      !...leaf age fractions
      REAL :: Fnew, Fgro, Fmat, Fold
      !...relative emission activity index
      INTEGER ::  AINDX 
      !...time step between LAIC and LAIp (days)
      INTEGER :: t 
      !...number of days between budbreak and the induction emission
      REAL     ti
      !...number of days between budbreak  and the initiation of peak
      !...emissions rates
      REAL     tm
      !

      REAL     Tt                   ! average temperature (K)
      ! daily ave temp

      ! End header ----------------------------------------------------

      ! Choose relative emission activity class
      ! See Table 2 of MEGAN v2.0 Documentation
      !

      IF (      (i_spc==imgn_acto) .OR. (i_spc==imgn_acta) .OR. (i_spc==imgn_form)   &
           .OR. (i_spc==imgn_ch4)  .OR. (i_spc==imgn_no)   .OR. (i_spc==imgn_co)     &
           ) THEN
         AINDX = 1

      ELSE IF ( (i_spc==imgn_myrc) .OR. (i_spc==imgn_sabi) .OR. (i_spc==imgn_limo)   &
           .OR. (i_spc==imgn_3car) .OR. (i_spc==imgn_ocim) .OR. (i_spc==imgn_bpin)   &
           .OR. (i_spc==imgn_apin) .OR. ( i_spc==imgn_omtp)                          &
           ) THEN
         AINDX = 2

      ELSE IF ( (i_spc==imgn_afarn) .OR. (i_spc==imgn_bcar) .OR. (i_spc==imgn_osqt)  &
           ) THEN
         AINDX = 3

      ELSE IF (i_spc==imgn_meoh) THEN
         aindx = 4

      ELSE IF ( (i_spc==imgn_isop) .OR. (i_spc==imgn_mbo) ) THEN
         aindx = 5
      ELSE
         WRITE(mesg,fmt = '("Invalid i_spc in SUBROUTINE GAMMA_A; i_spc = ", I3)') i_spc
         CALL wrf_error_fatal(mesg)
      END IF



      ! Time step between LAIp and LAIc (days)
      t = TSTLEN
      ! Tt is the average ambient air temperature (K) of the preceeding time
      ! interval.  Here, use the monthly-mean surface air temperature
      Tt   = D_TEMP

      ! Calculate foliage fraction
      ! (section 3.2.2 of Guenther et al. [2006])
      IF (LAIp .EQ. LAIc) THEN
         Fnew = 0.0
         Fgro = 0.1
         Fmat = 0.8
         Fold = 0.1
      ELSEIF (LAIp .GT. LAIc) THEN
         Fnew = 0.0
         Fgro = 0.0
         Fold = ( LAIp-LAIc ) / LAIp
         Fmat = 1.0-Fold
      ELSE ! LAIp < LAIc
         ! Calculate ti, which is the number of days between budbreak and
         ! the induction of isoprene emission.
         IF (Tt .LE. 303.0) THEN
            ! Eqn (18a) of Guenther et al. [2006]
            ti = 5.0 + 0.7*(300.0-Tt)
         ELSE
            ! Eqn (18b) of Guenther et al. [2006]
            ti = 2.9
         ENDIF
         ! tm is the number of days between budbreak and the initiation
         ! of peak isoprene emissions rates.
         ! Eqn (19) of Guenther et al. [2006]
         tm = 2.3*ti

         ! Calculate Fnew and Fmat, then Fgro and Fold
         !  Fnew
         IF (t .LE. ti) THEN
            ! Eqn (17a) of Guenther et al. [2006]
            Fnew = 1.0 - (LAIp/LAIc)
         ELSE
            ! Eqn (17b) of Guenther et al. [2006]
            Fnew = (ti/t) * ( 1-(LAIp/LAIc) )
         ENDIF

         ! Fmat
         IF (t .LE. tm) THEN
            ! Eqn (17c) of Guenther et al. [2006]
            Fmat = LAIp/LAIc
         ELSE
            ! Eqn (17d) of Guenther et al. [2006]
            Fmat = (LAIp/LAIc) + ( (t-tm)/t ) * ( 1-(LAIp/LAIc) )
         ENDIF

         Fgro = 1.0 - Fnew - Fmat
         Fold = 0.0

      ENDIF

      !Calculate GAMMA_A
      ! Anew, Agro, Amat, Aold are defined in module_data_megan2.F
      gam_A = Fnew*Anew(AINDX) + Fgro*Agro(AINDX)    &
           + Fmat*Amat(AINDX) + Fold*Aold(AINDX)


    END SUBROUTINE GAMMA_A

    ! ----------------------------------------------------------------
    ! SUBROUTINE SOLARANGLE calculates the solar angle
    ! Originally from Tan's solarangle.F
    !------------------------------------------------------------------
    SUBROUTINE SOLARANGLE( DAY, SHOUR, LAT, SIN_solarangle )
      !
      !
      !   Input:
      !            1) Day of year
      !            2) Latitude
      !            3) Hour
      !
      !   Output: sin of solar angle
      !

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN) :: DAY                  ! DOY or julian day
      REAL, INTENT(IN)    :: SHOUR                ! Solar hour
      REAL, INTENT(IN)    :: LAT                  ! Latitude
      REAL, INTENT(OUT)   :: SIN_solarangle

      ! Local scalars
      REAL    :: sindelta, cosdelta, A, B

      ! End header -----------------------------------------------------

      sindelta = -SIN(0.40907) * COS( 6.28*(REAL(DAY,KIND(0.))+10.)/365. )
      cosdelta = SQRT(1.-sindelta**2.)

      A = SIN( LAT*D2RAD ) * sindelta
      B = COS( LAT*D2RAD ) * cosdelta

      SIN_solarangle = A + B * COS(2.*PI*(SHOUR-12.)/24.)


    END SUBROUTINE SOLARANGLE




  END SUBROUTINE bio_emissions_megan2

END MODULE module_bioemi_megan2
