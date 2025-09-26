MODULE GOCART_DUST_AFWA
!
! AFWA dust routine
! Created by Sandra Jones (AER and AFWA) and Glenn Creighton (AFWA).
!

  USE module_data_gocart_dust

  IMPLICIT NONE  

  INTRINSIC max, min

CONTAINS
  SUBROUTINE gocart_dust_afwa_driver(ktau,dt,config_flags,julday,alt,t_phy,moist,u_phy,  &
         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,erod_dri,dustin,snowh,zs,   &
         ivgtyp,isltyp,vegfra,lai_vegmask,xland,xlat,xlong,gsw,dx,g,emis_dust,      &
         ust,znt,clay_wrf,sand_wrf,clay_nga,sand_nga,afwa_dustloft,                 &
         tot_dust,tot_edust,vis_dust,alpha,gamma,smtune,ustune,            &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  USE module_configure
  USE module_state_description
  USE module_data_sorgam, ONLY: factnuma,factnumc,soilfac

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: julday, ktau,                            &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp,               &
                                                     isltyp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                              moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                           chem
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_dust),OPTIONAL,           &
         INTENT(INOUT ) ::                                                 &
         emis_dust
   REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,     &
         INTENT(IN   ) ::                            smois
   REAL, DIMENSION( config_flags%num_soil_layers ) ,                       &
         INTENT(IN   ) ::                            zs
   REAL, DIMENSION( ims:ime , jms:jme, ndcls )             ,               &
         INTENT(IN   ) ::                            erod,erod_dri
   REAL, DIMENSION( ims:ime , jms:jme, 5 )                 ,               &
         INTENT(INOUT) ::                            dustin
   REAL, DIMENSION( ims:ime , jms:jme )                    ,               &
         INTENT(IN   ) ::                                                  &
                                                     u10,                  &
                                                     v10,                  &
                                                     gsw,                  &
                                                     vegfra,               &
                                                     lai_vegmask,          &
                                                     xland,                &
                                                     xlat,                 &
                                                     xlong,                &
                                                     ust,                  &
                                                     znt,                  &
                                                     clay_wrf,             &
                                                     sand_wrf,             &
                                                     clay_nga,             &
                                                     sand_nga,             &
                                                     snowh
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                         &
         INTENT(IN   ) ::                                                  &
                                                     alt,                  &
                                                     t_phy,                &
                                                     dz8w,p8w,             &
                                                     u_phy,v_phy,rho_phy
  REAL, DIMENSION( ims:ime , jms:jme )                    ,                &
         INTENT(  OUT) ::                            afwa_dustloft,        &
                                                     tot_edust
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                          &
         INTENT(  OUT) ::                            tot_dust,             &
                                                     vis_dust
  REAL, INTENT(IN   ) :: dt,dx,g

  ! Local variables

  INTEGER :: nmx,smx,i,j,k,imx,jmx,lmx,lhave
  INTEGER,DIMENSION (1,1) :: ilwi
  REAL*8, DIMENSION (1,1) :: erodtot
  REAL*8, DIMENSION (1,1) :: vegmask
  REAL*8, DIMENSION (1,1) :: gravsm
  REAL, DIMENSION( ims:ime , jms:jme ) :: clay,sand
  REAL*8, DIMENSION (1,1) :: drylimit
  REAL*8, DIMENSION (5)   :: tc,bems
  REAL*8, DIMENSION (1,1) :: airden,airmas,ustar
  REAL*8, DIMENSION (1) :: dxy
  REAL*8, DIMENSION (3) :: massfrac
  REAL*8                :: volsm
  REAL :: conver,converi
  REAL :: psi,ustart,w10
  REAL*8 :: zwant
  REAL, INTENT(IN   ) :: alpha, gamma, smtune, ustune
  INTEGER :: smois_opt

  conver=1.e-9
  converi=1.e9

  ! Number of dust bins

  imx=1
  jmx=1
  lmx=1
  nmx=ndust
  smx=ngsalt

  k=kts
  DO j=jts,jte
  DO i=its,ite

    ! Masked value for afwa_dustloft

    afwa_dustloft(i,j)=-99.

    ! Don't do dust over water!!!

    IF (xland(i,j) .lt. 1.5) THEN
      ilwi(1,1)=1

      ! Total concentration at lowest model level. This is still hardcoded
      ! for 5 bins.

      IF(config_flags%chem_opt == CB05_SORG_AQ_KPP .or. &
         config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP ) then
        tc(1)=0.0
        tc(2)=0.0
        tc(3)=0.0
        tc(4)=0.0
        tc(5)=0.0
      ELSE
        tc(1)=chem(i,kts,j,p_dust_1)*conver
        tc(2)=chem(i,kts,j,p_dust_2)*conver
        tc(3)=chem(i,kts,j,p_dust_3)*conver
        tc(4)=chem(i,kts,j,p_dust_4)*conver
        tc(5)=chem(i,kts,j,p_dust_5)*conver
      END IF

      ! Air mass and density at lowest model level.

      airmas(1,1)=-(p8w(i,kts+1,j)-p8w(i,kts,j))*dx*dx/g
      airden(1,1)=rho_phy(i,kts,j)
      ustar(1,1)=ust(i,j)
      dxy(1)=dx*dx

      ! Friction velocity tuning constant (Note: recommend 0.7 for PXLSM,
      ! else use 1.0.  This was created due to make the scheme compatible
      ! with the much stronger friction velocities coming out of PXLSM).
      
      ustar(1,1)=ustar(1,1) * ustune
 
      ! Total erodibility.  Determine what DSR we're using.
      ! Note, the DRI erodibility dataset is an optional 1km resolution dataset
      ! but currently is only available over southwest Asia. If running
      ! a domain outside of SWA, this will fill in the missing data from
      ! DRI with the Ginoux dataset, but there will be inconsistencies. GAC
      ! Erodibility is broken up into 3 bins, sum them up here.

      IF (config_flags%dust_dsr .eq. 1) then  ! DRI DSR
          IF (erod_dri(i,j,1).ge.0) THEN  ! Where DRI is defined
             erodtot(1,1) = SUM(erod_dri(i,j,:))
          ELSE ! Outside where DRI not defined, use Ginoux
             erodtot(1,1)=SUM(erod(i,j,:))
          ENDIF
      ELSE ! Ginoux DSR
          erodtot(1,1)=SUM(erod(i,j,:))
      ENDIF

      ! Set the vegmask variable to the desired vegation mask at this gridpoint

      IF (config_flags%dust_veg .eq. 1) then

         ! 12-month vegetation fraction
         ! If user chose this 12-month greenfrac vegetation mask option,
         ! cut off everything above 5%

         IF (vegfra(i,j) .ge. 5.) then
             vegmask(1,1) = 0.0
         ELSE
             vegmask(1,1) = 1.0
         ENDIF
      ELSE IF (config_flags%dust_veg .eq. 2) then

         ! 8-day MODIS LAI vegmask
         ! 1 = no veg, produce dust; 0 = vegation, do not produce dust

         vegmask(1,1) = lai_vegmask(i,j)
      ELSE

         ! Default choice = static ginoux vegmask

         IF (erod(i,j,1) .eq. 0) THEN
            vegmask(1,1) = 0.0
         ELSE
            vegmask(1,1) = 1.0
         ENDIF
      ENDIF

      ! Remove vegetated areas (vegmask=0) from total erodibility.

      erodtot(1,1) = erodtot(1,1) * vegmask(1,1)

      ! Option to use an optional high resolution soil type database from NGA.
      ! Option 0 = WRF (default); Option 1 = NGA
      ! Note NGA dataset is currently only available over southwest Asia
      ! Until a global dataset becomes available, option 0 is recommended
      ! for consistency.  If option 1 is chosen for a domain outside of
      ! SWA, this logic will fill in the areas missing from NGA with the
      ! defaults from WRF. It will work, but it will be inconsistent. GAC

      IF (config_flags%dust_soils .eq. 1) then
         IF (clay_nga(i,j) .ge.0) then
             clay(i,j) = clay_nga(i,j)
             sand(i,j) = sand_nga(i,j)
         ELSE
             clay(i,j) =clay_wrf(i,j)
             sand(i,j) =sand_wrf(i,j)
         ENDIF
      ELSE
         clay(i,j) =clay_wrf(i,j)
         sand(i,j) =sand_wrf(i,j)
      ENDIF

      ! Mass fractions of clay, silt, and sand.

      massfrac(1)=clay(i,j)
      massfrac(2)=1-(clay(i,j)+sand(i,j))
      massfrac(3)=sand(i,j)

      ! Don't allow roughness lengths greater than 20 cm to be lofted.
      ! This kludge accounts for land use types like urban areas and
      ! forests which would otherwise show up as high dust emitters.
      ! This is a placeholder for a more widely accepted kludge
      ! factor in the literature, which reduces lofting for rough areas.
      ! Forthcoming...

      IF (znt(i,j) .gt. 0.2) then
        ilwi(1,1)=0
      ENDIF

      ! Do not allow areas with bedrock, lava, or land-ice to loft.

      IF (isltyp(i,j) .eq. 15 .or. isltyp(i,j) .eq. 16. .or. &
          isltyp(i,j) .eq. 18) then
        ilwi(1,1)=0
      ENDIF

      ! Another hack to ensure dust does not loft from areas with snow
      ! cover...because, well, that doesn't make sense.

      IF (snowh(i,j) .gt. 0.01) then 
        ilwi(1,1)=0
      ENDIF

      ! Volumetric soil moisture can be tuned here with a dust_smtune
      ! set in the namelist.

      volsm=max(smois(i,1,j)*smtune,0.)

      ! Calculate gravimetric soil moisture.

      gravsm(1,1)=100*volsm/((1.-porosity(isltyp(i,j)))*(2.65*(1-clay(i,j))+2.50*clay(i,j)))

      ! Choose an LSM option and drylimit option.
      ! Drylimit calculations based on look-up table in
      ! Clapp and Hornberger (1978) for RUC and PXLSM and
      ! Cosby et al. (1984) for Noah LSM.

      smois_opt = 0
      IF (config_flags%dust_smois == 1) then
         sfc_select: SELECT CASE(config_flags%sf_surface_physics)
            CASE ( RUCLSMSCHEME, PXLSMSCHEME )
               drylimit(1,1) =0.035*(13.52*clay(i,j)+3.53)**2.68
               smois_opt = 1
            CASE ( LSMSCHEME )
               drylimit(1,1) =0.0756*(15.127*clay(i,j)+3.09)**2.3211
               smois_opt = 1
            CASE DEFAULT

               ! Don't currently support volumetric soil moisture
               ! for this scheme, use drylimit based on gravimetric 

               drylimit(1,1)=14.0*clay(i,j)*clay(i,j)+17.0*clay(i,j)
         END SELECT sfc_select
      ELSE

         ! use drylimit based on gravimetric soil moisture

         drylimit(1,1)=14.0*clay(i,j)*clay(i,j)+17.0*clay(i,j)
      END IF
 
      ! Call dust emission routine.

      call source_dust(imx, jmx, lmx, nmx, smx, dt, tc, ustar, massfrac, &
                       erodtot, ilwi, dxy, gravsm, volsm, airden, airmas, &
                       bems, ustart, g, drylimit, alpha, gamma, smois_opt)

      IF(config_flags%chem_opt == 2 .or. config_flags%chem_opt == 11 ) then
        dustin(i,j,1:5)=tc(1:5)*converi
      ELSE IF(config_flags%chem_opt == CB05_SORG_AQ_KPP .or.  &
              config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) then
!KW            chem(i,kts,j,p_p25i)=chem(i,kts,j,p_p25i) &
!                         +.25*(tc(1)+.286*tc(2))*converi
!            chem(i,kts,j,p_p25i)=max(chem(i,kts,j,p_p25i),1.e-16)
!            chem(i,kts,j,p_p25j)=chem(i,kts,j,p_p25j) &
!                         +.75*(tc(1)+.286*tc(2))*converi
!            chem(i,kts,j,p_p25j)=max(chem(i,kts,j,p_p25j),1.e-16)
!            chem(i,kts,j,p_soila)=chem(i,kts,j,p_soila) &
!                         +(.714*tc(2)+tc(3))*converi
!            chem(i,kts,j,p_soila)=max(chem(i,kts,j,p_soila),1.e-16)
         chem(i,kts,j,p_p25j)=chem(i,kts,j,p_p25j) + 0.03*sum(tc(1:5))*converi
         chem(i,kts,j,p_soila)=chem(i,kts,j,p_soila) + 0.97*1.02*sum(tc(1:5))*converi

         chem(i,kts,j,p_ac0) =  chem(i,kts,j,p_ac0) +  0.03*sum(tc(1:5))*converi*factnuma*soilfac
         chem(i,kts,j,p_corn) =  chem(i,kts,j,p_corn) + 0.97*1.02*sum(tc(1:5))*converi*factnumc*soilfac
      ELSE
        chem(i,kts,j,p_dust_1)=tc(1)*converi
        chem(i,kts,j,p_dust_2)=tc(2)*converi
        chem(i,kts,j,p_dust_3)=tc(3)*converi
        chem(i,kts,j,p_dust_4)=tc(4)*converi
        chem(i,kts,j,p_dust_5)=tc(5)*converi
      ENDIF

      ! Diagnostic dust lofting potential diagnostic calculation

      psi=0.
      w10=(u10(i,j)**2.+v10(i,j)**2.)**0.5
      IF (ustar(1,1) .ne. 0. .and. znt(i,j) .ne. 0.) THEN
        psi=0.4*w10/ustar(1,1)-LOG(10.0/znt(i,j))
      ENDIF
      IF (erodtot(1,1) .gt. 0.) then
        afwa_dustloft(i,j)=ustune*w10-ustart*(LOG(10.0/znt(i,j))+psi)/0.4
      ENDIF

      ! For output diagnostics (g m^-2 s^-1)

      emis_dust(i,1,j,p_edust1)=bems(1)
      emis_dust(i,1,j,p_edust2)=bems(2)
      emis_dust(i,1,j,p_edust3)=bems(3)
      emis_dust(i,1,j,p_edust4)=bems(4)
      emis_dust(i,1,j,p_edust5)=bems(5)

      ! Diagnostic total emitted dust (g m^-2 s^-1)

      tot_edust(i,j)=(bems(1)+bems(2)+bems(3)+bems(4)+bems(5))

    ENDIF

    ! Cumulative dust concentration (ug m^-3) and visibility (m) diagnostics
    ! Note visibility is capped at 20 km.  Simple visibility algorithm based
    ! on mean particle diameter for each dust bin - perfect spheres - perfect
    ! blackbodies.

    DO k=kts,kte
      tot_dust(i,k,j)=(chem(i,k,j,p_dust_1)+chem(i,k,j,p_dust_2)+       &
                       chem(i,k,j,p_dust_3)+chem(i,k,j,p_dust_4)+       &
                       chem(i,k,j,p_dust_5))*rho_phy(i,k,j)
      IF ( tot_dust(i,k,j) .gt. 0. ) THEN
        vis_dust(i,k,j)=MIN(3.912/                                      &
                          ((1.470E-6*chem(i,k,j,p_dust_1)+              &
                            7.877E-7*chem(i,k,j,p_dust_2)+              &
                            4.623E-7*chem(i,k,j,p_dust_3)+              &
                            2.429E-7*chem(i,k,j,p_dust_4)+              &
                            1.387E-7*chem(i,k,j,p_dust_5))*             &
                           rho_phy(i,k,j)),999999.)
      ELSE
        vis_dust(i,k,j)=999999.
      ENDIF
    ENDDO

  ENDDO
  ENDDO

  END SUBROUTINE gocart_dust_afwa_driver

  SUBROUTINE source_dust(imx, jmx, lmx, nmx, smx, dt1, tc, ustar, massfrac,&
                         erod, ilwi, dxy, gravsm, volsm, airden, airmas, &
                         bems, ustart, g0, drylimit, alpha, gamma, smois_opt)

  ! ****************************************************************************
  ! *  Evaluate the source of each dust particles size bin by soil emission  
  ! *
  ! *  Input:
  ! *         EROD      Fraction of erodible grid cell                (-)
  ! *         ILWI      Land/water flag                               (-)
  ! *         GRAVSM    Gravimetric soil moisture                     (g/g)
  ! *         VOLSM     Volumetric soil moisture                      (g/g)
  ! *         SOILM_OPT Soil moisture option (1:Use GRAVSM 2:VOLSM)   (-)
  ! *         DRYLIMIT  Upper GRAVSM (VOLSM) limit for air-dry soil   (g/g)
  ! *         ALPHA     Constant to fudge the total emission of dust  (1/m)
  ! *         GAMMA     Exponential tuning constant for erodibility   (-)
  ! *         DXY       Surface of each grid cell                     (m2)
  ! *         AIRMAS    Mass of air for each grid box                 (kg)
  ! *         AIRDEN    Density of air for each grid box              (kg/m3)
  ! *         USTAR     Friction velocity                             (m/s)
  ! *         MASSFRAC  Fraction of mass in each of 3 soil classes    (-)
  ! *         DT1       Time step                                     (s)
  ! *         NMX       Number of dust bins                           (-)
  ! *         SMX       Number of saltation bins                      (-)
  ! *         IMX       Number of I points                            (-)
  ! *         JMX       Number of J points                            (-)
  ! *         LMX       Number of L points                            (-)
  ! *
  ! *  Data (see module_data_gocart_dust):
  ! *         SPOINT    Pointer to 3 soil classes                     (-)
  ! *         DEN_DUST  Dust density                                  (kg/m3)
  ! *         DEN_SALT  Saltation particle density                    (kg/m3)
  ! *         REFF_SALT Reference saltation particle diameter         (m)
  ! *         REFF_DUST Reference dust particle diameter              (m)
  ! *         LO_DUST   Lower diameter limits for dust bins           (m)
  ! *         UP_DUST   Upper diameter limits for dust bins           (m)
  ! *         FRAC_SALT Soil class mass fraction for saltation bins   (-)
  ! *
  ! *  Parameters:
  ! *         BETAMAX   Maximum sandblasting mass efficiency          (-)
  ! *         CMB       Constant of proportionality                   (-)
  ! *         MMD_DUST  Mass median diameter of dust                  (m)
  ! *         GSD_DUST  Geometric standard deviation of dust          (-)
  ! *         LAMBDA    Side crack propogation length                 (m)
  ! *         CV        Normalization constant                        (-)
  ! *         G0        Gravitational acceleration                    (m/s2)
  ! *         G         Gravitational acceleration in cgs             (cm/s2)
  ! *      
  ! *  Working:
  ! *         BETA      Sandblasting mass efficiency                  (-)
  ! *         U_TS0     "Dry" threshold friction velocity             (m/s)
  ! *         U_TS      Moisture-adjusted threshold friction velocity (m/s)
  ! *         RHOA      Density of air in cgs                         (g/cm3)
  ! *         DEN       Dust density in cgs                           (g/cm3)
  ! *         DIAM      Dust diameter in cgs                          (cm)
  ! *         DMASS     Saltation mass distribution                   (-)
  ! *         DSURFACE  Saltation surface area per unit mass          (m2/kg)
  ! *         DS_REL    Saltation surface area distribution           (-)
  ! *         SALT      Saltation flux                                (kg/m/s)
  ! *         DLNDP     Dust bin width                                (-)
  ! *         EMIT      Total vertical mass flux                      (kg/m2/s)
  ! *         EMIT_VOL  Total vertical volume flux                    (m/s)
  ! *         DSRC      Mass of emitted dust               (kg/timestep/cell)
  ! *
  ! *  Output:
  ! *         TC        Total concentration of dust        (kg/kg/timestep/cell)
  ! *         BEMS      Source of each dust type           (kg/timestep/cell) 
  ! *         USTART    Threshold friction vel. (bin 7)               (m/s)
  ! *
  ! ****************************************************************************

  INTEGER, INTENT(IN)   :: nmx,imx,jmx,lmx,smx
  INTEGER, INTENT(IN)   :: ilwi(imx,jmx)
  REAL*8, INTENT(IN)    :: erod(imx,jmx)
  REAL*8, INTENT(IN)    :: ustar(imx,jmx)
  REAL*8, INTENT(IN)    :: gravsm(imx,jmx)
  REAL*8, INTENT(IN)    :: drylimit(imx,jmx) 
  REAL*8, INTENT(IN)    :: dxy(jmx)
  REAL*8, INTENT(IN)    :: airden(imx,jmx,lmx), airmas(imx,jmx,lmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(OUT)   :: bems(imx,jmx,nmx) 
  REAL, INTENT(IN)    :: g0,dt1
  REAL, INTENT(OUT)   :: ustart
  INTEGER, INTENT(IN) :: smois_opt
  REAL*8, INTENT(IN)    :: volsm

  REAL*8    :: den(smx), diam(smx)
  REAL*8    :: dvol(nmx), dlndp(nmx)
! REAL*8    :: distr_dust(nmx)
  REAL*8    :: dsurface(smx), ds_rel(smx)
  REAL*8    :: massfrac(3)
  REAL*8    :: u_ts0, u_ts, dsrc, srce, dmass, dvol_tot
  REAL*8    :: emit, emit_vol
  REAL      :: rhoa, g
  REAL*8    :: salt, stotal
  INTEGER   :: i, j, m, n, s

! Global tuning constant, alpha.  Sandblasting mass efficiency, beta.
! Beta maxes out for clay fractions above 0.2 => betamax.

  REAL, INTENT(IN)  :: alpha
  REAL, PARAMETER :: betamax=5.25E-4
  REAL*8 :: beta

! Experimental optional exponential tuning constant for erodibility.
! 0 < gamma < 1 -> more relative impact by low erodibility regions.
! 1 < gamma -> accentuates high erodibility regions.  Recommend this
! be set to 1 (default) unless looking for a way to increase spread
! within an ensemble framework.
  
  REAL, INTENT(IN) :: gamma

! Constant of proportionality from Marticorena et al, 1997 (unitless)
! Arguably more ~consistent~ fudge than alpha, which has many walnuts
! sprinkled throughout the literature. 

  REAL, PARAMETER :: cmb=1.0    ! Marticorena et al,1997
! REAL, PARAMETER :: cmb=2.61   ! White,1979

! Parameters used in Kok distribution function. Advise not to play with 
! these without the expressed written consent of someone who knows what
! they're doing. (See Kok, 2010 PNAS for details on this scheme).

! REAL, PARAMETER :: mmd_dust=3.4D-6  ! median mass diameter (m)
! REAL, PARAMETER :: gsd_dust=3.0     ! geometric standard deviation
! REAL, PARAMETER :: lambda=12.0D-6   ! crack propogation length (m)
! REAL, PARAMETER :: cv=12.62D-6      ! normalization constant

! Calculate saltation surface area distribution from sand, silt, and clay
! mass fractions and saltation bin fraction. This will later become a 
! modifier to the total saltation flux.  The reasoning here is that the 
! size and availability of saltators affects saltation efficiency. Based
! on Eqn. (32) in Marticorena & Bergametti, 1995 (hereon, MB95).

  DO n=1,smx
    dmass=massfrac(spoint(n))*frac_salt(n)
    dsurface(n)=0.75*dmass/(den_salt(n)*reff_salt(n))  
  ENDDO
  
! The following equation yields relative surface area fraction.  It will only
! work if you are representing the "full range" of all three soil classes.
! For this reason alone, we have incorporated particle sizes that encompass
! the clay class, to account for its relative area over the basal
! surface, even though these smaller bins would be unlikely to play any large
! role in the actual saltation process.

  stotal=SUM(dsurface(:))
  DO n=1,smx
    ds_rel(n)=dsurface(n)/stotal
  ENDDO

! Calculate total dust emission due to saltation of sand sized particles.
! Begin by calculating DRY threshold friction velocity (u_ts0).  Next adjust
! u_ts0 for moisture to get threshold friction velocity (u_ts). Then
! calculate saltation flux (salt) where ustar has exceeded u_ts.  Finally, 
! calculate total dust emission (tot_emit), taking into account erodibility. 

  g = g0*1.0E2                          ! (cm s^-2)
  emit=0.0

  DO n = 1, smx                         ! Loop over saltation bins
    den(n) = den_salt(n)*1.0D-3         ! (g cm^-3)
    diam(n) = 2.0*reff_salt(n)*1.0D2    ! (cm)
    DO i = 1,imx
      DO j = 1,jmx
        rhoa = airden(i,j,1)*1.0D-3     ! (g cm^-3)
 
        ! Threshold friction velocity as a function of the dust density and
        ! diameter from Bagnold (1941) (m s^-1).

        u_ts0 = 0.13*1.0D-2*SQRT(den(n)*g*diam(n)/rhoa)* &
                SQRT(1.0+0.006/den(n)/g/(diam(n))**2.5)/ &
                SQRT(1.928*(1331.0*(diam(n))**1.56+0.38)**0.092-1.0) 

        ! Friction velocity threshold correction function based on physical
        ! properties related to moisture tension. Soil moisture greater than
        ! dry limit serves to increase threshold friction velocity (making
        ! it more difficult to loft dust). When soil moisture has not reached
        ! dry limit, treat as dry (no correction to threshold friction
        ! velocity). GAC

        ! Calculate threshold friction velocity. If volumetric (gravimetric)
        ! water content is less than the drylimit, then the threshold friction
        ! velocity (u_ts) will be equal to the dry threshold friction velocity
        ! (u_ts0). EDH

        IF (smois_opt .EQ. 1) THEN
          IF (100.*volsm > drylimit(i,j)) THEN
            u_ts = MAX(0.0D+0,u_ts0*(sqrt(1.0+1.21*((100.*volsm)-drylimit(i,j))**0.68)))
          ELSE
            u_ts = u_ts0
          ENDIF
        ELSE
          IF (gravsm(i,j) > drylimit(i,j)) THEN
            u_ts = MAX(0.0D+0,u_ts0*(sqrt(1.0+1.21*(gravsm(i,j)-drylimit(i,j))**0.68)))
          ELSE
            u_ts = u_ts0
          END IF 
        END IF

        ! Bin 7 threshold friction velocity for diagnostic dust lofting
        ! potential product

        IF (n .eq. 7) THEN  ! Saltation bin 7 is small sand
          ustart = u_ts
        ENDIF
 
        ! Saltation flux (kg m^-1 s^-1) from MB95
        ! ds_rel is the relative surface area distribution
 
        IF (ustar(i,j) .gt. u_ts .and. erod(i,j) .gt. 0.0 .and. ilwi(i,j) == 1) THEN
          salt = cmb*ds_rel(n)*(airden(i,j,1)/g0)*(ustar(i,j)**3)* &
                 (1. + u_ts/ustar(i,j))*(1. - (u_ts**2)/(ustar(i,j)**2))
        ELSE 
          salt = 0.D0
        ENDIF
 
        ! Calculate total vertical mass flux (note beta has units of m^-1)
        ! Beta acts to tone down dust in areas with so few dust-sized particles
        ! that the lofting efficiency decreases.  Otherwise, super sandy zones
        ! would be huge dust producers, which is generally not the case.
        ! Equation derived from wind-tunnel experiments (see MB95).
 
        beta=10**(13.6*massfrac(1)-6.0)  ! (unitless)
        IF (beta .gt. betamax) THEN
          beta=betamax
        ENDIF
        emit=emit+salt*(erod(i,j)**gamma)*alpha*beta    ! (kg m^-2 s^-1)
      END DO
    END DO
  END DO                                 ! End do over saltation bins

  ! Now that we have the total dust emission, distribute into dust bins using 
  ! lognormal distribution (Dr. Jasper Kok, 2010), and
  ! calculate total mass emitted over the grid box over the timestep. 
  !
  ! In calculating the Kok distribution, we assume upper and lower limits to
  ! each bin. For reff_dust=(/0.73D-6,1.4D-6,2.4D-6,4.5D-6,8.0D-6/) (default),
  ! lower limits were ASSUMED at lo_dust=(/0.1D-6,1.0D-6,1.8D-6,3.0D-6,6.0D-6/)
  ! upper limits were ASSUMED at up_dust=(/1.0D-6,1.8D-6,3.0D-6,6.0D-6,10.0D-6/)
  ! These may be changed within module_data_gocart_dust.F, but make sure it is
  ! consistent with reff_dust values.  These values were taken from the original
  ! GOCART bin configuration. We use them here to calculate dust bin width,
  ! dlndp. dvol is the volume distribution. GAC
  !
  ! UPDATE: We bypass the calculation below and instead hardcode distr_dust for
  ! the five dust bins we are using here since this distribution is static and
  ! unnecessary to calculate at every time step. Keeping everything here to
  ! document the steps in obtaining distr_dust.  GAC 20140320
  
!  dvol_tot=0.
!  DO n=1,nmx  ! Loop over all dust bins
!    dlndp(n)=LOG(up_dust(n)/lo_dust(n))
!    dvol(n)=(2.0*reff_dust(n)/cv)*(1.+ERF(LOG(2.0*reff_dust(n)/mmd_dust)/(SQRT(2.)*LOG(gsd_dust))))*&
!          EXP(-(2.0*reff_dust(n)/lambda)**3.0)*dlndp(n)
!    dvol_tot=dvol_tot+dvol(n)
!
!   ! Convert mass flux to volume flux
!    emit_vol=emit/den_dust(n) ! (m s^-1)
!  END DO
!  DO n=1,nmx  ! Loop over all dust bins
!    distr_dust(n)=dvol(n)/dvol_tot
!  END DO
 
  ! Now distribute total vertical emission into dust bins and update
  ! concentration.

  DO n=1,nmx  ! Loop over all dust bins
    DO i=1,imx
      DO j=1,jmx

        ! Calculate total mass emitted

        dsrc = emit*distr_dust(n)*dxy(j)*dt1  ! (kg)
        IF (dsrc < 0.0) dsrc = 0.0

        ! Update dust mixing ratio at first model level.

        tc(i,j,1,n) = tc(i,j,1,n) + dsrc / airmas(i,j,1) ! (kg/kg)
        bems(i,j,n) = 1000.*dsrc/(dxy(j)*dt1) ! diagnostic (g/m2/s)
      END DO
    END DO
  END DO

  END SUBROUTINE source_dust


END MODULE GOCART_DUST_AFWA
